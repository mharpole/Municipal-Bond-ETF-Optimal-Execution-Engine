# ============================================================================
# global_helpers.R
# Municipal Bond ETF — Optimal Passive Limit-Order Execution Engine
# Version: 1.0.0
# ----------------------------------------------------------------------------
# Quantitative pipeline:
#   1. Two-factor Hull-White short-rate simulation           (rate risk)
#   2. Merton jump-diffusion overlay on macro announcement days (event risk)
#   3. Ornstein-Uhlenbeck premium/discount-to-NAV process     (microstructure)
#   4. Seasonality drift mapping                              (flow regime)
#   5. Path assembly: NAV x (1 + premium) -> tradable ETF price
#   6. Almgren-Chriss (2000/2001) optimal execution trajectory
#   7. Threshold derivation: AC urgency -> per-tranche buy-limit prices
#   8. Trade metrics: fill probability, expected cost, CVaR, safety margin
#
# Required packages:
#   install.packages(c("shiny","bslib","dplyr","tibble","tidyquant",
#                       "plotly","DT","shinyWidgets","shinycssloaders"))
#
# ----------------------------------------------------------------------------
# DATA-DRIVEN CALIBRATION (replaces manually-typed proxy inputs)
# ----------------------------------------------------------------------------
# Earlier drafts of this app asked the user to manually type a MOVE index
# level and a CDX spread as rate-vol / credit-stress proxies, plus ~20 other
# calibration numbers. Both are now pulled from free public data instead:
#   - Rate level & volatility : FRED DGS3MO / DGS10 (Treasury CMT yields)
#     replace the manual MOVE input — realized Treasury yield volatility IS
#     the thing MOVE approximates, sourced directly rather than via a proxy.
#   - Credit stress           : FRED BAMLH0A0HYM2 (ICE BofA US High Yield
#     Index Option-Adjusted Spread) replaces the manual CDX spread input —
#     an actual published credit spread series, not a stand-in for one.
#   - Macro event days        : a hardcoded 2026 FOMC/CPI release calendar
#     (sourced from federalreserve.gov and bls.gov, see MACRO CALENDAR
#     below) replaces manual date pickers.
# All three are pulled via tidyquant (Yahoo Finance for prices, FRED for
# the rate/credit series — neither requires an API key) and cached locally
# (see cached_price_history() / cached_fred_series()).
#
# What's left as an internal fixed constant (not exposed, not derivable from
# free data, but not a meaningful user preference either — see the
# constants block below) vs. what's still a genuine user input is documented
# inline at each point.
#
# PERFORMANCE NOTE: all matrix math is vectorized ACROSS PATHS. The only
# explicit R loop is over the (<=60) time steps, which is unavoidable for a
# path-dependent recursion. At 10,000 paths x 30 steps this is ~300K vector
# ops — well under a second in base R.
# ============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(tidyquant)
})

# ---- 0. Reference data -----------------------------------------------------

# Static fallback only — used when a live price pull fails, and as the sole
# source for duration, a fund-analytics figure NOT obtainable from a price
# feed (would need holdings-level data from the issuer).
TICKER_INFO <- tibble::tribble(
  ~ticker, ~name,                                    ~default_price, ~default_duration,
  "VTEB",  "Vanguard Tax-Exempt Bond ETF",             51.20,          6.1,
  "HIMU",  "Muni High-Income ETF",                     47.85,          7.4,
  "VTEL",  "Muni Ladder/Target ETF",                   49.10,          4.8,
  "MUB",   "iShares National Muni Bond ETF",          106.40,          6.0,
  "SUB",   "iShares Short-Term National Muni ETF",    105.90,          2.3
)

# Deterministic daily drift (decimal, e.g. 0.0005 = +5 bps/day) applied to
# the NAV process to represent institutional bond supply/demand flow effects
# associated with a calendar regime. Forward-looking judgment call — not
# something derivable from historical price data, so this stays a user input.
SEASONALITY_MAP <- c(
  "Neutral"                              = 0.00000,
  "Summer High-Reinvestment Window"      = 0.00050,
  "Q4 Tax-Loss Harvesting"               = -0.00080,
  "Q1 New-Issue Supply Surge"            = -0.00060,
  "January Effect (Reinvestment Demand)" = 0.00045
)

# Liquidity/impact presets replace two raw Almgren-Chriss coefficients
# (temporary impact eta, permanent impact gamma) that have no free-data
# source (they're market-microstructure/liquidity estimates) but do matter
# enough to the execution schedule to leave user-adjustable, just as a
# single categorical choice instead of two opaque numbers.
MARKET_IMPACT_PRESETS <- list(
  "High Liquidity (Large, Actively Traded ETF)" = list(eta = 0.010, gamma = 0.0010),
  "Moderate Liquidity (Typical Muni ETF)"       = list(eta = 0.020, gamma = 0.0020),
  "Low Liquidity (Thin / Niche Market)"         = list(eta = 0.045, gamma = 0.0045)
)

# ---- 0a. Fixed internal constants (not exposed in the UI) ------------------
# Structural modeling constants with no simple market-observable proxy and
# no meaningful "user preference" interpretation — reasonable literature-
# standard defaults, fixed here instead of asking the user to type them.
HW_A               <- 0.10     # Hull-White mean-reversion speed, factor 1
HW_B               <- 0.05     # Hull-White mean-reversion speed, factor 2
HW_RHO             <- -0.30    # correlation between the two HW factors
HW_SIGMA2_RATIO    <- 0.50     # sigma2 = this * (derived) sigma1
OU_KAPPA_DEFAULT   <- 6.0      # premium/discount mean-reversion speed
OU_SIGMA_DEFAULT   <- 0.0025   # premium/discount OU volatility (0.25%)
JUMP_BASE_INTENSITY  <- 0.02   # baseline daily jump probability
JUMP_EVENT_INTENSITY <- 0.65   # jump probability on a flagged macro day
JUMP_MEAN            <- -0.0015 # mean jump size (-0.15%)
JUMP_SD_BASE         <- 0.006   # baseline jump size std dev (0.6%), scaled
                                 # by credit stress — see fetch_credit_stress()
JUMP_EVENT_MULT      <- 2.5     # macro-day jump size multiplier
P_MIN_DEFAULT        <- 0.30    # baseline fill-probability target, day 1
N_PATHS_DEFAULT      <- 10000   # Monte Carlo path count
CONVEXITY_DEFAULT    <- 0.5     # bond convexity (second-order rate term)
HY_SPREAD_BASELINE   <- 4.0     # "typical" ICE BofA HY OAS level (%), used
                                 # to normalize the credit-stress multiplier
APP_VERSION          <- "1.0.0"

# ---- 0b. Macro event calendar (FOMC + CPI) ----------------------------------
# Hardcoded rather than pulled live — the Fed and BLS publish these release
# calendars up to a year in advance, so there's no "live data" version of
# this to fetch; it's simply looked up. Sourced from federalreserve.gov
# (FOMC meeting calendar) and bls.gov / BLS release schedule, current as of
# Aug 2026. Covers 2026 only — a horizon that runs into 2027 will just see
# no flagged macro days past Dec 2026 (graceful degradation, not an error).
FOMC_DATES_2026 <- as.Date(c(
  "2026-01-28", "2026-03-18", "2026-04-29", "2026-06-17",
  "2026-07-29", "2026-09-16", "2026-10-28", "2026-12-09"
))
CPI_RELEASE_DATES_2026 <- as.Date(c(
  "2026-01-13", "2026-02-13", "2026-03-11", "2026-04-10",
  "2026-05-12", "2026-06-10", "2026-07-14", "2026-08-12",
  "2026-09-11", "2026-10-14", "2026-11-10", "2026-12-10"
))
MACRO_CALENDAR <- sort(c(FOMC_DATES_2026, CPI_RELEASE_DATES_2026))

# ---- 0c. Local caching & saved-order persistence ---------------------------

# Two separate directories, on purpose:
#  - CACHE_DIR : disposable. Pulled market data; safe to delete any time,
#                will just be re-fetched. Fine to .gitignore.
#  - DATA_DIR  : durable. User-created saved order configurations — do NOT
#                treat as disposable / do NOT .gitignore.
CACHE_DIR <- "cache"
DATA_DIR  <- "data"
SAVED_ORDERS_FILE <- file.path(DATA_DIR, "saved_orders.rds")

.ensure_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
}

#' Fetch daily price history for a ticker via tidyquant::tq_get() (Yahoo
#' Finance, no API key), using a local RDS cache so the same ticker isn't
#' re-pulled from the network on every ticker switch / app reload within the
#' cache window. A cached pull younger than `max_age_hours` is reused as-is.
#' Otherwise a fresh pull is attempted; if that fails, a stale cache is still
#' returned (flagged via $stale) rather than failing outright. Returns NULL
#' only if there is neither a fresh pull nor any cache at all.
cached_price_history <- function(ticker, lookback_days = 252, max_age_hours = 24) {
  .ensure_dir(CACHE_DIR)
  cache_file <- file.path(CACHE_DIR, paste0(ticker, "_prices.rds"))

  cached <- if (file.exists(cache_file)) {
    tryCatch(readRDS(cache_file), error = function(e) NULL)
  } else NULL

  if (!is.null(cached)) {
    age_hours <- as.numeric(difftime(Sys.time(), cached$fetched_at, units = "hours"))
    if (age_hours < max_age_hours) {
      cached$from_cache <- TRUE
      cached$stale <- FALSE
      return(cached)
    }
  }

  fresh <- tryCatch(
    tidyquant::tq_get(ticker,
                       from = Sys.Date() - ceiling(lookback_days * 1.6),
                       to = Sys.Date(), get = "stock.prices"),
    error = function(e) NULL
  )

  if (!is.null(fresh) && nrow(fresh) >= 20) {
    result <- list(prices = fresh, fetched_at = Sys.time(), ticker = ticker,
                    from_cache = FALSE, stale = FALSE)
    saveRDS(result, cache_file)
    return(result)
  }

  if (!is.null(cached)) {
    cached$from_cache <- TRUE
    cached$stale <- TRUE
    return(cached)
  }
  NULL
}

#' Same caching pattern as cached_price_history(), but for a FRED economic
#' series (get = "economic.data") rather than a Yahoo Finance ticker.
cached_fred_series <- function(series_id, lookback_days = 252, max_age_hours = 24) {
  .ensure_dir(CACHE_DIR)
  cache_file <- file.path(CACHE_DIR, paste0("FRED_", series_id, ".rds"))

  cached <- if (file.exists(cache_file)) {
    tryCatch(readRDS(cache_file), error = function(e) NULL)
  } else NULL

  if (!is.null(cached)) {
    age_hours <- as.numeric(difftime(Sys.time(), cached$fetched_at, units = "hours"))
    if (age_hours < max_age_hours) {
      cached$from_cache <- TRUE
      cached$stale <- FALSE
      return(cached)
    }
  }

  fresh <- tryCatch(
    tidyquant::tq_get(series_id,
                       from = Sys.Date() - ceiling(lookback_days * 2.2),
                       to = Sys.Date(), get = "economic.data"),
    error = function(e) NULL
  )

  if (!is.null(fresh) && nrow(fresh) >= 20) {
    result <- list(series = fresh, fetched_at = Sys.time(), series_id = series_id,
                    from_cache = FALSE, stale = FALSE)
    saveRDS(result, cache_file)
    return(result)
  }

  if (!is.null(cached)) {
    cached$from_cache <- TRUE
    cached$stale <- TRUE
    return(cached)
  }
  NULL
}

#' Derive live/cached calibration (starting price + realized vol) for a
#' ticker. See cached_price_history() for the caching behavior.
#'
#' NOTE: duration and the ETF's premium/discount TIME SERIES are still NOT
#' obtainable from a price feed like this — duration would need holdings-
#' level data from the issuer, and daily NAV history for muni ETFs generally
#' isn't available for free. Duration keeps its static per-ticker reference
#' default (editable); premium/discount starting level stays a manual input.
fetch_live_calibration <- function(ticker, lookback_days = 252, max_age_hours = 24) {
  fallback_row <- TICKER_INFO[TICKER_INFO$ticker == ticker, ]
  fallback_price <- if (nrow(fallback_row) == 1) fallback_row$default_price else 50

  cached <- cached_price_history(ticker, lookback_days = lookback_days, max_age_hours = max_age_hours)

  if (is.null(cached)) {
    return(list(
      price0 = fallback_price, realized_daily_vol = NA_real_, last_price_date = NA,
      fetched_at = NA, stale = FALSE,
      source = "Static reference price (no live or cached data)"
    ))
  }

  px <- cached$prices[order(cached$prices$date), ]
  px <- utils::tail(px, lookback_days)
  log_ret <- diff(log(px$adjusted))

  src_label <- if (cached$stale) {
    "Cached price (stale \u2014 refresh failed, showing last successful pull)"
  } else if (cached$from_cache) {
    "Cached price (Yahoo Finance via tidyquant)"
  } else {
    "Live pull \u2014 Yahoo Finance via tidyquant"
  }

  list(
    price0 = as.numeric(utils::tail(px$close, 1)),
    realized_daily_vol = stats::sd(log_ret, na.rm = TRUE),
    last_price_date = utils::tail(px$date, 1),
    fetched_at = cached$fetched_at,
    stale = cached$stale,
    source = src_label
  )
}

#' Derive the Hull-White rate factor's level (r0, r_bar) and volatility
#' (sigma1) from live/cached Treasury yield data — DGS3MO (3-month T-bill,
#' short-rate proxy) and DGS10 (10-year, long-run anchor), both FRED series
#' via tidyquant, in percent, no scaling ambiguity. This is what replaces
#' the old manually-typed "ICE BofA MOVE Index" input: realized short-rate
#' volatility from actual Treasury data IS rate-market volatility, rather
#' than a manually-entered stand-in for it.
fetch_rate_environment <- function(lookback_days = 252, max_age_hours = 24) {
  fallback <- list(
    r0 = 0.038, r_bar = 0.040, hw_sigma1 = 0.009, as_of = NA,
    source = "Static fallback (no live/cached FRED data)"
  )

  short <- cached_fred_series("DGS3MO", lookback_days, max_age_hours)
  long  <- cached_fred_series("DGS10",  lookback_days, max_age_hours)
  if (is.null(short) || is.null(long)) return(fallback)

  s <- short$series[order(short$series$date), ]
  s <- s[!is.na(s$price), ]
  l <- long$series[order(long$series$date), ]
  l <- l[!is.na(l$price), ]
  if (nrow(s) < 20 || nrow(l) < 5) return(fallback)

  s <- utils::tail(s, lookback_days)
  daily_chg <- diff(s$price) / 100   # percentage points -> decimal

  list(
    r0 = utils::tail(s$price, 1) / 100,
    r_bar = utils::tail(l$price, 1) / 100,
    hw_sigma1 = stats::sd(daily_chg, na.rm = TRUE),
    as_of = utils::tail(s$date, 1),
    source = if (short$stale || long$stale) {
      "Cached Treasury data (stale \u2014 refresh failed)"
    } else if (short$from_cache || long$from_cache) {
      "Cached Treasury data (FRED via tidyquant)"
    } else {
      "Live \u2014 FRED (DGS3MO / DGS10) via tidyquant"
    }
  )
}

#' Derive a credit-stress level from the ICE BofA US High Yield Index
#' Option-Adjusted Spread (FRED series BAMLH0A0HYM2) — a real, published
#' credit spread benchmark that replaces the old manually-typed CDX spread
#' input. Returned as a level (percent) plus a normalized multiplier
#' (relative to HY_SPREAD_BASELINE) used to widen jump-diffusion vol on
#' macro announcement days when credit markets are already stressed.
fetch_credit_stress <- function(lookback_days = 60, max_age_hours = 24) {
  fallback <- list(spread_pct = HY_SPREAD_BASELINE, as_of = NA,
                    source = "Static fallback (no live/cached FRED data)")

  hy <- cached_fred_series("BAMLH0A0HYM2", lookback_days, max_age_hours)
  if (is.null(hy)) return(fallback)

  d <- hy$series[order(hy$series$date), ]
  d <- d[!is.na(d$price), ]
  if (nrow(d) < 1) return(fallback)

  list(
    spread_pct = utils::tail(d$price, 1),
    as_of = utils::tail(d$date, 1),
    source = if (hy$stale) "Cached credit spread (stale \u2014 refresh failed)"
             else if (hy$from_cache) "Cached credit spread (FRED via tidyquant)"
             else "Live \u2014 FRED (ICE BofA HY OAS) via tidyquant"
  )
}

# ---- 0d. Saved order configurations ----------------------------------------

# Maps every remaining sidebar input that defines an "order" to the Shiny
# widget type that created it, so a saved configuration can be replayed with
# the correct update*Input() call. Single source of truth for both what gets
# captured on Save and how it gets restored on Load.
ORDER_INPUT_TYPES <- c(
  ticker = "picker", horizon_days = "slider", seasonality = "select", lambda = "slider",
  price0 = "numeric", prem0 = "numeric", duration = "numeric",
  order_notional = "numeric", tranche_freq = "radio", market_impact = "select"
)
ALL_ORDER_INPUT_IDS <- names(ORDER_INPUT_TYPES)

#' Load all saved order configurations from disk (a named list: name -> a
#' named list of raw input values, one per ALL_ORDER_INPUT_IDS).
load_saved_orders <- function() {
  if (file.exists(SAVED_ORDERS_FILE)) {
    tryCatch(readRDS(SAVED_ORDERS_FILE), error = function(e) list())
  } else {
    list()
  }
}

#' Persist the full set of saved order configurations to disk.
save_saved_orders <- function(orders) {
  .ensure_dir(DATA_DIR)
  saveRDS(orders, SAVED_ORDERS_FILE)
  invisible(orders)
}

# ---- 1. Calendar helpers -----------------------------------------------------

#' Business-day sequence starting at start_date, length n_steps (Mon-Fri only)
business_day_sequence <- function(start_date, n_steps) {
  start_date <- as.Date(start_date)
  days <- seq(start_date, by = "day", length.out = n_steps * 2 + 10)
  wday <- as.POSIXlt(days)$wday   # 0 = Sunday, 6 = Saturday
  days <- days[wday != 0 & wday != 6]
  days[1:n_steps]
}

#' Look up which entries in the fixed MACRO_CALENDAR (FOMC + CPI, see above)
#' fall within the execution horizon, and map them onto integer business-day
#' step indices for the jump-diffusion overlay. No user input required.
upcoming_macro_dates <- function(start_date, n_steps) {
  start_date <- as.Date(start_date)
  bdays <- business_day_sequence(start_date, n_steps)
  horizon_end <- max(bdays)

  relevant <- MACRO_CALENDAR[MACRO_CALENDAR >= start_date & MACRO_CALENDAR <= horizon_end]
  idx <- match(relevant, bdays)
  keep <- !is.na(idx)

  list(steps = idx[keep], dates = relevant[keep])
}

# ---- 2. Two-factor Hull-White short-rate model ------------------------------

#' Simulate a two-factor Hull-White short-rate process.
#'   dr(t) = [r_bar - a*r(t) + u(t)] dt + sigma1 dW1
#'   du(t) = -b*u(t) dt + sigma2 dW2 ,   corr(dW1, dW2) = rho
#' Returns n_paths x (n_steps+1) matrices for r and u (column 1 = t0).
simulate_hull_white_2f <- function(n_paths, n_steps, dt,
                                    r0, r_bar,
                                    a = 0.10, b = 0.05,
                                    sigma1 = 0.010, sigma2 = 0.006,
                                    rho = -0.3) {
  r <- matrix(0, n_paths, n_steps + 1)
  u <- matrix(0, n_paths, n_steps + 1)
  r[, 1] <- r0
  u[, 1] <- 0

  sqdt <- sqrt(dt)
  for (t in 1:n_steps) {
    z1 <- rnorm(n_paths)
    z2_indep <- rnorm(n_paths)
    z2 <- rho * z1 + sqrt(1 - rho^2) * z2_indep

    u[, t + 1] <- u[, t] - b * u[, t] * dt + sigma2 * sqdt * z2
    r[, t + 1] <- r[, t] + (r_bar - a * r[, t] + u[, t]) * dt + sigma1 * sqdt * z1
  }
  list(r = r, u = u)
}

# ---- 3. Merton jump-diffusion overlay ---------------------------------------

#' Simulate per-path, per-step jump return shocks. Every step has a baseline
#' jump probability; steps flagged in jump_days (macro announcements) use an
#' elevated probability and a wider jump-size distribution.
#' Returns an n_paths x n_steps matrix of additive return shocks.
simulate_jump_shocks <- function(n_paths, n_steps, jump_days,
                                  base_intensity = 0.02,
                                  event_intensity = 0.65,
                                  jump_mean = -0.0015,
                                  jump_sd = 0.006,
                                  event_jump_sd_mult = 2.5) {
  shocks <- matrix(0, n_paths, n_steps)
  for (t in 1:n_steps) {
    is_event <- t %in% jump_days
    p <- if (is_event) event_intensity else base_intensity
    occurs <- rbinom(n_paths, 1, p)
    sd_t <- jump_sd * (if (is_event) event_jump_sd_mult else 1)
    size <- rnorm(n_paths, mean = jump_mean, sd = sd_t)
    shocks[, t] <- occurs * size
  }
  shocks
}

# ---- 4. Ornstein-Uhlenbeck premium/discount-to-NAV process -----------------

#' d(prem) = kappa * (theta_prem - prem) dt + sigma_prem dW
#' Returns n_paths x (n_steps+1) matrix of premium (+) / discount (-) values.
simulate_ou_premium <- function(n_paths, n_steps, dt, prem0,
                                 theta_prem = -0.0010,
                                 kappa = 6.0,
                                 sigma_prem = 0.0025) {
  prem <- matrix(0, n_paths, n_steps + 1)
  prem[, 1] <- prem0
  sqdt <- sqrt(dt)
  for (t in 1:n_steps) {
    z <- rnorm(n_paths)
    prem[, t + 1] <- prem[, t] + kappa * (theta_prem - prem[, t]) * dt + sigma_prem * sqdt * z
  }
  prem
}

# ---- 5. Assemble NAV + ETF price paths --------------------------------------

build_price_paths <- function(n_paths, n_steps, dt,
                               price0, duration, convexity = 0.5,
                               hw_params, jump_params, ou_params,
                               seasonality_drift_daily) {

  hw <- do.call(simulate_hull_white_2f,
                c(list(n_paths = n_paths, n_steps = n_steps, dt = dt), hw_params))

  dr <- hw$r[, 2:(n_steps + 1), drop = FALSE] - hw$r[, 1:n_steps, drop = FALSE]
  rate_return <- -duration * dr + 0.5 * convexity * dr^2

  jump_shocks <- do.call(simulate_jump_shocks,
                          c(list(n_paths = n_paths, n_steps = n_steps), jump_params))

  daily_return <- rate_return + jump_shocks + seasonality_drift_daily

  nav <- matrix(0, n_paths, n_steps + 1)
  nav[, 1] <- price0
  for (t in 1:n_steps) {
    nav[, t + 1] <- nav[, t] * (1 + daily_return[, t])
  }

  prem <- do.call(simulate_ou_premium,
                   c(list(n_paths = n_paths, n_steps = n_steps, dt = dt), ou_params))

  etf_price <- nav * (1 + prem)

  list(nav = nav, premium = prem, etf_price = etf_price, short_rate = hw$r)
}

# ---- 6. Almgren-Chriss optimal execution trajectory -------------------------

almgren_chriss_trajectory <- function(N, tau, X, sigma_daily, lambda,
                                       eta = 0.02, gamma = 0.002) {
  eta_tilde <- eta - gamma * tau / 2
  if (eta_tilde <= 0) eta_tilde <- eta

  j <- 0:N
  if (lambda <= 0 || sigma_daily <= 0) {
    kappa <- 0
    x <- X * (N - j) / N
  } else {
    kappa_tilde_sq <- (lambda * sigma_daily^2) / eta_tilde
    inside <- (tau^2 * kappa_tilde_sq) / 2 + 1
    kappa <- acosh(inside) / tau
    x <- X * sinh(kappa * (N - j) * tau) / sinh(kappa * N * tau)
  }
  x[1] <- X
  x[length(x)] <- 0

  list(x = x, kappa = kappa, kappa_T = kappa * N * tau)
}

# ---- 7. Threshold derivation -------------------------------------------------

derive_thresholds <- function(sim, ac_remaining_frac, p_min = 0.30) {
  n_steps <- ncol(sim$etf_price) - 1
  price_by_day <- sim$etf_price[, 2:(n_steps + 1), drop = FALSE]

  p_fill_target <- 1 - ac_remaining_frac * (1 - p_min)
  p_fill_target <- pmin(pmax(p_fill_target, 0.01), 0.995)

  thresholds <- numeric(n_steps)
  for (j in 1:n_steps) {
    thresholds[j] <- as.numeric(quantile(price_by_day[, j], probs = p_fill_target[j], na.rm = TRUE))
  }

  list(thresholds = thresholds, p_fill_target = p_fill_target)
}

# ---- 8. Trade metrics --------------------------------------------------------

compute_trade_metrics <- function(sim, thresholds, price0) {
  n_steps <- length(thresholds)
  price_by_day <- sim$etf_price[, 2:(n_steps + 1), drop = FALSE]

  touched <- price_by_day <= matrix(thresholds, nrow = nrow(price_by_day),
                                     ncol = n_steps, byrow = TRUE)
  filled_ever <- apply(touched, 1, any)
  fill_prob <- mean(filled_ever)

  first_fill_day <- apply(touched, 1, function(row) {
    idx <- which(row)
    if (length(idx) == 0) NA_integer_ else idx[1]
  })
  fill_price <- ifelse(is.na(first_fill_day), NA_real_, thresholds[first_fill_day])
  cost_vs_arrival <- (fill_price - price0) / price0

  filled_costs <- cost_vs_arrival[!is.na(cost_vs_arrival)]
  if (length(filled_costs) > 0) {
    es_cutoff <- quantile(filled_costs, probs = 0.95, na.rm = TRUE)
    expected_shortfall <- mean(filled_costs[filled_costs >= es_cutoff])
    expected_cost <- mean(filled_costs)
  } else {
    expected_shortfall <- NA_real_
    expected_cost <- NA_real_
  }

  median_path <- apply(price_by_day, 2, median)
  safety_margin_bps <- mean((median_path - thresholds) / median_path) * 10000

  list(
    fill_probability = fill_prob,
    expected_cost_bps = expected_cost * 10000,
    expected_shortfall_bps = expected_shortfall * 10000,
    safety_margin_bps = safety_margin_bps,
    first_fill_day = first_fill_day,
    fill_price = fill_price
  )
}

# ---- 9. Top-level pipeline orchestrator -------------------------------------

#' Run the full simulation + optimization pipeline. `inputs` now only needs
#' the handful of things that are genuine user choices or genuinely
#' unavailable from free data — everything else (rate level/vol, credit
#' stress, macro event days, and the ~15 structural constants) is pulled
#' live/cached or fixed internally. See ALL_ORDER_INPUT_IDS for the exact
#' fields `inputs` must have (ticker, horizon_days, start_date, seasonality,
#' lambda, price0, prem0, duration, order_notional, market_impact).
run_engine <- function(inputs) {

  n_steps <- inputs$horizon_days
  dt <- 1 / 252
  n_paths <- N_PATHS_DEFAULT

  macro <- upcoming_macro_dates(inputs$start_date, n_steps)
  jump_days <- macro$steps

  rate_env <- fetch_rate_environment()
  credit_env <- fetch_credit_stress()

  hw_params <- list(
    r0 = rate_env$r0, r_bar = rate_env$r_bar,
    a = HW_A, b = HW_B,
    sigma1 = rate_env$hw_sigma1,
    sigma2 = HW_SIGMA2_RATIO * rate_env$hw_sigma1,
    rho = HW_RHO
  )

  credit_multiplier <- max(0.5, credit_env$spread_pct / HY_SPREAD_BASELINE)

  jump_params <- list(
    jump_days = jump_days,
    base_intensity = JUMP_BASE_INTENSITY,
    event_intensity = JUMP_EVENT_INTENSITY,
    jump_mean = JUMP_MEAN,
    jump_sd = JUMP_SD_BASE * credit_multiplier,
    event_jump_sd_mult = JUMP_EVENT_MULT
  )

  ou_params <- list(
    prem0 = inputs$prem0, theta_prem = inputs$prem0,
    kappa = OU_KAPPA_DEFAULT, sigma_prem = OU_SIGMA_DEFAULT
  )

  seasonality_drift_daily <- SEASONALITY_MAP[[inputs$seasonality]]

  sim <- build_price_paths(
    n_paths = n_paths, n_steps = n_steps, dt = dt,
    price0 = inputs$price0, duration = inputs$duration, convexity = CONVEXITY_DEFAULT,
    hw_params = hw_params, jump_params = jump_params, ou_params = ou_params,
    seasonality_drift_daily = seasonality_drift_daily
  )

  daily_rets <- sim$etf_price[, 2:(n_steps + 1), drop = FALSE] /
    sim$etf_price[, 1:n_steps, drop = FALSE] - 1
  sigma_daily <- mean(apply(daily_rets, 1, sd), na.rm = TRUE)

  X_shares <- inputs$order_notional / inputs$price0

  impact <- MARKET_IMPACT_PRESETS[[inputs$market_impact]]
  if (is.null(impact)) impact <- MARKET_IMPACT_PRESETS[[2]]

  ac <- almgren_chriss_trajectory(
    N = n_steps, tau = dt, X = X_shares, sigma_daily = sigma_daily,
    lambda = inputs$lambda, eta = impact$eta, gamma = impact$gamma
  )
  ac_remaining_frac <- ac$x[2:(n_steps + 1)] / X_shares
  trades_shares <- -diff(ac$x)

  th <- derive_thresholds(sim, ac_remaining_frac, p_min = P_MIN_DEFAULT)

  metrics <- compute_trade_metrics(sim, th$thresholds, inputs$price0)

  schedule <- tibble::tibble(
    day = 1:n_steps,
    date = business_day_sequence(inputs$start_date, n_steps),
    ac_shares = round(trades_shares, 0),
    ac_cum_pct = cumsum(trades_shares) / X_shares,
    target_fill_prob = th$p_fill_target,
    threshold_price = round(th$thresholds, 3),
    median_sim_price = round(apply(sim$etf_price[, 2:(n_steps + 1), drop = FALSE], 2, median), 3)
  )

  list(
    ticker = inputs$ticker,
    sim = sim,
    thresholds = th,
    metrics = metrics,
    schedule = schedule,
    X_shares = X_shares,
    sigma_daily = sigma_daily,
    jump_days = jump_days,
    jump_dates = macro$dates,
    ac_kappa_T = ac$kappa_T,
    rate_env = rate_env,
    credit_env = credit_env,
    market_impact = inputs$market_impact
  )
}
