# Municipal Bond ETF — Optimal Execution Engine

**v1.0.1**

An R Shiny dashboard that computes optimal passive buy-limit order
thresholds for a municipal bond ETF over a multi-day execution window,
combining a multi-factor Monte Carlo simulation with an Almgren-Chriss
optimal-execution framework.

## Files

| File | Purpose |
|---|---|
| `app.R` | The Shiny app — UI, server, and `shinyApp()` call in one file |
| `global_helpers.R` | Quant engine (Hull-White, jump-diffusion, OU, Almgren-Chriss) + all live-data fetching |

Both files must sit in the same folder. RStudio auto-detects `app.R` and
shows the **Run App** button — no renaming or copying required.

## Setup & run

```r
install.packages(c(
  "shiny", "bslib", "dplyr", "tibble", "tidyquant",
  "plotly", "DT", "shinyWidgets", "shinycssloaders"
))
```

Open `app.R` in RStudio and click **Run App** — or from the console:

```r
shiny::runApp("app.R")
```

It runs an initial simulation automatically on load (default VTEB
configuration), and re-runs whenever you change inputs and click
**Run Simulation**.

## What's a user input vs. pulled/derived automatically

| Input | Why it's still manual |
|---|---|
| Asset Picker, Horizon, Seasonality, Risk Aversion (λ) | Genuine user choices |
| ETF Price, Duration | Auto-filled per ticker, editable override |
| Premium/Discount (%) | No free NAV data source for muni ETFs |
| Order Notional, Schedule Granularity | Your order, your choice |
| Market Impact / Liquidity | No free liquidity data — a 3-option preset |

**Pulled automatically** (status lines under the ticker picker):
- **ETF price** — most recent close via `tidyquant::tq_get()` (Yahoo Finance).
- **Rate level & volatility** — FRED `DGS3MO` / `DGS10` Treasury yields.
  Realized short-rate volatility replaces the old manually-typed MOVE index.
- **Credit stress** — FRED `BAMLH0A0HYM2` (ICE BofA US High Yield OAS),
  replacing the old manually-typed CDX spread.
- **Macro event days (CPI/FOMC)** — checked automatically against a
  hardcoded 2026 calendar (Fed meeting dates from federalreserve.gov, CPI
  release dates from bls.gov).

**Fixed internally, not exposed at all** (see `global_helpers.R` §0a):
Hull-White mean-reversion speeds/correlation, the OU premium/discount
model's speed and volatility, jump-diffusion intensities/size, bond
convexity, the baseline fill-probability target, and the Monte Carlo path
count (fixed at 10,000). Structural constants with no free-data source and
no real "user preference" reading.

## Caching & saved configurations

- **`cache/`** — every pulled series is cached locally (24-hour refresh),
  falling back to a stale cache (flagged amber) rather than failing if a
  pull fails. Disposable, safe to `.gitignore`.
- **`data/saved_orders.rds`** — the "Saved Configurations" card in the
  sidebar saves/reloads the full configuration by name, across sessions.
  Durable — don't `.gitignore` it.

```
.gitignore suggestion:
cache/
```

## Known simplifications

- Tranche fill logic uses simulated **daily close** prices as a proxy for
  intraday fills.
- The Simulation Paths chart renders ~200 sampled paths for browser
  performance; all percentile bands and metrics use the full 10,000-path
  simulation.
- The macro calendar covers 2026 only.

## Performance

All simulation math is vectorized across paths; the only explicit loop is
over the (≤60) time steps. At 10,000 paths × 30 days, well under a second
of compute in base R.

---

## Changelog

### v1.0.1 — 2026-08-16
- **Fixed:** app wasn't recognized as a Shiny app in RStudio — the
  versioned filenames (`ui_v1.0.0.R` / `server_v1.0.0.R`) didn't match what
  RStudio looks for (`ui.R`+`server.R` or `app.R`).
- **Changed:** `ui.R` and `server.R` combined into a single `app.R`
  (standard single-file Shiny format, auto-detected by RStudio). No logic
  changes — same UI, same server code.
- **Changed:** README and CHANGELOG merged into one file.

### v1.0.0 — 2026-08-16
First versioned release.

- **Core engine:** two-factor Hull-White short-rate simulation, Merton
  jump-diffusion overlay, Ornstein-Uhlenbeck premium/discount process,
  seasonality drift regime, Almgren-Chriss optimal execution trajectory,
  per-tranche buy-limit thresholds, fill probability / expected cost /
  CVaR95 / safety-margin metrics.
- **Live data:** ETF price, Treasury rate level/vol (FRED), and HY credit
  spread (FRED) auto-fetched via `tidyquant` — replacing earlier manually-
  typed MOVE index and CDX spread inputs. Macro event days (CPI/FOMC)
  auto-detected against a hardcoded 2026 calendar instead of manual date
  pickers.
- **Persistence:** local disk cache for market data (24h refresh, stale
  fallback); named saveable/loadable order configurations persisted across
  sessions.
- **Simplification:** sidebar inputs reduced from ~33 to 10 by auto-deriving
  or fixing (as internal constants) everything without a genuine free-data
  source or real user-preference reading.

*Versioning follows [SemVer](https://semver.org/): MAJOR.MINOR.PATCH.*
