# Municipal Bond ETF — Optimal Execution Engine

**Version 1.0.0** — see [CHANGELOG.md](./CHANGELOG.md) for release history.

A research-grade R Shiny dashboard that computes optimal passive buy-limit
order thresholds for a municipal bond ETF over a multi-day execution window,
combining a multi-factor Monte Carlo simulation with an Almgren-Chriss
optimal-execution framework.

## Files

| File | Purpose |
|---|---|
| `global_helpers_v1.0.0.R` | Quantitative engine: Hull-White (2F), Merton jump-diffusion, OU premium/discount, Almgren-Chriss optimizer, threshold + metric derivation, all live-data fetching |
| `ui_v1.0.0.R` | bslib dashboard UI (sidebar config + 3-tab main panel) |
| `server_v1.0.0.R` | Reactive pipeline wiring inputs → engine → table/plots/metrics |
| `CHANGELOG.md` | Version history |

## Setup

```r
install.packages(c(
  "shiny", "bslib", "dplyr", "tibble", "tidyquant",
  "plotly", "DT", "shinyWidgets", "shinycssloaders"
))
```

Shiny requires the exact filenames `ui.R` / `server.R` (or a single `app.R`)
to auto-detect an app — the version suffix is for the repo/release history,
not for direct execution. Copy (don't rename in place, so the versioned
originals stay intact) into working filenames before running:

```r
file.copy("global_helpers_v1.0.0.R", "global_helpers.R", overwrite = TRUE)
file.copy("ui_v1.0.0.R", "ui.R", overwrite = TRUE)
file.copy("server_v1.0.0.R", "server.R", overwrite = TRUE)
shiny::runApp(".")
```

(`ui.R`/`server.R`/`global_helpers.R` are also listed in the suggested
`.gitignore` below, so the working copies don't end up committed alongside
the versioned source of truth.)

The app runs an initial simulation automatically on load (with the default
VTEB configuration), and re-runs whenever you change inputs and click
**Run Simulation**.

## What's a user input vs. what's pulled/derived automatically

Earlier drafts asked for ~30 inputs, including a manually-typed MOVE index
level and CDX spread as proxies for rate and credit volatility. Both are
gone — replaced with real data pulled via `tidyquant`, and most of the
underlying model's structural constants are now fixed internally rather
than exposed as raw numbers. What's left in the sidebar (~10 inputs) is
either a genuine user preference or something with no free-data source:

| Input | Why it's still manual |
|---|---|
| Asset Picker, Horizon, Seasonality, Risk Aversion (λ) | Genuine user choices |
| ETF Price, Duration | Auto-filled per ticker (see below), editable override |
| Premium/Discount (%) | No free NAV data source for muni ETFs — manual |
| Order Notional, Schedule Granularity | Your order, your choice |
| Market Impact / Liquidity | No free liquidity/impact data — a 3-option preset (replaces two raw impact coefficients) |

**Pulled automatically, shown as status lines under the ticker picker:**

- **ETF price** — most recent close via `tidyquant::tq_get()` (Yahoo Finance).
- **Rate level & volatility** — FRED series `DGS3MO` (3-month T-bill, short
  rate) and `DGS10` (10-year, long-run anchor), via tidyquant's FRED
  support. Realized volatility of the 3-month yield **replaces the old
  manual MOVE index input** — this is literally the thing MOVE
  approximates, sourced directly instead of typed in.
- **Credit stress** — FRED series `BAMLH0A0HYM2` (ICE BofA US High Yield
  Index Option-Adjusted Spread), a real published credit spread that
  **replaces the old manual CDX spread input**.
- **Macro event days (CPI/FOMC)** — no longer date pickers. A hardcoded
  2026 calendar (Fed meeting dates from federalreserve.gov, CPI release
  dates from bls.gov, current as of Aug 2026) is checked automatically
  against your execution horizon.

**Fixed internally, not exposed at all** (see `global_helpers.R`, section
0a, for the exact values): Hull-White mean-reversion speeds and factor
correlation, the premium/discount OU model's speed and volatility (its
target level ties to your entered premium/discount instead), jump-diffusion
intensities and size, bond convexity, the baseline fill-probability target,
and the Monte Carlo path count (fixed at 10,000, matching the original
spec). These are structural modeling constants with no simple
market-observable proxy and no meaningful "user preference" reading — they
were cluttering the sidebar without giving you real control.

## Caching & saved configurations

**`cache/` — market data cache.** Every pulled series (per-ticker prices,
plus the shared `DGS3MO` / `DGS10` / `BAMLH0A0HYM2` series) is cached to
`cache/*.rds` with a fetch timestamp and reused for 24 hours before
re-pulling. If a fresh pull fails, the app falls back to the stale cache
(flagged amber in the status lines) rather than failing outright. Safe to
delete or `.gitignore` — it just gets rebuilt.

**`data/saved_orders.rds` — saved configurations.** The "Saved
Configurations" card at the top of the sidebar saves and reloads the full
current configuration by name, including across sessions. This is durable
user data — don't `.gitignore` it.

```
.gitignore suggestion:
cache/
ui.R
server.R
global_helpers.R
```

## Known simplifications

- Tranche fill logic uses simulated **daily close** prices as a proxy for
  "would a resting limit order have filled that day," rather than a modeled
  intraday path.
- The **Simulation Paths** chart renders a random sample of ~200 individual
  paths for browser performance, but all percentile bands and every metric
  in the app are computed from the full Monte Carlo simulation (10,000
  paths).
- The macro event calendar covers 2026 only; a horizon that runs past
  Dec 2026 will simply show no flagged event days beyond that point.

## Performance

All simulation math is vectorized across paths; the only explicit loop is
over the (≤60) time steps, required because each day's state depends on the
prior day's. At 10,000 paths × 30 days this is well under a second of
compute in base R.
