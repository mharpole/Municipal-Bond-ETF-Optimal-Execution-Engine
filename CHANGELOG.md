# Changelog

All notable changes to this app are documented here.

## v1.0.0 — 2026-08-16

First versioned release. Consolidates everything built through the initial
design and two revision passes:

**Core engine**
- Two-factor Hull-White short-rate simulation
- Merton jump-diffusion overlay on macro announcement days
- Ornstein-Uhlenbeck premium/discount-to-NAV process
- Seasonality drift regime selector
- Almgren-Chriss optimal execution trajectory (risk-aversion λ tunable)
- Per-tranche buy-limit threshold derivation from AC urgency × simulated
  price distribution
- Fill probability / expected cost / CVaR95 / safety-margin metrics

**Live data integration**
- ETF price auto-fetched per ticker via `tidyquant` (Yahoo Finance)
- Rate level & volatility auto-fetched from FRED (`DGS3MO`, `DGS10`) —
  replaces an earlier manually-typed MOVE index input
- Credit stress auto-fetched from FRED (`BAMLH0A0HYM2`, ICE BofA HY OAS) —
  replaces an earlier manually-typed CDX spread input
- Macro event days (CPI/FOMC) auto-detected against a hardcoded 2026
  calendar (sourced from federalreserve.gov and bls.gov) — replaces earlier
  manual date pickers

**Persistence**
- Local disk cache (`cache/`) for all pulled market data, 24-hour refresh
  window, with graceful fallback to a stale cache on fetch failure
- Named, saveable/loadable order configurations (`data/saved_orders.rds`),
  persisted across sessions

**Simplification**
- Sidebar inputs reduced from ~33 to 10 by auto-deriving or fixing (as
  documented internal constants) everything without a genuine free-data
  source or a real user-preference reading. Only genuine choices remain:
  asset, horizon, seasonality, risk aversion, order details, and a
  liquidity/market-impact preset.

---

*Versioning follows [SemVer](https://semver.org/): MAJOR.MINOR.PATCH.*
