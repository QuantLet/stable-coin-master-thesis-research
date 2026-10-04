# Terminology ledger

| Canonical term | Definition and use |
|---|---|
| reference-adjusted return | Return calculated after expressing EURS, IDRT and PAXG relative to EURUSD, IDRUSD and XAUUSD; primary portfolio outcome |
| EW | Equal-weight portfolio |
| GMV | Global minimum-variance portfolio based on trailing reference-adjusted return covariance |
| LTEC | Linear tail-event-comovement portfolio based on the lagged vector of coin-specific Quantile-Lasso penalties |
| QTEC | Quadratic tail-event-comovement portfolio combining lagged lambda exposure and trailing lambda covariance |
| lambda | Coin-specific Quantile-Lasso penalty underlying the DPI; not a depeg probability |
| 5% Expected Shortfall | Mean portfolio loss conditional on returns being at or below the empirical 5% quantile |
| moving-block bootstrap | Paired resampling of contiguous 90-day blocks across all portfolio strategies |

