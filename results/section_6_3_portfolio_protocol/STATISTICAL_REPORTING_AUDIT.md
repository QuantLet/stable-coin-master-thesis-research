# Statistical reporting audit

## Design readout

- Independent unit: one common calendar-day vector of portfolio returns.
- Evaluation sample: 2,162 days, 2020-06-30 to 2026-05-31.
- Repeated structure: daily returns and portfolio weights are serially dependent; lambda inputs arise from overlapping 90-day windows.
- Primary endpoints: annualised volatility and 5% Expected Shortfall.
- Secondary endpoints: maximum drawdown, Sortino ratio and turnover.
- Primary comparison family: GMV, LTEC and QTEC versus EW for two primary endpoints, giving six comparisons.

## Inference

All strategies are evaluated on identical dates and are resampled jointly. Confidence intervals and two-sided p values use 1,999 moving-block bootstrap replications with 90-day blocks. Holm adjustment is applied across the six primary comparisons. Maximum-drawdown p values are secondary and unadjusted.

## Reviewer-facing boundary

The bootstrap quantifies sampling uncertainty under the observed dependence structure; it does not establish that the portfolio rule would remain optimal in another exchange, liquidity regime or transaction-cost environment. The 30% cap is imposed at rebalance dates; realised weights may temporarily exceed the cap between rebalances because of price movements. Four missing price observations are handled by past-only carry and should be checked by excluding the affected dates in the later robustness section.

## Main statistical conclusion

GMV improves both prespecified primary endpoints relative to EW after Holm correction. LTEC and QTEC do not. The result therefore supports a covariance-diversification claim, not a claim that coin-level DPI lambdas forecast investable tail losses.

