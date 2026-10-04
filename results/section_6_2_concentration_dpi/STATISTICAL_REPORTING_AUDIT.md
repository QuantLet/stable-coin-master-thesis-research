# Statistical reporting audit

## Design readout

- Outcome: logarithm of the primary robust-scaled strict DPI.
- Analysis unit: calendar date.
- Main sample: 2251 regression dates from 2 April 2020 to 31 May 2026 after the one-day lag.
- Main exposures: one-day-lagged standardized NHHI and Top-2 market share, estimated separately.
- Dependence: adjacent DPI observations share 89 of 90 estimation days.
- Inference: Newey-West HAC covariance, Bartlett kernel, lag 90.
- Multiplicity: Holm correction across the two primary concentration coefficients.

## Reviewer-risk checks

- PASS: the dependent variable, concentration measures, lag structure and sample are defined.
- PASS: effect sizes, 95% intervals, exact p values and sample sizes are exported.
- PASS: persistent concentration series are not interpreted from an uncontrolled levels regression alone.
- PASS: 30-day changes, 30-day lags, fixed coin coverage, event exclusion, Crypto FRM and on-chain controls are examined.
- PASS: 999-replication moving-block residual-bootstrap intervals use 90-day blocks and include zero for both primary coefficients.
- PASS: no causal language is used.
- P1 boundary: market capitalization includes price and circulating supply, so lagging reduces but does not eliminate reverse association.
- P1 boundary: the on-chain extension describes selected Curve and Uniswap pools rather than the full eleven-coin market.

## Statistical conclusion

The restricted models produce positive concentration coefficients, but these estimates do not survive calendar-regime controls. The primary HAC(90), block-bootstrap and matched-sample sensitivity analyses do not identify a stable association between market-capitalization concentration and the DPI.
