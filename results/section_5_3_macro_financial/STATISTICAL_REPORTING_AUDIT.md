# Statistical reporting audit

## Scope and unit of analysis

- Input: 123,860 macro-factor coefficients from 11 target equations, five predictors and 2,252 rolling dates.
- Independent sampling dimension for inference: calendar date, after cross-target aggregation.
- Dependence adjustment: Newey-West HAC covariance with Bartlett kernel and lag 90, matching the overlap of the 90-day rolling estimator.

## Multiple-comparison families

- Ten pairwise factor-frequency contrasts: Holm correction across 10 tests.
- Five factor-specific sign-balance tests: Holm correction across five tests.
- Five factor-specific target-heterogeneity tests: Holm correction across five tests.
- The global five-factor equality test is a single omnibus test and is not combined with the pairwise family.

## Effect size and uncertainty

- Main table reports frequencies and HAC 95% confidence intervals rather than significance stars alone.
- Sign conclusions report positive-minus-negative percentage-point differences, intervals and Holm-adjusted p values.
- Coefficient-weight shares are explicitly labelled descriptive.

## Interpretation boundary

- No result is written as causal transmission.
- Active-set membership is not treated as predictive contribution.
- Absolute coefficient weights are comparable within the robust-scaled target-date equation but remain affected by L1 shrinkage and substitution among correlated predictors.
- The severe-versus-very-low DPI table is descriptive because DPI states and active sets arise from the same estimation system.

## Reviewer-risk note

The largest remaining identification risk is the word “transmission” in the proposed section title. The recommended title replaces it with “conditions and stablecoin tail risk.” Formal external-versus-internal attribution is deferred to Section 5.4, where macro-only, coin-only and joint models can be compared on the same loss criterion.
