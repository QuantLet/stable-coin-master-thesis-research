# Code and data provenance

- The empirical universe is the same eleven-coin, reference-adjusted deviation panel used in Chapters 4 and 5.
- The five macro-financial series are converted to daily log changes after past-only carry-forward and then lagged by one calendar day.
- A frozen 2020-04-01 USDC equation is reproduced coefficient by coefficient before estimation. The maximum absolute discrepancy is recorded in `qa/frozen_input_alignment_QA.csv`.
- The Quantile-Lasso path algorithm is kept unchanged. The new code adds block-specific rolling estimation, out-of-sample scoring, HAC inference, moving-block bootstrap inference and R figure export.
- Read-only copies of the supplied FRM implementation and the two public repository scripts are retained in `reference_code/`.

