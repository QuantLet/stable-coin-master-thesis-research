# Section 5.5 statistical reporting audit

- Analysis unit: calendar date, not 247,720 pair-window coefficients as independent replicates.
- Main window count: 2,252; each 90-day rolling window contains 11 target equations and 110 directed coin-to-coin coefficients.
- Network rule: positive non-zero coefficients only; inverse links remain separate. The edge interpretation is conditional and contemporaneous.
- Uncertainty: pointwise 95% circular moving-block bootstrap intervals (1,999 draws, 90-day blocks). Node-level out-minus-in contrasts use HAC(90) with Holm correction across all 11 nodes.
- Robustness: binary positive, unscaled positive, absolute-value weighted, imputation-unexposed, and chronological half-samples.
- Important limitation: bootstrap intervals describe the frozen sequence of selected networks. They do not quantify selection uncertainty from re-estimating Quantile Lasso inside each bootstrap draw.
- Important limitation: eigenvector and weighted closeness are sensitive to coefficient scaling. The main text reports rank changes rather than hiding them.
- Figure check: Figure 5.5 plots descriptive means, not inferential error bars; the source-data table and separate inferential tables provide uncertainty and exact corrections.
- Figure software: R 4.4.2 in the current runtime. Base R draws the chart; `ragg` and `svglite` export raster and SVG files. The PDF uses the editable base R vector device because the local Cairo/X11 dependency is unavailable.
