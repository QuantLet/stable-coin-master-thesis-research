### 6.4 Leakage-Free Out-of-Sample Portfolio Performance

Table 6.4 reports performance over 2,162 common out-of-sample days. The clearest result is obtained for the global minimum-variance portfolio (GMV). Its annualised volatility is 3.14%, compared with 10.93% for equal weight (EW), while its 5% Expected Shortfall (ES) is 39.03 basis points, compared with 101.39 basis points for EW. The paired differences are −7.80 percentage points for volatility (95% moving-block bootstrap interval: −11.92 to −2.41) and −62.36 basis points for ES (−94.78 to −27.70). Both comparisons remain significant after Holm adjustment (*p* = 0.006). The maximum drawdown also falls from 15.63% to 2.60%, although drawdown is treated as a secondary descriptive outcome.

**[Insert Table 6.4 here.]**

The DPI-based allocations do not produce the same improvement. LTEC has nearly the same volatility as EW (10.76%) and a higher ES (125.74 basis points); neither difference is statistically significant after adjustment. QTEC also leaves volatility essentially unchanged (10.89%) and raises ES to 118.21 basis points. The QTEC–EW ES difference is 16.82 basis points (unadjusted *p* = 0.016), but it does not meet the family-wise threshold after correcting the six primary comparisons (Holm-adjusted *p* = 0.064). Figure 6.4 summarises these results as risk ratios. The confidence intervals for both GMV outcomes lie below one, whereas the intervals for the LTEC and QTEC outcomes do not support a reduction relative to EW.

**[Insert Figure 6.4 here.]**

Implementation costs reinforce this distinction. Mean turnover per rebalance is 3.45% for GMV, but 49.93% for LTEC and 22.49% for QTEC. Under the common 10-basis-point one-way cost scenario, the annualised net returns of LTEC and QTEC fall to −4.25% and −2.40%, respectively. This cost calculation is a sensitivity scenario rather than an estimate of realised execution costs. It is not the source of the weak DPI-portfolio result: neither strategy improves the prespecified risk measures before costs.

The comparison therefore separates measurement from portfolio use. Conventional covariance diversification produces a large out-of-sample reduction in both average volatility and lower-tail loss. By contrast, directly minimising coin-level DPI penalties does not generate a stable risk ranking that can be converted into weekly portfolio weights. This is consistent with the Chapter 5 evidence that the penalties describe contemporaneous lower-tail structure more reliably than they predict next-period losses. The result does not invalidate the DPI as a measure of systemic depegging pressure; it limits the stronger claim that its cross-sectional components provide an immediately tradable hedging signal.

