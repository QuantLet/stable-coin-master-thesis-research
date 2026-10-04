### 6.2 Concentration and Systemic Depegging Pressure

Market concentration can affect stablecoin risk in opposing ways. Reliance on a small number of issuers may increase exposure to common reserve, custody or redemption shocks. At the same time, concentration may reflect a shift towards larger stablecoins with deeper liquidity and stronger redemption networks. The sign of the relationship is therefore an empirical question rather than a maintained assumption.

The analysis combines the 2,252 daily DPI observations from 1 April 2020 to 31 May 2026 with the concentration measures constructed in Section 6.1. The dependent variable is the logarithm of the primary DPI. NHHI and the Top-2 market share are standardized and entered separately with a one-day lag. The adjusted specification is

\[
\log(DPI_t)=\alpha+\rho\log(DPI_{t-1})+\beta C_{t-1}
+\gamma'Z_{t-1}+\eta_y+\varepsilon_t,
\]

where \(C_{t-1}\) denotes one of the two concentration measures and \(Z_{t-1}\) contains total sample capitalization, the five macro-financial changes used in the DPI system and the number of coins with positive observed capitalization. Year fixed effects absorb broad market regimes. Because adjacent DPI estimates share 89 of 90 estimation days, inference uses Newey–West covariance with a Bartlett kernel and lag 90. Holm adjustment is applied across the NHHI and Top-2 coefficients. Lagging concentration reduces the same-day mechanical link between depegging and market capitalization, although the estimates remain associational.

The simpler dynamic regressions initially indicate a positive relationship. A one-standard-deviation increase in lagged NHHI is associated with a 1.45% higher DPI (95% HAC interval, 0.63% to 2.28%; Holm-adjusted \(p=0.001\)). After controlling for market size and the macro-financial variables, the corresponding estimates are 1.77% for NHHI and 1.70% for the Top-2 share, with both adjusted \(p=0.002\). These estimates do not survive the inclusion of year effects. In the primary specification, the NHHI coefficient is −1.52% (95% interval, −4.24% to 1.27%) and the Top-2 coefficient is −2.01% (−4.93% to 1.01%); both have Holm-adjusted \(p=0.380\). The positive association in the restricted models is therefore not separable from differences across broad market periods and does not represent a stable within-regime relationship.

**[Insert Table 6.2 here.]**

The sensitivity results lead to the same conclusion. Neither a 30-day concentration lag, the complete eleven-coin sample, exclusion of the FTX and SVB windows, nor controls for Crypto FRM and the additional on-chain indicators produces a concentration coefficient distinguishable from zero. In the 30-day change model, the Top-2 coefficient is negative and nominally significant (−6.72%; unadjusted \(p=0.047\)), but it does not survive correction across the two concentration measures (Holm-adjusted \(p=0.094\)). Moving-block bootstrap intervals with a 90-day block also include zero for both primary coefficients.

**[Insert Figure 6.2 here.]**

The results distinguish market structure from measured system pressure. The market can remain highly concentrated in USDT and USDC without concentration itself providing a stable explanation for variation in the DPI. Concentration should therefore be interpreted as a structural exposure that limits diversification, rather than as a sufficient daily indicator of systemic depegging pressure. The portfolio analysis that follows evaluates the practical implication of this exposure directly.

**Figure 6.2 | Market concentration and systemic depegging pressure.** Points report the estimated percentage change in the DPI associated with a one-standard-deviation increase in lagged NHHI or the Top-2 market share; horizontal bars are 95% Newey–West HAC(90) confidence intervals. In the change specification, one unit is a 30-day concentration change equal to one full-sample standard deviation of the corresponding level. The adjusted models control for lagged DPI, total sample capitalization, five macro-financial changes, the number of observed coins and year effects. The Crypto FRM model uses its available matched sample, while the on-chain specification uses 1,521 dates from 2 April 2022 to 31 May 2026. The 30-day-change result for the Top-2 share is nominally different from zero but not after Holm correction across the two concentration measures. Source data are provided with the figure.

**Table 6.2 | Dynamic association between market concentration and the DPI.** Concentration measures are standardized and entered separately. Reported effects are percentage changes in the DPI per one-standard-deviation increase in lagged concentration. Confidence intervals use Newey–West covariance with a Bartlett kernel and lag 90. Holm-adjusted p values define the two primary concentration coefficients as one comparison family.
