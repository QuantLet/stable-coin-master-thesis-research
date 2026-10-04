### 6.3 Portfolio Designs and Evaluation Protocol

The concentration analysis does not establish that market structure predicts short-run changes in the DPI. The portfolio exercise therefore asks a narrower question: whether diversification based on conventional return covariance or coin-specific tail-risk information improves the out-of-sample stability of a stablecoin allocation. The exercise is evaluated as a risk-allocation problem rather than a return-maximisation problem.

Portfolio returns are constructed from the reference-adjusted price series used in the primary DPI. Prices of EURS, IDRT and PAXG are divided by EURUSD, IDRUSD and XAUUSD, respectively, while the remaining eight tokens retain the one-dollar benchmark. This treatment prevents movements in the reference asset from being counted as changes in stablecoin peg performance. The corresponding raw U.S.-dollar returns represent a different, dollar-investor exposure and are reserved for sensitivity analysis. Four missing price observations are carried forward from the most recent past observation before returns are calculated; no future value is used.

Four long-only portfolios are compared. The equal-weight portfolio (EW) provides a model-free benchmark. The global minimum-variance portfolio (GMV) minimises \(w_t'\Sigma^r_{t-1}w_t\), where \(\Sigma^r_{t-1}\) is the covariance matrix of reference-adjusted returns over the preceding 90 days. The linear tail-event-comovement portfolio (LTEC) minimises \(\lambda_{t-1}'w_t\), where \(\lambda_{t-1}\) contains the eleven coin-specific Quantile-Lasso penalties underlying the DPI. The quadratic tail-event-comovement portfolio (QTEC) combines the level and covariance of these penalties:

\[
\min_{w_t}\;\gamma
\frac{w_t'\Sigma^\lambda_{t-1}w_t}
{(w^{EW})'\Sigma^\lambda_{t-1}w^{EW}}
+(1-\gamma)
\frac{\lambda_{t-1}'w_t}
{\lambda_{t-1}'w^{EW}},
\]

where \(\Sigma^\lambda_{t-1}\) is estimated from the preceding 90 coin-level lambda vectors and \(\gamma=0.5\). Normalising both components by their equal-weight values makes the mixing parameter dimensionless. No expected-return constraint is imposed because short-window mean returns are dominated by rare depegs and subsequent reversals. All four portfolios satisfy \(w_{i,t}\geq0\), \(\sum_iw_{i,t}=1\), and \(w_{i,t}\leq0.30\) at each rebalance date. The common cap is particularly important for LTEC because a linear objective otherwise collapses to a single-token solution.

Weights are re-estimated every seven calendar days using a trailing 90-day window. A weight applied on day \(t\) uses only returns and lambdas observed through day \(t-1\). Between rebalancing dates, weights evolve with realised asset returns. The resulting evaluation sample contains 2,162 daily observations from 30 June 2020 to 31 May 2026. Turnover is calculated as one half of the absolute difference between the target weights and the pre-trade portfolio weights. This timing convention separates the information used to form the portfolio from the return used to evaluate it.

The primary performance measures are annualised volatility and the 5% Expected Shortfall (ES). Maximum drawdown, the Sortino ratio and turnover are reported as secondary diagnostics. Statistical comparisons are paired against EW because every strategy is evaluated on the same dates. Uncertainty is estimated with 1,999 moving-block bootstrap replications and a 90-day block length, preserving the dependence induced by the rolling lambda estimates. The six primary comparisons—three strategies against EW for volatility and ES—form one family and are adjusted using the Holm procedure. This protocol tests whether tail-risk information improves realised portfolio stability; it does not treat a lower in-sample objective value as evidence of superior out-of-sample performance.

**[Insert Table 6.3 here.]**

**Table 6.3 | Portfolio strategies and ex ante evaluation protocol.** EW denotes equal weight, GMV denotes global minimum variance, and LTEC and QTEC denote the linear and quadratic tail-event-comovement portfolios. All optimised weights use information available through the previous day, a 90-day estimation window, weekly rebalancing, long-only positions and a 30% per-token cap. Primary inference compares annualised volatility and 5% ES with EW using a paired 90-day moving-block bootstrap and Holm adjustment across six comparisons.
