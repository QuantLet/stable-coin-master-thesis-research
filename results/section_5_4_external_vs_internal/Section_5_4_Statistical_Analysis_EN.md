### Statistical analysis for Section 5.4

For target stablecoin \(i\), four 5% quantile specifications were estimated on each trailing 90-day window: an intercept-only benchmark, a five-variable macro-only model, a ten-variable coin-only model and a joint model containing both blocks. Stablecoin outcomes were reference-adjusted signed peg deviations. The main conditional specification used contemporaneous deviations of the other stablecoins and one-calendar-day-lagged macro-financial changes. Every variable was centred by its training-window median and divided by its median absolute deviation; a within-window standard deviation and then one were used only when the MAD was zero. The penalty path and strict minimum finite GACV rule were unchanged from the primary DPI estimation.

Two performance measures were reported. Conditional skill was defined as \(1-L_m^{GACV}/L_0^{GACV}\), where \(L_0^{GACV}\) and \(L_m^{GACV}\) are the date-level mean selected GACV values for the benchmark and model \(m\). For out-of-sample evaluation, model parameters and scaling constants were estimated on dates \(t-90,\ldots,t-1\) and evaluated at \(t\) using the quantile check loss \(\rho_{0.05}(u)=u\{0.05-\mathbf{1}(u<0)\}\). Losses were calculated on the training-window standardized outcome scale and then averaged across the 11 targets, so calendar date was the sampling unit. A second out-of-sample specification lagged the stablecoin block by one calendar day as well as the macro block.

Incremental coin information was measured by \(L_M-L_J\), and incremental macro information by \(L_C-L_J\), where \(M\), \(C\) and \(J\) denote the macro-only, coin-only and joint models. The two-block GACV reduction was also allocated using

\[
\phi_M=\tfrac{1}{2}\{(L_0-L_M)+(L_C-L_J)\},\qquad
\phi_C=\tfrac{1}{2}\{(L_0-L_C)+(L_M-L_J)\}.
\]

Means, loss differentials and skill ratios were estimated from date-level series. Confidence intervals used Newey–West HAC covariance with a Bartlett kernel and lag 90. The three pre-specified block contrasts within each information-timing specification formed one Holm-adjusted family. Circular moving-block bootstrap intervals used a 90-day block and 1,999 replications. Robustness checks excluded the 539 Chapter 4 output windows containing past-filled peg observations. Quantile calibration was assessed from the proportion of observations below the predicted 5% quantile.

