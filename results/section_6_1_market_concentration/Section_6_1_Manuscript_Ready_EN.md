### 6.1 Stablecoin Market-Capitalization Concentration

Market capitalization is unevenly distributed across stablecoins, making aggregate market size an incomplete description of market structure. Earlier evidence also documents that the stablecoin segment is substantially more concentrated than the broader crypto-asset market and remains dominated by a small number of fiat-backed tokens (Mayer and Bofinger, 2024; Kosse et al., 2023). We therefore measure concentration within the eleven-coin thesis sample. For date \(t\), the market share of coin \(i\) is \(s_{i,t}=MC_{i,t}/\sum_j MC_{j,t}\), and the Herfindahl-Hirschman Index is \(HHI_t=\sum_i s_{i,t}^2\). Because the number of coins with an observed positive market capitalization varies between eight and eleven, we report the normalized index

\[
NHHI_t=\frac{HHI_t-1/N_t}{1-1/N_t},
\]

together with the combined share of the two largest coins. Missing market capitalization is not replaced by zero or carried forward. These measures describe concentration within the selected sample rather than the entire stablecoin market.

Figure 6.1 shows a non-monotonic change in market structure. Median NHHI declined from 0.640 in 2020 to 0.291 in 2022, before rising to 0.513 in 2026. USDT remained the largest coin on every day of the sample. By 31 May 2026, USDT represented 69.4% of sample market capitalization and USDC 28.0%, giving a combined share of 97.3%. The corresponding NHHI was 0.516, below its value at the beginning of 2020. The two indicators therefore capture different changes: the decline in USDT's initial dominance reduced the normalized HHI, while the contraction of smaller competitors concentrated almost all remaining capitalization in USDT and USDC.

The evidence is thus more consistent with a transition from single-coin dominance to a highly concentrated two-coin structure than with a continuous rise in one-dimensional concentration. This distinction matters for the subsequent analysis because a high top-two share can create common exposure to two large issuers even when the HHI is below its earlier peak. Market-capitalization concentration alone does not establish that this structure raises depegging pressure; Section 6.2 tests that association using the primary DPI.

**[Insert Figure 6.1 here.]**

**Figure 6.1 | Stablecoin market-capitalization concentration.** The lines report 31-day rolling medians of the daily normalized Herfindahl-Hirschman Index and the combined market share of the two largest stablecoins. Daily shares are calculated from positive U.S.-dollar market capitalizations for the eleven-coin thesis sample from 1 January 2020 to 31 May 2026. The normalization uses the number of coins observed on each date. Missing market capitalization is neither set to zero nor carried forward. The underlying daily observations, rather than the smoothed series, are used for the reported summary statistics.
