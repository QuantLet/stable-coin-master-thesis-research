# Third-party data and provenance

This document records the upstream sources used in the thesis. It is a provenance record, not a grant of rights to redistribute third-party material.

| Source | Variables used | Role in analysis | Public-package treatment |
|---|---|---|---|
| CoinGecko API / local exports | stablecoin and crypto prices, market capitalisation, trading volume | peg panels, crypto FRM, and concentration measures | raw exports excluded; frozen analysis inputs and derived results retained where required for verification |
| FRED and market-data extracts retained in the section inputs | VIX, DXY, interest-rate, equity-market, and selected reference-asset series | macro-financial controls and reference-asset adjustment | exact files used by the analysis are retained with their original source labels |
| `MSCA-DN-Digital-Finance/stablecoin-onchain-data` | Curve 3pool imbalance and transactions; Uniswap v3 liquidity proxy | Chapter 5 on-chain cross-check and microstructure extension | derived daily CSV retained; upstream repository and cleaning provenance retained |
| Author-constructed outputs | DPI, Quantile-Lasso coefficients, concentration measures, portfolio returns, tables, figures | final thesis evidence | included |

## CoinGecko

CoinGecko distinguishes between commercial and custom redistribution rights, and its standard API terms restrict redistribution unless an appropriate agreement applies. The public package therefore excludes the raw API export tree. The code accepts a user-supplied path through `COINGECKO_DATA_ROOT` or `FRM_COINGECKO_ROOT`.

Official information:

- <https://www.coingecko.com/en/api_terms>
- <https://support.coingecko.com/hc/en-us/articles/16760512207257-What-Are-the-Differences-Between-Commercial-and-Custom-Licenses>
- <https://www.coingecko.com/en/api/enterprise/data-license>

## On-chain extension

The derived daily on-chain file traces to:

- <https://github.com/MSCA-DN-Digital-Finance/stablecoin-onchain-data>

The local package does not assign a licence to the upstream project. Before broad redistribution of upstream raw files, verify the current repository licence and attribution requirements. The retained daily variables are accompanied by the cleaning code and scope limitations used in the thesis.

## User responsibility before release

Confirm that the intended GitHub visibility and the author's data subscriptions permit publication of every third-party-derived file. If a licence does not permit redistribution, keep the affected file outside Git history and provide retrieval and processing instructions instead.
