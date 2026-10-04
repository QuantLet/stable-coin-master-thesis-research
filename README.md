<div style="margin: 0; padding: 0; text-align: center; border: none;">
  <a href="https://quantlet.com" target="_blank" style="text-decoration: none; border: none;">
    <img src="https://github.com/StefanGam/test-repo/blob/main/quantlet_design.png?raw=true" alt="Quantlet — where knowledge meets code" width="100%" style="margin: 0; padding: 0; display: block; border: none;" />
  </a>
</div>

```text
Name of Quantlet: FRMStablecoinRisk

Published in: Stablecoin_Systemic_Risk_Thesis

Description: Reproducible empirical analysis of systemic depegging pressure in stablecoin markets using reference-asset-adjusted peg deviations, rolling Quantile Lasso, Financial Risk Meter measures, event studies, tail-risk networks, macro-financial and on-chain variables, market concentration, and out-of-sample portfolio evaluation.

Keywords: stablecoin, depegging risk, systemic risk, Financial Risk Meter, Quantile Lasso, tail-risk network, macro-financial transmission, on-chain liquidity, market concentration, portfolio resilience

Author: Kewen FAN

Submitted: Kewen FAN, 2026-10-04

Datafile: Section-specific frozen analysis inputs are stored in results/*/source_data/input; derived source data, tables, and quality-assurance files are stored within the corresponding section folders. Raw CoinGecko API exports are not included in the public package because they are third-party licensed data.

Input: The package uses reference-asset-adjusted stablecoin price and peg-deviation panels, rolling Quantile-Lasso coefficients and DPI series, crypto-market and macro-financial variables, derived on-chain liquidity measures, stablecoin market-capitalisation data, and leakage-free portfolio inputs. The bundled frozen inputs reproduce the reported tables and figures without requiring the restricted raw CoinGecko exports.

Output: The Quantlet generates event-study estimates, stablecoin-to-stablecoin tail-risk links, active-driver and transmission-channel results, centrality and microstructure analyses, concentration-DPI tests, and out-of-sample portfolio and event-resilience tables and figures, together with source-data and QA records.

Example: results/section_4_4_reference_adjusted_event_study/figures/Figure_4_4_FRM_Event_Responses_Redesigned.png, results/section_5_4_external_vs_internal/figures/Figure_5_4_Relative_Tail_Risk_Information.png, results/section_6_5_event_resilience/figures/Figure_6_5_Portfolio_Drawdowns_Tail_Events.png
```

# Stablecoin systemic risk: replication package

This repository contains the code, frozen analysis inputs, tables, figures, and quality-assurance files used for the empirical results in Chapters 4–6 of Kewen Fan's thesis on systemic depegging pressure in stablecoin markets.

The package follows the manuscript's final empirical sequence:

1. construct a reference-asset-adjusted stablecoin depegging pressure index (DPI);
2. study event responses and the relation between stablecoin and broader crypto-market risk;
3. decompose active risk drivers into stablecoin and macro-financial channels;
4. compare external drivers with within-stablecoin links and extend the model with selected on-chain variables; and
5. relate market concentration to DPI and evaluate tail-risk-aware portfolios out of sample.

## Quick start

Use R 4.4 or a compatible recent version. From the repository root:

```bash
Rscript environment/install_packages.R
Rscript run_all.R
```

The installer uses the repository-local `environment/R-library` directory when the system library is not writable. The root reproduction script detects that directory automatically; it is excluded from Git.

The default command performs a non-destructive repository check. It does not rerun the computationally intensive rolling Quantile-Lasso estimation.

To regenerate tables and figures from the frozen section inputs:

```bash
REPRO_MODE=frozen Rscript run_all.R
```

To request the full estimation sequence, including rolling FRM and microstructure models:

```bash
REPRO_MODE=full FRM_CORES=4 FRM_MICRO_CORES=4 Rscript run_all.R
```

The full mode requires locally obtained CoinGecko exports for Section 4.5. Set their location before running:

```bash
export COINGECKO_DATA_ROOT=/absolute/path/to/coingecko
```

Section-specific commands and expected outputs are listed in [REPRODUCTION_ORDER.md](REPRODUCTION_ORDER.md). Each section also contains a Chinese README documenting its model, numerical results, and interpretation limits.

## Repository structure

```text
.
├── results/       # chapter-specific code, frozen inputs, tables, figures, and QA
├── work/          # consolidated upstream/local research code used to build the packages
├── environment/   # package installation and environment notes
├── scripts/       # repository validation utilities
├── manifest/      # file inventories and SHA-256 checksums
└── data/          # instructions for restricted third-party raw inputs
```

The `results/` folders are the recommended replication entry points. The `work/` folders preserve development provenance and are not the preferred route for regenerating final manuscript exhibits.

Repository-wide inventories are stored in `manifest/`. Rebuild them after any change with `python3 scripts/build_manifest.py`.

## Reproducibility levels

- **Repository check** verifies required files, section structure, and checksums.
- **Frozen-input reproduction** rebuilds analysis outputs from the exact section-level inputs used in the thesis. This is the recommended reviewer workflow.
- **Full reconstruction** rebuilds upstream panels and re-estimates rolling models. It is slower and may require third-party raw data that cannot be redistributed in this public package.

All final section folders preserve input manifests or checksums. Inference uses the methods documented in the corresponding section, including HAC standard errors, moving-block bootstrap procedures, and family-wise Holm adjustments where applicable.

## Data and licensing

Read [DATA_AVAILABILITY.md](DATA_AVAILABILITY.md), [THIRD_PARTY_DATA.md](THIRD_PARTY_DATA.md), and [LICENSE_NOTICE.md](LICENSE_NOTICE.md) before publishing or redistributing this repository. Raw CoinGecko API exports are intentionally excluded from the public package. No software license has been assigned automatically.

## Citation

Citation metadata are provided in [CITATION.cff](CITATION.cff). After creating a public GitHub release, archive that release with Zenodo and add the issued DOI to both the manuscript and this repository.

## 中文说明

本目录已经按论文最终实证顺序整理。建议审稿复现时直接使用各章节 `source_data/input/` 中的冻结输入；它们可以重建正文表图，而不必重复运行耗时的滚动 Quantile-Lasso。CoinGecko 原始导出受第三方再分发条件约束，因此未放入公开上传包；本地备份另行保存，不应直接上传 GitHub。
