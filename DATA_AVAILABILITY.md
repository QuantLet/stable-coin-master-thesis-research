# Data and code availability

## Manuscript-ready statement

**Data and code availability.** The analysis code, frozen section-level analysis inputs, figure source data, numerical results, and quality-assurance records supporting Chapters 4–6 are provided in this repository. The frozen inputs permit the reported tables and figures to be regenerated without repeating the complete upstream data collection. Raw CoinGecko API exports are not redistributed because they are third-party data subject to CoinGecko's licensing and redistribution conditions. Researchers wishing to reconstruct the upstream price, market-capitalisation, and volume panels should obtain the corresponding data directly from CoinGecko under an appropriate licence and set the local data path as described in the repository documentation. The on-chain extension uses derived daily variables constructed from the public `MSCA-DN-Digital-Finance/stablecoin-onchain-data` project; source provenance is retained in the section package. The repository has not yet been assigned a persistent DOI. A versioned archival DOI should be added after the final GitHub release is deposited with Zenodo.

## What is included

- executable R code for Sections 4.4–6.5;
- exact frozen inputs consumed by each final section script;
- table and figure source data;
- final CSV, Markdown, PDF, SVG, and PNG outputs, except large TIFF duplicates;
- input manifests, model QA, session information, and repository-wide SHA-256 checksums;
- the consolidated development code used to assemble the final section packages.

## What is not included in the public upload

- raw CoinGecko API exports;
- third-party release archives or Parquet files that are not required once the derived daily inputs have been frozen;
- API credentials, private account data, or local caches;
- the thesis manuscript itself.

## 中文说明

公开 GitHub 包已经包含重建论文表格和图片所需的章节级冻结输入，但不包含 CoinGecko 的原始 API 导出。若只核对论文结果，直接运行 frozen 模式即可；若要从最上游重新构造行情面板，需要研究者自行按 CoinGecko 的许可条件获取数据，并设置 `COINGECKO_DATA_ROOT`。本地原始数据备份与公开包分开保存，不应直接上传。

## Persistent archiving checklist

1. Create the GitHub repository and publish a tagged release, for example `v1.0.0`.
2. Connect the repository to Zenodo and archive the release.
3. Add the issued DOI to this file, `CITATION.cff`, and the thesis Data Availability statement.
4. Record the exact Git commit and release tag cited by the thesis.
