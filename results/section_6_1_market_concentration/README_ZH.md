# 第6.1节 市值集中度

本目录使用论文既定十一币样本的CoinGecko日度市值，完成2020-01-01至2026-05-31的HHI、NHHI、Top-1、Top-2和有效币种数计算。原PPT及本地脚本的研究方向被保留，但修正了缺失市值填零、原始HHI误标为normalized HHI，以及累计市值HHI缺乏明确经济含义的问题。

## 复现

在项目根目录运行：

```bash
Rscript outputs/section_6_1_market_concentration/code/01_build_market_concentration.R
```

脚本默认查找公开复现包的`data/raw/coingecko_subset`，也可用`FRM_COINGECKO_ROOT`指定研究者自行取得的CoinGecko数据根目录。公开包不再分发原始CoinGecko导出；若原始目录不可用，脚本会自动改用本节冻结的`source_data/market_cap_selected_long.csv`，因此表图仍可独立复现。PAXG在本地文件中缺少的最后55天已经以冻结JSON保存在`source_data/input/`，复现时不再调用网络。

## 主要文件

- `Section_6_1_Manuscript_Ready_EN.md`：可直接放入论文的英文正文和图注。
- `Section_6_1_Writing_Logic_ZH.md`：写作逻辑、指标定义及相对旧代码的修正。
- `figures/Figure_6_1_Stablecoin_Market_Concentration.*`：SVG、PDF、600 dpi TIFF和预览PNG。
- `tables/Table_6_1_Daily_Concentration.csv`：未平滑日度指标。
- `tables/Table_S6_1_Annual_Concentration.csv`：年度描述表。
- `tables/Table_S6_1_Coin_Shares.csv`：币种份额及覆盖。
- `qa/concentration_QA.csv`：边界、份额和缺失处理检查。
- `References_Section_6_1.bib`：核验后的相关参考文献。
