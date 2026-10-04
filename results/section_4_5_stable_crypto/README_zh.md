# 4.5 Stablecoin–Crypto 风险比较：代码与结果包

## 研究问题

本包检验 Stable FRM 与 Crypto FRM 是否共享风险状态、是否具有方向性样本外预测信息，以及六类冲击在两个市场中的反应是否不同。它延续 PPT 的 15 币 Crypto 网络、四个预测期限和 63 日滚动设定，并与 4.4 的参考资产调整和事件设计保持一致。

## 主文件

- `Section_4_5_Writing_Logic_and_Empirical_Design_zh.md`：中文写作逻辑、模型、主要结果和可写创新点。
- `Section_4_5_Manuscript_Ready_EN.md`：可直接进入论文的英文正文。
- `figures/Figure_4_5_Stablecoin_Crypto_Risk.*`：SVG、PDF、600 dpi TIFF 和 300 dpi PNG。
- `figures/Figure_4_5_caption.txt`：完整图注。
- `tables/Table_4_5d_OOS_R2.csv`：主样本外 \(R^2\)。
- `tables/Table_4_5e_OOS_Forecast_Tests.csv`：Clark–West、HAC 损失差与四期限 Holm 校正。
- `tables/Table_4_5f_Paired_Event_Effects_0_30_HAC90.csv`：Stable、Crypto 及直接差异的 0–30 日事件效应。
- `tables/Table_S4_5_OOS_Definition_Robustness.csv`：严格 GACV 与 9 币完整样本稳健性。
- `results/Section_4_5_OOS_Predictions.csv`：逐预测起点的主预测值。
- `qa/`：覆盖率、方法范围、图形预检、文件校验和 MD5 清单。

## FRM 口径

Stable FRM 的主口径为参考资产调整后的 11 币数值筛查序列；严格最小 GACV 为数值稳健性。Crypto FRM 使用 PPT 所列 15 个资产、90 日滚动窗口、5% 分位数和 25 个路径步。主系统保留窗口内可用资产，不填补本地缺失值；9 币完整覆盖系统作为样本稳健性。作者提供的 `FRM_Statistics_Algorithm.R` MD5 为 `b500265f1a91d231f127fe046a337bca`。

Stable FRM 的 GitHub 包装逻辑核对至 `fankewe/master-thesis-code` 提交 `b827743bd4d47deb04bfdcf6777ef44ae2a2d8f5`。该仓库没有现成的 Crypto FRM 输入或估计结果，因此本包是在同一算法和参数下对本地非稳定币数据重新估计，而不是把仓库中不存在的序列当成既有结果。

## 复现

需要 R 4.4 或兼容版本。FRM 估计仅依赖 base R；论文图需要 `ggplot2`、`patchwork`、`scales`、`svglite` 和 `ragg`。

```r
install.packages(c("ggplot2", "patchwork", "scales", "svglite", "ragg"))
```

在终端指定 CoinGecko 本地数据目录后运行：

```bash
export COINGECKO_DATA_ROOT=/path/to/coingecko
Rscript code/run_all.R
```

若保留包内已经估计好的 Crypto FRM，只重跑结果、表格和图形，并跳过需要第三方原始导出的面板构建：

```bash
FRM_SKIP_RAW_BUILD=1 FRM_SKIP_ESTIMATION=1 Rscript code/run_all.R
```

完整重跑顺序为：

1. `01_build_crypto_panel.R`：构建 15 币价格、市值和成交量面板。
2. `02_estimate_crypto_frm.R`：估计动态 15 币与完整 9 币 Crypto FRM。
3. `03_build_outcomes.R`：构建参考锚稳定币结果和滞后市值加权市场收益。
4. `04_cross_market_analysis.R`：状态、预测、稳健性和成对事件检验。
5. `05_plot_figure_4_5.R`：只用 R 导出论文图及 Source Data。
6. `06_finalize_QA.R`：执行最终完整性检查并生成 MD5 清单。

## 关键推断约束

- 预测起点 `t` 的训练样本只允许使用满足 `s+h≤t` 的历史目标，防止长周期前视泄漏。
- 样本外基准为训练样本历史均值；报告 Campbell–Thompson \(R^2\)。
- 嵌套预测比较采用 Clark–West，Stable 与 Crypto 的非嵌套比较采用 HAC 损失差。
- 每个预测结果、估计窗和比较家族内，对四个期限进行 Holm 校正。
- 事件模型沿用 4.4 的五阶段、年度固定效应、线性趋势、HAC(90) 和六事件 Holm 校正。
- 本地 Crypto 数据截至 2026-04-06；霍尔木兹 0–30 日完整，31–60 日不完整并被置为缺失。
