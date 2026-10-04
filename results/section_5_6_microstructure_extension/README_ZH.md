# 第五章链上微观变量扩展

## 研究问题

在既定 11 币、5% 分位数、90 日滚动 Quantile-Lasso 规格下，前一日可观察到的链上交易与流动性变量，是否在原有十个币际变量和五个宏观变量之外提供增量信息？这是第 5.4 节联合模型的匹配样本扩展，不替代第 4 章主 DPI，也不把三个以太坊池解释成全市场微观结构。

## 两组完全匹配的模型

- 原联合模型：目标币以外的十个稳定币参考资产调整后的有符号 peg deviation，加五个滞后一日的宏观变量。基准预测逐行读取第 5.4 节冻结输出，不改动原结果。
- 扩展联合模型：上述十五个变量，再加入三项滞后一日的链上变量：Curve 3pool 当日收盘库存失衡度、`log1p` Curve 3pool 日交易次数、`log1p` Uniswap v3 USDC/USDT 0.01% 费率池现价附近 ±25 tick 的美元库存代理。库存不等于中心化交易所订单簿深度或真实买卖价差。
- 两组使用相同的目标币、日期、90 日训练窗口、5% 分位数、窗口 median/MAD 缩放、25 步路径和严格最小有限 GACV。扩展与基准也使用相同的目标币—日期随机种子。
- 两个时点：`same_day_conditional` 使用评价日其他币的 peg deviation，只分析当期条件信息；`all_predictors_lagged` 将币际变量也滞后一日，才是严格的次日预测比较。链上变量和宏观变量在两种时点下均滞后一日。
- 链上三源完整的日历从 2022-01-01 开始；90 日训练窗口内还需要前一日链上信息，因此共同评价日为 2022-04-02 至 2026-05-31，共 1,521 日、每种时点 16,731 个目标币方程。缺失链上历史没有填零或回填。

## 评价和统计推断

主表配对报告平均选中 GACV 和样本外标准化分位数 check loss。正的 `improvement` 表示扩展模型更好。样本外同时列出相对于第 5.4 节截距模型的 quantile skill score，以免“比旧模型略好”被误读为“有预测价值”。每日先平均十一目标币，再以日期为分析单位，采用 Bartlett-HAC(90) 和 1,999 次、90 日圆形区块 bootstrap；四个配对比较共属一个 Holm 校正家族。新增变量的入选频率只是惩罚回归的描述性选择统计，不是其系数的选择后显著性。

## 核心结果

扩展模型的选中 GACV 在同期条件规格下降 4.5%，在完全滞后规格下降 7.5%；两项配对差异经 Holm 校正仍显著。相同评价日的样本外 check loss 分别变化为 **上升 1.1%** 和 **下降 2.2%**，两项区间均跨零，Holm 校正后 *p* 分别为 0.534 和 0.426。扩展模型相对滚动截距基准的 quantile skill score 仍为 −25.5% 和 −34.4%。至少一项链上变量进入的方程比例分别为 90.6% 和 93.2%，说明“经常入选”与“能改善留出样本表现”是两个不同问题。

仅作描述的子组结果：三个直接出现在这些池中的币（DAI、USDC、USDT）的损失改善幅度高于其余八币，但组间差异经两个时点的 Holm 校正后不显著（同期条件 *p* = 0.080，全部滞后 *p* = 0.411）。不能因此声称池内币有稳定独享的预测收益。

## 输入与复现

`source_data/input/` 存放冻结的 peg 面板、宏观序列、第 5.4 节原模型预测、未改动的 `FRM_Statistics_Algorithm.R`、第 4 章 DPI 以及清洗后的链上 CSV；本包因此可以独立读取输入。`reference_code/section5_4/` 保留基准估计脚本，`reference_code/onchain_cleaning/` 保留链上清洗脚本、来源说明和数据质量核查；原始 Parquet/Release ZIP 因体积未重复打包，下载来源见清洗 README。上游为 [MSCA-DN-Digital-Finance/stablecoin-onchain-data](https://github.com/MSCA-DN-Digital-Finance/stablecoin-onchain-data)。`qa/input_manifest_md5.csv` 核对本地副本与原文件。

在工作区根目录运行：

```bash
Rscript outputs/section_5_6_microstructure_extension/code/01_estimate_micro_extension.R
Rscript outputs/section_5_6_microstructure_extension/code/02_analyse_micro_extension.R
```

估计按 14 日批次保存检查点，可中断续算。`FRM_MICRO_CORES=4` 控制并行度；`FRM_MICRO_MAX_DATES=2` 可做短测试。主要结果在 `tables/Table_5_6_Matched_Model_Comparison.csv`，论文精简表在 `tables/Table_5_6_Manuscript_Ready.md`，可查看的工作簿为 `tables/Chapter_5_Microstructure_Comparison.xlsx`；变量入选频率和辅助 lambda 指标分别在同目录的补充表。每日和每目标币原始对照以及 R session、覆盖核查保存在 `source_data/` 与 `qa/`。`Section_5_6_Manuscript_Ready_EN.md` 可作为章节正文初稿。

## 解释边界

这三项变量是场所层面的压力代理，主要观察 DAI、USDC 和 USDT 所在池，但被作为共同状态变量加入十一目标币方程。由此得到的增量效果不能解释为某个币的专属交易深度，更不能证明因果传导。扩展后的 lambda 汇总仅作方法敏感性，不能与全样本主 DPI 的绝对水平直接等同。无论结果改善与否，都应同时报告条件拟合与真正滞后预测的差异。
