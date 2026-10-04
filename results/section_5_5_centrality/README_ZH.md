# 5.5 实证分析与复现

## 写作逻辑

5.2 确认同向下尾联系在系统层面存在，但未识别固定的双边传导方向；5.4 表明同期币际信息对条件拟合的贡献较大，却不提高次日预测。5.5 因而只研究**当前条件网络中的位置**：哪些币的正向联系更多／更强，位置是否跨时期与规格稳定。它不重新构造 DPI，也不加入新变量。

## 主分析

- 样本：与 4.4、5.2 和 5.4 一致的 11 个稳定币、2020-04-01 至 2026-05-31 的 2,252 个滚动窗口，每窗口 110 个有向币对，共 247,720 条币对系数记录。
- 网络：`A[source,target,t] = max(beta[target,source,t], 0)`，阈值为 `1e-10`；负系数不被误作同向下尾联系。系数来自冻结的窗口 median/MAD 缩放、严格最小有限 GACV 规格。
- 向外／向内强度：每个币的正向系数行和／列和，均除以十个可能对手币。向内特征向量中心性来自 `t(A)` 的主特征向量，按每日节点总和为 1 归一化。向外调和接近中心性用边长 `1/A` 的最短路径计算，对不可达节点赋零贡献。
- 推断：日期是独立分析单位。均值区间由 1,999 次、90 日圆形移动区块 bootstrap 计算。11 个“向外减向内”节点对比使用 Bartlett-HAC(90) 并在一个 11 项家族内作 Holm 校正。这些不是惩罚回归的选择后系数区间。

## 数值结果

- USDC 向外强度最高：0.138，95% 区块 bootstrap 区间 0.119–0.160。
- BUSD 向内强度最高：0.150，区间 0.118–0.187；其向内特征向量中心性也最高，为 0.127。
- TUSD 与 USDC 的向外调和接近中心性居前，分别为 0.214 和 0.213。
- Holm 校正后，USDC 向外减向内强度为 0.035（p = 0.047），BUSD 为 −0.072（p = 0.025），GUSD 为 −0.027（p = 0.0058）；其余八个节点不显著。
- 前后半样本的向外／向内强度排名 Spearman 相关分别为 0.30／0.13；主规格与未缩放规格分别为 0.66／−0.38。排名不稳定，不能命名固定的“风险输出者／接收者”。
- 排除 539 个有历史填补暴露的窗口后，向外／向内排名相关仍为 0.964／0.945。主结论不由填补窗口单独驱动。

## 文件与复现

在此目录运行：

```bash
Rscript code/section_5_5_centrality.R
```

新增的链上交叉核对独立运行，不会重估上述网络：

```bash
Rscript code/section_5_5_onchain_crosscheck.R
```

它读取本包的每日 USDC 中心性和 `../onchain_microdata_20260919/stablecoin_onchain_daily_20200101_20260531.csv`。按 4.4 已选定的 SVB 日期，比较 2023-02-08 至 2023-03-09 的 30 日基线与 2023-03-11 至 2023-03-12 的两日压力期；3 月 10 日已出现市场压力，故不计入基线。产出 `tables/Table_S5_5_SVB_Onchain_Crosscheck.csv` 和 `source_data/SVB_Onchain_Crosscheck_Daily.csv`。压力期 USDC 向内强度中位数为 0.205，基线为 0.107；Curve 3pool 失衡度分别为 0.302 和 0.060，日交易次数分别为 11,510 和 269。交易池只覆盖三个币和选定场所，此处是同期描述性核对，不作显著性或因果推断，也不把新变量补入冻结的 Quantile-Lasso 规格。

代码只依赖 base R，以及绘图用的 `ragg`、`svglite`。冻结输入已复制到本包的 `source_data/input/`，可以独立复现，不会重估或改写第四章。主文使用 `Section_5_5_Manuscript_Ready_EN.md` 与 `figures/Figure_5_5_Network_Roles.pdf`；`tables/Table_5_5_Network_Roles.csv` 提供完整强度与区间，`tables/Table_S5_5_All_Node_Centralities.csv` 提供所有四项指标，`tables/Table_S5_5_Specification_Rank_Sensitivity.csv` 和 `Table_S5_5_Node_Sensitivity.csv` 是规格敏感性，`tables/Table_S5_5_Out_Minus_In_HAC90.csv` 是多重校正推断。每日节点数据、图的 Source Data、输入 MD5 和 R session 信息均已保存。

## 与 PPT 和论文的关系

PPT 给出 eigenvector 与 closeness 的概念，旧公开仓库用绝对系数建图。这里保留这两类中心性，但主网络按 5.2 的同向下尾解释只用正系数；绝对值网络被列为敏感性检验。把正负系数混为一条“传染”边会与 5.2 冲突。所有结论只描述同期条件关系，不能延伸为因果传导或次日预测。
