# 6.1 写作逻辑与实证设计

## 本节只回答一个问题

十一币样本的市值是否集中在少数稳定币，以及这种集中结构如何随时间变化。本节不检验集中度是否导致DPI上升，也不讨论投资组合；两者分别留给6.2和6.3以后。

## 段落逻辑

1. **问题与测量。** 市场总规模不能反映规模分布，因此计算每日HHI、按当日有效币种数调整的NHHI，以及Top-2 market share。
2. **主要事实。** NHHI先下降后回升，并非PPT旧图可能暗示的单调变化。
3. **关键比较。** 样本末NHHI仍低于2020，但USDT和USDC合计份额已接近全部样本市值。两个指标分化，说明市场由USDT单一主导转向USDT-USDC双头集中。
4. **边界和衔接。** 该结果是样本市场结构的描述，不能直接推出集中度增加系统性风险；6.2再检验NHHI及Top-2 share与主DPI的动态关系。

## 指标定义

设CoinGecko报告的美元市值为 \(MC_{i,t}\)，当日样本市值份额为

\[
s_{i,t}=\frac{MC_{i,t}}{\sum_j MC_{j,t}}.
\]

本文采用0至1尺度的HHI：

\[
HHI_t=\sum_i s_{i,t}^2.
\]

乘以10,000即可得到传统点数尺度。由于币种覆盖数随发行、退出和数据可得性变化，主文使用

\[
NHHI_t=\frac{HHI_t-1/N_t}{1-1/N_t},
\]

其中 \(N_t\) 是当日有正市值的币种数。Top-2 share作为补充指标，避免HHI下降被错误解释为头部依赖下降。

## 相对PPT和旧代码的修正

- PPT的方向保留：仍然以市值份额和HHI描述集中度。
- 旧脚本把缺失市值设为0；新版不填零、不插值、不向前填充。
- 旧图标题使用“normalized HHI”，但代码计算的是原始HHI；新版同时保存HHI和按当日 \(N_t\) 计算的NHHI。
- 旧脚本还构造累计市值HHI。累计到当日的市值没有清晰的市场结构含义，因此不进入论文。
- 市值不是token supply。本节统一使用“market-capitalization concentration”，不写“concentration of stablecoin supply”。
- PAXG、EURS和IDRT的市值已经以美元表示，因此用于衡量经济规模时不再进行参考资产调整。参考资产调整仍用于前文的价格偏离和收益风险。

## 实证输出

- 主图：Figure 6.1，NHHI与Top-2 share的31日滚动中位数，共用0至1纵轴，无双轴、无事件标注。
- 主文不需要单独放年度大表。年度中位数、币种份额和数据覆盖表放入附录。
- 日度原始指标用于数字计算；滚动中位数只用于降低图形噪声。
- 这一节属于描述性市场结构测量，不进行显著性检验。市值是观察到的日度规模变量，对其均值添加p值不会增加识别能力。

## 锁定术语

| Canonical term | 使用方式 | 不使用的表述 |
|---|---|---|
| market-capitalization concentration | 本节研究对象 | supply concentration |
| Herfindahl-Hirschman Index (HHI) | 首次写全称，之后HHI | HHI market risk |
| normalized HHI (NHHI) | 按每日有效币种数调整 | normalized market risk |
| Top-2 market share | 两个最大币的合计份额 | market dominance index |
| eleven-coin thesis sample | 明确样本边界 | global stablecoin market |

