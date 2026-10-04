# Section 5.2 与论文上下文一致性审计

## 已保持一致

- **研究对象：** 延续 Chapter 4 的 reference-adjusted signed peg deviation，而非重新切回公开代码中的原始对数收益率。
- **主规格：** 11 个稳定币、5 个滞后一天的宏观金融变量、90 日滚动窗口、\(\tau=0.05\)、窗口内 median/MAD scaling 和 strict minimum finite GACV 均未改变。
- **时间边界：** 使用 2,252 个输出日，截止 2026-05-31；不把 PPT 中概括性的 “June 2026” 写成实证终点。
- **章节分工：** 5.1 讨论 active-set 构成；5.2 讨论币际联系；5.3 讨论宏观联系；5.4 再比较内部与外部来源。因此 5.2 不重复 5.1，也不提前给出中心性或投资组合结论。
- **Chapter 4 的解释边界：** 继续将 DPI 和系数解释为条件性、非因果且对缩放和样本构成敏感。新增结果反而量化了这种敏感性，没有与 4.6 冲突。
- **与 5.1 的状态结果：** 5.1 发现 severe 状态下稳定币总体 inclusion 降低；5.2 发现 aligned density 的 severe–very-low 差异很小且区间跨零。两者并不矛盾，因为总体收缩主要来自 inverse links 的减少，而不是 aligned links 的同步减少。
- **5.2 的正面主结论：** downside-aligned selection 在系统层面显著多于 inverse selection。该结果说明网络的符号构成偏向共同下尾运动，但不要求任何单一币对具有稳定的方向差异，因此与 55 项币对检验均不显著并不矛盾。

## 需要在正文中统一的两处措辞

1. 建议将 Chapter 5 标题从 **Sources and Transmission Channels of Systemic Depegging Pressure** 改为 **Sources and Conditional Linkages of Systemic Depegging Pressure**。现有设计没有时间先后或外生识别，`transmission channels` 容易被理解为因果机制。
2. 将 5.2 标题统一为 **Stablecoin-to-Stablecoin Tail-Risk Links**，不要混用 `Stable Coin`、`Stablecoin` 和缺少连字符的写法。
3. 4.6 末句中的 `stable coins` 建议同步改为 `stablecoins`，以保持术语一致。

## 不能沿用的旧式结论

- 不能把图中高频的 `source → target` 写成统计上稳定的风险输出路径：55 个方向差异经 HAC(90) 与 Holm 校正后均不显著。
- 不能写成 DPI 越高，币际同向网络越密：severe 与 very-low 状态差异的区块 bootstrap 区间跨越零。
- 不能把完整样本的前十名当作永久网络排序：robust-scaled 与 unscaled 的同向排名相关系数为 0.286，前后半样本为 0.169。
- 可以保留“相同参考资产和同类 peg-support design 的同向选择更频繁”，但必须写成条件相关，不能据此宣称赎回、抵押或交易渠道已被识别。

可以写成的主结论是：**币际网络在系统层面显著偏向 downside-aligned links，并呈现参考资产和设计类别结构，但具体双边方向并不稳定。**

## 对后续章节的约束

- 5.5 若计算 out-strength、in-strength 或 eigenvector centrality，必须从 `predictor → target` 构图。旧公开脚本直接传入 `A[target, predictor]` 会把箭头反向。
- 中心性必须按滚动窗口或子样本报告稳定性，不能只给全样本单一排名。
- 不必在 5.2 临时加入 \(\tau=0.10,0.25,0.50\)。这些分位数改变了本节的下尾研究对象，应和 63/90/180 日窗口、删币等一起放入 5.6 稳健性检验。
