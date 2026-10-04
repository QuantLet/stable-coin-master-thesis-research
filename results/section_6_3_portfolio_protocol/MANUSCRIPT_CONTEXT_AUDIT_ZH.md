# 与现有正文的衔接检查

当前Chapter 3.4把美元报价收益笼统地写成“retained for covariance and portfolio analysis”。6.3主回测改用参考资产调整收益后，这句话需要替换，否则会与非美元稳定币处理冲突。

建议替换为：

> Daily returns are retained for covariance and portfolio analysis. In the primary portfolio exercise, returns for EURS, IDRT and PAXG are calculated after expressing their prices relative to EURUSD, IDRUSD and XAUUSD, respectively, while the eight U.S.-dollar-pegged tokens retain their dollar-price returns. Raw U.S.-dollar returns for the full eleven-coin sample are reported only as a dollar-investor sensitivity analysis. These return measures do not enter the primary DPI.

其余衔接关系没有冲突：6.2已经将集中度解释为结构性暴露，6.3进一步检验不同配置规则的实际风险结果；第五章关于lambda样本外预测能力有限的结论，也与LTEC和QTEC未能改善主要风险终点相一致。

