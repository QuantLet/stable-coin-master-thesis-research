# Chapter 5.2 实证与代码包

本包基于 Chapter 4 主规格保存的 Quantile-Lasso 系数，分析 110 个稳定币有向币对在 2,252 个滚动窗口中的选择频率、符号、方向非对称性、DPI 状态变化和设计类别差异。统计推断以日期为单位，避免把同一天的币对或高度重叠的滚动窗口误当作独立观测。

## 主要结果

- 币间系数的总体 active density 为 63.5%；同向下尾联系为 39.6%，反向联系为 23.9%。
- 同向下尾联系比反向联系高 15.6 个百分点（HAC(90) 95% CI：11.6–19.7；与两项分类对比共同进行 Holm 校正后 \(p=9.04\times10^{-14}\)）。这是 5.2 的系统层面主结果。
- 62.3% 的 active coin-to-coin coefficients 为正，即表现为 downside-aligned conditional links。
- 最持续的同向联系为 USDT→TUSD（71.4%）、DAI→USDT（67.5%）和 USDP→TUSD（65.6%）。
- 55 个无向币对的平均绝对方向差为 5.9 个百分点，但在 HAC(90) 推断并对 55 项检验做 Holm 校正后，没有一个币对的方向差异显著。高频方向不能解释为稳定的风险输出层级。
- 相同参考资产币对的同向频率比不同参考资产高 9.1 个百分点（HAC(90) 95% CI：7.1–11.0；Holm-adjusted \(p=6.70\times10^{-20}\)）；同类 peg-support design 比跨类别高 4.4 个百分点（1.7–7.1；\(p=0.00144\)）。
- severe 与 very-low DPI 状态的同向密度相差 −1.2 个百分点（90 日区块 bootstrap 95% CI：−3.9–1.7），不足以支持“高 DPI 必然伴随更密集币际同向联系”。
- strict 与 screened GACV 的同向排名一致（Spearman \(\rho=1.000\)），排除 539 个含历史填补值的窗口后仍为 0.959；但 robust-scaled 与 unscaled 排名仅为 0.286，前后半样本仅为 0.169，说明具体币对排名具有规格和时期敏感性。

## 解释边界

网络方向表示“预测币进入目标币的条件 5% 分位数方程”，不表示时间先后或因果传染。稳定币解释变量是同期变量，系数经过 L1 惩罚，相关预测变量可以互相替代。区块 bootstrap 和 HAC 区间描述保存系数序列的不确定性，不是惩罚回归的选择后推断。DPI 状态与网络系数来自同一估计系统，因此状态比较只作描述。

## 复现

```bash
Rscript code/section_5_2_tail_risk_links.R
Rscript code/section_5_2_inference_and_robustness.R
```

以上命令应在解压后的 `section_5_2_tail_risk_links/` 目录内运行。第一个脚本生成描述性网络、主图和基础表；第二个脚本生成 HAC/区块 bootstrap 推断、组别检验和规格敏感性结果。两者均读取 `source_data/input/` 中冻结的估计输出。分析仅依赖 base R；重绘图形另需 `svglite` 与 `ragg`。图形提供 SVG、PDF、600-dpi TIFF 和 300-dpi PNG；所有检验均有 QA、输入 MD5 和 R session information。

## 文件入口

- `Section_5_2_Manuscript_Ready_EN.md`：可直接放入论文的英文正文。
- `Section_5_2_Statistical_Analysis_EN.md`：可放入方法或附录的统计说明。
- `Section_5_2_Conclusion_Replacement_EN.md`：可直接替换本节结尾的精简结论。
- `tables/Table_5_2_Manuscript_Ready.md`：主文表格及注释。
- `tables/Table_5_2_Systemwide_Downside_Alignment.csv`：系统层面主检验的完整数值。
- `figures/Figure_5_2_Directional_Tail_Risk_Links.*`：主文图。
- `MANUSCRIPT_CONTEXT_AUDIT_ZH.md`：与 Chapter 4、5.1 及后续章节的衔接检查。
- `CODE_PROVENANCE.md`：公开仓库与本地数值结果的职责边界。
