# 第6.2节：集中度与系统性脱锚压力

本目录包含第6.2节的可复现实证、论文正文、表格、图形及统计QA。分析将第6.1节的日度NHHI和Top-2份额与Chapter 4的primary DPI对齐，并沿用HAC(90)处理重叠滚动窗口。

## 复现

在项目根目录运行：

```bash
Rscript outputs/section_6_2_concentration_dpi/code/01_estimate_concentration_dpi.R
```

`source_data/input/`保存了本节使用的冻结输入，因此代码包不依赖桌面目录或网络。若冻结输入不存在，脚本才会回退到前文章节的标准输出路径。

## 主要输出

- `Section_6_2_Manuscript_Ready_EN.md`：可直接插入论文的英文正文及图表说明。
- `tables/Table_6_2_Manuscript_Ready.md`：精简主表。
- `tables/Table_S6_2_All_Specifications.csv`：全部模型、效应量、HAC区间和p值。
- `tables/Table_S6_2_Block_Bootstrap.csv`：90日移动区块bootstrap结果。
- `figures/Figure_6_2_Concentration_and_Depegging_Pressure.*`：SVG、PDF、600 dpi TIFF和PNG。
- `qa/analysis_QA.csv`：数据范围、样本和统计输出检查。

主结果使用完整DPI样本。Crypto FRM因数据止于2026-04-07，仅作为匹配样本稳健性控制；新增链上变量沿用Chapter 5.6的1521日共同样本，不改变primary DPI的定义。
