# Section 5.4 代码与结果包

## 一键运行

在本文件夹中执行：

```bash
FRM_CORES=4 Rscript code/run_all.R
```

脚本依次重建参考资产调整后的稳定币偏离和滞后宏观变量、滚动估计四组模型、生成统计表，并用R导出Figure 5.4。估计使用分批检查点，若中途停止，重新运行会继续未完成的批次。

估计与统计检验使用R基础包；绘图需要ggplot2、svglite和ragg。代码包已包含固定输入数据、原始FRM算法和本次完整预测结果。若只需重建表和图，可依次运行02_analyse_block_models.R和03_plot_figure_5_4.R，无需重新估计。

## 主结论

- 条件GACV结果中，macro-only、coin-only和joint模型相对基准分别降低损失8.3%、33.7%和37.0%。
- Shapley分解将联合模型改进的84.3%分配给币际信息，15.7%分配给宏观信息。
- 持出样本检验中，三种模型的skill均为负；joint模型在同日币际信息和全部滞后设定下的skill分别为−19.8%和−38.6%。
- 同日币际信息相对全滞后版本降低joint模型损失13.5%，说明币际关系主要反映同步尾部状态，没有形成稳定的次日预测优势。

## 文件

- `Section_5_4_Manuscript_Ready_EN.md`：可直接放入论文的英文正文、表题和图注。
- `Section_5_4_Writing_Logic_ZH.md`：本节写作顺序及图表位置。
- `Section_5_4_Statistical_Analysis_EN.md`：统计方法。
- `code/`：R估计、统计分析、作图和一键运行脚本。
- `tables/`：正文与附录结果表。
- `figures/`：SVG、PDF、600-dpi TIFF和PNG主图。
- `source_data/`：输入、逐日预测、作图数据和中间结果。
- `qa/`：冻结模型对齐、估计、分析和图形检查。
- `reference_code/`：原FRM算法及GitHub代码的只读对照副本。
