# Section 5.3 代码与结果包

## 一键复现

在本文件夹内运行：

```bash
Rscript code/section_5_3_macro_financial.R
```

脚本读取 `source_data/input/` 中冻结的Chapter 4主系数面板和敏感性面板，重新生成全部表格、主图、源数据和QA文件。脚本不重新估计DPI，也不会修改论文原文件。

## 主文件

- `Section_5_3_Manuscript_Ready_EN.md`：可直接放入论文的英文正文。
- `Section_5_3_Writing_Logic_ZH.md`：中文写作逻辑、图表位置和不能越界的结论。
- `Section_5_3_Statistical_Analysis_EN.md`：统计方法说明。
- `tables/Table_5_3_Manuscript_Ready.md`：主表的可粘贴版本。
- `figures/Figure_5_3_Macro_Financial_Selection.*`：PDF、SVG、TIFF与PNG格式主图。
- `figures/Figure_5_3_caption.txt`：主图图注。
- `source_data/Figure_5_3_Macro_Financial_Selection.csv`：图5.3全部55个单元格的源数据。
- `tables/Table_S5_12` 至 `Table_S5_17`：总体检验、两两比较、目标异质性、DPI状态、参考资产和稳健性结果。
- `reference_code/`：原FRM实现、当前主估计脚本和5.2推断脚本的只读对照副本。

## 主要结论

- CVIX进入频率最高，但标准化系数权重最低；高频进入不能等同于高贡献。
- S&P 500与VIX存在经Holm校正后仍可辨认的风险规避方向；CVIX、DXY和利率没有统一方向。
- 目标—因子排名对GACV筛选和插补窗口处理稳健，但对缩放方式和样本时期敏感。
- 本节支持“宏观条件与尾部风险相关”，不支持“宏观变量普遍占优”或固定因果传导。

## 注意

表5.3中的“权重”是标准化Lasso系数的绝对份额，不是预测损失下降量。宏观组和币际组的正式相对贡献应在5.4中通过macro-only、coin-only与joint模型进行比较。
