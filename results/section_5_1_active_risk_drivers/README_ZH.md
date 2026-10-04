# Chapter 5.1 实证与代码包

## 结论

- 5.1 需要实证，但不需要重新估计 DPI，也不需要展示全部系数。
- 24,772 个目标币—日期方程的平均 active-set size 为 9.53/15，中位数为 10（IQR 8–11）。
- 按候选变量数量标准化后，稳定币变量和宏观金融变量的纳入率分别为 63.5% 和 63.8%，不存在总体上的组别主导。
- 从 very-low 到 severe DPI 状态，平均 active-set size 从 10.11 降至 9.00；两组变量的纳入率均下降，但所选变量中稳定币的占比基本维持在 66.4%–66.9%。
- CVIX 和 BUSD 分别是最常被选择的宏观变量和稳定币变量，但月度选择率明显变化，因此不存在固定不变的驱动因素集合。

## 识别边界

active set 与 DPI 来自同一次 Quantile-Lasso 估计。因此，DPI 状态与 active-set size 的关系含有惩罚机制带来的机械成分，只能描述，不能解释为因果或独立的状态检验。选择频率也不等于经济影响大小；相关变量可能相互替代。

## 复现

在本文件所在目录的上一级工作区执行：

```bash
Rscript outputs/section_5_1_active_risk_drivers/code/section_5_1_active_sets.R
```

脚本读取 `source_data/input/` 中冻结的 Chapter 4 主规格输出，并重新生成主表、附录表、源数据、图形和 QA 文件。图形同时输出 SVG、PDF、600-dpi TIFF 和 300-dpi PNG。

## 文件说明

- `Section_5_1_Manuscript_Ready_EN.md`：可直接放入论文的英文正文、表题与图注。
- `Section_5_1_Writing_Logic_ZH.md`：中文写作逻辑与实证取舍。
- `tables/Table_5_1_Active_Set_Composition_by_DPI_State.csv`：正文主表。
- `tables/Table_5_1_Manuscript_Ready.md`：已经四舍五入并配有表注的正文表格。
- `figures/Figure_5_1_Active_Risk_Drivers.*`：正文主图。
- `tables/Table_S5_1_Predictor_Selection_Frequencies.csv`：逐变量附录表。
- `tables/Table_S5_2_Target_Active_Set_Size.csv`：逐目标币附录表。
- `tables/Table_S5_3_Active_Set_Composition_by_Year.csv`：年度附录表。
- `source_data/`：目标币—日期、日度和作图源数据。
- `qa/`：输入校验、MD5、运行环境、图形合同和图形预检结果。
