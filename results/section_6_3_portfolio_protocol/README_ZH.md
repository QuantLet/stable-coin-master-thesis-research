# 第6.3节：组合设计与评价协议

本代码包使用论文现有十一币参考资产调整价格和primary DPI的coin-level lambda，构建EW、GMV、LTEC和QTEC四种组合。权重采用90日历史窗口、周度再平衡、long-only和单币30%上限；第t日权重只使用截至t-1日的信息。

## 复现

在项目根目录运行：

```bash
Rscript outputs/section_6_3_portfolio_protocol/code/01_run_portfolio_protocol.R
```

代码只依赖R基础包。`source_data/input/`保存冻结输入，运行时不需要网络或桌面目录。

## 主要输出

- `Section_6_3_Manuscript_Ready_EN.md`：可直接放入正文的6.3英文文本。
- `tables/Table_6_3_Portfolio_Design.csv`：四种策略及共同约束。
- `tables/Table_S6_3_OOS_Performance.csv`：样本外绩效结果。
- `tables/Table_S6_3_Block_Bootstrap.csv`：相对EW的90日移动区块bootstrap及Holm校正。
- `source_data/Portfolio_Daily_Returns.csv`：日度组合收益与换手率。
- `source_data/Portfolio_Weights_Long.csv`：日度组合权重。
- `Results_Preview_for_Section_6_4_EN.md`：供下一节使用的结果段落，不建议重复放入6.3。
- `MANUSCRIPT_CONTEXT_AUDIT_ZH.md`：与Chapter 3.4的口径衔接修改。

