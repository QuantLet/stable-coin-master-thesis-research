# 第6.4节：严格样本外组合表现

本代码包基于第6.3节冻结的日度组合收益与权重，完成第6.4节的结果汇总、配对移动区块bootstrap、Holm多重检验校正及论文图表输出。

主要文件：

- `Section_6_4_Manuscript_Ready_EN.md`：可直接放入论文的英文正文。
- `tables/Table_6_4_Manuscript_Ready.md`：正文表格。
- `figures/Figure_6_4_Out_of_Sample_Portfolio_Risk.*`：SVG、PDF、TIFF和PNG格式的正文图。
- `figures/Figure_6_4_source_data.csv`：图形源数据。
- `code/01_build_oos_performance.R`：可复现R代码。
- `STATISTICAL_REPORTING_AUDIT.md`：统计报告审查。
- `MANUSCRIPT_CONTEXT_AUDIT_ZH.md`：与第五章及第6.3节的一致性检查。
- `qa/`：输入文件MD5、分析QA、图形静态预检和R环境信息。

从论文项目根目录运行：

```bash
Rscript outputs/section_6_4_oos_portfolio_performance/code/01_build_oos_performance.R
```

