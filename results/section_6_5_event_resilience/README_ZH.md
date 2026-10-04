# 第6.5节：重大尾部事件中的组合韧性

本代码包使用第6.3节冻结的无前视组合收益，以及第4.4节冻结的六个事件，完成0–30日压力窗口分析、事件分层移动区块bootstrap、Holm校正、区块长度敏感性检验和leave-one-event-out检验。

主要文件：

- `Section_6_5_Manuscript_Ready_EN.md`：可直接放入论文的6.5英文正文。
- `tables/Table_6_5_Manuscript_Ready.md`：正文表格。
- `tables/Table_S6_5_Event_Level_Performance.csv`：逐事件完整结果。
- `tables/Table_S6_5_Block_Length_Sensitivity.csv`：3日和14日区块敏感性。
- `tables/Table_S6_5_Leave_One_Event_Out.csv`：逐一剔除事件的检验。
- `figures/Figure_6_5_Portfolio_Drawdowns_Tail_Events.*`：SVG、PDF、TIFF和PNG正文图。
- `figures/Figure_6_5_source_data.csv`：主图源数据。
- `code/01_run_event_resilience.R`：完整R代码。
- `qa/`：窗口覆盖、输入MD5、分析QA、图形静态预检和R环境信息。

从论文项目根目录运行：

```bash
Rscript outputs/section_6_5_event_resilience/code/01_run_event_resilience.R
```

