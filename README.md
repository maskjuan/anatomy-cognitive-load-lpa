# 医学生解剖学认知负荷状况及教学配置启示：基于潜在剖面分析 —— 数据与分析代码

本仓库收录论文《医学生解剖学认知负荷状况及教学配置启示：基于潜在剖面分析》的去标识化原始数据、分析与绘图代码。

## 文件

- `N241_NASA_TLX_Raw_Responses_Deidentified.xlsx`：241 名医学生的原始评分，三种学习模态各 6 项、共 18 项（原始列名为英文维度名，脚本内会重命名为 ps_/td_/xr_ 前缀）
- `jiepou.R`：全流程分析脚本（数据清洗 -> 潜在剖面分析 LPA -> 自举似然比检验 BLRT -> 共表达网络 -> 出图）
- `四图绘图代码.R`：按投稿排版重绘正文四张图（画布宽度 16.0 / 11.0 / 14.5 / 13.5 cm，400 dpi，Times New Roman + 宋体）

## 数据来源

原始数据为公开的去标识化数据集：Zenodo, DOI: 10.5281/zenodo.23892312。本仓库附同一份数据文件，便于直接复现；正式引用请引用原始数据集。

`class_means.xlsx`、`LPA_fit_indices.xlsx`、`BLRT_p_values.csv`、`spearman_cor_matrix.csv`、`network_edges_r04.csv` 可由 `jiepou.R` 生成，放在脚本同目录即可运行全套流程。

## 运行环境

R 4.4；依赖 tidyLPA、mclust、psych、igraph、readxl、dplyr、tidyr、ggplot2、patchwork、scales、ragg、writexl。

`jiepou.R` 第 3.1b 节用 `mclust::mclustBootstrapLRT()`（EEI、100 次参数化自举、种子 20261102）复现正文 2.1 的 BLRT，P = 0.0099（即四舍五入的 0.010，为 100 次自举下的可分辨下限）；`tidySEM::BLRT()` 只接受 Mplus 类对象，无法用于本模型的 mclust 后端。

`四图绘图代码.R` 开头会关闭 showtext 的自动缩放，避免 400 dpi 画布上字号被压小。

## 说明

脚本中的类别命名、配色与图形版式与论文一致；四类剖面为标本主导高负荷型（20.7%）、二维图谱主导高负荷型（25.7%）、全域低负荷型（17.4%）与全模态中高负荷型（36.1%）。
