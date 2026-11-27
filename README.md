# 解剖学认知负荷的潜在剖面分型与教学配置 —— 分析代码

本仓库收录论文《解剖学认知负荷的潜在剖面分型与教学配置》的分析与绘图代码，只含脚本，不含原始数据。

## 文件

- `jiepou.R`：全流程分析脚本（数据清洗 -> 潜在剖面分析 LPA -> 共表达网络 -> 出图）
- `四图绘图代码.R`：按投稿排版重绘正文四张图（画布宽度 16.0 / 11.0 / 14.5 / 13.5 cm，400 dpi，Times New Roman + 宋体）

## 数据来源

原始去标识化数据为公开数据集：Zenodo, DOI: 10.5281/zenodo.23892312（241 名医学生，三种学习模态各 6 项、共 18 项 NASA-TLX 改编评分）。

下载后把 `N241_NASA_TLX_Raw_Responses_Deidentified.xlsx`、`class_means.xlsx`、`LPA_fit_indices.xlsx`、`spearman_cor_matrix.csv`、`network_edges_r04.csv` 放在脚本同目录即可运行。

## 运行环境

R 4.4；依赖 tidyLPA、psych、igraph、readxl、dplyr、tidyr、ggplot2、patchwork、scales、ragg、writexl。

`四图绘图代码.R` 开头会关闭 showtext 的自动缩放，避免 400 dpi 画布上字号被压小。

## 说明

脚本中的类别命名、配色与图形版式与论文一致；四类剖面为标本主导高负荷型（20.7%）、二维图谱主导高负荷型（25.7%）、全域低负荷型（17.4%）与全模态中高负荷型（36.1%）。
