# ============================================================
# 解剖学认知负荷数据分析 R 代码（最终优化版）
# 功能：数据清洗 → LPA 潜在剖面分析 → 共表达网络 → 高清出图
# 关键修正：
#   1. 列名避免以数字开头（3d_ 改为 xr_）
#   2. LPA 最终选定 4 类模型（基于 AIC/BIC/SABIC/Entropy）
#   3. 网络图只保留正相关边，修复 FR 布局负权重报错
#   4. 中文字体嵌入，修复 PDF 乱码，优化图例与线条粗细
# ============================================================

# 0. 环境准备
rm(list = ls())
library(readxl); library(dplyr); library(tidyr)
library(tidyLPA); library(psych); library(igraph)
library(ggplot2); library(writexl)

# 设置中文字体（Windows 用户）
# 如果画图仍乱码，请将 "Microsoft YaHei" 换成 "SimHei"
windowsFonts(zh = windowsFont("Microsoft YaHei"))

# ---------- 1. 读取数据 ----------
raw <- read_excel("N241_NASA_TLX_Raw_Responses_Deidentified.xlsx")
cat("原始维度：", dim(raw), "\n")

# ---------- 2. 列名简化与数据转换 ----------
new_names <- c(
  "sex",
  "ps_men","ps_phy","ps_tem","ps_per","ps_eff","ps_com",   # 实物标本
  "td_men","td_phy","td_tem","td_per","td_eff","td_com",   # 2D教材
  "xr_men","xr_phy","xr_tem","xr_per","xr_eff","xr_com"    # 3D XR (原3d_)
)
names(raw) <- new_names
load_vars <- new_names[-1]
X <- as.data.frame(lapply(raw[, load_vars], as.numeric))
rownames(X) <- paste0("S", seq_len(nrow(X)))
cat("缺失值：", sum(is.na(X)), "\n")

# ---------- 3. 潜在剖面分析 (LPA) ----------
X_z <- scale(X)
lpa_fit <- X_z %>% as.data.frame() %>%
  estimate_profiles(1:4, variances = "equal", covariances = "zero")

# 3.1 拟合指标表导出
fit_all <- do.call(rbind, lapply(lpa_fit, function(x) x$fit))
print(fit_all)
write_xlsx(fit_all, "LPA_fit_indices.xlsx")

# 3.2 选定 4 类模型（依据：AIC/BIC/SABIC最低，Entropy最高）
best <- lpa_fit[[4]]
cls <- best$model$classification
X$class <- as.factor(cls)
raw$class <- as.factor(cls)
cat("4 类模型的样本分布：\n"); print(table(cls))

# 3.3 类别均值
class_means <- X %>%
  group_by(class) %>%
  summarise(across(all_of(load_vars), mean, .names = "{.col}")) %>%
  as.data.frame()
print(class_means, digits = 2)
write_xlsx(class_means, "class_means.xlsx")

# ---------- 4. 共表达网络 ----------
cor_mat <- cor(X[, load_vars], method = "spearman", use = "pairwise.complete.obs")
write.csv(cor_mat, "spearman_cor_matrix.csv")


edges <- which(cor_mat > 0.4 & upper.tri(cor_mat), arr.ind = TRUE)
edge_list <- data.frame(
  from   = rownames(cor_mat)[edges[,1]],
  to     = colnames(cor_mat)[edges[,2]],
  weight = cor_mat[edges]
)
write.csv(edge_list, "network_edges_r04.csv", row.names = FALSE)



# 如果 sex 是数值型（如1和2），可以直接算；如果是字符串，也没问题
sex_table <- table(X$class, raw$sex)
print(sex_table)  # 先看看四个类别里的男女分布

# 执行卡方检验
chisq_result <- chisq.test(sex_table)
print(chisq_result)
print(sex_table)



library(ggplot2)
library(dplyr)

# 1. 构建数据框（与之前一致）
sex_data <- data.frame(
  Class = factor(c(1, 1, 2, 2, 3, 3, 4, 4)),
  Sex = factor(c("Female", "Male", "Female", "Male", "Female", "Male", "Female", "Male"),
               levels = c("Female", "Male")), # 女性在下/上，男性相反
  Count = c(35, 15, 50, 12, 22, 20, 51, 36)
)

# 计算百分比与标签
sex_data <- sex_data %>%
  group_by(Class) %>%
  mutate(
    Total = sum(Count),
    Percent = Count / Total * 100,
    Label = paste0(Count, "\n(", round(Percent, 1), "%)") # 人数换行，百分比放括号里
  )

# 2. 设置X轴短标签（用换行符 \\n 防止重叠）
class_labels <- c(
  "1" = "类别1\n标本主导高负荷\n(n=50)",
  "2" = "类别2\n2D图谱主导高负荷\n(n=62)",
  "3" = "类别3\n全域低负荷\n(n=42)",
  "4" = "类别4\n全模态中高负荷\n(n=87)"
)

# 3. 绘图（纵向）
p_sex_vertical <- ggplot(sex_data, aes(x = Class, y = Percent, fill = Sex)) +
  geom_bar(stat = "identity", width = 0.6, color = "white", linewidth = 0.5) +
  geom_text(aes(label = Label), 
            position = position_stack(vjust = 0.5), 
            size = 5, color = "white", fontface = "bold") + # 文字设为白色加粗，放在色块里更清晰
  scale_fill_manual(values = c("Female" = "#E64B35", "Male" = "#4DBBD5"), 
                    labels = c("Female" = "女性", "Male" = "男性")) +
  scale_x_discrete(labels = class_labels) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100)) +
  labs(x = NULL, y = "构成比 (%)", fill = "性别") +
  theme_bw(base_size = 14) +
  theme(
    text = element_text(family = "zh"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "top",
    axis.text.x = element_text(size = 10, lineheight = 1.2, color = "black")
  )
p_sex_vertical
# 4. 保存图片（注意：纵向图高度需要比宽度大一些，比如 7x6）
ggsave("sex_stacked_bar_vertical.pdf", p_sex_vertical, width = 7, height = 6)


















 
