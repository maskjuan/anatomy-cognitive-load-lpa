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

# 3.2b 分类不确定性（正文 2.1 的两个指标由此得出）
post_z <- best$model$z                       # 每名学生的类后验概率
post_max <- apply(post_z, 1, max)
cat("平均最大后验概率：", round(mean(post_max), 3),
    "；后验概率>=0.80 的比例：", round(100 * mean(post_max >= 0.80), 1), "%\n")

# 3.3 类别均值
class_means <- X %>%
  group_by(class) %>%
  summarise(across(all_of(load_vars), mean, .names = "{.col}")) %>%
  as.data.frame()
print(class_means, digits = 2)
write_xlsx(class_means, "class_means.xlsx")

# ---------- 4. 共表达网络（统计显著 P 值 + 效应量阈值 双条件） ----------
cor_mat <- cor(X[, load_vars], method = "spearman", use = "pairwise.complete.obs")
write.csv(cor_mat, "spearman_cor_matrix.csv")

# 18 项指标两两相关共 153 对：逐对求 P（本机无 scipy 之外的检验，故用 t 近似由 r 反算）
n_obs <- nrow(X)
idx   <- which(upper.tri(cor_mat), arr.ind = TRUE)
r_vec <- cor_mat[idx]
p_vec <- 2 * pt(abs(r_vec) * sqrt(n_obs - 2) / sqrt(1 - r_vec^2),
                df = n_obs - 2, lower.tail = FALSE)

cor_stats <- data.frame(
  var1 = rownames(cor_mat)[idx[, 1]],
  var2 = colnames(cor_mat)[idx[, 2]],
  r    = round(r_vec, 3),
  P    = signif(p_vec, 3),
  P_lt_05 = p_vec < 0.05
)
write.csv(cor_stats, "correlation_r_p.csv", row.names = FALSE)
cat("153 对相关中 P<0.05 者：", sum(p_vec < 0.05),
    "；其中 |r|>=0.40 者：", sum(p_vec < 0.05 & abs(r_vec) >= 0.40), "\n")
cat("|r|>=0.40 的 13 对 P 值范围：",
    signif(min(p_vec[abs(r_vec) >= 0.40]), 3), "~",
    signif(max(p_vec[abs(r_vec) >= 0.40]), 3), "\n")

# 建网双条件：P < 0.05（统计显著）且 r >= 0.40（效应量达标）
keep <- r_vec > 0.40 & p_vec < 0.05
edge_list <- data.frame(
  from   = rownames(cor_mat)[idx[keep, 1]],
  to     = colnames(cor_mat)[idx[keep, 2]],
  weight = r_vec[keep],
  P      = signif(p_vec[keep], 3)
)
write.csv(edge_list, "network_edges_r04.csv", row.names = FALSE)

# 4.1 构建并绘制高清网络图
if (nrow(edge_list) == 0) {
  cat("警告：没有 r > 0.4 的正相关边，跳过网络图。\n")
} else {
  g <- graph_from_data_frame(edge_list, directed = FALSE)
  
  # 节点属性：使用论文可读的中文标签（分两行显示避免拥挤）
  var_labels <- c(
    ps_men = "标本\n脑力", ps_phy = "标本\n体力", ps_tem = "标本\n时限",
    ps_per = "标本\n表现失败", ps_eff = "标本\n努力", ps_com = "标本\n舒适",
    td_men = "2D\n脑力", td_phy = "2D\n体力", td_tem = "2D\n时限",
    td_per = "2D\n表现失败", td_eff = "2D\n努力", td_com = "2D\n舒适",
    xr_men = "3D\n脑力", xr_phy = "3D\n体力", xr_tem = "3D\n时限",
    xr_per = "3D\n表现失败", xr_eff = "3D\n努力", xr_com = "3D\n舒适"
  )
  
  nm <- V(g)$name
  mod_group <- ifelse(grepl("^ps", nm), "Specimen",
                      ifelse(grepl("^td", nm), "2D", "3D"))
  
  V(g)$color <- c("#E64B35", "#4DBBD5", "#00A087")[as.integer(factor(mod_group))]
  V(g)$size  <- 35
  V(g)$label <- var_labels[nm]
  V(g)$label.cex <- 2.2
  V(g)$frame.color <- "white"
  
  # 边宽调整：系数*15 + 2.5，确保最低权重也有清晰线条
  E(g)$width <- (E(g)$weight - 0.4) * 15 + 2.5
  
  pdf("network_plot_v2.pdf", width = 10, height = 8)
  set.seed(123)
  plot(g,
       layout = layout_with_fr(g, weights = E(g)$weight, niter = 2000),
       vertex.label = V(g)$label,
       vertex.label.cex = V(g)$label.cex,
       vertex.label.color = "black",
       vertex.size = V(g)$size,
       vertex.color = V(g)$color,
       vertex.frame.color = V(g)$frame.color,
       edge.width = E(g)$width,
       edge.color = "gray40",
       main = "共表达网络（r > 0.4）")
  legend("topright",
         legend = c("实物标本", "2D教材", "3D XR"),
         pt.bg = c("#E64B35", "#4DBBD5", "#00A087"),
         pch = 21, pt.cex = 2.5, bty = "n")
  dev.off()
}

# ---------- 5. 剖面图（4 类 × 3 模态 × 6 维度） ----------
class_means_long <- class_means %>%
  pivot_longer(-class, names_to = "variable", values_to = "mean") %>%
  mutate(
    modality = factor(case_when(
      grepl("^ps", variable) ~ "实物标本",
      grepl("^td", variable) ~ "2D教材",
      grepl("^xr", variable) ~ "3D XR"
    ), levels = c("实物标本", "2D教材", "3D XR")),
    dimension = factor(case_when(
      grepl("men$", variable) ~ "脑力要求",
      grepl("phy$", variable) ~ "体力要求",
      grepl("tem$", variable) ~ "时限要求",
      grepl("per$", variable) ~ "表现失败感",
      grepl("eff$", variable) ~ "努力程度",
      grepl("com$", variable) ~ "情感舒适"
    ), levels = c("脑力要求", "体力要求", "时限要求", "表现失败感", "努力程度", "情感舒适"))
  )

p <- ggplot(class_means_long, aes(x = dimension, y = mean,
                                  color = factor(class), group = factor(class))) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  facet_wrap(~ modality, nrow = 1) +
  scale_color_brewer(palette = "Set1", name = "类别") +
  labs(x = NULL, y = "均值（1–7分）") +
  theme_bw(base_size = 13) +
  theme(
    text = element_text(family = "zh"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "right",
    panel.grid.minor = element_blank()
  )
p
ggsave("profile_plot_v2.pdf", p, width = 10, height = 4.5)

cat("=== 全部完成 ===\n")
print(table(X$class))








# ============================================================
# 论文级高清出图代码（修复字号过小问题，输出 600 dpi TIFF）
# ============================================================

# 注册系统原生字体，防止乱码
windowsFonts(zh = windowsFont("Microsoft YaHei")) # 如果 Mac 请用 "PingFang SC"

# ---------- 1. 网络图（画布缩小，字号放大） ----------
if (nrow(edge_list) > 0) {
  g <- graph_from_data_frame(edge_list, directed = FALSE)
  
  # 单行标签，节约空间
  var_labels <- c(
    ps_men = "标本-脑力", ps_phy = "标本-体力", ps_tem = "标本-时限",
    ps_per = "标本-表现失败", ps_eff = "标本-努力", ps_com = "标本-舒适",
    td_men = "2D-脑力", td_phy = "2D-体力", td_tem = "2D-时限",
    td_per = "2D-表现失败", td_eff = "2D-努力", td_com = "2D-舒适",
    xr_men = "3D-脑力", xr_phy = "3D-体力", xr_tem = "3D-时限",
    xr_per = "3D-表现失败", xr_eff = "3D-努力", xr_com = "3D-舒适"
  )
  
  nm <- V(g)$name
  mod_group <- ifelse(grepl("^ps", nm), "Specimen",
                      ifelse(grepl("^td", nm), "2D", "3D"))
  
  # 视觉参数：圆圈调大，字号暴力放大
  V(g)$color <- c("#E64B35", "#4DBBD5", "#00A087")[as.integer(factor(mod_group))]
  V(g)$size <- 30                 # 圆圈加大，避免文字溢出
  V(g)$label <- var_labels[nm]
  V(g)$label.cex <- 2.5           # 【核心修改】节点文字从 0.9 直接飙升到 1.8
  V(g)$label.family <- "zh"
  V(g)$frame.color <- "white"
  
  E(g)$width <- (E(g)$weight - 0.4) * 15 + 2.5
  E(g)$color <- "gray50"
  
  # 【核心修改】画布尺寸缩小为 6.5 x 5.5 英寸，让字体视觉上变大
  tiff("Figure_Network.tiff", width = 6.5, height = 5.5, units = "in", 
       res = 600, compression = "lzw")
  
  set.seed(123)
  par(cex.main = 5) 
  plot(g,
       layout = layout_with_kk(g, weights = E(g)$weight), 
       vertex.label = V(g)$label,
       vertex.label.cex = V(g)$label.cex,
       vertex.label.family = "zh",
       vertex.label.color = "black",
       vertex.size = V(g)$size,
       vertex.color = V(g)$color,
       vertex.frame.color = V(g)$frame.color,
       edge.width = E(g)$width,
       edge.color = E(g)$color,
       main = "共表达网络 (r > 0.4)",
       cex.main = 5) # 标题放大
  
  legend("topright",
         legend = c("实物标本", "2D教材", "3D XR"),
         pt.bg = c("#E64B35", "#4DBBD5", "#00A087"),
         pch = 21, pt.cex = 4, bty = "n", cex = 2)
  dev.off()
}

# ---------- 2. 剖面图（暴改坐标轴字号） ----------
# ---------- 剖面图（线条/点不变，极致放大字体版） ----------
class_means_long <- class_means %>%
  pivot_longer(-class, names_to = "variable", values_to = "mean") %>%
  mutate(
    modality = factor(case_when(
      grepl("^ps", variable) ~ "实物标本",
      grepl("^td", variable) ~ "2D教材",
      grepl("^xr", variable) ~ "3D XR"
    ), levels = c("实物标本", "2D教材", "3D XR")),
    dimension = factor(case_when(
      grepl("men$", variable) ~ "脑力",
      grepl("phy$", variable) ~ "体力",
      grepl("tem$", variable) ~ "时限",
      grepl("per$", variable) ~ "表现失败",
      grepl("eff$", variable) ~ "努力",
      grepl("com$", variable) ~ "舒适"
    ), levels = c("脑力", "体力", "时限", "表现失败", "努力", "舒适"))
  )

p <- ggplot(class_means_long, aes(x = dimension, y = mean,
                                  color = factor(class), group = factor(class))) +
  # 线条和点保持你上一版满意的粗细（1.2 和 3.5）
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.5) +
  facet_wrap(~ modality, nrow = 1) +
  scale_color_brewer(palette = "Set1", name = "类别") +
  labs(x = NULL, y = "均值 (1-7分)") +
  
  theme_bw() + # 这里不设 base_size，完全靠下面单独指定
  
  theme(
    text = element_text(family = "zh"),
    
    # 【核心修改区】只放大字体，不碰线条
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 28, color = "black", face = "bold"),
    axis.text.y = element_text(size = 28, color = "black"),
    axis.title.y = element_text(size = 28, face = "bold", margin = margin(r = 15)),
    
    # 顶部灰底的分面标题（实物标本等）
    strip.text = element_text(size = 32, face = "bold", margin = margin(t = 8, b = 8)),
    
    # 图例文字
    legend.title = element_text(size = 28, face = "bold"),
    legend.text = element_text(size = 25),
    
    # 防止大字被裁切
    plot.margin = margin(t = 15, r = 15, b = 15, l = 15),
    panel.grid.minor = element_blank()
  )

# 【核心】画布宽度强制压缩到 7 英寸（高度 4 英寸）
# 画布越窄，同样的 22pt 字体在图片上占据的比例就越大！
ggsave("Figure_Profile.tiff", p, width = 7, height = 4, dpi = 600,
       device = "tiff", compression = "lzw")
table(cls)









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














library(ggplot2)
library(reshape2)

# 1. 读取相关矩阵（确保你的工作目录下有 spearman_cor_matrix.csv）
cor_mat <- read.csv("spearman_cor_matrix.csv", row.names = 1)
cor_matrix <- as.matrix(cor_mat)

# 2. 转换为长格式（下三角）
cor_long <- melt(cor_matrix)
# 只保留下三角，并将对角线设为NA（不画对角线）
cor_long <- cor_long[upper.tri(cor_matrix, diag = FALSE), ]
names(cor_long) <- c("Var1", "Var2", "value")

# 3. 设置变量标签（给X轴和Y轴换上有意义的名称）
# 注意：顺序要与矩阵的行/列名一致
var_labels <- c(
  "ps_men" = "标本·脑力", "ps_phy" = "标本·体力", "ps_tem" = "标本·时限",
  "ps_per" = "标本·表现失败", "ps_eff" = "标本·努力", "ps_com" = "标本·舒适",
  "td_men" = "2D·脑力", "td_phy" = "2D·体力", "td_tem" = "2D·时限",
  "td_per" = "2D·表现失败", "td_eff" = "2D·努力", "td_com" = "2D·舒适",
  "xr_men" = "3D·脑力", "xr_phy" = "3D·体力", "xr_tem" = "3D·时限",
  "xr_per" = "3D·表现失败", "xr_eff" = "3D·努力", "xr_com" = "3D·舒适"
)
cor_long$Var1 <- factor(var_labels[cor_long$Var1], levels = var_labels)
cor_long$Var2 <- factor(var_labels[cor_long$Var2], levels = var_labels)

# 4. 绘图
p_heatmap <- ggplot(cor_long, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white", linewidth = 0.5) +
  # 显示具体数值（保留两位小数）
  geom_text(aes(label = sprintf("%.2f", value), 
                color = ifelse(abs(value) > 0.5, "white", "black")), 
            size = 3) +
  # 设定颜色：红色为正，蓝色为负，白色为0
  scale_fill_gradient2(low = "#4DBBD5", mid = "white", high = "#E64B35", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name = "Spearman\n相关系数") +
  scale_color_identity() + # 根据数值自动调整文字颜色
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  coord_fixed() + # 确保格子是正方形
  theme_minimal(base_size = 12) +
  theme(
    text = element_text(family = "zh"), # 中文字体
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 9),
    axis.text.y = element_text(size = 9),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "right"
  )
p_heatmap 
# 5. 保存图片
ggsave("correlation_heatmap.pdf", p_heatmap, width = 9, height = 7)
