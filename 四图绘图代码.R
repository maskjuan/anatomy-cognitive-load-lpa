###########################################################
# 《解剖学认知负荷的潜在剖面分型与教学配置》四张图 —— R 代码
# 运行：把本文件与 class_means.xlsx、spearman_cor_matrix.csv、
#       network_edges_r04.csv 放在同一目录，然后
#       Rscript 四图绘图代码.R   或在 RStudio 里 source()
# 依赖：ggplot2、patchwork、readxl、scales、dplyr、tidyr、ragg
# 输出：同目录 figures_rebuilt_R\ 下的 4 个 PNG（400 dpi）
#
# 统一排版（与投稿版正文一致）：
#   西文 Times New Roman，中文宋体
#   面板/图题 8.5 pt、轴标题 8 pt、刻度 7 pt、图例 7.5 pt、数据标注 6.5 pt
#   线宽 1.1 pt（geom_line linewidth=0.39 mm ≈1.1 pt）、点 3~4 pt
#   画布宽度 = 正文插入宽度：图1 16.0 cm、图2 11.0 cm、
#   图3 14.5 cm、图4 13.5 cm —— 故图上 7 pt 刻度插入 Word 后仍是 7 pt
###########################################################

library(ggplot2)
library(patchwork)
library(readxl)
library(scales)
library(dplyr)
library(tidyr)

## 关键：不少中文生信环境在 .Rprofile 里执行了 showtext::showtext_auto()，
## 它会把 ggplot 的 pt 字号按 showtext 自己的 dpi 重新换算，400 dpi 画布上
## 7 pt 会被画成 2 pt 左右 —— 这正是“跑出来字特别小”的原因。这里显式关掉。
if (requireNamespace("showtext", quietly = TRUE)) {
  invisible(try(showtext::showtext_auto(enable = FALSE), silent = TRUE))
  invisible(try(showtext::showtext_opts(dpi = 400), silent = TRUE))
}

## 定位脚本所在目录（source() 与 Rscript 两种方式都要能用）
get_here <- function() {
  of <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (!is.null(of)) return(dirname(normalizePath(of)))
  ca <- commandArgs(trailingOnly = FALSE)
  m  <- grep("^--file=", ca, value = TRUE)
  if (length(m)) return(dirname(normalizePath(sub("^--file=", "", m[1]))))
  getwd()
}
here <- get_here()
setwd(here)
outdir <- file.path(here, "figures_rebuilt_R1")
dir.create(outdir, showWarnings = FALSE)

## ---------------------------------------------------------- 全局字号
PT   <- 1 / .pt          # pt -> mm，供 geom_text 使用
FS_TICK  <- 7
FS_LAB   <- 8
FS_LEG   <- 7.5
FS_ANN   <- 6.5
FS_TITLE <- 8.5
FONT_EN  <- "Times New Roman"
FONT_CN  <- "SimSun"                     # 宋体；若无此字体可改 "SimHei"

theme_pub <- function(base_size = FS_TICK, family = FONT_EN, legend = "bottom") {
  theme_classic(base_size = base_size, base_family = family) +
    theme(
      axis.text      = element_text(size = FS_TICK, colour = "black"),
      axis.title     = element_text(size = FS_LAB, colour = "black"),
      axis.line      = element_line(linewidth = 0.28, colour = "black"),   # 0.8 pt
      axis.ticks     = element_line(linewidth = 0.28, colour = "black"),
      axis.ticks.length = unit(1.0, "mm"),
      plot.title     = element_text(size = FS_TITLE, hjust = 0.5),
      strip.text     = element_text(size = FS_TITLE, margin = margin(1.5, 0, 1.5, 0)),
      strip.background = element_blank(),
      legend.position = legend,
      legend.title   = element_blank(),
      legend.text    = element_text(size = FS_LEG),
      legend.key.size = unit(3.2, "mm"),
      legend.margin  = margin(0, 0, 0, 0),
      panel.grid     = element_blank(),
      plot.margin    = margin(2, 3, 1, 2, unit = "mm")
    )
}

save_png <- function(plot, file, width_cm, height_cm) {
  message("saving ", file)
  ragg::agg_png(file.path(outdir, file), width = width_cm, height = height_cm,
                units = "cm", res = 400, background = "white")
  print(plot)
  dev.off()
}

MOD <- c(ps = "PS", td = "2D", xr = "XR")
DIM <- c(men = "MD", phy = "PD", tem = "TD", per = "PF", eff = "EF", com = "CM")
col_mod <- c(PS = "#3B6EA5", `2D` = "#D0803A", XR = "#4E9E5B")
col_cls <- c(`1` = "#D62728", `2` = "#1F77B4", `3` = "#2CA02C", `4` = "#9467BD")
n_class <- c(`1` = 50, `2` = 62, `3` = 42, `4` = 87)

## ========================================================== 图1 四类剖面
cm_df <- read_excel(file.path(here, "class_means.xlsx"))
long1 <- cm_df %>%
  pivot_longer(-class, names_to = "var", values_to = "score") %>%
  mutate(modality = factor(MOD[sub("_.*", "", var)], levels = c("PS", "2D", "XR")),
         dimension = factor(DIM[sub(".*_", "", var)], levels = unname(DIM)),
         cls = factor(as.character(class), levels = names(col_cls)))

p1 <- ggplot(long1, aes(x = dimension, y = score, colour = cls, group = cls)) +
  geom_line(linewidth = 0.39) +
  geom_point(size = 1.1) +
  facet_wrap(~ modality, ncol = 1, scales = "free",
             labeller = as_labeller(c(PS = "Physical specimen", `2D` = "2D atlas", XR = "3D XR"))) +
  scale_colour_manual(values = col_cls, labels = function(x) paste("Class", x), name = NULL) +
  scale_y_continuous(limits = c(1.5, 6.4), breaks = 2:6, expand = expansion(0, 0)) +
  labs(x = NULL, y = "Mean score (1–7)") +
  theme_pub() +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box.spacing = unit(2, "mm"),
        panel.spacing = unit(4.5, "mm"),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.28),
        axis.line = element_blank(),
        panel.grid.major.y = element_line(linewidth = 0.14, linetype = "dotted",
                                          colour = "grey80"),
        strip.text = element_text(size = FS_TITLE, margin = margin(1.2, 0, 1.2, 0)),
        axis.text.x = element_text(size = FS_TICK),
        axis.text.y = element_text(size = FS_TICK)) +
  guides(colour = guide_legend(nrow = 1, byrow = TRUE))
save_png(p1, "fig1_profile.png", 16.0, 11.0)

## ========================================================== 图2 性别构成
fem <- c(`1` = 35, `2` = 50, `3` = 22, `4` = 51)   # 依 LPA 后验分类统计（原始公开数据无类别标签）
sex_df <- data.frame(cls = factor(names(n_class), levels = names(n_class))) %>%
  mutate(n = as.numeric(n_class[as.character(cls)]),
         fem = as.numeric(fem[as.character(cls)]),
         mal = n - fem,
         p_f = fem / n * 100,
         p_m = mal / n * 100,
         lab = c("标本主导高负荷型", "二维图谱主导高负荷型", "全域低负荷型", "全模态中高负荷型"),
         xlab = sprintf("类别%s（n=%d）\n%s", cls, n, lab))

sex_long <- bind_rows(
  sex_df %>% transmute(cls, xlab, sex = "男性", n_sex = mal, pct = p_m, ymin = 0,  ymax = p_m),
  sex_df %>% transmute(cls, xlab, sex = "女性", n_sex = fem, pct = p_f, ymin = p_m, ymax = 100)
) %>% mutate(sex = factor(sex, levels = c("女性", "男性")),
             ord = ifelse(sex == "女性", 2, 1),      # 先画男性、后画女性，红色在上
             x = as.numeric(cls),
             ymid = (ymin + ymax) / 2,
             txt = sprintf("%d\n(%.1f%%)", n_sex, pct)) %>%
  arrange(ord, x)

p2 <- ggplot(sex_long) +
  geom_rect(aes(xmin = x - 0.31, xmax = x + 0.31, ymin = ymin, ymax = ymax, fill = sex),
            colour = "white", linewidth = 0.18) +
  geom_text(aes(x = x, y = ymid, label = txt), size = 6.6 * PT, colour = "white",
            fontface = "bold", lineheight = 0.95, family = FONT_EN) +
  scale_fill_manual(values = c(女性 = "#DB4B39", 男性 = "#58A8C8"), name = NULL) +
  scale_x_continuous(breaks = seq_len(nrow(sex_df)), labels = sex_df$xlab,
                     expand = expansion(0.02, 0.02)) +
  scale_y_continuous(breaks = c(0, 25, 50, 75, 100), expand = expansion(0, 0)) +
  coord_cartesian(ylim = c(0, 100)) +
  labs(x = NULL, y = "性别占比（%）") +
  theme_pub(family = FONT_CN) +
  theme(axis.text.x = element_text(size = 6.0, lineheight = 1.0, colour = "black"),
        axis.line.x = element_line(linewidth = 0.28),
        axis.ticks.x = element_blank(),
        panel.grid.major.y = element_line(linewidth = 0.14, colour = "grey88"),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.justification = "center") +
  guides(fill = guide_legend(nrow = 1))
save_png(p2, "fig2_sex.png", 11.0, 8.9)

## ========================================================== 图3 相关矩阵热图
mat <- read.csv(file.path(here, "spearman_cor_matrix.csv"), row.names = 1,
                check.names = FALSE)
n_v <- nrow(mat)
labs <- paste0(MOD[sub("_.*", "", rownames(mat))], "-", DIM[sub(".*_", "", rownames(mat))])
N_OBS <- 241                       # 样本量，用于把 r 换算成 P 再换算成 q

## 逐对由 r 反算 P（t 近似，df = n-2），再对 153 对做 Benjamini–Hochberg 校正
p_of_r <- function(r, n = N_OBS) 2 * pt(abs(r) * sqrt(n - 2) / sqrt(1 - r^2),
                                        df = n - 2, lower.tail = FALSE)

pairs <- expand.grid(i = seq_len(n_v), j = seq_len(n_v)) %>%
  filter(i < j) %>%                                   # 只取上三角，共 153 对
  mutate(r = mat[cbind(i, j)],
         p = p_of_r(r)) %>%
  mutate(q = p.adjust(p, method = "BH"))              # BH 校正只在 153 对内进行

heat <- expand.grid(i = seq_len(n_v), j = seq_len(n_v)) %>%
  mutate(r = as.vector(mat[cbind(i, j)]),
         x = j, y = n_v + 1 - i,
         lab_x = factor(labs[j], levels = labs),
         keep = i != j) %>%
  left_join(pairs %>% select(i, j, q), by = c("i", "j")) %>%
  left_join(pairs %>% rename(i2 = j, j2 = i) %>% select(i2, j2, q), by = c("i" = "i2", "j" = "j2")) %>%
  mutate(q = coalesce(q.x, q.y),
         show = keep & abs(r) >= 0.40 & q < 0.05)     # 与正文一致：q<0.05 且 |r|≥0.40
cat("|r|>=0.40 的对数:", sum(pairs$r %>% abs() >= 0.40),
    "；同时 q<0.05 的对数:", sum(pairs$r %>% abs() >= 0.40 & pairs$q < 0.05), "\n")

p3 <- ggplot(heat, aes(x = x, y = y, fill = r)) +
  geom_tile(data = subset(heat, keep), colour = "white", linewidth = 0.2) +
  geom_text(data = subset(heat, show), aes(label = sprintf("%.2f", r)),
            size = 5.8 * PT, fontface = "bold", family = FONT_EN,
            colour = ifelse(abs(subset(heat, show)$r) > 0.45, "white", "black")) +
  ## 分隔线只画在矩阵内部，不向上穿过模态表头
  annotate("segment", x = c(6.5, 12.5), xend = c(6.5, 12.5),
           y = 0.5, yend = n_v + 0.5, linewidth = 0.28) +
  annotate("segment", x = 0.5, xend = n_v + 0.5,
           y = c(6.5, 12.5), yend = c(6.5, 12.5), linewidth = 0.28) +
  annotate("rect", xmin = 0.5, xmax = n_v + 0.5, ymin = 0.5, ymax = n_v + 0.5,
           fill = NA, colour = "black", linewidth = 0.36) +
  ## 模态表头正对三个 6×6 分块的中心（3.5 / 9.5 / 15.5），不再偏移半列
  annotate("text", x = c(3.5, 9.5, 15.5), y = n_v + 1.7, label = c("PS", "2D", "XR"),
           size = 7.5 * PT, fontface = "bold", family = FONT_EN) +
  ## 表头下不加横线，保持与正文插图一致的干净版式
  scale_fill_gradient2(low = "#2166AC", mid = "#F7F7F7", high = "#B2182B",
                       midpoint = 0, limits = c(-0.6, 0.6), oob = squish,
                       breaks = c(-0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6),
                       name = "Spearman r",
                       guide = guide_colourbar(barwidth = unit(4, "mm"),
                                               barheight = unit(112, "mm"),
                                               title.position = "right",
                                               title.vjust = 0.5,
                                               ticks.colour = "black")) +
  scale_x_continuous(breaks = seq_len(n_v), labels = labs, expand = c(0, 0)) +
  scale_y_continuous(breaks = seq_len(n_v), labels = rev(labs), expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  labs(x = NULL, y = NULL) +
  theme_pub() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 6),
        axis.text.y = element_text(size = 6),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        legend.position = "right",
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 7, angle = 90, hjust = 0.5, vjust = 0.5,
                                    margin = margin(0, 2, 0, 0)),
        panel.background = element_rect(fill = "white", colour = NA),
        plot.margin = margin(6, 7, 2, 2, unit = "mm"))
save_png(p3, "fig3_heat.png", 14.5, 13.35)

## ========================================================== 图4 强边强度（棒棒糖图）
ed <- read.csv(file.path(here, "network_edges_r04.csv"), check.names = FALSE)
ed <- ed %>%
  mutate(m1 = MOD[sub("_.*", "", from)], m2 = MOD[sub("_.*", "", to)],
         d1 = DIM[sub(".*_", "", from)], d2 = DIM[sub(".*_", "", to)],
         ord = match(m1, c("PS", "2D", "XR")),
         lab = sprintf("%s-%s – %s-%s", m1, d1, m2, d2)) %>%
  arrange(ord, desc(weight)) %>%
  mutate(ypos = rev(seq_len(n())),
         col = col_mod[m1])

p4 <- ggplot(ed) +
  geom_segment(aes(x = 0, xend = weight, y = ypos, yend = ypos, colour = m1),
               linewidth = 0.39, alpha = 0.7, show.legend = FALSE) +
  geom_point(aes(x = weight, y = ypos, colour = m1), size = 1.15, show.legend = FALSE) +
  geom_text(aes(x = weight + 0.013, y = ypos, label = sprintf("%.2f", weight), colour = m1),
            size = FS_ANN * PT, fontface = "bold", hjust = 0, family = FONT_EN,
            show.legend = FALSE) +
  geom_vline(xintercept = 0.40, linetype = "dashed", linewidth = 0.25, colour = "grey35") +
  annotate("text", x = 0.406, y = 0.30, label = "r = 0.40",
           size = 6.5 * PT, colour = "grey35", hjust = 0, family = FONT_EN) +
  geom_hline(yintercept = c(5.5, 2.5), linewidth = 0.25, colour = "grey85") +
  scale_colour_manual(values = col_mod) +
  scale_x_continuous(limits = c(0, 0.62), breaks = seq(0, 0.6, 0.1),
                     expand = expansion(0, 0)) +
  scale_y_continuous(breaks = ed$ypos, labels = ed$lab,
                     limits = c(-1.1, max(ed$ypos) + 0.4), expand = expansion(0, 0)) +
  coord_cartesian(clip = "off") +
  labs(x = "Spearman r", y = NULL) +
  theme_pub() +
  theme(axis.text.y = element_text(size = 6.6, colour = ed$col),
        axis.ticks.y = element_blank(),
        axis.line.y = element_line(linewidth = 0.28),
        plot.margin = margin(3, 2, 2, 2, unit = "mm"))
save_png(p4, "fig4_edges.png", 13.5, 8.5)

cat("四张图已输出到", outdir, "\n")

