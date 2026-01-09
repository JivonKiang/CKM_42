library(ggplot2)
library(dplyr)
library(tidyr)

# 读取数据
stats <- readxl::read_excel("RCS/RCS_Statistical_Results.xlsx")

# 数据处理
plot_data <- stats %>%
  # 转换长格式
  pivot_longer(
    cols = c(AUC, Sensitivity, Specificity),
    names_to = "Metric",
    values_to = "Value"
  ) %>%
  # 创建显著性标签
  mutate(
    Significant = ifelse(Value >= 0.5, "Significant", "Non-significant"),
    Fill = interaction(DataSource, Significant),
    Metric = factor(Metric, levels = c("AUC", "Sensitivity", "Specificity"))
  )

# 自定义颜色映射
fill_colors <- c(
  "CHARLS.Significant" = "#4daf4a",   # CHARLS绿色
  "NHANES.Significant" = "#377eb8",   # NHANES蓝色
  "CHARLS.Non-significant" = "#d3d3d3", 
  "NHANES.Non-significant" = "#d3d3d3"
)

# 生成可视化
p <- ggplot(plot_data, aes(x = Stage_Comparison, y = Value, fill = Fill)) +
  geom_col(position = position_dodge(0.9), width = 0.8) +
  geom_hline(yintercept = 0.5, color = "red", linetype = "dashed", linewidth = 0.8) +
  facet_grid(Variable ~ Metric, scales = "free_x") +
  scale_fill_manual(
    values = fill_colors,
    labels = c("CHARLS.Significant",   # CHARLS绿色
               "NHANES.Significant",   # NHANES蓝色
               "CHARLS.Non-significant", 
               "NHANES.Non-significant",
    guide = guide_legend(title = NULL))
  ) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
  labs(
    title = "Model Performance Metrics by Stage Comparison",
    x = "Stage Comparison",
    y = "Metric Value"
  ) +
  theme_bw(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    panel.grid.major.x = element_blank(),
    strip.background = element_rect(fill = "#f0f0f0"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "top"
  ) +
  geom_text(
    aes(label = sprintf("%.3f", Value), group = DataSource),
    position = position_dodge(0.9),
    vjust = -0.5,
    size = 3,
    color = "black"
  )

# 保存输出
ggsave(
  "RCS/Stage_Comparison_Metrics_Revised.png",
  plot = p,
  width = 14,
  height = 10,
  dpi = 300
)