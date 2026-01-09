rm(list = ls())
library(openxlsx)
library(dplyr)

adj <- read.xlsx("RCS/Hypertension/RCS_Statistical_Results.xlsx")

adj
str(adj)

# 加载必要包
library(purrr)
library(gridExtra)

# 定义输出主目录
main_output_dir <- "RCS adjust"
if (!dir.exists(main_output_dir)) dir.create(main_output_dir)

# 按DataSource分组处理
data_list <- adj %>%
  group_by(DataSource) %>%
  group_split()

# 创建处理函数
process_dataset <- function(data) {
  # 创建子目录
  data_source <- unique(data$DataSource)
  output_subdir <- file.path(main_output_dir, data_source)
  if (!dir.exists(output_subdir)) dir.create(output_subdir)
  
  # 长格式转换
  df_long <- data %>%
    pivot_longer(cols = c(AUC, Sensitivity, Specificity),
                 names_to = "Metric", values_to = "Value") %>%
    filter(Adjusted_Var %in% c("unadjusted", "Status", "Age", "Sex", 
                               "Diabetes", "Cancer", "Stroke", "Hypertension"))
  
  # 统计检验
  stat_test <- df_long %>%
    group_by(Metric, Variable) %>%
    wilcox_test(Value ~ Adjusted_Var, ref.group = "unadjusted") %>%
    adjust_pvalue(method = "fdr") %>%
    add_significance("p.adj")
  
  # 保存统计结果
  write.csv(stat_test, file.path(output_subdir, paste0(data_source, "_stats.csv")))
  
  # 生成分面箱线图
  p <- ggplot(df_long, aes(x = Adjusted_Var, y = Value, fill = Adjusted_Var)) +
    geom_boxplot() +
    geom_jitter(width = 0.2, alpha = 0.4, color = "grey20") +
    stat_compare_means(
      comparisons = list(
        c("unadjusted", "Status"), c("unadjusted", "Age"),
        c("unadjusted", "Sex"), c("unadjusted", "Diabetes"),
        c("unadjusted", "Cancer"), c("unadjusted", "Stroke"),
        c("unadjusted", "Hypertension")
      ),
      method = "wilcox.test",
      label = "p.signif",
      step.increase = 0.1
    ) +
    facet_grid(Metric ~ Variable, scales = "free_y") +
    scale_fill_brewer(palette = "Set2") +
    labs(title = data_source, x = "Adjustment Type", y = "Metric Value") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # 保存单独图片
  ggsave(file.path(output_subdir, paste0(data_source, "_plot.png")), 
         p, width = 12, height = 8)
  
  return(p)
}

# 对所有数据集执行处理
plots <- map(data_list, process_dataset)

# 合并图片（假设有两个数据库）
combined_plot <- grid.arrange(
  plots[[1]] + theme(legend.position = "none"), 
  plots[[2]] + theme(legend.position = "none"),
  ncol = 1,
  top = "Comparison Between Two Databases"
)

# 保存合并图
ggsave(file.path(main_output_dir, "Combined_Plot.png"), 
       combined_plot, width = 10, height = 20)
