rm(list = ls())

library(dplyr)
library(purrr)
library(ggplot2)
library(ggpubr)
library(rstatix)
library(tidyr)  # 推荐方式[2](@ref)
library(tidyverse) # 包含tidyr和dplyr[3](@ref)
library(openxlsx)
index <- readRDS("KM curve/total_results.RDS")$C_index %>% na.omit()

# 创建输出目录
output_dir <- "KM subgroup"
if (!dir.exists(output_dir)) dir.create(output_dir)

library(dplyr)
library(purrr)
library(ggplot2)
library(ggpubr)
library(rstatix)

# 创建输出子目录
dir.create(file.path(output_dir, "Plots"), showWarnings = FALSE)
dir.create(file.path(output_dir, "Stats"), showWarnings = FALSE)

# 过滤并分组数据
analysis_data <- index %>%
  filter(!is.na(C_index)) %>%  # 移除NA值[9,11](@ref)
  group_by(Database, Variable) %>% 
  nest() %>% 
  mutate(
    comparisons = map(data, ~ {
      none_data <- filter(.x, Stratification == "None")
      other_strata <- unique(.x$Stratification[.x$Stratification != "None"])
      
      map_df(other_strata, function(stratum){
        stratum_data <- filter(.x, Stratification == stratum)
        if(nrow(stratum_data) == 0) return(NULL)
        
        # Wilcoxon检验[1,4](@ref)
        wilcox_test <- wilcox.test(none_data$C_index, 
                                   stratum_data$C_index,
                                   na.rm = TRUE)  # 处理NA[10](@ref)
        
        tibble(
          Reference = "None",
          Compared = stratum,
          p.value = wilcox_test$p.value
        )
      })
    }),
    
    # FDR校正[3](@ref)
    adjusted = map(comparisons, ~ .x %>% 
                     mutate(fdr.p = p.adjust(p.value, method = "fdr")))
  )

# 保存统计结果
analysis_data %>% 
  select(Database, Variable, adjusted) %>% 
  unnest(adjusted)
  

# 生成可视化报告
report_data <- analysis_data %>% 
  unnest(adjusted) %>% 
  mutate(
    significance = case_when(
      fdr.p < 0.001 ~ "***",
      fdr.p < 0.01 ~ "**",
      fdr.p < 0.05 ~ "*",
      TRUE ~ "ns"
    )
  )

# 重构比较数据框
comparison_labels <- report_data %>%
  #filter(Stratification != "None") %>%
  transmute(
    group1 = Reference, 
    group2 = Compared,
    p = fdr.p,
    p.signif = significance
  )

write.xlsx(comparison_labels,file.path(output_dir, "statistical_results.xlsx"), 
          row.names = FALSE)

library(ggrepel)

# 自定义绘图函数
plot_comparisons <- function(data, db, var) {
  # 提取当前数据库和变量的比较标签
  current_labels <- comparison_labels %>% 
    filter(Database == db, Variable == var)
  
  ggboxplot(data, 
            x = "Stratification", 
            y = "C_index",
            color = "Stratification",
            palette = "jco") +
    stat_pvalue_manual(
      current_labels,
      label = "p.signif", 
      y.position = seq(
        from = max(data$C_index) * 1.1, 
        by = 0.02, 
        length.out = nrow(current_labels)
      ),  # 动态生成递增的y轴位置
      tip.length = 0.01,
      step.increase = 0.01  # 每组比较的纵向间距
    ) +
    labs(title = paste(db, var, sep = " - ")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# 批量生成图表[6,7](@ref)
analysis_data %>%
  pwalk(function(Database, Variable, data, ...) {
    p <- plot_comparisons(data, Database, Variable)
    
    # 双格式保存[6](@ref)
    ggsave(file.path(output_dir, paste0(Database, "_", Variable, ".png")), 
           plot = p, width = 8, height = 6)
    ggsave(file.path(output_dir, paste0(Database, "_", Variable, ".pdf")),
           plot = p, width = 8, height = 6)
  })

# 在analysis_data中添加plots列存储图形对象
analysis_data <- analysis_data %>%
  mutate(
    plot = pmap(list(data, Database, Variable), function(d, db, var) {
      plot_comparisons(d, db, var) + 
        theme(legend.position = "none")  # 移除图例避免重复
    })
  )

library(patchwork)

# 按Variable分组拼接
# 按Variable分组生成图形列表
plot_list <- analysis_data %>%
  group_by(Variable) %>%
  group_map(~ {
    wrap_plots(.x$plot, nrow = 1) + 
      plot_annotation(tag_levels = "A")
  })

# 垂直拼接所有子图
combined_plots <- wrap_plots(plot_list, ncol = 1) +
  plot_annotation(title = "Subgroup C-index Comparisons")

variable_count <- length(unique(analysis_data$Variable))
row_height <- 6  # 每行高度（英寸）
total_height <- row_height * variable_count

ggsave(
  file.path(output_dir, "combined_plots.png"),
  plot = combined_plots,
  width = 15,                # 假设每行最多5个子图
  height = 6 * length(unique(analysis_data$Variable)), # 动态计算高度
  dpi = 300
)
