####                        upset图取交集                -----------------------
rm(list = ls())
library(openxlsx)
NHANES <- read.xlsx("NHANES/Result/合格指标/merged_results.xlsx")
CHARLS <- read.xlsx("CHARLS/Result/合格指标/merged_results.xlsx")

NHANES
CHARLS

# 加载必要的包
library(tibble)
# 加载必要的包
library(UpSetR)
library(dplyr)
library(tidyr)
library(purrr)

# 创建输出文件夹(先删除已存在的)
if(dir.exists("upset")) unlink("upset", recursive = TRUE)
dir.create("upset")

# 标注数据来源并合并数据
nh <- NHANES %>% mutate(Source = "NHANES")
ch <- CHARLS %>% mutate(Source = "CHARLS")
combined <- bind_rows(nh, ch) %>% 
  mutate(Group_Source = paste(Group, Source, sep = "_"))

# 创建分组变量列表（修复deframe问题）
group_list <- combined %>%
  group_by(Group_Source) %>%
  summarise(Variables = list(unique(Variable))) %>%
  {setNames(.$Variables, .$Group_Source)}

# 生成UpSet图所需矩阵
upset_data <- combined %>%
  select(Variable, Group_Source) %>%
  mutate(value = 1) %>%
  pivot_wider(names_from = Group_Source, values_from = value, values_fill = 0) %>%
  as.data.frame()

# 绘制UpSet图并保存
pdf("upset/upset_plot.pdf", width = 12, height = 8)
upset(upset_data, 
      nsets = 20, 
      nintersects = 20,
      mb.ratio = c(0.6, 0.4),
      order.by = "freq",
      decreasing = TRUE)
dev.off()

jpeg("upset/upset_plot.jpg", width = 6, height = 4, units = 'in', res = 120)
upset(upset_data, 
      nsets = 20, 
      nintersects = 20,
      mb.ratio = c(0.3, 0.7),
      order.by = "freq",
      decreasing = TRUE)
dev.off()

library(tidyverse)
library(openxlsx)

# 输出Excel文件
write.xlsx(upset_data, "upset/upset_data.xlsx")

# 生成包含CIndex的宽表数据
upset_data_auc <- combined %>%
  select(Variable, Group_Source, CIndex_or_AUC) %>%
  # 确保每个变量在每组中唯一（取最大值）
  group_by(Variable, Group_Source) %>%
  summarise(CIndex = max(CIndex_or_AUC), .groups = "drop") %>%
  # 转换为宽格式
  pivot_wider(
    names_from = Group_Source,
    values_from = CIndex,
    values_fill = NA  # 缺失组合填充0
  ) %>%
  as.data.frame()

library(autoReg)
baseline <- gaze(Variable~Group+CIndex_or_AUC+p_value_num, data=combined,method=3)

# 输出Excel文件
write.xlsx(baseline, "upset/baseline.xlsx")


###   IQR   分类  ------------------------------------------------
library(dplyr)

# 计算全局四分位数
quantiles <- quantile(combined$CIndex_or_AUC, probs = seq(0, 1, 0.25), na.rm = TRUE)

# 创建四分位分箱标签
cluster_data <- combined %>%
  mutate(
    Quantile_Cluster = cut(CIndex_or_AUC,
                           breaks = quantiles,
                           labels = c("Q1", "Q2", "Q3", "Q4"),
                           include.lowest = TRUE)
  ) %>%
  select(Quantile_Cluster)  # 只保留新列

# 合并到原始数据
final_data <- bind_cols(combined, cluster_data)

# 验证结果
final_data %>%
  select(CIndex_or_AUC, Quantile_Cluster) %>%
  group_by(Quantile_Cluster) %>%
  summarise(
    Min = min(CIndex_or_AUC),
    Max = max(CIndex_or_AUC),
    Count = n()
  )

colnames(final_data)

baseline1 <- gaze(Quantile_Cluster~Variable+Group+CIndex_or_AUC+p_value_num, final_data,method=3)

# 输出Excel文件
write.xlsx(baseline1, "upset/baseline Quantile_Cluster.xlsx")

baseline2 <- gaze(Variable~Group+CIndex_or_AUC+p_value_num+Quantile_Cluster, final_data,method=3)

# 输出Excel文件
write.xlsx(baseline2, "upset/baseline Quantile_Cluster of Variable.xlsx")
