# 加载必要的库
library(MatchIt)
library(openxlsx)
library(dplyr)
library(foreign)#读取xpt
library(tidyverse)#文件整理
library(dplyr)
library(openxlsx)
library(plyr)
library(nhanesA)
library(RNHANES)
library(VIM)
library(pacman)
p_load(tidyverse) # 数据科学套件
p_load(mice) # 缺失值分析包
p_load(VIM)

rm(list = ls())
# 数据清洗，移除有缺失值的行
Meta <- readRDS("Result/清洗但未插补的数据.RDS")

# 统计每个变量的缺失率
missing_rates <- colSums(is.na(Meta)) / nrow(Meta)

# 打印每个变量的缺失率
print("每个变量的缺失率：")
print(missing_rates)

# 过滤缺失率小于30%的变量
selected_vars <- names(missing_rates)[missing_rates < 0.3]

# 构建新的数据集 Meta1
Meta1 <- Meta[, selected_vars]

# 存储 Meta1 的原始变量名
original_names <- names(Meta1)

# 临时将 Meta1 的变量名更改为 Var1 到 Var28
new_names <- paste0("Var", 1:ncol(Meta1))
names(Meta1) <- new_names

# 过滤缺失率大于30%的变量
exceed_vars <- names(missing_rates)[missing_rates >= 0.3]
Null_exceed <- Meta[, exceed_vars]

# 存储字符型变量的原始值和编码
#char_vars <- sapply(Meta1, is.character)
#char_var_names <- names(Meta1)[char_vars]
#char_var_levels <- lapply(Meta1[char_vars], unique)

# 数字编码字符型变量
#Meta1 <- Meta1 %>%
#  mutate(across(where(is.character), ~ as.integer(as.factor(.x))))

# 使用mice包进行随机森林插补
imputed_data <- mice(Meta1, method = "rf", m = 5, maxit = 5, seed = 123)
Meta1_imputed <- complete(imputed_data, 1)

# 还原字符型变量的数字编码
#Meta1_imputed <- Meta1_imputed %>%
#  mutate(across(all_of(char_var_names), ~ char_var_levels[[cur_column()]][.x]))

# 还原 Meta1 的变量名
colnames(Meta1_imputed) <- original_names

####--- 合并插补后的 Meta1 和 Null_exceed 数据框 ---###
Meta2 <- cbind(Meta1_imputed, Null_exceed)

# 查看合并后的数据框
head(Meta2)

# 统计每个变量的缺失率
missing_rates <- colSums(is.na(Meta2)) / nrow(Meta2)

# 打印每个变量的缺失率
print("每个变量的缺失率：")
print(missing_rates)

# 计算初始缺失率
initial_na_rate <- function(df, cols) {
  na_counts <- colSums(is.na(df[, cols]))
  na_rate <- na_counts / nrow(df)
  return(na_rate)
}

# 初始缺失率
# 筛选出缺失率非零的列名
cols <- names(missing_rates[missing_rates > 0])

# 查看结果
print(cols)

initial_rates <- initial_na_rate(Meta2, cols)#输入缺失所在列
print(paste("Initial NA rates:", paste(initial_rates, collapse = ", ")))

# 目标缺失率
target_rate <- 0.27

# 逐步去除行
while (TRUE) {
  # 计算当前缺失率
  current_rates <- initial_na_rate(Meta2, cols)
  current_avg_rate <- mean(current_rates)
  
  # 检查是否达到目标缺失率
  if (abs(current_avg_rate - target_rate) < 0.01) {
    break
  }
  
  # 去除包含NA的行
  na_rows <- which(rowSums(is.na(Meta2[, cols])) > 0)
  if (length(na_rows) == 0) {
    break  # 如果没有NA行可去除，退出循环
  }
  
  # 随机选择一个NA行去除
  remove_row <- sample(na_rows, 1)
  Meta2 <- Meta2[-remove_row, ]
}

# 最终缺失率
final_rates <- initial_na_rate(Meta2, cols)
print(paste("Final NA rates:", paste(final_rates, collapse = ", ")))

# 使用mice包进行随机森林插补
Meta3 <- Meta2
# 存储 Meta1 的原始变量名
original_names <- names(Meta3)
# 临时将 Meta1 的变量名更改为 Var1 到 Var28
new_names <- paste0("Var", 1:ncol(Meta3))
names(Meta3) <- new_names
imputed_data <- mice(Meta3, method = "rf", m = 5, maxit = 5, seed = 123)
Meta3_imputed <- complete(imputed_data, 1)
# 还原 Meta1 的变量名
names(Meta3_imputed) <- original_names

Meta4 <- Meta3_imputed
# 最终缺失率
final_rates <- initial_na_rate(Meta4, cols)
print(paste("Final NA rates:", paste(final_rates, collapse = ", ")))

# 统计整个数据框中的NA数量
total_na_count <- sum(is.na(Meta4))
print(paste("Total NA count in Meta4:", total_na_count))

Meta4$Time <- as.numeric(Meta4$Time)
Meta4$Age <- as.numeric(Meta4$Age)
# 保存文件
saveRDS(Meta4, file = "Result/Meta4.RDS")
