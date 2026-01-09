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
Meta <- readRDS("Result/待插补数据.RDS")
library(haven)

# 去除所有列的标签
Meta <- as.data.frame(lapply(Meta, zap_labels))

# 写入 CSV 文件
write.csv(Meta, "Result/待插补数据.csv", row.names = FALSE)

Meta <- read.csv("Result/待插补数据.csv")
# 将列名中的点（.）替换为空格
names(Meta) <- gsub("\\.", " ", names(Meta))
str(Meta)

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
imputed_data <- mice(Meta1, method = "rf", m = 5
                     #,
                     #maxit = 5, seed = 123
                     )
Meta1_imputed <- complete(imputed_data, 1)

# 还原字符型变量的数字编码
#Meta1_imputed <- Meta1_imputed %>%
#  mutate(across(all_of(char_var_names), ~ char_var_levels[[cur_column()]][.x]))

# 还原 Meta1 的变量名
colnames(Meta1_imputed) <- original_names

####--- 合并插补后的 Meta1 和 Null_exceed 数据框 ---###
Meta2 <- cbind(Meta1_imputed, Null_exceed)
saveRDS(Meta2,file = "Result/Meta2.RDS")

# 统计每个变量的缺失率
missing_rates <- colSums(is.na(Meta2)) / nrow(Meta2)
# 筛选出缺失率非零的列名
cols <- names(missing_rates[missing_rates > 0])
# 打印每个变量的缺失率
print("每个变量的缺失率：")
print(missing_rates)

#可以选择是否去除一些缺失率特别高的
#Meta2 <- Meta2[, -c(29:37,40,42,44,45)]

# 统计每个变量的缺失率
missing_rates <- colSums(is.na(Meta2)) / nrow(Meta2)
# 筛选出缺失率非零的列名
cols <- names(missing_rates[missing_rates > 0])
# 打印每个变量的缺失率
print("每个变量的缺失率：")
print(missing_rates)

# 打印初始缺失率
initial_rates <- colSums(is.na(Meta2[, cols])) / nrow(Meta2)
print(paste("Initial NA rates:", paste(initial_rates, collapse = ", ")))

# 目标缺失率
target_rate <- 0.29

# 逐步去除行以达到目标缺失率
reduce_missing_rate <- function(data, cols, target_rate, tolerance = 0.01) {
  current_rates <- colSums(is.na(data[, cols])) / nrow(data)
  current_avg_rate <- mean(current_rates)
  
  cat("Starting average missing rate:", current_avg_rate, "\n")
  
  while (abs(current_avg_rate - target_rate) > tolerance) {
    # 找到包含缺失值的行
    na_rows <- which(rowSums(is.na(data[, cols])) > 0)
    
    if (length(na_rows) == 0) {
      cat("No more rows with missing values to remove.\n")
      break
    }
    
    # 按缺失值数量降序排列，优先删除缺失值最多的行
    na_counts <- rowSums(is.na(data[, cols]))
    remove_row <- which.max(na_counts)
    
    # 删除选定的行
    data <- data[-remove_row, ]
    
    # 重新计算当前缺失率
    current_rates <- colSums(is.na(data[, cols])) / nrow(data)
    current_avg_rate <- mean(current_rates)
    
    cat("Current average missing rate:", current_avg_rate, "\n")
  }
  
  return(data)
}

# 调用函数
Meta2 <- reduce_missing_rate(Meta2, cols, target_rate)

# 打印最终结果
cat("Final average missing rate:", mean(colSums(is.na(Meta2[, cols])) / nrow(Meta2)), "\n")

# 使用mice包进行随机森林插补
Meta3 <- Meta2
saveRDS(Meta3,"Result/Meta3.RDS")

# 存储 Meta1 的原始变量名
original_names <- names(Meta3)
# 临时将 Meta1 的变量名更改为 Var1 到 Var28
new_names <- paste0("Var", 1:ncol(Meta3))
names(Meta3) <- new_names
library(mice)
imputed_data <- mice(Meta3, method = "rf", m = 5#, maxit = 5, seed = 123
                     )
Meta3_imputed <- complete(imputed_data, 1)
# 还原 Meta1 的变量名
names(Meta3_imputed) <- original_names

Meta4 <- Meta3_imputed
# 最终缺失率

final_rates <- colSums(is.na(Meta4)) / nrow(Meta4)
print(paste("Final NA rates:", paste(final_rates, collapse = ", ")))

# 统计整个数据框中的NA数量
total_na_count <- sum(is.na(Meta4))
print(paste("Total NA count in Meta4:", total_na_count))

Meta4$Time <- as.numeric(Meta4$Time)
Meta4$Age <- as.numeric(Meta4$Age)

saveRDS(Meta4,"Result/Meta4.RDS")


####二分类变量替换####
#colnames(data) <- gsub(" ", ".", colnames(data))
rm(list = ls())
data <- readRDS("Result/Meta4.RDS")
library(dplyr)

# 将列名中的点（.）替换为空格

names(data) <- gsub("\\ ", ".", names(data))

# 假设你的数据框名为data
# Marital Status列替换
summary(as.factor(data$`Status`))
#data <- data %>%
#  mutate(`Main death reason` = main_map[as.character(`Main death reason`)])

##MCOD for DM
#wheezy_map <- c("1" = "1_Yes", "0" = "2_No")
#data <- data %>% mutate(`MCOD for DM` = wheezy_map[as.character(`MCOD for DM`)])
#data <- data %>% mutate(`MCOD for HTN` = wheezy_map[as.character(`MCOD for HTN`)])

# Gender列替换
# 对Sex变量进行处理，将缺失值替换为"99_NA"
data$Sex <- ifelse(data$Sex == 1, "1_Male",
                   ifelse(data$Sex == 2, "2_Female",data$Sex))

# 对Marital.status变量进行处理，将缺失值替换为"99_NA"
data$Marital.status <- ifelse(data$Marital.status == 1, "1_married ",
                              ifelse(data$Marital.status == 3, "3_partnered ",
                                     ifelse(data$Marital.status == 4, "4_separated",
                                            ifelse(data$Marital.status == 5, "5_divorced ",
                                                   ifelse(data$Marital.status == 7, "7_widowed ",
                                                          ifelse(data$Marital.status == 8, "8_never marriedd",data$Marital.status))))))

# 对Education.level变量进行处理，将缺失值替换为"99_NA"
data$Education.level <- ifelse(data$Education.level == 1, "1_No formal education illiterate",
                               ifelse(data$Education.level == 2, "2_Did not finish primary school but capa",
                                      ifelse(data$Education.level == 3, "3_Sishu",
                                             ifelse(data$Education.level == 4, "4_Elementary school",
                                                    ifelse(data$Education.level == 5, "5_Middle school",
                                                           ifelse(data$Education.level == 6, "6_High school",
                                                                  ifelse(data$Education.level == 7, "7_Vocational school",
                                                                         ifelse(data$Education.level == 8, "8_Two/Three Year College/Associate degree",
                                                                                ifelse(data$Education.level == 9, "9_Four Year College/Bachelor's degree",
                                                                                       ifelse(data$Education.level == 10, "10_Post-graduated(Master/PhD)",data$Education.level))))))))))

# 对Hukou.status变量进行处理，将缺失值替换为"99_NA"
# 查找 "Hukou.status." 列的位置
col_index <- which(names(data) == "Hukou.status.")

# 修改列名
names(data)[col_index] <- "Hukou.status"

data$Hukou.status <- ifelse(data$Hukou.status == 1, "1_Agricultural hukou",
                            ifelse(data$Hukou.status == 2, "2_Non-agricultural hukou",
                                   ifelse(data$Hukou.status == 3, "3_Unified residence hukou",
                                          ifelse(data$Hukou.status == 4, "4_Do not have hukou",data$Hukou.status))))

# 对Smoking变量进行处理，将缺失值替换为"99_NA"
data$Smoking <- ifelse(data$Smoking == 0, "2_No",
                       ifelse(data$Smoking == 1, "1_Yes",data$Smoking))

# 对Drinking变量进行处理，将缺失值替换为"99_NA"
data$Drinking <- ifelse(data$Drinking == 0, "2_No",
                       ifelse(data$Drinking == 1, "1_Yes",data$Drinking))

# 假设 data 是你的数据框，并且已经包含了 Sleep.health 列
data$Sleep.health <- ifelse(data$Sleep.health == 1, "1_Rarely or none of the time < 1 day",
                            ifelse(data$Sleep.health == 2, "2_Some or a little of the time 1-2 days",
                                   ifelse(data$Sleep.health == 3, "3_Occasionally or a moderate amount of 3-4 days",
                                          ifelse(data$Sleep.health == 4, "4_Most or all of the time 5-7 days", data$Sleep.health))))

# Diabetes列替换
diabetes_map <- c("1" = "1_Yes", "0" = "2_No")
data <- data %>%
  mutate(`Diabetes` = diabetes_map[as.character(`Diabetes`)])

# Cancer列替换
cancer_map <- c("1" = "1_Yes", "0" = "2_No")
data <- data %>%
  mutate(`Cancer` = cancer_map[as.character(`Cancer`)])

# Stroke列替换
stroke_map <- c("1" = "1_Yes", "0" = "2_No")
data <- data %>% mutate(`Stroke` = stroke_map[as.character(`Stroke`)])
data <- data %>% mutate(`Hypertension` = stroke_map[as.character(`Hypertension`)])
data <- data %>% mutate(`Dyslipidemia` = stroke_map[as.character(`Dyslipidemia`)])
data <- data %>% mutate(`Heart.disease` = stroke_map[as.character(`Heart.disease`)])
data <- data %>% mutate(`Kidney.Diease` = stroke_map[as.character(`Kidney.Diease`)])

data <- data %>% mutate(`Intensive.Physical.Activity` = stroke_map[as.character(`Intensive.Physical.Activity`)])
data <- data %>% mutate(`Moderate.Physical.Activity` = stroke_map[as.character(`Moderate.Physical.Activity`)])
data <- data %>% mutate(`Light.Physical.Activity` = stroke_map[as.character(`Light.Physical.Activity`)])

stroke_map <- c("1" = "1_Yes", "2" = "2_No")
data <- data %>% mutate(`Chest.pain` = stroke_map[as.character(`Chest.pain`)])
data <- data %>% mutate(`Chest.Pains.When.Climbing` = stroke_map[as.character(`Chest.Pains.When.Climbing`)])


####去除非生理生化指标为NA的####
# 假设你的数据框名为data
#data[1:26][is.na(data[1:26])] <- "99_NA"

# 使用 apply() 函数检查每一行是否包含特定值
#data <- data[!apply(data, 1, function(row) any(row %in% c("7_Refused", "9_Don't Know"))), ]
#data <- data[!apply(data, 1, function(row) any(row %in% c("7_Refused", "9_Don't know"))), ]

# 保存文件
saveRDS(data, file = "Result/Meta4 after.RDS")

