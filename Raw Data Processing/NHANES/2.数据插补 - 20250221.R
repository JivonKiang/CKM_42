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
Meta2 <- Meta2[, -c(29:37,40,42,44,45)]

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

data <- Meta4
####二分类变量替换####
#colnames(data) <- gsub(" ", ".", colnames(data))
library(dplyr)

# 假设你的数据框名为data
# Marital Status列替换
summary(as.factor(data$`Main death reason`))
main_map <- c("1" = "1_Diseases of heart", 
              "2" = "2_Malignant neoplasms", 
              "3" = "3_Chronic lower respiratory", 
              "4" = "4_Accidents", 
              "5" = "5_Cerebrovascular diseases", 
              "6" = "6_Alzheimer’s disease",
              "7" = "7_Diabetes mellitus",
              "8" = "8_Influenza and pneumonia",
              "9" = "9_Kidney disease",
              "10" = "10_All other causes")
#data <- data %>%
#  mutate(`Main death reason` = main_map[as.character(`Main death reason`)])

##MCOD for DM
wheezy_map <- c("1" = "1_Yes", "0" = "2_No")
#data <- data %>% mutate(`MCOD for DM` = wheezy_map[as.character(`MCOD for DM`)])
#data <- data %>% mutate(`MCOD for HTN` = wheezy_map[as.character(`MCOD for HTN`)])

# Gender列替换
data <- data %>%
  mutate(Sex = case_when(
    Sex == 1 ~ "1_Male",
    Sex == 2 ~ "2_Female",
    TRUE ~ as.character(Sex) # 保持其他值不变
  ))

# Marital Status列替换
marital_level_map <- c("1" = "1_Married", 
                       "2" = "2_Widowed", 
                       "3" = "3_Divorced", 
                       "4" = "4_Separated", 
                       "5" = "5_Never married", 
                       "6" = "6_Living with partner",
                       "77" = "7_Refused", "99" = "9_Don't know")
data <- data %>%
  mutate(`Marital status` = marital_level_map[as.character(`Marital status`)])

# Education level列替换
education_level_map <- c("1" = "1_Less Than 9th Grade", 
                         "2" = "2_9-11th Grade (Includes 12th grade with no diploma)", 
                         "3" = "3_High School Grad/GED or Equivalent", 
                         "4" = "4_Some College or AA degree", 
                         "5" = "5_College Graduate or above", 
                         "7" = "7_Refused", "9" = "9_Don't know")
data <- data %>%
  mutate(`Education level` = education_level_map[as.character(`Education level`)])

# Race列替换
race_map <- c("1" = "1_Mexican American", "2" = "2_Other Hispanic", 
              "3" = "3_Non-Hispanic White", "4" = "4_Non-Hispanic Black", 
              #"6" = "6_Non-Hispanic Asian",
              "5" = "5_	Other Race - Including Multi-Racial"
              )
data <- data %>%
  mutate(Race = race_map[as.character(Race)])

# Smoking列替换
smoking_map <- c("1" = "1_Every day", "2" = "2_Some days", "3" = "3_Not at all", 
                 "7" = "7_Refused", "9" = "9_Don't know")
data <- data %>%
  mutate(`Smoking` = smoking_map[as.character(`Smoking`)])

# Days of drinking in the last year列去除777和999的行
#data <- data %>%
#  filter(`Days of drinking in the last year` != 777 & `Days of drinking in the last year` != 999)

# Activity列替换
wheezy_map <- c("1" = "1_Yes", "2" = "2_No", "7" = "7_Refused", "9" = "9_Don't know")
#data <- data %>% mutate(`Drinking` = wheezy_map[as.character(`Drinking`)])
#data <- data %>% mutate(`Vigorous work activity` = wheezy_map[as.character(`Vigorous work activity`)])
#data <- data %>% mutate(`Moderate work activity` = wheezy_map[as.character(`Moderate work activity`)])
#data <- data %>% mutate(`Mild work activity` = wheezy_map[as.character(`Mild work activity`)])

# Chest sound wheezy during exercise列替换
Sleep_map <- c("1" = "1_Bad", "2" = "1_Bad", "3" = "1_Bad", "0" = "2_Good", "7" = "7_Refused", "9" = "9_Don't know")
#data <- data %>%
#  mutate(`Sleep health` = wheezy_map[as.character(`Sleep health`)])

# Diabetes列替换
diabetes_map <- c("1" = "1_Yes", "3" = "1_Yes", "2" = "2_No", "7" = "7_Refused", "9" = "9_Don't know")
data <- data %>%
  mutate(`Diabetes` = diabetes_map[as.character(`Diabetes`)])

# Cancer列替换
cancer_map <- c("1" = "1_Yes","2" = "2_No", "7" = "7_Refused", "9" = "9_Don't know")
data <- data %>%
  mutate(`Cancer` = cancer_map[as.character(`Cancer`)])

# Stroke列替换
stroke_map <- c("1" = "1_Yes", "2" = "2_No", "7" = "7_Refused", "9" = "9_Don't know")
data <- data %>% mutate(`Stroke` = stroke_map[as.character(`Stroke`)])
data <- data %>% mutate(`Hypertension` = stroke_map[as.character(`Hypertension`)])
data <- data %>% mutate(`Hyperlipidemia` = stroke_map[as.character(`Hyperlipidemia`)])

# Coronary heart disease列替换
chd_map <- c("1" = "1_Yes", "2" = "2_No", "7" = "7_Refused", "9" = "9_Don't know")
#data <- data %>%
#  mutate(`Coronary heart disease` = chd_map[as.character(`Coronary heart disease`)])

# Congestive heart failure列替换
chf_map <- c("1" = "1_Yes", "2" = "2_No", "7" = "7_Refused", "9" = "9_Don't know")
data <- data %>%
  mutate(`Congestiveheart failure` = chf_map[as.character(`Congestiveheart failure`)])

# Myocardial infarction/heart attack列替换
mi_map <- c("1" = "1_Yes", "2" = "2_No", "7" = "7_Refused", "9" = "9_Don't know")
data <- data %>%
  mutate(`Myocardial infarction` = mi_map[as.character(`Myocardial infarction`)])

# Angina pectoris列替换
angina_map <- c("1" = "1_Yes", "2" = "2_No", "7" = "7_Refused", "9" = "9_Don't know")
data <- data %>%
  mutate(`Angina pectoris` = angina_map[as.character(`Angina pectoris`)])


wheezy_map <- c("1" = "1_Yes", "2" = "2_No", "7" = "7_Refused", "9" = "9_Don't know")
#data <- data %>% mutate(`Chest pain` = wheezy_map[as.character(`Chest pain`)])
#data <- data %>% mutate(`Severe chest pain` = wheezy_map[as.character(`Severe chest pain`)])
data <- data %>% mutate(`Breath shortness` = wheezy_map[as.character(`Breath shortness`)])

####去除非生理生化指标为NA的
# 假设你的数据框名为data
#data[1:26][is.na(data[1:26])] <- "99_NA"

# 使用 apply() 函数检查每一行是否包含特定值
data <- data[!apply(data, 1, function(row) any(row %in% c("7_Refused", "9_Don't Know"))), ]
data <- data[!apply(data, 1, function(row) any(row %in% c("7_Refused", "9_Don't know"))), ]

# 保存文件
saveRDS(data, file = "Result/Meta4.RDS")

