# 加载必要的库
library(MatchIt)
library(openxlsx)
library(dplyr)
# 数据清洗，移除有缺失值的行
rm(list = ls())
data <- readRDS("Result/Meta4 after.RDS")
data <- na.omit(data)

names(data) <- gsub("\\.", " ", names(data))

# 获取数值型列的名称，排除 Status 列
num_cols <- setdiff(names(which(sapply(data, is.numeric))), "Status")

# 过滤行
data <- data[apply(data[,num_cols], 1, function(x) any(x == 0) == FALSE), ]

###插补时BMI可能会随机插补，因此要重新计算
library(dplyr)

# 假设 data 是你的数据框，并且已经包含了 Height 和 Weight 列
data <- data %>%
  mutate(Height = Height * 100,  # 将身高从米转换为厘米
         BMI = Weight / (Height / 100) ^ 2)  # 计算 BMI

# 假设 data 是你的数据框，并且已经包含了 BMI 列
data <- data %>%
  filter(BMI >= 10 & BMI <= 60)


###因为是中国队列
data$Race <- c("asian")

library(dplyr)

data <- data %>%
  mutate(BMI_Category = case_when(
    Race %in% "asian" & BMI < 23 ~ "Normal",
    Race %in% "asian" & BMI >= 23 ~ "Abnormal",
    Race != "asian" & BMI < 25 ~ "Normal",
    Race != "asian" & BMI >= 25 ~ "Abnormal"
  ))

data <- data %>% 
  mutate(WC_Category = case_when(
    Race %in% "asian" & Sex == "2_Female" & WC < 80 ~ "Normal",
    Race %in% "asian" & Sex == "1_Male" & WC < 90 ~ "Normal",
    Race != "asian" & Sex == "2_Female" & WC < 88 ~ "Normal",
    Race != "asian" & Sex == "1_Male" & WC < 102 ~ "Normal",
    TRUE ~ "Abnormal"
  ))

data <- data %>% 
  mutate(FPG_Category = case_when(
    is.na(FPG) | !is.numeric(FPG) ~ "Abnormal",  # 处理缺失值和非数值
    FPG < 100 ~ "Normal",
    FPG >= 100 & FPG <= 125 ~ "Middle",
    TRUE ~ "High"  # 其他情况归为 High
  ))

data <- data %>% 
  mutate(HbA1C_Category = case_when(
    is.na(HbA1C) | !is.numeric(HbA1C) ~ "Abnormal",  # 处理缺失值和非数值
    HbA1C < 5.7 ~ "Normal",
    HbA1C >= 5.7 & HbA1C <= 6.4 ~ "Middle",
    TRUE ~ "High"  # 其他情况归为 High
  ))

data <- data %>% 
  mutate(SBP_Category = case_when(
    is.na(SBP) | !is.numeric(SBP) ~ "Abnormal",  # 处理缺失值和非数值
    SBP < 140 ~ "Normal",
    TRUE ~ "High"  # 其他情况归为 High
  ))

data <- data %>% 
  mutate(DBP_Category = case_when(
    is.na(DBP) | !is.numeric(DBP) ~ "Abnormal",  # 处理缺失值和非数值
    DBP <  90 ~ "Normal",
    TRUE ~ "High"  # 其他情况归为 High
  ))

data <- data %>% 
  mutate(TG_Category = case_when(
    is.na(TG) | !is.numeric(TG) ~ "Abnormal",  # 处理缺失值和非数值
    TG < 135 ~ "Normal",
    TRUE ~ "High"  # 其他情况归为 High
  ))
##HDL_Category
data <- data %>% 
  mutate(HDL_Category = case_when(
    is.na(HDL) | !is.numeric(HDL) ~ "Abnormal",  # 处理缺失值和非数值
    HDL >= 50 ~ "Normal",
    TRUE ~ "Abnormal"  # 其他情况归为 High
  ))

###---计算eGFR---###
compute_eGFR <- function(data) {
  # 确定性别的参数
  κ <- ifelse(data$Sex == "2_Female", 0.7, 0.9)
  α <- ifelse(data$Sex == "2_Female", -0.241, -0.302)
  
  # 计算Scr/κ和min/max值
  scr_over_kappa <- data$Scr / κ
  min_val <- pmin(scr_over_kappa, 1)  # 向量化计算min
  max_val <- pmax(scr_over_kappa, 1)  # 向量化计算max
  
  # 计算指数部分
  exponent_part <- (min_val ^ α) * (max_val ^ -1.200)
  
  # 处理年龄调整系数
  age_factor <- 0.9938 ^ data$Age
  
  # 性别额外调整因子（仅女性）
  gender_factor <- ifelse(data$Sex == "2_Female", 1.012, 1)
  
  # 计算eGFRcr
  eGFRcr <- 142 * exponent_part * age_factor * gender_factor
  
  # 将结果添加到数据框中
  data$eGFR <- eGFRcr
  return(data)
}

# 示例数据
test_data <- data.frame(
  Scr = c(1.2, 0.8),
  Age = c(50, 60),
  Sex = c("1_Male", "2_Female")  # 0代表男性，1代表女性
)

# 计算eGFR
result <- compute_eGFR(test_data)

data <- compute_eGFR(data)

data <- data %>% 
  mutate(eGFR_Category = case_when(
    is.na(eGFR) | !is.numeric(eGFR) ~ "Abnormal",  # 处理缺失值和非数值
    eGFR >= 90 ~ "G1",
    eGFR >= 60 & eGFR <= 89 ~ "G2",
    eGFR >= 45 & eGFR <= 59 ~ "G3a",
    eGFR >= 30 & eGFR <= 44 ~ "G3b",
    eGFR >= 15 & eGFR <= 29 ~ "G4",
    TRUE ~ "G5"  # 其他情况归为 High
  ))

###Subclinical_evidence
data <- data %>% 
  mutate(Subclinical_evidence = case_when(
    #`Chest pain` == "1_Yes" | `Severe chest pain` == "1_Yes" | 
      `Chest pain` == "1_Yes"| `Chest Pains When Climbing` == "1_Yes" ~ "Positive" ,
    TRUE ~ "Negative"
  ))

###Clinical_evidence
colnames(data)[18] <- "Kidney disease"

data <- data %>% 
  mutate(Clinical_evidence = case_when(
    `Stroke` == "1_Yes" | `Kidney disease` == "1_Yes" | `Heart disease` == "1_Yes" ~ "Positive",
    TRUE ~ "Negative"
  ))

###---CKM stage---###
# 加载必要的包
library(dplyr)

# 定义变量到数值的映射函数
variable_map <- function(var, value) {
  switch(var,
         "BMI_Category" = ifelse(value == "Abnormal", 1, 0),
         "WC_Category" = ifelse(value == "Abnormal", 1, 0),
         "HbA1C_Category" = case_when(
           value == "High" ~ 2,
           value == "Middle" ~ 1,
           TRUE ~ 0
         ),
         "FPG_Category" = case_when(
           value == "High" ~ 2,
           value == "Middle" ~ 1,
           TRUE ~ 0
         ),
         "SBP_Category" = case_when(
           value == "High" ~ 2,
           TRUE ~ 0
         ),
         "DBP_Category" = case_when(
           value == "High" ~ 2,
           TRUE ~ 0
         ),
         "TG_Category" = case_when(
           value == "Abnormal" ~ 2,
           TRUE ~ 0
         ),
         "HDL_Category" = case_when(
           value == "Abnormal" ~ 2,
           TRUE ~ 0
         ),
         "eGFR_Category" = case_when(
           value %in% c("G4") ~ 3,
           value %in% c("G5") ~ 4,
           TRUE ~ 0
         ),
         "Subclinical_evidence" = case_when(
           value == "Positive" ~ 3,
           TRUE ~ 0
         ),
         "Clinical_evidence" = case_when(
           value == "Positive" ~ 4,
           TRUE ~ 0
         ),
         stop("Unknown variable: ", var)  # 捕获未知变量
  )
}

# 计算每个变量的得分
data <- data %>%
  mutate(
    BMI_Score = variable_map("BMI_Category", BMI_Category),
    WC_Score = variable_map("WC_Category", WC_Category),
    HbA1C_Score = variable_map("HbA1C_Category", HbA1C_Category),
    FPG_Score = variable_map("FPG_Category", FPG_Category),
    SBP_Score = variable_map("SBP_Category", SBP_Category),
    DBP_Score = variable_map("DBP_Category", DBP_Category),
    TG_Score = variable_map("TG_Category", TG_Category),
    HDL_Score = variable_map("HDL_Category", HDL_Category),
    eGFR_Score = variable_map("eGFR_Category", eGFR_Category),
    Subclinical_Score = variable_map("Subclinical_evidence", Subclinical_evidence),
    Clinical_Score = variable_map("Clinical_evidence", Clinical_evidence)
  )

# 获取单个变量分数中的最大值作为 CKM_stage
# 获取单个变量分数中的最大值作为 CKM_stage
data <- data %>%
  mutate(
    CKM_stage = pmax(BMI_Score, WC_Score, HbA1C_Score, FPG_Score,
                      SBP_Score, DBP_Score, TG_Score, HDL_Score,
                      eGFR_Score, Subclinical_Score, Clinical_Score,
                    na.rm = TRUE)  # 忽略NA值
  )

# 查看结果
print(data)

# 使用 mutate 和 case_when 进行条件赋值
data <- data %>%
  mutate(
    CKM_stage_ab = case_when(
      CKM_stage == 4 & eGFR_Category == "G5" ~ "4b",  # 当 CKM_stage=4 且 eGFR_Category=G5 时标记为 4b
      CKM_stage == 4 & eGFR_Category != "G5" ~ "4a",  # 当 CKM_stage=4 但 eGFR_Category 不是 G5 时标记为 4a
      TRUE ~ as.character(CKM_stage)                  # 其他情况保持原值
    )
  )

saveRDS(data,file = "Result/CKM Stage.RDS")
