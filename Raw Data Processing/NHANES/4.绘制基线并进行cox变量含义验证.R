####---导入数据---####
#读取package
{library(survival)
  library(rms)
  library(tidyverse)
  library(caret)
  library(glmnet)
  library(openxlsx)
  library(data.table)
  library(tidyverse)
  library(caret)
  library(openxlsx)
  library(data.table)
  library(mlbench)
  library(klaR)
  library(tidyverse)
  library(caret)
  library(glmnet)
  library(openxlsx)
  library(data.table)
  library(mlbench)
  library(ROCR)
  library(pROC)
  library(multipleROC)
  library(pROC)
  library(DALEX)
  library(randomForest)
  library(dplyr)
  library(survival)
  library(survminer)
}
##读取数据
rm(list=ls())
#载入数据
# 读取数据
data1 <- readRDS("Result/CKM Stage.RDS")
data1 <- data1[!apply(data1, 1, function(row) any(row %in% c("7_Refused", "9_Don't Know"))), ]

####绘制基线调查表####
library(autoReg)
library(moonBook)
library(rrtable)
#data1$Status <- as.factor(data1$Status)

# 检查每列的非缺失值数量
cat("每列的非缺失值数量：\n")
print(sapply(data1, function(x) sum(!is.na(x))))

names(data1) <- gsub(" ", "_", names(data1))

categorical_vars <- c("Sex", "Marital_status", "Education_level", "Race", 
                      "Smoking", "Diabetes", "Cancer", "Stroke", "Hypertension",
                      "Hyperlipidemia", "Congestiveheart_failure", 
                      "Myocardial_infarction", "Angina_pectoris", 
                      "Breath_shortness", "BMI_Category", "WC_Category", 
                      "FPG_Category", "HbA1C_Category", "SBP_Category", 
                      "DBP_Category", "TG_Category", "HDL_Category", 
                      "eGFR_Category", "Subclinical_evidence", 
                      "Clinical_evidence", "CKM_stage_ab")

data1[categorical_vars] <- lapply(data1[categorical_vars], as.factor)

#3表示首先执行Shapiro-Wilk检验，由检验结果决定按正态还是非正态对数据进行分析。
# 运行 mytable
tryCatch({
  moonbook_result <- mytable(Status ~ .,
                             data = data1, digits = 2, method = 3, catMethod = 0, show.total = TRUE)
  cat("mytable 运行成功！\n")
}, error = function(e) {
  cat("运行 mytable 时出错：\n")
  print(e)
})


table2docx(moonbook_result[["res"]],"Result/moonbook_Baseline.docx")
if(! dir.exists("Result")){dir.create("Result")}
write.xlsx(moonbook_result[["res"]],"Result/moonbook_Baseline.xlsx")


#亚组分析
tryCatch({
  subgroup_result <- mytable(Status+CKM_stage_ab~.,
                             data = data1, digits = 2, method = 3, catMethod = 0, show.total = TRUE)
  cat("mytable 运行成功！\n")
}, error = function(e) {
  cat("运行 mytable 时出错：\n")
  print(e)
})

table2docx(subgroup_result,"Result/subgroup_ckm.docx")

####---导出为excel
library(openxlsx)

# 创建新的Excel工作簿
wb <- createWorkbook()

# 提取第一个分组（Status=0）的结果表
df_status0 <- as.data.frame(subgroup_result[[1]]$res)
# 去除冗余的空格（观察到你数据中的CKM_stage_ab列有大量空格）
df_status0$CKM_stage_ab <- trimws(df_status0$CKM_stage_ab)

# 添加第一个sheet
addWorksheet(wb, "Status_0")
writeData(wb, "Status_0", 
          df_status0,
          startCol = 1, startRow = 1, 
          headerStyle = createStyle(textDecoration = "bold"))

# 提取第二个分组（Status=1）的结果表
df_status1 <- as.data.frame(subgroup_result[[2]]$res)
df_status1$CKM_stage_ab <- trimws(df_status1$CKM_stage_ab)

# 添加第二个sheet
addWorksheet(wb, "Status_1")
writeData(wb, "Status_1", 
          df_status1,
          startCol = 1, startRow = 1, 
          headerStyle = createStyle(textDecoration = "bold"))

# 保存Excel文件
saveWorkbook(wb, "Result/subgroup_ckm.xlsx", overwrite = TRUE)

#逻辑模型拟合
#data$出院方式 <- as.factor(data$出院方式)
#data <- data[,-(5:6)]
#data <- data[,-18]
#data <- data[,-1]###去除SEQN
library(survival)

mod <- coxph(Surv(Time, Status) ~ .,###变量共线性的问题，手动选择变量
             data = data1#, id = PatientID
             )
summary(mod)

#单变量和多元逻辑回归（单变量向后选择）
单变量和多元逻辑回归 <- autoReg(mod,
                      uni=T,#逻辑值，是否执行单变量回归。默认为FALSE。
                      #multi=T,#逻辑值，是否执行多变量回归。默认为TRUE。
                      final=T,#逻辑值，是否执行逐步向后消除法。默认为FALSE。
                      #imputed=T,#逻辑值，是否执行多重插补。默认为FALSE。
                      threshold=0.05#,#数值，用于自动选择变量的p值阈值。默认为0.2。
                      #keepstats=T,#逻辑值，是否保留统计数据。默认为FALSE。
                      #showstats=T#,逻辑值，是否显示描述性统计数据。默认为TRUE。
                      )

# 移除含NA的变量（根据summary结果筛选）
exclude_vars <- c("Subclinical_evidence", "Clinical_evidence", 
                  "BMI_Score", "WC_Score", "HbA1C_Score", "FPG_Score",
                  "SBP_Score", "DBP_Score", "TG_Score", "HDL_Score",
                  "eGFR_Score", "Subclinical_Score", "Clinical_Score")

data_clean <- data1[, !(names(data1) %in% exclude_vars)]

# 重新拟合Cox模型
mod_clean <- coxph(Surv(Time, Status) ~ ., data = data_clean)
summary(mod_clean)

# 运行autoReg
result <- autoReg(mod_clean, 
                  uni = TRUE,    # 执行单变量分析
                  final = TRUE,  # 显示最终模型
                  threshold = 0.05)  # 阈值

saveRDS(result,"Result/cox result.RDS")
write.xlsx(result,"Result/cox result.xlsx")
#查看模型的统计量
模型的统计量 <- gaze(mod_clean)
write.xlsx(模型的统计量,"Result/cox description.xlsx")

#模型可视化-绘制森林图
#modelPlot(mod_clean)
#保存图片
# 1. 创建画布
png( 
  filename = "Result/Forest plot.jpg", # 文件名称
  width = 7000,           # 宽
  height = 6000,          # 高
  units = "px",          # 单位
  bg = "white",          # 背景颜色
  res = 600)              # 分辨率
# 2. 绘图
modelPlot(mod_clean)
# 3. 关闭画布
dev.off()

