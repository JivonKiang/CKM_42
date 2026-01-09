####  可视化 --------------------------------------------------------------
rm(list = ls())
library(openxlsx)
library(dplyr)

NHANES <- readRDS("NHANES/Result/data.RDS")
CHARLS <- readRDS("CHARLS/Result/data.RDS")

colnames(NHANES)
colnames(CHARLS)

other <- c("Time","Status","Age","Sex","Marital_status","Education_level","Smoking",
           "Diabetes","Cancer","Stroke","Hypertension","CKM_stage_ab")

Variable <- c("AIP","TyG",
              #"eGFR",
              "eGDR_BMI")###从cox汇总里粘贴出来

# 生成需要保留的变量列表
selected_vars <- unique(c(other,Variable))

# 过滤NHANES数据集
NHANES <- NHANES %>%
  select(all_of(selected_vars)) %>%
  # 添加数据集标识列
  mutate(DataSource = "NHANES")

# 过滤CHARLS数据集  
CHARLS <- CHARLS %>%
  select(all_of(selected_vars)) %>%
  # 添加数据集标识列
  mutate(DataSource = "CHARLS")

# 验证结果
cat("NHANES保留变量数:", ncol(NHANES)-1, "\n") # 减去新增的DataSource列
cat("CHARLS保留变量数:", ncol(CHARLS)-1, "\n")

# 查看变量名
cat("\n公共变量结构:\n")
intersect(names(NHANES), names(CHARLS)) %>% print()

whole <- rbind(NHANES,CHARLS)

library(autoReg)
library(rrtable)
library(survival)

data <- whole

# 查看当前因子水平分布（包含未使用的空水平）
table(data$Marital_status, useNA = "always")

# 查看实际存在的有效水平（使用droplevels）
droplevels(data$Marital_status) %>% levels()

# 批量处理所有因子变量
data <- data %>%
  mutate(across(where(is.factor), droplevels))

# 查看当前因子水平分布（包含未使用的空水平）
table(data$Marital_status, useNA = "always")


# 查看当前因子水平分布（包含未使用的空水平）
table(data$Smoking, useNA = "always")

# 查看实际存在的有效水平（使用droplevels）
droplevels(data$Smoking) %>% levels()

# 批量处理所有因子变量
data <- data %>%
  mutate(across(where(is.factor), droplevels))

# 查看当前因子水平分布（包含未使用的空水平）
table(data$Smoking, useNA = "always")

summary(data)
str(data)

# 加载必要的包
library(survival)
library(survminer)
library(openxlsx)
library(tidyverse)

# 创建保存结果的文件夹
if (!dir.exists("KM curve")) dir.create("KM curve")

# 定义分析变量
variables <- c("AIP", "TyG", "eGDR_BMI")

# 初始化结果存储数据结构
result_list <- list(
  C_index = data.frame(),
  KM_stats = data.frame()
)

# 分层分析主循环
for (db in c("NHANES", "CHARLS")) {
  # 按数据库划分数据
  db_data <- data %>% filter(DataSource == db)
  
  for (stage in levels(db_data$CKM_stage_ab)) {
    # 按stage划分数据
    stage_data <- db_data %>% filter(CKM_stage_ab == stage)
    if (nrow(stage_data) < 10) next  # 跳过样本量过小的分组
    
    for (var in variables) {
      # 移除缺失值
      complete_data <- stage_data %>% 
        filter(!is.na(.data[[var]]), !is.na(Time), !is.na(Status))
      
      # 第一部分：计算C-index
      cox_model <- coxph(
        as.formula(paste("Surv(Time, Status) ~", var)), 
        data = complete_data
      )
      c_index <- round(summary(cox_model)$concordance[1], 3)
      
      # 存储C-index结果
      result_list$C_index <- rbind(result_list$C_index, data.frame(
        Database = db,
        Stage = stage,
        Variable = var,
        C_index = c_index,
        N = nrow(complete_data)
      ))
      
      # 第二部分：KM曲线分析
      # 计算四分位数分组
      quantiles <- quantile(complete_data[[var]], probs = c(0, 0.25, 0.5, 0.75, 1))
      # 生成带换行符的标签
      group_labels <- sprintf("Q%d [%.1f to %.1f)", 
                              1:4,
                              round(quantiles[1:4], 1),
                              round(quantiles[2:5], 1))
      
      complete_data$quantile_group <- cut(
        complete_data[[var]],
        breaks = quantiles,
        include.lowest = TRUE,
        labels = group_labels  # 使用新标签
      )
      
      # 拟合生存曲线
      surv_fit <- survfit(Surv(Time, Status) ~ quantile_group, data = complete_data)
      
      # 计算log-rank检验
      logrank <- survdiff(Surv(Time, Status) ~ quantile_group, data = complete_data)
      logrank_p <- 1 - pchisq(logrank$chisq, df = 3)
      
      # 计算各分组的HR
      cox_model_hr <- coxph(
        Surv(Time, Status) ~ relevel(quantile_group, ref = group_labels[1]), 
        data = complete_data
      )
      hr_summary <- summary(cox_model_hr)
      coef_df <- as.data.frame(hr_summary$coefficients)[-1, ]
      confint_df <- as.data.frame(hr_summary$conf.int)[-1, ]
      
      # 生成标注文本
      annotation_text <- c(
        sprintf("Log-rank p = %.3f", logrank_p),
        sapply(1:3, function(i) {
          sprintf("Q%d: HR=%.2f (%.2f-%.2f), p=%.3f", 
                  i+1, confint_df[i,1], confint_df[i,3], confint_df[i,4], coef_df[i,5])
        })
      )
      
      # 绘制生存曲线
      km_plot <- ggsurvplot(
        surv_fit,
        data = complete_data,
        legend.labs = group_labels,  # 确保使用新标签
        pval = FALSE,
        risk.table = FALSE,
        legend.title = "Quantile Groups",
        title = paste(db, "_CKM Stage=", stage, "_", var),
        palette = "jco",
        ggtheme = theme_survminer() +
          theme(
            legend.text = element_text(size = 8, lineheight = 1.1)
            #legend.direction = "vertical",  # 关键参数
          )
      )$plot +
        annotate("text",
                 x = max(complete_data$Time)*0.01, 
                 y = 0.2,
                 label = paste(annotation_text, collapse = "\n"),
                 hjust = 0, size = 3)+
        guides(color = guide_legend(nrow = 2))  # 关键修改：将图例分为两行
      
      # 保存图片
      ggsave(
        filename = file.path("KM curve", 
                             paste0(db, "_CKM_Stage", stage, "_", var, "_KM.png")),
        plot = km_plot,
        width = 5, height = 4, dpi = 300
      )
      
      # 存储HR结果
      for (i in 1:3) {
        result_list$KM_stats <- rbind(result_list$KM_stats, data.frame(
          Database = db,
          Stage = stage,
          Variable = var,
          Comparison = paste("Q", i+1, " vs Q1", sep = ""),
          HR = confint_df[i,1],
          CI_low = confint_df[i,3],
          CI_high = confint_df[i,4],
          P_value = coef_df[i,5]
        ))
      }
    }
  }
}

# 保存结果到Excel
write.xlsx(result_list$C_index, file = "KM curve/C_index_results.xlsx")
write.xlsx(result_list$KM_stats, file = "KM curve/KM_stats_results.xlsx")