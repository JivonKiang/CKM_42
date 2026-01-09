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
# 在初始化时添加分层字段
result_list <- list(
  C_index = data.frame(
    Database = character(),
    Stage = character(),
    
    Stratification = character(),  # 新增字段
    Subgroup = character(),         # 新增字段
    
    Variable = character(),
    
    C_index = numeric(),
    N = integer()
  ),
  # 修改KM_stats初始化结构，确保分层/未分层字段统一
  KM_stats = data.frame(
    Database = character(), 
    Stage = character(),
    Stratification = character(),  # 分层类型
    Subgroup = character(),       # 子组标识
    Variable = character(),
    Comparison = character(),
    HR = numeric(),
    CI_low = numeric(),
    CI_high = numeric(),
    P_value = numeric()
  )
)

# 定义分层变量列表（包含处理方式）
strat_vars <- list(
  Age = list(type = "continuous", cutoff = 65),
  Sex = list(type = "factor"),
  Marital_status = list(type = "factor"),
  Education_level = list(type = "factor"),
  Smoking = list(type = "factor"),
  Diabetes = list(type = "factor"),
  Cancer = list(type = "factor"),
  Stroke = list(type = "factor"),
  Hypertension = list(type = "factor")
)

# 创建总结果存储对象
total_results <- list(
  C_index = data.frame(),
  KM_stats = data.frame()
)


# 分层分析主循环
for (analysis_type in c("unstratified", "stratified")) {
  for (db in c("NHANES", "CHARLS")) {
  # 按数据库划分数据
  db_data <- data %>% filter(DataSource == db)
  
  for (stage in levels(db_data$CKM_stage_ab)) {
    # 按stage划分数据
    stage_data <- db_data %>% filter(CKM_stage_ab == stage)
    
    if (nrow(stage_data) < 10) next  # 跳过样本量过小的分组
    
    for (var in variables) {
      # 原始未分层分析--------------------------------------------------------
      if (analysis_type == "unstratified") {
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
      # 在未分层分析中添加分层标识列
      result_list$C_index <- rbind(result_list$C_index, data.frame(
        Database = db,
        Stage = stage,
        Variable = var,
        Stratification = "None",  # 新增列
        Subgroup = "All",         # 新增列
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
          Stratification = "None",  # 新增默认值[4](@ref)
          Subgroup = "All",          # 新增默认值[4](@ref)
          Variable = var,
          Comparison = paste("Q", i+1, " vs Q1", sep = ""),
          HR = confint_df[i,1],
          CI_low = confint_df[i,3],
          CI_high = confint_df[i,4],
          P_value = coef_df[i,5]
        ))
      }
      }
    
    # 分层分析 ---------------------------------------------------------------
      for (strat_var in names(strat_vars)) {
        # 创建分层专用目录
        strat_dir <- file.path("KM curve", strat_var)
        if (!dir.exists(strat_dir)) dir.create(strat_dir)
        
        # 数据预处理
        strat_data <- stage_data %>%
          filter(!is.na(.data[[strat_var]])) %>%
          mutate(
            strat_group = case_when(
              strat_vars[[strat_var]]$type == "continuous" ~ 
                ifelse(.data[[strat_var]] < strat_vars[[strat_var]]$cutoff,
                       paste0("<", strat_vars[[strat_var]]$cutoff),
                       paste0("≥", strat_vars[[strat_var]]$cutoff)),
              TRUE ~ as.character(.data[[strat_var]])
            )
          ) %>%
          group_by(strat_group) %>%
          filter(n() >= 10) %>%  # 过滤小样本组
          ungroup()
        
        # 分层分析循环
        for (subgroup in unique(strat_data$strat_group)) {
          sub_data <- strat_data %>% 
            filter(strat_group == subgroup) %>%
            select(-strat_group)
          
          # 修正为使用当前子组数据：
          complete_data <- sub_data %>%  # 重新定义作用域
            filter(!is.na(.data[[var]]), !is.na(Time), !is.na(Status))
          
          {
            # 第一部分：计算C-index
            cox_model <- coxph(
              as.formula(paste("Surv(Time, Status) ~", var)), 
              data = complete_data
            )
            c_index <- round(summary(cox_model)$concordance[1], 3)
            
            # 存储C-index结果
            # 在未分层分析中添加分层标识列
            result_list$C_index <- rbind(result_list$C_index, data.frame(
              Database = db,
              Stage = stage,
              Variable = var,
              Stratification = strat_var,  # 新增列
              Subgroup = subgroup,         # 新增列
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
            
            # 特殊文件命名
            ggsave(
              file.path(strat_dir, paste(db, stage, var, subgroup, "KM.png", sep = "_")),
              plot = km_plot,
              width = 5, height = 4, dpi = 300
            )
            
            # 存储HR结果
            for (i in 1:3) {
              result_list$KM_stats <- rbind(result_list$KM_stats, data.frame(
                Database = db,
                Stage = stage,
                Stratification = strat_var,  # 新增默认值[4](@ref)
                Subgroup = subgroup,          # 新增默认值[4](@ref)
                Variable = var,
                Comparison = paste("Q", i+1, " vs Q1", sep = ""),
                HR = confint_df[i,1],
                CI_low = confint_df[i,3],
                CI_high = confint_df[i,4],
                P_value = coef_df[i,5]
              ))
            }
            
            total_results <- list(
              C_index = bind_rows(total_results$C_index, result_list$C_index),
              KM_stats = bind_rows(total_results$KM_stats, result_list$KM_stats)
            )
            }
        }
      }
    }
    }
  }
}

# 保存结果到Excel
# 创建保存结果的文件夹
if (!dir.exists("KM curve/Summary/")) dir.create("KM curve/Summary/")

# 创建对比表格
total_results$C_index %>%
  group_by(Variable, Stratification) %>%
  summarise(
    C_index_range = paste(round(min(C_index),3), round(max(C_index),3), sep = "-"),
    .groups = "drop"
  ) %>%
  mutate(DataSource = rep(c("NHANES", "CHARLS"), each = n()/2))

saveRDS(total_results,"KM curve/total_results.RDS")

write.xlsx(total_results[["C_index"]],"KM curve/Summary/C_index_comparison.xlsx")
write.xlsx(total_results[["KM_stats"]],"KM curve/Summary/KM_stats.xlsx")

