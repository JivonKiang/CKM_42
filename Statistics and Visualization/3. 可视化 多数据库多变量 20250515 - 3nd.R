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

summary(whole)

library(autoReg)
library(rrtable)
library(survival)
library(ggplot2)
library(rms)
library(pROC) # 新增用于计算AUC

data <- whole
str(data)

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

###   RCS  -----
# 创建保存图形的目录
dir.create("RCS", showWarnings = FALSE)

# 定义分析参数
data_sources <- c("CHARLS", "NHANES")
variables <- c("AIP", 
               "TyG", 
               #"eGFR", 
               "eGDR_BMI")
stage_pairs <- list(
  c(0, 1), c(1, 2), c(2, 3), 
  c(3, "4a"), c("4a", "4b")
)

# 初始化结果数据框
stat_results <- data.frame()

# 主循环
for (data_source in data_sources) {
  # 子集数据
  current_data <- data %>% filter(DataSource == data_source)
  
  for (var in variables) {
    for (i in seq_along(stage_pairs)) {
      # 获取当前阶段比较组
      stages <- stage_pairs[[i]]
      stage_label <- paste(stages, collapse = "_vs_")
      
      # 筛选数据并清理因子水平
      subset_data <- current_data %>%
        filter(CKM_stage_ab %in% stages) %>%
        mutate(CKM_stage_ab = droplevels(CKM_stage_ab))
      
      subset_data <- subset_data %>%
        mutate(CKM_stage_ab = factor(CKM_stage_ab, levels = stages))  # 强制指定有效水平
      
      # 有效性检查
      if (nrow(subset_data) < 10 || nlevels(subset_data$CKM_stage_ab) != 2) {
        message(paste("跳过:", data_source, var, stage_label))
        next
      }
      
      # 设置数据分布
      dd <- datadist(subset_data)
      options(datadist = "dd")
      
      # 自动选择最佳节点数
      best_k <- NULL
      best_aic <- Inf
      for (k in 3:7) {
        formula <- as.formula(paste("CKM_stage_ab ~ rcs(", var, ",", k, ")"))
        fit <- tryCatch(
          lrm(formula, data = subset_data, x = TRUE, y = TRUE),
          error = function(e) NULL
        )
        if (!is.null(fit)) {
          current_aic <- AIC(fit)
          if (current_aic < best_aic) {
            best_k <- k
            best_aic <- current_aic
          }
        }
      }
      
      # 模型拟合
      if (is.null(best_k)) next
      final_formula <- as.formula(paste("CKM_stage_ab ~ rcs(", var, ",", best_k, ")"))
      
      # 增强模型拟合设置（增加最大迭代）
      final_fit <- tryCatch(
        lrm(final_formula, data = subset_data, x=TRUE, y=TRUE, maxit=200),
        error = function(e) {
          message(paste("模型失败:", data_source, var, stage_label, "错误:", e$message))
          return(NULL)
        }
      )
      
      # 检查模型有效性
      if (is.null(final_fit) || final_fit$fail) {
        message(paste("模型无效:", data_source, var, stage_label))
        next
      }
      
      # 安全计算AIC
      safe_AIC <- function(fit) {
        if (inherits(fit, "lrm")) {
          return(fit$stats["Model L.R."] - 2*fit$stats["d.f."])
        } else {
          return(NA)
        }
      }
      
      # 计算交叉点
      # 修改后的预测部分代码
      pred <- eval(substitute(
        Predict(final_fit, VAR, fun = exp, ref.zero = TRUE),
        list(VAR = as.name(var))
      ))
      
      # 计算性能指标（新增AUC等）
      pred_prob <- plogis(predict(final_fit))
      roc_obj <- roc(response = subset_data$CKM_stage_ab, 
                     predictor = pred_prob)
      
      #str(roc_obj)
      
      # 获取最佳阈值下的性能指标
      coords_obj <- pROC::coords(
        roc_obj, 
        x = "best", 
        best.method = "youden",
        ret = c("sensitivity", "specificity")
      )
      
      metrics <- sprintf(
        "AUC: %.2f | Sens: %.1f%% | Spec: %.1f%%",
        pROC::auc(roc_obj),
        coords_obj$sensitivity * 100,
        coords_obj$specificity * 100
      )
      
      # 调整交叉点检测逻辑（保持变量名一致性）
      crossing_points <- numeric(0)
      for (j in 2:nrow(pred)) {
        if ((pred$yhat[j] < 1 & pred$yhat[j-1] >= 1) | 
            (pred$yhat[j] > 1 & pred$yhat[j-1] <= 1)) {
          x1 <- pred[[var]][j-1]  # 直接使用var作为列名
          x2 <- pred[[var]][j]
          y1 <- pred$yhat[j-1]
          y2 <- pred$yhat[j]
          cross_x <- x1 + (1 - y1) * (x2 - x1) / (y2 - y1)
          crossing_points <- c(crossing_points, cross_x)
        }
      }
      
      # 生成统计标注
      anova_res <- anova(final_fit)
      
      # 安全提取总体P值
      p_overall <- "N/A"
      var_row <- grep(paste0("^", var), rownames(anova_res), value = TRUE)
      if (length(var_row) > 0) {
        p_value <- anova_res[var_row[1], "P"]
        p_overall <- ifelse(p_value < 0.001, "<0.001", 
                            format.pval(p_value, digits = 3))
      }
      
      # 安全提取非线性P值
      p_nonlinear <- "N/A"
      nonlinear_row <- grep("Nonlinear", rownames(anova_res), value = TRUE)
      if (length(nonlinear_row) > 0) {
        p_value <- anova_res[nonlinear_row[1], "P"]
        p_nonlinear <- ifelse(p_value < 0.001, "<0.001",
                              format.pval(p_value, digits = 3))
      }
      
      # 收集统计信息
      # 安全计算AUC
      auc_value <- tryCatch(
        as.numeric(pROC::auc(roc_obj)),
        error = function(e) NA_real_
      )
      
      row_data <- data.frame(
        DataSource = data_source,
        Variable = var,
        Stage_Comparison = stage_label,
        AUC = round(auc_value, 3),
        Sensitivity = round(coords_obj$sensitivity, 3),
        Specificity = round(coords_obj$specificity, 3),
        Overall_P = p_overall,
        Nonlinear_P = p_nonlinear,
        Crossing_Points = if (length(crossing_points) > 0) 
          paste(round(crossing_points, 2), collapse = ", ") else NA,
        Best_Knots = best_k,
        AIC = round(best_aic, 1),
        Observations = nrow(subset_data)
      )
      
      # 添加到结果集
      stat_results <- rbind(stat_results, row_data)
      
      # 生成图形
      p <- ggplot(pred, aes(x = .data[[var]], y = yhat)) +  # 使用.data代词
        geom_hline(yintercept = 1, linetype = "dashed", color = "grey50") +
        geom_line(color = "#D53E4F", linewidth = 1.2) +
        geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "#D53E4F") +
        geom_histogram(
          data = subset_data,
          aes(x = .data[[var]], y = after_stat(density) * max(pred$yhat)/0.2),
          bins = 10, fill = "#3288BD", alpha = 0.3
        ) +
        scale_y_continuous(
          name = "Odds Ratio (95% CI)",
          sec.axis = sec_axis(
            ~ . * 0.2 / max(pred$yhat),
            name = "Density",
            labels = scales::percent
          )
        ) +
        labs(
          title = paste("Data:", data_source, "| Variable:", var),
          subtitle = paste("Stage Comparison:", stage_label),
          x = var
        ) +
        theme_bw(14) +
        theme(plot.title = element_text(face = "bold"))+
        labs(caption = sprintf("Overall P: %s | Nonlinear P: %s | %s", 
                               p_overall, p_nonlinear, metrics)) +
        theme(plot.caption = element_text(color = "blue", size = 10))
      
      # 添加统计标注
      if (length(crossing_points) > 0) {
        p <- p + 
          geom_vline(xintercept = crossing_points, linetype = "dashed", color = "grey50") +
          annotate("text", x = max(pred[[var]])*0.9, y = max(pred$upper)*0.95,
                   label = paste("Intersection at", 
                                 paste(round(crossing_points, 2), collapse = ", ")),
                   hjust = 1, size = 4)
      }
      
      # 保存图形
      filename <- paste0("RCS/", data_source, "_", var, "_", stage_label, ".png")
      ggsave(filename, plot = p, width = 6, height = 4, dpi = 300)
    }
  }
}

# 保存结果到Excel
openxlsx::write.xlsx(
  stat_results,
  "RCS/RCS_Statistical_Results.xlsx",
  colNames = TRUE,
  rowNames = FALSE,
  keepNA = TRUE
)
