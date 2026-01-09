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

Variable <- c("AIP","TyG","eGFR","eGDR_BMI")###从cox汇总里粘贴出来

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



###   先进行RCS绘图   ----------------------------------------------------------
summary(data)
str(data)

library(tidyverse)
# 按数据源分组为列表
data_list <- data %>% split(.$DataSource)
# 筛选核心变量并转为长格式（便于批量处理）
data_long <- map(data_list, ~ .x %>%
                   select(CKM_stage_ab, AIP, TyG, eGFR, eGDR_BMI) %>%
                   pivot_longer(-CKM_stage_ab, names_to = "Variable", values_to = "Value"))

###   密度图   -----
library(ggplot2)
plot_density <- function(df, source_name) {
  ggplot(df, aes(x = Value, fill = CKM_stage_ab)) +
    geom_density(alpha = 0.5) +
    facet_wrap(~Variable, scales = "free") +
    labs(title = paste("Probability Density by CKM Stage (DataSource:", source_name, ")"),
         x = "Value", y = "Density") +
    theme_bw()
}
# 生成所有数据源的密度图
imap(data_long, ~ plot_density(.x, .y) %>% print())

dev.off()

write.xlsx(NHANES,"NHANES For visual.xlsx")
write.xlsx(CHARLS,"CHARLS For visual.xlsx")

###   RCS  -----
summary(data)

# 加载必要包
library(pROC)
library(rms)
library(ggplot2)
library(gridExtra)

# 筛选CHARLS数据源并创建子集
charls_data <- subset(data, DataSource == "CHARLS" & CKM_stage_ab %in% c(0,1))

# 检查变量类型（确保CKM_stage_ab为二分类因子）
charls_data$CKM_stage_ab <- as.factor(as.character(charls_data$CKM_stage_ab))

# 使用rms包进行数据打包（支持限制性立方样条）
dd <- datadist(charls_data)
options(datadist = "dd")

# 构建逻辑回归模型（含AIP的立方样条）
# 自动选择最佳节点数量（基于AIC）
best_k <- NULL
best_aic <- Inf

# 尝试不同节点数量（例如从3到7）
for (k in 3:7) {
  fit <- lrm(CKM_stage_ab ~ rcs(AIP, k), data = charls_data, x = TRUE, y = TRUE)
  aic <- AIC(fit)
  
  if (aic < best_aic) {
    best_aic <- aic
    best_k <- k
  }
}

最佳节点数量 <- paste0("最佳节点数量:", best_k)
对应的AIC值 <- paste0("对应的AIC值:", best_aic)

# 使用最佳节点数量拟合最终模型
fit <- lrm(CKM_stage_ab ~ rcs(AIP, best_k), data = charls_data, x = TRUE, y = TRUE)

# 查看模型结果
summary(fit)

# 计算OR值跨越1时的x轴值
# 获取预测概率
pred <- Predict(fit, AIP, fun=exp, ref.zero=TRUE)

# 提取 AIP 值和对应的 OR 值
aip_values <- pred$AIP
or_values <- pred$yhat

# 找到 OR 值跨越 1 的 x 轴值
crossing_points <- c()  # 初始化为空向量，用于存储所有交叉点

# 遍历 OR 值，找到所有跨越 1 的点
for (i in 2:length(or_values)) {
  if ((or_values[i] < 1 & or_values[i - 1] > 1) | (or_values[i] > 1 & or_values[i - 1] < 1)) {
    # 线性插值找到交叉点
    crossing_x <- aip_values[i - 1] + (1 - or_values[i - 1]) * (aip_values[i] - aip_values[i - 1]) / (or_values[i] - or_values[i - 1])
    crossing_points <- c(crossing_points, crossing_x)
  }
}

# 输出所有交叉点
if (length(crossing_points) > 0) {
  cat("OR值跨越1的x轴值:", crossing_points, "\n")
} else {
  cat("没有找到OR值跨越1的点。\n")
}

Intersection <- paste0("Intersection point:", crossing_points)

# 1. 计算P值
anova_fit <- anova(fit)
p_overall <- format.pval(anova_fit["AIP", "P"], digits=3)        # 整体效应P值[7](@ref)
p_nonlinear <- format.pval(anova_fit[" Nonlinear", "P"], digits=3) # 非线性效应P值[7](@ref)

# 2. 计算AUC及置信区间
roc_obj <- roc(charls_data$CKM_stage_ab, predict(fit, type="fitted"))
auc_val <- round(auc(roc_obj), 3)
ci_val <- round(ci(roc_obj), 3)

# 3. 计算最佳截断点的灵敏度/特异度
best_coords <- coords(roc_obj, "best", ret=c("threshold","sensitivity","specificity"))
sens <- round(best_coords$sensitivity, 3)
spec <- round(best_coords$specificity, 3)

# 生成预测数据
pred <- Predict(fit, AIP, fun=exp, ref.zero=TRUE)

# 主图绘制
# 主图绘制（包含直方图叠加）
main_plot <- ggplot(pred) +
  # 直方图层（使用右轴）
  geom_histogram(
    data = charls_data, 
    aes(x = AIP, y = after_stat(density) * max(pred$yhat)/0.2),  # 缩放直方图高度
    fill = "#3288BD", 
    alpha = 0.3, 
    bins = 10
  ) +
  # RCS曲线层（左轴）
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray40") +
  geom_line(aes(x = AIP, y = yhat), color = "#D53E4F", linewidth = 1.2) +
  geom_ribbon(aes(x = AIP, ymin = lower, ymax = upper), fill = "#D53E4F", alpha = 0.2) +
  # 双坐标轴设置
  scale_y_continuous(
    name = "Odds Ratio (95% CI)", 
    sec.axis = sec_axis(
      ~ . * 0.2 / max(pred$yhat),  # 反向缩放右轴
      name = "Density",
      labels = scales::percent_format(accuracy = 1)
    )
  ) +
  labs(x = "AIP") +
  theme_bw() +
  theme(axis.line.y.right = element_line(color = "#3288BD"))  # 右轴颜色匹配直方图

# 修改P值计算逻辑（处理P=0的情况）
p_overall <- ifelse(anova_fit["AIP", "P"] < 0.001, "<0.001", 
                    format.pval(anova_fit["AIP", "P"], digits=3))
p_nonlinear <- ifelse(anova_fit[" Nonlinear", "P"] < 0.001, "<0.001", 
                      format.pval(anova_fit[" Nonlinear", "P"], digits=3))

# 动态计算安全位置（避免使用分位数）
x_pos <- min(pred$AIP)+1
y_base <- max(pred$upper) * 0.2  # 基于置信区间最大值定位

main_plot <- main_plot +
  annotate("text", x = x_pos, y = y_base,
           label = sprintf("P-overall %s\nP-nonlinear %s", p_overall, p_nonlinear),
           hjust = 0, size = 4, color = "black") +
  annotate("text", x = x_pos, y = y_base + 0.15, 
           label = sprintf("AUC = %s (95%% CI: %s-%s)", auc_val, ci_val[1], ci_val[3]),
           hjust = 0, size = 4) +
  annotate("text", x = x_pos, y = y_base + 0.25,
           label = sprintf("Sensitivity = %s ;Specificity = %s", sens, spec),
           hjust = 0, size = 4) 

# 将交叉点值转换为字符串，并用换行符连接
intersection_label <- paste("Intersection: ", 
                            paste(round(crossing_points, 2), collapse = ", "), 
                            sep = "")

# 添加注释
main_plot <- main_plot +
  annotate("text", x = x_pos, y = y_base + 0.35,
           label = intersection_label,
           hjust = 0, size = 4)


# 添加 OR 值跨越 1 的 x 轴值的注释和垂直虚线
for (x in crossing_points) {
  main_plot <- main_plot +
    geom_vline(xintercept = x, linetype = "dashed", color = "gray40")
}

# 输出图形
print(main_plot)

