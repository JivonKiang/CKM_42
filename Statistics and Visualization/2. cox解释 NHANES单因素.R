####  cox  可视化 --------------------------------------------------------------
rm(list = ls())
library(openxlsx)
library(dplyr)

NHANES <- readRDS("NHANES/Result/data.RDS")
CHARLS <- readRDS("CHARLS/Result/data.RDS")

variable <- read.xlsx("upset/upset_data.xlsx")
# 过滤 variable$sum 大于等于 5 的行
filtered_variable <- variable %>%
  filter(sum >= 5) %>% na.omit()

colnames(NHANES)
colnames(CHARLS)

other <- c("Time","Status","Age","Sex","Marital_status","Education_level","Smoking",
           "Diabetes","Cancer","Stroke","Hypertension")

# 生成需要保留的变量列表
selected_vars <- unique(c(other,filtered_variable$Variable))

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

data <- whole %>% filter(DataSource == "NHANES")
cox <- coxph(Surv(Time, Status) ~ ., data=data[,-30])

#reg <- autoReg(cox, 
#               uni = T,    # 执行单变量分析
#               final = F,  # 显示最终模型
#               threshold = 0.05)%>% myft() # 阈值
#table2pptx(reg,"cox analysis/cox result.pptx")  #导出到ppt，可编辑数据
#table2docx(reg,"cox analysis/cox result.docx")  #导出到docx，可编辑数据 

reg <- autoReg(cox, 
               uni = T,    # 执行单变量分析
               final = F,  # 显示最终模型
               threshold = 0.05) # 阈值
saveRDS(reg,"cox analysis/cox result.RDS")
write.xlsx(reg,"cox analysis/cox result.xlsx")


reg
str(reg)


# 创建保存结果的文件夹
if(!dir.exists("cox analysis")) dir.create("cox analysis")

library(forestplot)
library(stringr)

summary(as.factor(reg$`HR (univariable)`))
summary(as.factor(reg$`HR (multivariable)`))

reg <- as.data.frame(reg)
str(reg)

library(forestplot)

# 提取左侧三列（name, desc, stats）
left_columns <- cbind(reg$name, reg$desc, reg$stats)
# 处理分类变量层级（例如 Marital_status 的子项）
left_columns <- ifelse(left_columns == "", "    ", left_columns)  # 用缩进表示子变量

# 提取单因素和多因素 HR 及 P 值
extract_hr <- function(hr_col) {
  hr <- as.numeric(gsub("\\(.*", "", hr_col))
  ci_low <- as.numeric(gsub(".*\\((.*)-.*", "\\1", hr_col))
  ci_high <- as.numeric(gsub(".*-(.*),.*", "\\1", hr_col))
  p_value <- hr_col
  list(hr = hr, ci_low = ci_low, ci_high = ci_high, p = p_value)
}

univariable <- extract_hr(reg$`HR (univariable)`)
multivariable <- extract_hr(reg$`HR (multivariable)`)

# 步骤1：将列表转换为数据框（关键修复）
univariable_df <- as.data.frame(univariable) %>% 
  mutate(
    p_value = case_when(
      # 处理空字符串
      p == "" ~ NA_real_,
      # 匹配 p<.001 格式
      grepl("p\\s*<\\s*\\.\\d+", p) ~ 
        as.numeric(sub(".*p\\s*<\\s*\\.(\\d+).*", "0.\\1", p)),
      # 匹配 p=.065 格式
      grepl("p\\s*=\\s*\\.\\d+", p) ~ 
        as.numeric(sub(".*p\\s*=\\s*\\.(\\d+).*", "0.\\1", p)),
      TRUE ~ NA_real_
    )
  )

# 步骤3：重构标签生成逻辑（修复作用域问题）
risk_label <- univariable_df %>%
  mutate(
    Risk_Label = case_when(
      is.na(hr) | is.na(ci_low) | is.na(ci_high) ~ "Reference",  # 处理NA值[4](@ref)
      p_value >= 0.05 ~ "Non-sig",
      hr > 1 & ci_low > 1 ~ "Risk",
      hr < 1 & ci_high < 1 ~ "Protective",
      TRUE ~ "Non-sig"
    )
  ) %>% 
  pull(Risk_Label)

# 构建右侧列（HR 和 P 值）
right_columns <- cbind(
  sprintf("%.2f [%.2f–%.2f]", univariable$hr, univariable$ci_low, univariable$ci_high),
  risk_label,
  univariable_df$p_value
)

# 合并所有列
labeltext <- cbind(left_columns, right_columns)

header <- c("Variable", "Description", "Statistics", "HR (95% CI)",
            "Effect","P Value")

# 生成带列名的标签矩阵
labeltext <- rbind(header, labeltext)


# 绘制双列森林图
fp <- forestplot(
  labeltext = labeltext,
  mean = c(NA, univariable$hr),  # 首行无HR数据
  lower = c(NA, univariable$ci_low),
  upper = c(NA, univariable$ci_high),
  is.summary = c(TRUE, grepl("^\\S", reg$name)),  # 标题行+主变量加粗
  xlog = F,
  #col = fpColors(box = ifelse(risk_label == "Risk", "#B2182B", "#2166AC")),
  txt_gp = fpTxtGp(label = list(
    gpar(fontface = "bold", col = "black"),  # 标题行样式
    gpar(fontface = "plain")                  # 数据行样式
  ),
  cex = 0.8, xlab = gpar(cex = 1.2)),
  title = "Univariable Cox Regression Analysis",
  zero = 1,
  boxsize = 0.2,
  graph.pos = 4
)

# 生成森林图（网页8、网页10）
pdf(file.path("cox analysis", "forestplot.pdf"), width = 18, height = 10)
print(fp)
dev.off()

# 保存JPG（位图，适合快速查看）
jpeg(file.path("cox analysis", "forestplot.jpg"), width = 5000, height = 4000, res = 300)
print(fp)
dev.off()


header2 <- c("HR", "ci_low", "ci_high","P Value")

# 生成带列名的标签矩阵
labeltext2 <- rbind(header2, as.data.frame(univariable))

result <- cbind(as.data.frame(labeltext),labeltext2)

colnames(result) <- result[1,]

result <- result[-1,]

write.xlsx(result,"cox analysis/final result.xlsx")