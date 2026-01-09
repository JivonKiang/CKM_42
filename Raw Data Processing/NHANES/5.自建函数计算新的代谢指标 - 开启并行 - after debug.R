####自建函数计算新的指标####
library(purrr)
library(dplyr)
library(tidyr)
library(openxlsx)
rm(list=ls())
# 读取数据
data <- readRDS("Result/CKM Stage.RDS")###获得的ckm stage的重新打了标签4a，4b

library(dplyr)
data <- data %>% select(-CKM_stage)

data$WHtR <- data$WC/data$Height
str(data)

cox <- readRDS("Result/cox result.RDS")
str(cox)

# 加载必要的包
library(dplyr)
library(tidyr)

## 步骤1：从cox中提取分类变量信息并解析HR和P值 ----
# 提取分类变量信息（假设分类变量在'desc'列中包含分组信息）
class_vars <- cox[!grepl("Mean ± SD", cox$desc) #& cox$`HR (univariable)` != ""
                  ,]

# 示例：提取变量名和分组（如Sex变量中的Sex1_Male和Sex2_Female）
# 假设变量名通过下划线分割（如Sex1_Male → 变量名为Sex，分组为Male）
class_vars$var_name <- class_vars$name  # 提取变量名
class_vars$group <- class_vars$desc       # 提取分组标签

# 假设你的数据框名为class_vars
# 先将空字符串替换为NA
class_vars <- class_vars %>%
  mutate(var_name = na_if(var_name, ""))  # 将空字符串替换为NA

# 使用fill()填充NA为上一个非NA值
class_vars <- class_vars %>%
  fill(var_name, .direction = "down")

# 提取单因素HR和P值
class_vars$hr <- as.numeric(sapply(strsplit(class_vars$`HR (univariable)`, " "), `[`, 1))
class_vars$hr[is.na(class_vars$hr)] <- 1


# 提取 p 值
class_vars$p <- sapply(class_vars$`HR (univariable)`, function(x) {
  match <- sub(".*p[=<]([0-9.]+).*", "\\1", x)
  if (match == "") {
    return(0.001)  # 如果没有匹配到 p 值，返回 NA
  } else {
    return(as.numeric(match))
  }
})


str(class_vars)

# 步骤2：定义赋值函数 ----
names(data) <- gsub(" ", "_", names(data))
# 遍历 class_vars 中的每一行
for (i in 1:nrow(class_vars)) {
  # 当前行的变量名称和亚组
  var_name <- class_vars$var_name[i]
  group <- class_vars$group[i]
  p_value <- class_vars$p[i]
  hr_value <- class_vars$hr[i]
  
  # 检查变量名称是否在 data 中存在
  if (var_name %in% names(data)) {
    # 创建新列名称
    new_col_name <- paste0(var_name, "_hr")
    
    # 如果新列不存在，先创建新列并初始化为 1
    if (!(new_col_name %in% names(data))) {
      data[[new_col_name]] <- rep(1, nrow(data))
    }
    
    # 根据 p 值进行替换
    if (p_value < 0.05) {
      # 如果 p < 0.05，替换为对应的 hr 值
      data[data[[var_name]] == group, new_col_name] <- hr_value
    } else {
      # 如果 p ≥ 0.05，保持为 1（新列初始化时已设置为 1）
      next
    }
  } else {
    cat("Warning: Variable", var_name, "not found in data.\n")
  }
}

# 查看修改后的 data
head(data)

####数据准备成熟了####
# 替换 CKM_stage_ab 列中的值
#data$CKM_stage_ab <- sub("4a", "4", data$CKM_stage_ab)
#data$CKM_stage_ab <- sub("4b", "5", data$CKM_stage_ab)
# 提取所需的列

# 检查每个列是否为数值类型或在保留列表中
numeric_cols <- sapply(data, is.numeric)
all_unique <- sapply(data[, numeric_cols], function(x) length(unique(x)) == 1)
columns_to_keep <- names(numeric_cols)[numeric_cols & !all_unique]

# 仅保留这些列
data <- data[, columns_to_keep]
# 删除指定列
data <- data[, !(colnames(data) %in% c("SEQN", "Status", "Time"))]

# 找到以"Score"结尾的列名
columns_to_remove <- grep("Score$", names(data), value = TRUE)

# 去除这些列
data <- data[, setdiff(names(data), columns_to_remove)]

# 假设 data 是你的数据框
data <- data[, sapply(data, function(x) length(unique(x)) > 1)]

# 查看结果
head(data)

colnames <- colnames(data)
# 假设你的数据框名为data
# 保存原始列名
original_colnames <- colnames(data)

# 生成新的列名，格式为Var_1, Var_2, ..., Var_xx
new_colnames <- paste0("Var_", 1:ncol(data))

# 修改数据框的列名
colnames(data) <- new_colnames

# 查看结果
print(data)
print(original_colnames)  # 原始列名


# 定义运算符号和对数运算相关选项
operators <- c("*", "/")
ln_or_identity <- c("log", "log10", "identity")

library(parallel)
library(foreach)
library(doParallel)
# 设置并行后端
cl <- makeCluster(20)  # 使用除了主核之外的所有核心
doParallel::registerDoParallel(cl)

#data <- data[,c(1:10)]
# 生成所有可能的运算函数组合
generate_operations <- function() {
  all_operations <- list()
  
  for (var1_index in 1:ncol(data)) {
    for (var2_index in 1:ncol(data)) {
      for (op1 in operators) {
        for (var3_index in 1:ncol(data)) {
          for (number_choose in 1:2) {
            for (op2 in operators) {
              for (ln_op in ln_or_identity) {  # 新增循环遍历对数运算选项
                operation_str <- ""
                
                # 第一部分：var_1~var_11中的任意一个变量相乘或相除var_1~var_11中的任意一个变量
                part1 <- paste0("Var_", var1_index, op1, "Var_", var2_index)
                
                if (number_choose == 1) {
                  # 第二部分：要么*1
                  part2 <- paste0(part1, " * 1")
                } else {
                  # 第二部分：要么* or / 2
                  part2 <- paste0(part1, op1, " 2")
                }
                
                # 根据选择进行对数运算或保持不变
                if (ln_op == "log") {
                  # 第二部分：进行ln对数运算
                  part3 <- paste0("log(", part2, ")")
                } else if (ln_op == "log10") {
                  # 第二部分：进行log10对数运算
                  part3 <- paste0("log10(", part2, ")")
                } else {
                  # 第二部分：乘1（等同于不进行对数运算）
                  part3 <- paste0("1 * ", part2)
                }
                
                # 第三部分：将前一步的运算结果相乘或相除var_1~var_11中的任意一个变量
                part4 <- paste0(part3, op2, "Var_", var3_index)
                
                operation_str <- part4
                
                all_operations <- c(all_operations, operation_str)
              }
            }
          }
        }
      }
    }
  }
  
  return(all_operations)
}

# 假设已经有了名为data的数据框，且列名为var_1到ncol(data)

# 生成所有运算组合
all_operations <- generate_operations()

# 保存结果为 RDS 文件
saveRDS(all_operations, "Result/all_operations.RDS")

# 对每个运算组合，添加新列到数据框并计算
for (i in 1:length(all_operations)) {
  operation <- all_operations[[i]]
  data[, paste0("index_", sprintf("%03d", i))] <- with(data, eval(parse(text = operation)))
}

# 保存结果为 RDS 文件
saveRDS(data, "Result/index compute result.RDS")

# 创建一个数据框用于存储对应关系
operation_mapping <- data.frame(
  operation_name = paste0("index_", sprintf("%03d", 1:length(all_operations))),
  operation_expression = unlist(all_operations)
)
# 将对应关系数据框导出为csv文件
write.csv(operation_mapping, "Result/operation_mapping.csv", row.names = FALSE)

# 停止并行后端
stopCluster(cl)
