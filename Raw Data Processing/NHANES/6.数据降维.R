##读取数据
rm(list=ls())
library(openxlsx)
#载入数据
# 读取数据
data1 <- read.xlsx("Result/插补后去除na的数据.xlsx")
data1$Status <- as.factor(data1$Status)
summary(data1$Status)
#将该文件夹中的列名替换为标准名称的分页改为上面的标准名称
data1 <- data1[,-35]#去掉常数项
# 提取所需的列
index <- readRDS("Result/index compute result.RDS")
index <- index[,-(1:16)]###去掉index中前面的用于计算新指标的生理生化指标
data <- cbind(data1,index)
data <- data[,-(5:19)]###去掉分类变量
data_colname <- as.data.frame(colnames(data))
colnames(data)[1:98323] <- paste0("index_", 1:98323)###改一下起始点
index_colname <- as.data.frame(colnames(data))
colnames <- cbind(data_colname,index_colname)
write.csv(colnames,"Result/colnames.csv")
# 保存结果为 RDS 文件
saveRDS(data, "Result/cleaner data.RDS")

rm(list=ls())
data <- readRDS("Result/cleaner data.RDS")
#data <- na.omit(data)
###--- 使用Boruta进行特征选择 ---###
library(Boruta)
library(survival)
colnames(data)[1:2] <- c("Status","Time")
#data$Status <- as.factor(data$Status)###将status设置为因子型
# 检查Time列的类型
str(data$Time)

# 如果Time列是因子或字符类型，将其转换为数值型
if(is.factor(data$Time) || is.character(data$Time)) {
  data$Time <- as.numeric(as.character(data$Time))
}

# 检查是否有NA值，并处理它们
if(any(is.na(data$Time))) {
  # 你可以选择删除含有NA的行
  data <- data[!is.na(data$Time), ]
  # 或者用其他方式处理NA值，例如用中位数或均值填充
  # data$Time[is.na(data$Time)] <- median(data$Time, na.rm = TRUE)
}

# 检查Status列的类型
str(data$Status)

# 如果Status列不是数值型，将其转换为数值型
if(!is.numeric(data$Status)) {
  # 首先将Status列转换为字符型，以避免警告信息
  data$Status <- as.character(data$Status)
  
  # 然后转换为数值型，这里使用suppressWarnings来抑制可能的警告信息
  data$Status <- suppressWarnings(as.numeric(data$Status))
}

# 检查Status列是否包含NA值，并处理它们
if(any(is.na(data$Status))) {
  # 你可以选择删除含有NA的行
  data <- na.omit(data)
}

# 检查Status和Time列是否有缺失值
sum(is.na(data$Status))
sum(is.na(data$Time))

# 删除含有缺失值的行
data_clean <- na.omit(data[, c("Status", "Time")])

# 确保Status和Time列在数据框中存在
if("Status" %in% names(data) && "Time" %in% names(data)) {
  # 删除含有缺失值的行
  data_clean <- na.omit(data[, c("Status", "Time", colnames(data)[colnames(data) != "Status" & colnames(data) != "Time"])])
  
  # 检查Status列是否包含非0/1的值
  if(any(data_clean$Status != 0 & data_clean$Status != 1)) {
    # 将非0/1的值转换为NA
    data_clean$Status[data_clean$Status != 0 & data_clean$Status != 1] <- NA
    # 再次删除含有缺失值的行
    data_clean <- na.omit(data_clean)
  }
  
  # 确保Time列是数值型
  if(!is.numeric(data_clean$Time)) {
    data_clean$Time <- as.numeric(data_clean$Time)
  }
  
  # 检查Time列是否包含NA值，并处理它们
  if(any(is.na(data_clean$Time))) {
    data_clean <- na.omit(data_clean)
  }
  
  # 创建Surv对象
  surv_obj <- Surv(data_clean$Time, data_clean$Status)
  
  # 准备特征矩阵
  features <- data_clean[, -which(names(data_clean) %in% c("Status", "Time"))]
  
  # 运行Boruta算法
  boruta_result <- Boruta(x=features, y=surv_obj, doTrace = 2)
} else {
  stop("Status or Time column not found in the data.")
}

saveRDS(boruta_result,"Result/boruta.RDS")
#boruta_result <- readRDS("Result/boruta.RDS")
# 查看Boruta的结果
print(boruta_result)
# 获取确认的重要特征
confirmed_vars <- getSelectedAttributes(boruta_result, withTentative = T)
print(confirmed_vars)
write.csv(confirmed_vars,"Result/confirmed_vars_index.csv")
# 绘制Boruta的重要性图

# 设置PNG文件的输出路径和名称
#png_file_path <- "Result/boruta.png"
# 开启PNG设备，准备输出图像
#png(png_file_path, width = 300, height = 300#, units = "px", res = 120
#)
# 绘制图像，这里以基本绘图为例
#plot(boruta_result, xlab = "", xaxt = "n")
# 关闭PNG设备，保存图像
#dev.off()

colnames <- read.csv("Result/colnames.csv")
confirmed_vars <- as.data.frame(confirmed_vars)
index <- read.csv("Result/operation_mapping.csv")

colnames(confirmed_vars) <- c("names")
colnames(colnames)[3] <- c("names")
colnames(index)[1] <- c("names")

library(dplyr)
confirmed <- left_join(confirmed_vars,colnames,by="names")
confirmed <- left_join(confirmed,index,by="names")
write.csv(confirmed,"Result/confirmed.csv")

####将index替换为临床指标####
rm(list=ls())
data <- read.csv("Result/confirmed.csv")
data$转换成临床指标 <- data$operation_expression

# 创建临床指标和var的对应关系
var_mapping <- data.frame(
  var = paste0("var_", sprintf("%02d", 1:16)), # 格式化为两位数
  clinical_indicator = c("Standing.Height.(cm)",
                         "Weight.(kg)",
                         "BMI.(kg/m2)",
                         "Waist.Circumference.(cm)",
                         "WHtR",
                         "HDL-cholesterol.(mg/dL)",
                         "LDL-cholesterol.(mg/dL)",
                         "Triglycerides.(mg/dL)",
                         "Glucose.(mg/dL)",
                         "Uric.acid.(mg/dL)",
                         "Systolic blood pressure (mmHg)",
                         "Diastolic blood pressure  (mmHg)",
                         "Glycated haemoglobin (%)",
                         "Cholesterol (mg/dL)",
                         "C-reactive protein (mg/dL)",
                         "constant.term")
)

# 替换var_1到var_9为var_01到var_09
for (i in 1:9) {
  search_string <- paste0("var_", i)                 # 原始var字符串
  replace_string <- paste0("var_", sprintf("%02d", i)) # 格式化为var_01到var_09的var字符串
  data$转换成临床指标 <- gsub(search_string, replace_string, data$转换成临床指标)
}
# 去掉var_010和var_011最前面的0
data$转换成临床指标 <- gsub("var_010", "var_10", data$转换成临床指标)
data$转换成临床指标 <- gsub("var_011", "var_11", data$转换成临床指标)
data$转换成临床指标 <- gsub("var_012", "var_12", data$转换成临床指标)
data$转换成临床指标 <- gsub("var_013", "var_13", data$转换成临床指标)
data$转换成临床指标 <- gsub("var_014", "var_14", data$转换成临床指标)
data$转换成临床指标 <- gsub("var_015", "var_15", data$转换成临床指标)
data$转换成临床指标 <- gsub("var_016", "var_16", data$转换成临床指标)
# 替换var_1到var_11为对应的字符
for (i in 1:nrow(var_mapping)) {
  data$转换成临床指标 <- gsub(var_mapping$var[i], var_mapping$clinical_indicator[i], data$转换成临床指标)
}
colnames(data)[1:6] <- c("No.","index number","number","index number","operation","clinical information")
write.csv(data,"Result/confirmed_替换后.csv")

