####数据提取1####
library(foreign)#读取xpt
library(tidyverse)#文件整理
library(dplyr)
library(openxlsx)
library(plyr)
library(nhanesA)
library(RNHANES)
rm(list=ls())
final_mortality <- readRDS("original data/mortality/final_mortality.rds")

# 设置数据路径
path <- "original data/DEMO"
# 获取目录中的所有文件名，并返回完整的文件路径
fileNames <- list.files(path, full.names = TRUE)
# 使用lapply读取所有XPT文件
data <- lapply(fileNames, function(x) {
  tryCatch({
    read.xport(x, header = TRUE)
  }, error = function(e) {
    message("Error reading file: ", x)
    NULL  # 如果读取失败，返回NULL
  })
})

###去掉没有SEQN的文件之后重新读取list，然后使用join all合并所有文件进行后续分析
merged <- join_all(data,
                   by = c("SEQN"),
                   type = 'full')
merged <- merged[,c("SEQN",
                    "RIDAGEYR",
                    "RIAGENDR",
                    "DMDMARTL",
                    "DMDEDUC2",
                    "RIDRETH3")]
###去掉没有SEQN的文件之后重新读取list，然后使用join all合并所有文件进行后续分析
merged <- join_all(list(final_mortality,merged),
                   by = c("SEQN"),
                   type = 'full')
merged <- merged[!duplicated(merged$SEQN),]
if(! dir.exists("提取数据")){dir.create("提取数据")}
saveRDS(merged,file = '提取数据/提取数据-1.rds') #保存 rds


####数据提取2####
library(foreign)#读取xpt
library(tidyverse)#文件整理
library(dplyr)
library(openxlsx)
library(plyr)
library(nhanesA)
library(RNHANES)
rm(list=ls())
final_mortality <- readRDS("提取数据/提取数据-1.rds")

# 设置数据路径
path <- "original data/SMQ"
# 获取目录中的所有文件名，并返回完整的文件路径
fileNames <- list.files(path, full.names = TRUE)
# 使用lapply读取所有XPT文件
data <- lapply(fileNames, function(x) {
  tryCatch({
    read.xport(x, header = TRUE)
  }, error = function(e) {
    message("Error reading file: ", x)
    NULL  # 如果读取失败，返回NULL
  })
})

###去掉没有SEQN的文件之后重新读取list，然后使用join all合并所有文件进行后续分析
merged <- join_all(data,
                   by = c("SEQN"),
                   type = 'full')
merged <- merged[,c("SEQN","SMQ020")]
###去掉没有SEQN的文件之后重新读取list，然后使用join all合并所有文件进行后续分析
merged <- join_all(list(final_mortality,merged),
                   by = c("SEQN"),
                   type = 'full')
merged <- merged[!duplicated(merged$SEQN),]
if(! dir.exists("提取数据")){dir.create("提取数据")}
saveRDS(merged,file = '提取数据/提取数据-2.rds') #保存 rds





####数据提取3####
library(foreign)#读取xpt
library(tidyverse)#文件整理
library(dplyr)
library(openxlsx)
library(plyr)
library(nhanesA)
library(RNHANES)
rm(list=ls())
final_mortality <- readRDS("提取数据/提取数据-2.rds")

# 设置数据路径
path <- "original data/ALQ"
# 获取目录中的所有文件名，并返回完整的文件路径
fileNames <- list.files(path, full.names = TRUE)
# 使用lapply读取所有XPT文件
data <- lapply(fileNames, function(x) {
  tryCatch({
    read.xport(x, header = TRUE)
  }, error = function(e) {
    message("Error reading file: ", x)
    NULL  # 如果读取失败，返回NULL
  })
})

###去掉没有SEQN的文件之后重新读取list，然后使用join all合并所有文件进行后续分析
merged <- join_all(data,
                   by = c("SEQN"),
                   type = 'full')
merged <- merged[,c("SEQN","ALQ101")]
###去掉没有SEQN的文件之后重新读取list，然后使用join all合并所有文件进行后续分析
merged <- join_all(list(final_mortality,merged),
                   by = c("SEQN"),
                   type = 'full')
merged <- merged[!duplicated(merged$SEQN),]
if(! dir.exists("提取数据")){dir.create("提取数据")}
saveRDS(merged,file = '提取数据/提取数据-3.rds') #保存 rds






####数据提取4####
library(foreign)#读取xpt
library(tidyverse)#文件整理
library(dplyr)
library(openxlsx)
library(plyr)
library(nhanesA)
library(RNHANES)
rm(list=ls())
final_mortality <- readRDS("提取数据/提取数据-3.rds")

# 设置数据路径
path <- "original data/PAQ"
# 获取目录中的所有文件名，并返回完整的文件路径
fileNames <- list.files(path, full.names = TRUE)
# 使用lapply读取所有XPT文件
data <- lapply(fileNames, function(x) {
  tryCatch({
    read.xport(x, header = TRUE)
  }, error = function(e) {
    message("Error reading file: ", x)
    NULL  # 如果读取失败，返回NULL
  })
})

###去掉没有SEQN的文件之后重新读取list，然后使用join all合并所有文件进行后续分析
merged <- join_all(data,
                   by = c("SEQN"),
                   type = 'full')
merged <- merged[,c("SEQN","PAQ605","PAQ620","PAQ635")]
###去掉没有SEQN的文件之后重新读取list，然后使用join all合并所有文件进行后续分析
merged <- join_all(list(final_mortality,merged),
                   by = c("SEQN"),
                   type = 'full')
merged <- merged[!duplicated(merged$SEQN),]
if(! dir.exists("提取数据")){dir.create("提取数据")}
saveRDS(merged,file = '提取数据/提取数据-4.rds') #保存 rds



####数据提取5####
library(foreign)#读取xpt
library(tidyverse)#文件整理
library(dplyr)
library(openxlsx)
library(plyr)
library(nhanesA)
library(RNHANES)
rm(list=ls())
final_mortality <- readRDS("提取数据/提取数据-4.rds")

# 设置数据路径
path <- "original data/DPQ"
# 获取目录中的所有文件名，并返回完整的文件路径
fileNames <- list.files(path, full.names = TRUE)
# 使用lapply读取所有XPT文件
data <- lapply(fileNames, function(x) {
  tryCatch({
    read.xport(x, header = TRUE)
  }, error = function(e) {
    message("Error reading file: ", x)
    NULL  # 如果读取失败，返回NULL
  })
})

###去掉没有SEQN的文件之后重新读取list，然后使用join all合并所有文件进行后续分析
merged <- join_all(data,
                   by = c("SEQN"),
                   type = 'full')
merged0 <- merged[,c("SEQN","DPQ030")]
merged1 <- join_all(list(final_mortality,merged0),
                    by = c("SEQN"),
                    type = 'full')
merged1 <- merged1[!duplicated(merged1$SEQN),]
if(! dir.exists("提取数据")){dir.create("提取数据")}
saveRDS(merged1,file = '提取数据/提取数据-5.rds') #保存 rds




####数据提取6####
library(foreign)#读取xpt
library(tidyverse)#文件整理
library(dplyr)
library(openxlsx)
library(plyr)
library(nhanesA)
library(RNHANES)
rm(list=ls())
final_mortality <- readRDS("提取数据/提取数据-5.rds")

# 设置数据路径
path <- "original data/DIQ"
# 获取目录中的所有文件名，并返回完整的文件路径
fileNames <- list.files(path, full.names = TRUE)
# 使用lapply读取所有XPT文件
data <- lapply(fileNames, function(x) {
  tryCatch({
    read.xport(x, header = TRUE)
  }, error = function(e) {
    message("Error reading file: ", x)
    NULL  # 如果读取失败，返回NULL
  })
})

###去掉没有SEQN的文件之后重新读取list，然后使用join all合并所有文件进行后续分析
merged <- join_all(data,
                   by = c("SEQN"),
                   type = 'full')
merged0 <- merged[,c("SEQN","DIQ010")]
merged1 <- join_all(list(final_mortality,merged0),
                    by = c("SEQN"),
                    type = 'full')
merged1 <- merged1[!duplicated(merged1$SEQN),]
if(! dir.exists("提取数据")){dir.create("提取数据")}
saveRDS(merged1,file = '提取数据/提取数据-6.rds') #保存 rds




####数据提取7####
library(foreign)#读取xpt
library(tidyverse)#文件整理
library(dplyr)
library(openxlsx)
library(plyr)
library(nhanesA)
library(RNHANES)
rm(list=ls())
final_mortality <- readRDS("提取数据/提取数据-6.rds")

# 设置数据路径
path <- "original data/MCQ"
# 获取目录中的所有文件名，并返回完整的文件路径
fileNames <- list.files(path, full.names = TRUE)
# 使用lapply读取所有XPT文件
data <- lapply(fileNames, function(x) {
  tryCatch({
    read.xport(x, header = TRUE)
  }, error = function(e) {
    message("Error reading file: ", x)
    NULL  # 如果读取失败，返回NULL
  })
})

###去掉没有SEQN的文件之后重新读取list，然后使用join all合并所有文件进行后续分析
merged <- join_all(data,
                   by = c("SEQN"),
                   type = 'full')
merged0 <- merged[,c("SEQN","MCQ220","MCQ160F","MCQ160C","MCQ160B","MCQ180E","MCQ160D")]
merged1 <- join_all(list(final_mortality,merged0),
                    by = c("SEQN"),
                    type = 'full')
merged1 <- merged1[!duplicated(merged1$SEQN),]
if(! dir.exists("提取数据")){dir.create("提取数据")}
saveRDS(merged1,file = '提取数据/提取数据-7.rds') #保存 rds





####数据提取8####
library(foreign)#读取xpt
library(tidyverse)#文件整理
library(dplyr)
library(openxlsx)
library(plyr)
library(nhanesA)
library(RNHANES)
rm(list=ls())
final_mortality <- readRDS("提取数据/提取数据-7.rds")

# 设置数据路径
path <- "original data/BPQ"
# 获取目录中的所有文件名，并返回完整的文件路径
fileNames <- list.files(path, full.names = TRUE)
# 使用lapply读取所有XPT文件
data <- lapply(fileNames, function(x) {
  tryCatch({
    read.xport(x, header = TRUE)
  }, error = function(e) {
    message("Error reading file: ", x)
    NULL  # 如果读取失败，返回NULL
  })
})

###去掉没有SEQN的文件之后重新读取list，然后使用join all合并所有文件进行后续分析
merged <- join_all(data,
                   by = c("SEQN"),
                   type = 'full')
merged0 <- merged[,c("SEQN","BPQ020",
                     "BPQ080")]
merged1 <- join_all(list(final_mortality,merged0),
                    by = c("SEQN"),
                    type = 'full')
merged1 <- merged1[!duplicated(merged1$SEQN),]
if(! dir.exists("提取数据")){dir.create("提取数据")}
saveRDS(merged1,file = '提取数据/提取数据-8.rds') #保存 rds





####数据提取9####
library(foreign)#读取xpt
library(tidyverse)#文件整理
library(dplyr)
library(openxlsx)
library(plyr)
library(nhanesA)
library(RNHANES)
rm(list=ls())
final_mortality <- readRDS("提取数据/提取数据-8.rds")

# 设置数据路径
path <- "original data/BMX"
# 获取目录中的所有文件名，并返回完整的文件路径
fileNames <- list.files(path, full.names = TRUE)
# 使用lapply读取所有XPT文件
data <- lapply(fileNames, function(x) {
  tryCatch({
    read.xport(x, header = TRUE)
  }, error = function(e) {
    message("Error reading file: ", x)
    NULL  # 如果读取失败，返回NULL
  })
})

###去掉没有SEQN的文件之后重新读取list，然后使用join all合并所有文件进行后续分析
merged <- join_all(data,
                   by = c("SEQN"),
                   type = 'full')
merged0 <- merged[,c("SEQN","BMXHT",
                     "BMXWT",
                     "BMXBMI",
                     "BMXWAIST")]
merged1 <- join_all(list(final_mortality,merged0),
                    by = c("SEQN"),
                    type = 'full')
merged1 <- merged1[!duplicated(merged1$SEQN),]
if(! dir.exists("提取数据")){dir.create("提取数据")}
saveRDS(merged1,file = '提取数据/提取数据-9.rds') #保存 rds




####数据提取10####
library(foreign)#读取xpt
library(tidyverse)#文件整理
library(dplyr)
library(openxlsx)
library(plyr)
library(nhanesA)
library(RNHANES)
rm(list=ls())
final_mortality <- readRDS("提取数据/提取数据-9.rds")

# 设置数据路径
path <- "original data/BPX"
# 获取目录中的所有文件名，并返回完整的文件路径
fileNames <- list.files(path, full.names = TRUE)
# 使用lapply读取所有XPT文件
data <- lapply(fileNames, function(x) {
  tryCatch({
    read.xport(x, header = TRUE)
  }, error = function(e) {
    message("Error reading file: ", x)
    NULL  # 如果读取失败，返回NULL
  })
})

###去掉没有SEQN的文件之后重新读取list，然后使用join all合并所有文件进行后续分析
merged <- join_all(data,
                   by = c("SEQN"),
                   type = 'full')
merged0 <- merged[,c("SEQN","BPXSY3",
                     "BPXDI3")]
merged1 <- join_all(list(final_mortality,merged0),
                    by = c("SEQN"),
                    type = 'full')
merged1 <- merged1[!duplicated(merged1$SEQN),]
if(! dir.exists("提取数据")){dir.create("提取数据")}
saveRDS(merged1,file = '提取数据/提取数据-10.rds') #保存 rds



####数据提取11####
library(foreign)#读取xpt
library(tidyverse)#文件整理
library(dplyr)
library(openxlsx)
library(plyr)
library(nhanesA)
library(RNHANES)
rm(list=ls())
final_mortality <- readRDS("提取数据/提取数据-10.rds")

# 设置数据路径
path <- "original data/Glycohemoglobin"
# 获取目录中的所有文件名，并返回完整的文件路径
fileNames <- list.files(path, full.names = TRUE)
# 使用lapply读取所有XPT文件
data <- lapply(fileNames, function(x) {
  tryCatch({
    read.xport(x, header = TRUE)
  }, error = function(e) {
    message("Error reading file: ", x)
    NULL  # 如果读取失败，返回NULL
  })
})

###去掉没有SEQN的文件之后重新读取list，然后使用join all合并所有文件进行后续分析
merged <- join_all(data,
                   by = c("SEQN"),
                   type = 'full')
merged0 <- merged[,c("SEQN","LBXGH")]
merged1 <- join_all(list(final_mortality,merged0),
                    by = c("SEQN"),
                    type = 'full')
merged1 <- merged1[!duplicated(merged1$SEQN),]
if(! dir.exists("提取数据")){dir.create("提取数据")}
saveRDS(merged1,file = '提取数据/提取数据-11.rds') #保存 rds



####数据提取12####
library(foreign)#读取xpt
library(tidyverse)#文件整理
library(dplyr)
library(openxlsx)
library(plyr)
library(nhanesA)
library(RNHANES)
rm(list=ls())
final_mortality <- readRDS("提取数据/提取数据-11.rds")

# 设置数据路径
path <- "original data/Cholesterol"
# 获取目录中的所有文件名，并返回完整的文件路径
fileNames <- list.files(path, full.names = TRUE)
# 使用lapply读取所有XPT文件
data <- lapply(fileNames, function(x) {
  tryCatch({
    read.xport(x, header = TRUE)
  }, error = function(e) {
    message("Error reading file: ", x)
    NULL  # 如果读取失败，返回NULL
  })
})

###去掉没有SEQN的文件之后重新读取list，然后使用join all合并所有文件进行后续分析
merged <- join_all(data,
                   by = c("SEQN"),
                   type = 'full')
colnames(merged)
merged0 <- merged[,c("SEQN","LBXTC",
                     "LBDHDL",
                     "LBDLDL",
                     "LBXTR"
                     )]
merged1 <- join_all(list(final_mortality,merged0),
                    by = c("SEQN"),
                    type = 'full')
merged1 <- merged1[!duplicated(merged1$SEQN),]
if(! dir.exists("提取数据")){dir.create("提取数据")}
saveRDS(merged1,file = '提取数据/提取数据-12.rds') #保存 rds



####数据提取13####
library(foreign)#读取xpt
library(tidyverse)#文件整理
library(dplyr)
library(openxlsx)
library(plyr)
library(nhanesA)
library(RNHANES)
rm(list=ls())
final_mortality <- readRDS("提取数据/提取数据-12.rds")

# 设置数据路径
path <- "original data/CRP"
# 获取目录中的所有文件名，并返回完整的文件路径
fileNames <- list.files(path, full.names = TRUE)
# 使用lapply读取所有XPT文件
data <- lapply(fileNames, function(x) {
  tryCatch({
    read.xport(x, header = TRUE)
  }, error = function(e) {
    message("Error reading file: ", x)
    NULL  # 如果读取失败，返回NULL
  })
})

###去掉没有SEQN的文件之后重新读取list，然后使用join all合并所有文件进行后续分析
merged <- join_all(data,
                   by = c("SEQN"),
                   type = 'full')
colnames(merged)
merged0 <- merged[,c("SEQN","LBXCRP")]
merged1 <- join_all(list(final_mortality,merged0),
                    by = c("SEQN"),
                    type = 'full')
merged1 <- merged1[!duplicated(merged1$SEQN),]
if(! dir.exists("提取数据")){dir.create("提取数据")}
saveRDS(merged1,file = '提取数据/提取数据-13.rds') #保存 rds



####数据提取14####
library(foreign)#读取xpt
library(tidyverse)#文件整理
library(dplyr)
library(openxlsx)
library(plyr)
library(nhanesA)
library(RNHANES)
rm(list=ls())
final_mortality <- readRDS("提取数据/提取数据-13.rds")

# 设置数据路径
path <- "original data/Glucose"
# 获取目录中的所有文件名，并返回完整的文件路径
fileNames <- list.files(path, full.names = TRUE)
# 使用lapply读取所有XPT文件
data <- lapply(fileNames, function(x) {
  tryCatch({
    read.xport(x, header = TRUE)
  }, error = function(e) {
    message("Error reading file: ", x)
    NULL  # 如果读取失败，返回NULL
  })
})

###去掉没有SEQN的文件之后重新读取list，然后使用join all合并所有文件进行后续分析
merged <- join_all(data,
                   by = c("SEQN"),
                   type = 'full')
colnames(merged)
merged0 <- merged[,c("SEQN","LBXGLU")]
merged1 <- join_all(list(final_mortality,merged0),
                    by = c("SEQN"),
                    type = 'full')
merged1 <- merged1[!duplicated(merged1$SEQN),]
if(! dir.exists("提取数据")){dir.create("提取数据")}
saveRDS(merged1,file = '提取数据/提取数据-14.rds') #保存 rds



####数据提取15####
library(foreign)#读取xpt
library(tidyverse)#文件整理
library(dplyr)
library(openxlsx)
library(plyr)
library(nhanesA)
library(RNHANES)
rm(list=ls())
final_mortality <- readRDS("提取数据/提取数据-14.rds")

# 设置数据路径
path <- "original data/UA"
# 获取目录中的所有文件名，并返回完整的文件路径
fileNames <- list.files(path, full.names = TRUE)
# 使用lapply读取所有XPT文件
data <- lapply(fileNames, function(x) {
  tryCatch({
    read.xport(x, header = TRUE)
  }, error = function(e) {
    message("Error reading file: ", x)
    NULL  # 如果读取失败，返回NULL
  })
})
colnames(data[[1]])
###去掉没有SEQN的文件之后重新读取list，然后使用join all合并所有文件进行后续分析
merged <- join_all(data,
                   by = c("SEQN"),
                   type = 'full')
colnames(merged)
merged0 <- merged[,c("SEQN","LBXSUA","LBXSBU","LBXSCR")]
merged1 <- join_all(list(final_mortality,merged0),
                    by = c("SEQN"),
                    type = 'full')
merged1 <- merged1[!duplicated(merged1$SEQN),]
if(! dir.exists("提取数据")){dir.create("提取数据")}
saveRDS(merged1,file = '提取数据/提取数据-15.rds') #保存 rds


####数据提取16####
library(foreign)#读取xpt
library(tidyverse)#文件整理
library(dplyr)
library(openxlsx)
library(plyr)
library(nhanesA)
library(RNHANES)
rm(list=ls())
final_mortality <- readRDS("提取数据/提取数据-15.rds")

# 设置数据路径
path <- "original data/CBC"
# 获取目录中的所有文件名，并返回完整的文件路径
fileNames <- list.files(path, full.names = TRUE)
# 使用lapply读取所有XPT文件
data <- lapply(fileNames, function(x) {
  tryCatch({
    read.xport(x, header = TRUE)
  }, error = function(e) {
    message("Error reading file: ", x)
    NULL  # 如果读取失败，返回NULL
  })
})

###去掉没有SEQN的文件之后重新读取list，然后使用join all合并所有文件进行后续分析
merged <- join_all(data,
                   by = c("SEQN"),
                   type = 'full')
colnames(merged)
merged0 <- merged[,c("SEQN","LBXPLTSI")]
merged1 <- join_all(list(final_mortality,merged0),
                    by = c("SEQN"),
                    type = 'full')
merged1 <- merged1[!duplicated(merged1$SEQN),]
if(! dir.exists("提取数据")){dir.create("提取数据")}
saveRDS(merged1,file = '提取数据/提取数据-16.rds') #保存 rds

library(openxlsx)
write.xlsx(merged1,"提取数据/final data.xlsx")
###导出之后，xlsx中处理一下

####数据清洗为标准格式####
rm(list=ls())
data <- readRDS('提取数据/提取数据-16.rds')
data <- subset(data,eligstat=='1')
# 使用mutate和ifelse函数来创建新列CVD_Death，并根据条件标记1或0
library(dplyr)
#data <- data %>% filter(ucod_leading %in% c(1, 5))
data <- data[,-(4:8)]
data <- data[,-(1:2)]
colnames(data)[2:39] <- c("Time","Age",
                          "Sex",
                          "Marital status",
                          "Education level",
                          "Race",
                          "Smoking",
                          "Drinking",
                          "Vigorous work activity",
                          "Moderate work activity",
                          "Mild work activity",
                          "Sleep health",
                          "Diabetes",
                          "Cancer",
                          "Stroke",
                          "Hypertension",
                          "Hyperlipidemia",
                          "Coronary heart disease",
                          "Congestiveheart failure",
                          "Myocardial infarction",
                          "Angina pectoris",
                          "Height",
                          "Weight",
                          "BMI",
                          "WC",
                          "SBP",
                          "DBP",
                          "HbA1C",
                          "TC",
                          "HDL",
                          "LDL",
                          "TG",
                          "CRP",
                          "FGB",
                          "UA",
                          "BUN",
                          "Scr",
                          "PLT")

####二分类变量替换####
#colnames(data) <- gsub(" ", ".", colnames(data))
library(dplyr)

# 假设你的数据框名为data

# Gender列替换
data <- data %>%
  mutate(Sex = case_when(
    Sex == 1 ~ "1_Male",
    Sex == 2 ~ "2_Female",
    TRUE ~ as.character(Sex) # 保持其他值不变
  ))

# Marital Status列替换
marital_status_map <- c("1" = "1_Married", "2" = "2_Widowed", "3" = "3_Divorced", 
                        "4" = "4_Separated", "5" = "5_Never married", 
                        "6" = "6_Living with partner", "77" = "77_Refused", 
                        "99" = "99_Don't know")
data <- data %>%
  mutate(`Marital status` = marital_status_map[as.character(`Marital status`)])

# Education level列替换
education_level_map <- c("1" = "1_Less Than 9th Grade", 
                         "2" = "2_9-11th Grade (Includes 12th grade with no diploma)", 
                         "3" = "3_High School Grad/GED or Equivalent", 
                         "4" = "4_Some College or AA degree", 
                         "5" = "5_College Graduate or above", 
                         "7" = "7_Refused", "9" = "9_Don't Know")
data <- data %>%
  mutate(`Education level` = education_level_map[as.character(`Education level`)])

# Race列替换
race_map <- c("1" = "1_Mexican American", "2" = "2_Other Hispanic", 
              "3" = "3_Non-Hispanic White", "4" = "4_Non-Hispanic Black", 
              "6" = "6_Non-Hispanic Asian","7" = "7_	Other Race - Including Multi-Racial")
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
data <- data %>% mutate(`Vigorous work activity` = wheezy_map[as.character(`Vigorous work activity`)])
data <- data %>% mutate(`Moderate work activity` = wheezy_map[as.character(`Moderate work activity`)])
data <- data %>% mutate(`Mild work activity` = wheezy_map[as.character(`Mild work activity`)])

# Chest sound wheezy during exercise列替换
Sleep_map <- c("1" = "1_Bad", "2" = "1_Bad", "3" = "1_Bad", "0" = "2_Good", "7" = "7_Refused", "9" = "9_Don't know")
data <- data %>%
  mutate(`Sleep health` = wheezy_map[as.character(`Sleep health`)])

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
data <- data %>%
  mutate(`Coronary heart disease` = chd_map[as.character(`Coronary heart disease`)])

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

####去除非生理生化指标为NA的
# 假设你的数据框名为data
data[1:22][is.na(data[1:22])] <- "99_NA"

saveRDS(data,"Result/清洗但未插补的数据.RDS")
