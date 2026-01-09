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
                    "RIDRETH1")]
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

####数据提取17####
library(foreign)#读取xpt
library(tidyverse)#文件整理
library(dplyr)
library(openxlsx)
library(plyr)
library(nhanesA)
library(RNHANES)
rm(list=ls())
final_mortality <- readRDS("提取数据/提取数据-16.rds")

# 设置数据路径
path <- "original data/CDQ"
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
merged0 <- merged[,c("SEQN","CDQ001","CDQ008","CDQ010")]
merged1 <- join_all(list(final_mortality,merged0),
                    by = c("SEQN"),
                    type = 'full')
merged1 <- merged1[!duplicated(merged1$SEQN),]
if(! dir.exists("提取数据")){dir.create("提取数据")}
saveRDS(merged1,file = '提取数据/提取数据-17.rds') #保存 rds


###导出之后，xlsx中处理一下

####数据清洗为标准格式####
rm(list=ls())
data <- readRDS('提取数据/提取数据-17.rds')
data <- subset(data,eligstat=='1')
# 使用mutate和ifelse函数来创建新列CVD_Death，并根据条件标记1或0
library(dplyr)
#data <- data %>% filter(ucod_leading %in% c(1, 5))
data <- data[,-(7:8)]
data <- data[,-2]
colnames(data)[3:46] <- c("Main death reason","MCOD for DM","MCOD for HTN",
                          "Time","Age",
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
                          "FPG",
                          "UA",
                          "BUN",
                          "Scr",
                          "PLT",
                          "Chest pain","Severe chest pain","Breath shortness")


# 将第2列到第26列转换为因子
#for (i in 2:26) {
#  data[[i]] <- as.factor(data[[i]])
#}

summary(data)
saveRDS(data,"Result/清洗但未插补的数据.RDS")
