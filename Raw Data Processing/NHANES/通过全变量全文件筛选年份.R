library(openxlsx)
colnames(变量)
library(dplyr)
library(stringr)
load("NHANES20241008全变量全文件名称.RData")
colnames(变量)
heart_disease <- 变量 %>% filter(grepl('heart',变量$"Variable.Description"))#筛选
Cardiovascular <- 变量 %>% filter(grepl('Cardiovascular',变量$"Variable.Description"))#筛选

heart_disease_file <- 变量 %>% filter(grepl('heart',变量$"Data.File.Description"))#筛选
Cardiovascular_file <- 变量 %>% filter(grepl('Cardiovascular',变量$"Data.File.Description"))#筛选

write.xlsx(heart_disease,"heart_disease.xlsx")
write.xlsx(Cardiovascular_file,"Cardiovascular_file.xlsx")
