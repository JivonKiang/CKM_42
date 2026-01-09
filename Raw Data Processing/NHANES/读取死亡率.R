####NHANES VERSION 读取死亡率####
library(readr)
library(dplyr)
# the location where the .DAT file is saved:
setwd("D:\\8.研究历史及现况\\20241008_曹艳杰\\NHANES")

# remove all objects from the R environment
rm(list=ls())
srvyin <- paste("mortality//NHANES_2017_2018_MORT_2019_PUBLIC.dat")   # full .DAT name here
srvyout <- "Mortality" # shorthand dataset name here

# Example syntax:
#srvyin <- paste("NHANES_1999_2000_MORT_2019_PUBLIC.dat")   
#srvyout <- "NHANES_1999_2000"      


####NHANES VERSION 读取死亡率 批量处理####
datasets <- c("NHANES_1999_2000_MORT_2019_PUBLIC.dat",
              "NHANES_2001_2002_MORT_2019_PUBLIC.dat",
              "NHANES_2003_2004_MORT_2019_PUBLIC.dat",
              "NHANES_2005_2006_MORT_2019_PUBLIC.dat",
              "NHANES_2007_2008_MORT_2019_PUBLIC.dat",
              "NHANES_2009_2010_MORT_2019_PUBLIC.dat",
              "NHANES_2011_2012_MORT_2019_PUBLIC.dat",
              "NHANES_2013_2014_MORT_2019_PUBLIC.dat",
              "NHANES_2015_2016_MORT_2019_PUBLIC.dat",
              "NHANES_2017_2018_MORT_2019_PUBLIC.dat")
combined_data <- list()

for (file in datasets) {
  #srvyin <- paste("NHANES_2017_2018_MORT_2019_PUBLIC.dat")   # full .DAT name here
  srvyout <- gsub("_MORT_2019_PUBLIC.dat","",file) # shorthand dataset name here
  # read in the fixed-width format ASCII file
  dsn <- read_fwf(file=srvyin,
                  col_types = "iiiiiiii",
                  fwf_cols(seqn = c(1,6),
                           eligstat = c(15,15),
                           mortstat = c(16,16),
                           ucod_leading = c(17,19),
                           diabetes = c(20,20),
                           hyperten = c(21,21),
                           permth_int = c(43,45),
                           permth_exm = c(46,48)
                  ),
                  na = c("", "."))
  assign(paste0(srvyout), dsn)
  combined_data[[srvyout]] <- dsn
}
final_data <- do.call(rbind,combined_data)
colnames(combined_data)[1] <- c("SEQN")
# 将数据框保存为.rds文件
saveRDS(Mortality, "Mortality.rds")