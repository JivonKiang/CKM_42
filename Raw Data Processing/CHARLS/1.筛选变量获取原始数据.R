library(openxlsx)
library(dplyr)
library(stringr)
library(haven)
rm(list = ls())
data1 <- read_dta("data/Harmonized CHARLS/H_CHARLS_D_Data.dta")
#data2 <- read_dta("data/Harmonized CHARLS/H_CHARLS_EOL_a.dta")##数据库官方的随访结果
#data3 <- read_dta("data/Harmonized CHARLS/H_CHARLS_LH_a.dta")##遗产方面的内容

colnames(data1)
# 加载必要的包
library(dplyr)

####---第一个文件中的数据---####
# 定义周期1的变量名
vars_w1 <- c("r1iwy",
             "r1iwm",
             "r1iwstat",
             "r1agey",
             "ragender",
             "r1mstat",
             "s1educ_c",
             "r1hukou",
             "r1smokev",
             "r1drinkev",
             "r1vgact_c",
             "r1mdact_c",
             "r1ltact_c",
             "r1sleeprl",
             "r1diabe",
             "r1cancre",
             "r1stroke",
             "r1hibpe",
             "r1hearte",
             "r1dyslipe",
             "r1kidneye",
             "r1mheight",
             "r1mweight",
             "r1mbmi",
             "r1mwaist",
             "r1systo",
             "r1diasto")

# 定义公共变量（每个周期都需要提取）
common_vars <- c("radyear", "radmonth", "ID_w1")

# 合并变量列表
vars_w1 <- c(vars_w1, common_vars)
vars_w2 <- gsub("r1", "r2", vars_w1)
vars_w3 <- gsub("r1", "r3", vars_w1)
vars_w4 <- gsub("r1", "r4", vars_w1)

# 检查变量是否存在
check_vars <- function(vars, data) {
  missing_vars <- vars[!vars %in% colnames(data)]
  if (length(missing_vars) > 0) {
    warning(paste("以下变量不存在：", paste(missing_vars, collapse = ", ")))
    return(vars[vars %in% colnames(data)])  # 返回存在的变量
  } else {
    return(vars)
  }
}

# 提取周期1的变量
vars_w1 <- check_vars(vars_w1, data1)
extracted_w1 <- data1 %>%
  select(all_of(vars_w1))

# 提取周期2的变量
vars_w2 <- check_vars(vars_w2, data1)
extracted_w2 <- data1 %>%
  select(all_of(vars_w2))

# 提取周期3的变量
vars_w3 <- check_vars(vars_w3, data1)
extracted_w3 <- data1 %>%
  select(all_of(vars_w3))

# 提取周期4的变量
vars_w4 <- check_vars(vars_w4, data1)
extracted_w4 <- data1 %>%
  select(all_of(vars_w4))

# 如果需要将所有周期的数据合并到一个长格式的数据框中
# 可以使用 bind_rows()，但需要先对变量名进行统一处理
extracted_w1$wave <- 1  # 添加周期标识
extracted_w2$wave <- 2
extracted_w3$wave <- 3
extracted_w4$wave <- 4

# 统一变量名（以Wave 1的变量名为基准）
names(extracted_w2) <- gsub("r2", "r1", names(extracted_w2))
names(extracted_w3) <- gsub("r3", "r1", names(extracted_w3))
names(extracted_w4) <- gsub("r4", "r1", names(extracted_w4))

# 合并数据
combined_data <- bind_rows(extracted_w1, extracted_w2, extracted_w3, extracted_w4)

# 查看结果
head(combined_data)

####---单独提取一下几个周期特有的---####
Health1 <- read_dta("data/wave1/Health_Status_and_Functioning.dta")
Health2 <- read_dta("data/wave2/Health_Status_and_Functioning.dta")
Health3 <- read_dta("data/wave3/Health_Status_and_Functioning.dta")
Health4 <- read_dta("data/wave4/Health_Status_and_Functioning.dta")

library(dplyr)

# 假设 Health1, Health2, Health3, Health4 已经定义
data_frames <- list(Health1, Health2
                    #, Health3, Health4
                    )

# 第一步：从每个数据框中提取指定变量
extracted_data <- lapply(data_frames, function(df) {
  # 检查是否包含所有需要的变量
  if(all(c("ID", "da003", "da004") %in% colnames(df))) {
    df %>% select(ID, da003, da004)#wave3, wave4没有这个数据
  } else {
    stop("Some variables are missing in the data frame.")
  }
})

# 第二步：将提取后的数据框合并为一个数据框
merged_data <- bind_rows(extracted_data)

# 第三步：通过 left_join 将合并后的数据框与 combined_data 合并

# 查找 "ID_w1" 列的位置
col_index <- which(names(combined_data) == "ID_w1")

# 修改列名
names(combined_data)[col_index] <- "ID"

final_data <- left_join(combined_data, merged_data, by = "ID")

# 查看最终结果
print(final_data)

####---血液数据获取---####
library(haven)
blood_2011 <- read_dta("data/wave1/Blood.dta")
blood_2015 <- read_dta("data/wave3/Blood.dta")

blood_in2011 <- blood_2011[,c("ID",
                              "newhba1c",
                              "newcho",
                              "newhdl",
                              "newldl",
                              "newcrp",
                              "newtg",
                              "newglu",
                              "newua",
                              "newbun",
                              "newcrea",
                              "qc1_vb009")]
colnames(blood_in2011) <- c("ID",
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
                            "PLT")

blood_in2015 <- blood_2015[,c("ID",
                              "bl_hbalc",
                              "bl_cho",
                              "bl_hdl",
                              "bl_ldl",
                              "bl_crp",
                              "bl_tg",
                              "bl_glu",
                              "bl_ua",
                              "bl_bun",
                              "bl_crea",
                              "bl_plt")]
colnames(blood_in2015) <- c("ID",
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
                            "PLT")

blood <- rbind(blood_in2011,blood_in2015)

whole_data <- left_join(final_data,blood,by="ID")

saveRDS(whole_data,"Result/whole_data.RDS")
#write.csv(whole_data,"whole_data.csv")

####随访数据合并------------------------
rm(list = ls())
# 加载dplyr包
library(dplyr)
whole_data <- readRDS("Result/whole_data.RDS")

# 假设你的数据框名为df，并且有四列，这里我们用mtcars作为示例
df <- whole_data[, c("ID",
                     "wave",
                     "r1iwy",
                     "r1iwm",
                     "r1iwstat",
                     "radyear",
                     "radmonth"
                     )]  # 提取随访相关数据
colnames(df) <- c("ID",
                  "周期",
                  "随访年",
                  "随访月",
                  "随访状态",
                  "死亡年",
                  "死亡月")

summary(as.factor(df$随访状态))
str(df)

library(lubridate)
library(dplyr)
library(data.table)

####--------------
#数据预处理优化（向量化操作）
# 创建周期时间窗（预先生成日期对象）[9](@ref)
cycle_dt <- data.table(
  周期 = 1:4,
  start = as.Date(c("2011-01-01","2013-01-01","2015-01-01","2017-01-01")),
  end = as.Date(c("2012-12-31","2014-12-31","2016-12-31","2018-12-31"))
)

# 转换为data.table对象（提升处理速度）[5,9](@ref)
dt <- as.data.table(df) %>% 
  merge(cycle_dt, by = "周期") %>% 
  .[, `:=`(
    # 生成有效随访日期（自动处理无效月份）
    follow_date = fifelse(
      随访月 %between% c(1,12),
      make_date(floor(随访年), 随访月, 15),
      as.Date(NA)
    ),
    # 生成有效死亡日期（过滤不合理年份）
    death_date = fifelse(
      死亡月 %between% c(1,12) & 死亡年 >= 1900,
      make_date(floor(死亡年), 死亡月, 15),
      as.Date(NA)
    )
  )]

####--------------
#状态判断优化（向量化条件分支）
# 状态映射函数（利用标签属性）[3](@ref)
status_labels <- attr(dt$随访状态, "labels")
dt[, status := fcase(
  随访状态 == 1, "存活",
  随访状态 %in% c(5,6), "死亡",
  随访状态 %in% c(0,4), "失访",
  随访状态 == 9, "未知",
  default = NA_character_
)]
summary(as.factor(dt$status))

# 死亡日期有效性验证（区间判断）
dt[, death_valid := fifelse(
  death_date %between% list(start, end), 
  death_date, 
  as.Date(NA)
)]

####--------------
#时间计算优化（并行处理）
# 计算每个观察期的生存时间
dt[, time_days := fcase(
  !is.na(death_valid), as.integer(death_valid - start),
  is.na(follow_date), as.integer(end - start),  # 缺失随访日视为整周期
  default = as.integer(end - start)
)]

# 跨周期聚合（内存预分配优化）[3,9](@ref)
str(dt)
final_dt <- dt[
  order(ID, 周期),  # 确保时序正确性[3](@ref)
  .(
    # 状态判定（死亡优先）
    Status = if (any(随访状态 %in% c(5,6))) "死亡" 
    else if (last(随访状态 == 1)) "存活" 
    else "失访",
    
    # 时间计算（动态关联周期）
    Time = {
      # 提取有效死亡记录
      death_records <- .SD[!is.na(death_valid)]
      if (nrow(death_records) > 0) {
        first_death_cycle <- death_records[which.min(周期), 周期]
        as.integer(death_records[周期 == first_death_cycle, death_valid] - start[first_death_cycle])
      } else {
        # 计算最后有效随访时间
        last_follow <- max(follow_date, na.rm = TRUE)
        if (is.infinite(last_follow)) NA_integer_ 
        else {
          last_cycle <- .SD[which.max(follow_date), 周期]
          as.integer(last_follow - start[last_cycle])
        }
      }
    }
  ), 
  by = ID
][
  # 处理无效值
  , Time := fifelse(is.na(Time) | Time < 0, NA_integer_, Time)
]

summary(as.factor(final_dt$Status))
summary(as.factor(final_dt$Time))

library(dplyr)

str(final_dt)

library(data.table)

# 将 data.table 转换为 tibble
final_dt <- setDT(final_dt)

# 使用 dplyr 操作
final_dt <- final_dt %>%
  mutate(Status = case_when(
    Status == "存活" ~ 0L,
    Status == "死亡" ~ 1L,
    TRUE ~ as.integer(NA)  # 转换为整数型 NA
  )) %>%
  filter(!is.na(Status))  # 移除未知的行

summary(as.factor(final_dt$Status))
summary(as.factor(final_dt$Time))

# 检查 ID 列是否存在重复值
has_duplicates <- any(duplicated(final_dt$ID))

# 输出结果
if (has_duplicates) {
  cat("存在重复的 ID 值。\n")
} else {
  cat("ID 列中没有重复值。\n")
}

# 假设 final_dt 和 whole_data 已经定义
# 1. 对 whole_data 进行预处理：选择每个 ID 缺失值最少的那一行
library(dplyr)
#detach("package:plyr", unload = TRUE)  # 卸载冲突包
whole_data <- whole_data %>%
  group_by(ID) %>%
  mutate(na_count = rowSums(is.na(across(everything())))) %>%
  slice(which.min(na_count)) %>%
  ungroup()

# 2. 将 final_dt 与处理后的 whole_data 进行左连接
result_data <- left_join(final_dt, whole_data[, -ncol(whole_data)], by = "ID")

# 查看结果
print(result_data)

# 删除第 4 到 6 列和第 31 到 33 列
result_data <- result_data[, -c(4:6, 31:33)]

colnames(result_data)
colnames(result_data)[4:29] <- c(
  "Age",
  "Sex",
  "Marital status",
  "Education level",
  "Hukou status ",
  "Smoking",
  "Drinking",
  "Intensive Physical Activity",
  "Moderate Physical Activity",
  "Light Physical Activity",
  "Sleep health",
  "Diabetes",
  "Cancer",
  "Stroke",
  "Hypertension",
  "Heart disease",
  "Dyslipidemia",
  "Kidney Diease",
  "Height",
  "Weight",
  "BMI",
  "WC",
  "SBP",
  "DBP",
  "Chest pain",
  "Chest Pains When Climbing")

saveRDS(result_data,"Result/待插补数据.RDS")
