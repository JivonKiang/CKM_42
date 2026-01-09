rm(list = ls())
result_list <- readRDS("KM curve/total_results.RDS")

str(result_list)

library(ggplot2)
library(dplyr)
library(tidyr)

# 确保输出目录存在
if (!dir.exists("KM subgroup")) dir.create("KM subgroup")

# 加载必要包
library(pheatmap)
library(tibble)
library(grid)

# ----------------------------
# C_index 热图绘制（已修复）
# ----------------------------
plot_cindex_heatmap <- function(db) {
  data <- result_list$C_index %>%
    filter(Database == db) %>%
    unite("x_axis", Stage, Variable, sep = " | ") %>%
    unite("y_axis", Stratification, Subgroup, sep = " | ") %>%
    group_by(x_axis, y_axis) %>%
    summarise(mean_Cindex = mean(C_index, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = x_axis, values_from = mean_Cindex) %>%
    as.data.frame()
  
  # 处理空数据情况
  if (ncol(data) > 1) {
    rownames(data) <- data$y_axis
    mat <- as.matrix(data[, -1])
    
    # 生成两种格式输出
    ph <- pheatmap(mat,
                   color = colorRampPalette(c("#2166AC", "white", "#B2182B"))(50),
                   cluster_rows = FALSE, 
                   cluster_cols = FALSE,
                   fontsize = 6,
                   main = paste("C-index Heatmap:", db),
                   silent = TRUE)
    
    # 保存PDF
    pdf(file.path("KM subgroup", paste0("Cindex_", db, ".pdf")), 
        width = 16, height = 10)
    grid::grid.draw(ph$gtable)
    dev.off()
    
    # 保存JPG
    jpeg(file.path("KM subgroup", paste0("Cindex_", db, ".jpg")), 
         width = 2400, height = 1800, res = 300)
    grid::grid.draw(ph$gtable)
    dev.off()
  }
}

# ----------------------------
# KM_stats 热图绘制（修复版）
# ----------------------------
plot_km_heatmap <- function(db, comp) {
  # 数据预处理
  data_clean <- result_list$KM_stats %>%
    filter(Database == db, 
           Comparison == comp,
           !is.na(HR), !is.na(P_value)) %>%
    mutate(
      effect_type = case_when(
        HR > 1 & P_value < 0.05 ~ "Risk",
        HR < 1 & P_value < 0.05 ~ "Protective",
        TRUE ~ "Non-sig"
      )
    ) %>%
    unite("x_axis", Stage, Variable, sep = " | ") %>%
    unite("y_axis", Stratification, Subgroup, sep = " | ") %>%
    group_by(x_axis, y_axis, effect_type) %>%
    summarise(HR = mean(HR, na.rm = TRUE), .groups = "drop")
  
  # 风险效应处理
  if (any(data_clean$effect_type == "Risk")) {
    risk_data <- data_clean %>%
      filter(effect_type == "Risk") %>%
      pivot_wider(names_from = x_axis, values_from = HR) %>%
      tibble::column_to_rownames("y_axis")
    
    # 仅在有数据时生成
    if (ncol(risk_data) > 1) {
      ph_risk <- pheatmap(as.matrix(risk_data[,-1]),
                          color = colorRampPalette(c("#FDE0DD", "#C51B7D"))(50),
                          na_col = "grey90",
                          cluster_rows = FALSE,  # 关闭聚类
                          cluster_cols = FALSE,
                          main = paste(db, comp, "- Risk Effects"),
                          silent = TRUE)
      
      # 双格式保存
      save_plot(ph_risk, paste0("HR_Risk_", db, "_", gsub(" ", "", comp)))
    }
  }
  
  # 保护效应处理（逻辑同上）
  if (any(data_clean$effect_type == "Protective")) {
    protective_data <- data_clean %>%
      filter(effect_type == "Protective") %>%
      pivot_wider(names_from = x_axis, values_from = HR) %>%
      tibble::column_to_rownames("y_axis")
    
    if (ncol(protective_data) > 1) {
      ph_protective <- pheatmap(as.matrix(protective_data[,-1]),
                                color = colorRampPalette(c("#DEEBF7", "#08519C"))(50),
                                na_col = "grey90",
                                cluster_rows = FALSE,
                                cluster_cols = FALSE,
                                main = paste(db, comp, "- Protective Effects"),
                                silent = TRUE)
      
      save_plot(ph_protective, paste0("HR_Protective_", db, "_", gsub(" ", "", comp)))
    }
  }
}

# 通用保存函数
save_plot <- function(ph_object, filename_prefix) {
  # PDF格式
  pdf(file.path("KM subgroup", paste0(filename_prefix, ".pdf")), 
      width = 10, height = 8)
  grid::grid.newpage()
  grid::grid.draw(ph_object$gtable)
  dev.off()
  
  # JPG格式
  jpeg(file.path("KM subgroup", paste0(filename_prefix, ".jpg")), 
       width = 2400, height = 1800, res = 300)
  grid::grid.newpage()
  grid::grid.draw(ph_object$gtable)
  dev.off()
}

# ----------------------------
# 执行主程序
# ----------------------------
# C_index处理
library(purrr)
walk(unique(result_list$C_index$Database), plot_cindex_heatmap)

# KM_stats处理
for (db in unique(result_list$KM_stats$Database)) {
  for (comp in c("Q2 vs Q1", "Q3 vs Q1")) {
    tryCatch(
      plot_km_heatmap(db, comp),
      error = function(e) message(paste("跳过", db, comp, ":", e$message))
    )
  }
}