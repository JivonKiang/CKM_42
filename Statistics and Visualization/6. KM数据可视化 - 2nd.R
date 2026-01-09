library(readxl)
library(openxlsx)
library(dplyr)
library(purrr)

# 创建输出文件夹
if (!dir.exists("KM combined")) {
  dir.create("KM combined")
}

# 定义文件路径
input_dir <- "KM curve"
output_file <- file.path("KM combined", "combined_statistics.xlsx")

# 获取所有xlsx文件路径（排除临时文件）
excel_files <- list.files(
  path = input_dir,
  pattern = "\\.xlsx$",
  full.names = TRUE
) %>% 
  grep(pattern = "~\\$", value = TRUE, invert = TRUE)  # 排除打开的临时文件

# 验证文件数量
if(length(excel_files) != 2) {
  stop(paste("找到", length(excel_files), "个文件，但需要合并2个文件"))
}

# 读取数据 ------------------------------------------------------------
# 自定义读取函数（处理可能的不同sheet名称）
read_km_stats <- function(file) {
  # 自动检测第一个有效sheet
  sheets <- excel_sheets(file)
  target_sheet <- grep("stat|result|data", sheets, value = TRUE, ignore.case = TRUE)[1]
  
  if(is.na(target_sheet)) target_sheet <- 1  # 默认第一个sheet
  
  read_excel(file, sheet = target_sheet) %>% 
    mutate(across(where(is.character), ~ na_if(., "")))  # 转换空字符串为NA
}

# 读取所有数据
data_list <- map(excel_files, ~ {
  tryCatch(
    read_km_stats(.x),
    error = function(e) {
      message("读取文件出错：", .x)
      stop(e)
    }
  )
})

data_list

library(ggplot2)
library(patchwork)
library(ggsci)

# 创建可视化文件夹
if (!dir.exists("KM data visual")) {
  dir.create("KM data visual")
}

# 自定义可视化主题
my_theme <- theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        legend.position = "top",
        axis.text.x = element_text(angle = 45, hjust = 1))

# 数据集1可视化：C指数分布 -----------------------------------------------
plot_cindex <- function(data) {
  data %>%
    ggplot(aes(x = Stage, y = C_index, color = Variable)) +
    geom_point(size = 3) +
    geom_line(aes(group = Variable), linewidth = 1) +
    scale_y_continuous(limits = c(0.4, 0.8), breaks = seq(0.4, 0.8, 0.1)) +
    scale_color_nejm() +
    labs(title = paste("C-index by Stage -", unique(data$Database)),
         x = "CKD Stage", y = "C-index") +
    my_theme
}

# 数据集2可视化：HR森林图 -----------------------------------------------
plot_hr <- function(data) {
  # 数据处理：创建错开位置和标注文本
  valid_data <- data %>% 
    filter(!is.na(HR)) %>%
    mutate(
      # 动态设置比较组顺序并移除空level
      Comparison = factor(Comparison, levels = c("Q2 vs Q1", "Q3 vs Q1", "Q4 vs Q1")),
      Comparison = droplevels(Comparison)
    ) %>%
    mutate(
      group_id = as.numeric(Comparison),
      # 生成组内错开位置
      var_order = ave(seq_along(Comparison), Comparison, FUN = seq_along),
      y_position = group_id + (var_order - (max(var_order) + 1)/2) * 0.3
    ) %>%
    mutate(
      label_text = sprintf("HR: %.2f (%.2f-%.2f)\np = %.3g", 
                           HR, CI_low, CI_high, P_value)
    )
  
  # 动态调整标签位置
  max_hr <- max(valid_data$CI_high, na.rm = TRUE)
  label_x <- ifelse(max_hr > 5, max_hr * 1.2, 5)
  
  # 确保至少存在一个比较组
  if (nrow(valid_data) == 0) return(ggplot())
  
  # 绘图
  ggplot(valid_data, aes(x = HR, y = y_position, color = Variable)) +
    geom_vline(xintercept = 1, linetype = 2, color = "grey50") +
    geom_pointrange(aes(xmin = CI_low, xmax = CI_high), 
                    size = 0.7, position = position_dodge(width = 0.3)) +
    geom_text(aes(x = label_x, y = y_position, label = label_text),
              hjust = 0, color = "black", size = 3, lineheight = 0.8) +
    scale_x_log10(
      breaks = c(0.5, 1, 2, 5, 10),
      expand = expansion(mult = c(0.1, 0.5))
    ) +
    scale_y_continuous(
      breaks = unique(valid_data$group_id),
      labels = levels(valid_data$Comparison),
      limits = c(min(valid_data$group_id)-0.5, max(valid_data$group_id)+0.5)
    ) +
    scale_color_nejm() +
    labs(#title = paste("Hazard Ratios -", unique(data$Database)),
         #x = "Hazard Ratio (95% CI)", 
         y = "") +
    my_theme +
    theme(
      panel.grid.major.y = element_blank(),
      axis.text.y = element_text(size = 10, face = "bold"),
      plot.margin = margin(r = unit(3, "cm"))
    ) +
    coord_cartesian(clip = "off")
}

# 批量生成并保存可视化结果 ----------------------------------------------
walk(unique(data_list[[1]]$Database), function(db) {
  # 数据集1可视化
  cdata <- data_list[[1]] %>% filter(Database == db)
  p1 <- plot_cindex(cdata)
  
  # 数据集2可视化（分阶段）
  hrdata <- data_list[[2]] %>% filter(Database == db)
  
  stage_plots <- map(unique(hrdata$Stage), function(st) {
    p <- hrdata %>% 
      filter(Stage == st) %>% 
      plot_hr() +
      labs(subtitle = paste("Stage", st))
    
    # 保存单阶段HR图
    ggsave(file.path("KM data visual", 
                     paste0(db, "_Stage", st, "_HR.png")),
           plot = p, width = 8, height = 6, dpi = 300)
    p
  })
  
  # 合成组合图
  combined_plot <- p1 / wrap_plots(stage_plots) +
    plot_layout(heights = c(1, 2))# +
    #plot_annotation(title = paste("Analysis Report:", db),
     #               theme = theme(plot.title = element_text(hjust = 0.5, size = 16)))
  
  # 保存组合图
  ggsave(file.path("KM data visual", paste0(db, "_Combined.png")),
         plot = combined_plot, width = 12, height = 15, dpi = 300)
})