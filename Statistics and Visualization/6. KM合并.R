library(magick)
library(purrr)
library(dplyr)
library(stringr)

# 创建输出文件夹
if (!dir.exists("KM combined")) {
  dir.create("KM combined")
}

# 处理每个数据库
for (db in c("NHANES", "CHARLS")) {
  # 获取排序后的文件列表
  files <- list.files(
    path = "KM curve",
    pattern = sprintf("^%s_CKM_Stage.*_KM\\.png$", db),
    full.names = TRUE
  ) %>% 
    tibble(file = .) %>%
    mutate(
      stage = str_extract(file, "Stage(\\d+)") %>% str_remove("Stage") %>% as.integer(),
      var = str_extract(file, "_(\\d+)_KM") %>% str_remove_all("_|KM") %>% as.integer()
    ) %>%
    arrange(stage, var) %>%
    pull(file)
  
  if (length(files) != 18) stop(paste("Found", length(files), "files for", db))
  
  # 生成列优先标签矩阵（6行3列）
  label_matrix <- matrix(LETTERS[1:18], nrow = 6, ncol = 3, byrow = FALSE)
  
  # 创建标签向量（按行优先顺序访问矩阵）
  labels_col_order <- map_chr(1:18, ~ {
    row <- ((.x - 1) %/% 3) + 1  # 计算行号
    col <- ((.x - 1) %% 3) + 1    # 计算列号
    label_matrix[row, col]
  })
  
  # 读取并处理图像
  images <- map(1:18, function(i) {
    image_read(files[i]) %>% 
      image_scale("800x600") %>%  # 统一尺寸
      image_annotate(
        text = labels_col_order[i], # 按列优先顺序的标签
        location = "+20+20",
        size = 40,
        color = "black",
        weight = 600,
        boxcolor = "transparent"
      )
  })
  
  # 创建6行3列布局（保持原有图片排列顺序）
  combined <- image_blank(2400, 3600, "white")
  
  # 按行合并图片（保持原有布局）
  for (row in 1:6) {
    # 每行合并3张图
    row_images <- images[((row-1)*3 + 1) : (row*3)]
    combined_row <- image_append(image_join(row_images))
    
    # 计算垂直偏移量（每行600px）
    combined <- image_composite(
      combined, combined_row,
      offset = sprintf("+0+%d", (row-1)*600)
    )
  }
  
  image_write(combined, file.path("KM combined", paste0(db, "_combined_KM.png")))
}