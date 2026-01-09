library(magick)
library(stringr)

# 创建输出目录
if (!dir.exists("RCS_combined")) {
  dir.create("RCS_combined")
}

# 获取所有图片文件路径
files <- list.files("RCS", 
                    pattern = "\\.(png|jpg|jpeg)$", 
                    full.names = TRUE,
                    ignore.case = TRUE)

# 按前缀分类文件
file_groups <- split(files, sapply(files, function(x) {
  str_split_fixed(basename(x), "_", 2)[,1]
}))

# 处理每个组别
for (group_name in names(file_groups)) {
  # 按文件名排序
  sorted_files <- file_groups[[group_name]][order(basename(file_groups[[group_name]]))]
  
  # 确保每组有15张图片
  if (length(sorted_files) != 15) {
    message(paste("跳过", group_name, "：数量不符"))
    next
  }
  
  # 生成字母标签序列（A-O）
  labels <- LETTERS[1:15]
  
  # 将15张图片分为3列（每列5张）
  cols <- list(
    sorted_files[1:5],   # 第一列
    sorted_files[6:10],  # 第二列
    sorted_files[11:15]  # 第三列
  )
  
  # 创建列图片（添加字母标签）
  column_images <- lapply(cols, function(col_files) {
    # 处理每张图片
    imgs <- lapply(seq_along(col_files), function(j) {
      # 获取全局索引
      global_index <- match(col_files[j], sorted_files)
      
      # 读取图片并添加标签
      image_read(col_files[j]) %>% 
        image_annotate(
          text = labels[global_index],  # A-O字母
          location = "+20+20",          # 左上角偏移
          size = 150,                    # 字体大小
          color = "black",              # 字体颜色
          weight = 600,                 # 字体粗细
          boxcolor = "transparent"      # 透明背景
        )
    })
    image_append(image_join(imgs), stack = TRUE)
  })
  
  # 合并三列
  combined <- image_append(image_join(column_images), stack = FALSE)
  
  # 保存高质量结果
  image_write(combined, 
              path = file.path("RCS_combined", paste0(group_name, "_combined.jpg")),
              quality = 100)
}

message("图片合并完成！结果保存在 RCS_combined 文件夹中。")