# 拆分文件的脚本

split_file <- function(input_file, output_dir, filetype, lines_per_file = 50) {
  # 确保输入文件存在
  if (!file.exists(input_file)) stop("输入文件不存在")
  
  # 读取所有内容
  content <- readLines(input_file)
  
  # 创建输出目录
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  # 初始化变量
  file_count <- 1
  line_count <- 0
  buffer <- character(0)
  
  # 遍历每一行
  for (line in content) {
    buffer <- c(buffer, line) # 添加当前行到缓存
    line_count <- line_count + 1
    
    # 判断是否需要写入文件
    if (line_count >= lines_per_file || grepl("^[}]$", line)) {
      output_file <- file.path(output_dir, paste0(filetype, file_count, ".R"))
      writeLines(buffer, output_file) # 将缓冲区内容写入文件
      file_count <- file_count + 1    # 文件计数递增
      line_count <- 0                 # 重置行计数
      buffer <- character(0)          # 清空缓存
    }
  }
  
  # 将剩余内容写入文件
  if (length(buffer) > 0) {
    output_file <- file.path(output_dir, paste0(filetype, file_count, ".R"))
    writeLines(buffer, output_file)
  }
  
  message("拆分完成，生成文件数：", file_count)
}

# 使用脚本
split_file(
  input_file = "server.R",    # 输入文件路径
  output_dir = "./maintainance/split",     # 输出目录
  filetype = "server",
  lines_per_file = 1000         # 每个文件的行数
)

split_file(
  input_file = "ui.R",    # 输入文件路径
  output_dir = "./maintainance/split",     # 输出目录
  filetype = "ui",
  lines_per_file = 1000         # 每个文件的行数
)

split_file(
  input_file = "utils.R",    # 输入文件路径
  output_dir = "./maintainance/split",     # 输出目录
  filetype = "utils",
  lines_per_file = 1000         # 每个文件的行数
)
