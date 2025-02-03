
library(stringi)

pdf_paths <- list.files("C:/Users/y-zhe/OneDrive/Desktop/labels", full.names = TRUE)

extract_shipping_label_info <- function(pdf_path, dpi = 300) {
  
  pdf_image <- magick::image_read_pdf(pdf_path, density = dpi)
  
  # 加载 OCR 引擎
  eng <- tesseract::tesseract("eng")
  
  # 提取第一页文本（直接使用临时文件路径）
  ocr_text <- tesseract::ocr(pdf_image, engine = eng)
  
  # 分行拆分文本
  lines <- unlist(strsplit(ocr_text, "\n"))
  
  # 初始化变量
  customer_name <- NA
  tracking_number <- NA
  
  # 找到 "USPS TRACKING" 所在的行
  usps_line_index <- which(stri_detect_fixed(lines, "USPS TRACKING", case_insensitive = TRUE))
  
  if (length(usps_line_index) > 0) {
    name_line_indices <- usps_line_index - c(3:5)
    name_line_indices <- name_line_indices[name_line_indices > 0]  # 确保索引有效
    
    potential_names <- lines[name_line_indices]
    
    # 删除包含地址后缀关键词的行
    address_keywords <- c("APT", "SUITE", "UNIT", "AVE", "PKWY", "ST", "BLVD", "DR", "RD", "LN", "CT", "WAY", "PL", "HWY")
    regex_pattern <- paste0("(?i)\\b(", paste(address_keywords, collapse = "|"), ")(\\s+", paste(address_keywords, collapse = "|"), ")?\\b")
    potential_names <- potential_names[!stri_detect_regex(potential_names, regex_pattern, case_insensitive = TRUE)]
    
    # 检查每一行是否可能是名字
    for (potential_name in potential_names) {
      if (!is.na(potential_name)) {
        # 如果行中有数字，尝试仅移除数字部分进行进一步处理
        if (stri_detect_regex(potential_name, "\\d")) {
          potential_name <- gsub("\\d", "", potential_name)
        }
        
        # 清理名字，去掉前缀或无用字符
        first_upper_pos <- stri_locate_first_regex(potential_name, "[A-Z]")[1]
        if (!is.na(first_upper_pos)) {
          potential_name <- substr(potential_name, first_upper_pos, nchar(potential_name))
        }
        potential_name <- stri_trim_both(potential_name)
        
        # 校正名字，只保留有效单词
        words <- unlist(stri_split_fixed(potential_name, " "))
        valid_words <- words[nchar(words) >= 3]  # 仅保留长度大于等于 3 的单词
        if (length(valid_words) > 0) {
          potential_name <- stri_join(valid_words, collapse = " ")
        } else {
          next  # 跳过无效名字行
        }
        
        # 检查名字格式（支持 1 到 4 个单词）
        if (stri_detect_regex(
          potential_name, "^([A-Z]+|[A-Z][a-z]+)(\\s([A-Z]+|[A-Z][a-z]+)){0,3}$"
        )) {
          customer_name <- potential_name
          break
        }
      }
    }
  }
  
  # 提取运单号
  matches <- regmatches(ocr_text, gregexpr("\\b\\d{4} \\d{4} \\d{4} \\d{4} \\d{4} \\d{2}\\b", ocr_text))
  if (length(unlist(matches)) > 0) {
    tracking_number <- gsub(" ", "", unlist(matches)[1])  # 移除空格
  }
  
  # 返回姓名和运单号
  return(list(
    customer_name = if (!is.na(customer_name)) toupper(customer_name) else NA,
    tracking_number = tracking_number
  ))
}

for(pdf_path in pdf_paths)
{
 print(extract_shipping_label_info(pdf_path))
}

