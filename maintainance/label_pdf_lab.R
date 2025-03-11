
library(stringi)

pdf_paths <- list.files("/home/ubuntu/labels", full.names = TRUE)

### Debugging
pdf_paths
pdf_path <- pdf_paths[8]
dpi = 300
### Debugging

extract_shipping_label_info <- function(pdf_path, dpi = 300) {
  
  pdf_image <- magick::image_read_pdf(pdf_path, density = dpi)
  eng <- tesseract::tesseract("eng")
  ocr_text <- tesseract::ocr(pdf_image, engine = eng)
  lines <- unlist(strsplit(ocr_text, "\n"))
  
  customer_name <- NA
  tracking_number <- NA
  
  usps_line_index <- which(stri_detect_fixed(lines, "USPS TRACKING", case_insensitive = TRUE))
  
  # 找到 "SHIP TO:" 并提取名字
  ship_to_line <- lines[stri_detect_regex(lines, "SHIP TO:", case_insensitive = FALSE)]
  if (length(ship_to_line) > 0) {
    name_match <- stri_match_first_regex(ship_to_line[1], "SHIP TO:\\s*(.*)", case_insensitive = TRUE)
    if (!is.na(name_match[2]) && nchar(name_match[2]) > 0) {
      customer_name <- stri_trim_both(name_match[2])
    }
  } else if (length(usps_line_index) > 0) {
    name_line_indices <- usps_line_index - c(3:5)
    name_line_indices <- name_line_indices[name_line_indices > 0]
    potential_names <- lines[name_line_indices]
    
    address_keywords <- c("APT", "SUITE", "UNIT", "AVE", "PKWY", "ST", "BLVD", "DR", "RD", "LN", "CT", "WAY", "PL", "HWY", "PLZ")
    regex_pattern <- paste0("(?i)\\b(", paste(address_keywords, collapse = "|"), ")(\\s+", paste(address_keywords, collapse = "|"), ")?\\b")
    potential_names <- potential_names[!stri_detect_regex(potential_names, regex_pattern, case_insensitive = TRUE)]
    
    special_keywords <- c("#|‘")
    potential_names <- potential_names[!stri_detect_regex(potential_names, special_keywords, case_insensitive = TRUE)]
    
    potential_names <- potential_names[!stringi::stri_detect_regex(potential_names, "\\b[A-Za-z]+\\s?\\d+\\b")]
    
    for (potential_name in potential_names) {
      if (!is.na(potential_name)) {
        if (stri_detect_regex(potential_name, "\\d")) {
          potential_name <- gsub("\\d", "", potential_name)
        }
        
        first_upper_pos <- stri_locate_first_regex(potential_name, "[A-Za-z]")[1]  # 改为匹配任何字母
        if (!is.na(first_upper_pos)) {
          potential_name <- substr(potential_name, first_upper_pos, nchar(potential_name))
        }
        potential_name <- stri_trim_both(potential_name)
        
        words <- unlist(stri_split_fixed(potential_name, " "))
        valid_words <- words[nchar(words) >= 3]
        if (length(valid_words) > 0) {
          potential_name <- stri_join(valid_words, collapse = " ")
        } else {
          next
        }
        
        # 规范化名字：每个单词首字母大写
        potential_name <- stri_trans_totitle(potential_name)
        
        # 检查名字格式
        if (stri_detect_regex(
          potential_name, 
          "^([A-Z][a-zA-Z]*)(?:\\s+[A-Z][a-zA-Z]*){0,3}$"
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
    tracking_number <- gsub(" ", "", unlist(matches)[1])
  }
  
  return(list(
    customer_name = if (!is.na(customer_name)) toupper(customer_name) else NA,
    tracking_number = tracking_number
  ))
}

for(pdf_path in pdf_paths)
{
 print(extract_shipping_label_info(pdf_path))
}

