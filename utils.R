# 数据库连接
db_connection <- function() {
  dbConnect(
    RMariaDB::MariaDB(),
    dbname = "inventory_system",
    host = "localhost",
    user = "root",
    password = "goldenbeanllc",
    encoding = "utf8mb4"
  )
}

# 导出条形码
export_barcode_pdf <- function(sku, page_width, page_height, unit = "in") {
  # Create a temporary file path for the PDF
  temp_dir <- tempdir() 
  if (unit == "cm") {
    # 1 cm = 0.393701 in 
    page_width <- page_width / 2.54  
    page_height <- page_height / 2.54
  }
  
  if(length(unique(sku)) > 1)
    pdf_path <- paste0(temp_dir, "/multiple_barcode")  # 组合文件夹路径和文件名
  
  if(length(unique(sku)) == 1)
    pdf_path <- paste0(temp_dir, "/", unique(sku), "_", length(sku), "_barcode")
  
  custom_create_PDF(Labels = sku, 
                    name = pdf_path, 
                    type = "linear", 
                    page_width = page_width, 
                    page_height = page_height,
                    numrow = 1, 
                    numcol = 1, 
                    width_margin = 0, 
                    height_margin = 0.05)
  
  return(paste0(pdf_path, ".pdf"))
}

# 获得图片宽高比
get_image_dimensions <- function(image_path) {
  img <- magick::image_read(image_path)
  info <- magick::image_info(img)
  list(width = info$width, height = info$height)
}

# 保存压缩后的图片到服务器
save_compressed_image <- function(file_path, output_dir, image_name, quality = 75, max_width = 500) {
  # 验证输入
  if (is.null(file_path) || !file.exists(file_path)) {
    stop("Invalid input file path.")
  }
  
  if (!dir.exists(output_dir)) {
    stop("Output directory does not exist.")
  }
  
  tryCatch({
    # 加载图片
    img <- magick::image_read(file_path)
    
    # 获取原始宽度
    img_info <- magick::image_info(img)
    original_width <- img_info$width
    
    # 判断是否需要缩放
    if (original_width > max_width) {
      img <- magick::image_scale(img, paste0(max_width, "x"))  # 缩放图片
    }
    
    # 转为 JPEG 格式
    img <- magick::image_convert(img, format = "jpeg")
    
    # 保存图片
    output_path <- file.path(output_dir, image_name)
    magick::image_write(img, path = output_path, quality = quality)
    
    return(output_path)
  }, error = function(e) {
    showNotification(paste("图片压缩失败:", e$message), type = "error")
    return(NULL)
  })
}

# 将 Base64 编码的图片数据解码并保存为实际图片文件
base64_decode_image <- function(base64_string, output_path) {
  # 提取 Base64 数据部分（去掉头部信息，如 "data:image/png;base64,"）
  base64_data <- gsub("^data:image/[^;]+;base64,", "", base64_string)
  
  # 解码 Base64 数据为二进制文件
  decoded_image <- base64enc::base64decode(base64_data)
  
  # 写入文件
  writeBin(decoded_image, output_path)
}

# 显示上传图片的预览
render_image_preview <- function(img_src, img_info, ns) {
  renderUI({
    div(
      tags$img(src = img_src, width = "200px",
               style = "border: 1px solid #ddd; border-radius: 8px; padding: 5px; margin-bottom: 10px;"),
      tags$p(
        style = "color: #007BFF; font-size: 14px;",
        paste0("分辨率: ", img_info$width, "x", img_info$height,
               ", 文件大小: ", round(img_info$filesize / 1024, 2), " KB")
      ),
      actionButton(ns("clear_image_preview"), "清除图片", icon = icon("trash"), class = "btn-danger", style = "margin-top: 10px;")
    )
  })
}

# 图片上传区文字提示
render_paste_prompt <- function() {
  renderUI({
    div("将图片粘贴到这里（Ctrl+V 或 Cmd+V）",
        style = "color: #888; font-size: 16px; font-style: italic;")
  })
}

# 保存图片（文件上传或粘贴）
process_image_upload <- function(sku, file_data = NULL, pasted_data = NULL, inventory_path = NULL, output_dir = "/var/www/images") {
  if (is.null(file_data) && is.null(pasted_data)) {
    # 没有上传图片，返回库存路径或 NULL
    if (!is.null(inventory_path)) {
      showNotification("使用库存中现有图片路径", type = "message")
      return(inventory_path)
    } else {
      showNotification("未上传图片", type = "warning")
      return(NA)
    }
  }
  
  # 生成唯一文件名
  unique_image_name <- paste0(sku, "_", format(Sys.time(), "%Y%m%d%H%M%S"), ".jpg")
  final_image_path <- file.path(output_dir, unique_image_name)
  
  tryCatch({
    if (!is.null(file_data)) {
      compressed_path <- save_compressed_image(
        file_path = file_data$datapath,
        output_dir = output_dir,
        image_name = unique_image_name
      )
    } else if (!is.null(pasted_data)) {
      compressed_path <- save_compressed_image(
        file_path = pasted_data$datapath,
        output_dir = output_dir,
        image_name = unique_image_name
      )
    }
    
    if (!is.null(compressed_path)) {
      showNotification("图片已成功压缩并存入数据库！", type = "message")
      return(compressed_path)
    } else {
      showNotification("图片压缩存储处理失败！", type = "error")
      return(NA)
    }
  }, error = function(e) {
    showNotification("图片上传时发生错误！", type = "error")
    return(NA)
  })
}

# 给物品生成唯一码
generate_unique_code <- function(input, length = 4) {
  # Validate input
  if (is.null(input) || input == "") {
    return(NULL)  
  }
  
  # Validate length parameter
  if (!is.numeric(length) || length <= 0) {
    stop("Length must be a positive integer.")
  }
  
  # Generate a hash value
  hash_value <- digest::digest(enc2utf8(input), algo = "sha512")
  
  # Extract numeric seed from the hash
  hash_numeric <- abs(sum(utf8ToInt(hash_value))) %% .Machine$integer.max
  
  set.seed(hash_numeric)
  
  # Generate a random alphanumeric code
  random_output <- paste0(sample(c(LETTERS, 0:9), length, replace = TRUE), collapse = "")
  return(random_output)
}

# 删除拼音音调
remove_tone <- function(text) {
  # 替换规则：音调字母 -> 无音调字母
  text <- stri_replace_all_regex(text, "ā|á|ǎ|à|a", "a")
  text <- stri_replace_all_regex(text, "ē|é|ě|è|e", "e")
  text <- stri_replace_all_regex(text, "ī|í|ǐ|ì|i", "i")
  text <- stri_replace_all_regex(text, "ō|ó|ǒ|ò|o", "o")
  text <- stri_replace_all_regex(text, "ū|ú|ǔ|ù|u", "u")
  text <- stri_replace_all_regex(text, "ǖ|ǘ|ǚ|ǜ|ü", "u")
  text <- stri_replace_all_regex(text, "Ā|Á|Ǎ|À|A", "A")
  text <- stri_replace_all_regex(text, "Ē|É|Ě|È|E", "E")
  text <- stri_replace_all_regex(text, "Ī|Í|Ǐ|Ì|I", "I")
  text <- stri_replace_all_regex(text, "Ō|Ó|Ǒ|Ò|O", "O")
  text <- stri_replace_all_regex(text, "Ū|Ú|Ǔ|Ù|U", "U")
  text <- stri_replace_all_regex(text, "Ǖ|Ǘ|Ǚ|Ǜ|Ü", "U")
  return(text)
}

# 定义空的库存表
create_empty_inventory <- function() {
  data.frame(
    SKU = character(),            # Product SKU
    Maker = character(),          # Supplier
    MajorType = character(),      # Major category
    MinorType = character(),      # Minor category
    ItemName = character(),       # Item name
    Quantity = numeric(),         # Quantity in stock
    ProductCost = numeric(),      # Product Cost
    ShippingCost = numeric(),     # Shipping Cost
    ItemImagePath = character(),  # Path to item image
    stringsAsFactors = FALSE      # Avoid factor columns
  )
}

# 映射物品表列名
map_column_names <- function(data, column_mapping) {
  # Get the mapped columns in the order of column_mapping
  mapped_columns <- names(column_mapping)[names(column_mapping) %in% names(data)]
  
  # If no matching columns, return an empty data frame
  if (length(mapped_columns) == 0) {
    return(data.frame())
  }
  
  # Select and reorder columns in the order of column_mapping
  data <- data[, mapped_columns, drop = FALSE]
  
  # Rename columns according to column_mapping
  data <- setNames(data, column_mapping[mapped_columns])
  
  return(data)
}

# 渲染物品表图片列
render_image_column <- function(image_column_data, 
                                host_url = host_url, 
                                placeholder = placeholder_50px_path) {
  
  sapply(image_column_data, function(img) {
    if (is.na(img) || img == "") {
      # 返回占位符图片
      paste0('<img src="', placeholder, '" loading="lazy" width="50" height="50" style="object-fit:cover;"/>')
    } else {
      # 拼接完整的图片 URL
      img_path <- paste0(host_url, "/images/", basename(img))
      paste0('<img src="', img_path, '" loading="lazy" width="50" height="50" style="object-fit:cover;"/>')
    }
  }, USE.NAMES = FALSE)
}

# 更新物品状态
update_status <- function(con, unique_id, new_status = NULL, defect_status = NULL, 
                          shipping_method = NULL, clear_shipping_method = FALSE, 
                          refresh_trigger = NULL, update_timestamp = TRUE, 
                          clear_status_timestamp = NULL) {
  # 检查 UniqueID 是否存在
  current_status <- dbGetQuery(con, paste0(
    "SELECT Status, updated_at FROM unique_items WHERE UniqueID = '", unique_id, "'"
  ))
  
  if (nrow(current_status) == 0) {
    showNotification("UniqueID not found", type = "error")
    return()
  }
  
  # 如果 new_status 发生变化，记录状态历史
  if (!is.null(new_status) && current_status$Status != new_status) {
    dbExecute(con, paste0(
      "INSERT INTO item_status_history (UniqueID, previous_status, previous_status_timestamp) VALUES ('",
      unique_id, "', '", current_status$Status, "', '", current_status$updated_at, "')"
    ))
  }
  
  # 构建动态 SQL 子句
  set_clauses <- c(
    if (!is.null(new_status)) {
      # 检查 new_status 的合法性
      if (!new_status %in% names(status_columns)) {
        showNotification("Invalid status provided", type = "error")
        return()
      }
      # 获取时间戳列
      timestamp_column <- status_columns[[new_status]]
      timestamp_update <- if (update_timestamp) paste0(timestamp_column, " = NOW()") else NULL
      c("Status = ?", timestamp_update)
    } else {
      NULL
    },
    if (!is.null(defect_status)) "Defect = ?" else NULL,
    if (!is.null(shipping_method)) "IntlShippingMethod = ?" else NULL,
    if (clear_shipping_method) "IntlShippingMethod = NULL" else NULL, # 显式清空运输方式
    if (!is.null(clear_status_timestamp)) {
      # 检查 clear_status_timestamp 的合法性
      if (!clear_status_timestamp %in% names(status_columns)) {
        showNotification("Invalid clear_status_timestamp provided", type = "error")
        return()
      }
      paste0(status_columns[[clear_status_timestamp]], " = NULL") # 清空指定列
    } else {
      NULL
    }
  )
  
  # 拼接 SET 子句
  set_clause <- paste(set_clauses[!is.null(set_clauses)], collapse = ", ")
  
  # 如果没有任何更新内容，提示错误并返回
  if (set_clause == "") {
    showNotification("No updates provided", type = "error")
    return()
  }
  
  # 构建 SQL 查询
  query <- paste0(
    "UPDATE unique_items SET ", set_clause, " WHERE UniqueID = ?"
  )
  
  # 构建参数列表
  params <- c(
    if (!is.null(new_status)) list(new_status) else NULL,
    if (!is.null(defect_status)) list(defect_status) else NULL,
    if (!is.null(shipping_method)) list(shipping_method) else NULL,
    list(unique_id)  # 唯一 ID 是必须的
  )
  
  # 展平参数列表
  params <- unlist(params)
  
  # 执行 SQL 更新
  dbExecute(con, query, params = params)
  
  # 触发刷新
  if (!is.null(refresh_trigger)) {
    refresh_trigger(!refresh_trigger())
  }
}

# 渲染物品信息（入库，出库页）
renderItemInfo <- function(output, output_name, item_info = NULL, img_path, count_label = "待入库数", count_field = "PendingQuantity", img_height = "300px", image_only = FALSE) {
  if (is.null(item_info) || nrow(item_info) == 0) item_info <- data.frame(ItemName = "", Maker = "", MajorType = "", MinorType = "", PendingQuantity = 0, AvailableForOutbound = 0, stringsAsFactors = FALSE)
  count_value <- item_info[[count_field]][1]
  
  if(image_only) {
    output[[output_name]] <- renderUI({
      div(style="text-align: center;",
          img(src=img_path, height="600px", style="border: 2px solid #ddd; border-radius: 8px; box-shadow: 0px 4px 8px rgba(0, 0, 0, 0.1);"),
          tags$table(style="margin: 10px auto; border-collapse: collapse;",
                     tags$tr(tags$td(tags$strong(count_label), style="padding: 8px 10px; vertical-align: top; font-size: 30px;"),
                             tags$td(tags$span(ifelse(count_value == 0, paste0("无", count_label), count_value), style="color: #FF4500; font-weight: bold; font-size: 30px;")))
                     )
          )
    })
  } else {
    output[[output_name]] <- renderUI({
      fluidRow(
        column(6, div(style = "text-align: center;", img(src = img_path, height = img_height, style = "border: 2px solid #ddd; border-radius: 8px; box-shadow: 0px 4px 8px rgba(0, 0, 0, 0.1);"))),
        column(6, div(style = "padding: 20px; background-color: #f7f7f7; border: 1px solid #e0e0e0; border-radius: 8px; box-shadow: 0px 4px 8px rgba(0, 0, 0, 0.1); height: " %||% img_height,
                      tags$h4("商品信息", style = "border-bottom: 3px solid #4CAF50; margin-bottom: 15px; padding-bottom: 8px; font-weight: bold; color: #333;"),
                      tags$table(style = "width: 100%; font-size: 16px; color: #444;",
                                 tags$tr(tags$td(tags$strong("商品名:"), style = "padding: 8px 10px; width: 120px; vertical-align: top;"), tags$td(tags$span(item_info$ItemName[1], style = "color: #4CAF50; font-weight: bold;"))),
                                 tags$tr(tags$td(tags$strong("供应商:"), style = "padding: 8px 10px; vertical-align: top;"), tags$td(tags$span(item_info$Maker[1], style = "color: #4CAF50;"))),
                                 tags$tr(tags$td(tags$strong("大类:"), style = "padding: 8px 10px; vertical-align: top;"), tags$td(tags$span(item_info$MajorType[1], style = "color: #4CAF50;"))),
                                 tags$tr(tags$td(tags$strong("小类:"), style = "padding: 8px 10px; vertical-align: top;"), tags$td(tags$span(item_info$MinorType[1], style = "color: #4CAF50;"))),
                                 tags$tr(tags$td(tags$strong(count_label), style = "padding: 8px 10px; vertical-align: top;"), tags$td(tags$span(ifelse(count_value == 0, paste0("无", count_label), count_value), style = "color: #FF4500; font-weight: bold;")))
                      )
        ))
      )
    })
  }
}

# 状态渲染样式
apply_dynamic_styles <- function(table, column_names) {
  # 库存态样式
  if ("库存态" %in% column_names) {
    table <- table %>%
      formatStyle(
        "库存态",
        backgroundColor = styleEqual(
          c("采购", "国内入库", "国内售出", "国内出库", "美国入库", "美国调货", "美国发货", "交易完毕"),
          c("lightgray", "#c7e89b", "#9ca695", "#46a80d", "#6f52ff", "#529aff", "#faf0d4", "#f4c7fc")
        ),
        color = styleEqual(
          c("采购", "国内入库", "国内售出", "国内出库", "美国入库", "美国调货", "美国发货", "交易完毕"),
          c("black", "black", "black", "white", "white", "black", "black", "black")
        )
      )
  }
  
  # 瑕疵态样式
  if ("瑕疵态" %in% column_names) {
    table <- table %>%
      formatStyle(
        "瑕疵态",
        backgroundColor = styleEqual(
          c("未知", "无瑕", "瑕疵", "修复"),
          c("darkgray", "green", "red", "orange")
        ),
        color = styleEqual(
          c("未知", "无瑕", "瑕疵", "修复"),
          c("black", "white", "white", "white")
        )
      )
  }
  
  # 国际运输样式
  if ("国际运输" %in% column_names) {
    table <- table %>%
      formatStyle(
        "国际运输",
        backgroundColor = styleEqual(
          c("空运", "海运"),
          c("lightblue", "darkblue")
        ),
        color = styleEqual(
          c("空运", "海运"),
          c("black", "white")
        )
      )
  }
  
  # 转账类别样式
  if ("转账类别" %in% column_names) {
    table <- table %>%
      formatStyle(
        "转账类别",
        backgroundColor = styleEqual(
          c("采购", "税费", "杂费", "工资", "债务", "社保", "图解", "其他"), 
          c("#FFDDC1", "#FFD700", "#87CEEB", "#98FB98", "#FF6347", "#DDA0DD", "#f1f797", "#D3D3D3")
        ),
        color = styleEqual(
          c("采购", "税费", "杂费", "工资", "债务", "社保", "图解", "其他"), 
          c("black", "black", "black", "black", "black", "black", "black", "black")
        )    
      )
  }
  
  return(table)
}

# 删除匹配的预定单备注物品
remove_preorder_item_note <- function(str, target) {
  # 定义正则表达式，捕获【预定物品】和；之间的内容
  pattern <- "【预定物品】([^；]+)；"
  
  # 提取匹配结果
  match_info <- regexec(pattern, str, perl = TRUE)
  matches <- regmatches(str, match_info)
  
  # 判断是否找到了匹配字段
  if (length(matches[[1]]) > 1) {
    # 捕获组中的内容，例如 "测试商品，测试"
    field_content <- matches[[1]][2]
    
    # 按中文逗号分割成物品列表
    items <- unlist(strsplit(field_content, "，"))
    
    # 查找第一个完全匹配目标物品的索引
    idx <- match(target, items)
    
    # 如果找到了，则删除该项
    if (!is.na(idx)) {
      items <- items[-idx]
    }
    
    # 重新拼接物品列表（没有物品则为空字符串）
    new_field_content <- paste(items, collapse = "，")
    
    # 用新字段替换原始字符串中的【预定物品】字段
    new_str <- sub(pattern, paste0("【预定物品】", new_field_content, "；"), str, perl = TRUE)
    
    return(new_str)
  } else {
    # 如果没有找到【预定物品】字段，返回原字符串
    return(str)
  }
}

# 获取指定账户的余额
get_balance <- function(account_type, con) {
  # 查询指定账户的最新余额（按时间排序）
  query <- "
    SELECT Balance
    FROM transactions
    WHERE AccountType = ?
    ORDER BY TransactionTime DESC
    LIMIT 1
  "
  result <- dbGetQuery(con, query, params = list(account_type))
  
  # 如果没有记录，则余额为 0
  balance <- result$Balance[1] %||% 0  # 使用 %||% 防止 NULL
  return(as.numeric(balance))  # 返回数值型余额
}

# 更新账户余额显示
updateAccountOverview <- function(output, con) {
  output$salary_balance <- renderText({
    sprintf("¥%.2f", get_balance("工资卡", con))
  })
  
  output$dollar_balance <- renderText({
    sprintf("¥%.2f", get_balance("美元卡", con))
  })
  
  output$purchase_balance <- renderText({
    sprintf("¥%.2f", get_balance("买货卡", con))
  })
  
  output$general_balance <- renderText({
    sprintf("¥%.2f", get_balance("一般户卡", con))
  })
  
  output$total_balance <- renderText({
    total <- sum(
      get_balance("工资卡", con),
      get_balance("美元卡", con),
      get_balance("买货卡", con),
      get_balance("一般户卡", con),
      na.rm = TRUE
    )
    sprintf("¥%.2f", total)
  })
}

# 刷新账目记录
refreshTransactionTable <- function(account_type, cache_env, transaction_table_hash, output, con) {
  table_map <- list(
    "工资卡" = "salary_card_table",
    "美元卡" = "dollar_card_table",
    "买货卡" = "purchase_card_table",
    "一般户卡" = "general_card_table"
  )
  
  # 检查账户类型是否有效
  if (account_type %in% names(table_map)) {
    # 获取数据
    data <- fetchAndFormatTransactionData(account_type, cache_env, con)
    
    # 计算数据的哈希值
    current_hash <- digest::digest(data)
    
    # 确定哈希缓存键
    hash_key <- switch(
      account_type,
      "工资卡" = "salary",
      "美元卡" = "dollar",
      "买货卡" = "purchase",
      "一般户卡" = "general"
    )
    
    # 如果哈希值未变化，则跳过刷新
    if (!is.null(transaction_table_hash[[hash_key]]) && 
        transaction_table_hash[[hash_key]] == current_hash) {
      return()
    }
    
    # 更新哈希缓存
    transaction_table_hash[[hash_key]] <- current_hash

    # 更新输出
    output[[table_map[[account_type]]]] <- renderDT({
      # 渲染表格
      table_result <- render_table_with_images(
        data = data,
        column_mapping = transaction_common_columns,
        image_column = "TransactionImagePath",  # 图片列名
        options = modifyList(table_default_options, list(scrollY = "600px", searching = TRUE))
      )
      
      table <- apply_dynamic_styles(table_result$datatable, table_result$column_names)
      table
    }, server = TRUE)
  } else {
    showNotification("无效的账户类型！", type = "error")
  }
}

# 获取格式化好的账目记录表
fetchAndFormatTransactionData <- function(account_type, cache_env, con) {
  # 初始化缓存结构
  if (is.null(cache_env$transaction_data)) {
    cache_env$transaction_data <- list()
  }
  if (is.null(cache_env$transaction_metadata)) {
    cache_env$transaction_metadata <- list()
  }
  
  # 获取账户的最后更新时间
  last_updated_query <- "
    SELECT MAX(updated_at) AS LastUpdated
    FROM transactions
    WHERE AccountType = ?
  "
  last_updated_db <- dbGetQuery(con, last_updated_query, params = list(account_type))$LastUpdated[1]
  
  # 检查缓存是否有效
  cached_metadata <- cache_env$transaction_metadata[[account_type]]
  if (!is.null(cached_metadata)) {
    cached_last_updated <- cached_metadata$last_updated
    
    # 如果更新时间未改变，返回缓存的数据
    if (!is.na(last_updated_db) && last_updated_db == cached_last_updated) {
      return(cache_env$transaction_data[[account_type]])
    }
  }
  
  # 从数据库获取最新数据
  query <- "SELECT * FROM transactions WHERE AccountType = ? ORDER BY TransactionTime DESC"
  data <- dbGetQuery(con, query, params = list(account_type))
  
  # 格式化数据
  formatted_data <- data %>%
    mutate(
      TransactionTime = format(as.POSIXct(TransactionTime), "%Y-%m-%d %H:%M:%S"),
      AmountIn = ifelse(Amount > 0, sprintf("%.2f", Amount), NA),
      AmountOut = ifelse(Amount < 0, sprintf("%.2f", abs(Amount)), NA),
      Balance = sprintf("%.2f", Balance)
    )
  
  rownames(formatted_data) <- NULL
  
  # 更新缓存
  cache_env$transaction_data[[account_type]] <- formatted_data
  cache_env$transaction_metadata[[account_type]] <- list(
    last_updated = last_updated_db
  )
  
  return(formatted_data)
}

# 从账目表中获取信息填入左侧
fetchInputFromTable <- function(account_type, selected_row, cache_env, con, session) {
  if (!is.null(selected_row)) {
    # 获取指定账户的数据
    data <- fetchAndFormatTransactionData(account_type, cache_env, con)
    
    # 检查选中行是否有效
    if (selected_row > nrow(data) || selected_row <= 0 || nrow(data) == 0) {
      showNotification("选中的行无效或数据为空！", type = "error")
      return(NULL)
    }
    
    # 提取选中行的数据
    selected_data <- data[selected_row, ]

    # 更新输入框内容
    updateNumericInput(session, "amount", value = ifelse(!is.na(selected_data$AmountIn), 
                                                         as.numeric(selected_data$AmountIn), 
                                                         as.numeric(selected_data$AmountOut)))
    updateRadioButtons(session, "transaction_type", selected = ifelse(!is.na(selected_data$AmountIn), "in", "out"))
    updateDateInput(session, "custom_date", value = as.Date(selected_data$TransactionTime))
    updateTimeInput(session, "custom_time", value = format(as.POSIXct(selected_data$TransactionTime), "%H:%M:%S"))
    updateTextAreaInput(session, "remarks", value = selected_data$Remarks)
    
    # 更新 transaction_category 下拉菜单
    if (!is.null(selected_data$TransactionType) && selected_data$TransactionType %in% c("采购", "税费", "杂费", "工资", "债务", "社保", "其他")) {
      updateSelectInput(session, "transaction_category", selected = selected_data$TransactionType)
    } else {
      updateSelectInput(session, "transaction_category", selected = "其他")  # 如果数据为空，默认设置为 "其他"
    }
    
    # 返回 TransactionID 和 TransactionImagePath
    return(list(
      TransactionID = selected_data$TransactionID,
      TransactionImagePath = selected_data$TransactionImagePath
    ))
  }
  
  return(NULL)  # 未选中行时返回 NULL
}

# 从分页上获取当前账户名
getAccountType <- function(input) {
  switch(
    input$transaction_tabs,
    "工资卡" = "工资卡",
    "美元卡" = "美元卡",
    "买货卡" = "买货卡",
    "一般户卡" = "一般户卡",
    NULL
  )
}

# 转账截图点击看大图
handleTransactionImageClick <- function(account_type, input_table, image_col_index, input, cache_env, con, session) {
  observeEvent(input[[paste0(input_table, "_cell_clicked")]], {
    info <- input[[paste0(input_table, "_cell_clicked")]]
    
    # 检查是否点击了图片列
    if (!is.null(info) && !is.null(info$col) && !is.null(info$row)) {
      if (info$col == image_col_index) {  # 图片列的索引
        # 获取点击的图片路径
        selected_data <- fetchInputFromTable(account_type, input[[paste0(input_table, "_rows_selected")]], cache_env, con, session)
        img_path <- selected_data$TransactionImagePath
        
        req(img_path)  # 确保图片路径存在且不为空
        
        img_host_path <- paste0(host_url, "/images/", basename(img_path))
        
        # 弹出模态框显示图片
        showModal(modalDialog(
          title = "转账截图预览",
          tags$div(
            style = "overflow: auto; max-height: 700px; text-align: center;",
            tags$img(
              src = img_host_path,
              style = "max-width: 100%; height: auto; display: inline-block;"
            )
          ),
          size = "l",
          easyClose = TRUE,
          footer = NULL
        ))
      }
    }
  })
}

# 重置回登记模式
resetToCreateMode <- function(is_update_mode, selected_TransactionID, selected_TransactionImagePath, session) {
  is_update_mode(FALSE)
  selected_TransactionID(NULL)
  selected_TransactionImagePath(NULL)
  
  updateActionButton(session, "record_transaction", label = "登记", icon = icon("save"))
}

# 重置账务登记表
resetTransactionForm <- function(session, image_transactions) {
  updateNumericInput(session, "amount", value = 0)  # 重置金额
  updateRadioButtons(session, "transaction_type", selected = "out")  # 重置为“转出”
  updateDateInput(session, "custom_date", value = Sys.Date())  # 重置为当前日期
  updateTimeInput(session, "custom_time", value = format(Sys.time(), "%H:%M:%S"))  # 重置为当前时间
  updateTextAreaInput(session, "remarks", value = "")  # 清空备注
  image_transactions$reset()  # 重置图片上传组件
}

# 重置资产转移表
resetTransferForm <- function(session, image_transfer) {
  updateNumericInput(session, "transfer_amount", value = 0)  # 重置金额
  updateSelectInput(session, "from_account", selected = "美元卡")
  updateSelectInput(session, "to_account", selected = "工资卡")
  updateTextAreaInput(session, "transfer_remarks", value = "")  # 清空备注
  image_transfer$reset()  # 重置图片上传组件
}

# 更新余额
update_balance <- function(account_type, con) {
  # 初始化会话变量
  dbExecute(con, "SET @current_balance = 0;")
  
  # 执行更新语句
  query <- paste0(
    "UPDATE transactions ",
    "SET Balance = (@current_balance := @current_balance + Amount) ",
    "WHERE AccountType = '", account_type, "' ",
    "ORDER BY TransactionTime;"
  )
  
  dbExecute(con, query)
  showNotification("余额记录已重新计算", type = "message")
}

# 生成账务ID
generate_transaction_id <- function(account_type, amount, remarks, transaction_datetime) {
  # 将输入合并为字符串
  input_string <- paste(account_type, amount, remarks, transaction_datetime, sep = "|")
  
  # 生成 SHA256 哈希值并截取前 12 位
  transaction_id <- substr(digest::digest(input_string, algo = "sha256"), 1, 12)
  
  return(transaction_id)
}

# 从运单PDF提取收件人和运单号信息
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
  
  # **找到 "SHIP TO:" 关键词并提取名字**
  ship_to_line <- lines[stri_detect_regex(lines, "SHIP TO:", case_insensitive = FALSE)]
  if (length(ship_to_line) > 0) {
    # 直接提取 "SHIP TO:" 后的内容
    name_match <- stri_match_first_regex(ship_to_line[1], "SHIP TO:\\s*(.*)", case_insensitive = TRUE)
    if (!is.na(name_match[2]) && nchar(name_match[2]) > 0) {
      customer_name <- stri_trim_both(name_match[2])  # 获取 "SHIP TO:" 之后的内容
    }
  } else if (length(usps_line_index) > 0) {
    name_line_indices <- usps_line_index - c(3:5)
    name_line_indices <- name_line_indices[name_line_indices > 0]  # 确保索引有效
    
    potential_names <- lines[name_line_indices]
    
    # 删除包含地址后缀关键词的行
    address_keywords <- c("APT", "SUITE", "UNIT", "AVE", "PKWY", "ST", "BLVD", "DR", "RD", "LN", "CT", "WAY", "PL", "HWY")
    regex_pattern <- paste0("(?i)\\b(", paste(address_keywords, collapse = "|"), ")(\\s+", paste(address_keywords, collapse = "|"), ")?\\b")
    potential_names <- potential_names[!stri_detect_regex(potential_names, regex_pattern, case_insensitive = TRUE)]
    
    # 移除特殊符号行
    special_keywords <- c("#")
    potential_names <- potential_names[!stri_detect_regex(potential_names, special_keywords, case_insensitive = TRUE)]
    
    # 过滤掉 "词组+数字"
    potential_names <- potential_names[!stringi::stri_detect_regex(potential_names, "\\b[A-Za-z]+\\s?\\d+\\b")]
    
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
          potential_name, "^([A-Z]+|[A-Z][a-z]+|[a-z]+)(\\s([A-Z]+|[A-Z][a-z]+|[a-z]+)){0,3}$"
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

# 添加新物品记录（采购）
add_new_inventory_record <- function(con, sku, maker, major_type, minor_type, item_name, quantity, image_path = NULL) {
  tryCatch({
    sku <- trimws(sku)  # 清理空格
    # 检查是否已存在该 SKU
    existing_item <- dbGetQuery(con, "SELECT * FROM inventory WHERE SKU = ?", params = list(sku))
    
    if (nrow(existing_item) > 0) {
      return(NULL)  # 提前返回，表示无需新增记录
    }
    
    # 如果 SKU 不存在，插入新的库存记录
    dbExecute(con, "INSERT INTO inventory 
                    (SKU, Maker, MajorType, MinorType, ItemName, Quantity, ItemImagePath) 
                    VALUES (?, ?, ?, ?, ?, ?, ?)",
              params = list(
                sku, maker, major_type, minor_type, item_name, quantity, image_path
              ))
    
    showNotification(paste("新商品成功登记! SKU:", sku, ", 商品名:", item_name), type = "message")
    return(TRUE)
  }, error = function(e) {
    showNotification(paste("添加新库存记录时发生错误：", e$message), type = "error")
    return(FALSE)
  })
}

#从订单备注中提取预订单供应商和预定物品
extract_items_and_suppliers <- function(order_notes) {
  supplier_pattern <- "【供应商】(.*?)\\s"
  items_pattern <- "【预定物品】(.*?)(；|$)"
  
  supplier_match <- regmatches(order_notes, regexpr(supplier_pattern, order_notes, perl = TRUE))
  items_match <- regmatches(order_notes, regexpr(items_pattern, order_notes, perl = TRUE))
  
  if (length(supplier_match) > 0 && length(items_match) > 0) {
    supplier <- sub(supplier_pattern, "\\1", supplier_match, perl = TRUE)
    items_str <- sub(items_pattern, "\\1", items_match, perl = TRUE)
    items_list <- unlist(strsplit(items_str, "，"))
    items_list <- trimws(items_list)  # 去除前后空白
    return(data.frame(Item = items_list, Supplier = supplier, stringsAsFactors = FALSE))
  } else {
    return(data.frame(Item = character(0), Supplier = character(0), stringsAsFactors = FALSE))
  }
}

# 重置订单表
reset_order_form <- function(session, image_module) {
  updateTextInput(session, "order_id", value = "")
  updateSelectInput(session, "platform", selected = "")
  updateNumericInput(session, "transaction_amount", value = "")
  shinyjs::runjs("$('#transaction_amount').attr('placeholder', '成交额（$）');")  # 重新添加 placeholder
  updateTextInput(session, "customer_name", value = "")
  updateTextInput(session, "customer_netname", value = "")
  updateSelectizeInput(session, "preorder_supplier", selected = character(0))
  updateTextAreaInput(session, "preorder_item_name", value = "")
  shinyjs::runjs("$('#preorder_img').hide();")
  updateCheckboxInput(session, "is_preorder", value = FALSE)
  updateCheckboxInput(session, "is_transfer_order", value = FALSE)
  updateTextInput(session, "tracking_number", value = "")
  shinyjs::reset("shiplabel_pdf_upload")
  shinyjs::enable("tracking_number")
  image_module$reset()
  updateTextAreaInput(session, "order_notes", value = "")
}

# 带优先级的货架数据
get_shelf_items <- function(data, sku, valid_status = c("美国入库", "国内出库", "国内入库"),
                            defect_filter = "瑕疵", status_priority = c("美国入库" = 1, "国内出库" = 2, "国内入库" = 3),
                            sort_order = "up") {
  # 检查是否提供了优先级
  if (is.null(status_priority) || length(status_priority) == 0) {
    stop("请指定有效的状态优先级！")
  }
  
  # 过滤符合条件的物品，并自动排除 "国内出库" 且未进入国际物流的物品
  result <- data %>%
    filter(
      SKU == sku, 
      Status %in% valid_status, 
      Defect != defect_filter,
      !(Status == "国内出库" & (is.na(IntlTracking) | IntlTracking == ""))
    ) %>%
    select(SKU, UniqueID, ItemName, Status, Defect, ProductCost, ItemImagePath, IntlTracking) %>%
    mutate(StatusPriority = case_when(
      Status %in% names(status_priority) ~ status_priority[Status],
      TRUE ~ max(unlist(status_priority)) + 1  # 默认最低优先级
    )) 
  
  # 根据 sort_order 确定排序方式
  if (sort_order == "up") {
    result <- result %>% arrange(StatusPriority, ProductCost)  # 正序
  } else if (sort_order == "down") {
    result <- result %>% arrange(desc(StatusPriority), ProductCost)  # 逆序
  } else {
    stop("无效的排序方式，请选择 'up' 或 'down'！")
  }
  
  # 如果未找到符合条件的物品，返回 NULL
  if (nrow(result) == 0) {
    return(NULL)
  }
  
  return(result)
}

# 渲染订单信息
renderOrderInfo <- function(output, output_name, matching_orders, clickable = TRUE) {
  # 如果没有物品，返回提示信息
  if (is.null(matching_orders) || nrow(matching_orders) == 0) {
    output[[output_name]] <- renderUI({
      div("没有找到匹配的订单")
    })
    return()
  }
  
  output[[output_name]] <- renderUI({
    # 动态渲染订单卡片
    order_cards <- lapply(1:nrow(matching_orders), function(i) {
      order_info <- matching_orders[i, ]
      
      # 图片路径
      img_path <- ifelse(
        is.na(order_info$OrderImagePath) || order_info$OrderImagePath == "",
        placeholder_300px_path,
        paste0(host_url, "/images/", basename(order_info$OrderImagePath))
      )
      
      # 动态添加蒙版和打勾图标
      mask_overlay <- if (order_info$OrderStatus == "装箱") {
        div(
          style = "position: absolute; top: 0; left: 0; width: 100%; height: 100%; 
                  background: rgba(128, 128, 128, 0.6); display: flex; justify-content: center; align-items: center;
                  border-radius: 8px; z-index: 2;",
          tags$div(
            style = "width: 50px; height: 50px; background: #28a745; border-radius: 50%; display: flex; 
                     justify-content: center; align-items: center;",
            tags$i(class = "fas fa-check", style = "color: white; font-size: 24px;")  # 绿色勾
          )
        )
      } else {
        NULL
      }
      
      # 如果卡片不可点击，不设置 onclick 事件
      onclick_script <- if (clickable) {
        sprintf("Shiny.setInputValue('selected_order_id', '%s', {priority: 'event'})", order_info$OrderID)
      } else {
        NULL
      }
      
      # 渲染订单卡片
      div(
        id = paste0("order_card_", order_info$OrderID),  # 设置唯一 ID
        class = "order-card",
        style = paste0(
          "position: relative; display: inline-block; width: 550px; height: 310px; ",
          "background-color: #ffffff; border: 1px solid #ddd; border-radius: 8px; ",
          "box-shadow: 0px 4px 8px rgba(0, 0, 0, 0.1); margin-right: 15px; ",
          ifelse(clickable, "cursor: pointer;", "cursor: default;")  # 动态设置鼠标样式
        ),
        
        onclick = onclick_script,  # 动态设置点击事件
        
        mask_overlay,  # 动态显示蒙版
        
        # 卡片内容
        div(
          style = "display: flex; gap: 10px; height: 100%;",
          
          # 图片部分
          div(
            style = "flex: 1; text-align: center; display: flex; align-items: center; justify-content: center;",
            img(
              src = img_path,
              style = "height: 280px; max-width: 100%; object-fit: cover; border-radius: 8px;"
            )
          ),
          
          # 订单信息部分
          div(
            style = "flex: 1; overflow-y: auto; padding: 10px;",
            tags$table(
              style = "width: 100%; font-size: 14px; color: #444;",
              tags$tr(
                tags$td(tags$strong("订单号:"), style = "padding: 5px; vertical-align: top;"),
                tags$td(tags$span(order_info$OrderID, style = "color: #007BFF; font-weight: bold;"))
              ),
              tags$tr(
                tags$td(tags$strong("运单号:"), style = "padding: 5px; vertical-align: top;"),
                tags$td(tags$span(order_info$UsTrackingNumber, style = "color: #007BFF; font-weight: bold;"))
              ),
              tags$tr(
                tags$td(tags$strong("顾客姓名:"), style = "padding: 5px; vertical-align: top;"),
                tags$td(tags$span(order_info$CustomerName, style = "color: #007BFF;"))
              ),
              tags$tr(
                tags$td(tags$strong("平台:"), style = "padding: 5px; vertical-align: top;"),
                tags$td(tags$span(order_info$Platform, style = "color: #007BFF;"))
              ),
              tags$tr(
                tags$td(tags$strong("备注:"), style = "padding: 5px; vertical-align: top;"),
                tags$td(
                  div(
                    style = "color: #FF0000; background-color: #FFFFCC; padding: 6px; white-space: normal; word-wrap: break-word; font-size: 14px; border-radius: 4px;",
                    order_info$OrderNotes
                  )
                )
              ),
              tags$tr(
                tags$td(tags$strong("状态:"), style = "padding: 5px; vertical-align: top;"),
                tags$td(tags$span(order_info$OrderStatus, style = "color: #007BFF;"))
              )
            )
          )
        )
      )
    })
    
    div(
      style = "display: flex; gap: 15px; flex-wrap: nowrap; overflow-x: auto; padding: 10px;",
      do.call(tagList, order_cards)  # 返回卡片列表
    )
  })
}

# 生成运单ID（美国发货）
generate_order_id <- function(tracking_number, unique_ids) {
  # 验证 tracking_number 是否有效
  if (is.null(tracking_number) || tracking_number == "" || !is.character(tracking_number)) {
    stop("无效的运单号：tracking_number 必须是非空字符串。")
  }
  
  # 验证 unique_ids 是否为非空向量
  if (is.null(unique_ids) || length(unique_ids) == 0 || any(unique_ids == "")) {
    stop("无效的 unique_ids：必须是非空字符串向量。")
  }
  
  # 将运单号和所有 UniqueID 拼接成字符串
  input_string <- paste0(tracking_number, paste(unique_ids, collapse = ""))
  
  # 生成 SHA-256 哈希值
  hashed <- digest::digest(input_string, algo = "sha256", serialize = FALSE)
  
  # 截取前九位作为订单 ID
  order_id <- toupper(substr(hashed, 1, 9))
  
  return(order_id)
}

# 更新供应商下拉选项函数
update_maker_choices <- function(session, input_id, maker_data) {
  if (is.null(maker_data) || nrow(maker_data) == 0) {
    updateSelectizeInput(session, input_id, choices = NULL, server = TRUE)
  } else {
    choices <- c("", setNames(maker_data$Maker, paste0(maker_data$Maker, "(", maker_data$Pinyin, ")")))
    updateSelectizeInput(session, input_id, choices = choices, selected = "", server = TRUE)
  }
}

# 生成SKU
generate_sku <- function(item_type_data, major_type, minor_type, item_name, maker) {
  if (is.null(major_type) || major_type == "" || 
      is.null(minor_type) || minor_type == "" || 
      is.null(item_name) || item_name == "" || 
      is.null(maker) || maker == "") {
    return("")  # Return empty if any input is invalid
  }
  
  # Get MajorTypeSKU and MinorTypeSKU
  major_type_sku <- item_type_data %>%
    filter(MajorType == major_type) %>%
    pull(MajorTypeSKU) %>%
    unique()
  
  minor_type_sku <- item_type_data %>%
    filter(MinorType == minor_type) %>%
    pull(MinorTypeSKU) %>%
    unique()
  
  if (length(major_type_sku) == 0 || length(minor_type_sku) == 0) {
    return("")  # Return empty if no matching SKUs are found
  }
  
  # Generate unique code
  unique_code <- generate_unique_code(paste(item_name, maker, sep = "_"), length = 4)
  
  # Create the SKU in the format: MajorTypeSKU-MinorTypeSKU-UniqueCode
  paste0(major_type_sku, "-", minor_type_sku, "-", unique_code)
}

# 空的箱子与货架
create_empty_shelf_box <- function() {
  data.frame(
    SKU = character(),
    UniqueID = character(),
    ItemName = character(),
    Status = character(),
    Defect = character(),
    ProductCost = numeric(),
    ItemImagePath = character(),
    stringsAsFactors = FALSE
  )
}

# 渲染带图片的表
render_table_with_images <- function(data, 
                                     column_mapping, 
                                     selection = "single",
                                     image_column = NULL,
                                     options = table_default_options) {
  if (!is.null(image_column) && nrow(data) > 0) {
    # Render the image column
    data[[image_column]] <- render_image_column(data[[image_column]], host_url)
  }
  
  # Map column names for user-friendly display
  if (!is.null(column_mapping)) {
    data <- map_column_names(data, column_mapping)
  }
  
  # 获取更新后的列名
  updated_column_names <- colnames(data)
  
  # 返回列表，包括 datatable 对象和列名
  list(
    datatable = datatable(
      data,
      escape = FALSE,  # Disable HTML escaping to allow rendering of images
      selection = selection,
      rownames = FALSE,
      options = options
    ),
    column_names = updated_column_names
  )
}

# 更新订单ID
update_order_id <- function(con, unique_ids, order_id) {
  tryCatch({
    # 检查输入的 unique_ids 是否为空
    if (is.null(unique_ids) || length(unique_ids) == 0) {
      showNotification("未提供需要更新的物品 ID！", type = "error")
      return()
    }
    
    # 如果 order_id 为 NULL，清空指定物品的订单号
    if (is.null(order_id)) {
      # 构造 SQL 更新语句，批量清空 OrderID
      query <- sprintf("UPDATE unique_items SET OrderID = NULL WHERE UniqueID IN (%s)", 
                       paste(shQuote(unique_ids), collapse = ", "))
      
      # 执行 SQL 更新
      dbExecute(con, query)
      
      # 成功提示
      showNotification("订单号已成功清空！", type = "message")
    } else if (order_id == "") {
      showNotification("订单号不能为空字符串！", type = "error")
      return()
    } else {
      # 构造 SQL 更新语句，批量更新 OrderID
      query <- sprintf("UPDATE unique_items SET OrderID = '%s' WHERE UniqueID IN (%s)", 
                       order_id, paste(shQuote(unique_ids), collapse = ", "))
      
      # 执行 SQL 更新
      dbExecute(con, query)
      
      # 成功提示
      showNotification("订单号已成功更新！", type = "message")
    }
  }, error = function(e) {
    # 错误提示
    showNotification(paste("批量更新订单号时发生错误：", e$message), type = "error")
  })
}

# 更新订单状态
update_order_status <- function(order_id, new_status, updated_notes = NULL, refresh_trigger = NULL, con) {
  tryCatch({
    if (is.null(updated_notes)) {
      # 仅更新状态
      dbExecute(
        con,
        "UPDATE orders
         SET OrderStatus = ?, 
             updated_at = CURRENT_TIMESTAMP 
         WHERE OrderID = ?",
        params = list(new_status, order_id)
      )
      showNotification(sprintf("订单 %s 状态更新为 '%s'！", order_id, new_status), type = "message")
    } else {
      # 同时更新状态和备注
      dbExecute(
        con,
        "UPDATE orders
         SET OrderStatus = ?, 
             OrderNotes = ?, 
             updated_at = CURRENT_TIMESTAMP 
         WHERE OrderID = ?",
        params = list(new_status, updated_notes, order_id)
      )
      showNotification(sprintf("订单 %s 状态更新为 '%s'，备注已更新！", order_id, new_status), type = "message")
    }
    # 触发刷新
    if (!is.null(refresh_trigger)) {
      refresh_trigger(!refresh_trigger())
    }
  }, error = function(e) {
    showNotification(sprintf("更新订单状态和备注时发生错误：%s", e$message), type = "error")
  })
}

# 删除物品的确认框
deleteConfirmationModal <- function(item_count) {
  modalDialog(
    title = "确认删除",
    paste0("您已选择 ", item_count, " 件物品。这些物品删除后将无法恢复。是否继续？"),
    footer = tagList(
      modalButton("取消"),
      actionButton("confirm_delete_final", "确认删除", class = "btn-danger")
    )
  )
}

# 获取SKU的操作信息
fetchSkuOperationData <- function(sku, con, system_type) {
  # 根据地区动态生成查询语句
  query <- if (system_type == "cn") {
    "
      SELECT 
        inv.ItemImagePath,
        inv.ItemName,
        inv.Maker,
        inv.MajorType,
        inv.MinorType,
        inv.Quantity AS TotalQuantity, -- 总库存数量
        SUM(CASE WHEN u.Status = '采购' THEN 1 ELSE 0 END) AS PendingQuantity, -- 待入库数
        SUM(CASE WHEN u.Status = '国内入库' AND u.Defect != '瑕疵' THEN 1 ELSE 0 END) AS AvailableForOutbound -- 可出库数
      FROM inventory AS inv
      LEFT JOIN unique_items AS u
        ON inv.SKU = u.SKU
      WHERE inv.SKU = ?
      GROUP BY inv.ItemImagePath, inv.ItemName, inv.Maker, inv.MajorType, inv.MinorType, inv.Quantity
    "
  } else if (system_type == "us") {
    "
      SELECT 
        inv.ItemImagePath,
        inv.ItemName,
        inv.Maker,
        inv.MajorType,
        inv.MinorType,
        inv.Quantity AS TotalQuantity, -- 总库存数量
        SUM(CASE WHEN u.Status = '国内出库' THEN 1 ELSE 0 END) AS PendingQuantity -- 待入库数
      FROM inventory AS inv
      LEFT JOIN unique_items AS u
        ON inv.SKU = u.SKU
      WHERE inv.SKU = ?
      GROUP BY inv.ItemImagePath, inv.ItemName, inv.Maker, inv.MajorType, inv.MinorType, inv.Quantity
    "
  }
  
  # 执行查询并返回结果
  dbGetQuery(con, query, params = list(sku))
}

# 显示SKU信息
handleSkuInput <- function(
    sku_input,        # SKU 输入值
    output_name,      # 输出 UI 名称
    count_label,      # 显示的计数标签
    count_field,      # 数据字段名称
    con,              # 数据库连接
    output,           # 输出对象
    placeholder_path, # 默认占位图片路径
    host_url,          # 图片主机 URL
    image_mode = FALSE
) {
  sku <- trimws(sku_input) # 清理空格
  
  if (is.null(sku) || sku == "") {
    renderItemInfo(output, output_name, NULL, placeholder_path, count_label, count_field)
    return(NULL)
  }
  
  tryCatch({
    # 查询 SKU 数据
    item_info <- fetchSkuOperationData(sku, con, system_type)
    
    # 如果未找到记录
    if (nrow(item_info) == 0) {
      showNotification("未找到该条形码对应的物品！", type = "error")
      renderItemInfo(output, output_name, NULL, placeholder_path, count_label, count_field)
      return(NULL) # 返回 NULL
    }
    
    # 渲染商品信息
    renderItemInfo(
      output = output,
      output_name = output_name,
      item_info = item_info,
      img_path = ifelse(
        is.na(item_info$ItemImagePath[1]),
        placeholder_path,
        paste0(host_url, "/images/", basename(item_info$ItemImagePath[1]))
      ),
      count_label = count_label,
      count_field = count_field
    )
    
    if(image_mode) {
      renderItemInfo(
        output = output,
        output_name = paste0(output_name, "_image_mode"),
        item_info = item_info,
        img_path = ifelse(
          is.na(item_info$ItemImagePath[1]),
          placeholder_path,
          paste0(host_url, "/images/", basename(item_info$ItemImagePath[1]))
        ),
        image_only = TRUE
      )
    }
    
    # 返回 item_info
    return(item_info[[count_field]][1])
    
  }, error = function(e) {
    # 错误处理
    showNotification(paste("处理 SKU 输入时发生错误：", e$message), type = "error")
    return(NULL) # 返回 NULL
  })
}

# 处理入库出库操作
handleOperation <- function(
    unique_items_data,       # 数据集
    operation_name,          # 操作名称（如 "入库", "出库"）
    sku_field,               # SKU 字段的 input 名称
    output_name,             # 输出的 UI 名称
    query_status,            # 查询的初始状态
    update_status_value,     # 更新后的状态
    count_label,             # 计数标签
    count_field,             # 计数字段名称
    refresh_trigger,         # 数据刷新触发器
    con,                     # 数据库连接
    input, output, session,  # Shiny 的输入、输出和会话对象
    clear_field = NULL,      # 需要清空的字段
    clear_shipping_method = FALSE
) {
  sku <- trimws(input[[sku_field]])
  
  if (is.null(sku) || sku == "") {
    showNotification("请先扫描 SKU！", type = "error")
    renderItemInfo(output, output_name, NULL, placeholder_300px_path, count_label, count_field)
    return(NULL)
  }
  
  # 查询符合条件的物品
  sku_items <- unique_items_data %>%
    filter(SKU == sku, Status == query_status, Defect != "瑕疵") %>%
    arrange(ProductCost) %>%
    slice_head(n = 1)
  
  # 特殊情况：仅当操作为“入库”且状态为“国内出库”时检查
  if (operation_name == "入库" && query_status == "国内出库" && nrow(sku_items) == 0) {
    # 查询是否存在调货状态的物品
    transfer_items <- unique_items_data %>%
      filter(SKU == sku, Status == "美国调货", Defect != "瑕疵")
    
    if (nrow(transfer_items) > 0) {
      # 提取物品名称、订单号、运单号和图片路径
      item_name <- transfer_items$ItemName[1] %||% "未知商品"
      order_ids <- transfer_items$OrderID %>% unique()
      intl_tracking <- transfer_items$IntlTracking %>% unique()
      img_path <- transfer_items$ItemImagePath[1] %||% placeholder_300px_path
      
      # 弹窗提示用户
      showModal(modalDialog(
        title = paste0("调货提示 - SKU: ", sku, " - ", item_name),
        div(
          style = "display: flex; align-items: center; padding: 10px;",
          div(
            style = "flex: 1; text-align: center; margin-right: 20px;",
            tags$img(
              src = ifelse(is.na(img_path), placeholder_300px_path, paste0(host_url, "/images/", basename(img_path))),
              alt = "物品图片",
              style = "max-width: 150px; max-height: 150px; border: 1px solid #ddd; border-radius: 8px;"
            )
          ),
          div(
            style = "flex: 2;",
            tags$p("当前物品在入库前已被调货，请优先处理以下订单："),
            tags$b("物品名称："), item_name, tags$br(),
            tags$b("关联订单号："),
            lapply(order_ids, function(order_id) {
              div(
                style = "display: flex; align-items: center; margin-bottom: 5px;",
                tags$span(order_id, style = "flex: 1;"),
              )
            })
          )
        ),
        easyClose = TRUE,
        footer = modalButton("关闭")
      ))
      return(NULL)
    }
  }
  
  # 如果仍无符合条件的物品
  if (nrow(sku_items) == 0) {
    showNotification(paste0("无可", operation_name, "的物品！"), type = "message")
    return(NULL)
  }
  
  tryCatch({
    unique_id <- sku_items$UniqueID[1]
    item_name <- sku_items$ItemName[1]
    
    # 动态设置瑕疵状态
    defect_status <- if (operation_name == "入库" && !is.null(input$defective_item)) {
      ifelse(isTRUE(input$defective_item), "瑕疵", "无瑕")
    } else NULL
    
    # 动态设置运输方式
    shipping_method <- switch(
      operation_name,
      "撤回" = NULL,
      "出库" = input$outbound_shipping_method,
      "售出" = input$sold_shipping_method,
      NULL
    )
    
    # 动态清空字段
    if (!is.null(clear_field)) {
      dbExecute(con, paste0("UPDATE unique_items SET ", clear_field, " = NULL WHERE UniqueID = ?"), params = list(unique_id))
    }
    
    # 调用更新状态函数
    update_status(
      con = con,
      unique_id = unique_id,
      new_status = update_status_value,
      defect_status = defect_status,
      shipping_method = shipping_method,
      clear_shipping_method = clear_shipping_method,
      refresh_trigger = refresh_trigger
    )
    
    # 成功提示
    showNotification(paste0("物品成功", operation_name, "！"), type = "message")
    
    # 查询 SKU 数据并刷新 UI
    item_info <- fetchSkuOperationData(sku, con, system_type)
    renderItemInfo(
      output = output,
      output_name = output_name,
      item_info = item_info,
      img_path = ifelse(is.na(item_info$ItemImagePath[1]), placeholder_300px_path, paste0(host_url, "/images/", basename(item_info$ItemImagePath[1]))),
      count_label = count_label,
      count_field = count_field
    )
    
    # # 如果计数字段为 0，显示模态弹窗
    # if (item_info[[count_field]][1] == 0) {
    #   showModal(modalDialog(
    #     title = paste0(operation_name, "完成"),
    #     paste0("此 SKU 的商品已全部完成 ", operation_name, "！"),
    #     easyClose = TRUE,
    #     footer = NULL
    #   ))
    #   shinyjs::delay(2000, {
    #       removeModal()
    #   })
    # }
    
    # 重置输入框和控件
    updateTextInput(session, paste0(operation_name, "_sku"), value = "")
    if (operation_name == "入库") updateCheckboxInput(session, "defective_item", value = FALSE)
    
    return(list(item_name = item_name, unique_id = unique_id))
    
  }, error = function(e) {
    # 错误处理
    showNotification(paste0(operation_name, "失败：", e$message), type = "error")
  })
}

# 添加瑕疵备注
add_defective_note <- function(con, unique_id, note_content, status_label = "瑕疵", refresh_trigger = NULL) {
  # 获取当前日期并格式化
  current_date <- format(Sys.Date(), "%Y-%m-%d", tz = "Asia/Shanghai")
  
  # 为备注添加时间戳和状态标记
  new_note <- paste0("[", status_label, " ", current_date, "] ", note_content)
  
  # 查询现有备注
  current_notes <- dbGetQuery(
    con,
    "SELECT DefectNotes FROM unique_items WHERE UniqueID = ?",
    params = list(unique_id)
  )
  
  # 拼接备注
  if (nrow(current_notes) > 0 && !is.na(current_notes$DefectNotes[1])) {
    updated_notes <- paste(current_notes$DefectNotes[1], new_note, sep = "; ")
  } else {
    updated_notes <- new_note
  }
  
  # 更新数据库中的备注
  dbExecute(
    con,
    "UPDATE unique_items SET DefectNotes = ? WHERE UniqueID = ?",
    params = list(updated_notes, unique_id)
  )
  
  # 触发刷新机制
  if (!is.null(refresh_trigger)) {
    refresh_trigger(!refresh_trigger())
  }
}

# 更新订单拼图
update_order_montage <- function(order_id, con, unique_items_data) {
  req(order_id)  # 确保 order_id 不为空
  
  # 获取订单内关联物品的图片路径
  order_items <- unique_items_data %>% filter(OrderID == order_id)
  order_items_image_paths <- if (nrow(order_items) > 0) {
    order_items$ItemImagePath[!is.na(order_items$ItemImagePath)]
  } else {
    character(0)  # 如果为空，返回空字符向量
  }
  
  if (length(order_items_image_paths) > 0) {
    order_image_path <- generate_montage(order_items_image_paths, 
                                         paste0("/var/www/images/", order_id, "_montage_", format(Sys.time(), "%Y%m%d%H%M%S"), ".jpg"))
    
    # 更新数据库中的 `OrderImagePath`
    dbExecute(con, "UPDATE orders SET OrderImagePath = ? WHERE OrderID = ?", 
              params = list(order_image_path, order_id))
    
    showNotification(sprintf("订单 #%s 拼图生成成功！", order_id), type = "message")
    return(TRUE)  # 成功时返回 TRUE，调用者可决定是否刷新数据
  } else {
    showNotification(sprintf("订单 #%s 没有可用的商品图片！", order_id), type = "warning")
    return(FALSE)  # 失败时返回 FALSE
  }
}

# 订单注册与更新
register_order <- function(order_id, customer_name, customer_netname, platform, transaction_amount, order_notes, tracking_number, 
                           image_data, con, orders, box_items, unique_items_data,
                           is_transfer_order = NULL, is_preorder = NULL, preorder_supplier = NULL, preorder_item_name = NULL) {
  tryCatch({
    # 将 NULL 的 is_transfer_order 和 is_preorder 设置为默认值 FALSE
    is_transfer_order <- is_transfer_order %||% FALSE
    is_preorder <- is_preorder %||% FALSE
    
    # 查询是否已有相同订单号的记录
    existing_order <- orders() %>% filter(OrderID == order_id)
    
    # 初始化订单图片路径
    order_image_path <- NULL
    
    # 确定订单状态
    order_status <- "备货"  # 默认状态
    if (is_transfer_order && !is_preorder) {
      order_status <- "调货"
    } else if (is_preorder && !is_transfer_order) {
      order_status <- "预定"
    }
    
    # 检查发货箱中的物品是否含有“美国入库”状态
    if (nrow(box_items()) > 0) {
      # 如果 box_items() 中包含 "国内出库" 或 "美国入库"，设置状态为 "调货"
      if (any(box_items()$Status %in% c("国内出库", "美国入库"))) {
        order_status <- "调货"
      }
    }
    
    # 确认运单 PDF 文件的状态
    label_status <- if (!is.null(tracking_number)) {
      if (file.exists(file.path("/var/uploads/shiplabels", paste0(tracking_number, ".pdf")))) {
        "已上传"
      } else {
        "无"
      }
    } else {
      "无"
    }
    
    # # 如果状态为 "调货" 且 LabelStatus 为 "无"，显示错误通知
    # if (order_status == "调货" && label_status == "无") {
    #   showNotification("调货订单必须上传运单 PDF！", type = "error")
    #   return(FALSE)
    # }
    
    # 如果为预订单，生成或更新供应商备注
    if (is_preorder && !is.null(preorder_supplier)) {
      supplier_prefix <- "【供应商】"
      
      # 初始化物品备注部分
      items_note <- ""
      
      # 检查 preorder_item_name 是否为空
      if (!is.null(preorder_item_name) && preorder_item_name != "") {
        item_list <- unlist(strsplit(preorder_item_name, "\n"))
        item_list <- item_list[item_list != ""]
        if (length(item_list) > 0) {
          items_str <- paste(item_list, collapse = "，")
          items_note <- paste0(" 【预定物品】", items_str)
        }
      }
      
      # 创建新的供应商备注
      new_supplier_note <- paste0(supplier_prefix, preorder_supplier, items_note, "；")
      
      if (nrow(existing_order) > 0) {
        existing_notes <- existing_order$OrderNotes[1] %||% ""  # 确保为长度为 1 的字符
        
        # 检查是否包含分号，若包含则只保留分号后的内容
        if (grepl(";", existing_notes)) {
          user_notes <- sub(".*；", "", existing_notes)  # 提取分号后的内容
          order_notes <- paste0(new_supplier_note, user_notes)  # 与新备注拼接
        } else {
          # 如果没有分号，保留原有内容（假设全为用户输入）
          order_notes <- paste0(new_supplier_note, existing_notes)
        }
      } else {
        order_notes <- paste0(new_supplier_note, order_notes %||% "")
      }
    }
    
    # 获取发货箱中的物品图片路径
    box_data <- box_items()
    box_image_paths <- if (nrow(box_data) > 0) {
      box_data$ItemImagePath[!is.na(box_data$ItemImagePath)]
    } else {
      character(0)  # 如果为空，返回空字符向量
    }
    
    # 获取订单内关联物品的图片路径
    order_items <- unique_items_data() %>% filter(OrderID == order_id)
    
    order_items_image_paths <- if (nrow(order_items) > 0) {
      order_items$ItemImagePath[!is.na(order_items$ItemImagePath)]
    } else {
      character(0)  # 如果为空，返回空字符向量
    }
    
    # 获取订单中的图片路径作为 inventory_path
    order_image_path <- if (nrow(existing_order) > 0) {
      existing_order$OrderImagePath[1]  # 提取 OrderImagePath
    } else {
      NULL
    }
    
    # 处理 image_data 数据
    image_path <- process_image_upload(
      sku = order_id,
      file_data = image_data$uploaded_file(),
      pasted_data = image_data$pasted_file()
    )
    
    # 决定订单图片路径
    if (!is.na(image_path)) {
      # 如果用户上传或粘贴了图片，直接使用用户上传的图片路径（最高优先级）
      order_image_path <- image_path
    } else {
      # 如果没有用户上传或粘贴的图片，使用库存中的图片路径和发货箱的图片路径生成拼贴图
      combined_image_paths <- unique(c(order_items_image_paths, box_image_paths))
      if (length(combined_image_paths) > 0) {
        montage_path <- paste0("/var/www/images/", order_id, "_montage_", format(Sys.time(), "%Y%m%d%H%M%S"), ".jpg")
        order_image_path <- generate_montage(combined_image_paths, montage_path)
      } else {
        order_image_path <- NA  # 确保为长度为 1 的 NA
      }
    }
    
    # 确保所有参数为长度为 1 的值
    tracking_number <- tracking_number %||% NA
    order_notes <- order_notes %||% NA
    customer_name <- customer_name %||% NA
    customer_netname <- customer_netname %||% NA
    order_image_path <- as.character(order_image_path %||% NA)
    platform <- platform %||% ""

    # 插入或更新订单
    if (nrow(existing_order) > 0) {
      dbExecute(con, "
        UPDATE orders 
        SET OrderImagePath = COALESCE(?, OrderImagePath), 
            UsTrackingNumber = COALESCE(?, UsTrackingNumber), 
            OrderNotes = COALESCE(?, OrderNotes),
            CustomerName = COALESCE(?, CustomerName),
            CustomerNetName = COALESCE(?, CustomerNetName),
            Platform = COALESCE(?, Platform),
            TransactionAmount = ?,
            OrderStatus = ?,
            LabelStatus = ?
        WHERE OrderID = ?",
                params = list(
                  order_image_path,
                  tracking_number,
                  order_notes,
                  customer_name,
                  customer_netname,
                  platform,
                  transaction_amount,
                  order_status,
                  label_status,
                  order_id
                )
      )
      showNotification("订单信息已更新！", type = "message")
    } else {
      dbExecute(con, "
        INSERT INTO orders (OrderID, UsTrackingNumber, OrderNotes, CustomerName, CustomerNetName, Platform, TransactionAmount, OrderImagePath, OrderStatus, LabelStatus)
        VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
                params = list(
                  order_id,
                  tracking_number,
                  order_notes,
                  customer_name,
                  customer_netname,
                  platform,
                  transaction_amount,
                  order_image_path,
                  order_status,
                  label_status
                )
      )
      showNotification("订单已成功登记！", type = "message")
    }
    return(TRUE)
  }, error = function(e) {
    showNotification(paste("登记订单时发生错误：", e$message), type = "error")
    return(FALSE)
  })
}

# 更新运单PDF状态列
update_label_status_column <- function(con, pdf_directory = "/var/uploads/shiplabels") {
  # 列出所有 PDF 文件
  existing_files <- list.files(pdf_directory, full.names = FALSE)
  existing_tracking_numbers <- gsub("\\.pdf$", "", existing_files)  # 提取运单号（去掉 .pdf 后缀）
  
  tryCatch({
    # 动态生成 CASE 语句
    case_statements <- paste0(
      "CASE
        WHEN UsTrackingNumber IS NULL OR UsTrackingNumber = '' THEN '无'  -- 如果运单号为空或 NULL，更新为 '无'
        WHEN UsTrackingNumber IN (", 
      if (length(existing_tracking_numbers) > 0) {
        paste(sprintf("'%s'", existing_tracking_numbers), collapse = ",")
      } else {
        "''"  # 如果没有 PDF 文件，提供一个空的占位符
      }, 
      ") THEN
          CASE
            WHEN LabelStatus = '已打印' THEN '已打印'  -- 如果状态是 '已打印'，保持不变
            ELSE '已上传'                          -- 否则更新为 '已上传'
          END
        ELSE '无'  -- 如果运单号存在但没有对应的 PDF 文件，更新为 '无'
      END"
    )
    
    # 构建 SQL 更新语句
    update_query <- paste0(
      "UPDATE orders
       SET LabelStatus = ", case_statements
    )
    
    # 执行 SQL 更新
    dbExecute(con, update_query)
  }, error = function(e) {
    stop(paste("更新 LabelStatus 列时发生错误：", e$message))
  })
}


# 从输入数据中筛选数据
filter_unique_items_data_by_inputs <- function(
    data, 
    input, 
    maker_input_id, 
    status_input_id = NULL,
    item_name_input_id, 
    purchase_date_range_id = NULL, 
    sold_date_range_id = NULL,
    only_show_sold_id = NULL,
    exit_date_range_id = NULL,
    only_show_exit_id = NULL
) {
  req(data)  # 确保数据不为空
  
  # 按供应商筛选
  if (!is.null(input[[maker_input_id]]) && length(input[[maker_input_id]]) > 0 && any(input[[maker_input_id]] != "")) {
    data <- data %>% filter(Maker %in% input[[maker_input_id]])
  }
  
  # 按库存状态筛选
  if (!is.null(status_input_id) && !is.null(input[[status_input_id]]) && length(input[[status_input_id]]) > 0 && any(input[[status_input_id]] != "")) {
    data <- data %>% filter(Status %in% input[[status_input_id]])
  }
  
  # 按商品名称模糊匹配筛选
  if (!is.null(item_name_input_id) && !is.null(input[[item_name_input_id]]) && input[[item_name_input_id]] != "") {
    data <- data %>% filter(grepl(input[[item_name_input_id]], ItemName, ignore.case = TRUE))
  }
  
  # 按采购日期筛选
  if (!is.null(purchase_date_range_id) && !is.null(input[[purchase_date_range_id]]) && length(input[[purchase_date_range_id]]) == 2) {
    purchase_date_range <- as.Date(input[[purchase_date_range_id]])
    data <- data %>%
      filter(as.Date(PurchaseTime) >= purchase_date_range[1], 
             as.Date(PurchaseTime) <= purchase_date_range[2])
  }
  
  # 按售出日期筛选，仅对库存状态为‘国内售出’的物品有效
  if (!is.null(sold_date_range_id) && 
      !is.null(input[[sold_date_range_id]]) && 
      length(input[[sold_date_range_id]]) == 2) {
    sold_date_range <- as.Date(input[[sold_date_range_id]])

    if (!is.null(input[[only_show_sold_id]]) && input[[only_show_sold_id]]) {
      data <- data %>%
        filter(Status == "国内售出", 
               as.Date(DomesticSoldTime) >= sold_date_range[1], 
               as.Date(DomesticSoldTime) <= sold_date_range[2]) %>%
        select(-DomesticExitTime)  # 去掉“国内出库”列
    }
  }
  
  # 按出库日期筛选，仅对库存状态为‘国内出库’的物品有效
  if (!is.null(exit_date_range_id) && 
      !is.null(input[[exit_date_range_id]]) && 
      length(input[[exit_date_range_id]]) == 2) {
    exit_date_range <- as.Date(input[[exit_date_range_id]])

    if (!is.null(input[[only_show_exit_id]]) && input[[only_show_exit_id]]) {
      data <- data %>%
        filter(Status == "国内出库", 
               as.Date(DomesticExitTime) >= exit_date_range[1], 
               as.Date(DomesticExitTime) <= exit_date_range[2]) %>%
        select(-DomesticSoldTime)  # 去掉“国内售出”列
    }
  }
  
  return(data)
}

# 编辑库存数量
adjust_inventory_quantity <- function(con, sku, adjustment) {
  tryCatch({
    sku <- trimws(sku)  # 清理空格
    
    # 查询现有库存
    existing_item <- dbGetQuery(con, "SELECT * FROM inventory WHERE SKU = ?", params = list(sku))
    
    if (nrow(existing_item) > 0) {
      # SKU 存在，获取当前库存数量
      current_quantity <- existing_item$Quantity
      
      # 如果调整量为 0，仅返回当前库存
      if (adjustment == 0) {
        return(current_quantity)
      }
      
      # 计算新的库存
      new_quantity <- current_quantity + adjustment
      
      # 库存不足的校验（仅在减少库存时检查）
      if (adjustment < 0 && new_quantity < 0) {
        showNotification("库存不足，无法完成操作！", type = "error")
        return(current_quantity)
      }
      
      # 更新库存数量到数据库
      dbExecute(con, "UPDATE inventory SET Quantity = ? WHERE SKU = ?", params = list(new_quantity, sku))
      
      showNotification(paste("库存已成功调整! SKU:", sku), type = "message")
      return(new_quantity)
    } else {
      showNotification(paste("SKU 不存在，无法调整库存！SKU:", sku), type = "error")
      return(NULL)
    }
  }, error = function(e) {
    showNotification(paste("调整库存时发生错误：", e$message), type = "error")
    return(NULL)
  })
}

# 渲染订单物品卡片
renderOrderItems <- function(output, output_name, order_items, con, deletable = FALSE) {
  # 如果没有物品，返回提示信息
  if (is.null(order_items) || nrow(order_items) == 0) {
    output[[output_name]] <- renderUI({
      div("没有找到订单内物品")
    })
    return()
  }
  
  # 动态渲染物品卡片
  output[[output_name]] <- renderUI({
    item_cards <- lapply(1:nrow(order_items), function(i) {
      item <- order_items[i, ]
      
      # 图片路径
      item_img_path <- ifelse(
        is.na(item$ItemImagePath) || item$ItemImagePath == "",
        placeholder_150px_path,
        paste0(host_url, "/images/", basename(item$ItemImagePath))
      )
      
      # 获取国际运单状态
      intl_status <- if (!is.na(item$IntlTracking) && item$IntlTracking != "") {
        # 查询数据库中的 Status
        shipment_status <- dbGetQuery(
          con,
          "SELECT Status FROM intl_shipments WHERE TrackingNumber = ?",
          params = list(item$IntlTracking)
        )
        if (nrow(shipment_status) > 0) shipment_status$Status[1] else "未知状态"
      } else {
        "未邮寄"
      }
      
      # 动态添加蒙版和打勾图标
      mask_overlay <- if (item$Status == "美国发货") {
        div(
          style = "position: absolute; top: 0; left: 0; width: 100%; height: 100%; 
                  background: rgba(128, 128, 128, 0.6); display: flex; justify-content: center; align-items: center;
                  border-radius: 8px; z-index: 2;",
          tags$div(
            style = "width: 50px; height: 50px; background: #28a745; border-radius: 50%; display: flex; 
                     justify-content: center; align-items: center;",
            tags$i(class = "fas fa-check", style = "color: white; font-size: 24px;")  # 绿色勾
          )
        )
      } else {
        NULL
      }
      
      # 渲染卡片
      div(
        id = paste0("card_", i),  # 设置唯一 ID
        class = "card",
        style = "position: relative; display: inline-block; padding: 10px; width: 230px; text-align: center; 
                 border: 1px solid #ddd; border-radius: 8px; box-shadow: 0px 2px 4px rgba(0, 0, 0, 0.1);",
        
        mask_overlay,  # 动态显示蒙版
        
        div(
          style = "margin-bottom: 10px; position: relative;",
          tags$img(
            src = item_img_path,
            style = "height: 150px; object-fit: cover; border-radius: 8px;"  # 图片高度固定为150px
          ),
          if (deletable) {
            tags$button(
              class = "btn btn-danger btn-sm delete-btn",
              type = "button",
              style = "position: absolute; top: 5px; right: 5px;",
              onclick = sprintf("Shiny.setInputValue('delete_card', '%s', {priority: 'event'})", item$UniqueID),
              tags$i(class = "fas fa-trash-alt")
            )
          } else {
            NULL
          }
        ),
        
        tags$table(
          style = "width: 100%; font-size: 12px; color: #333;",
          tags$tr(
            tags$td(tags$strong("SKU:"), style = "padding: 0px; width: 60px;"),
            tags$td(item$SKU)
          ),
          tags$tr(
            tags$td(tags$strong("商品名:"), style = "padding: 0px;"),
            tags$td(item$ItemName)
          ),
          tags$tr(
            tags$td(tags$strong("库存状态:"), style = "padding: 0px;"),
            tags$td(
              item$Status,
              style = "color: #383efc; font-weight: bold;"  # 设置蓝色高亮
            )
          ),
          tags$tr(
            tags$td(tags$strong("国际物流:"), style = "padding: 0px;"),
            tags$td(
              intl_status,
              style = "color: #209126; font-weight: bold;"  # 设置绿色高亮
            )
          ),
          tags$tr(
            tags$td(tags$strong("瑕疵态:"), style = "padding: 0px;"),
            tags$td(ifelse(is.na(item$Defect), "无", item$Defect))  # 显示瑕疵状态
          ),
          tags$tr(
            tags$td(tags$strong("瑕疵注:"), style = "padding: 0px;"),
            tags$td(ifelse(is.na(item$DefectNotes) || item$DefectNotes == "", "无备注", item$DefectNotes))  # 显示瑕疵备注
          )
        )
      )
    })
    
    do.call(tagList, item_cards)  # 返回卡片列表
  })
}

# 动态拼接图片函数（接近正方形布局）
generate_montage <- function(image_paths, output_path, geometry = "+5+5") {
  # 计算行列数，保持接近正方形
  n_images <- length(image_paths)
  n_cols <- ceiling(sqrt(n_images))
  n_rows <- ceiling(n_images / n_cols)
  tile <- paste0(n_rows, "x", n_cols)  # 拼接行列数，例如 "3x3"
  
  # 加载所有图片
  images <- lapply(image_paths, magick::image_read)
  
  # 拼接图片
  montage <- magick::image_montage(
    do.call(c, images),
    tile = tile,      # 动态行列布局
    geometry = geometry # 图片间距
  )
  
  montage <- magick::image_convert(montage, format = "jpeg")
  
  # 保存拼接图片
  magick::image_write(montage, path = output_path)
  
  return(output_path)
}

# 数据下载分页的高级下拉菜单
createSearchableDropdown <- function(input_id, label, data, placeholder = "搜索...") {
  # 将数据转换为 Dropdown 所需格式
  options <- if (length(data) > 0) {
    lapply(data, function(item) list(key = item, text = item))
  } else {
    # 提供默认值避免空选项
    list(list(key = "no-data", text = "无数据"))
  }
  
  # 定义 DropdownMenuItemType
  DropdownMenuItemType <- function(type) {
    JS(paste0("jsmodule['@fluentui/react'].DropdownMenuItemType.", type))
  }
  
  # 包含搜索框的选项列表
  options_with_search <- function(opt) {
    filter_header <- list(
      key = "__FilterHeader__", 
      text = "-", 
      itemType = DropdownMenuItemType("Header") # 添加一个 Header 类型选项用于搜索框
    )
    append(list(filter_header), opt)
  }
  
  # 定义模糊搜索渲染逻辑
  render_search_box <- JS(paste0("(option) => {
    if (option.key !== '__FilterHeader__') {
      return option.text;
    }
    const onChange = (event, newValue) => {
      const query = newValue.toLocaleLowerCase();
      const checkboxLabels = document.querySelectorAll(
        '#", input_id, "-list .ms-Checkbox-label'
      );
      checkboxLabels.forEach(label => {
        const text = label.innerText.replace('\\n', '').replace('', '').toLocaleLowerCase();
        // 使用 indexOf 实现模糊匹配
        if (query === '' || text.indexOf(query) !== -1) {
          label.parentElement.style.display = 'flex';
        } else {
          label.parentElement.style.display = 'none';
        }
      });
    };
    const props = { 
      placeholder: '", placeholder, "', 
      underlined: true, 
      onChange 
    };
    return React.createElement(jsmodule['@fluentui/react'].SearchBox, props);
  }"))
  
  # 定义样式，控制下拉菜单的高度
  dropdown_styles <- JS("{
    callout: {
      maxHeight: '200px', // 设置下拉菜单最大高度
      overflowY: 'auto'   // 启用垂直滚动条
    }
  }")
  
  # 返回创建的 UI
  div(
    style = "padding-bottom: 15px;", # 外层 div 设置内边距和字体大小
    Dropdown.shinyInput(
      inputId = input_id,
      label = label,
      options = options_with_search(options), # 添加搜索框到选项列表中
      multiSelect = TRUE,
      placeholder = placeholder,
      onRenderOption = render_search_box, # 自定义渲染逻辑
      styles = dropdown_styles            # 应用样式控制下拉菜单高度
    )
  )
}

# 匹配USPS单号
match_tracking_number <- function(data, tracking_number_column, input_tracking_id) {
  # 清理输入运单号（去掉空格和非数字字符）
  cleaned_tracking_id <- gsub("[^0-9]", "", trimws(input_tracking_id))
  
  # **使用 `grepl()` 进行子字符串匹配**
  matched_data <- data %>%
    filter(
      !is.na(.data[[tracking_number_column]]) & 
        .data[[tracking_number_column]] != "" & 
        Vectorize(grepl)(.data[[tracking_number_column]], cleaned_tracking_id, fixed = TRUE)  # ✅ 关键匹配逻辑
    )
  
  return(matched_data)
}

# 定义排序函数
sort_requests <- function(df) {
  df %>%
    arrange(
      factor(RequestStatus, levels = c("紧急", "待处理", "已完成")),
      Maker,
      CreatedAt
    )
}

# 增量渲染任务板
refresh_board_incremental <- function(requests, output) {
  request_types <- list(
    "新品" = "new_product_board",
    "采购" = "purchase_request_board",
    "安排" = "provider_arranged_board",
    "完成" = "done_paid_board",
    "出库" = "outbound_request_board"
  )
  
  lapply(names(request_types), function(req_type) {
    output_id <- request_types[[req_type]]
    filtered_requests <- requests %>% filter(RequestType == req_type) %>% sort_requests()
    
    output[[output_id]] <- renderUI({
      if (nrow(filtered_requests) == 0) {
        div(style = "text-align: center; color: grey; margin-top: 20px;", tags$p("当前没有待处理事项"))
      } else {
        div(
          style = "display: grid; grid-template-columns: repeat(auto-fill, minmax(300px, 1fr)); row-gap: 15px; column-gap: 15px; padding: 5px;",
          lapply(filtered_requests$RequestID, function(request_id) {
            uiOutput(paste0("request_card_", request_id))
          })
        )
      }
    })
    
    # 直接渲染所有卡片，不检查是否已存在
    lapply(filtered_requests$RequestID, function(request_id) {
      render_single_request(request_id, filtered_requests, output)
    })
  })
}

# 更新单个 request 数据并重新渲染
update_single_request <- function(request_id, requests_data, output) {
  current_data <- requests_data()
  updated_row <- current_data %>% filter(RequestID == request_id)
  if (nrow(updated_row) > 0) {
    render_single_request(request_id, current_data, output)
  } else {
    output[[paste0("request_card_", request_id)]] <- renderUI(NULL)  # 删除时移除卡片
  }
}

# 渲染单个 request 卡片
render_single_request <- function(request_id, requests, output) {
  item <- requests %>% filter(RequestID == request_id)
  if (nrow(item) == 0) return()
  
  output[[paste0("request_card_", request_id)]] <- renderUI({
    card_colors <- switch(
      item$RequestStatus,
      "紧急" = list(bg = "#ffcdd2", border = "#e57373"),
      "待处理" = list(bg = "#fff9c4", border = "#ffd54f"),
      "已完成" = list(bg = "#c8e6c9", border = "#81c784"),
      list(bg = "#f0f0f0", border = "#bdbdbd")
    )
    
    div(
      class = "note-card",
      style = sprintf("width: 300px; background-color: %s; border: 2px solid %s; border-radius: 10px; box-shadow: 0 4px 8px rgba(0,0,0,0.1); padding: 10px; display: flex; flex-direction: column;",
                      card_colors$bg, card_colors$border),
      div(
        style = "display: flex; justify-content: space-between; align-items: flex-start; margin-bottom: 10px;",
        tags$div(
          style = "width: 42%; display: flex; flex-direction: column; align-items: center;",
          tags$img(
            src = ifelse(is.na(item$ItemImagePath), placeholder_150px_path, paste0(host_url, "/images/", basename(item$ItemImagePath))),
            style = "width: 100%; max-height: 120px; object-fit: contain; box-shadow: 0px 4px 6px rgba(0,0,0,0.1); border-radius: 5px; margin-bottom: 5px; cursor: pointer;",
            onclick = sprintf("Shiny.setInputValue('view_request_image', '%s')", 
                              ifelse(is.na(item$ItemImagePath), placeholder_150px_path, paste0(host_url, "/images/", basename(item$ItemImagePath)))),
            onmouseover = sprintf("showInventoryStatus(event, '%s')", item$SKU),
            onmouseout = "hideInventoryStatus()"
          ),
          tags$div(
            style = "width: 100%; text-align: center; font-size: 12px; color: #333;",
            tags$p(tags$b(item$ItemDescription), style = "margin: 0;"),
            tags$p(item$SKU, style = "margin: 0;"),
            tags$p(tags$b("供应商:"), tags$span(item$Maker, style = "color: blue; font-weight: bold;"), style = "margin: 0;"),
            tags$p(tags$b("请求数量:"), tags$span(item$Quantity, style = "color: red; font-weight: bold;"), style = "margin: 0;")
          )
        ),
        tags$div(
          style = "width: 54%; height: 194px; border: 1px solid #ddd; padding: 5px; background-color: #fff; overflow-y: auto; border-radius: 5px;",
          uiOutput(paste0("remarks_", request_id))
        )
      ),
      tags$div(
        style = "width: 100%; display: flex; flex-direction: column; margin-top: 5px;",
        tags$div(
          style = "width: 100%; display: flex; justify-content: space-between;",
          textInput(paste0("remark_input_", request_id), NULL, placeholder = "输入留言", width = "72%"),
          actionButton(paste0("submit_remark_", request_id), "提交", class = "btn-success", style = "width: 25%; height: 45px;")
        ),
        tags$div(
          style = "width: 100%; display: flex; gap: 5px; margin-top: 5px;",
          actionButton(paste0("mark_urgent_", request_id), "加急", class = "btn-danger", style = "flex-grow: 1; height: 45px;"),
          if (item$RequestType == "采购") {
            actionButton(paste0("provider_arranged_", request_id), "安排", class = "btn-primary", style = "flex-grow: 1; height: 45px;")
          } else if (item$RequestType == "安排") {
            tagList(
              actionButton(paste0("done_paid_", request_id), "完成", class = "btn-primary", style = "flex-grow: 1; height: 45px;"),
              actionButton(paste0("provider_arranged_cancel_", request_id), "撤回", class = "btn-warning", style = "flex-grow: 1; height: 45px;")
            )
          } else if (item$RequestType == "完成") {
            tagList(
              actionButton(paste0("stock_in_", request_id), "入库", class = "btn-success", style = "flex-grow: 1; height: 45px;"),
              actionButton(paste0("done_paid_cancel_", request_id), "撤回", class = "btn-warning", style = "flex-grow: 1; height: 45px;")
            )
          } else if (item$RequestType == "新品") {
            actionButton(paste0("complete_task_", request_id), "完成", class = "btn-primary", style = "flex-grow: 1; height: 45px;")
          },
          actionButton(paste0("delete_request_", request_id), "删除", class = "btn-secondary", style = "flex-grow: 1; height: 45px;")
        )
      )
    )
  })
  
  output[[paste0("remarks_", request_id)]] <- renderRemarks(request_id, requests)
}

# 定义一个 reactiveVal 跟踪已绑定的 RequestID
bound_requests <- reactiveVal(character())

# 绑定按钮事件
bind_buttons <- function(request_id, requests_data, input, output, session, con) {
  # 检查是否已绑定
  current_bound <- bound_requests()
  if (!request_id %in% current_bound) {
    # 加急按钮
    observeEvent(input[[paste0("mark_urgent_", request_id)]], {
      dbExecute(con, "UPDATE requests SET RequestStatus = '紧急' WHERE RequestID = ?", params = list(request_id))
      current_data <- requests_data()
      updated_data <- current_data %>% mutate(RequestStatus = ifelse(RequestID == request_id, "紧急", RequestStatus))
      requests_data(updated_data)
      update_single_request(request_id, requests_data, output)
    }, ignoreInit = TRUE)  # 移除 once = TRUE
    
    # 安排按钮
    observeEvent(input[[paste0("provider_arranged_", request_id)]], {
      dbExecute(con, "UPDATE requests SET RequestType = '安排' WHERE RequestID = ?", params = list(request_id))
      current_data <- requests_data()
      updated_data <- current_data %>% mutate(RequestType = ifelse(RequestID == request_id, "安排", RequestType))
      requests_data(updated_data)
      update_single_request(request_id, requests_data, output)
    }, ignoreInit = TRUE)
    
    # 完成按钮
    observeEvent(input[[paste0("done_paid_", request_id)]], {
      dbExecute(con, "UPDATE requests SET RequestType = '完成' WHERE RequestID = ?", params = list(request_id))
      current_data <- requests_data()
      updated_data <- current_data %>% mutate(RequestType = ifelse(RequestID == request_id, "完成", RequestType))
      requests_data(updated_data)
      update_single_request(request_id, requests_data, output)
    }, ignoreInit = TRUE)
    
    # 撤回（从安排到采购）
    observeEvent(input[[paste0("provider_arranged_cancel_", request_id)]], {
      dbExecute(con, "UPDATE requests SET RequestType = '采购' WHERE RequestID = ?", params = list(request_id))
      current_data <- requests_data()
      updated_data <- current_data %>% mutate(RequestType = ifelse(RequestID == request_id, "采购", RequestType))
      requests_data(updated_data)
      update_single_request(request_id, requests_data, output)
    }, ignoreInit = TRUE)
    
    # 撤回（从完成到安排）
    observeEvent(input[[paste0("done_paid_cancel_", request_id)]], {
      dbExecute(con, "UPDATE requests SET RequestType = '安排' WHERE RequestID = ?", params = list(request_id))
      current_data <- requests_data()
      updated_data <- current_data %>% mutate(RequestType = ifelse(RequestID == request_id, "安排", RequestType))
      requests_data(updated_data)
      update_single_request(request_id, requests_data, output)
    }, ignoreInit = TRUE)
    
    # 入库按钮（从完成到出库）
    observeEvent(input[[paste0("stock_in_", request_id)]], {
      dbExecute(con, "UPDATE requests SET RequestType = '出库', RequestStatus = '已完成' WHERE RequestID = ?", params = list(request_id))
      current_data <- requests_data()
      updated_data <- current_data %>% mutate(RequestType = ifelse(RequestID == request_id, "出库", RequestType),
                                              RequestStatus = ifelse(RequestID == request_id, "已完成", RequestStatus))
      requests_data(updated_data)
      update_single_request(request_id, requests_data, output)
    }, ignoreInit = TRUE)
    
    # 完成任务（新品）
    observeEvent(input[[paste0("complete_task_", request_id)]], {
      dbExecute(con, "UPDATE requests SET RequestStatus = '已完成' WHERE RequestID = ?", params = list(request_id))
      current_data <- requests_data()
      updated_data <- current_data %>% mutate(RequestStatus = ifelse(RequestID == request_id, "已完成", RequestStatus))
      requests_data(updated_data)
      update_single_request(request_id, requests_data, output)
    }, ignoreInit = TRUE)
    
    # 删除按钮
    observeEvent(input[[paste0("delete_request_", request_id)]], {
      dbExecute(con, "DELETE FROM requests WHERE RequestID = ?", params = list(request_id))
      current_data <- requests_data()
      updated_data <- current_data %>% filter(RequestID != request_id)
      requests_data(updated_data)
      update_single_request(request_id, requests_data, output)
    }, ignoreInit = TRUE)
    
    # 提交留言
    observeEvent(input[[paste0("submit_remark_", request_id)]], {
      remark <- input[[paste0("remark_input_", request_id)]]
      req(remark != "")
      
      current_data <- requests_data()
      new_remark <- format_remark(remark, system_type)
      
      # 解析 Quantity=X
      quantity_match <- regmatches(remark, regexec("^Quantity=(\\d+)$", remark))
      if (length(quantity_match[[1]]) > 1) {
        new_quantity <- as.integer(quantity_match[[1]][2])
        if (!is.na(new_quantity) && new_quantity > 0) {
          dbExecute(con, "UPDATE requests SET Quantity = ? WHERE RequestID = ?", params = list(new_quantity, request_id))
          updated_data <- current_data %>% mutate(Quantity = ifelse(RequestID == request_id, new_quantity, Quantity))
          requests_data(updated_data)
          update_single_request(request_id, requests_data, output)
          showNotification(paste("请求数量已更新为", new_quantity, "！"), type = "message")
          updateTextInput(session, paste0("remark_input_", request_id), value = "")
        } else {
          showNotification("无效的数量输入，请使用 'Quantity=X' 格式，X 为正整数。", type = "error")
        }
      } else if (length(maker_match <- regmatches(remark, regexec("^Maker=(.+)$", remark))[[1]]) > 1) {
        new_maker <- trimws(maker_match[2])
        if (nchar(new_maker) > 0) {
          dbExecute(con, "UPDATE requests SET Maker = ? WHERE RequestID = ?", params = list(new_maker, request_id))
          updated_data <- current_data %>% mutate(Maker = ifelse(RequestID == request_id, new_maker, Maker))
          requests_data(updated_data)
          update_single_request(request_id, requests_data, output)
          showNotification(paste("供应商已更新为", new_maker, "！"), type = "message")
          updateTextInput(session, paste0("remark_input_", request_id), value = "")
        } else {
          showNotification("无效的供应商输入，请使用 'Maker=XXX' 格式。", type = "error")
        }
      } else {
        current_row <- current_data %>% filter(RequestID == request_id)
        current_remarks <- current_row %>% pull(Remarks)
        updated_remarks <- ifelse(is.na(current_remarks) || current_remarks == "", new_remark, paste(new_remark, current_remarks, sep = ";"))
        
        dbExecute(con, "UPDATE requests SET Remarks = ? WHERE RequestID = ?", params = list(updated_remarks, request_id))
        updated_data <- current_data %>% mutate(Remarks = ifelse(RequestID == request_id, updated_remarks, Remarks))
        requests_data(updated_data)
        update_single_request(request_id, requests_data, output)
        updateTextInput(session, paste0("remark_input_", request_id), value = "")
      }
    }, ignoreInit = TRUE)
    
    # 更新已绑定列表
    bound_requests(unique(c(current_bound, request_id)))
  }
}

# 渲染留言板
renderRemarks <- function(request_id, requests) {
  current_remarks <- requests %>% filter(RequestID == request_id) %>% pull(Remarks)
  
  if (is.null(current_remarks) || is.na(current_remarks) || current_remarks == "") {
    remarks <- list()
  } else {
    remarks <- unlist(strsplit(trimws(current_remarks), ";"))
  }
  
  # 使用 reactiveVal 缓存渲染结果
  renderUI({
    if (length(remarks) > 0) {
      tags$div(
        lapply(remarks, function(h) {
          split_remarks <- strsplit(h, ": ", fixed = TRUE)[[1]]
          remark_time <- ifelse(length(split_remarks) > 1, split_remarks[1], "")
          remark_text <- ifelse(length(split_remarks) > 1, split_remarks[2], split_remarks[1])
          
          tags$div(
            style = "margin-bottom: 8px;",
            tags$p(remark_time, style = "font-size: 10px; color: grey; text-align: right; margin: 0;"),
            tags$p(remark_text, style = "font-size: 12px; color: black; text-align: left; margin: 0;")
          )
        })
      )
    } else {
      tags$p("暂无留言", style = "font-size: 12px; color: grey;")
    }
  })
}

# 生成格式化的便签留言
format_remark <- function(raw_remark, system_type) {
  if (is.null(raw_remark) || raw_remark == "") {
    return(NA_character_)
  }

  remark_prefix <- if (system_type == "cn") "[京]" else "[圳]"  # 根据系统类型添加前缀
  formatted_remark <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), ": ", remark_prefix, " ", raw_remark)

  return(formatted_remark)
}

# 库存数计算
process_data <- function(dat) {
  domestic <- dat %>% filter(Status == "国内入库")
  logistics <- dat %>% filter(Status == "国内出库" & !is.na(IntlTracking))
  us <- dat %>% filter(Status == "美国入库")
  sold <- dat %>% filter(Status %in% c("国内售出", "美国调货", "美国发货", "交易完毕"))
  
  list(
    domestic = list(
      count = nrow(domestic),
      value = sum(domestic$ProductCost, na.rm = TRUE),
      shipping = sum(domestic$IntlShippingCost + domestic$DomesticShippingCost, na.rm = TRUE)
    ),
    logistics = list(
      count = nrow(logistics),
      value = sum(logistics$ProductCost, na.rm = TRUE),
      shipping = sum(logistics$IntlShippingCost + logistics$DomesticShippingCost, na.rm = TRUE)
    ),
    us = list(
      count = nrow(us),
      value = sum(us$ProductCost, na.rm = TRUE),
      shipping = sum(us$IntlShippingCost + us$DomesticShippingCost, na.rm = TRUE)
    ),
    sold = list(
      count = nrow(sold),
      us_shipping_count = nrow(sold %>% filter(Status == "交易完毕")),
      value = sum(sold$ProductCost, na.rm = TRUE),
      shipping = sum(sold$IntlShippingCost + sold$DomesticShippingCost, na.rm = TRUE)
    )
  )
}

# 计算总货值和总运费
calculate_totals <- function(data) {
  total_value <- sum(c(
    data$domestic$value,
    data$logistics$value,
    data$us$value,
    data$sold$value
  ), na.rm = TRUE)
  
  total_shipping <- sum(c(
    data$domestic$shipping,
    data$logistics$shipping,
    data$us$shipping,
    data$sold$shipping
  ), na.rm = TRUE)
  
  list(total_value = total_value, total_shipping = total_shipping)
}

# 扩展 Shiny 的 JavaScript 朗读功能
speak_text <- function(text) {
  js_code <- sprintf("
    if (!window.speechSynthesis) {
      console.error('当前浏览器不支持语音合成');
    } else {
      var msg = new SpeechSynthesisUtterance('%s');
      msg.lang = 'zh-CN';
      window.speechSynthesis.speak(msg);
    }
  ", text)
  
  shinyjs::runjs(js_code)  # 运行 JavaScript 语音朗读
}

# 自定义函数
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

# 加载时清除无效和冗余的状态流转记录
clear_invalid_item_status_history <- function(con) {
  tryCatch({
    # 删除不以 "采购" 为起始状态的记录
    dbExecute(con, "
      DELETE FROM item_status_history
      WHERE UniqueID IN (
        SELECT UniqueID
        FROM (
          SELECT UniqueID, MIN(previous_status_timestamp) AS first_change_time
          FROM item_status_history
          GROUP BY UniqueID
          HAVING MAX(CASE WHEN previous_status = '采购' THEN 1 ELSE 0 END) = 0
        ) subquery
      )
    ")
  }, error = function(e) {
    showNotification(paste("清除无效记录失败：", e$message), type = "error")
  })
}
