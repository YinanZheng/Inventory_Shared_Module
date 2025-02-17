library(DBI)
library(httr)

# 日志文件路径
log_file <<- "/var/log//update_usps_tracking_status.log"

# 清理日志函数
clear_log_daily <- function(log_file) {
  if (file.exists(log_file)) {
    file_info <- file.info(log_file)
    # 如果日志文件上次修改时间不是今天，清空日志
    if (as.Date(file_info$mtime) < Sys.Date()) {
      file.create(log_file)  # 清空日志
    }
  } else {
    # 如果日志文件不存在，创建新文件
    dir.create(dirname(log_file), showWarnings = FALSE, recursive = TRUE)
    file.create(log_file)
  }
}

# 日志记录函数
log_message <- function(msg) {
  cat(sprintf("[%s] %s\n", Sys.time(), msg), file = log_file, append = TRUE)
}

# Connect to backend database
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


# 定义需要记录时间的状态
status_columns <<- list(
  "采购" = "PurchaseTime",
  "国内入库" = "DomesticEntryTime",
  "国内出库" = "DomesticExitTime",
  "国内售出" = "DomesticSoldTime",
  "美国入库" = "UsEntryTime",
  "美国发货" = "UsShippingTime",
  "美国调货" = "UsRelocationTime",
  "交易完毕" = "CompleteTime"
)

### USPS API functions

# 获取 Access Token
get_access_token <- function(client_id, client_secret) {
  # Token 文件路径
  token_file <- "/srv/shiny-server/erp-module/data/token_data.rds"
  
  # 检查是否有已存储的 Token
  if (file.exists(token_file)) {
    token_data <- readRDS(token_file)
    # 检查 Token 是否有效
    if (Sys.time() < token_data$expiry) {
      message("Using cached token.")
      return(token_data$token)
    }
  }
  
  # 如果没有 Token 或已过期，重新请求
  response <- httr::POST(
    url = "https://apis.usps.com/oauth2/v3/token",
    body = list(
      grant_type = "client_credentials",
      client_id = client_id,
      client_secret = client_secret
    ),
    encode = "form"
  )
  
  if (httr::status_code(response) == 200) {
    token_data <- httr::content(response, as = "parsed")
    access_token <- token_data$access_token
    expiry <- Sys.time() + as.numeric(token_data$expires_in) - 60  # 提前 1 分钟失效
    
    # 存储 Token 和过期时间
    saveRDS(list(token = access_token, expiry = expiry), token_file)
    message("New token requested and saved.")
    return(access_token)
  } else {
    log_message("Failed to get access token.")
    return()
  }
}

# 查询Tracking状态函数
get_tracking_info <- function(tracking_number, access_token, request_counter, request_timestamp) {
  if (is.null(tracking_number) || tracking_number == "") {
    log_message("Error: Invalid tracking number.")
    return(list(
      content = NULL,
      statusSummary = "Invalid tracking number",
      request_counter = request_counter,
      request_timestamp = request_timestamp
    ))
  }
  
  if (request_counter >= 30 && Sys.time() < request_timestamp + lubridate::hours(1)) {
    log_message("Error: Hourly request limit reached. Waiting for reset.")
    return(list(
      content = NULL,
      statusSummary = "Hourly request limit reached",
      request_counter = request_counter,
      request_timestamp = request_timestamp
    ))
  }
  
  # 生成 API 请求 URL
  url <- paste0("https://apis.usps.com/tracking/v3/tracking/", tracking_number)
  log_message(paste("Requesting URL:", url))
  
  # 执行 API 请求
  response <- GET(url, add_headers(Authorization = paste("Bearer", access_token)))
  
  # 记录状态码
  status <- status_code(response)
  log_message(paste("Response Status:", status))
  
  # 更新请求计数器
  if (Sys.time() >= request_timestamp + lubridate::hours(1)) {
    request_counter <- 1
    request_timestamp <- Sys.time()
  } else {
    request_counter <- request_counter + 1
  }
  
  # 处理 API 响应
  if (status == 200) {
    tracking_data <- httr::content(response, as = "parsed")
    
    # 处理 API 响应结构
    status_summary <- tracking_data$statusSummary
    if (is.null(status_summary) || status_summary == "") {
      if (!is.null(tracking_data$eventSummaries) && length(tracking_data$eventSummaries) > 0) {
        status_summary <- tracking_data$eventSummaries[[1]]
      } else {
        status_summary <- "No status available"
      }
    }
    
    return(list(
      content = tracking_data,
      statusSummary = status_summary,
      request_counter = request_counter,
      request_timestamp = request_timestamp
    ))
  } else {
    # 解析 API 失败响应
    response_content <- content(response, as = "text", encoding = "UTF-8")
    
    # 统一错误日志输出
    log_message(paste("Tracking request failed. Status Code:", status, "| Response:", response_content))
    
    return(list(
      content = NULL,
      statusSummary = paste("Request Failed - Status Code:", status),
      request_counter = request_counter,
      request_timestamp = request_timestamp
    ))
  }
}

# 更新订单状态
update_order_status <- function(order_id, new_status, con) {
  dbExecute(
    con,
    "UPDATE orders SET OrderStatus = ?, updated_at = CURRENT_TIMESTAMP WHERE OrderID = ?",
    params = list(new_status, order_id)
  )

  # 果订单状态变为 '送达'，则更新物品状态为 '交易完毕'
  if (new_status == "送达") {
    order_items <- dbGetQuery(con, "SELECT UniqueID FROM unique_items WHERE OrderID = ?", params = list(order_id))
    
    if (nrow(order_items) > 0) {
      for (item in order_items$UniqueID) {
        update_status(con, unique_id = item, new_status = "交易完毕")
      }
      log_message(paste("订单", order_id, "的所有物品状态已更新为 '交易完毕'"))
    } else {
      log_message(paste("订单", order_id, "没有关联的物品，无需更新"))
    }
  }
}

# 状态映射规则
extract_latest_status <- function(eventSummaries) {
  # 检查是否有 eventSummaries
  if (is.null(eventSummaries) || length(eventSummaries) == 0) {
    return(NA)
  }
  
  # 提取第一条记录
  latest_event <- eventSummaries[[1]]
  
  if (is.null(latest_event) || latest_event == "") {
    return(NA)
  }
  
  # 匹配状态
  status <- dplyr::case_when(
    grepl("Pre-Shipment|Awaiting Item|Shipping Label Created|label has been prepared|expect your package for mailing", latest_event) ~ "装箱",
    grepl("picked up|in possession|Departed Post Office", latest_event) ~ "发出",
    grepl("In Transit|Departed|Arrived at|Out for Delivery|item arrived at|item departed|is moving|on its way|out for delivery|in transit to|anticipate delivery", latest_event) ~ "在途",
    grepl("item was delivered", latest_event) ~ "送达"
  )
  
  return(status)
}

# 订单状态更新主逻辑
update_usps_tracking_status <- function() {
  # 清理日志文件
  clear_log_daily(log_file)
  
  # 日志记录
  log_message("Tracking status update started.")
  
  # 数据库连接信息
  con <- db_connection()
  
  # USPS API credentials
  client_id <<- Sys.getenv("USPS_CLIENT_ID")
  client_secret <<- Sys.getenv("USPS_CLIENT_SECRET")
  
  # 获取 Access Token
  token <- get_access_token(client_id, client_secret)
  
  # 初始化计数器和时间戳
  request_counter <- 0
  request_timestamp <- Sys.time()
  
  # 查询需要更新的订单
  eligible_orders <- dbGetQuery(con, "
    SELECT OrderID, UsTrackingNumber, OrderStatus, updated_at
    FROM orders
    WHERE OrderStatus IN ('装箱', '发出', '在途')
      AND (updated_at IS NULL OR TIMESTAMPDIFF(HOUR, updated_at, NOW()) >= 8)
  ")
  
  if (nrow(eligible_orders) == 0) {
    log_message("No eligible orders for update.")
    dbDisconnect(con)
    return()
  }
  
  log_message(paste(nrow(eligible_orders), "packages to be updated."))
  
  # 遍历符合条件的订单
  for (i in 1:nrow(eligible_orders)) {
    order <- eligible_orders[i, ]
    log_message(paste("Updating tracking number:", order$UsTrackingNumber))
    
    tracking_result <- get_tracking_info(order$UsTrackingNumber, token, request_counter, request_timestamp)
    
    if (!is.null(tracking_result)) {
      request_counter <- tracking_result$request_counter
      request_timestamp <- tracking_result$request_timestamp
      new_status <- extract_latest_status(tracking_result$content$eventSummaries)
      
      if (is.na(new_status)) {
        log_message(paste("NA Status for tracking number:", order$UsTrackingNumber))
        log_message(paste("Event Summaries:", tracking_result$content$eventSummaries))
      } else {
        log_message(paste(order$OrderStatus, "-->", new_status))
        update_order_status(order$OrderID, new_status, con)
      }
    } else {
      log_message(paste("No tracking result returned for tracking number:", order$UsTrackingNumber))
    }
    log_message("-------------------------------------------")
  }
  dbDisconnect(con)
  log_message("Tracking status update completed.")
}

### 

update_usps_tracking_status()
