library(DBI)
library(httr)

# 日志文件路径
log_file <<- "/var/log/update_dhl_tracking_status.log"

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

# 日志记录函数
log_message <- function(msg, reset = FALSE) {
  
  # 如果需要清空日志
  if (reset) {
    fileConn <- file(log_file, "w")  # 以写入模式打开文件（截断文件内容）
    close(fileConn)
  }
  
  cat(sprintf("[%s] %s\n", Sys.time(), msg), file = log_file, append = TRUE)
}

# 查询 DHL Tracking 信息
get_dhl_tracking_info <- function(tracking_number) {
  base_url <- "https://api-eu.dhl.com/track/shipments"
  api_key <- Sys.getenv("DHL_API_KEY")
  
  if (is.null(tracking_number) || tracking_number == "") {
    stop("Invalid tracking number.")
  }
  
  response <- tryCatch({
    httr::GET(
      url = base_url,
      httr::add_headers(`DHL-API-Key` = api_key),
      query = list(trackingNumber = tracking_number, language = "en")
    )
  }, error = function(e) {
    stop("DHL 请求失败：", e$message)
  })
  
  if (httr::status_code(response) == 200) {
    log_message(paste("DHL Tracking info retrieved for:", tracking_number))
    return(httr::content(response, as = "parsed"))
  } else {
    warning("DHL API 请求失败：", httr::content(response, as = "text"))
    return(NULL)
  }
}

# 获取最新状态描述
get_latest_event_description <- function(tracking_result) {
  # 检查是否有有效的事件
  if (is.null(tracking_result$shipments[[1]]$events) || length(tracking_result$shipments[[1]]$events) == 0) {
    return(NA)  # 无事件，返回 NA
  }
  
  # 提取最新事件
  latest_event <- tracking_result$shipments[[1]]$events[[1]]
  
  # 检查并返回 description
  if (!is.null(latest_event$description)) {
    return(latest_event$description)
  } else {
    return(NA)  # 无描述，返回 NA
  }
}

# 描述转化为状态
extract_dhl_status <- function(latest_description) {
  if (is.null(latest_description) || latest_description == "") {
    return(NA)  # 如果 description 为空，返回 NA
  }
  
  # 根据 DHL 返回的事件描述更新状态
  status <- dplyr::case_when(
    grepl("Shipment picked up", latest_description, ignore.case = TRUE) ~ "包裹发出",
    grepl("Delivered", latest_description, ignore.case = TRUE) ~ "包裹送达",
    TRUE ~ "在途运输"  # 默认状态
  )
  
  return(status)
}

get_logged_tracking_numbers <- function(log_file) {
  if (!file.exists(log_file)) {
    return(character(0))  # 如果日志文件不存在，返回空集合
  }
  
  # 读取日志文件
  log_content <- readLines(log_file, warn = FALSE)
  
  # 提取包含 "Updated status for:" 的行
  tracking_lines <- grep("Updated status for:", log_content, value = TRUE)
  
  # 提取 TrackingNumber
  tracking_numbers <- sub(".*Updated status for: ([^ ]+) .*", "\\1", tracking_lines)
  
  return(unique(tracking_numbers))  # 返回唯一的 TrackingNumber 集合
}

# 更新国际物流运单状态
update_dhl_tracking_status <- function() {
  log_message("DHL Tracking status update started.")
  
  con <- db_connection()
  
  # 获取 API Key
  api_key <- Sys.getenv("DHL_API_KEY")
  
  # 查询需要更新状态的运单
  eligible_shipments <- dbGetQuery(con, "
    SELECT TrackingNumber, Status, UpdatedAt 
    FROM intl_shipments
    WHERE Status IN ('运单创建', '包裹发出', '在途运输')
  ")
  
  # 如果没有运单，直接退出
  if (nrow(eligible_shipments) == 0) {
    log_message("No eligible DHL shipments for update.")
    dbDisconnect(con)
    return()
  }
  
  # 从日志文件中提取已登记的 TrackingNumber
  logged_tracking_numbers <- get_logged_tracking_numbers(log_file)
  current_tracking_numbers <- eligible_shipments$TrackingNumber
  
  # 找到新的 TrackingNumber
  new_tracking_numbers <- setdiff(current_tracking_numbers, logged_tracking_numbers)
  
  # 如果有新运单，清空日志
  if (length(new_tracking_numbers) > 0) {
    log_message("New eligible DHL shipments detected. Clearing log...", reset = TRUE)
  } else {
    log_message("No new shipments detected. Continuing with existing log.")
  }
  
  for (i in 1:nrow(eligible_shipments)) {
    shipment <- eligible_shipments[i, ]
    tracking_number <- shipment$TrackingNumber
    log_message(paste("Updating tracking number:", tracking_number))
    
    # 获取 DHL 跟踪信息
    tracking_result <- get_dhl_tracking_info(tracking_number)
    
    latest_description <- get_latest_event_description(tracking_result)
    
    if (!is.null(latest_description)) {
      # 提取最新状态
      new_status <- extract_dhl_status(latest_description)
      
      if (!is.na(new_status) && new_status != shipment$Status) {
        # 更新数据库中的状态
        dbExecute(
          con,
          "UPDATE intl_shipments 
           SET Status = ?, UpdatedAt = CURRENT_TIMESTAMP 
           WHERE TrackingNumber = ?",
          params = list(new_status, tracking_number)
        )
        log_message(paste("Updated status for:", tracking_number, "to:", new_status))
      } else {
        log_message(paste("No status update needed for:", tracking_number))
      }
    } else {
      log_message(paste("Failed to retrieve tracking info for:", tracking_number))
    }
  }
  
  dbDisconnect(con)
  log_message("DHL Tracking status update completed.")
}

### 

update_dhl_tracking_status()
