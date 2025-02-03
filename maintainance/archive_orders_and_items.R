library(DBI)

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

archive_orders_and_items <- function() {
  con <- db_connection()
  
  tryCatch({
    # 记录开始时间
    start_time <- Sys.time()
    message(sprintf("[%s] 开始归档任务...", format(start_time, "%Y-%m-%d %H:%M:%S")))
    
    # 开启事务
    dbBegin(con)
    message(sprintf("[%s] 数据库事务开始", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
    
    # 查询归档订单信息
    orders_to_archive <- dbGetQuery(con, "
      SELECT OrderID
      FROM orders
      WHERE OrderStatus = '送达' AND updated_at < DATE_SUB(NOW(), INTERVAL 90 DAY);
    ")
    order_count <- nrow(orders_to_archive)
    
    # 查询归档物品信息
    items_to_archive <- dbGetQuery(con, "
      SELECT UniqueID
      FROM unique_items
      WHERE OrderID IN (
        SELECT OrderID
        FROM orders
        WHERE OrderStatus = '送达' AND updated_at < DATE_SUB(NOW(), INTERVAL 90 DAY)
      );
    ")
    item_count <- nrow(items_to_archive)
    
    # 输出查询结果
    message(sprintf("[%s] 查询完成：%d 条订单，%d 条物品需要归档", 
                    format(Sys.time(), "%Y-%m-%d %H:%M:%S"), order_count, item_count))
    
    # 归档订单到 orders_archive
    dbExecute(con, "
      INSERT INTO orders_archive
      SELECT *
      FROM orders
      WHERE OrderStatus = '送达' AND updated_at < DATE_SUB(NOW(), INTERVAL 90 DAY);
    ")
    message(sprintf("[%s] 已归档 %d 条订单到 orders_archive", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), order_count))
    
    # 归档物品到 unique_items_archive
    dbExecute(con, "
      INSERT INTO unique_items_archive
      SELECT *
      FROM unique_items
      WHERE OrderID IN (
        SELECT OrderID
        FROM orders
        WHERE OrderStatus = '送达' AND updated_at < DATE_SUB(NOW(), INTERVAL 90 DAY)
      );
    ")
    message(sprintf("[%s] 已归档 %d 条物品到 unique_items_archive", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), item_count))
    
    for (item in items_to_archive$UniqueID) {
      update_status(con = con, unique_id = item, new_status = "完成")
    }
    showNotification(sprintf("[%s] 已将 %d 条物品状态更改为“完成”", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), item_count), type = "message")
    
    # 删除 unique_items 中已归档的物品
    dbExecute(con, "
      DELETE FROM unique_items
      WHERE OrderID IN (
        SELECT OrderID
        FROM orders
        WHERE OrderStatus = '送达' AND updated_at < DATE_SUB(NOW(), INTERVAL 90 DAY)
      );
    ")
    message(sprintf("[%s] 已从 unique_items 表中删除 %d 条物品", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), item_count))
    
    # 删除 orders 中已归档的订单
    dbExecute(con, "
      DELETE FROM orders
      WHERE OrderStatus = '送达' AND updated_at < DATE_SUB(NOW(), INTERVAL 90 DAY);
    ")
    message(sprintf("[%s] 已从 orders 表中删除 %d 条订单", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), order_count))
    
    # 提交事务
    dbCommit(con)
    end_time <- Sys.time()
    message(sprintf("[%s] 归档任务完成，总耗时 %.2f 秒", format(end_time, "%Y-%m-%d %H:%M:%S"), as.numeric(difftime(end_time, start_time, units = "secs"))))
    
  }, error = function(e) {
    # 回滚事务
    dbRollback(con)
    message(sprintf("[%s] 归档任务失败：%s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), e$message))
  })
}
