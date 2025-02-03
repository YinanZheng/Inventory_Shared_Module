library(DBI)

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

# 清理未被记录的图片 (每天运行一次)
clean_untracked_images <- function() {
  # 数据库连接信息
  con <- db_connection()
  
  tryCatch({
    # 1. 获取数据库中记录的图片路径（包括 inventory 和 orders 表）
    inventory_query <- "SELECT ItemImagePath FROM inventory WHERE ItemImagePath IS NOT NULL"
    orders_query <- "SELECT OrderImagePath FROM orders WHERE OrderImagePath IS NOT NULL"
    transactions_query <- "SELECT TransactionImagePath FROM transactions WHERE TransactionImagePath IS NOT NULL"
    requests_query <- "SELECT ItemImagePath FROM requests WHERE ItemImagePath IS NOT NULL"
    
    inventory_paths <- normalizePath(dbGetQuery(con, inventory_query)$ItemImagePath, mustWork = FALSE)
    orders_paths <- normalizePath(dbGetQuery(con, orders_query)$OrderImagePath, mustWork = FALSE)
    transactions_paths <- normalizePath(dbGetQuery(con, transactions_query)$TransactionImagePath, mustWork = FALSE)
    requests_paths <- normalizePath(dbGetQuery(con, requests_query)$ItemImagePath, mustWork = FALSE)
    
    # 合并所有记录路径
    recorded_paths <- unique(c(inventory_paths, orders_paths, transactions_paths, requests_paths))
    
    # 2. 列出目录中所有图片文件，并规范化路径
    all_files <- normalizePath(list.files("/var/www/images/", full.names = TRUE), mustWork = FALSE)
    
    # 3. 检查哪些文件未被记录
    untracked_files <- setdiff(all_files, recorded_paths)
    
    # 4. 删除未被记录的文件
    if (length(untracked_files) > 0) {
      sapply(untracked_files, file.remove)
      message("以下文件已被删除：")
      print(untracked_files)
    } else {
      message("没有未被记录的文件需要清理。")
    }
  }, error = function(e) {
    message("清理过程中出现错误：", e$message)
  })
  
  # 断开数据库连接
  dbDisconnect(con)
}

clean_untracked_images()
