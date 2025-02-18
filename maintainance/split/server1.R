# Define server logic
server <- function(input, output, session) {
  
  source("global.R", local = TRUE)
  
  ##############################################################################
  
  # 显示加载动画
  plan(multicore)  # 让数据加载异步执行，避免阻塞 UI
  shinyjs::show("loading-screen")  # 显示加载界面
  
  future({
    return(TRUE)  # 任务完成
  }) %>% 
    promises::then(function(result) {
      shinyjs::runjs("$('#loading-screen').fadeOut(1000);")  # 1秒淡出加载界面
    })
  
  ##############################################################################
  
  # Database
  con <- db_connection()
  
  # ReactiveVal 存储 item_type_data 数据
  item_type_data <- reactiveVal()
  
  # ReactiveVal 存储 完整 maker_list 数据
  maker_list <- reactiveVal()
  
  # 存储目前数据库中存在的makers与item_names
  makers_items_map <- reactiveVal(NULL)
  
  # 触发unique_items_data刷新
  unique_items_data_refresh_trigger <- reactiveVal(FALSE)
  
  # 触发inventory刷新
  inventory_refresh_trigger <- reactiveVal(FALSE)
  
  # 触发order刷新
  orders_refresh_trigger <- reactiveVal(FALSE)
  
  # 用于存储 barcode PDF 文件路径
  barcode_pdf_file_path <- reactiveVal(NULL)
  
  # 用于存储运单 PDF 文件路径
  label_pdf_file_path <- reactiveVal(NULL)
  
  # 初始化货架和箱子内物品（售出分页）
  shelf_items <- reactiveVal(create_empty_shelf_box())
  box_items <- reactiveVal(create_empty_shelf_box())
  
  # 创建全局环境变量用于存储缓存数据
  cache_env <- new.env()
  
  ####################################################################################################################################
  
  # 应用启动时加载数据: item_type_data
  observe({
    tryCatch({
      item_type_data(dbGetQuery(con, "SELECT * FROM item_type_data"))
    }, error = function(e) {
      item_type_data(NULL)  # 如果出错，设为空值
      showNotification("Initiation: Failed to load item type data.", type = "error")
    })
  })
  
  # 应用启动时加载数据: maker list
  observe({
    tryCatch({
      maker_list(dbGetQuery(con, "SELECT Name AS Maker, Pinyin FROM maker_list ORDER BY Pinyin ASC"))
    }, error = function(e) {
      maker_list(NULL)  # 如果出错，设为空值
      showNotification("Initiation: Failed to load maker list data.", type = "error")
    })
  })
  
  # 更新orders表中已有运单pdf的情况
  update_label_status_column(con)
  
  ####################################################################################################################################
  
  # 库存表
  inventory <- reactive({
    inventory_refresh_trigger()
    dbGetQuery(con, "SELECT * FROM inventory")
  })
  
  # 商品名自动联想
  item_names <- reactive({
    req(inventory())
    unique(inventory()$ItemName)  # 提取唯一的商品名
  })
  
  ####################################################################################################################################
  
  # 物品追踪表
  unique_items_data <- reactivePoll(
    intervalMillis = poll_interval, 
    session = session,       # 绑定 Shiny session，确保只在活跃时运行
    
    # **检查是否需要更新**（返回最近更新时间）
    checkFunc = function() {
      db_time <- dbGetQuery(con, "SELECT MAX(updated_at) FROM unique_items")[[1]]
      trigger_val <- unique_items_data_refresh_trigger()
      paste(db_time, trigger_val)
    },
    
    # **获取最新数据**
    valueFunc = function() {
      result <- dbGetQuery(con, "
      SELECT 
        unique_items.UniqueID, 
        unique_items.SKU, 
        unique_items.OrderID,
        unique_items.ProductCost,
        unique_items.DomesticShippingCost,
        unique_items.Status,
        unique_items.Defect,
        unique_items.DefectNotes,
        unique_items.IntlShippingMethod,
        unique_items.IntlTracking,
        unique_items.IntlShippingCost,
        unique_items.PurchaseTime,
        unique_items.DomesticEntryTime,
        unique_items.DomesticExitTime,
        unique_items.DomesticSoldTime,
        unique_items.UsEntryTime,
        unique_items.UsShippingTime,
        unique_items.UsRelocationTime,
        unique_items.PurchaseCheck,
        unique_items.updated_at,
        inventory.Maker,
        inventory.MajorType,
        inventory.MinorType,
        inventory.ItemName,
        inventory.ItemImagePath
      FROM 
        unique_items
      JOIN 
        inventory 
      ON 
        unique_items.SKU = inventory.SKU
      ORDER BY 
        unique_items.updated_at DESC
    ")
      
      # **当 `unique_items` 变更时，自动更新 `inventory`**
      dbExecute(con, "
      UPDATE inventory i
      JOIN (
        SELECT 
          SKU,
          AVG(ProductCost) AS AvgProductCost,
          AVG(DomesticShippingCost + IntlShippingCost) AS AvgShippingCost,
          SUM(Status IN ('国内入库', '国内出库', '美国入库')) AS TotalQuantity,
          SUM(Status = '国内入库') AS DomesticQuantity,
          SUM(Status = '国内出库') AS TransitQuantity,
          SUM(Status = '美国入库') AS UsQuantity,
          MAX(updated_at) AS LatestUpdateTime
        FROM unique_items
        GROUP BY SKU
      ) u ON i.SKU = u.SKU
      SET 
        i.ProductCost = ROUND(u.AvgProductCost, 2),
        i.ShippingCost = ROUND(u.AvgShippingCost, 2),
        i.Quantity = u.TotalQuantity,
        i.DomesticQuantity = u.DomesticQuantity,
        i.TransitQuantity = u.TransitQuantity,
        i.UsQuantity = u.UsQuantity,
        i.updated_at = u.LatestUpdateTime
    ")
      
      # **删除 `inventory` 中 SKU 在 `unique_items` 中不存在的物品**
      dbExecute(con, "
      DELETE FROM inventory 
      WHERE SKU NOT IN (SELECT DISTINCT SKU FROM unique_items)
    ")
      
      return(result)
    }
  )
  
  # 加载当前已有的 makers 和 item names 的对应关系
  observe({
    unique_data <- unique_items_data()  # 数据源
    makers_items <- unique_data %>%
      select(Maker, ItemName) %>%  # 选择需要的列
      distinct()                   # 确保唯一性
    
    makers_items_map(makers_items)  # 更新 reactiveVal
  })
  
  ####################################################################################################################################
  
  # 订单表
  orders <- reactive({
    orders_refresh_trigger()
    dbGetQuery(con, "SELECT * FROM orders")
  })

  ####################################################################################################################################
  
  clear_invalid_item_status_history(con)
  
  ####################################################################################################################################
  
  
  
  ############################################
  ######   unique_items_data 表的过滤   ######
  ############################################
  
  # 采购页过滤
  filtered_unique_items_data_purchase <- reactive({
    req(unique_items_data())
    data <- unique_items_data()
    
    # 根据输入进行进一步过滤
    data <- filter_unique_items_data_by_inputs(
      data = data,
      input = input,
      maker_input_id = "purchase_filter-maker",
      status_input_id = "purchase_filter-status",
      item_name_input_id = "purchase_filter-name"
    )
    
    # 统计 SKU, Status, 和 PurchaseTime 下的数量
    data <- data %>%
      group_by(SKU, Status, PurchaseTime) %>%
      mutate(ItemCount = n()) %>%  # 统计数量
      ungroup()
    
    # 去重：仅保留每个 SKU 和采购日期组合的第一条记录
    data <- data %>%
      arrange(desc(Status == "采购"), desc(updated_at)) %>%  # 按需求排序
      distinct(SKU, Status, PurchaseTime, .keep_all = TRUE)         # 去重，保留所有列
    
    data
  })
  
  # 入库页过滤
  filtered_unique_items_data_inbound <- reactive({
    req(unique_items_data())
    data <- unique_items_data()
    
    # 只显示本页相关状态
    data <- data %>%
      filter(Status %in% c("采购", "国内入库"))
    
    data <- filter_unique_items_data_by_inputs(
      data = data,
      input = input,
      maker_input_id = "inbound_filter-maker",
      status_input_id = "inbound_filter-status",
      item_name_input_id = "inbound_filter-name",
      purchase_date_range_id = "inbound_filter-purchase_date_range"
    )
    
    # 统计 SKU, Status, Defect, 和 PurchaseTime 下的数量
    data <- data %>%
      group_by(SKU, Status, Defect, PurchaseTime) %>%
      mutate(ItemCount = n()) %>%  # 条件统计数量
      ungroup()
    
    # 去重：仅保留每个 SKU 和组合的第一条记录
    data <- data %>%
      arrange(desc(updated_at)) %>%  # 按需求排序
      distinct(SKU, Status, Defect, PurchaseTime, .keep_all = TRUE)         # 去重，保留所有列
    
    data
  })
  
  # 出库页过滤
  filtered_unique_items_data_outbound <- reactive({
    req(unique_items_data())
    data <- unique_items_data()
    
    # 只显示本页相关状态
    data <- data %>%
      filter(Status %in% c("国内入库", "国内出库"), Defect != "瑕疵")
    
    data <- filter_unique_items_data_by_inputs(
      data = data,
      input = input,
      maker_input_id = "outbound_filter-maker",
      status_input_id = "outbound_filter-status",
      item_name_input_id = "outbound_filter-name",
      purchase_date_range_id = "outbound_filter-purchase_date_range"
    )
    
    # 统计 SKU, Status, 和 PurchaseTime 下的数量（仅统计非瑕疵状态）
    data <- data %>%
      group_by(SKU, Status, PurchaseTime) %>%
      mutate(ItemCount = n()) %>%  # 条件统计数量
      ungroup()
    
    # 去重：仅保留每个 SKU 和组合的第一条记录
    data <- data %>%
      arrange(desc(updated_at)) %>%  # 按需求排序
      distinct(SKU, Status, PurchaseTime, .keep_all = TRUE)         # 去重，保留所有列
    
    data
  })
  
  # 售出-物品售出分页过滤
  filtered_unique_items_data_sold <- reactive({
    req(unique_items_data())
    data <- unique_items_data()
    
    # 只显示本页相关状态
    data <- data %>%
      filter(Status %in% c("国内入库", "国内出库", "美国入库", "美国调货", "国内售出"), Defect != "瑕疵")

    data <- filter_unique_items_data_by_inputs(
      data = data,
      input = input,
      maker_input_id = "sold_filter-maker",
      status_input_id = "sold_filter-status",
      item_name_input_id = "sold_filter-name",
      purchase_date_range_id = "sold_filter-purchase_date_range"
    )
    
    # 统计 SKU, Status, 和 PurchaseTime 下的数量（仅统计非瑕疵状态）
    data <- data %>%
      group_by(SKU, Status, PurchaseTime) %>%
      mutate(ItemCount = n()) %>%  # 条件统计数量
      ungroup()
    
    
    # 去重：仅保留每个 SKU 和组合的第一条记录
    data <- data %>%
      arrange(desc(updated_at)) %>%  # 按需求排序
      distinct(SKU, Status, PurchaseTime, .keep_all = TRUE)         # 去重，保留所有列
    
    data
  })
  
  # 售出-订单管理分页过滤
  debounced_item_name <- debounce(
    reactive({ trimws(input[["sold-item_name"]]) }),  # 确保输入值是去除空格的
    millis = 300  # 设置防抖时间为 300 毫秒（可根据需要调整）
  )
  
  filtered_orders <- reactive({
    req(orders())  # 确保订单数据存在
    
    data <- orders()  # 获取所有订单数据
    
    # 根据订单号筛选
    if (!is.null(input$filter_order_id) && input$filter_order_id != "") {
      data <- data %>% filter(grepl(trimws(input$filter_order_id), OrderID, ignore.case = TRUE))
    }
    
    # 根据运单号筛选，处理前缀多余情况
    if (!is.null(input$filter_tracking_id) && input$filter_tracking_id != "") {
      data <- match_tracking_number(data, "UsTrackingNumber", input$filter_tracking_id)
    }

    # 根据顾客姓名筛选
    if (!is.null(input$filter_customer_name) && input$filter_customer_name != "") {
      data <- data %>% filter(grepl(trimws(input$filter_customer_name), CustomerName, ignore.case = TRUE))
    }
    
    # 根据顾客网名筛选
    if (!is.null(input$filter_customer_netname) && input$filter_customer_netname != "") {
      data <- data %>% filter(grepl(trimws(input$filter_customer_netname), CustomerNetName, ignore.case = TRUE))
    }
    
    # 根据电商平台筛选
    if (!is.null(input$filter_platform) && input$filter_platform != "") {
      data <- data %>% filter(Platform == input$filter_platform)
    }
    
    # 根据订单状态筛选
    if (!is.null(input$filter_order_status) && input$filter_order_status != "") {
      data <- data %>% filter(OrderStatus == input$filter_order_status)
    }
    
    # 根据 SKU 或商品名筛选
    req(unique_items_data())  # 确保 unique_items_data 数据存在
    
    # 筛选包含所输入 SKU 或商品名的订单
    if (!is.null(input$filter_sku) && input$filter_sku != "") {
      sku_orders <- unique_items_data() %>%
        filter(SKU == trimws(input$filter_sku)) %>%
        pull(OrderID) %>%  # 提取与 SKU 相关的订单号
        unique()
      
      data <- data %>% filter(OrderID %in% sku_orders)
    }
    
    item_name <- debounced_item_name()
    if (!is.null(item_name) && length(item_name) > 0 && nzchar(item_name)) {
      item_orders <- unique_items_data() %>%
        filter(grepl(debounced_item_name(), ItemName, ignore.case = TRUE)) %>%
        pull(OrderID) %>%  # 提取与商品名相关的订单号
        unique()
      data <- data %>% filter(OrderID %in% item_orders)
    }
    
    # 根据创建时间筛选
    if (!is.null(input$filter_order_date) && !is.null(input$filter_order_date[[1]]) && !is.null(input$filter_order_date[[2]])) {
      start_date <- input$filter_order_date[[1]]
      end_date <- input$filter_order_date[[2]]
      data <- data %>% filter(created_at >= start_date & created_at <= end_date)
    }
    
    # 根据订单备注筛选
    if (!is.null(input$filter_order_notes) && input$filter_order_notes != "") {
      data <- data %>% filter(grepl(input$filter_order_notes, OrderNotes, ignore.case = TRUE))
    }
    
    # 按录入时间倒序排列
    data <- data %>% arrange(desc(created_at))
    
    data
  })
  
  ###
  
  # 物品管理页过滤
  filtered_unique_items_data_manage <- reactive({
    req(unique_items_data())
    data <- unique_items_data()
    
    data <- filter_unique_items_data_by_inputs(
      data = data,
      input = input,
      maker_input_id = "manage_filter-maker",
      status_input_id = "manage_filter-status",
      item_name_input_id = "manage_filter-name",
      purchase_date_range_id = "manage_filter-purchase_date_range"
    )
    
    data
  })
  
  # 瑕疵品管理页过滤
  filtered_unique_items_data_defect <- reactive({
    req(unique_items_data())
    data <- unique_items_data()
    
    data <- filter_unique_items_data_by_inputs(
      data = data,
      input = input,
      maker_input_id = "defect_filter-maker",
      item_name_input_id = "defect_filter-name",
      purchase_date_range_id = "defect_filter-purchase_date_range"
    )
   
    # 默认过滤条件：状态为“国内入库”且 Defect 不为“未知”
    data <- data[!is.na(data$Defect) & data$Defect != "未知" & data$Status == "国内入库", ]

    # 处理开关互斥逻辑
    if (isTRUE(input$show_defects_only)) {
      # 如果仅显示瑕疵品
      data <- data[data$Defect == "瑕疵", ]
    } else if (isTRUE(input$show_perfects_only)) {
      # 如果仅显示无瑕品
      data <- data[data$Defect == "无瑕", ]
    }

    data
  })
  
  # 国际物流筛选
  filtered_unique_items_data_logistics <- reactive({
    req(unique_items_data())
    data <- unique_items_data()
    
    # # 只显示本页相关状态
    # data <- data %>%
    #   filter(Status %in% c("国内出库", "国内售出"), Defect != "瑕疵")

    data <- filter_unique_items_data_by_inputs(
      data = data,
      input = input,
      maker_input_id = "logistic_filter-maker",
      status_input_id = "logistic_filter-status",
      item_name_input_id = "logistic_filter-name",
      sold_date_range_id = "logistic_filter-sold_date_range",
      only_show_sold_id = "logistic_filter-only_show_sold",
      exit_date_range_id = "logistic_filter-exit_date_range",
      only_show_exit_id = "logistic_filter-only_show_exit"
    )
    
    shipping_method <- input$intl_shipping_method
    
    # 判断并根据物流方式筛选
    if (!is.null(shipping_method)) {
      data <- data %>% filter(IntlShippingMethod == shipping_method)
    }
    
    # 优先显示没有国际运单号的物品
    data <- data %>% arrange(is.na(IntlTracking), IntlTracking)
    
    data
  })
  
  # 查询页过滤-库存表
  filtered_inventory <- reactive({
    req(inventory(), unique_items_data()) # 确保数据存在
    
    result <- inventory()
    
    # 如果库存为空，返回空库存表
    if (nrow(result) == 0) {
      return(create_empty_inventory())
    }
    
    # 供应商筛选
    if (!is.null(input[["query_filter-maker"]]) && length(input[["query_filter-maker"]]) > 0 && any(input[["query_filter-maker"]] != "")) {
      result <- result %>% filter(Maker %in% input[["query_filter-maker"]])
    }
    
    # 商品名称模糊筛选
    if (!is.null(input[["query_filter-name"]]) && input[["query_filter-name"]] != "") {
      result <- result %>% filter(grepl(input[["query_filter-name"]], ItemName, ignore.case = TRUE))
    }
    
    # 根据售罄筛选
    if (!is.null(input$query_stock_status) && input$query_stock_status != "none") {
      if (input$query_stock_status == "us") {
        result <- result %>% filter(UsQuantity == 0 & DomesticQuantity > 0)  # 美国库存为 0
      } else if (input$query_stock_status == "domestic") {
        result <- result %>% filter(DomesticQuantity == 0 & UsQuantity > 0)  # 国内库存为 0
      } else if (input$query_stock_status == "all") {
        result <- result %>% filter(Quantity == 0)  # 全库存售罄
      }
    }
    
    result <- result[order(result$updated_at, decreasing = TRUE), ]
    return(result)
  })
  
  # 下载页过滤
  filtered_unique_items_data_download <- reactive({
    filter_unique_items_data_by_inputs(
      data = unique_items_data(),
      input = input,
      maker_input_id = "download_maker",
      item_name_input_id = "download_item_name",
      purchase_date_range_id = "download_date_range"
    )
  })
  
  ###########################################################################################################################
  
  
  # 渲染物品追踪数据表
  unique_items_table_purchase_selected_row <- callModule(uniqueItemsTableServer, "unique_items_table_purchase",
                                                         column_mapping <- c(common_columns, list(
                                                           PurchaseTime = "采购日",
                                                           ItemCount = "数量")
                                                         ), selection = "single", data = filtered_unique_items_data_purchase)
  
  unique_items_table_inbound_selected_row <- callModule(uniqueItemsTableServer, "unique_items_table_inbound",
                                                        column_mapping <- c(common_columns, list(
                                                          PurchaseTime = "采购日",
                                                          DomesticEntryTime = "入库日",
                                                          Defect = "瑕疵态",
                                                          ItemCount = "数量")
                                                        ), selection = "single", data = filtered_unique_items_data_inbound)
  
  unique_items_table_outbound_selected_row <- callModule(uniqueItemsTableServer, "unique_items_table_outbound", 
                                                         column_mapping <- c(common_columns, list(
                                                           PurchaseTime = "采购日",
                                                           DomesticExitTime = "出库日",
                                                           ItemCount = "数量")
                                                         ), selection = "single", data = filtered_unique_items_data_outbound)
  
  unique_items_table_sold_selected_row <- callModule(uniqueItemsTableServer, "unique_items_table_sold",
                                                     column_mapping <- c(common_columns, list(
                                                       PurchaseTime = "采购日",
                                                       DomesticSoldTime = "售出日",
                                                       ItemCount = "数量")
                                                     ), selection = "single", data = filtered_unique_items_data_sold)
  
  ####################################################################################################################################
  
  unique_items_table_manage_selected_row <- callModule(uniqueItemsTableServer, "unique_items_table_manage",
                                                       column_mapping <- c(common_columns, list(
                                                         DomesticShippingCost = "国内运费",
                                                         PurchaseTime = "采购日",
                                                         DomesticEntryTime = "入库日",
                                                         DomesticExitTime = "出库日",
                                                         DomesticSoldTime = "售出日",
                                                         UsEntryTime = "美入库日",
                                                         UsRelocationTime = "美调货日",
                                                         UsShippingTime = "美发货日",
                                                         OrderID = "订单号")
                                                       ), selection = "multiple", data = filtered_unique_items_data_manage,
                                                       option = modifyList(table_default_options, list(scrollY = "730px", searching = TRUE)))
  
  unique_items_table_defect_selected_row <- callModule(uniqueItemsTableServer, "unique_items_table_defect",
                                                       column_mapping <- c(common_columns, list(
                                                         PurchaseTime = "采购日",
                                                         DomesticEntryTime = "入库日",
                                                         Defect = "瑕疵态",
                                                         DefectNotes = "瑕疵备注")
                                                       ), selection = "multiple", data = filtered_unique_items_data_defect,
                                                       option = modifyList(table_default_options, list(scrollY = "730px", searching = TRUE)))
  
  unique_items_table_logistics_selected_row <- callModule(uniqueItemsTableServer, "unique_items_table_logistics",
                                                          column_mapping = c(common_columns, list(
                                                            IntlShippingMethod = "国际运输",
                                                            DomesticSoldTime = "售出日",
                                                            DomesticExitTime = "出库日",
                                                            IntlShippingCost = "国际运费",
                                                            IntlTracking = "国际运单"
                                                          )), selection = "multiple",
                                                          data = filtered_unique_items_data_logistics,
                                                          option = modifyList(table_default_options, list(scrollY = "730px", 
                                                                                                          searching = FALSE, 
                                                                                                          paging = TRUE,
                                                                                                          pageLength = 30,
                                                                                                          lengthMenu = c(30, 100, 200),
                                                                                                          dom = 'lftip')))
  
  output$filtered_inventory_table_query <- renderDT({  # input$filtered_inventory_table_query_rows_selected
    column_mapping <- list(
      SKU = "条形码",
      ItemName = "商品名",
      ItemImagePath = "商品图",
      Maker = "供应商",
      MajorType = "大类",
      MinorType = "小类",
      Quantity = "总库存数",
      DomesticQuantity = "国内库存数",
      TransitQuantity = "在途库存数",
      UsQuantity = "美国库存数",
      ProductCost = "平均成本",
      ShippingCost = "平均运费"
    )
    
    render_table_with_images(
      data = filtered_inventory(),
      column_mapping = column_mapping,
      image_column = "ItemImagePath"  # Specify the image column
    )$datatable
  })
  
  unique_items_table_download_selected_row <- callModule(uniqueItemsTableServer, "unique_items_table_download",
                                                         column_mapping <- c(common_columns, list(
                                                           Defect = "瑕疵态",
                                                           PurchaseTime = "采购日",
                                                           DomesticEntryTime = "入库日",
                                                           DomesticExitTime = "出库日",
                                                           DomesticSoldTime = "售出日")
                                                         ), data = filtered_unique_items_data_download)
  
  # 订单管理分页订单表
  selected_order_row <- callModule(orderTableServer, "orders_table_module",
                                   column_mapping = list(
                                     OrderID = "订单号",
                                     OrderImagePath = "订单图",
                                     CustomerName = "姓名",
                                     CustomerNetName = "网名",
                                     Platform = "平台",
                                     TransactionAmount = "成交额",
                                     UsTrackingNumber = "运单号",
                                     LabelStatus = "运单PDF",
                                     OrderStatus = "状态",
                                     OrderNotes = "备注",
                                     created_at = "创建时间"
                                   ),
                                   data = filtered_orders,  # 数据源
                                   selection = "single" # 单选模式
  )
  
  
  
  ####################################################################################################################################
  
  
  
  ################################################################
  ##                                                            ##
  ## 协作分页                                                   ##
  ##                                                            ##
  ################################################################
  
  # 缓存请求数据
  requests_data <- reactiveVal(data.frame())
  
  # 定期检查采购请求数据库的最新数据
  poll_requests <- reactivePoll(
    intervalMillis = poll_interval,
    session = session,
    checkFunc = function() {
      # 查询最新更新时间
      last_updated <- dbGetQuery(con, "SELECT MAX(UpdatedAt) AS last_updated FROM requests")$last_updated[1]
      if (is.null(last_updated)) {
        Sys.time()  # 如果无数据，返回当前时间
      } else {
        last_updated
      }
    },
    valueFunc = function() {
      result <- dbGetQuery(con, "SELECT * FROM requests")
      if (nrow(result) == 0) { data.frame() } else { result }
    }
  )
  
  # 加载数据
  observeEvent(poll_requests(), {
    requests <- poll_requests()
    requests_data(requests)
  })
  
  observe({
    requests <- requests_data()
    
    refresh_board(requests, output)
    
    # 渲染留言内容并绑定按钮事件
    lapply(requests$RequestID, function(request_id) {
      output[[paste0("remarks_", request_id)]] <- renderRemarks(request_id, requests)
      bind_buttons(request_id, requests, input, output, session, con)  # 按 RequestID 动态绑定按钮
    })
  })
  
  # SKU 和物品名输入互斥逻辑
  observeEvent(input$search_sku, {
    # 如果 SKU 搜索框有值，则清空物品名称搜索框
    if (input$search_sku != "") {
      updateTextInput(session, "search_name", value = "")  # 清空物品名称搜索框
    }
  })
  
  # SKU 和物品名输入互斥逻辑
  observeEvent(input$search_name, {
    # 如果物品名称搜索框有值，则清空 SKU 搜索框
    if (input$search_name != "") {
      updateTextInput(session, "search_sku", value = "")  # 清空 SKU 搜索框
    }
  })
  
  # SKU 和物品名称搜索预览
  observeEvent(c(input$search_sku, input$search_name), {
    # 如果两个输入框都为空，则清空预览
    if (input$search_sku == "" && input$search_name == "") {
      output$item_preview <- renderUI({ NULL })
      return()  # 结束逻辑
    }
    
    req(input$search_sku != "" | input$search_name != "")  # 确保至少一个搜索条件不为空
    
    # 获取清理后的输入值
    search_sku <- trimws(input$search_sku)
    search_name <- trimws(input$search_name)
    
    # 使用 unique_items_data() 进行过滤和统计
    result <- unique_items_data() %>%
      filter(
        (SKU == search_sku & search_sku != "") |  # SKU 精准匹配
          (grepl(search_name, ItemName, ignore.case = TRUE) & search_name != "")  # 名称模糊匹配
      ) %>%
      group_by(SKU, ItemName, ItemImagePath) %>%
      summarise(
        DomesticStock = sum(Status == "国内入库", na.rm = TRUE),  # 国内库存
        InTransitStock = sum(Status == "国内出库", na.rm = TRUE),  # 在途库存
        UsStock = sum(Status == "美国入库", na.rm = TRUE),  # 美国库存
        .groups = "drop"
      )
    
    # 动态更新预览界面
    if (nrow(result) > 0) {
      output$item_preview <- renderUI({
        div(
          style = "max-height: 300px; overflow-y: auto; padding: 10px; border: 1px solid #e0e0e0; border-radius: 8px; background-color: #f9f9f9;",
          lapply(1:nrow(result), function(i) {
            item <- result[i, ]
            img_path <- ifelse(
              is.na(item$ItemImagePath),
              placeholder_150px_path,  # 占位符路径
              paste0(host_url, "/images/", basename(item$ItemImagePath))  # 构建完整路径
            )
            div(
              style = "margin-bottom: 15px; padding: 10px; border-bottom: 1px solid #ccc;",
              tags$img(src = img_path, height = "150px", style = "display: block; margin: auto;"),
              tags$h5(item$ItemName, style = "text-align: center; margin-top: 10px;"),
              tags$h5(item$SKU, style = "text-align: center; margin-top: 10px;"),
              div(
                style = "text-align: center; font-size: 12px;",
                tags$span(paste("国内库存:", item$DomesticStock), style = "margin-right: 10px;"),
                tags$span(paste("在途库存:", item$InTransitStock), style = "margin-right: 10px;"),
                tags$span(paste("美国库存:", item$UsStock))
              )
            )
          })
        )
      })
    } else {
      output$item_preview <- renderUI({
        div(tags$p("未找到匹配的物品", style = "color: red; text-align: center;"))
      })
    }
  })
  
  # 库存品请求按钮
  observeEvent(input$add_request, {
    req(input$request_quantity > 0)  # 确保输入合法
    
    # 获取用户输入
    search_sku <- trimws(input$search_sku)
    search_name <- trimws(input$search_name)
    
    # 根据当前页面的类型确定 RequestType
    current_tab <- input$collaboration_tabs
    request_type <- switch(
      current_tab,
      "采购请求" = "采购",
      "出库请求" = "出库",
      return()  # 不匹配任何已知分页时，直接退出
    )
    
    # 检索数据并插入到数据库
    filtered_data <- unique_items_data() %>%
      filter(
        (SKU == search_sku & search_sku != "") |  # SKU 精准匹配
          (grepl(search_name, ItemName, ignore.case = TRUE) & search_name != "")  # 名称模糊匹配
      ) %>%
      distinct(SKU, Maker, ItemName, ItemImagePath)  # 去重
    
    tryCatch({
      # 主逻辑
      if (nrow(filtered_data) == 1) {
        request_id <- uuid::UUIDgenerate()
        
        item_image_path <- ifelse(is.na(filtered_data$ItemImagePath[1]), placeholder_150px_path, filtered_data$ItemImagePath[1])
        item_description <- ifelse(is.na(filtered_data$ItemName[1]), "未知", filtered_data$ItemName[1])

        dbExecute(con, 
                  "INSERT INTO requests (RequestID, SKU, Maker, ItemImagePath, ItemDescription, Quantity, RequestStatus, Remarks, RequestType) 
         VALUES (?, ?, ?, ?, ?, ?, '待处理', ?, ?)", 
                  params = list(request_id, filtered_data$SKU, filtered_data$Maker, item_image_path, item_description, 
                                input$request_quantity, format_remark(input$request_remark, system_type), request_type))
        
        bind_buttons(request_id, requests_data(), input, output, session, con)
        
        updateTextInput(session, "search_sku", value = "")
        updateTextInput(session, "search_name", value = "")
        updateNumericInput(session, "request_quantity", value = 1)
        
        showNotification("请求已成功创建", type = "message")
      } else if (nrow(filtered_data) > 1) {
        showNotification("搜索结果不唯一，请更精确地搜索 SKU 或物品名称", type = "error")
      } else {
        showNotification("未找到匹配的物品，请检查搜索条件", type = "error")
      }
    }, error = function(e) {
      # 捕获错误并打印详细信息
      showNotification(e, type = "error")
    })
  })
  
  # 初始化图片上传模块
  image_requests <- imageModuleServer("image_requests")
  
  # 新商品采购请求按钮
  observeEvent(input$submit_custom_request, {
    # 确保必要字段已填写
    req(input$custom_quantity > 0)
    
    # 获取用户输入
    custom_description <- trimws(input$custom_description)
    custom_quantity <- input$custom_quantity
    
    # 使用图片上传模块的返回数据
    custom_image_path <- process_image_upload(
      sku = "New-Request",  # 自定义物品没有 SKU，可以设置为固定值或动态生成
      file_data = image_requests$uploaded_file(),
      pasted_data = image_requests$pasted_file()
    )
    
    # 检查图片路径是否有效
    req(!is.null(custom_image_path) && !is.na(custom_image_path))
    
    # 生成唯一 RequestID
    request_id <- uuid::UUIDgenerate()
    
    # 将数据插入到数据库
    dbExecute(con, 
              "INSERT INTO requests (RequestID, SKU, Maker, ItemImagePath, ItemDescription, Quantity, RequestStatus, Remarks, RequestType) 
             VALUES (?, ?, '待定', ?, ?, ?, '待处理', ?, '采购')", 
              params = list(request_id, "New-Request", custom_image_path, custom_description, custom_quantity, format_remark(input$custom_remark, system_type)))
    
    bind_buttons(request_id, requests_data(), input, output, session, con) #绑定按钮逻辑
    
    # 清空输入字段
    updateTextInput(session, "custom_description", value = "")
    updateNumericInput(session, "custom_quantity", value = 1)
    image_requests$reset()
    showNotification("自定义请求已成功提交", type = "message")
  })
  
  # 出库标签页要禁用新物品请求
  observeEvent(input$collaboration_tabs, {
    current_tab <- input$collaboration_tabs
    
    if (current_tab == "出库请求") {
      # 禁用与新商品请求相关的控件
      shinyjs::disable("custom_description")
      shinyjs::disable("custom_quantity")
      shinyjs::disable("submit_custom_request")
    } else if (current_tab == "采购请求") {
      # 启用与新商品请求相关的控件
      shinyjs::enable("custom_description")
      shinyjs::enable("custom_quantity")
      shinyjs::enable("submit_custom_request")
    }
  })
  
  # 点击请求图片看大图
  observeEvent(input$view_request_image, {
    showModal(modalDialog(
      title = "请求物品图片",
      div(
        style = "overflow: auto; max-height: 700px; text-align: center;",        
        tags$img(src = input$view_request_image, style = "max-width: 100%; height: auto; display: inline-block;")
      ),
      size = "l",
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  
  
  ################################################################
  ##                                                            ##
  ## 采购分页                                                   ##
  ##                                                            ##
  ################################################################
  
  # 物品表过滤模块
  itemFilterServer(
    id = "purchase_filter",
    makers_items_map = makers_items_map
  )
  
  # 供应商模块
  supplierModuleServer(input, output, session, con, maker_list)
  
  # 物品大小类模块
  typeModuleServer("type_module", con, item_type_data)
  
  
  ### SKU冲撞检查
  
  # 合并依赖变量
  combined_inputs <- reactive({
    list(
      major_type = input[["type_module-new_major_type"]],
      minor_type = input[["type_module-new_minor_type"]],
      new_name = input[["purchase-item_name"]],
      new_maker = input$new_maker
    )
  })
  
  # 使用 debounce 延迟触发，避免短时间多次调用
  debounced_inputs <- debounce(combined_inputs, millis = 300)
  
  observeEvent(debounced_inputs(), {
    inputs <- debounced_inputs()
    
    # 检查 SKU 的来源
    is_from_table <- !is.null(unique_items_table_purchase_selected_row()) && 
      length(unique_items_table_purchase_selected_row()) > 0
    
    # 判断是否需要清空 SKU
    if (is.null(inputs$new_maker) || inputs$new_maker == "" || 
        is.null(inputs$new_name) || inputs$new_name == "") {
      updateTextInput(session, "new_sku", value = "")  # 清空 SKU
      return()
    }
    
    # Dynamically generate SKU
    sku <- generate_sku(
      item_type_data = item_type_data(),
      major_type = inputs$major_type,
      minor_type = inputs$minor_type,
      item_name = input[["purchase-item_name"]],
      maker = inputs$new_maker
    )
    
    if (is_from_table) {
      # 如果 SKU 来源于表格，直接更新输入字段
      updateTextInput(session, "new_sku", value = sku)
      # showNotification("SKU 已生成（来源于表格选择）", type = "message")
    } else {
      # 如果 SKU 不是来源于表格，检查是否冲突
      existing_sku <- inventory() %>% filter(SKU == sku)
      
      if (nrow(existing_sku) > 0) {
        # 如果 SKU 冲突，弹出模态窗口提醒用户
        showModal(modalDialog(
          title = "SKU 冲突",
          paste0("生成的 SKU '", sku, "' 已存在于库存中，请重新生成 SKU！"),
