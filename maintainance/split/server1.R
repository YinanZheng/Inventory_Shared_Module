# Define server logic
server <- function(input, output, session) {
  
  source("global.R", local = TRUE)
  
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
  
  # 触发item_status_history刷新
  item_status_history_refresh_trigger <- reactiveVal(FALSE)
  
  # 用于存储条形码 PDF 文件路径
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
    # 当 refresh_trigger 改变时触发更新
    inventory_refresh_trigger()
    
    tryCatch({
      # 从 unique_items 表中计算聚合数据并一次性更新
      dbExecute(
        con,
        "
        UPDATE inventory i
        JOIN (
          SELECT 
            SKU,
            AVG(ProductCost) AS AvgProductCost,
            AVG(DomesticShippingCost + IntlShippingCost) AS AvgShippingCost,
            SUM(Status IN ('国内入库', '国内出库', '美国入库')) AS TotalQuantity,
            SUM(Status = '国内入库') AS DomesticQuantity,
            SUM(Status = '国内出库') AS TransitQuantity,
            SUM(Status = '美国入库') AS UsQuantity
          FROM unique_items
          GROUP BY SKU
        ) u ON i.SKU = u.SKU
        SET 
          i.ProductCost = ROUND(u.AvgProductCost, 2),
          i.ShippingCost = ROUND(u.AvgShippingCost, 2),
          i.Quantity = u.TotalQuantity,
          i.DomesticQuantity = u.DomesticQuantity,
          i.TransitQuantity = u.TransitQuantity,
          i.UsQuantity = u.UsQuantity
        "
      )
      
      # 从 inventory 表中加载最新数据
      updated_inventory <- dbGetQuery(con, "SELECT * FROM inventory")
      return(updated_inventory)
      
    }, error = function(e) {
      showNotification(paste("更新库存表时发生错误：", e$message), type = "error")
      return(create_empty_inventory())  # 返回空的 inventory 数据表
    })
  })
  
  # 商品名自动联想
  item_names <- reactive({
    req(inventory())
    unique(inventory()$ItemName)  # 提取唯一的商品名
  })
  
  # 物品追踪表
  unique_items_data <- reactivePoll(
    intervalMillis = poll_interval, 
    session = session,       # 绑定 Shiny session，确保只在活跃时运行
    
    # **检查是否需要更新**（返回最近更新时间）
    checkFunc = function() {
      dbGetQuery(con, "SELECT MAX(updated_at) FROM unique_items")[[1]]
    },
    
    # **获取最新数据**
    valueFunc = function() {
      dbGetQuery(con, "
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
        unique_items.ReturnTime,
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
  
  # 订单表
  orders <- reactive({
    # 当 refresh_trigger 改变时触发更新
    orders_refresh_trigger()
    dbGetQuery(con, "SELECT * FROM orders")
  })
  
  ####################################################################################################################################
  
  # 物品状态历史表
  item_status_history <- reactive({
    item_status_history_refresh_trigger()
    dbGetQuery(con, "SELECT * FROM item_status_history")
  })
  
  ####################################################################################################################################
  
  clear_invalid_item_status_history(con)
  
  ####################################################################################################################################
  
  
  
  ############################################
  ######   unique_items_data 表的过滤   ######
  ############################################
  
  # 入库页过滤
  filtered_unique_items_data_inbound <- reactive({
    req(unique_items_data())
    data <- unique_items_data()
    
    # 只显示本页相关状态
    data <- data %>%
      filter(Status %in% c("国内出库", "美国入库"))
    
    data <- filter_unique_items_data_by_inputs(
      data = data,
      input = input,
      maker_input_id = "inbound_filter-maker",
      status_input_id = "inbound_filter-status",
      item_name_input_id = "inbound_filter-name"    
      )
    
    # 统计 SKU, Status, Defect, 和 PurchaseTime 下的数量
    data <- data %>%
      group_by(SKU, Status, Defect) %>%
      mutate(ItemCount = n()) %>%  # 条件统计数量
      ungroup()
    
    # 去重：仅保留每个 SKU 和组合的第一条记录
    data <- data %>%
      arrange(desc(updated_at)) %>%  # 按需求排序
      distinct(SKU, Status, Defect, .keep_all = TRUE)         # 去重，保留所有列
    
    data
  })
  
  # 订单管理页订单过滤
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
      data <- data %>% filter(grepl(input$filter_customer_name, CustomerName, ignore.case = TRUE))
    }
    
    # 根据顾客网名筛选
    if (!is.null(input$filter_customer_netname) && input$filter_customer_netname != "") {
      data <- data %>% filter(grepl(input$filter_customer_netname, CustomerNetName, ignore.case = TRUE))
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
    
    # 按录入时间倒序排列
    data <- data %>% arrange(desc(created_at))
    
    data
  })
  
  filtered_orders_arrived <- reactive({
    req(orders(), unique_items_data())  # 确保订单和物品数据存在
    
    # 获取订单和物品数据
    data_orders <- orders()
    data_items <- unique_items_data()
    
    # 筛选订单状态为“备货”的订单
    data_orders <- data_orders %>%
      filter(OrderStatus == "备货")
    
    # 条件 1：订单内所有物品都有国际运单号
    all_with_tracking_orders <- data_items %>%
      group_by(OrderID) %>%
      summarise(
        all_with_tracking = all(!is.na(IntlTracking) & IntlTracking != "")  # 所有物品都有运单号
      ) %>%
      filter(all_with_tracking) %>%  # 筛选符合条件的订单
      pull(OrderID)
    
    # 条件 2：订单内没有任何物品，备注有调货完成记录
    no_items_with_transfer_note_orders <- data_orders %>%
      filter(
        !(OrderID %in% data_items$OrderID) &  # 订单内没有任何物品
          grepl("【调货完成 \\d{4}-\\d{2}-\\d{2}】", OrderNotes)  # 备注包含调货完成记录
      ) %>%
      pull(OrderID)
    
    # 合并两种符合条件的订单
    valid_order_ids <- union(all_with_tracking_orders, no_items_with_transfer_note_orders)
    
    # 返回筛选后的订单
    filtered_orders <- data_orders %>%
      filter(OrderID %in% valid_order_ids)
    
    return(filtered_orders)
  })
  
  filtered_orders_waiting <- reactive({
    req(orders(), unique_items_data())  # 确保订单和物品数据存在
    
    # 获取订单和物品数据
    data_orders <- orders()
    data_items <- unique_items_data()
    
    # 筛选订单状态为“备货”的订单
    data_orders <- data_orders %>%
      filter(OrderStatus == "备货")
    
    # 条件 1：部分物品有国际运单号，部分没有的订单
    partial_tracking_orders <- data_items %>%
      group_by(OrderID) %>%
      summarise(
        has_tracking = any(!is.na(IntlTracking) & IntlTracking != ""),  # 至少一个物品有运单号
        no_tracking = any(is.na(IntlTracking) | IntlTracking == "")    # 至少一个物品没有运单号
      ) %>%
      filter(has_tracking & no_tracking) %>%  # 同时满足上述两种情况
      pull(OrderID)  # 提取符合条件的 OrderID
    
    # 条件 2：所有物品都没有国际运单号，但备注中有调货操作记录的订单
    no_tracking_with_transfer_note_orders <- data_items %>%
      group_by(OrderID) %>%
      summarise(
        all_no_tracking = all(is.na(IntlTracking) | IntlTracking == "")  # 所有物品都没有运单号
      ) %>%
      filter(all_no_tracking) %>%
      pull(OrderID) %>%
      intersect(  # 交集筛选，订单备注包含指定格式的调货记录
        data_orders %>%
          filter(grepl("【调货完成 \\d{4}-\\d{2}-\\d{2}】", OrderNotes)) %>%  # 正则匹配
          pull(OrderID)
      )
    
    # 合并两种符合条件的订单
    valid_order_ids <- union(partial_tracking_orders, no_tracking_with_transfer_note_orders)
    
    # 返回筛选后的订单
    filtered_orders <- data_orders %>%
      filter(OrderID %in% valid_order_ids)
    
    return(filtered_orders)
  })
  
  filtered_orders_relocation  <- reactive({
    req(orders())  # 确保订单和物品数据存在
    
    # 筛选订单状态为“调货”的订单
    filtered_orders <- orders() %>% filter(OrderStatus == "调货")
    
    return(filtered_orders)
  })
  
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
    data <- data[!is.na(data$Defect) & data$Defect != "未知" & data$Status == "美国入库", ]
    
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
    req(inventory())
    result <- inventory()
    
    # Return empty inventory if no results
    if (nrow(result) == 0) {
      return(create_empty_inventory())
    }
    
    # 按供应商筛选
    if (!is.null(input[["query_filter-maker"]]) && length(input[["query_filter-maker"]]) > 0 && any(input[["query_filter-maker"]] != "")) {
      result <- result %>% filter(Maker %in% input[["query_filter-maker"]])
    }
    
    # 按商品名称筛选
    if (!is.null(input[["query_filter-name"]]) && input[["query_filter-name"]] != "") {
      result <- result %>% filter(ItemName == input[["query_filter-name"]])
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
  
  
  
  ####################################################################################################################################
  
  
  
  # 渲染物品追踪数据表
  unique_items_table_inbound_selected_row <- callModule(uniqueItemsTableServer, "unique_items_table_inbound",
                                                        column_mapping <- c(common_columns, list(
                                                          ItemCount = "数量")
                                                        ), selection = "multiple", data = filtered_unique_items_data_inbound)
  

  # 订单管理分页订单表
  selected_order_row <- callModule(orderTableServer, "orders_table_module",
                                   column_mapping = orders_table_columns,
                                   data = filtered_orders,  # 数据源
                                   selection = "single" # 单选模式
  )
  
  selected_orders_table_arrived_row <- callModule(orderTableServer, "orders_table_arrived",
                                                  column_mapping = orders_table_columns,
                                                  options = modifyList(table_default_options, list(scrollY = "650px")),
                                                  data = filtered_orders_arrived,  # 数据源
                                                  selection = "single" # 单选模式
  )
  
  selected_orders_table_waiting_row <- callModule(orderTableServer, "orders_table_waiting",
                                                  column_mapping = orders_table_columns,
                                                  options = modifyList(table_default_options, list(scrollY = "650px")),
                                                  data = filtered_orders_waiting,  # 数据源
                                                  selection = "single" # 单选模式
  )
  
  selected_orders_table_relocation_row <- callModule(orderTableServer, "orders_relocation",
                                                  column_mapping = orders_table_columns,
                                                  options = modifyList(table_default_options, list(scrollY = "650px")),
                                                  data = filtered_orders_relocation,  # 数据源
                                                  selection = "single" # 单选模式
  )
  
  # 物品管理分页物品表
  unique_items_table_manage_selected_row <- callModule(uniqueItemsTableServer, "unique_items_table_manage",
                                                       column_mapping <- c(common_columns, list(
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
  
  # 瑕疵品管理分页物品表
  unique_items_table_defect_selected_row <- callModule(uniqueItemsTableServer, "unique_items_table_defect",
                                                       column_mapping = c(common_columns, list(
                                                         UsEntryTime = "美入库日",
                                                         Defect = "瑕疵态",
                                                         DefectNotes = "瑕疵备注")
                                                       ), selection = "multiple", data = filtered_unique_items_data_defect,
                                                       option = modifyList(table_default_options, list(scrollY = "730px", searching = TRUE)))
  
  # 国际物流管理分页物品表
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
  
  # 查询分页库存表
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
  
  # 下载分页物品表
  unique_items_table_download_selected_row <- callModule(uniqueItemsTableServer, "unique_items_table_download",
                                                         column_mapping <- c(common_columns, list(
                                                           Defect = "瑕疵态",
                                                           PurchaseTime = "采购日",
                                                           UsEntryTime = "美入库日",                                                           
                                                           UsRelocationTime = "美调货日",
                                                           UsShippingTime = "美发货日")
                                                         ), data = filtered_unique_items_data_download)
  

  
  
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
        
        # 获取用户输入的留言，保持空值为 NULL
        raw_remark <- input$request_remark
        formatted_remark <- if (raw_remark == "" || is.null(raw_remark)) NA_character_ else {
          remark_prefix <- if (system_type == "cn") "[京]" else "[圳]"  # 根据系统类型添加前缀
          paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), ": ", remark_prefix, " ", raw_remark)
        }
        
        dbExecute(con, 
                  "INSERT INTO requests (RequestID, SKU, Maker, ItemImagePath, ItemDescription, Quantity, RequestStatus, Remarks, RequestType) 
         VALUES (?, ?, ?, ?, ?, ?, '待处理', ?, ?)", 
                  params = list(request_id, filtered_data$SKU, filtered_data$Maker, item_image_path, item_description, input$request_quantity, formatted_remark, request_type))
        
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
    
    # 获取用户输入的留言，保持空值为 NULL
    raw_remark <- input$custom_remark
    formatted_remark <- if (raw_remark == "" || is.null(raw_remark)) NA_character_ else {
      remark_prefix <- if (system_type == "cn") "[京]" else "[圳]"  # 根据系统类型添加前缀
      paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), ": ", remark_prefix, " ", raw_remark)
    }
    
    # 将数据插入到数据库
    dbExecute(con, 
              "INSERT INTO requests (RequestID, SKU, Maker, ItemImagePath, ItemDescription, Quantity, RequestStatus, Remarks, RequestType) 
             VALUES (?, ?, '待定', ?, ?, ?, '待处理', ?, '采购')", 
              params = list(request_id, "New-Request", custom_image_path, custom_description, custom_quantity, formatted_remark))
    
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
  ## 入库分页                                                   ##
  ##                                                            ##
  ################################################################

  # 记录当前视图模式，初始为表格模式
  view_mode <- reactiveVal("table_mode")
  
  # 视图模式状态
  observeEvent(input$toggle_view, {
    # 切换视图
    shinyjs::toggle(id = "table_mode")
    shinyjs::toggle(id = "image_mode")
    
    # 根据当前模式更新 view_mode 变量
    if (view_mode() == "table_mode") {
      view_mode("image_mode")
      updateActionButton(session, "toggle_view", label = "切换至：图表模式")
    } else {
      view_mode("table_mode")
      updateActionButton(session, "toggle_view", label = "切换至：大图模式")
    }
  })
  
  # 监听标签页切换事件
  observeEvent(input$inventory_us, {
    if (input$inventory_us == "入库") {
      runjs("document.getElementById('inbound_sku').focus();")
    }
  })
  
  # 物品表过滤模块
  itemFilterServer(
    id = "inbound_filter",
    makers_items_map = makers_items_map
  )
  
  # 监听 SKU 输入
  observeEvent(input$inbound_sku, {
    req(input$inbound_sku)
    
    # 调用 handleSkuInput 并获取待入库数量
    pending_quantity <- handleSkuInput(
      sku_input = input$inbound_sku,
      output_name = "inbound_item_info",
      count_label = "待入库数",
      count_field = "PendingQuantity",
      con = con,
      output = output,
      placeholder_path = placeholder_300px_path,
      host_url = host_url,
      image_mode = TRUE
    )
    
    # 如果启用自动入库功能，直接执行入库逻辑
    if (input$auto_inbound) {
      req(input$inbound_sku)
      item_name <- handleOperation(
        unique_items_data(),
        operation_name = "入库", 
        sku_field = "inbound_sku",
        output_name = "inbound_item_info",
        query_status = "国内出库",
        update_status_value = "美国入库",
        count_label = "待入库数", 
        count_field = "PendingQuantity", 
        refresh_trigger = unique_items_data_refresh_trigger,      
        con,                  
        input, output, session
      )
      
      if (!is.null(item_name) && item_name != "") {
        if (input$speak_item_name) {  # 只有勾选“念出商品名”才朗读
          js_code <- sprintf('
            var msg = new SpeechSynthesisUtterance("%s");
            msg.lang = "zh-CN";
            window.speechSynthesis.speak(msg);
          ', item_name, nchar(item_name))
          
          shinyjs::runjs(js_code)  # 运行 JavaScript 语音朗读
        } else {
          runjs("playInboundSuccessSound()")  # 播放成功音效
        }
      } else {
        runjs("playInboundErrorSound()")  # 播放失败音效
        return()
      }
      
      # 清空 SKU 输入框
      updateTextInput(session, "inbound_sku", value = "")
    } else {
      # 未启用自动入库时更新待入库数量最大值
      if (!is.null(pending_quantity) && pending_quantity > 0) {
        updateNumericInput(session, "inbound_quantity", max = pending_quantity, value = 1)
        showNotification(paste0("已更新待入库数量最大值为 ", pending_quantity, "！"), type = "message")
      } else {
        updateNumericInput(session, "inbound_quantity", max = 0, value = 0)
