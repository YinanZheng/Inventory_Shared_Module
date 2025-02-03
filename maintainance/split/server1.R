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
            AVG(DomesticShippingCost + IntlShippingCost) AS AvgShippingCost
          FROM unique_items
          GROUP BY SKU
        ) u ON i.SKU = u.SKU
        SET 
          i.ProductCost = ROUND(u.AvgProductCost, 2),
          i.ShippingCost = ROUND(u.AvgShippingCost, 2)
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
  
  ####################################################################################################################################
  
  # 物品追踪表
  unique_items_data <- reactive({
    # 当 refresh_trigger 改变时触发更新
    unique_items_data_refresh_trigger()
    
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
      unique_items.UsSoldTime,
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
  })
  
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
    
    data <- filter_unique_items_data_by_inputs(
      data = data,
      input = input,
      maker_input_id = "logistic_filter-maker",
      status_input_id = "logistic_filter-status",
      item_name_input_id = "logistic_filter-name",
      sold_date_range_id = "logistic_filter-sold_date_range",
      exit_date_range_id = "logistic_filter-exit_date_range"
    )
    
    shipping_method <- input$intl_shipping_method
    
    # 判断并根据物流方式筛选
    if (!is.null(shipping_method)) {
      data <- data %>% filter(IntlShippingMethod == shipping_method)
    }
    
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
                                                        ), selection = "multiple", data = filtered_unique_items_data_inbound)
  
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
                                                          option = modifyList(table_default_options, list(scrollY = "730px", searching = TRUE)))
  
  output$filtered_inventory_table_query <- renderDT({  # input$filtered_inventory_table_query_rows_selected
    column_mapping <- list(
      SKU = "条形码",
      ItemName = "商品名",
      ItemImagePath = "商品图",
      Maker = "供应商",
      MajorType = "大类",
      MinorType = "小类",
      Quantity = "总库存数",
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
      showNotification("SKU 已生成（来源于表格选择）", type = "message")
    } else {
      # 如果 SKU 不是来源于表格，检查是否冲突
      existing_sku <- inventory() %>% filter(SKU == sku)
      
      if (nrow(existing_sku) > 0) {
        # 如果 SKU 冲突，弹出模态窗口提醒用户
        showModal(modalDialog(
          title = "SKU 冲突",
          paste0("生成的 SKU '", sku, "' 已存在于库存中，请重新生成 SKU！"),
          easyClose = TRUE,
          footer = modalButton("关闭")
        ))
        
        # 清空 SKU 输入字段
        updateTextInput(session, "new_sku", value = "")
      } else {
        # 如果 SKU 不冲突，更新输入字段
        updateTextInput(session, "new_sku", value = sku)
        showNotification("SKU 生成成功！", type = "message")
      }
    }
  })
  
  autocompleteInputServer("purchase", get_suggestions = item_names)  # 返回商品名列表
  
  # 采购商品图片处理模块
  image_purchase <- imageModuleServer("image_purchase")
  
  # 采购商品添加表（临时）
  added_items <- reactiveVal(create_empty_inventory())
  
  # Render added items table
  output$added_items_table <- renderDT({
    column_mapping <- list(
      SKU = "条形码",
      ItemName = "商品名",
      ItemImagePath = "商品图",
      Maker = "供应商",
      MajorType = "大类",
      MinorType = "小类",
      Quantity = "入库数量",
      ProductCost = "采购单价"
    )
    
    render_table_with_images(
      data = added_items(),
      column_mapping = column_mapping,
      selection = "multiple",
      image_column = "ItemImagePath",
      options = list(fixedHeader = TRUE,  # 启用表头固定
                     dom = 't',  # 隐藏搜索框和分页等控件
                     paging = FALSE,  # 禁止分页
                     searching = FALSE  # 禁止搜索
      )
    )$datatable
  })
  
  # Handle add item button click
  observeEvent(input$add_btn, {
    # 验证输入
    if (is.null(input[["purchase-item_name"]]) || input[["purchase-item_name"]] == "") {
      showNotification("请填写正确商品名称！", type = "error")
      return()
    }
    if (is.null(input$new_quantity) || input$new_quantity <= 0) {
      showNotification("请填写正确商品数量！", type = "error")
      return()
    }
    if (is.null(input$new_product_cost) || input$new_product_cost < 0 || input$new_product_cost > 999) {
      showNotification("请填写正确商品单价！", type = "error")
      return()
    }
    if (is.null(input$new_sku) || input$new_sku == "") {
      showNotification("请确保SKU正常显示！", type = "error")
      return()
    }
    
    # 检查是否存在该 SKU 的库存记录
    inventory_item <- tryCatch({
      dbGetQuery(con, "SELECT ItemImagePath FROM inventory WHERE SKU = ?", params = list(input$new_sku))
    }, error = function(e) {
      showNotification("检查库存时发生错误！", type = "error")
      return(data.frame())
    })
    
    existing_inventory_path <- if (nrow(inventory_item) > 0) inventory_item$ItemImagePath[1] else NULL
    
    # 上传或粘贴图片处理
    new_image_path <- process_image_upload(
      sku = input$new_sku,
      file_data = image_purchase$uploaded_file(),
      pasted_data = image_purchase$pasted_file(),
      inventory_path = existing_inventory_path
    )
    
    # 添加或更新记录
    existing_items <- added_items()
    existing_skus <- existing_items$SKU
    if (input$new_sku %in% existing_skus) {
      sku_index <- which(existing_skus == input$new_sku)
      current_image_path <- existing_items$ItemImagePath[sku_index]
      final_image_path <- if (!is.na(new_image_path) && new_image_path != "") {
        new_image_path
      } else {
        current_image_path
      }
      
      existing_items[sku_index, "SKU"] <- input$new_sku
      existing_items[sku_index, "Maker"] <- input$new_maker
      existing_items[sku_index, "MajorType"] <- input[["type_module-new_major_type"]]
      existing_items[sku_index, "MinorType"] <- input[["type_module-new_minor_type"]]
      existing_items[sku_index, "ItemName"] <- input[["purchase-item_name"]]
      existing_items[sku_index, "Quantity"] <- input$new_quantity
      existing_items[sku_index, "ProductCost"] <- round(input$new_product_cost, 2)
      existing_items[sku_index, "ItemImagePath"] <- as.character(final_image_path)
      
      added_items(existing_items)
      
      showNotification(paste("SKU 已更新:", input$new_sku, "已覆盖旧记录"), type = "message")
    } else {
      # 添加新记录
      new_item <- data.frame(
        SKU = input$new_sku,
        Maker = input$new_maker,
        MajorType = input[["type_module-new_major_type"]],
        MinorType = input[["type_module-new_minor_type"]],
        ItemName = input[["purchase-item_name"]],
        Quantity = input$new_quantity,
        ProductCost = round(input$new_product_cost, 2),
        ItemImagePath = new_image_path,
        stringsAsFactors = FALSE
      )
      added_items(bind_rows(existing_items, new_item))
      showNotification(paste("SKU 已添加:", input$new_sku, "商品名:", input[["purchase-item_name"]]), type = "message")
    }
    
    # 重置
    image_purchase$reset()
  })
  
  # 动态更新按钮文本和图标
  output$add_update_button_ui <- renderUI({
    # 检查SKU是否存在于added_items()
    sku_input <- input$new_sku
    if (is.null(sku_input) || sku_input == "") {
      label <- "添加" # 默认显示“添加”
      icon_type <- "plus" # 默认图标为“添加”图标
    } else {
      sku_exists <- sku_input %in% added_items()$SKU
      if (sku_exists) {
        label <- "更新" # SKU已存在时显示“更新”
        icon_type <- "edit" # 图标显示为“编辑”
      } else {
        label <- "添加" # SKU不存在时显示“添加”
        icon_type <- "plus" # 图标显示为“添加”
      }
    }
    
    # 创建动态按钮
    actionButton("add_btn", label, width = "100%", 
                 icon = icon(icon_type), 
                 style = "background-color: #006400; color: white;")
  })
  
  # Confirm button: Update database and handle images
  observeEvent(input$confirm_btn, {
    tryCatch({
      if (nrow(added_items()) == 0) {
        showNotification("请先录入至少一个商品!", type = "error")
        return()
      }
      
      added_items_df <- added_items()
      
      # Retrieve total package shipping cost from the UI
      total_shipping_cost <- input$new_shipping_cost
      if (is.null(total_shipping_cost) || total_shipping_cost <= 0) {
        total_shipping_cost <- 0  # Default to 0 if invalid
      }
      
      unit_shipping_cost <- total_shipping_cost / sum(added_items_df$Quantity)
      
      for (i in 1:nrow(added_items_df)) {
        add_new_inventory_record(
          con = con,
          sku = added_items_df$SKU[i],
          maker = added_items_df$Maker[i],
          major_type = added_items_df$MajorType[i],
          minor_type = added_items_df$MinorType[i],
          item_name = added_items_df$ItemName[i],
          quantity = 0, # 采购初始库存为 0 
          image_path = added_items_df$ItemImagePath[i]
        )
      }
      
      # 更新数据并触发 UI 刷新
      inventory_refresh_trigger(!inventory_refresh_trigger())
      
      # 同时添加信息到 unique_items 表中
      purchase_date <- format(as.Date(input$purchase_date), "%Y-%m-%d")
      
      batch_data <- do.call(rbind, lapply(1:nrow(added_items_df), function(i) {
        sku <- added_items_df$SKU[i]
        quantity <- added_items_df$Quantity[i]
        product_cost <- added_items_df$ProductCost[i]
        
        # Create rows for each quantity
        t(replicate(quantity, c(
          uuid::UUIDgenerate(),
          as.character(sku),
          as.numeric(product_cost),
          as.numeric(unit_shipping_cost),
          "采购",
          "未知",
          purchase_date
        )))
      }))
      
      # Validate data
      if (is.null(batch_data) || nrow(batch_data) == 0) {
        showNotification("采购数据无效，请检查输入！", type = "error")
        return()
      }
      
      # Convert to data frame
      batch_data <- as.data.frame(batch_data, stringsAsFactors = FALSE)
      
      # Insert into database
      dbBegin(con)
      tryCatch({
        for (i in 1:nrow(batch_data)) {
          dbExecute(con, "INSERT INTO unique_items (UniqueID, SKU, ProductCost, DomesticShippingCost, Status, Defect, PurchaseTime) VALUES (?, ?, ?, ?, ?, ?, ?)",
                    unname(as.vector(batch_data[i, ])))
        }
        dbCommit(con)
        showNotification("所有采购货物已成功登记！", type = "message")
      }, error = function(e) {
        dbRollback(con)
        showNotification(paste("采购登记失败:", e$message), type = "error")
      })
      
      unique_items_data_refresh_trigger(!unique_items_data_refresh_trigger())
      
      # Clear added items and reset input fields
      updateNumericInput(session, "new_quantity", value = 0)  # 恢复数量默认值
      updateNumericInput(session, "new_product_cost", value = 0)  # 恢复单价默认值
      updateNumericInput(session, "new_shipping_cost", value = 0)  # 恢复运费默认值
      updateTextInput(session, "purchase-item_name", value = "")
      image_purchase$reset() # 重置图片
      
      added_items(create_empty_inventory()) #清空添加表
      
    }, error = function(e) {
      showNotification(paste("发生错误:", e$message), type = "error")
    })
  })
  
  # 监听采购页选中items_table
  observeEvent(unique_items_table_purchase_selected_row(), {
    if (!is.null(unique_items_table_purchase_selected_row()) && length(unique_items_table_purchase_selected_row()) > 0) {
      selected_data <- filtered_unique_items_data_purchase()[unique_items_table_purchase_selected_row(), ]
      
      # showNotification(paste("Selected MajorType:", selected_data$MajorType))
      # showNotification(paste("Selected MinorType:", selected_data$MinorType))
      
      # Update input fields in the sidebar
      updateSelectInput(session, "new_maker", selected = selected_data$Maker)
      updateSelectInput(session, "type_module-new_major_type", selected = selected_data$MajorType)
      shinyjs::delay(100, {  # 延迟 100 毫秒
        updateSelectInput(session, "type_module-new_minor_type", selected = selected_data$MinorType)
      })
      updateTextInput(session, "purchase-item_name", value = selected_data$ItemName)
      updateNumericInput(session, "new_quantity", value = 0)
      updateNumericInput(session, "new_product_cost", value = selected_data$ProductCost) 
      updateNumericInput(session, "new_shipping_cost", value = 0)
    }
  })
  
  # 监听采购页选中added_items_table 用来更改添加数据
  observeEvent(input$added_items_table_rows_selected, {
    selected_row <- input$added_items_table_rows_selected
    
    if (length(selected_row) > 0) {
      # 仅处理最后一个选择的行
      last_selected <- tail(selected_row, 1) # 获取最后一个选择的行号
      selected_data <- added_items()[last_selected, ] # 提取最后一个选择的数据
      
      # 更新侧边栏的输入字段
      updateSelectInput(session, "new_maker", selected = selected_data$Maker)
      updateSelectInput(session, "type_module-new_major_type", selected = selected_data$MajorType)
      shinyjs::delay(100, {  # 延迟 100 毫秒
        updateSelectInput(session, "type_module-new_minor_type", selected = selected_data$MinorType)
      })
      updateTextInput(session, "purchase-item_name", value = selected_data$ItemName)
      updateNumericInput(session, "new_quantity", value = selected_data$Quantity)
      updateNumericInput(session, "new_product_cost", value = selected_data$ProductCost)
    }
  })
  
  # 显示总采购开销（含运费）
  output$total_cost <- renderText({
    total <- sum(added_items()$Quantity * added_items()$ProductCost) + input$new_shipping_cost
    paste0("请核实本次采购总金额: ¥", format(total, big.mark = ",", scientific = FALSE),
           "（其中包含运费: ¥", input$new_shipping_cost, ")")
  })
  
  # 监听删除按钮点击事件，弹出确认框
  observeEvent(input$delete_btn, {
    selected_row <- input$added_items_table_rows_selected
    
    # 如果没有选中行，提示用户
    if (length(selected_row) == 0) {
      showNotification("请选择要删除的记录", type = "error")
      return()
    }
    
    # 显示确认框
    showModal(
      modalDialog(
        title = HTML("<strong style='color: red;'>确认删除</strong>"),
        HTML(paste0(
          "<p>您确定要删除选中的 <strong>", length(selected_row), "</strong> 条记录吗？</p>",
