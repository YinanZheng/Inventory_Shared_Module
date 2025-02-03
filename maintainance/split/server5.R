  })
  
  #################################################################
  
  # 开销统计
  expense_summary_data <- reactive({
    req(input$time_range)
    data <- unique_items_data()
    
    start_date <- as.Date(input$time_range[1])
    end_date <- as.Date(input$time_range[2])
    
    time_sequence <- switch(input$precision,
                            "天" = seq.Date(from = start_date, to = end_date, by = "day"),
                            "周" = seq.Date(from = floor_date(start_date, "week"),
                                           to = floor_date(end_date, "week"), by = "week"),
                            "月" = seq.Date(from = floor_date(start_date, "month"),
                                           to = floor_date(end_date, "month"), by = "month"),
                            "年" = seq.Date(from = floor_date(start_date, "year"),
                                           to = floor_date(end_date, "year"), by = "year"))
    
    time_df <- data.frame(GroupDate = time_sequence)
    
    summarized_data <- data %>%
      filter(!is.na(PurchaseTime) & PurchaseTime >= start_date & PurchaseTime <= end_date) %>%
      mutate(
        GroupDate = case_when(
          input$precision == "天" ~ as.Date(PurchaseTime),
          input$precision == "周" ~ floor_date(as.Date(PurchaseTime), "week"),
          input$precision == "月" ~ floor_date(as.Date(PurchaseTime), "month"),
          input$precision == "年" ~ floor_date(as.Date(PurchaseTime), "year")
        )
      ) %>%
      group_by(GroupDate) %>%
      summarise(
        Cost_Domestic = sum(ProductCost + DomesticShippingCost, na.rm = TRUE),
        ProductCost = sum(ProductCost, na.rm = TRUE),
        DomesticShippingCost = sum(DomesticShippingCost, na.rm = TRUE),
        IntlShippingCost = sum(IntlShippingCost, na.rm = TRUE),
        TotalExpense = sum(ProductCost + DomesticShippingCost + IntlShippingCost, na.rm = TRUE),
        AllPurchaseCheck = all(PurchaseCheck == 1, na.rm = TRUE), # 是否全部为1
        .groups = "drop"
      )
    
    complete_data <- time_df %>%
      left_join(summarized_data, by = "GroupDate") %>%
      replace_na(list(
        Cost_Domestic = 0,
        ProductCost = 0,
        DomesticShippingCost = 0,
        IntlShippingCost = 0,
        TotalExpense = 0,
        AllPurchaseCheck = FALSE # 默认设置为 FALSE
      ))
    
    complete_data
  })
  
  # 定义 reactiveVal 用于存储观察器状态
  is_observer_click_suspended <- reactiveVal(TRUE)
  
  # 存储选定的时间范围
  selected_range <- reactiveVal(NULL) # 存储时间范围
  
  # 开销柱状图
  output$expense_chart <- renderPlotly({
    req(expense_summary_data())
    data <- expense_summary_data()
    
    # 获取用户选择的 Y 轴变量及颜色
    y_var <- switch(input$expense_type,
                    "total" = "TotalExpense",
                    "cost" = "ProductCost",
                    "domestic_shipping" = "DomesticShippingCost",
                    "intl_shipping" = "IntlShippingCost",
                    "cost_domestic" = "Cost_Domestic")
    color <- switch(input$expense_type,
                    "total" = "#007BFF",
                    "cost" = "#4CAF50",
                    "domestic_shipping" = "#FF5733",
                    "intl_shipping" = "#FFC107",
                    "cost_domestic" = "#17A2B8")
    
    # 根据精度生成时间范围标签
    data <- data %>%
      mutate(
        GroupLabel = case_when(
          input$precision == "天" ~ format(GroupDate, "%Y-%m-%d"),
          input$precision == "周" ~ paste(
            format(floor_date(GroupDate, "week"), "%Y-%m-%d"),
            "\n至\n",
            format(ceiling_date(GroupDate, "week") - 1, "%Y-%m-%d")
          ),
          input$precision == "月" ~ format(GroupDate, "%Y-%m"),
          input$precision == "年" ~ format(GroupDate, "%Y")
        )
      )
    
    # 创建柱状图
    p <- plot_ly(
      data,
      x = ~GroupLabel,
      y = ~get(y_var),
      type = "bar",
      name = NULL,
      marker = list(color = color),
      text = ~round(get(y_var), 2),
      textposition = "outside",
      source = "expense_chart" # 确保 source 唯一
    ) %>%
      # 注册事件
      event_register("plotly_click") %>%
      event_register("plotly_selected") %>%
      # 显示圆底对勾
      add_trace(
        type = "scatter",
        mode = "markers+text", # 同时使用 markers 和 text 模式
        x = ~GroupLabel,
        y = ~get(y_var) + (max(data[[y_var]], na.rm = TRUE) * 0.15), # 在柱子顶部留出空间
        marker = list(
          size = 20, # 圆点的大小
          color = ~ifelse(AllPurchaseCheck, "#039e2a", "#D3D3D3"), # 根据状态设置深绿色或浅灰色
          line = list(width = 0) # 移除外边框
        ),
        text = ~ifelse(AllPurchaseCheck, "\u2714", "\u2714"), # 使用 Unicode 的白色勾
        textfont = list(
          size = 14, # 增大字体，增强可见度
          color = "white", # 勾的颜色为白色
          weight = "bold" # 加粗字体
        ),
        textposition = "middle center", # 勾的位置在圆点正中央
        showlegend = FALSE # 不显示图例
      ) %>%
      # 添加布局和其他设置
      layout(
        xaxis = list(
          title = "",
          tickvals = data$GroupLabel,
          tickangle = -45,
          tickfont = list(size = 12),
          showgrid = FALSE
        ),
        yaxis = list(
          title = "采购开销（元）",
          tickfont = list(size = 12),
          range = c(0, max(data[[y_var]], na.rm = TRUE) * 1.2) # 给顶部留空间
        ),
        margin = list(l = 50, r = 20, t = 20, b = 50),
        showlegend = FALSE,
        plot_bgcolor = "#F9F9F9",
        paper_bgcolor = "#FFFFFF"
      )
    
    # 激活观察器
    if (is_observer_click_suspended()) {
      observer_click$resume()
      is_observer_click_suspended(FALSE)
    }

    p
  })
  
  # 定义点击观察器，初始状态为 suspended = TRUE
  observer_click <- observeEvent(event_data("plotly_click", source = "expense_chart"), suspended = TRUE, {
    clicked_point <- event_data("plotly_click", source = "expense_chart")
    if (!is.null(clicked_point)) {
      precision <- input$precision # 当前精度（天、周、月、年）

      # 根据精度解析点击的时间点
      clicked_date <- switch(
        precision,
        "年" = as.Date(paste0(clicked_point$x, "-01-01")), # 对"年"进行特殊处理
        as.Date(clicked_point$x) # 其他情况直接转为日期
      )
      
      # 根据精度计算时间范围
      range <- switch(precision,
                      "天" = c(clicked_date, clicked_date),
                      "周" = c(floor_date(clicked_date, "week"), ceiling_date(clicked_date, "week") - 1),
                      "月" = c(floor_date(clicked_date, "month"), ceiling_date(clicked_date, "month") - 1),
                      "年" = c(floor_date(clicked_date, "year"), ceiling_date(clicked_date, "year") - 1)
      )
      
      # 调用 updateDateRangeInput 更新用户界面的时间范围选择
      updateDateRangeInput(
        session,
        inputId = "time_range",
        start = range[1],
        end = range[2]
      )
      
      selected_range(range)
    }
  })

  # 筛选物品详情数据
  filtered_items <- reactive({
    req(selected_range()) # 确保时间范围存在
    range <- selected_range()
    
    # 从物品数据中筛选出时间范围内的数据
    unique_items_data() %>%
      filter(PurchaseTime >= range[1] & PurchaseTime <= range[2]) %>%
      arrange(PurchaseTime) # 按采购时间升序排列
  })
  
  # 渲染筛选
  callModule(uniqueItemsTableServer, "expense_details_table",
             column_mapping = c(common_columns, list(
               DomesticShippingCost = "国内运费",
               IntlShippingCost = "国际运费",
               PurchaseTime = "采购时间",
               PurchaseCheck = "核对"
             )),
             data = filtered_items
  )
  
  # 总开销分布饼图
  output$pie_chart <- renderPlotly({
    data <- expense_summary_data()
    
    # 饼图数据：计算总开销分布
    total_product_cost <- sum(data$ProductCost, na.rm = TRUE)
    total_domestic_shipping_cost <- sum(data$DomesticShippingCost, na.rm = TRUE)
    total_intl_shipping_cost <- sum(data$IntlShippingCost, na.rm = TRUE)
    
    pie_data <- data.frame(
      Category = c("商品成本", "国内运费", "国际运费"),
      Value = c(total_product_cost, total_domestic_shipping_cost, total_intl_shipping_cost)
    )
    
    # 获取时间范围
    time_range <- paste(as.Date(input$time_range[1]), "至", as.Date(input$time_range[2]))
    
    # 绘制饼图
    plot_ly(
      pie_data,
      labels = ~Category,
      values = ~Value,
      type = "pie",
      textinfo = "label+value",  # 显示标签和数值
      hoverinfo = "label+percent",  # 悬停显示类别和百分比
      insidetextorientation = "radial",
      marker = list(colors = c("#4CAF50", "#FF5733", "#FFC107"))
    ) %>%
      layout(
        annotations = list(
          x = 0.5, y = -0.2,  # 调整注释的位置
          text = paste("统计时间范围：", time_range),
          showarrow = FALSE,
          font = list(size = 12, color = "#666")
        ),
        showlegend = FALSE,  # 隐藏图例
        paper_bgcolor = "#F9F9F9",  # 背景颜色
        margin = list(l = 50, r = 30, t = 80, b = 50)  # 增加左右和底部边距
      )
  })
  
  # 重置时间范围
  observeEvent(input$reset_time_range, {
    # 重置时间范围到默认值（最近30天）
    default_start <- Sys.Date() - 30
    default_end <- Sys.Date()
    
    updateDateRangeInput(
      session,
      inputId = "time_range",
      start = default_start,
      end = default_end
    )
  })
  
  # 开销核对动态UI
  output$confirm_expense_check_ui <- renderUI({
    req(selected_range()) # 确保有选定的时间范围
    
    range <- selected_range() # 获取当前选定的时间范围
    
    # 判断范围是否相等
    label_text <- if (range[1] == range[2]) {
      paste0("确认开销核对（采购时间：", range[1], "）")
    } else {
      paste0("确认开销核对（采购时间：", range[1], " 至 ", range[2], "）")
    }
    
    actionButton(
      inputId = "confirm_expense_check_btn", 
      label = label_text,
      icon = icon("check-circle"), 
      class = "btn-success",
      style = "width: 100%; margin-top: 5px;"
    )
  })
  
  # 确认开销核对
  observeEvent(input$confirm_expense_check_btn, {
    req(filtered_items()) # 确保筛选出的物品数据存在
    
    # 获取筛选出的物品
    items_to_update <- filtered_items()
    
    if (nrow(items_to_update) == 0) {
      showNotification("当前筛选无物品可核对，请选择有效的柱子！", type = "error")
      return(NULL)
    }
    
    # 按采购时间分组统计
    grouped_expenses <- items_to_update %>%
      group_by(PurchaseTime) %>%
      summarise(
        TotalCost = sum(ProductCost, na.rm = TRUE),
        TotalDomesticShipping = sum(DomesticShippingCost, na.rm = TRUE)
      )
    
    # 更新数据库中的 PurchaseCheck 为 1
    tryCatch({
      dbExecute(
        con,
        "UPDATE unique_items SET PurchaseCheck = 1 WHERE UniqueID IN (?)",
        params = list(items_to_update$UniqueID)
      )
      
      unique_items_data_refresh_trigger(!unique_items_data_refresh_trigger())
      
      showNotification(paste("成功更新", nrow(items_to_update), "条物品的开销核对状态！"), type = "message")
      
      # 将物品成本和国内运费分别登记到"一般户卡"
      grouped_expenses %>%
        rowwise() %>%
        mutate(
          # 生成物品成本的交易记录
          CostTransaction = if (TotalCost > 0) {
            remarks_cost <- paste("[采购成本已核对]", "采购日期:", PurchaseTime)
            transaction_id <- generate_transaction_id("买货卡", TotalCost, remarks_cost, PurchaseTime)
            dbExecute(
              con,
              "INSERT INTO transactions (TransactionID, AccountType, Amount, Remarks, TransactionTime) VALUES (?, ?, ?, ?, ?)",
              params = list(
                transaction_id,
                "买货卡",
                -TotalCost,
                remarks_cost,
                PurchaseTime
              )
            )
            list(transaction_id) # 返回记录的 ID
          } else {
            list(NULL) # 如果总成本为 0，返回 NULL
          },
          
          # 生成国内运费的交易记录
          ShippingTransaction = if (TotalDomesticShipping > 0) {
            remarks_ship <- paste("[国内运费已核对]", "采购日期:", PurchaseTime)
            transaction_id <- generate_transaction_id("买货卡", TotalDomesticShipping, remarks_ship, PurchaseTime)
            dbExecute(
              con,
              "INSERT INTO transactions (TransactionID, AccountType, Amount, Remarks, TransactionTime) VALUES (?, ?, ?, ?, ?)",
              params = list(
                transaction_id,
                "买货卡",
                -TotalDomesticShipping,
                remarks_ship,
                PurchaseTime
              )
            )
            list(transaction_id) # 返回记录的 ID
          } else {
            list(NULL) # 如果国内运费为 0，返回 NULL
          }
        )
      
      showNotification("核对后的采购开销与国内运费已登记到'买货卡（139）'！", type = "message")
      
      # 重新计算所有balance记录
      update_balance("买货卡", con)

    }, error = function(e) {
      showNotification(paste0("更新失败!", e), type = "error")
    })
  })
  
  
  #################################################################
  
  # 库存总览数据统计
  overview_data <- reactive({
    data <- unique_items_data()
    domestic <- data %>% filter(Status == "国内入库")
    logistics <- data %>% filter(Status == "国内出库")
    us <- data %>% filter(Status == "美国入库")
    sold <- data %>% filter(Status %in% c("国内售出", "美国调货", "美国发货"))
    
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
        us_shipping_count = nrow(sold %>% filter(Status == "美国发货")),
        value = sum(sold$ProductCost, na.rm = TRUE),
        shipping = sum(sold$IntlShippingCost + sold$DomesticShippingCost, na.rm = TRUE)
      )
    )
  })
  
  # 输出卡片数据
  output$domestic_total_count <- renderText({ overview_data()$domestic$count })
  output$domestic_total_value <- renderText({ sprintf("¥%.2f", overview_data()$domestic$value) })
  output$domestic_shipping_cost <- renderText({ sprintf("¥%.2f", overview_data()$domestic$shipping) })
  
  output$logistics_total_count <- renderText({ overview_data()$logistics$count })
  output$logistics_total_value <- renderText({ sprintf("¥%.2f", overview_data()$logistics$value) })
  output$logistics_shipping_cost <- renderText({ sprintf("¥%.2f", overview_data()$logistics$shipping) })
  
  output$us_total_count <- renderText({ overview_data()$us$count })
  output$us_total_value <- renderText({ sprintf("¥%.2f", overview_data()$us$value) })
  output$us_shipping_cost <- renderText({ sprintf("¥%.2f", overview_data()$us$shipping) })
  
  output$sold_total_count <- renderText({ overview_data()$sold$count })
  output$sold_total_count_with_shipping <- renderText({
    count <- overview_data()$sold$count
    us_shipping_count <- overview_data()$sold$us_shipping_count
    paste0(count, " (", us_shipping_count, ")")
  })
  output$sold_total_value <- renderText({ sprintf("¥%.2f", overview_data()$sold$value) })
  output$sold_shipping_cost <- renderText({ sprintf("¥%.2f", overview_data()$sold$shipping) })
  
  # 状态流转桑基图
  output$status_sankey <- renderSankeyNetwork({
    # 获取物品状态历史数据
    history_data <- item_status_history()
    
    # 确保状态流转顺序正确
    links <- history_data %>%
      group_by(UniqueID) %>%
      arrange(previous_status_timestamp, .by_group = TRUE) %>%
      mutate(next_status = lead(previous_status)) %>%  # 获取下一个状态
      filter(!is.na(next_status)) %>%  # 过滤掉没有后续状态的记录
      ungroup() %>%
      group_by(source = previous_status, target = next_status) %>%
      summarise(value = n(), .groups = "drop")  # 汇总每对状态的流转次数
    
    links <- as.data.frame(links)
    
    # 定义节点
    nodes <- data.frame(name = unique(c(links$source, links$target)))
    
    # 映射 source 和 target 到节点索引
    links <- links %>%
      mutate(source = match(source, nodes$name) - 1,
             target = match(target, nodes$name) - 1)
    
    # 校验 links 和 nodes 是否有效
    if (nrow(links) == 0 || nrow(nodes) == 0) {
      showNotification("没有可用的状态流转数据，请检查数据源。", type = "error")
      return(NULL)
    }
    
    # 渲染桑基图
    sankeyNetwork(
      Links = links,
      Nodes = nodes,
      Source = "source",
      Target = "target",
      Value = "value",
      NodeID = "name",
      fontSize = 12,
      nodeWidth = 30
    )
  })
  
  
  #################################################################
  
  # 清空sku输入框
  observeEvent(input$clear_query_sku_btn, {
    updateTextInput(session, "query_sku", value = "")
  })
  
  # 监听查询页选中inventory table (for SKU query and chart summary)
  observeEvent(input$filtered_inventory_table_query_rows_selected, {
    selected_row <- input$filtered_inventory_table_query_rows_selected
    if (length(selected_row) > 0) {
      selected_data <- filtered_inventory()[selected_row, ]
      # 更新 SKU 输入框(生成库存图表用)
      updateTextInput(session, "query_sku", value = selected_data$SKU)
    }
  })
  
  
  
  ################################################################
  ##                                                            ##
  ## 数据下载分页                                               ##
  ##                                                            ##
  ################################################################
  
  # 动态生成供应商筛选器
  output$download_maker_ui <- renderUI({
    makers <- unique_items_data() %>% pull(Maker) %>% unique()
    
    createSearchableDropdown(
      input_id = "download_maker",
      label = "选择供应商:",
      data = makers,
      placeholder = "搜索供应商..."
    )
  })
  
  # 监听供应商选择变化并动态更新商品名称
  observe({
    req(unique_items_data())  # 确保数据存在
    
    # 获取用户选择的供应商
    selected_makers <- input$download_maker
    
    # 筛选商品名称
    if (!is.null(selected_makers) && length(selected_makers) > 0) {
      filtered_data <- unique_items_data() %>% filter(Maker %in% selected_makers)
    } else {
      filtered_data <- unique_items_data()
    }
    
    # 提取对应的商品名称，并在前面加一个空选项
    item_names <- c("", filtered_data %>% pull(ItemName) %>% unique())
    
    # 更新商品名称选项，默认选中空选项
    updateSelectizeInput(session, "download_item_name", choices = item_names, selected = "")
  })
  
  # 重置筛选逻辑
  observeEvent(input$download_reset_filters, {
    # 重置供应商筛选为全选
    makers <- unique_items_data() %>% pull(Maker) %>% unique()
    updateDropdown.shinyInput(
      session = session,
      inputId = "download_maker",
      options = lapply(makers, function(maker) list(key = maker, text = maker)), # 更新选项
      value = NULL # 重置为未选中状态
    )
    
    # 重置商品名称筛选为空选项
    updateSelectizeInput(session, "download_item_name", choices = "", selected = "")
    updateDateRangeInput(session, "download_date_range", start = Sys.Date() - 365, end = Sys.Date())
  })
  
  # 下载物品汇总表为 Excel
  output$download_details_xlsx <- downloadHandler(
    filename = function() {
      paste("物品汇总表（按采购日期）-", format(Sys.time(), "%Y%m%d-%H%M%S", tz = "Asia/Shanghai"), ".xlsx", sep = "")
    },
    content = function(file) {
      # 创建 Excel 文件
      wb <- createWorkbook()
      addWorksheet(wb, "物品汇总表")
      
      # 获取数据
      data <- filtered_unique_items_data_download()
      req(!is.null(data) && nrow(data) > 0)  # 确保数据非空
      
      data <- map_column_names(data, column_mapping = list(
        SKU = "条形码",
        ItemName = "商品名",
        ItemImagePath = "商品图",
        Maker = "供应商",
        MajorType = "大类",
        MinorType = "小类",
        ProductCost = "单价",
        DomesticShippingCost = "平摊运费",
        PurchaseTime = "采购日",
        Status = "库存态",
        Defect = "瑕疵态"
      ))
      
      # 按 SKU 计算全局库存统计
      sku_inventory_stats <- data %>%
        group_by(`条形码`) %>%
        summarize(
          总剩余库存数 = sum(`库存态` %in% c("国内入库", "国内出库", "美国入库")),
          国内库存数 = sum(`库存态` == "国内入库"),
          在途库存数 = sum(`库存态` == "国内出库"),
          美国库存数 = sum(`库存态` == "美国入库"),
          无瑕 = sum(`瑕疵态` == "无瑕"),
          瑕疵 = sum(`瑕疵态` == "瑕疵"),
          修复 = sum(`瑕疵态` == "修复"),
          .groups = "drop"
        )
      
      # 按条形码和采购日期分组，统计其他信息
      grouped_data <- data %>%
        group_by(`条形码`, `采购日`) %>%
        summarize(
          商品名 = first(`商品名`),
          商品图 = first(`商品图`),
          供应商 = first(`供应商`),
          大类 = first(`大类`),
          小类 = first(`小类`),
          批次单价 = mean(`单价`, na.rm = TRUE),
          批次平摊运费 = mean(`平摊运费`, na.rm = TRUE),
          批次采购数 = n(),  # 记录数
          .groups = "drop"
        )
      
      # 合并全局统计到分组数据
      final_data <- grouped_data %>%
        left_join(sku_inventory_stats, by = "条形码")
      
      n_col <- ncol(final_data)
      
      # 写入数据到 Excel
      writeData(wb, "物品汇总表", final_data, startCol = 1, startRow = 1)
      
      # 图片插入的列号
      col_to_insert <- which(colnames(final_data) == "商品图")
      
      # 设置固定高度 1 inch，计算动态宽度
      image_height <- 1
      
      # 插入图片到 Excel
      for (i in seq_len(nrow(final_data))) {
        image_path <- as.character(final_data[i, col_to_insert])
        
        image_width_max <- 1
        
        if (!is.na(image_path) && file.exists(image_path)) {
          
          # 获取图片的实际宽高比
          dims <- get_image_dimensions(image_path)
          width_ratio <- dims$width / dims$height  # 宽高比
          
          row_to_insert <- i + 1  # 对应数据的行号
          
          image_width <- image_height * width_ratio  # 动态宽度（英寸）
          
          # 更新最大宽度
          image_width_max <- max(image_width_max, image_width)
          
          insertImage(
            wb = wb,
            sheet = "物品汇总表",
            file = normalizePath(image_path),
            startRow = row_to_insert,
            startCol = col_to_insert,
            width = image_width,
            height = image_height,
            units = "in"
          )
          
          # 清空路径数据
          writeData(wb, "物品汇总表", "", startCol = col_to_insert, startRow = i + 1)
          
          # 调整行高和列宽
          setRowHeights(wb, "物品汇总表", rows = row_to_insert, heights = image_height * 78)
          
        } else {
          showNotification(paste("跳过不存在的图片:", image_path), type = "warning", duration = 5)
        }
      }
      
      # 最终设置列宽，保证所有图片适配最大宽度
      setColWidths(wb, "物品汇总表", cols = col_to_insert, widths = image_width_max * 16)
      
      # 自动调整其他列的宽度
      setColWidths(wb, "物品汇总表", cols = seq_len(n_col)[-col_to_insert], widths = "auto")
      
      # 保存 Excel 文件
      saveWorkbook(wb, file, overwrite = TRUE)
      showNotification("Excel 文件已成功下载", type = "message", duration = 5)
    }
  )
  
  # 下载物品明细表为 Excel
  output$download_summary_xlsx <- downloadHandler(
    filename = function() {
      paste("物品明细表-", format(Sys.time(), "%Y%m%d-%H%M%S", tz = "Asia/Shanghai"), ".xlsx", sep = "")
    },
    content = function(file) {
      # 创建 Excel 文件
      wb <- createWorkbook()
      addWorksheet(wb, "物品明细表")
      
      # 获取数据
      final_data <- filtered_unique_items_data_download()
      
      n_col <- ncol(final_data)
      
      # 写入数据到 Excel
      writeData(wb, "物品明细表", final_data, startCol = 1, startRow = 1)
      
      # 图片插入的列号
      col_to_insert <- which(colnames(final_data) == "ItemImagePath")
      
      # 设置固定高度 1 inch，计算动态宽度
      image_height <- 1
      
      # 插入图片到 Excel
      for (i in seq_len(nrow(final_data))) {
        image_path <- as.character(final_data[i, col_to_insert])
        
        image_width_max <- 1
        
        if (!is.na(image_path) && file.exists(image_path)) {
          
          # 获取图片的实际宽高比
          dims <- get_image_dimensions(image_path)
          width_ratio <- dims$width / dims$height  # 宽高比
          
          row_to_insert <- i + 1  # 对应数据的行号
          
          image_width <- image_height * width_ratio  # 动态宽度（英寸）
          
          # 更新最大宽度
          image_width_max <- max(image_width_max, image_width)
          
          insertImage(
            wb = wb,
            sheet = "物品明细表",
            file = normalizePath(image_path),
            startRow = row_to_insert,
            startCol = col_to_insert,
            width = image_width,
            height = image_height,
            units = "in"
          )
          
          # 清空路径数据
          writeData(wb, "物品明细表", "", startCol = col_to_insert, startRow = i + 1)
          
          # 调整行高和列宽
          setRowHeights(wb, "物品明细表", rows = row_to_insert, heights = image_height * 78)
          
        } else {
          showNotification(paste("跳过不存在的图片:", image_path), type = "warning", duration = 5)
        }
      }
      
      # 最终设置列宽，保证所有图片适配最大宽度
      setColWidths(wb, "物品明细表", cols = col_to_insert, widths = image_width_max * 16)
      
      # 自动调整其他列的宽度
      setColWidths(wb, "物品明细表", cols = seq_len(n_col)[-col_to_insert], widths = "auto")
      
      # 保存 Excel 文件
      saveWorkbook(wb, file, overwrite = TRUE)
      showNotification("Excel 文件已成功下载", type = "message", duration = 5)
    }
  )

  
  
  ################################################################
  ##                                                            ##
  ## 管理员                                                     ##
  ##                                                            ##
  ################################################################
  
  # 管理员登录状态
  admin_logged_in <- reactiveVal(FALSE)
  
  # 监听登录按钮
  observeEvent(input$admin_login_btn, {
    if (input$admin_password == admin_password) {
      admin_logged_in(TRUE)
      showNotification("登录成功！", type = "message")
    } else {
      showNotification("密码错误，请重试！", type = "error")
      admin_logged_in(FALSE)
    }
  })
  
  # 渲染管理员控制
  output$admin_controls <- renderUI({
    if (admin_logged_in()) {
      tagList(
        
        tags$h4("修改库存状态", style = "font-weight: bold; color: #28A745;"),
        
        # 目标状态选择
        selectInput("admin_target_status", "目标库存状态改为：", 
                    choices = c('采购','国内入库','国内出库','国内售出','美国入库','美国售出','美国发货','美国调货','退货'), 
                    selected = NULL, width = "100%"),
        
        # 是否记录修改时间
        checkboxInput("admin_record_timestamp", "记录修改时间", value = FALSE),
        
        # 更新选中物品状态
        actionButton("admin_update_status_btn", "更新库存状态", class = "btn-success", style = "width: 100%; margin-top: 10px;"),
        
        tags$hr(),
        
        tags$h4("修改瑕疵品状态", style = "font-weight: bold; color: #007BFF;"),
        
        # 目标状态选择
        selectInput("admin_target_defect", "目标瑕疵状态改为：", 
                    choices = c('未知','无瑕','瑕疵','修复'), 
                    selected = NULL, width = "100%"),
        
        # 更新选中物品瑕疵品状态
        actionButton("admin_update_defect_btn", "更新瑕疵品状态", class = "btn-info", style = "width: 100%; margin-top: 10px;"),
        
        tags$hr(),
        
        tags$h4("修改库存总数", style = "font-weight: bold; color: #FF5733;"),
        
        # 输入新的库存总数
        numericInput("admin_new_total_quantity", "新库存总数：", value = 0, min = 0, width = "100%"),
        
        # 提交修改库存总数的按钮
        actionButton("admin_update_inventory_btn", "修改库存总数", class = "btn-warning", style = "width: 100%; margin-top: 10px;")
      )
    } else {
      div(tags$p("请输入密码以访问管理员功能", style = "color: red; font-weight: bold; text-align: center;"))
    }
  })
  
  # 使用 uniqueItemsTableServer 渲染表格
  unique_items_table_admin_selected_row <- callModule(uniqueItemsTableServer, "admin_items_table", 
                                                      column_mapping = c(common_columns, list(
                                                        Defect = "瑕疵态",
                                                        PurchaseTime = "采购日",
                                                        DomesticEntryTime = "入库日",
                                                        DomesticExitTime = "出库日",
                                                        DomesticSoldTime = "出售日",
                                                        IntlShippingMethod = "国际运输",
                                                        OrderID = "订单号"
                                                      )), 
                                                      selection = "multiple", 
                                                      data = unique_items_data)
  
  # 更新库存状态按钮
  observeEvent(input$admin_update_status_btn, {
    req(input$admin_target_status, unique_items_table_admin_selected_row())
    
    # 获取选中行的索引
    selected_rows <- unique_items_table_admin_selected_row()
    selected_items <- unique_items_data()[selected_rows, ]
    
    # 确保有选中物品
    if (nrow(selected_items) == 0) {
      showNotification("请选择至少一个物品进行状态更新！", type = "error")
      return()
    }
    
    tryCatch({
      # 获取是否记录修改时间的选项
      record_timestamp <- input$admin_record_timestamp
      
      # 遍历选中物品
      lapply(1:nrow(selected_items), function(i) {
        unique_id <- selected_items$UniqueID[i]
        new_status <- input$admin_target_status
        
        # 调用 update_status 更新物品状态
        update_status(
          con = con,
          unique_id = unique_id,
          new_status = new_status,
          refresh_trigger = unique_items_data_refresh_trigger,
          update_timestamp = record_timestamp  # 使用用户选择的值
        )
      })
      
      # 更新数据并触发 UI 刷新
      unique_items_data_refresh_trigger(!unique_items_data_refresh_trigger())
      
      showNotification("库存状态更新成功！", type = "message")
    }, error = function(e) {
      # 捕获错误并通知用户
      showNotification(paste("库存状态更新失败：", e$message), type = "error")
    })
  })
  
  observeEvent(input$admin_update_defect_btn, {
    req(input$admin_target_defect, unique_items_table_admin_selected_row())
    
    # 获取选中行的索引
    selected_rows <- unique_items_table_admin_selected_row()
    selected_items <- unique_items_data()[selected_rows, ]
    
    # 确保有选中物品
    if (nrow(selected_items) == 0) {
      showNotification("请选择至少一个物品进行瑕疵品状态更新！", type = "error")
      return()
    }
    
    tryCatch({
      # 遍历选中物品
      lapply(1:nrow(selected_items), function(i) {
        unique_id <- selected_items$UniqueID[i]
        target_defect <- input$admin_target_defect  # 获取目标瑕疵品状态
        
        # 调用 update_status 更新瑕疵品状态
        update_status(
          con = con,
          unique_id = unique_id,
          new_status = NULL,  # 不更新物品状态
          defect_status = target_defect,  # 更新瑕疵品状态
          refresh_trigger = unique_items_data_refresh_trigger
        )
      })
      
      # 更新数据并触发 UI 刷新
      unique_items_data_refresh_trigger(!unique_items_data_refresh_trigger())
      
      showNotification("瑕疵品状态更新成功！", type = "message")
    }, error = function(e) {
      # 捕获错误并通知用户
      showNotification(paste("瑕疵品状态更新失败：", e$message), type = "error")
    })
  })
  
  
  # 更新总库存数按钮
  observeEvent(input$admin_update_inventory_btn, {
    # 获取点选的行数据
    selected_rows <- unique_items_table_admin_selected_row()
    selected_items <- unique_items_data()[selected_rows, ]
    
    # 校验是否有选中物品
    if (is.null(selected_rows) || nrow(selected_items) == 0) {
      showNotification("请先选择至少一件物品！", type = "error")
      return()
    }
    
    # 获取选中物品的 SKU 列表
    sku_counts <- selected_items %>%
      group_by(SKU) %>%
      summarize(SelectedCount = n(), .groups = "drop")  # 按 SKU 聚合
    
    # 获取新库存总数
    new_total_quantity <- input$admin_new_total_quantity
    
    # 校验库存输入
    if (is.null(new_total_quantity) || new_total_quantity < 0) {
      showNotification("库存总数必须为非负数！", type = "error")
      return()
    }
    
    tryCatch({
      # 遍历每个 SKU，更新库存总数
      lapply(1:nrow(sku_counts), function(i) {
        sku <- sku_counts$SKU[i]
        selected_count <- sku_counts$SelectedCount[i]
        
        # 检查 SKU 是否存在
        existing_record <- dbGetQuery(con, "SELECT SKU, Quantity FROM inventory WHERE SKU = ?", params = list(sku))
        if (nrow(existing_record) == 0) {
          showNotification(paste0("SKU ", sku, " 不存在！"), type = "error")
          return(NULL)
        }
        
        # 更新库存总数为新值
        dbExecute(con, "
        UPDATE inventory
        SET Quantity = ?
        WHERE SKU = ?",
                  params = list(new_total_quantity, sku)
        )
        
        showNotification(
          paste0("SKU ", sku, " 的库存总数已更新为 ", new_total_quantity, "！"),
          type = "message"
        )
      })
      
      # 更新数据并触发 UI 刷新
      unique_items_data_refresh_trigger(!unique_items_data_refresh_trigger())
      
    }, error = function(e) {
      # 捕获错误并通知用户
      showNotification(paste("修改库存总数时发生错误：", e$message), type = "error")
    })
  })
  
  
  #########################################################################################################################
  
  # Disconnect from the database on app stop
  onStop(function() {
    dbDisconnect(con)
  })
}
