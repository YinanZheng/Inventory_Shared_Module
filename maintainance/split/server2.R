          "<p><strong>注意：</strong> 此操作无法撤销！</p>"
        )),
        footer = tagList(
          modalButton("取消"),  # 关闭弹窗按钮
          actionButton("confirm_delete_selected", "确认删除", class = "btn-danger")
        ),
        easyClose = FALSE
      )
    )
  })
  
  # 确认删除逻辑
  observeEvent(input$confirm_delete_selected, {
    removeModal()  # 关闭确认弹窗
    
    selected_row <- input$added_items_table_rows_selected
    
    tryCatch({
      if (length(selected_row) > 0) {
        # 执行删除逻辑
        current_items <- added_items()
        updated_items <- current_items[-selected_row, ]  # 删除选中行
        added_items(updated_items)  # 更新 reactive 值
        
        # 通知用户
        showNotification("选中的记录已成功删除", type = "message")
      } else {
        showNotification("请选择要删除的记录", type = "error")
      }
    }, error = function(e) {
      # 捕获错误并通知用户
      showNotification(paste("删除失败：", e$message), type = "error")
    })
  })
  
  
  # 清空输入
  observeEvent(input$reset_btn, {
    tryCatch({
      # 清空输入控件
      update_maker_choices(session, "new_maker", maker_list())
      updateTextInput(session, "purchase-item_name", value = "")
      updateNumericInput(session, "new_quantity", value = 0)  # 恢复数量默认值
      updateNumericInput(session, "new_product_cost", value = 0)  # 恢复单价默认值
      updateNumericInput(session, "new_shipping_cost", value = 0)  # 恢复运费默认值
      updateTextInput(session, "new_sku", value = "")  # 清空 SKU
      
      # 重置图片控件
      image_purchase$reset()
      
      # 通知用户
      showNotification("输入已清空！", type = "message")
    }, error = function(e) {
      # 捕获错误并通知用户
      showNotification("清空输入时发生错误，请重试！", type = "error")
    })
  })
  
  
  
  ################################################################
  ##                                                            ##
  ## 入库分页                                                   ##
  ##                                                            ##
  ################################################################
  
  # 监听标签页切换事件
  observeEvent(input$inventory_china, {
    if (input$inventory_china == "入库") {
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
    # 调用 handleSkuInput 并获取待入库数量
    pending_quantity <- handleSkuInput(
      sku_input = input$inbound_sku,
      output_name = "inbound_item_info",
      count_label = "待入库数",
      count_field = "PendingQuantity",
      con = con,
      output = output,
      placeholder_path = placeholder_300px_path,
      host_url = host_url
    )
    
    # 如果启用自动入库功能，直接执行入库逻辑
    if (input$auto_inbound && !is.null(pending_quantity) && pending_quantity > 0) {
      unique_ID <- handleOperation(
        unique_items_data(),
        operation_name = "入库", 
        sku_field = "inbound_sku",
        output_name = "inbound_item_info",
        query_status = "采购",
        update_status_value = "国内入库",
        count_label = "待入库数", 
        count_field = "PendingQuantity", 
        refresh_trigger = unique_items_data_refresh_trigger,    
        con,                  
        input, output, session
      )
      
      # 检查是否成功处理
      if (!is.null(unique_ID) && unique_ID != "") {
        # 更新库存数据
        adjust_inventory_quantity(con, input$inbound_sku, adjustment = 1) #采购入库后 库存+1
        
        # 显示成功通知
        showNotification(paste0("SKU ", input$inbound_sku, " 的一个物品已自动入库！"), type = "message")
      } else {
        showNotification("自动入库失败，可能物品已全部入库或数据异常！", type = "error")
      }
      
      # 清空 SKU 输入框
      updateTextInput(session, "inbound_sku", value = "")
      runjs("document.getElementById('inbound_sku').focus();")
    } else {
      # 未启用自动入库时更新待入库数量最大值
      if (!is.null(pending_quantity) && pending_quantity > 0) {
        updateNumericInput(session, "inbound_quantity", max = pending_quantity, value = 1)
        showNotification(paste0("已更新待入库数量最大值为 ", pending_quantity, "！"), type = "message")
      } else {
        updateNumericInput(session, "inbound_quantity", max = 0, value = 0)
      }
    }
  })
  
  # 手动确认入库逻辑
  observeEvent(input$confirm_inbound_btn, {
    # 从输入中获取入库数量，确保为正整数
    inbound_quantity <- as.integer(input$inbound_quantity)
    if (is.na(inbound_quantity) || inbound_quantity <= 0) {
      showNotification("入库数量必须是一个正整数！", type = "error")
      return()
    }
    
    # 批量处理入库逻辑
    for (i in seq_len(inbound_quantity)) {
      unique_ID <- handleOperation(
        unique_items_data(),
        operation_name = "入库", 
        sku_field = "inbound_sku",
        output_name = "inbound_item_info",
        query_status = "采购",
        update_status_value = "国内入库",
        count_label = "待入库数", 
        count_field = "PendingQuantity", 
        refresh_trigger = unique_items_data_refresh_trigger,      
        con,                  
        input, output, session
      )
      
      # 如果未找到对应的 UniqueID，停止后续操作
      if (is.null(unique_ID) || unique_ID == "") {
        showNotification(paste0("此SKU第 ", i, " 件物品不存在，已中止入库！"), type = "error")
        break
      }
      
      # 检查是否启用了瑕疵品选项
      defective_item <- input$defective_item
      defect_notes <- trimws(input$defective_notes)
      
      if (defective_item && defect_notes != "") {
        tryCatch({
          add_defective_note(
            con = con,
            unique_id = unique_ID,
            note_content = defect_notes,
            status_label = "瑕疵",
            refresh_trigger = NULL
          )
          showNotification("瑕疵品备注已成功添加！", type = "message")
        }, error = function(e) {
          showNotification(paste("添加备注时发生错误：", e$message), type = "error")
        })
      } else if (defective_item) {
        showNotification("无瑕疵品备注！", type = "warning")
      }
    }
    
    # 批量调整库存
    adjust_inventory_quantity(con, input$inbound_sku, adjustment = inbound_quantity)  # 根据输入的数量调整库存
    
    # 刷新 UI 和数据
    unique_items_data_refresh_trigger(!unique_items_data_refresh_trigger())
    
    # 重置输入
    updateTextInput(session, "inbound_sku", value = "")
    updateNumericInput(session, "inbound_quantity", value = 1)
    runjs("document.getElementById('inbound_sku').focus();")
  })
  
  
  # 监听选中行并更新 SKU
  observeEvent(unique_items_table_inbound_selected_row(), {
    selected_row <- unique_items_table_inbound_selected_row()
    if (length(selected_row) > 0) {
      # 仅处理最后一个选择的行
      last_selected <- tail(selected_row, 1) # 获取最后一个选择的行号
      selected_sku <- filtered_unique_items_data_inbound()[last_selected, "SKU", drop = TRUE]
      updateTextInput(session, "inbound_sku", value = selected_sku)
    }
  })
  
  # 控制备注输入框显示/隐藏
  observeEvent(input$defective_item, {
    if (input$defective_item) {
      shinyjs::show("defective_notes_container")
    } else {
      shinyjs::hide("defective_notes_container")
      updateTextInput(session, "defective_notes", value = "") # 清空备注
    }
  })
  
  # PDF下载按钮默认禁用
  session$onFlushed(function() {
    shinyjs::disable("download_select_pdf")
  })
  
  # 生成选中商品条形码 PDF
  observeEvent(input$export_select_btn, {
    # 获取选中行
    selected_rows <- unique_items_table_inbound_selected_row()  # 从 DT 表选中行
    if (is.null(selected_rows) || length(selected_rows) == 0) {
      showNotification("请先选中至少一个商品！", type = "error")
      return()
    }
    
    # 获取选中物品的数据
    selected_items <- filtered_unique_items_data_inbound()[selected_rows, ]
    if (nrow(selected_items) == 0) {
      showNotification("选中数据无效，请重新选择！", type = "error")
      return()
    }
    
    skus <- selected_items$SKU
    
    # 调用现有函数生成条形码 PDF
    tryCatch({
      pdf_file <- export_barcode_pdf(
        sku = skus,
        page_width = page_width,  # 全局变量
        page_height = page_height,
        unit = size_unit
      )
      barcode_pdf_file_path(pdf_file)  # 保存生成的 PDF 路径
      
      showNotification("选中商品条形码已生成！", type = "message")
      shinyjs::enable("download_select_pdf")  # 启用下载按钮
    }, error = function(e) {
      showNotification(paste("生成条形码失败：", e$message), type = "error")
      shinyjs::disable("download_select_pdf")  # 禁用下载按钮
    })
  })
  
  # 下载选中商品条形码 PDF
  output$download_select_pdf <- downloadHandler(
    filename = function() {
      basename(barcode_pdf_file_path())  # 生成文件名
    },
    content = function(file) {
      file.copy(barcode_pdf_file_path(), file, overwrite = TRUE)
      shinyjs::disable("download_select_pdf")  # 禁用下载按钮
      barcode_pdf_file_path(NULL)  # 清空路径
    }
  )
  
  
  
  ################################################################
  ##                                                            ##
  ## 出库分页                                                   ##
  ##                                                            ##
  ################################################################
  
  # 监听标签页切换事件
  observeEvent(input$inventory_china, {
    if (input$inventory_china == "出库") {
      runjs("document.getElementById('outbound_sku').focus();")
    }
  })
  
  # 物品表过滤模块
  itemFilterServer(
    id = "outbound_filter",
    makers_items_map = makers_items_map
  )
  
  # 监听出库 SKU 输入
  observeEvent(input$outbound_sku, {
    handleSkuInput(
      sku_input = input$outbound_sku,
      output_name = "outbound_item_info",
      count_label = "可出库数",
      count_field = "AvailableForOutbound",
      con = con,
      output = output,
      placeholder_path = placeholder_300px_path,
      host_url = host_url
    )
  })
  
  # 自动出库逻辑
  observeEvent(input$outbound_sku, {
    req(input$auto_outbound)  # 仅在自动出库勾选时触发
    req(input$outbound_sku)   # 确保 SKU 输入框不为空
    
    # 调用出库处理逻辑
    handleOperation(
      unique_items_data(),
      operation_name = "出库", 
      sku_field = "outbound_sku",
      output_name = "outbound_item_info",
      query_status = "国内入库",
      update_status_value = "国内出库",
      count_label = "可出库数", 
      count_field = "AvailableForOutbound", 
      refresh_trigger = unique_items_data_refresh_trigger,     
      con,                  
      input, output, session
    )

    # 清空 SKU 输入框
    updateTextInput(session, "outbound_sku", value = "")
    runjs("document.getElementById('outbound_sku').focus();")
  })
  
  # 手动确认出库逻辑
  observeEvent(input$confirm_outbound_btn, {
    handleOperation(
      unique_items_data(),
      operation_name = "出库", 
      sku_field = "outbound_sku",
      output_name = "outbound_item_info",
      query_status = "国内入库",
      update_status_value = "国内出库",
      count_label = "可出库数", 
      count_field = "AvailableForOutbound", 
      refresh_trigger = unique_items_data_refresh_trigger,     
      con,                  
      input, output, session
    )
    
    updateTextInput(session, "outbound_sku", value = "")
    runjs("document.getElementById('outbound_sku').focus();")
  })
  
  # 撤回出库逻辑
  observeEvent(input$revert_outbound_btn, {
    handleOperation(
      unique_items_data(),
      operation_name = "撤回", 
      sku_field = "outbound_sku",
      output_name = "outbound_item_info",
      query_status = "国内出库",
      update_status_value = "国内入库",
      count_label = "可出库数", 
      count_field = "AvailableForOutbound", 
      refresh_trigger = unique_items_data_refresh_trigger, 
      clear_field = "DomesticExitTime", # 清空出库日期字段
      clear_shipping_method = TRUE, # 清空出库国际运输方式
      con,                  
      input, output, session
    )
    
    updateTextInput(session, "outbound_sku", value = "")
    runjs("document.getElementById('outbound_sku').focus();")
  })
  
  # 监听选中行并显示大图与物品信息
  observeEvent(unique_items_table_outbound_selected_row(), {
    if (!is.null(unique_items_table_outbound_selected_row()) && length(unique_items_table_outbound_selected_row()) > 0) {
      selected_sku <- filtered_unique_items_data_outbound()[unique_items_table_outbound_selected_row(), "SKU", drop = TRUE]
      handleSkuInput(
        sku_input = selected_sku,
        output_name = "outbound_item_info",
        count_label = "可出库数",
        count_field = "AvailableForOutbound",
        con = con,
        output = output,
        placeholder_path = placeholder_300px_path,
        host_url = host_url
      )
    }
  })
  
  
  
  ################################################################
  ##                                                            ##
  ## 售出分页                                                   ##
  ##                                                            ##
  ################################################################
  
  # 初始化模块绑定状态
  sold_filter_initialized <- reactiveVal(FALSE)
  
  # 动态更新侧边栏内容
  observe({
    req(input$sold_tabs)  # 确保主面板选项存在
    
    if (input$sold_tabs == "物品售出") {
      
      # 渲染动态侧边栏
      output$dynamic_sidebar <- renderUI({
        itemFilterUI(id = "sold_filter", border_color = "#28A745", text_color = "#28A745", status_choices = c("所有状态" = "", "国内入库", "国内出库", "美国入库", "美国调货", "国内售出"))
      })
      
      # 确保模块仅绑定一次
      if (!sold_filter_initialized()) {
        sold_filter_initialized(TRUE)  # 标记模块已绑定
        # 确保侧边栏渲染后绑定服务器逻辑
        session$onFlushed(function() {
          itemFilterServer(
            id = "sold_filter",
            makers_items_map = makers_items_map
          )
        })
      }
    } else if (input$sold_tabs == "订单管理") {
      # 订单管理分页：显示订单筛选区
      output$dynamic_sidebar <- renderUI({
        div(
          class = "card",
          style = "margin-bottom: 5px; padding: 15px; border: 1px solid #28A745; border-radius: 8px;",
          tags$h4("订单筛选", style = "color: #28A745; font-weight: bold;"),
          
          textInput("filter_order_id", "订单号", placeholder = "输入订单号", width = "100%"),
          textInput("filter_tracking_id", "运单号", placeholder = "输入运单号", width = "100%"),
          
          fluidRow(
            column(6, 
                   textInput("filter_customer_name", "顾客姓名", placeholder = "输入顾客姓名", width = "100%")),
            column(6, 
                   textInput("filter_customer_netname", "顾客网名", placeholder = "输入顾客网名", width = "100%"))
          ),
          
          fluidRow(
            column(6, 
                   selectInput(
                     inputId = "filter_platform",
                     label = "电商平台",
                     choices = c("所有平台" = "", "Etsy", "Shopify", "TikTok", "其他"),
                     selected = "",
                     width = "100%"
                   )),
            column(6, 
                   selectInput(
                     inputId = "filter_order_status",
                     label = "订单状态",
                     choices = c("所有状态" = "", "备货", "预定", "调货", "装箱", "发出", "在途", "送达"),
                     selected = "",
                     width = "100%"
                   ))
          ),
          
          fluidRow(
            column(6, 
                   textInput("filter_sku", "SKU反查", placeholder = "输入SKU", width = "100%")),
            column(6, 
                   autocompleteInputUI("sold", label = "商品名反查", placeholder = "输入商品名"))
          ),
          
          fluidRow(
            column(6, 
                   actionButton("delete_order_btn", "删除订单", class = "btn-danger", style = "width: 100%;")),
            column(6, 
                   actionButton("reset_filter_btn", "清空筛选条件", class = "btn-info", style = "width: 100%;"))
          )
        )
      })
    }
  })
  
  ############################ 
  #####   物品售出子页   ##### 
  ############################ 
  
  # 监听增加运单号按钮点击
  observeEvent(input$add_tracking_btn, {
    rows <- tracking_rows()
    if (rows < 3) {  # 最多允许添加2个运单号
      tracking_rows(rows + 1)
    } else {
      showNotification("最多只能添加 2 个运单号！", type = "warning")
    }
  })
  
  # 响应点击物品表的行，更新货架上的物品
  observe({
    selected_row <- unique_items_table_sold_selected_row()  # 获取选中的行
    sort_order <- input$arrow_direction  # 获取排序方向
    
    # 如果未选中行或未设置排序方向，则退出
    if (is.null(selected_row) || length(selected_row) == 0 || is.null(sort_order)) {
      return()
    }
    
    tryCatch({
      # 获取选中行对应的 SKU
      selected_sku <- filtered_unique_items_data_sold()[selected_row, "SKU", drop = TRUE]
      
      if (is.null(selected_sku) || selected_sku == "") {
        showNotification("未找到有效的 SKU！", type = "error")
        return()
      }
      
      # 从 unique_items_data 获取货架中符合条件的物品
      all_shelf_items <- get_shelf_items(data = unique_items_data(), sku = selected_sku, sort_order = sort_order)
      
      if (is.null(all_shelf_items)) {
        showNotification("货架上未找到对应 SKU 的物品！", type = "error")
        return()
      }
      
      # 从箱子中获取当前 SKU 的已选数量
      box_data <- box_items()
      box_sku_count <- sum(box_data$SKU == selected_sku)
      
      # 检查是否所有物品已移入箱子
      if (box_sku_count >= nrow(all_shelf_items)) {
        shelf_items(create_empty_shelf_box())  # 清空货架
        showNotification("该 SKU 的所有物品已移入箱子，货架已清空！", type = "message")
        return()
      }
      
      # 更新货架数据，移除已移入箱子的物品
      if (box_sku_count == 0) {
        updated_shelf_items <- all_shelf_items
      } else {
        updated_shelf_items <- all_shelf_items[-seq_len(box_sku_count), ]
      }
      
      shelf_items(updated_shelf_items)
      showNotification(paste("已加载 SKU:", selected_sku, "的货架物品！"), type = "message")
    }, error = function(e) {
      showNotification(paste("加载货架时发生错误：", e$message), type = "error")
    })
  })
  
  ##### 网名自动填写
  
  matching_customer <- reactive({
    req(input$customer_name)  # 确保用户输入了顾客姓名
    tryCatch({
      # 将用户输入和数据中的姓名都转换为大写
      customer_name_upper <- toupper(input$customer_name)
      result <- orders() %>%
        mutate(CustomerNameUpper = toupper(CustomerName)) %>%  # 添加大写姓名列
        filter(grepl(customer_name_upper, CustomerNameUpper))  # 模糊匹配大写姓名
      
      valid_result <- result %>%
        filter(!is.na(CustomerNetName) & CustomerNetName != "") %>%  # 过滤有效的网名
        slice_head(n = 1)  # 仅返回第一条有网名的记录
      
      # 返回第一个有效的网名或 NULL
      if (nrow(valid_result) > 0) {
        return(valid_result$CustomerNetName[1])
      } else {
        return(NULL)  # 没有匹配的网名
      }
    }, error = function(e) {
      showNotification("网名查找出错！", type = "error")
      return(NULL)
    })
  })
  
  
  # 缓存最近查询过的顾客姓名与网名
  cache <- reactiveVal(list())
  
  # 使用 debounce 避免频繁触发查询
  customer_name_delayed <- debounce(reactive(input$customer_name), 300)
  
  # 网名自动填写
  observeEvent(customer_name_delayed(), {
    # 如果用户清空了 customer_name，则清空 customer_netname
    if (customer_name_delayed() == "") {
      updateTextInput(session, "customer_netname", value = "")
      return()
    }
    
    req(customer_name_delayed())  # 确保用户输入不为空
    
    cache_data <- cache()
    
    # 检查缓存是否已有数据
    if (customer_name_delayed() %in% names(cache_data)) {
      netname <- cache_data[[customer_name_delayed()]]
    } else {
      # 查询数据库
      netname <- matching_customer()
      
      # 如果有结果，更新缓存
      if (!is.null(netname)) {
        cache_data[[customer_name_delayed()]] <- netname
        cache(cache_data)  # 更新缓存
      }
    }
    
    # 更新网名输入框
    updateTextInput(session, "customer_netname", value = netname %||% "")
  })
  
  ######
  
  #运单PDF上传模块
  observeEvent(input$shiplabel_pdf_upload, {
    req(input$shiplabel_pdf_upload)
    
    # PDF 文件路径
    pdf_path <- input$shiplabel_pdf_upload$datapath
    
    tryCatch({
      # 检查 PDF 的页数
      pdf_info <- pdftools::pdf_info(pdf_path)
      if (pdf_info$pages != 1) {
        output$upload_status_message <- renderUI({
          tags$p("仅允许上传单页运单文件，请重新上传。", style = "color: red;")
        })
        return()
      }
      
      label_info <- extract_shipping_label_info(pdf_path)

      # 提取的姓名填充到输入框
      updateTextInput(session, "customer_name", value = label_info$customer_name)
      
      # 将提取的运单号填充到输入框
      updateTextInput(session, "tracking_number", value = label_info$tracking_number)
      shinyjs::disable("tracking_number")
      
      # 保存文件到目标目录
      dest_file <- file.path("/var/uploads/shiplabels", paste0(label_info$tracking_number, ".pdf"))
      file.copy(pdf_path, dest_file, overwrite = TRUE)

      # 上传成功提示
      output$upload_status_message <- renderUI({
        tags$p("运单上传成功！运单信息已识别", style = "color: green;")
      })
    }, error = function(e) {
      output$upload_status_message <- renderUI({
        tags$p(paste0("文件上传失败！", e), style = "color: red;")
      })
    })
    
    # 延时清空提示信息
    later::later(function() {
      output$upload_status_message <- renderUI({
        NULL  # 清空提示信息
      })
    }, delay = 3)  # 延迟 3 秒后执行
  })
  
  # 出售订单图片处理模块
  image_sold <- imageModuleServer("image_sold")
  
  # 在输入订单号时检查订单信息并填充
  observeEvent(input$order_id, {
    # 检查订单号是否为空
    req(input$order_id)  # 如果订单号为空，停止执行
    
    tryCatch({
      # 去除空格和#号
      sanitized_order_id <- gsub("#", "", trimws(input$order_id))
      
      # 查询订单信息，包含新增字段
      existing_order <- orders() %>% {
        if (grepl("@", sanitized_order_id)) {
          # 如果 OrderID 包含 "@"
          at_prefix <- sub("@.*", "", sanitized_order_id)  # 提取 "@" 之前的所有字符
          filter(., grepl(paste0("^", at_prefix, "@"), OrderID))  # 匹配包含 "@" 且符合前缀的 OrderID
        } else {
          # 如果 OrderID 不包含 "@"
          filter(., OrderID == sanitized_order_id)
        }
      }
      
      # 如果订单存在，填充对应字段
      if (nrow(existing_order) > 0) {
        # 填充各字段信息
        updateSelectInput(session, "platform", selected = existing_order$Platform[1])
        
        updateTextInput(session, "customer_name", value = existing_order$CustomerName[1])
        # updateTextInput(session, "customer_netname", value = existing_order$CustomerNetName[1]) # 交给网名自动填写功能
        
        if (!is.null(existing_order$OrderStatus[1]) && !is.na(existing_order$OrderStatus[1])) {
          if (existing_order$OrderStatus[1] == "调货") {
            updateCheckboxInput(session, "is_transfer_order", value = TRUE)
            updateCheckboxInput(session, "is_preorder", value = FALSE)  # 确保互斥
          } else if (existing_order$OrderStatus[1] == "预定") {
            updateCheckboxInput(session, "is_transfer_order", value = FALSE)  # 确保互斥
            updateCheckboxInput(session, "is_preorder", value = TRUE)
            
            # 从备注中提取预定供应商
            if (!is.null(existing_order$OrderNotes[1]) && !is.na(existing_order$OrderNotes[1])) {
              supplier_prefix <- "【供应商】"
              # 使用正则表达式提取供应商信息
              supplier_match <- regmatches(existing_order$OrderNotes[1], 
                                           regexpr(paste0(supplier_prefix, "(.*?)；"), existing_order$OrderNotes[1]))
              if (length(supplier_match) > 0) {
                supplier_name <- sub(paste0(supplier_prefix, "(.*?)；"), "\\1", supplier_match)  # 提取中间的供应商名称
                updateSelectizeInput(session, "preorder_supplier", selected = supplier_name)  # 更新下拉菜单
              }
            }
          } else {
            # 其他情况，全部复选框设为 FALSE
            updateCheckboxInput(session, "is_transfer_order", value = FALSE)
            updateCheckboxInput(session, "is_preorder", value = FALSE)
            updateSelectizeInput(session, "preorder_supplier", selected = NULL)  # 清空供应商下拉菜单
          }
        } else {
          # 如果 OrderStatus 为空或 NULL，清空复选框和下拉菜单
          updateCheckboxInput(session, "is_transfer_order", value = FALSE)
          updateCheckboxInput(session, "is_preorder", value = FALSE)
          updateSelectizeInput(session, "preorder_supplier", selected = NULL)
        }
        
        updateTextInput(session, "tracking_number", value = existing_order$UsTrackingNumber[1])
        # 检查 LabelStatus
        if (existing_order$LabelStatus[1] != "无") {
          shinyjs::disable("tracking_number")  # 禁用输入框
        } else {
          shinyjs::enable("tracking_number")  # 启用输入框（以防之前禁用过）
        }
        
        updateTextAreaInput(session, "order_notes", value = existing_order$OrderNotes[1])
        
        # 动态更新按钮为“更新订单”
        output$register_order_button_ui <- renderUI({
          actionButton(
            "register_order_btn",
            "更新订单",
            icon = icon("edit"),
            class = "btn-success",
            style = "font-size: 16px; min-width: 130px; height: 42px;"
          )
        })
        
        showNotification("已找到订单信息！字段已自动填充", type = "message")
      } else {
        # 如果订单记录不存在，清空出order ID以外所有相关字段
        showNotification("未找到对应订单记录，可登记新订单", type = "warning")
        
        # 重置所有输入框, 除了order ID
        reset_order_form(session, image_sold, keep_order_id = TRUE)
        
        # 动态更新按钮为“登记订单”
        output$register_order_button_ui <- renderUI({
          actionButton(
            "register_order_btn",
            "登记订单",
            icon = icon("plus"),
            class = "btn-primary",
            style = "font-size: 16px; min-width: 130px; height: 42px;"
          )
        })
      }
    }, error = function(e) {
      # 捕获错误并通知用户
      showNotification(paste("检查订单时发生错误：", e$message), type = "error")
    })
  })
  
  # 确保调货和预定两个勾选框互斥
  observeEvent(input$is_transfer_order, {
    if (input$is_transfer_order) {
      updateCheckboxInput(session, "is_preorder", value = FALSE)
    }
  })
  
  # 确保调货和预定两个勾选框互斥
  observeEvent(input$is_preorder, {
    if (input$is_preorder) {
      updateCheckboxInput(session, "is_transfer_order", value = FALSE)
    }
  })
  
  # 动态填充供应商选择器
  observe({
    update_maker_choices(session, "preorder_supplier", maker_list())
  })
  
  # 控制预订单供应商选择器的显示
  observeEvent(input$is_preorder, {
    if (input$is_preorder) {
      # 显示供应商选择器
      shinyjs::show("preorder_supplier")
    } else {
      # 隐藏供应商选择器并清空选择
      shinyjs::hide("preorder_supplier")
      updateSelectizeInput(session, "preorder_supplier", selected = NULL)
    }
  })
  
  
  # 登记订单逻辑
  observeEvent(input$register_order_btn, {
    if (is.null(input$order_id) || input$order_id == "") {
      showNotification("订单号不能为空！", type = "error")
      return()
    }
    
    if (is.null(input$platform) || input$platform == "") {
      showNotification("电商平台不能为空，请选择一个平台！", type = "error")
      return()
    }
    
    # 去除空格和#号
    sanitized_order_id <- gsub("#", "", trimws(input$order_id))
    
    # 调用封装函数登记订单
    order_registered <- register_order(
      order_id = sanitized_order_id,
      customer_name = input$customer_name,
      customer_netname = input$customer_netname,
      platform = input$platform,
      order_notes = input$order_notes,
      tracking_number = input$tracking_number,
      image_data = image_sold,
      con = con,
      orders = orders,
      box_items = box_items,
      unique_items_data = unique_items_data,
      is_transfer_order = input$is_transfer_order,
      is_preorder = input$is_preorder,
      preorder_supplier = input$preorder_supplier
    )
    
    # 如果订单登记失败，直接退出
    if (!order_registered) {
      return()
    }
    
    orders_refresh_trigger(!orders_refresh_trigger())
  })
  
  # 清空订单信息按钮
  observeEvent(input$clear_order_btn, {
    reset_order_form(session, image_sold)
    showNotification("已清空所有输入！", type = "message")
  })
  
  ######
  
  # 渲染货架
  output$shelf_table <- renderDT({
    datatable_and_names <- render_table_with_images(shelf_items(), 
                                                    column_mapping = list(
                                                      SKU = "条形码",
                                                      ItemImagePath = "商品图",
                                                      ItemName = "商品名",
                                                      Status = "库存态",
                                                      Defect = "瑕疵态",
                                                      ProductCost = "单价"
                                                    ), 
                                                    selection = "single",
                                                    image_column = "ItemImagePath",
                                                    options = list(
                                                      scrollY = "278px",  # 根据内容动态调整滚动高度
                                                      scrollX = TRUE,  # 支持水平滚动
                                                      fixedHeader = TRUE,  # 启用表头固定
                                                      paging = TRUE,  # 启用分页
                                                      pageLength = 30,      # 每页显示30条
                                                      dom = 'frtip',         # 控制表格显示控件，去掉多余的功能
                                                      searching = FALSE  # 禁止搜索
                                                    ))
    
    table <- apply_dynamic_styles(datatable_and_names$datatable, datatable_and_names$column_names)
    table
  })
  
  # 渲染箱子
  output$box_table <- renderDT({
    datatable_and_names <- render_table_with_images(box_items(), 
                                                    column_mapping = list(
                                                      SKU = "条形码",
                                                      ItemImagePath = "商品图",
                                                      ItemName = "商品名",
                                                      Status = "库存态",
                                                      Defect = "瑕疵态",
                                                      ProductCost = "单价"
                                                    ), 
                                                    selection = "single",
                                                    image_column = "ItemImagePath",
                                                    options = list(
                                                      scrollY = "220px",  # 根据内容动态调整滚动高度
                                                      scrollX = TRUE,  # 支持水平滚动
                                                      fixedHeader = TRUE,  # 启用表头固定
                                                      paging = TRUE,  # 启用分页
                                                      pageLength = 30,      # 每页显示30条
                                                      dom = 'frtip',         # 控制表格显示控件，去掉多余的功能
                                                      searching = FALSE  # 禁止搜索
                                                    ))
    table <- apply_dynamic_styles(datatable_and_names$datatable, datatable_and_names$column_names)
    table
  })
  
  # 渲染货架物品数量
  output$shelf_count <- renderText({
    shelf_items <- shelf_items()  # 获取当前货架上的物品
    paste0("(", nrow(shelf_items), ")")  # 返回数量显示
  })
  
  # 渲染发货箱物品数量
  output$box_count <- renderText({
    box_items <- box_items()  # 获取当前发货箱内的物品
    paste0("(", nrow(box_items), ")")  # 返回数量显示
  })
  
  # 点击货架物品，移入箱子
  observeEvent(input$shelf_table_rows_selected, {
    selected_row <- input$shelf_table_rows_selected
    shelf_data <- shelf_items()
    
    if (!is.null(selected_row) && nrow(shelf_data) >= selected_row) {
      selected_item <- shelf_data[selected_row, ]  # 获取选中的物品
      sku <- selected_item$SKU  # 获取SKU
      status <- selected_item$Status  # 获取库存状态
      
      # 查询当前 SKU 的美国入库库存数量
      us_stock_count <- sum(shelf_data$SKU == sku & shelf_data$Status == "美国入库")
      
      if (status == "美国入库" && us_stock_count <= 1) {
        showModal(modalDialog(
          title = "注意",
          p("此商品在美国库存仅剩一件，请沟通核实后再进行调货"),
          footer = tagList(
            actionButton("verify_and_proceed", "已核实, 继续调货", class = "btn-primary"),
            modalButton("取消")
          ),
          easyClose = FALSE
        ))
      } else {
        # 直接执行入箱操作
        updateBox(selected_item, selected_row, shelf_data)
      }
    }
  })
  
  observeEvent(input$verify_and_proceed, {
    removeModal()  # 移除弹窗
    selected_row <- input$shelf_table_rows_selected
    shelf_data <- shelf_items()
    
    if (!is.null(selected_row) && nrow(shelf_data) >= selected_row) {
      selected_item <- shelf_data[selected_row, ]  # 获取选中的物品
      
      # 执行入箱操作
      updateBox(selected_item, selected_row, shelf_data)
    }
  })
  
  updateBox <- function(selected_item, selected_row, shelf_data) {
    # 更新箱子内容
    current_box <- box_items()
    box_items(bind_rows(selected_item, current_box))
    
    # 更新货架上的物品，移除已选的
    updated_shelf <- shelf_data[-selected_row, ]
    shelf_items(updated_shelf)
    
    showNotification("物品已移入箱子！", type = "message")
  }
  
  
  # 点击箱子物品，还回货架
  observeEvent(input$box_table_rows_selected, {
    selected_row <- input$box_table_rows_selected
    box_data <- box_items()
    
    if (!is.null(selected_row) && nrow(box_data) >= selected_row) {
      selected_item <- box_data[selected_row, ]  # 获取选中的物品
      
      # 更新货架内容
      current_shelf <- shelf_items()
      shelf_items(bind_rows(current_shelf, selected_item))
      
      # 更新箱子内的物品，移除已选的
      updated_box <- box_data[-selected_row, ]
      box_items(updated_box)
      
      showNotification("物品已还回货架！", type = "message")
    }
  })
  
  # 扫码上架功能
  observeEvent(input$sku_to_shelf, {
    req(input$sku_to_shelf)  # 确保输入框不为空
    
    tryCatch({
      # 获取输入的 SKU
      scanned_sku <- trimws(input$sku_to_shelf)
