      }
    }
  })
  
  # 确认入库逻辑
  observeEvent(input$confirm_inbound_btn, {
    # 从输入中获取入库数量，确保为正整数
    inbound_quantity <- as.integer(input$inbound_quantity)
    if (is.na(inbound_quantity) || inbound_quantity <= 0) {
      showNotification("入库数量必须是一个正整数！", type = "error")
      return()
    }
    
    # 批量处理入库逻辑
    for (i in seq_len(inbound_quantity)) {
      unique_id <- handleOperation(
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
      
      # 如果未找到对应的 UniqueID，停止后续操作
      if (is.null(unique_id) || unique_id == "") {
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
  ## 发货分页                                                   ##
  ##                                                            ##
  ################################################################
  
  # 页面切换时的聚焦
  observeEvent({
    req(input$inventory_us, input$shipping_tabs) # 确保两个输入都有效
    list(input$inventory_us, input$shipping_tabs)
  }, {
    if (input$inventory_us == "发货" && input$shipping_tabs == "国内售出发货") {
      runjs("document.getElementById('shipping_bill_number').focus();")
    }
    if (input$inventory_us == "发货" && input$shipping_tabs == "美国售出发货") {
      runjs("document.getElementById('us_shipping_bill_number').focus();")
    }
  })
  
  #############################################  数据准备
  
  # 当前订单ID
  current_order_id <- reactiveVal()
  
  # 装载匹配运单号的订单
  matching_orders <- reactive({
    # 如果运单号为空，返回空数据框
    if (is.null(input$shipping_bill_number) || input$shipping_bill_number == "") {
      return(data.frame())  # 返回空数据框
    }
    
    data <- match_tracking_number(orders(), "UsTrackingNumber", input$shipping_bill_number)
    
    data %>% arrange(OrderStatus == "装箱")
  })
  
  # 自动装载订单ID：current_order_id
  observe({
    req(matching_orders())  # 确保 matching_orders 存在
    
    if (nrow(matching_orders()) > 0) {
      # 设置第一个订单的 OrderID 为当前订单 ID
      current_order_id(matching_orders()$OrderID[1])
    }
  })
  
  # 装载当前订单物品信息
  order_items <- reactive({
    # 如果当前订单 ID 为空，返回空数据框
    if (is.null(current_order_id()) || trimws(current_order_id()) == "") {
      return(data.frame())  # 返回空数据框
    }
    # 筛选当前订单的物品
    unique_items_data() %>% filter(OrderID == current_order_id())
  })
  
  
  #############################################  渲染
  
  # 渲染订单信息卡片
  observe({
    req(input$shipping_bill_number, orders())
    
    if (nrow(matching_orders()) == 0) {
      renderOrderInfo(output, "order_info_card", data.frame())  # 清空订单信息卡片
      current_order_id(NULL)  # 清空当前订单 ID
      return()
    }
    
    renderOrderInfo(output, "order_info_card", matching_orders())
    
    all_packed <- all(matching_orders()$OrderStatus == "装箱")
    if (all_packed) {
      showModal(modalDialog(
        title = "运单完成提示",
        "当前运单号所对应的所有订单已完成装箱操作！",
        easyClose = TRUE,
        footer = NULL  # 不需要关闭按钮
      ))
      
      updateTextInput(session, "shipping_bill_number", value = "")
      runjs("document.getElementById('shipping_bill_number').focus();")
      
      # 延迟 2 秒后自动关闭弹窗
      shinyjs::delay(2000, removeModal())
    }
  })
  
  # 渲染订单物品标题
  observe({
    req(input$shipping_bill_number)
    
    # 如果 current_order_id 为空，清空标题
    if (is.null(current_order_id()) || trimws(current_order_id()) == "") {
      output$order_items_title <- renderUI({ NULL })  # 清空标题
      return()  # 停止后续逻辑
    }
    
    # 渲染标题
    output$order_items_title <- renderUI({
      tags$h4(
        HTML(paste0(as.character(icon("box")), " 订单号 ", current_order_id(), " 的物品")),
        style = "color: #28A745; font-weight: bold; margin-bottom: 15px;"
      )
    })
  })
  
  # 渲染物品信息卡片  
  observe({
    req(input$shipping_bill_number, order_items())
    
    if (nrow(order_items()) == 0) {
      renderOrderItems(output, "shipping_order_items_cards", data.frame(), con)  # 清空物品卡片
      return()
    }
    
    renderOrderItems(output, "shipping_order_items_cards", order_items(), con)
  })
  
  
  #############################################  逻辑
  
  # 延迟响应输入订单号，给手动输入留出空间
  debounced_order_id <- debounce(reactive(input$order_id_input), millis = 1000)  # 延迟 1000 毫秒
  
  # 输入订单号填写运单号
  observe({
    req(debounced_order_id())  # 确保输入框非空
    
    order_id <- trimws(debounced_order_id())
    
    result <- orders() %>%
      filter(OrderID == order_id) %>%
      select(UsTrackingNumber)
    
    # 更新运单号
    if (!is.null(result) && nrow(result) > 0) {
      updateTextInput(session, "shipping_bill_number", value = result$UsTrackingNumber[1])
      showNotification("运单号更新成功！", type = "message")
    } else {
      showNotification("未找到相关订单，请检查输入！", type = "error")
    }
    updateTextInput(session, "order_id_input", value = "")
  })
  
  # 清空运单号逻辑
  observeEvent(input$shipping_bill_number, {
    if (is.null(input$shipping_bill_number) || input$shipping_bill_number == "") {
      output$dynamic_ship_button <- renderUI({ NULL })
      label_pdf_file_path(NULL)  # 清空运单文件路径
      
      shinyjs::delay(3000, {
        current_order_id(NULL)  # 清空当前订单 ID
        output$order_items_title <- renderUI({ NULL })  # 清空标题
        renderOrderItems(output, "shipping_order_items_cards", data.frame(), con)  # 清空物品卡片
        renderOrderInfo(output, "order_info_card", data.frame())  # 清空订单信息卡片
      })
    }
  })
  
  # 点击订单卡片逻辑
  observeEvent(input$selected_order_id, {
    req(input$selected_order_id)  # 确保订单 ID 存在
    
    # 获取选中的订单 ID
    current_order_id(input$selected_order_id)
    
    # 更新高亮样式
    runjs(sprintf("
      $('.order-card').css('border-color', '#ddd');  // 清除其他卡片高亮
      $('.order-card').css('box-shadow', '0px 4px 8px rgba(0, 0, 0, 0.1)');  // 恢复默认阴影
      $('#order_card_%s').css('border-color', '#007BFF');  // 高亮选中卡片
      $('#order_card_%s').css('box-shadow', '0px 4px 8px rgba(0, 123, 255, 0.5)');  // 添加高亮阴影
    ", current_order_id(), current_order_id()))
    
    # 聚焦 SKU 输入框
    runjs("document.getElementById('sku_input').focus();")
  })
  
  # 监视订单信息状态，提示操作，动态显示按钮
  observe({
    req(input$shipping_bill_number, unique_items_data(), matching_orders(), current_order_id())
    
    # 获取当前选中订单信息
    current_order <- matching_orders() %>% filter(OrderID == current_order_id())
    # 确保选中订单存在
    req(nrow(current_order) > 0)
    
    # 存储运单文件路径
    label_pdf_file_path(file.path("/var/uploads/shiplabels", paste0(current_order$UsTrackingNumber, ".pdf")))
    
    # 获取当前订单内的物品
    current_items <- order_items()
    
    # 提示操作或警告
    if (current_order$OrderStatus != "装箱") {
      if (current_order$OrderStatus != "备货") {
        showNotification(
          paste0("当前订单状态为 '", current_order$OrderStatus, "' ，操作可能受限！请核对后继续。"),
          type = "warning"
        )
      } else { #如果订单状态为备货
        # 如果订单内无物品
        if (nrow(current_items) == 0) {
          showModal(modalDialog(
            title = "订单内无物品",
            div(
              "当前订单内未检测到任何物品，请核对订单信息无误后手动发货",
              style = "font-size: 16px; margin-bottom: 10px;"
            ),
            footer = NULL,
            easyClose = TRUE
          ))
          shinyjs::delay(2000, removeModal())
        } else { # 如果订单内有物品
          runjs("document.getElementById('sku_input').focus();")
          showNotification(
            paste0("请为订单 ", current_order_id(), " 扫描或输入SKU条码！"),
            type = "message"
          )
          
          # 检查是否符合装箱条件
          if (all(current_items$Status == "美国发货")) {
            order_notes <- current_order$OrderNotes
            has_transfer_note <- grepl("调货", order_notes, fixed = TRUE)
            
            if (has_transfer_note) {
              showModal(modalDialog(
                title = "调货物品",
                easyClose = FALSE,
                div(
                  style = "padding: 10px; font-size: 16px; color: #FF0000;",
                  paste0("订单 ", current_order_id(), " 混合了调货物品，请核对物品备齐后手动发货。")
                ),
                footer = tagList(
                  modalButton("关闭")
                )
              ))
            } else {
              showModal(modalDialog(
                title = "确认装箱",
                easyClose = FALSE,
                div(
                  style = "padding: 10px; font-size: 16px;",
                  paste0("订单 ", current_order_id(), " 的所有物品已完成入箱扫描")
                ),
                footer = tagList(
                  actionButton("confirm_shipping_btn", "确认装箱", icon = icon("check"), class = "btn-primary")
                )
              ))
            }
          }
        }
      }
    }
    
    # 动态显示下载运单按钮
    output$dynamic_label_download_button <- renderUI({
      req(label_pdf_file_path())  # 确保 label_pdf_file_path 不为空
      
      label_text <- switch(
        current_order$LabelStatus,
        "无" = "无运单文件",
        "已上传" = "下载运单",
        "已打印" = "运单已打印",
        "无运单文件" # 默认值
      )
      
      if (current_order$LabelStatus == "无") {
        div(
          label_text,
          class = "btn btn-secondary",
          style = "background-color: grey; color: white; cursor: not-allowed; padding: 6px 12px; border-radius: 4px; display: inline-block; text-align: center;"
        )
      } else {
        downloadButton("download_shipping_label_pdf", label = label_text, class = "btn btn-primary")
      }
    })
    
    # 动态显示手动发货按钮
    output$dynamic_ship_button <- renderUI({
      if (current_order$OrderStatus == "装箱") {
        return(NULL)
      }
      
      order_notes <- current_order$OrderNotes
      has_transfer_note <- grepl("调货", order_notes, fixed = TRUE)
      
      if (nrow(current_items) == 0 || (all(current_items$Status == "美国发货") && has_transfer_note)) {
        return(actionButton("ship_order_btn", "手动发货", icon = icon("paper-plane"), class = "btn-success", style = "margin-top: 10px;", width = "100%"))
      }
      return(NULL)
    })
  })
  
  # SKU 输入逻辑
  observeEvent(input$sku_input, {
    req(input$shipping_bill_number, input$sku_input)
    
    sku <- trimws(input$sku_input)
    
    # 查找SKU对应的物品
    matching_item <- order_items() %>% filter(SKU == sku)
    
    # 如果未找到对应的 SKU
    if (nrow(matching_item) == 0) {
      showNotification("未找到商品，请检查输入的商品是否存在于本订单！", type = "error")
      updateTextInput(session, "sku_input", value = "")
      return()
    }
    
    # 查找第一个状态不为“美国发货”的物品
    next_item <- matching_item %>% filter(Status != "美国发货") %>% slice(1)
    
    # 如果所有物品状态均为“美国发货”
    if (nrow(next_item) == 0) {
      showNotification("该商品已完成操作（状态为 '美国发货'）！", type = "message")
      updateTextInput(session, "sku_input", value = "")
      return()
    }
    
    # 自动更新物品状态为“美国发货”
    tryCatch({
      update_status(
        con = con,
        unique_id = next_item$UniqueID,
        new_status = "美国发货",
        refresh_trigger = unique_items_data_refresh_trigger
      )
      
      # 清空输入框
      updateTextInput(session, "sku_input", value = "")
      
    }, error = function(e) {
      showNotification(paste("更新状态时发生错误：", e$message), type = "error")
    })
  })
  
  # 确认装箱逻辑
  observeEvent(input$confirm_shipping_btn, {
    tryCatch({
      # 更新订单状态为“装箱”
      update_order_status(
        order_id = current_order_id(),
        new_status = "装箱",
        refresh_trigger = orders_refresh_trigger,
        con = con
      )

      # 关闭模态框
      removeModal()
    }, error = function(e) {
      # 捕获错误并通知用户
      showNotification(paste("发生错误：", e$message), type = "error")
    })
  })
  
  # 清空国内售出发货填写逻辑
  observeEvent(input$clear_shipping_bill_btn, {
    updateTextInput(session, "shipping_bill_number", value = "")
    label_pdf_file_path(NULL)  # 清空运单文件路径
    showNotification("运单号和 SKU 输入框已清空！", type = "message")
  })
  
  # 手动发货按钮功能
  observeEvent(input$ship_order_btn, {
    update_order_status(
      order_id = current_order_id(),
      new_status = "装箱",
      refresh_trigger = orders_refresh_trigger,
      con = con
    )
  })
  
  # 定义运单下载处理器
  output$download_shipping_label_pdf <- downloadHandler(
    filename = function() {
      basename(label_pdf_file_path())
    },
    content = function(file) {
      file.copy(label_pdf_file_path(), file, overwrite = TRUE)
      tracking_number <- tools::file_path_sans_ext(basename(label_pdf_file_path()))
      # 更新数据库中的 LabelStatus 为 "已打印"
      dbExecute(
        con,
        "UPDATE orders SET LabelStatus = '已打印' WHERE UsTrackingNumber = ?",
        params = list(tracking_number)
      )
      orders_refresh_trigger(!orders_refresh_trigger())
    }
  )
  
  #####################
  ### 美国发货部分  ###
  #####################
  
  # 创建新加订单物品容器
  new_order_items <- reactiveVal()
  
  # 运单号输入、清空后的反应逻辑
  debounced_us_shipping_bill_number <- debounce(reactive(gsub("[^0-9]", "", trimws(input$us_shipping_bill_number))), 500)
  
  # 计算 SKU 的有效库存数量
  stock_data <- reactive({
    req(unique_items_data())  # 确保数据存在
    unique_items_data() %>%
      filter(Status == "美国入库", is.na(Defect) | Defect != "瑕疵") %>%  # 确保过滤条件有效
      group_by(SKU) %>%
      summarise(StockQuantity = n(), .groups = "drop")
  })
  
  # 动态生成订单
  new_order <- reactive({
    req(input$us_shipping_bill_number, input$us_shipping_platform)
    
    # 如果平台未选择或运单号为空，返回 NULL
    if (input$us_shipping_platform == "" || input$us_shipping_bill_number == "") {
      return(NULL)
    }
    
    # 确保 new_order_items 存在
    req(new_order_items())
    
    # 检查物品列表是否为空
    if (nrow(new_order_items()) == 0) {
      return(NULL)  # 如果没有物品，返回 NULL
    }
    
    # 去除空格并提取数字部分
    cleaned_us_bill_number <- debounced_us_shipping_bill_number()
    
    # 生成订单 ID
    generated_order_id <- generate_order_id(
      cleaned_us_bill_number,
      new_order_items()$UniqueID
    )
    
    # 创建动态订单数据
    data.frame(
      OrderID = generated_order_id,
      UsTrackingNumber = cleaned_us_bill_number,
      CustomerName = "",
      CustomerNickname = "",
      Platform = input$us_shipping_platform,
      OrderImagePath = "",
      OrderNotes = trimws(input$us_shipping_order_notes),
      OrderStatus = "备货",
      stringsAsFactors = FALSE
    )
  })
  
  # 动态渲染订单卡片
  observe({
    req(new_order())
    
    renderOrderInfo(output, "order_info_card", new_order(), clickable = FALSE)
    
    # 更新标题
    output$order_items_title <- renderUI({
      tags$h4(
        HTML(paste0(as.character(icon("box")), " 订单号 ", new_order()$OrderID, " 的物品")),
        style = "color: #28A745; font-weight: bold; margin-bottom: 15px;"
      )
    })
  })
  
  # 动态渲染订单物品卡片
  observe({
    req(new_order_items())
    renderOrderItems(output, "shipping_order_items_cards", new_order_items(), con, deletable = TRUE)
  })
  
  observeEvent(input$us_shipping_sku_input, {
    req(input$us_shipping_sku_input)
    
    # 获取输入 SKU
    new_sku <- trimws(input$us_shipping_sku_input)
    
    # 校验 SKU 是否有效
    valid_sku <- stock_data() %>% filter(SKU == new_sku)
    if (nrow(valid_sku) == 0) {
      showNotification("输入的 SKU 不存在或状态不为 '美国入库'！", type = "error")
      updateTextInput(session, "us_shipping_sku_input", value = "")
      return()
    }
    
    # 获取当前物品
    current_items <- new_order_items()
    if (!is.null(current_items)) {
      existing_count <- sum(current_items$SKU == new_sku)
      if (existing_count >= valid_sku$StockQuantity[1]) {
        showNotification(paste0("输入的 SKU '", new_sku, "' 已达到库存上限！"), type = "error")
        updateTextInput(session, "us_shipping_sku_input", value = "")
        return()
      }
    }
    
    # 筛选未被选择的物品
    available_items <- unique_items_data() %>%
      filter(SKU == new_sku & Status == "美国入库" & !(UniqueID %in% current_items$UniqueID))
    
    if (nrow(available_items) == 0) {
      showNotification("该 SKU 的库存已用尽！", type = "error")
      updateTextInput(session, "us_shipping_sku_input", value = "")
      return()
    }
    
    # 添加未被选择的第一件物品
    item_info <- available_items %>% slice(1)
    current_items <- rbind(current_items, item_info)
    new_order_items(current_items)
    
    updateTextInput(session, "us_shipping_sku_input", value = "")
  })
  
  observe({
    # 获取延迟后的输入值
    bill_number <- debounced_us_shipping_bill_number()
    
    if (bill_number == "") {
      renderOrderInfo(output, "order_info_card", data.frame())  # 清空订单信息卡片
      output$order_items_title <- renderUI({ NULL })  # 清空标题
      renderOrderItems(output, "shipping_order_items_cards", data.frame(), con)  # 清空物品卡片
      shinyjs::hide("us_shipping_sku_input")
    } else {
      # 延迟后执行的逻辑
      shinyjs::show("us_shipping_sku_input")
      runjs("document.getElementById('us_shipping_sku_input').focus();")
    }
  })
  
  zero_stock_items <- reactiveVal(list())  # 用于存储库存为零的物品
  outbound_stock_items <- reactiveVal(list())
  
  # 美国售出发货按钮
  observeEvent(input$us_ship_order_btn, {
    req(new_order(), new_order_items())
    
    order <- new_order()
    items <- new_order_items()
    
    if (nrow(items) == 0) {
      showNotification("没有物品需要发货！", type = "error")
      return()
    }
    
    tryCatch({
      # 生成拼图路径
      combined_image_paths <- items$ItemImagePath[!is.na(items$ItemImagePath) & items$ItemImagePath != ""]
      if (length(combined_image_paths) == 0) {
        showNotification("无法生成订单图片：没有有效的物品图片路径", type = "warning")
        order_image_path <- ""
      } else {
        montage_path <- paste0("/var/www/images/", order$OrderID, "_montage_", format(Sys.time(), "%Y%m%d%H%M%S"), ".jpg")
        order_image_path <- generate_montage(combined_image_paths, montage_path)
      }
      
      # 插入订单到 `orders` 表
      dbExecute(con,
                "INSERT INTO orders (OrderID, UsTrackingNumber, CustomerName, CustomerNetName, Platform, OrderImagePath, OrderNotes, OrderStatus, created_at, updated_at)
       VALUES (?, ?, ?, ?, ?, ?, ?, ?, NOW(), NOW())",
                params = list(
                  order$OrderID,
                  order$UsTrackingNumber,
                  order$CustomerName,
                  order$CustomerNickname,
                  order$Platform,
                  order_image_path,
                  order$OrderNotes,
                  "装箱"
                )
      )
      
      # 使用逐条更新逻辑更新物品状态和订单号
      for (i in seq_len(nrow(items))) {
        next_item <- items[i, ]
        
        # 更新状态
        update_status(
          con = con,
          unique_id = next_item$UniqueID,
          new_status = "美国发货",
          refresh_trigger = NULL
        )
        
        # 更新订单号
        update_order_id(
          con = con,
          unique_id = next_item$UniqueID,
          order_id = order$OrderID
        )
      }
      
      # 更新数据并触发 UI 刷新
      orders_refresh_trigger(!orders_refresh_trigger())
      unique_items_data_refresh_trigger(!unique_items_data_refresh_trigger())
      
      sku_list_str <- paste0("'", paste(unique(items$SKU), collapse = "','"), "'")  # 转换为 SQL 格式
      
      # **手动查询仅包含需要的 SKU 的最新数据**
      latest_unique_items <- dbGetQuery(con, paste0("
        SELECT 
          ui.SKU, 
          ui.Status, 
          inv.ItemName, 
          inv.ItemImagePath, 
          inv.Maker
        FROM unique_items AS ui
        JOIN inventory AS inv ON ui.SKU = inv.SKU
        WHERE ui.SKU IN (", sku_list_str, ")
      "))
      
      # 检查库存并记录库存为零的物品
      zero_items <- list()  # 临时列表存储库存为零的物品
      outbound_items <- list()  # 临时列表存储需要出库的物品
      
      for (sku in unique(items$SKU)) {
        adjust_inventory_quantity(
          con = con,
          sku = sku,
          adjustment = -nrow(items %>% filter(SKU == sku))
        )
        
        # 检查库存
        result <- latest_unique_items %>%
          filter(SKU == sku) %>%
          group_by(SKU, ItemName, ItemImagePath, Maker) %>%
          summarise(
            DomesticStock = sum(Status == "国内入库", na.rm = TRUE),
            InTransitStock = sum(Status == "国内出库", na.rm = TRUE),
            UsStock = sum(Status == "美国入库", na.rm = TRUE),
            .groups = "drop"
          )
 
        total_stock <- sum(result$DomesticStock, result$InTransitStock, result$UsStock)
        if (total_stock == 0) {
          zero_items <- append(zero_items, list(result))
        } else if (result$UsStock == 0 && result$InTransitStock == 0 && result$DomesticStock > 0) {
          outbound_items <- append(outbound_items, list(result))
        }
      }
      
      # 更新 zero_stock_items 和 outbound_stock_items
      zero_stock_items(zero_items)
      outbound_stock_items(outbound_items)
      
      # 结果展示
      added_order <- orders() %>% filter(OrderID == order$OrderID)
      renderOrderInfo(output, "order_info_card", added_order, clickable = FALSE)
      
      output$order_items_title <- renderUI({
        tags$h4(
          HTML(paste0(as.character(icon("box")), " 订单号 ", order$OrderID, " 的物品")),
          style = "color: #28A745; font-weight: bold; margin-bottom: 15px;"
        )
      })
      
      added_order_items <- unique_items_data() %>% filter(OrderID == order$OrderID)
      renderOrderItems(output, "shipping_order_items_cards", added_order_items, con, deletable = FALSE)

      # 弹出模态框提示补货和出库请求
      if (length(zero_items) > 0 || length(outbound_items) > 0) {
        modal_content <- tagList()
        
        if (length(zero_items) > 0) {
          modal_content <- tagAppendChildren(
            modal_content,
            tags$div(
              style = "padding: 10px; background-color: #ffe6e6; border-radius: 8px; margin-bottom: 20px;",
              tags$h4("需要采购补货：", style = "color: red; margin-bottom: 15px;"),
              tags$div(
                style = "display: grid; grid-template-columns: repeat(auto-fit, minmax(300px, 1fr)); gap: 20px;",
                lapply(zero_items, function(item) {
                  div(
                    style = "background: white; box-shadow: 0 4px 8px rgba(0,0,0,0.1); border-radius: 8px; padding: 15px; display: flex; flex-direction: column; align-items: center;",
                    tags$img(
                      src = ifelse(is.na(item$ItemImagePath), placeholder_150px_path, paste0(host_url, "/images/", basename(item$ItemImagePath))),
                      style = "width: 150px; height: 150px; object-fit: cover; border-radius: 8px; margin-bottom: 10px;"
                    ),
                    tags$p(tags$b("物品名："), item$ItemName, style = "margin: 5px 0;"),
                    tags$p(tags$b("SKU："), item$SKU, style = "margin: 5px 0;"),
                    numericInput(
                      paste0("purchase_qty_", item$SKU),
                      "请求数量",
                      value = 1,
                      min = 1,
                      width = "80%"
                    ),
                    textAreaInput(
                      paste0("purchase_remark_input_", item$SKU),
                      "留言（可选）",
                      placeholder = "输入留言...",
                      width = "100%",
                      rows = 2
                    ),
                    actionButton(
                      paste0("create_request_purchase_", item$SKU),
                      "发出采购请求",
                      class = "btn-primary",
                      style = "margin-top: 10px; width: 100%;"
                    )
                  )
                })
              )
            )
          )
        }
        
        if (length(outbound_items) > 0) {
          modal_content <- tagAppendChildren(
            modal_content,
            tags$div(
              style = "padding: 10px; background-color: #e6f7ff; border-radius: 8px; margin-bottom: 20px;",
              tags$h4("可从国内调货：", style = "color: blue; margin-bottom: 15px;"),
              tags$div(
                style = "display: grid; grid-template-columns: repeat(auto-fit, minmax(300px, 1fr)); gap: 20px;",
                lapply(outbound_items, function(item) {
                  div(
                    style = "background: white; box-shadow: 0 4px 8px rgba(0,0,0,0.1); border-radius: 8px; padding: 15px; display: flex; flex-direction: column; align-items: center;",
                    tags$img(
                      src = ifelse(is.na(item$ItemImagePath), placeholder_150px_path, paste0(host_url, "/images/", basename(item$ItemImagePath))),
                      style = "width: 150px; height: 150px; object-fit: cover; border-radius: 8px; margin-bottom: 10px;"
                    ),
                    tags$p(tags$b("物品名："), item$ItemName, style = "margin: 5px 0;"),
                    tags$p(tags$b("SKU："), item$SKU, style = "margin: 5px 0;"),
                    tags$div(
                      style = "width: 100%; display: flex; align-items: center; justify-content: center; gap: 10px;",
                      numericInput(
                        paste0("outbound_qty_", item$SKU),
                        "请求数量",
                        value = 1,
                        min = 1,
                        width = "50%"
                      ),
                      tags$span(
                        paste("国内库存数:", item$DomesticStock),
                        style = "font-size: 14px; color: grey;"
                      )
                    ),
                    # 添加留言输入框
                    textAreaInput(
                      paste0("outbound_remark_input_", item$SKU),
                      "留言（可选）",
                      placeholder = "输入留言...",
                      width = "100%",
                      rows = 2
                    ),
                    actionButton(
                      paste0("create_request_outbound_", item$SKU),
                      "发出调货请求",
                      class = "btn-primary",
                      style = "margin-top: 10px; width: 100%;"
                    )
                  )
                })
              )
            )
          )
        }

        showModal(modalDialog(
          title = "处理库存请求",
          div(style = "max-height: 650px; overflow-y: auto;", modal_content),
          easyClose = FALSE,
          footer = tagList(
            actionButton("complete_requests", "关闭", class = "btn-success")
          )
        ))
      }
      
      showNotification(
        paste0("订单已成功发货！订单号：", order$OrderID, "，共发货 ", nrow(items), " 件。"),
        type = "message"
      )
    }, error = function(e) {
      showNotification(paste("发货失败：", e$message), type = "error")
    })
    
    # 延迟 2 秒清空输入框
    shinyjs::delay(2000, {
      updateTextInput(session, "us_shipping_bill_number", value = "")
      updateTextInput(session, "us_shipping_sku_input", value = "")
      updateSelectInput(session, "us_shipping_platform", selected = "TikTok")
    })
    
    runjs("document.getElementById('us_shipping_bill_number').focus();") # 聚焦 SKU 输入框
    new_order_items(NULL)  # 清空物品列表
  })
  
  # 用于记录已绑定的请求按钮
  observed_request_buttons <- reactiveValues(registered = character())
  
  # 监听添加请求按钮
  observe({
    # 获取当前所有动态生成的按钮 ID
    request_buttons <- grep("^create_request_", names(input), value = TRUE)
    
    # 筛选出尚未绑定的按钮
    new_buttons <- setdiff(request_buttons, observed_request_buttons$registered)
    
    # 为每个新按钮动态创建监听
    lapply(new_buttons, function(button_id) {
      observeEvent(input[[button_id]], {
        # 确定按钮类型（出库或采购）
        if (grepl("outbound", button_id)) {
          # 出库请求处理逻辑
          sku <- sub("create_request_outbound_", "", button_id)  # 提取 SKU
          items <- outbound_stock_items()  # 从 reactiveVal 获取需要出库的物品
          item <- items[[which(sapply(items, function(x) x$SKU == sku))]]  # 找到匹配的物品
          
          # 获取请求数量
          qty <- input[[paste0("outbound_qty_", sku)]]
          domestic_stock <- item$DomesticStock  # 国内现存库存数
          
          # 获取留言
          remark <- input[[paste0("outbound_remark_input_", sku)]]
          remark_prefix <- if (system_type == "cn") "[京]" else "[圳]"
          new_remark <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), ": ", remark_prefix, " ", remark)
          
          request_id <- uuid::UUIDgenerate()
          
          tryCatch({
            # **出库请求：只出库实际有的库存**
            outbound_qty <- min(qty, domestic_stock)  # 出库数量不能超过现有库存
            if (outbound_qty > 0) {
              dbExecute(con,
                        "INSERT INTO requests (RequestID, SKU, Maker, ItemImagePath, ItemDescription, Quantity, RequestStatus, RequestType, CreatedAt, Remarks)
         VALUES (?, ?, ?, ?, ?, ?, '待处理', '出库', NOW(), ?)",
                        params = list(
                          request_id,
                          sku,
                          item$Maker,
                          item$ItemImagePath,
                          item$ItemName,
                          outbound_qty,
                          ifelse(remark == "", NA_character_, new_remark)
                        )
              )
              showNotification(paste0("已发出出库请求，SKU：", sku, "，数量：", outbound_qty), type = "message")
              
              # 动态更新按钮文本和样式
              updateActionButton(session, inputId = button_id, label = HTML("<i class='fa fa-check'></i> 出库请求已发送"))
              runjs(sprintf("$('#%s').removeClass('btn-primary').addClass('btn-success');", button_id))
              shinyjs::disable(button_id)
            }
            
            # **如果出库数量不足，超出的部分创建采购请求**
            purchase_qty <- max(0, qty - domestic_stock)  # 需要采购的数量
            if (purchase_qty > 0) {
              purchase_request_id <- uuid::UUIDgenerate()
              dbExecute(con,
                        "INSERT INTO requests (RequestID, SKU, Maker, ItemImagePath, ItemDescription, Quantity, RequestStatus, RequestType, CreatedAt, Remarks)
         VALUES (?, ?, ?, ?, ?, ?, '待处理', '采购', NOW(), ?)",
                        params = list(
                          purchase_request_id,
                          sku,
                          item$Maker,
                          item$ItemImagePath,
