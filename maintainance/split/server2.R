          easyClose = TRUE,
          footer = modalButton("关闭")
        ))
        
        # 清空 SKU 输入字段
        updateTextInput(session, "new_sku", value = "")
      } else {
        # 如果 SKU 不冲突，更新输入字段
        updateTextInput(session, "new_sku", value = sku)
        # showNotification("SKU 生成成功！", type = "message")
      }
    }
  })
  
  autocompleteInputServer("purchase", get_suggestions = item_names)  # 返回商品名列表
  
  output$preorder_items_memo <- renderUI({
    # 从 orders() 中筛选出 OrderStatus 为“预定”的订单
    preorder_orders <- orders() %>% filter(OrderStatus == "预定")
    
    # 初始化空的数据框，用于存储所有物品和供应商信息
    all_items <- data.frame(Item = character(0), Supplier = character(0), stringsAsFactors = FALSE)
    
    # 遍历每个订单的 OrderNotes，提取物品和供应商信息
    for (order_note in preorder_orders$OrderNotes) {
      extracted <- extract_items_and_suppliers(order_note)
      all_items <- rbind(all_items, extracted)
    }
    
    # 去除重复的物品-供应商组合
    all_items <- unique(all_items)
    
    # 移除空字符串
    all_items <- all_items[all_items$Item != "", ]
    
    # 获取当前库存商品名称
    existing_items <- unique(inventory()$ItemName)
    
    if (nrow(all_items) == 0) {
      div("当前没有预订单物品")
    } else {
      # 创建物品列表，判断是否存在于库存
      item_list <- lapply(seq_len(nrow(all_items)), function(i) {
        item <- all_items$Item[i]
        supplier <- all_items$Supplier[i]
        
        # 判断该物品是否在库存中
        is_existing <- item %in% existing_items
        status_label <- if (is_existing) {
          tags$span("现", class = "status-badge status-existing")
        } else {
          tags$span("新", class = "status-badge status-new")
        }
        
        # 根据物品类型设置不同的 `onclick` 逻辑
        if (is_existing) {
          # “现”物品：填充到 purchase_filter-name
          onclick_script <- sprintf(
            "Shiny.setInputValue('selected_existing_item', '%s', {priority: 'event'});", 
            item
          )
        } else {
          # “新”物品：填充到 new_maker 和 purchase-item_name
          onclick_script <- sprintf(
            "Shiny.setInputValue('selected_new_item', '%s', {priority: 'event'}); Shiny.setInputValue('selected_new_supplier', '%s', {priority: 'event'});", 
            item, supplier
          )
        }
        
        # 创建可点击的物品项
        actionLink(
          inputId = paste0("preorder_item_", i), 
          label = div(
            style = "padding: 5px 0; border-bottom: 1px solid #eee; display: flex; align-items: center; cursor: pointer;",
            tags$span(paste0(item, "（", supplier, "）"), style = "flex-grow: 1;"),
            status_label
          ),
          onclick = onclick_script
        )
      })
      
      # 返回 UI 组件
      do.call(tagList, item_list)
    }
  })
  
  # 监听“新”物品的点击事件，填充到 `new_maker` 和 `purchase-item_name`
  observeEvent(input$selected_new_item, {
    req(input$selected_new_item)
    
    updateTextInput(session, "purchase-item_name", value = input$selected_new_item)
    
    delay(50, {
      req(input$selected_new_supplier)  # 确保 `selected_new_supplier` 存在
      updateSelectizeInput(session, "new_maker", selected = input$selected_new_supplier)
    })
  })
  
  
  # 监听“现”物品的点击事件，填充到 `purchase_filter-name`
  observeEvent(input$selected_existing_item, {
    req(input$selected_existing_item)
    
    updateSelectizeInput(session, "purchase_filter-name", selected = input$selected_existing_item)
  })
  
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
  observeEvent(input$inventory_cn, {
    if (input$inventory_cn == "入库") {
      runjs("document.getElementById('inbound_sku').focus();")
    }
  })
  
  # 物品表过滤模块
  itemFilterServer(
    id = "inbound_filter",
    makers_items_map = makers_items_map
  )
  
  # 创建全局变量存储 预订单的 order_id 和 unique_id
  preorder_info <- reactiveValues(order_id = NULL, item_name = NULL, unique_id = NULL)
  
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
      host_url = host_url
    )
    
    # 如果启用自动入库功能，直接执行入库逻辑
    if (input$auto_inbound) {
      req(input$inbound_sku)
      result <- handleOperation(
        unique_items_data(),
        operation_name = "入库", 
        sku_field = "inbound_sku",
        output_name = "inbound_item_info",
        query_status = "采购",
        update_status_value = "国内入库",
        count_label = "待入库数", 
        count_field = "PendingQuantity", 
        refresh_trigger = NULL,    
        con,                  
        input, output, session
      )
      
      if (!is.null(result)) {
        preorder_info$item_name <- result$item_name
        preorder_info$unique_id <- result$unique_id  # 存储 unique_id
        
        if (input$speak_inbound_item_name) {  # 只有勾选“念出商品名”才朗读
          js_code <- sprintf('
            var msg = new SpeechSynthesisUtterance("%s");
            msg.lang = "zh-CN";
            window.speechSynthesis.speak(msg);
          ', preorder_info$item_name)
          
          shinyjs::runjs(js_code)  # 运行 JavaScript 语音朗读
        } else {
          runjs("playSuccessSound()")  # 播放成功音效
        }
        
        orders_data <- dbGetQuery(con, "SELECT OrderID, OrderImagePath, OrderNotes, created_at FROM orders WHERE OrderStatus = '预定'")
        
        # 处理预定物品数据
        orders_data <- orders_data %>%
          mutate(PreorderItems = stri_match_first_regex(OrderNotes, "【预定物品】(.*?)；")[,2]) %>%
          filter(!is.na(PreorderItems)) %>%
          mutate(ItemList = stri_split_fixed(PreorderItems, "，")) %>%
          select(OrderID, OrderImagePath, OrderNotes, created_at, ItemList) %>%
          tidyr::unnest(ItemList)
        
        # 查找完全匹配的预订单
        matched_order <- orders_data %>%
          filter(ItemList == preorder_info$item_name) %>%
          arrange(created_at) %>%
          slice_head(n = 1)
        
        if (nrow(matched_order) > 0) {
          preorder_info$order_id <- matched_order$OrderID[1]  # 存储 order_id
          order_img_path <- ifelse(
            is.na(matched_order$OrderImagePath[1]) || matched_order$OrderImagePath[1] == "",
            placeholder_300px_path,
            paste0(host_url, "/images/", basename(matched_order$OrderImagePath[1]))
          )
          order_notes <- matched_order$OrderNotes[1]
          
          # **确保 `preorder_info$item_name` 只匹配以 `，` 或 `；` 结尾的完整项**
          pattern <- paste0("(】|，)(", preorder_info$item_name, ")(，|；)")
          highlighted_notes <- gsub(pattern, paste0("\\1<mark>\\2</mark>\\3"), order_notes, perl = TRUE)
          
          # 弹出确认对话框
          showModal(modalDialog(
            title = "预订单匹配",
            div(
              # 提示导语，使用强调样式
              tags$p("该商品已被如下预订单预定，是否直接做售出操作？", style = "font-weight: bold; color: #d9534f; text-align: center;"),
              div(
                tags$img(src = order_img_path, height = "300px", style = "display: block; margin: 10px auto; border-radius: 8px;")
              ),
              div(
                tags$p(HTML(paste("<strong>订单号:</strong>", preorder_info$order_id)), style = "margin-top: 10px;"),
                tags$p(HTML(paste("<strong>备注:</strong>", highlighted_notes)), style = "white-space: pre-wrap;")
              ),
              style = "text-align: left;"
            ),
            footer = tagList(
              modalButton("取消"),
              actionButton("confirm_bind_preorder", "确认预定品售出", class = "btn btn-primary")
            ),
            easyClose = FALSE  # 防止用户误触关闭
          ))
        } 
      } else {
        runjs("playErrorSound()")  # 播放失败音效
        return()
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
  
  # 监听 预订单"确认登记" 按钮，**确保只绑定一次**
  observeEvent(input$confirm_bind_preorder, {
    req(preorder_info$order_id, preorder_info$unique_id)  # 确保数据有效
    removeModal()  # 关闭 `预订单匹配`
    
    # 获取当前订单备注
    current_notes <- dbGetQuery(con, paste0(
      "SELECT OrderNotes FROM orders WHERE OrderID = '", preorder_info$order_id, "'"
    ))$OrderNotes
    
    if (!is.null(current_notes) && nchar(current_notes) > 0) {
      # **删除 `OrderNotes` 里匹配的 `item_name`**
      updated_notes <- remove_preorder_item_note(current_notes, preorder_info$item_name)
        
      # **如果 `updated_notes` 仅剩 `"【预定物品】；"`，改为 `"【预定物品登记完毕】"`**
      updated_notes <- sub("【预定物品】；", "【预定物品登记完毕】", updated_notes, fixed = TRUE)
      
      # **更新 `OrderNotes`**
      dbExecute(con, 
                "UPDATE orders SET OrderNotes = ? WHERE OrderID = ?", 
                params = list(updated_notes, preorder_info$order_id)
      )
      
      orders_refresh_trigger(!orders_refresh_trigger())
    }
    
    # 更新该物品的 `OrderID` 并修改 `Status`
    dbExecute(con, paste0(
      "UPDATE unique_items SET OrderID = '", preorder_info$order_id, "', Status = '国内售出'
     WHERE UniqueID = '", preorder_info$unique_id, "'"
    ))
    showNotification(paste0("物品已成功登记到预定单 ", preorder_info$order_id, "！"), type = "message")
    updateTabsetPanel(session, "inventory_cn", selected = "售出") # 跳转到“发货”页面
    # **延迟执行，确保 UI 加载完成后再切换子分页**
    shinyjs::delay(300, {
      updateTabsetPanel(session, "sold_tabs", selected = "订单管理")
    })    
    updateTextInput(session, "filter_order_id", value = preorder_info$order_id)
  }, ignoreInit = TRUE)  # **确保 `observeEvent` 只执行一次**
  
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
      result <- handleOperation(
        unique_items_data(),
        operation_name = "入库", 
        sku_field = "inbound_sku",
        output_name = "inbound_item_info",
        query_status = "采购",
        update_status_value = "国内入库",
        count_label = "待入库数", 
        count_field = "PendingQuantity", 
        refresh_trigger = NULL,      
        con,                  
        input, output, session
      )
      
      # 如果未找到对应的 UniqueID，停止后续操作
      if (is.null(result)) {
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

    # 重置输入
    updateTextInput(session, "inbound_sku", value = "")
    updateNumericInput(session, "inbound_quantity", value = 1)
    runjs("document.getElementById('inbound_sku').focus();")
  })
  
  
  # # 监听选中行并更新 SKU: 禁用
  # observeEvent(unique_items_table_inbound_selected_row(), {
  #   selected_row <- unique_items_table_inbound_selected_row()
  #   if (length(selected_row) > 0) {
  #     # 仅处理最后一个选择的行
  #     last_selected <- tail(selected_row, 1) # 获取最后一个选择的行号
  #     selected_sku <- filtered_unique_items_data_inbound()[last_selected, "SKU", drop = TRUE]
  #     updateTextInput(session, "inbound_sku", value = selected_sku)
  #   }
  # })
  
  # 监听选中行并显示大图与物品信息
  observeEvent(unique_items_table_inbound_selected_row(), {
    selected_row <- unique_items_table_inbound_selected_row()
    if (length(selected_row) > 0) {
      # 仅处理最后一个选择的行
      last_selected <- tail(selected_row, 1) # 获取最后一个选择的行号
      selected_sku <- filtered_unique_items_data_inbound()[last_selected, "SKU", drop = TRUE]
      handleSkuInput(
        sku_input = selected_sku,
        output_name = "inbound_item_info",
        count_label = "待入库数",
        count_field = "PendingQuantity",
        con = con,
        output = output,
        placeholder_path = placeholder_300px_path,
        host_url = host_url
      )    
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
  observeEvent(input$inventory_cn, {
    if (input$inventory_cn == "出库") {
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
    req(input$outbound_sku)
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
    result <- handleOperation(
      unique_items_data(),
      operation_name = "出库", 
      sku_field = "outbound_sku",
      output_name = "outbound_item_info",
      query_status = "国内入库",
      update_status_value = "国内出库",
      count_label = "可出库数", 
      count_field = "AvailableForOutbound", 
      refresh_trigger = NULL,     
      con,                  
      input, output, session
    )

    if (!is.null(result)) {
      if (input$speak_outbound_item_name) {  # 只有勾选“念出商品名”才朗读
        js_code <- sprintf('
            var msg = new SpeechSynthesisUtterance("%s");
            msg.lang = "zh-CN";
            window.speechSynthesis.speak(msg);
          ', result$item_name)
        
        shinyjs::runjs(js_code)  # 运行 JavaScript 语音朗读
      } else {
        runjs("playSuccessSound()")  # 播放成功音效
      }
    } else {
      runjs("playErrorSound()")  # 播放失败音效
      return()
    }
    
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
      refresh_trigger = NULL,     
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
      refresh_trigger = NULL, 
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
  
  # 监听 sold_tabs 的变化，调整 filter_tabs
  observeEvent(input$sold_tabs, {
    if (input$sold_tabs == "物品售出") {
      updateTabsetPanel(session, inputId = "filter_tabs", selected = "物品筛选")
    } else if (input$sold_tabs == "订单管理") {
      updateTabsetPanel(session, inputId = "filter_tabs", selected = "订单筛选")
    }
  })
  
  # 手动刷新订单表
  observeEvent(input$refresh_orders, {
    orders_refresh_trigger(!orders_refresh_trigger()) # 触发 orders 数据刷新
    showNotification("订单数据已刷新！", type = "message")
  })
  
  ############################ 
  #####   物品售出子页   ##### 
  ############################ 
  
  itemFilterServer(
    id = "sold_filter",
    makers_items_map = makers_items_map
  )
  
  # 响应点击物品表的行，更新货架上的物品
  observeEvent(list(unique_items_table_sold_selected_row(), input$arrow_direction), {
    selected_row <- unique_items_table_sold_selected_row()
    sort_order <- input$arrow_direction
    
    if (is.null(selected_row) || length(selected_row) == 0) {
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
