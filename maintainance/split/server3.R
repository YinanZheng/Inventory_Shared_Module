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
      runjs("playErrorSound()")  
    })
  }, ignoreNULL = TRUE, ignoreInit = TRUE)  # **防止初始时触发**

  ##### 网名自动填写组件
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
    req(input$order_id)  # 确保订单号不为空
    
    tryCatch({
      # 去除空格和#号
      sanitized_order_id <- gsub("#", "", trimws(input$order_id))
      
      # 处理不同输入情况
      if (grepl("@$", sanitized_order_id)) {
        # **用户输入 `1234@`，提取 `1234` 作为主单号**
        main_order_id <- sub("@$", "", sanitized_order_id)
      } else if (grepl("@", sanitized_order_id)) {
        # **用户输入完整的 `1234@1`，提取 `1234` 作为前缀**
        main_order_id <- sub("@.*", "", sanitized_order_id)
      } else {
        # **用户输入 `1234`，直接使用**
        main_order_id <- sanitized_order_id
      }
      
      # 查询数据库，优先查找完整匹配的 `OrderID`（如 `1234@1`）
      existing_order <- orders() %>% filter(OrderID == sanitized_order_id)
      
      # **检查主订单 `1234` 是否存在**
      main_order_exists <- nrow(orders() %>% filter(OrderID == main_order_id)) > 0
      
      # **如果找不到 `1234@1`，但 `1234` 存在，则填充 `1234` 的信息**
      if (nrow(existing_order) == 0 && main_order_exists) {
        existing_order <- orders() %>% filter(OrderID == main_order_id)
      }
      
      if (nrow(existing_order) > 0) {
        # 填充各字段信息
        updateSelectInput(session, "platform", selected = existing_order$Platform[1])
        updateNumericInput(session, "transaction_amount", value = existing_order$TransactionAmount[1])
        updateTextInput(session, "customer_name", value = existing_order$CustomerName[1])
        updateTextInput(session, "tracking_number", value = existing_order$UsTrackingNumber[1])
        updateTextAreaInput(session, "order_notes", value = existing_order$OrderNotes[1])
        
        # **检查 LabelStatus**
        if (existing_order$LabelStatus[1] != "无") {
          shinyjs::disable("tracking_number")  # 禁用输入框
        } else {
          shinyjs::enable("tracking_number")  # 启用输入框
        }
        
        # **处理 `调货` 和 `预定` 订单状态**
        if (!is.null(existing_order$OrderStatus[1]) && !is.na(existing_order$OrderStatus[1])) {
          if (existing_order$OrderStatus[1] == "调货") {
            updateCheckboxInput(session, "is_transfer_order", value = TRUE)
          } else {
            updateCheckboxInput(session, "is_transfer_order", value = FALSE)
          }
          
          if (existing_order$OrderStatus[1] == "预定") {
            updateCheckboxInput(session, "is_preorder", value = TRUE)
            updateSelectizeInput(session, "preorder_supplier", selected = character(0))
            updateTextAreaInput(session, "preorder_item_name", value = "")
            
            # **从 `OrderNotes` 提取预定供应商和物品信息**
            if (!is.null(existing_order$OrderNotes[1]) && !is.na(existing_order$OrderNotes[1])) {
              extracted <- extract_items_and_suppliers(existing_order$OrderNotes[1])
              if (nrow(extracted) > 0) {
                unique_suppliers <- unique(extracted$Supplier)
                if (length(unique_suppliers) > 0) {
                  updateSelectizeInput(session, "preorder_supplier", selected = unique_suppliers[1])
                }
                updateTextAreaInput(session, "preorder_item_name", value = paste(extracted$Item, collapse = "\n"))
              }
            }
          } else {
            updateCheckboxInput(session, "is_preorder", value = FALSE)
            updateSelectizeInput(session, "preorder_supplier", selected = character(0))
            updateTextAreaInput(session, "preorder_item_name", value = "")
          }
        } else {
          updateCheckboxInput(session, "is_transfer_order", value = FALSE)
          updateCheckboxInput(session, "is_preorder", value = FALSE)
          updateSelectizeInput(session, "preorder_supplier", selected = character(0))
          updateTextAreaInput(session, "preorder_item_name", value = "")
        }
        
        # **判断按钮逻辑**
        if (nrow(existing_order) > 0 && sanitized_order_id == main_order_id) {
          # **情况 1：当前输入的订单号已存在（如 `1234` 存在） → 显示“更新订单”**
          output$register_order_button_ui <- renderUI({
            actionButton(
              "register_order_btn",
              "更新订单",
              icon = icon("edit"),
              class = "btn-success",
              style = "font-size: 16px; min-width: 130px; height: 42px;"
            )
          })
        } else {
          # **情况 2：当前输入的是 `1234@1`，但 `1234@1` 不存在，且 `1234` 存在 → 显示“登记订单”**
          showNotification("主订单已存在，正在创建子订单", type = "warning")
          updateTextAreaInput(session, "order_notes", value = "")
          updateTextAreaInput(session, "preorder_item_name", value = "")
          
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
      } else {
        # **情况 3：订单 `1234` 和 `1234@1` 都不存在，用户创建新订单**
        showNotification("未找到对应订单记录，可登记新订单", type = "warning")
        
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
  
  # 动态填充供应商与商品名选择器
  observe({
    update_maker_choices(session, "preorder_supplier", maker_list())
    updateSelectizeInput(session, "preorder_item_name_db", choices = c("", inventory()$ItemName), selected = NULL, server = TRUE)
  })
  
  # 控制预订单显示
  observeEvent(input$is_preorder, {
    toggle(id = "preorder_fields", condition = input$is_preorder)
  })
  
  # 监听用户在 preorder_item_name_db 中的选择，并更新到 preorder_item_name
  observeEvent(input$preorder_item_name_db, {
    selected_item <- input$preorder_item_name_db
    
    if (!is.null(selected_item) && selected_item != "") {
      existing_text <- input$preorder_item_name
      existing_items <- unlist(strsplit(existing_text, "\n"))
      
      # 将新选定的物品添加到文本框
      new_text <- paste(existing_text, selected_item, sep = ifelse(existing_text == "", "", "\n"))
      updateTextAreaInput(session, "preorder_item_name", value = new_text)
      
      # 从 inventory() 获取商品图片路径
      selected_inventory <- inventory() %>% filter(ItemName == selected_item)
      
      if (nrow(selected_inventory) > 0) {
        img_path <- ifelse(is.na(selected_inventory$ItemImagePath) || selected_inventory$ItemImagePath == "",
                           placeholder_150px_path,
                           paste0(host_url, "/images/", basename(selected_inventory$ItemImagePath)))
        
        runjs(sprintf("$('#preorder_img').attr('src', '%s').show();", img_path))
      } else {
        runjs("$('#preorder_img').hide();")
      }
    }
  })
  
  # 登记、更新订单逻辑
  observeEvent(input$register_order_btn, {
    if (is.null(input$order_id) || input$order_id == "") {
      showNotification("订单号不能为空！", type = "error")
      runjs("playErrorSound()")
      return()
    }
    
    if (is.null(input$platform) || input$platform == "") {
      showNotification("电商平台不能为空，请选择一个平台！", type = "error")
      runjs("playErrorSound()")
      return()
    }
    
    if (is.null(input$transaction_amount) || is.na(input$transaction_amount)) {
      showNotification("总成交额不能为空！", type = "error")
      runjs("playErrorSound()")
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
      transaction_amount = input$transaction_amount,
      order_notes = input$order_notes,
      tracking_number = input$tracking_number,
      image_data = image_sold,
      con = con,
      orders = orders,
      box_items = box_items,
      unique_items_data = unique_items_data,
      is_transfer_order = input$is_transfer_order,
      is_preorder = input$is_preorder,
      preorder_supplier = input$preorder_supplier,
      preorder_item_name = input$preorder_item_name
    )
    
    # 如果订单登记失败，直接退出
    if (!order_registered) {
      runjs("playErrorSound()")
      return()
    } else {
      runjs("playSuccessSound()")
    }
    
    orders_refresh_trigger(!orders_refresh_trigger())
  })
  
  # 清空订单信息按钮
  observeEvent(input$clear_order_btn, {
    selected_order_id(NULL)
    associated_items(NULL)
    
    # 重置订单填写表
    reset_order_form(session, image_sold)
    
    # 重置库存商品名列表
    updateSelectizeInput(session, "preorder_item_name_db", choices = c("", inventory()$ItemName), selected = NULL, server = TRUE)
    
    # 清空订单关联物品表
    output$associated_items_title <- renderDT({ NULL }) # 清空标题
    renderOrderItems(output, "order_items_cards", data.frame(), con)  # 清空物品卡片
  })

  # 订单合并
  observeEvent(input$merge_order_btn, {
    tryCatch({
      # 获取用户选中的订单号
      selected_order <- filtered_orders()[selected_order_row(), ]
      selected_order_id <- selected_order$OrderID
      
      if (is.null(selected_order_id) || length(selected_order_id) != 1) {
        showNotification("请选择一个订单进行合并！", type = "error")
        return()
      }
      
      # 判断选中的订单是否包含 '@'，如果没有 '@'，则其本身就是主单
      main_order_id <- ifelse(grepl("@", selected_order_id), sub("@.*", "", selected_order_id), selected_order_id)
      
      # 获取可能的子单，包括主单本身和所有 `@` 子单
      possible_sub_orders <- orders() %>%
        filter(grepl(paste0("^", main_order_id, "(@\\d+)?$"), OrderID))
      
      # 如果只找到 **1 个** 订单，且它本身就是主单（无 `@`），则不能合并
      if (nrow(possible_sub_orders) == 1 && !grepl("@", selected_order_id)) {
        showNotification("当前订单未找到可合并的子单！", type = "error")
        return()
      }
      
      # 获取所有子单的订单状态、运单号和平台信息
      order_statuses <- unique(possible_sub_orders$OrderStatus)
      tracking_numbers <- unique(possible_sub_orders$UsTrackingNumber)
      platforms <- unique(possible_sub_orders$Platform)
      
      # 检查订单状态、运单号和平台是否满足合并条件
      if (!all(order_statuses == "备货") || length(tracking_numbers) > 1 || length(platforms) > 1) {
        showNotification("子单的订单状态必须全部为 '备货'，运单号和平台必须一致才可合并！", type = "error")
        return()
      }
      
      # 获取子单的所有物品
      sub_items <- unique_items_data() %>%
        filter(OrderID %in% possible_sub_orders$OrderID)
      
      # 处理子单物品图片路径拼接
      image_paths <- unique(sub_items$ItemImagePath[!is.na(sub_items$ItemImagePath)])
      merged_image_path <- if (length(image_paths) > 0) {
        montage_path <- paste0("/var/www/images/", main_order_id, "_montage_", format(Sys.time(), "%Y%m%d%H%M%S"), ".jpg")
        generate_montage(image_paths, montage_path)
      } else {
        NA
      }
      
      # 获取最早的 `created_at` 时间
      earliest_created_at <- min(possible_sub_orders$created_at, na.rm = TRUE)
      
      # **先删除所有子单（包括可能存在的主单）**
      dbExecute(con, sprintf(
        "DELETE FROM orders WHERE OrderID IN (%s)",
        paste(shQuote(possible_sub_orders$OrderID), collapse = ", ")
      ))
      
      # **插入合并后的主订单**
      merged_order <- tibble(
        OrderID = main_order_id,
        Platform = platforms[1],
        TransactionAmount = max(possible_sub_orders$TransactionAmount),
        UsTrackingNumber = tracking_numbers[1],
        CustomerName = ifelse(length(unique(possible_sub_orders$CustomerName)) > 0,
                              paste(unique(possible_sub_orders$CustomerName), collapse = ", "), NA),
        CustomerNetName = ifelse(length(unique(possible_sub_orders$CustomerNetName)) > 0,
                                 paste(unique(possible_sub_orders$CustomerNetName), collapse = ", "), NA),
        OrderImagePath = merged_image_path,  # 合并图片路径
        OrderNotes = ifelse(length(unique(possible_sub_orders$OrderNotes)) > 0,
                            paste(unique(possible_sub_orders$OrderNotes), collapse = " | "), NA),
        OrderStatus = "备货",
        created_at = earliest_created_at,  # 使用子单中最早的创建时间
        updated_at = Sys.time()
      )
      
      dbWriteTable(
        con, "orders", merged_order,
        append = TRUE, overwrite = FALSE
      )
      
      # 更新子单物品的订单号为主单号
      update_order_id(con, sub_items$UniqueID, main_order_id)
      
      showNotification(paste("订单合并成功！主单号为：", main_order_id, ", 共计", nrow(sub_items), "件物品"), type = "message")
      
      # 更新数据并触发 UI 刷新
      orders_refresh_trigger(!orders_refresh_trigger())
      
    }, error = function(e) {
      showNotification(paste("合并订单时发生错误：", e$message), type = "error")
    })
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
                                                    option = modifyList(table_default_options, list(scrollY = "278px"))
                                                    )
    
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
                                                    option = modifyList(table_default_options, list(scrollY = "220px"))
    )
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
        runjs("playSuccessSound()")
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
      runjs("playSuccessSound()")
    }
  })
  
  updateBox <- function(selected_item, selected_row, shelf_data) {
    # 更新箱子内容
    current_box <- box_items()
    box_items(bind_rows(selected_item, current_box))
    
    # 更新货架上的物品，移除已选的
    updated_shelf <- shelf_data[-selected_row, ]
    shelf_items(updated_shelf)
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
    }
  })
  
  # 扫码上架功能
  observeEvent(input$sku_to_shelf, {
    req(input$sku_to_shelf)  # 确保输入框不为空
    
    tryCatch({
      # 获取输入的 SKU
      scanned_sku <- trimws(input$sku_to_shelf)
      
      if (is.null(scanned_sku) || scanned_sku == "") {
        showNotification("请输入有效的 SKU！", type = "error")
        runjs("playErrorSound()")
        return()
      }
      
      # 从 unique_items_data 获取货架中符合条件的物品
      all_shelf_items <- get_shelf_items(data = unique_items_data(), sku = scanned_sku)
      
      # 如果货架中没有符合条件的物品，提示错误
      if (is.null(all_shelf_items)) {
        showNotification("货架上未找到对应 SKU 的物品！", type = "error")
        runjs("playErrorSound()")
        updateTextInput(session, "sku_to_shelf", value = "")  # 清空输入框
        return()
      }
      
      # 从箱子中获取当前 SKU 的已选数量
      box_data <- box_items()

      # 更新货架上的物品
      updated_shelf <- all_shelf_items[!all_shelf_items$UniqueID %in% box_data$UniqueID, ]
      shelf_items(updated_shelf)
      
      # 通知用户
      showNotification(paste("物品已上货架！SKU:", scanned_sku), type = "message")
      runjs("playSuccessSound()")
    }, error = function(e) {
      # 捕获错误并通知用户
      showNotification(paste("处理 SKU 时发生错误：", e$message), type = "error")
      runjs("playErrorSound()")
    })
    
    # 清空输入框
    updateTextInput(session, "sku_to_shelf", value = "")
  })
  
  # 扫码入箱功能
  observeEvent(input$sku_to_box, {
    req(input$sku_to_box)  # 确保输入框不为空
    
    tryCatch({
      # 获取输入的 SKU
      scanned_sku <- trimws(input$sku_to_box)
      
      if (is.null(scanned_sku) || scanned_sku == "") {
        showNotification("请输入有效的 SKU！", type = "error")
        runjs("playErrorSound()")
        return()
      }
      
      # 从 unique_items_data 获取货架中符合条件的物品
      all_shelf_items <- get_shelf_items(data = unique_items_data(), sku = scanned_sku)
      
      if (is.null(all_shelf_items)) {
        showNotification("货架上未找到对应 SKU 的物品！", type = "error")
        runjs("playErrorSound()")
        updateTextInput(session, "sku_to_box", value = "")  # 清空输入框
        return()
      }
      
      # 检查是否为 "美国入库" 状态且仅剩一件
      us_stock_count <- sum(all_shelf_items$Status == "美国入库")
      
      if (any(all_shelf_items$Status == "美国入库") && us_stock_count <= 1) {
        # 弹出模态框，提醒用户核实后再操作
        showModal(modalDialog(
          title = "注意",
          p("此商品在美国库存仅剩一件，请沟通核实后再进行调货"),
          footer = tagList(
            actionButton("verify_and_proceed_auto", "已核实, 继续调货", class = "btn-primary"),
            modalButton("取消")
          ),
          easyClose = FALSE
        ))
        
        # 监听 "已核实" 按钮事件，确认操作
        observeEvent(input$verify_and_proceed_auto, {
          removeModal()  # 关闭模态框
          process_box_addition(scanned_sku, all_shelf_items)  # 继续处理移入箱子操作
        })
        
        # 清空输入框
        updateTextInput(session, "sku_to_box", value = "")  # 清空输入框
        return()
      }
      
      # 如果不需要弹窗，直接处理入箱
      process_box_addition(scanned_sku, all_shelf_items)
    }, error = function(e) {
      # 捕获错误并通知用户
      showNotification(paste("处理 SKU 时发生错误：", e$message), type = "error")
      runjs("playErrorSound()")
    })
    
    # 清空输入框
    updateTextInput(session, "sku_to_box", value = "")
  })
  
  # 定义移入箱子的逻辑
  process_box_addition <- function(scanned_sku, all_shelf_items) {
    # 从箱子中获取当前 SKU 的已选数量
    box_data <- box_items()
    box_sku_count <- sum(box_data$SKU == scanned_sku)
    
    # 如果箱子中物品数量 >= 货架中物品总量，则阻止操作
    if (box_sku_count >= nrow(all_shelf_items)) {
      showNotification("该 SKU 的所有物品已移入箱子，无法继续添加！", type = "error")
      runjs("playErrorSound()")
      return()
    }
    
    # 获取优先级最高的物品
    selected_item <- all_shelf_items[box_sku_count + 1, ]
    
    # 更新箱子内容
    current_box <- box_items()
    box_items(bind_rows(selected_item, current_box))
    
    # 更新货架上的物品
    updated_shelf <- all_shelf_items[!all_shelf_items$UniqueID %in% box_items()$UniqueID, ]
    shelf_items(updated_shelf)
    runjs("playSuccessSound()")
  }
  
  zero_stock_items <- reactiveVal(list())  # 用于存储国内库存为零的物品
  
  # 确认售出
  observeEvent(input$confirm_order_btn, {
    req(input$order_id)
    
    tryCatch({
      
      if (nrow(box_items()) == 0) {
        showNotification("箱子内容不能为空！", type = "error")
        runjs("playErrorSound()")
        return()
      }
      
      if (is.null(input$platform) || input$platform == "") {
        showNotification("电商平台不能为空，请选择一个平台！", type = "error")
        runjs("playErrorSound()")
        return()
      }
      
      # 去除空格和#号
      sanitized_order_id <- gsub("#", "", trimws(input$order_id))
      
      # 确保订单已登记
      order_registered <- register_order(
        order_id = sanitized_order_id,
        customer_name = input$customer_name,
        customer_netname = input$customer_netname,
        platform = input$platform,
        transaction_amount = input$transaction_amount,
        order_notes = input$order_notes,
        tracking_number = input$tracking_number,
        image_data = image_sold,
        con = con,
        orders = orders,
        box_items = box_items,
        unique_items_data = unique_items_data,
        is_transfer_order = input$is_transfer_order,
        is_preorder = input$is_preorder,
        preorder_supplier = input$preorder_supplier,
        preorder_item_name = input$preorder_item_name
      )
      
      # 如果订单登记失败，直接退出
      if (!order_registered) {
        runjs("playErrorSound()")
        return()
      }
      
      orders_refresh_trigger(!orders_refresh_trigger())
      
      # 遍历箱子内物品，减库存并更新物品状态
      lapply(1:nrow(box_items()), function(i) {
        item <- box_items()[i, ]
        sku <- item$SKU
        
        # 根据当前状态决定新的状态
        current_status <- item$Status
        new_status <- ifelse(
          current_status %in% c("美国入库", "国内出库"), "美国调货",
          ifelse(current_status == "国内入库", "国内售出", NA)
        )
        
        # 更新 unique_items 表中的状态
        update_status(
          con = con,
          unique_id = item$UniqueID,
          new_status = new_status,
          shipping_method = if (new_status == "国内售出") input$sold_shipping_method else NULL,
          refresh_trigger = NULL
        )
        
        # 更新订单号
        update_order_id(
          con = con,
          unique_id = item$UniqueID,
          order_id = sanitized_order_id
        )
      }) # end of lapply
      
      # 检查库存并记录库存为零的物品
      zero_items <- list()  # 临时列表存储库存为零的物品
      
      for (sku in unique(box_items()$SKU)) {
        # 检查库存
        result <- unique_items_data() %>%
          filter(SKU == sku) %>%
          group_by(SKU, ItemName, ItemImagePath, Maker) %>%
          summarise(
            DomesticStock = sum(Status == "国内入库", na.rm = TRUE),
            .groups = "drop"
          )
        
        if (result$DomesticStock == 0) {
          zero_items <- append(zero_items, list(result))
        }
      }
      
      # 更新 zero_stock_items
      zero_stock_items(zero_items)
      
      # 弹出模态框提示补货和出库请求
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
        showModal(modalDialog(
          title = "处理库存请求",
          div(style = "max-height: 650px; overflow-y: auto;", modal_content),
          easyClose = FALSE,
          footer = tagList(
            actionButton("complete_requests", "关闭", class = "btn-success")
          )
        ))
      }

      # 清空箱子
      box_items(create_empty_shelf_box())
      
      # 重置所有输入框
      reset_order_form(session, image_sold)
      
      # 重置库存商品名列表
      updateSelectizeInput(session, "preorder_item_name_db", choices = c("", inventory()$ItemName), selected = NULL, server = TRUE)
      
      showNotification("订单已完成售出并更新状态！", type = "message")
      runjs("playSuccessSound()")
    }, error = function(e) {
      showNotification(paste("操作失败：", e$message), type = "error")
      runjs("playErrorSound()")
    })
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
        if (grepl("purchase", button_id)) {
          # 采购请求处理逻辑
          sku <- sub("create_request_purchase_", "", button_id)  # 提取 SKU
          items <- zero_stock_items()  # 从 reactiveVal 获取库存为零的物品
          item <- items[[which(sapply(items, function(x) x$SKU == sku))]]  # 找到匹配的物品
          
          # 获取请求数量
          qty <- input[[paste0("purchase_qty_", sku)]]
          request_id <- uuid::UUIDgenerate()
          
          tryCatch({
            # 插入采购请求到数据库
            dbExecute(con,
                      "INSERT INTO requests (RequestID, SKU, Maker, ItemImagePath, ItemDescription, Quantity, RequestStatus, Remarks, RequestType)
                     VALUES (?, ?, ?, ?, ?, ?, '待处理', ?, '采购')",
                      params = list(
                        request_id,
                        sku,
                        item$Maker,
                        item$ItemImagePath,
                        item$ItemName,
                        qty,
                        format_remark(input[[paste0("purchase_remark_input_", sku)]], system_type)
                      ))
            
            # 绑定按钮
            bind_buttons(request_id, requests_data(), input, output, session, con)
            
            # 动态更新按钮文本和样式
            updateActionButton(session, inputId = button_id, label = HTML("<i class='fa fa-check'></i> 采购请求已发送"))
            runjs(sprintf("$('#%s').removeClass('btn-primary').addClass('btn-success');", button_id))
            shinyjs::disable(button_id)
            
            # 提示成功消息
            showNotification(paste0("已发出采购请求，SKU：", sku, "，数量：", qty), type = "message")
          }, error = function(e) {
            # 提示错误消息
            showNotification(paste("发出采购请求失败：", e$message), type = "error")
          })
        }
      }, ignoreInit = TRUE)  # 忽略初始绑定时的触发
    })
    
    # 更新已注册的按钮 ID
    observed_request_buttons$registered <- union(observed_request_buttons$registered, new_buttons)
  })
  
  # 监听 "完成请求" 按钮事件
  observeEvent(input$complete_requests, {
    zero_stock_items(list())        # 清空补货物品列表
    removeModal()                   # 关闭模态框
  })
  
  ############################ 
  #####   订单管理子页   ##### 
  ############################ 
  
  # 订单关联物品容器
  associated_items <- reactiveVal(NULL)
  
  # 用于存储当前选中的订单ID
  selected_order_id <- reactiveVal(NULL)  
  
  # 商品名自动联想
  autocompleteInputServer("sold", get_suggestions = item_names)  # 返回商品名列表
  
  # 监听订单选择事件
  observeEvent(selected_order_row(), {
    selected_row <- selected_order_row()
    
    # 如果用户选择了订单，获取选中的订单数据
    selected_order <- filtered_orders()[selected_row, ]
    order_id <- selected_order$OrderID
    customer_name <- selected_order$CustomerName
    order_status <- selected_order$OrderStatus
    us_tracking_number <- selected_order$UsTrackingNumber
