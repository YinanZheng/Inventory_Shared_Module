      
      if (is.null(scanned_sku) || scanned_sku == "") {
        showNotification("请输入有效的 SKU！", type = "error")
        return()
      }
      
      # 从 unique_items_data 获取货架中符合条件的物品
      all_shelf_items <- get_shelf_items(data = unique_items_data(), sku = scanned_sku)
      
      # 如果货架中没有符合条件的物品，提示错误
      if (is.null(all_shelf_items)) {
        showNotification("货架上未找到对应 SKU 的物品！", type = "error")
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
      
    }, error = function(e) {
      # 捕获错误并通知用户
      showNotification(paste("处理 SKU 时发生错误：", e$message), type = "error")
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
        return()
      }
      
      # 从 unique_items_data 获取货架中符合条件的物品
      all_shelf_items <- get_shelf_items(data = unique_items_data(), sku = scanned_sku)
      
      if (is.null(all_shelf_items)) {
        showNotification("货架上未找到对应 SKU 的物品！", type = "error")
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
            actionButton("verify_and_proceed", "已核实, 继续调货", class = "btn-primary"),
            modalButton("取消")
          ),
          easyClose = FALSE
        ))
        
        # 监听 "已核实" 按钮事件，确认操作
        observeEvent(input$verify_and_proceed, {
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
    
    # 通知用户
    showNotification(paste("物品已移入箱子！SKU:", scanned_sku), type = "message")
  }
  
  # 确认售出
  observeEvent(input$confirm_order_btn, {
    req(input$order_id)
    
    tryCatch({
      
      if (nrow(box_items()) == 0) {
        showNotification("箱子内容不能为空！", type = "error")
        return()
      }
      
      if (is.null(input$platform) || input$platform == "") {
        showNotification("电商平台不能为空，请选择一个平台！", type = "error")
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
      
      # 遍历箱子内物品，减库存并更新物品状态
      lapply(1:nrow(box_items()), function(i) {
        item <- box_items()[i, ]
        sku <- item$SKU
        
        # 调整库存：减少数量
        adjust_inventory_quantity(con, sku, adjustment = -1)  # 减少 1 的库存数量
        
        # 根据当前状态决定新的状态
        current_status <- item$Status
        new_status <- ifelse(
          current_status %in% c("美国入库", "国内出库"), "美国调货",
          ifelse(current_status == "国内入库", "国内售出", NA)
        )
        
        if (is.na(new_status)) {
          showNotification(paste("无法确定 SKU", sku, "的目标状态，操作已终止！"), type = "error")
        }
        
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
      
      # 更新数据并触发 UI 刷新
      unique_items_data_refresh_trigger(!unique_items_data_refresh_trigger())
      
      showNotification("订单已完成售出并更新状态！", type = "message")
      
      # 清空箱子
      box_items(create_empty_shelf_box())
      
      # 重置所有输入框
      reset_order_form(session, image_sold)
      
    }, error = function(e) {
      showNotification(paste("操作失败：", e$message), type = "error")
    })
  })
  
  ############################ 
  #####   订单管理子页   ##### 
  ############################ 
  
  # 订单关联物品容器
  associated_items <- reactiveVal()
  
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
    
    label_pdf_file_path(file.path("/var/uploads/shiplabels", paste0(us_tracking_number, ".pdf")))
 
    # 填充左侧订单信息栏
    updateTextInput(session, "order_id", value = order_id)
    
    # 动态更新标题
    output$associated_items_title <- renderUI({
      # 获取相关物品和状态
      items <- associated_items() 
      
      # 如果 items 为空，显示默认标题
      if (is.null(items) || nrow(items) == 0) {
        return(div(
          style = "display: flex; align-items: center; justify-content: space-between;",
          tags$h4(
            sprintf("#%s - %s 的订单物品（无相关物品）", order_id, customer_name),
            style = "color: #007BFF; font-weight: bold; margin: 0;"
          ),
          if(selected_order$LabelStatus != "无") {
            downloadButton("download_pdf_manage", label = "下载运单", class = "btn btn-primary", 
                           style = "height: 34px; margin-left: 10px; font-size: 14px; padding: 5px 10px;")
          }
        ))
      }
      
      # 检查是否所有物品状态为“美国发货”
      all_us_shipping <- all(items$Status == "美国发货")
      
      # 如果所有物品都是美国发货，找到最晚的发货时间
      latest_shipping_time <- if (all_us_shipping) {
        max(items$UsShippingTime, na.rm = TRUE)  # 获取最晚的发货时间
      } else {
        NULL
      }
      
      div(
        style = "display: flex; align-items: center; justify-content: space-between;",
        
        # 左侧标题
        tags$h4(
          sprintf("#%s - %s 的订单物品%s",
                  order_id,
                  customer_name,
                  if (!is.null(latest_shipping_time)) {
                    sprintf("（发货日期：%s）", latest_shipping_time)
                  } else {
                    ""
                  }),
          style = "color: #007BFF; font-weight: bold; margin: 0;"
        ),
        
        # 右侧按钮（仅在订单状态为“预定”时显示）
        if (order_status == "预定") {
          actionButton(
            inputId = "complete_preorder",
            label = "已完成预定",
            class = "btn-success",
            style = "margin-left: auto; font-size: 14px; padding: 5px 10px;"
          )
        },
        
        if(selected_order$LabelStatus != "无") {
          downloadButton("download_pdf_manage", label = "下载运单", class = "btn btn-primary", 
                         style = "height: 34px; margin-left: 10px; font-size: 14px; padding: 5px 10px;")
        }
      )
    })
    
    # 更新关联物品数据
    associated_items <- associated_items(unique_items_data() %>% filter(OrderID == order_id))
  })
  
  # 定义运单下载处理器
  output$download_pdf_manage <- downloadHandler(
    filename = function() {
      basename(label_pdf_file_path())
    },
    content = function(file) {
      file.copy(label_pdf_file_path(), file, overwrite = TRUE)
    }
  )
  
  observeEvent(input$complete_preorder, {
    req(selected_order_row())
    
    # 获取选中订单
    selected_row <- selected_order_row()
    selected_order <- filtered_orders()[selected_row, ]
    order_id <- selected_order$OrderID
    existing_notes <- selected_order$OrderNotes %||% ""  # 若为空，则默认空字符串
    
    # 检查 associated_items 是否为空
    associated_items_data <- associated_items()
    if (is.null(associated_items_data) || nrow(associated_items_data) == 0) {
      showNotification("无法完成预定：订单中未找到关联物品！", type = "error")
      return()  # 提前退出，避免后续逻辑执行
    }
    
    # 在 R 中拼接备注内容
    new_notes <- paste(existing_notes, sprintf("【预定完成 %s】", format(Sys.Date(), "%Y-%m-%d")))
    
    tryCatch({
      # 使用拼接后的备注信息进行 SQL 更新
      dbExecute(con, "
      UPDATE orders
      SET OrderStatus = '备货',
          OrderNotes = ?
      WHERE OrderID = ?
    ", params = list(new_notes, order_id))
      
      # 重新加载最新的 orders 数据
      orders_refresh_trigger(!orders_refresh_trigger())
      
      # 通知用户操作成功
      showNotification(sprintf("订单 #%s 已更新为备货状态！", order_id), type = "message")
      
    }, error = function(e) {
      # 捕获错误并通知用户
      showNotification(sprintf("更新订单状态时发生错误：%s", e$message), type = "error")
    })
  })
  
  # 渲染物品信息卡片  
  observe({
    req(associated_items())
    if (nrow(associated_items()) == 0) {
      renderOrderItems(output, "order_items_cards", data.frame())  # 清空物品卡片
      return()
    }
    renderOrderItems(output, "order_items_cards", associated_items(), deletable = TRUE)
  })

  # 订单物品删除逻辑
  observeEvent(input$delete_card, {
    req(input$delete_card, associated_items())  # 确保输入和物品列表存在
    
    # 当前物品列表
    current_items <- associated_items()

    # 移除对应的物品
    deleted_item <- current_items %>% filter(UniqueID == input$delete_card)
    updated_items <- current_items %>% filter(UniqueID != input$delete_card)
    associated_items(updated_items)  # 更新物品列表

    # 查询物品原始状态
    original_state <- dbGetQuery(con, paste0(
      "SELECT * FROM item_status_history WHERE UniqueID = '", deleted_item$UniqueID, "' ORDER BY change_time DESC LIMIT 1"
    ))
    
    if (nrow(original_state) > 0) {
      # 恢复物品状态到原始状态
      update_status(
        con = con,
        unique_id = deleted_item$UniqueID,
        new_status = original_state$previous_status,
        clear_status_timestamp = deleted_item$Status
      )
    } else {
      showModal(modalDialog(
        title = "错误",
        paste0("未找到物品 (SKU: ", deleted_item$SKU, ") 之前的库存状态记录，请联系管理员手动更改物品库存状态"),
        footer = modalButton("关闭"),
        easyClose = TRUE
      ))    
    }
    
    # 恢复库存数量
    adjust_inventory_quantity(con, deleted_item$SKU, adjustment = 1)  # 增加库存数量
    
    # 清空物品的 OrderID
    update_order_id(
      con = con,
      unique_id = deleted_item$UniqueID,
      order_id = NULL  # 清空订单号
    )
    
    # 提示删除成功
    showNotification("物品已删除, 库存已归还。", type = "message")
    
    # 更新数据并触发 UI 刷新
    unique_items_data_refresh_trigger(!unique_items_data_refresh_trigger())
    orders_refresh_trigger(!orders_refresh_trigger())
  })
  
  # 清空筛选条件逻辑
  observeEvent(input$reset_filter_btn, {
    tryCatch({
      # 重置所有输入框和选择框
      updateTextInput(session, "filter_order_id", value = "")
      updateTextInput(session, "filter_tracking_id", value = "")
      updateTextInput(session, "filter_customer_name", value = "")
      updateTextInput(session, "filter_customer_netname", value = "")
      updateSelectInput(session, "filter_platform", selected = "")
      updateSelectInput(session, "filter_order_status", selected = "")
      updateTextInput(session, "filter_sku", value = "")
      updateTextInput(session, "sold-item_name", value = "")
      
      # 显示成功通知
      showNotification("筛选条件已清空！", type = "message")
    }, error = function(e) {
      # 捕获错误并显示通知
      showNotification(paste("清空筛选条件时发生错误：", e$message), type = "error")
    })
  })
  
  # 删除订单逻辑
  observeEvent(input$delete_order_btn, {
    req(selected_order_row())  # 确保用户选择了一行订单
    selected_row <- selected_order_row()
    
    # 获取选中的订单数据
    selected_order <- filtered_orders()[selected_row, ]
    order_id <- selected_order$OrderID
    
    # 显示确认弹窗
    showModal(
      modalDialog(
        title = "确认删除订单",
        paste0("您确定要删除订单 ", order_id, " 吗？此操作无法撤销！"),
        footer = tagList(
          modalButton("取消"),  # 关闭弹窗按钮
          actionButton("confirm_delete_order_btn", "确认删除", class = "btn-danger")
        )
      )
    )
  })
  
  # 确认删除订单逻辑
  observeEvent(input$confirm_delete_order_btn, {
    removeModal()  # 关闭确认弹窗
    
    req(selected_order_row())  # 确保用户选择了一行订单
    selected_row <- selected_order_row()
    
    # 获取选中的订单数据
    selected_order <- filtered_orders()[selected_row, ]
    order_id <- selected_order$OrderID
    
    tryCatch({
      # 获取与订单关联的物品
      associated_items <- unique_items_data() %>% filter(OrderID == order_id)
      
      if (nrow(associated_items) > 0) {
        # 遍历关联物品进行逆向操作
        lapply(1:nrow(associated_items), function(i) {
          item <- associated_items[i, ]
          
          # 查询物品的原始状态
          original_state <- dbGetQuery(con, paste0(
            "SELECT * FROM item_status_history WHERE UniqueID = '", item$UniqueID, "' ORDER BY change_time DESC LIMIT 1"
          ))
          
          if (nrow(original_state) > 0) {
            # 恢复库存数量
            adjust_inventory_quantity(con, item$SKU, adjustment = 1)  # 增加库存数量
            
            # 恢复物品状态
            update_status(
              con = con,
              unique_id = item$UniqueID,
              new_status = original_state$previous_status,
              clear_status_timestamp = item$Status
            )
            
            # 清空物品的 OrderID
            update_order_id(
              con = con,
              unique_id = item$UniqueID,
              order_id = NULL  # 清空订单号
            )
          } else {
            showNotification(paste0("物品 ", item$UniqueID, " 无状态历史记录，无法恢复。"), type = "error")
          }
        })
      }
      
      # 删除订单记录
      dbExecute(con, "DELETE FROM orders WHERE OrderID = ?", params = list(order_id))
      
      # 通知用户操作结果
      message <- if (nrow(associated_items) > 0) {
        paste("订单", order_id, "已成功删除，订单内物品已返回库存！")
      } else {
        paste("订单", order_id, "已成功删除，没有关联的物品需要处理！")
      }
      showNotification(message, type = "message")
      
      # 更新数据并触发 UI 刷新
      unique_items_data_refresh_trigger(!unique_items_data_refresh_trigger())
      orders_refresh_trigger(!orders_refresh_trigger())
      
      # 重置输入
      reset_order_form(session, image_sold)
      
      # 清空关联物品表
      output$associated_items_table <- renderDT({ NULL })
    }, error = function(e) {
      showNotification(paste("删除订单时发生错误：", e$message), type = "error")
    })
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
      
      # 检查订单号是否包含 "@"
      if (!grepl("@", selected_order_id)) {
        showNotification("选中的订单不包含识别符 '@'，无法进行合并！", type = "error")
        return()
      }
      
      # 提取主单号
      main_order_id <- sub("@.*", "", selected_order_id)  # 提取 '@' 之前的部分
      
      # 获取可能的子单
      possible_sub_orders <- orders() %>%
        filter(grepl(paste0("^", main_order_id, "@"), OrderID))
      
      # 检查子单是否满足合并条件
      if (nrow(possible_sub_orders) == 0) {
        showNotification("未找到符合条件的子单！", type = "error")
        return()
      }
      
      order_statuses <- unique(possible_sub_orders$OrderStatus)
      tracking_numbers <- unique(possible_sub_orders$UsTrackingNumber)
      platforms <- unique(possible_sub_orders$Platform)
      
      # 检查订单状态、运单号和平台是否满足合并条件
      if (!all(order_statuses == "备货") || length(tracking_numbers) > 1 || length(platforms) > 1) {
        showNotification("子单的订单状态必须全部为 '备货'，运单号和平台必须一致，无法合并！", type = "error")
        return()
      }
      
      # 获取子单的所有物品
      sub_items <- unique_items_data() %>%
        filter(OrderID %in% possible_sub_orders$OrderID)
      
      # 子单物品图片路径拼接
      image_paths <- unique(sub_items$ItemImagePath[!is.na(sub_items$ItemImagePath)])
      
      if (length(image_paths) > 0) {
        # 生成拼接图片路径（带时间戳）
        montage_path <- paste0("/var/www/images/", main_order_id, "_montage_", format(Sys.time(), "%Y%m%d%H%M%S"), ".jpg")
        # 调用拼接图片函数
        merged_image_path <- generate_montage(image_paths, montage_path)
      } else {
        merged_image_path <- NA  # 如果没有图片，路径设为 NA
      }
      
      # 更新订单信息
      merged_order <- tibble(
        OrderID = main_order_id,
        Platform = platforms[1],
        UsTrackingNumber = tracking_numbers[1],
        CustomerName = ifelse(length(unique(possible_sub_orders$CustomerName)) > 0,
                              paste(unique(possible_sub_orders$CustomerName), collapse = ", "), NA),
        CustomerNetName = ifelse(length(unique(possible_sub_orders$CustomerNetName)) > 0,
                                 paste(unique(possible_sub_orders$CustomerNetName), collapse = ", "), NA),
        OrderImagePath = merged_image_path,  # 使用拼接后的图片路径
        OrderNotes = ifelse(length(unique(possible_sub_orders$OrderNotes)) > 0,
                            paste(unique(possible_sub_orders$OrderNotes), collapse = " | "), NA),
        OrderStatus = "备货"
      )
      # 更新数据库中的订单
      dbWriteTable(
        con, "orders", merged_order,
        append = TRUE, overwrite = FALSE
      )
      
      # 删除子单
      dbExecute(con, sprintf(
        "DELETE FROM orders WHERE OrderID IN (%s)",
        paste(shQuote(possible_sub_orders$OrderID), collapse = ", ")
      ))
      
      # 更新子单物品的订单号为主单号
      update_order_id(con, sub_items$UniqueID, main_order_id)
      
      showNotification(paste("订单合并成功！主单号为：", main_order_id, ", 共计", nrow(sub_items), "件物品"), type = "message")
      
      # 更新数据并触发 UI 刷新
      unique_items_data_refresh_trigger(!unique_items_data_refresh_trigger())
      orders_refresh_trigger(!orders_refresh_trigger())
      
    }, error = function(e) {
      showNotification(paste("合并订单时发生错误：", e$message), type = "error")
    })
  })
  
  
  
  ##################################################################################################
  ##################################################################################################
  ##################################################################################################
  
  
  
  ################################################################
  ##                                                            ##
  ## 物品管理分页                                               ##
  ##                                                            ##
  ################################################################
  
  # 物品表过滤模块
  itemFilterServer(
    id = "manage_filter",
    makers_items_map = makers_items_map
  )

  # 采购商品图片处理模块
  image_manage <- imageModuleServer("image_manage")
  
  # 处理更新图片
  observeEvent(input$update_image_btn, {
    selected_rows <- unique_items_table_manage_selected_row()
    if (length(selected_rows) != 1) {
      showNotification("请确保只选中一行！", type = "error")
      return()
    }
    
    # 从选中的行获取 SKU
    selected_item <- filtered_unique_items_data_manage()[selected_rows, ]
    selected_sku <- selected_item$SKU
    
    if (is.null(selected_sku) || selected_sku == "") {
      showNotification("无法获取所选行的 SKU，请检查！", type = "error")
      return()
    }
    
    # 检查 SKU 是否存在于库存表
    existing_inventory_items <- inventory()
    if (!selected_sku %in% existing_inventory_items$SKU) {
      showNotification("库存中无此 SKU 商品，无法更新图片！", type = "error")
      return()
    }
    
    # 获取当前 SKU 的图片路径
    existing_item <- existing_inventory_items[existing_inventory_items$SKU == selected_sku, ]
    existing_image_path <- existing_item$ItemImagePath[1]
    
    # 处理图片上传或粘贴
    updated_image_path <- process_image_upload(
      sku = selected_sku,
      file_data = image_manage$uploaded_file(),
      pasted_data = image_manage$pasted_file(),
      inventory_path = existing_image_path
    )
    
    # 检查处理结果并更新数据库
    if (!is.null(updated_image_path) && !is.na(updated_image_path)) {
      tryCatch({
        # 更新数据库中 SKU 对应的图片路径
        dbExecute(con, "UPDATE inventory 
                    SET ItemImagePath = ? 
                    WHERE SKU = ?",
                  params = list(updated_image_path, selected_sku))
        
        # 更新数据并触发 UI 刷新
        unique_items_data_refresh_trigger(!unique_items_data_refresh_trigger())
        
        # 显示成功通知
        showNotification(paste0("SKU ", selected_sku, " 的图片已成功更新！"), type = "message")
      }, error = function(e) {
        # 数据库操作失败时提示错误
        showNotification("图片路径更新失败，请重试！", type = "error")
      })
    } else {
      # 未检测到有效图片数据
      showNotification("未检测到有效的图片数据，请上传或粘贴图片！", type = "error")
    }
    
    # 重置图片上传状态
    image_manage$reset()
  })
  
  # 处理更新价格
  observeEvent(input$update_info_btn, {
    # 获取所有选中行索引
    selected_rows <- unique_items_table_manage_selected_row()
    
    # 验证是否有选中行
    if (is.null(selected_rows) || length(selected_rows) == 0) {
      showNotification("请至少选中一行进行更新！", type = "error")
      return()
    }
    
    # 获取过滤后的数据
    selected_items <- filtered_unique_items_data_manage()[selected_rows, ]
    
    # 验证用户输入的新数据
    new_product_cost <- input$update_product_cost
    new_shipping_cost <- input$update_shipping_cost
    
    if (is.null(new_product_cost) || new_product_cost < 0) {
      showNotification("请输入有效的单价！", type = "error")
      return()
    }
    if (is.null(new_shipping_cost) || new_shipping_cost < 0) {
      showNotification("请输入有效的国内运费！", type = "error")
      return()
    }
    
    # 遍历选中行并更新数据库
    tryCatch({
      lapply(1:nrow(selected_items), function(i) {
        unique_id <- selected_items$UniqueID[i]
        
        # 更新数据库
        dbExecute(
          con,
          "UPDATE unique_items 
         SET ProductCost = ?, DomesticShippingCost = ? 
         WHERE UniqueID = ?",
          params = list(new_product_cost, new_shipping_cost, unique_id)
        )
      })
      
      # 刷新数据表
      unique_items_data_refresh_trigger(!unique_items_data_refresh_trigger())
      
      # 显示成功通知
      showNotification(paste0("成功更新了 ", nrow(selected_items), " 项物品的信息！"), type = "message")
    }, error = function(e) {
      showNotification(paste("更新失败：", e$message), type = "error")
    })
  })
  
  
  # 点击填写单价与运费
  observeEvent(unique_items_table_manage_selected_row(), {
    selected_rows <- unique_items_table_manage_selected_row()
    
    # 检查是否有选中行
    if (!is.null(selected_rows) && length(selected_rows) > 0) {
      # 获取最新点击的行索引
      latest_row <- tail(selected_rows, n = 1)
      
      # 获取过滤后的数据
      data <- filtered_unique_items_data_manage()
      
      # 确保数据框不为空且行索引有效
      if (!is.null(data) && nrow(data) >= latest_row) {
        selected_data <- data[latest_row, ]  # 提取最新点击的行数据
        
        # 更新输入框
        updateNumericInput(session, "update_product_cost", value = selected_data$ProductCost)
        updateNumericInput(session, "update_shipping_cost", value = selected_data$DomesticShippingCost)
        
        showNotification("已加载最新点击记录的信息！", type = "message")
      } else {
        showNotification("选中的行无效或数据为空！", type = "error")
      }
    } else {
      showNotification("未选中任何行！", type = "warning")
    }
  })

  # 清空
  observeEvent(input$clear_info_btn, {
    # 清空单价和运费输入框
    updateNumericInput(session, "update_product_cost", value = "")
    updateNumericInput(session, "update_shipping_cost", value = "")
    
    # 重置图片上传组件
    image_manage$reset()
    
    showNotification("商品信息已清空！", type = "message")
  })
  
  ###
  
  # 监听删除按钮点击事件，弹出确认框
  observeEvent(input$confirm_delete_btn, {
    selected_rows <- unique_items_table_manage_selected_row()
    
    # 如果没有选中行，提示用户
    if (is.null(selected_rows) || length(selected_rows) == 0) {
      showNotification("请先选择要删除的物品！", type = "error")
      return()
    }
    
    # 显示确认框
    showModal(deleteConfirmationModal(length(selected_rows)))
  })
  
  # 确认框内 "确认删除" 按钮逻辑
  observeEvent(input$confirm_delete_final, {
    selected_rows <- unique_items_table_manage_selected_row()
    
    # 如果没有选中行，提示用户
    if (is.null(selected_rows) || length(selected_rows) == 0) {
      showNotification("没有选中任何物品！", type = "error")
      return()
    }
    
    tryCatch({
      # 获取选中物品的 UniqueID 和 SKU
      selected_items <- filtered_unique_items_data_manage()[selected_rows, ]
      
      dbBegin(con) # 开启事务
      
      for (i in seq_len(nrow(selected_items))) {
        unique_id <- selected_items$UniqueID[i]
        sku <- selected_items$SKU[i]
        
        # 删除 unique_items 中对应的记录
        dbExecute(con, "
          DELETE FROM unique_items
          WHERE UniqueID = ?", params = list(selected_items$UniqueID[i]))
        
        # 删除 item_status_history 中对应的历史状态记录
        dbExecute(con, "
          DELETE FROM item_status_history
          WHERE UniqueID = ?", params = list(unique_id))
        
        remaining_items <- dbGetQuery(con, "
                            SELECT COUNT(*) AS RemainingCount
                            FROM unique_items
                            WHERE SKU = ?", params = list(sku))
        
        if (remaining_items$RemainingCount[1] > 0) {
          # 库存减一
          adjust_inventory_quantity(con, sku, adjustment = -1)
        } else {
          # 如果没有剩余记录，删除 inventory 表中的该 SKU
          dbExecute(con, "
            DELETE FROM inventory
            WHERE SKU = ?", params = list(sku))
        }
        inventory_refresh_trigger(!inventory_refresh_trigger())
      }
      
      dbCommit(con) # 提交事务
      
      # 通知用户成功删除
      showNotification("物品及其历史状态记录删除成功！", type = "message")
      
      # 更新数据并触发 UI 刷新
      unique_items_data_refresh_trigger(!unique_items_data_refresh_trigger())
      
    }, error = function(e) {
      dbRollback(con) # 回滚事务
      showNotification(paste("删除物品时发生错误：", e$message), type = "error")
    })
    
    # 关闭确认框
    removeModal()
  })
  
  
  
  ################################################################
  ##                                                            ##
  ## 瑕疵商品分页                                               ##
  ##                                                            ##
  ################################################################
  
  # 物品表过滤模块
  itemFilterServer(
    id = "defect_filter",
    makers_items_map = makers_items_map
  )
  
  # 处理登记为瑕疵品
  observeEvent(input$register_defective, {
    selected_rows <- unique_items_table_defect_selected_row()  # 获取选中行索引
    
    if (is.null(selected_rows) || length(selected_rows) == 0) {
      showNotification("请先选择物品！", type = "error")
      return()
    }
    
    tryCatch({
      # 获取选中物品的数据
      selected_data <- filtered_unique_items_data_defect()[selected_rows, ]
      
      # 检查是否所有选中物品的状态符合要求（Defect == "无瑕" 或 Defect == "修复"）
      invalid_items <- selected_data[!selected_data$Defect %in% c("无瑕", "修复"), ]
      if (nrow(invalid_items) > 0) {
        showNotification("只有‘无瑕’或‘修复’状态的物品可以登记为瑕疵品！", type = "error")
        return()
      }
      
      # 遍历每个选中物品，进行状态更新和备注添加
      lapply(selected_data$UniqueID, function(unique_id) {
        # 更新状态为瑕疵
        update_status(con, unique_id, defect_status = "瑕疵", refresh_trigger = unique_items_data_refresh_trigger)
        
        # 添加备注
        defect_notes <- trimws(input$manage_defective_notes)
        add_defective_note(
          con = con,
          unique_id = unique_id,
          note_content = defect_notes,
          status_label = "瑕疵",
          refresh_trigger = unique_items_data_refresh_trigger
        )
      })
      
      # 清空备注栏
      updateTextAreaInput(session, "manage_defective_notes", value = "")
      
      showNotification("所选物品已成功登记为瑕疵品！", type = "message")
    }, error = function(e) {
      showNotification(paste("登记失败：", e$message), type = "error")
    })
  })
  
  # 处理登记为修复品
  observeEvent(input$register_repair, {
    selected_rows <- unique_items_table_defect_selected_row()  # 获取选中行索引
    
    if (is.null(selected_rows) || length(selected_rows) == 0) {
      showNotification("请先选择物品！", type = "error")
      return()
    }
    
    tryCatch({
      # 获取选中物品的数据
      selected_data <- filtered_unique_items_data_defect()[selected_rows, ]
      
      # 检查是否所有选中物品都满足条件（Defect == "瑕疵"）
      invalid_items <- selected_data[selected_data$Defect != "瑕疵", ]
      if (nrow(invalid_items) > 0) {
        showNotification("只有‘瑕疵’状态的物品可以登记为修复品！", type = "error")
        return()
      }
      
      # 遍历每个选中物品，进行状态更新和备注添加
      lapply(selected_data$UniqueID, function(unique_id) {
        # 更新状态为修复
        update_status(con, unique_id, defect_status = "修复", refresh_trigger = unique_items_data_refresh_trigger)
        
        # 添加备注
        repair_notes <- trimws(input$manage_defective_notes)
        add_defective_note(
          con = con,
          unique_id = unique_id,
          note_content = repair_notes,
          status_label = "修复",
          refresh_trigger = unique_items_data_refresh_trigger
        )
      })
      
      # 清空备注栏
      updateTextAreaInput(session, "manage_defective_notes", value = "")
      
      showNotification("所选物品已成功登记为修复品！", type = "message")
    }, error = function(e) {
      showNotification(paste("登记失败：", e$message), type = "error")
    })
