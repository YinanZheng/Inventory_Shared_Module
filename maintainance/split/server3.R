                          item$ItemName,
                          purchase_qty,
                          ifelse(remark == "", NA_character_, new_remark)
                        )
              )
              showNotification(paste0("超出国内库存部分已创建采购请求，SKU：", sku, "，数量：", purchase_qty), type = "warning")
              # 动态更新按钮文本和样式
              updateActionButton(session, inputId = button_id, label = HTML("<i class='fa fa-check'></i> 出库+采购请求已发送"))
              runjs(sprintf("$('#%s').removeClass('btn-primary').addClass('btn-success');", button_id))
              shinyjs::disable(button_id)
            }
            
            # 绑定按钮
            bind_buttons(request_id, requests_data(), input, output, session, con)
          }, error = function(e) {
            # 提示错误消息
            showNotification(paste("发出出库请求失败：", e$message), type = "error")
          })
        } else if (grepl("purchase", button_id)) {
          # 采购请求处理逻辑
          sku <- sub("create_request_purchase_", "", button_id)  # 提取 SKU
          items <- zero_stock_items()  # 从 reactiveVal 获取库存为零的物品
          item <- items[[which(sapply(items, function(x) x$SKU == sku))]]  # 找到匹配的物品
          
          # 获取请求数量
          qty <- input[[paste0("purchase_qty_", sku)]]
          
          # 获取留言
          remark <- input[[paste0("purchase_remark_input_", sku)]]
          remark_prefix <- if (system_type == "cn") "[京]" else "[圳]"
          new_remark <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), ": ", remark_prefix, " ", remark)
          
          request_id <- uuid::UUIDgenerate()
          
          tryCatch({
            # 插入采购请求到数据库
            dbExecute(con,
                      "INSERT INTO requests (RequestID, SKU, Maker, ItemImagePath, ItemDescription, Quantity, RequestStatus, RequestType, CreatedAt, Remarks)
                     VALUES (?, ?, ?, ?, ?, ?, '待处理', '采购', NOW(), ?)",
                      params = list(
                        request_id,
                        sku,
                        item$Maker,
                        item$ItemImagePath,
                        item$ItemName,
                        qty,
                        ifelse(remark == "", NA_character_, new_remark)
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
    outbound_stock_items(list())    # 清空出库物品列表
    removeModal()                   # 关闭模态框
  })
  
  
  # 订单物品删除逻辑 （美国售出only）
  observeEvent(input$delete_card, {
    req(input$delete_card, new_order_items())  # 确保输入和物品列表存在
    
    # 当前物品列表
    current_items <- new_order_items()
    
    # 移除对应的物品
    updated_items <- current_items %>% filter(UniqueID != input$delete_card)
    new_order_items(updated_items)  # 更新物品列表
    
    # 提示删除成功
    showNotification("物品已删除。", type = "message")
  })
  
  # 清空逻辑
  observeEvent(input$clear_us_shipping_bill_btn, {
    updateTextInput(session, "us_shipping_bill_number", value = "")
    updateTextInput(session, "us_shipping_sku_input", value = "")
    updateSelectInput(session, "us_shipping_platform", selected = "TikTok")
    new_order_items(NULL)  # 清空物品列表
  })
  
  
  ##################################################################################################
  ##################################################################################################
  ##################################################################################################
  
  
  ################################################################
  ##                                                            ##
  ## 订单管理分页                                               ##
  ##                                                            ##
  ################################################################
  
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
    
    label_pdf_file_path(file.path("/var/uploads/shiplabels", paste0(selected_order$UsTrackingNumber, ".pdf")))
    
    # 填充左侧订单信息栏
    updateTextInput(session, "order_id", value = order_id)
    
    # 动态更新标题
    output$associated_items_title <- renderUI({
      div(
        style = "display: flex; align-items: center; justify-content: space-between;",
        
        # 左侧标题
        tags$h4(
          sprintf("#%s - %s 的订单物品", order_id, customer_name),
          style = "color: #007BFF; font-weight: bold; margin: 0;"
        ),
        
        # 右侧按钮（仅在订单状态为“预定”时显示）
        if (order_status == "调货") {
          tagList(
            actionButton(
              inputId = "complete_transfer",
              label = "已完成调货",
              class = "btn-success",
              style = "margin-left: auto; font-size: 14px; padding: 5px 10px;"
            ),
            downloadButton("download_shipping_label_pdf_manage", label = "下载运单", class = "btn btn-primary", 
                           style = "height: 34px; margin-left: 10px; font-size: 14px; padding: 5px 10px;")
          )
        } else {
          NULL
        }
      )
    })
    
    # 更新关联物品数据
    associated_items <- associated_items(unique_items_data() %>% filter(OrderID == order_id))
  })
  
  observeEvent(input$complete_transfer, {
    req(selected_order_row())
    
    # 获取选中订单
    selected_row <- selected_order_row()
    selected_order <- filtered_orders()[selected_row, ]
    order_id <- selected_order$OrderID
    existing_notes <- selected_order$OrderNotes %||% ""  # 若为空，则默认空字符串

    # 在 R 中拼接备注内容
    new_notes <- paste(existing_notes, sprintf("【调货完成 %s】", format(Sys.Date(), "%Y-%m-%d")))
    
    update_order_status(order_id = order_id, 
                        new_status = "备货", 
                        updated_notes = new_notes, 
                        refresh_trigger = orders_refresh_trigger,
                        con = con)
  })
  
  # 渲染物品信息卡片  
  observe({
    req(associated_items())
    if (nrow(associated_items()) == 0) {
      renderOrderItems(output, "order_items_cards", data.frame(), con)  # 清空物品卡片
      return()
    }
    renderOrderItems(output, "order_items_cards", associated_items(), con, deletable = FALSE)
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
  
  # 定义运单下载处理器
  output$download_shipping_label_pdf_manage <- downloadHandler(
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
  
  # 监听 "已经到到齐" 表格行的点击事件
  observeEvent(selected_orders_table_arrived_row(), {
    selected_row <- selected_orders_table_arrived_row() 
    req(selected_row) 
    
    tracking_number <- filtered_orders_arrived()[selected_row, "UsTrackingNumber"]
    
    # 如果运单号为空或缺失，显示提示信息
    if (is.null(tracking_number) || tracking_number == "") {
      showNotification("未找到运单号，请检查", type = "error")
      return()  # 终止后续操作
    }
    
    updateTabsetPanel(session, "inventory_us", selected = "发货") # 跳转到“发货”页面
    
    updateTextInput(session, "shipping_bill_number", value = tracking_number)
  })
  
  # 监听 "没有到齐" 表格行的点击事件
  observeEvent(selected_orders_table_waiting_row(), {
    selected_row <- selected_orders_table_waiting_row() 
    req(selected_row)
    
    tracking_number <- filtered_orders_waiting()[selected_row, "UsTrackingNumber"]
    
    # 如果运单号为空或缺失，显示提示信息
    if (is.null(tracking_number) || tracking_number == "") {
      showNotification("未找到运单号，请检查", type = "error")
      return()  # 终止后续操作
    }
    
    updateTabsetPanel(session, "inventory_us", selected = "发货") # 跳转到“发货”页面
    
    updateTextInput(session, "shipping_bill_number", value = tracking_number)
  })
  
  
  
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
  
  # 处理更新物品信息
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
    new_purchase_date <- input$update_purchase_date
    
    if (is.null(new_product_cost) || new_product_cost < 0) {
      showNotification("请输入有效的单价！", type = "error")
      return()
    }
    if (is.null(new_shipping_cost) || new_shipping_cost < 0) {
      showNotification("请输入有效的国内运费！", type = "error")
      return()
    }
    if (is.null(new_purchase_date) || !lubridate::is.Date(as.Date(new_purchase_date))) {
      showNotification("请输入有效的采购日期！", type = "error")
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
                 SET ProductCost = ?, DomesticShippingCost = ?, PurchaseTime = ? 
                 WHERE UniqueID = ?",
          params = list(new_product_cost, new_shipping_cost, as.Date(new_purchase_date), unique_id)
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
  
  # 点击填写物品信息
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
        updateDateInput(session, "update_purchase_date", value = as.Date(selected_data$PurchaseTime))
        
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
    updateDateInput(session, "update_purchase_date", value = Sys.Date())
    
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
        status <- selected_items$Status[i]  # 获取物品状态
        
        # 删除 unique_items 中对应的记录
        dbExecute(con, "
              DELETE FROM unique_items
              WHERE UniqueID = ?", params = list(unique_id))
        
        # 删除 item_status_history 中对应的历史状态记录
        dbExecute(con, "
              DELETE FROM item_status_history
              WHERE UniqueID = ?", params = list(unique_id))
        
        if (status != "采购") {  # 如果状态不是采购，调整库存
          remaining_quantity <- adjust_inventory_quantity(con, sku, adjustment = -1)
          
          if (is.null(remaining_quantity)) {
            showNotification(paste("无法调整库存，SKU:", sku), type = "error")
          } else if (remaining_quantity == 0) {
            # 如果库存为 0，删除 inventory 表中的该 SKU
            dbExecute(con, "
                      DELETE FROM inventory
                      WHERE SKU = ?", params = list(sku))
            showNotification(paste0(sku, "已从库存表中移除！"), type = "message")
          }
        }
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
  })
  
  # 监听“仅显示无瑕品”开关的状态变化
  observeEvent(input$show_perfects_only, {
    if (input$show_perfects_only && input$show_defects_only) {
      updateSwitchInput(session, "show_defects_only", value = FALSE)
    }
  })
  
  # 监听“仅显示瑕疵品”开关的状态变化
  observeEvent(input$show_defects_only, {
    if (input$show_defects_only && input$show_perfects_only) {
      updateSwitchInput(session, "show_perfects_only", value = FALSE)
    }
  })
  
  
  
  ################################################################
  ##                                                            ##
  ## 国际物流管理分页                                           ##
  ##                                                            ##
  ################################################################
  
  # 筛选逻辑
  itemFilterServer(
    id = "logistic_filter",
    makers_items_map = makers_items_map)
  
  ######################
  ### 国际运单登记分页
  ######################
  
  # 登记运单信息
  observeEvent(input$register_shipment_btn, {
    req(input$intl_tracking_number, input$intl_shipping_method, input$intl_total_shipping_cost)
    
    # 获取用户输入的值
    tracking_number <- trimws(input$intl_tracking_number)
    shipping_method <- input$intl_shipping_method
    total_cost <- as.numeric(input$intl_total_shipping_cost)
    
    tryCatch({
      # 更新或插入运单记录
      dbExecute(
        con,
        "INSERT INTO intl_shipments (TrackingNumber, ShippingMethod, TotalCost, Status)
       VALUES (?, ?, ?, '运单创建')
       ON DUPLICATE KEY UPDATE 
         ShippingMethod = VALUES(ShippingMethod), 
         TotalCost = VALUES(TotalCost),
         UpdatedAt = CURRENT_TIMESTAMP",
        params = list(tracking_number, shipping_method, total_cost)
      )
      
      # # 生成交易记录的备注
      # remarks <- paste0("[国际运费登记]", " 运单号：", tracking_number, " 运输方式：", shipping_method)
      # 
      # # 生成交易记录的 ID
      # transaction_id <- generate_transaction_id("一般户卡", total_cost, remarks, Sys.time())
      # 
      # # 插入交易记录到“一般户卡”
      # dbExecute(
      #   con,
      #   "INSERT INTO transactions (TransactionID, AccountType, Amount, Remarks, TransactionTime) 
      #  VALUES (?, ?, ?, ?, ?)",
      #   params = list(
      #     transaction_id,
      #     "一般户卡", 
      #     -total_cost,  # 转出金额为负值
      #     remarks,
      #     Sys.time()
      #   )
      # )
      # 
      # showNotification("国际运单登记成功，相关费用已记录到'一般户卡（541）'！", type = "message")
      # 
      # # 重新计算所有balance记录
      # update_balance("一般户卡", con)
      
      shinyjs::enable("link_tracking_btn")  # 启用挂靠运单按钮
    }, error = function(e) {
      showNotification(paste("操作失败：", e$message), type = "error")
    })
  })
  
  # 查询运单逻辑
  observeEvent(input$intl_tracking_number, {
    tracking_number <- input$intl_tracking_number
    
    if (is.null(tracking_number) || tracking_number == "") {
      # 如果运单号为空，清空相关输入字段并禁用按钮
      updateSelectInput(session, "intl_shipping_method", selected = "空运")
      updateNumericInput(session, "intl_total_shipping_cost", value = 0)
      shinyjs::disable("link_tracking_btn")  # 禁用挂靠运单按钮
      output$intl_status_display <- renderText({ "" })  # 清空状态显示
      return()
    }
    
    tryCatch({
      # 查询运单号对应的信息
      shipment_info <- dbGetQuery(
        con,
        "SELECT ShippingMethod, TotalCost, Status FROM intl_shipments WHERE TrackingNumber = ?",
        params = list(tracking_number)
      )
      
      if (nrow(shipment_info) > 0) {
        # 如果运单号存在，回填信息
        updateSelectInput(session, "intl_shipping_method", selected = shipment_info$ShippingMethod[1])
        updateNumericInput(session, "intl_total_shipping_cost", value = shipment_info$TotalCost[1])
        shinyjs::enable("link_tracking_btn")  # 启用挂靠运单按钮
        
        # 显示物流状态
        output$intl_status_display <- renderText({
          paste("物流状态:", shipment_info$Status[1])
        })
        
      } else {
        # 如果运单号不存在，清空相关字段并禁用按钮
        updateSelectInput(session, "intl_shipping_method", selected = "空运")
        updateNumericInput(session, "intl_total_shipping_cost", value = 0)
        shinyjs::disable("link_tracking_btn")  # 禁用挂靠运单按钮
        
        # 提示未找到状态
        output$intl_status_display <- renderText({
          "未找到对应的运单信息，可以登记新运单！"
        })
      }
    }, error = function(e) {
      # 遇到错误时禁用按钮并清空状态显示
      shinyjs::disable("link_tracking_btn")
      output$intl_status_display <- renderText({
        paste("查询失败：", e$message)
      })
      showNotification(paste("加载运单信息失败：", e$message), type = "error")
    })
  })
  
  # 货值汇总显示
  observeEvent(input$batch_value_btn, {
    tracking_number <- input$intl_tracking_number
    
    if (is.null(tracking_number) || tracking_number == "") {
      showNotification("请输入运单号后再执行此操作！", type = "error")
      return()
    }
    
    tryCatch({
      # 查询与运单号相关的汇总信息
      summary_info <- dbGetQuery(
        con,
        "
      SELECT 
        COUNT(*) AS TotalQuantity,
        SUM(ProductCost) AS TotalValue,
        SUM(DomesticShippingCost) AS TotalDomesticShipping,
        SUM(IntlShippingCost) AS TotalIntlShipping
      FROM unique_items
      WHERE IntlTracking = ?
      ",
        params = list(tracking_number)
      )
      
      # 查询运单号的运输方式
      shipping_method_info <- dbGetQuery(
        con,
        "SELECT ShippingMethod FROM intl_shipments WHERE TrackingNumber = ?",
        params = list(tracking_number)
      )
      
      if (nrow(summary_info) == 0 || is.na(summary_info$TotalQuantity[1])) {
        showNotification("未找到与当前运单号相关的货物信息！", type = "warning")
        return()
      }
      
      # 确定运输方式
      shipping_method <- ifelse(nrow(shipping_method_info) > 0, shipping_method_info$ShippingMethod[1], "未知")
      
      # 计算总价值合计
      total_value_sum <- summary_info$TotalValue[1] + summary_info$TotalDomesticShipping[1] + summary_info$TotalIntlShipping[1]
