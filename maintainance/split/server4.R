    
    # 存储当前选中的订单ID
    selected_order_id(order_id)
    
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
          div(
            style = "display: flex; gap: 10px;",
            actionButton("regen_order_image", label = "重新生成订单拼图", class = "btn btn-warning", 
                         style = "height: 34px; font-size: 14px; padding: 5px 10px;"),
            if (selected_order$LabelStatus != "无") {
              downloadButton("download_pdf_manage", label = "下载运单", class = "btn btn-primary", 
                             style = "height: 34px; font-size: 14px; padding: 5px 10px;")
            }
          )
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
        div(
          style = "display: flex; gap: 10px;",
          actionButton("regen_order_image", label = "重新生成订单拼图", class = "btn btn-warning", 
                       style = "height: 34px; font-size: 14px; padding: 5px 10px;"),
          if (order_status == "预定") {
            actionButton("complete_preorder", label = "已完成预定", class = "btn-success", 
                         style = "font-size: 14px; padding: 5px 10px;")
          },
          if (selected_order$LabelStatus != "无") {
            downloadButton("download_pdf_manage", label = "下载运单", class = "btn btn-primary", 
                           style = "height: 34px; font-size: 14px; padding: 5px 10px;")
          }
        )
      )
    })
    
    # 更新关联物品数据
    associated_items <- associated_items(unique_items_data() %>% filter(OrderID == order_id))
    
    # 重置图片上传模块
    image_sold$reset()
  })
  
  observeEvent(input$regen_order_image, {
    req(selected_order_id())
    success <- update_order_montage(selected_order_id(), con, unique_items_data())
    
    # 只有当拼图生成成功时才刷新订单数据
    if (success) {
      orders_refresh_trigger(!orders_refresh_trigger())
    }
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
    
    update_order_status(order_id, "备货", updated_notes = new_notes, refresh_trigger = orders_refresh_trigger, con)
  })
  
  # 渲染物品信息卡片  
  observe({
    req(associated_items())
    if (nrow(associated_items()) == 0) {
      renderOrderItems(output, "order_items_cards", data.frame(), con)  # 清空物品卡片
      return()
    }
    renderOrderItems(output, "order_items_cards", associated_items(), con, deletable = TRUE)
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
    
    # 清空物品的 OrderID
    update_order_id(
      con = con,
      unique_id = deleted_item$UniqueID,
      order_id = NULL  # 清空订单号
    )
    
    # 提示删除成功
    showNotification("物品已删除, 库存已归还。", type = "message")

    # 更新数据并触发 UI 刷新
    orders_refresh_trigger(!orders_refresh_trigger())
    
    # 检查并更新订单拼图
    order_id <- deleted_item$OrderID
    order_info <- dbGetQuery(con, paste0(
      "SELECT OrderImagePath FROM orders WHERE OrderID = '", order_id, "'"
    ))
    
    if (nrow(order_info) > 0) {
      order_image_path <- order_info$OrderImagePath
      # 检查 OrderImagePath 是否为空或包含“montage” <- 只有原图是拼图的时候才能更新
      if (is.na(order_image_path) || grepl("montage", order_image_path)) {
        update_order_montage(order_id, con, unique_items_data())
      }
    }
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
      orders_refresh_trigger(!orders_refresh_trigger())
      
      # 重置输入
      reset_order_form(session, image_sold)
      
      # 重置库存商品名列表
      updateSelectizeInput(session, "preorder_item_name_db", choices = c("", inventory()$ItemName), selected = NULL, server = TRUE)
      
      # 清空关联物品表
      output$associated_items_title <- renderDT({ NULL }) # 清空标题
      renderOrderItems(output, "order_items_cards", data.frame(), con)  # 清空物品卡片
    }, error = function(e) {
      showNotification(paste("删除订单时发生错误：", e$message), type = "error")
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
        
        # 更新inventory数据需要手动触发刷新
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
      }
      
      dbCommit(con) # 提交事务
      
      # 通知用户成功删除
      showNotification("物品及其历史状态记录删除成功！", type = "message")
      
      # 删除物品需要手动触发更新inventory
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
        update_status(con, unique_id, defect_status = "瑕疵", refresh_trigger = NULL)
        
        # 添加备注
        defect_notes <- trimws(input$manage_defective_notes)
        add_defective_note(
          con = con,
          unique_id = unique_id,
          note_content = defect_notes,
          status_label = "瑕疵",
          refresh_trigger = NULL
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
        update_status(con, unique_id, defect_status = "修复", refresh_trigger = NULL)
        
        # 添加备注
        repair_notes <- trimws(input$manage_defective_notes)
        add_defective_note(
          con = con,
          unique_id = unique_id,
          note_content = repair_notes,
          status_label = "修复",
          refresh_trigger = NULL
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
      
      # 格式化汇总信息
      summary_text <- HTML(paste0(
        "<div style='font-family: Arial, sans-serif; line-height: 2;'>",  # 调整行间距
        "<table style='width: 100%; border-collapse: collapse;'>",
        "<tr>",
        "<td style='color: #007BFF; font-weight: bold; text-align: left; width: 30%;'>运单号:</td>",
        "<td style='text-align: left; color: #000;'>", tracking_number, " <span style='color: #28A745;'>(", shipping_method, ")</span></td>",
        "</tr>",
        "<tr>",
        "<td style='color: #007BFF; font-weight: bold; text-align: left;'>总货物数量:</td>",
        "<td style='text-align: left;'>", summary_info$TotalQuantity[1], "</td>",
        "</tr>",
        "<tr>",
        "<td style='color: #007BFF; font-weight: bold; text-align: left;'>总货物价值:</td>",
        "<td style='text-align: left;'>￥", formatC(summary_info$TotalValue[1], format = "f", digits = 2), "</td>",
        "</tr>",
        "<tr>",
        "<td style='color: #007BFF; font-weight: bold; text-align: left;'>总国内运费:</td>",
        "<td style='text-align: left;'>￥", formatC(summary_info$TotalDomesticShipping[1], format = "f", digits = 2), "</td>",
        "</tr>",
        "<tr>",
        "<td style='color: #007BFF; font-weight: bold; text-align: left;'>总国际运费:</td>",
        "<td style='text-align: left;'>￥", formatC(summary_info$TotalIntlShipping[1], format = "f", digits = 2), "</td>",
        "</tr>",
        "<tr>",
        "<td style='color: #007BFF; font-weight: bold; text-align: left;'>合计总价值:</td>",
        "<td style='text-align: left; font-size: 18px; font-weight: bold;'>￥", formatC(total_value_sum, format = "f", digits = 2), "</td>",
        "</tr>",
        "</table>",
        "</div>"
      ))
      
      
      # 创建模态对话框
      showModal(modalDialog(
        title = HTML("<strong style='color: #007BFF;'>运单货值汇总</strong>"),
        HTML(summary_text),
        easyClose = TRUE,
        footer = modalButton("关闭")
      ))
    }, error = function(e) {
      showNotification(paste("操作失败：", e$message), type = "error")
    })
  })

  # 删除运单逻辑
  observeEvent(input$delete_shipment_btn, {
    tracking_number <- input$intl_tracking_number
    
    if (is.null(tracking_number) || tracking_number == "") {
      showNotification("请输入运单号后再执行此操作！", type = "error",  )
      return()
    }
    
    tryCatch({
      # 检查运单是否存在于 intl_shipments 表中
      shipment_exists <- dbGetQuery(
        con,
        "SELECT COUNT(*) AS count FROM intl_shipments WHERE TrackingNumber = ?",
        params = list(tracking_number)
      )
      
      if (shipment_exists$count == 0) {
        showNotification("运单号不存在，无法删除！", type = "warning")
        return()
      }
      
      # 如果运单存在，弹出确认对话框
      showModal(modalDialog(
        title = HTML("<strong style='color: #C70039;'>确认删除国际运单</strong>"),
        HTML(paste0(
          "<p>您确定要删除国际运单号 <strong>", tracking_number, "</strong> 吗？关联物品的国际运单信息也会被同时清空。此操作不可逆！</p>"
        )),
        easyClose = FALSE,
        footer = tagList(
          modalButton("取消"),
          actionButton("confirm_delete_shipment_btn", "确认删除", class = "btn-danger")
        )
      ))
    }, error = function(e) {
      showNotification(paste("检查运单时发生错误：", e$message), type = "error")
    })
  })
  
  
  # 监听确认删除运单按钮的点击事件
  observeEvent(input$confirm_delete_shipment_btn, {
    tracking_number <- trimws(input$intl_tracking_number)
    
    tryCatch({
      # 开始事务
      dbBegin(con)
      
      # 清空 unique_items 表中与运单号相关的运费
      dbExecute(con, "UPDATE unique_items SET IntlShippingCost = 0.00 WHERE IntlTracking = ?", params = list(tracking_number))
      
      # 从 intl_shipments 表中删除对应的运单号 (unique_items表会同时触发运单删除操作)
      dbExecute(con, "DELETE FROM intl_shipments WHERE TrackingNumber = ?", params = list(tracking_number))
      
      # # 删除 transactions 表中与运单号相关的记录
      # dbExecute(con, "DELETE FROM transactions WHERE Remarks LIKE ?", params = list(paste0("%[国际运费登记] 运单号：", tracking_number, "%")))
      
      # 提示删除成功
      showNotification("运单与关联的物品信息已成功删除！", type = "message")
      
      # 重新计算所有balance记录
      update_balance("一般户卡", con)
      
      # 清空输入框和相关字段
      updateTextInput(session, "intl_tracking_number", value = "")
      updateSelectInput(session, "intl_shipping_method", selected = "空运")
      updateNumericInput(session, "intl_total_shipping_cost", value = 0)
      
      # 提交事务
      dbCommit(con)
    }, error = function(e) {
      # 捕获错误并提示用户，回滚事务
      dbRollback(con)
      showNotification(paste("删除失败：", e$message), type = "error")
    })
    
    # 禁用挂靠按钮
    shinyjs::disable("link_tracking_btn")
    
    # 关闭确认对话框
    removeModal()
  })
  
  # 清空填写按钮逻辑
  observeEvent(input$clean_shipment_btn, {
    # 清空输入字段
    updateTextInput(session, "intl_tracking_number", value = "")  # 清空国际运单号
    updateSelectInput(session, "intl_shipping_method", selected = "空运")  # 重置国际运输方式为默认值
    updateNumericInput(session, "intl_total_shipping_cost", value = 0)  # 重置国际物流总运费为 0
    output$intl_status_display <- renderText({ "" })  # 清空状态显示
    
    # 提示用户清空完成
    showNotification("填写内容已清空！", type = "message")
  })
  
  # 点击行自动填写运单号
  observeEvent(unique_items_table_logistics_selected_row(), {
    selected_rows <- unique_items_table_logistics_selected_row()
    
    if (is.null(selected_rows) || length(selected_rows) == 0) {
      # 如果没有选中行，清空运单号输入框，并禁用挂靠按钮
      updateTextInput(session, "intl_tracking_number", value = "")
      shinyjs::disable("link_tracking_btn")  # 禁用按钮
      shinyjs::disable("unlink_tracking_btn")  # 禁用按钮
      return()
    }
    
    tryCatch({
      # 获取选中行的数据
      selected_data <- filtered_unique_items_data_logistics()[selected_rows, ]
      
