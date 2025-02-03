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
  
  # 登记运单信息
  observeEvent(input$register_shipment_btn, {
    req(input$intl_tracking_number, input$intl_shipping_method, input$intl_total_shipping_cost)
    
    # 获取用户输入的值
    tracking_number <- input$intl_tracking_number
    shipping_method <- input$intl_shipping_method
    total_cost <- as.numeric(input$intl_total_shipping_cost)
    
    tryCatch({
      # 更新或插入运单记录
      dbExecute(
        con,
        "INSERT INTO intl_shipments (TrackingNumber, ShippingMethod, TotalCost, Status)
       VALUES (?, ?, ?, '待分配')
       ON DUPLICATE KEY UPDATE 
         ShippingMethod = VALUES(ShippingMethod), 
         TotalCost = VALUES(TotalCost),
         UpdatedAt = CURRENT_TIMESTAMP",
        params = list(tracking_number, shipping_method, total_cost)
      )
      
      showNotification("国际运单登记成功，信息已更新，可执行挂靠操作！", type = "message", duration = 5)
      
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
      return()
    }
    
    tracking_number <- input$intl_tracking_number
    
    tryCatch({
      # 查询运单号对应的信息
      shipment_info <- dbGetQuery(
        con,
        "SELECT ShippingMethod, TotalCost FROM intl_shipments WHERE TrackingNumber = ?",
        params = list(tracking_number)
      )
      
      if (nrow(shipment_info) > 0) {
        # 如果运单号存在，回填信息
        updateSelectInput(session, "intl_shipping_method", selected = shipment_info$ShippingMethod[1])
        updateNumericInput(session, "intl_total_shipping_cost", value = shipment_info$TotalCost[1])
        shinyjs::enable("link_tracking_btn")  # 启用挂靠运单按钮
        showNotification("已加载运单信息，可执行挂靠操作！", type = "message", duration = 5)
      } else {
        # 如果运单号不存在，清空相关字段并禁用按钮
        updateSelectInput(session, "intl_shipping_method", selected = "空运")
        updateNumericInput(session, "intl_total_shipping_cost", value = 0)
        shinyjs::disable("link_tracking_btn")  # 禁用挂靠运单按钮
        showNotification("未找到对应的运单信息，请登记新运单！", type = "warning", duration = 5)
      }
    }, error = function(e) {
      shinyjs::disable("link_tracking_btn")  # 遇到错误时禁用按钮
      showNotification(paste("加载运单信息失败：", e$message), type = "error")
    })
  })
  
  # 货值汇总显示
  observeEvent(input$batch_value_btn, {
    tracking_number <- input$intl_tracking_number
    
    if (is.null(tracking_number) || tracking_number == "") {
      showNotification("请输入运单号后再执行此操作！", type = "error", duration = 5)
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
      showNotification("请输入运单号后再执行此操作！", type = "error", duration = 5)
      return()
    }
    
    # 弹出确认对话框
    showModal(modalDialog(
      title = HTML("<strong style='color: #C70039;'>确认删除运单</strong>"),
      HTML(paste0(
        "<p>您确定要删除运单号 <strong>", tracking_number, "</strong> 吗？此操作不可逆！</p>"
      )),
      easyClose = FALSE,
      footer = tagList(
        modalButton("取消"),
        actionButton("confirm_delete_shipment_btn", "确认删除", class = "btn-danger")
      )
    ))
  })
  
  # 监听确认删除按钮的点击事件
  observeEvent(input$confirm_delete_shipment_btn, {
    tracking_number <- input$intl_tracking_number
    
    tryCatch({
      # 开始事务
      dbBegin(con)
      
      # 从 intl_shipments 表中删除对应的运单号
      rows_affected <- dbExecute(
        con,
        "DELETE FROM intl_shipments WHERE TrackingNumber = ?",
        params = list(tracking_number)
      )
      
      if (rows_affected > 0) {
        # 如果删除成功
        showNotification("运单已成功删除！", type = "message", duration = 5)
        
        # 更新 unique_items 表中相关记录的平摊国际运费为 0.00
        dbExecute(
          con,
          "UPDATE unique_items 
         SET IntlShippingCost = 0.00 
         WHERE IntlTracking IS NULL AND IntlShippingCost > 0.00"
        )
        
        # 清空输入框
        updateTextInput(session, "intl_tracking_number", value = "")
        updateSelectInput(session, "intl_shipping_method", selected = "空运")
        updateNumericInput(session, "intl_total_shipping_cost", value = 0)
      } else {
        # 如果没有找到对应的运单号
        showNotification("未找到该运单，删除失败！", type = "warning", duration = 5)
      }
      
      # 提交事务
      dbCommit(con)
    }, error = function(e) {
      # 捕获错误并提示用户，回滚事务
      dbRollback(con)
      showNotification(paste("删除失败：", e$message), type = "error")
    })
    
    shinyjs::disable("link_tracking_btn")  # 禁用按钮
    
    # 更新数据并触发 UI 刷新
    unique_items_data_refresh_trigger(!unique_items_data_refresh_trigger())
    
    # 关闭确认对话框
    removeModal()
  })
  
  # 点击行自动填写运单号
  observeEvent(unique_items_table_logistics_selected_row(), {
    selected_rows <- unique_items_table_logistics_selected_row()
    
    if (is.null(selected_rows) || length(selected_rows) == 0) {
      # 如果没有选中行，清空运单号输入框
      updateTextInput(session, "intl_tracking_number", value = "")
      return()
    }
    
    tryCatch({
      # 获取选中行的数据
      selected_data <- filtered_unique_items_data_logistics()[selected_rows, ]
      
      # 提取所有选中行的国际物流单号（IntlTracking）
      unique_tracking_numbers <- unique(selected_data$IntlTracking)
      
      if (length(unique_tracking_numbers) == 1 && !is.na(unique_tracking_numbers)) {
        # 如果只有一个唯一的物流单号，填写到输入框
        updateTextInput(session, "intl_tracking_number", value = unique_tracking_numbers)
        showNotification("已根据选中行填写运单号！", type = "message")
      } else {
        # 如果有多个物流单号或为空，清空输入框并提示用户
        updateTextInput(session, "intl_tracking_number", value = "")
        showNotification("选中行包含多个不同的物流单号或为空，请检查！", type = "warning")
      }
    }, error = function(e) {
      showNotification(paste("操作失败：", e$message), type = "error")
    })
  })
  
  # 挂靠运单号逻辑
  observeEvent(input$link_tracking_btn, {
    selected_rows <- unique_items_table_logistics_selected_row()  # 获取用户选择的物品行
    tracking_number <- input$intl_tracking_number  # 获取输入的运单号
    shipping_method <- input$intl_shipping_method  # 获取选择的物流方式
    
    # 校验输入和选择
    if (is.null(selected_rows) || length(selected_rows) == 0) {
      showNotification("请先选择物品！", type = "error")
      return()
    }
    
    if (is.null(tracking_number) || tracking_number == "") {
      showNotification("运单号不能为空！", type = "error")
      return()
    }
    
    tryCatch({
      # 获取选中的物品数据
      selected_items <- filtered_unique_items_data_logistics()[selected_rows, ]
      
      # 检查物流方式是否一致
      inconsistent_methods <- selected_items %>%
        filter(is.na(IntlShippingMethod) | IntlShippingMethod != shipping_method)
      
      if (nrow(inconsistent_methods) > 0) {
        showNotification("选中物品的物流方式与当前选择的物流方式不一致！", type = "error")
        return()
      }
      
      # 批量更新数据库中的 `IntlTracking`
      dbBegin(con)
      for (i in seq_len(nrow(selected_items))) {
        dbExecute(
          con,
          "UPDATE unique_items SET IntlTracking = ? WHERE UniqueID = ?",
          params = list(tracking_number, selected_items$UniqueID[i])
        )
      }
      
      # 查询运单的总运费
      shipment_info <- dbGetQuery(
        con,
        "SELECT TotalCost FROM intl_shipments WHERE TrackingNumber = ?",
        params = list(tracking_number)
      )
      
      if (nrow(shipment_info) == 0) {
        showNotification("未找到该运单的总运费信息，请检查运单号是否正确。", type = "error")
        dbRollback(con)
        return()
      }
      
      total_cost <- as.numeric(shipment_info$TotalCost)
      
      # 查询挂靠到该运单的所有物品
      related_items <- dbGetQuery(
        con,
        "SELECT UniqueID FROM unique_items WHERE IntlTracking = ?",
        params = list(tracking_number)
      )
      
      if (nrow(related_items) == 0) {
        showNotification("未找到挂靠到该运单的物品。", type = "error")
        dbRollback(con)
        return()
      }
      
      # 计算平摊运费并更新到 `unique_items`
      per_item_cost <- total_cost / nrow(related_items)
      dbExecute(
        con,
        "UPDATE unique_items SET IntlShippingCost = ? WHERE IntlTracking = ?",
        params = list(per_item_cost, tracking_number)
      )
      
      dbCommit(con)
      
      # 更新数据并触发 UI 刷新
      unique_items_data_refresh_trigger(!unique_items_data_refresh_trigger())
      
      showNotification("运单号已成功挂靠，平摊运费已更新！", type = "message")
    }, error = function(e) {
      # 回滚事务并通知用户
      dbRollback(con)
      showNotification(paste("挂靠失败：", e$message), type = "error")
    })
  })
  
  # 解除运单号挂靠逻辑
  observeEvent(input$delete_tracking_btn, {
    selected_rows <- unique_items_table_logistics_selected_row()
    
    if (is.null(selected_rows) || length(selected_rows) == 0) {
      showNotification("请先选择需要删除运单号的物品！", type = "error")
      return()
    }
    
    tryCatch({
      selected_items <- filtered_unique_items_data_logistics()[selected_rows, ]
      
      # 解除运单号关联，清零运费数据
      lapply(selected_items$UniqueID, function(unique_id) {
        dbExecute(
          con,
          "UPDATE unique_items 
         SET IntlTracking = NULL, IntlShippingCost = 0.00
         WHERE UniqueID = ?",
          params = list(unique_id)
        )
      })
      
      # 更新数据并触发 UI 刷新
      unique_items_data_refresh_trigger(!unique_items_data_refresh_trigger())
      showNotification("运单号已成功删除！", type = "message")
      
    }, error = function(e) {
      showNotification(paste("删除运单号失败：", e$message), type = "error")
    })
  })
  
  
  
  ################################################################
  ##                                                            ##
  ## 账务管理分页                                               ##
  ##                                                            ##
  ################################################################
  
  resetToCreateMode <- function() {
    is_update_mode(FALSE)  # 切换回登记模式
    selected_TransactionID(NULL)  # 清空选中的 TransactionID
    selected_TransactionImagePath(NULL)  # 清空选中的 TransactionImagePath
    
    # 更新按钮为“登记”
    updateActionButton(session, "record_transaction", label = "登记", icon = icon("save"))
  }
  
  resetTransactionForm <- function(session) {
    updateNumericInput(session, "amount", value = 0)  # 重置金额
    updateRadioButtons(session, "transaction_type", selected = "out")  # 重置为“转出”
    updateDateInput(session, "custom_date", value = Sys.Date())  # 重置为当前日期
    updateTimeInput(session, "custom_time", value = format(Sys.time(), "%H:%M:%S"))  # 重置为当前时间
    updateTextAreaInput(session, "remarks", value = "")  # 清空备注
    image_transactions$reset()  # 重置图片上传组件
  }
  
  # 分页切换更新
  observe({
    if (input$transaction_tabs == "账户余额总览") {
      updateAccountOverview()
    }
    if (input$transaction_tabs == "工资卡") {
      refreshTransactionTable("工资卡")
      resetToCreateMode()
      resetTransactionForm(session)
    }
    if (input$transaction_tabs == "美元卡") {
      refreshTransactionTable("美元卡")
      resetToCreateMode()
      resetTransactionForm(session)
    }
    if (input$transaction_tabs == "买货卡") {
      refreshTransactionTable("买货卡")
      resetToCreateMode()
      resetTransactionForm(session)
    }
    if (input$transaction_tabs == "一般户卡") {
      refreshTransactionTable("一般户卡")
      resetToCreateMode()
      resetTransactionForm(session)
    }
  })
  
  # 登记转账记录
  observeEvent(input$record_transaction, {
    req(!is.null(input$amount), input$amount > 0, !is.null(input$transaction_type))
    
    # 确定账户类型
    account_type <- switch(
      input$transaction_tabs,
      "工资卡" = "工资卡",
      "美元卡" = "美元卡",
      "买货卡" = "买货卡",
      "一般户卡" = "一般户卡",
      NULL
    )
    
    if (is.null(account_type)) {
      showNotification("请选择有效的账户类型！", type = "error")
      return()
    }
    
    # 合并用户选择的日期和时间为完整时间戳
    transaction_time <- format(as.POSIXct(input$custom_time, format = "%H:%M:%S"), "%H:%M:%S")
    transaction_date <- paste(input$custom_date, transaction_time)
    transaction_datetime <- as.POSIXct(transaction_date, format = "%Y-%m-%d %H:%M:%S")
    
    # 生成 12 位 TransactionID
    transaction_id <- generate_transaction_id(account_type, input$amount, input$remarks, transaction_datetime)
    
    # 区分“登记”和“更新”模式
    if (is_update_mode()) {
      image_path <- process_image_upload(
        sku = selected_TransactionID(),
        file_data = image_transactions$uploaded_file(),
        pasted_data = image_transactions$pasted_file(),
        inventory_path = selected_TransactionImagePath(),
      )
      
      if (is.null(image_path) || length(image_path) != 1) {
        image_path <- ""  # 设置默认值
      }
      
      tryCatch({
        dbExecute(
          con,
          "UPDATE transactions 
         SET Amount = ?, Remarks = ?, TransactionTime = ?, TransactionImagePath = ?
         WHERE TransactionID = ?",
          params = list(
            ifelse(input$transaction_type == "in", input$amount, -input$amount),
            input$remarks,
            transaction_datetime,
            image_path,
            transaction_id
          )
        )
        showNotification("记录更新成功！", type = "message")
        
        update_balance(account_type, con)
        
        resetToCreateMode() # 重置为“登记”模式
        resetTransactionForm(session) # 重置输入框
        
        # 自动更新账户余额和表格
        updateAccountOverview()
        refreshTransactionTable(account_type)
      }, error = function(e) {
        showNotification(paste("更新失败：", e$message), type = "error")
      })
    } else {
      # 登记逻辑
      tryCatch({
        # 插入交易记录
        transaction_id <- generate_transaction_id(account_type, input$amount, input$remarks, transaction_datetime)
        dbExecute(
          con,
          "INSERT INTO transactions (TransactionID, AccountType, Amount, Remarks, TransactionImagePath, TransactionTime) VALUES (?, ?, ?, ?, ?, ?)",
          params = list(
            transaction_id,
            account_type,
            ifelse(input$transaction_type == "in", input$amount, -input$amount),
            input$remarks,
            process_image_upload(
              sku = transaction_id,
              file_data = image_transactions$uploaded_file(),
              pasted_data = image_transactions$pasted_file()
            ),
            transaction_datetime
          )
        )
        showNotification("记录登记成功！", type = "message")
        
        # 检查是否为最新记录
        latest_time <- dbGetQuery(
          con,
          "SELECT MAX(TransactionTime) AS LatestTime FROM transactions WHERE AccountType = ?",
          params = list(account_type)
        )$LatestTime[1]
        
        if (!is.null(latest_time) && transaction_datetime < as.POSIXct(latest_time)) {
          # 如果插入记录不是最新的，则重新计算余额
          update_balance(account_type, con)
        }
        
        resetTransactionForm(session) # 重置输入框
        
        # 自动更新账户余额和表格
        updateAccountOverview()
        refreshTransactionTable(account_type)
      }, error = function(e) {
        showNotification(paste("登记失败：", e$message), type = "error")
      })
    }
  })
  
  # 删除转账记录
  observeEvent(input$delete_transaction, {
    current_tab <- input$transaction_tabs
    
    account_type <- getAccountType(input)
    
    if (is.null(account_type)) {
      showNotification("请选择有效的账户类型！", type = "error")
      return()
    }
    
    # 获取选中的行
    selected_rows <- switch(
      current_tab,
      "工资卡" = input$salary_card_table_rows_selected,
      "美元卡" = input$dollar_card_table_rows_selected,
      "买货卡" = input$purchase_card_table_rows_selected,
      "一般户卡" = input$general_card_table_rows_selected
    )
    
    if (length(selected_rows) > 0) {
      # 手动构造 LIMIT 的参数
      row_index <- selected_rows - 1
      
      # 查询选中记录的 TransactionID
      query <- sprintf(
        "SELECT TransactionID FROM transactions WHERE AccountType = '%s' ORDER BY TransactionTime DESC LIMIT %d, 1",
        account_type, row_index
      )
      record_to_delete <- dbGetQuery(con, query)
      
      if (nrow(record_to_delete) > 0) {
        tryCatch({
          # 删除选中的记录
          dbExecute(con, "DELETE FROM transactions WHERE TransactionID = ?", params = list(record_to_delete$TransactionID))
          
          # 重新计算所有balance记录
          update_balance(account_type, con)
          
          # 自动刷新账户余额总览统计
          updateAccountOverview()
          
          # 自动刷新表格
          refreshTransactionTable(account_type)
        }, error = function(e) {
          showNotification(paste("删除失败：", e$message), type = "error")
        })
      } else {
        showNotification("无法找到选中的记录！", type = "error")
      }
    } else {
      showNotification("请选择要删除的记录", type = "error")
    }
    
    resetToCreateMode() # 重置为“登记”模式
    resetTransactionForm(session) # 重置输入框
  })
  
  # 资金转移
  observeEvent(input$record_transfer, {
    req(!is.null(input$transfer_amount), input$transfer_amount > 0)
    req(!is.null(input$from_account), !is.null(input$to_account))
    
    if (input$from_account == input$to_account) {
      showNotification("转出账户和转入账户不能相同！", type = "error")
      return()
    }
    
    # 动态生成备注信息
    transfer_remarks_from <- paste0("[转出至 ", input$to_account, "] ", input$transfer_remarks)
    transfer_remarks_to <- paste0("[从 ", input$from_account, " 转入] ", input$transfer_remarks)
    
    tryCatch({
      # 插入转出记录
      dbExecute(
        con,
        "INSERT INTO transactions (AccountType, Amount, Remarks, TransactionTime) VALUES (?, ?, ?, ?)",
        params = list(input$from_account, -input$transfer_amount, transfer_remarks_from, Sys.time())
      )
      
      # 插入转入记录
      dbExecute(
        con,
        "INSERT INTO transactions (AccountType, Amount, Remarks, TransactionTime) VALUES (?, ?, ?, ?)",
        params = list(input$to_account, input$transfer_amount, transfer_remarks_to, Sys.time())
      )
      
      showNotification("资金转移记录成功！", type = "message")
      
      # 自动更新账户余额
      updateAccountOverview()
      
      # 自动刷新表格
      refreshTransactionTable(input$from_account)
      refreshTransactionTable(input$to_account)
      
      # 清空表单
      updateNumericInput(session, "transfer_amount", value = NULL)  # 清空金额输入框
      updateSelectInput(session, "from_account", selected = "美元卡")  # 重置转出账户
      updateSelectInput(session, "to_account", selected = NULL)    # 重置转入账户
      updateTextAreaInput(session, "transfer_remarks", value = "") # 清空备注输入框
    }, error = function(e) {
      showNotification(paste("资金转移失败：", e$message), type = "error")
    })
  })
  
  # 转账证据图片处理模块
  image_transactions <- imageModuleServer("image_transactions")
  
  # 重置
  observeEvent(input$reset_form, {
    resetToCreateMode() # 重置为“登记”模式
    resetTransactionForm(session) # 重置输入框
    showNotification("表单已重置！", type = "message")
  })
  
  
  is_update_mode <- reactiveVal(FALSE)  # 初始化为登记模式
  selected_TransactionID  <- reactiveVal(NULL)  # 存储选中的记录 ID
  selected_TransactionImagePath <- reactiveVal(NULL)  # 存储选中的记录图片路径
  
  # 监听 工资卡 点选
  observeEvent(input$salary_card_table_rows_selected, {
    selected_row <- input$salary_card_table_rows_selected
    
    if (!is.null(selected_row)) {
      # 获取选中行的数据
      fetchData <- fetchInputFromTable("工资卡", selected_row)
      selected_TransactionID(fetchData$TransactionID)
      selected_TransactionImagePath(fetchData$TransactionImagePath)
      
      # 切换按钮为“更新”
      is_update_mode(TRUE)
      updateActionButton(session, "record_transaction", label = "更新", icon = icon("edit"))
      
      showNotification("信息已加载，准备更新记录", type = "message")
    }
  })
  
  # 监听 美元卡 点选
  observeEvent(input$dollar_card_table_rows_selected, {
    selected_row <- input$dollar_card_table_rows_selected
    
    if (!is.null(selected_row)) {
      # 获取选中行的数据
      fetchData <- fetchInputFromTable("美元卡", input$dollar_card_table_rows_selected)
      selected_TransactionID(fetchData$TransactionID)
      selected_TransactionImagePath(fetchData$TransactionImagePath)
      
      # 切换按钮为“更新”
      is_update_mode(TRUE)
      updateActionButton(session, "record_transaction", label = "更新", icon = icon("edit"))
      
      showNotification("信息已加载，准备更新记录", type = "message")
    }
  })
  
  # 监听 买货卡 点选
  observeEvent(input$purchase_card_table_rows_selected, {
    selected_row <- input$purchase_card_table_rows_selected
    
    if (!is.null(selected_row)) {
      # 获取选中行的数据
      fetchData <- fetchInputFromTable("买货卡", input$purchase_card_table_rows_selected)
      selected_TransactionID(fetchData$TransactionID)
      selected_TransactionImagePath(fetchData$TransactionImagePath)
      
      # 切换按钮为“更新”
      is_update_mode(TRUE)
      updateActionButton(session, "record_transaction", label = "更新", icon = icon("edit"))
      
      showNotification("信息已加载，准备更新记录", type = "message")
    }
  })
  
  # 监听 一般户卡 点选
  observeEvent(input$general_card_table_rows_selected, {
    selected_row <- input$general_card_table_rows_selected
    
    if (!is.null(selected_row)) {
      # 获取选中行的数据
      fetchData <- fetchInputFromTable("一般户卡", input$general_card_table_rows_selected)
      selected_TransactionID(fetchData$TransactionID)
      selected_TransactionImagePath(fetchData$TransactionImagePath)
      
      # 切换按钮为“更新”
      is_update_mode(TRUE)
      updateActionButton(session, "record_transaction", label = "更新", icon = icon("edit"))
      
      showNotification("信息已加载，准备更新记录", type = "message")
    }
  })
  
  # 处理工资卡表格的图片点击
  handleTransactionImageClick("工资卡", "salary_card_table", 4)
  
  # 处理美元卡表格的图片点击
  handleTransactionImageClick("美元卡", "dollar_card_table", 4)
  
  # 处理买货卡表格的图片点击
  handleTransactionImageClick("买货卡", "purchase_card_table", 4)
  
  # 处理一般户卡表格的图片点击
  handleTransactionImageClick("一般户卡", "general_card_table", 4)

  ################################################################
  ##                                                            ##
  ## 查询分页                                                   ##
  ##                                                            ##
  ################################################################
  
  # 监听主页面和子页面的切换
  observeEvent({
    list(input$inventory_china, input$query_tabs)  # 仅在这些输入发生变化时触发
  }, {
    if (input$inventory_china == "查询" && input$query_tabs == "商品状态") {
      inventory_refresh_trigger(!inventory_refresh_trigger())
      showNotification("库存表已加载！", type = "message")
    }
    
    if (input$inventory_china == "查询" && input$query_tabs == "库存总览") {
      item_status_history_refresh_trigger(!item_status_history_refresh_trigger())
      showNotification("库存状态历史已加载！", type = "message")
    }
  }, ignoreInit = TRUE)  # 忽略初始值
  
  # 物品表过滤模块
  itemFilterServer(
    id = "query_filter",
    makers_items_map = makers_items_map
  )
  
  # 根据SKU产生图表
  observe({
    sku <- trimws(input$query_sku)
    
    if (sku == "") {
      output$query_item_info <- renderUI({ div() })
      output$inventory_status_chart <- renderPlotly({ NULL })
      output$defect_status_chart <- renderPlotly({ NULL })
      return()
    }
    
    tryCatch({
      sku_data <- inventory() %>% filter(SKU == sku)
      
      if (nrow(sku_data) == 0) {
        output$query_item_info <- renderUI({
          div(tags$p("未找到该 SKU 对应的商品信息！", style = "color: red; font-size: 16px;"))
        })
        return()
      }
      
      output$query_item_info <- renderUI({
        img_path <- ifelse(
          is.na(sku_data$ItemImagePath[1]),
          placeholder_150px_path,
          paste0(host_url, "/images/", basename(sku_data$ItemImagePath[1]))
        )

        # 从 unique_items_data() 中计算额外信息
        sku_stats <- unique_items_data() %>%
          filter(SKU == sku) %>%
          summarise(
            美国库存数 = sum(Status == "美国入库", na.rm = TRUE),
            在途库存数 = sum(Status == "国内出库", na.rm = TRUE),
            国内库存数 = sum(Status == "国内入库", na.rm = TRUE),
            已售库存数 = sum(Status %in% c("国内售出", "美国售出", "美国调货", "美国发货"), na.rm = TRUE)
          )
      
        # 渲染图片和表格信息
        div(
          style = "display: flex; flex-direction: column; align-items: center; padding: 10px;",
          div(
            style = "text-align: center; margin-bottom: 10px;",
            tags$img(src = img_path, height = "150px", style = "border: 1px solid #ddd; border-radius: 8px;")
          ),
          div(
            style = "width: 100%; padding-left: 10px;",
            tags$table(
              style = "width: 100%; border-collapse: collapse;",
              tags$tr(tags$td(tags$b("商品名称：")), tags$td(sku_data$ItemName[1])),
              tags$tr(tags$td(tags$b("供应商：")), tags$td(sku_data$Maker[1])),
              tags$tr(tags$td(tags$b("分类：")), tags$td(paste(sku_data$MajorType[1], "/", sku_data$MinorType[1]))),
              tags$tr(tags$td(tags$b("平均成本：")), tags$td(sprintf("¥%.2f", sku_data$ProductCost[1]))),
              tags$tr(tags$td(tags$b("平均运费：")), tags$td(sprintf("¥%.2f", sku_data$ShippingCost[1]))),
              tags$tr(tags$td(tags$b("国内库存数：")), tags$td(sku_stats$国内库存数)),
              tags$tr(tags$td(tags$b("在途库存数：")), tags$td(sku_stats$在途库存数)),
              tags$tr(tags$td(tags$b("美国库存数：")), tags$td(sku_stats$美国库存数)),
              tags$tr(tags$td(tags$b("已售库存数：")), tags$td(sku_stats$已售库存数)),
              tags$tr(tags$td(tags$b("总库存数：")), tags$td(sku_data$Quantity[1]))
            )
          )
        )
      })
      
      # 渲染库存状态图表
      output$inventory_status_chart <- renderPlotly({
        tryCatch({
          data <- unique_items_data()
          
          if (is.null(input$query_sku) || trimws(input$query_sku) == "") {
            return(NULL)
          }
          
          # 筛选符合条件的数据
          inventory_status_data <- data %>%
            filter(SKU == trimws(input$query_sku)) %>%
            group_by(Status) %>%
            summarise(Count = n(), .groups = "drop")
          
          # 定义固定类别顺序和颜色
          status_levels <- c("采购", "国内入库", "国内售出", "国内出库", "美国入库", "美国调货", "美国售出", "美国发货", "退货")
          status_colors <- c("lightgray", "#c7e89b", "#9ca695", "#46a80d", "#6f52ff", "#529aff", "#869bb8", "#faf0d4", "red")
          
          # 确保数据按照固定类别顺序排列，并用 0 填充缺失类别
          inventory_status_data <- merge(
            data.frame(Status = status_levels),
            inventory_status_data,
            by = "Status",
            all.x = TRUE
          )
          inventory_status_data$Count[is.na(inventory_status_data$Count)] <- 0
          
          # 按 status_levels 排序，确保颜色对应
          inventory_status_data <- inventory_status_data[match(status_levels, inventory_status_data$Status), ]
          
          if (sum(inventory_status_data$Count) == 0) {
            # 如果没有数据，显示占位图
            plot_ly(type = "pie", labels = c("无数据"), values = c(1), textinfo = "label+value")
          } else {
            # 渲染饼图
            plot_ly(
              data = inventory_status_data,
              labels = ~Status,
              values = ~Count,
              type = "pie",
              textinfo = "label+value",       # 图上显示类别和数量
              hoverinfo = "label+percent+value", # 鼠标悬停显示类别、百分比和数量
              insidetextorientation = "auto", # 自动调整标签方向
              textposition = "inside",       # 标签显示在图形外部
              marker = list(colors = status_colors) # 按固定颜色映射
            ) %>%
              layout(
                showlegend = FALSE, # 隐藏图例
                margin = list(l = 20, r = 20, t = 30, b = 30), # 增加边距
                uniformtext = list(minsize = 10, mode = "hide") # 统一文本大小
              )
          }
        }, error = function(e) {
          showNotification(paste("库存状态图表生成错误：", e$message), type = "error")
          output$inventory_status_chart <- renderPlotly({ NULL })
        })
      })
      
      # 渲染瑕疵情况图表
      output$defect_status_chart <- renderPlotly({
        tryCatch({
          data <- unique_items_data()
          
          if (is.null(input$query_sku) || trimws(input$query_sku) == "") {
            return(NULL)
          }
          
          # 筛选符合条件的数据
          defect_status_data <- data %>%
            filter(SKU == trimws(input$query_sku)) %>%
            group_by(Defect) %>%
            summarise(Count = n(), .groups = "drop")
          
          # 定义固定类别顺序和颜色
          defect_levels <- c("未知", "无瑕", "瑕疵", "修复")
          defect_colors <- c("darkgray", "green", "red", "orange")
          
          # 确保数据按照固定类别顺序排列，并用 0 填充缺失类别
          defect_status_data <- merge(
            data.frame(Defect = defect_levels),
            defect_status_data,
            by = "Defect",
            all.x = TRUE
          )
          defect_status_data$Count[is.na(defect_status_data$Count)] <- 0
          
          # 按 defect_levels 排序，确保颜色对应
          defect_status_data <- defect_status_data[match(defect_levels, defect_status_data$Defect), ]
          
          if (sum(defect_status_data$Count) == 0) {
            # 如果没有数据，显示占位图
            plot_ly(type = "pie", labels = c("无数据"), values = c(1), textinfo = "label+value")
          } else {
            # 渲染饼图
            plot_ly(
              data = defect_status_data,
              labels = ~Defect,
              values = ~Count,
              type = "pie",
              textinfo = "label+value",       # 图上显示类别和数量
              hoverinfo = "label+percent+value", # 鼠标悬停显示类别、百分比和数量
              insidetextorientation = "auto", # 自动调整标签方向
              textposition = "inside",       # 标签显示在图形外部
              marker = list(colors = defect_colors) # 按固定颜色映射
            ) %>%
              layout(
                showlegend = FALSE, # 隐藏图例
                margin = list(l = 20, r = 20, t = 30, b = 30), # 增加边距
                uniformtext = list(minsize = 10, mode = "hide") # 统一文本大小
              )
          }
        }, error = function(e) {
          showNotification(paste("瑕疵情况图表生成错误：", e$message), type = "error")
          output$defect_status_chart <- renderPlotly({ NULL })
        })
      })
      
    }, error = function(e) {
      showNotification(paste("发生错误：", e$message), type = "error")
    })
