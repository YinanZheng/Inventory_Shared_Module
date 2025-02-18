      # 提取所有选中行的国际物流单号（IntlTracking）
      unique_tracking_numbers <- unique(selected_data$IntlTracking)
      
      # 检查选中的物品是否已经挂靠国际运单
      if (any(!is.na(selected_data$IntlTracking))) {
        # 如果所有物品都未挂靠国际运单
        if (length(unique_tracking_numbers) == 1 && !is.na(unique_tracking_numbers)) {
          # 如果只有一个唯一的物流单号，填写到输入框
          updateTextInput(session, "intl_tracking_number", value = unique_tracking_numbers)
          showNotification("已根据选中行填写运单号！", type = "message")
        } else {
          # 如果没有唯一物流单号，取最新点击的那个
          updateTextInput(session, "intl_tracking_number", value = selected_data$IntlTracking[nrow(selected_data)])
        }
        # 如果选中物品中存在已挂靠国际运单的物品
        shinyjs::disable("link_tracking_btn")  # 禁用按钮
        shinyjs::enable("unlink_tracking_btn")  # 启用按钮
      } else {
        # 如果所有物品都未挂靠国际运单
        shinyjs::enable("link_tracking_btn")  # 启用按钮
        shinyjs::disable("unlink_tracking_btn")  # 禁用按钮
      }
    }, error = function(e) {
      # 捕获错误并提示
      shinyjs::disable("link_tracking_btn")  # 禁用按钮
      shinyjs::disable("unlink_tracking_btn")  # 禁用按钮
      showNotification(paste("操作失败：", e$message), type = "error")
    })
  })
  
  
  
  ######################
  ### 挂靠管理分页
  ######################
  
  # 监听页面切换事件
  observeEvent(input$intl_shipment_tabs, {
    if (input$intl_shipment_tabs == "link_management") {
      tryCatch({
        # 查询数据库中状态为“运单新建”的最新运单
        latest_shipment <- dbGetQuery(
          con,
          "SELECT TrackingNumber
         FROM intl_shipments
         WHERE Status = '运单创建'
         ORDER BY CreatedAt DESC
         LIMIT 1"
        )

        if (nrow(latest_shipment) > 0) {
          # 填写到 intl_link_tracking_number
          updateTextInput(session, "intl_link_tracking_number", value = latest_shipment$TrackingNumber[1])
          showNotification("已自动填充最新的‘运单创建’状态的运单号！", type = "message")
        } else {
          # 未找到符合条件的运单
          updateTextInput(session, "intl_link_tracking_number", value = "")
          showNotification("未找到状态为‘运单创建’的运单！", type = "warning")
        }
      }, error = function(e) {
        # 捕获错误并提示
        showNotification(paste("检查运单状态时发生错误：", e$message), type = "error")
      })
    }
  })

  # 监听待挂靠运单号输入
  observeEvent(input$intl_link_tracking_number, {
    tracking_number <- input$intl_link_tracking_number  # 获取用户输入的运单号
    
    if (is.null(tracking_number) || tracking_number == "") {
      shinyjs::disable("link_tracking_btn")  # 禁用按钮
      shinyjs::disable("unlink_tracking_btn")  # 禁用按钮
      
      output$intl_link_display <- renderText({
        "请输入运单号以查看运单信息"
      })
      return()
    }
    
    tryCatch({
      # 查询运单信息
      shipment_info <- dbGetQuery(
        con,
        "SELECT Status, TotalCost, ShippingMethod, CreatedAt FROM intl_shipments WHERE TrackingNumber = ?",
        params = list(tracking_number)
      )
      
      if (nrow(shipment_info) == 0) {
        shinyjs::disable("link_tracking_btn")  # 禁用按钮
        shinyjs::disable("unlink_tracking_btn")  # 禁用按钮
        
        output$intl_link_display <- renderText({
          "未找到对应的运单信息，请检查"
        })
        return()
      }
      
      # 显示运单状态和运费
      output$intl_link_display <- renderUI({
        HTML(paste0(
          "物流状态:   ", shipment_info$Status[1], "<br>",
          "运输方式：  ", shipment_info$ShippingMethod[1], "<br>",
          "国际运费:   ￥", format(shipment_info$TotalCost[1], big.mark = ",", nsmall = 2), "<br>",
          "创建日期:   ", format(as.Date(shipment_info$CreatedAt[1]), "%Y-%m-%d")
        ))
      })
    }, error = function(e) {
      output$intl_link_display <- renderText({
        paste("查询运单信息失败：", e$message)
      })
    })
  })
  
  # 挂靠运单号逻辑
  observeEvent(input$link_tracking_btn, {
    tracking_number <- input$intl_link_tracking_number  # 获取用户输入的运单号
    selected_rows <- unique_items_table_logistics_selected_row()  # 获取用户选择的物品行
    
    if (is.null(selected_rows) || length(selected_rows) == 0) {
      showNotification("请先选择需要挂靠的物品行！", type = "error")
      return()
    }
    
    if (is.null(tracking_number) || tracking_number == "") {
      showNotification("运单号不能为空！", type = "error")
      return()
    }
    
    tryCatch({
      # 获取选中行的物品数据
      selected_items <- filtered_unique_items_data_logistics()[selected_rows, ]
      
      # 更新挂靠信息
      dbBegin(con)
      for (i in seq_len(nrow(selected_items))) {
        dbExecute(
          con,
          "UPDATE unique_items SET IntlTracking = ? WHERE UniqueID = ?",
          params = list(tracking_number, selected_items$UniqueID[i])
        )
      }
      
      # 查询挂靠到该运单的所有物品
      related_items <- dbGetQuery(
        con,
        "SELECT UniqueID FROM unique_items WHERE IntlTracking = ?",
        params = list(tracking_number)
      )
      
      if (nrow(related_items) == 0) {
        showNotification("当前运单号没有关联的物品！", type = "warning")
        dbRollback(con)
        return()
      }
      
      # 计算平摊运费
      shipment_info <- dbGetQuery(
        con,
        "SELECT TotalCost FROM intl_shipments WHERE TrackingNumber = ?",
        params = list(tracking_number)
      )
      total_cost <- as.numeric(shipment_info$TotalCost)
      per_item_cost <- total_cost / nrow(related_items)
      
      # 更新平摊运费
      dbExecute(
        con,
        "UPDATE unique_items SET IntlShippingCost = ? WHERE IntlTracking = ?",
        params = list(per_item_cost, tracking_number)
      )
      dbCommit(con)
      
      showNotification("运单号挂靠成功，平摊运费已更新！", type = "message")
    }, error = function(e) {
      dbRollback(con)
      showNotification(paste("挂靠失败：", e$message), type = "error")
    })
  })
  
  # 解除运单号挂靠逻辑
  observeEvent(input$unlink_tracking_btn, {
    selected_rows <- unique_items_table_logistics_selected_row()  # 获取用户选择的物品行
    tracking_number <- input$intl_link_tracking_number  # 获取用户输入的运单号
    
    # 校验用户选择的物品行
    if (is.null(selected_rows) || length(selected_rows) == 0) {
      showNotification("请先选择需要解除挂靠的物品行！", type = "error")
      return()
    }
    
    # 校验运单号
    if (is.null(tracking_number) || tracking_number == "") {
      showNotification("运单号不能为空！", type = "error")
      return()
    }
    
    tryCatch({
      # 查询运单信息
      shipment_info <- dbGetQuery(
        con,
        "SELECT TotalCost FROM intl_shipments WHERE TrackingNumber = ?",
        params = list(tracking_number)
      )
      
      if (nrow(shipment_info) == 0) {
        showNotification("未找到对应的运单信息，请检查输入的运单号！", type = "error")
        return()
      }
      
      # 获取选中行的物品数据
      selected_items <- filtered_unique_items_data_logistics()[selected_rows, ]
      selected_tracking_numbers <- unique(na.omit(selected_items$IntlTracking))
      
      # 开启事务处理
      dbBegin(con)
      
      # 批量解除挂靠并清零运费
      dbExecute(
        con,
          "UPDATE unique_items 
         SET IntlTracking = NULL, IntlShippingCost = 0.00 
         WHERE UniqueID IN (?)",
        params = list(selected_items$UniqueID)
      )
      
      # 重新计算剩余挂靠物品的平摊运费
      dbExecute(
        con, "
        UPDATE unique_items ui
        JOIN (
          SELECT IntlTracking, TotalCost / COUNT(*) AS PerItemCost
          FROM unique_items
          JOIN intl_shipments ON unique_items.IntlTracking = intl_shipments.TrackingNumber
          WHERE IntlTracking IN (?)
          GROUP BY IntlTracking
        ) calc ON ui.IntlTracking = calc.IntlTracking
        SET ui.IntlShippingCost = calc.PerItemCost",

        params = list(selected_tracking_numbers)
      )
      
      # 提交事务
      dbCommit(con)
      
      showNotification("运单号已成功解除挂靠，相关物品的平摊运费已重新计算！", type = "message")
    }, error = function(e) {
      # 回滚事务
      dbRollback(con)
      showNotification(paste("解除挂靠失败：", e$message), type = "error")
    })
  })
  
  
  
  ################################################################
  ##                                                            ##
  ## 账务管理分页                                               ##
  ##                                                            ##
  ################################################################
  
  is_update_mode <- reactiveVal(FALSE)  # 初始化为登记模式
  selected_TransactionID  <- reactiveVal(NULL)  # 存储选中的记录 ID
  selected_TransactionImagePath <- reactiveVal(NULL)  # 存储选中的记录图片路径
  
  # 初始化全局缓存，用于存储各账户的哈希值
  transaction_table_hash <- reactiveValues(
    salary = NULL,
    dollar = NULL,
    purchase = NULL,
    general = NULL
  )
  
  # 定义转账种类说明映射
  category_notes <- list(
    "采购" = "记录购买商品与相关运费的支出",
    "税费" = "包括会计费，公司税务等法定税款",
    "杂费" = "各种运营支出，例如包装材料费、网费等",
    "工资" = "员工薪资、劳务费等支付",
    "债务" = "记录公司借款还款",
    "社保" = "社保、公积金等相关转账",
    "图解" = "记录购买图解的支出",
    "其他" = "其他无法归类的交易"
  )
  
  # 账务登记的种类说明
  output$transaction_category_note <- renderText({
    category_notes[[input$transaction_category]] %||% ""
  })
  
  # 资金转移的种类说明
  output$transfer_category_note <- renderText({
    category_notes[[input$transfer_category]] %||% ""
  })
  
  # 分页切换更新
  observe({
    if (input$transaction_tabs == "账户余额总览") {
      updateAccountOverview(output, con)
    }
    
    account_type <- switch(
      input$transaction_tabs,
      "工资卡" = "工资卡",
      "美元卡" = "美元卡",
      "买货卡" = "买货卡",
      "一般户卡" = "一般户卡"
    )
    
    if (!is.null(account_type)) {
      refreshTransactionTable(account_type, cache_env, transaction_table_hash, output, con)  # 优化后的表格刷新
      resetToCreateMode(is_update_mode, selected_TransactionID, selected_TransactionImagePath, session)
      resetTransactionForm(session, image_transactions) # 重置输入框
      resetTransferForm(session, image_transfer) # 重置输入框
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
         SET Amount = ?, Remarks = ?, TransactionTime = ?, TransactionImagePath = ?, TransactionType = ?
         WHERE TransactionID = ?",
          params = list(
            ifelse(input$transaction_type == "in", input$amount, -input$amount),
            input$remarks,
            transaction_datetime,
            image_path,
            input$transaction_category,
            transaction_id
          )
        )
        showNotification("记录更新成功！", type = "message")
        
        update_balance(account_type, con)
        
        resetToCreateMode(is_update_mode, selected_TransactionID, selected_TransactionImagePath, session)
        resetTransactionForm(session, image_transactions) # 重置输入框
        
        # 自动更新账户余额和表格
        updateAccountOverview(output, con)
        refreshTransactionTable(account_type, cache_env, transaction_table_hash, output, con)
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
          "INSERT INTO transactions (TransactionID, AccountType, TransactionType, Amount, Remarks, TransactionImagePath, TransactionTime) VALUES (?, ?, ?, ?, ?, ?, ?)",
          params = list(
            transaction_id,
            account_type,
            input$transaction_category,
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
        
        resetTransactionForm(session, image_transactions)
        
        # 自动更新账户余额和表格
        updateAccountOverview(output, con)
        refreshTransactionTable(account_type, cache_env, transaction_table_hash, output, con)
      }, error = function(e) {
        showNotification(paste("登记失败：", e$message), type = "error")
      })
    }
  })
  
  # 登记资金转移记录
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
    
    # 处理图片上传
    transfer_image_path <- process_image_upload(
      sku = paste0(input$from_account, "_", input$to_account, "_", Sys.time()), # 用账户和时间生成唯一标识
      file_data = image_transfer$uploaded_file(),
      pasted_data = image_transfer$pasted_file()
    )
    
    if (is.null(transfer_image_path) || transfer_image_path == "") {
      transfer_image_path <- NULL  # 如果未上传图片，设置为 NULL
    }
    
    tryCatch({
      # 生成 TransactionID
      transaction_id_from <- generate_transaction_id(
        account_type = input$from_account,
        amount = -input$transfer_amount,
        remarks = transfer_remarks_from,
        transaction_datetime = Sys.time()
      )
      
      transaction_id_to <- generate_transaction_id(
        account_type = input$to_account,
        amount = input$transfer_amount,
        remarks = transfer_remarks_to,
        transaction_datetime = Sys.time()
      )
      
      # 插入转出记录
      dbExecute(
        con,
        "INSERT INTO transactions (TransactionID, AccountType, TransactionType, Amount, Remarks, TransactionImagePath, TransactionTime) 
       VALUES (?, ?, ?, ?, ?, ?, ?)",
        params = list(transaction_id_from, input$from_account, input$transfer_category, -input$transfer_amount, transfer_remarks_from, transfer_image_path, Sys.time())
      )
      
      # 插入转入记录
      dbExecute(
        con,
        "INSERT INTO transactions (TransactionID, AccountType, TransactionType, Amount, Remarks, TransactionImagePath, TransactionTime) 
       VALUES (?, ?, ?, ?, ?, ?, ?)",
        params = list(transaction_id_to, input$to_account, input$transfer_category, input$transfer_amount, transfer_remarks_to, transfer_image_path, Sys.time())
      )
      
      showNotification("资金转移记录成功！", type = "message")
      
      # 自动更新账户余额
      updateAccountOverview(output, con)
      
      # 自动刷新表格
      refreshTransactionTable(input$from_account, cache_env, transaction_table_hash, output, con)
      refreshTransactionTable(input$to_account, cache_env, transaction_table_hash, output, con)
      
      # 清空表单
      resetTransferForm(session, image_transfer) # 重置输入框
      
    }, error = function(e) {
      showNotification(paste("资金转移失败：", e$message), type = "error")
    })
  })
  
  # 删除转账记录 (登记)
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
          updateAccountOverview(output, con)
          
          # 自动刷新表格
          refreshTransactionTable(account_type, cache_env, transaction_table_hash, output, con)
        }, error = function(e) {
          showNotification(paste("删除失败：", e$message), type = "error")
        })
      } else {
        showNotification("无法找到选中的记录！", type = "error")
      }
    } else {
      showNotification("请选择要删除的记录", type = "error")
    }
    
    resetToCreateMode(is_update_mode, selected_TransactionID, selected_TransactionImagePath, session)
    resetTransactionForm(session, image_transactions) # 重置输入框
  })
  
  # 删除转账记录 (转移)
  observeEvent(input$delete_transfer, {
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
          updateAccountOverview(output, con)
          
          # 自动刷新表格
          refreshTransactionTable(account_type, cache_env, transaction_table_hash, output, con)
        }, error = function(e) {
          showNotification(paste("删除失败：", e$message), type = "error")
        })
      } else {
        showNotification("无法找到选中的记录！", type = "error")
      }
    } else {
      showNotification("请选择要删除的记录", type = "error")
    }
    
    resetTransferForm(session, image_transfer) # 重置输入框
  })
  
  # 转账证据图片处理模块 (登记)
  image_transactions <- imageModuleServer("image_transactions")
  
  # 转账证据图片处理模块 (转移)
  image_transfer <- imageModuleServer("image_transfer")
  
  # 重置 (登记)
  observeEvent(input$reset_form, {
    resetToCreateMode(is_update_mode, selected_TransactionID, selected_TransactionImagePath, session)
    resetTransactionForm(session, image_transactions) # 重置输入框
    showNotification("表单已重置！", type = "message")
  })
  
  # 重置 (转移)
  observeEvent(input$reset_form_transfer, {
    resetTransferForm(session, image_transfer) # 重置输入框
    showNotification("表单已重置！", type = "message")
  })
  
  ####

  # 监听 工资卡 点选
  observeEvent(input$salary_card_table_rows_selected, {
    selected_row <- input$salary_card_table_rows_selected
    
    if (!is.null(selected_row)) {
      # 获取选中行的数据
      fetchData <- fetchInputFromTable("工资卡", selected_row, cache_env, con, session)
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
      fetchData <- fetchInputFromTable("美元卡", input$dollar_card_table_rows_selected, cache_env, con, session)
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
      fetchData <- fetchInputFromTable("买货卡", input$purchase_card_table_rows_selected, cache_env, con, session)
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
      fetchData <- fetchInputFromTable("一般户卡", input$general_card_table_rows_selected, cache_env, con, session)
      selected_TransactionID(fetchData$TransactionID)
      selected_TransactionImagePath(fetchData$TransactionImagePath)
      
      # 切换按钮为“更新”
      is_update_mode(TRUE)
      updateActionButton(session, "record_transaction", label = "更新", icon = icon("edit"))
      
      showNotification("信息已加载，准备更新记录", type = "message")
    }
  })
  
  ####
  
  # 处理工资卡表格的图片点击
  handleTransactionImageClick("工资卡", "salary_card_table", 5, input, cache_env, con, session)
  
  # 处理美元卡表格的图片点击
  handleTransactionImageClick("美元卡", "dollar_card_table", 5, input, cache_env, con, session)
  
  # 处理买货卡表格的图片点击
  handleTransactionImageClick("买货卡", "purchase_card_table", 5, input, cache_env, con, session)
  
  # 处理一般户卡表格的图片点击
  handleTransactionImageClick("一般户卡", "general_card_table", 5, input, cache_env, con, session)

  
  
  ################################################################
  ##                                                            ##
  ## 查询分页                                                   ##
  ##                                                            ##
  ################################################################
  
  # 监听主页面和子页面的切换
  observeEvent({
    list(input$inventory_cn, input$query_tabs)  # 仅在这些输入发生变化时触发
  }, {
    if (input$inventory_cn == "查询" && input$query_tabs == "商品状态") {
      inventory_refresh_trigger(!inventory_refresh_trigger())
      showNotification("库存表已加载！", type = "message")
    }
  }, ignoreInit = TRUE)  # 忽略初始值
  
  # 物品表过滤模块
  itemFilterServer(
    id = "query_filter",
    makers_items_map = makers_items_map
  )
  
  # 监听点击事件，弹出大图
  observeEvent(input$show_large_image, {
    req(input$show_large_image)  # 确保图片路径有效
    
    showModal(modalDialog(
      title = "物品图片预览",
      tags$div(
        style = "overflow: auto; max-height: 700px; text-align: center;",
        tags$img(
          src = input$show_large_image,  # 直接使用传入的图片路径
          style = "max-width: 100%; height: auto; display: inline-block; border: 1px solid #ddd; border-radius: 8px;"
        )
      ),
      size = "l",
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  ###
  
  # 右键点击选择商品
  query_soldout_selected_item_details <- reactiveVal()
  
  # 监听鼠标右键 selected_inventory_row，并获取用户点击的 SKU。
  observeEvent(input$selected_inventory_row, {
    req(input$selected_inventory_row)
    
    row_index <- as.numeric(input$selected_inventory_row)  # 获取用户点击的行索引
    selected_item <- filtered_inventory()[row_index, ]  # 获取选中的数据
    
    if (nrow(selected_item) > 0) {
      # 存储物品详情
      query_soldout_selected_item_details(list(
        sku = selected_item$SKU,
        name = selected_item$ItemName,
        image = ifelse(
          is.na(selected_item$ItemImagePath) || selected_item$ItemImagePath == "",
          placeholder_150px_path,
          paste0(host_url, "/images/", basename(selected_item$ItemImagePath))
        ),
        maker = selected_item$Maker,
        domestic_stock = selected_item$DomesticQuantity
      ))
      
      # 动态更新出库请求按钮
      output$query_outbound_request_btn <- renderUI({
        if (selected_item$DomesticQuantity > 0) {
          actionButton("query_outbound_request", "出库请求", class = "btn btn-success btn-sm", style = "width: 100%;")
        } else {
          NULL  # 不显示按钮
        }
      })
    }
  })
  
  # 点击采购请求
  observeEvent(input$query_purchase_request, {
    req(query_soldout_selected_item_details())
    
    details <- query_soldout_selected_item_details()
    
    showModal(modalDialog(
      title = "创建采购请求",
      
      div(
        style = "display: flex; flex-direction: row; align-items: center; gap: 20px; margin-bottom: 15px;",
        
        # 左侧：商品图片 + 详情
        div(
          style = "flex: 0 0 40%; text-align: center;",
          tags$img(src = details$image, style = "width: 150px; height: auto; object-fit: contain; border-radius: 8px;"),
          div(
            tags$h4(details$name, style = "margin-top: 10px; color: #007BFF;"),
            tags$p(paste("SKU:", details$sku), style = "margin: 0; font-weight: bold;"),
            tags$p(paste("供应商:", details$maker), style = "margin: 0; color: #6c757d; font-size: 14px;")
          )
        ),
        
        # 右侧：采购数量 + 备注
        div(
          style = "flex: 0 0 50%;",
          numericInput("query_purchase_qty", "采购数量", value = 1, min = 1, width = "80%"),
          textAreaInput("query_purchase_remark", "备注", "", width = "80%", height = "80px")
        )
      ),
      
      footer = tagList(
        modalButton("取消"),
        actionButton("query_confirm_purchase", "确认采购", class = "btn-primary")
      )
    ))
  })
  
  # 确认采购
  observeEvent(input$query_confirm_purchase, {
    req(query_soldout_selected_item_details(), input$query_purchase_qty)
    
    details <- query_soldout_selected_item_details()
    request_id <- uuid::UUIDgenerate()
    
    # 数据库操作：插入采购请求
    dbExecute(con, "
    INSERT INTO requests (RequestID, SKU, Maker, ItemImagePath, ItemDescription, Quantity, RequestStatus, Remarks, RequestType)
    VALUES (?, ?, ?, ?, ?, ?, '待处理', ?, '采购')",
              params = list(
                request_id,
                details$sku,
                details$maker,
                details$image,
                details$name,
                input$query_purchase_qty,
                format_remark(input$query_purchase_remark, system_type) 
              )
    )
    
    bind_buttons(request_id, requests_data(), input, output, session, con)
    
    showNotification("采购请求已创建", type = "message")
    removeModal()  # 关闭模态框
  })
  
  # 点击出库请求
  observeEvent(input$query_outbound_request, {
    req(query_soldout_selected_item_details())
    
    details <- query_soldout_selected_item_details()
    
    showModal(modalDialog(
      title = "创建出库请求",
      
      div(
        style = "display: flex; flex-direction: row; align-items: center; gap: 20px; margin-bottom: 15px;",
        
        # 左侧：商品图片 + 详情
        div(
          style = "flex: 0 0 40%; text-align: center;",
          tags$img(src = details$image, style = "width: 150px; height: auto; object-fit: contain; border-radius: 8px;"),
          div(
            tags$h4(details$name, style = "margin-top: 10px; color: #007BFF;"),
            tags$p(paste("SKU:", details$sku), style = "margin: 0; font-weight: bold;"),
            tags$p(paste("供应商:", details$maker), style = "margin: 0; color: #6c757d; font-size: 14px;"),
            tags$p(
              paste("国内库存:", details$domestic_stock),
              style = paste("margin: 0;", ifelse(details$domestic_stock == 0, "color: #DC3545; font-weight: bold;", "color: #28A745;"))
            )
          )
        ),
        
        # 右侧：出库数量 + 备注
        div(
          style = "flex: 0 0 50%; display: flex; flex-direction: column; gap: 10px;",
          numericInput("query_outbound_qty", "出库数量", value = 1, min = 1, max = details$domestic_stock, width = "80%"),
          textAreaInput("query_outbound_remark", "备注", "", width = "80%", height = "80px")
        )
      ),
      
      footer = tagList(
        modalButton("取消"),
        actionButton("query_confirm_outbound", "确认出库", class = "btn-success")
      )
    ))
  })
  
  # 确认出库
  observeEvent(input$query_confirm_outbound, {
    req(query_soldout_selected_item_details(), input$query_outbound_qty)
    
    details <- query_soldout_selected_item_details()
    request_id <- uuid::UUIDgenerate()
    
    # 如果用户输入的出库数量大于国内库存，禁止提交
    if (input$query_outbound_qty > details$domestic_stock) {
      showNotification("出库数量不能大于国内库存数！", type = "error")
      return()
    }
    
    # 数据库操作：插入出库请求
    dbExecute(con, "
    INSERT INTO requests (RequestID, SKU, Maker, ItemImagePath, ItemDescription, Quantity, RequestStatus, Remarks, RequestType)
    VALUES (?, ?, ?, ?, ?, ?, '待处理', ?, '出库')",
              params = list(
                request_id,
                details$sku,
                details$maker,
                details$image,
                details$name,
                input$query_outbound_qty,
                format_remark(input$query_outbound_remark, system_type)
              )
    )
    
    bind_buttons(request_id, requests_data(), input, output, session, con)
    
    showNotification("出库请求已创建", type = "message")
    removeModal()  # 关闭模态框
  })
  
  ###
  
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
          placeholder_200px_path,
          paste0(host_url, "/images/", basename(sku_data$ItemImagePath[1]))
        )
        
        div(
          style = "display: flex; flex-direction: column; padding: 10px;",
          
          # 上部分：图片和基本信息
          div(
            style = "display: flex; align-items: flex-start; width: 100%;",
            
            # 图片区域（带点击事件）
            div(
              style = "flex: 1; text-align: center; padding-right: 10px;",
              tags$img(
                src = img_path, height = "200px",
                style = "border: 1px solid #ddd; border-radius: 8px; cursor: pointer;",
                onclick = sprintf("Shiny.setInputValue('show_large_image', '%s', {priority: 'event'})", img_path)
              )
            ),
            
            # 右侧：商品信息
            div(
              style = "flex: 2;",
