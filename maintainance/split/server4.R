      
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
      
      # 刷新物品表
      unique_items_data_refresh_trigger(!unique_items_data_refresh_trigger())
      
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
    
    # 更新数据并触发 UI 刷新
    unique_items_data_refresh_trigger(!unique_items_data_refresh_trigger())
    
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
      
      # 数据刷新触发
      unique_items_data_refresh_trigger(!unique_items_data_refresh_trigger())
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
      
      # 刷新数据触发 UI 更新
      unique_items_data_refresh_trigger(!unique_items_data_refresh_trigger())
      showNotification("运单号已成功解除挂靠，相关物品的平摊运费已重新计算！", type = "message")
      
    }, error = function(e) {
      # 回滚事务
      dbRollback(con)
      showNotification(paste("解除挂靠失败：", e$message), type = "error")
    })
  })
  
  
  
  ################################################################
  ##                                                            ##
  ## 账务核对分页                                               ##
  ##                                                            ##
  ################################################################
  
  transactions_data <- reactive({
    # 从数据库读取 transactions 表
    dbReadTable(con, "transactions")
  })
  
  ### 公司债务
  
  # Reactive 计算公司债务总和
  company_liabilities_total <- reactive({
    initial_liabilities <- 45000  # 初始公司债务
    
    # 从 transactions_data 获取 TransactionType 为 "债务" 的总和
    debt_transactions <- transactions_data() %>%
      filter(TransactionType == "债务") %>%
      summarise(total_debt = sum(Amount, na.rm = TRUE)) %>%
      pull(total_debt)
    
    # 返回公司债务总和
    initial_liabilities + debt_transactions
  })
    # 显示公司债务
  output$company_liabilities <- renderText({
    sprintf("¥%.2f", company_liabilities_total())
  })
  
  
  ### 社保
  
  # Reactive 计算公司社保总和
  social_security_total <- reactive({
    initial_social_security <- 4618  # 初始社保金额
    
    # 从 transactions_data 获取 TransactionType 为 "社保" 的总和
    social_transactions <- transactions_data() %>%
      filter(TransactionType == "社保") %>%
      summarise(total_social = sum(Amount, na.rm = TRUE)) %>%
      pull(total_social)
    
    # 返回公司社保总和
    initial_social_security + social_transactions
  })
  
  # 显示公司社保
  output$social_security <- renderText({
    sprintf("¥%.2f", social_security_total())
  })
  
  
  ### 工资
  
  # Reactive 计算工资总支出（只计算 Amount < 0 的部分，并取绝对值）
  salary_total <- reactive({
    transactions_data() %>%
      filter(TransactionType == "工资", Amount < 0) %>%
      summarise(total_salary = abs(sum(Amount, na.rm = TRUE))) %>%
      pull(total_salary)
  })
  
  # 显示工资总支出
  output$salary <- renderText({
    sprintf("¥%.2f", salary_total())
  })
  
  
  ### 现金流
  
  # 计算现金流
  cash_flow_total <- reactive({
    # 获取 transactions_data 中所有 Amount 的总和
    total_amount <- transactions_data() %>%
      summarise(total = sum(Amount, na.rm = TRUE)) %>%
      pull(total)
    
    # 获取公司债务和社保总额
    total_liabilities <- company_liabilities_total()
    total_social_security <- social_security_total()
    
    # 计算现金流
    cash_flow <- total_amount - total_liabilities - total_social_security
    
    return(cash_flow)
  })
  
  # 显示现金流
  output$cash_flow <- renderText({
    sprintf("¥%.2f", cash_flow_total())
  })
  
  
  ### 公司税费
  
  # 计算公司税费总支出
  company_tax_total <- reactive({
    transactions_data() %>%
      filter(TransactionType == "税费", Amount < 0) %>%
      summarise(total_tax = abs(sum(Amount, na.rm = TRUE))) %>%
      pull(total_tax)
  })
  
  # 显示公司税费
  output$company_tax <- renderText({
    sprintf("¥%.2f", company_tax_total())
  })
  

  ### 公司杂费
  
  # 计算公司杂费总支出
  company_expenses_total <- reactive({
    transactions_data() %>%
      filter(TransactionType %in% c("杂费", "图解"), Amount < 0) %>%
      summarise(total_expenses = abs(sum(Amount, na.rm = TRUE))) %>%
      pull(total_expenses)
  })
  
  # 显示公司杂费
  output$company_expenses <- renderText({
    sprintf("¥%.2f", company_expenses_total())
  })
  
  
  ### 投入总金额
  
  # 计算投入总金额
  total_investment_value <- reactive({
    initial_investment <- 82445.9  # 初始投入金额
    
    # 获取 transactions 表中 AccountType 为 "美元卡" 且 Amount > 0 的总和
    usd_card_transactions <- transactions_data() %>%
      filter(AccountType == "美元卡", Amount > 0) %>%
      summarise(total_investment = sum(Amount, na.rm = TRUE)) %>%
      pull(total_investment)
    
    # 计算最终投入总金额
    total_investment <- initial_investment + usd_card_transactions
    
    return(total_investment)
  })
  
  # 显示投入总金额
  output$total_investment <- renderText({
    sprintf("¥%.2f", total_investment_value())
  })
  
  
  ###
  
  # 计算实际总金额
  actual_total_value <- reactive({
    total_salary <- salary_total()  # 总工资
    total_cash_flow <- cash_flow_total()  # 现金流
    total_after_20241223 <- inventory_value_cost_data()$after$total_value + inventory_value_cost_data()$after$total_shipping  # 12月23日后货值（含运费）
    total_tax <- company_tax_total()  # 公司税费
    total_expenses <- company_expenses_total()  # 公司杂费
    
    actual_total <- total_salary + total_cash_flow + total_after_20241223 + total_tax + total_expenses
    
    return(actual_total)
  })
  
  # 显示实际总金额
  output$actual_total <- renderText({
    sprintf("¥%.2f", actual_total_value())
  })
  
  
  # 显示对账差额
  output$reconciliation_difference <- renderText({
    sprintf("¥%.2f", total_investment_value() - actual_total_value() )
  })
  
  
  ### 货值与运费
  
  # 计算货值与运费
  inventory_value_cost_data <- reactive({
    data <- unique_items_data()
    date_cutoff <- as.Date("2024-12-23")
    
    # 按时间分割数据
    before_20241223 <- data %>% filter(PurchaseTime <= date_cutoff)
    after_20241223 <- data %>% filter(PurchaseTime > date_cutoff)
    
    # 调用 process_data 处理数据
    before <- process_data(before_20241223)
    after <- process_data(after_20241223)
    
    # 汇总数据
    list(
      before = c(before, calculate_totals(before)),
      after = c(after, calculate_totals(after))
    )
  })
  
  # 显示12月23日前货值与运费统计数据
  output$before_20241223_total_value <- renderText({
    sprintf("¥%.2f", inventory_value_cost_data()$before$total_value)
  })
  output$before_20241223_total_shipping <- renderText({
    sprintf("¥%.2f", inventory_value_cost_data()$before$total_shipping)
  })
  output$before_20241223_domestic_value <- renderText({
    sprintf("¥%.2f", inventory_value_cost_data()$before$domestic$value)
  })
  output$before_20241223_domestic_shipping <- renderText({
    sprintf("¥%.2f", inventory_value_cost_data()$before$domestic$shipping)
  })
  output$before_20241223_logistics_value <- renderText({
    sprintf("¥%.2f", inventory_value_cost_data()$before$logistics$value)
  })
  output$before_20241223_logistics_shipping <- renderText({
    sprintf("¥%.2f", inventory_value_cost_data()$before$logistics$shipping)
  })
  output$before_20241223_us_value <- renderText({
    sprintf("¥%.2f", inventory_value_cost_data()$before$us$value)
  })
  output$before_20241223_us_shipping <- renderText({
    sprintf("¥%.2f", inventory_value_cost_data()$before$us$shipping)
  })
  output$before_20241223_sold_value <- renderText({
    sprintf("¥%.2f", inventory_value_cost_data()$before$sold$value)
  })
  output$before_20241223_sold_shipping <- renderText({
    sprintf("¥%.2f", inventory_value_cost_data()$before$sold$shipping)
  })
  
  # 显示12月23日后货值与运费统计数据
  output$after_20241223_total_value <- renderText({
    sprintf("¥%.2f", inventory_value_cost_data()$after$total_value)
  })
  output$after_20241223_total_shipping <- renderText({
    sprintf("¥%.2f", inventory_value_cost_data()$after$total_shipping)
  })
  output$after_20241223_domestic_value <- renderText({
    sprintf("¥%.2f", inventory_value_cost_data()$after$domestic$value)
  })
  output$after_20241223_domestic_shipping <- renderText({
    sprintf("¥%.2f", inventory_value_cost_data()$after$domestic$shipping)
  })
  output$after_20241223_logistics_value <- renderText({
    sprintf("¥%.2f", inventory_value_cost_data()$after$logistics$value)
  })
  output$after_20241223_logistics_shipping <- renderText({
    sprintf("¥%.2f", inventory_value_cost_data()$after$logistics$shipping)
  })
  output$after_20241223_us_value <- renderText({
    sprintf("¥%.2f", inventory_value_cost_data()$after$us$value)
  })
  output$after_20241223_us_shipping <- renderText({
    sprintf("¥%.2f", inventory_value_cost_data()$after$us$shipping)
  })
  output$after_20241223_sold_value <- renderText({
    sprintf("¥%.2f", inventory_value_cost_data()$after$sold$value)
  })
  output$after_20241223_sold_shipping <- renderText({
    sprintf("¥%.2f", inventory_value_cost_data()$after$sold$shipping)
  })
  
  
  
  
  ################################################################
  ##                                                            ##
  ## 查询分页                                                   ##
  ##                                                            ##
  ################################################################
  
  # 监听主页面和子页面的切换
  observeEvent({
    list(input$inventory_us, input$query_tabs)  # 仅在这些输入发生变化时触发
  }, {
    if (input$inventory_us == "查询" && input$query_tabs == "商品状态") {
      inventory_refresh_trigger(!inventory_refresh_trigger())
      showNotification("库存表已加载！", type = "message")
    }
    
    if (input$inventory_us == "查询" && input$query_tabs == "库存总览") {
      item_status_history_refresh_trigger(!item_status_history_refresh_trigger())
      showNotification("库存状态历史已加载！", type = "message")
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
              tags$table(
                style = "width: 100%; border-collapse: collapse; line-height: 2;",
                tags$tr(
                  tags$td(style = "white-space: nowrap; font-weight: bold; min-width: 90px;", "商品名称："), 
                  tags$td(style = "word-break: break-word;", sku_data$ItemName[1])
                ),
                tags$tr(
                  tags$td(style = "white-space: nowrap; font-weight: bold; min-width: 90px;", "供应商："), 
                  tags$td(style = "word-break: break-word;", sku_data$Maker[1])
                ),
                tags$tr(
                  tags$td(style = "white-space: nowrap; font-weight: bold; min-width: 90px;", "分类："), 
                  tags$td(style = "word-break: break-word;", paste(sku_data$MajorType[1], "/", sku_data$MinorType[1]))
                ),
                tags$tr(
                  tags$td(style = "white-space: nowrap; font-weight: bold; min-width: 90px;", "平均成本："), 
                  tags$td(style = "word-break: break-word;", sprintf("¥%.2f", sku_data$ProductCost[1]))
                ),
                tags$tr(
                  tags$td(style = "white-space: nowrap; font-weight: bold; min-width: 90px;", "平均运费："), 
                  tags$td(style = "word-break: break-word;", sprintf("¥%.2f", sku_data$ShippingCost[1]))
                )
              )
            )
          ),
          
          # 底部：库存信息
          div(
            style = "width: 100%; margin-top: 10px; text-align: center; padding-top: 5px; border-top: 1px solid #ddd;",
            tags$span(
              style = "font-weight: bold;",
              "库存数："
            ),
            tags$span(
              HTML(sprintf(
                "国内：%d &emsp;|&emsp; 在途：%d &emsp;|&emsp; 美国：%d &emsp;|&emsp; 总计：%d",
                sku_data$DomesticQuantity[1], 
                sku_data$TransitQuantity[1], 
                sku_data$UsQuantity[1], 
                sku_data$Quantity[1]
              ))
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
          status_levels <- c("采购", "国内入库", "国内售出", "国内出库", "美国入库", "美国调货", "美国发货", "退货")
          status_colors <- c("lightgray", "#c7e89b", "#9ca695", "#46a80d", "#6f52ff", "#529aff", "#faf0d4", "red")
          
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
        Cost_Domestic = round(sum(ProductCost + DomesticShippingCost, na.rm = TRUE), 2),
        ProductCost = round(sum(ProductCost, na.rm = TRUE), 2),
        DomesticShippingCost = round(sum(DomesticShippingCost, na.rm = TRUE), 2),
        IntlShippingCost = round(sum(IntlShippingCost, na.rm = TRUE), 2),
        TotalExpense = round(sum(ProductCost + DomesticShippingCost + IntlShippingCost, na.rm = TRUE), 2),
        AllPurchaseCheck = all(PurchaseCheck == 1, na.rm = TRUE), # 是否全部为1
        .groups = "drop"
      )
    
    complete_data <- time_df %>%
      left_join(summarized_data, by = "GroupDate") %>%
      replace_na(list(
        Cost_Domestic = 0,
        ProductCost = 0,
