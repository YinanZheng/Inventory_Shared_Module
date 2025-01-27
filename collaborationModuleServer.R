collaborationModuleServer <- function(id, con, unique_items_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # 缓存请求数据
    requests_data <- reactiveVal(data.frame())
    
    # Function: 渲染留言板
    renderRemarks <- function(request_id) {
      # 提取当前 RequestID 的 Remarks
      current_remarks <- requests_data() %>% filter(RequestID == request_id) %>% pull(Remarks)
      
      # 如果当前 Remarks 为空或 NULL，则初始化为空列表
      if (is.null(current_remarks) || is.na(current_remarks)) {
        remarks <- list()  # 如果为空，则返回空列表
      } else {
        remarks <- unlist(strsplit(trimws(current_remarks), ";"))  # 使用 ; 分隔记录
      }
      
      # 生成 HTML 字符串
      remarks_html <- if (length(remarks) > 0) {
        paste0(
          lapply(remarks, function(h) {
            # 将记录拆分为时间和内容
            split_remarks <- strsplit(h, ": ", fixed = TRUE)[[1]]
            remark_time <- ifelse(length(split_remarks) > 1, split_remarks[1], "")  # 时间部分
            remark_text <- ifelse(length(split_remarks) > 1, split_remarks[2], split_remarks[1])  # 信息部分
            
            # 生成每条记录的 HTML
            paste0(
              "<div style='margin-bottom: 8px;'>",
              "<p style='font-size: 10px; color: grey; text-align: right; margin: 0;'>", remark_time, "</p>",  # 时间灰色右对齐
              "<p style='font-size: 12px; color: black; text-align: left; margin: 0;'>", remark_text, "</p>",  # 信息黑色左对齐
              "</div>"
            )
          }),
          collapse = ""
        )
      } else {
        "<p style='font-size: 12px; color: grey;'>暂无留言</p>"  # 无记录时的默认内容
      }
      
      # 渲染 HTML
      renderUI({
        HTML(remarks_html)
      })
    }
    
    # Function: 渲染任务板与便签
    refresh_todo_board <- function() {
      ns <- session$ns  # 获取模块的命名空间
      requests <- requests_data()  # 使用缓存数据
      
      if (nrow(requests) == 0) {
        output$todo_board <- renderUI({
          div(style = "text-align: center; color: grey; margin-top: 20px;", tags$p("当前没有待处理事项"))
        })
      } else {
        # 按 RequestStatus 和 CreatedAt 排序
        requests <- requests %>%
          arrange(
            factor(RequestStatus, levels = c("紧急", "待处理", "已完成")),  # 自定义状态顺序
            CreatedAt  # 按创建时间升序排列
          )
        
        output$todo_board <- renderUI({
          div(
            style = "display: grid; grid-template-columns: repeat(auto-fill, minmax(400px, 1fr)); gap: 10px; padding: 10px;",
            lapply(1:nrow(requests), function(i) {
              item <- requests[i, , drop = FALSE]
              request_id <- item$RequestID

              # 根据状态设置便签背景颜色和边框颜色
              card_colors <- switch(
                item$RequestStatus,
                "紧急" = list(bg = "#ffcdd2", border = "#e57373"),    # 红色背景，深红色边框
                "待处理" = list(bg = "#fff9c4", border = "#ffd54f"),  # 橙色背景，深橙色边框
                "已完成" = list(bg = "#c8e6c9", border = "#81c784")   # 绿色背景，深绿色边框
              )
              
              # 渲染便签卡片
              div(
                class = "note-card",
                style = sprintf("
              position: relative;
              width: 400px;
              background-color: %s;
              border: 2px solid %s;
              border-radius: 10px;
              box-shadow: 0 4px 8px rgba(0,0,0,0.1);
              padding: 10px;
              display: flex;
              flex-direction: column;
              justify-content: space-between;
            ", card_colors$bg, card_colors$border),
                
                # 图片和留言记录
                div(
                  style = "display: flex; justify-content: space-between; align-items: flex-start; margin-bottom: 10px;",
                  tags$div(
                    style = "width: 38%; display: flex; flex-direction: column; align-items: center;",
                    tags$img(
                      src = ifelse(is.na(item$ItemImagePath), placeholder_150px_path, paste0(host_url, "/images/", basename(item$ItemImagePath))),
                      style = "width: 100%; max-height: 120px; object-fit: contain; border: 1px solid #ddd; border-radius: 5px; margin-bottom: 5px;"
                    ),
                    tags$div(
                      style = "width: 100%; text-align: center; font-size: 12px; color: #333;",
                      tags$p(item$ItemDescription, style = "margin: 0;"),
                      tags$p(item$SKU, style = "margin: 0;"),
                      tags$p(paste("供应商:", item$Maker), style = "margin: 0;"),
                      tags$p(
                        tags$b("请求数量:"), 
                        tags$span(item$Quantity, style = "color: red; font-weight: bold;"), 
                        style = "margin: 0;"
                      )
                    )
                  ),
                  tags$div(
                    style = "width: 58%; height: 194px; border: 1px solid #ddd; padding: 5px; background-color: #fff; overflow-y: auto; border-radius: 5px;",
                    uiOutput(ns(paste0("remarks_", item$RequestID)))  # 使用 RequestID 动态绑定到具体记录
                  )
                ),
                
                # 留言输入框和提交按钮
                tags$div(
                  style = "width: 100%; display: flex; flex-direction: column; align-items: flex-start; margin-top: 5px;",
                  tags$div(
                    style = "width: 100%; display: flex; justify-content: space-between;",
                    textInput(ns(paste0("remark_input_", request_id)), NULL, placeholder = "输入留言", width = "72%"),
                    actionButton(ns(paste0("submit_remark_", request_id)), "提交", class = "btn-success", style = "width: 25%; height: 45px;")
                  )
                ),
                
                # 状态按钮
                tags$div(
                  style = "width: 100%; display: flex; justify-content: space-between; margin-top: 5px;",
                  actionButton(ns(paste0("mark_urgent_", request_id)), "加急", class = "btn-danger", style = "width: 30%; height: 45px;"),
                  actionButton(ns(paste0("complete_task_", request_id)), "完成", class = "btn-primary", style = "width: 30%; height: 45px;"),
                  actionButton(ns(paste0("delete_request_", request_id)), "删除", class = "btn-warning", style = "width: 30%; height: 45px;")
                )
              )
            })
          )
        })
      }
    }
    
    # Function: 绑定按钮
    bind_buttons <- function(request_id) {
      ns <- session$ns  # 获取模块的命名空间
      
      # 按钮绑定逻辑
      observeEvent(input[[ns(paste0("mark_urgent_", request_id))]], {
        dbExecute(con, "UPDATE purchase_requests SET RequestStatus = '紧急' WHERE RequestID = ?", params = list(request_id))
        refresh_todo_board()
        showNotification(paste0("Request ", request_id, " 状态已标记为紧急"), type = "warning")
      }, ignoreInit = TRUE)  # 避免初始绑定时触发事件
      
      observeEvent(input[[ns(paste0("complete_task_", request_id))]], {
        dbExecute(con, "UPDATE purchase_requests SET RequestStatus = '已完成' WHERE RequestID = ?", params = list(request_id))
        refresh_todo_board()
        showNotification(paste0("Request ", request_id, " 已完成"), type = "message")
      }, ignoreInit = TRUE)
      
      observeEvent(input[[ns(paste0("delete_request_", request_id))]], {
        dbExecute(con, "DELETE FROM purchase_requests WHERE RequestID = ?", params = list(request_id))
        refresh_todo_board()
        showNotification(paste0("Request ", request_id, " 已删除"), type = "message")
      }, ignoreInit = TRUE)
      
      observeEvent(input[[ns(paste0("submit_remark_", request_id))]], {
        remark <- input[[ns(paste0("remark_input_", request_id))]]
        req(remark != "")
        
        # 根据系统类型添加前缀
        remark_prefix <- if (system_type == "china") "[京]" else "[圳]"
        new_remark <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), ": ", remark_prefix, " ", remark)
        
        # 更新数据库中的 Remarks 字段
        current_remarks <- requests_data() %>% filter(RequestID == request_id) %>% pull(Remarks)
        current_remarks_text <- ifelse(is.na(current_remarks), "", current_remarks)
        updated_remarks <- if (current_remarks_text == "") new_remark else paste(new_remark, current_remarks_text, sep = ";")
        
        dbExecute(con, "UPDATE purchase_requests SET Remarks = ? WHERE RequestID = ?", params = list(updated_remarks, request_id))
        
        # 动态更新 UI
        output[[ns(paste0("remarks_", request_id))]] <- renderUI({
          renderRemarks(request_id)
        })
        
        # 清空输入框
        updateTextInput(session, ns(paste0("remark_input_", request_id)), value = "")
        showNotification(paste0("留言已成功提交: ", request_id), type = "message")
      }, ignoreInit = TRUE)
    }
    
    # 定期检查采购请求数据库的最新数据
    poll_requests <- reactivePoll(
      intervalMillis = poll_interval,
      session = session,
      checkFunc = function() {
        # 查询最新更新时间
        last_updated <- dbGetQuery(con, "SELECT MAX(UpdatedAt) AS last_updated FROM purchase_requests")$last_updated[1]
        if (is.null(last_updated)) {
          Sys.time()  # 如果无数据，返回当前时间
        } else {
          last_updated
        }
      },
      valueFunc = function() {
        result <- dbGetQuery(con, "SELECT * FROM purchase_requests")
        if (nrow(result) == 0) { data.frame() } else { result }
      }
    )
    
    observeEvent(poll_requests(), {
      requests <- poll_requests()
      message("Polling requests updated")
      requests_data(requests)
    })
    
    observe({
      message("Refresh todo board triggered")
      requests <- requests_data()
      refresh_todo_board()
      
      # 为每条记录绑定按钮逻辑
      isolate({
        lapply(requests$RequestID, function(request_id) {
          output[[ns(paste0("remarks_", request_id))]] <- renderRemarks(request_id)
          bind_buttons(request_id)  # 按 RequestID 动态绑定按钮
        })
      })
    })
    
    # SKU 和物品名输入互斥逻辑
    observeEvent(input$search_sku, {
      # 如果 SKU 搜索框有值，则清空物品名称搜索框
      if (input$search_sku != "") {
        updateTextInput(session, "search_name", value = "")  # 清空物品名称搜索框
      }
    })
    
    # SKU 和物品名输入互斥逻辑
    observeEvent(input$search_name, {
      # 如果物品名称搜索框有值，则清空 SKU 搜索框
      if (input$search_name != "") {
        updateTextInput(session, "search_sku", value = "")  # 清空 SKU 搜索框
      }
    })
    
    # SKU 和物品名称搜索预览
    observeEvent(c(input$search_sku, input$search_name), {
      # 如果两个输入框都为空，则清空预览
      if (input$search_sku == "" && input$search_name == "") {
        output$item_preview <- renderUI({ NULL })
        return()  # 结束逻辑
      }
      
      req(input$search_sku != "" | input$search_name != "")  # 确保至少一个搜索条件不为空
      
      # 获取清理后的输入值
      search_sku <- trimws(input$search_sku)
      search_name <- trimws(input$search_name)
      
      # 使用 unique_items_data() 进行过滤和统计
      result <- unique_items_data() %>%
        filter(
          (SKU == search_sku & search_sku != "") |  # SKU 精准匹配
            (grepl(search_name, ItemName, ignore.case = TRUE) & search_name != "")  # 名称模糊匹配
        ) %>%
        group_by(SKU, ItemName, ItemImagePath) %>%
        summarise(
          DomesticStock = sum(Status == "国内入库", na.rm = TRUE),  # 国内库存
          InTransitStock = sum(Status == "国内出库", na.rm = TRUE),  # 在途库存
          UsStock = sum(Status == "美国入库", na.rm = TRUE),  # 美国库存
          .groups = "drop"
        )
      
      # 动态更新预览界面
      if (nrow(result) > 0) {
        output$item_preview <- renderUI({
          div(
            style = "max-height: 300px; overflow-y: auto; padding: 10px; border: 1px solid #e0e0e0; border-radius: 8px; background-color: #f9f9f9;",
            lapply(1:nrow(result), function(i) {
              item <- result[i, ]
              img_path <- ifelse(
                is.na(item$ItemImagePath),
                placeholder_150px_path,  # 占位符路径
                paste0(host_url, "/images/", basename(item$ItemImagePath))  # 构建完整路径
              )
              div(
                style = "margin-bottom: 15px; padding: 10px; border-bottom: 1px solid #ccc;",
                tags$img(src = img_path, height = "150px", style = "display: block; margin: auto;"),
                tags$h5(item$ItemName, style = "text-align: center; margin-top: 10px;"),
                tags$h5(item$SKU, style = "text-align: center; margin-top: 10px;"),
                div(
                  style = "text-align: center; font-size: 12px;",
                  tags$span(paste("国内库存:", item$DomesticStock), style = "margin-right: 10px;"),
                  tags$span(paste("在途库存:", item$InTransitStock), style = "margin-right: 10px;"),
                  tags$span(paste("美国库存:", item$UsStock))
                )
              )
            })
          )
        })
      } else {
        output$item_preview <- renderUI({
          div(tags$p("未找到匹配的物品", style = "color: red; text-align: center;"))
        })
      }
    })
    
    # 库存品采购请求按钮
    observeEvent(input$add_request, {
      req(input$request_quantity > 0)  # 确保输入合法
      
      # 获取用户输入
      search_sku <- trimws(input$search_sku)
      search_name <- trimws(input$search_name)
      
      # 检索数据并插入到数据库
      filtered_data <- unique_items_data() %>%
        filter(
          (SKU == search_sku & search_sku != "") |  # SKU 精准匹配
            (grepl(search_name, ItemName, ignore.case = TRUE) & search_name != "")  # 名称模糊匹配
        ) %>%
        distinct(SKU, Maker, ItemName, ItemImagePath)  # 去重
      
      tryCatch({
        # 主逻辑
        if (nrow(filtered_data) == 1) {
          request_id <- uuid::UUIDgenerate()
          
          item_image_path <- ifelse(is.na(filtered_data$ItemImagePath[1]), placeholder_150px_path, filtered_data$ItemImagePath[1])
          item_description <- ifelse(is.na(filtered_data$ItemName[1]), "未知", filtered_data$ItemName[1])
          
          dbExecute(con, 
                    "INSERT INTO purchase_requests (RequestID, SKU, Maker, ItemImagePath, ItemDescription, Quantity, RequestStatus) 
              VALUES (?, ?, ?, ?, ?, ?, '待处理')", 
                    params = list(request_id, filtered_data$SKU, filtered_data$Maker, item_image_path, item_description, input$request_quantity))
          
          refresh_todo_board()
          bind_buttons(request_id)
          
          updateTextInput(session, "search_sku", value = "")
          updateTextInput(session, "search_name", value = "")
          updateNumericInput(session, "request_quantity", value = 1)
          
          showNotification("请求已成功创建", type = "message")
        } else if (nrow(filtered_data) > 1) {
          showNotification("搜索结果不唯一，请更精确地搜索 SKU 或物品名称", type = "error")
        } else {
          showNotification("未找到匹配的物品，请检查搜索条件", type = "error")
        }
      }, error = function(e) {
        # 捕获错误并打印详细信息
        showNotification(e, type = "error")
      })
      
    })
    
    # 初始化图片上传模块
    image_purchase_requests <- imageModuleServer("image_purchase_requests")
    
    # 新商品采购请求按钮
    observeEvent(input$submit_custom_request, {
      # 确保必要字段已填写
      req(input$custom_quantity > 0)
      
      # 获取用户输入
      custom_description <- trimws(input$custom_description)
      custom_quantity <- input$custom_quantity
      
      # 使用图片上传模块的返回数据
      custom_image_path <- process_image_upload(
        sku = "New-Request",  # 自定义物品没有 SKU，可以设置为固定值或动态生成
        file_data = image_purchase_requests$uploaded_file(),
        pasted_data = image_purchase_requests$pasted_file()
      )
      
      # 检查图片路径是否有效
      req(!is.null(custom_image_path) && !is.na(custom_image_path))
      
      # 生成唯一 RequestID
      request_id <- uuid::UUIDgenerate()
      
      # 将数据插入到数据库
      dbExecute(con, 
                "INSERT INTO purchase_requests (RequestID, SKU, Maker, ItemImagePath, ItemDescription, Quantity, RequestStatus) 
             VALUES (?, ?, '待定', ?, ?, ?, '待处理')", 
                params = list(request_id, "New-Request", custom_image_path, custom_description, custom_quantity))
      
      # 刷新任务板
      refresh_todo_board()
      
      bind_buttons(request_id) #绑定按钮逻辑
      
      # 清空输入字段
      updateTextInput(session, "custom_description", value = "")
      updateNumericInput(session, "custom_quantity", value = 1)
      image_purchase_requests$reset()
      showNotification("自定义请求已成功提交", type = "message")
    })
  })
}