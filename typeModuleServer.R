typeModuleServer <- function(id, con, item_type_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns  # 为模块创建命名空间
    
    # 渲染大类下拉框
    output$major_type_ui <- renderUI({
      type_data <- item_type_data()
      
      if (is.null(type_data) || nrow(type_data) == 0) {
        selectizeInput(
          ns("new_major_type"), 
          NULL, 
          choices = NULL, 
          width = "100%", 
          options = list(placeholder = "暂无数据", maxOptions = 500)
        )      
      } else {
        choices <- setNames(
          c("", unique(type_data$MajorType)),  # 在值中添加空选项
          c("请选择", paste0(unique(type_data$MajorType), "（", unique(type_data$MajorTypeSKU), "）"))  # 在名称中添加对应提示
        )
        selectizeInput(
          ns("new_major_type"), 
          NULL, 
          choices = choices, 
          width = "100%", 
          options = list(placeholder = "选择或搜索大类", maxOptions = 500)
        )      
      }
    })
    
    # 新增大类逻辑
    observeEvent(input$add_major_type_btn, {
      showModal(modalDialog(
        title = "批量新增大类",
        fluidRow(
          column(6, textAreaInput(ns("new_major_types"), "大类名称:", placeholder = "每行一个大类名称")),
          column(6, textAreaInput(ns("new_major_skus"), "大类SKU:", placeholder = "每行一个SKU，与左侧名称对应"))
        ),
        footer = tagList(
          actionButton(ns("confirm_add_major_types"), "添加", class = "btn-primary"),
          modalButton("取消")
        )
      ))
    })
    
    observeEvent(input$confirm_add_major_types, {
      req(input$new_major_types, input$new_major_skus)
      
      # 解析用户输入
      major_names <- strsplit(input$new_major_types, "\n")[[1]]
      major_skus <- strsplit(input$new_major_skus, "\n")[[1]]
      
      # 清理空白行并对齐长度
      major_names <- unique(trimws(major_names))
      major_skus <- unique(trimws(major_skus))
      
      # 检查输入是否匹配
      if (length(major_names) == 0 || length(major_skus) == 0) {
        showNotification("请输入有效的大类名称和SKU！", type = "error")
        return()
      }
      
      if (length(major_names) != length(major_skus)) {
        showNotification("大类名称和SKU数量不匹配，请检查输入！", type = "error")
        return()
      }
      
      tryCatch({
        # 批量插入到数据库
        for (i in seq_along(major_names)) {
          if (major_names[i] != "" && major_skus[i] != "") {
            dbExecute(con, 
                      "INSERT INTO item_type_data (MajorType, MajorTypeSKU) VALUES (?, ?)",
                      params = list(major_names[i], major_skus[i]))
          }
        }
        
        showNotification("批量新增大类成功！", type = "message")
        removeModal()
        
        # 重新加载数据
        item_type_data(dbGetQuery(con, "SELECT * FROM item_type_data"))
      }, error = function(e) {
        showNotification(paste("批量新增大类失败：", e$message), type = "error")
        print(e)  # 输出错误日志以便调试
      })
    })
  })
}