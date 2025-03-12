itemFilterServer <- function(id, makers_items_map) {
  moduleServer(id, function(input, output, session) {
    # 缓存哈希值，用于优化更新
    makers_hash <- reactiveVal(NULL)
    filtered_item_names_hash <- reactiveVal(NULL)
    
    # 默认日期范围，用于重置
    default_dates <- list(
      start = Sys.Date() - 365,
      end = Sys.Date()
    )
    
    # 动态更新 makers 控件
    observe({
      req(makers_items_map())
      
      current_makers <- makers_items_map() %>% 
        pull(Maker) %>% 
        unique() %>% 
        sort()
      
      new_hash <- digest::digest(current_makers)
      if (!is.null(makers_hash()) && makers_hash() == new_hash) return()
      makers_hash(new_hash)
      
      updateSelectizeInput(
        session,
        inputId = "maker",
        choices = c("", current_makers),
        selected = "",
        server = TRUE
      )
    })
    
    # 动态过滤并更新 item names 控件
    observe({
      req(makers_items_map())
      
      selected_maker <- input$maker %||% ""
      selected_sku <- input$sku %||% ""
      
      filtered_item_names <- makers_items_map()
      
      if (selected_maker != "") {
        filtered_item_names <- filtered_item_names %>%
          filter(Maker == selected_maker)
      }
      if (selected_sku != "") {
        filtered_item_names <- filtered_item_names %>%
          filter(SKU == selected_sku) 
      }
      
      filtered_item_names <- filtered_item_names %>%
        pull(ItemName) %>%
        unique() %>%
        sort()
      
      new_hash <- digest::digest(filtered_item_names)
      if (!is.null(filtered_item_names_hash()) && filtered_item_names_hash() == new_hash) return()
      filtered_item_names_hash(new_hash)
      
      updateSelectizeInput(
        session,
        inputId = "name",
        choices = c("", filtered_item_names),
        selected = "",
        server = TRUE
      )
    })
    
    # 互斥逻辑：仅显示出库和仅显示售出不能同时选中
    observeEvent(input$only_show_exit, {
      if (isTRUE(input$only_show_exit)) {
        updateCheckboxInput(session, "only_show_sold", value = FALSE)
      }
    })
    
    observeEvent(input$only_show_sold, {
      if (isTRUE(input$only_show_sold)) {
        updateCheckboxInput(session, "only_show_exit", value = FALSE)
      }
    })
    
    # 重置筛选逻辑
    resetFilters <- function() {
      tryCatch({
        # 重置 makers
        makers_choices <- makers_items_map() %>% pull(Maker) %>% unique() %>% sort()
        updateSelectizeInput(session, "maker", choices = c("", makers_choices), selected = NULL, server = TRUE)
        
        # 重置 SKU（清空文本输入）
        updateTextInput(session, "sku", value = "")
        
        # 重置商品名称
        item_name_choices <- makers_items_map() %>% pull(ItemName) %>% unique() %>% sort()
        updateSelectizeInput(session, "name", choices = c("", item_name_choices), selected = NULL, server = TRUE)
        
        # 重置库存状态（如果存在）
        if (!is.null(input$status)) {
          updateSelectInput(session, "status", selected = "")
        }
        
        # 重置日期选择器（如果存在）
        if (!is.null(input$purchase_date_range)) {
          updateDateRangeInput(session, "purchase_date_range", start = default_dates$start, end = default_dates$end)
        }
        if (!is.null(input$sold_date_range)) {
          updateDateRangeInput(session, "sold_date_range", start = default_dates$start, end = default_dates$end)
        }
        if (!is.null(input$exit_date_range)) {
          updateDateRangeInput(session, "exit_date_range", start = default_dates$start, end = default_dates$end)
        }
        
        # 重置复选框
        updateCheckboxInput(session, "only_show_exit", value = FALSE)
        updateCheckboxInput(session, "only_show_sold", value = FALSE)
        
        showNotification("筛选条件已重置！", type = "message")
      }, error = function(e) {
        showNotification("重置输入时发生错误，请重试！", type = "error")
      })
    }
    
    # 绑定重置按钮
    observeEvent(input$reset_btn, {
      resetFilters()
    })
    
    # 返回外部接口
    return(list(
      resetFilters = resetFilters
    ))
  })
}
