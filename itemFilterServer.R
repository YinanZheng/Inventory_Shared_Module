itemFilterServer <- function(id, makers_items_map) {
  moduleServer(id, function(input, output, session) {
    makers_hash <- reactiveVal(NULL)
    filtered_item_names_hash <- reactiveVal(NULL)
    
    default_dates <- list(
      start = Sys.Date() - 365,
      end = Sys.Date()
    )
    
    observe({
      req(makers_items_map())
      current_makers <- makers_items_map() %>% pull(Maker) %>% unique() %>% sort()
      new_hash <- digest::digest(current_makers)
      if (!is.null(makers_hash()) && makers_hash() == new_hash) return()
      makers_hash(new_hash)
      updateSelectizeInput(session, "maker", choices = c("", current_makers), selected = "", server = TRUE)
    })
    
    observe({
      req(makers_items_map())
      selected_maker <- input$maker %||% ""
      filtered_item_names <- makers_items_map()
      if (selected_maker != "") {
        filtered_item_names <- filtered_item_names %>% filter(Maker == selected_maker)
      }
      filtered_item_names <- filtered_item_names %>% pull(ItemName) %>% unique() %>% sort()
      new_hash <- digest::digest(filtered_item_names)
      if (!is.null(filtered_item_names_hash()) && filtered_item_names_hash() == new_hash) return()
      filtered_item_names_hash(new_hash)
      updateSelectizeInput(session, "name", choices = c("", filtered_item_names), selected = "", server = TRUE)
    })
    
    observeEvent(input$clear_other, {
      updateTextInput(session, "other", value = "")
    })
    
    observeEvent(input$only_show_exit, {
      if (isTRUE(input$only_show_exit)) updateCheckboxInput(session, "only_show_sold", value = FALSE)
    })
    
    observeEvent(input$only_show_sold, {
      if (isTRUE(input$only_show_sold)) updateCheckboxInput(session, "only_show_exit", value = FALSE)
    })
    
    resetFilters <- function() {
      tryCatch({
        makers_choices <- makers_items_map() %>% pull(Maker) %>% unique() %>% sort()
        updateSelectizeInput(session, "maker", choices = c("", makers_choices), selected = NULL, server = TRUE)
        updateTextInput(session, "other", value = "")
        item_name_choices <- makers_items_map() %>% pull(ItemName) %>% unique() %>% sort()
        updateSelectizeInput(session, "name", choices = c("", item_name_choices), selected = NULL, server = TRUE)
        updateSelectInput(session, "status", selected = "")
        updateAirDateInput(session, "purchase_date_range", value = c(default_dates$start, default_dates$end))
        updateAirDateInput(session, "sold_date_range", value = c(default_dates$start, default_dates$end))
        updateAirDateInput(session, "exit_date_range", value = c(default_dates$start, default_dates$end))
        updateCheckboxInput(session, "only_show_exit", value = FALSE)
        updateCheckboxInput(session, "only_show_sold", value = FALSE)
        showNotification("筛选条件已重置！", type = "message")
      }, error = function(e) {
        showNotification("重置输入时发生错误，请重试！", type = "error")
      })
    }
    
    observeEvent(input$reset_btn, { resetFilters() })
    
    return(list(resetFilters = resetFilters))
  })
}
