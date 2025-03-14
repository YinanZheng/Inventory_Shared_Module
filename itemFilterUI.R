itemFilterUI <- function(
    id, 
    border_color = "#007BFF", 
    text_color = "#007BFF", 
    use_status = TRUE,
    status_choices = c("库存状态" = "", "采购", "国内入库", "国内出库", "国内售出", "美国入库", "美国发货", "美国调货", "退货"), 
    use_purchase_date = TRUE, 
    use_sold_date = FALSE, 
    use_exit_date = FALSE
) {
  ns <- NS(id)
  
  card_style <- sprintf(
    "padding: 10px; border: 1px solid %s; border-radius: 8px; box-shadow: 0px 4px 6px rgba(0,0,0,0.1);",
    border_color
  )
  title_style <- sprintf("color: %s; font-weight: bold; margin: 0;", text_color)
  
  input_width <- "100%"
  maker_options <- list(placeholder = '供应商', maxOptions = 500, create = FALSE)
  name_options <- list(placeholder = "商品名", create = TRUE)
  
  default_start_date <- Sys.Date() - 365
  default_end_date <- Sys.Date() + 1
  
  div(
    class = "card",
    style = card_style,
    
    div(
      style = "margin-bottom: 5px; display: flex; align-items: center; justify-content: space-between; width: 100%;",
      tags$h4("物品筛选", style = title_style),
      actionButton(
        ns("reset_btn"),
        "重置筛选",
        icon = icon("rotate-right"),
        class = "btn-danger",
        style = "font-size: 14px; height: 30px; padding: 5px 10px; margin-left: auto;"
      )
    ),
    
    # 供应商和状态筛选
    tagList(
      fluidRow(
        if (use_status) {
          list(
            column(7, selectizeInput(ns("maker"), NULL, choices = NULL, width = input_width, options = maker_options)),
            column(5, selectInput(ns("status"), NULL, choices = status_choices, selected = "", width = input_width))
          )
        } else {
          column(12, selectizeInput(ns("maker"), NULL, choices = NULL, width = input_width, options = maker_options))
        }
      )
    ),
    
    # 商品名筛选
    fluidRow(column(12, selectizeInput(ns("name"), NULL, choices = NULL, options = name_options, width = input_width))),
    
    # 万能筛选框
    fluidRow(
      column(12, div(
        style = "display: flex; align-items: center; gap: 0;",
        textInput(ns("other"), label = NULL, placeholder = "关键词（其他任意字段）", width = "100%"),
        actionButton(ns("clear_other"), label = "", 
                     icon = icon("xmark", style = "color: #D32F2F;"), 
                     style = "padding: 0 5px; border: none; margin-bottom: 14px; font-size: 18px; background-color: #F5F5F5; height: 45px; min-width: 34px;")
      ))
    ),
    
    # 日期范围筛选
    tagList(
      if (use_purchase_date) {
        fluidRow(column(12, dateRangeInput(ns("purchase_date_range"), "采购日期范围", 
                                           start = default_start_date, end = default_end_date, width = input_width)))
      },
      if (use_exit_date) {
        div(
          dateRangeInput(ns("exit_date_range"), "出库日期范围", 
                         start = default_start_date, end = default_end_date, width = input_width),
          checkboxInput(ns("only_show_exit"), "仅显示出库物品", value = FALSE, width = input_width)
        )
      },
      if (use_sold_date) {
        div(
          dateRangeInput(ns("sold_date_range"), "售出日期范围", 
                         start = default_start_date, end = default_end_date, width = input_width),
          checkboxInput(ns("only_show_sold"), "仅显示售出物品", value = FALSE, width = input_width)
        )
      }
    )
  )
}
