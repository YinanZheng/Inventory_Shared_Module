# collaboration_module.R

collaborationModuleUI <- function(id) {
  ns <- NS(id)
  tabPanel(
    "协作", icon = icon("users"),
    div(
      class = "layout-container",
      div(
        class = "sticky-sidebar",
        div(
          tags$h4("库存品采购请求", style = "font-weight: bold; color: #007BFF;"),
          fluidRow(
            column(6, textInput(ns("search_sku"), "按SKU搜索", placeholder = "输入SKU", width = "100%")),
            column(6, textInput(ns("search_name"), "按物品名搜索", placeholder = "输入物品名", width = "100%"))
          ),
          div(
            style = "margin-bottom: 10px;",
            div(
              style = "border: 1px solid #e0e0e0; border-radius: 8px; padding: 10px; background-color: #f9f9f9;",
              tags$h5("物品预览", style = "font-weight: bold; color: #007BFF;"),
              uiOutput(ns("item_preview"))
            )
          ),
          numericInput(ns("request_quantity"), "请求采购数量", value = 1, min = 1, width = "100%"),
          actionButton(ns("add_request"), "创建请求", icon = icon("plus"), class = "btn-success", style = "width: 100%; margin-top: 10px;"),
          tags$hr(),
          tags$h4("新商品采购请求", style = "font-weight: bold; color: #007BFF;"),
          imageModuleUI(ns("image_purchase_requests"), label = "请求物品图片上传"),
          textInput(ns("custom_description"), "物品名", placeholder = "输入物品名", width = "100%"),
          numericInput(ns("custom_quantity"), "请求采购数量", value = 1, min = 1, width = "100%"),
          actionButton(ns("submit_custom_request"), "创建请求", icon = icon("plus"), class = "btn-success", style = "width: 100%; margin-top: 10px;")
        )
      ),
      
      div(
        class = "resizable-divider",
      ),
      
      div(
        class = "main-panel",
        tabsetPanel(
          id = ns("collaboration_tabs"),
          type = "pills",
          tabPanel(
            title = "采购请求",  
            uiOutput(ns("todo_board"))
          )
        )
      )
    )
  )
}
