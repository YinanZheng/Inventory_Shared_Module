              tags$div(
                tags$h5("在途", style = "font-size: 16px; font-weight: bold; margin-bottom: 10px;"),
                tags$p(
                  textOutput("after_20241223_logistics_value", container = span, inline = TRUE),
                  " | ",
                  textOutput("after_20241223_logistics_shipping", container = span, inline = TRUE),
                  style = "font-size: 14px; margin-bottom: 0;"
                )
              ),
              tags$div(
                tags$h5("美国", style = "font-size: 16px; font-weight: bold; margin-bottom: 10px;"),
                tags$p(
                  textOutput("after_20241223_us_value", container = span, inline = TRUE),
                  " | ",
                  textOutput("after_20241223_us_shipping", container = span, inline = TRUE),
                  style = "font-size: 14px; margin-bottom: 0;"
                )
              ),
              tags$div(
                tags$h5("售出", style = "font-size: 16px; font-weight: bold; margin-bottom: 10px;"),
                tags$p(
                  textOutput("after_20241223_sold_value", container = span, inline = TRUE),
                  " | ",
                  textOutput("after_20241223_sold_shipping", container = span, inline = TRUE),
                  style = "font-size: 14px; margin-bottom: 0;"
                )
              )
            )
          ))
        ),
        fluidRow(
          column(6, div(
            class = "card shadow-lg",
            style = "background: #e0e0e0; color: black; padding: 15px; text-align: center; border-radius: 16px; margin-top: 20px;",
            tags$h4("公司债务", style = "font-weight: bold; margin-bottom: 10px;"),
            tags$h3(textOutput("company_liabilities"), style = "font-size: 24px; font-weight: bold; color: #007BFF; margin-bottom: 0;")
          )),
          column(6, div(
            class = "card shadow-lg",
            style = "background: #e0e0e0; color: black; padding: 15px; text-align: center; border-radius: 16px; margin-top: 20px;",
            tags$h4("社保(初始资金4618)", style = "font-weight: bold; margin-bottom: 10px;"),
            tags$h3(textOutput("social_security"), style = "font-size: 24px; font-weight: bold; color: #007BFF; margin-bottom: 0;")
          ))
        )
      )
    )
  ), # End of 账务管理
  
  tabPanel(
    "查询", icon = icon("search"), 
    div(
      class = "layout-container",  # Flexbox 容器
      div(
        class = "sticky-sidebar",  # sticky 侧边栏
        itemFilterUI(id = "query_filter", border_color = "#28A745", text_color = "#28A745", use_status = FALSE, use_purchase_date = FALSE),
        
        tags$hr(),
        
        div(
          class = "card",
          style = "margin-bottom: 20px; padding: 20px; border: 1px solid #007BFF; border-radius: 8px; box-shadow: 0px 4px 6px rgba(0,0,0,0.1);",
          tags$h4("查询商品", style = "color: #007BFF; font-weight: bold; margin-bottom: 15px;"),
          textInput("query_sku", NULL, placeholder = "请扫描或输入SKU", width = "100%"),
          actionButton("clear_query_sku_btn", "清空", icon = icon("eraser"), class = "btn btn-warning")
        )
      ),
      
      div(
        class = "resizable-divider",
      ),
      
      div(
        class = "main-panel",
        # 使用 tabsetPanel 来组织分页
        tabsetPanel(
          id = "query_tabs",
          type = "pills",
          tabPanel(
            "商品状态",
            fluidRow(
              column(
                5,
                div(
                  class = "card",
                  style = "height: 370px; margin-bottom: 5px; padding: 5px; border: 1px solid #007BFF; border-radius: 8px; box-shadow: 0px 4px 6px rgba(0,0,0,0.1);",
                  tags$h4("商品信息", style = "color: #007BFF; font-weight: bold; padding-left: 10px;"),
                  uiOutput("query_item_info") # 动态渲染物品信息
                )
              ),
              
              column(
                4,
                div(
                  class = "card",
                  style = "margin-bottom: 5px; padding: 5px; border: 1px solid #28a745; border-radius: 8px; box-shadow: 0px 4px 6px rgba(0,0,0,0.1);",
                  tags$h4("库存状态", style = "color: #28a745; font-weight: bold; padding-left: 10px;"),
                  plotlyOutput("inventory_status_chart", height = "320px") # 使用 plotlyOutput
                )
              ),
              
              column(
                3,
                div(
                  class = "card",
                  style = "margin-bottom: 5px; padding: 5px; border: 1px solid #dc3545; border-radius: 8px; box-shadow: 0px 4px 6px rgba(0,0,0,0.1);",
                  tags$h4("瑕疵情况", style = "color: #dc3545; font-weight: bold; padding-left: 10px"),
                  plotlyOutput("defect_status_chart", height = "320px") # 使用 plotlyOutput
                )
              )
            ),
            
            div(
              style = "display: flex; flex-direction: column;",
              div(
                style = "flex-grow: 1; overflow-y: auto; padding-top: 10px;",  # 表格自适应高度
                div(
                  id = "inventory_table_container_query",
                  DTOutput("filtered_inventory_table_query")
                )
              )
            )
          ), # end of 商品状态
          
          tabPanel(
            "采购开销",
            fluidRow(
              column(
                12,
                div(
                  class = "card",
                  style = "margin-bottom: 5px; padding: 5px; border: 1px solid #007BFF; border-radius: 8px; box-shadow: 0px 4px 6px rgba(0,0,0,0.1);",
                  
                  # 选择器行
                  fluidRow(
                    column(3,                   
                           dateRangeInput(
                             "time_range",
                             label = "选择采购时间范围",
                             start = Sys.Date() - 30, # 默认最近30天
                             end = Sys.Date()
                           )),
                    column(3,
                           radioButtons(
                             "precision",
                             label = "选择统计精度",
                             choices = c("天" = "天", "周" = "周", "月" = "月", "年" = "年"),
                             selected = "天",
                             inline = TRUE # 使选项横向排列
                           )),
                    column(5,
                           radioButtons(
                             "expense_type",
                             label = "选择显示内容",
                             choices = c("成本+国内运费" = "cost_domestic", "成本" = "cost", "国内运费" = "domestic_shipping", "国际运费" = "intl_shipping", "总开销" = "total"),
                             selected = "cost_domestic",
                             inline = TRUE # 使选项横向排列
                           )),
                    column(1,
                           actionButton(
                             "reset_time_range",
                             label = "",
                             icon = icon("redo"), # 添加一个重置图标
                             class = "btn-warning", # 设置按钮样式
                             style = "height: 50px; font-size: 14px;" # 设置样式
                           ))
                  ),
                  
                  # 图表行：柱状图 + 饼图
                  fluidRow(
                    column(9, plotlyOutput("expense_chart", height = "350px")), # 80% 宽度柱状图
                    column(3, plotlyOutput("pie_chart", height = "350px"))  # 20% 宽度饼图
                  ),
                  uniqueItemsTableUI("expense_details_table") # 物品详情表
                )
              )
            )
          ), # end of 开销汇总tab
          
          tabPanel(
            "库存总览",
            fluidRow(
              # 国内库存卡片
              column(
                3,
                div(
                  class = "card",
                  style = "padding: 20px; border: 1px solid #007BFF; border-radius: 8px; box-shadow: 0px 4px 6px rgba(0,0,0,0.1);",
                  tags$h4("国内库存", style = "color: #007BFF; font-weight: bold; text-align: center;"),
                  div(
                    style = "text-align: center; margin-top: 10px;",
                    tags$h3(textOutput("domestic_total_count"), style = "color: #007BFF; font-weight: bold;"),
                    tags$p("物品总数")
                  ),
                  tags$hr(style = "border: none; height: 1px; background-color: #ddd; margin: 15px 0;"),
                  div(
                    style = "text-align: center; margin-top: 10px;",
                    tags$h4(textOutput("domestic_total_value"), style = "color: #007BFF;"),
                    tags$p("货物价值")
                  ),
                  tags$hr(style = "border: none; height: 1px; background-color: #ddd; margin: 15px 0;"),
                  div(
                    style = "text-align: center; margin-top: 10px;",
                    tags$h4(textOutput("domestic_shipping_cost"), style = "color: #007BFF;"),
                    tags$p("运输成本")
                  )
                )
              ),
              # 国际物流卡片
              column(
                3,
                div(
                  class = "card",
                  style = "padding: 20px; border: 1px solid #28A745; border-radius: 8px; box-shadow: 0px 4px 6px rgba(0,0,0,0.1);",
                  tags$h4("国际物流", style = "color: #28A745; font-weight: bold; text-align: center;"),
                  div(
                    style = "text-align: center; margin-top: 10px;",
                    tags$h3(textOutput("logistics_total_count"), style = "color: #28A745; font-weight: bold;"),
                    tags$p("物品总数")
                  ),
                  tags$hr(style = "border: none; height: 1px; background-color: #ddd; margin: 15px 0;"),
                  div(
                    style = "text-align: center; margin-top: 10px;",
                    tags$h4(textOutput("logistics_total_value"), style = "color: #28A745;"),
                    tags$p("货物价值")
                  ),
                  tags$hr(style = "border: none; height: 1px; background-color: #ddd; margin: 15px 0;"),
                  div(
                    style = "text-align: center; margin-top: 10px;",
                    tags$h4(textOutput("logistics_shipping_cost"), style = "color: #28A745;"),
                    tags$p("运输成本")
                  )
                )
              ),
              # 美国库存卡片
              column(
                3,
                div(
                  class = "card",
                  style = "padding: 20px; border: 1px solid #6F42C1; border-radius: 8px; box-shadow: 0px 4px 6px rgba(0,0,0,0.1);",
                  tags$h4("美国库存", style = "color: #6F42C1; font-weight: bold; text-align: center;"),
                  div(
                    style = "text-align: center; margin-top: 10px;",
                    tags$h3(textOutput("us_total_count"), style = "color: #6F42C1; font-weight: bold;"),
                    tags$p("物品总数")
                  ),
                  tags$hr(style = "border: none; height: 1px; background-color: #ddd; margin: 15px 0;"),
                  div(
                    style = "text-align: center; margin-top: 10px;",
                    tags$h4(textOutput("us_total_value"), style = "color: #6F42C1;"),
                    tags$p("货物价值")
                  ),
                  tags$hr(style = "border: none; height: 1px; background-color: #ddd; margin: 15px 0;"),
                  div(
                    style = "text-align: center; margin-top: 10px;",
                    tags$h4(textOutput("us_shipping_cost"), style = "color: #6F42C1;"),
                    tags$p("运输成本")
                  )
                )
              ),
              # 商品售出卡片
              column(
                3,
                div(
                  class = "card",
                  style = "padding: 20px; border: 1px solid #FF5733; border-radius: 8px; box-shadow: 0px 4px 6px rgba(0,0,0,0.1);",
                  tags$h4("商品售出", style = "color: #FF5733; font-weight: bold; text-align: center;"),
                  div(
                    style = "text-align: center; margin-top: 10px;",
                    tags$h3(
                      textOutput("sold_total_count_with_shipping"),
                      style = "color: #FF5733; font-weight: bold;"
                    ),
                    tags$p("物品总数（已投递）")
                  )
                  ,
                  tags$hr(style = "border: none; height: 1px; background-color: #ddd; margin: 15px 0;"),
                  div(
                    style = "text-align: center; margin-top: 10px;",
                    tags$h4(textOutput("sold_total_value"), style = "color: #FF5733;"),
                    tags$p("货物价值")
                  ),
                  tags$hr(style = "border: none; height: 1px; background-color: #ddd; margin: 15px 0;"),
                  div(
                    style = "text-align: center; margin-top: 10px;",
                    tags$h4(textOutput("sold_shipping_cost"), style = "color: #FF5733;"),
                    tags$p("运输成本")
                  )
                )
              )
            ),
            
            tags$hr(style = "margin: 10px 0; border: 1px solid #ddd;"),
            
            fluidRow(
              column(
                12,
                div(
                  class = "card",
                  style = "padding: 20px; border: 1px solid #007BFF; border-radius: 8px; box-shadow: 0px 4px 6px rgba(0,0,0,0.1);",
                  tags$h4("库存状态流转桑基图", style = "color: #007BFF; font-weight: bold; text-align: center;"),
                  sankeyNetworkOutput("status_sankey", height = "345px")
                )
              )
            )
          ) # end of 库存汇总tab
        ) #end of tabsetPanel
      )
    )
  ), # end of 查询 tab
  
  tabPanel(
    "数据下载", icon = icon("download"),
    div(
      class = "layout-container",  # Flexbox 容器
      div(
        class = "sticky-sidebar",  # sticky 侧边栏
        div(
          class = "card shadow-sm", # 添加卡片样式
          style = "border: 1px solid #e0e0e0; border-radius: 8px; padding: 20px; background-color: #f9f9f9;",
          
          tags$h4("表格筛选", style = "color: #007BFF; font-weight: bold; margin-bottom: 15px;"),
          
          # 供应商筛选
          uiOutput("download_maker_ui"),  # 动态生成供应商筛选器,
          
          # 商品名称筛选
          selectizeInput(
            inputId = "download_item_name",
            label = "商品名称:",
            choices = NULL,          # 动态加载商品名称
            selected = NULL,         # 默认全选
            multiple = FALSE,        # 单选，适合精确匹配
            options = list(          # 提供更好的交互体验
              placeholder = "请输入商品名称...",
              create = FALSE         # 不允许用户输入新值
            ),
            width = "100%"
          ),
          
          # 采购日期筛选
          dateRangeInput(
            inputId = "download_date_range",
            label = "选择采购日期范围:",
            start = Sys.Date() - 365, # 默认最近365天
            end = Sys.Date(),        # 默认结束日期为今天
            format = "yyyy-mm-dd",   # 日期格式
            separator = " 至 ",
            width = "100%"
          ),
          
          actionButton("download_reset_filters", "重置筛选", class = "btn-secondary")
        ),
        
        tags$hr(),
        
        downloadButton(
          outputId = "download_summary_xlsx",
          label = "下载物品汇总表（按采购日期）",
          class = "btn-primary",
          style = "width: 100%; margin-top: 10px;"
        ),
        
        downloadButton(
          outputId = "download_details_xlsx",
          label = "下载物品明细表",
          class = "btn-primary",
          style = "width: 100%; margin-top: 10px;"
        )
      ),
      
      div(
        class = "resizable-divider",
      ),
      
      div(
        class = "main-panel",
        uniqueItemsTableUI("unique_items_table_download")
      )
    )
  ), # End of 数据下载 tab
  
  tabPanel(
    "管理员", icon = icon("user-shield"),
    div(
      class = "layout-container",
      div(
        class = "sticky-sidebar",
        passwordInput("admin_password", "请输入管理员密码：", width = "100%"),
        actionButton("admin_login_btn", "登录", icon = icon("unlock"), class = "btn-primary", style = "width: 100%; margin-top: 10px;"),
        tags$hr(),
        uiOutput("admin_controls")
      ),
      
      div(
        class = "resizable-divider",
      ),
      
      div(
        class = "main-panel",
        uniqueItemsTableUI("admin_items_table")  # 使用你的模组渲染物品明细表
      )
    )
  ) # End of 管理员 tab
)
