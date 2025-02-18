            tags$hr(style = "margin: 5px 0; border: 1px solid #ddd;"),  # 添加分隔线

            div(
              id = "item_table_container_sold",
              uniqueItemsTableUI("unique_items_table_sold")
            )
          ),
          tabPanel(
            title = "订单管理",
            div(
              class = "card",
              style = "height: 460px; padding: 5px; border: 1px solid #ccc; border-radius: 8px;", # 自动调整高度
              orderTableUI("orders_table_module")
            ),
            div(
              class = "card",
              style = "padding: 5px; border: 1px solid #ccc; border-radius: 8px;", # 自动调整高度
              uiOutput("associated_items_title"),  # 动态标题
              uiOutput("order_items_cards")  # 动态显示订单内物品卡片
            )
          )
        )
      )
    )
  ), # End of 售出
  
  
  tabPanel(
    "物品管理", icon = icon("list-check"),
    div(
      class = "layout-container",  # Flexbox 容器
      div(
        class = "sticky-sidebar",  # sticky 侧边栏
        itemFilterUI(id = "manage_filter", border_color = "#28A745", text_color = "#28A745", use_purchase_date = TRUE),
        
        tags$hr(), # 分隔线
        
        # 添加 TabsetPanel 组织不同功能
        tabsetPanel(
          id = "manage_tabs",
          type = "pills",
          tabPanel(
            "更新图片", icon = icon("image"),  # 图标
            div(
              class = "card shadow-sm", # 添加卡片样式
              style = "border: 1px solid #e0e0e0; border-radius: 8px; padding: 20px; background-color: #f9f9f9;",
              # 添加说明
              tags$p("请点选一行（一种商品）进行图片更新。", 
                     style = "font-size: 14px; color: #6c757d; margin-bottom: 10px;"),
              
              imageModuleUI("image_manage", label = "更新商品图片"),
              actionButton("update_image_btn", "更新图片", icon = icon("pen"), style = "background-color: #006400; color: white; width: 100%;")
            )
          ),
          tabPanel(
            "更新信息", icon = icon("edit"),  # 图标
            div(
              class = "card shadow-sm", # 添加卡片样式
              style = "border: 1px solid #e0e0e0; border-radius: 8px; padding: 20px; background-color: #f9f9f9;",
              
              # 添加说明
              tags$p("请点选一行或多行进行信息更新。", 
                     style = "font-size: 14px; color: #6c757d; margin-bottom: 10px;"),
              
              fluidRow(
                column(12, numericInput("update_product_cost", "修改单价", value = NULL, min = 0, width = "100%")),
                column(12, numericInput("update_shipping_cost", "修改国内运费", value = NULL, min = 0, width = "100%")),
                column(12, dateInput("update_purchase_date", "修改采购日期", value = Sys.Date(), width = "100%"))
              ),
              fluidRow(
                column(6, actionButton("update_info_btn", "更新信息", icon = icon("pen"), style = "background-color: #006400; color: white; width: 100%;")),
                column(6, actionButton("clear_info_btn", "清空", icon = icon("eraser"), style = "background-color: #8B0000; color: white; width: 100%;"))
              )
            )
          ),
          tabPanel(
            "删除", icon = icon("trash"),  # 图标
            div(
              class = "card shadow-sm", # 添加卡片样式
              style = "border: 1px solid #e0e0e0; border-radius: 8px; padding: 20px; background-color: #f9f9f9;",
              
              # 添加说明
              tags$p("请点选一行或多行物品，支持批量删除。", 
                     style = "font-size: 14px; color: #6c757d; margin-bottom: 10px;"),
              
              actionButton(
                "confirm_delete_btn",
                "确认删除",
                icon = icon("check"),
                class = "btn-primary",
                style = "font-size: 16px; width: 100%; height: 42px;"
              )
            )
          )
        )
      ),
      
      div(
        class = "resizable-divider",
      ),
      
      div(
        class = "main-panel",

        div(
          id = "item_table_container_manage",
          uniqueItemsTableUI("unique_items_table_manage")
        )
      )
    )
  ), # end of 物品管理 tab
  
  tabPanel(
    "瑕疵品管理", icon = icon("exclamation-circle"),
    div(
      class = "layout-container",  # Flexbox 容器
      div(
        class = "sticky-sidebar",  # sticky 侧边栏
        itemFilterUI(id = "defect_filter", border_color = "#28A745", text_color = "#28A745", use_status = FALSE, use_purchase_date = TRUE),
        
        tags$hr(), # 分隔线
        
        # 登记瑕疵品部分
        div(
          style = "margin-bottom: 20px;",
          actionButton(
            "register_defective", 
            "登记瑕疵品", 
            icon = icon("circle-exclamation"),
            class = "btn-warning", 
            style = "font-size: 16px; width: 100%;"
          )
        ),
        div(
          style = "margin-bottom: 20px;",
          tags$label("仅显示无瑕品", class = "control-label"),  
          switchInput(
            inputId = "show_perfects_only",  # 开关 ID
            label = NULL,                   # 不显示标签在开关上
            value = FALSE                   # 默认值：关闭
          )
        ),
        
        tags$hr(), # 分隔线
        
        # 登记修复品部分
        div(
          style = "margin-bottom: 20px;",
          actionButton(
            "register_repair", 
            "登记修复品", 
            icon = icon("hammer"),
            class = "btn-success", 
            style = "font-size: 16px; width: 100%;"
          )
        ),
        div(
          style = "margin-bottom: 20px;",
          tags$label("仅显示瑕疵品", class = "control-label"),  
          switchInput(
            inputId = "show_defects_only",  # 开关 ID
            label = NULL,                   # 不显示标签在开关上
            value = FALSE                   # 默认值：关闭
          )
        ),
        
        tags$hr(), # 分隔线
        
        # 备注输入框
        textAreaInput(
          inputId = "manage_defective_notes",
          label = "备注：",
          placeholder = "请输入备注内容...",
          width = "100%"
        )
      ),
      
      div(
        class = "resizable-divider",
      ),
      
      # 主面板：物品状态表
      div(
        class = "main-panel",
        
        div(
          id = "item_table_container_defect",
          uniqueItemsTableUI("unique_items_table_defect")
        )
      )
    )
  ), # end of 瑕疵品管理 tab
  
  tabPanel(
    "国际物流管理", icon = icon("globe"),
    div(
      class = "layout-container",
      div(
        class = "sticky-sidebar",
        itemFilterUI(id = "logistic_filter", 
                     use_purchase_date = FALSE,
                     use_sold_date = TRUE, use_exit_date = TRUE, use_status = FALSE,
                     border_color = "#28A745", text_color = "#28A745"),
        
        tags$hr(style = "margin: 5px 0; border: none;"),
        
        tabsetPanel(
          id = "intl_shipment_tabs",
          type = "pills",
          
          # 第一个 Tab：登记国际运单
          tabPanel(
            title = tagList(icon("file-alt"), "登记国际运单"),
            value = "register_shipment",
            div(
              class = "card shadow-sm",
              style = "padding: 10px; border: 1px solid #007BFF; border-radius: 8px; box-shadow: 0px 4px 6px rgba(0,0,0,0.1);",
              
              # Card 标题
              tags$h4("登记国际运单", style = "color: #007BFF; font-weight: bold; margin-bottom: 15px;"),
              
              # 输入内容
              fluidRow(
                column(12, textInput("intl_tracking_number", "国际运单号:", placeholder = "请输入空运或海运运单号", width = "100%")),
                column(12, textOutput("intl_status_display"), style = "color: blue; font-weight: bold; margin-bottom: 20px;"),
                column(12, selectInput("intl_shipping_method", "国际运输方式:", choices = c("空运" = "空运", "海运" = "海运"), selected = "空运", width = "100%")),
                column(12, numericInput("intl_total_shipping_cost", "国际物流总运费 (元):", value = 0, min = 0, width = "100%"))
              ),
              
              # 按钮
              fluidRow(
                column(6, actionButton("register_shipment_btn", "登记运单", icon = icon("save"), class = "btn-primary", style = "margin-top: 20px; width: 100%; font-size: 16px;")),
                column(6, actionButton("batch_value_btn", "包裹货值", icon = icon("dollar-sign"), class = "btn-success", style = "margin-top: 20px; width: 100%; font-size: 16px;"))
              ),
              fluidRow(
                column(6, actionButton("delete_shipment_btn", "删除运单", icon = icon("trash"), class = "btn-danger", style = "margin-top: 20px; width: 100%; font-size: 16px;")),
                column(6, actionButton("clean_shipment_btn", "清空填写", icon = icon("trash"), class = "btn-info", style = "margin-top: 20px; width: 100%; font-size: 16px;"))
              )
            )
          ),
          
          # 挂靠管理
          tabPanel(
            title = tagList(icon("link"), "挂靠管理"),
            value = "link_management",  # 添加唯一标识值
            div(
              class = "card shadow-sm",
              style = "padding: 10px; border: 1px solid #28A745; border-radius: 8px; box-shadow: 0px 4px 6px rgba(0,0,0,0.1);",
              
              # Card 标题
              tags$h4("挂靠管理", style = "color: #28A745; font-weight: bold; margin-bottom: 15px;"),
              
              fluidRow(
                column(12, textInput("intl_link_tracking_number", "", placeholder = "请输入要挂靠的运单号", width = "100%")),
                column(12, htmlOutput("intl_link_display"), style = "color: blue; font-weight: bold; margin-bottom: 20px;")
              ),
              
              # 挂靠和解除挂靠按钮
              fluidRow(
                column(6, actionButton("link_tracking_btn", "挂靠运单", icon = icon("link"), class = "btn-primary", style = "margin-top: 20px; width: 100%;", disabled = TRUE)),
                column(6, actionButton("unlink_tracking_btn", "解除挂靠", icon = icon("link-slash"), class = "btn-danger", style = "margin-top: 20px; width: 100%;", disabled = TRUE))
              )
            )
          )
        )
      ),
      
      div(
        class = "resizable-divider",
      ),
      
      div(
        class = "main-panel",
        
        div(
          id = "item_table_container_logistics",
          uniqueItemsTableUI("unique_items_table_logistics")
        )
      )
    )
  ), # end of 国际物流管理 tab
  
  tabPanel(
    "账务管理", icon = icon("wallet"),
    div(
      class = "layout-container",
      div(
        class = "sticky-sidebar",
        tabsetPanel(
          id = "sidebar_tabs",  # 用于服务器监听当前选中的分页
          type = "pills",
          selected = "账务登记", # 默认选中的分页
          
          # 账务登记分页
          tabPanel(
            title = "账务登记", icon = icon("file-invoice-dollar"),
            tags$h4("账务登记", style = "color: #007BFF; font-weight: bold; margin-bottom: 15px;"),
            
            fluidRow(
              column(7, numericInput("amount", "金额:", value = 0, min = 0, width = "100%")),
              column(5,  
                radioButtons(
                  inputId = "transaction_type",
                  label = "交易类型:",
                  choices = c("转出" = "out", "转入" = "in"),
                  selected = NULL,
                  inline = FALSE
                )
              )
            ),
            
            # 指定转款选择器
            fluidRow(
              column(12, dateInput("custom_date", "转款日期:", value = Sys.Date(), width = "100%")),
              column(12, timeInput("custom_time", "转款时间:", value = format(Sys.time(), "%H:%M:%S"), width = "100%"))
            ),
            
            # 转账种类下拉菜单
            fluidRow(
              column(12, selectInput(
                inputId = "transaction_category",
                label = "转账类别:",
                choices = c("采购", "税费", "杂费", "工资", "债务", "社保", "图解", "其他"),
                selected = "其他",
                width = "100%"
              ),
              div(
                style = "margin-top: 8px; padding: 10px; background-color: #f9f9f9; margin-bottom: 10px; border: 1px solid #ddd; border-radius: 4px; font-size: 14px; color: #555;",
                textOutput("transaction_category_note", inline = TRUE)
              ))
            ),
            
            textAreaInput("remarks", "备注:", placeholder = "请输入备注内容", width = "100%"),

            # 转账证据图片上传
            imageModuleUI("image_transactions", label = "转账证据上传", label_color = "#007BFF"),
            
            # 提交按钮
            actionButton("record_transaction", "登记", icon = icon("save"), 
                         class = "btn-primary", style = "width: 100%; margin-bottom: 10px;"),
            
            # 删除和重置按钮同一行
            fluidRow(
              column(
                width = 6,
                actionButton("delete_transaction", "删除选中行", icon = icon("trash"), 
                             class = "btn-danger", style = "width: 100%;")
              ),
              column(
                width = 6,
                actionButton("reset_form", "重置", icon = icon("redo"), 
                             class = "btn-info", style = "width: 100%;")
              )
            )
          ),
          
          # 资金转移分页
          tabPanel(
            title = "资金转移", icon = icon("exchange-alt"),
            tags$h4("资金转移", style = "color: #28A745; font-weight: bold; margin-bottom: 15px;"),
            
            # 转移金额输入框
            numericInput("transfer_amount", "转移金额:", value = 0, min = 0, width = "100%"),
            
            # 转出账户选择
            selectInput(
              inputId = "from_account",
              label = "转出账户:",
              choices = c("工资卡", "美元卡", "买货卡", "一般户卡"),
              selected = "美元卡",
              width = "100%"
            ),
            
            # 转入账户选择
            selectInput(
              inputId = "to_account",
              label = "转入账户:",
              choices = c("工资卡", "美元卡", "买货卡", "一般户卡"),
              selected = NULL,
              width = "100%"
            ),
            
            # 转账种类下拉菜单
            fluidRow(
              column(12, selectInput(
                inputId = "transfer_category",
                label = "转账类别:",
                choices = c("采购", "税费", "杂费", "工资", "债务", "社保", "其他"),
                selected = "其他",
                width = "100%"
              ),
              div(
                style = "margin-top: 8px; padding: 10px; background-color: #f9f9f9; margin-bottom: 10px; border: 1px solid #ddd; border-radius: 4px; font-size: 14px; color: #555;",
                textOutput("transfer_category_note", inline = TRUE)
              ))
            ),
            
            # 备注输入框
            textAreaInput("transfer_remarks", "备注:", placeholder = "请输入备注内容", width = "100%"),
            
            # 转账证据图片上传
            imageModuleUI("image_transfer", label = "转账证据上传", label_color = "#28A745"),
  
            # 转移登记按钮
            actionButton("record_transfer", "记录转移", icon = icon("exchange-alt"), 
                         class = "btn-success", style = "width: 100%; margin-bottom: 10px;"),
            
            # 删除和重置按钮同一行
            fluidRow(
              column(
                width = 6,
                actionButton("delete_transfer", "删除选中行", icon = icon("trash"), 
                             class = "btn-danger", style = "width: 100%;")
              ),
              column(
                width = 6,
                actionButton("reset_form_transfer", "重置", icon = icon("redo"), 
                             class = "btn-info", style = "width: 100%;")
              )
            )
          )
        )
      ),
      
      div(
        class = "resizable-divider",
      ),
      
      div(
        class = "main-panel",
        tabsetPanel(
          id = "transaction_tabs",  # 绑定到 input$tabs
          type = "pills",
          tabPanel("账户余额总览", 
          fluidRow(
           column(12, div(
             class = "card shadow-lg",
             style = "background: #1F1F1F; color: white; padding: 40px; text-align: center; border-radius: 16px; margin-top: 20px; margin-bottom: 40px; border: 2px solid #FFC107;",
             tags$h4("总余额", style = "font-weight: bold; font-size: 30px; margin-bottom: 20px; letter-spacing: 1.5px;"),
             tags$h3(
               textOutput("total_balance"),
               style = "font-size: 40px; margin-top: 0; font-weight: bold; text-shadow: 2px 2px 4px rgba(255, 193, 7, 0.8); color: #FFC107;"
             )
           ))
          ),
          fluidRow(
            column(3, div(
              class = "card shadow-lg",
              style = "background: linear-gradient(135deg, #FFC107, #FF9800); color: white; padding: 20px; text-align: center; border-radius: 16px; position: relative; overflow: hidden;",
              tags$div(
                style = "position: absolute; top: -10px; left: -10px; opacity: 0.3;",
                tags$img(src = "https://dummyimage.com/100x100/fff/000.png&text=$", width = "60px", height = "60px")
              ),
              tags$h4("买货卡 (139)", style = "font-weight: bold; margin-bottom: 10px;"),
              tags$h3(textOutput("purchase_balance"), style = "font-size: 24px; margin-top: 0;")
            )),
            column(3, div(
              class = "card shadow-lg",
              style = "background: linear-gradient(135deg, #6C757D, #495057); color: white; padding: 20px; text-align: center; border-radius: 16px; position: relative; overflow: hidden;",
              tags$div(
                style = "position: absolute; top: -10px; left: -10px; opacity: 0.3;",
                tags$img(src = "https://dummyimage.com/100x100/fff/000.png&text=$", width = "60px", height = "60px")
              ),
              tags$h4("一般户卡 (541)", style = "font-weight: bold; margin-bottom: 10px;"),
              tags$h3(textOutput("general_balance"), style = "font-size: 24px; margin-top: 0;")
            )),
            column(3, div(
              class = "card shadow-lg",
              style = "background: linear-gradient(135deg, #007BFF, #0056b3); color: white; padding: 20px; text-align: center; border-radius: 16px; position: relative; overflow: hidden;",
              tags$div(
                style = "position: absolute; top: -10px; left: -10px; opacity: 0.3;",
                tags$img(src = "https://dummyimage.com/100x100/fff/000.png&text=$", width = "60px", height = "60px")
              ),
              tags$h4("工资卡 (567)", style = "font-weight: bold; margin-bottom: 10px;"),
              tags$h3(textOutput("salary_balance"), style = "font-size: 24px; margin-top: 0;")
            )),
            column(3, div(
              class = "card shadow-lg",
              style = "background: linear-gradient(135deg, #28A745, #1E7E34); color: white; padding: 20px; text-align: center; border-radius: 16px; position: relative; overflow: hidden;",
              tags$div(
                style = "position: absolute; top: -10px; left: -10px; opacity: 0.3;",
                tags$img(src = "https://dummyimage.com/100x100/fff/000.png&text=$", width = "60px", height = "60px")
              ),
              tags$h4("美元卡 (553)", style = "font-weight: bold; margin-bottom: 10px;"),
              tags$h3(textOutput("dollar_balance"), style = "font-size: 24px; margin-top: 0;")
            ))
          )),
          tabPanel(title = "买货卡(139)", value = "买货卡", DTOutput("purchase_card_table")),
          tabPanel(title = "一般户卡(541)", value = "一般户卡", DTOutput("general_card_table")),
          tabPanel(title = "工资卡(567)", value = "工资卡", DTOutput("salary_card_table")),
          tabPanel(title = "美元卡(553)", value = "美元卡", DTOutput("dollar_card_table"))
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
        ),
        div(
          class = "card",
          style = "margin-bottom: 20px; padding: 20px; border: 1px solid #DC3545; border-radius: 8px; box-shadow: 0px 4px 6px rgba(0,0,0,0.1);",
          tags$h4("售罄物品", style = "color: #DC3545; font-weight: bold; margin-bottom: 15px;"),
          radioButtons(
            inputId = "query_stock_status",
            label = NULL,  # 不显示默认标题，使用 h4 作为标题
            choices = c("不过滤" = "none", "美国售罄, 国内有货" = "us", "国内售罄, 美国有货" = "domestic", "全库存售罄" = "all"),
            selected = "none",  # 默认选择 “不过滤”
            inline = FALSE
          )
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
                  style = "height: 373px; margin-bottom: 5px; padding: 5px; border: 1px solid #007BFF; border-radius: 8px; box-shadow: 0px 4px 6px rgba(0,0,0,0.1);",
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
                  id = "context-menu",
                  style = "display: none; position: absolute; background: white; border: 1px solid #ccc; box-shadow: 0px 4px 6px rgba(0,0,0,0.2); padding: 5px; border-radius: 5px; z-index: 1000;",
                  actionButton("query_purchase_request", "采购请求", class = "btn btn-primary btn-sm", style = "width: 100%; margin-bottom: 5px;"),
                  uiOutput("query_outbound_request_btn")  # 动态生成出库请求按钮                
                ),
                
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
                             end = Sys.Date() + 1
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
                  uiOutput("confirm_expense_check_ui"),
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
                    tags$p("物品总数（已送达）")
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
            end = Sys.Date() + 1,        # 默认结束日期为今天
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
