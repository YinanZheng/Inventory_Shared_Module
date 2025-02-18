# Define UI
ui <- navbarPage(
  title = "ERP系统（国内端）",
  id = "inventory_cn",  # 设置 ID，用于监听当前选中的主页面
  theme = shinytheme("flatly"), # 可选主题
  position = "fixed-top",
  
  header = tagList(
    shinyjs::useShinyjs(),  # 启用 shinyjs

    # 加载动画界面
    tags$div(
      id = "loading-screen",
      style = "position: fixed; width: 100%; height: 100%; background: white; 
           z-index: 9999; display: flex; flex-direction: column; 
           justify-content: center; align-items: center; text-align: center;",
      
      # 旋转的毛线球 GIF
      tags$img(src = "https://www.goldenbeanllc.com/icons/spinning_yarn.gif", 
               style = "width: 80px; height: 80px;"),
      
      # 加载提示文字
      tags$p("系统加载中，请稍后...", 
             style = "font-size: 18px; font-weight: bold; color: #333; margin-top: 10px;")
    ),
    
    tags$head(
      tags$link(rel = "icon", type = "image/x-icon", href = "https://www.goldenbeanllc.com/icons/favicon-96x96.png"),
      
      tags$style(HTML("
      
      /* 强制导航栏支持水平滚动 */
      .navbar-nav {
        display: flex !important;
        flex-wrap: nowrap !important;
        overflow-x: auto !important;
        white-space: nowrap !important;
        max-width: 100% !important; /* 防止宽度限制 */
      }
      
      /* 导航栏滚动条样式 */
      .navbar-nav::-webkit-scrollbar {
        height: 6px;
      }
      .navbar-nav::-webkit-scrollbar-thumb {
        background: #007BFF;
        border-radius: 10px;
      }
      
      /* 强制显示滚动条，小于1420px时 */
      @media (max-width: 1420px) {
        .navbar-nav {
          overflow-x: scroll !important;
        }
        .navbar-brand {
          display: none !important; /* 隐藏标题 */
        }
      }
      
      /* 限制 .navbar 的宽度扩展 */
      .navbar {
        display: block !important;
        overflow: hidden !important;
        width: 100% !important;
      }
      
      /* 小屏幕调整字体和间距 */
      @media (max-width: 900px) {
        .navbar-nav > li > a {
          font-size: 12px !important;
          padding: 6px 8px !important;
        }
      }
      
      /* 为导航栏顶部留出空间 */
      body {
        padding-top: 70px !important;
      }
      
      /* --------------------------------------------------------- */

      /* Flexbox 容器 */
      .layout-container {
        display: flex;
        flex-direction: row;
        height: 100%;
        width: 100%;
        overflow: hidden; /* 禁止滚动条 */
      }
      
      .sticky-sidebar {
        position: sticky; /* 保持固定 */
        z-index: 900;
        flex: 0 0 auto; /* 固定宽度并防止被压缩 */
        width: 380px; /* 默认宽度 */
        min-width: 280px; /* 最小宽度 */
        max-width: 580px; /* 最大宽度 */
        height: calc(100vh - 70px); /* 自动计算高度 */
        overflow-y: auto; /* 滚动支持 */
        border-right: 1px solid #e0e0e0;
        border-radius: 8px;
        padding: 20px;
        background-color: #f9f9f9;
        transition: width 0.2s ease; /* 增加平滑过渡效果 */
      }
      
      .main-panel {
        flex-grow: 1;
        overflow: hidden; /* 禁止滚动条 */
        padding: 20px;
        padding-top: 0px;
        background-color: #ffffff;
        transition: width 0.2s ease; /* 增加平滑过渡效果 */
      }
      
      .resizable-divider {
        background-color: #aaa;
        width: 5px;
        cursor: ew-resize;
        flex-shrink: 0;
      }
    
      table.dataTable thead th {
        white-space: nowrap; /* 表头内容强制不换行 */
      } 
      
      /* DT 搜索框左对齐 */
      div.dataTables_wrapper div.dataTables_filter {
          text-align: left !important; /* 搜索框文字左对齐 */
          float: left !important;      /* 搜索框容器浮动到左侧 */
      }
      
      div.dataTables_wrapper div.dataTables_filter label {
        display: inline-flex;       /* 让标签和输入框同行 */
        align-items: center;       /* 垂直居中对齐 */
        gap: 5px;                  /* 间距调整 */
      }
      
      /* 采购流程链条箭头 */
      .arrow-icon {
        margin-right: 10px;
      }
      
      /* 预定单采购备忘小icon */
      .status-badge {
        display: inline-block;
        padding: 2px 8px;
        border-radius: 6px;
        font-size: 12px;
        font-weight: bold;
        color: white;
        text-align: center;
        margin-left: 10px;
        min-width: 24px;
      }
      .status-existing {
        background-color: #28A745; /* 绿色 */
      }
      .status-new {
        background-color: #FFA500; /* 橙色 */
      }
    ")),
      
      tags$script(HTML("
      // 系统加载画面淡出
      $(document).ready(function() {
        $('#loading-screen').css('transition', 'opacity 1s ease-out');
      });
      
      // 复制粘贴图片
      $(document).on('paste', '[id$=\"paste_area\"]', function(event) {
        const items = (event.originalEvent.clipboardData || event.clipboardData).items;
        for (let i = 0; i < items.length; i++) {
          if (items[i].type.indexOf('image') !== -1) {
            const file = items[i].getAsFile();
            const reader = new FileReader();
  
            reader.onload = function(evt) {
              // 使用 currentTarget 确保获取的是父级元素的 id
              const inputId = event.currentTarget.id + '_pasted_image';
              Shiny.setInputValue(inputId, evt.target.result, {priority: 'event'});
            };
  
            reader.readAsDataURL(file);
            break;
          }
        }
      });

      // JavaScript 实现分隔条拖拽
      document.addEventListener('DOMContentLoaded', function() {
        function enableResizing(divider) {
          const sidebar = divider.previousElementSibling;  // 分隔条左侧的 sidebar
          let isResizing = false;
    
          divider.addEventListener('mousedown', function(e) {
            isResizing = true;
            document.body.style.cursor = 'ew-resize';
            document.body.style.userSelect = 'none';
          });
    
          document.addEventListener('mousemove', function(e) {
            if (!isResizing) return;
            const newSidebarWidth = Math.max(200, Math.min(600, e.clientX)); // 限制宽度范围
            sidebar.style.flex = `0 0 ${newSidebarWidth}px`;
            
             // 调整所有表格列宽
            $('.dataTable').DataTable().columns.adjust();
          });
    
          document.addEventListener('mouseup', function() {
            if (isResizing) {
              isResizing = false;
              document.body.style.cursor = '';
              document.body.style.userSelect = '';
              
              // 再次确保表格布局正确
              $('.dataTable').DataTable().columns.adjust();
            }
          });
        }
    
        function bindResizableDividers() {
          document.querySelectorAll('.resizable-divider').forEach(function(divider) {
            if (!divider.dataset.bound) { // 避免重复绑定
              enableResizing(divider);
              divider.dataset.bound = true; // 标记为已绑定
            }
          });
        }
    
        bindResizableDividers();
    
        // 分页切换后重新绑定
        $(document).on('shown.bs.tab', function() {
          bindResizableDividers();
          $('.dataTable').DataTable().columns.adjust();
        });
      });
      
      // 成功音效
      function playSuccessSound() {
        var audio = new Audio('https://www.goldenbeanllc.com/sounds/success-8bit.mp3');
        audio.play();
      }
      
      // 错误音效
      function playErrorSound() {
        var audio = new Audio('https://www.goldenbeanllc.com/sounds/error-8bit.mp3');
        audio.play();
      }
      
      // 右键点击查询库存页面
      $(document).ready(function() {
        $('#filtered_inventory_table_query').on('contextmenu', 'tr', function(event) {
          event.preventDefault();
          var rowIdx = $(this).index();
          
          Shiny.setInputValue('selected_inventory_row', rowIdx + 1, {priority: 'event'});
    
          $('#context-menu').css({
            display: 'block',
            left: event.pageX + 'px',
            top: event.pageY + 'px'
          });
        });
    
        $(document).on('click', function(event) {
          if (!$(event.target).closest('#context-menu').length) {
            $('#context-menu').hide();
          }
        });
      });
    "))
    )
  ),
  
  tabPanel(
    "协作", icon = icon("users"),
    div(
      class = "layout-container",
      
      # 左侧侧边栏
      div(
        class = "sticky-sidebar",
        div(
          tags$h4("库存品请求", style = "font-weight: bold; color: #007BFF;"),
          fluidRow(
            column(6, textInput("search_sku", "按SKU搜索", placeholder = "输入SKU", width = "100%")),
            column(6, textInput("search_name", "按物品名搜索", placeholder = "输入物品名", width = "100%"))
          ),
          div(
            style = "margin-bottom: 10px;",
            div(
              style = "border: 1px solid #e0e0e0; border-radius: 8px; padding: 10px; background-color: #f9f9f9;",
              tags$h5("物品预览", style = "font-weight: bold; color: #007BFF;"),
              uiOutput("item_preview")
            )
          ),
          numericInput("request_quantity", "请求数量", value = 0, min = 1, width = "100%"),
          textAreaInput("request_remark", "留言", placeholder = "（选填）", width = "100%", height = "60px"),
          actionButton("add_request", "创建请求", icon = icon("plus"), class = "btn-success", style = "width: 100%; margin-top: 10px;"),
          tags$hr(),
          tags$h4("新商品请求", style = "font-weight: bold; color: #007BFF;"),
          imageModuleUI("image_requests", label = "请求物品图片上传"),
          textInput("custom_description", "物品名", placeholder = "输入物品名", width = "100%"),
          numericInput("custom_quantity", "请求数量", value = 0, min = 1, width = "100%"),
          textAreaInput("custom_remark", "留言", placeholder = "（选填）", width = "100%", height = "60px"),
          actionButton("submit_custom_request", "创建请求", icon = icon("plus"), class = "btn-success", style = "width: 100%; margin-top: 10px;")
        )
      ),
      
      # 可调整的分割线
      div(class = "resizable-divider"),
      
      # 右侧主要面板
      div(
        class = "main-panel",
        
        # 采购流程 tabset
        tabsetPanel(
          id = "collaboration_tabs",
          type = "pills",
          
          # 采购流程链
          tabPanel(
            title = "采购请求",
            uiOutput("purchase_request_board")
          ),
          tabPanel(
            title = div(
              tags$span(class = "arrow-icon", icon("arrow-right")),
              "已安排",
            ), 
            uiOutput("provider_arranged_board")
          ),
          tabPanel(
            title = div(
              tags$span(class = "arrow-icon", icon("arrow-right")),
              "已完成",
            ), 
            uiOutput("done_paid_board")
          ),
          
          # 出库请求
          tabPanel(
            title = "出库请求",
            uiOutput("outbound_request_board")
          )
        )
      )
    )
  ), # End of 协作 tab
  
  tabPanel(
    "采购", icon = icon("shopping-cart"),
    div(
      class = "layout-container",  # Flexbox 容器
      div(
        class = "sticky-sidebar",  # sticky 侧边栏
        itemFilterUI(id = "purchase_filter", border_color = "#28A745", text_color = "#28A745", use_purchase_date = FALSE, use_status = FALSE),
        
        tags$hr(),
        
        fluidRow(
          column(10, 
                 selectizeInput("new_maker", "供应商:", choices = NULL, width = "100%",
                                options = list(placeholder = '输入名称(拼音)', maxOptions = 500))
          ),
          column(2, 
                 div(style = "display: flex; justify-content: flex-start; align-items: center; height: 100%;", 
                     actionButton("add_supplier_btn", label = NULL, icon = icon("plus"), 
                                  style = "font-size: 14px; width: 100%; height: 34px; padding: 0px; margin-top: 26px;")
                 )
          )
        ),
        
        typeModuleUI("type_module"),
        
        fluidRow(
          column(12, autocompleteInputUI("purchase", label = "商品名：", placeholder = "请输入商品名...")),
          column(12,
                 h5("预订单物品备忘（点击自动填写）", style = "color: #17a2b8;"),
                 div(
                   style = "border: 1px solid #ddd; padding: 10px; border-radius: 5px; background-color: #f5f5f5; max-height: 150px; 
                   overflow-y: auto; margin-bottom: 15px;",
                   uiOutput("preorder_items_memo")
                 )
          ),
          column(12, dateInput(
            inputId = "purchase_date",
            label = "采购日期:",
            value = Sys.Date(),  # 默认日期为今天
            width = "100%"
          ))
        ),
        
        fluidRow(
          column(4, numericInput("new_quantity", "数量:", value = 0, min = 0, step = 1)),
          column(4, numericInput("new_product_cost", "单价:", value = 0, min = 0)),
          column(4, numericInput("new_shipping_cost", "运费", value = 0, min = 0))
        ),
        
        fluidRow(
          column(12,textInput("new_sku", "SKU(自动生成):", value = "", width = "100%"))
        ),
        
        imageModuleUI("image_purchase"),
        
        actionButton("reset_btn", "重置采购登记", icon = icon("snowplow"), class = "btn-danger", 
                     style = "font-size: 14px; width: 100%; height: 45px; padding: 0px; margin-top: 26px;")
      ),
      
      div(
        class = "resizable-divider",
      ),
      
      div(
        class = "main-panel",
        style = "display: flex; flex-direction: column; height: 100%;", # 主面板填充剩余空间
        div(
          style = "flex-shrink: 0;", # 防止标题区域被压缩
          div(
            tags$span(icon("shopping-cart"), style = "margin-right: 5px;"),  # 使用 span 包裹图标
            "采购箱", 
            style = "font-size: 18px; font-weight: bold; color: #333; background-color: #c3d8fa; padding: 10px; text-align: center; border-radius: 4px;"
          )
        ),
        
        div(
          style = "flex-shrink: 0; padding-bottom: 20px;", # 确保表格区域高度固定
          column(12, DTOutput("added_items_table"))
        ),
        
        div(
          style = "flex-shrink: 0; padding: 20px 13px;",  # 固定按钮区域的高度
          fluidRow(
            column(2, style = "text-align: left;", uiOutput("add_update_button_ui")),            
            column(2, div(style = "text-align: right;",actionButton("confirm_btn", "确认登记", icon = icon("check"), class = "btn-primary", style = "width: 100%;"))),
            column(2, actionButton("delete_btn", "删除选中", icon = icon("trash"), class = "btn-danger", style = "width: 100%;")),
            column(6, div(textOutput("total_cost"),style = "font-size: 20px; font-weight: bold; color: blue; text-align: center;"))
          )
        ),
        
        tags$hr(style = "margin: 20px 0; border: 1px solid #ddd;"),  # 添加分隔线
        
        div(
          id = "item_table_container_purchase",
          uniqueItemsTableUI("unique_items_table_purchase")
        )
      )
    )
  ), # end of 采购登记 tab
  
  tabPanel(
    "入库", icon = icon("arrow-circle-down"),
    div(
      class = "layout-container",  # Flexbox 容器
      div(
        class = "sticky-sidebar",  # sticky 侧边栏
        itemFilterUI(id = "inbound_filter", border_color = "#28A745", text_color = "#28A745", status_choices = c("全部" = "", "采购", "国内入库")),
        
        tags$hr(style = "margin: 5px 0; border: none;"),
        
        fluidRow(
          column(
            12,
            div(
              class = "card shadow-sm", # 添加卡片样式
              style = "border: 1px solid #007BFF; border-radius: 8px; padding: 20px; background-color: #f9f9f9;",
              
              # 卡片标题
              div(
                style = "margin-bottom: 10px; padding-bottom: 8px;",
                tags$h4("入库操作", style = "color: #007BFF; font-weight: bold; margin-bottom: 5px;"),
              ),
              
              # SKU 输入框
              div(
                style = "margin-bottom: 15px;",
                textInput(
                  "inbound_sku", 
                  label = NULL, 
                  placeholder = "请扫描或输入SKU",
                  width = "100%"
                ),
                checkboxInput(
                  "auto_inbound",  # 勾选框的 inputId
                  label = "自动入库（瑕疵信息不会采用）", 
                  value = FALSE  # 默认不勾选
                ),
                conditionalPanel(
                  condition = "input.auto_inbound == true",  # 只有 auto_inbound 被选中时才显示
                  checkboxInput("speak_inbound_item_name", "念出商品名", value = FALSE)
                ),
              ),
              
              div(
                style = "width: 100%;",
                numericInput(
                  inputId = "inbound_quantity",
                  label = "入库数量",
                  value = 1,        # 默认值
                  min = 1,          # 最小值
                  max = 1,
                  step = 1          # 步长
                )
              ),
              
              # 瑕疵品复选框
              div(
                style = "margin-bottom: 20px; display: flex; align-items: center;",
                tags$input(
                  type = "checkbox", 
                  id = "defective_item", 
                  style = "width: 20px; height: 20px; margin-right: 10px;"
                ),
                tags$label("瑕疵品", `for` = "defective_item", style = "font-size: 18px; font-weight: bold; color: #444;")
              ),
              
              div(
                id = "defective_notes_container",
                style = "display: none; margin-top: 10px;",
                textAreaInput(
                  inputId = "defective_notes",
                  label = "瑕疵品备注：",
                  placeholder = "请输入备注内容...",
                  width = "100%"
                )
              ),
              
              # 确认入库按钮
              actionButton(
                "confirm_inbound_btn", 
                "确认入库", 
                icon = icon("check"), 
                class = "btn-primary", 
                style = "font-size: 16px; width: 100%; height: 42px;"
              )
            )
          )
        ),
        
        tags$hr(style = "margin: 5px 0; border: none;"),
        
        fluidRow( 
          # 条形码生成下载按钮
          column(12,              
                 tags$div(
                   class = "card",
                   style = "padding: 15px; margin-bottom: 20px; border: 1px solid #007BFF; border-radius: 5px; box-shadow: 0 2px 5px rgba(0, 0, 0, 0.1);",
                   
                   # 卡片标题
                   div(
                     style = "margin-bottom: 10px; padding-bottom: 8px;",
                     tags$h4("条形码下载", style = "color: #007BFF; font-weight: bold; margin-bottom: 5px;"),
                   ),
                   
                   tags$div(
                     style = "display: flex; justify-content: space-between; align-items: center;",
                     actionButton("export_select_btn", "生成条形码", icon = icon("barcode"), class = "btn-info"),
                     downloadButton("download_select_pdf", "下载条形码", class = "btn-primary")
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
          style = "height: 300px; margin-bottom: 10px;",
          column(12, uiOutput("inbound_item_info"), style = "margin-bottom: 10px;") # 动态渲染物品信息
        ), 
        
        div(
          id = "item_table_container_inbound",
          uniqueItemsTableUI("unique_items_table_inbound")
        )
      )
    )
  ), # end of 入库 tab
  
  tabPanel(
    "出库", icon = icon("arrow-circle-up"),
    div(
      class = "layout-container",  # Flexbox 容器
      div(
        class = "sticky-sidebar",  # sticky 侧边栏
        itemFilterUI(id = "outbound_filter", border_color = "#28A745", text_color = "#28A745", status_choices = c("全部" = "", "国内入库", "国内出库")),
        
        tags$hr(style = "margin: 5px 0; border: none;"),
        
        div(
          class = "card",
          style = "margin-bottom: 20px; padding: 20px; border: 1px solid #007BFF; border-radius: 8px; box-shadow: 0px 4px 6px rgba(0,0,0,0.1);",
          tags$h4("出库操作", style = "color: #007BFF; font-weight: bold; margin-bottom: 15px;"),
          textInput("outbound_sku", NULL, placeholder = "请扫描条形码操作，并核对物品", width = "100%"),
          checkboxInput(
            "auto_outbound",  # 勾选框的 inputId
            label = "自动出库", 
            value = FALSE  # 默认不勾选
          ),
          conditionalPanel(
            condition = "input.auto_outbound == true",  # 只有 auto_outbound 被选中时才显示
            checkboxInput("speak_outbound_item_name", "念出商品名", value = FALSE)
          ),
          
          tags$div(
            class = "card",
            style = "padding: 15px; border: 2px solid #007BFF; border-radius: 8px; background-color: #f9f9f9; box-shadow: 0px 4px 6px rgba(0,0,0,0.1);",
            tags$h4("选择国际运输方式:", style = "font-size: 18px; font-weight: bold; margin-bottom: 15px;"),
            radioButtons(
              inputId = "outbound_shipping_method",
              label = NULL, # 将标签移到卡片标题
              choices = list("空运" = "空运", "海运" = "海运"),
              selected = "空运",  # 默认选择空运
              inline = TRUE       # 设置为横向排布
            )
          ),
          
          actionButton(
            "confirm_outbound_btn", 
            "确认出库", 
            icon = icon("check"), 
            class = "btn-primary", 
            style = "font-size: 16px; width: 100%; height: 42px; margin-top: 10px;"
          ),
          actionButton(
            "revert_outbound_btn",
            "撤回出库",
            icon = icon("undo"),
            class = "btn-warning",
            style = "font-size: 16px; width: 100%; height: 42px; margin-top: 10px;"
          )
        )
      ),
      
      div(
        class = "resizable-divider",
      ),
      
      div(
        class = "main-panel",
        div(
          style = "height: 300px; margin-bottom: 10px;",
          column(12, uiOutput("outbound_item_info"), style = "margin-bottom: 10px;") # 动态渲染物品信息
        ), 
        
        div(
          id = "item_table_container_outbound",
          uniqueItemsTableUI("unique_items_table_outbound")
        )
      )
    )
  ), # end of 出库 tab
  
  tabPanel(
    "售出", icon = icon("dollar-sign"),
    div(
      class = "layout-container",
      
      # 左侧：动态变化的筛选区和订单登记
      div(
        class = "sticky-sidebar",
        tabsetPanel(
          id = "filter_tabs",  # 主标签页 ID
          type = "pills",
          tabPanel(
            title = "物品筛选",
            itemFilterUI(id = "sold_filter", border_color = "#28A745", text_color = "#28A745",
                         status_choices = c("全部" = "", "国内入库", "国内出库", "美国入库", "美国调货", "国内售出"))
          ),
          tabPanel(
            title = "订单筛选",
            div(
              class = "card",
              style = "margin-bottom: 5px; padding: 15px; border: 1px solid #28A745; border-radius: 8px;",
              tags$h4("订单筛选", style = "color: #28A745; font-weight: bold;"),
              textInput("filter_order_id", NULL, placeholder = "订单号", width = "100%"),
              textInput("filter_tracking_id", NULL, placeholder = "运单号", width = "100%"),
              fluidRow(
                column(6, textInput("filter_customer_name", NULL, placeholder = "顾客姓名", width = "100%")),
                column(6, textInput("filter_customer_netname", NULL, placeholder = "顾客网名", width = "100%"))
              ),
              fluidRow(
                column(6, textInput("filter_sku", NULL, placeholder = "SKU", width = "100%")),
                column(6, autocompleteInputUI("sold", NULL, placeholder = "商品名"))
              ),
              fluidRow(
                column(12, textInput("filter_order_notes", NULL, placeholder = "订单备注", width = "100%")),
              ),
              fluidRow(
                column(12, dateRangeInput("filter_order_date", "订单创建时间", start = Sys.Date() - 90, end = Sys.Date() + 1, format = "yyyy-mm-dd", width = "100%"))
              ),
              fluidRow(
                column(6, selectInput("filter_platform", "电商平台", choices = c("所有平台" = "", "Etsy", "Shopify", "TikTok", "其他"),
                                      selected = "", width = "100%")),
                column(6, selectInput("filter_order_status", "订单状态", 
                                      choices = c("全部" = "", "备货", "预定", "调货", "装箱", "发出", "在途", "送达"),
                                      selected = "", width = "100%"))
              ),

              fluidRow(
                column(4, actionButton("delete_order_btn", "删除", class = "btn-danger", style = "width: 100%;")),
                column(4, actionButton("reset_filter_btn", "清空", class = "btn-info", style = "width: 100%;")),
                column(4, actionButton("refresh_orders", "刷新", class = "btn-secondary", style = "width: 100%;"))
              )
            )
          )
        ),
        
        tags$hr(style = "margin: 5px 0; border: none;"),
        
        # 订单登记区（共用）
        div(id = "orderForm",
          class = "card",
          style = "margin-bottom: 5px; padding: 15px; border: 1px solid #007BFF; border-radius: 8px; box-shadow: 0px 4px 6px rgba(0,0,0,0.1);",
          
          tags$h4("订单登记与更新", style = "color: #007BFF; font-weight: bold; margin-bottom: 15px;"),
          
          fluidRow(
            column(
              7,
              textInput("order_id", "订单号", placeholder = "请输入订单号", width = "100%")
            ),
            column(
              5,
              selectInput(
                inputId = "platform",
                label = "电商平台",
                choices = c(
                  "请选择" = "",
                  "Etsy" = "Etsy",
                  "Shopify" = "Shopify",
                  "TikTok" = "TikTok",
                  "其他" = "其他"
                ),
                selected = "",
                width = "100%"
              )
            )
          ),
          
          fluidRow(column(12, numericInput("transaction_amount", "总成交额（美元）", value = 0.00, min = 0, step = 0.01, width = "100%"))),
     
          textInput("customer_name", "顾客姓名", placeholder = "请输入或运单提取", width = "100%"),
          textInput("customer_netname", "顾客网名", placeholder = "请输入", width = "100%"),
          
          fluidRow(
            column(6, div(checkboxInput("is_transfer_order", "调货", value = FALSE))),
            column(6, div(checkboxInput("is_preorder", "预定", value = FALSE))),
          ),
          
          hidden(
            div(id = "preorder_fields",
                style = "border: 1px solid #ccc; padding: 10px; margin-bottom: 15px; display: flex; flex-direction: column; align-items: center;",
                selectizeInput("preorder_supplier", "预定单供应商", choices = NULL, width = "100%", options = list(placeholder = '填选供应商...')),
                textAreaInput(
                  inputId = "preorder_item_name",
                  label = "预定单商品名（每行一个品名）",
                  placeholder = "请输入或选择",
                  width = "100%",
                  rows = 3  # 设置初始显示的行数
                ),                
                selectizeInput(
                  inputId = "preorder_item_name_db",
                  label = NULL,
                  choices = NULL,
                  width = "100%",
                  options = list(
                    placeholder = "已有商品名列表"
                  )
                ),
                div(id = "preorder_image_preview",
                    style = "width: 200px; height: auto; margin-top: 10px; text-align: center;",
                    img(src = "", id = "preorder_img", style = "max-width: 100%; max-height: 200px; display: none; border: 1px solid #ddd; border-radius: 8px;")
                )
            )
          ),
          
          # 运单号
          textInput("tracking_number", "运单号", placeholder = "输入运单号或运单提取", width = "100%"),
          
          # 运单PDF 文件上传组件
          fileInput("shiplabel_pdf_upload", "上传运单PDF", accept = ".pdf", width = "100%"),
          uiOutput("upload_status_message"),
          
          tags$div(style = "margin-top: 20px;"),  # 增加20px垂直间距
          
          # 订单图片上传
          imageModuleUI("image_sold", label = "订单图片上传", label_color = "#007BFF"),
          
          # 订单备注
          textAreaInput("order_notes", "订单备注", placeholder = "请输入备注内容", width = "100%"),
          
          # 按钮区
          div(
            style = "margin-top: 10px; display: flex; flex-direction: column; gap: 5px;",  # 增加垂直间距
            
            div(
              style = "display: flex; justify-content: space-between;",
              uiOutput("register_order_button_ui"),
              actionButton(
                "clear_order_btn",
                "清空订单",
                icon = icon("eraser"),
                class = "btn-warning",
                style = "font-size: 16px; width: 48%; height: 42px;"
              )
            ),
            
            div(
              style = "margin-top: 5px; display: flex; justify-content: center;",  # 设置行间距
              actionButton(
                "merge_order_btn",
                "合并订单",
                icon = icon("object-group"),
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
      
      # 主面板：售出和订单管理的分页
      div(
        class = "main-panel",
        tabsetPanel(
          id = "sold_tabs",
          type = "pills",
          
          tabPanel(
            title = "物品售出",
            fluidRow(
              # 货架部分
              column(6,
                     div(
                       class = "card",
                       style = "padding: 10px; margin-bottom: 5px; border: 1px solid #007BFF; border-radius: 10px; box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);",
                       
                       div(
                         style = "display: flex; align-items: center; justify-content: space-between; margin-bottom: 0px; height: 30px; padding: 0px",
                         
                         # 货架标题和动态显示数量
                         tags$h4(
                           HTML(paste0(
                             as.character(icon("warehouse")), 
                             "  货架  ",
                             span(style = "display: inline-flex; color: #007BFF; font-size: 18px;", textOutput("shelf_count")) # 动态显示数量
                           )),
                           style = "color: #007BFF; font-weight: bold;"
                         ),
                      
                         # 使用 Unicode 显示箭头
                         div(
                           style = "display: flex;",  # 使用 Flex 布局让内容在同一行显示
                           tags$label("排序:", style = "margin-right: 10px; font-weight: bold; font-size: 14px;"),  # 添加排序标签
                           radioButtons(
                             inputId = "arrow_direction",
                             label = NULL,  # 去掉默认的 radioButtons 标签
                             choices = list("↑" = "up", "↓" = "down"),  # Unicode 上箭头和下箭头
                             selected = "up",  # 默认选中上箭头
                             inline = TRUE  # 横向排列
                           ),
                           tags$style(HTML("
                            #arrow_direction.form-group {
                              margin-bottom: 0 !important; /* 移除默认的 margin-bottom */
                            }
                            #arrow_direction .radio-inline {
                              margin-right: 10px; /* 调整每个选项的间距 */
                            }
                          "))
                         ),
                         
                         # SKU 输入栏
                         textInput(
                           inputId = "sku_to_shelf",
                           label = NULL,  # 不显示标签
                           placeholder = "扫码上架",  # 提示文字
                           width = "200px"  # 控制输入框宽度
                         ),
                         tags$style(HTML("
                          #sku_to_shelf {
                            height: 35px !important;  /* 调整高度 */
                            font-size: 15px;          /* 调整字体大小 */
                            padding: 5px;             /* 调整内边距 */
                          }
                        "))
                       ),
                       
                       DTOutput("shelf_table")  # 显示货架上的物品
                     )
              ),
              
              # 箱子部分
              column(6,
                     div(
                       class = "card",
                       style = "padding: 10px; margin-bottom: 5px; border: 1px solid #28A745; border-radius: 10px; box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);",
                       
                       div(
                         style = "display: flex; align-items: center; justify-content: space-between; margin-bottom: 0px; height: 30px",
                         
                         # 发货箱标题和动态显示数量
                         tags$h4(
                           HTML(paste0(
                             as.character(icon("box")), 
                             "  发货箱  ",
                             span(style = "display: inline-flex; color: #28A745; font-size: 18px;", textOutput("box_count")) # 动态显示数量
                           )),
                           style = "color: #28A745; font-weight: bold; margin: 0;"
                         ),
                         
                         # SKU 输入栏
                         textInput(
                           inputId = "sku_to_box",
                           label = NULL,  # 不显示标签
                           placeholder = "扫码入箱",  # 提示文字
                           width = "200px"  # 控制输入框宽度
                         ),
                         tags$style(HTML("
                          #sku_to_box {
                            height: 35px !important;  /* 调整高度 */
                            font-size: 15px;          /* 调整字体大小 */
                            padding: 5px;             /* 调整内边距 */
                          }
                        "))
                       ),
                       
                       DTOutput("box_table"),  # 显示已放入箱子的物品
                       
                       fluidRow(
                         column(
                           width = 6, # 左侧按钮宽度
                           actionButton(
                             "confirm_order_btn",
                             "确认售出",
                             icon = icon("check"),
                             class = "btn-primary",
                             style = "font-size: 16px; width: 100%; height: 50px; margin-top: 10px;"
                           )
                         ),
                         column(
                           width = 6, # 右侧选择框宽度
                           tags$div(
                             style = "
                              display: flex;
                              align-items: center;
                              justify-content: flex-start;
                              border: 1px solid #007BFF;
                              border-radius: 8px;
                              height: 50px;
                              padding: 0 10px;
                              margin-top: 10px;
                            ",
                             tags$span(
                               "国际运输:",
                               style = "font-size: 16px; font-weight: bold; margin-right: 15px; line-height: 1;"
                             ),
                             tags$div(
                               style = "
                                  display: flex;
                                  align-items: center;
                                  height: 100%;
                                  margin-bottom: 0; /* 移除底部间距 */
                                ",
                               tags$style(HTML("
                                  #sold_shipping_method .radio {
                                    margin-bottom: 0 !important; /* 移除默认的 margin */
                                  }
                                  #sold_shipping_method {
                                    margin-bottom: 0 !important; /* 避免容器本身多余间距 */
                                  }
                                ")),
                               radioButtons(
                                 inputId = "sold_shipping_method",
                                 label = NULL, # 去掉默认 label
                                 choices = list("空运" = "空运", "海运" = "海运"),
                                 selected = "空运",  # 默认选择空运
                                 inline = TRUE       # 设置为横向排布
                               )
                             )
                           )
                         )
                       )
                     )
              )
            ),
            
