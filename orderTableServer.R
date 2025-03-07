orderTableServer <- function(input, output, session, column_mapping, selection = "single", data, 
                             options = modifyList(table_default_options, list(scrollY = "360px"))) {
  output$order_table <- renderDT({
    
    # 格式化数据
    formatted_data <- data() %>%
      mutate(
        created_at = format(
          as.POSIXct(created_at, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), # 将字符串转换为 POSIXct 格式
          "%y-%m-%d\n%H:%M:%S"  # 格式化为 25-01-01 (换行) 13:18:56
        )
      )
    
    # 初始化渲染表
    datatable_and_names <- render_table_with_images(
      data = formatted_data,                 # 使用传递的 reactive 数据源
      column_mapping = column_mapping, # 映射用户友好的列名
      selection = selection,
      image_column = "OrderImagePath", # 图片列映射
      options = options
    )
    
    # 获取数据列名
    column_names <- datatable_and_names$column_names
    table <- datatable_and_names$datatable
    
    # 运单状态字段高亮
    if ("运单PDF" %in% column_names) {
      table <- table %>%
        formatStyle(
          "运单PDF",
          backgroundColor = styleEqual(
            c("无", "已上传", "已打印"),
            c("#f6f2ff", "#95b3fc", "#b89afc")
          ),
          color = styleEqual(
            c("无", "已上传", "已打印"),
            c("black", "black", "black")
          )
        )
    }
    
    # 平台字段高亮
    if ("平台" %in% column_names) {
      table <- table %>%
        formatStyle(
          "平台",
          backgroundColor = styleEqual(
            c("Etsy", "Shopify", "TikTok", "其他"),
            c("#f45f0d", "#5a9a2a", "black", "lightgray")  # 不同平台的背景颜色
          ),
          color = styleEqual(
            c("Etsy", "Shopify", "TikTok", "其他"),
            c("white", "white", "white", "black")  # 字体颜色
          )
        )
    }
    
    # 状态字段高亮
    if ("状态" %in% column_names) {
      table <- table %>%
        formatStyle(
          "状态",
          backgroundColor = styleEqual(
            c('备货', '调货', '预定', '装箱', '发出', '在途', '送达', '取消'),
            c("#7881ff", "#a97afa", "#ff99ee", "#65d463", "#a6ffde", "#fae589", "#c2fcce", "#808080")  # 不同订单的背景颜色
          ),
          color = styleEqual(
            c('备货', '调货', '预定', '装箱', '发出', '在途', '送达', '取消'),
            c("black", "black", "black", "black", "black", "black", "black", "black")  # 字体颜色
          )
        )
    }
    table
  }, server = TRUE)
  
  # 监听用户点击图片列
  observeEvent(input$order_table_cell_clicked, {
    info <- input$order_table_cell_clicked
    
    # 检查是否点击了图片列（第二列）
    if (!is.null(info) && !is.null(info$col) && !is.null(info$row)) {
      if (info$col == 1) {  # 第二列在 R 中的索引是 1
        # 获取点击的图片路径
        img_path <- data()[info$row, "OrderImagePath"]
        req(img_path)  # 确保图片路径存在
        
        img_host_path <- paste0(host_url, "/images/", basename(img_path))
        
        # 弹出窗口显示大图
        showModal(modalDialog(
          title = "订单图片预览",
          tags$div(
            style = "overflow: auto; max-height: 700px; text-align: center;",
            tags$img(
              src = img_host_path,
              style = "max-width: 100%; height: auto; display: inline-block;"
            )
          ),
          size = "l",
          easyClose = TRUE,
          footer = NULL
        ))
      }
    }
  })
  
  # 返回选中行的索引
  reactive({
    input$order_table_rows_selected
  })
}
