uniqueItemsTableUI <- function(id) {
  ns <- NS(id)
  div(
    style = "flex-shrink: 0; padding-bottom: 10px; overflow-x: hidden; width: 100%;",
    DTOutput(ns("unique_items_table"))
  )
}