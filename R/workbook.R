#' @title Export a clean workbook
#'
#' @param named_list A named list of tibbles/dataframes.
#' @param path A full or relative path that includes file name.
#' @param col_width Minimum column width. Default to 15.
#' @param max_width Maximum column width. Default to 60.
#' @param font_name Font name. Default to "Leelawadee".
#' @param font_size Font size. Default to 10.
#' @param table_style Excel table style. Default to "TablestyleMedium10".
#'
#' @description
#'
#' The clean_workbook functions exports a list or a dataframe/tibble to nice xlsx file using openxlsx.
#'
#' * `clean_workbook_list()` takes a named_list as main arg and writes a xlsx file where each list item is a tab.
#'
#' * `clean_workbook_df()` takes a dataframe as main arg and writes a single-tabbed xlsx file.
#'
#' @describeIn clean_workbook_list Export a clean workbook from a named list of dataframes
#'
#' @return A written xlsx file
#'
#' @export
clean_workbook_list <- function(named_list, path, col_width = 15, max_width = 60, font_name = "Leelawadee", font_size = 10, table_style = "TablestyleMedium10"){

  rlang::check_installed("openxlsx", reason = "to use `clean_workbook_list()`")

  wb <- openxlsx::createWorkbook()

  purrr::iwalk(named_list, function(.x, .y) {
    openxlsx::addWorksheet(wb, .y)
    openxlsx::writeDataTable(wb, .y, .x, tableStyle = table_style)

    widths <- purrr::map(.x, ~ max(nchar(.x, keepNA = FALSE)), na.rm = F)
    widths <- ifelse(is.na(widths), 0, widths)
    widths_above_col_width <- widths[widths >= col_width]
    widths_above_max_width <- widths_above_col_width[widths_above_col_width >= max_width]


    openxlsx::setColWidths(wb, .y, widths = col_width, cols = 1:ncol(.x))
    openxlsx::setColWidths(wb, .y, widths = "auto", cols = which(colnames(.x) %in% names(widths_above_col_width)))
    openxlsx::setColWidths(wb, .y, widths = max_width, cols = which(colnames(.x) %in% names(widths_above_max_width)))
  })

  openxlsx::modifyBaseFont(wb, fontSize = font_size, fontName = font_name)

  openxlsx::saveWorkbook(wb, file = path, overwrite = TRUE)
}


#' @title Export a clean workbook
#'
#' @param df Dataframe to export
#' @param name Character string if the spreadsheet name
#' @param path A full full or relative path that includes file name.
#' @param col_width Minimum column width. Default to 15.
#' @param max_width Maximum column width. Default to 60.
#' @param font_name Font name. Default to "Leelawadee".
#' @param font_size Font size. Default to 10.
#' @param table_style Excel table style. Default to "TablestyleMedium10".
#'
#' @describeIn clean_workbook_list Export a clean workbook from a single dataframe
#'
#' @export
clean_workbook_df <- function(df, name, path, col_width = 15, max_width = 60, font_name = "Leelawadee", font_size = 10, table_style = "TablestyleMedium10"){

  rlang::check_installed("openxlsx", reason = "to use `clean_workbook_df()`")


  wb <- openxlsx::createWorkbook()

  openxlsx::addWorksheet(wb, name)
  openxlsx::writeDataTable(wb, name, df, tableStyle = table_style)

  widths <- purrr::map(df, ~ max(nchar(.x, keepNA = FALSE)), na.rm = F)
  widths <- ifelse(is.na(widths), 0, widths)
  widths_above_col_width <- widths[widths >= col_width]
  widths_above_max_width <- widths_above_col_width[widths_above_col_width >= max_width]


  openxlsx::setColWidths(wb, name, widths = col_width, cols = 1:ncol(df))
  openxlsx::setColWidths(wb, name, widths = "auto", cols = which(colnames(df) %in% names(widths_above_col_width)))
  openxlsx::setColWidths(wb, name, widths = max_width, cols = which(colnames(df) %in% names(widths_above_max_width)))


  openxlsx::modifyBaseFont(wb, fontSize = font_size, fontName = font_name)

  openxlsx::saveWorkbook(wb, file = path, overwrite = TRUE)
}
