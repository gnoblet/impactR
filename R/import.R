# Helo import!

# All ufunctions to import csv and xlsx
# To-do:
# - add other imports maybe, .rds, .shp, etc.


#' @title Initial import with clean names (not an option!)
#'
#' @param path The project relative path to the csv file
#' @param delim Field separator, default is ","
#' @param uuid Character string. Should we rename "submission_uuid" to "uuid"?
#' @param ... Parameters to pass to readxl::read_xlsx or readr::read_delim
#'
#' @return A clean-names-and-types tibble
#'
#' @description Import a dataset from a CSV or a XLSX file. It ensures that the right type of column is guessed using the max value of guess_max
#'
#' @describeIn import_csv Import a .csv file
#'
#' @export
import_csv <- function(path, delim = ",", uuid = NULL, ...){

  if (rlang::is_missing(path)) {rlang::abort("Please provide the path to the CSV file")}
  if (!file.exists(path)) {rlang::abort("This CSV file does not exist")}

  df <- readr::read_delim(path, delim = delim, guess_max = 21474836, ...) |>
    readr::type_convert() |>
    janitor::clean_names()

  # Useful for kobo loops, change submission_uuid name to something, e.g. "uuid"
  if(!is.null(uuid)) {
    if(!("submission_uuid" %in% colnames(df))) {stop("'submission_uuid' is not a column name")}
    df <- df |> dplyr::rename(uuid = !!rlang::sym("submission_uuid"))
  }

  return(df)
}



# XLSX sheet initial import with clean names (not an option!)
#'
#' @param path The project relative path to the csv file
#' @param sheet A character string of the sheet name or the sheet position
#' @param uuid Character string. Should we rename "submission_uuid" to "uuid"?
#' @param ... Parameters to pass to readxl::read_xlsx or readr::read_delim
#'
#' @describeIn import_csv Import one sheet from a .xlsx
#'
#' @export
import_xlsx <- function(path, sheet = 1, uuid = NULL, ...){

  # Is path provided?
  if (rlang::is_missing(path)) {rlang::abort("Please provide the path to the CSV file")}
  # Does file exist?
  if (!file.exists(path)) {rlang::abort("This XLSX file does not exist")}

  # Read, type convert, clean names
  df <- readxl::read_xlsx(path, sheet = sheet, guess_max = 21474836, ...) |>
    readr::type_convert() |>
    janitor::clean_names()

  # Useful for kobo loops, change submission_uuid name to something, e.g. "uuid"
  if(!is.null(uuid)) {
    if_not_in_stop(df, "submission_uuid", "the dataset", "submission_uuid")
    df <- df |> dplyr::rename(uuid = !!rlang::sym("submission_uuid"))
  }

  return(df)
}



#' Entire XLSX initial import with clean names (not an option!)
#'
#' @param path The project relative path to the csv file
#' @param uuid Character string. Should we rename "submission_uuid" to "uuid"
#'
#' @describeIn import_csv Import all sheets from a .xlsx
#'
#' @export
import_full_xlsx <- function(path = NULL, uuid = NULL){

  # Is path provided?
  if (rlang::is_missing(path)) {rlang::abort("Please provide the path to the CSV file")}
  # Does file exist?
  if (!file.exists(path)) {rlang::abort("This XLSX file does not exist")}

  # Get sheet names
  sheet_names <- readxl::excel_sheets(path)

  # Map xlsx_import
  sheets <- purrr::map(sheet_names, \(x) import_xlsx(path, sheet = x, uuid = uuid))

  # Name sheets
  sheets <- purrr::set_names(sheets, sheet_names)

  return(sheets)
}
