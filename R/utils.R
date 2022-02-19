# Hello, utils!
#
# Here are some utils for reading dataset, removing some variables,
# recoding some variables, and misc.



#' @title Abord bad argument
#'
#' @param arg An argument
#' @param must What arg must be
#' @param not Optional. What arg must not be.
#'
#' @return A stop statement
abort_bad_argument <- function(arg, must, not = NULL) {
  msg <- glue::glue("`{arg}` must {must}")
  if (!is.null(not)) {
    not <- typeof(not)
    msg <- glue::glue("{msg}; not {not}.")
  }

  rlang::abort("error_bad_argument",
        message = msg,
        arg = arg,
        must = must,
        not = not
  )
}



#' @title Stop statement "If not in colnames" with colnames
#'
#' @param .tbl A tibble
#' @param cols A vector of column names (quoted)
#' @param df Provide the tibble name as a character string
#' @param arg Default to NULL.
#'
#' @return A stop statement
if_not_in_stop <- function(.tbl, cols, df, arg = NULL){
  if (is.null(arg)) {
    msg <- glue::glue("following column/s is/are missing in `{df}`:")
  }
  else {
    msg <- glue::glue("following column/s from `{arg}` is/are missing in `{df}`:")
  }
  if (!all(cols %in% colnames(.tbl))) {
    rlang::abort(
      paste(
        msg,
        paste(
          subvec_not_in(cols, colnames(.tbl)),
          collapse = ", ")
      )
    )
  }
}



#' @title Stop statement "If not in vector"
#'
#' @param vec A vector of character strings
#' @param cols A vector of column names (quoted)
#' @param vec_name Provide the vector name as a character string
#' @param arg Default to NULL.
#'
#' @return A stop statement
if_vec_not_in_stop <- function(vec, cols, vec_name, arg = NULL){
  if (is.null(arg)) {
    msg <- glue::glue("following element/s is/are missing in `{vec_name}`:")
  }
  else {
    msg <- glue::glue("following element/s from `{arg}` is/are missing in `{vec_name}`:")
  }
  if (!all(cols %in% vec)) {
    rlang::abort(
      paste(
        msg,
        paste(
          subvec_not_in(cols, vec),
          collapse = ", ")
      )
    )
  }
}




#' @title Subvec in
#'
#' @param vector A vector to subset
#' @param set A set-vector
#'
#' @return A subset of a list or a vector
#'
#' @export
subvec_in <- function(vector, set){
  vector[vector %in% set]
}

#' @title Subvec not in
#'
#' @param vector A vector to subset
#' @param set A set-vector
#'
#' @return A subset of vector not in set
#'
#' @export
subvec_not_in <- function(vector, set){
  vector[!(vector %in% set)]
}



#' @title Initial import with clean names (not an option!)
#'
#' @param path The project relative path to the csv file
#' @param delim Field separator, default is ","
#' @param uuid Character string. Should we rename "submission_uuid" to "uuid"?
#' @param ... Parameters to pass to readxl::read_xlsx or readr::read_delim
#'
#' @return A clean-names-and-types tibble
#'
#' @description Import a dataset from a CSV or a XLSX file.
#'
#' @describeIn import_csv Import a .csv file
#'
#' @export
import_csv <- function(path, delim = ",", uuid = NULL, ...){

  if (rlang::is_missing(path)) {stop("Please provide the path to the CSV file.")}
  if (!file.exists(path)) {stop("This CSV file does not exist.")}

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
  if (rlang::is_missing(path)) {stop("Please provide the path to the CSV file.")}
  # Does file exist?
  if (!file.exists(path)) {stop("This XLSX file does not exist.")}

  # Read, type convert, clean names
  df <- readxl::read_xlsx(path, sheet = sheet, guess_max = 21474836, ...) |>
    readr::type_convert() |>
    janitor::clean_names()

  # Useful for kobo loops, change submission_uuid name to something, e.g. "uuid"
  if(!is.null(uuid)) {
    if(!("submission_uuid" %in% colnames(df))) {stop("'submission_uuid' is not a column name")}
    df <- df |> dplyr::rename(uuid = !!rlang::sym("submission_uuid"))
  }

  return(df)
}



#' Entire XLSX initial import with clean names (not an option!)
#'
#' @param path The project relative path to the csv file
#' @param uuid Character string. Should we rename "submission_uuid" to "uuid"
#' @param ... Parameters to pass to readxl::read_xlsx or readr::read_delim
#'
#' @describeIn import_csv Import all sheets from a .xlsx
#'
#' @export
import_full_xlsx <- function(path = NULL, uuid = NULL, ...){

  # Is path provided?
  if (rlang::is_missing(path)) {stop("Please provide the path to the CSV file.")}
  # Does file exist?
  if (!file.exists(path)) {stop("This XLSX file does not exist.")}

  # Get sheet names
  sheet_names <- readxl::excel_sheets(path)

  # Map xlsx_import
  sheets <- sheet_names |>
    purrr::map(~ import_xlsx(path, sheet = .x, uuid = uuid, ...))

  # Name sheets
  sheets <- purrr::set_names(sheets, sheet_names)

  return(sheets)
}



#' @title Remove cols from tibble
#'
#' @param .tbl A tibble
#' @param cols A vector of column names (quoted)
#'
#' @return A tibble with columns removed
#'
#' @export
rm_cols <- function(.tbl, cols) {

  if_not_in_stop(.tbl, cols, ".tbl", "cols")

  .tbl |>
    dplyr::select(- {{ cols }})
}



#' @title Replace values of some tibble's columns to one value
#'
#' @param .tbl A tibble
#' @param values A vector of values
#' @param to_value A value
#' @param cols A vector of column names (quoted)
#'
#' @return A tibble with recoded values to one value
#'
#' @details If the column type is a character and the replacement a numeric, then the numeric is coerced to a character. If the column type is a numeric and the replacement is a character, then the column is coerced to character. NAs will remains NAs of the right type.
#'
#' @export
rec_values <- function(.tbl, values, to_value, cols){

  if_not_in_stop(.tbl, cols, ".tbl", "cols")

  .tbl |>
    dplyr::mutate(dplyr::across(.cols = {{ cols }}, ~ replace(., . %in% values, to_value)))
}



#' @title Replace many types of NAs of a tibble to NA
#'
#' @param .tbl A tibble
#' @param cols A vector of column names (quoted)
#'
#' @return A tibble with one type of NAs
#'
#' @export
rec_na <- function(.tbl, cols){
  nas <- c(NULL, "NULL", "N/A", "n/a", 999, 998, 888, " ", Inf, -Inf,
           9999, "(vide)", "(empty)", "d/m", "", "NA", "na", "", " ")

  .tbl |>
    rec_values(nas, NA, {{ cols }})
}



#' @title Date to day and month ("fr" locale)
#'
#' @param .vec A vector of type POSIXct
#' @param locale The locale to pass to base `format`, default is "fr"
#'
#' @return The same vector with only the day and month.
#'
#' @export
date_to_day_month <- function(.vec, locale = "fr"){
  .vec |>
    format(format = "%d %b", locale = locale)
}



#' @title Vector encoding (from Latin-1 to UTF-8)
#'
#' @param .vec A character vector (Latin-1 as default)
#' @param from The string encoding from. Default is  "latin1"
#' @param to The string encoding to. Default is " "UTF-8"
#'
#' @return The same vector but UTF-8
#'
#' @export
encode <- function(.vec, from = "latin1", to = "UTF-8") {
  Encoding(.vec) <- from
  iconv(.vec, from, to, sub = "")
}



#' @title Rename columns
#'
#' @param .tbl A tible
#' @param old_cols A vector of old quoted variable names
#' @param new_cols A vector of new quoted variable names
#'
#' @return Updated tibble with new names
#'
#' @export
rename_cols <- function(.tbl, old_cols, new_cols){

  if_not_in_stop(.tbl, old_cols, ".tbl", "old_cols")

  if (length(old_cols) != length(new_cols)) {
    abort_bad_argument("new_cols", "be the same length as `old_cols`")
  }

  up_tbl <- .tbl |>
    dplyr::rename_with(~ new_cols, .cols = old_cols)

  return(up_tbl)
}



#' @title Sublist of start pattern of list names
#'
#' @param list A list
#' @param pattern_start A pattern
#'
#' @return A sublist
#'
#' @export
sublist_start <- function(list, pattern_start){
  list[startsWith(names(list), pattern_start)]
}



#' @title Get colnames that start with pattern
#'
#' @param .tbl  A tibble
#' @param pattern_start A pattern
#'
#' @return A vector of column names
#'
#' @export
tbl_col_start <- function(.tbl, pattern_start){
  cols <- colnames(.tbl)[startsWith(colnames(.tbl), pattern_start)]

  if (rlang::is_empty(cols)) rlang::warn("The resulting vector is empty.")

  return(cols)
}



#' @title Get colnames that end with pattern
#'
#' @param .tbl  A tibble
#' @param pattern_end A pattern
#'
#' @return A vector of column names
#'
#' @export
tbl_col_end <- function(.tbl, pattern_end){
  cols <- colnames(.tbl)[endsWith(colnames(.tbl), pattern_end)]

  if (rlang::is_empty(cols)) rlang::warn("The resulting vector is empty.")

  return(cols)
}



#' @title Get colnames that start and end with patterns
#'
#' @param .tbl  A tibble
#' @param pattern_start A pattern
#' @param pattern_end A pattern
#'
#' @return A vector of column names
#'
#' @export
tbl_col_start_end <- function(.tbl, pattern_start, pattern_end){
  cols <- colnames(.tbl)[startsWith(colnames(.tbl), pattern_start) & endsWith(colnames(.tbl), pattern_end)]

  if (rlang::is_empty(cols)) rlang::warn("The resulting vector is empty.")

  return(cols)
}



#' @title Left join a few tibble from a list
#'
#' @param list A list of tibbles
#' @param ... Grouping columns
#'
#' @return A left-joined tibble
#'
#' @export
left_joints <- function(list, ...) {

  quoted_cols <- purrr::map_chr(rlang::enquos(...), rlang::as_name)

  purrr::imap(list, ~ if_not_in_stop(.x, quoted_cols, .y, arg = "..."))

  joined <- list |>
    purrr::reduce(
      dplyr::left_join,
      by = quoted_cols
      )

  return(joined)
}



#' @title Remove cols from a tibble that exists in another tibble
#'
#' @param tibble_a A tibble to remove columns from
#' @param tibble_b A tibble to extract columns names from
#' @param cols Columns to keep in tibble.
#'
#' @return A tibble with some columns removed
#'
#' @export
diff_tibbles <- function(tibble_a, tibble_b, cols){

  quoted_cols <- purrr::map_chr(rlang::enquos(cols), rlang::as_name)

  if_not_in_stop(tibble_b, quoted_cols, "tibble_b", arg = "cols")

  cols_to_remove <- tibble_b |>
    dplyr::select(- dplyr::all_of(quoted_cols)) |>
    colnames()

  removed <- tibble_a |>
    dplyr::select(- dplyr::any_of(cols_to_remove))

  return(removed)
}



#' @title Get all select multiple from survey sheet
#'
#' @param survey The survey sheet from Kobo (with column "type" split)
#'
#' @details survey should have a split type column with types of variables such as "select_one", "select_multiple", etc.
#'
#' @return A character vector of select_one questions
#'
#' @export
get_one <- function(survey){
#
#   if (typeof(rlang::enquo(type)) == "character"){
#     type <- rlang::sym(type)
#   }

  select_one <- survey |>
    dplyr::filter(.data$type == "select_one") |>
    dplyr::pull(.data$name)

  return(select_one)
}


#' @title Get all select multiple from survey sheet
#' @param survey The survey sheet from Kobo (with column "type" split)
#'
#' @details survey should have a split type column with types of variables such as "select_one", "select_multiple", etc.
#'
#' @return A character vector of select_multiple questions
#'
#' @export
get_multiple <- function(survey){

  select_multiple <- survey |>
    dplyr::filter(.data$type == "select_multiple") |>
    dplyr::pull(.data$name)

  return(select_multiple)
}




#' @title Get all select multiple and parent other questions
#'
#' @param .tbl A tibble of data
#' @param other  A character vector of the start of all other column names. E.g., other = "other_"
#'
#' @details This function has a tibble and the associated survey sheet as inputs, as well as the beginning of the "other" character string. It returns all columns that exist in the tibble and are either a multiple choice or a parent other question. This allows then to compute these columns with `impactR::count_occ` after parent question may have been recoded.
#'
#' @return A character vector of other questions
#'
#' @export
get_other_parent <- function(.tbl, other){

  other_parent <- tbl_col_start(.tbl, other) |>
    stringr::str_remove(other)

  if (length(other_parent) == 0) {
    rlang::warn("Did you provide the right `other` pattern?")
  }

  return(other_parent)
}



#' @title Get all select multiple and parent other questions
#'
#' @param .tbl A tibble of data
#' @param survey The survey sheet from Kobo (with column "type" split)
#' @param other  A character vector of the start of all other column names. E.g., other = "other_"
#' @param type Unquoted column type in the survey sheet. Default to type.
#'
#' @details This function has a tibble and the associated survey sheet as inputs, as well as the beginning of the "other" character string. It returns all columns that exist in the tibble and are either a multiple choice or a parent other question. This allows then to compute these columns with `impactR::count_occ` after parent question may have been recoded.
#'
#' @return A character vector of select_multiple and other questions
#'
#' @export
get_multiple_and_other_parent <- function(.tbl, survey, other){

  other_parent <- get_other_parent(.tbl, other)

  select_multiple <- get_multiple(survey)

  union(other_parent, select_multiple)
}



#' @title Get choices from survey name (and paste them if you want!)
#'
#' @param survey A survey sheet from Kobo (already split with columns list_name and name present)
#' @param choices A choices sheet from Kobo (with column list_name and name)
#' @param col A quoted column name
#' @param conc Should choices be concatenated to column name? Default to true.
#'
#' @return A character vector of choices or pasted to `col` choices with sep "_"
#'
#' @export
get_choices <- function(survey, choices, col, conc = T){

  col_name <- rlang::as_name(rlang::enquo(col))

  if_vec_not_in_stop(survey$name, col_name, "survey$name", "col")

  to_return <- survey |>
    dplyr::filter(.data$name == col) |>
    dplyr::select(.data$list_name) |>
    dplyr::left_join(choices, by = "list_name") |>
    dplyr::pull(.data$name)

  if (rlang::is_true(conc)) {
    to_return <- stringr::str_c(col, to_return, sep = "_")
  }

  return(to_return)
}



#' @title Split survey type and list name
#'
#' @param survey A survey sheet from Kobo
#' @param col_to_split Usually `type`
#' @param into Vector of columns names to split to. Default to c("type", "list_name" )
#' @param sep The separator. Default to " ".
#' @param fill How to fill. Default to NA on the right.
#' @param ... Params to pass to `tidyr::separate`
#'
#' @return  A survey tibble, split
#'
#' @export
split_survey <- function (survey, col_to_split, into = c("type", "list_name"),
                          sep = " ", fill = "right", ...)
{
  col_to_split_name <- rlang::as_name(rlang::enquo(col_to_split))
  if_not_in_stop(survey, col_to_split_name, "survey", "col_to_split")
  survey <- tidyr::separate(survey, {{ col_to_split }}, into = into,
                            sep = sep, fill = fill, ...)
  return(survey)
}




#' @title Named group split
#'
#' @param .tbl A tibble of data
#' @param group Column to split group by
#'
#' @return  A split and named list of tibbles
#'
#' @export
named_group_split <- function (.tbl, group){

  group_name <- rlang::as_name(rlang::enquo(group))
  if_not_in_stop(.tbl, group_name, ".tbl", "group")

  names <- .tbl |>
    dplyr::group_by({{ group }}) |>
    dplyr::group_keys() |>
    dplyr::pull({{ group }})

  split <- .tbl |>
    dplyr::group_split({{ group }}) |>
    purrr::set_names(names)

  return(split)
}


