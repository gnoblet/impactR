# Hello, utils!
#
# Here are some utils for working with vectors
# - subvec in or not in a set
# - recode date to day and month
# - change encoding
# - get colnames with patterns



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




#' @title Date to day and month ("fr" locale)
#'
#' @param .vec A vector of type POSIXct"
#'
#' @return The same vector with only the day and month in the env locale.
#'
#' @export
date_to_day_month <- function(.vec){
  .vec |>
    format(format = "%d %b")
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
