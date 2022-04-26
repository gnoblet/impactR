

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
    msg <- glue::glue("The following column/s is/are missing in `{df}`:")
  }
  else {
    msg <- glue::glue("The following column/s from `{arg}` is/are missing in `{df}`:")
  }
  if (!all(cols %in% colnames(.tbl))) {
    rlang::abort(
      c("Missing columns",
        "*" =
        paste(
          msg,
          paste(
            subvec_not_in(cols, colnames(.tbl)),
            collapse = ", ")
        )
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
    msg <- glue::glue("The following element/s is/are missing in `{vec_name}`:")
  }
  else {
    msg <- glue::glue("The following element/s from `{arg}` is/are missing in `{vec_name}`:")
  }
  if (!all(cols %in% vec)) {
    rlang::abort(
      c("Missing elements",
        "*" =
        paste(
          msg,
          paste(
            subvec_not_in(cols, vec),
            collapse = ", ")
        )
      )
    )
  }
}