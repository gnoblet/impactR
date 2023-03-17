#' @title Abord bad argument
#'
#' @param arg1 An argument
#' @param must What arg must be
#' @param not Optional. What arg must not be.
#' @param arg2 Another argument
#' @param same The same type as arg1
#'
#' @return A stop statement
abort_bad_argument <- function(arg1, must, not = NULL, arg2 = NULL, same = NULL) {
  msg <- glue::glue("`{arg1}` must {must}")
  if (!is.null(not)) {
    not <- typeof(not)
    msg <- glue::glue("{msg}; not {not}.")
  }

  if (!is.null(same) & !is.null(arg2)) {
    same <- typeof(same)
    msg_i <- glue::glue("`{arg2}` is {same}.")
    msng <- c(msg, "i" = msg_i)
  }

  rlang::abort("error_bad_argument",
               message = msg,
               arg1 = arg1,
               must = must,
               not = not,
               arg2 = arg2,
               same = same
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

  missing_cols <- subvec_not_in(cols, colnames(.tbl))

  if (is.null(arg)) {
    if (length(missing_cols) >= 2) {
      msg <- glue::glue("The following columns are missing in `{df}`:")
    } else {
      msg <- glue::glue("The following column is missing in `{df}`:")
    }
  }
  else {
    if (length(missing_cols) >= 2) {
      msg <- glue::glue("The following columns from `{arg}` are missing in `{df}`:")
    } else {
      msg <- glue::glue("The following column from `{arg}` is missing in `{df}`:")
    }
  }
  if (length(missing_cols) >= 1) {
    rlang::abort(
      c("Missing columns",
        "*" =
        paste(
          msg,
          paste(
            missing_cols,
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


#' @title Stop statement values are not in set
#'
#' @param df A data frame
#' @param cols A column
#' @param set A vector of values
#'
#' @return A stop statement
are_values_in_set <- function(df, col, set){



  #------ Check for missing columns
  col_name <- rlang::as_name(rlang::ensym(col))
  if_not_in_stop(df, col_name, "df")

  #------ Values not in set

  # Remove NA
  #values <- df[[col_name]][!is.na(df[[col_name]])]

  # Values not in set
  in_set <- !(df[[col_name]] %in% set)

  # Count if 
  count_if <- sum(in_set, na.rm = TRUE) >= 1

  if (count_if) {
    rlang::abort(c(
      glue::glue("All columns must be in the following set: ", glue::glue_collapse(set, sep = ", ")),
      "i" = glue::glue(
        "Column {col_name} have values out of the set. Please check.")
      )
    )
  }

  return(TRUE)
}
