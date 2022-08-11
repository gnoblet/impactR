# Helo utils!

# All utils to work on dataframes/tibbles
# Recode
# Recode NAs
# Rename many columns
# Left join several dataframes
# Remove cols from df1 that exist ini df2
# Group split


#' @title Remove cols from tibble
#'
#' @param .tbl A tibble
#' @param ... Column names
#'
#' @return A tibble with columns removed
#'
#' @export
deselect <- function(.tbl, ...) {

  quoted_cols <- purrr::map_chr(rlang::enquos(...), rlang::as_name)
  if_not_in_stop(.tbl, quoted_cols, ".tbl", arg = "...")

  .tbl |>
    dplyr::select(- dplyr::all_of(quoted_cols))
}



#' @title Replace values of some tibble's columns to one value
#'
#' @param .tbl A tibble
#' @param values A vector of values
#' @param to_value A value
#' @param ... Column names
#'
#' @return A tibble with recoded values to one value
#'
#' @details If the column type is a character and the replacement a numeric, then the numeric is coerced to a character. If the column type is a numeric and the replacement is a character, then the column is coerced to character. NAs will remains NAs of the right type.
#'
#' @export
recode_values <- function(.tbl, values, to_value, ...){

  quoted_cols <- purrr::map_chr(rlang::enquos(...), rlang::as_name)
  if_not_in_stop(.tbl, quoted_cols, ".tbl", arg = "...")

  .tbl |>
    dplyr::mutate(dplyr::across(.cols = dplyr::all_of(quoted_cols), ~ replace(., . %in% values, to_value)))
}



#' @title Replace many types of NAs of a tibble to NA
#'
#' @param .tbl A tibble
#' @param ... Column names
#'
#' @return A tibble with one type of NAs
#'
#' @export
recode_na <- function(.tbl, ...){
  nas <- c(NULL, "NULL", "N/A", "n/a", 999, 998, 888, " ", Inf, -Inf,
           9999, "(vide)", "(empty)", "d/m", "", "NA", "na", "", " ",
           NaN, "NaN", "Na", -999, -9999, -998, -888)

  .tbl |>
    recode_values(nas, NA, ...)
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
    dplyr::rename_with(~ new_cols, .cols = dplyr::all_of(old_cols))

  return(up_tbl)
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
#' @param ... Columns to keep in tibble.
#'
#' @return A tibble with some columns removed
#'
#' @export
diff_tibbles <- function(tibble_a, tibble_b, ...){

  #-------- Checks

  quoted_cols <- purrr::map_chr(rlang::enquos(...), rlang::as_name)
  if_not_in_stop(tibble_b, quoted_cols, "tibble_b", arg = "cols")

  #-------- Make the diff

  cols_to_remove <- tibble_b |>
    dplyr::select(- dplyr::all_of(quoted_cols)) |>
    colnames()

  removed <- tibble_a |>
    dplyr::select(- dplyr::any_of(cols_to_remove))

  return(removed)
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




#' @title Calculate the duration of a survey
#'
#' @param .tbl A tibble
#' @param start Start column name
#' @param end End column name
#' @param new_colname The new column name of the time duration
#'
#' @details Note: it is necessary to have 'start' and 'end' columns
#'
#' @return A tibble with three new colums, including the duration of survey in minutes
#'
#' @export
survey_duration <- function(.tbl, start, end, new_colname = "survey_duration"){

  duration <- .tbl  |>
    dplyr::mutate(
      start = lubridate::ymd_hms({{ start }}, truncated = 1),
      end = lubridate::ymd_hms({{ end }}, truncated = 1),
      "{new_colname}" := round(difftime({{ end }},  start, units = "mins")) |>  as.double()
    )

  return(duration)
}


#' @title Check in-between surveys time
#'
#' @param .tbl A tibble
#' @param start Start column (unquoted)
#' @param end End column (unquoted)
#' @param ... The columns to group by, usually a locality or an enumerator id
#' @param new_colname The new column name of the time difference
#' @details Note: it is necessary to have 'start' and 'end' columns with no NA
#'
#' @return A tibble with ... removed
#'
#' @export
survey_difftime <- function(.tbl, start, end, ..., new_colname = "survey_difftime"){

  #-------- Checks

  # Check for start in .tbl
  start_name <- rlang::enquo(start) |> rlang::as_name()
  if_not_in_stop(.tbl, start_name, ".tbl", "start")

  # Check for start in .tbl
  end_name <- rlang::enquo(end) |> rlang::as_name()
  if_not_in_stop(.tbl, end_name, ".tbl", "end")

  # Check for ... in .tbl
  cols_to_keep <- purrr::map_chr(rlang::enquos(...), rlang::as_name)
  if_not_in_stop(.tbl, cols_to_keep, ".tbl", arg = "...")

  #-------- Add difftime column

  diff_time <- .tbl |>
    dplyr::arrange(..., {{ start }}) |>
    dplyr::group_by(...) |>
    dplyr::mutate("{new_colname}" := difftime({{ start }}, dplyr::lag({{ end }}), units = "mins") |> round() |> as.double()) |>
    dplyr::ungroup()

    return(diff_time)
}

#' Mutate with replacement if NULL or NA values
#'
#' @param .tbl A data.frame.
#' @param col A column from .tbl.
#' @param replacement A replacement of the same type as col.
#'
#' @return A mutated tibble.
#' @export
mutate_if_nulla <- function(.tbl, col, replacement){

  #---- Checks

  # is .tbl a data.frame or coercible to one?
  if (!is.data.frame(.tbl)) rlang::abort(".tbl must have 'data.frame' among its classes.")

  # col in .tbl
  col_name <- rlang::as_name(rlang::enquo(col))
  if_not_in_stop(.tbl, col_name, ".tbl", "col")

  # replacement type string
  if (typeof(.tbl[[col_name]]) != typeof(replacement)) {abort_bad_argument("replacement", "be the same type as `col`", not = replacement, arg2 = "col", .tbl[[col_name]])}

  mutated <- .tbl |>
    dplyr::mutate("{{ col }}" := ifelse(is.na({{ col }}) | is.null({{ col }}), replacement, {{ col }}))

  return(mutated)
}

#' Count the number of occurrences of a string over a data frame
#'
#' @param df A dataframe
#' @param pattern A pattern to pass to `stringr::str_count()`. Default to "".
#' @param new_colname The newly-mutated colname. Default to "count".
#'
#' @return A mutated dataframe
#'
#' @export
string_count <- function(df, pattern = "", new_colname = "count"){
  df <- df |>
    dplyr::mutate("{new_colname}" := purrr::pmap_int(
      df |> purrr::keep(\(x) is.character(x)),
      ~ stringr::str_count(c(...), pattern) |> sum(na.rm = T)))

  return(df)
}
