# To-do:.
# - make_log_from_check_list: make it use pmap instead of exec


# Since where is not exported from tidyselect's NAMESPACE
utils::globalVariables("where")


#' @title Pull uuid from a logical test
#'
#' @param .tbl A tibble
#' @param logical_test A logical test as a character expression
#' @param id_col Survey id, usually uuid
#'
#' @return A vector of ids
#'
#' @export
pull_uuid <- function(.tbl, logical_test = "uuid", id_col){

  #-------- Checks

  # Check for id_col in .tbl
  id_col_name <- rlang::enquo(id_col) |> rlang::as_name()
  if_not_in_stop(.tbl, id_col_name, ".tbl", "id_col")
  if(!is.data.frame(.tbl)) abort_bad_argument(".tbl", "be a data.frame", not = .tbl)

  if(logical_test == "uuid"){
    .tbl |> dplyr::pull({{ id_col }})
  } else {
    .tbl |>
      dplyr::filter(!!rlang::parse_expr(logical_test)) |>
      dplyr::select({{ id_col }})
  }
}



#' @title Make log
#'
#' @param .tbl A tibble of data
#' @param survey The survey sheet from the Kobo tool
#' @param id_col Survey id, usually uuid
#' @param id_check A string to identify the check
#' @param question_name A column of .tbl
#' @param why A long character string
#' @param logical_test A character string made of a logical expression
#' @param new_value Should we suggest a new value?
#' @param action A character with "check" as default
#' @param type The type of the question "double" or "character"
#' @param cols_to_keep Columns to keep in the log
#'
#' @return A tibble with a log for one question and one logical test
#'
#' @family functions to make logs
#' @seealso [make_log_from_check_list()] for a log based on many logical tests
#' @seealso [make_log_outlier()] for a log based on outliers
#' @seealso [make_log_other()] for a log of others
#' @seealso [make_all_logs()] for all logs: outliers, others, logical tests
#'
#' @importFrom rlang .data
#'
#' @export
make_log <- function(
  .tbl,
  survey,
  id_col,
  id_check = "empty",
  question_name,
  why = "empty",
  logical_test,
  new_value,
  action,
  type,
  cols_to_keep
){

  name <- label <- NULL


  #-------- Checks

  # Check for id_col in .tbl
  id_col_name <- rlang::enquo(id_col) |> rlang::as_name()
  if_not_in_stop(.tbl, id_col_name, ".tbl", "id_col")

  # Check for ... in .tbl
  if_not_in_stop(.tbl, cols_to_keep, ".tbl", arg = "...")


  # To do:
  # - add an abort check if type != question_name type or warninf if coerced introduction
  # - add an abort check if logical_test cannot be evaluated (is this possible and how?)
  # - add a warning check if new_value's type != question name's


  #-------- Make log tibble

  # Get uuid using the logical test
  pulled_uuid <- .tbl |>
    pull_uuid(logical_test, {{ id_col }})

  # Join and get the subset of .tbl
  new_tbl <- pulled_uuid  |>
    dplyr::left_join(.tbl, by = id_col_name)

  # Get label question
  question_label <- survey |>
    dplyr::filter(.data$name == question_name) |>
    dplyr::pull(.data$label)

  question_label <- ifelse(rlang::is_empty(question_label) | rlang::is_na(question_label), "", question_label)

  # Create the new_log entry
  new_log <- tibble::tibble(
    id_check = id_check,
    new_tbl |>  dplyr::select({{ cols_to_keep }}, {{ id_col }}),
    question_name = question_name,
    question_label = question_label,
    why = why,
    feedback = "Verified in check list",
    action = ifelse(is.na(action), "check", action),
    old_value = new_tbl |>
      dplyr::pull(!!question_name),
    new_value = new_value,
    type = type,
    other_parent_question = "",
    other_old_value = "",
    other_new_value = "",
    checkdate = paste0("Checked on ", Sys.time())
  )

  return(new_log)

}


#' @title Apply `make_log` to a tibble of check_list (one logical test per row)
#'
#' @param .tbl A tibble of data
#' @param survey The survey sheet of the Kobo tool
#' @param check_list A tibble of logical test to check for
#' @param id_col Survey id, usually uuid
#' @param ... Columns to keep in the log, e.g. today, i_enum_id
#'
#' @family functions to make logs
#' @seealso [make_log] for a log based on a logical test
#' @seealso [make_log_outlier()] for a log based on outliers
#' @seealso [make_log_other()] for a log of others
#' @seealso [make_all_logs()] for all logs: outliers, others, logical tests
#'
#' @importFrom rlang .data
#'
#' @export
make_log_from_check_list <- function(.tbl, survey, check_list, id_col, ...) {

  check_check_list(check_list, .tbl)


  # Check for id_col in .tbl
  id_col_name <- rlang::enquo(id_col) |> rlang::as_name()
  if_not_in_stop(.tbl, id_col_name, ".tbl", "id_col")

  # Check for ... in .tbl
  cols_to_keep <- purrr::map_chr(rlang::enquos(...), rlang::as_name)
  if_not_in_stop(.tbl, cols_to_keep, ".tbl", arg = "...")

  new_log <- check_list |>
    dplyr::select(.data$id_check, .data$question_name, .data$why, .data$logical_test, .data$new_value, .data$action, .data$type) |>
    dplyr::group_split(.data$id_check) |>
    purrr::map(~ as.list(.x))|>
    purrr::map(~ purrr::exec(make_log, !!!(c(.tbl = list(.tbl),
                                             survey = list(survey),
                                             id_col = list(id_col_name),
                                             .x,
                                             cols_to_keep = list(cols_to_keep)
                                             )
                                           )
                             )
               ) |>
    purrr::map(~ .x |> dplyr::mutate(dplyr::across(c(.data$question_label, .data$old_value), as.character))) |>
    dplyr::bind_rows()

  return(new_log)
}




#' @title Get all other values
#'
#' @param .tbl A tibble of data
#' @param other  A character vector of the start of all other column names. E.g., other = "other_"
#' @param id_col Survey id, usually uuid
#'
#' @return A tibble with all "other" answers
#'
#' @export
other_cols <- function(.tbl, other, id_col) {

  #-------- Checks
  id_col_name <- rlang::as_name(rlang::enquo(id_col))
  if_not_in_stop(.tbl, id_col_name, ".tbl", "id_col")

  if (length(tbl_col_start(.tbl, other)) == 0) {
    rlang::abort(paste("at least one column in `.tbl` must start with '", other, "'"))
  }

  #-------- Get others
  others <- .tbl |>
    dplyr::select({{ id_col }}, dplyr::starts_with(other)) |>
    # Faire pivoter la table et virer les NA
    tidyr::pivot_longer(tidyr::starts_with(other),
                        names_to = "question_name",
                        values_to = "old_value",
                        values_drop_na = T) |>
    dplyr::mutate(other_parent_question = stringr::str_remove(.data$question_name, other))


  return(others)
}



#' @title Get all other values and parent values
#'
#' @param .tbl A tibble of data
#' @param other_cols A tibble produced by `other_cols`
#' @param other  A character vector of the start of all other column names. E.g., other = "other_"
#' @param id_col Survey id, usuallly uuid
#'
#' @return A tibble with all "other" values and their parent values
#'
#' @importFrom rlang .data
#'
#' @export
other_parent_cols <- function(.tbl, other_cols, other, id_col) {

  #-------- Checks

  id_col_name <- rlang::as_name(rlang::enquo(id_col))
  if_not_in_stop(.tbl, id_col_name, ".tbl", "id_col")

  if (length(tbl_col_start(.tbl, other)) == 0) {
    rlang::abort(paste("at least one column in `.tbl` must start with '", other, "'"))
  }

  #-------- Prepare other tibble
  other_parent <- other_cols |>
    tibble::add_column(other_old_value = "")

  if(nrow(other_parent) == 0){
    other_parent <- tibble::tibble()
  } else{
    other_parent <- other_parent |>
      dplyr::group_split({{ id_col }}) |>
      purrr::map(~ .x |>  tidyr::pivot_wider({{ id_col }},
                                             names_from = .data$other_parent_question,
                                             values_from = .data$other_old_value)) |>
      purrr::map(~ .tbl |>
                   dplyr::select(dplyr::any_of(names(.x))) |>
                   dplyr::semi_join(.x, by = id_col_name)) |>
      dplyr::bind_rows() |>
      tidyr::pivot_longer(-{{ id_col }},
                          names_to = "other_parent_question",
                          values_to = "other_old_value",
                          values_drop_na = T)
  }
  return(other_parent)
}



#' @title Bind other and parent values
#'
#' @param .tbl A tibble of data
#' @param other  A character vector of the start of all other column names. E.g., other = "other_"
#' @param id_col Survey id, usually uuid (quoted or unquoted)
#'
#' @return A tibble with other and parent other questions and values
#'
#' @details Made to ease the use of other_cols and other_parent_cols
#'
#' @importFrom rlang .data
#'
#' @export
bind_others <- function(.tbl, other, id_col) {

  #-------- Checks

  id_col_name <- rlang::as_name(rlang::enquo(id_col))
  if_not_in_stop(.tbl, id_col_name, ".tbl", "id_col")

  if (length(tbl_col_start(.tbl, other)) == 0) {
    rlang::abort(paste("at least one column in `.tbl` must start with '", other, "'"))
  }

  #-------- Bind
  other_df <- other_cols(.tbl, other, {{ id_col }})
  other_parent_df <- other_parent_cols(.tbl, other_df, other, {{ id_col }})

  bind <- dplyr::left_join(other_df, other_parent_df, by = c(id_col_name, "other_parent_question"))

  return(bind)
}



#' @title Make "other questions" log
#'
#' @param .tbl A tibble of data
#' @param survey The survey sheet from Kobo
#' @param other A character vector of the start of all other column names. E.g., other = "other_".
#' @param id_col Survey id, usually uuid
#' @param ... Columns to keep in the log, e.g. uuid, enum_id
#'
#' @details It assumes that a parent question and its children "other" are coded as follows
#'     within the kobo tool : e.g., parent_question, other_parent_question.
#'
#' @return A log with all "other_cols".
#'
#' @family functions to make logs
#' @seealso [make_log] for a log based on a logical test
#' @seealso [make_log_from_check_list()] for a log based on many logical tests
#' @seealso [make_log_outlier()] for a log based on outliers
#' @seealso [make_all_logs()] for all logs: outliers, others, logical tests
#'
#' @importFrom rlang .data
#'
#' @export
make_log_other <- function(
  .tbl,
  survey,
  other,
  id_col,
  ...
) {

  #-------- Checks

  # Check for id_col in .tbl
  id_col_name <- rlang::enquo(id_col) |> rlang::as_name()
  if_not_in_stop(.tbl, id_col_name, ".tbl", "id_col")

  # Check for ... in .tbl
  cols_to_keep <- purrr::map_chr(rlang::enquos(...), rlang::as_name)
  if_not_in_stop(.tbl, cols_to_keep, ".tbl", arg = "...")

  #-------- Construct log tibble

  # Prepare survey
  survey <- survey |>
    rename_cols("label", "question_label")

  # Prepare data
  .old_tbl <- .tbl |>
    dplyr::select({{ id_col }}, ...)

  # Get sub data frame will all other values and their question name
  # Join and get perfect wonderful butterflies
  new_tbl <- .tbl |>
    bind_others(other, {{ id_col }}) |>
    dplyr::left_join(survey, by = c("other_parent_question" = "name")) |>
    dplyr::left_join(.old_tbl, by = id_col_name)

  # Create log
  new_log <- tibble::tibble(
    id_check = other,
    new_tbl |>
      dplyr::select(
        ...,
        {{ id_col }},
        .data$question_name,
        .data$question_label),
    why = "Other answer",
    feedback = "Fill in",
    action = "check",
    new_tbl |>
      dplyr::select(.data$old_value),
    new_value = "Fill in if necessary",
    type = "character",
    new_tbl |>
      dplyr::select(.data$other_parent_question, .data$other_old_value),
    other_new_value = "",
    checkdate = paste0("Checked on ", Sys.time())
  )
  return(new_log)
}



#' @title Get numeric colum names
#'
#' @param .tbl A tibble
#' @param survey Default to NULL. The survey sheet from Kobo.
#'
#' @return A characater vector of numeric columns names
#'
#' @details If the survey sheet is provided, it returns numeric columns from `.tbl` that are coded as decimal or numeric in the `type` column of `survey`. Otherwise, it returns all numeric columns from `.tbl`.
#'
#' @importFrom rlang .data
#'
#' @export
numeric_cols <- function(.tbl, survey = NULL) {

  # There is a warning since where is not exported from the tidyselect namespace
  # Who knows why
  # Need to be tackled
  # utils::globalVariables("where")

  # Check type of .tbl
  if (!is.data.frame(.tbl)) {
    abort_bad_argument(".tbl", "must be a data.frame", not = .tbl)
  }

  if (!is.null(survey)){
    # Check type of survey
    if (!is.data.frame(survey)) {
      abort_bad_argument("survey", "must be a data.frame", not = survey)
    }

    # Check if column type is in survey
    if_not_in_stop(survey, c("name", "type"), "survey")

    num_cols_survey <- survey |>
      dplyr::filter(.data$type %in% c("integer", "decimal", "calculate")) |>
      dplyr::pull(.data$name)

    # Check if there are numeric or decimal types in survey
    if (length(num_cols_survey) == 0) rlang::abort("no 'calculate', 'integer' or 'decimal' type in `survey`")

    # Check if these columns from survey are in .tbl
    if_not_in_stop(.tbl, num_cols_survey, ".tbl", "survey")

    .tbl <- .tbl |>
      dplyr::select(dplyr::all_of(num_cols_survey))

  }
  num_cols <- .tbl |>
    dplyr::select(where(is.numeric)) |>
    colnames()

  return(num_cols)
}



#' @title Get outliers outside standard deviations from the mean
#'
#' @param .tbl A tibble
#' @param col A numeric columns
#' @param times How many standard deviations from the mean? Default to 3
#' @param id_col Usually uuid
#'
#' @return A tibble of outliers
#'
#' @export
outliers_sd <- function(.tbl, col, times = 3, id_col) {

  #------- Checks

  # Check for id_col in .tbl
  id_col_name <- rlang::enquo(id_col) |> rlang::as_name()
  if_not_in_stop(.tbl, id_col_name, ".tbl", "id_col")

  # Check for col in .tbl
  col_name <- rlang::enquo(col) |> rlang::as_name()
  if_not_in_stop(.tbl, col_name, ".tbl", "col")

  #-------- Get outliers

  outliers <- .tbl |>
    dplyr::select({{ id_col }}, {{ col }}) |>
    dplyr::filter(abs(!!rlang::sym(col_name) - mean(!!rlang::sym(col_name))) > times * stats::sd(!!rlang::sym(col_name))) |>
    tidyr::pivot_longer({{ col }},
                        names_to = "question_name",
                        values_to = "old_value")

  return(outliers)
}



#' @title Get outliers using interquartile range (better for skewed-distribution)
#'
#' @param .tbl A tibble
#' @param col A numeric columns
#' @param times How much to deviate from IQR? Default to 1.5
#' @param id_col Usually uuid
#'
#' @return A tibble of outliers
#'
#' @export
outliers_iqr <- function(.tbl, col, times = 1.5, id_col) {

  #------- Checks

  # Check for id_col in .tbl
  id_col_name <- rlang::enquo(id_col) |> rlang::as_name()
  if_not_in_stop(.tbl, id_col_name, ".tbl", "id_col")

  # Check for col in .tbl
  col_name <- rlang::enquo(col) |> rlang::as_name()
  if_not_in_stop(.tbl, col_name, ".tbl", "col")

  #-------- Get outliers

  # Get col
  pulled_col <- .tbl |> dplyr::pull({{ col }})

  # Get IQR
  iqr <- stats::IQR(pulled_col, na.rm = T)

  # Get quantiles
  quantiles <- stats::quantile(pulled_col, na.rm = T)

  # Filter with the times * IQR rule
  outliers <- .tbl |>
    dplyr::select({{ id_col }}, {{ col }}) |>
    dplyr::filter(!!rlang::sym(col_name) < quantiles[2] - times * iqr | !!rlang::sym(col_name) > quantiles[4] + times * iqr)  |>
    tidyr::pivot_longer({{ col }},
                        names_to = "question_name",
                        values_to = "old_value")

  return(outliers)
}


#' @title Make outlier log
#'
#'
#' @param .tbl A tibble of data
#' @param survey The survey sheet from Kobo
#' @param id_col Survey id, usually uuid
#' @param ... Columns to keep in the log, e.g, today, enum_id
#'
#' @return A log with outliers
#'
#' @details It uses both interquartile range (1.5 rule) and standard deviation from mean (3 times rule) to look for outliers
#'
#' @family functions to make logs
#' @seealso [make_log] for a log based on a logical test
#' @seealso [make_log_from_check_list()] for a log based on many logical tests
#' @seealso [make_log_other()] for a log of others
#' @seealso [make_all_logs()] for all logs: outliers, others, logical tests
#'
#' @importFrom rlang .data
#'
#' @export
make_log_outlier <- function(.tbl, survey, id_col, ...) {

  #-------- Checks

  # Check for id_col in .tbl
  id_col_name <- rlang::enquo(id_col) |> rlang::as_name()
  if_not_in_stop(.tbl, id_col_name, ".tbl", "id_col")

  # Check for ... in .tbl
  cols_to_keep <- purrr::map_chr(rlang::enquos(...), rlang::as_name)
  if_not_in_stop(.tbl, cols_to_keep, ".tbl", arg = "...")

  #-------- Get outliers and construc log tibble

  # Get all numeric cols
  num_cols <- numeric_cols(.tbl, survey)

  if (length(num_cols) == 0) rlang::abort("There is no numeric columns in .tbl")

  # Get IQR outliers
  iqr <- num_cols |> purrr::map(~ outliers_iqr(.tbl, {{ .x }}, id_col = {{ id_col }}))

  # Get standard deviation outliers
  sd <- num_cols |> purrr::map(~ outliers_sd(.tbl, {{ .x }}, id_col = {{ id_col }}))

  #-------- Construct log tibble

  new_tbl <- dplyr::bind_rows(iqr, sd) |>
    dplyr::distinct() |>
    dplyr::left_join(survey |> dplyr::select(.data$name, .data$label), by = c("question_name" = "name")) |>
    dplyr::rename('question_label' := .data$label) |>
    dplyr::left_join(.tbl |> dplyr::select({{ id_col }},...), by = id_col_name)

  new_log <- tibble::tibble(
    id_check = "outlier",
    new_tbl |>
      dplyr::select(..., {{ id_col }}, .data$question_name),
    new_tbl |>
      dplyr::select(.data$question_label),
    why = "Outlier",
    feedback = "Fill in",
    action = "check",
    new_tbl |>
      dplyr::select(.data$old_value),
    new_value = "Fill in if necessary",
    type = "double",
    other_parent_question = "",
    other_new_value = "",
    checkdate = paste0("Checked on ", Sys.time())
  )

  return(new_log)
}




#' @title Make all logs
#'
#' @param .tbl A tibble of data
#' @param survey The survey sheet of the Kobo tool
#' @param check_list A tibble of logical test to check for
#' @param other A character vector of the start of all other column names. E.g., other = "other_"
#' @param id_col Survey id, usually "uuid"
#' @param ... Columns to keep in the log, e.g. today, i_enum_id
#'
#' @return A full log as a tibble
#'
#' @family functions to make logs
#' @seealso [make_log] for a log based on a logical test
#' @seealso [make_log_from_check_list()] for a log based on many logical tests
#' @seealso [make_log_outlier()] for a log based on outliers
#' @seealso [make_log_other()] for a log of others
#'
#' @importFrom rlang .data
#'
#' @export
make_all_logs <- function(.tbl, survey, check_list, other, id_col, ...) {

  if (length(tbl_col_start(.tbl, other)) != 0) {
    l <- list(
      make_log_from_check_list(.tbl, survey, check_list, {{ id_col }}, ...),
      make_log_other(.tbl, survey, other,  {{ id_col }}, ...),
      make_log_outlier(.tbl, survey, {{ id_col }}, ...)
    )
  } else {
    l <- list(
      make_log_from_check_list(.tbl, survey, check_list, {{ id_col }}, ...),
      make_log_outlier(.tbl, survey, {{ id_col }}, ...)
    )

    rlang::warn(paste0("No column in `.tbl` start with", other, ". Skipping using make_log_other()"))
  }

  to_return <- l |>
    purrr::map(~ .x |> dplyr::mutate(dplyr::across(.fns = as.character))) |>
    dplyr::bind_rows() |>
    readr::type_convert()

  return(to_return)
}
