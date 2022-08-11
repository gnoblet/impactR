# Helo checks!

# Some check functions
# - to check cleaning log
# - to check the logical check list df



#' @title  Check cleaning log
#' @param log A cleaning log
#' @param .tbl Data to clean
#' @param id_col The column id, usually "uuid"
#' @param other  A character vector of the start of all other column names. E.g., other = "other_"
#'
#' @details Names should be the English version. If necessary, please use [impactR::log_names_fr_en] beforehand.
#'
#' @family functions to check logs and check lists
#'
#' @importFrom rlang .data
#'
#' @export
check_cleaning_log <- function(log, .tbl, id_col, other){
  en_names <- c("id_check", "question_name", "question_label", "why", "feedback", "action", "old_value", "new_value", "type", "other_parent_question", "other_old_value", "other_new_value", "checkdate" )

  id_col_name <- rlang::as_name(rlang::enquo(id_col))

  # Check if the column names are the right names
  are_cols_in <- !(en_names %in% colnames(log))

  if(sum(are_cols_in) >= 1) {
    stop("The following columns are missing and required: ", paste(en_names[are_cols_in], collapse = ", "), ".")
  }

  # Check if all id_check are there
  if(sum(is.na(log$id_check)) >= 1){
    stop("There are missing 'id_check' values.")
  }

  # Check if all types are there
  if(sum(is.na(log$type)) >= 1){
    stop("There are missing 'type' values.")
  }

  # Check if all question_names are there
  if(sum(is.na(log$question_name) >= 1)){
    stop("There are missing 'question_name' values.")
  }

  # Check if the types are one of the right ones
  types <- sum(stringr::str_count(log$type, pattern = "character|double"))

  if(types < nrow(log)){
    stop("There are wrong types in column 'type'. It should be either 'character' or 'double'.")
  }

  # Check if the actions are one of the right ones
  actions <- sum(stringr::str_count(log$action, pattern = "keep|modify|remove|check|duplicate"))

  if(actions < nrow(log)){
    stop("There are wrong actions in column 'action'. It should be either 'keep', 'modify', 'remove', 'duplicate' or 'check'.")
  }

  # Check if question_name that needs a modification belongs to the rawdata
  question_names <- log |>
    dplyr::filter(.data$action == "modify" & id_check == other) |>
    tidyr::drop_na(.data$question_name) |>
    dplyr::pull(.data$question_name)

  are_cols_in <- question_names[!(question_names %in% colnames(.tbl))]

  if(length(are_cols_in) >= 1){
    stop("The following 'question_name' values does not exist in the rawdata column names:\n", paste(are_cols_in, collapse = "\n "))
  }

  # Check for remaining bits from template in "feedback"
  remaining_bits <- log |>
    dplyr::filter(
      .data$action %in% c("modify", "check", "remove", "duplicate"),
      stringr::str_count(.data$feedback, "Verified in check list|Fill in") >= 1
    ) |>
    dplyr::mutate(rem = paste0({{ id_col }}, ": ", .data$question_name)) |>
    dplyr::pull(.data$rem)

  if (length(remaining_bits) >= 1) {

    warning("The following id_col and question_name have remaining bits from the template such as 'Fill in' in column 'feedback', please check:\n ", paste(remaining_bits, collapse = "\n "))
  }

  # Check for remaining bits from template in "new_value"
  remaining_bits <- log |>
    dplyr::filter(
      .data$action %in% c("modify", "check", "remove", "duplicate"),
      stringr::str_count(.data$new_value, "Fill in if necessary") >= 1
    ) |>
    dplyr::mutate(rem = paste0({{ id_col }}, ": ", .data$question_name)) |>
    dplyr::pull(.data$rem)

  if (length(remaining_bits) >= 1) {

    warning("The following id_col and question_name have remaining bits from the template such as 'Fill in' in column 'new_value', please check:\n ", paste(remaining_bits, collapse = "\n "))
  }

  # Check if 'other_new_value' contains no value anymore and 'other_old_value' contains two or more
  onv <- log |>
    dplyr::filter(
      .data$action %in% c("modify") &
        stringr::str_count(.data$other_old_value, " ") >= 1 &
        is.na(.data$other_new_value)
      ) |>
    dplyr::mutate(onv = paste0({{ id_col }}, ": ", .data$question_name)) |>
    dplyr::pull(.data$onv)

  if (length(onv) >= 1) {

    warning("The following id_col and question_name had two other values and no value anymore in 'other_new_value', please check:\n ", paste(remaining_bits, collapse = "\n "))
  }

  # Check if other parent question_name that needs a modification belongs to the rawdata
  other_parent_questions <- log |>
    dplyr::filter(.data$action == "modify" & id_check == other) |>
    tidyr::drop_na(.data$other_parent_question) |>
    dplyr::pull(.data$other_parent_question)

  are_cols_in <- other_parent_questions[!(other_parent_questions %in% colnames(.tbl))]

  if(length(are_cols_in) >= 1){
    stop("The following 'other_parent_question' values does not exist in the rawdata column names:\n", paste(are_cols_in, collapse = "\n "))
  }

  # Check if there are duplicates
  duplicates <- log |>
    dplyr::filter(.data$action == "modify") |>
    tidyr::drop_na(.data$question_name) |>
    dplyr::mutate(dup = paste0({{ id_col }}, ": ", .data$question_name)) |>
    dplyr::pull(.data$dup)

  duplicates <- duplicates[duplicated(duplicates)]


  if (length(duplicates) >= 1) {

    stop("The following id_col and question_name are duplicates and to be modified:\n ", paste(duplicates, collapse = "\n "))
  }

  # Check if there are other parent duplicates
  duplicates <- log |>
    dplyr::filter(.data$action == "modify") |>
    tidyr::drop_na(.data$other_parent_question) |>
    dplyr::mutate(dup = paste0({{ id_col }}, ": ", .data$other_parent_question)) |>
    dplyr::pull(.data$dup)

  duplicates <- duplicates[duplicated(duplicates)]


  if (length(duplicates) >= 1) {

    stop("The following id_col and other_parent_question are duplicates and to be modified\n: ", paste(duplicates, collapse = "\n "))
  }


  # Check if there are old and new values to be modified that are identical
  identical <- log |>
    dplyr::filter(.data$action == "modify",
                  .data$old_value == .data$new_value)


  if(nrow(identical) >= 1) {

    identical <- identical |>
      dplyr::mutate(dup = paste0({{ id_col }}, ": ", .data$question_name)) |>
      dplyr::pull(.data$dup)

    warning("For the following id_col and question_name, old and new values are identical:\n ", paste(identical, collapse = "\n "))
  }


  # Check if there are old and new values to be modified that are identical
  identical <- log |>
    dplyr::filter(.data$action == "modify",
                  .data$other_old_value == .data$other_new_value)


  if(nrow(identical) >= 1) {

    identical <- identical |>
      dplyr::mutate(dup = paste0({{ id_col }}, ": ", .data$other_parent_question)) |>
      dplyr::pull(.data$dup)

    warning("For the following id_col and other_parent_question, old and new other parent values are identical:\n ", paste(identical, collapse = "\n "))
  }


  return(NULL)
}



#' @title  Check cleaning log against data
#'
#' @param check_list A check list (a tibble of a particular format)
#' @param .tbl Data to clean
#'
#' @details Names should be the English version. Please use [impactR::log_names_fr_en] beforehand.
#'
#' @family functions to check logs and check lists
#'
#' @importFrom rlang .data
#'
#' @export
check_check_list <- function(check_list, .tbl){
  en_names <- c("id_check", "question_name", "why", "logical_test", "new_value", "action", "type")

  # Check if the column names are the right names
  if_not_in_stop(check_list, en_names, "check_list")

  # Check if all id_check are there
  if(rlang::is_na(check_list$id_check)) rlang::abort("There are missing `id_check` values")

  # Check if all question_names are there
  if(rlang::is_na(check_list$question_name)) rlang::abort("There are missing `question_name` values")

  # Check if types are one of the right ones
  types <- sum(stringr::str_count(check_list$type, pattern = "character|double"))
  if(types < nrow(check_list)){
    abort_bad_argument("type", "be either character or double")
  }

  # Check if the actions are one of the right ones
  actions <- sum(stringr::str_count(check_list$action, pattern = "keep|modify|remove|check|duplicate"))

  if(actions < nrow(check_list)){
    abort_bad_argument("action", "be either 'keep', 'modify', 'remove', 'check' or 'duplicate")
  }

  # Check if question_name that needs a modification belongs to the rawdata
  are_cols_in <- !(check_list |> dplyr::pull(.data$question_name) %in% colnames(.tbl))

  if_not_in_stop(.tbl ,
                 check_list |>
                   dplyr::pull(.data$question_name),
                 ".tbl",
                 "question_name")

  return(TRUE)
}
