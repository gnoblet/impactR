# Helo checks!

# Some check functions
# - to check cleaning log
# - to check the logical check list df



#' @title  Check cleaning log
#' @param log A cleaning log
#' @param .tbl Data to clean
#'
#' @details Names should be the English version. If necessary, please use [impactR::log_names_fr_en] beforehand.
#'
#' @family functions to check logs and check lists
#'
#' @importFrom rlang .data
#'
#' @export
check_cleaning_log <- function(log, .tbl){
  en_names <- c("id_check", "question_name", "question_label", "why", "feedback", "action", "old_value", "new_value", "type", "other_parent_question", "other_old_value", "other_new_value", "checkdate" )

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
  are_cols_in <- !(log |> dplyr::filter(.data$action == "modify") |> dplyr::pull(.data$question_name) %in% colnames(.tbl))

  if(sum(are_cols_in) >= 1){
    stop("The following 'question_name' values does not exist in the rawdata column names: ", paste(log$question_name[are_cols_in], collapse = ", "), ".")
  }

  # To be added :
  # - check types of the question name in the log against its type in the rawdata
  # - check that new value is not the same as the old value
  # - check that new value is not

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
