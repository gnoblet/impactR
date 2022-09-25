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

  # Check if id_col is a column of log
  if(length(subvec_not_in(id_col_name, colnames(log))) > 0) {
    stop("The 'id_col' column is missing in log.")
  }

  # Check if id_col is a column of log and .tbl
  if_not_in_stop(.tbl, id_col_name, ".tbl", "id_col")
  if_not_in_stop(log, id_col_name, ".tbl", "id_col")

  # Check if id_col is present in data

  id_col_nin_data <- subvec_not_in(dplyr::pull(log, {{ id_col }}), dplyr::pull(.tbl, {{ id_col }}))
  if(length(id_col_nin_data) > 0){
    stop("The following 'id_col' values does not exist in data:\n", paste(id_col_nin_data, collapse = "\n "))
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
    dplyr::filter(.data$action == "modify" & .data$id_check == other) |>
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
        .data$id_check == other &
        stringr::str_count(.data$other_old_value, " ") >= 1 &
        is.na(.data$other_new_value)
      ) |>
    dplyr::mutate(onv = paste0({{ id_col }}, ": ", .data$question_name)) |>
    dplyr::pull(.data$onv)

  if (length(onv) >= 1) {

    warning("The following id_col and question_name had two other values and no value anymore in 'other_new_value', please check:\n ", paste(onv, collapse = "\n "))
  }

  # Check if other parent question_name that needs a modification belongs to the rawdata
  other_parent_questions <- log |>
    dplyr::filter(.data$action == "modify" & .data$id_check == other) |>
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
    dplyr::filter(
      .data$action == "modify",
      .data$id_check == other) |>
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
                  .data$id_check == other,
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






#' @title Check analysis
#' @param design A `srvyr` design
#' @param survey The survey sheet
#' @param choices The choices sheet
#' @param analysis Some analysis
#' @param col A variable from design
#' @param group A grouping variable, quoted
#' @param level Some confidence level
#'
#' @family functions to check logs and check lists
#'
#' @importFrom rlang .data
#'
#' @export
check_analysis <- function(design, survey, choices, analysis, col, group = NULL, level){

  des <- design$variables

  # Check for allowed analysis --------
  if (!(analysis %in% c("interact", "ratio", "prop_simple", "prop_simple_overall", "prop_multiple", "prop_multiple_overall", "mean", "median", "count_numeric"))) rlang::abort("Not one of the allowed analysis. Did you mean the right type of analysis?")



  # Check for column in design --------
  if (analysis != "ratio") {

    col_name <- rlang::enquo(col) |> rlang::as_name()
    if_not_in_stop(design, col_name, "design", "col")

    # # Check if col is in survey names --------
    # survey_names <- stats::na.omit(survey$name)
    # is_col_in <- col_name %in% survey_names
    #
    # if(!is_col_in) rlang::abort(glue::glue("{col_name} is not in the Survey sheet names; please check."))


  }

  # Check for ratio columns in design --------
  if (analysis == "ratio"){

    ratio_cols <- stringr::str_split(col, ",", simplify = F) |>
      purrr::flatten_chr() |>
      stringr::str_squish()

    if (length(ratio_cols) != 2) {
      rlang::abort(c(
        "Erreur pour le calcul du ratio",
        "*" = "Il ne contient pas deux vecteurs",
        "i" = paste0("Revoir l'argument col: ", col_name)))
    }

    if_not_in_stop(design, ratio_cols, "design", "col")

  }

  # Check if numeric cols for mean and median --------
  if (analysis %in% c("median", "mean", "count_numeric")) {
    type_col <- des |> srvyr::pull({{ col }})
    if (!is.numeric(type_col)) abort_bad_argument("col", "be numeric", not = type_col)
  }

  # Check if for level --------
  if (level < 0.9) {
    if (analysis != "ratio") rlang::warn(glue::glue("The confidence level used for analysis '{analysis}' for column '{col_name}' is below 90%."))
    if (analysis == "ratio") rlang::warn(glue::glue("The confidence level used for analysis '{analysis}' for column '{col}' is below 90%."))

  }

  # Check for group in design --------
  if (is.null(group)) {group_name <- NA_character_} else {

    group_name <- rlang::enquo(group) |> rlang::as_name()
    if_not_in_stop(design, group_name, "design", "group")

    type_group <- design |> srvyr::pull({{ group }})
    if (!is.character(type_group)) abort_bad_argument("group", "be character", not = type_group)

    # is_col_in <- group_name %in% survey_names
    #
    # if(!is_col_in) rlang::abort(glue::glue("{group_name} is not in the Survey sheet names; please check."))

  }

  # Check if column and group are the same in design --------
  if (analysis != "ratio") {
    if (!is.null(group) & (group_name == col_name)) {
      rlang::abort("`group` must not be the same as `col`")
    }
  }

  return(TRUE)

}



#' @title Check analysis
#'
#' @param dap Some dap
#' @param design A `srvyr` design
#' @param survey The survey sheet
#' @param choices The choices sheet

#'
#' @family functions to check logs and check lists
#'
#' @importFrom rlang .data
#'
#' @export
check_analysis_dap <- function(dap, design, survey, choices){

  dap_names <- c("id_analysis", "rq", "sub_rq", "indicator", "recall", "question_name", "subset", "analysis_name", "none_label", "group_name", "group", "level", "na_rm", "vartype")

  # Check if the column names are the right names
  if_not_in_stop(dap, dap_names, "dap")

  # Check if all id_check are there
  if(rlang::is_na(dap$id_analysis)) rlang::abort("There are missing `id_analysis` values")

  # Check if all question_names are there
  if(rlang::is_na(dap$question_name)) rlang::abort("There are missing `question_name` values")

  # Check if all id_analysis
  if (length(unique(dap$id_analysis)) < nrow(dap)) rlang::warn(" ")
  # Check if different levels in DAP
  if (length(unique(dap$level)) > 1) rlang::warn("There are different confidence levels in this DAP.")

  # Check if different levels in DAP
  if (length(unique(dap$vartype)) > 1) rlang::warn("There are different confidence levels in this DAP.")


}
