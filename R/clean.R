#' @title Rename log columns from French to English
#'
#' @param .tbl A tibble cleaning log (French names)
#'
#' @export
log_names_fr_en <- function(.tbl){
  fr_names <- c("id_check", "nom_question", "label_question", "explication", "feedback", "action", "ancienne_valeur", "nouvelle_valeur", "type", "autre_question_parent", "autre_ancienne_valeur", "autre_nouvelle_valeur", "date_production")
  en_names <- c("id_check", "question_name", "question_label", "why", "feedback", "action", "old_value", "new_value", "type", "other_parent_question", "other_old_value", "other_new_value", "checkdate" )
  .tbl |>
    rename_cols(fr_names, en_names)
}



#' @title Rename log columns from English to French
#'
#' @param .tbl A tibble cleaning log (French names)
#'
#' @export
log_names_en_fr  <- function(.tbl){
  fr_names <- c("id_check", "nom_question", "label_question", "explication", "feedback", "action", "ancienne_valeur", "nouvelle_valeur", "type", "autre_question_parent", "autre_ancienne_valeur", "autre_nouvelle_valeur", "date_production")
  en_names <- c("id_check", "question_name", "question_label", "why", "feedback", "action", "old_value", "new_value", "type", "other_parent_question", "other_old_value", "other_new_value", "checkdate" )

  .tbl |>
    rename_cols(en_names, fr_names)
}



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
    stop("There are wrong actions in column 'action'. It should be either 'keep', 'modify', 'remove' or 'check'.")
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



#' @title Delete from log
#'
#' @param .tbl A tibble
#' @param log A deletion log, which contains a column "action".
#' @param id_col The column id, usually "uuid"
#'
#' @description Removes surveys from .tbl where column "action" in the log is set to "remove".
#'
#' @return A lot of removal
#'
#' @importFrom rlang .data
#'
#' @export
remove_from_log <- function(.tbl, log, id_col = "uuid"){

  id_col_name <- rlang::as_name(rlang::enquo(id_col))

  .tbl |>
    dplyr::anti_join(log |> dplyr::filter(.data$action == "remove"), by = id_col_name)
}



#' @title Remove duplicates
#'
#' @param .tbl A tibble
#' @param log A deletion log, which contains a column "action".
#' @param id_col The column id, usually "uuid"
#'
#' @return Removal - duplicates
#'
#' @description Removes duplicated surveys from .tbl where column "action" in the log is set to "duplicate".
#'
#' @export
remove_duplicate <- function(.tbl, log, id_col = "uuid"){

  id_col_name <- rlang::as_name(rlang::enquo(id_col))

  to_remove <- log |>
    dplyr::filter(.data$action == "duplicate")

  if(nrow(to_remove) == 0) {warning("There was no duplicate entry in the log.")}

  tbl_to_remove <- .tbl |>
    dplyr::filter(id_col_name %in% to_remove |> dplyr::pull( {{ id_col }})) |>
    dplyr::group_by({{ id_col }}) |>
    dplyr::slice_head() |>
    dplyr::ungroup()

  .tbl <- .tbl |>
    dplyr::anti_join(to_remove, by = id_col_name) |>
    dplyr::bind_rows(tbl_to_remove)
}






#' @title Update rows from list
#'
#' @param .tbl A tibble
#' @param .list A list of tibbles with values to update
#' @param id_col The column id, usually "uuid"
#'
#' @return A tibble with values updated
#'
#' @export
update_rows_from_list <- function(.tbl, .list, id_col = "uuid"){

  id_col_name <- rlang::as_name(rlang::enquo(id_col))
  for (i in 1:length(.list)){
    .tbl <- dplyr::rows_update(.tbl, .list[[i]], by = id_col)
  }
  return(.tbl)
}



#' @title Modify from log
#'
#' @param .tbl A tibble
#' @param log A cleaning log
#' @param id_col The column id, usually "uuid"
#' @param type Either "character" or "double"
#'
#' @return A tibble with values modified
#'
#' @importFrom rlang .data
#'
#' @export
modify_from_log <- function(.tbl, log, id_col, type){

  to_modify <- log |>
    dplyr::filter(.data$action == "modify" & .data$id_check != "other" & .data$type == type) |>
    dplyr::select(.data$id_check, !!rlang::sym(id_col), .data$question_name, .data$new_value) |>
    dplyr::group_split(.data$question_name) |>
    purrr::map(~ .x |>
                 tidyr::pivot_wider(!!rlang::sym(id_col),
                                    names_from = .data$question_name,
                                    values_from = .data$new_value))

  # Finition
  .tbl |> update_rows_from_list(to_modify)
}




#' @title Recode "other_" from log
#'
#' @param .tbl A tibble
#' @param log A cleaning log
#' @param id_col The column id, usually "uuid"
#'
#' @return A tibble with "other_" recoded
#'
#' @importFrom rlang .data
#'
#' @export
recode_other_from_log <- function(.tbl, log, id_col = "uuid"){

  to_recode <- log |>
    dplyr::filter(.data$action == "modify" & .data$id_check == "other") |>
    dplyr::select(.data$id_check, !!rlang::sym(id_col), .data$question_name, .data$new_value) |>
    dplyr::group_split(.data$question_name) |>
    purrr::map(~ .x |>
                 dplyr::mutate(new_value = as.character(.data$new_value))) |>
    purrr::map(~ .x |>
                 tidyr::pivot_wider(!!rlang::sym(id_col),
                                    names_from = .data$question_name,
                                    values_from = .data$new_value))


  .tbl <- .tbl |> update_rows_from_list(to_recode)

  return(.tbl)
}




#' @title Recode parent "other_" from log
#'
#' @param .tbl A tibble
#' @param log A cleaning log
#' @param id_col The column id, usually "uuid"
#'
#' @return A tibble parent "other_" recoded
#'
#' @importFrom rlang .data
#'
#' @export
recode_other_parent_from_log <- function(.tbl, log, id_col = "uuid"){
  to_recode <- log |>
    dplyr::filter(.data$action == "modify" & .data$id_check == "other") |>
    dplyr::select(.data$id_check, !!rlang::sym(id_col), .data$question_name, .data$other_parent_question, .data$other_new_value) |>
    dplyr::group_split(.data$question_name) |>
    purrr::map(~ .x |>
                 tidyr::pivot_wider(!!rlang::sym(id_col),
                                    names_from = .data$other_parent_question,
                                    values_from = .data$other_new_value)
    )

  .tbl <- .tbl |>  update_rows_from_list(to_recode)

  return(.tbl)
}



#' @title Count occurences of variables
#'
#' @param .tbl A tibble
#' @param id_col Usually uuid... to count.
#' @param col A column of .tbl
#'
#' @details This is solely written to mimic variables names produced by Kobo after use of `janitor::clean_names()`. Example : *variable* is a multiple choice variable and *variable_choice1* is its count variable of choice 1.
#'
#' @export
count_occ <- function(.tbl, id_col, col){

  col_name <- rlang::as_name(rlang::enquo(col))
  id_col_name <- rlang::as_name(rlang::enquo(id_col))

  if_not_in_stop(.tbl, col_name, ".tbl", "col")

  agg <- .tbl |>
    dplyr::select({{ id_col}}, {{ col }}) |>
    tidyr::drop_na({{ col }}) |>
    tidyr::separate_rows({{ col }}, sep = " ") |>
    dplyr::mutate(`n` = 1) |>
    tidyr::pivot_wider({{ id_col }},
                        names_from = {{ col }},
                        names_prefix = paste0(col_name, "_"),
                        values_from = .data$n,
                        values_fill = 0)

  return(agg)
}



#' @title Count all occurences
#'
#' @param .tbl A tibble
#' @param survey A survey sheet from Kobo (with column "type" split)
#' @param choices A choices sheet from Kobo
#' @param id_col Usually uuid... to count.
#' @param other  A character vector of the start of all other column names. E.g., other = "other_"
#' @param type Unquoted column type in the survey sheet. Default to type.
#' @param return Either "count" (a list of counts of select_multiple) or "updated" (the updated .tbl).
#'
#' @return An updated tibble or a list of occurences
#'
#' @export
count_occ_all <- function(.tbl, survey, choices, id_col, other, type = "type", return = "updated"){

  if (!(return %in% c("count", "updated"))) {rlang::abort("`return` must either be 'count' or 'updated'")}

  multiple_cols <- get_multiple(survey, type)
  multiple_cols_with_choices <- purrr::map(multiple_cols, get_choices, survey = survey, choices = choices, conc = T) |>
    purrr::flatten_chr() |>
    janitor::make_clean_names()

  to_return <- purrr::map(multiple_cols, count_occ, .tbl = .tbl, id_col = {{ id_col }})


  if (return == "count"){

    return(to_return)

  } else if (return == "updated") {

    cols_order <- colnames(.tbl)

    joined <- left_joints(to_return, {{ id_col }})

    # Multiple cols from survey in .tbl
    count_cols <- subvec_in(multiple_cols_with_choices, colnames(.tbl))

    # In .tbl, not in joind --> needs 0s or NAs
    count_cols <- subvec_not_in(count_cols, colnames(joined))

    updated <- diff_tibbles(.tbl, joined,{{ id_col }}) |>
      dplyr::left_join(joined, rlang::as_name(rlang::enquo(id_col))) |>
      dplyr::mutate(dplyr::across(count_cols, function(x) {dplyr::if_else(is.na(x), NA_real_, 0) })) |>
      dplyr::relocate(colnames(cols_order))

    return(updated)
  }
}



#' @title Clean all
#'
#' @param .tbl A tibble
#' @param log A log, which contains a column "action".
#' @param survey A survey sheet from Kobo (with column "type" split)
#' @param choices A choices sheet from Kobo
#' @param id_col Usually uuid... to count.
#' @param other  A character vector of the start of all other column names. E.g., other = "other_"
#'
#' @details Apply all cleaning functions in the right order, modify character and double variables, recode others and other parents, remove duplicates, remove surveys, recount occurences. It uses default for count_occ_all.
#'
#' @return A cleaned tibble
#'
#' @export
clean_all <- function(.tbl, log, survey, choices, id_col, other){
  .tbl <- modify_from_log(.tbl, log, {{ id_col }}, "character")
  .tbl <- modify_from_log(.tbl, log, {{ id_col }}, "double")
  .tbl <- recode_other_from_log(.tbl, log, id_col = rlang::as_name(rlang::enquo(id_col)))
  .tbl <- recode_other_parent_from_log(.tbl, log, id_col = rlang::as_name(rlang::enquo(id_col)))
  .tbl <- remove_duplicate(.tbl, log, id_col = rlang::as_name(rlang::enquo(id_col)))
  .tbl <- remove_from_log(.tbl, log, id_col = rlang::as_name(rlang::enquo(id_col)))
  .tbl <- count_occ_all(.tbl, survey, choices, {{ id_col }}, other, type = "type", return = "updated")

  return(.tbl)
}

