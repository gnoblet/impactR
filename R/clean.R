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



#' @title Delete from log
#'
#' @param .tbl A tibble
#' @param log A deletion log, which contains a column "action"
#' @param id_col The column id, usually "uuid"
#'
#' @description Removes surveys from .tbl where column "action" in the log is set to "remove"
#'
#' @return A lot of removal
#'
#' @importFrom rlang .data
#'
#' @export
remove_from_log <- function(.tbl, log, id_col){

  id_col_name <- rlang::as_name(rlang::enquo(id_col))
  if_not_in_stop(.tbl, id_col_name, ".tbl", "id_col")

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
remove_duplicate <- function(.tbl, log, id_col){

  id_col_name <- rlang::as_name(rlang::enquo(id_col))
  if_not_in_stop(.tbl, id_col_name, ".tbl", "id_col")

  to_remove <- log |>
    dplyr::filter(.data$action == "duplicate")

  if (nrow(to_remove) > 0) {
    tbl_to_remove <- .tbl |>
    dplyr::filter(id_col_name %in% to_remove |> dplyr::pull( {{ id_col }})) |>
    dplyr::group_by({{ id_col }}) |>
    dplyr::slice_head() |>
    dplyr::ungroup()

  .tbl <- .tbl |>
    dplyr::anti_join(to_remove, by = id_col_name) |>
    dplyr::bind_rows(tbl_to_remove)
  }

  return(.tbl)
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
update_rows_from_list <- function(.tbl, .list, id_col){

  id_col_name <- rlang::as_name(rlang::enquo(id_col))
  if_not_in_stop(.tbl, id_col_name, ".tbl", "id_col")

  for (i in 1:length(.list)){
    .tbl <- dplyr::rows_update(.tbl, .list[[i]], by = id_col_name)
  }
  return(.tbl)
}



#' @title Modify from log
#'
#' @param .tbl A tibble
#' @param log A cleaning log
#' @param id_col The column id, usually "uuid"
#' @param type Either "character" or "double"
#' @param other A character vector of the start of all other column names. E.g., other = "other_"
#'
#' @return A tibble with values modified
#'
#' @importFrom rlang .data
#'
#' @export
modify_from_log <- function(.tbl, log, id_col, type, other){

  id_col_name <- rlang::as_name(rlang::enquo(id_col))
  if_not_in_stop(.tbl, id_col_name, ".tbl", "id_col")

  to_modify <- log |>
    dplyr::filter(.data$action == "modify" & .data$id_check != other & .data$type == type)
  if (nrow(to_modify) > 0) {
    to_modify <- to_modify |>
      dplyr::select(.data$id_check, {{ id_col }}, .data$question_name, .data$new_value) |>
      dplyr::group_split(.data$question_name) |>
      purrr::map(~ .x |>
                   tidyr::pivot_wider(!!rlang::enquo(id_col),
                                      names_from = .data$question_name,
                                      values_from = .data$new_value))
    # Finition
    .tbl <- .tbl |> update_rows_from_list(to_modify, {{ id_col }})
  }

    return(.tbl)
}




#' @title Recode "other_" from log
#'
#' @param .tbl A tibble
#' @param log A cleaning log
#' @param id_col The column id, usually "uuid"
#' @param other  A character vector of the start of all other column names. E.g., other = "other_"
#'
#' @return A tibble with "other_" recoded
#'
#' @importFrom rlang .data
#'
#' @export
recode_other_from_log <- function(.tbl, log, id_col, other){

  id_col_name <- rlang::as_name(rlang::enquo(id_col))
  if_not_in_stop(.tbl, id_col_name, ".tbl", "id_col")

  to_recode <- log |>
    dplyr::filter(.data$action == "modify" & .data$id_check == other)

  if (nrow(to_recode) > 0) {
    to_recode <- to_recode |>
      dplyr::select(.data$id_check, {{ id_col }}, .data$question_name, .data$new_value) |>
      dplyr::group_split(.data$question_name) |>
      purrr::map(~ .x |>
                   dplyr::mutate(new_value = as.character(.data$new_value))) |>
      purrr::map(~ .x |>
                   tidyr::pivot_wider({{ id_col }},
                                      names_from = .data$question_name,
                                      values_from = .data$new_value))

    .tbl <- .tbl |> update_rows_from_list(to_recode, {{ id_col }})
  }

  return(.tbl)
}




#' @title Recode parent "other_" from log
#'
#' @param .tbl A tibble
#' @param log A cleaning log
#' @param id_col The column id, usually "uuid"
#' @param other  A character vector of the start of all other column names. E.g., other = "other_"
#'
#' @return A tibble parent "other_" recoded
#'
#' @importFrom rlang .data
#'
#' @export
recode_other_parent_from_log <- function(.tbl, log, id_col, other){

  id_col_name <- rlang::as_name(rlang::enquo(id_col))
  if_not_in_stop(.tbl, id_col_name, ".tbl", "id_col")

  to_recode <- log |>
    dplyr::filter(.data$action == "modify" & .data$id_check ==  other)

  if (nrow(to_recode) > 0) {
    to_recode <- to_recode |>
      dplyr::select(.data$id_check, {{ id_col }}, .data$question_name, .data$other_parent_question, .data$other_new_value) |>
      dplyr::group_split(.data$question_name) |>
      purrr::map(~ .x |>
                   tidyr::pivot_wider({{ id_col }},
                                      names_from = .data$other_parent_question,
                                      values_from = .data$other_new_value)
      )
    .tbl <- .tbl |>  update_rows_from_list(to_recode, {{ id_col }})
  }

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
  if_not_in_stop(.tbl, id_col_name, ".tbl", "id_col")


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
#' @param id_col Usually uuid... to count
#' @param output Either "count" (a list of counts of select_multiple) or "updated" (the updated .tbl).
#'
#' @return An updated tibble or a list of occurences
#'
#' @export
count_occ_all <- function(.tbl, survey, choices, id_col, output = "updated"){

  # Initialize -----------------------------

  # Return's choice
  if (!(output %in% c("count", "updated"))) {rlang::abort("`return` must either be 'count' or 'updated'")}

  # Is id_col in .tbl?
  id_col_name <- rlang::as_name(rlang::enquo(id_col))
  if_not_in_stop(.tbl, id_col_name, ".tbl", "id_col")


  # Prepare objects -----------------------------

  #  Get multiple questions
  mult_question <- get_select_multiple(survey)

  # Get choices from multiple questions
  mult_choices<- mult_question |>
    purrr::set_names() |>
    purrr::map(get_choices, survey = survey, choices = choices, conc = T) |>
    purrr::map(~ janitor::make_clean_names(.x))


  # Count occurences for each multiple question -----------------------------

  to_return <- mult_question |>
    purrr::set_names() |>
    purrr::map(count_occ, .tbl = .tbl, id_col = {{ id_col }}) |>
    purrr::map2(.y = mult_question, ~ dplyr::left_join(.tbl |> dplyr::select({{ id_col }}, dplyr::all_of(.y)), .x, by = id_col_name)) |>
    purrr::map(~ janitor::clean_names(.x))


  # Simple count return -----------------------------

  if (output == "count"){

    return(to_return)

  }
  else if (output == "updated") {


  # Add missing with zeros and bind all -----------------------------

    cols_order <- colnames(.tbl)

    tbl_uuid <- .tbl |> dplyr::select({{ id_col }})

    # Multiple choices being zeros from survey
    mult_choices_zeros <- mult_choices |>
      # Create zero row tibble with mult as colnames
      purrr::map(~
                   tibble::as_tibble(
                     sapply(.x, \(x) double())
                   )
      ) |>
      # Add a zero row uuid column
      purrr::map(~ .x |> tibble::add_column(!!id_col_name := character(0), .before = 1)) |>
      # Add all uuids --> NA tibbles for all other cols, then mutate to 0 if parent col is not NA
      purrr::map2(.y = mult_question,
                  ~ dplyr::left_join(.tbl |> dplyr::select({{ id_col }}, dplyr::all_of(.y)),
                                     .x,
                                     by = id_col_name) |>
                    dplyr::mutate(
                      dplyr::across(
                        where(is.double),
                        function(el) {ifelse(is.na(!!rlang::sym(.y)), el, 0)})
                    )
      ) |>
      purrr::map2(.y = to_return, ~ diff_tibbles(.x, .y, {{ id_col }})) |>
      left_joints({{ id_col }})

  # Final joints -----------------------------

    joined <- to_return |>
      left_joints({{ id_col }}) |>
      dplyr::left_join(mult_choices_zeros, by = id_col_name)

    updated <- .tbl |>
      diff_tibbles(joined, {{ id_col }}) |>
      dplyr::left_join(joined, by = id_col_name) |>
      dplyr::relocate(dplyr::all_of(cols_order))

    return(updated)
  }
}









#' @title Clean all
#'
#' @param .tbl A tibble
#' @param log A log, which contains a column "action".
#' @param survey A survey sheet from Kobo (with column "type" split)
#' @param choices A choices sheet from Kobo
#' @param id_col Usually uuid... to count
#' @param other A character vector of the start of all other column names. E.g., other = "other_"
#'
#' @details Apply all cleaning functions, after checking the log, in the right order, modify character and double variables, recode others and other parents, remove duplicates, remove surveys, recount occurences. It uses default for count_occ_all.
#'
#' @return A cleaned tibble
#'
#' @export
clean_all <- function(.tbl, log, survey, choices, id_col, other){

  check_cleaning_log(log, .tbl, {{ id_col }}, other)

  .tbl <- modify_from_log(.tbl, log, {{ id_col }}, "character", other)
  .tbl <- modify_from_log(.tbl, log, {{ id_col }}, "double", other)
  .tbl <- recode_other_from_log(.tbl, log, {{ id_col }}, other)
  .tbl <- recode_other_parent_from_log(.tbl, log, {{ id_col }}, other)
  .tbl <- remove_duplicate(.tbl, log, {{ id_col }})
  .tbl <- remove_from_log(.tbl, log, {{ id_col }})
  .tbl <- count_occ_all(.tbl, survey, choices, {{ id_col }}, output = "updated")

  return(.tbl)
}
