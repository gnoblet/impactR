# Helo utils!

# All utils to work with kobo surveys and choices
# - get simple and multiple choices names
# - get other and other parent names
# - get choices
# - split survey "type" column
# - label data



#' @title Get all select one / simple choices from survey sheet
#'
#' @param survey The survey sheet from Kobo (with column "type" split)
#'
#' @details survey should have a split type column with types of variables such as "select_one", "select_multiple", etc.
#'
#' @return A character vector of select_one questions
#'
#' @export
get_select_one <- function(survey){
#
#   if (typeof(rlang::enquo(type)) == "character"){
#     type <- rlang::sym(type)
#   }

  select_one <- survey |>
    dplyr::filter(.data$type == "select_one") |>
    dplyr::pull(.data$name)

  return(select_one)
}


#' @title Get all select multiple from survey sheet
#'
#' @param survey The survey sheet from Kobo (with column "type" split)
#'
#' @details survey should have a split type column with types of variables such as "select_one", "select_multiple", etc.
#'
#' @return A character vector of select_multiple questions
#'
#' @export
get_select_multiple <- function(survey){

  select_multiple <- survey |>
    dplyr::filter(.data$type == "select_multiple") |>
    dplyr::pull(.data$name)

  return(select_multiple)
}




#' @title Get all select multiple and parent other questions
#'
#' @param .tbl A tibble of data
#' @param other  A character vector of the start of all other column names. E.g., other = "other_"
#'
#' @details This function has a tibble and the associated survey sheet as inputs, as well as the beginning of the "other" character string. It returns all columns that exist in the tibble and are either a multiple choice or a parent other question. This allows then to compute these columns with `impactR::count_occ` after parent question may have been recoded.
#'
#' @return A character vector of other questions
#'
#' @export
get_other_parent <- function(.tbl, other){

  other_parent <- tbl_col_start(.tbl, other) |>
    stringr::str_remove(other)

  if (length(other_parent) == 0) {
    rlang::warn("Did you provide the right `other` pattern?")
  }

  return(other_parent)
}



#' @title Get all select multiple and parent other questions
#'
#' @param .tbl A tibble of data
#' @param survey The survey sheet from Kobo (with column "type" split)
#' @param other  A character vector of the start of all other column names. E.g., other = "other_"
#'
#' @details This function has a tibble and the associated survey sheet as inputs, as well as the beginning of the "other" character string. It returns all columns that exist in the tibble and are either a multiple choice or a parent other question. This allows then to compute these columns with `impactR::count_occ` after parent question may have been recoded.
#'
#' @return A character vector of select_multiple and other questions
#'
#' @export
get_select_multiple_and_other_parent <- function(.tbl, survey, other){

  other_parent <- get_other_parent(.tbl, other)

  select_multiple <- get_select_multiple(survey)

  union(other_parent, select_multiple)
}



#' @title Get choices from survey name (and paste them if you want!)
#'
#' @param survey A survey sheet from Kobo (already split with columns list_name and name present)
#' @param choices A choices sheet from Kobo (with column list_name, label and name)
#' @param col A quoted column name
#' @param conc Should choices be concatenated to column name? Default to TRUE. Can only be used together swith label = F.
#' @param label Should the labels be returned?
#'
#' @return A character vector of choices or pasted to `col` choices with sep "_"
#'
#' @export
get_choices <- function(survey, choices, col, conc = T, label = F){

  col_name <- rlang::as_name(rlang::enquo(col))

  if_vec_not_in_stop(survey$name, col_name, "survey$name", "col")

  to_return <- survey |>
    dplyr::filter(.data$name == col_name) |>
    dplyr::select(.data$list_name) |>
    dplyr::left_join(choices, by = "list_name")

  if (!label) {
    to_return <- to_return |> dplyr::pull(.data$name)

    if (rlang::is_true(conc)) {
      to_return <- stringr::str_c(col_name, to_return, sep = "_")
    }} else {
      to_return <- to_return |>
        dplyr::select(.data$name, .data$label)
    }



  return(to_return)
}



#' @title Split survey type and list name
#'
#' @param survey A survey sheet from Kobo
#' @param col_to_split Usually `type`
#' @param into Vector of columns names to split to. Default to c("type", "list_name" )
#' @param sep The separator. Default to " ".
#' @param fill How to fill. Default to NA on the right.
#' @param ... Params to pass to `tidyr::separate`
#'
#' @return  A survey tibble, split
#'
#' @export
split_survey <- function (survey, col_to_split, into = c("type", "list_name"),
                          sep = " ", fill = "right", ...)
{
  col_to_split_name <- rlang::as_name(rlang::enquo(col_to_split))
  if_not_in_stop(survey, col_to_split_name, "survey", "col_to_split")
  survey <- tidyr::separate(survey, {{ col_to_split }}, into = into,
                            sep = sep, fill = fill, ...)
  return(survey)
}



#' @title Label select_multiple question
#'
#' @param data Some kobo data
#' @param survey Some survey sheet, with a split 'type' column, e.g. with `split_survey(type)`. It must have columns 'list_name' and 'name'
#' @param choices The corresponding choices sheet; it must have columns name and label
#' @param id_col The id column; usually uuid
#' @param col The select_multiple column to labelize
#' @param return_df Default returns the updated dataframe; "id_col" returns a dataframe with col and id_col; and, "vector" returns
#'
#' @return A labelled dataframe, sub-dataframe or vector
#'
#' @export
label_select_multiple <- function(data, survey, choices, id_col, col, return_df = NULL){

  # to ensure quoted or unquoted columns can be passed
  col <- rlang::sym(rlang::ensym(col))

  dict <- get_choices(survey, choices, {{ col }}, label = T) |>
    dplyr::rename(choices_label = .data$label, choices_name = .data$name)

  dict <- rlang::set_names(dict$choices_label, dict$choices_name)

  if (rlang::is_na(dict)) {
    recoded <- data

    rlang::warn(paste0(
      "There was no choices value to recode for column: ",
      rlang::as_name(rlang::enquo(col))
    ))
  } else {

     if (sum(is.na(data |> dplyr::pull({{ col }})) == nrow(data))) {
      recoded <- data
     } else {

      recoded <- data |>
        tidyr::separate_rows({{ col }}, sep = " ") |>
        dplyr::mutate("{{ col }}" := as.character({{ col }})) |>
        dplyr::mutate("{{ col }}" := dplyr::recode({{ col }}, !!!dict)) |>
        dplyr::group_by({{ id_col }}) |>
        dplyr::mutate("{{ col }}" := paste0({{ col }}, collapse = " "))  |>
        dplyr::distinct() |>
        dplyr::ungroup()
     }
  }

  if (!rlang::is_null(return_df)) {
    if (return_df == "vector"){
      recoded <- recoded |> dplyr::pull({{ col }})
    } else if (return_df == "id_col") {
      recoded <- recoded |> dplyr::select({{ id_col }}, {{ col }})
    }
  }
  return(recoded)

}



#' @title Label all select_multiple questions
#'
#' @param data Some kobo data
#' @param survey Some survey sheet, with a split 'type' column, e.g. with `split_survey(type)`. It must have columns 'list_name' and 'name'
#' @param choices The corresponding choices sheet; it must have columns name and label
#' @param id_col The id column; usually uuid
#'
#' @return Select-multiple labelled dataframe
#'
#' @export
label_all_select_multiple <- function(data, survey, choices, id_col){

  id_col_name <- rlang::as_name(rlang::enquo(id_col))
  col_names <- colnames(data)

  select_multiples  <- survey |>
    dplyr::filter(.data$type == "select_multiple" & .data$name %in% colnames(data)) |>
    dplyr::pull(.data$name)

  recoded <- purrr::map(select_multiples, ~ label_select_multiple(data, survey, choices, {{ id_col }}, {{ .x }}, "id_col")) |>
    left_joints({{ id_col }})

  data <- diff_tibbles(data, recoded, {{ id_col }})

  recoded <- data |>
    dplyr::left_join(recoded, by = id_col_name) |>
    dplyr::relocate(dplyr::all_of(col_names))

  return(recoded)
}



#' @title Label select_one question
#'
#' @param data Some kobo data
#' @param survey Some survey sheet, with a split 'type' column, e.g. with `split_survey(type)`. It must have columns 'list_name' and 'name'
#' @param choices The corresponding choices sheet; it must have columns name and label
#' @param id_col The id column; usually uuid
#' @param col The select_one column to labelize
#' @param return_df Default returns the updated dataframe; "id_col" returns a dataframe with col and id_col; and, "vector" returns
#'
#' @return A labelled dataframe, sub-dataframe or vector
#'
#' @export
label_select_one <- function(data, survey, choices, id_col, col, return_df = NULL){

  # to ensure quoted or unquoted columns can be passed
  col <- rlang::sym(rlang::ensym(col))

  dict <- get_choices(survey, choices, {{ col }}, label = T) |>
    dplyr::rename(choices_label = .data$label, choices_name = .data$name)

  dict <- rlang::set_names(dict$choices_label, dict$choices_name)

  if (rlang::is_na(dict)) {
    recoded <- data

    rlang::warn(paste0(
      "There was no choices value to recode for column: ",
      rlang::as_name(rlang::enquo(col))
    ))
  } else {

    if (sum(is.na(data |> dplyr::pull({{ col }})) == nrow(data))) {
      recoded <- data
    } else {
      recoded <- data |>
        dplyr::mutate("{{ col }}" := as.character({{ col }})) |>
        dplyr::mutate("{{ col }}" := dplyr::recode({{ col }}, !!!dict))
    }

  }

  if (!rlang::is_null(return_df)) {
    if (return_df == "vector"){
      recoded <- recoded |> dplyr::pull({{ col }})
    } else if (return_df == "id_col") {
      recoded <- recoded |> dplyr::select({{ id_col }}, {{ col }})
    }
  }
  return(recoded)

}



#' @title Label all selecy_one questions
#'
#' @param data Some kobo data
#' @param survey Some survey sheet, with a split 'type' column, e.g. with `split_survey(type)`. It must have columns 'list_name' and 'name'
#' @param choices The corresponding choices sheet; it must have columns name and label
#' @param id_col The id column; usually uuid
#'
#' @return Select-multiple labelled dataframe#'
#'
#' @export
label_all_select_one <- function(data, survey, choices, id_col){

  id_col_name <- rlang::as_name(rlang::enquo(id_col))
  col_names <- colnames(data)

  select_multiples  <- survey |>
    dplyr::filter(.data$type == "select_one" & .data$name %in% colnames(data)) |>
    dplyr::pull(.data$name)

  recoded <- purrr::map(
      select_multiples,
      ~ label_select_one(data, survey, choices, {{ id_col }}, {{ .x }}, "id_col")) |>
    left_joints({{ id_col }})

  data <- diff_tibbles(data, recoded, {{ id_col }})

  recoded <- data |>
    dplyr::left_join(recoded, by = id_col_name) |>
    dplyr::relocate(dplyr::all_of(col_names))

  return(recoded)
}



#' @title Label all select_one and select_multiple questions
#'
#' @param data Some kobo data
#' @param survey Some survey sheet, with a split 'type' column, e.g. with `split_survey(type)`. It must have columns 'list_name' and 'name'
#' @param choices The corresponding choices sheet; it must have columns name and label
#' @param id_col The id column; usually uuid
#'
#' @return Select multiples and select ones labelled dataframe
#'
#' @export
label <- function(data, survey, choices, id_col) {

  data |>
    label_all_select_multiple(survey, choices, {{ id_col }}) |>
    label_all_select_one(survey, choices, {{ id_col }})

}



#' @title Label data columns from survey sheet
#'
#' @param data Some Kobo data.
#' @param survey Some survey sheet, with a split 'type' column, e.g. with `split_survey(type)`. It must have columns 'list_name' and 'name'.
#' @param name_as_label Default to TRUE. Should the variable name be used as the label if label is missing?
#'
#' @return A dictionary or some labelled column names data
#'
#' @export

label_columns <- function(data, survey, name_as_label = TRUE){

  rlang::check_installed("labelled", reason = "to use `label_columns()`")

  survey <- survey |>
    tidyr::drop_na(.data$name)

  if (name_as_label) {
    survey <- survey |>
      dplyr::mutate(label = ifelse(is.na(.data$label), .data$name, .data$label))
  }

  added_cols <- subvec_not_in(colnames(data), survey$name)

  var_labels <- purrr::set_names(survey$label, survey$name) |>  as.list()

  if(length(added_cols) > 0) {
    var_labels_added <- purrr::set_names(added_cols, added_cols) |> as.list()

    var_labels <- c(var_labels, var_labels_added)
  }

  data <- data |>
    labelled::set_variable_labels(.labels = var_labels, .strict = FALSE)

  return(data)

}


#' @title Get dictionary from survey sheet
#'
#' @param data Some Kobo data.
#' @param survey Some survey sheet, with a split 'type' column, e.g. with `split_survey(type)`. It must have columns 'list_name' and 'name'.
#' @param name_as_label Default to TRUE. Should the variable name be used as the label if label is missing?
#'
#' @return A dictionary
#'
#' @export
get_dictionary <- function(data, survey, name_as_label = FALSE){

  rlang::check_installed("labelled", reason = "to use `get_dictionary()`")

  labelled_columns_data <- label_columns(data, survey, name_as_label)

  dictionary <- labelled::generate_dictionary(labelled_columns_data)

  return(dictionary)
}
