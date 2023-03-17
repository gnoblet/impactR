#' @title Tidy rowwise optimum (default to pmax) of several columns
#'
#' @param .tbl A tibble
#' @param ... Unquoted numeric column names
#' @param optimum Should we calculate "max", "min" or "both"? Default to TRUE
#' @param max_name Column name for the mutated pmax
#' @param min_name Column name for the mutated pmin
#' @param na_rm Remove NAs. Default to TRUE
#' @param keep To be used within mutate. Default to "all".
#'
#' @importFrom rlang `:=`
#'
#' @return A tibble with pmax, pmin or both (and all columns or none, depending on "keep")
#'
#' @export
rowwise_optimum <- function(.tbl, ...,  optimum = "max", max_name = "pmax", min_name = "pmin", na_rm = TRUE, keep = "all") {

  cols <- rlang::enquos(...)
  quoted_cols <- purrr::map_chr(cols, rlang::as_name)
  purrr::map(quoted_cols, ~ if_not_in_stop(.tbl, .x, ".tbl", "..."))
  purrr::map(quoted_cols, \(.x) {if(!is.numeric(.tbl |> dplyr::pull(dplyr::all_of(.x)))) {abort_bad_argument(.x, "be numeric", .tbl$.x)}})


  if (optimum == "both") {
    to_return <- .tbl |> dplyr::mutate(!!max_name := pmax(!!!cols  , na.rm = na_rm),
                                       !!min_name := pmin(!!!cols, na.rm = na_rm), .keep = keep)
  } else if (optimum == "max") {
    to_return <- .tbl |> dplyr::mutate(!!max_name := pmax(!!!cols, na.rm = na_rm), .keep = keep)
  } else if (optimum == "min") {
    to_return <- to_return |> dplyr::mutate(!!min_name := pmin(!!!cols, na.rm = na_rm), .keep = keep)
  } else {
    stop("Arg 'optimum' should be either 'max', 'min' or 'both'.")
  }


  return(to_return)
}



#' @title Survey proportion
#'
#' @param design A srvyr::design object
#' @param col A column to calculate proportion from
#' @param group A quoted or unquoted vector of columns to group by. Default to NULL for no group.
#' @param na_rm Should NAs from `col` be removed? Default to TRUE. na.rm does not work anymore within srvyr functions (workaround for now)
#' @param stat_name What should the statistic's column be named? Default to "prop"
#' @param ... Parameters to pass to `srvyr::survey_prop()`
#'
#' @return A survey-summarized-proportion tibble
#'
#' @export
svy_prop <- function(design, col, group = NULL, na_rm = TRUE, stat_name = "prop", ...){

  if (rlang::is_true(na_rm)) {
    design <- design |>
      srvyr::drop_na({{ col }})
  }

  to_return <- design |>
    srvyr::group_by(dplyr::across({{ group }}), dplyr::across({{ col }})) |>
    srvyr::summarize(
      "{stat_name}" := srvyr::survey_prop(...),
      "n_unw" := srvyr::unweighted(srvyr::n())
      ) |>
    dplyr::mutate("{stat_name}_unw" := prop.table(.data$n_unw)) |>
    dplyr::group_by(dplyr::across({{ group }})) |>
    dplyr::mutate(
      "n_tot" := sum(!!rlang::sym("n_unw"), na.rm = FALSE)
    ) |>
    dplyr::ungroup()

  return(to_return)
}



#' @title Survey mean
#'
#' @param design A srvyr::design object
#' @param col A column to calculate mean from
#' @param group A quoted or unquoted vector of columns to group by. Default to NULL for no group.
#' @param na_rm Should NAs from `col` be removed? Default to TRUE. na.rm does not work anymore within srvyr functions (workaround for now). It should work for `survey_mean`, matter of precaution
#' @param stat_name What should the statistic's column be named? Default to "mean"
#' @param ... Parameters to pass to `srvyr::survey_mean()`
#'
#' @return A survey-summarized-proportion tibble
#'
#' @export
svy_mean <- function(design, col, group = NULL, na_rm = TRUE, stat_name = "mean", ...){

  if (rlang::is_true(na_rm)) {
    design <- design |>
      srvyr::drop_na({{ col }})
  }

  to_return <- design |>
    srvyr::group_by(dplyr::across({{ group }})) |>
    srvyr::summarize(
      "{stat_name}" := srvyr::survey_mean({{ col }},...),
      "{stat_name}_unw" := srvyr::unweighted(mean({{ col }})),
      "n_unw" := srvyr::unweighted(srvyr::n())) |>
    dplyr::mutate(
     "n_tot" := !!rlang::sym("n_unw")
    )

  return(to_return)
}



#' @title Survey median
#'
#' @param design A srvyr::design object
#' @param col A column to calculate median from
#' @param group A quoted or unquoted vector of columns to group by. Default to NULL for no group
#' @param na_rm Should NAs from `col` be removed? Default to TRUE. na.rm does not work anymore within srvyr functions (workaround for now). It should work for `survey_median`, matter of precaution
#' @param stat_name What should the statistic's column be named? Default to "median"
#' @param ... Parameters to pass to `srvyr::survey_median()`
#'
#' @return A survey-summarized-proportion tibble
#'
#' @export
svy_median <- function(design, col, group = NULL, na_rm = TRUE, stat_name = "median", ...){

  if (rlang::is_true(na_rm)) {
    design <- design |>
      srvyr::drop_na({{ col }})
  }

  to_return <- design |>
    srvyr::group_by(dplyr::across({{ group }})) |>
    srvyr::summarize(
      "{stat_name}" := srvyr::survey_median({{ col }}, ...),
      "{stat_name}_unw" := srvyr::unweighted(stats::median({{ col }})),
      "n_unw" := srvyr::unweighted(srvyr::n()))|>
    dplyr::mutate(
     "n_tot" := !!rlang::sym("n_unw")
    )

  return(to_return)
}



#' @title Survey count for numeric variables
#'
#' @param design A srvyr::design object
#' @param col A column to count from
#' @param group A quoted or unquoted vector of columns to group by. Default to NULL for no group
#' @param na_rm Should NAs from `col` be removed? Default to TRUE. na.rm does not work anymore within srvyr functions (workaround for now). It should work for `survey_prop`, matter of precaution
#' @param stat_name What should the statistic's column be named? Default to "count_numeric"
#' @param ... Parameters to pass to `srvyr::survey_median()`
#'
#' @return A survey-summarized-proportion tibble
#'
#' @export
svy_count_numeric <- function(design, col, group = NULL, na_rm = TRUE, stat_name = "count_numeric", ...){

  if (rlang::is_true(na_rm)) {
    design <- design |>
      srvyr::drop_na({{ col }})
  }

  to_return <- design |>
    srvyr::mutate("{{ col }}" := as.character({{ col }})) |>
    srvyr::group_by(dplyr::across({{ group }}), dplyr::across({{ col }})) |>
    srvyr::summarize(
      "{stat_name}" := srvyr::survey_prop(...),
      "n_unw" := srvyr::unweighted(srvyr::n())) |>
    dplyr::mutate("{stat_name}_unw" := prop.table(.data$n_unw)) |>
    dplyr::group_by(dplyr::across({{ group }})) |>
    dplyr::mutate(
    "n_tot" := sum(!!rlang::sym("n_unw"), na.rm = FALSE)
    ) |>
    dplyr::ungroup()

  return(to_return)
}





#' @title Survey interaction means
#'
#' @param design A srvyr::design object
#' @param interact_cols A vector of columns to get interactions from
#' @param group A vector of columns to group by. Default to NULL
#' @param unnest_interaction Should interaction be unnested? Default to TRUE
#' @param na_rm Should NAs from `interact_cols` be removed? Default to TRUE
#' @param stat_name What should the statistic's column be named? Default to "prop"
#' @param ... Parameters to pass to srvyr::survey_mean()
#'
#' @return A survey-summarized-interaction tibble
#'
#' @export
svy_interact <- function(design, interact_cols, group = NULL, arrange = TRUE, unnest_interaction = TRUE, na_rm = TRUE, stat_name = "prop", ...){

  if (rlang::is_true(na_rm)) {
    design <- design |>
      srvyr::drop_na({{ interact_cols }})
  }

  to_return <- design |>
    srvyr::group_by(srvyr::across({{ group }}),
                    srvyr::interact(interaction = srvyr::across({{ interact_cols }}))) |>
    srvyr::summarize(
      "{stat_name}" := srvyr::survey_mean(...),
      "n_unw" := srvyr::unweighted(srvyr::n()))

  if (arrange) {
    to_return <- dplyr::arrange(to_return, dplyr::desc(.data$prop)) 
  }

  to_return <- to_return |>
    dplyr::mutate("{stat_name}_unw" := prop.table(.data$n_unw)) |>
    dplyr::group_by(dplyr::across({{ group }})) |>
    dplyr::mutate(
    "n_tot" := sum(!!rlang::sym("n_unw"), na.rm = FALSE)
    ) |>
    dplyr::ungroup()

  if (unnest_interaction) {
    to_return <- to_return |> tidyr::unnest("interaction")
  }

  return(to_return)
}



#' @title Survey ratio
#'
#' @param design A srvyr::design object
#' @param num The numerator column
#' @param denom The denominator column
#' @param group A quoted or unquoted vector of columns to group by. Default to NULL for no group
#' @param stat_name What should the statistic's column be named? Default to "ratio"
#' @param na_rm Should NAs from `num` and `denom` be removed? Default to TRUE. na.rm does not work anymore within srvyr functions (workaround for now). It should work for `survey_mean`, matter of precaution
#' @param ... Parameters to pass to srvyr::survey_mean()
#'
#' @return A survey-summarized-interaction tibble
#'
#' @export
svy_ratio <- function(design, num, denom, group = NULL, na_rm = TRUE, stat_name = "ratio", ...){

  if (rlang::is_true(na_rm)) {
    design <- design |>
      srvyr::drop_na({{ num }}) |>
      srvyr::drop_na({{ denom }})
  }

  to_return <- design |>
    srvyr::group_by(dplyr::across({{ group }})) |>
    srvyr::summarize(
      "{stat_name}" := srvyr::survey_ratio({{ num }}, {{ denom }},...),
      "{stat_name}_unw" := srvyr::unweighted(sum({{ num }}) / sum({{ denom }})),
      "n_unw" := srvyr::unweighted(srvyr::n())) |>
    dplyr::mutate(
     "n_tot" := !!rlang::sym("n_unw")
    )

  return(to_return)
}





#' @title Make analysis
#'
#' @param design A design object
#' @param survey The survey sheet from Kobo that contains at least column 'list_name' (split from 'type') and 'name'
#' @param choices The choices sheet from Kobo contains at least column 'list_name' (split from 'type') and 'name'
#' @param col Column to make analysis from
#' @param analysis One of "median", "mean", "prop_simple", "prop_simple_overall", "prop_multiple", "prop_multiple_overall", "ratio"
#' @param none_label Label for recoding NA if "prop_simple_overall" is selected. If NULL, the code "none_prop_simple_overall" is used as a label.
#' @param group A grouping variable, quoted
#' @param level Confidence level to pass to `svy_*` functions
#' @param na_rm Should NAs be removed prior to calculation ?
#' @param vartype Parameter from `srvyr` functions. Default to "ci"
#' @param get_label Should label(s) be joined? Default to `TRUE`
#'
#' @description This function still is experimental. `r lifecycle::badge("experimental")`
#'
#' @section General information:
#'
#'   * Survey and choices must be the final recoded versions of the data. For instance if you have recoded some "other" answers to new choices in the dataset. It must have been added to the choices sheet of the Kobo tool.
#'
#'   * Design is simply a design object mapped from the dataset thanks to `srvyr::as_survey_design()`.
#'   * Variables colnames must follow the following pattern in order for
#'
#' @section Types of analysis:
#' * Median: "median" computes the weighted median using `svy_median()` under the hood
#' * Mean : "mean" computes the weighted mean using `svy_mean()` under the hood
#' * Count numeric : "count_numeric" considers a numeric variable as a character one and then computes a simple proportion out of it.
#' * Simple proportion : there are two different possible calculation. The first one "prop_simple" removes NA values and calculate the weighted proportion thanks to `svy_prop()`. The second one "prop_simple_overall" mutate NA values to "none_prop_simple_overall" and then calculates the weighted proportion.
#' * Multiple proportion : there are two different possible calculation. The first one "prop_multiple" removes NA values from each dummy 1/0 choice column and calculate the weighted proportion thanks to `svy_prop()`. The second one "prop_multiple_overall" mutate NA values to 0 for each dummy 1/0 choice column and then calculates the weighted proportion.
#' * Ratio: ratio is still under construction for managing NAs. For now it removes them and simply computes the ratio of numeric columns col1 over col2, when `col` is "col1,col2".
#'
#' @return A summarized analysis
#'
#' @export
make_analysis <- function(
    design,
    survey,
    choices,
    col,
    analysis,
    none_label = NULL,
    group = NULL,
    level = 0.9,
    na_rm = TRUE,
    vartype = "ci",
    get_label = TRUE
){


  #-------- Checks

  check_analysis(design, survey, choices, analysis, {{ col }}, {{ group }}, level)

  col_name <- rlang::enquo(col) |> rlang::as_name()
  if (is.null(group)) {group_name <- NA_character_} else {

    group_name <- rlang::enquo(group) |> rlang::as_name()}

  # TO DO:
  # - check the type of the question using survey
  # - should we default to some automation if analysis is not provided?
  # - Improve grouping, unquoted variables, multiple grouping variables, etc.

  #-------- Make analysis

  stat_name = "stat"

  none <- "none_prop_simple_overall"


  if (analysis == "median") {

    design <- design |>
      srvyr::drop_na({{ col }})

    return <- design |>
      svy_median(!!rlang::sym(rlang::ensym(col)), {{ group }}, na_rm = na_rm, stat_name = stat_name, level = level, vartype = vartype) |>
      dplyr::mutate(name = col_name,
                    analysis = analysis)


  } else if (analysis == "mean") {

    design <- design |>
      srvyr::drop_na({{ col }})

    return <- design |>
      svy_mean(!!rlang::sym(rlang::ensym(col)), {{ group }}, na_rm = na_rm, stat_name = stat_name, level = level, vartype = vartype) |>
      dplyr::mutate(name = col_name,
                    analysis = analysis)



  } else if (analysis == "count_numeric") {

    design <- design |>
      srvyr::drop_na({{ col }})

    return <- design |>
      svy_count_numeric(!!rlang::sym(rlang::ensym(col)), {{ group }}, na_rm = na_rm, stat_name = stat_name, level = level, vartype = vartype) |>
      dplyr::mutate(name = col_name,
                    analysis = analysis) |>
      dplyr::rename(choices = {{ col }}) |>
      dplyr::mutate(choices_label = choices)


  } else if (analysis == "prop_simple") {

    design <- design |>
      srvyr::drop_na({{ col }})

    return <- design |>
      svy_prop({{ col }}, {{ group }}, na_rm = na_rm, stat_name = stat_name, level = level, vartype = vartype) |>
      dplyr::mutate(name = col_name,
                    analysis = analysis) |>
      dplyr::rename(choices = {{ col }})


    if (get_label) {

      got_choices <- get_choices(survey, choices, {{ col }}, label = TRUE)

      if (nrow(got_choices) == 0) {

        return <- return |>
          dplyr::mutate(choices_label = .data$choices)

      } else {

        return <- return |>
          dplyr::left_join(get_choices(survey, choices, {{ col }}, label = TRUE),
                           by = c("choices" = "name")) |>
          dplyr::rename(choices_label = label)
      }

    } else {

      return <- return |>
        dplyr::mutate(choices_label = choices)

    }

  } else if (analysis == "prop_simple_overall") {

    return <- design |>
      dplyr::mutate(!!rlang::sym(rlang::ensym(col)) := ifelse(is.na(!!rlang::sym(rlang::ensym(col))), none, !!rlang::sym(rlang::ensym(col)))) |>
      svy_prop({{ col }}, {{ group }}, na_rm = F, stat_name = stat_name, level = level, vartype = vartype) |>
      dplyr::mutate(name = col_name,
                    analysis = analysis) |>
      dplyr::rename(choices = {{ col }})


    if (get_label) {

      got_choices <- get_choices(survey, choices, {{ col }}, label = TRUE)

      if (nrow(got_choices) == 0) {

        return <- return |>
          dplyr::mutate(choices_label = .data$choices)

      } else {

        return <- return |>
          dplyr::left_join(get_choices(survey, choices, {{ col }}, label = TRUE),
                           by = c("choices" = "name")) |>
          dplyr::rename(choices_label = label)
      }

    } else {

      return <- return |>
        dplyr::mutate(choices_label = choices)

    }

    return <- return |>
      dplyr::mutate(choices_label = replace(.data$choices_label, choices == none, ifelse(is.null(none_label), none, none_label)))


  } else if (analysis == "prop_multiple") {

    design_colnames <- colnames(design$variables)

    design <- design |>
      srvyr::drop_na({{ col }})

    choices_conc <- get_choices(survey, choices, {{ col }}, conc = TRUE) |>
      subvec_in(design_colnames)

    if (length(choices_conc) == 0) {
      choices_conc <- design |>
        srvyr::select(dplyr::starts_with(stringr::str_c(col_name, "_"))) |>
        colnames()

      if (length(choices_conc) == 0) rlang::abort(c(
        "Could not get choices with the Kobo sheets and could not guess dummy choices columns to make analysis 'prop_multiple'.",
        "*" = glue::glue("For col: '{col_name}', either: (1) there is an empty list_name in survey, (2) there is no corresponding list_name in choice, and (3) there is no column of the format 'variable_choice' that exists in design."),
        "i" = glue::glue("Please check whether col : '{col_name}' is well-coded in the Kobo sheets or if it is a 'select_multiple' question type.")
      ))

    }

    choices_not_conc <- stringr::str_remove(choices_conc, stringr::str_c(col_name, "_"))

    return <- purrr::map2(
      choices_conc,
      choices_not_conc,
      \(x,y) {
        
        # Check if values are in set c(0, 1, NA)
        are_values_in_set(
          design[["variables"]], 
          !!rlang::as_name(rlang::ensym(x)), c(0, 1, NA))

        count <- svy_count_numeric(design, !!rlang::sym(rlang::ensym(x)), {{ group }}, na_rm = na_rm, stat_name = stat_name, level = level, vartype = vartype) |>
        dplyr::filter(!!rlang::sym(rlang::ensym(x)) == 1) |>
        deselect(!!rlang::sym(rlang::ensym(x))) |>
        dplyr::mutate(name = col_name,
                      analysis = analysis,
                      choices = y)
      }) |>
      dplyr::bind_rows()

    if (get_label) {

      got_choices <- get_choices(survey, choices, {{ col }}, label = TRUE)

      if (nrow(got_choices) == 0) {

        return <- return |>
          dplyr::mutate(choices_label = .data$choices)

      } else {

        return <- return |>
          dplyr::left_join(get_choices(survey, choices, {{ col }}, label = TRUE),
                           by = c("choices" = "name")) |>
          dplyr::rename(choices_label = label)
        }

    } else {

      return <- return |>
        dplyr::mutate(choices_label = choices)

    }


  }  else if (analysis == "prop_multiple_overall") {

    design_colnames <- colnames(design$variables)

    choices_conc <- get_choices(survey, choices, {{ col }}, conc = TRUE) |>
      subvec_in(design_colnames)

    if (length(choices_conc) == 0) {
      choices_conc <- design |>
        srvyr::select(dplyr::starts_with(stringr::str_c(col_name, "_"))) |>
        colnames()

      if (length(choices_conc) == 0) rlang::abort(c(
        "Could not get choices with the Kobo sheets and could not guess dummy choices columns to make analysis 'prop_multiple_overall'.",
        "*" = glue::glue("For col: '{col_name}', either: (1) there is an empty list_name in survey, (2) there is no corresponding list_name in choice, and (3) there is no column of the format 'variable_choice' that exists in design."),
        "i" = glue::glue("Please check whether col : '{col_name}' is well-coded in the Kobo sheets or if it is a 'select_multiple' question type.")
        ))

    }

    choices_not_conc <- stringr::str_remove(choices_conc, stringr::str_c(col_name, "_"))

    return <- purrr::map2(
      choices_conc,
      choices_not_conc,

      \(x,y) {

        design <- srvyr::mutate(design, srvyr::across({{ x }}, \(col) { ifelse(is.na(col), 0, col) }))
        
        # Check if values are in set c(0, 1)
        are_values_in_set(
          design[["variables"]], 
          !!rlang::as_name(rlang::ensym(x)), c(0, 1))

        count <- svy_count_numeric(design, !!rlang::sym(rlang::ensym(x)), {{ group }}, na_rm = na_rm, stat_name = stat_name, level = level, vartype = vartype) |>
        dplyr::filter(!!rlang::sym(rlang::ensym(x)) == 1) |>
        deselect(!!rlang::sym(rlang::ensym(x))) |>
        dplyr::mutate(name = col_name,
                      analysis = analysis,
                      choices = y)
      }) |>
      dplyr::bind_rows()

    if (get_label) {

      got_choices <- get_choices(survey, choices, {{ col }}, label = TRUE)

      if (nrow(got_choices) == 0) {

        return <- return |>
          dplyr::mutate(choices_label = .data$choices)

      } else {

        return <- return |>
          dplyr::left_join(get_choices(survey, choices, {{ col }}, label = TRUE),
                           by = c("choices" = "name")) |>
          dplyr::rename(choices_label = label)
      }

    } else {

      return <- return |>
        dplyr::mutate(choices_label = choices)

    }

  } else if (analysis == "ratio"){

    #-------- Ratio checks

    ratio_cols <- stringr::str_split(col_name, ",", simplify = F) |>
      purrr::flatten_chr() |>
      stringr::str_squish()

    num <- ratio_cols[1]
    denom <- ratio_cols[2]

    #-------- Calculate ratio

    return <- design |>
      svy_ratio(!!rlang::sym(rlang::ensym(num)), !!rlang::sym(rlang::ensym(denom)), {{ group }}, na_rm = na_rm, stat_name = stat_name, level = level, vartype = vartype) |>
      dplyr::mutate(name = col_name,
                    analysis = analysis)


  } else if (analysis %in% c("interact")) {

    # prop overall would be to replace NAs by 0 everywhere, to calculate over all HHs independantly of any SL
    rlang::abort("Function under development")
  } else {
    rlang::abort("Did you mean the right type of analysis?")
  }

  #-------- Add disagg group label

  if (is.null(group)) { return <- return |>
    dplyr::mutate(group_disagg = NA_character_, group_disagg_label = NA_character_) } else
    {
      return <- return |>
        dplyr::rename(group_disagg = {{ group }}) |>
        dplyr::left_join(get_choices(survey, choices, {{ group }}, label = T), by = c("group_disagg" = "name")) |>
        dplyr::rename(group_disagg_label = label)
    }


  #-------- Add grouping variables and levels

  return <- return |>
    dplyr::mutate(group = group_name)

  return(return)

}




#' @title Make analysis from data analysis plan
#'
#' @param design A design object
#' @param survey The survey sheet from Kobo that contains at least column 'list_name' (split from 'type') and 'name'
#' @param choices The choices sheet from Kobo contains at least column 'list_name' (split from 'type') and 'name'
#' @param dap A data analysis plan, typically from an excel sheet
#' @param bind Should it result in a list of a tibble? Default to list.
#'
#' @return A summarized analysis
#'
#' @family functions for survey analysis
#'
#' #' @description
#' `r lifecycle::badge("experimental")`
#'
#' This function still is experimental.
#'
#'
#' @importFrom rlang .data
#'
#' @export
make_analysis_from_dap <- function(
    design,
    survey,
    choices,
    dap,
    bind = FALSE
){

  check_analysis_dap(dap, design, survey, choices)

  mapped <- purrr::pmap(
    dap |>
      dplyr::select("question_name", "analysis", "none_label", "group", "level", "na_rm", "vartype"),
    function(question_name, analysis, none_label, group, level, na_rm, vartype){

      if (na_rm == "TRUE") {na_rm_lgl <- TRUE} else { na_rm_lgl <- FALSE}

      if (is.na(group)) {group_lgl <- NULL} else {group_lgl <- group}

      if (is.na(none_label)) {none_label_lgl <- NULL} else {none_label_lgl <- none_label}

      analysis <- make_analysis(design, survey, choices, {{ question_name }}, {{ analysis }}, none_label_lgl, group_lgl, level, na_rm_lgl, vartype)

      return(analysis)
    }
  ) |>
    purrr::set_names(dap$id_analysis)

  if (bind) {

    if(sum(is.na(dap[["id_analysis"]])) > 0) rlang::abort("There are missing values in column 'id_analysis'. In order to bind results, all values in 'id_analysis' must exist and be unique.")

    if(length(dap[["id_analysis"]]) > length(unique(dap[["id_analysis"]]))) rlang::abort("There are duplicated values in column 'id_analysis'. In order to bind results, all values in 'id_analysis' must exist and be unique.")

    mapped <- mapped |>
      purrr::map(\(x) x |> dplyr::mutate(dplyr::across(dplyr::starts_with("choices"), as.character))) |>
      dplyr::bind_rows(.id = "id_analysis") |>
      dplyr::left_join(dap |> impactR::deselect("analysis", "group"), by = "id_analysis")
  }


  return(mapped)

}




#' @title MSNA severity scores names and colors
#'
#' @return A tibble with three columns: color, level, name of severity scores
#'
#' @export
severity_score_reach <- function(){
  tibble::tibble(color = c("#EE5A59", "#F7ACAC", "#FACDCD", "#A7A9AC", "#58585A"),
                 score = c(5, 4, 3, 2, 1),
                 score_label = c("Extreme + (4+)",
                                 "Extreme (4)",
                                 "Severe (3)",
                                 "Stress (2)",
                                 "Minimal (1)"))
}



