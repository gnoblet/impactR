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
rowwise_optimum <- function(.tbl, ...,  optimum = "max", max_name = "pmax", min_name = "pmin", na_rm = T, keep = "all") {

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
#' @param ... Parameters to pass to `srvyr::survey_prop()`
#'
#' @return A survey-summarized-proportion tibble
#'
#' @export
svy_prop <- function(design, col, group = NULL, na_rm = T, ...){

  if (rlang::is_true(na_rm)) {
    design <- design |>
      srvyr::drop_na({{ col }})
  }

  design <- design |>
    srvyr::group_by(dplyr::across({{ group }}), dplyr::across({{ col }})) |>
    srvyr::summarize(prop = srvyr::survey_prop(...)) |>
    srvyr::ungroup()

  return(to_return)
}


#' @title Survey mean
#'
#' @param design A srvyr::design object
#' @param col A column to calculate mean from
#' @param group A quoted or unquoted vector of columns to group by. Default to NULL for no group.
#' @param na_rm Should NAs from `col` be removed? Default to TRUE. na.rm does not work anymore within srvyr functions (workaround for now). It should work for `survey_mean`, matter of precaution
#'
#' @param ... Parameters to pass to `srvyr::survey_mean()`
#'
#' @return A survey-summarized-proportion tibble
#'
#' @export
svy_mean <- function(design, col, group = NULL, ...){

  if (rlang::is_true(na_rm)) {
    design <- design |>
      srvyr::drop_na({{ col }})
  }

  design <- design |>
    srvyr::group_by(dplyr::across({{ group }}), dplyr::across({{ col }})) |>
    srvyr::summarize(mean = srvyr::survey_mean(...)) |>
    srvyr::ungroup()

  return(to_return)
}



#' @title Survey median
#'
#' @param design A srvyr::design object
#' @param col A column to calculate median from
#' @param group A quoted or unquoted vector of columns to group by. Default to NULL for no group.
#' @param na_rm Should NAs from `col` be removed? Default to TRUE. na.rm does not work anymore within srvyr functions (workaround for now). It should work for `survey_median`, matter of precaution
#' @param ... Parameters to pass to `srvyr::survey_median()`
#'
#' @return A survey-summarized-proportion tibble
#'
#' @export
svy_median <- function(design, col, group = NULL, ...){

  if (rlang::is_true(na_rm)) {
    design <- design |>
      srvyr::drop_na({{ col }})
  }

  design <- design |>
    srvyr::group_by(dplyr::across({{ group }}), dplyr::across({{ col }})) |>
    srvyr::summarize(median = srvyr::survey_median(...)) |>
    srvyr::ungroup()

  return(to_return)
}




#' @title Survey interaction means
#'
#' @param design A srvyr::design object
#' @param interact_cols A vector of columns to get interactions from
#' @param group A vector of columns to group by. This is the default.
#' @param unnest_interaction Should interaction be unnested? Default to TRUE.
#' @param ... Parameters to pass to srvyr::survey_mean()
#'
#' @return A survey-summarized-interaction tibble
#'
#' @export
svy_interact <- function(design, interact_cols, group = NULL, unnest_interaction = T, ...){
  to_return <- design |>
    srvyr::group_by(srvyr::across({{ group }}),
                    srvyr::interact(interaction = srvyr::across({{ interact_cols }}))) |>
    srvyr::summarize(prop = srvyr::survey_mean(...)) |>
    dplyr::arrange(dplyr::desc(.data$prop)) |>
    srvyr::ungroup()

  if (rlang::is_true(unnest_interaction)){
    to_return <- to_return |> tidyr::unnest(.data$interaction)
  }

  return(to_return)
}



#' @title Survey ratio
#'
#' @param design A srvyr::design object
#' @param num The numerator column
#' @param denom The denominator column
#' @param group A quoted or unquoted vector of columns to group by. Default to NULL for no group.
#' @param na_rm Should NAs from `num` and `denom` be removed? Default to TRUE. na.rm does not work anymore within srvyr functions (workaround for now). It should work for `survey_mean`, matter of precaution
#' @param ... Parameters to pass to srvyr::survey_mean()
#'
#' @return A survey-summarized-interaction tibble
#'
#' @export
svy_ratio <- function(design, num, denom, group){

  if (rlang::is_true(na_rm)) {
    design <- design |>
      srvyr::drop_na({{ num }}) |>
      srvyr::drop_na({{ denom }})
  }

  design <- design |>
    srvyr::group_by(dplyr::across({{ group }})) |>
    srvyr::summarize(ration = srvyr::survey_ratio({{ enum }}, {{ denom }}...)) |>
    srvyr::ungroup()

  return(to_return)
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





#' #' @title Calculate sums, averages, medians per group
#' #'
#' #' @param .tbl A tibble
#' #' @param agg Variable to aggregate
#' #' @param group Variables to aggregate by (e.g. group of population or administrative areas). Default to NULL
#' #' @param type Default to 'a'. What type of aggregation? a for average, s for sum, m for median
#' #'
#' #' @return A cross-table of frequencies
#' #'
#' #' @export
#' aggregate <- function(.tbl, agg, group = NULL, type = "a") {
#'
#'   #---------- Checks
#'
#'   agg_name <- rlang::as_name(rlang::enquo(agg))
#'   if_not_in_stop(.tbl, agg_name, ".tbl", "agg")
#'
#'   agg_p <- .tbl |> dplyr::pull({{ agg }})
#'   if (!(rlang::is_double(agg_p))) {
#'     abort_bad_argument("agg", "be double", agg_p)
#'   }
#'
#'   if (!(type %in% c("a", "m", "s"))) {
#'     rlang::abort(c(
#'       "`type` must be one of `a`, `m`, or `s`",
#'       "i" = "a` for average, `m` for median, `s` for sum"))
#'   }
#'
#'   #---------- Body
#'
#'   agg_f <- function(type){
#'     if (type == "a") {
#'       f <- function(x) {mean(x, na.rm = T)}
#'     } else if (type == "m") {
#'       f <- function(x) {median(x, na.rm = T)}
#'     } else if (type == "s") {
#'       f <- function(x) {sum(x, na.rm = T)}
#'     }
#'     return(f)
#'   }
#'
#'   agg <- .tbl |>
#'     dplyr::group_by(dplyr::across({{ group }})) |>
#'     dplyr::summarize({{ agg }} := agg_f(type)({{ agg }}), .groups = "drop_last") |>
#'     dplyr::ungroup()
#'
#'   return(agg)
#' }
#'
#'
#' #' @title Calculate aggregate and expose gt table
#' #'
#' #' @param .tbl A tibble
#' #' @param col Variable to pivot to columns
#' #' @param row Variable to rows
#' #' @param group Variable to group_by. Default to NULL
#' #' @param type Default to 'a'. What type of aggregation? a for average, s for sum, m for median
#' #'
#' #' @return A gt table of cross-frequencies
#' #'
#' #' @importFrom rlang :=
#' #'
#' #' @export
#' aggregate_gt_table <- function(.tbl, agg, group = NULL, type = "a"){
#'
#'   agg_name <- rlang::as_name(rlang::enquo(agg))
#'
#'   cnt <- aggregate(.tbl, {{ agg }}, {{ group }}, type = type)  |>
#'     dplyr::group_by(dplyr::across({{ group }}))
#'
#'
#'   # cols_col <- cnt |>
#'   #   dplyr::select(-dplyr::all_of({{ group }}))|>
#'   #   colnames()
#'
#'   gt_tbl <- gt::gt(cnt) #|>
#'     # gt::cols_label(
#'     #   {{ agg }} := ""
#'     # ) |>
#'     # # gt::tab_header(
#'     # #   title = ifelse(
#'     # #     rlang::quo_is_null(rlang::enquo(group)),
#'     # #     paste(row_name, "vs.", col_name, sep = " "),
#'     # #     paste(row_name, "vs.", col_name, "by", group_name, sep = " ")
#'     # #   # )) |>
#'     # gt::tab_spanner(
#'     #   label = agg_name,
#'     #   columns = {{ agg }}
#'    # ) |>
#'     # gt::tab_spanner(
#'     #   label = row_name,
#'     #   columns = row_name
#'     # )
#'
#'   return(gt_tbl)
#' }
#'
#'
#' @title Calculate cross-frequencies and expose gt table
#'
#' @param .tbl A tibble
#' @param col Variable to pivot to columns
#' @param row Variable to rows
#' @param group_col Variable to group_by. Default to NULL
#' @param output Default to 1. What should be the output? 1 for frequencies only, 2 for counts only, 3 for both.
#'
#' @return A gt table of cross-frequencies
#'
#' @importFrom rlang :=
#'
#' #' @export
#' cross_freq_gt_table <- function(.tbl, row, col, group_col = NULL, output = 1){
#'
#'   row_name <- rlang::as_name(rlang::enquo(row))
#'   col_name <- rlang::as_name(rlang::enquo(col))
#'
#'   if (!rlang::quo_is_null(rlang::enquo(group_col))) {
#'     group_col_name <- rlang::as_name(rlang::enquo(group_col))
#'     if_not_in_stop(.tbl, group_col_name, ".tbl", "group_col")
#'   }
#'
#'   cnt <- cross_freq(.tbl, {{ row }}, {{ col }}, {{ group_col }}, output = output) |>
#'     dplyr::group_by({{ group_col }})
#'
#'
#'   cols_col <- cnt |>
#'     dplyr::select(-dplyr::all_of(row_name)) |>
#'     colnames()
#'
#'   gt_tbl <- gt::gt(cnt) |>
#'     gt::cols_label(
#'       {{ row }} := ""
#'     ) |>
#'     gt::tab_header(
#'       title = ifelse(
#'         rlang::quo_is_null(rlang::enquo(group_col)),
#'         paste(row_name, "vs.", col_name, sep = " "),
#'         paste(row_name, "vs.", col_name, "by", group_col_name, sep = " ")
#'       )) |>
#'     gt::tab_spanner(
#'       label = col_name,
#'       columns = cols_col
#'     ) |>
#'     gt::tab_spanner(
#'       label = row_name,
#'       columns = row_name
#'     )
#'
#'   return(gt_tbl)
#' }
#'
#'
#'
#'
#' #' @title Calculate cross-frequencies and expose gt table
#' #'
#' #' @param .tbl A tibble
#' #' @param col Variable to pivot to columns
#' #' @param row Variable to rows
#' #' @param group_col Variable to group_by. Default to NULL
#' #' @param output Default to 1. What should be the output? 1 for frequencies only, 2 for counts only, 3 for both.
#' #'
#' #' @return A gt table of cross-frequencies
#' #'
#' #' @importFrom rlang :=
#' #'
#' #' @export
#' cross_freq_gt_table <- function(.tbl, row, col, group_col = NULL, output = 1){
#'
#'   row_name <- rlang::as_name(rlang::enquo(row))
#'   col_name <- rlang::as_name(rlang::enquo(col))
#'
#'   if (!rlang::quo_is_null(rlang::enquo(group_col))) {
#'     group_col_name <- rlang::as_name(rlang::enquo(group_col))
#'     if_not_in_stop(.tbl, group_col_name, ".tbl", "group_col")
#'   }
#'
#'   cnt <- cross_freq(.tbl, {{ row }}, {{ col }}, {{ group_col }}, output = output) |>
#'     dplyr::group_by({{ group_col }})
#'
#'
#'   cols_col <- cnt |>
#'     dplyr::select(-dplyr::all_of(row_name)) |>
#'     colnames()
#'
#'   gt_tbl <- gt::gt(cnt) |>
#'     gt::cols_label(
#'       {{ row }} := ""
#'     ) |>
#'     gt::tab_header(
#'       title = ifelse(
#'         rlang::quo_is_null(rlang::enquo(group_col)),
#'         paste(row_name, "vs.", col_name, sep = " "),
#'         paste(row_name, "vs.", col_name, "by", group_col_name, sep = " ")
#'       )) |>
#'     gt::tab_spanner(
#'       label = col_name,
#'       columns = cols_col
#'     ) |>
#'     gt::tab_spanner(
#'       label = row_name,
#'       columns = row_name
#'     )
#'
#'   return(gt_tbl)
#' }
#'
#'
#'
#' @title Calculate cross-frequencies
#'
#' @param .tbl A tibble
#' @param col Variable to pivot to columns
#' @param row Variable to rows
#' @param group_col Variable to group_by. Default to NULL
#' @param output Default to 1. What should be the output? 1 for frequencies only, 2 for counts only, 3 for both.
#'
#' @return A cross-table of frequencies
#' #'
#' #' @export
#' cross_freq <- function(.tbl, row, col, group = NULL, output = 1) {
#'
#'   #----------- Checks
#'
#'   if (!rlang::quo_is_null(rlang::enquo(group))) {
#'     group_name <- rlang::as_name(rlang::enquo(group))
#'     if_not_in_stop(.tbl, group_name, ".tbl", "group")
#'   }
#'
#'   row_name <- rlang::as_name(rlang::enquo(row))
#'   col_name <- rlang::as_name(rlang::enquo(col))
#'   if_not_in_stop(.tbl, row_name, ".tbl", "row")
#'   if_not_in_stop(.tbl, col_name, ".tbl", "col")
#'
#'
#'   if (!(output %in% c(1, 2, 3))) {
#'     rlang::abort(c(
#'       "`output` must be one of 1, 2, or 3",
#'       "i" = "1 for frequencies only, 2 for counts only, 3 for both"))
#'   }
#'
#'   #---------- Body
#'
#'   count <- if (rlang::quo_is_null(rlang::enquo(group))) {
#'     .tbl |>
#'       dplyr::group_by({{ row }}, {{ col }}) |>
#'       dplyr::summarize(`n` := dplyr::n(), .groups = "drop_last") |>
#'       dplyr::mutate(`freq` := round(.data$n / sum(.data$n), 3), .keep = "all")
#'   } else {
#'     .tbl |>
#'       named_group_split({{ group }}) |>
#'       purrr::map(
#'         ~ .x |>
#'           dplyr::group_by({{ row }}, {{ col }}) |>
#'           dplyr::summarize(`n` := dplyr::n(), .groups = "drop_last") |>
#'           dplyr::mutate(`freq` :=  round(.data$n / sum(.data$n), 3), .keep = "all")
#'       ) |>
#'       dplyr::bind_rows(.id = group_name)
#'   }
#'
#'   if (output == 1){
#'     final <- count |>
#'       dplyr::select(- .data$n) |>
#'       tidyr::pivot_wider(names_from = {{col}}, values_from = "freq")
#'   } else if (output == 2){
#'     final <- count |>
#'       dplyr::select(- .data$freq) |>
#'       tidyr::pivot_wider(names_from = {{col}}, values_from = "n")
#'   } else if (output == 3){
#'     final <- count |>
#'       dplyr::mutate(conc = paste0(.data$n, " (", scales::percent(.data$freq), ")")) |>
#'       dplyr::select(- c(.data$freq, .data$n)) |>
#'       tidyr::pivot_wider(names_from = {{col}}, values_from = "conc")
#'   }
#'
#'   final <- final |>
#'     dplyr::ungroup()
#'
#'   return(final)
#' }
#'

