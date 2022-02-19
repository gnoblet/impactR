#' #' @title  Generate select_multiples tables
#' #'
#' #' @param .tbl A tibble of data
#' #' @param survey The survey sheet from the Kobo tool; it needs to have a column "type" containing the type and the list names.
#' #' @param aggs Variables to summarize (for now, it needs to be a character vector)
#' #' @param other_group Variable(s) to group by such as administrative levels e.g. It will be displayed as unique columns
#' #' @param type Type of statistics to be displayed. Default to "np" `{n} ({p}%)`. See details for other options
#'
#' #'
#' #' @details The statistics to show are totally up to changes. If you want another one, it is just a matter of writing small functions to be wrapped up in `table_categorical`. For now, the following statistics can be displayed:
#' #' * type = "np": Display the count and the percentage as `{n} ({p}%)`
#' #' = type = "p": Display the percentage only as `{p}%`
#' #' * type = "n": Display the count only as `{n}`
#' #'
#' #' @export
#' map_table_categorical_to_select_multiple <- function(.tbl, survey, disagg, other_group = NULL, type = "np", subset = T, choices = NULL){
#'   select_multiples <- survey |>
#'     split_survey("type") |>
#'     get_multiple()
#'   #
#'   if (rlang::is_true(subset)) {
#'     .tbl <- .tbl |> purrr::reduce(.init = .tbl, .x = select_multiples, .f = ~ tidyr::separate_rows(.x, !!rlang::sym(.y), sep = " "))
#'
#'     tables <- map_table_categorical(
#'       .tbl,
#'       aggs = select_multiples,
#'       disagg = {{ disagg }},
#'       other_group = {{ other_group }},
#'       type = type)
#'   } else if (rlang::is_false(subset)) {
#'     if (rlang::is_null(choices)) {rlang::abort("Please provide the choices sheet from Kobo since you used 'subset = F'")}
#'     multiple_choices <- get_choices(survey, choices)
#'   }
#'
#'
#'
#'   return(tables)
#' }
#'
#' map_table_categorical_to_select_multiple <- function(.tbl, survey, choices, col, disagg, other_group = NULL, type = "np", subset = T, choices = NULL){
#'
#'   if (rlang::is_true(subset)) {
#'
#'     .tbl <- .tbl |>
#'       tidyr::separate_rows({{ col }}, sep = " ")
#'
#'     tables <- table_categorical(
#'       .tbl,
#'       agg = {{ col }},
#'       disagg = {{ disagg }},
#'       other_group = {{ other_group }},
#'       type = type)
#'
#'   } else if (rlang::is_false(subset)) {
#'
#'     if (rlang::is_null(choices)) {rlang::abort("Please provide the choices sheet from Kobo since you used 'subset = F'")}
#'
#'     multiple_choices <- get_choices(survey, choices, {{ col }}, conc = T)
#'
#'     tables <- map_tabl
#'
#'   }
#'
#'
#'
#'   return(tables)
#' }
#'
#'
#'
#' #' @title  Generate select_ones tables
#' #'
#' #' @param .tbl A tibble of data
#' #' @param survey The survey sheet from the Kobo tool; it needs to have a column "type" contains the types of questions such as "select_multiple", "select_one", etc. If not split, one can use `impactR::split_survey()`
#' #' @param aggs Variables to summarize (for now, it needs to be a character vector)
#' #' @param other_group Variable(s) to group by such as administrative levels e.g. It will be displayed as unique columns
#' #' @param type Type of statistics to be displayed. Default to "np" `{n} ({p}%)`. See details for other options
#' #'
#' #' @details The statistics to show are totally up to changes. If you want another one, it is just a matter of writing small functions to be wrapped up in `table_categorical`. For now, the following statistics can be displayed:
#' #' * type = "np": Display the count and the percentage as `{n} ({p}%)`
#' #' = type = "p": Display the percentage only as `{p}%`
#' #' * type = "n": Display the count only as `{n}`
#' #'
#' #' @export
#' map_table_categorical_to_select_ones <- function(.tbl, survey, disagg, other_group = NULL, type = "np"){
#'   select_ones <- survey |>
#'     split_survey("type") |>
#'     get_one()
#'   #
#'   tables <- map_table_categorical(
#'     .tbl,
#'     aggs = select_ones,
#'     disagg = {{ disagg }},
#'     other_group = {{ other_group }},
#'     type = type)
#'
#'   return(tables)
#' }
#'
#'
#'
#' #' @title Generate table for a list of categorical variables
#' #'
#' #' @param .tbl A tibble of data
#' #' @param survey The survey sheet from the Kobo tool; it needs to have a column "type" containing the type and the list names.
#' #' @param aggs Variables to summarize (for now, it needs to be a character vector)
#' #' @param disagg Variable to disaggregate by such as the group of population. It will be displayed as rows
#' #' @param other_group Variable(s) to group by such as administrative levels e.g. It will be displayed as unique columns
#' #' @param type Type of statistics to be displayed. Default to "np" `{n} ({p}%)`. See details for other options
#' #'
#' #' @details The statistics to show are totally up to changes. If you want another one, it is just a matter of writing small functions to be wrapped up in `table_categorical`. For now, the following statistics can be displayed:
#' #' * type = "np": Display the count and the percentage as `{n} ({p}%)`
#' #' = type = "p": Display the percentage only as `{p}%`
#' #' * type = "n": Display the count only as `{n}`
#' #'
#' #' @export
#' map_table_categorical <- function(.tbl, aggs, disagg, other_group = NULL, type = "np"){
#'
#'   #aggs <- purrr::map_chr(rlang::enquos(aggs), rlang::as_name)
#'   tables <- purrr::map(aggs, ~ table_categorical(.tbl, {{ .x }}  , {{ disagg }}, {{ other_group }}, type = type))
#'
#'   return(tables)
#' }
#'
#'
#' #' @title Generate categorical variable's summary table
#' #'
#' #' @param .tbl A tibble of data
#' #' @param agg Variable to summarize
#' #' @param disagg Variable to disaggregate by such as the group of population. It will be displayed as rows
#' #' @param other_group Variable(s) to group by such as administrative levels e.g. It will be displayed as unique columns
#' #' @param type Type of statistics to be displayed. Default to "a" `({mean} ({sd}))`. See details for other options.
#' #'
#' #' @details The statistics to show are totally up to changes. If you want another one, it is just a matter of writing small functions to be wrapped up in `table_categorical`. For now, the following statistics can be displayed:
#' #' * type = "np": Display the count and the percentage as `{n} ({p}%)`
#' #' = type = "p": Display the percentage only as `{p}%`
#' #' * type = "n": Display the count only as `{n}`
#' #'
#' #' @export
#' table_categorical <- function(.tbl, agg, disagg, other_group = NULL, type = "np"){
#'
#'   #---------- Checks
#'   agg_name <- rlang::as_name(rlang::enquo(agg))
#'   if_not_in_stop(.tbl, agg_name, ".tbl", "agg")
#'
#'   #---------- Process
#'   if (type == "np") {
#'     stat <- "{n} ({p}%)"
#'   } else if (type == "p") {
#'     stat <- "{p}%"
#'   } else if (type == "n") {
#'     stat <- "{n}"
#'   }
#'
#'   if(rlang::quo_is_null(rlang::enquo(other_group))){
#'     table <- gtsummary::tbl_cross(
#'       .tbl,
#'       row = {{ disagg }},
#'       col = {{ agg }},
#'       statistic = stat,
#'       missing_text = "NA",
#'       percent = "row"
#'     ) |>
#'       gtsummary::modify_caption(paste0(agg_name, " vs. ", disagg_name)) |>
#'       gtsummary::modify_header(label = "*Variable*")
#'   } else {
#'     table <- gtsummary::tbl_strata2(
#'       .tbl,
#'       strata = {{ other_group }},
#'       .tbl_fun = ~ .x |>  gtsummary::tbl_cross(
#'         row = {{ disagg }},
#'         col = {{ agg }},
#'         statistic = stat,
#'         missing_text = "NA",
#'         percent = "row")) |>
#'       gtsummary::modify_caption(paste0(agg_name, " vs. ", disagg_name, " by ", other_group_name)) |>
#'       gtsummary::modify_header(label = "*Variable*")
#'   }
#'
#'
#'
#'   return(table)
#' }
#'
#'
#' #' @title Generate table for a list of continuous variable
#' #'
#' #' @param .tbl A tibble of data
#' #' @param survey The survey sheet from the Kobo tool; it needs to have a column "type" containing the type and the list names.
#' #' @param aggs Variables to summarize (for now, it needs to be a character vector)
#' #' @param disagg Variable to disaggregate by such as the group of population. It will be displayed as rows
#' #' @param other_group Variable(s) to group by such as administrative levels e.g. It will be displayed as unique columns
#' #' @param type Type of statistics to be displayed. Default to "np" `{n} ({p}%)`. See details for other options
#' #'
#' #' @details The statistics to show are totally up to changes. If you want another one, it is just a matter of writing small functions to be wrapped up in `table::continuous`. For now, the following statistics can be displayed:
#' #' * type = "a": Display the average/mean and the standard deviation as `({mean} ({sd}))`
#' #' = type = "m": Display the median and the interquartile range as `{median} ({p25}, {p75})`
#' #' * type = "ao": Display the mean compared to the overall mean (difference level) as `{mean} (level}, diff: {diff})`. The overall mean is the group mean if other_group is provided
#' #' * type = "am" : Display the mean as a percentage for dummy variable such as multiple choices columns,  as `{percent}%`. The overall mean is the group mean if other_group is provided
#' #'
#' #' @export
#' map_table_continuous <- function(.tbl, aggs, disagg, other_group = NULL, type = "np"){
#'
#'   #aggs <- purrr::map_chr(rlang::enquos(aggs), rlang::as_name)
#'   tables <- purrr::map(aggs, ~ table_continuous(.tbl, {{ .x }}  , {{ disagg }}, {{ other_group }}, type = type))
#'
#'   return(tables)
#' }
#'
#'
#'
#'
#' #' @title Generate continuous variable's summary table
#' #'
#' #' @param .tbl A tibble of data
#' #' @param agg Variable to summarize
#' #' @param disagg Variable to disaggregate by such as the group of population. It will be displayed as rows
#' #' @param other_group Variable(s) to group by such as administrative levels e.g. It will be displayed as unique columns
#' #' @param type Type of statistics to be displayed. Default to "a" `({mean} ({sd}))`. See details for other options.
#' #'
#' #' @details The statistics to show are totally up to changes. If you want another one, it is just a matter of writing small functions to be wrapped up in `table::continuous`. For now, the following statistics can be displayed:
#' #' * type = "a": Display the average/mean and the standard deviation as `({mean} ({sd}))`
#' #' = type = "m": Display the median and the interquartile range as `{median} ({p25}, {p75})`
#' #' * type = "ao": Display the mean compared to the overall mean (difference level) as `{mean} (level}, diff: {diff})`. The overall mean is the group mean if other_group is provided
#' #' * type = "am" : Display the mean as a percentage for dummy variable such as multiple choices columns,  as `{percent}%`. The overall mean is the group mean if other_group is provided
#' #'
#' #' @export
#' table_continuous <- function(.tbl, agg, disagg, other_group = NULL, type = "a") {
#'
#'   agg_name <- rlang::as_name(rlang::enquo(agg))
#'   if_not_in_stop(.tbl, agg_name, ".tbl", "agg")
#'
#'   disagg_name <- rlang::as_name(rlang::enquo(disagg))
#'   if_not_in_stop(.tbl, disagg_name, ".tbl", "disagg")
#'
#'   if (!rlang::quo_is_null(rlang::enquo(other_group))) {
#'     other_group_name <- rlang::as_name(rlang::enquo(other_group))
#'     if_not_in_stop(.tbl, other_group_name, ".tbl", "other_group")
#'   }
#'
#'   if (type == "a") {
#'     stat <- "{mean} ({sd})"
#'   } else if (type == "m") {
#'     stat <- "{median} ({p25}, {p75})"
#'   } else if (type == "ao"){
#'     stat_fun <- function(data, full_data, type = "continuous", ...) {
#'       mean <- mean(data |> dplyr::pull({{ agg }}), na.rm = TRUE)
#'       great_mean <- mean(full_data |> dplyr::pull({{ agg }}), na.rm = TRUE)
#'       diff <- mean - great_mean
#'       dplyr::tibble(
#'         `mean` = mean,
#'         `great_mean` = great_mean,
#'         `diff` = diff,
#'         `level` = ifelse(diff > 0, "high", "low")
#'       )
#'     }
#'     stat <- "{mean} (level}, diff: {diff})"
#'   } else if (type == "am") {
#'     stat_fun <- function(data, full_data, type = "continuous", ...) {
#'       percent <- mean(data |> dplyr::pull({{ agg }}), na.rm = TRUE)
#'       n_data <- nrow(data)
#'       great_percent <- mean(full_data |> dplyr::pull({{ agg }}), na.rm = TRUE) * 100
#'       n_full_data <- nrow(data)
#'       dplyr::tibble(
#'         `p` = percent,
#'         `n_data` = n_data,
#'         `great_percent` = great_percent,
#'         `n_full_data` = n_full_data
#'       )
#'     }
#'     stat <- "{p}%"
#'   }
#'
#'   if(rlang::quo_is_null(rlang::enquo(other_group))){
#'     if(type %in% c("a", "m")) {
#'       table <- gtsummary::tbl_continuous(
#'         .tbl,
#'         variable = {{ agg }},
#'         include = {{ disagg }},
#'         statistic = everything() ~ stat,
#'         digits = everything() ~ 1)
#'     } else if (type %in% c("ao", "am")) {
#'       table <- gtsummary::tbl_custom_summary(
#'         .tbl,
#'         stat_fns = ~ stat_fun,
#'         statistic = ~ stat,
#'         include = c({{ disagg }})
#'       )
#'     }
#'     table <- table |>
#'       gtsummary::modify_caption(paste0(agg_name, " vs. ", disagg_name)) |>
#'       gtsummary::modify_header(label = "*Variable*")
#'   } else {
#'     if(type %in% c("a", "m")) {
#'       table <- gtsummary::tbl_strata2(
#'         .tbl,
#'         strata = {{ other_group }},
#'         .tbl_fun = ~ .x |> gtsummary::tbl_continuous(
#'           variable = {{ agg }},
#'           include = {{ disagg }},
#'           statistic = everything() ~ stat,
#'           digits = everything() ~ 1)
#'       )
#'     } else if (type %in% c("ao", "am")) {
#'       table <- gtsummary::tbl_strata2(
#'         .tbl,
#'         strata = {{ other_group }},
#'         .tbl_fun = ~ .x |>  gtsummary::tbl_custom_summary(
#'           stat_fns = ~ stat_fun,
#'           statistic = ~ stat,
#'           include = c({{ disagg }})))
#'       # |>
#'       #     gtsummary::modify_footnote(update = gtsummary::all_stat_cols() ~ glue::glue("{agg_name}: {stat}"))
#'     }
#'     table <- table |>
#'       gtsummary::modify_caption(paste0(agg_name, " vs. ", disagg_name, " by ", other_group_name)) |>
#'       gtsummary::modify_header(label = "*Variable*")
#'   }
#'
#'   return(table)
#' }




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

