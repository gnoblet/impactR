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
