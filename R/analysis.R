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

  if_not_in_stop(.tbl, !!!cols, ".tbl", "cols")

  cols <- rlang::enquos(..., names = T)
  if(!all(is.numeric(cols))) abort_bad_argument("All cols in ...", "be numeric")


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



#' @title Survey prop
#'
#' @param design A srvyr::design object
#' @param col A column to calculate proportion from
#' @param group A quoted or unquoted vector of columns to group by. Default to NULL for no group.
#' @param ... Parameters to pass to `srvyr::survey_prop()`
#'
#' @return A survey-summarized-proportion tibble
#'
#' @export
svy_prop <- function(design, col, group = NULL, ...){
  to_return <- design |>
      srvyr::group_by(dplyr::across({{ group }}), dplyr::across({{ col }})) |>
    srvyr::summarize(prop = srvyr::survey_prop(...)) |>
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
