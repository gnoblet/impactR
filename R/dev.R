#'
#' #' @title Download form (survey and choices)
#' #'
#' #' @param long_formid The long form id as a character string
#' #' @param user User name
#' #' @param pwd Password
#' #' @param api Link to api. Default to "https://kobo.humanitarianresponse.info"
#' #'
#' #' @details Heavily adapted from and many thanks to @ElliotMesss at https://github.com/ElliottMess/koboAPI/t
#' #'
#' #' @return A list of two tibbles survey and choices
#' #'
#' #' @export
#' dl_form <- function(long_formid, user, pwd, api = "https://kobo.humanitarianresponse.info") {
#'   raw_form_text_json <- paste0(api, "/assets/", long_formid, "/") |>
#'     httr::GET(httr::authenticate(user, pwd), httr::progress()) |>
#'     httr::content("text", encoding = "UTF-8") |>
#'     jsonlite::fromJSON()
#'
#'   # languages <- as.vector(raw_form_text_json$content$translations)
#'   # languages_labels <- paste0("label::", languages)
#'
#'
#'   choices <- raw_form_text_json$content$choices |>
#'     tibble::as_tibble() |>
#'     janitor::clean_names()
#'   survey <-  raw_form_text_json$content$survey |>
#'     tibble::as_tibble() |>
#'     janitor::clean_names()
#'
#'   form <- list("survey" = survey, "choices" = choices)
#'   return(form)
#' }
