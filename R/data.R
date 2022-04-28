#' Some data produced from a Kobo tool
#'
#' A dataset (in French) containing data for consensus, admin, enumerator info, chief of hh, and sanitary
#'
#' @format A data frame with 8 rows and 36 variables:
#' \describe{
#'   \item{start}{The starting time of the survey}
#'   \item{end}{The end time of the survey}
#'   ...
#' }
"data"

#' Some survey sheet from a Kobo tool
#'
#' A dataset (in French) containing a basic survey sheet with consensus, admin, enumerator info, chief of hh, and sanitary
#'
#' @format A data frame with 43 rows and 12 variables:
#' \describe{
#'   \item{type}{The type of the question (to split)}
#'   \item{name}{The name of the question}
#'   ...
#' }
"survey"


#' Some choices sheet from a Kobo tool
#'
#' A dataset (in French) containing a basic survey sheet with consensus, admin, enumerator info, chief of hh, and sanitary
#'
#' @format A data frame with 55 rows and 7 variables:
#' \describe{
#'   \item{list_name}{The list name}
#'   \item{name}{Tne choices's names}
#'   ...
#' }
"choices"


#' Some table of checks
#'
#' A dataset (in French) containing a list of checks to pass to `make_log`
#'
#' @format A data frame with a few rows and variables:
#' \describe{
#'   \item{id_check}{The check id}
#'   \item{question_name}{Tne Kobo survey question name}
#'   ...
#' }
"check_list"


#' Some random dap test
#'
#' A dataset (in French) containing a basic dap, just to test. Very experimental
#'
#' @format A data frame with 5 rows and 13 variables:
#' \describe{
#'   \item{question_name}{The question_name or the ratio}
#'   \item{analysis}{Tne R type of analysis to pass to `make_analysis`}
#'   ...
#' }
"dap"



#' Some random cleaning log
#'
#' A dataset (in French) containing a cleaning_log. Columns are necessary and comes from the `make_log` function
#'
#' @format A data frame with 17 rows and 17 variables:
#' \describe{
#'   \item{id_check}{The check id}
#'   \item{name}{Tne choices's names}
#'   ...
"cleaning_log"
