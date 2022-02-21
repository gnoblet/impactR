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
