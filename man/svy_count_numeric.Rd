% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/analysis.R
\name{svy_count_numeric}
\alias{svy_count_numeric}
\title{Survey count for numeric variables}
\usage{
svy_count_numeric(
  design,
  col,
  group = NULL,
  na_rm = TRUE,
  stat_name = "count_numeric",
  ...
)
}
\arguments{
\item{design}{A srvyr::design object}

\item{col}{A column to count from}

\item{group}{A quoted or unquoted vector of columns to group by. Default to NULL for no group}

\item{na_rm}{Should NAs from \code{col} be removed? Default to TRUE. na.rm does not work anymore within srvyr functions (workaround for now). It should work for \code{survey_prop}, matter of precaution}

\item{stat_name}{What should the statistic's column be named? Default to "count_numeric"}

\item{...}{Parameters to pass to \code{srvyr::survey_median()}}
}
\value{
A survey-summarized-proportion tibble
}
\description{
Survey count for numeric variables
}
