% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/analysis.R
\name{svy_interact}
\alias{svy_interact}
\title{Survey interaction means}
\usage{
svy_interact(
  design,
  interact_cols,
  group = NULL,
  arrange = TRUE,
  unnest_interaction = TRUE,
  na_rm = TRUE,
  stat_name = "prop",
  ...
)
}
\arguments{
\item{design}{A srvyr::design object}

\item{interact_cols}{A vector of columns to get interactions from}

\item{group}{A vector of columns to group by. Default to NULL}

\item{unnest_interaction}{Should interaction be unnested? Default to TRUE}

\item{na_rm}{Should NAs from \code{interact_cols} be removed? Default to TRUE}

\item{stat_name}{What should the statistic's column be named? Default to "prop"}

\item{...}{Parameters to pass to srvyr::survey_mean()}
}
\value{
A survey-summarized-interaction tibble
}
\description{
Survey interaction means
}
