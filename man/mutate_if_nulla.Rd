% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_df.R
\name{mutate_if_nulla}
\alias{mutate_if_nulla}
\title{Mutate with replacement if NULL or NA values}
\usage{
mutate_if_nulla(.tbl, col, replacement)
}
\arguments{
\item{.tbl}{A data.frame.}

\item{col}{A column from .tbl.}

\item{replacement}{A replacement of the same type as col.}
}
\value{
A mutated tibble.
}
\description{
Mutate with replacement if NULL or NA values
}
