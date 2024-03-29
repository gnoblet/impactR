% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_df.R
\name{diff_tibbles}
\alias{diff_tibbles}
\title{Remove cols from a tibble that exists in another tibble}
\usage{
diff_tibbles(tibble_a, tibble_b, ...)
}
\arguments{
\item{tibble_a}{A tibble to remove columns from}

\item{tibble_b}{A tibble to extract columns names from}

\item{...}{Columns to keep in tibble.}
}
\value{
A tibble with some columns removed
}
\description{
Remove cols from a tibble that exists in another tibble
}
