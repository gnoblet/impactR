% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/monitor.R
\name{outliers_iqr}
\alias{outliers_iqr}
\title{Get outliers using interquartile range (better for skewed-distribution)}
\usage{
outliers_iqr(.tbl, col, times = 1.5, id_col)
}
\arguments{
\item{.tbl}{A tibble}

\item{col}{A numeric columns}

\item{times}{How much to deviate from IQR? Default to 1.5}

\item{id_col}{Usually uuid}
}
\value{
A tibble of outliers
}
\description{
Get outliers using interquartile range (better for skewed-distribution)
}
