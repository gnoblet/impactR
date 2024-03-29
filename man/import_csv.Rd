% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import.R
\name{import_csv}
\alias{import_csv}
\alias{import_xlsx}
\alias{import_full_xlsx}
\title{Initial import with clean names (not an option!)}
\usage{
import_csv(path, delim = ",", uuid = NULL, ...)

import_xlsx(path, sheet = 1, uuid = NULL, ...)

import_full_xlsx(path = NULL, uuid = NULL)
}
\arguments{
\item{path}{The project relative path to the csv file}

\item{delim}{Field separator, default is ","}

\item{uuid}{Character string. Should we rename "submission_uuid" to "uuid"}

\item{...}{Parameters to pass to readxl::read_xlsx or readr::read_delim}

\item{sheet}{A character string of the sheet name or the sheet position}
}
\value{
A clean-names-and-types tibble
}
\description{
Import a dataset from a CSV or a XLSX file. It ensures that the right type of column is guessed using the max value of guess_max
}
\section{Functions}{
\itemize{
\item \code{import_csv()}: Import a .csv file

\item \code{import_xlsx()}: Import one sheet from a .xlsx

\item \code{import_full_xlsx()}: Import all sheets from a .xlsx

}}
