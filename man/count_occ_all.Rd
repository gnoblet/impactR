% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clean.R
\name{count_occ_all}
\alias{count_occ_all}
\title{Count all occurences}
\usage{
count_occ_all(.tbl, survey, choices, id_col, output = "updated")
}
\arguments{
\item{.tbl}{A tibble}

\item{survey}{A survey sheet from Kobo (with column "type" split)}

\item{choices}{A choices sheet from Kobo}

\item{id_col}{Usually uuid... to count}

\item{output}{Either "count" (a list of counts of select_multiple) or "updated" (the updated .tbl).}
}
\value{
An updated tibble or a list of occurences
}
\description{
Count all occurences
}
