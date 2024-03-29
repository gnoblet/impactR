% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_kobo.R
\name{get_dictionary}
\alias{get_dictionary}
\title{Get dictionary from survey sheet}
\usage{
get_dictionary(data, survey, name_as_label = FALSE)
}
\arguments{
\item{data}{Some Kobo data.}

\item{survey}{Some survey sheet, with a split 'type' column, e.g. with \code{split_survey(type)}. It must have columns 'list_name' and 'name'.}

\item{name_as_label}{Default to TRUE. Should the variable name be used as the label if label is missing?}
}
\value{
A dictionary
}
\description{
Get dictionary from survey sheet
}
