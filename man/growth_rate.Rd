% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/growth_rate.R
\name{growth_rate}
\alias{growth_rate}
\alias{growth_rate.default}
\alias{growth_rate.data.frame}
\title{Estimate growth rates}
\usage{
growth_rate(x, ...)

\method{growth_rate}{default}(x, ...)

\method{growth_rate}{data.frame}(x, n, time, by = NULL, ...)
}
\arguments{
\item{x}{Numeric vector containing counts or related metric to estimate growth rates.
It can also be a data frame containing \code{n} and \code{time} variables (for data frames only).}

\item{...}{Further arguments passed to other methods.}

\item{n}{Variable name of count data. Quoted or unquoted.}

\item{time}{Variable name of the date variable (any format that is sortable). Quoted or unquoted.}

\item{by}{Grouping variable}
}
\value{
When input value is a vector, then a vector of the same length that input parameter
is returned. When input value is a data.frame, tibble or data.table, a data.table is returned.
}
\description{
Estimate growth rates
}
\details{
More info about this growth rate equation can be seen next \link{...}
}
\note{
For growth rate estimation, the data is assumed to be sorted for vectors, for data.frames
data is sorted internally.
}
