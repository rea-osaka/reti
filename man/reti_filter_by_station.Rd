% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/choose_func.R
\name{reti_filter_by_station}
\alias{reti_filter_by_station}
\title{filter data by favorite station}
\usage{
reti_filter_by_station(df, stations)
}
\arguments{
\item{df}{A reti data}

\item{stations}{vector of favorite station names}
}
\description{
This function filters data by station names. This matching is
prefix search. And each vector elements is OR condision.
}
