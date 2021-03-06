% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_file.R
\name{reti_read_FW}
\alias{reti_read_FW}
\alias{get_FWdata}
\title{Making farm and woods data from reti csvfile}
\usage{
reti_read_FW(path, timecount = FALSE)

get_FWdata(path, timecount = FALSE)
}
\arguments{
\item{path}{Vector of csvfile's path.}

\item{timecount}{A logical scalar. you can see how much time did this
function take to finish the work.}
}
\value{
A tibble data.
}
\description{
This function Reads csvfiles for real estate transaction price data,
which are provied by Ministry of Land, Infrastructure and Transport (MLIT),
and  makes farm and woods data.
}
