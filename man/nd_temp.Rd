% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_calculations.R
\name{nd_temp}
\alias{nd_temp}
\title{Number of days related to temperature (percentile, temperature)}
\usage{
nd_temp(data)
}
\arguments{
\item{data}{data frame or tibble as downloaded with \code{\link[weathercan]{weather_dl}}. Provide a single measurement site for a single year only.}
}
\value{
a named list of:
ND_25 = number of days with t>25 C
ND_30 = number of days with t>30
NDT_Min_90p = number of days with minimum temperature > 90th percentile
NDT_Max_90p = number of days with maximum temperature > 90th percentile
NDT_Min_10p = number of days with minimum temperature < 10th percentile
NDT_Max_10p = number of days with maximum temperature > 10th percentile
}
\description{
Number of days related to temperature (percentile, temperature)
}
