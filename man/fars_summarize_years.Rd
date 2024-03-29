% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_summarize_years}
\alias{fars_summarize_years}
\title{Number of entries by month and year
This function counts the number of fatal injuries by month and year}
\usage{
fars_summarize_years(years)
}
\arguments{
\item{years}{a list or vector with the years to summarize}
}
\value{
a dataframe containing the number of fatal injuries per month and year
}
\description{
Number of entries by month and year
This function counts the number of fatal injuries by month and year
}
\examples{
\dontrun{
fars_summarize_years(c(2013, 2014, 2015))
}
}
\seealso{
\code{\link{fars_read_years}}
}
