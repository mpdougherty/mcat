% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/percent_listed.R
\name{percent_listed}
\alias{percent_listed}
\title{Percent Listed}
\usage{
percent_listed(individuals)
}
\arguments{
\item{individuals}{data frame; a data frame of individual mussel records.}
}
\value{
A data frame of sampled sites with the calculated percent listed MCAT
        metric
}
\description{
Calculates the percent listed MCAT metric for the input
individual mussel data frame.
}
\examples{
# Create the individual mussel data frame
individuals <- mcat::individuals

# Calculate percent listed for the individuals data frame
pl <- percent_listed(individuals)

}
