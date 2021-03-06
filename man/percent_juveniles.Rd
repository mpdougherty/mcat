% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/percent_juveniles.R
\name{percent_juveniles}
\alias{percent_juveniles}
\title{Percent Juveniles}
\usage{
percent_juveniles(individuals)
}
\arguments{
\item{individuals}{data frame; a data frame of individual mussel records.}
}
\value{
A data frame of sampled sites with the calculated percent juveniles
        MCAT metric
}
\description{
Calculates the percent juveniles MCAT metric for the input
individual mussel data frame.
}
\examples{
# Create the individual mussel data frame
individuals <- mcat::individuals

# Calculate percent juveniles for the individuals data frame
pj <- percent_juveniles(individuals)

}
