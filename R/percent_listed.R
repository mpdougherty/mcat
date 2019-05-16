#' @title Percent Listed
#'
#' @description Calculates the percent listed MCAT metric for the input
#' individual mussel data frame.
#'
#' @export
#' @param individuals    data frame; a data frame of individual mussel records.
#'
#' @return A data frame of sampled sites with the calculated percent listed MCAT
#'         metric
#'
#' @examples
#' # Create the individual mussel data frame
#' individuals <- mcat::individuals
#'
#' # Calculate percent listed for the individuals data frame
#' pl <- percent_listed(individuals)
#'
#' @importFrom dplyr group_by summarize
#'
percent_listed <- function(individuals) {
  # Get vector of listed species
  listed <- mcat::listed$listed

  # Set a flag field if the individual is listed and alive
  individuals$listed <- ifelse(individuals$Ename %in% listed &
                               individuals$NumberLive >= 1,
                               1, 0)

  # Group by SampleID
  individuals %>%
    dplyr::group_by(SampleID) %>%
    dplyr::summarize(SUM_NumberLive = sum(NumberLive),
                     SUM_Listed = sum(listed)) -> sample

  # Calculate percent listed
  sample$percent_listed <- (sample$SUM_Listed / sample$SUM_NumberLive) * 100

  # Convert NaN to zero (numerator and denominator is zero)
  sample$percent_listed[is.nan(sample$percent_listed)] <- 0

  # Convert Inf to zero (demominator is zero)
  sample$percent_listed[is.infinite(sample$percent_listed)] <- 0

  return(sample)
}
