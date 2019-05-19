#' @title Percent Over 15 Years
#'
#' @description Calculates the percent over 15 years MCAT metric for the input
#' individual mussel data frame.
#'
#' @export
#' @param individuals    data frame; a data frame of individual mussel records.
#'
#' @return A data frame of sampled sites with the calculated percent over 15 years
#'         MCAT metric
#'
#' @examples
#' # Create the individual mussel data frame
#' individuals <- mcat::individuals
#'
#' # Calculate percent over 15 years for the individuals data frame
#' pj <- percent_over_15yrs(individuals)
#'
#' @importFrom dplyr group_by summarize
#'
percent_over_15yrs <- function(individuals) {
  # Set a flag field if the individual is juvenile and alive
  individuals$over_15yrs <- ifelse(individuals$Age >= 15 &
                                    individuals$NumberLive >= 1,
                                   1, 0)

  # Check if Age is NA, set over 15 years to zero
  individuals$over_15yrs[is.na(individuals$over_15yrs)] <- 0

  # Group by SampleID
  individuals %>%
    dplyr::group_by(SampleID) %>%
    dplyr::summarize(SUM_NumberLive = sum(NumberLive),
                     SUM_Over_15yrs = sum(over_15yrs)) -> sample

  # Calculate percent over 15 years
  sample$percent_over_15yrs <- (sample$SUM_Over_15yrs / sample$SUM_NumberLive) * 100

  # Convert NaN to zero (numerator and denominator is zero)
  sample$percent_over_15yrs[is.nan(sample$percent_over_15yrs)] <- 0

  # Convert Inf to zero (demominator is zero)
  sample$percent_over_15yrs[is.infinite(sample$percent_over_15yrs)] <- 0

  return(sample)
}
