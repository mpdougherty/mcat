#' @title Percent Juveniles
#'
#' @description Calculates the percent juveniles MCAT metric for the input
#' individual mussel data frame.
#'
#' @export
#' @param individuals    data frame; a data frame of individual mussel records.
#'
#' @return A data frame of sampled sites with the calculated percent juveniles
#'         MCAT metric
#'
#' @examples
#' # Create the individual mussel data frame
#' individuals <- mcat::individuals
#'
#' # Calculate percent juveniles for the individuals data frame
#' pj <- percent_juveniles(individuals)
#'
#' @importFrom dplyr group_by summarize
#'
percent_juveniles <- function(individuals) {
  # Set a flag field if the individual is juvenile and alive
  individuals$juveniles <- ifelse(individuals$Age <= 5 &
                                         individuals$NumberLive >= 1,
                                  1, 0)

  # Check if Age is NA, set juveniles to zero
  individuals$juveniles[is.na(individuals$juveniles)] <- 0

  # Group by SampleID
  individuals %>%
    dplyr::group_by(SampleID) %>%
    dplyr::summarize(SUM_NumberLive = sum(NumberLive),
                     SUM_Juveniles = sum(juveniles)) -> sample

  # Calculate percent juveniles
  sample$percent_juveniles <- (sample$SUM_Juveniles / sample$SUM_NumberLive) * 100

  # Convert NaN to zero (numerator and denominator is zero)
  sample$percent_juveniles[is.nan(sample$percent_juveniles)] <- 0

  # Convert Inf to zero (demominator is zero)
  sample$percent_juveniles[is.infinite(sample$percent_juveniles)] <- 0

  return(sample)
}
