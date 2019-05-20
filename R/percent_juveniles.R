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
  # Create the `number_live` field
  individuals$number_live <- ifelse(individuals$Status == "Live", 1, 0)

  # Set a flag field if the individual is juvenile and alive
  individuals$juveniles <- ifelse(individuals$Age <= 5 &
                                         individuals$number_live >= 1,
                                  1, 0)

  # Check if Age is NA, set juveniles to zero
  individuals$juveniles[is.na(individuals$juveniles)] <- 0

  # Group by SampleID
  individuals %>%
    dplyr::group_by(SampleID) %>%
    dplyr::summarize(SUM_number_live = sum(number_live),
                     SUM_juveniles = sum(juveniles)) -> sample

  # Calculate percent juveniles
  sample$percent_juveniles <- (sample$SUM_juveniles / sample$SUM_number_live) * 100

  # Convert NaN to zero (numerator and denominator is zero)
  sample$percent_juveniles[is.nan(sample$percent_juveniles)] <- 0

  # Convert Inf to zero (demominator is zero)
  sample$percent_juveniles[is.infinite(sample$percent_juveniles)] <- 0

  return(sample)
}
