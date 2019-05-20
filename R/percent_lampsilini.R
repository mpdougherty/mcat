#' @title Percent Tribe Lampsilini
#'
#' @description Calculates the percent tribe Lampsilini MCAT metric for the
#' input individual mussel data frame.
#'
#' @export
#' @param individuals    data frame; a data frame of individual mussel records.
#'
#' @return A data frame of sampled sites with the calculated percent tribe
#'         lampsilini MCAT metric
#'
#' @examples
#' # Create the individual mussel data frame
#' individuals <- mcat::individuals
#'
#' # Calculate percent tribe lampsilini for the individuals data frame
#' plm <- percent_lampsilini(individuals)
#'
#' @importFrom dplyr group_by summarize
#'
percent_lampsilini <- function(individuals) {
  # Get vector of tribe lampsilini species
  lampsilini <- mcat::lampsilini$lampsilini

  # Create the `number_live` field
  individuals$number_live <- ifelse(individuals$Status == "Live", 1, 0)

  # Set a flag field if the individual is lampsilini and alive
  individuals$lampsilini <- ifelse(individuals$Ename %in% lampsilini &
                                   individuals$number_live >= 1,
                                   1, 0)

  # Group by SampleID
  individuals %>%
    dplyr::group_by(SampleID) %>%
    dplyr::summarize(SUM_number_live = sum(number_live),
                     SUM_lampsilini = sum(lampsilini)) -> sample

  # Calculate percent tribe lampsilini
  sample$percent_lampsilini <- (sample$SUM_lampsilini / sample$SUM_number_live) * 100

  # Convert NaN to zero (numerator and denominator is zero)
  sample$percent_lampsilini[is.nan(sample$percent_lampsilini)] <- 0

  # Convert Inf to zero (demominator is zero)
  sample$percent_lampsilini[is.infinite(sample$percent_lampsilini)] <- 0

  return(sample)
}
