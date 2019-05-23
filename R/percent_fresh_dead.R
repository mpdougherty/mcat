#' @title Percent Fresh Dead
#'
#' @description Calculates the percent fresh dead MCAT metric for the input
#' individual mussel data frame.
#'
#' @export
#' @param individuals    data frame; a data frame of individual mussel records.
#'
#' @return A data frame of sampled sites with the calculated percent fresh dead
#'         MCAT metric
#'
#' @examples
#' # Create the individual mussel data frame
#' individuals <- mcat::individuals
#'
#' # Calculate percent fresh dead for the individuals data frame
#' pj <- percent_juveniles(individuals)
#'
#' @importFrom dplyr group_by summarize
#'
percent_fresh_dead <- function(individuals) {
  # Set a flag field for the number of mussels per record (always = 1)
  individuals$mussels_per_record <- 1

  # Set a flag field if the individual is fresh dead
  individuals$fresh_dead <- ifelse(individuals$Status == "Fresh Dead", 1, 0)

  # Check if Status is NA, set fresh_dead to zero
  individuals$fresh_dead[is.na(individuals$fresh_dead)] <- 0

  # Group by SampleID
  individuals %>%
    dplyr::group_by(SampleID) %>%
    dplyr::summarize(SUM_mussels_per_record = sum(mussels_per_record),
                     SUM_fresh_dead = sum(fresh_dead)) -> sample

  # Calculate percent fresh dead
  sample$percent_fresh_dead <- (sample$SUM_fresh_dead /
                                  sample$SUM_mussels_per_record) * 100

  # Convert NaN to zero (numerator and denominator is zero)
  sample$percent_fresh_dead[is.nan(sample$percent_fresh_dead)] <- 0

  # Convert Inf to zero (demominator is zero)
  sample$percent_fresh_dead[is.infinite(sample$percent_fresh_dead)] <- 0

  return(sample)
}
