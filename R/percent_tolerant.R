#' @title Percent Tolerant
#'
#' @description Calculates the percent tolerant MCAT metric for the input
#' individual mussel data frame.
#'
#' @export
#' @param individuals    data frame; a data frame of individual mussel records.
#'
#' @return A data frame of sampled sites with the calculated percent tolerant
#'         MCAT metric
#'
#' @examples
#' # Create the individual mussel data frame
#' individuals <- mcat::individuals
#'
#' # Calculate percent tolerant for the individuals data frame
#' pt <- percent_tolerant(individuals)
#'
#' @importFrom dplyr group_by summarize
#'
percent_tolerant <- function(individuals) {
  # Get vector of tolerant species
  tolerant <- mcat::tolerant$tolerant

  # Create the `number_live` field
  individuals$number_live <- ifelse(individuals$Status == "Live", 1, 0)

  # Set a flag field if the individual is tolerant and alive
  individuals$tolerant <- ifelse(individuals$Ename %in% tolerant &
                                 individuals$number_live >= 1,
                                 1, 0)

  # Group by SampleID
  individuals %>%
    dplyr::group_by(SampleID) %>%
    dplyr::summarize(SUM_number_live = sum(number_live),
                     SUM_tolerant = sum(tolerant)) -> sample

  # Calculate percent listed
  sample$percent_tolerant <- (sample$SUM_tolerant / sample$SUM_number_live) * 100

  # Convert NaN to zero (numerator and denominator is zero)
  sample$percent_tolerant[is.nan(sample$percent_tolerant)] <- 0

  # Convert Inf to zero (demominator is zero)
  sample$percent_tolerant[is.infinite(sample$percent_tolerant)] <- 0

  return(sample)
}
