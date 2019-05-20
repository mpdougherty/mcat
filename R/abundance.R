#' @title Abundance
#'
#' @description Calculates the abundance MCAT metric for the input
#' individual mussel data frame.
#'
#' @export
#' @param individuals    data frame; a data frame of individual mussel records.
#'
#' @return A data frame of sampled sites with the calculated abundance
#'         MCAT metric
#'
#' @examples
#' # Create the individual mussel data frame
#' individuals <- mcat::individuals
#'
#' # Calculate abundance for the individuals data frame
#' a <- abundance(individuals)
#'
#' @importFrom dplyr group_by summarize %>%
#'
abundance <- function(individuals) {
  # Create the `number_live` field
  individuals$number_live <- ifelse(individuals$Status == "Live", 1, 0)

  # Set a flag field if the individual is alive
  individuals$abund <- ifelse(individuals$number_live >= 1, 1, 0)

  # Group by SampleID
  individuals %>%
    dplyr::group_by(SampleID) %>%
    dplyr::summarize(SUM_number_live = sum(number_live),
                     abundance = sum(abund)) -> sample

  return(sample)
}
