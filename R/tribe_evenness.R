#' @title Tribe Evenness
#'
#' @description Calculates the tribe evenness MCAT metric for the input
#' individual mussel data frame.
#'
#' @export
#' @param individuals    data frame; a data frame of individual mussel records.
#'
#' @return A data frame of sampled sites with the calculated tribe evenness
#'         MCAT metric
#'
#' @examples
#' # Create the individual mussel data frame
#' individuals <- mcat::individuals
#'
#' # Calculate species evenness for the individuals data frame
#' te <- tribe_evenness(individuals)
#'
#' @importFrom dplyr group_by summarize
#' @importFrom labdsv matrify
#' @importFrom vegan diversity specnumber
#'
tribe_evenness <- function(individuals) {
  # Get tribe data frame
  tribe <- mcat::tribe

  # Create the `number_live` field
  individuals$number_live <- ifelse(individuals$Status == "Live", 1, 0)

  # Create the tribe_name field
  individuals$tribe_name <- ifelse(individuals$Ename %in% tribe$species,
                                   tribe$tribe, NA)

  # Set a flag field if the individual has a tribe and is alive
  individuals$tribe <- ifelse(!is.na(individuals$tribe) &
                              individuals$number_live >= 1,
                              1, 0)

  # Group by SampleID
  individuals %>%
    dplyr::group_by(SampleID, tribe_name) %>%
    dplyr::summarize(SUM_tribe = sum(tribe)) -> sample_tribe

  # Convert sample_tribe from a grouped df to a regular data frame
  sample_tribe_df <- as.data.frame(sample_tribe)

  # Convert to vegan community data matrix-like format using labdsv::matrify
  sample_tribe_matrix <- labdsv::matrify(sample_tribe_df)

  # Calculate the Shannon-Weaver diversity index
  tribe_shannon_diversity <- vegan::diversity(sample_tribe_matrix)

  # Calculate the number of tribes per site (see ?vegan::diversity)
  tribe_number <- vegan::specnumber(sample_tribe_matrix)

  # Use the following equation to calculate Pielou's evenness (See the vegan
  # 'Diversity Vignette' for details).
  tribe_pielou_evenness <- tribe_shannon_diversity/log(tribe_number)

  # Set NaN pielou_evenness values to zero
  tribe_pielou_evenness <- ifelse(is.nan(tribe_pielou_evenness),
                                  0, tribe_pielou_evenness)

  # Create a data frame of results
  tribe_pielou <- data.frame(SampleID = names(tribe_pielou_evenness),
                             tribe_shannon_diversity,
                             tribe_number,
                             tribe_pielou_evenness)

  return(tribe_pielou)
}
