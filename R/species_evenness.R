#' @title Species Evenness
#'
#' @description Calculates the species evenness MCAT metric for the input
#' individual mussel data frame.
#'
#' @export
#' @param individuals    data frame; a data frame of individual mussel records.
#'
#' @return A data frame of sampled sites with the calculated species evenness
#'         MCAT metric
#'
#' @examples
#' # Create the individual mussel data frame
#' individuals <- mcat::individuals
#'
#' # Calculate species evenness for the individuals data frame
#' se <- species_evenness(individuals)
#'
#' @importFrom dplyr group_by summarize
#' @importFrom labdsv matrify
#' @importFrom vegan diversity specnumber
#'
species_evenness <- function(individuals) {
  # Create the `number_live` field
  individuals$number_live <- ifelse(individuals$Status == "Live", 1, 0)

  # Group by SampleID
  individuals %>%
    dplyr::group_by(SampleID, Ename) %>%
    dplyr::summarize(SUM_number_live = sum(number_live)) -> sample_species

  # Convert sample_species from a grouped df to a regular data frame
  sample_species_df <- as.data.frame(sample_species)

  # Convert to vegan community data matrix-like format using labdsv::matrify
  sample_species_matrix <- labdsv::matrify(sample_species_df)

  # Calculate the Shannon-Weaver diversity index
  shannon_diversity <- vegan::diversity(sample_species_matrix)

  # Calculate the number species per site (see ?vegan::diversity)
  species_number <- vegan::specnumber(sample_species_matrix)

  # Use the following equation to calculate Pielou's evenness (See the vegan
  # 'Diversity Vignette' for details).
  species_pielou_evenness <- shannon_diversity/log(species_number)

  # Set NaN pielou_evenness values to zero
  species_pielou_evenness <- ifelse(is.nan(species_pielou_evenness),
                                    0, species_pielou_evenness)

  # Create a data frame of results
  pielou <- data.frame(SampleID = names(species_pielou_evenness),
                       shannon_diversity,
                       species_number,
                       species_pielou_evenness)

  return(pielou)
}
