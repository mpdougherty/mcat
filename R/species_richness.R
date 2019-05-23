#' @title Species Richness
#'
#' @description Calculates the species richness MCAT metric for the input
#' individual mussel data frame.
#'
#' @export
#' @param individuals    data frame; a data frame of individual mussel records.
#'
#' @return A data frame of sampled sites with the calculated species richness
#'         MCAT metric
#'
#' @examples
#' # Create the individual mussel data frame
#' individuals <- mcat::individuals
#'
#' # Calculate species richness for the individuals data frame
#' sr <- species_richness(individuals)
#'
#' @importFrom dplyr group_by summarize
#' @importFrom labdsv matrify
#' @importFrom vegan rarefy diversity specnumber
#'
species_richness <- function(individuals) {
  # Create the `number_live` field
  individuals$number_live <- ifelse(individuals$Status == "Live", 1, 0)

  # Group by SampleID
  individuals %>%
    dplyr::group_by(SampleID, Ename) %>%
    dplyr::summarize(SUM_number_live = sum(number_live)) -> sample_species

  # Convert sample_tribe from a grouped df to a regular data frame
  sample_species_df <- as.data.frame(sample_species)

  # Convert to vegan community data matrix-like format using labdsv::matrify
  sample_species_matrix <- labdsv::matrify(sample_species_df)

  # Calculate rarefaction using a sample size of 100
  rarefy_es_100 <- vegan::rarefy(x = sample_species_matrix,
                                 sample = 100)

  # Calculate the Shannon-Weaver diversity index
  shannon_diversity <- vegan::diversity(sample_species_matrix)

  # Calculate the number species per site (see ?vegan::diversity)
  species_number <- vegan::specnumber(sample_species_matrix)

  # Create a data frame of results
  rarefy_species <- data.frame(SampleID = names(rarefy_es_100),
                               rarefy_es_100,
                               shannon = shannon_diversity,
                               species_number = species_number)

  return(rarefy_species)
}
