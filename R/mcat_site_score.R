#' @title MCAT Site Score
#'
#' @description Calculates the MCAT site score for the input individual mussel
#' data frame.
#'
#' @export
#' @param individuals    data frame; a data frame of individual mussel records.
#' @param threshold      a Thresolds object; An object that holds the thresholds
#'                       for each MCAT metric above which a site is considered
#'                       to have met the MCAT metric threshold for consideration
#'                       as a healthy mussel community for that metric.
#'
#' @return A data frame of sampled sites with the calculated suite of MCAT
#'         metrics and a total site score.
#'
#' @examples
#' # Create the individual mussel data frame
#' individuals <- mcat::individuals
#'
#' # Calculate percent fresh dead for the individuals data frame
#' mss <- mcat_site_score(individuals)
#'
#' @importFrom dplyr group_by summarize
#'
mcat_site_score <- function(individuals,
                            threshold = default_threshold) {
  # Calculate each MCAT metric
  pl  <- percent_listed(individuals)
  pt  <- percent_tolerant(individuals)
  plp <- percent_lampsilini(individuals)
  pj  <- percent_juveniles(individuals)
  po  <- percent_over_15yrs(individuals)
  pfd <- percent_fresh_dead(individuals)
  a   <- abundance(individuals)
  se  <- species_evenness(individuals)
  te  <- tribe_evenness(individuals)
  sr  <- species_richness(individuals)

  # Join all of the MCAT metrics into a new `samples_mcat` data frame
  samples_mcat <- merge(x = pl[, c("SampleID", "percent_listed")],
                        y = pt[, c("SampleID", "percent_tolerant")],
                        by = "SampleID", all = TRUE)

  samples_mcat <- merge(x = samples_mcat,
                        y = plp[, c("SampleID", "percent_lampsilini")],
                        by = "SampleID", all = TRUE)

  samples_mcat <- merge(x = samples_mcat,
                        y = pj[, c("SampleID", "percent_juveniles")],
                        by = "SampleID", all = TRUE)

  samples_mcat <- merge(x = samples_mcat,
                        y = po[, c("SampleID", "percent_over_15yrs")],
                        by = "SampleID", all = TRUE)

  samples_mcat <- merge(x = samples_mcat,
                        y = pfd[, c("SampleID", "percent_fresh_dead")],
                        by = "SampleID", all = TRUE)

  samples_mcat <- merge(x = samples_mcat,
                        y = a[, c("SampleID", "abundance")],
                        by = "SampleID", all = TRUE)

  samples_mcat <- merge(x = samples_mcat,
                        y = se[, c("SampleID", "species_pielou_evenness")],
                        by = "SampleID", all = TRUE)

  samples_mcat <- merge(x = samples_mcat,
                        y = te[, c("SampleID", "tribe_pielou_evenness")],
                        by = "SampleID", all = TRUE)

  samples_mcat <- merge(x = samples_mcat,
                        y = sr[, c("SampleID", "rarefy_es_100")],
                        by = "SampleID", all = TRUE)

  # Add flag fields to indicate those sites that meet the MCAT site threshold
  samples_mcat$mcat_perc_listed      <- ifelse(samples_mcat$percent_listed >
                                                 threshold@percent_listed,
                                               1, 0)
  samples_mcat$mcat_perc_tolerant    <- ifelse(samples_mcat$percent_tolerant <
                                                 threshold@percent_tolerant,
                                               1, 0)
  samples_mcat$mcat_perc_lampsilini  <- ifelse(samples_mcat$percent_lampsilini >
                                                 threshold@percent_lampsilini,
                                               1, 0)
  samples_mcat$mcat_perc_juveniles   <- ifelse(samples_mcat$percent_juvenile >
                                                 threshold@percent_juveniles,
                                               1, 0)
  samples_mcat$mcat_perc_over_15     <- ifelse(samples_mcat$percent_over_15yrs >
                                                 threshold@percent_over_15yrs,
                                               1, 0)
  samples_mcat$mcat_abundance        <- ifelse(samples_mcat$abundance >
                                                 threshold@abundance,
                                               1, 0)
  samples_mcat$mcat_species_evenness <- ifelse(samples_mcat$species_pielou_evenness >
                                                 threshold@species_evenness,
                                               1, 0)
  samples_mcat$mcat_tribe_evenness   <- ifelse(samples_mcat$tribe_pielou_evenness >
                                                 threshold@tribe_evenness,
                                               1, 0)
  samples_mcat$mcat_es100            <- ifelse(samples_mcat$rarefy_es_100 >
                                                 threshold@species_richness,
                                               1, 0)

  # Create MCAT Site Score
  samples_mcat$mcat_site_score <- samples_mcat$mcat_perc_listed +
                                  samples_mcat$mcat_perc_tolerant +
                                  samples_mcat$mcat_perc_lampsilini +
                                  samples_mcat$mcat_perc_juveniles +
                                  samples_mcat$mcat_perc_over_15 +
                                  samples_mcat$mcat_abundance +
                                  samples_mcat$mcat_species_evenness +
                                  samples_mcat$mcat_tribe_evenness +
                                  samples_mcat$mcat_es100

  return(samples_mcat)
}

default_threshold <- new("Threshold",
                         percent_listed = 3,
                         percent_tolerant = 40,
                         percent_lampsilini = 40,
                         percent_juveniles = 50,
                         percent_over_15yrs = 5,
                         percent_fresh_dead = 5,
                         abundance = 13,
                         species_evenness = 0.7,
                         tribe_evenness = 0.8,
                         species_richness = 3)
