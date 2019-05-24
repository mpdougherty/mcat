#' @title Threshold
#'
#' @description The Threshold object is designed to hold the cut-off values for
#' each MCAT metric above which a site is considered to have met the MCAT
#' metric threshold for consideration as a healthy mussel community for that
#' metric.
#'
#' @slot percent_listed     numeric; Percent of mussels listed
#' @slot percent_tolerant   numeric; Percent of mussels tolerant
#' @slot percent_lampsilini numeric; Percent of mussels lampsilini
#' @slot percent_juveniles  numeric; Percent of mussels juveniles
#' @slot percent_over_15yrs numeric; Percent of mussels over 15 years old
#' @slot percent_fresh_dead numeric; Percent of mussels freshly dead
#' @slot abundance          numeric; Abundance of mussels
#' @slot species_evenness   numeric; Species evenness of mussels
#' @slot tribe_evenness     numeric; Tribe evenness of mussels
#' @slot species_richness   numeric; Species richness of mussels
#'
#' @export
#'
setClass("Threshold",
         representation(percent_listed = "numeric",
                        percent_tolerant = "numeric",
                        percent_lampsilini = "numeric",
                        percent_juveniles = "numeric",
                        percent_over_15yrs = "numeric",
                        percent_fresh_dead = "numeric",
                        abundance = "numeric",
                        species_evenness = "numeric",
                        tribe_evenness = "numeric",
                        species_richness = "numeric")
)
