#' @title Threshold
#'
#' @description The Threshold object is designed to hold the cut-off values for
#' each MCAT metric above which a site is considered to have met the MCAT
#' metric threshold for consideration as a healthy mussel community for that
#' metric.
#'
#' @slot percent_listed
#' @slot percent_tolerant
#' @slot percent_lampsilini
#' @slot percent_juveniles
#' @slot percent_over_15yrs
#' @slot percent_fresh_dead
#' @slot abundance
#' @slot species_evenness
#' @slot tribe_evenness
#' @slot species_richness
#'
#' @export
#'
setClass("Threshold",
         slots = list(percent_listed = "numeric",
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
