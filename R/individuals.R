#' @docType data
#'
#' @title Individual Mussel Samples, Upper Mississippi River, Lower Pool 10
#'
#'@format A data frame with 1212 observations and 29 variables.
#' \describe{
#'    \item{SampleID}{Unique identifier for a sample to be entered, example -
#'                    200703131101. Calculated by concatenating DATE + ORG_ID +
#'                    SAMPLE_NUM}
#'    \item{Ename}{Species scientific name.}
#'    \item{Previously_Marked}{Yes/No - Slash or other non-numeric mark
#'                             indicating mussel was previously marked.}
#'    \item{Marked_mussel}{Yes/No - Slash or other non-numeric mark indicating
#'                         mussel was relocated to that site.}
#'    \item{Status}{Designates a condition of the shell (fresh dead, weathered
#'                  dead, or sub-fossil) in order to estimate how long the
#'                  specimen has been dead.}
#'    \item{Sex}{Male/Female}
#'    \item{Gravidity}{Female gravid, female not gravid, female charging, male}
#'    \item{Age}{Number of annuli}
#'    \item{Length}{Maximum length in mm (anterior/posterior axis)}
#'    \item{Height}{Maximum height in mm (ventral/dorsal axis)}
#'    \item{Voucher_Location}{Where has the specimen been deposited (e.g., JFBM
#'                            - James Ford Bell Museum, University of
#'                            Minnesota).}
#'    \item{Voucher_Type}{How has the specimen been deposited (Wet with soft
#'                        tissue preserved in ethanol or dry shell only).}
#'    \item{ZebrasAttached}{Presence or absence of Zebra mussels attached.}
#'    \item{Comments}{Comments about an individual mussel (e.g. shell highly
#'                    eroded, deformed, gaping).}
#'    \item{Northing}{Latitude}
#'    \item{Easting}{Longitude}
#'    \item{Method}{Survey protocol - quantative, qualitative}
#'    \item{NumberLive}{Number of live mussels. }
#'    \item{NUM5YEAR}{Number of mussels five years of age or older. }
#'    \item{NumZebraAttached}{Total number of zebra mussels attached to the
#'                            mussel categorized in four groups (0, 1-10, 11-50,
#'                            >50).}
#'    \item{PerZebraCoverage}{Percent of native mussel shel cover with zebra
#'                            mussels categorized in four groups (0, 1-10, 11-50,
#'                            51-100).}
#' }
#' @source This dataset was downloaded from the \href
#' {https://rsgisias.crrel.usace.army.mil/intro/mussels_dev.pub.main}
#' {USACE Mussel Database}.
#'
#' @keywords datasets
#'
"individuals"
