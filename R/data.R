#' County and Zip Code Arranged by Small Area
#'
#' County and Zip Code designations arranged by small area based on Utah Local
#' Health District and Small Area designation algorithm.
#'
#' @format
#' A data frame with 95 rows and 3 columns
#' \describe{
#'  \item{small_area}{Small Area numeric code}
#'  \item{county}{County numeric code}
#'  \item{zipcode}{Zip Code}
#' }
#' @source <https://ibis.utah.gov/ibisph-view/pdf/resource/SASGeoAssignCode.pdf>
"by_small_area"

#' Small Area and Zip Code Arranged by County
#'
#' Small Area and Zip Code designations arranged by county based on Utah Local
#' Health District and Small Area designation algorithm.
#'
#' @format
#' A data frame with 29 rows and 6 columns
#' \describe{
#'  \item{county_name}{County names}
#'  \item{county}{County numeric code}
#'  \item{district}{District numeric code}
#'  \item{small_area_by_county}{One-to-one small area to county, 96(missing)
#'  for counties with multiple small areas}
#'  \item{small_area}{Small Area numeric code}
#'  \item{zipcode}{Zip Code}
#' }
#' @source <https://ibis.utah.gov/ibisph-view/pdf/resource/SASGeoAssignCode.pdf>
"by_county"

#' Cross-border Zip code and county designations
#'
#' Table to determine small area when zip code and county borders cross. Small
#' Area with greatest HUD Res Rate will be used for inconsistent border crossings.
#'
#' @format
#' A data frame with 39 rows and 7 columns
#' \describe{
#'  \item{zip_cross}{Zip code}
#'  \item{county_1}{First county in zip code border}
#'  \item{sa_1}{First small area in zip code border}
#'  \item{county_2}{Second county in zip code border}
#'  \item{sa_2}{Second small area in zip code border}
#'  \item{biggest_sa}{Small area with greatest HUD res rate}
#'  \item{biggest_county}{County with greatest HUD res rate}
#' }
#' @source <https://ibis.utah.gov/ibisph-view/pdf/resource/SASGeoAssignCode.pdf>
"zip_county_cross"
