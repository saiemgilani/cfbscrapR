#' CFB Venue Information
#'
#' Pulls all college football venues and data on capacity, grass, city/state, location,
#' elevation, dome, timezone and construction year
#' 
#' @return A data frame with 335 rows and 13 variables:
#' \describe{
#'   \item{\code{venue_id}}{integer.}
#'   \item{\code{name}}{character.}
#'   \item{\code{capacity}}{integer.}
#'   \item{\code{grass}}{logical.}
#'   \item{\code{city}}{character.}
#'   \item{\code{state}}{character.}
#'   \item{\code{zip}}{character.}
#'   \item{\code{country_code}}{character.}
#'   \item{\code{location}}{list.}
#'   \item{\code{elevation}}{character.}
#'   \item{\code{year_constructed}}{integer.}
#'   \item{\code{dome}}{logical.}
#'   \item{\code{timezone}}{character.}
#' }
#' @source \url{https://api.collegefootballdata.com/venues}
#' @keywords Venues
#' @importFrom jsonlite "fromJSON"
#' @importFrom httr "GET"
#' @import dplyr
#' @export
#' @examples
#'
#' cfb_venues()
#'


cfb_venues <- function(){

  base_url = "https://api.collegefootballdata.com/venues"

  # Check for internet
  check_internet()

  # Create the GET request and set response as res
  res <- httr::GET(base_url)

  # Check the result
  check_status(res)

  # Get the content and return it as data.frame
  df = jsonlite::fromJSON(base_url) %>% 
    dplyr::rename(venue_id = .data$id) %>% 
    as.data.frame()

  return(df)
}
