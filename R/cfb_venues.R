#' CFB Venue Information
#'
#' Pulls all college football venues and data on capacity, grass, city/state, location,
#' elevation, dome, timezone and construction year
#' 
#' @return A data frame with 335 rows and 13 variables:
#' \describe{
#'   \item{`venue_id`}{integer.}
#'   \item{`name`}{character.}
#'   \item{`capacity`}{integer.}
#'   \item{`grass`}{logical.}
#'   \item{`city`}{character.}
#'   \item{`state`}{character.}
#'   \item{`zip`}{character.}
#'   \item{`country_code`}{character.}
#'   \item{`location`}{list.}
#'   \item{`elevation`}{character.}
#'   \item{`year_constructed`}{integer.}
#'   \item{`dome`}{logical.}
#'   \item{`timezone`}{character.}
#' }
#' @source <https://api.collegefootballdata.com/venues>
#' @keywords Venues
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET
#' @importFrom dplyr rename
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
