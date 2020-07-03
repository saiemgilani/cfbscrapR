#' CFB Venue Information
#'
#' Pulls all college football venues and data on capacity, grass, city/state, location,
#' elevation, dome, timezone and construction year
#' @keywords Venues
#' @importFrom jsonlite "fromJSON"
#' @importFrom httr "GET"
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
  res <- GET(base_url)

  # Check the result
  check_status(res)

  # Get the content and return it as data.frame
  df = fromJSON(base_url)

  return(df)
}
