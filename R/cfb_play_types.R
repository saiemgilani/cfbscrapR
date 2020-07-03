#' College Football Mapping for Play Types
#'
#' This data.frame helps identifies all play types identified in the play-by-play data.
#' This can be used to filter out play types when calling functions before hand.
#'
#'
#' @format A data frame with 45 rows and 3 variables:
#' \describe{
#'   \item{id}{Referencing play id}
#'   \item{text}{play type description}
#'   \item{abberivation}{play type abberivation used for function call}
#'   ...
#' }
#' @source \url{https://api.collegefootballdata.com/play/types}
#'
#' @importFrom jsonlite "fromJSON"
#' @importFrom httr "GET"
#' @importFrom utils "URLencode"
#' @importFrom assertthat "assert_that"
#'


cfb_play_types <- function(){

  base_url = "https://api.collegefootballdata.com/play/types"

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
