#' College Football Mapping for Play Stats Types
#'
#' This function identifies all play stats types identified in the play by play data.
#' This can be used to filter out play stats types when calling functions before hand.
#'
#' @format A data frame with 22 rows and 2 variables:
#' \describe{
#'   \item{id}{Referencing play id}
#'   \item{name}{Type of play stats}
#'   ...
#' }
#' @keywords Plays
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET
#' @export
#' @examples
#'
#' cfb_play_stats_types()
#'


cfb_play_stats_types <- function(){

  base_url = "https://api.collegefootballdata.com/play/stat/types"

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
