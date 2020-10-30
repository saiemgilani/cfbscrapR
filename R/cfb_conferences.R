#' CFB Conference Information
#'
#' Pulls all college football conferences and returns as data frame the following fields:
#' @return A data frame with 11 rows and 4 variables:
#' \describe{
#'   \item{conference_id}{Referencing conference id}
#'   \item{name}{Conference name}
#'   \item{long_name}{Long name for Conference}
#'   \item{abbreviation}{Conference abbreviation}
#'   ...
#' }
#' @source \url{https://api.collegefootballdata.com/conferences}
#' @keywords Conferences
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET
#' @import dplyr
#' @import tidyr
#' @export
#' @examples
#'
#' cfb_conferences()
#'


cfb_conferences <- function(){

  base_url = "https://api.collegefootballdata.com/conferences"

  # Check for internet
  check_internet()

  # Create the GET request and set response as res
  res <- httr::GET(base_url)

  # Check the result
  check_status(res)

  # Get the content and return it as data.frame
  df = jsonlite::fromJSON(base_url)

  # Rename id as conference_id, short_name as long_name
  df <- df %>%
    dplyr::rename(conference_id = .data$id,
                  long_name = .data$short_name)

  return(df)
}
