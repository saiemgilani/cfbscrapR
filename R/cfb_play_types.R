#' College Football Mapping for Play Types
#'
#' This data frame helps identifies all play types identified in the play-by-play data.
#' This can be used to filter out play types when calling functions before hand.
#'
#' @return A data frame with 48 rows and 3 variables:
#' \describe{
#'   \item{play_type_id}{Referencing play type id}
#'   \item{text}{play type description}
#'   \item{abbreviation}{play type abbreviation used for function call}
#'   ...
#' }
#' @source \url{https://api.collegefootballdata.com/play/types}
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET
#' @importFrom utils URLencode
#' @importFrom assertthat assert_that
#' @importFrom glue glue
#'


cfb_play_types <- function(){

  base_url = "https://api.collegefootballdata.com/play/types"

  # Check for internet
  check_internet()

  # Create the GET request and set response as res
  res <- httr::GET(base_url)

  # Check the result
  check_status(res)
  df <- data.frame()
  tryCatch(
    expr = {
      # Get the content and return it as data.frame
      df = jsonlite::fromJSON(base_url) %>%
        dplyr::rename(play_type_id = .data$id) %>%
        as.data.frame()

      message(glue::glue("{Sys.time()}: Scraping play types data..."))
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments or no play types data available!"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )
  return(df)
}
