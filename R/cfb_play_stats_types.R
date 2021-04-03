#' College Football Mapping for Play Stats Types
#'
#' This function identifies all play stats types identified in the play by play data.
#' This can be used to filter out play stats types when calling functions before hand.
#'
#' @return A data frame with 22 rows and 2 variables:
#' \describe{
#'   \item{play_stat_type_id}{Referencing play stat type ID}
#'   \item{name}{Type of play stats}
#'   ...
#' }
#' @source <https://api.collegefootballdata.com/play/stat/types>
#' @keywords Plays
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET
#' @importFrom glue glue
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
  res <- httr::GET(base_url)

  # Check the result
  check_status(res)
  
  df <- data.frame()
  tryCatch(
    expr = {
      # Get the content and return it as data.frame
      df = jsonlite::fromJSON(base_url) %>% 
        dplyr::rename(play_stat_type_id = .data$id) %>% 
        as.data.frame()
      message(glue::glue("{Sys.time()}: Scraping play stats types data..."))
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments or no play stats types data available!"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )
  return(df)
}
