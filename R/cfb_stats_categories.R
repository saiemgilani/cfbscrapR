#' College Football Mapping for Stats Categories
#'
#' This function identifies all Stats Categories identified in the regular stats endpoint.
#'
#' @return A data frame with 38 values:
#' \describe{
#'   \item{name}{Statistics Categories}
#'   ...
#' }
#' @source \url{https://api.collegefootballdata.com/stats/categories}
#' @keywords Stats Categories
#' @importFrom jsonlite "fromJSON"
#' @importFrom httr "GET"
#' @importFrom glue "glue"
#' @import dplyr
#' @export
#' @examples
#'
#' cfb_stats_categories()
#'


cfb_stats_categories <- function(){

  base_url = "https://api.collegefootballdata.com/stats/categories"

  # Check for internet
  check_internet()

  # Create the GET request and set response as res
  res <- httr::GET(base_url)

  # Check the result
  check_status(res)
  
  df <- data.frame()
  tryCatch(
    expr ={
      # Get the content and return it as list
      list = jsonlite::fromJSON(base_url)
      df <- as.data.frame(matrix(unlist(list), nrow=length(list), byrow = TRUE)) %>%
        dplyr::rename(category = .data$V1)
      
      message(glue::glue("{Sys.time()}: Scraping stats categories data..."))
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments or no stats categories data available!"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )   
  return(df)
}
