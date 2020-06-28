#' College Football Mapping for Stats Categories
#'
#' This function identifies all Stats Categories identified in the regular stats endpoint.
#'
#' @format A data.frame with 38 values:
#' \describe{
#'   \item{name}{Statistics Categories}
#'   ...
#' }
#' @keywords Stats Categories
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET
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
  res <- GET(base_url)

  # Check the result
  check_status(res)

  # Get the content and return it as list
  list = fromJSON(base_url)
  df <- as.data.frame(matrix(unlist(list),nrow=length(list),byrow = TRUE)) %>%
    rename(category = .data$V1)
  return(df)
}
