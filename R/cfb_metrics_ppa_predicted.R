#' Calculate Predicted Points using Down and Distance
#'
#' @param down (\emph{Integer} required): Down filter
#' @param distance (\emph{Integer} required): Distance filter
#'
#' @keywords Predicted Points 
#' @importFrom attempt stop_if_any
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET
#' @importFrom assertthat "assert_that"
#' @import dplyr
#' @import tidyr
#' @export
#' @examples
#'
#'
#' cfb_metrics_ppa_predicted(down = 1, distance = 10)
#' 
#' cfb_metrics_ppa_predicted(down = 3, distance = 10)
#'

cfb_metrics_ppa_predicted <- function(down,
                                      distance) {
  args <- list(down = down,
               distance = distance)
  
  # Check that none of arguments are null
  stop_if_any(args, is.null,
              msg="You need to specify both arguments, down and distance, as integers")
  
  # Check if down is numeric
  assert_that(is.numeric(down) & down <= 4,
              msg='Enter valid down (Integer): values from 1-4')
  
  # Check if distance is numeric
  assert_that(is.numeric(distance) & distance <= 99,
              msg='Enter valid distance (Integer): values from 1-99')

  base_url <- "https://api.collegefootballdata.com/ppa/predicted?"

  full_url <- paste0(base_url,
                     "down=", down,
                     "&distance=", distance)

  # Check for internet
  check_internet()

  # Create the GET request and set response as res
  res <- GET(full_url)

  # Check the result
  check_status(res)

  # Get the content, flatten and return result as data.frame
  df = fromJSON(full_url) 
  colnames(df) = gsub("Line", "_line", colnames(df))
  colnames(df) = gsub("Points", "_points", colnames(df))
  
  return(df)
}
