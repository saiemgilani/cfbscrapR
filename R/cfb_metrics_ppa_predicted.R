#' Calculate Predicted Points using Down and Distance
#'
#' @param down (*Integer* required): Down filter
#' @param distance (*Integer* required): Distance filter
#' 
#' @return A data frame with 2 variables:
#' \describe{
#'   \item{`yard_line`}{integer.}
#'   \item{`predicted_points`}{character.}
#' }
#' @source <https://api.collegefootballdata.com/ppa/predicted>
#' @keywords Predicted Points 
#' @importFrom attempt stop_if_any
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET
#' @importFrom assertthat assert_that
#' @importFrom glue glue
#' @import dplyr
#' @import tidyr
#' @export
#' @examples
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
              msg = "You need to specify both arguments, down and distance, as integers")
  
  # Check if down is numeric
  assertthat::assert_that(is.numeric(down) & down <= 4,
              msg = 'Enter valid down (Integer): values from 1-4')
  
  # Check if distance is numeric
  assertthat::assert_that(is.numeric(distance) & distance <= 99,
              msg = 'Enter valid distance (Integer): values from 1-99')

  base_url <- "https://api.collegefootballdata.com/ppa/predicted?"

  full_url <- paste0(base_url,
                     "down=", down,
                     "&distance=", distance)

  # Check for internet
  check_internet()

  # Create the GET request and set response as res
  res <- httr::GET(full_url)

  # Check the result
  check_status(res)
  
  df <- data.frame()
  tryCatch(
    expr ={    
      # Get the content, flatten and return result as data.frame
      df = jsonlite::fromJSON(full_url) 
      colnames(df) = gsub("Line", "_line", colnames(df))
      colnames(df) = gsub("Points", "_points", colnames(df))
      
      message(glue::glue("{Sys.time()}: Scraping CFBData metrics PPA predicted data..."))
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments or no CFBData metrics PPA predicted data available!"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )
  return(df)
}
