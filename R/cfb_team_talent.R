#' Get composite team talent rankings for all teams in a given year
#'
#' Extracts team talent composite as sourced from 247 rankings
#'
#' @param year (\emph{Integer} optional): Year 4 digit format (\emph{YYYY})
#'
#' @keywords Team talent
#' @importFrom jsonlite "fromJSON"
#' @importFrom httr "GET"
#' @importFrom utils "URLencode"
#' @importFrom assertthat "assert_that"
#' @importFrom glue "glue"
#' @export
#' @examples
#'
#' cfb_team_talent()
#'
#' cfb_team_talent(year = 2018)
#'


cfb_team_talent <- function(year = NULL) {
  if(!is.null(year)){
    ## check if year is numeric
    assert_that(is.numeric(year) & nchar(year) == 4,
                msg='Enter valid year as a number (YYYY)')
  }

  base_url <- "https://api.collegefootballdata.com/talent?"

  full_url <- paste0(base_url,
                     "year=", year)

  # Check for internet
  check_internet()

  # Create the GET request and set response as res
  res <- httr::GET(full_url)

  # Check the result
  check_status(res)

  df <- data.frame()
  tryCatch(
    expr ={
      # Get the content and return it as data.frame
      df = jsonlite::fromJSON(full_url) %>% 
        as.data.frame()
      
      message(glue::glue("{Sys.time()}: Scraping team talent..."))
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}:Invalid arguments or no team talent data available!"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )    
  return(df)
}
