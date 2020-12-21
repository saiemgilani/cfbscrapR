#' Calendar
#' Returns calendar of weeks by season
#'
#' @param year (\emph{Integer} required): Year,  4 digit format (\emph{YYYY})
#'
#' @return A data frame with 5 variables:
#' @source \url{https://api.collegefootballdata.com/calendar}
#' @importFrom dplyr rename mutate
#' @importFrom janitor clean_names
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET
#' @importFrom utils URLencode
#' @importFrom assertthat assert_that
#' @importFrom glue glue
#' @export
#' @examples
#'
#' cfb_calendar(2019)
#'
cfb_calendar <- function(year) {

  if(!is.null(year)){
    # check if year is numeric
    assert_that(is.numeric(year) & nchar(year) == 4,
                msg='Enter valid year as a number (YYYY)')
  }

  base_url <- "https://api.collegefootballdata.com/calendar?"
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
        janitor::clean_names() %>%
        as.data.frame()

      message(glue::glue("{Sys.time()}: Scraping calendar..."))
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}:Invalid arguments or no calendar data available!"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )
  return(df)


}
