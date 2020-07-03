#' Get Pre-game Win Probability Data from API
#'
#' @param year (\emph{Integer} optional): Year, 4 digit format (\emph{YYYY})
#' @param week (\emph{Integer} optional): Week - values from 1-15, 1-14 for seasons pre-playoff, i.e. 2013 or earlier
#' @param team (\emph{String} optional): D-I Team
#' @param season_type (\emph{String} default regular): Select Season Type: regular or postseason
#' 
#' @keywords Pre-game Win Probability Data
#' @importFrom jsonlite "fromJSON"
#' @importFrom httr "GET"
#' @importFrom utils "URLencode" "URLdecode"
#' @importFrom assertthat "assert_that"
#' @export
#'
#' @examples
#'
#' cfb_metrics_wp_pregame(year = 2019, week=9, team='Texas A&M')
#'
#'

cfb_metrics_wp_pregame <- function(year = NULL,
                                   week = NULL,
                                   team = NULL,
                                   season_type = 'regular') {
  
  if(!is.null(year)){
    # Check if year is numeric, if not NULL
    assert_that(is.numeric(year) & nchar(year) == 4,
                msg='Enter valid year (Integer): 4-digit (YYYY)')
  }
  if(!is.null(week)){
    # Check if week is numeric, if not NULL
    assert_that(is.numeric(week) & nchar(week) <= 2 & week <= 15,
                msg='Enter valid week (Integer): 1-15\n(14 for seasons pre-playoff, i.e. 2014 or earlier)')
  }
  if(!is.null(team)){
    # Encode team parameter for URL, if not NULL
    team = URLencode(team, reserved = TRUE)
  }
  if(season_type != 'regular'){
    # Check if season_type is appropriate, if not NULL
    assert_that(season_type %in% c('postseason'),
                msg='Enter valid season_type (String): regular or postseason.')
  }
  base_url <- "https://api.collegefootballdata.com/metrics/wp/pregame?"
  
  full_url <- paste0(base_url,
                     "year=", year,
                     "&week=", week,
                     "&team=", team,
                     "&seasonType=", season_type)
  
  # Check for internet
  check_internet()
  
  # Create the GET request and set response as res
  res <- GET(full_url)
  
  # Check the result
  check_status(res)
  
  # Get the content and return it as data.frame
  df = fromJSON(full_url)
  
  return(df)
}
