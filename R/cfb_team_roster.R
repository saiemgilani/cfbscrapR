#' Team Roster
#' Get a teams full roster by year. If year not selected, API defaults to most recent year (2019 as of 6/23/20)
#'
#' @param team (\emph{String} required): Team, select a valid team in D-I football
#' @param year (\emph{Integer} optional): Year,  4 digit format (\emph{YYYY})
#'
#' @keywords Team Roster
#' @importFrom jsonlite "fromJSON"
#' @importFrom httr "GET"
#' @importFrom utils "URLencode"
#' @importFrom assertthat "assert_that"
#' @export
#' @examples
#'
#' cfb_team_roster("Florida State")
#'

cfb_team_roster <- function(team, year = NULL){

  if(!is.null(year)){
    # check if year is numeric
    assert_that(is.numeric(year) & nchar(year) == 4,
                msg='Enter valid year as a number (YYYY)')
  }

  # Encode team parameter for URL
  team = URLencode(team, reserved = TRUE)

  base_url <- "https://api.collegefootballdata.com/roster?"

  full_url <- paste0(base_url,
                     "team=", team,
                     "&year=", year)

  # Check for internet
  check_internet()

  # Create the GET request and set response as res
  res <- GET(full_url)

  # Check the result
  check_status(res)

  # Get the content and return it as data.frame
  df = fromJSON(full_url) %>% 
    rename(athlete_id = .data$id) %>% 
    as.data.frame()

  return(df)
}

