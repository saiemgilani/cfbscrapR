#' CFB Recruiting Information - Team Rankings
#'
#' Gets CFB team recruiting ranks with filters available for year and team.
#' At least one of \strong{year} or \strong{team} must be specified for the function to run
#'
#' If you would like CFB recruiting information for players, please
#' see the `cfb_recruiting_player()` function
#'
#' If you would like to get CFB recruiting information based on position groups during a
#' time period for all FBS teams, please see the `cfb_recruiting_position()` function.
#'
#' @param year (\emph{Integer} optional): Recruiting Class Year, 4 digit format (\emph{YYYY}). \emph{Note: 2000 is the minimum value}
#' @param team (\emph{String} optional): Team - Select a valid team, D1 football
#'
#' @keywords Recruiting
#' @importFrom attempt stop_if_all
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET
#' @importFrom utils "URLencode"
#' @importFrom assertthat "assert_that"
#' @export
#' @examples
#'
#' cfb_recruiting_team(2018, team = "Texas")
#'
#' cfb_recruiting_team(2016, team = "Virginia")
#'
#' cfb_recruiting_team(2016, team = "Texas A&M")
#'
#' cfb_recruiting_team(2011)
#'

cfb_recruiting_team <- function(year = NULL,
                                team = NULL){

  args <- list(year = year, team = team)

  # Check that at least one argument is not null
  stop_if_all(args, is.null,
              msg='You need to specify at least one argument: \nyear, as integer in 4 digit format (YYYY) - Min: 2000, Max: 2020\n or team')

  if(!is.null(year)){
    ## check if year is numeric
    assert_that(is.numeric(year) & nchar(year)==4,
                msg='Enter valid year as integer in 4 digit format (YYYY)\n Min: 2000, Max: 2020')
  }
  if(!is.null(team)){
    # Encode team parameter for URL if not NULL
    team = URLencode(team, reserved = TRUE)
  }

  base_url = "https://api.collegefootballdata.com/recruiting/teams?"

  # Create full url using base and input arguments
  full_url = paste0(base_url,
                    "year=", year,
                    "&team=", team)

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
