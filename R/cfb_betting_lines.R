#' Get Betting information from games
#'
#' @param game_id (\emph{Integer} optional): Game ID filter for querying a single game
#' Can be found using the `cfb_game_info()` function
#' @param year (\emph{Integer} required): Year, 4 digit format(\emph{YYYY})
#' @param week (\emph{Integer} optional): Week - values from 1-15, 1-14 for seasons pre-playoff (i.e. 2013 or earlier)
#' @param season_type (\emph{String} default regular): Select Season Type: regular or postseason
#' @param team (\emph{String} optional): D-I Team
#' @param home_team (\emph{String} optional): Home D-I Team
#' @param away_team (\emph{String} optional): Away D-I Team
#' @param conference (\emph{String} optional): Conference abbreviation - Select a valid FBS conference
#' Conference abbreviations P5: ACC, B12, B1G, SEC, PAC
#' Conference abbreviations G5 and FBS Independents: CUSA, MAC, MWC, Ind, SBC, AAC
#'
#' @keywords Betting Lines
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET
#' @importFrom utils "URLencode"
#' @importFrom assertthat "assert_that"
#' @import dplyr
#' @import tidyr
#' @export
#'
#' @examples
#'
#' cfb_betting_lines(year = 2018, week = 1, team = 'Florida State', conference = 'ACC')
#'
#' #7 OTs LSU @ TAMU
#' cfb_betting_lines(year = 2018, week = 13, team = "Texas A&M", conference = 'SEC')
#'
#'

cfb_betting_lines <- function(game_id = NULL,
                              year = NULL,
                              week = NULL,
                              season_type = 'regular',
                              team = NULL,
                              home_team = NULL,
                              away_team = NULL,
                              conference = NULL) {
  if(!is.null(game_id)){
    # Check if game_id is numeric, if not NULL
    assert_that(is.numeric(game_id),
                msg='Enter valid game_id (numeric value)')
  }
  if(!is.null(year)){
    # Check if year is numeric, if not NULL
    assert_that(is.numeric(year) & nchar(year) == 4,
              msg='Enter valid year as a number (YYYY)')
  }
  if(!is.null(week)){
    # Check if week is numeric, if not NULL
    assert_that(is.numeric(week) & nchar(week) <= 2,
                msg='Enter valid week 1-15\n(14 for seasons pre-playoff, i.e. 2014 or earlier)')
  }
  if(season_type != 'regular'){
    # Check if season_type is appropriate, if not regular
    assert_that(season_type %in% c('postseason'),
                msg='Enter valid season_type: regular or postseason')
  }
  if(!is.null(team)){
    # Encode team parameter for URL, if not NULL
    team = URLencode(team, reserved = TRUE)
  }
  if(!is.null(home_team)){
    # Encode home_team parameter for URL, if not NULL
    home_team = URLencode(home_team, reserved = TRUE)
  }
  if(!is.null(away_team)){
    # Encode away_team parameter for URL, if not NULL
    away_team = URLencode(away_team, reserved = TRUE)
  }
  if(!is.null(conference)){
    # Check conference parameter in conference abbreviations, if not NULL
    assert_that(conference %in% cfbscrapR::cfb_conf_types_df$abbreviation,
                msg = "Incorrect conference abbreviation, potential misspelling.\nConference abbreviations P5: ACC, B12, B1G, SEC, PAC\nConference abbreviations G5 and Independents: CUSA, MAC, MWC, Ind, SBC, AAC")
    # Encode conference parameter for URL, if not NULL
    conference = URLencode(conference, reserved = TRUE)
  }


  base_url <- "https://api.collegefootballdata.com/lines?"

  full_url <- paste0(base_url,
                     "gameId=", game_id,
                     "&year=", year,
                     "&week=", week,
                     "&seasonType=", season_type,
                     "&team=", team,
                     "&home=", home_team,
                     "&away=", away_team,
                     "&conference=", conference)

  # Check for internet
  check_internet()

  # Create the GET request and set response as res
  res <- GET(full_url)

  # Check the result
  check_status(res)

  # Get the content and return it as data.frame
  df = fromJSON(full_url,flatten=TRUE) %>%
    map_if(is.data.frame,list) %>%
    as_tibble() %>%
    unnest(.data$lines)

  df <- as.data.frame(df)
  return(df)

}
