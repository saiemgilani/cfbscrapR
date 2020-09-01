#' Get Betting information from games
#'
#' @param game_id (\emph{Integer} optional): Game ID filter for querying a single game
#' Can be found using the \code{\link[cfbscrapR:cfb_game_info]{cfbscrapR::cfb_game_info()}} function
#' @param year (\emph{Integer} required): Year, 4 digit format(\emph{YYYY})
#' @param week (\emph{Integer} optional): Week - values from 1-15, 1-14 for seasons pre-playoff (i.e. 2013 or earlier)
#' @param season_type (\emph{String} default regular): Select Season Type: regular or postseason
#' @param team (\emph{String} optional): D-I Team
#' @param home_team (\emph{String} optional): Home D-I Team
#' @param away_team (\emph{String} optional): Away D-I Team
#' @param conference (\emph{String} optional): Conference abbreviation - Select a valid FBS conference\cr
#' Conference abbreviations P5: ACC, B12, B1G, SEC, PAC\cr
#' Conference abbreviations G5 and FBS Independents: CUSA, MAC, MWC, Ind, SBC, AAC\cr
#' @param line_provider (\emph{String} optional): Select Line Provider - Caesars, consensus, numberfire, or teamrankings
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
#' cfb_betting_lines(year = 2018, week = 12, team = 'Florida State')
#'
#' #7 OTs LSU at TAMU
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
                              conference = NULL,
                              line_provider = NULL) {
  
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
  
  if(!is.null(line_provider)){
    # Check line_provider parameter is a valid entry
    assert_that(line_provider %in% c("Caesars", "consensus", "numberfire", "teamrankings"),
                msg = "Enter valid line provider: Caesars, consensus, numberfire, or teamrankings")
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
  
  if(!is.null(line_provider)){
    if(is.list(df) & length(df)==0){
      df <- data.frame(game_id = game_id, spread = 0, formatted_spread = "home 0")
      return(df)
    }
    else if(!is.null(df$provider)){
      df <- df %>% 
        filter(.data$provider == line_provider) %>% 
        rename(
          game_id = .data$id,
          season_type = .data$seasonType,
          home_team = .data$homeTeam,
          home_conference = .data$homeConference,
          home_score = .data$homeScore,
          away_team = .data$awayTeam,
          away_conference = .data$awayConference,
          away_score = .data$awayScore,
          formatted_spread = .data$formattedSpread,
          over_under = .data$overUnder
        ) %>% as.data.frame()
      return(df)
    }
    else{
      df <- data.frame(game_id = game_id, spread = 0, formatted_spread = "home 0")
      return(df)
    }
  }
  if(is.list(df) & length(df)==0){
    df <- data.frame(game_id = game_id, spread = 0, formatted_spread = "home 0")
    return(df)
  }else{
    df <- df %>% 
      rename(
        game_id = .data$id,
        season_type = .data$seasonType,
        home_team = .data$homeTeam,
        home_conference = .data$homeConference,
        home_score = .data$homeScore,
        away_team = .data$awayTeam,
        away_conference = .data$awayConference,
        away_score = .data$awayScore,
        formatted_spread = .data$formattedSpread,
        over_under = .data$overUnder
      )
    df <- as.data.frame(df)
    return(df)
  }
}