#' Get Season Statistics by Player
#'
#' @param year (\emph{Integer} required): Year, 4 digit format (\emph{YYYY})
#' @param season_type (\emph{String} default: regular): Select Season Type - regular, postseason, or both
#' @param team (\emph{String} optional): D-I Team
#' @param conference (\emph{String} optional): Conference abbreviation - Select a valid FBS conference\cr
#' Conference abbreviations P5: ACC, B12, B1G, SEC, PAC\cr
#' Conference abbreviations G5 and FBS Independents: CUSA, MAC, MWC, Ind, SBC, AAC\cr
#' @param start_week (\emph{Integer} optional): Starting Week - values range from 1-15, 1-14 for seasons pre-playoff, i.e. 2013 or earlier
#' @param end_week (\emph{Integer} optional): Ending Week - values range fom 1-15, 1-14 for seasons pre-playoff, i.e. 2013 or earlier
#' @param season_type (\emph{String} default both): Select Season Type: regular, postseason, or both.
#' @param category (\emph{String} optional): Category filter (e.g defensive)\cr
#' Offense: passing, receiving, rushing\cr
#' Defense: defensive, fumbles, interceptions\cr
#' Special Teams: punting, puntReturns, kicking, kickReturns\cr
#' 
#' @keywords Player Season Stats
#' @importFrom jsonlite "fromJSON"
#' @importFrom httr "GET"
#' @importFrom utils "URLencode" "URLdecode"
#' @importFrom assertthat "assert_that"
#' @import dplyr
#' @import tidyr
#' @import purrr
#' @export
#' @examples
#'
#' cfb_stats_season_player(year=2018, conference = 'B12', start_week = 1,end_week = 7)
#'
#' cfb_stats_season_player(2019, team = 'LSU', category = 'passing')
#'
#' cfb_stats_season_player(2013, team = "Florida State", category = 'passing')
#'

cfb_stats_season_player <- function(year,
                                    season_type = 'regular',
                                    team = NULL,
                                    conference = NULL,
                                    start_week = NULL,
                                    end_week = NULL,
                                    category = NULL) {
  
  stat_categories <- c('passing', 'receiving', 'rushing', 'defensive', 'fumbles',
                       'interceptions', 'punting', 'puntReturns', 'kicking', 'kickReturns')
  
  # Check if year is numeric
  assert_that(is.numeric(year) & nchar(year) == 4,
              msg='Enter valid year (Integer): 4-digit (YYYY)')
  
  if(season_type != 'regular'){
    # Check if season_type is appropriate, if not NULL
    assert_that(season_type %in% c('postseason','both'),
                msg='Enter valid season_type (String): regular, postseason, or both')
  }
  if(!is.null(team)){
    # Encode team parameter for URL, if not NULL
    team = URLencode(team, reserved = TRUE)
  }
  if(!is.null(conference)){
    # Check conference parameter in conference abbreviations, if not NULL
    assert_that(conference %in% cfbscrapR::cfb_conf_types_df$abbreviation,
                msg = "Incorrect conference abbreviation, potential misspelling.\nConference abbreviations P5: ACC, B12, B1G, SEC, PAC\nConference abbreviations G5 and Independents: CUSA, MAC, MWC, Ind, SBC, AAC")
    # Encode conference parameter for URL, if not NULL
    conference = URLencode(conference, reserved = TRUE)
  }
  
  if(!is.null(start_week)){
    # Check if start_week is numeric, if not NULL
    assert_that(is.numeric(start_week) & nchar(start_week) <= 2,
                msg='Enter valid start_week (Integer): 1-15\n(14 for seasons pre-playoff, i.e. 2014 or earlier)')
  }
  if(!is.null(end_week)){
    # Check if week is numeric, if not NULL
    assert_that(is.numeric(end_week) & nchar(end_week) <= 2,
                msg='Enter valid end_week (Integer): 1-15\n(14 for seasons pre-playoff, i.e. 2014 or earlier)')
  }
  if(!is.null(start_week)&!is.null(end_week)){
    assert_that(start_week<=end_week,
                msg='Enter valid start_week, end_week range')
  }
  if(!is.null(category)){
    # Check category parameter in category if not NULL
    assert_that(category %in% stat_categories,
                msg = "Incorrect category, potential misspelling.\nOffense: passing, receiving, rushing\nDefense: defensive, fumbles, interceptions\nSpecial Teams: punting, puntReturns, kicking, kickReturns")
    # Encode conference parameter for URL, if not NULL
    category = URLencode(category, reserved = TRUE)
  }
  
  
  base_url <- "https://api.collegefootballdata.com/stats/player/season?"
  
  full_url <- paste0(base_url,
                     "year=", year,
                     "&seasonType=", season_type,
                     "&startWeek=", start_week,
                     "&endWeek=", end_week,
                     "&team=", team,
                     "&conference=", conference,
                     "&category=",category)
  
  # Check for internet
  check_internet()
  
  # Create the GET request and set response as res
  res <- GET(full_url)
  
  # Check the result
  check_status(res)
  
  # Get the content and return result as data.frame
  df = fromJSON(full_url) 
  
  df = df %>%
    mutate(
      statType = case_when(.data$statType =='In 20' ~ 'In20',
                           .data$statType =='QB Hur' ~ 'QBHUR',
                           TRUE ~ .data$statType),
      statType = paste0(.data$category,'_',.data$statType)) %>% 
    pivot_wider(names_from=.data$statType,
                values_from=.data$stat) %>% 
    rename(athlete_id = .data$playerId) %>% 
    select(.data$athlete_id, .data$player, .data$team, .data$conference, .data$category,
           starts_with('passing'),
           starts_with('receiving'),
           starts_with('rushing'),
           starts_with('fumbles'),
           starts_with('kicking'),
           starts_with('kickReturns'),
           starts_with('punting'),
           starts_with('puntReturns'),
           starts_with('defense'),
           starts_with('interceptions')) %>% 
    arrange(match(.data$category,c('passing','receiving','rushing','fumbles',
                             'kicking','kickReturns','punting','puntReturns',
                             'defensive','interceptions')))
  
  return(df)
}
  