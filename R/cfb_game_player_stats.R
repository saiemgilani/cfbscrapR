#' Get results information from games
#'
#' @param year (\emph{Integer} required): Year, 4 digit format(\emph{YYYY})
#' @param week (\emph{Integer} optional): Week - values from 1-15, 1-14 for seasons pre-playoff (i.e. 2013 or earlier)
#' @param season_type (\emph{String} default regular): Select Season Type: regular or postseason
#' @param team (\emph{String} optional): D-I Team
#' @param category (\emph{String} optional): Category filter (e.g defensive)\cr
#' Offense: passing, receiving, rushing\cr
#' Defense: defensive, fumbles, interceptions\cr
#' Special Teams: punting, puntReturns, kicking, kickReturns\cr
#' @param conference (\emph{String} optional): Conference abbreviation - Select a valid FBS conference\cr
#' Conference abbreviations P5: ACC, B12, B1G, SEC, PAC\cr
#' Conference abbreviations G5 and FBS Independents: CUSA, MAC, MWC, Ind, SBC, AAC\cr
#' @param game_id (\emph{Integer} optional): Game ID filter for querying a single game
#' Can be found using the `cfb_game_info()` function
#'
#' @keywords Game Info
#' @importFrom jsonlite "fromJSON"
#' @importFrom httr "GET"
#' @importFrom utils "URLencode" "URLdecode"
#' @importFrom assertthat "assert_that"
#' @import dplyr
#' @import tidyr
#' @import purrr
#' @export
#'
#' @examples
#'
#' cfb_game_player_stats(2018, week = 15, conference = 'Ind')
#'
#' cfb_game_player_stats(2013, week=1, team = "Florida State", category = 'passing')
#'

cfb_game_player_stats<- function(year,
                          week = NULL,
                          season_type = 'regular',
                          team = NULL,
                          conference = NULL,
                          category = NULL,
                          game_id = NULL) {

  stat_categories <- c('passing', 'receiving', 'rushing', 'defensive', 'fumbles',
                       'interceptions', 'punting', 'puntReturns', 'kicking', 'kickReturns')

  # Check if year is numeric
  assert_that(is.numeric(year) & nchar(year) == 4,
              msg='Enter valid year as a number (YYYY)')
  if(!is.null(week)){
    # Check if week is numeric, if not NULL
    assert_that(is.numeric(week) & nchar(week) <= 2,
                msg='Enter valid week 1-15\n(14 for seasons pre-playoff, i.e. 2014 or earlier)')
  }
  if(season_type != 'regular'){
    # Check if season_type is appropriate, if not regular
    assert_that(season_type %in% c('postseason'),
                msg='Enter valid season_type: regular, postseason')
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
  if(!is.null(category)){
    # Check category parameter in category if not NULL
    assert_that(category %in% stat_categories,
                msg = "Incorrect category, potential misspelling.\nOffense: passing, receiving, rushing\nDefense: defensive, fumbles, interceptions\nSpecial Teams: punting, puntReturns, kicking, kickReturns")
    # Encode conference parameter for URL, if not NULL
    category = URLencode(category, reserved = TRUE)
  }
  if(!is.null(game_id)){
    # Check if game_id is numeric, if not NULL
    assert_that(is.numeric(game_id),
                msg='Enter valid game_id (numeric value)')
  }

  base_url <- "https://api.collegefootballdata.com/games/players?"

  full_url <- paste0(base_url,
                     "year=", year,
                     "&week=", week,
                     "&seasonType=", season_type,
                     "&team=", team,
                     "&conference=", conference,
                     "&category=", category,
                     "&id=", game_id)

  # Check for internet
  check_internet()

  # Create the GET request and set response as res
  res <- GET(full_url)

  # Check the result
  check_status(res)

  # Get the content and return it as data.frame
  df = fromJSON(full_url)

  # Get the content, unnest, and return result as data.frame
  df = fromJSON(full_url,flatten=TRUE) %>%
    map_if(is.data.frame,list) %>%
    as_tibble() %>%
    rename(game_id = .data$id) %>%
    unnest(.data$teams) %>%
    map_if(is.data.frame,list) %>%
    as_tibble() %>%
    unnest(.data$categories) %>%
    map_if(is.data.frame,list) %>%
    as_tibble() %>%
    rename(category = .data$name) %>%
    unnest(.data$types) %>%
    map_if(is.data.frame,list) %>%
    as_tibble() %>%
    rename(stat_category = .data$name) %>%
    unnest(.data$athletes) %>%
    rename(athlete_id = .data$id,
           value = .data$stat)
  df = as.data.frame(df)
  return(df)
}
