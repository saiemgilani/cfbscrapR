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
#' @param category (\emph{String} optional): Category filter (e.g defensive)\cr
#' Offense: passing, receiving, rushing\cr
#' Defense: defensive, fumbles, interceptions\cr
#' Special Teams: punting, puntReturns, kicking, kickReturns\cr
#' 
#' @return A data frame with 59 variables:
#' \describe{
#'   \item{\code{team}}{character.}
#'   \item{\code{conference}}{character.}
#'   \item{\code{athlete_id}}{character.}
#'   \item{\code{player}}{character.}
#'   \item{\code{category}}{character.}
#'   \item{\code{passing_completions}}{double.}
#'   \item{\code{passing_att}}{double.}
#'   \item{\code{passing_pct}}{double.}
#'   \item{\code{passing_yds}}{double.}
#'   \item{\code{passing_td}}{double.}
#'   \item{\code{passing_int}}{double.}
#'   \item{\code{passing_ypa}}{double.}
#'   \item{\code{rushing_car}}{double.}
#'   \item{\code{rushing_yds}}{double.}
#'   \item{\code{rushing_td}}{double.}
#'   \item{\code{rushing_ypc}}{double.}
#'   \item{\code{rushing_long}}{double.}
#'   \item{\code{receiving_rec}}{double.}
#'   \item{\code{receiving_yds}}{double.}
#'   \item{\code{receiving_td}}{double.}
#'   \item{\code{receiving_ypr}}{double.}
#'   \item{\code{receiving_long}}{double.}
#'   \item{\code{fumbles_fum}}{double.}
#'   \item{\code{fumbles_rec}}{double.}
#'   \item{\code{fumbles_lost}}{double.}
#'   \item{\code{defensive_solo}}{double.}
#'   \item{\code{defensive_tot}}{double.}
#'   \item{\code{defensive_tfl}}{double.}
#'   \item{\code{defensive_sacks}}{double.}
#'   \item{\code{defensive_qb_hur}}{double.}
#'   \item{\code{interceptions_int}}{double.}
#'   \item{\code{interceptions_yds}}{double.}
#'   \item{\code{interceptions_avg}}{double.}
#'   \item{\code{interceptions_td}}{double.}
#'   \item{\code{defensive_pd}}{double.}
#'   \item{\code{defensive_td}}{double.}
#'   \item{\code{kicking_fgm}}{double.}
#'   \item{\code{kicking_fga}}{double.}
#'   \item{\code{kicking_pct}}{double.}
#'   \item{\code{kicking_xpa}}{double.}
#'   \item{\code{kicking_xpm}}{double.}
#'   \item{\code{kicking_pts}}{double.}
#'   \item{\code{kicking_long}}{double.}
#'   \item{\code{kick_returns_no}}{double.}
#'   \item{\code{kick_returns_yds}}{double.}
#'   \item{\code{kick_returns_avg}}{double.}
#'   \item{\code{kick_returns_td}}{double.}
#'   \item{\code{kick_returns_long}}{double.}
#'   \item{\code{punting_no}}{double.}
#'   \item{\code{punting_yds}}{double.}
#'   \item{\code{punting_ypp}}{double.}
#'   \item{\code{punting_long}}{double.}
#'   \item{\code{punting_in_20}}{double.}
#'   \item{\code{punting_tb}}{double.}
#'   \item{\code{punt_returns_no}}{double.}
#'   \item{\code{punt_returns_yds}}{double.}
#'   \item{\code{punt_returns_avg}}{double.}
#'   \item{\code{punt_returns_td}}{double.}
#'   \item{\code{punt_returns_long}}{double.}
#' }
#' @source \url{https://api.collegefootballdata.com/stats/player/season}
#' @keywords Player Season Stats
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET
#' @importFrom utils URLencode URLdecode
#' @importFrom assertthat assert_that
#' @importFrom janitor clean_names
#' @importFrom glue glue
#' @importFrom dplyr mutate mutate_at rename select
#' @importFrom tidyr pivot_wider everything
#' @export
#' @examples
#'
#' cfb_stats_season_player(year=2018, conference = 'B12', start_week = 1, end_week = 7)
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
  assertthat::assert_that(is.numeric(year) & nchar(year) == 4,
              msg='Enter valid year (Integer): 4-digit (YYYY)')
  
  if(season_type != 'regular'){
    # Check if season_type is appropriate, if not NULL
    assertthat::assert_that(season_type %in% c('postseason','both'),
                msg='Enter valid season_type (String): regular, postseason, or both')
  }
  if(!is.null(team)){
    if(team == "San Jose State"){
      team = utils::URLencode(paste0("San Jos","\u00e9", " State"), reserved = TRUE)
    } else{
      # Encode team parameter for URL if not NULL
      team = utils::URLencode(team, reserved = TRUE)
    }
  }
  if(!is.null(conference)){
    # # Check conference parameter in conference abbreviations, if not NULL
    # assertthat::assert_that(conference %in% cfbscrapR::cfb_conf_types_df$abbreviation,
    #             msg = "Incorrect conference abbreviation, potential misspelling.\nConference abbreviations P5: ACC, B12, B1G, SEC, PAC\nConference abbreviations G5 and Independents: CUSA, MAC, MWC, Ind, SBC, AAC")
    # Encode conference parameter for URL, if not NULL
    conference = utils::URLencode(conference, reserved = TRUE)
  }
  
  if(!is.null(start_week)){
    # Check if start_week is numeric, if not NULL
    assertthat::assert_that(is.numeric(start_week) & nchar(start_week) <= 2,
                msg='Enter valid start_week (Integer): 1-15\n(14 for seasons pre-playoff, i.e. 2014 or earlier)')
  }
  if(!is.null(end_week)){
    # Check if week is numeric, if not NULL
    assertthat::assert_that(is.numeric(end_week) & nchar(end_week) <= 2,
                msg='Enter valid end_week (Integer): 1-15\n(14 for seasons pre-playoff, i.e. 2014 or earlier)')
  }
  if(!is.null(start_week)&!is.null(end_week)){
    assertthat::assert_that(start_week<=end_week,
                msg='Enter valid start_week, end_week range')
  }
  if(!is.null(category)){
    # Check category parameter in category if not NULL
    assertthat::assert_that(category %in% stat_categories,
                msg = "Incorrect category, potential misspelling.\nOffense: passing, receiving, rushing\nDefense: defensive, fumbles, interceptions\nSpecial Teams: punting, puntReturns, kicking, kickReturns")
    # Encode conference parameter for URL, if not NULL
    category = utils::URLencode(category, reserved = TRUE)
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
  res <- httr::GET(full_url)
  
  # Check the result
  check_status(res)
  
  cols <- c("team", "conference", "athlete_id", "player", "category", 
            "passing_completions", "passing_att", "passing_pct", "passing_yds", 
            "passing_td", "passing_int", "passing_ypa", 
            "rushing_car", "rushing_yds", "rushing_td", "rushing_ypc", "rushing_long", 
            "receiving_rec","receiving_yds","receiving_td", "receiving_ypr", "receiving_long",
            "fumbles_fum", "fumbles_rec", "fumbles_lost",  
            "defensive_solo", "defensive_tot", "defensive_tfl", "defensive_sacks", 
            "defensive_qb_hur", "interceptions_int", "interceptions_yds", 
            "interceptions_avg", "interceptions_td", "defensive_pd", "defensive_td",      
            "kicking_fgm", "kicking_fga", "kicking_pct", 
            "kicking_xpa", "kicking_xpm", "kicking_pts",  "kicking_long",
            "kick_returns_no", "kick_returns_yds", "kick_returns_avg",
            "kick_returns_td",  "kick_returns_long",
            "punting_no", "punting_yds", "punting_ypp", 
            "punting_long", "punting_in_20",  "punting_tb", 
            "punt_returns_no", "punt_returns_yds", "punt_returns_avg", 
            "punt_returns_td", "punt_returns_long")
  
  numeric_cols <- c(
    "passing_completions", "passing_att", "passing_pct", "passing_yds", 
    "passing_td", "passing_int", "passing_ypa", 
    "rushing_car", "rushing_yds", "rushing_td", "rushing_ypc", "rushing_long", 
    "receiving_rec","receiving_yds","receiving_td", "receiving_ypr", "receiving_long",
    "fumbles_fum", "fumbles_rec", "fumbles_lost",  
    "defensive_solo", "defensive_tot", "defensive_tfl", "defensive_sacks", 
    "defensive_qb_hur", "interceptions_int", "interceptions_yds", 
    "interceptions_avg", "interceptions_td", "defensive_pd", "defensive_td",      
    "kicking_fgm", "kicking_fga", "kicking_pct", 
    "kicking_xpa", "kicking_xpm", "kicking_pts",  "kicking_long",
    "kick_returns_no", "kick_returns_yds", "kick_returns_avg",
    "kick_returns_td",  "kick_returns_long",
    "punting_no", "punting_yds", "punting_ypp", 
    "punting_long", "punting_in_20",  "punting_tb", 
    "punt_returns_no", "punt_returns_yds", "punt_returns_avg", 
    "punt_returns_td", "punt_returns_long")
        
  
  
  df <- data.frame()
  tryCatch(
    expr ={
      # Get the content and return result as data.frame
      df = jsonlite::fromJSON(full_url) %>%
        dplyr::mutate(
          statType = paste0(.data$category,'_',.data$statType)) %>%
        tidyr::pivot_wider(names_from = .data$statType,
                           values_from = .data$stat) %>% 
        dplyr::rename(athlete_id = .data$playerId) %>% 
        janitor::clean_names()
      
      df[cols[!(cols %in% colnames(df))]] = NA
      
      df <- df %>% 
        dplyr::select(cols, tidyr::everything()) %>% 
        dplyr::mutate_at(numeric_cols, as.numeric) %>% 
        as.data.frame()
      
      message(glue::glue("{Sys.time()}: Scraping season stats - player..."))
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments or no season stats - player data available!"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )
  return(df)
}
  
