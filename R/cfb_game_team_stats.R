#' Get Team Statistics by Game
#'
#' @param year (*Integer* required): Year, 4 digit format (*YYYY*)
#' @param week (*Integer* optional): Week - values range from 1-15, 1-14 for seasons pre-playoff, i.e. 2013 or earlier
#' @param season_type (*String* default: regular): Select Season Type - regular, postseason, or both
#' @param team (*String* optional): D-I Team
#' @param conference (*String* optional): Conference abbreviation - Select a valid FBS conference\cr
#' Conference abbreviations P5: ACC, B12, B1G, SEC, PAC\cr
#' Conference abbreviations G5 and FBS Independents: CUSA, MAC, MWC, Ind, SBC, AAC\cr
#' @param game_id (*Integer* optional): Game ID filter for querying a single game\cr
#' Can be found using the [cfbscrapR::cfb_game_info()] function
#' @param rows_per_team (*Integer* default 1): Both Teams for each game on one or two row(s), Options: 1 or 2
#'
#' @return A data frame with 78 variables:
#' \describe{
#'   \item{`game_id`}{integer.}
#'   \item{`school`}{character.}
#'   \item{`conference`}{character.}
#'   \item{`home_away`}{character.}
#'   \item{`points`}{integer.}
#'   \item{`total_yards`}{character.}
#'   \item{`net_passing_yards`}{character.}
#'   \item{`completion_attempts`}{character.}
#'   \item{`passing_tds`}{character.}
#'   \item{`yards_per_pass`}{character.}
#'   \item{`passes_intercepted`}{character.}
#'   \item{`interception_yards`}{character.}
#'   \item{`interception_tds`}{character.}
#'   \item{`rushing_attempts`}{character.}
#'   \item{`rushing_yards`}{character.}
#'   \item{`rush_tds`}{character.}
#'   \item{`yards_per_rush_attempt`}{character.}
#'   \item{`first_downs`}{character.}
#'   \item{`third_down_eff`}{character.}
#'   \item{`fourth_down_eff`}{character.}
#'   \item{`punt_returns`}{character.}
#'   \item{`punt_return_yards`}{character.}
#'   \item{`punt_return_tds`}{character.}
#'   \item{`kick_return_yards`}{character.}
#'   \item{`kick_return_tds`}{character.}
#'   \item{`kick_returns`}{character.}
#'   \item{`kicking_points`}{character.}
#'   \item{`fumbles_recovered`}{character.}
#'   \item{`fumbles_lost`}{character.}
#'   \item{`total_fumbles`}{character.}
#'   \item{`tackles`}{character.}
#'   \item{`tackles_for_loss`}{character.}
#'   \item{`sacks`}{character.}
#'   \item{`qb_hurries`}{character.}
#'   \item{`interceptions`}{character.}
#'   \item{`passes_deflected`}{character.}
#'   \item{`turnovers`}{character.}
#'   \item{`defensive_tds`}{character.}
#'   \item{`total_penalties_yards`}{character.}
#'   \item{`possession_time`}{character.}
#'   \item{`conference_allowed`}{character.}
#'   \item{`home_away_allowed`}{character.}
#'   \item{`points_allowed`}{integer.}
#'   \item{`total_yards_allowed`}{character.}
#'   \item{`net_passing_yards_allowed`}{character.}
#'   \item{`completion_attempts_allowed`}{character.}
#'   \item{`passing_tds_allowed`}{character.}
#'   \item{`yards_per_pass_allowed`}{character.}
#'   \item{`passes_intercepted_allowed`}{character.}
#'   \item{`interception_yards_allowed`}{character.}
#'   \item{`interception_tds_allowed`}{character.}
#'   \item{`rushing_attempts_allowed`}{character.}
#'   \item{`rushing_yards_allowed`}{character.}
#'   \item{`rush_tds_allowed`}{character.}
#'   \item{`yards_per_rush_attempt_allowed`}{character.}
#'   \item{`first_downs_allowed`}{character.}
#'   \item{`third_down_eff_allowed`}{character.}
#'   \item{`fourth_down_eff_allowed`}{character.}
#'   \item{`punt_returns_allowed`}{character.}
#'   \item{`punt_return_yards_allowed`}{character.}
#'   \item{`punt_return_tds_allowed`}{character.}
#'   \item{`kick_return_yards_allowed`}{character.}
#'   \item{`kick_return_tds_allowed`}{character.}
#'   \item{`kick_returns_allowed`}{character.}
#'   \item{`kicking_points_allowed`}{character.}
#'   \item{`fumbles_recovered_allowed`}{character.}
#'   \item{`fumbles_lost_allowed`}{character.}
#'   \item{`total_fumbles_allowed`}{character.}
#'   \item{`tackles_allowed`}{character.}
#'   \item{`tackles_for_loss_allowed`}{character.}
#'   \item{`sacks_allowed`}{character.}
#'   \item{`qb_hurries_allowed`}{character.}
#'   \item{`interceptions_allowed`}{character.}
#'   \item{`passes_deflected_allowed`}{character.}
#'   \item{`turnovers_allowed`}{character.}
#'   \item{`defensive_tds_allowed`}{character.}
#'   \item{`total_penalties_yards_allowed`}{character.}
#'   \item{`possession_time_allowed`}{character.}
#' }
#' @source <https://api.collegefootballdata.com/games/teams>
#' @keywords Team Game Stats
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET
#' @importFrom utils URLencode URLdecode
#' @importFrom assertthat assert_that
#' @importFrom janitor clean_names
#' @importFrom glue glue
#' @import dplyr
#' @import tidyr
#' @import purrr
#' @export
#' @examples
#'
#' cfb_game_team_stats(2019, team = 'LSU')
#'
#' cfb_game_team_stats(2013, team = "Florida State")
#'

cfb_game_team_stats <- function(year,
                                week = NULL,
                                season_type = 'regular',
                                team = NULL,
                                conference = NULL,
                                game_id = NULL,
                                rows_per_team = 1) {

  # Check if year is numeric
  assertthat::assert_that(is.numeric(year) & nchar(year) == 4,
              msg = 'Enter valid year (Integer): 4-digit (YYYY)')

  if(!is.null(week)){
    # Check if week is numeric, if not NULL
    assertthat::assert_that(is.numeric(week) & nchar(week) <= 2,
                msg = 'Enter valid week (Integer): 1-15\n(14 for seasons pre-playoff, i.e. 2014 or earlier)')
  }
  if(season_type != 'regular'){
    # Check if season_type is appropriate, if not NULL
    assertthat::assert_that(season_type %in% c('postseason','both'),
                msg = 'Enter valid season_type (String): regular, postseason, or both')
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
  if(!is.null(game_id)){
    # Check if game_id is numeric, if not NULL
    assertthat::assert_that(is.numeric(game_id),
                msg = 'Enter valid game_id value (Integer)\nCan be found using the `cfb_game_info()` function')
  }
  if(rows_per_team != 1){
    # Check if rows_per_team is 2, if not 1
    assertthat::assert_that(rows_per_team == 2,
                msg = 'Enter valid rows_per_team value (Integer): 1 or 2')
  }

  base_url <- "https://api.collegefootballdata.com/games/teams?"

  full_url <- paste0(base_url,
                     "year=", year,
                     "&week=", week,
                     "&seasonType=", season_type,
                     "&team=", team,
                     "&conference=", conference,
                     "&gameId=", game_id)

  # Check for internet
  check_internet()

  # Create the GET request and set response as res
  res <- httr::GET(full_url)

  # Check the result
  check_status(res)
  
  
  cols <- c("id", "school", "conference", "home_away",                     
            "points", "rushing_t_ds", "punt_return_yards","punt_return_t_ds", 
            "punt_returns", "passing_t_ds", "kicking_points",
            "interception_yards", "interception_t_ds", "passes_intercepted", 
            "fumbles_recovered", "total_fumbles", "tackles_for_loss", 
            "defensive_t_ds", "tackles", "sacks", "qb_hurries", 
            "passes_deflected", "possession_time", "interceptions", 
            "fumbles_lost", "turnovers", "total_penalties_yards", 
            "yards_per_rush_attempt", "rushing_attempts", "rushing_yards", 
            "yards_per_pass", "completion_attempts", "net_passing_yards", 
            "total_yards", "fourth_down_eff", "third_down_eff", 
            "first_downs", "kick_return_yards", "kick_return_t_ds", 
            "kick_returns")
  # Get the content, unnest, and return result as data.frame
  df = fromJSON(full_url, flatten=TRUE) %>%
    map_if(is.data.frame,list) %>%
    as_tibble() 
  
  if(nrow(df)==0){
    warning("Most likely a bye week, the data pulled from the API was empty. Returning nothing
            for this one week or team.")
    return(NULL)
  }
  df = df %>%
    unnest(.data$teams) %>%
    unnest(.data$stats)
  
  # Pivot category columns to get stats for each team game on one row
  df <- pivot_wider(df,
                    names_from = .data$category,
                    values_from = .data$stat)
  df <- df %>% 
    janitor::clean_names()
  df[cols[!(cols %in% colnames(df))]] = NA
  df <- df %>% 
    rename(
      game_id = .data$id,
      rush_tds = .data$rushing_t_ds,
      punt_return_tds = .data$punt_return_t_ds,
      passing_tds = .data$passing_t_ds,
      interception_tds = .data$interception_t_ds,
      defensive_tds = .data$defensive_t_ds,
      kick_return_tds = .data$kick_return_t_ds
    )
  
  if(rows_per_team == 1){
    # Join pivoted data with itself to get ultra-wide row
    # containing all game stats on one row for both teams
    df <- df %>%
      left_join(df,
                by= c('game_id','school'),
                suffix = c('', '_allowed'))
    
    cols1 <- c("game_id", "school",  "conference", "home_away",                    
               "points", "total_yards", "net_passing_yards", 
               "completion_attempts","passing_tds","yards_per_pass",
               "passes_intercepted","interception_yards", "interception_tds", 
               "rushing_attempts", "rushing_yards","rush_tds", "yards_per_rush_attempt",
               "first_downs", "third_down_eff", "fourth_down_eff",
               "punt_returns", "punt_return_yards", "punt_return_tds",               
               "kick_return_yards", "kick_return_tds", "kick_returns", "kicking_points",    
               "fumbles_recovered","fumbles_lost", "total_fumbles",                 
               "tackles", "tackles_for_loss", "sacks", "qb_hurries",  
               "interceptions", "passes_deflected", "turnovers","defensive_tds", 
               "total_penalties_yards", "possession_time",
               "conference_allowed", "home_away_allowed",                    
               "points_allowed", "total_yards_allowed", "net_passing_yards_allowed", 
               "completion_attempts_allowed","passing_tds_allowed","yards_per_pass_allowed",
               "passes_intercepted_allowed","interception_yards_allowed", "interception_tds_allowed", 
               "rushing_attempts_allowed", "rushing_yards_allowed","rush_tds_allowed", "yards_per_rush_attempt_allowed",
               "first_downs_allowed", "third_down_eff_allowed", "fourth_down_eff_allowed",
               "punt_returns_allowed", "punt_return_yards_allowed", "punt_return_tds_allowed",               
               "kick_return_yards_allowed", "kick_return_tds_allowed", "kick_returns_allowed", "kicking_points_allowed",    
               "fumbles_recovered_allowed","fumbles_lost_allowed", "total_fumbles_allowed",                 
               "tackles_allowed", "tackles_for_loss_allowed", "sacks_allowed", "qb_hurries_allowed",  
               "interceptions_allowed", "passes_deflected_allowed", "turnovers_allowed","defensive_tds_allowed", 
               "total_penalties_yards_allowed", "possession_time_allowed")
    
    if(!is.null(team)){
      
      team <- URLdecode(team)
      
      df <- df %>%
        filter(.data$school == team) %>% 
        select(cols1)
      
      return(df)
    } else if(!is.null(conference)){
      
      confs <- cfb_conferences()
      
      conference = URLdecode(conference)
      
      conf_name <- confs[confs$abbreviation == conference,]$name
      
      df <- df %>%
        filter(conference == conf_name) %>% 
        select(cols1)
      
      return(df)
    } else{
      df<-df %>% 
        select(cols1)
      return(df)
    }
  } else{
    cols2 <- c(
      "game_id", "school",  "conference", "home_away",                    
      "points", "total_yards", "net_passing_yards", 
      "completion_attempts","passing_tds","yards_per_pass",
      "passes_intercepted","interception_yards", "interception_tds", 
      "rushing_attempts", "rushing_yards","rush_tds", "yards_per_rush_attempt",
      "first_downs", "third_down_eff", "fourth_down_eff",
      "punt_returns", "punt_return_yards", "punt_return_tds",               
      "kick_return_yards", "kick_return_tds", "kick_returns", "kicking_points",    
      "fumbles_recovered","fumbles_lost", "total_fumbles",                 
      "tackles", "tackles_for_loss", "sacks", "qb_hurries",  
      "interceptions", "passes_deflected", "turnovers","defensive_tds", 
      "total_penalties_yards", "possession_time"
    )
    if(!is.null(team)){
      
      team <- URLdecode(team <- team)
      
      df <- df %>%
        filter(.data$school == team) %>% 
        select(cols2)
      return(df)
    } else if(!is.null(conference)){
      
      confs <- cfb_conferences()
      
      conference = URLdecode(conference)
      
      conf_name <- confs[confs$abbreviation == conference,]$name
      
      df <- df %>%
        filter(conference == conf_name) %>% 
        select(cols2)
      
      return(df)
    } else{
      df <- df %>% 
        select(cols2)
      return(df)
    }
  }
}
