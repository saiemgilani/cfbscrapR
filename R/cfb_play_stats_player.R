#' Gets player info associated by play
#'
#' Information describes the players involved in the play
#' this includes passer, receiver, defensive players who
#' create sacks or picks, etc
#'
#' @param year (*Integer* optional): Year, 4 digit format (*YYYY*)
#' @param week (*Integer* optional): Week - values from 1-15, 1-14 for seasons pre-playoff, i.e. 2013 or earlier
#' @param team (*String* optional): D-I Team
#' @param game_id (*Integer* optional): Game ID filter for querying a single game\cr
#' Can be found using the [cfbscrapR::cfb_game_info()] function
#' @param athlete_id (*Integer* optional): Athlete ID filter for querying a single athlete\cr
#' Can be found using the [cfbscrapR::cfb_player_info()] function.
#' @param stat_type_id (*Integer* optional): Stat Type ID filter for querying a single stat type\cr
#' Can be found using the [cfbscrapR::cfb_play_stats_types()] function
#' @param season_type (*String* default regular): Select Season Type: regular, postseason, or both
#'
#' @return A data frame with 54 variables:
#' \describe{
#'   \item{`play_id`}{character.}
#'   \item{`game_id`}{integer.}
#'   \item{`season`}{integer.}
#'   \item{`week`}{integer.}
#'   \item{`opponent`}{character.}
#'   \item{`team_score`}{integer.}
#'   \item{`opponent_score`}{integer.}
#'   \item{`drive_id`}{character.}
#'   \item{`period`}{integer.}
#'   \item{`yards_to_goal`}{integer.}
#'   \item{`down`}{integer.}
#'   \item{`distance`}{integer.}
#'   \item{`reception_player_id`}{character.}
#'   \item{`reception_player`}{character.}
#'   \item{`reception_yds`}{integer.}
#'   \item{`completion_player_id`}{character.}
#'   \item{`completion_player`}{character.}
#'   \item{`completion_yds`}{integer.}
#'   \item{`rush_player_id`}{character.}
#'   \item{`rush_player`}{character.}
#'   \item{`rush_yds`}{integer.}
#'   \item{`interception_player_id`}{character.}
#'   \item{`interception_player`}{character.}
#'   \item{`interception_stat`}{integer.}
#'   \item{`interception_thrown_player_id`}{character.}
#'   \item{`interception_thrown_player`}{character.}
#'   \item{`interception_thrown_stat`}{integer.}
#'   \item{`touchdown_player_id`}{character.}
#'   \item{`touchdown_player`}{character.}
#'   \item{`touchdown_stat`}{integer.}
#'   \item{`incompletion_player_id`}{character.}
#'   \item{`incompletion_player`}{character.}
#'   \item{`incompletion_stat`}{integer.}
#'   \item{`target_player_id`}{character.}
#'   \item{`target_player`}{character.}
#'   \item{`target_stat`}{integer.}
#'   \item{`fumble_recovered_player_id`}{logical.}
#'   \item{`fumble_recovered_player`}{logical.}
#'   \item{`fumble_recovered_stat`}{logical.}
#'   \item{`fumble_forced_player_id`}{logical.}
#'   \item{`fumble_forced_player`}{logical.}
#'   \item{`fumble_forced_stat`}{logical.}
#'   \item{`fumble_player_id`}{logical.}
#'   \item{`fumble_player`}{logical.}
#'   \item{`fumble_stat`}{logical.}
#'   \item{`sack_player_id`}{character.}
#'   \item{`sack_player`}{character.}
#'   \item{`sack_stat`}{integer.}
#'   \item{`sack_taken_player_id`}{character.}
#'   \item{`sack_taken_player`}{character.}
#'   \item{`sack_taken_stat`}{integer.}
#'   \item{`pass_breakup_player_id`}{logical.}
#'   \item{`pass_breakup_player`}{logical.}
#'   \item{`pass_breakup_stat`}{logical.}
#' }
#' @source <https://api.collegefootballdata.com/play/stats>
#' @keywords Player - PBP
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET
#' @importFrom utils URLencode
#' @importFrom assertthat assert_that
#' @import dplyr
#' @import tidyr
#' @import purrr
#' @export
#' @examples 
#' 
#' cfb_play_stats_player(game_id = 401110722)
#' 



cfb_play_stats_player <- function(year = NULL,
                                  week = NULL,
                                  team = NULL,
                                  game_id = NULL,
                                  athlete_id = NULL,
                                  stat_type_id = NULL,
                                  season_type = 'regular'){

  if(!is.null(year)){
    # Check if year is numeric, if not NULL
    assertthat::assert_that(is.numeric(year) & nchar(year) == 4,
                msg = 'Enter valid year (Integer): 4-digit (YYYY)')
  }
  if(!is.null(week)){
    # Check if week is numeric, if not NULL
    assertthat::assert_that(is.numeric(week) & nchar(week) <= 2 & week <= 15,
                msg = 'Enter valid week (Integer): 1-15\n(14 for seasons pre-playoff, i.e. 2014 or earlier)')
  }
  if(!is.null(team)){
    if(team == "San Jose State"){
      team = utils::URLencode(paste0("San Jos","\u00e9", " State"), reserved = TRUE)
    } else{
      # Encode team parameter for URL if not NULL
      team = utils::URLencode(team, reserved = TRUE)
    }
  }
  if(!is.null(game_id)){
    # Check if game_id is numeric, if not NULL
    assertthat::assert_that(is.numeric(game_id),
                msg = 'Enter valid game_id value (Integer)\nCan be found using the `cfb_game_info()` function')
  }
  if(!is.null(athlete_id)){
    # Check if athlete_id is numeric, if not NULL
    assertthat::assert_that(is.numeric(athlete_id),
                msg = 'Enter valid athlete_id value (Integer)\nCan be found using the `cfb_player_info()` function')
  }
  if(!is.null(stat_type_id)){
    # Check if stat_type_id is numeric, if not NULL
    assertthat::assert_that(is.numeric(stat_type_id),
                msg = 'Enter valid stat_type_id value (Integer)\nCan be found using the `cfb_play_stat_types()` function')
  }
  if(season_type != 'regular'){
    # Check if season_type is appropriate, if not NULL
    assertthat::assert_that(season_type %in% c('postseason','both'),
                msg = 'Enter valid season_type (String): regular, postseason, or both')
  }

  base_url <- "https://api.collegefootballdata.com/play/stats?"

  full_url = paste0(base_url,
                    "year=", year,
                    "&week=", week,
                    "&team=", team,
                    "&gameId=", game_id,
                    "&athleteID=", athlete_id,
                    "&statTypeId=", stat_type_id,
                    "&seasonType=", season_type)

  # Check for internet
  check_internet()

  # Create the GET request and set response as res
  res <- httr::GET(full_url)

  # Check the result
  check_status(res)
  
  clean_df <- data.frame()
  tryCatch(
    expr = {
      # Get the content and return it as data.frame
      df = jsonlite::fromJSON(full_url)
    
      cols = c('game_id','season', 'week','opponent','team_score','opponent_score',
               'drive_id', 'play_id', 'period', 'yards_to_goal', 'down', 'distance',
               'athlete_id', 'stat', 
               'reception','completion','rush','interception','interception_thrown',
               'touchdown','incompletion','target','fumble_recovered','fumble_forced',
               'fumble','sack','sack_taken','pass_breakup',
               'reception_player_id', 'reception_player','reception_yds',
               'completion_player_id','completion_player','completion_yds',
               'rush_player_id', 'rush_player',  'rush_yds', 
               'interception_player_id', 'interception_player','interception_stat', 
               'interception_thrown_player_id', 'interception_thrown_player','interception_thrown_stat', 
               'touchdown_player_id', 'touchdown_player', 'touchdown_stat', 
               'incompletion_player_id', 'incompletion_player','incompletion_stat',
               'target_player_id', 'target_player', 'target_stat', 
               'fumble_recovered_player_id',  'fumble_recovered_player', 'fumble_recovered_stat',
               'fumble_forced_player_id', 'fumble_forced_player', 'fumble_forced_stat',
               'fumble_player_id', 'fumble_player', 'fumble_stat', 
               'sack_player_id', 'sack_player', 'sack_stat', 
               'sack_taken_player_id', 'sack_taken_player', 'sack_taken_stat', 
               'pass_breakup_player_id', 'pass_breakup_player', 'pass_breakup_stat')
      
      df_cols = data.frame(matrix(NA, nrow=0, ncol=70))
      
      names(df_cols) <- cols
      
      df = df[!duplicated(df),]
      
      # Supply lists by splicing them into dots:
      coalesce_by_column <- function(df) {
        return(dplyr::coalesce(!!! as.list(df)))
      }
      
      df <- df %>% 
        dplyr::rename(
          game_id = .data$gameId,
          team_score = .data$teamScore,
          opponent_score = .data$opponentScore,
          drive_id = .data$driveId,
          play_id = .data$playId,
          yards_to_goal = .data$yardsToGoal,
          athlete_id = .data$athleteId,
          athlete_name = .data$athleteName,
          stat_type = .data$statType,
          stat = .data$stat
        )
      
      colnames(df) <- sub(' ',"_",tolower(colnames(df)))
      
      clean_df <- df %>% 
        tidyr::pivot_wider(names_from = .data$stat_type,
                    values_from = .data$athlete_name) 
      
      colnames(clean_df) <- sub(' ',"_",tolower(colnames(clean_df)))
      
      clean_df[cols[!(cols %in% colnames(clean_df))]] = NA
    
      clean_df <- clean_df %>%
       dplyr::mutate(
          reception_player = ifelse(!is.na(.data$reception), .data$reception, NA),
          completion_player = ifelse(!is.na(.data$completion), .data$completion, NA),
          rush_player = ifelse(!is.na(.data$rush), .data$rush, NA),
          interception_player = ifelse(!is.na(.data$interception), .data$interception, NA),
          interception_thrown_player = ifelse(!is.na(.data$interception_thrown), .data$interception_thrown, NA),
          touchdown_player = ifelse(!is.na(.data$touchdown), .data$touchdown, NA),
          incompletion_player = ifelse(!is.na(.data$incompletion), .data$incompletion, NA),
          target_player = ifelse(!is.na(.data$target), .data$target, NA),
          fumble_recovered_player = ifelse(!is.na(.data$fumble_recovered), .data$fumble_recovered, NA),
          fumble_forced_player = ifelse(!is.na(.data$fumble_forced), .data$fumble_forced, NA),
          fumble_player = ifelse(!is.na(.data$fumble), .data$fumble, NA),
          sack_player = ifelse(!is.na(.data$sack), .data$sack, NA),
          sack_taken_player = ifelse(!is.na(.data$sack_taken), .data$sack_taken, NA),
          pass_breakup_player = ifelse(!is.na(.data$pass_breakup), .data$pass_breakup, NA),
          reception_yds = ifelse(!is.na(.data$reception), .data$stat, NA),
          completion_yds = ifelse(!is.na(.data$completion), .data$stat, NA),
          rush_yds = ifelse(!is.na(.data$rush), .data$stat, NA),
          interception_stat = ifelse(!is.na(.data$interception), .data$stat, NA),
          interception_thrown_stat = ifelse(!is.na(.data$interception_thrown), .data$stat, NA),
          touchdown_stat = ifelse(!is.na(.data$touchdown), .data$stat, NA),
          incompletion_stat = ifelse(!is.na(.data$incompletion), .data$stat, NA),
          target_stat = ifelse(!is.na(.data$target), .data$stat, NA),
          fumble_recovered_stat = ifelse(!is.na(.data$fumble_recovered), .data$stat, NA),
          fumble_forced_stat = ifelse(!is.na(.data$fumble_forced), .data$stat, NA),
          fumble_stat = ifelse(!is.na(.data$fumble), .data$stat, NA),
          sack_stat = ifelse(!is.na(.data$sack), .data$stat, NA),
          sack_taken_stat = ifelse(!is.na(.data$sack_taken), .data$stat, NA),
          pass_breakup_stat = ifelse(!is.na(.data$pass_breakup), .data$stat, NA),
          reception_player_id = ifelse(!is.na(.data$reception), .data$athlete_id, NA),
          completion_player_id = ifelse(!is.na(.data$completion), .data$athlete_id, NA),
          rush_player_id = ifelse(!is.na(.data$rush), .data$athlete_id, NA),
          interception_player_id = ifelse(!is.na(.data$interception), .data$athlete_id, NA),
          interception_thrown_player_id = ifelse(!is.na(.data$interception_thrown), .data$athlete_id, NA),
          touchdown_player_id = ifelse(!is.na(.data$touchdown), .data$athlete_id, NA),
          incompletion_player_id = ifelse(!is.na(.data$incompletion), .data$athlete_id, NA),
          target_player_id = ifelse(!is.na(.data$target), .data$athlete_id, NA),
          fumble_recovered_player_id = ifelse(!is.na(.data$fumble_recovered), .data$athlete_id, NA),
          fumble_forced_player_id = ifelse(!is.na(.data$fumble_forced), .data$athlete_id, NA),
          fumble_player_id = ifelse(!is.na(.data$fumble), .data$athlete_id, NA),
          sack_player_id = ifelse(!is.na(.data$sack), .data$athlete_id, NA),
          sack_taken_player_id = ifelse(!is.na(.data$sack_taken), .data$athlete_id, NA),
          pass_breakup_player_id = ifelse(!is.na(.data$pass_breakup), .data$athlete_id, NA)
        ) %>%
        dplyr::select(
          .data$game_id, .data$season, .data$week, 
          .data$opponent, .data$team_score, .data$opponent_score,
          .data$drive_id, .data$play_id, .data$period, 
          .data$yards_to_goal, .data$down, .data$distance,
          .data$reception_player_id,
          .data$reception_player,
          .data$reception_yds,
          .data$completion_player_id,
          .data$completion_player,
          .data$completion_yds,
          .data$rush_player_id,
          .data$rush_player,
          .data$rush_yds,
          .data$interception_player_id,
          .data$interception_player,
          .data$interception_stat,
          .data$interception_thrown_player_id,
          .data$interception_thrown_player,
          .data$interception_thrown_stat,
          .data$touchdown_player_id,
          .data$touchdown_player,
          .data$touchdown_stat,
          .data$incompletion_player_id,
          .data$incompletion_player,
          .data$incompletion_stat,
          .data$target_player_id,
          .data$target_player,
          .data$target_stat,
          .data$fumble_recovered_player_id,
          .data$fumble_recovered_player,
          .data$fumble_recovered_stat,
          .data$fumble_forced_player_id,
          .data$fumble_forced_player,
          .data$fumble_forced_stat,
          .data$fumble_player_id,
          .data$fumble_player,
          .data$fumble_stat,
          .data$sack_player_id,
          .data$sack_player,
          .data$sack_stat,
          .data$sack_taken_player_id,
          .data$sack_taken_player,
          .data$sack_taken_stat,
          .data$pass_breakup_player_id,
          .data$pass_breakup_player,
          .data$pass_breakup_stat) %>% 
        dplyr::group_by(.data$play_id) %>% 
        summarise_all(coalesce_by_column) %>% 
       dplyr::ungroup()
      
      clean_df <- as.data.frame(clean_df)
      message(glue::glue("{Sys.time()}: Scraping play-level player stats data..."))
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments or no play-level player stats data available!"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )
  return(clean_df)
}
