#' Extract CFB (D-I) Play by Play Data - For plays
#'
#' Extracts raw game by game data. Data comes from https://api.collegefootballdata.com
#'
#' @param season_type Select Season Type (regular, postseason, both)
#' @param year Select year, (example: 2018)
#' @param week Select week, this is optional (also numeric)
#' @param team Select team name (example: Texas, Texas A&M, Clemson)
#' @param play_type Select play type (example: see the \code{\link[cfbscrapR:cfb_play_type_df]{cfbscrapR::cfb_play_type_df}})
#' @param epa_wpa Logical parameter (TRUE/FALSE) to return the Expected Points Added/Win Probability Added variables
#'
#' @keywords Play-by-Play
#' @import stringr
#' @import dplyr
#' @import tidyr
#' @importFrom jsonlite "fromJSON"
#' @importFrom utils "URLencode"
#' @importFrom utils "globalVariables"
#' @export
#'

cfb_pbp_data <- function(year,
                         season_type = 'regular',
                         week = 1,
                         team = NULL,
                         play_type = NULL,
                         epa_wpa=FALSE) {
  options(stringsAsFactors = FALSE)
  # Check if year is numeric, if not NULL
  assertthat::assert_that(is.numeric(year) & nchar(year) == 4,
                          msg = 'Enter valid year as a number (YYYY)')
  assertthat::assert_that(is.numeric(week) & nchar(week) <= 2,
                          msg = 'Enter valid week 1-15 \n(14 for seasons pre-playoff, i.e. 2014 or earlier)')
  if(!is.null(team)){
    # Encode team parameter for URL if not NULL
    team = utils::URLencode(team, reserved = TRUE)
  }
  if(season_type != 'regular'){
    # Check if season_type is appropriate, if not regular
    assertthat::assert_that(season_type %in% c('postseason'),
                            msg = 'Enter valid season_type: regular or postseason')
  }
  if(!is.null(play_type)){
    text <- play_type %in% cfbscrapR::cfb_play_type_df$text
    abbr <- play_type %in% cfbscrapR::cfb_play_type_df$abbreviation
    pt <- 
      assertthat::assert_that(
        (text | abbr) == TRUE, 
        msg = "Incorrect play type selected, please look at the available options in the Play Type DF.")
    if(text){
      pt_id = cfbscrapR::cfb_play_type_df$id[which(cfbscrapR::cfb_play_type_df$text == play_type)]
    }else{
      pt_id = cfbscrapR::cfb_play_type_df$id[which(cfbscrapR::cfb_play_type_df$abbreviation == play_type)]
    }
  }else{
    pt_id = NULL
  }
  ## Inputs
  ## Year, Week, Team
  
  play_base_url <- "https://api.collegefootballdata.com/plays?"

  full_url <- paste0(play_base_url,
                     "seasonType=", season_type,
                     "&year=", year,
                     "&week=", week,
                     "&team=", team,
                     "&playType=", pt_id)
  
  # Check for internet
  check_internet()

  # # Create the GET request and set response as res
  # res <- httr::GET(full_url)
  # 
  # # Check the result
  # check_status(res)

  raw_play_df <- fromJSON(full_url)
  raw_play_df <- do.call(data.frame, raw_play_df)
  
  if(nrow(raw_play_df)==0){
    warning("Most likely a bye week, the data pulled from the API was empty. Returning nothing
            for this one week or team.")
    return(NULL)
  }
  
  ## call/drive information
  drive_info = cfb_drives(year = year, season_type = season_type, team = team, week = week)

  clean_drive_df = clean_drive_info(drive_info)

  colnames(clean_drive_df) <- paste0("drive_",colnames(clean_drive_df))
    
  play_df = raw_play_df %>%
    dplyr::mutate(drive_id = as.numeric(.data$drive_id)) %>%
    dplyr::left_join(clean_drive_df,
                     by = c("drive_id" = "drive_drive_id",
                            "game_id" = "drive_game_id"),
                     suffix = c("_play", "_drive"))

  rm_cols = c(
    'drive_game_id', 'drive_id_drive',
    'drive_plays', 'drive_end_yardline',
    'drive_offense', 'drive_offense_conference',
    'drive_defense', 'drive_defense_conference',
    'drive_start_time.hours', 'drive_start_time.minutes', 'drive_start_time.seconds',
    'drive_start_period', 'drive_end_period',
    'drive_end_time.hours', 'drive_end_time.minutes', 'drive_end_time.seconds',
    'drive_elapsed.hours', 'drive_elapsed.minutes', 'drive_elapsed.seconds'
  )


  play_df <- play_df %>%
    dplyr::select(setdiff(names(play_df), rm_cols)) %>%
    dplyr::rename(drive_pts = .data$drive_pts_drive,
                  drive_result = .data$drive_drive_result,
                  id_play = .data$id,
                  offense_play = .data$offense,
                  defense_play = .data$defense)
  
  if(epa_wpa){
    if(year<=2005) {
      warning(
        "Data Quality prior to 2005 is not as consistent. This can affect the EPA/WPA values, proceed with caution."
      )
    }
    
    raw_df = clean_pbp_dat(play_df)
    play_df = penalty_detection(raw_df)
    
    g_ids = sort(unique(play_df$game_id))
    play_df = purrr::map_dfr(g_ids,
                             function(x) {
                              play_df %>%
                                dplyr::filter(.data$game_id == x) %>%
                                create_epa() %>%
                                # add_betting_cols(g_id = x, yr=year) %>% 
                                # create_wpa_betting() %>% 
                                create_wpa_naive()
                              })
      
    play_df <- play_df %>% 
      dplyr::select(-.data$drive_drive_number,
                    -.data$play_number) %>% 
      dplyr::select(.data$id_play,
                    .data$game_id,
                    .data$offense_play,
                    .data$defense_play,
                    .data$half,
                    .data$period,
                    .data$clock.minutes,
                    .data$clock.seconds,
                    .data$play_type,
                    .data$play_text,
                    .data$down,
                    .data$distance,
                    .data$yards_to_goal,
                    .data$yards_to_goal_end,
                    .data$yards_gained,
                    .data$TimeSecsRem,
                    .data$offense_score,
                    .data$defense_score,
                    .data$score_diff,
                    .data$score_diff_start,
                    .data$EPA,
                    .data$def_EPA,
                    .data$ep_before,
                    .data$ep_after,
                    .data$score_pts,
                    .data$wpa,
                    .data$wp_before,
                    .data$wp_after,
                    .data$ppa,
                    .data$game_play_number,
                    .data$drive_number,
                    .data$drive_play_number,
                    .data$firstD_by_poss,
                    .data$firstD_by_penalty,
                    .data$firstD_by_yards,
                    .data$Goal_To_Go,
                    .data$Under_two,
                    .data$offense_timeouts,
                    .data$defense_timeouts,
                    .data$change_of_poss,
                    .data$drive_start_yards_to_goal,
                    .data$drive_end_yards_to_goal,
                    .data$drive_yards,
                    .data$drive_scoring,
                    .data$drive_result,
                    .data$drive_pts,
                    .data$home,
                    .data$away,
                    .data$wpa_base,
                    .data$wpa_base_nxt,
                    .data$wpa_change,
                    .data$wpa_change_nxt,
                    .data$wpa_base_ind,
                    .data$wpa_base_nxt_ind,
                    .data$wpa_change_ind,
                    .data$wpa_change_nxt_ind,
                    dplyr::everything())
  }
  play_df <- as.data.frame(play_df)
  
  return(play_df)
}


#' Penalty Detection
#' Adds penalty columns to Play-by-Play data pulled from the API
#'
#' @param raw_df (\emph{data.frame} required): Performs data cleansing on Play-by-Play DataFrame, as pulled from `cfb_pbp_dat()`
#' @keywords internal
#' @import stringr
#' @import dplyr
#' @import tidyr
#' @export
#'

penalty_detection <- function(raw_df) {
  ## penalty detection-----
  #-- 'penalty' in play text----
  pen_text = str_detect(raw_df$play_text, regex("penalty", ignore_case = TRUE))
  #-- 'Declined' in play text----
  pen_declined_text = str_detect(raw_df$play_text, regex("declined", ignore_case = TRUE))
  #-- 'No Play' in play text----
  pen_no_play_text = str_detect(raw_df$play_text, regex("no play", ignore_case = TRUE))
  #-- 'Off-setting' in play text----
  pen_offset_text = str_detect(raw_df$play_text, regex("off-setting", ignore_case = TRUE))
  
  pen_1st_down_text = str_detect(raw_df$play_text, regex("1st down", ignore_case = TRUE))
  
  #-- Penalty play_types
  pen_type = raw_df$play_type == "Penalty" | raw_df$play_type == "penalty"
  
  #-- T/F flag conditions penalty_flag 
  raw_df$penalty_flag = FALSE
  raw_df$penalty_flag[pen_type] <- TRUE
  raw_df$penalty_flag[pen_text] <- TRUE
  #-- T/F flag conditions penalty_declined 
  raw_df$penalty_declined = FALSE
  raw_df$penalty_declined[pen_text & pen_declined_text] <- TRUE
  raw_df$penalty_declined[pen_type & pen_declined_text] <- TRUE
  #-- T/F flag conditions penalty_no_play 
  raw_df$penalty_no_play = FALSE
  raw_df$penalty_no_play[pen_text & pen_no_play_text] <- TRUE
  raw_df$penalty_no_play[pen_type & pen_no_play_text] <- TRUE
  #-- T/F flag conditions penalty_offset 
  raw_df$penalty_offset = FALSE
  raw_df$penalty_offset[pen_text & pen_offset_text] <- TRUE
  raw_df$penalty_offset[pen_type & pen_offset_text] <- TRUE
  #-- T/F flag conditions penalty_1st_conv 
  raw_df$penalty_1st_conv = FALSE
  raw_df$penalty_1st_conv[pen_text & pen_1st_down_text] <- TRUE
  raw_df$penalty_1st_conv[pen_type & pen_1st_down_text] <- TRUE
  #-- T/F flag for penalty text but not penalty play type -- 
  raw_df$penalty_text <- FALSE
  raw_df$penalty_text[pen_text & !pen_type] <- TRUE
  
  ##-- Kickoff down adjustment ----
  raw_df = raw_df %>%
   dplyr::mutate(
     down = ifelse(.data$down == 5 & str_detect(.data$play_type, "Kickoff"), 1, .data$down),
     down = ifelse(.data$down == 5 & str_detect(.data$play_type, "Penalty"), 1, .data$down),
     half = ifelse(.data$period <= 2, 1, 2)) %>% 
   dplyr::filter(
     !(.data$game_id == '302610012' & .data$down == 5 & .data$play_type == 'Rush')
   )
  
  return(raw_df)
}


#' Clean Play-by-Play data
#' Cleans Play-by-Play data pulled from the API's raw game data
#'
#' @param raw_df (\emph{data.frame} required): Performs data cleansing on Play-by-Play DataFrame, as pulled from `cfb_pbp_dat()`
#' @keywords internal
#' @import stringr
#' @import dplyr
#' @import tidyr
#' @export
#'

clean_pbp_dat <- function(raw_df) {
  ##--Play type vectors------
  scores_vec = c(
    "Blocked Field Goal Touchdown",
    "Blocked Punt Touchdown",
    "Missed Field Goal Return Touchdown",
    "Fumble Recovery (Opponent) Touchdown",
    "Fumble Return Touchdown",
    "Interception Return Touchdown",
    "Pass Interception Return Touchdown",
    "Punt Touchdown",
    "Punt Return Touchdown",
    "Sack Touchdown",
    "Uncategorized Touchdown",
    "Defensive 2pt Conversion",
    "Safety",
    "Passing Touchdown",
    "Rushing Touchdown",
    "Field Goal Good",
    "Pass Reception Touchdown",
    "Fumble Recovery (Own) Touchdown"
  )
  turnover_play_type = c(
    "Blocked Field Goal",
    "Blocked Field Goal Touchdown",
    "Blocked Punt",
    "Blocked Punt Touchdown",
    "Field Goal Missed",
    "Missed Field Goal Return",
    "Missed Field Goal Return Touchdown",
    "Fumble Recovery (Opponent)",
    "Fumble Recovery (Opponent) Touchdown",
    "Fumble Return Touchdown",
    "Interception",
    "Interception Return Touchdown",
    "Pass Interception Return",
    "Pass Interception Return Touchdown",
    "Punt",
    "Punt Touchdown",
    "Punt Return Touchdown",
    "Sack Touchdown",
    "Uncategorized Touchdown"
  )
  defense_score_vec = c(
    "Blocked Punt Touchdown",
    "Blocked Field Goal Touchdown",
    "Missed Field Goal Return Touchdown",
    "Punt Return Touchdown",
    "Fumble Recovery (Opponent) Touchdown",
    "Fumble Return Touchdown",
    "Kickoff Return Touchdown",
    "Defensive 2pt Conversion",
    "Safety",
    "Sack Touchdown",
    "Interception Return Touchdown",
    "Pass Interception Return Touchdown",
    "Uncategorized Touchdown"
  )
  turnover_vec = c(
    "Blocked Field Goal",
    "Blocked Field Goal Touchdown",
    "Blocked Punt",
    "Blocked Punt Touchdown",
    "Field Goal Missed",
    "Missed Field Goal Return",
    "Missed Field Goal Return Touchdown",
    "Fumble Recovery (Opponent)",
    "Fumble Recovery (Opponent) Touchdown",
    "Fumble Return Touchdown",
    "Defensive 2pt Conversion",
    "Interception",
    "Interception Return Touchdown",
    "Pass Interception Return",
    "Pass Interception Return Touchdown",
    "Punt Touchdown",
    "Punt Return Touchdown",
    "Sack Touchdown",
    "Uncategorized Touchdown"
  )
  normalplay = c(
    "Rush",
    "Pass",
    "Pass Reception",
    "Pass Incompletion",
    "Pass Completion",
    "Sack",
    "Fumble Recovery (Own)"
  )
  penalty = c('Penalty')
  offense_score_vec = c(
    "Passing Touchdown",
    "Rushing Touchdown",
    "Field Goal Good",
    "Pass Reception Touchdown",
    "Fumble Recovery (Own) Touchdown",
    "Punt Touchdown",
    "Kickoff Touchdown"
  )
  punt_vec = c(
    "Blocked Punt",
    "Blocked Punt Touchdown",
    "Punt",
    "Punt Touchdown",
    "Punt Return Touchdown"
  )
  kickoff_vec = c(
    "Kickoff",
    "Kickoff Return (Offense)",
    "Kickoff Return Touchdown",
    "Kickoff Touchdown"
  )
  
  raw_df <- raw_df %>% 
   dplyr::mutate(
      #-- Touchdowns----
      scoring_play = ifelse(.data$play_type %in% scores_vec, 1, 0),
      pts_scored = case_when(
        .data$play_type == "Blocked Field Goal Touchdown" ~ -7,
        .data$play_type == "Blocked Punt Touchdown" ~ -7,
        .data$play_type == "Missed Field Goal Return Touchdown" ~ -7,
        .data$play_type == "Fumble Recovery (Opponent) Touchdown" ~ -7,
        .data$play_type == "Fumble Return Touchdown" ~ -7,
        .data$play_type == "Interception Return Touchdown" ~ -7,
        .data$play_type == "Pass Interception Return Touchdown" ~ -7,
        .data$play_type == "Punt Touchdown" ~ 7,
        .data$play_type == "Punt Return Touchdown" ~ -7,
        .data$play_type == "Sack Touchdown" ~ -7,
        .data$play_type == "Uncategorized Touchdown" ~ 7,
        .data$play_type == "Defensive 2pt Conversion" ~ -2,
        .data$play_type == "Safety" ~ -2,
        .data$play_type == "Passing Touchdown" ~ 7,
        .data$play_type == "Rushing Touchdown" ~ 7,
        .data$play_type == "Field Goal Good" ~ 3,
        .data$play_type == "Pass Reception Touchdown" ~ 7,
        .data$play_type == "Fumble Recovery (Own) Touchdown" ~ 7,
        TRUE ~ 0),
      td_play = ifelse(str_detect(.data$play_text, regex("touchdown", ignore_case = TRUE)) &
                         !is.na(.data$play_text),
                       1,
                       0), 
      touchdown = ifelse(str_detect(.data$play_type, regex('touchdown', ignore_case = TRUE)) , 1, 0),
      off_td_play = ifelse(.data$play_type %in% offense_score_vec, 1, 0),
      def_td_play = ifelse(.data$play_type %in% defense_score_vec, 1, 0),
      #-- Kicks/Punts----
      kickoff_play = ifelse(.data$play_type %in% kickoff_vec, 1, 0),
      kickoff_tb = ifelse(str_detect(.data$play_text,regex("touchback", ignore_case = TRUE)) &
                            (.data$play_type %in% kickoff_vec) & !is.na(.data$play_text), 1, 0),
      kick_play = ifelse(str_detect(.data$play_text, regex("kick|kickoff", ignore_case = TRUE)) &
                           !is.na(.data$play_text), 1, 0),
      punt_play = ifelse(str_detect(.data$play_text, regex("punt", ignore_case = TRUE)) &
                           !is.na(.data$play_text), 1, 0),
      punt = ifelse(.data$play_type %in% punt_vec, 1, 0),
      punt_tb = ifelse(str_detect(.data$play_text,regex("touchback", ignore_case = TRUE)) &
                         (.data$play_type %in% punt_vec) & !is.na(.data$play_text), 1, 0),
      #-- Fumbles----
      fumble_vec = ifelse(str_detect(.data$play_text, "fumble") & !is.na(.data$play_text), 1, 0),
      #-- Pass/Rush----
      rush_vec = ifelse(
        (.data$play_type == "Rush" & !is.na(.data$play_text)) |
          .data$play_type == "Rushing Touchdown" |
          (
            .data$play_type == "Safety" &
              str_detect(.data$play_text, regex("run", ignore_case = TRUE)) & 
              !is.na(.data$play_text)
          ) |
          (
            .data$play_type == "Fumble Recovery (Opponent)" &
              str_detect(.data$play_text, regex("run", ignore_case = TRUE)) & 
              !is.na(.data$play_text)
          ) |
          (
            .data$play_type == "Fumble Recovery (Opponent) Touchdown" &
              str_detect(.data$play_text, regex("run", ignore_case = TRUE)) & 
              !is.na(.data$play_text)
          ) |
          (
            .data$play_type == "Fumble Recovery (Own)" &
              str_detect(.data$play_text, regex("run", ignore_case = TRUE)) & 
              !is.na(.data$play_text)
          ) |
          (
            .data$play_type == "Fumble Recovery (Own) Touchdown" &
              str_detect(.data$play_text, regex("run", ignore_case = TRUE)) & 
              !is.na(.data$play_text)
          ) |
          (
            .data$play_type == "Fumble Return Touchdown" &
              str_detect(.data$play_text, regex("run", ignore_case = TRUE)) & 
              !is.na(.data$play_text)
          ) |
          (.data$play_type == "Fumble Return Touchdown"),
        1,
        0
      ),
      pass_vec = if_else(
        .data$play_type == "Pass Reception" |
          .data$play_type == "Pass Completion" |
          .data$play_type == "Passing Touchdown" |
          .data$play_type == "Sack" | .data$play_type == "Pass" |
          .data$play_type == "Interception" |
          .data$play_type == "Pass Interception Return" |
          .data$play_type == "Interception Return Touchdown" |
          (.data$play_type == "Pass Incompletion" & !is.na(.data$play_text)) |
          .data$play_type == "Sack Touchdown" |
          (
            .data$play_type == "Safety" &
              str_detect(.data$play_text, regex('sacked', ignore_case = TRUE)) & !is.na(.data$play_text)
          ) |
          (
            .data$play_type == "Fumble Recovery (Own)" &
              str_detect(.data$play_text, regex("pass", ignore_case = TRUE)) & !is.na(.data$play_text)
          ) |
          (
            .data$play_type == "Fumble Recovery (Own)" &
              str_detect(.data$play_text, regex("sacked", ignore_case = TRUE)) & !is.na(.data$play_text)
          ) |
          (
            .data$play_type == "Fumble Recovery (Own) Touchdown" &
              str_detect(.data$play_text, regex("pass", ignore_case = TRUE)) & !is.na(.data$play_text)
          ) |
          (
            .data$play_type == "Fumble Recovery (Opponent)" &
              str_detect(.data$play_text, regex("pass", ignore_case = TRUE)) & !is.na(.data$play_text)
          ) |
          (
            .data$play_type == "Fumble Recovery (Opponent)" &
              str_detect(.data$play_text, regex("sacked", ignore_case = TRUE)) & !is.na(.data$play_text)
          ) |
          (
            .data$play_type == "Fumble Recovery (Opponent) Touchdown" &
              str_detect(.data$play_text, regex("pass", ignore_case = TRUE)) & !is.na(.data$play_text)
          ) |
          (
            .data$play_type == "Fumble Return Touchdown" &
              str_detect(.data$play_text, regex("pass", ignore_case = TRUE))  & !is.na(.data$play_text)
          ),
        1,
        0
      ), 
      #-- Sacks----
      sack_vec = ifelse(
        (.data$play_type %in% c("Sack","Sack Touchdown") |
        (.data$play_type == "Safety" & str_detect(.data$play_text, regex('sacked',ignore_case = TRUE)))) 
        & !is.na(.data$play_text), 1, 0),
      #-- Change of possession via turnover
      turnover_vec = ifelse(.data$play_type %in% turnover_vec, 1, 0),
      #-- Change of possession by lead('offense_play', 1)----
      change_of_poss = ifelse(.data$offense_play == lead(.data$offense_play, order_by = .data$id_play), 0, 1),
      change_of_poss = ifelse(is.na(.data$change_of_poss), 0, .data$change_of_poss),
      ## Fix strip-sacks to fumbles----
      play_type = ifelse(.data$fumble_vec == 1 & .data$sack_vec == 1 & 
                         .data$change_of_poss == 1 & .data$td_play == 0,
                         "Fumble Recovery (Opponent)", .data$play_type),
      play_type = ifelse(.data$fumble_vec == 1 & .data$sack_vec == 1 & .data$td_play == 1, 
                         "Fumble Recovery (Opponent)", .data$play_type),
      ## Portion of touchdown check for plays where touchdown is not listed in the play_type--
      td_check = ifelse(!str_detect(.data$play_type, "Touchdown"), 1, 0),
      #-- Fix kickoff fumble return TDs----
      play_type = ifelse(.data$kick_play == 1 & .data$fumble_vec == 1 & 
                         .data$td_play == 1 & .data$td_check == 1,
                         paste0(.data$play_type, " Touchdown"), 
                         .data$play_type),
      #-- Fix punt return TDs----
      play_type = ifelse(.data$punt_play == 1 & .data$td_play == 1 & .data$td_check == 1,
                         paste0(.data$play_type, " Touchdown"), 
                         .data$play_type),
      #-- Fix rush/pass tds that aren't explicit----
      play_type = ifelse(.data$td_play == 1 & .data$rush_vec == 1,
                         "Rushing Touchdown", 
                         .data$play_type),
      play_type = ifelse(.data$td_play == 1 & .data$pass_vec == 1,
                         "Passing Touchdown", 
                         .data$play_type),
      #-- Fix duplicated TD play_type labels----
      play_type = ifelse(.data$play_type == "Punt Touchdown Touchdown",
                         "Punt Touchdown",
                         .data$play_type),
      play_type = ifelse(.data$play_type == "Fumble Return Touchdown Touchdown",
                         "Fumble Return Touchdown",
                         .data$play_type),
      play_type = ifelse(.data$play_type == "Rushing Touchdown Touchdown",
                         "Rushing Touchdown",
                         .data$play_type),
      #-- Fix Pass Interception Return TD play_type labels----
      play_type = ifelse(str_detect(.data$play_text,"pass intercepted for a TD") & !is.na(.data$play_text),
                         "Interception Return Touchdown", .data$play_type),
      #-- Fix Sack/Fumbles Touchdown play_type labels----
      play_type = ifelse(str_detect(.data$play_text, regex("sacked", ignore_case = TRUE)) & 
                         str_detect(.data$play_text, regex("fumbled", ignore_case = TRUE)) &
                         str_detect(.data$play_text, regex("TD",ignore_case = TRUE)) &
                         !is.na(.data$play_text), 
                         "Sack Touchdown", .data$play_type),
      play_type = ifelse(play_type == "Interception", "Interception Return", play_type),
      play_type = ifelse(play_type == "Pass Interception Return", "Interception Return", play_type)
    ) %>% dplyr::select(-.data$td_check,-.data$td_play)
  return(raw_df)
}


#' Clean Drive Information
#' Cleans CFB (D-I) Drive-By-Drive Data to create `pts_drive` column
#'
#' @param drive_df (\emph{data.frame} required) Drive DataFrame pulled from API
#' @keywords internal
#' @import stringr
#' @import dplyr
#' @import tidyr
#' @export
#'

clean_drive_info <- function(drive_df){

  clean_drive = drive_df %>%
   dplyr::mutate(
      pts_drive = case_when(
        .data$drive_result == "TD" ~ 7,
        str_detect(.data$drive_result,"SF") ~ -2,
        .data$drive_result == 'FG GOOD' ~ 3,
        .data$drive_result == 'FG' ~ 3,
        .data$drive_result == 'MISSED FG TD' ~ -7,
        .data$drive_result == 'KICKOFF RETURN TD' ~ -7,
        .data$drive_result == 'END OF HALF TD' ~ 7,
        .data$drive_result == "END OF GAME TD" ~ 7,
        .data$drive_result == 'PUNT RETURN TD' ~ -7,
        .data$drive_result == 'PUNT TD' ~ -7,
        .data$drive_result == 'INT TD' ~ -7,
        .data$drive_result == 'INT RETURN TOUCH' ~ -7,
        .data$drive_result == 'FUMBLE RETURN TD' ~ -7,
        .data$drive_result == 'FUMBLE TD' ~ -7,
        .data$drive_result == 'DOWNS TD' ~ -7,
        str_detect(.data$drive_result,"TD") ~ 7,
        TRUE ~ 0),
      scoring = ifelse(.data$pts_drive != 0, TRUE, .data$scoring)) %>%
    dplyr::mutate(drive_id = as.numeric(.data$id)) %>%
    dplyr::arrange(.data$game_id, .data$drive_id)

  return(clean_drive)
}

#' Add Betting columns
#' This is only for DI-FBS football
#'
#' @param play_df (\emph{data.frame} required): Play-By-Play data.frame as pulled from `cfb_pbp_dat()` and `clean_pbp_dat()`)
#' @param g_id (\emph{Integer} optional): Game ID filter for querying a single game
#' Can be found using the `cfb_game_info()` function
#' @param yr (\emph{Integer} optional): Select year (example: 2018)
#' @keywords internal
#' @import stringr
#' @import dplyr
#' @import tidyr
#' @export
#' 

add_betting_cols <- function(play_df, g_id, yr){
  
  game_spread <- cfb_betting_lines(game_id = g_id, year=yr, line_provider = 'consensus') %>%
    dplyr::mutate(spread = as.numeric(.data$spread)) %>% 
    dplyr::rename(game_id = .data$id, 
                  formatted_spread = .data$formattedSpread) %>% 
    dplyr::select(.data$game_id, .data$spread, .data$formatted_spread)
  
  pbp_df <- play_df %>%
    dplyr::left_join(game_spread, by=c('game_id'))
  
  return(pbp_df)
}
