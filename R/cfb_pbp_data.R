#' Extract CFB (D-I) Play by Play Data - For plays
#'
#' Extracts raw game by game data. \cr
#' @source \url{https://api.collegefootballdata.com/plays}
#'
#' @param season_type Select Season Type (regular, postseason, both)
#' @param year Select year, (example: 2018)
#' @param week Select week, this is optional (also numeric)
#' @param team Select team name (example: Texas, Texas A&M, Clemson)
#' @param play_type Select play type (example: see the \code{\link[cfbscrapR:cfb_play_type_df]{cfbscrapR::cfb_play_type_df}})
#' @param epa_wpa Logical parameter (TRUE/FALSE) to return the Expected Points Added/Win Probability Added variables
#'
#' @return A data frame with 149 variables:
#' \describe{
#'   \item{\code{year}}{integer.}
#'   \item{\code{week}}{integer.}
#'   \item{\code{id_play}}{character.}
#'   \item{\code{game_id}}{integer.}
#'   \item{\code{offense_play}}{character.}
#'   \item{\code{defense_play}}{character.}
#'   \item{\code{half}}{integer.}
#'   \item{\code{period}}{integer.}
#'   \item{\code{clock.minutes}}{double.}
#'   \item{\code{clock.seconds}}{integer.}
#'   \item{\code{play_type}}{character.}
#'   \item{\code{play_text}}{character.}
#'   \item{\code{down}}{double.}
#'   \item{\code{distance}}{double.}
#'   \item{\code{yards_to_goal}}{double.}
#'   \item{\code{yards_to_goal_end}}{double.}
#'   \item{\code{yards_gained}}{integer.}
#'   \item{\code{TimeSecsRem}}{double.}
#'   \item{\code{offense_score}}{integer.}
#'   \item{\code{defense_score}}{integer.}
#'   \item{\code{score_diff}}{integer.}
#'   \item{\code{score_diff_start}}{double.}
#'   \item{\code{EPA}}{double.}
#'   \item{\code{ep_before}}{double.}
#'   \item{\code{ep_after}}{double.}
#'   \item{\code{def_EPA}}{double.}
#'   \item{\code{wpa}}{double.}
#'   \item{\code{wp_before}}{double.}
#'   \item{\code{wp_after}}{double.}
#'   \item{\code{score_pts}}{double.}
#'   \item{\code{ppa}}{character.}
#'   \item{\code{game_play_number}}{double.}
#'   \item{\code{drive_number}}{integer.}
#'   \item{\code{drive_play_number}}{double.}
#'   \item{\code{firstD_by_poss}}{double.}
#'   \item{\code{firstD_by_penalty}}{double.}
#'   \item{\code{firstD_by_yards}}{double.}
#'   \item{\code{down_end}}{integer.}
#'   \item{\code{distance_end}}{double.}
#'   \item{\code{Goal_To_Go}}{logical.}
#'   \item{\code{Under_two}}{logical.}
#'   \item{\code{offense_timeouts}}{integer.}
#'   \item{\code{defense_timeouts}}{integer.}
#'   \item{\code{change_of_poss}}{double.}
#'   \item{\code{home}}{character.}
#'   \item{\code{away}}{character.}
#'   \item{\code{home_wp_before}}{double.}
#'   \item{\code{away_wp_before}}{double.}
#'   \item{\code{home_wp_after}}{double.}
#'   \item{\code{away_wp_after}}{double.}
#'   \item{\code{wpa_base}}{double.}
#'   \item{\code{wpa_base_nxt}}{double.}
#'   \item{\code{wpa_change}}{double.}
#'   \item{\code{wpa_change_nxt}}{double.}
#'   \item{\code{wpa_base_ind}}{double.}
#'   \item{\code{wpa_base_nxt_ind}}{double.}
#'   \item{\code{wpa_change_ind}}{double.}
#'   \item{\code{wpa_change_nxt_ind}}{double.}
#'   \item{\code{ExpScoreDiff}}{double.}
#'   \item{\code{ExpScoreDiff_Time_Ratio}}{double.}
#'   \item{\code{drive_start_yards_to_goal}}{integer.}
#'   \item{\code{drive_end_yards_to_goal}}{integer.}
#'   \item{\code{drive_yards}}{integer.}
#'   \item{\code{drive_scoring}}{logical.}
#'   \item{\code{drive_result}}{character.}
#'   \item{\code{drive_pts}}{double.}
#'   \item{\code{new_id}}{double.}
#'   \item{\code{drive_id}}{double.}
#'   \item{\code{TimeSecsRem_end}}{double.}
#'   \item{\code{offense_conference}}{character.}
#'   \item{\code{defense_conference}}{character.}
#'   \item{\code{yard_line}}{integer.}
#'   \item{\code{scoring}}{logical.}
#'   \item{\code{drive_time_minutes_start}}{integer.}
#'   \item{\code{drive_time_seconds_start}}{integer.}
#'   \item{\code{drive_time_minutes_end}}{integer.}
#'   \item{\code{drive_time_seconds_end}}{integer.}
#'   \item{\code{drive_time_minutes_elapsed}}{double.}
#'   \item{\code{drive_time_seconds_elapsed}}{double.}
#'   \item{\code{scoring_play}}{double.}
#'   \item{\code{pts_scored}}{double.}
#'   \item{\code{off_td_play}}{double.}
#'   \item{\code{def_td_play}}{double.}
#'   \item{\code{touchdown}}{double.}
#'   \item{\code{safety}}{double.}
#'   \item{\code{kickoff_play}}{double.}
#'   \item{\code{kickoff_tb}}{double.}
#'   \item{\code{kickoff_onside}}{double.}
#'   \item{\code{kickoff_oob}}{double.}
#'   \item{\code{kickoff_fair_catch}}{double.}
#'   \item{\code{kickoff_downed}}{double.}
#'   \item{\code{kick_play}}{double.}
#'   \item{\code{kickoff_safety}}{double.}
#'   \item{\code{punt}}{double.}
#'   \item{\code{punt_play}}{double.}
#'   \item{\code{punt_tb}}{double.}
#'   \item{\code{punt_oob}}{double.}
#'   \item{\code{punt_fair_catch}}{double.}
#'   \item{\code{punt_downed}}{double.}
#'   \item{\code{fumble_vec}}{double.}
#'   \item{\code{rush}}{double.}
#'   \item{\code{pass}}{double.}
#'   \item{\code{sack_vec}}{double.}
#'   \item{\code{turnover_vec}}{double.}
#'   \item{\code{sack}}{double.}
#'   \item{\code{int}}{double.}
#'   \item{\code{int_td}}{double.}
#'   \item{\code{completion}}{double.}
#'   \item{\code{pass_attempt}}{double.}
#'   \item{\code{target}}{double.}
#'   \item{\code{pass_td}}{double.}
#'   \item{\code{rush_td}}{double.}
#'   \item{\code{penalty_flag}}{logical.}
#'   \item{\code{penalty_declined}}{logical.}
#'   \item{\code{penalty_no_play}}{logical.}
#'   \item{\code{penalty_offset}}{logical.}
#'   \item{\code{penalty_1st_conv}}{logical.}
#'   \item{\code{penalty_text}}{logical.}
#'   \item{\code{end_of_half}}{double.}
#'   \item{\code{downs_turnover}}{double.}
#'   \item{\code{half_play_number}}{double.}
#'   \item{\code{off_timeouts_rem_before}}{double.}
#'   \item{\code{def_timeouts_rem_before}}{double.}
#'   \item{\code{missing_yard_flag}}{logical.}
#'   \item{\code{first_by_penalty}}{double.}
#'   \item{\code{first_by_yards}}{double.}
#'   \item{\code{adj_TimeSecsRem}}{double.}
#'   \item{\code{turnover_vec_lag}}{double.}
#'   \item{\code{def_td_play_lag}}{double.}
#'   \item{\code{play_after_turnover}}{double.}
#'   \item{\code{receives_2H_kickoff}}{double.}
#'   \item{\code{lag_score_diff}}{double.}
#'   \item{\code{lag_offense_play}}{character.}
#'   \item{\code{lead_offense_play}}{character.}
#'   \item{\code{lead_offense_play2}}{character.}
#'   \item{\code{lead_kickoff_play}}{double.}
#'   \item{\code{offense_receives_2H_kickoff}}{double.}
#'   \item{\code{home_EPA}}{double.}
#'   \item{\code{away_EPA}}{double.}
#'   \item{\code{rz_play}}{double.}
#'   \item{\code{scoring_opp}}{double.}
#'   \item{\code{stuffed_run}}{double.}
#'   \item{\code{success}}{double.}
#'   \item{\code{epa_success}}{double.}
#'   \item{\code{def_wp_before}}{double.}
#'   \item{\code{lead_wp_before}}{double.}
#'   \item{\code{lead2_wp_before}}{double.}
#'   \item{\code{def_wp_after}}{double.}
#' }
#' @keywords Play-by-Play
#' @import stringr
#' @import tidyr
#' @importFrom purrr "map_dfr"
#' @importFrom dplyr "mutate" "left_join" "select" "rename" "filter" "group_by" "arrange" "ungroup"
#' @importFrom jsonlite "fromJSON"
#' @importFrom utils "URLencode"
#' @importFrom utils "globalVariables"
#' @importFrom assertthat "assert_that"
#' @export
#' 

cfb_pbp_data <- function(year,
                         season_type = 'regular',
                         week = 1,
                         team = NULL,
                         play_type = NULL,
                         epa_wpa=FALSE) {
  options(stringsAsFactors = FALSE)
  options(scipen = 999)
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
    assertthat::assert_that(season_type %in% c('postseason','both'),
                            msg = 'Enter valid season_type: regular, postseason, or both')
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
  
  raw_play_df <- jsonlite::fromJSON(full_url)
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
    'drive_game_id', 'drive_id_drive', #'drive_drive_number',
    'drive_plays', 'drive_start_yardline', 'drive_end_yardline',
    'drive_offense', 'drive_offense_conference',
    'drive_defense', 'drive_defense_conference',
    'drive_start_time.hours', 'drive_start_time.minutes', 'drive_start_time.seconds',
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
    
    #---- Purrr Map Function -----
    g_ids = sort(unique(play_df$game_id))
    play_df = purrr::map_dfr(g_ids,
                             function(x) {
                               play_df %>%
                                 dplyr::filter(.data$game_id == x) %>%
                                 penalty_detection() %>% 
                                 add_play_counts() %>% 
                                 clean_pbp_dat() %>% 
                                 clean_drive_dat() %>% 
                                 prep_epa_df_after() %>% 
                                 create_epa() %>%
                                 # add_betting_cols(g_id = x, yr=year) %>%
                                 # create_wpa_betting() %>%
                                 create_wpa_naive()
                             })
    
    play_df <- play_df %>% 
      dplyr::select(#-.data$drive_drive_number,
                    -.data$play_number) %>% 
      dplyr::select(.data$id_play,
                    .data$game_id,
                    .data$offense_play,
                    .data$defense_play,
                    .data$score_diff_start,
                    .data$score_diff,
                    .data$offense_score,
                    .data$defense_score,
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
                    .data$EPA,
                    .data$ep_before,
                    .data$ep_after,
                    .data$def_EPA,
                    .data$wpa,
                    .data$wp_before,
                    .data$wp_after,
                    .data$score_pts,
                    .data$change_of_poss,
                    .data$wpa_base,
                    .data$wpa_base_nxt,
                    .data$wpa_change,
                    .data$wpa_change_nxt,
                    .data$wpa_base_ind,
                    .data$wpa_base_nxt_ind,
                    .data$wpa_change_ind,
                    .data$wpa_change_nxt_ind,
                    .data$offense_receives_2H_kickoff,
                    .data$home_wp_before,
                    .data$away_wp_before,
                    .data$home_wp_after,
                    .data$away_wp_after,
                    .data$ExpScoreDiff,
                    .data$ExpScoreDiff_Time_Ratio,
                    .data$offense_timeouts,
                    .data$defense_timeouts,
                    .data$off_timeouts_rem_before,
                    .data$def_timeouts_rem_before,
                    .data$off_timeouts_rem_before,
                    .data$def_timeouts_rem_before,
                    .data$off_timeout_called,
                    .data$def_timeout_called,
                    .data$ppa,
                    .data$game_play_number,
                    .data$drive_number,
                    .data$drive_play_number,
                    .data$firstD_by_poss,
                    .data$firstD_by_penalty,
                    .data$firstD_by_yards,
                    .data$down_end,
                    .data$distance_end,
                    .data$Goal_To_Go,
                    .data$Under_two,
                    .data$home,
                    .data$away,
                    .data$drive_start_yards_to_goal,
                    .data$drive_end_yards_to_goal,
                    .data$drive_yards,
                    .data$drive_scoring,
                    .data$new_drive_result,
                    .data$drive_pts,
                    dplyr::everything())
  }
  play_df <- as.data.frame(play_df)
  
  return(play_df)
}


#' Penalty Detection
#' Adds penalty columns to Play-by-Play data pulled from the API
#'
#' @param raw_df (\emph{data.frame} required): Performs data cleansing on Play-by-Play DataFrame, as pulled from `cfb_pbp_dat()`
#' @details Runs penalty detection on the play text and play types. Requires the following columns be present:
#' \itemize{
#' \item{game_id}
#' \item{period}
#' \item{down}
#' \item{play_type}
#' \item{play_text}
#' }
#' @return The original `raw_df` with the following columns appended/redefined:
#' \describe{
#' \item{penalty_flag}{TRUE/FALSE flag for penalty play types or penalty in play text plays.}
#' \item{penalty_declined}{TRUE/FALSE flag for 'declined' in penalty play types or penalty in play text plays.}
#' \item{penalty_no_play}{TRUE/FALSE flag for 'no play' in penalty play types or penalty in play text plays.}
#' \item{penalty_offset}{TRUE/FALSE flag for 'off-setting' in penalty play types or penalty in play text plays.}
#' \item{penalty_1st_conv}{TRUE/FALSE flag for 1st Down in penalty play types or penalty in play text plays.}
#' \item{penalty_text}{TRUE/FALSE flag for penalty in text but not a penalty play type.}
#' \item{orig_play_type}{Copy of original play_type label prior to any changes by the proceeding functions}
#' \item{down}{Defines kickoff downs and penalties on kickoffs and converts them from 5 (as from the API) to 1.}
#' \item{play_type}{Defines `play_type`, "Penalty (Kickoff)", penalties on kickoffs with a repeat kick.}
#' \item{half}{Defines the half variable (1, 2).}
#' }
#' @keywords internal
#' @importFrom rlang ".data"
#' @importFrom stringr "str_detect"
#' @importFrom dplyr "mutate" "filter"
#' @import tidyr
#' @export
#'

penalty_detection <- function(raw_df) {
  #-- 'Penalty' in play text ----
  pen_text = stringr::str_detect(raw_df$play_text, regex("penalty", ignore_case = TRUE))
  #-- 'Declined' in play text ----
  pen_declined_text = stringr::str_detect(raw_df$play_text, regex("declined", ignore_case = TRUE))
  #-- 'No Play' in play text ----
  pen_no_play_text = stringr::str_detect(raw_df$play_text, regex("no play", ignore_case = TRUE))
  #-- 'Off-setting' in play text ----
  pen_offset_text = stringr::str_detect(raw_df$play_text, regex("off-setting", ignore_case = TRUE))
  #-- '1st Down' in play text ----
  pen_1st_down_text = stringr::str_detect(raw_df$play_text, regex("1st down", ignore_case = TRUE))
  
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
  raw_df$penalty_text[pen_text & !pen_type & !pen_declined_text & 
                        !pen_offset_text & !pen_no_play_text] <- TRUE
  
  ##-- Kickoff down adjustment ----
  raw_df = raw_df %>%
    dplyr::mutate(
      orig_play_type = .data$play_type,
      down = ifelse(.data$down == 5 & stringr::str_detect(.data$play_type, "Kickoff"), 1, .data$down),
      play_type = ifelse(.data$down == 5 & stringr::str_detect(.data$play_type, "Penalty"), 
                         "Penalty (Kickoff)", .data$play_type),
      down = ifelse(.data$down == 5 & stringr::str_detect(.data$play_type, "Penalty"), 1, .data$down),
      half = ifelse(.data$period <= 2, 1, 2)) %>% 
    dplyr::filter(
      !(.data$game_id == '302610012' & .data$down == 5 & .data$play_type == 'Rush')
    )
  return(raw_df)
}


#' Adds play counts to Play-by-Play data
#' Adds play counts to Play-by-Play data pulled from the API's raw game data
#'
#' @param play_df (\emph{data.frame} required): Adds play counts to Play-by-Play dataframe, as pulled from `cfb_pbp_dat()`
#' @return The original `play_df` with the following columns appended/redefined:
#' \describe{
#' \item{game_play_number}{.}
#' \item{half_clock.minutes}{.}
#' \item{TimeSecsRem}{.}
#' \item{Under_two}{.}
#' \item{half}{.}
#' \item{kickoff_play}{.}
#' \item{receives_2H_kickoff}{.}
#' \item{score_diff}{.}
#' \item{lag_score_diff}{.}
#' \item{lag_offense_play}{.}
#' \item{lead_offense_play}{.}
#' \item{lead_offense_play2}{.}
#' \item{score_pts}{.}
#' \item{score_diff_start}{.}
#' \item{offense_receives_2H_kickoff}{.}
#' \item{half_play_number}{.}
#' \item{lag_off_timeouts}{.}
#' \item{lag_def_timeouts}{.}
#' \item{off_timeouts_rem_before}{.}
#' \item{def_timeouts_rem_before}{.}
#' \item{off_timeout_called}{.}
#' \item{def_timeout_called}{.}
#' \item{lead_TimeSecsRem}{.}
#' \item{end_of_half}{.}
#' \item{lead_play_type}{.}
#' \item{lead_play_type2}{.}
#' \item{lead_play_type3}{.}
#' \item{change_of_poss}{.}
#' }
#' @keywords internal
#' @importFrom rlang ".data"
#' @importFrom dplyr "group_by" "mutate" "ungroup" "lead" "lag" "arrange"
#' @import stringr
#' @import tidyr
#' @export
#' 
#'

add_play_counts <- function(play_df) {
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
  defense_score_vec = c(
    "Blocked Punt Touchdown",
    "Blocked Punt (Safety)",
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
    "Field Goal Missed",
    "Missed Field Goal Return",
    "Missed Field Goal Return Touchdown",
    "Fumble Recovery (Opponent)",
    "Fumble Recovery (Opponent) Touchdown",
    "Fumble Return Touchdown",
    "Defensive 2pt Conversion",
    "Interception",
    "Interception Return",
    "Interception Return Touchdown",
    "Pass Interception",
    "Pass Interception Return",
    "Pass Interception Return Touchdown",
    "Blocked Punt",
    "Blocked Punt Touchdown",
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
    "Punt Touchdown", #<--- Punting Team recovers the return team fumble and scores
    "Punt Team Fumble Recovery Touchdown",
    "Kickoff Touchdown", #<--- Kickoff Team recovers the return team fumble and scores
    "Kickoff Team Fumble Recovery Touchdown"
  )
  punt_vec = c(
    "Blocked Punt",
    "Blocked Punt Touchdown",
    "Blocked Punt (Safety)",
    "Punt",
    "Punt Touchdown",
    "Punt Team Fumble Recovery",
    "Punt Team Fumble Recovery Touchdown",
    "Punt Return Touchdown"
  )
  kickoff_vec = c(
    "Kickoff",
    "Kickoff Return (Offense)",
    "Kickoff Return Touchdown",
    "Kickoff Touchdown",
    "Kickoff Team Fumble Recovery",
    "Kickoff Team Fumble Recovery Touchdown",
    "Kickoff (Safety)",
    "Penalty (Kickoff)"
  )
  int_vec = c(
    "Interception",
    "Interception Return",
    "Interception Return Touchdown",
    "Pass Interception",
    "Pass Interception Return",
    "Pass Interception Return Touchdown"
  )
  
  play_df <- play_df %>% 
    dplyr::group_by(.data$game_id) %>% 
    dplyr::mutate(
      play = ifelse(!(.data$play_type %in% c("End Period", "End of Half", "Timeout")), 1, 0),
      game_play_number = cumsum(.data$play),
      half_clock.minutes = ifelse(.data$period %in% c(1, 3), 15 + 
                                    .data$clock.minutes, .data$clock.minutes),
      TimeSecsRem = .data$half_clock.minutes * 60 + .data$clock.seconds,
      Under_two = .data$TimeSecsRem <= 180,
      half = ifelse(.data$period <= 2, 1, 2),
      kickoff_play = ifelse(.data$play_type %in% kickoff_vec, 1, 0),
      receives_2H_kickoff = ifelse(.data$game_play_number == 1 & .data$kickoff_play == 1 & 
                                     .data$offense_play == .data$home, 1, 
                                   ifelse(.data$game_play_number == 1 & .data$kickoff_play == 1 &
                                            .data$offense_play == .data$away,0,NA)),
      score_diff = .data$offense_score - .data$defense_score,
      lag_score_diff = dplyr::lag(.data$score_diff, 1),
      lag_score_diff = ifelse(.data$game_play_number == 1, 0, .data$lag_score_diff),
      lag_offense_play = dplyr::lag(.data$offense_play, 1),
      lag_offense_play = ifelse(.data$game_play_number == 1, .data$offense_play, .data$lag_offense_play),
      lead_offense_play = dplyr::lead(.data$offense_play, 1),
      lead_offense_play2 = dplyr::lead(.data$offense_play, 2),
      score_pts = ifelse(.data$lag_offense_play == .data$offense_play,
                         (.data$score_diff - .data$lag_score_diff),
                         (.data$score_diff + .data$lag_score_diff)),
      score_diff_start = ifelse(.data$lag_offense_play == .data$offense_play & 
                                  !(.data$play_type %in% kickoff_vec),
                                .data$lag_score_diff,
                                -1*.data$lag_score_diff)) %>% 
    tidyr::fill(.data$receives_2H_kickoff) %>% 
    dplyr::mutate(
      offense_receives_2H_kickoff = case_when(
        .data$offense_play == .data$home & .data$receives_2H_kickoff == 1 ~ 1,
        .data$offense_play == .data$away & .data$receives_2H_kickoff == 0 ~ 1,
        TRUE ~ 0)) %>% 
    dplyr::group_by(.data$game_id, .data$half) %>% 
    dplyr::arrange(.data$game_id, .data$half, .data$period, 
                   -.data$TimeSecsRem, .data$id_play, .by_group = TRUE) %>% 
    dplyr::mutate(
      half_play = ifelse(!(.data$play_type %in% c("End Period", "End of Half", "Timeout")), 1, 0),
      half_play_number = cumsum(.data$half_play),
      ## TO-DO: need to make sure these timeouts lines up with the teams
      lag_off_timeouts = lag(.data$offense_timeouts, 1),
      lag_off_timeouts = ifelse(.data$half_play_number == 1, 3, .data$lag_off_timeouts),
      lag_def_timeouts = lag(.data$defense_timeouts, 1),
      lag_def_timeouts = ifelse(.data$half_play_number == 1, 3, .data$lag_def_timeouts),
      off_timeouts_rem_before = ifelse(.data$lag_offense_play == .data$offense_play, 
                                       .data$lag_off_timeouts, .data$lag_def_timeouts),
      off_timeouts_rem_before = ifelse(.data$half_play_number == 1, 3, .data$off_timeouts_rem_before),
      def_timeouts_rem_before = ifelse(.data$lag_offense_play == .data$offense_play, 
                                       .data$lag_def_timeouts, .data$lag_off_timeouts),
      def_timeouts_rem_before = ifelse(.data$half_play_number == 1, 3, .data$def_timeouts_rem_before),
      
      off_timeout_called = ifelse(.data$offense_timeouts != .data$off_timeouts_rem_before, 1, 0),
      def_timeout_called = ifelse(.data$defense_timeouts != .data$def_timeouts_rem_before, 1, 0),
      lead_TimeSecsRem = dplyr::lead(.data$TimeSecsRem, 1),
      end_of_half = ifelse(is.na(.data$lead_TimeSecsRem), 1, 0),
      lead_play_type = dplyr::lead(.data$play_type, 1),
      lead_play_type2 = dplyr::lead(.data$play_type, 2),
      lead_play_type3 = dplyr::lead(.data$play_type, 3),
      #-- Change of possession by lead('offense_play', 1)----
      change_of_poss = ifelse(.data$offense_play == .data$lead_offense_play & 
                                .data$lead_play_type != "End Period", 0,
                              ifelse(.data$offense_play == .data$lead_offense_play2 & 
                                       .data$lead_play_type == "End Period", 0, 1)),
      change_of_poss = ifelse(is.na(.data$change_of_poss), 0, .data$change_of_poss),
      #TO-DO: define a fix for end of period plays on possession changing plays
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::arrange(.data$game_id,.data$half,.data$period,
                   -.data$TimeSecsRem, -.data$lead_TimeSecsRem, .data$id_play) 
  return(play_df)
}

#' Clean Play-by-Play data
#' Cleans Play-by-Play data pulled from the API's raw game data
#'
#' @param play_df (\emph{data.frame} required): Performs data cleansing on Play-by-Play DataFrame, as pulled from `cfb_pbp_dat()`
#' @return The original `play_df` with the following columns appended/redefined:
#' \describe{
#' \item{scoring_play}{.}
#' \item{td_play}{.}
#' \item{touchdown}{.}
#' \item{safety}{.}
#' \item{fumble_vec}{.}
#' \item{kickoff_play}{.}
#' \item{kickoff_tb}{.}
#' \item{kickoff_onside}{.}
#' \item{kickoff_oob}{.}
#' \item{kickoff_fair_catch}{.}
#' \item{kickoff_downed}{.}
#' \item{kick_play}{.}
#' \item{kickoff_safety}{.}
#' \item{punt}{.}
#' \item{punt_play}{.}
#' \item{punt_tb}{.}
#' \item{punt_oob}{.}
#' \item{punt_fair_catch}{.}
#' \item{punt_downed}{.}
#' \item{rush_vec}{.}
#' \item{pass_vec}{.}
#' \item{sack_vec}{.}
#' \item{play_type}{.}
#' \item{td_check}{.}
#' \item{id_play}{.}
#' \item{sack}{.}
#' \item{int}{.}
#' \item{int_td}{.}
#' \item{completion}{.}
#' \item{pass_attempt}{.}
#' \item{target}{.}
#' \item{pass_td}{.}
#' \item{rush_td}{.}
#' \item{turnover_vec}{.}
#' \item{off_td_play}{.}
#' \item{def_td_play}{.}
#' \item{downs_turnover}{.}
#' \item{scoring_play}{.}
#' \item{fg_inds}{.}
#' \item{yds_fg}{.}
#' \item{yards_to_goal}{.}
#' \item{pos_team}{.}
#' \item{def_pos_team}{.}
#' \item{lead_pos_team}{.}
#' \item{lead_pos_team2}{.}
#' \item{pos_team_timeouts_rem}{.}
#' \item{def_pos_team_timeouts_rem}{.}
#' \item{change_of_pos_team}{.}
#' }
#' @keywords internal
#' @importFrom stringr "str_detect" "str_remove" "str_remove"
#' @importFrom dplyr "mutate" "if_else" 
#' @import tidyr
#' @export
#'

clean_pbp_dat <- function(play_df) {
  ##--Play type vectors------
  scores_vec = c(
    "Blocked Punt Touchdown",
    "Blocked Punt (Safety)",
    "Blocked Field Goal Touchdown",
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
    "Kickoff (Safety)",
    "Passing Touchdown",
    "Rushing Touchdown",
    "Field Goal Good",
    "Pass Reception Touchdown",
    "Fumble Recovery (Own) Touchdown"
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
    "Interception Return",
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
    "Punt Touchdown", #<--- Punting Team recovers the return team fumble and scores
    "Kickoff Touchdown" #<--- Kickoff Team recovers the return team fumble and scores
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
    "Kickoff Touchdown",
    "Kickoff Team Fumble Recovery",
    "Kickoff Team Fumble Recovery Touchdown",
    "Kickoff (Safety)",
    "Penalty (Kickoff)"
  )
  int_vec = c(
    "Interception",
    "Interception Return",
    "Interception Return Touchdown",
    "Pass Interception",
    "Pass Interception Return",
    "Pass Interception Return Touchdown"
  )
  
  play_df <- play_df %>% 
    dplyr::mutate(
      #-- Touchdowns----
      scoring_play = ifelse(.data$play_type %in% scores_vec, 1, 0),
      
      td_play = ifelse(stringr::str_detect(.data$play_text, regex("touchdown|for a TD", ignore_case = TRUE)) &
                         !is.na(.data$play_text), 1, 0), 
      
      touchdown = ifelse(stringr::str_detect(.data$play_type, regex('touchdown', ignore_case = TRUE)) , 1, 0),
      safety = ifelse(stringr::str_detect(.data$play_text, regex("safety", ignore_case = TRUE)),1,0),
      #-- Fumbles----
      fumble_vec = ifelse(stringr::str_detect(.data$play_text, "fumble") & !is.na(.data$play_text), 1, 0),
      #-- Kicks/Punts----
      kickoff_play = ifelse(.data$play_type %in% kickoff_vec, 1, 0),
      kickoff_tb = ifelse(stringr::str_detect(.data$play_text,regex("touchback", ignore_case = TRUE)) &
                            (.data$kickoff_play == 1) & !is.na(.data$play_text), 1, 0),
      kickoff_onside = ifelse(stringr::str_detect(.data$play_text, regex("on-side|onside|on side", ignore_case = TRUE)) &
                                (.data$kickoff_play == 1) & !is.na(.data$play_text), 1, 0),
      kickoff_oob = ifelse(stringr::str_detect(.data$play_text, regex("out-of-bounds|out of bounds", ignore_case = TRUE)) &
                             (.data$kickoff_play == 1) & !is.na(.data$play_text), 1, 0),
      kickoff_fair_catch = ifelse(stringr::str_detect(.data$play_text, regex("fair catch|fair caught", ignore_case = TRUE)) &
                                    (.data$kickoff_play == 1) & !is.na(.data$play_text), 1, 0),
      kickoff_downed = ifelse(stringr::str_detect(.data$play_text, regex("downed", ignore_case = TRUE)) &
                                (.data$kickoff_play == 1) & !is.na(.data$play_text), 1, 0),
      kick_play = ifelse(stringr::str_detect(.data$play_text, regex("kick|kickoff", ignore_case = TRUE)) &
                           !is.na(.data$play_text), 1, 0),
      kickoff_safety = ifelse(!(.data$play_type %in% c('Blocked Punt','Penalty')) & .data$safety == 1 & 
                                stringr::str_detect(.data$play_text, 
                                           regex("kickoff", ignore_case = TRUE)), 1, 0),
      punt = ifelse(.data$play_type %in% punt_vec, 1, 0),
      punt_play = ifelse(stringr::str_detect(.data$play_text, regex("punt", ignore_case = TRUE)) &
                           !is.na(.data$play_text), 1, 0),
      punt_tb = ifelse(stringr::str_detect(.data$play_text, regex("touchback", ignore_case = TRUE)) &
                         (.data$punt == 1) & !is.na(.data$play_text), 1, 0),
      punt_oob = ifelse(stringr::str_detect(.data$play_text, regex("out-of-bounds|out of bounds", ignore_case = TRUE)) &
                          (.data$punt == 1) & !is.na(.data$play_text), 1, 0),
      punt_fair_catch = ifelse(stringr::str_detect(.data$play_text, regex("fair catch|fair caught", ignore_case = TRUE)) &
                                 (.data$punt == 1) & !is.na(.data$play_text), 1, 0),
      punt_downed = ifelse(stringr::str_detect(.data$play_text, regex("downed", ignore_case = TRUE)) &
                             (.data$punt == 1) & !is.na(.data$play_text), 1, 0),
      yds_punted = ifelse(.data$punt == 1, as.numeric(stringr::str_extract(
        stringi::stri_extract_first_regex(.data$play_text, '(?<= punt for)[^,]+'),
        "\\d+"
      )), NA_real_),
      yds_punt_gained = ifelse(.data$punt == 1, .data$yards_gained, NA_real_),
      #-- Pass/Rush----
      rush_vec = ifelse(
        (.data$play_type == "Rush" & !is.na(.data$play_text)) |
          .data$play_type == "Rushing Touchdown" |
          (.data$play_type == "Safety" &
             stringr::str_detect(.data$play_text, regex("run for", ignore_case = TRUE)) & !is.na(.data$play_text)) |
          (.data$play_type == "Fumble Recovery (Opponent)" &
             stringr::str_detect(.data$play_text, regex("run for", ignore_case = TRUE)) & !is.na(.data$play_text)) |
          (.data$play_type == "Fumble Recovery (Opponent) Touchdown" &
             stringr::str_detect(.data$play_text, regex("run for", ignore_case = TRUE)) & !is.na(.data$play_text)) |
          (.data$play_type == "Fumble Recovery (Own)" &
             stringr::str_detect(.data$play_text, regex("run for", ignore_case = TRUE)) & !is.na(.data$play_text)) |
          (.data$play_type == "Fumble Recovery (Own) Touchdown" &
             stringr::str_detect(.data$play_text, regex("run for", ignore_case = TRUE)) & !is.na(.data$play_text)) |
          (.data$play_type == "Fumble Return Touchdown" &
             stringr::str_detect(.data$play_text, regex("run for", ignore_case = TRUE)) & !is.na(.data$play_text)), 1, 0),
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
          (.data$play_type == "Safety" &
             stringr::str_detect(.data$play_text, regex('sacked', 
                                               ignore_case = TRUE)) & !is.na(.data$play_text)) |
          (.data$play_type == "Safety" &
             stringr::str_detect(.data$play_text, regex("pass complete", 
                                               ignore_case = TRUE)) & !is.na(.data$play_text)) |
          (.data$play_type == "Fumble Recovery (Own)" &
             stringr::str_detect(.data$play_text, regex("pass complete|pass incomplete|pass intercepted", 
                                               ignore_case = TRUE)) & !is.na(.data$play_text)) |
          (.data$play_type == "Fumble Recovery (Own)" &
             stringr::str_detect(.data$play_text, regex("sacked", 
                                               ignore_case = TRUE)) & !is.na(.data$play_text)) |
          (.data$play_type == "Fumble Recovery (Own) Touchdown" &
             stringr::str_detect(.data$play_text, regex("pass complete|pass incomplete|pass intercepted", 
                                               ignore_case = TRUE)) & !is.na(.data$play_text)) |
          (.data$play_type == "Fumble Recovery (Opponent)" &
             stringr::str_detect(.data$play_text, regex("pass complete|pass incomplete|pass intercepted", 
                                               ignore_case = TRUE)) & !is.na(.data$play_text)) |
          (.data$play_type == "Fumble Recovery (Opponent)" &
             stringr::str_detect(.data$play_text, regex("sacked", 
                                               ignore_case = TRUE)) & !is.na(.data$play_text)) |
          (.data$play_type == "Fumble Recovery (Opponent) Touchdown" &
             stringr::str_detect(.data$play_text, regex("pass complete|pass incomplete", 
                                               ignore_case = TRUE)) & !is.na(.data$play_text)) |
          (.data$play_type == "Fumble Return Touchdown" &
             stringr::str_detect(.data$play_text, regex("pass complete|pass incomplete", 
                                               ignore_case = TRUE)) & !is.na(.data$play_text))|
          (.data$play_type == "Fumble Return Touchdown" &
             stringr::str_detect(.data$play_text, regex("sacked", 
                                               ignore_case = TRUE)) & !is.na(.data$play_text)), 1, 0), 
      #-- Sacks----
      sack_vec = ifelse(
        ((.data$play_type %in% c("Sack", "Sack Touchdown"))|
           (.data$play_type %in% c("Fumble Recovery (Own)", "Fumble Recovery (Own) Touchdown",
                                   "Fumble Recovery (Opponent)", "Fumble Recovery (Opponent) Touchdown",
                                   "Fumble Return Touchdown") & 
              .data$pass_vec == 1 & stringr::str_detect(.data$play_text, regex('sacked', ignore_case = TRUE)))|
           (.data$play_type == "Safety" & stringr::str_detect(.data$play_text, regex('sacked', ignore_case = TRUE))))
        & !is.na(.data$play_text), 1, 0),
      ## Fix Strip-Sacks to Fumbles----
      play_type = ifelse(.data$fumble_vec == 1 & .data$pass_vec == 1 & 
                           .data$change_of_poss == 1 & .data$td_play == 0 & .data$down != 4 &
                           !(.data$play_type %in% defense_score_vec),
                         "Fumble Recovery (Opponent)", .data$play_type),
      play_type = ifelse(.data$fumble_vec == 1 & .data$pass_vec == 1 & 
                           .data$change_of_poss == 1 & .data$td_play == 1, 
                         "Fumble Recovery (Opponent) Touchdown", .data$play_type),
      ## Fix rushes with fumbles and a change of possession to fumbles----
      play_type = ifelse(.data$fumble_vec == 1 & .data$rush_vec == 1 & 
                           .data$change_of_poss == 1 & .data$td_play == 0 & 
                           !(.data$play_type %in% defense_score_vec),
                         "Fumble Recovery (Opponent)", .data$play_type),
      play_type = ifelse(.data$fumble_vec == 1 & .data$rush_vec == 1 & 
                           .data$change_of_poss == 1 & .data$td_play == 1, 
                         "Fumble Recovery (Opponent) Touchdown", .data$play_type),
      ## Portion of touchdown check for plays where touchdown is not listed in the play_type--
      td_check = ifelse(!str_detect(.data$play_type, "Touchdown"), 1, 0),
      #-- Fix kickoff fumble return TDs----
      play_type = ifelse(.data$kickoff_play == 1 & .data$fumble_vec == 1 & 
                           .data$td_play == 1 & .data$td_check == 1,
                         paste0(.data$play_type, " Touchdown"), 
                         .data$play_type),
      #-- Fix punt return TDs----
      play_type = ifelse(.data$punt_play == 1 & .data$td_play == 1 & .data$td_check == 1,
                         paste0(.data$play_type, " Touchdown"), 
                         .data$play_type),
      #-- Fix kick return TDs----
      play_type = ifelse(.data$kickoff_play == 1 & .data$fumble_vec == 0 &
                           .data$td_play == 1 & .data$td_check == 1,
                         "Kickoff Return Touchdown", 
                         .data$play_type),
      #-- Fix rush/pass tds that aren't explicit----
      play_type = ifelse(.data$td_play == 1 & .data$rush_vec == 1 &
                           .data$fumble_vec == 0 & .data$td_check == 1,
                         "Rushing Touchdown", 
                         .data$play_type),
      play_type = ifelse(.data$td_play == 1 & .data$pass_vec == 1 & .data$td_check == 1 &
                           .data$fumble_vec == 0 & !(.data$play_type %in% int_vec),
                         "Passing Touchdown", 
                         .data$play_type),
      play_type = ifelse(.data$pass_vec == 1 & .data$play_type == "Pass Reception" &
                           .data$yards_gained == .data$yards_to_goal &
                           .data$fumble_vec == 0 & !(.data$play_type %in% int_vec),
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
      play_type = ifelse(.data$play_type == "Uncategorized Touchdown Touchdown",
                         "Uncategorized Touchdown",
                         .data$play_type),
      #-- Fix Pass Interception Return TD play_type labels----
      play_type = ifelse(stringr::str_detect(.data$play_text,"pass intercepted for a TD") & !is.na(.data$play_text),
                         "Interception Return Touchdown", .data$play_type),
      #-- Fix Sack/Fumbles Touchdown play_type labels----
      play_type = ifelse(stringr::str_detect(.data$play_text, regex("sacked", ignore_case = TRUE)) & 
                           stringr::str_detect(.data$play_text, regex("fumbled", ignore_case = TRUE)) &
                           stringr::str_detect(.data$play_text, regex("TD",ignore_case = TRUE)) &
                           !is.na(.data$play_text), 
                         "Fumble Recovery (Opponent) Touchdown", .data$play_type),
      play_type = ifelse(.data$play_type == "Pass", "Pass Incompletion", .data$play_type),
      #--- Moving non-Touchdown pass interceptions to one play_type: "Interception Return" -----
      play_type = ifelse(.data$play_type == "Interception", "Interception Return", .data$play_type),
      play_type = ifelse(.data$play_type == "Pass Interception", "Interception Return", .data$play_type),
      play_type = ifelse(.data$play_type == "Pass Interception Return", "Interception Return", .data$play_type),
      #--- Moving Kickoff/Punt Touchdowns without fumbles to Kickoff/Punt Return Touchdown
      play_type = ifelse(.data$play_type == "Kickoff Touchdown" & .data$fumble_vec == 0, 
                         "Kickoff Return Touchdown", .data$play_type),
      play_type = ifelse(.data$play_type == "Punt Touchdown" & 
                           (.data$fumble_vec == 0 | (.data$fumble_vec == 1 & .data$game_id == 401112100)),
                         "Punt Return Touchdown", .data$play_type),
      play_type = ifelse(.data$play_type == "Fumble Return Touchdown" & (.data$pass_vec == 1|.data$rush_vec == 1), 
                         "Fumble Recovery (Opponent) Touchdown", .data$play_type),
      play_type = ifelse(.data$play_type %in% c("Pass Reception", "Rush", "Rushing Touchdown") & 
                           (.data$pass_vec == 1|.data$rush_vec == 1) & .data$safety == 1, 
                         "Safety", .data$play_type),
      play_type = ifelse(.data$kickoff_safety == 1, "Kickoff (Safety)", .data$play_type),
      id_play = ifelse(.data$id_play == 400852742102997104 & .data$play_type == "Kickoff", 400852742102997106, .data$id_play),
      id_play = ifelse(.data$id_play == 400852742102997106 & .data$play_type == "Defensive 2pt Conversion", 400852742102997104, .data$id_play),
      
      #--- Sacks ----
      sack = ifelse(
        (.data$play_type %in% c("Sack")|
           (.data$play_type %in% c("Fumble Recovery (Own)", 
                                   "Fumble Recovery (Own) Touchdown",
                                   "Fumble Recovery (Opponent)", 
                                   "Fumble Recovery (Opponent) Touchdown") & 
              .data$pass_vec == 1 & stringr::str_detect(.data$play_text, "sacked"))|
           (.data$play_type == "Safety" & stringr::str_detect(.data$play_text, regex('sacked', ignore_case = TRUE))))
        & !is.na(.data$play_text), 1, 0),
      #--- Interceptions ------
      int = ifelse(.data$play_type %in% c("Interception Return", "Interception Return Touchdown"), 1, 0),
      int_td = ifelse(.data$play_type %in% c("Interception Return Touchdown"), 1, 0),
      #--- Pass Completions, Attempts and Targets -------
      completion = ifelse(.data$play_type %in% c("Pass Reception","Pass Completion", "Passing Touchdown")|
                            ((.data$play_type %in% c("Fumble Recovery (Own)",
                                                     "Fumble Recovery (Own) Touchdown",
                                                     "Fumble Recovery (Opponent)",
                                                     "Fumble Recovery (Opponent) Touchdown") & .data$pass_vec == 1 &
                                !stringr::str_detect(.data$play_text,"sacked"))), 1, 0),
      pass_attempt = ifelse(.data$play_type %in% c("Pass Reception","Pass Completion",
                                                   "Passing Touchdown", "Pass Incompletion",
                                                   "Interception Return", 
                                                   "Interception Return Touchdown")|
                              ((.data$play_type %in% c("Fumble Recovery (Own)",
                                                       "Fumble Recovery (Own) Touchdown",
                                                       "Fumble Recovery (Opponent)",
                                                       "Fumble Recovery (Opponent) Touchdown") & .data$pass_vec == 1 &
                                  !stringr::str_detect(.data$play_text,"sacked"))), 1, 0),
      target = ifelse(.data$play_type %in% c("Pass Reception","Pass Completion",
                                             "Passing Touchdown","Pass Incompletion")|
                        ((.data$play_type %in% c("Fumble Recovery (Own)",
                                                 "Fumble Recovery (Own) Touchdown",
                                                 "Fumble Recovery (Opponent)",
                                                 "Fumble Recovery (Opponent) Touchdown") & .data$pass_vec == 1 &
                            !stringr::str_detect(.data$play_text,"sacked"))), 1, 0),
      #--- Pass/Rush TDs ------
      pass_td = ifelse(.data$play_type %in% c("Passing Touchdown"), 1, 0),
      rush_td = ifelse(.data$play_type %in% c("Rushing Touchdown"), 1, 0),
      #-- Change of possession via turnover
      turnover_vec = ifelse(.data$play_type %in% turnover_vec, 1, 0),
      off_td_play = ifelse(.data$play_type %in% offense_score_vec, 1, 0),
      def_td_play = ifelse(.data$play_type %in% defense_score_vec, 1, 0),
      downs_turnover = ifelse((.data$play_type %in% normalplay) & 
                                (.data$yards_gained < .data$distance) & (.data$down == 4) &
                                !(.data$penalty_1st_conv), 1, 0),
      #-- Touchdowns----
      scoring_play = ifelse(.data$play_type %in% scores_vec, 1, 0),
      fg_inds = ifelse(stringr::str_detect(.data$play_type, "Field Goal"), 1, 0),
      yds_fg = ifelse(.data$fg_inds == 1, as.numeric(
          stringr::str_remove(stringr::str_extract(.data$play_text, 
                              regex('\\d{0,2} Yd FG|\\d{0,2} Yd Field|\\d{0,2} Yard Field', ignore_case = TRUE)), 
              regex("yd FG|yd field|yard field", ignore_case = TRUE))), NA),
      yards_to_goal = ifelse(.data$fg_inds == 1 & !is.na(.data$yds_fg), .data$yds_fg-17, .data$yards_to_goal),
      pos_team = ifelse(.data$offense_play == .data$home & .data$kickoff_play == 1, .data$away,
                       ifelse(.data$offense_play == .data$away & .data$kickoff_play == 1, .data$home, .data$offense_play)),
      def_pos_team = ifelse(.data$pos_team == .data$home, .data$away, .data$home),
      lead_pos_team = dplyr::lead(.data$pos_team, 1),
      lead_pos_team2 = dplyr::lead(.data$pos_team, 2),
      pos_team_timeouts_rem = ifelse(.data$kickoff_play == 1, .data$def_timeouts_rem_before,
                                .data$off_timeouts_rem_before),
      def_pos_team_timeouts_rem = ifelse(.data$kickoff_play == 1, .data$off_timeouts_rem_before,
                                .data$def_timeouts_rem_before),
      #-- Change of pos_team by lead('pos_team', 1)----
      change_of_pos_team = ifelse(.data$pos_team == .data$lead_pos_team & 
                                .data$lead_play_type != "End Period", 0,
                              ifelse(.data$pos_team == .data$lead_pos_team2 & 
                                       .data$lead_play_type == "End Period", 0, 1)),
      change_of_pos_team = ifelse(is.na(.data$change_of_pos_team), 0, .data$change_of_pos_team)
        
    )
  return(play_df)
}

#' Create new Drive results and id data
#' Cleans Play-by-Play data pulled from the API's raw game data
#'
#' @param play_df (\emph{data.frame} required): Performs data cleansing on Play-by-Play DataFrame, as pulled from `cfb_pbp_dat()`
#' @return The original `play_df` with the following columns appended/redefined:
#' \describe{
#' \item{lag_change_of_poss}{.}
#' \item{lag_punt}{.}
#' \item{lag_scoring_play}{.}
#' \item{lag_turnover_vec}{.}
#' \item{lag_downs_turnover}{.}
#' \item{lead_play_type}{.}
#' \item{lead_play_type2}{.}
#' \item{lead_play_type3}{.}
#' \item{drive_numbers}{.}
#' \item{number_of_drives}{.}
#' \item{pts_scored}{.}
#' \item{new_drive_result}{.}
#' \item{new_drive_pts}{.}
#' \item{drive_scoring}{.}
#' \item{new_drive_result2}{.}
#' \item{drive_play}{.}
#' \item{drive_play_number}{.}
#' \item{new_id}{.}
#' \item{log_ydstogo}{.}
#' \item{down}{.}
#' \item{distance}{.}
#' \item{yards_to_goal}{.}
#' \item{yards_gained}{.}
#' \item{Goal_To_Go}{.}
#' }
#' @keywords internal
#' @importFrom rlang ".data"
#' @importFrom dplyr "group_by" "arrange" "mutate" "ungroup" "case_when" "select" "lead" "lag"
#' @importFrom stringr "str_detect"
#' @import tidyr
#' @export
#'

clean_drive_dat <- function(play_df) {
  
  play_df <- play_df %>% 
    dplyr::group_by(.data$game_id, .data$half) %>% 
    dplyr::arrange(.data$game_id, .data$half, .data$period,
                   -.data$TimeSecsRem, -.data$lead_TimeSecsRem, .data$id_play, .by_group = TRUE) %>% 
    dplyr::mutate(
      lag_change_of_poss = ifelse(.data$half_play_number == 1, 0, dplyr::lag(.data$change_of_poss, 1)),
      lag_punt = ifelse(.data$half_play_number == 1, 0, dplyr::lag(.data$punt, 1)),
      lag_scoring_play = ifelse(.data$half_play_number == 1, 0, dplyr::lag(.data$scoring_play, 1)),
      lag_turnover_vec = ifelse(.data$half_play_number == 1, 0, dplyr::lag(.data$turnover_vec, 1)),
      lag_downs_turnover = ifelse(.data$half_play_number == 1, 0, dplyr::lag(.data$downs_turnover, 1)),
      lead_play_type = dplyr::lead(.data$play_type, 1),
      lead_play_type2 = dplyr::lead(.data$play_type, 2),
      lead_play_type3 = dplyr::lead(.data$play_type, 3),
      drive_numbers = ifelse(.data$half_play_number == 1 & .data$play_type != "Timeout", 1,
                             ifelse(.data$lag_change_of_poss == 1 &
                                      (.data$lag_punt == 1 | .data$lag_downs_turnover == 1 |
                                         .data$lag_turnover_vec == 1), 1,
                                    ifelse(.data$lag_scoring_play == 1 & .data$kickoff_play == 1, 1, 0))),
      number_of_drives = cumsum(.data$drive_numbers),
      pts_scored = dplyr::case_when(
        .data$play_type == "Blocked Field Goal Touchdown" ~ -7,
        .data$play_type == "Blocked Punt (Safety)" & .data$safety == 1 ~ -2,
        .data$play_type == "Blocked Punt" & .data$safety == 1 ~ -2,
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
        .data$play_type == "Kickoff (Safety)" & .data$kickoff_safety == 1 ~ 2,
        .data$play_type == "Kickoff Return Touchdown" ~ -7,
        .data$play_type == "Kickoff Touchdown" ~ 7,
        .data$play_type == "Field Goal Good" ~ 3,
        .data$play_type == "Pass Reception Touchdown" ~ 7,
        .data$play_type == "Fumble Recovery (Own) Touchdown" ~ 7,
        TRUE ~ 0),
      new_drive_result = dplyr::case_when(
        .data$downs_turnover == 1 ~ "Downs Turnover",
        .data$play_type == "Punt" ~ "Punt",
        .data$play_type == "Blocked Punt (Safety)" & .data$safety == 1 ~ "Safety",
        .data$play_type == "Blocked Punt" & .data$safety == 1 ~ "Safety",
        .data$play_type == "Blocked Punt" ~ "Blocked Punt",
        .data$play_type == "Blocked Punt Touchdown" ~ "Blocked Punt Touchdown",
        .data$play_type == "Punt Touchdown" ~ "Punt Touchdown",
        .data$play_type == "Punt Return Touchdown" ~ "Punt Return Touchdown",
        .data$play_type == "Fumble Recovery (Opponent) Touchdown" ~ "Fumble Recovery (Opponent) Touchdown",
        .data$play_type == "Fumble Return Touchdown" ~ "Fumble Return Touchdown",
        .data$play_type == "Fumble Recovery (Opponent)" ~ "Fumble",
        .data$play_type == "Fumble Recovery (Own) Touchdown" ~  "Fumble Recovery (Own) Touchdown",
        .data$play_type == "Interception Return Touchdown" ~ "Interception Return Touchdown",
        .data$play_type == "Interception Return" ~ "Interception Return",
        .data$play_type == "Sack Touchdown" ~ "Sack Touchdown",
        .data$play_type == "Safety" & .data$kickoff_play == 0 ~ "Safety",
        .data$play_type == "Kickoff (Safety)" & .data$kickoff_safety == 1 ~ "Kickoff Safety",
        .data$play_type == "Kickoff" & .data$kickoff_safety == 1 ~ "Kickoff Safety",
        .data$play_type == "Kickoff Return Touchdown" ~ "Kickoff Return Touchdown",
        .data$play_type == "Kickoff Touchdown" ~ "Kickoff Touchdown",
        .data$play_type == "Passing Touchdown" ~ "Passing Touchdown",
        .data$play_type == "Pass Reception Touchdown" ~ "Passing Touchdown",
        .data$play_type == "Rushing Touchdown" ~ "Rushing Touchdown",
        .data$play_type == "Field Goal Good" ~ "Field Goal Good",
        .data$play_type == "Field Goal Missed" ~ "Field Goal Missed",
        .data$play_type == "Missed Field Goal Return" ~ "Missed Field Goal Return",
        .data$play_type == "Blocked Field Goal Touchdown" ~ "Blocked Field Goal Touchdown",
        .data$play_type == "Missed Field Goal Return Touchdown" ~ "Missed Field Goal Return Touchdown",
        .data$play_type == "Blocked Field Goal" ~ "Blocked Field Goal",
        .data$play_type == "Uncategorized Touchdown" ~ "Uncategorized Touchdown",
        .data$play_type == "End of Half" ~ "End Half",
        .data$play_type == "End of Game" ~ "End Game",
        .data$scoring_play == 0 & (.data$lead_TimeSecsRem == 0 | is.na(.data$lead_TimeSecsRem)) & period %in% c(2, 4)  ~ "End Half",
        TRUE ~ NA_character_),
      new_drive_pts = dplyr::case_when(
        .data$downs_turnover == 1 ~ 0,
        .data$play_type == "Punt" ~ 0,
        .data$play_type == "Blocked Punt (Safety)" & .data$safety == 1 ~ -2,
        .data$play_type == "Blocked Punt" & .data$safety == 1 ~ -2,
        .data$play_type == "Blocked Punt" ~ 0,
        .data$play_type == "Blocked Punt Touchdown" ~ -7,
        .data$play_type == "Punt Touchdown" ~ 7,
        .data$play_type == "Punt Return Touchdown" ~ -7,
        .data$play_type == "Fumble Recovery (Opponent) Touchdown" ~ -7,
        .data$play_type == "Fumble Return Touchdown" ~ -7,
        .data$play_type == "Fumble Recovery (Opponent)" ~ 0,
        .data$play_type == "Fumble Recovery (Own) Touchdown" ~  7,
        .data$play_type == "Interception Return Touchdown" ~ -7,
        .data$play_type == "Interception Return" ~ 0,
        .data$play_type == "Sack Touchdown" ~ -7,
        .data$play_type == "Safety" & .data$kickoff_play == 0 ~ -2,
        .data$play_type == "Kickoff (Safety)" & .data$kickoff_safety == 1 ~ 2,
        .data$play_type == "Kickoff" & .data$kickoff_safety == 1 ~ 2,
        .data$play_type == "Passing Touchdown" ~ 7,
        .data$play_type == "Pass Reception Touchdown" ~ 7,
        .data$play_type == "Rushing Touchdown" ~ 7,
        .data$play_type == "Field Goal Good" ~ 3,
        .data$play_type == "Field Goal Missed" ~ 0,
        .data$play_type == "Missed Field Goal Return" ~ 0,
        .data$play_type == "Blocked Field Goal Touchdown" ~ -7,
        .data$play_type == "Missed Field Goal Return Touchdown" ~ -7,
        .data$play_type == "Blocked Field Goal" ~ 0,
        .data$play_type == "Uncategorized Touchdown" ~ 7,
        .data$play_type == "End of Half" ~ 0,
        .data$play_type == "End of Game" ~ 0,
        .data$scoring_play == 0 & .data$TimeSecsRem == 0 & period %in% c(2, 4) ~ 0,
        TRUE ~ 0),
      new_drive_pts = ifelse(.data$new_drive_pts == 0, NA, .data$new_drive_pts),
      drive_scoring = ifelse(.data$new_drive_pts != 0, .data$scoring_play, NA_integer_)) %>% 
    dplyr::ungroup() %>% 
    dplyr::group_by(.data$game_id) %>% 
    dplyr::arrange(.data$game_id, .data$half, .data$period,
                   -.data$TimeSecsRem, -.data$lead_TimeSecsRem, 
                   .data$id_play, .by_group = TRUE) %>% 
    dplyr::mutate(drive_num = cumsum(.data$drive_numbers)) %>% 
    dplyr::group_by(.data$game_id, .data$half, .data$drive_num) %>% 
    tidyr::fill(.data$new_drive_result, .direction = c("updown")) %>% 
    tidyr::fill(.data$drive_scoring, .direction = c("updown")) %>% 
    tidyr::fill(.data$new_drive_pts, .direction = c("updown")) %>% 
    dplyr::mutate(
      new_drive_result2 = ifelse(is.na(.data$new_drive_result), "Uncategorized", .data$new_drive_result),
      drive_scoring = ifelse(is.na(.data$drive_scoring), 0, .data$drive_scoring),
      new_drive_pts = ifelse(is.na(.data$new_drive_pts), 0, .data$new_drive_pts)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(id_drive = paste0(.data$game_id, .data$drive_num)) %>% 
    dplyr::group_by(.data$game_id, .data$id_drive) %>% 
    dplyr::arrange(.data$game_id, .data$half, .data$period,
                   -.data$TimeSecsRem, -.data$lead_TimeSecsRem, .data$id_play, .by_group = TRUE) %>% 
    dplyr::mutate(
      drive_play = ifelse(!(.data$play_type %in% c("End Period", "End of Half", "Timeout")), 1, 0),
      drive_play_number = cumsum(.data$drive_play)) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(-.data$td_check,-.data$td_play) %>% 
    dplyr::mutate(
      new_id = gsub(pattern = unique(.data$game_id), "", x = .data$id_play),
      new_id = as.numeric(.data$new_id),
      log_ydstogo = ifelse(.data$distance == 0|is.nan(log(.data$distance)),log(1),log(.data$distance)),
      down = ifelse(stringr::str_detect(.data$play_type, "Kickoff"), 1, .data$down),
      distance = ifelse(stringr::str_detect(.data$play_type, "Kickoff"), 10, .data$distance),
      yards_to_goal = as.numeric(.data$yards_to_goal),
      yards_gained = as.numeric(.data$yards_gained),
      Goal_To_Go = ifelse(stringr::str_detect(.data$play_type, "Field Goal"),
                          .data$distance >= (.data$yards_to_goal - 17),
                          .data$distance >= .data$yards_to_goal))
    
  return(play_df)
}


#' Clean Drive Information
#' Cleans CFB (D-I) Drive-By-Drive Data to create `pts_drive` column
#'
#' @param drive_df (\emph{data.frame} required) Drive dataframe pulled from API via the `cfb_drives()` function
#' @details Cleans CFB (D-I) Drive-By-Drive Data to create `pts_drive` column. Requires the following columns be present:
#' \itemize{
#' \item{drive_id}{Returned as `drive_id`}
#' \item{drive_result}{End result of the drive}
#' \item{scoring}{Logical flag for if drive was a scoring drive}
#' \item{game_id}{Unique game identifier}
#' }
#' @return The original `drive_df` with the following columns appended to it:
#' \describe{
#' \item{drive_id}{Returned as `drive_id` from original variable `drive_id`}
#' \item{pts_drive}{End result of the drive}
#' \item{scoring}{Logical flag for if drive was a scoring drive updated}
#' }
#' @keywords internal
#' @importFrom rlang ".data"
#' @importFrom stringr "str_detect"
#' @importFrom dplyr "mutate" "arrange" "case_when"
#' @export
#'

clean_drive_info <- function(drive_df){
  
  clean_drive = drive_df %>%
    dplyr::mutate(
      pts_drive = dplyr::case_when(
        .data$drive_result == "TD" ~ 7,
        stringr::str_detect(.data$drive_result,"SF") ~ -2,
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
        stringr::str_detect(.data$drive_result,"TD") ~ 7,
        TRUE ~ 0),
      scoring = ifelse(.data$pts_drive != 0, TRUE, .data$scoring)) %>%
    dplyr::mutate(drive_id = as.numeric(.data$drive_id)) %>%
    dplyr::arrange(.data$game_id, .data$drive_id)
  
  return(clean_drive)
}

#' Creates the post-play inputs for the Expected Points model to predict on for each game
#'
#' @param dat (\emph{Data.Frame} required) Clean Play-by-Play DataFrame pulled from `cfb_pbp_dat()`
#' @details Prep for EPA calculations at the end of the play. Requires the following columns be present:
#' \itemize{
#' \item{game_id}
#' \item{id_play}
#' \item{drive_id}
#' \item{down}
#' \item{distance}
#' \item{period}
#' \item{yards_to_goal}
#' \item{play_type}
#' }
#' @return `dat` with the following columns appended/modified:
#' \itemize{
#'  \item{turnover_indicator}{.}
#'  \item{down}{.}
#'  \item{new_id}{.}
#'  \item{new_down}{.}
#'  \item{distance}{.}
#'  \item{yards_to_goal}{.}
#'  \item{yards_gained}{.}
#'  \item{turnover}{.}
#'  \item{drive_start_yards_to_goal}{.}
#'  \item{end_of_half}{.}
#'  \item{new_yardline}{.}
#'  \item{new_distance}{.}
#'  \item{new_log_ydstogo}{.}
#'  \item{new_Goal_To_Go}{.}
#'  \item{new_TimeSecsRem}{.}
#'  \item{new_Under_two}{.}
#'  \item{first_by_penalty}{.}
#'  \item{first_by_yards}{.}
#'  \item{firstD_by_poss}{.}
#'  \item{firstD_by_yards}{.}
#'  \item{firstD_by_penalty}{.}
#'  \item{yds_punted}{.}
#'  \item{yds_punt_gained}{.}
#'  \item{missing_yard_flag}{.}
#' }
#' @keywords internal
#' @importFrom rlang ".data"
#' @importFrom stringi "stri_extract_first_regex"
#' @importFrom dplyr "mutate" "arrange" "group_by" "case_when" "mutate_at" "ungroup"
#' @importFrom stringr "str_extract"
#' @import tidyr
#' @export
#'

prep_epa_df_after <- function(dat) {
  ##--Play type vectors------
  turnover_play_type = c(
    "Blocked Field Goal",
    "Blocked Field Goal Touchdown",
    "Blocked Punt",
    "Blocked Punt Touchdown",
    "Punt",
    "Punt Touchdown",
    "Punt Return Touchdown",
    "Sack Touchdown",
    "Field Goal Missed",
    "Missed Field Goal Return",
    "Missed Field Goal Return Touchdown",
    "Fumble Recovery (Opponent)",
    "Fumble Recovery (Opponent) Touchdown",
    "Interception",
    "Interception Return",
    "Interception Return Touchdown",
    "Pass Interception Return",
    "Pass Interception Return Touchdown",
    "Uncategorized Touchdown"
  )
  defense_score_vec = c(
    "Blocked Punt Touchdown",
    "Blocked Field Goal Touchdown",
    "Missed Field Goal Return Touchdown",
    "Punt Return Touchdown",
    "Fumble Recovery (Opponent) Touchdown",
    "Fumble Return Touchdown",
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
    "Interception Return",
    "Interception Return Touchdown",
    "Pass Interception Return",
    "Pass Interception Return Touchdown",
    "Punt",
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
  penalty = c(
    'Penalty',
    'Penalty (Kickoff)'
  )
  score = c(
    "Passing Touchdown",
    "Rushing Touchdown",
    "Field Goal Good",
    "Pass Reception Touchdown",
    "Fumble Recovery (Own) Touchdown",
    "Punt Touchdown"
  )
  kickoff = c(
    "Kickoff",
    "Kickoff Return (Offense)",
    "Kickoff Return Touchdown",
    "Kickoff Touchdown",
    "Kickoff Team Fumble Recovery",
    "Kickoff Team Fumble Recovery Touchdown",
    "Kickoff (Safety)",
    "Penalty (Kickoff)"
  )
  
  turnover_ind = dat$play_type %in% turnover_play_type
  dat$turnover = 0
  #define turnover on downs
  downs_turnover = (dat$play_type %in% normalplay & dat$yards_gained < dat$distance & dat$down == 4)
  # data is ordered
  new_offense = !(dat$offense_play == dplyr::lead(dat$offense_play, order_by = dat$id_play))
  scoring_plays = dat$play_type %in% score
  # end of half check as well
  end_of_half_plays = !(dat$half == dplyr::lead(dat$half, order_by = dat$id_play))
  # is specifically defined as a turnover
  turnover_play_check = dat$play_type %in% turnover_vec
  # turnovers only occur on actual change of offense
  # but not scoring plays
  # and not at the end of half.
  # Turnovers now capture downs, when there is a change of offense after a fourth down normal play.
  t_ind = (turnover_ind | (new_offense)) & 
    !scoring_plays & !end_of_half_plays & 
    (turnover_play_check | downs_turnover)
  
  dat$turnover[t_ind] <- 1
  
  dat = dat %>% 
    dplyr::ungroup() %>% 
    dplyr::group_by(.data$game_id, .data$half) %>%
    dplyr::arrange(.data$id_play, .by_group = TRUE) %>%
    dplyr::mutate(
      lead_yards_to_goal = dplyr::lead(.data$yards_to_goal, 1),
      turnover_indicator = 
        ifelse(
          (.data$play_type %in% defense_score_vec) | (.data$play_type %in% turnover_vec )|
            (.data$play_type %in% normalplay & .data$yards_gained < .data$distance & 
               .data$down == 4), 1, 0),
      # downs_turnover = ifelse((.data$play_type %in% normalplay) & (.data$yards_gained < .data$distance) & (.data$down == 4),1,0),
      down = as.numeric(.data$down),
      yards_gained = as.numeric(.data$yards_gained),
      #--New Down-----
      new_down = as.numeric(dplyr::case_when(
        #--- turnovers ---
        .data$turnover == 1 ~ 1,
        ##--Penalty Cases (new_down)-----
        # 8 cases with three T/F penalty flags
        # 4 cases in 1
        .data$play_type %in% penalty & .data$penalty_1st_conv ~ 1,
        # offsetting penalties, no penalties declined, no 1st down by penalty (1 case)
        .data$play_type %in% penalty & !.data$penalty_declined &
          .data$penalty_offset & !.data$penalty_1st_conv ~ .data$down,
        # offsetting penalties, penalty declined true, no 1st down by penalty
        # seems like it would be a regular play at that point (1 case, split in three)
        .data$play_type %in% penalty & .data$penalty_declined &
          .data$penalty_offset & !.data$penalty_1st_conv &
          .data$yards_gained < .data$distance & .data$down <= 3 ~ .data$down+1,
        .data$play_type %in% penalty & .data$penalty_declined &
          .data$penalty_offset & !.data$penalty_1st_conv &
          .data$yards_gained < .data$distance & .data$down == 4 ~ 1,
        .data$play_type %in% penalty & .data$penalty_declined &
          .data$penalty_offset & !.data$penalty_1st_conv &
          .data$yards_gained >= .data$distance ~ 1,
        # only penalty declined true, same logic as prior (1 case, split in three)
        .data$play_type %in% penalty & .data$penalty_declined &
          !.data$penalty_offset & !.data$penalty_1st_conv &
          .data$yards_gained < .data$distance & .data$down <= 3 ~ .data$down+1,
        .data$play_type %in% penalty & .data$penalty_declined &
          !.data$penalty_offset & !.data$penalty_1st_conv &
          .data$yards_gained < .data$distance & .data$down == 4 ~ 1,
        .data$play_type %in% penalty & .data$penalty_declined &
          !.data$penalty_offset & !.data$penalty_1st_conv &
          .data$yards_gained >= .data$distance ~ 1,
        # no other penalty flags true, lead on down (1 case)
        .data$play_type %in% penalty & !.data$penalty_declined &
          !.data$penalty_offset & !.data$penalty_1st_conv ~ dplyr::lead(.data$down, order_by=id_play),
        ##--Scores, kickoffs,  defensive scores----
        .data$play_type %in% score ~ 1,
        .data$play_type %in% kickoff ~ 1,
        .data$play_type %in% defense_score_vec ~ 1,
        ##--Regular Plays----
        # regular play 1st down
        .data$play_type %in% normalplay & .data$yards_gained >= .data$distance ~ 1,
        # iterate to next down due to not meeting the yards to gain
        .data$play_type %in% normalplay & .data$yards_gained < .data$distance & .data$down <= 3 ~ as.integer(.data$down) + 1,
        # turnover on downs
        .data$play_type %in% normalplay & .data$yards_gained < .data$distance & .data$down == 4 ~ 1
      )),
      drive_start_yards_to_goal = as.numeric(.data$drive_start_yards_to_goal),
      #--New Distance-----
      new_distance = as.numeric(dplyr::case_when(
        ##--turnovers, defensive scores, scores, kickoffs
        .data$turnover == 1 ~ 10,
        ##--Penalty cases (new_distance)
        #--offsetting penalties, keep same distance
        .data$play_type %in% penalty &
          .data$penalty_offset ~ as.numeric(.data$distance),
        #--penalty first down conversions, 10 or to goal
        .data$play_type %in% penalty & 
          .data$penalty_1st_conv ~ as.numeric(ifelse(.data$yards_to_goal  - .data$yards_gained <= 10,
                                                     as.numeric(.data$yards_to_goal),10)),
        #--penalty without first down conversion
        .data$play_type %in% penalty & !.data$penalty_declined &
          !.data$penalty_1st_conv &
          !.data$penalty_offset ~ as.numeric(ifelse((.data$yards_gained >= .data$distance) &
                                                      (.data$yards_to_goal - .data$yards_gained <= 10),
                                                    as.numeric(.data$yards_to_goal),10)),
        ##--normal plays
        .data$play_type %in% normalplay &
          .data$yards_gained >= .data$distance &
          (.data$yards_to_goal - .data$yards_gained >= 10) ~ 10,
        .data$play_type %in% normalplay &
          .data$yards_gained >= .data$distance &
          (.data$yards_to_goal  - .data$yards_gained <= 10) ~ as.numeric(.data$yards_to_goal),
        .data$play_type %in% normalplay &
          .data$yards_gained < .data$distance & down <= 3 ~ as.numeric(.data$distance - .data$yards_gained),
        .data$play_type %in% normalplay &
          .data$yards_gained < .data$distance &
          .data$down == 4 & (100 - (.data$yards_to_goal  - .data$yards_gained) >= 10) ~ 10,
        .data$play_type %in% normalplay &
          .data$yards_gained < .data$distance &
          .data$down == 4 &
          (100 - (.data$yards_to_goal  - .data$yards_gained) <= 10) ~ as.numeric(100 - .data$yards_to_goal),
        .data$play_type %in% defense_score_vec ~ 0,
        .data$play_type %in% score ~ 0,
        .data$play_type %in% kickoff ~ 10
      )),
      #--New Yardline----
      new_yardline = as.numeric(dplyr::case_when(
        .data$downs_turnover == 1 ~ 100 - .data$yards_to_goal + .data$yards_gained,
        .data$play_type %in% penalty & .data$penalty_offset ~ .data$yards_to_goal,
        .data$play_type %in% penalty & !.data$penalty_offset ~ .data$yards_to_goal - .data$yards_gained,
        .data$play_type %in% normalplay ~ .data$yards_to_goal - .data$yards_gained,
        .data$play_type %in% score ~ 0,
        .data$play_type %in% defense_score_vec ~ 0,
        .data$play_type %in% kickoff ~ .data$lead_yards_to_goal,
        .data$play_type %in% turnover_vec ~ 100 - .data$yards_to_goal + .data$yards_gained
      )),
      new_TimeSecsRem = ifelse(!is.na(dplyr::lead(.data$TimeSecsRem, order_by=.data$id_play)),
                               dplyr::lead(.data$TimeSecsRem,order_by=.data$id_play),0),
      new_log_ydstogo = if_else(.data$new_distance == 0, log(1), log(.data$new_distance)),
      new_Goal_To_Go = ifelse(.data$new_yardline <= .data$new_distance, TRUE, FALSE),
      # new under two minute warnings
      new_Under_two = .data$new_TimeSecsRem <= 180) %>%
    dplyr::mutate_at(vars(.data$new_TimeSecsRem), ~ replace_na(., 0)) %>% 
    dplyr::group_by(.data$game_id,.data$half,.data$drive_id) %>% 
    dplyr::arrange(.data$id_play, .by_group = TRUE) %>%
    dplyr::mutate(
      first_by_penalty = ifelse(.data$play_type %in% penalty & .data$penalty_1st_conv, 1,
                                ifelse(.data$play_type %in% penalty & .data$penalty_declined &
                                         .data$penalty_offset & !.data$penalty_1st_conv &
                                         .data$yards_gained > .data$distance, 1,
                                       ifelse(.data$play_type %in% penalty & .data$penalty_declined &
                                                !.data$penalty_offset & !.data$penalty_1st_conv &
                                                .data$yards_gained >= .data$distance, 1, 0))),
      first_by_yards = ifelse(.data$play_type %in% normalplay & 
                                .data$yards_gained >= .data$distance, 1, 0),
      firstD_by_poss = ifelse((.data$drive_play_number == 1 & .data$kickoff_play != 1)|
                                (.data$drive_play_number == 2 & dplyr::lag(.data$kickoff_play, 1) == 1), 1, 0),
      firstD_by_penalty = ifelse(dplyr::lag(.data$first_by_penalty, 1)==1 & .data$kickoff_play != 1, 1, 0),
      firstD_by_yards = ifelse(dplyr::lag(.data$first_by_yards, 1)==1 & .data$kickoff_play != 1, 1, 0),
      new_yardline = ifelse((.data$play_type == 'Blocked Punt'), 
                            dplyr::lead(.data$yards_to_goal, 1), .data$new_yardline),
      new_id = .data$id_play) %>%  
    dplyr::ungroup() %>% 
    dplyr::arrange(.data$new_id, .by_group = TRUE) %>%
    dplyr::select(-.data$play, -.data$half_play, -.data$drive_play) %>% 
    dplyr::mutate(
      #--Punt Plays--------------------------
      new_down = ifelse(.data$punt == 1|.data$kickoff_play == 1, 1, .data$new_down),
      new_distance = ifelse(.data$punt == 1, 10, .data$new_distance),
      new_log_ydstogo = ifelse(.data$punt == 1, log(10), .data$new_log_ydstogo),
      new_Goal_To_Go = ifelse(.data$punt == 1, FALSE, .data$new_Goal_To_Go),
      new_yardline = ifelse(.data$punt == 1 & .data$punt_tb == 1, 80, .data$new_yardline),
      new_yardline = ifelse(.data$punt == 1 & .data$punt_tb == 0, 
                            100 - .data$yards_to_goal +
                              .data$yds_punted - .data$yds_punt_gained, .data$new_yardline),
      # new_score_diff_start = .data$score_diff,
      new_down = ifelse(.data$kickoff_play == 1, 1, .data$new_down),
      new_yardline = ifelse(.data$kickoff_play == 1 & .data$kickoff_tb == 1, 75, .data$new_yardline)
    )
  
  #--End of Half Plays--------------------------
  end_of_half_plays = (dat$new_TimeSecsRem == 0)
  if (any(end_of_half_plays)) {
    dat$new_yardline[end_of_half_plays] <- 100
    dat$new_down[end_of_half_plays] <- 4
    dat$new_distance[end_of_half_plays] <- 100
    dat$end_of_half[end_of_half_plays] <- 1
    dat$new_log_ydstogo[end_of_half_plays] <- log(100)
    dat$new_Under_two[end_of_half_plays] <- dat$new_TimeSecsRem[end_of_half_plays] <= 180
  }
  
  # missed field goal needs to be here
  # needs to go before the na check to set to 99
  dat = dat %>% 
    dplyr::mutate(
      new_yardline = if_else(is.na(.data$new_yardline) &
        .data$play_type %in% c("Field Goal Missed", "Blocked Field Goal"),
        100 - (.data$yards_to_goal - 9),
        .data$new_yardline))
  
  #--General weird plays that don't have an easy fix----
  na_yd_line = which(is.na(dat$new_yardline) | dat$new_yardline >= 100)
  dat$new_yardline[na_yd_line] = dat$yard_line[na_yd_line+1]
  neg_distance = which(dat$new_distance < 0)
  dat$new_distance[neg_distance] = dat$distance[neg_distance+1]
  dat$new_log_ydstogo[neg_distance] = log(dat$new_distance[neg_distance])
  
  #--Missing yd_line Plays--------------------------
  missing_yd_line = dat$new_yardline == 0
  dat$new_yardline[missing_yd_line] = 100
  dat$new_log_ydstogo[missing_yd_line] = log(100)
  dat$new_down[missing_yd_line] = 1
  dat$missing_yard_flag <- FALSE
  dat$missing_yard_flag[missing_yd_line] <- TRUE
  
  dat = dat %>%
    dplyr::arrange(.data$new_id) %>%
    dplyr::mutate(new_id = gsub(pattern = unique(.data$game_id), "", x = .data$new_id),
                  new_id = as.numeric(.data$new_id))
  
  
  
  return(dat)
}


#' Add Betting columns
#' This is only for DI-FBS football
#'
#' @param play_df (\emph{data.frame} required): Play-By-Play dataframe as pulled from `cfb_pbp_dat()`, `clean_pbp_dat()`,`penalty_detection()`
#' @param g_id (\emph{Integer} optional): Game ID filter for querying a single game
#' Can be found using the `cfb_game_info()` function
#' @param yr (\emph{Integer} optional): Select year (example: 2018)
#' @details Add Betting columns. Requires the following parameters to be present:
#' \itemize{
#' \item{play_df}{Play-By-Play dataframe as pulled from `cfb_pbp_dat()` and `clean_pbp_dat()`}
#' \item{g_id}{Unique game identifier - `game_id`}
#' \item{yr}{Year parameter}
#' }
#' @return The original `play_df` with the following columns appended to it:
#' \describe{
#' \item{spread}{The spread for the game}
#' \item{formatted_spread}{Formatted spread with the favored team listed with the numeric spread}
#' }
#' @keywords internal
#' @importFrom rlang ".data"
#' @importFrom dplyr "mutate" "rename" "select" "left_join" "filter"
#' @importFrom glue "glue"
#' @export
#' 

add_betting_cols <- function(play_df, g_id, yr){
  tryCatch(
    expr = {
      game_spread <- cfb_betting_lines(game_id = g_id, year=yr) 
  
      game_spread <- game_spread %>%
        dplyr::filter(.data$provider == 'consensus') %>% 
        dplyr::mutate(spread = as.numeric(.data$spread)) %>% 
        dplyr::select(.data$game_id, .data$spread, .data$formatted_spread)
      
      play_df <- play_df %>%
        dplyr::left_join(game_spread, by=c('game_id'))
      },
    error = function(e) {
      message(glue::glue("{Sys.time()} - game_id {g_id}: Invalid arguments or no betting lines data available!"))
      },
    warning = function(w) {
      },
    finally = {
      }
  )
  return(play_df)
}