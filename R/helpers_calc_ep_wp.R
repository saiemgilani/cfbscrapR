#' Select the columns needed for EP predictions
#' @param pbp (\emph{data.frame} required) Play-by-Play dataframe pulled from API via the `cfb_pbp_data()` function
#' @importFrom dplyr "select"
ep_model_select <- function(pbp) {
  
  pbp <- pbp %>%
    dplyr::select(
      "half",
      "TimeSecsRem",
      "down",
      "distance",
      "yards_to_goal",
      "log_ydstogo",
      "Goal_To_Go",
      "Under_two"
    )
  return(pbp)
  
}



#' Select the columns needed for WP predictions
#' @param pbp (\emph{data.frame} required) Play-by-Play dataframe pulled from API via the `cfb_pbp_data()` function
#' @importFrom dplyr "select"
wp_model_select <- function(pbp) {
  
  pbp <- pbp %>%
    dplyr::select(
      "offense_receives_2H_kickoff",
      "offense_play",
      "defense_play",
      "home",
      "half",
      "TimeSecsRem",
      "adj_TimeSecsRem",
      "ep_before",
      "score_diff",
      "score_diff_start",
      "ExpScoreDiff",
      "ExpScoreDiff_Time_Ratio",
      "down",
      "distance",
      "yards_to_goal",
      "off_timeouts_rem_before",
      "def_timeouts_rem_before",
      "play_type",
      "play_text"
    )
  
  return(pbp)
  
}


#' Select the columns needed for WP predictions
#' @param pbp (\emph{data.frame} required) Play-by-Play dataframe pulled from API via the `cfb_pbp_data()` function
#' @importFrom dplyr "select"
wp_model_select_check <- function(pbp) {
  
  pbp <- pbp %>%
    dplyr::select(
      "offense_receives_2H_kickoff",
      "half",
      "period",
      "TimeSecsRem",
      "adj_TimeSecsRem",
      "ExpScoreDiff",
      "ExpScoreDiff_Time_Ratio",
      "ep_before",
      "score_diff",
      "score_diff_start",
      "down",
      "distance",
      "yards_to_goal",
      "half",
      "home",
      "off_timeouts_rem_before",
      "def_timeouts_rem_before",
      "play_type",
      "play_text",
      "wp_before",
      "wp_after",
      "wpa",
      "wpa_change_ind",
      "wpa_change",
      "wpa_base_ind",
      "wpa_base",
      "wpa_base_nxt",
      "wpa_change_nxt",
      "lead_wp_before",
      "lead_wp_before2",
      
    )
  
  return(pbp)
  
}
