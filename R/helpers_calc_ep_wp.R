#' Select the columns needed for EP predictions
#' @param pbp (\emph{data.frame} required) Play-by-Play dataframe pulled from API via the `cfb_pbp_data()` function
#' @importFrom dplyr select 
#' @importFrom tidyr everything
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
      "Under_two",
      "pos_score_diff_start",
      tidyr::everything()
    )
  return(pbp)
  
}

#' Select the columns needed for EP predictions
#' @param pbp (\emph{data.frame} required) Play-by-Play dataframe pulled from API via the `cfb_pbp_data()` function
#' @importFrom dplyr select 
#' @importFrom tidyr everything
ep_model_select_check <- function(pbp) {
  
  pbp <- pbp %>%
    dplyr::select(
      "pos_team",
      "def_pos_team",
      "half",
      "period",
      "TimeSecsRem",
      "TimeSecsRem_end",
      "lag_play_type",
      "lead_play_type",
      "play_type",
      "play_text",
      "yards_to_goal",
      "yards_to_goal_end",
      "down",
      "down_end",
      "distance",
      "distance_end",
      "yards_gained",
      "change_of_pos_team",
      "pos_score_diff_start",
      "pos_score_diff_start_end",
      "pos_score_diff",
      "EPA",
      "ep_before",
      "ep_after",
      "log_ydstogo",
      "log_ydstogo_end",
      "Goal_To_Go",
      "Goal_To_Go_end",
      "Under_two",
      "Under_two_end",
      tidyr::everything()
    )
  return(pbp)
  
}

#' Select the columns needed for WP predictions
#' @param pbp (\emph{data.frame} required) Play-by-Play dataframe pulled from API via the `cfb_pbp_data()` function
#' @importFrom dplyr select 
#' @importFrom tidyr everything

wp_model_select <- function(pbp) {
  
  pbp <- pbp %>%
    dplyr::select(
      "offense_receives_2H_kickoff",
      "offense_play",
      "defense_play",
      "home",
      "away",
      "pos_team",
      "def_pos_team",
      "pos_score_diff",
      "pos_score_diff_start",
      "score_diff",
      "score_diff_start",
      "half",
      "TimeSecsRem",
      "adj_TimeSecsRem",
      "ep_before",
      "ExpScoreDiff",
      "ExpScoreDiff_Time_Ratio",
      "down",
      "distance",
      "yards_to_goal",
      "pos_team_timeouts_rem_before",
      "def_pos_team_timeouts_rem_before",
      "play_type",
      "play_text",
      tidyr::everything()
    )
  
  return(pbp)
  
}


#' Select the columns needed for WP predictions
#' @param pbp (\emph{data.frame} required) Play-by-Play dataframe pulled from API via the `cfb_pbp_data()` function
#' @importFrom dplyr select
#' @importFrom tidyr everything
wp_model_select_check <- function(pbp) {
  
  pbp <- pbp %>%
    dplyr::select(
      "offense_receives_2H_kickoff",
      "half",
      "period",
      "pos_team",
      "def_pos_team",
      
      "pos_score_diff",
      "pos_score_diff_start",
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
      "pos_team_timeouts_rem_before",
      "def_pos_team_timeouts_rem_before",
      "play_type",
      "play_text",
      "wp_before",
      "wp_after",
      "wpa",
      "wpa_base",
      "wpa_base_ind",
      "wpa_change",
      "wpa_change_ind",
      "wpa_base_nxt",
      "wpa_base_nxt_ind",
      "wpa_change_nxt",
      "wpa_change_nxt_ind",
      "lead_wp_before",
      "lead_wp_before2",
      tidyr::everything()
    )
  
  return(pbp)
  
}

#' Select the columns needed for EP predictions
#' @param pbp (\emph{data.frame} required) Play-by-Play dataframe pulled from API via the `cfb_pbp_data()` function
#' @importFrom dplyr select 
ep_fg_model_select <- function(pbp) {
  
  pbp <- pbp %>%
    dplyr::select(
      "fg_inds",
      "half",
      "TimeSecsRem",
      "down",
      "distance",
      "yards_to_goal",
      "log_ydstogo",
      "Goal_To_Go",
      "Under_two",
      "pos_score_diff_start"
    )
  return(pbp)
}

#' Select the columns needed for EP predictions
#' @param pbp (\emph{data.frame} required) Play-by-Play dataframe pulled from API via the `cfb_pbp_data()` function
#' @param ep_model (\emph{model} default cfbscrapR:::ep_model): Expected Points (EP) Model
#' @importFrom dplyr select 
#' @importFrom stats predict
get_preds_ep <- function(pbp, ep_model = cfbscrapR:::ep_model){
  
  preds <- stats::predict(ep_model,pbp)
  
}
