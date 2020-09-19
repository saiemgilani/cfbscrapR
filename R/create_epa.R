#' Create EPA
#' Adds Expected Points calculations to Play-by-Play data.frame
#'
#' @param clean_pbp_dat (\emph{data.frame} required): Clean PBP as input from \code{\link[cfbscrapR:cfb_pbp_data]{cfbscrapR::cfb_pbp_data()}})
#' @param ep_model (\emph{model} default cfbscrapR:ep_model): Expected Points (EP) Model
#' @param fg_model (\emph{model} default cfbscrapR:fg_model): Field Goal (FG) Model
#' @keywords internal
#' @importFrom stats "na.omit"
#' @importFrom stats "predict"
#' @importFrom nnet "multinom"
#' @importFrom rlang ".data"
#' @import stringr
#' @import dplyr
#' @import tidyr
#' @export
#'

create_epa <- function(clean_pbp_dat,
                       ep_model=cfbscrapR:::ep_model,
                       fg_model=cfbscrapR:::fg_model) {
  #----------------- Code Description--------
  ## 1) prep_epa_df_before: Use map_dfr to prep before play model variables -> Make predictions 
  ## 2) prep_epa_df_after: Use map_dfr to prep after play model variables -> Make predictions
  ## 3) 
  ##
  ##
  ##
  ##
  ##
  # if you are trying to deal with multiple games at once
  # then you have to get the after individually.
  g_ids = sort(unique(clean_pbp_dat$game_id))
  clean_pbp_dat = purrr::map_dfr(g_ids,
                                 function(x) {
                                   clean_pbp_dat %>%
                                     dplyr::filter(.data$game_id == x) %>%
                                     prep_epa_df_before()
                                 })
  
  pred_df = clean_pbp_dat %>% 
    dplyr::select(.data$new_id, .data$drive_id, .data$game_id,
                  .data$TimeSecsRem,
                  .data$down, 
                  .data$distance,
                  .data$yards_to_goal, 
                  .data$log_ydstogo,
                  .data$Under_two, 
                  .data$Goal_To_Go) %>%
    dplyr::mutate(down = as.factor(.data$down))
  
  # ep_start - make predictions on pred_df_before (the output from `prep_epa_df_before()`)
  ep_start = as.data.frame(predict(ep_model, pred_df, type = 'prob'))
  colnames(ep_start) <- ep_model$lev
  ep_start_update = epa_fg_probs(dat = clean_pbp_dat,
                                 current_probs = ep_start,
                                 fg_mod = fg_model)
  weights = c(0, 3, -3, -2, -7, 2, 7)
  pred_df$ep_before = apply(ep_start_update, 1, function(row) {
    sum(row * weights)
  })
  
  # if you are trying to deal with multiple games at once
  # then you have to get the after individually.
  # get post play stuff per game
  g_ids = sort(unique(clean_pbp_dat$game_id))
  prep_df_after = purrr::map_dfr(g_ids,
                                 function(x) {
                                   clean_pbp_dat %>%
                                     dplyr::filter(.data$game_id == x) %>%
                                     prep_epa_df_after()
                                 })
  
  # predict ep_after
  ep_end = predict(ep_model, prep_df_after, type = 'prob')
  colnames(ep_end) <- ep_model$lev
  prep_df_after$ep_after = apply(ep_end, 1, function(row) {
    sum(row * weights)
  })
  
  # join together multiple dataframes back together
  # to get ep_before and ep_after for plays
  colnames(prep_df_after)[4:11] = paste0(colnames(prep_df_after)[4:11], "_end")
  pred_df = clean_pbp_dat %>%
    dplyr::left_join(prep_df_after,
                     by = c("game_id", "drive_id", "new_id")) %>%
    dplyr::left_join(
      pred_df %>% 
        dplyr::select(.data$new_id, .data$drive_id, .data$game_id, .data$ep_before),
      by = c("game_id", "drive_id", "new_id")
    )
  # constant vectors to be used again
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
    "Interception Return",
    "Interception Return Touchdown",
    "Pass Interception Return",
    "Pass Interception Return Touchdown",
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
  off_TD = c(
    "Passing Touchdown",
    "Rushing Touchdown",
    "Field Goal Good",
    "Pass Reception Touchdown",
    "Fumble Recovery (Own) Touchdown",
    "Punt Touchdown"
  )
  def_TD = c(
    "Defensive 2pt Conversion",
    "Safety",
    "Uncategorized Touchdown",
    "Blocked Field Goal Touchdown",
    "Missed Field Goal Return Touchdown",
    "Fumble Return Touchdown",
    "Fumble Recovery (Opponent) Touchdown",
    "Sack Touchdown",
    "Blocked Punt Touchdown",
    "Punt Touchdown",
    "Punt Return Touchdown",
    "Kickoff Return Touchdown",
    "Interception Return Touchdown",
    "Pass Interception Return Touchdown"
  )
  punt = c(
    "Blocked Punt",
    "Blocked Punt Touchdown",
    "Punt",
    "Punt Touchdown",
    "Punt Return Touchdown"
  )
  kickoff = c(
    "Kickoff",
    "Kickoff Return (Offense)",
    "Kickoff Return Touchdown",
    "Kickoff Touchdown"
  )
  field_goal = c(
    "Field Goal Good",
    "Blocked Field Goal",
    "Field Goal Missed",
    "Missed Field Goal Return",
    "Blocked Field Goal Touchdown",
    "Missed Field Goal Return Touchdown"
  )
  ## Kickoff plays
  ## Calculate EP before at kickoff as what happens if it was a touchback
  ## 25 yard line in 2012 and onwards
  ## Question for the class: where is the EPA on touchbacks being set to 0?
  kickoff_ind = (pred_df$play_type %in% kickoff)
  if(any(kickoff_ind)){
    new_kick = pred_df[kickoff_ind,]
    new_kick["down"] = as.factor(1)
    new_kick["distance"] = 10
    new_kick["yards_to_goal"] = 75
    new_kick["log_ydstogo"] = log(10)
    ep_kickoffs = as.data.frame(predict(ep_model, new_kick, type = 'prob'))
    if(table(kickoff_ind)[2] > 1){
      pred_df[kickoff_ind,"ep_before"] = apply(ep_kickoffs,1,function(row){
        sum(row*weights)
      })
    }
    else{
      pred_df[kickoff_ind,"ep_before"] = sum(ep_kickoffs * weights)
    }
  }
  
  # **Due to ESPN data quality issues, some drives end on 3rd down that are listed as turnovers
  # For turnover and punt plays make sure the ep_after is negative
  turnover_plays = which(pred_df$turnover == 1 & !kickoff_ind & (pred_df$play_type %in% turnover_play_type))
  pred_df[turnover_plays, "ep_after"] = -1*pred_df[turnover_plays, "ep_after"]
  
  # Game end EP is 0
  pred_df[pred_df$end_half_game_end == 1, "ep_after"] = 0
  
  ## Scoring plays from here on out
  pred_df[(pred_df$play_type %in% off_TD), "ep_after"] = 7
  pred_df[(pred_df$play_type %in% def_TD), "ep_after"] = -7
  pred_df[pred_df$play_type == "Defensive 2pt Conversion", "ep_before"] = 0
  pred_df[pred_df$play_type == "Defensive 2pt Conversion", "ep_after"] = -2
  
  pred_df[pred_df$play_type == "Safety", "ep_after"] = -2
  
  pred_df[pred_df$play_type == "Field Goal Good", "ep_after"] = 3
  
  pred_df = pred_df %>% 
    dplyr::group_by(.data$game_id) %>%
    dplyr::arrange(.data$new_id, .by_group = TRUE) %>%
    dplyr::mutate(
      ep_after = ifelse(.data$downs_turnover==1, -1*lead(.data$ep_before, 1), .data$ep_after),
      ep_after = ifelse(str_detect(.data$play_text,regex('safety', ignore_case = TRUE)) &
                          .data$play_type == 'Blocked Punt', -2, .data$ep_after),
      ep_after = ifelse(.data$turnover == 1 & is.na(.data$ep_after),
                        -1*lead(.data$ep_before,1),
                        .data$ep_after )) %>% 
    dplyr::ungroup()
  # Prep WPA variables, drop transformed columns-----
  pred_df = pred_df %>%
    dplyr::mutate(
      adj_TimeSecsRem = ifelse(.data$half == 1, 1800 + .data$TimeSecsRem, .data$TimeSecsRem),
      turnover_vec_lag = dplyr::lag(.data$turnover_vec, 1),
      def_td_play_lag = dplyr::lag(.data$def_td_play, 1),
      play_after_turnover = ifelse(.data$turnover_vec_lag == 1 & .data$def_td_play_lag != 1, 1, 0),
      score_diff = .data$offense_score - .data$defense_score,
      lag_score_diff = lag(.data$score_diff, 1),
      lag_score_diff = ifelse(.data$game_play_number == 1, 0, .data$lag_score_diff),
      offense_play_lag = dplyr::lag(.data$offense_play, 1),
      offense_play_lag = ifelse(.data$game_play_number == 1, .data$offense_play, .data$offense_play_lag),
      offense_play_lead = dplyr::lead(.data$offense_play, 1),
      offense_play_lead2 = dplyr::lead(.data$offense_play, 2),
      score_pts = ifelse(.data$offense_play_lag == .data$offense_play,
                         (.data$score_diff - .data$lag_score_diff),
                         (.data$score_diff + .data$lag_score_diff)),
      score_diff_start = ifelse(.data$offense_play_lag == .data$offense_play,
                                .data$lag_score_diff,
                                -1*.data$lag_score_diff),
      EPA = .data$ep_after - .data$ep_before,
      def_EPA = -1*.data$EPA,
      home_EPA = ifelse(.data$offense_play == .data$home, .data$EPA, -1*.data$EPA),
      away_EPA = -1*.data$home_EPA,
      ExpScoreDiff = .data$score_diff_start + .data$ep_before,
      half = as.factor(.data$half),
      ExpScoreDiff_Time_Ratio = .data$ExpScoreDiff/(.data$adj_TimeSecsRem + 1)) %>% 
    dplyr::rename(end_of_half = .data$end_half_game_end) %>%
    dplyr::select(-.data$log_ydstogo,
                  -.data$log_ydstogo_end,
                  -.data$Goal_To_Go_end,
                  -.data$Under_two_end) %>%
    dplyr::select(.data$game_id,
                  .data$new_id,
                  .data$id_play,
                  .data$drive_id,
                  .data$drive_number,
                  .data$drive_play_number,
                  .data$game_play_number,
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
                  .data$yards_gained,
                  .data$offense_score,
                  .data$defense_score,
                  .data$score_diff,
                  .data$score_diff_start,
                  .data$EPA,
                  .data$ep_before,
                  .data$ep_after,
                  .data$def_EPA,
                  .data$ppa,
                  .data$Goal_To_Go,
                  .data$offense_timeouts,
                  .data$defense_timeouts,
                  .data$drive_start_yards_to_goal,
                  .data$drive_end_yards_to_goal,
                  .data$drive_yards,
                  .data$drive_scoring,
                  .data$drive_result,
                  .data$drive_pts,
                  .data$home,
                  .data$away,
                  .data$Under_two,
                  .data$TimeSecsRem,
                  .data$down_end,
                  .data$distance_end,
                  .data$yards_to_goal_end,
                  .data$TimeSecsRem_end,
                  dplyr::everything()) %>%
    dplyr::rename(pass = .data$pass_vec,
                  rush = .data$rush_vec) %>%
    dplyr::mutate(
      rz_play = ifelse((.data$yards_to_goal <= 20), 1, 0),
      scoring_opp = ifelse((.data$yards_to_goal <= 40), 1, 0),
      stuffed_run = ifelse((.data$rush == 1 & .data$yards_gained <= 0), 1, 0),
      success = 
        ifelse(.data$yards_gained >= .5 * .data$distance & .data$down == 1, 1,
               ifelse(.data$yards_gained >= .7 * .data$distance & .data$down == 2, 1,
                      ifelse(.data$yards_gained >= .data$distance & .data$down == 3, 1,
                             ifelse(.data$yards_gained >= .data$distance & .data$down == 4, 1, 0)))),
      success = ifelse(.data$play_type %in% turnover_play_type, 0, .data$success),
      epa_success = ifelse(.data$EPA > 0, 1, 0)
    )
  
  return(pred_df)
}



#' Creates the post-play inputs for the Expected Points model to predict on for each game
#'
#' @param dat (\emph{Data.Frame} required) Clean Play-by-Play DataFrame pulled from `cfb_pbp_dat()`
#'
#' @keywords internal
#' @import stringr
#' @import dplyr
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
  penalty = c('Penalty')
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
    "Kickoff Touchdown"
  )
  dat = dat %>%
    dplyr::mutate_at(vars(.data$clock.minutes, .data$clock.seconds), ~ replace_na(., 0)) %>%
    dplyr::mutate(
      yards_to_goal = as.numeric(.data$yards_to_goal),
      distance = .data$distance,
      yards_gained = as.numeric(.data$yards_gained),
      clock.minutes = ifelse(.data$period %in% c(1, 3), 15 + .data$clock.minutes, .data$clock.minutes),
      raw_secs = .data$clock.minutes * 60 + .data$clock.seconds,
      half = ifelse(.data$period <= 2, 1, 2),
      new_yardline = 0,
      new_down = 0,
      new_distance = 0
      # log_ydstogo = 0
    ) %>% 
    dplyr::group_by(.data$game_id, .data$half) %>%
    dplyr::arrange(.data$id_play, .by_group = TRUE)
  
  turnover_ind = dat$play_type %in% turnover_play_type
  dat$turnover = 0
  #define turnover on downs
  downs_turnover = (dat$play_type %in% normalplay & dat$yards_gained < dat$distance & dat$down == 4)
  # data is ordered
  new_offense = !(dat$offense_play == lead(dat$offense_play, order_by = dat$id_play))
  scoring_plays = dat$play_type %in% score
  # end of half check as well
  end_of_half_plays = !(dat$half == lead(dat$half, order_by = dat$id_play))
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
    dplyr::group_by(.data$game_id) %>% 
    dplyr::arrange(.data$id_play, .by_group=TRUE) %>% 
    dplyr::mutate(
      play = 1,
      game_play_number = cumsum(.data$play)) %>%
    dplyr::group_by(.data$game_id, .data$half) %>%
    dplyr::arrange(.data$id_play, .by_group = TRUE) %>%
    dplyr::mutate(
      turnover_indicator = 
        ifelse(
          (.data$play_type %in% defense_score_vec) | (.data$play_type %in% turnover_vec )|
            (.data$play_type %in% normalplay & .data$yards_gained < .data$distance & 
               .data$down == 4), 1, 0),
      downs_turnover = ifelse((.data$play_type %in% normalplay) & (.data$yards_gained < .data$distance) & (.data$down == 4),1,0),
      down = as.numeric(.data$down),
      #--New Down-----
      new_down = as.numeric(case_when(
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
          !.data$penalty_offset & !.data$penalty_1st_conv ~ lead(.data$down, order_by=id_play),
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
      yards_gained = as.numeric(.data$yards_gained),
      drive_start_yards_to_goal = as.numeric(.data$drive_start_yards_to_goal),
      #--New Distance-----
      new_distance = as.numeric(case_when(
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
      new_yardline = as.numeric(case_when(
        .data$downs_turnover == 1 ~ 100 - .data$yards_to_goal + .data$yards_gained,
        .data$play_type %in% penalty & .data$penalty_offset ~ .data$yards_to_goal,
        .data$play_type %in% penalty & !.data$penalty_offset ~ .data$yards_to_goal - .data$yards_gained,
        .data$play_type %in% normalplay ~ .data$yards_to_goal - .data$yards_gained,
        .data$play_type %in% score ~ 0,
        .data$play_type %in% defense_score_vec ~ 0,
        .data$play_type %in% kickoff ~ .data$drive_start_yards_to_goal,
        .data$play_type %in% turnover_vec ~ 100 - .data$yards_to_goal + .data$yards_gained
      )),
      new_TimeSecsRem = ifelse(!is.na(lead(.data$TimeSecsRem, order_by=.data$id_play)),
                               lead(.data$TimeSecsRem,order_by=.data$id_play),0),
      new_log_ydstogo = if_else(.data$new_distance == 0, log(1), log(.data$new_distance)),
      new_Goal_To_Go = ifelse(.data$new_yardline <= .data$new_distance, TRUE, FALSE),
      # new under two minute warnings
      new_Under_two = .data$new_TimeSecsRem <= 120,
      end_half_game = 0,
      half_play = 1,
      half_play_number = cumsum(.data$half_play),
      off_timeouts_rem_before = ifelse(.data$half_play_number == 1, 3,lag(.data$offense_timeouts,1)),
      def_timeouts_rem_before = ifelse(.data$half_play_number == 1, 3,lag(.data$defense_timeouts,1))
    ) %>%
    dplyr::mutate_at(vars(.data$new_TimeSecsRem), ~ replace_na(., 0)) %>% 
    dplyr::group_by(.data$game_id,.data$half,.data$drive_id) %>% 
    dplyr::arrange(.data$id_play, .by_group = TRUE) %>%
    dplyr::mutate(
      drive_play = 1,
      drive_play_number = cumsum(.data$drive_play),
      first_by_penalty = ifelse(.data$play_type %in% penalty & .data$penalty_1st_conv, 1,
                                ifelse(.data$play_type %in% penalty & .data$penalty_declined &
                                         .data$penalty_offset & !.data$penalty_1st_conv &
                                         .data$yards_gained > .data$distance, 1,
                                       ifelse(.data$play_type %in% penalty & .data$penalty_declined &
                                                !.data$penalty_offset & !.data$penalty_1st_conv &
                                                .data$yards_gained >= .data$distance, 1, 0))),
      first_by_yards = ifelse(.data$play_type %in% normalplay & 
                                .data$yards_gained >= .data$distance, 1, 0)) %>% 
    dplyr::ungroup() %>% 
    dplyr::arrange(.data$id_play, .by_group = TRUE) %>%
    dplyr::mutate(
      firstD_by_poss = ifelse((.data$drive_play_number == 1 & .data$kickoff_play != 1)|
                                (.data$drive_play_number == 2 & lag(.data$kickoff_play, 1) == 1), 1, 0),
      firstD_by_penalty = ifelse(lag(.data$first_by_penalty, 1)==1 & .data$kickoff_play != 1, 1, 0),
      firstD_by_yards = ifelse(lag(.data$first_by_yards, 1)==1 & .data$kickoff_play != 1, 1, 0),
      new_yardline = ifelse((.data$play_type == 'Blocked Punt'), 
                            lead(.data$yards_to_goal, 1), .data$new_yardline)
    ) %>% 
    dplyr::select(-.data$play, -.data$half_play, -.data$drive_play)
  
  #--Punt Plays--------------------------
  punt_plays = dat$play_type == "Punt"
  touchback_punt = ifelse(!is.na(str_detect(dat$play_text,"touchback") & (punt_plays)),
                          str_detect(dat$play_text,"touchback") & (punt_plays),FALSE)
  
  dat[punt_plays,"new_down"] = 1
  dat[punt_plays,"new_distance"] = 10
  dat[punt_plays,"new_log_ydstogo"] = log(10)
  dat[punt_plays,"new_Goal_To_Go"] = FALSE
  dat[touchback_punt,"new_yardline"] = 80
  punt_play = dat[punt_plays,] %>% pull(.data$play_text)
  yds_punted = as.numeric(stringr::str_extract(
    stringi::stri_extract_first_regex(punt_play, '(?<= punt for)[^,]+'),
    "\\d+"
  ))
  # ball always changes hands
  punt_yd_line = dat[punt_plays,] %>% pull(.data$yards_to_goal)
  punt_yds_gained = dat[punt_plays,] %>% pull(.data$yards_gained)
  dat[punt_plays,"new_yardline"] = (100 - punt_yd_line) + yds_punted - punt_yds_gained
  
  #--End of Half Plays--------------------------
  end_of_half_plays = (dat$new_TimeSecsRem == 0)
  if (any(end_of_half_plays)) {
    dat$new_yardline[end_of_half_plays] <- 99
    dat$new_down[end_of_half_plays] <- 4
    dat$new_distance[end_of_half_plays] <- 99
    dat$end_half_game[end_of_half_plays] <- 1
    dat$new_log_ydstogo[end_of_half_plays] <- log(99)
    dat$new_Under_two[end_of_half_plays] <- dat$new_TimeSecsRem[end_of_half_plays] <= 120
  }
  
  # missed field goal needs to be here
  # needs to go before the na check to set to 99
  dat = dat %>% 
    dplyr::mutate(new_yardline = if_else(
      is.na(.data$new_yardline) &
        .data$play_type %in% c("Field Goal Missed", "Blocked Field Goal"),
      100 - (.data$yards_to_goal - 9),
      .data$new_yardline
    ))
  
  #--General weird plays that don't have an easy fix----
  na_yd_line = which(is.na(dat$new_yardline) | dat$new_yardline >= 100)
  dat$new_yardline[na_yd_line] = dat$yard_line[na_yd_line+1]
  neg_distance = which(dat$new_distance < 0)
  dat$new_distance[neg_distance] = dat$distance[neg_distance+1]
  dat$new_log_ydstogo[neg_distance] = log(dat$new_distance[neg_distance])
  
  #--Missing yd_line Plays--------------------------
  missing_yd_line = dat$new_yardline == 0
  dat$new_yardline[missing_yd_line] = 99
  dat$new_log_ydstogo[missing_yd_line] = log(99)
  dat$missing_yard_flag <- FALSE
  dat$missing_yard_flag[missing_yd_line] <- TRUE
  
  dat = dat %>%
    dplyr::mutate(new_down = as.factor(.data$new_down)) %>%
    dplyr::select(
      .data$game_id,
      .data$drive_id,
      .data$id_play,
      .data$new_TimeSecsRem,
      .data$new_down,
      .data$new_distance,
      .data$new_yardline,
      .data$new_log_ydstogo,
      .data$new_Goal_To_Go,
      .data$new_Under_two,
      .data$end_half_game,
      .data$turnover,
      .data$downs_turnover,
      .data$game_play_number,
      .data$half_play_number,
      .data$drive_play_number,
      .data$off_timeouts_rem_before,
      .data$def_timeouts_rem_before,
      .data$missing_yard_flag,
      .data$first_by_penalty,
      .data$first_by_yards,
      .data$firstD_by_penalty,
      .data$firstD_by_yards,
      .data$firstD_by_poss) %>% 
    dplyr::arrange(.data$id_play) %>%
    dplyr::mutate(id_play = gsub(pattern = unique(.data$game_id), "", x = .data$id_play),
                  id_play = as.numeric(.data$id_play))
  
  colnames(dat) = gsub("new_", "", colnames(dat))
  colnames(dat)[3] <- "new_id"
  
  dat = dat %>% 
    dplyr::rename("yards_to_goal"="yardline")
  
  return(dat)
}

#' Prep for EPA calculations at the start of the play
#' This is only for D1 football
#'
#'
#' Extracts raw game by game data.
#' @param df (\emph{data.frame} required) Clean Play-by-Play DataFrame pulled from `cfb_pbp_dat()`
#' @keywords internal
#' @import stringr
#' @import dplyr
#' @import tidyr
#' @export
#'
#'

prep_epa_df_before <- function(df) {
  ## define minutes, raw_secs, Under_two, half, new_id, log_ydstogo, redefine down
  ## filter not NA down and raw_secs
  df = df %>%
    dplyr::mutate(
      clock.minutes = ifelse(.data$period %in% c(1, 3), 15 + 
                               .data$clock.minutes, .data$clock.minutes),
      raw_secs = .data$clock.minutes * 60 + .data$clock.seconds,
      Under_two = .data$raw_secs <= 120,
      half = ifelse(.data$period <= 2, 1, 2),
      new_id = gsub(pattern = unique(.data$game_id), "", x = .data$id_play),
      new_id = as.numeric(.data$new_id),
      log_ydstogo = ifelse(.data$distance == 0|is.nan(log(.data$distance)),log(1),log(.data$distance)),
      down = ifelse(.data$down == 5 &
                      str_detect(.data$play_type, "Kickoff"), 1, .data$down)) %>% 
    dplyr::filter(.data$period <= 4, .data$down > 0) %>%
    dplyr::filter(!is.na(.data$down),!is.na(.data$raw_secs)) %>% 
    dplyr::rename(TimeSecsRem = .data$raw_secs)
  
  fg_inds = str_detect(df$play_type, "Field Goal")
  df[fg_inds, "yards_to_goal"] = df[fg_inds, "yards_to_goal"] + 17
  df[fg_inds, "log_ydstogo"] = log(df[fg_inds, "distance"])
  
  df = df %>% 
    dplyr::mutate(
      Goal_To_Go = ifelse(str_detect(.data$play_type, "Field Goal"),
                          .data$distance >= (.data$yards_to_goal - 17),
                          .data$distance >= .data$yards_to_goal )) %>%
    dplyr::filter(.data$log_ydstogo != -Inf) %>%
    dplyr::group_by(.data$drive_id) %>%
    dplyr::arrange(.data$new_id, .by_group = TRUE) %>%
    dplyr::ungroup()
  return(df)
}


#' Performs Field Goal adjustments for Expected Points model calculations
#'
#' Extracts raw game by game data.
#'
#' @param df (\emph{data.frame} required): Clean Play-By-Play data.frame as can be pulled from `cfb_pbp_dat()`
#' @param current_probs (\emph{data.frame} required): Expected Points (EP) model raw probability outputs from initial prediction
#' @param fg_mod (\emph{model}, default `cfbscrapR:::fg_model`): FG Model to be used for prediction on field goal (FG) attempts in Play-by-Play data.frame
#'
#' @keywords internal
#' @importFrom mgcv "bam"
#' @import stringr
#' @import dplyr
#' @import tidyr
#' @export
#'
#'

epa_fg_probs <- function(dat, current_probs, fg_mod) {
  fg_ind = str_detect((dat$play_type), "Field Goal")
  ep_ind = str_detect((dat$play_type), "Extra Point")
  inds = fg_ind | ep_ind
  fg_dat = dat[inds, ]
  
  # we are setting everythign after 0 seconds to have 0 probs.
  end_game_ind = which(dat$TimeSecsRem <= 0)
  current_probs[end_game_ind, ] <- 0
  
  make_fg_prob <- mgcv::predict.bam(fg_mod, newdata = fg_dat,
                                    type = "response")
  
  fg_dat<- fg_dat %>%
    # Subtract 5.065401 from TimeSecs since average time for FG att:
    dplyr::mutate(
      TimeSecsRem = .data$TimeSecsRem - 5.065401,
      # Correct the yrdline100:
      yards_to_goal = 100 - (.data$yards_to_goal - 9),
      # Not GoalToGo:
      Goal_To_Go = rep(FALSE, n()),
      # Now first down:
      down = rep("1", n()),
      # 10 yards to go
      log_ydstogo = rep(log(10), n()),
      # Create Under_TwoMinute_Warning indicator
      Under_two = ifelse(.data$TimeSecsRem < 120,
                         TRUE, FALSE)
    )
  # add in the fg make prob into this
  current_probs2 <- current_probs
  #current_probs2[fg_ind,] <- current_probs[fg_ind,] * (1 - make_fg_prob)
  val = (1 - make_fg_prob)
  ind <- dim(current_probs2[inds, ])[1]
  for (i in seq(1, ind)) {
    temp = current_probs2[inds, ]
    temp[i, ] = temp[i, ] * val[i]
  }
  current_probs2[inds, ] =  temp
  
  # now to flip all the probs,
  current_probs2[inds, "FG"] <- make_fg_prob + current_probs[inds, "Opp_FG"]
  current_probs2[inds, "Opp_FG"] <- current_probs[inds, "FG"]
  current_probs2[inds, "TD"] <- current_probs[inds, "Opp_TD"]
  current_probs2[inds, "Opp_TD"] <- current_probs[inds, "TD"]
  current_probs2[inds, "Safety"] <- current_probs[inds, "Opp_Safety"]
  current_probs2[inds, "Opp_Safety"] <- current_probs[inds, "Safety"]
  
  return(current_probs2)
}