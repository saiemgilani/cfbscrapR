#' Create EPA
#' Adds Expected Points calculations to Play-by-Play data.frame
#'
#' @param clean_pbp_dat (\emph{data.frame} required): Clean PBP as input from `cfb_pbp_dat()`)
#' @param ep_model (\emph{model} default \code{\link[cfbscrapR]{ep_model}}): Expected Points (EP) Model
#' @param fg_model (\emph{model} default \code{\link[cfbscrapR]{fg_model}}): Field Goal (FG) Model
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
  # constant vectors to be used again
  turnover_play_type = c(
    "Blocked Field Goal",
    "Blocked Field Goal Touchdown",
    "Blocked Punt",
    "Blocked Punt Touchdown",
    "Field Goal Missed",
    "Fumble Recovery (Opponent)",
    "Fumble Recovery (Opponent) Touchdown",
    "Missed Field Goal Return",
    "Missed Field Goal Return Touchdown",
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

  off_TD = c(
    "Passing Touchdown",
    "Rushing Touchdown",
    "Field Goal Good",
    "Pass Reception Touchdown",
    "Fumble Recovery (Own) Touchdown",
    "Punt Touchdown"
  )
  def_TD = c(
    "Blocked Punt Touchdown",
    "Fumble Return Touchdown",
    "Defensive 2pt Conversion",
    "Interception Return Touchdown",
    "Safety",
    "Missed Field Goal Return Touchdown",
    "Punt Touchdown",
    "Punt Return Touchdown",
    "Blocked Field Goal Touchdown",
    "Fumble Recovery (Opponent) Touchdown",
    "Pass Interception Return Touchdown",
    "Sack Touchdown",
    "Uncategorized Touchdown"
  )

  # if you are trying to deal with multiple games at once
  # then you have to get the after individually.
  g_ids = sort(unique(clean_pbp_dat$game_id))
  clean_pbp_dat = purrr::map_dfr(g_ids,
                                 function(x) {
                                   clean_pbp_dat %>%
                                     filter(.data$game_id == x) %>%
                                     prep_epa_df_before()
                                 })



  pred_df = clean_pbp_dat %>% 
    group_by(.data$drive_id) %>%
    arrange(.data$new_id, .by_group = TRUE) %>%
    select(.data$new_id,
           .data$drive_id,
           .data$game_id,
           .data$TimeSecsRem,
           .data$down,
           .data$distance,
           .data$yards_to_goal,
           .data$log_ydstogo,
           .data$Under_two,
           .data$Goal_To_Go) %>%
    mutate(down = as.factor(.data$down)) %>%
    ungroup()

  # ep_start

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
                                     filter(.data$game_id == x) %>%
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
    left_join(prep_df_after,
              by = c("game_id", "drive_id", "new_id")) %>%
    left_join(
      pred_df %>% select(.data$new_id, .data$drive_id, .data$game_id, .data$ep_before),
      by = c("game_id", "drive_id", "new_id")
    )

  ## kickoff plays
  ## calculate EP before at kickoff as what happens if it was a touchback
  ## 25 yard line in 2012 and onwards
  ## question for the class: where is the EPA on touchbacks being set to 0?
  kickoff_ind = (pred_df$play_type =='Kickoff')
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

  # For turnover and punt plays make sure the ep_after is negative
  # because of poor ESPN data quality,
  # some drives end on 3rd down and we have those listed as turnovers
  turnover_plays = which(pred_df$turnover == 1 & !kickoff_ind & pred_df$play_type %in% turnover_play_type)
  pred_df[turnover_plays, "ep_after"] = -1 * pred_df[turnover_plays, "ep_after"]

  # game end EP is 0
  pred_df[pred_df$end_half_game_end == 1, "ep_after"] = 0

  ## scoring plays from here on out
  pred_df[(pred_df$play_type %in% off_TD), "ep_after"] = 7
  pred_df[(pred_df$play_type %in% def_TD), "ep_after"] = -7
  pred_df[pred_df$play_type == "Safety", "ep_after"] = -2
  pred_df[pred_df$play_type == "Field Goal Good", "ep_after"] = 3

  # prep some variables for WPA, drop transformed columns
  pred_df = pred_df %>%
    mutate(adj_TimeSecsRem = ifelse(.data$half == 1, 1800 + .data$TimeSecsRem, .data$TimeSecsRem),
           EPA = .data$ep_after - .data$ep_before,
           score_diff = .data$offense_score - .data$defense_score,
           home_EPA = ifelse(.data$offense_play==.data$home,.data$EPA,-.data$EPA),
           away_EPA = -.data$home_EPA,
           ExpScoreDiff = .data$score_diff + .data$ep_before,
           half = as.factor(.data$half),
           ExpScoreDiff_Time_Ratio = .data$ExpScoreDiff/(.data$adj_TimeSecsRem + 1)) %>%
    select(-.data$yard_line,
           -.data$log_ydstogo,
           -.data$log_ydstogo_end,
           -.data$Goal_To_Go_end,
           -.data$end_half_game_end,
           -.data$Under_two_end) %>%
    select(.data$game_id,
           .data$drive_id,
           .data$new_id,
           .data$id_play,
           .data$offense_play,
           .data$defense_play,
           .data$home,
           .data$away,
           .data$period,
           .data$half,
           .data$clock.minutes,
           .data$clock.seconds,
           .data$offense_score,
           .data$defense_score,
           .data$play_type,
           .data$play_text,
           .data$drive_scoring,
           .data$TimeSecsRem,
           .data$Under_two,
           .data$down,
           .data$distance,
           .data$Goal_To_Go,
           .data$yards_to_goal,
           .data$yards_gained,
           .data$TimeSecsRem_end,
           .data$down_end,
           .data$distance_end,
           .data$yards_to_goal_end,
           everything()) %>%
    mutate(
      rz_play = ifelse((.data$yards_to_goal <= 20), 1, 0),
      scoring_opp = ifelse((.data$yards_to_goal <= 40), 1, 0),
      pass = if_else(
        .data$play_type == "Pass Reception" |
          .data$play_type == "Pass Completion" |
          .data$play_type == "Passing Touchdown" |
          .data$play_type == "Sack" |
          .data$play_type == "Pass Interception Return" |
          .data$play_type == "Pass Incompletion" |
          .data$play_type == "Sack Touchdown" |
          (.data$play_type == "Safety" &
             str_detect(.data$play_text, "sacked")) |
          (
            .data$play_type == "Fumble Recovery (Own)" &
              str_detect(.data$play_text, "pass")
          ) |
          (
            .data$play_type == "Fumble Recovery (Opponent)" &
              str_detect(.data$play_text, "pass")
          ),
        1,
        0
      ),
      rush = ifelse(
        .data$play_type == "Rush" | .data$play_type == "Rushing Touchdown" |
          (.data$play_type == "Safety" &
             str_detect(.data$play_text, "run")) |
          (
            .data$play_type == "Fumble Recovery (Opponent)" &
              str_detect(.data$play_text, "run")
          ) |
          (
            .data$play_type == "Fumble Recovery (Own)" &
              str_detect(.data$play_text, "run")
          ),
        1,
        0
      ),
      stuffed_run = ifelse((.data$rush == 1 &
                              .data$yards_gained <= 0), 1, 0),
      success = ifelse(
        .data$yards_gained >= .5 * .data$distance & .data$down == 1,
        1,
        ifelse(
          .data$yards_gained >= .7 * .data$distance & .data$down == 2,
          1,
          ifelse(
            .data$yards_gained >= .data$distance & .data$down == 3,
            1,
            ifelse(.data$yards_gained >= .data$distance &
                     .data$down == 4, 1, 0)
          )
        )
      ),
      success = ifelse(.data$play_type %in% turnover_play_type, 0, .data$success),
      epa_success = ifelse(.data$EPA > 0, 1, 0)
    )

  return(pred_df)
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
#'
#'

epa_fg_probs <- function(dat, current_probs, fg_mod) {
  fg_ind = str_detect((dat$play_type), "Field Goal")
  ep_ind = str_detect((dat$play_type), "Extra Point")
  inds = fg_ind | ep_ind
  fg_dat = dat[inds, ]

  # we are setting everythign after 0 seconds to have
  # 0 probs.
  end_game_ind = which(dat$TimeSecsRem <= 0)
  current_probs[end_game_ind, ] <- 0

  make_fg_prob <- mgcv::predict.bam(fg_mod, newdata = fg_dat,
                                    type = "response")

  fg_dat<- fg_dat %>%
    # Subtract 5.065401 from TimeSecs since average time for FG att:
    mutate(
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
  #current_probs2[fg_ind,] <- current_probs[fg_ind,] * (1-make_fg_prob)
  val = (1 - make_fg_prob)
  ind <- dim(current_probs2[inds, ])[1]
  for (i in seq(1, ind)) {
    temp = current_probs2[inds, ]
    temp[i, ] = temp[i, ] * val[i]
  }
  current_probs2[inds, ] =  temp


  # now to flip all the probs,
  current_probs2[inds, "FG"] <-
    make_fg_prob + current_probs[inds, "Opp_FG"]
  current_probs2[inds, "Opp_FG"] <- current_probs[inds, "FG"]
  current_probs2[inds, "TD"] <- current_probs[inds, "Opp_TD"]
  current_probs2[inds, "Opp_TD"] <- current_probs[inds, "TD"]
  current_probs2[inds, "Safety"] <-
    current_probs[inds, "Opp_Safety"]
  current_probs2[inds, "Opp_Safety"] <-
    current_probs[inds, "Safety"]
  return(current_probs2)
}

#' Creates the post-play inputs for the Expected Points model to predict on for each game
#'
#' @param dat (\emph{Data.Frame} required) Clean Play-by-Play DataFrame pulled from `cfb_pbp_dat()`
#'
#' @keywords internal
#' @import stringr
#' @import dplyr
#' @import tidyr
#'

prep_epa_df_after <- function(dat) {
  ##--Play type vectors------
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
    "Punt Touchdown",
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
    "Fumble Recovery (Own) Touchdown"
  )
  kickoff = c(
    "Kickoff",
    "Kickoff Return (Offense)",
    "Kickoff Return Touchdown",
    "Kickoff Touchdown"
  )
  dat = dat %>%
    mutate_at(vars(.data$clock.minutes, .data$clock.seconds), ~ replace_na(., 0)) %>%
    mutate(
      yards_to_goal = as.numeric(.data$yards_to_goal),
      distance = .data$distance,
      yards_gained = as.numeric(.data$yards_gained),
      clock.minutes = ifelse(.data$period %in% c(1, 3), 15 + .data$clock.minutes, .data$clock.minutes),
      raw_secs = .data$clock.minutes * 60 + .data$clock.seconds,
      half = ifelse(.data$period <= 2, 1, 2),
      new_yardline = 0,
      new_down = 0,
      new_distance = 0
      #log_ydstogo = 0
    ) %>% group_by(.data$game_id, .data$half) %>%
    dplyr::arrange(.data$id_play, .by_group = TRUE)

  turnover_ind = dat$play_type %in% turnover_play_type
  dat$turnover = 0
  #define turnover on downs
  downs_turnover = (dat$play_type %in% normalplay & dat$down == 4)
  # data is ordered
  new_offense = !(dat$offense_play == lead(dat$offense_play,order_by = dat$id_play))
  scoring_plays = dat$play_type %in% score
  # end of half check as well
  end_of_half_plays = !(dat$half == lead(dat$half,order_by = dat$id_play))
  # is specifically defined as a turnover
  turnover_play_check = dat$play_type %in% turnover_vec
  # turnovers only occur on actual change of offense
  # but not scoring plays
  # and not at the end of half.
  # Turnovers now capture downs, when there is a change of offense after a fourth down normal play.
  t_ind = (turnover_ind | (new_offense)) & !scoring_plays & !end_of_half_plays & (turnover_play_check | downs_turnover)

  dat$turnover[t_ind] <- 1


  dat = dat %>% ungroup() %>% group_by(.data$game_id, .data$half) %>%
    dplyr::arrange(.data$id_play, .by_group = TRUE) %>%
    mutate(
      turnover_indicator = ifelse(
        .data$play_type %in% defense_score_vec | .data$play_type %in% turnover_vec |
          .data$play_type %in% normalplay &
          .data$yards_gained < .data$distance & .data$down == 4,
        1,
        0
      ),
      down = as.numeric(.data$down),
      #--New Down-----
      new_down = as.numeric(case_when(
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
          .data$yards_gained > .data$distance ~ 1,
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
        ##--Scores, kickoffs, turnovers, defensive scores----
        .data$play_type %in% score ~ 1,
        .data$play_type %in% kickoff ~ 1,
        .data$play_type %in% turnover_vec ~ 1,
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
        ##--turnovers, defensive scores, scores, kickoffs
        .data$play_type %in% turnover_vec ~ 10,
        # play_type %in% turnover_vec &
        #   (100 - (yards_to_goal + yards_gained) >= 10) ~ 10,
        # play_type %in% turnover_vec &
        #   (100 - (yards_to_goal + yards_gained) <= 10) ~ 100 - (yards_to_goal  + yards_gained),
        .data$play_type %in% defense_score_vec ~ 0,
        .data$play_type %in% score ~ 0,
        .data$play_type %in% kickoff ~ 10
      )),
      #--New Yardline----
      new_yardline = as.numeric(case_when(
        .data$play_type %in% penalty & .data$penalty_offset ~ .data$yards_to_goal,
        .data$play_type %in% penalty & !.data$penalty_offset ~ .data$yards_to_goal - .data$yards_gained,
        .data$play_type %in% normalplay ~ .data$yards_to_goal - .data$yards_gained,
        .data$play_type %in% score ~ 0,
        .data$play_type %in% defense_score_vec ~ 0,
        .data$play_type %in% kickoff ~ .data$drive_start_yards_to_goal,
        .data$play_type %in% turnover_vec ~ 100 - .data$yards_to_goal + .data$yards_gained
      )),

      new_TimeSecsRem = ifelse(!is.na(lead(.data$TimeSecsRem,order_by=.data$id_play)),lead(.data$TimeSecsRem,order_by=.data$id_play),0),
      new_log_ydstogo = ifelse(.data$new_distance == 0, log(1),log(.data$new_distance)),
      new_Goal_To_Go = ifelse(.data$new_yardline <= .data$new_distance, TRUE, FALSE),
      # new under two minute warnings
      new_Under_two = .data$new_TimeSecsRem <= 120,
      end_half_game = 0
    ) %>%
    mutate_at(vars(.data$new_TimeSecsRem), ~ replace_na(., 0)) %>% ungroup()

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
  # ball always chances hands
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
    dat$new_Under_two[end_of_half_plays] <-
      dat$new_TimeSecsRem[end_of_half_plays] <= 120
  }

  # missed field goal needs to be here
  # needs to go before the na check to set to 99
  dat = dat %>% mutate(new_yardline = if_else(
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



  dat = dat %>%
    mutate(new_down = as.factor(.data$new_down)) %>%
    select(
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
      .data$turnover
    ) %>% arrange(.data$id_play) %>%
    mutate(id_play = gsub(pattern = unique(.data$game_id), "", x = .data$id_play),
           id_play = as.numeric(.data$id_play))
  colnames(dat) = gsub("new_", "", colnames(dat))
  colnames(dat)[3] <- "new_id"
  dat = dat %>% rename("yards_to_goal"="yardline")


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
#'
#'

prep_epa_df_before <- function(df) {
  df = df %>%
    mutate(
      clock.minutes = ifelse(.data$period %in% c(1, 3), 15 + .data$clock.minutes, .data$clock.minutes),
      raw_secs = .data$clock.minutes * 60 + .data$clock.seconds,
      Under_two = .data$raw_secs <= 120,
      half = ifelse(.data$period <= 2, 1, 2),
      new_id = gsub(pattern = unique(.data$game_id), "", x = .data$id_play),
      new_id = as.numeric(.data$new_id),
      log_ydstogo = ifelse(.data$distance == 0,log(0.5),log(.data$distance)),
      down = ifelse(.data$down == 5 &
                      str_detect(.data$play_type, "Kickoff"),1, .data$down)
    ) %>% filter(.data$period <= 4, .data$down > 0) %>%
    filter(!is.na(.data$down),!is.na(.data$raw_secs)) %>% rename(TimeSecsRem = .data$raw_secs)


  fg_inds = str_detect(df$play_type, "Field Goal")
  df[fg_inds, "yards_to_goal"] = df[fg_inds, "yards_to_goal"] + 17
  df[fg_inds, "log_ydstogo"] = log(df[fg_inds, "distance"])

  df = df %>% 
    mutate(
      Goal_To_Go = ifelse(str_detect(.data$play_type, "Field Goal"),
                          .data$distance >= (.data$yards_to_goal - 17),
                          .data$distance >= .data$yards_to_goal )) %>%
    filter(.data$log_ydstogo != -Inf) %>%
    group_by(.data$drive_id) %>%
    arrange(.data$new_id, .by_group = TRUE) %>%
    ungroup()
  return(df)
}

