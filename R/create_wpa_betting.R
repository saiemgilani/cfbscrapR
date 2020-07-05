#' Add Win Probability Added (WPA) calculations to Play-by-Play DataFrame (Betting)
#' This is only for D1 football
#'
#'
#' Extracts raw game by game data.
#' @param df (\emph{data.frame} required): Clean Play-by-Play data.frame with Expected Points Added (EPA) calculations
#' @param wp_model (\emph{model} default `cfbpointsR::wp_model`): Win Probability (WP) Model
#'
#' @keywords internal
#' @import dplyr
#' @import tidyr
#' @importFrom mgcv "bam"
#' @export
#'

create_wpa_betting <- function(df, wp_model = cfbscrapR:::wp_model_betting) {
  col_nec = c(
    "ExpScoreDiff",
    "TimeSecsRem",
    "half",
    "Under_two",
    "off_timeouts_rem_before",
    "def_timeouts_rem_before"
  )
  if (!all(col_nec %in% colnames(df))) {
    df = df %>% mutate(
      score_diff = .data$offense_score - .data$defense_score,
      home_EPA = ifelse(.data$offense_play == .data$home, .data$EPA,-.data$EPA),
      away_EPA = -.data$home_EPA,
      ExpScoreDiff = .data$score_diff + .data$ep_before,
      half = as.factor(.data$half),
      ExpScoreDiff_Time_Ratio = .data$ExpScoreDiff / (.data$TimeSecsRem + 1)
    )
  }
  
  df = df %>% arrange(.data$game_id, .data$new_id) %>% group_by(game_id) %>% 
    mutate(play_no =1,
     game_play_number = cumsum(play_no)) %>% select(-play_no)
  
  Off_Win_Prob = as.vector(predict(wp_model, newdata = df, type = "response"))
  df$wp_betting = Off_Win_Probme
  
  # Grab season from df for betting lines function
  season <-  min(df$season)
  
  # Pull spread info and calculate home win probability based on consensus spread
  game_spread <- cfb_betting_lines(year = season, provider = "consensus")
  
  g_ids <- sort(unique(df$game_id))

  df3 <- df %>% mutate(ExpScoreDiff = ifelse(.data$offense_play == .data$home, ExpScoreDiff - spread, ExpScoreDiff + spread ))
  
  
  g_ids = sort(unique(df$game_id))
  df2 = purrr::map_dfr(g_ids,
                       function(x) {
                         df3 %>%
                           filter(.data$game_id == x) %>%
                           wpa_calcs()
                       })
  return(df3)
}

#' WPA Calcs
#'
#' Extracts raw game by game data.
#' @param df (\emph{data.frame} required): Clean Play-by-Play data.frame with Expected Points Added (EPA) calculations
#' @keywords internal
#' @import dplyr
#' @import tidyr
#'
wpa_calcs_betting <- function(df) {
  # Pull
  ## add change of possession to df----
  ## do this last because we process
  ## new TDs etc
  df <- df %>%
    group_by(.data$half) %>%
    mutate(
      #-- ball changes hand----
      change_of_poss = ifelse(.data$offense_play == lead(.data$offense_play, order_by = .data$id_play), 0, 1),
      change_of_poss = ifelse(is.na(.data$change_of_poss), 0, .data$change_of_poss)
    ) %>% ungroup() %>% arrange(.data$id_play)
  
  df2 = df %>% mutate(
    def_wp_bet = 1 - .data$wp,
    home_wp_bet = if_else(.data$offense_play == .data$home,
                      .data$wp, .data$def_wp),
    away_wp_bet = if_else(.data$offense_play != .data$home,
                      .data$wp, .data$def_wp)
  ) %>%
    mutate(
      # base wpa
      end_of_half = ifelse(.data$half == lead(.data$half), 0, 1),
      lead_wp_bet = dplyr::lead(.data$wp),
      # account for turnover
      wpa_base_bet = .data$lead_wp - .data$wp,
      wpa_change_bet = ifelse(.data$change_of_poss == 1, (1 - .data$lead_wp) - .data$wp, .data$wpa_base),
      wpa_bet = ifelse(.data$end_of_half == 1, 0, .data$wpa_change),
      home_wp_post_bet = ifelse(.data$offense_play == .data$home,
                            .data$home_wp + .data$wpa,
                            .data$home_wp - .data$wpa),
      away_wp_post_bet = ifelse(.data$offense_play != .data$home,
                            .data$away_wp + .data$wpa,
                            .data$away_wp - .data$wpa),
      adj_TimeSecsRem = ifelse(.data$half == 1, 1800 + .data$TimeSecsRem, .data$TimeSecsRem)
    )
  return(df2)
}