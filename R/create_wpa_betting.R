#' Add Win Probability Added (WPA) calculations to Play-by-Play DataFrame (Betting)
#' This is only for D1 football
#'
#'
#' Extracts raw game by game data.
#' @param df (\emph{data.frame} required): Clean Play-by-Play data.frame with Expected Points Added (EPA) calculations
#' @param wp_model (\emph{model} default cfbscrapR:wp_model): Win Probability (WP) Model
#'
#' @keywords internal
#' @import dplyr
#' @import tidyr
#' @importFrom mgcv "bam"
#' @export
#'

create_wpa_betting <- function(df, wp_model = cfbscrapR:::wp_model) {
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
      home_EPA = ifelse(.data$offense_play == .data$home, .data$EPA, -.data$EPA),
      away_EPA = -.data$home_EPA,
      ExpScoreDiff = ifelse(.data$offense_play == .data$home,
                            .data$score_diff + .data$ep_before - .data$spread,
                            .data$score_diff + .data$ep_before + .data$spread),
      # ExpScoreDiff = .data$score_diff + .data$ep_before,
      ExpScoreDiff = as.numeric(.data$ExpScoreDiff),
      half = as.factor(.data$half),
      ExpScoreDiff_Time_Ratio = .data$ExpScoreDiff / (.data$adj_TimeSecsRem + 1)
    )
  }
  
  df = df %>% 
    arrange(.data$game_id, .data$new_id) 
    
  
  Off_Win_Prob = as.vector(predict(wp_model, newdata = df, type = "response"))
  df$wp_bet = Off_Win_Prob
  df <- df %>% 
    mutate(
      spread = ifelse(.data$offense_play ==.data$home, .data$spread,-.data$spread),
      # values taken at face value from Boyds Bets (https://www.boydsbets.com/college-football-spread-to-moneyline-conversion/)
      wp_bet = case_when(
        .data$game_play_number == 1 & .data$spread ==  0 ~ 0.50,
        .data$game_play_number == 1 & .data$spread == -0.5 ~ 0.50,
        .data$game_play_number == 1 & .data$spread ==  0.5 ~ 0.50,
        .data$game_play_number == 1 & .data$spread == -1 ~ 0.512,
        .data$game_play_number == 1 & .data$spread ==  1 ~ 0.4880,
        .data$game_play_number == 1 & .data$spread == -1.5 ~ 0.5250,
        .data$game_play_number == 1 & .data$spread ==  1.5 ~ 0.4660,
        .data$game_play_number == 1 & .data$spread == -2 ~ 0.5340,
        .data$game_play_number == 1 & .data$spread ==  2 ~ 0.4660,
        .data$game_play_number == 1 & .data$spread == -2.5 ~ 0.5430,
        .data$game_play_number == 1 & .data$spread ==  2.5 ~ 0.4570,
        .data$game_play_number == 1 & .data$spread == -3 ~ 0.5740,
        .data$game_play_number == 1 & .data$spread ==  3 ~ 0.4260,
        .data$game_play_number == 1 & .data$spread == -3.5 ~ 0.6060,
        .data$game_play_number == 1 & .data$spread ==  3.5 ~ 0.3940,
        .data$game_play_number == 1 & .data$spread == -4 ~ 0.6190,
        .data$game_play_number == 1 & .data$spread ==  4 ~ 0.3810,
        .data$game_play_number == 1 & .data$spread == -4.5 ~ 0.6310,
        .data$game_play_number == 1 & .data$spread ==  4.5 ~ 0.3690,
        .data$game_play_number == 1 & .data$spread == -5 ~ 0.6410,
        .data$game_play_number == 1 & .data$spread ==  5 ~ 0.3590,
        .data$game_play_number == 1 & .data$spread == -5.5 ~ 0.6510,
        .data$game_play_number == 1 & .data$spread ==  5.5 ~ 0.3490,
        .data$game_play_number == 1 & .data$spread == -6 ~ 0.6640,
        .data$game_play_number == 1 & .data$spread ==  6 ~ 0.3360,
        .data$game_play_number == 1 & .data$spread == -6.5 ~ 0.6770,
        .data$game_play_number == 1 & .data$spread ==  6.5 ~ 0.3230,   
        .data$game_play_number == 1 & .data$spread == -7 ~ 0.7030,
        .data$game_play_number == 1 & .data$spread ==  7 ~ 0.2970,  
        .data$game_play_number == 1 & .data$spread == -7.5 ~ 0.7300,
        .data$game_play_number == 1 & .data$spread ==  7.5 ~ 0.2700,  
        .data$game_play_number == 1 & .data$spread == -8 ~ 0.7380,
        .data$game_play_number == 1 & .data$spread ==  8 ~ 0.2620, 
        .data$game_play_number == 1 & .data$spread == -8.5 ~ 0.7460,
        .data$game_play_number == 1 & .data$spread ==  8.5 ~ 0.2540,  
        .data$game_play_number == 1 & .data$spread == -9 ~ 0.7510,
        .data$game_play_number == 1 & .data$spread ==  9 ~ 0.2490,  
        .data$game_play_number == 1 & .data$spread == -9.5 ~ 0.7550,
        .data$game_play_number == 1 & .data$spread ==  9.5 ~ 0.2450,  
        .data$game_play_number == 1 & .data$spread == -10 ~ 0.7740,
        .data$game_play_number == 1 & .data$spread ==  10 ~ 0.2260,
        .data$game_play_number == 1 & .data$spread == -10.5 ~ 0.7930,
        .data$game_play_number == 1 & .data$spread ==  10.5 ~ 0.2080,   
        .data$game_play_number == 1 & .data$spread == -11 ~ 0.7990,
        .data$game_play_number == 1 & .data$spread ==  11 ~ 0.2010,   
        .data$game_play_number == 1 & .data$spread == -11.5 ~ 0.8060,
        .data$game_play_number == 1 & .data$spread ==  11.5 ~ 0.1940,    
        .data$game_play_number == 1 & .data$spread == -12 ~ 0.8160,
        .data$game_play_number == 1 & .data$spread ==  12 ~ 0.1840,  
        .data$game_play_number == 1 & .data$spread == -12.5 ~ 0.8260,
        .data$game_play_number == 1 & .data$spread ==  12.5 ~ 0.1740, 
        .data$game_play_number == 1 & .data$spread == -13 ~ 0.8300,
        .data$game_play_number == 1 & .data$spread ==  13 ~ 0.1700, 
        .data$game_play_number == 1 & .data$spread == -13.5 ~ 0.8350,
        .data$game_play_number == 1 & .data$spread ==  13.5 ~ 0.1650,
        .data$game_play_number == 1 & .data$spread == -14 ~ 0.8510,
        .data$game_play_number == 1 & .data$spread ==  14 ~ 0.1490, 
        .data$game_play_number == 1 & .data$spread == -14.5 ~ 0.8680,
        .data$game_play_number == 1 & .data$spread ==  14.5 ~ 0.1320, 
        .data$game_play_number == 1 & .data$spread == -15 ~ 0.8740,
        .data$game_play_number == 1 & .data$spread ==  15 ~ 0.1260, 
        .data$game_play_number == 1 & .data$spread == -15.5 ~ 0.8810,
        .data$game_play_number == 1 & .data$spread ==  15.5 ~ 0.1190, 
        .data$game_play_number == 1 & .data$spread == -16 ~ 0.8860,
        .data$game_play_number == 1 & .data$spread ==  16 ~ 0.1140,
        .data$game_play_number == 1 & .data$spread == -16.5 ~ 0.8910,
        .data$game_play_number == 1 & .data$spread ==  16.5 ~ 0.1090,    
        .data$game_play_number == 1 & .data$spread == -17 ~ 0.9140,
        .data$game_play_number == 1 & .data$spread ==  17 ~ 0.0860,
        .data$game_play_number == 1 & .data$spread == -17.5 ~ 0.9370,
        .data$game_play_number == 1 & .data$spread ==  17.5 ~ 0.0630,
        .data$game_play_number == 1 & .data$spread == -18 ~ 0.9500,
        .data$game_play_number == 1 & .data$spread ==  18 ~ 0.0500,
        .data$game_play_number == 1 & .data$spread == -18.5 ~ 0.9620,
        .data$game_play_number == 1 & .data$spread ==  18.5 ~ 0.0380,
        .data$game_play_number == 1 & .data$spread == -19 ~ 0.9730,
        .data$game_play_number == 1 & .data$spread ==  19 ~ 0.0270,
        .data$game_play_number == 1 & .data$spread == -19.5 ~ 0.9840,
        .data$game_play_number == 1 & .data$spread ==  19.5 ~ 0.0160,
        .data$game_play_number == 1 & .data$spread <= -20 ~ 0.9999,
        .data$game_play_number == 1 & .data$spread >=  20 ~ 0.0001,        
        TRUE ~ .data$wp_bet)
      )
  
  g_ids <- sort(unique(df$game_id))

  
  
  
  g_ids = sort(unique(df$game_id))
  df2 = purrr::map_dfr(g_ids,
                       function(x) {
                         df %>%
                           filter(.data$game_id == x) %>%
                           wpa_calcs_betting()
                       })
  df2 <- df2 %>% 
    rename(
      ExpScoreDiff_bet = .data$ExpScoreDiff,
      ExpScoreDiff_Time_Ratio_bet = .data$ExpScoreDiff_Time_Ratio) %>% 
    mutate(
      ExpScoreDiff_bet = as.numeric(.data$ExpScoreDiff_bet),
      ExpScoreDiff_Time_Ratio_bet = as.numeric(.data$ExpScoreDiff_Time_Ratio_bet)
    )
  
  return(df2)
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
  df2 <- df %>%
    group_by(.data$half) %>%
    mutate(
      #-- ball changes hand----
      change_of_poss = ifelse(.data$offense_play == lead(.data$offense_play, order_by = .data$id_play), 0, 1),
      change_of_poss = ifelse(is.na(.data$change_of_poss), 0, .data$change_of_poss)
    ) %>% ungroup() %>% arrange(.data$id_play)
  
  df3 = df2 %>% 
    mutate(
      def_wp_bet = 1 - .data$wp_bet,
      home_wp_bet = if_else(.data$offense_play == .data$home,
                        .data$wp_bet, .data$def_wp_bet),
      away_wp_bet = if_else(.data$offense_play != .data$home,
                        .data$wp_bet, .data$def_wp_bet),
      # base wpa
      end_of_half = ifelse(.data$half == lead(.data$half), 0, 1),
      lead_wp_bet = dplyr::lead(.data$wp_bet),
      # account for turnover
      wpa_base_bet = .data$lead_wp_bet - .data$wp_bet,
      wpa_change_bet = ifelse(.data$change_of_poss == 1, 
                              (1 - .data$lead_wp_bet) - .data$wp_bet, 
                              .data$wpa_base_bet),
      wpa_bet = ifelse(.data$end_of_half == 1, 0, .data$wpa_change_bet),
      home_wp_post_bet = ifelse(.data$offense_play == .data$home,
                            .data$home_wp_bet + .data$wpa_bet,
                            .data$home_wp_bet - .data$wpa_bet),
      away_wp_post_bet = ifelse(.data$offense_play != .data$home,
                            .data$away_wp_bet + .data$wpa_bet,
                            .data$away_wp_bet - .data$wpa_bet)
    )
  return(df3)
}
