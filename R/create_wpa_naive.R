#' Add Win Probability Added (WPA) calculations to Play-by-Play DataFrame
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

create_wpa_naive <- function(df, wp_model = cfbscrapR:::wp_model) {
  col_nec = c(
    "ExpScoreDiff",
    "TimeSecsRem",
    "half",
    "Under_two",
    "off_timeouts_rem_before",
    "def_timeouts_rem_before"
  )
  if (!all(col_nec %in% colnames(df))) {
    df = df %>%dplyr::mutate(
      play_after_turnover = ifelse(lag(.data$turnover_vec, 1) == 1 & lag(.data$def_td_play, 1) != 1, 1, 0),
      score_diff = .data$offense_score - .data$defense_score,
      lag_score_diff = lag(.data$score_diff,1)
      score_diff_start = ifelse(.data$play_after_turnover == 1, 
                                -1*(ifelse(.data$game_play_number == 1, 0, lag_score_diff)),
                                ifelse(.data$scoring_play == 1, 
                                       ifelse(.data$game_play_number == 1, 0, lag_score_diff), 
                                       .data$score_diff)),
      home_EPA = ifelse(.data$offense_play == .data$home, .data$EPA, -.data$EPA),
      away_EPA = -.data$home_EPA,
      ExpScoreDiff = .data$score_diff_start + .data$ep_before,
      half = as.factor(.data$half),
      ExpScoreDiff_Time_Ratio = .data$ExpScoreDiff / (.data$adj_TimeSecsRem + 1)
    )
  }

  df = df %>% 
    dplyr::arrange(.data$game_id, .data$new_id)
  
  Off_Win_Prob = as.vector(predict(wp_model, newdata = df, type = "response"))
  df$wp_before = Off_Win_Prob

  g_ids = sort(unique(df$game_id))
  df2 = purrr::map_dfr(g_ids,
                       function(x) {
                         df %>%
                           dplyr::filter(.data$game_id == x) %>%
                           wpa_calcs_naive()
                       })
  return(df2)
}

#' WPA Calcs
#'
#' Extracts raw game by game data.
#' @param df (\emph{data.frame} required): Clean Play-by-Play data.frame with Expected Points Added (EPA) calculations
#' @keywords internal
#' @import dplyr
#' @import tidyr
#' @export
#' 
wpa_calcs_naive <- function(df) {

  df2 = df %>% 
   dplyr::mutate(
      def_wp_before = 1 - .data$wp_before,
      home_wp_before = if_else(.data$offense_play == .data$home,
                        .data$wp_before, 
                        .data$def_wp_before),
      away_wp_before = if_else(.data$offense_play != .data$home,
                        .data$wp_before, 
                        .data$def_wp_before)) %>%
   dplyr::mutate(
      # base wpa
      lead_wp_before = dplyr::lead(.data$wp_before, 1),
      wpa_base = .data$lead_wp_before - .data$wp_before,
      # account for turnover
      
      wpa_change = ifelse(.data$change_of_poss == 1, (1 - .data$lead_wp_before) - .data$wp_before, .data$wpa_base),
      wpa = ifelse(.data$end_of_half == 1, 0, .data$wpa_change),
      wp_after = .data$wp_before + .data$wpa,
      def_wp_after = 1 - .data$wp_after,
      home_wp_after = ifelse(.data$offense_play == .data$home,
                            .data$home_wp_before + .data$wpa,
                            .data$home_wp_before - .data$wpa),
      away_wp_after = ifelse(.data$offense_play != .data$home,
                            .data$away_wp_before + .data$wpa,
                            .data$away_wp_before - .data$wpa),
      wp_before = round(.data$wp_before, 7),
      def_wp_before = round(.data$def_wp_before, 7),
      home_wp_before = round(.data$home_wp_before, 7),
      away_wp_before = round(.data$away_wp_before, 7),
      lead_wp_before = round(.data$lead_wp_before, 7),
      wpa_base = round(.data$wpa_base, 7),
      wpa_change = round(.data$wpa_change, 7),
      wpa = round(.data$wpa, 7),
      wp_after = round(.data$wp_after, 7),
      def_wp_after = round(.data$def_wp_after, 7),
      home_wp_after = round(.data$home_wp_after, 7),
      away_wp_after = round(.data$away_wp_after, 7),
      
    )
  return(df2)
}
