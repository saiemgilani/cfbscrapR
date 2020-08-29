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
    df = df %>% mutate(
      play_after_turnover = ifelse(lag(.data$turnover_vec,1) == 1 & lag(.data$def_td_play,1) != 1, 1, 0),
      score_diff = .data$offense_score - .data$defense_score,
      score_diff_start = ifelse(.data$play_after_turnover == 1, 
                                -ifelse(.data$game_play_number == 1, 0, lag(.data$score_diff,1)),
                                ifelse(.data$scoring_play == 1, 
                                       ifelse(.data$game_play_number == 1, 0, lag(.data$score_diff,1)), 
                                       .data$score_diff)),
      home_EPA = ifelse(.data$offense_play == .data$home, .data$EPA,-.data$EPA),
      away_EPA = -.data$home_EPA,
      ExpScoreDiff = .data$score_diff_start + .data$ep_before,
      half = as.factor(.data$half),
      ExpScoreDiff_Time_Ratio = .data$ExpScoreDiff / (.data$adj_TimeSecsRem + 1)
    )
  }

  df = df %>% 
    arrange(.data$game_id, .data$new_id)
  
  Off_Win_Prob = as.vector(predict(wp_model, newdata = df, type = "response"))
  df$wp_before = Off_Win_Prob

  g_ids = sort(unique(df$game_id))
  df2 = purrr::map_dfr(g_ids,
                       function(x) {
                         df %>%
                           filter(.data$game_id == x) %>%
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
    mutate(
      def_wp_before = 1 - .data$wp_before,
      home_wp_before = if_else(.data$offense_play == .data$home,
                        .data$wp_before, 
                        .data$def_wp_before),
      away_wp_before = if_else(.data$offense_play != .data$home,
                        .data$wp_before, 
                        .data$def_wp_before)) %>%
    mutate(
      # base wpa
      end_of_half = ifelse(.data$half == lead(.data$half), 0, 1),
      lead_wp_before = dplyr::lead(.data$wp_before),
      # account for turnover
      wpa_base = .data$lead_wp_before - .data$wp_before,
      wpa_change = ifelse(.data$change_of_poss == 1, (1 - .data$lead_wp_before) - .data$wp_before, .data$wpa_base),
      wpa = ifelse(.data$end_of_half == 1, 0, .data$wpa_change),
      home_wp_post = ifelse(.data$offense_play == .data$home,
                            .data$home_wp_before + .data$wpa,
                            .data$home_wp_before - .data$wpa),
      away_wp_post = ifelse(.data$offense_play != .data$home,
                            .data$away_wp_before + .data$wpa,
                            .data$away_wp_before - .data$wpa)
    )
  return(df2)
}
