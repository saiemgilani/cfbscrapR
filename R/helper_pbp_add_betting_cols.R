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
#' @importFrom rlang .data
#' @importFrom dplyr mutate rename select left_join filter
#' @importFrom glue glue
#' @export
#' 

add_betting_cols <- function(play_df, g_id, yr){
  tryCatch(
    expr = {
      game_spread <- cfb_betting_lines(game_id = g_id, year=yr) 
      
      game_spread <- game_spread %>%
        dplyr::filter(.data$provider == 'consensus') %>% 
        dplyr::mutate(spread = as.numeric(.data$spread),
                      over_under = as.numeric(.data$over_under)) %>% 
        dplyr::select(.data$game_id, .data$spread, .data$formatted_spread, .data$over_under)
      
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