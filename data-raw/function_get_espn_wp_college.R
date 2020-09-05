library(dplyr)
get_espn_wp_college <- function(espn_game_id) {
  espn_wp <- data.frame()
  tryCatch(
    expr = {
      espn_wp <-
        httr::GET(url = glue::glue("http://site.api.espn.com/apis/site/v2/sports/football/college-football/summary?event={espn_game_id}")) %>%
        httr::content(as = "text", encoding = "UTF-8") %>%
        jsonlite::fromJSON(flatten = TRUE) %>%
        purrr::pluck("winprobability") %>%
        janitor::clean_names() %>%
        dplyr::mutate(
          espn_game_id = stringr::str_sub(play_id, end = stringr::str_length(espn_game_id))
        ) %>%
        dplyr::select(espn_game_id, play_id, home_win_percentage) 
      message(glue::glue("{Sys.time()}: Scraping ESPN wp data for GameID '{espn_game_id}'..."))
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: GameID '{espn_game_id}' invalid or no wp data available!"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )
  return(espn_wp)
}