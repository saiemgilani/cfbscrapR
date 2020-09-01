#' Get win probability chart data from ESPN
#' Graciously contributed by MrCaseB: https://gist.github.com/mrcaseb/0f868193affb4be152e8e82c43a4dc07
#' @param game_id (\emph{Integer} required): Game ID filter for querying a single game\cr
#' Can be found using the \code{\link[cfbscrapR:cfb_game_info]{cfbscrapR::cfb_game_info()}} function
#' @keywords Win Probability Chart Data
#' @importFrom jsonlite "fromJSON"
#' @importFrom httr "GET"
#' @importFrom utils "URLencode" "URLdecode"
#' @importFrom assertthat "assert_that"
#' @importFrom janitor "clean_names"
#' @export
#'
#' @examples
#'
#' cfb_metrics_espn_wp(game_id = 401012356)
#'
#'

cfb_metrics_espn_wp <- function(game_id) {

  args <- list(game_id = game_id)
  
  # Check that at search_term input argument is not null
  stop_if_all(args, is.null,
              msg="You need to specify at least one argument: game_id\n Can be found using the `cfb_game_info()` function")
  
  if(!is.null(game_id)){
    # Check if game_id is numeric, if not NULL
    assert_that(is.numeric(game_id),
                msg='Enter valid game_id value (Integer)\nCan be found using the `cfb_game_info()` function')
  }

  # Check for internet
  check_internet()

  espn_game_id <- game_id
  
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
          espn_game_id = stringr::str_sub(.data$play_id, end = stringr::str_length(.data$espn_game_id))
        ) %>%
        
        dplyr::mutate(
          away_win_percentage = 1 - .data$home_win_percentage
        )%>%
        dplyr::select(.data$espn_game_id, .data$play_id, .data$seconds_left, 
                      .data$home_win_percentage, .data$away_win_percentage) 
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