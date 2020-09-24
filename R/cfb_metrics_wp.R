#' Get win probability chart data from API
#'
#' @param game_id (\emph{Integer} required): Game ID filter for querying a single game\cr
#' Can be found using the \code{\link[cfbscrapR:cfb_game_info]{cfbscrapR::cfb_game_info()}} function
#' 
#' @return A data frame with 16 variables:
#' \describe{
#'   \item{\code{play_id}}{character.}
#'   \item{\code{play_text}}{character.}
#'   \item{\code{home_id}}{integer.}
#'   \item{\code{home}}{character.}
#'   \item{\code{away_id}}{integer.}
#'   \item{\code{away}}{character.}
#'   \item{\code{spread}}{character.}
#'   \item{\code{home_ball}}{logical.}
#'   \item{\code{home_score}}{integer.}
#'   \item{\code{away_score}}{integer.}
#'   \item{\code{down}}{integer.}
#'   \item{\code{distance}}{integer.}
#'   \item{\code{home_win_prob}}{character.}
#'   \item{\code{away_win_prob}}{double.}
#'   \item{\code{play_number}}{integer.}
#'   \item{\code{yard_line}}{integer.}
#' }
#' @source \url{https://api.collegefootballdata.com/metrics/wp}
#' @keywords Win Probability Chart Data
#' @importFrom attempt "stop_if_all"
#' @importFrom jsonlite "fromJSON"
#' @importFrom httr "GET"
#' @importFrom utils "URLencode" "URLdecode"
#' @importFrom assertthat "assert_that"
#' @importFrom janitor "clean_names"
#' @importFrom glue "glue"
#' @import dplyr
#' @import tidyr
#' @export
#' @examples
#'
#' cfb_metrics_wp(game_id = 401012356)
#'

cfb_metrics_wp <- function(game_id) {

  args <- list(game_id = game_id)
  
  # Check that at search_term input argument is not null
  attempt::stop_if_all(args, is.null,
              msg = "You need to specify at least one argument: game_id\n Can be found using the `cfb_game_info()` function")
  
  if(!is.null(game_id)){
    # Check if game_id is numeric, if not NULL
    assertthat::assert_that(is.numeric(game_id),
                msg = 'Enter valid game_id value (Integer)\nCan be found using the `cfb_game_info()` function')
  }
  
  base_url <- "https://api.collegefootballdata.com/metrics/wp?"

  full_url <- paste0(base_url,
                     "gameId=", game_id)

  # Check for internet
  check_internet()

  # Create the GET request and set response as res
  res <- httr::GET(full_url)

  # Check the result
  check_status(res)
  
  cols <- c("play_id", "play_text", "home_id", "home", "away_id", "away",       
            "spread", "home_ball", "home_score", "away_score", "down",
            "distance", "home_win_prob", "away_win_prob", "play_number",  "yard_line")
  
  df <- data.frame()
  tryCatch(
    expr = {
      # Get the content and return it as data.frame
      df = jsonlite::fromJSON(full_url) %>% 
        janitor::clean_names() %>% 
        dplyr::mutate(away_win_prob = 1 - as.numeric(.data$home_win_prob)) %>% 
        dplyr::select(cols)
        as.data.frame()
      
      message(glue::glue("{Sys.time()}: Scraping CFBData metrics win probability data..."))
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments or no CFBData metrics win probability data available!"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )    
  return(df)
}
