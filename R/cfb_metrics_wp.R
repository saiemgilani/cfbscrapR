#' Get win probability chart data from API
#'
#' @param game_id (\emph{Integer} required): Game ID filter for querying a single game
#' Can be found using the `cfb_game_info()` function
#' @param adjust_for_spread (\emph{Logical} default TRUE): Toggles pre-game spread adjustments
#' @keywords Win Probability Chart Data
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET
#' @importFrom utils "URLencode" "URLdecode"
#' @importFrom assertthat "assert_that"
#' @export
#'
#' @examples
#'
#' cfb_metrics_wp(game_id = 401012356)
#'
#'

cfb_metrics_wp <- function(game_id,
                           adjust_for_spread = TRUE) {

  args <- list(game_id = game_id)
  
  # Check that at search_term input argument is not null
  stop_if_all(args, is.null,
              msg="You need to specify at least one argument: game_id\n Can be found using the `cfb_game_info()` function")
  
  if(!is.null(game_id)){
    # Check if game_id is numeric, if not NULL
    assert_that(is.numeric(game_id),
                msg='Enter valid game_id value (Integer)\nCan be found using the `cfb_game_info()` function')
  }
  if(adjust_for_spread!=TRUE){
    # Check if adjust_for_spread is FALSE, if not TRUE
    assert_that(adjust_for_spread==FALSE,
                msg='Enter valid adjust_for_spread value (Logical) - TRUE or FALSE')
  }
  base_url <- "https://api.collegefootballdata.com/metrics/wp?"

  full_url <- paste0(base_url,
                     "gameId=", game_id,
                     "&adjustForSpread=", adjust_for_spread)

  # Check for internet
  check_internet()

  # Create the GET request and set response as res
  res <- GET(full_url)

  # Check the result
  check_status(res)

  # Get the content and return it as data.frame
  df = fromJSON(full_url)

  return(df)
}
