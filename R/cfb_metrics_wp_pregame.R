#' Get Pre-game Win Probability Data from API
#'
#' @param year (*Integer* optional): Year, 4 digit format (*YYYY*)
#' @param week (*Integer* optional): Week - values from 1-15, 1-14 for seasons pre-playoff, i.e. 2013 or earlier
#' @param team (*String* optional): D-I Team
#' @param season_type (*String* default regular): Select Season Type: regular or postseason
#' 
#' @return A data frame with 9 variables:
#' \describe{
#'   \item{`season`}{integer.}
#'   \item{`season_type`}{character.}
#'   \item{`week`}{integer.}
#'   \item{`game_id`}{integer.}
#'   \item{`home_team`}{character.}
#'   \item{`away_team`}{character.}
#'   \item{`spread`}{integer.}
#'   \item{`home_win_prob`}{double.}
#'   \item{`away_win_prob`}{double.}
#' }
#' @keywords Pre-game Win Probability Data
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET
#' @importFrom utils URLencode URLdecode
#' @importFrom assertthat assert_that
#' @importFrom janitor clean_names
#' @importFrom glue glue
#' @import dplyr
#' @import tidyr
#' @export
#' @examples
#'
#' cfb_metrics_wp_pregame(year = 2019, week=9, team='Texas A&M')
#'

cfb_metrics_wp_pregame <- function(year = NULL,
                                   week = NULL,
                                   team = NULL,
                                   season_type = 'regular') {
  
  if(!is.null(year)){
    # Check if year is numeric, if not NULL
    assertthat::assert_that(is.numeric(year) & nchar(year) == 4,
                msg = 'Enter valid year (Integer): 4-digit (YYYY)')
  }
  if(!is.null(week)){
    # Check if week is numeric, if not NULL
    assertthat::assert_that(is.numeric(week) & nchar(week) <= 2 & week <= 15,
                msg = 'Enter valid week (Integer): 1-15\n(14 for seasons pre-playoff, i.e. 2014 or earlier)')
  }
  if(!is.null(team)){
    # Encode team parameter for URL, if not NULL
    team = utils::URLencode(team, reserved = TRUE)
  }
  if(season_type != 'regular'){
    # Check if season_type is appropriate, if not NULL
    assertthat::assert_that(season_type %in% c('postseason'),
                msg = 'Enter valid season_type (String): regular or postseason.')
  }
  base_url <- "https://api.collegefootballdata.com/metrics/wp/pregame?"
  
  full_url <- paste0(base_url,
                     "year=", year,
                     "&week=", week,
                     "&team=", team,
                     "&seasonType=", season_type)
  
  # Check for internet
  check_internet()
  
  # Create the GET request and set response as res
  res <- httr::GET(full_url)
  
  # Check the result
  check_status(res)
  
  cols <- c("season", "season_type", "week", "game_id", 
            "home_team", "away_team", "spread", "home_win_prob","away_win_prob")
  
  df <- data.frame()
  tryCatch(
    expr ={
      # Get the content and return it as data.frame
      df = jsonlite::fromJSON(full_url) %>% 
        janitor::clean_names() %>% 
        dplyr::mutate(away_win_prob = 1 - as.numeric(.data$home_win_prob)) %>%  
        dplyr::select(cols) %>% 
        as.data.frame()
      
      message(glue::glue("{Sys.time()}: Scraping pre-game win probability data..."))
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments or no pre-game win probability data available!"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )
  return(df)
}
