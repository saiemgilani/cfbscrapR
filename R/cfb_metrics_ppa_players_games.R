#' Get player game averages for Predicted Points Added (PPA)
#'
#' @param year (*Integer* required): Year, 4 digit format (*YYYY*)
#' @param week (*Integer* optional): Week - values range from 1-15, 1-14 for seasons pre-playoff, i.e. 2013 or earlier
#' @param team (*String* optional): D-I Team
#' @param position (*string* optional): Position abbreviation of the player you are searching for.\cr
#' Position Group  - options include:\cr
#'  * Offense: QB, RB, FB, TE,  OL, G, OT, C, WR\cr
#'  * Defense: DB, CB, S, LB,  DE, DT, NT, DL\cr
#'  * Special Teams: K, P, LS, PK\cr
#' @param athlete_id (*Integer* optional): Athlete ID filter for querying a single athlete\cr
#' Can be found using the [cfbscrapR::cfb_player_info()] function.
#' @param threshold (*Integer* optional): Minimum threshold of plays.
#' @param excl_garbage_time (*Logical* default FALSE): Select whether to exclude Garbage Time (TRUE or FALSE)
#' 
#' @return A data frame with 9 variables:
#' \describe{
#'   \item{`season`}{integer.}
#'   \item{`week`}{integer.}
#'   \item{`name`}{character.}
#'   \item{`position`}{character.}
#'   \item{`team`}{character.}
#'   \item{`opponent`}{character.}
#'   \item{`avg_PPA_all`}{double.}
#'   \item{`avg_PPA_pass`}{double.}
#'   \item{`avg_PPA_rush`}{double.}
#' }
#' @source <https://api.collegefootballdata.com/ppa/players/games>
#' @keywords Players Predicted Points 
#' @importFrom attempt stop_if_all
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET
#' @importFrom utils URLencode 
#' @importFrom assertthat assert_that
#' @importFrom glue glue
#' @import dplyr
#' @import tidyr
#' @export
#' @examples
#' 
#' cfb_metrics_ppa_players_games(year = 2019,week=3, team = 'TCU')
#'

cfb_metrics_ppa_players_games <- function(year = NULL,
                                    week = NULL,
                                    team = NULL,
                                    position = NULL,
                                    athlete_id = NULL,
                                    threshold = NULL,
                                    excl_garbage_time = FALSE){

  # Position Group vector to check input arguments against
  pos_groups <- c('QB', 'RB', 'FB', 'TE', 'WR', 'OL', 'OT', 'G', 'OC',
                  'DB', 'CB', 'S', 'LB', 'DE', 'NT','DL', 'DT',
                  'K', 'P','PK','LS')
  

  if(!is.null(year)){
    ## check if year is numeric
    assertthat::assert_that(is.numeric(year) & nchar(year)==4,
                msg = 'Enter valid year as integer in 4 digit format (YYYY)')
  }
  if(!is.null(week)){
    # Check if week is numeric, if not NULL
    assertthat::assert_that(is.numeric(week) & nchar(week) <= 2,
                msg = 'Enter valid week (Integer): 1-15\n(14 for seasons pre-playoff, i.e. 2014 or earlier)')
  }
  if(!is.null(team)){
    if(team == "San Jose State"){
      team = utils::URLencode(paste0("San Jos","\u00e9", " State"), reserved = TRUE)
    } else{
      # Encode team parameter for URL if not NULL
      team = utils::URLencode(team, reserved = TRUE)
    }
  }
  if(!is.null(position)){
    ## check if position in position group set
    assertthat::assert_that(position %in% pos_groups,
                msg = 'Enter valid position group\nOffense: QB, RB, FB, TE, WR,  OL, G, OT, C\nDefense: DB, CB, S, LB, DL, DE, DT, NT\nSpecial Teams: K, P, LS, PK')
  }
  if(!is.null(athlete_id)){
    # Check if athlete_id is numeric, if not NULL
    assertthat::assert_that(is.numeric(athlete_id),
                msg = 'Enter valid athlete_id value (Integer)\nCan be found using the `cfb_player_info()` function')
  }
  if(!is.null(threshold)){
    # Check if threshold is numeric, if not NULL
    assertthat::assert_that(is.numeric(threshold),
                msg = 'Enter valid threshold value (Integer)')
  }
  if(excl_garbage_time!=FALSE){
    # Check if excl_garbage_time is TRUE, if not FALSE
    assertthat::assert_that(excl_garbage_time==TRUE,
                msg = 'Enter valid excl_garbage_time value (Logical) - TRUE or FALSE')
  }
  
  base_url <- "https://api.collegefootballdata.com/ppa/players/games?"
  
  full_url <- paste0(base_url,
                     "year=", year,
                     "&week=", week,
                     "&team=", team,
                     "&position=", position,
                     "&playerId=", athlete_id,
                     "&threshold=", threshold,
                     "&excludeGarbageTime=", excl_garbage_time)
  
  # Check for internet
  check_internet()
  
  # Create the GET request and set response as res
  res <- httr::GET(full_url)
  
  # Check the result
  check_status(res)
  
  df <- data.frame()
  tryCatch(
    expr = {
      # Get the content, flatten and return result as data.frame
      df = jsonlite::fromJSON(full_url, flatten = TRUE) 
      colnames(df) = gsub("averagePPA.", "avg_PPA_", colnames(df))
      
      message(glue::glue("{Sys.time()}: Scraping CFBData metrics PPA game-level players data..."))
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments or no CFBData metrics PPA game-level players data available!"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )    
  return(df)
}
