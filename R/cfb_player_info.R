#' Player Information Search
#'
#' A player search function with **`search_term`** as a required input. If left NULL, API default will only provide results for most recent year of final rosters: 2019
#' Would not recommend using the position group filter,it feels arbitrary and pernicious,
#' but it is there. May fortune favor you should you choose to use it.
#'
#' @param search_term (*String* required): Search term for the player you are trying to look up
#' @param position (*string* optional): Position of the player you are searching for.\cr
#' Position Group  - options include:\cr
#'  * Offense: QB, RB, FB, TE,  OL, G, OT, C, WR\cr
#'  * Defense: DB, CB, S, LB,  DE, DT, NT, DL\cr
#'  * Special Teams: K, P, LS, PK\cr
#' @param team (*String* optional): Team - Select a valid team, D1 football
#' @param year (*Integer* optional): Year, 4 digit format (*YYYY*).
#' If left NULL, API default will only provide results for most recent year of final rosters: 2020\cr
#' @return A data frame with 12 variables:
#' \describe{
#'   \item{`athlete_id`}{character. Unique player identifier - `athlete_id`.}
#'   \item{`team`}{character. Team of the player.}
#'   \item{`name`}{character. Player name.}
#'   \item{`first_name`}{character. Player first name.}
#'   \item{`last_name`}{character. Player last name.}
#'   \item{`weight`}{integer. Player weight.}
#'   \item{`height`}{integer. Player height.}
#'   \item{`jersey`}{integer. Player jersey number.}
#'   \item{`position`}{character. Player position.}
#'   \item{`home_town`}{character. Player home town.}
#'   \item{`team_color`}{character. Player team color.}
#'   \item{`team_color_secondary`}{character. Player team secondary color.}
#' }
#' @source <https://api.collegefootballdata.com/player/search>
#' @keywords Recruiting
#' @importFrom attempt stop_if_all
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET
#' @importFrom utils URLencode
#' @importFrom assertthat assert_that
#' @importFrom janitor clean_names
#' @importFrom glue glue
#' @import dplyr
#' @import tidyr
#' @export
#' @examples
#'
#' cfb_player_info(search_term = 'James', position = 'DB', team = 'Florida State', year = 2017)
#'
#' cfb_player_info(search_term = 'Lawrence', team = "Clemson")
#'
#' cfb_player_info(search_term = 'Duggan')
#'

cfb_player_info <- function(search_term,
                            position = NULL,
                            team = NULL,
                            year = NULL){
  
  args <- list(search_term = search_term)

  # Check that at search_term input argument is not null
  attempt::stop_if_all(args, is.null,
              msg = "You need to specify at least one argument:\nsearch_term as a string for the player you are trying to look up")

  # Encode search_term parameter for URL
  search_term = utils::URLencode(search_term, reserved = TRUE)

  # Position Group vector to check input arguments against
  pos_groups <- c('QB', 'RB', 'FB', 'TE', 'WR', 'OL', 'OT', 'G', 'OC',
                  'DB', 'CB', 'S', 'LB', 'DE', 'NT','DL', 'DT',
                  'K', 'P','PK','LS')

  if(!is.null(position)){
    ## check if position in position group set
    assertthat::assert_that(position %in% pos_groups,
                msg = 'Enter valid position group\nOffense: QB, RB, FB, TE, WR,  OL, G, OT, C\nDefense: DB, CB, S, LB, DL, DE, DT, NT\nSpecial Teams: K, P, LS, PK')
  }
  if(!is.null(team)){
    if(team == "San Jose State"){
      team = utils::URLencode(paste0("San Jos","\u00e9", " State"), reserved = TRUE)
    } else{
      # Encode team parameter for URL if not NULL
      team = utils::URLencode(team, reserved = TRUE)
    }
  }
  if(!is.null(year)){
    ## check if year is numeric
    assertthat::assert_that(is.numeric(year) & nchar(year)==4,
                msg = 'Enter valid year as integer in 4 digit format (YYYY)\n Min: 2000, Max: 2020')
  }
  base_url = "https://api.collegefootballdata.com/player/search?"

  # Create full url using base and input arguments
  full_url = paste0(base_url,
                    "searchTerm=", search_term,
                    "&position=",position,
                    "&team=", team,
                    "&year=",year)

  # Check for internet
  check_internet()

  # Create the GET request and set response as res
  res <- httr::GET(full_url)

  # Check the result
  check_status(res)
  
  df <- data.frame()
  tryCatch(
    expr = {
      # Get the content and return it as data.frame
      df = jsonlite::fromJSON(full_url) %>% 
        janitor::clean_names() %>% 
        dplyr::rename(
          athlete_id = .data$id,
          home_town = .data$hometown
        ) %>% 
        as.data.frame()
      
      message(glue::glue("{Sys.time()}: Scraping player info data..."))
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments or no player info data available!"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )        
  return(df)
}
