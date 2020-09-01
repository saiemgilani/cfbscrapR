#' Player Information Search
#'
#' A player search function with \strong{search_term} as a required input. If left NULL, API default will only provide results for most recent year of final rosters: 2019
#' Would not recommend using the position group filter,it feels arbitrary and pernicious,
#' but it is there. May fortune favor you should you choose to use it.
#'
#' @param search_term (\emph{String} required): Search term for the player you are trying to look up
#' @param position (\emph{string} optional): Position of the player you are searching for.\cr
#' Position Group  - options include:\cr
#'  * Offense: QB, RB, FB, TE,  OL, G, OT, C, WR\cr
#'  * Defense: DB, CB, S, LB,  DE, DT, NT, DL\cr
#'  * Special Teams: K, P, LS, PK\cr
#' @param team (\emph{String} optional): Team - Select a valid team, D1 football
#' @param year (\emph{Integer} optional): Year, 4 digit format (\emph{YYYY}).
#' If left NULL, API default will only provide results for most recent year of final rosters: 2019
#'
#' @keywords Recruiting
#' @importFrom attempt "stop_if_all"
#' @importFrom jsonlite "fromJSON"
#' @importFrom httr "GET"
#' @importFrom utils "URLencode"
#' @importFrom assertthat "assert_that"
#' @export
#' @examples
#'
#' cfb_player_info(search_term = 'James', position = 'DB', team = 'Florida State', year = 2017)
#'
#' cfb_player_info(search_term = 'Lawrence', team = "Clemson")
#'
#' cfb_player_info(search_term = 'Tua', position = 'QB')
#'
#' cfb_player_info(search_term = 'Duggan')
#'

cfb_player_info <- function(search_term,
                            position = NULL,
                            team = NULL,
                            year = NULL){

  args <- list(search_term = search_term)

  # Check that at search_term input argument is not null
  stop_if_all(args, is.null,
              msg="You need to specify at least one argument:\nsearch_term as a string for the player you are trying to look up")

  # Encode search_term parameter for URL
  search_term = URLencode(search_term, reserved = TRUE)

  # Position Group vector to check input arguments against
  pos_groups <- c('QB', 'RB', 'FB', 'TE', 'WR', 'OL', 'OT', 'G', 'OC',
                  'DB', 'CB', 'S', 'LB', 'DE', 'NT','DL', 'DT',
                  'K', 'P','PK','LS')

  if(!is.null(position)){
    ## check if position in position group set
    assert_that(position %in% pos_groups,
                msg='Enter valid position group\nOffense: QB, RB, FB, TE, WR,  OL, G, OT, C\nDefense: DB, CB, S, LB, DL, DE, DT, NT\nSpecial Teams: K, P, LS, PK')
  }
  if(!is.null(team)){
    # Encode team parameter for URL if not NULL
    team = URLencode(team, reserved = TRUE)
  }
  if(!is.null(year)){
    ## check if year is numeric
    assert_that(is.numeric(year) & nchar(year)==4,
                msg='Enter valid year as integer in 4 digit format (YYYY)\n Min: 2000, Max: 2020')
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
  res <- GET(full_url)

  # Check the result
  check_status(res)

  # Get the content and return it as data.frame
  df = fromJSON(full_url)
  if(nrow(df)==0){
    warning("The data pulled from the API was empty. Returning nothing
            for this one search term.")
    return(NULL)
  }
  
  df <- df %>% 
    rename(
      athlete_id = .data$id,
      home_town = .data$hometown,
      first_name = .data$firstName,
      last_name = .data$lastName,
      team_color = .data$teamColor,
      team_color_secondary = .data$teamColorSecondary) %>% 
    as.data.frame()
  
  return(df)
}
