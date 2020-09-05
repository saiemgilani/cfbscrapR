#' CFB Recruiting
#' Gets CFB recruiting information for a single year with filters available for team,
#' recruit type, state and position.
#'
#' At least one of \strong{year} or \strong{team} must be specified for the function to run
#'
#' If you would like CFB recruiting information for teams, please see the \code{\link[cfbscrapR:cfb_recruiting_team]{cfbscrapR::cfb_recruiting_team()}}  function
#'
#' If you would like to get cfb recruiting information based on position groups during a
#' time period for all FBS teams, please see the \code{\link[cfbscrapR:cfb_recruiting_position]{cfbscrapR::cfb_recruiting_position()}} function.
#'
#' @param year (\emph{Integer} optional): Year, 4 digit format (\emph{YYYY}) - Minimum: 2000, Maximum: 2020 currently
#' @param team (\emph{String} optional): D-I Team
#' @param recruit_type (\emph{String} optional): default API return is 'HighSchool', other options include 'JUCO'
#' or 'PrepSchool'  - For position group information
#' @param state (\emph{String} optional): Two letter State abbreviation
#' @param position (\emph{String} optional): Position Group  - options include:\cr
#'  * Offense: 'PRO', 'DUAL', 'RB', 'FB', 'TE',  'OT', 'OG', 'OC', 'WR'\cr
#'  * Defense: 'CB', 'S', 'OLB', 'ILB', 'WDE', 'SDE', 'DT'\cr
#'  * Special Teams: 'K', 'P'\cr
#'
#' @keywords Recruiting
#' @importFrom attempt "stop_if_all"
#' @importFrom jsonlite "fromJSON"
#' @importFrom httr "GET"
#' @importFrom utils "URLencode"
#' @importFrom assertthat "assert_that"
#' @importFrom glue "glue"
#' @import dplyr
#' @export
#' @examples
#'
#' cfb_recruiting_player(2018, team = "Texas")
#'
#' cfb_recruiting_player(2016, recruit_type = 'JUCO')
#'
#' cfb_recruiting_player(2020, recruit_type = 'HighSchool', position = 'OT', state = 'FL')
#'

cfb_recruiting_player <- function(year = NULL,
                                  team = NULL,
                                  recruit_type = 'HighSchool',
                                  state = NULL,
                                  position = NULL){

  args <- list(year = year,
               team = team)

  # Check that at least one argument is not null
  attempt::stop_if_all(args, is.null,
              msg = "You need to specify at least one of two arguments:\n year, as a number (YYYY) - Min: 2000, Max: 2020\n or team")
  # Position Group vector to check arguments against
  pos_groups <- c('PRO', 'DUAL', 'RB', 'FB', 'TE',  'OT', 'OG', 'OC', 'WR',
                  'CB', 'S', 'OLB', 'ILB', 'WDE', 'SDE', 'DT', 'K', 'P')
  if(!is.null(year)){
    ## check if year is numeric
    assertthat::assert_that(is.numeric(year) & nchar(year)==4,
                msg = 'Enter valid year as a number (YYYY) - Min: 2000, Max: 2020')
  }
  if(!is.null(team)){
    # Encode team parameter for URL if not NULL
    team = utils::URLencode(team, reserved = TRUE)
  }
  if(recruit_type !='HighSchool'){
    # Check if recruit_type is appropriate, if not HighSchool
    assertthat::assert_that(recruit_type %in% c('PrepSchool','JUCO'),
                msg = 'Enter valid recruit_type (String): HighSchool, PrepSchool, or JUCO')
  }
  if(!is.null(state)){
    ## check if state is length 2
    assertthat::assert_that(nchar(state)==2,
                msg = 'Enter valid 2-letter State abbreviation')
  }
  if(!is.null(position)){
    ## check if position in position group set
    assertthat::assert_that(position %in% pos_groups,
                msg = 'Enter valid position group \nOffense: PRO, DUAL, RB, FB, TE, OT, OG, OC, WR\nDefense: CB, S, OLB, ILB, WDE, SDE, DT\nSpecial Teams: K, P')
  }

  base_url = "https://api.collegefootballdata.com/recruiting/players?"

  # Create full url using base and input arguments
  full_url = paste0(base_url,
                    "year=", year,
                    "&team=", team,
                    "&classification=", recruit_type,
                    "&position=", position,
                    "&state=", state)

  # Check for internet
  check_internet()

  # Create the GET request and set response as res
  res <- httr::GET(full_url)

  # Check the result
  check_status(res)
  
  df <- data.frame()
  tryCatch(
    expr ={  
      # Get the content and return it as data.frame
      df = jsonlite::fromJSON(full_url) %>% 
        dplyr::rename(
          recruit_type = .data$recruitType,
          committed_to = .data$committedTo,
          state_province = .data$stateProvince) %>% 
        as.data.frame()
      
      message(glue::glue("{Sys.time()}: Scraping player recruiting data..."))
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments or no player recruiting data available!"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )
  return(df)
}


