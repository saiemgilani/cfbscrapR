#' CFB Recruiting
#' Gets CFB recruiting information for a single year with filters available for team,
#' recruit type, state and position.
#'
#' At least one of \strong{year} or \strong{team} must be specified for the function to run
#'
#' If you would like CFB recruiting information for teams, please
#' see the `cfb_recruiting_team()` function
#'
#' If you would like to get cfb recruiting information based on position groups during a
#' time period for all FBS teams, please see the `cfb_recruiting_position()` function.
#'
#' @param year (\emph{Integer} optional): Year, 4 digit format (\emph{YYYY}) - Minimum: 2000, Maximum: 2020 currently
#' @param team (\emph{String} optional): D-I Team
#' @param recruit_type (\emph{String} optional): default API return is 'HighSchool', other options include 'JUCO'
#' or 'PrepSchool'  - For position group information
#' @param state (\emph{String} optional): Two letter State abbreviation
#' @param position (\emph{String} optional): Position Group  - options include:
#'  * Offense: 'PRO', 'DUAL', 'RB', 'FB', 'TE',  'OT', 'OG', 'OC', 'WR'
#'  * Defense: 'CB', 'S', 'OLB', 'ILB', 'WDE', 'SDE', 'DT'
#'  * Special Teams: 'K', 'P'
#'
#' @keywords Recruiting
#' @importFrom attempt stop_if_all
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET
#' @importFrom utils "URLencode"
#' @importFrom assertthat "assert_that"
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
  stop_if_all(args, is.null,
              msg="You need to specify at least one of two arguments:\n year, as a number (YYYY) - Min: 2000, Max: 2020\n or team")
  # Position Group vector to check arguments against
  pos_groups <- c('PRO', 'DUAL', 'RB', 'FB', 'TE',  'OT', 'OG', 'OC', 'WR',
                  'CB', 'S', 'OLB', 'ILB', 'WDE', 'SDE', 'DT', 'K', 'P')
  if(!is.null(year)){
    ## check if year is numeric
    assert_that(is.numeric(year) & nchar(year)==4,
                msg='Enter valid year as a number (YYYY) - Min: 2000, Max: 2020')
  }
  if(!is.null(team)){
    # Encode team parameter for URL if not NULL
    team = URLencode(team, reserved = TRUE)
  }
  if(recruit_type !='HighSchool'){
    # Check if recruit_type is appropriate, if not HighSchool
    assert_that(recruit_type %in% c('PrepSchool','JUCO'),
                msg='Enter valid recruit_type (String): HighSchool, PrepSchool, or JUCO')
  }
  if(!is.null(state)){
    ## check if state is length 2
    assert_that(nchar(state)==2,
                msg='Enter valid 2-letter State abbreviation')
  }
  if(!is.null(position)){
    ## check if position in position group set
    assert_that(position %in% pos_groups,
                msg='Enter valid position group \nOffense: PRO, DUAL, RB, FB, TE, OT, OG, OC, WR\nDefense: CB, S, OLB, ILB, WDE, SDE, DT\nSpecial Teams: K, P')
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
  res <- GET(full_url)

  # Check the result
  check_status(res)

  # Get the content and return it as data.frame
  df = fromJSON(full_url)

  return(df)
}


