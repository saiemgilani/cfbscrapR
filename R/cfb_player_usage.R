#' Player Information Search
#'
#' A player usage function with **Year** as a required input.
#'
#' @param year (*Integer* required, default 2019): Year, 4 digit format (*YYYY*).
#' @param team (*String* optional): Team - Select a valid team, D1 football
#' @param conference (*String* optional): Conference abbreviation - Select a valid FBS conference\cr
#' Conference abbreviations P5: ACC, B12, B1G, SEC, PAC\cr
#' Conference abbreviations G5 and FBS Independents: CUSA, MAC, MWC, Ind, SBC, AAC\cr
#' @param position (*string* optional): Position of the player you are searching for.\cr
#' Position Group  - options include:\cr
#'  * Offense: QB, RB, FB, TE,  OL, G, OT, C, WR\cr
#'  * Defense: DB, CB, S, LB,  DE, DT, NT, DL\cr
#'  * Special Teams: K, P, LS, PK\cr
#' @param athlete_id (*Integer* optional): Athlete ID filter for querying a single athlete
#' Can be found using the [cfbscrapR::cfb_player_info()] function.
#' @param excl_garbage_time (*Logical* default FALSE): Select whether to exclude Garbage Time (TRUE/FALSE)
#'
#' @return A data frame with 14 variables:
#' \describe{
#'   \item{`season`}{integer.}
#'   \item{`athlete_id`}{character.}
#'   \item{`name`}{character.}
#'   \item{`position`}{character.}
#'   \item{`team`}{character.}
#'   \item{`conference`}{character.}
#'   \item{`usg_overall`}{double.}
#'   \item{`usg_pass`}{double.}
#'   \item{`usg_rush`}{double.}
#'   \item{`usg_1st_down`}{double.}
#'   \item{`usg_2nd_down`}{double.}
#'   \item{`usg_3rd_down`}{double.}
#'   \item{`usg_standard_downs`}{double.}
#'   \item{`usg_passing_downs`}{double.}
#' }
#' @source <https://api.collegefootballdata.com/player/usage>
#' @keywords Player Usage
#' @importFrom attempt stop_if_all
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET
#' @importFrom utils URLencode
#' @importFrom assertthat assert_that
#' @importFrom glue glue
#' @importFrom purrr map_if
#' @importFrom dplyr as_tibble rename
#' @export
#' @examples
#'
#' cfb_player_usage(year = 2019, position = 'WR', team = 'Florida State')
#'

cfb_player_usage <- function(year = 2019,
                             team = NULL,
                             conference = NULL,
                             position = NULL,
                             athlete_id = NULL,
                             excl_garbage_time = FALSE){

  args <- list(year = year)

  # Check that at search_term input argument is not null
  attempt::stop_if_all(args, is.null,
              msg = "You need to specify at least one argument:\nyear as an integer 4 digit format (YYYY)")

  # Position Group vector to check input arguments against
  pos_groups <- c('QB', 'RB', 'FB', 'TE', 'WR', 'OL', 'OT', 'G', 'OC',
                  'DB', 'CB', 'S', 'LB', 'DE', 'NT','DL', 'DT',
                  'K', 'P','PK','LS')
  if(!is.null(year)){
    ## check if year is numeric
    assertthat::assert_that(is.numeric(year) & nchar(year)==4,
                msg = 'Enter valid year as integer in 4 digit format (YYYY)\n Min: 2000, Max: 2020')
  }
  if(!is.null(team)){
    if(team == "San Jose State"){
      team = utils::URLencode(paste0("San Jos","\u00e9", " State"), reserved = TRUE)
    } else{
      # Encode team parameter for URL if not NULL
      team = utils::URLencode(team, reserved = TRUE)
    }
  }
  if(!is.null(conference)){
    # # Check conference parameter in conference abbreviations, if not NULL
    # assertthat::assert_that(conference %in% cfbscrapR::cfb_conf_types_df$abbreviation,
    #             msg = "Incorrect conference abbreviation, potential misspelling.\nConference abbreviations P5: ACC, B12, B1G, SEC, PAC\nConference abbreviations G5 and Independents: CUSA, MAC, MWC, Ind, SBC, AAC")
    # Encode conference parameter for URL, if not NULL
    conference = utils::URLencode(conference, reserved = TRUE)
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
  if(excl_garbage_time!=FALSE){
    # Check if excl_garbage_time is TRUE, if not FALSE
    assertthat::assert_that(excl_garbage_time==TRUE,
                msg = 'Enter valid excl_garbage_time value (Logical) - TRUE or FALSE')
  }

  base_url = "https://api.collegefootballdata.com/player/usage?"

  # Create full url using base and input arguments
  full_url = paste0(base_url,
                    "year=", year,
                    "&team=", team,
                    "&conference=", conference,
                    "&position=", position,
                    "&athleteID=", athlete_id,
                    "&excludeGarbageTime=",excl_garbage_time)

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
      df = jsonlite::fromJSON(full_url, flatten = TRUE) %>%
        purrr::map_if(is.data.frame,list) %>%
        dplyr::as_tibble() %>%
        dplyr::rename(
          athlete_id = .data$id,
          usg_overall = .data$usage.overall,
          usg_pass = .data$usage.pass,
          usg_rush = .data$usage.rush,
          usg_1st_down = .data$usage.firstDown,
          usg_2nd_down = .data$usage.secondDown,
          usg_3rd_down = .data$usage.thirdDown,
          usg_standard_downs = .data$usage.standardDowns,
          usg_passing_downs = .data$usage.passingDowns) %>% 
        as.data.frame()
      
      message(glue::glue("{Sys.time()}: Scraping player usage data..."))
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments or no player usage data available!"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )        
  return(df)
}
