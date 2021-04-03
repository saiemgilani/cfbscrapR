#' Get results information from games
#'
#' @param year (*Integer* required): Year, 4 digit format(*YYYY*)
#' @param week (*Integer* optional): Week - values from 1-15, 1-14 for seasons pre-playoff (i.e. 2013 or earlier)
#' @param season_type (*String* default regular): Select Season Type: regular, postseason, or both
#' @param team (*String* optional): D-I Team
#' @param home_team (*String* optional): Home D-I Team
#' @param away_team (*String* optional): Away D-I Team
#' @param conference (*String* optional): Conference abbreviation - Select a valid FBS conference\cr
#' Conference abbreviations P5: ACC, B12, B1G, SEC, PAC\cr
#' Conference abbreviations G5 and FBS Independents: CUSA, MAC, MWC, Ind, SBC, AAC\cr
#' @param game_id (*Integer* optional): Game ID filter for querying a single game\cr
#' @param quarter_scores (*Logical* default FALSE): This is a parameter to return the
#' list columns that give the score at each quarter: home_line_scores and away_line scores.\cr
#' I have defaulted the parameter to false so that you will not have to go to the trouble of dropping it.
#' 
#' @return A data frame with 22 variables:
#' \describe{
#'   \item{`game_id`}{integer.}
#'   \item{`season`}{integer.}
#'   \item{`week`}{integer.}
#'   \item{`season_type`}{character.}
#'   \item{`start_date`}{character.}
#'   \item{`start_time_tbd`}{logical.}
#'   \item{`neutral_site`}{logical.}
#'   \item{`conference_game`}{logical.}
#'   \item{`attendance`}{integer.}
#'   \item{`venue_id`}{integer.}
#'   \item{`venue`}{character.}
#'   \item{`home_id`}{integer.}
#'   \item{`home_team`}{character.}
#'   \item{`home_conference`}{character.}
#'   \item{`home_points`}{integer.}
#'   \item{`home_post_win_prob`}{character.}
#'   \item{`away_id`}{integer.}
#'   \item{`away_team`}{character.}
#'   \item{`away_conference`}{character.}
#'   \item{`away_points`}{integer.}
#'   \item{`away_post_win_prob`}{character.}
#'   \item{`excitement_index`}{character.}
#' }
#' @source <https://api.collegefootballdata.com/games>
#' @keywords Game Info
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
#' cfb_game_info(2018, week = 1)
#'
#' cfb_game_info(2018, week = 7, conference = 'Ind')
#'
#' #7 OTs LSU @ TAMU
#' cfb_game_info(2018, week = 13, team = "Texas A&M", quarter_scores=TRUE)
#'

cfb_game_info <- function(year,
                          week = NULL,
                          season_type = 'regular',
                          team = NULL,
                          home_team = NULL,
                          away_team = NULL,
                          conference = NULL,
                          game_id = NULL,
                          quarter_scores = FALSE) {
  # Check if year is numeric
  assertthat::assert_that(is.numeric(year) & nchar(year) == 4,
              msg = 'Enter valid year as a number (YYYY)')
  if(!is.null(week)){
    # Check if week is numeric, if not NULL
    assertthat::assert_that(is.numeric(week) & nchar(week) <= 2,
                msg = 'Enter valid week 1-15\n(14 for seasons pre-playoff, i.e. 2014 or earlier)')
  }
  if(season_type != 'regular'){
    # Check if season_type is appropriate, if not regular
    assertthat::assert_that(season_type %in% c('postseason','both'),
                msg = 'Enter valid season_type: regular, postseason, or both')
  }
  if(!is.null(team)){
    if(team == "San Jose State"){
      team = utils::URLencode(paste0("San Jos","\u00e9", " State"), reserved = TRUE)
    } else{
      # Encode team parameter for URL if not NULL
      team = utils::URLencode(team, reserved = TRUE)
    }
  }
  if(!is.null(home_team)){
    if(home_team == "San Jose State"){
      home_team = utils::URLencode(paste0("San Jos","\u00e9", " State"), reserved = TRUE)
    } else{
      # Encode home_team parameter for URL if not NULL
      home_team = utils::URLencode(home_team, reserved = TRUE)
    }
  }
  if(!is.null(away_team)){
    if(away_team == "San Jose State"){
      away_team = utils::URLencode(paste0("San Jos","\u00e9", " State"), reserved = TRUE)
    } else{
      # Encode team parameter for URL if not NULL
      away_team = utils::URLencode(away_team, reserved = TRUE)
    }
  }
  if(!is.null(conference)){
    # # Check conference parameter in conference abbreviations, if not NULL
    # assertthat::assert_that(conference %in% cfbscrapR::cfb_conf_types_df$abbreviation,
    #             msg = "Incorrect conference abbreviation, potential misspelling.\nConference abbreviations P5: ACC, B12, B1G, SEC, PAC\nConference abbreviations G5 and Independents: CUSA, MAC, MWC, Ind, SBC, AAC")
    # Encode conference parameter for URL, if not NULL
    conference = utils::URLencode(conference, reserved = TRUE)
  }
  if(!is.null(game_id)){
    # Check if game_id is numeric, if not NULL
    assertthat::assert_that(is.numeric(game_id),
                msg = 'Enter valid game_id (numeric value)')
  }

  base_url <- "https://api.collegefootballdata.com/games?"

  full_url <- paste0(base_url,
                     "year=", year,
                     "&week=", week,
                     "&seasonType=", season_type,
                     "&team=", team,
                     "&home=", home_team,
                     "&away=", away_team,
                     "&conference=", conference,
                     "&id=", game_id)

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
      df = jsonlite::fromJSON(full_url)
    
      if(!quarter_scores){
        df <- dplyr::select(df,-.data$home_line_scores,-.data$away_line_scores) %>% 
          dplyr::rename(game_id = .data$id) %>% 
          as.data.frame()
      } else{
        df <- df %>%
          tidyr::unnest_wider(.data$home_line_scores, names_sep = "_Q") %>%
          tidyr::unnest_wider(.data$away_line_scores, names_sep = "_Q")
        
        colnames(df) = gsub("_line_scores","_scores",colnames(df))
        df <- df %>% 
          dplyr::rename(game_id = .data$id) %>% 
          as.data.frame()
      }
      message(glue::glue("{Sys.time()}: Scraping game info data..."))
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments or no game info data available!"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )
  return(df)
}
