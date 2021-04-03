#' Get Game media information (TV, radio, etc)
#'
#' @param year (*Integer* required): Year, 4 digit format (*YYYY*)
#' @param week (*Integer* optional): Week, values from 1-15, 1-14 for seasons pre-playoff (i.e. 2013 or earlier)
#' @param season_type (*String* default both): Select Season Type, regular, postseason, or both
#' @param team (*String* optional): D-I Team
#' @param conference (*String* optional): Conference abbreviation - Select a valid FBS conference\cr
#' Conference abbreviations P5: ACC, B12, B1G, SEC, PAC\cr
#' Conference abbreviations G5 and FBS Independents: CUSA, MAC, MWC, Ind, SBC, AAC\cr
#' @param media_type (*String* optional): Media type filter: tv, radio, web, ppv, or mobile
#' 
#' @return A data frame with 13 variables:
#' \describe{
#'   \item{`game_id`}{integer.}
#'   \item{`season`}{integer.}
#'   \item{`week`}{integer.}
#'   \item{`season_type`}{character.}
#'   \item{`start_time`}{character.}
#'   \item{`is_start_time_tbd`}{logical.}
#'   \item{`home_team`}{character.}
#'   \item{`home_conference`}{character.}
#'   \item{`away_team`}{character.}
#'   \item{`away_conference`}{character.}
#'   \item{`tv`}{list.}
#'   \item{`radio`}{logical.}
#'   \item{`web`}{list.}
#' }
#' @source <https://api.collegefootballdata.com/games/media>
#' @keywords Game Info
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
#' cfb_game_media(2019, week = 4, conference = 'ACC')
#'

cfb_game_media <- function(year,
                           week = NULL,
                           season_type = 'both',
                           team = NULL,
                           conference = NULL,
                           media_type = NULL) {

  ## check if year is numeric
  assertthat::assert_that(is.numeric(year) & nchar(year) == 4,
              msg = 'Enter valid year as a number (YYYY)')
  if(!is.null(week)){
    assertthat::assert_that(is.numeric(week) & nchar(week) <= 2,
                msg = 'Enter valid week 1-15 \n(14 for seasons pre-playoff, i.e. 2014 or earlier)')
  }
  if(season_type != 'both'){
    # Check if season_type is appropriate, if not regular
    assertthat::assert_that(season_type %in% c('postseason','regular'),
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
  if(!is.null(conference)){
    # # Check conference parameter in conference abbreviations, if not NULL
    # assertthat::assert_that(conference %in% cfbscrapR::cfb_conf_types_df$abbreviation,
    #             msg = "Incorrect conference abbreviation, potential misspelling.\nConference abbreviations P5: ACC, B12, B1G, SEC, PAC\nConference abbreviations G5 and Independents: CUSA, MAC, MWC, Ind, SBC, AAC")
    # Encode conference parameter for URL, if not NULL
    conference = utils::URLencode(conference, reserved = TRUE)
  }

  base_url <- "https://api.collegefootballdata.com/games/media?"

  full_url <- paste0(base_url,
                     "year=", year,
                     "&week=", week,
                     "&seasonType=", season_type,
                     "&team=", team,
                     "&conference=", conference,
                     "&mediaType=", media_type)

  # Check for internet
  check_internet()

  # Create the GET request and set response as res
  res <- httr::GET(full_url)

  # Check the result
  check_status(res)
  
  cols <- c("game_id", "season", "week", "season_type", "start_time",
            "is_start_time_tbd", "home_team", "home_conference", "away_team",
            "away_conference","tv", "radio", "web")
  
  df <- data.frame()
  tryCatch(
    expr ={
      # Get the content and return it as data.frame
      df <- jsonlite::fromJSON(full_url) %>%
        pivot_wider(names_from = .data$mediaType,
                    values_from = .data$outlet,
                    values_fn = list) %>%
        janitor::clean_names() %>% 
        dplyr::rename(game_id = .data$id)
      
      df[cols[!(cols %in% colnames(df))]] = NA
      df <- df[!duplicated(df),]
      
      df <- df %>%
        dplyr::select(cols, dplyr::everything()) %>% 
        as.data.frame()
      
      message(glue::glue("{Sys.time()}: Scraping game media data..."))
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments or no game media data available!"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )
  return(df)

}
