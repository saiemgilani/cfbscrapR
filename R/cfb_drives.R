#' Get results information from games
#'
#' @param year (\emph{Integer} required): Year, 4 digit format (\emph{YYYY})
#' @param season_type (\emph{String} default regular): Select Season Type: regular, postseason, or both
#' @param week (\emph{Integer} optional): Week - values from 1-15, 1-14 for seasons pre-playoff, i.e. 2013 or earlier
#' @param team (\emph{String} optional): D-I Team
#' @param offense_team (\emph{String} optional): Offense D-I Team
#' @param defense_team (\emph{String} optional): Defense D-I Team
#' @param conference (\emph{String} optional): Conference name - select a valid FBS conference\cr
#' Conference names P5: ACC,  Big 12, Big Ten, SEC, Pac-12\cr
#' Conference names G5 and FBS Independents: Conference USA, Mid-American, Mountain West, FBS Independents, American Athletic\cr
#' @param offense_conference (\emph{String} optional): Offense DI Conference name - select an appropriate conference\cr
#' Conference names P5: ACC,  Big 12, Big Ten, SEC, Pac-12\cr
#' Conference names G5 and FBS Independents: Conference USA, Mid-American, Mountain West, FBS Independents, American Athletic\cr
#' @param defense_conference (\emph{String} optional): Defense DI Conference name - select an appropriate conference\cr
#' Conference names P5: ACC,  Big 12, Big Ten, SEC, Pac-12\cr
#' Conference names G5 and FBS Independents: Conference USA, Mid-American, Mountain West, FBS Independents, American Athletic\cr
#'
#' @keywords Drives
#' @importFrom jsonlite "fromJSON"
#' @importFrom httr "GET"
#' @importFrom utils "URLencode"
#' @importFrom assertthat "assert_that"
#' @importFrom glue "glue"
#' @import dplyr
#' @import tidyr
#' @export
#' @examples
#'
#' cfb_drives(2018, week = 1, team = "TCU")
#'
#' cfb_drives(2018, team = "Texas A&M", defense_conference = 'SEC')
#'

cfb_drives <- function(year,
                       season_type = 'regular',
                       week = NULL,
                       team = NULL,
                       offense_team = NULL,
                       defense_team = NULL,
                       conference = NULL,
                       offense_conference = NULL,
                       defense_conference = NULL) {

  # Check if year is numeric
  assertthat::assert_that(is.numeric(year) & nchar(year) == 4,
              msg = 'Enter valid year as a number (YYYY)')

  if(season_type != 'regular'){
    # Check if season_type is appropriate, if not regular
    assertthat::assert_that(season_type %in% c('postseason','both'),
                msg = 'Enter valid season_type: regular, postseason, or both')
  }
  if(!is.null(week)){
    # Check if week is numeric, if not NULL
    assertthat::assert_that(is.numeric(week) & nchar(week) <= 2,
                msg = 'Enter valid week 1-15 \n(14 for seasons pre-playoff, i.e. 2014 or earlier)')
  }
  if(!is.null(team)){
    # Encode team parameter for URL, if not NULL
    team = utils::URLencode(team, reserved = TRUE)
  }
  if(!is.null(offense_team)){
    # Encode offense_team parameter for URL, if not NULL
    offense_team = utils::URLencode(offense_team, reserved = TRUE)
  }
  if(!is.null(defense_team)){
    # Encode defense_team parameter for URL, if not NULL
    defense_team = utils::URLencode(defense_team, reserved = TRUE)
  }
  if(!is.null(conference)){
    # Check conference parameter in conference names, if not NULL
    assertthat::assert_that(conference %in% cfbscrapR::cfb_conf_types_df$name,
                msg = "Incorrect conference name, potential misspelling.\nConference names P5: ACC,  Big 12, Big Ten, SEC, Pac-12\nConference names G5 and Independents: Conference USA, Mid-American, Mountain West, FBS Independents, American Athletic")
    # Encode conference parameter for URL, if not NULL
    conference = utils::URLencode(conference, reserved = TRUE)
  }
  if(!is.null(offense_conference)){
    # Check offense_conference parameter in conference names, if not NULL
    assertthat::assert_that(offense_conference %in% cfbscrapR::cfb_conf_types_df$name,
                msg = "Incorrect offense_conference name, potential misspelling.\nConference names P5: ACC,  Big 12, Big Ten, SEC, Pac-12\nConference names G5 and Independents: Conference USA, Mid-American, Mountain West, FBS Independents, American Athletic")
    # Encode offense_conference parameter for URL, if not NULL
    offense_conference = utils::URLencode(offense_conference, reserved = TRUE)
  }
  if(!is.null(defense_conference)){
    # Check defense_conference parameter in conference names, if not NULL
    assertthat::assert_that(defense_conference %in% cfbscrapR::cfb_conf_types_df$name,
                msg = "Incorrect defense_conference name, potential misspelling.\nConference names P5: ACC,  Big 12, Big Ten, SEC, Pac-12\nConference names G5 and Independents: Conference USA, Mid-American, Mountain West, FBS Independents, American Athletic")
    # Encode defense_conference parameter for URL, if not NULL
    defense_conference = utils::URLencode(defense_conference, reserved = TRUE)
  }

  base_url <- "https://api.collegefootballdata.com/drives?"

  full_url <- paste0(base_url,
                     "year=", year,
                     "&seasonType=", season_type,
                     "&week=", week,
                     "&team=", team,
                     "&offense=", offense_team,
                     "&defense=", defense_team,
                     "&conference=", conference,
                     "&offenseConference=", offense_conference,
                     "&defenseConference=", defense_conference)

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
        dplyr::rename(
          time_minutes_start = .data$start_time.minutes,
          time_seconds_start = .data$start_time.seconds,
          time_minutes_end = .data$end_time.minutes,
          time_seconds_end = .data$end_time.seconds,
          time_minutes_elapsed = .data$elapsed.minutes,
          time_seconds_elapsed = .data$elapsed.seconds
        ) %>%
       dplyr::mutate(
          time_minutes_elapsed = ifelse(is.na(.data$time_minutes_elapsed), 0, .data$time_minutes_elapsed),
          time_seconds_elapsed = ifelse(is.na(.data$time_seconds_elapsed), 0, .data$time_seconds_elapsed)
        )
      message(glue::glue("{Sys.time()}: Scraping drives data..."))
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments or no drives data available!"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )
  return(df)
}
