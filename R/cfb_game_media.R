#' Get Game media information (TV, radio, etc)
#'
#' @param year (\emph{Integer} required): Year, 4 digit format (\emph{YYYY})
#' @param week (\emph{Integer} optional): Week, values from 1-15, 1-14 for seasons pre-playoff (i.e. 2013 or earlier)
#' @param season_type (\emph{String} default both): Select Season Type, regular, postseason, or both
#' @param team (\emph{String} optional): D-I Team
#' @param conference (\emph{String} optional): Conference abbreviation - Select a valid FBS conference\cr
#' Conference abbreviations P5: ACC, B12, B1G, SEC, PAC\cr
#' Conference abbreviations G5 and FBS Independents: CUSA, MAC, MWC, Ind, SBC, AAC\cr
#' @param media_type (\emph{String} optional): Media type filter: tv, radio, web, ppv, or mobile
#'
#' @keywords Game Info
#' @importFrom jsonlite "fromJSON"
#' @importFrom httr "GET"
#' @importFrom utils "URLencode"
#' @importFrom assertthat "assert_that"
#' @import dplyr
#' @import tidyr
#' @export
#' @examples 
#' 
#' cfb_game_media(2019, week = 1, conference = 'ACC')
#'

cfb_game_media <- function(year,
                           week = NULL,
                           season_type = 'both',
                           team = NULL,
                           conference = NULL,
                           media_type = NULL) {

  ## check if year is numeric
  assert_that(is.numeric(year) & nchar(year) == 4,
              msg='Enter valid year as a number (YYYY)')
  if(!is.null(week)){
    assert_that(is.numeric(week) & nchar(week) <= 2,
                msg='Enter valid week 1-15 \n(14 for seasons pre-playoff, i.e. 2014 or earlier)')
  }
  if(!is.null(team)){
    team = URLencode(team, reserved = TRUE)
  }
  if(!is.null(conference)){
    # Check conference parameter in conference abbreviations, if not NULL
    assert_that(conference %in% cfbscrapR::cfb_conf_types_df$abbreviation,
                msg = "Incorrect conference abbreviation, potential misspelling.\nConference abbreviations P5: ACC, B12, B1G, SEC, PAC\nConference abbreviations G5 and Independents: CUSA, MAC, MWC, Ind, SBC, AAC")
    # Encode conference parameter for URL, if not NULL
    conference = URLencode(conference, reserved = TRUE)
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
  res <- GET(full_url)

  # Check the result
  check_status(res)

  # Get the content and return it as data.frame
  df = fromJSON(full_url)

  df <- df %>%
    pivot_wider(names_from = .data$mediaType,
                values_from = .data$outlet,
                values_fn = .data$list) 

  df <- df[!duplicated(df),]

  return(df)

}
