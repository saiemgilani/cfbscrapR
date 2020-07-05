#' Get Game media information (TV, radio, etc)
#'
#' @param year (\emph{Integer} required): Year, 4 digit format (\emph{YYYY})
#' @param week (\emph{Integer} optional): Week, values from 1-15, 1-14 for seasons pre-playoff (i.e. 2013 or earlier)
#' @param season_type (\emph{String} default both): Select Season Type, regular, postseason, or both
#' @param team (\emph{String} optional): D-I Team
#' @param conference (\emph{String} optional):  Conference Name - select a valid FBS conference\cr
#' Conference Names P5: ACC,  Big 12, Big Ten, SEC, Pac-12\cr
#' Conference Names G5 and FBS Independents: Conference USA, Mid-American, Mountain West, FBS Independents, American Athletic\cr
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
    # Check conference parameter in conference names, if not NULL
    assert_that(conference %in% cfbscrapR::cfb_conf_types_df$name,
                msg = "Incorrect conference name, potential misspelling.\nConference names P5: ACC,  Big 12, Big Ten, SEC, Pac-12\nConference names G5 and Independents: Conference USA, Mid-American, Mountain West, FBS Independents, American Athletic")
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
                values_fn = .data$list) %>%
    unnest_wider(.data$tv, names_sep = "_") %>%
    unnest_wider(.data$radio, names_sep = "_") %>%
    unnest_wider(.data$web, names_sep = "_")


  df <- df[!duplicated(df),]

  return(df)

}
