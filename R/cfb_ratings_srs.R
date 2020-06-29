#' Get SRS historical rating data
#'
#' At least one of \strong{year} or \strong{team} must be specified for the function to run
#'
#' @param year (\emph{Integer} optional): Year, 4 digit format (\emph{YYYY})
#' @param team (\emph{String} optional): D-I Team
#' @param conference (\emph{String} optional): Conference name - select a valid FBS conference
#' Conference names P5: ACC,  Big 12, Big Ten, SEC, Pac-12
#' Conference names G5 and FBS Independents: Conference USA, Mid-American, Mountain West, FBS Independents, American Athletic
#'
#' @keywords SRS
#' @import dplyr
#' @import tidyr
#' @importFrom jsonlite "fromJSON"
#' @importFrom assertthat "assert_that"
#' @importFrom utils "URLencode"
#' @export
#' @examples
#'
#' cfb_ratings_srs(year = 2019, team = "Texas")
#'
#' cfb_ratings_srs(year = 2018, conference = 'SEC')
#'


cfb_ratings_srs <- function(year=NULL,team=NULL,conference=NULL){

  args <- list(year = year,
               team = team)

  # Check that at least one argument is not null
  stop_if_all(args, is.null,
              msg="You need to specify at least one of two arguments:\nyear, as a number (YYYY), or team")

  if(!is.null(year)){
    # check if year is numeric
    assert_that(is.numeric(year) & nchar(year) == 4,
                msg='Enter valid year as a number (YYYY)')
  }
  if(!is.null(team)){
    # Encode team parameter for URL, if not NULL
    team = URLencode(team, reserved = TRUE)
  }
  if(!is.null(conference)){
    # Check conference parameter in conference names, if not NULL
    assert_that(conference %in% cfbpointsR::cfb_conf_types_df$name,
                msg = "Incorrect Conference name, potential misspelling.\nConference names P5: ACC,  Big 12, Big Ten, SEC, Pac-12\nConference Names G5 and Independents: Conference USA, Mid-American, Mountain West, FBS Independents, American Athletic")
    # Encode conference parameter for URL, if not NULL
    conference = URLencode(conference, reserved = TRUE)
  }

  base_url = 'https://api.collegefootballdata.com/ratings/srs?'

  full_url = paste0(base_url,
                    "year=",year,
                    "&team=",team,
                    "&conference=",conference)

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

