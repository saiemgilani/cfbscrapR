#' Get S&P+ historical rating data
#'
#' At least one of \strong{year} or \strong{team} must be specified for the function to run
#'
#' @param year (\emph{Integer} optional): Year, 4 digit format (\emph{YYYY})
#' @param team (\emph{String} optional): D-I Team
#'
#' @keywords SP+
#' @importFrom attempt "stop_if_all"
#' @importFrom jsonlite "fromJSON"
#' @importFrom httr "GET"
#' @importFrom utils "URLencode"
#' @importFrom assertthat "assert_that"
#' @export
#' @examples
#'
#' cfb_ratings_sp(year = 2019)
#'
#' cfb_ratings_sp(team = 'Texas A&M')
#'
#' cfb_ratings_sp(year= 2019, team = "Texas")
#'


cfb_ratings_sp <- function(year = NULL, team = NULL){
  args <- list(year = year,
               team = team)

  # Check that at least one argument is not null
  stop_if_all(args, is.null,
              msg="You need to specify at least one of two arguments:\n year, as a number (YYYY), or team")

  if(!is.null(year)){
    # check if year is numeric and correct length
    assert_that(is.numeric(year) & nchar(year) == 4,
                msg='Enter valid year as a number in 4 digit format (YYYY)')
  }
  if(!is.null(team)){
    # Encode team parameter for URL, if not NULL
    team = URLencode(team, reserved = TRUE)
  }

  base_url = "https://api.collegefootballdata.com/ratings/sp"
  full_url = paste0(base_url,
                    "?year=",year,
                    '&team=',team)

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

