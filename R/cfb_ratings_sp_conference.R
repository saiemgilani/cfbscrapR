#' Get conference-level S&P+ historical rating data
#'
#' @param year (\emph{Integer} optional): Year, 4 digit format (\emph{YYYY})
#' @param conference (\emph{String} optional): Conference abbreviation - S&P+ information by conference\cr
#' Conference abbreviations P5: ACC, B12, B1G, SEC, PAC\cr
#' Conference abbreviations G5 and FBS Independents: CUSA, MAC, MWC, Ind, SBC, AAC\cr
#'
#' @keywords SP+
#' @importFrom attempt "stop_if_all"
#' @importFrom jsonlite "fromJSON"
#' @importFrom httr "GET"
#' @importFrom utils "URLencode"
#' @importFrom assertthat "assert_that"
#' @export
#'
#' @examples
#'
#' cfb_ratings_sp_conference(year = 2019)
#'
#' cfb_ratings_sp_conference(year = 2012, conference = 'SEC')
#'
#' cfb_ratings_sp_conference(year = 2016, conference = 'ACC')
#'

cfb_ratings_sp_conference <- function(year = NULL, conference = NULL){

  args <- list(year = year,
               conference = conference)

  # Check that at least one argument is not null
  stop_if_all(args, is.null,
              msg="You need to specify at least one of two arguments:\n year, as a number (YYYY), or conference\nConference abbreviations P5: ACC, B12, B1G, SEC, PAC\nConference abbreviations G5 and Independents: CUSA, MAC, MWC, Ind, SBC, AAC")

  if(!is.null(year)){
    # check if year is numeric and correct length
    assert_that(is.numeric(year) & nchar(year) == 4,
                msg='Enter valid year as a number in 4 digit format (YYYY)')
  }
  if(!is.null(conference)){
    # Check conference parameter in conference abbreviations, if not NULL
    assert_that(conference %in% cfbscrapR::cfb_conf_types_df$abbreviation,
                msg = "Incorrect conference abbreviation, potential misspelling.\nConference abbreviations P5: ACC, B12, B1G, SEC, PAC\nConference abbreviations G5 and Independents: CUSA, MAC, MWC, Ind, SBC, AAC")
    # Encode conference parameter for URL, if not NULL
    conference = URLencode(conference, reserved = TRUE)
  }
  base_url = 'https://api.collegefootballdata.com/ratings/sp/conferences'

  full_url = paste0(base_url,
                    "?year=",year,
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
