#' Team Info Lookup
#' Lists all teams in conference or all D-I teams if conference is left NULL
#' Current support only for D-I
#'
#' @param conference (\emph{String} optional): Conference abbreviation - Select a valid FBS conference
#' Conference abbreviations P5: ACC, B12, B1G, SEC, PAC,
#' Conference abbreviations G5 and FBS Independents: CUSA, MAC, MWC, Ind, SBC, AAC
#' @param only_fbs (\emph{Logical} default TRUE): Filter for only returning FBS teams for a given year.
#' If year is left blank while only_fbs is TRUE, then will return values for most current year
#' @param year (\emph{Integer} optional): Year, 4 digit format (\emph{YYYY}). Filter for getting a list of major division team for a given year
#'
#' @keywords Teams
#' @importFrom jsonlite "fromJSON"
#' @importFrom httr "GET"
#' @importFrom utils "URLencode"
#' @importFrom assertthat "assert_that"
#' @export
#' @examples
#'
#' cfb_team_info(conference = "SEC")
#'
#' cfb_team_info(conference = "Ind")
#'
#' cfb_team_info(year = 2019)
#'

cfb_team_info <- function(conference = NULL, only_fbs = TRUE, year = NULL) {

  if(!is.null(conference)){
    # Check conference parameter in conference abbreviations, if not NULL
    assert_that(conference %in% cfbscrapR::cfb_conf_types_df$abbreviation,
                msg = "Incorrect conference abbreviation, potential misspelling.\nConference abbreviations P5: ACC, B12, B1G, SEC, PAC\nConference abbreviations G5 and Independents: CUSA, MAC, MWC, Ind, SBC, AAC")
    # Encode conference parameter for URL, if not NULL
    conference = URLencode(conference, reserved = TRUE)

    base_url <-"https://api.collegefootballdata.com/teams?"

    full_url <- paste0(base_url,
                       "conference=",  conference)
    # Check for internet
    check_internet()

    # Create the GET request and set response as res
    res <- GET(full_url)

    # Check the result
    check_status(res)

    # Get the content and return it as data.frame
    df = fromJSON(full_url)

    return(df)
  }else{

    if(!is.null(year)){
      # Check if year is numeric, if not NULL
      assert_that(is.numeric(year) & nchar(year) == 4,
                  msg='Enter valid year as a number (YYYY)')
    }

    base_url <- "https://api.collegefootballdata.com/teams/fbs?"

    # if they want all fbs
    full_url = paste0(base_url,
                      "year=",year)

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
}
