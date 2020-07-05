#' Get team averages for Predicted Points Added (PPA)
#'
#' @param year (\emph{Integer} optional): Year, 4 digit format (\emph{YYYY})
#' @param team (\emph{String} optional): D-I Team
#' @param conference (\emph{String} optional): Conference name - select a valid FBS conference\cr
#' Conference names P5: ACC,  Big 12, Big Ten, SEC, Pac-12\cr
#' Conference names G5 and FBS Independents: Conference USA, Mid-American, Mountain West, FBS Independents, American Athletic\cr
#' @param excl_garbage_time (\emph{Logical} default FALSE): Select whether to exclude Garbage Time (TRUE or FALSE)
#' 
#' @keywords Teams Predicted Points 
#' @importFrom attempt "stop_if_all"
#' @importFrom jsonlite "fromJSON"
#' @importFrom httr "GET"
#' @importFrom utils "URLencode" 
#' @importFrom assertthat "assert_that"
#' @import dplyr
#' @import tidyr
#' @import purrr
#' @export
#' @examples
#' 
#' cfb_metrics_ppa_teams(year = 2019, team = 'TCU')
#'

cfb_metrics_ppa_teams <- function(year = 2019,
                                  team = NULL,
                                  conference = NULL,
                                  excl_garbage_time = FALSE){
  
  args <- list(year = year,
               team = team)
  
  # Check that at search_term input argument is not null
  stop_if_all(args, is.null,
              msg="You need to specify at least one of two arguments:\nyear as an integer 4 digit format (YYYY) or team (String) for a D-I team")
  
  if(!is.null(year)){
    ## check if year is numeric
    assert_that(is.numeric(year) & nchar(year)==4,
                msg='Enter valid year as integer in 4 digit format (YYYY)')
  }
  if(!is.null(team)){
    # Encode team parameter for URL if not NULL
    team = URLencode(team, reserved = TRUE)
  }
  if(!is.null(conference)){
    # Check conference parameter in conference names, if not NULL
    assert_that(conference %in% cfbscrapR::cfb_conf_types_df$name,
                msg = "Incorrect Conference name, potential misspelling.\nConference names P5: ACC,  Big 12, Big Ten, SEC, Pac-12\nConference Names G5 and Independents: Conference USA, Mid-American, Mountain West, FBS Independents, American Athletic")
    # Encode conference parameter for URL, if not NULL
    conference = URLencode(conference, reserved = TRUE)
  }
  if(excl_garbage_time != FALSE){
    # Check if excl_garbage_time is TRUE, if not FALSE
    assert_that(excl_garbage_time == TRUE,
                msg='Enter valid excl_garbage_time value (Logical) - TRUE or FALSE')
  }

  base_url <- "https://api.collegefootballdata.com/ppa/teams?"

  full_url <- paste0(base_url,
                     "year=", year,
                     "&team=", team,
                     "&conference=", conference,
                     "&excludeGarbageTime=", excl_garbage_time)

  # Check for internet
  check_internet()

  # Create the GET request and set response as res
  res <- GET(full_url)

  # Check the result
  check_status(res)

  # Get the content, flatten and return result as data.frame
  df = fromJSON(full_url,flatten = TRUE) 
  colnames(df) = gsub("offense.", "off_", colnames(df))
  colnames(df) = gsub("defense.", "def_", colnames(df))
  colnames(df) = gsub("cumulative.", "cumulative_", colnames(df))
  colnames(df) = gsub("Down", "_down", colnames(df))
  
  return(df)
}
