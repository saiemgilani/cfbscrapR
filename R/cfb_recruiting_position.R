#' CFB Recruiting Information - Position Groups
#'
#' If only start_year is provided, function will get CFB recruiting information based
#' on position groups during that year for all FBS teams.
#' 
#' If you would like CFB recruiting information for players, please
#' see the \code{\link[cfbscrapR:cfb_recruiting_player]{cfbscrapR::cfb_recruiting_player()}} function
#'  
#' If you would like CFB recruiting information for teams, please 
#' see the \code{\link[cfbscrapR:cfb_recruiting_team]{cfbscrapR::cfb_recruiting_team()}} function
#' 
#' @param start_year (\emph{Integer} optional): Start Year, 4 digit format (\emph{YYYY}). \emph{Note: 2000 is the minimum value}
#' @param end_year (\emph{Integer} optional): End Year,  4 digit format (\emph{YYYY}). \emph{Note: 2020 is the maximum value currently}
#' @param team (\emph{String} optional): Team - Select a valid team, D-I football
#' @param conference (\emph{String} optional): Conference name - select a valid FBS conference
#' Conference names P5: ACC,  Big 12, Big Ten, SEC, Pac-12
#' Conference names G5 and FBS Independents: Conference USA, Mid-American, Mountain West, FBS Independents, American Athletic
#'
#' @keywords Recruiting
#' @importFrom attempt "stop_if_all"
#' @importFrom jsonlite "fromJSON"
#' @importFrom httr "GET"
#' @importFrom utils "URLencode"
#' @importFrom assertthat "assert_that"
#' @importFrom glue "glue"
#' @import dplyr
#' @export
#' @examples
#'
#' cfb_recruiting_position(2018, team="Texas")
#'
#' cfb_recruiting_position(2016, 2020, team="Virginia")
#'
#' cfb_recruiting_position(2015, 2020, conference = "SEC")
#'

cfb_recruiting_position <- function(start_year = NULL, end_year = NULL,
                                    team = NULL, conference = NULL){

  if(!is.null(start_year)){
    # check if start_year is numeric
    assertthat::assert_that(is.numeric(start_year) & nchar(start_year) == 4,
                msg = 'Enter valid start_year as a number (YYYY) - Min: 2000, Max: 2020')
  }
  if(!is.null(end_year)){
    # check if end_year is numeric
    assertthat::assert_that(is.numeric(end_year) & nchar(end_year) == 4,
                msg = 'Enter valid end_year as a number (YYYY) - Min: 2000, Max: 2020')
  }
  if(!is.null(team)){
    # Encode team parameter for URL if not NULL
    team = utils::URLencode(team, reserved = TRUE)
  }
  if(!is.null(conference)){
    # Check conference parameter in conference names, if not NULL
    assertthat::assert_that(conference %in% cfbscrapR::cfb_conf_types_df$name,
                msg = "Incorrect conference name, potential misspelling.\nConference names P5: ACC,  Big 12, Big Ten, SEC, Pac-12\nConference names G5 and Independents: Conference USA, Mid-American, Mountain West, FBS Independents, American Athletic")
    # Encode conference parameter for URL, if not NULL
    conference = utils::URLencode(conference, reserved = TRUE)
  }

  base_url = "https://api.collegefootballdata.com/recruiting/groups?"

  # Create full url using base and input arguments
  full_url = paste0(base_url,
                    'startYear=',
                    start_year,
                    '&endYear=',
                    end_year,
                    "&team=",
                    team,
                    "&conference=",
                    conference)

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
      df = jsonlite::fromJSON(full_url)  %>%
        dplyr::rename(
          position_group = .data$positionGroup,
          avg_rating = .data$averageRating,
          total_rating = .data$totalRating,
          avg_stars = .data$averageStars) %>% 
        as.data.frame()
      
      message(glue::glue("{Sys.time()}: Scraping position group recruiting data..."))
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments or no position group recruiting data available!"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )      
  return(df)
}
