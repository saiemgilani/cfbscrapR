#' Get matchup history between two teams.
#'
#' @param team1 (\emph{String} required): D-I Team 1
#' @param team2 (\emph{String} required): D-I Team 2
#' @param min_year (\emph{Integer} optional): Minimum of year range, 4 digit format (\emph{YYYY})
#' @param max_year (\emph{Integer} optional): Maximum of year range, 4 digit format (\emph{YYYY})
#' 
#' @return A data frame with 11 variables:
#' \describe{
#'   \item{\code{season}}{integer.}
#'   \item{\code{week}}{integer.}
#'   \item{\code{season_type}}{character.}
#'   \item{\code{date}}{character.}
#'   \item{\code{neutral_site}}{logical.}
#'   \item{\code{venue}}{character.}
#'   \item{\code{home_team}}{character.}
#'   \item{\code{home_score}}{integer.}
#'   \item{\code{away_team}}{character.}
#'   \item{\code{away_score}}{integer.}
#'   \item{\code{winner}}{character.}
#' }
#' @source \url{https://api.collegefootballdata.com/teams/matchup}
#' @keywords Team Matchup
#' @importFrom attempt "stop_if_any"
#' @importFrom jsonlite "fromJSON"
#' @importFrom httr "GET"
#' @importFrom utils "URLencode"
#' @importFrom assertthat "assert_that"
#' @importFrom janitor "clean_names"
#' @importFrom glue "glue"
#' @export
#' @examples
#'
#' cfb_team_matchup('Texas','Oklahoma')
#'
#' cfb_team_matchup('Texas A&M','TCU')
#'
#' cfb_team_matchup('Texas A&M','TCU', min_year = 1975)
#'
#' cfb_team_matchup('Florida State', 'Florida', min_year = 1975)
#'

cfb_team_matchup <- function(team1, team2, min_year = NULL, max_year = NULL) {

  args <- list(team1 = team1, team2 = team2)

  # Check that any of the required arguments are not NULL
  stop_if_any(args, is.null,
              msg="You need to specify both arguments team1 and team2 in the cfb_team_matchup function call")

  if(!is.null(min_year)){
    # Check if min_year is numeric, if not NULL
    assertthat::assert_that(is.numeric(min_year) & nchar(min_year) == 4,
                msg='Enter valid min_year as a number (YYYY)')
  }
  if(!is.null(max_year)){
    # Check if max_year is numeric, if not NULL
    assertthat::assert_that(is.numeric(max_year) & nchar(max_year) == 4,
                msg='Enter valid max_year as a number (YYYY)')
  }

  if(!is.null(team1)){
    if(team1 == "San Jose State"){
      team1 = utils::URLencode(paste0("San Jos","\u00e9", " State"), reserved = TRUE)
    } else{
      # Encode team1 parameter for URL if not NULL
      team1 = utils::URLencode(team1, reserved = TRUE)
    }
  }
  if(!is.null(team1)){
    if(team2 == "San Jose State"){
      team2 = utils::URLencode(paste0("San Jos","\u00e9", " State"), reserved = TRUE)
    } else{
      # Encode team2 parameter for URL if not NULL
      team2 = utils::URLencode(team2, reserved = TRUE)
    }
  }
  
  base_url <- "https://api.collegefootballdata.com/teams/matchup?"

  full_url <-paste0(base_url,
                    "team1=", team1,
                    "&team2=", team2,
                    "&minYear=", min_year,
                    "&maxYear=", max_year)

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
      df = jsonlite::fromJSON(full_url)$games
      if(nrow(df)==0){
        warning("The data pulled from the API was empty.")
        return(NULL)
      }
      df <- df %>% 
        janitor::clean_names() %>% 
        as.data.frame()
      
      message(glue::glue("{Sys.time()}: Scraping team matchup..."))
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}:Invalid arguments or no team matchup data available!"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )
  return(df)
}
