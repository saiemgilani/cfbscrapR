#' Team Roster
#' Get a teams full roster by year. If team is not selected, API returns rosters for every team from the selected year.
#'
#' @param year (*Integer* required): Year,  4 digit format (*YYYY*)
#' @param team (*String* optional): Team, select a valid team in D-I football
#'
#'
#' @return A data frame with 12 variables:
#' \describe{
#'   \item{`athlete_id`}{character.}
#'   \item{`first_name`}{character.}
#'   \item{`last_name`}{character.}
#'   \item{`weight`}{integer.}
#'   \item{`height`}{integer.}
#'   \item{`jersey`}{integer.}
#'   \item{`year`}{integer.}
#'   \item{`position`}{character.}
#'   \item{`home_city`}{character.}
#'   \item{`home_state`}{character.}
#'   \item{`home_country`}{character.}
#'   \item{`team`}{character.}
#' }
#' @source <https://api.collegefootballdata.com/roster>
#' @keywords Team Roster
#' @importFrom dplyr rename mutate
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET
#' @importFrom utils URLencode
#' @importFrom assertthat assert_that
#' @importFrom glue glue
#' @export
#' @examples
#'
#' cfb_team_roster(year = 2013, team = "Florida State")
#'

cfb_team_roster <- function(year, team = NULL){
  team2 <- team
  
  # check if year is numeric
  assert_that(is.numeric(year) & nchar(year) == 4,
              msg='Enter valid year as a number (YYYY)')
  

  if(!is.null(team)){
    if(team == "San Jose State"){
      team = utils::URLencode(paste0("San Jos","\u00e9", " State"), reserved = TRUE)
    } else{
      # Encode team1 parameter for URL if not NULL
      team = utils::URLencode(team, reserved = TRUE)
    }
  }
  base_url <- "https://api.collegefootballdata.com/roster?"

  if(is.null(team)) {
    full_url <- paste0(base_url,
                       "year=", year)
  } else {
    full_url <- paste0(base_url,"team=", team,
                       "&year=", year)
  }

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
      df = jsonlite::fromJSON(full_url) %>%
        dplyr::rename(athlete_id = .data$id) %>%
        # Is this okay to just comment out?
        # Changing to team = NULL deleted the column
        #dplyr::mutate(team = team2) %>%
        as.data.frame()

      message(glue::glue("{Sys.time()}: Scraping team roster..."))
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}:Invalid arguments or no team roster data available!"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )
  return(df)
}

