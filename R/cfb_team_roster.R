#' Team Roster
#' Get a teams full roster by year. If year not selected, API defaults to most recent year (2020 as of 9/22/20)
#' If team is not selected, API returns rosters for every team from the selected year.
#'
#' @param year (\emph{Integer} optional): Year,  4 digit format (\emph{YYYY})
#' @param team (\emph{String} optional): Team, select a valid team in D-I football
#'
#'
#' @return A data frame with 12 variables:
#' \describe{
#'   \item{\code{athlete_id}}{character.}
#'   \item{\code{first_name}}{character.}
#'   \item{\code{last_name}}{character.}
#'   \item{\code{weight}}{integer.}
#'   \item{\code{height}}{integer.}
#'   \item{\code{jersey}}{integer.}
#'   \item{\code{year}}{integer.}
#'   \item{\code{position}}{character.}
#'   \item{\code{home_city}}{character.}
#'   \item{\code{home_state}}{character.}
#'   \item{\code{home_country}}{character.}
#'   \item{\code{team}}{character.}
#' }
#' @source \url{https://api.collegefootballdata.com/roster}
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
#' cfb_team_roster(team = "Florida State")
#'

cfb_team_roster <- function(year = NULL,team = NULL){
  team2 <- team
  if(!is.null(year)){
    # check if year is numeric
    assert_that(is.numeric(year) & nchar(year) == 4,
                msg='Enter valid year as a number (YYYY)')
  }

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

