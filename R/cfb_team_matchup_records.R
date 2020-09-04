#' Get matchup history between two teams.
#'
#' @param team1 (\emph{String} required): D-I Team 1
#' @param team2 (\emph{String} required): D-I Team 2
#' @param min_year (\emph{Integer} optional): Minimum of year range, 4 digit format (\emph{YYYY})
#' @param max_year (\emph{Integer} optional): Maximum of year range, 4 digit format (\emph{YYYY})
#'
#' @keywords Team Matchup Records
#' @importFrom attempt "stop_if_any"
#' @importFrom jsonlite "fromJSON"
#' @importFrom httr "GET"
#' @importFrom utils "URLencode"
#' @importFrom assertthat "assert_that"
#' @import dplyr
#' @import tidyr
#' @export
#' @examples
#'
#' cfb_team_matchup_records('Texas','Oklahoma')
#'
#' cfb_team_matchup_records('Texas A&M','TCU', min_year = 1975)
#'
#'

cfb_team_matchup_records <- function(team1, team2, min_year = NULL, max_year = NULL) {

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
    assert_that(is.numeric(max_year) & nchar(max_year) == 4,
                msg='Enter valid max_year as a number (YYYY)')
  }

  # Encode team1 parameter for URL
  team1 = utils::URLencode(team1, reserved = TRUE)

  # Encode team2 parameter for URL
  team2 = utils::URLencode(team2, reserved = TRUE)

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

  # Get the content and return it as data.frame
  df = jsonlite::fromJSON(full_url)
  df1<- enframe(unlist(df,use.names = TRUE))[1:7,]
  df <- pivot_wider(df1,
                    names_from = .data$name,
                    values_from = .data$value) %>% 
    dplyr::rename(start_year = .data$startYear,
           end_year = .data$endYear,
           team1_wins = .data$team1Wins,
           team2_wins = .data$team2Wins) %>% 
    dplyr::select(.data$start_year,.data$end_year,
           .data$team1,.data$team1_wins,
           .data$team2,.data$team2_wins,
           .data$ties)
  df <- as.data.frame(df)
  return(df)
}
