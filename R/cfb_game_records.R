#' Get Team records by year
#'
#' @param year (\emph{Integer} optional): Year, 4 digit format (\emph{YYYY})
#' @param team (\emph{String} optional): Team - Select a valid team, D1 football
#' @param conference (\emph{String} optional): Conference name - select a valid FBS conference
#' Conference names P5: ACC,  Big 12, Big Ten, SEC, Pac-12
#' Conference names G5 and FBS Independents: Conference USA, Mid-American, Mountain West, FBS Independents, American Athletic
#'
#' @keywords Team Info
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET
#' @importFrom utils "URLencode"
#' @importFrom assertthat "assert_that"
#' @import dplyr
#' @import tidyr
#' @export
#' @examples
#'
#' cfb_game_records(2018, team='Notre Dame')
#'
#'
#' cfb_game_records(2013, team = "Florida State")
#'

cfb_game_records <- function(year, team = NULL, conference = NULL) {


  ## check if year is numeric
  assert_that(is.numeric(year) & nchar(year) == 4,
              msg='Enter valid year (Integer): 4 digits (YYYY)')

  if(!is.null(team)){
    team = URLencode(team, reserved = TRUE)
  }
  if(!is.null(conference)){
    # Check conference parameter in conference names, if not NULL
    assert_that(conference %in% cfbpointsR::cfb_conf_types_df$name,
                msg = "Incorrect Conference Name, potential misspelling.\nConference Names P5: ACC,  Big 12, Big Ten, SEC, Pac-12\nConference Names G5 and Independents: Conference USA, Mid-American, Mountain West, FBS Independents, American Athletic")
    conference = URLencode(conference, reserved = TRUE)
  }

  base_url <- "https://api.collegefootballdata.com/records?"

  full_url <- paste0(base_url,
                     "year=", year,
                     "&team=", team,
                     "&conference=", conference)

  # Check for internet
  check_internet()

  # Create the GET request and set response as res
  res <- GET(full_url)

  # Check the result
  check_status(res)

  # Get the content and return it as data.frame
  df = fromJSON(full_url,flatten=TRUE)%>%
    rename(
      total_games = .data$total.games,
      total_wins = .data$total.wins,
      total_losses = .data$total.losses,
      total_ties = .data$total.ties,
      conference_games = .data$conferenceGames.games,
      conference_wins = .data$conferenceGames.wins,
      conference_losses = .data$conferenceGames.losses,
      conference_ties = .data$conferenceGames.ties,
      home_games = .data$homeGames.games,
      home_wins = .data$homeGames.wins,
      home_losses = .data$homeGames.losses,
      home_ties = .data$homeGames.ties,
      away_games = .data$awayGames.games,
      away_wins = .data$awayGames.wins,
      away_losses = .data$awayGames.losses,
      away_ties = .data$awayGames.ties
    )

  return(df)

}
