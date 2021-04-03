#' Get Team records by year
#'
#' @param year (*Integer* optional): Year, 4 digit format (*YYYY*)
#' @param team (*String* optional): Team - Select a valid team, D1 football
#' @param conference (*String* optional): DI Conference abbreviation - Select a valid FBS conference\cr
#' Conference abbreviations P5: ACC, B12, B1G, SEC, PAC\cr
#' Conference abbreviations G5 and FBS Independents: CUSA, MAC, MWC, Ind, SBC, AAC\cr
#' @return A data frame with 20 variables:
#' \describe{
#'   \item{`year`}{integer.}
#'   \item{`team`}{character.}
#'   \item{`conference`}{character.}
#'   \item{`division`}{character.}
#'   \item{`total_games`}{integer.}
#'   \item{`total_wins`}{integer.}
#'   \item{`total_losses`}{integer.}
#'   \item{`total_ties`}{integer.}
#'   \item{`conference_games`}{integer.}
#'   \item{`conference_wins`}{integer.}
#'   \item{`conference_losses`}{integer.}
#'   \item{`conference_ties`}{integer.}
#'   \item{`home_games`}{integer.}
#'   \item{`home_wins`}{integer.}
#'   \item{`home_losses`}{integer.}
#'   \item{`home_ties`}{integer.}
#'   \item{`away_games`}{integer.}
#'   \item{`away_wins`}{integer.}
#'   \item{`away_losses`}{integer.}
#'   \item{`away_ties`}{integer.}
#' }
#' @source <https://api.collegefootballdata.com/records>
#' @keywords Team Info
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET
#' @importFrom utils URLencode
#' @importFrom assertthat assert_that
#' @import dplyr
#' @import tidyr
#' @export
#' @examples
#'
#' cfb_game_records(2018, team = 'Notre Dame')
#'
#' cfb_game_records(2013, team = "Florida State")
#' 

cfb_game_records <- function(year, team = NULL, conference = NULL) {


  ## check if year is numeric
  assertthat::assert_that(is.numeric(year) & nchar(year) == 4,
              msg = 'Enter valid year (Integer): 4 digits (YYYY)')

  if(!is.null(team)){
    if(team == "San Jose State"){
      team = utils::URLencode(paste0("San Jos","\u00e9", " State"), reserved = TRUE)
    } else{
      # Encode team parameter for URL if not NULL
      team = utils::URLencode(team, reserved = TRUE)
    }
  }
  if(!is.null(conference)){
    # Check conference parameter in conference abbreviations, if not NULL
    # assertthat::assert_that(conference %in% cfbscrapR::cfb_conf_types_df$abbreviation,
    #                         msg = "Incorrect conference abbreviation, potential misspelling.\nConference abbreviations P5: ACC, B12, B1G, SEC, PAC\nConference abbreviations G5 and Independents: CUSA, MAC, MWC, Ind, SBC, AAC")
    # # Encode conference parameter for URL, if not NULL
    conference = utils::URLencode(conference, reserved = TRUE)
  }

  base_url <- "https://api.collegefootballdata.com/records?"

  full_url <- paste0(base_url,
                     "year=", year,
                     "&team=", team,
                     "&conference=", conference)

  # Check for internet
  check_internet()

  # Create the GET request and set response as res
  res <- httr::GET(full_url)

  # Check the result
  check_status(res)
  
  df <- data.frame()
  tryCatch(
    expr = {
      # Get the content and return it as data.frame
      df = jsonlite::fromJSON(full_url, flatten = TRUE)%>%
        dplyr::rename(
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
      message(glue::glue("{Sys.time()}: Scraping game records data..."))
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments or no game records data available!"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )
  return(df)

}
