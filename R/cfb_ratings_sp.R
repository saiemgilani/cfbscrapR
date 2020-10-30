#' Get S&P+ historical rating data
#'
#' At least one of \strong{year} or \strong{team} must be specified for the function to run
#'
#' @param year (\emph{Integer} optional): Year, 4 digit format (\emph{YYYY})
#' @param team (\emph{String} optional): D-I Team
#'
#' @return A data frame with 26 variables:
#' \describe{
#'   \item{\code{year}}{integer.}
#'   \item{\code{team}}{character.}
#'   \item{\code{conference}}{character.}
#'   \item{\code{rating}}{double.}
#'   \item{\code{second_order_wins}}{logical.}
#'   \item{\code{sos}}{logical.}
#'   \item{\code{offense_rating}}{double.}
#'   \item{\code{offense_success}}{logical.}
#'   \item{\code{offense_explosiveness}}{logical.}
#'   \item{\code{offense_rushing}}{logical.}
#'   \item{\code{offense_passing}}{logical.}
#'   \item{\code{offense_standard_downs}}{logical.}
#'   \item{\code{offense_passing_downs}}{logical.}
#'   \item{\code{offense_run_rate}}{logical.}
#'   \item{\code{offense_pace}}{logical.}
#'   \item{\code{defense_rating}}{double.}
#'   \item{\code{defense_success}}{logical.}
#'   \item{\code{defense_explosiveness}}{logical.}
#'   \item{\code{defense_rushing}}{logical.}
#'   \item{\code{defense_passing}}{logical.}
#'   \item{\code{defense_standard_downs}}{logical.}
#'   \item{\code{defense_passing_downs}}{logical.}
#'   \item{\code{defense_havoc_total}}{logical.}
#'   \item{\code{defense_havoc_front_seven}}{logical.}
#'   \item{\code{defense_havoc_db}}{logical.}
#'   \item{\code{special_teams_rating}}{double.}
#' }
#' @source \url{https://api.collegefootballdata.com/ratings/sp}
#' @keywords SP+
#' @importFrom attempt stop_if_all
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET
#' @importFrom utils URLencode
#' @importFrom assertthat assert_that
#' @importFrom glue glue
#' @importFrom dplyr rename
#' @export
#' @examples
#'
#' cfb_ratings_sp(year = 2019)
#'
#' cfb_ratings_sp(team = 'Texas A&M')
#'
#' cfb_ratings_sp(year= 2019, team = "Texas")
#'


cfb_ratings_sp <- function(year = NULL, team = NULL){
  args <- list(year = year,
               team = team)

  # Check that at least one argument is not null
  attempt::stop_if_all(args, is.null,
              msg = "You need to specify at least one of two arguments:\n year, as a number (YYYY), or team")

  if(!is.null(year)){
    # check if year is numeric and correct length
    assertthat::assert_that(is.numeric(year) & nchar(year) == 4,
                msg = 'Enter valid year as a number in 4 digit format (YYYY)')
  }
  if(!is.null(team)){
    if(team == "San Jose State"){
      team = utils::URLencode(paste0("San Jos","\u00e9", " State"), reserved = TRUE)
    } else{
      # Encode team parameter for URL if not NULL
      team = utils::URLencode(team, reserved = TRUE)
    }
  }

  base_url = "https://api.collegefootballdata.com/ratings/sp"
  full_url = paste0(base_url,
                    "?year=",year,
                    '&team=',team)

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
      df = jsonlite::fromJSON(full_url, flatten = TRUE) %>%
        dplyr::rename(
          second_order_wins = .data$secondOrderWins,
          offense_rating = .data$offense.rating,
          offense_success = .data$offense.success,
          offense_explosiveness = .data$offense.explosiveness,
          offense_rushing = .data$offense.rushing,
          offense_passing = .data$offense.passing,
          offense_standard_downs = .data$offense.standardDowns,
          offense_passing_downs = .data$offense.passingDowns,
          offense_run_rate = .data$offense.runRate,
          offense_pace = .data$offense.pace,
          defense_rating = .data$defense.rating,
          defense_success = .data$defense.success,
          defense_explosiveness = .data$defense.explosiveness,
          defense_rushing = .data$defense.rushing,
          defense_passing = .data$defense.passing,
          defense_standard_downs = .data$defense.standardDowns,
          defense_passing_downs = .data$defense.passingDowns,
          defense_havoc_total = .data$defense.havoc.total,
          defense_havoc_front_seven = .data$defense.havoc.frontSeven,
          defense_havoc_db = .data$defense.havoc.db,
          special_teams_rating = .data$specialTeams.rating) %>% 
        as.data.frame()
      
      message(glue::glue("{Sys.time()}: Scraping S&P+ ratings data..."))
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments or no S&P+ ratings data available!"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )
  return(df)
}

