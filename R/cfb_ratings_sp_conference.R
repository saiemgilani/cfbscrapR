#' Get conference-level S&P+ historical rating data
#'
#' @param year (\emph{Integer} optional): Year, 4 digit format (\emph{YYYY})
#' @param conference (\emph{String} optional): Conference abbreviation - S&P+ information by conference\cr
#' Conference abbreviations P5: ACC, B12, B1G, SEC, PAC\cr
#' Conference abbreviations G5 and FBS Independents: CUSA, MAC, MWC, Ind, SBC, AAC\cr
#'
#' @return A data frame with 25 variables:
#' \describe{
#'   \item{\code{year}}{integer.}
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
#' @source \url{https://api.collegefootballdata.com/ratings/sp/conferences}
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
#' cfb_ratings_sp_conference(year = 2019)
#'
#' cfb_ratings_sp_conference(year = 2012, conference = 'SEC')
#'
#' cfb_ratings_sp_conference(year = 2016, conference = 'ACC')
#'

cfb_ratings_sp_conference <- function(year = NULL, conference = NULL){

  args <- list(year = year,
               conference = conference)

  # Check that at least one argument is not null
  attempt::stop_if_all(args, is.null,
              msg = "You need to specify at least one of two arguments:\n year, as a number (YYYY), or conference\nConference abbreviations P5: ACC, B12, B1G, SEC, PAC\nConference abbreviations G5 and Independents: CUSA, MAC, MWC, Ind, SBC, AAC")

  if(!is.null(year)){
    # check if year is numeric and correct length
    assertthat::assert_that(is.numeric(year) & nchar(year) == 4,
                msg = 'Enter valid year as a number in 4 digit format (YYYY)')
  }
  if(!is.null(conference)){
    # # Check conference parameter in conference abbreviations, if not NULL
    # assertthat::assert_that(conference %in% cfbscrapR::cfb_conf_types_df$abbreviation,
    #             msg = "Incorrect conference abbreviation, potential misspelling.\nConference abbreviations P5: ACC, B12, B1G, SEC, PAC\nConference abbreviations G5 and Independents: CUSA, MAC, MWC, Ind, SBC, AAC")
    # Encode conference parameter for URL, if not NULL
    conference = utils::URLencode(conference, reserved = TRUE)
  }
  base_url = 'https://api.collegefootballdata.com/ratings/sp/conferences?'

  full_url = paste0(base_url,
                    "year=",year,
                    "&conference=",conference)

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
      df = jsonlite::fromJSON(full_url, flatten=TRUE) %>% 
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
      
      message(glue::glue("{Sys.time()}: Scraping conference-level S&P+ ratings data..."))
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments or no conference-level S&P+ ratings data available!"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )
  return(df)
}
