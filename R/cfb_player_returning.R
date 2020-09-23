#' Player Information Search
#'
#' A player usage function with \strong{Year} as a required input.
#'
#' @param year (\emph{Integer} required, default 2019): Year, 4 digit format (\emph{YYYY}).
#' @param team (\emph{String} optional): Team - Select a valid team, D1 football
#' @param conference (\emph{String} optional): Conference abbreviation - Select a valid FBS conference\cr
#' Conference abbreviations P5: ACC, B12, B1G, SEC, PAC\cr
#' Conference abbreviations G5 and FBS Independents: CUSA, MAC, MWC, Ind, SBC, AAC
#' @return A data frame with 15 variables:
#' \describe{
#'   \item{\code{season}}{integer.}
#'   \item{\code{team}}{character.}
#'   \item{\code{conference}}{character.}
#'   \item{\code{total_ppa}}{double.}
#'   \item{\code{total_passing_ppa}}{double.}
#'   \item{\code{total_receiving_ppa}}{double.}
#'   \item{\code{total_rushing_ppa}}{double.}
#'   \item{\code{percent_ppa}}{double.}
#'   \item{\code{percent_passing_ppa}}{double.}
#'   \item{\code{percent_receiving_ppa}}{double.}
#'   \item{\code{percent_rushing_ppa}}{double.}
#'   \item{\code{usage}}{double.}
#'   \item{\code{passing_usage}}{double.}
#'   \item{\code{receiving_usage}}{double.}
#'   \item{\code{rushing_usage}}{double.}
#' }
#' @source \url{https://api.collegefootballdata.com/player/returning}
#' @keywords Returning Production
#' @importFrom attempt "stop_if_all"
#' @importFrom jsonlite "fromJSON"
#' @importFrom httr "GET"
#' @importFrom utils "URLencode"
#' @importFrom assertthat "assert_that"
#' @importFrom glue "glue"
#' @import dplyr
#' @import tidyr
#' @export
#' @examples
#' 
#' cfb_player_returning(year = 2019, team = 'Florida State')
#'

cfb_player_returning <- function(year = 2019,
                                 team = NULL,
                                 conference = NULL){

  args <- list(year = year)

  # Check that at search_term input argument is not null
  attempt::stop_if_all(args, is.null,
              msg = "You need to specify at least one argument:\nyear as an integer 4 digit format (YYYY)")


  if(!is.null(year)){
    ## check if year is numeric
    assertthat::assert_that(is.numeric(year) & nchar(year)==4,
                msg = 'Enter valid year as integer in 4 digit format (YYYY)\n Min: 2000, Max: 2020')
  }
  if(!is.null(team)){
    # Encode team parameter for URL if not NULL
    team = utils::URLencode(team, reserved = TRUE)
  }
  if(!is.null(conference)){
    # # Check conference parameter in conference abbreviations, if not NULL
    # assertthat::assert_that(conference %in% cfbscrapR::cfb_conf_types_df$abbreviation,
    #             msg = "Incorrect conference abbreviation, potential misspelling.\nConference abbreviations P5: ACC, B12, B1G, SEC, PAC\nConference abbreviations G5 and Independents: CUSA, MAC, MWC, Ind, SBC, AAC")
    # Encode conference parameter for URL, if not NULL
    conference = utils::URLencode(conference, reserved = TRUE)
  }

  base_url = "https://api.collegefootballdata.com/player/returning?"

  # Create full url using base and input arguments
  full_url = paste0(base_url,
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
      df = jsonlite::fromJSON(full_url) %>% 
        dplyr::rename(
          total_ppa = .data$totalPPA,
          total_passing_ppa = .data$totalPassingPPA,
          total_receiving_ppa = .data$totalReceivingPPA,
          total_rushing_ppa = .data$totalRushingPPA,
          percent_ppa = .data$percentPPA,
          percent_passing_ppa = .data$percentPassingPPA,
          percent_receiving_ppa = .data$percentReceivingPPA,
          percent_rushing_ppa = .data$percentRushingPPA,
          passing_usage = .data$passingUsage,
          receiving_usage = .data$receivingUsage,
          rushing_usage = .data$rushingUsage) %>% 
        as.data.frame()
      
      message(glue::glue("{Sys.time()}: Scraping returning player data..."))
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments or no returning player data available!"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )   
  return(df)
}
