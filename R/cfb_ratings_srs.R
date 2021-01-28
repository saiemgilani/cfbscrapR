#' Get SRS historical rating data
#'
#' At least one of \strong{year} or \strong{team} must be specified for the function to run
#'
#' @param year (\emph{Integer} optional): Year, 4 digit format (\emph{YYYY})
#' @param team (\emph{String} optional): D-I Team
#' @param conference (\emph{String} optional): Conference abbreviation - SRS information by conference\cr
#' Conference abbreviations P5: ACC, B12, B1G, SEC, PAC\cr
#' Conference abbreviations G5 and FBS Independents: CUSA, MAC, MWC, Ind, SBC, AAC\cr
#'
#' @return A data frame with 6 variables:
#' \describe{
#'   \item{\code{year}}{integer.}
#'   \item{\code{team}}{character.}
#'   \item{\code{conference}}{character.}
#'   \item{\code{division}}{logical.}
#'   \item{\code{rating}}{numeric.}
#'   \item{\code{ranking}}{integer.}
#' }
#' @source \url{https://api.collegefootballdata.com/ratings/srs}
#' @keywords SRS
#' @importFrom attempt stop_if_all
#' @importFrom jsonlite fromJSON
#' @importFrom assertthat assert_that
#' @importFrom utils URLencode
#' @importFrom glue glue
#' @export
#' @examples
#'
#' cfb_ratings_srs(year = 2019, team = "Texas")
#'
#' cfb_ratings_srs(year = 2018, conference = 'SEC')
#'


cfb_ratings_srs <- function(year=NULL,team=NULL,conference=NULL){

  args <- list(year = year,
               team = team)

  # Check that at least one argument is not null
  attempt::stop_if_all(args, is.null,
              msg = "You need to specify at least one of two arguments:\nyear, as a number (YYYY), or team")

  if(!is.null(year)){
    # check if year is numeric
    assertthat::assert_that(is.numeric(year) & nchar(year) == 4,
                msg = 'Enter valid year as a number (YYYY)')
  }
  if(!is.null(team)){
    if(team == "San Jose State"){
      team = utils::URLencode(paste0("San Jos","\u00e9", " State"), reserved = TRUE)
    } else{
      # Encode team parameter for URL if not NULL
      team = utils::URLencode(team, reserved = TRUE)
    }
  }
  if(!is.null(conference)){
    # # Check conference parameter in conference abbreviations, if not NULL
    # assertthat::assert_that(conference %in% cfbscrapR::cfb_conf_types_df$abbreviation,
    #                         msg = "Incorrect conference abbreviation, potential misspelling.\nConference abbreviations P5: ACC, B12, B1G, SEC, PAC\nConference abbreviations G5 and Independents: CUSA, MAC, MWC, Ind, SBC, AAC")
    # Encode conference parameter for URL, if not NULL
    conference = utils::URLencode(conference, reserved = TRUE)
  }

  base_url = 'https://api.collegefootballdata.com/ratings/srs?'

  full_url = paste0(base_url,
                    "year=",year,
                    "&team=",team,
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
      df = jsonlite::fromJSON(full_url) %>%
        as.data.frame() %>%
        mutate(rating = as.numeric(.data$rating),
               ranking = as.integer(.data$ranking))

      message(glue::glue("{Sys.time()}: Scraping simple rating system (SRS) data..."))
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments or no simple rating system (SRS) data available!"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )
  return(df)
}

