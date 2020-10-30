#' CFB Recruiting Information - Team Rankings
#'
#' Gets CFB team recruiting ranks with filters available for year and team.
#' At least one of \strong{year} or \strong{team} must be specified for the function to run
#'
#' If you would like CFB recruiting information for players, please
#' see the \code{\link[cfbscrapR:cfb_recruiting_player]{cfbscrapR::cfb_recruiting_player()}} function
#'
#' If you would like to get CFB recruiting information based on position groups during a
#' time period for all FBS teams, please see the \code{\link[cfbscrapR:cfb_recruiting_position]{cfbscrapR::cfb_recruiting_position()}} function.
#'
#' @param year (\emph{Integer} optional): Recruiting Class Year, 4 digit format (\emph{YYYY}). \emph{Note: 2000 is the minimum value}
#' @param team (\emph{String} optional): Team - Select a valid team, D1 football
#'
#' @return A data frame with 4 variables:
#' \describe{
#'   \item{\code{year}}{integer.}
#'   \item{\code{rank}}{integer.}
#'   \item{\code{team}}{character.}
#'   \item{\code{points}}{character.}
#' }
#' @source \url{https://api.collegefootballdata.com/recruiting/teams}
#' @keywords Recruiting
#' @importFrom attempt stop_if_all
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET
#' @importFrom utils URLencode
#' @importFrom assertthat assert_that
#' @importFrom glue glue
#' @export
#' @examples
#'
#' cfb_recruiting_team(2018, team = "Texas")
#'
#' cfb_recruiting_team(2016, team = "Virginia")
#'
#' cfb_recruiting_team(2016, team = "Texas A&M")
#'
#' cfb_recruiting_team(2011)
#'

cfb_recruiting_team <- function(year = NULL,
                                team = NULL){

  args <- list(year = year, team = team)

  # Check that at least one argument is not null
  attempt::stop_if_all(args, is.null,
              msg = 'You need to specify at least one argument: \nyear, as integer in 4 digit format (YYYY) - Min: 2000, Max: 2020\n or team')

  if(!is.null(year)){
    ## check if year is numeric
    assertthat::assert_that(is.numeric(year) & nchar(year)==4,
                msg = 'Enter valid year as integer in 4 digit format (YYYY)\n Min: 2000, Max: 2020')
  }
  if(!is.null(team)){
    if(team == "San Jose State"){
      team = utils::URLencode(paste0("San Jos","\u00e9", " State"), reserved = TRUE)
    } else{
      # Encode team parameter for URL if not NULL
      team = utils::URLencode(team, reserved = TRUE)
    }
  }

  base_url = "https://api.collegefootballdata.com/recruiting/teams?"

  # Create full url using base and input arguments
  full_url = paste0(base_url,
                    "year=", year,
                    "&team=", team)

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
        as.data.frame()
      
      message(glue::glue("{Sys.time()}: Scraping team recruiting data..."))
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments or no team recruiting data available!"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )      
  return(df)
}
