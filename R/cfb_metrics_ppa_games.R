#' Get team game averages for Predicted Points Added (PPA)
#'
#' @param year (\emph{Integer} required): Year, 4 digit format (\emph{YYYY})
#' @param week (\emph{Integer} optional): Week - values range from 1-15, 1-14 for seasons pre-playoff, i.e. 2013 or earlier
#' @param team (\emph{String} optional): D-I Team
#' @param conference (\emph{String} optional): Conference abbreviation - Select a valid FBS conference\cr
#' Conference abbreviations P5: ACC, B12, B1G, SEC, PAC\cr
#' Conference abbreviations G5 and FBS Independents: CUSA, MAC, MWC, Ind, SBC, AAC\cr
#' @param excl_garbage_time (\emph{Logical} default FALSE): Select whether to exclude Garbage Time (TRUE or FALSE)
#' 
#' @keywords Teams Predicted Points 
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
#' cfb_metrics_ppa_games(year = 2019, team = 'TCU')
#'

cfb_metrics_ppa_games <- function(year,
                                  week = NULL,
                                  team = NULL,
                                  conference = NULL,
                                  excl_garbage_time = FALSE){
  
  args <- list(year = year)
  
  # Check that at search_term input argument is not null
  attempt::stop_if_all(args, is.null,
              msg = "You need to specify at least one argument:\nyear as an integer 4 digit format (YYYY)")
  
  ## check if year is numeric
  assertthat::assert_that(is.numeric(year) & nchar(year)==4,
              msg = 'Enter valid year as integer in 4 digit format (YYYY)')
  
  if(!is.null(week)){
    # Check if week is numeric, if not NULL
    assertthat::assert_that(is.numeric(week) & nchar(week) <= 2,
                msg = 'Enter valid week (Integer): 1-15\n(14 for seasons pre-playoff, i.e. 2014 or earlier)')
  }
  if(!is.null(team)){
    # Encode team parameter for URL if not NULL
    team = utils::URLencode(team, reserved = TRUE)
  }
  if(!is.null(conference)){
    # Check conference parameter in conference abbreviations, if not NULL
    assertthat::assert_that(conference %in% cfbscrapR::cfb_conf_types_df$abbreviation,
                            msg = "Incorrect conference abbreviation, potential misspelling.\nConference abbreviations P5: ACC, B12, B1G, SEC, PAC\nConference abbreviations G5 and Independents: CUSA, MAC, MWC, Ind, SBC, AAC")
    # Encode conference parameter for URL, if not NULL
    conference = utils::URLencode(conference, reserved = TRUE)
  }
  if(excl_garbage_time!=FALSE){
    # Check if excl_garbage_time is TRUE, if not FALSE
    assertthat::assert_that(excl_garbage_time==TRUE,
                msg = 'Enter valid excl_garbage_time value (Logical) - TRUE or FALSE')
  }
  
  base_url <- "https://api.collegefootballdata.com/ppa/games?"
  
  full_url <- paste0(base_url,
                     "year=", year,
                     "&week=", week,
                     "&team=", team,
                     "&conference=", conference,
                     "&excludeGarbageTime=", excl_garbage_time)
  
  # Check for internet
  check_internet()
  
  # Create the GET request and set response as res
  res <- httr::GET(full_url)
  
  # Check the result
  check_status(res)
  
  df <- data.frame()
  tryCatch(
    expr = {
      # Get the content, flatten and return result as data.frame
      df = jsonlite::fromJSON(full_url, flatten = TRUE) 
      colnames(df) = gsub("offense.", "off_", colnames(df))
      colnames(df) = gsub("defense.", "def_", colnames(df))
      colnames(df) = gsub("Down", "_down", colnames(df))
      
      df <- df %>% 
        dplyr::rename(game_id = .data$gameId) %>% 
        as.data.frame()
      
      message(glue::glue("{Sys.time()}: Scraping CFBData metrics PPA games data..."))
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments or no CFBData metrics PPA games data available!"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )    
  return(df)
}
