#' Get Season Statistics by Team
#'
#' @param year (\emph{Integer} required): Year, 4 digit format (\emph{YYYY})
#' @param season_type (\emph{String} default: regular): Select Season Type - regular, postseason, or both
#' @param team (\emph{String} optional): D-I Team
#' @param conference (\emph{String} optional): Conference abbreviation - Select a valid FBS conference\cr
#' Conference abbreviations P5: ACC, B12, B1G, SEC, PAC\cr
#' Conference abbreviations G5 and FBS Independents: CUSA, MAC, MWC, Ind, SBC, AAC\cr
#' @param start_week (\emph{Integer} optional): Starting Week - values range from 1-15, 1-14 for seasons pre-playoff, i.e. 2013 or earlier
#' @param end_week (\emph{Integer} optional): Ending Week - values range from 1-15, 1-14 for seasons pre-playoff, i.e. 2013 or earlier
#' 
#' @return A data frame with 43 variables:
#' \describe{
#'   \item{\code{games}}{integer.}
#'   \item{\code{time_of_poss_total}}{integer.}
#'   \item{\code{time_of_poss_pg}}{double.}
#'   \item{\code{pass_comps}}{integer.}
#'   \item{\code{pass_atts}}{integer.}
#'   \item{\code{completion_pct}}{double.}
#'   \item{\code{net_pass_yds}}{integer.}
#'   \item{\code{pass_ypa}}{double.}
#'   \item{\code{pass_ypr}}{double.}
#'   \item{\code{pass_TDs}}{integer.}
#'   \item{\code{interceptions}}{integer.}
#'   \item{\code{int_pct}}{double.}
#'   \item{\code{rush_atts}}{integer.}
#'   \item{\code{rush_yds}}{integer.}
#'   \item{\code{rush_TDs}}{integer.}
#'   \item{\code{rush_ypc}}{double.}
#'   \item{\code{total_yds}}{integer.}
#'   \item{\code{fumbles_lost}}{integer.}
#'   \item{\code{turnovers}}{integer.}
#'   \item{\code{turnovers_pg}}{double.}
#'   \item{\code{first_downs}}{integer.}
#'   \item{\code{third_downs}}{integer.}
#'   \item{\code{third_down_convs}}{integer.}
#'   \item{\code{third_conv_rate}}{double.}
#'   \item{\code{fourth_down_convs}}{integer.}
#'   \item{\code{fourth_downs}}{integer.}
#'   \item{\code{fourth_conv_rate}}{double.}
#'   \item{\code{penalties}}{integer.}
#'   \item{\code{penalty_yds}}{integer.}
#'   \item{\code{penalties_pg}}{double.}
#'   \item{\code{penalty_yds_pg}}{double.}
#'   \item{\code{yards_per_penalty}}{double.}
#'   \item{\code{kick_returns}}{integer.}
#'   \item{\code{kick_return_yds}}{integer.}
#'   \item{\code{kick_return_TDs}}{integer.}
#'   \item{\code{kick_return_avg}}{double.}
#'   \item{\code{punt_returns}}{integer.}
#'   \item{\code{punt_return_yds}}{integer.}
#'   \item{\code{punt_return_TDs}}{integer.}
#'   \item{\code{punt_return_avg}}{double.}
#'   \item{\code{passes_intercepted}}{integer.}
#'   \item{\code{passes_intercepted_yds}}{integer.}
#'   \item{\code{passes_intercepted_TDs}}{integer.}
#' }
#' @source \url{https://api.collegefootballdata.com/stats/season}
#' @keywords Team Season Stats
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET
#' @importFrom utils URLencode URLdecode
#' @importFrom assertthat assert_that
#' @importFrom glue glue
#' @importFrom dplyr select mutate rename
#' @importFrom tidyr pivot_wider
#' @export
#' @examples
#'
#' cfb_stats_season_team(year=2018, conference = 'B12', start_week = 1,end_week = 8)
#'
#' cfb_stats_season_team(2019, team = 'LSU')
#'
#' cfb_stats_season_team(2013, team = "Florida State")
#'

cfb_stats_season_team <- function(year,
                                  season_type = 'regular',
                                  team = NULL,
                                  conference = NULL,
                                  start_week = NULL,
                                  end_week = NULL) {

  # Check if year is numeric
  assertthat::assert_that(is.numeric(year) & nchar(year) == 4,
              msg='Enter valid year (Integer): 4-digit (YYYY)')
  if(season_type != 'regular'){
    # Check if season_type is appropriate, if not NULL
    assertthat::assert_that(season_type %in% c('postseason','both'),
                msg='Enter valid season_type (String): regular, postseason, or both')
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
    #             msg = "Incorrect conference abbreviation, potential misspelling.\nConference abbreviations P5: ACC, B12, B1G, SEC, PAC\nConference abbreviations G5 and Independents: CUSA, MAC, MWC, Ind, SBC, AAC")
    # Encode conference parameter for URL, if not NULL
    conference = utils::URLencode(conference, reserved = TRUE)
  }
  
  if(!is.null(start_week)){
    # Check if start_week is numeric, if not NULL
    assertthat::assert_that(is.numeric(start_week) & nchar(start_week) <= 2,
                msg='Enter valid start_week (Integer): 1-15\n(14 for seasons pre-playoff, i.e. 2014 or earlier)')
  }
  if(!is.null(end_week)){
    # Check if week is numeric, if not NULL
    assertthat::assert_that(is.numeric(end_week) & nchar(end_week) <= 2,
                msg='Enter valid end_week (Integer): 1-15\n(14 for seasons pre-playoff, i.e. 2014 or earlier)')
  }
  if(!is.null(start_week)&!is.null(end_week)){
    assertthat::assert_that(start_week<=end_week,
                msg='Enter valid start_week, end_week range')
  }



  base_url <- "https://api.collegefootballdata.com/stats/season?"

  full_url <- paste0(base_url,
                     "year=", year,
                     "&seasonType=", season_type,
                     "&startWeek=", start_week,
                     "&endWeek=", end_week,
                     "&team=", team,
                     "&conference=", conference)

  # Check for internet
  check_internet()

  # Create the GET request and set response as res
  res <- httr::GET(full_url)

  # Check the result
  check_status(res)
  
  df <- data.frame()
  
  #Expected column names for full season data
  expected_colnames <- c("season", "team", "conference", "passesIntercepted", "turnovers",           
                         "interceptionYards", "fumblesRecovered", "passCompletions", "rushingTDs", "puntReturnYards",      
                         "games", "fourthDowns", "puntReturns", "rushingYards", "totalYards",       
                         "kickReturnYards", "passingTDs", "rushingAttempts", "netPassingYards", "kickReturns",    
                         "possessionTime", "fourthDownConversions", "penalties", "puntReturnTDs", "firstDowns",           
                         "interceptionTDs", "penaltyYards", "passAttempts", "kickReturnTDs", "interceptions",
                         "thirdDownConversions", "thirdDowns", "fumblesLost")
  tryCatch(
    expr ={
      # Get the content and return result as data.frame
      df = jsonlite::fromJSON(full_url) 
      
      # Pivot category columns to get stats for each team game on one row
      df <- tidyr::pivot_wider(df,
                        names_from = .data$statName,
                        values_from = .data$statValue) 
      
      #Find missing columns, if any, and add them to found data
      missing <- setdiff(expected_colnames, colnames(df))
      df[missing] <- NA_real_
      
      df <- df %>%
        dplyr::mutate(
          time_of_poss_pg = ifelse(is.na("games"), NA_real_, .data$possessionTime/3600/.data$games),
          completion_pct = ifelse(is.na("passAttempts"), NA_real_ , .data$passCompletions/.data$passAttempts),
          pass_ypa = ifelse(is.na("passAttempts"), NA_real_, .data$netPassingYards/.data$passAttempts),
          pass_ypr = ifelse(is.na("passCompletions"), NA_real_, .data$netPassingYards/.data$passCompletions),
          int_pct = ifelse(is.na("passAttempts"), NA_real_, .data$interceptions/.data$passAttempts),
          rush_ypc = ifelse(is.na("rushingAttempts"), NA_real_, .data$rushingYards/.data$rushingAttempts),
          third_conv_rate = ifelse(is.na("thirdDowns"), NA_real_, .data$thirdDownConversions/.data$thirdDowns),
          fourth_conv_rate = ifelse(is.na("fourthDowns"), NA_real_, .data$fourthDownConversions/.data$fourthDowns),
          penalties_pg = ifelse(is.na("games"), NA_real_, .data$penalties/.data$games),
          penalty_yds_pg = ifelse(is.na("games"), NA_real_, .data$penaltyYards/.data$games),
          yards_per_penalty = ifelse(is.na("penalties"), NA_real_, .data$penaltyYards/.data$penalties),
          turnovers_pg = ifelse(is.na("games"), NA_real_, .data$turnovers/.data$games),
          kick_return_avg = ifelse(is.na("kickReturns"), NA_real_, .data$kickReturnYards/.data$kickReturns),
          punt_return_avg = ifelse(is.na("puntReturns"), NA_real_, .data$puntReturnYards/.data$puntReturns)) %>% 
        dplyr::select(
          .data$games, .data$possessionTime, .data$time_of_poss_pg,
          .data$passCompletions, .data$passAttempts, .data$completion_pct,
          .data$netPassingYards,.data$pass_ypa,.data$pass_ypr, 
          .data$passingTDs, .data$interceptions,.data$int_pct,
          .data$rushingAttempts, .data$rushingYards, .data$rushingTDs,
          .data$rush_ypc, .data$totalYards, 
          .data$fumblesLost, .data$turnovers,.data$turnovers_pg,
          .data$firstDowns, .data$thirdDowns, .data$thirdDownConversions,
          .data$third_conv_rate,.data$fourthDownConversions, 
          .data$fourthDowns,.data$fourth_conv_rate,
          .data$penalties, .data$penaltyYards,.data$penalties_pg,
          .data$penalty_yds_pg, .data$yards_per_penalty,
          .data$kickReturns, .data$kickReturnYards, 
          .data$kickReturnTDs,.data$kick_return_avg,
          .data$puntReturns, .data$puntReturnYards, 
          .data$puntReturnTDs,.data$punt_return_avg,
          .data$passesIntercepted, .data$interceptionYards, .data$interceptionTDs) %>% 
        dplyr::rename(
          time_of_poss_total=.data$possessionTime,
          pass_comps = .data$passCompletions,
          pass_atts = .data$passAttempts,
          net_pass_yds = .data$netPassingYards,
          pass_TDs = .data$passingTDs,
          rush_atts = .data$rushingAttempts,
          rush_yds = .data$rushingYards,
          rush_TDs = .data$rushingTDs,
          total_yds = .data$totalYards,
          fumbles_lost = .data$fumblesLost,
          first_downs = .data$firstDowns,
          third_downs = .data$thirdDowns,
          third_down_convs = .data$thirdDownConversions,
          fourth_downs = .data$fourthDowns,
          fourth_down_convs = .data$fourthDownConversions,
          penalty_yds = .data$penaltyYards,
          kick_returns = .data$kickReturns,
          kick_return_yds = .data$kickReturnYards,
          kick_return_TDs = .data$kickReturnTDs,
          punt_returns = .data$puntReturns,
          punt_return_yds = .data$puntReturnYards,
          punt_return_TDs = .data$puntReturnTDs,
          passes_intercepted = .data$passesIntercepted,
          passes_intercepted_yds = .data$interceptionYards,
          passes_intercepted_TDs = .data$interceptionTDs) %>% 
        as.data.frame()
      
      message(glue::glue("{Sys.time()}: Scraping season team stats..."))
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}:Invalid arguments or no season team stats data available!"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )
  return(df)
}
