#' Get Season Statistics by Team
#'
#' @param year (\emph{Integer} required): Year, 4 digit format (\emph{YYYY})
#' @param season_type (\emph{String} default: regular): Select Season Type - regular, postseason, or both
#' @param team (\emph{String} optional): D-I Team
#' @param conference (\emph{String} optional): Conference abbreviation - Select a valid FBS conference
#' Conference abbreviations P5: ACC, B12, B1G, SEC, PAC
#' Conference abbreviations G5 and FBS Independents: CUSA, MAC, MWC, Ind, SBC, AAC
#' @param start_week (\emph{Integer} optional): Starting Week - values range from 1-15, 1-14 for seasons pre-playoff, i.e. 2013 or earlier
#' @param end_week (\emph{Integer} optional): Ending Week - values range from 1-15, 1-14 for seasons pre-playoff, i.e. 2013 or earlier
#'
#' @keywords Team Season Stats
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET
#' @importFrom utils "URLencode" "URLdecode"
#' @importFrom assertthat "assert_that"
#' @import dplyr
#' @import tidyr
#' @import purrr
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
  assert_that(is.numeric(year) & nchar(year) == 4,
              msg='Enter valid year (Integer): 4-digit (YYYY)')
  if(season_type != 'regular'){
    # Check if season_type is appropriate, if not NULL
    assert_that(season_type %in% c('postseason','both'),
                msg='Enter valid season_type (String): regular, postseason, or both')
  }
  if(!is.null(team)){
    # Encode team parameter for URL, if not NULL
    team = URLencode(team, reserved = TRUE)
  }
  if(!is.null(conference)){
    # Check conference parameter in conference abbreviations, if not NULL
    assert_that(conference %in% cfbscrapR::cfb_conf_types_df$abbreviation,
                msg = "Incorrect conference abbreviation, potential misspelling.\nConference abbreviations P5: ACC, B12, B1G, SEC, PAC\nConference abbreviations G5 and Independents: CUSA, MAC, MWC, Ind, SBC, AAC")
    # Encode conference parameter for URL, if not NULL
    conference = URLencode(conference, reserved = TRUE)
  }
  
  if(!is.null(start_week)){
    # Check if start_week is numeric, if not NULL
    assert_that(is.numeric(start_week) & nchar(start_week) <= 2,
                msg='Enter valid start_week (Integer): 1-15\n(14 for seasons pre-playoff, i.e. 2014 or earlier)')
  }
  if(!is.null(end_week)){
    # Check if week is numeric, if not NULL
    assert_that(is.numeric(end_week) & nchar(end_week) <= 2,
                msg='Enter valid end_week (Integer): 1-15\n(14 for seasons pre-playoff, i.e. 2014 or earlier)')
  }
  if(!is.null(start_week)&!is.null(end_week)){
    assert_that(start_week<=end_week,
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
  res <- GET(full_url)

  # Check the result
  check_status(res)

  # Get the content and return result as data.frame
  df = fromJSON(full_url) 
  
  # Pivot category columns to get stats for each team game on one row
  df <- pivot_wider(df,
                    names_from = .data$statName,
                    values_from = .data$statValue) %>% 
    mutate(time_of_poss_pg = .data$possessionTime/3600/.data$games,
           completion_pct = .data$passCompletions/.data$passAttempts,
           pass_ypa = .data$netPassingYards/.data$passAttempts,
           pass_ypr = .data$netPassingYards/.data$passCompletions,
           int_pct = .data$interceptions/.data$passAttempts,
           rush_ypc = .data$rushingYards/.data$rushingAttempts,
           third_conv_rate = .data$thirdDownConversions/.data$thirdDowns,
           fourth_conv_rate = .data$fourthDownConversions/.data$fourthDowns,
           penalties_pg = .data$penalties/.data$games,
           penalty_yds_pg = .data$penaltyYards/.data$games,
           yards_per_penalty = .data$penaltyYards/.data$penalties,
           turnovers_pg = .data$turnovers/.data$games,
           kick_return_avg = .data$kickReturnYards/.data$kickReturns,
           punt_return_avg = .data$puntReturnYards/.data$puntReturns,
           ) %>% 
    select(.data$games, .data$possessionTime, .data$time_of_poss_pg,
           .data$passCompletions, .data$passAttempts, .data$completion_pct,
           .data$netPassingYards,.data$pass_ypa,.data$pass_ypr, 
           .data$passingTDs, .data$interceptions,.data$int_pct,
           .data$rushingAttempts, .data$rushingYards, .data$rushingTDs,
           .data$rush_ypc, .data$totalYards, 
           .data$fumblesLost, .data$turnovers,.data$turnovers_pg,
           .data$firstDowns, 
           .data$thirdDowns, .data$thirdDownConversions,.data$third_conv_rate,
           .data$fourthDownConversions, .data$fourthDowns,.data$fourth_conv_rate,
           .data$penalties, .data$penaltyYards,.data$penalties_pg,
           .data$penalty_yds_pg, .data$yards_per_penalty,
           .data$kickReturns, .data$kickReturnYards, 
           .data$kickReturnTDs,.data$kick_return_avg,
           .data$puntReturns, .data$puntReturnYards, 
           .data$puntReturnTDs,.data$punt_return_avg,
           .data$passesIntercepted, .data$interceptionYards, .data$interceptionTDs) %>% 
      rename(time_of_poss_total=.data$possessionTime,
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
             passes_intercepted_TDs = .data$interceptionTDs
             )
  df <- as.data.frame(df)
  return(df)
}
