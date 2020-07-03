#' Get Game Advanced Stats
#'
#' @param year (\emph{Integer} required): Year, 4 digit format(\emph{YYYY})
#' @param week (\emph{Integer} optional): Week - values from 1-15, 1-14 for seasons pre-playoff (i.e. 2013 or earlier)
#' @param team (\emph{String} optional): D-I Team
#' @param opponent (\emph{String} optional): Opponent D-I Team
#' @param excl_garbage_time (\emph{Logical} default FALSE): Select whether to exclude Garbage Time (TRUE/FALSE)
#' @param season_type (\emph{String} default both): Select Season Type: regular, postseason, or both.
#'
#' @keywords Game Advanced Stats
#' @importFrom jsonlite "fromJSON"
#' @importFrom httr "GET"
#' @importFrom utils "URLencode" "URLdecode"
#' @importFrom assertthat "assert_that"
#' @import dplyr
#' @import tidyr
#' @import purrr
#' @export
#' @examples
#'
#' cfb_stats_game_advanced(year = 2018, week = 12, team = 'Texas A&M')
#'
#' cfb_stats_game_advanced(2019, team = 'LSU')
#'
#' cfb_stats_game_advanced(2013, team = "Florida State")
#'

cfb_stats_game_advanced <- function(year,
                                    week = NULL,
                                    team = NULL,
                                    opponent = NULL,
                                    excl_garbage_time = FALSE,
                                    season_type = 'both') {

  # Check if year is numeric
  assert_that(is.numeric(year) & nchar(year) == 4,
              msg='Enter valid year (Integer): 4-digit (YYYY)')
  if(!is.null(week)){
    # Check if week is numeric, if not NULL
    assert_that(is.numeric(week) & nchar(week) <= 2,
                msg='Enter valid week (Integer): 1-15\n(14 for seasons pre-playoff, i.e. 2014 or earlier)')
  }
  if(!is.null(team)){
    # Encode team parameter for URL, if not NULL
    team = URLencode(team, reserved = TRUE)
  }
  if(!is.null(opponent)){
    # Encode opponent parameter for URL, if not NULL
    opponent = URLencode(opponent, reserved = TRUE)
  }
  if(excl_garbage_time!=FALSE){
    # Check if excl_garbage_time is TRUE, if not FALSE
    assert_that(excl_garbage_time==TRUE,
                msg='Enter valid excl_garbage_time value (Logical) - TRUE or FALSE')
  }

  if(season_type != 'both'){
    # Check if season_type is appropriate, if not regular
    assert_that(season_type %in% c('postseason','both'),
                msg='Enter valid season_type (String): regular, postseason, or both')
  }



  base_url <- "https://api.collegefootballdata.com/stats/game/advanced?"

  full_url <- paste0(base_url,
                     "year=", year,
                     "&week=", week,
                     "&team=", team,
                     "&opponent=", opponent,
                     "&excludeGarbageTime=", excl_garbage_time,
                     "&seasonType=", season_type)

  # Check for internet
  check_internet()

  # Create the GET request and set response as res
  res <- GET(full_url)

  # Check the result
  check_status(res)

  # Get the content, flatten and return result as data.frame
  df = fromJSON(full_url,flatten=TRUE) 
  
  # Column renaming for the 76 returned columns
  colnames(df) = gsub("offense.","off_",colnames(df))
  colnames(df) = gsub("defense.","def_",colnames(df))
  colnames(df) = gsub("Rate","_rate",colnames(df))
  colnames(df) = gsub("Total","_total",colnames(df))
  colnames(df) = gsub("Downs","_downs",colnames(df))
  colnames(df) = gsub("lineYards","line_yds",colnames(df))
  colnames(df) = gsub("secondLevelYards","second_lvl_yds",colnames(df))
  colnames(df) = gsub("openFieldYards","open_field_yds",colnames(df))
  colnames(df) = gsub("Success","_success",colnames(df))
  colnames(df) = gsub("fieldPosition","field_pos",colnames(df))
  colnames(df) = gsub("pointsPerOpportunity","pts_per_opp",colnames(df))
  colnames(df) = gsub("average","avg_",colnames(df))
  colnames(df) = gsub("Plays","_plays",colnames(df))
  colnames(df) = gsub("PPA","_ppa",colnames(df))
  colnames(df) = gsub("PredictedPoints","predicted_points",colnames(df))
  colnames(df) = gsub("Seven","_seven",colnames(df))
  colnames(df) = gsub(".avg","_avg",colnames(df))
  colnames(df) = gsub(".rate","_rate",colnames(df))
  colnames(df) = gsub(".explosiveness","_explosiveness",colnames(df))
  colnames(df) = gsub(".ppa","_ppa",colnames(df))
  colnames(df) = gsub(".total","_total",colnames(df))
  colnames(df) = gsub(".success","_success",colnames(df))
  colnames(df) = gsub(".front","_front",colnames(df))
  colnames(df) = gsub("_Start","_start",colnames(df))
  colnames(df) = gsub(".db","_db",colnames(df))
  colnames(df) = gsub('Id','_id',colnames(df))
  
  return(df)
}
