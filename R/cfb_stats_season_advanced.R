#' Get Season Advanced Statistics by Team
#'
#' @param year (\emph{Integer} required): Year, 4 digit format (\emph{YYYY})
#' @param team (\emph{String} optional): D-I Team
#' @param excl_garbage_time (\emph{Logical} default FALSE): Select whether to exclude Garbage Time (TRUE/FALSE)
#' @param start_week (\emph{Integer} optional): Starting Week - values range from 1-15, 1-14 for seasons pre-playoff, i.e. 2013 or earlier
#' @param end_week (\emph{Integer} optional): Ending Week - values range from 1-15, 1-14 for seasons pre-playoff, i.e. 2013 or earlier
#'
#' @keywords Team Season Advanced Stats
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
#' cfb_stats_season_advanced(2019, team = 'LSU')
#'
#' cfb_stats_season_advanced(2013, team = 'Florida State')
#'

cfb_stats_season_advanced <- function(year,
                                      team = NULL,
                                      excl_garbage_time = FALSE,
                                      start_week = NULL,
                                      end_week = NULL) {

  # Check if year is numeric
  assert_that(is.numeric(year) & nchar(year) == 4,
              msg='Enter valid year (Integer): 4-digit (YYYY)')
  if(!is.null(team)){
    # Encode team parameter for URL, if not NULL
    team = URLencode(team, reserved = TRUE)
  }
  if(excl_garbage_time!=FALSE){
    # Check if excl_garbage_time is TRUE, if not FALSE
    assert_that(excl_garbage_time==TRUE,
                msg='Enter valid excl_garbage_time value (Logical) - TRUE or FALSE')
  }
  if(!is.null(start_week)){
    # Check if start_week is numeric, if not NULL
    assert_that(is.numeric(start_week) & nchar(start_week) <= 2,
                msg='Enter valid start_week (Integer): 1-15\n(14 for seasons pre-playoff, i.e. 2014 or earlier)')
  }
  if(!is.null(end_week)){
    # Check if end_week is numeric, if not NULL
    assert_that(is.numeric(end_week) & nchar(end_week) <= 2,
                msg='Enter valid end_week (Integer): 1-15\n(14 for seasons pre-playoff, i.e. 2014 or earlier)')
  }
  if(!is.null(start_week)&!is.null(end_week)){
    assert_that(start_week <= end_week,
                msg='Enter valid start_week, end_week range')
  }

  base_url <- "https://api.collegefootballdata.com/stats/season/advanced?"

  full_url <- paste0(base_url,
                     "year=", year,
                     "&team=", team,
                     "&excludeGarbageTime=", excl_garbage_time,
                     "&startWeek=", start_week,
                     "&endWeek=", end_week)

  # Check for internet
  check_internet()

  # Create the GET request and set response as res
  res <- GET(full_url)

  # Check the result
  check_status(res)

  # Get the content and return result as data.frame
  df = fromJSON(full_url,flatten=TRUE) 
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
  
  return(df)
}
