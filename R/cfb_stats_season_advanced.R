#' Get Season Advanced Statistics by Team
#'
#' @param year (\emph{Integer} required): Year, 4 digit format (\emph{YYYY})
#' @param team (\emph{String} optional): D-I Team
#' @param excl_garbage_time (\emph{Logical} default FALSE): Select whether to exclude Garbage Time (TRUE/FALSE)
#' @param start_week (\emph{Integer} optional): Starting Week - values range from 1-15, 1-14 for seasons pre-playoff, i.e. 2013 or earlier
#' @param end_week (\emph{Integer} optional): Ending Week - values range from 1-15, 1-14 for seasons pre-playoff, i.e. 2013 or earlier
#' 
#' @return A data frame with 79 variables:
#' \describe{
#'   \item{\code{season}}{integer.}
#'   \item{\code{team}}{character.}
#'   \item{\code{conference}}{character.}
#'   \item{\code{off_plays}}{integer.}
#'   \item{\code{off_drives}}{integer.}
#'   \item{\code{off_ppa}}{double.}
#'   \item{\code{off_total_ppa}}{double.}
#'   \item{\code{off_success_rate}}{double.}
#'   \item{\code{off_explosiveness}}{double.}
#'   \item{\code{off_power_success}}{double.}
#'   \item{\code{off_stuff_rate}}{double.}
#'   \item{\code{off_line_yds}}{double.}
#'   \item{\code{off_line_yds_total}}{integer.}
#'   \item{\code{off_second_lvl_yds}}{double.}
#'   \item{\code{off_second_lvl_yds_total}}{integer.}
#'   \item{\code{off_open_field_yds}}{double.}
#'   \item{\code{off_open_field_yds_total}}{integer.}
#'   \item{\code{off_pts_per_opp}}{double.}
#'   \item{\code{off_field_pos_avg_start}}{double.}
#'   \item{\code{off_field_pos_avg_predicted_points}}{double.}
#'   \item{\code{off_havoc_total}}{double.}
#'   \item{\code{off_havoc_front_seven}}{double.}
#'   \item{\code{off_havoc_db}}{double.}
#'   \item{\code{off_standard_downs_rate}}{double.}
#'   \item{\code{off_standard_downs_ppa}}{double.}
#'   \item{\code{off_standard_downs_success_rate}}{double.}
#'   \item{\code{off_standard_downs_explosiveness}}{double.}
#'   \item{\code{off_passing_downs_rate}}{double.}
#'   \item{\code{off_passing_downs_ppa}}{double.}
#'   \item{\code{off_passing_downs_success_rate}}{double.}
#'   \item{\code{off_passing_downs_explosiveness}}{double.}
#'   \item{\code{off_rushing_plays_rate}}{double.}
#'   \item{\code{off_rushing_plays_ppa}}{double.}
#'   \item{\code{off_rushing_plays_total_ppa}}{double.}
#'   \item{\code{off_rushing_plays_success_rate}}{double.}
#'   \item{\code{off_rushing_plays_explosiveness}}{double.}
#'   \item{\code{off_passing_plays_rate}}{double.}
#'   \item{\code{off_passing_plays_ppa}}{double.}
#'   \item{\code{off_passing_plays_total_ppa}}{double.}
#'   \item{\code{off_passing_plays_success_rate}}{double.}
#'   \item{\code{off_passing_plays_explosiveness}}{double.}
#'   \item{\code{def_plays}}{integer.}
#'   \item{\code{def_drives}}{integer.}
#'   \item{\code{def_ppa}}{double.}
#'   \item{\code{def_total_ppa}}{double.}
#'   \item{\code{def_success_rate}}{double.}
#'   \item{\code{def_explosiveness}}{double.}
#'   \item{\code{def_power_success}}{double.}
#'   \item{\code{def_stuff_rate}}{double.}
#'   \item{\code{def_line_yds}}{double.}
#'   \item{\code{def_line_yds_total}}{integer.}
#'   \item{\code{def_second_lvl_yds}}{double.}
#'   \item{\code{def_second_lvl_yds_total}}{integer.}
#'   \item{\code{def_open_field_yds}}{double.}
#'   \item{\code{def_open_field_yds_total}}{integer.}
#'   \item{\code{def_pts_per_opp}}{double.}
#'   \item{\code{def_field_pos_avg_start}}{integer.}
#'   \item{\code{def_field_pos_avg_predicted_points}}{double.}
#'   \item{\code{def_havoc_total}}{double.}
#'   \item{\code{def_havoc_front_seven}}{double.}
#'   \item{\code{def_havoc_db}}{double.}
#'   \item{\code{def_standard_downs_rate}}{double.}
#'   \item{\code{def_standard_downs_ppa}}{double.}
#'   \item{\code{def_standard_downs_success_rate}}{double.}
#'   \item{\code{def_standard_downs_explosiveness}}{double.}
#'   \item{\code{def_passing_downs_rate}}{double.}
#'   \item{\code{def_passing_downs_ppa}}{double.}
#'   \item{\code{def_passing_downs_total_ppa}}{double.}
#'   \item{\code{def_passing_downs_success_rate}}{double.}
#'   \item{\code{def_passing_downs_explosiveness}}{double.}
#'   \item{\code{def_rushing_plays_rate}}{double.}
#'   \item{\code{def_rushing_plays_ppa}}{double.}
#'   \item{\code{def_rushing_plays_total_ppa}}{double.}
#'   \item{\code{def_rushing_plays_success_rate}}{double.}
#'   \item{\code{def_rushing_plays_explosiveness}}{double.}
#'   \item{\code{def_passing_plays_rate}}{double.}
#'   \item{\code{def_passing_plays_ppa}}{double.}
#'   \item{\code{def_passing_plays_success_rate}}{double.}
#'   \item{\code{def_passing_plays_explosiveness}}{double.}
#' }
#' @source \url{https://api.collegefootballdata.com/stats/season/advanced}
#' @keywords Team Season Advanced Stats
#' @importFrom jsonlite "fromJSON"
#' @importFrom httr "GET"
#' @importFrom utils "URLencode" "URLdecode"
#' @importFrom assertthat "assert_that"
#' @importFrom glue "glue"
#' @import dplyr
#' @export
#' @examples
#'
#' cfb_stats_season_advanced(2019, team = 'LSU')
#'

cfb_stats_season_advanced <- function(year,
                                      team = NULL,
                                      excl_garbage_time = FALSE,
                                      start_week = NULL,
                                      end_week = NULL) {

  # Check if year is numeric
  assertthat::assert_that(is.numeric(year) & nchar(year) == 4,
              msg = 'Enter valid year (Integer): 4-digit (YYYY)')
  if(!is.null(team)){
    # Encode team parameter for URL, if not NULL
    team = utils::URLencode(team, reserved = TRUE)
  }
  if(excl_garbage_time!=FALSE){
    # Check if excl_garbage_time is TRUE, if not FALSE
    assertthat::assert_that(excl_garbage_time==TRUE,
                msg = 'Enter valid excl_garbage_time value (Logical) - TRUE or FALSE')
  }
  if(!is.null(start_week)){
    # Check if start_week is numeric, if not NULL
    assertthat::assert_that(is.numeric(start_week) & nchar(start_week) <= 2,
                msg = 'Enter valid start_week (Integer): 1-15\n(14 for seasons pre-playoff, i.e. 2014 or earlier)')
  }
  if(!is.null(end_week)){
    # Check if week is numeric, if not NULL
    assertthat::assert_that(is.numeric(end_week) & nchar(end_week) <= 2,
                msg = 'Enter valid end_week (Integer): 1-15\n(14 for seasons pre-playoff, i.e. 2014 or earlier)')
  }
  if(!is.null(start_week)&!is.null(end_week)){
    assertthat::assert_that(start_week <= end_week,
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
  res <- httr::GET(full_url)

  # Check the result
  check_status(res)
  
  df <- data.frame()
  tryCatch(
    expr ={
      # Get the content and return result as data.frame
      df = jsonlite::fromJSON(full_url, flatten = TRUE) 
      colnames(df) = gsub("offense.", "off_", colnames(df))
      colnames(df) = gsub("defense.", "def_", colnames(df))
      colnames(df) = gsub("Rate", "_rate", colnames(df))
      colnames(df) = gsub("Total", "_total", colnames(df))
      colnames(df) = gsub("Downs", "_downs", colnames(df))
      colnames(df) = gsub("lineYards", "line_yds", colnames(df))
      colnames(df) = gsub("secondLevelYards", "second_lvl_yds", colnames(df))
      colnames(df) = gsub("openFieldYards", "open_field_yds", colnames(df))
      colnames(df) = gsub("Success", "_success", colnames(df))
      colnames(df) = gsub("fieldPosition", "field_pos", colnames(df))
      colnames(df) = gsub("pointsPerOpportunity", "pts_per_opp", colnames(df))
      colnames(df) = gsub("average", "avg_", colnames(df))
      colnames(df) = gsub("Plays", "_plays", colnames(df))
      colnames(df) = gsub("PPA", "_ppa", colnames(df))
      colnames(df) = gsub("PredictedPoints", "predicted_points", colnames(df))
      colnames(df) = gsub("Seven", "_seven", colnames(df))
      colnames(df) = gsub(".avg", "_avg", colnames(df))
      colnames(df) = gsub(".rate", "_rate", colnames(df))
      colnames(df) = gsub(".explosiveness", "_explosiveness", colnames(df))
      colnames(df) = gsub(".ppa", "_ppa", colnames(df))
      colnames(df) = gsub(".total", "_total", colnames(df))
      colnames(df) = gsub(".success", "_success", colnames(df))
      colnames(df) = gsub(".front", "_front", colnames(df))
      colnames(df) = gsub("_Start", "_start", colnames(df))
      colnames(df) = gsub(".db", "_db", colnames(df))
      
      df <- df %>% 
        as.data.frame()
      
      message(glue::glue("{Sys.time()}: Scraping season advanced stats..."))
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}:Invalid arguments or no season advanced stats data available!"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )
  return(df)
}
