#' Gets player info associated by play
#'
#' Information describes the players involved in the play
#' this includes passer, receiver, defensive players who
#' create sacks or picks, etc
#'
#' @param year (\emph{Integer} optional): Year, 4 digit format (\emph{YYYY})
#' @param week (\emph{Integer} optional): Week - values from 1-15, 1-14 for seasons pre-playoff, i.e. 2013 or earlier
#' @param team (\emph{String} optional): D-I Team
#' @param game_id (\emph{Integer} optional): Game ID filter for querying a single game
#' Can be found using the `cfb_game_info()` function
#' @param athlete_id (\emph{Integer} optional): Athlete ID filter for querying a single athlete
#' Can be found using the `cfb_player_info()` function.
#' @param stat_type_id (\emph{Integer} optional): Stat Type ID filter for querying a single stat type
#' Can be found using the `cfb_play_stats_types()` function
#' @param season_type (\emph{String} default regular): Select Season Type: regular, postseason, or both
#'
#' @keywords Player - PBP
#' @importFrom jsonlite "fromJSON"
#' @importFrom httr "GET"
#' @importFrom utils "URLencode"
#' @importFrom assertthat "assert_that"
#' @importFrom tidyr "pivot_wider"
#' @importFrom tidyr "unite"
#' @import dplyr
#' @import tidyr
#' @import purrr
#' @export
#' 



cfb_play_stats_player <- function(year = NULL,
                                  week = NULL,
                                  team = NULL,
                                  game_id = NULL,
                                  athlete_id = NULL,
                                  stat_type_id = NULL,
                                  season_type = 'regular'){

  if(!is.null(year)){
    # Check if year is numeric, if not NULL
    assert_that(is.numeric(year) & nchar(year) == 4,
                msg='Enter valid year (Integer): 4-digit (YYYY)')
  }
  if(!is.null(week)){
    # Check if week is numeric, if not NULL
    assert_that(is.numeric(week) & nchar(week) <= 2 & week <= 15,
                msg='Enter valid week (Integer): 1-15\n(14 for seasons pre-playoff, i.e. 2014 or earlier)')
  }
  if(!is.null(team)){
    # Encode team parameter for URL, if not NULL
    team = URLencode(team, reserved = TRUE)
  }
  if(!is.null(game_id)){
    # Check if game_id is numeric, if not NULL
    assert_that(is.numeric(game_id),
                msg='Enter valid game_id value (Integer)\nCan be found using the `cfb_game_info()` function')
  }
  if(!is.null(athlete_id)){
    # Check if athlete_id is numeric, if not NULL
    assert_that(is.numeric(athlete_id),
                msg='Enter valid athlete_id value (Integer)\nCan be found using the `cfb_player_info()` function')
  }
  if(!is.null(stat_type_id)){
    # Check if stat_type_id is numeric, if not NULL
    assert_that(is.numeric(stat_type_id),
                msg='Enter valid stat_type_id value (Integer)\nCan be found using the `cfb_play_stat_types()` function')
  }
  if(season_type != 'regular'){
    # Check if season_type is appropriate, if not NULL
    assert_that(season_type %in% c('postseason','both'),
                msg='Enter valid season_type (String): regular, postseason, or both')
  }

  base_url <- "https://api.collegefootballdata.com/play/stats?"

  full_url = paste0(base_url,
                    "year=", year,
                    "&week=", week,
                    "&team=", team,
                    "&gameId=", game_id,
                    "&athleteID=", athlete_id,
                    "&statTypeId=", stat_type_id,
                    "&seasonType=", season_type)

  # Check for internet
  check_internet()

  # Create the GET request and set response as res
  res <- GET(full_url)

  # Check the result
  check_status(res)

  # Get the content and return it as data.frame
  df = fromJSON(full_url)

  if(length(df)==0){
    warning(paste0('There is no data in the underlying API call ', full_url))
    return(NA)
  }

  df = df[!duplicated(df),]

  clean_df <- pivot_wider(df,
                          names_from = .data$statType,
                          values_from = .data$athleteName) %>%
              arrange(.data$week,.data$period,-.data$secondsRemaining)

  clean_df <- as.data.frame(clean_df)
  return(clean_df)
}
