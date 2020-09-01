#' Get Team Statistics by Game
#'
#' @param year (\emph{Integer} required): Year, 4 digit format (\emph{YYYY})
#' @param week (\emph{Integer} optional): Week - values range from 1-15, 1-14 for seasons pre-playoff, i.e. 2013 or earlier
#' @param season_type (\emph{String} default: regular): Select Season Type - regular, postseason, or both
#' @param team (\emph{String} optional): D-I Team
#' @param conference (\emph{String} optional): Conference abbreviation - Select a valid FBS conference\cr
#' Conference abbreviations P5: ACC, B12, B1G, SEC, PAC\cr
#' Conference abbreviations G5 and FBS Independents: CUSA, MAC, MWC, Ind, SBC, AAC\cr
#' @param game_id (\emph{Integer} optional): Game ID filter for querying a single game\cr
#' Can be found using the \code{\link[cfbscrapR:cfb_game_info]{cfbscrapR::cfb_game_info()}} function
#' @param rows_per_team (\emph{Integer} default 1): Both Teams for each game on one or two row(s), Options: 1 or 2
#'
#' @keywords Team Game Stats
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
#' cfb_game_team_stats(2019, team = 'LSU')
#'
#' cfb_game_team_stats(2013, team = "Florida State")
#'

cfb_game_team_stats <- function(year,
                                week = NULL,
                                season_type = 'regular',
                                team = NULL,
                                conference = NULL,
                                game_id = NULL,
                                rows_per_team = 1) {

  # Check if year is numeric
  assert_that(is.numeric(year) & nchar(year) == 4,
              msg='Enter valid year (Integer): 4-digit (YYYY)')

  if(!is.null(week)){
    # Check if week is numeric, if not NULL
    assert_that(is.numeric(week) & nchar(week) <= 2,
                msg='Enter valid week (Integer): 1-15\n(14 for seasons pre-playoff, i.e. 2014 or earlier)')
  }
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
  if(!is.null(game_id)){
    # Check if game_id is numeric, if not NULL
    assert_that(is.numeric(game_id),
                msg='Enter valid game_id value (Integer)\nCan be found using the `cfb_game_info()` function')
  }
  if(rows_per_team != 1){
    # Check if rows_per_team is 2, if not 1
    assert_that(rows_per_team == 2,
                msg='Enter valid rows_per_team value (Integer): 1 or 2')
  }

  base_url <- "https://api.collegefootballdata.com/games/teams?"

  full_url <- paste0(base_url,
                     "year=", year,
                     "&week=", week,
                     "&seasonType=", season_type,
                     "&team=", team,
                     "&conference=", conference,
                     "&gameId=", game_id)

  # Check for internet
  check_internet()

  # Create the GET request and set response as res
  res <- GET(full_url)

  # Check the result
  check_status(res)

  # Get the content, unnest, and return result as data.frame
  df = fromJSON(full_url, flatten=TRUE) %>%
    map_if(is.data.frame,list) %>%
    as_tibble() 
  
  if(nrow(df)==0){
    warning("Most likely a bye week, the data pulled from the API was empty. Returning nothing
            for this one week or team.")
    return(NULL)
  }
  df = df %>%
    unnest(.data$teams) %>%
    unnest(.data$stats)

  # Pivot category columns to get stats for each team game on one row
  df <- pivot_wider(df,
                    names_from = .data$category,
                    values_from = .data$stat)

  if(rows_per_team == 1){
    # Join pivoted data with itself to get ultra-wide row
    # containing all game stats on one row for both teams
    df <- df %>%
      left_join(df,
                by= c('id','school'),
                suffix = c('', '_allowed'))

    if(!is.null(team)){

      team <- URLdecode(team)

      df <- df %>%
        filter(.data$school == team)

      return(df)
    } else if(!is.null(conference)){

      confs <- cfb_conferences()

      conference = URLdecode(conference)

      conf_name <- confs[confs$abbreviation == conference,]$name

      df <- df %>%
        filter(conference == conf_name)

      return(df)
    } else{
      return(df)
    }
  } else{
    if(!is.null(team)){

      team <- URLdecode(team <- team)

      df <- df %>%
        filter(.data$school == team)

      return(df)
    } else if(!is.null(conference)){

      confs <- cfb_conferences()

      conference = URLdecode(conference)

      conf_name <- confs[confs$abbreviation == conference,]$name

      df <- df %>%
        filter(conference == conf_name)

      return(df)
    } else{
      return(df)
    }
  }
}
