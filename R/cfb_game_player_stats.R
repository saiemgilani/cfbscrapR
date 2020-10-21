#' Get results information from games
#'
#' @param year (\emph{Integer} required): Year, 4 digit format(\emph{YYYY})
#' @param week (\emph{Integer} optional): Week - values from 1-15, 1-14 for seasons pre-playoff (i.e. 2013 or earlier)
#' @param season_type (\emph{String} default regular): Select Season Type: regular or postseason
#' @param team (\emph{String} optional): D-I Team
#' @param category (\emph{String} optional): Category filter (e.g defensive)\cr
#' Offense: passing, receiving, rushing\cr
#' Defense: defensive, fumbles, interceptions\cr
#' Special Teams: punting, puntReturns, kicking, kickReturns\cr
#' @param conference (\emph{String} optional): Conference abbreviation - Select a valid FBS conference\cr
#' Conference abbreviations P5: ACC, B12, B1G, SEC, PAC\cr
#' Conference abbreviations G5 and FBS Independents: CUSA, MAC, MWC, Ind, SBC, AAC\cr
#' @param game_id (\emph{Integer} optional): Game ID filter for querying a single game
#' Can be found using the \code{\link[cfbscrapR:cfb_game_info]{cfbscrapR::cfb_game_info()}} function
#'
#' @return A data frame with 32 variables:
#' \describe{
#'   \item{\code{game_id}}{integer.}
#'   \item{\code{team}}{character.}
#'   \item{\code{conference}}{character.}
#'   \item{\code{home_away}}{character.}
#'   \item{\code{points}}{integer.}
#'   \item{\code{category}}{character.}
#'   \item{\code{athlete_id}}{character.}
#'   \item{\code{name}}{character.}
#'   \item{\code{c_att}}{character.}
#'   \item{\code{yds}}{double.}
#'   \item{\code{avg}}{double.}
#'   \item{\code{td}}{double.}
#'   \item{\code{int}}{double.}
#'   \item{\code{qbr}}{double.}
#'   \item{\code{car}}{double.}
#'   \item{\code{long}}{double.}
#'   \item{\code{rec}}{double.}
#'   \item{\code{no}}{double.}
#'   \item{\code{fg}}{character.}
#'   \item{\code{pct}}{double.}
#'   \item{\code{xp}}{character.}
#'   \item{\code{pts}}{double.}
#'   \item{\code{tb}}{double.}
#'   \item{\code{in_20}}{double.}
#'   \item{\code{fum}}{double.}
#'   \item{\code{lost}}{double.}
#'   \item{\code{tot}}{double.}
#'   \item{\code{solo}}{double.}
#'   \item{\code{sacks}}{double.}
#'   \item{\code{tfl}}{double.}
#'   \item{\code{pd}}{double.}
#'   \item{\code{qb_hur}}{double.}
#' }
#' @source \url{https://api.collegefootballdata.com/games/players}
#' @keywords Game Info
#' @importFrom jsonlite "fromJSON"
#' @importFrom httr "GET"
#' @importFrom utils "URLencode" "URLdecode"
#' @importFrom assertthat "assert_that"
#' @importFrom janitor "clean_names"
#' @importFrom glue "glue"
#' @import dplyr
#' @import tidyr
#' @import purrr
#' @export
#' @examples
#' 
#' cfb_game_player_stats(2018, week = 15, conference = 'Ind')
#'
#' cfb_game_player_stats(2013, week=1, team = "Florida State", category = 'passing')
#'

cfb_game_player_stats<- function(year,
                          week = NULL,
                          season_type = 'regular',
                          team = NULL,
                          conference = NULL,
                          category = NULL,
                          game_id = NULL) {

  stat_categories <- c('passing', 'receiving', 'rushing', 'defensive', 'fumbles',
                       'interceptions', 'punting', 'puntReturns', 'kicking', 'kickReturns')
  
  args <- list(year, week, season_type, team, conference, category, game_id)
  
  args <- args[lengths(args) != 0] 
  
  # Check if year is numeric
  assertthat::assert_that(is.numeric(year) & nchar(year) == 4,
              msg = 'Enter valid year as a number (YYYY)')
  if(!is.null(week)){
    # Check if week is numeric, if not NULL
    assertthat::assert_that(is.numeric(week) & nchar(week) <= 2,
                msg = 'Enter valid week 1-15\n(14 for seasons pre-playoff, i.e. 2014 or earlier)')
  }
  if(season_type != 'regular'){
    # Check if season_type is appropriate, if not regular
    assertthat::assert_that(season_type %in% c('postseason'),
                msg = 'Enter valid season_type: regular, postseason')
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
  if(!is.null(category)){
    # Check category parameter in category if not NULL
    assertthat::assert_that(category %in% stat_categories,
                msg = "Incorrect category, potential misspelling.\nOffense: passing, receiving, rushing\nDefense: defensive, fumbles, interceptions\nSpecial Teams: punting, puntReturns, kicking, kickReturns")
    # Encode conference parameter for URL, if not NULL
    category = utils::URLencode(category, reserved = TRUE)
  }
  if(!is.null(game_id)){
    # Check if game_id is numeric, if not NULL
    assertthat::assert_that(is.numeric(game_id),
                msg = 'Enter valid game_id (numeric value)')
  }

  base_url <- "https://api.collegefootballdata.com/games/players?"

  full_url <- paste0(base_url,
                     "year=", year,
                     "&week=", week,
                     "&seasonType=", season_type,
                     "&team=", team,
                     "&conference=", conference,
                     "&category=", category,
                     "&gameId=", game_id)

  # Check for internet
  check_internet()

  # Create the GET request and set response as res
  res <- httr::GET(full_url)

  # Check the result
  check_status(res)
  
  cols <- c("game_id", "team", "conference", "home_away", "points", "category", 
            "athlete_id", "name", "c_att", "yds", "avg", "td", "int", "qbr",
            "car", "long", "rec", "no", "fg", "pct", "xp", "pts", "tb", "in_20", 
            "fum", "lost", "tot", "solo", "sacks", "tfl", "pd", "qb_hur")
  numeric_cols <- c("yds", "avg", "td", "int", "qbr",
                    "car", "long", "rec", "no","pct","pts", "tb", "in_20", 
                    "fum", "lost", "tot", "solo", "sacks", "tfl", "pd", "qb_hur")
  
  df <- data.frame()
  tryCatch(
    expr ={
      # Get the content, tidyr::unnest, and return result as data.frame
      df = jsonlite::fromJSON(full_url, flatten=TRUE) %>%
        purrr::map_if(is.data.frame, list) %>%
        dplyr::as_tibble() %>%
        dplyr::rename(game_id = .data$id) %>%
        tidyr::unnest(.data$teams) %>%
        purrr::map_if(is.data.frame, list) %>%
        dplyr::as_tibble() %>%
        tidyr::unnest(.data$categories) %>%
        purrr::map_if(is.data.frame, list) %>%
        dplyr::as_tibble() %>%
        dplyr::rename(category = .data$name) %>%
        tidyr::unnest(.data$types) %>%
        purrr::map_if(is.data.frame, list) %>%
        dplyr::as_tibble() %>%
        dplyr::rename(stat_category = .data$name) %>%
        tidyr::unnest(.data$athletes) %>%
        dplyr::rename(athlete_id = .data$id,
                      team = .data$school,
                      value = .data$stat) %>% 
        pivot_wider(names_from = .data$stat_category, values_from = .data$value) %>% 
        janitor::clean_names()
      
      df[cols[!(cols %in% colnames(df))]] = NA
      
      df <- df %>% 
        dplyr::select(cols, dplyr::everything()) %>% 
        dplyr::mutate_at(numeric_cols, as.numeric) %>% 
        as.data.frame()
      
      message(glue::glue("{Sys.time()}: Scraping game player stats data..."))
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments or no game player stats data available!"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )
  # is_c_att_present <- any(grepl("C/ATT",colnames(df)))
  # if(is_c_att_present){
  #   df <- df %>%
  #    dplyr::mutate("C/ATT"="0/0")
  # }
  return(df)
}
