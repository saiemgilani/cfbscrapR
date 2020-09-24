#' Gets Historical CFB poll rankings at a specific week
#'
#' Postseason polls are after Week 13
#'
#' @param year (\emph{Integer} required): Year, 4 digit format (\emph{YYYY})
#' @param week (\emph{Integer} optional): Week, values from 1-15, 1-14 for seasons pre-playoff (i.e. 2013 or earlier)
#' @param season_type (\emph{String} default regular): Season type - regular or postseason
#' 
#' @return A data frame with 9 variables:
#' \describe{
#'   \item{\code{season}}{integer.}
#'   \item{\code{season_type}}{character.}
#'   \item{\code{week}}{integer.}
#'   \item{\code{poll}}{character.}
#'   \item{\code{rank}}{integer.}
#'   \item{\code{school}}{character.}
#'   \item{\code{conference}}{character.}
#'   \item{\code{first_place_votes}}{integer.}
#'   \item{\code{points}}{integer.}
#' }
#' @source \url{https://api.collegefootballdata.com/rankings}
#' @keywords CFB Rankings
#' @importFrom jsonlite "fromJSON"
#' @import dplyr
#' @import tidyr
#' @import purrr
#' @export
#' @examples
#' 
#' cfb_rankings(year = 2019, week = 12)
#'
#' cfb_rankings(year = 2018, week = 14)
#'
#' cfb_rankings(year = 2013, season_type = 'postseason')
#'

cfb_rankings <- function(year, week = NULL, season_type = 'regular'){

  if(!is.null(year)){
    ## check if year is numeric
    assertthat::assert_that(is.numeric(year) & nchar(year)==4,
                msg = 'Enter valid year (Integer) in 4 digit format (YYYY)')
  }
  if(!is.null(week)){
    # Check if week is numeric, if not NULL
    assertthat::assert_that(is.numeric(week) & nchar(week) <= 2,
                msg = 'Enter valid week 1-15, 1-14 for seasons pre-playoff, \n(i.e. 2014 or earlier)')
  }
  if(season_type != 'regular'){
    assertthat::assert_that(season_type == 'postseason',
                msg = 'Enter a valid season_type (String): regular or postseason')
  }

  base_url = "https://api.collegefootballdata.com/rankings?"


  url = paste0(base_url,
               "year=", year,
               "&week=", week,
               "&seasonType=", season_type)

  polls <- data.frame()
  tryCatch(
    expr = {
      polls <- jsonlite::fromJSON(url, flatten = TRUE) %>%
        purrr::map_if(is.data.frame,list) %>%
        dplyr::as_tibble() %>%
        tidyr::unnest(.data$polls) %>%
        tidyr::unnest(.data$ranks) %>%
        dplyr::group_by(.data$week, .data$poll) %>%
        dplyr::arrange(.data$rank, .by_group=TRUE) %>%
       dplyr::ungroup() %>% 
        dplyr::rename(
          season_type = .data$seasonType,
          first_place_votes = .data$firstPlaceVotes) %>% 
        as.data.frame()
  
      message(glue::glue("{Sys.time()}: Scraping rankings data..."))
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments or no rankings data available!"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )   
  return(polls)
}
