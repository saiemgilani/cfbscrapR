#' Gets Historical CFB poll rankings at a specific week
#'
#' Postseason polls are after Week 13
#'
#' @param year (*Integer* required): Year, 4 digit format (*YYYY*)
#' @param week (*Integer* optional): Week, values from 1-15, 1-14 for seasons pre-playoff (i.e. 2013 or earlier)
#' @param season_type (*String* default regular): Season type - regular or postseason
#' 
#' @return A data frame with 9 variables:
#' \describe{
#'   \item{`season`}{integer.}
#'   \item{`season_type`}{character.}
#'   \item{`week`}{integer.}
#'   \item{`poll`}{character.}
#'   \item{`rank`}{integer.}
#'   \item{`school`}{character.}
#'   \item{`conference`}{character.}
#'   \item{`first_place_votes`}{integer.}
#'   \item{`points`}{integer.}
#' }
#' @source <https://api.collegefootballdata.com/rankings>
#' @keywords CFB Rankings
#' @importFrom assertthat assert_that
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr arrange as_tibble group_by ungroup rename
#' @importFrom tidyr unnest
#' @importFrom purrr map_if
#' @importFrom glue glue
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
