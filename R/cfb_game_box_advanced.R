#' Get game advanced box score information
#'
#' @param game_id (\emph{Integer} required): Game ID filter for querying a single game
#' Can be found using the `cfb_game_info()` function
#'
#' @keywords Game Info
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET
#' @importFrom utils "URLencode" "URLdecode"
#' @importFrom assertthat "assert_that"
#' @import dplyr
#' @import tidyr
#' @import purrr
#' @export
#'
#' @examples
#'
#' cfb_game_box_advanced(401012356)
#'
#'

cfb_game_box_advanced<- function(game_id) {


  if(!is.null(game_id)){
    # Check if game_id is numeric, if not NULL
    assert_that(is.numeric(game_id),
                msg='Enter valid game_id (numeric value)')
  }

  base_url <- "https://api.collegefootballdata.com/game/box/advanced?"

  full_url <- paste0(base_url,
                     "&gameId=", game_id)

  # Check for internet
  check_internet()

  # Create the GET request and set response as res
  res <- GET(full_url)

  # Check the result
  check_status(res)

  # Get the content and return it as data.frame
  df = fromJSON(full_url)

  # Get the content, unnest, and return result as data.frame
  df = fromJSON(full_url,flatten=TRUE) %>%
    map_if(is.data.frame,list) %>%
    map_if(is.data.frame,list) #%>%
    #unnest(.data$ppa) %>%
    # unnest(.data$successRates) %>%
    # unnest(.data$explosiveness) %>%
    # unnest(.data$rushing) %>%
    # unnest(.data$havoc)

    # map_if(is.data.frame,list) %>%
    # as_tibble() %>%
    # unnest(.data$categories) %>%
    # map_if(is.data.frame,list) %>%
    # as_tibble() %>%
    # rename(category = .data$name) %>%
    # unnest(.data$types) %>%
    # map_if(is.data.frame,list) %>%
    # as_tibble() %>%
    # rename(stat_category = .data$name) %>%
    # unnest(.data$athletes) %>%
    # rename(athlete_id = .data$id,
    #        value = .data$stat)
  # df = as.data.frame(df)
  df<- df$teams$ppa
  return(df)
}
