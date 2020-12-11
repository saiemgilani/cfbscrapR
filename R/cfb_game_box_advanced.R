#' Get game advanced box score information
#'
#' @param game_id (\emph{Integer} required): Game ID filter for querying a single game
#' Can be found using the \code{\link[cfbscrapR:cfb_game_info]{cfbscrapR::cfb_game_info()}} function
#' @param long (\emph{Logical} default `FALSE`): Return the data in a long format.
#' @return A data frame with 2 rows and 69 variables:
#' \describe{
#'   \item{\code{team}}{character.}
#'   \item{\code{plays}}{double.}
#'   \item{\code{ppa_overall_total}}{double.}
#'   \item{\code{ppa_overall_quarter1}}{double.}
#'   \item{\code{ppa_overall_quarter2}}{double.}
#'   \item{\code{ppa_overall_quarter3}}{double.}
#'   \item{\code{ppa_overall_quarter4}}{double.}
#'   \item{\code{ppa_passing_total}}{double.}
#'   \item{\code{ppa_passing_quarter1}}{double.}
#'   \item{\code{ppa_passing_quarter2}}{double.}
#'   \item{\code{ppa_passing_quarter3}}{double.}
#'   \item{\code{ppa_passing_quarter4}}{double.}
#'   \item{\code{ppa_rushing_total}}{double.}
#'   \item{\code{ppa_rushing_quarter1}}{double.}
#'   \item{\code{ppa_rushing_quarter2}}{double.}
#'   \item{\code{ppa_rushing_quarter3}}{double.}
#'   \item{\code{ppa_rushing_quarter4}}{double.}
#'   \item{\code{cumulative_ppa_plays}}{double.}
#'   \item{\code{cumulative_ppa_overall_total}}{double.}
#'   \item{\code{cumulative_ppa_overall_quarter1}}{double.}
#'   \item{\code{cumulative_ppa_overall_quarter2}}{double.}
#'   \item{\code{cumulative_ppa_overall_quarter3}}{double.}
#'   \item{\code{cumulative_ppa_overall_quarter4}}{double.}
#'   \item{\code{cumulative_ppa_passing_total}}{double.}
#'   \item{\code{cumulative_ppa_passing_quarter1}}{double.}
#'   \item{\code{cumulative_ppa_passing_quarter2}}{double.}
#'   \item{\code{cumulative_ppa_passing_quarter3}}{double.}
#'   \item{\code{cumulative_ppa_passing_quarter4}}{double.}
#'   \item{\code{cumulative_ppa_rushing_total}}{double.}
#'   \item{\code{cumulative_ppa_rushing_quarter1}}{double.}
#'   \item{\code{cumulative_ppa_rushing_quarter2}}{double.}
#'   \item{\code{cumulative_ppa_rushing_quarter3}}{double.}
#'   \item{\code{cumulative_ppa_rushing_quarter4}}{double.}
#'   \item{\code{success_rates_overall_total}}{double.}
#'   \item{\code{success_rates_overall_quarter1}}{double.}
#'   \item{\code{success_rates_overall_quarter2}}{double.}
#'   \item{\code{success_rates_overall_quarter3}}{double.}
#'   \item{\code{success_rates_overall_quarter4}}{double.}
#'   \item{\code{success_rates_standard_downs_total}}{double.}
#'   \item{\code{success_rates_standard_downs_quarter1}}{double.}
#'   \item{\code{success_rates_standard_downs_quarter2}}{double.}
#'   \item{\code{success_rates_standard_downs_quarter3}}{double.}
#'   \item{\code{success_rates_standard_downs_quarter4}}{double.}
#'   \item{\code{success_rates_passing_downs_total}}{double.}
#'   \item{\code{success_rates_passing_downs_quarter1}}{double.}
#'   \item{\code{success_rates_passing_downs_quarter2}}{double.}
#'   \item{\code{success_rates_passing_downs_quarter3}}{double.}
#'   \item{\code{success_rates_passing_downs_quarter4}}{double.}
#'   \item{\code{explosiveness_overall_total}}{double.}
#'   \item{\code{explosiveness_overall_quarter1}}{double.}
#'   \item{\code{explosiveness_overall_quarter2}}{double.}
#'   \item{\code{explosiveness_overall_quarter3}}{double.}
#'   \item{\code{explosiveness_overall_quarter4}}{double.}
#'   \item{\code{rushing_power_success}}{double.}
#'   \item{\code{rushing_stuff_rate}}{double.}
#'   \item{\code{rushing_line_yds}}{double.}
#'   \item{\code{rushing_line_yd_avg}}{double.}
#'   \item{\code{rushing_second_lvl_yds}}{double.}
#'   \item{\code{rushing_second_lvl_yd_avg}}{double.}
#'   \item{\code{rushing_open_field_yds}}{double.}
#'   \item{\code{rushing_open_field_yd_avg}}{double.}
#'   \item{\code{havoc_total}}{double.}
#'   \item{\code{havoc_front_seven}}{double.}
#'   \item{\code{havoc_db}}{double.}
#'   \item{\code{scoring_opps_opportunities}}{double.}
#'   \item{\code{scoring_opps_points}}{double.}
#'   \item{\code{scoring_opps_pts_per_opp}}{double.}
#'   \item{\code{field_pos_avg_start}}{double.}
#'   \item{\code{field_pos_avg_starting_predicted_pts}}{double.}
#' }
#' @source \url{https://api.collegefootballdata.com/game/box/advanced}
#' @keywords Game Advanced Box Score 
#' @importFrom tibble enframe
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET
#' @importFrom utils URLencode URLdecode
#' @importFrom assertthat assert_that
#' @importFrom glue glue
#' @import dplyr
#' @import tidyr
#' @import purrr
#' @export
#' @examples
#'
#' cfb_game_box_advanced(401114233)
#'

cfb_game_box_advanced<- function(game_id, long = FALSE) {
  
  if(!is.null(game_id)){
    # Check if game_id is numeric, if not NULL
    assertthat::assert_that(is.numeric(game_id),
                            msg = 'Enter valid game_id (numeric value)')
  }
  
  base_url <- "https://api.collegefootballdata.com/game/box/advanced?"
  
  full_url <- paste0(base_url,
                     "gameId=", game_id)
  
  # Check for internet
  check_internet()
  
  # Create the GET request and set response as res
  res <- httr::GET(full_url)
  
  # Check the result
  check_status(res)
  
  df <- data.frame()
  tryCatch(
    expr ={
      # Get the content, tidyr::unnest, and return result as data.frame
      df <- jsonlite::fromJSON(full_url, flatten = TRUE) %>%
        purrr::map_if(is.data.frame, list) %>%
        purrr::map_if(is.data.frame, list) 
      
      df <- tibble::enframe(unlist(df$teams, use.names = TRUE)) 
      team1 <- seq(1, nrow(df)-1, by = 2)
      df1 <- df[team1,] %>% 
        dplyr::rename(stat = .data$name,
                      team1 = .data$value)
      
      team2 <- seq(2, nrow(df), by = 2)
      df2 <- df[team2,] %>% 
        dplyr::rename(team2 = .data$value) %>% 
        dplyr::select(.data$team2)
      
      df <- data.frame(cbind(df1, df2))
      df$stat <- substr(df$stat, 1, nchar(df$stat)-1) 
      df$stat = sub(".overall.", "_overall_", df$stat)
      df$stat = sub("Downs.", "_downs_", df$stat)
      df$stat = sub("Rates.", "_rates_", df$stat)
      df$stat = sub("Rate", "_rate", df$stat)
      df$stat = sub(".passing.", "_passing_", df$stat)
      df$stat = sub(".rushing.", "_rushing_", df$stat)
      df$stat = sub("rushing.", "rushing_", df$stat)
      df$stat = sub("rushing.", "rushing_", df$stat)
      df$stat = sub("fieldPosition.", "field_pos_", df$stat)
      df$stat = sub("lineYards", "line_yds", df$stat)
      df$stat = sub("secondLevelYards", "second_lvl_yds", df$stat)
      df$stat = sub("openFieldYards", "open_field_yds", df$stat)
      df$stat = sub("Success", "_success", df$stat)
      df$stat = sub("scoringOpportunities.", "scoring_opps_", df$stat)
      df$stat = sub("pointsPerOpportunity", "pts_per_opp", df$stat)
      df$stat = sub("Seven", "_seven", df$stat)
      df$stat = sub("havoc.", "havoc_", df$stat)
      df$stat = sub(".Average", "_avg", df$stat)
      df$stat = sub("averageStartingPredictedPoints", "avg_starting_predicted_pts", df$stat)
      df$stat = sub("averageStart", "avg_start", df$stat)
      df$stat = sub(".team", "_team", df$stat)
      df$stat = sub(".plays", "_plays", df$stat)
      df$stat = sub("cumulativePpa", "cumulative_ppa", df$stat)
      
      if(!long){
        team <- df %>%
          filter(stat == "ppa_team") %>%
          pivot_longer(cols = c(.data$team1, .data$team2)) %>%
          transmute(team = .data$value)
        
        df <- df %>%
          filter(!str_detect(.data$stat, "team")) %>%
          pivot_longer(cols = c(.data$team1, .data$team2)) %>%
          pivot_wider(names_from = .data$stat, values_from = .data$value) %>%
          select(-.data$name) %>%
          mutate_all(as.numeric) %>%
          bind_cols(team) %>%
          select(.data$team, everything()) %>% 
          as.data.frame()
      }
      
      message(glue::glue("{Sys.time()}: Scraping game advanced box score data for game_id '{game_id}'..."))
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: game_id '{game_id}' invalid or no game advanced box score data available!"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )
  return(df)
}