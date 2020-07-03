#' Get game advanced box score information
#'
#' @param game_id (\emph{Integer} required): Game ID filter for querying a single game
#' Can be found using the `cfb_game_info()` function
#'
#' @keywords Game Advanced Box Score 
#' @importFrom tibble "enframe"
#' @importFrom jsonlite "fromJSON"
#' @importFrom httr "GET"
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
    map_if(is.data.frame,list) 
  
  df <- enframe(unlist(df$teams,use.names = TRUE)) 
  team1 <- seq(1,nrow(df)-1,by=2)
  df1 <- df[team1,]
  df1 <- df1 %>% rename(stat = .data$name, 
                        team1 = .data$value)
  
  team2 <- seq(2,nrow(df),by=2)
  df2 <- df[team2,]
  df2 <- df2 %>% 
    rename(team2 = .data$value) %>% 
    select(.data$team2)
    
  df <- data.frame(cbind(df1,df2))
  df$stat <- substr(df$stat,1,nchar(df$stat)-1) 
  df$stat = sub(".overall.","_overall_", df$stat)
  df$stat = sub("Downs.","_downs_", df$stat)
  df$stat = sub("Rates.","_rates_", df$stat)
  df$stat = sub("Rate","_rate", df$stat)
  df$stat = sub(".passing.","_passing_", df$stat)
  df$stat = sub(".rushing.","_rushing_", df$stat)
  df$stat = sub("rushing.","rushing_", df$stat)
  df$stat = sub("rushing.","rushing_", df$stat)
  df$stat = sub("fieldPosition.","field_pos_", df$stat)
  df$stat = sub("lineYards","line_yds", df$stat)
  df$stat = sub("secondLevelYards","second_lvl_yds", df$stat)
  df$stat = sub("openFieldYards","open_field_yds", df$stat)
  df$stat = sub("Success","_success", df$stat)
  df$stat = sub("scoringOpportunities.","scoring_opps_", df$stat)
  df$stat = sub("pointsPerOpportunity","pts_per_opp", df$stat)
  df$stat = sub("Seven","_seven", df$stat)
  df$stat = sub("havoc.","havoc_", df$stat)
  df$stat = sub(".Average","_avg", df$stat)
  df$stat = sub("averageStartingPredictedPoints","avg_starting_predicted_pts", df$stat)
  df$stat = sub("averageStart","avg_start", df$stat)
  df$stat = sub(".team","_team", df$stat)
  
  
  return(df)
}
