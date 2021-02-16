
ncaa_team_stats <- function(game_id) {
  base_url <- "https://data.ncaa.com/casablanca/game/"
  
  full_url <- paste(base_url, game_id, "teamStats.json", sep="/")
  
  # Check for internet
  #check_internet()
  
  # Create the GET request and set response as res
  res <- httr::GET(full_url)
  
  # Check the result
  #check_status(res)
  
  ts.json <- fromJSON(full_url, flatten = TRUE)
  
  ts.df <- as.data.frame(ts.json$teams)
  #Store stats df to get stats for categories without a breakdown
  stats.df <- ts.df %>%
    purrr::map_if(is.data.frame, list) %>%
    dplyr::as_tibble() %>%
    tidyr::unnest(.data$stats) 
  #get stats for categories with a breakdown
  breakdown.df <- stats.df %>%
    purrr::map_if(is.data.frame, list) %>%
    dplyr::as_tibble() %>%
    tidyr::unnest(.data$breakdown, names_repair = "unique")
  
  stats.df <- stats.df %>% select(teamId, stat, data) %>% rename("team_id" = .data$teamId,
                                                                 "value" = .data$data)
  breakdown.df <- breakdown.df %>% mutate(stat = paste(.data$stat...2, .data$stat...4, sep = "_")) %>%
    select(teamId, stat, data...5) %>% rename("team_id" = .data$teamId, "value" = .data$data...5)
  
  #Merge top level stats and stat breakdowns
  all.stats <- rbind(stats.df, breakdown.df)
  #Widen the data
  stats.wider <- all.stats %>% pivot_wider(names_from = .data$stat, values_from = .data$value) %>% janitor::clean_names()
  
  return(stats.wider)
}
