
ncaa_scoring_summary <- function(game_id) {
  base_url <- "https://data.ncaa.com/casablanca/game/"
  
  full_url <- paste(base_url, game_id, "scoringSummary.json", sep="/")
  
  # Check for internet
  #check_internet()
  
  # Create the GET request and set response as res
  res <- httr::GET(full_url)
  
  # Check the result
  #check_status(res)
  
  scoring.json <- fromJSON(full_url, flatten = TRUE)
  
  scores <- as.data.frame(scoring.json$periods)
  summary.df <- scores %>%
    purrr::map_if(is.data.frame, list) %>%
    dplyr::as_tibble() %>%
    tidyr::unnest(.data$summary) %>%
    dplyr::mutate(series_order = row_number()) %>%
    janitor::clean_names()
  
  meta.data.df <- as.data.frame(scoring.json$meta$teams) %>% select(id, homeTeam, shortname)
  scoring.summary.final <- merge(summary.df, meta.data.df, by.x = "team_id", by.y = "id")
  scoring.summary.final <- scoring.summary.final %>% arrange(series_order) %>% 
    janitor::clean_names()
  
  return(scoring.summary.final)
}
