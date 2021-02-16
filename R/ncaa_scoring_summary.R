
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
    janitor::clean_names()
  
  return(summary.df)
}
