

ncaa_pbp <- function(game_id) {
  base_url <- "https://data.ncaa.com/casablanca/game/"

  full_url <- paste(base_url, game_id, "pbp.json", sep="/")
  
  # Check for internet
  #check_internet()
  
  # Create the GET request and set response as res
  res <- httr::GET(full_url)
  
  # Check the result
  #check_status(res)
  
  pbp.json <- fromJSON(full_url, flatten = TRUE)
  
  pbp.df <- as.data.frame(pbp.json$periods)
  
  pbp <- pbp.df %>%
    purrr::map_if(is.data.frame, list) %>%
    dplyr::as_tibble() %>%
    tidyr::unnest(.data$possessions) %>%
    purrr::map_if(is.data.frame, list) %>%
    dplyr::as_tibble() %>%
    tidyr::unnest(.data$plays, names_repair = "unique") %>%
    dplyr::select(shortTitle, teamId...3, time, scoreText, driveText, visitingScore, homeScore) %>%
    dplyr::rename("quarter" = .data$shortTitle,
           "team_id" = .data$teamId...3) %>%
    dplyr::mutate(play_order = row_number()) %>%
    janitor::clean_names()
  
  meta.data.df <- as.data.frame(pbp.json$meta$teams) %>% select(id, homeTeam, shortname)
  pbp.final <- merge(pbp, meta.data.df, by.x = "team_id", by.y = "id")
  pbp.final <- pbp.final %>% 
    dplyr::arrange(play_order) %>% 
    janitor::clean_names()
  
  return(pbp.final)
}
