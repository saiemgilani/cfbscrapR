

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
  
  pbp.final <- pbp.df %>%
    purrr::map_if(is.data.frame, list) %>%
    dplyr::as_tibble() %>%
    tidyr::unnest(.data$possessions) %>%
    purrr::map_if(is.data.frame, list) %>%
    dplyr::as_tibble() %>%
    tidyr::unnest(.data$plays, names_repair = "unique") %>%
    select(shortTitle, teamId...3, time, scoreText, driveText, visitingScore, homeScore) %>%
    rename("quarter" = .data$shortTitle,
           "team_id" = .data$teamId...3) %>%
    janitor::clean_names()
  
  return(pbp.final)
}
