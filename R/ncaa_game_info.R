
ncaa_game_info <- function(game_id) {
  base_url <- "https://data.ncaa.com/casablanca/game/"
  
  full_url <- paste(base_url, game_id, "gameInfo.json", sep="/")
  
  # Check for internet
  #check_internet()
  
  # Create the GET request and set response as res
  res <- httr::GET(full_url)
  
  # Check the result
  #check_status(res)
  

  info.json <- fromJSON(full_url, flatten = TRUE)
  info.df <- as.data.frame(unlist(info.json)) %>% rownames_to_column(var = "name") %>% 
    rename(value = .data$`unlist(info.json)`) %>%
    pivot_wider(names_from = .data$name, values_from = .data$value) %>% janitor::clean_names()
  
  return(info.df)
}
