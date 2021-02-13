

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
  
  game.plays <- do.call("rbind", apply(pbp.df, 1, function(x) {
    possessions <- x["possessions"]
    quarter <- x["shortTitle"]
    
    poss.df <- as.data.frame(possessions[[1]])
    print(head(poss.df))
    quarter.plays <- do.call("rbind", apply(poss.df, 1, function(x) {
      plays <- x["plays"]
      time <- x["time"]
      
      plays.df <- as.data.frame(plays)
      plays.df$Time <- time
      return(plays.df)
    }))
    
    quarter.plays$Quarter <- quarter
    return(quarter.plays)
  }))
  
  return(game.plays)
}
