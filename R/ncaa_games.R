
ncaa_games <- function(week, year) {
  assertthat::assert_that(is.numeric(year) & nchar(year) == 4,
                          msg = 'Enter valid year as a number (YYYY)')
  if(!is.null(week)){
    # Check if week is numeric, if not NULL
    assertthat::assert_that(is.numeric(week) & nchar(week) <= 2,
                            msg = 'Enter valid week 1-15\n(14 for seasons pre-playoff, i.e. 2014 or earlier)')
  }
  
  base_url <- "https://data.ncaa.com/casablanca/scoreboard/football/fbs/"
  
  full_url <- paste(base_url, year, week, "scoreboard.json", sep="/")
  
  # Check for internet
  check_internet()
  
  # Create the GET request and set response as res
  res <- httr::GET(full_url)
  
  # Check the result
  check_status(res)
  
  games.json <- fromJSON(full_url, flatten = TRUE)
  
  games <- games.json$games
  
  #Return single value for column names
  games$AwayConference <- apply(games, 1, function(x) {
    away.conf <- unlist(x["game.away.conferences"][[1]])[1]
    return(away.conf)
  })
  
  games$HomeConference <- apply(games, 1, function(x) {
    home.conf <- unlist(x["game.home.conferences"][[1]])[1]
    return(home.conf)
  })
  
  drops <- c("game.away.conferences", "game.home.conferences")
  games.final <- games[,!(names(games) %in% drops)]
  
  return(games.final)
}
