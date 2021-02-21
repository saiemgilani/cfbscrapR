
ncaa_box_score <- function(game_id) {
  base_url <- "https://data.ncaa.com/casablanca/game/"
  
  full_url <- paste(base_url, game_id, "boxscore.json", sep="/")
  
  # Check for internet
  #check_internet()
  
  # Create the GET request and set response as res
  res <- httr::GET(full_url)
  
  # Check the result
  #check_status(res)
  
  bs.json <- fromJSON(full_url, flatten = TRUE)
  
  bs.df <- as.data.frame(bs.json$tables)
  
  bs.df2 <- bs.df %>%
    purrr::map_if(is.data.frame, list) %>%
    dplyr::as_tibble() %>%
    tidyr::unnest(.data$data, names_repair = "unique")
  
  data <- bs.df2 %>%
    purrr::map_if(is.data.frame, list) %>%
    dplyr::as_tibble() %>%
    tidyr::unnest(.data$row, names_repair = "unique") 
  
  headers <- bs.df2 %>%
    purrr::map_if(is.data.frame, list) %>%
    dplyr::as_tibble() %>%
    tidyr::unnest(.data$header, names_repair = "unique")
  
  #Rename data
  combined <- cbind(data, headers)
  colnames(combined) <- c("id", "headerColor", "headerClass", "header", "playerCategory", "display", "total",
                          "id2", "headerColor2", "headerClass2", "statCategory", "headerdisplay", "row", "total2")
  
  #Assign player name to each row
  formatted <- combined %>% select(id, headerColor, headerClass, playerCategory, display, total, headerdisplay) %>% 
    mutate(playerid = cumsum(case_when(playerCategory == "playerName" ~ 1,
                                       TRUE ~ 0))) %>% 
    group_by(playerid) %>% 
    mutate(playerName = trimws(first(display))) %>% 
    ungroup() %>% 
    select(-playerid) %>%
    filter(is.na(playerCategory))
  
  #Clarify duplicate column names
  formatted$headerdisplay <- apply(formatted, 1, function(x) {
    id <- as.character(x["id"])
    stat.name <- as.character(x["headerdisplay"])
    
    if (id == "rushing_visiting" | id == "rushing_home") {
      return(paste0("Rushing.", stat.name))
    } else if (id == "receiving_visiting" | id == "receiving_home") {
      return(paste0("Receiving.", stat.name))
    } else if (id == "passing_visiting" | id == "passing_home") {
      return(paste0("Passing.", stat.name))
    } else if (id == "punt_returns_visiting" | id == "punt_returns_home") {
      return(paste0("Punt.Return.", stat.name))
    } else if (id == "kick_returns_visiting" | id == "kick_returns_home") {
      return(paste0("Kick.Return.", stat.name))
    }
    
    return(stat.name)
  })
  
  #Remove total fields, only focus on players
  no.total <- formatted %>% pivot_wider(names_from = .data$headerdisplay, values_from = .data$display) %>% 
    separate(`Passing.CP-ATT-INT`, c("PassingComp", "PassingAtt", "PassingInt"), "-") %>% 
    separate(`FG-FGA`, c("FG", "FGA"), "/") %>% 
    filter(is.na(total)) %>% 
    select(-c(headerClass, playerCategory, total, id, headerColor))
  
  no.total[,2:38] <- sapply(no.total[,2:38],as.numeric)
  no.total[is.na(no.total)] <- 0
  
  final <- no.total %>% 
    group_by(playerName) %>% 
    summarise(across(everything(), sum)) %>% 
    janitor::clean_names()
  
  return(final)
}
