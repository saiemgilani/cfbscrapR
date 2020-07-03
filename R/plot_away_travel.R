#' Plot a teams away travel
#' 
#'
#' @param year Year 
#' @param team_name Team Name
#'
#' @keywords Plot Travel
#' @import dplyr
#' @import ggplot2
#' @import glue 
#' @importFrom geosphere distHaversine
#' @export
#' @examples
#' 
#' plot_away_travel(2019,"Texas")
#' 
#'
#'
plot_away_travel <- function(year,team_name) {
  # get data
  team_sched <- cfb_game_info(year, team = team_name)
  venue_deets <- cfb_venues()
  # unnest venue location
  venue <- venue_deets %>%  select(.data$id, .data$name, .data$capacity,.data$location) %>%
    mutate(loc_x = unlist(.data$location$x),
           loc_y = unlist(.data$location$y)) %>% select(-.data$location)
  
  team_sched_deets = team_sched %>% left_join(venue, by = c("venue_id" = "id")) %>%
    mutate(win = if_else(
      .data$home_team == team_name,
      .data$home_points > .data$away_points,
      .data$home_points < .data$away_points
    ))
  
  home_games = team_sched_deets %>% filter(.data$home_team == team_name) %>% slice(1)
  away_games = team_sched_deets %>% filter(!.data$home_team == team_name) %>%
    mutate(start_x = home_games %>% pull(.data$loc_x),
           start_y = home_games %>% pull(.data$loc_y))
  
  ## meters calculation
  total_dist <-
    distHaversine(matrix(c(away_games$loc_y, away_games$loc_x), ncol = 2),
                  matrix(c(
                    away_games$start_y, away_games$start_x
                  ), ncol = 2))
  
  away_games$dist <- total_dist / 1609.34
  
  states <- map_data("state")
  map.opts <-
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      panel.background = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      axis.text.y = element_text(colour = "#FFFFFF"),
      axis.text.x = element_text(colour = "#FFFFFF")
    )
  
  total_miles_travelled <- round(sum(away_games$dist), 2)
  
  p <- ggplot(states) +
    geom_polygon(aes(x = .data$long, y = .data$lat, group = .data$group),
                 color = 'black',
                 fill = 'grey') + map.opts  +
    coord_fixed(1.3) +
    geom_curve(
      data = away_games,
      aes(
        x = .data$start_y,
        y = .data$start_x,
        xend = .data$loc_y,
        yend = .data$loc_x,
        col = .data$win,
      ),
      size = 1,
      curvature = .2
    ) +
    scale_color_manual(name = "Win", values = c("red", "green")) +
    labs(
      title = glue::glue("Distance travelled for {team_name} in {year}"),
      subtitle = glue::glue("{total_miles_travelled} miles travelled")
    )
  theme(
    panel.background = element_rect(fill = "white"),
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )
  return(p)
}