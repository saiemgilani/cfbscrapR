#' Plot the Drive/PBP Sequencing
#'
#' @param df Play-by-Play data frame (can be retrieved from \code{\link{cfb_pbp_data()}})
#' @keywords Plot PbP
#' @import dplyr
#' @import ggplot2
#' @import stringr
#' @import stringi
#' @import ggrepel
#' @export
#' @examples
#'
#'
#'
#' df = cfb_pbp_data(year=2019,week=9,team='Texas A&M',epa_wpa=TRUE)
#'
#' plot_pbp_sequencing(df)
#'

plot_pbp_sequencing <- function(df) {
  clean_game_df = prep_df_pbp_overview(df) %>% 
    filter(!stringr::str_detect(.data$play_type, "End"))
  game_id <- unique(clean_game_df$game_id)
  clean_game_df$new_drive_id = as.numeric(gsub(game_id, "", clean_game_df$drive_id))
  
  clean_drive_info = clean_game_df %>% group_by(.data$drive_id) %>%
    filter(row_number() == (n())) %>% ungroup() %>%
    mutate(
      y_max = max(.data$play_num) + 5,
      score_text = ifelse(.data$drive_scoring == TRUE, .data$score_text, NA)
    )
  
  nd_id = clean_drive_info$new_drive_id
  off_team = clean_drive_info$offense_play
  
  ggplot() +
    geom_tile(
      data = clean_game_df,
      aes(
        x = .data$new_drive_id,
        y = .data$play_num,
        fill = .data$clean_play_type,
        alpha = .data$yards_gained,
        width = 0.8
      ),
      size = 1.2
    ) +
    geom_text_repel(
      data = clean_game_df,
      aes(
        x = .data$new_drive_id,
        y = .data$play_num,
        label = .data$event
      ),
      point.padding = NA
    ) +
    geom_label_repel(
      data = clean_drive_info,
      aes(
        x = .data$new_drive_id,
        y = .data$y_max,
        label = .data$score_text
      ),
      point.padding = NA
    ) +
    coord_flip() +
    scale_alpha(
      range = c(0.05, 1),
      limits = c(-20, 20),
      breaks = c(-20,-10, 0, 10, 20),
      labels = c("-20+", "-10", "0", "10", "20+"),
      name = "Yards gained"
    ) +
    scale_x_reverse(labels = off_team,
                    breaks = nd_id,
                    expand = expansion(add = 1.2)) +
    labs(
      x = "",
      y = "",
      fill = "Play type",
      alpha = "Yards gained",
      title = "Play-by-Play Overview",
      subtitle = paste0(
        unique(clean_game_df$away),
        "@",
        unique(clean_game_df$home)
      ),
      caption = "@CFB_Data | @cfbscrapR"
    ) +
    theme_classic(base_size = 16)
}

#' prep_df_pbp_overview
#' @param df Play-by-Play data frame (can be retrieved from cfb_pbp_data)
#' @keywords Plot PBP
#' @import dplyr
#' @import ggplot2
#' @import stringr
#' @import stringi
#'
prep_df_pbp_overview <- function(df) {
  clean_df = df %>% arrange(.data$id_play) %>%
    mutate(
      event = case_when(
        str_detect(.data$play_text, "fumble") ~ "Fumble",
        str_detect(.data$play_text, "interception") ~ "INT",
        str_detect(.data$play_text, "sack") ~ "Sack",
        TRUE ~ "nothing"
      ),
      event = ifelse(.data$event == "nothing", NA, .data$event),
      clean_play_type = case_when(
        str_detect(.data$play_type, "Pass") ~ "Pass",
        str_detect(.data$play_type, "Rush") ~ "Rush",
        str_detect(.data$play_type, "Field Goal") ~ "Kick",
        str_detect(.data$play_type, "Kickoff") ~ "Kick",
        str_detect(.data$play_type, "Punt") ~ "Punt",
        str_detect(.data$play_type, "Penalty") ~ "Penalty",
        TRUE ~ "Other"
      ),
      drive_id = cumsum(.data$offense_play != lead(.data$offense_play)),
      score_text = paste0(.data$offense_score, "-", .data$defense_score)
    ) %>% group_by(.data$drive_id) %>%  arrange(.data$id_play) %>%
    mutate(play_num = row_number())  %>% ungroup()
  return(clean_df)
}
