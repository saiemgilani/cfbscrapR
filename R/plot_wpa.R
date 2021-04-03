#' Plot the WPA for a specific game
#' 
#'
#' @param dat (*data.frame* required) Play-by-Play data.frame as can be retrieved from [cfbscrapR::cfb_pbp_data()]
#' @param game_id (*Integer* optional) Game ID filter for querying a single game
#' Can be found using the [cfbscrapR::cfb_game_info()] function
#' @param away_color color selection for the away team
#' @param home_color color selection for the home team
#' @param winner Winner of contest, home or away.
#' @param bet TRUE or FALSE to use initial Win Probability converted from betting lines.
#'
#' @keywords Plot WPA
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @import ggplot2
#' @import stringr
#' @import ggrepel
#' @import tidyr
#' @export
#' @examples
#' df = cfb_pbp_data(year = 2019, week = 12, team = 'Baylor', epa_wpa = TRUE)
#' plot_wpa(df, away_color=c(OU="#841617"), home_color=c(BU="#003015"), winner="away")
#'


plot_wpa <- function(dat, game_id=NULL, away_color, home_color, winner="home",bet=FALSE){
  if(is.null(game_id)){
    total_games <- length(unique(dat$game_id))
    assert_that(total_games == 1, 
                msg = "More than one game present in the data, please specify which game id to plot")
  }
  if(!is.null(game_id)){
    assert_that(length(game_id)==1,
                msg = "Please specify one game id to plot")
    dat = dat %>% dplyr::filter(.data$game_id==game_id)
  }
  
  away_team <- names(away_color)
  home_team <- names(home_color)
  names(away_color) <- NULL
  names(home_color) <- NULL
  
  is_adj_time_present <- any(grepl("adj_TimeSecsRem",colnames(dat)))
  
  if(!is_adj_time_present){
    if(bet == FALSE){
      plot_df <- dat %>%
       dplyr::mutate(
          adj_TimeSecsRem = ifelse(
            .data$period %in% c(1, 3),
            .data$TimeSecsRem + 1800,
            .data$TimeSecsRem
          )
        ) %>%
        dplyr::select(.data$home_wp_before, .data$away_wp_before, .data$adj_TimeSecsRem)
    }else{
      plot_df <- dat %>%
       dplyr::mutate(
          adj_TimeSecsRem = ifelse(
            .data$period %in% c(1, 3),
            .data$TimeSecsRem + 1800,
            .data$TimeSecsRem
          )
        ) %>%
        dplyr::select(.data$home_wp_bet, .data$away_wp_bet, .data$adj_TimeSecsRem)
    }
  }else{
    if(bet == FALSE){
      plot_df <- dat %>% dplyr::select(.data$home_wp_before, .data$away_wp_before, .data$adj_TimeSecsRem)
    }else{
      plot_df <- dat %>% dplyr::select(.data$home_wp_bet, .data$away_wp_bet, .data$adj_TimeSecsRem)
    }
  }
  
  dups = duplicated(plot_df$adj_TimeSecsRem)
  
  if(any(dups)){
    print("Warning. Time was not recorded properly for this PBP")
    plot_df[dups,"adj_TimeSecsRem"] = plot_df[dups,"adj_TimeSecsRem"] - (cumsum(dups[dups])*2 + 5)
  }
  if(bet == FALSE){
    plot_df <- rbind(c(0.5,0.5,3600),plot_df)
  }
  if(winner=="away"){
    plot_df <- rbind(c(0,1,0),plot_df)
  }
  if(winner=="home"){
    plot_df <- rbind(c(1,0,0),plot_df)
  }
  if(bet == FALSE){
    # .data notation doesn't work here
    plot_df <- gather(plot_df, "team", "wp_before", -.data$adj_TimeSecsRem)


    p1 = ggplot(plot_df,aes(x=.data$adj_TimeSecsRem,y=.data$wp_before,color=.data$team)) +
      geom_line(size=2) +
      geom_hline(yintercept = 0.5, color = "gray", linetype = "dashed") +
      scale_x_reverse(breaks = seq(0, 3600, 300)) +
      scale_color_manual(labels = c(away_team,home_team),
                         values = c(away_color,home_color),
                         guide = FALSE)  +
      annotate("text", x = 3600, y = 0.1,
               label = away_team, color = away_color, size = 7) +
      annotate("text", x = 3200, y = 0.1,
               label = paste(" @",home_team), color = home_color, size=7) +
      scale_y_continuous(limits=c(0,1)) +
      geom_vline(xintercept = 900, linetype = "dashed", color= "black") +
      geom_vline(xintercept = 1800, linetype = "dashed",color= "black") +
      geom_vline(xintercept = 2700, linetype = "dashed", color= "black") +
      geom_vline(xintercept = 0, linetype = "dashed", color= "black") +
      labs(
        x = "Time Remaining (seconds)",
        y = "Win Probability",
        title = paste("Win Probability Chart"),
        subtitle = paste(home_team,"vs",away_team),
        caption = "Plot: @cfbscrapR | Data: CollegeFootballData API "
      ) + theme_bw(base_size = 16)
    return(p1)
  }
  else{
    # .data notation doesn't work here
    plot_df <- gather(plot_df, "team", "wp_bet", -.data$adj_TimeSecsRem)
    
    
    p1 = ggplot(plot_df,aes(x=.data$adj_TimeSecsRem,y=.data$wp_bet,color=.data$team)) +
      geom_line(size=2) +
      geom_hline(yintercept = 0.5, color = "gray", linetype = "dashed") +
      scale_x_reverse(breaks = seq(0, 3600, 300)) +
      scale_color_manual(labels = c(away_team,home_team),
                         values = c(away_color,home_color),
                         guide = FALSE)  +
      annotate("text", x = 3600, y = 0.1,
               label = away_team, color = away_color, size = 7) +
      annotate("text", x = 3200, y = 0.1,
               label = paste(" @",home_team), color = home_color, size=7) +
      scale_y_continuous(limits=c(0,1)) +
      geom_vline(xintercept = 900, linetype = "dashed", color= "black") +
      geom_vline(xintercept = 1800, linetype = "dashed",color= "black") +
      geom_vline(xintercept = 2700, linetype = "dashed", color= "black") +
      geom_vline(xintercept = 0, linetype = "dashed", color= "black") +
      labs(
        x = "Time Remaining (seconds)",
        y = "Win Probability",
        title = paste("Win Probability Chart"),
        subtitle = paste(home_team,"vs",away_team),
        caption = "Plot: @cfbscrapR | Data: CollegeFootballData API "
      ) + theme_bw(base_size = 16)
    return(p1)
  }
}
