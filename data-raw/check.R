library(cfbscrapR)
library(tidyverse)

off_TD = c(
  "Passing Touchdown",
  "Rushing Touchdown",
  "Field Goal Good",
  "Pass Reception Touchdown",
  "Fumble Recovery (Own) Touchdown",
  "Punt Touchdown"
)
def_TD = c(
  "Blocked Punt Touchdown",
  "Fumble Return Touchdown",
  "Defensive 2pt Conversion",
  "Interception Return Touchdown",
  "Safety",
  "Missed Field Goal Return Touchdown",
  "Punt Touchdown",
  "Punt Return Touchdown",
  "Blocked Field Goal Touchdown",
  "Fumble Recovery (Opponent) Touchdown",
  "Pass Interception Return Touchdown",
  "Sack Touchdown",
  "Uncategorized Touchdown"
)
kickoff = c(
  "Kickoff",
  "Kickoff Return (Offense)",
  "Kickoff Return Touchdown",
  "Kickoff Touchdown"
)
turnover_vec = c(
  "Blocked Field Goal",
  "Blocked Field Goal Touchdown",
  "Blocked Punt",
  "Blocked Punt Touchdown",
  "Field Goal Missed",
  "Missed Field Goal Return",
  "Missed Field Goal Return Touchdown",
  "Fumble Recovery (Opponent)",
  "Fumble Recovery (Opponent) Touchdown",
  "Fumble Return Touchdown",
  "Defensive 2pt Conversion",
  "Interception",
  "Interception Return Touchdown",
  "Pass Interception Return",
  "Pass Interception Return Touchdown",
  "Punt",
  "Punt Touchdown",
  "Punt Return Touchdown",
  "Sack Touchdown",
  "Uncategorized Touchdown"
)

pull_all_pbp_weeks <- function(yr=2019, start_wk = 1, end_wk=1, epa = TRUE){
  fd <- data.frame()
  for(i in start_wk:end_wk){
    print(i)
    pbp <- cfb_pbp_data(year = yr, season_type = 'regular', week = i, epa_wpa = epa)
    fd <- rbind(pbp, fd)
    Sys.sleep(5)
  }
  if(epa){
    filename<-paste0('pbp_stats_',yr,'_week_',start_wk,'_to_',end_wk,'.csv')
  }else{
    filename<-paste0('pbp_plain_',yr,'_week_',start_wk,'_to_',end_wk,'.csv')
  }
  write_csv(fd, filename)
  
  return(fd)
}

pull_all_pbp_year <- function(yr=2019, epa = FALSE){
  fd <- data.frame()
  pbp <- cfb_pbp_data(year = yr, season_type = 'regular', week = NULL, epa_wpa = epa)
  fd <- rbind(fd, pbp)
  Sys.sleep(3)
  if(epa){
    filename<-paste0('pbp_stats_',yr,'.csv')
  }else{
    filename<-paste0('pbp_plain_',yr,'.csv')
  }
  write_csv(fd, filename)
  return(fd)
}

year_for_pull = 2019
#--- Play by Play Data Pull/Save and Pre-processing -----------------------------------------------
# start_time <- proc.time()
# df_plain <- pull_all_pbp_weeks(yr = year_for_pull, start_wk = 1, end_wk=14, epa=FALSE)
# end_time <- proc.time()
# season_run <- end_time - start_time
# season run no EPA
# user  system elapsed 
# 7.69    1.05  130.61 
# print(season_run)
# df_plain <- read.csv('pbp_plain_2019.csv')

# yr_epa_start_time <- proc.time()
df_yr <- pull_all_pbp_year(yr = year_for_pull, epa=TRUE)
# yr_epa_season_run <- proc.time() - yr_epa_start_time
# print(yr_epa_season_run)
#   user  system elapsed
# 475.39    1.60  498.96

# epa_start_time <- proc.time()
# df_weeks <- pull_all_pbp_weeks(yr = year_for_pull, epa=TRUE)
# epa_season_run <- proc.time() - epa_start_time
# print(epa_season_run)
# #  user  system elapsed 
# # 45.58    0.29   54.04 
# start_time <- proc.time()
# df_plain <- pull_all_pbp_weeks(yr = year_for_pull, epa=FALSE)
# season_run <- proc.time() - start_time
# print(season_run)
# # user  system elapsed 
# # 0.70    0.08    8.86 


# df_diff <- df_plain %>% 
#   filter(!(id_play %in% df_weeks$id_play))

df_yr <- read.csv('pbp_stats_2019.csv')
# df <- create_wpa_naive(df)
df2 <- df_yr %>% 
  filter((play_type %in% off_TD)|
         (play_type %in% def_TD)) %>%
  mutate(
    wpa = round(wpa, digits = 4),
    wpa_base = round(wpa_base, digits = 4),
    wp_before = round(wp_before, digits = 4),
    lead_wp_before = round(lead_wp_before, digits = 4),
    def_wp_before = round(def_wp_before, digits = 4),
    home_wp_before = round(home_wp_before, digits = 4),
    away_wp_before = round(away_wp_before, digits = 4),
    home_wp_post = round(home_wp_post, digits = 4),
    away_wp_post = round(away_wp_post, digits = 4),
    home_wpa = home_wp_post - home_wp_before,
    home_wpa = round(home_wpa, digits = 4),
    away_wpa = away_wp_post - away_wp_before,
    away_wpa = round(away_wpa, digits = 4)
  ) %>% 
  select(game_id, 
         game_play_number, 
         drive_number, 
         drive_play_number, 
         play_type, 
         play_text,
         down,
         distance,
         yards_gained,
         yards_to_goal,
         score_diff,
         missing_yard_flag,
         first_by_penalty,
         first_by_yards,
         first_by_poss,
         td_play,
         off_td_play,
         def_td_play,
         kick_play,
         punt_play,
         rush_vec,
         pass_vec, 
         sack_vec,
         fumble_vec,
         change_of_poss,
         turnover,
         downs_turnover,
         wpa_change,
         wpa_base, 
         wpa, 
         wp_before,
         lead_wp_before, 
         def_wp_before, 
         home_wpa,
         home_wp_before, 
         home_wp_post,
         away_wpa,
         away_wp_before, 
         away_wp_post, 
         EPA, 
         ep_before, 
         ep_after,         
         offense_score, 
         defense_score,
         everything())

hist(df2$wpa)

df3<-df2 %>% 
  group_by(play_type) %>% 
  summarize(
    wpa_avg = mean(wpa,na.rm = TRUE),
    plays = n()
  )
kickoff = c(
  "Kickoff",
  "Kickoff Return (Offense)",
  "Kickoff Return Touchdown",
  "Kickoff Touchdown"
)
df4 <- df_yr %>% 
  filter(play_type %in% kickoff) %>% 
  mutate(
    wpa = round(wpa, digits = 4),
    wpa_base = round(wpa_base, digits = 4),
    wp_before = round(wp_before, digits = 4),
    lead_wp_before = round(lead_wp_before, digits = 4),
    def_wp_before = round(def_wp_before, digits = 4),
    home_wp_before = round(home_wp_before, digits = 4),
    away_wp_before = round(away_wp_before, digits = 4),
    home_wp_post = round(home_wp_post, digits = 4),
    away_wp_post = round(away_wp_post, digits = 4),
    home_wpa = home_wp_post - home_wp_before,
    home_wpa = round(home_wpa, digits = 4),
    away_wpa = away_wp_post - away_wp_before,
    away_wpa = round(away_wpa, digits = 4)
  ) %>% 
  select(game_id, 
         game_play_number, 
         drive_number, 
         drive_play_number, 
         play_type, 
         play_text,
         down,
         distance,
         yards_gained,
         yards_to_goal,
         score_diff,
         missing_yard_flag,
         first_by_penalty,
         first_by_yards,
         first_by_poss,
         td_play,
         off_td_play,
         def_td_play,
         kick_play,
         punt_play,
         rush_vec,
         pass_vec, 
         sack_vec,
         fumble_vec,
         change_of_poss,
         turnover,
         downs_turnover,
         wpa_change,
         wpa_base, 
         wpa, 
         wp_before,
         lead_wp_before, 
         def_wp_before, 
         home_wpa,
         home_wp_before, 
         home_wp_post,
         away_wpa,
         away_wp_before, 
         away_wp_post, 
         EPA, 
         ep_before, 
         ep_after,         
         offense_score, 
         defense_score,
         everything())
hist(df4$wpa)

df5<-df4 %>% 
  group_by(play_type) %>% 
  summarize(
    wpa_avg = mean(wpa, na.rm = TRUE),
    plays = n()
  )


df6 <- df_yr %>% 
  filter(play_type %in% turnover_vec) %>% 
  mutate(
    wpa = round(wpa, digits = 4),
    wpa_base = round(wpa_base, digits = 4),
    wp_before = round(wp_before, digits = 4),
    lead_wp_before = round(lead_wp_before, digits = 4),
    def_wp_before = round(def_wp_before, digits = 4),
    home_wp_before = round(home_wp_before, digits = 4),
    away_wp_before = round(away_wp_before, digits = 4),
    home_wp_post = round(home_wp_post, digits = 4),
    away_wp_post = round(away_wp_post, digits = 4),
    home_wpa = home_wp_post - home_wp_before,
    home_wpa = round(home_wpa, digits = 4),
    away_wpa = away_wp_post - away_wp_before,
    away_wpa = round(away_wpa, digits = 4)
  ) %>% 
  select(game_id, 
         game_play_number, 
         drive_number, 
         drive_play_number, 
         play_type, 
         play_text,
         down,
         distance,
         yards_gained,
         yards_to_goal,
         score_diff,
         missing_yard_flag,
         first_by_penalty,
         first_by_yards,
         first_by_poss,
         td_play,
         off_td_play,
         def_td_play,
         kick_play,
         punt_play,
         rush_vec,
         pass_vec, 
         sack_vec,
         fumble_vec,
         change_of_poss,
         turnover,
         downs_turnover,
         wpa_change,
         wpa_base, 
         wpa, 
         wp_before,
         lead_wp_before, 
         def_wp_before, 
         home_wpa,
         home_wp_before, 
         home_wp_post,
         away_wpa,
         away_wp_before, 
         away_wp_post, 
         EPA, 
         ep_before, 
         ep_after,         
         offense_score, 
         defense_score,
         everything())

hist(df6$wpa)

df7<-df6 %>% 
  group_by(play_type) %>% 
  summarize(
    wpa_avg = mean(wpa,na.rm = TRUE),
    plays = n()
  )


df_nd <- cfb_pbp_data(year=2019, week = 3, team='Notre Dame',
                      season_type='regular',
                      epa_wpa = TRUE)

df_nd <- df_nd %>% 
  mutate(
    wpa = round(wpa, digits = 4),
    wpa_change = round(wpa_change, digits = 4),
    wpa_base = round(wpa_base, digits = 4),
    wp_before = round(wp_before, digits = 4),
    lead_wp_before = round(lead_wp_before, digits = 4),
    def_wp_before = round(def_wp_before, digits = 4),
    home_wp_before = round(home_wp_before, digits = 4),
    away_wp_before = round(away_wp_before, digits = 4),
    home_wp_post = round(home_wp_post, digits = 4),
    away_wp_post = round(away_wp_post, digits = 4),
    home_wpa = home_wp_post - home_wp_before,
    home_wpa = round(home_wpa, digits = 4),
    away_wpa = away_wp_post - away_wp_before,
    away_wpa = round(away_wpa, digits = 4)
  ) %>% 
  select(game_id, 
         game_play_number, 
         drive_number,         
         down,
         distance,
         yards_gained,
         yards_to_goal,
         offense_play,
         defense_play,
         
         play_text,
         play_type, 
         play_after_turnover,
         score_diff,
         score_diff_start,
         scores,
         ExpScoreDiff,
         ExpScoreDiff_Time_Ratio,
         change_of_poss,
         
         wpa, 
         wpa_change,
         wpa_base, 
         wp_before,
         lead_wp_before, 
         punt_play,
         kickoff_play,
         def_td_play,
         def_wp_before, 
         drive_play_number,
         off_td_play,

         touchdown,
         turnover_vec,
         downs_turnover,
         score_diff_start,
         home_wpa,
         home_wp_before, 
         home_wp_post,
         away_wpa,
         away_wp_before, 
         away_wp_post, 
         td_play,
         kickoff_tb,
         kick_play,
         punt_tb,
         punt,
         EPA, 
         ep_before, 
         ep_after,    
         missing_yard_flag,
         drive_play_number, 
         firstD_by_poss,
         firstD_by_penalty,
         firstD_by_yards,
         first_by_penalty,
         first_by_yards,
         rush_vec,
         pass_vec, 
         sack_vec,
         fumble_vec,
         offense_score, 
         defense_score,
         everything())