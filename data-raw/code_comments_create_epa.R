###### CFB PBP DATA ####################################################################################
#-- Argument Assertion
#-- # Encode team parameter for URL if not NULL
#-- # Inputs: Season Type, Year, Week, Team, Play Type ID
#-- # Check for internet
#-- # Create the GET request and set response as res
#-- # Check the status of the response from the API
#-- # Create a data frame from the response
#-- # Call drive information using `cfb_drives(year = year, season_type = season_type, team = team, week = week)`
#-- ## Helper `clean_drive_info()`: 
#--    * Case when to create `pts_drive` 
#--    * Flag: `scoring` drive flag
#--    * Id: `drive_id` create `drive_id` column from `id` column from drives API end point
#-- # Add 'drive_' to column names of `clean_drive_df` (returned from `clean_drive_info()`)
#-- # Set drive_id to numeric, then join `clean_drive_df` to `raw_play_df` (returned from plays API end point)
#-- # Use setdiff to find the difference of the names of the play_df and the remove columns vector
#-- ## Helper `clean_pbp_dat()`:
#-- ## Helper `penalty_detection()`:

###### CREATE EPA FUNCTION ##############################################################################
## 1) prep_epa_df_before: Use map_dfr to prep before play model variables -> Make predictions 
## 2) prep_epa_df_after: Use map_dfr to prep after play model variables -> Make predictions
## 3) 
##
##
##
##
##
# constant vectors to be used again
# if you are trying to deal with multiple games at once
# then you have to get the after individually.
# ep_start
# if you are trying to deal with multiple games at once
# then you have to get the after individually.
# get post play stuff per game
# predict ep_after
# join together multiple dataframes back together
# to get ep_before and ep_after for plays
## kickoff plays
## calculate EP before at kickoff as what happens if it was a touchback
## 25 yard line in 2012 and onwards
## question for the class: where is the EPA on touchbacks being set to 0?

# **Due to ESPN data quality issues, some drives end on 3rd down that are listed as turnovers
# For turnover and punt plays make sure the ep_after is negative
# game end EP is 0
## scoring plays from here on out
# prep some variables for WPA, drop transformed columns






#---prep_epa_df_after----------
## Play type vectors
# define turnover on downs
# data is ordered
# End of Half check 
# Plays specifically defined as a turnover
# turnovers only occur on actual change of offense, but not scoring plays, and not at the end of half.
# Turnovers now capture downs, when there is a change of offense after a fourth down normal play.
#--New Down-----
##--Penalty Cases (new_down)-----
#### 8 cases with three T/F penalty flags
#### 4 cases in 1
#### offsetting penalties, no penalties declined, no 1st down by penalty (1 case)
#### offsetting penalties, penalty declined true, no 1st down by penalty
#### seems like it would be a regular play at that point (1 case, split in three)
#### only penalty declined true, same logic as prior (1 case, split in three)
#### no other penalty flags true, lead on down (1 case)
##--Scores, kickoffs, turnovers, defensive scores----
##--Regular Plays----
#### regular play 1st down
#### iterate to next down due to not meeting the yards to gain
#### turnover on downs
#--New Distance-----
##--Penalty cases (new_distance)
###--offsetting penalties, keep same distance
###--penalty first down conversions, 10 or to goal
###--penalty without first down conversion
##--normal plays
##--turnovers, defensive scores, scores, kickoffs

#--New Yardline----

#---prep_epa_df_before
## define minutes, raw_secs, Under_two, half, new_id, log_ydstogo, redefine down
## filter not NA down and raw_secs