# cfbscrapR 1.0.22

- ~~Fix conference parameters to match API (moved to abbreviation format).~~ Removed assertions for now, so users should be able to access conference data without issue, assuming the input argument is correct. May fortune favor your selection. 

- Add mgcv (>= v1.8.32) dependency and update WP model accordingly. Note on WPA: Kickoffs are problematic and our calculation algorithm does not appear to accomplishing what it needs to. We are working on this, aiming for a quick next version update centered around this. 

- Following play type renamings and merging:
  * Pass Interception Return --> Interception Return
  * Pass Interception --> Interception Return
  * Pass Interception Return Touchdown --> Interception Return Touchdown
  * Sack Touchdown --> Fumble Recovery (Opponent) Touchdown
  * Punt Touchdown ~ Punt Return Touchdown. 
  
- Update `rush_vec` and `pass_vec` regex definitions to be more precise on pulling rushing plays.

- Update definition of play_type definition for cleaning "Fumble Recovery (Opponent)" play types to actually distinguish between touchdowns and non-scoring opponent fumble recoveries (prior definition was combining the touchdowns into the non-scoring play type)

- Reduce the reach of the non-explicit rushing/passing touchdowns to be more careful about merging labels.

- Similarly, separated punt touchdowns into a specific type of offensive score where the punting team recovers a fumble and scores, all other `punt touchdowns` prior to this were punt return touchdowns. There is a specific Jalen Reagor (TCU) play where he pulls a Greg Reid and fumbles on the punt return only to recover the fumble and run it in for a 73 yard TD that is explicitly fixed.

- Add the following columns:
  - kickoff_onside
  - kickoff_fair_catch
  - kickoff_downed
  - punt_fair_catch
  - punt_downed
  - sack
  - int
  - int_td
  - completion
  - pass_attempt
  - target
  - pass_td
  - rush_td
  - safety
  
- add some return skeleton docs
- add column `drive_start_yardline` to the remove cols
- add parsing for kickoff safeties accounting for sign change

* Added [Jared Lee](https://twitter.com/JaredDLee)'s [animated win probability plot vignette](https://saiemgilani.github.io/cfbscrapR/articles/Animated_WP_Plotting.html) to the package documentation page
  ![Result](https://raw.githubusercontent.com/saiemgilani/cfbscrapR/master/man/figures/animated_wp.gif)
  - Contains important `add_player_cols()` function useful to parse existing play-by-play datasets and pull passer/rusher/receiver/etc. player names. 
* Added [Michael Egle](https://twitter.com/deceptivespeed_)'s [4th down tendency plot vignette](https://saiemgilani.github.io/cfbscrapR/articles/fourth_down_plot_tutorial.html) to the package documentation page

# cfbscrapR 1.0.21
- Fix conference parameters to match API (moved to abbreviation format).

# cfbscrapR 1.0.2
- Fix downs turnovers and several other discrepancies in EPA computations.

![EPA_YardsGained_cfbscrapR.png](https://i.imgur.com/Bw6VO90.png)

![EPA_YardsGained_cfbscrapR2.png](https://i.imgur.com/VYX12pZ.png)

- Fix majority of issues with win probability added.

![WPA_YardsGained_cfbscrapR.png](https://i.imgur.com/OFHTh9Y.jpg)

![WPA_YardsGained_cfbscrapR2.png](https://i.imgur.com/84zh9VY.jpg)

- Remove the [`add_betting_columns()`](https://saiemgilani.github.io/cfbscrapR/reference/add_betting_cols.html) function and the current betting win probability model from the [`cfb_pbp_data()`](https://saiemgilani.github.io/cfbscrapR/reference/cfb_pbp_data.html) function.
- Added [`cfb_ratings_fpi()`](https://saiemgilani.github.io/cfbscrapR/reference/cfb_ratings_fpi.html) function from @sabinanalytics's fork of the repository
- Added the [`cfb_metrics_espn_wp()`](https://saiemgilani.github.io/cfbscrapR/reference/cfb_metrics_espn_wp.html) function, courtesy of @mrcaseb
- Add [tests](https://github.com/saiemgilani/cfbscrapR/tree/master/tests/testthat) for a majority of the functions. This is the biggest behind the scenes change that will translate to more reliable functions. 
- Rename several function outputs from **camelCase to under_score** for consistency. Please adjust your scripts accordingly, apologies for the inconvenience.
- Remove `drives` parameter from [`cfb_pbp_data()`](https://saiemgilani.github.io/cfbscrapR/reference/cfb_pbp_data.html) function. For accessing drives information, please switch to the [`cfb_drives()`](https://saiemgilani.github.io/cfbscrapR/reference/cfb_drives.html) function. 
- For more complete summary of changes, see [Pull Request](https://github.com/saiemgilani/cfbscrapR/pull/5#issue-478275691)

# cfbscrapR 1.0.1
- bug fixes
# cfbscrapR 1.0.0

- Prep for CRAN  
- Full coverage of end-points from CFB Data  
- More parameter options for end points  
- Bug fixes on some functions  

# cfbscrapR 0.1.6

- Bug fix on pbp function  

# cfbscrapR 0.1.5

- Separate Recruiting Functions (Separate function for position groups)  
- Clean up Ranking function to properly return a value  
- Create Venue Function  

# cfbscrapR 0.1.4

Fixes include: 

- Ability to scrape 2015 (fix down issues on Kickoffs)
- Fix punt yard line @ end of play
- Proper turnover play indication
- Clarity on penalties. 

# cfbscrapR 0.1.3

This was a big update!

- New and Improved EP/WP models. 
- CV-LOSO for EP/WP models
- Fix the log_yards_to_goal to be the correct variable
- More consistent data fro API
- Timeout data available
- Better post play identification structure

# cfbscrapR 0.1.1 - 0.1.2

Some small fixes here and there

# cfbscrapR 0.1.0

Initial release of cfbscrapR with EPA/WPA models