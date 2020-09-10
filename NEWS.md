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