# College Football Expected Points
`cfbpointsR` is an R package for working with CFB data. It is an R API wrapper around https://collegefootballdata.com/. It provides users the capability to get a plethora of endpoints, and supplement that data with additional information (Expected Points Added/Win Probability added). 

__Note:__ The API ingests data from ESPN as well as other sources. For details on those source, please go the website linked above. Sometimes there are inconsitences in the underlying data itself. Please report issues here or to https://collegefootballdata.com/. 

## Installation
You can install `cfbpointsR` from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("saiemgilani/cfbpointsR")
```

## Functions

### Scraping Data 
* ```cfb_pbp_data(year,week,team,epa_wpa)```: Extract PBP data (append EPA/WPA data)
* ```cfb_game_info(2018,team="Texas A&M")```: Get information from games (Multiple options, all year, one week, one team, specific conference)
* ```cfb_play_stats_player(game_id)```: Gets player info associated by play (Game id )
* ```cfb_rankings(2018,1)```: Historical CFB poll rankings at a specific week
* ```cfb_team(conference)```: List all teams in a given D1 conference
* ```cfb_recruiting(2018,team="Texas")```: CFB recruiting information (General Year, Position groups btw years)



### EPA/WPA

* ```calculate_epa <- function(clean_pbp_dat)```: Calculate EPA 
* ```create_wpa <- function(df)```: Calculate WPA 


### Plots
* ```plot_pbp_sequencing(df)```: Plot Play by Play Sequencing per drive 
* ```plot_wpa(df,away_color,home_color,winner="away")```: Plot WPA 

## EP/WP Model
You can find more detailed information and explanation on the EP/WP models in the `cfbpointsR-models` repository.   
Link: [cfbpointsR-models](https://github.com/saiemgilani/cfbpointsR-models "cfbpointsR-models GitHub repository") 
