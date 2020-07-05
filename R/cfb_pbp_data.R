#' Extract CFB (D-I) Play by Play Data - For plays
#'
#' Extracts raw game by game data. Data comes from https://api.collegefootballdata.com
#'
#' @param season_type Select Season Type (regular, postseason, both)
#' @param year Select year, (example: 2018)
#' @param week Select week, this is optional (also numeric)
#' @param team Select team name (example: Texas, Texas A&M, Clemson)
#' @param play_type Select play type (example: see the cfb_play_type_df)
#' @param drive Enter anything, and you will get general drive information
#' @param epa_wpa Logical parameter (TRUE/FALSE) to return the Expected Points Added/Win Probability Added variables
#'
#' @keywords Play-by-Play
#' @import stringr
#' @import dplyr
#' @import tidyr
#' @importFrom jsonlite "fromJSON"
#' @importFrom utils "URLencode"
#' @importFrom utils "globalVariables"
#' @export
#'

cfb_pbp_data <- function(year,
                         season_type = 'regular',
                         week = 1,
                         team = NULL,
                         play_type = NULL,
                         drive=NULL,
                         epa_wpa=FALSE) {
  #require(jsonlite)
  # year <- 2019
  # week <- 12
  # team <- 'Baylor'
  # epa_wpa <- TRUE
  # season_type <- 'regular'
  # drive <- NULL
  # play_type <- NULL

  options(stringsAsFactors = FALSE)

  if (!is.null(play_type)) {
    text <- play_type %in% cfbscrapR::cfb_play_type_df$text
    abbr <- play_type %in% cfbscrapR::cfb_play_type_df$abbreviation
    pt <-
      assert_that((text | abbr) == TRUE, 
                  msg = "Incorrect play type selected, please look at the available options in the Play Type DF.")
    if (text) {
      pt_id = cfbscrapR::cfb_play_type_df$id[which(cfbscrapR::cfb_play_type_df$text == play_type)]
    } else{
      pt_id = cfbscrapR::cfb_play_type_df$id[which(cfbscrapR::cfb_play_type_df$abbreviation == play_type)]
    }
  }
  ## Inputs
  ## Year, Week, Team
  if(is.null(drive)){
    play_base_url <- paste0("https://api.collegefootballdata.com/plays?seasonType=",season_type,'&')
  } else{
    play_base_url <- paste0("https://api.collegefootballdata.com/drives?seasonType=",season_type,'&')
  }

  if (is.null(play_type) & is.null(team)) {
    # no play type, no team
    full_url <- paste0(play_base_url,
                       "year=", year,
                       "&week=", week)
  } else{
    # no team, play_type
    if (is.null(play_type)) {
      full_url <-
        paste0(play_base_url,
               "year=", year,
               "&week=", week,
               "&team=", URLencode(team, reserved = TRUE))
    } else if (is.null(team)) {
      # no team, play_type
      full_url <-
        paste0(play_base_url,
              "year=", year,
              "&week=", week,
              "&team=", URLencode(team, reserved = TRUE))
    } else{
      # team & play type
      full_url <-
        paste0(play_base_url,
              "year=", year,
              "&week=", week,
              "&team=", URLencode(team, reserved = TRUE),
              "&playType=", pt_id)
    }
  }

  raw_play_df <- fromJSON(full_url)
  raw_play_df <- do.call(data.frame, raw_play_df)
  if(nrow(raw_play_df)==0){
    warning("Most likely a bye week, the data pulled from the API was empty. Returning nothing
            for this one week or team.")
    return(NULL)
  }

  play_df <- raw_play_df
  ## call/drive information
  if(is.null(drive)){
    drive_info = cfb_pbp_data(
      year,
      season_type = season_type,
      team = team,
      week = week,
      drive = TRUE
    )

    clean_drive_df = clean_drive_info(drive_info)

    colnames(clean_drive_df) <- paste0("drive_",colnames(clean_drive_df))
    play_df = play_df %>%
      mutate(drive_id = as.numeric(.data$drive_id)) %>%
      left_join(clean_drive_df,
                by = c("drive_id" = "drive_drive_id",
                       "game_id" = "drive_game_id"),
                suffix = c("_play", "_drive"))

    rm_cols = c(
      'drive_game_id',
      'drive_offense_conference',
      'drive_defense_conference',
      'drive_offense',
      'offense_conferencec',
      'drive_defense',
      'defense_conference',
      'drive_id_drive',
      'drive_start_time.minutes',
      'drive_start_time.seconds',
      'drive_start_period',
      'drive_end_period',
      'drive_end_yardline',
      'drive_end_time.minutes',
      'drive_end_time.seconds',
      'drive_elapsed.seconds',
      'drive_elapsed.minutes',
      'drive_plays'
    )


    play_df = play_df %>%
      select(setdiff(names(play_df), rm_cols)) %>%
      rename(drive_pts = .data$drive_pts_drive,
             drive_result = .data$drive_drive_result,
             id_play= .data$id,
             offense_play= .data$offense,
             defense_play= .data$defense)
    if(epa_wpa){
      if(year<=2005) {
        warning(
          "Data Quality prior to 2005 is not as consistent. This can affect the EPA/WPA values, proceed with caution."
        )
      }
      play_df = clean_pbp_dat(play_df)
      g_ids = sort(unique(play_df$game_id))
      play_df = purrr::map_dfr(g_ids,
                               function(x) {
                                 play_df %>%
                                   filter(.data$game_id == x) %>%
                                   add_timeout_cols() %>% 
                                   add_betting_cols(g_id = x, yr=year)
                               })

      play_df = create_epa(play_df)
      play_df = create_wpa_betting(play_df)
      play_df = create_wpa_naive(play_df)
      play_df = play_df %>%
        group_by(.data$drive_id) %>%
        arrange(.data$new_id, .by_group=T) %>%
        ungroup()
    }
  }
  play_df <- as.data.frame(play_df)
  return(play_df)
}




#' Clean Play-by-Play data
#' Cleans Play-by-Play data pulled from the API's raw game data
#'
#' @param clean_pbp_dat (\emph{data.frame} required): Performs data cleansing on Play-by-Play DataFrame, as pulled from `cfb_pbp_dat()`
#' @keywords internal
#' @import stringr
#' @import dplyr
#' @import tidyr
#'

clean_pbp_dat <- function(raw_df) {
  ## vectors
  #-- touchdowns----
  td_e = str_detect(raw_df$play_text, "TD|Touchdown|TOUCHDOWN|touchdown")

  #-- kicks/punts----
  kick_vec = str_detect(raw_df$play_text, "KICK") &
    !is.na(raw_df$play_text)
  punt_vec = str_detect(raw_df$play_text, "Punt|punt") &
    !is.na(raw_df$play_text)
  #-- fumbles----
  fumble_vec = str_detect(raw_df$play_text, "fumble")
  #-- pass/rush----
  rush_vec = raw_df$play_type == "Rush"
  pass_vec = raw_df$play_type == "Pass Reception" |
    raw_df$play_type == "Pass Completion" |
    raw_df$play_type == "Pass"

  #-- sacks----
  #- only want non-safety sacks, otherwise would be an additional condition----
  sack_vec = raw_df$play_type == "Sack" |
    raw_df$play_type == "Sack Touchdown"

  #-- change of possession
  poss_change_vec = raw_df$change_of_poss == 1

  ## Fix strip-sacks to fumbles----
  raw_df$play_type[fumble_vec & sack_vec &
                     poss_change_vec & !td_e] <- "Fumble Recovery (Opponent)"

  raw_df$play_type[fumble_vec &
                     sack_vec & td_e] <- "Fumble Recovery (Opponent) Touchdown"

  ## touchdown check for plays where touchdown is not listed in the play_type----
  td_check = !str_detect(raw_df$play_type, "Touchdown")

  #-- fix kickoff fumble return TDs----
  raw_df$play_type[kick_vec & fumble_vec & td_e & td_check] <-
    paste0(raw_df$play_type[kick_vec &
                              fumble_vec &
                              td_e & td_check], " Touchdown")
  #-- fix punt return TDs----
  raw_df$play_type[punt_vec & td_e & td_check] <-
    paste0(raw_df$play_type[punt_vec &
                              td_e & td_check], " Touchdown")
  #-- fix rush/pass tds that aren't explicit----
  raw_df$play_type[td_e & rush_vec] = "Rushing Touchdown"
  raw_df$play_type[td_e & pass_vec] = "Passing Touchdown"

  #-- fix duplicated TD play_type labels----
  pun_td_sq = (raw_df$play_type == "Punt Touchdown Touchdown")
  raw_df$play_type[pun_td_sq] <- "Punt Touchdown"
  fum_td_sq = (raw_df$play_type == "Fumble Return Touchdown Touchdown")
  raw_df$play_type[fum_td_sq] == "Fumble Return Touchdown"
  rush_td_sq = (raw_df$play_type == "Rushing Touchdown Touchdown")
  raw_df$play_type[rush_td_sq] == "Rushing Touchdown"

  ## penalty detection-----
  #-- penalty in play text----
  pen_text = str_detect(raw_df$play_text, "Penalty|penalty|PENALTY")
  #-- declined in play text----
  pen_declined_text = str_detect(raw_df$play_text,"declined|Declined|DECLINED")
  #--NO PLAY in play text----
  pen_no_play_text = str_detect(raw_df$play_text,"no play|No Play|NO PLAY")
  #--off-setting in play text----
  pen_offset_text = str_detect(raw_df$play_text,"off-setting")|
    str_detect(raw_df$play_text,"Off-Setting")|
    str_detect(raw_df$play_text,"OFF-SETTING")
  pen_1st_down_text = str_detect(raw_df$play_text,"1st down")|
    str_detect(raw_df$play_text,"1st Down")|
    str_detect(raw_df$play_text,"1st DOWN")|
    str_detect(raw_df$play_text,"1ST Down")|
    str_detect(raw_df$play_text,"1ST down")|
    str_detect(raw_df$play_text,"1ST DOWN")

  #-- penalty play_types
  pen_type = raw_df$play_type == "Penalty"  | raw_df$play_type == "penalty"

  #-- penalty_flag T/F flag conditions
  raw_df$penalty_flag = F
  raw_df$penalty_flag[pen_type] <- T
  raw_df$penalty_flag[pen_text] <- T
  #-- penalty_declined T/F flag conditions
  raw_df$penalty_declined = F
  raw_df$penalty_declined[pen_text & pen_declined_text] <- T
  raw_df$penalty_declined[pen_type & pen_declined_text] <- T
  #-- penalty_no_play T/F flag conditions
  raw_df$penalty_no_play = F
  raw_df$penalty_no_play[pen_text & pen_no_play_text] <- T
  raw_df$penalty_no_play[pen_type & pen_no_play_text] <- T
  #-- penalty_offset T/F flag conditions
  raw_df$penalty_offset = F
  raw_df$penalty_offset[pen_text & pen_offset_text] <- T
  raw_df$penalty_offset[pen_type & pen_offset_text] <- T
  #-- penalty_1st_conv T/F flag conditions
  raw_df$penalty_1st_conv = F
  raw_df$penalty_1st_conv[pen_text & pen_1st_down_text] <- T
  raw_df$penalty_1st_conv[pen_type & pen_1st_down_text] <- T

  ## kickoff down adjustment
  raw_df = raw_df %>%
    mutate(down = ifelse(.data$down == 5 & str_detect(.data$play_type, "Kickoff"), 1, .data$down),
           down = ifelse(.data$down == 5 & str_detect(.data$play_type, "Penalty"),1 , .data$down),
           half = ifelse(.data$period <= 2, 1, 2))

  return(raw_df)
}


#' Add Timeout columns
#' This is only for DI-FBS football
#'
#' @param play_df (\emph{data.frame} required): Play-By-Play data.frame as pulled from `cfb_pbp_dat()` and `clean_pbp_dat()`)
#'
#' @keywords internal
#' @importFrom stringi "stri_trans_general"
#' @import stringr
#' @import dplyr
#' @import tidyr
#'

add_timeout_cols <- function(play_df) {
  play_df<- play_df %>%
    mutate(home = stringi::stri_trans_general(.data$home,id="Latin-ASCII"),
           away = stringi::stri_trans_general(.data$away,id="Latin-ASCII"))
  pbp_df <- play_df %>%
    group_by(.data$game_id, .data$half) %>%
    arrange(.data$id_play) %>%
    mutate(
      timeout_called = ifelse(.data$play_type %in% c("Timeout"), 1, 0),
      timeout_team = ifelse(
        .data$play_type %in% c("Timeout"),
        ifelse(
          !is.na(str_extract(.data$play_text, "timeout (.+)")),
          str_extract(.data$play_text, "timeout (.+)"),
          str_extract(.data$play_text, "Timeout (.+)")
        ),
        NA
      )
    ) %>%
    mutate(timeout_team = str_remove(.data$timeout_team, ",(.+)")) %>%
    mutate(
      timeout_team = stringi::stri_trans_general(str_to_lower(str_remove(.data$timeout_team, "Timeout ")),id="Latin-ASCII"),
      timeout_team = case_when(
        .data$timeout_team == "af" ~ "air force",
        .data$timeout_team == "air force academy" ~ "air force",
        .data$timeout_team == "air force falcons" ~ "air force",
        .data$timeout_team == "arkansas st." ~ "arkansas state",
        .data$timeout_team == "asu" ~ "arkansas state",
        .data$timeout_team == "ball state cardinals" ~ "ball state",
        .data$timeout_team == "bgsu" ~ "bowling green",
        .data$timeout_team == "brigham young" ~ "byu",
        .data$timeout_team == "byu cougars" ~ "byu",
        .data$timeout_team == "centrl michigan" ~ "central michigan",
        .data$timeout_team == "cmu" ~ "central michigan",
        .data$timeout_team == "coastl carolina" ~ "coastal carolina",
        .data$timeout_team == "cs" ~ "colorado state",
        .data$timeout_team == "eastrn michigan" ~ "eastern michigan",
        .data$timeout_team == "ecu" ~ "east carolina",
        .data$timeout_team == "emu" ~ "eastern michigan",
        .data$timeout_team == "fau" ~ "florida atlantic",
        .data$timeout_team == "fiu" ~ "florida international",
        .data$timeout_team == "fla atlantic" ~ "florida atlantic",
        .data$timeout_team == "florida intl" ~ "florida international",
        .data$timeout_team == "floridainternational" ~ "florida international",
        .data$timeout_team == "fresno st." ~ "fresno state",
        .data$timeout_team == "ga southern" ~ "georgia southern",
        .data$timeout_team == "gsu" ~ "georgia state",
        .data$timeout_team == "hawai`i" ~ "hawai'i",
        .data$timeout_team == "hawaii" ~ "hawai'i",
        .data$timeout_team == "iowa hawkeyes" ~ "iowa",
        .data$timeout_team == "las vegas" ~ "unlv",
        .data$timeout_team == "latech" ~ "louisiana tech",
        .data$timeout_team == "louisiana lafayette" ~ "louisiana",
        .data$timeout_team == "louisiana state" ~ "lsu",
        .data$timeout_team == "louisiana-lafayette" ~ "louisiana",
        .data$timeout_team == "massachusetts" ~ "umass",
        .data$timeout_team == "miami (fla.)" ~ "miami",
        .data$timeout_team == "miami (ohio)" ~ "miami (oh)",
        .data$timeout_team == "miami fl" ~ "miami",
        .data$timeout_team == "miami florida" ~ "miami",
        .data$timeout_team == "miami oh" ~ "miami (oh)",
        .data$timeout_team == "miami ohio" ~ "miami (oh)",
        .data$timeout_team == "middle tenn st" ~ "middle tennessee",
        .data$timeout_team == "minnesota gophers" ~ "minnesota",
        .data$timeout_team == "mississippi" ~ "ole miss",
        .data$timeout_team == "mt" ~ "middle tennessee",
        .data$timeout_team == "n.c. state" ~ "nc state",
        .data$timeout_team == "NA" ~ "",
        .data$timeout_team == "niu" ~ "northern illinois",
        .data$timeout_team == "nm state" ~ "new mexico state",
        .data$timeout_team == "nmsu" ~ "new mexico state",
        .data$timeout_team == "north carolina st" ~ "nc state",
        .data$timeout_team == "northernil" ~ "northern illinois",
        .data$timeout_team == "ohio bobcats" ~ "ohio",
        .data$timeout_team == "ohio university" ~ "ohio",
        .data$timeout_team == "olddominion" ~ "old dominion",
        .data$timeout_team == "ole ole miss" ~ "ole miss",
        .data$timeout_team == "oregon st." ~ "oregon state",
        .data$timeout_team == "rice owls" ~ "rice",
        .data$timeout_team == "san jose st" ~ "san jose state",
        .data$timeout_team == "san jose state" ~ "san jose state",
        .data$timeout_team == "sj" ~ "san jose state",
        .data$timeout_team == "sjsu" ~ "san jose state",
        .data$timeout_team == "smu mustangs" ~ "smu",
        .data$timeout_team == "southern  miss" ~ "southern mississippi",
        .data$timeout_team == "southern cal" ~ "usc",
        .data$timeout_team == "southern methodist university" ~ "smu",
        .data$timeout_team == "temple owls" ~ "temple",
        .data$timeout_team == "temple university" ~ "temple",
        .data$timeout_team == "texas el paso" ~ "utep",
        .data$timeout_team == "texas state university" ~ "texas state",
        .data$timeout_team == "texassan" ~ "ut san antonio",
        .data$timeout_team == "texas-san antonio" ~ "ut san antonio",
        .data$timeout_team == "tls" ~ "tulsa",
        .data$timeout_team == "troy university" ~ "troy",
        .data$timeout_team == "tulane green wave" ~ "tulane",
        .data$timeout_team == "uh" ~ "hawai'i",
        .data$timeout_team == "ui" ~ "idaho",
        .data$timeout_team == "ul" ~ "louisiana",
        .data$timeout_team == "ul lafayette" ~ "louisiana",
        .data$timeout_team == "ul monroe" ~ "louisiana monroe",
        .data$timeout_team == "ull" ~ "louisiana",
        .data$timeout_team == "ulm" ~ "louisiana monroe",
        .data$timeout_team == "university of idaho" ~ "idaho",
        .data$timeout_team == "usa" ~ "south alabama",
        .data$timeout_team == "usf" ~ "south florida",
        .data$timeout_team == "usm" ~ "southern mississippi",
        .data$timeout_team == "usu" ~ "utah state",
        .data$timeout_team == "utsa" ~ "ut san antonio",
        .data$timeout_team == "washington st." ~ "washington state",
        .data$timeout_team == "west virginia university" ~ "west virginia",
        .data$timeout_team == "westrn kentucky" ~ "western kentucky",
        .data$timeout_team == "westrn michigan" ~ "western michigan",
        .data$timeout_team == "wfu" ~ "wake forest",
        .data$timeout_team == "wku" ~ "western kentucky",
        .data$timeout_team == "wmu" ~ "western michigan",
        .data$timeout_team == "wsu" ~ "washington state",
        .data$timeout_team == "wyoming cowboys" ~ "wyoming",
        TRUE ~ .data$timeout_team
      ),
      home_timeout = ifelse(is.na(.data$timeout_team), 0,
                            ifelse(
                              str_detect(str_to_lower(.data$home), fixed(.data$timeout_team)) == TRUE, 1, 0
                            )),
      away_timeout = ifelse(is.na(.data$timeout_team), 0,
                            ifelse(
                              str_detect(str_to_lower(.data$away), fixed(.data$timeout_team)) == TRUE, 1, 0
                            )),
      off_timeouts_rem_before = NA,
      def_timeouts_rem_before = NA,
      off_timeouts_rem_after = NA,
      def_timeouts_rem_after = NA,
      home_timeouts_rem_before = NA,
      away_timeouts_rem_before = NA,
      home_timeouts_rem_after = NA,
      away_timeouts_rem_after = NA
    ) %>%
    mutate(
      home_timeout =
        case_when(
          .data$timeout_called == 1 & .data$home_timeout == 1 & .data$away_timeout == 1 ~
            ifelse(is.na(.data$timeout_team), 0,
                   ifelse(
                     str_detect(str_to_lower(.data$home),
                                paste0("^", .data$timeout_team, "$")) ==
                       TRUE, 1, 0
                   )),
          TRUE ~ .data$home_timeout
        ),
      away_timeout =
        case_when(
          .data$timeout_called == 1 & .data$home_timeout == 1 & .data$away_timeout == 1 ~
            ifelse(is.na(.data$timeout_team), 0,
                   ifelse(
                     str_detect(str_to_lower(.data$away),
                                paste0("^", .data$timeout_team, "$")) ==
                       TRUE, 1, 0
                   )),
          TRUE ~ .data$away_timeout
        )
    ) %>%
    mutate(
      home_timeouts_rem_after = 3 - cumsum(.data$home_timeout),
      away_timeouts_rem_after = 3 - cumsum(.data$away_timeout),
      home_timeouts_rem_before = ifelse(
        !is.na(lag(.data$home_timeouts_rem_after, order_by = .data$id_play)),
        lag(.data$home_timeouts_rem_after, order_by = .data$id_play),
        3
      ),
      away_timeouts_rem_before = ifelse(
        !is.na(lag(.data$away_timeouts_rem_after, order_by = .data$id_play)),
        lag(.data$away_timeouts_rem_after, order_by = .data$id_play),
        3
      ),
      off_timeouts_rem_after = ifelse(
        .data$offense_play == .data$home,
        .data$home_timeouts_rem_after,
        .data$away_timeouts_rem_after
      ),
      def_timeouts_rem_after = ifelse(
        .data$defense_play == .data$home,
        .data$home_timeouts_rem_after,
        .data$away_timeouts_rem_after
      ),
      off_timeouts_rem_before = ifelse(
        .data$offense_play == .data$home,
        .data$home_timeouts_rem_before,
        .data$away_timeouts_rem_before
      ),
      def_timeouts_rem_before = ifelse(
        .data$defense_play == .data$home,
        .data$home_timeouts_rem_before,
        .data$away_timeouts_rem_before
      )
    ) %>% ungroup()

  return(pbp_df)
}

#' Clean Drive Information
#' Cleans CFB (D-I) Drive-By-Drive Data to create `pts_drive` column
#'
#' @param drive_df (\emph{data.frame} required) Drive DataFrame pulled from API
#' @keywords Drive-by-Drive
#' @import stringr
#' @import dplyr
#' @import tidyr
#'
#'

clean_drive_info <- function(drive_df){

  clean_drive = drive_df %>%
    mutate(
      pts_drive = case_when(
        str_detect(.data$drive_result,"TD") ~ 7,
        #str_detect(drive_result,"FG") ~ 3,
        str_detect(.data$drive_result,"SF") ~ -2,
        .data$drive_result == 'FG GOOD' ~ 3,
        .data$drive_result == 'FG' ~ 3,
        .data$drive_result == 'MISSED FG TD' ~ -7,
        .data$drive_result == 'KICKOFF RETURN TD' ~ -7,
        .data$drive_result == 'END OF HALF TD' ~ 7,
        .data$drive_result == "END OF GAME TD" ~ 7,
        .data$drive_result == 'PUNT RETURN TD' ~ -7,
        .data$drive_result == 'PUNT TD' ~ -7,
        .data$drive_result == 'INT TD' ~ -7,
        .data$drive_result == 'INT RETURN TOUCH' ~ -7,
        .data$drive_result == 'FUMBLE RETURN TD' ~ -7,
        .data$drive_result == 'FUMBLE TD' ~ -7,
        .data$drive_result == 'DOWNS TD' ~ -7,
        TRUE ~ 0),
      scoring = ifelse(.data$pts_drive!=0, TRUE, .data$scoring)) %>%
    mutate(drive_id = as.numeric(.data$id)) %>%
    arrange(.data$game_id, .data$drive_id)

  return(clean_drive)
}

#' Add Betting columns
#' This is only for DI-FBS football
#'
#' @param play_df (\emph{data.frame} required): Play-By-Play data.frame as pulled from `cfb_pbp_dat()` and `clean_pbp_dat()`)
#' @param g_id (\emph{Integer} optional): Game ID filter for querying a single game
#' Can be found using the `cfb_game_info()` function
#' @param yr (\emph{Integer} optional): Select year (example: 2018)
#' @keywords internal
#' @import stringr
#' @import dplyr
#' @import tidyr
#'
add_betting_cols <- function(play_df, g_id, yr){
  game_spread <- cfb_betting_lines(game_id = g_id, year=yr, line_provider = 'consensus') %>% 
    mutate(spread = as.numeric(.data$spread)) %>% 
    rename(game_id = .data$id, formatted_spread = .data$formattedSpread) %>% 
    select(.data$game_id, .data$spread, .data$formatted_spread)
  pbp_df <- play_df %>% left_join(game_spread, by=c('game_id'))
  return(pbp_df)
}
