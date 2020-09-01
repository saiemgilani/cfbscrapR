library(testthat)
library(cfbscrapR)

test_that("CFB Play Stats - Player", {
  x <- cfb_play_stats_player(game_id = 401012356)
  y <- cfb_play_stats_player(game_id = 401110720)
  cols <- c("game_id", "season", "week", "opponent", "team_score", "opponent_score", 
            "drive_id", "play_id", "period", "yards_to_goal", "down", "distance", 
            "athlete_id", "stat", "reception", "completion", "rush", 
            "interception","interception_thrown", "touchdown", "incompletion", "target", 
            "fumble_recovered", "fumble_forced", "fumble", 
            "sack", "sack_taken", "pass_breakup")
  # df <- data.frame(matrix(NA, nrow = 0, ncol = 28))
  # df <- setNames(df,cols)
  # expect_equal(colnames(x), cols)
  # expect_equal(colnames(y), cols)
  expect_s3_class(x, "data.frame")
  expect_s3_class(y, "data.frame")
  
})