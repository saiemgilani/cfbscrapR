context("CFB Game Player Stats")

x <- cfb_game_player_stats(2018, week = 15, conference = 'Ind')

y <- cfb_game_player_stats(2013, week = 1, team = "Florida State", category = 'passing')

z <- cfb_game_player_stats(2013, week = 1, team = "Florida State")

cols <- c("game_id", "team", "conference", "home_away", "points", "category", 
          "athlete_id", "name", "c_att", "yds", "avg", "td", "int", "qbr",
          "car", "long", "rec", "no", "fg", "pct", "xp", "pts", "tb", "in_20", 
          "fum", "lost", "tot", "solo", "sacks", "tfl", "pd", "qb_hur")

test_that("CFB Game Player Stats", {
  expect_equal(colnames(x), cols)
  expect_equal(colnames(y), cols)
  expect_equal(colnames(z), cols)
  expect_s3_class(x, "data.frame")
  expect_s3_class(y, "data.frame")
  expect_s3_class(z, "data.frame")
})