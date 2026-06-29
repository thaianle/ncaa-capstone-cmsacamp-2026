# Useful resources:
# https://hoopr.sportsdataverse.org/index.html
# https://hoopr.sportsdataverse.org/articles/cbbd-college-basketball-data.html

library(hoopR)
# has_cbbd_key()
games <- cbbd_games(season = 2024, team = "Duke")

library(hoopR)
tictoc::tic()
progressr::with_progress({
  nba_pbp <- hoopR::load_nba_pbp()
})
tictoc::toc()

library(hoopR)