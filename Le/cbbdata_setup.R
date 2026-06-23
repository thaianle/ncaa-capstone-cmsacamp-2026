# https://github.com/andreweatherman/cbbdata
# https://cbbplotr.aweatherman.com/index.html

# install.packages("devtools")
# devtools::install_github("andreweatherman/cbbdata")

#cbbdata::cbd_create_account(username = 'thaianle',
                             #email = 'thaianle1102@gmail.com',
                             #password = '***',
                             #confirm_password = '***')

rm(list = ls())
library(tidyverse)
library(cbbdata)

# The "year/season" represents the year when the season concludes, based on
# my personal search

df_2022 <- cbd_torvik_player_season(year = 2022)

df_2023 <- cbd_torvik_player_season(year = 2023)

df_2024 <- cbd_torvik_player_season(year = 2024)

df_2025 <- cbd_torvik_player_season(year = 2025)

df <- bind_rows(df_2022, df_2023, df_2024, df_2025)
write.csv(df, "Le/player_stats.csv", row.names = FALSE)