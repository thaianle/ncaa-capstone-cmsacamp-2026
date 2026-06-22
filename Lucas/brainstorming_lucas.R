# capstone braining

#attempting the hoopR package
#install.packages("hoopR")
#library(hoopR)
#library(tidyverse)

# Pulls play-by-play data for men's college basketball
#mbb_pbp_data <- hoopR::load_mbb_pbp(seasons = 2021:2025)
#glimpse(mbb_pbp_data)

#by_game_data <- mbb_pbp_data |>
#  group_by(game_id)

# this didnt work lol^^

# install.packages("devtools")
# Skip the update prompt completely

#install.packages("xml2", type = "source")
#install.packages("curl", type = "both")
#library(devtools)

# 1. Install a stable version of httr2 that bypasses this bug
#devtools::install_version("httr2", version = "1.1.0", upgrade = "never", force = TRUE)



#devtools::install_github("andreweatherman/cbbdata")

# to register
#cbbdata::cbd_create_account(username = '###', email = '###', password = '###', confirm_password = '###')

#trying again
#install.packages("devtools")
devtools::install_github("andreweatherman/cbbdata")
# to register
#cbbdata::cbd_create_account(username = '###', email = '###@icloud.com', password = '###', confirm_password = '###')

# persistent log-in
cbbdata::cbd_login()

df_2022 <- cbd_torvik_player_season(year = 2022)







