# capstone brainstorming

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

#start here
library(cbbdata)
library(dplyr)
library(ggplot2)


#code thanks to An!
df_2022 <- cbd_torvik_player_season(year = 2022)

df_2023 <- cbd_torvik_player_season(year = 2023)

df_2024 <- cbd_torvik_player_season(year = 2024)

df_2025 <- cbd_torvik_player_season(year = 2025)

df <- bind_rows(df_2022, df_2023, df_2024, df_2025)

#if team name is equal to the team name in the previous year

#actually get rid of has_transffered its not really helpful
#transfer_df <- df |>
#  group_by(player) |>
#  filter(n_distinct(team) > 1) |>
#  ungroup()

#df <- df |>
#  mutate(has_transferred = player %in% transfer_df$player)

#df <- df |>
#  select(-is_transfer)

#shows the difference in points scored by players who had transferred vs players who had not
# didnt really do what i wanted so IGNORE
#df |>
#  ggplot(aes(x = is_transfer, y = g)) +
#  geom_col() +
#  facet_wrap(~year)

# trying to make transfer out year/transfer in year
# changed_team is the first year after transfer
df <- df |>
  arrange(id, year) |>
  group_by(id) |>
  mutate(prev_team = lag(team),
         prev_year = lag(year),
         #if they have a previous team AND 
         #the year is the one right after the previous year AND 
         #the team they're on is not the same as the previous team THEN
         #changed_team is through IF NOT THEN
         #changed_team is false 
         changed_team = ifelse(!is.na(prev_team) &
                                 year == (prev_year + 1) &
                                 team != prev_team,
                               TRUE,
                               FALSE))
#get these 3 in front of df just to see
df <- df |>
  relocate(changed_team)

df <- df |>
  relocate(prev_team)

df <- df |>
  relocate(prev_year)

#format to long in order to get columns of same player and their total games played for the different years side by side
# do i need this?
# turns out the answer is yes i do
# change player to id later since id is correct
df <- df |>
  mutate(prev_games = lag(g))

df <- df |>
  relocate(prev_games)

long <- df |>
  select(id, year, g, prev_games) |>
  pivot_longer(
    cols = c(g, prev_games),
    names_to = "season",
    values_to = "games"
  )

# this gives us bar chart of all of the games that all the transfer players played in
# before and after their transfer
long |>
  filter(!is.na(games)) |>
  ggplot(aes(x = season, y = games)) +
  geom_col()


# how do i get this but every transfer ever

#who transfers?
#how do transfer situations differ?
#what happens after they transfer?

# number of transferred players per year
df |>
  group_by(year) |>
  summarise(sum(changed_team == TRUE))

# conferences and their classification
unique(df$conf)
df <- df |>
  mutate(conference_classification = case_when(
    conf %in% c("B10", "SEC", "ACC", "B12", "BE") ~ "high-major",
    conf %in% c("Amer", "A10", "CAA", "MVC", "MWC", "P12", "WCC") ~ "mid-major",
    conf %in% c("AE", "ASun", "BSky", "BSth", "BW", "CUSA", "Horz",
              "Ivy", "MAAC", "MAC", "MEAC", "NEC", "OVC", "Pat",
              "SC", "Slnd", "SWAC", "Sum", "SB", "WAC", "ind") ~ "low-major"
  ))








