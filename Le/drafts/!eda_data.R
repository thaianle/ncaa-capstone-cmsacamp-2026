rm(list = ls()) # Clear the environment
library(tidyverse)
library(cbbdata)
library(hoopR)
library(gt)

# Read the player stats by season dataset
df_2022 <- cbd_torvik_player_season(year = 2022)
df_2023 <- cbd_torvik_player_season(year = 2023)
df_2024 <- cbd_torvik_player_season(year = 2024)
df_2025 <- cbd_torvik_player_season(year = 2025)

df <- bind_rows(df_2022, df_2023, df_2024, df_2025)
df

# Randomly sample 5 rows in the dataset (set seed for reproducibility)
set.seed(275225)

gt_table <- df |>
  slice_sample(n = 5) |>
  select(id, player, year, team, pos, exp, mpg, ppg, rpg, apg) |>
  gt()

# Display the table and save it as an image
gt_table

gt_table |>
  gtsave("eda_table.png")
