# https://github.com/andreweatherman/cbbdata
# https://cbbplotr.aweatherman.com/index.html

# install.packages("devtools")
# devtools::install_github("andreweatherman/cbbdata")

#cbbdata::cbd_create_account(username = 'thaianle',
                             #email = 'thaianle1102@gmail.com',
                             #password = '***',
                             #confirm_password = '***')

rm(list = ls())
library(cbbdata)

# The "year/season" represents the year when the season concludes, based on
# my personal search

df_2022 <- cbd_torvik_player_season(year = 2022)

df_2023 <- cbd_torvik_player_season(year = 2023)

df_2024 <- cbd_torvik_player_season(year = 2024)

df_2025 <- cbd_torvik_player_season(year = 2025)

df <- bind_rows(df_2022, df_2023, df_2024, df_2025)

# Summarize some variables
table(df$pos)
table(df$exp)
colSums(is.na(df))
# https://stackoverflow.com/questions/26273663/r-how-to-total-the-number-of-na-in-each-col-of-data-frame
# There are some missing values. We will investigate them later.

df |>
  ggplot(aes(x = mpg)) +
  geom_histogram()