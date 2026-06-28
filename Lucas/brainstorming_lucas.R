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
library(devtools)

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

#install.packages(c("tseries", "FactoMineR"), dependencies = TRUE)

#install.packages("factoextra", dependencies = TRUE)

#install.packages("factoextra", type = "binary")

#install.packages("FactoMineR")

#start here
library(cbbdata)
library(ggplot2)
library(tidyverse)
library(scales)
library(factoextra)
library(FactoMineR)
library(plotly)


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
                               FALSE)) |>
  ungroup()
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

# bar graph for transfer players by conference
df <- df |>
  mutate(prev_conf_class = lag(conference_classification))

df <- df |>
  relocate(prev_conf_class)

df <- df |>
  mutate(prev_conf = lag(conf))

df <- df |>
  relocate(prev_conf)


#conferences with most transfers out
df |>
  filter(changed_team == TRUE) |>
  ggplot(aes(x = fct_infreq(prev_conf), fill = prev_conf_class)) +
  geom_bar() +
  coord_flip()
#SEC has the most transfers out 
#for the most part, with obviously some variance, high-majors have a 
#lot of transfers out, mid-majors have medium amount out, and 
#low-majors have a low amount of transfers out

#conferences with the most transfers in
df |>
  filter(changed_team == TRUE) |>
  ggplot(aes(x = fct_infreq(conf), fill = conference_classification)) +
  geom_bar() +
  coord_flip()
#SEC has the most transfers in?
#again, for the most part, with obviously some variance, high-majors have a 
#lot of transfers in, mid-majors have medium amount in, and 
#low-majors have a low amount of transfers in

# positions
unique(df$pos)
# positions that transfer the most
df |>
  filter(changed_team == TRUE) |>
  ggplot(aes(x = pos)) +
  geom_bar()
# pure PG transfer the least (teams value what they bring to the table)
# wing G transfer the most (this position is kind of a combination between shooting G and SF)

# which class year is the most popular to transfer 

#find previous year like what class they were before the transfer happened
df <- df |>
  mutate(prev_class = lag(exp))

df <- df |>
  relocate(prev_class)

df |>
  filter(changed_team == TRUE) |>
  ggplot(aes(x = prev_class)) +
  geom_bar()
# most people transfer after their junior year 
# note: super senior status due to red shirt

# total athletes mpg before and after transferring
# get previous minutes per game
df <- df |>
  mutate(prev_mpg = lag(mpg))

df <- df |>
  relocate(prev_mpg)
df<- df|>
  relocate(mpg)

df |>
  filter(changed_team == TRUE)

long2 <- df |>
  select(id, year, mpg, prev_mpg) |>
  pivot_longer(
    cols = c(mpg, prev_mpg),
    names_to = "season",
    values_to = "mpg"
  )

long2 |>
  filter(!is.na(mpg)) |>
  ggplot(aes(x = season, y = mpg)) +
  geom_col() +
  scale_y_continuous(labels = scales::comma)
# people have more minutes per game after transferring

#what about a boxplot?
long2 |>
  filter(!is.na(mpg)) |>
  ggplot(aes(x = mpg, y = season)) +
  geom_boxplot()

#how about histogram?
long2 |>
  filter(!is.na(mpg)) |>
  ggplot(aes(x = mpg)) +
  geom_histogram() +
  facet_wrap("season")

#density plot?
long2 |>
  filter(!is.na(mpg)) |>
  ggplot(aes(x = mpg, fill = season)) +
  geom_density()

#plotting it x vs y
df |>
  filter(changed_team == TRUE) |>
  ggplot(aes(x = prev_mpg, y = mpg)) +
  geom_point(alpha = 0.6) +
  geom_smooth() +
  geom_abline(intercept = 0, slope = 1, color = "red", linewidth = 1.5)
#if a player had less that ~22 minutes per game, on average, they recieved more 
#minutes after their transfer, however, if a player had more than 22 minutes,
#on average, they recieved less playing time at the school they transferred to 

# ok big fish little pond
#df |>
#  filter(changed_team == TRUE & 
#           (prev_conf_class == "mid-major" | prev_conf_class == "low-major") &
#           conference_classification == "high-major") |>
#  group_by(id) |>
#  summarise(med_mpg = median(mpg),
#            med_prev_mpg = median(prev_mpg)) 
#select(id, mpg, prev_mpg) |>
#  pivot_longer(
#    cols = c(mpg, prev_mpg),
#    names_to = "season",
#    values_to = "mpg"
#  ) 


median_df <- df |>
  filter(changed_team == TRUE & 
           (prev_conf_class == "mid-major" | prev_conf_class == "low-major") &
           conference_classification == "high-major") |>
  mutate(mpg = as.numeric(mpg),
         prev_mpg = as.numeric(prev_mpg))


big_pond_median_current <- median(median_df$mpg)
big_pond_median_before <- median(median_df$prev_mpg)

big_pond_df <- data.frame(
  category = c("minutes_before_transfer", "minutes_after_transfer"),
  values = c(big_pond_median_before, big_pond_median_current)
) 


big_pond_df |>
  ggplot(aes(x = reorder(category, -values), y = values)) +
  geom_col()
# for players who transfer from a smaller program to a bigger one,
# the median minutes per game drops drastically

# what about the other way around?
other_median_df <- df |>
  filter(changed_team == TRUE & 
           (conference_classification == "mid-major" | conference_classification == "low-major") &
           prev_conf_class == "high-major") |>
  mutate(mpg = as.numeric(mpg),
         prev_mpg = as.numeric(prev_mpg))

little_pond_median_current <- median(other_median_df$mpg)
little_pond_median_before <- median(other_median_df$prev_mpg)

little_pond_df <- data.frame(
  category = c("minutes_before_transfer", "minutes_after_transfer"),
  values = c(little_pond_median_before, little_pond_median_current)
) 

little_pond_df |>
  ggplot(aes(x = reorder(category, values), y = values)) +
  geom_col()
# players that transfer from big to small schools,
# the median minutes per game increases

#what about ppg
df <- df |>
  mutate(prev_ppg = lag(ppg))


long_ppg <- df |>
  select(id, year, ppg, prev_ppg) |>
  pivot_longer(
    cols = c(ppg, prev_ppg),
    names_to = "season",
    values_to = "ppg"
  )

long_ppg |>
  filter(!is.na(ppg)) |>
  ggplot(aes(x = season, y = ppg)) +
  geom_col() +
  scale_y_continuous(labels = scales::comma)

#what about a boxplot?
long_ppg |>
  filter(!is.na(ppg)) |>
  ggplot(aes(x = ppg, y = season)) +
  geom_boxplot()

#how about histogram?
long_ppg |>
  filter(!is.na(ppg)) |>
  ggplot(aes(x = ppg)) +
  geom_histogram() +
  facet_wrap("season")

#density plot?
long_ppg |>
  filter(!is.na(ppg)) |>
  ggplot(aes(x = ppg, fill = season)) +
  geom_density()

#plotting it x vs y
df |>
  filter(changed_team == TRUE) |>
  ggplot(aes(x = prev_ppg, y = ppg)) +
  geom_point(alpha = 0.6) +
  geom_smooth() +
  geom_abline(intercept = 0, slope = 1, color = "red", linewidth = 1.5)
#if a player had less that ~7-8 points per game, on average, they received more 
#points per game after their transfer, however, if a player had more than 7-8 points per game,
#on average, they received less points per game time at the school they transferred to 

# kmeans clustering
# potential features: mpg, ppg, rpg, apg, tov, spg, efg, fgp, stl, blk, to, ast, rebound?, block?, 
# adj de/of rating, two_pct, three_pct, dunk_pct


 player_performance_df <- df |>
   select(mpg, ppg, rpg, apg, tov, spg, efg, fg_pct, stl, blk, two_pct, three_pct,
          dunk_pct, adj_oe, adj_de, changed_team) |>
   drop_na(mpg, ppg, rpg, apg, tov, spg, efg, fg_pct, stl, blk, two_pct, three_pct,
           dunk_pct, adj_oe, adj_de, changed_team) 
 
 player_performance_feat <- player_performance_df |>
   select(-changed_team) 
 
 player_performance_pca <- prcomp(player_performance_feat, center = TRUE, scale. = TRUE)
 summary(player_performance_pca)
 
 player_performance_pc_matrix <- player_performance_pca$x
 
 head(as.data.frame(player_performance_pc_matrix))
 
 #cov(player_performance_pc_matrix)
 
 as.data.frame(player_performance_pca$rotation) |> rownames_to_column("statistic")
 
 
 player_performance_stats_pca <- player_performance_df |> 
   mutate(pc1 = player_performance_pc_matrix[,1], 
          pc2 = player_performance_pc_matrix[,2],
          pc3 = player_performance_pc_matrix[,3])
 
 player_performance_stats_pca |> 
   ggplot(aes(x = pc1, y = pc2)) +
   geom_point(alpha = 0.5) +
   labs(x = "PC1 (31.65%) ", y = "PC2 (21.07%)")

 
# fviz_pca_var(): projection of variables
# fviz_pca_ind(): display observations with first two PCs
 player_performance_pca |> 
   fviz_pca_biplot(label = "var",
                   alpha.ind = 0.25,
                   alpha.var = 0.75,
                   labelsize = 5,
                   col.var = "red",
                   repel = TRUE)

 
 player_performance_pca |> 
   fviz_eig(addlabels = TRUE) +
   geom_hline(
     yintercept = 100 * (1 / ncol(player_performance_pca$x)), 
     linetype = "dashed", 
     color = "darkred",
   )
 
 player_performance_pc_data <- player_performance_pca$x[, 1:3]
 
 fviz_nbclust(player_performance_pc_data, kmeans, method = "wss")

set.seed(1234)
 
 km <- km <- kmeans(player_performance_pc_data, centers = 4, nstart = 25)
 
 player_performance_stats_pca <- player_performance_stats_pca |>
   mutate(cluster = factor(km$cluster))

 colnames(player_performance_stats_pca)

transfers <- player_performance_stats_pca |>
  filter(changed_team == 1)
 
non_transfers <- player_performance_stats_pca |>
  filter(changed_team == 0) 
 
#non-transfers
plot_ly(
   data = non_transfers,
   x = ~pc1,
   y = ~pc2,
   z = ~player_performance_pca$x[,3],
   color = ~cluster, 
   type = "scatter3d",
   mode = "markers"
 )

# elbow around 3 component
# while the red horizontal line says around 6 components is optimal
# if we want to visualize, we can only use 2-3 components
# however, pcs 1, 2, and 3 explain 62.4 percent of the variance
# which is still a decent level

# An advised to try kmeans instead of hierarchical because of the number of observations


# success by position
# success relative to those in the same position
# RAPM?









