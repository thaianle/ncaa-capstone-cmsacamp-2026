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
library(gt)
library(patchwork)


#code thanks to An!
df_2022 <- cbd_torvik_player_season(year = 2022)

df_2023 <- cbd_torvik_player_season(year = 2023)

df_2024 <- cbd_torvik_player_season(year = 2024)

df_2025 <- cbd_torvik_player_season(year = 2025)

df <- bind_rows(df_2022, df_2023, df_2024, df_2025)

#if team name is equal to the team name in the previous year

#actually get rid of has_transffered its not really helpful
transfer_df <- df |>
  group_by(player) |>
  filter(n_distinct(team) > 1) |>
  ungroup()

df <- df |>
  mutate(has_transferred = player %in% transfer_df$player)

career_transfers_by_player <- transfer_df |>
  select(id, year, team) |>
  arrange(id, year) |>
  group_by(id) |>
  mutate(prev_team = lag(team),
         transfer_made = ifelse(!(is.na(prev_team)) & (team != prev_team),
                                1, 0),
         cumulative_career_transfers = cumsum(transfer_made),
         num_transfers = max(cumulative_career_transfers)) |>
  select(id, year, cumulative_career_transfers, num_transfers)

career_transfers_by_player

df <- df |>
  left_join(career_transfers_by_player |>
              select(cumulative_career_transfers), by = "id")

df <- df |>
  relocate(cumulative_career_transfers)

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
  ggplot(aes(x = mpg)) +
  stat_ecdf(geom = "step") +
  scale_x_continuous(breaks = seq(0, 100, by = 5)) 

df_filtered <- df |> 
  filter(changed_team == TRUE) |> 
  filter(mpg > 7.5 | prev_mpg > 7.5) 

p_build <- ggplot(df_filtered, aes(x = prev_mpg, y = mpg)) + geom_smooth()
p_build
smooth_data <- ggplot_build(p_build)$data[[1]]
smooth_data
intersect_idx <- which.min(abs(smooth_data$y - smooth_data$x))
intersect_x   <- smooth_data$x[intersect_idx]
intersect_y   <- smooth_data$y[intersect_idx]

theme_set(theme_bw())

before_after_mpg <- df |>
  filter(changed_team == TRUE) |>
  filter(ppg > 2 & prev_ppg > 2) |>
  ggplot(aes(x = prev_mpg, y = mpg)) +
  geom_point(alpha = 0.6) +
  geom_smooth() +
  geom_abline(intercept = 0, slope = 1, color = "red", linewidth = 1.5) +
  labs(x = "before transfer",
       y = "after transfer",
       title = "Minutes per game") +
  theme(axis.title = element_text(size = 10),
        plot.title = element_text(size = 17))
  

plot_data <- ggplot()
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
  ggplot(aes(x = ppg)) +
  stat_ecdf(geom = "step") +
  scale_x_continuous(breaks = seq(0, 25, by = 5)) 

df_ppg_filtered <- df |> 
  filter(changed_team == TRUE) |> 
  filter(ppg > 2) 

p_ppg_build <- ggplot(df_ppg_filtered, aes(x = prev_ppg, y = ppg)) + geom_smooth()
p_ppg_build
smooth_ppg_data <- ggplot_build(p_ppg_build)$data[[1]]
smooth_ppg_data
intersect_ppg_idx <- which.min(abs(smooth_ppg_data$y - smooth_ppg_data$x))
intersect_ppg_x   <- smooth_ppg_data$x[intersect_ppg_idx]
intersect_ppg_y   <- smooth_ppg_data$y[intersect_ppg_idx]

before_after_ppg <- df |>
  filter(changed_team == TRUE) |>
  filter(ppg > 2 & prev_ppg > 2) |>
  ggplot(aes(x = prev_ppg, y = ppg)) +
  geom_point(alpha = 0.6) +
  geom_smooth() +
  geom_abline(intercept = 0, slope = 1, color = "red", linewidth = 1.5) + 
  labs(x = "before transfer",
       y = "after transfer",
       title = "Points per game") +
  theme(axis.title = element_text(size = 10),
        plot.title = element_text(size = 17)) 
  
  

# put these side by side

side_by_side <- before_after_mpg + before_after_ppg

ggsave("side_by_side_correct.png", plot = side_by_side)

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
 
 # elbow around 3 component
 # while the red horizontal line says around 6 components is optimal
 # if we want to visualize, we can only use 2-3 components
 # however, pcs 1, 2, and 3 explain 62.4 percent of the variance
 # which is still a decent level
 
 player_performance_pc_data <- player_performance_pca$x[, 1:3]
 
 fviz_nbclust(player_performance_pc_data, kmeans, method = "wss")

set.seed(1234)
 
 km <- km <- kmeans(player_performance_pc_data, centers = 4, nstart = 25)
 
 player_performance_stats_pca <- player_performance_stats_pca |>
   mutate(cluster = factor(km$cluster))
 
transfers     <- player_performance_stats_pca |> filter(changed_team == 1)
non_transfers <- player_performance_stats_pca |> filter(changed_team == 0)

 colnames(player_performance_stats_pca)

 
 
transfer_plot <- plot_ly(
   data = transfers,
   x = ~pc1,
   y = ~pc2,
   z = ~pc3,
   color = ~cluster
 )

transfer_plot

non_transfer_plot <- plot_ly(
  data = non_transfers,
  x = ~pc1,
  y = ~pc2,
  z = ~pc3,
  color = ~cluster
)

non_transfer_plot

# ok so its hard to see the difference in the graphs so i shall make a table

player_performance_table <- player_performance_stats_pca |>
  count(changed_team, cluster) |>
  group_by(changed_team) |> 
  # get to percent
  mutate(percent = 100*n/sum(n)) |>
  ungroup() |>
  select(changed_team, cluster, percent) |>
  pivot_wider(names_from = cluster,
              values_from = percent) |>
  gt()

player_performance_table
# what do we learn from this table?

# on the non-transfer side: the values are closer together, this kinda makes sense
# because a lot of our data is made up of non-transfers, and just doing this analysis 
# on the overall dataset gives us the character archetypes. the most find themselves
# in cluster 3, which are players that don't get a ton of minutes and play around slightly
# above average defense and slightly below average shooting. the least amount of players 
# that fall in a cluster is cluster 4, which is made of players that also don't get 
# a ton of minutes, have slightly above average shooting and slightly below average
# defensive skills.

# on the transfer side: the cluster with the most by 12% is cluster 3. these are the 
# players that don't get a ton of minutes and play around slightly above average 
# defense and slightly below average shooting. they are mainly transfering 


# An advised to try kmeans instead of hierarchical because of the number of observations



# success by position
# success relative to those in the same position

unique(df$pos)


#list function?


#scale all the metrics
position_df <- df |>
  group_by(pos) |>
  mutate(z_bpm = scale(bpm),
         z_ts = scale(ts),
         z_usg = scale(usg),
         z_ast_to = scale(ast_to),
         z_adj_de = scale(adj_de),
         z_porpag = scale(porpag),
         z_obpm = scale(obpm),
         z_ppg = scale(ppg),
         z_dreb = scale(dreb),
         z_oreb = scale(oreb),
         z_bpg = scale(bpg),
         z_dbpm = scale(dbpm),
         z_three_pct = scale(three_pct),
         z_apg = scale(apg),
         z_stl = scale(stl),
         z_adj_oe = scale(adj_oe)) |>
  ungroup()


# position_weights <- list(
#   "Wing G" = c(bpm = 0.24, ts = 0.22, usg = 0.2, ast_to = 0.18, agj_de = 0.16),
#   "Combo G" = c(bpm = 0.24, ast_to = 0.22, ts = 0.2, porpag = 0.18, usg = 0.16),
#   "Scoring PG" = c(ts = 0.24, obpm = 0.22, ppg = 0.2, ast_to = 0.18, usg = 0.16),
#   "PF/C" = c(ts = 0.24, dreb = 0.22, oreb = 0.2, bpg = 0.18, dbpm = 0.16),
#   "Wing F" = c(ts = 0.24, dbpm = 0.22, obpm = 0.2, three_pct = 0.18, usg = 0.16),
#   "C" = c(ts = 0.24, oreb = 0.22, dreb = 0.2, bpg = 0.18, dbpm = 0.16),
#   "Pure PG" = c(apg = 0.24, ast_to = 0.22, stl = 0.2, ts = 0.18, usg = 0.16),
#   "Stretch 4" = c(three_pct = 0.24, ts = 0.22, adj_oe = 0.2, dreb = 0.18, usg = 0.16)

Wing_G_df <- position_df |>
  filter(pos == "Wing G") |>
  mutate(success_score = (z_bpm*.24) + (z_ts*.22) + (z_usg*.20) + (z_ast_to*.18) + (z_adj_de*.16))

Combo_G_df <- position_df |>
  filter(pos == "Combo G") |>
  mutate(success_score = (z_bpm*.24) + (z_ast_to*.22) + (z_ts*.20) + (z_porpag*.18) + (z_usg*.16))

Scoring_PG_df <- position_df |>
  filter(pos == "Scoring PG") |>
  mutate(success_score = (z_ts*.24) + (z_obpm*.22) + (z_ppg*.20) + (z_ast_to*.18) + (z_usg*.16))

PF_C_df <- position_df |>
  filter(pos == "PF/C") |>
  mutate(success_score = (z_ts*.24) + (z_dreb*.22) + (z_oreb*.20) + (z_bpg*.18) + (z_usg*.16))

Wing_F_df <- position_df |>
  filter(pos == "Wing F") |>
  mutate(success_score = (z_ts*.24) + (z_dbpm*.22) + (z_obpm*.20) + (z_three_pct*.18) + (z_usg*.16))
  
C_df <- position_df |>
  filter(pos == "C") |>
  mutate(success_score = (z_ts*.24) + (z_oreb*.22) + (z_dreb*.20) + (z_bpg*.18) + (z_dbpm*.16))

Pure_PG_df <- position_df |>
  filter(pos == "Pure PG") |>
  mutate(success_score = (z_apg*.24) + (z_ast_to*.22) + (z_stl*.20) + (z_ts*.18) + (z_usg*.16))

Stretch_4_df <- position_df |>
  filter(pos == "Stretch 4") |>
  mutate(success_score = (z_three_pct*.24) + (z_ts*.22) + (z_adj_oe*.20) + (z_dreb*.18) + (z_usg*.16))

position_df <- bind_rows(Wing_G_df, Combo_G_df, Scoring_PG_df, PF_C_df, Wing_F_df, C_df, Pure_PG_df, Stretch_4_df)

position_df <- position_df |>
  relocate(success_score)

# did transfers perform better relative to their peers at the same position?

position_df |>
  drop_na(pos) |>
  group_by(pos, changed_team) |>
  summarise(mean_score = mean(success_score, na.rm = TRUE)) |>
  ggplot(aes(x = changed_team, y = mean_score)) +
  geom_col() +
  facet_wrap(~pos)

# RAPM?# posRAPM?

# make a gt table for data slide in slideshow
set.seed(1234)

data_table <- df |>
  select(year, id, changed_team, mpg, prev_mpg, ppg, prev_ppg, has_transferred, cumulative_career_transfers) |>
  slice_sample(n = 5) |>
  gt() |>
  cols_label(
    year = "Year",
    id = "Player ID",
    changed_team = "Year After Transfer",
    mpg = "Minutes Per Game",
    prev_mpg = "Previous Year's Minutes Per Game",
    ppg = "Points Per Game",
    prev_ppg = "Previous Year's Minutes Per Game",
    has_transferred = "Has Transferred",
    cumulative_career_transfers = "Cumulative Career Transfers"
  ) |>
  fmt_number(
    columns = c(mpg, prev_mpg, ppg, prev_ppg),
    decimals = 2
  )

gtsave("data_table.png", data = data_table)





