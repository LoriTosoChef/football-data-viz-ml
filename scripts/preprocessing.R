gc()

library("dplyr")
library("tidyverse")

# Multi-Class Classification: Win H - Win A - Draw -----------------------------

## Overall Ratings Only --------------------------------------------------------
# first attempt, without betting odds columns;
# only using average overall rating of each team

overall_rating_players <- player_stats %>%
  group_by(player_api_id, date) %>%
  summarise(overall_rating = mean(overall_rating))

# adding target column (Y)
# 0: Home Team Win --- 1: Away Team Win --- 2: Draw
# drop columns unnecessary for training
dataset_player_overall <- match[1:33] %>%
  mutate(target = ifelse(home_team_goal > away_team_goal, "H", ifelse(away_team_goal > home_team_goal, "A", "D"))) %>%
  select(-id, -country_id, -league_id, -season, -stage, -date, -home_team_goal, -away_team_goal) %>%
  left_join(overall_rating_players, 
             by = c('home_player_1' = 'player_api_id', 'season_year' = 'date')) %>%
  left_join(overall_rating_players, 
             by = c('home_player_2' = 'player_api_id', 'season_year' = 'date'), suffix = c('_h1', '_h2')) %>%
  left_join(overall_rating_players,
            by = c('home_player_3' = 'player_api_id', 'season_year' = 'date')) %>%
  left_join(overall_rating_players,
            by = c('home_player_4' = 'player_api_id', 'season_year' = 'date'), suffix = c('_h3', '_h4')) %>%
  left_join(overall_rating_players, 
            by = c('home_player_5' = 'player_api_id', 'season_year' = 'date')) %>%
  left_join(overall_rating_players, 
            by = c('home_player_6' = 'player_api_id', 'season_year' = 'date'), suffix = c('_h5', '_h6')) %>%
  left_join(overall_rating_players,
            by = c('home_player_7' = 'player_api_id', 'season_year' = 'date')) %>%
  left_join(overall_rating_players,
            by = c('home_player_8' = 'player_api_id', 'season_year' = 'date'), suffix = c('_h7', '_h8')) %>%
  left_join(overall_rating_players, 
            by = c('home_player_9' = 'player_api_id', 'season_year' = 'date')) %>%
  left_join(overall_rating_players, 
            by = c('home_player_10' = 'player_api_id', 'season_year' = 'date'), suffix = c('_h9', '_h10')) %>%
  left_join(overall_rating_players,
            by = c('home_player_11' = 'player_api_id', 'season_year' = 'date')) %>%
  select(-home_player_1, -home_player_2, -home_player_3, -home_player_4, -home_player_5, -home_player_6, 
         -home_player_7, -home_player_8, -home_player_9, -home_player_10, -home_player_11)

# get average home team rating and drop unnecessary columns
dataset_player_overall <- dataset_player_overall %>%
  mutate(overall_rating_H = rowMeans(select(dataset_player_overall, grep('overall_rating', names(dataset_player_overall))), na.rm = TRUE)) %>%
  select(-overall_rating, -overall_rating_h1, -overall_rating_h2, -overall_rating_h3, -overall_rating_h4, 
         -overall_rating_h5, -overall_rating_h6, -overall_rating_h7, -overall_rating_h8, -overall_rating_h9, 
         -overall_rating_h10)

# left joining away team players
dataset_player_overall <- dataset_player_overall %>%
  left_join(overall_rating_players, 
            by = c('away_player_1' = 'player_api_id', 'season_year' = 'date')) %>%
  left_join(overall_rating_players, 
            by = c('away_player_2' = 'player_api_id', 'season_year' = 'date'), suffix = c('_a1', '_a2')) %>%
  left_join(overall_rating_players,
            by = c('away_player_3' = 'player_api_id', 'season_year' = 'date')) %>%
  left_join(overall_rating_players,
            by = c('away_player_4' = 'player_api_id', 'season_year' = 'date'), suffix = c('_a3', '_a4')) %>%
  left_join(overall_rating_players, 
            by = c('away_player_5' = 'player_api_id', 'season_year' = 'date')) %>%
  left_join(overall_rating_players, 
            by = c('away_player_6' = 'player_api_id', 'season_year' = 'date'), suffix = c('_a5', '_a6')) %>%
  left_join(overall_rating_players,
            by = c('away_player_7' = 'player_api_id', 'season_year' = 'date')) %>%
  left_join(overall_rating_players,
            by = c('away_player_8' = 'player_api_id', 'season_year' = 'date'), suffix = c('_a7', '_a8')) %>%
  left_join(overall_rating_players, 
            by = c('away_player_9' = 'player_api_id', 'season_year' = 'date')) %>%
  left_join(overall_rating_players, 
            by = c('away_player_10' = 'player_api_id', 'season_year' = 'date'), suffix = c('_a9', '_a10')) %>%
  left_join(overall_rating_players,
            by = c('away_player_11' = 'player_api_id', 'season_year' = 'date')) %>%
  select(-away_player_1, -away_player_2, -away_player_3, -away_player_4, -away_player_5, -away_player_6, 
         -away_player_7, -away_player_8, -away_player_9, -away_player_10, -away_player_11)

# get average home team rating and drop unnecessary columns
dataset_player_overall <- dataset_player_overall %>%
  mutate(overall_rating_A = rowMeans(select(dataset_player_overall, grep('overall_rating', names(dataset_player_overall)), -overall_rating_H), na.rm = TRUE)) %>%
  select(-overall_rating, -overall_rating_a1, -overall_rating_a2, -overall_rating_a3, -overall_rating_a4, 
         -overall_rating_a5, -overall_rating_a6, -overall_rating_a7, -overall_rating_a8, -overall_rating_a9, 
         -overall_rating_a10, -home_team_api_id, -away_team_api_id, -season_year) %>%
  relocate(overall_rating_A, overall_rating_H, .before = target)

## Overall and GK Ratings ------------------------------------------------------
