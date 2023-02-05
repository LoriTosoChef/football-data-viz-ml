gc()
# Libraries --------------------------------------------------------------------

library("arrow")
library("dplyr")

# Reading files ----------------------------------------------------------------

dfs <- list.files(path = './data')
parquet_files <- list()
for (name in dfs) {
  parquet_files[[name]] <- read_parquet(paste0('./data/', name))
}

for (file in names(parquet_files)) {
  new_file <- gsub('.parquet', '', file)
  assign(new_file, parquet_files[[file]])
}

rm(parquet_files, file, dfs, name, new_file)

# Data Cleaning ----------------------------------------------------------------
# useless columns
player <- player %>% select(-id, -player_fifa_api_id)
player_stats <- player_stats %>% select(-id, -player_fifa_api_id)
team <- team %>% select(-id, -team_fifa_api_id, -team_short_name)
team_stats <- team_stats %>% select(-id, -team_fifa_api_id, -buildUpPlayDribbling)

# check # of na values
colSums(is.na(player_stats))
colSums(is.na(team_stats))
colSums(is.na(match))

# drop useless players and html tags
match <- match %>% 
  select(-goal, -shoton, -shotoff, -foulcommit, -card, -cross, -corner, -possession, -match_api_id,
                -home_player_X1, -home_player_X2, -home_player_X3, -home_player_X4, -home_player_X5,
                -home_player_X6, -home_player_X7, -home_player_X8, -home_player_X9, -home_player_X10,
                -home_player_X11, -away_player_Y1, -away_player_Y2, -away_player_Y3, -away_player_Y4,
                -away_player_Y5, -away_player_Y6, -away_player_Y7, -away_player_Y8, -away_player_Y9,
                -away_player_Y10, -away_player_Y11, -home_player_Y1, -home_player_Y2, -home_player_Y3,
                -home_player_Y4, -home_player_Y5, -home_player_Y6, -home_player_Y7, -home_player_Y8,
                -home_player_Y9, -home_player_Y10, -home_player_Y11, -away_player_X1, -away_player_X2,
                -away_player_X3, -away_player_X4, -away_player_X5, -away_player_X6, -away_player_X7, 
                -away_player_X8, -away_player_X9, -away_player_X10, -away_player_X11, -PSA, -PSH, -PSD) %>%
  drop_na(home_player_1, home_player_2, home_player_3, home_player_4, home_player_5,
          home_player_6, home_player_7, home_player_8, home_player_9, home_player_10,
          home_player_11, away_player_1, away_player_2, away_player_3, away_player_4,
          away_player_5, away_player_6, away_player_7, away_player_8, away_player_9,
          away_player_10, away_player_11)

# fill player_stats numeric columns NAs with column means, not many NAs, okay to fill with NAs
num_cols <- sapply(player_stats, is.numeric)
player_stats[num_cols] <- player_stats[num_cols] %>%
  mutate_all(~ifelse(is.na(.), mean(., na.rm = TRUE), .))
rm(num_cols)

# fill player_stats categorical columns NAs with column means, not many NAs, okay to fill with NAs
cat_cols <- sapply(player_stats, is.character)
player_stats[cat_cols] <- player_stats[cat_cols] %>%
  apply(MARGIN = 2, FUN = function(x) {
    if (any(is.na(x))) {
      x[is.na(x)] <- names(which.max(table(x)))
    }
    x
  })
rm(cat_cols)

# now get only the columns of betting odds and separate them in Home win odds, Draw odds and Away win odds.
# then separate the dataframe into separate dataframes based on this betting odds categories, will be used to 
# fill NA values and calculate row means only between selected columns.
# e.g., if NA value is an Away win odds, then compute the mean only between other Away win odds.
bookie_cols <- colnames(match)[colSums(is.na(match)) > 0]
A <- bookie_cols[grep('A$', bookie_cols)]
D <- bookie_cols[grep('D$', bookie_cols)]
H <- bookie_cols[grep('H$', bookie_cols)]

df_A <- match %>% select(id, all_of(A))
df_D <- match %>% select(id, all_of(D))
df_H <- match %>% select(id, all_of(H))

# fill NA values with row mean
df_A <- df_A %>%
  mutate_all(~ifelse(is.na(.), rowMeans(select(df_A, all_of(A)), na.rm = TRUE), .))

df_D <- df_D %>%
  mutate_all(~ifelse(is.na(.), rowMeans(select(df_D, all_of(D)), na.rm = TRUE), .))

df_H <- df_H %>%
  mutate_all(~ifelse(is.na(.), rowMeans(select(df_H, all_of(H)), na.rm = TRUE), .))

# concatenate back together the match_clean dataframe, first delete old columns and left_join the new ones.
# finally drop last NA values, i.e., betting odds not available by any bookie
match <- match %>%
  select(-A, -D, -H) %>%
  left_join(df_A, by = c('id' = 'id')) %>%
  left_join(df_D, by = c('id' = 'id')) %>%
  left_join(df_H, by = c('id' = 'id')) %>%
  drop_na()

rm(A, D, H, bookie_cols, df_A, df_D, df_H)

# last check
colSums(is.na(player_stats))
colSums(is.na(team_stats))
colSums(is.na(match))

# Converting Dates
player$birthday <- as.numeric(format(as.Date(player$birthday, format = "%Y-%m-%d"), "%Y"))
player_stats$date <- as.numeric(format(as.Date(player_stats$date, format = "%Y-%m-%d"), "%Y"))
team_stats$date <- as.numeric(format(as.Date(team_stats$date, format = "%Y-%m-%d"), "%Y"))
match$season_year <- as.numeric(substr(match$season, 1,4))
match <- match %>% relocate(season_year, .after = season)

#TODO check potential player observation missing for given years, same for teams

# Summaries --------------------------------------------------------------------

# league goals
summary_league_season <- match %>%
  left_join(league %>% select(id, name), by = c('league_id' = 'id'), suffix = c('', '')) %>%
  relocate(c(name), .before = season) %>%
  rename(league = name) %>%
  select(-country_id, -league_id) %>%
  left_join(team, by = c('home_team_api_id' = 'team_api_id'), suffix = c('', '_home')) %>%
  left_join(team, by = c('away_team_api_id' = 'team_api_id'), suffix = c('', '_away')) %>%
  group_by(league, season) %>%
  summarize(number_of_teams = n_distinct(team_long_name),
            avg_home_team_goals = mean(home_team_goal),
            avg_away_team_goals = mean(away_team_goal),
            avg_goal_diff = mean(home_team_goal - away_team_goal),
            avg_goals = mean(home_team_goal + away_team_goal),
            total_goals = sum(home_team_goal + away_team_goal)) %>%
  arrange(league, desc(season))

#TODO Player summary
# player_stats %>%
#   left_join(player %>% select(player_api_id, player_name, birthday), by = c('player_api_id' = 'player_api_id')) %>%
#   relocate(c(player_name, birthday), .before = date) %>%
#   select(-player_api_id)

#TODO Team summary