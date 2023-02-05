gc()

library("dplyr")
library("tidyverse")
library("ggplot2")
library("scales")
library("ggpubr")

# Global variables ---------------------------------------------------------
CWD <- getwd()
plot_dir <- '/fig/'

# average total goals over time
ggplot(data = summary_league_season, aes(x = season, y = avg_goals, group = league, color = league)) + 
  geom_line() + 
  geom_point() +
  expand_limits(y = c(2, 3.25)) +
  scale_y_continuous(
    labels = unit_format(unit = "Goals", scale = 1)
  ) + 
  labs(
    title = 'Average Goals Over Time',
    x = 'Season',
    y = 'Average Goal Scored'
  ) + 
  theme(plot.title = element_text(hjust = 0.5))

# average goals difference over time
ggplot(data = summary_league_season, aes(x = season, y = avg_goal_diff, group = league, color = league)) + 
  geom_line() + 
  geom_point() +
  expand_limits(y = c(0.1, 0.6)) +
  scale_y_continuous(
    labels = unit_format(unit = "Diff", scale = 1)
  ) + 
  labs(
    title = 'Average Goal Difference Over Time',
    x = 'Season',
    y = 'Average Goal Diff'
  ) + 
  theme(plot.title = element_text(hjust = 0.5))

# Plot goal difference over time for each league
goal_diff_plots <- list()
leagues <- unique(summary_league_season$league)
for (i in 1:length(leagues)) {
  idx <- i
  league_i <- leagues[i]
  goal_diff_plots[[idx]] <- ggplot() + 
    geom_line(data = summary_league_season %>% filter(league == league_i),
              aes(x = season, y = avg_home_team_goals, group = 1, color = 'Home Team')) + 
    geom_line(data = summary_league_season %>% filter(league == league_i),
              aes(x = season, y = avg_away_team_goals, group = 1, color = 'Away Team')) + 
    geom_point(data = summary_league_season %>% filter(league == league_i),
               aes(x = season, y = avg_home_team_goals, color = 'Home Team')) + 
    geom_point(data = summary_league_season %>% filter(league == league_i),
               aes(x = season, y = avg_away_team_goals, color = 'Away Team')) + 
    geom_label(data = summary_league_season %>% filter(league == league_i),
               aes(x = season, y = avg_home_team_goals,
                   label = sprintf('%0.2f', round(avg_home_team_goals, digits = 3))), nudge_y = 0.03) + 
    geom_label(data = summary_league_season %>% filter(league == league_i),
               aes(x = season, y = avg_away_team_goals,
                   label = sprintf('%0.2f', round(avg_away_team_goals, digits = 3))), nudge_y = 0.03) + 
    expand_limits(y = c(0.8, 2)) +
    labs(
      title = paste0(league_i),
      subtitle = 'Average Goal Scored by an Home and Away Team',
      x = 'Season',
      y = 'Average Goal Scored'
    ) + 
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
         
}
rm(i, idx, league_i)