library(tidyverse)
library(here)
source(here('helpers.R'))

league_info <- read_csv(here("league_info.csv"))

league <- "English Premier League"

league_ <- gsub("\\s", "_", tolower(league))
df <- read_leage_csvs(league_)
posterior <- read_rds(here(glue('posteriors/bvp_goals_no_corr/{league_}.rds')))

df <- 
  df %>% 
  mutate('season' = as.character(season)) %>% 
  mutate('home' = paste(home, season, sep = '_'),
         'away' = paste(away, season, sep = '_')) %>% 
  mutate('season_numeric' = as.numeric(as.factor(season)))
team_ids <- team_codes(df)
df <- 
  select(df, home, away, home_score, away_score, season, date, season_numeric) %>% 
  mutate('home_id' = team_ids[home],
         'away_id' = team_ids[away])

alphas <- posterior$alpha
deltas <- posterior$delta

alphas <- 
  alphas %>% 
  as_tibble() %>% 
  set_names(names(team_ids)) %>% 
  pivot_longer(cols = everything(),
               names_sep = '_',
               names_to = c('team', 'season'),
               values_to = 'draw') %>% 
  mutate('parameter' = 'alpha')

deltas  <- 
  deltas %>% 
  as_tibble() %>% 
  set_names(names(team_ids)) %>% 
  pivot_longer(cols = everything(),
               names_sep = '_',
               names_to = c('team', 'season'),
               values_to = 'draw') %>% 
  mutate('parameter' = 'delta')

team_strengths <- 
  bind_rows(alphas, deltas) %>% 
  group_by(team, season, parameter) %>% 
  summarise('posterior_mean' = mean(draw)) %>% 
  ungroup() %>% 
  pivot_wider(names_from ='parameter', 
              values_from = 'posterior_mean') %>% 
  filter(season == "2016-2017")

ggplot(team_strengths, aes(x = alpha, y = delta)) +
  geom_vline(xintercept = 0, lty = 2, alpha = 0.8, col = 'seagreen') +
  geom_hline(yintercept = 0, lty = 2, alpha = 0.8, col = 'seagreen') +
  ggrepel::geom_label_repel(aes(label = team), size = 4) +
  scale_x_continuous(limits  = c(-0.75, 0.75)) +
  scale_y_continuous(limits  = c(-0.6, 0.6)) +
  annotate(geom = 'text', x = 0.5, y = -0.6, label = 'Good Attack/Good Defense', fontface = 'bold') +
  annotate(geom = 'text', x = -0.5, y = -0.6, label = 'Bad Attack/Good Defense', fontface = 'bold') +
  annotate(geom = 'text', x = 0.5, y = 0.6, label = 'Good Attack/Bad Defense', fontface = 'bold') +
  annotate(geom = 'text', x = -0.5, y = 0.6, label = 'Bad Attack/Bad Defense', fontface = 'bold') +
  labs(x = 'Attacking Team Strength',
       y = 'Defensive Team Strength',
       title = 'English Premier League Team Strengths',
       subtitle = '2016-17')
ggsave('figures/epl_team_strengths.png', height = 9/1.5, width = 16/1.5)
