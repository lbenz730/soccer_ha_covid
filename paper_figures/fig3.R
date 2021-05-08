library(tidyverse)
library(here)
source(here('helpers.R'))

league_info <- read_csv(here("league_info.csv"))

league <- "German Bundesliga"

league_ <- gsub("\\s", "_", tolower(league))
df <- read_leage_csvs(league_)
posterior <- read_rds(here(glue('posteriors/bvp_goals_no_corr/{league_}.rds')))

df <- 
  df %>% 
  mutate('season' = as.character(season)) %>% 
  mutate('home' = paste(home, season, sep = '_'),
         'away' = paste(away, season, sep = '_')) %>% 
  mutate('season_numeric' = as.numeric(as.factor(season)))

### Filter Out Games for relegation playoffs
keep <- 
  df %>% 
  select(home, away, season) %>% 
  pivot_longer(c('home', 'away'),
               values_to = 'team') %>% 
  group_by(team, season) %>% 
  count() %>% 
  ungroup() %>% 
  filter(n > 3) 

df <- 
  df %>% 
  semi_join(keep, by = c('home' = 'team', 'season' = 'season')) %>% 
  semi_join(keep, by = c('away' = 'team', 'season' = 'season'))

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
  filter(season == "2015-2016")

ggplot(team_strengths, aes(x = alpha, y = delta)) +
  geom_vline(xintercept = 0, lty = 2, alpha = 0.8, col = 'seagreen') +
  geom_hline(yintercept = 0, lty = 2, alpha = 0.8, col = 'seagreen') +
  ggrepel::geom_label_repel(aes(label = team), size = 4) +
  scale_x_continuous(limits  = c(-0.6, 0.6)) +
  scale_y_continuous(limits  = c(-0.5, 0.5)) +
  annotate(geom = 'text', x = 0.45, y = -0.5, label = 'Good Attack/Good Defense', fontface = 'bold') +
  annotate(geom = 'text', x = -0.45, y = -0.5, label = 'Bad Attack/Good Defense', fontface = 'bold') +
  annotate(geom = 'text', x = 0.45, y = 0.5, label = 'Good Attack/Bad Defense', fontface = 'bold') +
  annotate(geom = 'text', x = -0.45, y = 0.5, label = 'Bad Attack/Bad Defense', fontface = 'bold') +
  labs(x = 'Attacking Team Strength',
       y = 'Defensive Team Strength',
       title = 'German Bundesliga Team Strengths',
       subtitle = '2015-16')
ggsave('figures/figure3.png', height = 9/1.5, width = 16/1.5)
