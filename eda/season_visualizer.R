library(tidyverse)
library(here)
source(here('helpers.R'))

league_info <- read_csv(here("league_info.csv"))

# league <- "German 2. Bundesliga"
league <- "English Premier League"

league_ <- gsub("\\s", "_", tolower(league))
df <- read_leage_csvs(league_)
posterior <- read_rds(here(glue('posteriors/bvp_goals_emprical_bayes/{league_}.rds')))

df <- read_leage_csvs(league) %>% 
  filter(!is.na(home_score), !is.na(away_score))

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

### Team IDs
df <- 
  df %>% 
  mutate('season' = as.character(season)) %>% 
  mutate('home' = paste(home, season, sep = '_'),
         'away' = paste(away, season, sep = '_')) 
team_ids <- team_codes(df)


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
  filter(season != "2015-2016")

ggplot(team_strengths, aes(x = alpha, y = delta)) +
  facet_wrap(~season) +
  geom_vline(xintercept = 0, lty = 2, alpha = 0.8, col = 'seagreen') +
  geom_hline(yintercept = 0, lty = 2, alpha = 0.8, col = 'seagreen') +
  ggrepel::geom_label_repel(aes(label = team), size = 2) +
  scale_x_continuous(limits  = c(-0.9, 0.9)) +
  scale_y_continuous(limits  = c(-0.9, 0.9)) +
  annotate(geom = 'text', x = 0.5, y = -0.85, label = 'Good Offense/Good Defense') +
  annotate(geom = 'text', x = -0.5, y = -0.85, label = 'Bad Offense/Good Defense') +
  annotate(geom = 'text', x = 0.5, y = 0.85, label = 'Good Offense/Bad Defense') +
  annotate(geom = 'text', x = -0.5, y = 0.85, label = 'Bad Offense/Bad Defense') +
  labs(x = 'Offensive Team Strength',
       y = 'Defensive Team Strength',
       title = 'English Premier League Team Strengths',
       subtitle = '2016-17 Through 2019-20')
ggsave('figures/epl_team_strengths.png', height = 9/1.2, width = 16/1.2)
