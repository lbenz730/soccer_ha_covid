library(tidyverse)
library(here)
source(here('helpers.R'))

league_info <- read_csv(here("league_info.csv"))
df_stats <- 
  map_dfr(league_info$alias, ~{
    league <- .x
    df <- read_leage_csvs(league) %>% 
      filter(!is.na(home_yellow_cards), !is.na(away_yellow_cards))
    
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
      semi_join(keep, by = c('away' = 'team', 'season' = 'season')) %>% 
      mutate('league' = league) %>% 
      mutate('season' = gsub('\\d+-', '', as.character(season)))
    
    df %>% 
      select(league, 
             season, 
             home_score,
             away_score, 
             'home_yc' = home_yellow_cards, 
             'away_yc' = away_yellow_cards)
  })


df_stats <- 
  df_stats %>% 
  pivot_longer(matches('home|away'),
               names_sep = '_',
               names_to = c('location', 'metric'))


df_stats2 <- 
  df_stats %>% 
  group_by(league, season, metric, location) %>% 
  summarise('mean' = mean(value)) %>% 
  ungroup()


ggplot(filter(df_stats2, metric == 'score'), aes(x = season, y = mean)) +
  facet_wrap(~league) +
  geom_point(size = 2, aes(color = location)) + 
  labs(x = 'Season',
       y = "Mean Goals/Game")

ggplot(filter(df_stats2, metric == 'yc'), aes(x = season, y = mean)) +
  facet_wrap(~league) +
  geom_point(size = 2, aes(color = location)) + 
  labs(x = 'Season',
       y = "Mean Yellow Cards/Game")
