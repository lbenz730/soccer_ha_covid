library(tidyverse)
library(here)
source(here('helpers.R'))

league_info <- read_csv(here("league_info.csv"))

df_yc_all <- NULL

for(i in 1:nrow(league_info)) {
  league <- league_info$alias[i]
  df <- read_leage_csvs(league) %>% 
    filter(!is.na(home_yellow_cards), !is.na(away_yellow_cards))
  
  ### In the Bundesliga, the last 2 games of the seaon is a promotion/regaltion game
  ### between 3rd place in 2nd division and 3rd from last in top division. Filtering out for now
  ### It's likely this is the ccould be the case in other leagues but not worrying as much about that now
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
  
  df_yc <- 
    bind_rows(
      select(df, 'team' = home, 'yc' = home_yellow_cards) %>% mutate('home' = T),
      select(df, 'team' = away, 'yc' = away_yellow_cards) %>% mutate('home' = F)
    )
  
  posterior <- read_rds(here(paste0('posteriors/bvp_yc/', gsub("\\s", "_", tolower(league)), '.rds')))
  
  df_yc <- 
    df_yc %>% 
    group_by(team) %>% 
    summarise('mean_yc' = mean(yc)) %>% 
    arrange(team) %>% 
    mutate('posterior_mean' = posterior$gamma %>% apply(2, mean),
           'league' = league)
  
  
  df_yc_all <- bind_rows(df_yc, df_yc_all)
}

ggplot(df_yc_all, aes(x = mean_yc, y = posterior_mean)) +
  facet_wrap(~league) +
  geom_point() +
  labs(x = 'YC/Game',
       y = 'Posterior Mean')
ggsave(here('eda/figures/team_yc.png'), width = 16/1.2, height = 9/1.2)

df_yc_all  <- NULL
for(i in 1:nrow(league_info)) {
  set.seed(12321)
  league <- league_info$alias[i]
  df <- read_leage_csvs(league) %>% 
    filter(!is.na(home_yellow_cards), !is.na(away_yellow_cards))
  
  ### In the Bundesliga, the last 2 games of the seaon is a promotion/regaltion game
  ### between 3rd place in 2nd division and 3rd from last in top division. Filtering out for now
  ### It's likely this is the ccould be the case in other leagues but not worrying as much about that now
  if(league == "German Bundesliga") {
    df <-
      group_by(df, season) %>%
      mutate('game_id' = 1:n()) %>%
      filter(game_id < max(game_id) - 1) %>%
      ungroup()
  }
  
  df_yc <- 
    bind_rows(
      select(df, 'team' = home, 'yc' = home_yellow_cards) %>% mutate('home' = T),
      select(df, 'team' = away, 'yc' = away_yellow_cards) %>% mutate('home' = F)
    ) 
  
  df_yc <- 
    df_yc %>% 
    mutate('league' = league) %>% 
    group_by(home) %>% 
    mutate('sim_draw' = rpois(n = n(), lambda = mean(yc))) %>% 
    ungroup()
  
  
  
  
  df_yc_all <- bind_rows(df_yc, df_yc_all)
}


df_yc_all <- 
  df_yc_all %>% 
  pivot_longer(cols = c('yc', 'sim_draw')) %>% 
  mutate('sim' = ifelse(name == 'yc', 'Actaul', 'Simulated'))


df_yc_all %>% 
  filter(home) %>% 
  ggplot(aes(x = value)) +
  geom_histogram(aes(fill = sim), position = 'identity', alpha = 0.5, binwidth = 1) +
  facet_wrap(~league) +
  labs(x = '# of YC',
       y = '# of Games',
       title = 'Home',
       fill = '')
ggsave(here('eda/figures/league_home_yc.png'), width = 16/1.2, height = 9/1.2)



df_yc_all %>% 
  filter(!home) %>% 
  ggplot(aes(x = value)) +
  geom_histogram(aes(fill = sim), position = 'identity', alpha = 0.5, binwidth = 1) +
  facet_wrap(~league) +
  labs(x = '# of YC',
       y = '# of Games',
       title = 'Away',
       fill = '')
ggsave(here('eda/figures/league_home_yc.png'), width = 16/1.2, height = 9/1.2)



library(tidyverse)
library(here)
source(here('helpers.R'))

league_info <- read_csv(here("league_info.csv"))

df_yc_all <- NULL

for(i in 1:nrow(league_info)) {
  league <- league_info$alias[i]
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
    semi_join(keep, by = c('away' = 'team', 'season' = 'season'))
  
  df_yc <- 
    bind_rows(
      select(df, 'team' = home, season, 'yc' = home_yellow_cards) %>% mutate('home' = T),
      select(df, 'team' = away, season, 'yc' = away_yellow_cards) %>% mutate('home' = F)
    ) %>% 
    mutate('league' = league,
           'season' = gsub('\\d+-', '', as.character(season)))
  
  
  
  
  
  
  df_yc_all <- bind_rows(df_yc, df_yc_all)
}


df_yc_all %>% 
  group_by(team, season, league) %>% 
  summarise('mean_yc' = mean(yc),
            'n_games' = n()) %>% 
  view()
  ungroup() %>% 
  ggplot(aes(x = mean_yc, y = season)) +
  facet_wrap(~league) +
  geom_point()

