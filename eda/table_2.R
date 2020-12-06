library(tidyverse)
library(here)
source(here('helpers.R'))

league_info <- 
  read_csv(here("league_info.csv")) %>% 
  select('league' = alias, restart_date) %>% 
  mutate('restart_date' = as.Date(restart_date, '%m/%d/%y'))

.x <- league_info$league[14]
.y <- league_info$restart_date[14]

league_summary <- 
  map2_dfr(league_info$league, league_info$restart_date, ~{
  league_ <- gsub("\\s", "_", tolower(.x))
  df <- read_leage_csvs(league_)
  
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
  covid_date <- .y
  df <- 
    df %>% 
    mutate('season' = as.character(season)) %>% 
    mutate('home' = paste(home, season, sep = '_'),
           'away' = paste(away, season, sep = '_')) %>% 
    mutate('season_numeric' = as.numeric(as.factor(season)))
  team_ids <- team_codes(df)
  df <- 
    df %>% 
    mutate('home_id' = team_ids[home],
           'away_id' = team_ids[away],
           'pre_covid' = as.numeric(date < covid_date))
  
  
  tibble('league' = .x,
         'restart_date' = .y,
         'n_games_goals_pre_covid' = nrow(df %>% filter(!is.na(home_score), !is.na(away_score)) %>% filter(pre_covid == 1)),
         'n_games_yc_pre_covid' = nrow(df %>% filter(!is.na(home_yellow_cards), !is.na(away_yellow_cards)) %>% filter(pre_covid == 1)),
         'n_games_goals_post_covid' = nrow(df %>% filter(!is.na(home_score), !is.na(away_score)) %>% filter(pre_covid == 0)),
         'n_games_yc_post_covid' = nrow(df %>% filter(!is.na(home_yellow_cards), !is.na(away_yellow_cards)) %>% filter(pre_covid == 0)),
         'n_team_seasons' = length(team_ids),
         'n_games_no_fans_pre_covid' = nrow(df %>% filter(pre_covid == 1, attendance == 0, !is.na(home_score), as.character(season) > '2018-19')),
         'n_games_fans_post_covid' = nrow(df %>% filter(pre_covid == 0, attendance > 0, !is.na(home_score))),
         'mean_fans_pre_covid' = mean(df$attendance[df$pre_covid == 1 & !is.na(df$home_score)]),
         'mean_fans_post_covid_w_fans' = mean(df$attendance[df$pre_covid == 0 & df$attendance > 0 & !is.na(df$home_score)]))
})
