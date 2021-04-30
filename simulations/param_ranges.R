### Script to find reasonable ranges of parameters to use in simulations for 
### - HA
### - mu
### - team strength correlation

library(tidyverse)
library(here)
library(glue)

source(here('helpers.R'))

league_info <- 
  read_csv(here('league_info.csv')) %>% 
  mutate('restart_date' = as.Date(restart_date, '%m/%d/%y'))

param_stats <- function(league, restart_date) {
  df <- 
    read_leage_csvs(league) %>% 
    filter(date < restart_date) 
  
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
  
  
  df <- 
    df %>% 
    mutate('season' = as.character(season)) %>% 
    mutate('home' = paste(home, season, sep = '_'),
           'away' = paste(away, season, sep = '_')) 
  
  team_ids <- team_codes(df)
  
  
  mu <- mean(df$away_score)
  ha <- mean(df$home_score - df$away_score)
  
  league_ <- gsub("\\s", "_", tolower(league))
  posterior <- read_rds(here(glue('posteriors/bvp_goals_no_corr/{league_}.rds')))
  
  alpha <- apply(posterior$alpha, 2, mean)
  delta <- apply(posterior$delta, 2, mean)
  
  rho <- cor(alpha, delta)
  rho_range <- 
    map_dbl(unique(df$season), ~cor(alpha[grepl(.x, names(team_ids))], delta[grepl(.x, names(team_ids))])) %>% 
    range()
  
  tibble('league' = league,
         'mu' = mu,
         'log_mu' = log(mu),
         'ha' = ha,
         'rho' = rho,
         'rho_min' = rho_range[1],
         'rho_max' = rho_range[2])
  
}

x <- map2_dfr(league_info$alias, league_info$restart_date, param_stats)
write_csv(x, here('param_ranges.csv'))
