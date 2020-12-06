library(tidyverse)
library(here)
source(here('helpers.R'))

league_info <- read_csv(here("league_info.csv"))

draws <- 
  map_dfr(league_info$alias, ~{

    league_ <- gsub("\\s", "_", tolower(.x))
    posterior <- read_rds(here(glue('posteriors/bvp_yc_covid_lambda3_season/{league_}.rds')))
    seasons <- 
      read_leage_csvs(league_) %>% 
      pull(season) %>% 
      unique() %>% 
      gsub('\\d+-', '', .)
    
    posterior$lambda3 %>% 
      as_tibble() %>% 
      set_names(paste0('season_', 1:ncol(.))) %>% 
      mutate('league' = .x) %>% 
      pivot_longer(cols = contains('season'),
                   names_to = 'season',
                   values_to = 'posterior_draw') %>% 
      mutate('season_numeric' = as.numeric(gsub('season_', '', season))) %>% 
      mutate('season' = as.character(seasons[season_numeric]))
      
                   
  }) 


sum_stats <- 
  group_by(draws, league, season_numeric, season) %>% 
  summarise('mean' = mean(posterior_draw),
            'p025' = quantile(posterior_draw, 0.025),
            'p975' = quantile(posterior_draw, 0.975)) %>% 
  ungroup()


ggplot(sum_stats, aes(x = season, y = mean)) +
  facet_wrap(~league) +
  geom_point() +
  labs(x = 'Season',
       y = expression(lambda[3]),
       title = 'Home/Away Goal Correlation Over Time',
       subtitle = expression(paste('Posterior Mean of  ', lambda[3])),
       caption = 'For seasons spanning multiple seasons, a year corresponds to the year during which the season ended')
ggsave('figures/lambda_3_posteriors.png', height = 9/1.2, width = 16/1.2) 

ggplot(sum_stats, aes(x = season, y = mean)) +
  facet_wrap(~league) +
  geom_segment(aes(x = season, xend = season, y = p025, yend = p975), col = 'seagreen', lwd = 1) +
  geom_point() +
  labs(x = 'Season',
       y = expression(lambda[3]),
       title = 'Home/Away Goal Correlation Over Time',
       subtitle = expression(paste('Posterior Mean and 95% Coverage Intervals of  ', lambda[3])),
       caption = 'For seasons spanning multiple seasons, a year corresponds to the year during which the season ended')
ggsave('figures/lambda_3_posteriors_intervals.png', height = 9/1.2, width = 16/1.2)
  