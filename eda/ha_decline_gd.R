library(tidyverse)
library(here)
library(ggridges)
library(kableExtra)
library(knitr)
source(here('helpers.R'))

league_info <- read_csv(here('league_info.csv'))

### Directory to Read in From
directory <- "bvp_goals_no_corr"

### Read in Posterior HA Draws
draws <- 
  map_dfr(league_info$alias, ~{
    league_ <- gsub("\\s", "_", tolower(.x))
    posterior <- try(suppressWarnings(read_rds(here(glue('posteriors/{directory}/{league_}.rds')))))
    if(any(class(posterior) == 'try-error')) {
      NULL
    } else {
      n_draw <-  length(posterior$home_field_pre)
      tibble('league' = .x,
             'posterior_draw' = c(posterior$home_field_pre, posterior$home_field_post),
             'mu' = mean(posterior$mu),
             'hfa_type' = rep(c('Pre-COVID (w/ Fans)', 'Post-COVID (w/out Fans)'), each = n_draw))
    }
  }) 

df_means <- 
  group_by(draws, league) %>% 
  summarise('mean_pre' = mean(posterior_draw[hfa_type == 'Pre-COVID (w/ Fans)']),
            'mean_post'  = mean(posterior_draw[hfa_type == 'Post-COVID (w/out Fans)']),
            'mu' = mu[1]) %>% 
  inner_join(select(league_info, 'league' = alias, logo_url))

df_means %>% 
  filter(!league %in% c('Norwegian Eliteserien', 'Swedish Allsvenskan')) %>% 
  mutate('ha_before' = exp(mean_pre + mu) - exp(mu),
         'ha_after' = exp(mean_post + mu) - exp(mu)) %>% 
  summarise('mean_before' = mean(ha_before),
            'mean_after' = mean(ha_after))
            

         