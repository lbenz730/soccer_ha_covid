library(tidyverse)
library(here)
library(ggridges)
source(here('helpers.R'))

league_info <- read_csv(here('league_info.csv'))

### Read in Posterior HA Draws
posterior_means <- 
  map_dfr(league_info$alias, ~{
    league_ <- gsub("\\s", "_", tolower(.x))
    goals_posterior <- try(suppressWarnings(read_rds(here(glue('posteriors/bvp_goals_no_corr/{league_}.rds')))))
    yc_posterior <- try(suppressWarnings(read_rds(here(glue('posteriors/bvp_yc_no_corr/{league_}.rds')))))
    
    tibble('league' = .x,
           'goals_ha_pre' = mean(goals_posterior$home_field_pre),
           'goals_ha_post' = mean(goals_posterior$home_field_post),
           'yc_ha_pre' = mean(yc_posterior$home_field_pre),
           'yc_ha_post' = mean(yc_posterior$home_field_post)
    )
  })

write_csv(posterior_means, here('models/empirical_baselines.csv'))
