library(tidyverse)
library(here)
source(here('helpers.R'))

league_info <- read_csv(here('league_info.csv'))

### Goals
probs <- 
  map_dfr(league_info$alias, ~{
    league_ <- gsub("\\s", "_", tolower(.x))
    posterior0 <- read_rds(here(glue('posteriors/bvp_goals_no_corr/{league_}.rds')))
    posterior1 <- read_rds(here(glue('posteriors/bvp_goals_lambda3/{league_}.rds')))
    tibble('league' = .x,
           'p_decrease_no_corr' = mean(posterior0$home_field_pre > posterior0$home_field_post),
           'p_decrease_corr' = mean(posterior1$home_field_pre > posterior1$home_field_post))
    
  }) 

probs$abs_change <- abs(probs$p_decrease_no_corr - probs$p_decrease_corr)


### YC
probs <- 
  map_dfr(league_info$alias, ~{
    league_ <- gsub("\\s", "_", tolower(.x))
    posterior0 <- read_rds(here(glue('posteriors/bvp_yc_no_corr/{league_}.rds')))
    posterior1 <- read_rds(here(glue('posteriors/bvp_yc_lambda3/{league_}.rds')))
    tibble('league' = .x,
           'p_decrease_no_corr' = mean(posterior0$home_field_pre < posterior0$home_field_post),
           'p_decrease_corr' = mean(posterior1$home_field_pre < posterior1$home_field_post))
    
  }) 

probs$abs_change <- abs(probs$p_decrease_no_corr - probs$p_decrease_corr)

