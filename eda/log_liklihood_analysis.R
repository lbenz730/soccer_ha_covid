library(tidyverse)
library(here)
library(ggridges)
source(here('helpers.R'))

league_info <- read_csv(here('league_info.csv'))

### Read in Log Lik for goals
df_ll <- 
  map_dfr(league_info$alias, ~{
    league_ <- gsub("\\s", "_", tolower(.x))
    posterior <- try(suppressWarnings(read_rds(here(glue('posteriors/bvp_goals_no_corr/{league_}.rds')))))
    posterior_l3 <- try(suppressWarnings(read_rds(here(glue('posteriors/bvp_goals_lambda3/{league_}.rds')))))
    if(class(posterior_l3) != 'try-error') {
      
      tibble('league' = .x,
             'log_lik_1' = mean(posterior$log_lik_1),
             'log_lik_2' = mean(posterior$log_lik_2),
             'log_lik' = mean(posterior$log_lik_1 + posterior$log_lik_2),
             'log_lik_1_lambda3' = mean(posterior_l3$log_lik_1),
             'log_lik_2_lambda3' = mean(posterior_l3$log_lik_2),
             'log_lik_lambda3' = mean(posterior_l3$log_lik_1 + posterior_l3$log_lik_2)
             
      )
    }
  })

filter(df_ll, log_lik < log_lik_lambda3) ### Correlation Preferable
filter(df_ll, log_lik > log_lik_lambda3) ### No Correlation Preferable


### Read in Log Lik for goals
df_ll_yc <- 
  map_dfr(league_info$alias, ~{
    league_ <- gsub("\\s", "_", tolower(.x))
    posterior <- try(suppressWarnings(read_rds(here(glue('posteriors/bvp_yc_no_corr/{league_}.rds')))))
    posterior_l3 <- try(suppressWarnings(read_rds(here(glue('posteriors/bvp_yc_lambda3/{league_}.rds')))))
    if(class(posterior_l3) != 'try-error') {
      
      tibble('league' = .x,
             'log_lik_1' = mean(posterior$log_lik_1),
             'log_lik_2' = mean(posterior$log_lik_2),
             'log_lik' = mean(posterior$log_lik_1 + posterior$log_lik_2),
             'log_lik_1_lambda3' = mean(posterior_l3$log_lik_1),
             'log_lik_2_lambda3' = mean(posterior_l3$log_lik_2),
             'log_lik_lambda3' = mean(posterior_l3$log_lik_1 + posterior_l3$log_lik_2)
             
      )
    }
  })

filter(df_ll_yc, log_lik < log_lik_lambda3) ### Correlation Preferable
filter(df_ll_yc, log_lik > log_lik_lambda3) ### No Correlation Preferable


