library(tidyverse)
library(here)
library(ggridges)
source(here('helpers.R'))

league_info <- read_csv(here('league_info.csv'))
draws <- 
  map_dfr(league_info$alias, ~{
    league_ <- gsub("\\s", "_", tolower(.x))
    posterior <- read_rds(here(glue('posteriors/bvp_goals_covid/{league_}.rds')))
    n_draw <-  length(posterior$home_field_pre)
    df <- tibble('league' = .x,
                 'posterior_draw' = c(posterior$home_field_pre, posterior$home_field_post),
                 'hfa_type' = rep(c('Pre-COVID (w/ Fans)', 'Post-COVID (w/out Fans)'), each = n_draw),
                 'reestimation' = T)
    
    posterior <- read_rds(here(glue('posteriors/bvp_goals_covid_no_reestimation/{league_}.rds')))
    n_draw <-  length(posterior$home_field_pre)
    df2 <- tibble('league' = .x,
                  'posterior_draw' = c(posterior$home_field_pre, posterior$home_field_post),
                  'hfa_type' = rep(c('Pre-COVID (w/ Fans)', 'Post-COVID (w/out Fans)'), each = n_draw),
                  'reestimation' = F)
    
    bind_rows(df, df2)
  }) 



df_means <- 
  draws %>% 
  group_by(league, hfa_type, reestimation) %>% 
  summarise('mean' = median(posterior_draw))

ggplot(filter(df_means, hfa_type == 'Post-COVID (w/out Fans)'), 
              aes(x = mean, y =fct_reorder(league, mean))) +
  geom_point(aes(color = hfa_type, shape = reestimation), size = 5) +
  geom_point(data = filter(df_means, hfa_type != 'Post-COVID (w/out Fans)'), 
             aes(color = hfa_type, shape = reestimation), size = 5) +
  labs(x = 'Post COVID HFA Posterior Mean (Log Scale)',
       y = 'League',
       title = 'Comparison of HFA Posterior Means',
       subtitle = 'Re-estimation of Team Strengths After COVID',
       shape = 'Re-estimation of Team Strengths')
ggsave(here('eda/figures/reestimation.png'), width = 16/1.2, height = 9/1.2)       


library(loo)
league_ <- "german_bundesliga"
model_1 <- read_rds(here(glue('model_objects/bvp_goals_covid/{league_}.rds')))
model_2 <- read_rds(here(glue('model_objects/bvp_goals_covid_no_reestimation/{league_}.rds')))
log_lik_1 <- extract_log_lik(model_1)
loo_1 <- loo(model_1)
