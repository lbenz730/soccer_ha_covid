library(tidyverse)
library(here)
library(rstan)
library(knitr)
library(kableExtra)
source(here('helpers.R'))

league_info <- read_csv(here('league_info.csv'))

directory <- "bvp_goals_no_corr"
df_stats <- 
  map_dfr(league_info$alias, ~{
    league <- .x
    league_ <- gsub("\\s", "_", tolower(league))
    model <- read_rds(here(glue('model_objects/{directory}/{league_}.rds')))
    
    as.data.frame(summary(model)[[1]]) %>%
      mutate(Parameter = as.factor(gsub("\\[.*]", "", rownames(.)))) %>% 
      group_by(Parameter) %>% 
      summarise('rhat_pct' = mean(abs(Rhat - 1) < 0.1),
                # 'max_rhat' = max(Rhat),
                # 'min_rhat' = min(Rhat),
                'mean_ess' = mean(n_eff)) %>% 
      mutate('league' = league)
  })

df_stats %>% 
  filter(Parameter %in% c('alpha', 'delta', 'sigma_a', 'sigma_d', 'gamma', 
                          'sigma_g', 'fixed_cov',
                          'home_field_pre', 'home_field_post', 'mu')) %>%
  ungroup() %>% 
  pivot_wider(names_from = 'Parameter',
              values_from = c( 'mean_ess')) %>% 
  inner_join( group_by(df_stats, league) %>% 
                summarise('rhat_pct' = mean(rhat_pct)) ) %>% 
  select(league, home_field_pre, home_field_post, mu, alpha, delta, sigma_a, sigma_d) %>% 
  arrange(league) %>% 
  # mutate_if(is.numeric, round) %>% 
  xtable::xtable(digits = 0) %>% 
  print(include.rownames = F)
  # kable(escape = F, align = 'c', 'latex') %>% 
  # kable_styling(latex_options = "striped") %>% 
  # add_header_above(c(" " = 2, "Effective Sample Size" = 7), bold = T) %>%
  # add_header_above(c("Goals (No Lambda 3)" = 9), bold = T, font_size = 24) %>%
  # add_footnote('rhat_pct = % of all RHat Values for all parameters between (0.9, 1.1)')


directory <- "bvp_yc_lambda3"
df_stats <- 
  map_dfr(league_info$alias, ~{
    league <- .x
    league_ <- gsub("\\s", "_", tolower(league))
    model <- read_rds(here(glue('model_objects/{directory}/{league_}.rds')))
    
    as.data.frame(summary(model)[[1]]) %>%
      mutate(Parameter = as.factor(gsub("\\[.*]", "", rownames(.)))) %>% 
      group_by(Parameter) %>% 
      summarise('rhat_pct' = mean(abs(Rhat - 1) < 0.1),
                'max_rhat' = max(Rhat),
                'min_rhat' = min(Rhat),
                'mean_ess' = mean(n_eff)) %>% 
      mutate('league' = league)
  })

df_stats %>% 
  filter(Parameter %in% c('alpha', 'delta', 'sigma_a', '
                          sigma_d', 'gamma', 
                          'sigma_g', 'fixed_cov',
                          'home_field_pre', 'home_field_post', 'mu')) %>%
  ungroup() %>% 
  pivot_wider(names_from = 'Parameter',
              values_from = c( 'mean_ess')) %>% 
  inner_join( group_by(df_stats, league) %>% 
                 summarise('rhat_pct' = mean(rhat_pct)) ) %>% 
  select(league, rhat_pct, everything()) %>% 
  arrange(league) %>% 
  mutate_if(is.numeric, round) %>% 
  kable(escape = F, align = 'c') %>% 
  kable_styling("striped") %>% 
  add_header_above(c(" " = 2, "Effective Sample Size" = 6), bold = T) %>%
  add_header_above(c("Yellow Cards Model (w/ Lambda 3)" = 8), bold = T, font_size = 24) %>% 
  add_footnote('rhat_pct = % of all RHat Values for all parameters between (0.9, 1.1)')


  
  