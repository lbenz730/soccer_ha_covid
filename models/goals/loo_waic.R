library(tidyverse)
library(rstan)
library(here)
source(here('helpers.R'))
options(mc.cores=parallel::detectCores())

league_info <- read_csv(here("league_info.csv"))

league <- "German Bundesliga"

########################### Restimation #######################################
df <- read_leage_csvs(league) %>% 
  filter(!is.na(home_score), !is.na(away_score))
df <-
  group_by(df, season) %>%
  mutate('game_id' = 1:n()) %>%
  filter(game_id < max(game_id) - 1) %>%
  ungroup()


### Team IDs
covid_date <- as.Date('2020-05-16')
df <- 
  df %>% 
  mutate('season' = as.character(season)) %>% 
  mutate('season' = case_when(date >= covid_date ~ paste0(season, '_covid'), 
                              T ~ season)) %>% 
  mutate('home' = paste(home, season, sep = '_'),
         'away' = paste(away, season, sep = '_'))
team_ids <- team_codes(df)
df <- 
  select(df, home, away, home_score, away_score, season, date) %>% 
  mutate('home_id' = team_ids[home],
         'away_id' = team_ids[away],
         'pre_covid' = as.numeric(date < covid_date))

### List of Stan Params
stan_data <- list(
  num_clubs = length(team_ids),
  num_games = nrow(df),
  home_team_code = df$home_id,
  away_team_code = df$away_id,
  h_goals = df$home_score,
  a_goals = df$away_score,
  ind_pre = df$pre_covid
)

### Fit Model
m1 <- stan(file = here('stan/bvp_goals_covid_lambda3_fixed.stan'), 
           data = stan_data, 
           seed = 73097,
           chains = 3, 
           iter = 7000, 
           warmup = 2000, 
           control = list(adapt_delta = 0.95))



###################################### No Restimation ##########################
df <- read_leage_csvs(league) %>% 
  filter(!is.na(home_score), !is.na(away_score))
df <-
  group_by(df, season) %>%
  mutate('game_id' = 1:n()) %>%
  filter(game_id < max(game_id) - 1) %>%
  ungroup()

df <- 
  df %>% 
  mutate('season' = as.character(season)) %>% 
  mutate('home' = paste(home, season, sep = '_'),
         'away' = paste(away, season, sep = '_'))
team_ids <- team_codes(df)
df <- 
  select(df, home, away, home_score, away_score, season, date) %>% 
  mutate('home_id' = team_ids[home],
         'away_id' = team_ids[away],
         'pre_covid' = as.numeric(date < covid_date))


### List of Stan Params
stan_data <- list(
  num_clubs = length(team_ids),
  num_games = nrow(df),
  home_team_code = df$home_id,
  away_team_code = df$away_id,
  h_goals = df$home_score,
  a_goals = df$away_score,
  ind_pre = df$pre_covid
)

### Fit Model
m0 <- stan(file = here('stan/bvp_goals_covid_lambda3_fixed.stan'), 
          data = stan_data, 
          seed = 73097,
          chains = 3, 
          iter = 7000, 
          warmup = 2000, 
          control = list(adapt_delta = 0.95))


### Save Models
write_rds(m1, here('model_objects/model_comparison/bundesliga_reestimation.rds'))
write_rds(m0, here('model_objects/model_comparison/bundesliga_no_reestimation.rds'))

##################################### Model Comparison ########################
library(loo)

m1 <- read_rds(here('model_objects/model_comparison/bundesliga_reestimation.rds'))
m0 <- read_rds(here('model_objects/model_comparison/bundesliga_no_reestimation.rds'))
log_lik_1 <- extract_log_lik(m1, merge_chains = F, parameter_name = 'log_lik_1')
r_eff_1 <- relative_eff(exp(log_lik_1))
waic_1 <- waic(log_lik_1)
loo_1 <- loo(log_lik_1, r_eff = r_eff_1)

log_lik_0 <- extract_log_lik(m0, merge_chains = F, parameter_name = 'log_lik_1')
r_eff_0 <- relative_eff(exp(log_lik_0))
waic_0 <- waic(log_lik_0)
loo_0 <- loo(log_lik_0, r_eff = r_eff_0)

loo_compare(waic_0, waic_1)
loo_compare(loo_0, loo_1)


lambda31 <- extract(m1)$lambda3
lambda30 <- extract(m0)$lambda3

tibble('lambda_3' = c(lambda31, lambda30),
       're_estimate' = rep(c('Re-Estimate', 'No Re-estimate'), each = 15000)) %>% 
  ggplot(aes(x = lambda_3)) +
  geom_histogram() +
  facet_wrap(~re_estimate)
