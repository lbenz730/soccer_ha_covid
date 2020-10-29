library(tidyverse)
library(rstan)
library(here)
source(here('helpers.R'))
options(mc.cores=parallel::detectCores())

league_info <- read_csv(here("league_info.csv"))

for(i in 1:nrow(league_info)) {
  league <- league_info$alias[i]
  print(league)
  df <- read_leage_csvs(league) %>% 
    filter(!is.na(home_yellow_cards), !is.na(away_yellow_cards))
  
  ### In the Bundesliga, the last 2 games of the seaon is a promotion/regaltion game
  ### between 3rd place in 2nd division and 3rd from last in top division. Filtering out for now
  ### It's likely this is the ccould be the case in other leagues but not worrying as much about that now
  if(league == "German Bundesliga") {
    df <-
      group_by(df, season) %>%
      mutate('game_id' = 1:n()) %>%
      filter(game_id < max(game_id) - 1) %>%
      ungroup()
  }
  
  ### Team IDs
  team_ids <- team_codes(df)
  df <- 
    select(df, home, away, home_yellow_cards, away_yellow_cards, season, date) %>% 
    mutate('home_id' = team_ids[home],
           'away_id' = team_ids[away])
  
  ### List of Stan Params
  stan_data <- list(
    num_clubs = length(team_ids),
    num_games = nrow(df),
    home_team_code = df$home_id,
    away_team_code = df$away_id,
    h_yc = df$home_yellow_cards,
    a_yc = df$away_yellow_cards
  )
  
  ### Fit Model
  model <- stan(file = here('stan/bvp_yc.stan'), 
                data = stan_data, 
                seed = 73097,
                chains = 3, 
                iter = 7000, 
                warmup = 2000, 
                control = list(adapt_delta = 0.95))
  
  ### Save Model and Posterior
  write_rds(model, here(paste0('model_objects/bvp_yc/', gsub("\\s", "_", tolower(league)), '.rds')))
  posterior <- extract(model)
  write_rds(posterior, here(paste0('posteriors/bvp_yc/', gsub("\\s", "_", tolower(league)), '.rds')))
}
