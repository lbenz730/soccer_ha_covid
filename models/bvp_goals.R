library(tidyverse)
library(rstan)
source('../helpers.R')
options(mc.cores=parallel::detectCores())

df <- read_leage_csvs("English Premier League")

### In the Bundesliga, the last 2 games of the seaon is a promotion/regaltion game
### between 3rd place in 2nd division and 3rd from last in top division. Filtering out for now
# df <- 
#   group_by(df, season) %>% 
#   mutate('game_id' = 1:n()) %>% 
#   filter(game_id < max(game_id) - 1) %>% 
#   ungroup()

### Team IDs
team_ids <- team_codes(df)
df <- 
  select(df, home, away, home_score, away_score, season, date) %>% 
  mutate('home_id' = team_ids[home],
         'away_id' = team_ids[away])

#enframe(team_ids, name = 'team', value = 'id')
stan_data <- list(
  num_clubs = length(team_ids),
  num_games = nrow(df),
  home_team_code = df$home_id,
  away_team_code = df$away_id,
  h_goals = df$home_score,
  a_goals = df$away_score
)

model <- stan(file = '../stan/bvp_goals.stan', data = stan_data, chains = 3, 
              iter = 5000, warmup = 1000, control = list(adapt_delta = 0.95))

posterior <- extract(model)
home_field <- mean(posterior$home_field)
alphas <- apply(posterior$alpha, 2, mean)
deltas <- apply(posterior$delta, 2, mean)
rhos <- apply(posterior$rho, 2, mean)

