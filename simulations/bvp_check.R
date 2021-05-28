library(tidyverse)
library(here)
library(glue)
library(rstan)

source(here('helpers.R'))


### Bivariate Poission Data Generation
generate_bivpois_big <- function(num_club, 
                                 team_strength_correlation, 
                                 lambda3, 
                                 home_advantage, 
                                 mu,
                                 n_seasons) {
  
  ### Covariance Matrix for Team Strengths
  covariance_matrix <- matrix(c(0.35^2, 
                                0.35^2 * team_strength_correlation, 
                                0.35^2 * team_strength_correlation, 
                                0.35^2), 
                              nrow = 2, byrow = T)
  
  ### Sample Team Strengths
  team_strengths <- MASS::mvrnorm(n = num_club, mu = c(0,0), Sigma = covariance_matrix)
  
  teams <- 
    tibble('club' = paste0("club", sprintf("%02d", seq_len(num_club))),
           'attack' = team_strengths[,1],
           'defend' = team_strengths[,2])
  
  ### Create Schedule
  games <- 
    teams %>%
    select(club) %>%
    flatten_chr() 
  games <- expand.grid(home = games, away = games)
  
  ### Duplicate Schedule n_seasons times
  games <- map_dfr(1:n_seasons, ~games)
  
  games <- 
    games %>%
    filter(home != away) %>%
    left_join(teams, by = c("home" = "club")) %>%
    rename(h_att = attack, h_def = defend) %>%
    left_join(teams, by = c("away" = "club")) %>%
    rename(a_att = attack, a_def = defend) %>%
    mutate(
      'lambda1' = exp(log(home_advantage + mu) + h_att + a_def),
      'lambda2' = exp(log(mu) + a_att + h_def),
      'h_goals' = rpois(n = nrow(.), lambda = (lambda1 + lambda3)),
      'a_goals' = rpois(n = nrow(.), lambda = (lambda2 + lambda3)),
      'home_game' = 1) %>%
    select(home, away, h_goals, a_goals, home_game)
  
  sim_out <- 
    list(
      'method' = "bivpois",
      'teams' = teams,
      'games' = games,
      'mu' = mu,
      'home_advantage' = home_advantage,
      'team_strength_correlation' = team_strength_correlation,
      'lambda3' = lambda3
    )
}

x <- 
  generate_bivpois_big(num_club = 20,
                     team_strength_correlation = -0.8,
                     lambda3 = 0,
                     home_advantage = 0.25,
                     mu = exp(0.15),
                     n_seasons = 100)


team_codes <- 
  tibble('club' = sort(unique(c(x$games$home, x$games$away)))) %>%
  mutate('code' = seq_len(nrow(.)))

fit_data <- 
  left_join(x$games, team_codes, by = c("home" = "club")) %>%
  rename(home_code = code) %>%
  left_join(team_codes, by = c("away" = "club")) %>%
  rename(away_code = code)

stan_data <- 
  list('num_clubs' = nrow(team_codes),
       'num_games' = nrow(fit_data),
       'home' = fit_data$home_code,
       'away' = fit_data$away_code,
       'h_goals' = fit_data$h_goals,
       'a_goals' = fit_data$a_goals,
       'homeg' = fit_data$home_game)

### Fit BVP MODEL 
bivpois <- stan(file = here("simulations/biv_pois.stan"), 
                seed = 12321,
                data = stan_data,
                chains = 2, iter = 5000, warmup = 2000, init = "random", thin = 5,
                cores = 2, control = list(adapt_delta = 0.99),
                  
                verbose = F)

bivpois_params <- rstan::extract(bivpois, pars = c("mu", "eta", "alpha", "delta"))

bivpois_eta <- mean(bivpois_params$eta)
bivpois_mu <- mean(bivpois_params$mu)

### 0.246624
exp(bivpois_eta + bivpois_mu) - exp(bivpois_mu)

### 0.2201
exp(bivpois_eta ) - 1

