### Function to Run Simulations
library(tidyverse)
library(here)
library(rstan)
library(glue)

run_simulation <- function(x) {
  sim_run <- x$run
  sim_id <- x$sim_id
  lambda3 <- x$lambda3
  team_strength_correlation <- x$team_strength_correlation
  
  cat('\nRUNNING SIM:', sim_id, '\n')
  
  
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
  bivpois_estimated_ha <- exp(bivpois_eta + bivpois_mu) - exp(bivpois_mu)
  bivpois_bias <- mean(bivpois_estimated_ha -  x$home_advantage)
  bivpois_mse <- mean((bivpois_estimated_ha -  x$home_advantage)^2)
  
  ### Fit Bayes LM Paired Comparison
  pc_stan_data <- 
    list(
    'num_clubs' = nrow(team_codes),
    'num_games' = nrow(fit_data),
    'home' = fit_data$home_code,
    'away' = fit_data$away_code,
    'goal_diff' = fit_data$h_goals - fit_data$a_goals
  )
  
  pc_stan <- stan(file = here("simulations/paired_comp.stan"), 
                  seed = 12321,
                  data = pc_stan_data,
                  chains = 2, iter = 5000, warmup = 2000, init = "random", thin = 5,
                  cores = 2, control = list(adapt_delta = 0.99))
  
  pc_pars <- rstan::extract(pc_stan, pars = 'alpha')
  pc_eta <- mean(pc_pars$alpha)
  pc_bias <- mean(pc_eta - x$home_advantage)
  pc_mse <- mean((pc_eta - x$home_advantage)^2)
  
  ### FIT LM:
  fit_data <- 
    fit_data %>%
    mutate(goal_diff = h_goals - a_goals)
  fit_lm <- lm(goal_diff ~ home + away, data = fit_data)
  lm_eta <- fit_lm$coefficients[1]
  lm_eta_bias <- mean(lm_eta - x$home_advantage)
  lm_eta_mse <- mean((lm_eta - x$home_advantage)^2)
  
  ### Return
  df_out <- 
    tibble(
      'method' = x$method,
      'true_params' = x$home_advantage,
      'bivpois_eta' = bivpois_eta,
      'bivpois_estimated_ha' =  bivpois_estimated_ha,
      'bivpois_bias' = bivpois_bias,
      'bivpois_mse' = bivpois_mse, 
      'pc_eta' = pc_eta,
      'pc_bias' = pc_bias,
      'pc_mse' = pc_mse,
      'lm_eta' = lm_eta, 
      'lm_bias' = lm_eta_bias, 
      'lm_mse' = lm_eta_mse,
      'team_strength_correlation' = team_strength_correlation,
      'mu' = ifelse(is.null(x$mu), NA, x$mu)
    )
  

  ### Write File
  if(!dir.exists(here(glue('simulations/sim_files/v2_sims')))) {
    dir.create(here(glue('simulations/sim_files/v2_sims')))
  }
  write_csv(df_out, here(glue('simulations/sim_files/v2_sims/{sim_id}.csv')))
  
  return(df_out)
}