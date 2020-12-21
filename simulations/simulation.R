library(tidyverse)
library(here)
library(rstan)
library(glue)
source(here('helpers.R'))

set.seed(0)

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### SIMULATION: DATA GENERATION
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
generate_bivpois <- function(run, num_club) {
  
  teams <- tibble(
    club = paste0("club", sprintf("%02d", seq_len(num_club))),
    attack = rnorm(n = num_club, mean = 0, sd = 0.35),
    defend = rnorm(n = num_club, mean = 0, sd = 0.35),
    #cov = rnorm(n = num_club, mean = 0, sd = 0.1)
    cov = 0
  )
  
  games <- teams %>%
    select(club) %>%
    flatten_chr() 
  games <- expand.grid(home = games, away = games)
  
  home_advantage <- runif(1, 0, log(2))
  
  games <- games %>%
    filter(home != away) %>%
    left_join(teams, by = c("home" = "club")) %>%
    rename(h_att = attack, h_def = defend, h_cov = cov) %>%
    left_join(teams, by = c("away" = "club")) %>%
    rename(a_att = attack, a_def = defend, a_cov = cov) %>%
    mutate(
      lambda1 = exp(0 + home_advantage + h_att + a_def),
      lambda2 = exp(0 + a_att + h_def),
      h_goals = rpois(n = nrow(.), lambda = (lambda1)),
      a_goals = rpois(n = nrow(.), lambda = (lambda2)),
      home_game = 1
    ) %>%
    select(home, away, h_goals, a_goals, home_game)
  
  list(
    method = "bivpois",
    teams = teams,
    games = games,
    home_advantage = home_advantage,
    run = run
  )
}


# Define parameters for the simulation
n_reps <- 200

# Create data sets
bivpois_data <- lapply(X = 1:n_reps, FUN = generate_bivpois, num_club = 20)

simulation_data <- c(
  bivpois_data
)


## SIMULATION 1: BVP
simulation_fun <- function(x) {
  #x <- simulation_data[[1]]
  sim_run <- x$run
  team_codes <- 
    tibble(
    club = sort(unique(c(x$games$home, x$games$away)))) %>%
    mutate(code = seq_len(nrow(.)))
  
  fit_data <- 
    left_join(x$games, team_codes, by = c("home" = "club")) %>%
    rename(home_code = code) %>%
    left_join(team_codes, by = c("away" = "club")) %>%
    rename(away_code = code)
  
  stan_data <- list(
    num_clubs = nrow(team_codes),
    num_games = nrow(fit_data),
    home = fit_data$home_code,
    away = fit_data$away_code,
    h_goals = fit_data$h_goals,
    a_goals = fit_data$a_goals,
    homeg = fit_data$home_game
  )
  
  ### Fit BVP MODEL
  
  bivpois <- stan(file = here("simulations/biv_pois.stan"), 
                  seed = 12321,
                  data = stan_data,
                  chains = 2, iter = 5000, warmup = 2000, init = "random", thin = 5,
                  cores = 2, control = list(adapt_delta = 0.99))
  
  bivpois_params <- rstan::extract(bivpois, pars = c("mu", "eta", "alpha", "delta"))
  bivpois_eta <- mean(bivpois_params$eta)
  bivpois_eta_bias <- mean(bivpois_eta - x$home_advantage)
  bivpois_eta_mse <- mean((bivpois_eta - x$home_advantage)^2)
  
  
  
  ### FIT LM:
  fit_data <- fit_data %>%
    mutate(goal_diff = h_goals - a_goals)
  fit_lm <- lm(goal_diff ~ home + away, data = fit_data)
  summary(fit_lm)
  lm_eta <- fit_lm$coefficients[1]
  lm_eta_bias <- mean(lm_eta - x$home_advantage)
  lm_eta_mse <- mean((lm_eta - x$home_advantage)^2)
  df_out <- 
    tibble(
      true_params = x$home_advantage,
      bivpois_eta = bivpois_eta,
      bivpois_eta_bias = bivpois_eta_bias,
      bivpois_eta_mse = bivpois_eta_mse, 
      lm_eta = lm_eta, 
      lm_eta_bias = lm_eta_bias, 
      lm_eta_mse = lm_eta_mse
    )
  
  write_csv(df_out, here(glue('simulations/sim_files/{sim_run}.csv')))
  
  tibble(
    true_params = x$home_advantage,
    bivpois_eta = bivpois_eta,
    bivpois_eta_bias = bivpois_eta_bias,
    bivpois_eta_mse = bivpois_eta_mse, 
    lm_eta = lm_eta, 
    lm_eta_bias = lm_eta_bias, 
    lm_eta_mse = lm_eta_mse
  )
}




simulation <- map_df(.x = simulation_data, .f = simulation_fun)


simulation_long <-  simulation %>% 
  pivot_longer(cols = c(bivpois_eta, lm_eta), names_to = "type", values_to = "estimated_ha")

simulation_long %>% 
  mutate(estimated_ha = ifelse(type != "lm_eta", exp(estimated_ha)-1, estimated_ha)) %>% 
  ggplot(aes(exp(true_params)-1, estimated_ha, fill = type, group = type, colour = type)) + 
  geom_point(pch = 21)+ 
  geom_smooth() + 
  scale_fill_brewer(palette = "Set1", labels = c("BVP", "LM"), "Estimate") + 
  scale_colour_brewer(palette = "Set1", labels = c("BVP", "LM"), "Estimate") + 
  labs(title="Home advantage estimates -- 100 simulations", 
       y = "Estimated HA", x = "Known HA") + 
  theme_bw()

library(ggbeeswarm)

simulation_long %>% 
  mutate(estimated_ha = 
           ifelse(type != "lm_eta", exp(estimated_ha)-1, estimated_ha), 
         estimated_ha_bias = estimated_ha - (exp(true_params)-1)) %>%
  group_by(type) %>% 
  summarise(mean_abs_bias = mean(abs(estimated_ha_bias)), 
            mean_bias = mean(estimated_ha_bias),
            lower_025 = quantile(estimated_ha_bias, 0.025), 
            upper_975 = quantile(estimated_ha_bias, 0.975))

simulation_long %>% 
  mutate(estimated_ha = 
           ifelse(type != "lm_eta", exp(estimated_ha)-1, estimated_ha), 
         estimated_ha_bias = estimated_ha - (exp(true_params)-1)) %>%
  ggplot(aes(x = (estimated_ha_bias), y = type, fill = type)) + 
  geom_boxplot(width = 0.1) + 
  geom_quasirandom(alpha = 0.5, pch = 21, groupOnX = FALSE) + 
  labs(title = "Simulated Bias in Home Advantage Estimates", 
       x = "Bias (Estimated HA - Known HA)") + 
  scale_y_discrete(labels = c("Bivariate\nPoisson", "Linear\nRegression"), "") + 
  geom_vline(aes(xintercept = 0), lty = 2, lwd =0.5) +
  theme(legend.position = 'none',
        axis.text.y = element_text(size = 18))
ggsave(file = here("simulations/sim_results.png"), height = 9/1.2, width = 16/1.2)

