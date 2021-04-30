### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### SIMULATION: DATA GENERATION Processes
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
library(tidyverse)

### Bivariate Poission Data Generation
generate_bivpois <- function(run, sim_id, num_club, team_strength_correlation, lambda3, home_advantage, mu) {
  
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
      'lambda3' = lambda3,
      'run' = run,
      'sim_id' = sim_id
    )
}


### Bivariate Normal Data Generation
generate_bivnorm <- function(run, sim_id, num_club, team_strength_correlation, home_advantage) {
  
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
  
  
  games <- 
    games %>%
    filter(home != away) %>%
    left_join(teams, by = c("home" = "club")) %>%
    rename(h_att = attack, h_def = defend) %>%
    left_join(teams, by = c("away" = "club")) %>%
    rename(a_att = attack, a_def = defend) %>%
    mutate(
      'mu1' = 5 + home_advantage + h_att + a_def, 
      'mu2' = 5 + a_att + h_def,
      'h_goals' = round(truncnorm::rtruncnorm(n = nrow(.), a = -0.49999, mean = mu1, sd = 1)), ## left truncated at -0.49999
      'a_goals' = round(truncnorm::rtruncnorm(n = nrow(.), a = -0.49999, mean = mu2, sd = 1)), ## left truncated at  -0.49999
      'home_game' = 1) %>%
    select(home, away, h_goals, a_goals, home_game)
  
  sim_out <- 
    list(
      'method' = "bivnorm",
      'teams' = teams,
      'games' = games,
      'home_advantage' = home_advantage,
      'team_strength_correlation' = team_strength_correlation,
      'run' = run,
      'sim_id' = sim_id
    )
}
