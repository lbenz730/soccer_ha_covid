library(tidyverse)
library(here)
library(rstan)
library(glue)
library(ggbeeswarm)
library(ggridges)

source(here('helpers.R'))
source(here('simulations/simulation_dgps.R'))
source(here('simulations/run_simulation.R'))

### Set Seed for Simulations
set.seed(0)

### Define parameters for the simulation
n_reps <- 100
num_club <- 20
df_bvp_dgp <- list('run' = 1:n_reps,
                   'num_club' = num_club,
                   'team_strength_correlation' = c(-0.8, -0.4, 0),
                   'home_advantage' = c(0, 0.25, 0.5),
                   'mu' = exp(c(0, 0.15, 0.3)),
                   'lambda3' = 0)

bvn_dgp <- 
  expand.grid(df_bvp_dgp) %>%  
  distinct(run, num_club, team_strength_correlation, home_advantage) %>% 
  mutate('sim_id' = 1:nrow(.) + nrow(expand.grid(df_bvp_dgp))) %>% 
  transpose()
bvp_dgp <- 
  expand.grid(df_bvp_dgp) %>%  
  mutate('sim_id' = 1:nrow(.)) %>% 
  transpose()


### Create data sets
bivpois_data <- 
  map(bvp_dgp, ~generate_bivpois(run = .x$run, 
                                 sim_id = .x$sim_id,
                                 num_club = .x$num_club,
                                 team_strength_correlation = .x$team_strength_correlation,
                                 home_advantage = .x$home_advantage,
                                 mu = .x$mu,
                                 lambda3 = .x$lambda3))

bivnorm_data <- 
  map(bvn_dgp, ~generate_bivnorm(run = .x$run, 
                                 sim_id = .x$sim_id,
                                 num_club = .x$num_club,
                                 home_advantage = .x$home_advantage,
                                 team_strength_correlation = .x$team_strength_correlation))

simulation_data <- c(bivpois_data, bivnorm_data) 

### Run Sims (Indicies to Run from different places, if needed)
start_ix <- 1
end_ix <- 3600
simulation_data <- simulation_data[start_ix:end_ix]
simulation <- map_df(.x = simulation_data, .f = run_simulation)
