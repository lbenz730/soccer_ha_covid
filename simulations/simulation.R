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
n_reps <- 200
num_club <- 20
bvp_dgp <- list('run' = 1:n_reps,
                'num_club' = num_club,
                'team_strength_correlation' = c(-0.5, 0, 0.5),
                'lambda3' = c(0, 0.1, 0.2))
bvp_dgp <- transpose(expand.grid(bvp_dgp) %>%  mutate('sim_id' = 1:nrow(.))) 

### Create data sets
bivpois_data <- 
  map(bvp_dgp, ~generate_bivpois(run = .x$run, 
                                 sim_id = .x$sim_id,
                                 num_club = .x$num_club,
                                 team_strength_correlation = .x$team_strength_correlation,
                                 lambda3 = .x$lambda3))

simulation_data <- c(bivpois_data) ### Probably add in lm_data

### Run Sims
start_ix <- 1
end_ix <- 1800
simulation_data <- simulation_data[start_ix:end_ix]
simulation <- map_df(.x = simulation_data, .f = run_simulation)

### Analysis
files <- dir('sim_files/v2_sims', full.names = T)
simulation <- map_dfr(files, read_csv)

simulation_long <-
  simulation %>%
  pivot_longer(cols = c(bivpois_estimated_ha, lm_eta), names_to = "model_type", values_to = "estimated_ha")

df_results <-
  simulation_long %>%
  mutate('estimated_ha_bias' = estimated_ha - (exp(true_params)-1))

df_summary <- 
  df_results %>%
  group_by(model_type, team_strength_correlation, lambda3) %>%
  summarise(mean_abs_bias = mean(abs(estimated_ha_bias)),
            mean_bias = mean(estimated_ha_bias),
            lower_025 = quantile(estimated_ha_bias, 0.025),
            upper_975 = quantile(estimated_ha_bias, 0.975)) 

ggplot(df_summary, aes(x = lambda3, y = mean_abs_bias)) +
  facet_wrap(~ifelse(model_type == 'lm_eta', 'Linear Regression', 'Bivariate Poisson')) +
  geom_col(aes(fill = as.character(team_strength_correlation)), position = 'dodge') +
  labs(title = "Simulated Bias in Home Advantage Estimates",
       x = 'Lambda3',
       y = "Mean Absolute Bias",
       fill = 'Team Strength Correlation') 
ggsave(file = here("simulations/sim_results2.png"), height = 9/1.2, width = 16/1.2)

### Plot
ggplot(df_results, aes(x = (estimated_ha_bias), y = model_type, fill = model_type)) +
  facet_grid(paste('Team Strength Correlation:', team_strength_correlation) ~ paste('Lambda3:', lambda3)) +
  # geom_boxplot(width = 0.1, outlier.shape = NA) +
  # geom_beeswarm(alpha = 0.5, pch = 21, groupOnX = FALSE) +
  geom_density_ridges(scale = 0.8) +
  # geom_quasirandom(alpha = 0.5, pch = 21, groupOnX = FALSE) +
  labs(title = "Simulated Bias in Home Advantage Estimates",
       x = "Bias (Estimated HA - Known HA)") +
  scale_y_discrete(labels = c("Bivariate\nPoisson", "Linear\nRegression"), "") +
  geom_vline(aes(xintercept = 0), lty = 2, lwd =0.5) +
  theme(legend.position = 'none',
        axis.text.y = element_text(size = 18))

ggsave(file = here("simulations/sim_results.png"), height = 9/1.2, width = 16/1.2)

