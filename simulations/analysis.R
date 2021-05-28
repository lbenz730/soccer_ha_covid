library(tidyverse)
library(here)
library(rstan)
library(glue)
library(ggbeeswarm)
library(ggridges)

source(here('helpers.R'))

### Analysis
files <- dir('sim_files/v2_sims', full.names = T)

simulation <- furrr::future_map_dfr(files, read_csv)

simulation_long <-
  simulation %>%
  pivot_longer(cols = c(bivpois_estimated_ha, pc_eta, lm_eta), names_to = "model_type", values_to = "estimated_ha") %>%
  mutate('model_type' = case_when(model_type == 'lm_eta' ~ 'Linear Regression',
                                  model_type == 'pc_eta' ~ 'Paired Comparisons',
                                  T ~ 'Bivariate Poisson')) %>%
  mutate('model_type' = fct_relevel(model_type, 'Bivariate Poisson', 'Paired Comparisons', 'Linear Regression')) %>%
  mutate('method' = case_when(method == 'bivnorm' ~ 'Bivariate Normal',
                              method == 'bivpois' ~ 'Bivariate Poisson'))


df_results <-
  simulation_long %>%
  mutate('estimated_ha_bias' = estimated_ha - true_params)

df_summary <-
  df_results %>%
  group_by(model_type, method, team_strength_correlation, mu, 'home_advantage' = true_params) %>%
  summarise(mean_abs_bias = mean(abs(estimated_ha_bias)),
            mean_bias = mean(estimated_ha_bias),
            lower_025 = quantile(estimated_ha_bias, 0.025),
            upper_975 = quantile(estimated_ha_bias, 0.975))

ggplot(filter(df_summary, method == 'Bivariate Poisson'),
       aes(x = fct_reorder(as.character(team_strength_correlation), team_strength_correlation), y = mean_abs_bias,
           fill = model_type, label = sprintf('%0.2f', mean_abs_bias))) +
  facet_grid(paste('Mu:', log(mu)) ~ paste('Home Advantage:', home_advantage)) +
  geom_col(position = 'dodge') +
  geom_text(position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Simulated Bias in Home Advantage Estimates",
       subtitle = 'DGP: Bivariate Poisson',
       x = 'Team Strength Correlation',
       y = "Mean Absolute Bias",
       fill = 'Model') +
  scale_y_continuous(limits = c(0, 0.9))
ggsave(file = here("simulations/figures/sim_results_bvp.png"), height = 9/1.2, width = 16/1.2)

ggplot(filter(df_summary, method == 'Bivariate Normal'),
       aes(x = fct_reorder(as.character(team_strength_correlation), team_strength_correlation), y = mean_abs_bias,
           fill = model_type, label = sprintf('%0.2f', mean_abs_bias))) +
  facet_wrap(~paste('Home Advantage:', home_advantage)) +
  geom_col(position = 'dodge') +
  geom_text(position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Simulated Bias in Home Advantage Estimates",
       subtitle = 'DGP: Bivariate Normal',
       x = 'Team Strength Correlation',
       y = "Mean Absolute Bias",
       fill = 'Model') +
  scale_y_continuous(limits = c(0, 0.9))
ggsave(file = here("simulations/figures/sim_results_bvn.png"), height = 9/1.2, width = 16/1.2)

# ### Plot
ggplot(df_results %>% filter(method == 'Bivariate Poisson'), aes(x = (estimated_ha_bias), y = model_type, fill = model_type)) +
  # facet_grid(paste('DGP:', method) ~ paste('Home Advantage:', true_params)) +
  facet_wrap(~paste('Home Advantage:', true_params)) +
  # geom_boxplot(width = 0.1, outlier.shape = NA) +
  # geom_beeswarm(alpha = 0.5, pch = 21, groupOnX = FALSE) +
  # geom_quasirandom(alpha = 0.5, pch = 21, groupOnX = FALSE) +
  geom_density_ridges(scale = 0.8, alpha = 0.5) +
  labs(title = "Simulated Bias in Home Advantage Estimates",
       x = "Bias (Estimated HA - Known HA)",
       y = 'Model',
       fill = 'Mu') +
  geom_vline(aes(xintercept = 0), lty = 2, lwd =0.5) +
  theme(axis.text.y = element_text(size = 18))

ggsave(file = here("simulations/figures/sim_ridge.png"), height = 9/1.2, width = 16/1.2)


ggplot(filter(df_summary, (method == 'Bivariate Poisson' & mu == 1) | method == 'Bivariate Normal'),
       aes(x = fct_reorder(as.character(team_strength_correlation), team_strength_correlation), y = mean_abs_bias,
           fill = model_type, label = sprintf('%0.4f', mean_abs_bias))) +
  facet_grid(paste('DGP:', method) ~ paste('Home Advantage:', home_advantage)) +
  geom_col(position = 'dodge') +
  geom_text(position = position_dodge(width = 0.9), vjust = -0.5, size = 3.5) +
  labs(title = "Simulated Bias in Home Advantage Estimates",
       x = 'Team Strength Correlation',
       y = "Mean Absolute Bias",
       fill = 'Model') +
  scale_y_continuous(limits = c(0, 0.55))
ggsave(file = here("simulations/figures/dgp_grid.png"), height = 9/1.2, width = 16/1.2)


library(xtable)
library(knitr)
library(kableExtra)
x <- 
  df_summary %>% 
  ungroup() %>% 
  filter(method == 'Bivariate Normal' | log(mu) == 0) %>% 
  select(method, model_type, home_advantage, team_strength_correlation, mean_abs_bias, mean_bias) %>% 
  mutate('mean_abs_bias' = round(mean_abs_bias, 10),
         'mean_bias' = round(mean_bias, 10)) %>% 
  arrange(home_advantage) %>% 
  pivot_wider(names_from = c('home_advantage', 'team_strength_correlation'),
              values_from = c('mean_abs_bias', 'mean_bias'),
  ) %>% 
  
  arrange(desc(method)) %>% 
  select(-method) 

x %>% 
  select(model_type, 
         contains('bias_0_-0.8'), contains('bias_0_-0.4'), contains('bias_0_0')
  ) %>% 
  xtable(digits = 3) %>%
  print(include.rownames = F)


x %>% 
  select(model_type, 
         contains('bias_0.25_-0.8'), contains('bias_0.25_-0.4'), contains('bias_0.25_0')
  ) %>% 
  xtable(digits = 3) %>%
  print(include.rownames = F)

x %>% 
  select(model_type, 
         contains('bias_0.5_-0.8'), contains('bias_0.5_-0.4'), contains('bias_0.5_0')
  ) %>% 
  xtable(digits = 3) %>%
  print(include.rownames = F)




