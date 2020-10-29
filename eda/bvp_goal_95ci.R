library(tidyverse)
library(here)
source(here('helpers.R'))

league_info <- read_csv(here('league_info.csv'))
goal_stats <- map_dfr(league_info$alias, wld_rates)
goal_stats  <- 
  goal_stats %>% 
  mutate('home_field_mean' = map_dbl(league, ~posterior_quantile('bvp_goals', .x, 'home_field', mean = T)),
         'home_field_low' = map_dbl(league, ~posterior_quantile('bvp_goals', .x, 'home_field', q = 0.025)),
         'home_field_high' = map_dbl(league, ~posterior_quantile('bvp_goals', .x, 'home_field', q = 0.975))) %>% 
  mutate(league = fct_reorder(league, home_field_mean))


ggplot(goal_stats, aes(x = home_field_mean, y = league)) +
  geom_segment(aes(x = home_field_low, xend = home_field_high, 
                   y = league, yend = league, color = league), lwd = 3) +
  geom_point() +
  theme(legend.position = 'none') + 
  labs(x = 'Home Field Advantage Coefficient',
       y = 'League',
       title = 'Home Field Advantage for Selected European Leagues',
       subtitle = 'Bivariate Poisson Model: Goals')
ggsave(here('eda/figures/bvp_goals_hfa_fixed_intercept.png'), width = 16/1.2, height = 9/1.2)

write_csv(goal_stats, here('eda/goal_stats.csv'))

ggplot(goal_stats, aes(x = avg_goal_diff, y = home_field_mean)) +
  geom_point(aes(col = league), size = 2.5) +
  labs(x = 'Average (Home - Away) Goal Differential',
       y = 'Home Field Advantage Posterior Mean',
       title = 'Home Field Advantage for Selected European Leagues',
       subtitle = 'Bivariate Poisson Model: Goals')
ggsave(here('eda/figures/bvp_goals_hfa_vs_gd.png'), width = 16/1.2, height = 9/1.2)

