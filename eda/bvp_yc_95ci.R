library(tidyverse)
library(here)
source(here('helpers.R'))

league_info <- read_csv(here('league_info.csv')) 
yc_stats <- map_dfr(league_info$alias, wld_rates)
yc_stats  <- 
  yc_stats %>% 
  mutate('home_field_mean' = map_dbl(league, ~posterior_quantile('bvp_yc', .x, 'home_field', mean = T)),
         'home_field_low' = map_dbl(league, ~posterior_quantile('bvp_yc', .x, 'home_field', q = 0.025)),
         'home_field_high' = map_dbl(league, ~posterior_quantile('bvp_yc', .x, 'home_field', q = 0.975))) %>% 
  mutate(league = fct_reorder(league, home_field_mean))


ggplot(yc_stats, aes(x = home_field_mean, y = league)) +
  geom_segment(aes(x = home_field_low, xend = home_field_high, 
                   y = league, yend = league, color = league), lwd = 3) +
  geom_point() +
  theme(legend.position = 'none') + 
  labs(x = 'Home Field Advantage Coefficient',
       y = 'League',
       title = 'Home Field Advantage for Selected European Leagues',
       subtitle = 'Bivariate Poisson Model: Yellow Cards')
ggsave(here('eda/figures/bvp_yc_hfa_fixed_intercept.png'), width = 16/1.2, height = 9/1.2)

write_csv(yc_stats, here('eda/stats/yc_stats.csv'))

ggplot(yc_stats, aes(x = avg_yc_diff/yc_per_game, y = home_field_mean)) +
  geom_point(aes(col = league), size = 2.5) +
  labs(x = 'Average (Home - Away)/Average (Home + Away) Yellow Card Differential',
       y = 'Home Field Advantage Posterior Mean',
       title = 'Home Field Advantage for Selected European Leagues',
       subtitle = 'Bivariate Poisson Model: Yellow Cards')
ggsave(here('eda/figures/bvp_yc_hfa_vs_ycd_rel.png'), width = 16/1.2, height = 9/1.2)

ggplot(yc_stats, aes(x = avg_yc_diff, y = home_field_mean)) +
  geom_point(aes(col = league), size = 2.5) +
  labs(x = 'Average (Home - Away) Yellow Card Differential',
       y = 'Home Field Advantage Posterior Mean',
       title = 'Home Field Advantage for Selected European Leagues',
       subtitle = 'Bivariate Poisson Model: Yellow Cards')
ggsave(here('eda/figures/bvp_yc_hfa_vs_ycd.png'), width = 16/1.2, height = 9/1.2)


select(yc_stats, league, contains('yc'), contains('goal'), contains('diff')) %>% 
  mutate('yc_pct' = avg_yc_diff/yc_per_game,
         'goal_pct' = avg_goal_diff/goals_per_game) %>% 
  select(league, contains('pct')) %>% 
  pivot_longer(-league,
               names_to = 'stat') %>% 
  ggplot(aes(x = league, y = value)) +
  facet_wrap(~stat) +
  geom_col(aes(fill = league)) +
  coord_flip() +
  theme(legend.position = 'none') +
  labs(x = 'League', 
       y = 'Value')

