library(tidyverse)
library(here)
source(here('helpers.R'))

league_info <- read_csv(here('league_info.csv')) 
rc_stats <- map_dfr(league_info$alias, wld_rates)
rc_stats  <- 
  rc_stats %>% 
  mutate('home_field_mean' = map_dbl(league, ~posterior_quantile('bvp_rc', .x, 'home_field', mean = T)),
         'home_field_low' = map_dbl(league, ~posterior_quantile('bvp_rc', .x, 'home_field', q = 0.025)),
         'home_field_high' = map_dbl(league, ~posterior_quantile('bvp_rc', .x, 'home_field', q = 0.975))) %>% 
  mutate(league = fct_reorder(league, home_field_mean))


ggplot(rc_stats, aes(x = home_field_mean, y = league)) +
  geom_segment(aes(x = home_field_low, xend = home_field_high, 
                   y = league, yend = league, color = league), lwd = 3) +
  geom_point() +
  theme(legend.position = 'none') + 
  labs(x = 'Home Field Advantage Coefficient',
       y = 'League',
       title = 'Home Field Advantage for Selected European Leagues',
       subtitle = 'Bivariate Poisson Model: Red Cards')
ggsave(here('eda/figures/bvp_rc_hfa_fixed_intercept.png'), width = 16/1.2, height = 9/1.2)

write_csv(rc_stats, here('eda/stats/rc_stats.csv'))

ggplot(rc_stats, aes(x = avg_rc_diff, y = home_field_mean)) +
  geom_point(aes(col = league), size = 2.5) +
  labs(x = 'Average (Home - Away) Red Card Differential',
       y = 'Home Field Advantage Posterior Mean',
       title = 'Home Field Advantage for Selected European Leagues',
       subtitle = 'Bivariate Poisson Model: Red Cards')
ggsave(here('eda/figures/bvp_rc_hfa_vs_rcd.png'), width = 16/1.2, height = 9/1.2)

