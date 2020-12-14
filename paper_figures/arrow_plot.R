library(tidyverse)
library(here)
source(here('helpers.R'))

league_info <- read_csv(here("league_info.csv"))

### Read in Posterior HA Draws
posterior_means <- 
  map_dfr(league_info$alias, ~{
    league_ <- gsub("\\s", "_", tolower(.x))
    goals_posterior <- try(suppressWarnings(read_rds(here(glue('posteriors/bvp_goals_no_corr/{league_}.rds')))))
    yc_posterior <- try(suppressWarnings(read_rds(here(glue('posteriors/bvp_yc_lambda3/{league_}.rds')))))
    
    tibble('league' = .x,
           'goals_ha_pre' = mean(goals_posterior$home_field_pre),
           'goals_ha_post' = mean(goals_posterior$home_field_post),
           'yc_ha_pre' = mean(yc_posterior$home_field_pre),
           'yc_ha_post' = mean(yc_posterior$home_field_post)
    )
  })

df_means <- 
  posterior_means %>% 
  rename_at(vars(contains('ha')), ~gsub('_ha', '', .x)) %>% 
  pivot_longer(cols = -league,
               names_sep = '_',
               names_to = c('stat', 'ha_type'),
               values_to = 'value') %>% 
  pivot_wider(names_from = stat,
              values_from = value)


ggplot(df_means, aes(x = goals, y = yc)) + 
  facet_wrap(~league) +
  geom_vline(xintercept = 0, lty = 2) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_point(data = select(df_means, -league), alpha = 0.2, size = 2) +
  geom_point(aes(color = league), size = 4, alpha = 0.4) +
  geom_segment(data = posterior_means,
               aes(x = goals_ha_pre,
                   xend = goals_ha_post,
                   y = yc_ha_pre,
                   yend = yc_ha_post,
                   color = league),
               arrow = arrow(length = unit(0.4, "cm")), lwd = 1.7) +
  scale_x_continuous(limits = c(-0.4, 0.6)) +
  scale_y_continuous(limits = c(-0.7, 0.3)) +
  annotate('text', x = 0.45, y = -0.6, label = 'Increasing HA', size = 3.5) +
  annotate('segment', x = 0.3, y = -0.65, xend = 0.6, yend = -0.65, arrow = arrow(length = unit(0.3, 'cm'))) +
  annotate('text', x = -0.2, y = -0.6, label = 'Decreasing HA', size = 3.5) +
  annotate('segment', x = -0.05, y = -0.65, xend = -0.35, yend = -0.65, arrow = arrow(length = unit(0.3, 'cm'))) +
  annotate('text', x = -0.4, y = -0.425, label = 'Increasing HA', size = 3, angle = 90) +
  annotate('segment', x = -0.375, y = -0.25, xend = -0.375, yend = -0.65, arrow = arrow(length = unit(0.3, 'cm'))) +
  annotate('text', x = -0.4, y = 0.025, label = 'Decreasing HA', size = 3, angle = 90) +
  annotate('segment', x = -0.375, y = -0.15, xend = -0.375, yend = 0.25, arrow = arrow(length = unit(0.3, 'cm'))) +
  theme(legend.position = 'none') +
  labs(x = 'Goals Home Advantage Posterior Mean',
       y = 'Yellow Cards Home Advantage Posterior Mean',
       title = 'Change in Home Advantage',
       subtitle = 'Arrow from Pre-Covid HA to Post-Covid HA')
ggsave('figures/arrow_plot.png', height = 9 * 1.35, width = 16 * 1.5)

