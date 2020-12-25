library(tidyverse)
library(patchwork)
library(grid)
library(gridExtra)
library(here)
source(here('helpers.R'))

league_info <- read_csv(here("league_info.csv"))

### Read in Posterior HA Draws
posterior_means <- 
  map_dfr(league_info$alias, ~{
    league_ <- gsub("\\s", "_", tolower(.x))
    goals_posterior <- try(suppressWarnings(read_rds(here(glue('posteriors/bvp_goals_no_corr/{league_}.rds')))))
    yc_posterior <- try(suppressWarnings(read_rds(here(glue('posteriors/bvp_yc_lambda3/{league_}.rds')))))
    
    tibble('league' = ifelse(.x == 'English League Championship', 'English Championship', .x),
           'goals_ha_pre' = mean(goals_posterior$home_field_pre),
           'goals_ha_post' = mean(goals_posterior$home_field_post),
           'yc_ha_pre' = mean(yc_posterior$home_field_pre),
           'yc_ha_post' = mean(yc_posterior$home_field_post)
    ) %>% 
      mutate('cluster' = case_when(
        goals_ha_post > goals_ha_pre & yc_ha_pre > yc_ha_post ~ "Goals HA Increase | Yellow Cards HA Increase",
        goals_ha_post < goals_ha_pre & yc_ha_pre > yc_ha_post ~ "Goals HA Decrease | Yellow Cards HA Increase",
        goals_ha_post > goals_ha_pre & yc_ha_pre < yc_ha_post ~ "Goals HA Increase | Yellow Cards HA Decrease",
        goals_ha_post < goals_ha_pre & yc_ha_pre < yc_ha_post ~ "Goals HA Decrease | Yellow Cards HA Decrease")) %>% 
      mutate('magnitude' = sqrt((goals_ha_post-goals_ha_pre)^2 + (yc_ha_post - yc_ha_pre)^2))
    
  })

df_means <- 
  posterior_means %>% 
  rename_at(vars(contains('ha')), ~gsub('_ha', '', .x)) %>% 
  pivot_longer(cols = matches('goals|yc'),
               names_sep = '_',
               names_to = c('stat', 'ha_type'),
               values_to = 'value') %>% 
  pivot_wider(names_from = stat,
              values_from = value)

theme_set(theme_bw() + 
            theme(plot.title = element_text(size = 40, vjust = 3, hjust = 0.5),
                  strip.text = element_text(size = 28),
                  axis.text = element_text(size = 22),
                  plot.margin = unit(c(2,0.5,0.5,0.5), "cm")
            ))


p1 <- 
  df_means %>% 
  filter(cluster == 'Goals HA Decrease | Yellow Cards HA Decrease') %>% 
  ggplot(aes(x = yc, y = goals)) + 
  facet_wrap(~fct_reorder(league, desc(magnitude)), ncol = 4) +
  geom_vline(xintercept = 0, lty = 2) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_point(data = select(df_means, -league), alpha = 0.2, size = 3) +
  geom_point(color = "#F8766D", size = 10, alpha = 0.4) +
  geom_segment(data = posterior_means %>% filter(cluster == 'Goals HA Decrease | Yellow Cards HA Decrease'),
               color = "#F8766D",
               aes(x = yc_ha_pre,
                   xend = yc_ha_post,
                   y = goals_ha_pre,
                   yend = goals_ha_post),
               arrow = arrow(length = unit(0.7, "cm")), lwd = 4, show.legend = F) +
  scale_x_continuous(limits = c(-0.65, 0.2)) +
  scale_y_continuous(limits = c(-0.4, 0.5)) +
  annotate('text', x = -0.555, y = 0.17, label = 'Decreasing HA\n(Goals)', size = 7, angle = 90) +
  annotate('segment', x = -0.625, y = 0.475, xend = -0.625, yend = -0.16, arrow = arrow(length = unit(0.4, 'cm')), lwd = 1.5) +
  annotate('text', x = -0.275, y = -0.305, label = 'Decreasing HA (Yellow Cards)', size = 7) +
  annotate('segment', x = -0.635, y = -0.375, xend = 0.1, yend = -0.375,  arrow = arrow(length = unit(0.4, 'cm')), lwd = 1.5) +
  labs(x = '',
       y = '',
       title = 'Yellow Cards HA Decrease + Goals HA Decrease',
       color = '',
       fill = '')

p2 <-
  df_means %>% 
  filter(cluster == 'Goals HA Increase | Yellow Cards HA Decrease') %>% 
  ggplot(aes(x = yc, y = goals)) + 
  facet_wrap(~fct_reorder(league, desc(magnitude)), ncol = 4) +
  geom_vline(xintercept = 0, lty = 2) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_point(data = select(df_means, -league), alpha = 0.2, size = 3) +
  geom_point(color = "#C77CFF", size = 10, alpha = 0.4) +
  geom_segment(data = posterior_means %>% filter(cluster == 'Goals HA Increase | Yellow Cards HA Decrease'),
               color = "#C77CFF",
               aes(x = yc_ha_pre,
                   xend = yc_ha_post,
                   y = goals_ha_pre,
                   yend = goals_ha_post),
               arrow = arrow(length = unit(0.7, "cm")), lwd = 4, show.legend = F) +
  scale_x_continuous(limits = c(-0.65, 0.2)) +
  scale_y_continuous(limits = c(-0.4, 0.5)) +
  annotate('text', x = -0.555, y = 0.17, label = 'Decreasing HA\n(Goals)', size = 7, angle = 90) +
  annotate('segment', x = -0.625, y = 0.475, xend = -0.625, yend = -0.15, arrow = arrow(length = unit(0.4, 'cm')), lwd = 1.5) +
  annotate('text', x = -0.275, y = -0.305, label = 'Decreasing HA (Yellow Cards)', size = 7) +
  annotate('segment', x = -0.635, y = -0.375, xend = 0.1, yend = -0.375,  arrow = arrow(length = unit(0.4, 'cm')), lwd = 1.5) +
  labs(x = '',
       y = '',
       title = 'Yellow Cards HA Decrease + Goals HA Increase',
       caption = 'Yellow Cards HA Increase + Goals HA Increase',
       color = '',
       fill = '') +
  theme(plot.caption = element_text(hjust = 0.5, size = 40, vjust = -8))

p3 <- 
  df_means %>% 
  filter(cluster == 'Goals HA Increase | Yellow Cards HA Increase') %>% 
  ggplot(aes(x = yc, y = goals)) + 
  facet_wrap(~fct_reorder(league, desc(magnitude)), ncol = 4) +
  geom_vline(xintercept = 0, lty = 2) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_point(data = select(df_means, -league), alpha = 0.2, size = 3) +
  geom_point(color = "#00BFC4", size = 10, alpha = 0.4) +
  geom_segment(data = posterior_means %>% filter(cluster == 'Goals HA Increase | Yellow Cards HA Increase'),
               color = "#00BFC4",
               aes(x = yc_ha_pre,
                   xend = yc_ha_post,
                   y = goals_ha_pre,
                   yend = goals_ha_post),
               arrow = arrow(length = unit(0.7, "cm")), lwd = 4, show.legend = F) +
  scale_x_continuous(limits = c(-0.65, 0.2)) +
  scale_y_continuous(limits = c(-0.4, 0.5)) +
  annotate('text', x = -0.555, y = 0.17, label = 'Decreasing HA\n(Goals)', size = 7, angle = 90) +
  annotate('segment', x = -0.625, y = 0.475, xend = -0.625, yend = -0.15, arrow = arrow(length = unit(0.4, 'cm')), lwd = 1.5) +
  annotate('text', x = -0.275, y = -0.305, label = 'Decreasing HA (Yellow Cards)', size = 7) +
  annotate('segment', x = -0.635, y = -0.375, xend = 0.1, yend = -0.375,  arrow = arrow(length = unit(0.4, 'cm')), lwd = 1.5) +
  labs(x = '',
       y = '',
       title = '',
       color = '',
       fill = '')


design <-"
11
11
11
33
2#"


p1 + p3 + p2 + plot_layout(design = design) + 
  plot_annotation(title = 'Change in Home Advantages:\nGoals and Yellow Cards',
                  subtitle = 'Arrows reflect Pre-Covid to Post-Covid posterior means in HA',
                  theme = theme(plot.title = element_text(size = 72, hjust = 0.5, face = 'bold'),
                                plot.subtitle = element_text(size = 48, hjust = 0.5, face = 'bold')))

ggsave('figures/figure5.png', height = 30, width = 22)



