library(tidyverse)
library(here)
library(ggridges)
source(here('helpers.R'))

league_info <- read_csv(here('league_info.csv'))

### Directory to Read in From
# directory <- "bvp_yc_no_corr"
directory <- "bvp_yc_lambda3"

if(!dir.exists(here(glue('paper_figures/figures/cards/{directory}')))) {
  dir.create(here(glue('paper_figures/figures/cards/{directory}')))
} 

### Read in Posterior HA Draws
draws <- 
  map_dfr(league_info$alias, ~{
    league_ <- gsub("\\s", "_", tolower(.x))
    posterior <- try(suppressWarnings(read_rds(here(glue('posteriors/{directory}/{league_}.rds')))))
    if(any(class(posterior) == 'try-error')) {
      NULL
    } else {
      n_draw <-  length(posterior$home_field_pre)
      tibble('league' = .x,
             'posterior_draw' = c(posterior$home_field_pre, posterior$home_field_post),
             'hfa_type' = rep(c('Pre-COVID (w/ Fans)', 'Post-COVID (w/out Fans)'), each = n_draw))
    }
  }) 


### Ridgeline Plot
df_medians <- 
  group_by(draws, league, hfa_type) %>% 
  summarise('median' = median(posterior_draw),
            'mean' = mean(posterior_draw))

draws$league_f <- factor(draws$league, 
                         levels = df_medians$league[df_medians$hfa_type == 'Pre-COVID (w/ Fans)'][order(df_medians$median[df_medians$hfa_type == 'Pre-COVID (w/ Fans)'], decreasing = F)])



ggplot(draws, aes(x = posterior_draw, y = league_f)) +
  geom_vline(lty = 2, xintercept = 0) +
  geom_density_ridges(aes(fill = hfa_type), alpha = 0.5, quantiles = 0.5, quantile_lines = T) +
  labs(x = 'Home Advantage',
       y = 'League',
       fill = '',
       title = 'Home Advantage for Selected European Leagues',
       subtitle = 'Yellow Cards') +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 24),
        plot.title = element_text(hjust = 0.5, size = 30),
        plot.subtitle = element_text(hjust = 0.5, size = 24),
        legend.text = element_text(size = 16)) +
  scale_x_continuous(limits = c(-1, 0.75)) 
ggsave(here(glue('paper_figures/figures/cards/{directory}/yc_ridge.png')), width = 16/1.2, height = 9/1.2)

### Lambda 3 plot, if applicable
if(str_detect(directory, 'lambda3')) {
  lambda_draws <- 
    map_dfr(league_info$alias, ~{
      league_ <- gsub("\\s", "_", tolower(.x))
      posterior <- try(suppressWarnings(read_rds(here(glue('posteriors/{directory}/{league_}.rds')))))
      if(any(class(posterior) == 'try-error')) {
        NULL
      } else {
        tibble('league' = .x,
               'lambda_3' = posterior$lambda3)
      }
    }) 
  
  ggplot(lambda_draws, aes(x = lambda_3, y = league)) +
    geom_density_ridges(fill = 'seagreen', alpha = 0.5, quantiles = 0.5, quantile_lines = T) +
    labs(x = 'Lambda 3',
         y = 'League',
         fill = '',
         title = 'Posterior Distributions for Lambda 3',
         subtitle = 'Bivariate Poisson Model: Yellow Cards')
  ggsave(here(glue('paper_figures/figures/cards/{directory}/lambda3_yc_ridge.png')), width = 16/1.2, height = 9/1.2)
}

### Posterior Mean Plot
df_means <- 
  group_by(draws, league) %>% 
  summarise('mean_pre' = mean(posterior_draw[hfa_type == 'Pre-COVID (w/ Fans)']),
            'mean_post'  = mean(posterior_draw[hfa_type == 'Post-COVID (w/out Fans)'])) %>% 
  inner_join(select(league_info, 'league' = alias, logo_url))

ggplot(df_means, aes(x = mean_pre, y = mean_post)) +
  geom_abline(slope = 1, intercept = 0) +
  # scale_x_continuous(limits = c(0.01, 0.8)) +
  # scale_y_continuous(limits = c(-0.15, 0.15)) +
  geom_hline(yintercept = 0, alpha = 0.4, lty = 2) +
  # geom_label(aes(label = league)) +
  ggrepel::geom_label_repel(aes(label = league, fill = mean_post - mean_pre), size = 2.2, alpha = 0.6) +
  scale_fill_viridis_c(option = 'C') +
  labs(x = 'HA Posterior Mean Pre-COVID',
       y = 'HA Posterior Mean Post-COVID',
       fill = 'Change in Posterior Mean',
       title = 'Change in Home Advantage for Select European Leagues',
       subtitle = 'Yellow Cards') +
  theme(legend.text = element_text(size = 7)) 
ggsave(here(glue('paper_figures/figures/cards/{directory}/yc_posterior_means.png')), width = 16/1.2, height = 9/1.2)


### P HA Decrease
probs <- 
  map_dfr(league_info$alias, ~{
    league_ <- gsub("\\s", "_", tolower(.x))
    posterior <- try(suppressWarnings(read_rds(here(glue('posteriors/{directory}/{league_}.rds')))))
    if(any(class(posterior) == 'try-error')) {
      NULL
    } else {
      tibble('league' = .x,
             'p_decrease' = mean(posterior$home_field_pre < posterior$home_field_post))
    }
  }) 


ggplot(probs, aes(x = p_decrease, y = fct_reorder(league, p_decrease))) +
  geom_col(fill = 'seagreen') + 
  labs(x = 'P(HA Post-COVID < HA Pre-COVID)',
       y = 'League',
       title = 'Probability of Decline in Home Advantage (Yellow Cards)') +
  geom_text(aes(label = paste0(sprintf('%0.1f', 100*p_decrease), '%')), nudge_x = 0.035) +
  scale_x_continuous(labels = scales::percent)
ggsave(here(glue('paper_figures/figures/cards/{directory}/p_hfa_decline_yc.png')), width = 16/1.2, height = 9/1.2)


### Table:
df_means %>% 
  select(-logo_url) %>% 
  mutate('delta' = mean_post -mean_pre) %>% 
  mutate('pct' = paste0(sprintf('%0.1f', delta/abs(mean_pre) * 100), '%')) %>%  
  inner_join(probs) %>% 
  arrange(desc(p_decrease)) %>% 
  xtable::xtable(digits = c(0, 0, 3,3,3,1,3)) %>%
  print(include.rownames = F)
