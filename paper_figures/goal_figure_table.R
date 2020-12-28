library(tidyverse)
library(here)
library(ggridges)
library(kableExtra)
library(knitr)
source(here('helpers.R'))

league_info <- read_csv(here('league_info.csv'))

### Directory to Read in From
directory <- "bvp_goals_no_corr"

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
  scale_x_continuous(limits = c(-0.5, 0.6)) +
  geom_density_ridges(aes(fill = hfa_type), alpha = 0.5, quantiles = 0.5, quantile_lines = T) +
  labs(x = 'Home Advantage',
       y = 'League',
       fill = '',
       title = 'Home Advantage for Selected European Leagues',
       subtitle = 'Goals') +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 24),
        plot.title = element_text(hjust = 0.5, size = 30),
        plot.subtitle = element_text(hjust = 0.5, size = 24),
        legend.text = element_text(size = 16))

ggsave(here(glue('paper_figures/figures/figure3.png')), width = 16/1.2, height = 9/1.2)

### Posterior Means
df_means <- 
  group_by(draws, league) %>% 
  summarise('mean_pre' = mean(posterior_draw[hfa_type == 'Pre-COVID (w/ Fans)']),
            'mean_post'  = mean(posterior_draw[hfa_type == 'Post-COVID (w/out Fans)'])) %>% 
  inner_join(select(league_info, 'league' = alias, logo_url))
### P HA Decrease
probs <- 
  map_dfr(league_info$alias, ~{
    league_ <- gsub("\\s", "_", tolower(.x))
    posterior <- try(suppressWarnings(read_rds(here(glue('posteriors/{directory}/{league_}.rds')))))
    if(any(class(posterior) == 'try-error')) {
      NULL
    } else {
      tibble('league' = .x,
             'p_decrease' = mean(posterior$home_field_pre > posterior$home_field_post))
    }
  }) 

### Table:
df_means %>% 
  select(-logo_url) %>% 
  mutate('delta' = mean_post -mean_pre) %>% 
  mutate('pct' = paste0(sprintf('%0.1f', delta/abs(mean_pre) * 100), '%')) %>%  
  inner_join(probs) %>% 
  arrange(desc(p_decrease)) 
  xtable::xtable(digits = c(0, 0, 3,3,3,1,3)) %>%
  print(include.rownames = F)