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
             'posterior_draw' = posterior$home_field_post - posterior$home_field_pre)
    }
  }) 

### Ridgeline Plot
df_medians <- 
  group_by(draws, league) %>% 
  summarise('median' = median(posterior_draw),
            'mean' = mean(posterior_draw))
draws$league_f <- factor(draws$league, levels = df_medians$league[order(df_medians$median, decreasing = F)])



ggplot(draws, aes(x = posterior_draw, y = league_f)) +
  geom_vline(lty = 2, xintercept = 0) +
  scale_x_continuous(limits = c(-0.8, 0.6)) +
  geom_density_ridges(fill = 'seagreen', alpha = 0.5, quantiles = 0.5, quantile_lines = T) +
  labs(x = 'Change in Home Advantage',
       y = 'League',
       fill = '',
       title = 'Change in Home Advantage for Selected European Leagues',
       subtitle = 'Goals') +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 24),
        plot.title = element_text(hjust = 0.5, size = 26),
        plot.subtitle = element_text(hjust = 0.5, size = 24),
        legend.text = element_text(size = 16))

ggsave(here(glue('paper_figures/figures/figure4.png')), width = 16/1.2, height = 9/1.2)


### Directory to Read in From
directory <- "bvp_yc_lambda3"

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
             'posterior_draw' = posterior$home_field_post - posterior$home_field_pre)
    }
  }) 

### Ridgeline Plot
df_medians <- 
  group_by(draws, league) %>% 
  summarise('median' = median(posterior_draw),
            'mean' = mean(posterior_draw))
draws$league_f <- factor(draws$league, levels = df_medians$league[order(df_medians$median, decreasing = F)])



ggplot(draws, aes(x = posterior_draw, y = league_f)) +
  geom_vline(lty = 2, xintercept = 0) +
  scale_x_continuous(limits = c(-1.1, 1.1)) +
  geom_density_ridges(fill = 'yellow', alpha = 0.5, quantiles = 0.5, quantile_lines = T) +
  labs(x = 'Change in Home Advantage',
       y = 'League',
       fill = '',
       title = 'Change in Home Advantage for Selected European Leagues',
       subtitle = 'Yellow Cards') +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 24),
        plot.title = element_text(hjust = 0.5, size = 26),
        plot.subtitle = element_text(hjust = 0.5, size = 24),
        legend.text = element_text(size = 16))

ggsave(here(glue('paper_figures/figures/figure5.png')), width = 16/1.2, height = 9/1.2)

