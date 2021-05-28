library(tidyverse)
library(here)
library(glue)

source(here('helpers.R'))
source(here('simulations/simulation_dgps.R'))

league_info <- read_csv(here('league_info.csv'))

### Big 5 Leagues, minus France which didn't return
big_4 <- c('German Bundesliga', 'English Premier League',
           'Italy Serie A', 'Spanish La Liga')

### Read in Results for these leagues
df_scores <- map_dfr(big_4, ~{read_leage_csvs(.x) %>% mutate('league' = .x)})

### Filter Out Post-Covid Games
df_scores <- 
  df_scores %>% 
  inner_join(select(league_info, 'league' = alias, restart_date)) %>% 
  mutate('restart_date' = as.Date(restart_date, '%m/%d/%y')) %>% 
  filter(date < restart_date)

### Parameters
mu <- mean(df_scores$away_score) ### E[away_goals] == mu
home_advantage <- mean(df_scores$home_score - df_scores$away_score) ### E[home_goals] == mu + home_advantage
home_sd <- sd(df_scores$home_score)
away_sd <- sd(df_scores$away_score)

### Observed Distribution of Scores
observed_score_dist <- 
  df_scores %>% 
  group_by(home_score, away_score) %>% 
  summarise('n' = n(),
            'pct' = n()/nrow(df_scores)) %>% 
  ungroup()

###################### Simulate Data #############
sim_data <- function(num_club) {
  ### Team Strengths
  covariance_matrix <- matrix(c(0.35^2, 0, 0, 0.35^2), nrow = 2, byrow = T)
  team_strengths <- MASS::mvrnorm(n = num_club, mu = c(0,0), Sigma = covariance_matrix)
  teams <- 
    tibble('club' = paste0("club", sprintf("%02d", seq_len(num_club))),
           'attack' = team_strengths[,1],
           'defend' = team_strengths[,2])
  
  ### Create Schedule
  games <- 
    teams %>%
    select(club) %>%
    flatten_chr() 
  games <- expand.grid(home = games, away = games)
  
  ### BVP Score Sampling
  bvp_games <- 
    games %>%
    filter(home != away) %>%
    left_join(teams, by = c("home" = "club")) %>%
    rename(h_att = attack, h_def = defend) %>%
    left_join(teams, by = c("away" = "club")) %>%
    rename(a_att = attack, a_def = defend) %>%
    mutate(
      'lambda1' = exp(log(home_advantage + mu) + h_att + a_def),
      'lambda2' = exp(log(mu) + a_att + h_def),
      'h_goals' = rpois(n = nrow(.), lambda = lambda1),
      'a_goals' = rpois(n = nrow(.), lambda = lambda2)) %>%
    select(home, away, h_goals, a_goals) %>% 
    mutate('method' = 'bivpois')
  
  ### BVN Score Sampling
  bvn_games <- 
    games %>%
    filter(home != away) %>%
    left_join(teams, by = c("home" = "club")) %>%
    rename(h_att = attack, h_def = defend) %>%
    left_join(teams, by = c("away" = "club")) %>%
    rename(a_att = attack, a_def = defend) %>%
    mutate(
      'mu1' = mu + home_advantage + h_att + a_def, 
      'mu2' = mu + a_att + h_def,
      'h_goals' = round(truncnorm::rtruncnorm(n = nrow(.), a = -0.49999, mean = mu1, sd = home_sd)), ## left truncated at -0.49999
      'a_goals' = round(truncnorm::rtruncnorm(n = nrow(.), a = -0.49999, mean = mu2, sd = away_sd))) %>%  ## left truncated at  -0.49999
    select(home, away, h_goals, a_goals) %>% 
    mutate('method' = 'bivnorm')
  
  ### Return
  return(bind_rows(bvp_games, bvn_games))
}

### Simulate the Data
set.seed(12321)
n_seasons <- 1000
df_sims <- map_dfr(1:n_seasons, ~sim_data(num_club = 20))

sim_score_dist <- 
  df_sims %>% 
  group_by(method, 'home_score' = h_goals, 'away_score' = a_goals) %>% 
  summarise('n' = n()) %>% 
  group_by(method) %>% 
  mutate('pct' = n/sum(n)) %>% 
  ungroup()

### Compare Results
score_dist <- 
  bind_rows(
    full_join(observed_score_dist, 
              filter(sim_score_dist, method == 'bivpois'),
              by = c('home_score', 'away_score'),
              suffix = c('_obs', '_sim')) %>% 
      mutate('method' = 'bivpois'),
    full_join(observed_score_dist, 
              filter(sim_score_dist, method == 'bivnorm'),
              by = c('home_score', 'away_score'),
              suffix = c('_obs', '_sim')) %>% 
      mutate('method' = 'bivnorm')
  ) %>% 
  mutate_if(is.numeric, ~replace_na(.x, 0)) %>% 
  mutate('error' = pct_sim - pct_obs) %>% 
  mutate(method = ifelse(method == 'bivpois', 'Bivariate Poisson', 'Bivariate Normal'))

df_errors <- 
  score_dist %>% 
  group_by(method) %>% 
  summarise('error' = sum(abs(error)),
            'sq_error' = sum(error^2))

ggplot(score_dist, aes(x = home_score, y = away_score)) +
  facet_wrap(~method) +
  geom_tile(aes(fill = error), color = 'black') +
  scale_fill_gradient2(high = "red", mid = "white", low = "blue", labels = function(x) {paste0(x*100, '%')}) +
  geom_text(data = df_errors, x = 10, y = 15, aes(label = paste('Sum Absolute Difference:', sprintf('%0.3f', error),
                                                                '\nSum Squared Difference:', sprintf('%0.3f', sq_error)))) +
  labs(x = 'Home Goals',
       y = 'Away Goals',
       fill = 'Simulated Freq. -\nOberserved Freq.') +
  theme(legend.text = element_text(size = 6)) 
ggsave(here('simulations/sim_check.png'), height = 9/1.2, width = 16/1.2)


df_sims %>% 
  group_by(method) %>% 
  summarise('mu' = mean(a_goals),
            'home_advantage' = mean(h_goals - a_goals),
            'home_sd' = sd(h_goals),
            'away_sd' = sd(a_goals)) %>% 
  bind_rows(tibble('method' = 'actual',
                   'mu' = mu,
                   'home_advantage' = home_advantage,
                   'home_sd' = home_sd,
                   'away_sd' = away_sd))

