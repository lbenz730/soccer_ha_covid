library(tidyverse)
library(here)
library(glue)

### Get All CSVs for given League (alias column in league_info.csv)
read_leage_csvs <- function(league) {
  file_league <- gsub("\\s", "_", tolower(league))
  files <- dir(here(paste0("fbref_data/", file_league)), full.names = T)
  
  game_stats <- 
    map_dfr(files, read_csv) %>% 
    arrange(date)
  
  return(game_stats)
}

### Dictionary of Team Codes
team_codes <- function(df) {
  teams <- sort(unique(c(df$home, df$away)))
  codes <- 1:length(teams)
  names(codes) <- teams
  
  return(codes)
}

### Home win, away win, draw rates by league
wld_rates <- function(league) {
  df <- read_leage_csvs(league) 
  tibble('league' = league,
         'home_win_rate' = mean(df$home_score > df$away_score, na.rm = T),
         'away_win_rate' = mean(df$home_score < df$away_score, na.rm = T),
         'draw_rate' = mean(df$home_score == df$away_score, na.rm = T),
         'yc_draw_rate' = mean(df$home_yellow_cards == df$away_yellow_cards, na.rm = T),
         'goals_per_game' = mean(df$home_score + df$away_score, na.rm = T),
         'yc_per_game' = mean(df$home_yellow_cards + df$away_yellow_cards, na.rm = T),
         'rc_per_game' = mean(df$home_red_cards + df$away_red_cards, na.rm = T),
         'avg_goal_diff' = mean(df$home_score - df$away_score, na.rm = T),
         'avg_yc_diff' = mean(df$home_yellow_cards - df$away_yellow_cards, na.rm = T),
         'avg_rc_diff' = mean(df$home_red_cards - df$away_red_cards, na.rm = T))
}

### Get quantile/mean from posterio
posterior_quantile <- function(model, league, parameter, q = 0.5, mean = F) {
  league <- gsub("\\s", "_", tolower(league))
  posterior <- read_rds(here(glue('posteriors/{model}/{league}.rds')))
  if(!mean) {
    return(quantile(unlist(posterior[parameter]), q))
  } else {
    return(mean(unlist(posterior[parameter]))) 
  }
}

### ggplot theme
theme_set(theme_bw() +
            theme(plot.title = element_text(hjust = 0.5, size = 24),
                  plot.subtitle = element_text(hjust = 0.5, size = 18),
                  axis.title = element_text(size = 20),
                  strip.text = element_text(size = 14),
                  legend.position = "bottom"))