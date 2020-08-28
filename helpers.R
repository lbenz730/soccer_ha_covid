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
  df <- read_leage_csvs(league) %>% 
    filter(!is.na(home_score), !is.na(away_score))
  tibble('league' = league,
         'home_win_rate' = mean(df$home_score > df$away_score),
         'away_win_rate' = mean(df$home_score < df$away_score),
         'draw_rate' = mean(df$home_score == df$away_score),
         'avg_goal_diff' = mean(df$home_score - df$away_score))
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