library(tidyverse)

### Get All CSVs for given League (alias column in league_info.csv)
read_leage_csvs <- function(league) {
  file_league <- gsub("\\s", "_", tolower(league))
  files <- dir(paste0("../fbref_data/", file_league), full.names = T)
  
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
