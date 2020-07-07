library(tidyverse)
library(XML)
library(RCurl)
library(furrr)
library(glue)
plan(multicore(workers = parallel::detectCores() - 1))
options(future.fork.enable = T)


get_match_stats <- function(game_html) {
  ### Get metadata about the match
  text <- unlist(str_split(game_html, "<a href=\"/en/matches/"))
  date <- as.Date(substring(text[2], 1, 10))
  game_url <- paste0("https://fbref.com/en/matches/", gsub("\".*", "", text[3]))
  home_team <- gsub("^.*\">", "", gsub("</a>.*", "", gsub("^.*data-stat=\"squad_a\" >", "", game_html)))
  away_team <- gsub("^.*\">", "", gsub("</a>.*", "", gsub("^.*data-stat=\"squad_b\" >", "", game_html)))
  home_xg <- as.numeric(gsub("</td>.*", "", gsub("^.*data-stat=\"xg_a\" >", "", game_html)))
  away_xg <- as.numeric(gsub("</td>.*", "", gsub("^.*data-stat=\"xg_b\" >", "", game_html)))
  referee <- gsub(".*>", "", gsub("</td>.*", "", gsub("^.*data-stat=\"referee\".csk=\"", "", game_html)))
  attendance <- as.numeric(gsub(",", "", gsub(".*>", "", gsub("</td>.*", "", gsub("^.*data-stat=\"attendance\".csk=\"", "", game_html)))))
  scores <- as.numeric(str_extract(unlist(str_split(game_html, "&ndash;")), "^\\d+|\\d+$"))
  home_score <- scores[1]
  away_score <- scores[2]
  
  if(!is.na(home_score) & is.na(attendance)) {
    attendance <- 0
  }
  
  
  df <- tibble("date" = date,
               "home" = home_team,
               "away" = away_team,
               "home_score" = home_score,
               "away_score" = away_score,
               "home_xg" = home_xg,
               "away_xg" = away_xg,
               "attendance" = attendance,
               "referee" = referee,
               "match_report_url" = game_url)
  
  ### Lots of stats from the match report
  if(!str_detect(game_url, "NA")) {
    match_report <- readHTMLTable(getURL(game_url))
    ix <- which(grepl("summary", names(match_report)))
    home_ix <- ix[1]
    away_ix <- ix[2]
    
    home_df <- match_report[[home_ix]] 
    away_df <- match_report[[away_ix]] 
    
    misc_ix <- which(grepl("misc", names(match_report)))
    if(length(misc_ix) > 0 & !is.null(home_df) & !is.null(away_df)) {
      names(home_df)[c(25, 27, 29, 31)] <- c("pATT", "pPrgDist", "cPrgDist", "dAtt")
      names(away_df)[c(25, 27, 29, 31)] <- c("pATT", "pPrgDist", "cPrgDist", "dAtt")
      home_df <- 
        select(home_df,
               "pk_goals" = PK,
               "pk_attempts" = PKatt,
               "yellow_cards" = CrdY,
               "red_cards" = CrdR,
               "shots" = Sh,
               "shots_on_target" = SoT,
               "touches" = Touches,
               "pressures" = Press,
               "tackles" = Tkl,
               "interceptions" = Int,
               "blocks" = Blocks,
               "shot_creating_actions" = SCA,
               "goal_creating_actions" = GCA,
               "passes_attempted" = pATT,
               "passes_completed" = Cmp,
               "carries" = Carries,
               "passing_progressive_dist" = pPrgDist,
               "carry_progressive_dist" = cPrgDist,
               "dribble_attempts" = dAtt,
               "dribble_success" = Succ) %>% 
        mutate_all(as.character) %>% 
        mutate_all(as.numeric) %>% 
        apply(2, sum) %>% 
        tibble::enframe(.) %>% 
        pivot_wider(names_from = name,
                    values_from = value) %>% 
        mutate("fouls_committed" = sum(as.numeric(as.character(match_report[[misc_ix[1]]]$Fls))))
      
      away_df <- 
        select(away_df,
               "pk_goals" = PK,
               "pk_attempts" = PKatt,
               "yellow_cards" = CrdY,
               "red_cards" = CrdR,
               "shots" = Sh,
               "shots_on_target" = SoT,
               "touches" = Touches,
               "pressures" = Press,
               "tackles" = Tkl,
               "interceptions" = Int,
               "blocks" = Blocks,
               "shot_creating_actions" = SCA,
               "goal_creating_actions" = GCA,
               "passes_attempted" = pATT,
               "passes_completed" = Cmp,
               "carries" = Carries,
               "passing_progressive_dist" = pPrgDist,
               "carry_progressive_dist" = cPrgDist,
               "dribble_attempts" = dAtt,
               "dribble_success" = Succ) %>% 
        mutate_all(as.character) %>% 
        mutate_all(as.numeric) %>% 
        apply(2, sum) %>% 
        tibble::enframe(.) %>% 
        pivot_wider(names_from = name,
                    values_from = value) %>% 
        mutate("fouls_committed" = sum(as.numeric(as.character(match_report[[misc_ix[2]]]$Fls))))
      
      names(home_df) <- paste0("home_", names(home_df))
      names(away_df) <- paste0("away_", names(away_df))
    } else if(!is.null(home_df) & !is.null(away_df)) {
      
      home_df <- 
        select(home_df,
               "pk_goals" = PK,
               "pk_attempts" = PKatt,
               "yellow_cards" = CrdY,
               "red_cards" = CrdR,
               "shots" = Sh,
               "shots_on_target" = SoT,
               "tackles" = TklW,
               "interceptions" = Int,
               "fouls_committed" = Fls) %>% 
        mutate_all(as.character) %>% 
        mutate_all(as.numeric) %>% 
        apply(2, sum) %>% 
        tibble::enframe(.) %>% 
        pivot_wider(names_from = name,
                    values_from = value)
      
      away_df <- 
        select(away_df,
               "pk_goals" = PK,
               "pk_attempts" = PKatt,
               "yellow_cards" = CrdY,
               "red_cards" = CrdR,
               "shots" = Sh,
               "shots_on_target" = SoT,
               "tackles" = TklW,
               "interceptions" = Int,
               "fouls_committed" = Fls) %>% 
        mutate_all(as.character) %>% 
        mutate_all(as.numeric) %>% 
        apply(2, sum) %>% 
        tibble::enframe(.) %>% 
        pivot_wider(names_from = name,
                    values_from = value)
      
      names(home_df) <- paste0("home_", names(home_df))
      names(away_df) <- paste0("away_", names(away_df))
      
    }
    
  } else {
    home_df <- NULL
    away_df <- NULL
  }
  
  df <- bind_cols(df, home_df, away_df)
  
  return(df) 
}


### Scrape Data
league_info <- read_csv("league_info.csv")
for(i in 9:nrow(league_info)) {
  base_url <- league_info$fbref_url[i]
  league_id <- league_info$fbref_league_id[i]
  league <- gsub("https://fbref.com/en/comps/\\d+/history/", "", gsub("-Seasons", "", base_url))
  
  x <- scan(base_url, sep = "\n", what = "")
  x <- x[str_detect(x, "<tr ><th scope=\"row\" class=\"left \" data-stat=\"season\"")]
  years <- gsub(".*-Stats\">", "", gsub("</a>.*", "", gsub("<tr ><th scope=\"row\" class=\"left \" data-stat=\"season\" >", "", x)))
  season_ids <- as.numeric(gsub("\\/.*", "", gsub(glue(".*/en/comps/{league_id}/"), "", x)))
  
  urls <- gsub("NA\\/", "", paste0("https://fbref.com/en/comps/", league_id, "/", season_ids, "/schedule/", years, "-", league, "-Fixtures"))
  
  if(!dir.exists(paste0("fbref_data/", gsub("\\s", "_", tolower(league_info$alias[i]))))) {
    dir.create(paste0("fbref_data/", gsub("\\s", "_", tolower(league_info$alias[i]))))
  }
  
  for(j in 1:5) {
    url <- urls[j]
    x <- scan(url, what = "", sep = "\n")
    x <- x[str_detect(x, "data-stat=\"gameweek|round\" ") & str_detect(x, "^<tr ><th scope=\"row\"")]
    df_stats <- future_map_dfr(x, ~get_match_stats(.x)) %>% 
      distinct() %>% 
      mutate("season" = years[j])
    write_csv(df_stats, paste0("fbref_data/", gsub("\\s", "_", tolower(league_info$alias[i])), "/", years[j], ".csv"))
  }
  
}