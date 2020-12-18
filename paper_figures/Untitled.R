df_means %>% 
  select(-logo_url) %>% 
  mutate('delta' = mean_post -mean_pre) %>% 
  mutate('pct' = paste0(sprintf('%0.1f',  100 * delta/abs(mean_pre)), '%')) %>% 
  xtable::xtable(digits = c(0, 0, 3,3,3,1)) %>% 
  print(include.rownames = F)

                 