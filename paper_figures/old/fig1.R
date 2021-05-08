library(tidyverse)
library(here)
source(here('helpers.R'))
library(ggbeeswarm)

sim_files <- dir(here('simulations/sim_files'), full.names = T)
simulation <- map_dfr(sim_files, read_csv)


simulation_long <-  
  simulation %>% 
  pivot_longer(cols = c(bivpois_eta, lm_eta), 
               names_to = "type", 
               values_to = "estimated_ha")


simulation_long %>% 
  mutate(estimated_ha = 
           ifelse(type != "lm_eta", exp(estimated_ha)-1, estimated_ha), 
         estimated_ha_bias = estimated_ha - (exp(true_params)-1)) %>%
  group_by(type) %>% 
  summarise(mean_abs_bias = mean(abs(estimated_ha_bias)), 
            mean_bias = mean(estimated_ha_bias),
            lower_025 = quantile(estimated_ha_bias, 0.025), 
            upper_975 = quantile(estimated_ha_bias, 0.975))

simulation_long %>% 
  mutate(estimated_ha = 
           ifelse(type != "lm_eta", exp(estimated_ha)-1, estimated_ha), 
         estimated_ha_bias = estimated_ha - (exp(true_params)-1)) %>%
  ggplot(aes(x = (estimated_ha_bias), y = type, fill = type)) + 
  geom_boxplot(width = 0.1) + 
  geom_quasirandom(alpha = 0.5, pch = 21, groupOnX = FALSE) + 
  labs(title = "Simulated Bias in Home Advantage Estimates", 
       x = "Bias (Estimated HA - Known HA)") + 
  scale_y_discrete(labels = c("Bivariate\nPoisson", "Linear\nRegression"), "") + 
  geom_vline(aes(xintercept = 0), lty = 2, lwd =0.5) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 24),
        plot.title = element_text(hjust = 0.5, size = 30),
        plot.subtitle = element_text(hjust = 0.5, size = 24),
        legend.position = 'none')
ggsave(file = here("paper_figures/figures/figure1.png"), height = 9/1.2, width = 16/1.2)

