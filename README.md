# Soccer HA Covid
This repository contains the data and code used in a manuscript _Estimating the change in soccer's home advantage during the Covid-19 pandemic using bivariate Poisson regression_ by Luke Benz and Michael Lopez. A pre-print of our manuscript is available [here]().

### Data:

* __league_info.csv:__ csv of general information on 17 European Leagues used in this analysis. Most important are the `restart_date` (the date the league returned to play following the pause of its season to the Covid-19 pandemic) and `fbref_league_id`, the league's unique id on [Football Reference](https://fbref.com/en/).
* __fbref_scraper.R:__ R script for scraping game level statistics for each league from [Football Reference](https://fbref.com/en/)
* __fbref_data/__: Folder containing all data used for this project. Each of the 17 leagues has its own folder, containing 5 csv files of game level statistics for games played that year. 

### Models

* __models/cards/__ R scripts for fitting yellow card models (Model (4) in paper)
* __stan/cards/__ STAN files for fitting yellow card models (Model (4) in paper)
* __models/goals/__ R scripts for fitting goal card models (Model (3) in paper)
* __stan/cards/__ STAN files for fitting goal card models (Model (3) in paper)
* __models/empirical_baselines.R:__ Saves __models/empirical_baselines.csv__ for empirical Bayes priors in Models (3) and (4) with no-zero correlation.

In order to replicate the entire model fitting process simply run one the appropriate R script from above. For example [models/goals/bvp_goals_no_corr.R](https://github.com/lbenz730/soccer_ha_covid/blob/master/models/goals/bvp_goals_no_corr.R) is Model (3) presented in our manuscript while [models/cards/bvp_yc_lambda3.R](https://github.com/lbenz730/soccer_ha_covid/blob/master/models/cards/bvp_yc_lambda3.R) is Model (4) presented in our manuscript.

### Posteriors
`.rds` files of posterior draws are available for goals and yellow card models for each league. 

* __bvp_goals_no_corr/:__ Folder of posterior draw `.rds` objects for Model (3) in paper
* __bvp_goals_lambda_3/:__ Folder of posterior draw `.rds` objects for Model (4) in paper

### Model Objects
`.rds` files of model objects are available for goals and yellow card models for each league. 

* __bvp_goals_no_corr/:__ Folder of `.rds` model objects for Model (3) in paper
* __bvp_goals_lambda_3/:__ Folder of `.rds` model objects for Model (4) in paper

### Simulations

* __simulations/simulation.R__ R script for running simulations described in Section (4).
* __simulations/biv_pois.stan__ STAN file for running simulations described in Section (4).
* __simulations/sim_files/:__ Folder of saved simulation results

### Other

* __paper_figures/:__ R scripts to produce all figures and tables we present in the manuscript.
* __eda/:__ Folder of old or draft versions of analysis.
* __helpers.R:__ R script of useful helper functions


Note the `rstan` package is required to work with model objects and/or run the modelling scripts. For assistance installing Stan, please refer to the [official documentation](https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started).
