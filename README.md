# Soccer HA Covid
This repository contains the data and code used in a manuscript _Estimating the change in soccer's home advantage during the Covid-19 pandemic using bivariate Poisson regression_ by Luke Benz and Michael Lopez. A pre-print of our manuscript is available [here](https://arxiv.org/abs/2012.14949).

### Data:

* __league_info.csv:__ csv of general information on 17 European Leagues used in this analysis. Most important are the `restart_date` (the date the league returned to play following the pause of its season to the Covid-19 pandemic) and `fbref_league_id`, the league's unique id on [Football Reference](https://fbref.com/en/).
* __fbref_scraper.R:__ R script for scraping game level statistics for each league from [Football Reference](https://fbref.com/en/)
* __fbref_data/__: Folder containing all data used for this project. Each of the 17 leagues has its own folder, containing 5 csv files of game level statistics for games played that year. 

### Models

* __models/cards/__ R scripts for fitting yellow card models (Model (4) in paper)
* __stan/cards/__ Stan files for fitting yellow card models (Model (4) in paper)
* __models/goals/__ R scripts for fitting goal card models (Model (3) in paper)
* __stan/cards/__ Stan files for fitting goal card models (Model (3) in paper)
* __models/empirical_baselines.R:__ Saves __models/empirical_baselines.csv__ for empirical Bayes priors in Models (3) and (4) with no-zero correlation.

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
* __simulations/paired_comp.stan__ STAN file for running simulations described in Section (4).
* __simulations/sim_files/:__ Folder of saved simulation results (specifically, see __v2_sims/__ for the most up to date versions in version 2 of the paper).

### Other

* __paper_figures/:__ R scripts to produce all figures and tables we present in the manuscript.
* __eda/:__ Folder of old or draft versions of analysis.
* __helpers.R:__ R script of useful helper functions

---

### Steps to Run the Model(s)
In order to replicate the entire model fitting process simply run:

* [models/goals/bvp_goals_no_corr.R](https://github.com/lbenz730/soccer_ha_covid/blob/master/models/goals/bvp_goals_no_corr.R) Model (3) presented in our manuscript. 
* [models/cards/bvp_yc_lambda3.R](https://github.com/lbenz730/soccer_ha_covid/blob/master/models/cards/bvp_yc_lambda3.R) Model (4) presented in our manuscript. 

Such a script takes the following steps:

1. Creates model-specific directories for saving both model objects and posterior `.rds` files.
2. Reads in data for a specific league from __fbref_data/__ folder, via `read_league_csvs()` helper function in __helpers.R__.
3. Filters data to relevant games.
4. Prepares data for use by Stan.
5. Sources corresponding Stan file and fits model.
6. Saves league specific model `.rds` object into model-specific folder.
7. Saves posterior draws `.rds` obeject into model-specific folder.
8. Repeat 2-7 for each of 17 leagues. 

Model (4), our yellow card model which assumes correlation > 0, relies on [empirical baselines](https://github.com/lbenz730/soccer_ha_covid/blob/master/models/empirical_baselines.csv) as priors from a [version of the model fit with no correlation](https://github.com/lbenz730/soccer_ha_covid/blob/master/models/cards/bvp_yc_no_corr.R). 

In order to fully reproduce the results in our paper, one would run the following scripts in order:

1. [models/goals/bvp_goals_no_corr.R](https://github.com/lbenz730/soccer_ha_covid/blob/master/models/goals/bvp_goals_no_corr.R): Model (3) presented in our manuscript. 
2. [models/cards/bvp_yc_no_corr.R](https://github.com/lbenz730/soccer_ha_covid/blob/master/models/cards/bvp_yc_no_corr.R): For yellow card priors when fitting Model (4). 
3. [models/empirical_baselines.R](): Extracting posterior means of no-correlataion versions of models for use in priors when fitting verions of models with correlation.
4. [models/cards/bvp_yc_lambda3.R](https://github.com/lbenz730/soccer_ha_covid/blob/master/models/cards/bvp_yc_lambda3.R) Model (4) presented in our manuscript.
5. [models/goals/bvp_goals_lambda3.R](https://github.com/lbenz730/soccer_ha_covid/blob/master/models/goals/bvp_goals_lambda3.R): Verion of Model (3) w/ corrletion; not presented in manuscript.


---

Note the `rstan` package is required to work with model objects and/or run the modeling scripts. For assistance installing Stan, please refer to the [official documentation](https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started).
