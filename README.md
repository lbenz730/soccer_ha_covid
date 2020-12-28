# Soccer HA Covid
This repository contains the data and code used in a manuscript _Estimating the change in soccer's home advantage during the Covid-19 pandemic using bivariate Poisson regression_ by Luke Benz and Michael Lopez. A pre-print of our manuscript is available [here]().

### Data:

* __league_info.csv:__ csv of general information on 17 European Leagues used in this analysis.
* __fbref_scraper.R:__ R script for scraping game level statistics for each league.
* __fbref_data/__: Folder containing all data used for this project. Each of the 17 leagues has it's own folder, containing 5 csv files of game level statistics for games played that year. 

### Models

* __models/cards/__ R scripts for fitting yellow card models (Model (4) in paper)
* __stan/cards/__ STAN files for fitting yellow card models (Model (4) in paper)
* __models/goals/__ R scripts for fitting goal card models (Model (3) in paper)
* __stan/cards/__ STAN files for fitting goal card models (Model (3) in paper)
* __models/empirical_baselines.R:__ Saves __models/empirical_baselines.csv__ for empirical Bayes priors in Models (3) and (4) with no-zero correlation.

### Simulations

* __simulations/simulation.R__ R script for running simulations described in Section (4).
* __simulations/biv_pois.stan__ STAN file for running simulations described in Section (4).
* __simulations/sim_files/:__ Folder of saved simulation results

### Other

* __paper_figures/:__ R scripts to produce all figures and tables we present in the manuscript.
* __eda/:__ Folder of old or draft versions of analysis.
* __helpers.R:__ R script of useful helper functions
