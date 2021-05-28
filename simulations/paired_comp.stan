data {
  int<lower=1> num_clubs;                           // number of clubs
  int<lower=1> num_games;                           // number of games
  int<lower=1,upper=num_clubs> home[num_games];     // home club for game g
  int<lower=1,upper=num_clubs> away[num_games];     // away club for game g
  int goal_diff[num_games];                 
}
parameters {
  vector[num_clubs] team_strength;                  // team strength
  real<lower = 0> sigma_team;
  real alpha;                                         // homefield
  real<lower = 0> sigma;
}
model {
  // priors
  alpha ~ normal(0, 10);
  team_strength ~ normal(0, sigma_team);
  sigma_team ~ inv_gamma(1,1);
  sigma ~ inv_gamma(1,1);
  
  for(g in 1:num_games) {
    goal_diff[g] ~ normal(alpha + team_strength[home[g]] - team_strength[away[g]], sigma);
  }
}
