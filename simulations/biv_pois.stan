data {
  int<lower=1> num_clubs;                           // number of clubs
  int<lower=1> num_games;                           // number of games
  int<lower=1,upper=num_clubs> home[num_games];     // home club for game g
  int<lower=1,upper=num_clubs> away[num_games];     // away club for game g
  int<lower=0> h_goals[num_games];                  // home goals for game g
  int<lower=0> a_goals[num_games];                  // away goals for game g
  int<lower=0,upper=1> homeg[num_games];            // home field for game g
}
parameters {
  vector[num_clubs] raw_alpha;                  // attacking intercepts
  vector[num_clubs] raw_delta;                  // defending intercepts

  real mu;                                          // fixed intercept
  real eta;                                         // homefield
  real<lower=0> sigma_a;                            // attacking sd
  real<lower=0> sigma_d;                            // defending sd
}
transformed parameters {
  vector[num_clubs] alpha;
  vector[num_clubs] delta;
  
  alpha = raw_alpha * sigma_a;
  delta = raw_delta * sigma_d;
}
model {
  vector[num_games] lambda1;
  vector[num_games] lambda2;

  // priors
  raw_alpha ~ normal(0, 1);
  raw_delta ~ normal(0, 1);
  mu ~ normal(0, 10);
  eta ~ normal(0, 10);
  sigma_a ~ normal(0, 10);
  sigma_d ~ normal(0, 10);

  // likelihood
  for (g in 1:num_games) {
    lambda1[g] = exp(mu + (eta * homeg[g]) + alpha[home[g]] + delta[away[g]]);
    lambda2[g] = exp(mu + alpha[away[g]] + delta[home[g]]);
  }
  h_goals ~ poisson(lambda1);
  a_goals ~ poisson(lambda2);
}
