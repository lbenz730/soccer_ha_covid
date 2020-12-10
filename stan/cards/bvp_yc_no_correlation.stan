data {
  int<lower=1> num_clubs;                                     // number of clubs
  int<lower=1> num_games;                                     // number of games
  int<lower=1,upper=num_clubs> home_team_code[num_games];     // home club for game g
  int<lower=1,upper=num_clubs> away_team_code[num_games];     // away club for game g
  int<lower=0> h_yc[num_games];                               // yellow cards for game g
  int<lower=0> a_yc[num_games];                            // away yellow goals for game g
}
parameters {

  real mu;                                      // fixed intercept
  real home_field;                              // home field advantage
 // real fixed_cov;                             // covariance intercept
  real<lower=0> sigma_g;                        // team_strength
  
  vector[num_clubs] gamma;   
}
model {
  vector[num_games] lambda1;
  vector[num_games] lambda2;
  //vector[num_games] lambda3;

  // priors
  gamma ~ normal(0, sigma_g);
  mu ~ normal(0, 10);
  //fixed_cov ~ normal(0, 10);
  home_field ~ normal(0, 10); 
  sigma_g ~ inv_gamma(1,1);

  // likelihood
  for (g in 1:num_games) {
    lambda1[g] = exp(mu + home_field + gamma[home_team_code[g]]);
    lambda2[g] = exp(mu + gamma[away_team_code[g]]);
    //lambda3[g] = exp(fixed_cov); // intercept
  }
  h_yc ~ poisson(lambda1);
  a_yc ~ poisson(lambda2);
}
