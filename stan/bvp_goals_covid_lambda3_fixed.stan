data {
  int<lower=1> num_clubs;                                     // number of clubs
  int<lower=1> num_games;                                     // number of games
  int<lower=1,upper=num_clubs> home_team_code[num_games];     // home club for game g
  int<lower=1,upper=num_clubs> away_team_code[num_games];     // away club for game g
  int<lower=0> h_goals[num_games];                            // home goals for game g
  int<lower=0> a_goals[num_games];                            // away goals for game g
  int<lower=0,upper=1> ind_pre[num_games];                    // indicator if game is pre/post covid
}
parameters {
  vector[num_clubs] alpha;                  // attacking intercepts
  vector[num_clubs] delta;                  // defending intercepts

  real mu;                                      // fixed intercept
  real home_field_pre;                              // home field advantage (pre-covid)
  real home_field_post;                              // home field advantage (post-covid)
  real fixed_cov;                             // covariance intercept
  real<lower=0> sigma_a;                        // attacking sd
  real<lower=0> sigma_d;                        // defending sd
}
model {
  vector[num_games] lambda1;
  vector[num_games] lambda2;
  vector[num_games] lambda3;

  // priors
  alpha ~ normal(0, sigma_a);
  delta ~ normal(0, sigma_d);
  mu ~ normal(0, 10);
  fixed_cov ~ normal(0, 10);
  home_field_pre ~ normal(0, 10); //pre and post
  home_field_post ~ normal(0, 10); //pre and post
  sigma_a ~ inv_gamma(1,1);
  sigma_d ~ inv_gamma(1,1);

  // likelihood
  for (g in 1:num_games) {
    lambda1[g] = exp(mu + home_field_pre * ind_pre[g] + home_field_post * (1 - ind_pre[g]) + alpha[home_team_code[g]] + delta[away_team_code[g]]);
    lambda2[g] = exp(mu + alpha[away_team_code[g]] + delta[home_team_code[g]]);
    lambda3[g] = exp(fixed_cov); // intercept
  }
  h_goals ~ poisson(lambda1 + lambda3);
  a_goals ~ poisson(lambda2 + lambda3);
}
generated quantities{
    // vector[num_games] log_lik_1;
    // vector[num_games] log_lik_2;
    // //vector[2*num_games] log_lik;
    // vector[num_games] lambda1;
    // vector[num_games] lambda2;
    real lambda3;
    
    lambda3 = exp(fixed_cov); 
    
    // for (g in 1:num_games) {
    //   lambda1[g] = exp(mu + home_field_pre * ind_pre[g] + home_field_post * (1 - ind_pre[g]) + alpha[home_team_code[g]] + delta[away_team_code[g]]);
    //   lambda2[g] = exp(mu + alpha[away_team_code[g]] + delta[home_team_code[g]]);
    //   log_lik_1[g] = poisson_lpmf( h_goals[g] | lambda1[g] + lambda3 );
    //   log_lik_2[g] = poisson_lpmf( a_goals[g] | lambda2[g] + lambda3 );    
    // }
    
    // log_lik = append_row(log_lik_1, log_lik_2);
    
}
