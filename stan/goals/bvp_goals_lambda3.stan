data {
  int<lower=1> num_clubs;                                     // number of clubs
  int<lower=1> num_games;                                     // number of games
  int<lower=1> num_seasons;                                     // number of seasons
  
  int<lower=1,upper=num_seasons> season[num_games];           // season for game g
  int<lower=1,upper=num_clubs> home_team_code[num_games];     // home club for game g
  int<lower=1,upper=num_clubs> away_team_code[num_games];     // away club for game g
  
  int<lower=0> h_goals[num_games];                            // home goals for game g
  int<lower=0> a_goals[num_games];                            // away goals for game g
  
  int<lower=0,upper=1> ind_pre[num_games];                    // indicator if game is pre/post covid
  
  // Parameters for Empirical Bayes Prios
  real mu_hf_pre;
  real mu_hf_post;
  real sd_hf_pre;
  real sd_hf_post;
}
parameters {
  vector[num_clubs] alpha;                  // attacking intercepts
  vector[num_clubs] delta;                  // defending intercepts
  
  real<lower=0> sigma_a;                  // attacking sd
  real<lower=0> sigma_d;                  // defending sd
  
  vector[num_seasons] mu;                   // mean goals/game
  real home_field_pre;                     // home field advantage (pre-covid)
  real home_field_post;                    // home field advantage (post-covid)
  real fixed_cov;                         // covariance intercept
}
model {
  vector[num_games] lambda1;
  vector[num_games] lambda2;
  vector[num_games] lambda3;
  
  // priors
  alpha ~ normal(0, sigma_a);
  delta ~ normal(0, sigma_d);
  mu ~ normal(0, 5);
  fixed_cov ~ normal(0, sqrt(0.5));
  sigma_a ~ inv_gamma(1,1);
  sigma_d ~ inv_gamma(1,1);
  
  // Empirical Bayes Priors
  home_field_pre ~ normal(mu_hf_pre, sd_hf_pre);
  home_field_post ~ normal(mu_hf_post, sd_hf_post);

  
  // likelihood
  for (g in 1:num_games) {
    lambda1[g] = exp(mu[season[g]] + home_field_pre * ind_pre[g] + home_field_post * (1 - ind_pre[g]) + alpha[home_team_code[g]] + delta[away_team_code[g]]);
    lambda2[g] = exp(mu[season[g]] + alpha[away_team_code[g]] + delta[home_team_code[g]]);
    lambda3[g] = exp(fixed_cov); // intercept
  }
  h_goals ~ poisson(lambda1 + lambda3);
  a_goals ~ poisson(lambda2 + lambda3);
}
generated quantities{
  vector[num_games] lambda1;
  vector[num_games] lambda2;
  real lambda3;
  
  real log_lik_1 = 0;
  real log_lik_2 = 0;
  
  lambda3 = exp(fixed_cov); 
   for(g in 1:num_games) {
    lambda1[g] = exp(mu[season[g]] + home_field_pre * ind_pre[g] + home_field_post * (1 - ind_pre[g]) + alpha[home_team_code[g]] + delta[away_team_code[g]]);
    lambda2[g] = exp(mu[season[g]] + alpha[away_team_code[g]] + delta[home_team_code[g]]);
    
    // Update total log likelihood
    log_lik_1 += poisson_lpmf(h_goals[g] | lambda1[g] + lambda3);
    log_lik_2 += poisson_lpmf(a_goals[g] | lambda2[g] + lambda3);
  }
}