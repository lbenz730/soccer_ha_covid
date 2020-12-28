data {
  int<lower=1> num_clubs;                                     // number of clubs
  int<lower=1> num_games;                                     // number of games
  int<lower=1> num_seasons;                                     // number of seasons
  
  int<lower=1,upper=num_seasons> season[num_games];           // season for game g
  int<lower=1,upper=num_clubs> home_team_code[num_games];     // home club for game g
  int<lower=1,upper=num_clubs> away_team_code[num_games];     // away club for game g
  
  int<lower=0> h_yc[num_games];                               // yellow cards for game g
  int<lower=0> a_yc[num_games];                               // away yellow goals for game g
  
  int<lower=0,upper=1> ind_pre[num_games];                    // indicator if game is pre/post covid
  
  // Parameters for Empirical Bayes Prios
  real mu_hf_pre;
  real mu_hf_post;
  real sd_hf_pre;
  real sd_hf_post;
}
parameters {
  vector[num_clubs] gamma;                  // team intercepts [tau in Model (4)]
  real<lower=0> sigma_g;                  // team intercept sd [sigma_team in Model (4)]
  
  vector[num_seasons] mu;                   // mean yc/game
  real home_field_pre;                     // home field advantage (pre-covid) [T in Model (4)]
  real home_field_post;                    // home field advantage (post-covid) [T' in Model (4)
  real fixed_cov;                         // covariance intercept [gamma in Model (4)]
}
model {
  vector[num_games] lambda1;
  vector[num_games] lambda2;
  vector[num_games] lambda3;
  
  // priors
  gamma ~ normal(0, sigma_g);
  mu ~ normal(0, 5);
  sigma_g ~ inv_gamma(1,1);
  fixed_cov ~ normal(0, sqrt(2));
  sigma_g ~ inv_gamma(1,1);
  
  // Empirical Bayes Priors
  home_field_pre ~ normal(mu_hf_pre, sd_hf_pre);
  home_field_post ~ normal(mu_hf_post, sd_hf_post);
  
  
  // likelihood
  for (g in 1:num_games) {
    lambda1[g] = exp(mu[season[g]] + home_field_pre * ind_pre[g] + home_field_post * (1 - ind_pre[g]) + gamma[home_team_code[g]]);
    lambda2[g] = exp(mu[season[g]] + gamma[home_team_code[g]]);
    lambda3[g] = exp(fixed_cov); // intercept
  }
  h_yc ~ poisson(lambda1 + lambda3);
  a_yc ~ poisson(lambda2 + lambda3);
}
generated quantities{
  vector[num_games] lambda1;
  vector[num_games] lambda2;
  real lambda3;
  
  real log_lik_1 = 0;
  real log_lik_2 = 0;
  
  lambda3 = exp(fixed_cov); 
  for (g in 1:num_games) {
    lambda1[g] = exp(mu[season[g]] + home_field_pre * ind_pre[g] + home_field_post * (1 - ind_pre[g]) + gamma[home_team_code[g]]);
    lambda2[g] = exp(mu[season[g]] + gamma[home_team_code[g]]);
    
    // Update total log likelihood
    log_lik_1 += poisson_lpmf(h_yc[g] | lambda1[g] + lambda3);
    log_lik_2 += poisson_lpmf(a_yc[g] | lambda2[g] + lambda3);
  }
}
