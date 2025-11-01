#' Bayesian Sampling Models for Sentence-Level Observations
#' 
#' This program is free software; you can redistribute it and/or modify
#' it under the terms of the GNU General Public License as published by
#' the Free Software Foundation; either version 3 of the License, or
#' (at your option) any later version.
#
#' This program is distributed in the hope that it will be useful,
#' but WITHOUT ANY WARRANTY; without even the implied warranty of
#' MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#' GNU General Public License for more details.
#
#' A copy of the GNU General Public License is available at
#' http://www.gnu.org/licenses/
#'
#'
#' This code defines Bayesian sampling models for estimating latent variables
#' based on sentence-level observations for a specific student. The models are
#' formulated differently depending on the number of censored (incomplete)
#' and fully observed passages. The goal is to estimate latent variables
#' such as accuracy (theta_acc) and speed (theta_spd) for the student's
#' reading performance.
# Stan Models for Different Scenarios
# Scenario 1: Multiple Observed and Multiple Censored Sentence
testlet_scoring_multi_obs_multi_cens_sentence <- "
data {
  int<lower=0> N_obs;             // Number of observed values
  int<lower=0> N_cens;            // Number of censored values
  int<lower=0> N_score;           // Number of sentences for scoring
  int<lower=0> K;                 // Number of passages with data
  int<lower=0> K_score;           // Number of passages for scoring
  int<lower=0> Passage_obs[N_obs]; // Passage indices of observed values
  int<lower=0> Passage_cens[N_cens]; // Passage indices of censored values
  int<lower=0> Passage_score[N_score]; //Passage indicators for scoring
  int<lower=0> Count_obs[N_obs];        // Array of observed values
  int<lower=0> Count_cens[N_cens];      // Array of censoring points for censored values
  real logT10_obs[N_obs];        // Array of observed values
  real logT10_cens[N_cens];      // Array of censoring points for censored values
  int<lower=0> MaxN_obs[N_obs]; // Sentence lengths of observed values
  int<lower=0> MaxN_cens[N_cens]; // Sentence lengths of censored values
  int<lower=0> MaxN_score[N_score]; //Sentence lengths for scoring passages
  real a_obs[N_obs];
  real a_cens[N_cens];
  real b_obs[N_obs];
  real b_cens[N_cens];
  real alpha_obs[N_obs];
  real alpha_cens[N_cens];
  real beta_obs[N_obs];
  real beta_cens[N_cens];
  real a_score[N_score]; // All _score params are for sentences of scoring passages
  real b_score[N_score];
  real alpha_score[N_score];
  real beta_score[N_score];
  real<lower=0> gamma1;
  real<lower=0> gamma2;
  real<lower=0> sigma;
  real<lower=-1,upper=1> rho;
  real<lower=-1,upper=1> rhoTestlet;
}
parameters {
  real Z1;
  real Z2;
  real V1[K];
  real V2[K];
}

transformed parameters {
  real theta1;
  real theta2;
  real U1[K];
  real U2[K];
  
  theta1 = Z1;
  theta2 = rho*Z1 + sqrt(1-rho^2)*Z2;
  for (k in 1:K) {
    U1[k] = V1[k];
    U2[k] = rhoTestlet*V1[k] + sqrt(1-rhoTestlet^2)*V2[k];
  }
}
model {

      Z1 ~ normal(0, 1);
      Z2 ~ normal(0, 1);
      for (k in 1:K) {
        V1[k] ~ normal(0, 1);
        V2[k] ~ normal(0, 1);
      }

  for (i in 1:N_obs) {
    real p_obs;
    real mu_obs;
    
    p_obs = Phi(fmax(-5, fmin(5, a_obs[i] * (theta1 + gamma1 * U1[Passage_obs[i]]) / sqrt(1 + gamma1^2) - b_obs[i])));
    mu_obs = beta_obs[i]-sigma*(theta2+gamma2*U2[Passage_obs[i]])/sqrt(1+gamma2^2);

    Count_obs[i] ~ binomial(MaxN_obs[i], p_obs);
    logT10_obs[i] ~ normal(mu_obs, 1/alpha_obs[i]);
  }
 
  // Likelihood for censored values - binomial distribution
for (i in 1:N_cens) {
  real p_cens;
  real mu_cens;
  real logLikelihoodCount;
  real logLikelihoodLogT10;

  // Calculate the probability of observing Count_cens[i]
  p_cens = Phi(fmax(-5, fmin(5, a_cens[i] * (theta1 + gamma1 * U1[Passage_cens[i]]) / sqrt(1 + gamma1^2) - b_cens[i])));
  mu_cens = beta_cens[i]-sigma*(theta2+gamma2*U2[Passage_cens[i]])/sqrt(1+gamma2^2);

  // Calculate the log-likelihood for Count_cens[i]
  logLikelihoodCount = binomial_lccdf(Count_cens[i]-1 | MaxN_cens[i], p_cens);

  // Calculate the log-likelihood for logT10_cens[i]
  logLikelihoodLogT10 = normal_lccdf(logT10_cens[i] | mu_cens, 1 / alpha_cens[i]);

  // Add the log-likelihood to the target
  target += logLikelihoodCount + logLikelihoodLogT10;
}
}

// The part below will estimate model-based wrc, seconds, and WCPM from internal or external passages!
// Note the selection of internal vs external is handled with prior functions that would call this stan syntax!

generated quantities{
  real tim_ex[N_score]; //Expeced time matrix
  real <lower=0> cnt_ex[N_score]; //Expected count matrix
  real <lower=0> exp_cnt; //Model-based obs.counts
  real <lower=0> exp_tim; //Model-based secs
  real <lower=0> wcpm; //Model-based WCPM
  
  real V1_score[K_score]; 
  real V2_score[K_score]; 
  real U1_score[K_score];
  real U2_score[K_score];
  
  for (k in 1:K_score){
        V1_score[k] = normal_rng(0, 1);
        V2_score[k] = normal_rng(0, 1);
        U1_score[k] = V1_score[k];
        U2_score[k] = rhoTestlet*V1_score[k] + sqrt(1-rhoTestlet^2)*V2_score[k];
  }

 for(i in 1:N_score){
    real p_score; real mu_score;
    p_score = Phi(fmax(-5, fmin(5, a_score[i] * (theta1 + gamma1 * U1_score[Passage_score[i]]) / sqrt(1 + gamma1^2) - b_score[i])));
    mu_score = beta_score[i]-sigma*(theta2+gamma2*U2_score[Passage_score[i]])/sqrt(1+gamma2^2);
    cnt_ex[i] = p_score * MaxN_score[i];
    tim_ex[i]=(exp(mu_score + log(MaxN_score[i]) -log(10) + 0.5 * 1/(square(alpha_score[i]))));
  }
    exp_cnt=sum(cnt_ex);
    exp_tim=sum(tim_ex);
    wcpm=exp_cnt/exp_tim*60;
}
"
# Scenario 2: Multiple Observed and One Censored Sentence
testlet_scoring_multi_obs_one_cens_sentence <- "
data {
  int<lower=0> N_obs;             // Number of observed values
  int<lower=0> N_score;           // Number of sentences for scoring
  int<lower=0> K;                 // Number of passages with data
  int<lower=0> K_score;           // Number of passages for scoring
  int<lower=0> Passage_obs[N_obs]; // Passage indices of observed values
  int<lower=0> Passage_cens; //   Passage index of censored value
  int<lower=0> Passage_score[N_score]; //Passage indicators for scoring
  int<lower=0> Count_obs[N_obs];        // Array of observed values
  int<lower=0> Count_cens;      // Censoring point for censored value
  real logT10_obs[N_obs];        // Array of observed values
  real logT10_cens;      // Censoring points for censored value
  int<lower=0> MaxN_obs[N_obs]; // Sentence lengths of observed values
  int<lower=0> MaxN_cens; // Sentence length of censored value
  int<lower=0> MaxN_score[N_score]; //Sentence lengths for scoring passages
  real a_obs[N_obs];
  real a_cens;
  real b_obs[N_obs];
  real b_cens;
  real alpha_obs[N_obs];
  real alpha_cens;
  real beta_obs[N_obs];
  real beta_cens;
  real a_score[N_score]; // All _score params are for sentences of scoring passages
  real b_score[N_score];
  real alpha_score[N_score];
  real beta_score[N_score];
  real<lower=0> gamma1;
  real<lower=0> gamma2;
  real<lower=0> sigma;
  real<lower=-1,upper=1> rho;
  real<lower=-1,upper=1> rhoTestlet;
}
parameters {
  real Z1;
  real Z2;
  real V1[K];
  real V2[K];
}

transformed parameters {
  real theta1;
  real theta2;
  real U1[K];
  real U2[K];
  
  theta1 = Z1;
  theta2 = rho*Z1 + sqrt(1-rho^2)*Z2;
  for (k in 1:K) {
    U1[k] = V1[k];
    U2[k] = rhoTestlet*V1[k] + sqrt(1-rhoTestlet^2)*V2[k];
  }
}
model {
    real p_cens;
  real mu_cens;
  real logLikelihoodCount;
  real logLikelihoodLogT10;

      Z1 ~ normal(0, 1);
      Z2 ~ normal(0, 1);
      for (k in 1:K) {
        V1[k] ~ normal(0, 1);
        V2[k] ~ normal(0, 1);
      }

  for (i in 1:N_obs) {
    real p_obs;
    real mu_obs;
    
    p_obs = Phi(fmax(-5, fmin(5, a_obs[i] * (theta1 + gamma1 * U1[Passage_obs[i]]) / sqrt(1 + gamma1^2) - b_obs[i])));
    mu_obs = beta_obs[i]-sigma*(theta2+gamma2*U2[Passage_obs[i]])/sqrt(1+gamma2^2);

    Count_obs[i] ~ binomial(MaxN_obs[i], p_obs);
    logT10_obs[i] ~ normal(mu_obs, 1/alpha_obs[i]);
  }
 
  // Likelihood for censored value - binomial distribution

  // Calculate the probability of observing Count_cens
  p_cens = Phi(fmax(-5, fmin(5, a_cens * (theta1 + gamma1 * U1[Passage_cens]) / sqrt(1 + gamma1^2) - b_cens)));
  mu_cens = beta_cens-sigma*(theta2+gamma2*U2[Passage_cens])/sqrt(1+gamma2^2);

  // Calculate the log-likelihood for Count_cens
  logLikelihoodCount = binomial_lccdf(Count_cens-1 | MaxN_cens, p_cens);

  // Calculate the log-likelihood for logT10_cens
  logLikelihoodLogT10 = normal_lccdf(logT10_cens | mu_cens, 1 / alpha_cens);

  // Add the log-likelihood to the target
  target += logLikelihoodCount + logLikelihoodLogT10;

}
// The part below will estimate model-based wrc, seconds, and WCPM from internal or external passages!
// Note the selection of internal vs external is handled with prior functions that would call this stan syntax!

generated quantities{
  real tim_ex[N_score]; //Expeced time matrix
  real <lower=0> cnt_ex[N_score]; //Expected count matrix
  real <lower=0> exp_cnt; //Model-based obs.counts
  real <lower=0> exp_tim; //Model-based secs
  real <lower=0> wcpm; //Model-based WCPM
  
  real V1_score[K_score]; 
  real V2_score[K_score]; 
  real U1_score[K_score];
  real U2_score[K_score];
  
  for (k in 1:K_score){
        V1_score[k] = normal_rng(0, 1);
        V2_score[k] = normal_rng(0, 1);
        U1_score[k] = V1_score[k];
        U2_score[k] = rhoTestlet*V1_score[k] + sqrt(1-rhoTestlet^2)*V2_score[k];
  }

 for(i in 1:N_score){
    real p_score; real mu_score;
    p_score = Phi(fmax(-5, fmin(5, a_score[i] * (theta1 + gamma1 * U1_score[Passage_score[i]]) / sqrt(1 + gamma1^2) - b_score[i])));
    mu_score = beta_score[i]-sigma*(theta2+gamma2*U2_score[Passage_score[i]])/sqrt(1+gamma2^2);
    cnt_ex[i] = p_score * MaxN_score[i];
    tim_ex[i]=(exp(mu_score + log(MaxN_score[i]) -log(10) + 0.5 * 1/(square(alpha_score[i]))));
  }
    exp_cnt=sum(cnt_ex);
    exp_tim=sum(tim_ex);
    wcpm=exp_cnt/exp_tim*60;
}

"

# Scenario 3: One Observed and Multiple Censored Sentence
testlet_scoring_one_obs_multi_cens_sentence <- "
data {
  int<lower=0> N_cens;            // Number of censored values
  int<lower=0> N_score;           // Number of sentences for scoring
  int<lower=0> K;                 // Number of passages with data
  int<lower=0> K_score;           // Number of passages for scoring
  int<lower=0> Passage_obs;       // Passage index of observed value
  int<lower=0> Passage_cens[N_cens]; // Passage indices of censored values
  int<lower=0> Passage_score[N_score]; //Passage indicators for scoring
  int<lower=0> Count_obs;         // Observed value
  int<lower=0> Count_cens[N_cens]; // Array of censoring points for censored values
  real logT10_obs;               // Observed value
  real logT10_cens[N_cens];      // Array of censoring points for censored values
  int<lower=0> MaxN_obs;         // Sentence length of observed value
  int<lower=0> MaxN_cens[N_cens]; // Sentence lengths of censored values
  int<lower=0> MaxN_score[N_score]; //Sentence lengths for scoring passages
  real a_obs;
  real a_cens[N_cens];
  real b_obs;
  real b_cens[N_cens];
  real alpha_obs;
  real alpha_cens[N_cens];
  real beta_obs;
  real beta_cens[N_cens];
  real a_score[N_score]; // All _score params are for sentences of scoring passages
  real b_score[N_score];
  real alpha_score[N_score];
  real beta_score[N_score];
  real<lower=0> gamma1;
  real<lower=0> gamma2;
  real<lower=0> sigma;
  real<lower=-1,upper=1> rho;
  real<lower=-1,upper=1> rhoTestlet;
}
parameters {
  real Z1;
  real Z2;
  real V1[K];
  real V2[K];
}

transformed parameters {
  real theta1;
  real theta2;
  real U1[K];
  real U2[K];
  
  theta1 = Z1;
  theta2 = rho*Z1 + sqrt(1-rho^2)*Z2;
  for (k in 1:K) {
    U1[k] = V1[k];
    U2[k] = rhoTestlet*V1[k] + sqrt(1-rhoTestlet^2)*V2[k];
  }
}
model {
      real p_obs;
      real mu_obs;

      Z1 ~ normal(0, 1);
      Z2 ~ normal(0, 1);
      for (k in 1:K) {
        V1[k] ~ normal(0, 1);
        V2[k] ~ normal(0, 1);
      }

  // Likelihood for the observed value - binomial and normal distribution

  p_obs = Phi(fmax(-5, fmin(5, a_obs * (theta1 + gamma1 * U1[Passage_obs]) / sqrt(1 + gamma1^2) - b_obs)));
  mu_obs = beta_obs - sigma * (theta2 + gamma2 * U2[Passage_obs]) / sqrt(1 + gamma2^2);

  Count_obs ~ binomial(MaxN_obs, p_obs);
  logT10_obs ~ normal(mu_obs, 1 / alpha_obs);

  // Likelihood for censored values - binomial and normal distribution
  for (i in 1:N_cens) {
    real p_cens;
    real mu_cens;
    real logLikelihoodCount;
    real logLikelihoodLogT10;

    // Calculate the probability of observing Count_cens[i]
    p_cens = Phi(fmax(-5, fmin(5, a_cens[i] * (theta1 + gamma1 * U1[Passage_cens[i]]) / sqrt(1 + gamma1^2) - b_cens[i])));
    mu_cens = beta_cens[i] - sigma * (theta2 + gamma2 * U2[Passage_cens[i]]) / sqrt(1 + gamma2^2);

    // Calculate the log-likelihood for Count_cens[i]
    logLikelihoodCount = binomial_lccdf(Count_cens[i] - 1 | MaxN_cens[i], p_cens);

    // Calculate the log-likelihood for logT10_cens[i]
    logLikelihoodLogT10 = normal_lccdf(logT10_cens[i] | mu_cens, 1 / alpha_cens[i]);

    // Add the log-likelihood to the target
    target += logLikelihoodCount + logLikelihoodLogT10;
  }

}

// The part below will estimate model-based wrc, seconds, and WCPM from internal or external passages!
// Note the selection of internal vs external is handled with prior functions that would call this stan syntax!

generated quantities{
  real tim_ex[N_score]; //Expeced time matrix
  real <lower=0> cnt_ex[N_score]; //Expected count matrix
  real <lower=0> exp_cnt; //Model-based obs.counts
  real <lower=0> exp_tim; //Model-based secs
  real <lower=0> wcpm; //Model-based WCPM
  
  real V1_score[K_score]; 
  real V2_score[K_score]; 
  real U1_score[K_score];
  real U2_score[K_score];
  
  for (k in 1:K_score){
        V1_score[k] = normal_rng(0, 1);
        V2_score[k] = normal_rng(0, 1);
        U1_score[k] = V1_score[k];
        U2_score[k] = rhoTestlet*V1_score[k] + sqrt(1-rhoTestlet^2)*V2_score[k];
  }

 for(i in 1:N_score){
    real p_score; real mu_score;
    p_score = Phi(fmax(-5, fmin(5, a_score[i] * (theta1 + gamma1 * U1_score[Passage_score[i]]) / sqrt(1 + gamma1^2) - b_score[i])));
    mu_score = beta_score[i]-sigma*(theta2+gamma2*U2_score[Passage_score[i]])/sqrt(1+gamma2^2);
    cnt_ex[i] = p_score * MaxN_score[i];
    tim_ex[i]=(exp(mu_score + log(MaxN_score[i]) -log(10) + 0.5 * 1/(square(alpha_score[i]))));
  }
    exp_cnt=sum(cnt_ex);
    exp_tim=sum(tim_ex);
    wcpm=exp_cnt/exp_tim*60;
}

"
# Scenario 4: Multiple Observed and No Censored Sentence
testlet_scoring_multi_obs_no_cens_sentence <- "
data {
  int<lower=0> N_obs;             // Number of sentences read
  int<lower=0> N_score;           // Number of sentences for scoring
  int<lower=0> K;                 // Number of passages read
  int<lower=0> K_score;           // Number of passages for scoring
  int<lower=0> Passage_obs[N_obs]; // Passage indicators for sentences read
  int<lower=0> Passage_score[N_score]; //Passage indicators for scoring
  int<lower=0> Count_obs[N_obs];        // Array of observed values
  real logT10_obs[N_obs];        // Array of observed values
  int<lower=0> MaxN_obs[N_obs]; // Sentence lengths of observed values
  int<lower=0> MaxN_score[N_score]; //Sentence lengths for scoring passages
  real a_obs[N_obs]; // All _obs params are for sentences of passages read!
  real b_obs[N_obs];
  real alpha_obs[N_obs];
  real beta_obs[N_obs];
  real a_score[N_score]; // All _score params are for sentences of scoring passages
  real b_score[N_score];
  real alpha_score[N_score];
  real beta_score[N_score];
  real<lower=0> gamma1;
  real<lower=0> gamma2;
  real<lower=0> sigma;
  real<lower=-1,upper=1> rho;
  real<lower=-1,upper=1> rhoTestlet;
}
parameters {
  real Z1;
  real Z2;
  real V1[K];
  real V2[K];
}

transformed parameters {
  real theta1;
  real theta2;
  real U1[K];
  real U2[K];
  
  theta1 = Z1;
  theta2 = rho*Z1 + sqrt(1-rho^2)*Z2;
  for (k in 1:K) {
    U1[k] = V1[k];
    U2[k] = rhoTestlet*V1[k] + sqrt(1-rhoTestlet^2)*V2[k];
  }
}
model {

      Z1 ~ normal(0, 1);
      Z2 ~ normal(0, 1);
      for (k in 1:K) {
        V1[k] ~ normal(0, 1);
        V2[k] ~ normal(0, 1);
      }

  for (i in 1:N_obs) {
    real p_obs;
    real mu_obs;
    
    p_obs = Phi(fmax(-5, fmin(5, a_obs[i] * (theta1 + gamma1 * U1[Passage_obs[i]]) / sqrt(1 + gamma1^2) - b_obs[i])));
    mu_obs = beta_obs[i]-sigma*(theta2+gamma2*U2[Passage_obs[i]])/sqrt(1+gamma2^2);

    Count_obs[i] ~ binomial(MaxN_obs[i], p_obs);
    logT10_obs[i] ~ normal(mu_obs, 1/alpha_obs[i]);
  }

}

// The part below will estimate model-based wrc, seconds, and WCPM from internal or external passages!
// Note the selection of internal vs external is handled with prior functions that would call this stan syntax!
generated quantities{
  real tim_ex[N_score]; //Expeced time matrix
  real <lower=0> cnt_ex[N_score]; //Expected count matrix
  real <lower=0> exp_cnt; //Model-based obs.counts
  real <lower=0> exp_tim; //Model-based secs
  real <lower=0> wcpm; //Model-based WCPM
  
  real V1_score[K_score]; 
  real V2_score[K_score]; 
  real U1_score[K_score];
  real U2_score[K_score];
  
  for (k in 1:K_score){
        V1_score[k] = normal_rng(0, 1);
        V2_score[k] = normal_rng(0, 1);
        U1_score[k] = V1_score[k];
        U2_score[k] = rhoTestlet*V1_score[k] + sqrt(1-rhoTestlet^2)*V2_score[k];
  }

 for(i in 1:N_score){
    real p_score; real mu_score;
    p_score = Phi(fmax(-5, fmin(5, a_score[i] * (theta1 + gamma1 * U1_score[Passage_score[i]]) / sqrt(1 + gamma1^2) - b_score[i])));
    mu_score = beta_score[i]-sigma*(theta2+gamma2*U2_score[Passage_score[i]])/sqrt(1+gamma2^2);
    cnt_ex[i] = p_score * MaxN_score[i];
    tim_ex[i]=(exp(mu_score + log(MaxN_score[i]) -log(10) + 0.5 * 1/(square(alpha_score[i]))));
  }
    exp_cnt=sum(cnt_ex);
    exp_tim=sum(tim_ex);
    wcpm=exp_cnt/exp_tim*60;
}

"

# Scenario 5: No Observed and Multiple Censored Sentence
testlet_scoring_no_obs_multi_cens_sentence <- " 
data {
  int<lower=0> N_cens;            // Number of censored values
  int<lower=0> N_score;           // Number of sentences for scoring
  int<lower=0> K;                 // Number of passages with data
  int<lower=0> K_score;           // Number of passages for scoring
  int<lower=0> Passage_cens[N_cens]; // Passage indices of censored values
  int<lower=0> Passage_score[N_score]; //Passage indicators for scoring
  int<lower=0> Count_cens[N_cens];      // Array of censoring points for censored values
  real logT10_cens[N_cens];      // Array of censoring points for censored values
  int<lower=0> MaxN_cens[N_cens]; // Sentence lengths of censored values
  int<lower=0> MaxN_score[N_score]; //Sentence lengths for scoring passages
  real a_cens[N_cens];
  real b_cens[N_cens];
  real alpha_cens[N_cens];
  real beta_cens[N_cens];
  real a_score[N_score]; // All _score params are for sentences of scoring passages
  real b_score[N_score];
  real alpha_score[N_score];
  real beta_score[N_score];
  real<lower=0> gamma1;
  real<lower=0> gamma2;
  real<lower=0> sigma;
  real<lower=-1,upper=1> rho;
  real<lower=-1,upper=1> rhoTestlet;
}
parameters {
  real Z1;
  real Z2;
  real V1[K];
  real V2[K];
}

transformed parameters {
  real theta1;
  real theta2;
  real U1[K];
  real U2[K];
  
  theta1 = Z1;
  theta2 = rho*Z1 + sqrt(1-rho^2)*Z2;
  for (k in 1:K) {
    U1[k] = V1[k];
    U2[k] = rhoTestlet*V1[k] + sqrt(1-rhoTestlet^2)*V2[k];
  }
}
model {
  
  Z1 ~ normal(0, 1);
  Z2 ~ normal(0, 1);
  for (k in 1:K) {
    V1[k] ~ normal(0, 1);
    V2[k] ~ normal(0, 1);
  }
  
  // Likelihood for censored values - binomial distribution
  for (i in 1:N_cens) {
    real p_cens;
    real mu_cens;
    real logLikelihoodCount;
    real logLikelihoodLogT10;
    
    // Calculate the probability of observing Count_cens[i]
    p_cens = Phi(fmax(-5, fmin(5, a_cens[i] * (theta1 + gamma1 * U1[Passage_cens[i]]) / sqrt(1 + gamma1^2) - b_cens[i])));
    mu_cens = beta_cens[i]-sigma*(theta2+gamma2*U2[Passage_cens[i]])/sqrt(1+gamma2^2);
    
    // Calculate the log-likelihood for Count_cens[i]
    logLikelihoodCount = binomial_lccdf(Count_cens[i]-1 | MaxN_cens[i], p_cens);
    
    // Calculate the log-likelihood for logT10_cens[i]
    logLikelihoodLogT10 = normal_lccdf(logT10_cens[i] | mu_cens, 1 / alpha_cens[i]);
    
    // Add the log-likelihood to the target
    target += logLikelihoodCount + logLikelihoodLogT10;
  }
  
}

// The part below will estimate model-based wrc, seconds, and WCPM from internal or external passages!
// Note the selection of internal vs external is handled with prior functions that would call this stan syntax!

generated quantities{
  real tim_ex[N_score]; //Expeced time matrix
  real <lower=0> cnt_ex[N_score]; //Expected count matrix
  real <lower=0> exp_cnt; //Model-based obs.counts
  real <lower=0> exp_tim; //Model-based secs
  real <lower=0> wcpm; //Model-based WCPM
  
  real V1_score[K_score]; 
  real V2_score[K_score]; 
  real U1_score[K_score];
  real U2_score[K_score];
  
  for (k in 1:K_score){
        V1_score[k] = normal_rng(0, 1);
        V2_score[k] = normal_rng(0, 1);
        U1_score[k] = V1_score[k];
        U2_score[k] = rhoTestlet*V1_score[k] + sqrt(1-rhoTestlet^2)*V2_score[k];
  }

 for(i in 1:N_score){
    real p_score; real mu_score;
    p_score = Phi(fmax(-5, fmin(5, a_score[i] * (theta1 + gamma1 * U1_score[Passage_score[i]]) / sqrt(1 + gamma1^2) - b_score[i])));
    mu_score = beta_score[i]-sigma*(theta2+gamma2*U2_score[Passage_score[i]])/sqrt(1+gamma2^2);
    cnt_ex[i] = p_score * MaxN_score[i];
    tim_ex[i]=(exp(mu_score + log(MaxN_score[i]) -log(10) + 0.5 * 1/(square(alpha_score[i]))));
  }
    exp_cnt=sum(cnt_ex);
    exp_tim=sum(tim_ex);
    wcpm=exp_cnt/exp_tim*60;
}

"
# Scenario 6: One Observed and One Censored Sentence
testlet_scoring_one_obs_one_cens_sentence <- "
data {
  int<lower=0> N_obs;             // Number of observed values
  int<lower=0> N_cens;            // Number of censored values
  int<lower=0> N_score;           // Number of sentences for scoring
  int<lower=0> K;                 // Number of passages with data
  int<lower=0> K_score;           // Number of passages for scoring
  int<lower=0> Passage_obs;       // Passage index of observed value
  int<lower=0> Passage_cens; // Passage indices of censored values
  int<lower=0> Passage_score[N_score]; //Passage indicators for scoring
  int<lower=0> Count_obs;         // Observed value
  int<lower=0> Count_cens; // Array of censoring points for censored values
  real logT10_obs;               // Observed value
  real logT10_cens;      // Array of censoring points for censored values
  int<lower=0> MaxN_obs;         // Sentence length of observed value
  int<lower=0> MaxN_cens; // Sentence lengths of censored values
  int<lower=0> MaxN_score[N_score]; //Sentence lengths for scoring passages
  real a_obs;
  real a_cens;
  real b_obs;
  real b_cens;
  real alpha_obs;
  real alpha_cens;
  real beta_obs;
  real beta_cens;
  real a_score[N_score]; // All _score params are for sentences of scoring passages
  real b_score[N_score];
  real alpha_score[N_score];
  real beta_score[N_score];
  real<lower=0> gamma1;
  real<lower=0> gamma2;
  real<lower=0> sigma;
  real<lower=-1,upper=1> rho;
  real<lower=-1,upper=1> rhoTestlet;
}
parameters {
  real Z1;
  real Z2;
  real V1[K];
  real V2[K];
}

transformed parameters {
  real theta1;
  real theta2;
  real U1[K];
  real U2[K];
  
  theta1 = Z1;
  theta2 = rho*Z1 + sqrt(1-rho^2)*Z2;
  for (k in 1:K) {
    U1[k] = V1[k];
    U2[k] = rhoTestlet*V1[k] + sqrt(1-rhoTestlet^2)*V2[k];
  }
}
model {

  real p_cens;
  real mu_cens;
  real p_obs;
  real mu_obs;
  
  real logLikelihoodCount;
  real logLikelihoodLogT10;

  Z1 ~ normal(0, 1);
  Z2 ~ normal(0, 1);
  for (k in 1:K) {
    V1[k] ~ normal(0, 1);
    V2[k] ~ normal(0, 1);
  }

  // Likelihood for the observed value - binomial and normal distribution

  p_obs = Phi(fmax(-5, fmin(5, a_obs * (theta1 + gamma1 * U1[Passage_obs]) / sqrt(1 + gamma1^2) - b_obs)));
  mu_obs = beta_obs - sigma * (theta2 + gamma2 * U2[Passage_obs]) / sqrt(1 + gamma2^2);

  Count_obs ~ binomial(MaxN_obs, p_obs);
  logT10_obs ~ normal(mu_obs, 1 / alpha_obs);
  
  // Likelihood for censored value - binomial distribution

  // Calculate the probability of observing Count_cens
  p_cens = Phi(fmax(-5, fmin(5, a_cens * (theta1 + gamma1 * U1[Passage_cens]) / sqrt(1 + gamma1^2) - b_cens)));
  mu_cens = beta_cens-sigma*(theta2+gamma2*U2[Passage_cens])/sqrt(1+gamma2^2);

  // Calculate the log-likelihood for Count_cens
  logLikelihoodCount = binomial_lccdf(Count_cens-1 | MaxN_cens, p_cens);

  // Calculate the log-likelihood for logT10_cens
  logLikelihoodLogT10 = normal_lccdf(logT10_cens | mu_cens, 1 / alpha_cens);

  // Add the log-likelihood to the target
  target += logLikelihoodCount + logLikelihoodLogT10;

}

// The part below will estimate model-based wrc, seconds, and WCPM from internal or external passages!
// Note the selection of internal vs external is handled with prior functions that would call this stan syntax!

generated quantities{
  real tim_ex[N_score]; //Expeced time matrix
  real <lower=0> cnt_ex[N_score]; //Expected count matrix
  real <lower=0> exp_cnt; //Model-based obs.counts
  real <lower=0> exp_tim; //Model-based secs
  real <lower=0> wcpm; //Model-based WCPM
  
  real V1_score[K_score]; 
  real V2_score[K_score]; 
  real U1_score[K_score];
  real U2_score[K_score];
  
  for (k in 1:K_score){
        V1_score[k] = normal_rng(0, 1);
        V2_score[k] = normal_rng(0, 1);
        U1_score[k] = V1_score[k];
        U2_score[k] = rhoTestlet*V1_score[k] + sqrt(1-rhoTestlet^2)*V2_score[k];
  }

 for(i in 1:N_score){
    real p_score; real mu_score;
    p_score = Phi(fmax(-5, fmin(5, a_score[i] * (theta1 + gamma1 * U1_score[Passage_score[i]]) / sqrt(1 + gamma1^2) - b_score[i])));
    mu_score = beta_score[i]-sigma*(theta2+gamma2*U2_score[Passage_score[i]])/sqrt(1+gamma2^2);
    cnt_ex[i] = p_score * MaxN_score[i];
    tim_ex[i]=(exp(mu_score + log(MaxN_score[i]) -log(10) + 0.5 * 1/(square(alpha_score[i]))));
  }
    exp_cnt=sum(cnt_ex);
    exp_tim=sum(tim_ex);
    wcpm=exp_cnt/exp_tim*60;
}

"
# Define Stan models
model_multi_obs_multi_cens_sentence <- rstan::stan_model(model_code = testlet_scoring_multi_obs_multi_cens_sentence)
model_multi_obs_one_cens_sentence <- rstan::stan_model(model_code = testlet_scoring_multi_obs_one_cens_sentence)
model_one_obs_multi_cens_sentence <- rstan::stan_model(model_code = testlet_scoring_one_obs_multi_cens_sentence)
model_multi_obs_no_cens_sentence <- rstan::stan_model(model_code = testlet_scoring_multi_obs_no_cens_sentence)
model_no_obs_multi_cens_sentence <- rstan::stan_model(model_code = testlet_scoring_no_obs_multi_cens_sentence)
model_one_obs_one_cens_sentence <- rstan::stan_model(model_code = testlet_scoring_one_obs_one_cens_sentence)
