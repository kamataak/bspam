#' Call by scoring.sentence.censoring function 
#'
#'
#' Copyright (C) 2021-2024 The ORF Project Team
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
#' 
#' A copy of the GNU General Public License is available at
#' http://www.gnu.org/licenses/
#'
#'
#' Pre-processing data: Function prepares data for Stan model
#' Enter a single student's data for preparation
#' 
#' Assume a student has read K >= 2 passages
#' 
#' @param Count: A vector with the number of words correct per passage
#' It should be K-dimensional
#' @param logT10: The log-scale reading time per 10 words per passage
#' It should be K-dimensional
#' @param N: A vector of passage lengths
#' It should be K-dimensional
#' @param Passage: Task ids 
#' @param a, b: Model parameters related to the count data model (K-dim)
#' @param alpha, beta: Model parameters related to the time data model (K-dim)
#' @param gamma1, gamma2: Hyperparameters related to the testlet model
#' @param sigma: The latent standard deviation of the time latent component
#' @param rho: The correlation between count and time latent components
#' @param rhoTestlet: The testlet-based reliability  
#' @param C: A vector of indicators whether a specific sentence was 
#' censored (1) or fully observed (0) -- K-dim
#' 
#' @return  list
create_data_list_sentence <- function(Count, logT10, MaxN, MaxN_score,
                             Passage, Passage_score, a, b, 
                             alpha, beta,
                             a_score, b_score, alpha_score, beta_score,
                             gamma1, gamma2, 
                             sigma, rho, rhoTestlet, 
                             C) {
  
  N_obs <- sum(C == 0, na.rm = TRUE)
  N_cens <- sum(C == 1, na.rm = TRUE)
  
  # Ensure that even single-row outputs are treated as vectors
  safe_na_omit <- function(x) {
    result <- na.omit(x)
    # print(result)
    if (length(result) > 0) {
      if (!is.null(dim(result)) && dim(result)[1] == 1) {
        # If result is a 1-row matrix or data frame, convert to vector
        return(as.vector(unlist(result)))
      } else {
        # If result has more than 1 row, return as is
        return(result)  
      }
    } else {
      # If result is empty, return it
      return(result)  
    }
    # if (length(result) > 0) {
    #   if(dim(result)[1] == 1) {
    #     return(as.vector(unlist(result)))
    #   } else {
    #     return(result)  
    #   }
    # } else {
    #   return(result)  
    # }
  }
  data_list <- list(
    N_obs = N_obs,
    N_cens = N_cens,
    N_score = length(MaxN_score),
    K = max(Passage),
    K_score = max(Passage_score),
    Passage_obs = safe_na_omit(Passage[C == 0]),
    Passage_cens = safe_na_omit(Passage[C == 1]),
    Passage_score = Passage_score, 
    Count_obs = na.omit(Count[C == 0]),
    Count_cens = na.omit(Count[C == 1]),
    logT10_obs = na.omit(logT10[C == 0]),
    logT10_cens = na.omit(logT10[C == 1]),
    MaxN_obs = safe_na_omit(MaxN[C == 0]),
    MaxN_cens = safe_na_omit(MaxN[C == 1]),
    MaxN_score = MaxN_score,
    #Note: for the parameters of the passages to be scored, does it matter to account for censoring? I don't think soo. but just wanted to ask.
    #Note: the stan code for the testlet model does not use the _cens params! No need to worry about those. 
    a_obs = na.omit(a[C == 0]),
    a_cens = na.omit(a[C == 1]),
    a_score=a_score,
    b_obs = na.omit(b[C == 0]),
    b_cens = na.omit(b[C == 1]),
    b_score=b_score,
    alpha_obs = na.omit(alpha[C == 0]),
    alpha_cens = na.omit(alpha[C == 1]),
    alpha_score=alpha_score,
    beta_obs = na.omit(beta[C == 0]),
    beta_cens = na.omit(beta[C == 1]),
    beta_score=beta_score,
    gamma1 = gamma1,
    gamma2 = gamma2,
    sigma = sigma,
    rho = rho,
    rhoTestlet = rhoTestlet
  )
  
  # print(data_list)
  return(data_list)
}

# Performs EAP scoring for a single student
# Testlet model with/without censoring
score_testlet_sentence <- function(data_list) {
  C1 <- data_list$N_cens
  C0 <- data_list$N_obs
  
  if (C0 >= 2 && C1 >= 2) {
    fit <- rstan::sampling(model_multi_obs_multi_cens_sentence, 
                    data = data_list, chains = 4, 
                    iter = 2000, warmup = 1000)
    summary_info <- rstan::summary(fit)
    # Extract posterior means
    posterior_means <- summary_info$summary[, "mean"]
    # Extract the means for theta1 and theta2
    theta1_mean <- posterior_means["theta1"]
    theta2_mean <- posterior_means["theta2"]
    count_mean <- posterior_means["exp_cnt"]
    time_mean <- posterior_means["exp_tim"]
    wcpm_mean <- posterior_means["wcpm"]
    
    # Extract the sd's for theta1 and theta2
    posterior_sd <- summary_info$summary[, "sd"]
    theta1_sd <- posterior_sd["theta1"]
    theta2_sd <- posterior_sd["theta2"]
    count_sd <- posterior_sd["exp_cnt"]
    time_sd <- posterior_sd["exp_tim"]
    wcpm_sd <- posterior_sd["wcpm"]
    
  } else if (C0 >= 2 && C1 == 1) { # C1 is cens, C0 is obs
    fit <- rstan::sampling(model_multi_obs_one_cens_sentence, #model_one_obs_multi_cens_sentence, 
                    data = data_list, chains = 4, 
                    iter = 2000, warmup = 1000)
    summary_info <- rstan::summary(fit)
    # Extract posterior means
    posterior_means <- summary_info$summary[, "mean"]
    # Extract the means for theta1 and theta2
    theta1_mean <- posterior_means["theta1"]
    theta2_mean <- posterior_means["theta2"]
    count_mean <- posterior_means["exp_cnt"]
    time_mean <- posterior_means["exp_tim"]
    wcpm_mean <- posterior_means["wcpm"]
    # Extract the sd's for theta1 and theta2
    posterior_sd <- summary_info$summary[, "sd"]
    theta1_sd <- posterior_sd["theta1"]
    theta2_sd <- posterior_sd["theta2"]
    count_sd <- posterior_sd["exp_cnt"]
    time_sd <- posterior_sd["exp_tim"]
    wcpm_sd <- posterior_sd["wcpm"]
  } else if (C0 >= 2 && C1 == 0) { ## -----> This is the testlet model!!!
    fit <- rstan::sampling(model_multi_obs_no_cens_sentence, #model_no_obs_multi_cens_sentence, 
                    data = data_list, chains = 4, 
                    iter = 2000, warmup = 1000)
    summary_info <- rstan::summary(fit)
    # Extract posterior means
    posterior_means <- summary_info$summary[, "mean"]
    # Extract the means for theta1 and theta2 + scoring params
    theta1_mean <- posterior_means["theta1"]
    theta2_mean <- posterior_means["theta2"]
    count_mean <- posterior_means["exp_cnt"]
    time_mean <- posterior_means["exp_tim"]
    wcpm_mean <- posterior_means["wcpm"]
    
    # Extract the sd's for theta1 and theta2 + scoring params
    posterior_sd <- summary_info$summary[, "sd"]
    theta1_sd <- posterior_sd["theta1"]
    theta2_sd <- posterior_sd["theta2"]
    count_sd <- posterior_sd["exp_cnt"]
    time_sd <- posterior_sd["exp_tim"]
    wcpm_sd <- posterior_sd["wcpm"]
    
  } else if (C0 == 1 && C1 >= 2) { # C1 is cens, C0 is obs
    fit <- rstan::sampling(model_one_obs_multi_cens_sentence, #model_multi_obs_one_cens_sentence, 
                    data = data_list, chains = 4, 
                    iter = 2000, warmup = 1000)
    summary_info <- rstan::summary(fit)
    # Extract posterior means
    posterior_means <- summary_info$summary[, "mean"]
    # Extract the means for theta1 and theta2
    theta1_mean <- posterior_means["theta1"]
    theta2_mean <- posterior_means["theta2"]
    count_mean <- posterior_means["exp_cnt"]
    time_mean <- posterior_means["exp_tim"]
    wcpm_mean <- posterior_means["wcpm"]
    # Extract the sd's for theta1 and theta2
    posterior_sd <- summary_info$summary[, "sd"]
    theta1_sd <- posterior_sd["theta1"]
    theta2_sd <- posterior_sd["theta2"]
    count_sd <- posterior_sd["exp_cnt"]
    time_sd <- posterior_sd["exp_tim"]
    wcpm_sd <- posterior_sd["wcpm"]
  } else if (C0 == 0 && C1 >= 2) {
    fit <- rstan::sampling(model_no_obs_multi_cens_sentence, #model_multi_obs_no_cens_sentence, 
                    data = data_list, chains = 4, 
                    iter = 2000, warmup = 1000)
    summary_info <- rstan::summary(fit)
    # Extract posterior means
    posterior_means <- summary_info$summary[, "mean"]
    # Extract the means for theta1 and theta2
    theta1_mean <- posterior_means["theta1"]
    theta2_mean <- posterior_means["theta2"]
    count_mean <- posterior_means["exp_cnt"]
    time_mean <- posterior_means["exp_tim"]
    wcpm_mean <- posterior_means["wcpm"]
    # Extract the sd's for theta1 and theta2
    posterior_sd <- summary_info$summary[, "sd"]
    theta1_sd <- posterior_sd["theta1"]
    theta2_sd <- posterior_sd["theta2"]
    count_sd <- posterior_sd["exp_cnt"]
    time_sd <- posterior_sd["exp_tim"]
    wcpm_sd <- posterior_sd["wcpm"]
  } else if (C0 == 1 && C1 == 1) {
    fit <- rstan::sampling(model_one_obs_one_cens_sentence, 
                           data = data_list, chains = 4, 
                           iter = 2000, warmup = 1000)
    summary_info <- rstan::summary(fit)
    # Extract posterior means
    posterior_means <- summary_info$summary[, "mean"]
    # Extract the means for theta1 and theta2
    theta1_mean <- posterior_means["theta1"]
    theta2_mean <- posterior_means["theta2"]
    count_mean <- posterior_means["exp_cnt"]
    time_mean <- posterior_means["exp_tim"]
    wcpm_mean <- posterior_means["wcpm"]
    # Extract the sd's for theta1 and theta2
    posterior_sd <- summary_info$summary[, "sd"]
    theta1_sd <- posterior_sd["theta1"]
    theta2_sd <- posterior_sd["theta2"]
    count_sd <- posterior_sd["exp_cnt"]
    time_sd <- posterior_sd["exp_tim"]
    wcpm_sd <- posterior_sd["wcpm"]
    #Stop the analyses in case of cases with excessive missingness (NAs not censored sentence observations!)
  }else{
    stop("Error: Your data contain cases with excessive missingness. Consider discarding these cases and rerun the analyses.")
  }
  
  return(list(theta_acc = as.numeric(theta1_mean),
              theta_spd = as.numeric(theta2_mean),
              theta_acc_sd = as.numeric(theta1_sd),
              theta_spd_sd = as.numeric(theta2_sd), 
              count=as.numeric(count_mean), 
              time=as.numeric(time_mean), 
              wcpm=as.numeric(wcpm_mean), 
              count_sd=as.numeric(count_sd),
              time_sd=as.numeric(time_sd),
              wcpm_sd=as.numeric(wcpm_sd))
         )
  
}
