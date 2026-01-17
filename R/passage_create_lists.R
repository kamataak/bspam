#' Scoring.Passage function 
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
#' @param MaxN: A vector of passage lengths
#' It should be K-dimensional
#' @param a, b: Model parameters related to the count data model (K-dim)
#' @param alpha, beta: Model parameters related to the time data model (K-dim)
#' @param sigma: The latent standard deviation of the time latent component
#' @param rho: The correlation between count and time latent components
#' @param C: A vector of indicators whether a specific passage was 
#' censored (1) or fully observed (0) -- K-dim
#'  
create_data_list <- function(Count, logT10, MaxN, MaxN_score,
                             a, b, alpha,beta,
                             a_score, b_score, alpha_score, beta_score,
                             sigma, rho, C) {
  # Calculate the number of observed and censored passages
  N_obs <- sum(C == 0, na.rm = TRUE) # try na.rm = T, 04/07/24 by Kuo
  N_cens <- sum(C == 1, na.rm = TRUE) # try na.rm = T, 04/07/24 by Kuo
  
  #Attach dim spec to prevent stan issues when only 1 task is used for scoring.
  N_score = length(MaxN_score)
  dim(MaxN_score) <- N_score
  
  dim(a_score) <- N_score
  dim(b_score) <- N_score
  dim(alpha_score) <- N_score
  dim(beta_score) <- N_score
  
  # Create a list containing subsets of input data
  data_list <- list(
    N_obs = N_obs,
    N_cens = N_cens,
    N_score = N_score,
    Count_obs = na.omit(Count[C == 0]),
    Count_cens = na.omit(Count[C == 1]),
    logT10_obs = na.omit(logT10[C == 0]),
    logT10_cens = na.omit(logT10[C == 1]),
    MaxN_obs = na.omit(MaxN[C == 0]),
    MaxN_cens = na.omit(MaxN[C == 1]),
    MaxN_score = MaxN_score,
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
    sigma = sigma,
    rho = rho
  )
  
  # print(data_list)
  return(data_list)
}

score_testlet <- function(data_list) {
  # Performs Bayesian scoring for a single student
  # who has read K >= 2 passages in the calibrated ORF instrument
  # Enter processed passage-level (no testlet) data with censoring indicator
  
  # Extract number of censored and fully observed passages
  C1 <- data_list$N_cens
  C0 <- data_list$N_obs
  
  # Below if statements determine which version of the Stan model to use
  if (C0 >= 2 && C1 >= 2) { # C0 is obs, C1 is cens
    fit <- rstan::sampling(model_multi_obs_multi_cens, 
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
    
    
  } else if (C0 >= 2 && C1 == 1) { # C0 is obs, C1 is cens
    fit <- rstan::sampling(model_multi_obs_one_cens, # model_one_obs_multi_cens, 
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
    
  } else if (C0 >= 2 && C1 == 0) { # C0 is obs, C1 is cens 
    fit <- rstan::sampling(model_multi_obs_no_cens, #model_no_obs_multi_cens, 
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
    
    } else if (C0 == 1 && C1 >= 2) { # C0 is obs, C1 is cens
    fit <- rstan::sampling(model_one_obs_multi_cens, #model_multi_obs_one_cens, 
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
    
  } else if (C0 == 0 && C1 >= 2) { # C0 is obs, C1 is cens
    fit <- rstan::sampling(model_no_obs_multi_cens, #model_multi_obs_no_cens, 
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
    
  } else if (C0 == 1 && C1 == 1) { # C0 is obs, C1 is cens
    fit <- rstan::sampling(model_one_obs_one_cens, 
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
  }else{
    stop("Error: Your data contain cases with less than two task-level observations, possibly, due to missingness. Consider discarding these cases and rerun the analyses.")
  }
  
  # Returns estimated posterior means and standard deviations
  # for theta1 (latent reading trait) and theta2 (latent speed trait)
  return(list(theta_acc = as.numeric(theta1_mean),
              theta_spd = as.numeric(theta2_mean),
              theta_acc_sd = as.numeric(theta1_sd),
              theta_spd_sd = as.numeric(theta2_sd), 
              count=as.numeric(count_mean), 
              time=as.numeric(time_mean), 
              wcpm=as.numeric(wcpm_mean), 
              count_sd=as.numeric(count_sd),
              time_sd=as.numeric(time_sd),
              wcpm_sd=as.numeric(wcpm_sd)))
  
}
