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
#' censored (0) or fully observed (1) -- K-dim
#'  
create_data_list <- function(Count, logT10, MaxN, 
                             a, b, alpha,beta, 
                             sigma, rho, C) {
  # Calculate the number of observed and censored passages
  N_obs <- sum(C == 1)
  N_cens <- sum(C == 0)
  
  # Create a list containing subsets of input data
  data_list <- list(
    N_obs = N_obs,
    N_cens = N_cens,
    Count_obs = Count[C == 1],
    Count_cens = Count[C == 0],
    logT10_obs = logT10[C == 1],
    logT10_cens = logT10[C == 0],
    MaxN_obs = MaxN[C == 1],
    MaxN_cens = MaxN[C == 0],
    a_obs = a[C == 1],
    a_cens = a[C == 0],
    b_obs = b[C == 1],
    b_cens = b[C == 0],
    alpha_obs = alpha[C == 1],
    alpha_cens = alpha[C == 0],
    beta_obs = beta[C == 1],
    beta_cens = beta[C == 0],
    sigma = sigma,
    rho = rho
  )
  
  return(data_list)
}

score_testlet <- function(data_list) {
  # Performs Bayesian scoring for a single student
  # who has read K >= 2 passages in the calibrated ORF instrument
  # Enter processed passage-level (no testlet) data with censoring indicator
  
  # Extract number of censored and fully observed passages
  C0 <- data_list$N_cens
  C1 <- data_list$N_obs
  
  # Below if statements determine which version of the Stan model to use
  if (C0 >= 2 && C1 >= 2) {
    fit <- sampling(model_multi_obs_multi_cens, 
                    data = data_list, chains = 4, 
                    iter = 2000, warmup = 1000)
    summary_info <- summary(fit)
    # Extract posterior means
    posterior_means <- summary_info$summary[, "mean"]
    # Extract the means for theta1 and theta2
    theta1_mean <- posterior_means["theta1"]
    theta2_mean <- posterior_means["theta2"]
    # Extract the sd's for theta1 and theta2
    posterior_sd <- summary_info$summary[, "sd"]
    theta1_sd <- posterior_sd["theta1"]
    theta2_sd <- posterior_sd["theta2"]
  } else if (C0 >= 2 && C1 == 1) {
    fit <- sampling(model_one_obs_multi_cens, 
                    data = data_list, chains = 4, 
                    iter = 2000, warmup = 1000)
    summary_info <- summary(fit)
    # Extract posterior means
    posterior_means <- summary_info$summary[, "mean"]
    # Extract the means for theta1 and theta2
    theta1_mean <- posterior_means["theta1"]
    theta2_mean <- posterior_means["theta2"]
    # Extract the sd's for theta1 and theta2
    posterior_sd <- summary_info$summary[, "sd"]
    theta1_sd <- posterior_sd["theta1"]
    theta2_sd <- posterior_sd["theta2"]
  } else if (C0 >= 2 && C1 == 0) {
    fit <- sampling(model_no_obs_multi_cens, 
                    data = data_list, chains = 4, 
                    iter = 2000, warmup = 1000)
    summary_info <- summary(fit)
    # Extract posterior means
    posterior_means <- summary_info$summary[, "mean"]
    # Extract the means for theta1 and theta2
    theta1_mean <- posterior_means["theta1"]
    theta2_mean <- posterior_means["theta2"]
    # Extract the sd's for theta1 and theta2
    posterior_sd <- summary_info$summary[, "sd"]
    theta1_sd <- posterior_sd["theta1"]
    theta2_sd <- posterior_sd["theta2"]
  } else if (C0 == 1 && C1 >= 2) {
    fit <- sampling(model_multi_obs_one_cens, 
                    data = data_list, chains = 4, 
                    iter = 2000, warmup = 1000)
    summary_info <- summary(fit)
    # Extract posterior means
    posterior_means <- summary_info$summary[, "mean"]
    # Extract the means for theta1 and theta2
    theta1_mean <- posterior_means["theta1"]
    theta2_mean <- posterior_means["theta2"]
    # Extract the sd's for theta1 and theta2
    posterior_sd <- summary_info$summary[, "sd"]
    theta1_sd <- posterior_sd["theta1"]
    theta2_sd <- posterior_sd["theta2"]
  } else if (C0 == 0 && C1 >= 2) {
    fit <- sampling(model_multi_obs_no_cens, 
                    data = data_list, chains = 4, 
                    iter = 2000, warmup = 1000)
    summary_info <- summary(fit)
    # Extract posterior means
    posterior_means <- summary_info$summary[, "mean"]
    # Extract the means for theta1 and theta2
    theta1_mean <- posterior_means["theta1"]
    theta2_mean <- posterior_means["theta2"]
    # Extract the sd's for theta1 and theta2
    posterior_sd <- summary_info$summary[, "sd"]
    theta1_sd <- posterior_sd["theta1"]
    theta2_sd <- posterior_sd["theta2"]
  } else if (C0 == 1 && C1 == 1) {
    fit <- sampling(model_one_obs_one_cens, 
                    data = data_list, chains = 4, 
                    iter = 2000, warmup = 1000)
    summary_info <- summary(fit)
    # Extract posterior means
    posterior_means <- summary_info$summary[, "mean"]
    # Extract the means for theta1 and theta2
    theta1_mean <- posterior_means["theta1"]
    theta2_mean <- posterior_means["theta2"]
    # Extract the sd's for theta1 and theta2
    posterior_sd <- summary_info$summary[, "sd"]
    theta1_sd <- posterior_sd["theta1"]
    theta2_sd <- posterior_sd["theta2"]
  }
  
  # Returns estimated posterior means and standard deviations
  # for theta1 (latent reading trait) and theta2 (latent speed trait)
  return(list(theta_acc = as.numeric(theta1_mean),
              theta_spd = as.numeric(theta2_mean),
              theta_acc_sd = as.numeric(theta1_sd),
              theta_spd_sd = as.numeric(theta2_sd)))
  
}
