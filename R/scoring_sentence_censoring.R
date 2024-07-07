#' scoring.sentence.censoring function 
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
#' censored (0) or fully observed (1) -- K-dim
#' 
#' @return  list
#' @export
scoring.sentence.censoring <- function(Count=NULL, logT10=NULL, N=NULL, 
                                        Passage=NULL, a=NULL, b=NULL, alpha=NULL,beta=NULL, 
                                        gamma1=NULL, gamma2=NULL,
                                        sigma=NULL, rho=NULL, rhoTestlet=NULL, C=NULL) {
  
  # loading logger
  log.initiating()
  
  flog.info("Begin scoring sentence censoring process", name = "orfrlog")
  
  suppress_output <- function(expr) {
    sink(nullfile())
    expr
    sink()
  }
  
  Y <- data.matrix(Count) # [r:58,c:23], r->person, c->sentence
  logT10 <- data.matrix(logT10) # [r,c]
  Cens <- data.matrix(C)
  
  # get Passage orders
  unique_id <- Passage %>% unique()
  
  # Get passage sequence
  passage_seq <- Passage
  
  for (i in 1:length(unique_id)) {
    passage_seq[passage_seq == unique_id[i]] <- i
  }
  
  # person number
  n = dim(Cens)[[1]]
  
  theta_acc_est <-rep(0,n)
  theta_spd_est <-rep(0,n)
  theta_acc_sd <-rep(0,n)
  theta_spd_sd <-rep(0,n)
  
  for (k in 1:n) {
    
    # for debug
    # print(paste(" k= ", k))
    
    data_list <- create_data_list_sentence(Y[k,], logT10[k,], N, 
                                            passage_seq, a, b, 
                                            alpha, beta, 
                                            gamma1, gamma2,
                                            sigma, rho, rhoTestlet,
                                            Cens[k,])
  
    suppress_output({
      score_object <- score_testlet_sentence(data_list)
    })
    
    theta_acc_est[k] <- score_object$theta_acc
    theta_spd_est[k] <- score_object$theta_spd
    theta_acc_sd[k] <- score_object$theta_acc_sd
    theta_spd_sd[k] <- score_object$theta_spd_sd
      
  }
  
  flog.info("End scoring sentence censoring process", name = "orfrlog")
  
  return(list(theta_acc_est=theta_acc_est,
              theta_spd_est=theta_spd_est,
              theta_acc_sd=theta_acc_sd,
              theta_spd_sd=theta_spd_sd))
}
