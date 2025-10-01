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
#' censored (1) or fully observed (0) -- K-dim
#' 
#' @return  list
scoring.sentence.censoring <- function(Count=NULL, 
                                       logT10=NULL, 
                                       N=NULL, 
                                       N_score=NULL,
                                       Passage=NULL, 
                                       Passage_score=NULL, 
                                       a=NULL,
                                       a_score=NULL,
                                       b=NULL,
                                       b_score=NULL,
                                       alpha=NULL,
                                       alpha_score=NULL,
                                       beta=NULL,
                                       beta_score=NULL,
                                       gamma1=NULL, 
                                       gamma2=NULL,
                                       sigma=NULL, 
                                       rho=NULL, 
                                       rhoTestlet=NULL, 
                                       C=NULL, 
                                       type=NULL) {
  
  # loading logger
  log.initiating()
  
  flog.info("Begin scoring sentence censoring process", name = "orfrlog")
  
  suppress_output <- function(expr) {
    sink(nullfile())
    expr
    sink()
  }
  
  Y <- data.matrix(Count) # e.g., [r:58,c:23], r->person, c->sentence
  logT10 <- data.matrix(logT10) # [r,c]
  Cens <- data.matrix(C)
  
  # Get orders & sequence for passages READ
  unique_id <- Passage %>% unique()
  passage_seq <- Passage
  
  for (i in 1:length(unique_id)) {
    passage_seq[passage_seq == unique_id[i]] <- i
  }
  
  # Get orders & sequence for passages to be SCORED
  unique_id_score <- Passage_score %>% unique()
  passage_seq_score <- Passage_score
  
  for (i in 1:length(unique_id_score)) {
    passage_seq_score[passage_seq_score == unique_id_score[i]] <- i
  }
  
  
  # person number
  n = dim(Cens)[[1]]
  
  theta_acc_est <-rep(0,n)
  theta_spd_est <-rep(0,n)
  theta_acc_sd <-rep(0,n)
  theta_spd_sd <-rep(0,n)
  count_est <-rep(0,n)
  time_est <-rep(0,n)
  wcpm_est <-rep(0,n)
  count_sd <-rep(0,n)
  time_sd <-rep(0,n)
  wcpm_sd <-rep(0,n)
  
  for (k in 1:n) {
    
    # for debug
    # print(paste(" k= ", k))
    
    #each k slices the kth row of the examinee x sentence data!
    data_list <- create_data_list_sentence(Count=Y[k,], 
                                           logT10=logT10[k,], 
                                           MaxN=N,
                                           MaxN_score=N_score,
                                           Passage=passage_seq,
                                           Passage_score=passage_seq_score,
                                           a=a, 
                                           b=b, 
                                           alpha=alpha, 
                                           beta=beta,
                                           a_score=a_score,
                                           b_score=b_score, 
                                           alpha_score=alpha_score,
                                           beta_score=beta_score,
                                           gamma1=gamma1, 
                                           gamma2=gamma2,
                                           sigma=sigma, 
                                           rho=rho, 
                                           rhoTestlet=rhoTestlet,
                                           C=Cens[k,])
  
    suppress_output({
      score_object <- score_testlet_sentence(data_list)
    })
    
    theta_acc_est[k] <- score_object$theta_acc
    theta_spd_est[k] <- score_object$theta_spd
    theta_acc_sd[k] <- score_object$theta_acc_sd
    theta_spd_sd[k] <- score_object$theta_spd_sd
    time_est[k] <- score_object$time
    count_est[k] <- score_object$count
    wcpm_est[k] <- score_object$wcpm
    time_sd[k] <- score_object$time_sd
    count_sd[k] <- score_object$count_sd
    wcpm_sd[k] <- score_object$wcpm_sd
      
  }
  
  flog.info("End scoring sentence censoring process", name = "orfrlog")
  
  if(type=="general"){
  return(list(theta_acc_est=theta_acc_est,
              theta_spd_est=theta_spd_est,
              theta_acc_sd=theta_acc_sd,
              theta_spd_sd=theta_spd_sd, 
              count_est=count_est, 
              time_est=time_est,
              count_sd=count_sd, 
              time_sd=time_sd))
  }else if(type=="orf"){
    return(list(theta_acc_est=theta_acc_est,
                theta_spd_est=theta_spd_est,
                theta_acc_sd=theta_acc_sd,
                theta_spd_sd=theta_spd_sd, 
                count_est=count_est, 
                time_est=time_est,
                count_sd=count_sd, 
                time_sd=time_sd, 
                wcpm_est=wcpm_est, 
                wcpm_sd=wcpm_sd))
  }
}
