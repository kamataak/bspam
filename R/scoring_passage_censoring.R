# scoring.passage function 
#
#
# Copyright (C) 2021-2026 The ORF Project Team
# 
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# A copy of the GNU General Public License is available at
# http://www.gnu.org/licenses/
#
#
# Pre-processing data: Function prepares data for Stan model
# Enter a single student's data for preparation
# 
# Assume a student has read K >= 2 passages
# 
# @param Count A vector of observed count outcomes for the passages used for scoring the person.
# @param logT10 A vector of log-transformed reading times per 10 words for the passages used for scoring the person.
# @param N A vector containing the maximum possible counts, such as passage lengths, for the observed passages.
# @param N_score A vector containing the maximum possible counts, such as passage lengths, for the passages to be scored.
# @param a A Model parameters related to the count model for the observed passages.
# @param b A Model parameters related to the count model for the observed passages.
# @param alpha A Model parameters related to the time model for the observed passages.
# @param beta A Model parameters related to the time model for the observed passages.
# @param a_score A Model parameters related to the count model for the passages to be scored.
# @param b_score A Model parameters related to the count model for the passages to be scored.
# @param alpha_score A Model parameters related to the time model for the passages to be scored.
# @param beta_score A Model parameters related to the time model for the passages to be scored.
# @param sigma The standard deviation parameter for the latent speed component.
# @param rho The correlation between the latent accuracy and speed components.
# @param C A vector of censoring indicators for the observed passages, where 0 indicates a fully observed passage and 1 indicates a censored passage.
# @param type Output type. Use `"general"` for latent accuracy and speed
#     estimates and `"orf"` to additionally return model-based WCPM estimates.
#      
# @return A list containing data and model parameters formatted for
#     passage-level Stan scoring models.
scoring.passage.censoring <- function(Count=NULL, 
                                      logT10=NULL, 
                                      N=NULL,
                                      N_score=NULL,
                                     # passage=NULL, 
                                     # passage_score=NULL,
                                      a=NULL,
                                      a_score=NULL, 
                                      b=NULL,
                                      b_score=NULL,
                                      alpha=NULL,
                                      alpha_score=NULL,
                                      beta=NULL,
                                      beta_score=NULL,
                                      sigma=NULL, 
                                      rho=NULL, 
                                      C=NULL, 
                                      type=NULL) {
  
  # loading logger
  log.initiating()
  
  flog.info("Begin scoring passage process", name = "orfrlog")
  
  # suppress_output <- function(expr) {
  #   sink(nullfile())
  #   expr
  #   sink()
  # }
  suppress_output <- function(expr) {
    con <- file(nullfile(), open = "w")

    sink(con)
    on.exit({
      if (sink.number() > 0) {
        sink()
      }
      close(con)
    }, add = TRUE)

    force(expr)
  }
  
  Y <- data.matrix(Count) # [r,c], r->person, c->passage
  logT10 <- data.matrix(logT10) # [r,c]
  N <- N # [passage]
  Cens <- data.matrix(C)
  
  n <- dim(Y)[[1]]
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
    #print(paste(" k= ", k))
    
    index <- which(!is.na(Y[k,]))
    
    data_list <- create_data_list(
      Count=Y[k,index], 
      logT10=logT10[k,index], 
      MaxN=N[index],
      MaxN_score=N_score,
      a=a[index],
      a_score=a_score,
      b=b[index],
      b_score=b_score,
      alpha=alpha[index],
      alpha_score=alpha_score,
      beta=beta[index],
      beta_score=beta_score,
      sigma=sigma, 
      rho=rho,
      C=Cens[k,index])

    suppress_output({
      score_object <- score_testlet(data_list)
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
  
  flog.info("End scoring passage process", name = "orfrlog")
  
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
