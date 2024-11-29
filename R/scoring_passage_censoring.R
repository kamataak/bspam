#' scoring.passage function 
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
#' @param a, b: Model parameters related to the count data model (K-dim)
#' @param alpha, beta: Model parameters related to the time data model (K-dim)
#' @param sigma: The latent standard deviation of the time latent component
#' @param rho: The correlation between count and time latent components
#' @param C: A vector of indicators whether a specific passage was 
#' censored (1) or fully observed (0) -- K-dim
#' 
#' @return  list
scoring.passage.censoring <- function(Count=NULL, logT10=NULL, N=NULL, 
                            a=NULL, b=NULL, alpha=NULL,beta=NULL, 
                            sigma=NULL, rho=NULL, C=NULL) {
  
  # loading logger
  log.initiating()
  
  flog.info("Begin scoring passage process", name = "orfrlog")
  
  suppress_output <- function(expr) {
    sink(nullfile())
    expr
    sink()
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
  
  for (k in 1:n) {
    
    # for debug
    # print(paste(" k= ", k))
    
    index <- which(!is.na(Y[k,]))
    
    data_list <- create_data_list(Y[k,index], logT10[k,index], N[index], 
                                  a[index], b[index], 
                                  alpha[index], beta[index], 
                                  sigma, rho, 
                                  Cens[k,index])

    suppress_output({
      score_object <- score_testlet(data_list)
    })
    
    theta_acc_est[k] <- score_object$theta_acc
    theta_spd_est[k] <- score_object$theta_spd
    theta_acc_sd[k] <- score_object$theta_acc_sd
    theta_spd_sd[k] <- score_object$theta_spd_sd
      
  }
  
  flog.info("End scoring passage process", name = "orfrlog")
  
  return(list(theta_acc_est=theta_acc_est,
              theta_spd_est=theta_spd_est,
              theta_acc_sd=theta_acc_sd,
              theta_spd_sd=theta_spd_sd))
}
