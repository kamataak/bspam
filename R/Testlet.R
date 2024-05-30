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
#' @param data A data frame. It has the information of student, passage, sentence, obs.count and time.  
#' @param person.id each student's id.
#' @param sub.task.id each sentence's id.
#' @param obs.count The column name in the data that represents the words read correctly for each sentence
#' @param time The column name in the data that represents the reading time for the sentence.
#' @param task.id The column name in the data that represents the unique passage identifier.
#' @param max.counts The column name in the data that represents the number of words in a sentence.
#'
#' @details
#' Additional details...
#' 
#' @note
#' Additional note...
#' 
#' 
#' @examples
#' # example code
#' fit.model.testlet <- function(data=NULL, person.id="", sub.task.id="",obs.count="", time="", task.id="", max.counts="")
#' 
#' @return list
#' @export
fit.model.testlet <- function(data=NULL, person.id="", sub.task.id="",obs.count="", time="", task.id="", max.counts="") {
  # loading logger
  log.initiating()
  if (is.null(data)) {
    flog.info("Dataset cannot be NULL!", name = "orfrlog")
    return
  } else {
    if (person.id == "" | sub.task.id == "" | obs.count == ""  | time == "" | task.id == ""  | max.counts == "") {
      flog.info("Missed columns! Make sure person.id, sub.task.id, obs.count, time, task.id, and max.counts are set.", name = "orfrlog")
      return(NA)
    } else {
      # get specific columns only
      data <- data %>% select(person.id, sub.task.id, obs.count,time,task.id,max.counts)
      colnames(data) <- c("person.id", "sub.task.id", "obs.count", "time", "task.id", "max.counts")

      # Add a column to manage unique passage_sentence id
      # data$taskid_subid <- paste(data$task.id, data$sub.task.id, sep="_")
      
      # check the data consistency
      # passage need to be read by at least two students
      count_number <- data %>% select(task.id, person.id) %>% unique() %>% group_by(task.id) %>% count() %>% filter(n < 2) %>% select(task.id)
      if (dim(count_number)[1]) {# any row exists?
        flog.info("Any passage should be read by at least two students.", name = "orfrlog")
        flog.info(paste("The following passages are only read by one student:", paste(as.character(unlist(count_number)), collapse = " ")), name = "orfrlog")
        return(NA)
      }
      flog.info("Begin testlet process", name = "orfrlog")
      
      # stan model
      model_code <- "
        data {
          int<lower=1> N; 
          int<lower=1> K;
          int<lower=0> Count[N]; 
          int<lower=0> MaxN[N]; 
          int<lower=0> Passage[N]; 
          real logT10[N];
          real a[N];
          real b[N];
          real alpha[N];
          real beta[N];
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
          
          for (i in 1:N) {
            Count[i] ~ binomial(MaxN[i], Phi(fmax(-5,fmin(5,a[i]*(theta1+gamma1*U1[Passage[i]])/sqrt(1+gamma1^2)-b[i]))));
            logT10[i] ~ normal(beta[i]-sigma*(theta2+gamma2*U2[Passage[i]])/sqrt(1+gamma2^2), 1/alpha[i]);
          }
        }"
      m <- rstan::stan_model(model_code = model_code)
      
      # create unique sentence id
      # Determine the maximum number of characters in sub.task.id
      max_length <- max(nchar(as.character(data$sub.task.id)))  
      
      # Create new unique ID code with leading zeros for padding
      temp <- data %>% select(task.id, sub.task.id) %>% 
        mutate(unique.id = as.numeric(paste(task.id, sprintf("%0*s", max_length, as.numeric(sub.task.id)), sep="")))  
      data$unique.id <- temp$unique.id
      
      Ys <- data %>%
        select(person.id, unique.id, obs.count) %>%
        spread(key = unique.id, value = obs.count) %>%
        select(-person.id)
      Y <- as.matrix(Ys)
      for (i in 1:ncol(Y)) {
        Y[,i]<-ifelse(is.na(Y[,i]),NaN,Y[,i])
      }
      
      logT <- data %>%
        mutate(logsecs=log(time)) %>%
        select(person.id, unique.id, logsecs) %>%
        # spread(key = sub.task.id, value = logsecs) %>%
        pivot_wider(names_from = unique.id, values_from = logsecs) %>%
        select(-person.id)
      # N <- sentence_data %>% 
      #   group_by_at(5) %>% summarise_at(6,max) %>% 
      #   select(obs.count)x
      
      # Maybe this is incorrect
      # Ns <- data %>% 
      #   select(person.id, sub.task.id, obs.count) %>% 
      #   group_by(sub.task.id) %>%  
      #   summarize(max(obs.count))
      Ns <- data %>% select(unique.id, max.counts) %>% 
        group_by(unique.id) %>% summarize(max.counts=max(max.counts))
      
      N <- pull(Ns) # the number of words in each sentence
      
      N.matrix <- matrix(rep(as.matrix(N),dim(Y)[1]),nrow = dim(Y)[1], byrow = TRUE)
      logT10 <- logT - log(N.matrix) + log(10)
      
      # getPassage <- as.numeric(substr(names(logT10),1,5))
      getPassage <-
        as.numeric(unlist((data %>% select(task.id, unique.id) %>% 
                             group_by(unique.id) %>% unique())[,1]))
      
      # get Passage orders
      unique_id <- getPassage %>% unique()
      
      Passage <- getPassage
      for (i in 1:length(unique_id)) {
        Passage[Passage == unique_id[i]] <- i
      }
      
      # Passage <- c()
      # i <- 1
      # for (j in 1:length(getPassage)) {
      #   if (getPassage[j] == unique_id[i]) {
      #     Passage <- append(Passage,i)
      #   } else {
      #     i <- i + 1
      #     Passage <- append(Passage,i)
      #   }
      # }
      
      n.It <- length(Passage) # the number of sentences 
      
      # as matrix
      logT10 <- as.matrix(logT10)
      
      
      parms.mom <- testlet_parms_mom(Y,logT10,Passage,N,n.It)
      
      # Calculate MCEM estimators
      parms.in <- parms.mom
      if (parms.in$gamma1<1e-6) {parms.in$gamma1 <- 0.01}
      if (parms.in$gamma2<1e-6) {parms.in$gamma2 <- 0.01}
      
      n.iter <- c(10,2)
      M.iter <- c(2,20)
      parms.mcem <- iterate_full_testlet_MCEM(Y,logT10,Passage,N,c(10,2),c(2,20),parms.in,m,n.It)
      
      # add task.id and sub.task.id to the output
      taskID <-  list(as.vector(getPassage))
      subtaskID <- as.vector(Ns[,1])
      parms.mcem <- append(parms.mcem,taskID,after=0)
      parms.mcem <- append(parms.mcem,subtaskID,after=1)
      
      # add Y, logT10, N for scoring
      parms.mcem <- append(parms.mcem,list(Y),after=2)
      parms.mcem <- append(parms.mcem,list(logT10),after=3)
      parms.mcem <- append(parms.mcem,list(N),after=4)
      
      # names(parms.mcem) <- c("task.id","sub.task.id", names(parms.mcem)[3:11])
      names(parms.mcem) <- c("task.id","sub.task.id", "Y", "logT10", "N", names(parms.mcem)[6:14])
     
      output.list <- list(task.param = tibble(a = parms.mcem$a,
                                            b = parms.mcem$b,
                                            alpha = parms.mcem$alpha,
                                            beta = parms.mcem$beta,
                                            task.id = parms.mcem$task.id,
                                            sub.task.id = parms.mcem$sub.task.id),
                          hyper.param = tibble(sigma = parms.mcem$sigma,
                                             gamma1 = parms.mcem$gamma1,
                                             gamma2 = parms.mcem$gamma2,
                                             rho.theta = parms.mcem$rho.theta,
                                             rho.testlet = parms.mcem$rho.testlet
                                             ),
                          Y = parms.mcem$Y,
                          logT10 = parms.mcem$logT10,
                          N = parms.mcem$N)
                          
                          
      class(output.list) <- "fit.model.testlet" # define class
      
      flog.info("End testlet process", name = "orfrlog")
      
      return (output.list)
      
    }   
    
  }

}


# n is the number of students in the sample
# 
# Passage indicates the passage that each sentence belongs to
# 
# n.P is the number of passages in the sample
# 
# n.It is the number of items in the sample
# 
# N is the number of words in each sentence
testlet_parms_mom <- function(Y,logT10,Pass,N,n.It) {
  
  # Needed functions for implementation
  # Function 1 of 3
  evaluate_rho <- function(data,par) {
    R <- data[1]
    Q <- data[2]
    r <- par
    sigma <- matrix(c(1,r,r,1), ncol=2)
    MD <- (R - pmvnorm(upper = c(Q,Q), mean = rep(0,2), sigma = sigma)[1])^2
    return(MD)
  }
  # Function 2 of 3
  evaluate_rho_biv <- function(data,par) {
    R <- data[1]
    Q1 <- data[2]
    Q2 <- data[3]
    r <- par
    sigma <- matrix(c(1,r,r,1), ncol=2)
    MD <- (R - pmvnorm(upper = c(Q1,Q2), mean = rep(0,2), sigma = sigma)[1])^2
    return(MD)
  }
  # Function 3 of 3
  reading_data_parms_ab <- function(Y,N) {
    Y.bar <- mean(Y)
    S2 <- mean((Y-Y.bar)^2)
    if (Y.bar<N) {
      Q <- qnorm(Y.bar/N)
    } else {
      Q <- qnorm((Y.bar+0.05)/(N+0.1))
    }
    R <- (S2 + Y.bar^2 - Y.bar)/(N*(N-1))
    parms.in <- 0.5
    rho.min <- optim(parms.in, fn = evaluate_rho, method = "Brent", data = c(R,Q), lower = 0, upper = 1)
    a.out <- sqrt(rho.min$par/(1-rho.min$par))
    b.out <- -Q*sqrt(1+a.out^2)
    parms <- list(a = a.out, b = b.out)
  }
  
  ##
  # Calculating a and b for count data
  n <- dim(Y)[1]
  n.It <- dim(Y)[2]
  a.out <- matrix(nrow = 1, ncol = n.It)
  b.out <- matrix(nrow = 1, ncol = n.It)
  for (i in 1:n.It) {
    if (mean(na.omit(Y[,i]))<N[i]) {
      ab.out <- reading_data_parms_ab(na.omit(Y[,i]),N[i])
    } else {
      ab.out <- reading_data_parms_ab(na.omit(Y[,i]),N[i]+1/length(na.omit(Y[,i])))
    }
    
    a.out[i] <- ab.out$a
    b.out[i] <- ab.out$b
  }
  a.out <- as.numeric(a.out)
  b.out <- as.numeric(b.out)
  
  ##
  # Calculating omega (alpha), beta, and sigma for time data
  beta.out <- apply(logT10,2,mean,na.rm=T)
  sigma2.in <- NULL
  n.C <- NULL
  PID <- unique(Pass)
  n.P <- length(PID)
  for (j in 1:n.P) {
    C <- cov(logT10[,Pass==PID[j]],use="pairwise.complete.obs")
    sigma2.in <- c(sigma2.in,mean(C[upper.tri(C)==T]))
    n0 <- dim(C)[1]
    n.C <- c(n.C,n0*(n0-1)/2)
  }
  sigma2 <- sum(n.C*sigma2.in)/sum(n.C)
  omega2 <- apply(logT10,2,var,na.rm=T)-sigma2
  sigma.out <- sqrt(sigma2)
  omega2[omega2<=0] <- min(omega2[omega2>0])*0.8
  omega.out <- sqrt(omega2)
  
  ##
  # gamma2 estimation
  S <- 1-is.na(logT10)
  n.pair.mat <- t(S)%*%S
  ind.mat1 <- matrix(rep(Pass,n.It),byrow=T,nrow=n.It)
  ind.mat2 <- t(ind.mat1)
  C.full <- cov(logT10,use="pairwise.complete.obs")
  index <- which(upper.tri(C.full)==T)
  C.select <- C.full[index]
  ind1.select <- ind.mat1[index]
  ind2.select <- ind.mat2[index]
  n.pairs <- n.pair.mat[index]
  index <- (ind1.select!=ind2.select)
  C.select <- C.select[index]
  ind1.select <- ind1.select[index]
  ind2.select <- ind2.select[index]
  n.pairs <- n.pairs[index]
  index <- which(is.na(C.select)==F)
  C.select <- C.select[index]
  ind1.select <- ind1.select[index]
  ind2.select <- ind2.select[index]
  n.pairs <- n.pairs[index]
  ind.pos1 <- match(ind1.select, unique(Pass))
  ind.pos2 <- match(ind2.select, unique(Pass))
  unique.id <- matrix(as.numeric(unique(cbind(ind.pos1,ind.pos2))),
                      ncol = 2, byrow = F)
  Y.exp <- rep(0,nrow(unique.id))
  n.use <- rep(0,length(Y.exp))
  for (kk in 1:nrow(unique.id)) {
    index1 <- which(ind.pos1==unique.id[kk,1])
    index2 <- which(ind.pos2==unique.id[kk,2])
    index <- intersect(index1,index2)
    Y.exp[kk] <- mean(C.select[index])
    n.use[kk] <- mean(n.pairs[index])
  }
  weighted.inv <- sum(n.use*Y.exp/sigma.out^2)/sum(n.use)
  gamma2sq.out <- pmax(0,1/weighted.inv-1)
  gamma2.out <- sqrt(gamma2sq.out)
  
  ##
  # gamma1 estimation
  N.mat <- matrix(rep(N,n.It),byrow=T,nrow=n.It)
  S <- 1-is.na(Y)
  n.pair.mat <- t(S)%*%S
  Y.0 <- Y
  Y.0[is.na(Y.0)==T] <- 0
  Y.cross <- t(Y.0)%*%Y.0
  Y.cross <- Y.cross/n.pair.mat
  p.cross <- Y.cross/N.mat/t(N.mat)
  mean.Y <- apply(Y,2,mean,na.rm=T)
  mean.p <- mean.Y/N
  mat.mean.p1 <- matrix(rep(mean.p,n.It),byrow=T,nrow=n.It)
  mat.mean.p2 <- t(mat.mean.p1)
  ind.mat1 <- matrix(rep(Pass,n.It),byrow=T,nrow=n.It)
  ind.mat2 <- t(ind.mat1)
  a.est1 <- matrix(rep(a.out,n.It),byrow=T,nrow=n.It)
  a.est2 <- t(a.est1)
  b.est1 <- matrix(rep(b.out,n.It),byrow=T,nrow=n.It)
  b.est2 <- t(b.est1)
  index <- which(upper.tri(p.cross)==T)
  n.pair <- n.pair.mat[index]
  p.cross <- p.cross[index]
  mat.mean.p1 <- mat.mean.p1[index]
  mat.mean.p2 <- mat.mean.p2[index]
  ind.mat1 <- ind.mat1[index]
  ind.mat2 <- ind.mat2[index]
  a.est1 <- a.est1[index]
  a.est2 <- a.est2[index]
  b.est1 <- b.est1[index]
  b.est2 <- b.est2[index]
  index <- which(is.na(p.cross)==F)
  n.pair <- n.pair[index]
  p.cross <- p.cross[index]
  mat.mean.p1 <- mat.mean.p1[index]
  mat.mean.p2 <- mat.mean.p2[index]
  ind.mat1 <- ind.mat1[index]
  ind.mat2 <- ind.mat2[index]
  a.est1 <- a.est1[index]
  a.est2 <- a.est2[index]
  b.est1 <- b.est1[index]
  b.est2 <- b.est2[index]
  index <- which(ind.mat1!=ind.mat2)
  n.pair <- n.pair[index]
  p.cross <- p.cross[index]
  mat.mean.p1 <- mat.mean.p1[index]
  mat.mean.p2 <- mat.mean.p2[index]
  ind.mat1 <- ind.mat1[index]
  ind.mat2 <- ind.mat2[index]
  a.est1 <- a.est1[index]
  a.est2 <- a.est2[index]
  b.est1 <- b.est1[index]
  b.est2 <- b.est2[index]
  rho.biv <- rep(0,length(p.cross))
  
  for (k in 1:length(p.cross)) {
    parms.in <- 0.5
    R <- p.cross[k]
    Q1 <- -b.est1[k]/sqrt(1+a.est1[k]^2)
    Q2 <- -b.est2[k]/sqrt(1+a.est2[k]^2)
    rho.min <- optim(parms.in, fn = evaluate_rho_biv, method = "Brent", data = c(R,Q1,Q2), lower = 0, upper = 1)
    rho.biv[k] <- rho.min$par
  }
  
  Y.pre <- rho.biv*sqrt(1+a.est1^2)*sqrt(1+a.est2^2)/(a.est1*a.est2)
  Y.exp <- rep(0,nrow(unique.id))
  n.use <- rep(0,length(Y.exp))
  for (kk in 1:nrow(unique.id)) {
    index1 <- which(ind.pos1==unique.id[kk,1])
    index2 <- which(ind.pos2==unique.id[kk,2])
    index <- intersect(index1,index2)
    Y.exp[kk] <- mean(Y.pre[index])
    n.use[kk] <- mean(n.pairs[index])
  }
  weighted.inv <- sum(n.use*Y.exp)/sum(n.use)
  gamma1sq.out <- pmax(0,1/weighted.inv-1)
  gamma1.out <- sqrt(gamma1sq.out)
  
  ##
  # rho estimation
  cov_YT <- cov(Y,logT10,use="pairwise.complete.obs")
  N.mat <- matrix(rep(N,n.It),byrow=F,nrow=n.It)
  S <- 1-is.na(Y)
  n.pair.mat <- t(S)%*%S
  a.est <- matrix(rep(a.out,n.It),byrow=F,nrow=n.It)
  b.est <- matrix(rep(b.out,n.It),byrow=F,nrow=n.It)
  scaled.cov <- -cov_YT*sqrt(2*pi/sigma2)/N.mat*sqrt((1+a.est^2)/a.est^2)*exp(0.5*b.est^2/(1+a.est^2))
  scaled.cov <- scaled.cov*sqrt(1+gamma1.out^2)*sqrt(1+gamma2.out^2)
  ind.mat1 <- matrix(rep(Pass,n.It),byrow=F,nrow=n.It)
  ind.mat2 <- t(ind.mat1)
  n.pair1 <- n.pair.mat[ind.mat1!=ind.mat2]
  scaled.cov1 <- scaled.cov[ind.mat1!=ind.mat2]
  n.pair2 <- n.pair.mat[ind.mat1==ind.mat2]
  scaled.cov2 <- scaled.cov[ind.mat1==ind.mat2]
  n.pair1 <- n.pair1[is.na(scaled.cov1)==F]
  scaled.cov1 <- scaled.cov1[is.na(scaled.cov1)==F]
  scaled.cov1[scaled.cov1>1] <- 1
  scaled.cov1[scaled.cov1<(-1)] <- -1
  rho.theta <- mean(scaled.cov1)
  scaled.cov2 <- (scaled.cov2 - rho.theta)/(gamma1.out*gamma2.out)
  scaled.cov2[scaled.cov2>1] <- 1
  scaled.cov2[scaled.cov2<(-1)] <- -1
  rho.testlet <- mean(scaled.cov2)
  
  mom.parms <- list(a = a.out, b = b.out, beta = beta.out,
                    sigma = sigma.out, omega = omega.out,
                    alpha = 1/omega.out,
                    gamma1 = gamma1.out, gamma2 = gamma2.out,
                    rho.theta = rho.theta, rho.testlet = rho.testlet)
  
  return(mom.parms)
}

# Likelihood Components for General Testlet Model
# That is: item-specific parameters, except common sigma,
# gamma1 and gamma2 are common to all testlets

# Count NLLH per Testlet
Item.Parms.NegLogLLH.Count <- function(par,Y,n.Y,N.item,gamma1,theta1,U1) {
  # U1 here should be M x n (for only a single testlet)
  a <- par[1]
  b <- par[2]
  
  LogLLH <- rep(0,n.Y)
  
  V1 <- (theta1+gamma1*U1)/sqrt(1+gamma1^2)
  Arg1 <- t(pnorm(V1*a-b))
  Count.LogLLH <- rowMeans(apply(Arg1, 2, function(col) dbinom(Y,N.item,col,log=T)))
  
  NegLogLLH <- -sum(Count.LogLLH)
  
  return(NegLogLLH)
}

Count.NegLogLLH <- function(par,Y,S,n.vec,N,J,Passage,a.in,b.in,theta1,U1) {
  
  gamma1 <- exp(par)
  negLogLLH <- rep(0,J)
  a.run <- a.in 
  b.run <- b.in
  
  for (j in 1:J) {
    index.j <- which(S[,j]==1)
    Y.j <- Y[index.j,j]
    n.j <- n.vec[j]
    a.item <- a.run[j]
    b.item <- b.run[j]
    U1.testlet <- U1[,Passage[j],index.j]
    theta1.j <- theta1[,index.j]
    run.j <- optim(c(a.item,b.item),
                   fn = Item.Parms.NegLogLLH.Count,
                   Y = Y.j, 
                   n.Y = n.j, 
                   N.item = N[j], 
                   gamma1 = gamma1,
                   theta1 = theta1.j, 
                   U1 = U1.testlet)
    negLogLLH[j] <- run.j$val
    a.run[j] <- run.j$par[1]
    b.run[j] <- run.j$par[2]
  }
  
  negLogLLH <- sum(negLogLLH)
  
  return(negLogLLH)
  
}

Item.Parms.NegLogLLH.logTime <- function(par,logT10,n.T,sigma,gamma2,theta2,U2) {
  # U2 here should be M x n (for only a single testlet)
  alpha <- exp(par[1])
  beta <- par[2]
  
  LogLLH <- rep(0,n.T)
  
  V2 <- sigma*t((theta2+gamma2*U2)/sqrt(1+gamma2^2))
  logTime.LogLLH <- rowMeans(apply(V2, 2, 
                                   function(col) dnorm(logT10, mean = beta - col, sd = 1/alpha, log=T)))
  
  NegLogLLH <- -sum(logTime.LogLLH)
  
  return(NegLogLLH)
}

logTime.NegLogLLH <- function(par,logT10,S,n.vec,J,Passage,alpha.in,beta.in,theta2,U2) {
  
  sigma <- exp(par[1])
  gamma2 <- exp(par[2])
  negLogLLH <- rep(0,J)
  alpha.run <- alpha.in
  beta.run <- beta.in
  
  for (j in 1:J) {
    index.j <- which(S[,j]==1)
    logT10.j <- logT10[index.j,j]
    n.j <- n.vec[j]
    alpha.item <- alpha.run[j]
    beta.item <- beta.run[j]
    U2.testlet <- U2[,Passage[j],index.j]
    theta2.j <- theta2[,index.j]
    run.j <- optim(c(log(alpha.item),beta.item),
                   fn = Item.Parms.NegLogLLH.logTime,
                   logT10 = logT10.j, 
                   n.T = n.j, 
                   sigma = sigma,
                   gamma2 = gamma2,
                   theta2 = theta2.j, 
                   U2 = U2.testlet)
    negLogLLH[j] <- run.j$val
    alpha.run[j] <- exp(run.j$par[1])
    beta.run[j] <- run.j$par[2]
  }
  
  negLogLLH <- sum(negLogLLH)
  
  return(negLogLLH)
  
}

# Latent Ability NLLH
Parms.NegLogLLH.rhoTheta <- function(par,n,theta1,theta2) {
  rho <- 2*pnorm(par)-1
  
  LogLLH <- rep(0,n)
  
  for (i in 1:n) {
    
    Sigma <- matrix(c(1,rho,rho,1),nrow=2)
    Theta.LogLLH <- mean(apply(cbind(theta1[,i],theta2[,i]), 1, function(row) dmvnorm(row,mean=c(0,0),sigma=Sigma,log=T)))
    
    LogLLH[i] <- Theta.LogLLH
    
  }
  
  NegLogLLH <- -sum(LogLLH)
  
  return(NegLogLLH)
}

# Testlet Correlation NLLH
Parms.NegLogLLH.Testlet <- function(par,n,K,U1,U2) {
  
  rhoTestlet <- 2*pnorm(par)-1
  
  LogLLH <- rep(0,n)
  
  for (i in 1:n) {
    
    A1 <- U1[,,i]
    A2 <- U2[,,i]
    S <- 1 - is.na(A1[1,])
    
    Sigma2 <- matrix(c(1,rhoTestlet,rhoTestlet,1),nrow=2)
    U.LogLLH <- rep(0,K)
    for (j in which(S==1)) {
      U.LogLLH[j] <- mean(apply(cbind(A1[,j],A2[,j]), 1, function(row) dmvnorm(row,mean=c(0,0),sigma=Sigma2,log=T)))
    }
    
    LogLLH[i] <- sum(U.LogLLH)
    
  }
  
  NegLogLLH <- -sum(LogLLH)
  
  return(NegLogLLH)
}

iterate_full_testlet_MCEM <- function(Y,logT10,Passage,N,n.iter,M.iter,par.in,m,n.It) {
  
  if (missing(par.in)) {par.in <- testlet_parms_mom(Y,logT10,Passage,N)}
  n <- dim(Y)[1]
  J <- dim(Y)[2]
  n.P <- length(unique(Passage))
  
  a.run <- par.in$a
  b.run <- par.in$b
  alpha.run <- par.in$alpha
  beta.run <- par.in$beta
  gamma1.run <- par.in$gamma1
  gamma2.run <- par.in$gamma2
  sigma.run <- par.in$sigma
  rho.run <- par.in$rho.theta
  rhoTestlet.run <- par.in$rho.testlet
  
  num.forloops <- length(n.iter)
  
  for (ii in 1:num.forloops) {
    
    M <- M.iter[ii]
    
    theta1 <- array(NA,dim=c(M,n))
    theta2 <- array(NA,dim=c(M,n))
    U1 <- array(NA,dim=c(M,n.P,n))
    U2 <- array(NA,dim=c(M,n.P,n))
    
    for (i in 1:n) {
      S <- 1 - is.na(Y[i,])
      Y.new <- Y[i,S==1]
      logT10.new <- logT10[i,S==1]
      Pass.i <- Passage[S==1]
      K.i <- length(unique(Passage[S==1]))
      index.i <- as.integer(factor(Pass.i, levels = unique(Pass.i)))
      
      fit <- rstan::sampling(m,#stan(model_code = model_code, 
                      data = list(N = sum(S), K = K.i,
                                  Count = Y.new,  logT10 = logT10.new,
                                  MaxN = N[S==1], Passage = index.i,
                                  a = a.run[S==1], b = b.run[S==1],
                                  alpha = alpha.run[S==1], beta = beta.run[S==1],
                                  gamma1 = gamma1.run,
                                  gamma2 = gamma2.run,
                                  sigma = sigma.run,
                                  rho = rho.run,
                                  rhoTestlet = rhoTestlet.run),
                      chains = 1, warmup = 1000, iter = (2000+M), cores = 1, refresh = 0)
      
      fit_ss <- rstan::extract(fit, permuted = TRUE) # Kuo memo: specifying rstan, otherwise, when load tidyverse, it will occur an error
      theta1[,i] <- fit_ss$theta1[1:M]
      theta2[,i] <- fit_ss$theta2[1:M]
      U1[,unique(Pass.i),i] <- fit_ss$U1[1:M,]
      U2[,unique(Pass.i),i] <- fit_ss$U2[1:M,]
      
    }
    
    S <- 1-is.na(Y)
    n.vec <- colSums(S)
    gamma1.optim <- optim(log(gamma1.run),
                          fn = Count.NegLogLLH,
                          method = "BFGS",
                          Y = Y, S = S, n.vec = n.vec,
                          N = N, J = n.It, Passage = Passage,
                          a.in = a.run, b.in = b.run,
                          theta1 = theta1, U1 = U1)
    gamma1.out <- exp(gamma1.optim$par)
    
    a.out <- rep(0,J)
    b.out <- rep(0,J)
    for (j in 1:J) {
      index.j <- which(S[,j]==1)
      Y.j <- Y[index.j,j]
      n.j <- n.vec[j]
      ab_item.optim <- optim(c(a.run[j],b.run[j]),
                             fn = Item.Parms.NegLogLLH.Count,
                             method = "BFGS",
                             Y = Y.j, n.Y = n.j,
                             N.item = N[j], gamma1 = gamma1.out,
                             theta1 = theta1[,index.j], U1 = U1[,Passage[j],index.j])
      a.out[j] <- ab_item.optim$par[1]
      b.out[j] <- ab_item.optim$par[2]
    }
    
    siggam.optim <- optim(c(log(sigma.run),log(gamma2.run)),
                          fn = logTime.NegLogLLH,
                          method = "BFGS",
                          logT10 = logT10, S = S, n.vec = n.vec,
                          J = 10, Passage = Passage,
                          alpha.in = par.in$alpha,
                          beta.in = par.in$beta,
                          theta2 = theta2, U2 = U2)
    sigma.out <- exp(siggam.optim$par[1])
    gamma2.out <- exp(siggam.optim$par[2])
    
    alpha.out <- rep(0,J)
    beta.out <- rep(0,J)
    for (j in 1:J) {
      index.j <- which(S[,j]==1)
      logT10.j <- logT10[index.j,j]
      n.j <- n.vec[j]
      alphabeta_item.optim <- optim(c(log(alpha.run[j]),beta.run[j]),
                                    fn = Item.Parms.NegLogLLH.logTime,
                                    method = "BFGS",
                                    logT10 = logT10.j,
                                    n.T = n.j, sigma = sigma.out, 
                                    gamma2 = gamma2.out,
                                    theta2 = theta2[,index.j], U2 = U2[,Passage[j],index.j])
      alpha.out[j] <- exp(alphabeta_item.optim$par[1])
      beta.out[j] <- alphabeta_item.optim$par[2]
    }
    
    theta.mod.out <- optim(qnorm((rho.run+1)/2), fn = Parms.NegLogLLH.rhoTheta,
                           method = "BFGS",
                           n = n, theta1 = theta1, theta2 = theta2)
    rho.out <- 2*pnorm(theta.mod.out$par)-1
    
    testlet.mod.out <- optim(qnorm((rhoTestlet.run+1)/2), 
                             fn = Parms.NegLogLLH.Testlet,
                             method = "BFGS",
                             n = n, K = n.P, U1 = U1, U2 = U2)
    rhoTestlet.out <- 2*pnorm(testlet.mod.out$par)-1
    
    a.run <- a.out
    b.run <- b.out
    alpha.run <- alpha.out
    beta.run <- beta.out
    sigma.run <- sigma.out
    gamma1.run <- gamma1.out
    gamma2.run <- gamma2.out
    rho.run <- rho.out
    rhoTestlet.run <- rhoTestlet.out
    
  }
  
  par.mcem <- list(a = a.out, b = b.out, alpha = alpha.out,
                   beta = beta.out, sigma = sigma.out,
                   gamma1 = gamma1.out, gamma2 = gamma2.out,
                   rho.theta = rho.out, rho.testlet = rhoTestlet.out)
  
}

# m <- stan_model(model_code = model_code)
