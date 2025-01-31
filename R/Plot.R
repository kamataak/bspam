#' Plot function to show graph of fit.model class
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
#
#' A copy of the GNU General Public License is available at
#' http://www.gnu.org/licenses/
#'
#' @param object    fit.model object
#' @param task      task ids for plotting
#' @param parameter model parameter for plotting, a,b,alpha,beta
#' @param sort      sorting flag, TRUE or FALSE
#'
#'
#' @import plotly
#' 
#' @export plot.task
#' @export
plot.task <- function(object, task=NULL, parameter, sort=F){
  
  if(class(object)!="fit.model" & class(object)!="fit.model.testlet")
    stop("Error: It seems like your object is not obtained through bspam. Make sure you use the calibration output from the `fit.model()` function!")
  
  if(F %in% (parameter %in% c("a", "b", "alpha", "beta")))
    stop("Error: Please check your parameter name(s)! Make sure they are entered correctly.")
  
  #Selection of tasks if any
  if(is.null(task)){
    task.sel <- object$task.param
  }else{
    if(F %in% (task %in% object$task.param$task.id))
      stop("Error: Please check your task IDs! Make sure they are entered correctly.")
    task.sel <- object$task.param %>%
      filter(task.id %in% task)
  }
  
  if(length(parameter) == 1){
    task.sel <- task.sel %>%
      mutate(task.id=as.character(task.id)) %>%
      rename(par_grph=paste(parameter))
    
    if(sort==T){
      task.sel <- task.sel %>%
        arrange(par_grph) %>%
        mutate(task.id=factor(task.id, levels=.$task.id))
    }
    plt_obj <- task.sel %>%
      plot_ly(y=~task.id, 
              x=~par_grph, 
              type ="bar",
              text=~paste("Task ID: ", task.id, '<br>Estimate:', paste(round(par_grph,3))), 
              showlegend=F, 
              hoverinfo='text',
              textposition = "none",
              height=max(n_distinct(task.sel$task.id)*13, 400),
              color = I("#CC0035")) %>%
      layout(xaxis=list(title = paste(parameter), zeroline = F, showline = F, showticklabels = T, showgrid = T),
             yaxis=list(title = "Task ID", zeroline = F, showline = F, showticklabels = T, showgrid = T, dtick=1))
  }else if(length(parameter) == 2){
    task.sel <- task.sel %>% 
      mutate(task.id=as.character(task.id)) %>%
      rename(par_grph_x=paste(parameter[1]), par_grph_y=paste(parameter[2]))
    
    plt_obj <- task.sel %>%
      plot_ly(x=~par_grph_x, 
              y=~par_grph_y, 
              type = "scatter", 
              mode = "markers",
              text=~paste("Task ID: ", task.id, '<br>', 
                          paste(parameter[1]), ':', paste(round(par_grph_x, 3)), '<br>', 
                          paste(parameter[2]), ':', paste(round(par_grph_y,3))),
              showlegend=F,
              hoverinfo='text',
              textposition = "none",
              color = I("#CC0035")) %>%
      layout(xaxis = list(title = list(text =paste(parameter[1]))), 
             yaxis = list(title = list(text =paste(parameter[2]))))
  }
  else{
    stop("Error: Only two parameters can be visualized at a time!")
  }
  plt_obj
}
#' Plot function to show graph of scoring class
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
#
#' A copy of the GNU General Public License is available at
#' http://www.gnu.org/licenses/
#'
#' @param object    scoring object
#' @param person    person ids for plotting
#' @param parameter model parameter for plotting, a,b,alpha,beta
#' @param show.se   standard error bar flag, TRUE for showing or FALSE for no showing
#' @param sort      sorting flag, TRUE or FALSE
#'
#'
#' @import plotly
#' 
#' @export plot.person
#' @export
plot.person <- function(object, person=NULL, parameter, show.se=T, sort=F){
  
  if(class(object)!="scoring")
    stop("Error: It seems like your object is not obtained through bspam. Make sure you use the scoring output from the `scoring()` function!")
  
  if(F %in% (parameter %in% c("theta", "tau", "wcpm")))
    stop("Error: Please check your parameter name(s)! Make sure they are entered correctly.")
  
  class(object) = "list"
  object <- as.data.frame(object)
  
  #Selection of persons (if any)
  if(is.null(person)){
    person.sel <- object
  }else{
    if(F %in% (person %in% object$person.id))
      stop("Error: Please check your person IDs! Make sure they are entered correctly.")
    person.sel <- object %>%
      filter(person.id %in% person)
  }
  
  #Change Parameter Ext.
  parameter <- person.sel %>%
    dplyr::select(starts_with(parameter), -ends_with(".obs")) %>% # exclude wcpm.obs
    colnames()
  
  if(length(parameter) == 1){
    person.sel <- person.sel %>%
      mutate(person.id=as.factor(person.id)) %>%
      rename(par_grph=paste(parameter))
    
    if(sort==T){
      person.sel <- person.sel %>%
        arrange(par_grph) %>%
        mutate(person.id=factor(person.id, levels=.$person.id))
    }
    
    if(show.se==T){
      person.sel <- person.sel %>%
        rename(se_grph=paste("se.", parameter, sep = "")) %>%
        mutate(se_grph=1.96*se_grph) %>%
        mutate(ci_low=par_grph - se_grph, 
               ci_high=par_grph + se_grph)
      
      error_x <- list(array=~se_grph, color="lightgray", width="2")
      
      plt_obj <- person.sel %>% 
        plot_ly(y=~person.id, 
                x=~par_grph, 
                type ="scatter",
                mode = "markers",
                text=~paste("person ID: ", person.id, '<br>Estimate:', paste(round(par_grph,3)), '<br>CI Low:', paste(round(ci_low,3)), '<br>CI High:', paste(round(ci_high,3))),
                showlegend=F, 
                hoverinfo='text',
                error_x=error_x,
                textposition = "none",
                height=max(n_distinct(person.sel$person.id)*13, 400),
                color = I("#CC0035")) %>%
        layout(xaxis=list(title = paste(parameter), zeroline = F, showline = F, showticklabels = T, showgrid = T), 
               yaxis=list(title = "Person ID", zeroline = F, showline = F, showticklabels = T, showgrid = T, dtick = 1, autosize=T, automargin = TRUE, tickmode = "linear")) 
    }else{
      plt_obj <- person.sel %>% 
        plot_ly(y=~person.id, 
                x=~par_grph, 
                type ="bar",
                text=~paste("person ID: ", person.id, '<br>Estimate:', paste(round(par_grph,3))), 
                showlegend=F, 
                hoverinfo='text',
                textposition = "none",
                height=max(n_distinct(person.sel$person.id)*13, 400),
                color = I("#CC0035")) %>%
        layout(xaxis=list(title = paste(parameter), zeroline = F, showline = F, showticklabels = T, showgrid = T),
               yaxis=list(title = "Person ID", zeroline = F, showline = F, showticklabels = T, showgrid = T, dtick=1, automargin = TRUE, tickmode = "linear", autosize=T))
    }
    
  }else if(length(parameter) == 2){
    
    person.sel <- person.sel %>%
      mutate(person.id=as.character(person.id)) %>%
      rename(par_grph_x=paste(parameter[1]), par_grph_y=paste(parameter[2]))
    
    if(show.se==T){
      person.sel <- person.sel %>%
        rename(se_grph_x=paste("se.", parameter[1], sep = ""), 
               se_grph_y=paste("se.", parameter[2], sep = "")) %>%
        mutate(se_grph_x=1.96*se_grph_x,
               se_grph_y=1.96*se_grph_y) %>%
        mutate(ci_low_x=par_grph_x - se_grph_x, 
               ci_high_x=par_grph_x + se_grph_x, 
               ci_low_y=par_grph_y - se_grph_y, 
               ci_high_y=par_grph_y + se_grph_y)
      error_x <- list(array=~se_grph_x, color="lightgray", width="2")
      error_y <- list(array=~se_grph_y, color="lightgray", width="2")
    }else{
      error_x <- NULL
      error_y <- NULL
    }
    plt_obj <- person.sel %>%
      plot_ly(x=~par_grph_x, 
              y=~par_grph_y, 
              type = "scatter", 
              mode = "markers",
              error_x=error_x,
              error_y=error_y,
              text=~paste("person ID: ", person.id, '<br>', 
                          paste(parameter[1]), ':', paste(round(par_grph_x, 3)), '[', paste(round(ci_low_x,3)), ',', paste(round(ci_high_x,3)), ']', '<br>', 
                          paste(parameter[2]), ':', paste(round(par_grph_y,3)), '[', paste(round(ci_low_y,3)), ',', paste(round(ci_high_y,3)), ']'),
              showlegend=F,
              hoverinfo='text',
              textposition = "none",
              color = I("#CC0035")) %>%
      layout(xaxis = list(title = list(text =paste(parameter[1]))), 
             yaxis = list(title = list(text =paste(parameter[2]))))
  } else{
    stop("Error: Only two parameters can be visualized at a time!")
  }
  plt_obj
}
#' Plot function to show information of calibration
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
#
#' A copy of the GNU General Public License is available at
#' http://www.gnu.org/licenses/
#'
#' @param calib_data    calibration object
#'
#' @import plotly
#' @import mvtnorm
#' @import tidyverse
#' @import nleqslv
#' 
#' @export plot.information
#' @export
plot.information <- function(calib_data=NULL) {
  
  # functions for this function
  # Required functions
  count.gen <- function(df, nn, np){
    output <- numeric()
    for (i in 1:(nn*np)){
      output[i] <- rbinom(n=1, size=df$numwords.p[i], prob=df$p.theta[i])
    }
    output
  }
  
  time.gen <- function(df, nn, np) {
    output <- numeric()
    for (i in 1:(nn*np)) {
      output[i] <- rnorm(n=1, mean=df$mu[i], sd=sqrt(df$sig[i]))
    }
    output
  }
  
  mod.pd1 <- function(theta) {
    eta <- a.par*theta - b.par
    term1 <- sum(a.par*Ru*dnorm(eta)/pnorm(eta))
    term2 <- sum(a.par*(nwords-Ru)*dnorm(eta)/(1-pnorm(eta)))
    pd1 <- term1 - term2
  }
  get.perfectcases <- function(data) {
    perfect.cases <- data %>% dplyr::group_by(student.id,occasion) %>%
      dplyr::summarise(wrc_sum=sum(wrc),
                       numwords.p_sum=sum(numwords.p), .groups = "drop_last") %>%
      dplyr::filter(wrc_sum == numwords.p_sum) %>%
      tidyr::unite("perfect.cases", student.id:occasion, sep = "_", remove = TRUE, na.rm = FALSE) %>%
      dplyr::select(perfect.cases)
    return(invisible(perfect.cases))
  }   
  est.theta.tau <- function(stu.data, pass.data, case,q=49, lo = -12, hi = 12, external=NULL, rho = rho, vartau=vartau,Estimator) {
    ##### read in data and parameters ######
    stu.dat01 <- stu.data %>% filter(stu.data$student.id==case)
    if (nrow(stu.dat01) == 0) {
      # flog.info(paste("No data for:", case), name = "orfrlog")
      return(NULL)
    }
    pass.read <- stu.dat01 %>% dplyr::select(passage.id)
    
    # passage.id should be included in MCEM object
    pass.dat01 <- pass.data %>% semi_join(pass.read, by = "passage.id")
    n.pass <- nrow(pass.dat01)
    
    numwords.total <- stu.dat01 %>% dplyr::select(numwords.p) %>% c() %>% unlist() %>% sum()
    grade <- stu.dat01 %>% dplyr::select(grade) %>% c() %>% unlist %>% unique()
    
    wrc <- stu.dat01 %>% dplyr::select(wrc) %>% c() %>% unlist()
    lgsec <- stu.dat01 %>% dplyr::select(lgsec) %>% c() %>% unlist()
    numwords.p <- stu.dat01 %>% dplyr::select(numwords.p) %>% c() %>% unlist()
    lgsec10 <- lgsec-log(numwords.p) + log(10)
    
    a.par <- pass.dat01 %>% dplyr::select(a) %>% c() %>% unlist()
    b.par <- pass.dat01 %>% dplyr::select(b) %>% c() %>% unlist()
    alpha.par <- pass.dat01 %>% dplyr::select(alpha) %>% c() %>% unlist()
    beta.par <- pass.dat01 %>% dplyr::select(beta) %>% c() %>% unlist()
    
    if (!is.null(external))  { # When external passages
      
      # get a, b, alpha, beta from MCEM with specific passage.id
      a.par.external <- pass.data %>% filter(passage.id %in% external) %>% dplyr::select(a) %>% c() %>% unlist()
      b.par.external <- pass.data %>% filter(passage.id %in% external) %>% dplyr::select(b) %>% c() %>% unlist()
      alpha.par.external <- pass.data %>% filter(passage.id %in% external) %>% dplyr::select(alpha) %>% c() %>% unlist()
      beta.par.external <- pass.data %>% filter(passage.id %in% external) %>% dplyr::select(beta) %>% c() %>% unlist()
      numwords.p.external <- pass.data %>% filter(passage.id %in% external) %>% dplyr::select(numwords.p) %>% c() %>% unlist()
    }
    
    # Compute n.pass.wcpm and numwords.total.wcpm
    if (is.null(external)) {
      n.pass.wcpm <- n.pass
      numwords.total.wcpm <- sum(numwords.p)
    } else {
      n.pass.wcpm <- length(external)
      numwords.total.wcpm <- sum(numwords.p.external)
    }
    
    # Compute observed accuracy (wrc), speed (secs), and fluency (wcpm).
    wrc.obs <- stu.dat01 %>% dplyr::select(wrc) %>% sum()
    secs.obs <- stu.dat01 %>% dplyr::select(sec) %>% sum()
    wcpm.obs <- wrc.obs/secs.obs*60
    
    ##### R functions ######
    score.mle <- function (theta_prior){
      tol_loglike <- 1e-04
      tol_NR <- 1e-3
      max_itr <- 1000
      theta_grid <- seq(lo, hi, 0.5)
      l1 <- function(th_try) { sum(a.par*wrc*dnorm(a.par*th_try-b.par)/pnorm(a.par*th_try-b.par))-
          sum(a.par*(numwords.p-wrc)*dnorm(a.par*th_try-b.par)/(1-pnorm(a.par*th_try-b.par)))}
      
      l2<-function(th_try){sum((a.par^2*wrc*(ddnorm(a.par*th_try-b.par)*pnorm(a.par*th_try-b.par)-dnorm(a.par*th_try-b.par)^2))/((pnorm(a.par*th_try-b.par))^2))-
          sum(((numwords.p-wrc)*a.par^2*(ddnorm(a.par*th_try-b.par)*(1-pnorm(a.par*th_try-b.par))+dnorm(a.par*th_try-b.par)^2))/((1-pnorm(a.par*th_try-b.par))^2))}
      
      if (abs(l1(theta_prior)) <= tol_loglike) {
        theta_hat <- theta_prior
        SE <- 1 / sqrt(-l2(theta_prior))
      } else { # start iteration
        if (theta_prior >= lo && theta_prior <= hi) {
          theta_curr <- theta_prior
        } else {
          theta_curr <- 0
        }
        itr <- 0
        l1_curr <- l1(theta_curr)
        delta <- 100
        while ((abs(l1_curr) > tol_loglike) || (abs(delta) > tol_NR)) {
          if (itr > max_itr) {
            message<-paste('MLE does not converge in replication',rep)
            model.file.name<-paste("mlenonconvergence",rep,".txt")
            write(x=message,file=model.file.name,append = FALSE)
            break }
          itr <- itr + 1
          delta <- l1_curr / l2(theta_curr)
          if ((theta_curr - delta > lo) &&
              (theta_curr - delta < hi)) {
            theta_upd <- theta_curr - delta
          } else if ((theta_curr - delta < lo) ||
                     (theta_curr - delta > hi)) {
            if (l1_curr * l1(lo) < 0) {
              theta_upd <- (theta_curr + lo) / 2
            }
            else if (l1_curr * l1(hi) < 0) {
              theta_upd <- (theta_curr + hi) / 2
            } else {
              # if all three derivatives (min, curr, max) have the same
              # signs, use the grid search
              l1_grid <- matrix(NA, length(theta_grid), 1)
              for (g in 1:length(theta_grid)) {
                l1_grid[g] <- l1(theta_grid[g])
              }
              theta_upd <- theta_grid[abs(l1_grid) == min(abs(l1_grid))]
            }
          }
          theta_curr <- theta_upd
          l1_curr <- l1(theta_curr)
        }
        theta_hat <- theta_curr
        SE <- 1 / sqrt(-l2(theta_curr))
      }
      c(theta_hat, SE)
    }
    score.map<-function (ppar_init)
    {
      th_init  = ppar_init[1]
      tau_init = ppar_init[2]
      
      map<- c(th_init, tau_init)
      # Newton-Raphson with an initial vector of [th1, th2]
      N = 1000
      eps = 1.0e-3
      nitr = 0 
      while ( N > 0 ){
        
        # [1] Restrain th_try & tau_try within truncate
        # initial estimates are obtained from previous test set/items
        if ( lo < th_init && th_init < hi ){
          th_try = th_init } else if  (th_init <= lo) {
            th_try = lo} else if (th_init >= hi) {
              th_try = hi} 
        
        if ( lo < tau_init && tau_init < hi ){ 
          tau_try = tau_init } else if (tau_init <= lo){
            tau_try = lo }else if (tau_init >= hi){
              tau_try = hi}
        
        # [2] first & second partial derivatives of posterior density
        # (partial) derivative of the natural log of posterior density
        l1_binom =sum(a.par*wrc*dnorm(a.par*th_try-b.par)/pnorm(a.par*th_try-b.par))-
          sum(a.par*(numwords.p-wrc)*dnorm(a.par*th_try-b.par)/(1-pnorm(a.par*th_try-b.par)))-
          (vartau*th_try-rho*sqrt(vartau)*tau_try)/(vartau-rho^2*vartau)
        l1_lognorm = -sum(alpha.par^2*(lgsec10 -beta.par+tau_try))-(tau_try-rho*sqrt(vartau)*th_try)/(vartau-rho^2*vartau) 
        #  Hessian matrix
        lam11 = -1/(1-rho^2)+sum((a.par^2*wrc*(ddnorm(a.par*th_try-b.par)*pnorm(a.par*th_try-b.par)-dnorm(a.par*th_try-b.par)^2))/((pnorm(a.par*th_try-b.par))^2))-
          sum(((numwords.p-wrc)*a.par^2*(ddnorm(a.par*th_try-b.par)*(1-pnorm(a.par*th_try-b.par))+dnorm(a.par*th_try-b.par)^2))/((1-pnorm(a.par*th_try-b.par))^2))
        
        lam22 = -sum(alpha.par^2)-1/((1-rho^2)*vartau)
        lam12 = rho/((1-rho^2)*sqrt(vartau))
        # [3] obtain and evaluate the delta
        delta = solve(matrix(c(lam11, lam12, lam12, lam22),nrow=2,ncol=2), c(l1_binom, l1_lognorm)) 
        if ( abs( delta[1] ) < eps &&  abs(delta[2]) < eps )
        {
          map[1] = th_try 
          map[2] = tau_try 
          nitr = 500 - N
          break
        }else{
          th_init = th_try - delta[1]
          tau_init = tau_try - delta[2]
        }
        N = N - 1;
      }
      if (N == 0)
      { 
        map = c(th_init, tau_init) 
        message<-paste('MAP does not converge in replication',rep)
        model.file.name<-paste("mapnonconvergence",rep,".txt")
        write(x=message,file=model.file.name,append = FALSE)
        write(x=message,file=model.file.name,append = FALSE)
      }
      # standard error
      map_se = sqrt(diag(solve( -matrix(c(lam11, lam12, lam12, lam22),nrow=2,ncol=2), diag(2) )))
      return(rbind(map,map_se))
    }
    score.eap<-function (nquad)
    {
      # Nodes for numerical integration
      Q <- seq(-5, 5, length = nquad)
      eap_step <- Q[2] - Q[1]
      nnode   = nquad^2 
      nodes   = matrix(rep(0,nnode*2),nnode,2)
      for (k in 1 : nquad){
        nodes[(nquad * (k - 1) + 1) : (nquad * k), 1] = -5 + eap_step * (k - 1) 
        nodes[(nquad * (k - 1) + 1) : (nquad * k), 2] = seq(-5, 5, length = nquad)
      }
      mu_p<-c(0,0)
      cov_p<- matrix(c(vartau,1,1,vartau),2,2)
      weight <- dmvnorm(nodes, mu_p, cov_p)
      weight <-  weight/sum(weight)
      # "pr" : [nnode, nitem]
      nitem  = length(wrc) 
      rdisc = rep(a.par, each=nnode) #replicate a.par (length = nitem) nnode times
      rint  = rep(b.par, each=nnode) 
      rresponse = rep(wrc, each=nnode) 
      rnumwords = rep(numwords.p, each=nnode) 
      rnode_th = rep(nodes[,1], nitem) 
      
      pr = matrix(dbinom(rresponse, rnumwords, pnorm(rdisc*rnode_th - rint)),ncol = nitem)
      like_binom = apply(pr,1,prod) 
      
      ralpha = rep(alpha.par, each=nnode)
      rbeta = rep(beta.par, each=nnode)
      rnode_tau = rep(nodes[,2], nitem)
      rlgsec10 = rep(lgsec10, each=nnode)
      rlgsec = rep(lgsec, each=nnode)
      rt<-matrix(dnorm(rlgsec10,rbeta-rnode_tau,1/ralpha),ncol = nitem)
      #rt<-matrix(dnorm(rlgsec,rbeta+log(numwords.p) - log(10)-rnode_tau,1/ralpha),ncol = nitem)
      like_rt = apply(rt, 1,prod)
      like = like_binom* like_rt
      
      numer1 = apply(nodes* cbind( like * weight, like * weight) ,2,sum) 
      denom1 = apply(cbind( like * weight, like * weight),2,sum ) 
      eap = numer1 / denom1 
      
      numer2 = sum((nodes[,1]-eap[1])^2* like * weight) 
      denom2 = sum (like * weight) 
      eap_se_th = sqrt(numer2/ denom2)
      numer3 = sum((nodes[,2]-eap[2])^2* like * weight) 
      denom3 = sum (like * weight) 
      eap_se_tau = sqrt(numer3/ denom3)
      eap_se<-c(eap_se_th,eap_se_tau)
      
      numer4 = sum((nodes[,1]-eap[1])*(nodes[,2]-eap[2])* like * weight) 
      denom4 = sum (like * weight) 
      eap_cov = numer4/ denom4
      return(c(eap,eap_se,eap_cov))
    }
    
    ######Start estimating####
    theta.mle <- Inf # for perfect case
    eta <- NA
    se.theta.mle <- NA
    wrc.mle0 <- NA
    wcpm.mle0 <- NA
    k.theta0 <- NA
    se.wcpm.mle0 <- NA
    secs.mle0 <- NA
    
    # MLE for tau & theta
    tau.mle <- sum(alpha.par^2*(beta.par - lgsec10))/sum(alpha.par^2)
    se.tau.mle <- sum(alpha.par^2)^(-0.5)
    perfect.cases <- c(t(get.perfectcases(stu.data)))
    case_split <- matrix(str_split(perfect.cases, "_",simplify = TRUE),ncol = 2)
    if (!(case %in% case_split[,1])) {
      out.mle<-score.mle(0.5)
      theta.mle <- out.mle[1]
      se.theta.mle <- out.mle[2]
      
      if (is.null(external)) { #internal
        secs.mle0 <- sum(exp(beta.par - log(10) + log(numwords.p) - tau.mle + ((1/alpha.par)^2)/2))
        wrc.mle0 <- sum(numwords.p*pnorm(a.par*theta.mle - b.par))
        wcpm.mle0 <- wrc.mle0/secs.mle0*60
        k.theta0 <- sum(a.par*numwords.p*dnorm( a.par*theta.mle - b.par ))/sum(numwords.p*pnorm( a.par*theta.mle - b.par ))
        se.wcpm.mle0 <- wcpm.mle0*(k.theta0^2*se.theta.mle^2 + se.tau.mle^2)^0.5
      } else {
        # if external, will calculate with external a, b, alpha, and beta
        secs.mle0 <- sum(exp(beta.par.external - log(10) + log(numwords.p.external) - tau.mle + ((1/alpha.par.external)^2)/2))
        wrc.mle0 <- sum(numwords.p.external*pnorm(a.par.external*theta.mle - b.par.external))
        wcpm.mle0 <- wrc.mle0/secs.mle0*60
        k.theta0 <- sum(a.par.external*numwords.p.external*dnorm( a.par.external*theta.mle - b.par.external ))/sum(numwords.p.external*pnorm( a.par.external*theta.mle - b.par.external ))
        se.wcpm.mle0 <- wcpm.mle0*(k.theta0^2*se.theta.mle^2 + se.tau.mle^2)^0.5
      }
    }
    if (Estimator == "mle") {
      out <- tibble(student.id=case, grade=grade,
                    n.pass=n.pass, numwords.total=numwords.total,
                    wrc.obs, secs.obs, wcpm.obs,
                    tau.mle,
                    theta.mle,
                    se.tau.mle,
                    se.theta.mle,
                    wrc.mle=wrc.mle0, secs.mle=secs.mle0,
                    n.pass.wcpm,numwords.total.wcpm,
                    wcpm.mle=wcpm.mle0, se.wcpm.mle=se.wcpm.mle0)
      return(out)
    } else if (Estimator == "map") {
      ests.map <- NA
      in.vals <- c(max(-5,min(5,theta.mle)),max(-5*sqrt(vartau),min(5*sqrt(vartau),tau.mle)))
      out.map<- score.map(in.vals)
      ests.map <- out.map[1,]
      # MAP Standard Errors for tau and theta
      se.tau.map <- out.map[2,2]
      eta <- a.par*ests.map[1] - b.par
      se.theta.map <-out.map[2,1]
      # MAP WCPM score
      if (is.null(external)) { #internal
        wrc.map <- sum(numwords.p*pnorm(a.par*ests.map[1] - b.par))
        secs.map <- sum(exp(beta.par - log(10) + log(numwords.p) - ests.map[2] + ((1/alpha.par)^2)/2))
        wcpm.map <- wrc.map/secs.map*60
        k.theta.map <- sum(a.par*numwords.p*dnorm( a.par*ests.map[1] - b.par ))/sum(numwords.p*pnorm( a.par*ests.map[1] - b.par ))
        se.wcpm.map <- wcpm.map*(k.theta.map^2*se.theta.map^2 + se.tau.map^2)^0.5
      } else {
        # if external, will calculate with external a, b, alpha, and beta
        wrc.map <- sum(numwords.p.external*pnorm(a.par.external*ests.map[1] - b.par.external))
        secs.map <- sum(exp(beta.par.external - log(10) + log(numwords.p.external) - ests.map[2] + ((1/alpha.par.external)^2)/2))
        wcpm.map <- wrc.map/secs.map*60
        k.theta.map <- sum(a.par.external*numwords.p.external*dnorm( a.par.external*ests.map[1] - b.par.external ))/sum(numwords.p.external*pnorm( a.par.external*ests.map[1] - b.par.external ))
        se.wcpm.map <- wcpm.map*(k.theta.map^2*se.theta.map^2 + se.tau.map^2)^0.5
      }
      # End of MAP estimation
      
      out <- tibble(student.id=case, grade=grade,
                    n.pass=n.pass, numwords.total=numwords.total,
                    wrc.obs, secs.obs, wcpm.obs,
                    tau.map=ests.map[2],
                    theta.map=ests.map[1],
                    # add these two columns, similar to EAP output
                    se.tau.map=se.tau.map,
                    se.theta.map=se.theta.map,
                    wrc.map, secs.map,
                    n.pass.wcpm,numwords.total.wcpm,
                    wcpm.map, se.wcpm.map)
      return(out)
    } else if (Estimator == "eap") {
      eap.result<-score.eap(nquad = q)
      ests.quad <- eap.result[c(1,2)]
      se.quad <- eap.result[c(3,4)]
      cov.quad<-eap.result[5]
      # QUAD WCPM score
      if (is.null(external)) { #internal
        wrc.quad <- sum(numwords.p*pnorm(a.par*ests.quad[1] - b.par))
        secs.quad <- sum(exp(beta.par - log(10) + log(numwords.p) - ests.quad[2] + ((1/alpha.par)^2)/2))
        wcpm.quad <- wrc.quad/secs.quad*60
        k.theta.quad <- sum(a.par*numwords.p*dnorm( a.par*ests.quad[1] - b.par ))/sum(numwords.p*pnorm( a.par*ests.quad[1] - b.par ))
        se.wcpm.quad <- wcpm.quad*(k.theta.quad^2*se.quad[1]^2 + se.quad[2]^2)^0.5
      } else {
        # if external, will calculate with external a, b, alpha, and beta
        wrc.quad <- sum(numwords.p.external*pnorm(a.par.external*ests.quad[1] - b.par.external))
        secs.quad <- sum(exp(beta.par.external - log(10) + log(numwords.p.external) - ests.quad[2] + ((1/alpha.par.external)^2)/2))
        wcpm.quad <- wrc.quad/secs.quad*60
        k.theta.quad <- sum(a.par.external*numwords.p.external*dnorm( a.par.external*ests.quad[1] - b.par.external ))/sum(numwords.p.external*pnorm( a.par.external*ests.quad[1] - b.par.external ))
        se.wcpm.quad <- wcpm.quad*(k.theta.quad^2*se.quad[1]^2 + se.quad[2]^2)^0.5
      }
      # End of EAP
      
      out <- tibble(student.id=case, grade=grade,
                    n.pass=n.pass, numwords.total=numwords.total,
                    wrc.obs, secs.obs, wcpm.obs,
                    tau.eap = ests.quad[2],
                    theta.eap = ests.quad[1],
                    se.tau.eap = se.quad[2],
                    se.theta.eap = se.quad[1],
                    cov.eap = cov.quad,
                    wrc.eap = wrc.quad,
                    secs.eap = secs.quad,
                    n.pass.wcpm,numwords.total.wcpm,
                    wcpm.eap = wcpm.quad,
                    se.wcpm.eap = se.wcpm.quad)
      return(out)
    }
  }
  
  get.cases <- function(data) {
    cases <- data %>% select(student.id,occasion) %>% unique() %>%
      unite("cases", student.id:occasion, sep = "_", remove = TRUE, na.rm = FALSE) %>%
      select(cases)
    print(cases)
    return(invisible(cases))
  }
  
  # pass.param <-
  #   calib_data$pass.param %>%
  #   dplyr::select(passage.id, nwords.p, a, b, alpha, beta)
  #an error in pass.param dataset
  # pass.param[which(pass.param$passage.id=="33003"),2]<-86
  
  # passage.list <-
  #   person_data %>% dplyr::select(id.passage) %>% unique() %>% dplyr::rename(passage.id = id.passage)
  # 
  # ppp <- pass.param %>% semi_join(passage.list)
  # p.sets <-
  #   list(
  #     ppp %>% filter(passage.id==32016 | passage.id==33003),
  #     ppp %>% filter(passage.id==32015 | passage.id==32016 |
  #                      passage.id==33003 | passage.id==33037),
  #     ppp %>% filter(passage.id==32010 | passage.id==32012 |
  #                      passage.id==32015 | passage.id==32016 |
  #                      passage.id==33003 | passage.id==33037),
  #     ppp %>% filter(passage.id==32010 | passage.id==32012 |
  #                      passage.id==32015 | passage.id==32016 |
  #                      passage.id==33003 | passage.id==33037 |
  #                      passage.id==33040 | passage.id==42055 ),
  #     ppp %>% filter(passage.id==23019 | passage.id==32004 |
  #                      passage.id==32010 | passage.id==32012 |
  #                      passage.id==32015 | passage.id==32016 |
  #                      passage.id==33003 | passage.id==33037 |
  #                      passage.id==33040 | passage.id==42055 ),
  #     ppp)
  
  #use 6 passages to demonstrate the ORF information functions
  # prms<-p.sets[[3]]
  
  prms <- calib_data
  
  task_size <- length(prms$task.param$task.id)
  
  hyperprms<-calib_data$hyper.param
  vartau<-hyperprms$vartau
  rho<-hyperprms$rho
  
  #plot theta/tau item-level and test-level information functions
  th_try <- seq(-3, 3, 0.01)
  pdf("thetaiteminf.pdf", width = 15, height = 10, family = "serif")
  row.num<-floor(task_size/3)+1*(task_size%%3>0)
  lmat <- matrix(c(1:task_size,rep(0,3*row.num-task_size)), floor(task_size/3)+1*(task_size%%3>0), 3, byrow = T)
  lmat <- rbind( cbind(0, lmat), 0)
  layout(lmat, c( 0.2, rep(1, 3) ), c(rep(1, floor(task_size/3)+1*(task_size%%3>0)), 0.2) )
  par( mar = c(0.5, 0.5, 0.5, 0.5), mgp = c(1.1, 0.5, 0), tcl = -0.2 )
  test.info <- numeric(length(th_try))
  for (j in 1:task_size)
  {
    a.par<-prms$task.param$a[j]
    b.par<-prms$task.param$b[j]
    amax<-max(prms$task.param$a)
    numwords.p<-prms$task.param$max.counts[j]
    wrcmax<-numwords.p*(pnorm(amax*th_try-b.par))
    ymax<-max(-{-1/(1-rho^2)+((amax^2*wrcmax*(ddnorm(amax*th_try-b.par)*pnorm(amax*th_try-b.par)-dnorm(amax*th_try-b.par)^2))/((pnorm(amax*th_try-b.par))^2))-
        (((numwords.p-wrcmax)*amax^2*(ddnorm(amax*th_try-b.par)*(1-pnorm(amax*th_try-b.par))+dnorm(amax*th_try-b.par)^2))/((1-pnorm(amax*th_try-b.par))^2))})
    plot(0, 0, type = 'n', xlim = c(-3, 3), ylim = c(0,ymax+5),
         xlab = '', ylab = '', axes = F)
    wrc<-numwords.p*(pnorm(a.par*th_try-b.par))
    points(th_try, item.info <- -{-1/(1-rho^2)+((a.par^2*wrc*(ddnorm(a.par*th_try-b.par)*pnorm(a.par*th_try-b.par)-dnorm(a.par*th_try-b.par)^2))/((pnorm(a.par*th_try-b.par))^2))-
        (((numwords.p-wrc)*a.par^2*(ddnorm(a.par*th_try-b.par)*(1-pnorm(a.par*th_try-b.par))+dnorm(a.par*th_try-b.par)^2))/((1-pnorm(a.par*th_try-b.par))^2))}, type = 'l', col = "black")
    test.info <- test.info + item.info
    text(3, ymax, paste("Passage", j), pos = 2, cex = 1.5)
    box()
    # x axis
    if (j > task_size-3)
    {
      axis(1, at = c(-3, -2, -1, 0, 1, 2, 3), cex.axis = 1.5)
      mtext(expression( paste("Latent variable ", theta) ), 1, line = 2, cex = 1.5)
    }
    # y axis
    if (j %% 3 == 1)
    {
      axis(2, at = c(0, 5,10,15,20,25,30,35,40,45,50,55,60,65), cex.axis = 1.5)
      mtext("Information", 2, line = 2, cex = 1.5)
    }
  }
  dev.off()
  
  pdf("thetatestinfo.pdf", width = 6, height = 3.5, family = "serif")
  par( mar = c(2, 2, 0.5, 2), mgp = c(1.1, 0.3, 0), tcl = -0.2 )
  plot(th_try, test.info, type = 'l', xlim = c(-3, 3), ylim = c(0,max(test.info)+5),
       xlab = '', ylab = '', lwd = 1.5)
  mtext(expression( paste("Latent variable ", theta) ), 1, line = 1.1, cex = 0.9)
  mtext("Information", 2, line = 1.1, cex = 0.9)
  par(new = T)
  plot(th_try, test.info^(-0.5), type = 'l', xlim = c(-3, 3), ylim = c(0,max(test.info^(-0.5))+0.5),
       xlab = '', ylab = '', lwd = 1.5, lty = 2, axes = F)
  
  axis(4)
  mtext("SE", 4, line = 1.1, cex = 0.9)
  legend(1, max(test.info^(-0.5))+0.2, c("Information", "SE"), lty = 1:2, bty = 'n')
  dev.off()
  
  
  tau_try <- seq(-1, 1, 0.01)
  pdf("tauiteminfo.pdf", width = 15, height = 10, family = "serif")
  row.num<-floor(task_size/3)+1*(task_size%%3>0)
  lmat <- matrix(c(1:task_size,rep(0,3*row.num-task_size)), floor(task_size/3)+1*(task_size%%3>0), 3, byrow = T)
  lmat <- rbind( cbind(0, lmat), 0)
  layout(lmat, c( 0.2, rep(1, 3) ), c(rep(1, floor(task_size/3)+1*(task_size%%3>0)), 0.2) )
  par( mar = c(0.5, 0.5, 0.5, 0.5), mgp = c(1.1, 0.5, 0), tcl = -0.2 )
  test.info <- numeric(length(tau_try))
  for (j in 1:task_size)
  {
    alpha.par<-prms$task.param$alpha[j]
    ymax<-max(prms$task.param$alpha)^2+1/((1-rho^2)*vartau)+5
    plot(0, 0, type = 'n', xlim = c(-1, 1), ylim = c(0,ymax),
         xlab = '', ylab = '', axes = F)
    
    points(tau_try, item.info <- rep((alpha.par^2)+1/((1-rho^2)*vartau),length(tau_try)), type = 'l', col = "black")
    test.info <- test.info + item.info
    text(1, 5, paste("Passage", j), pos = 2, cex = 1.5)
    box()
    # x axis
    if (j> task_size-3)
    {
      axis(1, at = c(-1, -0.5,  0, 0.5, 1), cex.axis = 1.5)
      mtext(expression( paste("Latent variable ", tau) ), 1, line = 2, cex = 1.5)
    }
    # y axis
    if (j %% 3 == 1)
    {
      axis(2, at = seq(0,ymax,5), cex.axis = 1.5)
      mtext("Information", 2, line = 2, cex = 1.5)
    }
  }
  dev.off()
  
  pdf("tautestinfo.pdf", width = 6, height = 3.5, family = "serif")
  par( mar = c(2, 2, 0.5, 2), mgp = c(1.1, 0.3, 0), tcl = -0.2 )
  plot(tau_try, test.info, type = 'l', xlim = c(-1, 1), ylim = c(0, max(test.info)+5),
       xlab = '', ylab = '', lwd = 1.5)
  mtext(expression( paste("Latent variable ", tau) ), 1, line = 1.1, cex = 0.9)
  mtext("Information", 2, line = 1.1, cex = 0.9)
  par(new = T)
  plot(tau_try, test.info^(-0.5), type = 'l', xlim = c(-1, 1), ylim = c(0, max(test.info^(-0.5))+1),
       xlab = '', ylab = '', lwd = 1.5, lty = 2, axes = F)
  
  axis(4)
  mtext("SE", 4, line = 1.1, cex = 0.9)
  legend(0.3, max(test.info^(-0.5))+0.5, c("Information", "SE"), lty = 1:2, bty = 'n')
  dev.off()
  
  #plot WCPM information
  prms$b_irt<-prms$task.param$b/prms$task.param$a
  prms$beta_irt<-prms$task.param$beta+log(prms$task.param$max.counts/10)
  
  nquad = 25
  Q <- seq(-3, 3, length = nquad)
  eap_step <- Q[2] - Q[1]
  nnode   = nquad^2 
  nodes   = matrix(rep(0,nnode*2),nnode,2)
  for (k in 1 : nquad){
    nodes[(nquad * (k - 1) + 1) : (nquad * k), 1] = -3 + eap_step * (k - 1) 
    nodes[(nquad * (k - 1) + 1) : (nquad * k), 2] = seq(-1, 1, length = nquad)
  }
  
  theta <- nodes[,1]
  tau <- nodes[,2]
  
  N <- nrow(nodes) 
  I <- nrow(prms$task.param)
  
  a <- prms$task.param$a; a_rep <- rep(a, each=N)
  b <- prms$b_irt; b_rep <- rep(b, each=N)
  alpha <- prms$task.param$alpha; alpha_rep <- rep(alpha, each=N)
  beta <- prms$beta_irt; beta_rep <- rep(beta, each=N)
  nwords <- prms$task.param$max.counts; nwords_rep <- rep(nwords, each=N)
  nwords_matrix<-matrix(rep(nwords,N),nrow = N,byrow = TRUE)
  
  theta_rep <- rep(theta, I)  
  tau_rep <- rep(tau, I)
  
  set.seed(1)
  mu_time <- beta_rep - tau_rep
  sd_time <- 1/alpha_rep
  time_data <- rnorm(n=N*I, mean=mu_time, sd=sd_time)
  time_mat <- matrix(time_data, nc=I)
  time10_mat <- time_mat - log(nwords_matrix) + log(10)
  
  prb <- pnorm(a_rep*(theta_rep-b_rep))
  respon <- rbinom(n = N*I, size = nwords_rep, prob = prb)
  respon_mat <- matrix(respon, nc=I)
  
  time_mat.l<-time_mat %>% 
    exp()%>%
    as.data.frame()%>%
    pivot_longer(cols = contains("V"), names_to = "secs")
  
  logtime_mat.l<-time_mat %>% 
    as.data.frame()%>%
    pivot_longer(cols = contains("V"), names_to = "lgsec")
  
  respon_mat.l<-respon_mat %>% 
    as.data.frame()%>%
    pivot_longer(cols = contains("V"), names_to = "wrc")
  
  stu.data <- cbind.data.frame(rep(c(1:N),each=I),rep(c(1:I),N),rep(nwords,N),rep("fall",I*N),rep(3,I*N),respon_mat.l$value,time_mat.l$value,logtime_mat.l$value)
  colnames(stu.data )<-c("student.id","passage.id","numwords.p","occasion","grade", "wrc","sec","lgsec")
  
  cases<-as.data.frame(seq(1:length(theta)))
  colnames(cases)<-"cases"
  external=NULL
  seq_id_all <- nrow(cases)
  pass.data<-cbind.data.frame(a,a*b,alpha,beta- log(nwords) + log(10),c(1:I),nwords)
  colnames(pass.data)<-c("a" , "b" , "alpha", "beta", "passage.id", "numwords.p")
  
  theta.tau <- foreach(i=1:seq_id_all, .combine = "rbind") %do%
    { est.theta.tau(stu.data,
                    pass.data,
                    cases$cases[i],
                    lo=-4, hi=4, q=150, external=external,rho = 0.01,vartau = 100,Estimator = "map")
    }
  
  # # Initialize an empty list to store results
  # theta.tau <- list()
  # 
  # # Loop through each index i from 1 to seq_id_all
  # for (i in 1:seq_id_all) {
  #   # Call est.theta.tau function and store result in current iteration index of the list
  #   theta.tau[[i]] <- est.theta.tau(stu.data,
  #                                        pass.data,
  #                                        cases$cases[i],
  #                                        lo = -4, hi = 4, q = 150,
  #                                        external = external, rho = 0.01, vartau = 100,
  #                                        Estimator = "map")
  # }
  
  jacob.a<-jacob.b<-jacob.alpha<-jacob.beta<-rep(NA,I)
  wcpm.se<-wcpm<-matrix(NA,nrow=N,ncol = I)
  mu<-nu<-jacob.theta<-jacob.tau<-rep(NA,I)
  jacob.vec<-matrix(NA,nrow=6,ncol = I)
  n.row<-4+2
  n.col<-4+2
  CV_hard<-matrix(0,nrow = n.row,ncol= n.col)
  
  #delta method is based on 1/alpha
  pass.data$alpha<-1/pass.data$alpha
  for (j in 1:N){
    for (i in 1:I){
      mu[i]<-pnorm(pass.data$a[i]*theta.tau$theta.map[j]-pass.data$b[i])
      nu[i]<-exp(pass.data$beta[i]+log(nwords[i]) - log(10)-theta.tau$tau.map[j]+pass.data$alpha[i]^2/2)
      jacob.a[i]<-nwords[i]*dnorm(pass.data$a[i]*theta.tau$theta.map[j]-pass.data$b[i])*theta.tau$theta.map[j]*60/sum(nu[i])
      jacob.b[i]<- -nwords[i]*dnorm(pass.data$a[i]*theta.tau$theta.map[j]-pass.data$b[i])*60/sum(nu[i])
      jacob.alpha[i]<- -sum(nwords[i]*mu[i])*60*nu[i]*pass.data$alpha[i]/((sum(nu[i]))^2)
      jacob.beta[i]<- -sum(nwords[i]*mu[i])*60*nu[i]/((sum(nu[i]))^2)
      jacob.theta[i]<-60/sum(nu[i])*sum(nwords[i]*dnorm(pass.data$a[i]*theta.tau$theta.map[j]-pass.data$b[i])*pass.data$a[i])
      jacob.tau[i]<-sum(nwords[i]*mu[i])*60/(sum(nu[i]))
      jacob.vec[,i]<-c(jacob.a[i],jacob.b[i],jacob.alpha[i],jacob.beta[i],jacob.theta[i],jacob.tau[i])
      
      CV_hard[(4+1),(4+1)]<-(theta.tau$se.theta.map[j])^2
      CV_hard[(4+2),((4+2))]<-(theta.tau$se.tau.map[j])^2
      
      wcpm.se[j,i]<-sqrt(t(jacob.vec[,i])%*%CV_hard%*%jacob.vec[,i])
      
      wcpm[j,i]<-sum(nwords[i]*mu[i])*60/sum(nu[i])
    } 
  }
  delta<-cbind(wcpm,wcpm.se)
  result<-list()
  result[[1]]<-theta.tau
  result[[2]]<-delta
  
  #WCPM information
  wcpm<-result[[2]]
  z<-wcpm[,c(1:I)]
  wcpmSE<-wcpm[,c((I+1):(2*I))]
  wcpmInfo<-(wcpmSE)^(-2)
  
  which_item<-1
  Value<-wcpmInfo[,which_item]
  plot_obj <- plot_ly(x=~theta, y=~tau, z=~Value, type = "scatter3d",color=~(Value),mode="markers")%>%
    layout( scene = list(xaxis = list(title = "Theta"), yaxis = list(title = "Tau"), zaxis = list(title = paste("Passage",which_item, "WCPM Information"))))
  plot_obj
}