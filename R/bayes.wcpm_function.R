##################################################################################################
################### THIS IS THE FUNCTION TO ESTIMATE MODEL-BASED WCPM PARAMETERS #################
##################################################################################################
#' Bayes function when running mcem with bayes setting
#' @param calib.data - fit.model class object
#' @param person.data - individual reading data
#' @param person.id The column name in the data that represents the unique individual identifier.
#' @param task.id The column name in the data that represents the unique task identifier.
#' @param max.counts The column name in the data that represents the number of words in a task.
#' @param occasion The column name in the data that represents the unique occasion.
#' @param group The column name in the data that represents the unique group.
#' @param obs.counts The column name in the data that represents the words read correctly for each case.
#' @param time The column name in the data that represents the time, in seconds, for each case.
#' @param cases - student id vectors, will directly use passage data if no calib.data provided
#' @param external - if not NULL, will use not student read passages for estimating
#' @param parallel parallel=T, #logical, run in parallel? "T" or "F"
#' @param type - output type, "general" and "orf", default "general" only output tau & theta. "orf" will output wcpm
#' @param n.chains int., number of the chains
#' @param iter int., number of the iterations after the burn-in period
#' @param burn int., number of the burn-in iteration
#' @param thin int, thinning interval, a.k.a, period of saving samples
#'
#' @import tibble
#' @return list
bayes.wcpm <- function(
    calib.data=NA,
    person.data=NA,
    person.id=NULL,
    task.id = NULL,
    occasion = NULL,
    group = NULL,
    max.counts = NULL,
    obs.counts = NULL,
    time = NULL,
    cases = NULL,
    external=NULL,
    type = NULL,
    parallel=T, #logical, run in parallel? "T" or "F"
    n.chains=NA, # pos. int., number of the chains
    iter=NA,  # pos. int., number of the iterations after the burn-in period
    burn=NA,  # pos. int., number of the burn-in iterations
    thin=1 #pos. int, thinning interval, a.k.a, period of saving samples
)
{
  
  log.initiating()
  flog.info("Begin wcpm process with bayes setting", name = "orfrlog")
  if (class(calib.data)[1] == "fit.model") {
    pass.data <- calib.data$task.param
  }
  else {
    flog.info("Missing fit.model object, end wcpm process", 
              name = "orfrlog")
    return(NULL)
  }
  if (is.null(c(person.id, task.id, occasion, group, max.counts, 
                obs.counts, time))) {
    person.data <- person.data %>% select(-lgsec)
    colnames(person.data) <- c("person.id", "task.id", "max.counts", 
                               "occasion", "group", "obs.counts", "time")
  }
  else {
    if (occasion == "") {
      person.data["occasion"] <- 1
      occasion = "occasion"
    }
    if (group == "") {
      person.data["group"] <- 1
      group = "group"
    }
    person.data <- person.data[, c(person.id, task.id, occasion, 
                                   group, max.counts, obs.counts, time)]
    colnames(person.data) <- c("person.id", "task.id", "occasion", 
                               "group", "max.counts", "obs.counts", "time")
  }
  if (is.null(cases)) {
    person.data <- person.data
  }
  else {
    person.data <- person.data %>% mutate(case_sel = paste(person.id, 
                                                           occasion, sep = "_")) %>% filter(case_sel %in% cases$cases) %>% 
      select(-case_sel)
  }
  stu_id <- person.data %>% select(person.id) %>% distinct()
  pas_param_read <- calib.data$task.param %>% filter(task.id %in% 
                                                       person.data$task.id) %>% arrange(task.id)
  desc_out <- person.data %>% rename(occasion = occasion) %>% 
    group_by(person.id, occasion, group) %>% summarise(task.n = n_distinct(task.id), 
                                                       max.counts.total = sum(max.counts), obs.counts.obs = sum(obs.counts), 
                                                       secs.obs = sum(time)) %>% ungroup() %>% mutate(wcpm.obs = obs.counts.obs/secs.obs * 
                                                                                                        60)
  desc_out <- stu_id %>% left_join(desc_out) %>% select(person.id, 
                                                        everything())
  time.data <- person.data %>% select(person.id, task.id, time) %>% 
    pivot_wider(names_from = task.id, values_from = time) %>% 
    tibble::column_to_rownames("person.id") %>% select(sort(colnames(.))) %>% 
    as.matrix()
  count.data <- person.data %>% select(person.id, task.id, 
                                       obs.counts) %>% pivot_wider(names_from = task.id, values_from = obs.counts) %>% 
    tibble::column_to_rownames("person.id") %>% select(sort(colnames(.))) %>% 
    as.matrix()
  n.words <- person.data %>% select(task.id, max.counts) %>% 
    arrange(task.id) %>% distinct() %>% deframe()
  if (is.null(external)) {
    nonmis_ind <- !is.na(time.data)
    pas_est_ind <- apply(nonmis_ind, 2, as.numeric)
    rownames(pas_est_ind) <- rownames(time.data)
    pas_param_est <- pas_param_read
    desc_out <- desc_out %>% mutate(task.n.wcpm = task.n, 
                                    max.counts.total.wcpm = max.counts.total)
  }
  else {
    pas_param_est <- calib.data$task.param %>% filter(task.id %in% 
                                                        external) %>% arrange(task.id)
    pas_est_ind <- matrix(1, nrow = nrow(time.data), ncol = length(external), 
                          dimnames = list(rownames(time.data), pas_param_est$task.id))
    desc_out <- desc_out %>% mutate(task.n.wcpm = n_distinct(pas_param_est$task.id), 
                                    max.counts.total.wcpm = sum(pas_param_est$max.counts))
  }
  J <- nrow(time.data)
  I <- ncol(time.data)
  K <- nrow(pas_param_est)
  if (type == "orf") {
    param_est <- c("exp_cnt", "exp_tim", "wcpm", "theta", "tau")
  }
  else {
    param_est <- c("exp_cnt", "exp_tim", "theta", "tau")
  }
  
  if (is.na(n.chains)) {
    n.chains <- min(max(4), detectCores() - 1)
  }
  else {
    n.chains <- min(max(4), n.chains)
  }
  inits <- vector(mode = "list", length = n.chains)
  for (i in 1:n.chains) {
    theta = rnorm(J)
    rng_names <- c("base::Wichmann-Hill", "base::Marsaglia-Multicarry", 
                   "base::Super-Duper", "base::Mersenne-Twister")
    if (n.chains <= length(rng_names)) {
      rng_name_sel <- rng_names[i]
    }
    else {
      rng_names_rep <- rep(rng_names, n.chains)
      rng_name_sel <- rng_names_rep[i]
    }
    gen_init <- list(theta = theta, .RNG.name = rng_name_sel, 
                     .RNG.seed = i)
    inits[[i]] <- gen_init
  }
  time.mis <- T %in% is.na(time.data)
  count.mis <- T %in% is.na(count.data)
  if (time.mis == T | count.mis == T) {
    bayes.soft = "jags"
    cat("==== Running the analyses with JAGS ==== \n \n")
  }
  else {
    bayes.soft = "stan"
    cat("==== Running the analysis with STAN ==== \n \n")
  }
  if (bayes.soft == "jags") {
    time.data <- time.data %>% as.vector()
    count.data <- count.data %>% as.vector()
    ind.per <- rep(1:J, I)
    ind.pas <- rep(1:I, each = J)
    nw <- rep(n.words, each = J)
    mis.time.loc <- which(is.na(time.data))
    mis.count.loc <- which(is.na(count.data))
    time.data <- time.data[-mis.time.loc]
    count.data <- count.data[-mis.count.loc]
    nw <- nw[-mis.count.loc]
    n.obs <- length(time.data)
    ind.per.obs <- ind.per[-mis.time.loc]
    ind.pas.obs <- ind.pas[-mis.time.loc]
    runjags::runjags.options(force.summary = T)
    data.list <- list(J = J, K = K, tim = log(time.data), 
                      res = count.data, N = n.obs, ind_per = ind.per.obs, 
                      ind_pas = ind.pas.obs, nw_read = nw, a_read = pas_param_read$a, 
                      b_read = pas_param_read$b, alpha_read = pas_param_read$alpha, 
                      beta_raw_read = pas_param_read$beta + log(pas_param_read$max.counts/10), 
                      pas_est_ind = pas_est_ind, nw_est = pas_param_est$max.counts, 
                      a_est = pas_param_est$a, b_est = pas_param_est$b, 
                      alpha_est = pas_param_est$alpha, beta_raw_est = pas_param_est$beta + 
                        log(pas_param_est$max.counts/10), ptau = 1/calib.data$hyper.param$vartau, 
                      cvr = calib.data$hyper.param$rho * sqrt(calib.data$hyper.param$vartau))
    jags.syntax <- "\n    model{\n\n\n\n##### ++++++++++++++++++++++++++++\nfor(n in 1:N){\nres[n] ~ dbin(p[n], nw_read[n])\nprobit(p[n]) <- a_read[ind_pas[n]] * theta[ind_per[n]] - b_read[ind_pas[n]]\ntim[n] ~ dnorm(mu[n], pow(alpha_read[ind_pas[n]], 2))\nmu[n] <- beta_raw_read[ind_pas[n]] - tau[ind_per[n]]\n}\n\n\n# Estimation of WCPM for target passages\n    for(j in 1:J){\n    for(k in 1:K){\n      cnt_ex[j,k] <- (phi(a_est[k] * theta[j] - b_est[k]) * nw_est[k]) * pas_est_ind[j,k]\n      tim_ex[j,k] <- (exp(beta_raw_est[k] - tau[j] + 0.5 * 1/(pow(alpha_est[k],2))))* pas_est_ind[j,k]\n    }\n      theta[j] ~ dnorm(0,1)\n      tau[j] ~ dnorm(mtau[j], ptau)\n      mtau[j] <- cvr * theta[j]\n\n      exp_cnt[j] <- sum(cnt_ex[j,])\n      exp_tim[j] <- sum(tim_ex[j,])\n      wcpm[j] <- exp_cnt[j]/exp_tim[j]*60\n    }\n}\n"
    if (isTRUE(parallel)) {
      jags_meth <- "parallel"
    }
    else {
      jags_meth <- "rjags"
    }
    jags_out <- runjags::autorun.jags(model = jags.syntax, 
                                      monitor = param_est, data = data.list, n.chains = n.chains, 
                                      thin = thin, inits = inits, method = jags_meth, modules = "glm")
    par_est <- jags_out$summaries %>% as.data.frame() %>% 
      select(Mean, SD, Lower95, Upper95)
    rm(jags_out)
    gc()
  }
  else if (bayes.soft == "stan") {
    time.data <- time.data %>% t()
    count.data <- count.data %>% t()
    pas_est_ind <- pas_est_ind %>% t()
    
    nw_read = n.words
    a_read = pas_param_read$a 
    b_read = pas_param_read$b
    alpha_read = pas_param_read$alpha 
    beta_raw_read = pas_param_read$beta + log(pas_param_read$max.counts/10)
    pas_est_ind = pas_est_ind
    nw_est = pas_param_est$max.counts 
    a_est = pas_param_est$a
    b_est = pas_param_est$b 
    alpha_est = pas_param_est$alpha
    beta_raw_est = pas_param_est$beta + log(pas_param_est$max.counts/10)
    
    dim_read <- length(n.words)
    dim_est <- length(nw_est)
    
    dim(nw_read) = dim_read
    dim(a_read) = dim_read
    dim(b_read) = dim_read
    dim(alpha_read) = dim_read
    dim(beta_raw_read) = dim_read
    
    #dim(pas_est_ind) = dim_est
    dim(nw_est) = dim_est
    dim(a_est) = dim_est
    dim(b_est) = dim_est
    dim(alpha_est) = dim_est
    dim(beta_raw_est) = dim_est
    
    data.list <- list(J = J, 
                      I = I, 
                      K = K, 
                      tim = log(time.data), 
                      res = count.data, 
                      nw_read = nw_read, 
                      a_read = a_read, 
                      b_read = b_read, 
                      alpha_read = alpha_read, 
                      beta_raw_read = beta_raw_read, 
                      pas_est_ind = pas_est_ind, 
                      nw_est = nw_est, 
                      a_est = a_est, 
                      b_est = b_est, 
                      alpha_est = alpha_est, 
                      beta_raw_est = beta_raw_est, 
                      stau = sqrt(calib.data$hyper.param$vartau), 
                      cvr = calib.data$hyper.param$rho * sqrt(calib.data$hyper.param$vartau))
    stan.syntax <- "\ndata{\n// Data\n  int <lower=0> J; //number of individuals\n  int <lower=0> I; //number of passages read\n  int <lower=0> K; //number of passages external\n  int <lower=0> res[I,J]; //array of counts\n  real tim[I,J]; //array of times\n  real pas_est_ind[K, J]; //array of passage indicators\n  int <lower=0> nw_read[I]; //vector of number of the words per passage\n  int <lower=0> nw_est[K]; //vector of number of the words per passage\n\n// Known Parameters\n  real <lower=0> stau; // SD of tau\n  real cvr; // Covariance between theta and tau\n\n  vector <lower=0> [I] alpha_read; //time discrimintion\n  vector[I] beta_raw_read; //time intensity\n  vector <lower=0> [I] a_read; //accuracy discrimination\n  vector[I] b_read; //accuracy difficulty (threshold style)\n\n  vector <lower=0> [K] alpha_est; //time discrimintion\n  vector[K] beta_raw_est; //time intensity\n  vector <lower=0> [K] a_est; //accuracy discrimination\n  vector[K] b_est; //accuracy difficulty (threshold style)\n\n}\n\nparameters{\n  vector[J] theta; //accuracy ability\n  vector[J] tau; //speed ability\n}\n\n\nmodel{\n  // Priors\n  theta ~ normal(0, 1);\n\n  // Likelihood\nfor(i in 1:I){\n  res[i] ~ binomial(nw_read[i], Phi(a_read[i] * theta - b_read[i]));\n  tim[i] ~ normal(beta_raw_read[i] - tau, 1/alpha_read[i]);\n              }\n}\n\n// Estimation of model-based WCPM\ngenerated quantities{\n\n  real tim_ex[K,J]; //Expeced time matrix\n  real <lower=0> cnt_ex[K,J]; //Expected count matrix\n  vector <lower=0> [J] exp_cnt; //Model-based obs.counts\n  vector <lower=0> [J] exp_tim; //Model-based secs\n  vector <lower=0> [J] wcpm; //Model-based WCPM\n\n\n for(j in 1:J){\n    for(k in 1:K){real p; real mu;\n      p=Phi(a_est[k] * theta[j] - b_est[k]);\n      cnt_ex[k,j]=(p*nw_est[k]) * pas_est_ind[k, j];\n      mu=beta_raw_est[k] - tau[j];\n      tim_ex[k,j]=(exp(mu + 0.5 * 1/(square(alpha_est[k])))) * pas_est_ind[k, j];\n    }\n    exp_cnt[j]=sum(cnt_ex[,j]);\n    exp_tim[j]=sum(tim_ex[,j]);\n    wcpm[j]=exp_cnt[j]/exp_tim[j]*60;\n  }\n}\n\n"
    if (isTRUE(parallel)) {
      n.cores <- min(max(4), detectCores() - 1)
    }
    else {
      n.cores <- 1
    }
    stan_out <- rstan::stan(model_code = stan.syntax, pars = param_est, 
                            data = data.list, chains = n.chains, warmup = 1000, 
                            iter = 5000, thin = thin, cores = n.cores, init = inits, 
                            control = list(adapt_delta = 0.99))
    par_est <- rstan::summary(stan_out)$summary %>% as.data.frame() %>% 
      rownames_to_column(var = "Parameter") %>% filter(Parameter != 
                                                         "lp__") %>% select(Mean = mean, SD = sd, Lower95 = `2.5%`, 
                                                                            Upper95 = `97.5%`)
  }
  par_est <- par_est %>% 
    mutate(person.id = rep(desc_out$person.id, length(param_est)), 
           occasion = rep(desc_out$occasion, length(param_est)), 
           group = rep(desc_out$group, length(param_est)), 
           Parameter = rep(param_est, each = J))
  
  
  par_est_wide <- par_est %>% 
    pivot_wider(names_from = c("Parameter"), values_from = c("Mean", "SD", "Lower95", "Upper95"))
  
  if(type=="orf"){
    final_out <- desc_out %>% 
      left_join(par_est_wide) %>% 
      select(person.id, occasion, group, task.n, max.counts.total, obs.counts.obs, secs.obs, wcpm.obs,
             tau.est=Mean_tau, theta.est=Mean_theta, se.tau.est=SD_tau, se.theta.est=SD_theta,
             low.95.est.theta=Lower95_theta, up.95.est.theta=Upper95_theta,
             low.95.est.tau=Lower95_tau, up.95.est.tau=Upper95_tau, 
             obs.counts.est = Mean_exp_cnt, secs.est = Mean_exp_tim, 
             task.n.wcpm, max.counts.total.wcpm, 
             wcpm.est = Mean_wcpm, se.wcpm.est = SD_wcpm,
             low.95.est.wcpm=Lower95_wcpm, up.95.est.wcpm=Upper95_wcpm)
  }else{
    final_out <- desc_out %>% 
      left_join(par_est_wide) %>% 
      select(person.id, occasion, group, task.n, max.counts.total, obs.counts.obs, secs.obs, wcpm.obs,
             tau.est=Mean_tau, theta.est=Mean_theta, se.tau.est=SD_tau, se.theta.est=SD_theta,
             low.95.est.theta=Lower95_theta, up.95.est.theta=Upper95_theta,
             low.95.est.tau=Lower95_tau, up.95.est.tau=Upper95_tau, 
             obs.counts.est = Mean_exp_cnt, secs.est = Mean_exp_tim)
  }
  
  colnames(final_out) <- gsub(pattern = "est", x = colnames(final_out), 
                              replacement = bayes.soft)
  flog.info("End wcpm process with bayes setting", name = "orfrlog")
  final_out
}
