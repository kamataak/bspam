# Internal cache for compiled Stan models
.bspam_stan_model_cache <- new.env(parent = emptyenv())

get_stan_model <- function(model_name) {
  model_map <- list(
    # Passage-level scoring models
    model_multi_obs_multi_cens = testlet_scoring_multi_obs_multi_cens,
    model_multi_obs_one_cens   = testlet_scoring_multi_obs_one_cens,
    model_one_obs_multi_cens   = testlet_scoring_one_obs_multi_cens,
    model_multi_obs_no_cens    = testlet_scoring_multi_obs_no_cens,
    model_no_obs_multi_cens    = testlet_scoring_no_obs_multi_cens,
    model_one_obs_one_cens     = testlet_scoring_one_obs_one_cens,
    
    # Sentence-level scoring models
    model_multi_obs_multi_cens_sentence = testlet_scoring_multi_obs_multi_cens_sentence,
    model_multi_obs_one_cens_sentence   = testlet_scoring_multi_obs_one_cens_sentence,
    model_one_obs_multi_cens_sentence   = testlet_scoring_one_obs_multi_cens_sentence,
    model_multi_obs_no_cens_sentence    = testlet_scoring_multi_obs_no_cens_sentence,
    model_no_obs_multi_cens_sentence    = testlet_scoring_no_obs_multi_cens_sentence,
    model_one_obs_one_cens_sentence     = testlet_scoring_one_obs_one_cens_sentence
  )
  
  if (!model_name %in% names(model_map)) {
    stop(
      "Unknown Stan model name: '", model_name, "'. ",
      "Available models are: ",
      paste(names(model_map), collapse = ", "),
      call. = FALSE
    )
  }
  
  if (!exists(model_name, envir = .bspam_stan_model_cache, inherits = FALSE)) {
    message(
      "Compiling Stan model '", model_name,
      "' for the first time. This may take a few minutes. ",
      "Later calls in this R session will reuse the compiled model."
    )
    
    rstan::rstan_options(auto_write = TRUE)
    
    compiled_model <- rstan::stan_model(
      model_code = model_map[[model_name]],
      model_name = model_name
    )
    
    assign(model_name, compiled_model, envir = .bspam_stan_model_cache)
  }
  
  get(model_name, envir = .bspam_stan_model_cache, inherits = FALSE)
}