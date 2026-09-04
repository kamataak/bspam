# Global variables used in non-standard evaluation
# These names are primarily data-frame column names referenced
# inside dplyr/tidyverse pipelines.
utils::globalVariables(c(
  
  # bayes() / bayes.wcpm()
  "Parameter",
  "Mean",
  "SD",
  "Lower95",
  "Upper95",
  "SSeff",
  "psrf",
  "2.5%",
  "97.5%",
  "n_eff",
  "Rhat",
  "ESS",
  "Mean_tau",
  "Mean_theta",
  "SD_tau",
  "SD_theta",
  "Lower95_theta",
  "Upper95_theta",
  "Lower95_tau",
  "Upper95_tau",
  "Mean_exp_cnt",
  "Mean_exp_tim",
  "Mean_wcpm",
  "SD_wcpm",
  "Lower95_wcpm",
  "Upper95_wcpm",
  "lgsec",
  "case_sel",
  "obs.counts.obs",
  "max.counts.total",
  "max.counts.total.wcpm",
  
  # desc.data()
  "pas.wcpm",
  "tot.obs.counts",
  "tot.time",
  "unq.subtask",
  "pas.obs.counts",
  "pas.time",
  "pas.sent",
  "pas.max.counts",
  
  # getBootstrapSE()
  "a",
  "b",
  "alpha",
  "se.tau.mle",
  "se.theta.mle",
  "bse.theta.mle",
  "bse.tau.mle",
  "bse.wcpm.mle",
  "tau.eap",
  "theta.eap",
  "se.tau.eap",
  "se.theta.eap",
  "obs.counts.eap",
  "secs.eap",
  "wcpm.eap",
  "se.wcpm.eap",
  "bse.theta.eap",
  "bse.tau.eap",
  "bse.wcpm.eap",
  "tau.map",
  "theta.map",
  "se.tau.map",
  "se.theta.map",
  "bse.theta.map",
  "bse.tau.map",
  "bse.wcpm.map",
  
  # plot.information()
  "a.par",
  "b.par",
  "Ru",
  "student.id",
  "numwords.p",
  "wrc_sum",
  "numwords.p_sum",
  "passage.id",
  "sec",
  
  # plot.person(), plot.task(), plot.wcpm()
  "par_grph",
  "se_grph",
  "se_grph_x",
  "se_grph_y",
  "par_grph_x",
  "par_grph_y",
  
  # prep()
  "obs_sequence",
  
  # run.scoring()
  "i"
))