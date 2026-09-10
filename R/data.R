#' @title Passage-level Oral Reading Fluency assessment data set
#'
#' @description This is an example data set. It is a passage-level Oral Reading Fluency 
#' assessment data set for 85 students who were assigned to read 2 to 12 passages 
#' among the same 12 passages.
#' The data is a small subset of the data collected by Nese and Kamata (2014-2018).
#' 
#' @format A data frame with 847 rows and 7 variables.
#' \describe{
#'   \item{\code{id.student}}{unique student identifier}
#'   \item{\code{occasion}}{identifier for longitudinal assessment occasions;
#'   here a triannual assessment administered in the fall, winter, and spring of a school year}
#'   \item{\code{grade}}{student grade level}
#'   \item{\code{id.passage}}{unique passage identifier}
#'   \item{\code{numwords.pass}}{total number of words in the passage}
#'   \item{\code{wrc}}{words read correct}
#'   \item{\code{sec}}{seconds to read the passage}
#' }
#' @references 
#' Nese, J. F. T. & Kamata, A. (2014-2018). Measuring Oral Reading Fluency: 
#'      Computerized Oral Reading Evaluation (Project No. R305A140203) [Grant]. 
#'      Institute of Education Sciences, U.S. Department of Education. 
#' @source \url{https://jnese.github.io/core-blog/}
"passage2"

#' @title Task calibration example output object by MCEM
#'
#' @description This is an example calibration output object obtained using
#' the MCEM estimator in \code{fit.model}. The object contains calibrated
#' oral reading fluency passage parameters together with population-level
#' hyperparameters.
#'
#' @format A list of two elements: \code{$pass.param} is a data frame with
#' 150 rows and 10 variables, and \code{$hyper.param} is a data frame with
#' 1 row and 4 variables.
#'
#' \code{$pass.param}
#' \describe{
#'   \item{\code{a}}{parameter controlling binomial success probabilities}
#'   \item{\code{b}}{parameter controlling binomial success probabilities}
#'   \item{\code{alpha}}{parameter controlling reading times}
#'   \item{\code{beta}}{parameter controlling reading times}
#'   \item{\code{se_a}}{standard error of a}
#'   \item{\code{se_b}}{standard error of b}
#'   \item{\code{se_alpha}}{standard error of alpha}
#'   \item{\code{se_beta}}{standard error of beta}
#'   \item{\code{passage.id}}{passage ID}
#'   \item{\code{nwords.p}}{total number of words in the passage}
#' }
#'
#' \code{$hyper.param}
#' \describe{
#'   \item{\code{vartau}}{variance of latent speed ability tau}
#'   \item{\code{rho}}{correlation between the two latent variables}
#'   \item{\code{se_vartau}}{standard error of vartau}
#'   \item{\code{se_rho}}{standard error of rho}
#' }
#'
#' @source \url{https://jnese.github.io/core-blog/}
#'
#' @references
#' Nese, J. F. T. & Kamata, A. (2014-2018). Measuring Oral Reading Fluency:
#' Computerized Oral Reading Evaluation (Project No. R305A140203) [Grant].
#' Institute of Education Sciences, U.S. Department of Education.
"passage.calib.mcem"

#' @title Task calibration example output object by Bayes
#'
#' @description This is an example calibration output object obtained using
#' the Bayesian estimator in \code{fit.model}, with \code{est = "bayes"}.
#' The object contains calibrated oral reading fluency passage parameters
#' together with population-level hyperparameters.
#'
#' @format A list of two elements: \code{$pass.param} is a data frame with
#' 150 rows and 10 variables, and \code{$hyper.param} is a data frame with
#' 1 row and 4 variables.
#'
#' \code{$pass.param}
#' \describe{
#'   \item{\code{a}}{parameter controlling binomial success probabilities}
#'   \item{\code{b}}{parameter controlling binomial success probabilities}
#'   \item{\code{alpha}}{parameter controlling reading times}
#'   \item{\code{beta}}{parameter controlling reading times}
#'   \item{\code{se_a}}{standard error of a}
#'   \item{\code{se_b}}{standard error of b}
#'   \item{\code{se_alpha}}{standard error of alpha}
#'   \item{\code{se_beta}}{standard error of beta}
#'   \item{\code{passage.id}}{passage ID}
#'   \item{\code{nwords.p}}{total number of words in the passage}
#' }
#'
#' \code{$hyper.param}
#' \describe{
#'   \item{\code{vartau}}{variance of latent speed ability tau}
#'   \item{\code{rho}}{correlation between the two latent variables}
#'   \item{\code{se_vartau}}{standard error of vartau}
#'   \item{\code{se_rho}}{standard error of rho}
#' }
#'
#' @source \url{https://jnese.github.io/core-blog/}
#'
#' @references
#' Nese, J. F. T. & Kamata, A. (2014-2018). Measuring Oral Reading Fluency:
#' Computerized Oral Reading Evaluation (Project No. R305A140203) [Grant].
#' Institute of Education Sciences, U.S. Department of Education.
"passage.calib.bayes"

#' @title Sentence-level Oral Reading Fluency assessment data set
#'
#' @description This is an example data set. It is a sentence-level Oral Reading Fluency 
#' assessment data set for 58 students who were assigned to read 4 passages 
#' with a total of 23 sentences.
#' The data is a small subset of the data collected by Nese and Kamata (2014-2018).
#'
#' @format 1334 rows and 8 variables:
#' \describe{
#'   \item{\code{id.student}}{unique student identifier}
#'   \item{\code{grade}}{student grade level}
#'   \item{\code{id.passage}}{unique passage identifier}
#'   \item{\code{ind.passage}}{passage index}
#'   \item{\code{id.sentence}}{unique sentence sequence}   
#'   \item{\code{numwords.sent}}{the total number of words in the sentence}
#'   \item{\code{wrc}}{the number of words read correct}
#'   \item{\code{sec}}{time to read the sentence in seconds}
#' }
#' @references 
#' Nese, J. F. T. & Kamata, A. (2014-2018). Measuring Oral Reading Fluency: 
#'      Computerized Oral Reading Evaluation (Project No. R305A140203) [Grant]. 
#'      Institute of Education Sciences, U.S. Department of Education. 
#' @source \url{https://jnese.github.io/core-blog/}
"sentence.level.data"

#' @title Sentence-level data set with high censoring
#'
#' @description This is an example data set. It is a sentence-level Oral Reading Fluency 
#' assessment data set with high proportion of censoring data for 58 students who were assigned to read 4 passages 
#' with a total of 23 sentences.
#' The data is a small subset of the data collected by Nese and Kamata (2014-2018).
#'
#' @format 1334 rows and 9 variables:
#' \describe{
#'   \item{\code{id.student}}{unique student identifier}
#'   \item{\code{grade}}{student grade level}
#'   \item{\code{id.passage}}{unique passage identifier}
#'   \item{\code{ind.passage}}{passage index}
#'   \item{\code{id.sentence}}{unique sentence sequence}   
#'   \item{\code{numwords.sent}}{the total number of words in the sentence}
#'   \item{\code{wrc}}{the number of words read correct}
#'   \item{\code{sec}}{time to read the sentence in seconds}
#'   \item{\code{cens}}{censoring indicator, where 1 indicates a censored
#'   observation and 0 indicates a fully observed observation}
#' }
#' @references 
#' Nese, J. F. T. & Kamata, A. (2014-2018). Measuring Oral Reading Fluency: 
#'      Computerized Oral Reading Evaluation (Project No. R305A140203) [Grant]. 
#'      Institute of Education Sciences, U.S. Department of Education. 
#' @source \url{https://jnese.github.io/core-blog/}
"sentence.cens.high"

#' @title Sentence-level data set with low censoring
#'
#' @description This is an example data set. It is a sentence-level Oral Reading Fluency 
#' assessment data set with low proportion of censoring data for 58 students who were assigned to read 4 passages 
#' with a total of 23 sentences.
#' The data is a small subset of the data collected by Nese and Kamata (2014-2018).
#'
#' @format 1334 rows and 9 variables:
#' \describe{
#'   \item{\code{id.student}}{unique student identifier}
#'   \item{\code{grade}}{student grade level}
#'   \item{\code{id.passage}}{unique passage identifier}
#'   \item{\code{ind.passage}}{passage index}
#'   \item{\code{id.sentence}}{unique sentence sequence}   
#'   \item{\code{numwords.sent}}{the total number of words in the sentence}
#'   \item{\code{wrc}}{the number of words read correct}
#'   \item{\code{sec}}{time to read the sentence in seconds}
#'   \item{\code{cens}}{censoring indicator, where 1 indicates a censored
#'   observation and 0 indicates a fully observed observation}
#' }
#' @references 
#' Nese, J. F. T. & Kamata, A. (2014-2018). Measuring Oral Reading Fluency: 
#'      Computerized Oral Reading Evaluation (Project No. R305A140203) [Grant]. 
#'      Institute of Education Sciences, U.S. Department of Education. 
#' @source \url{https://jnese.github.io/core-blog/}
"sentence.cens.low"