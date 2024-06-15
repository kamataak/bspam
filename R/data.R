#' @title Passage-level Oral Reading Fluency assessment data set
#'
#' @description This is an example data set. It is a passage-level Oral Reading Fluency 
#' assessment data set for 85 students who were assigned to read 2 to 12 passages 
#' among the same 12 passages.
#' The data is a small subset of the data collected by Nese and Kamata (2014-2018).
#' 
#' @usage \code{data(passage2)}
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
#'      \url{https://ies.ed.gov/funding/grantsearch/details.asp?ID=1492}
#' @source \url{https://jnese.github.io/core-blog/}
"passage2"

#' @title Task calibration example output object by MCEM
#'
#' @description This is an example output object from running the \code{fit.model}
#' function by using the MCEM estimator option. It is a result of calibrating 
#' an oral reading fluency data set from Nese and Kamata (2014-2018) with 
#' xxxx students on 150 passages. 
#'
#' @format A list of two elements: \code{$pass.param} is a dataframe with 
#' 150 rows and 10 variables, and \code{$hyper.param} is a dataframe with
#' 1 row and 4 variables
#' @format \code{$pass.param}
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
#'   \item{\code{nwords.p}}{the total number of words in the passage}
#' }
#' @format \code{$hyper.param}
#' \describe{
#'   \item{\code{vartau}}{variance of latent reading ability tau}
#'   \item{\code{rho}}{correlation between two latent variables}
#'   \item{\code{se_vartau}}{standard error of vartar}
#'   \item{\code{se_rho}}{standard error of rho}
#' }
#' @source \url{https://jnese.github.io/core-blog/}
#' Nese, J. F. T. & Kamata, A. (2014-2018). Measuring Oral Reading Fluency: 
#'      Computerized Oral Reading Evaluation (Project No. R305A140203) [Grant]. 
#'      Institute of Education Sciences, U.S. Department of Education. 
#'      \url{https://ies.ed.gov/funding/grantsearch/details.asp?ID=1492}
"passage.calib.mcem"

#' @title Task calibration example output object by Bayes
#'
#' @description This is an example output object from running the 
#' \code{fit.model.byaes}
#' function by using the Bayesian estimator option. It is a result of calibrating 
#' an oral reading fluency data set from Nese and Kamata (2014-2018) with 
#' xxxx students on 150 passages. 
#'
#' @format two lists: $pass.param with 150 rows and $hyper.param with 4 variables
#' @format $pass.param
#' \describe{
#'   \item{a}{parameter controlling binomial success probabilities}
#'   \item{b}{parameter controlling binomial success probabilities}
#'   \item{alpha}{parameter controlling reading times}
#'   \item{beta}{parameter controlling reading times}
#'   \item{se_a}{standard error of a}
#'   \item{se_b}{standard error of b}
#'   \item{se_alpha}{standard error of alpha}
#'   \item{se_beta}{standard error of beta}
#'   \item{passage.id}{passage ID}
#'   \item{nwords.p}{the total }
#' }
#' @format $hyper.param
#' \describe{
#'   \item{vartau}{variance of latent reading ability tau}
#'   \item{rho}{correlation between two latent variables}
#'   \item{se_vartau}{standard error of vartar}
#'   \item{se_rho}{standard error of rho}
#' }
#' @source \url{https://jnese.github.io/core-blog/}
#' @references 
#' Nese, J. F. T. & Kamata, A. (2014-2018). Measuring Oral Reading Fluency: 
#'      Computerized Oral Reading Evaluation (Project No. R305A140203) [Grant]. 
#'      Institute of Education Sciences, U.S. Department of Education. 
#'      \url{https://ies.ed.gov/funding/grantsearch/details.asp?ID=1492}
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
#'      \url{https://ies.ed.gov/funding/grantsearch/details.asp?ID=1492}
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
#'   \item{\code{cens}}{whether a specific passage was censored (0) or fully observed (1)}
#' }
#' @references 
#' Nese, J. F. T. & Kamata, A. (2014-2018). Measuring Oral Reading Fluency: 
#'      Computerized Oral Reading Evaluation (Project No. R305A140203) [Grant]. 
#'      Institute of Education Sciences, U.S. Department of Education. 
#'      \url{https://ies.ed.gov/funding/grantsearch/details.asp?ID=1492}
#' @source \url{https://jnese.github.io/core-blog/}
"sentence.cens.high"

#' @title Sentence-level data set with high censoring
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
#'   \item{\code{cens}}{whether a specific passage was censored (0) or fully observed (1)}
#' }
#' @references 
#' Nese, J. F. T. & Kamata, A. (2014-2018). Measuring Oral Reading Fluency: 
#'      Computerized Oral Reading Evaluation (Project No. R305A140203) [Grant]. 
#'      Institute of Education Sciences, U.S. Department of Education. 
#'      \url{https://ies.ed.gov/funding/grantsearch/details.asp?ID=1492}
#' @source \url{https://jnese.github.io/core-blog/}
"sentence.cens.low"