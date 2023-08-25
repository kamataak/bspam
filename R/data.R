#' @title Passage calibration data set
"MCEM"

#' @title Passage-level student data set
#'
#' @description A data set consisted of reading accuracy and time data for 12 passages from 85 students.
#'
#' @format 847 rows and 7 variables:
#' \describe{
#'   \item{id.student}{unique student identifier}
#'   \item{occasion}{identifier for longitudinal assessment occasions;
#'   here a triannual assessment administered in the fall, winter, and spring of a school year}
#'   \item{grade}{student grade level}
#'   \item{id.passage}{unique passage identifier}
#'   \item{numwords.pass}{total number of words in the passage}
#'   \item{wrc}{words read correct}
#'   \item{sec}{seconds to read the passage}
#' }
#' @source \url{https://jnese.github.io/core-blog/}
"passage2"

#' @title Passage calibration data with MCEM
#'
#' @description A data set calibrated with MCEM.
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
"passage.calib.mcem"

#' @title Passage calibration data with bayes
#'
#' @description A data set calibrated with bayes.
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
"passage.calib.bayes"


