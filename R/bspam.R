#' bspam: Speed-Accuracy Psychometric Modeling for Binomial Count Outcome Data
#'
#' @description
#' The \pkg{bspam} package provides tools for fitting and scoring
#' speed-accuracy psychometric models for repeatedly measured binomial count
#' outcome data. The model jointly represents accuracy and completion time
#' through latent person-level accuracy and speed components.
#'
#' The package supports model calibration and person scoring using
#' Monte Carlo EM and Bayesian approaches. It also includes functionality for
#' oral reading fluency (ORF) applications, including model-based WCPM
#' estimation, testlet models for sentence-level data, censoring-aware
#' scoring, descriptive summaries, and interactive visualizations.
#'
#' Most users will interact with \code{\link{prep}}, \code{\link{fit.model}},
#' \code{\link{scoring}}, and the associated summary and plotting methods.
#'
#' @references
#' Potgieter, C. J., Kamata, A., & Kara, Y. (2017).
#' An EM algorithm for estimating an oral reading speed and accuracy model.
#' \emph{arXiv preprint arXiv:1705.10446}.
#' \url{https://arxiv.org/abs/1705.10446}
#'
#' Kara, Y., Kamata, A., Potgieter, C. J., & Nese, J. F. T. (2020).
#' Estimating model-based oral reading fluency: A Bayesian approach with a
#' binomial-lognormal joint latent model.
#' \emph{Educational and Psychological Measurement, 80}(5), 847-869.
#' \doi{10.1177/0013164419900208}
#'
#' @importFrom stats cov dbinom dnorm na.omit optim pnorm qnorm rbinom rnorm
#'     runif sd time uniroot var
#' @importFrom stringr str_split
#' @importFrom tibble column_to_rownames deframe rownames_to_column
#' @importFrom dplyr bind_rows filter group_by left_join mutate select summarise
#' 
#' @author Kuo Wang <wangkuo@nifty.com>
#' @author Akihito Kamata <akamata@smu.edu>
#' @author Cornelis J. Potgieter <c.potgieter@tcu.edu>
#' @author Joseph F. T. Nese <jnese@uoregon.edu>
#' @author Yusuf Kara <ykara@mail.smu.edu>
#' @author Sarunya Somsong <sarunya.ss@hotmail.com>
#' @author Xin Qiao <xqiao@usf.edu>
#'
#' @aliases bspam-package
#' @name bspam-package
NULL
