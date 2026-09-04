#' bspam : A package for fitting the speed-accuracy psychometric model for repeatedly measured count outcome data.
#'
#' @description 
#' \strong{Purpose of this package} 
#' 
#' `bspam` is an R package that contains functions to fit the speed-accuracy 
#' psychometric model for repeatedly measured count outcome data (Potgieter, Kamata & Kara, 2017; 
#' Kara, Kamata, Potgieter & Nese, 2020), where the accuracy is modeled by a binomial 
#' count latent variable model. For example, the use of this modeling technique 
#' allows model-based calibration and scoring for oral reading fluency (ORF) assessment data. 
#' 
#' \strong{Design philosophy} 
#' 
#' Write Design philosophy
#' 
#' @importFrom stats cov dbinom dnorm na.omit optim pnorm qnorm rbinom rnorm runif sd time uniroot var 
#' @importFrom stringr str_split
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