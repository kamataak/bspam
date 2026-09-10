# Summary methods for the bspam package.
#
# Copyright (C) 2021-2026 The ORF Project Team
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# A copy of the GNU General Public License is available at
# http://www.gnu.org/licenses/
#

#' Summarize a fitted testlet model
#'
#' Prints the estimated sub-task parameters and population-level
#' hyperparameters from an object of class \code{"fit.model.testlet"}.
#'
#' @param object A fitted testlet-model object of class
#'     \code{"fit.model.testlet"}, typically returned by
#'     \code{\link{fit.model.testlet}} or by \code{\link{fit.model}} with
#'     \code{testlet = TRUE}.
#' @param digits Integer passed to the table-printing method. The current
#'     implementation formats reported parameter estimates to three decimal
#'     places before printing. Default is \code{4}.
#' @param ... Additional arguments for the summary method. Currently not used.
#'
#' @details
#' The printed output first reports the first four estimated sub-task
#' parameters from the fitted model. It then prints \code{sigma},
#' \code{gamma1}, \code{gamma2}, \code{rho.theta}, and
#' \code{rho.testlet}.
#'
#' @return Invisibly returns \code{NULL} after printing the model summary.
#'
#' @seealso \code{\link{fit.model.testlet}} and \code{\link{fit.model}}.
#'
#' @import mvtnorm
#' @import tidyverse
#' @method summary fit.model.testlet
#' @export
summary.fit.model.testlet <- function(object, digits=4,...) {
  z <- object
  tb <- as.data.frame(t(do.call(rbind, z[[1]])))
  tb <- tb[,(1:4)]
  
  tb_numeric <- as.data.frame(lapply(tb, function(x) as.numeric(as.character(x))))
  tt <- as.data.frame(sapply(lapply(tb_numeric, sprintf, fmt = "%6.3f"), as.numeric))
  
  print(tt, digits = digits, print.gap = 3L) # specific minimum digits
  cat("\n====== Hyper Parameters ======\n")
  cat("sigma      :")
  # cat(paste(format(z$hyper.param$vartau,digits=6,nsmall=digits), "\n"))
  # cat(paste(greek$rho), "            :     ")
  # cat(paste(format(z$hyper.param$rho,digits=6,nsmall=digits), "\n"))
  cat(paste(sprintf(fmt = "%6.3f", z$hyper.param$sigma), "\n")) # CHANGE TO .3f
  cat("gamma1     :")
  cat(paste(sprintf(fmt = "%6.3f", z$hyper.param$gamma1), "\n")) # CHANGE TO .3f
  cat("gamma2     :")
  cat(paste(sprintf(fmt = "%6.3f", z$hyper.param$gamma2), "\n")) # CHANGE TO .3f
  cat("rho.theta  :")
  cat(paste(sprintf(fmt = "%6.3f", z$hyper.param$rho.theta), "\n")) # CHANGE TO .3f
  cat("rho.testlet:")
  cat(paste(sprintf(fmt = "%6.3f", z$hyper.param$rho.testlet), "\n")) # CHANGE TO .3f
}

#' Summarize a fitted bspam model
#'
#' Prints the calibrated task parameters and population-level hyperparameters
#' from an object of class \code{"fit.model"}.
#'
#' @param object A fitted model object of class \code{"fit.model"}, typically
#'     returned by \code{\link{fit.model}}.
#' @param digits Integer passed to the table-printing method. The current
#'     implementation formats reported parameter estimates to three decimal
#'     places before printing. Default is \code{4}.
#' @param ... Additional arguments for the summary method. Currently not used.
#'
#' @details
#' The printed output first reports the first four calibrated task parameters,
#' corresponding to \code{a}, \code{b}, \code{alpha}, and \code{beta}.
#' It then reports \code{vartau}, the variance of the latent speed component,
#' and \code{rho}, the correlation between latent accuracy and speed.
#'
#' @return Invisibly returns \code{NULL} after printing the model summary.
#'
#' @seealso \code{\link{fit.model}} and
#'     \code{\link{summary.fit.model.testlet}}.
#'
#' @import mvtnorm
#' @import tidyverse
#' @method summary fit.model
#' @export
summary.fit.model <- function(object, digits=4,...) {
  
  z <- object
  tb <- as.data.frame(t(do.call(rbind, z[[1]])))
  tb <- tb[,(1:4)]
  
  tb_numeric <- as.data.frame(lapply(tb, function(x) as.numeric(as.character(x))))
  tt <- as.data.frame(sapply(lapply(tb_numeric, sprintf, fmt = "%6.3f"), as.numeric))
  
  print(tt, digits = digits, print.gap = 3L) # specific minimum digits
  cat("\n====== Hyper Parameters ======\n")
  cat(paste(paste0("Variance of ", "tau"), ":     "))
  # cat(paste(format(z$hyper.param$vartau,digits=6,nsmall=digits), "\n"))
  # cat(paste(greek$rho), "            :     ")
  # cat(paste(format(z$hyper.param$rho,digits=6,nsmall=digits), "\n"))
  cat(paste(sprintf(fmt = "%6.3f", z$hyper.param$vartau), "\n")) # CHANGE TO .3f
  cat(paste("Rho"), "            :     ")
  cat(paste(sprintf(fmt = "%6.3f", z$hyper.param$rho), "\n")) # CHANGE TO .3f
}

#' Summarize bspam scoring results
#'
#' Prints and returns a person-level summary from an object of class
#' \code{"scoring"}. The method supports both task-level and testlet scoring
#' output.
#'
#' @param object A scoring object of class \code{"scoring"}, typically
#'     returned by \code{\link{scoring}}.
#' @param digits Integer retained for compatibility with the summary method.
#'     The current screen output is formatted to three decimal places.
#'     Default is \code{4}.
#' @param verbose Logical. If \code{TRUE}, the formatted summary is printed.
#'     The processed summary data are returned invisibly regardless of this
#'     setting. Default is \code{TRUE}.
#' @param factor.scores Logical. If \code{FALSE}, columns whose names contain
#'     \code{"theta"} or \code{"tau"} are removed. Default is \code{TRUE}.
#' @param show Character string controlling the amount of information.
#'     With \code{"short"} (default), observed-data and design-summary columns
#'     are omitted. With \code{"long"}, those columns are retained.
#' @param ... Additional arguments for the summary method. Currently not used.
#'
#' @details
#' The method automatically distinguishes task-level scoring output from
#' testlet scoring output. For task-level results, the short display removes
#' occasion, group, task count, maximum-count total, observed-count total,
#' observed time, and observed WCPM when present. For testlet results, the
#' corresponding sub-task and testlet-specific fields are handled as well.
#'
#' When \code{verbose = TRUE}, a formatted table is printed. The invisibly
#' returned object contains the corresponding numeric summary data rather than
#' the display-formatted values.
#'
#' @return Invisibly returns a data frame containing the processed person-level
#'     scoring summary. Included columns depend on \code{factor.scores},
#'     \code{show}, scoring type, and whether ORF/WCPM output is present.
#'
#' @seealso \code{\link{scoring}} and \code{\link{summary.bootstrap}}.
#'
#' @method summary scoring
#' @export
summary.scoring <- function(object, digits=4, verbose=TRUE,
                            factor.scores=TRUE, show="short",...) {
  
  z <- object
  tb <- as.data.frame(t(do.call(rbind, z)))
  
  # Detect new testlet scoring format
  is_testlet <- any(c("sub.task.n", "obs.counts.total") %in% colnames(tb))
  
  if (is_testlet) {
    no_show_columns <- c(
      "occasion", "group", "task.n", "sub.task.n",
      "max.counts.total", "obs.counts.total",
      "secs.obs", "wcpm.obs"
    )
    meta_cols <- 7
  } else {
    if ("wcpm.obs" %in% colnames(tb)) {
      no_show_columns <- c(
        "occasion", "group", "task.n",
        "max.counts.total", "obs.counts.obs",
        "secs.obs", "wcpm.obs"
      )
    } else {
      no_show_columns <- c(
        "occasion", "group", "task.n",
        "max.counts.total", "obs.counts.obs",
        "secs.obs"
      )
    }
    meta_cols <- 6
  }
  
  # don't output theta and tau, if FALSE
  if (factor.scores == FALSE) {
    tb <- tb %>% select(-contains(c("tau", "theta")))
  }
  
  getNames <- colnames(tb)
  cols_num <- ncol(tb)
  
  # screen print formatting
  tt_num <- as.matrix(unlist(lapply(
    as.double(unlist(tb[, (meta_cols + 1):cols_num])),
    sprintf,
    fmt = "%6.3f"
  )))
  
  dim(tt_num) <- c(nrow(tb), cols_num - meta_cols)
  tt <- cbind(tb[, 1:meta_cols], tt_num)
  colnames(tt) <- getNames
  
  # prepare invisible data output
  if (nrow(tb) == 1) {
    tm1 <- t(sapply(tb %>% select(-contains("occasion")), as.numeric))
  } else {
    tm1 <- sapply(tb %>% select(-contains("occasion")), as.numeric)
  }
  
  tm2 <- tb %>% select("occasion")
  tb <- cbind(tm1, tm2)[, c(1, cols_num, 2:(cols_num - 1))]
  
  if (show == "short") {
    tt <- tt %>% select(-any_of(no_show_columns))
    tb <- tb %>% select(-any_of(no_show_columns))
  }
  
  rownames(tt) <- NULL
  rownames(tb) <- NULL
  
  if (verbose == TRUE) {
    print(tt, row.names = FALSE)
    return(invisible(tb))
  } else {
    return(invisible(tb))
  }
}

#' Summarize bootstrap scoring results
#'
#' Prints and returns bootstrap-based scoring results from an object of class
#' \code{"bootstrap"}. The method can alternatively report cases for which the
#' bootstrap procedure produced an error.
#'
#' @param object A bootstrap scoring object of class \code{"bootstrap"}.
#' @param digits Integer retained for compatibility with the summary method.
#'     The current screen output is formatted to three decimal places.
#'     Default is \code{4}.
#' @param geterror Logical. If \code{TRUE}, the method reports
#'     \code{object$error_case} instead of the bootstrap scoring table.
#'     Default is \code{FALSE}.
#' @param verbose Logical. If \code{TRUE}, the formatted bootstrap summary is
#'     printed. Processed results are returned invisibly. Default is
#'     \code{TRUE}.
#' @param factor.scores Logical. If \code{FALSE}, columns whose names contain
#'     \code{"theta"} or \code{"tau"} are removed. Default is \code{FALSE}.
#' @param ... Additional arguments for the summary method. Currently not used.
#'
#' @details
#' With \code{geterror = FALSE}, the method summarizes
#' \code{object$bootstrap.out}. Numeric scoring columns are formatted to three
#' decimal places for screen display, while returned values remain numeric.
#'
#' With \code{geterror = TRUE}, existing error cases are printed and returned
#' invisibly. If no error cases are present, the method prints a message.
#' If the bootstrap output contains no columns, it prints
#' \code{"Bootstrap has 0 obs."}.
#'
#' @return With nonempty bootstrap output, invisibly returns the processed
#'     bootstrap scoring table. With \code{geterror = TRUE} and available
#'     error cases, invisibly returns \code{object$error_case}. Otherwise an
#'     informative message is printed.
#'
#' @seealso \code{\link{scoring}} and \code{\link{summary.scoring}}.
#'
#' @method summary bootstrap
#' @export
summary.bootstrap <- function(object, digits=4, geterror=FALSE,verbose=TRUE,factor.scores=FALSE,...) {
  z <- object
  
  tb <- z$bootstrap.out
  if (geterror == TRUE) {
    if (length(z$error_case) != 0) {
      print(z$error_case)
      return(invisible(z$error_case))
    } else {
      print("Bootstrap has no error cases.")
    }
  } else {
    if (ncol(tb) != 0) {
      # don't output theta and tau, if FALSE
      if (factor.scores==FALSE) {
        tb <- tb %>% select(-contains(c("tau", "theta")))
      }
      
      getNames <- colnames(tb)
      cols_num <- ncol(tb)
      
      #set screen print out to be short decimal
      tt <- as.matrix(unlist(lapply(as.double(unlist((tb[,c(6:cols_num)]))),
                                    sprintf, fmt = "%6.3f")))
      dim(tt) <- c(nrow(tb),(cols_num-5))
      tt <- cbind(tb[,c(1:5)], tt)
      colnames(tt) <- getNames
      # prepare for data output
      if (nrow(tb) == 1) {
        tm1 <- t(sapply(tb %>% select(-contains(c("occasion"))), as.numeric))
      } else {
        tm1 <- sapply(tb %>% select(-contains(c("occasion"))), as.numeric)
      }
      
      tm2 <- tb %>% select("occasion")
      tb <- cbind(tm1, tm2)[,c(1,cols_num,2:(cols_num-1))]
      if (verbose == TRUE) {
        # only verbose TRUE will print out on screen
        print.noquote(tt)
        return(invisible(tb))
      } else {
        return(invisible(tb))
      }
    } else {
      print("Bootstrap has 0 obs.")
    }
  }
}

