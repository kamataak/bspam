#' summary the information of fit.model class
#'
#'
#' Copyright (C) 2021-2023 The ORF Project Team
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
#' @param object = object
#' @param digits = print out numeric with specific digits
#' @param ... = parameter
#'
#' @import mvtnorm
#' @import tidyverse
#' @return printing information
#'
#' @method summary fit.model
#' @export
summary.fit.model <- function(object, digits=4,...) {
  z <- object
  tb <- as.data.frame(t(do.call(rbind, z[[1]])))
  tb <- tb[,(1:4)]
  
  tt <- as.data.frame(sapply(lapply(tb, sprintf, fmt = "%6.3f"), as.numeric))
  
  print(tt, digits = digits, print.gap = 3L) # specific minimum digits
  cat("\n====== Hyper Parameters ======\n")
  cat(paste(paste0("Variance of ", "tau"), ":     "))
  # cat(paste(format(z$hyper.param$vartau,digits=6,nsmall=digits), "\n"))
  # cat(paste(greek$rho), "            :     ")
  # cat(paste(format(z$hyper.param$rho,digits=6,nsmall=digits), "\n"))
  cat(paste(sprintf(fmt = "%6.6f", z$hyper.param$vartau), "\n"))
  cat(paste("Rho"), "            :     ")
  cat(paste(sprintf(fmt = "%6.6f", z$hyper.param$rho), "\n"))
}
#' summary the information of wcpm class
#'
#'
#' Copyright (C) 2021-2023 The ORF Project Team
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
#' @param object = object
#' @param digits = print out numeric with specific digits
#' @param verbose - boolean, if TRUE, shows the summary, default is TRUE
#' @param factor.scores - theta and tau output flag, default is TRUE # before was FALSE
#' @param show - output flag, "long" and "short", default "short" only output estimate result. "long" will output estimate result and data. 
#'
#' @return scoring dataset with task information and estimated score
#' @method summary scoring
#' @export
summary.scoring <- function(object, digits=4,verbose=TRUE,factor.scores=TRUE, show="short") {
  
  z <- object
  # try dataframe
  tb <- as.data.frame(t(do.call(rbind, z))) # convert list to data frame
  # print(colnames(tb)) for debug
  
  if("wcpm.obs" %in% colnames(tb)) {
    if ("wcpm.jags" %in% colnames(tb)) {  # for bayes object
      no_show_columns <- c('occasion', 'group', 'task.n', 'max.counts.total', 'obs.counts.obs', 'secs.obs', 'wcpm.obs',
                           'task.n.wcpm', 'max.counts.total.wcpm', 'wcpm.jags', 'se.wcpm.jags', 'low.95.jags.wcpm', 'up.95.jags.wcpm')  
    } else if ("wcpm.stan" %in% colnames(tb)) { # for stan output
      no_show_columns <- c('occasion', 'group', 'task.n', 'max.counts.total', 'obs.counts.obs', 'secs.obs', 'wcpm.obs',
                           'task.n.wcpm', 'max.counts.total.wcpm', 'wcpm.stan', 'se.wcpm.stan', 'low.95.stan.wcpm', 'up.95.stan.wcpm')  
    } else {
      no_show_columns <- c('occasion', 'group', 'task.n', 'max.counts.total', 'obs.counts.obs', 'secs.obs', 'wcpm.obs')
    }
  } else {
    no_show_columns <- c('occasion', 'group', 'task.n', 'max.counts.total', 'obs.counts.obs', 'secs.obs')
  }
  
  # don't output theta and tau, if FALSE
  if (factor.scores==FALSE) {
    tb <- tb %>% select(-contains(c("tau", "theta")))
  }
  
  getNames <- colnames(tb)
  
  cols_num <- ncol(tb)
  #set screen print out to be short decimal
  tt <- as.matrix(unlist(lapply(as.double(unlist((tb[,c(6:cols_num)]))),
                                sprintf, fmt = "%6.3f")))
  dim(tt) <- c(dim(tb)[1],(cols_num-5))
  tt <- cbind(tb[,c(1:5)], tt)
  colnames(tt) <- getNames
  # prepare for data output
  if (nrow(tb) == 1) {
    tm1 <- t(sapply(tb %>% select(-contains(c("occasion"))), as.numeric))
  } else {
    tm1 <- sapply(tb %>% select(-contains(c("occasion"))), as.numeric)
  }
  #  tm1 <- sapply(tb %>% select(-contains(c("occasion"))), as.numeric)
  tm2 <- tb %>% select("occasion")
  tb <- cbind(tm1, tm2)[,c(1,cols_num,2:(cols_num-1))]
  
  if (show == "short") {
    tt <- tt %>% select(-no_show_columns)
    tb <- tb %>% select(-no_show_columns)
  }
  
  #exclude rownames
  rownames(tt) <- NULL
  rownames(tb) <- NULL
  
  if (verbose == TRUE) {
    # only verbose TRUE will print out on screen
    print(tt, row.names = F)
    return(invisible(tb))
  } else {
    return(invisible(tb))
  }
  
}
#' summary the information of bootstrap class
#'
#'
#' Copyright (C) 2021-2023 The ORF Project Team
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
#' @param object = bootstrap object
#' @param digits = print out numeric with specific digits
#' @param geterror, summary error case, default FALSE
#' @param verbose show summary on screen, default TRUE
#' @param factor.scores - theta and tau output flag, default is FALSE
#'
#' @return table
#' @method summary bootstrap
#' @export
summary.bootstrap <- function(object, digits=4, geterror=FALSE,verbose=TRUE,factor.scores=FALSE) {
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
