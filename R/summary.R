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
  tab <- cbind(as.double(z$task.param$task.id), # change to double to clearly print the columns
               as.vector(z$task.param$a),
               as.vector(z$task.param$b),
               z$task.param$alpha,
               z$task.param$beta)
  colnames(tab)=c("task.id","a","      b","   alpha","   beta")
  rownames(tab)=paste0(c(rep(1:length(tab[,1]))),".")
  #  print(tab[, 1:3]) # only print a part of columns
  #  print(tab)
  print(tab, digits = digits, print.gap = 2L) # specific minimum digits
  cat("\n====== Hyper Parameters ======\n")
  cat(paste(paste0("Variance of ", greek$tau), ":     "))
  cat(paste(format(z$hyper.param$vartau,digits=6,nsmall=digits), "\n"))
  cat(paste(greek$rho), "            :     ")
  cat(paste(format(z$hyper.param$rho,digits=6,nsmall=digits), "\n"))
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
#' @param factor.scores - theta and tau output flag, default is FALSE
#' @param show - output flag, "long" and "short", default "short" only output estimate result. "long" will output estimate result and data. 
#'
#' @return scoring dataset with task information and estimated score
#' @method summary scoring
#' @export
summary.scoring <- function(object, digits=4,verbose=TRUE,factor.scores=FALSE, show="short") {
  no_show_columns <- c('person.id', 'occasion', 'group', 'task.n', 'max.counts.total', 'obs.counts.obs', 'secs.obs', 'wcpm.obs')
  z <- object
  
  tb <- as.data.frame(t(do.call(rbind, z)))
  
  # don't output theta and tau, if FALSE
  if (factor.scores==FALSE) {
    tb <- tb %>% select(-contains(c("tau", "theta")))
  }
  
  getNames <- colnames(tb)
  
  cols_num <- ncol(tb)
  #set screen print out to be short decimal
  tt <- as.matrix(unlist(lapply(as.double(unlist((tb[,c(6:cols_num)]))),
                                sprintf, fmt = "%6.2f")))
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
  
  if (verbose == TRUE) {
    # only verbose TRUE will print out on screen
    print(tt)
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
                                    sprintf, fmt = "%6.2f")))
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
