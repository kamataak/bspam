# This file includes utilities of the bspam package.
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

#' Prepare response data for bspam analyses
#'
#' Prepares long-format response data for model fitting and scoring with
#' \pkg{bspam}. The function standardizes the required variable names and
#' constructs the data structures used internally by the package.
#'
#' Both task-level and sub-task-level data are supported. In oral reading
#' fluency (ORF) applications, task-level data correspond to passage-level
#' observations, whereas sub-task-level data correspond to sentence-level
#' observations nested within passages.
#'
#' @param data A data frame containing the response data in long format.
#' @param person.id Quoted variable name in \code{data} identifying persons.
#' @param task.id Quoted variable name in \code{data} identifying tasks.
#'     In the ORF assessment context, this is the passage identifier.
#' @param sub.task.id Quoted variable name in \code{data} identifying
#'     sub-tasks. In the ORF assessment context, this is the sentence
#'     identifier. This argument is required when
#'     \code{sentence_level = TRUE}.
#' @param occasion Quoted variable name in \code{data} identifying measurement
#'     occasions. If omitted, all observations are assigned to a single
#'     occasion.
#' @param group Quoted variable name in \code{data} identifying groups.
#'     If omitted, all observations are assigned to a single group.
#' @param max.counts Quoted variable name in \code{data} giving the maximum
#'     possible count for each task or sub-task. In the ORF assessment
#'     context, this is the number of words in the passage or sentence.
#' @param obs.counts Quoted variable name in \code{data} giving the observed
#'     number of successful outcomes. In the ORF assessment context, this is
#'     the number of words read correctly.
#' @param time Quoted variable name in \code{data} giving the observed
#'     completion time, in seconds.
#' @param cens Optional quoted variable name in \code{data} containing
#'     censoring indicators. For sentence-level data, if this argument is
#'     omitted, all observations are treated as fully observed and a
#'     censoring indicator equal to \code{0} is created internally.
#' @param sentence_level Logical. If \code{FALSE}, task-level data are
#'     prepared. If \code{TRUE}, sub-task-level data are prepared.
#'     Default is \code{FALSE}.
#'
#' @details
#' For task-level data, \code{prep()} reshapes the observed count and time
#' variables into person-by-task structures used by the task-level
#' speed-accuracy model. Reading or completion times are transformed to the
#' natural logarithm of time standardized to 10 task units. In ORF
#' applications, this corresponds to log reading time per 10 words.
#'
#' For sub-task-level data, the function preserves the task and sub-task
#' structure in the standardized long-format data and constructs corresponding
#' person-by-observation matrices for sentence-level analyses.
#'
#' If \code{occasion} or \code{group} is not supplied, a constant value of
#' \code{1} is created for that variable. For sentence-level data, if
#' \code{cens} is not supplied, a censoring variable containing zeros is
#' created, indicating that all observations are treated as fully observed.
#'
#' @return A prepared-data object containing two components:
#'     \describe{
#'       \item{\code{data.long}}{A standardized long-format data frame using
#'       the variable names expected internally by \pkg{bspam}.}
#'       \item{\code{data.wide}}{A list of matrices and vectors used by the
#'       model-fitting and scoring functions.}
#'     }
#'
#'     For task-level data, the returned object has class
#'     \code{"prepared.task"}. Its \code{data.wide} component contains
#'     \code{Y}, the observed count matrix; \code{logT10}, the matrix of
#'     log-transformed completion times standardized to 10 units;
#'     \code{N}, the vector of task maximum counts; and \code{I}, the number
#'     of tasks.
#'
#'     For sub-task-level data, the returned object has class
#'     \code{"prepared.sub.task"}. Its \code{data.wide} component contains
#'     the corresponding count, time, and maximum-count structures used for
#'     sentence-level analyses.
#'
#' @seealso
#' \code{\link{fit.model}} for model calibration and
#' \code{\link{scoring}} for person-level scoring.
#'
#' @examples
#' prepared <- prep(
#'   data = passage2,
#'   person.id = "id.student",
#'   task.id = "id.passage",
#'   occasion = "occasion",
#'   group = "grade",
#'   max.counts = "numwords.pass",
#'   obs.counts = "wrc",
#'   time = "sec"
#' )
#'
#' @import tidyr
#' @import dplyr
#' @import tidyverse
#'
#' @export
prep <- function(data=data,person.id="",task.id="",sub.task.id="",occasion="",group="",max.counts="",obs.counts="",time="", cens="",sentence_level = FALSE) {
  # loading logger
  log.initiating()
  flog.info("Begin preparing data process", name = "orfrlog")
  if (occasion == "") {
    # add default occasion
    data["occasion"] <- 1
    occasion = "occasion"
  } 
  if (group == "") {
    # add default group
    data["group"] <- 1
    group = "group"
  } 
  if (cens == "") {
    if (sentence_level == TRUE) {
      # add default censoring
      data["cens"] <- 0 # which mean all data are observation not censoring
      cens = "cens"     
    } 
  } 
  
  
  dat <- data
  tryCatch (
    expr = {

      # c1 <- dat[person.id] # person.id
      # c2 <- dat[task.id] # task.id
      # c3 <- dat[max.counts] # max.counts
      # c4 <- dat[occasion] # occasion
      # c5 <- dat[group] # group
      # c6 <- dat[obs.counts] # obs.counts
      # c7 <- dat[time] # time
      # c8 <- dat[cens] # censoring
      # c9 <- dat[sub.task.id] # censoring
      # lgsec <- log(c7) # lgsec
      # 
      # dat <- data.frame(c1,c2,c9,c3,c4,c5,c6,c7,c8,lgsec)
      # colnames(dat) <- col.labels
      
      if (sentence_level == FALSE) { # for passage level data
        #  col.names = c(studentid,passageid,numwords.p,season,grade,wrc,time)
        
        c1 <- dat[person.id] # person.id
        c2 <- dat[task.id] # task.id
        c3 <- dat[max.counts] # max.counts
        c4 <- dat[occasion] # occasion
        c5 <- dat[group] # group
        c6 <- dat[obs.counts] # obs.counts
        c7 <- dat[time] # time
        lgsec <- log(c7) # lgsec
        
        if (cens != "") {
          c8 <- dat[cens] # censoring     
          dat <- data.frame(c1,c2,c3,c4,c5,c6,c7,c8,lgsec)
          col.labels <- c("person.id","task.id","max.counts","occasion","group","obs.counts","time","cens","lgsec")
          
        } else {
          dat <- data.frame(c1,c2,c3,c4,c5,c6,c7,lgsec)
          col.labels <- c("person.id","task.id","max.counts","occasion","group","obs.counts","time","lgsec")
        }
        
        colnames(dat) <- col.labels
        
        tp <- as.data.frame(dat %>% select(person.id, task.id, obs.counts) %>%
                              pivot_wider(names_from = task.id, values_from = obs.counts))
        
        rownames(tp) <- as.character(tp$person.id)
        Y <- tp %>% select(-person.id)
        Y <- Y[ , order(names(Y))] # sort by passage.id
        Y <- as.matrix(Y)
        for (i in 1:ncol(Y)) {
          Y[,i]<-ifelse(is.na(Y[,i]),NA,Y[,i]) #NaN
        }
        logT <-  as.data.frame(dat %>%
                                 mutate(lgsec=log(time)) %>%
                                 select(person.id, task.id, lgsec) %>%
                                 pivot_wider(names_from = task.id, values_from = lgsec) %>%
                                 select(-person.id))
        rownames(logT) <- as.character(tp$person.id)
        logT <- logT[ , order(names(logT))] # sort by person.id
        N <- as.data.frame(dat %>%
                             group_by(task.id) %>% arrange(task.id) %>% # sort by person.id
                             summarise(max.counts=max(max.counts)) %>% # numwords.pass
                             select(-task.id))
        rownames(N) <- colnames(Y)
        N <- pull(N)
        I <- length(N)
        N.matrix <- matrix(rep(as.matrix(N),dim(Y)[1]),nrow = dim(Y)[1], byrow = TRUE)
        logT10 <- logT - log(N.matrix) + log(10)
        
        data.in <- list(Y = Y, logT10 = logT10, N = N, I = I)
      } else { # for sentence level data
        #  col.names = c(studentid,passageid,numwords.p,season,grade,wrc,time)
        col.labels <- c("person.id","task.id","sub.task.id","max.counts","occasion","group","obs.counts","time","cens","lgsec")
        
        c1 <- dat[person.id] # person.id
        c2 <- dat[task.id] # task.id
        c3 <- dat[max.counts] # max.counts
        c4 <- dat[occasion] # occasion
        c5 <- dat[group] # group
        c6 <- dat[obs.counts] # obs.counts
        c7 <- dat[time] # time
        c8 <- dat[cens] # censoring
        c9 <- dat[sub.task.id] # censoring
        lgsec <- log(c7) # lgsec
        
        dat <- data.frame(c1,c2,c9,c3,c4,c5,c6,c7,c8,lgsec)
        colnames(dat) <- col.labels
        
        df <- dat %>%
          group_by(person.id) %>%
          mutate(obs_sequence = row_number()) %>% # Create a sequence for each task.id within person.id
          ungroup()
        tp <- as.data.frame(df %>% select(person.id, obs_sequence, obs.counts) %>%
                              pivot_wider(names_from = obs_sequence, 
                                          values_from = obs.counts,
                                          names_prefix = "obs_"))
        
        Y <- tp %>% select(-person.id)
        
        # Extract the numeric part from the column names and use it for ordering
        numeric_order <- order(as.numeric(gsub("obs_", "", colnames(Y))))
        
        # Order the data frame by the numeric order
        Y <- Y[, numeric_order]
        
        # Convert to a matrix
        Y <- as.matrix(Y)
        
        for (i in 1:ncol(Y)) {
          Y[,i]<-ifelse(is.na(Y[,i]),NA,Y[,i]) #NaN
        }
        logT <-  as.data.frame(df %>%
                                 mutate(lgsec=log(time)) %>%
                                 select(person.id, obs_sequence, lgsec) %>%
                                 pivot_wider(names_from = obs_sequence, 
                                             values_from = lgsec,
                                             names_prefix = "obs_") %>%
                                 select(-person.id))
        
        # Order the data frame by the numeric order
        logT <- logT[, numeric_order]
        
        N <- as.data.frame(df %>% select(person.id, obs_sequence, max.counts) %>%
                             pivot_wider(names_from = obs_sequence, 
                                         values_from = max.counts,
                                         names_prefix = "obs_") %>%
                             select(-person.id))
        
        I <- length(N)
        N.vec <- N %>% slice(1)
        N.matrix <- matrix(rep(as.matrix(N),dim(Y)[1]),nrow = dim(Y)[1], byrow = TRUE)
        logT10 <- logT - log(N.matrix) + log(10)
        
        logT10 <- logT10[, numeric_order]
        
        data.in <- list(Y = Y, logT10 = logT10, N = N.matrix, N.vec=N.vec, I = I)
      }
      
      output <- list(data.long=dat,
                     data.wide=data.in)
      flog.info("End preparing data process", name = "orfrlog")
      
      if (sentence_level == FALSE) {
        class(output) <- "prepared.task"        
      } else {
        class(output) <- "prepared.sub.task"
      }

      return(output)
    },
    warning = function(w) {
      flog.info("There was a warning message. Something is wrong!", name = "orfrlog")
      flog.info(w, name = "orfrlog")
    },
    error = function(w) {
      flog.info("There was an error message. Something is wrong!", name = "orfrlog")
      flog.info(w, name = "orfrlog")
    }
  )
}
#' Create person-by-occasion case identifiers
#'
#' Creates unique case identifiers by combining the \code{person.id} and
#' \code{occasion} variables in prepared response data. These identifiers
#' are used by scoring functions to identify person-by-occasion records.
#'
#' @param data A data frame containing \code{person.id} and
#'     \code{occasion} variables.
#'
#' @return Invisibly returns a one-column data frame containing unique case
#'     identifiers in the variable \code{cases}.
#'
#' @export
get.cases <- function(data) {
  cases <- data %>% select(person.id,occasion) %>% unique() %>%
    unite("cases", person.id:occasion, sep = "_", remove = TRUE, na.rm = FALSE) %>%
    select(cases)
  #print(cases)
  return(invisible(cases))
}

# Identify person-by-occasion cases with perfect accuracy.
get.perfectcases <- function(data) {
  perfect.cases <- data %>% group_by(person.id,occasion) %>%
    summarise(obs.counts.sum=sum(obs.counts),
              max.counts.sum=sum(max.counts), .groups = "drop_last") %>%
    filter(obs.counts.sum == max.counts.sum) %>%
    unite("perfect.cases", person.id:occasion, sep = "_", remove = TRUE, na.rm = FALSE) %>%
    select(perfect.cases)
  return(invisible(perfect.cases))
}

# Identify person-by-occasion cases with zero observed correct counts.
get.zerocases <- function(data) {
  zero.cases <- data %>% group_by(person.id,occasion) %>%
    summarise(obs.counts.sum=sum(obs.counts),
              .groups = "drop") %>%
    filter(obs.counts.sum == 0) %>%
    unite("zero.cases", person.id:occasion, sep = "_", remove = TRUE, na.rm = FALSE) %>%
    select(zero.cases)
  return(invisible(zero.cases))
}

# Convert raw response data to the standardized long format used internally.
preplong <- function(data,
                     person.id="",
                     task.id="",
                     occasion="",
                     group="",
                     max.counts="",
                     obs.counts="",
                     time=""){
  
  if (occasion == "") {
    # add default occasion
    data["occasion"] <- 1
    occasion = "occasion"
  } 
  if (group == "") {
    # add default occasion
    data["group"] <- 1
    group = "group"
  } 
  vars <- c(person.id,
            task.id,
            occasion,
            group,
            max.counts,
            obs.counts,
            time)
  dat <- data %>%
    select(all_of(vars)) %>%
    rename(person.id=1,task.id=2,
           occasion=3,group=4,
           max.counts=5,obs.counts=6,time=7) %>%
    mutate(lgsec=log(.[[7]]))
  #lgsec10 = log(.[[7]] - log(.[[5]]) + log(10)
  #           stu_season_id2=paste(.[[1]],.[[3]],sep="_"))
  return(dat)
}


# Convert raw task-level response data to the wide matrices used internally.
prepwide <- function(data,
                     person.id,
                     task.id,
                     max.counts,
                     obs.counts,
                     time){
  vars <- c(person.id,
            task.id,
            max.counts,
            obs.counts,
            time)
  dat <- data %>%
    select(all_of(vars))
  Y <- dat %>%
    select(vars[1], vars[2], vars[4]) %>%
    spread(key = vars[2], value = vars[4]) %>%
    select(-vars[1])
  Y <- as.matrix(Y)
  for (i in 1:ncol(Y)) {
    Y[,i]<-ifelse(is.na(Y[,i]),NaN,Y[,i])
  }
  logT <- dat %>%
    mutate(logsecs=log(.[[5]])) %>%
    select(vars[1], vars[2], logsecs) %>%
    spread(key = vars[2], value = logsecs) %>%
    select(-vars[1])
  N <- dat %>%
    group_by_at(2) %>%
    #    summarise_at(3,max) %>%
    summarise_at(.vars = names(.)[3],max) %>%
    select(-vars[2])
  N <- pull(N)
  I <- length(N)
  N.matrix <- matrix(rep(as.matrix(N),dim(Y)[1]),nrow = dim(Y)[1], byrow = TRUE)
  logT10 <- tibble(logT - log(N.matrix) + log(10))
  data.in <- list(Y = Y, logT10 = logT10, N = N, I = I)
  return(data.in)
}

# Remove passages that do not meet the overlap criterion.
exclude_passages <- function(passage) {
  err_list <- get_errlist(passage)
  return (passage %>% filter(!(id.passage %in% err_list)))
  
}

# Identify passages that do not share enough students with other passages.
get_errlist <- function(passage) {
  # get unique passage list
  passage_ids <- as.matrix(passage %>% select(id.passage) %>% unique())
  
  flag <- 0
  err_list <- c()
  for (i in 1:length(passage_ids)) {
    flag <- 0
    #print(passage_ids[i])
    set_a <- passage %>% filter(id.passage==passage_ids[i]) %>% select(id.student)
    for (j in 1:length(passage_ids)) {
      if (j != i) {
        set_b <- passage %>% filter(id.passage==passage_ids[j]) %>% select(id.student)
        if (nrow(intersect(set_a,set_b)) > 2) {
          flag <- 1
          #print("break")
          break
        }
      }
    }
    if (flag == 0) {
      print(paste("got ",passage_ids[i]))
      ll <- length(err_list)+1
      err_list[ll] <- passage_ids[i]
    }
    
  }
  return (err_list)
}
#' Aggregate word-level ORF data
#'
#' Aggregates word-level response and timing data to the sentence or passage
#' level. The function calculates words read correctly, elapsed reading time,
#' the number of words, and observed words-correct-per-minute (WCPM).
#'
#' @param data A data frame containing word-level response and timing data.
#' @param agg.level Character string specifying the aggregation level.
#'     Available options are \code{"sentence"} and \code{"passage"}.
#'     Default is \code{"sentence"}.
#' @param person.id Quoted variable name identifying persons.
#' @param passage.id Quoted variable name identifying passages.
#' @param word.pos.sen Quoted variable name giving each word's position within
#'     a sentence. Required when \code{agg.level = "sentence"}.
#' @param word.pos.pas Quoted variable name giving each word's position within
#'     a passage. Required when \code{agg.level = "passage"}.
#' @param sen.pos.pas Quoted variable name giving each sentence's position
#'     within a passage. Required when \code{agg.level = "sentence"}.
#' @param start.time Quoted variable name giving the word-level start time.
#' @param end.time Quoted variable name giving the word-level end time.
#' @param time.scale Character string indicating the unit of the input start
#'     and end times. Use \code{"centi"} for hundredths of a second or
#'     \code{"sec"} for seconds. Default is \code{"centi"}.
#' @param score Quoted variable name giving the word-level accuracy indicator,
#'     coded \code{1} for a correctly read word and \code{0} otherwise.
#'
#' @details
#' When \code{agg.level = "sentence"}, the output contains one row per
#' person-by-passage-by-sentence combination. When
#' \code{agg.level = "passage"}, the output contains one row per
#' person-by-passage combination.
#'
#' Regardless of the input \code{time.scale}, the returned elapsed-time
#' variable is expressed in seconds. Observed WCPM is calculated as the number
#' of words read correctly divided by elapsed time in seconds, multiplied by
#' 60.
#'
#' @return A data frame containing the aggregated response information.
#'     Sentence-level output includes \code{wrc}, \code{secs},
#'     \code{nwords.sen}, and \code{wcpm.sen}. Passage-level output includes
#'     \code{wrc}, \code{secs}, \code{nwords.pas}, and \code{wcpm.pas}.
#'
#' @import tidyverse
#'
#' @export
agg.word <- function(data, 
                     agg.level="sentence", 
                     person.id="", 
                     passage.id="", 
                     word.pos.sen="",  
                     word.pos.pas="",
                     sen.pos.pas="",
                     start.time="", 
                     end.time="",
                     time.scale="centi",
                     score=""){
  if(agg.level=="sentence"){
    
    col_sel <- c(person.id, passage.id, word.pos.sen, sen.pos.pas, start.time, end.time, score)
    
    if(all(col_sel %in% colnames(data))!=T){ 
      stop("Error: At least one of your column names are not correct!")
    }
    
    dat_sel <- data %>%
      select(all_of(col_sel))
    
    col_lab <- c("person.id", "passage.id", "word.pos.sen", "sen.pos.pas", "start.time", "end.time", "score")
    
    colnames(dat_sel) <- col_lab
    
    if(time.scale!="centi" & time.scale!="sec"){
      stop("Error: Check your time.scale argument!")
    }else if (time.scale=="sec"){
      dat_sel <- dat_sel %>%
        mutate(start.time=start.time*100, 
               end.time=end.time*100)
    }
    
    dat_agg <- dat_sel %>%
      group_by(person.id, passage.id, sen.pos.pas) %>%
      summarise(wrc=sum(score, na.rm = T), 
                secs=(max(end.time, na.rm = T)-min(start.time[start.time > 0], na.rm = T))/100, 
                nwords.sen=max(word.pos.sen, na.rm = T), 
                wcpm.sen=wrc/secs*60)
    
  }else if (agg.level=="passage"){
    
    col_sel <- c(person.id, passage.id, word.pos.pas, start.time, end.time, score)
    
    if(all(col_sel %in% colnames(data))!=T){ 
      stop("Error: At least one of your column names are not correct!")
    }
    
    dat_sel <- data %>%
      select(all_of(col_sel))
    
    col_lab <- c("person.id", "passage.id", "word.pos.pas", "start.time", "end.time", "score")
    
    colnames(dat_sel) <- col_lab
    
    if(time.scale!="centi" & time.scale!="sec"){
      stop("Error: Check your time.scale argument!")
    }else if (time.scale=="sec"){
      dat_sel <- dat_sel %>%
        mutate(start.time=start.time*100, 
               end.time=end.time*100)
    }
    
    dat_agg <- dat_sel %>%
      group_by(person.id, passage.id) %>%
      summarise(wrc=sum(score, na.rm = T), 
                secs=(max(end.time, na.rm = T)-min(start.time[start.time > 0], na.rm = T))/100, 
                nwords.pas=max(word.pos.pas, na.rm = T), 
                wcpm.pas=wrc/secs*60)
  }else{
    stop("\nIncorrect agg.level specification! It should be either `sentence` or `passage`")
  }
  return(dat_agg)
  
}
#' Summarize response data
#'
#' Computes descriptive summaries of task-level or sub-task-level response
#' data. Summaries can be produced at the person level or aggregated across
#' the sample. For oral reading fluency (ORF) data, observed WCPM summaries
#' can also be included.
#'
#' @param data A data frame containing response data.
#' @param person.id Quoted variable name identifying persons.
#' @param task.id Quoted variable name identifying tasks. In the ORF
#'     assessment context, this is the passage identifier.
#' @param max.counts Quoted variable name giving the maximum possible count
#'     for each task or sub-task. In the ORF assessment context, this is the
#'     number of words in the passage or sentence.
#' @param obs.counts Quoted variable name giving the observed number of
#'     successful outcomes. In the ORF assessment context, this is the number
#'     of words read correctly.
#' @param time Quoted variable name giving completion time, in seconds.
#' @param sub.task.id Quoted variable name identifying sub-tasks. In the ORF
#'     assessment context, this is the sentence identifier. This argument is
#'     required when \code{testlet = TRUE}.
#' @param desc.level Character string specifying the level of the descriptive
#'     output. Use \code{"sample"} for a sample-level summary or
#'     \code{"person"} for person-level summaries. Default is
#'     \code{"sample"}.
#' @param verbose Logical controlling the amount of sample-level output.
#'     When \code{TRUE}, the full result from \code{psych::describe()} is
#'     returned. When \code{FALSE}, a reduced set of summary statistics is
#'     returned. Default is \code{TRUE}.
#' @param type Character string specifying the output type. If
#'     \code{"general"}, general count and time summaries are produced.
#'     If \code{"orf"}, observed WCPM summaries are additionally produced.
#'     Default is \code{"general"}.
#' @param testlet Logical. If \code{FALSE}, task-level data are summarized.
#'     If \code{TRUE}, sub-task-level data are summarized while retaining
#'     their task structure. Default is \code{FALSE}.
#'
#' @details
#' For task-level data, person-level summaries include the number of observed
#' tasks and totals for maximum counts, observed counts, and time. With
#' \code{type = "orf"}, the function also calculates passage-level observed
#' WCPM, the average passage WCPM for each person, and an overall observed
#' WCPM based on total correct words and total time.
#'
#' For sub-task-level data, person-level summaries additionally include the
#' number of distinct sub-tasks. With \code{type = "orf"}, sentence-level
#' observations are first aggregated within passages before passage-level and
#' overall observed WCPM summaries are calculated.
#'
#' @return If \code{desc.level = "person"}, returns a person-level data frame.
#'     If \code{desc.level = "sample"}, returns descriptive statistics
#'     generated by \code{psych::describe()}; when \code{verbose = FALSE},
#'     only the variables, sample size, mean, standard deviation, minimum, and
#'     maximum are retained.
#'
#' @import tidyverse
#'
#' @export
desc.data <- function(data=NULL, 
                      person.id=NULL, 
                      task.id=NULL, 
                      max.counts=NULL, 
                      obs.counts=NULL, 
                      time=NULL, 
                      sub.task.id=NULL, 
                      desc.level="sample", 
                      verbose=T,
                      type="general",
                      testlet=F){
  
  if(testlet==F & !is.null(sub.task.id)){
    stop("Error: You chose testlet=F but provided a sub.task.id! If the data contain testlets, choose testlet=T.")
  }
  
  if(testlet==T & is.null(sub.task.id)){
    stop("Error: You chose testlet=T but did not provide a sub.task.id!")
  }
  
  
  if(testlet==F){
    vars <- c(person.id, task.id, max.counts, obs.counts, time)
    data <- data %>% select(all_of(vars))
    colnames(data) <- c("person.id", "task.id", "max.counts", "obs.counts", "time")
    
    if(type=="general"){
      data.person <- data %>%
        group_by(person.id) %>%
        summarise(n.task=n_distinct(task.id), 
                  tot.max.counts=sum(max.counts, na.rm = T), 
                  tot.obs.counts=sum(obs.counts, na.rm = T), 
                  tot.time=sum(time, na.rm = T))
    }else if(type=="orf"){
      data.person <- data %>%
        mutate(pas.wcpm=obs.counts/time*60) %>%
        group_by(person.id) %>%
        summarise(n.pas=n_distinct(task.id), 
                  tot.max.counts=sum(max.counts, na.rm = T), 
                  tot.obs.counts=sum(obs.counts, na.rm = T), 
                  tot.time=sum(time, na.rm = T), 
                  avg.pas.wcpm=mean(pas.wcpm, na.omit=T)) %>%
        ungroup() %>%
        mutate(gen.wcpm=tot.obs.counts/tot.time*60)
    }else{
      stop("Error: type can be either `general` or `orf`!")
    }
    
  }else if (testlet==T){
    vars <- c(person.id, task.id, sub.task.id, max.counts, obs.counts, time)
    data <- data %>% select(all_of(vars))
    colnames(data) <- c("person.id", "task.id", "sub.task.id", "max.counts", "obs.counts", "time")
    
    if(type=="general"){
      data.person <- data %>%
        mutate(unq.subtask=paste(task.id, sub.task.id, sep = "_")) %>%
          group_by(person.id) %>%
          summarise(n.task=n_distinct(task.id),
                    n.subtask=n_distinct(unq.subtask), 
                    tot.max.counts=sum(max.counts, na.rm = T), 
                    tot.obs.counts=sum(obs.counts, na.rm = T), 
                    tot.time=sum(time, na.rm = T))
    }else if (type=="orf"){
      data.person <- data %>%
        group_by(person.id, task.id) %>%
        summarise(pas.max.counts=sum(max.counts, na.rm = T), 
                  pas.obs.counts=sum(obs.counts, na.rm = T), 
                  pas.time=sum(time, na.rm = T), 
                  pas.sent=n_distinct(sub.task.id)) %>%
        ungroup() %>%
        mutate(pas.wcpm=pas.obs.counts/pas.time*60) %>%
        group_by(person.id) %>%
        summarise(n.pas=n_distinct(task.id),
                  n.sent=sum(pas.sent, na.rm=T), 
                  tot.max.counts=sum(pas.max.counts, na.rm = T), 
                  tot.obs.counts=sum(pas.obs.counts, na.rm = T), 
                  tot.time=sum(pas.time, na.rm = T), 
                  avg.pas.wcpm=mean(pas.wcpm, na.rm=T)) %>%
        ungroup() %>%
        mutate(gen.wcpm=tot.obs.counts/tot.time*60)
    }else{
      stop("Error: type can be either `general` or `orf`!")
    }
  }
  
  if(desc.level=="person"){
    return(data.person)
  }else if(desc.level=="sample"){
    samp.desc <- data.person %>%
      select(-person.id) %>%
      psych::describe()
    if(verbose==T){
      return(samp.desc)
    } else if(verbose==F){
      samp.desc.short <- samp.desc %>%
        select(vars, n, mean, sd, min, max)
      return(samp.desc.short)
    }
  } 
}

# Check whether RStan is available before Stan-based analyses.
.check_rstan <- function(context = "this analysis") {
  if (!requireNamespace("rstan", quietly = TRUE)) {
    stop(
      context, " requires the 'rstan' package. ",
      "Please install RStan before using this functionality.",
      call. = FALSE
    )
  }
  
  invisible(TRUE)
}

# Check whether runjags and JAGS are available before JAGS-based analyses.
.check_runjags <- function(context = "this analysis") {
  if (!requireNamespace("runjags", quietly = TRUE)) {
    stop(
      context, " requires the 'runjags' package and a working JAGS installation. ",
      "Please install JAGS and the 'runjags' package before using this functionality.",
      call. = FALSE
    )
  }
  
  invisible(TRUE)
}