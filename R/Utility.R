#' This file includes utilities of bspam package.
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
#'
#' prep function prepares input data for fit.model function
#'
#' @param data = student response data
#' @param person.id Quoted variable name in \code{person.data} that indicates 
#'     the unique person identifier.
#' @param task.id Quoted variable name in \code{person.data} that indicates 
#'     the unique task identifier. In the ORF assessment context, it is the
#'     passage identifier.
#' @param sub.task.id Quoted variable name in \code{person.data} that indicates 
#'     the unique sub task identifier. In the ORF assessment context, it is the
#'     sentence identifier. It is required when sentence_level is TRUE.
#' @param occasion The column name in the data that represents the unique occasion.
#' @param group The column name in the data that represents the unique group.
#' @param max.counts Quoted variable name in \code{person.data} that indicates 
#'     the number of attempts in the task. In the ORF assessment context,
#'     it is the number of words in the passage. 
#' @param obs.counts Quoted variable name in \code{person.data} that indicates 
#'     the number of successful attempts in each task. In the ORF assessment
#'     context, it is the number of words read correctly for the passage.
#' @param time Quoted variable name in \code{person.data} that indicates 
#'     the time in seconds took to complete the task. In the ORF context, 
#'     it is the time took to complete reading the passage. 
#' @param cens The column name in the data that represents the censoring indicators 
#'      whether a specific task or sub task was censored (1) or fully observed (0).
#' @param sentence_level flag for sentence or passage level data, default is FALSE
#'      
#' @import tidyr
#' @import dplyr
#' @import tidyverse
#'
#' @return data list (data.long: data frame,
#'                    data.wide: list of Y, logT10, N, I)
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
#' Returns cases (person and occasion) applied in [fit.model] function.
#'
#' @param data = person response data
#'
#' @return cases vector
#'
#' @export
get.cases <- function(data) {
  cases <- data %>% select(person.id,occasion) %>% unique() %>%
    unite("cases", person.id:occasion, sep = "_", remove = TRUE, na.rm = FALSE) %>%
    select(cases)
  #print(cases)
  return(invisible(cases))
}

get.perfectcases <- function(data) {
  perfect.cases <- data %>% group_by(person.id,occasion) %>%
    summarise(obs.counts.sum=sum(obs.counts),
              max.counts.sum=sum(max.counts), .groups = "drop_last") %>%
    filter(obs.counts.sum == max.counts.sum) %>%
    unite("perfect.cases", person.id:occasion, sep = "_", remove = TRUE, na.rm = FALSE) %>%
    select(perfect.cases)
  return(invisible(perfect.cases))
}

get.zerocases <- function(data) {
  zero.cases <- data %>% group_by(person.id,occasion) %>%
    summarise(obs.counts.sum=sum(obs.counts),
              .groups = "drop") %>%
    filter(obs.counts.sum == 0) %>%
    unite("zero.cases", person.id:occasion, sep = "_", remove = TRUE, na.rm = FALSE) %>%
    select(zero.cases)
  return(invisible(zero.cases))
}

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

exclude_passages <- function(passage) {
  err_list <- get_errlist(passage)
  return (passage %>% filter(!(id.passage %in% err_list)))
  
}

#' 
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
#' agg.word function to aggregate word information to sentence or passage level
#'
#' @param data = word level data
#' @param agg.level indicator for sentence or passage level data, default is sentence.
#' @param person.id variable name that indicates the unique person identifier.
#' @param passage.id variable name hat indicates 
#'     the unique task identifier. In the ORF assessment context, it is the
#'     passage identifier.
#' @param word.pos.sen The column name that represents 
#'     the word's specific position sequence in a sentence. 
#' @param word.pos.pas The column name that represents 
#'     the word's specific position sequence in a passage. 
#' @param sen.pos.pas The column name that represents 
#'     the sentence's specific position sequence in a passage. 
#' @param start.time The column name in the data that represents the start time
#'     that the word has been read.
#' @param end.time The column name in the data that represents the end time
#'     that the word has been read.
#' @param time.scale indicator for the unit of reading time, default is centi.
#'     the value can be centi (hundredth) or sec (second)
#' @param score The column name in the data that represents the word correctly 
#'     has been read. 0 or 1 with 1 means correctly read.
#'     
#' @import tidyverse
#'
#' @return data frame (sentence or passage level)
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
#' desc.data function to describe data information 
#' 
#' @param data A data frame or a data generated by the \code{prep} function. 
#' @param person.id Quoted variable name in \code{data} that indicates 
#'     the unique person identifier.
#' @param task.id Quoted variable name in \code{data} that indicates 
#'     the unique task identifier. In the ORF assessment context, it is the
#'     passage identifier.
#' @param max.counts Quoted variable name in \code{data} that indicates 
#'     the number of attempts in the task. In the ORF assessment context,
#'     it is the number of words in the passage. 
#' @param obs.counts Quoted variable name in \code{data} that indicates 
#'     the number of successful attempts in each task. In the ORF assessment
#'     context, it is the number of words read correctly for the passage.
#' @param time Quoted variable name in \code{data} that indicates 
#'     the time in seconds took to complete the task. In the ORF context, 
#'     it is the time took to complete reading the passage.
#' @param sub.task.id Quoted variable name in \code{data} that indicates 
#'     the unique sub task identifier. In the ORF assessment context, it is the
#'     sentence identifier. It is required when testlet is TRUE.
#' @param desc.level Quoted string that indicates which data summarization. If \code{"sample"} (default),
#'     will show description of data sample. If \code{"person"}, will output the summarization of person data. 
#' 
#' @param verbose Boolean. If \code{TRUE}, the summary will be output.
#'     Default is \code{TRUE}.
#'     
#' @param type Quoted string, indication of the choice of output. If \code{"general"} (default),
#'      wcpm scores are not reported. If \code{"orf"}, wcpm scores will be reported.
#'      
#' @param testlet Boolean. If \code{TRUE}, runs with sub task level, otherwise, with task level.
#'      Default is \code{FALSE}.
#'    
#' @import tidyverse
#'
#' @return data frame (based on sample data or person information)
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
        unq.subtask=paste(task.id, sub.task.id, sep = "_") %>%
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