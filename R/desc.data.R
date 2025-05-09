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


