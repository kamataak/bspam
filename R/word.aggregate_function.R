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

