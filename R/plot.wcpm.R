plot.wcpm <- function(object, person=NULL, show.se=T, show.abline=T){
  
  if(class(object)!="scoring")
    stop("Error: It seems like your object is not obtained through bspam. Make sure you used the scoring output from the `scoring()` function!")
  
  if(!"wcpm.obs" %in% names(object))
    stop("Error: It seems like you did not use type='orf' in the scoring function.")
  
  class(object) = "list"
  object <- as.data.frame(object)
  
  #Selection of persons (if any)
  if(is.null(person)){
    person.sel <- object
  }else{
    if(F %in% (person %in% object$person.id))
      stop("Error: Please check your person IDs! Make sure they are entered correctly.")
    person.sel <- object %>%
      filter(person.id %in% person)
  }
  
  #Change Parameter Ext.
  parameter <- person.sel %>%
    dplyr::select(starts_with("wcpm")) %>% 
    colnames()
  
  person.sel <- person.sel %>%
    mutate(person.id=as.character(person.id)) %>%
    rename(par_grph_x=paste(parameter[1]), par_grph_y=paste(parameter[2]))
  
  if(show.se==T){
    person.sel <- person.sel %>%
      rename(se_grph_y=paste("se.", parameter[2], sep = "")) %>%
      mutate(se_grph_y=1.96*se_grph_y) %>%
      mutate(ci_low_y=round(par_grph_y - se_grph_y, 3), 
             ci_high_y=round(par_grph_y + se_grph_y, 3))
    error_y <- list(array=~se_grph_y, color="lightgray", width="2")
    text=~paste("person ID: ", person.id, '<br>', 
                paste(parameter[1]), ':', paste(round(par_grph_x, 3)),  '<br>', 
                paste(parameter[2]), ':', paste(round(par_grph_y,3)), '[', paste((ci_low_y)), ',', paste((ci_high_y)), ']')
  }else{
    error_y <- NULL
    ci_low_y <- NULL
    ci_high_y <- NULL
    text=~paste("person ID: ", person.id, '<br>', 
                paste(parameter[1]), ':', paste(round(par_grph_x, 3)),  '<br>', 
                paste(parameter[2]), ':', paste(round(par_grph_y,3)))
  }
  plt_obj <- person.sel %>%
    plot_ly(x=~par_grph_x, 
            y=~par_grph_y, 
            type = "scatter", 
            mode = "markers",
            error_y=error_y,
            text=text,
            showlegend=F,
            hoverinfo='text',
            textposition = "none",
            color = I("#CC0035")) %>%
    layout(xaxis = list(title = list(text =paste(parameter[1])), nticks=10,
                        range <- c(min(c(person.sel$par_grph_x, person.sel$par_grph_y)), max(c(person.sel$par_grph_x, person.sel$par_grph_y)))), 
           yaxis = list(title = list(text =paste(parameter[2])), nticks=10,
                        range <- c(min(c(person.sel$par_grph_x, person.sel$par_grph_y)), max(c(person.sel$par_grph_x, person.sel$par_grph_y)))))
  
  if(isTRUE(show.abline)){
    plt_obj <- plt_obj %>%
      add_trace(x = ~par_grph_x, y = ~par_grph_x, 
                type = 'scatter', mode = 'lines', name = 'Best Fit Line', 
                line = list(color = 'black', width=1), hoverinfo="none", error_y=NULL)
  }else{
    plt_ob <- plt_obj
  }
  plt_obj
}