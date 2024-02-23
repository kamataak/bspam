#' Plot function to show graph of fit.model class
#'
#'
#' Copyright (C) 2021-2024 The ORF Project Team
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
#' @param object    fit.model object
#' @param task      task ids for plotting
#' @param parameter model parameter for plotting, a,b,alpha,beta
#' @param sort      sorting flag, TRUE or FALSE
#'
#'
#' @import plotly
#' 
#' @method plot.fit.model
#' @export
plot.fit.model <- function(object, task=NULL, parameter, sort=F){
  
  if(class(object)!="fit.model")
    stop("Error: It seems like your object is not obtained through bspam. Make sure you use the calibration output from the `fit.model()` function!")
  
  if(F %in% (parameter %in% c("a", "b", "alpha", "beta")))
    stop("Error: Please check your parameter name(s)! Make sure they are entered correctly.")
  
  #Selection of tasks if any
  if(is.null(task)){
    task.sel <- object$task.param
  }else{
    if(F %in% (task %in% object$task.param$task.id))
      stop("Error: Please check your task IDs! Make sure they are entered correctly.")
    task.sel <- object$task.param %>%
      filter(task.id %in% task)
  }
  
  if(length(parameter) == 1){
    task.sel <- task.sel %>%
      mutate(task.id=as.character(task.id)) %>%
      rename(par_grph=paste(parameter))
    
    if(sort==T){
      task.sel <- task.sel %>%
        arrange(par_grph) %>%
        mutate(task.id=factor(task.id, levels=.$task.id))
    }
    plt_obj <- task.sel %>%
      plot_ly(y=~task.id, 
              x=~par_grph, 
              type ="bar",
              text=~paste("Task ID: ", task.id, '<br>Estimate:', paste(round(par_grph,3))), 
              showlegend=F, 
              hoverinfo='text',
              textposition = "none",
              height=max(n_distinct(task.sel$task.id)*13, 400),
              color = I("#CC0035")) %>%
      layout(xaxis=list(title = paste(parameter), zeroline = F, showline = F, showticklabels = T, showgrid = T),
             yaxis=list(title = "Task ID", zeroline = F, showline = F, showticklabels = T, showgrid = T, dtick=1))
  }else if(length(parameter) == 2){
    task.sel <- task.sel %>% 
      mutate(task.id=as.character(task.id)) %>%
      rename(par_grph_x=paste(parameter[1]), par_grph_y=paste(parameter[2]))
    
    plt_obj <- task.sel %>%
      plot_ly(x=~par_grph_x, 
              y=~par_grph_y, 
              type = "scatter", 
              mode = "markers",
              text=~paste("Task ID: ", task.id, '<br>', 
                          paste(parameter[1]), ':', paste(round(par_grph_x, 3)), '<br>', 
                          paste(parameter[2]), ':', paste(round(par_grph_y,3))),
              showlegend=F,
              hoverinfo='text',
              textposition = "none",
              color = I("#CC0035")) %>%
      layout(xaxis = list(title = list(text =paste(parameter[1]))), 
             yaxis = list(title = list(text =paste(parameter[2]))))
  }
  else{
    stop("Error: Only two parameters can be visualized at a time!")
  }
  plt_obj
}
#' Plot function to show graph of scoring class
#'
#'
#' Copyright (C) 2021-2024 The ORF Project Team
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
#' @param object    scoring object
#' @param person    person ids for plotting
#' @param parameter model parameter for plotting, a,b,alpha,beta
#' @param show.se   standard error bar flag, TRUE for showing or FALSE for no showing
#' @param sort      sorting flag, TRUE or FALSE
#'
#'
#' @import plotly
#' 
#' @method plot.scoring
#' @export
plot.scoring <- function(object, person=NULL, parameter, show.se=T, sort=F){
  
  if(class(object)!="scoring")
    stop("Error: It seems like your object is not obtained through bspam. Make sure you use the scoring output from the `scoring()` function!")
  
  if(F %in% (parameter %in% c("theta", "tau", "wcpm")))
    stop("Error: Please check your parameter name(s)! Make sure they are entered correctly.")
  
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
    dplyr::select(starts_with(parameter)) %>%
    colnames()
  
  if(length(parameter) == 1){
    person.sel <- person.sel %>%
      mutate(person.id=as.factor(person.id)) %>%
      rename(par_grph=paste(parameter))
    
    if(sort==T){
      person.sel <- person.sel %>%
        arrange(par_grph) %>%
        mutate(person.id=factor(person.id, levels=.$person.id))
    }
    
    if(show.se==T){
      person.sel <- person.sel %>%
        rename(se_grph=paste("se.", parameter, sep = "")) %>%
        mutate(se_grph=1.96*se_grph) %>%
        mutate(ci_low=par_grph - se_grph, 
               ci_high=par_grph + se_grph)
      
      error_x <- list(array=~se_grph, color="lightgray", width="2")
      
      plt_obj <- person.sel %>% 
        plot_ly(y=~person.id, 
                x=~par_grph, 
                type ="scatter",
                mode = "markers",
                text=~paste("person ID: ", person.id, '<br>Estimate:', paste(round(par_grph,3)), '<br>CI Low:', paste(round(ci_low,3)), '<br>CI High:', paste(round(ci_high,3))),
                showlegend=F, 
                hoverinfo='text',
                error_x=error_x,
                textposition = "none",
                height=max(n_distinct(person.sel$person.id)*13, 400),
                color = I("#CC0035")) %>%
        layout(xaxis=list(title = paste(parameter), zeroline = F, showline = F, showticklabels = T, showgrid = T), 
               yaxis=list(title = "Person ID", zeroline = F, showline = F, showticklabels = T, showgrid = T, dtick = 1, autosize=T, automargin = TRUE, tickmode = "linear")) 
    }else{
      plt_obj <- person.sel %>% 
        plot_ly(y=~person.id, 
                x=~par_grph, 
                type ="bar",
                text=~paste("person ID: ", person.id, '<br>Estimate:', paste(round(par_grph,3))), 
                showlegend=F, 
                hoverinfo='text',
                textposition = "none",
                height=max(n_distinct(person.sel$person.id)*13, 400),
                color = I("#CC0035")) %>%
        layout(xaxis=list(title = paste(parameter), zeroline = F, showline = F, showticklabels = T, showgrid = T),
               yaxis=list(title = "Person ID", zeroline = F, showline = F, showticklabels = T, showgrid = T, dtick=1, automargin = TRUE, tickmode = "linear", autosize=T))
    }
    
  }else if(length(parameter) == 2){
    
    person.sel <- person.sel %>%
      mutate(person.id=as.character(person.id)) %>%
      rename(par_grph_x=paste(parameter[1]), par_grph_y=paste(parameter[2]))
    
    if(show.se==T){
      person.sel <- person.sel %>%
        rename(se_grph_x=paste("se.", parameter[1], sep = ""), 
               se_grph_y=paste("se.", parameter[2], sep = "")) %>%
        mutate(se_grph_x=1.96*se_grph_x,
               se_grph_y=1.96*se_grph_y) %>%
        mutate(ci_low_x=par_grph_x - se_grph_x, 
               ci_high_x=par_grph_x + se_grph_x, 
               ci_low_y=par_grph_y - se_grph_y, 
               ci_high_y=par_grph_y + se_grph_y)
      error_x <- list(array=~se_grph_x, color="lightgray", width="2")
      error_y <- list(array=~se_grph_y, color="lightgray", width="2")
    }else{
      error_x <- NULL
      error_y <- NULL
    }
    plt_obj <- person.sel %>%
      plot_ly(x=~par_grph_x, 
              y=~par_grph_y, 
              type = "scatter", 
              mode = "markers",
              error_x=error_x,
              error_y=error_y,
              text=~paste("person ID: ", person.id, '<br>', 
                          paste(parameter[1]), ':', paste(round(par_grph_x, 3)), '[', paste(round(ci_low_x,3)), ',', paste(round(ci_high_x,3)), ']', '<br>', 
                          paste(parameter[2]), ':', paste(round(par_grph_y,3)), '[', paste(round(ci_low_y,3)), ',', paste(round(ci_high_y,3)), ']'),
              showlegend=F,
              hoverinfo='text',
              textposition = "none",
              color = I("#CC0035")) %>%
      layout(xaxis = list(title = list(text =paste(parameter[1]))), 
             yaxis = list(title = list(text =paste(parameter[2]))))
  } else{
    stop("Error: Only two parameters can be visualized at a time!")
  }
  plt_obj
}