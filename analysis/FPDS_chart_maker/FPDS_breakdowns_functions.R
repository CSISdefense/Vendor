################################################################################
# Functions for FPDS breakdowns 2.0 Shiny App - March 2017
# Add stacked plots and drawdown period lines - Oct 2017
################################################################################

library(magrittr)
library(dplyr)
library(lazyeval)
library(forcats)

populate_ui_var_lists <- function(
  # Fills the ui menus with appropriate variables from the tibble passed to it
  #
  # Args:
  data_source,    # tibble from which to populate the ui menus
  session = getDefaultReactiveDomain()  # shiny app session
){
  
  # get the class for each variable (except fiscal year)
  var_class <- sapply(data_source, class)
  
  # put numeric variables in the y_var list
  numerics <- names(data_source)[
    (var_class == "numeric" | var_class == "integer") &
      !tolower(colnames(data_source)) %in% c("fiscal.year","fiscal_year")]
  updateSelectInput(session, "y_var", choices = numerics)
  
  # put categorical variables in the color_var and facet_var lists
  categories <- names(data_source)[var_class == "factor"]
  categories <- c("None", categories)
  updateSelectInput(session, "color_var", choices = categories)
  updateSelectInput(session, "facet_var", choices = categories)
}


#When facet !="None"
# theme(strip.background = element_rect(colour = "#554449", fill = "white", size=0.5),
# panel.border = element_rect(colour = "#554449", fill=NA, size=0.5)) +

add_period <- function(
  # Args:
  main_plot,    # a plot of the data, should be drawn using build_plot
  plot_data,
  chart_geom, #Line chart or bar chart
  color='#808389',
  text=TRUE #Whether or not to show period names
  #
  # Returns:
  #   A ggplot object including user-specified geom layer
){
  
  # add 4 drawdown periods
  # specify four drawdown periods
  period <- c("Pre-drawdown", "Start of Drawdown", "BCA decline period", "Current")
  startFY <- c(2009, 2011, 2013, 2016)
  endFY <- c(2010,2012,2015,2016)
  drawdownpd <- data.frame(period, startFY, endFY)
  if(chart_geom == "Line Chart") {
    main_plot <-main_plot+
      geom_vline(data=drawdownpd, mapping=aes(xintercept=startFY-0.5
                                              # color=period
      ),
      linetype='dashed',
      colour="black",
      size=0.3)
    if(text==TRUE)
      main_plot <-main_plot+
        geom_text(data=drawdownpd,mapping=aes(x=startFY-0.5,
                                              label=period),
                  y=(range(plot_data[,ncol(plot_data)])[1]),
                  colour=color, size=3, angle=90, vjust=1.2, hjust=0)
  } else {
    main_plot <- main_plot+
      geom_vline(data=drawdownpd, mapping=aes(xintercept=startFY-0.5),
                 linetype='solid',size=0.35,
                 colour="black") +
      if(text==TRUE)
        main_plot <-main_plot+
          geom_text(data=drawdownpd,mapping=aes(x=startFY, 
                                                label=period),
                    # y=(range(plot_data[,ncol(plot_data)])[1]),
                    y=0,
                    colour=color, size=3, angle=90, vjust=-0.5, hjust=0)
  }
  main_plot
}


populate_edit_var <- function(
  # Populates the edit_var element on the edit page, based on the current data
  
  # Args:
  current_data,    # the current data for the app
  input,           # shiny input object
  session = getDefaultReactiveDomain() # shiny app session
){
  
  # insert the variable selection list
  insertUI(
    selector = "#edit_var_placeholder",
    ui = tags$div(
      selectInput(
        inputId = "edit_var",
        label = "Variables",
        choices = names(current_data),
        selected = names(current_data)[1],
        multiple = FALSE,
        selectize = FALSE,
        size = length(names(current_data))
      ),
      id = "edit_var_select"
    )
  )
  
  
  # update the variable renaming text box
  updateTextInput(
    session,
    inputId = "rename_var_txt",
    value = names(current_data)[1]
  )
  
}


create_edit_values_list <- function(
  # creates the list of values available for editing, when the user changes the
  # variable they are examining
  #
  # Args:
  current_data,  # current data frame in the app
  input,         # shiny input object
  session = getDefaultReactiveDomain()  # shiny session object
){
  
  
  edit_var_class <- class(unlist(
    current_data[which(names(current_data) == input$edit_var)]
  ))
  
  if(edit_var_class != "factor") {
    values_shown <- "*Not a Category Variable*"
    
    insertUI(
      selector = "#edit_value_placeholder",
      ui = tags$div(
        selectInput(
          inputId = "edit_value",
          label = "Values",
          choices = values_shown,
          multiple = FALSE,
          selectize = FALSE,
          size = 2
        ),
        id = "edit_value_select"
      )
    )
  } else {
    values_shown <- levels(unlist(
      current_data[which(names(current_data) == input$edit_var)]))
    
    insertUI(
      selector = "#edit_value_placeholder",
      ui = tags$div(
        selectInput(
          inputId = "edit_value",
          label = "Values",
          choices = values_shown,
          multiple = FALSE,
          selectize = FALSE,
          size = length(values_shown)
        ),
        id = "edit_value_select"
      )
    )
  }
  
  # update the rename text box
  updateTextInput(
    session,
    inputId = "rename_value_txt",
    value = values_shown[1]
  )
  
}


clear_edit_ui <- function(
  # removes the variable and value selection selectInputs from the Edit Data tab
  #
  # Args:
  input,    # shiny input object
  session = getDefaultReactiveDomain()  # shiny session object
){
  
  removeUI(
    selector = "#edit_value_select",
    multiple = TRUE,
    immediate = TRUE
  )
  
  removeUI(
    selector = "#edit_var_select",
    multiple = TRUE,
    immediate = TRUE
  )
  
}

drop_from_frame <- function(
  # filters out and drops factor levels from a factor in a data frame
  #
  # Args:
  passed_frame,    # the data frame, as an object
  passed_var,   # the name of the variable, as a string
  passed_levels,    # the name of the levels to drop, as a string
  session = getDefaultReactiveDomain()    # shiny session object
  #
  # Returns:
  #   The data frame with the factor level removed
){
  # stack overflow: https://tinyurl.com/mtys7xo
  passed_frame %<>%
    filter_(interp(~!val %in% passed_levels, val = as.name(passed_var)))
  
  passed_frame[[passed_var]] <- fct_drop(passed_frame[[passed_var]])
  
  return(passed_frame)
}




update_title <- function(
  # populates the title field with a dynamic title, if appropriate
  #
  # Args:
  passed_data,   # the data used in the plot
  input,    # shiny input object
  user_title,   # "None" unless the user has manually entered a title
  session = getDefaultReactiveDomain()   # shiny session object
  #
  
){
  if(user_title != "None") {
    updateTextInput(session, "title_text", value = user_title)
    return()
  }
  
  title <- input$y_var
  if(input$color_var != "None"){
    if(input$facet_var != "None"){
      title <- paste(
        title, "by", input$color_var, "and", input$facet_var)
      if(!is.null(input$second_var)){
        title <- paste(
          title, "and", input$secibd_var)
      }
    } else {
      title <- paste(title, "by", input$color_var)
    }
  } else if(input$facet_var != "None"){
    title <- paste(title, "by", input$facet_var)
  }
  
  # check for a single-level filter
  cats <- names(passed_data)[sapply(passed_data, class) == "factor"]
  for(i in seq_along(cats)){
    if(length(unique(passed_data[[i]])) == 1){
      title <- paste(unlist(unique(passed_data[[i]])), title)
    }
  }
  
  if(input$y_total_or_share == "As Total") title <- paste("Total", title)
  if(input$y_total_or_share == "As Share") title <- paste("Share of", title)
  
  updateTextInput(session, "title_text", value = title)
  
}


rename_value <- function(
  # Renames a factor level to user-specified name, in the passed data frame
  #
  # Args:
  passed_data,    # the data frame in which to rename the value
  input,     # shiny input object
  session = getDefaultReactiveDomain()    # shiny session object
  #
  # Returns: a data frame with the factor level renamed
){
  levels(passed_data[[input$edit_var]])[levels(passed_data[[
    input$edit_var]]) == input$edit_value] <- input$rename_value_txt
  
  return(passed_data)
}



get_bottom<-function(
  size=8
){
  
  c<-text_grob(caption,
               hjust = 1,
               x = 1,
               family = "Open Sans",
               color = "#003366",
               face = "italic",
               size = size)
  return(c)
  
}



make_chart_from_input <- function(
  current_data,
  chart_geom,
  y_var,
  fy_var,
  color_var = "None",
  facet_var = "None",
  second_var = NULL,
  labels_and_colors,
  column_key,
  start_fy = NULL,
  end_fy = NULL,
  show_legend= TRUE,
  show_title= "No",
  show_period = "No",
  y_total_or_share = "As Total", #Default to As Total? I'm not sure what it should be.
  filetype = "None",
  caption = "Source: FPDS; CSIS analysis"
){
  # Builds a ggplot based on user settings, for display on the main panel.
  # Reactive binding will cause the ggplot to update when the user changes any
  # relevant setting.
  #
  # Returns:
  #   a fully built ggplot object
  # get appropriately formatted data to use in the plot
  # browser()
  if(all(!is.null(second_var),facet_var==second_var | second_var=="None")) second_var<-NULL

  total_data <- format_data_for_plot(data=current_data,
                                              share=FALSE,
                                              fy_var=fy_var,
                                              start_fy=start_fy,
                                              end_fy=end_fy,
                                              y_var=y_var,
                                              color_var=color_var,
                                              facet_var=facet_var,
                                              second_var=second_var,
                                              labels_and_colors=labels_and_colors)
  
  share_data <- format_data_for_plot(data=current_data,
                                              share=TRUE,
                                              fy_var=fy_var,
                                              start_fy=start_fy,
                                              end_fy=end_fy,
                                              y_var=y_var,
                                              color_var=color_var,
                                              facet_var=facet_var,
                                              second_var=second_var,
                                              labels_and_colors=labels_and_colors)
  
  # build plot with user-specified geoms
  # build plot with user-specified geoms
  if(chart_geom %in% c("Period Stacked","Double Stacked")){
    # make the stacked plot
    # produce the single bar plot and line plot
    bar_plot <-  build_plot(data=total_data,
                            chart_geom="Bar Chart",
                            share=FALSE,
                            x_var=fy_var,
                            y_var=y_var,
                            color_var=color_var,
                            facet_var=facet_var,
                            second_var=second_var,
                            labels_and_colors=labels_and_colors,
                            column_key=column_key,
                            legend=FALSE,
                            caption=FALSE)
    
    line_plot <- build_plot(data=share_data,
                            chart_geom="Line Chart",
                            share=TRUE,
                            x_var=fy_var,
                            y_var=y_var,
                            color_var=color_var,
                            facet_var=facet_var,
                            second_var=second_var,
                            labels_and_colors=labels_and_colors,
                            column_key=column_key,
                            legend=FALSE,
                            caption=FALSE) 
    #The line plot limits are shifted to align with the bar plot.
    
    if (show_period == "Yes"){
      bar_plot <-  add_period(bar_plot,total_data,"Bar Chart",
                              text=FALSE)
      line_plot <-  add_period(line_plot,share_data,"Line Chart",
                               text=FALSE)
    }
    
    if(chart_geom =="Period Stacked"){
      #Consolidate categories for Vendor Size
      if(!"Fiscal.Year" %in% colnames(total_data))
        colnames(total_data)[colnames(total_data)==fy_var]<-"Fiscal.Year"
      period_data<-read_and_join(total_data,
                                 "Lookup_Fiscal_Year_Period.csv",
                                 directory="economic/",
                                 path="https://raw.githubusercontent.com/CSISdefense/Lookup-Tables/master/",
                                 by="Fiscal.Year",
                                 add_var="sequestration.period"
      )
      if(!fy_var %in% colnames(total_data))
        colnames(total_data)[colnames(total_data)=="Fiscal.Year"]<-fy_var
      
      period_data <-format_period_average(data=period_data,
                                          period_var="sequestration.period",
                                          y_var=y_var,
                                          breakout=c(color_var,facet_var,second_var),
                                          labels_and_colors=labels_and_colors)
      
      #Doing this manually for now
      period_data<-as.data.frame(period_data)
      period_data[,"sequestration.period"] <- ordered(period_data[,"sequestration.period"],
                                                      levels=c("Out of Period",
                                                               "Pre-drawdown",
                                                               "Start Drawdown",
                                                               "BCA Decline",
                                                               "Rebound")
      )
      
      period_plot<-build_plot(data=period_data,
                              chart_geom="Bar Chart",
                              share=FALSE,
                              x_var="sequestration.period",
                              y_var=y_var,
                              color_var=color_var,
                              facet_var=facet_var,
                              second_var=second_var,
                              labels_and_colors=labels_and_colors,
                              column_key=column_key,
                              legend=FALSE,
                              caption=FALSE
      )  + 
       theme(axis.text.x = element_text(vjust = 1,hjust = 1, angle = 35))
        
      
      if(color_var!="None" & show_legend %in% c("Yes",TRUE)){
        # lay the stacked plots
        
        # mainplot <- annotate_figure(ggarrange(bar_plot, 
        #                                       ggarrange(line_plot, period_plot, ncol=2, widths = c(2.12,2.88)),
        #                                       common.legend = TRUE, legend = "bottom",
        #                                       nrow=2,
        #                                       heights = c(1.2,0.8)),
        #                             bottom=get_bottom())
        
        # P2 <- annotate_figure(mainplot, 
        #                       bottom = text_grob(caption,
        #                                          hjust = 1, x = 1, family = "Open Sans" , color = "#003366", face = "italic", size = 8))
        # 
        # 
        
        if(filetype == "png"){
          lay <- rbind(c(1,1,2,2),
                       c(1,1,2,2),
                       c(3,3,3,3),
                       c(3,3,3,3),
                       c(3,3,3,3))
          
          
          mainplot <- grid.arrange(line_plot + 
                                     theme(text = element_text(size = 45,
                                                               lineheight = 0.13)),
                                   period_plot + 
                                     theme(text = element_text(size = 45,
                                                               lineheight = 0.13)) ,
                                   bar_plot +
                                     theme(legend.position = "bottom") + 
                                     theme(text = element_text(size = 45,
                                                               lineheight = 0.13)),
                                   layout_matrix = lay,
                                   bottom = get_bottom(size = 35))
          
          
          
          # mainplot <- annotate_figure(ggarrange(ggarrange(line_plot +
          #                                                   theme(text = element_text(size = 50)), 
          #                                                 period_plot +
          #                                                   theme(text = element_text(size = 50)),
          #                                                   # labs(caption=caption) +
          #                                                   # theme(plot.caption = element_text(size = 35)), 
          #                                                 ncol=2, 
          #                                                 widths = c(2.12,2.88)),
          #                                       bar_plot +
          #                                         theme(legend.position = "bottom") + 
          #                                         theme(text = element_text(size = 50)), 
          #                                         # labs(caption=caption) +
          #                                         # theme(plot.caption = element_text(size = 35)),
          #                                       nrow=2, 
          #                                       heights = c(0.8,1.2)),
          #                             bottom = get_bottom(size = 35)
          #                             )
          #    #                         bottom=get_bottom())
        }
        else{
          mainplot <- annotate_figure(ggarrange(ggarrange(line_plot, 
                                                          period_plot, 
                                                          ncol=2, 
                                                          widths = c(2.12,2.88)),
                                                bar_plot, 
                                                common.legend = TRUE, legend = "bottom",
                                                nrow=2,
                                                heights = c(0.8,1.2)),
                                      bottom=get_bottom())
        }
        
        
        
        
      }
      else{
        # lay the stacked plots
        lay <- rbind(c(1,1,2,2),
                     c(1,1,2,2),
                     c(3,3,3,3),
                     c(3,3,3,3),
                     c(3,3,3,3))
        
        if(filetype == "png"){
          bar_plot<-bar_plot+theme(text = element_text(size = 45,
                                                       lineheight=0.13))
          line_plot<-line_plot+theme(text = element_text(size = 45,
                                                         lineheight=0.13))
          period_plot<-period_plot + 
            theme(text = element_text(size = 45,lineheight=0.13)) 
          
        }
        # else{
        mainplot<-grid.arrange(line_plot,
                               period_plot,
                               bar_plot,
                               layout_matrix = lay,
                               bottom=get_bottom(ifelse(filetype == "png",35,8)))
        #}
      }
      # build plot with user-specified geoms
    } 
    else if(chart_geom == "Double Stacked"){
      # make the stacked plot
      # produce the single bar plot and line plot
      
      
      bar_plot <- bar_plot + theme(plot.margin=unit(c(.25,0.25,-.2,0.25), "cm"))
      
      line_plot <-line_plot + scale_x_continuous(
        limits = c(start_fy-0.5, end_fy+0.5),
        breaks = function(x){seq(start_fy, end_fy, by = 1)},
        labels = function(x){str_sub(as.character(x), -2, -1)}
      )+ theme(plot.margin=unit(c(-0.3,0.25,0.3,0.25), "cm"))
      bar_plot$width<-line_plot$width
      
      if(color_var!="None"){
        # lay the stacked plots
        
        
        if(show_legend %in% c("Yes",TRUE)){
          
          #increase the font size for downloading plot version "png"
          if(filetype == "png") {
            lay <- rbind(c(1),
                         # c(1),
                         # c(1),
                         # c(1),
                         # c(2),
                         # c(2),
                         c(2))
            line_plot<-line_plot+ theme(legend.pos="bottom",
                    text = element_text(size = 45,
                                        lineheight=0.13))
            
            bar_plot<-bar_plot+theme(text = element_text(size = 45,
                                                          lineheight=0.13))
            
            mainplot <- grid.arrange(
              bar_plot,
              line_plot,
              layout_matrix = lay,
              bottom=get_bottom(ifelse(filetype == "png",35,8)))
            
          } else{
            
            
            mainplot <- annotate_figure(ggarrange(bar_plot, line_plot,
                                                  common.legend = TRUE, legend = "bottom",
                                                  nrow=2,
                                                  heights = c(1.2,0.8)),
                                        bottom=get_bottom())
            
            # P2 <- annotate_figure(mainplot, 
            #   bottom = text_grob(caption,
            #     hjust = 1, x = 1, family = "Open Sans" , color = "#003366", face = "italic", size = 8))
            # 
            # P2
            
          }
        }
        else{ 
          lay <- rbind(c(1),
                       c(1),
                       c(1),
                       c(2),
                       c(2))
          #increase the font size for downloading plot version "png"
          if(filetype == "png"){
            
            mainplot <- ggarrange(bar_plot +
                                    font("xy.title", size = 45) +
                                    font("xy.text", size = 45) +
                                    font("legend.text", size = 45) +
                                    theme(text = element_text(size = 45,lineheight=0.13)),
                                  line_plot +
                                    font("xy.title", size = 45) +
                                    font("xy.text", size = 45) +
                                    font("legend.text", size = 45) +
                                    theme(text = element_text(size = 45,lineheight=0.13))+
                                    labs(caption=caption),#+theme(plot.caption = element_text(size=35)),
                                  common.legend = FALSE,
                                  nrow = 2,
                                  heights = c(1.2, 0.8)
            )
          } else{
            
            mainplot <- ggarrange(bar_plot,
                                  line_plot+ labs(caption=caption),
                                  common.legend = FALSE,
                                  nrow = 2,
                                  heights = c(1.2, 0.8))
            
          }
        }
        
      }
      
      else{
        
        #increase the font size for downloading plot version "png"
        
        # lay the stacked plots
        lay <- rbind(c(1,1,1,1),
                     c(1,1,1,1),
                     c(1,1,1,1),
                     c(2,2,2,2),
                     c(2,2,2,2))
        
        if(filetype == "png") {
          mainplot <-grid.arrange(bar_plot + 
                         font("xy.title", size = 45) +
                         font("xy.text", size = 45) +
                         font("legend.text", size = 45) +
                         theme(text = element_text(size = 45,lineheight=0.13)),
                       line_plot + 
                         font("xy.title", size = 45) +
                         font("xy.text", size = 45) +
                         font("legend.text", size = 45) +
                         theme(text = element_text(size = 45,lineheight=0.13)),
                       layout_matrix = lay,
                       bottom=get_bottom(size=35))
        }
        else{
          mainplot <-grid.arrange(bar_plot,
                       line_plot, 
                       layout_matrix = lay,
                       bottom=get_bottom())
        }
      }
    }
  }
  else {
    # make the bar plot or line plot (total or share)
    # set the dataset for plot
    if(y_total_or_share == "As Share"){
      plot_data <- share_data
    } else {plot_data <- total_data}
    
    
    # build bar plot or line plot
    mainplot <- build_plot(data=plot_data,
                           chart_geom=chart_geom,
                           share= ifelse(y_total_or_share == "As Share",TRUE,FALSE),
                           x_var=fy_var,
                           y_var=y_var,
                           color_var=color_var,
                           facet_var=facet_var,
                           second_var=second_var,
                           labels_and_colors=labels_and_colors,
                           column_key=column_key,
                           legend= ifelse(show_legend %in% c("No",FALSE),FALSE,TRUE),
                           
                           caption = FALSE)+labs(caption=caption)
    
    #Special handling for variable data sources.
    if(facet_var=="Source"){
      if(any(is.null(second_var),second_var=="None")){
        mainplot<-mainplot+facet_grid(Source ~ ., scales="free_y")#ifelse(input$source_facet & input$facet_var=="None",
      }
      else{
        mainplot<-mainplot+facet_grid(as.formula(paste0("`",facet_var, "` ~ `", second_var, "`")), scales="free_y")
      }
      #When calendar and fiscal year are both in the date samplpe
      # if(class(full_data[,fy_var])=="Date" & chart_geom=="Bar Chart"){
      #   months<-unique(month(full_data[!is.na(full_data[,y_var]),fy_var]))
      #   if(all(months %in% c(4,7)) & length(months)==2){
      #     if (color_var =="None"){
      #       mainplot<-mainplot+geom_bar(aes_q(x = as.name(fy_var),y = as.name(y_var)),width=3.6, stat="identity")
      #     } else {
      #       mainplot<-mainplot+geom_bar(aes_q(x = as.name(fy_var),y = as.name(y_var),fill = as.name(color_var)),width=3.6, stat="identity")
      #     }
      #   }
      # }
    }
    
    if (show_period == "Yes")
      mainplot <-  add_period(mainplot,plot_data,chart_geom,
                              text=FALSE)
    
    #diigtheme1:::diiggraph()
    
    if(show_title == TRUE){
      mainplot <- mainplot + ggtitle(title_text)
    }
    
    # return the built plot
    if(filetype == "png") 
      mainplot<-mainplot+theme(text = element_text(size = 45,lineheight=0.13))+theme(plot.caption = element_text(size=35))
    
  } # END OF ELSE(bar or line plot)
  mainplot
}
