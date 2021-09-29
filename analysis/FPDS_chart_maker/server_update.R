################################################################################
# FPDS breakdowns 2.0 app - March 2017
# V4
# Add stacked plots and drawdown period lines - Oct 2017
# server.R
################################################################################

library(shiny)
library(magrittr)
library(forcats)
library(Cairo)
library(shinyBS)
library(csis360)
#library(diigtheme1)
library(stringr)
library(plyr)
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(data.table)
library(gridExtra)
library(grid)
library(gtable)
library(ggpubr)
library(showtext)
font_add_google("Open Sans")


shinyServer(function(input, output, session) {
  showtext.auto()
  options(scipen = 99)
  options(shiny.maxRequestSize=1000*1024^2)
  source("FPDS_breakdowns_functions.R")
  
  # read data
  load("2017_unaggregated_FPDS.Rda")
  original_data<-full_data
  # original_data <- read_csv("2016_unaggregated_FPDS.csv")
  
  # in case user renames the data-frame choosing variables
  vars <- reactiveValues(
    fiscal_year = "fiscal_year",
    user_title = "None")
  
  # create working copies of the data for user modification, while retaining
  # the original data in case the user wants to reset to it
  current_data <- original_data
  changed_data <- original_data
  
  # fill the variable lists in the ui with variables from current_data
  populate_ui_var_lists(current_data)
  

  
  
  make_chart_from_input <- function(
    chart_geom,
    y_var,
    color_var = "None",
    facet_var = "None",
    labels_and_colors,
    start_fy = NULL,
    end_fy = NULL,
    show_period = "No",
    y_total_or_share = "As Total", #Default to As Total? I'm not sure what it should be.
    filetype = NULL
  ){
    # Builds a ggplot based on user settings, for display on the main panel.
    # Reactive binding will cause the ggplot to update when the user changes any
    # relevant setting.
    #
    # Returns:
    #   a fully built ggplot object
    # get appropriately formatted data to use in the plot
    # browser()
    total_data <- csis360::format_data_for_plot(data=current_data,
                                                share=FALSE,
                                                fy_var=vars$fiscal_year,
                                                start_fy=start_fy,
                                                end_fy=end_fy,
                                                y_var=y_var,
                                                color_var=color_var,
                                                facet_var=facet_var,
                                                labels_and_colors=labels_and_colors)
    share_data <- csis360::format_data_for_plot(data=current_data,
                                                share=TRUE,
                                                fy_var=vars$fiscal_year,
                                                start_fy=start_fy,
                                                end_fy=end_fy,
                                                y_var=y_var,
                                                color_var=color_var,
                                                facet_var=facet_var,
                                                labels_and_colors=labels_and_colors)
    
    # build plot with user-specified geoms
    # build plot with user-specified geoms
    if(chart_geom == "Period Stacked"){
      # make the stacked plot
      # produce the single bar plot and line plot
      bar_plot <-  build_plot(data=total_data,
                              chart_geom="Bar Chart",
                              share=FALSE,
                              x_var=vars$fiscal_year,
                              y_var=y_var,
                              color_var=color_var,
                              facet_var=facet_var,
                              labels_and_colors=labels_and_colors,
                              column_key=column_key,
                              legend=FALSE,
                              caption=FALSE)
      if (show_period == "Yes")
        bar_plot <-  add_period(bar_plot,total_data,"Bar Chart",
                                text=FALSE)
      
      
      #If there is a breakout, extract the legend
      if(color_var!="None"){
        bar_legend<-get_legend(bar_plot+theme(legend.position = "bottom", legend.margin = margin(t=-1, unit="cm")
                                              
                                              
                                              
                                              
        ))
      }
      
      bar_plot<-bar_plot+theme(legend.position = "none")
      line_plot <- build_plot(data=share_data,
                              chart_geom="Line Chart",
                              share=TRUE,
                              x_var=vars$fiscal_year,
                              y_var=y_var,
                              color_var=color_var,
                              facet_var=facet_var,
                              labels_and_colors=labels_and_colors,
                              column_key=column_key,
                              legend=FALSE,
                              caption=FALSE
      )
      if (show_period == "Yes")
        line_plot <-  add_period(line_plot,share_data,"Line Chart",
                                 text=FALSE)
      
      
      
      
      
      #Consolidate categories for Vendor Size
      period_data<-read_and_join(total_data,
                                 "Lookup_Fiscal_Year_Period.csv",
                                 directory="economic/",
                                 path="https://raw.githubusercontent.com/CSISdefense/Lookup-Tables/master/",
                                 by="fiscal_year",
                                 add_var="sequestration.period"
      )
      
      period_data <-format_period_average(data=period_data,
                                          period_var="sequestration.period",
                                          y_var=y_var,
                                          breakout=c(color_var,facet_var),
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
                              labels_and_colors=labels_and_colors,
                              column_key=column_key,
                              legend=FALSE,
                              caption=FALSE
      )
      
      
      if(color_var!="None"){
        # lay the stacked plots
        
        P1 <- ggarrange(bar_plot, 
                        ggarrange(line_plot, period_plot, ncol=2, widths = c(2.12,2.88)),
                        common.legend = TRUE, legend = "bottom",
                        nrow=2,
                        heights = c(1.2,0.8))
        # P2 <- annotate_figure(P1, 
        #                       bottom = text_grob("Source: FPDS; CSIS analysis",
        #                                          hjust = 1, x = 1, family = "Open Sans" , color = "#003366", face = "italic", size = 8))
        # 
        # 
        P1
        
      }
      else{
        # lay the stacked plots
        lay <- rbind(c(1,1,1,1),
                     c(1,1,1,1),
                     c(1,1,1,1),
                     c(2,2,3,3),
                     c(2,2,3,3))
        
        if(filetype == "png"){
          grid.arrange(bar_plot + 
                         font("xy.title", size = 50) +
                         font("xy.text", size = 50) +
                         font("legend.text", size = 50),
                       line_plot +
                         font("xy.title", size = 50) +
                         font("xy.text", size = 50) +
                         font("legend.text", size = 50),
                       period_plot,
                       layout_matrix = lay)
                       
        } else{
        grid.arrange(bar_plot,
                     line_plot,
                     period_plot,
                     layout_matrix = lay)
        }
      }
      # build plot with user-specified geoms
    } 
    
    else if(chart_geom == "Double Stacked"){
      # make the stacked plot
      # produce the single bar plot and line plot
      bar_plot <-  build_plot(data=total_data,
                              chart_geom="Bar Chart",
                              share=FALSE,
                              x_var=vars$fiscal_year,
                              y_var=y_var,
                              color_var=color_var,
                              facet_var=facet_var,
                              labels_and_colors=labels_and_colors,
                              column_key=column_key,
                              legend=FALSE,
                              caption=FALSE)
      if (show_period == "Yes")
        bar_plot <-  add_period(bar_plot,total_data,"Bar Chart",
                                text=FALSE)
      
      bar_plot <- bar_plot + theme(plot.margin=unit(c(.25,0.25,-.2,0.25), "cm"))
      
      
      #The line plot limits are shifted to align with the bar plot.
      line_plot <- build_plot(data=share_data,
                              chart_geom="Line Chart",
                              share=TRUE,
                              x_var=vars$fiscal_year,
                              y_var=y_var,
                              color_var=color_var,
                              facet_var=facet_var,
                              labels_and_colors=labels_and_colors,
                              legend = FALSE,
                              caption = FALSE,
                              column_key=column_key) + scale_x_continuous(
                                limits = c(start_fy-0.5, end_fy+0.5),
                                breaks = function(x){seq(start_fy, end_fy, by = 1)},
                                labels = function(x){str_sub(as.character(x), -2, -1)}
                              )
      line_plot <- line_plot + theme(plot.margin=unit(c(-0.3,0.25,0,0.25), "cm"))
      bar_plot$width<-line_plot$width
      if (show_period == "Yes")
        line_plot <-  add_period(line_plot,share_data,"Line Chart",
                                 text=FALSE)
      
      
      if(color_var!="None"){
        # lay the stacked plots
        
        
        if(show_legend == "Yes"){
          
          #increase the font size for downloading plot version "png"
          if(filetype == "png") {
            P1 <- ggarrange(barplot + 
                              font("xy.title", size = 50) +
                              font("xy.text", size = 50) +
                              font("legend.text", size = 50),
                            line_plot +
                              font("xy.title", size = 50) +
                              font("xy.text", size = 50) +
                              font("legend.text", size = 50),
                            common.legend = TRUE,
                            legend = "bottom",
                            nrow = 2)
            P1
          } else{
            P1 <- ggarrange(bar_plot, line_plot,
                          common.legend = TRUE, legend = "bottom",
                          nrow=2,
                          heights = c(1.2,0.8))
          # P2 <- annotate_figure(P1, 
          #   bottom = text_grob("Source: FPDS; CSIS analysis",
          #     hjust = 1, x = 1, family = "Open Sans" , color = "#003366", face = "italic", size = 8))
          # 
          # P2
          P1
          }
        }
        else{ 
          
          #increase the font size for downloading plot version "png"
          if(filetype == "png"){
             P1 <- ggarrange(bar_plot +
                               font("xy.title", size = 50) +
                               font("xy.text", size = 50) +
                               font("legend.text", size = 50),
                             line_plot +
                               font("xy.title", size = 50) +
                               font("xy.text", size = 50) +
                               font("legend.text", size = 50),
                             common.legend = FALSE,
                             nrow = 2,
                             heights = c(1.2, 0.8)
                             )
             P1
          } else{
            P1 <- ggarrange(bar_plot,
                            line_plot,
                            common.legend = FALSE,
                            nrow = 2,
                            heights = c(1.2, 0.8))
            P1
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
          grid.arrange(bar_plot +
                         font("xy.title", size = 50) +
                         font("xy.text", size = 50) +
                         font("legend.text", size = 50),
                       line_plot +
                         font("xy.title", size = 50) +
                         font("xy.text", size = 50) +
                         font("legend.text", size = 50),
                       layout_matrix = lay)
        }
        else{
          grid.arrange(bar_plot,
                       line_plot, 
                       layout_matrix = lay)
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
      
      if(show_legend == "No"){
        mainplot <- build_plot(data=plot_data,
                               chart_geom=chart_geom,
                               share= ifelse(y_total_or_share == "As Share",TRUE,FALSE),
                               x_var=vars$fiscal_year,
                               y_var=y_var,
                               color_var=color_var,
                               facet_var=facet_var,
                               labels_and_colors=labels_and_colors,
                               column_key=column_key,
                               legend= FALSE,
                               caption = FALSE)  #Xinyi, remove caption: basic bar/line plot
      }
      else {
        mainplot <- build_plot(data=plot_data,
                               chart_geom=chart_geom,
                               share= ifelse(y_total_or_share == "As Share",TRUE,FALSE),
                               x_var=vars$fiscal_year,
                               y_var=y_var,
                               color_var=color_var,
                               facet_var=facet_var,
                               labels_and_colors=labels_and_colors,
                               column_key=column_key,
                               caption = FALSE)  #Xinyi, remove caption: basic bar/line plot
      }
      
      if (show_period == "Yes")
        mainplot <-  add_period(mainplot,plot_data,chart_geom,
                                text=FALSE)
      
      
      
      #diigtheme1:::diiggraph()
      
      if(show_title == TRUE){
        mainplot <- mainplot + ggtitle(title_text)
      }
      
      # return the built plot
      mainplot
    } # END OF ELSE(bar or line plot)
    
  }
  browser()
  # calls mainplot(), defined above, to create a plot for the plot output area
  # mainplot <- reactive({
  # 
  #   
  # })
  
  mainplot <- reactive({
    renderPlot({make_chart_from_input(
      chart_geom = renderPrint({input$chart_geom}),
      y_var = renderPrint({input$y_var}),
      color_var = renderPrint({input$color_var}),
      facet_var = renderPrint({input$facet_var}),
      labels_and_colors = labels_and_colors,
      start_fy = renderPrint({input$year[1]}),
      end_fy = renderPrint({input$year[2]}),
      show_period = renderPrint({input$show_period}),
      y_total_or_share = renderPrint({input$y_total_or_share}), #Default to As Total? I'm not sure what it should be.
      filetype = NULL
    )
    })
  })
  output$plot <- mainplot()
  # output$plot <- renderPlot({
  #   
  #   reactive({
  #     make_chart_from_input(
  #       chart_geom = input$chart_geom,
  #       y_var = input$y_var,
  #       color_var = input$color_var,
  #       facet_var = input$facet_var,
  #       labels_and_colors = labels_and_colors,
  #       start_fy = input$year[1],
  #       end_fy = input$year[2],
  #       show_period = input$show_period,
  #       y_total_or_share = input$y_total_or_share, #Default to As Total? I'm not sure what it should be.
  #       filetype = NULL
  #     )
  #   })
    
    # annotate_figure(mainplot(), 
    #                 bottom = text_grob("Source:FPDS; CSIS analysis",
    #                                    hjust = 1,
    #                                    x = 1,
    #                                    family = "Open Sans",
    #                                    color = "#003366",
    #                                    face = "italic",
    #                                    size = 8))
    
  # })
  
  
  # runs the download data button on the edit page
  output$download_current <- downloadHandler(
    filename = "edited_data_view.csv",
    content = function(file){
      write_csv(changed_data, file)
    }
  )
  
  # runs the download plot data button
  output$download_plot <- downloadHandler(
    filename = "plot_data.csv",
    content = function(file){
      if(input$chart_geom == "Double Stacked") {
        plotdata <- csis360::format_data_for_plot(data=current_data,
                                                  share=FALSE,
                                                  fy_var=vars$fiscal_year,
                                                  start_fy=input$year[1],
                                                  end_fy=input$year[2],
                                                  y_var=input$y_var,
                                                  color_var=input$color_var,
                                                  facet_var=input$facet_var,
                                                  labels_and_colors=labels_and_colors)
        
        sharedata <-   csis360::format_data_for_plot(data=current_data,
                                                     share=TRUE,
                                                     fy_var=vars$fiscal_year,
                                                     start_fy=input$year[1],
                                                     end_fy=input$year[2],
                                                     y_var=input$y_var,
                                                     color_var=input$color_var,
                                                     facet_var=input$facet_var,
                                                     labels_and_colors=labels_and_colors)
        
        
        joinkey <- names(sharedata)[1:ncol(sharedata)-1]
        plot_data <- left_join(plotdata, sharedata, by=joinkey)
        names(plot_data)[ncol(plot_data)] <- paste(input$y_var, ".Sharamout")
      } else{
        csis360::format_data_for_plot(data=current_data,
                                      share=ifelse(input$y_total_or_share == "As Share",TRUE,FALSE),
                                      fy_var=vars$fiscal_year,
                                      start_fy=input$year[1],
                                      end_fy=input$year[2],
                                      y_var=input$y_var,
                                      color_var=input$color_var,
                                      facet_var=input$facet_var,
                                      labels_and_colors=labels_and_colors)
      }
      write_csv(plot_data, file)
    }
  )
  
  ##Orginal code
  #runs the download PNG button
   # output$download_image <- downloadHandler(
   #   filename = "plot_image.png",
   #   content = function(file){
   #     ggsave(
   #     filename = file,
   #     plot = mainplot(),
   #     width = input$save_plot_width,
   #     height = input$save_plot_height,
   #     units = "in")
   #   }
   # )
  # 
  ##Xinyi's work
  output$download_image_PNG <- downloadHandler(
    filename = "plot_image.png",
    content = function(file){
      png(file, 
          width = input$save_plot_width,
          height = input$save_plot_height,
          res = 600,
          units = "in")
      mainplot <- reactive({
        make_chart_from_input(
          chart_geom = input$chart_geom,
          y_var = input$y_var,
          color_var = input$color_var,
          facet_var = input$facet_var,
          labels_and_colors = labels_and_colors,
          start_fy = input$year[1],
          end_fy = input$year[2],
          show_period = input$show_period,
          y_total_or_share = input$y_total_or_share,
          filetype = "png"
        )
      })
      dev.off()
      # ggsave(
      #   filename = file,
      #   plot = annotate_figure(mainplot + theme(text = element_text(size = 50)),
      #                          bottom = text_grob("Source:FPDS; CSIS analysis",
      #                                             hjust = 1,
      #                                             x = 1,
      #                                             family = "Open Sans",
      #                                             color = "#003366",
      #                                             face = "italic",
      #                                             size = 35)),
      #   width = input$save_plot_width,
      #   height = input$save_plot_height,
      #   device = "png",
      #   units = "in",
      #   dpi = 600)
    }
  )
  
  
  
  
  output$download_image_EPS <- downloadHandler(
    filename = "plot_image.eps",
    content = function(file){
      ggsave(
        filename = file,
        plot = annotate_figure(mainplot() + theme(text = element_text(size = 50)),
                               bottom = text_grob("Source:FPDS; CSIS analysis",
                                                  hjust = 1,
                                                  x = 1,
                                                  family = "Open Sans",
                                                  color = "#003366",
                                                  face = "italic",
                                                  size = 35)),
        device = "eps",
        width = input$save_plot_width,
        height = input$save_plot_height,
        units = "in",
        dpi = 600)
    }
  )
  
  # populate and depopulate ui elements when the user changes tabs
  observeEvent(input$current_tab, {
    if(input$current_tab == "Edit Data"){
      populate_edit_var(current_data, input)
      create_edit_values_list(current_data, input)
    } else {
      clear_edit_ui(input)
      populate_ui_var_lists(current_data)
      changed_data <<- current_data
    }
  })
  
  
  # change ui elements when the user selects a different variable in the edit tab
  observeEvent(input$edit_var, {
    # change the variable rename text box
    updateTextInput(
      session,
      inputId = "rename_var_txt",
      value = input$edit_var
    )
    # delete previous values edit box
    removeUI(selector = "#edit_value_select")
    # make a new values edit box
    create_edit_values_list(changed_data, input)
  })
  
  
  # drop values from all frames at user request
  observeEvent(input$drop_value_btn, {
    
    changed_data <<- changed_data %>%
      drop_from_frame(input$edit_var, input$edit_value)
    
    # update edit_value list to reflect dropped value
    removeUI(selector = "#edit_value_select")
    create_edit_values_list(changed_data, input)
  })
  
  
  # discard all factor levels except the selected level, when the user clicks
  # the "keep" button
  observeEvent(input$keep_value_btn, {
    
    dropped <- unique(changed_data[[input$edit_var]])
    dropped <- dropped[dropped != input$edit_value]
    
    changed_data <<- changed_data %>%
      drop_from_frame(input$edit_var, dropped)
    
    # update edit_value list to reflect dropped value
    removeUI(selector = "#edit_value_select")
    create_edit_values_list(changed_data, input)
  })
  
  # apply data changes on click of "apply changes" button
  observeEvent(input$apply_changes_btn, {
    current_data <<- changed_data
    updateTabsetPanel(
      session,
      inputId = "current_tab",
      selected = "Charts"
    )
    update_title(current_data, input, vars$user_title)
  })
  
  # discard data changes on click of "discard changes" button
  observeEvent(input$discard_btn, {
    changed_data <<- current_data
    removeUI(selector = "#edit_value_select")
    create_edit_values_list(current_data, input)
  })
  
  # restore orginal data on click of "restore original data" button
  observeEvent(input$restore_btn, {
    changed_data <<- original_data
    current_data <<- original_data
    removeUI(selector = "#edit_value_select")
    create_edit_values_list(current_data, input)
    update_title(current_data, input, vars$user_title)
    removeUI(selector = "#edit_var_select")
    populate_edit_var(changed_data, input)
    
    
  })
  
  # update title depending on variable selection
  observeEvent(input$color_var, {
    update_title(current_data, input, vars$user_title)
  })
  
  observeEvent(input$facet_var, {
    update_title(current_data, input, vars$user_title)
  })
  
  
  # tells the app to stop dynamically changing the title, when the lock title
  # button is activated
  observeEvent(input$lock_title, {
    if(input$lock_title) vars$user_title <- input$title_text
    if(!input$lock_title){
      vars$user_title <- "None"
      update_title(current_data, input, vars$user_title)
    }
  })
  
  # renames the selected variable to whatever is in the text box, when the user
  # clicks the variable rename button
  observeEvent(input$rename_var_btn, {
    if(input$rename_var_txt != "") {
      names(changed_data)[names(changed_data) == input$edit_var] <<-
        input$rename_var_txt
      
      if(input$edit_var == vars$fiscal_year) {
        vars$fiscal_year <- input$rename_var_txt
      }
      
      removeUI(selector = "#edit_var_select")
      populate_edit_var(changed_data, input)
      removeUI(selector = "#edit_value_select")
      create_edit_values_list(changed_data, input)
    }
  })
  
  # renames the selected factor level to whatever is in the text box, when
  # the user clicks the factor level rename button
  observeEvent(input$rename_value_btn, {
    if(input$rename_value_txt != "" &
       input$edit_value != "*Not a Category Variable*") {
      
      changed_data <<- rename_value(changed_data, input)
      
      if(input$edit_var == vars$fiscal_year) {
        vars$fiscal_year <- input$rename_var_txt
      }
      
      removeUI(selector = "#edit_value_select")
      create_edit_values_list(changed_data, input)
    }
  })
  
  # propagates the selected factor level's name to the rename textbox
  observeEvent(input$edit_value, {
    updateTextInput(
      session,
      inputId = "rename_value_txt",
      value = input$edit_value
    )
  })
  
  # accepts file upload
  observeEvent(input$csv_btn, {
    if(is.null(input$file_upload)) return(NULL)
    
    original_data <<- fread(
      input$file_upload$datapath,
      stringsAsFactors = TRUE,
      data.table = FALSE)
    
    vars$fiscal_year <- names(original_data)[1]
    
    if("Action.Obligation" %in% tolower(colnames(original_data))){
      sum_index <-
        which(tolower(colnames(original_data)) == "Action.Obligation")
      
      original_data <- deflate(original_data,
                               fy_var = vars$fiscal_year,
                               money_var = colnames(original_data)[sum_index]
      )
      
    }
    
    current_data <<- original_data
    changed_data <<- original_data
    
    clear_edit_ui(input)
    populate_edit_var(current_data, input)
    create_edit_values_list(current_data, input)
    
  })
  
  
  
})
