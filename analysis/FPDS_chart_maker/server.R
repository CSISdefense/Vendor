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
library(stringr)
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
#This requires an internet connection and is not ideal, but oddly it'll throw warnings if this isn't called even if the font is available.
font_add_google("Open Sans")


shinyServer(function(input, output, session) {
  showtext_auto()
  options(scipen = 99)
  options(shiny.maxRequestSize=1000*1024^2)
  source("FPDS_breakdowns_functions.R")
  
  # read data
  load("unaggregated_FPDS.Rda")
  original_data<-full_data

  # in case user renames the data-frame choosing variables
  vars <- reactiveValues(
    Fiscal_Year = "Fiscal_Year",
    user_title = "None")
  
  # create working copies of the data for user modification, while retaining
  # the original data in case the user wants to reset to it
  current_data <- original_data
  changed_data <- original_data
  
  # fill the variable lists in the ui with variables from current_data
  populate_ui_var_lists(current_data)
# debug(make_chart_from_input)
# debug(group_data_for_plot)
  mainplot<-reactive({make_chart_from_input(
    current_data=current_data,
    chart_geom = input$chart_geom,
    y_var = input$y_var,
    fy_var= vars$Fiscal_Year,
    color_var = input$color_var,
    facet_var = input$facet_var,
    labels_and_colors = labels_and_colors,
    column_key=column_key,
    start_fy = input$year[1],
    end_fy = input$year[2],
    show_legend=input$show_legend,
    show_period = input$show_period,
    show_title = input$show_title,
    y_total_or_share = input$y_total_or_share,
    filetype = "None"
  )
    })  
  
  
  output$plot <- renderPlot({
    # annotate_figure(
      mainplot()
                    # bottom = text_grob("Source:FPDS; CSIS analysis",
                    #                    hjust = 1,
                    #                    x = 1,
                    #                    family = "Open Sans",
                    #                    color = "#003366",
                    #                    face = "italic",
                    #                    size = 8))

    # P2 <- annotate_figure(P1,
    #   bottom = text_grob("Source: FPDS; CSIS analysis",
    #     hjust = 1, x = 1, family = "Open Sans" , color = "#003366", face = "italic", size = 8))
    #


  })
  
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
                                                  fy_var=vars$Fiscal_Year,
                                                  start_fy=input$year[1],
                                                  end_fy=input$year[2],
                                                  y_var=input$y_var,
                                                  color_var=input$color_var,
                                                  facet_var=input$facet_var,
                                                  labels_and_colors=labels_and_colors)
        
        sharedata <-   csis360::format_data_for_plot(data=current_data,
                                                     share=TRUE,
                                                     fy_var=vars$Fiscal_Year,
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
        plot_data<-csis360::format_data_for_plot(data=current_data,
                                      share=ifelse(input$y_total_or_share == "As Share",TRUE,FALSE),
                                      fy_var=vars$Fiscal_Year,
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
  size<-45
  pngplot<-reactive({make_chart_from_input(
    current_data=current_data,
    chart_geom = input$chart_geom,
    y_var = input$y_var,
    fy_var= vars$Fiscal_Year,
    color_var = input$color_var,
    facet_var = input$facet_var,
    labels_and_colors = labels_and_colors,
    column_key=column_key,
    start_fy = input$year[1],
    end_fy = input$year[2],
    show_legend=input$show_legend,
    show_period = input$show_period,
    show_title = input$show_title,
    y_total_or_share = input$y_total_or_share,
    filetype = "png"
  )+
      theme(text=element_text(size=size,lineheight=0.13),
            legend.spacing.x = unit(0.1, 'cm'),
            plot.caption = element_text(size=round(size * 5/6,0))
      )
  })
  
  output$download_image_PNG <- downloadHandler(
    filename = "plot_image.png",
    content = function(file){
      ggsave(
        filename = file,
        plot = pngplot(),
        dpi=600,
        width = input$save_plot_width,
        height = input$save_plot_height,
        device = "png",
        units = "in")
    }
  )
  
  
  epsplot<-reactive({make_chart_from_input(
    current_data=current_data,
    chart_geom = input$chart_geom,
    y_var = input$y_var,
    fy_var= vars$Fiscal_Year,
    color_var = input$color_var,
    facet_var = input$facet_var,
    labels_and_colors = labels_and_colors,
    column_key=column_key,
    start_fy = input$year[1],
    end_fy = input$year[2],
    show_legend=input$show_legend,
    show_period = input$show_period,
    show_title = input$show_title,
    y_total_or_share = input$y_total_or_share,
    filetype = "eps"
  )
  })
  
  output$download_image_EPS <- downloadHandler(
    filename = "plot_image.eps",
    content = function(file){
      ggsave(
        filename = file,
        plot = epsplot(), 
        device = "eps",
        width = input$save_plot_width,
        height = input$save_plot_height,
        units = "in")
    }
  )
  
  
  
  # svgplot<-reactive({make_chart_from_input(
  #   current_data=current_data,
  #   chart_geom = input$chart_geom,
  #   y_var = input$y_var,
  #   fy_var= vars$Fiscal_Year,
  #   color_var = input$color_var,
  #   facet_var = input$facet_var,
  #   labels_and_colors = labels_and_colors,
  #   column_key=column_key,
  #   start_fy = input$year[1],
  #   end_fy = input$year[2],
  #   show_legend=input$show_legend,
  #   show_period = input$show_period,
  #   show_title = input$show_title,
  #   y_total_or_share = input$y_total_or_share,
  #   filetype = "svg"
  # )
  # })
  # 
  # output$download_image_SVG <- downloadHandler(
  #   filename = "plot_image.svg",
  #   content = function(file){
  #     ggsave(
  #       filename = file,
  #       plot = svgplot(), 
  #       device = "svg",
  #       width = input$save_plot_width,
  #       height = input$save_plot_height,
  #       units = "in")
  #   }
  # )
  
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
      
      if(input$edit_var == vars$Fiscal_Year) {
        vars$Fiscal_Year <- input$rename_var_txt
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
      
      if(input$edit_var == vars$Fiscal_Year) {
        vars$Fiscal_Year <- input$rename_var_txt
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
    #This is a bad way to do this.
    vars$Fiscal_Year <- names(original_data)[1]
    
    if("Action.Obligation" %in% tolower(colnames(original_data))){
      sum_index <-
        which(tolower(colnames(original_data)) == "Action.Obligation")
      
      original_data <- deflate(original_data,
                               fy_var = vars$Fiscal_Year,
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
