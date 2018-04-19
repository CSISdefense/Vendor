################################################################################
# Vendor Count App - March 2017
#
# server.R
################################################################################

library(shiny)
library(magrittr)
library(forcats)
library(Cairo)
library(shinyBS)
library(stringr)
library(tidyverse)
library(ggplot2)
library(grid)
library(gridExtra)
library(gtable)
library(ggpubr)
library(csis360)


shinyServer(function(input, output, session) {
  options(scipen = 99)
  #windowsFonts(`Open Sans` = windowsFont("Open Sans"))
  source("vendor_count_functions.R")
  
  # read data  
  load("2016_vendor_count.Rda")
  
  # in case user renames the data-frame choosing variables
  vars <- reactiveValues(
    double_counted = c(
      "PlatformPortfolio" = "PlatformPortfolio",
      "SubCustomer" = "SubCustomer"),
    frame = "current_top_level",
    fiscal_year = "fiscal_year",
    user_title = "None")
  
  # create working copies of the data for user modification, while retaining
  # the original data in case the user wants to reset to it
  current_platform_sub <- platform_sub
  changed_platform_sub <- platform_sub
  current_platform_only <- platform_only
  changed_platform_only <- platform_only
  current_sub_only <- sub_only
  changed_sub_only <- sub_only
  current_top_level <- top_level
  changed_top_level <- top_level
  
  # fill the variable lists in the ui with variables from current_platform_sub
  populate_ui_var_lists(current_platform_sub)

  mainplot <- reactive({
    # browser()
    # Builds a ggplot based on user settings, for display on the main panel.
    # Reactive binding will cause the ggplot to update when the user changes any
    # relevant setting.  
    #  
    # Returns:
    #   a fully built ggplot object
    # get appropriately formatted data to use in the plot

    vars$frame <- 
      choose_data_frame(current_platform_sub, input, vars$double_counted)
    plot_data <- csis360::format_data_for_plot(data=get(vars$frame),
                                               share=FALSE,
                                               fy_var=vars$fiscal_year,
                                               start_fy=input$year[1],
                                               end_fy=input$year[2],
                                               y_var=input$y_var,
                                               color_var=input$color_var,
                                               facet_var=input$facet_var,
                                               labels_and_colors=labels_and_colors)
    
    
    # format_data_for_plot(get(vars$frame), vars$fiscal_year, input)
    # build plot with user-specified geoms
    if(input$chart_geom == "Double Stacked"){
      VC_Bar_Plot <- build_plot(data=plot_data,
                                chart_geom="Bar Chart",
                                share=FALSE,
                                x_var=vars$fiscal_year,
                                y_var=input$y_var,
                                color_var= input$facet_var,
                                labels_and_colors=labels_and_colors,
                                column_key=column_key,
                                legend=FALSE,
                                caption=FALSE)
      VC_Bar_Plot <-    VC_Bar_Plot + labs(y="Total #
Vendors") 
      VC_Bar_Plot <- VC_Bar_Plot + theme(plot.margin=unit(c(.25,0.25,-.2,0.25), "cm"))
      
      
      
      # build_plot_from_input(plot_data, input)
      line_plot <- build_plot(data=plot_data,
                              chart_geom="Line Chart",
                              share=FALSE,
                              x_var=vars$fiscal_year,
                              y_var=input$y_var,
                              color_var=input$color_var,
                              labels_and_colors=labels_and_colors,
                              legend=TRUE,
                              caption = TRUE,
                              column_key=column_key) +         scale_x_continuous(
                                limits = c(input$year[1]-0.5, input$year[2]+0.5),
                                breaks = function(x){seq(input$year[1], input$year[2], by = 1)},
                                labels = function(x){str_sub(as.character(x), -2, -1)}
                              )
      line_plot <-    line_plot + theme(plot.margin=unit(c(-0.3,0.25,0,0.25), "cm"))
      VC_Bar_Plot$width<-line_plot$width
      line_plot <- line_plot + labs(x = "Fiscal Year", 
                                    y = "Vendor Count 
by Vendor Size")
      
      line_plot <- line_plot+ theme(legend.position = "bottom", legend.margin=margin(t = -0.2, unit='cm'))

      lay <- rbind(c(1,1,1,1),
                   c(1,1,1,1),
                   c(2,2,2,2),
                   c(2,2,2,2),
                   c(2,2,2,2))
      grid.arrange(VC_Bar_Plot,
                   line_plot,
                   layout_matrix = lay)
    } else {
      mainplot <- build_plot(data=plot_data,
                             chart_geom=input$chart_geom,
                             share=FALSE,
                             x_var=vars$fiscal_year,
                             y_var=input$y_var,
                             color_var=input$color_var,
                             facet_var=input$facet_var,
                             labels_and_colors=labels_and_colors,
                             column_key=column_key,
                             legend=TRUE,
                             caption=TRUE)
      
      # add overall visual settings to the plot
      mainplot <- mainplot + labs(x = "Fiscal Year", y = "Vendor Count") + 
        get_plot_theme() 
      
      if(input$show_title == TRUE){
        mainplot <- mainplot + ggtitle(input$title_text) 
      }
      
      if(length(input$facet_var) == 0){
        mainplot <- mainplot +  theme(axis.text.x = element_text(
          size = 9,
          family = "Open Sans",
          vjust = 7,
          margin = margin(-10,0,0,0)))}
      else if(length(input$facet_var) > 0){
        mainplot <- mainplot +  theme(axis.text.x = element_text(
          size = 9,
          family = "Open Sans",
          vjust = 7),
          strip.background = element_rect(colour = "#554449", fill = "white", size=0.5),
          panel.border = element_rect(colour = "#554449", fill=NA, size=0.5)
        )}
      
      
      # return the built plot
      return(mainplot)
    }
    
    
  })
  
  
  output$plot <- renderPlot({
    mainplot()
  })
  
  
  output$current_frame <- renderText({
    paste("displayed data: \n", vars$frame)
  })
  
  output$download_current <- downloadHandler(
    filename = "edited_data_view.csv",
    content = function(file){
      write_csv(changed_platform_sub, file) 
    }
  )
  
  output$download_plot <- downloadHandler(
    filename = "plot_data.csv",
    content = function(file){
      write.csv(format_data_for_plot(get(vars$frame),vars$fiscal_year, input), 
                file, row.names = FALSE)
    }
  )
  
  output$download_image <- downloadHandler(
    filename = "VC_plot_image.png",
    content = function(file){
      ggsave(
        filename = file,
        plot = mainplot(),
        width = input$save_plot_width,
        height = input$save_plot_height,
        units = "in")
    }
  )
  # populate and depopulate ui elements when the user changes tabs
  observeEvent(input$current_tab, {
    if(input$current_tab == "Edit Data"){  
      populate_edit_var(current_platform_sub, input)
      create_edit_values_list(current_platform_sub, input)
    } else {
      clear_edit_ui(input)
      populate_ui_var_lists(current_platform_sub)
      changed_platform_sub <<- current_platform_sub
      changed_platform_only <<- current_platform_only
      changed_top_level <<- current_top_level
      changed_sub_only <<- current_sub_only
    }
  })
  
  
  # change ui elements when the user changes variable in the edit tab
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
    create_edit_values_list(changed_platform_sub, input)
  })
  
  
  # drop values from all frames at user request
  observeEvent(input$drop_value_btn, {
    
    changed_platform_sub <<- changed_platform_sub %>%
      drop_from_frame(input$edit_var, input$edit_value)
    
    if(input$edit_var != vars$double_counted["SubCustomer"]){
      changed_platform_only <<- changed_platform_only %>%
        drop_from_frame(input$edit_var, input$edit_value)
    }
    if(input$edit_var != vars$double_counted["PlatformPortfolio"]){
      changed_sub_only <<- changed_sub_only %>%
        drop_from_frame(input$edit_var, input$edit_value)
    }
    if(input$edit_var != vars$double_counted["PlatformPortfolio"] &
       input$edit_var != vars$double_counted["SubCustomer"]) {
      changed_top_level <<- changed_top_level %>%
        drop_from_frame(input$edit_var, input$edit_value)
    }
    
    # update edit_value list to reflect dropped value
    removeUI(selector = "#edit_value_select")
    create_edit_values_list(changed_platform_sub, input)
  })
  
  observeEvent(input$keep_value_btn, {
    
    dropped <- unique(changed_platform_sub[[input$edit_var]])
    dropped <- dropped[dropped != input$edit_value]
    
    changed_platform_sub <<- changed_platform_sub %>%
      drop_from_frame(input$edit_var, dropped)
    
    if(input$edit_var != vars$double_counted["SubCustomer"]){
      changed_platform_only <<- changed_platform_only %>%
        drop_from_frame(input$edit_var, dropped)
    }
    if(input$edit_var != vars$double_counted["PlatformPortfolio"]){
      changed_sub_only <<- changed_sub_only %>%
        drop_from_frame(input$edit_var, dropped)
    }
    if(!(input$edit_var %in% vars$double_counted)) {
      changed_top_level <<- changed_top_level %>%
        drop_from_frame(input$edit_var, dropped)
    }
    
    # update edit_value list to reflect dropped value
    removeUI(selector = "#edit_value_select")
    create_edit_values_list(changed_platform_sub, input)
  })
  
  # apply data edits when user says so
  observeEvent(input$apply_changes_btn, {
    current_platform_sub <<- changed_platform_sub
    current_platform_only <<- changed_platform_only
    current_sub_only <<- changed_sub_only
    current_top_level <<- changed_top_level
    updateTabsetPanel(
      session,
      inputId = "current_tab",
      selected = "Charts"
    )
    vars$frame <- 
      choose_data_frame(current_platform_sub, input, vars$double_counted)
    update_title(get(vars$frame), input, vars$user_title)
  })
  
  # discard data changes when user says so
  observeEvent(input$discard_btn, {
    changed_platform_sub <<- current_platform_sub
    changed_sub_only <<- current_sub_only
    changed_platform_only <<- current_platform_only
    changed_top_level <<- current_top_level
    removeUI(selector = "#edit_value_select")
    create_edit_values_list(current_platform_sub, input)
  })
  
  # restore orginal data on request
  observeEvent(input$restore_btn, {
    changed_platform_sub <<- platform_sub
    changed_platform_only <<- platform_only
    changed_sub_only <<- sub_only
    changed_top_level <<- top_level
    current_platform_sub <<- platform_sub
    current_platform_only <<- platform_only
    current_sub_only <<- sub_only
    current_top_level <<- top_level
    removeUI(selector = "#edit_value_select")
    create_edit_values_list(current_platform_sub, input)
    update_title(get(vars$frame), input, vars$user_title)
    removeUI(selector = "#edit_var_select")
    populate_edit_var(changed_platform_sub, input)
    
    
  })
  
  # choose the active data frame depending on user selections
  observeEvent(input$color_var, {
    vars$frame <- 
      choose_data_frame(current_platform_sub, input, vars$double_counted)
    update_title(get(vars$frame), input, vars$user_title)
  })
  observeEvent(input$color_var_2, {
    vars$frame <- 
      choose_data_frame(current_platform_sub, input, vars$double_counted)
    update_title(get(vars$frame), input, vars$user_title)
  })
  observeEvent(input$facet_var, {
    vars$frame <- 
      choose_data_frame(current_platform_sub, input, vars$double_counted)
    update_title(get(vars$frame), input, vars$user_title)
  })
  
  observeEvent(input$lock_title, {
    if(input$lock_title) vars$user_title <- input$title_text
    if(!input$lock_title){
      vars$user_title <- "None"
      update_title(get(vars$frame), input, vars$user_title)
    }
  })
  
  observeEvent(input$rename_var_btn, {
    if(input$rename_var_txt != "") {
      names(changed_sub_only)[names(changed_sub_only) == input$edit_var] <<-
        input$rename_var_txt
      names(changed_platform_only)[names(changed_platform_only) == input$edit_var] <<-
        input$rename_var_txt
      names(changed_platform_sub)[names(changed_platform_sub) == input$edit_var] <<-
        input$rename_var_txt
      names(changed_top_level)[names(changed_top_level) == input$edit_var] <<-
        input$rename_var_txt
      
      if(input$edit_var == vars$double_counted["SubCustomer"]) {
        vars$double_counted["SubCustomer"] <- input$rename_var_txt
      }
      
      if(input$edit_var == vars$double_counted["PlatformPortfolio"]) {
        vars$double_counted["PlatformPortfolio"] <- input$rename_var_txt
      }
      
      if(input$edit_var == vars$fiscal_year) {
        vars$fiscal_year <- input$rename_var_txt
      }
      
      removeUI(selector = "#edit_var_select")
      populate_edit_var(changed_platform_sub, input)
      removeUI(selector = "#edit_value_select")
      create_edit_values_list(changed_platform_sub, input) 
    }
  })
  
  
  observeEvent(input$rename_value_btn, {
    if(input$rename_value_txt != "" &
       input$edit_value != "*Not a Category Variable*") {
      
      changed_top_level <<- rename_value(changed_top_level, input)
      changed_platform_sub <<- rename_value(changed_platform_sub, input)
      changed_platform_only <<- rename_value(changed_platform_only, input)
      changed_sub_only <<- rename_value(changed_sub_only, input)
      
      
      if(input$edit_var == vars$double_counted["SubCustomer"]) {
        vars$double_counted["SubCustomer"] <- input$rename_var_txt
      }
      
      if(input$edit_var == vars$double_counted["PlatformPortfolio"]) {
        vars$double_counted["PlatformPortfolio"] <- input$rename_var_txt
      }
      
      if(input$edit_var == vars$fiscal_year) {
        vars$fiscal_year <- input$rename_var_txt
      }
      
      removeUI(selector = "#edit_value_select")
      create_edit_values_list(changed_platform_sub, input)
    }
  })
  
  observeEvent(input$edit_value, {
    updateTextInput(
      session,
      inputId = "rename_value_txt",
      value = input$edit_value
    )
  })
  
  
})