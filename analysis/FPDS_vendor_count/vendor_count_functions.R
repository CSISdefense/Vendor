################################################################################
# Functions for vendor count Shiny App - March 2017
#
################################################################################

library(magrittr)
library(tidyverse)
library(lazyeval)
library(forcats)
library(dplyr)

populate_ui_var_lists <- function(
  # Fills the ui menus with appropriate variables from the tibble passed to it
  #
  # Args:
  data_source,    # tibble from which to populate the ui menus  
  session = getDefaultReactiveDomain()  # shiny app session
  ){
  
  # get the class for each variable (except fiscal year)
  var_class <- sapply(data_source[-1], class)
  
  # put numeric variables in the y_var list
  numerics <- names(data_source[-1])[
    var_class == "numeric" | var_class == "integer"]
  updateSelectInput(session, "y_var", choices = numerics)
  updateSelectInput(session, "y_var_2", choices = c("None", numerics))
  
  # put categorical variables in the color_var and facet_var lists
  categories <- names(data_source[-1])[var_class == "factor"]
  categories <- c("None", categories)
  updateSelectInput(session, "color_var", choices = categories)
  updateSelectInput(session, "facet_var", choices = categories)
}  

format_data_for_plot <- function(
  # Returns data in the appropriate format for the user-specified plot
  #
  # Args:
  incoming_data,   # data to format for the plot, as a tibble
  fy_var,          # name of fiscal year variable, as string
  input,           # shiny input object
  session = getDefaultReactiveDomain()  # shiny app session
  #
  # Returns:
  #   a tibble of formatted data
  ){  

  shown_data <- incoming_data
  breakouts <- c(input$color_var, input$facet_var, input$color_var_2)
  breakouts <- breakouts[breakouts != "None"]
  
  # account for potential spaces in breakouts and fy_var
  if(grepl(" ", fy_var)) fy_var <- paste0("`", fy_var, "`")
  if(length(breakouts) >= 1){
    if(grepl(" ", breakouts[1])) breakouts[1] <- paste0("`", breakouts[1], "`")
  }
  if(length(breakouts) == 2){
    if(grepl(" ", breakouts[2])) breakouts[2] <- paste0("`", breakouts[2], "`")
  }
  # filter by year - see https://tinyurl.com/lm2u8xs
  shown_data %<>%
    filter_(paste0(fy_var, ">=", as.character(input$year[1]), "&", fy_var,
                   "<=", as.character(input$year[2])))
  
  # aggregate to the level of [fiscal year x breakouts]
  # the evaluation for dplyr::summarize_ was a pain in the ass to figure out;
  # see stack overflow at https://tinyurl.com/z82ywf3
  
  if(length(breakouts) == 0){
    if(input$y_var_2 != "None"){
      if(!fy_var %in% colnames(shown_data)) 
        stop(paste("shown_data is missing variable from the group by list:", fy_var) )
      shown_data_1 <- shown_data %>%
        group_by_(fy_var) %>%
        summarize_(
          sum_val = interp(~sum(var, na.rm = TRUE), var = as.name(input$y_var)))
      shown_data_2 <- shown_data %>%
        group_by_(fy_var) %>%
        summarize_(
          sum_val_2 = interp(~sum(var, na.rm = TRUE), var = as.name(input$y_var_2)))
      
    } else{
      if(!fy_var %in% colnames(shown_data)) 
        stop(paste("shown_data is missing variable from the group by list:", fy_var) )
      shown_data_1 <- shown_data %>%
        group_by_(fy_var) %>%
        summarize_(
          sum_val = interp(~sum(var, na.rm = TRUE), var = as.name(input$y_var)))
    }
  } else {
    if(input$y_var_2 != "None"){
      if(!fy_var %in% colnames(shown_data) | any(!breakouts %in% colnames(shown_data))) 
        stop(paste("shown_data is missing variable from the group by list:", fy_var,breakouts) )
      
      shown_data_1 <- shown_data %>%
        group_by_(.dots = c(fy_var, breakouts)) %>%
        summarize_(
          sum_val = interp(~sum(var, na.rm = TRUE), var = as.name(input$y_var)))
      shown_data_2 <- shown_data %>%
        group_by_(.dots = c(fy_var, breakouts)) %>%
        summarize_(
          sum_val_2 = interp(~sum(var, na.rm = TRUE), var = as.name(input$y_var_2)))
    } else {
      if(!fy_var %in% colnames(shown_data) | any(!breakouts %in% colnames(shown_data))) 
        stop(paste("shown_data is missing variable from the group by list:", fy_var,breakouts) )
      shown_data_1 <- shown_data %>%
        group_by_(.dots = c(fy_var, breakouts)) %>%
        summarize_(
          sum_val = interp(~sum(var, na.rm = TRUE), var = as.name(input$y_var)))
    }
  }  

  #
  # NOTE: NAs replaced with 0 here; potential data quality issue
  #
  names(shown_data_1)[which(names(shown_data_1) == "sum_val")] <- input$y_var
  shown_data_1[is.na(shown_data_1)] <- 0
  if(input$y_var_2 != "None"){
    names(shown_data_2)[which(names(shown_data_1) == "sum_val_2")] <- input$y_var_2
    shown_data_2[is.na(shown_data_2)] <- 0
  }

  # calculate shares if share checkbox is checked
  if(input$y_total_or_share == "As Share"){
    shown_data <- calculate_share(breakouts, shown_data_1, input)
    if(input$y_var_2 != "None"){
      shown_data %<>% left_join(
        calculate_share(breakouts, shown_data_2, input))
    }
  }
  
  # calculate change from baseline if change from baseline checkbox is checked
  if(input$y_total_or_share == "As Change From Base"){
    shown_data <- calculate_cfb(breakouts, shown_data_1, input)
    if(input$y_var_2 != "None"){
      shown_data %<>% left_join(
        calculate_cfb(breakouts, shown_data_2, input, two = TRUE))
    }
  }
  
  if(input$y_total_or_share == "As Total") return(shown_data_1)
  
  
  # For the case where the user displays shares not broken out by any variable.
  # This is going to make a very boring chart of 100% shares, 
  # but it's handled here to avoid displaying an error.
  # if(input$y_total_or_share == "As Share" & input$color_var == "None"){
  #   shown_data %<>%
  #     mutate(total = 1)
  #   shown_data <- shown_data[which(names(shown_data) != input$y_var)]
  #   names(shown_data)[which(names(shown_data) == "total")] <- input$y_var
  # }
  
  # return the ggplot-ready data
  return(shown_data)
}

calculate_share <- function(
  # returns the share of a variable to the format_data_for_plot function
  
  breakouts,
  shown_data,
  input,
  two = TRUE
){
  
  # share_vars indicates which columns are being used to calculate the shares.
  # If there's only one breakout, it's set to -1:
  # "everything except fiscal year." 
  # With two breakouts, it's set to c(-1, -2):
  # "everything except fiscal year and the facet variable."
  share_vars <- c(-1, -length(breakouts))
  
  # spread the shares breakout variable across multiple columns
  if(input$color_var != "None") {
    if(two){
      shown_data %<>%
        spread_(input$color_var, input$y_var_2)
    } else {
      shown_data %<>%
        spread_(input$color_var, input$y_var)
    }
  }
  
  #
  # NOTE: NAs replaced with 0 here; potential data quality issue
  #
  shown_data[is.na(shown_data)] <- 0
  
  # calculate a total for each row - i.e. the total for the shares breakout
  # variable for each fiscal year,
  # or for each [fiscal year x facet variable] combo
  shown_data$total <- rowSums(shown_data[share_vars])
  
  # divide each column by the total column, to get each column as shares
  shown_data[share_vars] <-
    sapply(shown_data[share_vars], function(x){x / shown_data$total})
  shown_data %<>% select(-total)
  
  # gather the data back to long form  
  shown_data <- gather_(
    data = shown_data,
    key_col = input$color_var,
    value_col = ifelse(two, input$y_var_2, input$y_var),
    gather_cols = names(shown_data[share_vars])
  )

  return(shown_data)
}


calculate_cfb <- function(
  # returns the change from baseline for the format_data_for_plot function
  
  breakouts,
  shown_data,
  input,
  two = FALSE
){
  
  # cfb_vars indicates which columns are being used to calculate the shares.
  # If there's only one breakout, it's set to -1:
  # "everything except fiscal year." 
  # With two breakouts, it's set to c(-1, -2):
  # "everything except fiscal year and the facet variable."
  cfb_vars <- c(-1, -length(breakouts))
  
  # spread the shares breakout variable across multiple columns
  if(input$color_var != "None"){
    if(two){
      shown_data %<>%
        spread_(input$color_var, input$y_var_2)
    } else {
      shown_data %<>%
        spread_(input$color_var, input$y_var)
    }
  }
  
  #
  # NOTE: NAs replaced with 0 here; potential data quality issue
  #
  shown_data[is.na(shown_data)] <- 0
  
  # divide each column by its own first year, to get each column as change
  # from base
  shown_data[cfb_vars] <-
    sapply(shown_data[cfb_vars], function(x){x / x[1]})
  
  # gather the data back to long form
  shown_data <- gather_(
    data = shown_data,
    key_col = input$color_var,
    value_col = ifelse(two, input$y_var_2, input$y_var),
    gather_cols = names(shown_data[cfb_vars])
  )

  return(shown_data)
}


build_plot_from_input <- function(
  # Adds a geom layer to a ggplot object based on user input.  
  # Intended to handle ggplot settings that depend on user input.
  # Settings that apply universally should be added in server.R
  #
  # Args:
  plot_data,    # tibble of formatted data for the ggplot
  input,        # shiny input object
  session = getDefaultReactiveDomain() # shiny app session
  #
  # Returns:
  #   A ggplot object including user-specified geom layer
  ){
  
  mainplot <- ggplot(data = plot_data)
  
  # add a line layer, broken out by color if requested
  if(input$chart_geom == "Line Chart"){
    if(input$color_var == "None"){
      mainplot <- mainplot +
        geom_line(aes_q(
          x = as.name(names(plot_data)[1]),
          y = as.name(input$y_var)
        ))
    } else {
      mainplot <- mainplot +
        geom_line(aes_q(
          x = as.name(names(plot_data)[1]),
          y = as.name(input$y_var),
          color = as.name(input$color_var)
        ))
    }
  }
  
  # add a bar layer, broken out by color if requested
  if(input$chart_geom == "Bar Chart"){
    if(input$color_var == "None"){
      mainplot <- mainplot +
        geom_bar(aes_q(
          x = as.name(names(plot_data)[1]),
          y = as.name(input$y_var)
        ),
        stat = "identity")
    } else {
      mainplot <- mainplot +
        geom_bar(aes_q(
          x = as.name(names(plot_data)[1]),
          y = as.name(input$y_var),
          fill = as.name(input$color_var)
        ),
        stat = "identity")
    }
  }
  
  if(input$chart_geom == "Double Stacked"){
    if(input$color_var == "None"){
      mainplot <- mainplot +
        geom_bar(aes_q(
          x = as.name(names(plot_data)[1]),
          y = as.name(input$y_var)
        ),
        stat = "identity")
    } else {
      mainplot <- mainplot +
        geom_bar(aes_q(
          x = as.name(names(plot_data)[1]),
          y = as.name(input$y_var),
          fill = as.name(input$color_var_2)
        ),
        stat = "identity")
    }
  }
  
  if(input$chart_geom == "Double Stacked" & input$chart_geom == "Line Chart"){
    if(input$facet_var == "None"){
      mainplot <- mainplot +
        geom_bar(aes_q(
          x = as.name(names(plot_data)[1]),
          y = as.name(input$y_var)
        ),
        stat = "identity")
    }else {
      mainplot <- mainplot +
        geom_bar(aes_q(
          x = as.name(names(plot_data)[1]),
          y = as.name(input$y_var),
          fill = as.name(input$facet_var)
        ),
        stat = "identity")
    }
  }
    
      
  
  
  # add a second geom if requested
  if(input$chart_geom == "Line Chart" & input$y_var_2 != "None"){
    if(input$color_var == "None"){
      mainplot <- mainplot +
        geom_line(aes_q(
          x = as.name(names(plot_data)[1]),
          y = as.name(input$y_var_2)),
        color = "green"
        )
    } else {
      mainplot <- mainplot +
        geom_line(aes_q(
          x = as.name(names(plot_data)[1]),
          y = as.name(input$y_var_2),
          color = as.name(input$color_var)
        ))
    }
  }
  
  # second geom bar layer
  if(input$chart_geom == "Bar Chart" & input$y_var_2 != "None"){
    if(input$color_var == "None"){
      mainplot <- mainplot +
        geom_bar(aes_q(
          x = as.name(names(plot_data)[1]),
          y = as.name(input$y_var_2)
        ),
        stat = "identity")
    } else {
      mainplot <- mainplot +
        geom_bar(aes_q(
          x = as.name(names(plot_data)[1]),
          y = as.name(input$y_var_2),
          fill = as.name(input$color_var)
        ),
        stat = "identity")
    }
  }
  
  
  
  
  # add faceting if requested
  if(input$facet_var != "None"){
    mainplot <- mainplot +
      facet_wrap(as.formula(paste0("~ `",input$facet_var, "`"))) 
  }

  mainplot <- mainplot +
    scale_x_continuous(labels = function(x){str_sub(as.character(x), -2, -1)}) +
    scale_y_continuous(
      labels = function(x){
        sapply(x, function(y){
          if(is.na(y)) return("NA")
          y_lab <- "yuge"
          if(abs(y) < 1e15) y_lab <- paste0(round(y/1e12), "T")
          if(abs(y) < 1e13) y_lab <- paste0(round(y/1e12, 1), "T")
          if(abs(y) < 1e12) y_lab <- paste0(round(y/1e9), "B")
          if(abs(y) < 1e10) y_lab <- paste0(round(y/1e9,1), "B")
          if(abs(y) < 1e9) y_lab <- paste0(round(y/1e6), "M")
          if(abs(y) < 1e7) y_lab <- paste0(round(y/1e6,1), "M")
          if(abs(y) < 1e6) y_lab <- paste0(round(y/1000), "k")
          if(abs(y) < 1e4) y_lab <- paste0(round(y/1000, 1), "k")
          if(abs(y) < 1000) ylab <- as.character(round(y))
          if(abs(y) < 100) y_lab <- as.character(round(y,1))
          if(abs(y) < 10) y_lab <- as.character(round(y,2))
          return(y_lab)
        })
        
      })
  
  # return the plot to server.R
  return(mainplot)
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



choose_data_frame <- function(
  # chooses which level of aggregation the main plot should use
  # 
  # Args:
  platform_sub,  # the least aggregated data frame
  input,    # the shiny input object
  double_count_vars,    # vector of the names of the vars to avoid doublecounting
  session = getDefaultReactiveDomain()   # shiny session object
  #
  # Returns:
  #   The name of the data frame with the correct level of aggregation
){
  # define which variables are used
  use_sub <- (
    input$color_var == double_count_vars["SubCustomer"] |
    input$color_var_2 == double_count_vars["SubCustomer"] |
    input$facet_var == double_count_vars["SubCustomer"] |
    length(unique(platform_sub[[double_count_vars["SubCustomer"]]])) == 1)
  
  use_platform <- (
    input$color_var == double_count_vars["PlatformPortfolio"] |
    input$color_var_2 == double_count_vars["PlatformPortfolio"] |
    input$facet_var == double_count_vars["PlatformPortfolio"] |
    length(unique(platform_sub[[double_count_vars["PlatformPortfolio"]]])) == 1)
  
  if(use_sub & use_platform) return("current_platform_sub")
  if(use_sub) return("current_sub_only")
  if(use_platform) return("current_platform_only")
  return("current_top_level")
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