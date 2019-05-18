################################################################
# Function to get Top n rows of aggregated table
################################################################

Top_N_Aggregation <- function(
  filename,          # csv file name
  group1,            # groupby variable 1, e.g. "Fiscal_Year"
  group2,            # groupby variable 2, e.g. "Simple"
  group3 = "None",   # groupby variable 3, or you can ignore this variable
  var1,              # first summarize variable & ranking reference
  var2= "None",      # 2nd summarize variable, or you can ignore this variable
  topnum             # top number, e.g. 10, 20...
  # Return:
  # a data frame
){
  library(lazyeval)
  library(magrittr)
  library(dplyr)
  #library(readr)
  
  # read data
  data <- read.csv(filename,header = TRUE,
                   na.strings=c("NA","NULL", " "),
                   fileEncoding="UTF-8-BOM")
  
  if(group3 == "None"){
    if(var2 == "None"){
      # convert the column class of selected variable to numeric
      cols_to_change <- c(which(names(data)==var1))
      data[cols_to_change] = as.numeric(unlist(data[cols_to_change]))
      
      # Aggregation
      mydata <- data %>%
        group_by_(.dots = c(group1, group2)) %>%
        summarize_(
          sum_var1 = lazyeval::interp(~sum(var, na.rm = TRUE), var = as.name(var1)))
      
      # Rank and find where NA is, assign 10000000 rank to it, make  the rank below it -1
      sortedna <- mydata %>%
        filter_(interp(~ is.na(which_column), which_column = as.name(group2)))
      sortedna$Rank <- 1000000
      
      sorted <- mydata %>%
        filter_(interp(~ !is.na(which_column), which_column = as.name(group2))) %>%
        arrange_(.dots = c(group1,paste0("desc(sum_var1)"))) %>%
        group_by_(.dots = c(group1)) %>%
        mutate(Rank=row_number())
      # Add a column to split the Top N with others
      df <- sorted %>% mutate(Top = ifelse(Rank %in% 1:topnum, 1, 0))
      
      # Split data into 2 parts
      # TopN
      df1 <- filter(df, Top == 1)
      # Not TopN
      df2 <- filter(df, Top == 0)
      # Combine notTOP10 data with the contractor NA data, and make aggregration
      df2 <- bind_rows(sortedna,df2)
      df2 <- df2 %>%
        group_by_(.dots = c(group1)) %>%
        summarize(
          sum_var1 = sum(sum_var1))
      
      # dataframe manipulation for the binding in next step
      df2$Rank <- 1000000
      
      df2[,group2]  <- "all other"
      
      df2 <- df2[,c(1,2,4,3)]
      df1 <- df1[-5]
      
      # Combine two data: TOP 10 and all the rest
      fulldata <- bind_rows(df1,df2)
      # Reorder df
      
      fulldata <- fulldata %>%
        arrange_(.dots = c(group1,paste0("Rank")))
      
      fulldata <- within(fulldata,Rank[Rank > topnum]<- "other")
      
      colname_to_change <- c(which(names(fulldata)=="sum_var1"))
      
      names(fulldata)[colname_to_change] <- c(var1)
    } else{
      # convert the column class of selected variable to numeric
      cols_to_change <- c(which(names(data)==var1),which(names(data)==var2))
      data[cols_to_change] = as.numeric(unlist(data[cols_to_change]))
      
      # Aggregation
      mydata <- data %>%
        group_by_(.dots = c(group1, group2)) %>%
        summarize_(
          sum_var1 = lazyeval::interp(~sum(var, na.rm = TRUE), var = as.name(var1)),
          sum_var2 = lazyeval::interp(~sum(var, na.rm = TRUE), var = as.name(var2)))
      
      # Rank and find where NA is, assign 10000000 rank to it, make  the rank below it -1
      sortedna <- mydata %>%
        filter_(interp(~ is.na(which_column), which_column = as.name(group2)))
      sortedna$Rank <- 1000000
      
      sorted <- mydata %>%
        filter_(interp(~ !is.na(which_column), which_column = as.name(group2))) %>%
        arrange_(.dots = c(group1,paste0("desc(sum_var1)"))) %>%
        group_by_(.dots = c(group1)) %>%
        mutate(Rank=row_number())
      # Add a column to split the Top N with others
      df <- sorted %>% mutate(Top = ifelse(Rank %in% 1:topnum, 1, 0))
      
      # Split data into 2 parts
      # TopN
      df1 <- filter(df, Top == 1)
      # Not TopN
      df2 <- filter(df, Top == 0)
      # Combine notTOP10 data with the contractor NA data, and make aggregration
      df2 <- bind_rows(sortedna,df2)
      df2 <- df2 %>%
        group_by_(.dots = c(group1)) %>%
        summarize(
          sum_var1 = sum(sum_var1),
          sum_var2 = sum(sum_var2))
      
      # dataframe manipulation for the binding in next step
      df2$Rank <- 1000000
      
      df2[,group2]  <- "all other"
      
      df2 <- df2[,c(1,2,5,3,4)]
      df1 <- df1[-6]
      
      # Combine two data: TOP 10 and all the rest
      fulldata <- bind_rows(df1,df2)
      # Reorder df
      
      fulldata <- fulldata %>%
        arrange_(.dots = c(group1,paste0("Rank")))
      
      fulldata <- within(fulldata,Rank[Rank > topnum]<- "other")
      
      colname_to_change <- c(which(names(fulldata)=="sum_var1"),which(names(fulldata)=="sum_var2"))
      
      names(fulldata)[colname_to_change] <- c(var1,var2)
    }
    
  }else{
    if(var2 == "None"){
      # convert the column class of selected variable to numeric
      cols_to_change <- c(which(names(data)==var1))
      data[cols_to_change] = as.numeric(unlist(data[cols_to_change]))
      
      # Aggregation
      mydata <- data %>%
        group_by_(.dots = c(group1, group2, group3)) %>%
        summarize_(
          sum_var1 = lazyeval::interp(~sum(var, na.rm = TRUE), var = as.name(var1)))
      
      # Rank and find where NA is, assign 10000000 rank to it, make  the rank below it -1
      sortedna <- mydata %>%
        filter_(interp(~ is.na(which_column), which_column = as.name(group3)))
      sortedna$Rank <- 1000000
      
      sorted <- mydata %>%
        filter_(interp(~ !is.na(which_column), which_column = as.name(group3))) %>%
        arrange_(.dots = c(group1,group2, paste0("desc(sum_var1)"))) %>%
        group_by_(.dots = c(group1, group2)) %>%
        mutate(Rank=row_number())
      # Add a column to split the Top N with others
      df <- sorted %>% mutate(Top = ifelse(Rank %in% 1:topnum, 1, 0))
      
      # Split data into 2 parts
      # TopN
      df1 <- filter(df, Top == 1)
      # Not TopN
      df2 <- filter(df, Top == 0)
      # Combine notTOP10 data with the contractor NA data, and make aggregration
      df2 <- bind_rows(sortedna,df2)
      df2 <- df2 %>%
        group_by_(.dots = c(group1, group2)) %>%
        summarize(
          sum_var1 = sum(sum_var1))
      
      # dataframe manipulation for the binding in next step
      df2$Rank <- 1000000
      
      df2[,group3]  <- "all other"
      
      df2 <- df2[,c(1,2,5,3,4)]
      df1 <- df1[-6]
      
      # Combine two data: TOP 10 and all the rest
      fulldata <- bind_rows(df1,df2)
      # Reorder df
      
      fulldata <- fulldata %>%
        arrange_(.dots = c(group1,group2, paste0("Rank")))
      
      fulldata <- within(fulldata,Rank[Rank > topnum]<- "other")
      
      colname_to_change <- c(which(names(fulldata)=="sum_var1"))
      
      
      names(fulldata)[colname_to_change] <- c(var1)
    } else{
      # convert the column class of selected variable to numeric
      cols_to_change <- c(which(names(data)==var1),which(names(data)==var2))
      data[cols_to_change] = as.numeric(unlist(data[cols_to_change]))
      
      # Aggregation
      mydata <- data %>%
        group_by_(.dots = c(group1, group2, group3)) %>%
        summarize_(
          sum_var1 = lazyeval::interp(~sum(var, na.rm = TRUE), var = as.name(var1)),
          sum_var2 = lazyeval::interp(~sum(var, na.rm = TRUE), var = as.name(var2)))
      
      # Rank and find where NA is, assign 10000000 rank to it, make  the rank below it -1
      sortedna <- mydata %>%
        filter_(interp(~ is.na(which_column), which_column = as.name(group3)))
      sortedna$Rank <- 1000000
      
      sorted <- mydata %>%
        filter_(interp(~ !is.na(which_column), which_column = as.name(group3))) %>%
        arrange_(.dots = c(group1,group2, paste0("desc(sum_var1)"))) %>%
        group_by_(.dots = c(group1, group2)) %>%
        mutate(Rank=row_number())
      # Add a column to split the Top N with others
      df <- sorted %>% mutate(Top = ifelse(Rank %in% 1:topnum, 1, 0))
      
      # Split data into 2 parts
      # TopN
      df1 <- filter(df, Top == 1)
      # Not TopN
      df2 <- filter(df, Top == 0)
      # Combine notTOP10 data with the contractor NA data, and make aggregration
      df2 <- bind_rows(sortedna,df2)
      df2 <- df2 %>%
        group_by_(.dots = c(group1, group2)) %>%
        summarize(
          sum_var1 = sum(sum_var1),
          sum_var2 = sum(sum_var2))
      
      # dataframe manipulation for the binding in next step
      df2$Rank <- 1000000
      
      df2[,group3]  <- "all other"
      
      df2 <- df2[,c(1,2,6,3,4,5)]
      df1 <- df1[-7]
      
      # Combine two data: TOP 10 and all the rest
      fulldata <- bind_rows(df1,df2)
      # Reorder df
      
      fulldata <- fulldata %>%
        arrange_(.dots = c(group1,group2, paste0("Rank")))
      
      fulldata <- within(fulldata,Rank[Rank > topnum]<- "other")
      
      colname_to_change <- c(which(names(fulldata)=="sum_var1"),which(names(fulldata)=="sum_var2"))
      
      
      names(fulldata)[colname_to_change] <- c(var1,var2)
    }
    
  }
  
  return(fulldata)
  
}


df_top <- Top_N_Aggregation(filename = "Vendor.SP_TopVendorHistoryBucketSimple.csv",
                            group1 = "Fiscal_Year",
                            group2 = "Simple",
                            group3 = "ContractorDisplayName",
                            var1 = "SumOfobligatedAmount",
                            #var2 = "SumOfnumberOfActions",
                            topnum = 15)


library(readr)
write_csv(df_top, "K:/R-Shiny/Interns/Zhian/Vendor/Top_15.csv")
