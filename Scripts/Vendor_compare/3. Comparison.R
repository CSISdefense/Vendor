#################################################################
# Data Comparision Section
#################################################################

###################################################################
# function to make the comparision file
###################################################################
data_compare <- function(
  # Args:
  govdatafile,    # filename for gov data
  csisdatafile   # filename for csis data
  #
  # Returns: a data frame
){
  library(dplyr)
  library(readr)
  
  # read tables
  govdata <- read.csv(govdatafile)
  csisdata <- read.csv(csisdatafile)
  lookuptable <- read.csv("VendorNames_Lookup_Table.csv")
  lookuptable <- lookuptable[-2:-3]
  
  # Add the parentID to govdata through lookup table
  govdata <- left_join(govdata,lookuptable, by = c("Global.Vendor.Name" = "GlobalVendorName"))
  govdata <- govdata[-7]
  
  # Add the csis data
  combinedata <- left_join(govdata,csisdata, by = c("Year"="fiscal_year","parentid"="parentid"))
  combinedata <- combinedata[,c(1,2,7,4,3,8,9,10)]
  
  # Compare the # of Amount and Action
  combinedata$Diff_Amount <- combinedata$Dollars.Obligated - combinedata$ObligatedAmount
  combinedata$Diff_Action <- combinedata$Number.of.Actions - combinedata$NumberOfActions
  combinedata$Diff_Freq <- combinedata$Number.of.Actions - combinedata$FreqCount
  
  return(combinedata)
}

#####################################################################
# Function to select top N discrepency by year
#####################################################################
select_top_N <- function(
  df,  # name of dataframe
  topn # number of top records you want
  #
  # Returns:a data frame
){
  library(dplyr)
  df$absdiff <- abs(df$Diff_Amount)
  df_top <- df %>%
    group_by(Year) %>%
    arrange(Year, desc(absdiff)) %>%
    top_n(n = topn, wt = absdiff)
  return(df_top)
}

######################################################################
# function to export multiple R objercts to one Excel Workbook
######################################################################
save.xlsx <- function (
  # This is the function to export multiple R objercts to one Excel Workbook
  # The method will work for data frames, matrices, time series, and tables
  # Args:
  file, # excel file name
  ...   # objects need to be saved into excel
  #
  # Returns: an Excel workbook
  )
{
  library(openxlsx)
  objects <- list(...)
  fargs <- as.list(match.call(expand.dots = TRUE))
  objnames <- as.character(fargs)[-c(1, 2)]
  nobjects <- length(objects)
  wb <- createWorkbook("zw")
  for (i in 1:nobjects) {
    addWorksheet(wb, objnames[i])
    writeData(wb, sheet = objnames[i], x = objects[[i]])
    print(paste("writing the", objnames[i], "df."))
  }
  
  print(paste("Workbook", file, "has", nobjects, "worksheets."))
  saveWorkbook(wb, file, overwrite = TRUE)
}


setwd("./data")

# make the comparision table
DoD_compare <- data_compare("Top_100_data_DoD.csv","csis_dod_sql_data.csv")
Army_compare <- data_compare("Top_100_data_Army.csv","csis_army_sql_data.csv")
Airforce_compare <- data_compare("Top_100_data_Airforce.csv","csis_airforce_sql_data.csv")
Navy_compare <- data_compare("Top_100_data_Navy.csv","csis_navy_sql_data.csv")

# select top 10 records each year
dod_top_10 <- select_top_N(DoD_compare,10)
army_top_10 <- select_top_N(Army_compare,10)
airforce_top_10 <- select_top_N(Airforce_compare,10)
navy_top_10 <- select_top_N(Navy_compare,10)

# save all results into one file+
save.xlsx("comparison_result_all.xlsx", dod_top_10, army_top_10, airforce_top_10, navy_top_10)

