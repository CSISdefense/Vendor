######################################################################
# Function for Data Processing for the data in FPDS website Top 100
######################################################################

# Function to combine desired sheets in all .xlsx files in one folder
combine_data_from_xlsx <- function(
  # This function is used to extract certain sheets in all excel files in one folder,
  # and combine them into one csv file
  # Please make sure the sheet names in all xlsx are exactly same
  # Args:
  folderpath,      # path of the folder, example: "./Top100data"
  xlsxnamepattern, # pattern of the name of .xlsx files, 
                   # example: "Top_100_Contractors_Report_Fiscal_Year.*\\.xlsx"
  sheetname,       # name of the sheet you want to extract
  csvfilename      # name of saved csv file
  #
  # Returns:
  # a csv file of Global.Vendor.Name, Number.of.Actions, Dollars.Obligated, %Actions and %Amount
){
  library(dplyr)
  library(tidyverse)
  ##Read xlsx files named in certain pattern
  filenames <- list.files(path = folderpath,
                          pattern = xlsxnamepattern)
  
  ##Create list of data frame names without the ".xlsx" part 
  namelen <- nchar(filenames[1]) - 5
  dfnames <-substr(filenames,1,namelen)
  print("Ready to read files.")
  ##Create a list of all data frame
  alldata <- list()
  ##Load all files
  for(i in dfnames){
    library(rlist)
    filepath <- file.path(folderpath,paste(i,".xlsx",sep=""))
    print(filepath)
    library(openxlsx)
    i<- read.xlsx(filepath, sheet = sheetname)
    alldata <- list.append(alldata,i)
  }
  print("Complete reading all xlsx files.")
  ##Rename column names and assign year column to data frame
  index = 1
  fulldata <- data.frame()
  for(df in alldata){
    library(tibble)
    # Rename colnames
    names(df) <- names(i)
    # Add a new column - Year
    df <- add_column(df, Year = substr(dfnames[index],40,43))
    # Combine all tables
    fulldata <- bind_rows(fulldata,df)
    index = index + 1
  }
  
  ##Remove the yearly sum rows and reorder columns
  fulldata <- fulldata[!(is.na(fulldata$Global.Vendor.Name) | fulldata$Global.Vendor.Name=="TOTAL"), ]
  fulldata <- fulldata[, c(6,1,2,3,4,5)]
  
  ##Save data to csv
  write_csv(fulldata, csvfilename)
  print("Complete saving csv file.")
}


#####################################################################################
##Read files named Top_100_Contractors_Report_Fiscal_Year_XXXX.xlsx
#folderpath = "./Top100data"
#xlsxnamepattern="Top_100_Contractors_Report_Fiscal_Year.*\\.xlsx"
#sheetname = "Navy (1700)"
#csvfilename = "Top_100_data_Navy.csv"
#####################################################################################
# NAVY DATA
combine_data_from_xlsx("./Top100data","Top_100_Contractors_Report_Fiscal_Year.*\\.xlsx",
                       "Navy (1700)","Top_100_data_Navy.csv")

# ARMY DATA
combine_data_from_xlsx("./Top100data","Top_100_Contractors_Report_Fiscal_Year.*\\.xlsx",
                       "Army (2100)","Top_100_data_Army.csv")

# AIR FORCE DATA
combine_data_from_xlsx("./Top100data","Top_100_Contractors_Report_Fiscal_Year.*\\.xlsx",
                       "Air Force (5700)","Top_100_data_AirForce.csv")

######################################################################################
# Make the mapping table
######################################################################################
# Read saved csv files
navydata <- read.csv("Top_100_data_Navy.csv")
armydata <- read.csv("Top_100_data_Army.csv")
airforcedata <- read.csv("Top_100_data_AirForce.csv")

# combine into one file
fulldata <- bind_rows(navydata,armydata,airforcedata)
govvendernames <- unique(fulldata[2])
write_csv(govvendernames, "FPDS_gov_vendor_names.csv")

# Get the StandardizedVendorName in Access
library(readxl)
govsdvendor <- read_excel("FPDS_gov_vendor_names.xlsx",sheet="FPDS_gov_vendor_names")

# Read vender name table in csis database
library(dplyr)
VendorName <- read.csv("Vendor_VendorName.csv")
VendorNametb <- VendorName[!duplicated(VendorName[2:3]),2:3]
# map through StandardizedVendorName
mappingtable <- left_join(govsdvendor,VendorNametb, by = c("StandardizedVendorName"="standardizedvendorname"))
# save result to csv
library(readr)
write_csv(mappingtable, "mappingtable.csv")
