#################################################################
# Running SQL server in R
# Save the result into a zip file
#################################################################

# Reference: https://www.mssqltips.com/sqlservertip/4880/sql-server-data-access-using-r--part-1/

library("RODBC") #package commonly used for SQL Server connections
library("dplyr")
library(readr)

# build a new connection
dbhandle <- odbcDriverConnect('driver={SQL Server};server=VMDatabase;database=DIIG;trusted_connection=true')

data <- sqlQuery(dbhandle, 'select * from [Contractor].[DunsnumberToParentContractorHistory]')

# Function to zip the data
zipped.csv <- function(
  # Args:
  df,           # dataframe to save
  csvfilename,  # csv file name
  zipfilename   # zip file name
  # Returns:
  # if the zip file exists, it will append the csv file into the zip file,
  # if not, it will create a new zip file
  ) {
  library(zip)
  # write temp csv
  write.csv(df,csvfilename)
  # zip temp csv
  if (file.exists(zippedfile)){
    zip_append(zippedfile,csvfilename)
  } else {
    zip(zipfilename,csvfilename)
  }
  # delete temp csv
  unlink(csvfilename)
  
  ##########################################################
  # solution for the zip function error: status 127
  # https://stackoverflow.com/questions/29129681/create-zip-file-error-running-command-had-status-127/29480538#29480538
  
} # end of zip function

zipped.csv(data,"20171024.csv","DunsnumberToParentContractorHistory.zip")

zipped.csv(data,"20171025.csv","DunsnumberToParentContractorHistory.zip")
