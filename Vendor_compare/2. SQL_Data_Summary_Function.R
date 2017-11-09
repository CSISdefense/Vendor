#################################################################
# Function to get summary table from csis database
# Source: SQL server: Vendor_SP_EntityIDHistoryAgency
#################################################################

# function to get data by given ContractingOfficeAgencyID
sql_data_processing <- function(
  # Returns data frame in the appropriate format
  # Args:
  startyear,  
  endyear,    
  COAid      # ContractingOfficeAgencyID
  #
  # Returns:
  #   a data frame of ParentID,SumofAmount, SumofAction, Freq, Year
) {
  library(dplyr)
  library(readr)
  # Read the data
  rawdata <- read.csv("Top100/Vendor_SP_EntityIDHistoryAgency.csv")
  names(rawdata)[1] <- "fiscal_year"
  
  # extract the data from specific year range
  sqldata <- filter(rawdata, fiscal_year >= startyear & fiscal_year <= endyear)
  
  # Drop observations with NULL parentID and select the data for selected
  sqldata <- filter(sqldata, parentid != "NULL" & contractingofficeagencyid == COAid)
  
  # Drop observations with NULL numberofActions and Amount
  sqldata <- filter(sqldata, SumObligatedAmount != "NULL" & SumNumberOfActions != "NULL")
  
  # Asign the column class
  sqldata$SumObligatedAmount <- as.numeric(as.character(sqldata$SumObligatedAmount))
  sqldata$SumNumberOfActions <- as.numeric(as.character(sqldata$SumNumberOfActions))
  
  # Get the summary of data
  sqldata <- sqldata %>%
    group_by(fiscal_year,parentid) %>%
    summarize(ObligatedAmount = sum(SumObligatedAmount),
              NumberOfActions = sum(SumNumberOfActions),
              FreqCount = sum(FreqCount))
  
  # Return the data frame
  return(sqldata)
}


# Get Army data
Armydata <- sql_data_processing(2006,2016,2100)

# Get Air Force Data
Airforcedata <- sql_data_processing(2006,2016,5700)

# Get Navy data
Navydata <- sql_data_processing(2006,2016,1700)

# save data
write_csv(Armydata, "army_sql_data.csv")
write_csv(Airforcedata, "airforce_sql_data.csv")
write_csv(Navydata, "navy_sql_data.csv")
