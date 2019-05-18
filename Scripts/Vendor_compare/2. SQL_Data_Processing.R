#################################################################
# Data Processing for csis data 
# Source: SQL server: Vendor_EntityIDHistoryAgency
#################################################################

library(dplyr)
library(readr)

# Read the data
data2016 <- read.csv("Top100/Vendor_EntityIDHistoryAgency2016.csv")
data2015 <- read.csv("Top100/Vendor_EntityIDHistoryAgency2015.csv")
data2014 <- read.csv("Top100/Vendor_EntityIDHistoryAgency2014.csv")
data2013 <- read.csv("Top100/Vendor_EntityIDHistoryAgency2013.csv")
data2012 <- read.csv("Top100/Vendor_EntityIDHistoryAgency2012.csv")
data2011 <- read.csv("Top100/Vendor_EntityIDHistoryAgency2011.csv")
data2010 <- read.csv("Top100/Vendor_EntityIDHistoryAgency2010.csv")
data2009 <- read.csv("Top100/Vendor_EntityIDHistoryAgency2009.csv")
data2008 <- read.csv("Top100/Vendor_EntityIDHistoryAgency2008.csv")
data2007 <- read.csv("Top100/Vendor_EntityIDHistoryAgency2007.csv")
data2006 <- read.csv("Top100/Vendor_EntityIDHistoryAgency2006.csv")

# Function for data processing
data_processing <- function(
  # Returns data frame in the appropriate format
  # Args:
  mydata, # raw data
  fy      # fiscal year of the data
  #
  # Returns:
  #   a data frame of ParentID,SumofAmount, Freq, Year
  ) {
  
  # Drop observations with NULL parentID and select the data for DoD 9700
  DoDdata <- filter(mydata, parentid != "NULL" & DepartmentID == "9700")
  
  # Asign the column class
  class(DoDdata$NumberOfActions) <- "numeric"
  class(DoDdata$ObligatedAmount) <- "numeric"
  
  # Get the summary of data
  DoDdata <- DoDdata %>%
    group_by(parentid) %>%
    summarize(ObligatedAmount = sum(ObligatedAmount),
              NumberOfActions = sum(NumberOfActions),
              FreqCount = n())
  
  # Select top 150 companies by ObligatedAmount
  Top150 <- DoDdata %>%
    filter(rank(desc(ObligatedAmount))<=150)
  
  # Add year coloumn and reorder the dataframe
  Top150$Year <- fy
  Top150$Rank <- rank(desc(Top150$ObligatedAmount))
  Top150 <- arrange(Top150, Rank)
  
  # Return the data frame
  return(Top150)
}

# Data Processing
Top150_2016 <- data_processing(data2016,"2016")
Top150_2015 <- data_processing(data2015,"2015")
Top150_2014 <- data_processing(data2014,"2014")
Top150_2013 <- data_processing(data2013,"2013")
Top150_2012 <- data_processing(data2012,"2012")
Top150_2011 <- data_processing(data2011,"2011")
Top150_2010 <- data_processing(data2010,"2010")
Top150_2009 <- data_processing(data2009,"2009")
Top150_2008 <- data_processing(data2008,"2008")
Top150_2007 <- data_processing(data2007,"2007")
Top150_2006 <- data_processing(data2006,"2006")

# combine all tables
fulldata <- bind_rows(Top150_2016,Top150_2015,Top150_2014,Top150_2013,Top150_2012,
                      Top150_2011,Top150_2010,Top150_2009,Top150_2008,Top150_2007,
                      Top150_2006)

# save data
write_csv(fulldata, "Top_150_data_csis.csv")
