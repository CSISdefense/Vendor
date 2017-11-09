#################################################################
# Data Processing for csis data 
# Source: SQL server: Vendor_SP_EntityIDHistoryAgency
#################################################################

library(dplyr)
library(readr)

# Read the data
sqldata <- read.csv("Top100/Vendor_SP_EntityIDHistoryAgency.csv")
names(sqldata)[1] <- "fiscal_year"

# extract the data from 2006 to 2016
sqldata <- filter(sqldata, fiscal_year >= 2006 & fiscal_year <= 2016)

# Drop observations with NULL parentID and select the data for DoD 9700
DoDdata <- filter(sqldata, parentid != "NULL" & DepartmentID == "9700")

# Asign the column class
DoDdata$SumObligatedAmount <- as.numeric(as.character(DoDdata$SumObligatedAmount))
class(DoDdata$SumNumberOfActions) <- "integer"

# Get the summary of data
DoDdata <- DoDdata %>%
  group_by(fiscal_year,parentid) %>%
  summarize(ObligatedAmount = sum(SumObligatedAmount),
            NumberOfActions = sum(SumNumberOfActions),
            FreqCount = sum(FreqCount))

# save data
write_csv(DoDdata, "csis_sql_data.csv")
