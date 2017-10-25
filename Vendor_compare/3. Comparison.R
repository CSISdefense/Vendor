#################################################################
# Data Comparision
#################################################################

library(readxl)
library(dplyr)
library(readr)

govdata <- read.csv("Top_100_data_Global.csv")
csisdata <- read.csv("Top_150_data_csis.csv")
csissqldata <- read.csv("csis_sql_data.csv")
lookuptable <- read_excel("VendorNames_Lookup_Table.xlsx", sheet = "Lookup_Table")


# Add the parentID to govdata through lookup table
govdata <- left_join(govdata,lookuptable[1:3], by = c("Global.Vendor.Name" = "Global Vendor Name"))
govdata <- govdata[-7]

# Add the csis data
combinedata <- left_join(govdata,csisdata, by = c("Year"="Year","ParentID_M"="parentid"))
combinedata <- combinedata[-5:-6]

# Compare the # of Amount and Action
combinedata$Diff_Amount <- combinedata$Dollars.Obligated - combinedata$ObligatedAmount
combinedata$Diff_Action <- combinedata$Number.of.Actions - combinedata$NumberOfActions
combinedata$Diff_Freq <- combinedata$Number.of.Actions - combinedata$FreqCount

# Add the csis data
combinedata <- left_join(combinedata,csissqldata, by = c("Year"="fiscal_year","ParentID_M"="parentid"))


# Compare the # of Amount and Action
combinedata$Diff_Amount2 <- combinedata$Dollars.Obligated - combinedata$ObligatedAmount.y
combinedata$Diff_Action2 <- combinedata$Number.of.Actions - combinedata$NumberOfActions.y
combinedata$Diff_Freq2 <- combinedata$Number.of.Actions - combinedata$FreqCount.y


# save result
write_csv(combinedata,"comparision_result_v2.csv")
