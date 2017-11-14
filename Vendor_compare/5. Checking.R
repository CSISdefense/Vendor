#################################################################
# Function to check the result of sql
#################################################################
# functions to make data processing
handle_single_year <- function(datafile) {
  library(dplyr)
  mydata <- read.csv(datafile)
  mydata <- filter(mydata, parentid != "NULL" & DepartmentID == "9700")
  mydata$ObligatedAmount <- as.numeric(as.character(mydata$ObligatedAmount))
  mydata$fiscal_year <- as.numeric(as.character(mydata$fiscal_year))
  mydatasum <- mydata %>%
    group_by(fiscal_year) %>%
    summarize(SumObligatedAmount = sum(ObligatedAmount))
  return(mydatasum)
}

handle_multi_year <- function(datafile) {
  library(dplyr)
  mydata <- read.csv(datafile)
  names(mydata)[1] <- "fiscal_year"
  mydata <- filter(mydata, fiscal_year >= 2006 & fiscal_year <= 2016)
  mydata <- filter(mydata, parentid != "NULL" & DepartmentID == "9700")
  mydata$SumObligatedAmount <- as.numeric(as.character(mydata$SumObligatedAmount))
  mydatasum <- mydata %>%
    group_by(fiscal_year) %>%
    summarize(SumObligatedAmount = sum(SumObligatedAmount))
  return(mydatasum)
}

# Function for cross check
checksum <- function(data1, data2){
  library(dplyr)
  combinedata <- full_join(data1,data2, by = "fiscal_year")
  combinedata$diff <- combinedata$SumObligatedAmount.x - combinedata$SumObligatedAmount.y
  for (i in 1:length(combinedata$diff)) {
    if(is.na(combinedata$diff[i])) {
      print(paste("Warning: FY", combinedata$fiscal_year[i], "does not have a match."))
    } else if (combinedata$diff[i] != 0){
      print(paste("ERROR: Discrepancy in FY", combinedata$fiscal_year[i]))
    }
  }

}

data1 <- handle_single_year("Top100/Vendor_EntityIDHistoryAgency2006.csv")
data2 <- handle_multi_year("Top100/Vendor_SP_EntityIDHistoryAgency.csv")
checksum(data1,data2)
