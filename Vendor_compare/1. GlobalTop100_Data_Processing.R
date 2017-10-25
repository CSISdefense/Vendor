#################################################################
# Data Processing for the data in FPDS website Top 100
#################################################################

library(tidyverse)
library(dplyr)
library(readr)
library(readxl)


# read xls files
data2016 <- read_excel("Top100/Top_100_Contractors_Report_Fiscal_Year_2016.xls", sheet = "DoD (9700)")
data2015 <- read_excel("Top100/Top_100_Contractors_Report_Fiscal_Year_2015.xls", sheet = "DoD (9700)")
data2014 <- read_excel("Top100/Top_100_Contractors_Report_Fiscal_Year_2014_v3.xls", sheet = "DoD (9700)")
data2013 <- read_excel("Top100/Top_100_Contractors_Report_Fiscal_Year_2013.xls", sheet = "DoD (9700)")
data2012 <- read_excel("Top100/Top_100_Contractors_Report_Fiscal_Year_2012.xls", sheet = "DoD (9700)")
data2011 <- read_excel("Top100/Top_100_Contractors_Report_Fiscal_Year_2011.xls", sheet = "DOD (9700)")
data2010 <- read_excel("Top100/Top_100_Contractors_Report_Fiscal_Year_2010.xls", sheet = "DoD (9700)")
data2009 <- read_excel("Top100/Top_100_Contractors_Report_Fiscal_Year_2009.xls", sheet = "DoD (9700)")
data2008 <- read_excel("Top100/Top_100_Contractors_Report_Fiscal_Year_2008.xls", sheet = "DOD (9700)")
data2007 <- read_excel("Top100/Top_100_Contractors_Report_Fiscal_Year_2007.xls", sheet = "DOD (9700)")
data2006 <- read_excel("Top100/Top_100_Contractors_Report_Fiscal_Year_2006.xls", sheet = "DOD(9700)")

# add year
data2016$Year <- "2016"
data2015$Year <- "2015"
data2014$Year <- "2014"
data2013$Year <- "2013"
data2012$Year <- "2012"
data2011$Year <- "2011"
data2010$Year <- "2010"
data2009$Year <- "2009"
data2008$Year <- "2008"
data2007$Year <- "2007"
data2006$Year <- "2006"

# rename column names
names(data2012) <- names(data2016)
names(data2011) <- names(data2016)
names(data2010) <- names(data2016)
names(data2009) <- names(data2016)
names(data2008) <- names(data2016)
names(data2007) <- names(data2016)
names(data2006) <- names(data2016)

# combine all tables
fulldata <- bind_rows(data2016,data2015,data2014,data2013,data2012,data2011,data2010,
                      data2009,data2008,data2007,data2006)

# remove the yearly sum rows and reorder columns
fulldata <- fulldata[!(is.na(fulldata$`Global Vendor Name`) | fulldata$`Global Vendor Name`=="TOTAL"), ]
fulldata <- fulldata[, c(6,1,2,3,4,5)]

# save data
write_csv(fulldata, "Top_100_data_Global.csv")


#####################################################################
# creating the lookup table

fulldata <- read.csv("Top_100_data.csv")

# read VendorName file
VendorName <- read.csv("Top100/Vendor_VendorName.csv")
names(VendorName)[1] <- "vendorname"

# select unique vendor name in Top 100
globalname <- unique(fulldata[2])

# join wo tables by vendor name
rm(maptable)
maptable <- left_join(globalname,VendorName[1:3], by = c("Global Vendor Name" = "vendorname"))

write_csv(maptable, "maptable.csv")

# read standardized vendor names
sdvendornames <- read_excel("GlobalVendorNames.xlsx", sheet = "GlobalVendorNames")
sdvendornames <- sdvendornames[-2]
names(sdvendornames)[2] <- "sd vendor names"
rm(maptable2)
maptable2 <- left_join(sdvendornames,VendorName[2:3], by = c("sd vendor names"="standardizedvendorname"))
write_csv(maptable2, "maptable2.csv")

maptable3 <- distinct(maptable2)
write_csv(maptable3, "maptable3.csv")
a <- unique(maptable3[2])
