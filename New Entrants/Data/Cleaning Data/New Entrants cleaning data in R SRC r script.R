####Background info####
# Data to be used for studying new entrants in the market for government contracts.
# 
# Longitudinal Data of firms over time where the unique identifier is DUNS number and the time
# periods are by data_year 
# 
# 2 main sources: SAM and FPDS
# 
# 
# Main variables of interest: 
#   
# Whether a firm is a new entrant
# 
# Whether a firm is small or non-small
# 
# Whether a firm exits
# 
# Whether a firm graduates
# 
# If still survived after 3 years
# 
# If still survived after 5 years
# 
# If s till survived after 10 years
# 
# Firm age
# 
# Foreign owned
# 
# Woman owned
# 
# Veteran owned
# 
# Minority Owned
# 
# Domestic Location
# 
# Industry (2-digit NAICS)
# 
# Number of total obligations
# 
# Number of total actions
# 
# Federal Agency
# 
# Any set aside information

#******************************************************************
########################Set Up ################################
#******************************************************************

library(matrixStats)
library(describer)
library(tidyverse)
library(openxlsx)
library(httr)
library(jsonlite)
library(plyr)
library(data.table)
library(lubridate)
library(dplyr)
library(foreach)

setwd("K:/2018-01 NPS New Entrants/Data/Data/Cleaning data/Get files")

#******************************************************************
########################2000 SAM Data ################################
#******************************************************************
##Get and Save into CSV
Get_2000 <- read.xlsx("Get_2000.xlsx")

write.csv(Get_2000, file="2000_datapull_yr.csv")

##save(Get_2000, file="2000_datapull_yr.Rda")

##****************************************

##Use new CSV

datapull_yr_2000 <- read.csv("2000_datapull_yr.csv")

names(datapull_yr_2000)

##********************************##
##****Drop Vars not used*********##
##********************************##

##keep these vars

datapull_yr_2000 <- datapull_yr_2000 %>% select(activationDate, fiscalYearEndCloseDate, 
                                                registrationDate, duns, expirationDate, 
                                                status, businessStartDate, 
                                                samAddress.countryCode, 
                                                countryOfIncorporation, naics, 
                                                naics.naicsCode, naics.naicsName)

##data editor

edit(datapull_yr_2000)

##number of missing per variable

sum(is.na(datapull_yr_2000$duns))

##how to drop a variable

# datapull_yr_2000$x <- NULL //if we want to delete the variable x



##********************************##
##****de-string if necessary*********##
##********************************##

##provide the structure of the data set

str(datapull_yr_2000)



##********************************##
##****Clean DUNS numbers*********##
##********************************##

##check uniqueness of DUNS as a variable##

n_distinct(datapull_yr_2000$duns) ##duns is a unique identifier! 

length(unique(datapull_yr_2000$duns)) == nrow(datapull_yr_2000) ##TRUE, confirmed duns as a unique identifier

##check uniqueness of countryofIncorporation as a variable##
n_distinct(datapull_yr_2000$countryOfIncorporation)

table(datapull_yr_2000$countryOfIncorporation)


##********************************##
##****create year variable*********##
##********************************##

datapull_yr_2000$data_year <- 2000


##********************************##
##****clean Entry variable: registrationDate*********##
##********************************##


##change registrationdate to the format as.Date
datapull_yr_2000$registrationDate<-as.Date(as.character(datapull_yr_2000$registrationDate))



##********************************##
##****clean expirationDate*********##
##********************************##

##change expirationDate to the format as.Date
datapull_yr_2000$expirationDate<-as.Date(as.character(datapull_yr_2000$expirationDate))


##********************************##
##****clean businessstartDate*********##
##********************************##

##change businessstartDate to the format as.Date
datapull_yr_2000$businessStartDate<-as.Date(as.character(datapull_yr_2000$businessStartDate))


##********************************##
#************creating var describing registration yr and biz start date yr******#
#*******************************##

#************************
##create registration year
datapull_yr_2000 <- datapull_yr_2000 %>%
  dplyr::mutate(registrationYear = (format(datapull_yr_2000$registrationDate, "%Y")))

str(datapull_yr_2000$registrationYear) ##it's a character

#make registrationYear numeric
datapull_yr_2000$registrationYear<-as.numeric(as.character(datapull_yr_2000$registrationYear))

str(datapull_yr_2000$registrationYear)
#***********************
###

#********************************
##create business start date year
datapull_yr_2000 <- datapull_yr_2000 %>%
  dplyr::mutate(businessStartYear = (format(datapull_yr_2000$businessStartDate, "%Y")))

str(datapull_yr_2000$businessStartYear)

#make businessstartYear numeric

datapull_yr_2000$businessStartYear<-as.numeric(as.character(datapull_yr_2000$businessStartYear))
str(datapull_yr_2000$businessStartYear)
#********************************
#

##count the number of observations with registrationYear in 1999, 2000, 2000

tab <- table(datapull_yr_2000$registrationYear)

tab

nrow(datapull_yr_2000[datapull_yr_2000$registrationYear==1997, ]) ##3 in year 1997

nrow(datapull_yr_2000[datapull_yr_2000$registrationYear==1998, ]) ##0 in year 1998

nrow(datapull_yr_2000[datapull_yr_2000$registrationYear==1999, ]) ##31 in year 1999

nrow(datapull_yr_2000[datapull_yr_2000$registrationYear==2000, ]) ##1800 in year 1999

###



##********************************##
##******create firm age*******##
#firm age = registration date - business start date
##********************************##

##drop variables that did not enter in 2000 by registrationDate //DON'T NEED TO DO THIS NOW
#datapull_yr_2000 <- datapull_yr_2000[(format(datapull_yr_2000$registrationDate,"%Y")=="2000"), ]


##subtract the two years for the age
datapull_yr_2000 <- datapull_yr_2000 %>% 
  dplyr::mutate(starting_age = (datapull_yr_2000$registrationYear) - (datapull_yr_2000$businessStartYear)) 

##********************************##
##****Save File*********##
##********************************##

#write.csv(datapull_yr_2000, file="2001_datapull_cleaned.csv")

save(datapull_yr_2000, file="2000_datapull_cleaned.Rda")


#*********************************************************************



#******************************************************************
########################2001 SAM Data ################################
#******************************************************************

##Get and Save into CSV
Get_2001 <- read.xlsx("Get_2001.xlsx")

write.csv(Get_2001, file="2001_datapull_yr.csv")

##save(Get_2001, file="2001_datapull_yr.Rda")

##****************************************

##Use new CSV

datapull_yr_2001 <- read.csv("2001_datapull_yr.csv")

names(datapull_yr_2001)

##********************************##
##****Drop Vars not used*********##
##********************************##

##keep these vars

datapull_yr_2001 <- datapull_yr_2001 %>% select(activationDate, fiscalYearEndCloseDate, registrationDate, duns, expirationDate, status, businessStartDate, samAddress.countryCode, countryOfIncorporation, naics, naics.naicsCode, naics.naicsName)

##data editor

edit(datapull_yr_2001)

##number of missing per variable

sum(is.na(datapull_yr_2001$duns))

##how to drop a variable

# datapull_yr_2001$x <- NULL //if we want to delete the variable x



##********************************##
##****de-string if necessary*********##
##********************************##

##provide the structure of the data set

str(datapull_yr_2001)



##********************************##
##****Clean DUNS numbers*********##
##********************************##

##check uniqueness of DUNS as a variable##

n_distinct(datapull_yr_2001$duns) ##duns is a unique identifier! 


##check uniqueness of countryofIncorporation as a variable##
n_distinct(datapull_yr_2001$countryOfIncorporation)

table(datapull_yr_2001$countryOfIncorporation)


##********************************##
##****create year variable*********##
##********************************##

datapull_yr_2001$data_year <- 2001


##********************************##
##****clean Entry variable: registrationDate*********##
##********************************##


##change registrationdate to the format as.Date
datapull_yr_2001$registrationDate<-as.Date(as.character(datapull_yr_2001$registrationDate))



##********************************##
##****clean expirationDate*********##
##********************************##

##change expirationDate to the format as.Date
datapull_yr_2001$expirationDate<-as.Date(as.character(datapull_yr_2001$expirationDate))


##********************************##
##****clean businessstartDate*********##
##********************************##

##change businessstartDate to the format as.Date
datapull_yr_2001$businessStartDate<-as.Date(as.character(datapull_yr_2001$businessStartDate))


##********************************##
#************creating var describing registration yr and biz start date yr******#
#*******************************##

#************************
##create registration year
datapull_yr_2001 <- datapull_yr_2001 %>%
  dplyr::mutate(registrationYear = (format(datapull_yr_2001$registrationDate, "%Y")))

str(datapull_yr_2001$registrationYear) ##it's a character

#make registrationYear numeric
datapull_yr_2001$registrationYear<-as.numeric(as.character(datapull_yr_2001$registrationYear))

str(datapull_yr_2001$registrationYear)
#***********************
###

#********************************
##create business start date year
datapull_yr_2001 <- datapull_yr_2001 %>%
  dplyr::mutate(businessStartYear = (format(datapull_yr_2001$businessStartDate, "%Y")))

str(datapull_yr_2001$businessStartYear)

#make businessstartYear numeric

datapull_yr_2001$businessStartYear<-as.numeric(as.character(datapull_yr_2001$businessStartYear))
str(datapull_yr_2001$businessStartYear)
#********************************
#

##count the number of observations with registrationYear in 1999, 2000, 2001

tab <- table(datapull_yr_2001$registrationYear)

tab

nrow(datapull_yr_2001[datapull_yr_2001$registrationYear==1997, ]) ##3 in year 1997

nrow(datapull_yr_2001[datapull_yr_2001$registrationYear==1998, ]) ##0 in year 1998

nrow(datapull_yr_2001[datapull_yr_2001$registrationYear==1999, ]) ##31 in year 1999

nrow(datapull_yr_2001[datapull_yr_2001$registrationYear==2000, ]) ##1800 in year 1999

###



##********************************##
##******create firm age*******##
#firm age = registration date - business start date
##********************************##

##drop variables that did not enter in 2001 by registrationDate //DON'T NEED TO DO THIS NOW
#datapull_yr_2001 <- datapull_yr_2001[(format(datapull_yr_2001$registrationDate,"%Y")=="2000"), ]


##subtract the two years for the age
datapull_yr_2001 <- datapull_yr_2001 %>% 
  dplyr::mutate(starting_age = (datapull_yr_2001$registrationYear) - (datapull_yr_2001$businessStartYear)) 

##********************************##
##****Save File*********##
##********************************##

#write.csv(datapull_yr_2001, file="2001_datapull_cleaned.csv")

save(datapull_yr_2001, file="2001_datapull_cleaned.Rda")



#*********************************************************************

#******************************************************************
########################2002-03 SAM Data ################################
#******************************************************************
##Get and Save into CSV
Get_2002_03 <- read.xlsx("Get_2002_03.xlsx")

write.csv(Get_2002_03, file="2002_03_datapull_yr.csv")

##save(Get_2001, file="2001_datapull_yr.Rda")

##****************************************

##Use new CSV

datapull_yr_2002_03 <- read.csv("2002_03_datapull_yr.csv")

names(datapull_yr_2002_03)

##********************************##
##****Drop Vars not used*********##
##********************************##

##keep these vars

datapull_yr_2002_03 <- datapull_yr_2002_03 %>% select(activationDate, fiscalYearEndCloseDate, registrationDate, duns, expirationDate, status, businessStartDate, samAddress.countryCode, countryOfIncorporation, naics, naics.naicsCode, naics.naicsName)

##data editor

edit(datapull_yr_2002_03)

##number of missing per variable

sum(is.na(datapull_yr_2002_03$duns)) ## 0 NAs in duns

##how to drop a variable

# datapull_yr_2001$x <- NULL //if we want to delete the variable x



##********************************##
##****de-string if necessary*********##
##********************************##

##provide the structure of the data set

str(datapull_yr_2002_03)



##********************************##
##****Clean DUNS numbers*********##
##********************************##

##check uniqueness of DUNS as a variable##

n_distinct(datapull_yr_2002_03$duns) ##duns is a unique identifier! 


##check uniqueness of countryofIncorporation as a variable##
n_distinct(datapull_yr_2002_03$countryOfIncorporation) ##19 distinct values

table(datapull_yr_2002_03$countryOfIncorporation)


##********************************##
##****create year variable*********##
##********************************##

datapull_yr_2002_03$data_year <- 2002


##********************************##
##****clean Entry variable: registrationDate*********##
##********************************##


##change registrationdate to the format as.Date
datapull_yr_2002_03$registrationDate<-as.Date(as.character(datapull_yr_2002_03$registrationDate))



##********************************##
##****clean expirationDate*********##
##********************************##

##change expirationDate to the format as.Date
datapull_yr_2002_03$expirationDate<-as.Date(as.character(datapull_yr_2002_03$expirationDate))


##********************************##
##****clean businessstartDate*********##
##********************************##

##change businessstartDate to the format as.Date
datapull_yr_2002_03$businessStartDate<-as.Date(as.character(datapull_yr_2002_03$businessStartDate))


##********************************##
#************creating var describing registration yr and biz start date yr******#
#*******************************##

#************************
##create registration year
datapull_yr_2002_03 <- datapull_yr_2002_03 %>%
  dplyr::mutate(registrationYear = (format(datapull_yr_2002_03$registrationDate, "%Y")))

str(datapull_yr_2002_03$registrationYear) ##it's a character

#make registrationYear numeric
datapull_yr_2002_03$registrationYear<-as.numeric(as.character(datapull_yr_2002_03$registrationYear))

str(datapull_yr_2002_03$registrationYear)
#***********************
###

#********************************
##create business start date year
datapull_yr_2002_03<- datapull_yr_2002_03 %>%
  dplyr::mutate(businessStartYear = (format(datapull_yr_2002_03$businessStartDate, "%Y")))

str(datapull_yr_2002_03$businessStartYear)

#make businessstartYear numeric

datapull_yr_2002_03$businessStartYear<-as.numeric(as.character(datapull_yr_2002_03$businessStartYear))
str(datapull_yr_2002_03$businessStartYear)
#********************************
#

##count the number of observations with registrationYear in each year

tab2002_03 <- table(datapull_yr_2002_03$registrationYear)

tab2002_03

nrow(datapull_yr_2001[datapull_yr_2001$registrationYear==1998, ]) ##4

nrow(datapull_yr_2001[datapull_yr_2001$registrationYear==1999, ]) ##13

nrow(datapull_yr_2001[datapull_yr_2001$registrationYear==2000, ]) ##40

nrow(datapull_yr_2001[datapull_yr_2001$registrationYear==2001, ]) ##2408

nrow(datapull_yr_2001[datapull_yr_2001$registrationYear==2002, ]) ##1317


###



##********************************##
##******create firm age*******##
#firm age = registration date - business start date
##********************************##

##drop variables that did not enter in 2001 by registrationDate //DON'T NEED THIS ANYMORE
#datapull_yr_2001 <- datapull_yr_2001[(format(datapull_yr_2001$registrationDate,"%Y")=="2000"), ]


##subtract the two years for the age
datapull_yr_2002_03 <- datapull_yr_2002_03 %>% 
  dplyr::mutate(starting_age = (datapull_yr_2002_03$registrationYear) - (datapull_yr_2002_03$businessStartYear)) 

##********************************##
##****Save File*********##
##********************************##

#write.csv(datapull_yr_2002_03, file="2002_03_datapull_cleaned.csv")

save(datapull_yr_2002_03, file="2002_03_datapull_cleaned.Rda")

#*********************************************************************


#******************************************************************
########################2004-05 SAM Data ################################
#******************************************************************

##Get and Save into CSV
Get_2004_05 <- read.xlsx("Get_2004_05.xlsx")

write.csv(Get_2004_05, file="2004_05_datapull_yr.csv")

##save(Get_2001, file="2001_datapull_yr.Rda")

##****************************************

##Use new CSV

datapull_yr_2004_05 <- read.csv("2004_05_datapull_yr.csv")

names(datapull_yr_2004_05)

##********************************##
##****Drop Vars not used*********##
##********************************##

##keep these vars

datapull_yr_2004_05 <- datapull_yr_2004_05 %>% select(activationDate, fiscalYearEndCloseDate, registrationDate, duns, expirationDate, status, businessStartDate, samAddress.countryCode, countryOfIncorporation, naics, naics.naicsCode, naics.naicsName)

##data editor

edit(datapull_yr_2004_05)

##number of missing per variable

sum(is.na(datapull_yr_2004_05$duns)) ##0 missing values for DUNS number

##how to drop a variable

# datapull_yr_2001$x <- NULL //if we want to delete the variable x



##********************************##
##****de-string if necessary*********##
##********************************##

##provide the structure of the data set

str(datapull_yr_2004_05)



##********************************##
##****Clean DUNS numbers*********##
##********************************##

##check uniqueness of DUNS as a variable##

n_distinct(datapull_yr_2004_05$duns) ##duns is NOT a unique identifier! there are 
#3873 unique cases of duns but 2875 observations,  which means one is duplicated 3 times
#or two are duplicated

##another way to check duplicates

length(unique(datapull_yr_2004_05$duns)) == nrow(datapull_yr_2004_05) ##should return TRUE if there are no duplicates
#^^ returns FALSE, so there are indeed duplicates

###FIND DUPLICATES

n_occur <- data.frame(table(datapull_yr_2004_05$duns)) ##gives a data frame with a list of duns and the number of times they occurred

n_occur[n_occur$Freq >1, ] ##tells me which duns occur more than once and their frequency
##DUNS 180515785 occurs 2 times
##DUNS 849622840 occurs 2 times

##subset to evaluate duplicates 

duplicates1 <- data.frame(datapull_yr_2004_05[c(datapull_yr_2004_05$duns==180515785), ])

duplicates2 <- data.frame(datapull_yr_2004_05[c(datapull_yr_2004_05$duns==849622840), ])
##^^they are definitely duplicates so removing one of each of the two duplicates

##remove duplicates

datapull_yr_2004_05 <- datapull_yr_2004_05[!duplicated(datapull_yr_2004_05), ]

##check duplicates

length(unique(datapull_yr_2004_05$duns)) == nrow(datapull_yr_2004_05) ##should return TRUE if there are no duplicates
#^^duplicates removed!!


##check uniqueness of countryofIncorporation as a variable##
n_distinct(datapull_yr_2004_05$countryOfIncorporation)

table(datapull_yr_2004_05$countryOfIncorporation)


##********************************##
##****create year variable*********##
##********************************##

datapull_yr_2004_05$data_year <- 2004


##********************************##
##****clean Entry variable: registrationDate*********##
##********************************##


##change registrationdate to the format as.Date
datapull_yr_2004_05$registrationDate<-as.Date(as.character(datapull_yr_2004_05$registrationDate))



##********************************##
##****clean expirationDate*********##
##********************************##

##change expirationDate to the format as.Date
datapull_yr_2004_05$expirationDate<-as.Date(as.character(datapull_yr_2004_05$expirationDate))


##********************************##
##****clean businessstartDate*********##
##********************************##

##change businessstartDate to the format as.Date
datapull_yr_2004_05$businessStartDate<-as.Date(as.character(datapull_yr_2004_05$businessStartDate))


##********************************##
#************creating var describing registration yr and biz start date yr******#
#*******************************##

#************************
##create registration year
datapull_yr_2004_05 <- datapull_yr_2004_05 %>%
  dplyr::mutate(registrationYear = (format(datapull_yr_2004_05$registrationDate, "%Y")))

str(datapull_yr_2004_05$registrationYear) ##it's a character

#make registrationYear numeric
datapull_yr_2004_05$registrationYear<-as.numeric(as.character(datapull_yr_2004_05$registrationYear))

str(datapull_yr_2004_05$registrationYear)
#***********************
###

#********************************
##create business start date year
datapull_yr_2004_05 <- datapull_yr_2004_05 %>%
  dplyr::mutate(businessStartYear = (format(datapull_yr_2004_05$businessStartDate, "%Y")))

str(datapull_yr_2004_05$businessStartYear)

#make businessstartYear numeric

datapull_yr_2004_05$businessStartYear<-as.numeric(as.character(datapull_yr_2004_05$businessStartYear))
str(datapull_yr_2004_05$businessStartYear)
#********************************
#

##count the number of observations with registrationYear in other yeras

tab <- table(datapull_yr_2004_05$registrationYear)

tab

nrow(datapull_yr_2004_05[datapull_yr_2004_05$registrationYear==1997, ]) ##5 

nrow(datapull_yr_2004_05[datapull_yr_2004_05$registrationYear==1998, ]) ##19 

nrow(datapull_yr_2004_05[datapull_yr_2004_05$registrationYear==1999, ]) ##35 

nrow(datapull_yr_2004_05[datapull_yr_2004_05$registrationYear==2000, ]) ##54

nrow(datapull_yr_2004_05[datapull_yr_2004_05$registrationYear==2001, ]) ##688

nrow(datapull_yr_2004_05[datapull_yr_2004_05$registrationYear==2002, ]) ##879

nrow(datapull_yr_2004_05[datapull_yr_2004_05$registrationYear==2003, ]) ##1249

nrow(datapull_yr_2004_05[datapull_yr_2004_05$registrationYear==2004, ]) ##940

nrow(datapull_yr_2004_05[datapull_yr_2004_05$registrationYear==2005, ]) ##5



###



##********************************##
##******create firm age*******##
#firm age = registration date - business start date
##********************************##

##drop variables that did not enter in 2001 by registrationDate //DON'T NEED TO DO THIS NOW
#datapull_yr_2001 <- datapull_yr_2001[(format(datapull_yr_2001$registrationDate,"%Y")=="2000"), ]


##subtract the two years for the age
datapull_yr_2004_05 <- datapull_yr_2004_05 %>% 
  dplyr::mutate(starting_age = (datapull_yr_2004_05$registrationYear) - (datapull_yr_2004_05$businessStartYear)) 

##********************************##
##****Save File*********##
##********************************##

#write.csv(datapull_yr_2004_05, file="2004_05_datapull_cleaned.csv")

save(datapull_yr_2004_05, file="2004_05_datapull_cleaned.Rda")

##*************************************************************************

#******************************************************************
########################2006-2007 SAM Data ################################
#******************************************************************

##Get and Save into CSV
Get_2006_07 <- read.xlsx("Get_2006_07.xlsx")

write.csv(Get_2006_07, file="2006_07_datapull_yr.csv")

##save(Get_2001, file="2001_datapull_yr.Rda")

##****************************************

##Use new CSV

datapull_yr_2006_07 <- read.csv("2006_07_datapull_yr.csv")

names(datapull_yr_2006_07)

##********************************##
##****Drop Vars not used*********##
##********************************##

##keep these vars

datapull_yr_2006_07 <- datapull_yr_2006_07 %>% select(activationDate, fiscalYearEndCloseDate, registrationDate, duns, expirationDate, status, businessStartDate, samAddress.countryCode, countryOfIncorporation, naics, naics.naicsCode, naics.naicsName)

##data editor

edit(datapull_yr_2006_07)

##number of missing per variable

sum(is.na(datapull_yr_2006_07$duns)) ##0 missing values for DUNS number

##how to drop a variable

# datapull_yr_2001$x <- NULL //if we want to delete the variable x



##********************************##
##****de-string if necessary*********##
##********************************##

##provide the structure of the data set

str(datapull_yr_2006_07)



##********************************##
##****Clean DUNS numbers*********##
##********************************##

##check uniqueness of DUNS as a variable##

n_distinct(datapull_yr_2006_07$duns) ##duns is NOT a unique identifier! there are 
#3948 unique cases of duns but 3954 observations, which indicates that there are duplicates

##another way to check duplicates

length(unique(datapull_yr_2006_07$duns)) == nrow(datapull_yr_2006_07) ##should return TRUE if there are no duplicates
#^^ returns FALSE, so there are indeed duplicates

###FIND DUPLICATES

n_occur <- data.frame(table(datapull_yr_2006_07$duns)) ##gives a data frame with a list of duns and the number of times they occurred

n_occur[n_occur$Freq >1, ] ##tells me which duns occur more than once and their frequency
# 1765 158423843    2
# 3306 835456179    2
# 3361 844048558    2
# 3388 849492384    2
# 3646 940612948    3

##subset to evaluate duplicates 

duplicates1 <- data.frame(datapull_yr_2006_07[c(datapull_yr_2006_07$duns==158423843), ])

duplicates2 <- data.frame(datapull_yr_2006_07[c(datapull_yr_2006_07$duns==835456179), ])

duplicates3 <- data.frame(datapull_yr_2006_07[c(datapull_yr_2006_07$duns==844048558), ])

duplicates4 <- data.frame(datapull_yr_2006_07[c(datapull_yr_2006_07$duns==849492384), ])

duplicates5 <- data.frame(datapull_yr_2006_07[c(datapull_yr_2006_07$duns==940612948), ])
##^^they are definitely duplicates so removing one of each of the two duplicates

##remove duplicates

datapull_yr_2006_07 <- datapull_yr_2006_07[!duplicated(datapull_yr_2006_07), ]

##check duplicates

length(unique(datapull_yr_2006_07$duns)) == nrow(datapull_yr_2006_07) ##should return TRUE if there are no duplicates
#^^duplicates removed!!


##check uniqueness of countryofIncorporation as a variable##
n_distinct(datapull_yr_2006_07$countryOfIncorporation)

table(datapull_yr_2006_07$countryOfIncorporation)


##********************************##
##****create year variable*********##
##********************************##

datapull_yr_2006_07$data_year <- 2006


##********************************##
##****clean Entry variable: registrationDate*********##
##********************************##


##change registrationdate to the format as.Date
datapull_yr_2006_07$registrationDate<-as.Date(as.character(datapull_yr_2006_07$registrationDate))



##********************************##
##****clean expirationDate*********##
##********************************##

##change expirationDate to the format as.Date
datapull_yr_2006_07$expirationDate<-as.Date(as.character(datapull_yr_2006_07$expirationDate))


##********************************##
##****clean businessstartDate*********##
##********************************##

##change businessstartDate to the format as.Date
datapull_yr_2006_07$businessStartDate<-as.Date(as.character(datapull_yr_2006_07$businessStartDate))


##********************************##
#************creating var describing registration yr and biz start date yr******#
#*******************************##

#************************
##create registration year
datapull_yr_2006_07 <- datapull_yr_2006_07 %>%
  dplyr::mutate(registrationYear = (format(datapull_yr_2006_07$registrationDate, "%Y")))

str(datapull_yr_2006_07$registrationYear) ##it's a character

#make registrationYear numeric
datapull_yr_2006_07$registrationYear<-as.numeric(as.character(datapull_yr_2006_07$registrationYear))

str(datapull_yr_2006_07$registrationYear)
#***********************
###

#********************************
##create business start date year
datapull_yr_2006_07 <- datapull_yr_2006_07 %>%
  dplyr::mutate(businessStartYear = (format(datapull_yr_2006_07$businessStartDate, "%Y")))

str(datapull_yr_2006_07$businessStartYear)

#make businessstartYear numeric

datapull_yr_2006_07$businessStartYear<-as.numeric(as.character(datapull_yr_2006_07$businessStartYear))
str(datapull_yr_2006_07$businessStartYear)
#********************************
#

##count the number of observations with registrationYear in other yeras

tab <- table(datapull_yr_2006_07$registrationYear)

tab

nrow(datapull_yr_2006_07[datapull_yr_2006_07$registrationYear==1997, ]) ##5 

nrow(datapull_yr_2006_07[datapull_yr_2006_07$registrationYear==1998, ]) ##19 

nrow(datapull_yr_2006_07[datapull_yr_2006_07$registrationYear==1999, ]) ##25

nrow(datapull_yr_2006_07[datapull_yr_2006_07$registrationYear==2000, ]) ##57

nrow(datapull_yr_2006_07[datapull_yr_2006_07$registrationYear==2001, ]) ##507

nrow(datapull_yr_2006_07[datapull_yr_2006_07$registrationYear==2002, ]) ##546

nrow(datapull_yr_2006_07[datapull_yr_2006_07$registrationYear==2003, ]) ##365

nrow(datapull_yr_2006_07[datapull_yr_2006_07$registrationYear==2004, ]) ##655

nrow(datapull_yr_2006_07[datapull_yr_2006_07$registrationYear==2005, ]) ##1032

nrow(datapull_yr_2006_07[datapull_yr_2006_07$registrationYear==2006, ]) ##735

nrow(datapull_yr_2006_07[datapull_yr_2006_07$registrationYear==2007, ]) ##2



###



##********************************##
##******create firm age*******##
#firm age = registration date - business start date
##********************************##

##drop variables that did not enter in 2001 by registrationDate //DON'T NEED TO DO THIS NOW
#datapull_yr_2001 <- datapull_yr_2001[(format(datapull_yr_2001$registrationDate,"%Y")=="2000"), ]


##subtract the two years for the age
datapull_yr_2006_07 <- datapull_yr_2006_07 %>% 
  dplyr::mutate(starting_age = (datapull_yr_2006_07$registrationYear) - (datapull_yr_2006_07$businessStartYear)) 

##********************************##
##****Save File*********##
##********************************##

#write.csv(datapull_yr_2006_07, file="2006_07_datapull_cleaned.csv")

save(datapull_yr_2006_07, file="2006_07_datapull_cleaned.Rda")

#*******************************************************************************

#******************************************************************
########################2008-2009 SAM Data ################################
#******************************************************************


##Get and Save into CSV
Get_2008_09 <- read.xlsx("Get_2008_09.xlsx")

write.csv(Get_2008_09, file="2008_09_datapull_yr.csv")

##save(Get_2001, file="2001_datapull_yr.Rda")

##****************************************

##Use new CSV

datapull_yr_2008_09 <- read.csv("2008_09_datapull_yr.csv")

names(datapull_yr_2008_09)

##********************************##
##****Drop Vars not used*********##
##********************************##

##keep these vars

datapull_yr_2008_09 <- datapull_yr_2008_09 %>% select(activationDate, fiscalYearEndCloseDate, registrationDate, duns, expirationDate, status, businessStartDate, samAddress.countryCode, countryOfIncorporation, naics, naics.naicsCode, naics.naicsName)

##data editor

edit(datapull_yr_2008_09)

##number of missing per variable

sum(is.na(datapull_yr_2008_09$duns)) ##0 missing values for DUNS number

##how to drop a variable

# datapull_yr_2001$x <- NULL //if we want to delete the variable x



##********************************##
##****de-string if necessary*********##
##********************************##

##provide the structure of the data set

str(datapull_yr_2008_09)



##********************************##
##****Clean DUNS numbers*********##
##********************************##

##check uniqueness of DUNS as a variable##

n_distinct(datapull_yr_2008_09$duns) ##duns is NOT a unique identifier! there are 
#2864 unique cases of duns but 2872 observations, which indicates that there are duplicates

##another way to check duplicates

length(unique(datapull_yr_2008_09$duns)) == nrow(datapull_yr_2008_09) ##should return TRUE if there are no duplicates
#^^ returns FALSE, so there are indeed duplicates

###FIND DUPLICATES

n_occur <- data.frame(table(datapull_yr_2008_09$duns)) ##gives a data frame with a list of duns and the number of times they occurred

n_occur[n_occur$Freq >1, ] ##tells me which duns occur more than once and their frequency
# 155   10982713    2
# 789  107978582    2
# 911  126473271    2
# 1021 140042636    2
# 1910 623391323    2
# 2392 808862957    2
# 2508 843766747    2
# 2793 962441044    2

##subset to evaluate duplicates 

duplicates1 <- data.frame(datapull_yr_2008_09[c(datapull_yr_2008_09$duns==10982713), ])

duplicates2 <- data.frame(datapull_yr_2008_09[c(datapull_yr_2008_09$duns==107978582), ])

duplicates3 <- data.frame(datapull_yr_2008_09[c(datapull_yr_2008_09$duns==126473271), ])

duplicates4 <- data.frame(datapull_yr_2008_09[c(datapull_yr_2008_09$duns==140042636), ])

duplicates5 <- data.frame(datapull_yr_2008_09[c(datapull_yr_2008_09$duns==623391323), ])

duplicates6 <- data.frame(datapull_yr_2008_09[c(datapull_yr_2008_09$duns==808862957), ])

duplicates7 <- data.frame(datapull_yr_2008_09[c(datapull_yr_2008_09$duns==843766747), ])

duplicates8 <- data.frame(datapull_yr_2008_09[c(datapull_yr_2008_09$duns==962441044), ])
##^^they are definitely duplicates so removing one of each of the two duplicates

##remove duplicates

datapull_yr_2008_09 <- datapull_yr_2008_09[!duplicated(datapull_yr_2008_09), ]

##check duplicates

length(unique(datapull_yr_2008_09$duns)) == nrow(datapull_yr_2008_09) ##should return TRUE if there are no duplicates
#^^duplicates removed!!


##check uniqueness of countryofIncorporation as a variable##
n_distinct(datapull_yr_2008_09$countryOfIncorporation)

table(datapull_yr_2008_09$countryOfIncorporation)


##********************************##
##****create year variable*********##
##********************************##

datapull_yr_2008_09$data_year <- 2008


##********************************##
##****clean Entry variable: registrationDate*********##
##********************************##


##change registrationdate to the format as.Date
datapull_yr_2008_09$registrationDate<-as.Date(as.character(datapull_yr_2008_09$registrationDate))



##********************************##
##****clean expirationDate*********##
##********************************##

##change expirationDate to the format as.Date
datapull_yr_2008_09$expirationDate<-as.Date(as.character(datapull_yr_2008_09$expirationDate))


##********************************##
##****clean businessstartDate*********##
##********************************##

##change businessstartDate to the format as.Date
datapull_yr_2008_09$businessStartDate<-as.Date(as.character(datapull_yr_2008_09$businessStartDate))


##********************************##
#************creating var describing registration yr and biz start date yr******#
#*******************************##

#************************
##create registration year
datapull_yr_2008_09 <- datapull_yr_2008_09 %>%
  dplyr::mutate(registrationYear = (format(datapull_yr_2008_09$registrationDate, "%Y")))

str(datapull_yr_2008_09$registrationYear) ##it's a character

#make registrationYear numeric
datapull_yr_2008_09$registrationYear<-as.numeric(as.character(datapull_yr_2008_09$registrationYear))

str(datapull_yr_2008_09$registrationYear)
#***********************
###

#********************************
##create business start date year
datapull_yr_2008_09 <- datapull_yr_2008_09 %>%
  dplyr::mutate(businessStartYear = (format(datapull_yr_2008_09$businessStartDate, "%Y")))

str(datapull_yr_2008_09$businessStartYear)

#make businessstartYear numeric

datapull_yr_2008_09$businessStartYear<-as.numeric(as.character(datapull_yr_2008_09$businessStartYear))
str(datapull_yr_2008_09$businessStartYear)
#********************************
#

##count the number of observations with registrationYear in other yeras

tab <- table(datapull_yr_2008_09$registrationYear)

tab

nrow(datapull_yr_2008_09[datapull_yr_2008_09$registrationYear==1997, ]) ##2 

nrow(datapull_yr_2008_09[datapull_yr_2008_09$registrationYear==1998, ]) ##15 

nrow(datapull_yr_2008_09[datapull_yr_2008_09$registrationYear==1999, ]) ##23

nrow(datapull_yr_2008_09[datapull_yr_2008_09$registrationYear==2000, ]) ##29

nrow(datapull_yr_2008_09[datapull_yr_2008_09$registrationYear==2001, ]) ##308

nrow(datapull_yr_2008_09[datapull_yr_2008_09$registrationYear==2002, ]) ##365

nrow(datapull_yr_2008_09[datapull_yr_2008_09$registrationYear==2003, ]) ##181

nrow(datapull_yr_2008_09[datapull_yr_2008_09$registrationYear==2004, ]) ##323

nrow(datapull_yr_2008_09[datapull_yr_2008_09$registrationYear==2005, ]) ##336

nrow(datapull_yr_2008_09[datapull_yr_2008_09$registrationYear==2006, ]) ##381

nrow(datapull_yr_2008_09[datapull_yr_2008_09$registrationYear==2007, ]) ##651

nrow(datapull_yr_2008_09[datapull_yr_2008_09$registrationYear==2008, ]) ##247

nrow(datapull_yr_2008_09[datapull_yr_2008_09$registrationYear==2009, ]) ##3



###



##********************************##
##******create firm age*******##
#firm age = registration date - business start date
##********************************##

##drop variables that did not enter in 2001 by registrationDate //DON'T NEED TO DO THIS NOW
#datapull_yr_2001 <- datapull_yr_2001[(format(datapull_yr_2001$registrationDate,"%Y")=="2000"), ]


##subtract the two years for the age
datapull_yr_2008_09 <- datapull_yr_2008_09 %>% 
  dplyr::mutate(starting_age = (datapull_yr_2008_09$registrationYear) - (datapull_yr_2008_09$businessStartYear)) 

##********************************##
##****Save File*********##
##********************************##

#write.csv(datapull_yr_2008_09, file="2008_09_datapull_cleaned.csv")

save(datapull_yr_2008_09, file="2008_09_datapull_cleaned.Rda")

#*******************************************************************************

#******************************************************************
########################2010-2011 SAM Data ################################
#******************************************************************

##Get and Save into CSV
Get_2010_11 <- read.xlsx("Get_2010_11.xlsx")

write.csv(Get_2010_11, file="2010_11_datapull_yr.csv")

##save(Get_2001, file="2001_datapull_yr.Rda")

##****************************************

##Use new CSV

datapull_yr_2010_11 <- read.csv("2010_11_datapull_yr.csv")

names(datapull_yr_2010_11)

##********************************##
##****Drop Vars not used*********##
##********************************##

##keep these vars

datapull_yr_2010_11 <- datapull_yr_2010_11 %>% select(activationDate, fiscalYearEndCloseDate, registrationDate, duns, expirationDate, status, businessStartDate, samAddress.countryCode, countryOfIncorporation, naics, naics.naicsCode, naics.naicsName)

##data editor

edit(datapull_yr_2010_11)

##number of missing per variable

sum(is.na(datapull_yr_2010_11$duns)) ##0 missing values for DUNS number

##how to drop a variable

# datapull_yr_2001$x <- NULL //if we want to delete the variable x



##********************************##
##****de-string if necessary*********##
##********************************##

##provide the structure of the data set

str(datapull_yr_2010_11)



##********************************##
##****Clean DUNS numbers*********##
##********************************##

##check uniqueness of DUNS as a variable##

n_distinct(datapull_yr_2010_11$duns) ##duns is NOT a unique identifier! there are 
#3663 unique cases of duns but 3675 observations, which indicates that there are duplicates

##another way to check duplicates

length(unique(datapull_yr_2010_11$duns)) == nrow(datapull_yr_2010_11) ##should return TRUE if there are no duplicates
#^^ returns FALSE, so there are indeed duplicates

###FIND DUPLICATES

n_occur <- data.frame(table(datapull_yr_2010_11$duns)) ##gives a data frame with a list of duns and the number of times they occurred

n_occur[n_occur$Freq >1, ] ##tells me which duns occur more than once and their frequency
# 343   20491833    2
# 946   97996974    2
# 1134 121345136    2
# 1205 129134289    3
# 2149 606476398    2
# 2426 694024753    2
# 2479 780234456    2
# 3340 926613761    2
# 3358 928992650    2
# 3430 944095827    2
# 3648 968883293    2

##subset to evaluate duplicates 

duplicates1 <- data.frame(datapull_yr_2010_11[c(datapull_yr_2010_11$duns==20491833), ])

duplicates2 <- data.frame(datapull_yr_2010_11[c(datapull_yr_2010_11$duns==97996974 ), ])

duplicates3 <- data.frame(datapull_yr_2010_11[c(datapull_yr_2010_11$duns==121345136), ])

duplicates4 <- data.frame(datapull_yr_2010_11[c(datapull_yr_2010_11$duns==129134289), ])

duplicates5 <- data.frame(datapull_yr_2010_11[c(datapull_yr_2010_11$duns==606476398), ])

duplicates6 <- data.frame(datapull_yr_2010_11[c(datapull_yr_2010_11$duns==694024753), ])

duplicates7 <- data.frame(datapull_yr_2010_11[c(datapull_yr_2010_11$duns==780234456), ])

duplicates8 <- data.frame(datapull_yr_2010_11[c(datapull_yr_2010_11$duns==926613761), ])

duplicates9 <- data.frame(datapull_yr_2010_11[c(datapull_yr_2010_11$duns==928992650 ), ])

duplicates10 <- data.frame(datapull_yr_2010_11[c(datapull_yr_2010_11$duns==944095827), ])

duplicates11 <- data.frame(datapull_yr_2010_11[c(datapull_yr_2010_11$duns==968883293), ])
##^^they are definitely duplicates so removing one of each of the two duplicates

##remove duplicates

datapull_yr_2010_11 <- datapull_yr_2010_11[!duplicated(datapull_yr_2010_11), ]

##check duplicates

length(unique(datapull_yr_2010_11$duns)) == nrow(datapull_yr_2010_11) ##should return TRUE if there are no duplicates
#^^duplicates removed!!


##check uniqueness of countryofIncorporation as a variable##
n_distinct(datapull_yr_2010_11$countryOfIncorporation)

table(datapull_yr_2010_11$countryOfIncorporation)


##********************************##
##****create year variable*********##
##********************************##

datapull_yr_2010_11$data_year <- 2010


##********************************##
##****clean Entry variable: registrationDate*********##
##********************************##


##change registrationdate to the format as.Date
datapull_yr_2010_11$registrationDate<-as.Date(as.character(datapull_yr_2010_11$registrationDate))



##********************************##
##****clean expirationDate*********##
##********************************##

##change expirationDate to the format as.Date
datapull_yr_2010_11$expirationDate<-as.Date(as.character(datapull_yr_2010_11$expirationDate))


##********************************##
##****clean businessstartDate*********##
##********************************##

##change businessstartDate to the format as.Date
datapull_yr_2010_11$businessStartDate<-as.Date(as.character(datapull_yr_2010_11$businessStartDate))


##********************************##
#************creating var describing registration yr and biz start date yr******#
#*******************************##

#************************
##create registration year
datapull_yr_2010_11 <- datapull_yr_2010_11 %>%
  dplyr::mutate(registrationYear = (format(datapull_yr_2010_11$registrationDate, "%Y")))

str(datapull_yr_2010_11$registrationYear) ##it's a character

#make registrationYear numeric
datapull_yr_2010_11$registrationYear<-as.numeric(as.character(datapull_yr_2010_11$registrationYear))

str(datapull_yr_2010_11$registrationYear)
#***********************
###

#********************************
##create business start date year
datapull_yr_2010_11 <- datapull_yr_2010_11 %>%
  dplyr::mutate(businessStartYear = (format(datapull_yr_2010_11$businessStartDate, "%Y")))

str(datapull_yr_2010_11$businessStartYear)

#make businessstartYear numeric

datapull_yr_2010_11$businessStartYear<-as.numeric(as.character(datapull_yr_2010_11$businessStartYear))
str(datapull_yr_2010_11$businessStartYear)
#********************************
#

##count the number of observations with registrationYear in other yeras

tab <- table(datapull_yr_2010_11$registrationYear)

tab

nrow(datapull_yr_2010_11[datapull_yr_2010_11$registrationYear==1997, ]) ##2 

nrow(datapull_yr_2010_11[datapull_yr_2010_11$registrationYear==1998, ]) ##15 

nrow(datapull_yr_2010_11[datapull_yr_2010_11$registrationYear==1999, ]) ##20

nrow(datapull_yr_2010_11[datapull_yr_2010_11$registrationYear==2000, ]) ##35

nrow(datapull_yr_2010_11[datapull_yr_2010_11$registrationYear==2001, ]) ##280

nrow(datapull_yr_2010_11[datapull_yr_2010_11$registrationYear==2002, ]) ##274

nrow(datapull_yr_2010_11[datapull_yr_2010_11$registrationYear==2003, ]) ##172

nrow(datapull_yr_2010_11[datapull_yr_2010_11$registrationYear==2004, ]) ##267

nrow(datapull_yr_2010_11[datapull_yr_2010_11$registrationYear==2005, ]) ##209

nrow(datapull_yr_2010_11[datapull_yr_2010_11$registrationYear==2006, ]) ##258

nrow(datapull_yr_2010_11[datapull_yr_2010_11$registrationYear==2007, ]) ##307

nrow(datapull_yr_2010_11[datapull_yr_2010_11$registrationYear==2008, ]) ##387

nrow(datapull_yr_2010_11[datapull_yr_2010_11$registrationYear==2009, ]) ##956

nrow(datapull_yr_2010_11[datapull_yr_2010_11$registrationYear==2010, ]) ##478

nrow(datapull_yr_2010_11[datapull_yr_2010_11$registrationYear==2011, ]) ##3


###



##********************************##
##******create firm age*******##
#firm age = registration date - business start date
##********************************##

##drop variables that did not enter in 2001 by registrationDate //DON'T NEED TO DO THIS NOW
#datapull_yr_2001 <- datapull_yr_2001[(format(datapull_yr_2001$registrationDate,"%Y")=="2000"), ]


##subtract the two years for the age
datapull_yr_2010_11 <- datapull_yr_2010_11 %>% 
  dplyr::mutate(starting_age = (datapull_yr_2010_11$registrationYear) - (datapull_yr_2010_11$businessStartYear)) 

##********************************##
##****Save File*********##
##********************************##

#write.csv(datapull_yr_2010_11, file="2010_11_datapull_cleaned.csv")

save(datapull_yr_2010_11, file="2010_11_datapull_cleaned.Rda")

#*******************************************************************************

#******************************************************************
########################2012-2013 SAM Data ################################
#******************************************************************

##Get and Save into CSV
Get_2012_13 <- read.xlsx("Get_2012_13.xlsx")

write.csv(Get_2012_13, file="2012_13_datapull_yr.csv")

##save(Get_2001, file="2001_datapull_yr.Rda")

##****************************************

##Use new CSV

datapull_yr_2012_13 <- read.csv("2012_13_datapull_yr.csv")

names(datapull_yr_2012_13)

##********************************##
##****Drop Vars not used*********##
##********************************##

##keep these vars

datapull_yr_2012_13 <- datapull_yr_2012_13 %>% select(activationDate, fiscalYearEndCloseDate, registrationDate, duns, expirationDate, status, businessStartDate, samAddress.countryCode, countryOfIncorporation, naics, naics.naicsCode, naics.naicsName)

##data editor

edit(datapull_yr_2012_13)

##number of missing per variable

sum(is.na(datapull_yr_2012_13$duns)) ##0 missing values for DUNS number

##how to drop a variable

# datapull_yr_2001$x <- NULL //if we want to delete the variable x



##********************************##
##****de-string if necessary*********##
##********************************##

##provide the structure of the data set

str(datapull_yr_2012_13)



##********************************##
##****Clean DUNS numbers*********##
##********************************##

##check uniqueness of DUNS as a variable##

n_distinct(datapull_yr_2012_13$duns) ##duns is NOT a unique identifier! there are 
#2686 unique cases of duns but 2714 observations, which indicates that there are duplicates

##another way to check duplicates

length(unique(datapull_yr_2012_13$duns)) == nrow(datapull_yr_2012_13) ##should return TRUE if there are no duplicates
#^^ returns FALSE, so there are indeed duplicates

###FIND DUPLICATES

n_occur <- data.frame(table(datapull_yr_2012_13$duns)) ##gives a data frame with a list of duns and the number of times they occurred

n_occur[n_occur$Freq >1, ] ##tells me which duns occur more than once and their frequency
# 47     3242013    7
# 186   13287602    3
# 668   72632552    3
# 1015 114281793    2
# 1051 123736899    2
# 1182 136792640    2
# 1212 140986105    2
# 1412 180865180    4
# 1569 362358574    4
# 1642 601466522    2
# 1689 608958653    2
# 1854 783227189    2
# 1899 787688258    2
# 2157 829376321    2
# 2203 831067731    2
# 2470 958660193    2
# 2577 966307618    2

##subset to evaluate duplicates 

duplicates1 <- data.frame(datapull_yr_2012_13[c(datapull_yr_2012_13$duns==3242013), ])

duplicates2 <- data.frame(datapull_yr_2012_13[c(datapull_yr_2012_13$duns==13287602), ])

duplicates3 <- data.frame(datapull_yr_2012_13[c(datapull_yr_2012_13$duns==72632552), ])

duplicates4 <- data.frame(datapull_yr_2012_13[c(datapull_yr_2012_13$duns==114281793), ])

duplicates5 <- data.frame(datapull_yr_2012_13[c(datapull_yr_2012_13$duns==123736899 ), ])

duplicates6 <- data.frame(datapull_yr_2012_13[c(datapull_yr_2012_13$duns==136792640), ])

duplicates7 <- data.frame(datapull_yr_2012_13[c(datapull_yr_2012_13$duns==140986105), ])

duplicates8 <- data.frame(datapull_yr_2012_13[c(datapull_yr_2012_13$duns==180865180), ])

duplicates9 <- data.frame(datapull_yr_2012_13[c(datapull_yr_2012_13$duns==362358574), ])

duplicates10 <- data.frame(datapull_yr_2012_13[c(datapull_yr_2012_13$duns==601466522), ])

duplicates11 <- data.frame(datapull_yr_2012_13[c(datapull_yr_2012_13$duns==783227189), ])

duplicates12 <- data.frame(datapull_yr_2012_13[c(datapull_yr_2012_13$duns==787688258), ])

duplicates13 <- data.frame(datapull_yr_2012_13[c(datapull_yr_2012_13$duns==829376321), ])

duplicates14 <- data.frame(datapull_yr_2012_13[c(datapull_yr_2012_13$duns==831067731), ])

duplicates15 <- data.frame(datapull_yr_2012_13[c(datapull_yr_2012_13$duns==958660193), ])

duplicates16 <- data.frame(datapull_yr_2012_13[c(datapull_yr_2012_13$duns==608958653), ])

duplicates17 <- data.frame(datapull_yr_2012_13[c(datapull_yr_2012_13$duns==966307618), ])
##^^they are definitely duplicates so removing one of each of the two duplicates

##remove duplicates

datapull_yr_2012_13 <- datapull_yr_2012_13[!duplicated(datapull_yr_2012_13), ]

##check duplicates

length(unique(datapull_yr_2012_13$duns)) == nrow(datapull_yr_2012_13) ##should return TRUE if there are no duplicates
#^^duplicates removed!!


##check uniqueness of countryofIncorporation as a variable##
n_distinct(datapull_yr_2012_13$countryOfIncorporation)

table(datapull_yr_2012_13$countryOfIncorporation)


##********************************##
##****create year variable*********##
##********************************##

datapull_yr_2012_13$data_year <- 2012


##********************************##
##****clean Entry variable: registrationDate*********##
##********************************##


##change registrationdate to the format as.Date
datapull_yr_2012_13$registrationDate<-as.Date(as.character(datapull_yr_2012_13$registrationDate))



##********************************##
##****clean expirationDate*********##
##********************************##

##change expirationDate to the format as.Date
datapull_yr_2012_13$expirationDate<-as.Date(as.character(datapull_yr_2012_13$expirationDate))


##********************************##
##****clean businessstartDate*********##
##********************************##

##change businessstartDate to the format as.Date
datapull_yr_2012_13$businessStartDate<-as.Date(as.character(datapull_yr_2012_13$businessStartDate))


##********************************##
#************creating var describing registration yr and biz start date yr******#
#*******************************##

#************************
##create registration year
datapull_yr_2012_13 <- datapull_yr_2012_13 %>%
  dplyr::mutate(registrationYear = (format(datapull_yr_2012_13$registrationDate, "%Y")))

str(datapull_yr_2012_13$registrationYear) ##it's a character

#make registrationYear numeric
datapull_yr_2012_13$registrationYear<-as.numeric(as.character(datapull_yr_2012_13$registrationYear))

str(datapull_yr_2012_13$registrationYear)
#***********************
###

#********************************
##create business start date year
datapull_yr_2012_13 <- datapull_yr_2012_13 %>%
  dplyr::mutate(businessStartYear = (format(datapull_yr_2012_13$businessStartDate, "%Y")))

str(datapull_yr_2012_13$businessStartYear)

#make businessstartYear numeric

datapull_yr_2012_13$businessStartYear<-as.numeric(as.character(datapull_yr_2012_13$businessStartYear))
str(datapull_yr_2012_13$businessStartYear)
#********************************
#

##count the number of observations with registrationYear in other yeras

tab <- table(datapull_yr_2012_13$registrationYear)

tab

nrow(datapull_yr_2012_13[datapull_yr_2012_13$registrationYear==1997, ]) ##1 

nrow(datapull_yr_2012_13[datapull_yr_2012_13$registrationYear==1998, ]) ##8 

nrow(datapull_yr_2012_13[datapull_yr_2012_13$registrationYear==1999, ]) ##6

nrow(datapull_yr_2012_13[datapull_yr_2012_13$registrationYear==2000, ]) ##27

nrow(datapull_yr_2012_13[datapull_yr_2012_13$registrationYear==2001, ]) ##157

nrow(datapull_yr_2012_13[datapull_yr_2012_13$registrationYear==2002, ]) ##176

nrow(datapull_yr_2012_13[datapull_yr_2012_13$registrationYear==2003, ]) ##147

nrow(datapull_yr_2012_13[datapull_yr_2012_13$registrationYear==2004, ]) ##257

nrow(datapull_yr_2012_13[datapull_yr_2012_13$registrationYear==2005, ]) ##137

nrow(datapull_yr_2012_13[datapull_yr_2012_13$registrationYear==2006, ]) ##148

nrow(datapull_yr_2012_13[datapull_yr_2012_13$registrationYear==2007, ]) ##165

nrow(datapull_yr_2012_13[datapull_yr_2012_13$registrationYear==2008, ]) ##154

nrow(datapull_yr_2012_13[datapull_yr_2012_13$registrationYear==2009, ]) ##311

nrow(datapull_yr_2012_13[datapull_yr_2012_13$registrationYear==2010, ]) ##261

nrow(datapull_yr_2012_13[datapull_yr_2012_13$registrationYear==2011, ]) ##373

nrow(datapull_yr_2012_13[datapull_yr_2012_13$registrationYear==2012, ]) ##358
###



##********************************##
##******create firm age*******##
#firm age = registration date - business start date
##********************************##

##drop variables that did not enter in 2001 by registrationDate //DON'T NEED TO DO THIS NOW
#datapull_yr_2001 <- datapull_yr_2001[(format(datapull_yr_2001$registrationDate,"%Y")=="2000"), ]


##subtract the two years for the age
datapull_yr_2012_13 <- datapull_yr_2012_13 %>% 
  dplyr::mutate(starting_age = (datapull_yr_2012_13$registrationYear) - (datapull_yr_2012_13$businessStartYear)) 

##********************************##
##****Save File*********##
##********************************##

#write.csv(datapull_yr_2012_13, file="2012_13_datapull_cleaned.csv")

save(datapull_yr_2012_13, file="2012_13_datapull_cleaned.Rda")

#*******************************************************************************

#******************************************************************
########################2014-2015 SAM Data ################################
#******************************************************************

##Get and Save into CSV
Get_2014_15 <- read.csv("Get_2014_15.csv")

write.csv(Get_2014_15, file="2014_15_datapull_yr.csv")

##save(Get_2001, file="2001_datapull_yr.Rda")

##****************************************

##Use new CSV

datapull_yr_2014_15 <- read.csv("2014_15_datapull_yr.csv")

names(datapull_yr_2014_15)

##********************************##
##****Drop Vars not used*********##
##********************************##

##keep these vars

datapull_yr_2014_15 <- datapull_yr_2014_15 %>% select(activationDate, fiscalYearEndCloseDate, registrationDate, duns, expirationDate, status, businessStartDate, samAddress.countryCode, countryOfIncorporation, naics, naics.naicsCode, naics.naicsName)

##data editor

edit(datapull_yr_2014_15)

##number of missing per variable

sum(is.na(datapull_yr_2014_15$duns)) ##0 missing values for DUNS number

##how to drop a variable

# datapull_yr_2001$x <- NULL //if we want to delete the variable x



##********************************##
##****de-string if necessary*********##
##********************************##

##provide the structure of the data set

str(datapull_yr_2014_15)



##********************************##
##****Clean DUNS numbers*********##
##********************************##

##check uniqueness of DUNS as a variable##

n_distinct(datapull_yr_2014_15$duns) ##duns is NOT a unique identifier! there are 
#693 unique cases of duns but 7030 observations, which indicates that there are duplicates

##another way to check duplicates

length(unique(datapull_yr_2014_15$duns)) == nrow(datapull_yr_2014_15) ##should return TRUE if there are no duplicates
#^^ returns FALSE, so there are indeed duplicates

###FIND DUPLICATES

n_occur <- data.frame(table(datapull_yr_2014_15$duns)) ##gives a data frame with a list of duns and the number of times they occurred

n_occur[n_occur$Freq >1, ] ##tells me which duns occur more than once and their frequency
# 24     1804074    2
# 189    6919179    2
# 231    7912298    3
# 540   18692550    2
# 567   19762801    2
# 709   24355526    2
# 781   27871090    2
# 844   31141586    2
# 957   37190233    2
# 1056  42164145    2
# 1419  61021143    2
# 1474  64645336    2
# 1537  68281815    2
# 1582  70110358    2
# 1750  78375068    2
# 1942  78647475    2
# 2884 102796976    2
# 3409 141149893    2
# 3540 147533074    2
# 3602 150875045    2
# 3611 151568024    2
# 3661 156154528    2
# 3823 168787955    3
# 4099 194561903    2
# 4129 197174126    2
# 4632 611633913    2
# 4649 612830703    4
# 4676 616392754    2
# 4736 620825091    2
# 5028 780398009    2
# 5175 790439710    2
# 5234 795719793    2
# 5307 800189966    2
# 5427 806759655    2
# 5868 831562041    2
# 5885 831740134    2
# 5890 831809491    2
# 6041 840147065    2
# 6078 848490079    2
# 6395 945062438    2
# 6467 959050519    2
# 6584 962736547    2
# 6926 969006977    2

##subset to evaluate duplicates 

duplicates1 <- data.frame(datapull_yr_2014_15[c(datapull_yr_2014_15$duns==1804074), ])

duplicates2 <- data.frame(datapull_yr_2014_15[c(datapull_yr_2014_15$duns==6919179), ])

duplicates3 <- data.frame(datapull_yr_2014_15[c(datapull_yr_2014_15$duns==7912298), ])

duplicates4 <- data.frame(datapull_yr_2014_15[c(datapull_yr_2014_15$duns==18692550), ])

duplicates5 <- data.frame(datapull_yr_2014_15[c(datapull_yr_2014_15$duns==19762801), ])

duplicates6 <- data.frame(datapull_yr_2014_15[c(datapull_yr_2014_15$duns==24355526), ])

duplicates7 <- data.frame(datapull_yr_2014_15[c(datapull_yr_2014_15$duns==27871090), ])

duplicates8 <- data.frame(datapull_yr_2014_15[c(datapull_yr_2014_15$duns==31141586), ])

duplicates9 <- data.frame(datapull_yr_2014_15[c(datapull_yr_2014_15$duns==37190233), ])

duplicates10 <- data.frame(datapull_yr_2014_15[c(datapull_yr_2014_15$duns==42164145), ])

duplicates11 <- data.frame(datapull_yr_2014_15[c(datapull_yr_2014_15$duns==61021143), ])

duplicates12 <- data.frame(datapull_yr_2014_15[c(datapull_yr_2014_15$duns==64645336), ])

duplicates13 <- data.frame(datapull_yr_2014_15[c(datapull_yr_2014_15$duns==68281815), ])

duplicates14 <- data.frame(datapull_yr_2014_15[c(datapull_yr_2014_15$duns==70110358), ])

duplicates15 <- data.frame(datapull_yr_2014_15[c(datapull_yr_2014_15$duns==78375068), ])

duplicates16 <- data.frame(datapull_yr_2014_15[c(datapull_yr_2014_15$duns==78647475), ])

duplicates17 <- data.frame(datapull_yr_2014_15[c(datapull_yr_2014_15$duns==102796976), ])
##^^they are definitely duplicates so removing one of each of the two duplicates ..also didn't check all of them here

##remove duplicates

datapull_yr_2014_15 <- datapull_yr_2014_15[!duplicated(datapull_yr_2014_15), ]

##check duplicates

length(unique(datapull_yr_2014_15$duns)) == nrow(datapull_yr_2014_15) ##should return TRUE if there are no duplicates
#^^duplicates removed!!


##check uniqueness of countryofIncorporation as a variable##
n_distinct(datapull_yr_2014_15$countryOfIncorporation)

table(datapull_yr_2014_15$countryOfIncorporation)


##********************************##
##****create year variable*********##
##********************************##

datapull_yr_2014_15$data_year <- 2014


##********************************##
##****clean Entry variable: registrationDate*********##
##********************************##


##change registrationdate to the format as.Date
datapull_yr_2014_15$registrationDate<-as.Date(as.character(datapull_yr_2014_15$registrationDate))



##********************************##
##****clean expirationDate*********##
##********************************##

##change expirationDate to the format as.Date
datapull_yr_2014_15$expirationDate<-as.Date(as.character(datapull_yr_2014_15$expirationDate))


##********************************##
##****clean businessstartDate*********##
##********************************##

##change businessstartDate to the format as.Date
datapull_yr_2014_15$businessStartDate<-as.Date(as.character(datapull_yr_2014_15$businessStartDate))


##********************************##
#************creating var describing registration yr and biz start date yr******#
#*******************************##

#************************
##create registration year
datapull_yr_2014_15 <- datapull_yr_2014_15 %>%
  dplyr::mutate(registrationYear = (format(datapull_yr_2014_15$registrationDate, "%Y")))

str(datapull_yr_2014_15$registrationYear) ##it's a character

#make registrationYear numeric
datapull_yr_2014_15$registrationYear<-as.numeric(as.character(datapull_yr_2014_15$registrationYear))

str(datapull_yr_2014_15$registrationYear)
#***********************
###

#********************************
##create business start date year
datapull_yr_2014_15 <- datapull_yr_2014_15 %>%
  dplyr::mutate(businessStartYear = (format(datapull_yr_2014_15$businessStartDate, "%Y")))

str(datapull_yr_2014_15$businessStartYear)

#make businessstartYear numeric

datapull_yr_2014_15$businessStartYear<-as.numeric(as.character(datapull_yr_2014_15$businessStartYear))
str(datapull_yr_2014_15$businessStartYear)
#********************************
#

##count the number of observations with registrationYear in other yeras

tab <- table(datapull_yr_2014_15$registrationYear)

tab

nrow(datapull_yr_2014_15[datapull_yr_2014_15$registrationYear==1997, ]) ##1 

nrow(datapull_yr_2014_15[datapull_yr_2014_15$registrationYear==1998, ]) ##12 

nrow(datapull_yr_2014_15[datapull_yr_2014_15$registrationYear==1999, ]) ##19

nrow(datapull_yr_2014_15[datapull_yr_2014_15$registrationYear==2000, ]) ##31

nrow(datapull_yr_2014_15[datapull_yr_2014_15$registrationYear==2001, ]) ##448

nrow(datapull_yr_2014_15[datapull_yr_2014_15$registrationYear==2002, ]) ##342

nrow(datapull_yr_2014_15[datapull_yr_2014_15$registrationYear==2003, ]) ##275

nrow(datapull_yr_2014_15[datapull_yr_2014_15$registrationYear==2004, ]) ##401

nrow(datapull_yr_2014_15[datapull_yr_2014_15$registrationYear==2005, ]) ##294

nrow(datapull_yr_2014_15[datapull_yr_2014_15$registrationYear==2006, ]) ##275

nrow(datapull_yr_2014_15[datapull_yr_2014_15$registrationYear==2007, ]) ##306

nrow(datapull_yr_2014_15[datapull_yr_2014_15$registrationYear==2008, ]) ##348

nrow(datapull_yr_2014_15[datapull_yr_2014_15$registrationYear==2009, ]) ##551

nrow(datapull_yr_2014_15[datapull_yr_2014_15$registrationYear==2010, ]) ##491

nrow(datapull_yr_2014_15[datapull_yr_2014_15$registrationYear==2011, ]) ##546

nrow(datapull_yr_2014_15[datapull_yr_2014_15$registrationYear==2012, ]) ##543

nrow(datapull_yr_2014_15[datapull_yr_2014_15$registrationYear==2012, ]) ##1214

nrow(datapull_yr_2014_15[datapull_yr_2014_15$registrationYear==2012, ]) ##863

nrow(datapull_yr_2014_15[datapull_yr_2014_15$registrationYear==2012, ]) ##23
###



##********************************##
##******create firm age*******##
#firm age = registration date - business start date
##********************************##

##drop variables that did not enter in 2001 by registrationDate //DON'T NEED TO DO THIS NOW
#datapull_yr_2001 <- datapull_yr_2001[(format(datapull_yr_2001$registrationDate,"%Y")=="2000"), ]


##subtract the two years for the age
datapull_yr_2014_15 <- datapull_yr_2014_15 %>% 
  dplyr::mutate(starting_age = (datapull_yr_2014_15$registrationYear) - (datapull_yr_2014_15$businessStartYear)) 

##********************************##
##****Save File*********##
##********************************##

#write.csv(datapull_yr_2014_15, file="2014_15_datapull_cleaned.csv")

save(datapull_yr_2014_15, file="2014_15_datapull_cleaned.Rda")

#*******************************************************************************


#******************************************************************
########################2016-2017 SAM Data ################################
#******************************************************************

##Get and Save into CSV
Get_2016_17 <- read.csv("Get_2016_17.csv")

write.csv(Get_2016_17, file="2016_17_datapull_yr.csv")

##save(Get_2001, file="2001_datapull_yr.Rda")

##****************************************

##Use new CSV

datapull_yr_2016_17 <- read.csv("2016_17_datapull_yr.csv")

names(datapull_yr_2016_17)

##********************************##
##****Drop Vars not used*********##
##********************************##

##keep these vars

datapull_yr_2016_17 <- datapull_yr_2016_17 %>% select(activationDate, fiscalYearEndCloseDate, registrationDate, duns, expirationDate, status, businessStartDate, samAddress.countryCode, countryOfIncorporation, naics, naics.naicsCode, naics.naicsName)

##data editor

edit(datapull_yr_2016_17)

##number of missing per variable

sum(is.na(datapull_yr_2016_17$duns)) ##0 missing values for DUNS number

##how to drop a variable

# datapull_yr_2001$x <- NULL //if we want to delete the variable x



##********************************##
##****de-string if necessary*********##
##********************************##

##provide the structure of the data set

str(datapull_yr_2016_17)



##********************************##
##****Clean DUNS numbers*********##
##********************************##

##check uniqueness of DUNS as a variable##

n_distinct(datapull_yr_2016_17$duns) ##duns IS a unique identifier! 

##another way to check duplicates

length(unique(datapull_yr_2016_17$duns)) == nrow(datapull_yr_2016_17) ##should return TRUE if there are no duplicates
#^^ returns TRUE, so there are indeed duplicates

###FIND DUPLICATES

n_occur <- data.frame(table(datapull_yr_2016_17$duns)) ##gives a data frame with a list of duns and the number of times they occurred

n_occur[n_occur$Freq >1, ] ##tells me which duns occur more than once and their frequency

##remove duplicates (not necessary here)

# datapull_yr_2016_17 <- datapull_yr_2016_17[!duplicated(datapull_yr_2016_17), ]
# 
# ##check duplicates
# 
# length(unique(datapull_yr_2016_17$duns)) == nrow(datapull_yr_2016_17) ##should return TRUE if there are no duplicates
#^^duplicates removed!!


##check uniqueness of countryofIncorporation as a variable##
n_distinct(datapull_yr_2016_17$countryOfIncorporation)

table(datapull_yr_2016_17$countryOfIncorporation)


##********************************##
##****create year variable*********##
##********************************##

datapull_yr_2016_17$data_year <- 2016


##********************************##
##****clean Entry variable: registrationDate*********##
##********************************##


##change registrationdate to the format as.Date
datapull_yr_2016_17$registrationDate<-as.Date(as.character(datapull_yr_2016_17$registrationDate))



##********************************##
##****clean expirationDate*********##
##********************************##

##change expirationDate to the format as.Date
datapull_yr_2016_17$expirationDate<-as.Date(as.character(datapull_yr_2016_17$expirationDate))


##********************************##
##****clean businessstartDate*********##
##********************************##

##change businessstartDate to the format as.Date
datapull_yr_2016_17$businessStartDate<-as.Date(as.character(datapull_yr_2016_17$businessStartDate))


##********************************##
#************creating var describing registration yr and biz start date yr******#
#*******************************##

#************************
##create registration year
datapull_yr_2016_17 <- datapull_yr_2016_17 %>%
  dplyr::mutate(registrationYear = (format(datapull_yr_2016_17$registrationDate, "%Y")))

str(datapull_yr_2016_17$registrationYear) ##it's a character

#make registrationYear numeric
datapull_yr_2016_17$registrationYear<-as.numeric(as.character(datapull_yr_2016_17$registrationYear))

str(datapull_yr_2016_17$registrationYear)
#***********************
###

#********************************
##create business start date year
datapull_yr_2016_17 <- datapull_yr_2016_17 %>%
  dplyr::mutate(businessStartYear = (format(datapull_yr_2016_17$businessStartDate, "%Y")))

str(datapull_yr_2016_17$businessStartYear)

#make businessstartYear numeric

datapull_yr_2016_17$businessStartYear<-as.numeric(as.character(datapull_yr_2016_17$businessStartYear))
str(datapull_yr_2016_17$businessStartYear)
#********************************
#

##count the number of observations with registrationYear in other yeras

tab <- table(datapull_yr_2016_17$registrationYear)

tab

nrow(datapull_yr_2016_17[datapull_yr_2016_17$registrationYear==1994, ]) ##1

nrow(datapull_yr_2016_17[datapull_yr_2016_17$registrationYear==1997, ]) ##2

nrow(datapull_yr_2016_17[datapull_yr_2016_17$registrationYear==1998, ]) ##8 

nrow(datapull_yr_2016_17[datapull_yr_2016_17$registrationYear==1999, ]) ##8

nrow(datapull_yr_2016_17[datapull_yr_2016_17$registrationYear==2000, ]) ##27

nrow(datapull_yr_2016_17[datapull_yr_2016_17$registrationYear==2001, ]) ##226

nrow(datapull_yr_2016_17[datapull_yr_2016_17$registrationYear==2002, ]) ##226

nrow(datapull_yr_2016_17[datapull_yr_2016_17$registrationYear==2003, ]) ##149

nrow(datapull_yr_2016_17[datapull_yr_2016_17$registrationYear==2004, ]) ##190

nrow(datapull_yr_2016_17[datapull_yr_2016_17$registrationYear==2005, ]) ##175

nrow(datapull_yr_2016_17[datapull_yr_2016_17$registrationYear==2006, ]) ##193

nrow(datapull_yr_2016_17[datapull_yr_2016_17$registrationYear==2007, ]) ##225

nrow(datapull_yr_2016_17[datapull_yr_2016_17$registrationYear==2008, ]) ##256

nrow(datapull_yr_2016_17[datapull_yr_2016_17$registrationYear==2009, ]) ##404

nrow(datapull_yr_2016_17[datapull_yr_2016_17$registrationYear==2010, ]) ##347

nrow(datapull_yr_2016_17[datapull_yr_2016_17$registrationYear==2011, ]) ##372

nrow(datapull_yr_2016_17[datapull_yr_2016_17$registrationYear==2012, ]) ##321

nrow(datapull_yr_2016_17[datapull_yr_2016_17$registrationYear==2013, ]) ##448

nrow(datapull_yr_2016_17[datapull_yr_2016_17$registrationYear==2014, ]) ##448

nrow(datapull_yr_2016_17[datapull_yr_2016_17$registrationYear==2015, ]) ##1455

nrow(datapull_yr_2016_17[datapull_yr_2016_17$registrationYear==2016, ]) ##1088

###



##********************************##
##******create firm age*******##
#firm age = registration date - business start date
##********************************##

##drop variables that did not enter in 2001 by registrationDate //DON'T NEED TO DO THIS NOW
#datapull_yr_2001 <- datapull_yr_2001[(format(datapull_yr_2001$registrationDate,"%Y")=="2000"), ]


##subtract the two years for the age
datapull_yr_2016_17 <- datapull_yr_2016_17 %>% 
  dplyr::mutate(starting_age = (datapull_yr_2016_17$registrationYear) - (datapull_yr_2016_17$businessStartYear)) 

##********************************##
##****Save File*********##
##********************************##

##write.csv(datapull_yr_2016_17, file="2016_17_datapull_cleaned.csv")

save(datapull_yr_2016_17, file="2016_17_datapull_cleaned.Rda")

#*******************************************************************************

#******************************************************************
########################Combine all years and remove duplicates ################################
#******************************************************************

#**************
##combine
#**************
datapull_all <- rbind(datapull_yr_2001, datapull_yr_2002_03, datapull_yr_2004_05, datapull_yr_2006_07, datapull_yr_2008_09, datapull_yr_2010_11, datapull_yr_2012_13, datapull_yr_2014_15, datapull_yr_2016_17)

#**************
##investigate duplicates
#**************

##check uniqueness of DUNS as a variable##

n_distinct(datapull_all$duns) ##duns is not, 36424 unique and 36426 all together 

##another way to check duplicates

length(unique(datapull_all$duns)) == nrow(datapull_all) ##should return TRUE if there are no duplicates
#^^ returns TRUE, so there are indeed duplicates

###FIND DUPLICATES

n_occur <- data.frame(table(datapull_all$duns)) ##gives a data frame with a list of duns and the number of times they occurred

n_occur[n_occur$Freq >1, ] ##tells me which duns occur more than once and their frequency

##investigate the duplicates

duplicates1 <- data.frame(datapull_all[c(datapull_all$duns==5824268), ]) ##same one that showed up in ddata pull year 2008 and data pull year 2010

duplicates2 <- data.frame(datapull_all[c(datapull_all$duns==155512853), ]) ##same one that showed up in ddata pull year 2008 and data pull year 2010


##remove duplicates 

datapull_all <- datapull_all[!duplicated(datapull_all$duns), ]

# ##check duplicates
length(unique(datapull_all$duns)) == nrow(datapull_all) ##should return TRUE if there are no duplicates
#^^duplicates removed!!


##********************************##
##****Save File*********##
##********************************##

#write.csv(datapull_all, file="datapull_all.csv")

save(datapull_all, file="SAM_datapull_all.Rda")

#******************************************************


#******************************************************************
########################Count number of new entrants in each year! ################################
#******************************************************************
#datapull_all <- read.csv("datapull_all.csv")

load(file = "SAM_datapull_all.Rda")



registrationyear_count <- table(datapull_all$registrationYear)

registrationyear_count






#Greg Replication
process_pull<-function(data){
  data<- data %>% select(activationDate, fiscalYearEndCloseDate, 
                         registrationDate, duns, expirationDate, 
                         status, businessStartDate, samAddress.countryCode, 
                         countryOfIncorporation, naics, 
                         naics.naicsCode, naics.naicsName)
  
  ##********************************##
  ##****clean Entry variable: registrationDate*********##
  ##********************************##
  
  ##change registrationdate to the format as.Date
  data$registrationDate<-as.Date(as.character(data$registrationDate))
  
  ##********************************##
  ##****clean expirationDate*********##
  ##********************************##
  
  ##change expirationDate to the format as.Date
  data$expirationDate<-as.Date(as.character(data$expirationDate))
  
  ##********************************##
  ##****clean businessstartDate*********##
  ##********************************##
  
  ##change businessstartDate to the format as.Date
  data$businessStartDate<-as.Date(as.character(data$businessStartDate))
  
  ##********************************##
  #************creating var describing registration yr and biz start date yr******#
  #*******************************##
  
  #************************
  ##create registration year
  data$registrationYear <- format(data$registrationDate, "%Y")
  
  #make registrationYear numeric
  data$registrationYear<-as.numeric(as.character(data$registrationYear))
  
  #********************************
  ##create business start date year
  data$businessStartYear <- format(data$businessStartDate, "%Y")
  
  #make businessstartYear numeric
  
  data$businessStartYear<-as.numeric(as.character(data$businessStartYear))
  #********************************
  data
}

get <- process_pull( read.xlsx("Get_2001.xlsx") )
get <-rbind(get,  process_pull( read.xlsx("Get_2002_03.xlsx")))
get <- rbind(get, process_pull(  read.xlsx("Get_2004_05.xlsx")))
get<-rbind(get,process_pull(  read.xlsx("Get_2004_05.xlsx")))
get<-rbind(get,process_pull(  read.xlsx("Get_2006_07.xlsx")))
get<-rbind(get,process_pull(  read.xlsx("Get_2008_09.xlsx")))
get<-rbind(get,process_pull(  read.xlsx("Get_2010_11.xlsx")))
get<-rbind(get, process_pull(  read.xlsx("Get_2012_13.xlsx")))
get<-rbind(get, process_pull(  read.csv("Get_2014_15.csv")))
get<-rbind(get, process_pull(  read.csv("Get_2016_17.csv")))

fastcheck<-get %>% group_by(duns) %>%
  dplyr::summarize(minofreg=min(registrationYear,na.rm=TRUE)) %>%
  group_by(minofreg) %>% dplyr::summarise(duns_count=length(duns)) 

#*********************************************************************
#************************************************************************

#******************************************************************
########################FPDS data################################
#******************************************************************

#******************************************************
###set up

setwd("K:/2018-01 NPS New Entrants/Data/Data/Cleaning data/FPDS")


##Get and Save into CSV
FPDS_data <- read.delim("Vendor.SP_DunsnumberNewEntrants_w_SAMuniqueDunsRAW.txt", fill = TRUE, header=TRUE,  na.strings = c("", "NULL"))


write.csv(FPDS_data, file="FPDS_w_SAMuniqueDUNS.csv")


#*****************************************************
##Use the data

FPDS_data <- read.csv("FPDS_w_SAMuniqueDUNS.csv")

##also load SAM data
setwd("K:/2018-01 NPS New Entrants/Data/Data/Cleaning data/Get files")

load(file = "datapull_all.Rda")


##back to FPDs wd
setwd("K:/2018-01 NPS New Entrants/Data/Data/Cleaning data/FPDS")


##*****************************************##
############prelim cleaning#################
#*******************************************#

##rename DirectCOBSD to biz_size and i..fiscal_year to FYear

names(FPDS_data)

names(FPDS_data)[names(FPDS_data) == "DirectCOBSD"] <- "biz_size"

names(FPDS_data)[names(FPDS_data) == "?..fiscal_year"] <- "FYear"


### Investigate how many unique duns # there are

unique_FPDS_duns <- data.frame(table(FPDS_data$Dunsnumber)) ##gives a data frame with a list of duns and the number of times they occurred
##there are only 9,318 

##sort by year and duns#

FPDS_data <- FPDS_data[order(FPDS_data$FYear, FPDS_data$Dunsnumber), ]

##drop entries in FY before 2000

FPDS_data <- FPDS_data[!(FPDS_data$FYear<2000), ]

##count number of unique duns numbers
unique_FPDS_duns_studyperiod <- data.frame(table(FPDS_data$Dunsnumber)) ##gives a data frame with a list of duns and the number of times they occurred



##*****************************************##
############Choose NAICS category depending#################
#on lgst dollar amount
#*******************************************#

##NAICS2 = the NAICS category that has the highest amount of obligations

##count number of NAs in NAICS2
sum(is.na(FPDS_data$NAICS2)) ##30,655 --> 14 percent of the observations

##create a variable that is equal to the total obligations over the 
# study period for each duns number


##create subsetted data with only FY, duns, obligated amount, and naics
Duns_obligations <- data.frame( FPDS_data$FYear, FPDS_data$Dunsnumber, FPDS_data$obligatedAmount, FPDS_data$NAICS2)

##count how many rows each Dunsnumber has
names(Duns_obligations)
unique_duns_fpdsclean <- data.frame(table(Duns_obligations$FPDS_data.Dunsnumbe)) ##gives a data frame with a list of duns and the number of times they occurred

##sort by duns
names(Duns_obligations)
Duns_obligations <- Duns_obligations[order(Duns_obligations$FPDS_data.Dunsnumber, Duns_obligations$FPDS_data.FYear, Duns_obligations$FPDS_data.NAICS2), ]


##step 1: use dplyr to create a new data frame grouped by DUNS nad NAICS
#and sum obligated amount for all unique combinations
DO_newvar <- Duns_obligations %>% group_by(FPDS_data.Dunsnumber, FPDS_data.NAICS2) %>%
    dplyr::summarize(obligated_amount=sum(FPDS_data.obligatedAmount, na.rm=TRUE))

#step2: eliminate the combinations that are not the highest
DO_max_newvar <- DO_newvar %>% group_by(FPDS_data.Dunsnumber) %>%
  dplyr::mutate(desc_rank = row_number(desc(obligated_amount))) ##row_number with ties (there were 31) it chooses the first one  

#step3: subset DO_max_newvar where rank==1

duns_and_NAICS <- subset(DO_max_newvar, desc_rank==1)

#check to see if FPDS_data.Dunsnumber is a unique identifier
##check uniqueness of DUNS as a variable##

n_distinct(duns_and_NAICS$FPDS_data.Dunsnumber) ##returns 8733 when should return 8764

length(unique(duns_and_NAICS$FPDS_data.Dunsnumber)) == nrow(duns_and_NAICS) ##FALSE

table(duns_and_NAICS$desc_rank)

n_occur <- data.frame(table(duns_and_NAICS$FPDS_data.Dunsnumber)) ##gives a data frame with a list of duns and the number of times they occurred

duplicates_in_dunsandNAICS <- n_occur[n_occur$Freq >1, ] ##tells me which duns occur more than once and their frequency



#drop obligatedamount and desc_rank
names(duns_and_NAICS)
duns_and_NAICS$obligated_amount <- duns_and_NAICS$desc_rank <- NULL

#change name of duns in duns_and_NAICS
names(duns_and_NAICS)
names(FPDS_data)

names(duns_and_NAICS)[names(duns_and_NAICS) == "FPDS_data.Dunsnumber"] <- "Dunsnumber"
names(duns_and_NAICS)[names(duns_and_NAICS) == "FPDS_data.NAICS2"] <- "topNAICS"

#step 4: left join between FPDS_data and duns_and_NAICS

FPDS_data_w_topNAICS <- join(FPDS_data, duns_and_NAICS, by = "Dunsnumber", type = "left", match = "all")

##check number of NAs again
sum(is.na(FPDS_data_w_topNAICS$topNAICS)) ##15688 --> 7 percent of the observations


##*****************************************##
############Choose small or large depending#################
#on lgst dollar amount
#*******************************************#

##Small = small or non-small depending on which category has the highest
#amount of obligations

##count number of NAs in biz_size
sum(is.na(FPDS_data_w_topNAICS$biz_size)) ##12 --> .00 percent of the observations

##create subsetted data with only FY, duns, obligated amount, and naics
names(FPDS_data)
Duns_smallbiz <- data.frame( FPDS_data$FYear, FPDS_data$Dunsnumber, FPDS_data$obligatedAmount, FPDS_data$biz_size)

##count how many rows each Dunsnumber has
names(Duns_smallbiz)
unique_duns_fpdsclean <- data.frame(table(Duns_smallbiz$FPDS_data.Dunsnumber)) ##gives a data frame with a list of duns and the number of times they occurred

##sort by duns
names(Duns_smallbiz)
Duns_smallbiz <- Duns_smallbiz[order(Duns_smallbiz$FPDS_data.Dunsnumber, Duns_smallbiz$FPDS_data.FYear, Duns_smallbiz$FPDS_data.biz_size), ]


##step 1: use dplyr to create a new data frame grouped by DUNS nad NAICS
#and sum obligated amount for all unique combinations
DO_newvar_sb <- Duns_smallbiz %>% group_by(FPDS_data.Dunsnumber, FPDS_data.biz_size) %>%
  dplyr::summarize(obligated_amount=sum(FPDS_data.obligatedAmount, na.rm=TRUE))

#step2: eliminate the combinations that are not the highest
DO_max_newvar_sb <- DO_newvar_sb %>% group_by(FPDS_data.Dunsnumber) %>%
  dplyr::mutate(desc_rank = row_number(desc(obligated_amount))) ##row_number with ties (there were 31) it chooses the first one  

#step3: subset DO_max_newvar_sb where rank==1

duns_and_smallbiz <- subset(DO_max_newvar_sb, desc_rank==1)

#check to see if FPDS_data.Dunsnumber is a unique identifier
##check uniqueness of DUNS as a variable##

n_distinct(duns_and_smallbiz$FPDS_data.Dunsnumber) ##returns 8733 

length(unique(duns_and_smallbiz$FPDS_data.Dunsnumber)) == nrow(duns_and_smallbiz) ##TRUE

table(duns_and_smallbiz$desc_rank)

n_occur <- data.frame(table(duns_and_smallbiz$FPDS_data.Dunsnumber)) ##gives a data frame with a list of duns and the number of times they occurred

duplicates_in_dunsandsmallbiz <- n_occur[n_occur$Freq >1, ] ##tells me which duns occur more than once and their frequency

##is unique identifier!

#drop obligatedamount and desc_rank
names(duns_and_smallbiz)
duns_and_smallbiz$obligated_amount <- duns_and_smallbiz$desc_rank <- NULL

#change name of duns in duns_and_NAICS
names(duns_and_smallbiz)
names(FPDS_data)

names(duns_and_smallbiz)[names(duns_and_smallbiz) == "FPDS_data.Dunsnumber"] <- "Dunsnumber"
names(duns_and_smallbiz)[names(duns_and_smallbiz) == "FPDS_data.biz_size"] <- "top_small_biz"

#step 4: left join between FPDS_data and duns_and_NAICS

FPDS_data_w_topNAICS_topSB <- join(FPDS_data_w_topNAICS, duns_and_smallbiz, by = "Dunsnumber", type = "left", match = "all")

##check number of NAs again
sum(is.na(FPDS_data_w_topNAICS_topSB$topNAICS)) ##15688 --> 7 percent of the observations
sum(is.na(FPDS_data_w_topNAICS_topSB$top_small_biz)) ##0 

##*****************************************##
############create the total obligated amount#################
#associated with each DUNS number over the entire time period
#*******************************************#

##count number of NAs in obligated amount
sum(is.na(FPDS_data_w_topNAICS_topSB$obligatedAmount)) ##0 --> .00 percent of the observations

##create subsetted data with only FY, duns, obligated amount
names(FPDS_data)
Duns_total_obligatedamount <- data.frame( FPDS_data$FYear, FPDS_data$Dunsnumber, FPDS_data$obligatedAmount)

##count how many rows each Dunsnumber has
names(Duns_total_obligatedamount)
unique_duns_fpdsclean <- data.frame(table(Duns_total_obligatedamount$FPDS_data.Dunsnumber)) ##gives a data frame with a list of duns and the number of times they occurred

##sort by duns
names(Duns_total_obligatedamount)
Duns_total_obligatedamount <- Duns_total_obligatedamount[order(Duns_total_obligatedamount$FPDS_data.Dunsnumber, Duns_total_obligatedamount$FPDS_data.FYear), ]


##step 1: use dplyr to create a new data frame grouped by DUNS and obligated amount
#and sum obligated amount for all unique combinations
DO_newvar_totalobligations <- Duns_total_obligatedamount %>% group_by(FPDS_data.Dunsnumber) %>%
  dplyr::summarize(total_obligations=sum(FPDS_data.obligatedAmount, na.rm=TRUE))

#check to see if FPDS_data.Dunsnumber is a unique identifier
##check uniqueness of DUNS as a variable##

n_distinct(DO_newvar_totalobligations$FPDS_data.Dunsnumber) ##returns 8733 

length(unique(DO_newvar_totalobligations$FPDS_data.Dunsnumber)) == nrow(DO_newvar_totalobligations) ##TRUE

##is unique identifier!

#change name of duns in duns_and_NAICS
names(DO_newvar_totalobligations)
names(FPDS_data)

names(DO_newvar_totalobligations)[names(DO_newvar_totalobligations) == "FPDS_data.Dunsnumber"] <- "Dunsnumber"

#step 4: left join between FPDS_data and duns_and_NAICS

FPDS_data_w_topNAICS_topSB_totalobl <- join(FPDS_data_w_topNAICS_topSB, DO_newvar_totalobligations, by = "Dunsnumber", type = "left", match = "all")


##*****************************************##
############create the total obligated amount#################
#associated with each DUNS number in each year
#*******************************************#

##create subsetted data with only FY, duns, obligated amount
names(FPDS_data)
Duns_FYobligations <- data.frame( FPDS_data$FYear, FPDS_data$Dunsnumber, FPDS_data$obligatedAmount)


##sort by duns and FY
names(Duns_FYobligations)
Duns_FYobligations <- Duns_FYobligations[order(Duns_FYobligations$FPDS_data.Dunsnumber, Duns_FYobligations$FPDS_data.FYear), ]


##step 1: use dplyr to create a new data frame grouped by DUNS and FY
#and sum obligated amount for all unique combinations
DO_newvar_FYobligations <- Duns_FYobligations %>% group_by(FPDS_data.FYear, FPDS_data.Dunsnumber) %>%
  dplyr::summarize(FY_obligated_amount=sum(FPDS_data.obligatedAmount, na.rm=TRUE))



#change name of duns in duns_and_NAICS
names(DO_newvar_FYobligations)
names(FPDS_data)

names(DO_newvar_FYobligations)[names(DO_newvar_FYobligations) == "FPDS_data.Dunsnumber"] <- "Dunsnumber"
names(DO_newvar_FYobligations)[names(DO_newvar_FYobligations) == "FPDS_data.FYear"] <- "FYear"


#step 4: left join between FPDS_data and duns_and_NAICS

FPDS_data_w_topNAICS_topSB_totalobl_FYobl <- join(FPDS_data_w_topNAICS_topSB_totalobl, DO_newvar_FYobligations, by = c("FYear", "Dunsnumber"), type = "left", match = "all")


##*****************************************##
############create the total # of actions#################
#associated with each DUNS number over the entire time period
#*******************************************#

##count number of NAs in number of actions
sum(is.na(FPDS_data$numberOfActions)) ##222

##create subsetted data with only FY, duns, number of actions
names(FPDS_data)
Duns_total_actions <- data.frame(FPDS_data$FYear, FPDS_data$Dunsnumber, FPDS_data$numberOfActions)


sum(is.na(Duns_total_actions$FPDS_data.numberOfActions)) ##222

##sort by duns
names(Duns_total_actions)
Duns_total_actions <- Duns_total_actions[order(Duns_total_actions$FPDS_data.Dunsnumber, Duns_total_actions$FPDS_data.FYear), ]


##step 1: use dplyr to create a new data frame grouped by DUNS and obligated amount
#and sum number of actions for all unique combinations
DO_newvar_numberactions <- Duns_total_actions %>% group_by(FPDS_data.Dunsnumber) %>%
  dplyr::summarize(total_actions=sum(FPDS_data.numberOfActions, na.rm=TRUE))

#check to see if FPDS_data.Dunsnumber is a unique identifier
##check uniqueness of DUNS as a variable##

n_distinct(DO_newvar_numberactions$FPDS_data.Dunsnumber) ##returns 8733 

length(unique(DO_newvar_numberactions$FPDS_data.Dunsnumber)) == nrow(DO_newvar_numberactions) ##TRUE

##is unique identifier!

#change name of duns in DO_newvar_numberactions
names(DO_newvar_totalobligations)
names(FPDS_data)

names(DO_newvar_numberactions)[names(DO_newvar_numberactions) == "FPDS_data.Dunsnumber"] <- "Dunsnumber"

#step 4: left join between FPDS_data and duns_and_NAICS

FPDS_data_w_topNAICS_topSB_totalobl_FYobl_totact <- join(FPDS_data_w_topNAICS_topSB_totalobl_FYobl, DO_newvar_numberactions, by = "Dunsnumber", type = "left", match = "all")


##*****************************************##
############Create the total # of actions#################
#associated with each DUNS number in each year
#*******************************************#

##create subsetted data with only FY, duns, totalactions
names(FPDS_data)
Duns_FYactions <- data.frame( FPDS_data$FYear, FPDS_data$Dunsnumber, FPDS_data$numberOfActions)


##sort by duns and FY
names(Duns_FYactions)
Duns_FYactions <- Duns_FYactions[order(Duns_FYactions$FPDS_data.Dunsnumber, Duns_FYactions$FPDS_data.FYear), ]


##step 1: use dplyr to create a new data frame grouped by DUNS and FY
#and sum obligated amount for all unique combinations
DO_newvar_FYactions <- Duns_FYactions %>% group_by(FPDS_data.FYear, FPDS_data.Dunsnumber) %>%
  dplyr::summarize(FY_numberofactions=sum(FPDS_data.numberOfActions, na.rm=TRUE))



#change name of duns in duns_and_NAICS
names(DO_newvar_FYactions)
names(FPDS_data)

names(DO_newvar_FYactions)[names(DO_newvar_FYactions) == "FPDS_data.Dunsnumber"] <- "Dunsnumber"
names(DO_newvar_FYactions)[names(DO_newvar_FYactions) == "FPDS_data.FYear"] <- "FYear"


#step 4: left join between FPDS_data and duns_and_NAICS

FPDS_data_w_topNAICS_topSB_totalobl_FYobl_totact_FYact <- join(FPDS_data_w_topNAICS_topSB_totalobl_FYobl_totact, DO_newvar_FYactions, by = c("FYear", "Dunsnumber"), type = "left", match = "all")

#*************************************************
##*****************************************##
############Choose max sign date #################
#for each duns number
#*******************************************#

##create subsetted data with only FY, duns, obligated amount, and AnnualMaxOfSignedDate
Duns_maxsigndate <- data.frame(FPDS_data$FYear, FPDS_data$Dunsnumber, FPDS_data$obligatedAmount, FPDS_data$AnnualMaxOfSignedDate)

names(Duns_maxsigndate)[names(Duns_maxsigndate) == "FPDS_data.Dunsnumber"] <- "Dunsnumber"
names(Duns_maxsigndate)[names(Duns_maxsigndate) == "FPDS_data.FYear"] <- "FYear"
names(Duns_maxsigndate)[names(Duns_maxsigndate) == "FPDS_data.obligatedAmount"] <- "obligated_amount"
names(Duns_maxsigndate)[names(Duns_maxsigndate) == "FPDS_data.AnnualMaxOfSignedDate"] <- "AnnualMaxSignedDate"


##sort by duns
names(Duns_maxsigndate)
Duns_maxsigndate <- Duns_maxsigndate[order(Duns_maxsigndate$Dunsnumber, Duns_maxsigndate$FYear, Duns_maxsigndate$AnnualMaxSignedDate), ]


##create a variable that ranks max signed date, 1 being the highest signed date
DO_newvar_maxsigndate <- Duns_maxsigndate %>% group_by(Dunsnumber) %>%
  dplyr::mutate(desc_rank = row_number(desc(AnnualMaxSignedDate))) 

##subset DO_max_newvar where rank==1

duns_and_maxsigndate <- subset(DO_newvar_maxsigndate, desc_rank==1)

#check to see if FPDS_data.Dunsnumber is a unique identifier
##check uniqueness of DUNS as a variable##

n_distinct(duns_and_maxsigndate$Dunsnumber) ##returns 8733 when should return 8764

length(unique(duns_and_maxsigndate$Dunsnumber)) == nrow(duns_and_maxsigndate) ##TRUE

#drop obligatedamount and desc_rank

duns_and_maxsigndate$obligated_amount <- duns_and_maxsigndate$desc_rank <- NULL

#change name of 
names(duns_and_maxsigndate)[names(duns_and_maxsigndate) == "AnnualMaxSignedDate"] <- "obsv_period_maxsigndate"

#step 4: left join between FPDS_data and duns_and_NAICS

FPDS_data_w_topNAICS_topSB_totalobl_FYobl_totact_FYact_maxSD <- join(FPDS_data_w_topNAICS_topSB_totalobl_FYobl_totact_FYact, duns_and_maxsigndate, by = c("Dunsnumber"), type = "left", match = "all")


##********************************##
##****Save File*********##
##********************************##

save(FPDS_data_w_topNAICS_topSB_totalobl_FYobl_totact_FYact_maxSD, file="FPDS_datapull_all.Rda")

#******************************************************



#*****************************************************************
##*****************************************##
############Combine SAM and FPDS data#################
#
#*******************************************#

setwd("K:/2018-01 NPS New Entrants/Data/Data/Cleaning data/Get files")

load(file = "SAM_datapull_all.Rda")

SAM_all <- data.frame(datapull_all)

setwd("K:/2018-01 NPS New Entrants/Data/Data/Cleaning data/FPDS")

load(file = "FPDS_datapull_all.Rda")

FPDS_all <- data.frame(FPDS_data_w_topNAICS_topSB_totalobl_FYobl_totact_FYact_maxSD)


setwd("K:/2018-01 NPS New Entrants/Data/Data/Cleaning data/SAM and FPDS Combined")

##Make a list of unique duns from the FPDS data

FPDS_uniqueduns <- FPDS_data_w_topNAICS_topSB_totalobl_FYobl_totact_FYact_maxSD[!duplicated(FPDS_data_w_topNAICS_topSB_totalobl_FYobl_totact_FYact_maxSD$Dunsnumber), ]

length(unique(FPDS_uniqueduns$Dunsnumber)) == nrow(FPDS_uniqueduns)

FPDS_all <- data.frame(FPDS_data_w_topNAICS_topSB_totalobl_FYobl_totact_FYact_maxSD)

##make a dataframe of only FPDS duns

FPDS_Dunsnumber_only <- c(FPDS_uniqueduns$Dunsnumber)

FPDS_Dunsnumber_only <- data.frame(FPDS_Dunsnumber_only)

names(FPDS_Dunsnumber_only)[names(FPDS_Dunsnumber_only) == "FPDS_Dunsnumber_only"] <- "Duns"


##make a dataframe of only SAM duns

SAM_Dunsnumber_only <- c(datapull_all$duns)

SAM_Dunsnumber_only <- data.frame(SAM_Dunsnumber_only)

names(SAM_Dunsnumber_only)[names(SAM_Dunsnumber_only) == "SAM_Dunsnumber_only"] <- "Duns"


#******************************###
####Remove duplicates grouped  by year and duns in FPDS####
#****************************************

#***************#
####FPDS 2000####
#***************#

FPDS_2000 <- FPDS_all[which(FPDS_all$FYear == 2000), names(FPDS_all)]

##drop duplicate duns



#***************#
####FPDS 2001####
#***************#


FPDS_2001 <- FPDS_all[which(FPDS_all$FYear == 2001), names(FPDS_all)]


#***************#
####FPDS 2002####
#***************#

FPDS_2002 <- FPDS_all[which(FPDS_all$FYear == 2002), names(FPDS_all)]


#***************#
####FPDS 2003####
#***************#

FPDS_2003 <- FPDS_all[which(FPDS_all$FYear == 2003), names(FPDS_all)]


#***************#
####FPDS 2004####
#***************#

FPDS_2004 <- FPDS_all[which(FPDS_all$FYear == 2004), names(FPDS_all)]


#***************#
####FPDS 2005####
#***************#

FPDS_2005 <- FPDS_all[which(FPDS_all$FYear == 2005), names(FPDS_all)]


#***************#
####FPDS 2006####
#***************#

FPDS_2006 <- FPDS_all[which(FPDS_all$FYear == 2006), names(FPDS_all)]


#***************#
####FPDS 2007####
#***************#

FPDS_2007 <- FPDS_all[which(FPDS_all$FYear == 2007), names(FPDS_all)]


#***************#
####FPDS 2008####
#***************#

FPDS_2008 <- FPDS_all[which(FPDS_all$FYear == 2008), names(FPDS_all)]


#***************#
####FPDS 2009####
#***************#

FPDS_2009 <- FPDS_all[which(FPDS_all$FYear == 2009), names(FPDS_all)]


#***************#
####FPDS 2010####
#***************#


FPDS_2010 <- FPDS_all[which(FPDS_all$FYear == 2010), names(FPDS_all)]


#***************#
####FPDS 2011####
#***************#

FPDS_2011 <- FPDS_all[which(FPDS_all$FYear == 2011), names(FPDS_all)]



#***************#
####FPDS 2012####
#***************#

FPDS_2012 <- FPDS_all[which(FPDS_all$FYear == 2012), names(FPDS_all)]


#***************#
####FPDS 2013####
#***************#

FPDS_2013 <- FPDS_all[which(FPDS_all$FYear == 2013), names(FPDS_all)]


#***************#
####FPDS 2014####
#***************#

FPDS_2014 <- FPDS_all[which(FPDS_all$FYear == 2014), names(FPDS_all)]


#***************#
####FPDS 2015####
#***************#

FPDS_2015 <- FPDS_all[which(FPDS_all$FYear == 2015), names(FPDS_all)]


#***************#
####FPDS 2016####
#***************#

FPDS_2016 <- FPDS_all[which(FPDS_all$FYear == 2016), names(FPDS_all)]


#***************#
####FPDS 2017####
#***************#

FPDS_2017 <- FPDS_all[which(FPDS_all$FYear == 2017), names(FPDS_all)]















##make a dataframe of only FPDS duns

FPDS_Dunsnumber_only <- c(FPDS_uniqueduns$Dunsnumber)

FPDS_Dunsnumber_only <- data.frame(FPDS_Dunsnumber_only)

names(FPDS_Dunsnumber_only)[names(FPDS_Dunsnumber_only) == "FPDS_Dunsnumber_only"] <- "Duns"


##make a dataframe of only SAM duns

SAM_Dunsnumber_only <- c(datapull_all$duns)

SAM_Dunsnumber_only <- data.frame(SAM_Dunsnumber_only)

names(SAM_Dunsnumber_only)[names(SAM_Dunsnumber_only) == "SAM_Dunsnumber_only"] <- "Duns"


#******************************###
####Remove duplicates grouped by Duns in FPDS####
#****************************************      

FPDS_unique <- FPDS_all[!duplicated(FPDS_all$Dunsnumber), ]

#*************************************
####Merge SAM and FPDS unique duns####
#************************************
names(FPDS_unique)
names(SAM_all)

##change SAM_all field name for duns

names(SAM_all)[names(SAM_all) == "duns"] <- "Dunsnumber"

names(SAM_all)

##merge FPDS_unique and SAM_all

SAM_and_FPDS_uniqueDuns <- join(FPDS_unique, SAM_all, by = c("Dunsnumber"), type = "left", match = "all")

#*************************
##make obsper_maxsigndate_yr
#***************************

##as date
SAM_and_FPDS_uniqueDuns$obsv_period_maxsigndate<-as.Date(as.character(SAM_and_FPDS_uniqueDuns$obsv_period_maxsigndate))

str(SAM_and_FPDS_uniqueDuns$obsv_period_maxsigndate)

##create obsv_period_maxsigneddate year
SAM_and_FPDS_uniqueDuns <- SAM_and_FPDS_uniqueDuns %>%
  dplyr::mutate(obsvper_signdate_year = (format(SAM_and_FPDS_uniqueDuns$obsv_period_maxsigndate, "%Y")))

str(SAM_and_FPDS_uniqueDuns$obsvper_signdate_year) ##it's a character

#make obsv_periodmaxsigndateyear numeric
SAM_and_FPDS_uniqueDuns$obsvper_signdate_year<-as.numeric(as.character(SAM_and_FPDS_uniqueDuns$obsvper_signdate_year))

str(SAM_and_FPDS_uniqueDuns$obsvper_signdate_year)

#*************************
##save!
#***************************

save(SAM_and_FPDS_uniqueDuns, file="SAM_and_FPDS_uniqueDuns.Rda")




#**************************************************************************

#*************************************************************************
#******************************###
####Remove duplicates grouped  by year and duns in FPDS####
#****************************************


#***************#
####FPDS 2000####
#***************#

FPDS_2000 <- FPDS_all[which(FPDS_all$FYear == 2000), names(FPDS_all)]

##count duplicate duns

n_occur <- data.frame(table(FPDS_2000$Dunsnumber)) ##gives a data frame with a list of duns and the number of times they occurred

n_occur_count <- data.frame(n_occur[n_occur$Freq >1, ]) ##tells me which duns occur more than once and their frequency


##drop duplicate duns

FPDS_2000_unique <- FPDS_2000[!duplicated(FPDS_2000$Dunsnumber), ]


#***************#
####FPDS 2001####
#***************#


FPDS_2001 <- FPDS_all[which(FPDS_all$FYear == 2001), names(FPDS_all)]

##count duplicate duns

n_occur <- data.frame(table(FPDS_2001$Dunsnumber)) ##gives a data frame with a list of duns and the number of times they occurred

n_occur_count <- data.frame(n_occur[n_occur$Freq >1, ]) ##tells me which duns occur more than once and their frequency


##drop duplicate duns

FPDS_2001_unique <- FPDS_2001[!duplicated(FPDS_2001$Dunsnumber), ]


#***************#
####FPDS 2002####
#***************#

FPDS_2002 <- FPDS_all[which(FPDS_all$FYear == 2002), names(FPDS_all)]

##count duplicate duns

n_occur <- data.frame(table(FPDS_2002$Dunsnumber)) ##gives a data frame with a list of duns and the number of times they occurred

n_occur_count <- data.frame(n_occur[n_occur$Freq >1, ]) ##tells me which duns occur more than once and their frequency


##drop duplicate duns

FPDS_2002_unique <- FPDS_2002[!duplicated(FPDS_2002$Dunsnumber), ]


#***************#
####FPDS 2003####
#***************#

FPDS_2003 <- FPDS_all[which(FPDS_all$FYear == 2003), names(FPDS_all)]

##count duplicate duns

n_occur <- data.frame(table(FPDS_2003$Dunsnumber)) ##gives a data frame with a list of duns and the number of times they occurred

n_occur_count <- data.frame(n_occur[n_occur$Freq >1, ]) ##tells me which duns occur more than once and their frequency


##drop duplicate duns

FPDS_2003_unique <- FPDS_2003[!duplicated(FPDS_2003$Dunsnumber), ]



#***************#
####FPDS 2004####
#***************#

FPDS_2004 <- FPDS_all[which(FPDS_all$FYear == 2004), names(FPDS_all)]


##count duplicate duns

n_occur <- data.frame(table(FPDS_2004$Dunsnumber)) ##gives a data frame with a list of duns and the number of times they occurred

n_occur_count <- data.frame(n_occur[n_occur$Freq >1, ]) ##tells me which duns occur more than once and their frequency


##drop duplicate duns

FPDS_2004_unique <- FPDS_2004[!duplicated(FPDS_2004$Dunsnumber), ]



#***************#
####FPDS 2005####
#***************#

FPDS_2005 <- FPDS_all[which(FPDS_all$FYear == 2005), names(FPDS_all)]
##count duplicate duns

n_occur <- data.frame(table(FPDS_2005$Dunsnumber)) ##gives a data frame with a list of duns and the number of times they occurred

n_occur_count <- data.frame(n_occur[n_occur$Freq >1, ]) ##tells me which duns occur more than once and their frequency


##drop duplicate duns

FPDS_2005_unique <- FPDS_2005[!duplicated(FPDS_2005$Dunsnumber), ]



#***************#
####FPDS 2006####
#***************#

FPDS_2006 <- FPDS_all[which(FPDS_all$FYear == 2006), names(FPDS_all)]

##count duplicate duns

n_occur <- data.frame(table(FPDS_2006$Dunsnumber)) ##gives a data frame with a list of duns and the number of times they occurred

n_occur_count <- data.frame(n_occur[n_occur$Freq >1, ]) ##tells me which duns occur more than once and their frequency


##drop duplicate duns

FPDS_2006_unique <- FPDS_2006[!duplicated(FPDS_2006$Dunsnumber), ]


#***************#
####FPDS 2007####
#***************#

FPDS_2007 <- FPDS_all[which(FPDS_all$FYear == 2007), names(FPDS_all)]
##count duplicate duns

n_occur <- data.frame(table(FPDS_2007$Dunsnumber)) ##gives a data frame with a list of duns and the number of times they occurred

n_occur_count <- data.frame(n_occur[n_occur$Freq >1, ]) ##tells me which duns occur more than once and their frequency


##drop duplicate duns

FPDS_2007_unique <- FPDS_2007[!duplicated(FPDS_2007$Dunsnumber), ]



#***************#
####FPDS 2008####
#***************#

FPDS_2008 <- FPDS_all[which(FPDS_all$FYear == 2008), names(FPDS_all)]
##count duplicate duns

n_occur <- data.frame(table(FPDS_2008$Dunsnumber)) ##gives a data frame with a list of duns and the number of times they occurred

n_occur_count <- data.frame(n_occur[n_occur$Freq >1, ]) ##tells me which duns occur more than once and their frequency


##drop duplicate duns

FPDS_2008_unique <- FPDS_2008[!duplicated(FPDS_2008$Dunsnumber), ]



#***************#
####FPDS 2009####
#***************#

FPDS_2009 <- FPDS_all[which(FPDS_all$FYear == 2009), names(FPDS_all)]
##count duplicate duns

n_occur <- data.frame(table(FPDS_2009$Dunsnumber)) ##gives a data frame with a list of duns and the number of times they occurred

n_occur_count <- data.frame(n_occur[n_occur$Freq >1, ]) ##tells me which duns occur more than once and their frequency


##drop duplicate duns

FPDS_2009_unique <- FPDS_2009[!duplicated(FPDS_2009$Dunsnumber), ]



#***************#
####FPDS 2010####
#***************#


FPDS_2010 <- FPDS_all[which(FPDS_all$FYear == 2010), names(FPDS_all)]
##count duplicate duns

n_occur <- data.frame(table(FPDS_2010$Dunsnumber)) ##gives a data frame with a list of duns and the number of times they occurred

n_occur_count <- data.frame(n_occur[n_occur$Freq >1, ]) ##tells me which duns occur more than once and their frequency


##drop duplicate duns

FPDS_2010_unique <- FPDS_2010[!duplicated(FPDS_2010$Dunsnumber), ]



#***************#
####FPDS 2011####
#***************#

FPDS_2011 <- FPDS_all[which(FPDS_all$FYear == 2011), names(FPDS_all)]
##count duplicate duns

n_occur <- data.frame(table(FPDS_2011$Dunsnumber)) ##gives a data frame with a list of duns and the number of times they occurred

n_occur_count <- data.frame(n_occur[n_occur$Freq >1, ]) ##tells me which duns occur more than once and their frequency


##drop duplicate duns

FPDS_2011_unique <- FPDS_2011[!duplicated(FPDS_2011$Dunsnumber), ]




#***************#
####FPDS 2012####
#***************#

FPDS_2012 <- FPDS_all[which(FPDS_all$FYear == 2012), names(FPDS_all)]
##count duplicate duns

n_occur <- data.frame(table(FPDS_2012$Dunsnumber)) ##gives a data frame with a list of duns and the number of times they occurred

n_occur_count <- data.frame(n_occur[n_occur$Freq >1, ]) ##tells me which duns occur more than once and their frequency


##drop duplicate duns

FPDS_2012_unique <- FPDS_2012[!duplicated(FPDS_2012$Dunsnumber), ]



#***************#
####FPDS 2013####
#***************#

FPDS_2013 <- FPDS_all[which(FPDS_all$FYear == 2013), names(FPDS_all)]
##count duplicate duns

n_occur <- data.frame(table(FPDS_2013$Dunsnumber)) ##gives a data frame with a list of duns and the number of times they occurred

n_occur_count <- data.frame(n_occur[n_occur$Freq >1, ]) ##tells me which duns occur more than once and their frequency


##drop duplicate duns

FPDS_2013_unique <- FPDS_2013[!duplicated(FPDS_2013$Dunsnumber), ]



#***************#
####FPDS 2014####
#***************#

FPDS_2014 <- FPDS_all[which(FPDS_all$FYear == 2014), names(FPDS_all)]
##count duplicate duns

n_occur <- data.frame(table(FPDS_2014$Dunsnumber)) ##gives a data frame with a list of duns and the number of times they occurred

n_occur_count <- data.frame(n_occur[n_occur$Freq >1, ]) ##tells me which duns occur more than once and their frequency


##drop duplicate duns

FPDS_2014_unique <- FPDS_2014[!duplicated(FPDS_2014$Dunsnumber), ]



#***************#
####FPDS 2015####
#***************#

FPDS_2015 <- FPDS_all[which(FPDS_all$FYear == 2015), names(FPDS_all)]
##count duplicate duns

n_occur <- data.frame(table(FPDS_2015$Dunsnumber)) ##gives a data frame with a list of duns and the number of times they occurred

n_occur_count <- data.frame(n_occur[n_occur$Freq >1, ]) ##tells me which duns occur more than once and their frequency


##drop duplicate duns

FPDS_2015_unique <- FPDS_2015[!duplicated(FPDS_2015$Dunsnumber), ]



#***************#
####FPDS 2016####
#***************#

FPDS_2016 <- FPDS_all[which(FPDS_all$FYear == 2016), names(FPDS_all)]
##count duplicate duns

n_occur <- data.frame(table(FPDS_2016$Dunsnumber)) ##gives a data frame with a list of duns and the number of times they occurred

n_occur_count <- data.frame(n_occur[n_occur$Freq >1, ]) ##tells me which duns occur more than once and their frequency


##drop duplicate duns

FPDS_2016_unique <- FPDS_2016[!duplicated(FPDS_2016$Dunsnumber), ]



#***************#
####FPDS 2017####
#***************#

FPDS_2017 <- FPDS_all[which(FPDS_all$FYear == 2017), names(FPDS_all)]
##count duplicate duns

n_occur <- data.frame(table(FPDS_2017$Dunsnumber)) ##gives a data frame with a list of duns and the number of times they occurred

n_occur_count <- data.frame(n_occur[n_occur$Freq >1, ]) ##tells me which duns occur more than once and their frequency


##drop duplicate duns

FPDS_2017_unique <- FPDS_2017[!duplicated(FPDS_2017$Dunsnumber), ]


















