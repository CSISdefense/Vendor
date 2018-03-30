###Survival Rates ####

#install.packages("matrixStats")
library(matrixStats)
library(describer)
library(tidyverse)
#install.packages("lubridate")
#install.packages("openxlsx")
library(openxlsx)
#install.packages("httr")
library(httr)
#install.packages("jsonlite")
library(jsonlite)
library(plyr)
#install.packages("data.table")
library(data.table)
library(lubridate)


##Marielle working Directory
setwd("K:/2018-01 NPS New Entrants/Data/Data")
getwd()

##Sam working Directory
setwd("K:/2018-01 NPS New Entrants/Data/Data/Cleaned Data")
getwd()

### import data Signeddate nop filter####
sd_nop <- read_csv("Panel Data reg2001-2016 - ver 2.csv")

df2001_sd <- sd_nop %>% 
  filter(year(registrationDate) == 2001 )
df2002_sd <- sd_nop %>% 
  filter(year(registrationDate) == 2002 )
df2003_sd <- sd_nop %>% 
  filter(year(registrationDate) == 2003 )
df2004_sd <- sd_nop %>% 
  filter(year(registrationDate) == 2004 )
df2005_sd <- sd_nop %>% 
  filter(year(registrationDate) == 2005 )
df2006_sd <- sd_nop %>% 
  filter(year(registrationDate) == 2006 )


###Cleaning variables to categorical and numeric####

df2001_sd$NAICS2 = as.factor(df2001_sd$NAICS2)
df2001_sd$ServicesCategory = as.factor(df2001_sd$ServicesCategory)
df2001_sd$location = as.numeric(df2001_sd$location)
df2001_sd$ownership.woman = as.numeric(df2001_sd$ownership.woman)
df2001_sd$ownership.veteran = as.numeric(df2001_sd$ownership.veteran)
df2001_sd$ownership.minority = as.numeric(df2001_sd$ownership.minority)
df2001_sd$ownership.foreign = as.numeric(df2001_sd$ownership.foreign)
df2001_sd$survival.status = as.numeric(df2001_sd$survival.status)
df2001_sd$DEPARTMENT_NAME = as.factor(df2001_sd$DEPARTMENT_NAME)
df2001_sd$AGENCY_NAME = as.factor(df2001_sd$AGENCY_NAME)


df2002_sd$NAICS2 = as.factor(df2002_sd$NAICS2)
df2002_sd$ServicesCategory = as.factor(df2002_sd$ServicesCategory)
df2002_sd$location = as.numeric(df2002_sd$location)
df2002_sd$ownership.woman = as.numeric(df2002_sd$ownership.woman)
df2002_sd$ownership.veteran = as.numeric(df2002_sd$ownership.veteran)
df2002_sd$ownership.minority = as.numeric(df2002_sd$ownership.minority)
df2002_sd$ownership.foreign = as.numeric(df2002_sd$ownership.foreign)
df2002_sd$survival.status = as.numeric(df2002_sd$survival.status)
df2002_sd$DEPARTMENT_NAME = as.factor(df2002_sd$DEPARTMENT_NAME)
df2002_sd$AGENCY_NAME = as.factor(df2002_sd$AGENCY_NAME)

df2003_sd$NAICS2 = as.factor(df2003_sd$NAICS2)
df2003_sd$ServicesCategory = as.factor(df2003_sd$ServicesCategory)
df2003_sd$location = as.numeric(df2003_sd$location)
df2003_sd$ownership.woman = as.numeric(df2003_sd$ownership.woman)
df2003_sd$ownership.veteran = as.numeric(df2003_sd$ownership.veteran)
df2003_sd$ownership.minority = as.numeric(df2003_sd$ownership.minority)
df2003_sd$ownership.foreign = as.numeric(df2003_sd$ownership.foreign)
df2003_sd$survival.status = as.numeric(df2003_sd$survival.status)
df2003_sd$DEPARTMENT_NAME = as.factor(df2003_sd$DEPARTMENT_NAME)
df2003_sd$AGENCY_NAME = as.factor(df2003_sd$AGENCY_NAME)

df2004_sd$NAICS2 = as.factor(df2004_sd$NAICS2)
df2004_sd$ServicesCategory = as.factor(df2004_sd$ServicesCategory)
df2004_sd$location = as.numeric(df2004_sd$location)
df2004_sd$ownership.woman = as.numeric(df2004_sd$ownership.woman)
df2004_sd$ownership.veteran = as.numeric(df2004_sd$ownership.veteran)
df2004_sd$ownership.minority = as.numeric(df2004_sd$ownership.minority)
df2004_sd$ownership.foreign = as.numeric(df2004_sd$ownership.foreign)
df2004_sd$survival.status = as.numeric(df2004_sd$survival.status)
df2004_sd$DEPARTMENT_NAME = as.factor(df2004_sd$DEPARTMENT_NAME)
df2004_sd$AGENCY_NAME = as.factor(df2004_sd$AGENCY_NAME)

df2005_sd$NAICS2 = as.factor(df2005_sd$NAICS2)
df2005_sd$ServicesCategory = as.factor(df2005_sd$ServicesCategory)
df2005_sd$location = as.numeric(df2005_sd$location)
df2005_sd$ownership.woman = as.numeric(df2005_sd$ownership.woman)
df2005_sd$ownership.veteran = as.numeric(df2005_sd$ownership.veteran)
df2005_sd$ownership.minority = as.numeric(df2005_sd$ownership.minority)
df2005_sd$ownership.foreign = as.numeric(df2005_sd$ownership.foreign)
df2005_sd$survival.status = as.numeric(df2005_sd$survival.status)
df2005_sd$DEPARTMENT_NAME = as.factor(df2005_sd$DEPARTMENT_NAME)
df2005_sd$AGENCY_NAME = as.factor(df2005_sd$AGENCY_NAME)

df2006_sd$NAICS2 = as.factor(df2006_sd$NAICS2)
df2006_sd$ServicesCategory = as.factor(df2006_sd$ServicesCategory)
df2006_sd$location = as.numeric(df2006_sd$location)
df2006_sd$ownership.woman = as.numeric(df2006_sd$ownership.woman)
df2006_sd$ownership.veteran = as.numeric(df2006_sd$ownership.veteran)
df2006_sd$ownership.minority = as.numeric(df2006_sd$ownership.minority)
df2006_sd$ownership.foreign = as.numeric(df2006_sd$ownership.foreign)
df2006_sd$survival.status = as.numeric(df2006_sd$survival.status)
df2006_sd$DEPARTMENT_NAME = as.factor(df2006_sd$DEPARTMENT_NAME)
df2006_sd$AGENCY_NAME = as.factor(df2006_sd$AGENCY_NAME)





### DOD filter####
dod_sd <- read_csv("Panel Data reg2001-2016 DOD - ver2.csv")

df2001_DOD <- dod_sd %>% 
  filter(year(registrationDate) == 2001 )
df2002_DOD <- dod_sd %>% 
  filter(year(registrationDate) == 2002 )
df2003_DOD <- dod_sd %>% 
  filter(year(registrationDate) == 2003 )
df2004_DOD <- dod_sd %>% 
  filter(year(registrationDate) == 2004 )
df2005_DOD <- dod_sd %>% 
  filter(year(registrationDate) == 2005 )
df2006_DOD <- dod_sd %>% 
  filter(year(registrationDate) == 2006 )

###Cleaning variables to categorical and numeric####
df2001_DOD$NAICS2 = as.factor(df2001_DOD$NAICS2)
df2001_DOD$ServicesCategory = as.factor(df2001_DOD$ServicesCategory)
df2001_DOD$location = as.numeric(df2001_DOD$location)
df2001_DOD$ownership.woman = as.numeric(df2001_DOD$ownership.woman)
df2001_DOD$ownership.veteran = as.numeric(df2001_DOD$ownership.veteran)
df2001_DOD$ownership.minority = as.numeric(df2001_DOD$ownership.minority)
df2001_DOD$ownership.foreign = as.numeric(df2001_DOD$ownership.foreign)
df2001_DOD$survival.status = as.numeric(df2001_DOD$survival.status)
df2001_DOD$DEPARTMENT_NAME = as.factor(df2001_DOD$DEPARTMENT_NAME)
df2001_DOD$AGENCY_NAME = as.factor(df2001_DOD$AGENCY_NAME)

df2002_DOD$NAICS2 = as.factor(df2002_DOD$NAICS2)
df2002_DOD$ServicesCategory = as.factor(df2002_DOD$ServicesCategory)
df2002_DOD$location = as.numeric(df2002_DOD$location)
df2002_DOD$ownership.woman = as.numeric(df2002_DOD$ownership.woman)
df2002_DOD$ownership.veteran = as.numeric(df2002_DOD$ownership.veteran)
df2002_DOD$ownership.minority = as.numeric(df2002_DOD$ownership.minority)
df2002_DOD$ownership.foreign = as.numeric(df2002_DOD$ownership.foreign)
df2002_DOD$survival.status = as.numeric(df2002_DOD$survival.status)
df2002_DOD$DEPARTMENT_NAME = as.factor(df2002_DOD$DEPARTMENT_NAME)
df2002_DOD$AGENCY_NAME = as.factor(df2002_DOD$AGENCY_NAME)

df2003_DOD$NAICS2 = as.factor(df2003_DOD$NAICS2)
df2003_DOD$ServicesCategory = as.factor(df2003_DOD$ServicesCategory)
df2003_DOD$location = as.numeric(df2003_DOD$location)
df2003_DOD$ownership.woman = as.numeric(df2003_DOD$ownership.woman)
df2003_DOD$ownership.veteran = as.numeric(df2003_DOD$ownership.veteran)
df2003_DOD$ownership.minority = as.numeric(df2003_DOD$ownership.minority)
df2003_DOD$ownership.foreign = as.numeric(df2003_DOD$ownership.foreign)
df2003_DOD$survival.status = as.numeric(df2003_DOD$survival.status)
df2003_DOD$DEPARTMENT_NAME = as.factor(df2003_DOD$DEPARTMENT_NAME)
df2003_DOD$AGENCY_NAME = as.factor(df2003_DOD$AGENCY_NAME)

df2004_DOD$NAICS2 = as.factor(df2004_DOD$NAICS2)
df2004_DOD$ServicesCategory = as.factor(df2004_DOD$ServicesCategory)
df2004_DOD$location = as.numeric(df2004_DOD$location)
df2004_DOD$ownership.woman = as.numeric(df2004_DOD$ownership.woman)
df2004_DOD$ownership.veteran = as.numeric(df2004_DOD$ownership.veteran)
df2004_DOD$ownership.minority = as.numeric(df2004_DOD$ownership.minority)
df2004_DOD$ownership.foreign = as.numeric(df2004_DOD$ownership.foreign)
df2004_DOD$survival.status = as.numeric(df2004_DOD$survival.status)
df2004_DOD$DEPARTMENT_NAME = as.factor(df2004_DOD$DEPARTMENT_NAME)
df2004_DOD$AGENCY_NAME = as.factor(df2004_DOD$AGENCY_NAME)

df2005_DOD$NAICS2 = as.factor(df2005_DOD$NAICS2)
df2005_DOD$ServicesCategory = as.factor(df2005_DOD$ServicesCategory)
df2005_DOD$location = as.numeric(df2005_DOD$location)
df2005_DOD$ownership.woman = as.numeric(df2005_DOD$ownership.woman)
df2005_DOD$ownership.veteran = as.numeric(df2005_DOD$ownership.veteran)
df2005_DOD$ownership.minority = as.numeric(df2005_DOD$ownership.minority)
df2005_DOD$ownership.foreign = as.numeric(df2005_DOD$ownership.foreign)
df2005_DOD$survival.status = as.numeric(df2005_DOD$survival.status)
df2005_DOD$DEPARTMENT_NAME = as.factor(df2005_DOD$DEPARTMENT_NAME)
df2005_DOD$AGENCY_NAME = as.factor(df2005_DOD$AGENCY_NAME)

df2006_DOD$NAICS2 = as.factor(df2006_DOD$NAICS2)
df2006_DOD$ServicesCategory = as.factor(df2006_DOD$ServicesCategory)
df2006_DOD$location = as.numeric(df2006_DOD$location)
df2006_DOD$ownership.woman = as.numeric(df2006_DOD$ownership.woman)
df2006_DOD$ownership.veteran = as.numeric(df2006_DOD$ownership.veteran)
df2006_DOD$ownership.minority = as.numeric(df2006_DOD$ownership.minority)
df2006_DOD$ownership.foreign = as.numeric(df2006_DOD$ownership.foreign)
df2006_DOD$survival.status = as.numeric(df2006_DOD$survival.status)
df2006_DOD$DEPARTMENT_NAME = as.factor(df2006_DOD$DEPARTMENT_NAME)
df2006_DOD$AGENCY_NAME = as.factor(df2006_DOD$AGENCY_NAME)


###Descriptive Stats####

###Survival Rates all agencies 2001-2006####

##3-year
sr3yr2001_sd <- sum(df2001_sd$years.survived>=3)/nrow(df2001_sd)
sr3yr2001sm_sd <- sum((df2001_sd$years.survived>=3 & df2001_sd$biz_size == 0), na.rm = TRUE)/sum(df2001_sd$biz_size == 0, na.rm = TRUE)
sr3yr2001non_sd <- sum((df2001_sd$years.survived>=3 & df2001_sd$biz_size == 1), na.rm = TRUE)/sum(df2001_sd$biz_size == 1, na.rm = TRUE)

sr3yr2002_sd <- sum(df2002_sd$years.survived>=3)/nrow(df2002_sd) 
sr3yr2002sm_sd <- sum((df2002_sd$years.survived>=3 & df2002_sd$biz_size == 0), na.rm = TRUE)/sum(df2002_sd$biz_size == 0, na.rm = TRUE)
sr3yr2002non_sd <- sum((df2002_sd$years.survived>=3 & df2002_sd$biz_size == 1), na.rm = TRUE)/sum(df2002_sd$biz_size == 1, na.rm = TRUE)

sr3yr2003_sd <- sum(df2003_sd$years.survived>=3)/nrow(df2003_sd)
sr3yr2003sm_sd <- sum((df2003_sd$years.survived>=3 & df2003_sd$biz_size == 0), na.rm = TRUE)/sum(df2003_sd$biz_size == 0, na.rm = TRUE)
sr3yr2003non_sd <- sum((df2003_sd$years.survived>=3 & df2003_sd$biz_size == 1), na.rm = TRUE)/sum(df2003_sd$biz_size == 1, na.rm = TRUE)

sr3yr2004_sd <- sum(df2004_sd$years.survived>=3)/nrow(df2004_sd) 
sr3yr2004sm_sd <- sum((df2004_sd$years.survived>=3 & df2004_sd$biz_size == 0), na.rm = TRUE)/sum(df2004_sd$biz_size == 0, na.rm = TRUE)
sr3yr2004non_sd <- sum((df2004_sd$years.survived>=3 & df2004_sd$biz_size == 1), na.rm = TRUE)/sum(df2004_sd$biz_size == 1, na.rm = TRUE)

sr3yr2005_sd <- sum(df2005_sd$years.survived>=3)/nrow(df2005_sd) 
sr3yr2005sm_sd <- sum((df2005_sd$years.survived>=3 & df2005_sd$biz_size == 0), na.rm = TRUE)/sum(df2005_sd$biz_size == 0, na.rm = TRUE)
sr3yr2005non_sd <- sum((df2005_sd$years.survived>=3 & df2005_sd$biz_size == 1), na.rm = TRUE)/sum(df2005_sd$biz_size == 1, na.rm = TRUE)

sr3yr2006_sd <- sum(df2006_sd$years.survived>=3)/nrow(df2006_sd) 
sr3yr2006sm_sd <- sum((df2006_sd$years.survived>=3 & df2006_sd$biz_size == 0), na.rm = TRUE)/sum(df2006_sd$biz_size == 0, na.rm = TRUE)
sr3yr2006non_sd <- sum((df2006_sd$years.survived>=3 & df2006_sd$biz_size == 1), na.rm = TRUE)/sum(df2006_sd$biz_size == 1, na.rm = TRUE)

##5-year
sr5yr2001_sd <- sum(df2001_sd$years.survived>=5)/nrow(df2001_sd)
sr5yr2001sm_sd <- sum((df2001_sd$years.survived>=5 & df2001_sd$biz_size == 0), na.rm = TRUE)/sum(df2001_sd$biz_size == 0, na.rm = TRUE)
sr5yr2001non_sd <- sum((df2001_sd$years.survived>=5 & df2001_sd$biz_size == 1), na.rm = TRUE)/sum(df2001_sd$biz_size == 1, na.rm = TRUE)

sr5yr2002_sd <- sum(df2002_sd$years.survived>=5)/nrow(df2002_sd) 
sr5yr2002sm_sd <- sum((df2002_sd$years.survived>=5 & df2002_sd$biz_size == 0), na.rm = TRUE)/sum(df2002_sd$biz_size == 0, na.rm = TRUE)
sr5yr2002non_sd <- sum((df2002_sd$years.survived>=5 & df2002_sd$biz_size == 1), na.rm = TRUE)/sum(df2002_sd$biz_size == 1, na.rm = TRUE)

sr5yr2003_sd <- sum(df2003_sd$years.survived>=5)/nrow(df2003_sd)
sr5yr2003sm_sd <- sum((df2003_sd$years.survived>=5 & df2003_sd$biz_size == 0), na.rm = TRUE)/sum(df2003_sd$biz_size == 0, na.rm = TRUE)
sr5yr2003non_sd <- sum((df2003_sd$years.survived>=5 & df2003_sd$biz_size == 1), na.rm = TRUE)/sum(df2003_sd$biz_size == 1, na.rm = TRUE)

sr5yr2004_sd <- sum(df2004_sd$years.survived>=5)/nrow(df2004_sd) 
sr5yr2004sm_sd <- sum((df2004_sd$years.survived>=5 & df2004_sd$biz_size == 0), na.rm = TRUE)/sum(df2004_sd$biz_size == 0, na.rm = TRUE)
sr5yr2004non_sd <- sum((df2004_sd$years.survived>=5 & df2004_sd$biz_size == 1), na.rm = TRUE)/sum(df2004_sd$biz_size == 1, na.rm = TRUE)

sr5yr2005_sd <- sum(df2005_sd$years.survived>=5)/nrow(df2005_sd) 
sr5yr2005sm_sd <- sum((df2005_sd$years.survived>=5 & df2005_sd$biz_size == 0), na.rm = TRUE)/sum(df2005_sd$biz_size == 0, na.rm = TRUE)
sr5yr2005non_sd <- sum((df2005_sd$years.survived>=5 & df2005_sd$biz_size == 1), na.rm = TRUE)/sum(df2005_sd$biz_size == 1, na.rm = TRUE)

sr5yr2006_sd <- sum(df2006_sd$years.survived>=5)/nrow(df2006_sd) 
sr5yr2006sm_sd <- sum((df2006_sd$years.survived>=5 & df2006_sd$biz_size == 0), na.rm = TRUE)/sum(df2006_sd$biz_size == 0, na.rm = TRUE)
sr5yr2006non_sd <- sum((df2006_sd$years.survived>=5 & df2006_sd$biz_size == 1), na.rm = TRUE)/sum(df2006_sd$biz_size == 1, na.rm = TRUE)


##10-year
sr10yr2001_sd <- (sum(df2001_sd$years.survived>=10))/nrow(df2001_sd) 
sr10yr2001sm_sd <- sum((df2001_sd$years.survived>=10 & df2001_sd$biz_size == 0), na.rm = TRUE)/sum(df2001_sd$biz_size == 0, na.rm = TRUE)
sr10yr2001non_sd <- sum((df2001_sd$years.survived>=10 & df2001_sd$biz_size == 1), na.rm = TRUE)/sum(df2002_sd$biz_size == 1, na.rm = TRUE)


sr10yr2002_sd <- sum(df2002_sd$years.survived>=10)/nrow(df2002_sd) 
sr10yr2002sm_sd <- sum((df2002_sd$years.survived>=10 & df2002_sd$biz_size == 0), na.rm = TRUE)/sum(df2002_sd$biz_size == 0, na.rm = TRUE)
sr10yr2002non_sd <- sum((df2002_sd$years.survived>=10 & df2002_sd$biz_size == 1), na.rm = TRUE)/sum(df2002_sd$biz_size == 1, na.rm = TRUE)

sr10yr2003_sd <- sum(df2003_sd$years.survived>=10)/nrow(df2003_sd)
sr10yr2003sm_sd <- sum((df2003_sd$years.survived>=10 & df2003_sd$biz_size == 0), na.rm = TRUE)/sum(df2003_sd$biz_size == 0, na.rm = TRUE)
sr10yr2003non_sd <- sum((df2003_sd$years.survived>=10 & df2003_sd$biz_size == 1), na.rm = TRUE)/sum(df2003_sd$biz_size == 1, na.rm = TRUE)

sr10yr2004_sd <- sum(df2004_sd$years.survived>=10)/nrow(df2004_sd) 
sr10yr2004sm_sd <- sum((df2004_sd$years.survived>=10 & df2004_sd$biz_size == 0), na.rm = TRUE)/sum(df2004_sd$biz_size == 0, na.rm = TRUE)
sr10yr2004non_sd <- sum((df2004_sd$years.survived>=10 & df2004_sd$biz_size == 1), na.rm = TRUE)/sum(df2004_sd$biz_size == 1, na.rm = TRUE)

sr10yr2005_sd <- sum(df2005_sd$years.survived>=10)/nrow(df2005_sd) 
sr10yr2005sm_sd <- sum((df2005_sd$years.survived>=10 & df2005_sd$biz_size == 0), na.rm = TRUE)/sum(df2005_sd$biz_size == 0, na.rm = TRUE)
sr10yr2005non_sd <- sum((df2005_sd$years.survived>=10 & df2005_sd$biz_size == 1), na.rm = TRUE)/sum(df2005_sd$biz_size == 1, na.rm = TRUE)

sr10yr2006_sd <- sum(df2006_sd$years.survived>=10)/nrow(df2006_sd) 
sr10yr2006sm_sd <- sum((df2006_sd$years.survived>=10 & df2006_sd$biz_size == 0), na.rm = TRUE)/sum(df2006_sd$biz_size == 0, na.rm = TRUE)
sr10yr2006non_sd <- sum((df2006_sd$years.survived>=10 & df2006_sd$biz_size == 1), na.rm = TRUE)/sum(df2006_sd$biz_size == 1, na.rm = TRUE)

##bound all survival rates together and reformated dataframe

survivalrate_sd <- cbind(sr3yr2001_sd, sr3yr2001sm_sd, sr3yr2001non_sd, sr5yr2001_sd, sr5yr2001sm_sd, sr5yr2001non_sd, 
                          sr10yr2001_sd, sr10yr2001sm_sd, sr10yr2001non_sd,
                          sr3yr2002_sd, sr3yr2002sm_sd, sr3yr2002non_sd, sr5yr2002_sd, sr5yr2002sm_sd, sr5yr2002non_sd, 
                          sr10yr2002_sd, sr10yr2002sm_sd, sr10yr2002non_sd,
                          sr3yr2003_sd, sr3yr2003sm_sd, sr3yr2003non_sd, sr5yr2003_sd, sr5yr2003sm_sd, sr5yr2003non_sd, 
                          sr10yr2003_sd, sr10yr2003sm_sd, sr10yr2003non_sd,
                          sr3yr2004_sd, sr3yr2004sm_sd, sr3yr2004non_sd, sr5yr2004_sd, sr5yr2004sm_sd, sr5yr2004non_sd, 
                          sr10yr2004_sd, sr10yr2004sm_sd, sr10yr2004non_sd,
                          sr3yr2005_sd, sr3yr2005sm_sd, sr3yr2005non_sd, sr5yr2005_sd, sr5yr2005sm_sd, sr5yr2005non_sd, 
                          sr10yr2005_sd, sr10yr2005sm_sd, sr10yr2005non_sd,
                          sr3yr2006_sd, sr3yr2006sm_sd, sr3yr2006non_sd, sr5yr2006_sd, sr5yr2006sm_sd, sr5yr2006non_sd, 
                          sr10yr2006_sd, sr10yr2006sm_sd, sr10yr2006non_sd)
srnames_sd <- c("3yr_2001_all_sd", "3yr_2001_sm_sd", 
                 "3yr_2001_non_sd", "5yr_2001_all_sd", "5yr_2001_sm_sd", "5yr_2001_non_sd", "10yr_2001_all_sd", 
                 "10yr_2001_sm_sd", "10yr_2001_non_sd",
                 "3yr_2002_all_sd", "3yr_2002_sm_sd", "3yr_2002_non_sd", "5yr_2002_all_sd", "5yr_2002_sm_sd", "5yr_2002_non_sd", 
                 "10yr_2002_all_sd", "10yr_2002_sm_sd", "10yr_2002_non_sd",
                 "3yr_2003_all_sd", "3yr_2003_sm_sd", "3yr_2003_non_sd", "5yr_2003_all_sd", "5yr_2003_sm_sd", "5yr_2003_non_sd", 
                 "10yr_2003_all_sd", "10yr_2003_sm_sd", "10yr_2003_non_sd",
                 "3yr_2004_all_sd", "3yr_2004_sm_sd", "3yr_2004_non_sd", "5yr_2004_all_sd", "5yr_2004_sm_sd", "5yr_2004_non_sd", 
                 "10yr_2004_all_sd", "10yr_2004_sm_sd", "10yr_2004_non_sd",
                 "3yr_2005_all_sd", "3yr_2005_sm_sd", "3yr_2005_non_sd", "5yr_2005_all_sd", "5yr_2005_sm_sd", "5yr_2005_non_sd", 
                 "10yr2005_all_sd", "10yr_2005_sm_sd", "10yr_2005_non_sd",
                 "3yr_2006_all_sd", "3yr_2006_sm_sd", "3yr_2006_non_sd", "5yr_2006_all_sd", "5yr_2006_sm_sd", "5yr_2006_non_sd", 
                 "10yr_2006_all_sd", "10yr_2006_sm_sd", "10yr_2006_non_sd")
colnames(survivalrate_sd) <- srnames_sd

sr_sd <- as.data.frame(survivalrate_sd)

survival.rates_sd <- sr_sd %>% 
  gather("3yr_2001_all_sd":"10yr_2006_non_sd", key = "time_year_type", value = "survivalrate_sd") %>% 
  separate(time_year_type, into = c("time", "year", "type"), sep = "_") %>% 
  arrange(time, year) %>% 
  unite("time_type", time, type, sep = "_") 

survival.rates_sd$time_type[18] <- "10yr_all"
survival.rates_sd$year[18] <- 2005

survival.rates_sd <- survival.rates_sd %>% 
  spread(key = "time_type", value = "survivalrate_sd")

survival.rates_sd[, c(1, 5, 7, 6, 8, 10, 9, 2, 4, 3)]

write.csv(survival.rates_sd, "survival rates_SD reg2001-2006.csv")


###Survival Rates with no parent filter DOD####
##3-year
sr3yr2001_DOD <- sum(df2001_DOD$years.survived>=3)/nrow(df2001_DOD)
sr3yr2001sm_DOD <- sum((df2001_DOD$years.survived>=3 & df2001_DOD$biz_size == 0), na.rm = TRUE)/sum(df2001_DOD$biz_size == 0, na.rm = TRUE)
sr3yr2001non_DOD <- sum((df2001_DOD$years.survived>=3 & df2001_DOD$biz_size == 1), na.rm = TRUE)/sum(df2001_DOD$biz_size == 1, na.rm = TRUE)

sr3yr2002_DOD <- sum(df2002_DOD$years.survived>=3)/nrow(df2002_DOD) 
sr3yr2002sm_DOD <- sum((df2002_DOD$years.survived>=3 & df2002_DOD$biz_size == 0), na.rm = TRUE)/sum(df2002_DOD$biz_size == 0, na.rm = TRUE)
sr3yr2002non_DOD <- sum((df2002_DOD$years.survived>=3 & df2002_DOD$biz_size == 1), na.rm = TRUE)/sum(df2002_DOD$biz_size == 1, na.rm = TRUE)

sr3yr2003_DOD <- sum(df2003_DOD$years.survived>=3)/nrow(df2003_DOD)
sr3yr2003sm_DOD <- sum((df2003_DOD$years.survived>=3 & df2003_DOD$biz_size == 0), na.rm = TRUE)/sum(df2003_DOD$biz_size == 0, na.rm = TRUE)
sr3yr2003non_DOD <- sum((df2003_DOD$years.survived>=3 & df2003_DOD$biz_size == 1), na.rm = TRUE)/sum(df2003_DOD$biz_size == 1, na.rm = TRUE)

sr3yr2004_DOD <- sum(df2004_DOD$years.survived>=3)/nrow(df2004_DOD) 
sr3yr2004sm_DOD <- sum((df2004_DOD$years.survived>=3 & df2004_DOD$biz_size == 0), na.rm = TRUE)/sum(df2004_DOD$biz_size == 0, na.rm = TRUE)
sr3yr2004non_DOD <- sum((df2004_DOD$years.survived>=3 & df2004_DOD$biz_size == 1), na.rm = TRUE)/sum(df2004_DOD$biz_size == 1, na.rm = TRUE)

sr3yr2005_DOD <- sum(df2005_DOD$years.survived>=3)/nrow(df2005_DOD) 
sr3yr2005sm_DOD <- sum((df2005_DOD$years.survived>=3 & df2005_DOD$biz_size == 0), na.rm = TRUE)/sum(df2005_DOD$biz_size == 0, na.rm = TRUE)
sr3yr2005non_DOD <- sum((df2005_DOD$years.survived>=3 & df2005_DOD$biz_size == 1), na.rm = TRUE)/sum(df2005_DOD$biz_size == 1, na.rm = TRUE)

sr3yr2006_DOD <- sum(df2006_DOD$years.survived>=3)/nrow(df2006_DOD) 
sr3yr2006sm_DOD <- sum((df2006_DOD$years.survived>=3 & df2006_DOD$biz_size == 0), na.rm = TRUE)/sum(df2006_DOD$biz_size == 0, na.rm = TRUE)
sr3yr2006non_DOD <- sum((df2006_DOD$years.survived>=3 & df2006_DOD$biz_size == 1), na.rm = TRUE)/sum(df2006_DOD$biz_size == 1, na.rm = TRUE)

##5-year
sr5yr2001_DOD <- sum(df2001_DOD$years.survived>=5)/nrow(df2001_DOD)
sr5yr2001sm_DOD <- sum((df2001_DOD$years.survived>=5 & df2001_DOD$biz_size == 0), na.rm = TRUE)/sum(df2001_DOD$biz_size == 0, na.rm = TRUE)
sr5yr2001non_DOD <- sum((df2001_DOD$years.survived>=5 & df2001_DOD$biz_size == 1), na.rm = TRUE)/sum(df2001_DOD$biz_size == 1, na.rm = TRUE)

sr5yr2002_DOD <- sum(df2002_DOD$years.survived>=5)/nrow(df2002_DOD) 
sr5yr2002sm_DOD <- sum((df2002_DOD$years.survived>=5 & df2002_DOD$biz_size == 0), na.rm = TRUE)/sum(df2002_DOD$biz_size == 0, na.rm = TRUE)
sr5yr2002non_DOD <- sum((df2002_DOD$years.survived>=5 & df2002_DOD$biz_size == 1), na.rm = TRUE)/sum(df2002_DOD$biz_size == 1, na.rm = TRUE)

sr5yr2003_DOD <- sum(df2003_DOD$years.survived>=5)/nrow(df2003_DOD)
sr5yr2003sm_DOD <- sum((df2003_DOD$years.survived>=5 & df2003_DOD$biz_size == 0), na.rm = TRUE)/sum(df2003_DOD$biz_size == 0, na.rm = TRUE)
sr5yr2003non_DOD <- sum((df2003_DOD$years.survived>=5 & df2003_DOD$biz_size == 1), na.rm = TRUE)/sum(df2003_DOD$biz_size == 1, na.rm = TRUE)

sr5yr2004_DOD <- sum(df2004_DOD$years.survived>=5)/nrow(df2004_DOD) 
sr5yr2004sm_DOD <- sum((df2004_DOD$years.survived>=5 & df2004_DOD$biz_size == 0), na.rm = TRUE)/sum(df2004_DOD$biz_size == 0, na.rm = TRUE)
sr5yr2004non_DOD <- sum((df2004_DOD$years.survived>=5 & df2004_DOD$biz_size == 1), na.rm = TRUE)/sum(df2004_DOD$biz_size == 1, na.rm = TRUE)

sr5yr2005_DOD <- sum(df2005_DOD$years.survived>=5)/nrow(df2005_DOD) 
sr5yr2005sm_DOD <- sum((df2005_DOD$years.survived>=5 & df2005_DOD$biz_size == 0), na.rm = TRUE)/sum(df2005_DOD$biz_size == 0, na.rm = TRUE)
sr5yr2005non_DOD <- sum((df2005_DOD$years.survived>=5 & df2005_DOD$biz_size == 1), na.rm = TRUE)/sum(df2005_DOD$biz_size == 1, na.rm = TRUE)

sr5yr2006_DOD <- sum(df2006_DOD$years.survived>=5)/nrow(df2006_DOD) 
sr5yr2006sm_DOD <- sum((df2006_DOD$years.survived>=5 & df2006_DOD$biz_size == 0), na.rm = TRUE)/sum(df2006_DOD$biz_size == 0, na.rm = TRUE)
sr5yr2006non_DOD <- sum((df2006_DOD$years.survived>=5 & df2006_DOD$biz_size == 1), na.rm = TRUE)/sum(df2006_DOD$biz_size == 1, na.rm = TRUE)


##10-year
sr10yr2001_DOD <- (sum(df2001_DOD$years.survived>=10))/nrow(df2001_DOD) 
sr10yr2001sm_DOD <- sum((df2001_DOD$years.survived>=10 & df2001_DOD$biz_size == 0), na.rm = TRUE)/sum(df2001_DOD$biz_size == 0, na.rm = TRUE)
sr10yr2001non_DOD <- sum((df2001_DOD$years.survived>=10 & df2001_DOD$biz_size == 1), na.rm = TRUE)/sum(df2001_DOD$biz_size == 1, na.rm = TRUE)


sr10yr2002_DOD <- sum(df2002_DOD$years.survived>=10)/nrow(df2002_DOD) 
sr10yr2002sm_DOD <- sum((df2002_DOD$years.survived>=10 & df2002_DOD$biz_size == 0), na.rm = TRUE)/sum(df2002_DOD$biz_size == 0, na.rm = TRUE)
sr10yr2002non_DOD <- sum((df2002_DOD$years.survived>=10 & df2002_DOD$biz_size == 1), na.rm = TRUE)/sum(df2002_DOD$biz_size == 1, na.rm = TRUE)

sr10yr2003_DOD <- sum(df2003_DOD$years.survived>=10)/nrow(df2003_DOD)
sr10yr2003sm_DOD <- sum((df2003_DOD$years.survived>=10 & df2003_DOD$biz_size == 0), na.rm = TRUE)/sum(df2003_DOD$biz_size == 0, na.rm = TRUE)
sr10yr2003non_DOD <- sum((df2003_DOD$years.survived>=10 & df2003_DOD$biz_size == 1), na.rm = TRUE)/sum(df2003_DOD$biz_size == 1, na.rm = TRUE)

sr10yr2004_DOD <- sum(df2004_DOD$years.survived>=10)/nrow(df2004_DOD) 
sr10yr2004sm_DOD <- sum((df2004_DOD$years.survived>=10 & df2004_DOD$biz_size == 0), na.rm = TRUE)/sum(df2004_DOD$biz_size == 0, na.rm = TRUE)
sr10yr2004non_DOD <- sum((df2004_DOD$years.survived>=10 & df2004_DOD$biz_size == 1), na.rm = TRUE)/sum(df2004_DOD$biz_size == 1, na.rm = TRUE)

sr10yr2005_DOD <- sum(df2005_DOD$years.survived>=10)/nrow(df2005_DOD) 
sr10yr2005sm_DOD <- sum((df2005_DOD$years.survived>=10 & df2005_DOD$biz_size == 0), na.rm = TRUE)/sum(df2005_DOD$biz_size == 0, na.rm = TRUE)
sr10yr2005non_DOD <- sum((df2005_DOD$years.survived>=10 & df2005_DOD$biz_size == 1), na.rm = TRUE)/sum(df2005_DOD$biz_size == 1, na.rm = TRUE)

sr10yr2006_DOD <- sum(df2006_DOD$years.survived>=10)/nrow(df2006_DOD) 
sr10yr2006sm_DOD <- sum((df2006_DOD$years.survived>=10 & df2006_DOD$biz_size == 0), na.rm = TRUE)/sum(df2006_DOD$biz_size == 0, na.rm = TRUE)
sr10yr2006non_DOD <- sum((df2006_DOD$years.survived>=10 & df2006_DOD$biz_size == 1), na.rm = TRUE)/sum(df2006_DOD$biz_size == 1, na.rm = TRUE)

##bound all survival rates together and reformated dataframe

survivalrate_DOD <- cbind(sr3yr2001_DOD, sr3yr2001sm_DOD, sr3yr2001non_DOD, sr5yr2001_DOD, sr5yr2001sm_DOD, sr5yr2001non_DOD, 
                          sr10yr2001_DOD, sr10yr2001sm_DOD, sr10yr2001non_DOD,
                          sr3yr2002_DOD, sr3yr2002sm_DOD, sr3yr2002non_DOD, sr5yr2002_DOD, sr5yr2002sm_DOD, sr5yr2002non_DOD, 
                          sr10yr2002_DOD, sr10yr2002sm_DOD, sr10yr2002non_DOD,
                          sr3yr2003_DOD, sr3yr2003sm_DOD, sr3yr2003non_DOD, sr5yr2003_DOD, sr5yr2003sm_DOD, sr5yr2003non_DOD, 
                          sr10yr2003_DOD, sr10yr2003sm_DOD, sr10yr2003non_DOD,
                          sr3yr2004_DOD, sr3yr2004sm_DOD, sr3yr2004non_DOD, sr5yr2004_DOD, sr5yr2004sm_DOD, sr5yr2004non_DOD, 
                          sr10yr2004_DOD, sr10yr2004sm_DOD, sr10yr2004non_DOD,
                          sr3yr2005_DOD, sr3yr2005sm_DOD, sr3yr2005non_DOD, sr5yr2005_DOD, sr5yr2005sm_DOD, sr5yr2005non_DOD, 
                          sr10yr2005_DOD, sr10yr2005sm_DOD, sr10yr2005non_DOD,
                          sr3yr2006_DOD, sr3yr2006sm_DOD, sr3yr2006non_DOD, sr5yr2006_DOD, sr5yr2006sm_DOD, sr5yr2006non_DOD, 
                          sr10yr2006_DOD, sr10yr2006sm_DOD, sr10yr2006non_DOD)
srnames_DOD <- c("3yr_2001_all_DOD", "3yr_2001_sm_DOD", 
                 "3yr_2001_non_DOD", "5yr_2001_all_DOD", "5yr_2001_sm_DOD", "5yr_2001_non_DOD", "10yr_2001_all_DOD", 
                 "10yr_2001_sm_DOD", "10yr_2001_non_DOD",
                 "3yr_2002_all_DOD", "3yr_2002_sm_DOD", "3yr_2002_non_DOD", "5yr_2002_all_DOD", "5yr_2002_sm_DOD", "5yr_2002_non_DOD", 
                 "10yr_2002_all_DOD", "10yr_2002_sm_DOD", "10yr_2002_non_DOD",
                 "3yr_2003_all_DOD", "3yr_2003_sm_DOD", "3yr_2003_non_DOD", "5yr_2003_all_DOD", "5yr_2003_sm_DOD", "5yr_2003_non_DOD", 
                 "10yr_2003_all_DOD", "10yr_2003_sm_DOD", "10yr_2003_non_DOD",
                 "3yr_2004_all_DOD", "3yr_2004_sm_DOD", "3yr_2004_non_DOD", "5yr_2004_all_DOD", "5yr_2004_sm_DOD", "5yr_2004_non_DOD", 
                 "10yr_2004_all_DOD", "10yr_2004_sm_DOD", "10yr_2004_non_DOD",
                 "3yr_2005_all_DOD", "3yr_2005_sm_DOD", "3yr_2005_non_DOD", "5yr_2005_all_DOD", "5yr_2005_sm_DOD", "5yr_2005_non_DOD", 
                 "10yr2005_all_DOD", "10yr_2005_sm_DOD", "10yr_2005_non_DOD",
                 "3yr_2006_all_DOD", "3yr_2006_sm_DOD", "3yr_2006_non_DOD", "5yr_2006_all_DOD", "5yr_2006_sm_DOD", "5yr_2006_non_DOD", 
                 "10yr_2006_all_DOD", "10yr_2006_sm_DOD", "10yr_2006_non_DOD")
colnames(survivalrate_DOD) <- srnames_DOD

sr_DOD <- as.data.frame(survivalrate_DOD)

survival.rates_DOD <- sr_DOD %>% 
  gather("3yr_2001_all_DOD":"10yr_2006_non_DOD", key = "time_year_type", value = "survivalrate_DOD") %>% 
  separate(time_year_type, into = c("time", "year", "type"), sep = "_") %>% 
  arrange(time, year) %>% 
  unite("time_type", time, type, sep = "_") 

survival.rates_DOD$time_type[18] <- "10yr_all"
survival.rates_DOD$year[18] <- 2005

survival.rates_DOD <- survival.rates_DOD %>% 
  spread(key = "time_type", value = "survivalrate_DOD")

survival.rates_DOD[, c(1, 5, 7, 6, 8, 10, 9, 2, 4, 3)]

write.csv(survival.rates_DOD, "survival rates_DOD-nop.csv")

####Descriptives: Firm-level characteristics ####
#Focusing on 2001 Sample: descriptives for firm-level independent variables (firm age, firm ownership,
#firm location, contract obligations, PSC code) and t-test testing the differences between small and non-small
#new entrants means for each independent var for all 2001 and then those that survived after 3 yrs, 5 yrs and 10
#yrs. 

####firm-level descriptives no parent id filter####
all2001_nop <- df2001_nop %>% 
  select(firm.age, years.in.SAM:obligated.amt, DEPARTMENT_NAME, AGENCY_NAME) %>% 
  mutate(NAICS.f = as.numeric(NAICS2), PSC.f = as.numeric(ServicesCategory), dept.f = as.numeric(DEPARTMENT_NAME), agency.f = as.numeric(AGENCY_NAME)) %>% 
  select(years.in.SAM, location:obligated.amt, NAICS.f:agency.f) %>% 
  summarise_all(funs(mean = mean, 
                     sd = sd), na.rm = TRUE)
all2001.num_nop <- df2001_nop %>% 
  select(firm.age, biz_size, years.in.SAM:obligated.amt, DEPARTMENT_NAME, AGENCY_NAME) %>% 
  mutate(NAICS.f = as.numeric(NAICS2), PSC.f = as.numeric(ServicesCategory), dept.f = as.numeric(DEPARTMENT_NAME), agency.f = as.numeric(AGENCY_NAME)) %>% 
  select(firm.age, biz_size, years.in.SAM, location:obligated.amt, NAICS.f:agency.f) 


all2001tidy_nop <- all2001_nop %>% 
  gather(stat, val) %>%
  separate(stat, into = c("var", "stat"), sep = "_") %>%
  spread(stat, val) %>%
  select(var, mean, sd)

#small
all2001sm_nop <- df2001_nop%>% 
  filter(biz_size == 0) %>% 
  mutate(NAICS.f = as.numeric(NAICS2), PSC.f = as.numeric(ServicesCategory), dept.f = as.numeric(DEPARTMENT_NAME), agency.f = as.numeric(AGENCY_NAME)) %>% 
  select(firm.age, years.in.SAM, location:obligated.amt, NAICS.f:agency.f) %>% 
  summarise_all(funs(mean = mean, 
                     sd = sd), na.rm = TRUE)

all2001smtidy_nop <- all2001sm_nop %>% 
  gather(stat, val) %>%
  separate(stat, into = c("var", "stat"), sep = "_") %>%
  spread(stat, val) %>%
  select(var, mean, sd)



#not small

all2001non_nop <- df2001_nop %>% 
  filter(biz_size == 1) %>% 
  mutate(NAICS.f = as.numeric(NAICS2), PSC.f = as.numeric(ServicesCategory), dept.f = as.numeric(DEPARTMENT_NAME), agency.f = as.numeric(AGENCY_NAME)) %>% 
  select(firm.age, years.in.SAM, location:obligated.amt, NAICS.f:agency.f) %>% 
  summarise_all(funs(mean = mean, 
                     sd = sd), na.rm = TRUE)

all2001nontidy_nop <- all2001non_nop %>% 
  gather(stat, val) %>%
  separate(stat, into = c("var", "stat"), sep = "_") %>%
  spread(stat, val) %>%
  select(var, mean, sd)


# 3yr 

x2001.3_nop <- df2001_nop %>% 
  filter(three.year == 1) %>% 
  mutate(NAICS.f = as.numeric(NAICS2), PSC.f = as.numeric(ServicesCategory), dept.f = as.numeric(DEPARTMENT_NAME), agency.f = as.numeric(AGENCY_NAME)) %>% 
  select(firm.age, years.in.SAM, location:obligated.amt, NAICS.f:agency.f) %>% 
  summarise_all(funs(mean = mean, 
                     sd = sd), na.rm = TRUE)

x2001.3tidy_nop <- x2001.3_nop %>% 
  gather(stat, val) %>%
  separate(stat, into = c("var", "stat"), sep = "_") %>%
  spread(stat, val) %>%
  select(var, mean, sd)

x2001.3sm_nop <- df2001_nop %>% 
  filter(three.year == 1, biz_size == 0) %>% 
  mutate(NAICS.f = as.numeric(NAICS2), PSC.f = as.numeric(ServicesCategory), dept.f = as.numeric(DEPARTMENT_NAME), agency.f = as.numeric(AGENCY_NAME)) %>% 
  select(firm.age, years.in.SAM, location:obligated.amt, NAICS.f:agency.f) %>% 
  summarise_all(funs(mean = mean, 
                     sd = sd), na.rm = TRUE)

x2001.3smtidy_nop <- x2001.3sm_nop %>% 
  gather(stat, val) %>%
  separate(stat, into = c("var", "stat"), sep = "_") %>%
  spread(stat, val) %>%
  select(var, mean, sd)


x2001.3non_nop <- df2001_nop %>% 
  filter(three.year == 1, biz_size == 0) %>% 
  mutate(NAICS.f = as.numeric(NAICS2), PSC.f = as.numeric(ServicesCategory), dept.f = as.numeric(DEPARTMENT_NAME), agency.f = as.numeric(AGENCY_NAME)) %>% 
  select(firm.age, years.in.SAM, location:obligated.amt, NAICS.f:agency.f) %>% 
  summarise_all(funs(mean = mean, 
                     sd = sd), na.rm = TRUE)

x2001.3nontidy_nop <- x2001.3non_nop %>% 
  gather(stat, val) %>%
  separate(stat, into = c("var", "stat"), sep = "_") %>%
  spread(stat, val) %>%
  select(var, mean, sd)

##5 yr


x2001.5_nop <- df2001_nop %>% 
  mutate(NAICS.f = as.numeric(NAICS2), PSC.f = as.numeric(ServicesCategory), dept.f = as.numeric(DEPARTMENT_NAME), agency.f = as.numeric(AGENCY_NAME)) %>% 
  select(firm.age, years.in.SAM, location:obligated.amt, NAICS.f:agency.f) %>% 
  summarise_all(funs(mean = mean, 
                     sd = sd), na.rm = TRUE)

x2001.5tidy_nop <- x2001.5_nop %>% 
  gather(stat, val) %>%
  separate(stat, into = c("var", "stat"), sep = "_") %>%
  spread(stat, val) %>%
  select(var, mean, sd)

x2001.5sm_nop <- df2001_nop %>% 
  filter(five.year == 1, biz_size == 0) %>% 
  mutate(NAICS.f = as.numeric(NAICS2), PSC.f = as.numeric(ServicesCategory), dept.f = as.numeric(DEPARTMENT_NAME), agency.f = as.numeric(AGENCY_NAME)) %>% 
  select(firm.age, years.in.SAM, location:obligated.amt, NAICS.f:agency.f) %>% 
  summarise_all(funs(mean = mean, 
                     sd = sd), na.rm = TRUE)

x2001.5smtidy_nop <- x2001.5sm_nop %>% 
  gather(stat, val) %>%
  separate(stat, into = c("var", "stat"), sep = "_") %>%
  spread(stat, val) %>%
  select(var, mean, sd)


x2001.5non_nop <- df2001_nop %>% 
  filter(five.year == 1, biz_size == 0) %>% 
  mutate(NAICS.f = as.numeric(NAICS2), PSC.f = as.numeric(ServicesCategory), dept.f = as.numeric(DEPARTMENT_NAME), agency.f = as.numeric(AGENCY_NAME)) %>% 
  select(firm.age, years.in.SAM, location:obligated.amt, NAICS.f:agency.f) %>% 
  summarise_all(funs(mean = mean, 
                     sd = sd), na.rm = TRUE)

x2001.5nontidy_nop <- x2001.3non_nop %>% 
  gather(stat, val) %>%
  separate(stat, into = c("var", "stat"), sep = "_") %>%
  spread(stat, val) %>%
  select(var, mean, sd)

##10 yr


x2001.10_nop <- df2001_nop %>% 
  mutate(NAICS.f = as.numeric(NAICS2), PSC.f = as.numeric(ServicesCategory), dept.f = as.numeric(DEPARTMENT_NAME), agency.f = as.numeric(AGENCY_NAME)) %>% 
  select(firm.age, years.in.SAM, location:obligated.amt, NAICS.f:agency.f) %>% 
  summarise_all(funs(mean = mean, 
                     sd = sd), na.rm = TRUE)

x2001.10tidy_nop <- x2001.10_nop %>% 
  gather(stat, val) %>%
  separate(stat, into = c("var", "stat"), sep = "_") %>%
  spread(stat, val) %>%
  select(var, mean, sd)


x2001.10sm_nop <- df2001_nop %>% 
  filter(ten.year == 1, biz_size == 0) %>% 
  mutate(NAICS.f = as.numeric(NAICS2), PSC.f = as.numeric(ServicesCategory), dept.f = as.numeric(DEPARTMENT_NAME), agency.f = as.numeric(AGENCY_NAME)) %>% 
  select(firm.age, years.in.SAM, location:obligated.amt, NAICS.f:agency.f) %>% 
  summarise_all(funs(mean = mean, 
                     sd = sd), na.rm = TRUE)

x2001.10smtidy_nop <- x2001.10sm_nop %>% 
  gather(stat, val) %>%
  separate(stat, into = c("var", "stat"), sep = "_") %>%
  spread(stat, val) %>%
  select(var, mean, sd)

x2001.10non_nop <- df2001_nop %>% 
  filter(ten.year == 1, biz_size == 0) %>% 
  mutate(NAICS.f = as.numeric(NAICS2), PSC.f = as.numeric(ServicesCategory), dept.f = as.numeric(DEPARTMENT_NAME), agency.f = as.numeric(AGENCY_NAME)) %>% 
  select(firm.age, years.in.SAM, location:obligated.amt, NAICS.f:agency.f) %>% 
  summarise_all(funs(mean = mean, 
                     sd = sd), na.rm = TRUE)

x2001.10nontidy_nop <- x2001.10non_nop %>% 
  gather(stat, val) %>%
  separate(stat, into = c("var", "stat"), sep = "_") %>%
  spread(stat, val) %>%
  select(var, mean, sd)



stats.2001_nop <- bind_rows(list(all2001tidy_nop, all2001smtidy_nop, all2001nontidy_nop, 
                                 x2001.3tidy_nop, x2001.3smtidy_nop, x2001.3nontidy_nop, 
                                 x2001.5tidy_nop, x2001.5smtidy_nop, x2001.5nontidy_nop,
                                 x2001.10tidy_nop, x2001.10smtidy_nop, x2001.10nontidy_nop), .id = "ID")

stats.2001_nop$ID[stats.2001_nop$ID == 1] = "all 2001"
stats.2001_nop$ID[stats.2001_nop$ID == 2] = "all 2001 small"
stats.2001_nop$ID[stats.2001_nop$ID == 3] = "all 2001 non-small"
stats.2001_nop$ID[stats.2001_nop$ID == 4] = "3 year"
stats.2001_nop$ID[stats.2001_nop$ID == 5] = "3 year small"
stats.2001_nop$ID[stats.2001_nop$ID == 6] = "3 year non-small"
stats.2001_nop$ID[stats.2001_nop$ID == 7] = "5 year"  
stats.2001_nop$ID[stats.2001_nop$ID == 8] = "5 year small"
stats.2001_nop$ID[stats.2001_nop$ID == 9] = "5 year non-small"
stats.2001_nop$ID[stats.2001_nop$ID == 10] = "10 year"
stats.2001_nop$ID[stats.2001_nop$ID == 11] = "10 year small"
stats.2001_nop$ID[stats.2001_nop$ID == 12] = "10 year non-small"


##t test

all_nop <- lapply(all2001.num_nop[ ,c("firm.age", "years.in.SAM", "ownership.woman", "ownership.veteran", 
                                      "ownership.minority", "ownership.foreign", "location", 
                                      "contract.actions", "obligated.amt", "NAICS.f", "PSC.f", "dept.f", "agency.f")], 
                  function(x) t.test(x ~ all2001.num_nop$biz_size))

t.test_nop <- as.data.frame(cbind("firm.age" = all_nop$firm.age, "years.in.SAM" = all_nop$years.in.SAM, 
                                  "ownership.woman" = all_nop$ownership.woman, 'ownership.veteran' = all_nop$ownership.veteran, 
                                  "ownership.minority" = all_nop$ownership.minority, 
                                  "ownership.foreign" = all_nop$ownership.foreign, "location" = all_nop$location, 
                                  "contract.actions" = all_nop$contract.actions, 
                                  "obligated.amt" = all_nop$obligated.amt, "NAICS" = all_nop$NAICS.f, "PSC" = all_nop$PSC.f, "department" = all_nop$dept.f, "agency" = all_nop$agency.f))


t.test_nop$firm.age = as.character(t.test$firm.age)
t.test_nop$years.in.SAM = as.character(t.test$years.in.SAM)
t.test_nop$ownership.woman = as.character(t.test$ownership.woman)
t.test_nop$ownership.veteran = as.character(t.test$ownership.veteran)
t.test_nop$ownership.foreign = as.character(t.test$ownership.foreign)
t.test_nop$ownership.minority = as.character(t.test$ownership.minority)
t.test_nop$location = as.character(t.test$location)
t.test_nop$contract.actions = as.character(t.test$contract.actions)
t.test_nop$obligated.amt = as.character(t.test$obligated.amt)
t.test_nop$NAICS = as.character(t.test$NAICS)
t.test_nop$PSC = as.character(t.test$PSC)
t.test_nop$department = as.character(t.test$department)
t.test_nop$agency = as.character(t.test$agency)

write.csv(survival.rates_nop, "survival rates_nop.csv")
write.csv(t.test_nop, "t-test numeric_nop.csv")

#### Categorical variable levels

naics.lev_nop <- as.data.frame(levels(df2001_nop$NAICS2))
psc.lev_nop <- as.data.frame(levels(df2001_nop$ServicesCategory))
dept.lev_nop <- as.data.frame(levels(df2001_nop$DEPARTMENT_NAME))
agency.lev_nop <- as.data.frame(levels(df2001_nop$DEPARTMENT_NAME))



####firm-level descriptives with parent id filter####
all2001 <- df2001 %>% 
  select(firm.age, years.in.SAM:obligated.amt, DEPARTMENT_NAME, AGENCY_NAME) %>% 
  mutate(NAICS.f = as.numeric(NAICS2), PSC.f = as.numeric(ServicesCategory), dept.f = as.numeric(DEPARTMENT_NAME), agency.f = as.numeric(AGENCY_NAME)) %>% 
  select(years.in.SAM, location:obligated.amt, NAICS.f:agency.f) %>% 
  summarise_all(funs(mean = mean, 
                     sd = sd), na.rm = TRUE)
all2001.num <- df2001 %>% 
  select(firm.age, biz_size, years.in.SAM:obligated.amt, DEPARTMENT_NAME, AGENCY_NAME) %>% 
  mutate(NAICS.f = as.numeric(NAICS2), PSC.f = as.numeric(ServicesCategory), dept.f = as.numeric(DEPARTMENT_NAME), agency.f = as.numeric(AGENCY_NAME)) %>% 
  select(firm.age, biz_size, years.in.SAM, location:obligated.amt, NAICS.f:agency.f) 


all2001tidy <- all2001 %>% 
  gather(stat, val) %>%
  separate(stat, into = c("var", "stat"), sep = "_") %>%
  spread(stat, val) %>%
  select(var, mean, sd)

#small
all2001sm <- df2001%>% 
  filter(biz_size == 0) %>% 
  mutate(NAICS.f = as.numeric(NAICS2), PSC.f = as.numeric(ServicesCategory), dept.f = as.numeric(DEPARTMENT_NAME), agency.f = as.numeric(AGENCY_NAME)) %>% 
  select(firm.age, years.in.SAM, location:obligated.amt, NAICS.f:agency.f) %>% 
  summarise_all(funs(mean = mean, 
                     sd = sd), na.rm = TRUE)

all2001smtidy <- all2001sm %>% 
  gather(stat, val) %>%
  separate(stat, into = c("var", "stat"), sep = "_") %>%
  spread(stat, val) %>%
  select(var, mean, sd)



#not small

all2001non <- df2001 %>% 
  filter(biz_size == 1) %>% 
  mutate(NAICS.f = as.numeric(NAICS2), PSC.f = as.numeric(ServicesCategory), dept.f = as.numeric(DEPARTMENT_NAME), agency.f = as.numeric(AGENCY_NAME)) %>% 
  select(firm.age, years.in.SAM, location:obligated.amt, NAICS.f:agency.f) %>% 
  summarise_all(funs(mean = mean, 
                     sd = sd), na.rm = TRUE)

all2001nontidy <- all2001non %>% 
  gather(stat, val) %>%
  separate(stat, into = c("var", "stat"), sep = "_") %>%
  spread(stat, val) %>%
  select(var, mean, sd)


# 3yr 

x2001.3 <- df2001 %>% 
  filter(three.year == 1) %>% 
  mutate(NAICS.f = as.numeric(NAICS2), PSC.f = as.numeric(ServicesCategory), dept.f = as.numeric(DEPARTMENT_NAME), agency.f = as.numeric(AGENCY_NAME)) %>% 
  select(firm.age, years.in.SAM, location:obligated.amt, NAICS.f:agency.f) %>% 
  summarise_all(funs(mean = mean, 
                     sd = sd), na.rm = TRUE)

x2001.3tidy <- x2001.3 %>% 
  gather(stat, val) %>%
  separate(stat, into = c("var", "stat"), sep = "_") %>%
  spread(stat, val) %>%
  select(var, mean, sd)

x2001.3sm <- df2001 %>% 
  filter(three.year == 1, biz_size == 0) %>% 
  mutate(NAICS.f = as.numeric(NAICS2), PSC.f = as.numeric(ServicesCategory), dept.f = as.numeric(DEPARTMENT_NAME), agency.f = as.numeric(AGENCY_NAME)) %>% 
  select(firm.age, years.in.SAM, location:obligated.amt, NAICS.f:agency.f) %>% 
  summarise_all(funs(mean = mean, 
                     sd = sd), na.rm = TRUE)

x2001.3smtidy <- x2001.3sm %>% 
  gather(stat, val) %>%
  separate(stat, into = c("var", "stat"), sep = "_") %>%
  spread(stat, val) %>%
  select(var, mean, sd)


x2001.3non <- df2001 %>% 
  filter(three.year == 1, biz_size == 0) %>% 
  mutate(NAICS.f = as.numeric(NAICS2), PSC.f = as.numeric(ServicesCategory), dept.f = as.numeric(DEPARTMENT_NAME), agency.f = as.numeric(AGENCY_NAME)) %>% 
  select(firm.age, years.in.SAM, location:obligated.amt, NAICS.f:agency.f) %>% 
  summarise_all(funs(mean = mean, 
                     sd = sd), na.rm = TRUE)

x2001.3nontidy <- x2001.3non %>% 
  gather(stat, val) %>%
  separate(stat, into = c("var", "stat"), sep = "_") %>%
  spread(stat, val) %>%
  select(var, mean, sd)

##5 yr


x2001.5 <- df2001 %>% 
  mutate(NAICS.f = as.numeric(NAICS2), PSC.f = as.numeric(ServicesCategory), dept.f = as.numeric(DEPARTMENT_NAME), agency.f = as.numeric(AGENCY_NAME)) %>% 
  select(firm.age, years.in.SAM, location:obligated.amt, NAICS.f:agency.f) %>% 
  summarise_all(funs(mean = mean, 
                     sd = sd), na.rm = TRUE)

x2001.5tidy <- x2001.5 %>% 
  gather(stat, val) %>%
  separate(stat, into = c("var", "stat"), sep = "_") %>%
  spread(stat, val) %>%
  select(var, mean, sd)

x2001.5sm <- df2001 %>% 
  filter(five.year == 1, biz_size == 0) %>% 
  mutate(NAICS.f = as.numeric(NAICS2), PSC.f = as.numeric(ServicesCategory), dept.f = as.numeric(DEPARTMENT_NAME), agency.f = as.numeric(AGENCY_NAME)) %>% 
  select(firm.age, years.in.SAM, location:obligated.amt, NAICS.f:agency.f) %>% 
  summarise_all(funs(mean = mean, 
                     sd = sd), na.rm = TRUE)

x2001.5smtidy <- x2001.5sm %>% 
  gather(stat, val) %>%
  separate(stat, into = c("var", "stat"), sep = "_") %>%
  spread(stat, val) %>%
  select(var, mean, sd)


x2001.5non <- df2001 %>% 
  filter(five.year == 1, biz_size == 0) %>% 
  mutate(NAICS.f = as.numeric(NAICS2), PSC.f = as.numeric(ServicesCategory), dept.f = as.numeric(DEPARTMENT_NAME), agency.f = as.numeric(AGENCY_NAME)) %>% 
  select(firm.age, years.in.SAM, location:obligated.amt, NAICS.f:agency.f) %>% 
  summarise_all(funs(mean = mean, 
                     sd = sd), na.rm = TRUE)

x2001.5nontidy <- x2001.3non %>% 
  gather(stat, val) %>%
  separate(stat, into = c("var", "stat"), sep = "_") %>%
  spread(stat, val) %>%
  select(var, mean, sd)

##10 yr


x2001.10 <- df2001 %>% 
  mutate(NAICS.f = as.numeric(NAICS2), PSC.f = as.numeric(ServicesCategory), dept.f = as.numeric(DEPARTMENT_NAME), agency.f = as.numeric(AGENCY_NAME)) %>% 
  select(firm.age, years.in.SAM, location:obligated.amt, NAICS.f:agency.f) %>% 
  summarise_all(funs(mean = mean, 
                     sd = sd), na.rm = TRUE)

x2001.10tidy <- x2001.10 %>% 
  gather(stat, val) %>%
  separate(stat, into = c("var", "stat"), sep = "_") %>%
  spread(stat, val) %>%
  select(var, mean, sd)


x2001.10sm <- df2001 %>% 
  filter(ten.year == 1, biz_size == 0) %>% 
  mutate(NAICS.f = as.numeric(NAICS2), PSC.f = as.numeric(ServicesCategory), dept.f = as.numeric(DEPARTMENT_NAME), agency.f = as.numeric(AGENCY_NAME)) %>% 
  select(firm.age, years.in.SAM, location:obligated.amt, NAICS.f:agency.f) %>% 
  summarise_all(funs(mean = mean, 
                     sd = sd), na.rm = TRUE)

x2001.10smtidy <- x2001.10sm %>% 
  gather(stat, val) %>%
  separate(stat, into = c("var", "stat"), sep = "_") %>%
  spread(stat, val) %>%
  select(var, mean, sd)

x2001.10non <- df2001 %>% 
  filter(ten.year == 1, biz_size == 0) %>% 
  mutate(NAICS.f = as.numeric(NAICS2), PSC.f = as.numeric(ServicesCategory), dept.f = as.numeric(DEPARTMENT_NAME), agency.f = as.numeric(AGENCY_NAME)) %>% 
  select(firm.age, years.in.SAM, location:obligated.amt, NAICS.f:agency.f) %>% 
  summarise_all(funs(mean = mean, 
                     sd = sd), na.rm = TRUE)

x2001.10nontidy <- x2001.10non %>% 
  gather(stat, val) %>%
  separate(stat, into = c("var", "stat"), sep = "_") %>%
  spread(stat, val) %>%
  select(var, mean, sd)



stats.2001 <- bind_rows(list(all2001tidy, all2001smtidy, all2001nontidy, 
                             x2001.3tidy, x2001.3smtidy, x2001.3nontidy, 
                             x2001.5tidy, x2001.5smtidy, x2001.5nontidy,
                             x2001.10tidy, x2001.10smtidy, x2001.10nontidy), .id = "ID")

stats.2001$ID[stats.2001$ID == 1] = "all 2001"
stats.2001$ID[stats.2001$ID == 2] = "all 2001 small"
stats.2001$ID[stats.2001$ID == 3] = "all 2001 non-small"
stats.2001$ID[stats.2001$ID == 4] = "3 year"
stats.2001$ID[stats.2001$ID == 5] = "3 year small"
stats.2001$ID[stats.2001$ID == 6] = "3 year non-small"
stats.2001$ID[stats.2001$ID == 7] = "5 year"  
stats.2001$ID[stats.2001$ID == 8] = "5 year small"
stats.2001$ID[stats.2001$ID == 9] = "5 year non-small"
stats.2001$ID[stats.2001$ID == 10] = "10 year"
stats.2001$ID[stats.2001$ID == 11] = "10 year small"
stats.2001$ID[stats.2001$ID == 12] = "10 year non-small"


##t test

all <- lapply(all2001.num[ ,c("firm.age", "years.in.SAM", "ownership.woman", "ownership.veteran", 
                              "ownership.minority", "ownership.foreign", "location", 
                              "contract.actions", "obligated.amt", "NAICS.f", "PSC.f", "dept.f", "agency.f")], 
              function(x) t.test(x ~ all2001.num$biz_size))

t.test <- as.data.frame(cbind("firm.age" = all$firm.age, "years.in.SAM" = all$years.in.SAM, 
                              "ownership.woman" = all$ownership.woman, 'ownership.veteran' = all$ownership.veteran, 
                              "ownership.minority" = all$ownership.minority, 
                              "ownership.foreign" = all$ownership.foreign, "location" = all$location, 
                              "contract.actions" = all$contract.actions, 
                              "obligated.amt" = all$obligated.amt, "NAICS" = all$NAICS.f, "PSC" = all$PSC.f, "department" = all$dept.f, "agency" = all$agency.f))


t.test$firm.age = as.character(t.test$firm.age)
t.test$years.in.SAM = as.character(t.test$years.in.SAM)
t.test$ownership.woman = as.character(t.test$ownership.woman)
t.test$ownership.veteran = as.character(t.test$ownership.veteran)
t.test$ownership.foreign = as.character(t.test$ownership.foreign)
t.test$ownership.minority = as.character(t.test$ownership.minority)
t.test$location = as.character(t.test$location)
t.test$contract.actions = as.character(t.test$contract.actions)
t.test$obligated.amt = as.character(t.test$obligated.amt)
t.test$NAICS = as.character(t.test$NAICS)
t.test$PSC = as.character(t.test$PSC)
t.test$department = as.character(t.test$department)
t.test$agency = as.character(t.test$agency)

write.csv(survival.rates, "survival rates.csv")
write.csv(survival.rates_nop, "survival rates reg2000-2011-no parent filter.csv")
write.csv(t.test, "t-test numeric.csv")

#### Categorical variable levels

naics.lev <- as.data.frame(levels(df2001$NAICS2))
psc.lev <- as.data.frame(levels(df2001$ServicesCategory))
dept.lev <- as.data.frame(levels(df2001$DEPARTMENT_NAME))
agency.lev <- as.data.frame(levels(df2001$DEPARTMENT_NAME))

### Graduated firms ####
### Grad: Survival Rates####

###Survival rates by regDate

gradfirm <- read_csv("graduatedfirms.csv")

df2001_grad <- gradfirm %>% 
  filter(year(registrationDate.x) == 2001 )
df2002_grad <- gradfirm %>%
  filter(year(registrationDate.x) == 2002 )
df2003_grad <- gradfirm %>%
  filter(year(registrationDate.x) == 2003 )
df2004_grad <- gradfirm %>%
  filter(year(registrationDate.x) == 2004 )
df2005_grad <- gradfirm %>%
  filter(year(registrationDate.x) == 2005 )
df2006_grad <- gradfirm %>%
  filter(year(registrationDate.x) == 2006 )

sr3yr2001_grad <- sum(df2001_grad$years.in.SAM>=3)/nrow(df2001_grad)
sr3yr2001sm_grad <- sum((df2001_grad$years.in.SAM>=3 & df2001_grad$biz_size.x == 0), na.rm = TRUE)/nrow(df2001_grad)
sr3yr2001non_grad <- sum((df2001_grad$years.in.SAM>=3 & df2001_grad$biz_size.x == 1), na.rm = TRUE)/nrow(df2001_grad)

sr3yr2002_grad <- sum(df2002_grad$years.in.SAM>=3)/nrow(df2002_grad) 
sr3yr2002sm_grad <- sum((df2002_grad$years.in.SAM>=3 & df2002_grad$biz_size.x == 0), na.rm = TRUE)/nrow(df2002_grad)
sr3yr2002non_grad <- sum((df2002_grad$years.in.SAM>=3 & df2002_grad$biz_size.x == 1), na.rm = TRUE)/nrow(df2002_grad)

sr3yr2003_grad <- sum(df2003_grad$years.in.SAM>=3)/nrow(df2003_grad)
sr3yr2003sm_grad <- sum((df2003_grad$years.in.SAM>=3 & df2003_grad$biz_size.x == 0), na.rm = TRUE)/nrow(df2003_grad)
sr3yr2003non_grad <- sum((df2003_grad$years.in.SAM>=3 & df2003_grad$biz_size.x == 1), na.rm = TRUE)/nrow(df2003_grad)

sr3yr2004_grad <- sum(df2004_grad$years.in.SAM>=3)/nrow(df2004_grad) 
sr3yr2004sm_grad <- sum((df2004_grad$years.in.SAM>=3 & df2004_grad$biz_size.x == 0), na.rm = TRUE)/nrow(df2004_grad)
sr3yr2004non_grad <- sum((df2004_grad$years.in.SAM>=3 & df2004_grad$biz_size.x == 1), na.rm = TRUE)/nrow(df2004_grad)

sr3yr2005_grad <- sum(df2005_grad$years.in.SAM>=3)/nrow(df2005_grad) 
sr3yr2005sm_grad <- sum((df2005_grad$years.in.SAM>=3 & df2005_grad$biz_size.x == 0), na.rm = TRUE)/nrow(df2005_grad)
sr3yr2005non_grad <- sum((df2005_grad$years.in.SAM>=3 & df2005_grad$biz_size.x == 1), na.rm = TRUE)/nrow(df2005_grad)

sr3yr2006_grad <- sum(df2006_grad$years.in.SAM>=3)/nrow(df2006_grad) 
sr3yr2006sm_grad <- sum((df2006_grad$years.in.SAM>=3 & df2006_grad$biz_size.x == 0), na.rm = TRUE)/nrow(df2006_grad)
sr3yr2006non_grad <- sum((df2006_grad$years.in.SAM>=3 & df2006_grad$biz_size.x == 1), na.rm = TRUE)/nrow(df2006_grad)

##5-year
sr5yr2001_grad <- sum(df2001_grad$years.in.SAM>=5)/nrow(df2001_grad)
sr5yr2001sm_grad <- sum((df2001_grad$years.in.SAM>=5 & df2001_grad$biz_size.x == 0), na.rm = TRUE)/nrow(df2001_grad)
sr5yr2001non_grad <- sum((df2001_grad$years.in.SAM>=5 & df2001_grad$biz_size.x == 1), na.rm = TRUE)/nrow(df2001_grad)

sr5yr2002_grad <- sum(df2002_grad$years.in.SAM>=5)/nrow(df2002_grad) 
sr5yr2002sm_grad <- sum((df2002_grad$years.in.SAM>=5 & df2002_grad$biz_size.x == 0), na.rm = TRUE)/nrow(df2002_grad)
sr5yr2002non_grad <- sum((df2002_grad$years.in.SAM>=5 & df2002_grad$biz_size.x == 1), na.rm = TRUE)/nrow(df2002_grad)

sr5yr2003_grad <- sum(df2003_grad$years.in.SAM>=5)/nrow(df2003_grad)
sr5yr2003sm_grad <- sum((df2003_grad$years.in.SAM>=5 & df2003_grad$biz_size.x == 0), na.rm = TRUE)/nrow(df2003_grad)
sr5yr2003non_grad <- sum((df2003_grad$years.in.SAM>=5 & df2003_grad$biz_size.x == 1), na.rm = TRUE)/nrow(df2003_grad)

sr5yr2004_grad <- sum(df2004_grad$years.in.SAM>=5)/nrow(df2004_grad) 
sr5yr2004sm_grad <- sum((df2004_grad$years.in.SAM>=5 & df2004_grad$biz_size.x == 0), na.rm = TRUE)/nrow(df2004_grad)
sr5yr2004non_grad <- sum((df2004_grad$years.in.SAM>=5 & df2004_grad$biz_size.x == 1), na.rm = TRUE)/nrow(df2004_grad)

sr5yr2005_grad <- sum(df2005_grad$years.in.SAM>=5)/nrow(df2005_grad) 
sr5yr2005sm_grad <- sum((df2005_grad$years.in.SAM>=5 & df2005_grad$biz_size.x == 0), na.rm = TRUE)/nrow(df2005_grad)
sr5yr2005non_grad <- sum((df2005_grad$years.in.SAM>=5 & df2005_grad$biz_size.x == 1), na.rm = TRUE)/nrow(df2005_grad)

sr5yr2006_grad <- sum(df2006_grad$years.in.SAM>=5)/nrow(df2006_grad) 
sr5yr2006sm_grad <- sum((df2006_grad$years.in.SAM>=5 & df2006_grad$biz_size.x == 0), na.rm = TRUE)/nrow(df2006_grad)
sr5yr2006non_grad <- sum((df2006_grad$years.in.SAM>=5 & df2006_grad$biz_size.x == 1), na.rm = TRUE)/nrow(df2006_grad)


##10-year
sr10yr2001_grad <- (sum(df2001_grad$years.in.SAM>=10)/nrow(df2001_grad) 
                    sr10yr2001sm_grad <- sum((df2001_grad$years.in.SAM>=10 & df2001_grad$biz_size.x == 0), na.rm = TRUE)/893
                    sr10yr2001non_grad <- sum((df2001_grad$years.in.SAM>=10 & df2001_grad$biz_size.x == 1), na.rm = TRUE)/893
                    
                    
                    sr10yr2002_grad <- sum(df2002_grad$years.in.SAM>=10)/nrow(df2002_grad) 
                    sr10yr2002sm_grad <- sum((df2002_grad$years.in.SAM>=10 & df2002_grad$biz_size.x == 0), na.rm = TRUE)/nrow(df2002_grad)
                    sr10yr2002non_grad <- sum((df2002_grad$years.in.SAM>=10 & df2002_grad$biz_size.x == 1), na.rm = TRUE)/nrow(df2002_grad)
                    
                    sr10yr2003_grad <- sum(df2003_grad$years.in.SAM>=10)/nrow(df2003_grad)
                    sr10yr2003sm_grad <- sum((df2003_grad$years.in.SAM>=10 & df2003_grad$biz_size.x == 0), na.rm = TRUE)/nrow(df2003_grad)
                    sr10yr2003non_grad <- sum((df2003_grad$years.in.SAM>=10 & df2003_grad$biz_size.x == 1), na.rm = TRUE)/nrow(df2003_grad)
                    
                    sr10yr2004_grad <- sum(df2004_grad$years.in.SAM>=10)/nrow(df2004_grad) 
                    sr10yr2004sm_grad <- sum((df2004_grad$years.in.SAM>=10 & df2004_grad$biz_size.x == 0), na.rm = TRUE)/nrow(df2004_grad)
                    sr10yr2004non_grad <- sum((df2004_grad$years.in.SAM>=10 & df2004_grad$biz_size.x == 1), na.rm = TRUE)/nrow(df2004_grad)
                    
                    sr10yr2005_grad <- sum(df2005_grad$years.in.SAM>=10)/nrow(df2005_grad) 
                    sr10yr2005sm_grad <- sum((df2005_grad$years.in.SAM>=10 & df2005_grad$biz_size.x == 0), na.rm = TRUE)/nrow(df2005_grad)
                    sr10yr2005non_grad <- sum((df2005_grad$years.in.SAM>=10 & df2005_grad$biz_size.x == 1), na.rm = TRUE)/nrow(df2005_grad)
                    
                    sr10yr2006_grad <- sum(df2006_grad$years.in.SAM>=10)/nrow(df2006_grad) 
                    sr10yr2006sm_grad <- sum((df2006_grad$years.in.SAM>=10 & df2006_grad$biz_size.x == 0), na.rm = TRUE)/nrow(df2006_grad)
                    sr10yr2006non_grad <- sum((df2006_grad$years.in.SAM>=10 & df2006_grad$biz_size.x == 1), na.rm = TRUE)/nrow(df2006_grad)
                    
                    ##bound all survival rates together and reformated dataframe
                    
                    survivalrate_grad <- cbind(sr3yr2001_grad, sr3yr2001sm_grad, sr3yr2001non_grad, sr5yr2001_grad, sr5yr2001sm_grad, sr5yr2001non_grad, 
                                               sr10yr2001_grad, sr10yr2001sm_grad, sr10yr2001non_grad,
                                               sr3yr2002_grad, sr3yr2002sm_grad, sr3yr2002non_grad, sr5yr2002_grad, sr5yr2002sm_grad, sr5yr2002non_grad, 
                                               sr10yr2002_grad, sr10yr2002sm_grad, sr10yr2002non_grad,
                                               sr3yr2003_grad, sr3yr2003sm_grad, sr3yr2003non_grad, sr5yr2003_grad, sr5yr2003sm_grad, sr5yr2003non_grad, 
                                               sr10yr2003_grad, sr10yr2003sm_grad, sr10yr2003non_grad,
                                               sr3yr2004_grad, sr3yr2004sm_grad, sr3yr2004non_grad, sr5yr2004_grad, sr5yr2004sm_grad, sr5yr2004non_grad, 
                                               sr10yr2004_grad, sr10yr2004sm_grad, sr10yr2004non_grad,
                                               sr3yr2005_grad, sr3yr2005sm_grad, sr3yr2005non_grad, sr5yr2005_grad, sr5yr2005sm_grad, sr5yr2005non_grad, 
                                               sr10yr2005_grad, sr10yr2005sm_grad, sr10yr2005non_grad,
                                               sr3yr2006_grad, sr3yr2006sm_grad, sr3yr2006non_grad, sr5yr2006_grad, sr5yr2006sm_grad, sr5yr2006non_grad, 
                                               sr10yr2006_grad, sr10yr2006sm_grad, sr10yr2006non_grad)
                    srnames_grad <- c("3yr_2001_all_grad", "3yr_2001_sm_grad", 
                                      "3yr_2001_non_grad", "5yr_2001_all_grad", "5yr_2001_sm_grad", "5yr_2001_non_grad", "10yr_2001_all_grad", 
                                      "10yr_2001_sm_grad", "10yr_2001_non_grad",
                                      "3yr_2002_all_grad", "3yr_2002_sm_grad", "3yr_2002_non_grad", "5yr_2002_all_grad", "5yr_2002_sm_grad", "5yr_2002_non_grad", 
                                      "10yr_2002_all_grad", "10yr_2002_sm_grad", "10yr_2002_non_grad",
                                      "3yr_2003_all_grad", "3yr_2003_sm_grad", "3yr_2003_non_grad", "5yr_2003_all_grad", "5yr_2003_sm_grad", "5yr_2003_non_grad", 
                                      "10yr_2003_all_grad", "10yr_2003_sm_grad", "10yr_2003_non_grad",
                                      "3yr_2004_all_grad", "3yr_2004_sm_grad", "3yr_2004_non_grad", "5yr_2004_all_grad", "5yr_2004_sm_grad", "5yr_2004_non_grad", 
                                      "10yr_2004_all_grad", "10yr_2004_sm_grad", "10yr_2004_non_grad",
                                      "3yr_2005_all_grad", "3yr_2005_sm_grad", "3yr_2005_non_grad", "5yr_2005_all_grad", "5yr_2005_sm_grad", "5yr_2005_non_grad", 
                                      "10yr_2005_all_grad", "10yr_2005_sm_grad", "10yr_2005_non_grad",
                                      "3yr_2006_all_grad", "3yr_2006_sm_grad", "3yr_2006_non_grad", "5yr_2006_all_grad", "5yr_2006_sm_grad", "5yr_2006_non_grad", 
                                      "10yr_2006_all_grad", "10yr_2006_sm_grad", "10yr_2006_non_grad")
                    colnames(survivalrate_grad) <- srnames_grad
                    
                    sr_grad <- as.data.frame(survivalrate_grad)
                    
                    survival.rates_grad <- sr_grad %>% 
                      gather("3yr_2001_all_grad":"10yr_2006_non_grad", key = "time_year_type_delete", value = "survivalrate_grad") %>% 
                      separate(time_year_type_delete, into = c("time", "year", "type", "delete"), sep = "_") %>% 
                      arrange(time, year) %>% 
                      unite("time_type", time, type, sep = "_") 
                    
                    
                    
                    survival.rates_grad <- survival.rates_grad %>% 
                      spread(key = "time_type", value = "survivalrate_grad") %>% 
                      select(-delete)
                    
                    survival.rates_grad[, c(1, 5, 7, 6, 8, 10, 9, 2, 4, 3)]
                    
                    write.csv(survival.rates_grad, "survival rates_grad-nop.csv")
                    
                    
