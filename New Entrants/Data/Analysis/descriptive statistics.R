install.packages("matrixStats")
library(matrixStats)
library(describer)
library(tidyverse)

##Marielle working Directory
setwd("K:/2018-01 NPS New Entrants/Data/Data")
getwd()

##Sam working Directory
setwd("K:/2018-01 NPS New Entrants/Data/Data/Cleaned Data")
getwd()

###if no parent filter

df2001_nop <- read_csv("dataset2001nop.csv")
df2002_nop <- read_csv("dataset2002nop.csv")
df2003_nop <- read_csv("dataset2003nop.csv")
df2004_nop <- read_csv("dataset2004nop.csv")
df2005_nop <- read_csv("dataset2005nop.csv")
df2006_nop <- read_csv("dataset2006nop.csv")


### if parent filter

df2001 <- read_csv("dataset2001.csv")
df2002 <- read_csv("dataset2002.csv")
df2003 <- read_csv("dataset2003.csv")
df2004 <- read_csv("dataset2004.csv")
df2005 <- read_csv("dataset2005.csv")
df2006 <- read_csv("dataset2006.csv")

###Cleaning variables to categorical and numeric####
###No Parent ID Filter
df2001_nop$NAICS2 = as.factor(df2001_nop$NAICS2)
df2001_nop$ServicesCategory = as.factor(df2001_nop$ServicesCategory)
df2001_nop$location = as.numeric(df2001_nop$location)
df2001_nop$ownership.woman = as.numeric(df2001_nop$ownership.woman)
df2001_nop$ownership.veteran = as.numeric(df2001_nop$ownership.veteran)
df2001_nop$ownership.minority = as.numeric(df2001_nop$ownership.minority)
df2001_nop$ownership.foreign = as.numeric(df2001_nop$ownership.foreign)
df2001_nop$survival.status = as.numeric(df2001_nop$survival.status)
df2001_nop$DEPARTMENT_NAME = as.factor(df2001_nop$DEPARTMENT_NAME)
df2001_nop$AGENCY_NAME = as.factor(df2001_nop$AGENCY_NAME)


df2002_nop$NAICS2 = as.factor(df2002_nop$NAICS2)
df2002_nop$ServicesCategory = as.factor(df2002_nop$ServicesCategory)
df2002_nop$location = as.numeric(df2002_nop$location)
df2002_nop$ownership.woman = as.numeric(df2002_nop$ownership.woman)
df2002_nop$ownership.veteran = as.numeric(df2002_nop$ownership.veteran)
df2002_nop$ownership.minority = as.numeric(df2002_nop$ownership.minority)
df2002_nop$ownership.foreign = as.numeric(df2002_nop$ownership.foreign)
df2002_nop$survival.status = as.numeric(df2002_nop$survival.status)
df2002_nop$DEPARTMENT_NAME = as.factor(df2002_nop$DEPARTMENT_NAME)
df2002_nop$AGENCY_NAME = as.factor(df2002_nop$AGENCY_NAME)

df2003_nop$NAICS2 = as.factor(df2003_nop$NAICS2)
df2003_nop$ServicesCategory = as.factor(df2003_nop$ServicesCategory)
df2003_nop$location = as.numeric(df2003_nop$location)
df2003_nop$ownership.woman = as.numeric(df2003_nop$ownership.woman)
df2003_nop$ownership.veteran = as.numeric(df2003_nop$ownership.veteran)
df2003_nop$ownership.minority = as.numeric(df2003_nop$ownership.minority)
df2003_nop$ownership.foreign = as.numeric(df2003_nop$ownership.foreign)
df2003_nop$survival.status = as.numeric(df2003_nop$survival.status)
df2003_nop$DEPARTMENT_NAME = as.factor(df2003_nop$DEPARTMENT_NAME)
df2003_nop$AGENCY_NAME = as.factor(df2003_nop$AGENCY_NAME)

df2004_nop$NAICS2 = as.factor(df2004_nop$NAICS2)
df2004_nop$ServicesCategory = as.factor(df2004_nop$ServicesCategory)
df2004_nop$location = as.numeric(df2004_nop$location)
df2004_nop$ownership.woman = as.numeric(df2004_nop$ownership.woman)
df2004_nop$ownership.veteran = as.numeric(df2004_nop$ownership.veteran)
df2004_nop$ownership.minority = as.numeric(df2004_nop$ownership.minority)
df2004_nop$ownership.foreign = as.numeric(df2004_nop$ownership.foreign)
df2004_nop$survival.status = as.numeric(df2004_nop$survival.status)
df2004_nop$DEPARTMENT_NAME = as.factor(df2004_nop$DEPARTMENT_NAME)
df2004_nop$AGENCY_NAME = as.factor(df2004_nop$AGENCY_NAME)

df2005b_nop$NAICS2 = as.factor(df2005_nop$NAICS2)
df2005_nop$ServicesCategory = as.factor(df2005_nop$ServicesCategory)
df2005_nop$location = as.numeric(df2005_nop$location)
df2005_nop$ownership.woman = as.numeric(df2005_nop$ownership.woman)
df2005_nop$ownership.veteran = as.numeric(df2005_nop$ownership.veteran)
df2005_nop$ownership.minority = as.numeric(df2005_nop$ownership.minority)
df2005_nop$ownership.foreign = as.numeric(df2005_nop$ownership.foreign)
df2005_nop$survival.status = as.numeric(df2005_nop$survival.status)
df2005_nop$DEPARTMENT_NAME = as.factor(df2005_nop$DEPARTMENT_NAME)
df2005_nop$AGENCY_NAME = as.factor(df2005_nop$AGENCY_NAME)

df2006_nop$NAICS2 = as.factor(df2006_nop$NAICS2)
df2006_nop$ServicesCategory = as.factor(df2006_nop$ServicesCategory)
df2006_nop$location = as.numeric(df2006_nop$location)
df2006_nop$ownership.woman = as.numeric(df2006_nop$ownership.woman)
df2006_nop$ownership.veteran = as.numeric(df2006_nop$ownership.veteran)
df2006_nop$ownership.minority = as.numeric(df2006_nop$ownership.minority)
df2006_nop$ownership.foreign = as.numeric(df2006_nop$ownership.foreign)
df2006_nop$survival.status = as.numeric(df2006_nop$survival.status)
df2006_nop$DEPARTMENT_NAME = as.factor(df2006_nop$DEPARTMENT_NAME)
df2006_nop$AGENCY_NAME = as.factor(df2006_nop$AGENCY_NAME)

###Parent ID filter

df2001$NAICS2 = as.factor(df2001$NAICS2)
df2001$ServicesCategory = as.factor(df2001$ServicesCategory)
df2001$location = as.numeric(df2001$location)
df2001$ownership.woman = as.numeric(df2001$ownership.woman)
df2001$ownership.veteran = as.numeric(df2001$ownership.veteran)
df2001$ownership.minority = as.numeric(df2001$ownership.minority)
df2001$ownership.foreign = as.numeric(df2001$ownership.foreign)
df2001$survival.status = as.numeric(df2001$survival.status)
df2001$DEPARTMENT_NAME = as.factor(df2001$DEPARTMENT_NAME)
df2001$AGENCY_NAME = as.factor(df2001$AGENCY_NAME)

df2002$NAICS2 = as.factor(df2002$NAICS2)
df2002$ServicesCategory = as.factor(df2002$ServicesCategory)
df2002$location = as.numeric(df2002$location)
df2002$ownership.woman = as.numeric(df2002$ownership.woman)
df2002$ownership.veteran = as.numeric(df2002$ownership.veteran)
df2002$ownership.minority = as.numeric(df2002$ownership.minority)
df2002$ownership.foreign = as.numeric(df2002$ownership.foreign)
df2002$survival.status = as.numeric(df2002$survival.status)
df2002$DEPARTMENT_NAME = as.factor(df2002$DEPARTMENT_NAME)
df2002$AGENCY_NAME = as.factor(df2002$AGENCY_NAME)

df2003$NAICS2 = as.factor(df2003$NAICS2)
df2003$ServicesCategory = as.factor(df2003$ServicesCategory)
df2003$location = as.numeric(df2003$location)
df2003$ownership.woman = as.numeric(df2003$ownership.woman)
df2003$ownership.veteran = as.numeric(df2003$ownership.veteran)
df2003$ownership.minority = as.numeric(df2003$ownership.minority)
df2003$ownership.foreign = as.numeric(df2003$ownership.foreign)
df2003$survival.status = as.numeric(df2003$survival.status)
df2003$DEPARTMENT_NAME = as.factor(df2003$DEPARTMENT_NAME)
df2003$AGENCY_NAME = as.factor(df2003$AGENCY_NAME)

df2004$NAICS2 = as.factor(df2004$NAICS2)
df2004$ServicesCategory = as.factor(df2004$ServicesCategory)
df2004$location = as.numeric(df2004$location)
df2004$ownership.woman = as.numeric(df2004$ownership.woman)
df2004$ownership.veteran = as.numeric(df2004$ownership.veteran)
df2004$ownership.minority = as.numeric(df2004$ownership.minority)
df2004$ownership.foreign = as.numeric(df2004$ownership.foreign)
df2004$survival.status = as.numeric(df2004$survival.status)
df2004$DEPARTMENT_NAME = as.factor(df2004$DEPARTMENT_NAME)
df2004$AGENCY_NAME = as.factor(df2004$AGENCY_NAME)

df2005$NAICS2 = as.factor(df2005$NAICS2)
df2005$ServicesCategory = as.factor(df2005$ServicesCategory)
df2005$location = as.numeric(df2005$location)
df2005$ownership.woman = as.numeric(df2005$ownership.woman)
df2005$ownership.veteran = as.numeric(df2005$ownership.veteran)
df2005$ownership.minority = as.numeric(df2005$ownership.minority)
df2005$ownership.foreign = as.numeric(df2005$ownership.foreign)
df2005$survival.status = as.numeric(df2005$survival.status)
df2005$DEPARTMENT_NAME = as.factor(df2005$DEPARTMENT_NAME)
df2005$AGENCY_NAME = as.factor(df2005$AGENCY_NAME)

df2006$NAICS2 = as.factor(df2006$NAICS2)
df2006$ServicesCategory = as.factor(df2006$ServicesCategory)
df2006$location = as.numeric(df2006$location)
df2006$ownership.woman = as.numeric(df2006$ownership.woman)
df2006$ownership.veteran = as.numeric(df2006$ownership.veteran)
df2006$ownership.minority = as.numeric(df2006$ownership.minority)
df2006$ownership.foreign = as.numeric(df2006$ownership.foreign)
df2006$survival.status = as.numeric(df2006$survival.status)
df2006$DEPARTMENT_NAME = as.factor(df2006$DEPARTMENT_NAME)
df2006$AGENCY_NAME = as.factor(df2006$AGENCY_NAME)

class(dataset.2001$ServicesCategory)

###Descriptive Stats####

###Survival Rates no parent filter####

##3-year
sr3yr2001_nop <- sum(df2001_nop$years.in.SAM>=3, value=TRUE)/nrow(df2001_nop)
sr3yr2001sm_nop <- sum((df2001_nop$years.in.SAM>=3 & df2001_nop$biz_size == 0), na.rm = TRUE, value = TRUE)/nrow(df2001_nop)
sr3yr2001non_nop <- sum((df2001_nop$years.in.SAM>=3 & df2001_nop$biz_size == 1), na.rm = TRUE, value = TRUE)/nrow(df2001_nop)

sr3yr2002_nop <- sum(df2002_nop$years.in.SAM>=3, value=TRUE)/nrow(df2002_nop) 
sr3yr2002sm_nop <- sum((df2002_nop$years.in.SAM>=3 & df2002_nop$biz_size == 0), na.rm = TRUE, value = TRUE)/nrow(df2002_nop)
sr3yr2002non_nop <- sum((df2002_nop$years.in.SAM>=3 & df2002_nop$biz_size == 1), na.rm = TRUE, value = TRUE)/nrow(df2002_nop)

sr3yr2003_nop <- sum(df2003_nop$years.in.SAM>=3, value=TRUE)/nrow(df2003_nop)
sr3yr2003sm_nop <- sum((df2003_nop$years.in.SAM>=3 & df2003_nop$biz_size == 0), na.rm = TRUE, value = TRUE)/nrow(df2003_nop)
sr3yr2003non_nop <- sum((df2003_nop$years.in.SAM>=3 & df2003_nop$biz_size == 1), na.rm = TRUE, value = TRUE)/nrow(df2003_nop)

sr3yr2004_nop <- sum(df2004_nop$years.in.SAM>=3, value=TRUE)/nrow(df2004_nop) 
sr3yr2004sm_nop <- sum((df2004_nop$years.in.SAM>=3 & df2004_nop$biz_size == 0), na.rm = TRUE, value = TRUE)/nrow(df2004_nop)
sr3yr2004non_nop <- sum((df2004_nop$years.in.SAM>=3 & df2004_nop$biz_size == 1), na.rm = TRUE, value = TRUE)/nrow(df2004_nop)

sr3yr2005_nop <- sum(df2005_nop$years.in.SAM>=3, value=TRUE)/nrow(df2005_nop) 
sr3yr2005sm_nop <- sum((df2005_nop$years.in.SAM>=3 & df2005_nop$biz_size == 0), na.rm = TRUE, value = TRUE)/nrow(df2005_nop)
sr3yr2005non_nop <- sum((df2005_nop$years.in.SAM>=3 & df2005_nop$biz_size == 1), na.rm = TRUE, value = TRUE)/nrow(df2005_nop)

sr3yr2006_nop <- sum(df2006_nop$years.in.SAM>=3, value=TRUE)/nrow(df2006_nop) 
sr3yr2006sm_nop <- sum((df2006_nop$years.in.SAM>=3 & df2006_nop$biz_size == 0), na.rm = TRUE, value = TRUE)/nrow(df2006_nop)
sr3yr2006non_nop <- sum((df2006_nop$years.in.SAM>=3 & df2006_nop$biz_size == 1), na.rm = TRUE, value = TRUE)/nrow(df2006_nop)

##5-year
sr5yr2001_nop <- sum(df2001_nop$years.in.SAM>=5, value=TRUE)/nrow(df2001_nop)
sr5yr2001sm_nop <- sum((df2001_nop$years.in.SAM>=5 & df2001_nop$biz_size == 0), na.rm = TRUE, value = TRUE)/nrow(df2001_nop)
sr5yr2001non_nop <- sum((df2001_nop$years.in.SAM>=5 & df2001_nop$biz_size == 1), na.rm = TRUE, value = TRUE)/nrow(df2001_nop)

sr5yr2002_nop <- sum(df2002_nop$years.in.SAM>=5, value=TRUE)/nrow(df2002_nop) 
sr5yr2002sm_nop <- sum((df2002_nop$years.in.SAM>=5 & df2002_nop$biz_size == 0), na.rm = TRUE, value = TRUE)/nrow(df2002_nop)
sr5yr2002non_nop <- sum((df2002_nop$years.in.SAM>=5 & df2002_nop$biz_size == 1), na.rm = TRUE, value = TRUE)/nrow(df2002_nop)

sr5yr2003_nop <- sum(df2003_nop$years.in.SAM>=5, value=TRUE)/nrow(df2003_nop)
sr5yr2003sm_nop <- sum((df2003_nop$years.in.SAM>=5 & df2003_nop$biz_size == 0), na.rm = TRUE, value = TRUE)/nrow(df2003_nop)
sr5yr2003non_nop <- sum((df2003_nop$years.in.SAM>=5 & df2003_nop$biz_size == 1), na.rm = TRUE, value = TRUE)/nrow(df2003_nop)

sr5yr2004_nop <- sum(df2004_nop$years.in.SAM>=5, value=TRUE)/nrow(df2004_nop) 
sr5yr2004sm_nop <- sum((df2004_nop$years.in.SAM>=5 & df2004_nop$biz_size == 0), na.rm = TRUE, value = TRUE)/nrow(df2004_nop)
sr5yr2004non_nop <- sum((df2004_nop$years.in.SAM>=5 & df2004_nop$biz_size == 1), na.rm = TRUE, value = TRUE)/nrow(df2004_nop)

sr5yr2005_nop <- sum(df2005_nop$years.in.SAM>=5, value=TRUE)/nrow(df2005_nop) 
sr5yr2005sm_nop <- sum((df2005_nop$years.in.SAM>=5 & df2005_nop$biz_size == 0), na.rm = TRUE, value = TRUE)/nrow(df2005_nop)
sr5yr2005non_nop <- sum((df2005_nop$years.in.SAM>=5 & df2005_nop$biz_size == 1), na.rm = TRUE, value = TRUE)/nrow(df2005_nop)

sr5yr2006_nop <- sum(df2006_nop$years.in.SAM>=5, value=TRUE)/nrow(df2006_nop) 
sr5yr2006sm_nop <- sum((df2006_nop$years.in.SAM>=5 & df2006_nop$biz_size == 0), na.rm = TRUE, value = TRUE)/nrow(df2006_nop)
sr5yr2006non_nop <- sum((df2006_nop$years.in.SAM>=5 & df2006_nop$biz_size == 1), na.rm = TRUE, value = TRUE)/nrow(df2006_nop)


##10-year
sr10yr2001_nop <- (sum(df2001_nop$years.in.SAM>=10, value=TRUE))/nrow(df2001_nop) 
sr10yr2001sm_nop <- sum((df2001_nop$years.in.SAM>=10 & df2001_nop$biz_size == 0), na.rm = TRUE, value = TRUE)/893
sr10yr2001non_nop <- sum((df2001_nop$years.in.SAM>=10 & df2001_nop$biz_size == 1), na.rm = TRUE, value = TRUE)/893


sr10yr2002_nop <- sum(df2002_nop$years.in.SAM>=10, value=TRUE)/nrow(df2002_nop) 
sr10yr2002sm_nop <- sum((df2002_nop$years.in.SAM>=10 & df2002_nop$biz_size == 0), na.rm = TRUE, value = TRUE)/nrow(df2002_nop)
sr10yr2002non_nop <- sum((df2002_nop$years.in.SAM>=10 & df2002_nop$biz_size == 1), na.rm = TRUE, value = TRUE)/nrow(df2002_nop)

sr10yr2003_nop <- sum(df2003_nop$years.in.SAM>=10, value=TRUE)/nrow(df2003_nop)
sr10yr2003sm_nop <- sum((df2003_nop$years.in.SAM>=10 & df2003_nop$biz_size == 0), na.rm = TRUE, value = TRUE)/nrow(df2003_nop)
sr10yr2003non_nop <- sum((df2003_nop$years.in.SAM>=10 & df2003_nop$biz_size == 1), na.rm = TRUE, value = TRUE)/nrow(df2003_nop)

sr10yr2004_nop <- sum(df2004_nop$years.in.SAM>=10, value=TRUE)/nrow(df2004_nop) 
sr10yr2004sm_nop <- sum((df2004_nop$years.in.SAM>=10 & df2004_nop$biz_size == 0), na.rm = TRUE, value = TRUE)/nrow(df2004_nop)
sr10yr2004non_nop <- sum((df2004_nop$years.in.SAM>=10 & df2004_nop$biz_size == 1), na.rm = TRUE, value = TRUE)/nrow(df2004_nop)

sr10yr2005_nop <- sum(df2005_nop$years.in.SAM>=10, value=TRUE)/nrow(df2005_nop) 
sr10yr2005sm_nop <- sum((df2005_nop$years.in.SAM>=10 & df2005_nop$biz_size == 0), na.rm = TRUE, value = TRUE)/nrow(df2005_nop)
sr10yr2005non_nop <- sum((df2005_nop$years.in.SAM>=10 & df2005_nop$biz_size == 1), na.rm = TRUE, value = TRUE)/nrow(df2005_nop)

sr10yr2006_nop <- sum(df2006_nop$years.in.SAM>=10, value=TRUE)/nrow(df2006_nop) 
sr10yr2006sm_nop <- sum((df2006_nop$years.in.SAM>=10 & df2006_nop$biz_size == 0), na.rm = TRUE, value = TRUE)/nrow(df2006_nop)
sr10yr2006non_nop <- sum((df2006_nop$years.in.SAM>=10 & df2006_nop$biz_size == 1), na.rm = TRUE, value = TRUE)/nrow(df2006_nop)

##bound all survival rates together and reformated dataframe

survivalrate_nop <- cbind(sr3yr2001_nop, sr3yr2001sm_nop, sr3yr2001non_nop, sr5yr2001_nop, sr5yr2001sm_nop, sr5yr2001non_nop, 
  sr10yr2001_nop, sr10yr2001sm_nop, sr10yr2001non_nop,
  sr3yr2002_nop, sr3yr2002sm_nop, sr3yr2002non_nop, sr5yr2002_nop, sr5yr2002sm_nop, sr5yr2002non_nop, 
  sr10yr2002_nop, sr10yr2002sm_nop, sr10yr2002non_nop,
  sr3yr2003_nop, sr3yr2003sm_nop, sr3yr2003non_nop, sr5yr2003_nop, sr5yr2003sm_nop, sr5yr2003non_nop, 
  sr10yr2003_nop, sr10yr2003sm_nop, sr10yr2003non_nop,
  sr3yr2004_nop, sr3yr2004sm_nop, sr3yr2004non_nop, sr5yr2004_nop, sr5yr2004sm_nop, sr5yr2004non_nop, 
  sr10yr2004_nop, sr10yr2004sm_nop, sr10yr2004non_nop,
  sr3yr2005_nop, sr3yr2005sm_nop, sr3yr2005non_nop, sr5yr2005_nop, sr5yr2005sm_nop, sr5yr2005non_nop, 
  sr10yr2005_nop, sr10yr2005sm_nop, sr10yr2005non_nop,
  sr3yr2006_nop, sr3yr2006sm_nop, sr3yr2006non_nop, sr5yr2006_nop, sr5yr2006sm_nop, sr5yr2006non_nop, 
  sr10yr2006_nop, sr10yr2006sm_nop, sr10yr2006non_nop)
srnames_nop <- c("3yr_2001_all_nop", "3yr_2001_sm_nop", 
  "3yr_2001_non_nop", "5yr_2001_all_nop", "5yr_2001_sm_nop", "5yr_2001_non_nop", "10yr_2001_all_nop", 
  "10yr_2001_sm_nop", "10yr_2001_non_nop",
  "3yr_2002_all_nop", "3yr_2002_sm_nop", "3yr_2002_non_nop", "5yr_2002_all_nop", "5yr_2002_sm_nop", "5yr_2002_non_nop", 
  "10yr_2002_all_nop", "10yr_2002_sm_nop", "10yr_2002_non_nop",
  "3yr_2003_all_nop", "3yr_2003_sm_nop", "3yr_2003_non_nop", "5yr_2003_all_nop", "5yr_2003_sm_nop", "5yr_2003_non_nop", 
  "10yr_2003_all_nop", "10yr_2003_sm_nop", "10yr_2003_non_nop",
  "3yr_2004_all_nop", "3yr_2004_sm_nop", "3yr_2004_non_nop", "5yr_2004_all_nop", "5yr_2004_sm_nop", "5yr_2004_non_nop", 
  "10yr_2004_all_nop", "10yr_2004_sm_nop", "10yr_2004_non_nop",
  "3yr_2005_all_nop", "3yr_2005_sm_nop", "3yr_2005_non_nop", "5yr_2005_all_nop", "5yr_2005_sm_nop", "5yr_2005_non_nop", 
  "10yr2005_all_nop", "10yr_2005_sm_nop", "10yr_2005_non_nop",
  "3yr_2006_all_nop", "3yr_2006_sm_nop", "3yr_2006_non_nop", "5yr_2006_all_nop", "5yr_2006_sm_nop", "5yr_2006_non_nop", 
  "10yr_2006_all_nop", "10yr_2006_sm_nop", "10yr_2006_non_nop")
 colnames(survivalrate_nop) <- srnames_nop
 
sr_nop <- as.data.frame(survivalrate_nop)

survival.rates_nop <- sr_nop %>% 
  gather("3yr_2001_all_nop":"10yr_2006_non_nop", key = "time_year_type", value = "survivalrate_nop") %>% 
  separate(time_year_type, into = c("time", "year", "type"), sep = "_") %>% 
  arrange(time, year) %>% 
  unite("time_type", time, type, sep = "_") 

survival.rates_nop$time_type[18] <- "10yr_all"
survival.rates_nop$year[18] <- 2005
  
survival.rates_nop <- survival.rates_nop %>% 
  spread(key = "time_type", value = "survivalrate_nop")
  
survival.rates_nop[, c(1, 5, 7, 6, 8, 10, 9, 2, 4, 3)]

###Survival Rates with parent filter####

##3-year
sr3yr2001 <- sum(df2001$years.in.SAM>=3, value=TRUE)/nrow(df2001)
sr3yr2001sm <- sum((df2001$years.in.SAM>=3 & df2001$biz_size == 0), na.rm = TRUE, value = TRUE)/nrow(df2001)
sr3yr2001non <- sum((df2001$years.in.SAM>=3 & df2001$biz_size == 1), na.rm = TRUE, value = TRUE)/nrow(df2001)

sr3yr2002 <- sum(df2002$years.in.SAM>=3, value=TRUE)/nrow(df2002) 
sr3yr2002sm <- sum((df2002$years.in.SAM>=3 & df2002$biz_size == 0), na.rm = TRUE, value = TRUE)/nrow(df2002)
sr3yr2002non <- sum((df2002$years.in.SAM>=3 & df2002$biz_size == 1), na.rm = TRUE, value = TRUE)/nrow(df2002)

sr3yr2003 <- sum(df2003$years.in.SAM>=3, value=TRUE)/nrow(df2003)
sr3yr2003sm <- sum((df2003$years.in.SAM>=3 & df2003$biz_size == 0), na.rm = TRUE, value = TRUE)/nrow(df2003)
sr3yr2003non <- sum((df2003$years.in.SAM>=3 & df2003$biz_size == 1), na.rm = TRUE, value = TRUE)/nrow(df2003)

sr3yr2004 <- sum(df2004$years.in.SAM>=3, value=TRUE)/nrow(df2004) 
sr3yr2004sm <- sum((df2004$years.in.SAM>=3 & df2004$biz_size == 0), na.rm = TRUE, value = TRUE)/nrow(df2004)
sr3yr2004non <- sum((df2004$years.in.SAM>=3 & df2004$biz_size == 1), na.rm = TRUE, value = TRUE)/nrow(df2004)

sr3yr2005 <- sum(df2005$years.in.SAM>=3, value=TRUE)/nrow(df2005) 
sr3yr2005sm <- sum((df2005$years.in.SAM>=3 & df2005$biz_size == 0), na.rm = TRUE, value = TRUE)/nrow(df2005)
sr3yr2005non <- sum((df2005$years.in.SAM>=3 & df2005$biz_size == 1), na.rm = TRUE, value = TRUE)/nrow(df2005)

sr3yr2006 <- sum(df2006$years.in.SAM>=3, value=TRUE)/nrow(df2006) 
sr3yr2006sm <- sum((df2006$years.in.SAM>=3 & df2006$biz_size == 0), na.rm = TRUE, value = TRUE)/nrow(df2006)
sr3yr2006non <- sum((df2006$years.in.SAM>=3 & df2006$biz_size == 1), na.rm = TRUE, value = TRUE)/nrow(df2006)

##5-year
sr5yr2001 <- sum(df2001$years.in.SAM>=5, value=TRUE)/nrow(df2001)
sr5yr2001sm <- sum((df2001$years.in.SAM>=5 & df2001$biz_size == 0), na.rm = TRUE, value = TRUE)/nrow(df2001)
sr5yr2001non <- sum((df2001$years.in.SAM>=5 & df2001$biz_size == 1), na.rm = TRUE, value = TRUE)/nrow(df2001)

sr5yr2002 <- sum(df2002$years.in.SAM>=5, value=TRUE)/nrow(df2002) 
sr5yr2002sm <- sum((df2002$years.in.SAM>=5 & df2002$biz_size == 0), na.rm = TRUE, value = TRUE)/nrow(df2002)
sr5yr2002non <- sum((df2002$years.in.SAM>=5 & df2002$biz_size == 1), na.rm = TRUE, value = TRUE)/nrow(df2002)

sr5yr2003 <- sum(df2003$years.in.SAM>=5, value=TRUE)/nrow(df2003)
sr5yr2003sm <- sum((df2003$years.in.SAM>=5 & df2003$biz_size == 0), na.rm = TRUE, value = TRUE)/nrow(df2003)
sr5yr2003non <- sum((df2003$years.in.SAM>=5 & df2003$biz_size == 1), na.rm = TRUE, value = TRUE)/nrow(df2003)

sr5yr2004 <- sum(df2004$years.in.SAM>=5, value=TRUE)/nrow(df2004) 
sr5yr2004sm <- sum((df2004$years.in.SAM>=5 & df2004$biz_size == 0), na.rm = TRUE, value = TRUE)/nrow(df2004)
sr5yr2004non <- sum((df2004$years.in.SAM>=5 & df2004$biz_size == 1), na.rm = TRUE, value = TRUE)/nrow(df2004)

sr5yr2005 <- sum(df2005$years.in.SAM>=5, value=TRUE)/nrow(df2005) 
sr5yr2005sm <- sum((df2005$years.in.SAM>=5 & df2005$biz_size == 0), na.rm = TRUE, value = TRUE)/nrow(df2005)
sr5yr2005non <- sum((df2005$years.in.SAM>=5 & df2005$biz_size == 1), na.rm = TRUE, value = TRUE)/nrow(df2005)

sr5yr2006 <- sum(df2006$years.in.SAM>=5, value=TRUE)/nrow(df2006) 
sr5yr2006sm <- sum((df2006$years.in.SAM>=5 & df2006$biz_size == 0), na.rm = TRUE, value = TRUE)/nrow(df2006)
sr5yr2006non <- sum((df2006$years.in.SAM>=5 & df2006$biz_size == 1), na.rm = TRUE, value = TRUE)/nrow(df2006)


##10-year
sr10yr2001 <- (sum(df2001$years.in.SAM>=10, value=TRUE))/nrow(df2001)
sr10yr2001sm <- sum((df2001$years.in.SAM>=10 & df2001$biz_size == 0), na.rm = TRUE, value = TRUE)/nrow(df2001)
sr10yr2001non <- sum((df2001$years.in.SAM>=10 & df2001$biz_size == 1), na.rm = TRUE, value = TRUE)/nrow(df2001)

sr10yr2002 <- sum(df2002$years.in.SAM>=10, value=TRUE)/nrow(df2002) 
sr10yr2002sm <- sum((df2002$years.in.SAM>=10 & df2002$biz_size == 0), na.rm = TRUE, value = TRUE)/nrow(df2002)
sr10yr2002non <- sum((df2002$years.in.SAM>=10 & df2002$biz_size == 1), na.rm = TRUE, value = TRUE)/nrow(df2002)

sr10yr2003 <- sum(df2003$years.in.SAM>=10, value=TRUE)/nrow(df2003)
sr10yr2003sm <- sum((df2003$years.in.SAM>=10 & df2003$biz_size == 0), na.rm = TRUE, value = TRUE)/nrow(df2003)
sr10yr2003non <- sum((df2003$years.in.SAM>=10 & df2003$biz_size == 1), na.rm = TRUE, value = TRUE)/nrow(df2003)

sr10yr2004 <- sum(df2004$years.in.SAM>=10, value=TRUE)/nrow(df2004) 
sr10yr2004sm <- sum((df2004$years.in.SAM>=10 & df2004$biz_size == 0), na.rm = TRUE, value = TRUE)/nrow(df2004)
sr10yr2004non <- sum((df2004$years.in.SAM>=10 & df2004$biz_size == 1), na.rm = TRUE, value = TRUE)/nrow(df2004)

sr10yr2005 <- sum(df2005$years.in.SAM>=10, value=TRUE)/nrow(df2005) 
sr10yr2005sm <- sum((df2005$years.in.SAM>=10 & df2005$biz_size == 0), na.rm = TRUE, value = TRUE)/nrow(df2005)
sr10yr2005non <- sum((df2005$years.in.SAM>=10 & df2005$biz_size == 1), na.rm = TRUE, value = TRUE)/nrow(df2005)

sr10yr2006 <- sum(df2006$years.in.SAM>=10, value=TRUE)/nrow(df2006) 
sr10yr2006sm <- sum((df2006$years.in.SAM>=10 & df2006$biz_size == 0), na.rm = TRUE, value = TRUE)/nrow(df2006)
sr10yr2006non <- sum((df2006$years.in.SAM>=10 & df2006$biz_size == 1), na.rm = TRUE, value = TRUE)/nrow(df2006)

##bound all survival rates together and reformated dataframe

survivalrate <- cbind(sr3yr2001, sr3yr2001sm, sr3yr2001non, sr5yr2001, sr5yr2001sm, sr5yr2001non, 
                      sr10yr2001, sr10yr2001sm, sr10yr2001non,
                      sr3yr2002, sr3yr2002sm, sr3yr2002non, sr5yr2002, sr5yr2002sm, sr5yr2002non, 
                      sr10yr2002, sr10yr2002sm, sr10yr2002non,
                      sr3yr2003, sr3yr2003sm, sr3yr2003non, sr5yr2003, sr5yr2003sm, sr5yr2003non, 
                      sr10yr2003, sr10yr2003sm, sr10yr2003non,
                      sr3yr2004, sr3yr2004sm, sr3yr2004non, sr5yr2004, sr5yr2004sm, sr5yr2004non, 
                      sr10yr2004, sr10yr2004sm, sr10yr2004non,
                      sr3yr2005, sr3yr2005sm, sr3yr2005non, sr5yr2005, sr5yr2005sm, sr5yr2005non, 
                      sr10yr2005, sr10yr2005sm, sr10yr2005non,
                      sr3yr2006, sr3yr2006sm, sr3yr2006non, sr5yr2006, sr5yr2006sm, sr5yr2006non, 
                      sr10yr2006, sr10yr2006sm, sr10yr2006non)
srnames <- c("3yr_2001_all", "3yr_2001_sm", 
             "3yr_2001_non", "5yr_2001_all", "5yr_2001_sm", "5yr_2001_non", "10yr_2001_all", 
             "10yr_2001_sm", "10yr_2001_non",
             "3yr_2002_all", "3yr_2002_sm", "3yr_2002_non", "5yr_2002_all", "5yr_2002_sm", "5yr_2002_non", 
             "10yr_2002_all", "10yr_2002_sm", "10yr_2002_non",
             "3yr_2003_all", "3yr_2003_sm", "3yr_2003_non", "5yr_2003_all", "5yr_2003_sm", "5yr_2003_non", 
             "10yr_2003_all", "10yr_2003_sm", "10yr_2003_non",
             "3yr_2004_all", "3yr_2004_sm", "3yr_2004_non", "5yr_2004_all", "5yr_2004_sm", "5yr_2004_non", 
             "10yr_2004_all", "10yr_2004_sm", "10yr_2004_non",
             "3yr_2005_all", "3yr_2005_sm", "3yr_2005_non", "5yr_2005_all", "5yr_2005_sm", "5yr_2005_non", 
             "10yr2005_all", "10yr_2005_sm", "10yr_2005_non",
             "3yr_2006_all", "3yr_2006_sm", "3yr_2006_non", "5yr_2006_all", "5yr_2006_sm", "5yr_2006_non", 
             "10yr_2006_all", "10yr_2006_sm", "10yr_2006_non")
colnames(survivalrate) <- srnames

sr <- as.data.frame(survivalrate)

survival.rates <- sr %>% 
  gather("3yr_2001_all":"10yr_2006_non", key = "time_year_type", value = "survivalrate") %>% 
  separate(time_year_type, into = c("time", "year", "type"), sep = "_") %>% 
  arrange(time, year) %>% 
  unite("time_type", time, type, sep = "_") 

survival.rates$time_type[18] <- "10yr_all"
survival.rates$year[18] <- 2005

survival.rates <- survival.rates %>% 
  spread(key = "time_type", value = "survivalrate")

survival.rates[, c(1, 5, 7, 6, 8, 10, 9, 2, 4, 3)]


####Descriptives: Firm-level characteristics ####
#Focusing on 2001 Sample: descriptives for firm-level independent variables (firm age, firm ownership,
#firm location, contract obligations, PSC code) and t-test testing the differences between small and non-small
#new entrants means for each independent var for all 2001 and then those that survived after 3 yrs, 5 yrs and 10
#yrs. 

##exporting to csv for Sam

write.csv(df2001_nop, file = "df2001_nop.csv")
write.csv(df2002_nop, file = "df2002_nop.csv")
write.csv(df2003_nop, file = "df2003_nop.csv")
write.csv(df2004_nop, file = "df2004_nop.csv")
write.csv(df2005_nop, file = "df2005_nop.csv")
write.csv(df2006_nop, file = "df2006_nop.csv")


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

