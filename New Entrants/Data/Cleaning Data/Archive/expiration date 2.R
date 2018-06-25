library(openxlsx)
library(httr)
library(jsonlite)
library(plyr)
library(data.table)
library(tidyverse)
library(lubridate)

##SD: expiration date by signeddate####

panel_data <- read_csv("Panel Data 2001-2016 -nop.csv")
full_fpds <- read_csv("SAM Data merged with FPDS, exp2000-2019.csv")

expiration2_0116 <- panel_data %>% 
  left_join(full_fpds, by = "duns") %>% 
  group_by(duns) %>% 
  filter(!is.na(signeddate)) %>% 
  mutate(maxyear = max(signeddate), na.rm = TRUE) %>% 
  mutate(exp2 = maxyear %m+% years(1)) %>% 
  mutate(months.in.SAM = ((year(exp2) - year(registrationDate)) * 12) + month(exp2) - month(registrationDate)) %>% 
  mutate(years_in_SAM = months.in.SAM/12) %>% 
  mutate(three_year = years_in_SAM>=3, "YES","NO") %>% 
  mutate(five_year = years_in_SAM>=5, "YES","NO") %>% 
  mutate(ten_year = years_in_SAM>=10, "YES","NO") %>%
  mutate(signyear = year(signeddate)) %>% 
  select(duns, biz_size, years_in_SAM, three_year, five_year,ten_year, registrationDate, exp2, survival.status, signyear) 

exp2.unique = expiration2_0116[!duplicated(expiration2_0116),]

a <- df2001_nop$duns[1:298]
b <- a[!duplicated(a)]

df2001_nop <- exp2.unique %>% 
  filter(year(registrationDate) == 2001) %>% 
  arrange(duns, signyear)
df2002_nop <- filter(exp2.unique, year(registrationDate) == 2002)
df2003_nop <- filter(exp2.unique, year(registrationDate) == 2003)
df2004_nop <- filter(exp2.unique, year(registrationDate) == 2004)
df2005_nop <- filter(exp2.unique, year(registrationDate) == 2005)
df2006_nop <- filter(exp2.unique, year(registrationDate) == 2006)

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

df2005_nop$NAICS2 = as.factor(df2005_nop$NAICS2)
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

sr3yr2001_nop <- sum(df2001_nop$years_in_SAM>=3)/nrow(df2001_nop)
sr3yr2001sm_nop <- sum((df2001_nop$years_in_SAM>=3 & df2001_nop$biz_size == 0), na.rm = TRUE)/sum(df2001_nop$biz_size == 0, na.rm = TRUE)
sr3yr2001non_nop <- sum((df2001_nop$years_in_SAM>=3 & df2001_nop$biz_size == 1), na.rm = TRUE)/sum(df2001_nop$biz_size == 1, na.rm = TRUE)

sr3yr2002_nop <- sum(df2002_nop$years_in_SAM>=3)/nrow(df2002_nop) 
sr3yr2002sm_nop <- sum((df2002_nop$years_in_SAM>=3 & df2002_nop$biz_size == 0), na.rm = TRUE)/sum(df2002_nop$biz_size == 0, na.rm = TRUE)
sr3yr2002non_nop <- sum((df2002_nop$years_in_SAM>=3 & df2002_nop$biz_size == 1), na.rm = TRUE)/sum(df2002_nop$biz_size == 1, na.rm = TRUE)

sr3yr2003_nop <- sum(df2003_nop$years_in_SAM>=3)/nrow(df2003_nop)
sr3yr2003sm_nop <- sum((df2003_nop$years_in_SAM>=3 & df2003_nop$biz_size == 0), na.rm = TRUE)/sum(df2003_nop$biz_size == 0, na.rm = TRUE)
sr3yr2003non_nop <- sum((df2003_nop$years_in_SAM>=3 & df2003_nop$biz_size == 1), na.rm = TRUE)/sum(df2003_nop$biz_size == 1, na.rm = TRUE)

sr3yr2004_nop <- sum(df2004_nop$years_in_SAM>=3)/nrow(df2004_nop) 
sr3yr2004sm_nop <- sum((df2004_nop$years_in_SAM>=3 & df2004_nop$biz_size == 0), na.rm = TRUE)/sum(df2004_nop$biz_size == 0, na.rm = TRUE)
sr3yr2004non_nop <- sum((df2004_nop$years_in_SAM>=3 & df2004_nop$biz_size == 1), na.rm = TRUE)/sum(df2004_nop$biz_size == 1, na.rm = TRUE)

sr3yr2005_nop <- sum(df2005_nop$years_in_SAM>=3)/nrow(df2005_nop) 
sr3yr2005sm_nop <- sum((df2005_nop$years_in_SAM>=3 & df2005_nop$biz_size == 0), na.rm = TRUE)/sum(df2005_nop$biz_size == 0, na.rm = TRUE)
sr3yr2005non_nop <- sum((df2005_nop$years_in_SAM>=3 & df2005_nop$biz_size == 1), na.rm = TRUE)/sum(df2005_nop$biz_size == 1, na.rm = TRUE)

sr3yr2006_nop <- sum(df2006_nop$years_in_SAM>=3)/nrow(df2006_nop) 
sr3yr2006sm_nop <- sum((df2006_nop$years_in_SAM>=3 & df2006_nop$biz_size == 0), na.rm = TRUE)/sum(df2006_nop$biz_size == 0, na.rm = TRUE)
sr3yr2006non_nop <- sum((df2006_nop$years_in_SAM>=3 & df2006_nop$biz_size == 1), na.rm = TRUE)/sum(df2006_nop$biz_size == 1, na.rm = TRUE)

##5-year
sr5yr2001_nop <- sum(df2001_nop$years_in_SAM>=5)/nrow(df2001_nop)
sr5yr2001sm_nop <- sum((df2001_nop$years_in_SAM>=5 & df2001_nop$biz_size == 0), na.rm = TRUE)/sum(df2001_nop$biz_size == 0, na.rm = TRUE)
sr5yr2001non_nop <- sum((df2001_nop$years_in_SAM>=5 & df2001_nop$biz_size == 1), na.rm = TRUE)/sum(df2001_nop$biz_size == 1, na.rm = TRUE)

sr5yr2002_nop <- sum(df2002_nop$years_in_SAM>=5)/nrow(df2002_nop) 
sr5yr2002sm_nop <- sum((df2002_nop$years_in_SAM>=5 & df2002_nop$biz_size == 0), na.rm = TRUE)/sum(df2002_nop$biz_size == 0, na.rm = TRUE)
sr5yr2002non_nop <- sum((df2002_nop$years_in_SAM>=5 & df2002_nop$biz_size == 1), na.rm = TRUE)/sum(df2002_nop$biz_size == 1, na.rm = TRUE)

sr5yr2003_nop <- sum(df2003_nop$years_in_SAM>=5)/nrow(df2003_nop)
sr5yr2003sm_nop <- sum((df2003_nop$years_in_SAM>=5 & df2003_nop$biz_size == 0), na.rm = TRUE)/sum(df2003_nop$biz_size == 0, na.rm = TRUE)
sr5yr2003non_nop <- sum((df2003_nop$years_in_SAM>=5 & df2003_nop$biz_size == 1), na.rm = TRUE)/sum(df2003_nop$biz_size == 1, na.rm = TRUE)

sr5yr2004_nop <- sum(df2004_nop$years_in_SAM>=5)/nrow(df2004_nop) 
sr5yr2004sm_nop <- sum((df2004_nop$years_in_SAM>=5 & df2004_nop$biz_size == 0), na.rm = TRUE)/sum(df2004_nop$biz_size == 0, na.rm = TRUE)
sr5yr2004non_nop <- sum((df2004_nop$years_in_SAM>=5 & df2004_nop$biz_size == 1), na.rm = TRUE)/sum(df2004_nop$biz_size == 1, na.rm = TRUE)

sr5yr2005_nop <- sum(df2005_nop$years_in_SAM>=5)/nrow(df2005_nop) 
sr5yr2005sm_nop <- sum((df2005_nop$years_in_SAM>=5 & df2005_nop$biz_size == 0), na.rm = TRUE)/sum(df2005_nop$biz_size == 0, na.rm = TRUE)
sr5yr2005non_nop <- sum((df2005_nop$years_in_SAM>=5 & df2005_nop$biz_size == 1), na.rm = TRUE)/sum(df2005_nop$biz_size == 1, na.rm = TRUE)

sr5yr2006_nop <- sum(df2006_nop$years_in_SAM>=5)/nrow(df2006_nop) 
sr5yr2006sm_nop <- sum((df2006_nop$years_in_SAM>=5 & df2006_nop$biz_size == 0), na.rm = TRUE)/sum(df2006_nop$biz_size == 0, na.rm = TRUE)
sr5yr2006non_nop <- sum((df2006_nop$years_in_SAM>=5 & df2006_nop$biz_size == 1), na.rm = TRUE)/sum(df2006_nop$biz_size == 1, na.rm = TRUE)


##10-year
sr10yr2001_nop <- (sum(df2001_nop$years_in_SAM>=10))/nrow(df2001_nop) 
sr10yr2001sm_nop <- sum((df2001_nop$years_in_SAM>=10 & df2001_nop$biz_size == 0), na.rm = TRUE)/sum(df2001_nop$biz_size == 0, na.rm = TRUE)
sr10yr2001non_nop <- sum((df2001_nop$years_in_SAM>=10 & df2001_nop$biz_size == 1), na.rm = TRUE)/sum(df2002_nop$biz_size == 1, na.rm = TRUE)


sr10yr2002_nop <- sum(df2002_nop$years_in_SAM>=10)/nrow(df2002_nop) 
sr10yr2002sm_nop <- sum((df2002_nop$years_in_SAM>=10 & df2002_nop$biz_size == 0), na.rm = TRUE)/sum(df2002_nop$biz_size == 0, na.rm = TRUE)
sr10yr2002non_nop <- sum((df2002_nop$years_in_SAM>=10 & df2002_nop$biz_size == 1), na.rm = TRUE)/sum(df2002_nop$biz_size == 1, na.rm = TRUE)

sr10yr2003_nop <- sum(df2003_nop$years_in_SAM>=10)/nrow(df2003_nop)
sr10yr2003sm_nop <- sum((df2003_nop$years_in_SAM>=10 & df2003_nop$biz_size == 0), na.rm = TRUE)/sum(df2003_nop$biz_size == 0, na.rm = TRUE)
sr10yr2003non_nop <- sum((df2003_nop$years_in_SAM>=10 & df2003_nop$biz_size == 1), na.rm = TRUE)/sum(df2003_nop$biz_size == 1, na.rm = TRUE)

sr10yr2004_nop <- sum(df2004_nop$years_in_SAM>=10)/nrow(df2004_nop) 
sr10yr2004sm_nop <- sum((df2004_nop$years_in_SAM>=10 & df2004_nop$biz_size == 0), na.rm = TRUE)/sum(df2004_nop$biz_size == 0, na.rm = TRUE)
sr10yr2004non_nop <- sum((df2004_nop$years_in_SAM>=10 & df2004_nop$biz_size == 1), na.rm = TRUE)/sum(df2004_nop$biz_size == 1, na.rm = TRUE)

sr10yr2005_nop <- sum(df2005_nop$years_in_SAM>=10)/nrow(df2005_nop) 
sr10yr2005sm_nop <- sum((df2005_nop$years_in_SAM>=10 & df2005_nop$biz_size == 0), na.rm = TRUE)/sum(df2005_nop$biz_size == 0, na.rm = TRUE)
sr10yr2005non_nop <- sum((df2005_nop$years_in_SAM>=10 & df2005_nop$biz_size == 1), na.rm = TRUE)/sum(df2005_nop$biz_size == 1, na.rm = TRUE)

sr10yr2006_nop <- sum(df2006_nop$years_in_SAM>=10)/nrow(df2006_nop) 
sr10yr2006sm_nop <- sum((df2006_nop$years_in_SAM>=10 & df2006_nop$biz_size == 0), na.rm = TRUE)/sum(df2006_nop$biz_size == 0, na.rm = TRUE)
sr10yr2006non_nop <- sum((df2006_nop$years_in_SAM>=10 & df2006_nop$biz_size == 1), na.rm = TRUE)/sum(df2006_nop$biz_size == 1, na.rm = TRUE)

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
write.csv(survival.rates_nop, "survival rates reg 2000-2006 - signeddate exp.csv")

exp <- read_csv("survival rates reg2000-2011-no parent filter.csv")
signed <- read_csv("survival rates reg 2000-2006 - signeddate exp.csv")
 

x <- exp - signed

###ED: expiration Date by expirationDate####

Panel_data_ED <- read_csv("Panel data 2001-2016 -nop.csv")

three.01 <- Panel_data_ED %>% 
  left_join(full_fpds[c("duns","signeddate", "registrationDate")], by = "duns") %>% 
  filter(year(registrationDate) == 2001) %>% 
  filter(three.year == "TRUE") %>% 
  filter(year(signeddate) == 2004)
  
three.unique = three.01[!duplicated(three.01["duns"]),]

five.01 <-Panel_data_ED %>% 
  left_join(full_fpds[c("duns","signeddate", "registrationDate")], by = "duns") %>% 
  filter(year(registrationDate) == 2001) %>% 
  filter(five.year == "TRUE") %>% 
  filter(year(signeddate) == 2006)

five.unique = five.01[!duplicated(five.01["duns"]),]

ten.01 <-Panel_data_ED %>% 
  left_join(full_fpds[c("duns","signeddate", "registrationDate")], by = "duns") %>% 
  filter(year(registrationDate) == 2001) %>% 
  filter(ten.year == "TRUE") %>% 
  filter(year(signeddate) == 2011)

ten.unique = ten.01[!duplicated(ten.01["duns"]),]

all.three.01 <- Panel_data_ED %>% 
  left_join(full_fpds[c("duns","signeddate", "registrationDate")], by = "duns") %>% 
  filter(year(registrationDate) == 2001) %>% 
  filter(three.year == "TRUE")

all.three.unique = all.three.01[!duplicated(all.three.01["duns"]),]

all.five.01 <-Panel_data_ED %>% 
  left_join(full_fpds[c("duns","signeddate", "registrationDate")], by = "duns") %>% 
  filter(year(registrationDate) == 2001) %>% 
  filter(five.year == "TRUE")

all.five.unique = all.five.01[!duplicated(all.five.01["duns"]),]

all.ten.01 <-Panel_data_ED %>% 
  left_join(full_fpds[c("duns","signeddate", "registrationDate")], by = "duns") %>% 
  filter(year(registrationDate) == 2001) %>% 
  filter(ten.year == "TRUE")

all.ten.unique = all.ten.01[!duplicated(all.ten.01["duns"]),]

x3 <- nrow(three.unique)
all3 <- nrow(all.three.unique)
x5 <- nrow(five.unique)
all5 <- nrow(all.five.unique)
x10 <- nrow(ten.unique)
all10 <- nrow(all.ten.unique)

df.all <- as.data.frame(c(all3, all5, all10))
colnames(df.all) = "all firms"
df.x <- as.data.frame(c(x3,x5,x10))
colnames(df.x) = "current contract"
df <- cbind(df.all, df.x)
rownames(df) <- c("3","5","10")

write.csv(df, "allvscurrent.csv")
