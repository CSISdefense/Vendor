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
library(ggrepel)


#setwd("K:/2018-01 NPS New Entrants/Data/Data/Cleaned Data/FPDS")

setwd("K:/2018-01 NPS New Entrants/Data/Data/Cleaning data/FPDS")

load(file = "FPDS_datapull_all_v2.Rda")
length(unique(FPDS_cleaned_unique$Dunsnumber)) == nrow(FPDS_cleaned_unique)

#******************************************************************
########################Count number of new entrants in each year! ################################
#******************************************************************


##fpds data
registrationyear_count <- table(SAM_and_FPDS_uniqueDuns$registrationYear)

registrationyear_count


##sam data
load(file = "SAM_datapull_all.Rda")
registrationyear_count <- table(datapull_all$registrationYear)

registrationyear_count


##change top_small_biz to 1= small biz and 0 = non-small biz

str(SAM_and_FPDS_uniqueDuns$top_small_biz)

table(SAM_and_FPDS_uniqueDuns$top_small_biz)

SAM_and_FPDS_uniqueDuns <- SAM_and_FPDS_uniqueDuns[!(SAM_and_FPDS_uniqueDuns$top_small_biz==":"), ]

table(SAM_and_FPDS_uniqueDuns$top_small_biz)

##make biz_size binary
SAM_and_FPDS_uniqueDuns$top_smallbiz_bin <- revalue(SAM_and_FPDS_uniqueDuns$top_small_biz, c("S"="1", "O"="0"))

str(SAM_and_FPDS_uniqueDuns$top_smallbiz_bin)

table(SAM_and_FPDS_uniqueDuns$top_smallbiz_bin)

##drop years less than 2001

SAM_and_FPDS_uniqueDuns <- SAM_and_FPDS_uniqueDuns[!(SAM_and_FPDS_uniqueDuns$registrationYear<"2001"), ]

SAM_and_FPDS_uniqueDuns <- SAM_and_FPDS_uniqueDuns[order(SAM_and_FPDS_uniqueDuns$registrationYear), ]


#******************************#
####bar graph for FPDS Data####
#******************************#
str(FPDS_cleaned_unique$top_small_biz)
#convert biz_size to numeric
##change top_small_biz to 1= small biz and 0 = non-small biz

str(FPDS_cleaned_unique$top_small_biz)

table(FPDS_cleaned_unique$top_small_biz)

FPDS_cleaned_unique <- FPDS_cleaned_unique[!(FPDS_cleaned_unique$top_small_biz==":"), ]

table(FPDS_cleaned_unique$top_small_biz)

##make biz_size binary
FPDS_cleaned_unique$top_smallbiz_bin <- revalue(FPDS_cleaned_unique$top_small_biz, c("S"="1", "O"="0"))

str(FPDS_cleaned_unique$top_smallbiz_bin)

table(FPDS_cleaned_unique$top_smallbiz_bin)

##drop observations with Registration Year before 2000
FPDS_cleaned_unique <- FPDS_cleaned_unique[!(FPDS_cleaned_unique$registrationYear<2001), ]
FPDS_cleaned_unique <- FPDS_cleaned_unique[!(FPDS_cleaned_unique$registrationYear>2016), ]

length(unique(FPDS_cleaned_unique$Dunsnumber)) == nrow(FPDS_cleaned_unique)

##find out which are duplicates
n_occur <- data.frame(table(FPDS_cleaned_unique$Dunsnumber)) ##gives a data frame with a list of duns and the number of times they occurred

n_occur[n_occur$Freq > 1, ]

totyear_count <- FPDS_cleaned_unique %>% 
  filter(top_smallbiz_bin == 1 | top_smallbiz_bin == 0) %>% 
  group_by(registrationYear) %>% 
  dplyr::summarise(n())  


FPDS_bargraphCount <- FPDS_cleaned_unique %>%
  filter(top_smallbiz_bin == 1 | top_smallbiz_bin == 0) %>%
  group_by(registrationYear, top_smallbiz_bin) %>%
  dplyr::summarise(n()) %>%
  dplyr::rename("regpersize"=`n()`) %>%
  left_join(totyear_count, by = "registrationYear") %>%
  dplyr::rename("regperyear"=`n()`) 
  

ggplot(FPDS_bargraphCount, aes(x = registrationYear, y = regpersize, fill = factor(top_smallbiz_bin), label = regperyear)) +
  geom_bar(stat = 'identity', position = 'stack') +
  ylab("Number of New Entrants") +
  xlab("Registration Year") +
  scale_x_continuous(breaks = c(2001:2016)) +
  ##scale_fill_manual(name = "New Entrants Types", values = c("deepskyblue", "royalblue1"), labels = c("small", "non-small")) +
  scale_fill_manual(name = "New Entrants Types", values = c("darkslategray1", "cadetblue4"), labels = c("small", "non-small")) +
  ggtitle("Number of New Entrants Per Year (2001-2016) - All Federal Agencies")+
  ##geom_text_repel(data = subset(FPDS_bargraphCount, registrationYear >=2014), aes(label = regpersize), size = 4, box.padding = .1, 
              ###    angle = 45) +
  ##geom_text(data = subset(FPDS_bargraphCount, registrationYear < 2014), aes(label = regpersize), size = 4, position = position_stack(vjust = .5), angle = 45)
  geom_text(data = subset(FPDS_bargraphCount, registrationYear <= 2016), aes(label = regpersize), size = 4, position = position_stack(vjust = .5), angle = 45)

#******************************
##chart for DoD
#******************************
FPDS_cleaned_unique <- FPDS_cleaned_unique[(FPDS_cleaned_unique$customer=="Defense"), ]



totyear_count <- FPDS_cleaned_unique %>% 
  filter(top_smallbiz_bin == 1 | top_smallbiz_bin == 0) %>% 
  group_by(registrationYear) %>% 
  dplyr::summarise(n())  


FPDS_bargraphCount <- FPDS_cleaned_unique %>%
  filter(top_smallbiz_bin == 1 | top_smallbiz_bin == 0) %>%
  group_by(registrationYear, top_smallbiz_bin) %>%
  dplyr::summarise(n()) %>%
  dplyr::rename("regpersize"=`n()`) %>%
  left_join(totyear_count, by = "registrationYear") %>%
  dplyr::rename("regperyear"=`n()`) 


ggplot(FPDS_bargraphCount, aes(x = registrationYear, y = regpersize, fill = factor(top_smallbiz_bin), label = regperyear)) +
  geom_bar(stat = 'identity', position = 'stack') +
  ylab("Number of New Entrants") +
  xlab("Registration Year") +
  scale_x_continuous(breaks = c(2001:2016)) +
  ##scale_fill_manual(name = "New Entrants Types", values = c("deepskyblue", "royalblue1"), labels = c("small", "non-small")) +
  scale_fill_manual(name = "New Entrants Types", values = c("darkslategray1", "cadetblue4"), labels = c("small", "non-small")) +
  ggtitle("Number of New Entrants Per Year (2001-2016) - DoD")+
  ##geom_text_repel(data = subset(FPDS_bargraphCount, registrationYear >=2014), aes(label = regpersize), size = 4, box.padding = .1, 
                  ##angle = 45) +
  ##geom_text(data = subset(FPDS_bargraphCount, registrationYear < 2014), aes(label = regpersize), size = 4, position = position_stack(vjust = .5), angle = 45)
  geom_text(data = subset(FPDS_bargraphCount, registrationYear <= 2016), aes(label = regpersize), size = 4, position = position_stack(vjust = .5), angle = 45)





#****************************#
####bar graph for SAM data####
#*****************************#

datapull_all <- datapull_all[!(datapull_all$registrationYear<"2001"), ]


SAM_totyear_count <- datapull_all %>% 
  group_by(registrationYear) %>% 
  dplyr::summarise(n())  


SAM_bargraphCount <- datapull_all %>%
  group_by(registrationYear) %>%
  dplyr::summarise(n()) %>%
  dplyr::rename("regpersize"=`n()`) %>%
  left_join(SAM_totyear_count, by = "registrationYear") %>%
  dplyr::rename("regperyear"=`n()`) 


ggplot(SAM_bargraphCount, aes(x = registrationYear, y = regpersize, label = regperyear)) +
  geom_bar(stat = 'identity') +
  ylab("Number of New Entrants") +
  xlab("Registration Year") +
  scale_x_continuous(breaks = c(2001:2016)) +
  ##scale_fill_manual(name = "New Entrants Types", values = c("deepskyblue", "royalblue1"), labels = c("small", "non-small")) +
  scale_fill_manual(name = "New Entrants Types", values = c("cadetblue4")) +
  ggtitle("Number of New Entrants Per Year (2001-2016) - All Federal Agencies")+
  geom_text_repel(data = subset(SAM_bargraphCount, registrationYear >=2014), aes(label = regpersize), size = 4, box.padding = .1, 
                  angle = 45) +
  geom_text(data = subset(SAM_bargraphCount, registrationYear < 2014), aes(label = regpersize), size = 4, position = position_stack(vjust = .5), angle = 45)

#******************************************************************************************************

#*****************************************************************#
####Calculate the survival and graduation rates using FPDS Data####
#*****************************************************************#


####2001####

#create necessary vars#

#************#
#survival vars
#************#

#subset the 2001 data
data_2001 <- FPDS_cleaned_unique[!(FPDS_cleaned_unique$registrationYear!="2001"), ]

##create variable describing whether a firm survived 3 years

data_2001 <- data_2001 %>%
  dplyr::mutate(survive_3yr = ifelse(exitYear < 2003, "0", "1")) 
           
##create variable describing whether a firm survived 5 years
data_2001 <- data_2001 %>%
  dplyr::mutate(survive_5yr = ifelse(exitYear < 2005, "0", "1")) 


##create variable describing whether a firm survived 10 years
data_2001 <- data_2001 %>%
  dplyr::mutate(survive_10yr = ifelse(exitYear < 2010, "0", "1")) 

#***************#
#graduation vars#
#************#

##create variable describing whether or not a firm graduated in the 10 year period

data_2001 <- data_2001 %>%
  dplyr::mutate(graduated = ifelse(top_smallbiz_bin))



#*****************
#Survival#
#*****************
##3-year##
table(data_2001$survive_3yr) #0=10168, 1=17266

numerator_3yr_2001 <- length(which(data_2001$survive_3yr==1))

denominator_3yr_2001 <- length(data_2001$survive_3yr)

survival_3yr_2001 <- numerator_3yr_2001/denominator_3yr_2001


##5-year##
table(data_2001$survive_5yr) #0=10168, 1=17266

numerator_5yr_2001 <- length(which(data_2001$survive_5yr==1))

denominator_5yr_2001 <- length(data_2001$survive_5yr)

survival_5yr_2001 <- numerator_5yr_2001/denominator_5yr_2001


##10-year##
table(data_2001$survive_10yr) #0=10168, 1=17266

numerator_10yr_2001 <- length(which(data_2001$survive_10yr==1))

denominator_10yr_2001 <- length(data_2001$survive_10yr)

survival_10yr_2001 <- numerator_10yr_2001/denominator_10yr_2001

#**********#
#Graduation#
#**********#




#**********************************************************************#

#***********#
####2002####
#***********#

#creae necessary variables to do this#

#subset the 2001 data
data_2002 <- FPDS_cleaned_unique[!(FPDS_cleaned_unique$registrationYear!="2002"), ]

##create variable describing whether a firm survived 3 years

data_2002 <- data_2002 %>%
  dplyr::mutate(survive_3yr = ifelse(exitYear < 2004, "0", "1")) 

##create variable describing whether a firm survived 5 years
data_2002 <- data_2002 %>%
  dplyr::mutate(survive_5yr = ifelse(exitYear < 2006, "0", "1")) 


##create variable describing whether a firm survived 10 years
data_2002 <- data_2002 %>%
  dplyr::mutate(survive_10yr = ifelse(exitYear < 2011, "0", "1")) 

#*****************
##Survival Rates##
#*****************

##3-year##
table(data_2002$survive_3yr) #0=12449, 1=21743

numerator_3yr_2002 <- length(which(data_2002$survive_3yr==1))

denominator_3yr_2002 <- length(data_2002$survive_3yr)

survival_3yr_2002 <- numerator_3yr_2002/denominator_3yr_2002


##5-year##
table(data_2002$survive_5yr) #0=10168, 1=17266

numerator_5yr_2002 <- length(which(data_2002$survive_5yr==1))

denominator_5yr_2002 <- length(data_2002$survive_5yr)

survival_5yr_2002 <- numerator_5yr_2002/denominator_5yr_2002


##10-year##
table(data_2002$survive_10yr) #0=10168, 1=17266

numerator_10yr_2002 <- length(which(data_2002$survive_10yr==1))

denominator_10yr_2002 <- length(data_2002$survive_10yr)

survival_10yr_2002 <- numerator_10yr_2002/denominator_10yr_2002

#****************#
#Graduation Rates#
#****************#

#**********************************************************************#

#***********#
####2003####
#***********#

##Create the variables to do this##

#subset the 2001 data
data_2003 <- FPDS_cleaned_unique[!(FPDS_cleaned_unique$registrationYear!="2003"), ]

##create variable describing whether a firm survived 3 years

data_2003 <- data_2003 %>%
  dplyr::mutate(survive_3yr = ifelse(exitYear < 2005, "0", "1")) 

##create variable describing whether a firm survived 5 years
data_2003 <- data_2003 %>%
  dplyr::mutate(survive_5yr = ifelse(exitYear < 2007, "0", "1")) 


##create variable describing whether a firm survived 10 years
data_2003 <- data_2003 %>%
  dplyr::mutate(survive_10yr = ifelse(exitYear < 2012, "0", "1"))

#***********#
#survival rates#
#**************#

##3-year##
table(data_2003$survive_3yr) #0=12449, 1=21743

numerator_3yr_2003 <- length(which(data_2003$survive_3yr==1))

denominator_3yr_2003 <- length(data_2003$survive_3yr)

survival_3yr_2003 <- numerator_3yr_2003/denominator_3yr_2003


##5-year##
table(data_2003$survive_5yr) #0=10168, 1=17266

numerator_5yr_2003 <- length(which(data_2003$survive_5yr==1))

denominator_5yr_2003 <- length(data_2003$survive_5yr)

survival_5yr_2003 <- numerator_5yr_2003/denominator_5yr_2003


##10-year##
table(data_2003$survive_10yr) #0=10168, 1=17266

numerator_10yr_2003 <- length(which(data_2003$survive_10yr==1))

denominator_10yr_2003 <- length(data_2003$survive_10yr)

survival_10yr_2003 <- numerator_10yr_2003/denominator_10yr_2003


#**********************************************************************#


#**********#
####2004####
#**********#
data_2004 <- FPDS_cleaned_unique[!(FPDS_cleaned_unique$registrationYear!="2004"), ]

##create variable describing whether a firm survived 3 years

data_2004 <- data_2004 %>%
  dplyr::mutate(survive_3yr = ifelse(exitYear < 2006, "0", "1")) 

##create variable describing whether a firm survived 5 years
data_2004 <- data_2004 %>%
  dplyr::mutate(survive_5yr = ifelse(exitYear < 2009, "0", "1")) 


##create variable describing whether a firm survived 10 years
data_2004 <- data_2004 %>%
  dplyr::mutate(survive_10yr = ifelse(exitYear < 2013, "0", "1")) 


#***********#
#survival rates#
#**************#

##3-year##
table(data_2004$survive_3yr) #0=12449, 1=21743

numerator_3yr_2004 <- length(which(data_2004$survive_3yr==1))

denominator_3yr_2004 <- length(data_2004$survive_3yr)

survival_3yr_2004 <- numerator_3yr_2004/denominator_3yr_2004


##5-year##
table(data_2004$survive_5yr) #0=10168, 1=17266

numerator_5yr_2004 <- length(which(data_2004$survive_5yr==1))

denominator_5yr_2004 <- length(data_2004$survive_5yr)

survival_5yr_2004 <- numerator_5yr_2004/denominator_5yr_2004


##10-year##
table(data_2004$survive_10yr) #0=10168, 1=17266

numerator_10yr_2004 <- length(which(data_2004$survive_10yr==1))

denominator_10yr_2004 <- length(data_2004$survive_10yr)

survival_10yr_2004 <- numerator_10yr_2004/denominator_10yr_2004

#*********#
#graduation rates
#****************

#**********************************************************************#


#**********#
####2005####
#***********#

##calculate the necessary vars##
data_2005 <- FPDS_cleaned_unique[!(FPDS_cleaned_unique$registrationYear!="2005"), ]

##create variable describing whether a firm survived 3 years

data_2005 <- data_2005 %>%
  dplyr::mutate(survive_3yr = ifelse(exitYear < 2007, "0", "1")) 

##create variable describing whether a firm survived 5 years
data_2005 <- data_2005 %>%
  dplyr::mutate(survive_5yr = ifelse(exitYear < 2010, "0", "1")) 


##create variable describing whether a firm survived 10 years
data_2005 <- data_2005 %>%
  dplyr::mutate(survive_10yr = ifelse(exitYear < 2014, "0", "1")) 
#*************#
#survival rates#
#***************#
##3-year##
table(data_2005$survive_3yr) #0=12449, 1=21743

numerator_3yr_2005 <- length(which(data_2005$survive_3yr==1))

denominator_3yr_2005 <- length(data_2005$survive_3yr)

survival_3yr_2005 <- numerator_3yr_2005/denominator_3yr_2005


##5-year##
table(data_2005$survive_5yr) #0=10168, 1=17266

numerator_5yr_2005 <- length(which(data_2005$survive_5yr==1))

denominator_5yr_2005 <- length(data_2005$survive_5yr)

survival_5yr_2005 <- numerator_5yr_2005/denominator_5yr_2005


##10-year##
table(data_2005$survive_10yr) #0=10168, 1=17266

numerator_10yr_2005 <- length(which(data_2005$survive_10yr==1))

denominator_10yr_2005 <- length(data_2005$survive_10yr)

survival_10yr_2005 <- numerator_10yr_2005/denominator_10yr_2005

#**********#
#graduation rates#
#***********#

#**********************************************************************#

#***********#
####2006####
#************#

#create necessary vars#

data_2006 <- FPDS_cleaned_unique[!(FPDS_cleaned_unique$registrationYear!="2006"), ]

##create variable describing whether a firm survived 3 years

data_2006 <- data_2006 %>%
  dplyr::mutate(survive_3yr = ifelse(exitYear < 2008, "0", "1")) 

##create variable describing whether a firm survived 5 years
data_2006 <- data_2006 %>%
  dplyr::mutate(survive_5yr = ifelse(exitYear < 2011, "0", "1")) 


##create variable describing whether a firm survived 10 years
data_2006 <- data_2006 %>%
  dplyr::mutate(survive_10yr = ifelse(exitYear < 2015, "0", "1")) 

#*************#
#survival rates#
#**************#

##3-year##
table(data_2006$survive_3yr) #0=12449, 1=21743

numerator_3yr_2006 <- length(which(data_2006$survive_3yr==1))

denominator_3yr_2006 <- length(data_2006$survive_3yr)

survival_3yr_2006 <- numerator_3yr_2006/denominator_3yr_2006


##5-year##
table(data_2006$survive_5yr) #0=10168, 1=17266

numerator_5yr_2006 <- length(which(data_2006$survive_5yr==1))

denominator_5yr_2006 <- length(data_2006$survive_5yr)

survival_5yr_2006 <- numerator_5yr_2006/denominator_5yr_2006


##10-year##
table(data_2006$survive_10yr) #0=10168, 1=17266

numerator_10yr_2006 <- length(which(data_2006$survive_10yr==1))

denominator_10yr_2006 <- length(data_2006$survive_10yr)

survival_10yr_2006 <- numerator_10yr_2006/denominator_10yr_2006

#****************#
#graduation rates#
#****************#


#********************************************************************
