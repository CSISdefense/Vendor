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

##sam work computer
setwd("K:/2018-01 NPS New Entrants/Data/Data/Cleaning data/FPDS")

##sam laptop
setwd("/Users/samanthacohen/Desktop/Diig backup/New Entrants/R Data")

load(file = "FPDS_datapull_all_v3.Rda")
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

##creates a dataframe that counts how many new entrants enter in each year
count_total_newentrants <- FPDS_cleaned_unique %>% 
  filter(top_smallbiz_bin == 1 | top_smallbiz_bin == 0) %>% 
  group_by(registrationYear) %>% 
  dplyr::summarise(n())  

##creates a dataframe that counts how many small vendors and how many non-small vendors are in each year
#and then joins it with the counts of all new vendors in each year
FPDS_bargraphCount <- FPDS_cleaned_unique %>%
  filter(top_smallbiz_bin == 1 | top_smallbiz_bin == 0) %>%
  group_by(registrationYear, top_smallbiz_bin) %>%
  dplyr::summarise(n()) %>%
  dplyr::rename("regpersize"=`n()`) %>%
  left_join(count_total_newentrants, by = "registrationYear") %>%
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
####Calculate the survival and graduation rates using FPDS Data for all federal agencies####
#*****************************************************************#


#create necessary vars#

####2001 all fed agencies####
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




#*****************
#Survival#
#*****************
#************#
#*****ALL****#
#************#
##3-year##
table(data_2001$survive_3yr) #0=10168, 1=17266

numerator_3yrALL_2001 <- length(which(data_2001$survive_3yr==1))

denominator_3yrALL_2001 <- length(data_2001$survive_3yr)

survival_3yrALL_2001 <- numerator_3yrALL_2001/denominator_3yrALL_2001
survival_3yrALL_2001

##5-year##
table(data_2001$survive_5yr) #0=10168, 1=17266

numerator_5yrALL_2001 <- length(which(data_2001$survive_5yr==1))

denominator_5yrALL_2001 <- length(data_2001$survive_5yr)

survival_5yrALL_2001 <- numerator_5yrALL_2001/denominator_5yrALL_2001
survival_5yrALL_2001

##10-year##
table(data_2001$survive_10yr) #0=10168, 1=17266

numerator_10yrALL_2001 <- length(which(data_2001$survive_10yr==1))

denominator_10yrALL_2001 <- length(data_2001$survive_10yr)

survival_10yrALL_2001 <- numerator_10yrALL_2001/denominator_10yrALL_2001
survival_10yrALL_2001

#**********#
#Graduation#
#**********#

table(data_2001$graduated)

##for all firms, whether or not they survived after 10 years

numerator_gradALL_2001 <- length(which(data_2001$graduated==1))

denominator_gradALL_2001 <- length(data_2001$graduated)

graduatedALL_2001 <- numerator_gradALL_2001/denominator_gradALL_2001
graduatedALL_2001

##for only firms that survived the ten years
numerator_gradALL_2001_10yr <- length(which(data_2001$graduated==1 & data_2001$survive_10yr==1))

denominator_gradALL_2001_10yr <- length(data_2001$graduated)

graduatedALL_2001_10yr <- numerator_gradALL_2001_10yr/denominator_gradALL_2001_10yr
graduatedALL_2001_10yr


#**************************
#**********#
#***SMALL**#
#**********#
##3-year##
table(data_2001$survive_3yr) #0=10168, 1=17266

numerator_3yrSM_2001 <- length(which(data_2001$survive_3yr==1 & data_2001$biz_size==1))

denominator_3yrSM_2001 <- length(which(data_2001$biz_size==1))

survival_3yrSM_2001 <- numerator_3yrSM_2001/denominator_3yrSM_2001
survival_3yrSM_2001

##5-year##
table(data_2001$survive_5yr) #0=10168, 1=17266

numerator_5yrSM_2001 <- length(which(data_2001$survive_5yr==1 & data_2001$biz_size==1))

denominator_5yrSM_2001 <- length(which(data_2001$biz_size==1))

survival_5yrSM_2001 <- numerator_5yrSM_2001/denominator_5yrSM_2001
survival_5yrSM_2001

##10-year##
table(data_2001$survive_10yr) #0=10168, 1=17266

numerator_10yrSM_2001 <- length(which(data_2001$survive_10yr==1 & data_2001$biz_size==1))

denominator_10yrSM_2001 <- length(which(data_2001$biz_size==1))

survival_10yrSM_2001 <- numerator_10yrSM_2001/denominator_10yrSM_2001
survival_10yrSM_2001


#**********#
#***NON-SMALL**#
#**********#

##3-year##
table(data_2001$survive_3yr) #0=10168, 1=17266

numerator_3yrNSM_2001 <- length(which(data_2001$survive_3yr==1 & data_2001$biz_size==0))

denominator_3yrNSM_2001 <- length(which(data_2001$biz_size==0))

survival_3yrNSM_2001 <- numerator_3yrNSM_2001/denominator_3yrNSM_2001
survival_3yrNSM_2001

##5-year##
table(data_2001$survive_5yr) #0=10168, 1=17266

numerator_5yrNSM_2001 <- length(which(data_2001$survive_5yr==1 & data_2001$biz_size==0))

denominator_5yrNSM_2001 <- length(which(data_2001$biz_size==0))

survival_5yrNSM_2001 <- numerator_5yrNSM_2001/denominator_5yrNSM_2001
survival_5yrNSM_2001

##10-year##
table(data_2001$survive_10yr) #0=10168, 1=17266

numerator_10yrNSM_2001 <- length(which(data_2001$survive_10yr==1 & data_2001$biz_size==0))

denominator_10yrNSM_2001 <- length(which(data_2001$biz_size==0))

survival_10yrNSM_2001 <- numerator_10yrNSM_2001/denominator_10yrNSM_2001
survival_10yrNSM_2001


#****************#
####2001 DOD only#####
#****************#
#subset the 2001 data
data_DOD_2001 <- FPDS_cleaned_unique[!(FPDS_cleaned_unique$registrationYear!="2001"), ]
data_DOD_2001 <- data_DOD_2001[!(data_DOD_2001$customer!="Defense"), ]

##create variable describing whether a firm survived 3 years

data_DOD_2001 <- data_DOD_2001 %>%
  dplyr::mutate(survive_3yr = ifelse(exitYear < 2003, "0", "1")) 

##create variable describing whether a firm survived 5 years
data_DOD_2001 <- data_DOD_2001 %>%
  dplyr::mutate(survive_5yr = ifelse(exitYear < 2005, "0", "1")) 


##create variable describing whether a firm survived 10 years
data_DOD_2001 <- data_DOD_2001 %>%
  dplyr::mutate(survive_10yr = ifelse(exitYear < 2010, "0", "1")) 




#*****************
#Survival#
#*****************
#************#
#*****ALL****#
#************#
##3-year##
table(data_DOD_2001$survive_3yr) #0=10168, 1=17266

numerator_3yrALL_2001_DOD <- length(which(data_DOD_2001$survive_3yr==1))

denominator_3yrALL_2001_DoD <- length(data_DOD_2001$survive_3yr)

survival_3yrALL_2001_DoD <- numerator_3yrALL_2001_DOD/denominator_3yrALL_2001_DoD
survival_3yrALL_2001_DoD

##5-year##
table(data_DOD_2001$survive_5yr) #0=10168, 1=17266

numerator_5yrALL_2001_DoD <- length(which(data_DOD_2001$survive_5yr==1))

denominator_5yrALL_2001_DoD <- length(data_DOD_2001$survive_5yr)

survival_5yrALL_2001_DoD <- numerator_5yrALL_2001_DoD/denominator_5yrALL_2001_DoD
survival_5yrALL_2001_DoD

##10-year##
table(data_DOD_2001$survive_10yr) #0=10168, 1=17266

numerator_10yrALL_2001_DoD <- length(which(data_DOD_2001$survive_10yr==1))

denominator_10yrALL_2001_DoD <- length(data_DOD_2001$survive_10yr)

survival_10yrALL_2001_DoD <- numerator_10yrALL_2001_DoD/denominator_10yrALL_2001_DoD
survival_10yrALL_2001_DoD

#**********#
#Graduation#
#**********#
##for all firms
table(data_DOD_2001$graduated)

numerator_gradALL_2001_DoD <- length(which(data_DOD_2001$graduated==1))

denominator_gradALL_2001_DoD <- length(data_DOD_2001$graduated)

graduatedALL_2001_DoD <- numerator_gradALL_2001_DoD/denominator_gradALL_2001_DoD
graduatedALL_2001_DoD

##for firms that only survived to the end of the study period
numerator_gradALL_2001_DoD_10yr <- length(which(data_DOD_2001$graduated==1 & data_DOD_2001$survive_10yr==1))

denominator_gradALL_2001_DoD_10yr <- length(data_DOD_2001$graduated)

graduatedALL_2001_DoD_10yr <- numerator_gradALL_2001_DoD_10yr/denominator_gradALL_2001_DoD_10yr
graduatedALL_2001_DoD_10yr


#**************************
#**********#
#***SMALL**#
#**********#
##3-year##
table(data_DOD_2001$survive_3yr) #0=10168, 1=17266

numerator_3yrSM_2001_DoD <- length(which(data_DOD_2001$survive_3yr==1 & data_DOD_2001$biz_size==1))

denominator_3yrSM_2001_DoD <- length(which(data_DOD_2001$biz_size==1))

survival_3yrSM_2001_DoD <- numerator_3yrSM_2001_DoD/denominator_3yrSM_2001_DoD
survival_3yrSM_2001_DoD

##5-year##
table(data_DOD_2001$survive_5yr) #0=10168, 1=17266

numerator_5yrSM_2001_DoD <- length(which(data_DOD_2001$survive_5yr==1 & data_DOD_2001$biz_size==1))

denominator_5yrSM_2001_DoD <- length(which(data_DOD_2001$biz_size==1))

survival_5yrSM_2001_DoD <- numerator_5yrSM_2001_DoD/denominator_5yrSM_2001_DoD
survival_5yrSM_2001_DoD

##10-year##
table(data_DOD_2001$survive_10yr) #0=10168, 1=17266

numerator_10yrSM_2001_DoD <- length(which(data_DOD_2001$survive_10yr==1 & data_DOD_2001$biz_size==1))

denominator_10yrSM_2001_DoD <- length(which(data_DOD_2001$biz_size==1))

survival_10yrSM_2001_DoD <- numerator_10yrSM_2001_DoD/denominator_10yrSM_2001_DoD
survival_10yrSM_2001_DoD


#**********#
#***NON-SMALL**#
#**********#

##3-year##
table(data_DOD_2001$survive_3yr) #0=10168, 1=17266

numerator_3yrNSM_2001_DoD <- length(which(data_DOD_2001$survive_3yr==1 & data_DOD_2001$biz_size==0))

denominator_3yrNSM_2001_DoD <- length(which(data_DOD_2001$biz_size==0))

survival_3yrNSM_2001_DoD <- numerator_3yrNSM_2001_DoD/denominator_3yrNSM_2001_DoD
survival_3yrNSM_2001_DoD

##5-year##
table(data_DOD_2001$survive_5yr) #0=10168, 1=17266

numerator_5yrNSM_2001_DoD <- length(which(data_DOD_2001$survive_5yr==1 & data_DOD_2001$biz_size==0))

denominator_5yrNSM_2001_DoD <- length(which(data_DOD_2001$biz_size==0))

survival_5yrNSM_2001_DoD <- numerator_5yrNSM_2001_DoD/denominator_5yrNSM_2001_DoD
survival_5yrNSM_2001_DoD

##10-year##
table(data_DOD_2001$survive_10yr) #0=10168, 1=17266

numerator_10yrNSM_2001_DoD <- length(which(data_DOD_2001$survive_10yr==1 & data_DOD_2001$biz_size==0))

denominator_10yrNSM_2001_DoD <- length(which(data_DOD_2001$biz_size==0))

survival_10yrNSM_2001_DoD <- numerator_10yrNSM_2001_DoD/denominator_10yrNSM_2001_DoD
survival_10yrNSM_2001_DoD




#**********************************************************************#

#***********#
####2002 all fed agencies####
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


#*************
#***ALL******#
#*************#
#*****************
##Survival Rates##
#*****************

##3-year##
table(data_2002$survive_3yr) #0=12449, 1=21743

numerator_3yrALL_2002 <- length(which(data_2002$survive_3yr==1))

denominator_3yrALL_2002 <- length(data_2002$survive_3yr)

survival_3yrALL_2002 <- numerator_3yrALL_2002/denominator_3yrALL_2002
survival_3yrALL_2002

##5-year##
table(data_2002$survive_5yr) #0=10168, 1=17266

numerator_5yrALL_2002 <- length(which(data_2002$survive_5yr==1))

denominator_5yrALL_2002 <- length(data_2002$survive_5yr)

survival_5yrALL_2002 <- numerator_5yrALL_2002/denominator_5yrALL_2002
survival_5yrALL_2002

##10-year##
table(data_2002$survive_10yr) #0=10168, 1=17266

numerator_10yrALL_2002 <- length(which(data_2002$survive_10yr==1))

denominator_10yrALL_2002 <- length(data_2002$survive_10yr)

survival_10yrALL_2002 <- numerator_10yrALL_2002/denominator_10yrALL_2002
survival_10yrALL_2002 
#****************#
#Graduation Rates#
#****************#
table(data_2002$graduated)

numerator_grad_2002 <- length(which(data_2002$graduated==1))

denominator_grad_2002 <- length(data_2002$graduated)

graduated_2002 <- numerator_grad_2002/denominator_grad_2002
graduated_2002

##graduation for only those who survived after 10 years
numerator_grad_2002_10yr <- length(which(data_2002$graduated==1 & data_2002$survive_10yr==1))

denominator_grad_2002_10yr <- length(data_2002$graduated)

graduated_2002_10yr <- numerator_grad_2002_10yr/denominator_grad_2002_10yr
graduated_2002_10yr


#**********#
#***SMALL**#
#**********#
##3-year##
table(data_2002$survive_3yr) #0=12449, 1=21743

numerator_3yrSM_2002 <- length(which(data_2002$survive_3yr==1 & data_2002$biz_size==1))

denominator_3yrSM_2002 <- length(which(data_2002$biz_size==1))

survival_3yrSM_2002 <- numerator_3yrSM_2002/denominator_3yrSM_2002
survival_3yrSM_2002

##5-year##
table(data_2002$survive_5yr) #0=10168, 1=17266

numerator_5yrSM_2002 <- length(which(data_2002$survive_5yr==1 & data_2002$biz_size==1))

denominator_5yrSM_2002 <- length(which(data_2002$biz_size==1))

survival_5yrSM_2002 <- numerator_5yrSM_2002/denominator_5yrSM_2002
survival_5yrSM_2002 

##10-year##
table(data_2002$survive_10yr) #0=10168, 1=17266

numerator_10yrSM_2002 <- length(which(data_2002$survive_10yr==1 & data_2002$biz_size==1))

denominator_10yrSM_2002 <- length(which(data_2002$biz_size==1))

survival_10yrSM_2002 <- numerator_10yrSM_2002/denominator_10yrSM_2002
survival_10yrSM_2002

#**********#
#***NON-SMALL*#
#***********#
##3-year##
table(data_2002$survive_3yr) #0=12449, 1=21743

numerator_3yrNSM_2002 <- length(which(data_2002$survive_3yr==1 & data_2002$biz_size==0))

denominator_3yrNSM_2002 <- length(which(data_2002$biz_size==0))

survival_3yrNSM_2002 <- numerator_3yrNSM_2002/denominator_3yrNSM_2002
survival_3yrNSM_2002

##5-year##
table(data_2002$survive_5yr) #0=10168, 1=17266

numerator_5yrNSM_2002 <- length(which(data_2002$survive_5yr==1 & data_2002$biz_size==0))

denominator_5yrNSM_2002 <- length(which(data_2002$biz_size==0))

survival_5yrNSM_2002 <- numerator_5yrNSM_2002/denominator_5yrNSM_2002
survival_5yrNSM_2002 

##10-year##
table(data_2002$survive_10yr) #0=10168, 1=17266

numerator_10yrNSM_2002 <- length(which(data_2002$survive_10yr==1 & data_2002$biz_size==0))

denominator_10yrNSM_2002 <- length(which(data_2002$biz_size==0))

survival_10yrNSM_2002 <- numerator_10yrNSM_2002/denominator_10yrNSM_2002
survival_10yrNSM_2002


#************#
#### 2002 DOD ONLY####
#****************#

#creae necessary variables to do this#

#subset the 2001 data
data_2002_DOD <- FPDS_cleaned_unique[!(FPDS_cleaned_unique$registrationYear!="2002"), ]
data_2002_DOD <- data_2002_DOD[!(data_2002_DOD$customer!="Defense"), ]

##create variable describing whether a firm survived 3 years

data_2002_DOD <- data_2002_DOD %>%
  dplyr::mutate(survive_3yr = ifelse(exitYear < 2004, "0", "1")) 

##create variable describing whether a firm survived 5 years
data_2002_DOD <- data_2002_DOD %>%
  dplyr::mutate(survive_5yr = ifelse(exitYear < 2006, "0", "1")) 


##create variable describing whether a firm survived 10 years
data_2002_DOD <- data_2002_DOD %>%
  dplyr::mutate(survive_10yr = ifelse(exitYear < 2011, "0", "1")) 


#*************
#***ALL******#
#*************#
#*****************
##Survival Rates##
#*****************

##3-year##
table(data_2002_DOD$survive_3yr) #0=12449, 1=21743

numerator_3yrALL2002_DOD <- length(which(data_2002_DOD$survive_3yr==1))

denominator_3yrALL2002_DOD <- length(data_2002_DOD$survive_3yr)

survival_3yrALL2002_DOD <- numerator_3yrALL2002_DOD/denominator_3yrALL2002_DOD
survival_3yrALL2002_DOD

##5-year##
table(data_2002_DOD$survive_5yr) #0=10168, 1=17266

numerator_5yrALL2002_DOD <- length(which(data_2002_DOD$survive_5yr==1))

denominator_5yrALL2002_DOD <- length(data_2002_DOD$survive_5yr)

survival_5yrALL2002_DOD <- numerator_5yrALL2002_DOD/denominator_5yrALL2002_DOD
survival_5yrALL2002_DOD

##10-year##
table(data_2002_DOD$survive_10yr) #0=10168, 1=17266

numerator_10yrALL2002_DOD <- length(which(data_2002_DOD$survive_10yr==1))

denominator_10yrALL2002_DOD <- length(data_2002_DOD$survive_10yr)

survival_10yrALL2002_DOD <- numerator_10yrALL2002_DOD/denominator_10yrALL2002_DOD
survival_10yrALL2002_DOD 
#****************#
#Graduation Rates#
#****************#
table(data_2002_DOD$graduated)

numerator_grad2002_DOD <- length(which(data_2002_DOD$graduated==1))

denominator_grad2002_DOD <- length(data_2002_DOD$graduated)

graduated2002_DOD <- numerator_grad2002_DOD/denominator_grad2002_DOD
graduated2002_DOD

##for those that survived 10 years
numerator_grad2002_DOD_10yr <- length(which(data_2002_DOD$graduated==1 & data_2002_DOD$survive_10yr==0))

denominator_grad2002_DOD_10yr <- length(data_2002_DOD$graduated)

graduated_2002_DOD_10yr <- numerator_grad2002_DOD_10yr/denominator_grad2002_DOD_10yr
graduated_2002_DOD_10yr


#**********#
#***SMALL**#
#**********#
##3-year##
table(data_2002_DOD$survive_3yr) #0=12449, 1=21743

numerator_3yrSM2002_DOD <- length(which(data_2002_DOD$survive_3yr==1 & data_2002_DOD$biz_size==1))

denominator_3yrSM2002_DOD <- length(which(data_2002_DOD$biz_size==1))

survival_3yrSM2002_DOD <- numerator_3yrSM2002_DOD/denominator_3yrSM2002_DOD
survival_3yrSM2002_DOD

##5-year##
table(data_2002_DOD$survive_5yr) #0=10168, 1=17266

numerator_5yrSM2002_DOD <- length(which(data_2002_DOD$survive_5yr==1 & data_2002_DOD$biz_size==1))

denominator_5yrSM2002_DOD <- length(which(data_2002_DOD$biz_size==1))

survival_5yrSM2002_DOD <- numerator_5yrSM2002_DOD/denominator_5yrSM2002_DOD
survival_5yrSM2002_DOD 

##10-year##
table(data_2002_DOD$survive_10yr) #0=10168, 1=17266

numerator_10yrSM2002_DOD <- length(which(data_2002_DOD$survive_10yr==1 & data_2002_DOD$biz_size==1))

denominator_10yrSM2002_DOD <- length(which(data_2002_DOD$biz_size==1))

survival_10yrSM2002_DOD <- numerator_10yrSM2002_DOD/denominator_10yrSM2002_DOD
survival_10yrSM2002_DOD

#**********#
#***NON-SMALL*#
#***********#
##3-year##
table(data_2002_DOD$survive_3yr) #0=12449, 1=21743

numerator_3yrNSM2002_DOD <- length(which(data_2002_DOD$survive_3yr==1 & data_2002_DOD$biz_size==0))

denominator_3yrNSM2002_DOD <- length(which(data_2002_DOD$biz_size==0))

survival_3yrNSM2002_DOD <- numerator_3yrNSM2002_DOD/denominator_3yrNSM2002_DOD
survival_3yrNSM2002_DOD

##5-year##
table(data_2002_DOD$survive_5yr) #0=10168, 1=17266

numerator_5yrNSM2002_DOD <- length(which(data_2002_DOD$survive_5yr==1 & data_2002_DOD$biz_size==0))

denominator_5yrNSM2002_DOD <- length(which(data_2002_DOD$biz_size==0))

survival_5yrNSM2002_DOD <- numerator_5yrNSM2002_DOD/denominator_5yrNSM2002_DOD
survival_5yrNSM2002_DOD 

##10-year##
table(data_2002_DOD$survive_10yr) #0=10168, 1=17266

numerator_10yrNSM2002_DOD <- length(which(data_2002_DOD$survive_10yr==1 & data_2002_DOD$biz_size==0))

denominator_10yrNSM2002_DOD <- length(which(data_2002_DOD$biz_size==0))

survival_10yrNSM2002_DOD <- numerator_10yrNSM2002_DOD/denominator_10yrNSM2002_DOD
survival_10yrNSM2002_DOD



#**********************************************************************#

#***********#
####2003 all federal agencies####
#***********#
#*********#
#ALL
#**********
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

numerator_3yrALL_2003 <- length(which(data_2003$survive_3yr==1))

denominator_3yrALL_2003 <- length(data_2003$survive_3yr)

survival_3yrALL_2003 <- numerator_3yrALL_2003/denominator_3yrALL_2003
survival_3yrALL_2003

##5-year##
table(data_2003$survive_5yr) #0=10168, 1=17266

numerator_5yrALL_2003 <- length(which(data_2003$survive_5yr==1))

denominator_5yrALL_2003 <- length(data_2003$survive_5yr)

survival_5yrALL_2003 <- numerator_5yrALL_2003/denominator_5yrALL_2003
survival_5yrALL_2003

##10-year##
table(data_2003$survive_10yr) #0=10168, 1=17266

numerator_10yrALL_2003 <- length(which(data_2003$survive_10yr==1))

denominator_10yrALL_2003 <- length(data_2003$survive_10yr)

survival_10yrALL_2003 <- numerator_10yrALL_2003/denominator_10yrALL_2003
survival_10yrALL_2003

#****************#
#Graduation Rates#
#****************#
table(data_2003$graduated)

numerator_grad_2003 <- length(which(data_2003$graduated==1))

denominator_grad_2003 <- length(data_2003$graduated)

graduated_2003 <- numerator_grad_2003/denominator_grad_2003
graduated_2003

##only for those that survived 10 years
numerator_grad_2003_10yr <- length(which(data_2003$graduated==1 & data_2003$survive_10yr==1))

denominator_grad_2003_10yr <- length(data_2003$graduated)

graduated_2003_10yr <- numerator_grad_2003_10yr/denominator_grad_2003_10yr
graduated_2003_10yr


#*******#
#***SMALL***#
#********#
##3-year##
table(data_2003$survive_3yr) #0=12449, 1=21743

numerator_3yrSM_2003 <- length(which(data_2003$survive_3yr==1 & data_2003$biz_size==1))

denominator_3yrSM_2003 <- length(which(data_2003$biz_size==1))

survival_3yrSM_2003 <- numerator_3yrSM_2003/denominator_3yrSM_2003
survival_3yrSM_2003

##5-year##
table(data_2003$survive_5yr) #0=10168, 1=17266

numerator_5yrSM_2003 <- length(which(data_2003$survive_5yr==1 & data_2003$biz_size==1))

denominator_5yrSM_2003 <- length(which(data_2003$biz_size==1))

survival_5yrSM_2003 <- numerator_5yrSM_2003/denominator_5yrSM_2003
survival_5yrSM_2003

##10-year##
table(data_2003$survive_10yr) #0=10168, 1=17266

numerator_10yrSM_2003 <- length(which(data_2003$survive_10yr==1 & data_2003$biz_size==1))

denominator_10yrSM_2003 <- length(which(data_2003$biz_size==1))

survival_10yrSM_2003 <- numerator_10yrSM_2003/denominator_10yrSM_2003
survival_10yrSM_2003

#*****#
#NONSMALL*
#******#
##3-year##
table(data_2003$survive_3yr) #0=12449, 1=21743

numerator_3yrNSM_2003 <- length(which(data_2003$survive_3yr==1 & data_2003$biz_size==0))

denominator_3yrNSM_2003 <- length(which(data_2003$biz_size==0))

survival_3yrNSM_2003 <- numerator_3yrNSM_2003/denominator_3yrNSM_2003
survival_3yrNSM_2003

##5-year##
table(data_2003$survive_5yr) #0=10168, 1=17266

numerator_5yrNSM_2003 <- length(which(data_2003$survive_5yr==1 & data_2003$biz_size==0))

denominator_5yrNSM_2003 <- length(which(data_2003$biz_size==0))

survival_5yrNSM_2003 <- numerator_5yrNSM_2003/denominator_5yrNSM_2003
survival_5yrNSM_2003

##10-year##
table(data_2003$survive_10yr) #0=10168, 1=17266

numerator_10yrNSM_2003 <- length(which(data_2003$survive_10yr==1 & data_2003$biz_size==0))

denominator_10yrNSM_2003 <- length(which(data_2003$biz_size==0))

survival_10yrNSM_2003 <- numerator_10yrNSM_2003/denominator_10yrNSM_2003
survival_10yrNSM_2003


#*********#
#### 2003 DOD ONLY####
#*************#

#*********#
#ALL
#**********
##Create the variables to do this##

#subset the 2001 data
data_2003_DOD <- FPDS_cleaned_unique[!(FPDS_cleaned_unique$registrationYear!="2003"), ]
data_2003_DOD <- data_2003_DOD[!(data_2003_DOD$customer!="Defense"), ]


##create variable describing whether a firm survived 3 years

data_2003_DOD <- data_2003_DOD %>%
  dplyr::mutate(survive_3yr = ifelse(exitYear < 2005, "0", "1")) 

##create variable describing whether a firm survived 5 years
data_2003_DOD <- data_2003_DOD %>%
  dplyr::mutate(survive_5yr = ifelse(exitYear < 2007, "0", "1")) 


##create variable describing whether a firm survived 10 years
data_2003_DOD <- data_2003_DOD %>%
  dplyr::mutate(survive_10yr = ifelse(exitYear < 2012, "0", "1"))

#***********#
#survival rates#
#**************#

##3-year##
table(data_2003_DOD$survive_3yr) #0=12449, 1=21743

numerator_3yrALL_2003_DOD <- length(which(data_2003_DOD$survive_3yr==1))

denominator_3yrALL_2003_DOD <- length(data_2003_DOD$survive_3yr)

survival_3yrALL_2003_DOD <- numerator_3yrALL_2003_DOD/denominator_3yrALL_2003_DOD
survival_3yrALL_2003_DOD

##5-year##
table(data_2003_DOD$survive_5yr) #0=10168, 1=17266

numerator_5yrALL_2003_DOD <- length(which(data_2003_DOD$survive_5yr==1))

denominator_5yrALL_2003_DOD <- length(data_2003_DOD$survive_5yr)

survival_5yrALL_2003_DOD <- numerator_5yrALL_2003_DOD/denominator_5yrALL_2003_DOD
survival_5yrALL_2003_DOD

##10-year##
table(data_2003_DOD$survive_10yr) #0=10168, 1=17266

numerator_10yrALL_2003_DOD <- length(which(data_2003_DOD$survive_10yr==1))

denominator_10yrALL_2003_DOD <- length(data_2003_DOD$survive_10yr)

survival_10yrALL_2003_DOD <- numerator_10yrALL_2003_DOD/denominator_10yrALL_2003_DOD
survival_10yrALL_2003_DOD

#****************#
#Graduation Rates#
#****************#
table(data_2003_DOD$graduated)

numerator_grad_2003_DOD <- length(which(data_2003_DOD$graduated==1))

denominator_grad_2003_DOD <- length(data_2003_DOD$graduated)

graduated_2003_DOD <- numerator_grad_2003_DOD/denominator_grad_2003_DOD
graduated_2003_DOD

##graduation for those who survived 10 years
numerator_grad_2003_DOD_10yr <- length(which(data_2003_DOD$graduated==1 & data_2003_DOD$survive_10yr==1))

denominator_grad_2003_DOD_10yr <- length(data_2003_DOD$graduated)

graduated_2003_DOD_10yr <- numerator_grad_2003_DOD_10yr/denominator_grad_2003_DOD_10yr
graduated_2003_DOD_10yr


#*******#
#***SMALL***#
#********#
##3-year##
table(data_2003_DOD$survive_3yr) #0=12449, 1=21743

numerator_3yrSM_2003_DOD <- length(which(data_2003_DOD$survive_3yr==1 & data_2003_DOD$biz_size==1))

denominator_3yrSM_2003_DOD <- length(which(data_2003_DOD$biz_size==1))

survival_3yrSM_2003_DOD <- numerator_3yrSM_2003_DOD/denominator_3yrSM_2003_DOD
survival_3yrSM_2003_DOD

##5-year##
table(data_2003_DOD$survive_5yr) #0=10168, 1=17266

numerator_5yrSM_2003_DOD <- length(which(data_2003_DOD$survive_5yr==1 & data_2003_DOD$biz_size==1))

denominator_5yrSM_2003_DOD <- length(which(data_2003_DOD$biz_size==1))

survival_5yrSM_2003_DOD <- numerator_5yrSM_2003_DOD/denominator_5yrSM_2003_DOD
survival_5yrSM_2003_DOD

##10-year##
table(data_2003_DOD$survive_10yr) #0=10168, 1=17266

numerator_10yrSM_2003_DOD <- length(which(data_2003_DOD$survive_10yr==1 & data_2003_DOD$biz_size==1))

denominator_10yrSM_2003_DOD <- length(which(data_2003_DOD$biz_size==1))

survival_10yrSM_2003_DOD <- numerator_10yrSM_2003_DOD/denominator_10yrSM_2003_DOD
survival_10yrSM_2003_DOD

#*****#
#NONSMALL*
#******#
##3-year##
table(data_2003_DOD$survive_3yr) #0=12449, 1=21743

numerator_3yrNSM_2003_DOD <- length(which(data_2003_DOD$survive_3yr==1 & data_2003_DOD$biz_size==0))

denominator_3yrNSM_2003_DOD <- length(which(data_2003_DOD$biz_size==0))

survival_3yrNSM_2003_DOD <- numerator_3yrNSM_2003_DOD/denominator_3yrNSM_2003_DOD
survival_3yrNSM_2003_DOD

##5-year##
table(data_2003_DOD$survive_5yr) #0=10168, 1=17266

numerator_5yrNSM_2003_DOD <- length(which(data_2003_DOD$survive_5yr==1 & data_2003_DOD$biz_size==0))

denominator_5yrNSM_2003_DOD <- length(which(data_2003_DOD$biz_size==0))

survival_5yrNSM_2003_DOD <- numerator_5yrNSM_2003_DOD/denominator_5yrNSM_2003_DOD
survival_5yrNSM_2003_DOD

##10-year##
table(data_2003_DOD$survive_10yr) #0=10168, 1=17266

numerator_10yrNSM_2003_DOD <- length(which(data_2003_DOD$survive_10yr==1 & data_2003_DOD$biz_size==0))

denominator_10yrNSM_2003_DOD <- length(which(data_2003_DOD$biz_size==0))

survival_10yrNSM_2003_DOD <- numerator_10yrNSM_2003_DOD/denominator_10yrNSM_2003_DOD
survival_10yrNSM_2003_DOD




#**********************************************************************#


#**********#
####2004 all fed agencies####
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


#********#
#**ALL**#
#********#

#***********#
#survival rates#
#**************#

##3-year##
table(data_2004$survive_3yr) #0=12449, 1=21743

numerator_3yrALL_2004 <- length(which(data_2004$survive_3yr==1))

denominator_3yrALL_2004 <- length(data_2004$survive_3yr)

survival_3yrALL_2004 <- numerator_3yrALL_2004/denominator_3yrALL_2004
survival_3yrALL_2004

##5-year##
table(data_2004$survive_5yr) #0=10168, 1=17266

numerator_5yrALL_2004 <- length(which(data_2004$survive_5yr==1))

denominator_5yrALL_2004 <- length(data_2004$survive_5yr)

survival_5yrALL_2004 <- numerator_5yrALL_2004/denominator_5yrALL_2004
survival_5yrALL_2004

##10-year##
table(data_2004$survive_10yr) #0=10168, 1=17266

numerator_10yrALL_2004 <- length(which(data_2004$survive_10yr==1))

denominator_10yrALL_2004 <- length(data_2004$survive_10yr)

survival_10yrALL_2004 <- numerator_10yrALL_2004/denominator_10yrALL_2004
survival_10yrALL_2004

#****************#
#Graduation Rates#
#****************#
table(data_2004$graduated)

numerator_grad_2004 <- length(which(data_2004$graduated==1))

denominator_grad_2004 <- length(data_2004$graduated)

graduated_2004 <- numerator_grad_2004/denominator_grad_2004
graduated_2004 

##graduation 10 year survivors only
numerator_grad_2004_10yr <- length(which(data_2004$graduated==1 & data_2004$survive_10yr==1))

denominator_grad_2004_10yr <- length(data_2004$graduated)

graduated_2004_10yr <- numerator_grad_2004_10yr/denominator_grad_2004_10yr
graduated_2004_10yr 



#******#
#*SMALL*#
#******#
##3-year##
table(data_2004$survive_3yr) #0=12449, 1=21743

numerator_3yrSM_2004 <- length(which(data_2004$survive_3yr==1 & data_2004$biz_size==1))

denominator_3yrSM_2004 <- length(which(data_2004$biz_size==1))

survival_3yrSM_2004 <- numerator_3yrSM_2004/denominator_3yrSM_2004
survival_3yrSM_2004

##5-year##
table(data_2004$survive_5yr) #0=10168, 1=17266

numerator_5yrSM_2004 <- length(which(data_2004$survive_5yr==1 & data_2004$biz_size==1))

denominator_5yrSM_2004 <- length(which(data_2004$biz_size==1))

survival_5yrSM_2004 <- numerator_5yrSM_2004/denominator_5yrSM_2004
survival_5yrSM_2004

##10-year##
table(data_2004$survive_10yr) #0=10168, 1=17266

numerator_10yrSM_2004 <- length(which(data_2004$survive_10yr==1 & data_2004$biz_size==1))

denominator_10yrSM_2004 <- length(which(data_2004$biz_size==1))

survival_10yrSM_2004 <- numerator_10yrSM_2004/denominator_10yrSM_2004
survival_10yrSM_2004



#********#
#NON SMALL#
#********#
##3-year##
table(data_2004$survive_3yr) #0=12449, 1=21743

numerator_3yrNSM_2004 <- length(which(data_2004$survive_3yr==1 & data_2004$biz_size==0))

denominator_3yrNSM_2004 <- length(which(data_2004$biz_size==0))

survival_3yrNSM_2004 <- numerator_3yrNSM_2004/denominator_3yrNSM_2004
survival_3yrNSM_2004 

##5-year##
table(data_2004$survive_5yr) #0=10168, 1=17266

numerator_5yrNSM_2004 <- length(which(data_2004$survive_5yr==1 & data_2004$biz_size==0))

denominator_5yrNSM_2004 <- length(which(data_2004$biz_size==0))

survival_5yrNSM_2004 <- numerator_5yrNSM_2004/denominator_5yrNSM_2004
survival_5yrNSM_2004

##10-year##
table(data_2004$survive_10yr) #0=10168, 1=17266

numerator_10yrNSM_2004 <- length(which(data_2004$survive_10yr==1 & data_2004$biz_size==0))

denominator_10yrNSM_2004 <- length(which(data_2004$biz_size==0))

survival_10yrNSM_2004 <- numerator_10yrNSM_2004/denominator_10yrNSM_2004
survival_10yrNSM_2004 


#***************#
#### 2004 DOD ONLY####
#**************#
data_2004_DOD <- FPDS_cleaned_unique[!(FPDS_cleaned_unique$registrationYear!="2004"), ]
data_2004_DOD <- data_2004_DOD[!(data_2004_DOD$customer!="Defense"), ]

##create variable describing whether a firm survived 3 years

data_2004_DOD <- data_2004_DOD %>%
  dplyr::mutate(survive_3yr = ifelse(exitYear < 2006, "0", "1")) 

##create variable describing whether a firm survived 5 years
data_2004_DOD <- data_2004_DOD %>%
  dplyr::mutate(survive_5yr = ifelse(exitYear < 2009, "0", "1")) 


##create variable describing whether a firm survived 10 years
data_2004_DOD <- data_2004_DOD %>%
  dplyr::mutate(survive_10yr = ifelse(exitYear < 2013, "0", "1")) 


#********#
#**ALL**#
#********#

#***********#
#survival rates#
#**************#

##3-year##
table(data_2004_DOD$survive_3yr) #0=12449, 1=21743

numerator_3yrALL_2004_DOD <- length(which(data_2004_DOD$survive_3yr==1))

denominator_3yrALL_2004_DOD <- length(data_2004_DOD$survive_3yr)

survival_3yrALL_2004_DOD <- numerator_3yrALL_2004_DOD/denominator_3yrALL_2004_DOD
survival_3yrALL_2004_DOD

##5-year##
table(data_2004_DOD$survive_5yr) #0=10168, 1=17266

numerator_5yrALL_2004_DOD <- length(which(data_2004_DOD$survive_5yr==1))

denominator_5yrALL_2004_DOD <- length(data_2004_DOD$survive_5yr)

survival_5yrALL_2004_DOD <- numerator_5yrALL_2004_DOD/denominator_5yrALL_2004_DOD
survival_5yrALL_2004_DOD

##10-year##
table(data_2004_DOD$survive_10yr) #0=10168, 1=17266

numerator_10yrALL_2004_DOD <- length(which(data_2004_DOD$survive_10yr==1))

denominator_10yrALL_2004_DOD <- length(data_2004_DOD$survive_10yr)

survival_10yrALL_2004_DOD <- numerator_10yrALL_2004_DOD/denominator_10yrALL_2004_DOD
survival_10yrALL_2004_DOD

#****************#
#Graduation Rates#
#****************#
table(data_2004_DOD$graduated)

numerator_grad_2004_DOD <- length(which(data_2004_DOD$graduated==1))

denominator_grad_2004_DOD <- length(data_2004_DOD$graduated)

graduated_2004_DOD <- numerator_grad_2004_DOD/denominator_grad_2004_DOD
graduated_2004_DOD 

##those who survived 10 years only
numerator_grad_2004_DOD_10yr <- length(which(data_2004_DOD$graduated==1 & data_2004_DOD$survive_10yr==1))

denominator_grad_2004_DOD_10yr <- length(data_2004_DOD$graduated)

graduated_2004_DOD_10yr <- numerator_grad_2004_DOD_10yr/denominator_grad_2004_DOD_10yr
graduated_2004_DOD_10yr 


#******#
#*SMALL*#
#******#
##3-year##
table(data_2004_DOD$survive_3yr) #0=12449, 1=21743

numerator_3yrSM_2004_DOD <- length(which(data_2004_DOD$survive_3yr==1 & data_2004_DOD$biz_size==1))

denominator_3yrSM_2004_DOD <- length(which(data_2004_DOD$biz_size==1))

survival_3yrSM_2004_DOD <- numerator_3yrSM_2004_DOD/denominator_3yrSM_2004_DOD
survival_3yrSM_2004_DOD

##5-year##
table(data_2004_DOD$survive_5yr) #0=10168, 1=17266

numerator_5yrSM_2004_DOD <- length(which(data_2004_DOD$survive_5yr==1 & data_2004_DOD$biz_size==1))

denominator_5yrSM_2004_DOD <- length(which(data_2004_DOD$biz_size==1))

survival_5yrSM_2004_DOD <- numerator_5yrSM_2004_DOD/denominator_5yrSM_2004_DOD
survival_5yrSM_2004_DOD

##10-year##
table(data_2004_DOD$survive_10yr) #0=10168, 1=17266

numerator_10yrSM_2004_DOD <- length(which(data_2004_DOD$survive_10yr==1 & data_2004_DOD$biz_size==1))

denominator_10yrSM_2004_DOD <- length(which(data_2004_DOD$biz_size==1))

survival_10yrSM_2004_DOD <- numerator_10yrSM_2004_DOD/denominator_10yrSM_2004_DOD
survival_10yrSM_2004_DOD



#********#
#NON SMALL#
#********#
##3-year##
table(data_2004_DOD$survive_3yr) #0=12449, 1=21743

numerator_3yrNSM_2004_DOD <- length(which(data_2004_DOD$survive_3yr==1 & data_2004_DOD$biz_size==0))

denominator_3yrNSM_2004_DOD <- length(which(data_2004_DOD$biz_size==0))

survival_3yrNSM_2004_DOD <- numerator_3yrNSM_2004_DOD/denominator_3yrNSM_2004_DOD
survival_3yrNSM_2004_DOD 

##5-year##
table(data_2004_DOD$survive_5yr) #0=10168, 1=17266

numerator_5yrNSM_2004_DOD <- length(which(data_2004_DOD$survive_5yr==1 & data_2004_DOD$biz_size==0))

denominator_5yrNSM_2004_DOD <- length(which(data_2004_DOD$biz_size==0))

survival_5yrNSM_2004_DOD <- numerator_5yrNSM_2004_DOD/denominator_5yrNSM_2004_DOD
survival_5yrNSM_2004_DOD

##10-year##
table(data_2004_DOD$survive_10yr) #0=10168, 1=17266

numerator_10yrNSM_2004_DOD <- length(which(data_2004_DOD$survive_10yr==1 & data_2004_DOD$biz_size==0))

denominator_10yrNSM_2004_DOD <- length(which(data_2004_DOD$biz_size==0))

survival_10yrNSM_2004_DOD <- numerator_10yrNSM_2004_DOD/denominator_10yrNSM_2004_DOD
survival_10yrNSM_2004_DOD 



#**********************************************************************#



#**********#
####2005 all fed agencies####
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

#******#
#**ALL*#
#*****#
#*************#
#survival rates#
#***************#
##3-year##
table(data_2005$survive_3yr) #0=12449, 1=21743

numerator_3yrALL_2005 <- length(which(data_2005$survive_3yr==1))

denominator_3yrALL_2005 <- length(data_2005$survive_3yr)

survival_3yrALL_2005 <- numerator_3yrALL_2005/denominator_3yrALL_2005
survival_3yrALL_2005

##5-year##
table(data_2005$survive_5yr) #0=10168, 1=17266

numerator_5yrALL_2005 <- length(which(data_2005$survive_5yr==1))

denominator_5yrALL_2005 <- length(data_2005$survive_5yr)

survival_5yrALL_2005 <- numerator_5yrALL_2005/denominator_5yrALL_2005
survival_5yrALL_2005

##10-year##
table(data_2005$survive_10yr) #0=10168, 1=17266

numerator_10yrALL_2005 <- length(which(data_2005$survive_10yr==1))

denominator_10yrALL_2005 <- length(data_2005$survive_10yr)

survival_10yrALL_2005 <- numerator_10yrALL_2005/denominator_10yrALL_2005
survival_10yrALL_2005 
#****************#
#Graduation Rates#
#****************#
table(data_2005$graduated)

numerator_grad_2005 <- length(which(data_2005$graduated==1))

denominator_grad_2005 <- length(data_2005$graduated)

graduated_2005 <- numerator_grad_2005/denominator_grad_2005
graduated_2005

##for those that survived 10 years only
numerator_grad_2005_10yr <- length(which(data_2005$graduated==1 & data_2005$survive_10yr==1))

denominator_grad_2005_10yr <- length(data_2005$graduated)

graduated_2005_10yr <- numerator_grad_2005_10yr/denominator_grad_2005_10yr
graduated_2005_10yr



#******#
#SMALL#
#******#
##3-year##
table(data_2005$survive_3yr) #0=12449, 1=21743

numerator_3yrSM_2005 <- length(which(data_2005$survive_3yr==1 & data_2005$biz_size==1))

denominator_3yrSM_2005 <- length(which(data_2005$biz_size==1))

survival_3yrSM_2005 <- numerator_3yrSM_2005/denominator_3yrSM_2005
survival_3yrSM_2005

##5-year##
table(data_2005$survive_5yr) #0=10168, 1=17266

numerator_5yrSM_2005 <- length(which(data_2005$survive_5yr==1 & data_2005$biz_size==1))

denominator_5yrSM_2005 <- length(which(data_2005$biz_size==1))

survival_5yrSM_2005 <- numerator_5yrSM_2005/denominator_5yrSM_2005
survival_5yrSM_2005 

##10-year##
table(data_2005$survive_10yr) #0=10168, 1=17266

numerator_10yrSM_2005 <- length(which(data_2005$survive_10yr==1 & data_2005$biz_size==1))

denominator_10yrSM_2005 <- length(which(data_2005$biz_size==1))

survival_10yrSM_2005 <- numerator_10yrSM_2005/denominator_10yrSM_2005
survival_10yrSM_2005

#*******#
#NON SMAL#
#*******#
##3-year##
table(data_2005$survive_3yr) #0=12449, 1=21743

numerator_3yrNSM_2005 <- length(which(data_2005$survive_3yr==1 & data_2005$biz_size==0))

denominator_3yrNSM_2005 <- length(which(data_2005$biz_size==0))

survival_3yrNSM_2005 <- numerator_3yrNSM_2005/denominator_3yrNSM_2005
survival_3yrNSM_2005

##5-year##
table(data_2005$survive_5yr) #0=10168, 1=17266

numerator_5yrNSM_2005 <- length(which(data_2005$survive_5yr==1 & data_2005$biz_size==0))

denominator_5yrNSM_2005 <- length(which(data_2005$biz_size==0))

survival_5yrNSM_2005 <- numerator_5yrNSM_2005/denominator_5yrNSM_2005
survival_5yrNSM_2005

##10-year##
table(data_2005$survive_10yr) #0=10168, 1=17266

numerator_10yrNSM_2005 <- length(which(data_2005$survive_10yr==1 & data_2005$biz_size==0))

denominator_10yrNSM_2005 <- length(which(data_2005$biz_size==0))

survival_10yrNSM_2005 <- numerator_10yrNSM_2005/denominator_10yrNSM_2005
survival_10yrNSM_2005

#************#
####2005 DOD ONLY####
#**************#
data_2005_DOD <- FPDS_cleaned_unique[!(FPDS_cleaned_unique$registrationYear!="2005"), ]
data_2005_DOD <- data_2005_DOD[!(data_2005_DOD$customer!="Defense"), ]


##create variable describing whether a firm survived 3 years

data_2005_DOD <- data_2005_DOD %>%
  dplyr::mutate(survive_3yr = ifelse(exitYear < 2007, "0", "1")) 

##create variable describing whether a firm survived 5 years
data_2005_DOD <- data_2005_DOD %>%
  dplyr::mutate(survive_5yr = ifelse(exitYear < 2010, "0", "1")) 


##create variable describing whether a firm survived 10 years
data_2005_DOD <- data_2005_DOD %>%
  dplyr::mutate(survive_10yr = ifelse(exitYear < 2014, "0", "1")) 

#******#
#**ALL*#
#*****#
#*************#
#survival rates#
#***************#
##3-year##
table(data_2005_DOD$survive_3yr) #0=12449, 1=21743

numerator_3yrALL_2005_DOD <- length(which(data_2005_DOD$survive_3yr==1))

denominator_3yrALL_2005_DOD <- length(data_2005_DOD$survive_3yr)

survival_3yrALL_2005_DOD <- numerator_3yrALL_2005_DOD/denominator_3yrALL_2005_DOD
survival_3yrALL_2005_DOD

##5-year##
table(data_2005_DOD$survive_5yr) #0=10168, 1=17266

numerator_5yrALL_2005_DOD <- length(which(data_2005_DOD$survive_5yr==1))

denominator_5yrALL_2005_DOD <- length(data_2005_DOD$survive_5yr)

survival_5yrALL_2005_DOD <- numerator_5yrALL_2005_DOD/denominator_5yrALL_2005_DOD
survival_5yrALL_2005_DOD

##10-year##
table(data_2005_DOD$survive_10yr) #0=10168, 1=17266

numerator_10yrALL_2005_DOD <- length(which(data_2005_DOD$survive_10yr==1))

denominator_10yrALL_2005_DOD <- length(data_2005_DOD$survive_10yr)

survival_10yrALL_2005_DOD <- numerator_10yrALL_2005_DOD/denominator_10yrALL_2005_DOD
survival_10yrALL_2005_DOD 
#****************#
#Graduation Rates#
#****************#
table(data_2005_DOD$graduated)

numerator_grad_2005_DOD <- length(which(data_2005_DOD$graduated==1))

denominator_grad_2005_DOD <- length(data_2005_DOD$graduated)

graduated_2005_DOD <- numerator_grad_2005_DOD/denominator_grad_2005_DOD
graduated_2005_DOD

##survived 10 years only
numerator_grad_2005_DOD_10yr <- length(which(data_2005_DOD$graduated==1 & data_2005_DOD$survive_10yr==1))

denominator_grad_2005_DOD_10yr <- length(data_2005_DOD$graduated)

graduated_2005_DOD_10yr <- numerator_grad_2005_DOD_10yr/denominator_grad_2005_DOD_10yr
graduated_2005_DOD_10yr


#******#
#SMALL#
#******#
##3-year##
table(data_2005_DOD$survive_3yr) #0=12449, 1=21743

numerator_3yrSM_2005_DOD <- length(which(data_2005_DOD$survive_3yr==1 & data_2005_DOD$biz_size==1))

denominator_3yrSM_2005_DOD <- length(which(data_2005_DOD$biz_size==1))

survival_3yrSM_2005_DOD <- numerator_3yrSM_2005_DOD/denominator_3yrSM_2005_DOD
survival_3yrSM_2005_DOD

##5-year##
table(data_2005_DOD$survive_5yr) #0=10168, 1=17266

numerator_5yrSM_2005_DOD <- length(which(data_2005_DOD$survive_5yr==1 & data_2005_DOD$biz_size==1))

denominator_5yrSM_2005_DOD <- length(which(data_2005_DOD$biz_size==1))

survival_5yrSM_2005_DOD <- numerator_5yrSM_2005_DOD/denominator_5yrSM_2005_DOD
survival_5yrSM_2005_DOD 

##10-year##
table(data_2005_DOD$survive_10yr) #0=10168, 1=17266

numerator_10yrSM_2005_DOD <- length(which(data_2005_DOD$survive_10yr==1 & data_2005_DOD$biz_size==1))

denominator_10yrSM_2005_DOD <- length(which(data_2005_DOD$biz_size==1))

survival_10yrSM_2005_DOD <- numerator_10yrSM_2005_DOD/denominator_10yrSM_2005_DOD
survival_10yrSM_2005_DOD

#*******#
#NON SMAL#
#*******#
##3-year##
table(data_2005_DOD$survive_3yr) #0=12449, 1=21743

numerator_3yrNSM_2005_DOD <- length(which(data_2005_DOD$survive_3yr==1 & data_2005_DOD$biz_size==0))

denominator_3yrNSM_2005_DOD <- length(which(data_2005_DOD$biz_size==0))

survival_3yrNSM_2005_DOD <- numerator_3yrNSM_2005_DOD/denominator_3yrNSM_2005_DOD
survival_3yrNSM_2005_DOD

##5-year##
table(data_2005_DOD$survive_5yr) #0=10168, 1=17266

numerator_5yrNSM_2005_DOD <- length(which(data_2005_DOD$survive_5yr==1 & data_2005_DOD$biz_size==0))

denominator_5yrNSM_2005_DOD <- length(which(data_2005_DOD$biz_size==0))

survival_5yrNSM_2005_DOD <- numerator_5yrNSM_2005_DOD/denominator_5yrNSM_2005_DOD
survival_5yrNSM_2005_DOD

##10-year##
table(data_2005_DOD$survive_10yr) #0=10168, 1=17266

numerator_10yrNSM_2005_DOD <- length(which(data_2005_DOD$survive_10yr==1 & data_2005_DOD$biz_size==0))

denominator_10yrNSM_2005_DOD <- length(which(data_2005_DOD$biz_size==0))

survival_10yrNSM_2005_DOD <- numerator_10yrNSM_2005_DOD/denominator_10yrNSM_2005_DOD
survival_10yrNSM_2005_DOD


#**********************************************************************#

#***********#
####2006 all fed agencies####
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
#*********#
#**ALL****#
#*********
##3-year##
table(data_2006$survive_3yr) #0=12449, 1=21743

numerator_3yrALL_2006 <- length(which(data_2006$survive_3yr==1))

denominator_3yrALL_2006 <- length(data_2006$survive_3yr)

survival_3yrALL_2006 <- numerator_3yrALL_2006/denominator_3yrALL_2006
survival_3yrALL_2006

##5-year##
table(data_2006$survive_5yr) #0=10168, 1=17266

numerator_5yrALL_2006 <- length(which(data_2006$survive_5yr==1))

denominator_5yrALL_2006 <- length(data_2006$survive_5yr)

survival_5yrALL_2006 <- numerator_5yrALL_2006/denominator_5yrALL_2006
survival_5yrALL_2006

##10-year##
table(data_2006$survive_10yr) #0=10168, 1=17266

numerator_10yrALL_2006 <- length(which(data_2006$survive_10yr==1))

denominator_10yrALL_2006 <- length(data_2006$survive_10yr)

survival_10yrALL_2006 <- numerator_10yrALL_2006/denominator_10yrALL_2006
survival_10yrALL_2006
#****************#
#Graduation Rates#
#****************#
table(data_2006$graduated)

numerator_grad_2006 <- length(which(data_2006$graduated==1))

denominator_grad_2006 <- length(data_2006$graduated)

graduated_2006 <- numerator_grad_2006/denominator_grad_2006
graduated_2006

##for only those who survived 10 years
numerator_grad_2006_10yr <- length(which(data_2006$graduated==1 & data_2006$survive_10yr==1))

denominator_grad_2006_10yr <- length(data_2006$graduated)

graduated_2006_10yr <- numerator_grad_2006_10yr/denominator_grad_2006_10yr
graduated_2006_10yr



#*****#
#***SMALL***
#*******#
##3-year##
table(data_2006$survive_3yr) #0=12449, 1=21743

numerator_3yrSM_2006 <- length(which(data_2006$survive_3yr==1 & data_2006$biz_size==1))

denominator_3yrSM_2006 <- length(which(data_2006$biz_size==1))

survival_3yrSM_2006 <- numerator_3yrSM_2006/denominator_3yrSM_2006
survival_3yrSM_2006 

##5-year##
table(data_2006$survive_5yr) #0=10168, 1=17266

numerator_5yrSM_2006 <- length(which(data_2006$survive_5yr==1 & data_2006$biz_size==1))

denominator_5yrSM_2006 <- length(which(data_2006$biz_size==1))

survival_5yrSM_2006 <- numerator_5yrSM_2006/denominator_5yrSM_2006
survival_5yrSM_2006 

##10-year##
table(data_2006$survive_10yr) #0=10168, 1=17266

numerator_10yrSM_2006 <- length(which(data_2006$survive_10yr==1 & data_2006$biz_size==1))

denominator_10yrSM_2006 <- length(which(data_2006$biz_size==1))

survival_10yrSM_2006 <- numerator_10yrSM_2006/denominator_10yrSM_2006
survival_10yrSM_2006


#********#
#*NON SMALL*#
#*********#
##3-year##
table(data_2006$survive_3yr) #0=12449, 1=21743

numerator_3yrNSM_2006 <- length(which(data_2006$survive_3yr==1 & data_2006$biz_size==0))

denominator_3yrNSM_2006 <- length(which(data_2006$biz_size==0))

survival_3yrNSM_2006 <- numerator_3yrNSM_2006/denominator_3yrNSM_2006
survival_3yrNSM_2006

##5-year##
table(data_2006$survive_5yr) #0=10168, 1=17266

numerator_5yrNSM_2006 <- length(which(data_2006$survive_5yr==1 & data_2006$biz_size==0))

denominator_5yrNSM_2006 <- length(which(data_2006$biz_size==0))

survival_5yrNSM_2006 <- numerator_5yrNSM_2006/denominator_5yrNSM_2006
survival_5yrNSM_2006 

##10-year##
table(data_2006$survive_10yr) #0=10168, 1=17266

numerator_10yrNSM_2006 <- length(which(data_2006$survive_10yr==1 & data_2006$biz_size==0))

denominator_10yrNSM_2006 <- length(which(data_2006$biz_size==0))

survival_10yrNSM_2006 <- numerator_10yrNSM_2006/denominator_10yrNSM_2006
survival_10yrNSM_2006 

#***************#
####2006 DOD ONLY####
#***************#
#create necessary vars#

data_2006_DOD <- FPDS_cleaned_unique[!(FPDS_cleaned_unique$registrationYear!="2006"), ]
data_2006_DOD <- data_2006_DOD[!(data_2006_DOD$customer!="Defense"), ]


##create variable describing whether a firm survived 3 years

data_2006_DOD <- data_2006_DOD %>%
  dplyr::mutate(survive_3yr = ifelse(exitYear < 2008, "0", "1")) 

##create variable describing whether a firm survived 5 years
data_2006_DOD <- data_2006_DOD %>%
  dplyr::mutate(survive_5yr = ifelse(exitYear < 2011, "0", "1")) 


##create variable describing whether a firm survived 10 years
data_2006_DOD <- data_2006_DOD %>%
  dplyr::mutate(survive_10yr = ifelse(exitYear < 2015, "0", "1")) 

#*************#
#survival rates#
#**************#
#*********#
#**ALL****#
#*********
##3-year##
table(data_2006_DOD$survive_3yr) #0=12449, 1=21743

numerator_3yrALL_2006_DOD <- length(which(data_2006_DOD$survive_3yr==1))

denominator_3yrALL_2006_DOD <- length(data_2006_DOD$survive_3yr)

survival_3yrALL_2006_DOD <- numerator_3yrALL_2006_DOD/denominator_3yrALL_2006_DOD
survival_3yrALL_2006_DOD

##5-year##
table(data_2006_DOD$survive_5yr) #0=10168, 1=17266

numerator_5yrALL_2006_DOD <- length(which(data_2006_DOD$survive_5yr==1))

denominator_5yrALL_2006_DOD <- length(data_2006_DOD$survive_5yr)

survival_5yrALL_2006_DOD <- numerator_5yrALL_2006_DOD/denominator_5yrALL_2006_DOD
survival_5yrALL_2006_DOD

##10-year##
table(data_2006_DOD$survive_10yr) #0=10168, 1=17266

numerator_10yrALL_2006_DOD <- length(which(data_2006_DOD$survive_10yr==1))

denominator_10yrALL_2006_DOD <- length(data_2006_DOD$survive_10yr)

survival_10yrALL_2006_DOD <- numerator_10yrALL_2006_DOD/denominator_10yrALL_2006_DOD
survival_10yrALL_2006_DOD
#****************#
#Graduation Rates#
#****************#
table(data_2006_DOD$graduated)

numerator_grad_2006_DOD <- length(which(data_2006_DOD$graduated==1))

denominator_grad_2006_DOD <- length(data_2006_DOD$graduated)

graduated_2006_DOD <- numerator_grad_2006_DOD/denominator_grad_2006_DOD
graduated_2006_DOD

##graduation for those who survived after 10 years only
numerator_grad_2006_DOD_10yr <- length(which(data_2006_DOD$graduated==1 & data_2006_DOD$survive_10yr==1))

denominator_grad_2006_DOD_10yr <- length(data_2006_DOD$graduated)

graduated_2006_DOD_10yr <- numerator_grad_2006_DOD_10yr/denominator_grad_2006_DOD_10yr
graduated_2006_DOD_10yr


#*****#
#***SMALL***
#*******#
##3-year##
table(data_2006_DOD$survive_3yr) #0=12449, 1=21743

numerator_3yrSM_2006_DOD <- length(which(data_2006_DOD$survive_3yr==1 & data_2006_DOD$biz_size==1))

denominator_3yrSM_2006_DOD <- length(which(data_2006_DOD$biz_size==1))

survival_3yrSM_2006_DOD <- numerator_3yrSM_2006_DOD/denominator_3yrSM_2006_DOD
survival_3yrSM_2006_DOD 

##5-year##
table(data_2006_DOD$survive_5yr) #0=10168, 1=17266

numerator_5yrSM_2006_DOD <- length(which(data_2006_DOD$survive_5yr==1 & data_2006_DOD$biz_size==1))

denominator_5yrSM_2006_DOD <- length(which(data_2006_DOD$biz_size==1))

survival_5yrSM_2006_DOD <- numerator_5yrSM_2006_DOD/denominator_5yrSM_2006_DOD
survival_5yrSM_2006_DOD 

##10-year##
table(data_2006_DOD$survive_10yr) #0=10168, 1=17266

numerator_10yrSM_2006_DOD <- length(which(data_2006_DOD$survive_10yr==1 & data_2006_DOD$biz_size==1))

denominator_10yrSM_2006_DOD <- length(which(data_2006_DOD$biz_size==1))

survival_10yrSM_2006_DOD <- numerator_10yrSM_2006_DOD/denominator_10yrSM_2006_DOD
survival_10yrSM_2006_DOD


#********#
#*NON SMALL*#
#*********#
##3-year##
table(data_2006_DOD$survive_3yr) #0=12449, 1=21743

numerator_3yrNSM_2006_DOD <- length(which(data_2006_DOD$survive_3yr==1 & data_2006_DOD$biz_size==0))

denominator_3yrNSM_2006_DOD <- length(which(data_2006_DOD$biz_size==0))

survival_3yrNSM_2006_DOD <- numerator_3yrNSM_2006_DOD/denominator_3yrNSM_2006_DOD
survival_3yrNSM_2006_DOD

##5-year##
table(data_2006_DOD$survive_5yr) #0=10168, 1=17266

numerator_5yrNSM_2006_DOD <- length(which(data_2006_DOD$survive_5yr==1 & data_2006_DOD$biz_size==0))

denominator_5yrNSM_2006_DOD <- length(which(data_2006_DOD$biz_size==0))

survival_5yrNSM_2006_DOD <- numerator_5yrNSM_2006_DOD/denominator_5yrNSM_2006_DOD
survival_5yrNSM_2006_DOD 

##10-year##
table(data_2006_DOD$survive_10yr) #0=10168, 1=17266

numerator_10yrNSM_2006_DOD <- length(which(data_2006_DOD$survive_10yr==1 & data_2006_DOD$biz_size==0))

denominator_10yrNSM_2006_DOD <- length(which(data_2006_DOD$biz_size==0))

survival_10yrNSM_2006_DOD <- numerator_10yrNSM_2006_DOD/denominator_10yrNSM_2006_DOD
survival_10yrNSM_2006_DOD 

####Display all graduation rates####
#2001
graduatedALL_2001_10yr
graduatedALL_2001_DoD_10yr

#2002
graduated_2002_10yr
graduated_2002_DOD_10yr

#2003
graduated_2003_10yr
graduated_2003_DOD_10yr

#2004
graduated_2004_10yr
graduated_2004_DOD_10yr

#2005
graduated_2005_10yr
graduated_2005_DOD_10yr


#2006
graduated_2006_10yr
graduated_2006_DOD_10yr

#********************************************************************

#********************************************************************
####% of obligations that go to different subgroups of new entrants in each year####
#********************************************************************

##drop observations with Registration Year before 2000
FPDS_cleaned_unique_graphs <- FPDS_cleaned_unique[!(FPDS_cleaned_unique$registrationYear<2001), ]
FPDS_cleaned_unique_graphs <- FPDS_cleaned_unique[!(FPDS_cleaned_unique$registrationYear>2016), ]

 
#**********
##%of obligations that go to new entrants as opposed to incumbent firms in each year##
#**********

#*********
##%of obligations that go to small and non-small new entrants in each year##
#*********
##ALL Fed Agencies##

##creates a dataframe that counts the total number of obligations in each year
count_total_obligations <- FPDS_cleaned_unique_graphs %>% 
  filter(top_smallbiz_bin == 1 | top_smallbiz_bin == 0) %>% 
  group_by(registrationYear) %>% 
  dplyr::summarise(sum_obligations = sum(total_obligations)) 
  


##create a dataframe that calculates the number of obligations that go to small vendors in each
#year and then number of obligations that go to non-small vendors in each year and joins it 
#with the counts of total number of obligations in each year and then calculate the percent
#of obligations that go to each group in each year
FPDS_obligationscount <- FPDS_cleaned_unique_graphs %>%
  filter(top_smallbiz_bin == 1 | top_smallbiz_bin == 0) %>%
  group_by(registrationYear, top_smallbiz_bin) %>%
  dplyr::summarise(sum_obligations = sum(total_obligations)) %>%
  dplyr::rename("tot_obl_bysize"=`sum_obligations`) %>%
  left_join(count_total_obligations, by = "registrationYear") %>%
  dplyr::rename("tot_obl_byyear"=`sum_obligations`) %>%
  dplyr::mutate(percent_obl_dec = tot_obl_bysize / tot_obl_byyear) %>%
  dplyr::mutate(percent_obl = percent_obl_dec * 100) %>%
  dplyr::mutate(percent_obl = round(percent_obl, 0)) %>%
  dplyr::mutate(total_percent = 100)


ggplot(FPDS_obligationscount, aes(x = registrationYear, y = tot_obl_bysize, fill = factor(top_smallbiz_bin), label = percent_obl)) +
  geom_bar(stat = 'identity', position = 'stack') +
  ylab("Total Obligations") +
  xlab("Entry Year") +
  scale_x_continuous(breaks = c(2001:2016)) +
  ##scale_fill_manual(name = "New Entrants Types", values = c("deepskyblue", "royalblue1"), labels = c("small", "non-small")) +
  scale_fill_manual(name = "New Entrants Types", values = c("darkslategray1", "cadetblue4"), labels = c("non-small", "small")) +
  ggtitle("Percent of Obligations for Small and Non-Small New Entrants (2001-2016) - All Federal Agencies")+
  ##geom_text_repel(data = subset(FPDS_bargraphCount, registrationYear >=2014), aes(label = regpersize), size = 4, box.padding = .1, 
  ###    angle = 45) +
  ##geom_text(data = subset(FPDS_bargraphCount, registrationYear < 2014), aes(label = regpersize), size = 4, position = position_stack(vjust = .5), angle = 45)
  geom_text_repel(data = subset(FPDS_obligationscount, registrationYear <= 2016), aes(label = scales::percent(percent_obl_dec)), size = 4, position = position_stack(vjust = .3), angle = 90)



##for DoD only##

FPDS_cleaned_unique_graphs_DOD <- FPDS_cleaned_unique_graphs[(FPDS_cleaned_unique_graphs$customer=="Defense"), ]

##creates a dataframe that counts the total number of obligations in each year
count_total_obligations_DOD <- FPDS_cleaned_unique_graphs_DOD %>% 
  filter(top_smallbiz_bin == 1 | top_smallbiz_bin == 0) %>% 
  group_by(registrationYear) %>% 
  dplyr::summarise(sum_obligations = sum(total_obligations)) 



##create a dataframe that calculates the number of obligations that go to small vendors in each
#year and then number of obligations that go to non-small vendors in each year and joins it 
#with the counts of total number of obligations in each year and then calculate the percent
#of obligations that go to each group in each year
FPDS_obligationscount_DOD <- FPDS_cleaned_unique_graphs_DOD %>%
  filter(top_smallbiz_bin == 1 | top_smallbiz_bin == 0) %>%
  group_by(registrationYear, top_smallbiz_bin) %>%
  dplyr::summarise(sum_obligations = sum(total_obligations)) %>%
  dplyr::rename("tot_obl_bysize"=`sum_obligations`) %>%
  left_join(count_total_obligations_DOD, by = "registrationYear") %>%
  dplyr::rename("tot_obl_byyear"=`sum_obligations`) %>%
  dplyr::mutate(percent_obl_dec = tot_obl_bysize / tot_obl_byyear) %>%
  dplyr::mutate(percent_obl = percent_obl_dec * 100) %>%
  dplyr::mutate(percent_obl = round(percent_obl, 0)) %>%
  dplyr::mutate(total_percent = 100)
 

ggplot(FPDS_obligationscount_DOD, aes(x = registrationYear, y = tot_obl_bysize, fill = factor(top_smallbiz_bin), label = percent_obl)) +
  geom_bar(stat = 'identity', position = 'stack') +
  ylab("Total Obligations") +
  xlab("Entry Year") +
  scale_x_continuous(breaks = c(2001:2016)) +
  ##scale_fill_manual(name = "New Entrants Types", values = c("deepskyblue", "royalblue1"), labels = c("small", "non-small")) +
  scale_fill_manual(name = "New Entrants Types", values = c("darkslategray1", "cadetblue4"), labels = c("non-small", "small")) +
  ggtitle("Percent of Obligations for Small and Non-Small New Entrants (2001-2016) - All Federal Agencies")+
  ##geom_text_repel(data = subset(FPDS_bargraphCount, registrationYear >=2014), aes(label = regpersize), size = 4, box.padding = .1, 
  ###    angle = 45) +
  ##geom_text(data = subset(FPDS_bargraphCount, registrationYear < 2014), aes(label = regpersize), size = 4, position = position_stack(vjust = .5), angle = 45)
  geom_text_repel(data = subset(FPDS_obligationscount_DOD, registrationYear <= 2016), aes(label = scales::percent(percent_obl_dec)), size = 4, position = position_stack(vjust = .3), angle = 90)



#*********
##%of obligations that go to small and non-small incumbent firms in each year##
#*********



#********#
##%of obligations that go to different setaside programs
#********#


##******#
##% of obligations that go to graduated firms
#*******#

##create a variable that describes whether a firm graduated and survived 10 yrs

#step one, append all yearly datas to have survived 3 yr, 5 yr, 10 yt etc
FPDS_all_yrs1 <- rbind(data_2001, data_2002)

FPDS_all_yrs2 <- rbind(FPDS_all_yrs1, data_2003)

FPDS_all_yrs3 <- rbind(FPDS_all_yrs2, data_2004)

FPDS_all_yrs4 <- rbind(FPDS_all_yrs3, data_2005)

FPDS_all_yrs5 <- rbind(FPDS_all_yrs4, data_2006)


#step two, create 


##creates a dataframe that counts the total number of obligations in each year
count_total_obligations <- FPDS_cleaned_unique_graphs %>% 
  filter(top_smallbiz_bin == 1 | top_smallbiz_bin == 0) %>% 
  group_by(registrationYear) %>% 
  dplyr::summarise(sum_obligations = sum(total_obligations)) 



##create a dataframe that calculates the number of obligations that go to small vendors in each
#year and then number of obligations that go to non-small vendors in each year and joins it 
#with the counts of total number of obligations in each year and then calculate the percent
#of obligations that go to each group in each year
FPDS_obligationscount <- FPDS_cleaned_unique_graphs %>%
  filter(top_smallbiz_bin == 1 | top_smallbiz_bin == 0) %>%
  group_by(registrationYear, top_smallbiz_bin) %>%
  dplyr::summarise(sum_obligations = sum(total_obligations)) %>%
  dplyr::rename("tot_obl_bysize"=`sum_obligations`) %>%
  left_join(count_total_obligations, by = "registrationYear") %>%
  dplyr::rename("tot_obl_byyear"=`sum_obligations`) %>%
  dplyr::mutate(percent_obl_dec = tot_obl_bysize / tot_obl_byyear) %>%
  dplyr::mutate(percent_obl = percent_obl_dec * 100) %>%
  dplyr::mutate(percent_obl = round(percent_obl, 0)) %>%
  dplyr::mutate(total_percent = 100)


ggplot(FPDS_obligationscount, aes(x = registrationYear, y = tot_obl_bysize, fill = factor(top_smallbiz_bin), label = percent_obl)) +
  geom_bar(stat = 'identity', position = 'stack') +
  ylab("Total Obligations") +
  xlab("Entry Year") +
  scale_x_continuous(breaks = c(2001:2016)) +
  ##scale_fill_manual(name = "New Entrants Types", values = c("deepskyblue", "royalblue1"), labels = c("small", "non-small")) +
  scale_fill_manual(name = "New Entrants Types", values = c("darkslategray1", "cadetblue4"), labels = c("non-small", "small")) +
  ggtitle("Percent of Obligations for Small and Non-Small New Entrants (2001-2016) - All Federal Agencies")+
  ##geom_text_repel(data = subset(FPDS_bargraphCount, registrationYear >=2014), aes(label = regpersize), size = 4, box.padding = .1, 
  ###    angle = 45) +
  ##geom_text(data = subset(FPDS_bargraphCount, registrationYear < 2014), aes(label = regpersize), size = 4, position = position_stack(vjust = .5), angle = 45)
  geom_text_repel(data = subset(FPDS_obligationscount, registrationYear <= 2016), aes(label = scales::percent(percent_obl_dec)), size = 4, position = position_stack(vjust = .3), angle = 90)

##All Fed Agencies##
##creates a dataframe that counts the total number of obligations in each year
count_total_obligations <- FPDS_cleaned_unique_graphs %>% 
  filter(top_smallbiz_bin == 1 | top_smallbiz_bin == 0) %>% 
  group_by(registrationYear) %>% 
  dplyr::summarise(sum_obligations = sum(total_obligations)) 




##for DoD only##






