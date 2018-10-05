##Executes New Entrants analysis on DoD entries vs. non-DoD entries

#******************************************************************
########################SET UP################################
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
library(ggthemes)
library(extrafont)
library(scales)
library(magrittr)
library(csis360)
library(scales)
library(gtable)
library(grid)
library(gridExtra)

#load sc data
setwd("K:/2018-01 NPS New Entrants/Data/Data/Cleaning data/FPDS")
load(file = "FPDS_datapull_all_v3_allfed.Rda")

#drop observations with Registration Year before 2001
FPDS_cleaned_unique <- FPDS_cleaned_unique[!(FPDS_cleaned_unique$registrationYear<2001), ]
FPDS_cleaned_unique <- FPDS_cleaned_unique[!(FPDS_cleaned_unique$registrationYear>2016), ]

length(unique(FPDS_cleaned_unique$Dunsnumber)) == nrow(FPDS_cleaned_unique)

#seperate DoD and non-DoD
FPDS_cleaned_unique_nd <- FPDS_cleaned_unique %>% #non-DoD
  filter(customer != "Defense")

FPDS_cleaned_unique_DOD <- FPDS_cleaned_unique %>% #non-DoD
  filter(customer == "Defense")


#******************************************************************************************************
#*******************#
#####Non-DOD STATS#####
#*******************#

#####Non-DoD 2001#####
#*******************#
#subset the 2001 data
data_2001_nd <- FPDS_cleaned_unique_nd[!(FPDS_cleaned_unique_nd$registrationYear!="2001"), ]

data_2001_nd <- data_2001_nd %>%
  dplyr::mutate(survive_3yr = ifelse(exitYear < 2003, "0", "1")) 

##create variable describing whether a firm survived 5 years
data_2001_nd <- data_2001_nd %>%
  dplyr::mutate(survive_5yr = ifelse(exitYear < 2005, "0", "1")) 


##create variable describing whether a firm survived 10 years
data_2001_nd <- data_2001_nd %>%
  dplyr::mutate(survive_10yr = ifelse(exitYear < 2010, "0", "1")) 

##create variable describing whether a firm survived in 2016
data_2001_nd <- data_2001_nd %>%
  dplyr::mutate(survive_2016 = ifelse(exitYear < 2016, "0", "1"))


str(data_2001_nd)

data_2001_nd$survive_3yr<-as.numeric(as.character(data_2001_nd$survive_3yr))
data_2001_nd$survive_5yr<-as.numeric(as.character(data_2001_nd$survive_5yr))
data_2001_nd$survive_10yr<-as.numeric(as.character(data_2001_nd$survive_10yr))
data_2001_nd$survive_2016<-as.numeric(as.character(data_2001_nd$survive_2016))

str(data_2001_nd)

##t-test between small and nonsmall survival rates##
#3-year#

table(data_2001_nd$top_smallbiz_bin)
table(data_2001_nd$survive_3yr)
table(data_2001_nd$top_smallbiz_bin, data_2001_nd$survive_3yr)

t.test(survive_3yr ~ top_smallbiz_bin, data = data_2001_nd)

#5-year#
table(data_2001_nd$top_smallbiz_bin, data_2001_nd$survive_5yr)

t.test(survive_5yr ~ top_smallbiz_bin, data = data_2001_nd)

#10-year#
table(data_2001_nd$top_smallbiz_bin, data_2001_nd$survive_10yr)

t.test(survive_10yr ~ top_smallbiz_bin, data = data_2001_nd)


#*****************
#Survival#
#*****************
#************#
#*****ALL****#
#************#
##3-year##
table(data_2001_nd$survive_3yr) #0=10168, 1=17266

numerator_3yrALL_2001_nd <- length(which(data_2001_nd$survive_3yr==1))

denominator_3yrALL_2001_nd <- length(data_2001_nd$survive_3yr)

survival_3yrALL_2001_nd <- numerator_3yrALL_2001_nd/denominator_3yrALL_2001_nd
survival_3yrALL_2001_nd

##5-year##
table(data_2001_nd$survive_5yr) #0=10168, 1=17266

numerator_5yrALL_2001_nd <- length(which(data_2001_nd$survive_5yr==1))

denominator_5yrALL_2001_nd <- length(data_2001_nd$survive_5yr)

survival_5yrALL_2001_nd <- numerator_5yrALL_2001_nd/denominator_5yrALL_2001_nd
survival_5yrALL_2001_nd

##10-year##
table(data_2001_nd$survive_10yr) #0=10168, 1=17266

numerator_10yrALL_2001_nd <- length(which(data_2001_nd$survive_10yr==1))

denominator_10yrALL_2001_nd <- length(data_2001_nd$survive_10yr)

survival_10yrALL_2001_nd <- numerator_10yrALL_2001_nd/denominator_10yrALL_2001_nd
survival_10yrALL_2001_nd

#**********#
#Graduation#
#**********#
##for all firms
table(data_2001_nd$graduated)

numerator_gradALL_2001_nd <- length(which(data_2001_nd$graduated==1))

denominator_gradALL_2001_nd <- length(data_2001_nd$graduated)

graduatedALL_2001_nd <- numerator_gradALL_2001_nd/denominator_gradALL_2001_nd
graduatedALL_2001_nd

##for firms that only survived to the end of the study period
numerator_gradALL_2001_nd_10yr <- length(which(data_2001_nd$graduated==1 & data_2001_nd$survive_10yr==1))

#denominator_gradALL_2001_nd_10yr <- length(data_2001_nd$graduated)

denominator_gradALL_2001_nd_10yr <- length(which(data_2001_nd$top_smallbiz_bin==1))

graduatedALL_2001_nd_10yr <- numerator_gradALL_2001_nd_10yr/denominator_gradALL_2001_nd_10yr
graduatedALL_2001_nd_10yr


#**************************
#**********#
#***SMALL**#
#**********#
##3-year##
table(data_2001_nd$survive_3yr) #0=10168, 1=17266

numerator_3yrSM_2001_nd <- length(which(data_2001_nd$survive_3yr==1 & data_2001_nd$top_smallbiz_bin==1))

denominator_3yrSM_2001_nd <- length(which(data_2001_nd$top_smallbiz_bin==1))

survival_3yrSM_2001_nd <- numerator_3yrSM_2001_nd/denominator_3yrSM_2001_nd
survival_3yrSM_2001_nd

##5-year##
table(data_2001_nd$survive_5yr) #0=10168, 1=17266

numerator_5yrSM_2001_nd <- length(which(data_2001_nd$survive_5yr==1 & data_2001_nd$top_smallbiz_bin==1))

denominator_5yrSM_2001_nd <- length(which(data_2001_nd$top_smallbiz_bin==1))

survival_5yrSM_2001_nd <- numerator_5yrSM_2001_nd/denominator_5yrSM_2001_nd
survival_5yrSM_2001_nd

##10-year##
table(data_2001_nd$survive_10yr) #0=10168, 1=17266

numerator_10yrSM_2001_nd <- length(which(data_2001_nd$survive_10yr==1 & data_2001_nd$top_smallbiz_bin==1))

denominator_10yrSM_2001_nd <- length(which(data_2001_nd$top_smallbiz_bin==1))

survival_10yrSM_2001_nd <- numerator_10yrSM_2001_nd/denominator_10yrSM_2001_nd
survival_10yrSM_2001_nd


#**********#
#***NON-SMALL**#
#**********#

##3-year##
table(data_2001_nd$survive_3yr) #0=10168, 1=17266

numerator_3yrNSM_2001_nd <- length(which(data_2001_nd$survive_3yr==1 & data_2001_nd$top_smallbiz_bin==0))

denominator_3yrNSM_2001_nd <- length(which(data_2001_nd$top_smallbiz_bin==0))

survival_3yrNSM_2001_nd <- numerator_3yrNSM_2001_nd/denominator_3yrNSM_2001_nd
survival_3yrNSM_2001_nd

##5-year##
table(data_2001_nd$survive_5yr) #0=10168, 1=17266

numerator_5yrNSM_2001_nd <- length(which(data_2001_nd$survive_5yr==1 & data_2001_nd$top_smallbiz_bin==0))

denominator_5yrNSM_2001_nd <- length(which(data_2001_nd$top_smallbiz_bin==0))

survival_5yrNSM_2001_nd <- numerator_5yrNSM_2001_nd/denominator_5yrNSM_2001_nd
survival_5yrNSM_2001_nd

##10-year##
table(data_2001_nd$survive_10yr) #0=10168, 1=17266

numerator_10yrNSM_2001_nd <- length(which(data_2001_nd$survive_10yr==1 & data_2001_nd$top_smallbiz_bin==0))

denominator_10yrNSM_2001_nd <- length(which(data_2001_nd$top_smallbiz_bin==0))

survival_10yrNSM_2001_nd <- numerator_10yrNSM_2001_nd/denominator_10yrNSM_2001_nd
survival_10yrNSM_2001_nd

#*******************#
#******2016 check***#
#*******************#
##*********
#***2016 survival 
#**********
survive_10yr_count_2001_nd <- length(which(data_2001_nd$survive_10yr==1))
##ALL##
numerator_2016_ALL_2001_nd <- length(which(data_2001_nd$survive_2016==1))

denominator_2016_ALL_2001_nd <- length(data_2001_nd$survive_2016)

survivalrate_2016_ALL_2001_nd <- numerator_2016_ALL_2001_nd/denominator_2016_ALL_2001_nd
survivalrate_2016_ALL_2001_nd


##SMALL##
numerator_2016_SM_2001_nd <- length(which(data_2001_nd$survive_2016==1 & data_2001_nd$top_smallbiz_bin==1))

denominator_2016_SM_2001_nd <- length(which(data_2001_nd$top_smallbiz_bin==1))

survivalrate_2016_SM_2001_nd <- numerator_2016_SM_2001_nd/denominator_2016_SM_2001_nd
survivalrate_2016_SM_2001_nd


##NONSMALL##
numerator_2016_NS_2001_nd <- length(which(data_2001_nd$survive_2016==1 & data_2001_nd$top_smallbiz_bin==0))

denominator_2016_NS_2001_nd <- length(which(data_2001_nd$top_smallbiz_bin==0))

survivalrate_2016_NS_2001_nd <- numerator_2016_NS_2001_nd/denominator_2016_NS_2001_nd
survivalrate_2016_NS_2001_nd


table(data_2001_nd$top_smallbiz_bin, data_2001_nd$survive_2016)


t.test(survive_2016 ~ top_smallbiz_bin, data = data_2001_nd)


#####Non-DoD 2002#####
#*******************#
#subset the 2002 data
data_2002_nd <- FPDS_cleaned_unique_nd[!(FPDS_cleaned_unique_nd$registrationYear!="2002"), ]

##create variable describing whether a firm survived 3 years

data_2002_nd <- data_2002_nd %>%
  dplyr::mutate(survive_3yr = ifelse(exitYear < 2004, "0", "1")) 

##create variable describing whether a firm survived 5 years
data_2002_nd <- data_2002_nd %>%
  dplyr::mutate(survive_5yr = ifelse(exitYear < 2006, "0", "1")) 


##create variable describing whether a firm survived 10 years
data_2002_nd <- data_2002_nd %>%
  dplyr::mutate(survive_10yr = ifelse(exitYear < 2011, "0", "1")) 

##create variable describing whether a firm survived in 2016
data_2002_nd <- data_2002_nd %>%
  dplyr::mutate(survive_2016 = ifelse(exitYear < 2016, "0", "1"))


str(data_2002_nd)

data_2002_nd$survive_3yr<-as.numeric(as.character(data_2002_nd$survive_3yr))
data_2002_nd$survive_5yr<-as.numeric(as.character(data_2002_nd$survive_5yr))
data_2002_nd$survive_10yr<-as.numeric(as.character(data_2002_nd$survive_10yr))
data_2002_nd$survive_2016<-as.numeric(as.character(data_2002_nd$survive_2016))

str(data_2002_nd)

##t test to test the differences between small and non small survival##
table(data_2002_nd$top_smallbiz_bin)
table(data_2002_nd$survive_3yr)
table(data_2002_nd$top_smallbiz_bin, data_2002_nd$survive_3yr)

t.test(survive_3yr ~ top_smallbiz_bin, data = data_2002_nd)

#5-year#
table(data_2002_nd$top_smallbiz_bin, data_2002_nd$survive_5yr)

t.test(survive_5yr ~ top_smallbiz_bin, data = data_2002_nd)

#10-year#
table(data_2002_nd$top_smallbiz_bin, data_2002_nd$survive_10yr)

t.test(survive_10yr ~ top_smallbiz_bin, data = data_2002_nd)


#*************
#***ALL******#
#*************#
#*****************
##Survival Rates##
#*****************

##3-year##
table(data_2002_nd$survive_3yr) #0=12449, 1=21743

numerator_3yrALL2002_nd <- length(which(data_2002_nd$survive_3yr==1))

denominator_3yrALL2002_nd <- length(data_2002_nd$survive_3yr)

survival_3yrALL2002_nd <- numerator_3yrALL2002_nd/denominator_3yrALL2002_nd
survival_3yrALL2002_nd

##5-year##
table(data_2002_nd$survive_5yr) #0=10168, 1=17266

numerator_5yrALL2002_nd <- length(which(data_2002_nd$survive_5yr==1))

denominator_5yrALL2002_nd <- length(data_2002_nd$survive_5yr)

survival_5yrALL2002_nd <- numerator_5yrALL2002_nd/denominator_5yrALL2002_nd
survival_5yrALL2002_nd

##10-year##
table(data_2002_nd$survive_10yr) #0=10168, 1=17266

numerator_10yrALL2002_nd <- length(which(data_2002_nd$survive_10yr==1))

denominator_10yrALL2002_nd <- length(data_2002_nd$survive_10yr)

survival_10yrALL2002_nd <- numerator_10yrALL2002_nd/denominator_10yrALL2002_nd
survival_10yrALL2002_nd 
#****************#
#Graduation Rates#
#****************#
table(data_2002_nd$graduated)

numerator_grad2002_nd <- length(which(data_2002_nd$graduated==1))

denominator_grad2002_nd <- length(data_2002_nd$graduated)

graduated2002_nd <- numerator_grad2002_nd/denominator_grad2002_nd
graduated2002_nd

##for those that survived 10 years
numerator_grad2002_nd_10yr <- length(which(data_2002_nd$graduated==1 & data_2002_nd$survive_10yr==0))

#denominator_grad2002_nd_10yr <- length(data_2002_nd$graduated)

denominator_grad2002_nd_10yr <- length(which(data_2002_nd$top_smallbiz_bin==1))

graduated_2002_nd_10yr <- numerator_grad2002_nd_10yr/denominator_grad2002_nd_10yr
graduated_2002_nd_10yr


#**********#
#***SMALL**#
#**********#
##3-year##
table(data_2002_nd$survive_3yr) #0=12449, 1=21743

numerator_3yrSM2002_nd <- length(which(data_2002_nd$survive_3yr==1 & data_2002_nd$top_smallbiz_bin==1))

denominator_3yrSM2002_nd <- length(which(data_2002_nd$top_smallbiz_bin==1))

survival_3yrSM2002_nd <- numerator_3yrSM2002_nd/denominator_3yrSM2002_nd
survival_3yrSM2002_nd

##5-year##
table(data_2002_nd$survive_5yr) #0=10168, 1=17266

numerator_5yrSM2002_nd <- length(which(data_2002_nd$survive_5yr==1 & data_2002_nd$top_smallbiz_bin==1))

denominator_5yrSM2002_nd <- length(which(data_2002_nd$top_smallbiz_bin==1))

survival_5yrSM2002_nd <- numerator_5yrSM2002_nd/denominator_5yrSM2002_nd
survival_5yrSM2002_nd 

##10-year##
table(data_2002_nd$survive_10yr) #0=10168, 1=17266

numerator_10yrSM2002_nd <- length(which(data_2002_nd$survive_10yr==1 & data_2002_nd$top_smallbiz_bin==1))

denominator_10yrSM2002_nd <- length(which(data_2002_nd$top_smallbiz_bin==1))

survival_10yrSM2002_nd <- numerator_10yrSM2002_nd/denominator_10yrSM2002_nd
survival_10yrSM2002_nd

#**********#
#***NON-SMALL*#
#***********#
##3-year##
table(data_2002_nd$survive_3yr) #0=12449, 1=21743

numerator_3yrNSM2002_nd <- length(which(data_2002_nd$survive_3yr==1 & data_2002_nd$top_smallbiz_bin==0))

denominator_3yrNSM2002_nd <- length(which(data_2002_nd$top_smallbiz_bin==0))

survival_3yrNSM2002_nd <- numerator_3yrNSM2002_nd/denominator_3yrNSM2002_nd
survival_3yrNSM2002_nd

##5-year##
table(data_2002_nd$survive_5yr) #0=10168, 1=17266

numerator_5yrNSM2002_nd <- length(which(data_2002_nd$survive_5yr==1 & data_2002_nd$top_smallbiz_bin==0))

denominator_5yrNSM2002_nd <- length(which(data_2002_nd$top_smallbiz_bin==0))

survival_5yrNSM2002_nd <- numerator_5yrNSM2002_nd/denominator_5yrNSM2002_nd
survival_5yrNSM2002_nd 

##10-year##
table(data_2002_nd$survive_10yr) #0=10168, 1=17266

numerator_10yrNSM2002_nd <- length(which(data_2002_nd$survive_10yr==1 & data_2002_nd$top_smallbiz_bin==0))

denominator_10yrNSM2002_nd <- length(which(data_2002_nd$top_smallbiz_bin==0))

survival_10yrNSM2002_nd <- numerator_10yrNSM2002_nd/denominator_10yrNSM2002_nd
survival_10yrNSM2002_nd

#*******************#
#******2016 check***#
#*******************#
##*********
#***2016 survival 
#**********
survive_10yr_count_2002_nd <- length(which(data_2002_nd$survive_10yr==1))
##ALL##
numerator_2016_ALL_2002_nd <- length(which(data_2002_nd$survive_2016==1))

denominator_2016_ALL_2002_nd <- length(data_2002_nd$survive_2016)

survivalrate_2016_ALL_2002_nd <- numerator_2016_ALL_2002_nd/denominator_2016_ALL_2002_nd
survivalrate_2016_ALL_2002_nd


##SMALL##
numerator_2016_SM_2002_nd <- length(which(data_2002_nd$survive_2016==1 & data_2002_nd$top_smallbiz_bin==1))

denominator_2016_SM_2002_nd <- length(which(data_2002_nd$top_smallbiz_bin==1))

survivalrate_2016_SM_2002_nd <- numerator_2016_SM_2002_nd/denominator_2016_SM_2002_nd
survivalrate_2016_SM_2002_nd


##NONSMALL##
numerator_2016_NS_2002_nd<- length(which(data_2002_nd$survive_2016==1 & data_2002_nd$top_smallbiz_bin==0))

denominator_2016_NS_2002_nd <- length(which(data_2002_nd$top_smallbiz_bin==0))

survivalrate_2016_NS_2002_nd <- numerator_2016_NS_2002_nd/denominator_2016_NS_2002_nd
survivalrate_2016_NS_2002_nd


table(data_2002_nd$top_smallbiz_bin, data_2002_nd$survive_2016)


t.test(survive_2016 ~ top_smallbiz_bin, data = data_2002_nd)

#####Non-DoD 2003#####
#*******************#
#subset the 2003 data
data_2003_nd <- FPDS_cleaned_unique_nd[!(FPDS_cleaned_unique_nd$registrationYear!="2003"), ]

##create variable describing whether a firm survived 3 years

data_2003_nd <- data_2003_nd %>%
  dplyr::mutate(survive_3yr = ifelse(exitYear < 2005, "0", "1")) 

##create variable describing whether a firm survived 5 years
data_2003_nd <- data_2003_nd %>%
  dplyr::mutate(survive_5yr = ifelse(exitYear < 2007, "0", "1")) 


##create variable describing whether a firm survived 10 years
data_2003_nd <- data_2003_nd %>%
  dplyr::mutate(survive_10yr = ifelse(exitYear < 2012, "0", "1"))

##create variable describing whether a firm survived in 2016
data_2003_nd <- data_2003_nd %>%
  dplyr::mutate(survive_2016 = ifelse(exitYear < 2016, "0", "1"))


str(data_2003_nd)

data_2003_nd$survive_3yr<-as.numeric(as.character(data_2003_nd$survive_3yr))
data_2003_nd$survive_5yr<-as.numeric(as.character(data_2003_nd$survive_5yr))
data_2003_nd$survive_10yr<-as.numeric(as.character(data_2003_nd$survive_10yr))
data_2003_nd$survive_2016<-as.numeric(as.character(data_2003_nd$survive_2016))

str(data_2003_nd)

###
##t test to test the differences between small and non small survival##
table(data_2003_nd$top_smallbiz_bin)
table(data_2003_nd$survive_3yr)
table(data_2003_nd$top_smallbiz_bin, data_2003_nd$survive_3yr)

t.test(survive_3yr ~ top_smallbiz_bin, data = data_2003_nd)

#5-year#
table(data_2003_nd$top_smallbiz_bin, data_2003_nd$survive_5yr)

t.test(survive_5yr ~ top_smallbiz_bin, data = data_2003_nd)

#10-year#
table(data_2003_nd$top_smallbiz_bin, data_2003_nd$survive_10yr)

t.test(survive_10yr ~ top_smallbiz_bin, data = data_2003_nd)

#***********#
#survival rates#
#**************#

##3-year##
table(data_2003_nd$survive_3yr) #0=12449, 1=21743

numerator_3yrALL_2003_nd <- length(which(data_2003_nd$survive_3yr==1))

denominator_3yrALL_2003_nd <- length(data_2003_nd$survive_3yr)

survival_3yrALL_2003_nd <- numerator_3yrALL_2003_nd/denominator_3yrALL_2003_nd
survival_3yrALL_2003_nd

##5-year##
table(data_2003_nd$survive_5yr) #0=10168, 1=17266

numerator_5yrALL_2003_nd <- length(which(data_2003_nd$survive_5yr==1))

denominator_5yrALL_2003_nd <- length(data_2003_nd$survive_5yr)

survival_5yrALL_2003_nd <- numerator_5yrALL_2003_nd/denominator_5yrALL_2003_nd
survival_5yrALL_2003_nd

##10-year##
table(data_2003_nd$survive_10yr) #0=10168, 1=17266

numerator_10yrALL_2003_nd <- length(which(data_2003_nd$survive_10yr==1))

denominator_10yrALL_2003_nd <- length(data_2003_nd$survive_10yr)

survival_10yrALL_2003_nd <- numerator_10yrALL_2003_nd/denominator_10yrALL_2003_nd
survival_10yrALL_2003_nd

#****************#
#Graduation Rates#
#****************#
table(data_2003_nd$graduated)

numerator_grad_2003_nd <- length(which(data_2003_nd$graduated==1))

denominator_grad_2003_nd <- length(data_2003_nd$graduated)

graduated_2003_nd <- numerator_grad_2003_nd/denominator_grad_2003_nd
graduated_2003_nd

##graduation for those who survived 10 years
table(data_2003_nd$graduated)
table(data_2003_nd$survive_10yr)
table(data_2003_nd$top_smallbiz_bin)


numerator_grad_2003_nd_10yr <- length(which(data_2003_nd$graduated==1 & data_2003_nd$survive_10yr==1))

#denominator_grad_2003_nd_10yr <- length(data_2003_nd$graduated)

denominator_grad_2003_nd_10yr <- length(which(data_2003_nd$top_smallbiz_bin==1))

graduated_2003_nd_10yr <- numerator_grad_2003_nd_10yr/denominator_grad_2003_nd_10yr
graduated_2003_nd_10yr


#*******#
#***SMALL***#
#********#
##3-year##
table(data_2003_nd$survive_3yr) #0=12449, 1=21743

numerator_3yrSM_2003_nd <- length(which(data_2003_nd$survive_3yr==1 & data_2003_nd$top_smallbiz_bin==1))

denominator_3yrSM_2003_nd <- length(which(data_2003_nd$top_smallbiz_bin==1))

survival_3yrSM_2003_nd <- numerator_3yrSM_2003_nd/denominator_3yrSM_2003_nd
survival_3yrSM_2003_nd

##5-year##
table(data_2003_nd$survive_5yr) #0=10168, 1=17266

numerator_5yrSM_2003_nd <- length(which(data_2003_nd$survive_5yr==1 & data_2003_nd$top_smallbiz_bin==1))

denominator_5yrSM_2003_nd <- length(which(data_2003_nd$top_smallbiz_bin==1))

survival_5yrSM_2003_nd <- numerator_5yrSM_2003_nd/denominator_5yrSM_2003_nd
survival_5yrSM_2003_nd

##10-year##
table(data_2003_nd$survive_10yr) #0=10168, 1=17266

numerator_10yrSM_2003_nd <- length(which(data_2003_nd$survive_10yr==1 & data_2003_nd$top_smallbiz_bin==1))

denominator_10yrSM_2003_nd <- length(which(data_2003_nd$top_smallbiz_bin==1))

survival_10yrSM_2003_nd <- numerator_10yrSM_2003_nd/denominator_10yrSM_2003_nd
survival_10yrSM_2003_nd

#*****#
#NONSMALL*
#******#
##3-year##
table(data_2003_nd$survive_3yr) #0=12449, 1=21743

numerator_3yrNSM_2003_nd <- length(which(data_2003_nd$survive_3yr==1 & data_2003_nd$top_smallbiz_bin==0))

denominator_3yrNSM_2003_nd <- length(which(data_2003_nd$top_smallbiz_bin==0))

survival_3yrNSM_2003_nd <- numerator_3yrNSM_2003_nd/denominator_3yrNSM_2003_nd
survival_3yrNSM_2003_nd

##5-year##
table(data_2003_nd$survive_5yr) #0=10168, 1=17266

numerator_5yrNSM_2003_nd <- length(which(data_2003_nd$survive_5yr==1 & data_2003_nd$top_smallbiz_bin==0))

denominator_5yrNSM_2003_nd <- length(which(data_2003_nd$top_smallbiz_bin==0))

survival_5yrNSM_2003_nd <- numerator_5yrNSM_2003_nd/denominator_5yrNSM_2003_nd
survival_5yrNSM_2003_nd

##10-year##
table(data_2003_nd$survive_10yr) #0=10168, 1=17266

numerator_10yrNSM_2003_nd <- length(which(data_2003_nd$survive_10yr==1 & data_2003_nd$top_smallbiz_bin==0))

denominator_10yrNSM_2003_nd <- length(which(data_2003_nd$top_smallbiz_bin==0))

survival_10yrNSM_2003_nd <- numerator_10yrNSM_2003_nd/denominator_10yrNSM_2003_nd
survival_10yrNSM_2003_nd

#*******************#
#******2016 check***#
#*******************#
##*********
#***2016 survival 
#**********
survive_10yr_count_2003_nd <- length(which(data_2003_nd$survive_10yr==1))
##ALL##
numerator_2016_ALL_2003_nd <- length(which(data_2003_nd$survive_2016==1))

denominator_2016_ALL_2003_nd <- length(data_2003_nd$survive_2016)

survivalrate_2016_ALL_2003_nd <- numerator_2016_ALL_2003_nd /denominator_2016_ALL_2003_nd 
survivalrate_2016_ALL_2003_nd 


##SMALL##
numerator_2016_SM_2003_nd <- length(which(data_2003_nd$survive_2016==1 & data_2003_nd$top_smallbiz_bin==1))

denominator_2016_SM_2003_nd <- length(which(data_2003_nd$top_smallbiz_bin==1))

survivalrate_2016_SM_2003_nd <- numerator_2016_SM_2003_nd /denominator_2016_SM_2003_nd 
survivalrate_2016_SM_2003_nd 


##NONSMALL##
numerator_2016_NS_2003_nd <- length(which(data_2003_nd$survive_2016==1 & data_2003_nd$top_smallbiz_bin==0))

denominator_2016_NS_2003_nd <- length(which(data_2003_nd$top_smallbiz_bin==0))

survivalrate_2016_NS_2003_nd  <- numerator_2016_NS_2003_nd /denominator_2016_NS_2003_nd 
survivalrate_2016_NS_2003_nd 


table(data_2003_nd$top_smallbiz_bin, data_2003_nd$survive_2016)


t.test(survive_2016 ~ top_smallbiz_bin, data = data_2003_nd)



#####Non-DoD 2004#####
#*******************#
#subset the 2004 data
data_2004_nd <- FPDS_cleaned_unique_nd[!(FPDS_cleaned_unique_nd$registrationYear!="2004"), ]

##create variable describing whether a firm survived 3 years

data_2004_nd <- data_2004_nd %>%
  dplyr::mutate(survive_3yr = ifelse(exitYear < 2006, "0", "1")) 

##create variable describing whether a firm survived 5 years
data_2004_nd <- data_2004_nd %>%
  dplyr::mutate(survive_5yr = ifelse(exitYear < 2009, "0", "1")) 


##create variable describing whether a firm survived 10 years
data_2004_nd <- data_2004_nd %>%
  dplyr::mutate(survive_10yr = ifelse(exitYear < 2013, "0", "1")) 

##create variable describing whether a firm survived in 2016
data_2004_nd <- data_2004_nd %>%
  dplyr::mutate(survive_2016 = ifelse(exitYear < 2016, "0", "1"))


data_2004_nd$survive_3yr<-as.numeric(as.character(data_2004_nd$survive_3yr))
data_2004_nd$survive_5yr<-as.numeric(as.character(data_2004_nd$survive_5yr))
data_2004_nd$survive_10yr<-as.numeric(as.character(data_2004_nd$survive_10yr))
data_2004_nd$survive_2016<-as.numeric(as.character(data_2004_nd$survive_2016))

str(data_2004_nd)

###
##t test to test the differences between small and non small survival##
table(data_2004_nd$top_smallbiz_bin)
table(data_2004_nd$survive_3yr)
table(data_2004_nd$top_smallbiz_bin, data_2004_nd$survive_3yr)

t.test(survive_3yr ~ top_smallbiz_bin, data = data_2004_nd)

#5-year#
table(data_2004_nd$top_smallbiz_bin, data_2004_nd$survive_5yr)

t.test(survive_5yr ~ top_smallbiz_bin, data = data_2004_nd)

#10-year#
table(data_2004_nd$top_smallbiz_bin, data_2004_nd$survive_10yr)

t.test(survive_10yr ~ top_smallbiz_bin, data = data_2004_nd)


#********#
#**ALL**#
#********#

#***********#
#survival rates#
#**************#

##3-year##
table(data_2004_nd$survive_3yr) #0=12449, 1=21743

numerator_3yrALL_2004_nd <- length(which(data_2004_nd$survive_3yr==1))

denominator_3yrALL_2004_nd <- length(data_2004_nd$survive_3yr)

survival_3yrALL_2004_nd <- numerator_3yrALL_2004_nd/denominator_3yrALL_2004_nd
survival_3yrALL_2004_nd

##5-year##
table(data_2004_nd$survive_5yr) #0=10168, 1=17266

numerator_5yrALL_2004_nd <- length(which(data_2004_nd$survive_5yr==1))

denominator_5yrALL_2004_nd <- length(data_2004_nd$survive_5yr)

survival_5yrALL_2004_nd <- numerator_5yrALL_2004_nd/denominator_5yrALL_2004_nd
survival_5yrALL_2004_nd

##10-year##
table(data_2004_nd$survive_10yr) #0=10168, 1=17266

numerator_10yrALL_2004_nd <- length(which(data_2004_nd$survive_10yr==1))

denominator_10yrALL_2004_nd <- length(data_2004_nd$survive_10yr)

survival_10yrALL_2004_nd <- numerator_10yrALL_2004_nd/denominator_10yrALL_2004_nd
survival_10yrALL_2004_nd

#****************#
#Graduation Rates#
#****************#
table(data_2004_nd$graduated)

numerator_grad_2004_nd <- length(which(data_2004_nd$graduated==1))

denominator_grad_2004_nd <- length(data_2004_nd$graduated)

graduated_2004_nd <- numerator_grad_2004_nd/denominator_grad_2004_nd
graduated_2004_nd 

##those who survived 10 years only
numerator_grad_2004_nd_10yr <- length(which(data_2004_nd$graduated==1 & data_2004_nd$survive_10yr==1))

#denominator_grad_2004_nd_10yr <- length(data_2004_nd$graduated)

denominator_grad_2004_nd_10yr <- length(which(data_2004_nd$top_smallbiz_bin==1))

graduated_2004_nd_10yr <- numerator_grad_2004_nd_10yr/denominator_grad_2004_nd_10yr
graduated_2004_nd_10yr 


#******#
#*SMALL*#
#******#
##3-year##
table(data_2004_nd$survive_3yr) #0=12449, 1=21743

numerator_3yrSM_2004_nd <- length(which(data_2004_nd$survive_3yr==1 & data_2004_nd$top_smallbiz_bin==1))

denominator_3yrSM_2004_nd <- length(which(data_2004_nd$top_smallbiz_bin==1))

survival_3yrSM_2004_nd <- numerator_3yrSM_2004_nd/denominator_3yrSM_2004_nd
survival_3yrSM_2004_nd

##5-year##
table(data_2004_nd$survive_5yr) #0=10168, 1=17266

numerator_5yrSM_2004_nd <- length(which(data_2004_nd$survive_5yr==1 & data_2004_nd$top_smallbiz_bin==1))

denominator_5yrSM_2004_nd <- length(which(data_2004_nd$top_smallbiz_bin==1))

survival_5yrSM_2004_nd <- numerator_5yrSM_2004_nd/denominator_5yrSM_2004_nd
survival_5yrSM_2004_nd

##10-year##
table(data_2004_nd$survive_10yr) #0=10168, 1=17266

numerator_10yrSM_2004_nd <- length(which(data_2004_nd$survive_10yr==1 & data_2004_nd$top_smallbiz_bin==1))

denominator_10yrSM_2004_nd <- length(which(data_2004_nd$top_smallbiz_bin==1))

survival_10yrSM_2004_nd <- numerator_10yrSM_2004_nd/denominator_10yrSM_2004_nd
survival_10yrSM_2004_nd



#********#
#NON SMALL#
#********#
##3-year##
table(data_2004_nd$survive_3yr) #0=12449, 1=21743

numerator_3yrNSM_2004_nd <- length(which(data_2004_nd$survive_3yr==1 & data_2004_nd$top_smallbiz_bin==0))

denominator_3yrNSM_2004_nd <- length(which(data_2004_nd$top_smallbiz_bin==0))

survival_3yrNSM_2004_nd <- numerator_3yrNSM_2004_nd/denominator_3yrNSM_2004_nd
survival_3yrNSM_2004_nd 

##5-year##
table(data_2004_nd$survive_5yr) #0=10168, 1=17266

numerator_5yrNSM_2004_nd <- length(which(data_2004_nd$survive_5yr==1 & data_2004_nd$top_smallbiz_bin==0))

denominator_5yrNSM_2004_nd <- length(which(data_2004_nd$top_smallbiz_bin==0))

survival_5yrNSM_2004_nd <- numerator_5yrNSM_2004_nd/denominator_5yrNSM_2004_nd
survival_5yrNSM_2004_nd

##10-year##
table(data_2004_nd$survive_10yr) #0=10168, 1=17266

numerator_10yrNSM_2004_nd <- length(which(data_2004_nd$survive_10yr==1 & data_2004_nd$top_smallbiz_bin==0))

denominator_10yrNSM_2004_nd <- length(which(data_2004_nd$top_smallbiz_bin==0))

survival_10yrNSM_2004_nd <- numerator_10yrNSM_2004_nd/denominator_10yrNSM_2004_nd
survival_10yrNSM_2004_nd 

#*******************#
#******2016 check***#
#*******************#
##*********
#***2016 survival 
#**********
survive_10yr_count_2004_nd <- length(which(data_2004_nd$survive_10yr==1))
##ALL##
numerator_2016_ALL_2004_nd <- length(which(data_2004_nd$survive_2016==1))

denominator_2016_ALL_2004_nd <- length(data_2004_nd$survive_2016)

survivalrate_2016_ALL_2004_nd <- numerator_2016_ALL_2004_nd /denominator_2016_ALL_2004_nd
survivalrate_2016_ALL_2004_nd 


##SMALL##
numerator_2016_SM_2004_nd <- length(which(data_2004_nd$survive_2016==1 & data_2004_nd$top_smallbiz_bin==1))

denominator_2016_SM_2004_nd <- length(which(data_2004_nd$top_smallbiz_bin==1))

survivalrate_2016_SM_2004_nd <- numerator_2016_SM_2004_nd /denominator_2016_SM_2004_nd 
survivalrate_2016_SM_2004_nd 


##NONSMALL##
numerator_2016_NS_2004_nd <- length(which(data_2004_nd$survive_2016==1 & data_2004_nd$top_smallbiz_bin==0))

denominator_2016_NS_2004_nd <- length(which(data_2004_nd$top_smallbiz_bin==0))

survivalrate_2016_NS_2004_nd  <- numerator_2016_NS_2004_nd /denominator_2016_NS_2004_nd 
survivalrate_2016_NS_2004_nd 


table(data_2004_nd$top_smallbiz_bin, data_2004_nd$survive_2016)


t.test(survive_2016 ~ top_smallbiz_bin, data = data_2004_nd)


#####Non-DoD 2005#####
#*******************#
#subset the 2005 data
data_2005_nd <- FPDS_cleaned_unique_nd[!(FPDS_cleaned_unique_nd$registrationYear!="2005"), ]

##create variable describing whether a firm survived 3 years

data_2005_nd <- data_2005_nd %>%
  dplyr::mutate(survive_3yr = ifelse(exitYear < 2007, "0", "1")) 

##create variable describing whether a firm survived 5 years
data_2005_nd <- data_2005_nd %>%
  dplyr::mutate(survive_5yr = ifelse(exitYear < 2010, "0", "1")) 


##create variable describing whether a firm survived 10 years
data_2005_nd <- data_2005_nd %>%
  dplyr::mutate(survive_10yr = ifelse(exitYear < 2014, "0", "1")) 

##create variable describing whether a firm survived in 2016
data_2005_nd <- data_2005_nd %>%
  dplyr::mutate(survive_2016 = ifelse(exitYear < 2016, "0", "1"))


str(data_2005_nd)

data_2005_nd$survive_3yr<-as.numeric(as.character(data_2005_nd$survive_3yr))
data_2005_nd$survive_5yr<-as.numeric(as.character(data_2005_nd$survive_5yr))
data_2005_nd$survive_10yr<-as.numeric(as.character(data_2005_nd$survive_10yr))
data_2005_nd$survive_2016<-as.numeric(as.character(data_2005_nd$survive_2016))

str(data_2005_nd)

##**********
##t test to test the differences between small and non small survival##
table(data_2005_nd$top_smallbiz_bin)
table(data_2005_nd$survive_3yr)
table(data_2005_nd$top_smallbiz_bin, data_2005_nd$survive_3yr)

t.test(survive_3yr ~ top_smallbiz_bin, data = data_2005_nd)

#5-year#
table(data_2005_nd$top_smallbiz_bin, data_2005_nd$survive_5yr)

t.test(survive_5yr ~ top_smallbiz_bin, data = data_2005_nd)

#10-year#
table(data_2005_nd$top_smallbiz_bin, data_2005_nd$survive_10yr)

t.test(survive_10yr ~ top_smallbiz_bin, data = data_2005_nd)

#******#
#**ALL*#
#*****#
#*************#
#survival rates#
#***************#
##3-year##
table(data_2005_nd$survive_3yr) #0=12449, 1=21743

numerator_3yrALL_2005_nd <- length(which(data_2005_nd$survive_3yr==1))

denominator_3yrALL_2005_nd <- length(data_2005_nd$survive_3yr)

survival_3yrALL_2005_nd <- numerator_3yrALL_2005_nd/denominator_3yrALL_2005_nd
survival_3yrALL_2005_nd

##5-year##
table(data_2005_nd$survive_5yr) #0=10168, 1=17266

numerator_5yrALL_2005_nd <- length(which(data_2005_nd$survive_5yr==1))

denominator_5yrALL_2005_nd <- length(data_2005_nd$survive_5yr)

survival_5yrALL_2005_nd <- numerator_5yrALL_2005_nd/denominator_5yrALL_2005_nd
survival_5yrALL_2005_nd

##10-year##
table(data_2005_nd$survive_10yr) #0=10168, 1=17266

numerator_10yrALL_2005_nd <- length(which(data_2005_nd$survive_10yr==1))

denominator_10yrALL_2005_nd <- length(data_2005_nd$survive_10yr)

survival_10yrALL_2005_nd <- numerator_10yrALL_2005_nd/denominator_10yrALL_2005_nd
survival_10yrALL_2005_nd 
#****************#
#Graduation Rates#
#****************#
table(data_2005_nd$graduated)

numerator_grad_2005_nd <- length(which(data_2005_nd$graduated==1))

denominator_grad_2005_nd <- length(data_2005_nd$graduated)

graduated_2005_nd <- numerator_grad_2005_nd/denominator_grad_2005_nd
graduated_2005_nd

##survived 10 years only
numerator_grad_2005_nd_10yr <- length(which(data_2005_nd$graduated==1 & data_2005_nd$survive_10yr==1))

#denominator_grad_2005_nd_10yr <- length(data_2005_nd$graduated)

denominator_grad_2005_nd_10yr <- length(which(data_2005_nd$top_smallbiz_bin==1))

graduated_2005_nd_10yr <- numerator_grad_2005_nd_10yr/denominator_grad_2005_nd_10yr
graduated_2005_nd_10yr


#******#
#SMALL#
#******#
##3-year##
table(data_2005_nd$survive_3yr) #0=12449, 1=21743

numerator_3yrSM_2005_nd <- length(which(data_2005_nd$survive_3yr==1 & data_2005_nd$top_smallbiz_bin==1))

denominator_3yrSM_2005_nd <- length(which(data_2005_nd$top_smallbiz_bin==1))

survival_3yrSM_2005_nd <- numerator_3yrSM_2005_nd/denominator_3yrSM_2005_nd
survival_3yrSM_2005_nd

##5-year##
table(data_2005_nd$survive_5yr) #0=10168, 1=17266

numerator_5yrSM_2005_nd <- length(which(data_2005_nd$survive_5yr==1 & data_2005_nd$top_smallbiz_bin==1))

denominator_5yrSM_2005_nd <- length(which(data_2005_nd$top_smallbiz_bin==1))

survival_5yrSM_2005_nd <- numerator_5yrSM_2005_nd/denominator_5yrSM_2005_nd
survival_5yrSM_2005_nd 

##10-year##
table(data_2005_nd$survive_10yr) #0=10168, 1=17266

numerator_10yrSM_2005_nd <- length(which(data_2005_nd$survive_10yr==1 & data_2005_nd$top_smallbiz_bin==1))

denominator_10yrSM_2005_nd <- length(which(data_2005_nd$top_smallbiz_bin==1))

survival_10yrSM_2005_nd <- numerator_10yrSM_2005_nd/denominator_10yrSM_2005_nd
survival_10yrSM_2005_nd

#*******#
#NON SMAL#
#*******#
##3-year##
table(data_2005_nd$survive_3yr) #0=12449, 1=21743

numerator_3yrNSM_2005_nd <- length(which(data_2005_nd$survive_3yr==1 & data_2005_nd$top_smallbiz_bin==0))

denominator_3yrNSM_2005_nd <- length(which(data_2005_nd$top_smallbiz_bin==0))

survival_3yrNSM_2005_nd <- numerator_3yrNSM_2005_nd/denominator_3yrNSM_2005_nd
survival_3yrNSM_2005_nd

##5-year##
table(data_2005_nd$survive_5yr) #0=10168, 1=17266

numerator_5yrNSM_2005_nd <- length(which(data_2005_nd$survive_5yr==1 & data_2005_nd$top_smallbiz_bin==0))

denominator_5yrNSM_2005_nd <- length(which(data_2005_nd$top_smallbiz_bin==0))

survival_5yrNSM_2005_nd <- numerator_5yrNSM_2005_nd/denominator_5yrNSM_2005_nd
survival_5yrNSM_2005_nd

##10-year##
table(data_2005_nd$survive_10yr) #0=10168, 1=17266

numerator_10yrNSM_2005_nd <- length(which(data_2005_nd$survive_10yr==1 & data_2005_nd$top_smallbiz_bin==0))

denominator_10yrNSM_2005_nd <- length(which(data_2005_nd$top_smallbiz_bin==0))

survival_10yrNSM_2005_nd <- numerator_10yrNSM_2005_nd/denominator_10yrNSM_2005_nd
survival_10yrNSM_2005_nd


#*******************#
#******2016 check***#
#*******************#
##*********
#***2016 survival 
#**********
survive_10yr_count_2005_nd <- length(which(data_2005_nd$survive_10yr==1))
##ALL##
numerator_2016_ALL_2005_nd <- length(which(data_2005_nd$survive_2016==1))

denominator_2016_ALL_2005_nd <- length(data_2005_nd$survive_2016)

survivalrate_2016_ALL_2005_nd <- numerator_2016_ALL_2005_nd /denominator_2016_ALL_2005_nd
survivalrate_2016_ALL_2005_nd 


##SMALL##
numerator_2016_SM_2005_nd <- length(which(data_2005_nd$survive_2016==1 & data_2005_nd$top_smallbiz_bin==1))

denominator_2016_SM_2005_nd <- length(which(data_2005_nd$top_smallbiz_bin==1))

survivalrate_2016_SM_2005_nd <- numerator_2016_SM_2005_nd /denominator_2016_SM_2005_nd 
survivalrate_2016_SM_2005_nd


##NONSMALL##
numerator_2016_NS_2005_nd <- length(which(data_2005_nd$survive_2016==1 & data_2005_nd$top_smallbiz_bin==0))

denominator_2016_NS_2005_nd <- length(which(data_2005_nd$top_smallbiz_bin==0))

survivalrate_2016_NS_2005_nd  <- numerator_2016_NS_2005_nd /denominator_2016_NS_2005_nd
survivalrate_2016_NS_2005_nd 


table(data_2005_nd$top_smallbiz_bin, data_2005_nd$survive_2016)


t.test(survive_2016 ~ top_smallbiz_bin, data = data_2005_nd)


#####Non-DoD 2006#####
#*******************#
#subset the 2006 data
data_2006_nd <- FPDS_cleaned_unique_nd[!(FPDS_cleaned_unique_nd$registrationYear!="2006"), ]


##create variable describing whether a firm survived 3 years

data_2006_nd <- data_2006_nd %>%
  dplyr::mutate(survive_3yr = ifelse(exitYear < 2008, "0", "1")) 

##create variable describing whether a firm survived 5 years
data_2006_nd <- data_2006_nd %>%
  dplyr::mutate(survive_5yr = ifelse(exitYear < 2011, "0", "1")) 


##create variable describing whether a firm survived 10 years
data_2006_nd <- data_2006_nd %>%
  dplyr::mutate(survive_10yr = ifelse(exitYear < 2015, "0", "1")) 

##create variable describing whether a firm survived in 2016
data_2006_nd <- data_2006_nd %>%
  dplyr::mutate(survive_2016 = ifelse(exitYear < 2016, "0", "1"))


str(data_2006_nd)

data_2006_nd$survive_3yr<-as.numeric(as.character(data_2006_nd$survive_3yr))
data_2006_nd$survive_5yr<-as.numeric(as.character(data_2006_nd$survive_5yr))
data_2006_nd$survive_10yr<-as.numeric(as.character(data_2006_nd$survive_10yr))
data_2006_nd$survive_2016<-as.numeric(as.character(data_2006_nd$survive_2016))

str(data_2006_nd)

#**********
##t test to test the differences between small and non small survival##
table(data_2006_nd$top_smallbiz_bin)
table(data_2006_nd$survive_3yr)
table(data_2006_nd$top_smallbiz_bin, data_2006_nd$survive_3yr)

t.test(survive_3yr ~ top_smallbiz_bin, data = data_2006_nd)

#5-year#
table(data_2006_nd$top_smallbiz_bin, data_2006_nd$survive_5yr)

t.test(survive_5yr ~ top_smallbiz_bin, data = data_2006_nd)

#10-year#
table(data_2006_nd$top_smallbiz_bin, data_2006_nd$survive_10yr)

t.test(survive_10yr ~ top_smallbiz_bin, data = data_2006_nd)


#*************#
#survival rates#
#**************#
#*********#
#**ALL****#
#*********
##3-year##
table(data_2006_nd$survive_3yr) #0=12449, 1=21743

numerator_3yrALL_2006_nd <- length(which(data_2006_nd$survive_3yr==1))

denominator_3yrALL_2006_nd <- length(data_2006_nd$survive_3yr)

survival_3yrALL_2006_nd <- numerator_3yrALL_2006_nd/denominator_3yrALL_2006_nd
survival_3yrALL_2006_nd

##5-year##
table(data_2006_nd$survive_5yr) #0=10168, 1=17266

numerator_5yrALL_2006_nd <- length(which(data_2006_nd$survive_5yr==1))

denominator_5yrALL_2006_nd <- length(data_2006_nd$survive_5yr)

survival_5yrALL_2006_nd <- numerator_5yrALL_2006_nd/denominator_5yrALL_2006_nd
survival_5yrALL_2006_nd

##10-year##
table(data_2006_nd$survive_10yr) #0=10168, 1=17266

numerator_10yrALL_2006_nd <- length(which(data_2006_nd$survive_10yr==1))

denominator_10yrALL_2006_nd <- length(data_2006_nd$survive_10yr)

survival_10yrALL_2006_nd <- numerator_10yrALL_2006_nd/denominator_10yrALL_2006_nd
survival_10yrALL_2006_nd
#****************#
#Graduation Rates#
#****************#
table(data_2006_nd$graduated)

numerator_grad_2006_nd <- length(which(data_2006_nd$graduated==1))

denominator_grad_2006_nd <- length(data_2006_nd$graduated)

graduated_2006_nd <- numerator_grad_2006_nd/denominator_grad_2006_nd
graduated_2006_nd

##graduation for those who survived after 10 years only
numerator_grad_2006_nd_10yr <- length(which(data_2006_nd$graduated==1 & data_2006_nd$survive_10yr==1))

#denominator_grad_2006_nd_10yr <- length(data_2006_nd$graduated)

denominator_grad_2006_nd_10yr <- length(which(data_2006_nd$top_smallbiz_bin==1))

graduated_2006_nd_10yr <- numerator_grad_2006_nd_10yr/denominator_grad_2006_nd_10yr
graduated_2006_nd_10yr


#*****#
#***SMALL***
#*******#
##3-year##
table(data_2006_nd$survive_3yr) #0=12449, 1=21743

numerator_3yrSM_2006_nd <- length(which(data_2006_nd$survive_3yr==1 & data_2006_nd$top_smallbiz_bin==1))

denominator_3yrSM_2006_nd <- length(which(data_2006_nd$top_smallbiz_bin==1))

survival_3yrSM_2006_nd <- numerator_3yrSM_2006_nd/denominator_3yrSM_2006_nd
survival_3yrSM_2006_nd 

##5-year##
table(data_2006_nd$survive_5yr) #0=10168, 1=17266

numerator_5yrSM_2006_nd <- length(which(data_2006_nd$survive_5yr==1 & data_2006_nd$top_smallbiz_bin==1))

denominator_5yrSM_2006_nd <- length(which(data_2006_nd$top_smallbiz_bin==1))

survival_5yrSM_2006_nd <- numerator_5yrSM_2006_nd/denominator_5yrSM_2006_nd
survival_5yrSM_2006_nd 

##10-year##
table(data_2006_nd$survive_10yr) #0=10168, 1=17266

numerator_10yrSM_2006_nd <- length(which(data_2006_nd$survive_10yr==1 & data_2006_nd$top_smallbiz_bin==1))

denominator_10yrSM_2006_nd <- length(which(data_2006_nd$top_smallbiz_bin==1))

survival_10yrSM_2006_nd <- numerator_10yrSM_2006_nd/denominator_10yrSM_2006_nd
survival_10yrSM_2006_nd


#********#
#*NON SMALL*#
#*********#
##3-year##
table(data_2006_nd$survive_3yr) #0=12449, 1=21743

numerator_3yrNSM_2006_nd <- length(which(data_2006_nd$survive_3yr==1 & data_2006_nd$top_smallbiz_bin==0))

denominator_3yrNSM_2006_nd <- length(which(data_2006_nd$top_smallbiz_bin==0))

survival_3yrNSM_2006_nd <- numerator_3yrNSM_2006_nd/denominator_3yrNSM_2006_nd
survival_3yrNSM_2006_nd

##5-year##
table(data_2006_nd$survive_5yr) #0=10168, 1=17266

numerator_5yrNSM_2006_nd <- length(which(data_2006_nd$survive_5yr==1 & data_2006_nd$top_smallbiz_bin==0))

denominator_5yrNSM_2006_nd <- length(which(data_2006_nd$top_smallbiz_bin==0))

survival_5yrNSM_2006_nd <- numerator_5yrNSM_2006_nd/denominator_5yrNSM_2006_nd
survival_5yrNSM_2006_nd 

##10-year##
table(data_2006_nd$survive_10yr) #0=10168, 1=17266

numerator_10yrNSM_2006_nd <- length(which(data_2006_nd$survive_10yr==1 & data_2006_nd$top_smallbiz_bin==0))

denominator_10yrNSM_2006_nd <- length(which(data_2006_nd$top_smallbiz_bin==0))

survival_10yrNSM_2006_nd <- numerator_10yrNSM_2006_nd/denominator_10yrNSM_2006_nd
survival_10yrNSM_2006_nd 

#*******************#
#******2016 check***#
#*******************#
##*********
#***2016 survival 
#**********
survive_10yr_count_2006_nd <- length(which(data_2006_nd$survive_10yr==1))
##ALL##
numerator_2016_ALL_2006_nd <- length(which(data_2006_nd$survive_2016==1))

denominator_2016_ALL_2006_nd <- length(data_2006_nd$survive_2016)

survivalrate_2016_ALL_2006_nd <- numerator_2016_ALL_2006_nd /denominator_2016_ALL_2006_nd
survivalrate_2016_ALL_2006_nd 


##SMALL##
numerator_2016_SM_2006_nd <- length(which(data_2006_nd$survive_2016==1 & data_2006_nd$top_smallbiz_bin==1))

denominator_2016_SM_2006_nd <- length(which(data_2006_nd$top_smallbiz_bin==1))

survivalrate_2016_SM_2006_nd <- numerator_2016_SM_2006_nd /denominator_2016_SM_2006_nd
survivalrate_2016_SM_2006_nd


##NONSMALL##
numerator_2016_NS_2006_nd <- length(which(data_2006_nd$survive_2016==1 & data_2006_nd$top_smallbiz_bin==0))

denominator_2016_NS_2006_nd <- length(which(data_2006_nd$top_smallbiz_bin==0))

survivalrate_2016_NS_2006_nd  <- numerator_2016_NS_2006_nd /denominator_2016_NS_2006_nd
survivalrate_2016_NS_2006_nd


table(data_2006_nd$top_smallbiz_bin, data_2006_nd$survive_2016)


t.test(survive_2016 ~ top_smallbiz_bin, data = data_2006_nd)



#******************************************************************************************************
#*******************#
#####DOD STATS#####
#*******************#

##All DoD STATS Should be the same
##Just listing here for convenience


#####DoD 2001#####
#*******************#
#subset the 2001 data
data_DOD_2001 <- FPDS_cleaned_unique[!(FPDS_cleaned_unique$registrationYear!="2001"), ]
data_DOD_2001 <- data_DOD_2001[!(data_DOD_2001$customer!="Defense"), ]

data_DOD_2001 <- data_DOD_2001 %>%
  dplyr::mutate(survive_3yr = ifelse(exitYear < 2003, "0", "1")) 

##create variable describing whether a firm survived 5 years
data_DOD_2001 <- data_DOD_2001 %>%
  dplyr::mutate(survive_5yr = ifelse(exitYear < 2005, "0", "1")) 


##create variable describing whether a firm survived 10 years
data_DOD_2001 <- data_DOD_2001 %>%
  dplyr::mutate(survive_10yr = ifelse(exitYear < 2010, "0", "1")) 

##create variable describing whether a firm survived in 2016
data_DOD_2001 <- data_DOD_2001 %>%
  dplyr::mutate(survive_2016 = ifelse(exitYear < 2016, "0", "1"))


str(data_DOD_2001)

data_DOD_2001$survive_3yr<-as.numeric(as.character(data_DOD_2001$survive_3yr))
data_DOD_2001$survive_5yr<-as.numeric(as.character(data_DOD_2001$survive_5yr))
data_DOD_2001$survive_10yr<-as.numeric(as.character(data_DOD_2001$survive_10yr))
data_DOD_2001$survive_2016<-as.numeric(as.character(data_DOD_2001$survive_2016))

str(data_DOD_2001)

##t-test between small and nonsmall survival rates##
#3-year#

table(data_DOD_2001$top_smallbiz_bin)
table(data_DOD_2001$survive_3yr)
table(data_DOD_2001$top_smallbiz_bin, data_DOD_2001$survive_3yr)

t.test(survive_3yr ~ top_smallbiz_bin, data = data_DOD_2001)

#5-year#
table(data_DOD_2001$top_smallbiz_bin, data_DOD_2001$survive_5yr)

t.test(survive_5yr ~ top_smallbiz_bin, data = data_DOD_2001)

#10-year#
table(data_DOD_2001$top_smallbiz_bin, data_DOD_2001$survive_10yr)

t.test(survive_10yr ~ top_smallbiz_bin, data = data_DOD_2001)


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

#denominator_gradALL_2001_DoD_10yr <- length(data_DOD_2001$graduated)

denominator_gradALL_2001_DoD_10yr <- length(which(data_DOD_2001$top_smallbiz_bin==1))

graduatedALL_2001_DoD_10yr <- numerator_gradALL_2001_DoD_10yr/denominator_gradALL_2001_DoD_10yr
graduatedALL_2001_DoD_10yr


#**************************
#**********#
#***SMALL**#
#**********#
##3-year##
table(data_DOD_2001$survive_3yr) #0=10168, 1=17266

numerator_3yrSM_2001_DoD <- length(which(data_DOD_2001$survive_3yr==1 & data_DOD_2001$top_smallbiz_bin==1))

denominator_3yrSM_2001_DoD <- length(which(data_DOD_2001$top_smallbiz_bin==1))

survival_3yrSM_2001_DoD <- numerator_3yrSM_2001_DoD/denominator_3yrSM_2001_DoD
survival_3yrSM_2001_DoD

##5-year##
table(data_DOD_2001$survive_5yr) #0=10168, 1=17266

numerator_5yrSM_2001_DoD <- length(which(data_DOD_2001$survive_5yr==1 & data_DOD_2001$top_smallbiz_bin==1))

denominator_5yrSM_2001_DoD <- length(which(data_DOD_2001$top_smallbiz_bin==1))

survival_5yrSM_2001_DoD <- numerator_5yrSM_2001_DoD/denominator_5yrSM_2001_DoD
survival_5yrSM_2001_DoD

##10-year##
table(data_DOD_2001$survive_10yr) #0=10168, 1=17266

numerator_10yrSM_2001_DoD <- length(which(data_DOD_2001$survive_10yr==1 & data_DOD_2001$top_smallbiz_bin==1))

denominator_10yrSM_2001_DoD <- length(which(data_DOD_2001$top_smallbiz_bin==1))

survival_10yrSM_2001_DoD <- numerator_10yrSM_2001_DoD/denominator_10yrSM_2001_DoD
survival_10yrSM_2001_DoD


#**********#
#***NON-SMALL**#
#**********#

##3-year##
table(data_DOD_2001$survive_3yr) #0=10168, 1=17266

numerator_3yrNSM_2001_DoD <- length(which(data_DOD_2001$survive_3yr==1 & data_DOD_2001$top_smallbiz_bin==0))

denominator_3yrNSM_2001_DoD <- length(which(data_DOD_2001$top_smallbiz_bin==0))

survival_3yrNSM_2001_DoD <- numerator_3yrNSM_2001_DoD/denominator_3yrNSM_2001_DoD
survival_3yrNSM_2001_DoD

##5-year##
table(data_DOD_2001$survive_5yr) #0=10168, 1=17266

numerator_5yrNSM_2001_DoD <- length(which(data_DOD_2001$survive_5yr==1 & data_DOD_2001$top_smallbiz_bin==0))

denominator_5yrNSM_2001_DoD <- length(which(data_DOD_2001$top_smallbiz_bin==0))

survival_5yrNSM_2001_DoD <- numerator_5yrNSM_2001_DoD/denominator_5yrNSM_2001_DoD
survival_5yrNSM_2001_DoD

##10-year##
table(data_DOD_2001$survive_10yr) #0=10168, 1=17266

numerator_10yrNSM_2001_DoD <- length(which(data_DOD_2001$survive_10yr==1 & data_DOD_2001$top_smallbiz_bin==0))

denominator_10yrNSM_2001_DoD <- length(which(data_DOD_2001$top_smallbiz_bin==0))

survival_10yrNSM_2001_DoD <- numerator_10yrNSM_2001_DoD/denominator_10yrNSM_2001_DoD
survival_10yrNSM_2001_DoD

#*******************#
#******2016 check***#
#*******************#
##*********
#***2016 survival 
#**********
survive_10yr_count_2001_DOD <- length(which(data_DOD_2001$survive_10yr==1))
##ALL##
numerator_2016_ALL_2001_DOD <- length(which(data_DOD_2001$survive_2016==1))

denominator_2016_ALL_2001_DOD <- length(data_DOD_2001$survive_2016)

survivalrate_2016_ALL_2001_DOD <- numerator_2016_ALL_2001_DOD/denominator_2016_ALL_2001_DOD
survivalrate_2016_ALL_2001_DOD


##SMALL##
numerator_2016_SM_2001_DOD <- length(which(data_DOD_2001$survive_2016==1 & data_DOD_2001$top_smallbiz_bin==1))

denominator_2016_SM_2001_DOD <- length(which(data_DOD_2001$top_smallbiz_bin==1))

survivalrate_2016_SM_2001_DOD <- numerator_2016_SM_2001_DOD/denominator_2016_SM_2001_DOD
survivalrate_2016_SM_2001_DOD


##NONSMALL##
numerator_2016_NS_2001_DOD <- length(which(data_DOD_2001$survive_2016==1 & data_DOD_2001$top_smallbiz_bin==0))

denominator_2016_NS_2001_DOD <- length(which(data_DOD_2001$top_smallbiz_bin==0))

survivalrate_2016_NS_2001_DOD <- numerator_2016_NS_2001_DOD/denominator_2016_NS_2001_DOD
survivalrate_2016_NS_2001_DOD


table(data_DOD_2001$top_smallbiz_bin, data_DOD_2001$survive_2016)


t.test(survive_2016 ~ top_smallbiz_bin, data = data_DOD_2001)


#####DoD 2002#####
#*******************#
#subset the 2002 data
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

##create variable describing whether a firm survived in 2016
data_2002_DOD <- data_2002_DOD %>%
  dplyr::mutate(survive_2016 = ifelse(exitYear < 2016, "0", "1"))


str(data_2002_DOD)

data_2002_DOD$survive_3yr<-as.numeric(as.character(data_2002_DOD$survive_3yr))
data_2002_DOD$survive_5yr<-as.numeric(as.character(data_2002_DOD$survive_5yr))
data_2002_DOD$survive_10yr<-as.numeric(as.character(data_2002_DOD$survive_10yr))
data_2002_DOD$survive_2016<-as.numeric(as.character(data_2002_DOD$survive_2016))

str(data_2002_DOD)

##t test to test the differences between small and non small survival##
table(data_2002_DOD$top_smallbiz_bin)
table(data_2002_DOD$survive_3yr)
table(data_2002_DOD$top_smallbiz_bin, data_2002_DOD$survive_3yr)

t.test(survive_3yr ~ top_smallbiz_bin, data = data_2002_DOD)

#5-year#
table(data_2002_DOD$top_smallbiz_bin, data_2002_DOD$survive_5yr)

t.test(survive_5yr ~ top_smallbiz_bin, data = data_2002_DOD)

#10-year#
table(data_2002_DOD$top_smallbiz_bin, data_2002_DOD$survive_10yr)

t.test(survive_10yr ~ top_smallbiz_bin, data = data_2002_DOD)


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

#denominator_grad2002_DOD_10yr <- length(data_2002_DOD$graduated)

denominator_grad2002_DOD_10yr <- length(which(data_2002_DOD$top_smallbiz_bin==1))

graduated_2002_DOD_10yr <- numerator_grad2002_DOD_10yr/denominator_grad2002_DOD_10yr
graduated_2002_DOD_10yr


#**********#
#***SMALL**#
#**********#
##3-year##
table(data_2002_DOD$survive_3yr) #0=12449, 1=21743

numerator_3yrSM2002_DOD <- length(which(data_2002_DOD$survive_3yr==1 & data_2002_DOD$top_smallbiz_bin==1))

denominator_3yrSM2002_DOD <- length(which(data_2002_DOD$top_smallbiz_bin==1))

survival_3yrSM2002_DOD <- numerator_3yrSM2002_DOD/denominator_3yrSM2002_DOD
survival_3yrSM2002_DOD

##5-year##
table(data_2002_DOD$survive_5yr) #0=10168, 1=17266

numerator_5yrSM2002_DOD <- length(which(data_2002_DOD$survive_5yr==1 & data_2002_DOD$top_smallbiz_bin==1))

denominator_5yrSM2002_DOD <- length(which(data_2002_DOD$top_smallbiz_bin==1))

survival_5yrSM2002_DOD <- numerator_5yrSM2002_DOD/denominator_5yrSM2002_DOD
survival_5yrSM2002_DOD 

##10-year##
table(data_2002_DOD$survive_10yr) #0=10168, 1=17266

numerator_10yrSM2002_DOD <- length(which(data_2002_DOD$survive_10yr==1 & data_2002_DOD$top_smallbiz_bin==1))

denominator_10yrSM2002_DOD <- length(which(data_2002_DOD$top_smallbiz_bin==1))

survival_10yrSM2002_DOD <- numerator_10yrSM2002_DOD/denominator_10yrSM2002_DOD
survival_10yrSM2002_DOD

#**********#
#***NON-SMALL*#
#***********#
##3-year##
table(data_2002_DOD$survive_3yr) #0=12449, 1=21743

numerator_3yrNSM2002_DOD <- length(which(data_2002_DOD$survive_3yr==1 & data_2002_DOD$top_smallbiz_bin==0))

denominator_3yrNSM2002_DOD <- length(which(data_2002_DOD$top_smallbiz_bin==0))

survival_3yrNSM2002_DOD <- numerator_3yrNSM2002_DOD/denominator_3yrNSM2002_DOD
survival_3yrNSM2002_DOD

##5-year##
table(data_2002_DOD$survive_5yr) #0=10168, 1=17266

numerator_5yrNSM2002_DOD <- length(which(data_2002_DOD$survive_5yr==1 & data_2002_DOD$top_smallbiz_bin==0))

denominator_5yrNSM2002_DOD <- length(which(data_2002_DOD$top_smallbiz_bin==0))

survival_5yrNSM2002_DOD <- numerator_5yrNSM2002_DOD/denominator_5yrNSM2002_DOD
survival_5yrNSM2002_DOD 

##10-year##
table(data_2002_DOD$survive_10yr) #0=10168, 1=17266

numerator_10yrNSM2002_DOD <- length(which(data_2002_DOD$survive_10yr==1 & data_2002_DOD$top_smallbiz_bin==0))

denominator_10yrNSM2002_DOD <- length(which(data_2002_DOD$top_smallbiz_bin==0))

survival_10yrNSM2002_DOD <- numerator_10yrNSM2002_DOD/denominator_10yrNSM2002_DOD
survival_10yrNSM2002_DOD

#*******************#
#******2016 check***#
#*******************#
##*********
#***2016 survival 
#**********
survive_10yr_count_2002_DOD <- length(which(data_2002_DOD$survive_10yr==1))
##ALL##
numerator_2016_ALL_2002_DOD <- length(which(data_2002_DOD$survive_2016==1))

denominator_2016_ALL_2002_DOD <- length(data_2002_DOD$survive_2016)

survivalrate_2016_ALL_2002_DOD <- numerator_2016_ALL_2002_DOD/denominator_2016_ALL_2002_DOD
survivalrate_2016_ALL_2002_DOD


##SMALL##
numerator_2016_SM_2002_DOD <- length(which(data_2002_DOD$survive_2016==1 & data_2002_DOD$top_smallbiz_bin==1))

denominator_2016_SM_2002_DOD <- length(which(data_2002_DOD$top_smallbiz_bin==1))

survivalrate_2016_SM_2002_DOD <- numerator_2016_SM_2002_DOD/denominator_2016_SM_2002_DOD
survivalrate_2016_SM_2002_DOD


##NONSMALL##
numerator_2016_NS_2002_DOD<- length(which(data_2002_DOD$survive_2016==1 & data_2002_DOD$top_smallbiz_bin==0))

denominator_2016_NS_2002_DOD <- length(which(data_2002_DOD$top_smallbiz_bin==0))

survivalrate_2016_NS_2002_DOD <- numerator_2016_NS_2002_DOD/denominator_2016_NS_2002_DOD
survivalrate_2016_NS_2002_DOD


table(data_2002_DOD$top_smallbiz_bin, data_2002_DOD$survive_2016)


t.test(survive_2016 ~ top_smallbiz_bin, data = data_2002_DOD)

#####DoD 2003#####
#*******************#
#subset the 2003 data
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

##create variable describing whether a firm survived in 2016
data_2003_DOD <- data_2003_DOD %>%
  dplyr::mutate(survive_2016 = ifelse(exitYear < 2016, "0", "1"))


str(data_2003_DOD)

data_2003_DOD$survive_3yr<-as.numeric(as.character(data_2003_DOD$survive_3yr))
data_2003_DOD$survive_5yr<-as.numeric(as.character(data_2003_DOD$survive_5yr))
data_2003_DOD$survive_10yr<-as.numeric(as.character(data_2003_DOD$survive_10yr))
data_2003_DOD$survive_2016<-as.numeric(as.character(data_2003_DOD$survive_2016))

str(data_2003_DOD)

###
##t test to test the differences between small and non small survival##
table(data_2003_DOD$top_smallbiz_bin)
table(data_2003_DOD$survive_3yr)
table(data_2003_DOD$top_smallbiz_bin, data_2003_DOD$survive_3yr)

t.test(survive_3yr ~ top_smallbiz_bin, data = data_2003_DOD)

#5-year#
table(data_2003_DOD$top_smallbiz_bin, data_2003_DOD$survive_5yr)

t.test(survive_5yr ~ top_smallbiz_bin, data = data_2003_DOD)

#10-year#
table(data_2003_DOD$top_smallbiz_bin, data_2003_DOD$survive_10yr)

t.test(survive_10yr ~ top_smallbiz_bin, data = data_2003_DOD)

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
table(data_2003$graduated)
table(data_2003$survive_10yr)
table(data_2003$top_smallbiz_bin)

table(data_2003_DOD$graduated)
table(data_2003_DOD$survive_10yr)
table(data_2003_DOD$top_smallbiz_bin)


numerator_grad_2003_DOD_10yr <- length(which(data_2003_DOD$graduated==1 & data_2003_DOD$survive_10yr==1))

#denominator_grad_2003_DOD_10yr <- length(data_2003_DOD$graduated)

denominator_grad_2003_DOD_10yr <- length(which(data_2003_DOD$top_smallbiz_bin==1))

graduated_2003_DOD_10yr <- numerator_grad_2003_DOD_10yr/denominator_grad_2003_DOD_10yr
graduated_2003_DOD_10yr


#*******#
#***SMALL***#
#********#
##3-year##
table(data_2003_DOD$survive_3yr) #0=12449, 1=21743

numerator_3yrSM_2003_DOD <- length(which(data_2003_DOD$survive_3yr==1 & data_2003_DOD$top_smallbiz_bin==1))

denominator_3yrSM_2003_DOD <- length(which(data_2003_DOD$top_smallbiz_bin==1))

survival_3yrSM_2003_DOD <- numerator_3yrSM_2003_DOD/denominator_3yrSM_2003_DOD
survival_3yrSM_2003_DOD

##5-year##
table(data_2003_DOD$survive_5yr) #0=10168, 1=17266

numerator_5yrSM_2003_DOD <- length(which(data_2003_DOD$survive_5yr==1 & data_2003_DOD$top_smallbiz_bin==1))

denominator_5yrSM_2003_DOD <- length(which(data_2003_DOD$top_smallbiz_bin==1))

survival_5yrSM_2003_DOD <- numerator_5yrSM_2003_DOD/denominator_5yrSM_2003_DOD
survival_5yrSM_2003_DOD

##10-year##
table(data_2003_DOD$survive_10yr) #0=10168, 1=17266

numerator_10yrSM_2003_DOD <- length(which(data_2003_DOD$survive_10yr==1 & data_2003_DOD$top_smallbiz_bin==1))

denominator_10yrSM_2003_DOD <- length(which(data_2003_DOD$top_smallbiz_bin==1))

survival_10yrSM_2003_DOD <- numerator_10yrSM_2003_DOD/denominator_10yrSM_2003_DOD
survival_10yrSM_2003_DOD

#*****#
#NONSMALL*
#******#
##3-year##
table(data_2003_DOD$survive_3yr) #0=12449, 1=21743

numerator_3yrNSM_2003_DOD <- length(which(data_2003_DOD$survive_3yr==1 & data_2003_DOD$top_smallbiz_bin==0))

denominator_3yrNSM_2003_DOD <- length(which(data_2003_DOD$top_smallbiz_bin==0))

survival_3yrNSM_2003_DOD <- numerator_3yrNSM_2003_DOD/denominator_3yrNSM_2003_DOD
survival_3yrNSM_2003_DOD

##5-year##
table(data_2003_DOD$survive_5yr) #0=10168, 1=17266

numerator_5yrNSM_2003_DOD <- length(which(data_2003_DOD$survive_5yr==1 & data_2003_DOD$top_smallbiz_bin==0))

denominator_5yrNSM_2003_DOD <- length(which(data_2003_DOD$top_smallbiz_bin==0))

survival_5yrNSM_2003_DOD <- numerator_5yrNSM_2003_DOD/denominator_5yrNSM_2003_DOD
survival_5yrNSM_2003_DOD

##10-year##
table(data_2003_DOD$survive_10yr) #0=10168, 1=17266

numerator_10yrNSM_2003_DOD <- length(which(data_2003_DOD$survive_10yr==1 & data_2003_DOD$top_smallbiz_bin==0))

denominator_10yrNSM_2003_DOD <- length(which(data_2003_DOD$top_smallbiz_bin==0))

survival_10yrNSM_2003_DOD <- numerator_10yrNSM_2003_DOD/denominator_10yrNSM_2003_DOD
survival_10yrNSM_2003_DOD

#*******************#
#******2016 check***#
#*******************#
##*********
#***2016 survival 
#**********
survive_10yr_count_2003_DOD <- length(which(data_2003_DOD$survive_10yr==1))
##ALL##
numerator_2016_ALL_2003_DOD <- length(which(data_2003_DOD$survive_2016==1))

denominator_2016_ALL_2003_DOD <- length(data_2003_DOD$survive_2016)

survivalrate_2016_ALL_2003_DOD <- numerator_2016_ALL_2003_DOD /denominator_2016_ALL_2003_DOD 
survivalrate_2016_ALL_2003_DOD 


##SMALL##
numerator_2016_SM_2003_DOD <- length(which(data_2003_DOD$survive_2016==1 & data_2003_DOD$top_smallbiz_bin==1))

denominator_2016_SM_2003_DOD <- length(which(data_2003_DOD$top_smallbiz_bin==1))

survivalrate_2016_SM_2003_DOD <- numerator_2016_SM_2003_DOD /denominator_2016_SM_2003_DOD 
survivalrate_2016_SM_2003_DOD 


##NONSMALL##
numerator_2016_NS_2003_DOD <- length(which(data_2003_DOD$survive_2016==1 & data_2003_DOD$top_smallbiz_bin==0))

denominator_2016_NS_2003_DOD <- length(which(data_2003_DOD$top_smallbiz_bin==0))

survivalrate_2016_NS_2003_DOD  <- numerator_2016_NS_2003_DOD /denominator_2016_NS_2003_DOD 
survivalrate_2016_NS_2003_DOD 


table(data_2003_DOD$top_smallbiz_bin, data_2003_DOD$survive_2016)


t.test(survive_2016 ~ top_smallbiz_bin, data = data_2003_DOD)



#####DoD 2004#####
#*******************#
#subset the 2004 data
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

##create variable describing whether a firm survived in 2016
data_2004_DOD <- data_2004_DOD %>%
  dplyr::mutate(survive_2016 = ifelse(exitYear < 2016, "0", "1"))


str(data_2004)

data_2004_DOD$survive_3yr<-as.numeric(as.character(data_2004_DOD$survive_3yr))
data_2004_DOD$survive_5yr<-as.numeric(as.character(data_2004_DOD$survive_5yr))
data_2004_DOD$survive_10yr<-as.numeric(as.character(data_2004_DOD$survive_10yr))
data_2004_DOD$survive_2016<-as.numeric(as.character(data_2004_DOD$survive_2016))

str(data_2004_DOD)

###
##t test to test the differences between small and non small survival##
table(data_2004_DOD$top_smallbiz_bin)
table(data_2004_DOD$survive_3yr)
table(data_2004_DOD$top_smallbiz_bin, data_2004_DOD$survive_3yr)

t.test(survive_3yr ~ top_smallbiz_bin, data = data_2004_DOD)

#5-year#
table(data_2004_DOD$top_smallbiz_bin, data_2004_DOD$survive_5yr)

t.test(survive_5yr ~ top_smallbiz_bin, data = data_2004_DOD)

#10-year#
table(data_2004_DOD$top_smallbiz_bin, data_2004_DOD$survive_10yr)

t.test(survive_10yr ~ top_smallbiz_bin, data = data_2004_DOD)


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

#denominator_grad_2004_DOD_10yr <- length(data_2004_DOD$graduated)

denominator_grad_2004_DOD_10yr <- length(which(data_2004_DOD$top_smallbiz_bin==1))

graduated_2004_DOD_10yr <- numerator_grad_2004_DOD_10yr/denominator_grad_2004_DOD_10yr
graduated_2004_DOD_10yr 


#******#
#*SMALL*#
#******#
##3-year##
table(data_2004_DOD$survive_3yr) #0=12449, 1=21743

numerator_3yrSM_2004_DOD <- length(which(data_2004_DOD$survive_3yr==1 & data_2004_DOD$top_smallbiz_bin==1))

denominator_3yrSM_2004_DOD <- length(which(data_2004_DOD$top_smallbiz_bin==1))

survival_3yrSM_2004_DOD <- numerator_3yrSM_2004_DOD/denominator_3yrSM_2004_DOD
survival_3yrSM_2004_DOD

##5-year##
table(data_2004_DOD$survive_5yr) #0=10168, 1=17266

numerator_5yrSM_2004_DOD <- length(which(data_2004_DOD$survive_5yr==1 & data_2004_DOD$top_smallbiz_bin==1))

denominator_5yrSM_2004_DOD <- length(which(data_2004_DOD$top_smallbiz_bin==1))

survival_5yrSM_2004_DOD <- numerator_5yrSM_2004_DOD/denominator_5yrSM_2004_DOD
survival_5yrSM_2004_DOD

##10-year##
table(data_2004_DOD$survive_10yr) #0=10168, 1=17266

numerator_10yrSM_2004_DOD <- length(which(data_2004_DOD$survive_10yr==1 & data_2004_DOD$top_smallbiz_bin==1))

denominator_10yrSM_2004_DOD <- length(which(data_2004_DOD$top_smallbiz_bin==1))

survival_10yrSM_2004_DOD <- numerator_10yrSM_2004_DOD/denominator_10yrSM_2004_DOD
survival_10yrSM_2004_DOD



#********#
#NON SMALL#
#********#
##3-year##
table(data_2004_DOD$survive_3yr) #0=12449, 1=21743

numerator_3yrNSM_2004_DOD <- length(which(data_2004_DOD$survive_3yr==1 & data_2004_DOD$top_smallbiz_bin==0))

denominator_3yrNSM_2004_DOD <- length(which(data_2004_DOD$top_smallbiz_bin==0))

survival_3yrNSM_2004_DOD <- numerator_3yrNSM_2004_DOD/denominator_3yrNSM_2004_DOD
survival_3yrNSM_2004_DOD 

##5-year##
table(data_2004_DOD$survive_5yr) #0=10168, 1=17266

numerator_5yrNSM_2004_DOD <- length(which(data_2004_DOD$survive_5yr==1 & data_2004_DOD$top_smallbiz_bin==0))

denominator_5yrNSM_2004_DOD <- length(which(data_2004_DOD$top_smallbiz_bin==0))

survival_5yrNSM_2004_DOD <- numerator_5yrNSM_2004_DOD/denominator_5yrNSM_2004_DOD
survival_5yrNSM_2004_DOD

##10-year##
table(data_2004_DOD$survive_10yr) #0=10168, 1=17266

numerator_10yrNSM_2004_DOD <- length(which(data_2004_DOD$survive_10yr==1 & data_2004_DOD$top_smallbiz_bin==0))

denominator_10yrNSM_2004_DOD <- length(which(data_2004_DOD$top_smallbiz_bin==0))

survival_10yrNSM_2004_DOD <- numerator_10yrNSM_2004_DOD/denominator_10yrNSM_2004_DOD
survival_10yrNSM_2004_DOD 

#*******************#
#******2016 check***#
#*******************#
##*********
#***2016 survival 
#**********
survive_10yr_count_2004_DOD <- length(which(data_2004_DOD$survive_10yr==1))
##ALL##
numerator_2016_ALL_2004_DOD <- length(which(data_2004_DOD$survive_2016==1))

denominator_2016_ALL_2004_DOD <- length(data_2004_DOD$survive_2016)

survivalrate_2016_ALL_2004_DOD <- numerator_2016_ALL_2004_DOD /denominator_2016_ALL_2004_DOD
survivalrate_2016_ALL_2004_DOD 


##SMALL##
numerator_2016_SM_2004_DOD <- length(which(data_2004_DOD$survive_2016==1 & data_2004_DOD$top_smallbiz_bin==1))

denominator_2016_SM_2004_DOD <- length(which(data_2004_DOD$top_smallbiz_bin==1))

survivalrate_2016_SM_2004_DOD <- numerator_2016_SM_2004_DOD /denominator_2016_SM_2004_DOD 
survivalrate_2016_SM_2004_DOD 


##NONSMALL##
numerator_2016_NS_2004_DOD <- length(which(data_2004_DOD$survive_2016==1 & data_2004_DOD$top_smallbiz_bin==0))

denominator_2016_NS_2004_DOD <- length(which(data_2004_DOD$top_smallbiz_bin==0))

survivalrate_2016_NS_2004_DOD  <- numerator_2016_NS_2004_DOD /denominator_2016_NS_2004_DOD 
survivalrate_2016_NS_2004_DOD 


table(data_2004_DOD$top_smallbiz_bin, data_2004_DOD$survive_2016)


t.test(survive_2016 ~ top_smallbiz_bin, data = data_2004_DOD)


#####DoD 2005#####
#*******************#
#subset the 2005 data
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

##create variable describing whether a firm survived in 2016
data_2005_DOD <- data_2005_DOD %>%
  dplyr::mutate(survive_2016 = ifelse(exitYear < 2016, "0", "1"))


str(data_2005_DOD)

data_2005_DOD$survive_3yr<-as.numeric(as.character(data_2005_DOD$survive_3yr))
data_2005_DOD$survive_5yr<-as.numeric(as.character(data_2005_DOD$survive_5yr))
data_2005_DOD$survive_10yr<-as.numeric(as.character(data_2005_DOD$survive_10yr))
data_2005_DOD$survive_2016<-as.numeric(as.character(data_2005_DOD$survive_2016))

str(data_2005_DOD)

##**********
##t test to test the differences between small and non small survival##
table(data_2005_DOD$top_smallbiz_bin)
table(data_2005_DOD$survive_3yr)
table(data_2005_DOD$top_smallbiz_bin, data_2005_DOD$survive_3yr)

t.test(survive_3yr ~ top_smallbiz_bin, data = data_2005_DOD)

#5-year#
table(data_2005_DOD$top_smallbiz_bin, data_2005_DOD$survive_5yr)

t.test(survive_5yr ~ top_smallbiz_bin, data = data_2005_DOD)

#10-year#
table(data_2005_DOD$top_smallbiz_bin, data_2005_DOD$survive_10yr)

t.test(survive_10yr ~ top_smallbiz_bin, data = data_2005_DOD)

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

#denominator_grad_2005_DOD_10yr <- length(data_2005_DOD$graduated)

denominator_grad_2005_DOD_10yr <- length(which(data_2005_DOD$top_smallbiz_bin==1))

graduated_2005_DOD_10yr <- numerator_grad_2005_DOD_10yr/denominator_grad_2005_DOD_10yr
graduated_2005_DOD_10yr


#******#
#SMALL#
#******#
##3-year##
table(data_2005_DOD$survive_3yr) #0=12449, 1=21743

numerator_3yrSM_2005_DOD <- length(which(data_2005_DOD$survive_3yr==1 & data_2005_DOD$top_smallbiz_bin==1))

denominator_3yrSM_2005_DOD <- length(which(data_2005_DOD$top_smallbiz_bin==1))

survival_3yrSM_2005_DOD <- numerator_3yrSM_2005_DOD/denominator_3yrSM_2005_DOD
survival_3yrSM_2005_DOD

##5-year##
table(data_2005_DOD$survive_5yr) #0=10168, 1=17266

numerator_5yrSM_2005_DOD <- length(which(data_2005_DOD$survive_5yr==1 & data_2005_DOD$top_smallbiz_bin==1))

denominator_5yrSM_2005_DOD <- length(which(data_2005_DOD$top_smallbiz_bin==1))

survival_5yrSM_2005_DOD <- numerator_5yrSM_2005_DOD/denominator_5yrSM_2005_DOD
survival_5yrSM_2005_DOD 

##10-year##
table(data_2005_DOD$survive_10yr) #0=10168, 1=17266

numerator_10yrSM_2005_DOD <- length(which(data_2005_DOD$survive_10yr==1 & data_2005_DOD$top_smallbiz_bin==1))

denominator_10yrSM_2005_DOD <- length(which(data_2005_DOD$top_smallbiz_bin==1))

survival_10yrSM_2005_DOD <- numerator_10yrSM_2005_DOD/denominator_10yrSM_2005_DOD
survival_10yrSM_2005_DOD

#*******#
#NON SMAL#
#*******#
##3-year##
table(data_2005_DOD$survive_3yr) #0=12449, 1=21743

numerator_3yrNSM_2005_DOD <- length(which(data_2005_DOD$survive_3yr==1 & data_2005_DOD$top_smallbiz_bin==0))

denominator_3yrNSM_2005_DOD <- length(which(data_2005_DOD$top_smallbiz_bin==0))

survival_3yrNSM_2005_DOD <- numerator_3yrNSM_2005_DOD/denominator_3yrNSM_2005_DOD
survival_3yrNSM_2005_DOD

##5-year##
table(data_2005_DOD$survive_5yr) #0=10168, 1=17266

numerator_5yrNSM_2005_DOD <- length(which(data_2005_DOD$survive_5yr==1 & data_2005_DOD$top_smallbiz_bin==0))

denominator_5yrNSM_2005_DOD <- length(which(data_2005_DOD$top_smallbiz_bin==0))

survival_5yrNSM_2005_DOD <- numerator_5yrNSM_2005_DOD/denominator_5yrNSM_2005_DOD
survival_5yrNSM_2005_DOD

##10-year##
table(data_2005_DOD$survive_10yr) #0=10168, 1=17266

numerator_10yrNSM_2005_DOD <- length(which(data_2005_DOD$survive_10yr==1 & data_2005_DOD$top_smallbiz_bin==0))

denominator_10yrNSM_2005_DOD <- length(which(data_2005_DOD$top_smallbiz_bin==0))

survival_10yrNSM_2005_DOD <- numerator_10yrNSM_2005_DOD/denominator_10yrNSM_2005_DOD
survival_10yrNSM_2005_DOD


#*******************#
#******2016 check***#
#*******************#
##*********
#***2016 survival 
#**********
survive_10yr_count_2005_DOD <- length(which(data_2005_DOD$survive_10yr==1))
##ALL##
numerator_2016_ALL_2005_DOD <- length(which(data_2005_DOD$survive_2016==1))

denominator_2016_ALL_2005_DOD <- length(data_2005_DOD$survive_2016)

survivalrate_2016_ALL_2005_DOD <- numerator_2016_ALL_2005_DOD /denominator_2016_ALL_2005_DOD
survivalrate_2016_ALL_2005_DOD 


##SMALL##
numerator_2016_SM_2005_DOD <- length(which(data_2005_DOD$survive_2016==1 & data_2005_DOD$top_smallbiz_bin==1))

denominator_2016_SM_2005_DOD <- length(which(data_2005_DOD$top_smallbiz_bin==1))

survivalrate_2016_SM_2005_DOD <- numerator_2016_SM_2005_DOD /denominator_2016_SM_2005_DOD 
survivalrate_2016_SM_2005_DOD


##NONSMALL##
numerator_2016_NS_2005_DOD <- length(which(data_2005_DOD$survive_2016==1 & data_2005_DOD$top_smallbiz_bin==0))

denominator_2016_NS_2005_DOD <- length(which(data_2005_DOD$top_smallbiz_bin==0))

survivalrate_2016_NS_2005_DOD  <- numerator_2016_NS_2005_DOD /denominator_2016_NS_2005_DOD
survivalrate_2016_NS_2005_DOD 


table(data_2005_DOD$top_smallbiz_bin, data_2005_DOD$survive_2016)


t.test(survive_2016 ~ top_smallbiz_bin, data = data_2005_DOD)


#####DoD 2006#####
#*******************#
#subset the 2006 data
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

##create variable describing whether a firm survived in 2016
data_2006_DOD <- data_2006_DOD %>%
  dplyr::mutate(survive_2016 = ifelse(exitYear < 2016, "0", "1"))


str(data_2006_DOD)

data_2006_DOD$survive_3yr<-as.numeric(as.character(data_2006_DOD$survive_3yr))
data_2006_DOD$survive_5yr<-as.numeric(as.character(data_2006_DOD$survive_5yr))
data_2006_DOD$survive_10yr<-as.numeric(as.character(data_2006_DOD$survive_10yr))
data_2006_DOD$survive_2016<-as.numeric(as.character(data_2006_DOD$survive_2016))

str(data_2006_DOD)

#**********
##t test to test the differences between small and non small survival##
table(data_2006_DOD$top_smallbiz_bin)
table(data_2006_DOD$survive_3yr)
table(data_2006_DOD$top_smallbiz_bin, data_2006_DOD$survive_3yr)

t.test(survive_3yr ~ top_smallbiz_bin, data = data_2006_DOD)

#5-year#
table(data_2006_DOD$top_smallbiz_bin, data_2006_DOD$survive_5yr)

t.test(survive_5yr ~ top_smallbiz_bin, data = data_2006_DOD)

#10-year#
table(data_2006_DOD$top_smallbiz_bin, data_2006_DOD$survive_10yr)

t.test(survive_10yr ~ top_smallbiz_bin, data = data_2006_DOD)


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

#denominator_grad_2006_DOD_10yr <- length(data_2006_DOD$graduated)

denominator_grad_2006_DOD_10yr <- length(which(data_2006_DOD$top_smallbiz_bin==1))

graduated_2006_DOD_10yr <- numerator_grad_2006_DOD_10yr/denominator_grad_2006_DOD_10yr
graduated_2006_DOD_10yr


#*****#
#***SMALL***
#*******#
##3-year##
table(data_2006_DOD$survive_3yr) #0=12449, 1=21743

numerator_3yrSM_2006_DOD <- length(which(data_2006_DOD$survive_3yr==1 & data_2006_DOD$top_smallbiz_bin==1))

denominator_3yrSM_2006_DOD <- length(which(data_2006_DOD$top_smallbiz_bin==1))

survival_3yrSM_2006_DOD <- numerator_3yrSM_2006_DOD/denominator_3yrSM_2006_DOD
survival_3yrSM_2006_DOD 

##5-year##
table(data_2006_DOD$survive_5yr) #0=10168, 1=17266

numerator_5yrSM_2006_DOD <- length(which(data_2006_DOD$survive_5yr==1 & data_2006_DOD$top_smallbiz_bin==1))

denominator_5yrSM_2006_DOD <- length(which(data_2006_DOD$top_smallbiz_bin==1))

survival_5yrSM_2006_DOD <- numerator_5yrSM_2006_DOD/denominator_5yrSM_2006_DOD
survival_5yrSM_2006_DOD 

##10-year##
table(data_2006_DOD$survive_10yr) #0=10168, 1=17266

numerator_10yrSM_2006_DOD <- length(which(data_2006_DOD$survive_10yr==1 & data_2006_DOD$top_smallbiz_bin==1))

denominator_10yrSM_2006_DOD <- length(which(data_2006_DOD$top_smallbiz_bin==1))

survival_10yrSM_2006_DOD <- numerator_10yrSM_2006_DOD/denominator_10yrSM_2006_DOD
survival_10yrSM_2006_DOD


#********#
#*NON SMALL*#
#*********#
##3-year##
table(data_2006_DOD$survive_3yr) #0=12449, 1=21743

numerator_3yrNSM_2006_DOD <- length(which(data_2006_DOD$survive_3yr==1 & data_2006_DOD$top_smallbiz_bin==0))

denominator_3yrNSM_2006_DOD <- length(which(data_2006_DOD$top_smallbiz_bin==0))

survival_3yrNSM_2006_DOD <- numerator_3yrNSM_2006_DOD/denominator_3yrNSM_2006_DOD
survival_3yrNSM_2006_DOD

##5-year##
table(data_2006_DOD$survive_5yr) #0=10168, 1=17266

numerator_5yrNSM_2006_DOD <- length(which(data_2006_DOD$survive_5yr==1 & data_2006_DOD$top_smallbiz_bin==0))

denominator_5yrNSM_2006_DOD <- length(which(data_2006_DOD$top_smallbiz_bin==0))

survival_5yrNSM_2006_DOD <- numerator_5yrNSM_2006_DOD/denominator_5yrNSM_2006_DOD
survival_5yrNSM_2006_DOD 

##10-year##
table(data_2006_DOD$survive_10yr) #0=10168, 1=17266

numerator_10yrNSM_2006_DOD <- length(which(data_2006_DOD$survive_10yr==1 & data_2006_DOD$top_smallbiz_bin==0))

denominator_10yrNSM_2006_DOD <- length(which(data_2006_DOD$top_smallbiz_bin==0))

survival_10yrNSM_2006_DOD <- numerator_10yrNSM_2006_DOD/denominator_10yrNSM_2006_DOD
survival_10yrNSM_2006_DOD 

#*******************#
#******2016 check***#
#*******************#
##*********
#***2016 survival 
#**********
survive_10yr_count_2006_DOD <- length(which(data_2006_DOD$survive_10yr==1))
##ALL##
numerator_2016_ALL_2006_DOD <- length(which(data_2006_DOD$survive_2016==1))

denominator_2016_ALL_2006_DOD <- length(data_2006_DOD$survive_2016)

survivalrate_2016_ALL_2006_DOD <- numerator_2016_ALL_2006_DOD /denominator_2016_ALL_2006_DOD
survivalrate_2016_ALL_2006_DOD 


##SMALL##
numerator_2016_SM_2006_DOD <- length(which(data_2006_DOD$survive_2016==1 & data_2006_DOD$top_smallbiz_bin==1))

denominator_2016_SM_2006_DOD <- length(which(data_2006_DOD$top_smallbiz_bin==1))

survivalrate_2016_SM_2006_DOD <- numerator_2016_SM_2006_DOD /denominator_2016_SM_2006_DOD
survivalrate_2016_SM_2006_DOD


##NONSMALL##
numerator_2016_NS_2006_DOD <- length(which(data_2006_DOD$survive_2016==1 & data_2006_DOD$top_smallbiz_bin==0))

denominator_2016_NS_2006_DOD <- length(which(data_2006_DOD$top_smallbiz_bin==0))

survivalrate_2016_NS_2006_DOD  <- numerator_2016_NS_2006_DOD /denominator_2016_NS_2006_DOD
survivalrate_2016_NS_2006_DOD


table(data_2006_DOD$top_smallbiz_bin, data_2006_DOD$survive_2016)


t.test(survive_2016 ~ top_smallbiz_bin, data = data_2006_DOD)


#******************************************************************************************************
#*******************#
#####GRAPHS#####
#*******************#

####Non-DOD: Number of newe Entrants per year 2001-2016####

##creates a dataframe that counts how many new entrants enter in each year
count_total_newentrants_nd <- FPDS_cleaned_unique_nd %>% 
  filter(top_smallbiz_bin == 1 | top_smallbiz_bin == 0) %>% 
  group_by(registrationYear) %>% 
  dplyr::summarise(n())  

##creates a dataframe that counts how many small vendors and how many non-small vendors are in each year
#and then joins it with the counts of all new vendors in each year
FPDS_bargraphCount_nd <- FPDS_cleaned_unique_nd %>%
  filter(top_smallbiz_bin == 1 | top_smallbiz_bin == 0) %>%
  group_by(registrationYear, top_smallbiz_bin) %>%
  dplyr::summarise(n()) %>%
  dplyr::rename("regpersize"=`n()`) %>%
  left_join(count_total_newentrants_nd, by = "registrationYear") %>%
  dplyr::rename("regperyear"=`n()`) 


NE_count_nd <- ggplot(FPDS_bargraphCount_nd, aes(x = registrationYear, y = regpersize, fill = factor(top_smallbiz_bin), label = regperyear)) +
  geom_bar(stat = 'identity', position = 'stack') +
  ylab("Number of New Entrants") +
  xlab("Registration Year") +
  scale_x_continuous(breaks = c(2001:2016)) +
  scale_fill_manual(name = "New Entrant Type", values = c("#66CCCC", "#336666"), labels = c("non-small", "small")) +
  ggtitle("Number of New Entrants Per Year (2001-2016) - Non-DoD")+
  geom_text(data = subset(FPDS_bargraphCount_nd, registrationYear <= 2016), aes(label = regpersize), size = 4, position = position_stack(vjust = .5), angle = 45) +
  scale_y_continuous(label=comma)

####DOD: Number of newe Entrants per year 2001-2016####
totyear_count_dod <- FPDS_cleaned_unique_DOD %>% 
  filter(top_smallbiz_bin == 1 | top_smallbiz_bin == 0) %>% 
  group_by(registrationYear) %>% 
  dplyr::summarise(n())  


FPDS_bargraphCount_dod <- FPDS_cleaned_unique_DOD %>%
  filter(top_smallbiz_bin == 1 | top_smallbiz_bin == 0) %>%
  group_by(registrationYear, top_smallbiz_bin) %>%
  dplyr::summarise(n()) %>%
  dplyr::rename("regpersize"=`n()`) %>%
  left_join(totyear_count_dod, by = "registrationYear") %>%
  dplyr::rename("regperyear"=`n()`) 


NE_count_DoD <- ggplot(FPDS_bargraphCount_dod, aes(x = registrationYear, y = regpersize, fill = factor(top_smallbiz_bin), label = regperyear)) +
  geom_bar(stat = 'identity', position = 'stack') +
  ylab("Number of New Entrants") +
  xlab("Registration Year") +
  scale_x_continuous(breaks = c(2001:2016)) +
  scale_fill_manual(name = "New Entrant Type", values = c("#66CCCC", "#336666"), labels = c("non-small", "small")) +
  ggtitle("Number of New Entrants Per Year (2001-2016) - DoD")+
  geom_text(data = subset(FPDS_bargraphCount_dod, registrationYear <= 2016), aes(label = regpersize), size = 4, position = position_stack(vjust = .5), angle = 45) +
  scale_y_continuous(label=comma)

##combine graphs
grid.arrange(NE_count_nd, NE_count_DoD)
