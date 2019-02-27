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
library(ggthemes)
library(extrafont)
library(scales)
library(magrittr)
library(csis360)
library(scales)
library(gtable)
library(grid)
library(gridExtra)

##sam work computer
setwd("K:/2018-01 NPS New Entrants/Data/Data/Cleaning data/FPDS")

##sam laptop
#setwd("/Users/samanthacohen/Desktop/Diig backup/New Entrants/R Data")

load(file = "FPDS_datapull_all_v3_allfed.Rda")


#******************************************************************************************************

#*****************************************************************#
####Calculate the survival and graduation rates using FPDS Data for all federal agencies####
#*****************************************************************#


#********************#
####load data sets####
#********************#

dataframelist <- list("data_2001.Rda", "data_2001_DOD.Rda", "data_2002.Rda", "data_2002_DOD.Rda",
                 "data_2003.Rda", "data_2003_DOD.Rda", "data_2004.Rda", "data_2004_DOD.Rda",
                 "data_2005.Rda", "data_2005_DOD.Rda", "data_2006.Rda", "data_2006_DOD.Rda")

for(i in dataframelist){
  load(file = i)
}

#**********************#
####Survival Rates####
#**********************#

calc_survivalrates <- function(x) {
  #ALL#
  #3 year
  numerator_1 <- length(which(x$survive_3yr==1))
  denominator_1 <- length(x$survive_3yr)
  
  survival_1 <- numerator_1/denominator_1
  
  #5 year
  numerator_2 <- length(which(x$survive_5yr==1))
  denominator_2 <- length(x$survive_5yr)
  
  survival_2 <- numerator_2/denominator_2
  
  #10 year
  numerator_3 <- length(which(x$survive_10yr==1))
  denominator_3 <- length(x$survive_10yr)
  
  survival_3 <- numerator_3/denominator_3
  
  #2016
  numerator_4 <- length(which(x$survive_2016==1))
  denominator_4 <- length(x$survive_2016)
  
  survivale_4 <- numerator_4/denominator_4
  
  #SMALL#
  #3 year
  numerator_5 <- length(which(x$survive_3yr==1 & x$top_smallbiz_bin==1))
  denominator_5 <- length(which(x$top_smallbiz_bin==1))
  
  survival_5 <- numerator_4/denominator_4
  
  #5 year
  numerator_6 <- length(which(x$survive_5yr==1 & x$top_smallbiz_bin==1))
  denominator_6 <- length(which(x$top_smallbiz_bin==1))
  
  survival_6 <- numerator_5/denominator_5
  
  #10 year
  numerator_7 <- length(which(x$survive_5yr==1 & x$top_smallbiz_bin==1))
  denominator_7 <- length(which(x$top_smallbiz_bin==1))
  
  survival_7 <- numerator_5/denominator_5
  
  #2016
  numerator_8 <- length(which(x$survive_2016==1 & x$top_smallbiz_bin==1))
  denominator_8 <- length(which(x$top_smallbiz_bin==1))
  
  survival_8 <- numerator_8/denominator_8
  
  #NONSMALL#
  #3 year
  numerator_9 <- length(which(x$survive_3yr==1 & x$top_smallbiz_bin==0))
  denominator_9 <- length(which(x$top_smallbiz_bin==0))
  
  survival_9 <- numerator_9/denominator_9
  
  #5 year 
  numerator_10 <- length(which(x$survive_5yr==1 & x$top_smallbiz_bin==0))
  denominator_10 <- length(which(x$top_smallbiz_bin==0))
  
  survival_10 <- numerator_10/denominator_10
  
  #10 year
  numerator_11 <- length(which(x$survive_10yr==1 & x$top_smallbiz_bin==0))
  denominator_11 <- length(which(x$top_smallbiz_bin==0))
  
  survival_11 <- numerator_11/denominator_11
  
  #2016
  numerator_12 <- length(which(x$survive_2016==1 & x$top_smallbiz_bin==0))
  denominator_12 <- length(which(x$top_smallbiz_bin==0))
  
  survival_12 <- numerator_12/denominator_12
  
  #display results
  all_survivalrates <- c(survival_1, survival_2, survival_3, survival_4, survival_5,
                         survival_6, survival_7, survival_8, survival_9, survival_10,
                         survival_11, survival_12)
  
}


#loop#

datalist <- list(data_2001=data_2001, data_2001_DoD=data_2001_DOD, 
                 data_2002=data_2002, data_2002_DoD=data_2002_DOD, 
                 data_2003=data_2003, data_2003_DoD=data_2003_DOD, 
                 data_2004=data_2004, data_2004_DoD=data_2004_DOD, 
                 data_2005=data_2005, data_2005_DoD=data_2005_DOD, 
                 data_2006=data_2006, data_2006_DoD=data_2006_DOD)

survivalrates <- lapply(datalist, calc_survivalrates)

survivalrates


#*********************************************************#
####t-tests between small and non-small survival rates####
#*********************************************************#

calc_ttests <- function(x){
  t.test(survive_3yr ~ top_smallbiz_bin, data = x)
  t.test(survive_5yr ~ top_smallbiz_bin, data = x)
  t.test(survive_10yr ~ top_smallbiz_bin, data = x)
  t.test(survive_2016 ~ top_smallbiz_bin, data = x)
}


datalist <- list(data_2001=data_2001, data_2001_DoD=data_2001_DOD, 
                 data_2002=data_2002, data_2002_DoD=data_2002_DOD, 
                 data_2003=data_2003, data_2003_DoD=data_2003_DOD, 
                 data_2004=data_2004, data_2004_DoD=data_2004_DOD, 
                 data_2005=data_2005, data_2005_DoD=data_2005_DOD, 
                 data_2006=data_2006, data_2006_DoD=data_2006_DOD)


t_tests <- lapply(datalist, calc_ttests)

t_tests



#**********************#
####Graduation Rates####
#**********************#

#function#
calc_gradrates <- function(x) {
  denominator <- length(which(x$top_smallbiz_bin==1))
  numerator_1 <- length(which(x$graduated==1))
  numerator_2 <- length(which(x$graduated==1 & x$survive_10yr==0)) 
  numerator_3 <- length(which(x$graduated==1 & x$survive_10yr==1)) 
  numerator_4 <- length(which(x$graduated==1 & x$survive_10yr==1 & x$survive_2016==0)) 
  numerator_5 <- length(which(x$graduated==1 & x$survive_2016==1))
  
  gradrate_1 <- numerator_1/denominator ##all small NE
  gradrate_2 <- numerator_2/denominator ##NE that did not survive 10 years
  gradrate_3 <- numerator_3/denominator ##NE that did survive 10 years
  gradrate_4 <- numerator_4/denominator ##NE that did not survive in 2016
  graduate_5 <- numerator_5/denominator ##NE that did survive in 2016
  
  allrates <- c(gradrate_1, gradrate_2, gradrate_3, gradrate_4, graduate_5)
  allrates
  
}

#loop#

datalist <- list(data_2001=data_2001, data_2001_DoD=data_DOD_2001, 
                 data_2002=data_2002, data_2002_DoD=data_2002_DOD, 
                 data_2003=data_2003, data_2003_DoD=data_2003_DOD, 
                 data_2004=data_2004, data_2004_DoD=data_2004_DOD, 
                 data_2005=data_2005, data_2005_DoD=data_2005_DOD, 
                 data_2006=data_2006, data_2006_DoD=data_2006_DOD)
gradrates <- lapply(datalist, calc_gradrates)

gradrates

#**************************************************************#


#************************************#
####survival and grad rates charts####
#************************************#



















#**************************************************************#


