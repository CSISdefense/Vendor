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
library(csis360)

#*****************************************************************#
#*****************************************************************#
#*****************************************************************#
#*****************************************************************#
#!!!!!!!!!!!!!!!!!!!!!!!READ ME!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
# Before executing this R file, choose whether you want to clean the data
# with all federal agencies or just for DoD that includes subcustomer 
# field. First check which data you load (lines ~ 26 and 41) and then
# you will have to go to the save section and decide which save you need to do
#AND AROUND 836, change cleaned_ or cleaned (former for services latter 
#for all fed agencies)

# ALSO need to comment out section called "choose navy category depending..."
# when doing all federal agencies data cleaning!
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#

#******************************************************************
########################FPDS data################################
#******************************************************************

#******************************************************
#!!!!!import from raw data folder not copied into cleaning data folder
setwd("K:/2018-01 NPS New Entrants/Data/Data/Raw Data/FPDS")

##without services
FPDS_data <- read.delim("Vendor.SP_DunsnumberNewEntrants_all.txt", fill = TRUE, header=TRUE,  na.strings = c("", "NULL"))


##with services 

#FPDS_data <- read.delim("Vendor.SP_DunsnumberNewEntrants_all_withservices.txt", fill = TRUE, header=TRUE,  na.strings = c("", "NULL"))


# ###set up
# 
# setwd("K:/2018-01 NPS New Entrants/Data/Data/Cleaning data/FPDS")
# 
# 
# ##Get and Save into CSV
# FPDS_data <- read.delim("Vendor.SP_DunsnumberNewEntrants_all_RAW.txt", fill = TRUE, header=TRUE,  na.strings = c("", "NULL"))
# 
# 
# write.csv(FPDS_data, file="FPDS_all.csv")
# 
# str(FPDS_data$Dunsnumber)
# #*****************************************************
# ##Use the data
# 
# FPDS_data <- read.csv("FPDS_all.csv")

setwd("K:/2018-01 NPS New Entrants/Data/Data/Cleaning data/FPDS")
##*****************************************##
############prelim cleaning#################
#*******************************************#

##rename DirectCOBSD to biz_size and i..fiscal_year to FYear

names(FPDS_data)

names(FPDS_data)[names(FPDS_data) == "DirectCOBSD"] <- "biz_size"
#GSS: Added remove_bom from CSIS360 to handle the ?.. removal. 
FPDS_data<-remove_bom(FPDS_data)
names(FPDS_data)[names(FPDS_data) == "fiscal_year"] <- "FYear"


### Investigate how many unique duns # there are

sum(is.na(FPDS_data$Dunsnumber)) ##604 missing Dunsnumbers

##drop missing Dunsnumbers
FPDS_data <- FPDS_data[!(is.na(FPDS_data$Dunsnumber)), ]

unique_FPDS_duns <- data.frame(table(FPDS_data$Dunsnumber)) ##gives a data frame with a list of duns and the number of times they occurred
##there are 810382

##sort by year and duns#

FPDS_data <- FPDS_data[order(FPDS_data$FYear, FPDS_data$Dunsnumber), ]


##count number of unique duns numbers
unique_FPDS_duns_studyperiod <- data.frame(table(FPDS_data$Dunsnumber)) ##gives a data frame with a list of duns and the number of times they occurred

# ##check number of characters in duns
# nchar_ <- nchar(FPDS_data$Dunsnumber)
# 
# summary(nchar_)

##*************************************##
####Deflating Dollar Amounts####
#****************************************#



FPDS_data <- deflate(FPDS_data, money_var = "obligatedAmount", fy_var = "FYear")

##update obligatedAmount: change orignial 
#'obligatedAmount' to 'obligatedAmount_nominal'
#and
#'obligatedAmount.Deflator.2016' to 'obligatedAmount'

names(FPDS_data)[names(FPDS_data) == "obligatedAmount"] <- "obligatedAmount_nominal"
names(FPDS_data)[names(FPDS_data) == "obligatedAmount.Deflator.2016"] <- "obligatedAmount"



##*****************************************##
############Choose NAICS category depending#################
#on lgst dollar amount
#*******************************************#

##NAICS2 = the NAICS category that has the highest amount of obligations

##count number of NAs in NAICS2
sum(is.na(FPDS_data$NAICS2)) ##480808 --> 7 percent of the observations

##create a variable that is equal to the total obligations over the 
# study period for each duns number


##create subsetted data with only FY, duns, obligated amount, and naics
Duns_obligations <- data.frame( FPDS_data$FYear, FPDS_data$Dunsnumber, FPDS_data$obligatedAmount, FPDS_data$NAICS2)
names(Duns_obligations)

##count unique duns & how many rows each Dunsnumber has
unique_Duns_obligations <- data.frame(table(Duns_obligations$FPDS_data.Dunsnumber)) ##810382

##drop NA's in NAICS2
sum(is.na(Duns_obligations)) ##18995
Duns_obligations <- Duns_obligations[!(is.na(Duns_obligations$FPDS_data.NAICS2)), ] 

##sort by duns
names(Duns_obligations)
Duns_obligations <- Duns_obligations[order(Duns_obligations$FPDS_data.Dunsnumber, Duns_obligations$FPDS_data.FYear, Duns_obligations$FPDS_data.NAICS2), ]

##see number of NA's in obligated amount in Duns_obligations
names(Duns_obligations)
sum(is.na(Duns_obligations$FPDS_data.obligatedAmount)) ##0
sum(is.na(Duns_obligations$FPDS_data.NAICS2)) ##0

##step 1: use dplyr to create a new data frame grouped by DUNS nad NAICS
#and sum obligated amount for all unique combinations
DO_newvar <- Duns_obligations %>% group_by(FPDS_data.Dunsnumber, FPDS_data.NAICS2) %>%
  dplyr::summarize(obligated_amount=sum(FPDS_data.obligatedAmount, na.rm=TRUE))

#there are 1042339 unique combinations of DUNS and NAICS

#count NAs in NAICS for DO_newvar
sum(is.na(DO_newvar$FPDS_data.NAICS2)) ##0


#step2: eliminate the combinations that are not the highest
DO_max_newvar <- DO_newvar %>% group_by(FPDS_data.Dunsnumber) %>%
  dplyr::mutate(desc_rank = row_number(desc(obligated_amount))) ##row_number with ties (there were 31) it chooses the first one  

unique_Duns_DO_max_newvar <- data.frame(table(DO_max_newvar$FPDS_data.Dunsnumber)) ##18214

#step3: subset DO_max_newvar where rank==1

duns_and_NAICS <- subset(DO_max_newvar, desc_rank==1)

#check to see if FPDS_data.Dunsnumber is a unique identifier
##check uniqueness of DUNS as a variable##

n_distinct(duns_and_NAICS$FPDS_data.Dunsnumber) ##returns 628179

length(unique(duns_and_NAICS$FPDS_data.Dunsnumber)) == nrow(duns_and_NAICS) ##TRUE

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

FPDS_data$TransactionCount <- FPDS_data$IsSAMduns <- FPDS_data$OriginIsInternational <- NULL

FPDS_data_w_topNAICS <- join(FPDS_data, duns_and_NAICS, by = "Dunsnumber", type = "left", match = "all")

##check number of NAs again
sum(is.na(FPDS_data_w_topNAICS$topNAICS)) ##57420 


##*****************************************##
####Choose small or large depending####
#on lgst dollar amount
#*******************************************#

##Small = small or non-small depending on which category has the highest
#amount of obligations

##count number of NAs in biz_size
sum(is.na(FPDS_data_w_topNAICS$biz_size)) ##523 --> .00 percent of the observations

##create subsetted data with only FY, duns, obligated amount, and biz_size
names(FPDS_data)
Duns_smallbiz <- data.frame( FPDS_data$FYear, FPDS_data$Dunsnumber, FPDS_data$obligatedAmount, FPDS_data$biz_size)

##count how many rows each Dunsnumber has
names(Duns_smallbiz)
unique_duns_fpdsclean <- data.frame(table(Duns_smallbiz$FPDS_data.Dunsnumber)) ##gives a data frame with a list of duns and the number of times they occurred

##sort by duns
names(Duns_smallbiz)
Duns_smallbiz <- Duns_smallbiz[order(Duns_smallbiz$FPDS_data.Dunsnumber, Duns_smallbiz$FPDS_data.FYear, Duns_smallbiz$FPDS_data.biz_size), ]


##step 1: use dplyr to create a new data frame grouped by DUNS nad biz_size
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

n_distinct(duns_and_smallbiz$FPDS_data.Dunsnumber) ##returns 664767

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
sum(is.na(FPDS_data_w_topNAICS_topSB$topNAICS)) ##57420 --> 7 percent of the observations
sum(is.na(FPDS_data_w_topNAICS_topSB$top_small_biz)) ##129 


##********************************##
####change top_small_biz to 1 = small, 0 = non-small####
##********************************##

##change top_small_biz to 1= small biz and 0 = non-small biz

str(FPDS_data_w_topNAICS_topSB$top_small_biz)

table(FPDS_data_w_topNAICS_topSB$top_small_biz)

FPDS_data_w_topNAICS_topSB <- FPDS_data_w_topNAICS_topSB[complete.cases(FPDS_data_w_topNAICS_topSB$top_small_biz), ]
FPDS_data_w_topNAICS_topSB <- FPDS_data_w_topNAICS_topSB[!(FPDS_data_w_topNAICS_topSB$top_small_biz==":"), ]

table(FPDS_data_w_topNAICS_topSB$top_small_biz)

##make top biz_size binary
FPDS_data_w_topNAICS_topSB$top_smallbiz_bin <- revalue(FPDS_data_w_topNAICS_topSB$top_small_biz, c("S"="1", "O"="0"))

str(FPDS_data_w_topNAICS_topSB$top_smallbiz_bin)

table(FPDS_data_w_topNAICS_topSB$top_smallbiz_bin)


##change biz_size to binary
str(FPDS_data_w_topNAICS_topSB$biz_size)

table(FPDS_data_w_topNAICS_topSB$biz_size)

FPDS_data_w_topNAICS_topSB <- FPDS_data_w_topNAICS_topSB[complete.cases(FPDS_data_w_topNAICS_topSB$biz_size), ]
FPDS_data_w_topNAICS_topSB <- FPDS_data_w_topNAICS_topSB[!(FPDS_data_w_topNAICS_topSB$biz_size==":"), ]

table(FPDS_data_w_topNAICS_topSB$biz_size)

##make biz_size binary
FPDS_data_w_topNAICS_topSB$biz_size <- revalue(FPDS_data_w_topNAICS_topSB$biz_size, c("S"="1", "O"="0"))

str(FPDS_data_w_topNAICS_topSB$biz_size)

table(FPDS_data_w_topNAICS_topSB$biz_size)

##drop old top small biz var
FPDS_data_w_topNAICS_topSB$top_small_biz <- NULL


####calculate whether a vendor graduated during the period####


##calculate entry year

Duns_minsigndate <- data.frame(FPDS_data$FYear, FPDS_data$Dunsnumber, FPDS_data$obligatedAmount, FPDS_data$AnnualMaxOfSignedDate)

names(Duns_minsigndate)[names(Duns_minsigndate) == "FPDS_data.Dunsnumber"] <- "Dunsnumber"
names(Duns_minsigndate)[names(Duns_minsigndate) == "FPDS_data.FYear"] <- "FYear"
names(Duns_minsigndate)[names(Duns_minsigndate) == "FPDS_data.obligatedAmount"] <- "obligated_amount"
names(Duns_minsigndate)[names(Duns_minsigndate) == "FPDS_data.AnnualMaxOfSignedDate"] <- "AnnualMaxSignedDate"


##sort by duns
names(Duns_minsigndate)
Duns_minsigndate <- Duns_minsigndate[order(Duns_minsigndate$Dunsnumber, Duns_minsigndate$FYear, Duns_minsigndate$AnnualMaxSignedDate), ]

##create a variable that ranks max signed date, 1 being the lowest signed date
DO_newvar_minsigndate <- Duns_minsigndate %>% group_by(Dunsnumber) %>%
  dplyr::mutate(asc_rank = row_number(AnnualMaxSignedDate)) 

##subset DO_max_newvar where rank==1

duns_and_minsigndate <- subset(DO_newvar_minsigndate, asc_rank==1)


#check to see if FPDS_data.Dunsnumber is a unique identifier
##check uniqueness of DUNS as a variable##

n_distinct(duns_and_minsigndate$Dunsnumber) ##

length(unique(duns_and_minsigndate$Dunsnumber)) == nrow(duns_and_minsigndate) ##TRUE

#drop obligatedamount and desc_rank

duns_and_minsigndate$obligated_amount <- duns_and_minsigndate$asc_rank <- duns_and_minsigndate$FYear <- NULL

#change name of 
names(duns_and_minsigndate)[names(duns_and_minsigndate) == "AnnualMaxSignedDate"] <- "obsv_period_MINsigndate"

##merge data
FPDS_data_w_topNAICS_topSB_minsigneddate <- join(FPDS_data_w_topNAICS_topSB, duns_and_minsigndate, by = c("Dunsnumber"), type = "left", match = "all")

##change minsigndate to entry year
FPDS_data_w_topNAICS_topSB_minsigneddate$obsv_period_MINsigndate<-as.Date(as.character(FPDS_data_w_topNAICS_topSB_minsigneddate$obsv_period_MINsigndate))

str(FPDS_data_w_topNAICS_topSB_minsigneddate$obsv_period_MINsigndate)

##create entry year
FPDS_data_w_topNAICS_topSB_minsigneddate <- FPDS_data_w_topNAICS_topSB_minsigneddate %>%
  dplyr::mutate(entryyear = (format(FPDS_data_w_topNAICS_topSB_minsigneddate$obsv_period_MINsigndate, "%Y")))

str(FPDS_data_w_topNAICS_topSB_minsigneddate$entryyear) ##it's a character

#make entry year numeric
FPDS_data_w_topNAICS_topSB_minsigneddate$entryyear<-as.numeric(as.character(FPDS_data_w_topNAICS_topSB_minsigneddate$entryyear))

str(FPDS_data_w_topNAICS_topSB_minsigneddate$entryyear)

##drop unnecessary vars
FPDS_data_w_topNAICS_topSB_minsigneddate$obsv_period_MINsigndate <- NULL

#******
##Calculate final year
##create subsetted data with only FY, duns, obligated amount, and AnnualMaxOfSignedDate
Duns_maxsigndate <- data.frame(FPDS_data$FYear, FPDS_data$Dunsnumber, 
                               FPDS_data$obligatedAmount, 
                               FPDS_data$AnnualMaxOfSignedDate)

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

n_distinct(duns_and_maxsigndate$Dunsnumber) 

length(unique(duns_and_maxsigndate$Dunsnumber)) == nrow(duns_and_maxsigndate) ##TRUE

#drop obligatedamount and desc_rank
names(duns_and_maxsigndate)
duns_and_maxsigndate$obligated_amount <- duns_and_maxsigndate$desc_rank <- duns_and_maxsigndate$FYear <- NULL

#change name of 
names(duns_and_maxsigndate)[names(duns_and_maxsigndate) == "AnnualMaxSignedDate"] <- "obsv_period_maxsigndate"

#step 4: left join between FPDS_data and duns_and_NAICS

FPDS_data_w_topNAICS_topSB_minsigneddate_maxsigndate <- 
  join(FPDS_data_w_topNAICS_topSB_minsigneddate,
       duns_and_maxsigndate, by = c("Dunsnumber"), 
       type = "left", match = "all")

##change max sign date to final year
FPDS_data_w_topNAICS_topSB_minsigneddate_maxsigndate$obsv_period_maxsigndate<-as.Date(as.character(FPDS_data_w_topNAICS_topSB_minsigneddate_maxsigndate$obsv_period_maxsigndate))

str(FPDS_data_w_topNAICS_topSB_minsigneddate_maxsigndate$obsv_period_maxsigndate)

##create final year
FPDS_data_w_topNAICS_topSB_minsigneddate_maxsigndate <- FPDS_data_w_topNAICS_topSB_minsigneddate_maxsigndate %>%
  dplyr::mutate(finalyear = (format(obsv_period_maxsigndate, "%Y")))

str(FPDS_data_w_topNAICS_topSB_minsigneddate_maxsigndate$finalyear) ##it's a character

#make final year numeric
FPDS_data_w_topNAICS_topSB_minsigneddate_maxsigndate$finalyear<-as.numeric(as.character(FPDS_data_w_topNAICS_topSB_minsigneddate_maxsigndate$finalyear))

str(FPDS_data_w_topNAICS_topSB_minsigneddate_maxsigndate$finalyear)

FPDS_data_w_topNAICS_topSB_minsigneddate_maxsigndate$obsv_period_maxsigndate <- NULL

##now make graduate
##step 1: create variable that says whether or not a business was small in its first year of existence
##finding the one year where fy=entryyear & biz_size==1
base_yr_small_data <- FPDS_data_w_topNAICS_topSB_minsigneddate_maxsigndate %>% group_by(Dunsnumber) %>%
  dplyr::mutate(baseyrsmall = max(ifelse(biz_size==1 & FYear==entryyear, 1, 0), na.rm=TRUE))


base_yr_small_data <- base_yr_small_data %>% group_by(Dunsnumber) %>%
  dplyr::mutate(finalyr_smalldollars = sum(ifelse(biz_size==1 & FYear==finalyear, obligatedAmount, NA), na.rm=TRUE),
                finalyr_lgdollars = sum(ifelse(biz_size==0 & FYear==finalyear, obligatedAmount, NA), na.rm=TRUE))

base_yr_small_data <- base_yr_small_data %>% group_by(Dunsnumber) %>%
  dplyr::mutate(grad = ifelse(abs(finalyr_lgdollars)>(abs(finalyr_smalldollars) & baseyrsmall==0), 1, 0))

##collapse on graduated so that duns is a unique identifier
base_yr_small_data <- base_yr_small_data %>% group_by(Dunsnumber) %>%
  dplyr::summarize(grad_unique = sum(grad, na.rm=TRUE))

length(unique(base_yr_small_data$Dunsnumber)) == nrow(base_yr_small_data)

base_yr_small_data <- base_yr_small_data %>% group_by(Dunsnumber) %>%
  dplyr::mutate(graduated = ifelse(grad_unique==0, 0, 1))

##MERGE
#drop unnecessary vars

base_yr_small_data$grad_unique <- NULL

#Join
FPDS_data_w_topNAICS_topSB <- join(FPDS_data_w_topNAICS_topSB, base_yr_small_data, by = "Dunsnumber", type = "left", match = "all")


##*****************************************##
############create the total obligated amount TIME PERIOD#################
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

n_distinct(DO_newvar_totalobligations$FPDS_data.Dunsnumber) ##returns 664767 

length(unique(DO_newvar_totalobligations$FPDS_data.Dunsnumber)) == nrow(DO_newvar_totalobligations) ##TRUE

##is unique identifier!

#change name of duns in duns_and_NAICS
names(DO_newvar_totalobligations)
names(FPDS_data)

names(DO_newvar_totalobligations)[names(DO_newvar_totalobligations) == "FPDS_data.Dunsnumber"] <- "Dunsnumber"

#step 4: left join between FPDS_data and duns_and_NAICS

FPDS_data_w_topNAICS_topSB_totalobl <- join(FPDS_data_w_topNAICS_topSB, DO_newvar_totalobligations, by = "Dunsnumber", type = "left", match = "all")


##*****************************************##
############create the total obligated amount YR#################
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


# ##*****************************************##
# ############create the total # of actions#################
# #associated with each DUNS number over the entire time period
# #*******************************************#
# 
# ##count number of NAs in number of actions
# sum(is.na(FPDS_data$numberOfActions)) ##38223
# 
# ##create subsetted data with only FY, duns, number of actions
# names(FPDS_data)
# Duns_total_actions <- data.frame(FPDS_data$FYear, FPDS_data$Dunsnumber, FPDS_data$numberOfActions)
# 
# 
# sum(is.na(Duns_total_actions$FPDS_data.numberOfActions)) ##38223
# 
# ##sort by duns
# names(Duns_total_actions)
# Duns_total_actions <- Duns_total_actions[order(Duns_total_actions$FPDS_data.Dunsnumber, Duns_total_actions$FPDS_data.FYear), ]
# 
# 
# ##step 1: use dplyr to create a new data frame grouped by DUNS and obligated amount
# #and sum number of actions for all unique combinations
# DO_newvar_numberactions <- Duns_total_actions %>% group_by(FPDS_data.Dunsnumber) %>%
#   dplyr::summarize(total_actions=sum(FPDS_data.numberOfActions, na.rm=TRUE))
# 
# #check to see if FPDS_data.Dunsnumber is a unique identifier
# ##check uniqueness of DUNS as a variable##
# 
# n_distinct(DO_newvar_numberactions$FPDS_data.Dunsnumber) ##returns 8733 
# 
# length(unique(DO_newvar_numberactions$FPDS_data.Dunsnumber)) == nrow(DO_newvar_numberactions) ##TRUE
# 
# ##is unique identifier!
# 
# #change name of duns in DO_newvar_numberactions
# names(DO_newvar_totalobligations)
# names(FPDS_data)
# 
# names(DO_newvar_numberactions)[names(DO_newvar_numberactions) == "FPDS_data.Dunsnumber"] <- "Dunsnumber"
# 
# #step 4: left join between FPDS_data and duns_and_NAICS
# 
# FPDS_data_w_topNAICS_topSB_totalobl_FYobl_totact <- join(FPDS_data_w_topNAICS_topSB_totalobl_FYobl, DO_newvar_numberactions, by = "Dunsnumber", type = "left", match = "all")


# ##*****************************************##
# ############Create the total # of actions#################
# #associated with each DUNS number in each year
# #*******************************************#
# 
# ##create subsetted data with only FY, duns, totalactions
# names(FPDS_data)
# Duns_FYactions <- data.frame( FPDS_data$FYear, FPDS_data$Dunsnumber, FPDS_data$numberOfActions)
# 
# 
# ##sort by duns and FY
# names(Duns_FYactions)
# Duns_FYactions <- Duns_FYactions[order(Duns_FYactions$FPDS_data.Dunsnumber, Duns_FYactions$FPDS_data.FYear), ]
# 
# 
# ##step 1: use dplyr to create a new data frame grouped by DUNS and FY
# #and sum obligated amount for all unique combinations
# DO_newvar_FYactions <- Duns_FYactions %>% group_by(FPDS_data.FYear, FPDS_data.Dunsnumber) %>%
#   dplyr::summarize(FY_numberofactions=sum(FPDS_data.numberOfActions, na.rm=TRUE))
# 
# 
# 
# #change name of duns in duns_and_NAICS
# names(DO_newvar_FYactions)
# names(FPDS_data)
# 
# names(DO_newvar_FYactions)[names(DO_newvar_FYactions) == "FPDS_data.Dunsnumber"] <- "Dunsnumber"
# names(DO_newvar_FYactions)[names(DO_newvar_FYactions) == "FPDS_data.FYear"] <- "FYear"
# 
# 
# #step 4: left join between FPDS_data and duns_and_NAICS
# 
# FPDS_data_w_topNAICS_topSB_totalobl_FYobl_totact_FYact <- join(FPDS_data_w_topNAICS_topSB_totalobl_FYobl_totact, DO_newvar_FYactions, by = c("FYear", "Dunsnumber"), type = "left", match = "all")

#*************************************************
##*****************************************##
############Choose max sign date #################
#for each duns number
#*******************************************#

#GSS: Little bit of cleanup
str(FPDS_data)
FPDS_data$AnnualMaxOfSignedDate_date<-as.Date(as.character(FPDS_data$AnnualMaxOfSignedDate))

#GSS: Rather than ranking and then cutting down, it's possible to get both the min and max in a single step
Duns_signdate<-FPDS_data %>% group_by(Dunsnumber) %>%
  dplyr::summarise(
    min_FYear = min(FYear,na.rm=TRUE),
    max_FYear = max(FYear,na.rm=TRUE),
    min_AnnualMaxOfSignedDate = min(AnnualMaxOfSignedDate_date,na.rm=TRUE),
    max_FYear = max(FYear,na.rm=TRUE),
    max_AnnualMaxOfSignedDate = max(AnnualMaxOfSignedDate_date,na.rm=TRUE)
  )
Duns_signdate<-Duns_signdate %>% mutate(min_CYear=year(min_AnnualMaxOfSignedDate),
                                        max_CYear=year(max_AnnualMaxOfSignedDate))

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
names(duns_and_maxsigndate)
duns_and_maxsigndate$obligated_amount <- duns_and_maxsigndate$desc_rank <- duns_and_maxsigndate$FYear <- NULL

#change name of 
names(duns_and_maxsigndate)[names(duns_and_maxsigndate) == "AnnualMaxSignedDate"] <- "obsv_period_maxsigndate"

#step 4: left join between FPDS_data and duns_and_NAICS

#FPDS_data_w_topNAICS_topSB_totalobl_FYobl_totact_FYact_maxSD <- join(FPDS_data_w_topNAICS_topSB_totalobl_FYobl_totact_FYact, duns_and_maxsigndate, by = c("Dunsnumber"), type = "left", match = "all")

FPDS_data_w_topNAICS_topSB_totalobl_FYobl_maxSD <- join(FPDS_data_w_topNAICS_topSB_totalobl_FYobl, duns_and_maxsigndate, by = c("Dunsnumber"), type = "left", match = "all")

#FPDS_data_cleaned <- FPDS_data_w_topNAICS_topSB_totalobl_FYobl_totact_FYact_maxSD

##*****************************************##
############Choose min sign date #################
#for each duns number
#*******************************************#

Duns_minsigndate <- data.frame(FPDS_data$FYear, FPDS_data$Dunsnumber, FPDS_data$obligatedAmount, FPDS_data$AnnualMaxOfSignedDate)

names(Duns_minsigndate)[names(Duns_minsigndate) == "FPDS_data.Dunsnumber"] <- "Dunsnumber"
names(Duns_minsigndate)[names(Duns_minsigndate) == "FPDS_data.FYear"] <- "FYear"
names(Duns_minsigndate)[names(Duns_minsigndate) == "FPDS_data.obligatedAmount"] <- "obligated_amount"
names(Duns_minsigndate)[names(Duns_minsigndate) == "FPDS_data.AnnualMaxOfSignedDate"] <- "AnnualMaxSignedDate"


##sort by duns
names(Duns_minsigndate)
Duns_minsigndate <- Duns_minsigndate[order(Duns_minsigndate$Dunsnumber, Duns_minsigndate$FYear, Duns_minsigndate$AnnualMaxSignedDate), ]

##create a variable that ranks max signed date, 1 being the lowest signed date
DO_newvar_minsigndate <- Duns_minsigndate %>% group_by(Dunsnumber) %>%
  dplyr::mutate(asc_rank = row_number(AnnualMaxSignedDate)) 

##subset DO_max_newvar where rank==1

duns_and_minsigndate <- subset(DO_newvar_minsigndate, asc_rank==1)


#check to see if FPDS_data.Dunsnumber is a unique identifier
##check uniqueness of DUNS as a variable##

n_distinct(duns_and_minsigndate$Dunsnumber) ##returns 8733 when should return 8764

length(unique(duns_and_minsigndate$Dunsnumber)) == nrow(duns_and_minsigndate) ##TRUE

#drop obligatedamount and desc_rank

duns_and_minsigndate$obligated_amount <- duns_and_minsigndate$asc_rank <- duns_and_minsigndate$FYear <- NULL

#change name of 
names(duns_and_minsigndate)[names(duns_and_minsigndate) == "AnnualMaxSignedDate"] <- "obsv_period_MINsigndate"


#GSS: Let's do a annual count of dunsnumbers based on the min of calendar year derived fromr signed date here and now
duns_and_minsigndate$CYear<-year(as.Date(as.character(duns_and_minsigndate$obsv_period_MINsigndate)))
duns_and_minsigndate%>% group_by(CYear)%>%
  dplyr::summarise(
    DunsCount=length(Dunsnumber)
  )


#step 4: left join between FPDS_data and duns_and_NAICS

#FPDS_data_w_topNAICS_topSB_totalobl_FYobl_totact_FYact_maxSD_minSD <- join(FPDS_data_w_topNAICS_topSB_totalobl_FYobl_totact_FYact_maxSD, duns_and_minsigndate, by = c("Dunsnumber"), type = "left", match = "all")

#***************************************************************************
# #BELOW (SAVE SAVE AND LOAD LOAD) ONLY NECESSARY IF YOU RUN OUT OF SPACE AND GET AN ERROR BC OF THAT
# #!!!!!!!!!!!!!!!!!!clear global environment
# save(FPDS_data_w_topNAICS_topSB_totalobl_FYobl_maxSD, file="intermediary_leftof_join.Rda")
# 
# save(duns_and_minsigndate, file="intermediary_rightof_join.Rda")
# 
# load(file = "intermediary_leftof_join.Rda")
# load(file = "intermediary_rightof_join.Rda")
# ##******************************************************


FPDS_data_w_topNAICS_topSB_totalobl_FYobl_maxSD_minSD <- join(FPDS_data_w_topNAICS_topSB_totalobl_FYobl_maxSD, duns_and_minsigndate, by = c("Dunsnumber"), type = "left", match = "all")



#GSS: Let's do a annual count of dunsnumbers based on the min of calendar year derived fromr signed date here and now
FPDS_data_w_topNAICS_topSB_totalobl_FYobl_maxSD_minSD%>% group_by(CYear)%>%
  dplyr::summarise(
    DunsCount=length(unique(Dunsnumber))
  )


#FPDS_data_cleaned <- FPDS_data_w_topNAICS_topSB_totalobl_FYobl_totact_FYact_maxSD_minSD

FPDS_data_cleaned <- FPDS_data_w_topNAICS_topSB_totalobl_FYobl_maxSD_minSD

# ##*****************************************##
# ############Choose navy category depending#################
# #on whether it has a navy account
# #*******************************************#
# 
# 
# ##create subsetted data with only duns, and subcustomer
# 
# FPDS_data_subcust <- data.frame(FPDS_data_cleaned$Dunsnumber, FPDS_data_cleaned$subcustomer)
# 
# ##step 1: use dplyr to create a new data frame grouped by
# #DUNS and subcustomer from FPDS_data_cleaned
# 
# names(FPDS_data_subcust)
# 
# FPDS_data_subcust_navy <- FPDS_data_subcust %>% group_by(FPDS_data_cleaned.Dunsnumber) %>%
#   dplyr::mutate(navy_count = ifelse(FPDS_data_cleaned.subcustomer=="Navy", "1", "0"))
# 
# ##sortby duns
# 
# FPDS_data_subcust_navy <- FPDS_data_subcust_navy[order(FPDS_data_subcust_navy$FPDS_data_cleaned.Dunsnumber), ]
# 
# 
# str(FPDS_data_subcust_navy$navy_count)
# 
# FPDS_data_subcust_navy$navy_count<-as.numeric(as.character(FPDS_data_subcust_navy$navy_count))
# 
# 
# ##step 2: create a variable navy_cust that equals the max of navy_count for each unique dunsnumber
# 
# FPDS_data_subcust_navy_count <- FPDS_data_subcust_navy %>% group_by(FPDS_data_cleaned.Dunsnumber) %>%
#   dplyr::summarize(navy_cust=max(navy_count, na.rm = TRUE))
# 
# str(FPDS_data_subcust_navy_count$navy_cust)
# 
# names(FPDS_data_subcust_navy_count)[names(FPDS_data_subcust_navy_count) == "FPDS_data_cleaned.Dunsnumber"] <- "Dunsnumber"
# 
# 
# FPDS_data_cleaned_ <- join(FPDS_data_cleaned, FPDS_data_subcust_navy_count, by = c("Dunsnumber"), type = "left", match = "all")
# 


#*********************************************************************
####To calculate the number of new entrants that entered each year####
#*********************************************************************
# 
# #FOR WITH SERVICES
# #!!!!!!!!!!!!!!remove duplicate duns numbers here!
# ##collapse by duns number so only have unique duns numbers
# FPDS_cleaned_unique <- FPDS_data_cleaned_[!duplicated(FPDS_data_cleaned$Dunsnumber), ]
# ##664767

#FOR WITHOUT SERVICES
FPDS_cleaned_unique <- FPDS_data_cleaned[!duplicated(FPDS_data_cleaned$Dunsnumber), ]

#GSS: Let's do a annual count of dunsnumbers based on the min of calendar year derived from signed date here and now
FPDS_cleaned_unique$CYear<-year(as.Date(as.character(FPDS_cleaned_unique$obsv_period_MINsigndate)))
FPDS_cleaned_unique%>% group_by(CYear)%>%
  dplyr::summarise(
    DunsCount=length(unique(Dunsnumber))
  )

#***************************************#
####registration year####
#***************************************#

str(FPDS_cleaned_unique$obsv_period_MINsigndate)

##make as date

FPDS_cleaned_unique$obsv_period_MINsigndate<-as.Date(as.character(FPDS_cleaned_unique$obsv_period_MINsigndate))

str(FPDS_cleaned_unique$obsv_period_MINsigndate)

##create registration year
FPDS_cleaned_unique <- FPDS_cleaned_unique %>%
  dplyr::mutate(registrationYear = (format(FPDS_cleaned_unique$obsv_period_MINsigndate, "%Y")))


#GSS: Let's do a annual count of dunsnumbers based on the min of calendar year derived from signed date here and now
FPDS_cleaned_unique%>% group_by(registrationYear)%>%
  dplyr::summarise(
    DunsCount=length(unique(Dunsnumber))
  )

FPDS_cleaned_unique$CYear<-year(as.Date(as.character(FPDS_cleaned_unique$obsv_period_MINsigndate)))
FPDS_cleaned_unique%>% group_by(CYear)%>%
  dplyr::summarise(
    DunsCount=length(unique(Dunsnumber))
  )



str(FPDS_cleaned_unique$registrationYear) ##it's a character

#make registrationYear numeric
FPDS_cleaned_unique$registrationYear<-as.numeric(as.character(FPDS_cleaned_unique$registrationYear))

str(FPDS_cleaned_unique$registrationYear)


#*************************************#
####exit year#####
#*************************************#
str(FPDS_cleaned_unique$obsv_period_maxsigndate)

##make as date

FPDS_cleaned_unique$obsv_period_maxsigndate<-as.Date(as.character(FPDS_cleaned_unique$obsv_period_maxsigndate))

str(FPDS_cleaned_unique$obsv_period_maxsigndate)

##create registration year
FPDS_cleaned_unique <- FPDS_cleaned_unique %>%
  dplyr::mutate(exitYear = (format(FPDS_cleaned_unique$obsv_period_maxsigndate, "%Y")))

str(FPDS_cleaned_unique$registrationYear) ##it's a character

#make registrationYear numeric
FPDS_cleaned_unique$registrationYear<-as.numeric(as.character(FPDS_cleaned_unique$registrationYear))

str(FPDS_cleaned_unique$registrationYear)

##********************************##
####****Save File*********#####
##********************************##

##drop entries in FY before 2000

FPDS_cleaned_unique <- FPDS_cleaned_unique[!(FPDS_cleaned_unique$registrationYear<2000), ]

#FOR SERVICES CLEANING
#save(FPDS_cleaned_unique, file="FPDS_datapull_all_v3.Rda")

##on 9/18 saving a NEW FILE bc this one above ("FPDS_datapull_allv3.Rda" is now only defense

#FOR ALL FED AGENCIES CLEANING
save(FPDS_cleaned_unique, file="FPDS_datapull_all_v3_allfed.Rda")



#******************************************************



















# 
# #***********************************************************************#
# 
# #*****************************************#
# ####Whether a firm is an incumbent firm####
# #*****************************************#
# 
# ##because of space  concerns, empty the environment and then reload
# #FPDS_cleaned_unique and FPDS_data
# 
# setwd("K:/2018-01 NPS New Entrants/Data/Data/Raw Data/FPDS")
# FPDS_data <- read.delim("Vendor.SP_DunsnumberNewEntrants_all.txt", fill = TRUE, header=TRUE,  na.strings = c("", "NULL"))
# 
# 
# setwd("K:/2018-01 NPS New Entrants/Data/Data/Cleaning data/FPDS")
# load(file = "FPDS_datapull_all_v3.Rda")
# 
# 
# ##step 1: in FPDS_data, create a variable that gives total obligations in each year
# 
# #create a dataframe to edit
# FPDS_data_intermediate <- FPDS_data
# 
# names(FPDS_data_intermediate)[names(FPDS_data_intermediate) == "ï..fiscal_year"] <- "FYear"
# 
# 
# ##count number of NAs in obligated amount
# sum(is.na(FPDS_data_intermediate$obligatedAmount)) ##1266 
# 
# ##create subsetted data with only FY, duns, obligated amount
# names(FPDS_data_intermediate)
# FPDS_data_intermediate_obligatedamnt <- data.frame(FPDS_data_intermediate$FYear, FPDS_data_intermediate$Dunsnumber, FPDS_data_intermediate$obligatedAmount)
# 
# names(FPDS_data_intermediate_obligatedamnt)
# names(FPDS_data_intermediate_obligatedamnt)[names(FPDS_data_intermediate_obligatedamnt) == "FPDS_data_intermediate.FYear"] <- "FYear"
# names(FPDS_data_intermediate_obligatedamnt)[names(FPDS_data_intermediate_obligatedamnt) == "FPDS_data_intermediate.Dunsnumber"] <- "Dunsnumber"
# names(FPDS_data_intermediate_obligatedamnt)[names(FPDS_data_intermediate_obligatedamnt) == "FPDS_data_intermediate.obligatedAmount"] <- "obligatedAmount"
# 
# 
# ##count how many rows each Dunsnumber has
# names(FPDS_data_intermediate_obligatedamnt)
# unique_FPDS_data_intermediate_obligatedamnt <- data.frame(table(FPDS_data_intermediate_obligatedamnt$Dunsnumber)) ##gives a data frame with a list of duns and the number of times they occurred
# #810382 unique duns numbers
# 
# ##sort by duns
# names(FPDS_data_intermediate_obligatedamnt)
# FPDS_data_intermediate_obligatedamnt <- FPDS_data_intermediate_obligatedamnt[order(FPDS_data_intermediate_obligatedamnt$FYear, FPDS_data_intermediate_obligatedamnt$Dunsnumber), ]
# 
# #remove years before 2001
# FPDS_data_intermediate_obligatedamnt <- FPDS_data_intermediate_obligatedamnt[!(FPDS_data_intermediate_obligatedamnt$FYear<2001), ]
# 
# ##step 1: use dplyr to create a new data frame grouped by fiscal year 
# #and sum obligated amount for all unique combinations
# FPDS_newvar_totalobligations <- FPDS_data_intermediate_obligatedamnt %>% group_by(FYear) %>%
#   dplyr::summarize(total_obligations_allvendors = sum(obligatedAmount, na.rm=TRUE))
# 
# #step 2: left join between FPDS_newvar_totalobligations and FPDS_cleaned_unique
# 
# FPDS_cleaned_unique_wtotalobligations <- join(FPDS_cleaned_unique, FPDS_newvar_totalobligations, by = "FYear", type = "left", match = "all")
# 
# ##drop years before 2001
# FPDS_cleaned_unique_wtotalobligations <- FPDS_cleaned_unique_wtotalobligations[!(FPDS_cleaned_unique_wtotalobligations$registrationYear<2001), ]
# 
# ##step 3: create new entrants total obligations variable
# 
# count_total_obligations_newentrants <- FPDS_cleaned_unique_wtotalobligations %>%
#   group_by(FYear) %>%
#   dplyr::summarise(sum_obligations_newentrants = sum(total_obligations, na.rm = TRUE))
# 
# ##step 4: subset the FPDS_cleaned_unique_wtotalobligations data so it's just each year and totalobligations_allvendors
# 
# count_total_obligations_allvendors <- FPDS_cleaned_unique_wtotalobligations %>%
#   group_by(FYear) %>%
#   dplyr::summarise(sum_obligation_allvendors = min(total_obligations_allvendors))
# 
# 
# ##step 5: merge the dataframe from step 4 to count_total_obligations_newentrants
# 
# count_totalobl_allvend_and_NE <- join(count_total_obligations_newentrants, count_total_obligations_allvendors, by = "FYear", type = "left", match = "all")
# 
# ##step 6: make a new variable of totalobligations_incumbentfirsms by 
# #subtracting the total_obligations_newentrants from total_obligations_allvendors in each year
# 
# count_totalobl_allvend_and_NE <- count_totalobl_allvend_and_NE %>% group_by(FYear) %>%
#   dplyr::mutate(sum_obligations_incumbents = sum_obligation_allvendors - sum_obligations_newentrants)
# 
# ##step 7: merge data to FPDS_unique
# 
# FPDS_cleaned_unique_wtotalobligations_allvend_NE <- join(FPDS_cleaned_unique_wtotalobligations, count_totalobl_allvend_and_NE, by = "FYear", type = "left", match = "all")
# 
# ##step : save data
# 
# save(FPDS_cleaned_unique_wtotalobligations_allvend_NE, file="FPDS_cleaned_unique_wtotalobligations.Rda")
# 
# #**********************************************************
# 
# 
# # ##make Small business numeric##
# # 
# # str(FPDS_cleaned_unique$top_small_biz)
# # 
# # table(FPDS_cleaned_unique$top_small_biz)
# # 
# # FPDS_cleaned_unique <- FPDS_cleaned_unique[complete.cases(FPDS_cleaned_unique$top_small_biz), ]
# # FPDS_cleaned_unique <- FPDS_cleaned_unique[!(FPDS_cleaned_unique$top_small_biz==":"), ]
# # 
# # table(FPDS_cleaned_unique$top_small_biz)
# # 
# # ##make top biz_size binary
# # FPDS_cleaned_unique$top_smallbiz_bin <- revalue(FPDS_cleaned_unique$top_small_biz, c("S"="1", "O"="0"))
# # 
# # str(FPDS_cleaned_unique$top_smallbiz_bin)
# # 
# # table(FPDS_cleaned_unique$top_smallbiz_bin)
# # 
# # 
# # ##change biz_size to binary
# # str(FPDS_cleaned_unique$biz_size)
# # 
# # table(FPDS_cleaned_unique$biz_size)
# # 
# # FPDS_cleaned_unique <- FPDS_cleaned_unique[complete.cases(FPDS_cleaned_unique$biz_size), ]
# # FPDS_cleaned_unique <- FPDS_cleaned_unique[!(FPDS_cleaned_unique$biz_size==":"), ]
# # 
# # table(FPDS_cleaned_unique$biz_size)
# # 
# # ##make biz_size binary
# # FPDS_cleaned_unique$biz_size <- revalue(FPDS_cleaned_unique$biz_size, c("S"="1", "O"="0"))
# # 
# # str(FPDS_cleaned_unique$biz_size)
# # 
# # table(FPDS_cleaned_unique$biz_size)
# # 
# # save final
# # save(FPDS_cleaned_unique, file="FPDS_datapull_all_v3.Rda")
# # 
# # #***************************************************************************#
# registrationyear_count <- table(FPDS_cleaned_unique$registrationYear)
# 
# registrationyear_count
# 
# exitYear_count <- table(FPDS_cleaned_unique$exitYear)
# exitYear_count
# 
# Duns_signdate$Dunsnumber
# Duns_signdate%>% group_by(min_CYear)%>%
#   dplyr::summarise(
#   DunsCount=length(Dunsnumber)
#   )
# 
# Duns_signdate%>% group_by(max_CYear)%>%
#   dplyr::summarise(
#     DunsCount=length(Dunsnumber)
#   )

