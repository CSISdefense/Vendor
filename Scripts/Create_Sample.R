# ---
# title: "Create Sample"
# output:
#   html_document:
#     keep_md: yes
#     toc: yes
# date: "Wednesday, July 11, 2019"
# ---

#Setup
library(csis360)
library(ggplot2)
library(dplyr)
library(arm)
library(R2WinBUGS)
library(knitr)
library(foreign)
library(stargazer)
library(texreg)
library(reshape2)
library(tidyverse)
source("https://raw.githubusercontent.com/CSISdefense/Vendor/master/Scripts/DIIGstat.r")

if(!exists("def")) load("data/clean/transformed_def.Rdata")
## Computational Sample Creation


# load(file="../Data/Clean//def_sample.Rdata")

#Output variables
summary(def$b_Term)
summary(def$b_CBre)
summary(def$n_CBre_Then_Year)
summary(def$p_CBre)
#Study Variables
summary(def$CompOffr)
summary(def$cl_def3_HHI_lag1)
summary(def$cl_def6_HHI_lag1)
#Controls
summary(def$cl_Ceil)
summary(def$cl_Days)
summary(def$Veh) 
summary(def$PricingFee)
summary(def$b_UCA)
summary(def$b_Intl)
summary(def$NAICS)
summary(def$NAICS3)
summary(def$Office)
summary(def$Agency)
summary(def$StartCY)
summary(def$cl_def3_ratio_lag1)
summary(def$cl_def6_obl_lag1)
summary(def$cl_def6_ratio_lag1)
summary(def$cl_US6_avg_sal_lag1)

#New Controls
complete<-
  #Dependent Variables
  !is.na(def$b_Term)& #summary(def$b_Term)
  !is.na(def$b_CBre)&
  #Study Variables
  !is.na(def$CompOffr)&
  !is.na(def$cl_def3_HHI_lag1)&
  !is.na(def$cl_def6_HHI_lag1)&
  #Controls
  !is.na(def$cl_Ceil)&
  !is.na(def$cl_Days)&
  !is.na(def$Veh) &
  !is.na(def$PricingFee)&
  !is.na(def$b_Intl)& 
  !is.na(def$b_UCA)& 
  !is.na(def$NAICS)&
  !is.na(def$NAICS3)&
  !is.na(def$Office)&
  !is.na(def$Agency)&
  !is.na(def$StartCY)&
  !is.na(def$cl_def3_ratio_lag1)&
  !is.na(def$cl_def6_obl_lag1Const)&
  !is.na(def$cl_def6_ratio_lag1)&
  !is.na(def$cl_US6_avg_sal_lag1)



summary(complete)
summary(def$Action_Obligation.OMB20_GDP18)
money<-def$Action_Obligation.OMB20_GDP18
any(def$Action_Obligation.OMB20_GDP18<0)
money[def$Action_Obligation.OMB20_GDP18<0]<-0
sum(def$Action_Obligation.OMB20_GDP18[def$Action_Obligation.OMB20_GDP18<0])

#Missing data, how many records and how much money
length(money[!complete])/length(money)
sum(money[!complete],na.rm=TRUE)/sum(money,na.rm=TRUE)

#What portion of contracts have potential options, 
sum(money[def$AnyUnmodifiedUnexercisedOptions==1],na.rm=TRUE)/
  sum(money,na.rm=TRUE)

#Missing data with potential options how much money?
length(money[!complete&def$AnyUnmodifiedUnexercisedOptions==1])/
  length(money[def$AnyUnmodifiedUnexercisedOptions==1])
sum(money[!complete&def$AnyUnmodifiedUnexercisedOptions==1],na.rm=TRUE)/
  sum(money[def$AnyUnmodifiedUnexercisedOptions==1],na.rm=TRUE)

#Refreshing
serv_complete<-def[complete,]
serv_smp1m<-serv_complete[sample(nrow(serv_complete),1000000),]
serv_smp<-serv_complete[sample(nrow(serv_complete),250000),]
rm(serv_complete)
serv_opt<-def[complete&def$AnyUnmodifiedUnexercisedOptions==1,]

#To instead replace entries in existing sample, use  this code.
# load(file="data/clean/def_sample.Rdata")
serv_smp<-update_sample_col_CSIScontractID(serv_smp,
                                           def[complete,],
                                           col=NULL,
                                           drop_and_replace=TRUE)

serv_smp1m<-update_sample_col_CSIScontractID(serv_smp1m,
                                           def[complete,],
                                           col=NULL,
                                           drop_and_replace=TRUE)

serv_opt<-update_sample_col_CSIScontractID(serv_opt,
                                             def[complete,],
                                             col=NULL,
                                             drop_and_replace=TRUE)

# serv_smp<-serv_smp %>% dplyr::select(-c(Ceil, qCRais))
# serv_smp1m<-serv_smp1m %>% dplyr::select(-c(Ceil, qCRais))
# serv_opt<-serv_opt %>% dplyr::select(-c(Ceil, qCRais))


save(file="data/clean/def_sample.Rdata",serv_smp,serv_smp1m,serv_opt)
write.foreign(df=serv_smp,
              datafile="data//clean//def_sample250k.dat",
              codefile="data//clean//def_sample250k_code.do",
              package = "Stata")
write.foreign(df=serv_smp1m,
              datafile="data//clean//def_sample1m.dat",
              codefile="data//clean//def_sample1m_code.do",
              package = "Stata")
