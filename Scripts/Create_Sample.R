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
contract_transform_verify(def,dollars_suffix="Then_Year")


# load(file="Data/Clean//def_sample.Rdata")

# summary(def_breach$n_CBre_Then_Year)
# def_breach$n_CBre_Then_Year<-def_breach$n_CBre_Then_Year-1
# summary(def_breach$n_CBre_Then_Year)
# summary(def_breach$ln_CBre_Then_Year)
# def_breach$ln_CBre_Then_Year<-na_non_positive_log(def_breach$n_CBre_Then_Year)
# summary(def_breach$ln_CBre_Then_Year)
# def_breach<-def_breach %>% dplyr::select(-n_CBre_OMB20_GDP18,-ln_CBre_OMB20_GDP18)


#Output variables
summary(def$b_Term)
summary(def$b_CBre)
summary(def$ln_CBre_Then_Year)
summary(def$p_CBre)
#Study Variables
summary(def$CompOffr)
summary(def$cl_def3_HHI_lag1) 
summary(def$cl_def6_HHI_lag1) 
#Controls
summary(def$cl_Ceil_Then_Year)
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
  # !is.na(def$ln_CBre_Then_Year)& NA when 0, but that's okay.r
  #Study Variables
  !is.na(def$CompOffr)&
  !is.na(def$cl_def3_HHI_lag1)&
  !is.na(def$cl_def6_HHI_lag1)&
  #Controls
  !is.na(def$cl_Ceil_Then_Year)&
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
  !is.na(def$cl_def6_obl_lag1)&
  !is.na(def$cl_def6_ratio_lag1)&
  !is.na(def$cl_US6_avg_sal_lag1)



summary(complete)
summary(def$Action_Obligation_Then_Year)
money<-def$Action_Obligation_Then_Year
any(def$Action_Obligation_Then_Year<0)
money[def$Action_Obligation_Then_Year<0]<-0
sum(def$Action_Obligation_Then_Year[def$Action_Obligation_Then_Year<0])








#Missing data, how many records and how much money
length(money[!complete])/length(money)
sum(money[!complete],na.rm=TRUE)/sum(money,na.rm=TRUE)

if(file.exists("data/clean/def_sample.Rdata")) load(file="data/cleaen/defe_smple.Rdata")
else{
  smp_complete<-def[complete,]
  # #
  smp1m<-smp_complete[sample(nrow(smp_complete),1000000),]
  smp<-smp_complete[sample(nrow(smp_complete),250000),]
  def_breach<-smp_complete[def$b_CBre==1,]
  rm(smp_complete)
}

#To instead replace entries in existing sample, use  this code.
#Missing colnames in sample from Def
# colnames(smp)[!colnames(smp) %in% colnames(def)]
# colnames(smp1m)[!colnames(smp1m) %in% colnames(def)]
# colnames(def)[!colnames(def) %in% colnames(smp)]
# 
# nrow(smp[smp$CSIScontractID %in% def$CSIScontractID[complete],])
# nrow(smp[!smp$CSIScontractID %in% def$CSIScontractID[complete],])


#  #         
# # #Sample adjustments
# # colnames(smp)[colnames(smp)=="Action.Obligation"]<-"Action_Obligation_Then_Year"
# # colnames(smp1m)[colnames(smp1m)=="Action.Obligation"]<-"Action_Obligation_Then_Year"
# memory.limit(36000)
# smp<-smp[,colnames(smp)=="CSIScontractID"]
# # smp<-update_sample_col_CSIScontractID(smp,def[complete,],drop_and_replace=TRUE)
# smp1m<-smp1m[,colnames(smp1m)=="CSIScontractID"]
# # smp1m<-update_sample_col_CSIScontractID(smp1m,def[complete,],drop_and_replace=TRUE)

# smp<-update_sample_col_CSIScontractID(smp,def[complete,],drop_and_replace=FALSE,col="cln_Base_Then_Year")
# 
# smp<-update_sample_col_CSIScontractID(smp,def,drop_and_replace=FALSE,col="cln_Base_Then_Year")
# smp1m<-update_sample_col_CSIScontractID(smp1m,def[complete,],drop_and_replace=FALSE,col="cln_Base_Then_Year")
# def_breach<-update_sample_col_CSIScontractID(def_breach,def[complete,],drop_and_replace=FALSE,col="cln_Base_Then_Year")


# debug(contract_transform_verify)
# smp<-smp%>%select(-cln_Base_Then_Year)

# verify_transform(smp,"UnmodifiedBase_Then_Year","cln_Base_Then_Year",just_check_na=TRUE)
# summary(smp$UnmodifiedBase_Then_Year)
# 
# summary(smp$UnmodifiedBase_Then_Year)
# summary(smp$cln_Base_Then_Year)
# 
# 
# 
# smp$CSIScontractID[is.na(smp$cln_Base_Then_Year)&!is.na(smp$UnmodifiedBase_Then_Year)]






# verify_transform(smp,"def3_ratio_lag1","cl_def3_ratio_lag1",just_check_na=TRUE,log=FALSE)
# verify_transform(smp,"def6_ratio_lag1","cl_def6_ratio_lag1",just_check_na=TRUE,log=FALSE)


contract_transform_verify(smp,dollars_suffix="Then_Year",just_check_na=TRUE)
contract_transform_verify(smp1m,dollars_suffix="Then_Year",just_check_na=TRUE)
contract_transform_verify(def_breach,dollars_suffix="Then_Year",just_check_na=TRUE)

save(file="data//clean//def_sample.Rdata",smp,smp1m,def_breach)
write.foreign(df=smp,
              datafile="Data//clean//def_sample250k.dat",
              codefile="Data//clean//def_sample250k_code.do",
              package = "Stata")
write.foreign(df=smp1m,
              datafile="Data//clean//def_sample1m.dat",
              codefile="Data//clean//def_sample1m_code.do",
              package = "Stata")
write.foreign(df=def_breach,
              datafile="Data//clean//def_breach.dat",
              codefile="Data//clean//def_breach_code.do",
              package = "Stata")
save(file="data//clean//def_sample.Rdata",smp,smp1m,def_breach)

