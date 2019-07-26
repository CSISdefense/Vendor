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

if(!exists("def_serv")) load("data/clean/transformed_def_serv.Rdata")
## Computational Sample Creation
summary(def_serv$UnmodifiedContractBaseAndAllOptionsValue.Then.Year)
summary(def_serv$UnmodifiedContractBaseAndExercisedOptionsValue)









def_serv$Base2Ceil<-def_serv$UnmodifiedContractBaseAndAllOptionsValue.Then.Year/def_serv$UnmodifiedContractBaseAndExercisedOptionsValue
def_serv$Base2Ceil[def_serv$Base2Ceil<1 | !is.finite(def_serv$Base2Ceil)]<-NA
summary(def_serv$Base2Ceil)
def_serv$cl_Base2Ceil<-arm::rescale(log(def_serv$Base2Ceil))
summary(def_serv$cl_Base2Ceil)
ggplot(def_serv %>% filter(Base2Ceil>1 & Base2Ceil<10),aes(x=Base2Ceil))+geom_histogram(bins=30)
ggplot(def_serv %>% filter(Base2Ceil>1),aes(x=Base2Ceil))+geom_histogram(bins=30)+scale_x_log10()
ggplot(def_serv %>% filter(Base2Ceil>1),aes(x=Base2Ceil))+geom_histogram(bins=50)+scale_x_log10()
# def_serv$cp_AvlOpt<-arm::rescale(def_serv$p_AvlOpt)
# def_serv <-def_serv %>% dplyr::select(-AvlOpt,-p_AvlOpt,-cp_AvlOpt)



def_serv$p_CBre<-(def_serv$ChangeOrderBaseAndAllOptionsValue/
                    def_serv$UnmodifiedContractBaseAndAllOptionsValue.Then.Year)+1
def_serv$p_CBre[
  is.na(def_serv$p_CBre) & def_serv$b_CBre==0]<-1


#Output variables
summary(def_serv$b_Term)
summary(def_serv$b_CBre)
summary(def_serv$lp_OptGrowth) #Missing
summary(def_serv$ExercisedOptions)
summary(def_serv$AnyUnmodifiedUnexercisedOptions)
#Study Variables
summary(def_serv$cl_US6_avg_sal_lag1Const)
summary(def_serv$cl_CFTE)
summary(def_serv$c_pPBSC)
summary(def_serv$c_pOffPSC)
summary(def_serv$c_pairHist)
summary(def_serv$cl_pairCA)
#Controls
summary(def_serv$CompOffr)
summary(def_serv$cl_Offr)
summary(def_serv$cl_Ceil)
summary(def_serv$capped_cl_Days)
summary(def_serv$Veh) 
summary(def_serv$PricingUCA)
summary(def_serv$PlaceCountryISO3)
summary(def_serv$NAICS)
summary(def_serv$NAICS3)
summary(def_serv$Office)
summary(def_serv$Agency)
summary(def_serv$StartCY)
summary(def_serv$cl_def3_HHI_lag1)#Missing!
summary(def_serv$cl_def3_ratio_lag1)#Missing!
summary(def_serv$cl_def6_HHI_lag1)#Missing!
summary(def_serv$cl_def6_obl_lag1)#Missing!
summary(def_serv$cl_def6_ratio_lag1)#Missing!
summary(def_serv$cl_US6_avg_sal_lag1)#Missing!
#New Controls
summary(def_serv$cl_OffCA)
summary(def_serv$cl_OffCA)
summary(def_serv$c_pMarket)
summary(def_serv$Crisis)
summary(def_serv$cl_office_naics_hhi_k)
summary(def_serv$cl_office_naics_hhi_obl)

complete<-
  #Dependent Variables
  !is.na(def_serv$b_Term)& #summary(def_serv$b_Term)
  !is.na(def_serv$b_CBre)&
  !is.na(def_serv$lp_OptGrowth)&
  !is.na(def_serv$ExercisedOptions)&
  !is.na(def_serv$AnyUnmodifiedUnexercisedOptions)&
  #Study Variables
  !is.na(def_serv$cl_US6_avg_sal_lag1Const)&
  !is.na(def_serv$cl_CFTE)&
  !is.na(def_serv$c_pPBSC)&
  !is.na(def_serv$c_pOffPSC)&
  !is.na(def_serv$c_pairHist)&
  !is.na(def_serv$cl_pairCA)&
  #Controls
  !is.na(def_serv$CompOffr)&
  !is.na(def_serv$cl_Offr)&
  !is.na(def_serv$cl_Ceil)&
  !is.na(def_serv$capped_cl_Days)&
  !is.na(def_serv$Veh) &
  !is.na(def_serv$PricingUCA)&
  !is.na(def_serv$PlaceCountryISO3)& #New Variable
  # !is.na(def_serv$b_UCA)& No longer  used
  !is.na(def_serv$NAICS)&
  !is.na(def_serv$NAICS3)&
  !is.na(def_serv$Office)&
  !is.na(def_serv$Agency)&
  !is.na(def_serv$StartCY)&
  !is.na(def_serv$cl_def3_HHI_lag1)&
  !is.na(def_serv$cl_def3_ratio_lag1)&
  !is.na(def_serv$cl_def6_HHI_lag1)&
  !is.na(def_serv$cl_def6_obl_lag1Const)&
  !is.na(def_serv$cl_def6_ratio_lag1)&
  #New Controls
  !is.na(def_serv$cl_OffCA)& #summary(def_serv$cl_OffCA)
  !is.na(def_serv$cl_OffVol)& #summary(def_serv$cl_OffVol)
  !is.na(def_serv$c_pMarket)&  #summary(def_serv$c_pMarket)
  !is.na(def_serv$Crisis)&  #summary(def_serv$c_pMarket)
  !is.na(def_serv$cl_office_naics_hhi_k)
!is.na(def_serv$cl_Base2Ceil)


summary(complete)
summary(def_serv$Action_Obligation.OMB20_GDP18)
money<-def_serv$Action_Obligation.OMB20_GDP18
any(def_serv$Action_Obligation.OMB20_GDP18<0)
money[def_serv$Action_Obligation.OMB20_GDP18<0]<-0
sum(def_serv$Action_Obligation.OMB20_GDP18[def_serv$Action_Obligation.OMB20_GDP18<0])

#Missing data, how many records and how much money
length(money[!complete])/length(money)
sum(money[!complete],na.rm=TRUE)/sum(money,na.rm=TRUE)

#What portion of contracts have potential options, 
sum(money[def_serv$AnyUnmodifiedUnexercisedOptions==1],na.rm=TRUE)/
  sum(money,na.rm=TRUE)

#Missing data with potential options how much money?
length(money[!complete&def_serv$AnyUnmodifiedUnexercisedOptions==1])/
  length(money[def_serv$AnyUnmodifiedUnexercisedOptions==1])
sum(money[!complete&def_serv$AnyUnmodifiedUnexercisedOptions==1],na.rm=TRUE)/
  sum(money[def_serv$AnyUnmodifiedUnexercisedOptions==1],na.rm=TRUE)

#Refreshing
serv_complete<-def_serv[complete,]
serv_smp1m<-serv_complete[sample(nrow(serv_complete),1000000),]
serv_smp<-serv_complete[sample(nrow(serv_complete),250000),]
rm(serv_complete)
serv_opt<-def_serv[complete&def_serv$AnyUnmodifiedUnexercisedOptions==1,]

#To instead replace entries in existing sample, use  this code.
# load(file="data/clean/def_sample.Rdata")
serv_smp<-update_sample_col_CSIScontractID(serv_smp,
                                           def_serv[complete,],
                                           col=NULL,
                                           drop_and_replace=TRUE)

serv_smp1m<-update_sample_col_CSIScontractID(serv_smp1m,
                                           def_serv[complete,],
                                           col=NULL,
                                           drop_and_replace=TRUE)

serv_opt<-update_sample_col_CSIScontractID(serv_opt,
                                             def_serv[complete,],
                                             col=NULL,
                                             drop_and_replace=TRUE)

# serv_smp<-serv_smp %>% dplyr::select(-c(Ceil, qCRais))
# serv_smp1m<-serv_smp1m %>% dplyr::select(-c(Ceil, qCRais))
# serv_opt<-serv_opt %>% dplyr::select(-c(Ceil, qCRais))


save(file="data/clean/def_sample.Rdata",serv_smp,serv_smp1m,serv_opt)
write.foreign(df=serv_smp,
              datafile="data//clean//def_serv_sample250k.dat",
              codefile="data//clean//def_serv_sample250k_code.do",
              package = "Stata")
write.foreign(df=serv_smp1m,
              datafile="data//clean//def_serv_sample1m.dat",
              codefile="data//clean//def_serv_sample1m_code.do",
              package = "Stata")
write.foreign(df=serv_opt,
              datafile="data//clean//def_serv_opt.dat",
              codefile="data//clean//def_serv_opt_code.do",
              package = "Stata")
