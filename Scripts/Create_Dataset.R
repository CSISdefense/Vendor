# ---
# title: "Create Dataset"
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



# Contracts are classified using a mix of numerical and categorical variables. While the changes in numerical variables are easy to grasp and summarize, a contract may have one line item that is competed and another that is not. As is detailed in the exploration on R&D, we are only considering information available prior to contract start. The percentage of contract obligations that were competed is a valuable benchmark, but is highly influenced by factors that occured after contract start..



##Prepare Data
# First we load the data. The dataset used is a U.S. Defense Contracting dataset derived from   FPDS.


###Data Transformations and Summary


load(file="Data/Clean/defense_contract_complete_detail.RData")
def<-def[def$MinOfSignedDate>=as.Date("2008-01-01") & def$MinOfSignedDate<=as.Date("2015-12-31"),]
def<-transform_contract(def)
def$l_Ceil<-na_non_positive_log(def$UnmodifiedContractBaseAndAllOptionsValue_Then_Year)
def$cl_Ceil<-arm::rescale(def$l_Ceil)
def$l_def6_obl_lag1<-na_non_positive_log(def$def6_obl_lag1)
def$cl_def6_obl_lag1<-arm::rescale(def$l_def6_obl_lag1)
def$l_US6_avg_sal_lag1<-na_non_positive_log(def$US6_avg_sal_lag1)
def$cl_US6_avg_sal_lag1<-arm::rescale(def$l_US6_avg_sal_lag1)

def<-def[!colnames(def) %in% colnames(def)[grep("^l_",colnames(def))]]
def<-def[!colnames(def) %in% colnames(def)[grep("^capped_l_",colnames(def))]]


l_Offr
def$l_Offr<-na_non_positive_log(def$UnmodifiedNumberOfOffersReceived)
#ObligationWT
if("Action_Obligation.Then.Year" %in% colnames(def)){
  def$ObligationWT_Then_Year<-def$Action_Obligation.Then.Year
  def$ObligationWT_Then_Year[def$ObligationWT_Then_Year<0]<-NA
}
colnames(def)[colnames(def)=="Action_Obligation.Then.Year"]<-"Action_Obligation_Then_Year"
colnames(def)[colnames(def)=="UnmodifiedContractBaseAndAllOptionsValue.Then.Year"]<-
  "UnmodifiedContractBaseAndAllOptionsValue_Then_Year"


# # head(def)
# debug(transform_contract)
# 
# 

# load(file="..\\data\\clean\\defense_contract_all_detail.Rdata")
# def_update<-def_all
# rm(def_all)
# def_update<-def_update[def_update$MinOfSignedDate>=as.Date("2008-01-01") & def_update$MinOfSignedDate<=as.Date("2015-12-31"),]
# def_update<-def_update %>% dplyr::select(CSIScontractID,IsComplete)
# 
# 
# 
# length(unique((def_serv$ContractingOfficeCode)))
# length(unique((def_serv$Agency)))
# length(unique((def_serv$NAICS)))
# length(unique((def_serv$NAICS3)))
# 
# 
# 
# 
# 
# #********** NAICS - Office hhi
# annual_office_naics_hhi <- read.csv("data//clean//annual_office_naics_hhi.csv", header = TRUE, row.names = "X") %>% 
#   dplyr::select(-c("obligatedAmount","numberOfContracts"))
# 
# #Pulling in join annual_office_naics_hhi
# def_serv <- def_serv %>%
#   mutate("StartFY_lag1" = StartFY - 1) %>%
#   left_join(annual_office_naics_hhi, by = c("Office" = "ContractingOfficeCode", "StartFY_lag1" = "Fiscal_year")) %>%
#   dplyr::select(-c("StartFY_lag1"))
# rm(annual_office_naics_hhi)
# 
# #Pulling in join avg_office_naics_hhi
# avg_office_naics_hhi <- read.csv("data//clean//average_office_naics_hhi.csv", header = TRUE, row.names = "X") %>% 
#   dplyr::select(-c("obligatedAmount","numberOfContracts"))
# def_serv <- def_serv %>%
#   left_join(avg_office_naics_hhi, by = c("Office" = "ContractingOfficeCode"))
# rm(avg_office_naics_hhi)
# 
# #Imputing missing
# summary(def_serv$office_naics_hhi_obl)
# def_serv$office_naics_hhi_obl[is.na(def_serv$office_naics_hhi_obl)]<-def_serv$avg_office_naics_hhi_obl[is.na(def_serv$office_naics_hhi_obl)]
# summary(def_serv$office_naics_hhi_k)
# def_serv$office_naics_hhi_k[is.na(def_serv$office_naics_hhi_k)]<-def_serv$avg_office_naics_hhi_k[is.na(def_serv$office_naics_hhi_k)]
# 
# def_serv$cl_office_naics_hhi_obl <- arm::rescale(na_non_positive_log(def_serv$office_naics_hhi_obl))
# def_serv$cl_office_naics_hhi_k <- arm::rescale(na_non_positive_log(def_serv$office_naics_hhi_k))
# def_serv<- def_serv %>% dplyr::select(-c(avg_office_naics_hhi_obl,avg_office_naics_hhi_k))
# 


save(file="data/clean/transformed_def_serv.Rdata",def_serv)

