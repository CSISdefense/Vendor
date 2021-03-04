################################################################################
# Data Pre-Processing for Vendor Size Shiny Graphic
# UPDATED 2/22/17
#
# This script does pre-processing to get a SQL query into usable form for shiny
# graphics
#
# Input:
#   CSV-format results from SQL query:
#     Vendor_SP_CompetitionVendorSizeHistoryBucketPlatformSubCustomer
#
# Output: CSV file (CleanedVendorSize.csv)
# with data in the minimal form needed by Shiny script
################################################################################

# install.packages("../csis360_0.0.0.9022.tar.gz")

library(tidyverse)
library(magrittr)
library(csis360)
library(Hmisc)
# read in data
full_data <- read_delim(
  "Data//semi_clean//Summary.SP_CompetitionVendorSizeHistoryBucketPlatformSubCustomer.txt",delim = "\t",
  col_names = TRUE, col_types = "cccccccccc",na=c("NA","NULL"))

if(substring(full_data$fiscal_year[nrow(full_data)],1,15)=="Completion time")
  full_data<-full_data[-nrow(full_data)]

full_data<-standardize_variable_names(full_data)
# coerce Amount to be a numeric variable
full_data$Action_Obligation %<>% as.numeric()
full_data$Number.Of.Actions %<>% as.numeric()
full_data$Fiscal.Year <- as.numeric(full_data$Fiscal.Year)

# discard pre-2000
full_data %<>% filter(Fiscal.Year >= 2000 & Customer=="Defense")


full_data<-deflate(full_data,
                  money_var = "Action_Obligation",
                  deflator_var="OMB19_19"
)
 
   colnames(full_data)[colnames(full_data)=="Action_Obligation_Then_Year"]<-"Action_Obligation"
full_data<-deflate(full_data,
                   money_var = "Action_Obligation",
                   deflator_var="OMB20_GDP18"
)



#Consolidate categories for Vendor Size

full_data<-csis360::read_and_join(full_data,
  "LOOKUP_Contractor_Size.csv",
  by="Vendor.Size",
  add_var="Shiny.VendorSize",
  path="https://raw.githubusercontent.com/CSISdefense/R-scripts-and-data/master/",
  dir="Lookups/"
)



# classify competition
full_data<-csis360::read_and_join(full_data,
  "Lookup_SQL_CompetitionClassification.csv",
  by=c("CompetitionClassification","ClassifyNumberOfOffers"),
  replace_na_var="ClassifyNumberOfOffers",
  add_var=c("Competition.sum",
    "Competition.multisum",
    "Competition.effective.only",
    "No.Competition.sum"),
  path="https://raw.githubusercontent.com/CSISdefense/R-scripts-and-data/master/",
  dir="Lookups/"
)


#Classify Product or Service Codes
full_data<-csis360::read_and_join(full_data,
  "LOOKUP_Buckets.csv",
  # by="ProductOrServiceArea",
  by="ProductServiceOrRnDarea",
  replace_na_var="ProductServiceOrRnDarea",
  add_var="ProductServiceOrRnDarea.sum",
  path="https://raw.githubusercontent.com/CSISdefense/R-scripts-and-data/master/",
  dir="Lookups/"
)


full_data<-replace_nas_with_unlabeled(full_data,"SubCustomer","Uncategorized")
full_data<-csis360::read_and_join(full_data,
                        "SubCustomer.csv",
                        by=c("Customer","SubCustomer"),
                        add_var="SubCustomer.platform",
                        path="https://raw.githubusercontent.com/CSISdefense/Lookup-Tables/master/",
                        dir="office/"
)


full_data<-replace_nas_with_unlabeled(full_data,"PlatformPortfolio")

# debug(csis360::prepare_labels_and_colors)
# load("Shiny Apps/FPDS_chart_maker/2016_unaggregated_FPDS.Rda")
labels_and_colors<-csis360::prepare_labels_and_colors(full_data)


column_key<-csis360::get_column_key(full_data)

# set correct data types
full_data %<>%
  select(-Customer) %>%
  # select(-ClassifyNumberOfOffers) %>%
  mutate(SubCustomer = factor(SubCustomer)) %>%
  mutate(SubCustomer.platform = factor(SubCustomer.platform)) %>%
  mutate(ProductServiceOrRnDarea = factor(ProductServiceOrRnDarea)) %>%
  mutate(PlatformPortfolio = factor(PlatformPortfolio)) %>%
  mutate(Shiny.VendorSize = factor(Shiny.VendorSize)) %>%
  mutate(ProductServiceOrRnDarea.sum = factor(ProductServiceOrRnDarea.sum)) %>%
  mutate(Competition.sum = factor(Competition.sum)) %>%
  mutate(Competition.effective.only = factor(Competition.effective.only)) %>%
  mutate(Competition.multisum = factor(Competition.multisum))  %>%
  mutate(No.Competition.sum = factor(No.Competition.sum))

colnames(full_data)[colnames(full_data)=="Fiscal.Year"]<-"fiscal_year"


full_data$dFYear<-as.Date(paste("1/1/",as.character(full_data$fiscal_year),sep=""),"%m/%d/%Y")

# write output to CleanedVendorSize.csv
# save(full_data,labels_and_colors,column_key, file="Shiny Apps//FPDS_chart_maker//2018_unaggregated_FPDS.Rda")

# # 
# partial_2018 <- read_delim(
#   "Data//Single_Year_Summary_2019-02-25.csv",delim = ",")
# colnames(partial_2018)[colnames(partial_2018)=="X9"]<-"ContractActions"
# 
# sum(text_to_number(partial_2018$`Action Obligation`))
# # 
# # 
# partial_2018<-standardize_variable_names(partial_2018)
# 
# 
# colnames(partial_2018)[colnames(partial_2018)=="Contracting.Agency.ID"]<-"AgencyID"
# partial_2018$Action_Obligation<-text_to_number(partial_2018$Action_Obligation)
# partial_2018$Fiscal.Year<-2018
# sum(partial_2018$Action_Obligation)
# 
# 
# partial_2018<-deflate(partial_2018,
#                    money_var = "Action_Obligation",
#                    deflator_var="OMB19_19"
# )
# 
# partial_2018<-deflate(partial_2018,
#                       money_var = "Action_Obligation",
#                       deflator_var="OMB20_GDP18"
# )
# 
# sum(partial_2018$Action_Obligation.Then.Year)
# # 
# partial_2018<-transform_contract(partial_2018)
# 
# 
# 
# partial_2018<-read_and_join(partial_2018,
#                             path="https://raw.githubusercontent.com/CSISdefense/Lookup-Tables/master/",
#                             "Agency_AgencyID.csv",
#                             dir="",
#                             by=c("AgencyID"),
#                             add_var=c("Customer","SubCustomer","Platform"),
#                             skip_check_var="Platform")
# 
# sum(partial_2018$Action_Obligation.Then.Year)

# partial_2018<-partial_2018%>%filter(Customer=="Defense")
# 
# #Classify Product or Service Codes
# partial_2018<-csis360::read_and_join(partial_2018,
#                                   "LOOKUP_Buckets.csv",
#                                   # by="ProductOrServiceArea",
#                                   by="ProductServiceOrRnDarea",
#                                   replace_na_var="ProductServiceOrRnDarea",
#                                   add_var="ProductServiceOrRnDarea.sum",
#                                   path="https://raw.githubusercontent.com/CSISdefense/R-scripts-and-data/master/",
#                                   dir="Lookups/"
# )
# 
# 
# partial_2018<-replace_nas_with_unlabeled(partial_2018,"SubCustomer","Uncategorized")
# partial_2018<-csis360::read_and_join(partial_2018,
#                                   "Lookup_SubCustomer.csv",
#                                   by=c("Customer","SubCustomer"),
#                                   add_var="SubCustomer.platform",
#                                   path="https://raw.githubusercontent.com/CSISdefense/R-scripts-and-data/master/",
#                                   dir="Lookups/"
# )
# 
# 
# colnames(partial_2018)[colnames(partial_2018)=="Fiscal.Year"]<-"fiscal_year"
# colnames(partial_2018)[colnames(partial_2018)=="ContractActions"]<-"Number.Of.Actions"
# 
# colnames(partial_2018)[colnames(partial_2018) %in% colnames(full_data)]
# colnames(full_data)[!colnames(full_data) %in% colnames(partial_2018)]
# 
# 
# 
# 
# partial_2018<-partial_2018 %>% group_by(ProductServiceOrRnDarea,
#                                         ProductServiceOrRnDarea.sum,
#                           SubCustomer,
#                           SubCustomer.platform,
#                           fiscal_year) %>%
#   dplyr::summarize(Action_Obligation.Then.Year=sum(Action_Obligation.Then.Year,na.rm=TRUE),
#                    Action_Obligation.OMB.2019=sum(Action_Obligation.OMB.2019,na.rm=TRUE),
#                    Number.Of.Actions=sum(Number.Of.Actions,na.rm=TRUE))
# 
# 
# 
# partial_2018$PlatformPortfolio<-"Unlabeled"
# partial_2018$Vendor.Size<-"Unlabeled"
# partial_2018$CompetitionClassification<-"Unlabeled"
# partial_2018$ClassifyNumberOfOffers<-"Unlabeled"
# partial_2018$Shiny.VendorSize<-"Unlabeled"
# partial_2018$Competition.sum<-"Unlabeled"
# partial_2018$Competition.effective.only<-"Unlabeled"
# partial_2018$Competition.multisum<-"Unlabeled"
# partial_2018$No.Competition.sum<-"Unlabeled"
# 
# full_data<-rbind(full_data,as.data.frame(partial_2018))
# 
save(full_data,labels_and_colors,column_key, file="analysis/FPDS_chart_maker/unaggregated_FPDS.Rda")
# 
# 

# write.csv(full_data%>%group_by(fiscal_year)%>%
#       dplyr::summarize(Action_Obligation.Then.Year=sum(Action_Obligation.Then.Year,na.rm=TRUE),
#                                                          Action_Obligation.OMB.2019=sum(Action_Obligation.OMB.2019,na.rm=TRUE),
#                                                          Number.Of.Actions=sum(Number.Of.Actions,na.rm=TRUE)),
#       file="topline_usaspending.csv"
# )
# 
# 
# full_data <- read_delim(
#   "Data//2017_Summary.SP_CompetitionVendorSizeHistoryBucketPlatformSubCustomer.txt",delim = "\t",
#   col_names = TRUE, col_types = "cccccccccc",na=c("NA","NULL"))
