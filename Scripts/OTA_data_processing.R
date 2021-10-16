# Data Pre-Processing for Vendor Size Shiny Graphic
# UPDATED 2021/03/07
#
# This script does pre-processing to get a SQL query into usable form for shiny
# graphics
#
# Input:
#   CSV-format results from SQL query:
#     Vendor_SP_CompetitionVendorSizeHistoryBucketPlatformSubCustomer
#
# Output: CSV file (unaggregated_FPDS.Rda)
# with data in the minimal form needed by Shiny script
################################################################################

# install.packages("../csis360_0.0.0.9022.tar.gz")

library(tidyverse)
library(magrittr)
library(csis360)
library(Hmisc)
# read in data
OTA_data <- read_delim(
  "data_raw//OTA_All_FIelds.csv",delim = ",",
  col_names = TRUE, guess_max = 500000,na=c("NA","NULL"),skip = 2)

colnames(OTA_data)<-gsub(" ","_",colnames(OTA_data))
initial_clean<-function(df){
  if(substring(df$fiscal_year[nrow(df)],1,12)=="Completion time")
    df<-df[-nrow(df),]
  
  df<-standardize_variable_names(df)
  # coerce Amount to be a numeric variable
  df$Action_Obligation %<>% as.numeric()
  if("Number.Of.Actions" %in% colnames(df)) 
    df$Number.Of.Actions %<>% as.numeric()
  df$Fiscal.Year <- as.numeric(df$Fiscal.Year)
  colnames(df)[colnames(df)=="Contractingcustomer"]<-"ContractingCustomer"
  colnames(df)[colnames(df)=="platformportfolio"]<-"PlatformPortfolio"
  # discard pre-2000
  df %<>% filter(Fiscal.Year >= 2000 & ContractingCustomer=="Defense")
  colnames(df)[colnames(df)=="Action_Obligation_Then_Year"]<-"Action_Obligation"
  colnames(df)[colnames(df)=="Fiscal.Year"]<-"fiscal_year"
  df$dFYear<-as.Date(paste("1/1/",as.character(df$fiscal_year),sep=""),"%m/%d/%Y")
  df
}

OTA_data<-deflate(OTA_data,
                   money_var = "Dollars_Obligated",
                   fy_var="Fiscal_Year",
                   deflator_var="OMB20_GDP20",
                   path="C:\\Users\\gsand\\Repositories\\Lookup-Tables\\"
)



#Consolidate categories for Vendor Size

OTA_data<-csis360::read_and_join(OTA_data,
  "LOOKUP_Contractor_Size.csv",
  by="Vendor.Size",
  add_var="Shiny.VendorSize",
  path="https://raw.githubusercontent.com/CSISdefense/R-scripts-and-data/master/",
  dir="Lookups/"
)



# classify competition
OTA_data<-csis360::read_and_join(OTA_data,
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

# classify competition
OTA_data<-csis360::read_and_join(OTA_data,
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

OTA_data$Product_or_Service_Code
#Classify Product or Service Codes
OTA_data<-csis360::read_and_join_experiment(OTA_data,
                                  "ProductOrServiceCodes.csv",
                                  by=c("Product_or_Service_Code"="ProductOrServiceCode"),
                                  # replace_na_var="ProductServiceOrCode",
                                  add_var=c("PlatformPortfolio","ProductServiceOrRnDarea"),
                                  path="C:\\Users\\gsand\\Repositories\\Lookup-Tables\\",
                                    #"https://raw.githubusercontent.com/CSISdefense/R-scripts-and-data/master/",
                                  dir=""
)



#Classify Product or Service Codes
OTA_data<-csis360::read_and_join(OTA_data,
  "LOOKUP_Buckets.csv",
  # by="ProductOrServiceArea",
  by="ProductServiceOrRnDarea",
  replace_na_var="ProductServiceOrRnDarea",
  add_var="ProductServiceOrRnDarea.sum",
  path="https://raw.githubusercontent.com/CSISdefense/R-scripts-and-data/master/",
  dir="Lookups/"
)


OTA_data<-replace_nas_with_unlabeled(OTA_data,"ContractingSubCustomer","Uncategorized")
OTA_data<-csis360::read_and_join_experiment(OTA_data,
                        "SubCustomer.csv",
                        by=c("ContractingCustomer"="Customer","ContractingSubCustomer"="SubCustomer"),
                        add_var=c("SubCustomer.platform","SubCustomer.sum"),
                        path="https://raw.githubusercontent.com/CSISdefense/Lookup-Tables/master/",
                        dir="office/"
)

OTA_data<-csis360::read_and_join_experiment(OTA_data,
                                             "Vehicle.csv",
                                             by=c("Vehicle"="Vehicle.detail"),
                                             add_var=c("Vehicle.sum","Vehicle.AwardTask"),
                                             path="https://raw.githubusercontent.com/CSISdefense/Lookup-Tables/master/",
                                             # path="K:/Users/Greg/Repositories/Lookup-Tables/",
                                             dir="contract/"
)

OTA_data<-replace_nas_with_unlabeled(OTA_data,"PlatformPortfolio")

OTA_data$PricingUCA.sum<-factor(OTA_data$PricingUCA)
OTA_data<-replace_nas_with_unlabeled(OTA_data,"PricingUCA.sum")
levels(OTA_data$PricingUCA.sum)<-
  list("FFP"="FFP",
       "Less Common"=c("Other FP","T&M/LH/FPLOE"),
       "Incentive"="Incentive",
       "Other CB"="Other CB",
       "UCA"="UCA",
       "Unclear"=c("Combination/Other","Unlabeled"))





# 
# OTA_data$PricingUCA<-OTA_data$PricingFee
# summary(factor(OTA_data$PricingUCA))
# OTA_data$PricingUCA[is.na(OTA_data$IsUCA)]<-NA
# OTA_data$PricingUCA[!is.na(OTA_data$IsUCA)&OTA_data$IsUCA==1]<-"UCA"



# debug(csis360::prepare_labels_and_colors)
# load("Shiny Apps/FPDS_chart_maker/2016_unaggregated_FPDS.Rda")

platpsc<-read_and_join_experiment(platpsc,
                            path="https://raw.githubusercontent.com/CSISdefense/Lookup-Tables/master/",
                            "Agency_AgencyID.csv",
                            dir="",
                            by=c("Contracting.Agency.ID"="AgencyID"),
                            add_var=c("SubCustomer"),#Contracting.Agency.ID
                            skip_check_var=c("Platform","SubCustomer"),
                            guess_max=2000)
colnames(platpsc)[colnames(platpsc)=="SubCustomer"]<-"ContractingSubCustomer"

platpsc<-replace_nas_with_unlabeled(platpsc,"ContractingSubCustomer","Uncategorized")
platpsc<-csis360::read_and_join_experiment(platpsc,
                                             "SubCustomer.csv",
                                             by=c("ContractingCustomer"="Customer","ContractingSubCustomer"="SubCustomer"),
                                             add_var=c("SubCustomer.platform","SubCustomer.sum"),
                                             path="https://raw.githubusercontent.com/CSISdefense/Lookup-Tables/master/",
                                             dir="office/"
)


# set correct data types
OTA_data %<>%
  select(-ContractingCustomer) %>%
  # select(-ClassifyNumberOfOffers) %>%
  mutate(ContractingSubCustomer = factor(ContractingSubCustomer)) %>%
  mutate(SubCustomer.platform = factor(SubCustomer.platform)) %>%
  mutate(ProductServiceOrRnDarea = factor(ProductServiceOrRnDarea)) %>%
  mutate(PlatformPortfolio = factor(PlatformPortfolio)) %>%
  mutate(Shiny.VendorSize = factor(Shiny.VendorSize)) %>%
  mutate(ProductServiceOrRnDarea.sum = factor(ProductServiceOrRnDarea.sum)) %>%
  mutate(Competition.sum = factor(Competition.sum)) %>%
  mutate(Competition.effective.only = factor(Competition.effective.only)) %>%
  mutate(Competition.multisum = factor(Competition.multisum))  %>%
  mutate(No.Competition.sum = factor(No.Competition.sum)) %>%
  mutate(Vehicle = factor(Vehicle)) %>%
  mutate(PricingUCA = factor(PricingUCA))


platpsc %<>%
  select(-ContractingCustomer) %>%
  # select(-ClassifyNumberOfOffers) %>%
  mutate(ContractingSubCustomer = factor(ContractingSubCustomer)) %>%
  mutate(SubCustomer.platform = factor(SubCustomer.platform)) %>%
  mutate(ProductServiceOrRnDarea = factor(ProductServiceOrRnDarea)) %>%
  mutate(PlatformPortfolio = factor(PlatformPortfolio))
  


detail_lc<-csis360::prepare_labels_and_colors(platpsc)
detail_ck<-csis360::get_column_key(platpsc)



labels_and_colors<-csis360::prepare_labels_and_colors(OTA_data)

column_key<-csis360::get_column_key(OTA_data)


# write output to CleanedVendorSize.csv
# save(OTA_data,labels_and_colors,column_key, file="Shiny Apps//FPDS_chart_maker//2018_unaggregated_FPDS.Rda")

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

# 
# sum(partial_2018$Action_Obligation.Then.Year)

# partial_2018<-partial_2018%>%filter(ContractingCustomer=="Defense")
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
#                                   by=c("ContractingCustomer","ContractingSubCustomer"),
#                                   add_var="SubCustomer.platform",
#                                   path="https://raw.githubusercontent.com/CSISdefense/R-scripts-and-data/master/",
#                                   dir="Lookups/"
# )
# 
# 
# colnames(partial_2018)[colnames(partial_2018)=="Fiscal.Year"]<-"fiscal_year"
# colnames(partial_2018)[colnames(partial_2018)=="ContractActions"]<-"Number.Of.Actions"
# 
# colnames(partial_2018)[colnames(partial_2018) %in% colnames(OTA_data)]
# colnames(OTA_data)[!colnames(OTA_data) %in% colnames(partial_2018)]
# 
# 
# 
# 
# partial_2018<-partial_2018 %>% group_by(ProductServiceOrRnDarea,
#                                         ProductServiceOrRnDarea.sum,
#                           ContractingSubCustomer,
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
# OTA_data<-rbind(OTA_data,as.data.frame(partial_2018))
# 
save(OTA_data,labels_and_colors,column_key, file="analysis/FPDS_chart_maker/unaggregated_FPDS.Rda")
save(platpsc,labels_and_colors,column_key, file="data/semi_clean/platpsc_FPDS.Rda")
# 
# 

# write.csv(OTA_data%>%group_by(fiscal_year)%>%
#       dplyr::summarize(Action_Obligation.Then.Year=sum(Action_Obligation.Then.Year,na.rm=TRUE),
#                                                          Action_Obligation.OMB.2019=sum(Action_Obligation.OMB.2019,na.rm=TRUE),
#                                                          Number.Of.Actions=sum(Number.Of.Actions,na.rm=TRUE)),
#       file="topline_usaspending.csv"
# )
# 
# 
# OTA_data <- read_delim(
#   "Data//2017_Summary.SP_CompetitionVendorSizeHistoryBucketPlatformSubCustomer.txt",delim = "\t",
#   col_names = TRUE, col_types = "cccccccccc",na=c("NA","NULL"))
