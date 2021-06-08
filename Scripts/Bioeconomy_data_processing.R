################################################################################
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
library(readr)

source("scripts\\NAICS.r")
# read in data

bio_data<-read_delim(file.path("data","semi_clean","Economic.SP_NASbioeconomy.txt"),delim="\t",na=c("NULL","NA"),
                    col_names = TRUE, guess_max = 500000)

#kludge while I redownload
# bio_data<-bio_data %>% dplyr::filter(ProductOrServiceCodeText!="GUIDED MISSILE COMPONENTS"|principalnaicscode!=541714)
bio_data<-standardize_variable_names(bio_data)
colnames(bio_data)[colnames(bio_data)=="obligatedAmount"]<-"Action_Obligation"

# bio_data$Action_Obligation %<>% as.numeric()
# bio_data$Number.Of.Actions %<>% as.numeric()
# bio_data$Fiscal.Year <- as.numeric(bio_data$Fiscal.Year)
# lc<-prepare_labels_and_colors(bio_data)
bio_data$productorservicecode[bio_data$productorservicecode==""]<-NA
bio_data<-read_and_join_experiment(bio_data,
                                        lookup_file = "ProductOrServiceCodes.csv",
                                        path="https://raw.githubusercontent.com/CSISdefense/Lookup-Tables/master/",
                                        dir="",
                             by=c("productorservicecode"="ProductOrServiceCode"),
                             add_var=c("ProductServiceOrRnDarea","BioRelated","RnD_BudgetActivity","Simple"),
                             skip_check_var=c("BioRelated","RnD_BudgetActivity"))
bio_data$BioRelated[bio_data$BioRelated==""]<-NA
bio_data$RnD_BudgetActivity[is.na(bio_data$RnD_BudgetActivity)]<-bio_data$Simple[is.na(bio_data$RnD_BudgetActivity)]


bio_data$NAICS2<-create_naics2(bio_data$principalnaicscode)



bio_data$NASbioEconomy[bio_data$principalnaicscode=="339112"]<-"Surgical and Medical Equipment"

bio_data$biobased<-factor(bio_data$biobased)
levels(bio_data$biobased)<-list("Biobased"="1","Not Biobased"="0")
summary(bio_data$biobased)

bio_data$NASbioEconomypt<-factor(bio_data$NASbioEconomypt)



bio_data$NASbioEconomypt[bio_data$NASbioEconomy %in% c("Biorefining (food)",
                                                     "Crop products")]<-0
levels(bio_data$NASbioEconomypt)<-list("NAICS Partial Code"="1","Other NAICS Code"="0")


summary(bio_data$NASbioEconomypt)
sum(bio_data$Action_Obligation_Then_Year[bio_data$nationalinterestactioncode=="P20C"])

bio_data<-deflate(bio_data,
                 money_var = "Action_Obligation",
                 deflator_var="OMB20_GDP20"
)
summary(factor(bio_data$NASbioEconomy))
bio_data$NASbioEconomy[is.na(bio_data$NASbioEconomy)&bio_data$biobased=="Biobased"]<-"Bioproduct Outside NAS chosen codes"
bio_data$NASbioEconomy[is.na(bio_data$NASbioEconomy)&bio_data$nationalinterestactioncode=="P20C"]<-"Other COVID19 Pandemic"
bio_data$NASbioEconomy[is.na(bio_data$NASbioEconomy)&bio_data$nationalinterestactioncode=="O14E"]<-"Other Ebola Response"


# bio_data<-read_and_join_experiment(bio_data,
#                                         lookup_file = "Lookup_PrincipalNAICScode.csv",
#                                         path="https://raw.githubusercontent.com/CSISdefense/Lookup-Tables/master/",
#                                         dir="economic\\",
#                              by=c("NAICS_Code"="principalnaicscode"),
#                              add_var=c("principalnaicscodeText","NAICS_shorthand"),
#                              skip_check_var=c("principalnaicscodeText","NAICS_shorthand"))


bio_data<-read_and_join_experiment(bio_data,
                                        lookup_file = "Lookup_PrincipalNAICScode.csv",
                                        path="https://raw.githubusercontent.com/CSISdefense/Lookup-Tables/master/",
                                        dir="economic\\",
                             by=c("NAICS2"="principalnaicscode"),
                             add_var=c("principalnaicscodeText","NAICS_shorthand"),
                             skip_check_var=c("principalnaicscodeText","NAICS_shorthand"))

colnames(bio_data)[colnames(bio_data)=="principalnaicscodeText"]<-"NAICS2text"
colnames(bio_data)[colnames(bio_data)=="NAICS_shorthand"]<-"NAICS2shorthand"

if(substring(bio_data$Fiscal.Year[nrow(bio_data)],1,12)=="Completion time")
  bio_data<-bio_data[-nrow(bio_data),]

bio_data$recoveredmaterialclauses[bio_data$recoveredmaterialclauses==""]<-NA


# discard pre-2000
bio_data <-bio_data%>% filter(Fiscal.Year >= 2000)

bio_ck<-get_column_key(bio_data)
bio_lc<-prepare_labels_and_colors(bio_data)


# write.csv(unique(bio_data$NASbioEconomy[!is.na(bio_data$NASbioEconomy)]) ,"new_colors.csv",row.names = FALSE)

# 
#  
#    colnames(bio_data)[colnames(bio_data)=="Action_Obligation_Then_Year"]<-"Action_Obligation"
# bio_data<-deflate(bio_data,
#                    money_var = "Action_Obligation",
#                    deflator_var="OMB20_GDP20"
# )
# 
# 
# 
# #Consolidate categories for Vendor Size
# 
# bio_data<-csis360::read_and_join(bio_data,
#   "LOOKUP_Contractor_Size.csv",
#   by="Vendor.Size",
#   add_var="Shiny.VendorSize",
#   path="https://raw.githubusercontent.com/CSISdefense/R-scripts-and-data/master/",
#   dir="Lookups/"
# )
# 
# 
# 
# # classify competition
# bio_data<-csis360::read_and_join(bio_data,
#   "Lookup_SQL_CompetitionClassification.csv",
#   by=c("CompetitionClassification","ClassifyNumberOfOffers"),
#   replace_na_var="ClassifyNumberOfOffers",
#   add_var=c("Competition.sum",
#     "Competition.multisum",
#     "Competition.effective.only",
#     "No.Competition.sum"),
#   path="https://raw.githubusercontent.com/CSISdefense/R-scripts-and-data/master/",
#   dir="Lookups/"
# )
# 
# # classify competition
# bio_data<-csis360::read_and_join(bio_data,
#                                   "Lookup_SQL_CompetitionClassification.csv",
#                                   by=c("CompetitionClassification","ClassifyNumberOfOffers"),
#                                   replace_na_var="ClassifyNumberOfOffers",
#                                   add_var=c("Competition.sum",
#                                             "Competition.multisum",
#                                             "Competition.effective.only",
#                                             "No.Competition.sum"),
#                                   path="https://raw.githubusercontent.com/CSISdefense/R-scripts-and-data/master/",
#                                   dir="Lookups/"
# )
# 
# #Classify Product or Service Codes
# bio_data<-csis360::read_and_join(bio_data,
#   "LOOKUP_Buckets.csv",
#   # by="ProductOrServiceArea",
#   by="ProductServiceOrRnDarea",
#   replace_na_var="ProductServiceOrRnDarea",
#   add_var="ProductServiceOrRnDarea.sum",
#   path="https://raw.githubusercontent.com/CSISdefense/R-scripts-and-data/master/",
#   dir="Lookups/"
# )
# 
# 
# bio_data<-replace_nas_with_unlabeled(bio_data,"ContractingSubCustomer","Uncategorized")
# bio_data<-csis360::read_and_join_experiment(bio_data,
#                         "SubCustomer.csv",
#                         by=c("ContractingCustomer"="Customer","ContractingSubCustomer"="SubCustomer"),
#                         add_var=c("SubCustomer.platform","SubCustomer.sum"),
#                         path="https://raw.githubusercontent.com/CSISdefense/Lookup-Tables/master/",
#                         dir="office/"
# )
# 
# bio_data<-replace_nas_with_unlabeled(bio_data,"PlatformPortfolio")
# 
# bio_data$PricingUCA.sum<-factor(bio_data$PricingUCA)
# bio_data<-replace_nas_with_unlabeled(bio_data,"PricingUCA.sum")
# levels(bio_data$PricingUCA.sum)<-
#   list("FFP"="FFP",
#        "Less Common"=c("Other FP","T&M/LH/FPLOE"),
#        "Incentive"="Incentive",
#        "Other CB"="Other CB",
#        "UCA"="UCA",
#        "Unclear"=c("Combination/Other","Unlabeled"))
# 
# 
# 
# 
# # 
# # bio_data$PricingUCA<-bio_data$PricingFee
# # summary(factor(bio_data$PricingUCA))
# # bio_data$PricingUCA[is.na(bio_data$IsUCA)]<-NA
# # bio_data$PricingUCA[!is.na(bio_data$IsUCA)&bio_data$IsUCA==1]<-"UCA"
# 
# 
# 
# # debug(csis360::prepare_labels_and_colors)
# # load("Shiny Apps/FPDS_chart_maker/2016_unaggregated_FPDS.Rda")
# 
# 
# 
# # set correct data types
# bio_data %<>%
#   select(-ContractingCustomer) %>%
#   # select(-ClassifyNumberOfOffers) %>%
#   mutate(ContractingSubCustomer = factor(ContractingSubCustomer)) %>%
#   mutate(SubCustomer.platform = factor(SubCustomer.platform)) %>%
#   mutate(ProductServiceOrRnDarea = factor(ProductServiceOrRnDarea)) %>%
#   mutate(PlatformPortfolio = factor(PlatformPortfolio)) %>%
#   mutate(Shiny.VendorSize = factor(Shiny.VendorSize)) %>%
#   mutate(ProductServiceOrRnDarea.sum = factor(ProductServiceOrRnDarea.sum)) %>%
#   mutate(Competition.sum = factor(Competition.sum)) %>%
#   mutate(Competition.effective.only = factor(Competition.effective.only)) %>%
#   mutate(Competition.multisum = factor(Competition.multisum))  %>%
#   mutate(No.Competition.sum = factor(No.Competition.sum)) %>%
#   mutate(Vehicle = factor(Vehicle)) %>%
#   mutate(PricingUCA = factor(PricingUCA))
# 
# colnames(bio_data)[colnames(bio_data)=="Fiscal.Year"]<-"fiscal_year"
# 
# 
# labels_and_colors<-csis360::prepare_labels_and_colors(bio_data)
# 
# column_key<-csis360::get_column_key(bio_data)
# bio_data$dFYear<-as.Date(paste("1/1/",as.character(bio_data$Fiscal.Year),sep=""),"%m/%d/%Y")
# 
# # write output to CleanedVendorSize.csv
# # save(bio_data,labels_and_colors,column_key, file="Shiny Apps//FPDS_chart_maker//2018_unaggregated_FPDS.Rda")
# 
# # # 
# # partial_2018 <- read_delim(
# #   "Data//Single_Year_Summary_2019-02-25.csv",delim = ",")
# # colnames(partial_2018)[colnames(partial_2018)=="X9"]<-"ContractActions"
# # 
# # sum(text_to_number(partial_2018$`Action Obligation`))
# # # 
# # # 
# # partial_2018<-standardize_variable_names(partial_2018)
# # 
# # 
# # colnames(partial_2018)[colnames(partial_2018)=="Contracting.Agency.ID"]<-"AgencyID"
# # partial_2018$Action_Obligation<-text_to_number(partial_2018$Action_Obligation)
# # partial_2018$Fiscal.Year<-2018
# # sum(partial_2018$Action_Obligation)
# # 
# # 
# # partial_2018<-deflate(partial_2018,
# #                    money_var = "Action_Obligation",
# #                    deflator_var="OMB19_19"
# # )
# # 
# # partial_2018<-deflate(partial_2018,
# #                       money_var = "Action_Obligation",
# #                       deflator_var="OMB20_GDP18"
# # )
# # 
# # sum(partial_2018$Action_Obligation.Then.Year)
# # # 
# # partial_2018<-transform_contract(partial_2018)
# # 
# # 
# # 
# # partial_2018<-read_and_join(partial_2018,
# #                             path="https://raw.githubusercontent.com/CSISdefense/Lookup-Tables/master/",
# #                             "Agency_AgencyID.csv",
# #                             dir="",
# #                             by=c("AgencyID"),
# #                             add_var=c("ContractingCustomer","SubCustomer","Platform"),
# #                             skip_check_var="Platform")
# # 
# # sum(partial_2018$Action_Obligation.Then.Year)
# 
# # partial_2018<-partial_2018%>%filter(ContractingCustomer=="Defense")
# # 
# # #Classify Product or Service Codes
# # partial_2018<-csis360::read_and_join(partial_2018,
# #                                   "LOOKUP_Buckets.csv",
# #                                   # by="ProductOrServiceArea",
# #                                   by="ProductServiceOrRnDarea",
# #                                   replace_na_var="ProductServiceOrRnDarea",
# #                                   add_var="ProductServiceOrRnDarea.sum",
# #                                   path="https://raw.githubusercontent.com/CSISdefense/R-scripts-and-data/master/",
# #                                   dir="Lookups/"
# # )
# # 
# # 
# # partial_2018<-replace_nas_with_unlabeled(partial_2018,"SubCustomer","Uncategorized")
# # partial_2018<-csis360::read_and_join(partial_2018,
# #                                   "Lookup_SubCustomer.csv",
# #                                   by=c("ContractingCustomer","ContractingSubCustomer"),
# #                                   add_var="SubCustomer.platform",
# #                                   path="https://raw.githubusercontent.com/CSISdefense/R-scripts-and-data/master/",
# #                                   dir="Lookups/"
# # )
# # 
# # 
# # colnames(partial_2018)[colnames(partial_2018)=="Fiscal.Year"]<-"fiscal_year"
# # colnames(partial_2018)[colnames(partial_2018)=="ContractActions"]<-"Number.Of.Actions"
# # 
# # colnames(partial_2018)[colnames(partial_2018) %in% colnames(bio_data)]
# # colnames(bio_data)[!colnames(bio_data) %in% colnames(partial_2018)]
# # 
# # 
# # 
# # 
# # partial_2018<-partial_2018 %>% group_by(ProductServiceOrRnDarea,
# #                                         ProductServiceOrRnDarea.sum,
# #                           ContractingSubCustomer,
# #                           SubCustomer.platform,
# #                           Fiscal.Year) %>%
# #   dplyr::summarize(Action_Obligation.Then.Year=sum(Action_Obligation.Then.Year,na.rm=TRUE),
# #                    Action_Obligation.OMB.2019=sum(Action_Obligation.OMB.2019,na.rm=TRUE),
# #                    Number.Of.Actions=sum(Number.Of.Actions,na.rm=TRUE))
# # 
# # 
# # 
# # partial_2018$PlatformPortfolio<-"Unlabeled"
# # partial_2018$Vendor.Size<-"Unlabeled"
# # partial_2018$CompetitionClassification<-"Unlabeled"
# # partial_2018$ClassifyNumberOfOffers<-"Unlabeled"
# # partial_2018$Shiny.VendorSize<-"Unlabeled"
# # partial_2018$Competition.sum<-"Unlabeled"
# # partial_2018$Competition.effective.only<-"Unlabeled"
# # partial_2018$Competition.multisum<-"Unlabeled"
# # partial_2018$No.Competition.sum<-"Unlabeled"
# # 
# # bio_data<-rbind(bio_data,as.data.frame(partial_2018))
# # 

# # 
# # 
# 
# # write.csv(bio_data%>%group_by(Fiscal.Year)%>%
# #       dplyr::summarize(Action_Obligation.Then.Year=sum(Action_Obligation.Then.Year,na.rm=TRUE),
# #                                                          Action_Obligation.OMB.2019=sum(Action_Obligation.OMB.2019,na.rm=TRUE),
# #                                                          Number.Of.Actions=sum(Number.Of.Actions,na.rm=TRUE)),
# #       file="topline_usaspending.csv"
# # )
# # 
# # 
# # bio_data <- read_delim(
# #   "Data//2017_Summary.SP_CompetitionVendorSizeHistoryBucketPlatformSubCustomer.txt",delim = "\t",
# #   col_names = TRUE, col_types = "cccccccccc",na=c("NA","NULL"))


ota<-read_delim(file.path("data_raw","OTA_All_FIelds.csv"),delim=",",na=c("NULL","NA"),
                     col_names = TRUE, guess_max = 500000,skip=2)
ota$COVIDmention<-FALSE
ota$
  gsub(" ","_",colnames(ota))



#Life science RnD
life_rnd<-read_delim(file.path("data_raw","Bioeconomy","FFS_export_table_2021-05-23T11 42 50.584Z.csv"),delim=",",na=c("NULL","NA"),
                     col_names = TRUE, guess_max = 500000,skip=11)
# colnames(life_rnd)[grep("X[0-9]",colnames(life_rnd))]<-life_rnd[1,grep("X[0-9]",colnames(life_rnd))]
check_column<-max(which(life_rnd[1,]!=""))
colnames(life_rnd)[life_rnd[1,]!=""]<-c(life_rnd[1,which(life_rnd[1,]!="")][-1],"ManualLabel")
life_rnd<-life_rnd[-which(life_rnd[,check_column]=="Total for selected values"),]
life_rnd<-life_rnd[life_rnd[,1]!="[measures]",]
life_rnd<-life_rnd[life_rnd[,2]!="",]
life_rnd<-pivot_longer(life_rnd, names_to="Fiscal.Year", cols=colnames(life_rnd)[grep("[1-2][0-9][0-9][0-9]",colnames(life_rnd))])
life_rnd$value[life_rnd$value=="-"]<-"0"
life_rnd$value<-text_to_number(life_rnd$value)
life_rnd<-deflate(life_rnd,
                  money_var = "value",
                  deflator_var="OMB20_GDP20"
)
life2020<-readxl::read_excel(file.path("data_raw","Bioeconomy","nsf21329-data-tables-tables","nsf21329-tab017.xlsx"),skip=3)
life2020value<-as.numeric(life2020$`2020 (preliminary)`[life2020$Field=="Life sciences"])
life
# data.frame("[Science and Engineering]"="Science",
#               "[Broad Fields]"= "Life sciences",
#                "[Detailed Fields]"="Total Life Sciences",
#                "[Federal Department]"=NA,
#                "[Federal Agency]"=NA,
#                "[Basic Research and Applied Research]"=NA,
#                "ManualLabel"=NA,
#                "Fiscal.Year"=2020,
#               "value_Then_Year"=life2020value)

life_HERD<-read_delim(file.path("data_raw","Bioeconomy","HERD_export_table_2021-06-08T02 40 54.456Z.csv"),delim=",",na=c("NULL","NA"),
                      col_names = TRUE, guess_max = 500000,skip=11)
colnames(life_HERD)[colnames(life_HERD)=="X5"]<-"Expenditures"
colnames(life_HERD)<-gsub("\\[","",colnames(life_HERD))
colnames(life_HERD)<-gsub("\\]","",colnames(life_HERD))
colnames(life_HERD)<-gsub(" ","_",colnames(life_HERD))
check_column<-max(which(colnames(life_HERD)!="Expenditures"))
life_HERD<-life_HERD[-which(life_HERD[,check_column]=="Total for selected values"),]

life_HERD<-life_HERD[!is.na(life_HERD$Detailed_Field),]
life_HERD<-life_HERD[!is.na(life_HERD$Fiscal_Year),]
life_HERD$Fiscal_Year<-text_to_number(life_HERD$Fiscal_Year)
life_HERD$Expenditures<-life_HERD$Expenditures*1000
life_HERD<-deflate(life_HERD,
                  money_var = "Expenditures",
                  deflator_var="OMB20_GDP20"
)

save(bio_data,bio_lc,bio_ck,life_rnd, file="data/Clean/BioEconomy.Rda")#bio_lc

