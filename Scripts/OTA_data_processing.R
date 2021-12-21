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
OTA_data_current <- read_delim(
  "data_raw//OTA_All_Fields.csv",delim = ",",
  col_names = TRUE, guess_max = 500000,na=c("NA","NULL"),skip = 2)



OTA_data_current$MentionsCovid<-TRUE
OTA_data_current$MCDCcovid<-FALSE

OTA_data_current$MentionsCovid[grep("COVID-19",OTA_data_current$`Description of Requirement`,invert=TRUE)]<-FALSE
OTA_data_current$MCDCcovid[OTA_data_current$PIID=="W15QKN1691002" & 
                     OTA_data_current$MentionsCovid]<-TRUE

summary(OTA_data_current$MCDCcovid)
View(OTA_data_current$`Description of Requirement`[OTA_data_current$MCDCcovid==TRUE])
descript<-OTA_data_current$`Description of Requirement`[OTA_data_current$MCDCcovid==TRUE]
OTA_data_current$`Dollars Obligated`<-text_to_number(OTA_data_current$`Dollars Obligated`)


OTA_data <- read_delim(
  "data_raw//OTA_NPS_report.csv",delim = ",",
  col_names = TRUE, guess_max = 500000,na=c("NA","NULL"))


nrow(OTA_data[OTA_data$`Description of Requirement` %in% descript,])
OTA_data$MCDCcovid<-FALSE
OTA_data$MCDCcovid[OTA_data$`Description of Requirement` %in% descript]<-TRUE

OTA_data_current %>% group_by(MCDCcovid,`Fiscal Year`) %>% dplyr::summarize(n=length(`Fiscal Year`),
                                                                            o=sum(`Dollars Obligated`))
OTA_data %>% group_by(MCDCcovid,`Fiscal Year`) %>% dplyr::summarize(n=length(`Fiscal Year`),
                                                                            o=sum(`Dollars Obligated`))


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
                   deflator_var="OMB20_GDP20"
)

OTA_data$dFYear<-as.Date(paste("1/1/",as.character(OTA_data$Fiscal_Year),sep=""),"%m/%d/%Y")

#Consolidate categories for Vendor Size

# OTA_data<-csis360::read_and_join(OTA_data,
#   "LOOKUP_Contractor_Size.csv",
#   by="Vendor.Size",
#   add_var="Shiny.VendorSize",
#   path="https://raw.githubusercontent.com/CSISdefense/R-scripts-and-data/master/",
#   dir="Lookups/"
# )



# classify competition
# OTA_data<-csis360::read_and_join(OTA_data,
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

# classify competition
# OTA_data<-csis360::read_and_join(OTA_data,
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
colnames(OTA_data)[colnames(OTA_data)=="PlatformPortfolio"]<-"OrigPlat"


levels(factor(OTA_data$Contracting_Agency_Name))
#Classify Product or Service Codes
OTA_data<-csis360::read_and_join_experiment(OTA_data,
                                  "ProductOrServiceCodes.csv",
                                  by=c("Product_or_Service_Code"="ProductOrServiceCode"),
                                  # replace_na_var="ProductServiceOrCode",
                                  add_var=c("PlatformPortfolio","ProductServiceOrRnDarea"),
                                  path=#"C:\\Users\\gsand\\Repositories\\Lookup-Tables\\",
                                    "https://raw.githubusercontent.com/CSISdefense/Lookup-Tables/master/",
                                  dir=""
)

OTA_data$OrigPlat<-factor(OTA_data$OrigPlat)
levels(OTA_data$OrigPlat)<-list(
  "Aircraft"="Aircraft and Drones",
  "Electronics, Comms, & Sensors"="Electronics and Communications",
  "Facilities and Construction"="Facilities and Construction",
  "Land Vehicles"="Land Vehicles",
  "Missile and Space Systems"="Missile and Space Systems"     ,
  "Other Products"="Other Products",
  "Other R&D and Knowledge Based"="Other R&D and Knowledge Based",
  "Other Services"="Other Services",
  "Space Systems"="Space Systems",
  "Unmanned"="Unmanned",
  "Ordnance and Missiles"="Weapons and Ammunition" 
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



colnames(OTA_data)[colnames(OTA_data)=="PlatformPortfolio"]<-"PSCPlatformPortfolio"
OTA_data<-read_and_join_experiment(OTA_data,
                            path="https://raw.githubusercontent.com/CSISdefense/Lookup-Tables/master/",
                            "Agency_AgencyID.csv",
                            dir="",
                            by=c("Contracting_Agency_ID"="AgencyID"),
                            add_var=c("Customer","SubCustomer","PlatformPortfolio"),#Contracting.Agency.ID
                            skip_check_var=c("PlatformPortfolio","SubCustomer"),
                            guess_max=2000)
OTA_data$PlatformPortfolio[is.na(OTA_data$PlatformPortfolio)]<-OTA_data$PSCPlatformPortfolio[is.na(OTA_data$PlatformPortfolio)]

levels(factor(OTA_data$PlatformPortfolio))
OTA_data$Dollars_Obligated_OMB20_GDP20
# View(OTA_data %>% filter(as.character(PlatformPortfolio)!=as.character(OrigPlat)) %>%
#        group_by(Product_or_Service_Code,Product_or_Service_Description,PlatformPortfolio,OrigPlat) %>% 
#        summarise(Dollars_Obligated_OMB20_GDP20=sum(Dollars_Obligated_OMB20_GDP20)))


# 
OTA_data<-replace_nas_with_unlabeled(OTA_data,"SubCustomer","Uncategorized")
OTA_data<-csis360::read_and_join_experiment(OTA_data,
                        "SubCustomer.csv",
                        by=c("Customer"="Customer","SubCustomer"="SubCustomer"),
                        add_var=c("SubCustomer.platform","SubCustomer.sum"),
                        path="https://raw.githubusercontent.com/CSISdefense/Lookup-Tables/master/",
                        dir="office/"
)

OTA_data %>% group_by(Contracting_Agency_Name ,Contracting_Agency_ID) %>% 
  filter(Customer=="Defense") %>%summarise(Dollars_Obligated_OMB20_GDP20=sum(Dollars_Obligated_OMB20_GDP20))
OTA_data$SubCustomer.OTA<-OTA_data$SubCustomer
OTA_data$SubCustomer.OTA[OTA_data$Contracting_Agency_ID=="97AE"]<-"DARPA"

# OTA_data<-csis360::read_and_join_experiment(OTA_data,
#                                              "Vehicle.csv",
#                                              by=c("Vehicle"="Vehicle.detail"),
#                                              add_var=c("Vehicle.sum","Vehicle.AwardTask"),
#                                              path="https://raw.githubusercontent.com/CSISdefense/Lookup-Tables/master/",
#                                              # path="K:/Users/Greg/Repositories/Lookup-Tables/",
#                                              dir="contract/"
# )
# 
# OTA_data<-replace_nas_with_unlabeled(OTA_data,"PlatformPortfolio")
# 
# OTA_data$PricingUCA.sum<-factor(OTA_data$PricingUCA)
# OTA_data<-replace_nas_with_unlabeled(OTA_data,"PricingUCA.sum")
# levels(OTA_data$PricingUCA.sum)<-
#   list("FFP"="FFP",
#        "Less Common"=c("Other FP","T&M/LH/FPLOE"),
#        "Incentive"="Incentive",
#        "Other CB"="Other CB",
#        "UCA"="UCA",
#        "Unclear"=c("Combination/Other","Unlabeled"))
# 
# 



# 
# OTA_data$PricingUCA<-OTA_data$PricingFee
# summary(factor(OTA_data$PricingUCA))
# OTA_data$PricingUCA[is.na(OTA_data$IsUCA)]<-NA
# OTA_data$PricingUCA[!is.na(OTA_data$IsUCA)&OTA_data$IsUCA==1]<-"UCA"



# debug(csis360::prepare_labels_and_colors)
# load("Shiny Apps/FPDS_chart_maker/2016_unaggregated_FPDS.Rda")
# 
# colnames(platpsc)[colnames(platpsc)=="SubCustomer"]<-"ContractingSubCustomer"
# 
# platpsc<-replace_nas_with_unlabeled(platpsc,"ContractingSubCustomer","Uncategorized")
# platpsc<-csis360::read_and_join_experiment(platpsc,
#                                              "SubCustomer.csv",
#                                              by=c("ContractingCustomer"="Customer","ContractingSubCustomer"="SubCustomer"),
#                                              add_var=c("SubCustomer.platform","SubCustomer.sum"),
#                                              path="https://raw.githubusercontent.com/CSISdefense/Lookup-Tables/master/",
#                                              dir="office/"
# )
# 

OTA_data$ProductServiceOrRnDarea.covid<-as.character(OTA_data$ProductServiceOrRnDarea.sum)
OTA_data$ProductServiceOrRnDarea.covid[OTA_data$MCDCcovid==TRUE]<-"R&D (Covid-19 Medical CBRN Defense Consortium)"
OTA_data$ProductServiceOrRnDarea.covid[OTA_data$ProductServiceOrRnDarea.covid=="R&D"]<-"R&D (Other)"



# set correct data types
OTA_data %<>%
  # select(-ContractingCustomer) %>%
  # select(-ClassifyNumberOfOffers) %>%
  mutate(SubCustomer.OTA = factor(SubCustomer.OTA)) %>%
  mutate(SubCustomer.sum = factor(SubCustomer.sum)) %>%
  mutate(SubCustomer.platform = factor(SubCustomer.platform)) %>%
  mutate(ProductServiceOrRnDarea = factor(ProductServiceOrRnDarea)) %>%
  mutate(PlatformPortfolio = factor(PlatformPortfolio)) %>%
  mutate(ProductServiceOrRnDarea.covid = factor(ProductServiceOrRnDarea.covid)) %>%
  # mutate(Shiny.VendorSize = factor(Shiny.VendorSize)) %>%
  mutate(ProductServiceOrRnDarea.sum = factor(ProductServiceOrRnDarea.sum))# %>%
  # mutate(Competition.sum = factor(Competition.sum)) %>%
  # mutate(Competition.effective.only = factor(Competition.effective.only)) %>%
  # mutate(Competition.multisum = factor(Competition.multisum))  %>%
  # mutate(No.Competition.sum = factor(No.Competition.sum)) %>%
  # mutate(Vehicle = factor(Vehicle)) %>%
  # mutate(PricingUCA = factor(PricingUCA))



ota_lc<-csis360::prepare_labels_and_colors(OTA_data)
ota_ck<-csis360::get_column_key(OTA_data)

levels(factor(OTA_data$Contracting_Department_ID))

save(OTA_data,ota_lc,ota_ck, file="data/semi_clean/Federal_OTA.Rda")
ota_def<-OTA_data %>% filter(Contracting_Department_ID=="9700")
save(ota_def,ota_lc,ota_ck, file="data/semi_clean/Defense_OTA.Rda")
