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

OTA_data_historic <- read_delim(
  "data_raw//OTA_all_fields_before_FY22.csv",delim = ",",
  col_names = TRUE, guess_max = 500000,na=c("NA","NULL"),skip = 2)

OTA_data_current<-rbind(OTA_data_historic,OTA_data_current)
OTA_data_current<-standardize_variable_names(OTA_data_current)#,
                                             # path="C:\\Users\\gsand\\Repositories\\Lookup-Tables\\style\\")
OTA_data_current$Date_Signed<-as.Date(OTA_data_current$Date_Signed,"%m/%d/%y")
OTA_data_current$fiscal_quarter<- quarter(OTA_data_current$Date_Signed,fiscal_start=10)
# 
# OTA_data_current$ProductOrServiceCode[OTA_data_current$ProductOrServiceCode=="7.00E+21"]<-"7E21"


OTA_data_current<-apply_standard_lookups(OTA_data_current)

# colnames(OTA_data)<-gsub(" ","_",colnames(OTA_data))


OTA_data<-OTA_data_current

#### Standard additions ####

if("PlatformPortfolio" %in% colnames(OTA_data)){
  colnames(OTA_data)[colnames(OTA_data)=="PlatformPortfolio"]<-"OrigPlat"
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
  
}

levels(factor(OTA_data$ContractingAgencyName))
#Classify Product or Service Codes

# write.csv(colnames(OTA_data),"OTA_names.csv")

OTA_data<-OTA_data %>% mutate(Fiscal_Year_gt_2020=ifelse(Fiscal_Year>2020,1,0))
OTA_data<-csis360::read_and_join_experiment(OTA_data,
                                            "PSCAtransition.csv",
                                            dir="ProductOrService/",
                                            by=c("ProductOrServiceCode"="ProductOrServiceCode",
                                                 "Fiscal_Year_gt_2020"="Fiscal_Year_gt_2020"),
                                            add_var=c("PlatformPortfolio"),
                                                                              path=#"C:\\Users\\gsand\\Repositories\\Lookup-Tables\\",
                                    "https://raw.githubusercontent.com/CSISdefense/Lookup-Tables/master/",
                                            skip_check_var = c("PlatformPortfolio"),
)
colnames(OTA_data)[colnames(OTA_data)=="PlatformPortfolio"]<-"TransitionPlatformPortfolio"




OTA_data<-csis360::read_and_join_experiment(OTA_data,
                                  "ProductOrServiceCodes.csv",
                                  by=c("ProductOrServiceCode"="ProductOrServiceCode"),
                                  # replace_na_var="ProductServiceOrCode",
                                  add_var=c("PlatformPortfolio"),
                                  path=#"C:\\Users\\gsand\\Repositories\\Lookup-Tables\\",
                                    "https://raw.githubusercontent.com/CSISdefense/Lookup-Tables/master/",
                                  dir="",
                                  skip_check_var =  "PlatformPortfolio"
)

OTA_data$PlatformPortfolio[is.na(OTA_data$PlatformPortfolio)]<-
  OTA_data$TransitionPlatformPortfolio[is.na(OTA_data$PlatformPortfolio)]

#Manual check because we have to draw from two sources
NA.check.OTA_data<-subset(OTA_data, is.na(PlatformPortfolio)&!is.na(ProductOrServiceCode),
                          select=c("ProductOrServiceCode","TransitionPlatformPortfolio","PlatformPortfolio"))
if(nrow(NA.check.OTA_data)>0){
  print(unique(NA.check.OTA_data))
  stop(paste(nrow(NA.check.OTA_data),"rows of NAs generated in PlatformPortfolio"))
}

OTA_data<-OTA_data %>% dplyr::select(-Fiscal_Year_gt_2020,-TransitionPlatformPortfolio)

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

# View(OTA_data %>% filter(as.character(PlatformPortfolio)!=as.character(OrigPlat)) %>%
#        group_by(Product_or_Service_Code,Product_or_Service_Description,PlatformPortfolio,OrigPlat) %>% 
#        summarise(Dollars_Obligated_OMB20_GDP20=sum(Dollars_Obligated_OMB20_GDP20)))


# 
# OTA_data<-replace_nas_with_unlabeled(OTA_data,"SubCustomer","Uncategorized")
# summary(factor(OTA_data$SubCustomer)

OTA_data %>% group_by(ContractingAgencyName ,Contracting_Agency_ID) %>% 
  filter(Customer=="Defense") %>%summarise(Action_Obligation_OMB25_GDP23=sum(Action_Obligation_OMB25_GDP23))
OTA_data$SubCustomer.OTA<-OTA_data$SubCustomer
OTA_data$SubCustomer.OTA[OTA_data$Contracting_Agency_ID=="97AE"]<-"DARPA"
OTA_data$SubCustomer.OTA[OTA_data$Contracting_Agency_ID=="97F5"]<-"WHS"
OTA_data$SubCustomer.OTA[OTA_data$SubCustomer  %in% c("DLA","DISA")]<-"Other DoD"

OTA_data$SubCustomer.OTA.sum<-factor(OTA_data$SubCustomer.OTA)
levels(OTA_data$SubCustomer.OTA.sum)<-list(
    "Army"="Army",
    "Navy"="Navy",
    "Air Force"="Air Force",
    "SDA"="SDA",
    "MDA"="MDA",
    "DARPA"="DARPA",           
    "Other DoD"=c("MilitaryHealth","WHS","Other DoD"),
    "Interior"="Interior",
    "DHS"=c("OPO","TSA")
)

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

#### Covid Labeling ####


OTA_data_current$MentionsCovid<-TRUE
OTA_data_current$TopCovid<-FALSE

# OTA_data$TopCovid<-FALSE


OTA_data_current %>% group_by(TopCovid,Fiscal_Year) %>% dplyr::summarize(n=length(Fiscal_Year),
                                                                         o=sum(Action_Obligation_OMB25_GDP23))


# OTA_data_current$MentionsCovid[grep("COVID-19",OTA_data_current$Description_of_Requirement,invert=TRUE)]<-FALSE
OTA_data_current$TopCovid[OTA_data_current$PIID %in% c("W15QKN1691002",
                                                       "W15QKN2191003",
                                                       "W911QY2190001",
                                                       "W911QY2190002",
                                                       "W911QY2190003",
                                                       "W911NF2190001",
                                                       "W911NF2190003",
                                                       "W912CG2190001") & 
                            OTA_data_current$MentionsCovid]<-TRUE

summary(OTA_data_current$TopCovid)
View(OTA_data_current$Description_of_Requirement[OTA_data_current$TopCovid==TRUE])
descript<-OTA_data_current$Description_of_Requirement[OTA_data_current$TopCovid==TRUE]
# OTA_data$TopCovid[OTA_data$Description_of_Requirement %in% descript]<-TRUE

nrow(OTA_data[OTA_data$Description_of_Requirement %in% descript,])

OTA_data$ProductServiceOrRnDarea.covid<-as.character(OTA_data$SimpleArea)
OTA_data$ProductServiceOrRnDarea.covid[OTA_data$TopCovid==TRUE]<-"R&D (Top OTAs responding to Covid-19)"
OTA_data$ProductServiceOrRnDarea.covid[OTA_data$ProductServiceOrRnDarea.covid=="R&D"]<-"R&D (Other)"

#### Remotely Crewed labeling ####
# sum( text_to_number(OTA_data_current$Action_Obligation_OMB25_GDP23[grep("UNMANNED",OTA_data_current$Description_of_Requirement)]),na.rm=TRUE)

OTA_data$IsRemotelyOperated<-FALSE
OTA_data$IsRemotelyOperated[OTA_data$ProductOrServiceCode=="1550"]<-TRUE
sum(OTA_data$Action_Obligation_OMB25_GDP23[OTA_data$IsRemotelyOperated],na.rm=TRUE)



OTA_data<-read_and_join_experiment(OTA_data,
                                           path="data_raw//",dir="",lookup_file = "OTA_descrip_row_number.csv",
                                           add_var="descrip_row_number",
                                           skip_check_var = "descrip_row_number",
                                           by="Description_of_Requirement")

OTA_data_current<-read_and_join_experiment(OTA_data,
                                           path="data//semi_clean//",dir="",lookup_file = "ota_description_UAS.csv",
                                           add_var="Remotely_Crewed",
                                           skip_check_var = "Remotely_Crewed",
                                           by="descrip_row_number",
                                           col_types = "cnccccccccc")


OTA_data$IsRemotelyOperated[OTA_data$Remotely_Crewed %in% c("UAS","UAS/C-UAS")]<-TRUE
sum(OTA_data$Action_Obligation_OMB25_GDP23[OTA_data$IsRemotelyOperated],na.rm=TRUE)

OTA_data$ReferencedIDVAgencyID[is.na(OTA_data$ReferencedIDVAgencyID)]<-""
OTA_data<-read_and_join_experiment(OTA_data,
                                           path="data//semi_clean//",dir="",lookup_file = "ota_CAU_UAS.csv",
                                           add_var="Remotely_Crewed_CAU",
                                           skip_check_var = "Remotely_Crewed_CAU",
                                           by=c("PIIDAgencyID","PIID","ReferencedIDVAgencyID","Referenced_IDV_PIID"),col_types="dcccc")

summary(factor(OTA_data$Remotely_Crewed_CAU))


OTA_data$IsRemotelyOperated[OTA_data$Remotely_Crewed_CAU %in% c("UAS","UAS/CUAS")]<-TRUE

sum(OTA_data$Action_Obligation_OMB25_GDP23[OTA_data$IsRemotelyOperated],na.rm=TRUE)

OTA_data$PlatformPortfolioUAV<-as.character(OTA_data$PlatformPortfolio)
OTA_data$PlatformPortfolioUAV[OTA_data$IsRemotelyOperated==TRUE]<-"Remotely Crewed"


OTA_data$TypeOfAgreement<-factor(OTA_data$TypeOfAgreement)
levels(OTA_data$TypeOfAgreement)<-list("Prototype"=c("PROTOTYPE","Prototype"),
                                      "Production"=c("PRODUCTION","PRODUCTIO","Production"))


#### Final Cleanup ####
# set correct data types
OTA_data %<>%
  # select(-ContractingCustomer) %>%
  # select(-ClassifyNumberOfOffers) %>%
  mutate(SubCustomer.OTA = factor(SubCustomer.OTA)) %>%
  mutate(SubCustomer.OTA.sum = factor(SubCustomer.OTA.sum)) %>%
  mutate(SubCustomer.sum = factor(SubCustomer.sum)) %>%
  mutate(SubCustomer.platform = factor(SubCustomer.platform)) %>%
  mutate(ProductServiceOrRnDarea = factor(ProductServiceOrRnDarea)) %>%
  mutate(PlatformPortfolio = factor(PlatformPortfolio)) %>%
  mutate(PlatformPortfolioUAV = factor(PlatformPortfolioUAV)) %>%
  mutate(ProductServiceOrRnDarea.covid = factor(ProductServiceOrRnDarea.covid)) %>%
  # mutate(Shiny.VendorSize = factor(Shiny.VendorSize)) %>%
  mutate(SimpleArea = factor(SimpleArea))# %>%
  # mutate(Competition.sum = factor(Competition.sum)) %>%
  # mutate(Competition.effective.only = factor(Competition.effective.only)) %>%
  # mutate(Competition.multisum = factor(Competition.multisum))  %>%
  # mutate(No.Competition.sum = factor(No.Competition.sum)) %>%
  # mutate(Vehicle = factor(Vehicle)) %>%
  # mutate(PricingUCA = factor(PricingUCA))

str(OTA_data$ContractingOfficeName)
OTA_data$MajorCommandName[OTA_data$MajorCommandName==""]<-"Unlabeled"

ota_lc<-csis360::prepare_labels_and_colors(OTA_data %>% select(-ContractingOfficeName,-MajorCommandName),
                                           path=file.path(get_local_lookup_path(),"style\\"))
                                           # path="C:\\Users\\gsand\\Repositories\\Lookup-Tables\\style\\")
ota_ck<-csis360::get_column_key(OTA_data)#,
                                # path="C:\\Users\\gsand\\Repositories\\Lookup-Tables\\style\\")


save(OTA_data,ota_lc,ota_ck, file="data/clean/Federal_OTA.Rda")
ota_def<-OTA_data %>% filter(DepartmentID=="9700")
save(ota_def,ota_lc,ota_ck, file="data/clean/Defense_OTA.Rda")


# load(file="data/semi_clean/Defense_OTA.Rda")
if(!exists("fed_data")){
  load(file=file.path("data","clean","fed_summary_FPDS.rda"))
}


colnames(OTA_data)[colnames(OTA_data)=="Non-traditionalGovernmentContractorParticipationDescription"]<-
  "NontraditionalGovernmentContractorParticipationDescription"
colnames(OTA_data)[colnames(OTA_data)=="Non-traditionalGovernmentContractorParticipationCode"]<-
  "NontraditionalGovernmentContractorParticipationCode"

ota_contract<-OTA_data
ota_contract$IsOTA<-"OTA"
fed_data$IsOTA<-"Contract"


summary(factor(OTA_data$NontraditionalGovernmentContractorParticipationDescription))
colnames(ota_contract)[colnames(ota_contract)=="Customer"]<-"ContractingCustomer"


summary(factor(OTA_data$NontraditionalGovernmentContractorParticipationCode))

summary(factor(ota_contract$AnyCommercial))
ota_contract$AnyCommercial<-factor(ota_contract$NontraditionalGovernmentContractorParticipationCode)
levels(ota_contract$AnyCommercial)=list(
  "Y"="NSP",
  "NonDev"="NonDev",
  "Other OTA"=c("CS","DEC"),
  "N"="N"
)



ota_contract<-rbind(ota_contract[,colnames(ota_contract)[colnames(ota_contract) %in% colnames(fed_data)]],
                    fed_data[,colnames(fed_data)[colnames(fed_data) %in% colnames(ota_contract)]])
# platpscintldef$IsOTA<-"Contract"

ota_contract$SubCustomer.sum<-as.character(ota_contract$SubCustomer.sum)
ota_contract$SubCustomer.sum[ota_contract$ContractingCustomer!="Defense"]<-"Civilian"


# ota_contract<-rbind(ota_contract[,colnames(ota_contract)[colnames(ota_contract) %in% colnames(platpscintldef)]],
#                     platpscintldef[,colnames(platpscintldef)[colnames(platpscintldef) %in% colnames(ota_contract)]])
save(ota_contract,ota_lc,ota_ck, file="data/clean/OTA_contract.Rda")

summary(fed_data$PlatformPortfolioUAV)


if(!exists("def_data"))
  load(file="analysis/FPDS_chart_maker/unaggregated_def.Rda")

def_kota<-ota_def
def_kota$IsOTA<-"OTA"
def_data$IsOTA<-"Contract"
def_data$AreaType<-paste("Contract for",def_data $SimpleArea)
def_kota$AreaType<-paste("OTA for",def_kota$TypeOfAgreement)




def_kota<-rbind(def_kota[,colnames(def_kota)[colnames(def_kota) %in% colnames(def_data)]],
               def_data[,colnames(def_data)[colnames(def_data) %in% colnames(def_kota)]])%>%
  filter(Fiscal_Year>=2015)


def_kota$AreaType<-factor(def_kota$AreaType)
levels(def_kota$AreaType)<-list(
  "OTA for Production"="OTA for Production",
  "Contract for Products"="Contract for Products (All)",
  "OTA for Prototype"="OTA for Prototype" ,
  "Contract for R&D"="Contract for R&D",
  "Contract for Services"="Contract for Services (Non-R&D)",
  "Unlabeled"=c("Contract for Unlabeled","OTA for NA")
)
def_kota$SimpleAreaType<-def_kota$AreaType
levels(def_kota$SimpleAreaType)<-list(
  "Products and Production"=c("Contract for Products",
                              "OTA for Production"),
  "R&D and Prototypes"=c("Contract for R&D",
                         "OTA for Prototype"),
  "Services"="Contract for Services",
  "Unlabeled"="Unlabeled")

def_kota<-def_kota %>% mutate(
  SimpleAreaType=factor(SimpleAreaType),
  AreaType=factor(AreaType)
)

summary(factor(def_kota$AreaType))

kota_ck<-get_column_key(def_kota,path="offline")
kota_lc<-prepare_labels_and_colors(def_kota,path="offline")
save(def_kota,kota_lc,kota_ck, file="data/clean/def_kota.Rda")
