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
library(readr)
#This is a kludge until the FMS repo is public
source(file.path("..","Trade","Scripts","Trade_Standardize.r"))
# read in data
local_path<-get_local_lookup_path()

initial_clean<-function(df,only_defense=TRUE){
  df<-standardize_variable_names(df)
  # colnames(df)[colnames(df)=="Fiscal.Year"]<-"Fiscal_Year"
  # coerce Amount to be a numeric variable
  # if("Action_Obligation" %in% colnames(df)) 
  #   df$Action_Obligation %<>% text_to_number()
  # if("NumberOfActions" %in% colnames(df)) 
  #   df$NumberOfActions %<>% text_to_number()
  # df$Fiscal_Year <- text_to_number(df$Fiscal_Year)
  colnames(df)[colnames(df)=="Customer"]<-"ContractingCustomer"
  # colnames(df)[colnames(df)=="platformportfolio"]<-"PlatformPortfolio"
  # discard pre-1990
  df %<>% filter(Fiscal_Year >= 1990) #Fiscal_Year >= 2000 & 
  if(only_defense)
    df %<>% filter(Fiscal_Year >= 1990 & ContractingCustomer=="Defense") #Fiscal_Year >= 2000 & 
  # colnames(df)[colnames(df)=="Action_Obligation_Then_Year"]<-"Action_Obligation"
  # df$dFYear<-as.Date(paste("1/1/",as.character(df$Fiscal_Year),sep=""),"%m/%d/%Y")
  
  df
}

# full_data %>% filter(fiscal_year==2021) %>% summarise(o=sum(SumOfobligatedAmount))

# full_data <- read_delim(
#   "Data//semi_clean//Federal_Summary.SP_CompetitionVendorSizeHistoryBucketPlatformSubCustomerpcau.txt",delim = "\t",
#   col_names = TRUE, guess_max = 2000000,na=c("NA","NULL"))



#############Full Data and Fed Data##########

full_data <- read_delim(
  # "Data//semi_clean//Defense_Summary.SP_CompetitionVendorSizeHistoryBucketPlatformSubCustomerLength.txt",
  # "Data//semi_clean//Federal_Summary.SP_CompetitionVendorSizeHistoryBucketPlatformSubCustomerInternational.txt",
  # "Data//semi_clean//Federal_Summary.SP_CompetitionVendorSizeHistoryBucketPlatformSubCustomer.txt",
  "Data//semi_clean//Federal_Budget.SP_CompetitionVendorSizeHistoryBucketPlatformSubCustomerFMS.txt",
  # "Data//semi_clean//Defense_Budget.SP_CompetitionVendorSizeHistoryBucketPlatformSubCustomerFMS.txt",
  delim = "\t",
  col_names = TRUE, guess_max = 2000000,na=c("NA","NULL"))

colnames(full_data)[colnames(full_data)=="UnmodifiedUltimateDurationCategory...13"]<-"UnmodifiedUltimateDurationCategory"
colnames(full_data)[colnames(full_data)=="CurrentDurationCategory...14"]<-"CurrentDurationCategory"

# full_data<-initial_clean(full_data,only_defense=FALSE)
fed_data<- apply_standard_lookups(full_data)#,
full_data<-initial_clean(fed_data,only_defense=TRUE)
# path="K:/Users/Greg/Repositories/Lookup-Tables/style")

if(!"OriginIsForeign" %in% colnames(full_data) & "OriginISOalpha3" %in% colnames(full_data) ){
  full_data<-read_and_join_experiment(full_data,lookup_file="Location_CountryCodes.csv",
                                      path="https://raw.githubusercontent.com/CSISdefense/Lookup-Tables/master/",dir="location/",
                                      add_var = c("isforeign"),#"USAID region",
                                      by=c("OriginISOalpha3"="alpha-3"),
                                      # skip_check_var=c("NATOyear",	"MajorNonNATOyear","NTIByear"	,"SEATOendYear","RioTreatyStartYear","RioTreatyEndYear","FiveEyes","OtherTreatyName"	,"OtherTreatyStartYear","OtherTreatyEndYear","isforeign"),
                                      missing_file="missing_DSCA_iso.csv")
  colnames(full_data)[colnames(full_data)=="isforeign"]<-"OriginIsForeign"
  
  full_data <- full_data %>% dplyr::select(-PlaceIsForeign,-VendorIsForeign) 
  full_data %<>% add_alliance(isoAlpha3_col= "PlaceISOalpha3", drop_col = TRUE,prefix="Place")
  full_data$VendorISOalpha3[full_data$VendorISOalpha3=="~NJ"]<-NA
  full_data %<>% add_alliance(isoAlpha3_col= "VendorISOalpha3", drop_col = TRUE,prefix="Vendor")
  full_data %<>% add_alliance(isoAlpha3_col= "OriginISOalpha3", drop_col = TRUE,prefix="Origin")
  
  
  full_data$VendorSize_Intl<-factor(full_data$Shiny.VendorSize)
  levels(full_data$VendorSize_Intl)<-list(
    "Unlabeled"="Unlabeled",
    "International"="International",
    "U.S. Big Five"=c("Big Five","U.S. Big Five"),
    "U.S. Large"=c("Large","U.S. Large"),
    "U.S. Medium"=c("Medium","U.S. Medium"),
    "U.S. Small"=c("Small","U.S. Small")
  )
  full_data$VendorSize_Intl[full_data$VendorIsForeign==1]<-"International"
  full_data$VendorSize_Intl[is.na(full_data$VendorIsForeign)]<-"Unlabeled"
}

if(all(is.na(full_data[nrow(full_data),]))){
  full_data<-full_data[1:nrow(full_data)-1,]
  warning("Echo row dropped")
}

# if("ContractingCustomer" %in% colnames(full_data))
# full_data %<>%  select(-ContractingCustomer)
# set correct data types
full_data %<>%
  # select(-ClassifyNumberOfOffers) %>%
  mutate(ContractingSubCustomer = factor(ContractingSubCustomer)) %>%
  mutate(SubCustomer.platform = factor(SubCustomer.platform)) %>%
  mutate(ProductServiceOrRnDarea = factor(ProductServiceOrRnDarea)) %>%
  mutate(PlatformPortfolio = factor(PlatformPortfolio)) %>%
  # mutate(PlatformPortfolioUAV = factor(PlatformPortfolioUAV)) %>%
  mutate(Shiny.VendorSize = factor(Shiny.VendorSize)) %>%
  mutate(SimpleArea = factor(SimpleArea)) %>%
  mutate(Competition.sum = factor(Competition.sum)) %>%
  mutate(Competition.effective.only = factor(Competition.effective.only)) %>%
  mutate(Competition.multisum = factor(Competition.multisum))  %>%
  mutate(No.Competition.sum = factor(No.Competition.sum)) %>%
  mutate(Vehicle = factor(Vehicle)) %>%
  mutate(Vehicle.sum = factor(Vehicle.sum)) %>%
  mutate(Vehicle.sum7 = factor(Vehicle.sum7)) %>%
  mutate(Vehicle.AwardTask = factor(Vehicle.AwardTask)) %>%
  mutate(PricingUCA = factor(PricingUCA)) %>%
  mutate(IsFMS = factor(IsFMS)) %>%
  mutate(PlaceOfManufacture_Sum = factor(PlaceOfManufacture_Sum)) %>%
  mutate(VendorIsForeign = factor(VendorIsForeign))%>%
  mutate(PlaceIsForeign = factor(PlaceIsForeign))


labels_and_colors<-csis360::prepare_labels_and_colors(full_data %>% select(-recoveredmaterialclauses))

column_key<-csis360::get_column_key(full_data)

save(full_data,labels_and_colors,column_key, file="analysis/FPDS_chart_maker/unaggregated_FPDS.Rda")

fpds_lc<-csis360::prepare_labels_and_colors(full_data %>% select(-recoveredmaterialclauses))

fpds_ck<-csis360::get_column_key(full_data)

fpds_data<-full_data %>% filter(ContractingCustomer=="Defense"&
                                  Fiscal_Year>=2010)

save(fpds_data,fpds_lc, fpds_ck,file = "..\\FMS\\data\\clean\\fpds_transaction_summary.rda")
rm(fpds_data)

fed_data %<>%
  # select(-ClassifyNumberOfOffers) %>%
  mutate(ContractingSubCustomer = factor(ContractingSubCustomer)) %>%
  mutate(SubCustomer.platform = factor(SubCustomer.platform)) %>%
  mutate(ProductServiceOrRnDarea = factor(ProductServiceOrRnDarea)) %>%
  mutate(PlatformPortfolio = factor(PlatformPortfolio)) %>%
  mutate(PlatformPortfolioUAV = factor(PlatformPortfolioUAV)) %>%
  mutate(Shiny.VendorSize = factor(Shiny.VendorSize)) %>%
  mutate(SimpleArea = factor(SimpleArea)) %>%
  mutate(Competition.sum = factor(Competition.sum)) %>%
  mutate(Competition.effective.only = factor(Competition.effective.only)) %>%
  mutate(Competition.multisum = factor(Competition.multisum))  %>%
  mutate(No.Competition.sum = factor(No.Competition.sum)) %>%
  mutate(Vehicle = factor(Vehicle)) %>%
  mutate(Vehicle.sum = factor(Vehicle.sum)) %>%
  mutate(Vehicle.sum7 = factor(Vehicle.sum7)) %>%
  mutate(Vehicle.AwardTask = factor(Vehicle.AwardTask)) %>%
  mutate(PricingUCA = factor(PricingUCA)) %>%
  mutate(IsFMS = factor(IsFMS)) %>%
  mutate(PlaceOfManufacture_Sum = factor(PlaceOfManufacture_Sum)) %>%
  mutate(VendorIsForeign = factor(VendorIsForeign))%>%
  mutate(PlaceIsForeign = factor(PlaceIsForeign))


fed_lc<-csis360::prepare_labels_and_colors(full_data)

fed_ck<-csis360::get_column_key(full_data)

save(fed_data,fed_lc,fed_ck, file="data/clean/fed_summary_FPDS.Rda")


#############Defense Data (faster query run)##########
def_data<- read_delim(
  "Data//semi_clean//Summary.SP_CompetitionVendorSizeHistoryBucketPlatformSubCustomerLength.csv",delim = "\t",
  col_names = TRUE, guess_max = 2000000,na=c("NA","NULL"))
problems(def_data[nrow(def_data),])

def_data<-initial_clean(def_data,only_defense = TRUE)
def_data<-apply_standard_lookups(def_data)#,

#def_data
def_data %<>%
  # select(-ClassifyNumberOfOffers) %>%
  mutate(ContractingSubCustomer = factor(ContractingSubCustomer)) %>%
  mutate(SubCustomer.platform = factor(SubCustomer.platform)) %>%
  mutate(SubCustomer.JPO = factor(SubCustomer.JPO)) %>%
  mutate(ProductServiceOrRnDarea = factor(ProductServiceOrRnDarea)) %>%
  mutate(PlatformPortfolio = factor(PlatformPortfolio)) %>%
  mutate(Shiny.VendorSize = factor(Shiny.VendorSize)) %>%
  mutate(SimpleArea = factor(SimpleArea)) %>%
  mutate(Competition.sum = factor(Competition.sum)) %>%
  mutate(Competition.effective.only = factor(Competition.effective.only)) %>%
  mutate(Competition.multisum = factor(Competition.multisum))  %>%
  mutate(No.Competition.sum = factor(No.Competition.sum)) %>%
  mutate(Vehicle = factor(Vehicle)) %>%
  mutate(Vehicle.sum = factor(Vehicle.sum)) %>%
  mutate(Vehicle.sum7 = factor(Vehicle.sum7)) %>%
  mutate(Vehicle.AwardTask = factor(Vehicle.AwardTask)) %>%
  mutate(fiscal_quarter = factor(fiscal_quarter)) %>%
  mutate(PricingUCA = factor(PricingUCA)) #%>%
# mutate(IsFMS = factor(IsFMS)) %>%
# mutate(PlaceOfManufacture_Sum = factor(PlaceOfManufacture_Sum)) %>%
# mutate(VendorIsForeign = factor(VendorIsForeign))%>%
# mutate(PlaceIsForeign = factor(PlaceIsForeign))

def_data$Fiscal_YQ<-NA
def_data$Fiscal_YQ[!is.na(def_data$fiscal_quarter)]<-text_to_number(paste(def_data$Fiscal_Year[!is.na(def_data$fiscal_quarter)],
                                         text_to_number(def_data$fiscal_quarter[!is.na(def_data$fiscal_quarter)]),sep="."))
def_data$Fiscal_YQ[is.na(def_data$Fiscal_YQ)]<-def_data$Fiscal_Year[is.na(def_data$Fiscal_YQ)]

def_data$PricingInflation.1yearUCA<-as.character(def_data$PricingInflation.1year)
def_data$PricingInflation.1yearUCA[def_data$PricingUCA.sum=="UCA"]<-"UCA"
def_data$PricingMechanism<-as.character(def_data$PricingMechanism)
def_lc<-csis360::prepare_labels_and_colors(def_data, path=file.path(local_path,"style\\"))

def_ck<-csis360::get_column_key(def_data %>% select(-PricingMechanism),path=file.path(local_path,"style\\"))

save(def_data,def_lc,def_ck, file="analysis/FPDS_chart_maker/unaggregated_def.Rda")
# load(file="analysis/FPDS_chart_maker/unaggregated_def.Rda")


###########Product Service Code, Agency, Platform ############

platpscdefcd<-read_delim(file.path("data","semi_clean","Location.SP_ProdServPlatformAgencyCongressionalDistrict.csv"),delim="\t",na=c("NULL","NA"),
                    col_names = TRUE, guess_max = 10000000)
platpscdefcd<-apply_standard_lookups(platpscdefcd)

platpscdefcd<-initial_clean(platpscdefcd)

platpsc<-read_delim(file.path("data","semi_clean","Federal_ProdservPlatform.txt"),delim="\t",na=c("NULL","NA"),
                    col_names = TRUE, guess_max = 10000000)

platpsc<-initial_clean(platpsc)
platpsc<-apply_standard_lookups(platpsc)



platpsc %<>%
  # select(-ContractingCustomer) %>%
  # select(-ClassifyNumberOfOffers) %>%
  mutate(SubCustomer = factor(SubCustomer)) %>%
  mutate(SubCustomer.platform = factor(SubCustomer.platform)) %>%
  # mutate(SubCustomer.JPO = factor(SubCustomer.JPO)) %>%
  mutate(ProductServiceOrRnDarea = factor(ProductServiceOrRnDarea)) %>%
  mutate(PlatformPortfolio = factor(PlatformPortfolio)) %>%
  mutate(CrisisProductOrServiceArea = factor(CrisisProductOrServiceArea))



detail_lc<-csis360::prepare_labels_and_colors(platpsc)
detail_ck<-csis360::get_column_key(platpsc)

save(platpsc,detail_lc,detail_ck, file="data/clean/platpsc_FPDS.Rda")


platpscintl<-read_delim(file.path("data","semi_clean","Federal_Location.SP_ProdServPlatformAgencyPlaceOriginVendor.txt"),delim="\t",na=c("NULL","NA"),
                        col_names = TRUE, guess_max = 10000000)
problems(platpscintl)
colnames(platpscintl)[colnames(platpscintl)=="Customer"]<-"ContractingCustomer"

#Weird kludge for duplicate
if(colnames(platpscintl)[1]=="productorservicecode" & colnames(platpscintl)[5]=="ProductOrServiceCode"){
  platpscintl<-platpscintl %>% select(-productorservicecode)
}
# View(platpscintl[(nrow(platpscintl)-3):nrow(platpscintl),])
# View(platpscintldef[(nrow(platpscintldef)-3):nrow(platpscintldef),])
# debug(initial_clean)
platpscintl<-apply_standard_lookups(platpscintl)#,path=local_path
platpscintl<-initial_clean(platpscintl)
platpscintldef<-initial_clean(platpscintl,only_defense=FALSE)

write.csv(platpscintldef %>% filter(PlatformPortfolio=="Ordnance and Missiles"), 
          file="Output//Munitions//OM.csv",
          row.names = FALSE)

summary(platpscintldef$PlatformPortfolio)

write.csv(platpscintldef %>% filter(PlatformPortfolio=="Other Products"&SubCustomer=="Army"), 
          file="Output//Munitions//ArmyOtherProducts.csv",
          row.names = FALSE)

# n<-platpscintl %>% group_by(IsFMS,IsFMSmac,IsFMSml,fundedbyforeignentity) %>%
#   summarise(n=length(fiscal_year),min=min(fiscal_year),max=max(fiscal_year))


#Vendor Size
# platpscintldef$VendorSize_Intl<-factor(platpscintldef$Shiny.VendorSize)
# levels(platpscintldef$VendorSize_Intl)<-list(
#   "Unlabeled"="Unlabeled",
#   "International"="International",
#   "U.S. Big Five"=c("Big Five","U.S. Big Five"),
#   "U.S. Large"=c("Large","U.S. Large"),
#   "U.S. Medium"=c("Medium","U.S. Medium"),
#   "U.S. Small"=c("Small","U.S. Small")
# )
# platpscintldef$VendorSize_Intl[platpscintldef$VendorIsForeign==1]<-"International"
# platpscintldef$VendorSize_Intl[is.na(platpscintldef$VendorIsForeign)]<-"Unlabeled"

#foreign_funding_description



platpscintldef %<>% read_and_join_experiment(lookup_file="Budget_FundedByForeignEntity.csv",
                                             path="https://raw.githubusercontent.com/CSISdefense/Lookup-Tables/master/",dir="budget/",
                                             add_var = c("foreign_funding_description","IsFMS"),
                                             by=c("fundedbyforeignentity"),
                                             # missing_file="missing_iso.csv",
                                             skip_check_var = c("foreign_funding_description","IsFMS")
)



platpscintldef$IsJSF[platpscintldef$ProjectID==87]<-"JSF (F-35)"
platpscintldef$IsJSF[!is.na(platpscintldef$ProjectID)&platpscintldef$ProjectID!=87&platpscintldef$IsUnknown==0]<-"Other Project"
platpscintldef$IsJSF[is.na(platpscintldef$IsUnknown)|platpscintldef$IsUnknown==1]<-"Unknown Project"
# platpscintldef$IsJSF[is.na(platpscintldef$IsJSF)]
summary(factor(platpscintldef$IsJSF))

platpscintldef$IsFMSplaceIntl<-paste(platpscintldef$IsFMS,platpscintldef$PlaceIsForeign,sep="\n")
summary(factor(platpscintldef$IsFMSplaceIntl))
platpscintldef$IsFMSplaceIntl<-factor(platpscintldef$IsFMSplaceIntl)
levels(platpscintldef$IsFMSplaceIntl)=list(
  "Includes FMS\nPerformed Internationally"="1\n1",
  "Includes FMS\nPerformed Domestically"="1\n0",
  "No FMS\nPerformed Internationally"="0\n1",
  "No FMS\nPerformed Domestically"="0\n0",
  "Unlabeled"=c("0\nNA","1\nNA",   "NA\n0" ,  "NA\n1"  ,"NA\nNA" )
)













# 
# full_data$PricingUCA<-full_data$PricingFee
# summary(factor(full_data$PricingUCA))
# full_data$PricingUCA[is.na(full_data$IsUCA)]<-NA
# full_data$PricingUCA[!is.na(full_data$IsUCA)&full_data$IsUCA==1]<-"UCA"



# debug(csis360::prepare_labels_and_colors)
# load("Shiny Apps/FPDS_chart_maker/2016_unaggregated_FPDS.Rda")




platpscintl %<>%
  # select(-ContractingCustomer) %>%
  # select(-ClassifyNumberOfOffers) %>%
  mutate(SubCustomer = factor(SubCustomer)) %>%
  mutate(SubCustomer.platform = factor(SubCustomer.platform)) %>%
  mutate(SubCustomer.JPO = factor(SubCustomer.JPO)) %>%
  mutate(ProductServiceOrRnDarea = factor(ProductServiceOrRnDarea)) %>%
  mutate(PlatformPortfolio = factor(PlatformPortfolio)) %>%
  mutate(PlatformPortfolioUAV = factor(PlatformPortfolioUAV)) %>%
  mutate(CrisisProductOrServiceArea = factor(CrisisProductOrServiceArea))



platpscintldef %<>%
  # select(-ContractingCustomer) %>%
  # select(-ClassifyNumberOfOffers) %>%
  mutate(SubCustomer = factor(SubCustomer)) %>%
  mutate(SubCustomer.platform = factor(SubCustomer.platform)) %>%
  mutate(SubCustomer.JPO = factor(SubCustomer.JPO)) %>%
  mutate(ProductServiceOrRnDarea = factor(ProductServiceOrRnDarea)) %>%
  mutate(PlatformPortfolio = factor(PlatformPortfolio)) %>%
  mutate(PlatformPortfolioUAV = factor(PlatformPortfolioUAV))



intl_lc<-csis360::prepare_labels_and_colors(platpscintldef)
intl_ck<-csis360::get_column_key(platpscintldef)

save(platpscintldef,intl_lc, intl_ck,file="data/clean/platpscintl_FPDS.Rda")

fed_lc<-csis360::prepare_labels_and_colors(platpscintl)
fed_ck<-csis360::get_column_key(platpscintl)

save(platpscintl,fed_lc, fed_ck,file="data/clean/Federal_platpscintl_FPDS.Rda")

##############Software #############
sw<-read_delim(file.path("data","semi_clean","Summary.SP_SoftwareDetail.txt"),delim="\t",na=c("NULL","NA"),
               col_names = TRUE, guess_max = 10000000)



sw<-apply_standard_lookups(sw)

sw_lc<-prepare_labels_and_colors(sw)
sw_ck<-get_column_key(sw)

save(sw,sw_lc,sw_ck, file="data/clean/sw_FPDS.Rda")

#############JADC2##########

jadc2 <- read_delim(
  "Data//semi_clean//Summary.SP_JADC2detail.txt",delim = "\t",
  col_names = TRUE, guess_max = 2000000,na=c("NA","NULL"))

jadc2<-apply_standard_lookups(jadc2)#,


jadc2_lc<-csis360::prepare_labels_and_colors(jadc2)
jadc2_ck<-csis360::get_column_key(jadc2)
save(jadc2,jadc2_lc, jadc2_ck,file="data/clean/jadc2.Rda")







##############Pricing History 1980-2021 #############

pricing<- read_csv(
  "Data_Raw//FPDS_Reports//Defense_Pricing_Mechanism_Agency.csv",
  col_names = TRUE, guess_max = 5000000,na=c(""))

pricing<-apply_standard_lookups(pricing)#,
pricing<-standardize_variable_names(pricing)
colnames(pricing)<-gsub(" ","",colnames(pricing))
pricing<-standardize_variable_names(pricing)
colnames(pricing)[colnames(pricing)=="DollarsObligated"]<-"Action_Obligation"
colnames(pricing)[colnames(pricing)=="PricingMechanismCode"]<-"TypeOfContractPricing"
pricing$Action_Obligation<-text_to_number(pricing$Action_Obligation)
# pricing<-pricing %>% select(-PricingInflation)
pricing<-pricing %>% select(-MajorCommandName)
pricing_lc<-csis360::prepare_labels_and_colors(pricing  %>% select(-ContractingOfficeName,-PricingMechanism ))

pricing_ck<-csis360::get_column_key(pricing)

save(pricing,pricing_lc,pricing_ck, file="data/clean/pricing_historical.Rda")




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
#                       deflator_var="OMB20_GDP20"
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
# colnames(partial_2018)[colnames(partial_2018)=="ContractActions"]<-"NumberOfActions"
# 
# colnames(partial_2018)[colnames(partial_2018) %in% colnames(full_data)]
# colnames(full_data)[!colnames(full_data) %in% colnames(partial_2018)]
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
#                    NumberOfActions=sum(NumberOfActions,na.rm=TRUE))
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
# 
# 

# write.csv(full_data%>%group_by(fiscal_year)%>%
#       dplyr::summarize(Action_Obligation.Then.Year=sum(Action_Obligation.Then.Year,na.rm=TRUE),
#                                                          Action_Obligation.OMB.2019=sum(Action_Obligation.OMB.2019,na.rm=TRUE),
#                                                          NumberOfActions=sum(NumberOfActions,na.rm=TRUE)),
#       file="topline_usaspending.csv"
# )
# 
# 
# full_data <- read_delim(
#   "Data//2017_Summary.SP_CompetitionVendorSizeHistoryBucketPlatformSubCustomer.txt",delim = "\t",
#   col_names = TRUE, col_types = "cccccccccc",na=c("NA","NULL"))


# ***** Handled in apply_standard_lookups


