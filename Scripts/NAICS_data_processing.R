# Data Pre-Processing for Census data
#
# Input:
#   CSV-format/Txt results from Census website query:
#     
#
# Output: unaggregated_FPDS.Rda
################################################################################

# install.packages("../csis360_0.0.0.9022.tar.gz")

library(tidyverse)
library(magrittr)
library(csis360)
library(readr)
# read in data
local_path<-get_local_lookup_path()


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
  # full_data<-read_and_join_experiment(full_data,lookup_file="Location_CountryCodes.csv",
  #                                     path="https://raw.githubusercontent.com/CSISdefense/Lookup-Tables/master/",dir="location/",
  #                                     add_var = c("isforeign"),#"USAID region",
  #                                     by=c("OriginISOalpha3"="alpha-3"),
  #                                     # skip_check_var=c("NATOyear",	"MajorNonNATOyear","NTIByear"	,"SEATOendYear","RioTreatyStartYear","RioTreatyEndYear","FiveEyes","OtherTreatyName"	,"OtherTreatyStartYear","OtherTreatyEndYear","isforeign"),
  #                                     missing_file="missing_DSCA_iso.csv")
  # colnames(full_data)[colnames(full_data)=="isforeign"]<-"OriginIsForeign"
  
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


labels_and_colors<-prepare_labels_and_colors(full_data %>% select(-recoveredmaterialclauses))

column_key<-get_column_key(full_data)

full_data$Fiscal_YQ[!is.na(full_data$fiscal_quarter_YTD)]<-text_to_number(paste(full_data$Fiscal_Year[!is.na(full_data$fiscal_quarter_YTD)],
                                                                          text_to_number(full_data$fiscal_quarter_YTD[!is.na(full_data$fiscal_quarter_YTD)]),sep="."))
full_data$Fiscal_YQ[is.na(full_data$Fiscal_YQ)]<-full_data$Fiscal_Year[is.na(full_data$Fiscal_YQ)]
full_data$YTD<-factor(ifelse(full_data$Fiscal_Year==max(full_data$Fiscal_Year),"YTD","Full Year"),levels=c("Full Year","YTD"))
save(full_data,labels_and_colors,column_key, file="analysis/FPDS_chart_maker/unaggregated_FPDS.Rda")




summary(factor(full_data$YTD))
fpds_lc<-csis360::prepare_labels_and_colors(full_data %>% select(-recoveredmaterialclauses))

fpds_ck<-csis360::get_column_key(full_data)

fpds_data<-full_data %>% filter(ContractingCustomer=="Defense"&
                                  Fiscal_Year>=2010)

save(fpds_data,fpds_lc, fpds_ck,file = "..\\Trade\\data\\clean\\fpds_transaction_summary.rda")
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


fed_lc<-csis360::prepare_labels_and_colors(fed_data)

fed_ck<-csis360::get_column_key(fed_data)

fed_data$YTD<-factor(ifelse(fed_data$Fiscal_Year==max(fed_data$Fiscal_Year),"YTD","Full Year"),levels=c("Full Year","YTD"))
save(fed_data,fed_lc,fed_ck, file="data/clean/fed_summary_FPDS.Rda")


#############Defense Data (faster query run)##########
def_data<- read_delim(
  "Data//semi_clean//Federal_Summary.SP_CompetitionVendorSizeHistoryBucketPlatformSubCustomerLength.txt",delim = "\t",
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
def_lc<-prepare_labels_and_colors(def_data, path=file.path(local_path,"style\\"))

def_ck<-get_column_key(def_data %>% select(-PricingMechanism),path=file.path(local_path,"style\\"))
def_data$YTD<-factor(ifelse(def_data$Fiscal_Year==max(def_data$Fiscal_Year),"YTD","Full Year"),levels=c("Full Year","YTD"))
save(def_data,def_lc,def_ck, file="analysis/FPDS_chart_maker/unaggregated_def.Rda")
# load(file="analysis/FPDS_chart_maker/unaggregated_def.Rda")


###########Product Service Code, Agency, Platform ############

platpscdefcd<-read_delim(file.path("data","semi_clean","Defense_Location.SP_ProdServPlatformAgencyCongressionalDistrict.txt"),delim="\t",na=c("NULL","NA"),
                    col_names = TRUE, guess_max = 10000000)

platpscdefcd<-apply_standard_lookups(platpscdefcd)
any(duplicated(colnames(platpscdefcd)))

platpscdefcd<-initial_clean(platpscdefcd)


cd_lc<-csis360::prepare_labels_and_colors(platpscdefcd)
cd_ck<-csis360::get_column_key(platpscdefcd)
platpscdefcd$YTD<-factor(ifelse(platpscdefcd$Fiscal_Year==max(platpscdefcd$Fiscal_Year),"YTD","Full Year"),levels=c("Full Year","YTD"))
save(platpscdefcd,cd_lc, cd_ck,file="data/clean/platpscdefcd.Rda")

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
  mutate(PlatformPortfolio = factor(PlatformPortfolio))




detail_lc<-csis360::prepare_labels_and_colors(platpsc)
detail_ck<-csis360::get_column_key(platpsc)

save(platpsc,detail_lc,detail_ck, file="data/clean/platpsc_FPDS.Rda")


platpscintl<-read_delim(file.path("data","semi_clean","Federal_Location.SP_ProdServPlatformAgencyPlaceOriginVendor.txt"),delim="\t",na=c("NULL","NA"),
                        col_names = TRUE, guess_max = 10000000)
problems(platpscintl)
colnames(platpscintl)[colnames(platpscintl)=="Customer"]<-"ContractingCustomer"

# View(platpscintl[(nrow(platpscintl)-3):nrow(platpscintl),])
# View(platpscintldef[(nrow(platpscintldef)-3):nrow(platpscintldef),])
# debug(initial_clean)
platpscintl<-apply_standard_lookups(platpscintl)#,path=local_path
platpscintl<-initial_clean(platpscintl,only_defense=FALSE)
platpscintldef<-initial_clean(platpscintl,only_defense=TRUE)

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
  mutate(PlatformPortfolioUAV = factor(PlatformPortfolioUAV)) 



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
platpscintldef$YTD<-factor(ifelse(platpscintldef$Fiscal_Year==max(platpscintldef$Fiscal_Year),"YTD","Full Year"),levels=c("Full Year","YTD"))
save(platpscintldef,intl_lc, intl_ck,file="data/clean/platpscintl_FPDS.Rda")

fedpsc_lc<-csis360::prepare_labels_and_colors(platpscintl)
fedpsc_ck<-csis360::get_column_key(platpscintl)
platpscintl$YTD<-factor(ifelse(platpscintl$Fiscal_Year==max(platpscintl$Fiscal_Year),"YTD","Full Year"),levels=c("Full Year","YTD"))
save(platpscintl,fedpsc_lc, fedpsc_ck,file="data/clean/Federal_platpscintl_FPDS.Rda")


############## NAICS and High Tech Non-Trad #############
economic<-read_delim(file.path("data","semi_clean","Economic.ProdServPlatformNAICS.csv"),delim="\t",na=c("NULL","NA"),
                    col_names = TRUE, guess_max = 10000000)

economic<-apply_standard_lookups(economic)
economic<-read_and_join_experiment(economic,
                               lookup_file="Lookup_PrincipalNAICScode.csv",
                               directory="economic//",
                               by=c("principalnaicscode"="principalnaicscode"),
                               add_var=c("CriticalTech"),
                               skip_check_var =c("CriticalTech"),
                               missing_file="fpds_naics.csv")


economic_lc<-prepare_labels_and_colors(economic)
economic_ck<-get_column_key(economic)
save(economic, file="data/clean/ProdServPlatformNAICS.rda")


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


