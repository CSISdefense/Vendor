###########################################
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
###############################################

# install.packages("../csis360_0.0.0.9022.tar.gz")

library(tidyverse)
library(magrittr)
library(csis360)
library(readr)
library(fetch)
library(askpass)
library(odbc)
library(DBI)
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



#Full Data and Fed Data##########
  
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
  
  if(all(is.na(full_data[nrow(full_data),]))){
    full_data<-full_data[1:nrow(full_data)-1,]
    warning("Echo row dropped")
  }
  
  
  labels_and_colors<-prepare_labels_and_colors(full_data %>% select(-recoveredmaterialclauses))
  
  column_key<-get_column_key(full_data)
  
  full_data$Fiscal_YQ[!is.na(full_data$fiscal_quarter_YTD)]<-text_to_number(paste(full_data$Fiscal_Year[!is.na(full_data$fiscal_quarter_YTD)],
                                                                            text_to_number(full_data$fiscal_quarter_YTD[!is.na(full_data$fiscal_quarter_YTD)]),sep="."))
  full_data$Fiscal_YQ[is.na(full_data$Fiscal_YQ)]<-full_data$Fiscal_Year[is.na(full_data$Fiscal_YQ)]
  full_data$YTD<-factor(ifelse(full_data$Fiscal_Year==max(full_data$Fiscal_Year),"YTD","Full Year"),levels=c("Full Year","YTD"))
  save(full_data,labels_and_colors,column_key, file="analysis/FPDS_chart_maker/unaggregated_FPDS.Rda")
  
  full_datacat<-catalog("analysis/FPDS_chart_maker/", engines$rda,pattern="*FPDS*")
  write.csv(full_datacat$unaggregated_FPDS,file=file.path("docs","catalog","unaggregated_FPDS.csv"))
  

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


#Defense Data##########
def_data<- read_delim(
  "Data//semi_clean//Defense_Summary.SP_CompetitionVendorSizeHistoryBucketPlatformSubCustomerLength.txt",delim = "\t",
  col_names = TRUE, guess_max = 2000000,na=c("NA","NULL"))
problems(def_data)

def_data<-initial_clean(def_data,only_defense = TRUE)
def_data<-apply_standard_lookups(def_data,path="offline")#,

summary(factor(def_data$PricingInflation.1yearUCA))

def_data$PricingInflation.1yearUCA<-as.character(def_data$PricingInflation.1year)
def_data$PricingInflation.1yearUCA[def_data$PricingUCA.sum=="UCA"]<-"UCA"

def_lc<-prepare_labels_and_colors(def_data, path=file.path(local_path,"style\\"))
# add_labels_and_colors(def_data,"PricingUCA")
# add_labels_and_colors(def_data,"PricingUCA.sum","Pricing")
  def_ck<-get_column_key(def_data %>% select(-PricingMechanism),path="offline")
  def_data$YTD<-factor(ifelse(def_data$Fiscal_Year==max(def_data$Fiscal_Year),"YTD","Full Year"),levels=c("Full Year","YTD"))

    save(def_data,def_lc,def_ck, file="analysis/FPDS_chart_maker/unaggregated_def.Rda")

    def_data_cat<-catalog("analysis/FPDS_chart_maker/", engines$rda,pattern="*unaggregated_def*")
    def_data_cat$unaggregated_FPDS%>%dplyr::filter(Class=="character")
    
    write.csv(def_data_cat$unaggregated_def,file=file.path("docs","catalog","unaggregated_def.csv"),row.names = FALSE)


###########Cong Dist: Product Service Code, Agency, Platform ############

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



###########Plat PSC International ############
platpscintl<-read_delim(file.path("data","semi_clean","Federal_Location.SP_ProdServPlatformAgencyPlaceOriginVendor.txt"),delim="\t",na=c("NULL","NA"),
                        col_names = TRUE, guess_max = 10000000)
problems(platpscintl)
colnames(platpscintl)[colnames(platpscintl)=="Customer"]<-"ContractingCustomer"

# View(platpscintl[(nrow(platpscintl)-3):nrow(platpscintl),])
# View(platpscintldef[(nrow(platpscintldef)-3):nrow(platpscintldef),])
# debug(initial_clean)
platpscintl<-apply_standard_lookups(platpscintl)#,path=local_path

platpscintldef<-initial_clean(platpscintl,only_defense=TRUE)

summary(platpscintldef$PlatformPortfolio)

# write.csv(platpscintldef %>% filter(PlatformPortfolio=="Ordnance and Missiles"), 
#           file="Output//Munitions//OM.csv",
#           row.names = FALSE)

# write.csv(platpscintldef %>% filter(PlatformPortfolio=="Other Products"&SubCustomer=="Army"), 
#           file="Output//Munitions//ArmyOtherProducts.csv",
#           row.names = FALSE)

# n<-platpscintl %>% group_by(IsFMS,IsFMSmac,IsFMSml,fundedbyforeignentity) %>%
#   summarise(n=length(fiscal_year),min=min(fiscal_year),max=max(fiscal_year))

#Vendor Size

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


platpscintl %<>%
  # select(-ContractingCustomer) %>%
  # select(-ClassifyNumberOfOffers) %>%
  mutate(ProductServiceOrRnDarea = factor(ProductServiceOrRnDarea)) 



platpscintldef %<>%
  # select(-ContractingCustomer) %>%
  # select(-ClassifyNumberOfOffers) %>%
  mutate(ProductServiceOrRnDarea = factor(ProductServiceOrRnDarea))

# 
platpscintldef<-standardize_variable_names(platpscintldef)

colnames(platpscintldef)[colnames(platpscintldef)=="IsForeignHeadquarters"]<-"IsForeignHQ"
if("IsForeignHQ" %in% colnames(platpscintldef)){
  platpscintldef <- platpscintldef%>% mutate(IsForeignHQ=case_when(
    IsForeignHQ==1~1,
    VendorIsForeign==1~1,
    TRUE~IsForeignHQ
  ))
}

platpscintldef<-platpscintldef %>% mutate(
  AnyInternational=as.logical(case_when(
    IsFMS==1~T,
    MFGorPerformIsForeign==1~T,
    OriginIsForeign==1~T,
    VendorIsForeign==1~T,
    PlaceIsForeign==1~T
  ))
)



intl_lc<-csis360::prepare_labels_and_colors(platpscintldef)
intl_ck<-csis360::get_column_key(platpscintldef)
platpscintldef$YTD<-factor(ifelse(platpscintldef$Fiscal_Year==max(platpscintldef$Fiscal_Year),"YTD","Full Year"),levels=c("Full Year","YTD"))
save(platpscintldef,intl_lc, intl_ck,file="data/clean/platpscintl_FPDS.Rda")

fedpsc_lc<-csis360::prepare_labels_and_colors(platpscintl)
fedpsc_ck<-csis360::get_column_key(platpscintl)
platpscintl$YTD<-factor(ifelse(platpscintl$Fiscal_Year==max(platpscintl$Fiscal_Year),"YTD","Full Year"),levels=c("Full Year","YTD"))
save(platpscintl,fedpsc_lc, fedpsc_ck,file="data/clean/Federal_platpscintl_FPDS.Rda")
# load(file="data/clean/Federal_platpscintl_FPDS.Rda")
file.exists("data/clean/Federal_platpscintl_FPDS.Rda")
platpscintlcat<-catalog("data/clean/", engines$rda,pattern="*platpscintl*")
write.csv(platpscintlcat$Federal_platpscintl_FPDS,file=file.path("docs","catalog","Federal_platpscintl_FPDS.csv"),row.names = FALSE)
write.csv(platpscintlcat$platpscintl_FPDS,file=file.path("docs","catalog","platpscintl_FPDS.csv"),row.names = FALSE)


platpscintlcat$Federal_platpscintl_FPDS
platpscintlcat$Federal_platpscintl_FPDS$Co
platpscintlcat<-catalog("data/semi_clean/", engines$csv)
platpscintlcat<-catalog(file.path("data","semi_clean","Federal_Location.SP_ProdServPlatformAgencyPlaceOriginVendor.txt"), engines$csv)


summary(engines)

create_dictionary(platpscintl)

### Ship PSC Initial ####

load(file="data/clean/platpscintl_FPDS.Rda")
shippscintldef<-platpscintldef %>% filter(PlatformPortfolio == "Ships & Submarines")
nrow(shippscintldef)

shippscintldef<-read_and_join_experiment(shippscintldef,
                             "ProductOrServiceCodes.csv",
                             by=c("ProductOrServiceCode"="ProductOrServiceCode"),
                             path="offline",
                             add_var=c("ShipCategory"),
                             skip_check_var = c("ShipCategory"),
                             directory=""
)

shippscintldef <- shippscintldef %>%
  mutate(ShipCategory = ifelse(SimpleArea == "Products" & (is.na(ShipCategory)
                         | ShipCategory == ""), "Other Products", ShipCategory))
shippscintldef <- shippscintldef %>%
  mutate(ShipCategory = ifelse(SimpleArea == "Services" & (is.na(ShipCategory) 
                         | ShipCategory == ""), "Other Service", ShipCategory))

#shippscintldef %>% filter(ShipCategory == "") %>% group_by(ProductOrServiceCode, ProductOrServiceCodeText) %>% 
 # summarize(obl=sum(Action_Obligation_OMB25_GDP23)) %>% arrange(-obl) #Greg left this here, HHC removing for now, replacing with below

shippscintldef %>% group_by(ProductOrServiceCode, ProductOrServiceCodeText) %>% 
 summarize(obl=sum(Action_Obligation_OMB25_GDP23)) %>% arrange(-obl)

shippscintldef %>% group_by()

fedpsc_lc<-csis360::prepare_labels_and_colors(shippscintldef)
fedpsc_ck<-csis360::get_column_key(shippscintldef)

save(shippscintldef,fedpsc_lc, fedpsc_ck,file="data/clean/Defense_Ship_FPDS.Rda")
write_csv(shippscintldef,file.path("output","shippscintldef.csv"))



# NAICS and High Tech Non-Trad ####
economic<-read_delim(file.path("data","semi_clean","Defense_Economic.SP_NAICSprodservNonTraditionalHistory.txt"),delim="\t",na=c("NULL","NA"),
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

#PBL ######
# pbl<-read_delim(file.path("Data","Semi_clean","Contract.SP_PBLfpdsPartial.txt"),delim="\t",guess_max=10000000)
# pbl$transaction_description
# problems(pbl)
# colnames(pbl)
login<-askpass("Please enter the SQL login account")
pwd<-askpass("Please enter the SQL account password")

vmcon <- dbConnect(odbc(),
                   Driver = "SQL Server",
                   Server = "vmsqldiig.database.windows.net",
                   Database = "CSIS360",
                   UID = login,
                   PWD =pwd)

#2203 2025/05/07. Expecting an 8 hour run.
#0200 2025/06/07 Or so.
sql<-paste0("EXEC [Contract].SP_PBLfpdsPartial")

pbl_partial<-dbGetQuery(vmcon,  sql)
save(pbl_partial, file=file.path("data","semi_clean","Contract.SP_PBLfpdsPartial.rda"))

path<-"Output"
xlsx<-"PBL_full.xlsx"
sheet<-"Full"
if(file.exists(file.path(path,xlsx))){
  wb <- openxlsx::loadWorkbook(file.path(path,xlsx))
} else{
  wb<-openxlsx::createWorkbook(file.path(path,xlsx))
}
if(!sheet %in% names(wb))
  openxlsx::addWorksheet(wb,sheet)
# numstyle<-openxlsx::createStyle(numFmt = num_format)
# pstyle<-openxlsx::createStyle(numFmt = "PERCENTAGE")
# if(excel_then_year){
openxlsx::writeData(wb, pbl_partial, sheet = sheet)
openxlsx::saveWorkbook(wb,file=(file.path(path,xlsx)),overwrite = TRUE)


pbl_short<- pbl_partial %>% group_by(fiscal_year,
                                     parent_contract_award_unique_key,
                                     contract_award_unique_key,
                                     contract_transaction_unique_key,
                                     transaction_description,
                                     ContractingCustomer,
                                     SubCustomer,
                                     PlatformPortfolio,
                                     ProductOrServiceArea,
                                     # ProductOrServiceCode,
                                     ProjectID,
                                     claimantprogramcode,
                                     CompetitionClassification,
                                     ClassifyNumberOfOffers,
                                     VehicleClassification,
                                     VendorSize,
                                     # SizeOfSumOfObligatedAmount,
                                     CurrentDurationCategory,
                                     UnmodifiedUltimateDurationCategory,
                                     performancebasedservicecontract,
                                     typeofcontractpricing,
                                     ContractLabelID,
                                     ContractLabelText,
                                     IsPerformanceBasedLogistics,
                                     obligatedAmount,
                                     numberOfActions  ) %>% summarise(obligatedAmount=sum(obligatedAmount),
                                                                      numberOfActions=sum(numberOfActions))

pbl_short<-apply_standard_lookups(pbl_short)


path<-"Output"
xlsx<-"PBL_select_columns.xlsx"
sheet<-"Full"
if(file.exists(file.path(path,xlsx))){
  wb <- openxlsx::loadWorkbook(file.path(path,xlsx))
} else{
  wb<-openxlsx::createWorkbook(file.path(path,xlsx))
}
if(!sheet %in% names(wb))
  openxlsx::addWorksheet(wb,sheet)
# numstyle<-openxlsx::createStyle(numFmt = num_format)
# pstyle<-openxlsx::createStyle(numFmt = "PERCENTAGE")
# if(excel_then_year){
openxlsx::writeData(wb, pbl_short, sheet = sheet)
openxlsx::saveWorkbook(wb,file=(file.path(path,xlsx)),overwrite = TRUE)




#Space ########
if(!exists("platpscintl")) load(file="data/clean/Federal_platpscintl_FPDS.Rda")
space<-read_delim(file.path("data","semi_clean","ProductOrServiceCode.SP_SpaceDetail.txt"),delim="\t",na=c("NULL","NA"),
               col_names = TRUE, guess_max = 10000000)
space<-apply_standard_lookups(space,path="offline")
colnames(space)[colnames(space)=="ProductOrServiceCode...7"]<-"ProductOrServiceCode"
colnames(space)[colnames(space)=="ProjectID...9"]<-"ProjectID"
space<-space %>% select(-ProductOrServiceCode...26,-ProjectID...29)
space<-apply_standard_lookups(space)
space$YTD<-factor(ifelse(space$Fiscal_Year==max(space$Fiscal_Year),"YTD","Full Year"),levels=c("Full Year","YTD"))
space_lc<-prepare_labels_and_colors(space)
space_ck<-get_column_key(space)
space_fedpsc<-platpscintl %>% filter(PlatformPortfolio=="Space Systems")


save(spaceplatpscintl,space,space_lc,space_ck,space_fedpsc,fedpsc_ck,fedpsc_lc, file="data/clean/space_FPDS.Rda")


#Software #############
sw<-read_delim(file.path("data","semi_clean","Summary.SP_SoftwareDetail.txt"),delim="\t",na=c("NULL","NA"),
               col_names = TRUE, guess_max = 10000000)



sw<-apply_standard_lookups(sw)

sw_lc<-prepare_labels_and_colors(sw)
sw_ck<-get_column_key(sw)

save(sw,sw_lc,sw_ck, file="data/clean/sw_FPDS.Rda")

#JADC2##########

jadc2 <- read_delim(
  "Data//semi_clean//Summary.SP_JADC2detail.txt",delim = "\t",
  col_names = TRUE, guess_max = 2000000,na=c("NA","NULL"))

jadc2<-apply_standard_lookups(jadc2)#,


jadc2_lc<-csis360::prepare_labels_and_colors(jadc2)
jadc2_ck<-csis360::get_column_key(jadc2)
save(jadc2,jadc2_lc, jadc2_ck,file="data/clean/jadc2.Rda")





#Pricing History 1980-2021 #############

pricing<- read_csv(
  "Data_Raw//FPDS_Reports//Defense_Pricing_Mechanism.csv",
  # "Data_Raw//FPDS_Reports//Defense_Pricing_Mechanism_Agency.csv",
  skip = 2,
  col_names = TRUE, guess_max = 5000000,na=c(""))


colnames(pricing)<-gsub(" ","",colnames(pricing))
pricing<-standardize_variable_names(pricing)
colnames(pricing)[colnames(pricing)=="FiscalYear"]<-"Fiscal_Year"
pricing<-apply_standard_lookups(pricing)#,

colnames(pricing)[colnames(pricing)=="DollarsObligated"]<-"Action_Obligation"
colnames(pricing)[colnames(pricing)=="PricingMechanismCode"]<-"TypeOfContractPricing"

pricing$Action_Obligation<-text_to_number(pricing$Action_Obligation)
# pricing<-pricing %>% select(-PricingInflation)
# pricing<-pricing %>% select(-MajorCommandName)
pricing_lc<-csis360::prepare_labels_and_colors(pricing %>% select(-ContractingOfficeName,
                                                                  -PricingMechanism ,
                                                                  -MajorCommandName))

pricing_ck<-csis360::get_column_key(pricing)

save(pricing,pricing_lc,pricing_ck, file="data/clean/pricing_latest.Rda")






#Pricing History 1980-2021 #############

pricing<- read_csv(
  "Data_Raw//FPDS_Reports//Defense_Pricing_Mechanism_Agency.csv",
  # "Data_Raw//FPDS_Reports//Defense_Pricing_Mechanism_Agency.csv",
  col_names = TRUE, guess_max = 5000000,na=c(""))


colnames(pricing)<-gsub(" ","",colnames(pricing))
pricing<-standardize_variable_names(pricing)

pricing<-apply_standard_lookups(pricing)#,

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
## Recipient_UEI #######
ruh<-read_delim(file.path("data","semi_clean","Vendor.RecipientUEIpartial.txt"),delim="\t",
                na=c("NULL",""))
ruh<-apply_standard_lookups(ruh)

ruh<-deflate(ruh,money_var="DefenseObligated")


rpuh<-read_delim(file.path("data","semi_clean","Vendor.Parent_UEIhistory.txt"),delim="\t",
                na=c("NULL",""))
rpuh<-apply_standard_lookups(rpuh)

rpuh<-deflate(rpuh,money_var="DefenseObligated")

rpuh <- rpuh %>%
  mutate(AlwaysIsSmallLabel= case_when(
    AlwaysIsSmall==1 ~ "Consistently Small Vendor",
    AlwaysIsSmall==0 ~ "Variably Small or Larger Vendor"),
    IsEntityTraditionalLabel= case_when(
      IsEntityTraditional==1 | is.na(IsEntityTraditional) | IsEntityTraditional=="Unlabeled"~ "Traditional\nDefense Contractor",
      IsEntityTraditional==0 ~ "Non-Traditional\nDefense Contractor"),
    LargestContract2018dollars=case_when(
      IsEntityAbove2018constantCommercialItem7500k==1 ~ "[$7.5 M+]",
      IsEntityAbove2018constantCostAccounting2000kThreshold==1 ~ "[$2.0 M - $7.5M)",
      IsEntityAbove2018constantSimplifedAcquisition250kThreshold==1 ~ "[$250k K - $2.0 M)",
      IsEntityAbove2018constantMTAthreshold==1 ~ "[$10 K - $250 K)",
      IsEntityAbove2018constantMTAthreshold==0 ~ "[0 K - $10 K)"
    )
  )


def_rpuh<-rpuh %>% filter(AnyDefense==1) 
def_rpuh <- def_rpuh %>%
  group_by(Fiscal_Year)%>% mutate(count=1,
                                  hhi=(100*DefenseObligated_OMB25_GDP23/
                                         sum(DefenseObligated_OMB25_GDP23,na.rm = TRUE))^2) 
  
  rpuh<-rpuh %>%group_by(Fiscal_Year)%>% mutate(count=1,
                                   hhi=(100*Action_Obligation_OMB25_GDP23/
                                          sum(Action_Obligation_OMB25_GDP23,na.rm = TRUE))^2) 
  
# def_rpuh<-ruh %>% filter(AnyDefense==1) %>% mutate(Parent_UEI=if_else(!is.na(Parent_UEI),Parent_UEI,UEI)) %>%
#   group_by(Parent_UEI,Fiscal_Year)%>%
#   summarise(
#     # ParentID=if_else(max(ParentID,na.rm=TRUE)==min(ParentID,na.rm=TRUE),
#     #                    ParentID,NA),
#     # 
#     # StandardizedTopContractor=if_else(max(StandardizedTopContractor,na.rm=TRUE)==
#     #                                     min(StandardizedTopContractor,na.rm=TRUE),
#     #                                   StandardizedTopContractor,NA),
#     Parent_UEIFirstDate=min(Parent_UEIFirstDate,na.rm=TRUE)
#     ,Defense_Action_Obligation_OMB25_GDP23=sum(DefenseObligated_OMB25_GDP23,na.rm=TRUE),
#     Defense_Action_Obligation_Then_Year=sum(DefenseObligated_Then_Year,na.rm=TRUE),
#     # StandardizedTopContractor=if_else(max(topISO3countrycode,na.rm=TRUE)==
#     #                                     min(topISO3countrycode,na.rm=TRUE),
#     #                                   topISO3countrycode,NA),
#     MaxOfCAUobligatedAmount=max(MaxOfCAUobligatedAmount,na.rm=TRUE)
#     ,AnyIsSmall=max(AnyIsSmall,na.rm=TRUE)
#     ,AlwaysIsSmall=min(AlwaysIsSmall)
#     ,ObligatedAmountIsSmall=sum(ObligatedAmountIsSmall,na.rm=TRUE)
#     ,IsOnePercentPlusSmall=max(IsOnePercentPlusSmall,na.rm=TRUE)
#     ,IsEntityTraditional=max(IsEntityTraditional,na.rm=TRUE)
#     # ,EntitySizeCode      
#     ,IsEntityAbove1990constantMTAthreshold=max(IsEntityAbove1990constantMTAthreshold,na.rm=TRUE)
#     ,IsEntityAbove1990constantMTAthreshold=max(IsEntityAbove1990constantMTAthreshold,na.rm=TRUE)
#     ,IsEntityAbove2016constantMTAthreshold=max(IsEntityAbove2016constantMTAthreshold,na.rm=TRUE)
#     ,IsEntityAbove2018constantMTAthreshold=max(IsEntityAbove2018constantMTAthreshold,na.rm=TRUE)
#     ,IsEntityAbove2016constantArbitrary1000k=max(IsEntityAbove2016constantArbitrary1000k,na.rm=TRUE)
#     ,IsEntityAbove2018constantSimplifedAcquisition250kThreshold=
#       max(IsEntityAbove2018constantSimplifedAcquisition250kThreshold,na.rm=TRUE)
#     ,IsEntityAbove2018constantCommercialItem7500k=max(IsEntityAbove2018constantCommercialItem7500k)
#     ,IsEntityAbove2018constantCostAccounting2000kThreshold=
#       max(IsEntityAbove2018constantCostAccounting2000kThreshold,na.rm=TRUE)
#     ,AnyEntityUSplaceOfPerformance=max(AnyEntityUSplaceOfPerformance,na.rm=TRUE)
#     ,AnyEntityForeignPlaceOfPerformance=max(AnyEntityForeignPlaceOfPerformance,na.rm=TRUE)
#     )  

# t<-hMisc::cut2(def_rpuh$DefenseObligated_OMB25_GDP23,cuts=c(100000,20000))

rpuh_lc<-prepare_labels_and_colors(def_rpuh,path="Offline")  
rpuh_ck<-get_column_key(def_rpuh,path="Offline")

  summary(factor(def_rpuh$IsEntityTraditional))
  save(ruh,rpuh,def_rpuh,rpuh_lc,rpuh_ck,file=file.path("data","clean","RecipientUEI.rda"))


