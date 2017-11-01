#=================================================================================================================#
# Data processing for Monopolies Data
#=================================================================================================================#
rm(list = ls())
library(tidyverse)
library(csis360)
ddply cannot
  
# Path<-"C:\\Users\\gsand_000.ALPHONSE\\Documents\\Development\\R-scripts-and-data\\"
Path<-"K:\\2007-01 PROFESSIONAL SERVICES\\R scripts and data\\"
source(paste(Path,"lookups.r",sep=""))


file<-unz("Data\\Defense_Vendor_EntityIDhistoryNAICS.zip",
          filename="Defense_Vendor_EntityIDhistoryNAICS.txt")
# defense_naics_vendor <- read_tsv(file,
#                           col_names = TRUE,
#                           na = c("","NA","NULL"))

#Import Defense vendor list by NAICS.
defense_naics_vendor <- read.table(file,
                           header = TRUE,
                           na.strings = c("","NA","NULL"),
                           quote="\"",#Necessary because there are some 's in the names.
                           sep = "\t")



#Import Defense Vendor list.
file<-unz("Data\\Defense_Vendor_EntityIDhistory.zip",
          filename="Defense_Vendor_EntityIDhistory.txt")

defense_vendor <- read.table(file,
                                   header = TRUE,
                                   na.strings = c("","NA","NULL"),
                                   quote="\"",#Necessary because there are some 's in the names.
                                   sep = "\t")



defense_vendor<-apply_lookups(Path,defense_vendor)
defense_naics_vendor<-apply_lookups(Path,defense_naics_vendor)


defense_vendor<-csis360::read_and_join(defense_vendor,
                                       "Lookup_ParentID.csv",
                                       path="https://raw.githubusercontent.com/CSISdefense/Lookup-Tables/master/",
                                       by="ParentID",
                                       directory="vendor//",
                                       new_var_checked=FALSE,
                                       add_var="Abbreviation"
                                       # NA.check.columns="Fair.Competed"
)

defense_naics_vendor<-csis360::read_and_join(defense_naics_vendor,
  "Lookup_ParentID.csv",
  path="https://raw.githubusercontent.com/CSISdefense/Lookup-Tables/master/",
  by="ParentID",
  directory="vendor//",
  new_var_checked=FALSE,
  add_var="Abbreviation"
  # NA.check.columns="Fair.Competed"
)

defense_vendor<- defense_vendor %>%
  group_by(Fiscal.Year) %>%
  transform(percentage = sum(Action.Obligation))


defense_vendor<-ddply(defense_vendor,
                      .(Fiscal.Year),
                      transform,
                      pos = rank(-Action.Obligation,
                                 ties.method ="min"),
                      pct = Action.Obligation / sum(Action.Obligation))

defense_naics_vendor<- defense_naics_vendor %>%
  group_by(Fiscal.Year,NAICS_Code) %>%
  transform(pct = Action.Obligation / sum(Action.Obligation))
  # dplyr::ddply(defense_naics_vendor,
  # .(Fiscal.Year,NAICS_Code),
  # transform, 
  # pos = rank(-Action.Obligation,
  #   ties.method ="min"),
  # pct = Action.Obligation / sum(Action.Obligation))


annual_summary<-ddply(defense_vendor,
  .(Fiscal.Year),
  summarize, 
  Action.Obligation = sum(Action.Obligation),
  vendor_count=length(Fiscal.Year),
  hh_index=sum((pct*100)^2)
)

annual_naics_summary<-ddply(defense_naics_vendor,
  .(Fiscal.Year,NAICS_Code,Industry_Text),
  summarize, 
  Action.Obligation = sum(Action.Obligation),
  vendor_count=length(Fiscal.Year),
  hh_index=sum((pct*100)^2)
)


save(defense_naics_vendor,
  defense_vendor,
  annual_summary,
  file="data//defense_naics_vendor.Rdata")











# # Transform some columns of interest into factor or integer and deflating
# 
# # sequestration<-deflate(sequestration,
# #   money_var = "PrimeOrSubObligatedAmount",
# #   deflator_var="Deflator.2016"
# # )
# 
# 
# 
# # Aggregating sequestration by Fiscal.Year, Platform Portfolio and IsSubContract
# 
#   #This suddenly stopped working after R / package updates. ??
# sequestration_Facet<- sequestration %>% dplyr::group_by(Fiscal.Year,
#                                                   PlatformPortfolio,
#                                                 #  IsSubContract,
#                                                   SubCustomer.sum,
#                                                 SubCustomer.platform,
#                                                 Pricing.Mechanism.sum,
#                                                 Pricing.Mechanism.Fee,
#                                                 ProductServiceOrRnDarea,
#                                                 ProductServiceOrRnDarea.sum,
#                                                   Faceting,
#                                                 Vendor.Size.sum,
#                                                 Shiny.VendorSize,
#                                                 IsFSRSreportable) %>%
#     dplyr::summarise(PrimeOrSubTotalAmount.2016 = sum(PrimeOrSubObligatedAmount.2016)/1e+9)
# 
# # Rename 'IsSubContract' column
# #sequestration_Facet$IsSubContract <- ifelse(sequestration_Facet$IsSubContract == 1,
# #                                            "SubContract", 
# #                                            "Prime Contract")
# # colnames(sequestration_Facet)[3] <- "SubCustomer.sum"
# 
# full_data<- sequestration_Facet
# labels_and_colors<-prepare_labels_and_colors(full_data)
# 
# column_key<-csis360::get_column_key(full_data)
# 
# save(labels_and_colors,column_key,full_data,file="Shiny Apps//SubContracts//subcontract_full_data.RData")
