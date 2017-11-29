#=================================================================================================================#
# Data processing for Monopolies Data
#=================================================================================================================#
rm(list = ls())
library(tidyverse)
library(csis360)
library(dplyr)


Path<-"C:\\Users\\gsand_000.ALPHONSE\\Documents\\Development\\R-scripts-and-data\\"
# Path<-"K:\\2007-01 PROFESSIONAL SERVICES\\R scripts and data\\"
source(paste(Path,"lookups.r",sep=""))


file<-unz("Data\\Defense_Vendor_EntityIDhistoryNAICS.zip",
          filename="Defense_Vendor_EntityIDhistoryNAICS.txt")
# defense_naics_vendor <- read_tsv(file,
#                           col_names = TRUE,
#                           NA = c("","NA","NULL"))

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


defense_vendor<-defense_vendor %>% group_by(Fiscal.Year)

defense_vendor<-defense_vendor %>%
  dplyr::mutate(
    pos = rank(-Obligation.2016,
               ties.method ="min"),
    pct = ifelse(Action.Obligation>0,
                 Action.Obligation / sum(Action.Obligation[Action.Obligation>0]),
                 NA
    )
  )


annual_summary<-defense_vendor %>%
  dplyr::summarize(
  Action.Obligation = sum(Action.Obligation),
  Obligation.2016 = sum(Obligation.2016),
  vendor_count=length(Fiscal.Year),
  hh_index=sum((pct*100)^2,na.rm=TRUE),
  top4=sum(ifelse(pos<=4,pct,NA),na.rm=TRUE),
  top8=sum(ifelse(pos<=8,pct,NA),na.rm=TRUE),
  top12=sum(ifelse(pos<=8,pct,NA),na.rm=TRUE),
  top20=sum(ifelse(pos<=20,pct,NA),na.rm=TRUE),
  top50=sum(ifelse(pos<=50,pct,NA),na.rm=TRUE)
)


defense_naics_vendor<-defense_naics_vendor %>% group_by(Fiscal.Year,NAICS_Code)
group_vars(defense_naics_vendor)

defense_naics_vendor<-defense_naics_vendor %>% #filter(Action.Obligation>0) %>%
 dplyr::mutate(
  pos = rank(-Action.Obligation,
             ties.method ="min"),
  pct = ifelse(Action.Obligation>0,
               Action.Obligation / sum(Action.Obligation[Action.Obligation>0]),
               NA
               )
)


#Learned the filtering approach from
# https://stackoverflow.com/questions/23438476/dplyr-idiom-for-summarize-a-filtered-group-by-and-also-replace-any-nas-due-to

annual_naics_summary<-defense_naics_vendor %>% group_by(Fiscal.Year,NAICS_Code) %>%
  dplyr::summarize( 
  Action.Obligation = sum(Action.Obligation),
  Obligation.2016 = sum(Obligation.2016),
  vendor_count=length(Fiscal.Year),
  hh_index=sum((pct*100)^2,na.rm=TRUE),
  pct_sum_check=sum(pct),
  top4=sum(pct[pos<=4],na.rm=TRUE),
  top8=sum(pct[pos<=8],na.rm=TRUE),
  top12=sum(pct[pos<=12],na.rm=TRUE),
  top20=sum(pct[pos<=20],na.rm=TRUE),
  top50=sum(pct[pos<=50],na.rm=TRUE)
)

save(defense_naics_vendor,
  defense_vendor,
  annual_summary,
  annual_naics_summary,
  file="data//defense_naics_vendor.Rdata")

write.csv(defense_naics_vendor,"data//defense_naics_vendor.csv")
write.csv(defense_vendor,"data//defense_vendor.csv")
write.csv(annual_naics_summary,"data//annual_naics_summary.csv")
write.csv(annual_summary,"data//annual_summary.csv")