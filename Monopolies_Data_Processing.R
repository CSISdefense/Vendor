#=================================================================================================================#
# Data processing for Monopolies Data
#=================================================================================================================#
rm(list = ls())
library(tidyverse)
library(csis360)
library(dplyr)
library(readr)
library(csis360)

source("https://raw.githubusercontent.com/CSISdefense/R-scripts-and-data/master/lookups.r")
source("DIIGstat.r")
source("NAICS.r")
Path<-"https://raw.githubusercontent.com/CSISdefense/R-scripts-and-data/master/"

file<-unz("Data\\Defense_Vendor.SP_EntityIDhistoryNAICS.zip",
          filename="Defense_Vendor.SP_EntityIDhistoryNAICS.txt")
 # defense_naics_vendor <- read_tsv(file,
 #                           col_names = TRUE,
 #                           NA = c("","NA","NULL"))

#Import Defense vendor list by NAICS.
defense_naics_vendor <- read_delim("Data\\Defense_Vendor.SP_EntityIDhistoryNAICS.txt",
                           # header = TRUE,
                           na = c("","NA","NULL"),
                           # quote="\"",#Necessary because there are some 's in the names.
                           delim = "\t")

problems(defense_naics_vendor)

#Import Defense Vendor list.
file<-unz("Data\\Defense_Vendor_EntityIDhistory.zip",
          filename="Defense_Vendor_EntityIDhistory.txt")

defense_vendor <- read.table(file,
                                   header = TRUE,
                                   na.strings = c("","NA","NULL"),
                                   quote="\"",#Necessary because there are some 's in the names.
                                   sep = "\t")


defense_naics_vendor<-clean_entity(defense_naics_vendor)
defense_vendor<-clean_entity(defense_vendor)

defense_naics_vendor<-label_naics_mismatch(defense_naics_vendor)
defense_naics_vendor$exclude<-FALSE
defense_naics_vendor$exclude[data$Mismatch %in% c("Not tracked: Agriculture",
                                  "Not tracked: Railroads",
                                  "Not tracked: Postal Service",
                                  "Not tracked: Pension and Other Funds",
                                  "Not tracked: Schools and Universities",
                                  "Not tracked: Labor Unions and Political Organizations",
                                  "Not tracked: Religious Organizations",
                                  "Not tracked: Private Households"
)| is.na(data$NAICS_Code)]<-TRUE


#******************Calculate Defense Wide Values****************
defense_vendor<-defense_vendor %>% group_by(CalendarYear)

defense_vendor<-defense_vendor %>%
  dplyr::mutate(
    pos = rank(-Action.Obligation.2016,
               ties.method ="min"),
    pct = ifelse(Action.Obligation>0,
                 Action.Obligation / sum(Action.Obligation[Action.Obligation>0]),
                 NA
    )
  )


annual_summary<-defense_vendor %>%
  dplyr::summarize(
  Action.Obligation = sum(Action.Obligation),
  # Obligation.2016 = sum(Action.Obligation.2016),
  vendor_count=length(CalendarYear),
  hh_index=sum((pct*100)^2,na.rm=TRUE),
  top4=sum(ifelse(pos<=4,pct,NA),na.rm=TRUE),
  top8=sum(ifelse(pos<=8,pct,NA),na.rm=TRUE),
  top12=sum(ifelse(pos<=8,pct,NA),na.rm=TRUE),
  top20=sum(ifelse(pos<=20,pct,NA),na.rm=TRUE),
  top50=sum(ifelse(pos<=50,pct,NA),na.rm=TRUE)
)


#*****************NAICS 6****************************

  annual_naics6_summary<-summarize_annual_naics(defense_naics_vendor)
  annual_naics5_summary<-summarize_annual_naics(defense_naics_vendor,5)
  annual_naics4_summary<-summarize_annual_naics(defense_naics_vendor,4)
  annual_naics3_summary<-summarize_annual_naics(defense_naics_vendor,3)
  annual_naics2_summary<-summarize_annual_naics(defense_naics_vendor,2)

  
  
  #*********************Read in Core US data************************************
  path<-"Data\\Economic\\Comparative Statistics for the United States and the States"
  core<-readr::read_csv(file.path(path,"ECN_2012_US_00CCOMP1_with_ann.csv"))
  core<-core[!core$GEO.id=="Geographic identifier code",]
  if(all(!is.na(core$GEO.id2))){
    core<-core[,!colnames(core) %in% c("GEO.id2")]
  }
  core$`GEO.display-label`[1]
  if(all(core$GEO.id=="0100000US")){
    core<-core[,!colnames(core) %in% c("GEO.id")]
  }
  if(all(core$`GEO.display-label`=="United States")){
    core<-core[,!colnames(core) %in% c("GEO.display-label")]
  }
  
  
  
  
  if(length(core$NAICS.id[core$YEAR.id=="2012"])!=length(unique(core$NAICS.id[core$YEAR.id=="2012"]))){
    stop("Duplicate 2012 entry")
  }
  if(length(core$NAICS.id[core$YEAR.id=="2007"])!=length(unique(core$NAICS.id[core$YEAR.id=="2007"]))){
    stop("Duplicate 2012 entry")
  }
  core$YEAR.id<-as.numeric(core$YEAR.id)
  core$NAICS_Code<-core$NAICS.id
  core$NAICS_Code[core$NAICS.id %in% c("48-49(104)","48-49(105)")]<-"48-49"
  
    colnames(core)[colnames(core)=="NAICS.display-label"]<-"NAICS.display.label"
  colnames(core)[colnames(core)=="GEO.display-label"]<-"GEO.display.label"
  
  
  View(core[is.na(as.numeric(core$PAYANN)),])
  View(core[is.na(as.numeric(core$EMP)),])
  View(core[is.na(as.numeric(core$RCPTOT)),])
  
  
  core$US_rcp<-as.numeric(core$RCPTOT)*1000
  core$US_pay<-as.numeric(core$PAYANN)*1000
  core$avg_sal<-core$US_pay/as.numeric(core$EMP)

  const4<-subset(core, nchar(NAICS_Code)==6 & substring(NAICS_Code,1,2)=="23")
  const4$NAICS_Code<-substring(const4$NAICS_Code,1,4)
  const4<-const4%>%group_by(YEAR.id,NAICS_Code)%>% 
    dplyr::summarize(US_rcp=sum(US_rcp),
                     US_pay=sum(US_pay),
                     EMP=sum(as.numeric(EMP))
    )
    
save(defense_naics_vendor,
  defense_vendor,
  annual_summary,
  annual_naics6_summary,
  annual_naics5_summary,
  annual_naics4_summary,
  annual_naics3_summary,
  annual_naics2_summary,
  core,
  file="data//defense_naics_vendor.Rdata")
load(file="data//defense_naics_vendor.Rdata")
write.csv(defense_naics_vendor,"data//defense_naics_vendor.csv")
write.csv(defense_vendor,"data//defense_vendor.csv")
write.csv(annual_naics_summary,"data//annual_naics_summary.csv")
write.csv(annual_summary,"data//annual_summary.csv")


#**********************Combine Econ and Def**************

# dput(colnames(test))

# View()
  test<-join_economic(annual_naics2_summary,core,2)
  test<-join_economic(annual_naics3_summary,core,3)
  test<-join_economic(annual_naics4_summary,core,4)
  test<-join_economic(annual_naics5_summary,core,5)
  test<-join_economic(annual_naics6_summary,core,6)