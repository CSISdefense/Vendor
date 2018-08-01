#=================================================================================================================#
# Data processing for Monopolies Data
#=================================================================================================================#
rm(list = ls())
library(tidyverse)
library(csis360)
library(dplyr)
library(readr)

source("https://raw.githubusercontent.com/CSISdefense/R-scripts-and-data/master/lookups.r")
Path<-"https://raw.githubusercontent.com/CSISdefense/R-scripts-and-data/master/"

file<-unz("Data\\Defense_Vendor.SP_EntityIDhistoryNAICS.zip",
          filename="Defense_Vendor.SP_EntityIDhistoryNAICS.txt")
 defense_naics_vendor <- read_tsv(file,
                           col_names = TRUE,
                           NA = c("","NA","NULL"))

#Import Defense vendor list by NAICS.
defense_naics_vendor <- read_delim("Data\\Defense_Vendor.SP_EntityIDhistoryNAICS.txt",
                           # header = TRUE,
                           na = c("","NA","NULL"),
                           # quote="\"",#Necessary because there are some 's in the names.
                           delim = "\t")



#Import Defense Vendor list.
file<-unz("Data\\Defense_Vendor_EntityIDhistory.zip",
          filename="Defense_Vendor_EntityIDhistory.txt")

defense_vendor <- read.table(file,
                                   header = TRUE,
                                   na.strings = c("","NA","NULL"),
                                   quote="\"",#Necessary because there are some 's in the names.
                                   sep = "\t")



defense_vendor<-standardize_variable_names(defense_vendor)
defense_naics_vendor<-standardize_variable_names(defense_naics_vendor)
defense_naics_vendor<-deflate(defense_naics_vendor,
                              money_var = "Action.Obligation",
                              deflator_var="Deflator.2016"
                              )

defense_vendor<-csis360::read_and_join(defense_vendor,
                                       "Lookup_ParentID.csv",
                                       path="https://raw.githubusercontent.com/CSISdefense/Lookup-Tables/master/",
                                       by="ParentID",
                                       directory="vendor/",
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
  Obligation.2016 = sum(Action.Obligation.2016),
  vendor_count=length(Fiscal.Year),
  hh_index=sum((pct*100)^2,na.rm=TRUE),
  top4=sum(ifelse(pos<=4,pct,NA),na.rm=TRUE),
  top8=sum(ifelse(pos<=8,pct,NA),na.rm=TRUE),
  top12=sum(ifelse(pos<=8,pct,NA),na.rm=TRUE),
  top20=sum(ifelse(pos<=20,pct,NA),na.rm=TRUE),
  top50=sum(ifelse(pos<=50,pct,NA),na.rm=TRUE)
)


#*****************NAICS 6****************************


  
  summarize_annual_naics<-function(data,naics_level=6){
    data$NAICS_Code<-substr(data$NAICS_Code,1,naics_level)
    if(naics_level==2){
      data$NAICS_Code<-create_naics2(data$NAICS_Code)
    }
    
    data<-data %>% group_by(Fiscal.Year,NAICS_Code)
    
    data<-data %>% #filter(Action.Obligation>0) %>%
      dplyr::mutate(
        pos = rank(-Action.Obligation.Then.Year,
                   ties.method ="min"),
        pct = ifelse(Action.Obligation.Then.Year>0,
                     Action.Obligation.Then.Year / sum(Action.Obligation.Then.Year[Action.Obligation.Then.Year>0]),
                     NA
        )
      )
    
    #Learned the filtering approach from
    # https://stackoverflow.com/questions/23438476/dplyr-idiom-for-summarize-a-filtered-group-by-and-also-replace-any-nas-due-to
    
    output<-data %>% group_by(Fiscal.Year,NAICS_Code) %>%
      dplyr::summarize( 
        Action.Obligation = sum(Action.Obligation.Then.Year),
        Obligation.2016 = sum(Action.Obligation.2016),
        vendor_count=length(Fiscal.Year),
        hh_index=sum((pct*100)^2,na.rm=TRUE),
        pct_sum_check=sum(pct),
        top4=sum(pct[pos<=4],na.rm=TRUE),
        top8=sum(pct[pos<=8],na.rm=TRUE),
        top12=sum(pct[pos<=12],na.rm=TRUE),
        top20=sum(pct[pos<=20],na.rm=TRUE),
        top50=sum(pct[pos<=50],na.rm=TRUE)
      )
    output
  }
  
  annual_naics6_summary<-summarize_annual_naics(defense_naics_vendor)
  annual_naics5_summary<-summarize_annual_naics(defense_naics_vendor,5)
  annual_naics4_summary<-summarize_annual_naics(defense_naics_vendor,4)
  annual_naics3_summary<-summarize_annual_naics(defense_naics_vendor,3)
  annual_naics2_summary<-summarize_annual_naics(defense_naics_vendor,2)
  
save(defense_naics_vendor,
  defense_vendor,
  annual_summary,
  annual_naics6_summary,
  annual_naics5_summary,
  annual_naics4_summary,
  annual_naics3_summary,
  annual_naics2_summary,
  file="data//defense_naics_vendor.Rdata")
load(file="data//defense_naics_vendor.Rdata")
write.csv(defense_naics_vendor,"data//defense_naics_vendor.csv")
write.csv(defense_vendor,"data//defense_vendor.csv")
write.csv(annual_naics_summary,"data//annual_naics_summary.csv")
write.csv(annual_summary,"data//annual_summary.csv")

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
  stop("Duplicate 20012 entry")
}
if(length(core$NAICS.id[core$YEAR.id=="2007"])!=length(unique(core$NAICS.id[core$YEAR.id=="2007"]))){
  stop("Duplicate 20012 entry")
}
core$YEAR.id<-as.numeric(core$YEAR.id)
core$NAICS_Code<-core$NAICS.id
core$NAICS_Code[core$NAICS.id %in% c("48-49(104)","48-49(105)")]<-"48-49"


core$NAICS.id
annual_naics2_summary$NAICS_Code


colnames(core)[colnames(core)=="NAICS.display-label"]<-"NAICS.display.label"
colnames(core)[colnames(core)=="GEO.display-label"]<-"GEO.display.label"

join_economic<-function(data,core,num){
  
  data<-left_join(data,core,by=c("Fiscal.Year"="YEAR.id","NAICS_Code"="NAICS_Code"))
  data<-csis360::read_and_join(data,
                                                lookup_file = "Lookup_NAICS_code.csv",
                                                path="",
                                                dir="Lookup\\",
                                                by="NAICS_Code",
                                                skip_check_var="NAICS_DESCRIPTION"
  )
  
  data$RCPTOT<-as.numeric(data$RCPTOT)

  data$Mismatch<-NA
  #11
  data$Mismatch[substr(data$NAICS_Code,1,2) %in% c(11)]<-"Not tracked: Agriculture"
  #23
  data$Mismatch[substr(data$NAICS_Code,1,3) %in% c(233,234,235)]<-"Reassigned in 2002: Contruction"
  data$Mismatch[substr(data$NAICS_Code,1,4) %in% c(2361,2362,2371,2372,2373,2379,2381,2382,2383,2389)]<-"Uncollated at NAICS 4-5: Construction"
  #31
  data$Mismatch[substr(data$NAICS_Code,1,6) %in% c(314994,315210,315220,315990,316210,316998)]<-"New in 2012: Missing from Economy Stats"
  data$Mismatch[substr(data$NAICS_Code,1,5) %in% c(31528)]<-"New in 2012: Missing from Economy Stats"
  data$Mismatch[substr(data$NAICS_Code,1,6) %in% c(316212)]<-"Reassigned in 2012: Footware Consolidation"
  #32
  data$Mismatch[substr(data$NAICS_Code,1,6) %in% c(322220,322230)]<-"New in 2012: Missing from Economy Stats"
  #33
  data$Mismatch[substr(data$NAICS_Code,1,5) %in% c(33324)]<-"New in 2012: Missing from Economy Stats"
  data$Mismatch[substr(data$NAICS_Code,1,6) %in% c(331110,331523,332216,332613,333316,333318,333413,333517)]<-"New in 2012: Missing from Economy Stats"
  data$Mismatch[substr(data$NAICS_Code,1,6) %in% c(334118,334614,336310,336390)]<-"New in 2012: Missing from Economy Stats"
  data$Mismatch[substr(data$NAICS_Code,1,6) %in% c(339111)]<-"Reassigned in 2007: Laboratory Apparatus/Furniture Manufacture"
  #42
  data$Mismatch[substr(data$NAICS_Code,1,3) %in% c(421,422)]<-"Reassigned in 2002: Warehouse"
  #48
  data$Mismatch[substr(data$NAICS_Code,1,3) %in% c(482)]<-"Not tracked: Railroads"
  #49
  data$Mismatch[substr(data$NAICS_Code,1,3) %in% c(491)]<-"Not tracked: Postal Service"
  #51
  data$Mismatch[substr(data$NAICS_Code,1,3) %in% c(513,514)]<-"Reassigned in 2002: Telecom and Information Services"
  data$Mismatch[substr(data$NAICS_Code,1,3) %in% c(516)]<-"Reassigned in 2007: Telecom and Information Services"
  data$Mismatch[substr(data$NAICS_Code,1,4) %in% c(5173,5175,5181)]<-"Reassigned in 2007: Telecom and Information Services"
  data$Mismatch[substr(data$NAICS_Code,1,6) %in% c(517211,517212,517910)]<-"Reassigned in 2007: Telecom and Information Services"
  #52
  data$Mismatch[substr(data$NAICS_Code,1,3) %in% c(525)]<-"Not tracked: Pension and Other Funds"
  #54
  data$Mismatch[substr(data$NAICS_Code,1,6) %in% c(541710)]<-"Reassigned in 2007: R&D Phys/Life/Engineering"
  #56
  data$Mismatch[substr(data$NAICS_Code,1,6) %in% c(561310)]<-"Reassigned in 2007: Employment Placement Agencies"
  #61
  data$Mismatch[substr(data$NAICS_Code,1,4) %in% c(6111,6112,6113)]<-"Not tracked: Schools and Universities"
  #81
  data$Mismatch[substr(data$NAICS_Code,1,4) %in% c(8131)]<-"Not tracked: Religious Organizations"
  data$Mismatch[substr(data$NAICS_Code,1,5) %in% c(81393,81394)]<-"Not tracked: Labor Unions and Political Organizations"
  data$Mismatch[substr(data$NAICS_Code,1,3) %in% c(814)]<-"Not tracked: Private Households"
  #92
  data$Mismatch[substr(data$NAICS_Code,1,2) %in% c(92)]<-"Not tracked: Public Administration"
  
  
  mismatch<-subset(data,Fiscal.Year %in% c(2007,2012) &
                     is.na(NAICS.id) &
                     !is.na(NAICS_Code))
  mismatch<-mismatch %>% group_by(NAICS_Code) %>%
    dplyr::summarize(Action.Obligation=sum(Action.Obligation,na.rm=TRUE),
                     Obligation.2016=sum(Obligation.2016,na.rm=TRUE),
                     minyear=min(Fiscal.Year),
                     maxyear=max(Fiscal.Year)
    )
  

  
  
  mismatch<-subset(data,Fiscal.Year %in% c(2007,2012) &
                     is.na(NAICS.id) &
                     !is.na(NAICS_Code))
  mismatch<-mismatch %>% group_by(NAICS_Code,NAICS_DESCRIPTION,Mismatch) %>%
    dplyr::summarize(Action.Obligation=sum(Action.Obligation,na.rm=TRUE),
                     Obligation.2016=sum(Obligation.2016,na.rm=TRUE),
                     minyear=min(Fiscal.Year),
                     maxyear=max(Fiscal.Year)
    )
  
    write.csv(file=paste("Output\\NAICSunmatched",num,".csv",sep=""),
            mismatch
            )
    
    summed<-subset(data,Fiscal.Year>=2007) %>% 
      group_by(NAICS_Code,NAICS_DESCRIPTION,Mismatch) %>%
      dplyr::summarize(Action.Obligation=sum(Action.Obligation,na.rm=TRUE),
                       Obligation.2016=sum(Obligation.2016,na.rm=TRUE),
                       RCPTOT=sum(RCPTOT,na.rm=TRUE),
                       minyear=min(Fiscal.Year),
                       maxyear=max(Fiscal.Year),
                       NAICS.display.label=max(NAICS.display.label,na.rm=TRUE),
      )
    
    
  write.csv(file=paste("Output\\NAICSsummed",num,".csv",sep=""),
            summed
            
  )
  data
}




debug(join_economic)
test<-join_economic(annual_naics2_summary,core,2)
test<-join_economic(annual_naics3_summary,core,3)
test<-join_economic(annual_naics4_summary,core,4)
test<-join_economic(annual_naics5_summary,core,5)
test<-join_economic(annual_naics6_summary,core,6)