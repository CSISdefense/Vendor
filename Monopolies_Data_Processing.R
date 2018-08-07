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
defense_naics_vendor$exclude[data$Mismatch %in% get_exclude_list() | is.na(data$NAICS_Code)]<-TRUE


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

defense_naics_vendor$NAICS_Code[substr(defense_naics_vendor$NAICS_Code,1,5)=="54171"]<-"54171"


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
  
  
  duplicate_NAICS_check(core)
  core$YEAR.id<-as.numeric(core$YEAR.id)
  core$NAICS_Code<-core$NAICS.id
  core$NAICS_Code[core$NAICS.id %in% c("48-49(104)","48-49(105)")]<-"48-49"
  
    colnames(core)[colnames(core)=="NAICS.display-label"]<-"NAICS.display.label"
  colnames(core)[colnames(core)=="GEO.display-label"]<-"GEO.display.label"
  colnames(core)[colnames(core)=="OPTAX.display-label"]<-"OPTAX.display.label"
  

  core$rcp<-as.numeric(core$RCPTOT)*1000
  core$pay<-as.numeric(core$PAYANN)*1000
  

  #Remove (r) footnotes, which denote revised data and prevent conversion to numerall
  #Annual pay
  revised_list<-is.na(core$pay) & !is.na(core$PAYANN) &
    substr(core$PAYANN,nchar(core$PAYANN)-2,nchar(core$PAYANN))=="(r)"
  core$PAYANN[revised_list]
  core$mismatch[revised_list]<-paste(ifelse(is.na(core$mismatch[revised_list]),"",core$mismatch[revised_list])
                                                  ,"Removed revised footnote from PAYANN")
  core$pay[revised_list]<-as.numeric(substr(core$PAYANN[revised_list],1,nchar(core$PAYANN[revised_list])-3))
  
  #Reciepts
  revised_list<-is.na(core$rcp) & !is.na(core$RCPTOT) &
    substr(core$RCPTOT,nchar(core$RCPTOT)-2,nchar(core$RCPTOT))=="(r)"
  core$RCPTOT[revised_list]
  core$mismatch[revised_list]<-paste(ifelse(is.na(core$mismatch[revised_list]),"",core$mismatch[revised_list])
                                                  ,"Removed revised footnote from RCPTOT")
  core$rcp[revised_list]<-as.numeric(substr(core$RCPTOT[revised_list],1,nchar(core$RCPTOT[revised_list])-3))
  
  #Employees
  revised_list<-is.na(as.numeric(core$EMP)) & !is.na(core$EMP) &
    substr(core$EMP,nchar(core$EMP)-2,nchar(core$EMP))=="(r)"
  core$EMP[revised_list]
  core$mismatch[revised_list]<-paste(ifelse(is.na(core$mismatch[revised_list]),"",core$mismatch[revised_list])
                                                  ,"Removed revised footnote from EMP")
  core$EMP[revised_list]<-substr(core$EMP[revised_list],1,nchar(core$EMP[revised_list])-3)
  # debug(fill_in_core_gap)
  core<-rbind(core,fill_in_core_gap(core,4,"23"))
  core<-rbind(core,fill_in_core_gap(core,5,"23"))
  duplicate_NAICS_check(core)
  
  
  core$avg_sal<-core$pay/as.numeric(core$EMP)
  core$census_period<-NA
  core$census_period[core$YEAR.id==2007]<-"2007-2011"
  core$census_period[core$YEAR.id==2012]<-"2012-2016"
  
  def_all_levels<-rbind(annual_naics2_summary,annual_naics3_summary,annual_naics4_summary,
        annual_naics5_summary,
        subset(annual_naics6_summary,nchar(NAICS_Code)==6))#Extra step for R&D fix
  def_all_levels<-subset(def_all_levels,CalendarYear %in% c(2007,2012) & exclude==FALSE)
  def_all_levels[duplicated(def_all_levels[c("NAICS_Code","CalendarYear")]),]
  
  core<-full_join(core,def_all_levels,by=c("YEAR.id"="CalendarYear","NAICS_Code"="NAICS_Code"))
  duplicate_NAICS_check(core)
  core<-core[colnames(core) %in% c("GEO.id2", "NAICS.id", "NAICS.display.label",
                                  "OPTAX.id", "OPTAX.display.label",  "YEAR.id",
                                  "ESTAB", "RCPTOT", "PAYANN", "EMP", 
                                  "NAICS_Code", "rcp", "pay", "avg_sal", "census_period" ,
                                   "obl")]
  
  #Insert handling of 92 here
  core<-subset(core,!is.na(NAICS.id) | !is.na(rcp))
  core$ratio<-core$obl/core$rcp
  colnames(core)[colnames(core)=="obl"]<-"period_obl"
  
  
  core<-impute_from_higher_NAICS(core,4)
  core<-impute_from_higher_NAICS(core,5)
  core<-impute_from_higher_NAICS(core,6)
  
  duplicate_NAICS_check(core)
  
  View(core[!is.na(core$PAYANN)&is.na(core$pay),])
  # View(core[!is.na(core$EMP)&is.na(as.numeric(core$EMP)),])
  # View(core[!is.na(core$RCPTOT)&is.na(as.numeric(core$RCPTOT)),])
  
  sum(core$period_obl[!is.na(core$PAYANN)&is.na(core$pay)],na.rm=TRUE)
  sum(core$period_obl[!is.na(core$EMP)&is.na(as.numeric(core$EMP))],na.rm=TRUE)
  sum(core$period_obl[!is.na(core$RCPTOT)&is.na(core$rcp)],na.rm=TRUE)
  sum(core$period_obl[is.na(core$avg_sal)&is.na(as.numeric(core$RCPTOT))],na.rm=TRUE)
  sum(core$period_obl[is.na(core$ratio)&is.na(as.numeric(core$RCPTOT))],na.rm=TRUE)
  
  #************Saving********************
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
debug(join_economic)
annual_naics2_summary<-join_economic(annual_naics2_summary,core,2)
annual_naics3_summary<-join_economic(annual_naics3_summary,core,3)
annual_naics4_summary<-join_economic(annual_naics4_summary,core,4)
annual_naics5_summary<-join_economic(annual_naics5_summary,core,5)
annual_naics6_summary<-join_economic(annual_naics6_summary,core,6)