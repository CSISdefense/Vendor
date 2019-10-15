
create_naics2<-function(NAICS){
  NAICS2<-substring(NAICS,1,2)
  NAICS2[NAICS2 %in% c('31','32','33')]<-'31-33'
  NAICS2[NAICS2 %in% c('44','45')]<-'44-45'
  NAICS2[NAICS2 %in% c('48','49')]<-'48-49'
  NAICS2<-factor(NAICS2)
  NAICS2
}


# library(csis360)
# 
# naics<-read.csv("C:/Users/GSanders/Documents/Repositories/Lookup-Tables/economic/Lookup_PrincipalNAICScode.csv",na.strings="NULL")
# naics<-csis360::remove_bom(naics)
# naics$principalNAICS2DigitCode<-as.character(naics$principalNAICS2DigitCode)
# naics$principalNAICS2DigitCode[naics$principalNAICS2DigitCode %in% c('31','32','33')]<-'31-33'
# naics$principalNAICS2DigitCode[naics$principalNAICS2DigitCode %in% c('44','45')]<-'44-45'
# naics$principalNAICS2DigitCode[naics$principalNAICS2DigitCode %in% c('48','49')]<-'48-49'
# naics$principalNAICS2DigitCode<-factor(naics$principalNAICS2DigitCode)
# 
# summary(naics$principalnaicscode)
# summary(naics$principalNAICS2DigitCode)
# summary(naics$principalNAICS3DigitCode)
# summary(naics$principalNAICS4DigitCode)
# 
# naics2_adjust<-function(data){
#   data$NAICS2<-as.character(data$NAICS2)
#   data$NAICS2[data$NAICS2 %in% c('21','22','23')]<-'21-23'
#   data$NAICS2[data$NAICS2 %in% c('44','45')]<-'44-45'
#   data$NAICS2[data$NAICS2 %in% c('48','49')]<-'48-49'
#   data$NAICS2<-factor(naics$NAICS2)
# }



clean_entity<-function(data){
  data<-standardize_variable_names(data)
  # data<-deflate(data, #Not compatible with calendar year
  #                               money_var = "Action_Obligation",
  #                               deflator_var="Deflator.2016"
  # )
  # data<-csis360::read_and_join(data,
  #                                              "Lookup_ParentID.csv",
  #                                              path="https://raw.githubusercontent.com/CSISdefense/Lookup-Tables/master/",
  #                                              by="ParentID",
  #                                              directory="vendor//",
  #                                              new_var_checked=FALSE,
  #                                              add_var="Abbreviation"
  #                                              # NA.check.columns="Fair.Competed"
  # )
  # 
  colnames(data)[colnames(data)=="principalNAICScode"]<-"NAICS_Code"
  colnames(data)[colnames(data)=="principalnaicscodeText"]<-"naics_text"
  colnames(data)[colnames(data)=="NAICS_DESCRIPTION"]<-"naics_text"
  
  
  data
  
}



label_naics_mismatch<-function(data){
  
  data$mismatch<-NA
  #11
  data$mismatch[substr(data$NAICS_Code,1,2) %in% c(11)]<-"Not tracked: Agriculture"
  #21
  data$mismatch[substr(data$NAICS_Code,1,6) %in% c(221114,221115,221116,221117,221118)]<-"New in 2012: Missing from Economy Stats"
  #23
  data$mismatch[substr(data$NAICS_Code,1,3) %in% c(233,234,235)]<-"Reassigned in 2002"
  data$mismatch[substr(data$NAICS_Code,1,4) %in% c(2361,2362,2371,2372,2373,2379,2381,2382,2383,2389)]<-"Uncollated at NAICS 4-5"
  #31
  data$mismatch[substr(data$NAICS_Code,1,6) %in% c(311224,311314,311710,313110,313220,313240,313310)]<-"New in 2012: Missing from Economy Stats"
  data$mismatch[substr(data$NAICS_Code,1,6) %in% c(314994,314120,314910)]<-"New in 2012: Missing from Economy Stats"
  data$mismatch[substr(data$NAICS_Code,1,6) %in% c(315110,315190,315210,315220,315990,316210,316998)]<-"New in 2012: Missing from Economy Stats"
  data$mismatch[substr(data$NAICS_Code,1,5) %in% c(31135,31223,31524,31528)]<-"New in 2012: Missing from Economy Stats"
  data$mismatch[substr(data$NAICS_Code,1,6) %in% c(316212)]<-"Reassigned in 2012"
  #32
  data$mismatch[substr(data$NAICS_Code,1,6) %in% c(322219,322220,322230,323120)]<-"New in 2012: Missing from Economy Stats"
  data$mismatch[substr(data$NAICS_Code,1,6) %in% c(325130,325180,325194, 325220,327110,327120)]<-"New in 2012: Missing from Economy Stats"
  #33
  data$mismatch[substr(data$NAICS_Code,1,5) %in% c(33324)]<-"New in 2012: Missing from Economy Stats"
  data$mismatch[substr(data$NAICS_Code,1,6) %in% c(331110,331313,331318,331410,331420,331523,331529)]<-"New in 2012: Missing from Economy Stats"
  data$mismatch[substr(data$NAICS_Code,1,6) %in% c(332119,332215,332216,332613)]<-"New in 2012: Missing from Economy Stats"
  data$mismatch[substr(data$NAICS_Code,1,6) %in% c(333316,333318,333413,333517,333519)]<-"New in 2012: Missing from Economy Stats"
  data$mismatch[substr(data$NAICS_Code,1,6) %in% c(334118,334614,335210)]<-"New in 2012: Missing from Economy Stats"
  data$mismatch[substr(data$NAICS_Code,1,6) %in% c(336310,336320,336390,339910,339930,339940)]<-"New in 2012: Missing from Economy Stats"
  data$mismatch[substr(data$NAICS_Code,1,6) %in% c(339111)]<-"Reassigned in 2007"
  data$mismatch[substr(data$NAICS_Code,1,5) %in% c(33342)]<-"Not tracked: Bad Label"
  
  #42
  data$mismatch[substr(data$NAICS_Code,1,3) %in% c(421,422)]<-"Reassigned in 2002"
  #44
  data$mismatch[substr(data$NAICS_Code,1,5) %in% c(44314)]<-"New in 2012: Missing from Economy Stats"
  data$mismatch[substr(data$NAICS_Code,1,6) %in% c(441228)]<-"New in 2012: Missing from Economy Stats"
  #45
  data$mismatch[substr(data$NAICS_Code,1,6) %in% c(454310)]<-"New in 2012: Missing from Economy Stats"
  #48
  data$mismatch[substr(data$NAICS_Code,1,3) %in% c(482)]<-"Not tracked: Railroads"
  #49
  data$mismatch[substr(data$NAICS_Code,1,3) %in% c(491)]<-"Not tracked: Postal Service"
  #51
  data$mismatch[substr(data$NAICS_Code,1,3) %in% c(513,514)]<-"Reassigned in 2002"
  data$mismatch[substr(data$NAICS_Code,1,3) %in% c(516)]<-"Reassigned in 2007"
  data$mismatch[substr(data$NAICS_Code,1,4) %in% c(5173,5175,5181)]<-"Reassigned in 2007"
  data$mismatch[substr(data$NAICS_Code,1,6) %in% c(517211,517212,517910)]<-"Reassigned in 2007"
  #52
  data$mismatch[substr(data$NAICS_Code,1,3) %in% c(525)]<-"Not tracked: Pension and Other Funds"
  #54
  data$mismatch[substr(data$NAICS_Code,1,5) %in% c(54171)]<-"Scientific R&D Manual Aggregation"
  data$mismatch[substr(data$NAICS_Code,1,5) %in% c(54112)]<-"Not Tracked: Offices of Notaries"
  #56
  data$mismatch[substr(data$NAICS_Code,1,6) %in% c(561310)]<-"Reassigned in 2007"
  #61
  data$mismatch[substr(data$NAICS_Code,1,4) %in% c(6111,6112,6113)]<-"Not tracked: Schools and Universities"
  #72
  data$mismatch[substr(data$NAICS_Code,1,4) %in% c(7225)]<-"New in 2012: Missing from Economy Stats"
  #81
  data$mismatch[substr(data$NAICS_Code,1,4) %in% c(8131)]<-"Not tracked: Religious Organizations"
  data$mismatch[substr(data$NAICS_Code,1,5) %in% c(81393,81394)]<-"Not tracked: Labor Unions and Political Organizations"
  data$mismatch[substr(data$NAICS_Code,1,3) %in% c(814)]<-"Not tracked: Private Households"
  #92
  data$mismatch[substr(data$NAICS_Code,1,2) %in% c(92)]<-"Not tracked: Public Administration"
  #99
  data$mismatch[substr(data$NAICS_Code,1,2) %in% c(99)]<-"Not tracked: Industries not classified"
  
  
  
  data
}




duplicate_NAICS_check<-function(core){
  core$NAICS.id[is.na(core$NAICS.id)]<-core$NAICS_Code[is.na(core$NAICS.id)]
  if(length(core$NAICS.id[core$YEAR.id=="2012"])!=length(unique(core$NAICS.id[core$YEAR.id=="2012"]))){
    stop("Duplicate 2012 entry")
  }
  if(length(core$NAICS.id[core$YEAR.id=="2007"])!=length(unique(core$NAICS.id[core$YEAR.id=="2007"]))){
    stop("Duplicate 2012 entry")
  }
}




summarize_annual_naics<-function(data,naics_level=6){
  data$NAICS_Code<-substr(data$NAICS_Code,1,naics_level)
  if(naics_level==2){
    data$NAICS_Code<-create_naics2(data$NAICS_Code)
  }
  
  data<-data %>% group_by(CalendarYear,exclude,NAICS_Code)
  
  data<-data %>% #filter(Action_Obligation>0) %>%
    dplyr::mutate(
      pos = rank(-Action_Obligation,
                 ties.method ="min"),
      pct = ifelse(Action_Obligation>0,
                   Action_Obligation / sum(Action_Obligation[Action_Obligation>0]),
                   NA
      )
    )
  
  #Learned the filtering approach from
  # https://stackoverflow.com/questions/23438476/dplyr-idiom-for-summarize-a-filtered-group-by-and-also-replace-any-nas-due-to
  
  output<-data %>% group_by(CalendarYear,exclude,NAICS_Code) %>%
    dplyr::summarize( 
      naics_text=ifelse(naics_level==6, max(naics_text,na.rm=TRUE),NA),
      obl = sum(Action_Obligation),#Action_Obligation.Then.Year
      # Obligation.2016 = sum(Action_Obligation.2016),
      cont_count=length(CalendarYear),
      hh_index=sum((pct*100)^2,na.rm=TRUE),
      pct_sum_check=sum(pct,na.rm=TRUE),
      top4=sum(pct[pos<=4],na.rm=TRUE),
      top8=sum(pct[pos<=8],na.rm=TRUE),
      top12=sum(pct[pos<=12],na.rm=TRUE),
      top20=sum(pct[pos<=20],na.rm=TRUE),
      top50=sum(pct[pos<=50],na.rm=TRUE)
    )
  
  output<-label_naics_mismatch(output)
  output$mismatch[output$exclude==TRUE & is.na(output$mismatch)]<-"Not tracked: Excluded at Lower Level"
  not_in_sample<-(output$mismatch %in% get_unstable_list()) | output$hh_index==0
  output$hh_index[not_in_sample]<-NA
  output$top4[not_in_sample]<-NA
  output$top8[not_in_sample]<-NA
  output$top12[not_in_sample]<-NA
  output$top20[not_in_sample]<-NA
  output$top50[not_in_sample]<-NA
  output
}

fill_in_core_gap<-function(data,
                           level,
                           naics_limiter
){ 
  data<-subset(core, nchar(NAICS_Code)==6 & substring(NAICS_Code,1,nchar(naics_limiter))==naics_limiter)
  data$NAICS_Code<-substring(data$NAICS_Code,1,level)
  newrow<-data%>%group_by(YEAR.id,NAICS_Code)%>% 
    dplyr::summarize(rcp=sum(rcp),
                     pay=sum(pay),
                     EMP=sum(as.numeric(EMP))
    )
  newrow$EMP<-as.character(newrow$EMP)
  newrow$GEO.id2<-as.character(NA)
  newrow$NAICS.id<-newrow$NAICS_Code
  newrow$NAICS.display.label<-as.character(NA)
  newrow$OPTAX.id<-as.character(NA)
  newrow$OPTAX.display.label<-as.character(NA)
  newrow$ESTAB<-as.character(NA)
  newrow$RCPTOT<-as.character(NA)
  newrow$PAYANN<-as.character(NA)
  newrow$mismatch<-as.character(NA)
  as.data.frame(newrow[,colnames(data)])
}

impute_from_higher_NAICS<-function(data,naics_level){
  #Input protection
  if(naics_level<=2) stop("There is no higher level than NAICS 2")
  if(naics_level==3) stop("Haven't yet implemented handling for the oddities of NAICS 2 (e.g. 33-35)")
  
  #Create the higher level dataframe
  higher<-subset(data,nchar(NAICS.id)==naics_level-1)
  higher<-higher[c("NAICS.id","YEAR.id","ratio","avg_sal")]
  
  colnames(higher)[colnames(higher)=="NAICS.id"]<-"higher.NAICS.id"
  colnames(higher)[colnames(higher)=="ratio"]<-"higher.ratio"
  colnames(higher)[colnames(higher)=='avg_sal']<-"higher.avg_sal"
  
  #Label targets rows with a NAICS level one higher
  data$higher.NAICS.id<-NA
  impute_list<-nchar(data$NAICS.id)==naics_level & (is.na(data$avg_sal) | is.na(data$ratio)) 
  data[impute_list,]
  data$higher.NAICS.id[impute_list]<-substr(data$NAICS.id[impute_list],1,naics_level-1)
  duplicated(higher[c("higher.NAICS.id","YEAR.id")])
  data<-left_join(data,higher)
  avg_sal_impute_list<-impute_list&is.na(data$avg_sal)
  data$mismatch[avg_sal_impute_list]<-paste(ifelse(is.na(data$mismatch[avg_sal_impute_list]),"",data$mismatch[avg_sal_impute_list]),
                                            "Avg_sal imputed from higher level NAICS",data$higher.NAICS.id[avg_sal_impute_list])
  
  data$avg_sal[avg_sal_impute_list]<-data$higher.avg_sal[avg_sal_impute_list]
  
  ratio_impute_list<-impute_list&is.na(data$ratio)
  data$mismatch[ratio_impute_list]<-paste(ifelse(is.na(data$mismatch[ratio_impute_list]),"",data$mismatch[ratio_impute_list]),
                                          "Ratio imputed from a higher level NAICS: ",data$higher.NAICS.id[ratio_impute_list])
  
  data$ratio[ratio_impute_list]<-data$higher.ratio[ratio_impute_list]
  data[!colnames(data) %in% c("higher.NAICS.id","higher.ratio","higher.avg_sal")]
}



join_economic<-function(data,core,naics_level){
  #Removing mismatch as it can be brought in from data.
  core<-core[!colnames(core) %in% "mismatch"]
  data$census_period<-NA
  data$census_period[data$CalendarYear>=2007 & data$CalendarYear<2012]<-"2007-2011"
  data$census_period[data$CalendarYear>=2012 & data$CalendarYear<2017]<-"2012-2016"
  data$exclude<-"No"
  data$exclude[data$mismatch %in% get_unstable_list()]<-"Not in sample"
  data$exclude[data$mismatch %in% get_exclude_list() | is.na(data$NAICS_Code)]<-"Not in stats"
  
    
  data<-left_join(data,core,by=c("census_period"="census_period","NAICS_Code"="NAICS_Code"))
  data<-read_and_join_experiment(data,
                                          lookup_file = "Lookup_PrincipalNAICScode.csv",
                                          path="https://raw.githubusercontent.com/CSISdefense/Lookup-Tables/master/",
                                          dir="economic\\",
                               by=c("NAICS_Code"="principalnaicscode"),
                               add_var=c("principalnaicscodeText","NAICS_shorthand"),
                               skip_check_var=c("principalnaicscodeText","NAICS_shorthand")
                               
  )
  data$naics_text[is.na(data$naics_text)]<-data$principalnaicscodeText[is.na(data$naics_text)]
  
  
  
  mismatch<-subset(data,!is.na(census_period) &
                     is.na(NAICS.id) &
                     !is.na(NAICS_Code))
  mismatch<-mismatch %>% group_by(NAICS_Code,naics_text,mismatch,exclude) %>%
    dplyr::summarize(obl=sum(obl,na.rm=TRUE),
                     # Obligation.2016=sum(Obligation.2016,na.rm=TRUE),
                     minyear=min(CalendarYear),
                     maxyear=max(CalendarYear)
    )
  
  write.csv(file=paste("Output\\NAICSunmatched",naics_level,".csv",sep=""),
            mismatch,
            row.names = FALSE
  )
  
  summed<-subset(data,!is.na(census_period)) %>% 
    group_by(NAICS_Code,naics_text,mismatch,exclude) %>%
    dplyr::summarize(obl=sum(obl,na.rm=TRUE),
                     # Obligation.2016=sum(Obligation.2016,na.rm=TRUE),
                     rcp=sum(rcp),
                     pay=sum(pay),
                     EMP=sum(as.numeric(EMP)),
                     minyear=min(CalendarYear),
                     maxyear=max(CalendarYear),
                     NAICS.display.label=max(NAICS.display.label,na.rm=TRUE)
    )
  
  
  write.csv(file=paste("Output\\NAICSsummed",naics_level,".csv",sep=""),
            summed,
            row.names = FALSE
            
  )
  # data$footnote<-NA
  # data$footnote[is.na(as.numeric(data$NAICS.id)) | is.na(as.numeric(data$RCPTOT)) | 
  #                       is.na(as.numeric(data$PAYANN))  | is.na(as.numeric(data$EMP))]<-
  #   paste(
  #     ifelse(is.na(as.numeric(data$NAICS.id)),paste("NAICS.id:",data$NAICS.id,""),""),
  #         ifelse(is.na(as.numeric(data$RCPTOT)),paste("RCPTOT:",data$RCPTOT,""),""),
  #         ifelse(is.na(as.numeric(data$PAYANN)),paste("PAYANN:",data$PAYANN,""),""),
  #         ifelse(is.na(as.numeric(data$EMP)),paste("NAICS.id:",data$EMP,""),"")
  #         )
  # 
  data$naics_text[!is.na(data$NAICS.display.label)]<-data$NAICS.display.label[!is.na(data$NAICS.display.label)]
  
  overall_naics<-subset(data,!is.na(census_period))
  
  colnames(overall_naics)[colnames(overall_naics)=="EMP"]<-"emp"
  colnames(overall_naics)[colnames(overall_naics)=="ESTAB"]<-"estab"

  
  overall_naics<-overall_naics[order(overall_naics$CalendarYear,overall_naics$exclude,overall_naics$NAICS_Code),] 
  overall_naics<-overall_naics[colnames(overall_naics) %in% c("CalendarYear","exclude", "NAICS_Code","naics_text",
                                  "mismatch",
                                  "obl","period_obl",  "rcp","ratio","OPTAX.display.label",  
                                  "pay","emp", "avg_sal",
                                  "cont_count", "estab",
                                  "hh_index",  "top4", "top8", "top12", "top20", "top50"
  )]
  overall_naics<-overall_naics[c("CalendarYear","exclude", "NAICS_Code","naics_text",
                                  "mismatch",
                                  "obl", "period_obl", "rcp","ratio","OPTAX.display.label",  
                                  "pay","emp", "avg_sal",
                                  "cont_count", "estab",
                                  "hh_index",  "top4", "top8", "top12", "top20", "top50"
  )]
  
  colnames(overall_naics)[colnames(overall_naics) %in% c("obl", "period_obl","ratio", "cont_count", "hh_index", "pct_sum_check",
                                                         "top4", "top8", "top12", "top20", "top50", "mismatch")]<-
    paste("def",naics_level,"_",colnames(overall_naics)[colnames(overall_naics) %in% c("obl", "period_obl","ratio","cont_count", "hh_index", "pct_sum_check",
                                                                                       "top4", "top8", "top12", "top20", "top50","mismatch")],sep="")
  
  colnames(overall_naics)[colnames(overall_naics) %in% c("rcp", "pay", "avg_sal", "emp", "estab")]<-
    paste("US",naics_level,"_",colnames(overall_naics)[colnames(overall_naics) %in% c(
      "rcp", "pay", "avg_sal", "emp", "estab")],sep="")
  
  
  
  # write.csv(overall_naics,file=paste("Output\\overall_naics",naics_level,".csv",sep=""),
  #           row.names = FALSE)
  
  overall_naics
}



get_exclude_list<-function(){
  c("Not tracked: Agriculture",
    "Not tracked: Railroads",
    "Not tracked: Postal Service",
    "Not tracked: Pension and Other Funds",
    "Not tracked: Schools and Universities",
    "Not tracked: Labor Unions and Political Organizations",
    "Not Tracked: Offices of Notaries",
    "Not tracked: Religious Organizations",
    "Not tracked: Private Households",
    "Not tracked: Industries not classified",
    "Not tracked: Bad Label",
    "Not tracked: Excluded at Lower Level"
  )
}

get_unstable_list<-function(){
  c("Reassigned in 2002",
  "Reassigned in 2012",
  "New in 2012: Missing from Economy Stats",
  "Reassigned in 2007")
}


