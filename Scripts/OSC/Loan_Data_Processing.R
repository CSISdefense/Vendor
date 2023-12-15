###############################################################
# Load and process loan data from multiple sourcse
################################################################
library(dplyr)
library(tidyr)
library(tidyverse)
library(csis360)
library(readr)
library(sqldf)
library(odbc)
library(askpass)
library(DBI)
library(askpass)
library(openxlsx)
library(tidyverse)

login<-askpass("Please enter the login account")
pwd<-askpass("Please enter the account password")

con <- dbConnect(odbc(),
                 Driver = "SQL Server",
                 Server = "vmsqldiig.database.windows.net",
                 Database = "CSIS360",
                 UID = login,
                 PWD =pwd)



loan<-dbReadTable(con,  name = SQL('"Assistance"."OSCloanDataSet"'))
# load(file="data/semi_clean/OSC/EnergyLoanDataSet.rda")


standard_assistance_lookups<-function(df){
  df<-df %>% mutate(cfda_num=ifelse(
    nchar(cfda_number)>6,
    round(text_to_number(cfda_number),3),
    cfda_number))
  if(any(!is.na(df$cfda_number)&is.na(df$cfda_num)))
    stop("Mangled CFDA number")
  df$assistance_type_code=text_to_number(df$assistance_type_code)
  df<-read_and_join_experiment(df,directory="assistance//",lookup_file="assistance_type_code.csv",
                               by="assistance_type_code")
  df
}


# View(award_summary(loan) %>% filter(!assistance_type_code==11|is.na(assistance_type_code==11)))



drop_empties<-function(df){
  for(c in colnames(df)){
    if(all(is.na(df[,c])))
      df<-df[,!colnames(df)==c]
  }
  df
}
loan<-drop_empties(loan)
summary(factor(loanSelected$awarding_agency_name))
loanEnergyAward<-loanSelected %>% filter(awarding_agency_name=="Department of Energy")
loanEnergy<-drop_empties(loanEnergy)
loanEnergyAward<-award_summary(loanEnergy)
colnames(loanSelected)[!colnames(loanSelected) %in% colnames(loanEnergy)]


colnames(loan)

loan<-standard_assistance_lookups(loan)

summary(factor(loan$cfda_num))
summary(factor(loan$cfda_number))
loan$cfda_number[is.na(loan$cfda_num)]

colnames(loanEnergy)[!colnames(loanEnergy) %in% colnames(loan_award)]


write.csv(loan %>% group_by(assistance_type_code,assistance_type_description)%>%filter(!is.na(assistance_type_description)) %>%
  summarise(),file="assistance_type_code.csv",row.names = FALSE)


cfda_summary<-loan %>% group_by(cfda_title,cfda_num,assistance_type_code) %>% 
  summarise(n=length(assistance_type_code),
            total_outlayed_amount_for_overall_award=sum(total_outlayed_amount_for_overall_award,na.rm=TRUE),
            face_value_of_loan=sum(face_value_of_loan,na.rm=TRUE),
            min_period_of_performance_start_date =min(period_of_performance_start_date))  %>%
  arrange(cfda_num)
write.csv(cfda_summary,"data/semi_clean/OSC/cfda_summary.csv",row.names = FALSE)

save(loan,file="data/semi_clean/OSC/FAADCloanDataSet.rda")

#Over 13 million and no way to identify critical tech, seperating 
loanPPP<-loan %>% filter(cfda_num==59.073)
save(loanPPP,file="data/semi_clean/OSC/PPPloanDataSet.rda")

#Around 6 million, also no clear way to identify critical tech
loanDisaster <-loan  %>% filter(cfda_num %in% c(59.008,59.063))
save(loanDisaster,file="data/semi_clean/OSC/SBAdisasterLoanDataSet.rda")

#These are the loans we have paths to identify critical technologies
loanSelected<- loan %>% filter(cfda_num %in% c(31.007,59.011,59.012,59.016,59.041,59.054,81.126))
save(loanSelected,file="data/semi_clean/OSC/SelectedLoanDataSet.rda")

loanOther <- loan %>% filter(!cfda_num %in% c(31.007,59.011,59.012,59.016,59.041,59.054,81.126,
                                     59.073,
                                     59.008,59.063))
save(loanOther,file="data/semi_clean/OSC/OtherLoanDataSet.rda")


load(file="data/semi_clean/OSC/SelectedLoanDataSet.rda")

###ExIm bank
exim<-read_csv("Data_Raw/Loans/Authorizations_From_10_01_2006_Thru_12_31_2022.csv",na = "N/A")
colnames(exim)<-gsub(" ",".",colnames(exim))
colnames(exim)<-gsub("/",".",colnames(exim))
exim$Primary.Export.Product.NAICS<-NA
exim$Primary.Export.Product.SIC<-NA
exim$Primary.Export.Product.NAICS[nchar(exim$Primary.Export.Product.NAICS.SIC.code)==6
                                  & !is.na(exim$Primary.Export.Product.NAICS.SIC.code)]<-
  text_to_number(exim$Primary.Export.Product.NAICS.SIC.code[nchar(exim$Primary.Export.Product.NAICS.SIC.code)==6
                                                            & !is.na(exim$Primary.Export.Product.NAICS.SIC.code)])
exim$Primary.Export.Product.SIC[nchar(exim$Primary.Export.Product.NAICS.SIC.code)<6
                                  & !is.na(exim$Primary.Export.Product.NAICS.SIC.code)]<-
  exim$Primary.Export.Product.NAICS.SIC.code[nchar(exim$Primary.Export.Product.NAICS.SIC.code)<6
                                                            & !is.na(exim$Primary.Export.Product.NAICS.SIC.code)]

View(exim[!duplicated(exim$Primary.Export.Product.SIC),"Primary.Export.Product.SIC"]%>%arrange(Primary.Export.Product.SIC))

#SIC codes? sometimes have a letter as a 5th character
exim$Primary.Export.Product.SIC.code<-NA
exim$Primary.Export.Product.SIC.code[!is.na(exim$Primary.Export.Product.NAICS.SIC.code)&
                                          is.na(exim$Primary.Export.Product.SIC.code)&
                                       nchar(exim$Primary.Export.Product.NAICS.SIC.code)<6]<-
  text_to_number(
    substr(exim$Primary.Export.Product.SIC.code[!is.na(exim$Primary.Export.Product.NAICS.SIC.code)&
                                                              is.na(exim$Primary.Export.Product.SIC.code)&
                                                  nchar(exim$Primary.Export.Product.NAICS.SIC.code)<6],1,4))
exim<-read_and_join_experiment(exim,
                                     lookup_file="Lookup_PrincipalNAICScode.csv",
                                     directory="economic//",
                                     by=c("Primary.Export.Product.NAICS"="principalnaicscode"),
                                     add_var=c("principalnaicscodeText"),
                                     skip_check_var =c("principalnaicscodeText"),
                                     missing_file="naics.csv")




list.files(file.path("Data_Raw","Economic"))
file.exists(file.path("Data_Raw","Economic","1987_SIC_to_1997_NAICS.xls"))
# sic1997<-readWorkbook(file.path("Data_Raw","Economic","1987_SIC_to_1997_NAICS.xls"))
sic1997<-read_csv(file.path("Data_Raw","Economic","1987_SIC_to_1997_NAICS.csv"))
sic1997complete<-sic1997 %>% filter(is.na(`Part Indicator`))
colnames(sic1997complete)<-colnames(sic1997complete) %>% make.names()
write_csv(sic1997complete,file.path("Data_Raw","Economic","1987_SIC_to_1997_NAICS_complete.csv"))
colnames(sic1997complete)
exim<-read_and_join_experiment(exim,
                               lookup_file="1987_SIC_to_1997_NAICS_complete.csv",
                               directory="data_raw//economic//",
                               path="",
                               by=c("Primary.Export.Product.SIC.code"="SIC"),
                               add_var=c("SIC.Titles.and.Part.Descriptions"),
                               skip_check_var =c("SIC.Titles.and.Part.Description"),
                               missing_file="sic.csv")




save(exim,file="Data/Semi_Clean/OSC/exim.rda")


#


rbind_files<-function(files,path){
  for(i in 1:length(files)){
    if(i==1)
      x<-read_csv(file.path(path,files[i]))
    else{
      y<-read_csv(file.path(path,files[i]))
      x<-rbind(x,y)
      if(ncol(x)!=ncol(y) | any(colnames(x)!=colnames(y))){
        warning(colnames(x)[!colnames(x) %in% colnames(y)])
        warning(colnames(y)[!colnames(y) %in% colnames(x)])
        stop("Column Names mismatch")
      }
    }
  }
  x
}


file.list<-list.files(file.path("Data_Raw","Assistance"))
file.list<-file.list[gsub("^.*\\.","",file.list)=="csv"]

list.504<-file.list[grep("^foia-504",file.list)]
sba.504<-rbind_files(list.504,file.path("Data_Raw","Assistance"))

test<-as.Date(sba.sbg$PROJECT_START_DATE,"%m/%d/%Y")




list.7a<-file.list[grep("^foia-7a",file.list)]
sba.7a<-rbind_files(list.7a,file.path("Data_Raw","Assistance"))

list.sbg<-file.list[grep("^foia-sbg",file.list)]
sba.sbg<-rbind_files(list.sbg,file.path("Data_Raw","Assistance"))

if(any(is.na(test)&!is.na(sba.sbg$PROJECT_START_DATE)&
       sba.sbg$PROJECT_START_DATE!="###############################################################################################################################################################################################################################################################")){
  sba.sbg$PROJECT_START_DATE[is.na(test)&!is.na(sba.sbg$PROJECT_START_DATE)]
  stop("Malformed date")
} else {
  sba.sbg$PROJECT_START_DATE<-test
  sba.sbg$ProjectStartYear<-year(sba.sbg$PROJECT_START_DATE)
  rm(test)
}



sbic_providers<-read_csv(file.path("Data_Raw","Assistance","sbic_contacts.csv"),na = "na")

save(sba.504,sba.7a,sba.sbg,sbic_providers,file=file.path("data","semi_Clean","OSC","sba_programs.rda"))
