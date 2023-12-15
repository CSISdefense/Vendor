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


award_summary<-function(df){
  minmax<-function(x){
    ifelse(min(x)==max(x),min(x),NA)
  }
  minmod<-function(x,mod){
    min(ifelse(is.na(mod) | mod==min(mod,na.rm=FALSE),x,NA),na.rm = TRUE)
  }
  
  latest<-function(x,date){
    min(ifelse(date==max(date),x,NA),na.rm = TRUE)
  }
  
  loan %>% group_by(assistance_award_unique_key) %>%
    summarise(
      # assistance_transaction_unique_key=minmax(assistance_transaction_unique_key),
      assistance_award_unique_key=minmax(assistance_award_unique_key),
      recipient_name=minmax(recipient_name),
      transaction_count=length(assistance_award_unique_key),
      cfda_num=minmax(cfda_num),
      cfda_title=minmax(cfda_title),
      # sai_number=minmax(sai_number),
      assistance_type_code=minmax(assistance_type_code),
      assistance_type_description=minmax(assistance_type_description),
      prime_award_base_transaction_description=minmax(prime_award_base_transaction_description),
      period_of_performance_start_date=minmod(period_of_performance_start_date,modification_number),
      initial_period_of_performance_current_end_date=minmod(period_of_performance_current_end_date,modification_number),
      federal_action_obligation=sum(federal_action_obligation,na.rm=TRUE),
      original_loan_subsidy_cost=sum(original_loan_subsidy_cost,na.rm=TRUE),
      # latest_total_outlayed_amount_for_overall_award=latest(total_outlayed_amount_for_overall_award,action_date),
      face_value_of_loan=sum(face_value_of_loan,na.rm=TRUE),
      non_federal_funding_amount=sum(non_federal_funding_amount,na.rm=TRUE),
      indirect_cost_federal_share_amount=sum(indirect_cost_federal_share_amount,na.rm=TRUE),
      # latest_transaction_description=latest(transaction_description,action_date),
      max_last_modified_date=max(last_modified_date),
      award_id_fain=minmax(award_id_fain),
      # modification_number=minmax(modification_number),
      award_id_uri=minmax(award_id_uri),
      sai_number=minmax(sai_number),
      federal_action_obligation=minmax(federal_action_obligation),
      total_obligated_amount=minmax(total_obligated_amount),
      total_outlayed_amount_for_overall_award=minmax(total_outlayed_amount_for_overall_award),
      indirect_cost_federal_share_amount=minmax(indirect_cost_federal_share_amount),
      non_federal_funding_amount=minmax(non_federal_funding_amount),
      total_non_federal_funding_amount=minmax(total_non_federal_funding_amount),
      face_value_of_loan=minmax(face_value_of_loan),
      original_loan_subsidy_cost=minmax(original_loan_subsidy_cost),
      total_face_value_of_loan=minmax(total_face_value_of_loan),
      total_loan_subsidy_cost=minmax(total_loan_subsidy_cost),
      disaster_emergency_fund_codes_for_overall_award=minmax(disaster_emergency_fund_codes_for_overall_award),
      outlayed_amount_from_COVID.19_supplementals_for_overall_award=minmax(outlayed_amount_from_COVID.19_supplementals_for_overall_award),
      obligated_amount_from_COVID.19_supplementals_for_overall_award=minmax(obligated_amount_from_COVID.19_supplementals_for_overall_award),
      outlayed_amount_from_IIJA_supplemental_for_overall_award=minmax(outlayed_amount_from_IIJA_supplemental_for_overall_award),
      obligated_amount_from_IIJA_supplemental_for_overall_award=minmax(obligated_amount_from_IIJA_supplemental_for_overall_award),
      action_date=minmax(action_date),
      action_date_fiscal_year=minmax(action_date_fiscal_year),
      period_of_performance_start_date=minmax(period_of_performance_start_date),
      period_of_performance_current_end_date=minmax(period_of_performance_current_end_date),
      awarding_agency_code=minmax(awarding_agency_code),
      awarding_agency_name=minmax(awarding_agency_name),
      awarding_sub_agency_code=minmax(awarding_sub_agency_code),
      awarding_sub_agency_name=minmax(awarding_sub_agency_name),
      awarding_office_code=minmax(awarding_office_code),
      awarding_office_name=minmax(awarding_office_name),
      funding_agency_code=minmax(funding_agency_code),
      Funding_Agency_Name=minmax(Funding_Agency_Name),
      funding_sub_agency_code=minmax(funding_sub_agency_code),
      funding_sub_agency_name=minmax(funding_sub_agency_name),
      funding_office_code=minmax(funding_office_code),
      funding_office_name=minmax(funding_office_name),
      treasury_accounts_funding_this_award=minmax(treasury_accounts_funding_this_award),
      federal_accounts_funding_this_award=minmax(federal_accounts_funding_this_award),
      object_classes_funding_this_award=minmax(object_classes_funding_this_award),
      program_activities_funding_this_award=minmax(program_activities_funding_this_award),
      recipient_uei=minmax(recipient_uei),
      recipient_duns=minmax(recipient_duns),
      recipient_name=minmax(recipient_name),
      recipient_name_raw=minmax(recipient_name_raw),
      recipient_parent_uei=minmax(recipient_parent_uei),
      recipient_parent_duns=minmax(recipient_parent_duns),
      recipient_parent_name=minmax(recipient_parent_name),
      recipient_parent_name_raw=minmax(recipient_parent_name_raw),
      recipient_country_code=minmax(recipient_country_code),
      recipient_country_name=minmax(recipient_country_name),
      recipient_address_line_1=minmax(recipient_address_line_1),
      recipient_address_line_2=minmax(recipient_address_line_2),
      recipient_city_code=minmax(recipient_city_code),
      recipient_city_name=minmax(recipient_city_name),
      prime_award_transaction_recipient_county_fips_code=minmax(prime_award_transaction_recipient_county_fips_code),
      recipient_county_name=minmax(recipient_county_name),
      prime_award_transaction_recipient_state_fips_code=minmax(prime_award_transaction_recipient_state_fips_code),
      recipient_state_code=minmax(recipient_state_code),
      recipient_state_name=minmax(recipient_state_name),
      recipient_zip_code=minmax(recipient_zip_code),
      recipient_zip_last_4_code=minmax(recipient_zip_last_4_code),
      prime_award_transaction_recipient_cd_original=minmax(prime_award_transaction_recipient_cd_original),
      prime_award_transaction_recipient_cd_current=minmax(prime_award_transaction_recipient_cd_current),
      recipient_foreign_city_name=minmax(recipient_foreign_city_name),
      recipient_foreign_province_name=minmax(recipient_foreign_province_name),
      recipient_foreign_postal_code=minmax(recipient_foreign_postal_code),
      primary_place_of_performance_scope=minmax(primary_place_of_performance_scope),
      primary_place_of_performance_country_code=minmax(primary_place_of_performance_country_code),
      primary_place_of_performance_country_name=minmax(primary_place_of_performance_country_name),
      primary_place_of_performance_code=minmax(primary_place_of_performance_code),
      primary_place_of_performance_city_name=minmax(primary_place_of_performance_city_name),
      prime_award_transaction_place_of_performance_county_fips_code=minmax(prime_award_transaction_place_of_performance_county_fips_code),
      primary_place_of_performance_county_name=minmax(primary_place_of_performance_county_name),
      prime_award_transaction_place_of_performance_state_fips_code=minmax(prime_award_transaction_place_of_performance_state_fips_code),
      primary_place_of_performance_state_name=minmax(primary_place_of_performance_state_name),
      primary_place_of_performance_zip_4=minmax(primary_place_of_performance_zip_4),
      prime_award_transaction_place_of_performance_cd_original=minmax(prime_award_transaction_place_of_performance_cd_original),
      prime_award_transaction_place_of_performance_cd_current=minmax(prime_award_transaction_place_of_performance_cd_current),
      primary_place_of_performance_foreign_location=minmax(primary_place_of_performance_foreign_location),
      cfda_number=minmax(cfda_number),
      cfda_title=minmax(cfda_title),
      funding_opportunity_number=minmax(funding_opportunity_number),
      funding_opportunity_goals_text=minmax(funding_opportunity_goals_text),
      assistance_type_code=minmax(assistance_type_code),
      transaction_description=minmax(transaction_description),
      prime_award_base_transaction_description=minmax(prime_award_base_transaction_description),
      business_funds_indicator_code=minmax(business_funds_indicator_code),
      business_funds_indicator_description=minmax(business_funds_indicator_description),
      business_types_code=minmax(business_types_code),
      business_types_description=minmax(business_types_description),
      correction_delete_indicator_code=minmax(correction_delete_indicator_code),
      correction_delete_indicator_description=minmax(correction_delete_indicator_description),
      action_type_code=minmax(action_type_code),
      action_type_description=minmax(action_type_description),
      record_type_code=minmax(record_type_code),
      record_type_description=minmax(record_type_description),
      highly_compensated_officer_1_name=minmax(highly_compensated_officer_1_name),
      highly_compensated_officer_1_amount=minmax(highly_compensated_officer_1_amount),
      highly_compensated_officer_2_name=minmax(highly_compensated_officer_2_name),
      highly_compensated_officer_2_amount=minmax(highly_compensated_officer_2_amount),
      highly_compensated_officer_3_name=minmax(highly_compensated_officer_3_name),
      highly_compensated_officer_3_amount=minmax(highly_compensated_officer_3_amount),
      highly_compensated_officer_4_name=minmax(highly_compensated_officer_4_name),
      highly_compensated_officer_4_amount=minmax(highly_compensated_officer_4_amount),
      highly_compensated_officer_5_name=minmax(highly_compensated_officer_5_name),
      highly_compensated_officer_5_amount=minmax(highly_compensated_officer_5_amount),
      usaspending_permalink=minmax(usaspending_permalink),
      last_modified_date=minmax(last_modified_date),
      USAspending_file_name=minmax(USAspending_file_name),
      cfda_num=minmax(cfda_num),
      assistance_type_description=minmax(assistance_type_description),
    )
  x
}

# View(award_summary(loan) %>% filter(!assistance_type_code==11|is.na(assistance_type_code==11)))



drop_empties<-function(df){
  for(c in colnames(df)){
    if(all(is.na(df[,c])))
      df<-df[,!colnames(df)==c]
  }
  df
}
loanEnergy<-drop_empties(loanEnergy)
colnames(loanEnergy)

loanEnergy<-standard_assistance_lookups(loanEnergy)

summary(factor(loan$cfda_num))
summary(factor(loan$cfda_number))
loan$cfda_number[is.na(loan$cfda_num)]

loan<-standard_assistance_lookups(loan)

colnames(loanEnergy)[!colnames(loanEnergy) %in% colnames(loan_award)]



write.csv()
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


list.7a<-file.list[grep("^foia-7a",file.list)]
sba.7a<-rbind_files(list.7a,file.path("Data_Raw","Assistance"))

list.sbg<-file.list[grep("^foia-sbg",file.list)]
sba.sbg<-rbind_files(list.sbg,file.path("Data_Raw","Assistance"))

sbic_providers<-read_csv(file.path("Data_Raw","Assistance","sbic_contacts.csv"),na = "na")

save(sba.504,sba.7a,sba.sbg,sbic_providers,file=file.path("data","semi_Clean","OSC","sba_programs.rda"))