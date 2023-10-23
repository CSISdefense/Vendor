###############################################################
# Get Top 10 rows of aggregated Amount and # of action
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

# path<-"C:\\Users\\grego\\Repositories\\USAspending-local\\"
# path<-"F:\\Users\\Greg\\Repositories\\USAspending-local\\"

path<-"F:\\Users\\gsanders\\Documents\\Repositories\\USAspending-local\\"
dir<-"Agency Assistance"

load("F:\\Users\\gsanders\\Documents\\Repositories\\Vendor\\data\\semi_clean\\OSC\\FAADCloanDataSet.rda")

dir.exists(file.path(path,dir,"XIMB"))
Agencies<-c("XIMB","Commerce","SBA","DoD","Energy")



for(agency in Agencies){
  if(!dir.exists(file.path(path,dir,agency)))
    stop(paste("Missing Directory"),file.path(path,dir,agency))
  full_path<-file.path(path,dir,agency)
  files<-list.files(full_path) 
  files<-files[!files %in% gsub(paste(full_path,"/",sep=""),"",list.dirs(full_path))]
  flen<-nchar(files)
  
  # extension<-gsub("^.*\\.","",files)
  # dirs<-gsub("\\.pdf$","",files)
  # dirs<-gsub("\\.xlsx$","",dirs)
  # dirs<-gsub("\\.xls$","",dirs)
  # 
  # # shorten_name<-function(x){
  # #   x<-gsub("Research, Development, Test and Evaluation","RDTnE",x)
  # #   x<-gsub("MasterJustificationBook","",x)
  # # }
  # 
  # safe_name<-function(x){
  #   x<-shorten_name(x)
  #   x<-gsub(" ","_",x)
  #   x<-gsub("&","and",x)
  #   x<-gsub("[(|)|,]","",x)  
  # }
  # 
  # dirs<-safe_name(dirs)
  # 
  # 
  # file.rename(from=file.path(full_path,files),
  #             to=file.path(full_path,shorten_name(files)))
  # 
  
  dirs<-list.dirs(file.path(full_path,dir,agency),recursive = FALSE)
  file<-list.files(full_path)
  file<-file[gsub("^.*\\.","",file)=="zip"]
  
  #Extract all the attachments
  for(f in file){
    unzip(file.path(full_path,f),overwrite=FALSE,junkpaths=TRUE,exdir=full_path)
    
  }
}


i<-read_csv(file.path(full_path,dir,agency,"FY2023_All_Contracts_Full_20230811_1.csv"),n_max=1000000,guess_max=1000000)
# f<-read_delim("C:\\Users\\grego\\Repositories\\DIIGsql\\data_raw\\Errorlogging.FlatFileErrors.csv",delim=",",skip=1,col_names=FALSE)

max(nchar(i$prime_award_transaction_place_of_performance_cd_original),na.rm=TRUE)

stage1<-read_csv("https://raw.githubusercontent.com/CSISdefense/Lookup-Tables/master/ImportAids/ErrorLogging_FPDSstage1_size.csv")
filecheck<-data.frame(colname=colnames(i))
#List columns dropped from the new file that are present in Errorlogging.FPDSstage1
#these may have been renamed.
stage1 %>% filter(!colname %in% filecheck$colname &
                    !colname %in% c("CSISmodifiedDate", "CSIScreatedDate",
                                    "IsDuplicateUTI","number_of_employees",
                                    "annual_revenues", "USAspending_file_name"))

# max(i$last_modified_date)

login<-askpass("Please enter the login account")
pwd<-askpass("Please enter the account password")

con <- dbConnect(odbc(),
                 Driver = "SQL Server",
                 Server = "vmsqldiig.database.windows.net",
                 Database = "CSIS360",
                 UID = login,
                 PWD =pwd)


agency<-"SBA"
file.list<-list.files(file.path(path,dir,agency))
file.list<-file.list[gsub("^.*\\.","",file.list)=="csv"]
# Error: nanodbc/nanodbc.cpp:1769: 22001: [Microsoft][ODBC SQL Server Driver]String data, right truncation 
#My working theory is that in the absence of name matches, it assigns columns to the database
#table in order, and that this lead to a truncation error that disappeared once all the names
#matched.

loan<-dbReadTable(con,  name = SQL('"Assistance"."OSCloanDataSet"'))
save(loan,file="data/semi_clean/OSC/FAADCloanDataSet.rda")
dbDisconnect(con)

rm(loan)


#6:41 am run  8/29
#On to second set of 100k by 7:01, which included file import. 20 min / file on desktop?
#8:00 am run? on to set three by 8:43
#1 million rows finished at 10:44Am, so may 2h45m
#Error: nanodbc/nanodbc.cpp:1769: 22001: [Microsoft][ODBC SQL Server Driver]String data, right truncation 
for (file.name in 1:35){
  print(file.list[file.name])
  i<-read_csv(file.path(path,dir,agency,file.list[file.name]),n_max=1000000,guess_max=1000000)
  #Only loans, using code because the name field is often blank
  i <-i %>% filter(assistance_type_code %in% c("08","8","11"))
  if(nrow(i)==0)
    next
  i<- as.data.frame(i)
  i$USAspending_file_name<-file.list[file.name]
  filecheck<-data.frame(colname=colnames(i))
  filecheck$maxlen<-NULL
  for(c in 1:nrow(filecheck)){
    if (numbers::mod(c,10) == 0) print(c)
    filecheck$maxlen[c]<-max(sapply(i[,filecheck$colname[c]],nchar),na.rm=TRUE)
  }
  filecheck$importtype<-sapply(i,class)
  t<-read_and_join_experiment(filecheck,"ErrorLogging_FAADCstage1_size.csv",directory="ImportAids//",skip_check_var = "stage1size")
  t$stage1size<-text_to_number(t$stage1size)
  if(nrow(t %>% filter (maxlen>=stage1size))>0){
    t %>% filter (maxlen>=stage1size)
    # stop("Column length will lead to truncation")
  }
  distinct(t %>% select(stage1type,importtype))
  if(nrow(t %>% filter (stage1type %in% c("decimal","date")&importtype=="character" ))){
    t %>% filter (stage1type %in% c("decimal","date")&importtype=="character" )
    stop("Column type mismatch")
  }
  
  # write_csv(t %>% filter(!is.na(stage1size)),"match.csv")
  #started running at 10:09 pm finished by 6 am, 3/4 done by 12:30 am
  #5:10
  print(nrow(i))
  for (r in 0:9){
    start<-(r*100000)+1
    end<-((r+1)*100000)
    #Stop when we've reached the end of imports
    if(start>nrow(i)) {break}
    if(end>nrow(i)) {end<-nrow(i)}
    print(c(start,end))
    dbAppendTable(conn = con, 
                 name = SQL('"ErrorLogging"."FAADCstage1"'), 
                 value = i[start:end,])  ## x is any data frame
    #https://stackoverflow.com/questions/66864660/r-dbi-sql-server-dbwritetable-truncates-rows-field-types-parameter-does-not-w
    # values <- DBI::sqlAppendTable(
    #   con = con,
    #   table = Id(database = "CSIS360", schema = "ErrorLogging", table = "FPDSstage1"),
    #   values = i[start:end,])
    # DBI::dbExecute(conn = con, values)
  }
}

#Note, transactions will not commit until you disconnect! Whether or not
#the computer maintains this connection in the meantime doesn't matter,
#formal disconnection is required.
dbDisconnect(con)


#ROBDC is not available  for this version of R

#Failure with 2023/08 file
# Error: nanodbc/nanodbc.cpp:1769: 42000: [Microsoft][ODBC SQL Server Driver][SQL Server]Invalid column name 'prime_award_transaction_recipient_cd_original'.  [Microsoft][ODBC SQL Server Driver][SQL Server]Invalid column name 'prime_award_transaction_recipient_cd_current'.  [Microsoft][ODBC SQL Server Driver][SQL Server]Invalid column name 'prime_award_transaction_place_of_performance_cd_original'.  [Microsoft][ODBC SQL Server Driver][SQL Server]Invalid column name 'prime_award_transaction_place_of_performance_cd_current'.  [Microsoft][ODBC SQL Server Driver][SQL Server]Statement(s) could not be prepared.


#started running at 10:09 pm
#By 10:45 (I was off taking a shower, it failed)
#100000 can be written successfully, but 1 m fails
# Error: nanodbc/nanodbc.cpp:1769: 08S01: [Microsoft][ODBC SQL Server Driver][DBNETLIB]ConnectionWrite (send()).  [Microsoft][ODBC SQL Server Driver][DBNETLIB]General network error. Check your network documentation. 

#100000k success
#11:05 starting again


#https://stackoverflow.com/questions/14334840/how-to-insert-a-dataframe-into-a-sql-server-table
#https://stackoverflow.com/questions/54692733/how-to-insert-r-dataframe-into-existing-table-in-sql-server
#https://stackoverflow.com/questions/1402001/ms-sql-bulk-insert-with-rodbc/42489082#42489082




#Check for misaligned columns
# any(is.na(ie[,ncol(ie)]))
# view(ie[,ncol(ie)])
# max(nchar(ie$solicitation_identifier),na.rm=TRUE)
# view(ie$solicitation_identifier)
# unique(stringi::stri_enc_mark(ie$solicitation_identifier))
# 
# max(nchar(i$disaster_emergency_fund_codes_for_overall_award),na.rm=TRUE)
# max(nchar(i$treasury_accounts_funding_this_award),na.rm=TRUE)
# max(nchar(i$federal_accounts_funding_this_award),na.rm=TRUE)
# max(nchar(i$object_classes_funding_this_award),na.rm=TRUE)
# max(nchar(i$program_activities_funding_this_award),na.rm=TRUE)
# 
# 


#https://usaspending-help.zendesk.com/hc/en-us/community/posts/360033231714-Unique-Transaction-Keys-Added-to-Contract-and-Financial-Assistance-Downloads-
# The following Unique Transaction IDs have been added to the downloads across USAspending.gov. 
# 
# 1. For Contract Award records:
#   
#   A concatenation of agencyID, parent_award_agency_id, award_id_piid, modification_number, parent_award_id, and transaction_number, with a single underscore ('_') character inserted in between each. If a field is blank, it is recorded as "-NONE-". agencyID is an FPDS field that captures the SubTier Agency that submitted the transaction to FPDS (often distinct from the awarding agency). These same six fields are part of the unique key for contract awards in FPDS.

# 3. Contract IDV Award records:
  
# The same format as Contract Award is used, but only agencyID, award_id_piid, and modification_number are filled in, even if additional fields in the key are present in the IDV record; the rest of the fields are recorded as "- NONE-" for unique key purposes, even when present. This follows the unique key model for contract IDVs in FPDS.

