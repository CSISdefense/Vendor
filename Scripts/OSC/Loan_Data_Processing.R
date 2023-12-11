
###############################################################
# Sync FPDStypetable.ProductOrServiceCode with repo version
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


login<-askpass("Please enter the login account")
pwd<-askpass("Please enter the account password")

con <- dbConnect(odbc(),
                 Driver = "SQL Server",
                 Server = "vmsqldiig.database.windows.net",
                 Database = "CSIS360",
                 UID = login,
                 PWD =pwd)



loan<-dbReadTable(con,  name = SQL('"Assistance"."OSCloanDataSet"'))
loan$cfda_num<-round(text_to_number(loan$cfda_number),3)
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



loan$cdfa_title
load(file="data/semi_clean/OSC/FAADCloanDataSet.rda")






test<-loan %>% group_by(cfda_title,cfda_num,assistance_type_code) %>% summarise(n=length(assistance_type_code)) %>% arrange(cfda_num)



