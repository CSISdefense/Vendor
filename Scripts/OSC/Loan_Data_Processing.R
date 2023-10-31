
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
save(loan,file="data/semi_clean/OSC/FAADCloanDataSet.rda")
load(file="data/semi_clean/OSC/FAADCloanDataSet.rda")
loan<-loan %>% filter(assistance_type_code!="09")

