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

login<-askpass("Please enter the login account")
pwd<-askpass("Please enter the account password")

con <- dbConnect(odbc(),
                 Driver = "SQL Server",
                 Server = "vmsqldiig.database.windows.net",
                 Database = "CSIS360",
                 UID = login,
                 PWD =pwd)


#Run started at 11:30 pm 2/1. Still going at 9:04 Am am. 2/2
munition<-dbGetQuery(con, "SELECT [CSIScontractID]
      ,[signeddate]
      ,[obligatedAmount]
      ,[baseandexercisedoptionsvalue]
      ,[baseandalloptionsvalue]
            ,[currentcompletiondate]
      ,[ultimatecompletiondate]
            ,[ProjectID]
      ,[ProjectAbbreviation]
      ,[ProjectName]
      ,[IncludesMOSA]
      ,[ProjectPlatform]
            ,[EntityID]
      ,[ParentID]
      ,[vendorname]
      ,[transaction_description]
            ,[claimantprogramcode]
      ,[ProductOrServiceCode]
      ,[principalnaicscode]
      ,[multiyearcontract]
      ,ContractingAgencyID
      ,FundingAgencyID
                     FROM Contract.FPDSpartial WHERE PlatformPortfolio in ('Ordnance and Missiles','Missile Defense') AND ContractingCustomer='Defense'")

summary(factor(munition$principalnaicscode[is.na(text_to_number(munition$principalnaicscode))]))
munition<-apply_standard_lookups(munition)
save(munition,file=file.path("data","semi_clean","munition_transaction.rda"))
     