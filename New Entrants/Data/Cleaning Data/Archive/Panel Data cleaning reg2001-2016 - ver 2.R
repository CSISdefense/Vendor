install.packages("openxlsx")
install.packages("httr")
install.packages("jsonlite")
install.packages("plyr")
install.packages("data.table")
install.packages("tidyverse")
library(openxlsx)
library(httr)
library(jsonlite)
library(plyr)
library(data.table)
library(tidyverse)
library(lubridate)

##Marielle working Directory
setwd("K:/2018-01 NPS New Entrants/Data/Data")
getwd()

##Sam working Directory
setwd("K:.....")
getwd()

###ALL departments####
###Run this first:

final <- read_csv("SAM Data merged with FPDS, exp2000-2019.csv")

final_joined = final[!duplicated(final),]

final_joined <- final_joined %>% 
  dplyr::mutate(age_at_start = year(registrationDate) - year(businessStartDate)) %>% 
  dplyr::rename(country = `samAddress countryCode`)

###created a new variable for 2 digit NAICS codes and removed NA fields
NAICS.edit = final_joined %>% 
  dplyr::mutate(NAICS2 = substr(principalnaicscode, 0, 2)) %>%
  filter(!is.na(principalnaicscode)) %>% 
  filter(NAICS2 != "NU")

NAICS.edit$NAICS2[NAICS.edit$NAICS2 %in% c(31,32,33)] = "31-33"
NAICS.edit$NAICS2[NAICS.edit$NAICS2 %in% c(44,45)] = "44-45"
NAICS.edit$NAICS2[NAICS.edit$NAICS2 %in% c(48,49)] = "48-49"
#GS: Simple coding assignment, do the same for this####

#### identified within each DUNS# which NAICs code had the greatest total
#### obligatedamount to determine one NAICS per DUNS

NAICS.edit2 = NAICS.edit %>% 
  select(duns, obligatedamount, NAICS2) %>% 
  group_by(duns, NAICS2) %>% 
  slice(which.max(obligatedamount)) %>% 
  arrange (duns, desc(obligatedamount))

NAICS.edit.unique = NAICS.edit2[!duplicated(NAICS.edit2[,c('duns')]),]

NAICS.unique.column = NAICS.edit.unique %>% 
  select(duns, NAICS2)

psc_names <- read_csv("K:/2018-01 NPS New Entrants/Data/Data/Raw Data/PSC DIIG categories.csv")
agency_codes <- read.xlsx("K:/2018-01 NPS New Entrants/Data/Data/Raw Data/FPDS agency codes.xlsx")

agency_codes_unique = agency_codes[!duplicated(agency_codes[,c('DEPARTMENT_ID','AGENCY_CODE')]),] 


psc_names$productorservicecode <- as.character(psc_names$ProductOrServiceCode)

final_joined$productorservicecode <- as.character(final_joined$productorservicecode)


name.defined <- final_joined %>% 
  left_join(psc_names[ , c("productorservicecode","ServicesCategory")], by = "productorservicecode") %>% 
  left_join(agency_codes_unique, by = c("agencyid"="AGENCY_CODE")) %>% 
  select(duns, productorservicecode, ServicesCategory, DEPARTMENT_ID, agencyid, DEPARTMENT_NAME, AGENCY_NAME, obligatedamount)

agency <- name.defined %>% 
  select(duns, DEPARTMENT_ID, agencyid, DEPARTMENT_NAME, AGENCY_NAME, obligatedamount) %>% 
  filter(!is.na(AGENCY_NAME)) %>%
  group_by(duns, DEPARTMENT_NAME, AGENCY_NAME) %>% 
  dplyr::summarize(max(obligatedamount)) %>% 
  arrange(desc(`max(obligatedamount)`))

agency.unique = agency[!duplicated(agency[,c("duns")]),]


PSC_code = name.defined %>% 
  select(duns, obligatedamount, productorservicecode, ServicesCategory) %>% 
  filter(!is.na(productorservicecode)) %>%
  filter(productorservicecode != 0, productorservicecode != "N: NO - SERVICE WHERE PBA IS NOT USED.") %>% 
  group_by(duns, ServicesCategory) %>% 
  dplyr::summarize(max(obligatedamount)) %>% 
  arrange(desc(`max(obligatedamount)`))

psc.code.unique = PSC_code[!duplicated(PSC_code[,c("duns")]),]


###devised the fields total contract actions (all contracts that a certain firm had between 
##the given timeframe) and total obligated amount (sum of obligated amount for each contract)

unique.contract <- final_joined[!duplicated(final_joined[,c("unique_transaction_id")]),]

contobsac <- unique.contract %>% 
  select(duns, obligatedamount, unique_transaction_id) %>% 
  group_by(duns) %>% 
  dplyr::summarize(obligated.amt = sum(obligatedamount), contract.actions = n())

#### joined these to the previous dataframe


PSC = as.data.frame(psc.code.unique)
oblandact = as.data.frame(contobsac)
agency = as.data.frame(agency.unique)
NAICS = as.data.frame(NAICS.unique.column)
###########filtering dataset####

###NOP, limit by 10+1 years ####


create_year_edit<-function(data,
                           registration_year){
  year.edit <- data %>% 
    filter(year(registrationDate) == registration_year) %>% 
    group_by(duns) %>% 
    filter(year(signeddate) <= year(registrationDate) + 10) %>% 
    dplyr::mutate(lastsigneddate = max(signeddate)) %>% 
    dplyr::mutate(months.survived = ((year(lastsigneddate) - year(registrationDate)) * 12) 
                  + month(lastsigneddate) - month(registrationDate))
  year.edit.unique <- year.edit[!duplicated(year.edit[,c('duns')]),]
  year <- as.data.frame(year.edit.unique)
  joined.2 <- merge.data.frame(NAICS, year, by = "duns")
  joined.25 <- joined.2 %>%
    select(duns, NAICS2, age_at_start, months.survived, lastsigneddate, status, registrationDate, 
           businessStartDate, country, womenownedflag, veteranownedflag, aiobflag, naobflag,
           minorityownedbusinessflag, apaobflag, baobflag, baobflag, saaobflag, haobflag, 
           isnativehawaiianownedorganizationorfirm, isotherminorityowned, istriballyownedfirm, 
           isalaskannativeownedcorporationorfirm, other_minority_owned_business, 
           isforeignownedandlocated, expirationDate, contractingofficerbusinesssizedetermination)
  joined.3 <- merge.data.frame(joined.25, PSC, by = "duns", all.x = TRUE)
  joined.4 <- merge.data.frame(joined.3, oblandact, by = "duns", all.x = TRUE)
  joined.5 <- merge.data.frame(joined.4, agency, by = "duns", all.x = TRUE)
  dataset.year <- joined.5 %>% 
    dplyr::mutate(firm.age = year(registrationDate) - year(businessStartDate)) %>% 
    dplyr::mutate(location = ifelse(country == "USA", "1", "0")) %>% 
    dplyr::mutate(ownership.woman = ifelse(womenownedflag == 1, "1", 0)) %>% 
    dplyr::mutate(ownership.veteran = ifelse(veteranownedflag == 1, "1",0)) %>% 
    dplyr::mutate(ownership.minority = ifelse(aiobflag == 1 |minorityownedbusinessflag == 1 | 
                                                apaobflag == 1 | 
                                                baobflag == 1 | naobflag == 1 | saaobflag == 1 | 
                                                haobflag == 1 | isnativehawaiianownedorganizationorfirm == 1 | 
                                                isotherminorityowned == 1 | istriballyownedfirm == 1 | 
                                                isalaskannativeownedcorporationorfirm == 1 | 
                                                other_minority_owned_business == 1, "1", 0)) %>% 
    dplyr::mutate(ownership.foreign = ifelse(isforeignownedandlocated == 1, "1", 0)) %>% 
    dplyr::mutate(years.survived = months.survived/12) %>% 
    dplyr::mutate(survival.status = ifelse(year(lastsigneddate) >= 2010, "1", "0")) %>%
    dplyr::mutate(three.year = years.survived>=3, "YES","NO") %>% 
    dplyr::mutate(five.year = years.survived>=5, "YES","NO") %>% 
    dplyr::mutate(ten.year = years.survived>=10, "YES","NO") %>%   ##only works since start at same time
    dplyr::mutate(biz_size = ifelse(contractingofficerbusinesssizedetermination == "O", 1, 0)) %>% 
    select(duns, biz_size, NAICS2, ServicesCategory, location, ownership.woman, 
           ownership.veteran, ownership.minority, ownership.foreign, contract.actions,
           obligated.amt, years.survived, lastsigneddate, firm.age, three.year, five.year,ten.year, lastsigneddate, survival.status, DEPARTMENT_NAME, AGENCY_NAME, registrationDate) %>% 
    filter(NAICS2 != "NU")
  dataset.year
  }
#2001 ####

dataset.2001 <- create_year_edit(final_joined,2001)

#2002 ####
dataset.2002 <- create_year_edit(final_joined,2002)

#2003 ####
dataset.2003 <- create_year_edit(final_joined,2003)

#2004 ####
dataset.2004 <- create_year_edit(final_joined,2004)

#2005 ####
dataset.2005 <- create_year_edit(final_joined,2005)

#2006 ####
dataset.2006 <- create_year_edit(final_joined,2006)

#2007 ####
dataset.2007 <- create_year_edit(final_joined,2007)

#2008 ####
dataset.2008 <- create_year_edit(final_joined,2008)

#2009 ####
dataset.2009 <- create_year_edit(final_joined,2009)

#2010 ####
dataset.2010 <- create_year_edit(final_joined,2010)

#2011 ####
dataset.2011 <- create_year_edit(final_joined,2011)

#2012 ####
dataset.2012 <- create_year_edit(final_joined,2012)

#2013 ####
dataset.2013 <- create_year_edit(final_joined,2013)

#2014 ####
dataset.2014 <- create_year_edit(final_joined,2014)

#2015 ####
dataset.2015 <- create_year_edit(final_joined,2015)

#2016 ####
dataset.2016 <- create_year_edit(final_joined,2016)

#### bind each created dataset together####

all.dataset.SD <- rbind(dataset.2001, dataset.2002, dataset.2003, 
                        dataset.2004, dataset.2005, dataset.2006, 
                        dataset.2007, dataset.2008, dataset.2009, 
                        dataset.2010, dataset.2011, dataset.2012, 
                        dataset.2013, dataset.2014, dataset.2015,
                        dataset.2016)



write.csv(all.dataset.SD, "Panel Data reg2001-2016 - ver4.csv")

------------------------------------------------------------
  
###DOD Panel Data ####

final_joined_DOD <- final_joined %>% 
  left_join(agency_codes_unique, by = c("agencyid"="AGENCY_CODE")) %>% 
  filter(DEPARTMENT_ID == "9700")


###########filtering dataset

###created a new variable for 2 digit NAICS codes and removed NA fields
NAICS.edit = final_joined_DOD %>% 
  dplyr::mutate(NAICS2 = substr(principalnaicscode, 0, 2)) %>%
  filter(!is.na(principalnaicscode)) %>% 
  filter(NAICS2 != "NU")

NAICS.edit$NAICS2[NAICS.edit$NAICS2 %in% c(31,32,33)] = "31-33"
NAICS.edit$NAICS2[NAICS.edit$NAICS2 %in% c(44,45)] = "44-45"
NAICS.edit$NAICS2[NAICS.edit$NAICS2 %in% c(48,49)] = "48-49"

#### identified within each DUNS# which NAICs code had the greatest total
#### obligatedamount to determine one NAICS per DUNS

NAICS.edit2 = NAICS.edit %>% 
  select(duns, obligatedamount, NAICS2) %>% 
  group_by(duns, NAICS2) %>% 
  slice(which.max(obligatedamount)) %>% 
  arrange (duns, desc(obligatedamount))

NAICS.edit.unique = NAICS.edit2[!duplicated(NAICS.edit2[,c('duns')]),]

NAICS.unique.column = NAICS.edit.unique %>% 
  select(duns, NAICS2)


##joined the above sections together by duns and selected only the columns needed

NAICS = as.data.frame(NAICS.unique.column)

###cross reference agency and department codes with a list of names and used one of our 
##datasets in SQL to identify the PSC codes -- used a similar method to determine agency and PSC
##for each firm as was used to determine the NAICS code

name.defined <- final_joined_DOD %>% 
  left_join(psc_names[ , c("productorservicecode","ServicesCategory")], by = "productorservicecode") %>% 
  select(duns, productorservicecode, ServicesCategory, DEPARTMENT_ID, agencyid, DEPARTMENT_NAME, AGENCY_NAME, obligatedamount)

agency <- name.defined %>% 
  select(duns, DEPARTMENT_ID, agencyid, DEPARTMENT_NAME, AGENCY_NAME, obligatedamount) %>% 
  filter(!is.na(AGENCY_NAME)) %>%
  group_by(duns, DEPARTMENT_NAME, AGENCY_NAME) %>% 
  dplyr::summarize(max(obligatedamount)) %>% 
  arrange(desc(`max(obligatedamount)`))

agency.unique = agency[!duplicated(agency[,c("duns")]),]


PSC_code = name.defined %>% 
  select(duns, obligatedamount, productorservicecode, ServicesCategory) %>% 
  filter(!is.na(productorservicecode)) %>%
  filter(productorservicecode != 0, productorservicecode != "N: NO - SERVICE WHERE PBA IS NOT USED.") %>% 
  group_by(duns, ServicesCategory) %>% 
  dplyr::summarize(max(obligatedamount)) %>% 
  arrange(desc(`max(obligatedamount)`))

psc.code.unique = PSC_code[!duplicated(PSC_code[,c("duns")]),]


###devised the fields total contract actions (all contracts that a certain firm had between 
##the given timeframe) and total obligated amount (sum of obligated amount for each contract)

unique.contract <- final_joined_DOD[!duplicated(final_joined_DOD[,c("unique_transaction_id")]),]

contobsac <- unique.contract %>% 
  select(duns, obligatedamount, unique_transaction_id) %>% 
  group_by(duns) %>% 
  dplyr::summarize(obligated.amt = sum(obligatedamount), contract.actions = n())

#### joined these to the previous dataframe


PSC = as.data.frame(psc.code.unique)
oblandact = as.data.frame(contobsac)
agency = as.data.frame(agency.unique)

create_year_edit_DOD<-function(data,
                           registration_year){
  year.edit <- data %>% 
    filter(year(registrationDate) == registration_year) %>% 
    group_by(duns) %>% 
    filter(year(signeddate) <= year(registrationDate) + 10) %>% 
    dplyr::mutate(lastsigneddate = max(signeddate)) %>% 
    dplyr::mutate(months.survived = ((year(lastsigneddate) - year(registrationDate)) * 12) 
                  + month(lastsigneddate) - month(registrationDate))
  year.edit.unique <- year.edit[!duplicated(year.edit[,c('duns')]),]
  year <- as.data.frame(year.edit.unique)
  joined.2 <- merge.data.frame(NAICS, year, by = "duns")
  joined.25 <- joined.2 %>%
    select(duns, NAICS2, age_at_start, months.survived, lastsigneddate, status, registrationDate, 
           businessStartDate, country, womenownedflag, veteranownedflag, aiobflag, naobflag,
           minorityownedbusinessflag, apaobflag, baobflag, baobflag, saaobflag, haobflag, 
           isnativehawaiianownedorganizationorfirm, isotherminorityowned, istriballyownedfirm, 
           isalaskannativeownedcorporationorfirm, other_minority_owned_business, 
           isforeignownedandlocated, expirationDate, contractingofficerbusinesssizedetermination)
  joined.3 <- merge.data.frame(joined.25, PSC, by = "duns", all.x = TRUE)
  joined.4 <- merge.data.frame(joined.3, oblandact, by = "duns", all.x = TRUE)
  joined.5 <- merge.data.frame(joined.4, agency, by = "duns", all.x = TRUE)
  dataset.year <- joined.5 %>% 
    dplyr::mutate(firm.age = year(registrationDate) - year(businessStartDate)) %>% 
    dplyr::mutate(location = ifelse(country == "USA", "1", "0")) %>% 
    dplyr::mutate(ownership.woman = ifelse(womenownedflag == 1, "1", 0)) %>% 
    dplyr::mutate(ownership.veteran = ifelse(veteranownedflag == 1, "1",0)) %>% 
    dplyr::mutate(ownership.minority = ifelse(aiobflag == 1 |minorityownedbusinessflag == 1 | 
                                                apaobflag == 1 | 
                                                baobflag == 1 | naobflag == 1 | saaobflag == 1 | 
                                                haobflag == 1 | isnativehawaiianownedorganizationorfirm == 1 | 
                                                isotherminorityowned == 1 | istriballyownedfirm == 1 | 
                                                isalaskannativeownedcorporationorfirm == 1 | 
                                                other_minority_owned_business == 1, "1", 0)) %>% 
    dplyr::mutate(ownership.foreign = ifelse(isforeignownedandlocated == 1, "1", 0)) %>% 
    dplyr::mutate(years.survived = months.survived/12) %>% 
    dplyr::mutate(survival.status = ifelse(year(lastsigneddate) >= 2010, "1", "0")) %>%
    dplyr::mutate(three.year = years.survived>=3, "YES","NO") %>% 
    dplyr::mutate(five.year = years.survived>=5, "YES","NO") %>% 
    dplyr::mutate(ten.year = years.survived>=10, "YES","NO") %>%   ##only works since start at same time
    dplyr::mutate(biz_size = ifelse(contractingofficerbusinesssizedetermination == "O", 1, 0)) %>% 
    select(duns, biz_size, NAICS2, ServicesCategory, location, ownership.woman, 
           ownership.veteran, ownership.minority, ownership.foreign, contract.actions,
           obligated.amt, years.survived, lastsigneddate, firm.age, three.year, five.year,ten.year, lastsigneddate, survival.status, DEPARTMENT_NAME, AGENCY_NAME, registrationDate) %>% 
    filter(NAICS2 != "NU")
  dataset.year
}

#2001 ####

dataset.2001_DOD <- create_year_edit_DOD(final_joined_DOD,2001)

#2002 ####
dataset.2002_DOD <- create_year_edit_DOD(final_joined_DOD,2002)

#2003 ####
dataset.2003_DOD <- create_year_edit_DOD(final_joined_DOD,2003)

#2004 ####
dataset.2004_DOD <- create_year_edit_DOD(final_joined_DOD,2004)

#2005 ####
dataset.2005_DOD <- create_year_edit_DOD(final_joined_DOD,2005)

#2006 ####
dataset.2006_DOD <- create_year_edit_DOD(final_joined_DOD,2006)

#2007 ####
dataset.2007_DOD <- create_year_edit_DOD(final_joined_DOD,2007)

#2008 ####
dataset.2008_DOD <- create_year_edit_DOD(final_joined_DOD,2008)

#2009 ####
dataset.2009_DOD <- create_year_edit_DOD(final_joined_DOD,2009)

#2010 ####
dataset.2010_DOD <- create_year_edit_DOD(final_joined_DOD,2010)

#2011 ####
dataset.2011_DOD <- create_year_edit_DOD(final_joined_DOD,2011)

#2012 ####
dataset.2012_DOD <- create_year_edit_DOD(final_joined_DOD,2012)

#2013 ####
dataset.2013_DOD <- create_year_edit_DOD(final_joined_DOD,2013)

#2014 ####
dataset.2014_DOD <- create_year_edit_DOD(final_joined_DOD,2014)

#2015 ####
dataset.2015_DOD <- create_year_edit_DOD(final_joined_DOD,2015)

#2016 ####
dataset.2016_DOD <- create_year_edit_DOD(final_joined_DOD,2016)


#### bind each created dataset together####

all.dataset_DOD.SD <- rbind(dataset.2001, dataset.2002, dataset.2003, 
                        dataset.2004, dataset.2005, dataset.2006, 
                        dataset.2007, dataset.2008, dataset.2009, 
                        dataset.2010, dataset.2011, dataset.2012, 
                        dataset.2013, dataset.2014, dataset.2015,
                        dataset.2016)



write.csv(all.dataset_DOD.SD, "Panel Data reg2001-2016 DOD - ver4.csv")
