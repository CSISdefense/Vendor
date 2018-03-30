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
  mutate(age_at_start = year(registrationDate) - year(businessStartDate)) %>% 
  rename(country = `samAddress countryCode`)

###created a new variable for 2 digit NAICS codes and removed NA fields
NAICS.edit = final_joined %>% 
  mutate(NAICS2 = substr(principalnaicscode, 0, 2)) %>%
  filter(!is.na(principalnaicscode)) %>% 
  filter(NAICS2 != "NU")

NAICS.edit$NAICS2[NAICS.edit$NAICS2 == 31] = "31-33"
NAICS.edit$NAICS2[NAICS.edit$NAICS2 == 32] = "31-33"
NAICS.edit$NAICS2[NAICS.edit$NAICS2 == 33] = "31-33"
NAICS.edit$NAICS2[NAICS.edit$NAICS2 == 44] = "44-45"
NAICS.edit$NAICS2[NAICS.edit$NAICS2 == 45] = "44-45"
NAICS.edit$NAICS2[NAICS.edit$NAICS2 == 48] = "48-49"
NAICS.edit$NAICS2[NAICS.edit$NAICS2 == 49] = "48-49"  

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

psc_names <- read_csv("PSC DIIG categories.csv")
agency_codes <- read.xlsx("FPDS agency codes.xlsx")

agency_codes_unique = agency_codes[!duplicated(agency_codes[,c('DEPARTMENT_ID','AGENCY_CODE')]),] 


psc_names$productorservicecode <- as.character(psc_names$ProductOrServiceCode)

final_joined$productorservicecode <- as.character(final_joined$productorservicecode)


name.defined <- final_joined %>% 
  left_join(psc_names[ , c("productorservicecode","ServicesCategory")], by = "productorservicecode") %>% 
  left_join(agency_codes_unique, by = c("mod_agency"="AGENCY_CODE")) %>% 
  select(duns, productorservicecode, ServicesCategory, DEPARTMENT_ID, mod_agency, DEPARTMENT_NAME, AGENCY_NAME, obligatedamount)

agency <- name.defined %>% 
  select(duns, DEPARTMENT_ID, mod_agency, DEPARTMENT_NAME, AGENCY_NAME, obligatedamount) %>% 
  filter(!is.na(AGENCY_NAME)) %>%
  group_by(duns, DEPARTMENT_NAME, AGENCY_NAME) %>% 
  summarize(max(obligatedamount)) %>% 
  arrange(desc(`max(obligatedamount)`))

agency.unique = agency[!duplicated(agency[,c("duns")]),]


PSC_code = name.defined %>% 
  select(duns, obligatedamount, productorservicecode, ServicesCategory) %>% 
  filter(!is.na(productorservicecode)) %>%
  filter(productorservicecode != 0, productorservicecode != "N: NO - SERVICE WHERE PBA IS NOT USED.") %>% 
  group_by(duns, ServicesCategory) %>% 
  summarize(max(obligatedamount)) %>% 
  arrange(desc(`max(obligatedamount)`))

psc.code.unique = PSC_code[!duplicated(PSC_code[,c("duns")]),]


###devised the fields total contract actions (all contracts that a certain firm had between 
##the given timeframe) and total obligated amount (sum of obligated amount for each contract)

unique.contract <- final_joined[!duplicated(final_joined[,c("unique_transaction_id")]),]

contobsac <- unique.contract %>% 
  select(duns, obligatedamount, unique_transaction_id) %>% 
  group_by(duns) %>% 
  summarize(obligated.amt = sum(obligatedamount), contract.actions = n())

#### joined these to the previous dataframe


PSC = as.data.frame(psc.code.unique)
oblandact = as.data.frame(contobsac)
agency = as.data.frame(agency.unique)
###########filtering dataset####

###NOP, limit by 10+1 years ####

#2001 ####
year.edit = final_joined %>% 
  filter(year(registrationDate) == 2001) %>% 
  group_by(duns) %>% 
  filter(year(signeddate) <= year(registrationDate) + 10) %>% 
  mutate(lastsigneddate = max(signeddate)) %>% 
  mutate(months.survived = ((year(lastsigneddate) - year(registrationDate)) * 12) 
         + month(lastsigneddate) - month(registrationDate))
           
#filter(contractingofficerbusinesssizedetermination == "S")
year.edit.unique = year.edit[!duplicated(year.edit[,c('duns')]),]
 
##joined the above sections together by duns and selected only the columns needed

NAICS = as.data.frame(NAICS.unique.column)
year = as.data.frame(year.edit.unique)

joined.2 = merge.data.frame(NAICS, year, by = "duns")  

joined.25 = joined.2 %>%
  select(duns, NAICS2, age_at_start, months.survived, lastsigneddate, status, registrationDate, 
         businessStartDate, country, womenownedflag, veteranownedflag, aiobflag, naobflag,
         minorityownedbusinessflag, apaobflag, baobflag, baobflag, saaobflag, haobflag, 
         isnativehawaiianownedorganizationorfirm, isotherminorityowned, istriballyownedfirm, 
         isalaskannativeownedcorporationorfirm, other_minority_owned_business, 
         isforeignownedandlocated, expirationDate, contractingofficerbusinesssizedetermination)

###cross reference agency and department codes with a list of names and used one of our 
##datasets in SQL to identify the PSC codes -- used a similar method to determine agency and PSC
##for each firm as was used to determine the NAICS code

joined.3 = merge.data.frame(joined.25, PSC, by = "duns", all.x = TRUE)
joined.4 = merge.data.frame(joined.3, oblandact, by = "duns", all.x = TRUE)
joined.5 = merge.data.frame(joined.4, agency, by = "duns", all.x = TRUE)


####selecting and creating needed fields  

#### variables I need: Firm Age, Location, Ownership, years in SAM, survival status, biz_size, 3 years, 5 years, 10 years 


dataset.2001 = joined.5 %>% 
  mutate(firm.age = year(registrationDate) - year(businessStartDate)) %>% 
  mutate(location = ifelse(country == "USA", "1", "0")) %>% 
  mutate(ownership.woman = ifelse(womenownedflag == 1, "1", 0)) %>% 
  mutate(ownership.veteran = ifelse(veteranownedflag == 1, "1",0)) %>% 
  mutate(ownership.minority = ifelse(aiobflag == 1 |minorityownedbusinessflag == 1 | 
                                       apaobflag == 1 | 
                                       baobflag == 1 | naobflag == 1 | saaobflag == 1 | 
                                       haobflag == 1 | isnativehawaiianownedorganizationorfirm == 1 | 
                                       isotherminorityowned == 1 | istriballyownedfirm == 1 | 
                                       isalaskannativeownedcorporationorfirm == 1 | 
                                       other_minority_owned_business == 1, "1", 0)) %>% 
  mutate(ownership.foreign = ifelse(isforeignownedandlocated == 1, "1", 0)) %>% 
  mutate(years.survived = months.survived/12) %>% 
  mutate(survival.status = ifelse(year(lastsigneddate) >= 2010, "1", "0")) %>%
  mutate(three.year = years.survived>=3, "YES","NO") %>% 
  mutate(five.year = years.survived>=5, "YES","NO") %>% 
  mutate(ten.year = years.survived>=10, "YES","NO") %>%   ##only works since start at same time
  mutate(biz_size = ifelse(contractingofficerbusinesssizedetermination == "O", 1, 0)) %>% 
  select(duns, biz_size, NAICS2, ServicesCategory, location, ownership.woman, 
         ownership.veteran, ownership.minority, ownership.foreign, contract.actions,
         obligated.amt, years.survived, lastsigneddate, firm.age, three.year, five.year,ten.year, lastsigneddate, survival.status, DEPARTMENT_NAME, AGENCY_NAME, registrationDate) %>% 
  filter(NAICS2 != "NU")

#2002 ####
year.edit = final_joined %>% 
  filter(year(registrationDate) == 2002) %>% 
  group_by(duns) %>% 
  filter(year(signeddate) <= year(registrationDate) + 10) %>% 
  mutate(lastsigneddate = max(signeddate)) %>% 
  mutate(months.survived = ((year(lastsigneddate) - year(registrationDate)) * 12) 
         + month(lastsigneddate) - month(registrationDate))
  
  #filter(contractingofficerbusinesssizedetermination == "S")
  year.edit.unique = year.edit[!duplicated(year.edit[,c('duns')]),]


##joined the above sections together by duns and selected only the columns needed

NAICS = as.data.frame(NAICS.unique.column)
year = as.data.frame(year.edit.unique)

joined.2 = merge.data.frame(NAICS, year, by = "duns")  

joined.25 = joined.2 %>%
  select(duns, NAICS2, age_at_start, months.survived, lastsigneddate, status, registrationDate, 
         businessStartDate, country, womenownedflag, veteranownedflag, aiobflag, naobflag,
         minorityownedbusinessflag, apaobflag, baobflag, baobflag, saaobflag, haobflag, 
         isnativehawaiianownedorganizationorfirm, isotherminorityowned, istriballyownedfirm, 
         isalaskannativeownedcorporationorfirm, other_minority_owned_business, 
         isforeignownedandlocated, expirationDate, contractingofficerbusinesssizedetermination)

###cross reference agency and department codes with a list of names and used one of our 
##datasets in SQL to identify the PSC codes -- used a similar method to determine agency and PSC
##for each firm as was used to determine the NAICS code

joined.3 = merge.data.frame(joined.25, PSC, by = "duns", all.x = TRUE)
joined.4 = merge.data.frame(joined.3, oblandact, by = "duns", all.x = TRUE)
joined.5 = merge.data.frame(joined.4, agency, by = "duns", all.x = TRUE)


####selecting and creating needed fields  

#### variables I need: Firm Age, Location, Ownership, years in SAM, survival status, biz_size, 3 years, 5 years, 10 years 


dataset.2002 = joined.5 %>% 
  mutate(firm.age = year(registrationDate) - year(businessStartDate)) %>% 
  mutate(location = ifelse(country == "USA", "1", "0")) %>% 
  mutate(ownership.woman = ifelse(womenownedflag == 1, "1", 0)) %>% 
  mutate(ownership.veteran = ifelse(veteranownedflag == 1, "1",0)) %>% 
  mutate(ownership.minority = ifelse(aiobflag == 1 |minorityownedbusinessflag == 1 | 
                                       apaobflag == 1 | 
                                       baobflag == 1 | naobflag == 1 | saaobflag == 1 | 
                                       haobflag == 1 | isnativehawaiianownedorganizationorfirm == 1 | 
                                       isotherminorityowned == 1 | istriballyownedfirm == 1 | 
                                       isalaskannativeownedcorporationorfirm == 1 | 
                                       other_minority_owned_business == 1, "1", 0)) %>% 
  mutate(ownership.foreign = ifelse(isforeignownedandlocated == 1, "1", 0)) %>% 
  mutate(years.survived = months.survived/12) %>% 
  mutate(survival.status = ifelse(year(lastsigneddate) >= 2010, "1", "0")) %>%
  mutate(three.year = years.survived>=3, "YES","NO") %>% 
  mutate(five.year = years.survived>=5, "YES","NO") %>% 
  mutate(ten.year = years.survived>=10, "YES","NO") %>%   ##only works since start at same time
  mutate(biz_size = ifelse(contractingofficerbusinesssizedetermination == "O", 1, 0)) %>% 
  select(duns, biz_size, NAICS2, ServicesCategory, location, ownership.woman, 
         ownership.veteran, ownership.minority, ownership.foreign, contract.actions,
         obligated.amt, years.survived, lastsigneddate, firm.age, three.year, five.year,ten.year, lastsigneddate, survival.status, DEPARTMENT_NAME, AGENCY_NAME, registrationDate) %>% 
  filter(NAICS2 != "NU")

#2003 ####
year.edit = final_joined %>% 
  filter(year(registrationDate) == 2003) %>% 
  group_by(duns) %>% 
  filter(year(signeddate) <= year(registrationDate) + 10) %>% 
  mutate(lastsigneddate = max(signeddate)) %>% 
  mutate(months.survived = ((year(lastsigneddate) - year(registrationDate)) * 12) 
         + month(lastsigneddate) - month(registrationDate)) 
  
  #filter(contractingofficerbusinesssizedetermination == "S")
  year.edit.unique = year.edit[!duplicated(year.edit[,c('duns')]),]


##joined the above sections together by duns and selected only the columns needed

NAICS = as.data.frame(NAICS.unique.column)
year = as.data.frame(year.edit.unique)

joined.2 = merge.data.frame(NAICS, year, by = "duns")  

joined.25 = joined.2 %>%
  select(duns, NAICS2, age_at_start, months.survived, lastsigneddate, status, registrationDate, 
         businessStartDate, country, womenownedflag, veteranownedflag, aiobflag, naobflag,
         minorityownedbusinessflag, apaobflag, baobflag, baobflag, saaobflag, haobflag, 
         isnativehawaiianownedorganizationorfirm, isotherminorityowned, istriballyownedfirm, 
         isalaskannativeownedcorporationorfirm, other_minority_owned_business, 
         isforeignownedandlocated, expirationDate, contractingofficerbusinesssizedetermination)

###cross reference agency and department codes with a list of names and used one of our 
##datasets in SQL to identify the PSC codes -- used a similar method to determine agency and PSC
##for each firm as was used to determine the NAICS code

joined.3 = merge.data.frame(joined.25, PSC, by = "duns", all.x = TRUE)
joined.4 = merge.data.frame(joined.3, oblandact, by = "duns", all.x = TRUE)
joined.5 = merge.data.frame(joined.4, agency, by = "duns", all.x = TRUE)


####selecting and creating needed fields  

#### variables I need: Firm Age, Location, Ownership, years in SAM, survival status, biz_size, 3 years, 5 years, 10 years 


dataset.2003 = joined.5 %>% 
  mutate(firm.age = year(registrationDate) - year(businessStartDate)) %>% 
  mutate(location = ifelse(country == "USA", "1", "0")) %>% 
  mutate(ownership.woman = ifelse(womenownedflag == 1, "1", 0)) %>% 
  mutate(ownership.veteran = ifelse(veteranownedflag == 1, "1",0)) %>% 
  mutate(ownership.minority = ifelse(aiobflag == 1 |minorityownedbusinessflag == 1 | 
                                       apaobflag == 1 | 
                                       baobflag == 1 | naobflag == 1 | saaobflag == 1 | 
                                       haobflag == 1 | isnativehawaiianownedorganizationorfirm == 1 | 
                                       isotherminorityowned == 1 | istriballyownedfirm == 1 | 
                                       isalaskannativeownedcorporationorfirm == 1 | 
                                       other_minority_owned_business == 1, "1", 0)) %>% 
  mutate(ownership.foreign = ifelse(isforeignownedandlocated == 1, "1", 0)) %>% 
  mutate(years.survived = months.survived/12) %>% 
  mutate(survival.status = ifelse(year(lastsigneddate) >= 2010, "1", "0")) %>%
  mutate(three.year = years.survived>=3, "YES","NO") %>% 
  mutate(five.year = years.survived>=5, "YES","NO") %>% 
  mutate(ten.year = years.survived>=10, "YES","NO") %>%   ##only works since start at same time
  mutate(biz_size = ifelse(contractingofficerbusinesssizedetermination == "O", 1, 0)) %>% 
  select(duns, biz_size, NAICS2, ServicesCategory, location, ownership.woman, 
         ownership.veteran, ownership.minority, ownership.foreign, contract.actions,
         obligated.amt, years.survived, lastsigneddate, firm.age, three.year, five.year,ten.year, lastsigneddate, survival.status, DEPARTMENT_NAME, AGENCY_NAME, registrationDate) %>% 
  filter(NAICS2 != "NU")

#2004 ####
year.edit = final_joined %>% 
  filter(year(registrationDate) == 2004) %>% 
  group_by(duns) %>% 
  filter(year(signeddate) <= year(registrationDate) + 10) %>% 
  mutate(lastsigneddate = max(signeddate)) %>% 
  mutate(months.survived = ((year(lastsigneddate) - year(registrationDate)) * 12) 
         + month(lastsigneddate) - month(registrationDate)) 
  
  #filter(contractingofficerbusinesssizedetermination == "S")
  year.edit.unique = year.edit[!duplicated(year.edit[,c('duns')]),]


##joined the above sections together by duns and selected only the columns needed

NAICS = as.data.frame(NAICS.unique.column)
year = as.data.frame(year.edit.unique)

joined.2 = merge.data.frame(NAICS, year, by = "duns")  

joined.25 = joined.2 %>%
  select(duns, NAICS2, age_at_start, months.survived, lastsigneddate, status, registrationDate, 
         businessStartDate, country, womenownedflag, veteranownedflag, aiobflag, naobflag,
         minorityownedbusinessflag, apaobflag, baobflag, baobflag, saaobflag, haobflag, 
         isnativehawaiianownedorganizationorfirm, isotherminorityowned, istriballyownedfirm, 
         isalaskannativeownedcorporationorfirm, other_minority_owned_business, 
         isforeignownedandlocated, expirationDate, contractingofficerbusinesssizedetermination)

###cross reference agency and department codes with a list of names and used one of our 
##datasets in SQL to identify the PSC codes -- used a similar method to determine agency and PSC
##for each firm as was used to determine the NAICS code

joined.3 = merge.data.frame(joined.25, PSC, by = "duns", all.x = TRUE)
joined.4 = merge.data.frame(joined.3, oblandact, by = "duns", all.x = TRUE)
joined.5 = merge.data.frame(joined.4, agency, by = "duns", all.x = TRUE)


####selecting and creating needed fields  

#### variables I need: Firm Age, Location, Ownership, years in SAM, survival status, biz_size, 3 years, 5 years, 10 years 


dataset.2004 = joined.5 %>% 
  mutate(firm.age = year(registrationDate) - year(businessStartDate)) %>% 
  mutate(location = ifelse(country == "USA", "1", "0")) %>% 
  mutate(ownership.woman = ifelse(womenownedflag == 1, "1", 0)) %>% 
  mutate(ownership.veteran = ifelse(veteranownedflag == 1, "1",0)) %>% 
  mutate(ownership.minority = ifelse(aiobflag == 1 |minorityownedbusinessflag == 1 | 
                                       apaobflag == 1 | 
                                       baobflag == 1 | naobflag == 1 | saaobflag == 1 | 
                                       haobflag == 1 | isnativehawaiianownedorganizationorfirm == 1 | 
                                       isotherminorityowned == 1 | istriballyownedfirm == 1 | 
                                       isalaskannativeownedcorporationorfirm == 1 | 
                                       other_minority_owned_business == 1, "1", 0)) %>% 
  mutate(ownership.foreign = ifelse(isforeignownedandlocated == 1, "1", 0)) %>% 
  mutate(years.survived = months.survived/12) %>% 
  mutate(survival.status = ifelse(year(lastsigneddate) >= 2010, "1", "0")) %>%
  mutate(three.year = years.survived>=3, "YES","NO") %>% 
  mutate(five.year = years.survived>=5, "YES","NO") %>% 
  mutate(ten.year = years.survived>=10, "YES","NO") %>%   ##only works since start at same time
  mutate(biz_size = ifelse(contractingofficerbusinesssizedetermination == "O", 1, 0)) %>% 
  select(duns, biz_size, NAICS2, ServicesCategory, location, ownership.woman, 
         ownership.veteran, ownership.minority, ownership.foreign, contract.actions,
         obligated.amt, years.survived, lastsigneddate, firm.age, three.year, five.year,ten.year, lastsigneddate, survival.status, DEPARTMENT_NAME, AGENCY_NAME, registrationDate) %>% 
  filter(NAICS2 != "NU")

#2005 ####
year.edit = final_joined %>% 
  filter(year(registrationDate) == 2005) %>% 
  group_by(duns) %>% 
  filter(year(signeddate) <= year(registrationDate) + 10) %>% 
  mutate(lastsigneddate = max(signeddate)) %>% 
  mutate(months.survived = ((year(lastsigneddate) - year(registrationDate)) * 12)
         + month(lastsigneddate) - month(registrationDate))
  
  #filter(contractingofficerbusinesssizedetermination == "S")
  year.edit.unique = year.edit[!duplicated(year.edit[,c('duns')]),]


##joined the above sections together by duns and selected only the columns needed

NAICS = as.data.frame(NAICS.unique.column)
year = as.data.frame(year.edit.unique)

joined.2 = merge.data.frame(NAICS, year, by = "duns")  

joined.25 = joined.2 %>%
  select(duns, NAICS2, age_at_start, months.survived, lastsigneddate, status, registrationDate, 
         businessStartDate, country, womenownedflag, veteranownedflag, aiobflag, naobflag,
         minorityownedbusinessflag, apaobflag, baobflag, baobflag, saaobflag, haobflag, 
         isnativehawaiianownedorganizationorfirm, isotherminorityowned, istriballyownedfirm, 
         isalaskannativeownedcorporationorfirm, other_minority_owned_business, 
         isforeignownedandlocated, expirationDate, contractingofficerbusinesssizedetermination)

###cross reference agency and department codes with a list of names and used one of our 
##datasets in SQL to identify the PSC codes -- used a similar method to determine agency and PSC
##for each firm as was used to determine the NAICS code

joined.3 = merge.data.frame(joined.25, PSC, by = "duns", all.x = TRUE)
joined.4 = merge.data.frame(joined.3, oblandact, by = "duns", all.x = TRUE)
joined.5 = merge.data.frame(joined.4, agency, by = "duns", all.x = TRUE)


####selecting and creating needed fields  

#### variables I need: Firm Age, Location, Ownership, years in SAM, survival status, biz_size, 3 years, 5 years, 10 years 


dataset.2005 = joined.5 %>% 
  mutate(firm.age = year(registrationDate) - year(businessStartDate)) %>% 
  mutate(location = ifelse(country == "USA", "1", "0")) %>% 
  mutate(ownership.woman = ifelse(womenownedflag == 1, "1", 0)) %>% 
  mutate(ownership.veteran = ifelse(veteranownedflag == 1, "1",0)) %>% 
  mutate(ownership.minority = ifelse(aiobflag == 1 |minorityownedbusinessflag == 1 | 
                                       apaobflag == 1 | 
                                       baobflag == 1 | naobflag == 1 | saaobflag == 1 | 
                                       haobflag == 1 | isnativehawaiianownedorganizationorfirm == 1 | 
                                       isotherminorityowned == 1 | istriballyownedfirm == 1 | 
                                       isalaskannativeownedcorporationorfirm == 1 | 
                                       other_minority_owned_business == 1, "1", 0)) %>% 
  mutate(ownership.foreign = ifelse(isforeignownedandlocated == 1, "1", 0)) %>% 
  mutate(years.survived = months.survived/12) %>% 
  mutate(survival.status = ifelse(year(lastsigneddate) >= 2010, "1", "0")) %>%
  mutate(three.year = years.survived>=3, "YES","NO") %>% 
  mutate(five.year = years.survived>=5, "YES","NO") %>% 
  mutate(ten.year = years.survived>=10, "YES","NO") %>%   ##only works since start at same time
  mutate(biz_size = ifelse(contractingofficerbusinesssizedetermination == "O", 1, 0)) %>% 
  select(duns, biz_size, NAICS2, ServicesCategory, location, ownership.woman, 
         ownership.veteran, ownership.minority, ownership.foreign, contract.actions,
         obligated.amt, years.survived, lastsigneddate, firm.age, three.year, five.year,ten.year, lastsigneddate, survival.status, DEPARTMENT_NAME, AGENCY_NAME, registrationDate) %>% 
  filter(NAICS2 != "NU")

#2006 ####
year.edit = final_joined %>% 
  filter(year(registrationDate) == 2006) %>% 
  group_by(duns) %>% 
  filter(year(signeddate) <= year(registrationDate) + 10) %>% 
  mutate(lastsigneddate = max(signeddate)) %>% 
  mutate(months.survived = ((year(lastsigneddate) - year(registrationDate)) * 12) 
         + month(lastsigneddate) - month(registrationDate)) 
  
  #filter(contractingofficerbusinesssizedetermination == "S")
  year.edit.unique = year.edit[!duplicated(year.edit[,c('duns')]),]


##joined the above sections together by duns and selected only the columns needed

NAICS = as.data.frame(NAICS.unique.column)
year = as.data.frame(year.edit.unique)

joined.2 = merge.data.frame(NAICS, year, by = "duns")  

joined.25 = joined.2 %>%
  select(duns, NAICS2, age_at_start, months.survived, lastsigneddate, status, registrationDate, 
         businessStartDate, country, womenownedflag, veteranownedflag, aiobflag, naobflag,
         minorityownedbusinessflag, apaobflag, baobflag, baobflag, saaobflag, haobflag, 
         isnativehawaiianownedorganizationorfirm, isotherminorityowned, istriballyownedfirm, 
         isalaskannativeownedcorporationorfirm, other_minority_owned_business, 
         isforeignownedandlocated, expirationDate, contractingofficerbusinesssizedetermination)

###cross reference agency and department codes with a list of names and used one of our 
##datasets in SQL to identify the PSC codes -- used a similar method to determine agency and PSC
##for each firm as was used to determine the NAICS code

joined.3 = merge.data.frame(joined.25, PSC, by = "duns", all.x = TRUE)
joined.4 = merge.data.frame(joined.3, oblandact, by = "duns", all.x = TRUE)
joined.5 = merge.data.frame(joined.4, agency, by = "duns", all.x = TRUE)


####selecting and creating needed fields  

#### variables I need: Firm Age, Location, Ownership, years in SAM, survival status, biz_size, 3 years, 5 years, 10 years 


dataset.2006 = joined.5 %>% 
  mutate(firm.age = year(registrationDate) - year(businessStartDate)) %>% 
  mutate(location = ifelse(country == "USA", "1", "0")) %>% 
  mutate(ownership.woman = ifelse(womenownedflag == 1, "1", 0)) %>% 
  mutate(ownership.veteran = ifelse(veteranownedflag == 1, "1",0)) %>% 
  mutate(ownership.minority = ifelse(aiobflag == 1 |minorityownedbusinessflag == 1 | 
                                       apaobflag == 1 | 
                                       baobflag == 1 | naobflag == 1 | saaobflag == 1 | 
                                       haobflag == 1 | isnativehawaiianownedorganizationorfirm == 1 | 
                                       isotherminorityowned == 1 | istriballyownedfirm == 1 | 
                                       isalaskannativeownedcorporationorfirm == 1 | 
                                       other_minority_owned_business == 1, "1", 0)) %>% 
  mutate(ownership.foreign = ifelse(isforeignownedandlocated == 1, "1", 0)) %>% 
  mutate(years.survived = months.survived/12) %>% 
  mutate(survival.status = ifelse(year(lastsigneddate) >= 2010, "1", "0")) %>%
  mutate(three.year = years.survived>=3, "YES","NO") %>% 
  mutate(five.year = years.survived>=5, "YES","NO") %>% 
  mutate(ten.year = years.survived>=10, "YES","NO") %>%   ##only works since start at same time
  mutate(biz_size = ifelse(contractingofficerbusinesssizedetermination == "O", 1, 0)) %>% 
  select(duns, biz_size, NAICS2, ServicesCategory, location, ownership.woman, 
         ownership.veteran, ownership.minority, ownership.foreign, contract.actions,
         obligated.amt, years.survived, lastsigneddate, firm.age, three.year, five.year,ten.year, lastsigneddate, survival.status, DEPARTMENT_NAME, AGENCY_NAME, registrationDate) %>% 
  filter(NAICS2 != "NU")

#2007 ####
year.edit = final_joined %>% 
  filter(year(registrationDate) == 2007) %>% 
  group_by(duns) %>% 
  filter(year(signeddate) <= year(registrationDate) + 10) %>% 
  mutate(lastsigneddate = max(signeddate)) %>% 
  mutate(months.survived = ((year(lastsigneddate) - year(registrationDate)) * 12) 
         + month(lastsigneddate) - month(registrationDate)) 
  
  #filter(contractingofficerbusinesssizedetermination == "S")
  year.edit.unique = year.edit[!duplicated(year.edit[,c('duns')]),]


##joined the above sections together by duns and selected only the columns needed

NAICS = as.data.frame(NAICS.unique.column)
year = as.data.frame(year.edit.unique)

joined.2 = merge.data.frame(NAICS, year, by = "duns")  

joined.25 = joined.2 %>%
  select(duns, NAICS2, age_at_start, months.survived, lastsigneddate, status, registrationDate, 
         businessStartDate, country, womenownedflag, veteranownedflag, aiobflag, naobflag,
         minorityownedbusinessflag, apaobflag, baobflag, baobflag, saaobflag, haobflag, 
         isnativehawaiianownedorganizationorfirm, isotherminorityowned, istriballyownedfirm, 
         isalaskannativeownedcorporationorfirm, other_minority_owned_business, 
         isforeignownedandlocated, expirationDate, contractingofficerbusinesssizedetermination)

###cross reference agency and department codes with a list of names and used one of our 
##datasets in SQL to identify the PSC codes -- used a similar method to determine agency and PSC
##for each firm as was used to determine the NAICS code

joined.3 = merge.data.frame(joined.25, PSC, by = "duns", all.x = TRUE)
joined.4 = merge.data.frame(joined.3, oblandact, by = "duns", all.x = TRUE)
joined.5 = merge.data.frame(joined.4, agency, by = "duns", all.x = TRUE)


####selecting and creating needed fields  

#### variables I need: Firm Age, Location, Ownership, years in SAM, survival status, biz_size, 3 years, 5 years, 10 years 


dataset.2007 = joined.5 %>% 
  mutate(firm.age = year(registrationDate) - year(businessStartDate)) %>% 
  mutate(location = ifelse(country == "USA", "1", "0")) %>% 
  mutate(ownership.woman = ifelse(womenownedflag == 1, "1", 0)) %>% 
  mutate(ownership.veteran = ifelse(veteranownedflag == 1, "1",0)) %>% 
  mutate(ownership.minority = ifelse(aiobflag == 1 |minorityownedbusinessflag == 1 | 
                                       apaobflag == 1 | 
                                       baobflag == 1 | naobflag == 1 | saaobflag == 1 | 
                                       haobflag == 1 | isnativehawaiianownedorganizationorfirm == 1 | 
                                       isotherminorityowned == 1 | istriballyownedfirm == 1 | 
                                       isalaskannativeownedcorporationorfirm == 1 | 
                                       other_minority_owned_business == 1, "1", 0)) %>% 
  mutate(ownership.foreign = ifelse(isforeignownedandlocated == 1, "1", 0)) %>% 
  mutate(years.survived = months.survived/12) %>% 
  mutate(survival.status = ifelse(year(lastsigneddate) >= 2010, "1", "0")) %>%
  mutate(three.year = years.survived>=3, "YES","NO") %>% 
  mutate(five.year = years.survived>=5, "YES","NO") %>% 
  mutate(ten.year = years.survived>=10, "YES","NO") %>%   ##only works since start at same time
  mutate(biz_size = ifelse(contractingofficerbusinesssizedetermination == "O", 1, 0)) %>% 
  select(duns, biz_size, NAICS2, ServicesCategory, location, ownership.woman, 
         ownership.veteran, ownership.minority, ownership.foreign, contract.actions,
         obligated.amt, years.survived, lastsigneddate, firm.age, three.year, five.year,ten.year, lastsigneddate, survival.status, DEPARTMENT_NAME, AGENCY_NAME, registrationDate) %>% 
  filter(NAICS2 != "NU")

#2008 ####
year.edit = final_joined %>% 
  filter(year(registrationDate) == 2008) %>% 
  group_by(duns) %>% 
  filter(year(signeddate) <= year(registrationDate) + 10) %>% 
  mutate(lastsigneddate = max(signeddate)) %>% 
  mutate(months.survived = ((year(lastsigneddate) - year(registrationDate)) * 12) 
         + month(lastsigneddate) - month(registrationDate))
  
  #filter(contractingofficerbusinesssizedetermination == "S")
  year.edit.unique = year.edit[!duplicated(year.edit[,c('duns')]),]


##joined the above sections together by duns and selected only the columns needed

NAICS = as.data.frame(NAICS.unique.column)
year = as.data.frame(year.edit.unique)

joined.2 = merge.data.frame(NAICS, year, by = "duns")  

joined.25 = joined.2 %>%
  select(duns, NAICS2, age_at_start, months.survived, lastsigneddate, status, registrationDate, 
         businessStartDate, country, womenownedflag, veteranownedflag, aiobflag, naobflag,
         minorityownedbusinessflag, apaobflag, baobflag, baobflag, saaobflag, haobflag, 
         isnativehawaiianownedorganizationorfirm, isotherminorityowned, istriballyownedfirm, 
         isalaskannativeownedcorporationorfirm, other_minority_owned_business, 
         isforeignownedandlocated, expirationDate, contractingofficerbusinesssizedetermination)

###cross reference agency and department codes with a list of names and used one of our 
##datasets in SQL to identify the PSC codes -- used a similar method to determine agency and PSC
##for each firm as was used to determine the NAICS code

joined.3 = merge.data.frame(joined.25, PSC, by = "duns", all.x = TRUE)
joined.4 = merge.data.frame(joined.3, oblandact, by = "duns", all.x = TRUE)
joined.5 = merge.data.frame(joined.4, agency, by = "duns", all.x = TRUE)


####selecting and creating needed fields  

#### variables I need: Firm Age, Location, Ownership, years in SAM, survival status, biz_size, 3 years, 5 years, 10 years 


dataset.2008 = joined.5 %>% 
  mutate(firm.age = year(registrationDate) - year(businessStartDate)) %>% 
  mutate(location = ifelse(country == "USA", "1", "0")) %>% 
  mutate(ownership.woman = ifelse(womenownedflag == 1, "1", 0)) %>% 
  mutate(ownership.veteran = ifelse(veteranownedflag == 1, "1",0)) %>% 
  mutate(ownership.minority = ifelse(aiobflag == 1 |minorityownedbusinessflag == 1 | 
                                       apaobflag == 1 | 
                                       baobflag == 1 | naobflag == 1 | saaobflag == 1 | 
                                       haobflag == 1 | isnativehawaiianownedorganizationorfirm == 1 | 
                                       isotherminorityowned == 1 | istriballyownedfirm == 1 | 
                                       isalaskannativeownedcorporationorfirm == 1 | 
                                       other_minority_owned_business == 1, "1", 0)) %>% 
  mutate(ownership.foreign = ifelse(isforeignownedandlocated == 1, "1", 0)) %>% 
  mutate(years.survived = months.survived/12) %>% 
  mutate(survival.status = ifelse(year(lastsigneddate) >= 2010, "1", "0")) %>%
  mutate(three.year = years.survived>=3, "YES","NO") %>% 
  mutate(five.year = years.survived>=5, "YES","NO") %>% 
  mutate(ten.year = years.survived>=10, "YES","NO") %>%   ##only works since start at same time
  mutate(biz_size = ifelse(contractingofficerbusinesssizedetermination == "O", 1, 0)) %>% 
  select(duns, biz_size, NAICS2, ServicesCategory, location, ownership.woman, 
         ownership.veteran, ownership.minority, ownership.foreign, contract.actions,
         obligated.amt, years.survived, lastsigneddate, firm.age, three.year, five.year,ten.year, lastsigneddate, survival.status, DEPARTMENT_NAME, AGENCY_NAME, registrationDate) %>% 
  filter(NAICS2 != "NU")

#2009 ####
year.edit = final_joined %>% 
  filter(year(registrationDate) == 2009) %>% 
  group_by(duns) %>% 
  filter(year(signeddate) <= year(registrationDate) + 10) %>% 
  mutate(lastsigneddate = max(signeddate)) %>% 
  mutate(months.survived = ((year(lastsigneddate) - year(registrationDate)) * 12) + month(lastsigneddate) - month(registrationDate))
  
  #filter(contractingofficerbusinesssizedetermination == "S")
  year.edit.unique = year.edit[!duplicated(year.edit[,c('duns')]),]


##joined the above sections together by duns and selected only the columns needed

NAICS = as.data.frame(NAICS.unique.column)
year = as.data.frame(year.edit.unique)

joined.2 = merge.data.frame(NAICS, year, by = "duns")  

joined.25 = joined.2 %>%
  select(duns, NAICS2, age_at_start, months.survived, lastsigneddate, status, registrationDate, 
         businessStartDate, country, womenownedflag, veteranownedflag, aiobflag, naobflag,
         minorityownedbusinessflag, apaobflag, baobflag, baobflag, saaobflag, haobflag, 
         isnativehawaiianownedorganizationorfirm, isotherminorityowned, istriballyownedfirm, 
         isalaskannativeownedcorporationorfirm, other_minority_owned_business, 
         isforeignownedandlocated, expirationDate, contractingofficerbusinesssizedetermination)

###cross reference agency and department codes with a list of names and used one of our 
##datasets in SQL to identify the PSC codes -- used a similar method to determine agency and PSC
##for each firm as was used to determine the NAICS code

joined.3 = merge.data.frame(joined.25, PSC, by = "duns", all.x = TRUE)
joined.4 = merge.data.frame(joined.3, oblandact, by = "duns", all.x = TRUE)
joined.5 = merge.data.frame(joined.4, agency, by = "duns", all.x = TRUE)


####selecting and creating needed fields  

#### variables I need: Firm Age, Location, Ownership, years in SAM, survival status, biz_size, 3 years, 5 years, 10 years 


dataset.2009 = joined.5 %>% 
  mutate(firm.age = year(registrationDate) - year(businessStartDate)) %>% 
  mutate(location = ifelse(country == "USA", "1", "0")) %>% 
  mutate(ownership.woman = ifelse(womenownedflag == 1, "1", 0)) %>% 
  mutate(ownership.veteran = ifelse(veteranownedflag == 1, "1",0)) %>% 
  mutate(ownership.minority = ifelse(aiobflag == 1 |minorityownedbusinessflag == 1 | 
                                       apaobflag == 1 | 
                                       baobflag == 1 | naobflag == 1 | saaobflag == 1 | 
                                       haobflag == 1 | isnativehawaiianownedorganizationorfirm == 1 | 
                                       isotherminorityowned == 1 | istriballyownedfirm == 1 | 
                                       isalaskannativeownedcorporationorfirm == 1 | 
                                       other_minority_owned_business == 1, "1", 0)) %>% 
  mutate(ownership.foreign = ifelse(isforeignownedandlocated == 1, "1", 0)) %>% 
  mutate(years.survived = months.survived/12) %>% 
  mutate(survival.status = ifelse(year(lastsigneddate) >= 2010, "1", "0")) %>%
  mutate(three.year = years.survived>=3, "YES","NO") %>% 
  mutate(five.year = years.survived>=5, "YES","NO") %>% 
  mutate(ten.year = years.survived>=10, "YES","NO") %>%   ##only works since start at same time
  mutate(biz_size = ifelse(contractingofficerbusinesssizedetermination == "O", 1, 0)) %>% 
  select(duns, biz_size, NAICS2, ServicesCategory, location, ownership.woman, 
         ownership.veteran, ownership.minority, ownership.foreign, contract.actions,
         obligated.amt, years.survived, lastsigneddate, firm.age, three.year, five.year,ten.year, lastsigneddate, survival.status, DEPARTMENT_NAME, AGENCY_NAME, registrationDate) %>% 
  filter(NAICS2 != "NU")

#2010 ####
year.edit = final_joined %>% 
  filter(year(registrationDate) == 2010) %>% 
  group_by(duns) %>% 
  filter(year(signeddate) <= year(registrationDate) + 10) %>% 
  mutate(lastsigneddate = max(signeddate)) %>% 
  mutate(months.survived = ((year(lastsigneddate) - year(registrationDate)) * 12) + month(lastsigneddate) - month(registrationDate))
  
  #filter(contractingofficerbusinesssizedetermination == "S")
  year.edit.unique = year.edit[!duplicated(year.edit[,c('duns')]),]


##joined the above sections together by duns and selected only the columns needed

NAICS = as.data.frame(NAICS.unique.column)
year = as.data.frame(year.edit.unique)

joined.2 = merge.data.frame(NAICS, year, by = "duns")  

joined.25 = joined.2 %>%
  select(duns, NAICS2, age_at_start, months.survived, lastsigneddate, status, registrationDate, 
         businessStartDate, country, womenownedflag, veteranownedflag, aiobflag, naobflag,
         minorityownedbusinessflag, apaobflag, baobflag, baobflag, saaobflag, haobflag, 
         isnativehawaiianownedorganizationorfirm, isotherminorityowned, istriballyownedfirm, 
         isalaskannativeownedcorporationorfirm, other_minority_owned_business, 
         isforeignownedandlocated, expirationDate, contractingofficerbusinesssizedetermination)

###cross reference agency and department codes with a list of names and used one of our 
##datasets in SQL to identify the PSC codes -- used a similar method to determine agency and PSC
##for each firm as was used to determine the NAICS code

joined.3 = merge.data.frame(joined.25, PSC, by = "duns", all.x = TRUE)
joined.4 = merge.data.frame(joined.3, oblandact, by = "duns", all.x = TRUE)
joined.5 = merge.data.frame(joined.4, agency, by = "duns", all.x = TRUE)


####selecting and creating needed fields  

#### variables I need: Firm Age, Location, Ownership, years in SAM, survival status, biz_size, 3 years, 5 years, 10 years 


dataset.2010 = joined.5 %>% 
  mutate(firm.age = year(registrationDate) - year(businessStartDate)) %>% 
  mutate(location = ifelse(country == "USA", "1", "0")) %>% 
  mutate(ownership.woman = ifelse(womenownedflag == 1, "1", 0)) %>% 
  mutate(ownership.veteran = ifelse(veteranownedflag == 1, "1",0)) %>% 
  mutate(ownership.minority = ifelse(aiobflag == 1 |minorityownedbusinessflag == 1 | 
                                       apaobflag == 1 | 
                                       baobflag == 1 | naobflag == 1 | saaobflag == 1 | 
                                       haobflag == 1 | isnativehawaiianownedorganizationorfirm == 1 | 
                                       isotherminorityowned == 1 | istriballyownedfirm == 1 | 
                                       isalaskannativeownedcorporationorfirm == 1 | 
                                       other_minority_owned_business == 1, "1", 0)) %>% 
  mutate(ownership.foreign = ifelse(isforeignownedandlocated == 1, "1", 0)) %>% 
  mutate(years.survived = months.survived/12) %>% 
  mutate(survival.status = ifelse(year(lastsigneddate) >= 2010, "1", "0")) %>%
  mutate(three.year = years.survived>=3, "YES","NO") %>% 
  mutate(five.year = years.survived>=5, "YES","NO") %>% 
  mutate(ten.year = years.survived>=10, "YES","NO") %>%   ##only works since start at same time
  mutate(biz_size = ifelse(contractingofficerbusinesssizedetermination == "O", 1, 0)) %>% 
  select(duns, biz_size, NAICS2, ServicesCategory, location, ownership.woman, 
         ownership.veteran, ownership.minority, ownership.foreign, contract.actions,
         obligated.amt, years.survived, lastsigneddate, firm.age, three.year, five.year,ten.year, lastsigneddate, survival.status, DEPARTMENT_NAME, AGENCY_NAME, registrationDate) %>% 
  filter(NAICS2 != "NU")

#2011 ####
year.edit = final_joined %>% 
  filter(year(registrationDate) == 2011) %>% 
  group_by(duns) %>% 
  filter(year(signeddate) <= year(registrationDate) + 10) %>% 
  mutate(lastsigneddate = max(signeddate)) %>% 
  mutate(months.survived = ((year(lastsigneddate) - year(registrationDate)) * 12) + month(lastsigneddate) - month(registrationDate))
  
  #filter(contractingofficerbusinesssizedetermination == "S")
  year.edit.unique = year.edit[!duplicated(year.edit[,c('duns')]),]


##joined the above sections together by duns and selected only the columns needed

NAICS = as.data.frame(NAICS.unique.column)
year = as.data.frame(year.edit.unique)

joined.2 = merge.data.frame(NAICS, year, by = "duns")  

joined.25 = joined.2 %>%
  select(duns, NAICS2, age_at_start, months.survived, lastsigneddate, status, registrationDate, 
         businessStartDate, country, womenownedflag, veteranownedflag, aiobflag, naobflag,
         minorityownedbusinessflag, apaobflag, baobflag, baobflag, saaobflag, haobflag, 
         isnativehawaiianownedorganizationorfirm, isotherminorityowned, istriballyownedfirm, 
         isalaskannativeownedcorporationorfirm, other_minority_owned_business, 
         isforeignownedandlocated, expirationDate, contractingofficerbusinesssizedetermination)

###cross reference agency and department codes with a list of names and used one of our 
##datasets in SQL to identify the PSC codes -- used a similar method to determine agency and PSC
##for each firm as was used to determine the NAICS code

joined.3 = merge.data.frame(joined.25, PSC, by = "duns", all.x = TRUE)
joined.4 = merge.data.frame(joined.3, oblandact, by = "duns", all.x = TRUE)
joined.5 = merge.data.frame(joined.4, agency, by = "duns", all.x = TRUE)


####selecting and creating needed fields  

#### variables I need: Firm Age, Location, Ownership, years in SAM, survival status, biz_size, 3 years, 5 years, 10 years 


dataset.2011 = joined.5 %>% 
  mutate(firm.age = year(registrationDate) - year(businessStartDate)) %>% 
  mutate(location = ifelse(country == "USA", "1", "0")) %>% 
  mutate(ownership.woman = ifelse(womenownedflag == 1, "1", 0)) %>% 
  mutate(ownership.veteran = ifelse(veteranownedflag == 1, "1",0)) %>% 
  mutate(ownership.minority = ifelse(aiobflag == 1 |minorityownedbusinessflag == 1 | 
                                       apaobflag == 1 | 
                                       baobflag == 1 | naobflag == 1 | saaobflag == 1 | 
                                       haobflag == 1 | isnativehawaiianownedorganizationorfirm == 1 | 
                                       isotherminorityowned == 1 | istriballyownedfirm == 1 | 
                                       isalaskannativeownedcorporationorfirm == 1 | 
                                       other_minority_owned_business == 1, "1", 0)) %>% 
  mutate(ownership.foreign = ifelse(isforeignownedandlocated == 1, "1", 0)) %>% 
  mutate(years.survived = months.survived/12) %>% 
  mutate(survival.status = ifelse(year(lastsigneddate) >= 2010, "1", "0")) %>%
  mutate(three.year = years.survived>=3, "YES","NO") %>% 
  mutate(five.year = years.survived>=5, "YES","NO") %>% 
  mutate(ten.year = years.survived>=10, "YES","NO") %>%   ##only works since start at same time
  mutate(biz_size = ifelse(contractingofficerbusinesssizedetermination == "O", 1, 0)) %>% 
  select(duns, biz_size, NAICS2, ServicesCategory, location, ownership.woman, 
         ownership.veteran, ownership.minority, ownership.foreign, contract.actions,
         obligated.amt, years.survived, lastsigneddate, firm.age, three.year, five.year,ten.year, lastsigneddate, survival.status, DEPARTMENT_NAME, AGENCY_NAME, registrationDate) %>% 
  filter(NAICS2 != "NU")

#2012 ####
year.edit = final_joined %>% 
  filter(year(registrationDate) == 2012) %>% 
  group_by(duns) %>% 
  filter(year(signeddate) <= year(registrationDate) + 10) %>% 
  mutate(lastsigneddate = max(signeddate)) %>% 
  mutate(months.survived = ((year(lastsigneddate) - year(registrationDate)) * 12) + month(lastsigneddate) - month(registrationDate))
  
  #filter(contractingofficerbusinesssizedetermination == "S")
  year.edit.unique = year.edit[!duplicated(year.edit[,c('duns')]),]


##joined the above sections together by duns and selected only the columns needed

NAICS = as.data.frame(NAICS.unique.column)
year = as.data.frame(year.edit.unique)

joined.2 = merge.data.frame(NAICS, year, by = "duns")  

joined.25 = joined.2 %>%
  select(duns, NAICS2, age_at_start, months.survived, lastsigneddate, status, registrationDate, 
         businessStartDate, country, womenownedflag, veteranownedflag, aiobflag, naobflag,
         minorityownedbusinessflag, apaobflag, baobflag, baobflag, saaobflag, haobflag, 
         isnativehawaiianownedorganizationorfirm, isotherminorityowned, istriballyownedfirm, 
         isalaskannativeownedcorporationorfirm, other_minority_owned_business, 
         isforeignownedandlocated, expirationDate, contractingofficerbusinesssizedetermination)

###cross reference agency and department codes with a list of names and used one of our 
##datasets in SQL to identify the PSC codes -- used a similar method to determine agency and PSC
##for each firm as was used to determine the NAICS code

joined.3 = merge.data.frame(joined.25, PSC, by = "duns", all.x = TRUE)
joined.4 = merge.data.frame(joined.3, oblandact, by = "duns", all.x = TRUE)
joined.5 = merge.data.frame(joined.4, agency, by = "duns", all.x = TRUE)


####selecting and creating needed fields  

#### variables I need: Firm Age, Location, Ownership, years in SAM, survival status, biz_size, 3 years, 5 years, 10 years 


dataset.2012 = joined.5 %>% 
  mutate(firm.age = year(registrationDate) - year(businessStartDate)) %>% 
  mutate(location = ifelse(country == "USA", "1", "0")) %>% 
  mutate(ownership.woman = ifelse(womenownedflag == 1, "1", 0)) %>% 
  mutate(ownership.veteran = ifelse(veteranownedflag == 1, "1",0)) %>% 
  mutate(ownership.minority = ifelse(aiobflag == 1 |minorityownedbusinessflag == 1 | 
                                       apaobflag == 1 | 
                                       baobflag == 1 | naobflag == 1 | saaobflag == 1 | 
                                       haobflag == 1 | isnativehawaiianownedorganizationorfirm == 1 | 
                                       isotherminorityowned == 1 | istriballyownedfirm == 1 | 
                                       isalaskannativeownedcorporationorfirm == 1 | 
                                       other_minority_owned_business == 1, "1", 0)) %>% 
  mutate(ownership.foreign = ifelse(isforeignownedandlocated == 1, "1", 0)) %>% 
  mutate(years.survived = months.survived/12) %>% 
  mutate(survival.status = ifelse(year(lastsigneddate) >= 2010, "1", "0")) %>%
  mutate(three.year = years.survived>=3, "YES","NO") %>% 
  mutate(five.year = years.survived>=5, "YES","NO") %>% 
  mutate(ten.year = years.survived>=10, "YES","NO") %>%   ##only works since start at same time
  mutate(biz_size = ifelse(contractingofficerbusinesssizedetermination == "O", 1, 0)) %>% 
  select(duns, biz_size, NAICS2, ServicesCategory, location, ownership.woman, 
         ownership.veteran, ownership.minority, ownership.foreign, contract.actions,
         obligated.amt, years.survived, lastsigneddate, firm.age, three.year, five.year,ten.year, lastsigneddate, survival.status, DEPARTMENT_NAME, AGENCY_NAME, registrationDate) %>% 
  filter(NAICS2 != "NU")

#2013 ####
year.edit = final_joined %>% 
  filter(year(registrationDate) == 2013) %>% 
  group_by(duns) %>% 
  filter(year(signeddate) <= year(registrationDate) + 10) %>% 
  mutate(lastsigneddate = max(signeddate)) %>% 
  mutate(months.survived = ((year(lastsigneddate) - year(registrationDate)) * 12) + (month(lastsigneddate) - month(registrationDate)))
  
  #filter(contractingofficerbusinesssizedetermination == "S")
  year.edit.unique = year.edit[!duplicated(year.edit[,c('duns')]),]


##joined the above sections together by duns and selected only the columns needed

NAICS = as.data.frame(NAICS.unique.column)
year = as.data.frame(year.edit.unique)

joined.2 = merge.data.frame(NAICS, year, by = "duns")  

joined.25 = joined.2 %>%
  select(duns, NAICS2, age_at_start, months.survived, lastsigneddate, status, registrationDate, 
         businessStartDate, country, womenownedflag, veteranownedflag, aiobflag, naobflag,
         minorityownedbusinessflag, apaobflag, baobflag, baobflag, saaobflag, haobflag, 
         isnativehawaiianownedorganizationorfirm, isotherminorityowned, istriballyownedfirm, 
         isalaskannativeownedcorporationorfirm, other_minority_owned_business, 
         isforeignownedandlocated, expirationDate, contractingofficerbusinesssizedetermination)

###cross reference agency and department codes with a list of names and used one of our 
##datasets in SQL to identify the PSC codes -- used a similar method to determine agency and PSC
##for each firm as was used to determine the NAICS code

joined.3 = merge.data.frame(joined.25, PSC, by = "duns", all.x = TRUE)
joined.4 = merge.data.frame(joined.3, oblandact, by = "duns", all.x = TRUE)
joined.5 = merge.data.frame(joined.4, agency, by = "duns", all.x = TRUE)


####selecting and creating needed fields  

#### variables I need: Firm Age, Location, Ownership, years in SAM, survival status, biz_size, 3 years, 5 years, 10 years 


dataset.2013 = joined.5 %>% 
  mutate(firm.age = year(registrationDate) - year(businessStartDate)) %>% 
  mutate(location = ifelse(country == "USA", "1", "0")) %>% 
  mutate(ownership.woman = ifelse(womenownedflag == 1, "1", 0)) %>% 
  mutate(ownership.veteran = ifelse(veteranownedflag == 1, "1",0)) %>% 
  mutate(ownership.minority = ifelse(aiobflag == 1 |minorityownedbusinessflag == 1 | 
                                       apaobflag == 1 | 
                                       baobflag == 1 | naobflag == 1 | saaobflag == 1 | 
                                       haobflag == 1 | isnativehawaiianownedorganizationorfirm == 1 | 
                                       isotherminorityowned == 1 | istriballyownedfirm == 1 | 
                                       isalaskannativeownedcorporationorfirm == 1 | 
                                       other_minority_owned_business == 1, "1", 0)) %>% 
  mutate(ownership.foreign = ifelse(isforeignownedandlocated == 1, "1", 0)) %>% 
  mutate(years.survived = months.survived/12) %>% 
  mutate(survival.status = ifelse(year(lastsigneddate) >= 2010, "1", "0")) %>%
  mutate(three.year = years.survived>=3, "YES","NO") %>% 
  mutate(five.year = years.survived>=5, "YES","NO") %>% 
  mutate(ten.year = years.survived>=10, "YES","NO") %>%   ##only works since start at same time
  mutate(biz_size = ifelse(contractingofficerbusinesssizedetermination == "O", 1, 0)) %>% 
  select(duns, biz_size, NAICS2, ServicesCategory, location, ownership.woman, 
         ownership.veteran, ownership.minority, ownership.foreign, contract.actions,
         obligated.amt, years.survived, lastsigneddate, firm.age, three.year, five.year,ten.year, lastsigneddate, survival.status, DEPARTMENT_NAME, AGENCY_NAME, registrationDate) %>% 
  filter(NAICS2 != "NU")

#2014 ####
year.edit = final_joined %>% 
  filter(year(registrationDate) == 2014) %>% 
  group_by(duns) %>% 
  filter(year(signeddate) <= year(registrationDate) + 10) %>% 
  mutate(lastsigneddate = max(signeddate)) %>% 
  mutate(months.survived = ((year(lastsigneddate) - year(registrationDate)) * 12) + month(lastsigneddate) - month(registrationDate))
  
  #filter(contractingofficerbusinesssizedetermination == "S")
  year.edit.unique = year.edit[!duplicated(year.edit[,c('duns')]),]


##joined the above sections together by duns and selected only the columns needed

NAICS = as.data.frame(NAICS.unique.column)
year = as.data.frame(year.edit.unique)

joined.2 = merge.data.frame(NAICS, year, by = "duns")  

joined.25 = joined.2 %>%
  select(duns, NAICS2, age_at_start, months.survived, lastsigneddate, status, registrationDate, 
         businessStartDate, country, womenownedflag, veteranownedflag, aiobflag, naobflag,
         minorityownedbusinessflag, apaobflag, baobflag, baobflag, saaobflag, haobflag, 
         isnativehawaiianownedorganizationorfirm, isotherminorityowned, istriballyownedfirm, 
         isalaskannativeownedcorporationorfirm, other_minority_owned_business, 
         isforeignownedandlocated, expirationDate, contractingofficerbusinesssizedetermination)

###cross reference agency and department codes with a list of names and used one of our 
##datasets in SQL to identify the PSC codes -- used a similar method to determine agency and PSC
##for each firm as was used to determine the NAICS code

joined.3 = merge.data.frame(joined.25, PSC, by = "duns", all.x = TRUE)
joined.4 = merge.data.frame(joined.3, oblandact, by = "duns", all.x = TRUE)
joined.5 = merge.data.frame(joined.4, agency, by = "duns", all.x = TRUE)


####selecting and creating needed fields  

#### variables I need: Firm Age, Location, Ownership, years in SAM, survival status, biz_size, 3 years, 5 years, 10 years 


dataset.2014 = joined.5 %>% 
  mutate(firm.age = year(registrationDate) - year(businessStartDate)) %>% 
  mutate(location = ifelse(country == "USA", "1", "0")) %>% 
  mutate(ownership.woman = ifelse(womenownedflag == 1, "1", 0)) %>% 
  mutate(ownership.veteran = ifelse(veteranownedflag == 1, "1",0)) %>% 
  mutate(ownership.minority = ifelse(aiobflag == 1 |minorityownedbusinessflag == 1 | 
                                       apaobflag == 1 | 
                                       baobflag == 1 | naobflag == 1 | saaobflag == 1 | 
                                       haobflag == 1 | isnativehawaiianownedorganizationorfirm == 1 | 
                                       isotherminorityowned == 1 | istriballyownedfirm == 1 | 
                                       isalaskannativeownedcorporationorfirm == 1 | 
                                       other_minority_owned_business == 1, "1", 0)) %>% 
  mutate(ownership.foreign = ifelse(isforeignownedandlocated == 1, "1", 0)) %>% 
  mutate(years.survived = months.survived/12) %>% 
  mutate(survival.status = ifelse(year(lastsigneddate) >= 2010, "1", "0")) %>%
  mutate(three.year = years.survived>=3, "YES","NO") %>% 
  mutate(five.year = years.survived>=5, "YES","NO") %>% 
  mutate(ten.year = years.survived>=10, "YES","NO") %>%   ##only works since start at same time
  mutate(biz_size = ifelse(contractingofficerbusinesssizedetermination == "O", 1, 0)) %>% 
  select(duns, biz_size, NAICS2, ServicesCategory, location, ownership.woman, 
         ownership.veteran, ownership.minority, ownership.foreign, contract.actions,
         obligated.amt, years.survived, lastsigneddate, firm.age, three.year, five.year,ten.year, lastsigneddate, survival.status, DEPARTMENT_NAME, AGENCY_NAME, registrationDate) %>% 
  filter(NAICS2 != "NU")

#2015 ####
year.edit = final_joined %>% 
  filter(year(registrationDate) == 2015) %>% 
  group_by(duns) %>% 
  filter(year(signeddate) <= year(registrationDate) + 10) %>% 
  mutate(lastsigneddate = max(signeddate)) %>% 
  mutate(months.survived = ((year(lastsigneddate) - year(registrationDate)) * 12) + month(lastsigneddate) - month(registrationDate))
  
  #filter(contractingofficerbusinesssizedetermination == "S")
  year.edit.unique = year.edit[!duplicated(year.edit[,c('duns')]),]


##joined the above sections together by duns and selected only the columns needed

NAICS = as.data.frame(NAICS.unique.column)
year = as.data.frame(year.edit.unique)

joined.2 = merge.data.frame(NAICS, year, by = "duns")  

joined.25 = joined.2 %>%
  select(duns, NAICS2, age_at_start, months.survived, lastsigneddate, status, registrationDate, 
         businessStartDate, country, womenownedflag, veteranownedflag, aiobflag, naobflag,
         minorityownedbusinessflag, apaobflag, baobflag, baobflag, saaobflag, haobflag, 
         isnativehawaiianownedorganizationorfirm, isotherminorityowned, istriballyownedfirm, 
         isalaskannativeownedcorporationorfirm, other_minority_owned_business, 
         isforeignownedandlocated, expirationDate, contractingofficerbusinesssizedetermination)

###cross reference agency and department codes with a list of names and used one of our 
##datasets in SQL to identify the PSC codes -- used a similar method to determine agency and PSC
##for each firm as was used to determine the NAICS code

joined.3 = merge.data.frame(joined.25, PSC, by = "duns", all.x = TRUE)
joined.4 = merge.data.frame(joined.3, oblandact, by = "duns", all.x = TRUE)
joined.5 = merge.data.frame(joined.4, agency, by = "duns", all.x = TRUE)


####selecting and creating needed fields  

#### variables I need: Firm Age, Location, Ownership, years in SAM, survival status, biz_size, 3 years, 5 years, 10 years 


dataset.2015 = joined.5 %>% 
  mutate(firm.age = year(registrationDate) - year(businessStartDate)) %>% 
  mutate(location = ifelse(country == "USA", "1", "0")) %>% 
  mutate(ownership.woman = ifelse(womenownedflag == 1, "1", 0)) %>% 
  mutate(ownership.veteran = ifelse(veteranownedflag == 1, "1",0)) %>% 
  mutate(ownership.minority = ifelse(aiobflag == 1 |minorityownedbusinessflag == 1 | 
                                       apaobflag == 1 | 
                                       baobflag == 1 | naobflag == 1 | saaobflag == 1 | 
                                       haobflag == 1 | isnativehawaiianownedorganizationorfirm == 1 | 
                                       isotherminorityowned == 1 | istriballyownedfirm == 1 | 
                                       isalaskannativeownedcorporationorfirm == 1 | 
                                       other_minority_owned_business == 1, "1", 0)) %>% 
  mutate(ownership.foreign = ifelse(isforeignownedandlocated == 1, "1", 0)) %>% 
  mutate(years.survived = months.survived/12) %>% 
  mutate(survival.status = ifelse(year(lastsigneddate) >= 2010, "1", "0")) %>%
  mutate(three.year = years.survived>=3, "YES","NO") %>% 
  mutate(five.year = years.survived>=5, "YES","NO") %>% 
  mutate(ten.year = years.survived>=10, "YES","NO") %>%   ##only works since start at same time
  mutate(biz_size = ifelse(contractingofficerbusinesssizedetermination == "O", 1, 0)) %>% 
  select(duns, biz_size, NAICS2, ServicesCategory, location, ownership.woman, 
         ownership.veteran, ownership.minority, ownership.foreign, contract.actions,
         obligated.amt, years.survived, lastsigneddate, firm.age, three.year, five.year,ten.year, lastsigneddate, survival.status, DEPARTMENT_NAME, AGENCY_NAME, registrationDate) %>% 
  filter(NAICS2 != "NU")

#2016 ####
year.edit = final_joined %>% 
  filter(year(registrationDate) == 2016) %>% 
  group_by(duns) %>% 
  filter(year(signeddate) <= year(registrationDate) + 10) %>% 
  mutate(lastsigneddate = max(signeddate)) %>% 
  mutate(months.survived = ((year(lastsigneddate) - year(registrationDate)) * 12) + month(lastsigneddate) - month(registrationDate))
  
  #filter(contractingofficerbusinesssizedetermination == "S")
  year.edit.unique = year.edit[!duplicated(year.edit[,c('duns')]),]


##joined the above sections together by duns and selected only the columns needed

NAICS = as.data.frame(NAICS.unique.column)
year = as.data.frame(year.edit.unique)

joined.2 = merge.data.frame(NAICS, year, by = "duns")  

joined.25 = joined.2 %>%
  select(duns, NAICS2, age_at_start, months.survived, lastsigneddate, status, registrationDate, 
         businessStartDate, country, womenownedflag, veteranownedflag, aiobflag, naobflag,
         minorityownedbusinessflag, apaobflag, baobflag, baobflag, saaobflag, haobflag, 
         isnativehawaiianownedorganizationorfirm, isotherminorityowned, istriballyownedfirm, 
         isalaskannativeownedcorporationorfirm, other_minority_owned_business, 
         isforeignownedandlocated, expirationDate, contractingofficerbusinesssizedetermination)

###cross reference agency and department codes with a list of names and used one of our 
##datasets in SQL to identify the PSC codes -- used a similar method to determine agency and PSC
##for each firm as was used to determine the NAICS code

joined.3 = merge.data.frame(joined.25, PSC, by = "duns", all.x = TRUE)
joined.4 = merge.data.frame(joined.3, oblandact, by = "duns", all.x = TRUE)
joined.5 = merge.data.frame(joined.4, agency, by = "duns", all.x = TRUE)


####selecting and creating needed fields  

#### variables I need: Firm Age, Location, Ownership, years in SAM, survival status, biz_size, 3 years, 5 years, 10 years 


dataset.2016 = joined.5 %>% 
  mutate(firm.age = year(registrationDate) - year(businessStartDate)) %>% 
  mutate(location = ifelse(country == "USA", "1", "0")) %>% 
  mutate(ownership.woman = ifelse(womenownedflag == 1, "1", 0)) %>% 
  mutate(ownership.veteran = ifelse(veteranownedflag == 1, "1",0)) %>% 
  mutate(ownership.minority = ifelse(aiobflag == 1 |minorityownedbusinessflag == 1 | 
                                       apaobflag == 1 | 
                                       baobflag == 1 | naobflag == 1 | saaobflag == 1 | 
                                       haobflag == 1 | isnativehawaiianownedorganizationorfirm == 1 | 
                                       isotherminorityowned == 1 | istriballyownedfirm == 1 | 
                                       isalaskannativeownedcorporationorfirm == 1 | 
                                       other_minority_owned_business == 1, "1", 0)) %>% 
  mutate(ownership.foreign = ifelse(isforeignownedandlocated == 1, "1", 0)) %>% 
  mutate(years.survived = months.survived/12) %>% 
  mutate(survival.status = ifelse(year(lastsigneddate) >= 2010, "1", "0")) %>%
  mutate(three.year = years.survived>=3, "YES","NO") %>% 
  mutate(five.year = years.survived>=5, "YES","NO") %>% 
  mutate(ten.year = years.survived>=10, "YES","NO") %>%   ##only works since start at same time
  mutate(biz_size = ifelse(contractingofficerbusinesssizedetermination == "O", 1, 0)) %>% 
  select(duns, biz_size, NAICS2, ServicesCategory, location, ownership.woman, 
         ownership.veteran, ownership.minority, ownership.foreign, contract.actions,
         obligated.amt, years.survived, lastsigneddate, firm.age, three.year, five.year,ten.year, lastsigneddate, survival.status, DEPARTMENT_NAME, AGENCY_NAME, registrationDate) %>% 
  filter(NAICS2 != "NU")









#### bind each created dataset together####

all.dataset.SD <- rbind(dataset.2001, dataset.2002, dataset.2003, 
                        dataset.2004, dataset.2005, dataset.2006, 
                        dataset.2007, dataset.2008, dataset.2009, 
                        dataset.2010, dataset.2011, dataset.2012, 
                        dataset.2013, dataset.2014, dataset.2015,
                        dataset.2016)



write.csv(all.dataset.SD, "Panel Data reg2001-2016, SD2010-2025 - nop, 10plus1 year view.csv")

------------------------------------------------------------
  
###DOD Panel Data ####

final <- read_csv("SAM Data merged with FPDS, exp2000-2019.csv")

final_joined = final[!duplicated(final),]

agency_codes <- read.xlsx("FPDS agency codes.xlsx")

agency_codes_unique = agency_codes[!duplicated(agency_codes[,c('DEPARTMENT_ID','AGENCY_CODE')]),] 

agency.unique = agency_codes[!duplicated(agency_codes[,c('DEPARTMENT_NAME')]),] 

final_joined_DOD <- final_joined %>% 
  mutate(age_at_start = year(registrationDate) - year(businessStartDate)) %>% 
  rename(country = `samAddress countryCode`) %>% 
  left_join(agency_codes_unique, by = c("mod_agency"="AGENCY_CODE")) %>% 
  filter(DEPARTMENT_ID == "9700")


###########filtering dataset

###created a new variable for 2 digit NAICS codes and removed NA fields
NAICS.edit = final_joined_DOD %>% 
  mutate(NAICS2 = substr(principalnaicscode, 0, 2)) %>%
  filter(!is.na(principalnaicscode)) %>% 
  filter(NAICS2 != "NU")

NAICS.edit$NAICS2[NAICS.edit$NAICS2 == 31] = "31-33"
NAICS.edit$NAICS2[NAICS.edit$NAICS2 == 32] = "31-33"
NAICS.edit$NAICS2[NAICS.edit$NAICS2 == 33] = "31-33"
NAICS.edit$NAICS2[NAICS.edit$NAICS2 == 44] = "44-45"
NAICS.edit$NAICS2[NAICS.edit$NAICS2 == 45] = "44-45"
NAICS.edit$NAICS2[NAICS.edit$NAICS2 == 48] = "48-49"
NAICS.edit$NAICS2[NAICS.edit$NAICS2 == 49] = "48-49"  

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


psc_names <- read_csv("PSC DIIG categories.csv")

psc_names$productorservicecode <- as.character(psc_names$ProductOrServiceCode)

final_joined_DOD$productorservicecode <- as.character(final_joined_DOD$productorservicecode)


name.defined <- final_joined_DOD %>% 
  left_join(psc_names[ , c("productorservicecode","ServicesCategory")], by = "productorservicecode") %>% 
  select(duns, productorservicecode, ServicesCategory, DEPARTMENT_ID, mod_agency, DEPARTMENT_NAME, AGENCY_NAME, obligatedamount)

agency <- name.defined %>% 
  select(duns, DEPARTMENT_ID, mod_agency, DEPARTMENT_NAME, AGENCY_NAME, obligatedamount) %>% 
  filter(!is.na(AGENCY_NAME)) %>%
  group_by(duns, DEPARTMENT_NAME, AGENCY_NAME) %>% 
  summarize(max(obligatedamount)) %>% 
  arrange(desc(`max(obligatedamount)`))

agency.unique = agency[!duplicated(agency[,c("duns")]),]


PSC_code = name.defined %>% 
  select(duns, obligatedamount, productorservicecode, ServicesCategory) %>% 
  filter(!is.na(productorservicecode)) %>%
  filter(productorservicecode != 0, productorservicecode != "N: NO - SERVICE WHERE PBA IS NOT USED.") %>% 
  group_by(duns, ServicesCategory) %>% 
  summarize(max(obligatedamount)) %>% 
  arrange(desc(`max(obligatedamount)`))

psc.code.unique = PSC_code[!duplicated(PSC_code[,c("duns")]),]


###devised the fields total contract actions (all contracts that a certain firm had between 
##the given timeframe) and total obligated amount (sum of obligated amount for each contract)

unique.contract <- final_joined_DOD[!duplicated(final_joined_DOD[,c("unique_transaction_id")]),]

contobsac <- unique.contract %>% 
  select(duns, obligatedamount, unique_transaction_id) %>% 
  group_by(duns) %>% 
  summarize(obligated.amt = sum(obligatedamount), contract.actions = n())

#### joined these to the previous dataframe


PSC = as.data.frame(psc.code.unique)
oblandact = as.data.frame(contobsac)
agency = as.data.frame(agency.unique)

#2001 ####
year.edit = final_joined_DOD %>% 
  filter(year(registrationDate) == 2001) %>% 
  group_by(duns) %>% 
  filter(year(signeddate) <= year(registrationDate) + 10) %>% 
  mutate(lastsigneddate = max(signeddate)) %>% 
  mutate(months.survived = ((year(lastsigneddate) - year(registrationDate)) * 12) 
         + month(lastsigneddate) - month(registrationDate))

#filter(contractingofficerbusinesssizedetermination == "S")
year.edit.unique = year.edit[!duplicated(year.edit[,c('duns')]),]

##joined the above sections together by duns and selected only the columns needed

NAICS = as.data.frame(NAICS.unique.column)
year = as.data.frame(year.edit.unique)

joined.2 = merge.data.frame(NAICS, year, by = "duns")  

joined.25 = joined.2 %>%
  select(duns, NAICS2, age_at_start, months.survived, lastsigneddate, status, registrationDate, 
         businessStartDate, country, womenownedflag, veteranownedflag, aiobflag, naobflag,
         minorityownedbusinessflag, apaobflag, baobflag, baobflag, saaobflag, haobflag, 
         isnativehawaiianownedorganizationorfirm, isotherminorityowned, istriballyownedfirm, 
         isalaskannativeownedcorporationorfirm, other_minority_owned_business, 
         isforeignownedandlocated, expirationDate, contractingofficerbusinesssizedetermination)

###cross reference agency and department codes with a list of names and used one of our 
##datasets in SQL to identify the PSC codes -- used a similar method to determine agency and PSC
##for each firm as was used to determine the NAICS code

joined.3 = merge.data.frame(joined.25, PSC, by = "duns", all.x = TRUE)
joined.4 = merge.data.frame(joined.3, oblandact, by = "duns", all.x = TRUE)
joined.5 = merge.data.frame(joined.4, agency, by = "duns", all.x = TRUE)


####selecting and creating needed fields  

#### variables I need: Firm Age, Location, Ownership, years in SAM, survival status, biz_size, 3 years, 5 years, 10 years 


dataset.2001 = joined.5 %>% 
  mutate(firm.age = year(registrationDate) - year(businessStartDate)) %>% 
  mutate(location = ifelse(country == "USA", "1", "0")) %>% 
  mutate(ownership.woman = ifelse(womenownedflag == 1, "1", 0)) %>% 
  mutate(ownership.veteran = ifelse(veteranownedflag == 1, "1",0)) %>% 
  mutate(ownership.minority = ifelse(aiobflag == 1 |minorityownedbusinessflag == 1 | 
                                       apaobflag == 1 | 
                                       baobflag == 1 | naobflag == 1 | saaobflag == 1 | 
                                       haobflag == 1 | isnativehawaiianownedorganizationorfirm == 1 | 
                                       isotherminorityowned == 1 | istriballyownedfirm == 1 | 
                                       isalaskannativeownedcorporationorfirm == 1 | 
                                       other_minority_owned_business == 1, "1", 0)) %>% 
  mutate(ownership.foreign = ifelse(isforeignownedandlocated == 1, "1", 0)) %>% 
  mutate(years.survived = months.survived/12) %>% 
  mutate(survival.status = ifelse(year(lastsigneddate) >= 2010, "1", "0")) %>%
  mutate(three.year = years.survived>=3, "YES","NO") %>% 
  mutate(five.year = years.survived>=5, "YES","NO") %>% 
  mutate(ten.year = years.survived>=10, "YES","NO") %>%   ##only works since start at same time
  mutate(biz_size = ifelse(contractingofficerbusinesssizedetermination == "O", 1, 0)) %>% 
  select(duns, biz_size, NAICS2, ServicesCategory, location, ownership.woman, 
         ownership.veteran, ownership.minority, ownership.foreign, contract.actions,
         obligated.amt, years.survived, lastsigneddate, firm.age, three.year, five.year,ten.year, lastsigneddate, survival.status, DEPARTMENT_NAME, AGENCY_NAME, registrationDate) %>% 
  filter(NAICS2 != "NU")

#2002 ####
year.edit = final_joined_DOD %>% 
  filter(year(registrationDate) == 2002) %>% 
  group_by(duns) %>% 
  filter(year(signeddate) <= year(registrationDate) + 10) %>% 
  mutate(lastsigneddate = max(signeddate)) %>% 
  mutate(months.survived = ((year(lastsigneddate) - year(registrationDate)) * 12) 
         + month(lastsigneddate) - month(registrationDate))

#filter(contractingofficerbusinesssizedetermination == "S")
year.edit.unique = year.edit[!duplicated(year.edit[,c('duns')]),]


##joined the above sections together by duns and selected only the columns needed

NAICS = as.data.frame(NAICS.unique.column)
year = as.data.frame(year.edit.unique)

joined.2 = merge.data.frame(NAICS, year, by = "duns")  

joined.25 = joined.2 %>%
  select(duns, NAICS2, age_at_start, months.survived, lastsigneddate, status, registrationDate, 
         businessStartDate, country, womenownedflag, veteranownedflag, aiobflag, naobflag,
         minorityownedbusinessflag, apaobflag, baobflag, baobflag, saaobflag, haobflag, 
         isnativehawaiianownedorganizationorfirm, isotherminorityowned, istriballyownedfirm, 
         isalaskannativeownedcorporationorfirm, other_minority_owned_business, 
         isforeignownedandlocated, expirationDate, contractingofficerbusinesssizedetermination)

###cross reference agency and department codes with a list of names and used one of our 
##datasets in SQL to identify the PSC codes -- used a similar method to determine agency and PSC
##for each firm as was used to determine the NAICS code

joined.3 = merge.data.frame(joined.25, PSC, by = "duns", all.x = TRUE)
joined.4 = merge.data.frame(joined.3, oblandact, by = "duns", all.x = TRUE)
joined.5 = merge.data.frame(joined.4, agency, by = "duns", all.x = TRUE)


####selecting and creating needed fields  

#### variables I need: Firm Age, Location, Ownership, years in SAM, survival status, biz_size, 3 years, 5 years, 10 years 


dataset.2002 = joined.5 %>% 
  mutate(firm.age = year(registrationDate) - year(businessStartDate)) %>% 
  mutate(location = ifelse(country == "USA", "1", "0")) %>% 
  mutate(ownership.woman = ifelse(womenownedflag == 1, "1", 0)) %>% 
  mutate(ownership.veteran = ifelse(veteranownedflag == 1, "1",0)) %>% 
  mutate(ownership.minority = ifelse(aiobflag == 1 |minorityownedbusinessflag == 1 | 
                                       apaobflag == 1 | 
                                       baobflag == 1 | naobflag == 1 | saaobflag == 1 | 
                                       haobflag == 1 | isnativehawaiianownedorganizationorfirm == 1 | 
                                       isotherminorityowned == 1 | istriballyownedfirm == 1 | 
                                       isalaskannativeownedcorporationorfirm == 1 | 
                                       other_minority_owned_business == 1, "1", 0)) %>% 
  mutate(ownership.foreign = ifelse(isforeignownedandlocated == 1, "1", 0)) %>% 
  mutate(years.survived = months.survived/12) %>% 
  mutate(survival.status = ifelse(year(lastsigneddate) >= 2010, "1", "0")) %>%
  mutate(three.year = years.survived>=3, "YES","NO") %>% 
  mutate(five.year = years.survived>=5, "YES","NO") %>% 
  mutate(ten.year = years.survived>=10, "YES","NO") %>%   ##only works since start at same time
  mutate(biz_size = ifelse(contractingofficerbusinesssizedetermination == "O", 1, 0)) %>% 
  select(duns, biz_size, NAICS2, ServicesCategory, location, ownership.woman, 
         ownership.veteran, ownership.minority, ownership.foreign, contract.actions,
         obligated.amt, years.survived, lastsigneddate, firm.age, three.year, five.year,ten.year, lastsigneddate, survival.status, DEPARTMENT_NAME, AGENCY_NAME, registrationDate) %>% 
  filter(NAICS2 != "NU")

#2003 ####
year.edit = final_joined_DOD %>% 
  filter(year(registrationDate) == 2003) %>% 
  group_by(duns) %>% 
  filter(year(signeddate) <= year(registrationDate) + 10) %>% 
  mutate(lastsigneddate = max(signeddate)) %>% 
  mutate(months.survived = ((year(lastsigneddate) - year(registrationDate)) * 12) 
         + month(lastsigneddate) - month(registrationDate)) 

#filter(contractingofficerbusinesssizedetermination == "S")
year.edit.unique = year.edit[!duplicated(year.edit[,c('duns')]),]


##joined the above sections together by duns and selected only the columns needed

NAICS = as.data.frame(NAICS.unique.column)
year = as.data.frame(year.edit.unique)

joined.2 = merge.data.frame(NAICS, year, by = "duns")  

joined.25 = joined.2 %>%
  select(duns, NAICS2, age_at_start, months.survived, lastsigneddate, status, registrationDate, 
         businessStartDate, country, womenownedflag, veteranownedflag, aiobflag, naobflag,
         minorityownedbusinessflag, apaobflag, baobflag, baobflag, saaobflag, haobflag, 
         isnativehawaiianownedorganizationorfirm, isotherminorityowned, istriballyownedfirm, 
         isalaskannativeownedcorporationorfirm, other_minority_owned_business, 
         isforeignownedandlocated, expirationDate, contractingofficerbusinesssizedetermination)

###cross reference agency and department codes with a list of names and used one of our 
##datasets in SQL to identify the PSC codes -- used a similar method to determine agency and PSC
##for each firm as was used to determine the NAICS code

joined.3 = merge.data.frame(joined.25, PSC, by = "duns", all.x = TRUE)
joined.4 = merge.data.frame(joined.3, oblandact, by = "duns", all.x = TRUE)
joined.5 = merge.data.frame(joined.4, agency, by = "duns", all.x = TRUE)


####selecting and creating needed fields  

#### variables I need: Firm Age, Location, Ownership, years in SAM, survival status, biz_size, 3 years, 5 years, 10 years 


dataset.2003 = joined.5 %>% 
  mutate(firm.age = year(registrationDate) - year(businessStartDate)) %>% 
  mutate(location = ifelse(country == "USA", "1", "0")) %>% 
  mutate(ownership.woman = ifelse(womenownedflag == 1, "1", 0)) %>% 
  mutate(ownership.veteran = ifelse(veteranownedflag == 1, "1",0)) %>% 
  mutate(ownership.minority = ifelse(aiobflag == 1 |minorityownedbusinessflag == 1 | 
                                       apaobflag == 1 | 
                                       baobflag == 1 | naobflag == 1 | saaobflag == 1 | 
                                       haobflag == 1 | isnativehawaiianownedorganizationorfirm == 1 | 
                                       isotherminorityowned == 1 | istriballyownedfirm == 1 | 
                                       isalaskannativeownedcorporationorfirm == 1 | 
                                       other_minority_owned_business == 1, "1", 0)) %>% 
  mutate(ownership.foreign = ifelse(isforeignownedandlocated == 1, "1", 0)) %>% 
  mutate(years.survived = months.survived/12) %>% 
  mutate(survival.status = ifelse(year(lastsigneddate) >= 2010, "1", "0")) %>%
  mutate(three.year = years.survived>=3, "YES","NO") %>% 
  mutate(five.year = years.survived>=5, "YES","NO") %>% 
  mutate(ten.year = years.survived>=10, "YES","NO") %>%   ##only works since start at same time
  mutate(biz_size = ifelse(contractingofficerbusinesssizedetermination == "O", 1, 0)) %>% 
  select(duns, biz_size, NAICS2, ServicesCategory, location, ownership.woman, 
         ownership.veteran, ownership.minority, ownership.foreign, contract.actions,
         obligated.amt, years.survived, lastsigneddate, firm.age, three.year, five.year,ten.year, lastsigneddate, survival.status, DEPARTMENT_NAME, AGENCY_NAME, registrationDate) %>% 
  filter(NAICS2 != "NU")

#2004 ####
year.edit = final_joined_DOD %>% 
  filter(year(registrationDate) == 2004) %>% 
  group_by(duns) %>% 
  filter(year(signeddate) <= year(registrationDate) + 10) %>% 
  mutate(lastsigneddate = max(signeddate)) %>% 
  mutate(months.survived = ((year(lastsigneddate) - year(registrationDate)) * 12) 
         + month(lastsigneddate) - month(registrationDate)) 

#filter(contractingofficerbusinesssizedetermination == "S")
year.edit.unique = year.edit[!duplicated(year.edit[,c('duns')]),]


##joined the above sections together by duns and selected only the columns needed

NAICS = as.data.frame(NAICS.unique.column)
year = as.data.frame(year.edit.unique)

joined.2 = merge.data.frame(NAICS, year, by = "duns")  

joined.25 = joined.2 %>%
  select(duns, NAICS2, age_at_start, months.survived, lastsigneddate, status, registrationDate, 
         businessStartDate, country, womenownedflag, veteranownedflag, aiobflag, naobflag,
         minorityownedbusinessflag, apaobflag, baobflag, baobflag, saaobflag, haobflag, 
         isnativehawaiianownedorganizationorfirm, isotherminorityowned, istriballyownedfirm, 
         isalaskannativeownedcorporationorfirm, other_minority_owned_business, 
         isforeignownedandlocated, expirationDate, contractingofficerbusinesssizedetermination)

###cross reference agency and department codes with a list of names and used one of our 
##datasets in SQL to identify the PSC codes -- used a similar method to determine agency and PSC
##for each firm as was used to determine the NAICS code

joined.3 = merge.data.frame(joined.25, PSC, by = "duns", all.x = TRUE)
joined.4 = merge.data.frame(joined.3, oblandact, by = "duns", all.x = TRUE)
joined.5 = merge.data.frame(joined.4, agency, by = "duns", all.x = TRUE)


####selecting and creating needed fields  

#### variables I need: Firm Age, Location, Ownership, years in SAM, survival status, biz_size, 3 years, 5 years, 10 years 


dataset.2004 = joined.5 %>% 
  mutate(firm.age = year(registrationDate) - year(businessStartDate)) %>% 
  mutate(location = ifelse(country == "USA", "1", "0")) %>% 
  mutate(ownership.woman = ifelse(womenownedflag == 1, "1", 0)) %>% 
  mutate(ownership.veteran = ifelse(veteranownedflag == 1, "1",0)) %>% 
  mutate(ownership.minority = ifelse(aiobflag == 1 |minorityownedbusinessflag == 1 | 
                                       apaobflag == 1 | 
                                       baobflag == 1 | naobflag == 1 | saaobflag == 1 | 
                                       haobflag == 1 | isnativehawaiianownedorganizationorfirm == 1 | 
                                       isotherminorityowned == 1 | istriballyownedfirm == 1 | 
                                       isalaskannativeownedcorporationorfirm == 1 | 
                                       other_minority_owned_business == 1, "1", 0)) %>% 
  mutate(ownership.foreign = ifelse(isforeignownedandlocated == 1, "1", 0)) %>% 
  mutate(years.survived = months.survived/12) %>% 
  mutate(survival.status = ifelse(year(lastsigneddate) >= 2010, "1", "0")) %>%
  mutate(three.year = years.survived>=3, "YES","NO") %>% 
  mutate(five.year = years.survived>=5, "YES","NO") %>% 
  mutate(ten.year = years.survived>=10, "YES","NO") %>%   ##only works since start at same time
  mutate(biz_size = ifelse(contractingofficerbusinesssizedetermination == "O", 1, 0)) %>% 
  select(duns, biz_size, NAICS2, ServicesCategory, location, ownership.woman, 
         ownership.veteran, ownership.minority, ownership.foreign, contract.actions,
         obligated.amt, years.survived, lastsigneddate, firm.age, three.year, five.year,ten.year, lastsigneddate, survival.status, DEPARTMENT_NAME, AGENCY_NAME, registrationDate) %>% 
  filter(NAICS2 != "NU")

#2005 ####
year.edit = final_joined_DOD %>% 
  filter(year(registrationDate) == 2005) %>% 
  group_by(duns) %>% 
  filter(year(signeddate) <= year(registrationDate) + 10) %>% 
  mutate(lastsigneddate = max(signeddate)) %>% 
  mutate(months.survived = ((year(lastsigneddate) - year(registrationDate)) * 12)
         + month(lastsigneddate) - month(registrationDate))

#filter(contractingofficerbusinesssizedetermination == "S")
year.edit.unique = year.edit[!duplicated(year.edit[,c('duns')]),]


##joined the above sections together by duns and selected only the columns needed

NAICS = as.data.frame(NAICS.unique.column)
year = as.data.frame(year.edit.unique)

joined.2 = merge.data.frame(NAICS, year, by = "duns")  

joined.25 = joined.2 %>%
  select(duns, NAICS2, age_at_start, months.survived, lastsigneddate, status, registrationDate, 
         businessStartDate, country, womenownedflag, veteranownedflag, aiobflag, naobflag,
         minorityownedbusinessflag, apaobflag, baobflag, baobflag, saaobflag, haobflag, 
         isnativehawaiianownedorganizationorfirm, isotherminorityowned, istriballyownedfirm, 
         isalaskannativeownedcorporationorfirm, other_minority_owned_business, 
         isforeignownedandlocated, expirationDate, contractingofficerbusinesssizedetermination)

###cross reference agency and department codes with a list of names and used one of our 
##datasets in SQL to identify the PSC codes -- used a similar method to determine agency and PSC
##for each firm as was used to determine the NAICS code

joined.3 = merge.data.frame(joined.25, PSC, by = "duns", all.x = TRUE)
joined.4 = merge.data.frame(joined.3, oblandact, by = "duns", all.x = TRUE)
joined.5 = merge.data.frame(joined.4, agency, by = "duns", all.x = TRUE)


####selecting and creating needed fields  

#### variables I need: Firm Age, Location, Ownership, years in SAM, survival status, biz_size, 3 years, 5 years, 10 years 


dataset.2005 = joined.5 %>% 
  mutate(firm.age = year(registrationDate) - year(businessStartDate)) %>% 
  mutate(location = ifelse(country == "USA", "1", "0")) %>% 
  mutate(ownership.woman = ifelse(womenownedflag == 1, "1", 0)) %>% 
  mutate(ownership.veteran = ifelse(veteranownedflag == 1, "1",0)) %>% 
  mutate(ownership.minority = ifelse(aiobflag == 1 |minorityownedbusinessflag == 1 | 
                                       apaobflag == 1 | 
                                       baobflag == 1 | naobflag == 1 | saaobflag == 1 | 
                                       haobflag == 1 | isnativehawaiianownedorganizationorfirm == 1 | 
                                       isotherminorityowned == 1 | istriballyownedfirm == 1 | 
                                       isalaskannativeownedcorporationorfirm == 1 | 
                                       other_minority_owned_business == 1, "1", 0)) %>% 
  mutate(ownership.foreign = ifelse(isforeignownedandlocated == 1, "1", 0)) %>% 
  mutate(years.survived = months.survived/12) %>% 
  mutate(survival.status = ifelse(year(lastsigneddate) >= 2010, "1", "0")) %>%
  mutate(three.year = years.survived>=3, "YES","NO") %>% 
  mutate(five.year = years.survived>=5, "YES","NO") %>% 
  mutate(ten.year = years.survived>=10, "YES","NO") %>%   ##only works since start at same time
  mutate(biz_size = ifelse(contractingofficerbusinesssizedetermination == "O", 1, 0)) %>% 
  select(duns, biz_size, NAICS2, ServicesCategory, location, ownership.woman, 
         ownership.veteran, ownership.minority, ownership.foreign, contract.actions,
         obligated.amt, years.survived, lastsigneddate, firm.age, three.year, five.year,ten.year, lastsigneddate, survival.status, DEPARTMENT_NAME, AGENCY_NAME, registrationDate) %>% 
  filter(NAICS2 != "NU")

#2006 ####
year.edit = final_joined_DOD %>% 
  filter(year(registrationDate) == 2006) %>% 
  group_by(duns) %>% 
  filter(year(signeddate) <= year(registrationDate) + 10) %>% 
  mutate(lastsigneddate = max(signeddate)) %>% 
  mutate(months.survived = ((year(lastsigneddate) - year(registrationDate)) * 12) 
         + month(lastsigneddate) - month(registrationDate)) 

#filter(contractingofficerbusinesssizedetermination == "S")
year.edit.unique = year.edit[!duplicated(year.edit[,c('duns')]),]


##joined the above sections together by duns and selected only the columns needed

NAICS = as.data.frame(NAICS.unique.column)
year = as.data.frame(year.edit.unique)

joined.2 = merge.data.frame(NAICS, year, by = "duns")  

joined.25 = joined.2 %>%
  select(duns, NAICS2, age_at_start, months.survived, lastsigneddate, status, registrationDate, 
         businessStartDate, country, womenownedflag, veteranownedflag, aiobflag, naobflag,
         minorityownedbusinessflag, apaobflag, baobflag, baobflag, saaobflag, haobflag, 
         isnativehawaiianownedorganizationorfirm, isotherminorityowned, istriballyownedfirm, 
         isalaskannativeownedcorporationorfirm, other_minority_owned_business, 
         isforeignownedandlocated, expirationDate, contractingofficerbusinesssizedetermination)

###cross reference agency and department codes with a list of names and used one of our 
##datasets in SQL to identify the PSC codes -- used a similar method to determine agency and PSC
##for each firm as was used to determine the NAICS code

joined.3 = merge.data.frame(joined.25, PSC, by = "duns", all.x = TRUE)
joined.4 = merge.data.frame(joined.3, oblandact, by = "duns", all.x = TRUE)
joined.5 = merge.data.frame(joined.4, agency, by = "duns", all.x = TRUE)


####selecting and creating needed fields  

#### variables I need: Firm Age, Location, Ownership, years in SAM, survival status, biz_size, 3 years, 5 years, 10 years 


dataset.2006 = joined.5 %>% 
  mutate(firm.age = year(registrationDate) - year(businessStartDate)) %>% 
  mutate(location = ifelse(country == "USA", "1", "0")) %>% 
  mutate(ownership.woman = ifelse(womenownedflag == 1, "1", 0)) %>% 
  mutate(ownership.veteran = ifelse(veteranownedflag == 1, "1",0)) %>% 
  mutate(ownership.minority = ifelse(aiobflag == 1 |minorityownedbusinessflag == 1 | 
                                       apaobflag == 1 | 
                                       baobflag == 1 | naobflag == 1 | saaobflag == 1 | 
                                       haobflag == 1 | isnativehawaiianownedorganizationorfirm == 1 | 
                                       isotherminorityowned == 1 | istriballyownedfirm == 1 | 
                                       isalaskannativeownedcorporationorfirm == 1 | 
                                       other_minority_owned_business == 1, "1", 0)) %>% 
  mutate(ownership.foreign = ifelse(isforeignownedandlocated == 1, "1", 0)) %>% 
  mutate(years.survived = months.survived/12) %>% 
  mutate(survival.status = ifelse(year(lastsigneddate) >= 2010, "1", "0")) %>%
  mutate(three.year = years.survived>=3, "YES","NO") %>% 
  mutate(five.year = years.survived>=5, "YES","NO") %>% 
  mutate(ten.year = years.survived>=10, "YES","NO") %>%   ##only works since start at same time
  mutate(biz_size = ifelse(contractingofficerbusinesssizedetermination == "O", 1, 0)) %>% 
  select(duns, biz_size, NAICS2, ServicesCategory, location, ownership.woman, 
         ownership.veteran, ownership.minority, ownership.foreign, contract.actions,
         obligated.amt, years.survived, lastsigneddate, firm.age, three.year, five.year,ten.year, lastsigneddate, survival.status, DEPARTMENT_NAME, AGENCY_NAME, registrationDate) %>% 
  filter(NAICS2 != "NU")

#2007 ####
year.edit = final_joined_DOD %>% 
  filter(year(registrationDate) == 2007) %>% 
  group_by(duns) %>% 
  filter(year(signeddate) <= year(registrationDate) + 10) %>% 
  mutate(lastsigneddate = max(signeddate)) %>% 
  mutate(months.survived = ((year(lastsigneddate) - year(registrationDate)) * 12) 
         + month(lastsigneddate) - month(registrationDate)) 

#filter(contractingofficerbusinesssizedetermination == "S")
year.edit.unique = year.edit[!duplicated(year.edit[,c('duns')]),]


##joined the above sections together by duns and selected only the columns needed

NAICS = as.data.frame(NAICS.unique.column)
year = as.data.frame(year.edit.unique)

joined.2 = merge.data.frame(NAICS, year, by = "duns")  

joined.25 = joined.2 %>%
  select(duns, NAICS2, age_at_start, months.survived, lastsigneddate, status, registrationDate, 
         businessStartDate, country, womenownedflag, veteranownedflag, aiobflag, naobflag,
         minorityownedbusinessflag, apaobflag, baobflag, baobflag, saaobflag, haobflag, 
         isnativehawaiianownedorganizationorfirm, isotherminorityowned, istriballyownedfirm, 
         isalaskannativeownedcorporationorfirm, other_minority_owned_business, 
         isforeignownedandlocated, expirationDate, contractingofficerbusinesssizedetermination)

###cross reference agency and department codes with a list of names and used one of our 
##datasets in SQL to identify the PSC codes -- used a similar method to determine agency and PSC
##for each firm as was used to determine the NAICS code

joined.3 = merge.data.frame(joined.25, PSC, by = "duns", all.x = TRUE)
joined.4 = merge.data.frame(joined.3, oblandact, by = "duns", all.x = TRUE)
joined.5 = merge.data.frame(joined.4, agency, by = "duns", all.x = TRUE)


####selecting and creating needed fields  

#### variables I need: Firm Age, Location, Ownership, years in SAM, survival status, biz_size, 3 years, 5 years, 10 years 


dataset.2007 = joined.5 %>% 
  mutate(firm.age = year(registrationDate) - year(businessStartDate)) %>% 
  mutate(location = ifelse(country == "USA", "1", "0")) %>% 
  mutate(ownership.woman = ifelse(womenownedflag == 1, "1", 0)) %>% 
  mutate(ownership.veteran = ifelse(veteranownedflag == 1, "1",0)) %>% 
  mutate(ownership.minority = ifelse(aiobflag == 1 |minorityownedbusinessflag == 1 | 
                                       apaobflag == 1 | 
                                       baobflag == 1 | naobflag == 1 | saaobflag == 1 | 
                                       haobflag == 1 | isnativehawaiianownedorganizationorfirm == 1 | 
                                       isotherminorityowned == 1 | istriballyownedfirm == 1 | 
                                       isalaskannativeownedcorporationorfirm == 1 | 
                                       other_minority_owned_business == 1, "1", 0)) %>% 
  mutate(ownership.foreign = ifelse(isforeignownedandlocated == 1, "1", 0)) %>% 
  mutate(years.survived = months.survived/12) %>% 
  mutate(survival.status = ifelse(year(lastsigneddate) >= 2010, "1", "0")) %>%
  mutate(three.year = years.survived>=3, "YES","NO") %>% 
  mutate(five.year = years.survived>=5, "YES","NO") %>% 
  mutate(ten.year = years.survived>=10, "YES","NO") %>%   ##only works since start at same time
  mutate(biz_size = ifelse(contractingofficerbusinesssizedetermination == "O", 1, 0)) %>% 
  select(duns, biz_size, NAICS2, ServicesCategory, location, ownership.woman, 
         ownership.veteran, ownership.minority, ownership.foreign, contract.actions,
         obligated.amt, years.survived, lastsigneddate, firm.age, three.year, five.year,ten.year, lastsigneddate, survival.status, DEPARTMENT_NAME, AGENCY_NAME, registrationDate) %>% 
  filter(NAICS2 != "NU")

#2008 ####
year.edit = final_joined_DOD %>% 
  filter(year(registrationDate) == 2008) %>% 
  group_by(duns) %>% 
  filter(year(signeddate) <= year(registrationDate) + 10) %>% 
  mutate(lastsigneddate = max(signeddate)) %>% 
  mutate(months.survived = ((year(lastsigneddate) - year(registrationDate)) * 12) 
         + month(lastsigneddate) - month(registrationDate))

#filter(contractingofficerbusinesssizedetermination == "S")
year.edit.unique = year.edit[!duplicated(year.edit[,c('duns')]),]


##joined the above sections together by duns and selected only the columns needed

NAICS = as.data.frame(NAICS.unique.column)
year = as.data.frame(year.edit.unique)

joined.2 = merge.data.frame(NAICS, year, by = "duns")  

joined.25 = joined.2 %>%
  select(duns, NAICS2, age_at_start, months.survived, lastsigneddate, status, registrationDate, 
         businessStartDate, country, womenownedflag, veteranownedflag, aiobflag, naobflag,
         minorityownedbusinessflag, apaobflag, baobflag, baobflag, saaobflag, haobflag, 
         isnativehawaiianownedorganizationorfirm, isotherminorityowned, istriballyownedfirm, 
         isalaskannativeownedcorporationorfirm, other_minority_owned_business, 
         isforeignownedandlocated, expirationDate, contractingofficerbusinesssizedetermination)

###cross reference agency and department codes with a list of names and used one of our 
##datasets in SQL to identify the PSC codes -- used a similar method to determine agency and PSC
##for each firm as was used to determine the NAICS code

joined.3 = merge.data.frame(joined.25, PSC, by = "duns", all.x = TRUE)
joined.4 = merge.data.frame(joined.3, oblandact, by = "duns", all.x = TRUE)
joined.5 = merge.data.frame(joined.4, agency, by = "duns", all.x = TRUE)


####selecting and creating needed fields  

#### variables I need: Firm Age, Location, Ownership, years in SAM, survival status, biz_size, 3 years, 5 years, 10 years 


dataset.2008 = joined.5 %>% 
  mutate(firm.age = year(registrationDate) - year(businessStartDate)) %>% 
  mutate(location = ifelse(country == "USA", "1", "0")) %>% 
  mutate(ownership.woman = ifelse(womenownedflag == 1, "1", 0)) %>% 
  mutate(ownership.veteran = ifelse(veteranownedflag == 1, "1",0)) %>% 
  mutate(ownership.minority = ifelse(aiobflag == 1 |minorityownedbusinessflag == 1 | 
                                       apaobflag == 1 | 
                                       baobflag == 1 | naobflag == 1 | saaobflag == 1 | 
                                       haobflag == 1 | isnativehawaiianownedorganizationorfirm == 1 | 
                                       isotherminorityowned == 1 | istriballyownedfirm == 1 | 
                                       isalaskannativeownedcorporationorfirm == 1 | 
                                       other_minority_owned_business == 1, "1", 0)) %>% 
  mutate(ownership.foreign = ifelse(isforeignownedandlocated == 1, "1", 0)) %>% 
  mutate(years.survived = months.survived/12) %>% 
  mutate(survival.status = ifelse(year(lastsigneddate) >= 2010, "1", "0")) %>%
  mutate(three.year = years.survived>=3, "YES","NO") %>% 
  mutate(five.year = years.survived>=5, "YES","NO") %>% 
  mutate(ten.year = years.survived>=10, "YES","NO") %>%   ##only works since start at same time
  mutate(biz_size = ifelse(contractingofficerbusinesssizedetermination == "O", 1, 0)) %>% 
  select(duns, biz_size, NAICS2, ServicesCategory, location, ownership.woman, 
         ownership.veteran, ownership.minority, ownership.foreign, contract.actions,
         obligated.amt, years.survived, lastsigneddate, firm.age, three.year, five.year,ten.year, lastsigneddate, survival.status, DEPARTMENT_NAME, AGENCY_NAME, registrationDate) %>% 
  filter(NAICS2 != "NU")

#2009 ####
year.edit = final_joined_DOD %>% 
  filter(year(registrationDate) == 2009) %>% 
  group_by(duns) %>% 
  filter(year(signeddate) <= year(registrationDate) + 10) %>% 
  mutate(lastsigneddate = max(signeddate)) %>% 
  mutate(months.survived = ((year(lastsigneddate) - year(registrationDate)) * 12) + month(lastsigneddate) - month(registrationDate))

#filter(contractingofficerbusinesssizedetermination == "S")
year.edit.unique = year.edit[!duplicated(year.edit[,c('duns')]),]


##joined the above sections together by duns and selected only the columns needed

NAICS = as.data.frame(NAICS.unique.column)
year = as.data.frame(year.edit.unique)

joined.2 = merge.data.frame(NAICS, year, by = "duns")  

joined.25 = joined.2 %>%
  select(duns, NAICS2, age_at_start, months.survived, lastsigneddate, status, registrationDate, 
         businessStartDate, country, womenownedflag, veteranownedflag, aiobflag, naobflag,
         minorityownedbusinessflag, apaobflag, baobflag, baobflag, saaobflag, haobflag, 
         isnativehawaiianownedorganizationorfirm, isotherminorityowned, istriballyownedfirm, 
         isalaskannativeownedcorporationorfirm, other_minority_owned_business, 
         isforeignownedandlocated, expirationDate, contractingofficerbusinesssizedetermination)

###cross reference agency and department codes with a list of names and used one of our 
##datasets in SQL to identify the PSC codes -- used a similar method to determine agency and PSC
##for each firm as was used to determine the NAICS code

joined.3 = merge.data.frame(joined.25, PSC, by = "duns", all.x = TRUE)
joined.4 = merge.data.frame(joined.3, oblandact, by = "duns", all.x = TRUE)
joined.5 = merge.data.frame(joined.4, agency, by = "duns", all.x = TRUE)


####selecting and creating needed fields  

#### variables I need: Firm Age, Location, Ownership, years in SAM, survival status, biz_size, 3 years, 5 years, 10 years 


dataset.2009 = joined.5 %>% 
  mutate(firm.age = year(registrationDate) - year(businessStartDate)) %>% 
  mutate(location = ifelse(country == "USA", "1", "0")) %>% 
  mutate(ownership.woman = ifelse(womenownedflag == 1, "1", 0)) %>% 
  mutate(ownership.veteran = ifelse(veteranownedflag == 1, "1",0)) %>% 
  mutate(ownership.minority = ifelse(aiobflag == 1 |minorityownedbusinessflag == 1 | 
                                       apaobflag == 1 | 
                                       baobflag == 1 | naobflag == 1 | saaobflag == 1 | 
                                       haobflag == 1 | isnativehawaiianownedorganizationorfirm == 1 | 
                                       isotherminorityowned == 1 | istriballyownedfirm == 1 | 
                                       isalaskannativeownedcorporationorfirm == 1 | 
                                       other_minority_owned_business == 1, "1", 0)) %>% 
  mutate(ownership.foreign = ifelse(isforeignownedandlocated == 1, "1", 0)) %>% 
  mutate(years.survived = months.survived/12) %>% 
  mutate(survival.status = ifelse(year(lastsigneddate) >= 2010, "1", "0")) %>%
  mutate(three.year = years.survived>=3, "YES","NO") %>% 
  mutate(five.year = years.survived>=5, "YES","NO") %>% 
  mutate(ten.year = years.survived>=10, "YES","NO") %>%   ##only works since start at same time
  mutate(biz_size = ifelse(contractingofficerbusinesssizedetermination == "O", 1, 0)) %>% 
  select(duns, biz_size, NAICS2, ServicesCategory, location, ownership.woman, 
         ownership.veteran, ownership.minority, ownership.foreign, contract.actions,
         obligated.amt, years.survived, lastsigneddate, firm.age, three.year, five.year,ten.year, lastsigneddate, survival.status, DEPARTMENT_NAME, AGENCY_NAME, registrationDate) %>% 
  filter(NAICS2 != "NU")

#2010 ####
year.edit = final_joined_DOD %>% 
  filter(year(registrationDate) == 2010) %>% 
  group_by(duns) %>% 
  filter(year(signeddate) <= year(registrationDate) + 10) %>% 
  mutate(lastsigneddate = max(signeddate)) %>% 
  mutate(months.survived = ((year(lastsigneddate) - year(registrationDate)) * 12) + month(lastsigneddate) - month(registrationDate))

#filter(contractingofficerbusinesssizedetermination == "S")
year.edit.unique = year.edit[!duplicated(year.edit[,c('duns')]),]


##joined the above sections together by duns and selected only the columns needed

NAICS = as.data.frame(NAICS.unique.column)
year = as.data.frame(year.edit.unique)

joined.2 = merge.data.frame(NAICS, year, by = "duns")  

joined.25 = joined.2 %>%
  select(duns, NAICS2, age_at_start, months.survived, lastsigneddate, status, registrationDate, 
         businessStartDate, country, womenownedflag, veteranownedflag, aiobflag, naobflag,
         minorityownedbusinessflag, apaobflag, baobflag, baobflag, saaobflag, haobflag, 
         isnativehawaiianownedorganizationorfirm, isotherminorityowned, istriballyownedfirm, 
         isalaskannativeownedcorporationorfirm, other_minority_owned_business, 
         isforeignownedandlocated, expirationDate, contractingofficerbusinesssizedetermination)

###cross reference agency and department codes with a list of names and used one of our 
##datasets in SQL to identify the PSC codes -- used a similar method to determine agency and PSC
##for each firm as was used to determine the NAICS code

joined.3 = merge.data.frame(joined.25, PSC, by = "duns", all.x = TRUE)
joined.4 = merge.data.frame(joined.3, oblandact, by = "duns", all.x = TRUE)
joined.5 = merge.data.frame(joined.4, agency, by = "duns", all.x = TRUE)


####selecting and creating needed fields  

#### variables I need: Firm Age, Location, Ownership, years in SAM, survival status, biz_size, 3 years, 5 years, 10 years 


dataset.2010 = joined.5 %>% 
  mutate(firm.age = year(registrationDate) - year(businessStartDate)) %>% 
  mutate(location = ifelse(country == "USA", "1", "0")) %>% 
  mutate(ownership.woman = ifelse(womenownedflag == 1, "1", 0)) %>% 
  mutate(ownership.veteran = ifelse(veteranownedflag == 1, "1",0)) %>% 
  mutate(ownership.minority = ifelse(aiobflag == 1 |minorityownedbusinessflag == 1 | 
                                       apaobflag == 1 | 
                                       baobflag == 1 | naobflag == 1 | saaobflag == 1 | 
                                       haobflag == 1 | isnativehawaiianownedorganizationorfirm == 1 | 
                                       isotherminorityowned == 1 | istriballyownedfirm == 1 | 
                                       isalaskannativeownedcorporationorfirm == 1 | 
                                       other_minority_owned_business == 1, "1", 0)) %>% 
  mutate(ownership.foreign = ifelse(isforeignownedandlocated == 1, "1", 0)) %>% 
  mutate(years.survived = months.survived/12) %>% 
  mutate(survival.status = ifelse(year(lastsigneddate) >= 2010, "1", "0")) %>%
  mutate(three.year = years.survived>=3, "YES","NO") %>% 
  mutate(five.year = years.survived>=5, "YES","NO") %>% 
  mutate(ten.year = years.survived>=10, "YES","NO") %>%   ##only works since start at same time
  mutate(biz_size = ifelse(contractingofficerbusinesssizedetermination == "O", 1, 0)) %>% 
  select(duns, biz_size, NAICS2, ServicesCategory, location, ownership.woman, 
         ownership.veteran, ownership.minority, ownership.foreign, contract.actions,
         obligated.amt, years.survived, lastsigneddate, firm.age, three.year, five.year,ten.year, lastsigneddate, survival.status, DEPARTMENT_NAME, AGENCY_NAME, registrationDate) %>% 
  filter(NAICS2 != "NU")

#2011 ####
year.edit = final_joined_DOD %>% 
  filter(year(registrationDate) == 2011) %>% 
  group_by(duns) %>% 
  filter(year(signeddate) <= year(registrationDate) + 10) %>% 
  mutate(lastsigneddate = max(signeddate)) %>% 
  mutate(months.survived = ((year(lastsigneddate) - year(registrationDate)) * 12) + month(lastsigneddate) - month(registrationDate))

#filter(contractingofficerbusinesssizedetermination == "S")
year.edit.unique = year.edit[!duplicated(year.edit[,c('duns')]),]


##joined the above sections together by duns and selected only the columns needed

NAICS = as.data.frame(NAICS.unique.column)
year = as.data.frame(year.edit.unique)

joined.2 = merge.data.frame(NAICS, year, by = "duns")  

joined.25 = joined.2 %>%
  select(duns, NAICS2, age_at_start, months.survived, lastsigneddate, status, registrationDate, 
         businessStartDate, country, womenownedflag, veteranownedflag, aiobflag, naobflag,
         minorityownedbusinessflag, apaobflag, baobflag, baobflag, saaobflag, haobflag, 
         isnativehawaiianownedorganizationorfirm, isotherminorityowned, istriballyownedfirm, 
         isalaskannativeownedcorporationorfirm, other_minority_owned_business, 
         isforeignownedandlocated, expirationDate, contractingofficerbusinesssizedetermination)

###cross reference agency and department codes with a list of names and used one of our 
##datasets in SQL to identify the PSC codes -- used a similar method to determine agency and PSC
##for each firm as was used to determine the NAICS code

joined.3 = merge.data.frame(joined.25, PSC, by = "duns", all.x = TRUE)
joined.4 = merge.data.frame(joined.3, oblandact, by = "duns", all.x = TRUE)
joined.5 = merge.data.frame(joined.4, agency, by = "duns", all.x = TRUE)


####selecting and creating needed fields  

#### variables I need: Firm Age, Location, Ownership, years in SAM, survival status, biz_size, 3 years, 5 years, 10 years 


dataset.2011 = joined.5 %>% 
  mutate(firm.age = year(registrationDate) - year(businessStartDate)) %>% 
  mutate(location = ifelse(country == "USA", "1", "0")) %>% 
  mutate(ownership.woman = ifelse(womenownedflag == 1, "1", 0)) %>% 
  mutate(ownership.veteran = ifelse(veteranownedflag == 1, "1",0)) %>% 
  mutate(ownership.minority = ifelse(aiobflag == 1 |minorityownedbusinessflag == 1 | 
                                       apaobflag == 1 | 
                                       baobflag == 1 | naobflag == 1 | saaobflag == 1 | 
                                       haobflag == 1 | isnativehawaiianownedorganizationorfirm == 1 | 
                                       isotherminorityowned == 1 | istriballyownedfirm == 1 | 
                                       isalaskannativeownedcorporationorfirm == 1 | 
                                       other_minority_owned_business == 1, "1", 0)) %>% 
  mutate(ownership.foreign = ifelse(isforeignownedandlocated == 1, "1", 0)) %>% 
  mutate(years.survived = months.survived/12) %>% 
  mutate(survival.status = ifelse(year(lastsigneddate) >= 2010, "1", "0")) %>%
  mutate(three.year = years.survived>=3, "YES","NO") %>% 
  mutate(five.year = years.survived>=5, "YES","NO") %>% 
  mutate(ten.year = years.survived>=10, "YES","NO") %>%   ##only works since start at same time
  mutate(biz_size = ifelse(contractingofficerbusinesssizedetermination == "O", 1, 0)) %>% 
  select(duns, biz_size, NAICS2, ServicesCategory, location, ownership.woman, 
         ownership.veteran, ownership.minority, ownership.foreign, contract.actions,
         obligated.amt, years.survived, lastsigneddate, firm.age, three.year, five.year,ten.year, lastsigneddate, survival.status, DEPARTMENT_NAME, AGENCY_NAME, registrationDate) %>% 
  filter(NAICS2 != "NU")

#2012 ####
year.edit = final_joined_DOD %>% 
  filter(year(registrationDate) == 2012) %>% 
  group_by(duns) %>% 
  filter(year(signeddate) <= year(registrationDate) + 10) %>% 
  mutate(lastsigneddate = max(signeddate)) %>% 
  mutate(months.survived = ((year(lastsigneddate) - year(registrationDate)) * 12) + month(lastsigneddate) - month(registrationDate))

#filter(contractingofficerbusinesssizedetermination == "S")
year.edit.unique = year.edit[!duplicated(year.edit[,c('duns')]),]


##joined the above sections together by duns and selected only the columns needed

NAICS = as.data.frame(NAICS.unique.column)
year = as.data.frame(year.edit.unique)

joined.2 = merge.data.frame(NAICS, year, by = "duns")  

joined.25 = joined.2 %>%
  select(duns, NAICS2, age_at_start, months.survived, lastsigneddate, status, registrationDate, 
         businessStartDate, country, womenownedflag, veteranownedflag, aiobflag, naobflag,
         minorityownedbusinessflag, apaobflag, baobflag, baobflag, saaobflag, haobflag, 
         isnativehawaiianownedorganizationorfirm, isotherminorityowned, istriballyownedfirm, 
         isalaskannativeownedcorporationorfirm, other_minority_owned_business, 
         isforeignownedandlocated, expirationDate, contractingofficerbusinesssizedetermination)

###cross reference agency and department codes with a list of names and used one of our 
##datasets in SQL to identify the PSC codes -- used a similar method to determine agency and PSC
##for each firm as was used to determine the NAICS code

joined.3 = merge.data.frame(joined.25, PSC, by = "duns", all.x = TRUE)
joined.4 = merge.data.frame(joined.3, oblandact, by = "duns", all.x = TRUE)
joined.5 = merge.data.frame(joined.4, agency, by = "duns", all.x = TRUE)


####selecting and creating needed fields  

#### variables I need: Firm Age, Location, Ownership, years in SAM, survival status, biz_size, 3 years, 5 years, 10 years 


dataset.2012 = joined.5 %>% 
  mutate(firm.age = year(registrationDate) - year(businessStartDate)) %>% 
  mutate(location = ifelse(country == "USA", "1", "0")) %>% 
  mutate(ownership.woman = ifelse(womenownedflag == 1, "1", 0)) %>% 
  mutate(ownership.veteran = ifelse(veteranownedflag == 1, "1",0)) %>% 
  mutate(ownership.minority = ifelse(aiobflag == 1 |minorityownedbusinessflag == 1 | 
                                       apaobflag == 1 | 
                                       baobflag == 1 | naobflag == 1 | saaobflag == 1 | 
                                       haobflag == 1 | isnativehawaiianownedorganizationorfirm == 1 | 
                                       isotherminorityowned == 1 | istriballyownedfirm == 1 | 
                                       isalaskannativeownedcorporationorfirm == 1 | 
                                       other_minority_owned_business == 1, "1", 0)) %>% 
  mutate(ownership.foreign = ifelse(isforeignownedandlocated == 1, "1", 0)) %>% 
  mutate(years.survived = months.survived/12) %>% 
  mutate(survival.status = ifelse(year(lastsigneddate) >= 2010, "1", "0")) %>%
  mutate(three.year = years.survived>=3, "YES","NO") %>% 
  mutate(five.year = years.survived>=5, "YES","NO") %>% 
  mutate(ten.year = years.survived>=10, "YES","NO") %>%   ##only works since start at same time
  mutate(biz_size = ifelse(contractingofficerbusinesssizedetermination == "O", 1, 0)) %>% 
  select(duns, biz_size, NAICS2, ServicesCategory, location, ownership.woman, 
         ownership.veteran, ownership.minority, ownership.foreign, contract.actions,
         obligated.amt, years.survived, lastsigneddate, firm.age, three.year, five.year,ten.year, lastsigneddate, survival.status, DEPARTMENT_NAME, AGENCY_NAME, registrationDate) %>% 
  filter(NAICS2 != "NU")

#2013 ####
year.edit = final_joined_DOD %>% 
  filter(year(registrationDate) == 2013) %>% 
  group_by(duns) %>% 
  filter(year(signeddate) <= year(registrationDate) + 10) %>% 
  mutate(lastsigneddate = max(signeddate)) %>% 
  mutate(months.survived = ((year(lastsigneddate) - year(registrationDate)) * 12) + (month(lastsigneddate) - month(registrationDate)))

#filter(contractingofficerbusinesssizedetermination == "S")
year.edit.unique = year.edit[!duplicated(year.edit[,c('duns')]),]


##joined the above sections together by duns and selected only the columns needed

NAICS = as.data.frame(NAICS.unique.column)
year = as.data.frame(year.edit.unique)

joined.2 = merge.data.frame(NAICS, year, by = "duns")  

joined.25 = joined.2 %>%
  select(duns, NAICS2, age_at_start, months.survived, lastsigneddate, status, registrationDate, 
         businessStartDate, country, womenownedflag, veteranownedflag, aiobflag, naobflag,
         minorityownedbusinessflag, apaobflag, baobflag, baobflag, saaobflag, haobflag, 
         isnativehawaiianownedorganizationorfirm, isotherminorityowned, istriballyownedfirm, 
         isalaskannativeownedcorporationorfirm, other_minority_owned_business, 
         isforeignownedandlocated, expirationDate, contractingofficerbusinesssizedetermination)

###cross reference agency and department codes with a list of names and used one of our 
##datasets in SQL to identify the PSC codes -- used a similar method to determine agency and PSC
##for each firm as was used to determine the NAICS code

joined.3 = merge.data.frame(joined.25, PSC, by = "duns", all.x = TRUE)
joined.4 = merge.data.frame(joined.3, oblandact, by = "duns", all.x = TRUE)
joined.5 = merge.data.frame(joined.4, agency, by = "duns", all.x = TRUE)


####selecting and creating needed fields  

#### variables I need: Firm Age, Location, Ownership, years in SAM, survival status, biz_size, 3 years, 5 years, 10 years 


dataset.2013 = joined.5 %>% 
  mutate(firm.age = year(registrationDate) - year(businessStartDate)) %>% 
  mutate(location = ifelse(country == "USA", "1", "0")) %>% 
  mutate(ownership.woman = ifelse(womenownedflag == 1, "1", 0)) %>% 
  mutate(ownership.veteran = ifelse(veteranownedflag == 1, "1",0)) %>% 
  mutate(ownership.minority = ifelse(aiobflag == 1 |minorityownedbusinessflag == 1 | 
                                       apaobflag == 1 | 
                                       baobflag == 1 | naobflag == 1 | saaobflag == 1 | 
                                       haobflag == 1 | isnativehawaiianownedorganizationorfirm == 1 | 
                                       isotherminorityowned == 1 | istriballyownedfirm == 1 | 
                                       isalaskannativeownedcorporationorfirm == 1 | 
                                       other_minority_owned_business == 1, "1", 0)) %>% 
  mutate(ownership.foreign = ifelse(isforeignownedandlocated == 1, "1", 0)) %>% 
  mutate(years.survived = months.survived/12) %>% 
  mutate(survival.status = ifelse(year(lastsigneddate) >= 2010, "1", "0")) %>%
  mutate(three.year = years.survived>=3, "YES","NO") %>% 
  mutate(five.year = years.survived>=5, "YES","NO") %>% 
  mutate(ten.year = years.survived>=10, "YES","NO") %>%   ##only works since start at same time
  mutate(biz_size = ifelse(contractingofficerbusinesssizedetermination == "O", 1, 0)) %>% 
  select(duns, biz_size, NAICS2, ServicesCategory, location, ownership.woman, 
         ownership.veteran, ownership.minority, ownership.foreign, contract.actions,
         obligated.amt, years.survived, lastsigneddate, firm.age, three.year, five.year,ten.year, lastsigneddate, survival.status, DEPARTMENT_NAME, AGENCY_NAME, registrationDate) %>% 
  filter(NAICS2 != "NU")

#2014 ####
year.edit = final_joined_DOD %>% 
  filter(year(registrationDate) == 2014) %>% 
  group_by(duns) %>% 
  filter(year(signeddate) <= year(registrationDate) + 10) %>% 
  mutate(lastsigneddate = max(signeddate)) %>% 
  mutate(months.survived = ((year(lastsigneddate) - year(registrationDate)) * 12) + month(lastsigneddate) - month(registrationDate))

#filter(contractingofficerbusinesssizedetermination == "S")
year.edit.unique = year.edit[!duplicated(year.edit[,c('duns')]),]


##joined the above sections together by duns and selected only the columns needed

NAICS = as.data.frame(NAICS.unique.column)
year = as.data.frame(year.edit.unique)

joined.2 = merge.data.frame(NAICS, year, by = "duns")  

joined.25 = joined.2 %>%
  select(duns, NAICS2, age_at_start, months.survived, lastsigneddate, status, registrationDate, 
         businessStartDate, country, womenownedflag, veteranownedflag, aiobflag, naobflag,
         minorityownedbusinessflag, apaobflag, baobflag, baobflag, saaobflag, haobflag, 
         isnativehawaiianownedorganizationorfirm, isotherminorityowned, istriballyownedfirm, 
         isalaskannativeownedcorporationorfirm, other_minority_owned_business, 
         isforeignownedandlocated, expirationDate, contractingofficerbusinesssizedetermination)

###cross reference agency and department codes with a list of names and used one of our 
##datasets in SQL to identify the PSC codes -- used a similar method to determine agency and PSC
##for each firm as was used to determine the NAICS code

joined.3 = merge.data.frame(joined.25, PSC, by = "duns", all.x = TRUE)
joined.4 = merge.data.frame(joined.3, oblandact, by = "duns", all.x = TRUE)
joined.5 = merge.data.frame(joined.4, agency, by = "duns", all.x = TRUE)


####selecting and creating needed fields  

#### variables I need: Firm Age, Location, Ownership, years in SAM, survival status, biz_size, 3 years, 5 years, 10 years 


dataset.2014 = joined.5 %>% 
  mutate(firm.age = year(registrationDate) - year(businessStartDate)) %>% 
  mutate(location = ifelse(country == "USA", "1", "0")) %>% 
  mutate(ownership.woman = ifelse(womenownedflag == 1, "1", 0)) %>% 
  mutate(ownership.veteran = ifelse(veteranownedflag == 1, "1",0)) %>% 
  mutate(ownership.minority = ifelse(aiobflag == 1 |minorityownedbusinessflag == 1 | 
                                       apaobflag == 1 | 
                                       baobflag == 1 | naobflag == 1 | saaobflag == 1 | 
                                       haobflag == 1 | isnativehawaiianownedorganizationorfirm == 1 | 
                                       isotherminorityowned == 1 | istriballyownedfirm == 1 | 
                                       isalaskannativeownedcorporationorfirm == 1 | 
                                       other_minority_owned_business == 1, "1", 0)) %>% 
  mutate(ownership.foreign = ifelse(isforeignownedandlocated == 1, "1", 0)) %>% 
  mutate(years.survived = months.survived/12) %>% 
  mutate(survival.status = ifelse(year(lastsigneddate) >= 2010, "1", "0")) %>%
  mutate(three.year = years.survived>=3, "YES","NO") %>% 
  mutate(five.year = years.survived>=5, "YES","NO") %>% 
  mutate(ten.year = years.survived>=10, "YES","NO") %>%   ##only works since start at same time
  mutate(biz_size = ifelse(contractingofficerbusinesssizedetermination == "O", 1, 0)) %>% 
  select(duns, biz_size, NAICS2, ServicesCategory, location, ownership.woman, 
         ownership.veteran, ownership.minority, ownership.foreign, contract.actions,
         obligated.amt, years.survived, lastsigneddate, firm.age, three.year, five.year,ten.year, lastsigneddate, survival.status, DEPARTMENT_NAME, AGENCY_NAME, registrationDate) %>% 
  filter(NAICS2 != "NU")

#2015 ####
year.edit = final_joined_DOD %>% 
  filter(year(registrationDate) == 2015) %>% 
  group_by(duns) %>% 
  filter(year(signeddate) <= year(registrationDate) + 10) %>% 
  mutate(lastsigneddate = max(signeddate)) %>% 
  mutate(months.survived = ((year(lastsigneddate) - year(registrationDate)) * 12) + month(lastsigneddate) - month(registrationDate))

#filter(contractingofficerbusinesssizedetermination == "S")
year.edit.unique = year.edit[!duplicated(year.edit[,c('duns')]),]


##joined the above sections together by duns and selected only the columns needed

NAICS = as.data.frame(NAICS.unique.column)
year = as.data.frame(year.edit.unique)

joined.2 = merge.data.frame(NAICS, year, by = "duns")  

joined.25 = joined.2 %>%
  select(duns, NAICS2, age_at_start, months.survived, lastsigneddate, status, registrationDate, 
         businessStartDate, country, womenownedflag, veteranownedflag, aiobflag, naobflag,
         minorityownedbusinessflag, apaobflag, baobflag, baobflag, saaobflag, haobflag, 
         isnativehawaiianownedorganizationorfirm, isotherminorityowned, istriballyownedfirm, 
         isalaskannativeownedcorporationorfirm, other_minority_owned_business, 
         isforeignownedandlocated, expirationDate, contractingofficerbusinesssizedetermination)

###cross reference agency and department codes with a list of names and used one of our 
##datasets in SQL to identify the PSC codes -- used a similar method to determine agency and PSC
##for each firm as was used to determine the NAICS code

joined.3 = merge.data.frame(joined.25, PSC, by = "duns", all.x = TRUE)
joined.4 = merge.data.frame(joined.3, oblandact, by = "duns", all.x = TRUE)
joined.5 = merge.data.frame(joined.4, agency, by = "duns", all.x = TRUE)


####selecting and creating needed fields  

#### variables I need: Firm Age, Location, Ownership, years in SAM, survival status, biz_size, 3 years, 5 years, 10 years 


dataset.2015 = joined.5 %>% 
  mutate(firm.age = year(registrationDate) - year(businessStartDate)) %>% 
  mutate(location = ifelse(country == "USA", "1", "0")) %>% 
  mutate(ownership.woman = ifelse(womenownedflag == 1, "1", 0)) %>% 
  mutate(ownership.veteran = ifelse(veteranownedflag == 1, "1",0)) %>% 
  mutate(ownership.minority = ifelse(aiobflag == 1 |minorityownedbusinessflag == 1 | 
                                       apaobflag == 1 | 
                                       baobflag == 1 | naobflag == 1 | saaobflag == 1 | 
                                       haobflag == 1 | isnativehawaiianownedorganizationorfirm == 1 | 
                                       isotherminorityowned == 1 | istriballyownedfirm == 1 | 
                                       isalaskannativeownedcorporationorfirm == 1 | 
                                       other_minority_owned_business == 1, "1", 0)) %>% 
  mutate(ownership.foreign = ifelse(isforeignownedandlocated == 1, "1", 0)) %>% 
  mutate(years.survived = months.survived/12) %>% 
  mutate(survival.status = ifelse(year(lastsigneddate) >= 2010, "1", "0")) %>%
  mutate(three.year = years.survived>=3, "YES","NO") %>% 
  mutate(five.year = years.survived>=5, "YES","NO") %>% 
  mutate(ten.year = years.survived>=10, "YES","NO") %>%   ##only works since start at same time
  mutate(biz_size = ifelse(contractingofficerbusinesssizedetermination == "O", 1, 0)) %>% 
  select(duns, biz_size, NAICS2, ServicesCategory, location, ownership.woman, 
         ownership.veteran, ownership.minority, ownership.foreign, contract.actions,
         obligated.amt, years.survived, lastsigneddate, firm.age, three.year, five.year,ten.year, lastsigneddate, survival.status, DEPARTMENT_NAME, AGENCY_NAME, registrationDate) %>% 
  filter(NAICS2 != "NU")

#2016 ####
year.edit = final_joined_DOD %>% 
  filter(year(registrationDate) == 2016) %>% 
  group_by(duns) %>% 
  filter(year(signeddate) <= year(registrationDate) + 10) %>% 
  mutate(lastsigneddate = max(signeddate)) %>% 
  mutate(months.survived = ((year(lastsigneddate) - year(registrationDate)) * 12) + month(lastsigneddate) - month(registrationDate))

#filter(contractingofficerbusinesssizedetermination == "S")
year.edit.unique = year.edit[!duplicated(year.edit[,c('duns')]),]


##joined the above sections together by duns and selected only the columns needed

NAICS = as.data.frame(NAICS.unique.column)
year = as.data.frame(year.edit.unique)

joined.2 = merge.data.frame(NAICS, year, by = "duns")  

joined.25 = joined.2 %>%
  select(duns, NAICS2, age_at_start, months.survived, lastsigneddate, status, registrationDate, 
         businessStartDate, country, womenownedflag, veteranownedflag, aiobflag, naobflag,
         minorityownedbusinessflag, apaobflag, baobflag, baobflag, saaobflag, haobflag, 
         isnativehawaiianownedorganizationorfirm, isotherminorityowned, istriballyownedfirm, 
         isalaskannativeownedcorporationorfirm, other_minority_owned_business, 
         isforeignownedandlocated, expirationDate, contractingofficerbusinesssizedetermination)

###cross reference agency and department codes with a list of names and used one of our 
##datasets in SQL to identify the PSC codes -- used a similar method to determine agency and PSC
##for each firm as was used to determine the NAICS code

joined.3 = merge.data.frame(joined.25, PSC, by = "duns", all.x = TRUE)
joined.4 = merge.data.frame(joined.3, oblandact, by = "duns", all.x = TRUE)
joined.5 = merge.data.frame(joined.4, agency, by = "duns", all.x = TRUE)


####selecting and creating needed fields  

#### variables I need: Firm Age, Location, Ownership, years in SAM, survival status, biz_size, 3 years, 5 years, 10 years 


dataset.2016 = joined.5 %>% 
  mutate(firm.age = year(registrationDate) - year(businessStartDate)) %>% 
  mutate(location = ifelse(country == "USA", "1", "0")) %>% 
  mutate(ownership.woman = ifelse(womenownedflag == 1, "1", 0)) %>% 
  mutate(ownership.veteran = ifelse(veteranownedflag == 1, "1",0)) %>% 
  mutate(ownership.minority = ifelse(aiobflag == 1 |minorityownedbusinessflag == 1 | 
                                       apaobflag == 1 | 
                                       baobflag == 1 | naobflag == 1 | saaobflag == 1 | 
                                       haobflag == 1 | isnativehawaiianownedorganizationorfirm == 1 | 
                                       isotherminorityowned == 1 | istriballyownedfirm == 1 | 
                                       isalaskannativeownedcorporationorfirm == 1 | 
                                       other_minority_owned_business == 1, "1", 0)) %>% 
  mutate(ownership.foreign = ifelse(isforeignownedandlocated == 1, "1", 0)) %>% 
  mutate(years.survived = months.survived/12) %>% 
  mutate(survival.status = ifelse(year(lastsigneddate) >= 2010, "1", "0")) %>%
  mutate(three.year = years.survived>=3, "YES","NO") %>% 
  mutate(five.year = years.survived>=5, "YES","NO") %>% 
  mutate(ten.year = years.survived>=10, "YES","NO") %>%   ##only works since start at same time
  mutate(biz_size = ifelse(contractingofficerbusinesssizedetermination == "O", 1, 0)) %>% 
  select(duns, biz_size, NAICS2, ServicesCategory, location, ownership.woman, 
         ownership.veteran, ownership.minority, ownership.foreign, contract.actions,
         obligated.amt, years.survived, lastsigneddate, firm.age, three.year, five.year,ten.year, lastsigneddate, survival.status, DEPARTMENT_NAME, AGENCY_NAME, registrationDate) %>% 
  filter(NAICS2 != "NU")

#### bind each created dataset together####

all.dataset_DOD.SD <- rbind(dataset.2001, dataset.2002, dataset.2003, 
                        dataset.2004, dataset.2005, dataset.2006, 
                        dataset.2007, dataset.2008, dataset.2009, 
                        dataset.2010, dataset.2011, dataset.2012, 
                        dataset.2013, dataset.2014, dataset.2015,
                        dataset.2016)



write.csv(all.dataset_DOD.SD, "Panel Data reg2001-2016, SD2010-2025 DOD- nop, 10plus1 year view.csv")
