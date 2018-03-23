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


final <- read_csv("allSAMData FPDS.csv")

final_joined = final[!duplicated(final),]

final_joined <- final_joined %>% 
  mutate(age_at_start = year(registrationDate) - year(businessStartDate)) %>% 
  mutate(months_in_SAM = ((year(expirationDate) - year(registrationDate)) * 12) + month(expirationDate) - month(registrationDate)) %>% 
  rename(country = `samAddress countryCode`)


##2001
###########filtering dataset

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


#### selected only small businesses that registered in the year 2001
year.edit = final_joined %>% 
  mutate(regyear = year(registrationDate)) %>%
  arrange(regyear) %>% 
  filter(regyear == 2001) 


#filter(contractingofficerbusinesssizedetermination == "S")
year.edit.unique = year.edit[!duplicated(year.edit[,c('duns')]),]

#### filtered out firms with parent duns of the parent duns did not register in 2001

parent.edit = year.edit %>%
  mutate (haveparent = ifelse(duns == parentdunsnumber|parentdunsnumber == "NULL", "NO", "YES")) %>% 
  filter(haveparent == "NO") %>% 
  #filter(contractingofficerbusinesssizedetermination != ":") %>% 
  #filter(contractingofficerbusinesssizedetermination != "NULL") %>% 
  select(duns, haveparent, fiscal_year, signeddate, parentdunsnumber, vendorname) %>% 
  arrange(fiscal_year) 

parent.unique = parent.edit[!duplicated(parent.edit[,c('duns')]),] 

##joined the above sections together by duns and selected only the columns needed

NAICS = as.data.frame(NAICS.unique.column)
year = as.data.frame(year.edit.unique)
parent = as.data.frame(parent.unique)

joined.1 = merge.data.frame(parent, NAICS, by = "duns")
joined.2 = merge.data.frame(joined.1, year, by = "duns")

###joined.2 = merge.data.frame(NAICS, year, by = "duns")  ##no parent

joined.25 = joined.2 %>% 
  select(duns, NAICS2, age_at_start, months_in_SAM, status,regyear, registrationDate, 
         businessStartDate, country, womenownedflag, veteranownedflag, aiobflag, naobflag,
         minorityownedbusinessflag, apaobflag, baobflag, baobflag, saaobflag, haobflag, 
         isnativehawaiianownedorganizationorfirm, isotherminorityowned, istriballyownedfirm, 
         isalaskannativeownedcorporationorfirm, other_minority_owned_business, 
         isforeignownedandlocated, expirationDate, contractingofficerbusinesssizedetermination)

###cross reference agency and department codes with a list of names and used one of our 
##datasets in SQL to identify the PSC codes -- used a similar method to determine agency and PSC
##for each firm as was used to determine the NAICS code


psc_names <- read_csv("PSC.csv")
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
  mutate(years.in.SAM = months_in_SAM/12) %>% 
  mutate(survival.status = ifelse(year(expirationDate) > 2011, "1", "0")) %>%
  mutate(three.year = years.in.SAM>=3, "YES","NO") %>% 
  mutate(five.year = years.in.SAM>=5, "YES","NO") %>% 
  mutate(ten.year = years.in.SAM>=10, "YES","NO") %>%   ##only works since start at same time
  mutate(biz_size = ifelse(contractingofficerbusinesssizedetermination == "O", 1, 0)) %>% 
  select(duns, biz_size, years.in.SAM, NAICS2, ServicesCategory, location, ownership.woman, 
         ownership.veteran, ownership.minority, ownership.foreign, contract.actions,
         obligated.amt, firm.age, three.year, five.year,ten.year, survival.status, DEPARTMENT_NAME, AGENCY_NAME) %>% 
  filter(NAICS2 != "NU")

##2002

year.edit = final_joined %>% 
  mutate(regyear = year(registrationDate)) %>%
  arrange(regyear) %>% 
  filter(regyear == 2002) 


#filter(contractingofficerbusinesssizedetermination == "S")
year.edit.unique = year.edit[!duplicated(year.edit[,c('duns')]),]

#### filtered out firms with parent duns of the parent duns did not register in 2002

parent.edit = year.edit %>%
  mutate (haveparent = ifelse(duns == parentdunsnumber|parentdunsnumber == "NULL", "NO", "YES")) %>% 
  filter(haveparent == "NO") %>% 
  #filter(contractingofficerbusinesssizedetermination != ":") %>% 
  #filter(contractingofficerbusinesssizedetermination != "NULL") %>% 
  select(duns, haveparent, fiscal_year, signeddate, parentdunsnumber, vendorname) %>% 
  arrange(fiscal_year) 

parent.unique = parent.edit[!duplicated(parent.edit[,c('duns')]),] 


NAICS = as.data.frame(NAICS.unique.column)
year = as.data.frame(year.edit.unique)
parent = as.data.frame(parent.unique)

joined.1 = merge.data.frame(parent, NAICS, by = "duns")
joined.2 = merge.data.frame(joined.1, year, by = "duns")

##joined.2 = merge.data.frame(NAICS, year, by = "duns")  ##no parent

joined.25 = joined.2 %>% 
  select(duns, NAICS2, age_at_start, months_in_SAM, status,regyear, registrationDate, 
         businessStartDate, country, womenownedflag, veteranownedflag, aiobflag, naobflag,
         minorityownedbusinessflag, apaobflag, baobflag, baobflag, saaobflag, haobflag, 
         isnativehawaiianownedorganizationorfirm, isotherminorityowned, istriballyownedfirm, 
         isalaskannativeownedcorporationorfirm, other_minority_owned_business, 
         isforeignownedandlocated, expirationDate, contractingofficerbusinesssizedetermination)

joined.3 = merge.data.frame(joined.25, PSC, by = "duns", all.x = TRUE)
joined.4 = merge.data.frame(joined.3, oblandact, by = "duns", all.x = TRUE)
joined.5 = merge.data.frame(joined.4, agency, by = "duns", all.x = TRUE)


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
  mutate(years.in.SAM = months_in_SAM/12) %>% 
  mutate(survival.status = ifelse(year(expirationDate) > 2011, "1", "0")) %>%
  mutate(three.year = years.in.SAM>=3, "YES","NO") %>% 
  mutate(five.year = years.in.SAM>=5, "YES","NO") %>% 
  mutate(ten.year = years.in.SAM>=10, "YES","NO") %>%   ##only works since start at same time
  mutate(biz_size = ifelse(contractingofficerbusinesssizedetermination == "O", 1, 0)) %>% 
  select(duns, biz_size, years.in.SAM, NAICS2, ServicesCategory, location, ownership.woman, 
         ownership.veteran, ownership.minority, ownership.foreign, contract.actions,
         obligated.amt, firm.age, three.year, five.year,ten.year, survival.status, DEPARTMENT_NAME, AGENCY_NAME) %>% 
  filter(NAICS2 != "NU")

##2003

year.edit = final_joined %>% 
  mutate(regyear = year(registrationDate)) %>%
  arrange(regyear) %>% 
  filter(regyear == 2003) 


#filter(contractingofficerbusinesssizedetermination == "S")
year.edit.unique = year.edit[!duplicated(year.edit[,c('duns')]),]

#### filtered out firms with parent duns of the parent duns did not register in 2003

parent.edit = year.edit %>%
  mutate (haveparent = ifelse(duns == parentdunsnumber|parentdunsnumber == "NULL", "NO", "YES")) %>% 
  filter(haveparent == "NO") %>% 
  #filter(contractingofficerbusinesssizedetermination != ":") %>% 
  #filter(contractingofficerbusinesssizedetermination != "NULL") %>% 
  select(duns, haveparent, fiscal_year, signeddate, parentdunsnumber, vendorname) %>% 
  arrange(fiscal_year) 

parent.unique = parent.edit[!duplicated(parent.edit[,c('duns')]),] 


NAICS = as.data.frame(NAICS.unique.column)
year = as.data.frame(year.edit.unique)
parent = as.data.frame(parent.unique)

joined.1 = merge.data.frame(parent, NAICS, by = "duns")
joined.2 = merge.data.frame(joined.1, year, by = "duns")

##joined.2 = merge.data.frame(NAICS, year, by = "duns")  ##no parent

joined.25 = joined.2 %>% 
  select(duns, NAICS2, age_at_start, months_in_SAM, status,regyear, registrationDate, 
         businessStartDate, country, womenownedflag, veteranownedflag, aiobflag, naobflag,
         minorityownedbusinessflag, apaobflag, baobflag, baobflag, saaobflag, haobflag, 
         isnativehawaiianownedorganizationorfirm, isotherminorityowned, istriballyownedfirm, 
         isalaskannativeownedcorporationorfirm, other_minority_owned_business, 
         isforeignownedandlocated, expirationDate, contractingofficerbusinesssizedetermination)

joined.3 = merge.data.frame(joined.25, PSC, by = "duns", all.x = TRUE)
joined.4 = merge.data.frame(joined.3, oblandact, by = "duns", all.x = TRUE)
joined.5 = merge.data.frame(joined.4, agency, by = "duns", all.x = TRUE)


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
  mutate(years.in.SAM = months_in_SAM/12) %>% 
  mutate(survival.status = ifelse(year(expirationDate) > 2011, "1", "0")) %>%
  mutate(three.year = years.in.SAM>=3, "YES","NO") %>% 
  mutate(five.year = years.in.SAM>=5, "YES","NO") %>% 
  mutate(ten.year = years.in.SAM>=10, "YES","NO") %>%   ##only works since start at same time
  mutate(biz_size = ifelse(contractingofficerbusinesssizedetermination == "O", 1, 0)) %>% 
  select(duns, biz_size, years.in.SAM, NAICS2, ServicesCategory, location, ownership.woman, 
         ownership.veteran, ownership.minority, ownership.foreign, contract.actions,
         obligated.amt, firm.age, three.year, five.year,ten.year, survival.status, DEPARTMENT_NAME, AGENCY_NAME) %>% 
  filter(NAICS2 != "NU")

#2004

year.edit = final_joined %>% 
  mutate(regyear = year(registrationDate)) %>%
  arrange(regyear) %>% 
  filter(regyear == 2004) 


#filter(contractingofficerbusinesssizedetermination == "S")
year.edit.unique = year.edit[!duplicated(year.edit[,c('duns')]),]

#### filtered out firms with parent duns of the parent duns did not register in 2004

parent.edit = year.edit %>%
  mutate (haveparent = ifelse(duns == parentdunsnumber|parentdunsnumber == "NULL", "NO", "YES")) %>% 
  filter(haveparent == "NO") %>% 
  #filter(contractingofficerbusinesssizedetermination != ":") %>% 
  #filter(contractingofficerbusinesssizedetermination != "NULL") %>% 
  select(duns, haveparent, fiscal_year, signeddate, parentdunsnumber, vendorname) %>% 
  arrange(fiscal_year) 

parent.unique = parent.edit[!duplicated(parent.edit[,c('duns')]),] 


NAICS = as.data.frame(NAICS.unique.column)
year = as.data.frame(year.edit.unique)
parent = as.data.frame(parent.unique)

joined.1 = merge.data.frame(parent, NAICS, by = "duns")
joined.2 = merge.data.frame(joined.1, year, by = "duns")

##joined.2 = merge.data.frame(NAICS, year, by = "duns")  ##no parent

joined.25 = joined.2 %>% 
  select(duns, NAICS2, age_at_start, months_in_SAM, status,regyear, registrationDate, 
         businessStartDate, country, womenownedflag, veteranownedflag, aiobflag, naobflag,
         minorityownedbusinessflag, apaobflag, baobflag, baobflag, saaobflag, haobflag, 
         isnativehawaiianownedorganizationorfirm, isotherminorityowned, istriballyownedfirm, 
         isalaskannativeownedcorporationorfirm, other_minority_owned_business, 
         isforeignownedandlocated, expirationDate, contractingofficerbusinesssizedetermination)

joined.3 = merge.data.frame(joined.25, PSC, by = "duns", all.x = TRUE)
joined.4 = merge.data.frame(joined.3, oblandact, by = "duns", all.x = TRUE)
joined.5 = merge.data.frame(joined.4, agency, by = "duns", all.x = TRUE)


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
  mutate(years.in.SAM = months_in_SAM/12) %>% 
  mutate(survival.status = ifelse(year(expirationDate) > 2011, "1", "0")) %>%
  mutate(three.year = years.in.SAM>=3, "YES","NO") %>% 
  mutate(five.year = years.in.SAM>=5, "YES","NO") %>% 
  mutate(ten.year = years.in.SAM>=10, "YES","NO") %>%   ##only works since start at same time
  mutate(biz_size = ifelse(contractingofficerbusinesssizedetermination == "O", 1, 0)) %>% 
  select(duns, biz_size, years.in.SAM, NAICS2, ServicesCategory, location, ownership.woman, 
         ownership.veteran, ownership.minority, ownership.foreign, contract.actions,
         obligated.amt, firm.age, three.year, five.year,ten.year, survival.status, DEPARTMENT_NAME, AGENCY_NAME) %>% 
  filter(NAICS2 != "NU")

#2005

year.edit = final_joined %>% 
  mutate(regyear = year(registrationDate)) %>%
  arrange(regyear) %>% 
  filter(regyear == 2005) 


#filter(contractingofficerbusinesssizedetermination == "S")
year.edit.unique = year.edit[!duplicated(year.edit[,c('duns')]),]

#### filtered out firms with parent duns of the parent duns did not register in 2005

parent.edit = year.edit %>%
  mutate (haveparent = ifelse(duns == parentdunsnumber|parentdunsnumber == "NULL", "NO", "YES")) %>% 
  filter(haveparent == "NO") %>% 
  #filter(contractingofficerbusinesssizedetermination != ":") %>% 
  #filter(contractingofficerbusinesssizedetermination != "NULL") %>% 
  select(duns, haveparent, fiscal_year, signeddate, parentdunsnumber, vendorname) %>% 
  arrange(fiscal_year) 

parent.unique = parent.edit[!duplicated(parent.edit[,c('duns')]),] 


NAICS = as.data.frame(NAICS.unique.column)
year = as.data.frame(year.edit.unique)
parent = as.data.frame(parent.unique)

joined.1 = merge.data.frame(parent, NAICS, by = "duns")
joined.2 = merge.data.frame(joined.1, year, by = "duns")

##joined.2 = merge.data.frame(NAICS, year, by = "duns")  ##no parent

joined.25 = joined.2 %>% 
  select(duns, NAICS2, age_at_start, months_in_SAM, status,regyear, registrationDate, 
         businessStartDate, country, womenownedflag, veteranownedflag, aiobflag, naobflag,
         minorityownedbusinessflag, apaobflag, baobflag, baobflag, saaobflag, haobflag, 
         isnativehawaiianownedorganizationorfirm, isotherminorityowned, istriballyownedfirm, 
         isalaskannativeownedcorporationorfirm, other_minority_owned_business, 
         isforeignownedandlocated, expirationDate, contractingofficerbusinesssizedetermination)

joined.3 = merge.data.frame(joined.25, PSC, by = "duns", all.x = TRUE)
joined.4 = merge.data.frame(joined.3, oblandact, by = "duns", all.x = TRUE)
joined.5 = merge.data.frame(joined.4, agency, by = "duns", all.x = TRUE)


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
  mutate(years.in.SAM = months_in_SAM/12) %>% 
  mutate(survival.status = ifelse(year(expirationDate) > 2011, "1", "0")) %>%
  mutate(three.year = years.in.SAM>=3, "YES","NO") %>% 
  mutate(five.year = years.in.SAM>=5, "YES","NO") %>% 
  mutate(ten.year = years.in.SAM>=10, "YES","NO") %>%   ##only works since start at same time
  mutate(biz_size = ifelse(contractingofficerbusinesssizedetermination == "O", 1, 0)) %>% 
  select(duns, biz_size, years.in.SAM, NAICS2, ServicesCategory, location, ownership.woman, 
         ownership.veteran, ownership.minority, ownership.foreign, contract.actions,
         obligated.amt, firm.age, three.year, five.year,ten.year, survival.status, DEPARTMENT_NAME, AGENCY_NAME) %>% 
  filter(NAICS2 != "NU")

#2006

year.edit = final_joined %>% 
  mutate(regyear = year(registrationDate)) %>%
  arrange(regyear) %>% 
  filter(regyear == 2006) 


#filter(contractingofficerbusinesssizedetermination == "S")
year.edit.unique = year.edit[!duplicated(year.edit[,c('duns')]),]

#### filtered out firms with parent duns of the parent duns did not register in 2006

parent.edit = year.edit %>%
  mutate (haveparent = ifelse(duns == parentdunsnumber|parentdunsnumber == "NULL", "NO", "YES")) %>% 
  filter(haveparent == "NO") %>% 
  #filter(contractingofficerbusinesssizedetermination != ":") %>% 
  #filter(contractingofficerbusinesssizedetermination != "NULL") %>% 
  select(duns, haveparent, fiscal_year, signeddate, parentdunsnumber, vendorname) %>% 
  arrange(fiscal_year) 

parent.unique = parent.edit[!duplicated(parent.edit[,c('duns')]),] 


NAICS = as.data.frame(NAICS.unique.column)
year = as.data.frame(year.edit.unique)
parent = as.data.frame(parent.unique)

joined.1 = merge.data.frame(parent, NAICS, by = "duns")
joined.2 = merge.data.frame(joined.1, year, by = "duns")

##joined.2 = merge.data.frame(NAICS, year, by = "duns")  ##no parent

joined.25 = joined.2 %>% 
  select(duns, NAICS2, age_at_start, months_in_SAM, status,regyear, registrationDate, 
         businessStartDate, country, womenownedflag, veteranownedflag, aiobflag, naobflag,
         minorityownedbusinessflag, apaobflag, baobflag, baobflag, saaobflag, haobflag, 
         isnativehawaiianownedorganizationorfirm, isotherminorityowned, istriballyownedfirm, 
         isalaskannativeownedcorporationorfirm, other_minority_owned_business, 
         isforeignownedandlocated, expirationDate, contractingofficerbusinesssizedetermination)

joined.3 = merge.data.frame(joined.25, PSC, by = "duns", all.x = TRUE)
joined.4 = merge.data.frame(joined.3, oblandact, by = "duns", all.x = TRUE)
joined.5 = merge.data.frame(joined.4, agency, by = "duns", all.x = TRUE)


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
  mutate(years.in.SAM = months_in_SAM/12) %>% 
  mutate(survival.status = ifelse(year(expirationDate) > 2011, "1", "0")) %>%
  mutate(three.year = years.in.SAM>=3, "YES","NO") %>% 
  mutate(five.year = years.in.SAM>=5, "YES","NO") %>% 
  mutate(ten.year = years.in.SAM>=10, "YES","NO") %>%   ##only works since start at same time
  mutate(biz_size = ifelse(contractingofficerbusinesssizedetermination == "O", 1, 0)) %>% 
  select(duns, biz_size, years.in.SAM, NAICS2, ServicesCategory, location, ownership.woman, 
         ownership.veteran, ownership.minority, ownership.foreign, contract.actions,
         obligated.amt, firm.age, three.year, five.year,ten.year, survival.status, DEPARTMENT_NAME, AGENCY_NAME) %>% 
  filter(NAICS2 != "NU")

#2007

year.edit = final_joined %>% 
  mutate(regyear = year(registrationDate)) %>%
  arrange(regyear) %>% 
  filter(regyear == 2007) 


#filter(contractingofficerbusinesssizedetermination == "S")
year.edit.unique = year.edit[!duplicated(year.edit[,c('duns')]),]

#### filtered out firms with parent duns of the parent duns did not register in 2007

parent.edit = year.edit %>%
  mutate (haveparent = ifelse(duns == parentdunsnumber|parentdunsnumber == "NULL", "NO", "YES")) %>% 
  filter(haveparent == "NO") %>% 
  #filter(contractingofficerbusinesssizedetermination != ":") %>% 
  #filter(contractingofficerbusinesssizedetermination != "NULL") %>% 
  select(duns, haveparent, fiscal_year, signeddate, parentdunsnumber, vendorname) %>% 
  arrange(fiscal_year) 

parent.unique = parent.edit[!duplicated(parent.edit[,c('duns')]),] 


NAICS = as.data.frame(NAICS.unique.column)
year = as.data.frame(year.edit.unique)
parent = as.data.frame(parent.unique)

joined.1 = merge.data.frame(parent, NAICS, by = "duns")
joined.2 = merge.data.frame(joined.1, year, by = "duns")

##joined.2 = merge.data.frame(NAICS, year, by = "duns")  ##no parent

joined.25 = joined.2 %>% 
  select(duns, NAICS2, age_at_start, months_in_SAM, status,regyear, registrationDate, 
         businessStartDate, country, womenownedflag, veteranownedflag, aiobflag, naobflag,
         minorityownedbusinessflag, apaobflag, baobflag, baobflag, saaobflag, haobflag, 
         isnativehawaiianownedorganizationorfirm, isotherminorityowned, istriballyownedfirm, 
         isalaskannativeownedcorporationorfirm, other_minority_owned_business, 
         isforeignownedandlocated, expirationDate, contractingofficerbusinesssizedetermination)

joined.3 = merge.data.frame(joined.25, PSC, by = "duns", all.x = TRUE)
joined.4 = merge.data.frame(joined.3, oblandact, by = "duns", all.x = TRUE)
joined.5 = merge.data.frame(joined.4, agency, by = "duns", all.x = TRUE)


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
  mutate(years.in.SAM = months_in_SAM/12) %>% 
  mutate(survival.status = ifelse(year(expirationDate) > 2011, "1", "0")) %>%
  mutate(three.year = years.in.SAM>=3, "YES","NO") %>% 
  mutate(five.year = years.in.SAM>=5, "YES","NO") %>% 
  mutate(ten.year = years.in.SAM>=10, "YES","NO") %>%   ##only works since start at same time
  mutate(biz_size = ifelse(contractingofficerbusinesssizedetermination == "O", 1, 0)) %>% 
  select(duns, biz_size, years.in.SAM, NAICS2, ServicesCategory, location, ownership.woman, 
         ownership.veteran, ownership.minority, ownership.foreign, contract.actions,
         obligated.amt, firm.age, three.year, five.year,ten.year, survival.status, DEPARTMENT_NAME, AGENCY_NAME) %>% 
  filter(NAICS2 != "NU")


##2008

year.edit = final_joined %>% 
  mutate(regyear = year(registrationDate)) %>%
  arrange(regyear) %>% 
  filter(regyear == 2008) 


#filter(contractingofficerbusinesssizedetermination == "S")
year.edit.unique = year.edit[!duplicated(year.edit[,c('duns')]),]

#### filtered out firms with parent duns of the parent duns did not register in 2008

parent.edit = year.edit %>%
  mutate (haveparent = ifelse(duns == parentdunsnumber|parentdunsnumber == "NULL", "NO", "YES")) %>% 
  filter(haveparent == "NO") %>% 
  #filter(contractingofficerbusinesssizedetermination != ":") %>% 
  #filter(contractingofficerbusinesssizedetermination != "NULL") %>% 
  select(duns, haveparent, fiscal_year, signeddate, parentdunsnumber, vendorname) %>% 
  arrange(fiscal_year) 

parent.unique = parent.edit[!duplicated(parent.edit[,c('duns')]),] 


NAICS = as.data.frame(NAICS.unique.column)
year = as.data.frame(year.edit.unique)
parent = as.data.frame(parent.unique)

joined.1 = merge.data.frame(parent, NAICS, by = "duns")
joined.2 = merge.data.frame(joined.1, year, by = "duns")

##joined.2 = merge.data.frame(NAICS, year, by = "duns")  ##no parent

joined.25 = joined.2 %>% 
  select(duns, NAICS2, age_at_start, months_in_SAM, status,regyear, registrationDate, 
         businessStartDate, country, womenownedflag, veteranownedflag, aiobflag, naobflag,
         minorityownedbusinessflag, apaobflag, baobflag, baobflag, saaobflag, haobflag, 
         isnativehawaiianownedorganizationorfirm, isotherminorityowned, istriballyownedfirm, 
         isalaskannativeownedcorporationorfirm, other_minority_owned_business, 
         isforeignownedandlocated, expirationDate, contractingofficerbusinesssizedetermination)

joined.3 = merge.data.frame(joined.25, PSC, by = "duns", all.x = TRUE)
joined.4 = merge.data.frame(joined.3, oblandact, by = "duns", all.x = TRUE)
joined.5 = merge.data.frame(joined.4, agency, by = "duns", all.x = TRUE)


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
  mutate(years.in.SAM = months_in_SAM/12) %>% 
  mutate(survival.status = ifelse(year(expirationDate) > 2011, "1", "0")) %>%
  mutate(three.year = years.in.SAM>=3, "YES","NO") %>% 
  mutate(five.year = years.in.SAM>=5, "YES","NO") %>% 
  mutate(ten.year = years.in.SAM>=10, "YES","NO") %>%   ##only works since start at same time
  mutate(biz_size = ifelse(contractingofficerbusinesssizedetermination == "O", 1, 0)) %>% 
  select(duns, biz_size, years.in.SAM, NAICS2, ServicesCategory, location, ownership.woman, 
         ownership.veteran, ownership.minority, ownership.foreign, contract.actions,
         obligated.amt, firm.age, three.year, five.year,ten.year, survival.status, DEPARTMENT_NAME, AGENCY_NAME) %>% 
  filter(NAICS2 != "NU")

##2009

year.edit = final_joined %>% 
  mutate(regyear = year(registrationDate)) %>%
  arrange(regyear) %>% 
  filter(regyear == 2009) 


#filter(contractingofficerbusinesssizedetermination == "S")
year.edit.unique = year.edit[!duplicated(year.edit[,c('duns')]),]

#### filtered out firms with parent duns of the parent duns did not register in 2009

parent.edit = year.edit %>%
  mutate (haveparent = ifelse(duns == parentdunsnumber|parentdunsnumber == "NULL", "NO", "YES")) %>% 
  filter(haveparent == "NO") %>% 
  #filter(contractingofficerbusinesssizedetermination != ":") %>% 
  #filter(contractingofficerbusinesssizedetermination != "NULL") %>% 
  select(duns, haveparent, fiscal_year, signeddate, parentdunsnumber, vendorname) %>% 
  arrange(fiscal_year) 

parent.unique = parent.edit[!duplicated(parent.edit[,c('duns')]),] 


NAICS = as.data.frame(NAICS.unique.column)
year = as.data.frame(year.edit.unique)
parent = as.data.frame(parent.unique)

joined.1 = merge.data.frame(parent, NAICS, by = "duns")
joined.2 = merge.data.frame(joined.1, year, by = "duns")

##joined.2 = merge.data.frame(NAICS, year, by = "duns")  ##no parent

joined.25 = joined.2 %>% 
  select(duns, NAICS2, age_at_start, months_in_SAM, status,regyear, registrationDate, 
         businessStartDate, country, womenownedflag, veteranownedflag, aiobflag, naobflag,
         minorityownedbusinessflag, apaobflag, baobflag, baobflag, saaobflag, haobflag, 
         isnativehawaiianownedorganizationorfirm, isotherminorityowned, istriballyownedfirm, 
         isalaskannativeownedcorporationorfirm, other_minority_owned_business, 
         isforeignownedandlocated, expirationDate, contractingofficerbusinesssizedetermination)

joined.3 = merge.data.frame(joined.25, PSC, by = "duns", all.x = TRUE)
joined.4 = merge.data.frame(joined.3, oblandact, by = "duns", all.x = TRUE)
joined.5 = merge.data.frame(joined.4, agency, by = "duns", all.x = TRUE)


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
  mutate(years.in.SAM = months_in_SAM/12) %>% 
  mutate(survival.status = ifelse(year(expirationDate) > 2011, "1", "0")) %>%
  mutate(three.year = years.in.SAM>=3, "YES","NO") %>% 
  mutate(five.year = years.in.SAM>=5, "YES","NO") %>% 
  mutate(ten.year = years.in.SAM>=10, "YES","NO") %>%   ##only works since start at same time
  mutate(biz_size = ifelse(contractingofficerbusinesssizedetermination == "O", 1, 0)) %>% 
  select(duns, biz_size, years.in.SAM, NAICS2, ServicesCategory, location, ownership.woman, 
         ownership.veteran, ownership.minority, ownership.foreign, contract.actions,
         obligated.amt, firm.age, three.year, five.year,ten.year, survival.status, DEPARTMENT_NAME, AGENCY_NAME) %>% 
  filter(NAICS2 != "NU")


##2010

year.edit = final_joined %>% 
  mutate(regyear = year(registrationDate)) %>%
  arrange(regyear) %>% 
  filter(regyear == 2010) 


#filter(contractingofficerbusinesssizedetermination == "S")
year.edit.unique = year.edit[!duplicated(year.edit[,c('duns')]),]

#### filtered out firms with parent duns of the parent duns did not register in 2010

parent.edit = year.edit %>%
  mutate (haveparent = ifelse(duns == parentdunsnumber|parentdunsnumber == "NULL", "NO", "YES")) %>% 
  filter(haveparent == "NO") %>% 
  #filter(contractingofficerbusinesssizedetermination != ":") %>% 
  #filter(contractingofficerbusinesssizedetermination != "NULL") %>% 
  select(duns, haveparent, fiscal_year, signeddate, parentdunsnumber, vendorname) %>% 
  arrange(fiscal_year) 

parent.unique = parent.edit[!duplicated(parent.edit[,c('duns')]),] 


NAICS = as.data.frame(NAICS.unique.column)
year = as.data.frame(year.edit.unique)
parent = as.data.frame(parent.unique)

joined.1 = merge.data.frame(parent, NAICS, by = "duns")
joined.2 = merge.data.frame(joined.1, year, by = "duns")

##joined.2 = merge.data.frame(NAICS, year, by = "duns")  ##no parent

joined.25 = joined.2 %>% 
  select(duns, NAICS2, age_at_start, months_in_SAM, status,regyear, registrationDate, 
         businessStartDate, country, womenownedflag, veteranownedflag, aiobflag, naobflag,
         minorityownedbusinessflag, apaobflag, baobflag, baobflag, saaobflag, haobflag, 
         isnativehawaiianownedorganizationorfirm, isotherminorityowned, istriballyownedfirm, 
         isalaskannativeownedcorporationorfirm, other_minority_owned_business, 
         isforeignownedandlocated, expirationDate, contractingofficerbusinesssizedetermination)

joined.3 = merge.data.frame(joined.25, PSC, by = "duns", all.x = TRUE)
joined.4 = merge.data.frame(joined.3, oblandact, by = "duns", all.x = TRUE)
joined.5 = merge.data.frame(joined.4, agency, by = "duns", all.x = TRUE)



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
  mutate(years.in.SAM = months_in_SAM/12) %>% 
  mutate(survival.status = ifelse(year(expirationDate) > 2011, "1", "0")) %>%
  mutate(three.year = years.in.SAM>=3, "YES","NO") %>% 
  mutate(five.year = years.in.SAM>=5, "YES","NO") %>% 
  mutate(ten.year = years.in.SAM>=10, "YES","NO") %>%   ##only works since start at same time
  mutate(biz_size = ifelse(contractingofficerbusinesssizedetermination == "O", 1, 0)) %>% 
  select(duns, biz_size, years.in.SAM, NAICS2, ServicesCategory, location, ownership.woman, 
         ownership.veteran, ownership.minority, ownership.foreign, contract.actions,
         obligated.amt, firm.age, three.year, five.year,ten.year, survival.status, DEPARTMENT_NAME, AGENCY_NAME) %>% 
  filter(NAICS2 != "NU")

##2011

year.edit = final_joined %>% 
  mutate(regyear = year(registrationDate)) %>%
  arrange(regyear) %>% 
  filter(regyear == 2011) 


#filter(contractingofficerbusinesssizedetermination == "S")
year.edit.unique = year.edit[!duplicated(year.edit[,c('duns')]),]

#### filtered out firms with parent duns of the parent duns did not register in 2011

parent.edit = year.edit %>%
  mutate (haveparent = ifelse(duns == parentdunsnumber|parentdunsnumber == "NULL", "NO", "YES")) %>% 
  filter(haveparent == "NO") %>% 
  #filter(contractingofficerbusinesssizedetermination != ":") %>% 
  #filter(contractingofficerbusinesssizedetermination != "NULL") %>% 
  select(duns, haveparent, fiscal_year, signeddate, parentdunsnumber, vendorname) %>% 
  arrange(fiscal_year) 

parent.unique = parent.edit[!duplicated(parent.edit[,c('duns')]),] 


NAICS = as.data.frame(NAICS.unique.column)
year = as.data.frame(year.edit.unique)
parent = as.data.frame(parent.unique)

joined.1 = merge.data.frame(parent, NAICS, by = "duns")
joined.2 = merge.data.frame(joined.1, year, by = "duns")

##joined.2 = merge.data.frame(NAICS, year, by = "duns")  ##no parent

joined.25 = joined.2 %>% 
  select(duns, NAICS2, age_at_start, months_in_SAM, status,regyear, registrationDate, 
         businessStartDate, country, womenownedflag, veteranownedflag, aiobflag, naobflag,
         minorityownedbusinessflag, apaobflag, baobflag, baobflag, saaobflag, haobflag, 
         isnativehawaiianownedorganizationorfirm, isotherminorityowned, istriballyownedfirm, 
         isalaskannativeownedcorporationorfirm, other_minority_owned_business, 
         isforeignownedandlocated, expirationDate, contractingofficerbusinesssizedetermination)

joined.3 = merge.data.frame(joined.25, PSC, by = "duns", all.x = TRUE)
joined.4 = merge.data.frame(joined.3, oblandact, by = "duns", all.x = TRUE)
joined.5 = merge.data.frame(joined.4, agency, by = "duns", all.x = TRUE)


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
  mutate(years.in.SAM = months_in_SAM/12) %>% 
  mutate(survival.status = ifelse(year(expirationDate) > 2011, "1", "0")) %>%
  mutate(three.year = years.in.SAM>=3, "YES","NO") %>% 
  mutate(five.year = years.in.SAM>=5, "YES","NO") %>% 
  mutate(ten.year = years.in.SAM>=10, "YES","NO") %>%   ##only works since start at same time
  mutate(biz_size = ifelse(contractingofficerbusinesssizedetermination == "O", 1, 0)) %>% 
  select(duns, biz_size, years.in.SAM, NAICS2, ServicesCategory, location, ownership.woman, 
         ownership.veteran, ownership.minority, ownership.foreign, contract.actions,
         obligated.amt, firm.age, three.year, five.year,ten.year, survival.status, DEPARTMENT_NAME, AGENCY_NAME) %>% 
  filter(NAICS2 != "NU")

#### bind each created dataset together

all.dataset <- rbind(dataset.2001, dataset.2002, dataset.2003, dataset.2004, dataset.2005, dataset.2006, dataset.2007, dataset.2008, dataset.2009, dataset.2010, dataset.2011)

##all.dataset.parentno <- rbind(dataset.2001, dataset.2002, dataset.2003, dataset.2004, dataset.2005, dataset.2006, dataset.2007, dataset.2008, dataset.2009, dataset.2010, dataset.2011)

write.csv(all.dataset, "SAM2001-2011.csv")

write.csv(all.dataset.parentno, "parentno2001-2011.csv")
