## x = year(registrationDate)  OR Regyear
##y = nrows()
##add lines for biz_size 0 or 1

full_FPDS <- read_csv("SAM Data merged with FPDS, exp2000-2019.csv")

final_joined = full_FPDS[!duplicated(full_FPDS),]

final_joined <- final_joined %>% 
  mutate(age_at_start = year(registrationDate) - year(businessStartDate)) %>% 
  mutate(months_in_SAM = ((year(expirationDate) - year(registrationDate)) * 12) + month(expirationDate) - month(registrationDate)) %>% 
  rename(country = `samAddress countryCode`)




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


##2012

year.edit = final_joined %>% 
  mutate(regyear = year(registrationDate)) %>%
  arrange(regyear) %>% 
  filter(regyear == 2012) 


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

joined.3 = merge.data.frame(joined.25, PSC, by = "duns", all.x = TRUE)
joined.4 = merge.data.frame(joined.3, oblandact, by = "duns", all.x = TRUE)
joined.5 = merge.data.frame(joined.4, agency, by = "duns", all.x = TRUE)


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
  mutate(years.in.SAM = months_in_SAM/12) %>% 
  mutate(survival.status = ifelse(year(expirationDate) > 2012, "1", "0")) %>%
  mutate(three.year = years.in.SAM>=3, "YES","NO") %>% 
  mutate(five.year = years.in.SAM>=5, "YES","NO") %>% 
  mutate(ten.year = years.in.SAM>=10, "YES","NO") %>%   ##only works since start at same time
  mutate(biz_size = ifelse(contractingofficerbusinesssizedetermination == "O", 1, 0)) %>% 
  select(duns, biz_size, years.in.SAM, NAICS2, ServicesCategory, location, ownership.woman, 
         ownership.veteran, ownership.minority, ownership.foreign, contract.actions,
         obligated.amt, firm.age, three.year, five.year,ten.year, survival.status, DEPARTMENT_NAME, AGENCY_NAME) %>% 
  filter(NAICS2 != "NU")

##2013

year.edit = final_joined %>% 
  mutate(regyear = year(registrationDate)) %>%
  arrange(regyear) %>% 
  filter(regyear == 2013) 


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
  mutate(years.in.SAM = months_in_SAM/12) %>% 
  mutate(survival.status = ifelse(year(expirationDate) > 2013, "1", "0")) %>%
  mutate(three.year = years.in.SAM>=3, "YES","NO") %>% 
  mutate(five.year = years.in.SAM>=5, "YES","NO") %>% 
  mutate(ten.year = years.in.SAM>=10, "YES","NO") %>%   ##only works since start at same time
  mutate(biz_size = ifelse(contractingofficerbusinesssizedetermination == "O", 1, 0)) %>% 
  select(duns, biz_size, years.in.SAM, NAICS2, ServicesCategory, location, ownership.woman, 
         ownership.veteran, ownership.minority, ownership.foreign, contract.actions,
         obligated.amt, firm.age, three.year, five.year,ten.year, survival.status, DEPARTMENT_NAME, AGENCY_NAME) %>% 
  filter(NAICS2 != "NU")

##2014

year.edit = final_joined %>% 
  mutate(regyear = year(registrationDate)) %>%
  arrange(regyear) %>% 
  filter(regyear == 2014) 


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
  mutate(years.in.SAM = months_in_SAM/12) %>% 
  mutate(survival.status = ifelse(year(expirationDate) > 2014, "1", "0")) %>%
  mutate(three.year = years.in.SAM>=3, "YES","NO") %>% 
  mutate(five.year = years.in.SAM>=5, "YES","NO") %>% 
  mutate(ten.year = years.in.SAM>=10, "YES","NO") %>%   ##only works since start at same time
  mutate(biz_size = ifelse(contractingofficerbusinesssizedetermination == "O", 1, 0)) %>% 
  select(duns, biz_size, years.in.SAM, NAICS2, ServicesCategory, location, ownership.woman, 
         ownership.veteran, ownership.minority, ownership.foreign, contract.actions,
         obligated.amt, firm.age, three.year, five.year,ten.year, survival.status, DEPARTMENT_NAME, AGENCY_NAME) %>% 
  filter(NAICS2 != "NU")

##2015

year.edit = final_joined %>% 
  mutate(regyear = year(registrationDate)) %>%
  arrange(regyear) %>% 
  filter(regyear == 2015) 


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
  mutate(years.in.SAM = months_in_SAM/12) %>% 
  mutate(survival.status = ifelse(year(expirationDate) > 2015, "1", "0")) %>%
  mutate(three.year = years.in.SAM>=3, "YES","NO") %>% 
  mutate(five.year = years.in.SAM>=5, "YES","NO") %>% 
  mutate(ten.year = years.in.SAM>=10, "YES","NO") %>%   ##only works since start at same time
  mutate(biz_size = ifelse(contractingofficerbusinesssizedetermination == "O", 1, 0)) %>% 
  select(duns, biz_size, years.in.SAM, NAICS2, ServicesCategory, location, ownership.woman, 
         ownership.veteran, ownership.minority, ownership.foreign, contract.actions,
         obligated.amt, firm.age, three.year, five.year,ten.year, survival.status, DEPARTMENT_NAME, AGENCY_NAME) %>% 
  filter(NAICS2 != "NU")

##2016

year.edit = final_joined %>% 
  mutate(regyear = year(registrationDate)) %>%
  arrange(regyear) %>% 
  filter(regyear == 2016) 


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
  mutate(years.in.SAM = months_in_SAM/12) %>% 
  mutate(survival.status = ifelse(year(expirationDate) > 2016, "1", "0")) %>%
  mutate(three.year = years.in.SAM>=3, "YES","NO") %>% 
  mutate(five.year = years.in.SAM>=5, "YES","NO") %>% 
  mutate(ten.year = years.in.SAM>=10, "YES","NO") %>%   ##only works since start at same time
  mutate(biz_size = ifelse(contractingofficerbusinesssizedetermination == "O", 1, 0)) %>% 
  select(duns, biz_size, years.in.SAM, NAICS2, ServicesCategory, location, ownership.woman, 
         ownership.veteran, ownership.minority, ownership.foreign, contract.actions,
         obligated.amt, firm.age, three.year, five.year,ten.year, survival.status, DEPARTMENT_NAME, AGENCY_NAME) %>% 
  filter(NAICS2 != "NU")

panel_data <- read_csv("Panel Data reg 2001-2011.csv")
panel_data <- select(panel_data, -X1)

x0116.dataset <- rbind(panel_data, dataset.2012, dataset.2013, dataset.2014, dataset.2015, dataset.2016)
x0116.dataset <- x0116.dataset %>% 
  filter(!is.na(biz_size))
  

library(tidyverse)
library(lubridate)



timeseries_data <- x0116.dataset %>% 
  left_join(full_FPDS[, c("duns", "registrationDate")], by = "duns") %>% 
  distinct() %>% 
  mutate(regyear = year(registrationDate))
  
timeseries_data.all <- timeseries_data %>% 
  group_by(regyear) %>% 
  dplyr::summarise(freq = n()) %>% 
  right_join(timeseries_data, by = "regyear")
  
timeseries_data.size <- timeseries_data.all %>% 
  group_by(regyear, biz_size, freq) %>% 
  dplyr::summarise(sizefreq = n()) %>%
  spread(biz_size, sizefreq) %>% 
  rename("non_small" = "1", "small" = "0", "all" = "freq")
 # right_join(timeseries_data.all, by = c("regyear", "biz_size"))

timeseries_data.all$survival.status = as.numeric(time)


timeseries_data.NAICS <- timeseries_data.all %>% 
  group_by(regyear, survival.status, NAICS2) %>% 
  dplyr::summarize(NAICSfreq = n()) %>% 
  #spread(NAICS2, NAICSfreq) %>% 
  filter(regyear>=2001 & regyear<=2010) %>% 
  group_by(regyear, survival.status) %>% 
  top_n(n = 3, wt = NAICSfreq)

timeseries_data.NAICS[is.na(timeseries_data.NAICS)] <- 0

names(timeseries_data.size)



ggplot(timeseries_data.size, aes(x = regyear)) +
  geom_line(aes(y = all, colour = "black")) +
  geom_line(aes(y = non_small, colour = "blue")) +
  geom_line(aes(y = small, colour = "red")) +
  ylab("Number of new entrants") +
  xlab("Year") +
  scale_x_continuous("Registration Date", labels = as.character(timeseries_data.size$regyear), breaks = timeseries_data.size$regyear) +
  scale_color_manual(name = "New Entrants Types", values = c("black" = "black", "blue" = "blue", "red" = "red"), labels = c("all","non-small", "small"))



##ggplot(timeseries_data.NAICS, aes(x = regyear, y = NAICSfreq, fill = NAICS2)) +
##  geom_bar(stat = "identity", width = .5, position = "dodge") +
##  ylab("NAICS") +
##  scale_x_continuous("Registration Date", labels = as.character(timeseries_data.size$regyear), breaks = timeseries_data.size$regyear) +
##  facet_wrap(~survival.status)
  


t_test <- read_csv("t-test reg2000-2011.csv")
t_test_nop <- read_csv("t-test reg2000-2011-no parent filter.csv")
survival_rates <- read_csv("survival rates reg2000-2011.csv")
survival_rates_nop <- read_csv("survival rates reg2000-2011-no parent filter.csv")



