##Exploration of results

##Survival rates by sector####

sd_nop <- read_csv("Panel Data reg2001-2016 - ver4.csv")

df2001_sd <- sd_nop %>% 
  filter(year(registrationDate) == 2001 )
df2002_sd <- sd_nop %>% 
  filter(year(registrationDate) == 2002 )
df2003_sd <- sd_nop %>% 
  filter(year(registrationDate) == 2003 )
df2004_sd <- sd_nop %>% 
  filter(year(registrationDate) == 2004 )
df2005_sd <- sd_nop %>% 
  filter(year(registrationDate) == 2005 )
df2006_sd <- sd_nop %>% 
  filter(year(registrationDate) == 2006 )

df2001_sd$NAICS2 = as.factor(df2001_sd$NAICS2)
df2001_sd$ServicesCategory = as.factor(df2001_sd$ServicesCategory)
df2001_sd$location = as.numeric(df2001_sd$location)
df2001_sd$ownership.woman = as.numeric(df2001_sd$ownership.woman)
df2001_sd$ownership.veteran = as.numeric(df2001_sd$ownership.veteran)
df2001_sd$ownership.minority = as.numeric(df2001_sd$ownership.minority)
df2001_sd$ownership.foreign = as.numeric(df2001_sd$ownership.foreign)
df2001_sd$survival.status = as.numeric(df2001_sd$survival.status)
df2001_sd$DEPARTMENT_NAME = as.factor(df2001_sd$DEPARTMENT_NAME)
df2001_sd$AGENCY_NAME = as.factor(df2001_sd$AGENCY_NAME)


df2002_sd$NAICS2 = as.factor(df2002_sd$NAICS2)
df2002_sd$ServicesCategory = as.factor(df2002_sd$ServicesCategory)
df2002_sd$location = as.numeric(df2002_sd$location)
df2002_sd$ownership.woman = as.numeric(df2002_sd$ownership.woman)
df2002_sd$ownership.veteran = as.numeric(df2002_sd$ownership.veteran)
df2002_sd$ownership.minority = as.numeric(df2002_sd$ownership.minority)
df2002_sd$ownership.foreign = as.numeric(df2002_sd$ownership.foreign)
df2002_sd$survival.status = as.numeric(df2002_sd$survival.status)
df2002_sd$DEPARTMENT_NAME = as.factor(df2002_sd$DEPARTMENT_NAME)
df2002_sd$AGENCY_NAME = as.factor(df2002_sd$AGENCY_NAME)

df2003_sd$NAICS2 = as.factor(df2003_sd$NAICS2)
df2003_sd$ServicesCategory = as.factor(df2003_sd$ServicesCategory)
df2003_sd$location = as.numeric(df2003_sd$location)
df2003_sd$ownership.woman = as.numeric(df2003_sd$ownership.woman)
df2003_sd$ownership.veteran = as.numeric(df2003_sd$ownership.veteran)
df2003_sd$ownership.minority = as.numeric(df2003_sd$ownership.minority)
df2003_sd$ownership.foreign = as.numeric(df2003_sd$ownership.foreign)
df2003_sd$survival.status = as.numeric(df2003_sd$survival.status)
df2003_sd$DEPARTMENT_NAME = as.factor(df2003_sd$DEPARTMENT_NAME)
df2003_sd$AGENCY_NAME = as.factor(df2003_sd$AGENCY_NAME)

df2004_sd$NAICS2 = as.factor(df2004_sd$NAICS2)
df2004_sd$ServicesCategory = as.factor(df2004_sd$ServicesCategory)
df2004_sd$location = as.numeric(df2004_sd$location)
df2004_sd$ownership.woman = as.numeric(df2004_sd$ownership.woman)
df2004_sd$ownership.veteran = as.numeric(df2004_sd$ownership.veteran)
df2004_sd$ownership.minority = as.numeric(df2004_sd$ownership.minority)
df2004_sd$ownership.foreign = as.numeric(df2004_sd$ownership.foreign)
df2004_sd$survival.status = as.numeric(df2004_sd$survival.status)
df2004_sd$DEPARTMENT_NAME = as.factor(df2004_sd$DEPARTMENT_NAME)
df2004_sd$AGENCY_NAME = as.factor(df2004_sd$AGENCY_NAME)

df2005_sd$NAICS2 = as.factor(df2005_sd$NAICS2)
df2005_sd$ServicesCategory = as.factor(df2005_sd$ServicesCategory)
df2005_sd$location = as.numeric(df2005_sd$location)
df2005_sd$ownership.woman = as.numeric(df2005_sd$ownership.woman)
df2005_sd$ownership.veteran = as.numeric(df2005_sd$ownership.veteran)
df2005_sd$ownership.minority = as.numeric(df2005_sd$ownership.minority)
df2005_sd$ownership.foreign = as.numeric(df2005_sd$ownership.foreign)
df2005_sd$survival.status = as.numeric(df2005_sd$survival.status)
df2005_sd$DEPARTMENT_NAME = as.factor(df2005_sd$DEPARTMENT_NAME)
df2005_sd$AGENCY_NAME = as.factor(df2005_sd$AGENCY_NAME)

df2006_sd$NAICS2 = as.factor(df2006_sd$NAICS2)
df2006_sd$ServicesCategory = as.factor(df2006_sd$ServicesCategory)
df2006_sd$location = as.numeric(df2006_sd$location)
df2006_sd$ownership.woman = as.numeric(df2006_sd$ownership.woman)
df2006_sd$ownership.veteran = as.numeric(df2006_sd$ownership.veteran)
df2006_sd$ownership.minority = as.numeric(df2006_sd$ownership.minority)
df2006_sd$ownership.foreign = as.numeric(df2006_sd$ownership.foreign)
df2006_sd$survival.status = as.numeric(df2006_sd$survival.status)
df2006_sd$DEPARTMENT_NAME = as.factor(df2006_sd$DEPARTMENT_NAME)
df2006_sd$AGENCY_NAME = as.factor(df2006_sd$AGENCY_NAME)


###which sectors####
dod_sd <- read_csv("Panel Data reg2001-2016 DOD - ver4.csv")

year_div <- dod_sd %>% 
  mutate(regyear = year(registrationDate)) %>% 
  filter(regyear <= 2006) %>% 
  filter(biz_size == 1 | biz_size == 0) %>% 
  group_by(regyear) %>% 
  dplyr::summarise(n()) 

year_size_div <- dod_sd %>% 
  mutate(regyear = year(registrationDate)) %>% 
  filter(regyear <= 2006) %>% 
  filter(biz_size == 1 | biz_size == 0) %>% 
  group_by(regyear, biz_size) %>% 
  dplyr::summarise(n()) %>% 
  left_join(year_div, by = "regyear") %>% 
  dplyr::rename("total registration per year" = `n().y`) %>% 
  dplyr::rename("registration per year per business size" = `n().x`)

write.csv(year_size_div, "count total registration per year and by biz_size - DOD.csv")

year_div <- sd_nop %>% 
  mutate(regyear = year(registrationDate)) %>% 
  filter(regyear <= 2006) %>% 
  filter(biz_size == 1 | biz_size == 0) %>% 
  group_by(regyear) %>% 
  dplyr::summarise(n()) 

year_size_div <- sd_nop %>% 
  mutate(regyear = year(registrationDate)) %>% 
  filter(regyear <= 2006) %>% 
  filter(biz_size == 1 | biz_size == 0) %>% 
  group_by(regyear, biz_size) %>% 
  dplyr::summarise(n()) %>% 
  left_join(year_div, by = "regyear") %>% 
  dplyr::rename("total registration per year" = `n().y`) %>% 
  dplyr::rename("registration per year per business size" = `n().x`)

write.csv(year_size_div, "count total registration per year and by biz_size.csv")

NAICS_div <- sd_nop %>% 
  mutate(regyear = year(registrationDate)) %>% 
  filter(regyear <= 2006) %>% 
  filter(biz_size == 1 | biz_size == 0) %>% 
  group_by(regyear, NAICS2) %>% 
  dplyr::summarise(n()) 

write.csv(NAICS_div, "NAICS count reg2001-2006.csv")


##11, 21, 22, 23, 31-33, 42, 44-45, 48-49, 
##51, 52, 53, 54, 56, 61, 62, 71, 72, 81, 92
##(14, 15, 55, 99)

##11####

df2001_sd_11 <- filter(df2001_sd, NAICS2 == 11)
df2002_sd_11 <- filter(df2002_sd, NAICS2 == 11)
df2003_sd_11 <- filter(df2003_sd, NAICS2 == 11)
df2004_sd_11 <- filter(df2004_sd, NAICS2 == 11)
df2005_sd_11 <- filter(df2005_sd, NAICS2 == 11)
df2006_sd_11 <- filter(df2006_sd, NAICS2 == 11)

##3-year
sr3yr2001_sd <- sum(df2001_sd_11$years.survived>=3)/nrow(df2001_sd)
sr3yr2001sm_sd <- sum((df2001_sd_11$years.survived>=3 & df2001_sd_11$biz_size == 0), na.rm = TRUE)/sum(df2001_sd$biz_size == 0, na.rm = TRUE)
sr3yr2001non_sd <- sum((df2001_sd_11$years.survived>=3 & df2001_sd_11$biz_size == 1), na.rm = TRUE)/sum(df2001_sd$biz_size == 1, na.rm = TRUE)

sr3yr2002_sd <- sum(df2002_sd_11$years.survived>=3)/nrow(df2002_sd) 
sr3yr2002sm_sd <- sum((df2002_sd_11$years.survived>=3 & df2002_sd_11$biz_size == 0), na.rm = TRUE)/sum(df2002_sd$biz_size == 0, na.rm = TRUE)
sr3yr2002non_sd <- sum((df2002_sd_11$years.survived>=3 & df2002_sd_11$biz_size == 1), na.rm = TRUE)/sum(df2002_sd$biz_size == 1, na.rm = TRUE)

sr3yr2003_sd <- sum(df2003_sd_11$years.survived>=3)/nrow(df2003_sd)
sr3yr2003sm_sd <- sum((df2003_sd_11$years.survived>=3 & df2003_sd_11$biz_size == 0), na.rm = TRUE)/sum(df2003_sd$biz_size == 0, na.rm = TRUE)
sr3yr2003non_sd <- sum((df2003_sd_11$years.survived>=3 & df2003_sd_11$biz_size == 1), na.rm = TRUE)/sum(df2003_sd$biz_size == 1, na.rm = TRUE)

sr3yr2004_sd <- sum(df2004_sd_11$years.survived>=3)/nrow(df2004_sd) 
sr3yr2004sm_sd <- sum((df2004_sd_11$years.survived>=3 & df2004_sd_11$biz_size == 0), na.rm = TRUE)/sum(df2004_sd$biz_size == 0, na.rm = TRUE)
sr3yr2004non_sd <- sum((df2004_sd_11$years.survived>=3 & df2004_sd_11$biz_size == 1), na.rm = TRUE)/sum(df2004_sd$biz_size == 1, na.rm = TRUE)

sr3yr2005_sd <- sum(df2005_sd_11$years.survived>=3)/nrow(df2005_sd) 
sr3yr2005sm_sd <- sum((df2005_sd_11$years.survived>=3 & df2005_sd_11$biz_size == 0), na.rm = TRUE)/sum(df2005_sd$biz_size == 0, na.rm = TRUE)
sr3yr2005non_sd <- sum((df2005_sd_11$years.survived>=3 & df2005_sd_11$biz_size == 1), na.rm = TRUE)/sum(df2005_sd$biz_size == 1, na.rm = TRUE)

sr3yr2006_sd <- sum(df2006_sd_11$years.survived>=3)/nrow(df2006_sd) 
sr3yr2006sm_sd <- sum((df2006_sd_11$years.survived>=3 & df2006_sd_11$biz_size == 0), na.rm = TRUE)/sum(df2006_sd$biz_size == 0, na.rm = TRUE)
sr3yr2006non_sd <- sum((df2006_sd_11$years.survived>=3 & df2006_sd_11$biz_size == 1), na.rm = TRUE)/sum(df2006_sd$biz_size == 1, na.rm = TRUE)

##5-year
sr5yr2001_sd <- sum(df2001_sd_11$years.survived>=5)/nrow(df2001_sd)
sr5yr2001sm_sd <- sum((df2001_sd_11$years.survived>=5 & df2001_sd_11$biz_size == 0), na.rm = TRUE)/sum(df2001_sd$biz_size == 0, na.rm = TRUE)
sr5yr2001non_sd <- sum((df2001_sd_11$years.survived>=5 & df2001_sd_11$biz_size == 1), na.rm = TRUE)/sum(df2001_sd$biz_size == 1, na.rm = TRUE)

sr5yr2002_sd <- sum(df2002_sd_11$years.survived>=5)/nrow(df2002_sd) 
sr5yr2002sm_sd <- sum((df2002_sd_11$years.survived>=5 & df2002_sd_11$biz_size == 0), na.rm = TRUE)/sum(df2002_sd$biz_size == 0, na.rm = TRUE)
sr5yr2002non_sd <- sum((df2002_sd_11$years.survived>=5 & df2002_sd_11$biz_size == 1), na.rm = TRUE)/sum(df2002_sd$biz_size == 1, na.rm = TRUE)

sr5yr2003_sd <- sum(df2003_sd_11$years.survived>=5)/nrow(df2003_sd)
sr5yr2003sm_sd <- sum((df2003_sd_11$years.survived>=5 & df2003_sd_11$biz_size == 0), na.rm = TRUE)/sum(df2003_sd$biz_size == 0, na.rm = TRUE)
sr5yr2003non_sd <- sum((df2003_sd_11$years.survived>=5 & df2003_sd_11$biz_size == 1), na.rm = TRUE)/sum(df2003_sd$biz_size == 1, na.rm = TRUE)

sr5yr2004_sd <- sum(df2004_sd_11$years.survived>=5)/nrow(df2004_sd) 
sr5yr2004sm_sd <- sum((df2004_sd_11$years.survived>=5 & df2004_sd_11$biz_size == 0), na.rm = TRUE)/sum(df2004_sd$biz_size == 0, na.rm = TRUE)
sr5yr2004non_sd <- sum((df2004_sd_11$years.survived>=5 & df2004_sd_11$biz_size == 1), na.rm = TRUE)/sum(df2004_sd$biz_size == 1, na.rm = TRUE)

sr5yr2005_sd <- sum(df2005_sd_11$years.survived>=5)/nrow(df2005_sd) 
sr5yr2005sm_sd <- sum((df2005_sd_11$years.survived>=5 & df2005_sd_11$biz_size == 0), na.rm = TRUE)/sum(df2005_sd$biz_size == 0, na.rm = TRUE)
sr5yr2005non_sd <- sum((df2005_sd_11$years.survived>=5 & df2005_sd_11$biz_size == 1), na.rm = TRUE)/sum(df2005_sd$biz_size == 1, na.rm = TRUE)

sr5yr2006_sd <- sum(df2006_sd_11$years.survived>=5)/nrow(df2006_sd) 
sr5yr2006sm_sd <- sum((df2006_sd_11$years.survived>=5 & df2006_sd_11$biz_size == 0), na.rm = TRUE)/sum(df2006_sd$biz_size == 0, na.rm = TRUE)
sr5yr2006non_sd <- sum((df2006_sd_11$years.survived>=5 & df2006_sd_11$biz_size == 1), na.rm = TRUE)/sum(df2006_sd$biz_size == 1, na.rm = TRUE)


##10-year
sr10yr2001_sd <- (sum(df2001_sd_11$years.survived>=10))/nrow(df2001_sd) 
sr10yr2001sm_sd <- sum((df2001_sd_11$years.survived>=10 & df2001_sd_11$biz_size == 0), na.rm = TRUE)/sum(df2001_sd$biz_size == 0, na.rm = TRUE)
sr10yr2001non_sd <- sum((df2001_sd_11$years.survived>=10 & df2001_sd_11$biz_size == 1), na.rm = TRUE)/sum(df2002_sd$biz_size == 1, na.rm = TRUE)


sr10yr2002_sd <- sum(df2002_sd_11$years.survived>=10)/nrow(df2002_sd) 
sr10yr2002sm_sd <- sum((df2002_sd_11$years.survived>=10 & df2002_sd_11$biz_size == 0), na.rm = TRUE)/sum(df2002_sd$biz_size == 0, na.rm = TRUE)
sr10yr2002non_sd <- sum((df2002_sd_11$years.survived>=10 & df2002_sd_11$biz_size == 1), na.rm = TRUE)/sum(df2002_sd$biz_size == 1, na.rm = TRUE)

sr10yr2003_sd <- sum(df2003_sd_11$years.survived>=10)/nrow(df2003_sd)
sr10yr2003sm_sd <- sum((df2003_sd_11$years.survived>=10 & df2003_sd_11$biz_size == 0), na.rm = TRUE)/sum(df2003_sd$biz_size == 0, na.rm = TRUE)
sr10yr2003non_sd <- sum((df2003_sd_11$years.survived>=10 & df2003_sd_11$biz_size == 1), na.rm = TRUE)/sum(df2003_sd$biz_size == 1, na.rm = TRUE)

sr10yr2004_sd <- sum(df2004_sd_11$years.survived>=10)/nrow(df2004_sd) 
sr10yr2004sm_sd <- sum((df2004_sd_11$years.survived>=10 & df2004_sd_11$biz_size == 0), na.rm = TRUE)/sum(df2004_sd$biz_size == 0, na.rm = TRUE)
sr10yr2004non_sd <- sum((df2004_sd_11$years.survived>=10 & df2004_sd_11$biz_size == 1), na.rm = TRUE)/sum(df2004_sd$biz_size == 1, na.rm = TRUE)

sr10yr2005_sd <- sum(df2005_sd_11$years.survived>=10)/nrow(df2005_sd) 
sr10yr2005sm_sd <- sum((df2005_sd_11$years.survived>=10 & df2005_sd_11$biz_size == 0), na.rm = TRUE)/sum(df2005_sd$biz_size == 0, na.rm = TRUE)
sr10yr2005non_sd <- sum((df2005_sd_11$years.survived>=10 & df2005_sd_11$biz_size == 1), na.rm = TRUE)/sum(df2005_sd$biz_size == 1, na.rm = TRUE)

sr10yr2006_sd <- sum(df2006_sd_11$years.survived>=10)/nrow(df2006_sd) 
sr10yr2006sm_sd <- sum((df2006_sd_11$years.survived>=10 & df2006_sd_11$biz_size == 0), na.rm = TRUE)/sum(df2006_sd$biz_size == 0, na.rm = TRUE)
sr10yr2006non_sd <- sum((df2006_sd_11$years.survived>=10 & df2006_sd_11$biz_size == 1), na.rm = TRUE)/sum(df2006_sd$biz_size == 1, na.rm = TRUE)

##bound all survival rates together and reformated dataframe

survivalrate_sd <- cbind(sr3yr2001_sd, sr3yr2001sm_sd, sr3yr2001non_sd, sr5yr2001_sd, sr5yr2001sm_sd, sr5yr2001non_sd, 
                         sr10yr2001_sd, sr10yr2001sm_sd, sr10yr2001non_sd,
                         sr3yr2002_sd, sr3yr2002sm_sd, sr3yr2002non_sd, sr5yr2002_sd, sr5yr2002sm_sd, sr5yr2002non_sd, 
                         sr10yr2002_sd, sr10yr2002sm_sd, sr10yr2002non_sd,
                         sr3yr2003_sd, sr3yr2003sm_sd, sr3yr2003non_sd, sr5yr2003_sd, sr5yr2003sm_sd, sr5yr2003non_sd, 
                         sr10yr2003_sd, sr10yr2003sm_sd, sr10yr2003non_sd,
                         sr3yr2004_sd, sr3yr2004sm_sd, sr3yr2004non_sd, sr5yr2004_sd, sr5yr2004sm_sd, sr5yr2004non_sd, 
                         sr10yr2004_sd, sr10yr2004sm_sd, sr10yr2004non_sd,
                         sr3yr2005_sd, sr3yr2005sm_sd, sr3yr2005non_sd, sr5yr2005_sd, sr5yr2005sm_sd, sr5yr2005non_sd, 
                         sr10yr2005_sd, sr10yr2005sm_sd, sr10yr2005non_sd,
                         sr3yr2006_sd, sr3yr2006sm_sd, sr3yr2006non_sd, sr5yr2006_sd, sr5yr2006sm_sd, sr5yr2006non_sd, 
                         sr10yr2006_sd, sr10yr2006sm_sd, sr10yr2006non_sd)
srnames_sd <- c("3yr_2001_all_sd", "3yr_2001_sm_sd", 
                "3yr_2001_non_sd", "5yr_2001_all_sd", "5yr_2001_sm_sd", "5yr_2001_non_sd", "10yr_2001_all_sd", 
                "10yr_2001_sm_sd", "10yr_2001_non_sd",
                "3yr_2002_all_sd", "3yr_2002_sm_sd", "3yr_2002_non_sd", "5yr_2002_all_sd", "5yr_2002_sm_sd", "5yr_2002_non_sd", 
                "10yr_2002_all_sd", "10yr_2002_sm_sd", "10yr_2002_non_sd",
                "3yr_2003_all_sd", "3yr_2003_sm_sd", "3yr_2003_non_sd", "5yr_2003_all_sd", "5yr_2003_sm_sd", "5yr_2003_non_sd", 
                "10yr_2003_all_sd", "10yr_2003_sm_sd", "10yr_2003_non_sd",
                "3yr_2004_all_sd", "3yr_2004_sm_sd", "3yr_2004_non_sd", "5yr_2004_all_sd", "5yr_2004_sm_sd", "5yr_2004_non_sd", 
                "10yr_2004_all_sd", "10yr_2004_sm_sd", "10yr_2004_non_sd",
                "3yr_2005_all_sd", "3yr_2005_sm_sd", "3yr_2005_non_sd", "5yr_2005_all_sd", "5yr_2005_sm_sd", "5yr_2005_non_sd", 
                "10yr2005_all_sd", "10yr_2005_sm_sd", "10yr_2005_non_sd",
                "3yr_2006_all_sd", "3yr_2006_sm_sd", "3yr_2006_non_sd", "5yr_2006_all_sd", "5yr_2006_sm_sd", "5yr_2006_non_sd", 
                "10yr_2006_all_sd", "10yr_2006_sm_sd", "10yr_2006_non_sd")
colnames(survivalrate_sd) <- srnames_sd

sr_sd <- as.data.frame(survivalrate_sd)

survival.rates_sd <- sr_sd %>% 
  gather("3yr_2001_all_sd":"10yr_2006_non_sd", key = "time_year_type", value = "survivalrate_sd") %>% 
  separate(time_year_type, into = c("time", "year", "type"), sep = "_") %>% 
  arrange(time, year) %>% 
  unite("time_type", time, type, sep = "_") 

survival.rates_sd$time_type[18] <- "10yr_all"
survival.rates_sd$year[18] <- 2005

survival.rates_sd_11 <- survival.rates_sd %>% 
  spread(key = "time_type", value = "survivalrate_sd")

survival.rates_sd_11[, c(1, 5, 7, 6, 8, 10, 9, 2, 4, 3)]

##21####

df2001_sd_21 <- filter(df2001_sd, NAICS2 == 21)
df2002_sd_21 <- filter(df2002_sd, NAICS2 == 21)
df2003_sd_21 <- filter(df2003_sd, NAICS2 == 21)
df2004_sd_21 <- filter(df2004_sd, NAICS2 == 21)
df2005_sd_21 <- filter(df2005_sd, NAICS2 == 21)
df2006_sd_21 <- filter(df2006_sd, NAICS2 == 21)

##3-year
sr3yr2001_sd <- sum(df2001_sd_21$years.survived>=3)/nrow(df2001_sd)
sr3yr2001sm_sd <- sum((df2001_sd_21$years.survived>=3 & df2001_sd_21$biz_size == 0), na.rm = TRUE)/sum(df2001_sd$biz_size == 0, na.rm = TRUE)
sr3yr2001non_sd <- sum((df2001_sd_21$years.survived>=3 & df2001_sd_21$biz_size == 1), na.rm = TRUE)/sum(df2001_sd$biz_size == 1, na.rm = TRUE)

sr3yr2002_sd <- sum(df2002_sd_21$years.survived>=3)/nrow(df2002_sd) 
sr3yr2002sm_sd <- sum((df2002_sd_21$years.survived>=3 & df2002_sd_21$biz_size == 0), na.rm = TRUE)/sum(df2002_sd$biz_size == 0, na.rm = TRUE)
sr3yr2002non_sd <- sum((df2002_sd_21$years.survived>=3 & df2002_sd_21$biz_size == 1), na.rm = TRUE)/sum(df2002_sd$biz_size == 1, na.rm = TRUE)

sr3yr2003_sd <- sum(df2003_sd_21$years.survived>=3)/nrow(df2003_sd)
sr3yr2003sm_sd <- sum((df2003_sd_21$years.survived>=3 & df2003_sd_21$biz_size == 0), na.rm = TRUE)/sum(df2003_sd$biz_size == 0, na.rm = TRUE)
sr3yr2003non_sd <- sum((df2003_sd_21$years.survived>=3 & df2003_sd_21$biz_size == 1), na.rm = TRUE)/sum(df2003_sd$biz_size == 1, na.rm = TRUE)

sr3yr2004_sd <- sum(df2004_sd_21$years.survived>=3)/nrow(df2004_sd) 
sr3yr2004sm_sd <- sum((df2004_sd_21$years.survived>=3 & df2004_sd_21$biz_size == 0), na.rm = TRUE)/sum(df2004_sd$biz_size == 0, na.rm = TRUE)
sr3yr2004non_sd <- sum((df2004_sd_21$years.survived>=3 & df2004_sd_21$biz_size == 1), na.rm = TRUE)/sum(df2004_sd$biz_size == 1, na.rm = TRUE)

sr3yr2005_sd <- sum(df2005_sd_21$years.survived>=3)/nrow(df2005_sd) 
sr3yr2005sm_sd <- sum((df2005_sd_21$years.survived>=3 & df2005_sd_21$biz_size == 0), na.rm = TRUE)/sum(df2005_sd$biz_size == 0, na.rm = TRUE)
sr3yr2005non_sd <- sum((df2005_sd_21$years.survived>=3 & df2005_sd_21$biz_size == 1), na.rm = TRUE)/sum(df2005_sd$biz_size == 1, na.rm = TRUE)

sr3yr2006_sd <- sum(df2006_sd_21$years.survived>=3)/nrow(df2006_sd) 
sr3yr2006sm_sd <- sum((df2006_sd_21$years.survived>=3 & df2006_sd_21$biz_size == 0), na.rm = TRUE)/sum(df2006_sd$biz_size == 0, na.rm = TRUE)
sr3yr2006non_sd <- sum((df2006_sd_21$years.survived>=3 & df2006_sd_21$biz_size == 1), na.rm = TRUE)/sum(df2006_sd$biz_size == 1, na.rm = TRUE)

##5-year
sr5yr2001_sd <- sum(df2001_sd_21$years.survived>=5)/nrow(df2001_sd)
sr5yr2001sm_sd <- sum((df2001_sd_21$years.survived>=5 & df2001_sd_21$biz_size == 0), na.rm = TRUE)/sum(df2001_sd$biz_size == 0, na.rm = TRUE)
sr5yr2001non_sd <- sum((df2001_sd_21$years.survived>=5 & df2001_sd_21$biz_size == 1), na.rm = TRUE)/sum(df2001_sd$biz_size == 1, na.rm = TRUE)

sr5yr2002_sd <- sum(df2002_sd_21$years.survived>=5)/nrow(df2002_sd) 
sr5yr2002sm_sd <- sum((df2002_sd_21$years.survived>=5 & df2002_sd_21$biz_size == 0), na.rm = TRUE)/sum(df2002_sd$biz_size == 0, na.rm = TRUE)
sr5yr2002non_sd <- sum((df2002_sd_21$years.survived>=5 & df2002_sd_21$biz_size == 1), na.rm = TRUE)/sum(df2002_sd$biz_size == 1, na.rm = TRUE)

sr5yr2003_sd <- sum(df2003_sd_21$years.survived>=5)/nrow(df2003_sd)
sr5yr2003sm_sd <- sum((df2003_sd_21$years.survived>=5 & df2003_sd_21$biz_size == 0), na.rm = TRUE)/sum(df2003_sd$biz_size == 0, na.rm = TRUE)
sr5yr2003non_sd <- sum((df2003_sd_21$years.survived>=5 & df2003_sd_21$biz_size == 1), na.rm = TRUE)/sum(df2003_sd$biz_size == 1, na.rm = TRUE)

sr5yr2004_sd <- sum(df2004_sd_21$years.survived>=5)/nrow(df2004_sd) 
sr5yr2004sm_sd <- sum((df2004_sd_21$years.survived>=5 & df2004_sd_21$biz_size == 0), na.rm = TRUE)/sum(df2004_sd$biz_size == 0, na.rm = TRUE)
sr5yr2004non_sd <- sum((df2004_sd_21$years.survived>=5 & df2004_sd_21$biz_size == 1), na.rm = TRUE)/sum(df2004_sd$biz_size == 1, na.rm = TRUE)

sr5yr2005_sd <- sum(df2005_sd_21$years.survived>=5)/nrow(df2005_sd) 
sr5yr2005sm_sd <- sum((df2005_sd_21$years.survived>=5 & df2005_sd_21$biz_size == 0), na.rm = TRUE)/sum(df2005_sd$biz_size == 0, na.rm = TRUE)
sr5yr2005non_sd <- sum((df2005_sd_21$years.survived>=5 & df2005_sd_21$biz_size == 1), na.rm = TRUE)/sum(df2005_sd$biz_size == 1, na.rm = TRUE)

sr5yr2006_sd <- sum(df2006_sd_21$years.survived>=5)/nrow(df2006_sd) 
sr5yr2006sm_sd <- sum((df2006_sd_21$years.survived>=5 & df2006_sd_21$biz_size == 0), na.rm = TRUE)/sum(df2006_sd$biz_size == 0, na.rm = TRUE)
sr5yr2006non_sd <- sum((df2006_sd_21$years.survived>=5 & df2006_sd_21$biz_size == 1), na.rm = TRUE)/sum(df2006_sd$biz_size == 1, na.rm = TRUE)


##10-year
sr10yr2001_sd <- (sum(df2001_sd_21$years.survived>=10))/nrow(df2001_sd) 
sr10yr2001sm_sd <- sum((df2001_sd_21$years.survived>=10 & df2001_sd_21$biz_size == 0), na.rm = TRUE)/sum(df2001_sd$biz_size == 0, na.rm = TRUE)
sr10yr2001non_sd <- sum((df2001_sd_21$years.survived>=10 & df2001_sd_21$biz_size == 1), na.rm = TRUE)/sum(df2002_sd$biz_size == 1, na.rm = TRUE)


sr10yr2002_sd <- sum(df2002_sd_21$years.survived>=10)/nrow(df2002_sd) 
sr10yr2002sm_sd <- sum((df2002_sd_21$years.survived>=10 & df2002_sd_21$biz_size == 0), na.rm = TRUE)/sum(df2002_sd$biz_size == 0, na.rm = TRUE)
sr10yr2002non_sd <- sum((df2002_sd_21$years.survived>=10 & df2002_sd_21$biz_size == 1), na.rm = TRUE)/sum(df2002_sd$biz_size == 1, na.rm = TRUE)

sr10yr2003_sd <- sum(df2003_sd_21$years.survived>=10)/nrow(df2003_sd)
sr10yr2003sm_sd <- sum((df2003_sd_21$years.survived>=10 & df2003_sd_21$biz_size == 0), na.rm = TRUE)/sum(df2003_sd$biz_size == 0, na.rm = TRUE)
sr10yr2003non_sd <- sum((df2003_sd_21$years.survived>=10 & df2003_sd_21$biz_size == 1), na.rm = TRUE)/sum(df2003_sd$biz_size == 1, na.rm = TRUE)

sr10yr2004_sd <- sum(df2004_sd_21$years.survived>=10)/nrow(df2004_sd) 
sr10yr2004sm_sd <- sum((df2004_sd_21$years.survived>=10 & df2004_sd_21$biz_size == 0), na.rm = TRUE)/sum(df2004_sd$biz_size == 0, na.rm = TRUE)
sr10yr2004non_sd <- sum((df2004_sd_21$years.survived>=10 & df2004_sd_21$biz_size == 1), na.rm = TRUE)/sum(df2004_sd$biz_size == 1, na.rm = TRUE)

sr10yr2005_sd <- sum(df2005_sd_21$years.survived>=10)/nrow(df2005_sd) 
sr10yr2005sm_sd <- sum((df2005_sd_21$years.survived>=10 & df2005_sd_21$biz_size == 0), na.rm = TRUE)/sum(df2005_sd$biz_size == 0, na.rm = TRUE)
sr10yr2005non_sd <- sum((df2005_sd_21$years.survived>=10 & df2005_sd_21$biz_size == 1), na.rm = TRUE)/sum(df2005_sd$biz_size == 1, na.rm = TRUE)

sr10yr2006_sd <- sum(df2006_sd_21$years.survived>=10)/nrow(df2006_sd) 
sr10yr2006sm_sd <- sum((df2006_sd_21$years.survived>=10 & df2006_sd_21$biz_size == 0), na.rm = TRUE)/sum(df2006_sd$biz_size == 0, na.rm = TRUE)
sr10yr2006non_sd <- sum((df2006_sd_21$years.survived>=10 & df2006_sd_21$biz_size == 1), na.rm = TRUE)/sum(df2006_sd$biz_size == 1, na.rm = TRUE)

##bound all survival rates together and reformated dataframe

survivalrate_sd <- cbind(sr3yr2001_sd, sr3yr2001sm_sd, sr3yr2001non_sd, sr5yr2001_sd, sr5yr2001sm_sd, sr5yr2001non_sd, 
                         sr10yr2001_sd, sr10yr2001sm_sd, sr10yr2001non_sd,
                         sr3yr2002_sd, sr3yr2002sm_sd, sr3yr2002non_sd, sr5yr2002_sd, sr5yr2002sm_sd, sr5yr2002non_sd, 
                         sr10yr2002_sd, sr10yr2002sm_sd, sr10yr2002non_sd,
                         sr3yr2003_sd, sr3yr2003sm_sd, sr3yr2003non_sd, sr5yr2003_sd, sr5yr2003sm_sd, sr5yr2003non_sd, 
                         sr10yr2003_sd, sr10yr2003sm_sd, sr10yr2003non_sd,
                         sr3yr2004_sd, sr3yr2004sm_sd, sr3yr2004non_sd, sr5yr2004_sd, sr5yr2004sm_sd, sr5yr2004non_sd, 
                         sr10yr2004_sd, sr10yr2004sm_sd, sr10yr2004non_sd,
                         sr3yr2005_sd, sr3yr2005sm_sd, sr3yr2005non_sd, sr5yr2005_sd, sr5yr2005sm_sd, sr5yr2005non_sd, 
                         sr10yr2005_sd, sr10yr2005sm_sd, sr10yr2005non_sd,
                         sr3yr2006_sd, sr3yr2006sm_sd, sr3yr2006non_sd, sr5yr2006_sd, sr5yr2006sm_sd, sr5yr2006non_sd, 
                         sr10yr2006_sd, sr10yr2006sm_sd, sr10yr2006non_sd)
srnames_sd <- c("3yr_2001_all_sd", "3yr_2001_sm_sd", 
                "3yr_2001_non_sd", "5yr_2001_all_sd", "5yr_2001_sm_sd", "5yr_2001_non_sd", "10yr_2001_all_sd", 
                "10yr_2001_sm_sd", "10yr_2001_non_sd",
                "3yr_2002_all_sd", "3yr_2002_sm_sd", "3yr_2002_non_sd", "5yr_2002_all_sd", "5yr_2002_sm_sd", "5yr_2002_non_sd", 
                "10yr_2002_all_sd", "10yr_2002_sm_sd", "10yr_2002_non_sd",
                "3yr_2003_all_sd", "3yr_2003_sm_sd", "3yr_2003_non_sd", "5yr_2003_all_sd", "5yr_2003_sm_sd", "5yr_2003_non_sd", 
                "10yr_2003_all_sd", "10yr_2003_sm_sd", "10yr_2003_non_sd",
                "3yr_2004_all_sd", "3yr_2004_sm_sd", "3yr_2004_non_sd", "5yr_2004_all_sd", "5yr_2004_sm_sd", "5yr_2004_non_sd", 
                "10yr_2004_all_sd", "10yr_2004_sm_sd", "10yr_2004_non_sd",
                "3yr_2005_all_sd", "3yr_2005_sm_sd", "3yr_2005_non_sd", "5yr_2005_all_sd", "5yr_2005_sm_sd", "5yr_2005_non_sd", 
                "10yr2005_all_sd", "10yr_2005_sm_sd", "10yr_2005_non_sd",
                "3yr_2006_all_sd", "3yr_2006_sm_sd", "3yr_2006_non_sd", "5yr_2006_all_sd", "5yr_2006_sm_sd", "5yr_2006_non_sd", 
                "10yr_2006_all_sd", "10yr_2006_sm_sd", "10yr_2006_non_sd")
colnames(survivalrate_sd) <- srnames_sd

sr_sd <- as.data.frame(survivalrate_sd)

survival.rates_sd <- sr_sd %>% 
  gather("3yr_2001_all_sd":"10yr_2006_non_sd", key = "time_year_type", value = "survivalrate_sd") %>% 
  separate(time_year_type, into = c("time", "year", "type"), sep = "_") %>% 
  arrange(time, year) %>% 
  unite("time_type", time, type, sep = "_") 

survival.rates_sd$time_type[18] <- "10yr_all"
survival.rates_sd$year[18] <- 2005

survival.rates_sd_21  <- survival.rates_sd %>% 
  spread(key = "time_type", value = "survivalrate_sd")

survival.rates_sd_21[, c(1, 5, 7, 6, 8, 10, 9, 2, 4, 3)]

##22####

df2001_sd_22 <- filter(df2001_sd, NAICS2 == 22)
df2002_sd_22 <- filter(df2002_sd, NAICS2 == 22)
df2003_sd_22 <- filter(df2003_sd, NAICS2 == 22)
df2004_sd_22 <- filter(df2004_sd, NAICS2 == 22)
df2005_sd_22 <- filter(df2005_sd, NAICS2 == 22)
df2006_sd_22 <- filter(df2006_sd, NAICS2 == 22)

##3-year
sr3yr2001_sd <- sum(df2001_sd_22$years.survived>=3)/nrow(df2001_sd)
sr3yr2001sm_sd <- sum((df2001_sd_22$years.survived>=3 & df2001_sd_22$biz_size == 0), na.rm = TRUE)/sum(df2001_sd$biz_size == 0, na.rm = TRUE)
sr3yr2001non_sd <- sum((df2001_sd_22$years.survived>=3 & df2001_sd_22$biz_size == 1), na.rm = TRUE)/sum(df2001_sd$biz_size == 1, na.rm = TRUE)

sr3yr2002_sd <- sum(df2002_sd_22$years.survived>=3)/nrow(df2002_sd) 
sr3yr2002sm_sd <- sum((df2002_sd_22$years.survived>=3 & df2002_sd_22$biz_size == 0), na.rm = TRUE)/sum(df2002_sd$biz_size == 0, na.rm = TRUE)
sr3yr2002non_sd <- sum((df2002_sd_22$years.survived>=3 & df2002_sd_22$biz_size == 1), na.rm = TRUE)/sum(df2002_sd$biz_size == 1, na.rm = TRUE)

sr3yr2003_sd <- sum(df2003_sd_22$years.survived>=3)/nrow(df2003_sd)
sr3yr2003sm_sd <- sum((df2003_sd_22$years.survived>=3 & df2003_sd_22$biz_size == 0), na.rm = TRUE)/sum(df2003_sd$biz_size == 0, na.rm = TRUE)
sr3yr2003non_sd <- sum((df2003_sd_22$years.survived>=3 & df2003_sd_22$biz_size == 1), na.rm = TRUE)/sum(df2003_sd$biz_size == 1, na.rm = TRUE)

sr3yr2004_sd <- sum(df2004_sd_22$years.survived>=3)/nrow(df2004_sd) 
sr3yr2004sm_sd <- sum((df2004_sd_22$years.survived>=3 & df2004_sd_22$biz_size == 0), na.rm = TRUE)/sum(df2004_sd$biz_size == 0, na.rm = TRUE)
sr3yr2004non_sd <- sum((df2004_sd_22$years.survived>=3 & df2004_sd_22$biz_size == 1), na.rm = TRUE)/sum(df2004_sd$biz_size == 1, na.rm = TRUE)

sr3yr2005_sd <- sum(df2005_sd_22$years.survived>=3)/nrow(df2005_sd) 
sr3yr2005sm_sd <- sum((df2005_sd_22$years.survived>=3 & df2005_sd_22$biz_size == 0), na.rm = TRUE)/sum(df2005_sd$biz_size == 0, na.rm = TRUE)
sr3yr2005non_sd <- sum((df2005_sd_22$years.survived>=3 & df2005_sd_22$biz_size == 1), na.rm = TRUE)/sum(df2005_sd$biz_size == 1, na.rm = TRUE)

sr3yr2006_sd <- sum(df2006_sd_22$years.survived>=3)/nrow(df2006_sd) 
sr3yr2006sm_sd <- sum((df2006_sd_22$years.survived>=3 & df2006_sd_22$biz_size == 0), na.rm = TRUE)/sum(df2006_sd$biz_size == 0, na.rm = TRUE)
sr3yr2006non_sd <- sum((df2006_sd_22$years.survived>=3 & df2006_sd_22$biz_size == 1), na.rm = TRUE)/sum(df2006_sd$biz_size == 1, na.rm = TRUE)

##5-year
sr5yr2001_sd <- sum(df2001_sd_22$years.survived>=5)/nrow(df2001_sd)
sr5yr2001sm_sd <- sum((df2001_sd_22$years.survived>=5 & df2001_sd_22$biz_size == 0), na.rm = TRUE)/sum(df2001_sd$biz_size == 0, na.rm = TRUE)
sr5yr2001non_sd <- sum((df2001_sd_22$years.survived>=5 & df2001_sd_22$biz_size == 1), na.rm = TRUE)/sum(df2001_sd$biz_size == 1, na.rm = TRUE)

sr5yr2002_sd <- sum(df2002_sd_22$years.survived>=5)/nrow(df2002_sd) 
sr5yr2002sm_sd <- sum((df2002_sd_22$years.survived>=5 & df2002_sd_22$biz_size == 0), na.rm = TRUE)/sum(df2002_sd$biz_size == 0, na.rm = TRUE)
sr5yr2002non_sd <- sum((df2002_sd_22$years.survived>=5 & df2002_sd_22$biz_size == 1), na.rm = TRUE)/sum(df2002_sd$biz_size == 1, na.rm = TRUE)

sr5yr2003_sd <- sum(df2003_sd_22$years.survived>=5)/nrow(df2003_sd)
sr5yr2003sm_sd <- sum((df2003_sd_22$years.survived>=5 & df2003_sd_22$biz_size == 0), na.rm = TRUE)/sum(df2003_sd$biz_size == 0, na.rm = TRUE)
sr5yr2003non_sd <- sum((df2003_sd_22$years.survived>=5 & df2003_sd_22$biz_size == 1), na.rm = TRUE)/sum(df2003_sd$biz_size == 1, na.rm = TRUE)

sr5yr2004_sd <- sum(df2004_sd_22$years.survived>=5)/nrow(df2004_sd) 
sr5yr2004sm_sd <- sum((df2004_sd_22$years.survived>=5 & df2004_sd_22$biz_size == 0), na.rm = TRUE)/sum(df2004_sd$biz_size == 0, na.rm = TRUE)
sr5yr2004non_sd <- sum((df2004_sd_22$years.survived>=5 & df2004_sd_22$biz_size == 1), na.rm = TRUE)/sum(df2004_sd$biz_size == 1, na.rm = TRUE)

sr5yr2005_sd <- sum(df2005_sd_22$years.survived>=5)/nrow(df2005_sd) 
sr5yr2005sm_sd <- sum((df2005_sd_22$years.survived>=5 & df2005_sd_22$biz_size == 0), na.rm = TRUE)/sum(df2005_sd$biz_size == 0, na.rm = TRUE)
sr5yr2005non_sd <- sum((df2005_sd_22$years.survived>=5 & df2005_sd_22$biz_size == 1), na.rm = TRUE)/sum(df2005_sd$biz_size == 1, na.rm = TRUE)

sr5yr2006_sd <- sum(df2006_sd_22$years.survived>=5)/nrow(df2006_sd) 
sr5yr2006sm_sd <- sum((df2006_sd_22$years.survived>=5 & df2006_sd_22$biz_size == 0), na.rm = TRUE)/sum(df2006_sd$biz_size == 0, na.rm = TRUE)
sr5yr2006non_sd <- sum((df2006_sd_22$years.survived>=5 & df2006_sd_22$biz_size == 1), na.rm = TRUE)/sum(df2006_sd$biz_size == 1, na.rm = TRUE)


##10-year
sr10yr2001_sd <- (sum(df2001_sd_22$years.survived>=10))/nrow(df2001_sd) 
sr10yr2001sm_sd <- sum((df2001_sd_22$years.survived>=10 & df2001_sd_22$biz_size == 0), na.rm = TRUE)/sum(df2001_sd$biz_size == 0, na.rm = TRUE)
sr10yr2001non_sd <- sum((df2001_sd_22$years.survived>=10 & df2001_sd_22$biz_size == 1), na.rm = TRUE)/sum(df2002_sd$biz_size == 1, na.rm = TRUE)


sr10yr2002_sd <- sum(df2002_sd_22$years.survived>=10)/nrow(df2002_sd) 
sr10yr2002sm_sd <- sum((df2002_sd_22$years.survived>=10 & df2002_sd_22$biz_size == 0), na.rm = TRUE)/sum(df2002_sd$biz_size == 0, na.rm = TRUE)
sr10yr2002non_sd <- sum((df2002_sd_22$years.survived>=10 & df2002_sd_22$biz_size == 1), na.rm = TRUE)/sum(df2002_sd$biz_size == 1, na.rm = TRUE)

sr10yr2003_sd <- sum(df2003_sd_22$years.survived>=10)/nrow(df2003_sd)
sr10yr2003sm_sd <- sum((df2003_sd_22$years.survived>=10 & df2003_sd_22$biz_size == 0), na.rm = TRUE)/sum(df2003_sd$biz_size == 0, na.rm = TRUE)
sr10yr2003non_sd <- sum((df2003_sd_22$years.survived>=10 & df2003_sd_22$biz_size == 1), na.rm = TRUE)/sum(df2003_sd$biz_size == 1, na.rm = TRUE)

sr10yr2004_sd <- sum(df2004_sd_22$years.survived>=10)/nrow(df2004_sd) 
sr10yr2004sm_sd <- sum((df2004_sd_22$years.survived>=10 & df2004_sd_22$biz_size == 0), na.rm = TRUE)/sum(df2004_sd$biz_size == 0, na.rm = TRUE)
sr10yr2004non_sd <- sum((df2004_sd_22$years.survived>=10 & df2004_sd_22$biz_size == 1), na.rm = TRUE)/sum(df2004_sd$biz_size == 1, na.rm = TRUE)

sr10yr2005_sd <- sum(df2005_sd_22$years.survived>=10)/nrow(df2005_sd) 
sr10yr2005sm_sd <- sum((df2005_sd_22$years.survived>=10 & df2005_sd_22$biz_size == 0), na.rm = TRUE)/sum(df2005_sd$biz_size == 0, na.rm = TRUE)
sr10yr2005non_sd <- sum((df2005_sd_22$years.survived>=10 & df2005_sd_22$biz_size == 1), na.rm = TRUE)/sum(df2005_sd$biz_size == 1, na.rm = TRUE)

sr10yr2006_sd <- sum(df2006_sd_22$years.survived>=10)/nrow(df2006_sd) 
sr10yr2006sm_sd <- sum((df2006_sd_22$years.survived>=10 & df2006_sd_22$biz_size == 0), na.rm = TRUE)/sum(df2006_sd$biz_size == 0, na.rm = TRUE)
sr10yr2006non_sd <- sum((df2006_sd_22$years.survived>=10 & df2006_sd_22$biz_size == 1), na.rm = TRUE)/sum(df2006_sd$biz_size == 1, na.rm = TRUE)

##bound all survival rates together and reformated dataframe

survivalrate_sd <- cbind(sr3yr2001_sd, sr3yr2001sm_sd, sr3yr2001non_sd, sr5yr2001_sd, sr5yr2001sm_sd, sr5yr2001non_sd, 
                         sr10yr2001_sd, sr10yr2001sm_sd, sr10yr2001non_sd,
                         sr3yr2002_sd, sr3yr2002sm_sd, sr3yr2002non_sd, sr5yr2002_sd, sr5yr2002sm_sd, sr5yr2002non_sd, 
                         sr10yr2002_sd, sr10yr2002sm_sd, sr10yr2002non_sd,
                         sr3yr2003_sd, sr3yr2003sm_sd, sr3yr2003non_sd, sr5yr2003_sd, sr5yr2003sm_sd, sr5yr2003non_sd, 
                         sr10yr2003_sd, sr10yr2003sm_sd, sr10yr2003non_sd,
                         sr3yr2004_sd, sr3yr2004sm_sd, sr3yr2004non_sd, sr5yr2004_sd, sr5yr2004sm_sd, sr5yr2004non_sd, 
                         sr10yr2004_sd, sr10yr2004sm_sd, sr10yr2004non_sd,
                         sr3yr2005_sd, sr3yr2005sm_sd, sr3yr2005non_sd, sr5yr2005_sd, sr5yr2005sm_sd, sr5yr2005non_sd, 
                         sr10yr2005_sd, sr10yr2005sm_sd, sr10yr2005non_sd,
                         sr3yr2006_sd, sr3yr2006sm_sd, sr3yr2006non_sd, sr5yr2006_sd, sr5yr2006sm_sd, sr5yr2006non_sd, 
                         sr10yr2006_sd, sr10yr2006sm_sd, sr10yr2006non_sd)
srnames_sd <- c("3yr_2001_all_sd", "3yr_2001_sm_sd", 
                "3yr_2001_non_sd", "5yr_2001_all_sd", "5yr_2001_sm_sd", "5yr_2001_non_sd", "10yr_2001_all_sd", 
                "10yr_2001_sm_sd", "10yr_2001_non_sd",
                "3yr_2002_all_sd", "3yr_2002_sm_sd", "3yr_2002_non_sd", "5yr_2002_all_sd", "5yr_2002_sm_sd", "5yr_2002_non_sd", 
                "10yr_2002_all_sd", "10yr_2002_sm_sd", "10yr_2002_non_sd",
                "3yr_2003_all_sd", "3yr_2003_sm_sd", "3yr_2003_non_sd", "5yr_2003_all_sd", "5yr_2003_sm_sd", "5yr_2003_non_sd", 
                "10yr_2003_all_sd", "10yr_2003_sm_sd", "10yr_2003_non_sd",
                "3yr_2004_all_sd", "3yr_2004_sm_sd", "3yr_2004_non_sd", "5yr_2004_all_sd", "5yr_2004_sm_sd", "5yr_2004_non_sd", 
                "10yr_2004_all_sd", "10yr_2004_sm_sd", "10yr_2004_non_sd",
                "3yr_2005_all_sd", "3yr_2005_sm_sd", "3yr_2005_non_sd", "5yr_2005_all_sd", "5yr_2005_sm_sd", "5yr_2005_non_sd", 
                "10yr2005_all_sd", "10yr_2005_sm_sd", "10yr_2005_non_sd",
                "3yr_2006_all_sd", "3yr_2006_sm_sd", "3yr_2006_non_sd", "5yr_2006_all_sd", "5yr_2006_sm_sd", "5yr_2006_non_sd", 
                "10yr_2006_all_sd", "10yr_2006_sm_sd", "10yr_2006_non_sd")
colnames(survivalrate_sd) <- srnames_sd

sr_sd <- as.data.frame(survivalrate_sd)

survival.rates_sd <- sr_sd %>% 
  gather("3yr_2001_all_sd":"10yr_2006_non_sd", key = "time_year_type", value = "survivalrate_sd") %>% 
  separate(time_year_type, into = c("time", "year", "type"), sep = "_") %>% 
  arrange(time, year) %>% 
  unite("time_type", time, type, sep = "_") 

survival.rates_sd$time_type[18] <- "10yr_all"
survival.rates_sd$year[18] <- 2005

survival.rates_sd_22 <- survival.rates_sd %>% 
  spread(key = "time_type", value = "survivalrate_sd")

survival.rates_sd_22[, c(1, 5, 7, 6, 8, 10, 9, 2, 4, 3)]

##23####

df2001_sd_23 <- filter(df2001_sd, NAICS2 == 23)
df2002_sd_23 <- filter(df2002_sd, NAICS2 == 23)
df2003_sd_23 <- filter(df2003_sd, NAICS2 == 23)
df2004_sd_23 <- filter(df2004_sd, NAICS2 == 23)
df2005_sd_23 <- filter(df2005_sd, NAICS2 == 23)
df2006_sd_23 <- filter(df2006_sd, NAICS2 == 23)

##3-year
sr3yr2001_sd <- sum(df2001_sd_23$years.survived>=3)/nrow(df2001_sd)
sr3yr2001sm_sd <- sum((df2001_sd_23$years.survived>=3 & df2001_sd_23$biz_size == 0), na.rm = TRUE)/sum(df2001_sd$biz_size == 0, na.rm = TRUE)
sr3yr2001non_sd <- sum((df2001_sd_23$years.survived>=3 & df2001_sd_23$biz_size == 1), na.rm = TRUE)/sum(df2001_sd$biz_size == 1, na.rm = TRUE)

sr3yr2002_sd <- sum(df2002_sd_23$years.survived>=3)/nrow(df2002_sd) 
sr3yr2002sm_sd <- sum((df2002_sd_23$years.survived>=3 & df2002_sd_23$biz_size == 0), na.rm = TRUE)/sum(df2002_sd$biz_size == 0, na.rm = TRUE)
sr3yr2002non_sd <- sum((df2002_sd_23$years.survived>=3 & df2002_sd_23$biz_size == 1), na.rm = TRUE)/sum(df2002_sd$biz_size == 1, na.rm = TRUE)

sr3yr2003_sd <- sum(df2003_sd_23$years.survived>=3)/nrow(df2003_sd)
sr3yr2003sm_sd <- sum((df2003_sd_23$years.survived>=3 & df2003_sd_23$biz_size == 0), na.rm = TRUE)/sum(df2003_sd$biz_size == 0, na.rm = TRUE)
sr3yr2003non_sd <- sum((df2003_sd_23$years.survived>=3 & df2003_sd_23$biz_size == 1), na.rm = TRUE)/sum(df2003_sd$biz_size == 1, na.rm = TRUE)

sr3yr2004_sd <- sum(df2004_sd_23$years.survived>=3)/nrow(df2004_sd) 
sr3yr2004sm_sd <- sum((df2004_sd_23$years.survived>=3 & df2004_sd_23$biz_size == 0), na.rm = TRUE)/sum(df2004_sd$biz_size == 0, na.rm = TRUE)
sr3yr2004non_sd <- sum((df2004_sd_23$years.survived>=3 & df2004_sd_23$biz_size == 1), na.rm = TRUE)/sum(df2004_sd$biz_size == 1, na.rm = TRUE)

sr3yr2005_sd <- sum(df2005_sd_23$years.survived>=3)/nrow(df2005_sd) 
sr3yr2005sm_sd <- sum((df2005_sd_23$years.survived>=3 & df2005_sd_23$biz_size == 0), na.rm = TRUE)/sum(df2005_sd$biz_size == 0, na.rm = TRUE)
sr3yr2005non_sd <- sum((df2005_sd_23$years.survived>=3 & df2005_sd_23$biz_size == 1), na.rm = TRUE)/sum(df2005_sd$biz_size == 1, na.rm = TRUE)

sr3yr2006_sd <- sum(df2006_sd_23$years.survived>=3)/nrow(df2006_sd) 
sr3yr2006sm_sd <- sum((df2006_sd_23$years.survived>=3 & df2006_sd_23$biz_size == 0), na.rm = TRUE)/sum(df2006_sd$biz_size == 0, na.rm = TRUE)
sr3yr2006non_sd <- sum((df2006_sd_23$years.survived>=3 & df2006_sd_23$biz_size == 1), na.rm = TRUE)/sum(df2006_sd$biz_size == 1, na.rm = TRUE)

##5-year
sr5yr2001_sd <- sum(df2001_sd_23$years.survived>=5)/nrow(df2001_sd)
sr5yr2001sm_sd <- sum((df2001_sd_23$years.survived>=5 & df2001_sd_23$biz_size == 0), na.rm = TRUE)/sum(df2001_sd$biz_size == 0, na.rm = TRUE)
sr5yr2001non_sd <- sum((df2001_sd_23$years.survived>=5 & df2001_sd_23$biz_size == 1), na.rm = TRUE)/sum(df2001_sd$biz_size == 1, na.rm = TRUE)

sr5yr2002_sd <- sum(df2002_sd_23$years.survived>=5)/nrow(df2002_sd) 
sr5yr2002sm_sd <- sum((df2002_sd_23$years.survived>=5 & df2002_sd_23$biz_size == 0), na.rm = TRUE)/sum(df2002_sd$biz_size == 0, na.rm = TRUE)
sr5yr2002non_sd <- sum((df2002_sd_23$years.survived>=5 & df2002_sd_23$biz_size == 1), na.rm = TRUE)/sum(df2002_sd$biz_size == 1, na.rm = TRUE)

sr5yr2003_sd <- sum(df2003_sd_23$years.survived>=5)/nrow(df2003_sd)
sr5yr2003sm_sd <- sum((df2003_sd_23$years.survived>=5 & df2003_sd_23$biz_size == 0), na.rm = TRUE)/sum(df2003_sd$biz_size == 0, na.rm = TRUE)
sr5yr2003non_sd <- sum((df2003_sd_23$years.survived>=5 & df2003_sd_23$biz_size == 1), na.rm = TRUE)/sum(df2003_sd$biz_size == 1, na.rm = TRUE)

sr5yr2004_sd <- sum(df2004_sd_23$years.survived>=5)/nrow(df2004_sd) 
sr5yr2004sm_sd <- sum((df2004_sd_23$years.survived>=5 & df2004_sd_23$biz_size == 0), na.rm = TRUE)/sum(df2004_sd$biz_size == 0, na.rm = TRUE)
sr5yr2004non_sd <- sum((df2004_sd_23$years.survived>=5 & df2004_sd_23$biz_size == 1), na.rm = TRUE)/sum(df2004_sd$biz_size == 1, na.rm = TRUE)

sr5yr2005_sd <- sum(df2005_sd_23$years.survived>=5)/nrow(df2005_sd) 
sr5yr2005sm_sd <- sum((df2005_sd_23$years.survived>=5 & df2005_sd_23$biz_size == 0), na.rm = TRUE)/sum(df2005_sd$biz_size == 0, na.rm = TRUE)
sr5yr2005non_sd <- sum((df2005_sd_23$years.survived>=5 & df2005_sd_23$biz_size == 1), na.rm = TRUE)/sum(df2005_sd$biz_size == 1, na.rm = TRUE)

sr5yr2006_sd <- sum(df2006_sd_23$years.survived>=5)/nrow(df2006_sd) 
sr5yr2006sm_sd <- sum((df2006_sd_23$years.survived>=5 & df2006_sd_23$biz_size == 0), na.rm = TRUE)/sum(df2006_sd$biz_size == 0, na.rm = TRUE)
sr5yr2006non_sd <- sum((df2006_sd_23$years.survived>=5 & df2006_sd_23$biz_size == 1), na.rm = TRUE)/sum(df2006_sd$biz_size == 1, na.rm = TRUE)


##10-year
sr10yr2001_sd <- (sum(df2001_sd_23$years.survived>=10))/nrow(df2001_sd) 
sr10yr2001sm_sd <- sum((df2001_sd_23$years.survived>=10 & df2001_sd_23$biz_size == 0), na.rm = TRUE)/sum(df2001_sd$biz_size == 0, na.rm = TRUE)
sr10yr2001non_sd <- sum((df2001_sd_23$years.survived>=10 & df2001_sd_23$biz_size == 1), na.rm = TRUE)/sum(df2002_sd$biz_size == 1, na.rm = TRUE)


sr10yr2002_sd <- sum(df2002_sd_23$years.survived>=10)/nrow(df2002_sd) 
sr10yr2002sm_sd <- sum((df2002_sd_23$years.survived>=10 & df2002_sd_23$biz_size == 0), na.rm = TRUE)/sum(df2002_sd$biz_size == 0, na.rm = TRUE)
sr10yr2002non_sd <- sum((df2002_sd_23$years.survived>=10 & df2002_sd_23$biz_size == 1), na.rm = TRUE)/sum(df2002_sd$biz_size == 1, na.rm = TRUE)

sr10yr2003_sd <- sum(df2003_sd_23$years.survived>=10)/nrow(df2003_sd)
sr10yr2003sm_sd <- sum((df2003_sd_23$years.survived>=10 & df2003_sd_23$biz_size == 0), na.rm = TRUE)/sum(df2003_sd$biz_size == 0, na.rm = TRUE)
sr10yr2003non_sd <- sum((df2003_sd_23$years.survived>=10 & df2003_sd_23$biz_size == 1), na.rm = TRUE)/sum(df2003_sd$biz_size == 1, na.rm = TRUE)

sr10yr2004_sd <- sum(df2004_sd_23$years.survived>=10)/nrow(df2004_sd) 
sr10yr2004sm_sd <- sum((df2004_sd_23$years.survived>=10 & df2004_sd_23$biz_size == 0), na.rm = TRUE)/sum(df2004_sd$biz_size == 0, na.rm = TRUE)
sr10yr2004non_sd <- sum((df2004_sd_23$years.survived>=10 & df2004_sd_23$biz_size == 1), na.rm = TRUE)/sum(df2004_sd$biz_size == 1, na.rm = TRUE)

sr10yr2005_sd <- sum(df2005_sd_23$years.survived>=10)/nrow(df2005_sd) 
sr10yr2005sm_sd <- sum((df2005_sd_23$years.survived>=10 & df2005_sd_23$biz_size == 0), na.rm = TRUE)/sum(df2005_sd$biz_size == 0, na.rm = TRUE)
sr10yr2005non_sd <- sum((df2005_sd_23$years.survived>=10 & df2005_sd_23$biz_size == 1), na.rm = TRUE)/sum(df2005_sd$biz_size == 1, na.rm = TRUE)

sr10yr2006_sd <- sum(df2006_sd_23$years.survived>=10)/nrow(df2006_sd) 
sr10yr2006sm_sd <- sum((df2006_sd_23$years.survived>=10 & df2006_sd_23$biz_size == 0), na.rm = TRUE)/sum(df2006_sd$biz_size == 0, na.rm = TRUE)
sr10yr2006non_sd <- sum((df2006_sd_23$years.survived>=10 & df2006_sd_23$biz_size == 1), na.rm = TRUE)/sum(df2006_sd$biz_size == 1, na.rm = TRUE)

##bound all survival rates together and reformated dataframe

survivalrate_sd <- cbind(sr3yr2001_sd, sr3yr2001sm_sd, sr3yr2001non_sd, sr5yr2001_sd, sr5yr2001sm_sd, sr5yr2001non_sd, 
                         sr10yr2001_sd, sr10yr2001sm_sd, sr10yr2001non_sd,
                         sr3yr2002_sd, sr3yr2002sm_sd, sr3yr2002non_sd, sr5yr2002_sd, sr5yr2002sm_sd, sr5yr2002non_sd, 
                         sr10yr2002_sd, sr10yr2002sm_sd, sr10yr2002non_sd,
                         sr3yr2003_sd, sr3yr2003sm_sd, sr3yr2003non_sd, sr5yr2003_sd, sr5yr2003sm_sd, sr5yr2003non_sd, 
                         sr10yr2003_sd, sr10yr2003sm_sd, sr10yr2003non_sd,
                         sr3yr2004_sd, sr3yr2004sm_sd, sr3yr2004non_sd, sr5yr2004_sd, sr5yr2004sm_sd, sr5yr2004non_sd, 
                         sr10yr2004_sd, sr10yr2004sm_sd, sr10yr2004non_sd,
                         sr3yr2005_sd, sr3yr2005sm_sd, sr3yr2005non_sd, sr5yr2005_sd, sr5yr2005sm_sd, sr5yr2005non_sd, 
                         sr10yr2005_sd, sr10yr2005sm_sd, sr10yr2005non_sd,
                         sr3yr2006_sd, sr3yr2006sm_sd, sr3yr2006non_sd, sr5yr2006_sd, sr5yr2006sm_sd, sr5yr2006non_sd, 
                         sr10yr2006_sd, sr10yr2006sm_sd, sr10yr2006non_sd)
srnames_sd <- c("3yr_2001_all_sd", "3yr_2001_sm_sd", 
                "3yr_2001_non_sd", "5yr_2001_all_sd", "5yr_2001_sm_sd", "5yr_2001_non_sd", "10yr_2001_all_sd", 
                "10yr_2001_sm_sd", "10yr_2001_non_sd",
                "3yr_2002_all_sd", "3yr_2002_sm_sd", "3yr_2002_non_sd", "5yr_2002_all_sd", "5yr_2002_sm_sd", "5yr_2002_non_sd", 
                "10yr_2002_all_sd", "10yr_2002_sm_sd", "10yr_2002_non_sd",
                "3yr_2003_all_sd", "3yr_2003_sm_sd", "3yr_2003_non_sd", "5yr_2003_all_sd", "5yr_2003_sm_sd", "5yr_2003_non_sd", 
                "10yr_2003_all_sd", "10yr_2003_sm_sd", "10yr_2003_non_sd",
                "3yr_2004_all_sd", "3yr_2004_sm_sd", "3yr_2004_non_sd", "5yr_2004_all_sd", "5yr_2004_sm_sd", "5yr_2004_non_sd", 
                "10yr_2004_all_sd", "10yr_2004_sm_sd", "10yr_2004_non_sd",
                "3yr_2005_all_sd", "3yr_2005_sm_sd", "3yr_2005_non_sd", "5yr_2005_all_sd", "5yr_2005_sm_sd", "5yr_2005_non_sd", 
                "10yr2005_all_sd", "10yr_2005_sm_sd", "10yr_2005_non_sd",
                "3yr_2006_all_sd", "3yr_2006_sm_sd", "3yr_2006_non_sd", "5yr_2006_all_sd", "5yr_2006_sm_sd", "5yr_2006_non_sd", 
                "10yr_2006_all_sd", "10yr_2006_sm_sd", "10yr_2006_non_sd")
colnames(survivalrate_sd) <- srnames_sd

sr_sd <- as.data.frame(survivalrate_sd)

survival.rates_sd <- sr_sd %>% 
  gather("3yr_2001_all_sd":"10yr_2006_non_sd", key = "time_year_type", value = "survivalrate_sd") %>% 
  separate(time_year_type, into = c("time", "year", "type"), sep = "_") %>% 
  arrange(time, year) %>% 
  unite("time_type", time, type, sep = "_") 

survival.rates_sd$time_type[18] <- "10yr_all"
survival.rates_sd$year[18] <- 2005

survival.rates_sd_23 <- survival.rates_sd %>% 
  spread(key = "time_type", value = "survivalrate_sd")

survival.rates_sd_23[, c(1, 5, 7, 6, 8, 10, 9, 2, 4, 3)]

##"31-33"####

df2001_sd_31 <- filter(df2001_sd, NAICS2 == "31-33")
df2002_sd_31 <- filter(df2002_sd, NAICS2 == "31-33")
df2003_sd_31 <- filter(df2003_sd, NAICS2 == "31-33")
df2004_sd_31 <- filter(df2004_sd, NAICS2 == "31-33")
df2005_sd_31 <- filter(df2005_sd, NAICS2 == "31-33")
df2006_sd_31 <- filter(df2006_sd, NAICS2 == "31-33")

##3-year
sr3yr2001_sd <- sum(df2001_sd_31$years.survived>=3)/nrow(df2001_sd)
sr3yr2001sm_sd <- sum((df2001_sd_31$years.survived>=3 & df2001_sd_31$biz_size == 0), na.rm = TRUE)/sum(df2001_sd$biz_size == 0, na.rm = TRUE)
sr3yr2001non_sd <- sum((df2001_sd_31$years.survived>=3 & df2001_sd_31$biz_size == 1), na.rm = TRUE)/sum(df2001_sd$biz_size == 1, na.rm = TRUE)

sr3yr2002_sd <- sum(df2002_sd_31$years.survived>=3)/nrow(df2002_sd) 
sr3yr2002sm_sd <- sum((df2002_sd_31$years.survived>=3 & df2002_sd_31$biz_size == 0), na.rm = TRUE)/sum(df2002_sd$biz_size == 0, na.rm = TRUE)
sr3yr2002non_sd <- sum((df2002_sd_31$years.survived>=3 & df2002_sd_31$biz_size == 1), na.rm = TRUE)/sum(df2002_sd$biz_size == 1, na.rm = TRUE)

sr3yr2003_sd <- sum(df2003_sd_31$years.survived>=3)/nrow(df2003_sd)
sr3yr2003sm_sd <- sum((df2003_sd_31$years.survived>=3 & df2003_sd_31$biz_size == 0), na.rm = TRUE)/sum(df2003_sd$biz_size == 0, na.rm = TRUE)
sr3yr2003non_sd <- sum((df2003_sd_31$years.survived>=3 & df2003_sd_31$biz_size == 1), na.rm = TRUE)/sum(df2003_sd$biz_size == 1, na.rm = TRUE)

sr3yr2004_sd <- sum(df2004_sd_31$years.survived>=3)/nrow(df2004_sd) 
sr3yr2004sm_sd <- sum((df2004_sd_31$years.survived>=3 & df2004_sd_31$biz_size == 0), na.rm = TRUE)/sum(df2004_sd$biz_size == 0, na.rm = TRUE)
sr3yr2004non_sd <- sum((df2004_sd_31$years.survived>=3 & df2004_sd_31$biz_size == 1), na.rm = TRUE)/sum(df2004_sd$biz_size == 1, na.rm = TRUE)

sr3yr2005_sd <- sum(df2005_sd_31$years.survived>=3)/nrow(df2005_sd) 
sr3yr2005sm_sd <- sum((df2005_sd_31$years.survived>=3 & df2005_sd_31$biz_size == 0), na.rm = TRUE)/sum(df2005_sd$biz_size == 0, na.rm = TRUE)
sr3yr2005non_sd <- sum((df2005_sd_31$years.survived>=3 & df2005_sd_31$biz_size == 1), na.rm = TRUE)/sum(df2005_sd$biz_size == 1, na.rm = TRUE)

sr3yr2006_sd <- sum(df2006_sd_31$years.survived>=3)/nrow(df2006_sd) 
sr3yr2006sm_sd <- sum((df2006_sd_31$years.survived>=3 & df2006_sd_31$biz_size == 0), na.rm = TRUE)/sum(df2006_sd$biz_size == 0, na.rm = TRUE)
sr3yr2006non_sd <- sum((df2006_sd_31$years.survived>=3 & df2006_sd_31$biz_size == 1), na.rm = TRUE)/sum(df2006_sd$biz_size == 1, na.rm = TRUE)

##5-year
sr5yr2001_sd <- sum(df2001_sd_31$years.survived>=5)/nrow(df2001_sd)
sr5yr2001sm_sd <- sum((df2001_sd_31$years.survived>=5 & df2001_sd_31$biz_size == 0), na.rm = TRUE)/sum(df2001_sd$biz_size == 0, na.rm = TRUE)
sr5yr2001non_sd <- sum((df2001_sd_31$years.survived>=5 & df2001_sd_31$biz_size == 1), na.rm = TRUE)/sum(df2001_sd$biz_size == 1, na.rm = TRUE)

sr5yr2002_sd <- sum(df2002_sd_31$years.survived>=5)/nrow(df2002_sd) 
sr5yr2002sm_sd <- sum((df2002_sd_31$years.survived>=5 & df2002_sd_31$biz_size == 0), na.rm = TRUE)/sum(df2002_sd$biz_size == 0, na.rm = TRUE)
sr5yr2002non_sd <- sum((df2002_sd_31$years.survived>=5 & df2002_sd_31$biz_size == 1), na.rm = TRUE)/sum(df2002_sd$biz_size == 1, na.rm = TRUE)

sr5yr2003_sd <- sum(df2003_sd_31$years.survived>=5)/nrow(df2003_sd)
sr5yr2003sm_sd <- sum((df2003_sd_31$years.survived>=5 & df2003_sd_31$biz_size == 0), na.rm = TRUE)/sum(df2003_sd$biz_size == 0, na.rm = TRUE)
sr5yr2003non_sd <- sum((df2003_sd_31$years.survived>=5 & df2003_sd_31$biz_size == 1), na.rm = TRUE)/sum(df2003_sd$biz_size == 1, na.rm = TRUE)

sr5yr2004_sd <- sum(df2004_sd_31$years.survived>=5)/nrow(df2004_sd) 
sr5yr2004sm_sd <- sum((df2004_sd_31$years.survived>=5 & df2004_sd_31$biz_size == 0), na.rm = TRUE)/sum(df2004_sd$biz_size == 0, na.rm = TRUE)
sr5yr2004non_sd <- sum((df2004_sd_31$years.survived>=5 & df2004_sd_31$biz_size == 1), na.rm = TRUE)/sum(df2004_sd$biz_size == 1, na.rm = TRUE)

sr5yr2005_sd <- sum(df2005_sd_31$years.survived>=5)/nrow(df2005_sd) 
sr5yr2005sm_sd <- sum((df2005_sd_31$years.survived>=5 & df2005_sd_31$biz_size == 0), na.rm = TRUE)/sum(df2005_sd$biz_size == 0, na.rm = TRUE)
sr5yr2005non_sd <- sum((df2005_sd_31$years.survived>=5 & df2005_sd_31$biz_size == 1), na.rm = TRUE)/sum(df2005_sd$biz_size == 1, na.rm = TRUE)

sr5yr2006_sd <- sum(df2006_sd_31$years.survived>=5)/nrow(df2006_sd) 
sr5yr2006sm_sd <- sum((df2006_sd_31$years.survived>=5 & df2006_sd_31$biz_size == 0), na.rm = TRUE)/sum(df2006_sd$biz_size == 0, na.rm = TRUE)
sr5yr2006non_sd <- sum((df2006_sd_31$years.survived>=5 & df2006_sd_31$biz_size == 1), na.rm = TRUE)/sum(df2006_sd$biz_size == 1, na.rm = TRUE)


##10-year
sr10yr2001_sd <- (sum(df2001_sd_31$years.survived>=10))/nrow(df2001_sd) 
sr10yr2001sm_sd <- sum((df2001_sd_31$years.survived>=10 & df2001_sd_31$biz_size == 0), na.rm = TRUE)/sum(df2001_sd$biz_size == 0, na.rm = TRUE)
sr10yr2001non_sd <- sum((df2001_sd_31$years.survived>=10 & df2001_sd_31$biz_size == 1), na.rm = TRUE)/sum(df2002_sd$biz_size == 1, na.rm = TRUE)


sr10yr2002_sd <- sum(df2002_sd_31$years.survived>=10)/nrow(df2002_sd) 
sr10yr2002sm_sd <- sum((df2002_sd_31$years.survived>=10 & df2002_sd_31$biz_size == 0), na.rm = TRUE)/sum(df2002_sd$biz_size == 0, na.rm = TRUE)
sr10yr2002non_sd <- sum((df2002_sd_31$years.survived>=10 & df2002_sd_31$biz_size == 1), na.rm = TRUE)/sum(df2002_sd$biz_size == 1, na.rm = TRUE)

sr10yr2003_sd <- sum(df2003_sd_31$years.survived>=10)/nrow(df2003_sd)
sr10yr2003sm_sd <- sum((df2003_sd_31$years.survived>=10 & df2003_sd_31$biz_size == 0), na.rm = TRUE)/sum(df2003_sd$biz_size == 0, na.rm = TRUE)
sr10yr2003non_sd <- sum((df2003_sd_31$years.survived>=10 & df2003_sd_31$biz_size == 1), na.rm = TRUE)/sum(df2003_sd$biz_size == 1, na.rm = TRUE)

sr10yr2004_sd <- sum(df2004_sd_31$years.survived>=10)/nrow(df2004_sd) 
sr10yr2004sm_sd <- sum((df2004_sd_31$years.survived>=10 & df2004_sd_31$biz_size == 0), na.rm = TRUE)/sum(df2004_sd$biz_size == 0, na.rm = TRUE)
sr10yr2004non_sd <- sum((df2004_sd_31$years.survived>=10 & df2004_sd_31$biz_size == 1), na.rm = TRUE)/sum(df2004_sd$biz_size == 1, na.rm = TRUE)

sr10yr2005_sd <- sum(df2005_sd_31$years.survived>=10)/nrow(df2005_sd) 
sr10yr2005sm_sd <- sum((df2005_sd_31$years.survived>=10 & df2005_sd_31$biz_size == 0), na.rm = TRUE)/sum(df2005_sd$biz_size == 0, na.rm = TRUE)
sr10yr2005non_sd <- sum((df2005_sd_31$years.survived>=10 & df2005_sd_31$biz_size == 1), na.rm = TRUE)/sum(df2005_sd$biz_size == 1, na.rm = TRUE)

sr10yr2006_sd <- sum(df2006_sd_31$years.survived>=10)/nrow(df2006_sd) 
sr10yr2006sm_sd <- sum((df2006_sd_31$years.survived>=10 & df2006_sd_31$biz_size == 0), na.rm = TRUE)/sum(df2006_sd$biz_size == 0, na.rm = TRUE)
sr10yr2006non_sd <- sum((df2006_sd_31$years.survived>=10 & df2006_sd_31$biz_size == 1), na.rm = TRUE)/sum(df2006_sd$biz_size == 1, na.rm = TRUE)

##bound all survival rates together and reformated dataframe

survivalrate_sd <- cbind(sr3yr2001_sd, sr3yr2001sm_sd, sr3yr2001non_sd, sr5yr2001_sd, sr5yr2001sm_sd, sr5yr2001non_sd, 
                         sr10yr2001_sd, sr10yr2001sm_sd, sr10yr2001non_sd,
                         sr3yr2002_sd, sr3yr2002sm_sd, sr3yr2002non_sd, sr5yr2002_sd, sr5yr2002sm_sd, sr5yr2002non_sd, 
                         sr10yr2002_sd, sr10yr2002sm_sd, sr10yr2002non_sd,
                         sr3yr2003_sd, sr3yr2003sm_sd, sr3yr2003non_sd, sr5yr2003_sd, sr5yr2003sm_sd, sr5yr2003non_sd, 
                         sr10yr2003_sd, sr10yr2003sm_sd, sr10yr2003non_sd,
                         sr3yr2004_sd, sr3yr2004sm_sd, sr3yr2004non_sd, sr5yr2004_sd, sr5yr2004sm_sd, sr5yr2004non_sd, 
                         sr10yr2004_sd, sr10yr2004sm_sd, sr10yr2004non_sd,
                         sr3yr2005_sd, sr3yr2005sm_sd, sr3yr2005non_sd, sr5yr2005_sd, sr5yr2005sm_sd, sr5yr2005non_sd, 
                         sr10yr2005_sd, sr10yr2005sm_sd, sr10yr2005non_sd,
                         sr3yr2006_sd, sr3yr2006sm_sd, sr3yr2006non_sd, sr5yr2006_sd, sr5yr2006sm_sd, sr5yr2006non_sd, 
                         sr10yr2006_sd, sr10yr2006sm_sd, sr10yr2006non_sd)
srnames_sd <- c("3yr_2001_all_sd", "3yr_2001_sm_sd", 
                "3yr_2001_non_sd", "5yr_2001_all_sd", "5yr_2001_sm_sd", "5yr_2001_non_sd", "10yr_2001_all_sd", 
                "10yr_2001_sm_sd", "10yr_2001_non_sd",
                "3yr_2002_all_sd", "3yr_2002_sm_sd", "3yr_2002_non_sd", "5yr_2002_all_sd", "5yr_2002_sm_sd", "5yr_2002_non_sd", 
                "10yr_2002_all_sd", "10yr_2002_sm_sd", "10yr_2002_non_sd",
                "3yr_2003_all_sd", "3yr_2003_sm_sd", "3yr_2003_non_sd", "5yr_2003_all_sd", "5yr_2003_sm_sd", "5yr_2003_non_sd", 
                "10yr_2003_all_sd", "10yr_2003_sm_sd", "10yr_2003_non_sd",
                "3yr_2004_all_sd", "3yr_2004_sm_sd", "3yr_2004_non_sd", "5yr_2004_all_sd", "5yr_2004_sm_sd", "5yr_2004_non_sd", 
                "10yr_2004_all_sd", "10yr_2004_sm_sd", "10yr_2004_non_sd",
                "3yr_2005_all_sd", "3yr_2005_sm_sd", "3yr_2005_non_sd", "5yr_2005_all_sd", "5yr_2005_sm_sd", "5yr_2005_non_sd", 
                "10yr2005_all_sd", "10yr_2005_sm_sd", "10yr_2005_non_sd",
                "3yr_2006_all_sd", "3yr_2006_sm_sd", "3yr_2006_non_sd", "5yr_2006_all_sd", "5yr_2006_sm_sd", "5yr_2006_non_sd", 
                "10yr_2006_all_sd", "10yr_2006_sm_sd", "10yr_2006_non_sd")
colnames(survivalrate_sd) <- srnames_sd

sr_sd <- as.data.frame(survivalrate_sd)

survival.rates_sd <- sr_sd %>% 
  gather("3yr_2001_all_sd":"10yr_2006_non_sd", key = "time_year_type", value = "survivalrate_sd") %>% 
  separate(time_year_type, into = c("time", "year", "type"), sep = "_") %>% 
  arrange(time, year) %>% 
  unite("time_type", time, type, sep = "_") 

survival.rates_sd$time_type[18] <- "10yr_all"
survival.rates_sd$year[18] <- 2005

survival.rates_sd_31 <- survival.rates_sd %>% 
  spread(key = "time_type", value = "survivalrate_sd")

survival.rates_sd_31[, c(1, 5, 7, 6, 8, 10, 9, 2, 4, 3)]

##42####

df2001_sd_42 <- filter(df2001_sd, NAICS2 == 42)
df2002_sd_42 <- filter(df2002_sd, NAICS2 == 42)
df2003_sd_42 <- filter(df2003_sd, NAICS2 == 42)
df2004_sd_42 <- filter(df2004_sd, NAICS2 == 42)
df2005_sd_42 <- filter(df2005_sd, NAICS2 == 42)
df2006_sd_42 <- filter(df2006_sd, NAICS2 == 42)

##3-year
sr3yr2001_sd <- sum(df2001_sd_42$years.survived>=3)/nrow(df2001_sd)
sr3yr2001sm_sd <- sum((df2001_sd_42$years.survived>=3 & df2001_sd_42$biz_size == 0), na.rm = TRUE)/sum(df2001_sd$biz_size == 0, na.rm = TRUE)
sr3yr2001non_sd <- sum((df2001_sd_42$years.survived>=3 & df2001_sd_42$biz_size == 1), na.rm = TRUE)/sum(df2001_sd$biz_size == 1, na.rm = TRUE)

sr3yr2002_sd <- sum(df2002_sd_42$years.survived>=3)/nrow(df2002_sd) 
sr3yr2002sm_sd <- sum((df2002_sd_42$years.survived>=3 & df2002_sd_42$biz_size == 0), na.rm = TRUE)/sum(df2002_sd$biz_size == 0, na.rm = TRUE)
sr3yr2002non_sd <- sum((df2002_sd_42$years.survived>=3 & df2002_sd_42$biz_size == 1), na.rm = TRUE)/sum(df2002_sd$biz_size == 1, na.rm = TRUE)

sr3yr2003_sd <- sum(df2003_sd_42$years.survived>=3)/nrow(df2003_sd)
sr3yr2003sm_sd <- sum((df2003_sd_42$years.survived>=3 & df2003_sd_42$biz_size == 0), na.rm = TRUE)/sum(df2003_sd$biz_size == 0, na.rm = TRUE)
sr3yr2003non_sd <- sum((df2003_sd_42$years.survived>=3 & df2003_sd_42$biz_size == 1), na.rm = TRUE)/sum(df2003_sd$biz_size == 1, na.rm = TRUE)

sr3yr2004_sd <- sum(df2004_sd_42$years.survived>=3)/nrow(df2004_sd) 
sr3yr2004sm_sd <- sum((df2004_sd_42$years.survived>=3 & df2004_sd_42$biz_size == 0), na.rm = TRUE)/sum(df2004_sd$biz_size == 0, na.rm = TRUE)
sr3yr2004non_sd <- sum((df2004_sd_42$years.survived>=3 & df2004_sd_42$biz_size == 1), na.rm = TRUE)/sum(df2004_sd$biz_size == 1, na.rm = TRUE)

sr3yr2005_sd <- sum(df2005_sd_42$years.survived>=3)/nrow(df2005_sd) 
sr3yr2005sm_sd <- sum((df2005_sd_42$years.survived>=3 & df2005_sd_42$biz_size == 0), na.rm = TRUE)/sum(df2005_sd$biz_size == 0, na.rm = TRUE)
sr3yr2005non_sd <- sum((df2005_sd_42$years.survived>=3 & df2005_sd_42$biz_size == 1), na.rm = TRUE)/sum(df2005_sd$biz_size == 1, na.rm = TRUE)

sr3yr2006_sd <- sum(df2006_sd_42$years.survived>=3)/nrow(df2006_sd) 
sr3yr2006sm_sd <- sum((df2006_sd_42$years.survived>=3 & df2006_sd_42$biz_size == 0), na.rm = TRUE)/sum(df2006_sd$biz_size == 0, na.rm = TRUE)
sr3yr2006non_sd <- sum((df2006_sd_42$years.survived>=3 & df2006_sd_42$biz_size == 1), na.rm = TRUE)/sum(df2006_sd$biz_size == 1, na.rm = TRUE)

##5-year
sr5yr2001_sd <- sum(df2001_sd_42$years.survived>=5)/nrow(df2001_sd)
sr5yr2001sm_sd <- sum((df2001_sd_42$years.survived>=5 & df2001_sd_42$biz_size == 0), na.rm = TRUE)/sum(df2001_sd$biz_size == 0, na.rm = TRUE)
sr5yr2001non_sd <- sum((df2001_sd_42$years.survived>=5 & df2001_sd_42$biz_size == 1), na.rm = TRUE)/sum(df2001_sd$biz_size == 1, na.rm = TRUE)

sr5yr2002_sd <- sum(df2002_sd_42$years.survived>=5)/nrow(df2002_sd) 
sr5yr2002sm_sd <- sum((df2002_sd_42$years.survived>=5 & df2002_sd_42$biz_size == 0), na.rm = TRUE)/sum(df2002_sd$biz_size == 0, na.rm = TRUE)
sr5yr2002non_sd <- sum((df2002_sd_42$years.survived>=5 & df2002_sd_42$biz_size == 1), na.rm = TRUE)/sum(df2002_sd$biz_size == 1, na.rm = TRUE)

sr5yr2003_sd <- sum(df2003_sd_42$years.survived>=5)/nrow(df2003_sd)
sr5yr2003sm_sd <- sum((df2003_sd_42$years.survived>=5 & df2003_sd_42$biz_size == 0), na.rm = TRUE)/sum(df2003_sd$biz_size == 0, na.rm = TRUE)
sr5yr2003non_sd <- sum((df2003_sd_42$years.survived>=5 & df2003_sd_42$biz_size == 1), na.rm = TRUE)/sum(df2003_sd$biz_size == 1, na.rm = TRUE)

sr5yr2004_sd <- sum(df2004_sd_42$years.survived>=5)/nrow(df2004_sd) 
sr5yr2004sm_sd <- sum((df2004_sd_42$years.survived>=5 & df2004_sd_42$biz_size == 0), na.rm = TRUE)/sum(df2004_sd$biz_size == 0, na.rm = TRUE)
sr5yr2004non_sd <- sum((df2004_sd_42$years.survived>=5 & df2004_sd_42$biz_size == 1), na.rm = TRUE)/sum(df2004_sd$biz_size == 1, na.rm = TRUE)

sr5yr2005_sd <- sum(df2005_sd_42$years.survived>=5)/nrow(df2005_sd) 
sr5yr2005sm_sd <- sum((df2005_sd_42$years.survived>=5 & df2005_sd_42$biz_size == 0), na.rm = TRUE)/sum(df2005_sd$biz_size == 0, na.rm = TRUE)
sr5yr2005non_sd <- sum((df2005_sd_42$years.survived>=5 & df2005_sd_42$biz_size == 1), na.rm = TRUE)/sum(df2005_sd$biz_size == 1, na.rm = TRUE)

sr5yr2006_sd <- sum(df2006_sd_42$years.survived>=5)/nrow(df2006_sd) 
sr5yr2006sm_sd <- sum((df2006_sd_42$years.survived>=5 & df2006_sd_42$biz_size == 0), na.rm = TRUE)/sum(df2006_sd$biz_size == 0, na.rm = TRUE)
sr5yr2006non_sd <- sum((df2006_sd_42$years.survived>=5 & df2006_sd_42$biz_size == 1), na.rm = TRUE)/sum(df2006_sd$biz_size == 1, na.rm = TRUE)


##10-year
sr10yr2001_sd <- (sum(df2001_sd_42$years.survived>=10))/nrow(df2001_sd) 
sr10yr2001sm_sd <- sum((df2001_sd_42$years.survived>=10 & df2001_sd_42$biz_size == 0), na.rm = TRUE)/sum(df2001_sd$biz_size == 0, na.rm = TRUE)
sr10yr2001non_sd <- sum((df2001_sd_42$years.survived>=10 & df2001_sd_42$biz_size == 1), na.rm = TRUE)/sum(df2002_sd$biz_size == 1, na.rm = TRUE)


sr10yr2002_sd <- sum(df2002_sd_42$years.survived>=10)/nrow(df2002_sd) 
sr10yr2002sm_sd <- sum((df2002_sd_42$years.survived>=10 & df2002_sd_42$biz_size == 0), na.rm = TRUE)/sum(df2002_sd$biz_size == 0, na.rm = TRUE)
sr10yr2002non_sd <- sum((df2002_sd_42$years.survived>=10 & df2002_sd_42$biz_size == 1), na.rm = TRUE)/sum(df2002_sd$biz_size == 1, na.rm = TRUE)

sr10yr2003_sd <- sum(df2003_sd_42$years.survived>=10)/nrow(df2003_sd)
sr10yr2003sm_sd <- sum((df2003_sd_42$years.survived>=10 & df2003_sd_42$biz_size == 0), na.rm = TRUE)/sum(df2003_sd$biz_size == 0, na.rm = TRUE)
sr10yr2003non_sd <- sum((df2003_sd_42$years.survived>=10 & df2003_sd_42$biz_size == 1), na.rm = TRUE)/sum(df2003_sd$biz_size == 1, na.rm = TRUE)

sr10yr2004_sd <- sum(df2004_sd_42$years.survived>=10)/nrow(df2004_sd) 
sr10yr2004sm_sd <- sum((df2004_sd_42$years.survived>=10 & df2004_sd_42$biz_size == 0), na.rm = TRUE)/sum(df2004_sd$biz_size == 0, na.rm = TRUE)
sr10yr2004non_sd <- sum((df2004_sd_42$years.survived>=10 & df2004_sd_42$biz_size == 1), na.rm = TRUE)/sum(df2004_sd$biz_size == 1, na.rm = TRUE)

sr10yr2005_sd <- sum(df2005_sd_42$years.survived>=10)/nrow(df2005_sd) 
sr10yr2005sm_sd <- sum((df2005_sd_42$years.survived>=10 & df2005_sd_42$biz_size == 0), na.rm = TRUE)/sum(df2005_sd$biz_size == 0, na.rm = TRUE)
sr10yr2005non_sd <- sum((df2005_sd_42$years.survived>=10 & df2005_sd_42$biz_size == 1), na.rm = TRUE)/sum(df2005_sd$biz_size == 1, na.rm = TRUE)

sr10yr2006_sd <- sum(df2006_sd_42$years.survived>=10)/nrow(df2006_sd) 
sr10yr2006sm_sd <- sum((df2006_sd_42$years.survived>=10 & df2006_sd_42$biz_size == 0), na.rm = TRUE)/sum(df2006_sd$biz_size == 0, na.rm = TRUE)
sr10yr2006non_sd <- sum((df2006_sd_42$years.survived>=10 & df2006_sd_42$biz_size == 1), na.rm = TRUE)/sum(df2006_sd$biz_size == 1, na.rm = TRUE)

##bound all survival rates together and reformated dataframe

survivalrate_sd <- cbind(sr3yr2001_sd, sr3yr2001sm_sd, sr3yr2001non_sd, sr5yr2001_sd, sr5yr2001sm_sd, sr5yr2001non_sd, 
                         sr10yr2001_sd, sr10yr2001sm_sd, sr10yr2001non_sd,
                         sr3yr2002_sd, sr3yr2002sm_sd, sr3yr2002non_sd, sr5yr2002_sd, sr5yr2002sm_sd, sr5yr2002non_sd, 
                         sr10yr2002_sd, sr10yr2002sm_sd, sr10yr2002non_sd,
                         sr3yr2003_sd, sr3yr2003sm_sd, sr3yr2003non_sd, sr5yr2003_sd, sr5yr2003sm_sd, sr5yr2003non_sd, 
                         sr10yr2003_sd, sr10yr2003sm_sd, sr10yr2003non_sd,
                         sr3yr2004_sd, sr3yr2004sm_sd, sr3yr2004non_sd, sr5yr2004_sd, sr5yr2004sm_sd, sr5yr2004non_sd, 
                         sr10yr2004_sd, sr10yr2004sm_sd, sr10yr2004non_sd,
                         sr3yr2005_sd, sr3yr2005sm_sd, sr3yr2005non_sd, sr5yr2005_sd, sr5yr2005sm_sd, sr5yr2005non_sd, 
                         sr10yr2005_sd, sr10yr2005sm_sd, sr10yr2005non_sd,
                         sr3yr2006_sd, sr3yr2006sm_sd, sr3yr2006non_sd, sr5yr2006_sd, sr5yr2006sm_sd, sr5yr2006non_sd, 
                         sr10yr2006_sd, sr10yr2006sm_sd, sr10yr2006non_sd)
srnames_sd <- c("3yr_2001_all_sd", "3yr_2001_sm_sd", 
                "3yr_2001_non_sd", "5yr_2001_all_sd", "5yr_2001_sm_sd", "5yr_2001_non_sd", "10yr_2001_all_sd", 
                "10yr_2001_sm_sd", "10yr_2001_non_sd",
                "3yr_2002_all_sd", "3yr_2002_sm_sd", "3yr_2002_non_sd", "5yr_2002_all_sd", "5yr_2002_sm_sd", "5yr_2002_non_sd", 
                "10yr_2002_all_sd", "10yr_2002_sm_sd", "10yr_2002_non_sd",
                "3yr_2003_all_sd", "3yr_2003_sm_sd", "3yr_2003_non_sd", "5yr_2003_all_sd", "5yr_2003_sm_sd", "5yr_2003_non_sd", 
                "10yr_2003_all_sd", "10yr_2003_sm_sd", "10yr_2003_non_sd",
                "3yr_2004_all_sd", "3yr_2004_sm_sd", "3yr_2004_non_sd", "5yr_2004_all_sd", "5yr_2004_sm_sd", "5yr_2004_non_sd", 
                "10yr_2004_all_sd", "10yr_2004_sm_sd", "10yr_2004_non_sd",
                "3yr_2005_all_sd", "3yr_2005_sm_sd", "3yr_2005_non_sd", "5yr_2005_all_sd", "5yr_2005_sm_sd", "5yr_2005_non_sd", 
                "10yr2005_all_sd", "10yr_2005_sm_sd", "10yr_2005_non_sd",
                "3yr_2006_all_sd", "3yr_2006_sm_sd", "3yr_2006_non_sd", "5yr_2006_all_sd", "5yr_2006_sm_sd", "5yr_2006_non_sd", 
                "10yr_2006_all_sd", "10yr_2006_sm_sd", "10yr_2006_non_sd")
colnames(survivalrate_sd) <- srnames_sd

sr_sd <- as.data.frame(survivalrate_sd)

survival.rates_sd <- sr_sd %>% 
  gather("3yr_2001_all_sd":"10yr_2006_non_sd", key = "time_year_type", value = "survivalrate_sd") %>% 
  separate(time_year_type, into = c("time", "year", "type"), sep = "_") %>% 
  arrange(time, year) %>% 
  unite("time_type", time, type, sep = "_") 

survival.rates_sd$time_type[18] <- "10yr_all"
survival.rates_sd$year[18] <- 2005

survival.rates_sd_42 <- survival.rates_sd %>% 
  spread(key = "time_type", value = "survivalrate_sd")

survival.rates_sd_42[, c(1, 5, 7, 6, 8, 10, 9, 2, 4, 3)]

##44-45####

df2001_sd_44 <- filter(df2001_sd, NAICS2 == "44-45")
df2002_sd_44 <- filter(df2002_sd, NAICS2 == "44-45")
df2003_sd_44 <- filter(df2003_sd, NAICS2 == "44-45")
df2004_sd_44 <- filter(df2004_sd, NAICS2 == "44-45")
df2005_sd_44 <- filter(df2005_sd, NAICS2 == "44-45")
df2006_sd_44 <- filter(df2006_sd, NAICS2 == "44-45")

##3-year
sr3yr2001_sd <- sum(df2001_sd_44$years.survived>=3)/nrow(df2001_sd)
sr3yr2001sm_sd <- sum((df2001_sd_44$years.survived>=3 & df2001_sd_44$biz_size == 0), na.rm = TRUE)/sum(df2001_sd$biz_size == 0, na.rm = TRUE)
sr3yr2001non_sd <- sum((df2001_sd_44$years.survived>=3 & df2001_sd_44$biz_size == 1), na.rm = TRUE)/sum(df2001_sd$biz_size == 1, na.rm = TRUE)

sr3yr2002_sd <- sum(df2002_sd_44$years.survived>=3)/nrow(df2002_sd) 
sr3yr2002sm_sd <- sum((df2002_sd_44$years.survived>=3 & df2002_sd_44$biz_size == 0), na.rm = TRUE)/sum(df2002_sd$biz_size == 0, na.rm = TRUE)
sr3yr2002non_sd <- sum((df2002_sd_44$years.survived>=3 & df2002_sd_44$biz_size == 1), na.rm = TRUE)/sum(df2002_sd$biz_size == 1, na.rm = TRUE)

sr3yr2003_sd <- sum(df2003_sd_44$years.survived>=3)/nrow(df2003_sd)
sr3yr2003sm_sd <- sum((df2003_sd_44$years.survived>=3 & df2003_sd_44$biz_size == 0), na.rm = TRUE)/sum(df2003_sd$biz_size == 0, na.rm = TRUE)
sr3yr2003non_sd <- sum((df2003_sd_44$years.survived>=3 & df2003_sd_44$biz_size == 1), na.rm = TRUE)/sum(df2003_sd$biz_size == 1, na.rm = TRUE)

sr3yr2004_sd <- sum(df2004_sd_44$years.survived>=3)/nrow(df2004_sd) 
sr3yr2004sm_sd <- sum((df2004_sd_44$years.survived>=3 & df2004_sd_44$biz_size == 0), na.rm = TRUE)/sum(df2004_sd$biz_size == 0, na.rm = TRUE)
sr3yr2004non_sd <- sum((df2004_sd_44$years.survived>=3 & df2004_sd_44$biz_size == 1), na.rm = TRUE)/sum(df2004_sd$biz_size == 1, na.rm = TRUE)

sr3yr2005_sd <- sum(df2005_sd_44$years.survived>=3)/nrow(df2005_sd) 
sr3yr2005sm_sd <- sum((df2005_sd_44$years.survived>=3 & df2005_sd_44$biz_size == 0), na.rm = TRUE)/sum(df2005_sd$biz_size == 0, na.rm = TRUE)
sr3yr2005non_sd <- sum((df2005_sd_44$years.survived>=3 & df2005_sd_44$biz_size == 1), na.rm = TRUE)/sum(df2005_sd$biz_size == 1, na.rm = TRUE)

sr3yr2006_sd <- sum(df2006_sd_44$years.survived>=3)/nrow(df2006_sd) 
sr3yr2006sm_sd <- sum((df2006_sd_44$years.survived>=3 & df2006_sd_44$biz_size == 0), na.rm = TRUE)/sum(df2006_sd$biz_size == 0, na.rm = TRUE)
sr3yr2006non_sd <- sum((df2006_sd_44$years.survived>=3 & df2006_sd_44$biz_size == 1), na.rm = TRUE)/sum(df2006_sd$biz_size == 1, na.rm = TRUE)

##5-year
sr5yr2001_sd <- sum(df2001_sd_44$years.survived>=5)/nrow(df2001_sd)
sr5yr2001sm_sd <- sum((df2001_sd_44$years.survived>=5 & df2001_sd_44$biz_size == 0), na.rm = TRUE)/sum(df2001_sd$biz_size == 0, na.rm = TRUE)
sr5yr2001non_sd <- sum((df2001_sd_44$years.survived>=5 & df2001_sd_44$biz_size == 1), na.rm = TRUE)/sum(df2001_sd$biz_size == 1, na.rm = TRUE)

sr5yr2002_sd <- sum(df2002_sd_44$years.survived>=5)/nrow(df2002_sd) 
sr5yr2002sm_sd <- sum((df2002_sd_44$years.survived>=5 & df2002_sd_44$biz_size == 0), na.rm = TRUE)/sum(df2002_sd$biz_size == 0, na.rm = TRUE)
sr5yr2002non_sd <- sum((df2002_sd_44$years.survived>=5 & df2002_sd_44$biz_size == 1), na.rm = TRUE)/sum(df2002_sd$biz_size == 1, na.rm = TRUE)

sr5yr2003_sd <- sum(df2003_sd_44$years.survived>=5)/nrow(df2003_sd)
sr5yr2003sm_sd <- sum((df2003_sd_44$years.survived>=5 & df2003_sd_44$biz_size == 0), na.rm = TRUE)/sum(df2003_sd$biz_size == 0, na.rm = TRUE)
sr5yr2003non_sd <- sum((df2003_sd_44$years.survived>=5 & df2003_sd_44$biz_size == 1), na.rm = TRUE)/sum(df2003_sd$biz_size == 1, na.rm = TRUE)

sr5yr2004_sd <- sum(df2004_sd_44$years.survived>=5)/nrow(df2004_sd) 
sr5yr2004sm_sd <- sum((df2004_sd_44$years.survived>=5 & df2004_sd_44$biz_size == 0), na.rm = TRUE)/sum(df2004_sd$biz_size == 0, na.rm = TRUE)
sr5yr2004non_sd <- sum((df2004_sd_44$years.survived>=5 & df2004_sd_44$biz_size == 1), na.rm = TRUE)/sum(df2004_sd$biz_size == 1, na.rm = TRUE)

sr5yr2005_sd <- sum(df2005_sd_44$years.survived>=5)/nrow(df2005_sd) 
sr5yr2005sm_sd <- sum((df2005_sd_44$years.survived>=5 & df2005_sd_44$biz_size == 0), na.rm = TRUE)/sum(df2005_sd$biz_size == 0, na.rm = TRUE)
sr5yr2005non_sd <- sum((df2005_sd_44$years.survived>=5 & df2005_sd_44$biz_size == 1), na.rm = TRUE)/sum(df2005_sd$biz_size == 1, na.rm = TRUE)

sr5yr2006_sd <- sum(df2006_sd_44$years.survived>=5)/nrow(df2006_sd) 
sr5yr2006sm_sd <- sum((df2006_sd_44$years.survived>=5 & df2006_sd_44$biz_size == 0), na.rm = TRUE)/sum(df2006_sd$biz_size == 0, na.rm = TRUE)
sr5yr2006non_sd <- sum((df2006_sd_44$years.survived>=5 & df2006_sd_44$biz_size == 1), na.rm = TRUE)/sum(df2006_sd$biz_size == 1, na.rm = TRUE)


##10-year
sr10yr2001_sd <- (sum(df2001_sd_44$years.survived>=10))/nrow(df2001_sd) 
sr10yr2001sm_sd <- sum((df2001_sd_44$years.survived>=10 & df2001_sd_44$biz_size == 0), na.rm = TRUE)/sum(df2001_sd$biz_size == 0, na.rm = TRUE)
sr10yr2001non_sd <- sum((df2001_sd_44$years.survived>=10 & df2001_sd_44$biz_size == 1), na.rm = TRUE)/sum(df2002_sd$biz_size == 1, na.rm = TRUE)


sr10yr2002_sd <- sum(df2002_sd_44$years.survived>=10)/nrow(df2002_sd) 
sr10yr2002sm_sd <- sum((df2002_sd_44$years.survived>=10 & df2002_sd_44$biz_size == 0), na.rm = TRUE)/sum(df2002_sd$biz_size == 0, na.rm = TRUE)
sr10yr2002non_sd <- sum((df2002_sd_44$years.survived>=10 & df2002_sd_44$biz_size == 1), na.rm = TRUE)/sum(df2002_sd$biz_size == 1, na.rm = TRUE)

sr10yr2003_sd <- sum(df2003_sd_44$years.survived>=10)/nrow(df2003_sd)
sr10yr2003sm_sd <- sum((df2003_sd_44$years.survived>=10 & df2003_sd_44$biz_size == 0), na.rm = TRUE)/sum(df2003_sd$biz_size == 0, na.rm = TRUE)
sr10yr2003non_sd <- sum((df2003_sd_44$years.survived>=10 & df2003_sd_44$biz_size == 1), na.rm = TRUE)/sum(df2003_sd$biz_size == 1, na.rm = TRUE)

sr10yr2004_sd <- sum(df2004_sd_44$years.survived>=10)/nrow(df2004_sd) 
sr10yr2004sm_sd <- sum((df2004_sd_44$years.survived>=10 & df2004_sd_44$biz_size == 0), na.rm = TRUE)/sum(df2004_sd$biz_size == 0, na.rm = TRUE)
sr10yr2004non_sd <- sum((df2004_sd_44$years.survived>=10 & df2004_sd_44$biz_size == 1), na.rm = TRUE)/sum(df2004_sd$biz_size == 1, na.rm = TRUE)

sr10yr2005_sd <- sum(df2005_sd_44$years.survived>=10)/nrow(df2005_sd) 
sr10yr2005sm_sd <- sum((df2005_sd_44$years.survived>=10 & df2005_sd_44$biz_size == 0), na.rm = TRUE)/sum(df2005_sd$biz_size == 0, na.rm = TRUE)
sr10yr2005non_sd <- sum((df2005_sd_44$years.survived>=10 & df2005_sd_44$biz_size == 1), na.rm = TRUE)/sum(df2005_sd$biz_size == 1, na.rm = TRUE)

sr10yr2006_sd <- sum(df2006_sd_44$years.survived>=10)/nrow(df2006_sd) 
sr10yr2006sm_sd <- sum((df2006_sd_44$years.survived>=10 & df2006_sd_44$biz_size == 0), na.rm = TRUE)/sum(df2006_sd$biz_size == 0, na.rm = TRUE)
sr10yr2006non_sd <- sum((df2006_sd_44$years.survived>=10 & df2006_sd_44$biz_size == 1), na.rm = TRUE)/sum(df2006_sd$biz_size == 1, na.rm = TRUE)

##bound all survival rates together and reformated dataframe

survivalrate_sd <- cbind(sr3yr2001_sd, sr3yr2001sm_sd, sr3yr2001non_sd, sr5yr2001_sd, sr5yr2001sm_sd, sr5yr2001non_sd, 
                         sr10yr2001_sd, sr10yr2001sm_sd, sr10yr2001non_sd,
                         sr3yr2002_sd, sr3yr2002sm_sd, sr3yr2002non_sd, sr5yr2002_sd, sr5yr2002sm_sd, sr5yr2002non_sd, 
                         sr10yr2002_sd, sr10yr2002sm_sd, sr10yr2002non_sd,
                         sr3yr2003_sd, sr3yr2003sm_sd, sr3yr2003non_sd, sr5yr2003_sd, sr5yr2003sm_sd, sr5yr2003non_sd, 
                         sr10yr2003_sd, sr10yr2003sm_sd, sr10yr2003non_sd,
                         sr3yr2004_sd, sr3yr2004sm_sd, sr3yr2004non_sd, sr5yr2004_sd, sr5yr2004sm_sd, sr5yr2004non_sd, 
                         sr10yr2004_sd, sr10yr2004sm_sd, sr10yr2004non_sd,
                         sr3yr2005_sd, sr3yr2005sm_sd, sr3yr2005non_sd, sr5yr2005_sd, sr5yr2005sm_sd, sr5yr2005non_sd, 
                         sr10yr2005_sd, sr10yr2005sm_sd, sr10yr2005non_sd,
                         sr3yr2006_sd, sr3yr2006sm_sd, sr3yr2006non_sd, sr5yr2006_sd, sr5yr2006sm_sd, sr5yr2006non_sd, 
                         sr10yr2006_sd, sr10yr2006sm_sd, sr10yr2006non_sd)
srnames_sd <- c("3yr_2001_all_sd", "3yr_2001_sm_sd", 
                "3yr_2001_non_sd", "5yr_2001_all_sd", "5yr_2001_sm_sd", "5yr_2001_non_sd", "10yr_2001_all_sd", 
                "10yr_2001_sm_sd", "10yr_2001_non_sd",
                "3yr_2002_all_sd", "3yr_2002_sm_sd", "3yr_2002_non_sd", "5yr_2002_all_sd", "5yr_2002_sm_sd", "5yr_2002_non_sd", 
                "10yr_2002_all_sd", "10yr_2002_sm_sd", "10yr_2002_non_sd",
                "3yr_2003_all_sd", "3yr_2003_sm_sd", "3yr_2003_non_sd", "5yr_2003_all_sd", "5yr_2003_sm_sd", "5yr_2003_non_sd", 
                "10yr_2003_all_sd", "10yr_2003_sm_sd", "10yr_2003_non_sd",
                "3yr_2004_all_sd", "3yr_2004_sm_sd", "3yr_2004_non_sd", "5yr_2004_all_sd", "5yr_2004_sm_sd", "5yr_2004_non_sd", 
                "10yr_2004_all_sd", "10yr_2004_sm_sd", "10yr_2004_non_sd",
                "3yr_2005_all_sd", "3yr_2005_sm_sd", "3yr_2005_non_sd", "5yr_2005_all_sd", "5yr_2005_sm_sd", "5yr_2005_non_sd", 
                "10yr2005_all_sd", "10yr_2005_sm_sd", "10yr_2005_non_sd",
                "3yr_2006_all_sd", "3yr_2006_sm_sd", "3yr_2006_non_sd", "5yr_2006_all_sd", "5yr_2006_sm_sd", "5yr_2006_non_sd", 
                "10yr_2006_all_sd", "10yr_2006_sm_sd", "10yr_2006_non_sd")
colnames(survivalrate_sd) <- srnames_sd

sr_sd <- as.data.frame(survivalrate_sd)

survival.rates_sd <- sr_sd %>% 
  gather("3yr_2001_all_sd":"10yr_2006_non_sd", key = "time_year_type", value = "survivalrate_sd") %>% 
  separate(time_year_type, into = c("time", "year", "type"), sep = "_") %>% 
  arrange(time, year) %>% 
  unite("time_type", time, type, sep = "_") 

survival.rates_sd$time_type[18] <- "10yr_all"
survival.rates_sd$year[18] <- 2005

survival.rates_sd_44 <- survival.rates_sd %>% 
  spread(key = "time_type", value = "survivalrate_sd")

survival.rates_sd_44[, c(1, 5, 7, 6, 8, 10, 9, 2, 4, 3)]

##"48-49"####

df2001_sd_48 <- filter(df2001_sd, NAICS2 == "48-49")
df2002_sd_48 <- filter(df2002_sd, NAICS2 == "48-49")
df2003_sd_48 <- filter(df2003_sd, NAICS2 == "48-49")
df2004_sd_48 <- filter(df2004_sd, NAICS2 == "48-49")
df2005_sd_48 <- filter(df2005_sd, NAICS2 == "48-49")
df2006_sd_48 <- filter(df2006_sd, NAICS2 == "48-49")

##3-year
sr3yr2001_sd <- sum(df2001_sd_48$years.survived>=3)/nrow(df2001_sd)
sr3yr2001sm_sd <- sum((df2001_sd_48$years.survived>=3 & df2001_sd_48$biz_size == 0), na.rm = TRUE)/sum(df2001_sd$biz_size == 0, na.rm = TRUE)
sr3yr2001non_sd <- sum((df2001_sd_48$years.survived>=3 & df2001_sd_48$biz_size == 1), na.rm = TRUE)/sum(df2001_sd$biz_size == 1, na.rm = TRUE)

sr3yr2002_sd <- sum(df2002_sd_48$years.survived>=3)/nrow(df2002_sd) 
sr3yr2002sm_sd <- sum((df2002_sd_48$years.survived>=3 & df2002_sd_48$biz_size == 0), na.rm = TRUE)/sum(df2002_sd$biz_size == 0, na.rm = TRUE)
sr3yr2002non_sd <- sum((df2002_sd_48$years.survived>=3 & df2002_sd_48$biz_size == 1), na.rm = TRUE)/sum(df2002_sd$biz_size == 1, na.rm = TRUE)

sr3yr2003_sd <- sum(df2003_sd_48$years.survived>=3)/nrow(df2003_sd)
sr3yr2003sm_sd <- sum((df2003_sd_48$years.survived>=3 & df2003_sd_48$biz_size == 0), na.rm = TRUE)/sum(df2003_sd$biz_size == 0, na.rm = TRUE)
sr3yr2003non_sd <- sum((df2003_sd_48$years.survived>=3 & df2003_sd_48$biz_size == 1), na.rm = TRUE)/sum(df2003_sd$biz_size == 1, na.rm = TRUE)

sr3yr2004_sd <- sum(df2004_sd_48$years.survived>=3)/nrow(df2004_sd) 
sr3yr2004sm_sd <- sum((df2004_sd_48$years.survived>=3 & df2004_sd_48$biz_size == 0), na.rm = TRUE)/sum(df2004_sd$biz_size == 0, na.rm = TRUE)
sr3yr2004non_sd <- sum((df2004_sd_48$years.survived>=3 & df2004_sd_48$biz_size == 1), na.rm = TRUE)/sum(df2004_sd$biz_size == 1, na.rm = TRUE)

sr3yr2005_sd <- sum(df2005_sd_48$years.survived>=3)/nrow(df2005_sd) 
sr3yr2005sm_sd <- sum((df2005_sd_48$years.survived>=3 & df2005_sd_48$biz_size == 0), na.rm = TRUE)/sum(df2005_sd$biz_size == 0, na.rm = TRUE)
sr3yr2005non_sd <- sum((df2005_sd_48$years.survived>=3 & df2005_sd_48$biz_size == 1), na.rm = TRUE)/sum(df2005_sd$biz_size == 1, na.rm = TRUE)

sr3yr2006_sd <- sum(df2006_sd_48$years.survived>=3)/nrow(df2006_sd) 
sr3yr2006sm_sd <- sum((df2006_sd_48$years.survived>=3 & df2006_sd_48$biz_size == 0), na.rm = TRUE)/sum(df2006_sd$biz_size == 0, na.rm = TRUE)
sr3yr2006non_sd <- sum((df2006_sd_48$years.survived>=3 & df2006_sd_48$biz_size == 1), na.rm = TRUE)/sum(df2006_sd$biz_size == 1, na.rm = TRUE)

##5-year
sr5yr2001_sd <- sum(df2001_sd_48$years.survived>=5)/nrow(df2001_sd)
sr5yr2001sm_sd <- sum((df2001_sd_48$years.survived>=5 & df2001_sd_48$biz_size == 0), na.rm = TRUE)/sum(df2001_sd$biz_size == 0, na.rm = TRUE)
sr5yr2001non_sd <- sum((df2001_sd_48$years.survived>=5 & df2001_sd_48$biz_size == 1), na.rm = TRUE)/sum(df2001_sd$biz_size == 1, na.rm = TRUE)

sr5yr2002_sd <- sum(df2002_sd_48$years.survived>=5)/nrow(df2002_sd) 
sr5yr2002sm_sd <- sum((df2002_sd_48$years.survived>=5 & df2002_sd_48$biz_size == 0), na.rm = TRUE)/sum(df2002_sd$biz_size == 0, na.rm = TRUE)
sr5yr2002non_sd <- sum((df2002_sd_48$years.survived>=5 & df2002_sd_48$biz_size == 1), na.rm = TRUE)/sum(df2002_sd$biz_size == 1, na.rm = TRUE)

sr5yr2003_sd <- sum(df2003_sd_48$years.survived>=5)/nrow(df2003_sd)
sr5yr2003sm_sd <- sum((df2003_sd_48$years.survived>=5 & df2003_sd_48$biz_size == 0), na.rm = TRUE)/sum(df2003_sd$biz_size == 0, na.rm = TRUE)
sr5yr2003non_sd <- sum((df2003_sd_48$years.survived>=5 & df2003_sd_48$biz_size == 1), na.rm = TRUE)/sum(df2003_sd$biz_size == 1, na.rm = TRUE)

sr5yr2004_sd <- sum(df2004_sd_48$years.survived>=5)/nrow(df2004_sd) 
sr5yr2004sm_sd <- sum((df2004_sd_48$years.survived>=5 & df2004_sd_48$biz_size == 0), na.rm = TRUE)/sum(df2004_sd$biz_size == 0, na.rm = TRUE)
sr5yr2004non_sd <- sum((df2004_sd_48$years.survived>=5 & df2004_sd_48$biz_size == 1), na.rm = TRUE)/sum(df2004_sd$biz_size == 1, na.rm = TRUE)

sr5yr2005_sd <- sum(df2005_sd_48$years.survived>=5)/nrow(df2005_sd) 
sr5yr2005sm_sd <- sum((df2005_sd_48$years.survived>=5 & df2005_sd_48$biz_size == 0), na.rm = TRUE)/sum(df2005_sd$biz_size == 0, na.rm = TRUE)
sr5yr2005non_sd <- sum((df2005_sd_48$years.survived>=5 & df2005_sd_48$biz_size == 1), na.rm = TRUE)/sum(df2005_sd$biz_size == 1, na.rm = TRUE)

sr5yr2006_sd <- sum(df2006_sd_48$years.survived>=5)/nrow(df2006_sd) 
sr5yr2006sm_sd <- sum((df2006_sd_48$years.survived>=5 & df2006_sd_48$biz_size == 0), na.rm = TRUE)/sum(df2006_sd$biz_size == 0, na.rm = TRUE)
sr5yr2006non_sd <- sum((df2006_sd_48$years.survived>=5 & df2006_sd_48$biz_size == 1), na.rm = TRUE)/sum(df2006_sd$biz_size == 1, na.rm = TRUE)


##10-year
sr10yr2001_sd <- (sum(df2001_sd_48$years.survived>=10))/nrow(df2001_sd) 
sr10yr2001sm_sd <- sum((df2001_sd_48$years.survived>=10 & df2001_sd_48$biz_size == 0), na.rm = TRUE)/sum(df2001_sd$biz_size == 0, na.rm = TRUE)
sr10yr2001non_sd <- sum((df2001_sd_48$years.survived>=10 & df2001_sd_48$biz_size == 1), na.rm = TRUE)/sum(df2002_sd$biz_size == 1, na.rm = TRUE)


sr10yr2002_sd <- sum(df2002_sd_48$years.survived>=10)/nrow(df2002_sd) 
sr10yr2002sm_sd <- sum((df2002_sd_48$years.survived>=10 & df2002_sd_48$biz_size == 0), na.rm = TRUE)/sum(df2002_sd$biz_size == 0, na.rm = TRUE)
sr10yr2002non_sd <- sum((df2002_sd_48$years.survived>=10 & df2002_sd_48$biz_size == 1), na.rm = TRUE)/sum(df2002_sd$biz_size == 1, na.rm = TRUE)

sr10yr2003_sd <- sum(df2003_sd_48$years.survived>=10)/nrow(df2003_sd)
sr10yr2003sm_sd <- sum((df2003_sd_48$years.survived>=10 & df2003_sd_48$biz_size == 0), na.rm = TRUE)/sum(df2003_sd$biz_size == 0, na.rm = TRUE)
sr10yr2003non_sd <- sum((df2003_sd_48$years.survived>=10 & df2003_sd_48$biz_size == 1), na.rm = TRUE)/sum(df2003_sd$biz_size == 1, na.rm = TRUE)

sr10yr2004_sd <- sum(df2004_sd_48$years.survived>=10)/nrow(df2004_sd) 
sr10yr2004sm_sd <- sum((df2004_sd_48$years.survived>=10 & df2004_sd_48$biz_size == 0), na.rm = TRUE)/sum(df2004_sd$biz_size == 0, na.rm = TRUE)
sr10yr2004non_sd <- sum((df2004_sd_48$years.survived>=10 & df2004_sd_48$biz_size == 1), na.rm = TRUE)/sum(df2004_sd$biz_size == 1, na.rm = TRUE)

sr10yr2005_sd <- sum(df2005_sd_48$years.survived>=10)/nrow(df2005_sd) 
sr10yr2005sm_sd <- sum((df2005_sd_48$years.survived>=10 & df2005_sd_48$biz_size == 0), na.rm = TRUE)/sum(df2005_sd$biz_size == 0, na.rm = TRUE)
sr10yr2005non_sd <- sum((df2005_sd_48$years.survived>=10 & df2005_sd_48$biz_size == 1), na.rm = TRUE)/sum(df2005_sd$biz_size == 1, na.rm = TRUE)

sr10yr2006_sd <- sum(df2006_sd_48$years.survived>=10)/nrow(df2006_sd) 
sr10yr2006sm_sd <- sum((df2006_sd_48$years.survived>=10 & df2006_sd_48$biz_size == 0), na.rm = TRUE)/sum(df2006_sd$biz_size == 0, na.rm = TRUE)
sr10yr2006non_sd <- sum((df2006_sd_48$years.survived>=10 & df2006_sd_48$biz_size == 1), na.rm = TRUE)/sum(df2006_sd$biz_size == 1, na.rm = TRUE)

##bound all survival rates together and reformated dataframe

survivalrate_sd <- cbind(sr3yr2001_sd, sr3yr2001sm_sd, sr3yr2001non_sd, sr5yr2001_sd, sr5yr2001sm_sd, sr5yr2001non_sd, 
                         sr10yr2001_sd, sr10yr2001sm_sd, sr10yr2001non_sd,
                         sr3yr2002_sd, sr3yr2002sm_sd, sr3yr2002non_sd, sr5yr2002_sd, sr5yr2002sm_sd, sr5yr2002non_sd, 
                         sr10yr2002_sd, sr10yr2002sm_sd, sr10yr2002non_sd,
                         sr3yr2003_sd, sr3yr2003sm_sd, sr3yr2003non_sd, sr5yr2003_sd, sr5yr2003sm_sd, sr5yr2003non_sd, 
                         sr10yr2003_sd, sr10yr2003sm_sd, sr10yr2003non_sd,
                         sr3yr2004_sd, sr3yr2004sm_sd, sr3yr2004non_sd, sr5yr2004_sd, sr5yr2004sm_sd, sr5yr2004non_sd, 
                         sr10yr2004_sd, sr10yr2004sm_sd, sr10yr2004non_sd,
                         sr3yr2005_sd, sr3yr2005sm_sd, sr3yr2005non_sd, sr5yr2005_sd, sr5yr2005sm_sd, sr5yr2005non_sd, 
                         sr10yr2005_sd, sr10yr2005sm_sd, sr10yr2005non_sd,
                         sr3yr2006_sd, sr3yr2006sm_sd, sr3yr2006non_sd, sr5yr2006_sd, sr5yr2006sm_sd, sr5yr2006non_sd, 
                         sr10yr2006_sd, sr10yr2006sm_sd, sr10yr2006non_sd)
srnames_sd <- c("3yr_2001_all_sd", "3yr_2001_sm_sd", 
                "3yr_2001_non_sd", "5yr_2001_all_sd", "5yr_2001_sm_sd", "5yr_2001_non_sd", "10yr_2001_all_sd", 
                "10yr_2001_sm_sd", "10yr_2001_non_sd",
                "3yr_2002_all_sd", "3yr_2002_sm_sd", "3yr_2002_non_sd", "5yr_2002_all_sd", "5yr_2002_sm_sd", "5yr_2002_non_sd", 
                "10yr_2002_all_sd", "10yr_2002_sm_sd", "10yr_2002_non_sd",
                "3yr_2003_all_sd", "3yr_2003_sm_sd", "3yr_2003_non_sd", "5yr_2003_all_sd", "5yr_2003_sm_sd", "5yr_2003_non_sd", 
                "10yr_2003_all_sd", "10yr_2003_sm_sd", "10yr_2003_non_sd",
                "3yr_2004_all_sd", "3yr_2004_sm_sd", "3yr_2004_non_sd", "5yr_2004_all_sd", "5yr_2004_sm_sd", "5yr_2004_non_sd", 
                "10yr_2004_all_sd", "10yr_2004_sm_sd", "10yr_2004_non_sd",
                "3yr_2005_all_sd", "3yr_2005_sm_sd", "3yr_2005_non_sd", "5yr_2005_all_sd", "5yr_2005_sm_sd", "5yr_2005_non_sd", 
                "10yr2005_all_sd", "10yr_2005_sm_sd", "10yr_2005_non_sd",
                "3yr_2006_all_sd", "3yr_2006_sm_sd", "3yr_2006_non_sd", "5yr_2006_all_sd", "5yr_2006_sm_sd", "5yr_2006_non_sd", 
                "10yr_2006_all_sd", "10yr_2006_sm_sd", "10yr_2006_non_sd")
colnames(survivalrate_sd) <- srnames_sd

sr_sd <- as.data.frame(survivalrate_sd)

survival.rates_sd <- sr_sd %>% 
  gather("3yr_2001_all_sd":"10yr_2006_non_sd", key = "time_year_type", value = "survivalrate_sd") %>% 
  separate(time_year_type, into = c("time", "year", "type"), sep = "_") %>% 
  arrange(time, year) %>% 
  unite("time_type", time, type, sep = "_") 

survival.rates_sd$time_type[18] <- "10yr_all"
survival.rates_sd$year[18] <- 2005

survival.rates_sd_48 <- survival.rates_sd %>% 
  spread(key = "time_type", value = "survivalrate_sd")

survival.rates_sd_48[, c(1, 5, 7, 6, 8, 10, 9, 2, 4, 3)]

##51####

df2001_sd_51 <- filter(df2001_sd, NAICS2 == 51)
df2002_sd_51 <- filter(df2002_sd, NAICS2 == 51)
df2003_sd_51 <- filter(df2003_sd, NAICS2 == 51)
df2004_sd_51 <- filter(df2004_sd, NAICS2 == 51)
df2005_sd_51 <- filter(df2005_sd, NAICS2 == 51)
df2006_sd_51 <- filter(df2006_sd, NAICS2 == 51)

##3-year
sr3yr2001_sd <- sum(df2001_sd_51$years.survived>=3)/nrow(df2001_sd)
sr3yr2001sm_sd <- sum((df2001_sd_51$years.survived>=3 & df2001_sd_51$biz_size == 0), na.rm = TRUE)/sum(df2001_sd$biz_size == 0, na.rm = TRUE)
sr3yr2001non_sd <- sum((df2001_sd_51$years.survived>=3 & df2001_sd_51$biz_size == 1), na.rm = TRUE)/sum(df2001_sd$biz_size == 1, na.rm = TRUE)

sr3yr2002_sd <- sum(df2002_sd_51$years.survived>=3)/nrow(df2002_sd) 
sr3yr2002sm_sd <- sum((df2002_sd_51$years.survived>=3 & df2002_sd_51$biz_size == 0), na.rm = TRUE)/sum(df2002_sd$biz_size == 0, na.rm = TRUE)
sr3yr2002non_sd <- sum((df2002_sd_51$years.survived>=3 & df2002_sd_51$biz_size == 1), na.rm = TRUE)/sum(df2002_sd$biz_size == 1, na.rm = TRUE)

sr3yr2003_sd <- sum(df2003_sd_51$years.survived>=3)/nrow(df2003_sd)
sr3yr2003sm_sd <- sum((df2003_sd_51$years.survived>=3 & df2003_sd_51$biz_size == 0), na.rm = TRUE)/sum(df2003_sd$biz_size == 0, na.rm = TRUE)
sr3yr2003non_sd <- sum((df2003_sd_51$years.survived>=3 & df2003_sd_51$biz_size == 1), na.rm = TRUE)/sum(df2003_sd$biz_size == 1, na.rm = TRUE)

sr3yr2004_sd <- sum(df2004_sd_51$years.survived>=3)/nrow(df2004_sd) 
sr3yr2004sm_sd <- sum((df2004_sd_51$years.survived>=3 & df2004_sd_51$biz_size == 0), na.rm = TRUE)/sum(df2004_sd$biz_size == 0, na.rm = TRUE)
sr3yr2004non_sd <- sum((df2004_sd_51$years.survived>=3 & df2004_sd_51$biz_size == 1), na.rm = TRUE)/sum(df2004_sd$biz_size == 1, na.rm = TRUE)

sr3yr2005_sd <- sum(df2005_sd$years.survived>=3)/nrow(df2005_sd) 
sr3yr2005sm_sd <- sum((df2005_sd$years.survived>=3 & df2005_sd$biz_size == 0), na.rm = TRUE)/sum(df2005_sd$biz_size == 0, na.rm = TRUE)
sr3yr2005non_sd <- sum((df2005_sd$years.survived>=3 & df2005_sd$biz_size == 1), na.rm = TRUE)/sum(df2005_sd$biz_size == 1, na.rm = TRUE)

sr3yr2006_sd <- sum(df2006_sd_51$years.survived>=3)/nrow(df2006_sd) 
sr3yr2006sm_sd <- sum((df2006_sd_51$years.survived>=3 & df2006_sd_51$biz_size == 0), na.rm = TRUE)/sum(df2006_sd$biz_size == 0, na.rm = TRUE)
sr3yr2006non_sd <- sum((df2006_sd_51$years.survived>=3 & df2006_sd_51$biz_size == 1), na.rm = TRUE)/sum(df2006_sd$biz_size == 1, na.rm = TRUE)

##5-year
sr5yr2001_sd <- sum(df2001_sd_51$years.survived>=5)/nrow(df2001_sd)
sr5yr2001sm_sd <- sum((df2001_sd_51$years.survived>=5 & df2001_sd_51$biz_size == 0), na.rm = TRUE)/sum(df2001_sd$biz_size == 0, na.rm = TRUE)
sr5yr2001non_sd <- sum((df2001_sd_51$years.survived>=5 & df2001_sd_51$biz_size == 1), na.rm = TRUE)/sum(df2001_sd$biz_size == 1, na.rm = TRUE)

sr5yr2002_sd <- sum(df2002_sd_51$years.survived>=5)/nrow(df2002_sd) 
sr5yr2002sm_sd <- sum((df2002_sd_51$years.survived>=5 & df2002_sd_51$biz_size == 0), na.rm = TRUE)/sum(df2002_sd$biz_size == 0, na.rm = TRUE)
sr5yr2002non_sd <- sum((df2002_sd_51$years.survived>=5 & df2002_sd_51$biz_size == 1), na.rm = TRUE)/sum(df2002_sd$biz_size == 1, na.rm = TRUE)

sr5yr2003_sd <- sum(df2003_sd_51$years.survived>=5)/nrow(df2003_sd)
sr5yr2003sm_sd <- sum((df2003_sd_51$years.survived>=5 & df2003_sd_51$biz_size == 0), na.rm = TRUE)/sum(df2003_sd$biz_size == 0, na.rm = TRUE)
sr5yr2003non_sd <- sum((df2003_sd_51$years.survived>=5 & df2003_sd_51$biz_size == 1), na.rm = TRUE)/sum(df2003_sd$biz_size == 1, na.rm = TRUE)

sr5yr2004_sd <- sum(df2004_sd_51$years.survived>=5)/nrow(df2004_sd) 
sr5yr2004sm_sd <- sum((df2004_sd_51$years.survived>=5 & df2004_sd_51$biz_size == 0), na.rm = TRUE)/sum(df2004_sd$biz_size == 0, na.rm = TRUE)
sr5yr2004non_sd <- sum((df2004_sd_51$years.survived>=5 & df2004_sd_51$biz_size == 1), na.rm = TRUE)/sum(df2004_sd$biz_size == 1, na.rm = TRUE)

sr5yr2005_sd <- sum(df2005_sd_51$years.survived>=5)/nrow(df2005_sd) 
sr5yr2005sm_sd <- sum((df2005_sd_51$years.survived>=5 & df2005_sd_51$biz_size == 0), na.rm = TRUE)/sum(df2005_sd$biz_size == 0, na.rm = TRUE)
sr5yr2005non_sd <- sum((df2005_sd_51$years.survived>=5 & df2005_sd_51$biz_size == 1), na.rm = TRUE)/sum(df2005_sd$biz_size == 1, na.rm = TRUE)

sr5yr2006_sd <- sum(df2006_sd_51$years.survived>=5)/nrow(df2006_sd) 
sr5yr2006sm_sd <- sum((df2006_sd_51$years.survived>=5 & df2006_sd_51$biz_size == 0), na.rm = TRUE)/sum(df2006_sd$biz_size == 0, na.rm = TRUE)
sr5yr2006non_sd <- sum((df2006_sd_51$years.survived>=5 & df2006_sd_51$biz_size == 1), na.rm = TRUE)/sum(df2006_sd$biz_size == 1, na.rm = TRUE)


##10-year
sr10yr2001_sd <- (sum(df2001_sd_51$years.survived>=10))/nrow(df2001_sd) 
sr10yr2001sm_sd <- sum((df2001_sd_51$years.survived>=10 & df2001_sd_51$biz_size == 0), na.rm = TRUE)/sum(df2001_sd$biz_size == 0, na.rm = TRUE)
sr10yr2001non_sd <- sum((df2001_sd_51$years.survived>=10 & df2001_sd_51$biz_size == 1), na.rm = TRUE)/sum(df2002_sd$biz_size == 1, na.rm = TRUE)


sr10yr2002_sd <- sum(df2002_sd_51$years.survived>=10)/nrow(df2002_sd) 
sr10yr2002sm_sd <- sum((df2002_sd_51$years.survived>=10 & df2002_sd_51$biz_size == 0), na.rm = TRUE)/sum(df2002_sd$biz_size == 0, na.rm = TRUE)
sr10yr2002non_sd <- sum((df2002_sd_51$years.survived>=10 & df2002_sd_51$biz_size == 1), na.rm = TRUE)/sum(df2002_sd$biz_size == 1, na.rm = TRUE)

sr10yr2003_sd <- sum(df2003_sd_51$years.survived>=10)/nrow(df2003_sd)
sr10yr2003sm_sd <- sum((df2003_sd_51$years.survived>=10 & df2003_sd_51$biz_size == 0), na.rm = TRUE)/sum(df2003_sd$biz_size == 0, na.rm = TRUE)
sr10yr2003non_sd <- sum((df2003_sd_51$years.survived>=10 & df2003_sd_51$biz_size == 1), na.rm = TRUE)/sum(df2003_sd$biz_size == 1, na.rm = TRUE)

sr10yr2004_sd <- sum(df2004_sd_51$years.survived>=10)/nrow(df2004_sd) 
sr10yr2004sm_sd <- sum((df2004_sd_51$years.survived>=10 & df2004_sd_51$biz_size == 0), na.rm = TRUE)/sum(df2004_sd$biz_size == 0, na.rm = TRUE)
sr10yr2004non_sd <- sum((df2004_sd_51$years.survived>=10 & df2004_sd_51$biz_size == 1), na.rm = TRUE)/sum(df2004_sd$biz_size == 1, na.rm = TRUE)

sr10yr2005_sd <- sum(df2005_sd_51$years.survived>=10)/nrow(df2005_sd) 
sr10yr2005sm_sd <- sum((df2005_sd_51$years.survived>=10 & df2005_sd_51$biz_size == 0), na.rm = TRUE)/sum(df2005_sd$biz_size == 0, na.rm = TRUE)
sr10yr2005non_sd <- sum((df2005_sd_51$years.survived>=10 & df2005_sd_51$biz_size == 1), na.rm = TRUE)/sum(df2005_sd$biz_size == 1, na.rm = TRUE)

sr10yr2006_sd <- sum(df2006_sd_51$years.survived>=10)/nrow(df2006_sd) 
sr10yr2006sm_sd <- sum((df2006_sd_51$years.survived>=10 & df2006_sd_51$biz_size == 0), na.rm = TRUE)/sum(df2006_sd$biz_size == 0, na.rm = TRUE)
sr10yr2006non_sd <- sum((df2006_sd_51$years.survived>=10 & df2006_sd_51$biz_size == 1), na.rm = TRUE)/sum(df2006_sd$biz_size == 1, na.rm = TRUE)

##bound all survival rates together and reformated dataframe

survivalrate_sd <- cbind(sr3yr2001_sd, sr3yr2001sm_sd, sr3yr2001non_sd, sr5yr2001_sd, sr5yr2001sm_sd, sr5yr2001non_sd, 
                         sr10yr2001_sd, sr10yr2001sm_sd, sr10yr2001non_sd,
                         sr3yr2002_sd, sr3yr2002sm_sd, sr3yr2002non_sd, sr5yr2002_sd, sr5yr2002sm_sd, sr5yr2002non_sd, 
                         sr10yr2002_sd, sr10yr2002sm_sd, sr10yr2002non_sd,
                         sr3yr2003_sd, sr3yr2003sm_sd, sr3yr2003non_sd, sr5yr2003_sd, sr5yr2003sm_sd, sr5yr2003non_sd, 
                         sr10yr2003_sd, sr10yr2003sm_sd, sr10yr2003non_sd,
                         sr3yr2004_sd, sr3yr2004sm_sd, sr3yr2004non_sd, sr5yr2004_sd, sr5yr2004sm_sd, sr5yr2004non_sd, 
                         sr10yr2004_sd, sr10yr2004sm_sd, sr10yr2004non_sd,
                         sr3yr2005_sd, sr3yr2005sm_sd, sr3yr2005non_sd, sr5yr2005_sd, sr5yr2005sm_sd, sr5yr2005non_sd, 
                         sr10yr2005_sd, sr10yr2005sm_sd, sr10yr2005non_sd,
                         sr3yr2006_sd, sr3yr2006sm_sd, sr3yr2006non_sd, sr5yr2006_sd, sr5yr2006sm_sd, sr5yr2006non_sd, 
                         sr10yr2006_sd, sr10yr2006sm_sd, sr10yr2006non_sd)
srnames_sd <- c("3yr_2001_all_sd", "3yr_2001_sm_sd", 
                "3yr_2001_non_sd", "5yr_2001_all_sd", "5yr_2001_sm_sd", "5yr_2001_non_sd", "10yr_2001_all_sd", 
                "10yr_2001_sm_sd", "10yr_2001_non_sd",
                "3yr_2002_all_sd", "3yr_2002_sm_sd", "3yr_2002_non_sd", "5yr_2002_all_sd", "5yr_2002_sm_sd", "5yr_2002_non_sd", 
                "10yr_2002_all_sd", "10yr_2002_sm_sd", "10yr_2002_non_sd",
                "3yr_2003_all_sd", "3yr_2003_sm_sd", "3yr_2003_non_sd", "5yr_2003_all_sd", "5yr_2003_sm_sd", "5yr_2003_non_sd", 
                "10yr_2003_all_sd", "10yr_2003_sm_sd", "10yr_2003_non_sd",
                "3yr_2004_all_sd", "3yr_2004_sm_sd", "3yr_2004_non_sd", "5yr_2004_all_sd", "5yr_2004_sm_sd", "5yr_2004_non_sd", 
                "10yr_2004_all_sd", "10yr_2004_sm_sd", "10yr_2004_non_sd",
                "3yr_2005_all_sd", "3yr_2005_sm_sd", "3yr_2005_non_sd", "5yr_2005_all_sd", "5yr_2005_sm_sd", "5yr_2005_non_sd", 
                "10yr2005_all_sd", "10yr_2005_sm_sd", "10yr_2005_non_sd",
                "3yr_2006_all_sd", "3yr_2006_sm_sd", "3yr_2006_non_sd", "5yr_2006_all_sd", "5yr_2006_sm_sd", "5yr_2006_non_sd", 
                "10yr_2006_all_sd", "10yr_2006_sm_sd", "10yr_2006_non_sd")
colnames(survivalrate_sd) <- srnames_sd

sr_sd <- as.data.frame(survivalrate_sd)

survival.rates_sd <- sr_sd %>% 
  gather("3yr_2001_all_sd":"10yr_2006_non_sd", key = "time_year_type", value = "survivalrate_sd") %>% 
  separate(time_year_type, into = c("time", "year", "type"), sep = "_") %>% 
  arrange(time, year) %>% 
  unite("time_type", time, type, sep = "_") 

survival.rates_sd$time_type[18] <- "10yr_all"
survival.rates_sd$year[18] <- 2005

survival.rates_sd_51 <- survival.rates_sd %>% 
  spread(key = "time_type", value = "survivalrate_sd")

survival.rates_sd_51[, c(1, 5, 7, 6, 8, 10, 9, 2, 4, 3)]

##52####

df2001_sd_52 <- filter(df2001_sd, NAICS2 == 52)
df2002_sd_52 <- filter(df2002_sd, NAICS2 == 52)
df2003_sd_52 <- filter(df2003_sd, NAICS2 == 52)
df2004_sd_52 <- filter(df2004_sd, NAICS2 == 52)
df2005_sd_52 <- filter(df2005_sd, NAICS2 == 52)
df2006_sd_52 <- filter(df2006_sd, NAICS2 == 52)

##3-year
sr3yr2001_sd <- sum(df2001_sd_52$years.survived>=3)/nrow(df2001_sd)
sr3yr2001sm_sd <- sum((df2001_sd_52$years.survived>=3 & df2001_sd_52$biz_size == 0), na.rm = TRUE)/sum(df2001_sd$biz_size == 0, na.rm = TRUE)
sr3yr2001non_sd <- sum((df2001_sd_52$years.survived>=3 & df2001_sd_52$biz_size == 1), na.rm = TRUE)/sum(df2001_sd$biz_size == 1, na.rm = TRUE)

sr3yr2002_sd <- sum(df2002_sd_52$years.survived>=3)/nrow(df2002_sd) 
sr3yr2002sm_sd <- sum((df2002_sd_52$years.survived>=3 & df2002_sd_52$biz_size == 0), na.rm = TRUE)/sum(df2002_sd$biz_size == 0, na.rm = TRUE)
sr3yr2002non_sd <- sum((df2002_sd_52$years.survived>=3 & df2002_sd_52$biz_size == 1), na.rm = TRUE)/sum(df2002_sd$biz_size == 1, na.rm = TRUE)

sr3yr2003_sd <- sum(df2003_sd_52$years.survived>=3)/nrow(df2003_sd)
sr3yr2003sm_sd <- sum((df2003_sd_52$years.survived>=3 & df2003_sd_52$biz_size == 0), na.rm = TRUE)/sum(df2003_sd$biz_size == 0, na.rm = TRUE)
sr3yr2003non_sd <- sum((df2003_sd_52$years.survived>=3 & df2003_sd_52$biz_size == 1), na.rm = TRUE)/sum(df2003_sd$biz_size == 1, na.rm = TRUE)

sr3yr2004_sd <- sum(df2004_sd_52$years.survived>=3)/nrow(df2004_sd) 
sr3yr2004sm_sd <- sum((df2004_sd_52$years.survived>=3 & df2004_sd_52$biz_size == 0), na.rm = TRUE)/sum(df2004_sd$biz_size == 0, na.rm = TRUE)
sr3yr2004non_sd <- sum((df2004_sd_52$years.survived>=3 & df2004_sd_52$biz_size == 1), na.rm = TRUE)/sum(df2004_sd$biz_size == 1, na.rm = TRUE)

sr3yr2005_sd <- sum(df2005_sd_52$years.survived>=3)/nrow(df2005_sd) 
sr3yr2005sm_sd <- sum((df2005_sd_52$years.survived>=3 & df2005_sd_52$biz_size == 0), na.rm = TRUE)/sum(df2005_sd$biz_size == 0, na.rm = TRUE)
sr3yr2005non_sd <- sum((df2005_sd_52$years.survived>=3 & df2005_sd_52$biz_size == 1), na.rm = TRUE)/sum(df2005_sd$biz_size == 1, na.rm = TRUE)

sr3yr2006_sd <- sum(df2006_sd_52$years.survived>=3)/nrow(df2006_sd) 
sr3yr2006sm_sd <- sum((df2006_sd_52$years.survived>=3 & df2006_sd_52$biz_size == 0), na.rm = TRUE)/sum(df2006_sd$biz_size == 0, na.rm = TRUE)
sr3yr2006non_sd <- sum((df2006_sd_52$years.survived>=3 & df2006_sd_52$biz_size == 1), na.rm = TRUE)/sum(df2006_sd$biz_size == 1, na.rm = TRUE)

##5-year
sr5yr2001_sd <- sum(df2001_sd_52$years.survived>=5)/nrow(df2001_sd)
sr5yr2001sm_sd <- sum((df2001_sd_52$years.survived>=5 & df2001_sd_52$biz_size == 0), na.rm = TRUE)/sum(df2001_sd$biz_size == 0, na.rm = TRUE)
sr5yr2001non_sd <- sum((df2001_sd_52$years.survived>=5 & df2001_sd_52$biz_size == 1), na.rm = TRUE)/sum(df2001_sd$biz_size == 1, na.rm = TRUE)

sr5yr2002_sd <- sum(df2002_sd_52$years.survived>=5)/nrow(df2002_sd) 
sr5yr2002sm_sd <- sum((df2002_sd_52$years.survived>=5 & df2002_sd_52$biz_size == 0), na.rm = TRUE)/sum(df2002_sd$biz_size == 0, na.rm = TRUE)
sr5yr2002non_sd <- sum((df2002_sd_52$years.survived>=5 & df2002_sd_52$biz_size == 1), na.rm = TRUE)/sum(df2002_sd$biz_size == 1, na.rm = TRUE)

sr5yr2003_sd <- sum(df2003_sd_52$years.survived>=5)/nrow(df2003_sd)
sr5yr2003sm_sd <- sum((df2003_sd_52$years.survived>=5 & df2003_sd_52$biz_size == 0), na.rm = TRUE)/sum(df2003_sd$biz_size == 0, na.rm = TRUE)
sr5yr2003non_sd <- sum((df2003_sd_52$years.survived>=5 & df2003_sd_52$biz_size == 1), na.rm = TRUE)/sum(df2003_sd$biz_size == 1, na.rm = TRUE)

sr5yr2004_sd <- sum(df2004_sd_52$years.survived>=5)/nrow(df2004_sd) 
sr5yr2004sm_sd <- sum((df2004_sd_52$years.survived>=5 & df2004_sd_52$biz_size == 0), na.rm = TRUE)/sum(df2004_sd$biz_size == 0, na.rm = TRUE)
sr5yr2004non_sd <- sum((df2004_sd_52$years.survived>=5 & df2004_sd_52$biz_size == 1), na.rm = TRUE)/sum(df2004_sd$biz_size == 1, na.rm = TRUE)

sr5yr2005_sd <- sum(df2005_sd_52$years.survived>=5)/nrow(df2005_sd) 
sr5yr2005sm_sd <- sum((df2005_sd_52$years.survived>=5 & df2005_sd_52$biz_size == 0), na.rm = TRUE)/sum(df2005_sd$biz_size == 0, na.rm = TRUE)
sr5yr2005non_sd <- sum((df2005_sd_52$years.survived>=5 & df2005_sd_52$biz_size == 1), na.rm = TRUE)/sum(df2005_sd$biz_size == 1, na.rm = TRUE)

sr5yr2006_sd <- sum(df2006_sd_52$years.survived>=5)/nrow(df2006_sd) 
sr5yr2006sm_sd <- sum((df2006_sd_52$years.survived>=5 & df2006_sd_52$biz_size == 0), na.rm = TRUE)/sum(df2006_sd$biz_size == 0, na.rm = TRUE)
sr5yr2006non_sd <- sum((df2006_sd_52$years.survived>=5 & df2006_sd_52$biz_size == 1), na.rm = TRUE)/sum(df2006_sd$biz_size == 1, na.rm = TRUE)


##10-year
sr10yr2001_sd <- (sum(df2001_sd_52$years.survived>=10))/nrow(df2001_sd) 
sr10yr2001sm_sd <- sum((df2001_sd_52$years.survived>=10 & df2001_sd_52$biz_size == 0), na.rm = TRUE)/sum(df2001_sd$biz_size == 0, na.rm = TRUE)
sr10yr2001non_sd <- sum((df2001_sd_52$years.survived>=10 & df2001_sd_52$biz_size == 1), na.rm = TRUE)/sum(df2002_sd$biz_size == 1, na.rm = TRUE)


sr10yr2002_sd <- sum(df2002_sd_52$years.survived>=10)/nrow(df2002_sd) 
sr10yr2002sm_sd <- sum((df2002_sd_52$years.survived>=10 & df2002_sd_52$biz_size == 0), na.rm = TRUE)/sum(df2002_sd$biz_size == 0, na.rm = TRUE)
sr10yr2002non_sd <- sum((df2002_sd_52$years.survived>=10 & df2002_sd_52$biz_size == 1), na.rm = TRUE)/sum(df2002_sd$biz_size == 1, na.rm = TRUE)

sr10yr2003_sd <- sum(df2003_sd_52$years.survived>=10)/nrow(df2003_sd)
sr10yr2003sm_sd <- sum((df2003_sd_52$years.survived>=10 & df2003_sd_52$biz_size == 0), na.rm = TRUE)/sum(df2003_sd$biz_size == 0, na.rm = TRUE)
sr10yr2003non_sd <- sum((df2003_sd_52$years.survived>=10 & df2003_sd_52$biz_size == 1), na.rm = TRUE)/sum(df2003_sd$biz_size == 1, na.rm = TRUE)

sr10yr2004_sd <- sum(df2004_sd_52$years.survived>=10)/nrow(df2004_sd) 
sr10yr2004sm_sd <- sum((df2004_sd_52$years.survived>=10 & df2004_sd_52$biz_size == 0), na.rm = TRUE)/sum(df2004_sd$biz_size == 0, na.rm = TRUE)
sr10yr2004non_sd <- sum((df2004_sd_52$years.survived>=10 & df2004_sd_52$biz_size == 1), na.rm = TRUE)/sum(df2004_sd$biz_size == 1, na.rm = TRUE)

sr10yr2005_sd <- sum(df2005_sd_52$years.survived>=10)/nrow(df2005_sd) 
sr10yr2005sm_sd <- sum((df2005_sd_52$years.survived>=10 & df2005_sd_52$biz_size == 0), na.rm = TRUE)/sum(df2005_sd$biz_size == 0, na.rm = TRUE)
sr10yr2005non_sd <- sum((df2005_sd_52$years.survived>=10 & df2005_sd_52$biz_size == 1), na.rm = TRUE)/sum(df2005_sd$biz_size == 1, na.rm = TRUE)

sr10yr2006_sd <- sum(df2006_sd_52$years.survived>=10)/nrow(df2006_sd) 
sr10yr2006sm_sd <- sum((df2006_sd_52$years.survived>=10 & df2006_sd_52$biz_size == 0), na.rm = TRUE)/sum(df2006_sd$biz_size == 0, na.rm = TRUE)
sr10yr2006non_sd <- sum((df2006_sd_52$years.survived>=10 & df2006_sd_52$biz_size == 1), na.rm = TRUE)/sum(df2006_sd$biz_size == 1, na.rm = TRUE)

##bound all survival rates together and reformated dataframe

survivalrate_sd <- cbind(sr3yr2001_sd, sr3yr2001sm_sd, sr3yr2001non_sd, sr5yr2001_sd, sr5yr2001sm_sd, sr5yr2001non_sd, 
                         sr10yr2001_sd, sr10yr2001sm_sd, sr10yr2001non_sd,
                         sr3yr2002_sd, sr3yr2002sm_sd, sr3yr2002non_sd, sr5yr2002_sd, sr5yr2002sm_sd, sr5yr2002non_sd, 
                         sr10yr2002_sd, sr10yr2002sm_sd, sr10yr2002non_sd,
                         sr3yr2003_sd, sr3yr2003sm_sd, sr3yr2003non_sd, sr5yr2003_sd, sr5yr2003sm_sd, sr5yr2003non_sd, 
                         sr10yr2003_sd, sr10yr2003sm_sd, sr10yr2003non_sd,
                         sr3yr2004_sd, sr3yr2004sm_sd, sr3yr2004non_sd, sr5yr2004_sd, sr5yr2004sm_sd, sr5yr2004non_sd, 
                         sr10yr2004_sd, sr10yr2004sm_sd, sr10yr2004non_sd,
                         sr3yr2005_sd, sr3yr2005sm_sd, sr3yr2005non_sd, sr5yr2005_sd, sr5yr2005sm_sd, sr5yr2005non_sd, 
                         sr10yr2005_sd, sr10yr2005sm_sd, sr10yr2005non_sd,
                         sr3yr2006_sd, sr3yr2006sm_sd, sr3yr2006non_sd, sr5yr2006_sd, sr5yr2006sm_sd, sr5yr2006non_sd, 
                         sr10yr2006_sd, sr10yr2006sm_sd, sr10yr2006non_sd)
srnames_sd <- c("3yr_2001_all_sd", "3yr_2001_sm_sd", 
                "3yr_2001_non_sd", "5yr_2001_all_sd", "5yr_2001_sm_sd", "5yr_2001_non_sd", "10yr_2001_all_sd", 
                "10yr_2001_sm_sd", "10yr_2001_non_sd",
                "3yr_2002_all_sd", "3yr_2002_sm_sd", "3yr_2002_non_sd", "5yr_2002_all_sd", "5yr_2002_sm_sd", "5yr_2002_non_sd", 
                "10yr_2002_all_sd", "10yr_2002_sm_sd", "10yr_2002_non_sd",
                "3yr_2003_all_sd", "3yr_2003_sm_sd", "3yr_2003_non_sd", "5yr_2003_all_sd", "5yr_2003_sm_sd", "5yr_2003_non_sd", 
                "10yr_2003_all_sd", "10yr_2003_sm_sd", "10yr_2003_non_sd",
                "3yr_2004_all_sd", "3yr_2004_sm_sd", "3yr_2004_non_sd", "5yr_2004_all_sd", "5yr_2004_sm_sd", "5yr_2004_non_sd", 
                "10yr_2004_all_sd", "10yr_2004_sm_sd", "10yr_2004_non_sd",
                "3yr_2005_all_sd", "3yr_2005_sm_sd", "3yr_2005_non_sd", "5yr_2005_all_sd", "5yr_2005_sm_sd", "5yr_2005_non_sd", 
                "10yr2005_all_sd", "10yr_2005_sm_sd", "10yr_2005_non_sd",
                "3yr_2006_all_sd", "3yr_2006_sm_sd", "3yr_2006_non_sd", "5yr_2006_all_sd", "5yr_2006_sm_sd", "5yr_2006_non_sd", 
                "10yr_2006_all_sd", "10yr_2006_sm_sd", "10yr_2006_non_sd")
colnames(survivalrate_sd) <- srnames_sd

sr_sd <- as.data.frame(survivalrate_sd)

survival.rates_sd <- sr_sd %>% 
  gather("3yr_2001_all_sd":"10yr_2006_non_sd", key = "time_year_type", value = "survivalrate_sd") %>% 
  separate(time_year_type, into = c("time", "year", "type"), sep = "_") %>% 
  arrange(time, year) %>% 
  unite("time_type", time, type, sep = "_") 

survival.rates_sd$time_type[18] <- "10yr_all"
survival.rates_sd$year[18] <- 2005

survival.rates_sd_52 <- survival.rates_sd %>% 
  spread(key = "time_type", value = "survivalrate_sd")

survival.rates_sd_52[, c(1, 5, 7, 6, 8, 10, 9, 2, 4, 3)]

##53####

df2001_sd_53 <- filter(df2001_sd, NAICS2 == 53)
df2002_sd_53 <- filter(df2002_sd, NAICS2 == 53)
df2003_sd_53 <- filter(df2003_sd, NAICS2 == 53)
df2004_sd_53 <- filter(df2004_sd, NAICS2 == 53)
df2005_sd_53 <- filter(df2005_sd, NAICS2 == 53)
df2006_sd_53 <- filter(df2006_sd, NAICS2 == 53)

##3-year
sr3yr2001_sd <- sum(df2001_sd_53$years.survived>=3)/nrow(df2001_sd)
sr3yr2001sm_sd <- sum((df2001_sd_53$years.survived>=3 & df2001_sd_53$biz_size == 0), na.rm = TRUE)/sum(df2001_sd$biz_size == 0, na.rm = TRUE)
sr3yr2001non_sd <- sum((df2001_sd_53$years.survived>=3 & df2001_sd_53$biz_size == 1), na.rm = TRUE)/sum(df2001_sd$biz_size == 1, na.rm = TRUE)

sr3yr2002_sd <- sum(df2002_sd_53$years.survived>=3)/nrow(df2002_sd) 
sr3yr2002sm_sd <- sum((df2002_sd_53$years.survived>=3 & df2002_sd_53$biz_size == 0), na.rm = TRUE)/sum(df2002_sd$biz_size == 0, na.rm = TRUE)
sr3yr2002non_sd <- sum((df2002_sd_53$years.survived>=3 & df2002_sd_53$biz_size == 1), na.rm = TRUE)/sum(df2002_sd$biz_size == 1, na.rm = TRUE)

sr3yr2003_sd <- sum(df2003_sd_53$years.survived>=3)/nrow(df2003_sd)
sr3yr2003sm_sd <- sum((df2003_sd_53$years.survived>=3 & df2003_sd_53$biz_size == 0), na.rm = TRUE)/sum(df2003_sd$biz_size == 0, na.rm = TRUE)
sr3yr2003non_sd <- sum((df2003_sd_53$years.survived>=3 & df2003_sd_53$biz_size == 1), na.rm = TRUE)/sum(df2003_sd$biz_size == 1, na.rm = TRUE)

sr3yr2004_sd <- sum(df2004_sd_53$years.survived>=3)/nrow(df2004_sd) 
sr3yr2004sm_sd <- sum((df2004_sd_53$years.survived>=3 & df2004_sd_53$biz_size == 0), na.rm = TRUE)/sum(df2004_sd$biz_size == 0, na.rm = TRUE)
sr3yr2004non_sd <- sum((df2004_sd_53$years.survived>=3 & df2004_sd_53$biz_size == 1), na.rm = TRUE)/sum(df2004_sd$biz_size == 1, na.rm = TRUE)

sr3yr2005_sd <- sum(df2005_sd_53$years.survived>=3)/nrow(df2005_sd) 
sr3yr2005sm_sd <- sum((df2005_sd_53$years.survived>=3 & df2005_sd_53$biz_size == 0), na.rm = TRUE)/sum(df2005_sd$biz_size == 0, na.rm = TRUE)
sr3yr2005non_sd <- sum((df2005_sd_53$years.survived>=3 & df2005_sd_53$biz_size == 1), na.rm = TRUE)/sum(df2005_sd$biz_size == 1, na.rm = TRUE)

sr3yr2006_sd <- sum(df2006_sd_53$years.survived>=3)/nrow(df2006_sd) 
sr3yr2006sm_sd <- sum((df2006_sd_53$years.survived>=3 & df2006_sd_53$biz_size == 0), na.rm = TRUE)/sum(df2006_sd$biz_size == 0, na.rm = TRUE)
sr3yr2006non_sd <- sum((df2006_sd_53$years.survived>=3 & df2006_sd_53$biz_size == 1), na.rm = TRUE)/sum(df2006_sd$biz_size == 1, na.rm = TRUE)

##5-year
sr5yr2001_sd <- sum(df2001_sd_53$years.survived>=5)/nrow(df2001_sd)
sr5yr2001sm_sd <- sum((df2001_sd_53$years.survived>=5 & df2001_sd_53$biz_size == 0), na.rm = TRUE)/sum(df2001_sd$biz_size == 0, na.rm = TRUE)
sr5yr2001non_sd <- sum((df2001_sd_53$years.survived>=5 & df2001_sd_53$biz_size == 1), na.rm = TRUE)/sum(df2001_sd$biz_size == 1, na.rm = TRUE)

sr5yr2002_sd <- sum(df2002_sd_53$years.survived>=5)/nrow(df2002_sd) 
sr5yr2002sm_sd <- sum((df2002_sd_53$years.survived>=5 & df2002_sd_53$biz_size == 0), na.rm = TRUE)/sum(df2002_sd$biz_size == 0, na.rm = TRUE)
sr5yr2002non_sd <- sum((df2002_sd_53$years.survived>=5 & df2002_sd_53$biz_size == 1), na.rm = TRUE)/sum(df2002_sd$biz_size == 1, na.rm = TRUE)

sr5yr2003_sd <- sum(df2003_sd_53$years.survived>=5)/nrow(df2003_sd)
sr5yr2003sm_sd <- sum((df2003_sd_53$years.survived>=5 & df2003_sd_53$biz_size == 0), na.rm = TRUE)/sum(df2003_sd$biz_size == 0, na.rm = TRUE)
sr5yr2003non_sd <- sum((df2003_sd_53$years.survived>=5 & df2003_sd_53$biz_size == 1), na.rm = TRUE)/sum(df2003_sd$biz_size == 1, na.rm = TRUE)

sr5yr2004_sd <- sum(df2004_sd_53$years.survived>=5)/nrow(df2004_sd) 
sr5yr2004sm_sd <- sum((df2004_sd_53$years.survived>=5 & df2004_sd_53$biz_size == 0), na.rm = TRUE)/sum(df2004_sd$biz_size == 0, na.rm = TRUE)
sr5yr2004non_sd <- sum((df2004_sd_53$years.survived>=5 & df2004_sd_53$biz_size == 1), na.rm = TRUE)/sum(df2004_sd$biz_size == 1, na.rm = TRUE)

sr5yr2005_sd <- sum(df2005_sd_53$years.survived>=5)/nrow(df2005_sd) 
sr5yr2005sm_sd <- sum((df2005_sd_53$years.survived>=5 & df2005_sd_53$biz_size == 0), na.rm = TRUE)/sum(df2005_sd$biz_size == 0, na.rm = TRUE)
sr5yr2005non_sd <- sum((df2005_sd_53$years.survived>=5 & df2005_sd_53$biz_size == 1), na.rm = TRUE)/sum(df2005_sd$biz_size == 1, na.rm = TRUE)

sr5yr2006_sd <- sum(df2006_sd_53$years.survived>=5)/nrow(df2006_sd) 
sr5yr2006sm_sd <- sum((df2006_sd_53$years.survived>=5 & df2006_sd_53$biz_size == 0), na.rm = TRUE)/sum(df2006_sd$biz_size == 0, na.rm = TRUE)
sr5yr2006non_sd <- sum((df2006_sd_53$years.survived>=5 & df2006_sd_53$biz_size == 1), na.rm = TRUE)/sum(df2006_sd$biz_size == 1, na.rm = TRUE)


##10-year
sr10yr2001_sd <- (sum(df2001_sd_53$years.survived>=10))/nrow(df2001_sd) 
sr10yr2001sm_sd <- sum((df2001_sd_53$years.survived>=10 & df2001_sd_53$biz_size == 0), na.rm = TRUE)/sum(df2001_sd$biz_size == 0, na.rm = TRUE)
sr10yr2001non_sd <- sum((df2001_sd_53$years.survived>=10 & df2001_sd_53$biz_size == 1), na.rm = TRUE)/sum(df2002_sd$biz_size == 1, na.rm = TRUE)


sr10yr2002_sd <- sum(df2002_sd_53$years.survived>=10)/nrow(df2002_sd) 
sr10yr2002sm_sd <- sum((df2002_sd_53$years.survived>=10 & df2002_sd_53$biz_size == 0), na.rm = TRUE)/sum(df2002_sd$biz_size == 0, na.rm = TRUE)
sr10yr2002non_sd <- sum((df2002_sd_53$years.survived>=10 & df2002_sd_53$biz_size == 1), na.rm = TRUE)/sum(df2002_sd$biz_size == 1, na.rm = TRUE)

sr10yr2003_sd <- sum(df2003_sd_53$years.survived>=10)/nrow(df2003_sd)
sr10yr2003sm_sd <- sum((df2003_sd_53$years.survived>=10 & df2003_sd_53$biz_size == 0), na.rm = TRUE)/sum(df2003_sd$biz_size == 0, na.rm = TRUE)
sr10yr2003non_sd <- sum((df2003_sd_53$years.survived>=10 & df2003_sd_53$biz_size == 1), na.rm = TRUE)/sum(df2003_sd$biz_size == 1, na.rm = TRUE)

sr10yr2004_sd <- sum(df2004_sd_53$years.survived>=10)/nrow(df2004_sd) 
sr10yr2004sm_sd <- sum((df2004_sd_53$years.survived>=10 & df2004_sd_53$biz_size == 0), na.rm = TRUE)/sum(df2004_sd$biz_size == 0, na.rm = TRUE)
sr10yr2004non_sd <- sum((df2004_sd_53$years.survived>=10 & df2004_sd_53$biz_size == 1), na.rm = TRUE)/sum(df2004_sd$biz_size == 1, na.rm = TRUE)

sr10yr2005_sd <- sum(df2005_sd_53$years.survived>=10)/nrow(df2005_sd) 
sr10yr2005sm_sd <- sum((df2005_sd_53$years.survived>=10 & df2005_sd_53$biz_size == 0), na.rm = TRUE)/sum(df2005_sd$biz_size == 0, na.rm = TRUE)
sr10yr2005non_sd <- sum((df2005_sd_53$years.survived>=10 & df2005_sd_53$biz_size == 1), na.rm = TRUE)/sum(df2005_sd$biz_size == 1, na.rm = TRUE)

sr10yr2006_sd <- sum(df2006_sd_53$years.survived>=10)/nrow(df2006_sd) 
sr10yr2006sm_sd <- sum((df2006_sd_53$years.survived>=10 & df2006_sd_53$biz_size == 0), na.rm = TRUE)/sum(df2006_sd$biz_size == 0, na.rm = TRUE)
sr10yr2006non_sd <- sum((df2006_sd_53$years.survived>=10 & df2006_sd_53$biz_size == 1), na.rm = TRUE)/sum(df2006_sd$biz_size == 1, na.rm = TRUE)

##bound all survival rates together and reformated dataframe

survivalrate_sd <- cbind(sr3yr2001_sd, sr3yr2001sm_sd, sr3yr2001non_sd, sr5yr2001_sd, sr5yr2001sm_sd, sr5yr2001non_sd, 
                         sr10yr2001_sd, sr10yr2001sm_sd, sr10yr2001non_sd,
                         sr3yr2002_sd, sr3yr2002sm_sd, sr3yr2002non_sd, sr5yr2002_sd, sr5yr2002sm_sd, sr5yr2002non_sd, 
                         sr10yr2002_sd, sr10yr2002sm_sd, sr10yr2002non_sd,
                         sr3yr2003_sd, sr3yr2003sm_sd, sr3yr2003non_sd, sr5yr2003_sd, sr5yr2003sm_sd, sr5yr2003non_sd, 
                         sr10yr2003_sd, sr10yr2003sm_sd, sr10yr2003non_sd,
                         sr3yr2004_sd, sr3yr2004sm_sd, sr3yr2004non_sd, sr5yr2004_sd, sr5yr2004sm_sd, sr5yr2004non_sd, 
                         sr10yr2004_sd, sr10yr2004sm_sd, sr10yr2004non_sd,
                         sr3yr2005_sd, sr3yr2005sm_sd, sr3yr2005non_sd, sr5yr2005_sd, sr5yr2005sm_sd, sr5yr2005non_sd, 
                         sr10yr2005_sd, sr10yr2005sm_sd, sr10yr2005non_sd,
                         sr3yr2006_sd, sr3yr2006sm_sd, sr3yr2006non_sd, sr5yr2006_sd, sr5yr2006sm_sd, sr5yr2006non_sd, 
                         sr10yr2006_sd, sr10yr2006sm_sd, sr10yr2006non_sd)
srnames_sd <- c("3yr_2001_all_sd", "3yr_2001_sm_sd", 
                "3yr_2001_non_sd", "5yr_2001_all_sd", "5yr_2001_sm_sd", "5yr_2001_non_sd", "10yr_2001_all_sd", 
                "10yr_2001_sm_sd", "10yr_2001_non_sd",
                "3yr_2002_all_sd", "3yr_2002_sm_sd", "3yr_2002_non_sd", "5yr_2002_all_sd", "5yr_2002_sm_sd", "5yr_2002_non_sd", 
                "10yr_2002_all_sd", "10yr_2002_sm_sd", "10yr_2002_non_sd",
                "3yr_2003_all_sd", "3yr_2003_sm_sd", "3yr_2003_non_sd", "5yr_2003_all_sd", "5yr_2003_sm_sd", "5yr_2003_non_sd", 
                "10yr_2003_all_sd", "10yr_2003_sm_sd", "10yr_2003_non_sd",
                "3yr_2004_all_sd", "3yr_2004_sm_sd", "3yr_2004_non_sd", "5yr_2004_all_sd", "5yr_2004_sm_sd", "5yr_2004_non_sd", 
                "10yr_2004_all_sd", "10yr_2004_sm_sd", "10yr_2004_non_sd",
                "3yr_2005_all_sd", "3yr_2005_sm_sd", "3yr_2005_non_sd", "5yr_2005_all_sd", "5yr_2005_sm_sd", "5yr_2005_non_sd", 
                "10yr2005_all_sd", "10yr_2005_sm_sd", "10yr_2005_non_sd",
                "3yr_2006_all_sd", "3yr_2006_sm_sd", "3yr_2006_non_sd", "5yr_2006_all_sd", "5yr_2006_sm_sd", "5yr_2006_non_sd", 
                "10yr_2006_all_sd", "10yr_2006_sm_sd", "10yr_2006_non_sd")
colnames(survivalrate_sd) <- srnames_sd

sr_sd <- as.data.frame(survivalrate_sd)

survival.rates_sd <- sr_sd %>% 
  gather("3yr_2001_all_sd":"10yr_2006_non_sd", key = "time_year_type", value = "survivalrate_sd") %>% 
  separate(time_year_type, into = c("time", "year", "type"), sep = "_") %>% 
  arrange(time, year) %>% 
  unite("time_type", time, type, sep = "_") 

survival.rates_sd$time_type[18] <- "10yr_all"
survival.rates_sd$year[18] <- 2005

survival.rates_sd_53 <- survival.rates_sd %>% 
  spread(key = "time_type", value = "survivalrate_sd")

survival.rates_sd_53[c(1, 5, 7, 6, 8, 10, 9, 2, 4, 3)]

##54####

df2001_sd_54 <- filter(df2001_sd, NAICS2 == 54)
df2002_sd_54 <- filter(df2002_sd, NAICS2 == 54)
df2003_sd_54 <- filter(df2003_sd, NAICS2 == 54)
df2004_sd_54 <- filter(df2004_sd, NAICS2 == 54)
df2005_sd_54 <- filter(df2005_sd, NAICS2 == 54)
df2006_sd_54 <- filter(df2006_sd, NAICS2 == 54)

##3-year
sr3yr2001_sd <- sum(df2001_sd_54$years.survived>=3)/nrow(df2001_sd)
sr3yr2001sm_sd <- sum((df2001_sd_54$years.survived>=3 & df2001_sd_54$biz_size == 0), na.rm = TRUE)/sum(df2001_sd$biz_size == 0, na.rm = TRUE)
sr3yr2001non_sd <- sum((df2001_sd_54$years.survived>=3 & df2001_sd_54$biz_size == 1), na.rm = TRUE)/sum(df2001_sd$biz_size == 1, na.rm = TRUE)

sr3yr2002_sd <- sum(df2002_sd_54$years.survived>=3)/nrow(df2002_sd) 
sr3yr2002sm_sd <- sum((df2002_sd_54$years.survived>=3 & df2002_sd_54$biz_size == 0), na.rm = TRUE)/sum(df2002_sd$biz_size == 0, na.rm = TRUE)
sr3yr2002non_sd <- sum((df2002_sd_54$years.survived>=3 & df2002_sd_54$biz_size == 1), na.rm = TRUE)/sum(df2002_sd$biz_size == 1, na.rm = TRUE)

sr3yr2003_sd <- sum(df2003_sd_54$years.survived>=3)/nrow(df2003_sd)
sr3yr2003sm_sd <- sum((df2003_sd_54$years.survived>=3 & df2003_sd_54$biz_size == 0), na.rm = TRUE)/sum(df2003_sd$biz_size == 0, na.rm = TRUE)
sr3yr2003non_sd <- sum((df2003_sd_54$years.survived>=3 & df2003_sd_54$biz_size == 1), na.rm = TRUE)/sum(df2003_sd$biz_size == 1, na.rm = TRUE)

sr3yr2004_sd <- sum(df2004_sd_54$years.survived>=3)/nrow(df2004_sd) 
sr3yr2004sm_sd <- sum((df2004_sd_54$years.survived>=3 & df2004_sd_54$biz_size == 0), na.rm = TRUE)/sum(df2004_sd$biz_size == 0, na.rm = TRUE)
sr3yr2004non_sd <- sum((df2004_sd_54$years.survived>=3 & df2004_sd_54$biz_size == 1), na.rm = TRUE)/sum(df2004_sd$biz_size == 1, na.rm = TRUE)

sr3yr2005_sd <- sum(df2005_sd_54$years.survived>=3)/nrow(df2005_sd) 
sr3yr2005sm_sd <- sum((df2005_sd_54$years.survived>=3 & df2005_sd_54$biz_size == 0), na.rm = TRUE)/sum(df2005_sd$biz_size == 0, na.rm = TRUE)
sr3yr2005non_sd <- sum((df2005_sd_54$years.survived>=3 & df2005_sd_54$biz_size == 1), na.rm = TRUE)/sum(df2005_sd$biz_size == 1, na.rm = TRUE)

sr3yr2006_sd <- sum(df2006_sd_54$years.survived>=3)/nrow(df2006_sd) 
sr3yr2006sm_sd <- sum((df2006_sd_54$years.survived>=3 & df2006_sd_54$biz_size == 0), na.rm = TRUE)/sum(df2006_sd$biz_size == 0, na.rm = TRUE)
sr3yr2006non_sd <- sum((df2006_sd_54$years.survived>=3 & df2006_sd_54$biz_size == 1), na.rm = TRUE)/sum(df2006_sd$biz_size == 1, na.rm = TRUE)

##5-year
sr5yr2001_sd <- sum(df2001_sd_54$years.survived>=5)/nrow(df2001_sd)
sr5yr2001sm_sd <- sum((df2001_sd_54$years.survived>=5 & df2001_sd_54$biz_size == 0), na.rm = TRUE)/sum(df2001_sd$biz_size == 0, na.rm = TRUE)
sr5yr2001non_sd <- sum((df2001_sd_54$years.survived>=5 & df2001_sd_54$biz_size == 1), na.rm = TRUE)/sum(df2001_sd$biz_size == 1, na.rm = TRUE)

sr5yr2002_sd <- sum(df2002_sd_54$years.survived>=5)/nrow(df2002_sd) 
sr5yr2002sm_sd <- sum((df2002_sd_54$years.survived>=5 & df2002_sd_54$biz_size == 0), na.rm = TRUE)/sum(df2002_sd$biz_size == 0, na.rm = TRUE)
sr5yr2002non_sd <- sum((df2002_sd_54$years.survived>=5 & df2002_sd_54$biz_size == 1), na.rm = TRUE)/sum(df2002_sd$biz_size == 1, na.rm = TRUE)

sr5yr2003_sd <- sum(df2003_sd_54$years.survived>=5)/nrow(df2003_sd)
sr5yr2003sm_sd <- sum((df2003_sd_54$years.survived>=5 & df2003_sd_54$biz_size == 0), na.rm = TRUE)/sum(df2003_sd$biz_size == 0, na.rm = TRUE)
sr5yr2003non_sd <- sum((df2003_sd_54$years.survived>=5 & df2003_sd_54$biz_size == 1), na.rm = TRUE)/sum(df2003_sd$biz_size == 1, na.rm = TRUE)

sr5yr2004_sd <- sum(df2004_sd_54$years.survived>=5)/nrow(df2004_sd) 
sr5yr2004sm_sd <- sum((df2004_sd_54$years.survived>=5 & df2004_sd_54$biz_size == 0), na.rm = TRUE)/sum(df2004_sd$biz_size == 0, na.rm = TRUE)
sr5yr2004non_sd <- sum((df2004_sd_54$years.survived>=5 & df2004_sd_54$biz_size == 1), na.rm = TRUE)/sum(df2004_sd$biz_size == 1, na.rm = TRUE)

sr5yr2005_sd <- sum(df2005_sd_54$years.survived>=5)/nrow(df2005_sd) 
sr5yr2005sm_sd <- sum((df2005_sd_54$years.survived>=5 & df2005_sd_54$biz_size == 0), na.rm = TRUE)/sum(df2005_sd$biz_size == 0, na.rm = TRUE)
sr5yr2005non_sd <- sum((df2005_sd_54$years.survived>=5 & df2005_sd_54$biz_size == 1), na.rm = TRUE)/sum(df2005_sd$biz_size == 1, na.rm = TRUE)

sr5yr2006_sd <- sum(df2006_sd_54$years.survived>=5)/nrow(df2006_sd) 
sr5yr2006sm_sd <- sum((df2006_sd_54$years.survived>=5 & df2006_sd_54$biz_size == 0), na.rm = TRUE)/sum(df2006_sd$biz_size == 0, na.rm = TRUE)
sr5yr2006non_sd <- sum((df2006_sd_54$years.survived>=5 & df2006_sd_54$biz_size == 1), na.rm = TRUE)/sum(df2006_sd$biz_size == 1, na.rm = TRUE)


##10-year
sr10yr2001_sd <- (sum(df2001_sd_54$years.survived>=10))/nrow(df2001_sd) 
sr10yr2001sm_sd <- sum((df2001_sd_54$years.survived>=10 & df2001_sd_54$biz_size == 0), na.rm = TRUE)/sum(df2001_sd$biz_size == 0, na.rm = TRUE)
sr10yr2001non_sd <- sum((df2001_sd_54$years.survived>=10 & df2001_sd_54$biz_size == 1), na.rm = TRUE)/sum(df2002_sd$biz_size == 1, na.rm = TRUE)


sr10yr2002_sd <- sum(df2002_sd_54$years.survived>=10)/nrow(df2002_sd) 
sr10yr2002sm_sd <- sum((df2002_sd_54$years.survived>=10 & df2002_sd_54$biz_size == 0), na.rm = TRUE)/sum(df2002_sd$biz_size == 0, na.rm = TRUE)
sr10yr2002non_sd <- sum((df2002_sd_54$years.survived>=10 & df2002_sd_54$biz_size == 1), na.rm = TRUE)/sum(df2002_sd$biz_size == 1, na.rm = TRUE)

sr10yr2003_sd <- sum(df2003_sd_54$years.survived>=10)/nrow(df2003_sd)
sr10yr2003sm_sd <- sum((df2003_sd_54$years.survived>=10 & df2003_sd_54$biz_size == 0), na.rm = TRUE)/sum(df2003_sd$biz_size == 0, na.rm = TRUE)
sr10yr2003non_sd <- sum((df2003_sd_54$years.survived>=10 & df2003_sd_54$biz_size == 1), na.rm = TRUE)/sum(df2003_sd$biz_size == 1, na.rm = TRUE)

sr10yr2004_sd <- sum(df2004_sd_54$years.survived>=10)/nrow(df2004_sd) 
sr10yr2004sm_sd <- sum((df2004_sd_54$years.survived>=10 & df2004_sd_54$biz_size == 0), na.rm = TRUE)/sum(df2004_sd$biz_size == 0, na.rm = TRUE)
sr10yr2004non_sd <- sum((df2004_sd_54$years.survived>=10 & df2004_sd_54$biz_size == 1), na.rm = TRUE)/sum(df2004_sd$biz_size == 1, na.rm = TRUE)

sr10yr2005_sd <- sum(df2005_sd_54$years.survived>=10)/nrow(df2005_sd) 
sr10yr2005sm_sd <- sum((df2005_sd_54$years.survived>=10 & df2005_sd_54$biz_size == 0), na.rm = TRUE)/sum(df2005_sd$biz_size == 0, na.rm = TRUE)
sr10yr2005non_sd <- sum((df2005_sd_54$years.survived>=10 & df2005_sd_54$biz_size == 1), na.rm = TRUE)/sum(df2005_sd$biz_size == 1, na.rm = TRUE)

sr10yr2006_sd <- sum(df2006_sd_54$years.survived>=10)/nrow(df2006_sd) 
sr10yr2006sm_sd <- sum((df2006_sd_54$years.survived>=10 & df2006_sd_54$biz_size == 0), na.rm = TRUE)/sum(df2006_sd$biz_size == 0, na.rm = TRUE)
sr10yr2006non_sd <- sum((df2006_sd_54$years.survived>=10 & df2006_sd_54$biz_size == 1), na.rm = TRUE)/sum(df2006_sd$biz_size == 1, na.rm = TRUE)

##bound all survival rates together and reformated dataframe

survivalrate_sd <- cbind(sr3yr2001_sd, sr3yr2001sm_sd, sr3yr2001non_sd, sr5yr2001_sd, sr5yr2001sm_sd, sr5yr2001non_sd, 
                         sr10yr2001_sd, sr10yr2001sm_sd, sr10yr2001non_sd,
                         sr3yr2002_sd, sr3yr2002sm_sd, sr3yr2002non_sd, sr5yr2002_sd, sr5yr2002sm_sd, sr5yr2002non_sd, 
                         sr10yr2002_sd, sr10yr2002sm_sd, sr10yr2002non_sd,
                         sr3yr2003_sd, sr3yr2003sm_sd, sr3yr2003non_sd, sr5yr2003_sd, sr5yr2003sm_sd, sr5yr2003non_sd, 
                         sr10yr2003_sd, sr10yr2003sm_sd, sr10yr2003non_sd,
                         sr3yr2004_sd, sr3yr2004sm_sd, sr3yr2004non_sd, sr5yr2004_sd, sr5yr2004sm_sd, sr5yr2004non_sd, 
                         sr10yr2004_sd, sr10yr2004sm_sd, sr10yr2004non_sd,
                         sr3yr2005_sd, sr3yr2005sm_sd, sr3yr2005non_sd, sr5yr2005_sd, sr5yr2005sm_sd, sr5yr2005non_sd, 
                         sr10yr2005_sd, sr10yr2005sm_sd, sr10yr2005non_sd,
                         sr3yr2006_sd, sr3yr2006sm_sd, sr3yr2006non_sd, sr5yr2006_sd, sr5yr2006sm_sd, sr5yr2006non_sd, 
                         sr10yr2006_sd, sr10yr2006sm_sd, sr10yr2006non_sd)
srnames_sd <- c("3yr_2001_all_sd", "3yr_2001_sm_sd", 
                "3yr_2001_non_sd", "5yr_2001_all_sd", "5yr_2001_sm_sd", "5yr_2001_non_sd", "10yr_2001_all_sd", 
                "10yr_2001_sm_sd", "10yr_2001_non_sd",
                "3yr_2002_all_sd", "3yr_2002_sm_sd", "3yr_2002_non_sd", "5yr_2002_all_sd", "5yr_2002_sm_sd", "5yr_2002_non_sd", 
                "10yr_2002_all_sd", "10yr_2002_sm_sd", "10yr_2002_non_sd",
                "3yr_2003_all_sd", "3yr_2003_sm_sd", "3yr_2003_non_sd", "5yr_2003_all_sd", "5yr_2003_sm_sd", "5yr_2003_non_sd", 
                "10yr_2003_all_sd", "10yr_2003_sm_sd", "10yr_2003_non_sd",
                "3yr_2004_all_sd", "3yr_2004_sm_sd", "3yr_2004_non_sd", "5yr_2004_all_sd", "5yr_2004_sm_sd", "5yr_2004_non_sd", 
                "10yr_2004_all_sd", "10yr_2004_sm_sd", "10yr_2004_non_sd",
                "3yr_2005_all_sd", "3yr_2005_sm_sd", "3yr_2005_non_sd", "5yr_2005_all_sd", "5yr_2005_sm_sd", "5yr_2005_non_sd", 
                "10yr2005_all_sd", "10yr_2005_sm_sd", "10yr_2005_non_sd",
                "3yr_2006_all_sd", "3yr_2006_sm_sd", "3yr_2006_non_sd", "5yr_2006_all_sd", "5yr_2006_sm_sd", "5yr_2006_non_sd", 
                "10yr_2006_all_sd", "10yr_2006_sm_sd", "10yr_2006_non_sd")
colnames(survivalrate_sd) <- srnames_sd

sr_sd <- as.data.frame(survivalrate_sd)

survival.rates_sd <- sr_sd %>% 
  gather("3yr_2001_all_sd":"10yr_2006_non_sd", key = "time_year_type", value = "survivalrate_sd") %>% 
  separate(time_year_type, into = c("time", "year", "type"), sep = "_") %>% 
  arrange(time, year) %>% 
  unite("time_type", time, type, sep = "_") 

survival.rates_sd$time_type[18] <- "10yr_all"
survival.rates_sd$year[18] <- 2005

survival.rates_sd_54 <- survival.rates_sd %>% 
  spread(key = "time_type", value = "survivalrate_sd")

survival.rates_sd_54[, c(1, 5, 7, 6, 8, 10, 9, 2, 4, 3)]

##56####

df2001_sd_56 <- filter(df2001_sd, NAICS2 == 56)
df2002_sd_56 <- filter(df2002_sd, NAICS2 == 56)
df2003_sd_56 <- filter(df2003_sd, NAICS2 == 56)
df2004_sd_56 <- filter(df2004_sd, NAICS2 == 56)
df2005_sd_56 <- filter(df2005_sd, NAICS2 == 56)
df2006_sd_56 <- filter(df2006_sd, NAICS2 == 56)

##3-year
sr3yr2001_sd <- sum(df2001_sd_56$years.survived>=3)/nrow(df2001_sd)
sr3yr2001sm_sd <- sum((df2001_sd_56$years.survived>=3 & df2001_sd_56$biz_size == 0), na.rm = TRUE)/sum(df2001_sd$biz_size == 0, na.rm = TRUE)
sr3yr2001non_sd <- sum((df2001_sd_56$years.survived>=3 & df2001_sd_56$biz_size == 1), na.rm = TRUE)/sum(df2001_sd$biz_size == 1, na.rm = TRUE)

sr3yr2002_sd <- sum(df2002_sd_56$years.survived>=3)/nrow(df2002_sd) 
sr3yr2002sm_sd <- sum((df2002_sd_56$years.survived>=3 & df2002_sd_56$biz_size == 0), na.rm = TRUE)/sum(df2002_sd$biz_size == 0, na.rm = TRUE)
sr3yr2002non_sd <- sum((df2002_sd_56$years.survived>=3 & df2002_sd_56$biz_size == 1), na.rm = TRUE)/sum(df2002_sd$biz_size == 1, na.rm = TRUE)

sr3yr2003_sd <- sum(df2003_sd_56$years.survived>=3)/nrow(df2003_sd)
sr3yr2003sm_sd <- sum((df2003_sd_56$years.survived>=3 & df2003_sd_56$biz_size == 0), na.rm = TRUE)/sum(df2003_sd$biz_size == 0, na.rm = TRUE)
sr3yr2003non_sd <- sum((df2003_sd_56$years.survived>=3 & df2003_sd_56$biz_size == 1), na.rm = TRUE)/sum(df2003_sd$biz_size == 1, na.rm = TRUE)

sr3yr2004_sd <- sum(df2004_sd_56$years.survived>=3)/nrow(df2004_sd) 
sr3yr2004sm_sd <- sum((df2004_sd_56$years.survived>=3 & df2004_sd_56$biz_size == 0), na.rm = TRUE)/sum(df2004_sd$biz_size == 0, na.rm = TRUE)
sr3yr2004non_sd <- sum((df2004_sd_56$years.survived>=3 & df2004_sd_56$biz_size == 1), na.rm = TRUE)/sum(df2004_sd$biz_size == 1, na.rm = TRUE)

sr3yr2005_sd <- sum(df2005_sd_56$years.survived>=3)/nrow(df2005_sd) 
sr3yr2005sm_sd <- sum((df2005_sd_56$years.survived>=3 & df2005_sd_56$biz_size == 0), na.rm = TRUE)/sum(df2005_sd$biz_size == 0, na.rm = TRUE)
sr3yr2005non_sd <- sum((df2005_sd_56$years.survived>=3 & df2005_sd_56$biz_size == 1), na.rm = TRUE)/sum(df2005_sd$biz_size == 1, na.rm = TRUE)

sr3yr2006_sd <- sum(df2006_sd_56$years.survived>=3)/nrow(df2006_sd) 
sr3yr2006sm_sd <- sum((df2006_sd_56$years.survived>=3 & df2006_sd_56$biz_size == 0), na.rm = TRUE)/sum(df2006_sd$biz_size == 0, na.rm = TRUE)
sr3yr2006non_sd <- sum((df2006_sd_56$years.survived>=3 & df2006_sd_56$biz_size == 1), na.rm = TRUE)/sum(df2006_sd$biz_size == 1, na.rm = TRUE)

##5-year
sr5yr2001_sd <- sum(df2001_sd_56$years.survived>=5)/nrow(df2001_sd)
sr5yr2001sm_sd <- sum((df2001_sd_56$years.survived>=5 & df2001_sd_56$biz_size == 0), na.rm = TRUE)/sum(df2001_sd$biz_size == 0, na.rm = TRUE)
sr5yr2001non_sd <- sum((df2001_sd_56$years.survived>=5 & df2001_sd_56$biz_size == 1), na.rm = TRUE)/sum(df2001_sd$biz_size == 1, na.rm = TRUE)

sr5yr2002_sd <- sum(df2002_sd_56$years.survived>=5)/nrow(df2002_sd) 
sr5yr2002sm_sd <- sum((df2002_sd_56$years.survived>=5 & df2002_sd_56$biz_size == 0), na.rm = TRUE)/sum(df2002_sd$biz_size == 0, na.rm = TRUE)
sr5yr2002non_sd <- sum((df2002_sd_56$years.survived>=5 & df2002_sd_56$biz_size == 1), na.rm = TRUE)/sum(df2002_sd$biz_size == 1, na.rm = TRUE)

sr5yr2003_sd <- sum(df2003_sd_56$years.survived>=5)/nrow(df2003_sd)
sr5yr2003sm_sd <- sum((df2003_sd_56$years.survived>=5 & df2003_sd_56$biz_size == 0), na.rm = TRUE)/sum(df2003_sd$biz_size == 0, na.rm = TRUE)
sr5yr2003non_sd <- sum((df2003_sd_56$years.survived>=5 & df2003_sd_56$biz_size == 1), na.rm = TRUE)/sum(df2003_sd$biz_size == 1, na.rm = TRUE)

sr5yr2004_sd <- sum(df2004_sd_56$years.survived>=5)/nrow(df2004_sd) 
sr5yr2004sm_sd <- sum((df2004_sd_56$years.survived>=5 & df2004_sd_56$biz_size == 0), na.rm = TRUE)/sum(df2004_sd$biz_size == 0, na.rm = TRUE)
sr5yr2004non_sd <- sum((df2004_sd_56$years.survived>=5 & df2004_sd_56$biz_size == 1), na.rm = TRUE)/sum(df2004_sd$biz_size == 1, na.rm = TRUE)

sr5yr2005_sd <- sum(df2005_sd_56$years.survived>=5)/nrow(df2005_sd) 
sr5yr2005sm_sd <- sum((df2005_sd_56$years.survived>=5 & df2005_sd_56$biz_size == 0), na.rm = TRUE)/sum(df2005_sd$biz_size == 0, na.rm = TRUE)
sr5yr2005non_sd <- sum((df2005_sd_56$years.survived>=5 & df2005_sd_56$biz_size == 1), na.rm = TRUE)/sum(df2005_sd$biz_size == 1, na.rm = TRUE)

sr5yr2006_sd <- sum(df2006_sd_56$years.survived>=5)/nrow(df2006_sd) 
sr5yr2006sm_sd <- sum((df2006_sd_56$years.survived>=5 & df2006_sd_56$biz_size == 0), na.rm = TRUE)/sum(df2006_sd$biz_size == 0, na.rm = TRUE)
sr5yr2006non_sd <- sum((df2006_sd_56$years.survived>=5 & df2006_sd_56$biz_size == 1), na.rm = TRUE)/sum(df2006_sd$biz_size == 1, na.rm = TRUE)


##10-year
sr10yr2001_sd <- (sum(df2001_sd_56$years.survived>=10))/nrow(df2001_sd) 
sr10yr2001sm_sd <- sum((df2001_sd_56$years.survived>=10 & df2001_sd_56$biz_size == 0), na.rm = TRUE)/sum(df2001_sd$biz_size == 0, na.rm = TRUE)
sr10yr2001non_sd <- sum((df2001_sd_56$years.survived>=10 & df2001_sd_56$biz_size == 1), na.rm = TRUE)/sum(df2002_sd$biz_size == 1, na.rm = TRUE)


sr10yr2002_sd <- sum(df2002_sd_56$years.survived>=10)/nrow(df2002_sd) 
sr10yr2002sm_sd <- sum((df2002_sd_56$years.survived>=10 & df2002_sd_56$biz_size == 0), na.rm = TRUE)/sum(df2002_sd$biz_size == 0, na.rm = TRUE)
sr10yr2002non_sd <- sum((df2002_sd_56$years.survived>=10 & df2002_sd_56$biz_size == 1), na.rm = TRUE)/sum(df2002_sd$biz_size == 1, na.rm = TRUE)

sr10yr2003_sd <- sum(df2003_sd_56$years.survived>=10)/nrow(df2003_sd)
sr10yr2003sm_sd <- sum((df2003_sd_56$years.survived>=10 & df2003_sd_56$biz_size == 0), na.rm = TRUE)/sum(df2003_sd$biz_size == 0, na.rm = TRUE)
sr10yr2003non_sd <- sum((df2003_sd_56$years.survived>=10 & df2003_sd_56$biz_size == 1), na.rm = TRUE)/sum(df2003_sd$biz_size == 1, na.rm = TRUE)

sr10yr2004_sd <- sum(df2004_sd_56$years.survived>=10)/nrow(df2004_sd) 
sr10yr2004sm_sd <- sum((df2004_sd_56$years.survived>=10 & df2004_sd_56$biz_size == 0), na.rm = TRUE)/sum(df2004_sd$biz_size == 0, na.rm = TRUE)
sr10yr2004non_sd <- sum((df2004_sd_56$years.survived>=10 & df2004_sd_56$biz_size == 1), na.rm = TRUE)/sum(df2004_sd$biz_size == 1, na.rm = TRUE)

sr10yr2005_sd <- sum(df2005_sd_56$years.survived>=10)/nrow(df2005_sd) 
sr10yr2005sm_sd <- sum((df2005_sd_56$years.survived>=10 & df2005_sd_56$biz_size == 0), na.rm = TRUE)/sum(df2005_sd$biz_size == 0, na.rm = TRUE)
sr10yr2005non_sd <- sum((df2005_sd_56$years.survived>=10 & df2005_sd_56$biz_size == 1), na.rm = TRUE)/sum(df2005_sd$biz_size == 1, na.rm = TRUE)

sr10yr2006_sd <- sum(df2006_sd_56$years.survived>=10)/nrow(df2006_sd) 
sr10yr2006sm_sd <- sum((df2006_sd_56$years.survived>=10 & df2006_sd_56$biz_size == 0), na.rm = TRUE)/sum(df2006_sd$biz_size == 0, na.rm = TRUE)
sr10yr2006non_sd <- sum((df2006_sd_56$years.survived>=10 & df2006_sd_56$biz_size == 1), na.rm = TRUE)/sum(df2006_sd$biz_size == 1, na.rm = TRUE)

##bound all survival rates together and reformated dataframe

survivalrate_sd <- cbind(sr3yr2001_sd, sr3yr2001sm_sd, sr3yr2001non_sd, sr5yr2001_sd, sr5yr2001sm_sd, sr5yr2001non_sd, 
                         sr10yr2001_sd, sr10yr2001sm_sd, sr10yr2001non_sd,
                         sr3yr2002_sd, sr3yr2002sm_sd, sr3yr2002non_sd, sr5yr2002_sd, sr5yr2002sm_sd, sr5yr2002non_sd, 
                         sr10yr2002_sd, sr10yr2002sm_sd, sr10yr2002non_sd,
                         sr3yr2003_sd, sr3yr2003sm_sd, sr3yr2003non_sd, sr5yr2003_sd, sr5yr2003sm_sd, sr5yr2003non_sd, 
                         sr10yr2003_sd, sr10yr2003sm_sd, sr10yr2003non_sd,
                         sr3yr2004_sd, sr3yr2004sm_sd, sr3yr2004non_sd, sr5yr2004_sd, sr5yr2004sm_sd, sr5yr2004non_sd, 
                         sr10yr2004_sd, sr10yr2004sm_sd, sr10yr2004non_sd,
                         sr3yr2005_sd, sr3yr2005sm_sd, sr3yr2005non_sd, sr5yr2005_sd, sr5yr2005sm_sd, sr5yr2005non_sd, 
                         sr10yr2005_sd, sr10yr2005sm_sd, sr10yr2005non_sd,
                         sr3yr2006_sd, sr3yr2006sm_sd, sr3yr2006non_sd, sr5yr2006_sd, sr5yr2006sm_sd, sr5yr2006non_sd, 
                         sr10yr2006_sd, sr10yr2006sm_sd, sr10yr2006non_sd)
srnames_sd <- c("3yr_2001_all_sd", "3yr_2001_sm_sd", 
                "3yr_2001_non_sd", "5yr_2001_all_sd", "5yr_2001_sm_sd", "5yr_2001_non_sd", "10yr_2001_all_sd", 
                "10yr_2001_sm_sd", "10yr_2001_non_sd",
                "3yr_2002_all_sd", "3yr_2002_sm_sd", "3yr_2002_non_sd", "5yr_2002_all_sd", "5yr_2002_sm_sd", "5yr_2002_non_sd", 
                "10yr_2002_all_sd", "10yr_2002_sm_sd", "10yr_2002_non_sd",
                "3yr_2003_all_sd", "3yr_2003_sm_sd", "3yr_2003_non_sd", "5yr_2003_all_sd", "5yr_2003_sm_sd", "5yr_2003_non_sd", 
                "10yr_2003_all_sd", "10yr_2003_sm_sd", "10yr_2003_non_sd",
                "3yr_2004_all_sd", "3yr_2004_sm_sd", "3yr_2004_non_sd", "5yr_2004_all_sd", "5yr_2004_sm_sd", "5yr_2004_non_sd", 
                "10yr_2004_all_sd", "10yr_2004_sm_sd", "10yr_2004_non_sd",
                "3yr_2005_all_sd", "3yr_2005_sm_sd", "3yr_2005_non_sd", "5yr_2005_all_sd", "5yr_2005_sm_sd", "5yr_2005_non_sd", 
                "10yr2005_all_sd", "10yr_2005_sm_sd", "10yr_2005_non_sd",
                "3yr_2006_all_sd", "3yr_2006_sm_sd", "3yr_2006_non_sd", "5yr_2006_all_sd", "5yr_2006_sm_sd", "5yr_2006_non_sd", 
                "10yr_2006_all_sd", "10yr_2006_sm_sd", "10yr_2006_non_sd")
colnames(survivalrate_sd) <- srnames_sd

sr_sd <- as.data.frame(survivalrate_sd)

survival.rates_sd <- sr_sd %>% 
  gather("3yr_2001_all_sd":"10yr_2006_non_sd", key = "time_year_type", value = "survivalrate_sd") %>% 
  separate(time_year_type, into = c("time", "year", "type"), sep = "_") %>% 
  arrange(time, year) %>% 
  unite("time_type", time, type, sep = "_") 

survival.rates_sd$time_type[18] <- "10yr_all"
survival.rates_sd$year[18] <- 2005

survival.rates_sd_56 <- survival.rates_sd %>% 
  spread(key = "time_type", value = "survivalrate_sd")

survival.rates_sd_56[, c(1, 5, 7, 6, 8, 10, 9, 2, 4, 3)]

##61####

df2001_sd_61 <- filter(df2001_sd, NAICS2 == 61)
df2002_sd_61 <- filter(df2002_sd, NAICS2 == 61)
df2003_sd_61 <- filter(df2003_sd, NAICS2 == 61)
df2004_sd_61 <- filter(df2004_sd, NAICS2 == 61)
df2005_sd_61 <- filter(df2005_sd, NAICS2 == 61)
df2006_sd_61 <- filter(df2006_sd, NAICS2 == 61)

##3-year
sr3yr2001_sd <- sum(df2001_sd_61$years.survived>=3)/nrow(df2001_sd)
sr3yr2001sm_sd <- sum((df2001_sd_61$years.survived>=3 & df2001_sd_61$biz_size == 0), na.rm = TRUE)/sum(df2001_sd$biz_size == 0, na.rm = TRUE)
sr3yr2001non_sd <- sum((df2001_sd_61$years.survived>=3 & df2001_sd_61$biz_size == 1), na.rm = TRUE)/sum(df2001_sd$biz_size == 1, na.rm = TRUE)

sr3yr2002_sd <- sum(df2002_sd_61$years.survived>=3)/nrow(df2002_sd) 
sr3yr2002sm_sd <- sum((df2002_sd_61$years.survived>=3 & df2002_sd_61$biz_size == 0), na.rm = TRUE)/sum(df2002_sd$biz_size == 0, na.rm = TRUE)
sr3yr2002non_sd <- sum((df2002_sd_61$years.survived>=3 & df2002_sd_61$biz_size == 1), na.rm = TRUE)/sum(df2002_sd$biz_size == 1, na.rm = TRUE)

sr3yr2003_sd <- sum(df2003_sd_61$years.survived>=3)/nrow(df2003_sd)
sr3yr2003sm_sd <- sum((df2003_sd_61$years.survived>=3 & df2003_sd_61$biz_size == 0), na.rm = TRUE)/sum(df2003_sd$biz_size == 0, na.rm = TRUE)
sr3yr2003non_sd <- sum((df2003_sd_61$years.survived>=3 & df2003_sd_61$biz_size == 1), na.rm = TRUE)/sum(df2003_sd$biz_size == 1, na.rm = TRUE)

sr3yr2004_sd <- sum(df2004_sd_61$years.survived>=3)/nrow(df2004_sd) 
sr3yr2004sm_sd <- sum((df2004_sd_61$years.survived>=3 & df2004_sd_61$biz_size == 0), na.rm = TRUE)/sum(df2004_sd$biz_size == 0, na.rm = TRUE)
sr3yr2004non_sd <- sum((df2004_sd_61$years.survived>=3 & df2004_sd_61$biz_size == 1), na.rm = TRUE)/sum(df2004_sd$biz_size == 1, na.rm = TRUE)

sr3yr2005_sd <- sum(df2005_sd_61$years.survived>=3)/nrow(df2005_sd) 
sr3yr2005sm_sd <- sum((df2005_sd_61$years.survived>=3 & df2005_sd_61$biz_size == 0), na.rm = TRUE)/sum(df2005_sd$biz_size == 0, na.rm = TRUE)
sr3yr2005non_sd <- sum((df2005_sd_61$years.survived>=3 & df2005_sd_61$biz_size == 1), na.rm = TRUE)/sum(df2005_sd$biz_size == 1, na.rm = TRUE)

sr3yr2006_sd <- sum(df2006_sd_61$years.survived>=3)/nrow(df2006_sd) 
sr3yr2006sm_sd <- sum((df2006_sd_61$years.survived>=3 & df2006_sd_61$biz_size == 0), na.rm = TRUE)/sum(df2006_sd$biz_size == 0, na.rm = TRUE)
sr3yr2006non_sd <- sum((df2006_sd_61$years.survived>=3 & df2006_sd_61$biz_size == 1), na.rm = TRUE)/sum(df2006_sd$biz_size == 1, na.rm = TRUE)

##5-year
sr5yr2001_sd <- sum(df2001_sd_61$years.survived>=5)/nrow(df2001_sd)
sr5yr2001sm_sd <- sum((df2001_sd_61$years.survived>=5 & df2001_sd_61$biz_size == 0), na.rm = TRUE)/sum(df2001_sd$biz_size == 0, na.rm = TRUE)
sr5yr2001non_sd <- sum((df2001_sd_61$years.survived>=5 & df2001_sd_61$biz_size == 1), na.rm = TRUE)/sum(df2001_sd$biz_size == 1, na.rm = TRUE)

sr5yr2002_sd <- sum(df2002_sd_61$years.survived>=5)/nrow(df2002_sd) 
sr5yr2002sm_sd <- sum((df2002_sd_61$years.survived>=5 & df2002_sd_61$biz_size == 0), na.rm = TRUE)/sum(df2002_sd$biz_size == 0, na.rm = TRUE)
sr5yr2002non_sd <- sum((df2002_sd_61$years.survived>=5 & df2002_sd_61$biz_size == 1), na.rm = TRUE)/sum(df2002_sd$biz_size == 1, na.rm = TRUE)

sr5yr2003_sd <- sum(df2003_sd_61$years.survived>=5)/nrow(df2003_sd)
sr5yr2003sm_sd <- sum((df2003_sd_61$years.survived>=5 & df2003_sd_61$biz_size == 0), na.rm = TRUE)/sum(df2003_sd$biz_size == 0, na.rm = TRUE)
sr5yr2003non_sd <- sum((df2003_sd_61$years.survived>=5 & df2003_sd_61$biz_size == 1), na.rm = TRUE)/sum(df2003_sd$biz_size == 1, na.rm = TRUE)

sr5yr2004_sd <- sum(df2004_sd_61$years.survived>=5)/nrow(df2004_sd) 
sr5yr2004sm_sd <- sum((df2004_sd_61$years.survived>=5 & df2004_sd_61$biz_size == 0), na.rm = TRUE)/sum(df2004_sd$biz_size == 0, na.rm = TRUE)
sr5yr2004non_sd <- sum((df2004_sd_61$years.survived>=5 & df2004_sd_61$biz_size == 1), na.rm = TRUE)/sum(df2004_sd$biz_size == 1, na.rm = TRUE)

sr5yr2005_sd <- sum(df2005_sd_61$years.survived>=5)/nrow(df2005_sd) 
sr5yr2005sm_sd <- sum((df2005_sd_61$years.survived>=5 & df2005_sd_61$biz_size == 0), na.rm = TRUE)/sum(df2005_sd$biz_size == 0, na.rm = TRUE)
sr5yr2005non_sd <- sum((df2005_sd_61$years.survived>=5 & df2005_sd_61$biz_size == 1), na.rm = TRUE)/sum(df2005_sd$biz_size == 1, na.rm = TRUE)

sr5yr2006_sd <- sum(df2006_sd_61$years.survived>=5)/nrow(df2006_sd) 
sr5yr2006sm_sd <- sum((df2006_sd_61$years.survived>=5 & df2006_sd_61$biz_size == 0), na.rm = TRUE)/sum(df2006_sd$biz_size == 0, na.rm = TRUE)
sr5yr2006non_sd <- sum((df2006_sd_61$years.survived>=5 & df2006_sd_61$biz_size == 1), na.rm = TRUE)/sum(df2006_sd$biz_size == 1, na.rm = TRUE)


##10-year
sr10yr2001_sd <- (sum(df2001_sd_61$years.survived>=10))/nrow(df2001_sd) 
sr10yr2001sm_sd <- sum((df2001_sd_61$years.survived>=10 & df2001_sd_61$biz_size == 0), na.rm = TRUE)/sum(df2001_sd$biz_size == 0, na.rm = TRUE)
sr10yr2001non_sd <- sum((df2001_sd_61$years.survived>=10 & df2001_sd_61$biz_size == 1), na.rm = TRUE)/sum(df2002_sd$biz_size == 1, na.rm = TRUE)


sr10yr2002_sd <- sum(df2002_sd_61$years.survived>=10)/nrow(df2002_sd) 
sr10yr2002sm_sd <- sum((df2002_sd_61$years.survived>=10 & df2002_sd_61$biz_size == 0), na.rm = TRUE)/sum(df2002_sd$biz_size == 0, na.rm = TRUE)
sr10yr2002non_sd <- sum((df2002_sd_61$years.survived>=10 & df2002_sd_61$biz_size == 1), na.rm = TRUE)/sum(df2002_sd$biz_size == 1, na.rm = TRUE)

sr10yr2003_sd <- sum(df2003_sd_61$years.survived>=10)/nrow(df2003_sd)
sr10yr2003sm_sd <- sum((df2003_sd_61$years.survived>=10 & df2003_sd_61$biz_size == 0), na.rm = TRUE)/sum(df2003_sd$biz_size == 0, na.rm = TRUE)
sr10yr2003non_sd <- sum((df2003_sd_61$years.survived>=10 & df2003_sd_61$biz_size == 1), na.rm = TRUE)/sum(df2003_sd$biz_size == 1, na.rm = TRUE)

sr10yr2004_sd <- sum(df2004_sd_61$years.survived>=10)/nrow(df2004_sd) 
sr10yr2004sm_sd <- sum((df2004_sd_61$years.survived>=10 & df2004_sd_61$biz_size == 0), na.rm = TRUE)/sum(df2004_sd$biz_size == 0, na.rm = TRUE)
sr10yr2004non_sd <- sum((df2004_sd_61$years.survived>=10 & df2004_sd_61$biz_size == 1), na.rm = TRUE)/sum(df2004_sd$biz_size == 1, na.rm = TRUE)

sr10yr2005_sd <- sum(df2005_sd_61$years.survived>=10)/nrow(df2005_sd) 
sr10yr2005sm_sd <- sum((df2005_sd_61$years.survived>=10 & df2005_sd_61$biz_size == 0), na.rm = TRUE)/sum(df2005_sd$biz_size == 0, na.rm = TRUE)
sr10yr2005non_sd <- sum((df2005_sd_61$years.survived>=10 & df2005_sd_61$biz_size == 1), na.rm = TRUE)/sum(df2005_sd$biz_size == 1, na.rm = TRUE)

sr10yr2006_sd <- sum(df2006_sd_61$years.survived>=10)/nrow(df2006_sd) 
sr10yr2006sm_sd <- sum((df2006_sd_61$years.survived>=10 & df2006_sd_61$biz_size == 0), na.rm = TRUE)/sum(df2006_sd$biz_size == 0, na.rm = TRUE)
sr10yr2006non_sd <- sum((df2006_sd_61$years.survived>=10 & df2006_sd_61$biz_size == 1), na.rm = TRUE)/sum(df2006_sd$biz_size == 1, na.rm = TRUE)

##bound all survival rates together and reformated dataframe

survivalrate_sd <- cbind(sr3yr2001_sd, sr3yr2001sm_sd, sr3yr2001non_sd, sr5yr2001_sd, sr5yr2001sm_sd, sr5yr2001non_sd, 
                         sr10yr2001_sd, sr10yr2001sm_sd, sr10yr2001non_sd,
                         sr3yr2002_sd, sr3yr2002sm_sd, sr3yr2002non_sd, sr5yr2002_sd, sr5yr2002sm_sd, sr5yr2002non_sd, 
                         sr10yr2002_sd, sr10yr2002sm_sd, sr10yr2002non_sd,
                         sr3yr2003_sd, sr3yr2003sm_sd, sr3yr2003non_sd, sr5yr2003_sd, sr5yr2003sm_sd, sr5yr2003non_sd, 
                         sr10yr2003_sd, sr10yr2003sm_sd, sr10yr2003non_sd,
                         sr3yr2004_sd, sr3yr2004sm_sd, sr3yr2004non_sd, sr5yr2004_sd, sr5yr2004sm_sd, sr5yr2004non_sd, 
                         sr10yr2004_sd, sr10yr2004sm_sd, sr10yr2004non_sd,
                         sr3yr2005_sd, sr3yr2005sm_sd, sr3yr2005non_sd, sr5yr2005_sd, sr5yr2005sm_sd, sr5yr2005non_sd, 
                         sr10yr2005_sd, sr10yr2005sm_sd, sr10yr2005non_sd,
                         sr3yr2006_sd, sr3yr2006sm_sd, sr3yr2006non_sd, sr5yr2006_sd, sr5yr2006sm_sd, sr5yr2006non_sd, 
                         sr10yr2006_sd, sr10yr2006sm_sd, sr10yr2006non_sd)
srnames_sd <- c("3yr_2001_all_sd", "3yr_2001_sm_sd", 
                "3yr_2001_non_sd", "5yr_2001_all_sd", "5yr_2001_sm_sd", "5yr_2001_non_sd", "10yr_2001_all_sd", 
                "10yr_2001_sm_sd", "10yr_2001_non_sd",
                "3yr_2002_all_sd", "3yr_2002_sm_sd", "3yr_2002_non_sd", "5yr_2002_all_sd", "5yr_2002_sm_sd", "5yr_2002_non_sd", 
                "10yr_2002_all_sd", "10yr_2002_sm_sd", "10yr_2002_non_sd",
                "3yr_2003_all_sd", "3yr_2003_sm_sd", "3yr_2003_non_sd", "5yr_2003_all_sd", "5yr_2003_sm_sd", "5yr_2003_non_sd", 
                "10yr_2003_all_sd", "10yr_2003_sm_sd", "10yr_2003_non_sd",
                "3yr_2004_all_sd", "3yr_2004_sm_sd", "3yr_2004_non_sd", "5yr_2004_all_sd", "5yr_2004_sm_sd", "5yr_2004_non_sd", 
                "10yr_2004_all_sd", "10yr_2004_sm_sd", "10yr_2004_non_sd",
                "3yr_2005_all_sd", "3yr_2005_sm_sd", "3yr_2005_non_sd", "5yr_2005_all_sd", "5yr_2005_sm_sd", "5yr_2005_non_sd", 
                "10yr2005_all_sd", "10yr_2005_sm_sd", "10yr_2005_non_sd",
                "3yr_2006_all_sd", "3yr_2006_sm_sd", "3yr_2006_non_sd", "5yr_2006_all_sd", "5yr_2006_sm_sd", "5yr_2006_non_sd", 
                "10yr_2006_all_sd", "10yr_2006_sm_sd", "10yr_2006_non_sd")
colnames(survivalrate_sd) <- srnames_sd

sr_sd <- as.data.frame(survivalrate_sd)

survival.rates_sd <- sr_sd %>% 
  gather("3yr_2001_all_sd":"10yr_2006_non_sd", key = "time_year_type", value = "survivalrate_sd") %>% 
  separate(time_year_type, into = c("time", "year", "type"), sep = "_") %>% 
  arrange(time, year) %>% 
  unite("time_type", time, type, sep = "_") 

survival.rates_sd$time_type[18] <- "10yr_all"
survival.rates_sd$year[18] <- 2005

survival.rates_sd_61 <- survival.rates_sd %>% 
  spread(key = "time_type", value = "survivalrate_sd")

survival.rates_sd_61[, c(1, 5, 7, 6, 8, 10, 9, 2, 4, 3)]

##62####

df2001_sd_62 <- filter(df2001_sd, NAICS2 == 62)
df2002_sd_62 <- filter(df2002_sd, NAICS2 == 62)
df2003_sd_62 <- filter(df2003_sd, NAICS2 == 62)
df2004_sd_62 <- filter(df2004_sd, NAICS2 == 62)
df2005_sd_62 <- filter(df2005_sd, NAICS2 == 62)
df2006_sd_62 <- filter(df2006_sd, NAICS2 == 62)

##3-year
sr3yr2001_sd <- sum(df2001_sd_62$years.survived>=3)/nrow(df2001_sd)
sr3yr2001sm_sd <- sum((df2001_sd_62$years.survived>=3 & df2001_sd_62$biz_size == 0), na.rm = TRUE)/sum(df2001_sd$biz_size == 0, na.rm = TRUE)
sr3yr2001non_sd <- sum((df2001_sd_62$years.survived>=3 & df2001_sd_62$biz_size == 1), na.rm = TRUE)/sum(df2001_sd$biz_size == 1, na.rm = TRUE)

sr3yr2002_sd <- sum(df2002_sd_62$years.survived>=3)/nrow(df2002_sd) 
sr3yr2002sm_sd <- sum((df2002_sd_62$years.survived>=3 & df2002_sd_62$biz_size == 0), na.rm = TRUE)/sum(df2002_sd$biz_size == 0, na.rm = TRUE)
sr3yr2002non_sd <- sum((df2002_sd_62$years.survived>=3 & df2002_sd_62$biz_size == 1), na.rm = TRUE)/sum(df2002_sd$biz_size == 1, na.rm = TRUE)

sr3yr2003_sd <- sum(df2003_sd_62$years.survived>=3)/nrow(df2003_sd)
sr3yr2003sm_sd <- sum((df2003_sd_62$years.survived>=3 & df2003_sd_62$biz_size == 0), na.rm = TRUE)/sum(df2003_sd$biz_size == 0, na.rm = TRUE)
sr3yr2003non_sd <- sum((df2003_sd_62$years.survived>=3 & df2003_sd_62$biz_size == 1), na.rm = TRUE)/sum(df2003_sd$biz_size == 1, na.rm = TRUE)

sr3yr2004_sd <- sum(df2004_sd_62$years.survived>=3)/nrow(df2004_sd) 
sr3yr2004sm_sd <- sum((df2004_sd_62$years.survived>=3 & df2004_sd_62$biz_size == 0), na.rm = TRUE)/sum(df2004_sd$biz_size == 0, na.rm = TRUE)
sr3yr2004non_sd <- sum((df2004_sd_62$years.survived>=3 & df2004_sd_62$biz_size == 1), na.rm = TRUE)/sum(df2004_sd$biz_size == 1, na.rm = TRUE)

sr3yr2005_sd <- sum(df2005_sd_62$years.survived>=3)/nrow(df2005_sd) 
sr3yr2005sm_sd <- sum((df2005_sd_62$years.survived>=3 & df2005_sd_62$biz_size == 0), na.rm = TRUE)/sum(df2005_sd$biz_size == 0, na.rm = TRUE)
sr3yr2005non_sd <- sum((df2005_sd_62$years.survived>=3 & df2005_sd_62$biz_size == 1), na.rm = TRUE)/sum(df2005_sd$biz_size == 1, na.rm = TRUE)

sr3yr2006_sd <- sum(df2006_sd_62$years.survived>=3)/nrow(df2006_sd) 
sr3yr2006sm_sd <- sum((df2006_sd_62$years.survived>=3 & df2006_sd_62$biz_size == 0), na.rm = TRUE)/sum(df2006_sd$biz_size == 0, na.rm = TRUE)
sr3yr2006non_sd <- sum((df2006_sd_62$years.survived>=3 & df2006_sd_62$biz_size == 1), na.rm = TRUE)/sum(df2006_sd$biz_size == 1, na.rm = TRUE)

##5-year
sr5yr2001_sd <- sum(df2001_sd_62$years.survived>=5)/nrow(df2001_sd)
sr5yr2001sm_sd <- sum((df2001_sd_62$years.survived>=5 & df2001_sd_62$biz_size == 0), na.rm = TRUE)/sum(df2001_sd$biz_size == 0, na.rm = TRUE)
sr5yr2001non_sd <- sum((df2001_sd_62$years.survived>=5 & df2001_sd_62$biz_size == 1), na.rm = TRUE)/sum(df2001_sd$biz_size == 1, na.rm = TRUE)

sr5yr2002_sd <- sum(df2002_sd_62$years.survived>=5)/nrow(df2002_sd) 
sr5yr2002sm_sd <- sum((df2002_sd_62$years.survived>=5 & df2002_sd_62$biz_size == 0), na.rm = TRUE)/sum(df2002_sd$biz_size == 0, na.rm = TRUE)
sr5yr2002non_sd <- sum((df2002_sd_62$years.survived>=5 & df2002_sd_62$biz_size == 1), na.rm = TRUE)/sum(df2002_sd$biz_size == 1, na.rm = TRUE)

sr5yr2003_sd <- sum(df2003_sd_62$years.survived>=5)/nrow(df2003_sd)
sr5yr2003sm_sd <- sum((df2003_sd_62$years.survived>=5 & df2003_sd_62$biz_size == 0), na.rm = TRUE)/sum(df2003_sd$biz_size == 0, na.rm = TRUE)
sr5yr2003non_sd <- sum((df2003_sd_62$years.survived>=5 & df2003_sd_62$biz_size == 1), na.rm = TRUE)/sum(df2003_sd$biz_size == 1, na.rm = TRUE)

sr5yr2004_sd <- sum(df2004_sd_62$years.survived>=5)/nrow(df2004_sd) 
sr5yr2004sm_sd <- sum((df2004_sd_62$years.survived>=5 & df2004_sd_62$biz_size == 0), na.rm = TRUE)/sum(df2004_sd$biz_size == 0, na.rm = TRUE)
sr5yr2004non_sd <- sum((df2004_sd_62$years.survived>=5 & df2004_sd_62$biz_size == 1), na.rm = TRUE)/sum(df2004_sd$biz_size == 1, na.rm = TRUE)

sr5yr2005_sd <- sum(df2005_sd_62$years.survived>=5)/nrow(df2005_sd) 
sr5yr2005sm_sd <- sum((df2005_sd_62$years.survived>=5 & df2005_sd_62$biz_size == 0), na.rm = TRUE)/sum(df2005_sd$biz_size == 0, na.rm = TRUE)
sr5yr2005non_sd <- sum((df2005_sd_62$years.survived>=5 & df2005_sd_62$biz_size == 1), na.rm = TRUE)/sum(df2005_sd$biz_size == 1, na.rm = TRUE)

sr5yr2006_sd <- sum(df2006_sd_62$years.survived>=5)/nrow(df2006_sd) 
sr5yr2006sm_sd <- sum((df2006_sd_62$years.survived>=5 & df2006_sd_62$biz_size == 0), na.rm = TRUE)/sum(df2006_sd$biz_size == 0, na.rm = TRUE)
sr5yr2006non_sd <- sum((df2006_sd_62$years.survived>=5 & df2006_sd_62$biz_size == 1), na.rm = TRUE)/sum(df2006_sd$biz_size == 1, na.rm = TRUE)


##10-year
sr10yr2001_sd <- (sum(df2001_sd_62$years.survived>=10))/nrow(df2001_sd) 
sr10yr2001sm_sd <- sum((df2001_sd_62$years.survived>=10 & df2001_sd_62$biz_size == 0), na.rm = TRUE)/sum(df2001_sd$biz_size == 0, na.rm = TRUE)
sr10yr2001non_sd <- sum((df2001_sd_62$years.survived>=10 & df2001_sd_62$biz_size == 1), na.rm = TRUE)/sum(df2002_sd$biz_size == 1, na.rm = TRUE)


sr10yr2002_sd <- sum(df2002_sd_62$years.survived>=10)/nrow(df2002_sd) 
sr10yr2002sm_sd <- sum((df2002_sd_62$years.survived>=10 & df2002_sd_62$biz_size == 0), na.rm = TRUE)/sum(df2002_sd$biz_size == 0, na.rm = TRUE)
sr10yr2002non_sd <- sum((df2002_sd_62$years.survived>=10 & df2002_sd_62$biz_size == 1), na.rm = TRUE)/sum(df2002_sd$biz_size == 1, na.rm = TRUE)

sr10yr2003_sd <- sum(df2003_sd_62$years.survived>=10)/nrow(df2003_sd)
sr10yr2003sm_sd <- sum((df2003_sd_62$years.survived>=10 & df2003_sd_62$biz_size == 0), na.rm = TRUE)/sum(df2003_sd$biz_size == 0, na.rm = TRUE)
sr10yr2003non_sd <- sum((df2003_sd_62$years.survived>=10 & df2003_sd_62$biz_size == 1), na.rm = TRUE)/sum(df2003_sd$biz_size == 1, na.rm = TRUE)

sr10yr2004_sd <- sum(df2004_sd_62$years.survived>=10)/nrow(df2004_sd) 
sr10yr2004sm_sd <- sum((df2004_sd_62$years.survived>=10 & df2004_sd_62$biz_size == 0), na.rm = TRUE)/sum(df2004_sd$biz_size == 0, na.rm = TRUE)
sr10yr2004non_sd <- sum((df2004_sd_62$years.survived>=10 & df2004_sd_62$biz_size == 1), na.rm = TRUE)/sum(df2004_sd$biz_size == 1, na.rm = TRUE)

sr10yr2005_sd <- sum(df2005_sd_62$years.survived>=10)/nrow(df2005_sd) 
sr10yr2005sm_sd <- sum((df2005_sd_62$years.survived>=10 & df2005_sd_62$biz_size == 0), na.rm = TRUE)/sum(df2005_sd$biz_size == 0, na.rm = TRUE)
sr10yr2005non_sd <- sum((df2005_sd_62$years.survived>=10 & df2005_sd_62$biz_size == 1), na.rm = TRUE)/sum(df2005_sd$biz_size == 1, na.rm = TRUE)

sr10yr2006_sd <- sum(df2006_sd_62$years.survived>=10)/nrow(df2006_sd) 
sr10yr2006sm_sd <- sum((df2006_sd_62$years.survived>=10 & df2006_sd_62$biz_size == 0), na.rm = TRUE)/sum(df2006_sd$biz_size == 0, na.rm = TRUE)
sr10yr2006non_sd <- sum((df2006_sd_62$years.survived>=10 & df2006_sd_62$biz_size == 1), na.rm = TRUE)/sum(df2006_sd$biz_size == 1, na.rm = TRUE)

##bound all survival rates together and reformated dataframe

survivalrate_sd <- cbind(sr3yr2001_sd, sr3yr2001sm_sd, sr3yr2001non_sd, sr5yr2001_sd, sr5yr2001sm_sd, sr5yr2001non_sd, 
                         sr10yr2001_sd, sr10yr2001sm_sd, sr10yr2001non_sd,
                         sr3yr2002_sd, sr3yr2002sm_sd, sr3yr2002non_sd, sr5yr2002_sd, sr5yr2002sm_sd, sr5yr2002non_sd, 
                         sr10yr2002_sd, sr10yr2002sm_sd, sr10yr2002non_sd,
                         sr3yr2003_sd, sr3yr2003sm_sd, sr3yr2003non_sd, sr5yr2003_sd, sr5yr2003sm_sd, sr5yr2003non_sd, 
                         sr10yr2003_sd, sr10yr2003sm_sd, sr10yr2003non_sd,
                         sr3yr2004_sd, sr3yr2004sm_sd, sr3yr2004non_sd, sr5yr2004_sd, sr5yr2004sm_sd, sr5yr2004non_sd, 
                         sr10yr2004_sd, sr10yr2004sm_sd, sr10yr2004non_sd,
                         sr3yr2005_sd, sr3yr2005sm_sd, sr3yr2005non_sd, sr5yr2005_sd, sr5yr2005sm_sd, sr5yr2005non_sd, 
                         sr10yr2005_sd, sr10yr2005sm_sd, sr10yr2005non_sd,
                         sr3yr2006_sd, sr3yr2006sm_sd, sr3yr2006non_sd, sr5yr2006_sd, sr5yr2006sm_sd, sr5yr2006non_sd, 
                         sr10yr2006_sd, sr10yr2006sm_sd, sr10yr2006non_sd)
srnames_sd <- c("3yr_2001_all_sd", "3yr_2001_sm_sd", 
                "3yr_2001_non_sd", "5yr_2001_all_sd", "5yr_2001_sm_sd", "5yr_2001_non_sd", "10yr_2001_all_sd", 
                "10yr_2001_sm_sd", "10yr_2001_non_sd",
                "3yr_2002_all_sd", "3yr_2002_sm_sd", "3yr_2002_non_sd", "5yr_2002_all_sd", "5yr_2002_sm_sd", "5yr_2002_non_sd", 
                "10yr_2002_all_sd", "10yr_2002_sm_sd", "10yr_2002_non_sd",
                "3yr_2003_all_sd", "3yr_2003_sm_sd", "3yr_2003_non_sd", "5yr_2003_all_sd", "5yr_2003_sm_sd", "5yr_2003_non_sd", 
                "10yr_2003_all_sd", "10yr_2003_sm_sd", "10yr_2003_non_sd",
                "3yr_2004_all_sd", "3yr_2004_sm_sd", "3yr_2004_non_sd", "5yr_2004_all_sd", "5yr_2004_sm_sd", "5yr_2004_non_sd", 
                "10yr_2004_all_sd", "10yr_2004_sm_sd", "10yr_2004_non_sd",
                "3yr_2005_all_sd", "3yr_2005_sm_sd", "3yr_2005_non_sd", "5yr_2005_all_sd", "5yr_2005_sm_sd", "5yr_2005_non_sd", 
                "10yr2005_all_sd", "10yr_2005_sm_sd", "10yr_2005_non_sd",
                "3yr_2006_all_sd", "3yr_2006_sm_sd", "3yr_2006_non_sd", "5yr_2006_all_sd", "5yr_2006_sm_sd", "5yr_2006_non_sd", 
                "10yr_2006_all_sd", "10yr_2006_sm_sd", "10yr_2006_non_sd")
colnames(survivalrate_sd) <- srnames_sd

sr_sd <- as.data.frame(survivalrate_sd)

survival.rates_sd <- sr_sd %>% 
  gather("3yr_2001_all_sd":"10yr_2006_non_sd", key = "time_year_type", value = "survivalrate_sd") %>% 
  separate(time_year_type, into = c("time", "year", "type"), sep = "_") %>% 
  arrange(time, year) %>% 
  unite("time_type", time, type, sep = "_") 

survival.rates_sd$time_type[18] <- "10yr_all"
survival.rates_sd$year[18] <- 2005

survival.rates_sd_62 <- survival.rates_sd %>% 
  spread(key = "time_type", value = "survivalrate_sd")

survival.rates_sd_62[, c(1, 5, 7, 6, 8, 10, 9, 2, 4, 3)]

##71####

df2001_sd_71 <- filter(df2001_sd, NAICS2 == 71)
df2002_sd_71 <- filter(df2002_sd, NAICS2 == 71)
df2003_sd_71 <- filter(df2003_sd, NAICS2 == 71)
df2004_sd_71 <- filter(df2004_sd, NAICS2 == 71)
df2005_sd_71 <- filter(df2005_sd, NAICS2 == 71)
df2006_sd_71 <- filter(df2006_sd, NAICS2 == 71)

##3-year
sr3yr2001_sd <- sum(df2001_sd_71$years.survived>=3)/nrow(df2001_sd)
sr3yr2001sm_sd <- sum((df2001_sd_71$years.survived>=3 & df2001_sd_71$biz_size == 0), na.rm = TRUE)/sum(df2001_sd$biz_size == 0, na.rm = TRUE)
sr3yr2001non_sd <- sum((df2001_sd_71$years.survived>=3 & df2001_sd_71$biz_size == 1), na.rm = TRUE)/sum(df2001_sd$biz_size == 1, na.rm = TRUE)

sr3yr2002_sd <- sum(df2002_sd_71$years.survived>=3)/nrow(df2002_sd) 
sr3yr2002sm_sd <- sum((df2002_sd_71$years.survived>=3 & df2002_sd_71$biz_size == 0), na.rm = TRUE)/sum(df2002_sd$biz_size == 0, na.rm = TRUE)
sr3yr2002non_sd <- sum((df2002_sd_71$years.survived>=3 & df2002_sd_71$biz_size == 1), na.rm = TRUE)/sum(df2002_sd$biz_size == 1, na.rm = TRUE)

sr3yr2003_sd <- sum(df2003_sd_71$years.survived>=3)/nrow(df2003_sd)
sr3yr2003sm_sd <- sum((df2003_sd_71$years.survived>=3 & df2003_sd_71$biz_size == 0), na.rm = TRUE)/sum(df2003_sd$biz_size == 0, na.rm = TRUE)
sr3yr2003non_sd <- sum((df2003_sd_71$years.survived>=3 & df2003_sd_71$biz_size == 1), na.rm = TRUE)/sum(df2003_sd$biz_size == 1, na.rm = TRUE)

sr3yr2004_sd <- sum(df2004_sd_71$years.survived>=3)/nrow(df2004_sd) 
sr3yr2004sm_sd <- sum((df2004_sd_71$years.survived>=3 & df2004_sd_71$biz_size == 0), na.rm = TRUE)/sum(df2004_sd$biz_size == 0, na.rm = TRUE)
sr3yr2004non_sd <- sum((df2004_sd_71$years.survived>=3 & df2004_sd_71$biz_size == 1), na.rm = TRUE)/sum(df2004_sd$biz_size == 1, na.rm = TRUE)

sr3yr2005_sd <- sum(df2005_sd_71$years.survived>=3)/nrow(df2005_sd) 
sr3yr2005sm_sd <- sum((df2005_sd_71$years.survived>=3 & df2005_sd_71$biz_size == 0), na.rm = TRUE)/sum(df2005_sd$biz_size == 0, na.rm = TRUE)
sr3yr2005non_sd <- sum((df2005_sd_71$years.survived>=3 & df2005_sd_71$biz_size == 1), na.rm = TRUE)/sum(df2005_sd$biz_size == 1, na.rm = TRUE)

sr3yr2006_sd <- sum(df2006_sd_71$years.survived>=3)/nrow(df2006_sd) 
sr3yr2006sm_sd <- sum((df2006_sd_71$years.survived>=3 & df2006_sd_71$biz_size == 0), na.rm = TRUE)/sum(df2006_sd$biz_size == 0, na.rm = TRUE)
sr3yr2006non_sd <- sum((df2006_sd_71$years.survived>=3 & df2006_sd_71$biz_size == 1), na.rm = TRUE)/sum(df2006_sd$biz_size == 1, na.rm = TRUE)

##5-year
sr5yr2001_sd <- sum(df2001_sd_71$years.survived>=5)/nrow(df2001_sd)
sr5yr2001sm_sd <- sum((df2001_sd_71$years.survived>=5 & df2001_sd_71$biz_size == 0), na.rm = TRUE)/sum(df2001_sd$biz_size == 0, na.rm = TRUE)
sr5yr2001non_sd <- sum((df2001_sd_71$years.survived>=5 & df2001_sd_71$biz_size == 1), na.rm = TRUE)/sum(df2001_sd$biz_size == 1, na.rm = TRUE)

sr5yr2002_sd <- sum(df2002_sd_71$years.survived>=5)/nrow(df2002_sd) 
sr5yr2002sm_sd <- sum((df2002_sd_71$years.survived>=5 & df2002_sd_71$biz_size == 0), na.rm = TRUE)/sum(df2002_sd$biz_size == 0, na.rm = TRUE)
sr5yr2002non_sd <- sum((df2002_sd_71$years.survived>=5 & df2002_sd_71$biz_size == 1), na.rm = TRUE)/sum(df2002_sd$biz_size == 1, na.rm = TRUE)

sr5yr2003_sd <- sum(df2003_sd_71$years.survived>=5)/nrow(df2003_sd)
sr5yr2003sm_sd <- sum((df2003_sd_71$years.survived>=5 & df2003_sd_71$biz_size == 0), na.rm = TRUE)/sum(df2003_sd$biz_size == 0, na.rm = TRUE)
sr5yr2003non_sd <- sum((df2003_sd_71$years.survived>=5 & df2003_sd_71$biz_size == 1), na.rm = TRUE)/sum(df2003_sd$biz_size == 1, na.rm = TRUE)

sr5yr2004_sd <- sum(df2004_sd_71$years.survived>=5)/nrow(df2004_sd) 
sr5yr2004sm_sd <- sum((df2004_sd_71$years.survived>=5 & df2004_sd_71$biz_size == 0), na.rm = TRUE)/sum(df2004_sd$biz_size == 0, na.rm = TRUE)
sr5yr2004non_sd <- sum((df2004_sd_71$years.survived>=5 & df2004_sd_71$biz_size == 1), na.rm = TRUE)/sum(df2004_sd$biz_size == 1, na.rm = TRUE)

sr5yr2005_sd <- sum(df2005_sd_71$years.survived>=5)/nrow(df2005_sd) 
sr5yr2005sm_sd <- sum((df2005_sd_71$years.survived>=5 & df2005_sd_71$biz_size == 0), na.rm = TRUE)/sum(df2005_sd$biz_size == 0, na.rm = TRUE)
sr5yr2005non_sd <- sum((df2005_sd_71$years.survived>=5 & df2005_sd_71$biz_size == 1), na.rm = TRUE)/sum(df2005_sd$biz_size == 1, na.rm = TRUE)

sr5yr2006_sd <- sum(df2006_sd_71$years.survived>=5)/nrow(df2006_sd) 
sr5yr2006sm_sd <- sum((df2006_sd_71$years.survived>=5 & df2006_sd_71$biz_size == 0), na.rm = TRUE)/sum(df2006_sd$biz_size == 0, na.rm = TRUE)
sr5yr2006non_sd <- sum((df2006_sd_71$years.survived>=5 & df2006_sd_71$biz_size == 1), na.rm = TRUE)/sum(df2006_sd$biz_size == 1, na.rm = TRUE)


##10-year
sr10yr2001_sd <- (sum(df2001_sd_71$years.survived>=10))/nrow(df2001_sd) 
sr10yr2001sm_sd <- sum((df2001_sd_71$years.survived>=10 & df2001_sd_71$biz_size == 0), na.rm = TRUE)/sum(df2001_sd$biz_size == 0, na.rm = TRUE)
sr10yr2001non_sd <- sum((df2001_sd_71$years.survived>=10 & df2001_sd_71$biz_size == 1), na.rm = TRUE)/sum(df2002_sd$biz_size == 1, na.rm = TRUE)


sr10yr2002_sd <- sum(df2002_sd_71$years.survived>=10)/nrow(df2002_sd) 
sr10yr2002sm_sd <- sum((df2002_sd_71$years.survived>=10 & df2002_sd_71$biz_size == 0), na.rm = TRUE)/sum(df2002_sd$biz_size == 0, na.rm = TRUE)
sr10yr2002non_sd <- sum((df2002_sd_71$years.survived>=10 & df2002_sd_71$biz_size == 1), na.rm = TRUE)/sum(df2002_sd$biz_size == 1, na.rm = TRUE)

sr10yr2003_sd <- sum(df2003_sd_71$years.survived>=10)/nrow(df2003_sd)
sr10yr2003sm_sd <- sum((df2003_sd_71$years.survived>=10 & df2003_sd_71$biz_size == 0), na.rm = TRUE)/sum(df2003_sd$biz_size == 0, na.rm = TRUE)
sr10yr2003non_sd <- sum((df2003_sd_71$years.survived>=10 & df2003_sd_71$biz_size == 1), na.rm = TRUE)/sum(df2003_sd$biz_size == 1, na.rm = TRUE)

sr10yr2004_sd <- sum(df2004_sd_71$years.survived>=10)/nrow(df2004_sd) 
sr10yr2004sm_sd <- sum((df2004_sd_71$years.survived>=10 & df2004_sd_71$biz_size == 0), na.rm = TRUE)/sum(df2004_sd$biz_size == 0, na.rm = TRUE)
sr10yr2004non_sd <- sum((df2004_sd_71$years.survived>=10 & df2004_sd_71$biz_size == 1), na.rm = TRUE)/sum(df2004_sd$biz_size == 1, na.rm = TRUE)

sr10yr2005_sd <- sum(df2005_sd_71$years.survived>=10)/nrow(df2005_sd) 
sr10yr2005sm_sd <- sum((df2005_sd_71$years.survived>=10 & df2005_sd_71$biz_size == 0), na.rm = TRUE)/sum(df2005_sd$biz_size == 0, na.rm = TRUE)
sr10yr2005non_sd <- sum((df2005_sd_71$years.survived>=10 & df2005_sd_71$biz_size == 1), na.rm = TRUE)/sum(df2005_sd$biz_size == 1, na.rm = TRUE)

sr10yr2006_sd <- sum(df2006_sd_71$years.survived>=10)/nrow(df2006_sd) 
sr10yr2006sm_sd <- sum((df2006_sd_71$years.survived>=10 & df2006_sd_71$biz_size == 0), na.rm = TRUE)/sum(df2006_sd$biz_size == 0, na.rm = TRUE)
sr10yr2006non_sd <- sum((df2006_sd_71$years.survived>=10 & df2006_sd_71$biz_size == 1), na.rm = TRUE)/sum(df2006_sd$biz_size == 1, na.rm = TRUE)

##bound all survival rates together and reformated dataframe

survivalrate_sd <- cbind(sr3yr2001_sd, sr3yr2001sm_sd, sr3yr2001non_sd, sr5yr2001_sd, sr5yr2001sm_sd, sr5yr2001non_sd, 
                         sr10yr2001_sd, sr10yr2001sm_sd, sr10yr2001non_sd,
                         sr3yr2002_sd, sr3yr2002sm_sd, sr3yr2002non_sd, sr5yr2002_sd, sr5yr2002sm_sd, sr5yr2002non_sd, 
                         sr10yr2002_sd, sr10yr2002sm_sd, sr10yr2002non_sd,
                         sr3yr2003_sd, sr3yr2003sm_sd, sr3yr2003non_sd, sr5yr2003_sd, sr5yr2003sm_sd, sr5yr2003non_sd, 
                         sr10yr2003_sd, sr10yr2003sm_sd, sr10yr2003non_sd,
                         sr3yr2004_sd, sr3yr2004sm_sd, sr3yr2004non_sd, sr5yr2004_sd, sr5yr2004sm_sd, sr5yr2004non_sd, 
                         sr10yr2004_sd, sr10yr2004sm_sd, sr10yr2004non_sd,
                         sr3yr2005_sd, sr3yr2005sm_sd, sr3yr2005non_sd, sr5yr2005_sd, sr5yr2005sm_sd, sr5yr2005non_sd, 
                         sr10yr2005_sd, sr10yr2005sm_sd, sr10yr2005non_sd,
                         sr3yr2006_sd, sr3yr2006sm_sd, sr3yr2006non_sd, sr5yr2006_sd, sr5yr2006sm_sd, sr5yr2006non_sd, 
                         sr10yr2006_sd, sr10yr2006sm_sd, sr10yr2006non_sd)
srnames_sd <- c("3yr_2001_all_sd", "3yr_2001_sm_sd", 
                "3yr_2001_non_sd", "5yr_2001_all_sd", "5yr_2001_sm_sd", "5yr_2001_non_sd", "10yr_2001_all_sd", 
                "10yr_2001_sm_sd", "10yr_2001_non_sd",
                "3yr_2002_all_sd", "3yr_2002_sm_sd", "3yr_2002_non_sd", "5yr_2002_all_sd", "5yr_2002_sm_sd", "5yr_2002_non_sd", 
                "10yr_2002_all_sd", "10yr_2002_sm_sd", "10yr_2002_non_sd",
                "3yr_2003_all_sd", "3yr_2003_sm_sd", "3yr_2003_non_sd", "5yr_2003_all_sd", "5yr_2003_sm_sd", "5yr_2003_non_sd", 
                "10yr_2003_all_sd", "10yr_2003_sm_sd", "10yr_2003_non_sd",
                "3yr_2004_all_sd", "3yr_2004_sm_sd", "3yr_2004_non_sd", "5yr_2004_all_sd", "5yr_2004_sm_sd", "5yr_2004_non_sd", 
                "10yr_2004_all_sd", "10yr_2004_sm_sd", "10yr_2004_non_sd",
                "3yr_2005_all_sd", "3yr_2005_sm_sd", "3yr_2005_non_sd", "5yr_2005_all_sd", "5yr_2005_sm_sd", "5yr_2005_non_sd", 
                "10yr2005_all_sd", "10yr_2005_sm_sd", "10yr_2005_non_sd",
                "3yr_2006_all_sd", "3yr_2006_sm_sd", "3yr_2006_non_sd", "5yr_2006_all_sd", "5yr_2006_sm_sd", "5yr_2006_non_sd", 
                "10yr_2006_all_sd", "10yr_2006_sm_sd", "10yr_2006_non_sd")
colnames(survivalrate_sd) <- srnames_sd

sr_sd <- as.data.frame(survivalrate_sd)

survival.rates_sd <- sr_sd %>% 
  gather("3yr_2001_all_sd":"10yr_2006_non_sd", key = "time_year_type", value = "survivalrate_sd") %>% 
  separate(time_year_type, into = c("time", "year", "type"), sep = "_") %>% 
  arrange(time, year) %>% 
  unite("time_type", time, type, sep = "_") 

survival.rates_sd$time_type[18] <- "10yr_all"
survival.rates_sd$year[18] <- 2005

survival.rates_sd_71 <- survival.rates_sd %>% 
  spread(key = "time_type", value = "survivalrate_sd")

survival.rates_sd_71[, c(1, 5, 7, 6, 8, 10, 9, 2, 4, 3)]

##72####

df2001_sd_72 <- filter(df2001_sd, NAICS2 == 72)
df2002_sd_72 <- filter(df2002_sd, NAICS2 == 72)
df2003_sd_72 <- filter(df2003_sd, NAICS2 == 72)
df2004_sd_72 <- filter(df2004_sd, NAICS2 == 72)
df2005_sd_72 <- filter(df2005_sd, NAICS2 == 72)
df2006_sd_72 <- filter(df2006_sd, NAICS2 == 72)

##3-year
sr3yr2001_sd <- sum(df2001_sd_72$years.survived>=3)/nrow(df2001_sd)
sr3yr2001sm_sd <- sum((df2001_sd_72$years.survived>=3 & df2001_sd_72$biz_size == 0), na.rm = TRUE)/sum(df2001_sd$biz_size == 0, na.rm = TRUE)
sr3yr2001non_sd <- sum((df2001_sd_72$years.survived>=3 & df2001_sd_72$biz_size == 1), na.rm = TRUE)/sum(df2001_sd$biz_size == 1, na.rm = TRUE)

sr3yr2002_sd <- sum(df2002_sd_72$years.survived>=3)/nrow(df2002_sd) 
sr3yr2002sm_sd <- sum((df2002_sd_72$years.survived>=3 & df2002_sd_72$biz_size == 0), na.rm = TRUE)/sum(df2002_sd$biz_size == 0, na.rm = TRUE)
sr3yr2002non_sd <- sum((df2002_sd_72$years.survived>=3 & df2002_sd_72$biz_size == 1), na.rm = TRUE)/sum(df2002_sd$biz_size == 1, na.rm = TRUE)

sr3yr2003_sd <- sum(df2003_sd_72$years.survived>=3)/nrow(df2003_sd)
sr3yr2003sm_sd <- sum((df2003_sd_72$years.survived>=3 & df2003_sd_72$biz_size == 0), na.rm = TRUE)/sum(df2003_sd$biz_size == 0, na.rm = TRUE)
sr3yr2003non_sd <- sum((df2003_sd_72$years.survived>=3 & df2003_sd_72$biz_size == 1), na.rm = TRUE)/sum(df2003_sd$biz_size == 1, na.rm = TRUE)

sr3yr2004_sd <- sum(df2004_sd_72$years.survived>=3)/nrow(df2004_sd) 
sr3yr2004sm_sd <- sum((df2004_sd_72$years.survived>=3 & df2004_sd_72$biz_size == 0), na.rm = TRUE)/sum(df2004_sd$biz_size == 0, na.rm = TRUE)
sr3yr2004non_sd <- sum((df2004_sd_72$years.survived>=3 & df2004_sd_72$biz_size == 1), na.rm = TRUE)/sum(df2004_sd$biz_size == 1, na.rm = TRUE)

sr3yr2005_sd <- sum(df2005_sd_72$years.survived>=3)/nrow(df2005_sd) 
sr3yr2005sm_sd <- sum((df2005_sd_72$years.survived>=3 & df2005_sd_72$biz_size == 0), na.rm = TRUE)/sum(df2005_sd$biz_size == 0, na.rm = TRUE)
sr3yr2005non_sd <- sum((df2005_sd_72$years.survived>=3 & df2005_sd_72$biz_size == 1), na.rm = TRUE)/sum(df2005_sd$biz_size == 1, na.rm = TRUE)

sr3yr2006_sd <- sum(df2006_sd_72$years.survived>=3)/nrow(df2006_sd) 
sr3yr2006sm_sd <- sum((df2006_sd_72$years.survived>=3 & df2006_sd_72$biz_size == 0), na.rm = TRUE)/sum(df2006_sd$biz_size == 0, na.rm = TRUE)
sr3yr2006non_sd <- sum((df2006_sd_72$years.survived>=3 & df2006_sd_72$biz_size == 1), na.rm = TRUE)/sum(df2006_sd$biz_size == 1, na.rm = TRUE)

##5-year
sr5yr2001_sd <- sum(df2001_sd_72$years.survived>=5)/nrow(df2001_sd)
sr5yr2001sm_sd <- sum((df2001_sd_72$years.survived>=5 & df2001_sd_72$biz_size == 0), na.rm = TRUE)/sum(df2001_sd$biz_size == 0, na.rm = TRUE)
sr5yr2001non_sd <- sum((df2001_sd_72$years.survived>=5 & df2001_sd_72$biz_size == 1), na.rm = TRUE)/sum(df2001_sd$biz_size == 1, na.rm = TRUE)

sr5yr2002_sd <- sum(df2002_sd_72$years.survived>=5)/nrow(df2002_sd) 
sr5yr2002sm_sd <- sum((df2002_sd_72$years.survived>=5 & df2002_sd_72$biz_size == 0), na.rm = TRUE)/sum(df2002_sd$biz_size == 0, na.rm = TRUE)
sr5yr2002non_sd <- sum((df2002_sd_72$years.survived>=5 & df2002_sd_72$biz_size == 1), na.rm = TRUE)/sum(df2002_sd$biz_size == 1, na.rm = TRUE)

sr5yr2003_sd <- sum(df2003_sd_72$years.survived>=5)/nrow(df2003_sd)
sr5yr2003sm_sd <- sum((df2003_sd_72$years.survived>=5 & df2003_sd_72$biz_size == 0), na.rm = TRUE)/sum(df2003_sd$biz_size == 0, na.rm = TRUE)
sr5yr2003non_sd <- sum((df2003_sd_72$years.survived>=5 & df2003_sd_72$biz_size == 1), na.rm = TRUE)/sum(df2003_sd$biz_size == 1, na.rm = TRUE)

sr5yr2004_sd <- sum(df2004_sd_72$years.survived>=5)/nrow(df2004_sd) 
sr5yr2004sm_sd <- sum((df2004_sd_72$years.survived>=5 & df2004_sd_72$biz_size == 0), na.rm = TRUE)/sum(df2004_sd$biz_size == 0, na.rm = TRUE)
sr5yr2004non_sd <- sum((df2004_sd_72$years.survived>=5 & df2004_sd_72$biz_size == 1), na.rm = TRUE)/sum(df2004_sd$biz_size == 1, na.rm = TRUE)

sr5yr2005_sd <- sum(df2005_sd_72$years.survived>=5)/nrow(df2005_sd) 
sr5yr2005sm_sd <- sum((df2005_sd_72$years.survived>=5 & df2005_sd_72$biz_size == 0), na.rm = TRUE)/sum(df2005_sd$biz_size == 0, na.rm = TRUE)
sr5yr2005non_sd <- sum((df2005_sd_72$years.survived>=5 & df2005_sd_72$biz_size == 1), na.rm = TRUE)/sum(df2005_sd$biz_size == 1, na.rm = TRUE)

sr5yr2006_sd <- sum(df2006_sd_72$years.survived>=5)/nrow(df2006_sd) 
sr5yr2006sm_sd <- sum((df2006_sd_72$years.survived>=5 & df2006_sd_72$biz_size == 0), na.rm = TRUE)/sum(df2006_sd$biz_size == 0, na.rm = TRUE)
sr5yr2006non_sd <- sum((df2006_sd_72$years.survived>=5 & df2006_sd_72$biz_size == 1), na.rm = TRUE)/sum(df2006_sd$biz_size == 1, na.rm = TRUE)


##10-year
sr10yr2001_sd <- (sum(df2001_sd_72$years.survived>=10))/nrow(df2001_sd) 
sr10yr2001sm_sd <- sum((df2001_sd_72$years.survived>=10 & df2001_sd_72$biz_size == 0), na.rm = TRUE)/sum(df2001_sd$biz_size == 0, na.rm = TRUE)
sr10yr2001non_sd <- sum((df2001_sd_72$years.survived>=10 & df2001_sd_72$biz_size == 1), na.rm = TRUE)/sum(df2002_sd$biz_size == 1, na.rm = TRUE)


sr10yr2002_sd <- sum(df2002_sd_72$years.survived>=10)/nrow(df2002_sd) 
sr10yr2002sm_sd <- sum((df2002_sd_72$years.survived>=10 & df2002_sd_72$biz_size == 0), na.rm = TRUE)/sum(df2002_sd$biz_size == 0, na.rm = TRUE)
sr10yr2002non_sd <- sum((df2002_sd_72$years.survived>=10 & df2002_sd_72$biz_size == 1), na.rm = TRUE)/sum(df2002_sd$biz_size == 1, na.rm = TRUE)

sr10yr2003_sd <- sum(df2003_sd_72$years.survived>=10)/nrow(df2003_sd)
sr10yr2003sm_sd <- sum((df2003_sd_72$years.survived>=10 & df2003_sd_72$biz_size == 0), na.rm = TRUE)/sum(df2003_sd$biz_size == 0, na.rm = TRUE)
sr10yr2003non_sd <- sum((df2003_sd_72$years.survived>=10 & df2003_sd_72$biz_size == 1), na.rm = TRUE)/sum(df2003_sd$biz_size == 1, na.rm = TRUE)

sr10yr2004_sd <- sum(df2004_sd_72$years.survived>=10)/nrow(df2004_sd) 
sr10yr2004sm_sd <- sum((df2004_sd_72$years.survived>=10 & df2004_sd_72$biz_size == 0), na.rm = TRUE)/sum(df2004_sd$biz_size == 0, na.rm = TRUE)
sr10yr2004non_sd <- sum((df2004_sd_72$years.survived>=10 & df2004_sd_72$biz_size == 1), na.rm = TRUE)/sum(df2004_sd$biz_size == 1, na.rm = TRUE)

sr10yr2005_sd <- sum(df2005_sd_72$years.survived>=10)/nrow(df2005_sd) 
sr10yr2005sm_sd <- sum((df2005_sd_72$years.survived>=10 & df2005_sd_72$biz_size == 0), na.rm = TRUE)/sum(df2005_sd$biz_size == 0, na.rm = TRUE)
sr10yr2005non_sd <- sum((df2005_sd_72$years.survived>=10 & df2005_sd_72$biz_size == 1), na.rm = TRUE)/sum(df2005_sd$biz_size == 1, na.rm = TRUE)

sr10yr2006_sd <- sum(df2006_sd_72$years.survived>=10)/nrow(df2006_sd) 
sr10yr2006sm_sd <- sum((df2006_sd_72$years.survived>=10 & df2006_sd_72$biz_size == 0), na.rm = TRUE)/sum(df2006_sd$biz_size == 0, na.rm = TRUE)
sr10yr2006non_sd <- sum((df2006_sd_72$years.survived>=10 & df2006_sd_72$biz_size == 1), na.rm = TRUE)/sum(df2006_sd$biz_size == 1, na.rm = TRUE)

##bound all survival rates together and reformated dataframe

survivalrate_sd <- cbind(sr3yr2001_sd, sr3yr2001sm_sd, sr3yr2001non_sd, sr5yr2001_sd, sr5yr2001sm_sd, sr5yr2001non_sd, 
                         sr10yr2001_sd, sr10yr2001sm_sd, sr10yr2001non_sd,
                         sr3yr2002_sd, sr3yr2002sm_sd, sr3yr2002non_sd, sr5yr2002_sd, sr5yr2002sm_sd, sr5yr2002non_sd, 
                         sr10yr2002_sd, sr10yr2002sm_sd, sr10yr2002non_sd,
                         sr3yr2003_sd, sr3yr2003sm_sd, sr3yr2003non_sd, sr5yr2003_sd, sr5yr2003sm_sd, sr5yr2003non_sd, 
                         sr10yr2003_sd, sr10yr2003sm_sd, sr10yr2003non_sd,
                         sr3yr2004_sd, sr3yr2004sm_sd, sr3yr2004non_sd, sr5yr2004_sd, sr5yr2004sm_sd, sr5yr2004non_sd, 
                         sr10yr2004_sd, sr10yr2004sm_sd, sr10yr2004non_sd,
                         sr3yr2005_sd, sr3yr2005sm_sd, sr3yr2005non_sd, sr5yr2005_sd, sr5yr2005sm_sd, sr5yr2005non_sd, 
                         sr10yr2005_sd, sr10yr2005sm_sd, sr10yr2005non_sd,
                         sr3yr2006_sd, sr3yr2006sm_sd, sr3yr2006non_sd, sr5yr2006_sd, sr5yr2006sm_sd, sr5yr2006non_sd, 
                         sr10yr2006_sd, sr10yr2006sm_sd, sr10yr2006non_sd)
srnames_sd <- c("3yr_2001_all_sd", "3yr_2001_sm_sd", 
                "3yr_2001_non_sd", "5yr_2001_all_sd", "5yr_2001_sm_sd", "5yr_2001_non_sd", "10yr_2001_all_sd", 
                "10yr_2001_sm_sd", "10yr_2001_non_sd",
                "3yr_2002_all_sd", "3yr_2002_sm_sd", "3yr_2002_non_sd", "5yr_2002_all_sd", "5yr_2002_sm_sd", "5yr_2002_non_sd", 
                "10yr_2002_all_sd", "10yr_2002_sm_sd", "10yr_2002_non_sd",
                "3yr_2003_all_sd", "3yr_2003_sm_sd", "3yr_2003_non_sd", "5yr_2003_all_sd", "5yr_2003_sm_sd", "5yr_2003_non_sd", 
                "10yr_2003_all_sd", "10yr_2003_sm_sd", "10yr_2003_non_sd",
                "3yr_2004_all_sd", "3yr_2004_sm_sd", "3yr_2004_non_sd", "5yr_2004_all_sd", "5yr_2004_sm_sd", "5yr_2004_non_sd", 
                "10yr_2004_all_sd", "10yr_2004_sm_sd", "10yr_2004_non_sd",
                "3yr_2005_all_sd", "3yr_2005_sm_sd", "3yr_2005_non_sd", "5yr_2005_all_sd", "5yr_2005_sm_sd", "5yr_2005_non_sd", 
                "10yr2005_all_sd", "10yr_2005_sm_sd", "10yr_2005_non_sd",
                "3yr_2006_all_sd", "3yr_2006_sm_sd", "3yr_2006_non_sd", "5yr_2006_all_sd", "5yr_2006_sm_sd", "5yr_2006_non_sd", 
                "10yr_2006_all_sd", "10yr_2006_sm_sd", "10yr_2006_non_sd")
colnames(survivalrate_sd) <- srnames_sd

sr_sd <- as.data.frame(survivalrate_sd)

survival.rates_sd <- sr_sd %>% 
  gather("3yr_2001_all_sd":"10yr_2006_non_sd", key = "time_year_type", value = "survivalrate_sd") %>% 
  separate(time_year_type, into = c("time", "year", "type"), sep = "_") %>% 
  arrange(time, year) %>% 
  unite("time_type", time, type, sep = "_") 

survival.rates_sd$time_type[18] <- "10yr_all"
survival.rates_sd$year[18] <- 2005

survival.rates_sd_72 <- survival.rates_sd %>% 
  spread(key = "time_type", value = "survivalrate_sd")

survival.rates_sd_72[, c(1, 5, 7, 6, 8, 10, 9, 2, 4, 3)]

##81####

df2001_sd_81 <- filter(df2001_sd, NAICS2 == 81)
df2002_sd_81 <- filter(df2002_sd, NAICS2 == 81)
df2003_sd_81 <- filter(df2003_sd, NAICS2 == 81)
df2004_sd_81 <- filter(df2004_sd, NAICS2 == 81)
df2005_sd_81 <- filter(df2005_sd, NAICS2 == 81)
df2006_sd_81 <- filter(df2006_sd, NAICS2 == 81)

##3-year
sr3yr2001_sd <- sum(df2001_sd_81$years.survived>=3)/nrow(df2001_sd)
sr3yr2001sm_sd <- sum((df2001_sd_81$years.survived>=3 & df2001_sd_81$biz_size == 0), na.rm = TRUE)/sum(df2001_sd$biz_size == 0, na.rm = TRUE)
sr3yr2001non_sd <- sum((df2001_sd_81$years.survived>=3 & df2001_sd_81$biz_size == 1), na.rm = TRUE)/sum(df2001_sd$biz_size == 1, na.rm = TRUE)

sr3yr2002_sd <- sum(df2002_sd_81$years.survived>=3)/nrow(df2002_sd) 
sr3yr2002sm_sd <- sum((df2002_sd_81$years.survived>=3 & df2002_sd_81$biz_size == 0), na.rm = TRUE)/sum(df2002_sd$biz_size == 0, na.rm = TRUE)
sr3yr2002non_sd <- sum((df2002_sd_81$years.survived>=3 & df2002_sd_81$biz_size == 1), na.rm = TRUE)/sum(df2002_sd$biz_size == 1, na.rm = TRUE)

sr3yr2003_sd <- sum(df2003_sd_81$years.survived>=3)/nrow(df2003_sd)
sr3yr2003sm_sd <- sum((df2003_sd_81$years.survived>=3 & df2003_sd_81$biz_size == 0), na.rm = TRUE)/sum(df2003_sd$biz_size == 0, na.rm = TRUE)
sr3yr2003non_sd <- sum((df2003_sd_81$years.survived>=3 & df2003_sd_81$biz_size == 1), na.rm = TRUE)/sum(df2003_sd$biz_size == 1, na.rm = TRUE)

sr3yr2004_sd <- sum(df2004_sd_81$years.survived>=3)/nrow(df2004_sd) 
sr3yr2004sm_sd <- sum((df2004_sd_81$years.survived>=3 & df2004_sd_81$biz_size == 0), na.rm = TRUE)/sum(df2004_sd$biz_size == 0, na.rm = TRUE)
sr3yr2004non_sd <- sum((df2004_sd_81$years.survived>=3 & df2004_sd_81$biz_size == 1), na.rm = TRUE)/sum(df2004_sd$biz_size == 1, na.rm = TRUE)

sr3yr2005_sd <- sum(df2005_sd_81$years.survived>=3)/nrow(df2005_sd) 
sr3yr2005sm_sd <- sum((df2005_sd_81$years.survived>=3 & df2005_sd_81$biz_size == 0), na.rm = TRUE)/sum(df2005_sd$biz_size == 0, na.rm = TRUE)
sr3yr2005non_sd <- sum((df2005_sd_81$years.survived>=3 & df2005_sd_81$biz_size == 1), na.rm = TRUE)/sum(df2005_sd$biz_size == 1, na.rm = TRUE)

sr3yr2006_sd <- sum(df2006_sd_81$years.survived>=3)/nrow(df2006_sd) 
sr3yr2006sm_sd <- sum((df2006_sd_81$years.survived>=3 & df2006_sd_81$biz_size == 0), na.rm = TRUE)/sum(df2006_sd$biz_size == 0, na.rm = TRUE)
sr3yr2006non_sd <- sum((df2006_sd_81$years.survived>=3 & df2006_sd_81$biz_size == 1), na.rm = TRUE)/sum(df2006_sd$biz_size == 1, na.rm = TRUE)

##5-year
sr5yr2001_sd <- sum(df2001_sd_81$years.survived>=5)/nrow(df2001_sd)
sr5yr2001sm_sd <- sum((df2001_sd_81$years.survived>=5 & df2001_sd_81$biz_size == 0), na.rm = TRUE)/sum(df2001_sd$biz_size == 0, na.rm = TRUE)
sr5yr2001non_sd <- sum((df2001_sd_81$years.survived>=5 & df2001_sd_81$biz_size == 1), na.rm = TRUE)/sum(df2001_sd$biz_size == 1, na.rm = TRUE)

sr5yr2002_sd <- sum(df2002_sd_81$years.survived>=5)/nrow(df2002_sd) 
sr5yr2002sm_sd <- sum((df2002_sd_81$years.survived>=5 & df2002_sd_81$biz_size == 0), na.rm = TRUE)/sum(df2002_sd$biz_size == 0, na.rm = TRUE)
sr5yr2002non_sd <- sum((df2002_sd_81$years.survived>=5 & df2002_sd_81$biz_size == 1), na.rm = TRUE)/sum(df2002_sd$biz_size == 1, na.rm = TRUE)

sr5yr2003_sd <- sum(df2003_sd_81$years.survived>=5)/nrow(df2003_sd)
sr5yr2003sm_sd <- sum((df2003_sd_81$years.survived>=5 & df2003_sd_81$biz_size == 0), na.rm = TRUE)/sum(df2003_sd$biz_size == 0, na.rm = TRUE)
sr5yr2003non_sd <- sum((df2003_sd_81$years.survived>=5 & df2003_sd_81$biz_size == 1), na.rm = TRUE)/sum(df2003_sd$biz_size == 1, na.rm = TRUE)

sr5yr2004_sd <- sum(df2004_sd_81$years.survived>=5)/nrow(df2004_sd) 
sr5yr2004sm_sd <- sum((df2004_sd_81$years.survived>=5 & df2004_sd_81$biz_size == 0), na.rm = TRUE)/sum(df2004_sd$biz_size == 0, na.rm = TRUE)
sr5yr2004non_sd <- sum((df2004_sd_81$years.survived>=5 & df2004_sd_81$biz_size == 1), na.rm = TRUE)/sum(df2004_sd$biz_size == 1, na.rm = TRUE)

sr5yr2005_sd <- sum(df2005_sd_81$years.survived>=5)/nrow(df2005_sd) 
sr5yr2005sm_sd <- sum((df2005_sd_81$years.survived>=5 & df2005_sd_81$biz_size == 0), na.rm = TRUE)/sum(df2005_sd$biz_size == 0, na.rm = TRUE)
sr5yr2005non_sd <- sum((df2005_sd_81$years.survived>=5 & df2005_sd_81$biz_size == 1), na.rm = TRUE)/sum(df2005_sd$biz_size == 1, na.rm = TRUE)

sr5yr2006_sd <- sum(df2006_sd_81$years.survived>=5)/nrow(df2006_sd) 
sr5yr2006sm_sd <- sum((df2006_sd_81$years.survived>=5 & df2006_sd_81$biz_size == 0), na.rm = TRUE)/sum(df2006_sd$biz_size == 0, na.rm = TRUE)
sr5yr2006non_sd <- sum((df2006_sd_81$years.survived>=5 & df2006_sd_81$biz_size == 1), na.rm = TRUE)/sum(df2006_sd$biz_size == 1, na.rm = TRUE)


##10-year
sr10yr2001_sd <- (sum(df2001_sd_81$years.survived>=10))/nrow(df2001_sd) 
sr10yr2001sm_sd <- sum((df2001_sd_81$years.survived>=10 & df2001_sd_81$biz_size == 0), na.rm = TRUE)/sum(df2001_sd$biz_size == 0, na.rm = TRUE)
sr10yr2001non_sd <- sum((df2001_sd_81$years.survived>=10 & df2001_sd_81$biz_size == 1), na.rm = TRUE)/sum(df2002_sd$biz_size == 1, na.rm = TRUE)


sr10yr2002_sd <- sum(df2002_sd_81$years.survived>=10)/nrow(df2002_sd) 
sr10yr2002sm_sd <- sum((df2002_sd_81$years.survived>=10 & df2002_sd_81$biz_size == 0), na.rm = TRUE)/sum(df2002_sd$biz_size == 0, na.rm = TRUE)
sr10yr2002non_sd <- sum((df2002_sd_81$years.survived>=10 & df2002_sd_81$biz_size == 1), na.rm = TRUE)/sum(df2002_sd$biz_size == 1, na.rm = TRUE)

sr10yr2003_sd <- sum(df2003_sd_81$years.survived>=10)/nrow(df2003_sd)
sr10yr2003sm_sd <- sum((df2003_sd_81$years.survived>=10 & df2003_sd_81$biz_size == 0), na.rm = TRUE)/sum(df2003_sd$biz_size == 0, na.rm = TRUE)
sr10yr2003non_sd <- sum((df2003_sd_81$years.survived>=10 & df2003_sd_81$biz_size == 1), na.rm = TRUE)/sum(df2003_sd$biz_size == 1, na.rm = TRUE)

sr10yr2004_sd <- sum(df2004_sd_81$years.survived>=10)/nrow(df2004_sd) 
sr10yr2004sm_sd <- sum((df2004_sd_81$years.survived>=10 & df2004_sd_81$biz_size == 0), na.rm = TRUE)/sum(df2004_sd$biz_size == 0, na.rm = TRUE)
sr10yr2004non_sd <- sum((df2004_sd_81$years.survived>=10 & df2004_sd_81$biz_size == 1), na.rm = TRUE)/sum(df2004_sd$biz_size == 1, na.rm = TRUE)

sr10yr2005_sd <- sum(df2005_sd_81$years.survived>=10)/nrow(df2005_sd) 
sr10yr2005sm_sd <- sum((df2005_sd_81$years.survived>=10 & df2005_sd_81$biz_size == 0), na.rm = TRUE)/sum(df2005_sd$biz_size == 0, na.rm = TRUE)
sr10yr2005non_sd <- sum((df2005_sd_81$years.survived>=10 & df2005_sd_81$biz_size == 1), na.rm = TRUE)/sum(df2005_sd$biz_size == 1, na.rm = TRUE)

sr10yr2006_sd <- sum(df2006_sd_81$years.survived>=10)/nrow(df2006_sd) 
sr10yr2006sm_sd <- sum((df2006_sd_81$years.survived>=10 & df2006_sd_81$biz_size == 0), na.rm = TRUE)/sum(df2006_sd$biz_size == 0, na.rm = TRUE)
sr10yr2006non_sd <- sum((df2006_sd_81$years.survived>=10 & df2006_sd_81$biz_size == 1), na.rm = TRUE)/sum(df2006_sd$biz_size == 1, na.rm = TRUE)

##bound all survival rates together and reformated dataframe

survivalrate_sd <- cbind(sr3yr2001_sd, sr3yr2001sm_sd, sr3yr2001non_sd, sr5yr2001_sd, sr5yr2001sm_sd, sr5yr2001non_sd, 
                         sr10yr2001_sd, sr10yr2001sm_sd, sr10yr2001non_sd,
                         sr3yr2002_sd, sr3yr2002sm_sd, sr3yr2002non_sd, sr5yr2002_sd, sr5yr2002sm_sd, sr5yr2002non_sd, 
                         sr10yr2002_sd, sr10yr2002sm_sd, sr10yr2002non_sd,
                         sr3yr2003_sd, sr3yr2003sm_sd, sr3yr2003non_sd, sr5yr2003_sd, sr5yr2003sm_sd, sr5yr2003non_sd, 
                         sr10yr2003_sd, sr10yr2003sm_sd, sr10yr2003non_sd,
                         sr3yr2004_sd, sr3yr2004sm_sd, sr3yr2004non_sd, sr5yr2004_sd, sr5yr2004sm_sd, sr5yr2004non_sd, 
                         sr10yr2004_sd, sr10yr2004sm_sd, sr10yr2004non_sd,
                         sr3yr2005_sd, sr3yr2005sm_sd, sr3yr2005non_sd, sr5yr2005_sd, sr5yr2005sm_sd, sr5yr2005non_sd, 
                         sr10yr2005_sd, sr10yr2005sm_sd, sr10yr2005non_sd,
                         sr3yr2006_sd, sr3yr2006sm_sd, sr3yr2006non_sd, sr5yr2006_sd, sr5yr2006sm_sd, sr5yr2006non_sd, 
                         sr10yr2006_sd, sr10yr2006sm_sd, sr10yr2006non_sd)
srnames_sd <- c("3yr_2001_all_sd", "3yr_2001_sm_sd", 
                "3yr_2001_non_sd", "5yr_2001_all_sd", "5yr_2001_sm_sd", "5yr_2001_non_sd", "10yr_2001_all_sd", 
                "10yr_2001_sm_sd", "10yr_2001_non_sd",
                "3yr_2002_all_sd", "3yr_2002_sm_sd", "3yr_2002_non_sd", "5yr_2002_all_sd", "5yr_2002_sm_sd", "5yr_2002_non_sd", 
                "10yr_2002_all_sd", "10yr_2002_sm_sd", "10yr_2002_non_sd",
                "3yr_2003_all_sd", "3yr_2003_sm_sd", "3yr_2003_non_sd", "5yr_2003_all_sd", "5yr_2003_sm_sd", "5yr_2003_non_sd", 
                "10yr_2003_all_sd", "10yr_2003_sm_sd", "10yr_2003_non_sd",
                "3yr_2004_all_sd", "3yr_2004_sm_sd", "3yr_2004_non_sd", "5yr_2004_all_sd", "5yr_2004_sm_sd", "5yr_2004_non_sd", 
                "10yr_2004_all_sd", "10yr_2004_sm_sd", "10yr_2004_non_sd",
                "3yr_2005_all_sd", "3yr_2005_sm_sd", "3yr_2005_non_sd", "5yr_2005_all_sd", "5yr_2005_sm_sd", "5yr_2005_non_sd", 
                "10yr2005_all_sd", "10yr_2005_sm_sd", "10yr_2005_non_sd",
                "3yr_2006_all_sd", "3yr_2006_sm_sd", "3yr_2006_non_sd", "5yr_2006_all_sd", "5yr_2006_sm_sd", "5yr_2006_non_sd", 
                "10yr_2006_all_sd", "10yr_2006_sm_sd", "10yr_2006_non_sd")
colnames(survivalrate_sd) <- srnames_sd

sr_sd <- as.data.frame(survivalrate_sd)

survival.rates_sd <- sr_sd %>% 
  gather("3yr_2001_all_sd":"10yr_2006_non_sd", key = "time_year_type", value = "survivalrate_sd") %>% 
  separate(time_year_type, into = c("time", "year", "type"), sep = "_") %>% 
  arrange(time, year) %>% 
  unite("time_type", time, type, sep = "_") 

survival.rates_sd$time_type[18] <- "10yr_all"
survival.rates_sd$year[18] <- 2005

survival.rates_sd_81 <- survival.rates_sd %>% 
  spread(key = "time_type", value = "survivalrate_sd")

survival.rates_sd_81[, c(1, 5, 7, 6, 8, 10, 9, 2, 4, 3)]

##92####

df2001_sd_92 <- filter(df2001_sd, NAICS2 == 92)
df2002_sd_92 <- filter(df2002_sd, NAICS2 == 92)
df2003_sd_92 <- filter(df2003_sd, NAICS2 == 92)
df2004_sd_92 <- filter(df2004_sd, NAICS2 == 92)
df2005_sd_92 <- filter(df2005_sd, NAICS2 == 92)
df2006_sd_92 <- filter(df2006_sd, NAICS2 == 92)

##3-year
sr3yr2001_sd <- sum(df2001_sd_92$years.survived>=3)/nrow(df2001_sd)
sr3yr2001sm_sd <- sum((df2001_sd_92$years.survived>=3 & df2001_sd_92$biz_size == 0), na.rm = TRUE)/sum(df2001_sd$biz_size == 0, na.rm = TRUE)
sr3yr2001non_sd <- sum((df2001_sd_92$years.survived>=3 & df2001_sd_92$biz_size == 1), na.rm = TRUE)/sum(df2001_sd$biz_size == 1, na.rm = TRUE)

sr3yr2002_sd <- sum(df2002_sd_92$years.survived>=3)/nrow(df2002_sd) 
sr3yr2002sm_sd <- sum((df2002_sd_92$years.survived>=3 & df2002_sd_92$biz_size == 0), na.rm = TRUE)/sum(df2002_sd$biz_size == 0, na.rm = TRUE)
sr3yr2002non_sd <- sum((df2002_sd_92$years.survived>=3 & df2002_sd_92$biz_size == 1), na.rm = TRUE)/sum(df2002_sd$biz_size == 1, na.rm = TRUE)

sr3yr2003_sd <- sum(df2003_sd_92$years.survived>=3)/nrow(df2003_sd)
sr3yr2003sm_sd <- sum((df2003_sd_92$years.survived>=3 & df2003_sd_92$biz_size == 0), na.rm = TRUE)/sum(df2003_sd$biz_size == 0, na.rm = TRUE)
sr3yr2003non_sd <- sum((df2003_sd_92$years.survived>=3 & df2003_sd_92$biz_size == 1), na.rm = TRUE)/sum(df2003_sd$biz_size == 1, na.rm = TRUE)

sr3yr2004_sd <- sum(df2004_sd_92$years.survived>=3)/nrow(df2004_sd) 
sr3yr2004sm_sd <- sum((df2004_sd_92$years.survived>=3 & df2004_sd_92$biz_size == 0), na.rm = TRUE)/sum(df2004_sd$biz_size == 0, na.rm = TRUE)
sr3yr2004non_sd <- sum((df2004_sd_92$years.survived>=3 & df2004_sd_92$biz_size == 1), na.rm = TRUE)/sum(df2004_sd$biz_size == 1, na.rm = TRUE)

sr3yr2005_sd <- sum(df2005_sd_92$years.survived>=3)/nrow(df2005_sd) 
sr3yr2005sm_sd <- sum((df2005_sd_92$years.survived>=3 & df2005_sd_92$biz_size == 0), na.rm = TRUE)/sum(df2005_sd$biz_size == 0, na.rm = TRUE)
sr3yr2005non_sd <- sum((df2005_sd_92$years.survived>=3 & df2005_sd_92$biz_size == 1), na.rm = TRUE)/sum(df2005_sd$biz_size == 1, na.rm = TRUE)

sr3yr2006_sd <- sum(df2006_sd_92$years.survived>=3)/nrow(df2006_sd) 
sr3yr2006sm_sd <- sum((df2006_sd_92$years.survived>=3 & df2006_sd_92$biz_size == 0), na.rm = TRUE)/sum(df2006_sd$biz_size == 0, na.rm = TRUE)
sr3yr2006non_sd <- sum((df2006_sd_92$years.survived>=3 & df2006_sd_92$biz_size == 1), na.rm = TRUE)/sum(df2006_sd$biz_size == 1, na.rm = TRUE)

##5-year
sr5yr2001_sd <- sum(df2001_sd_92$years.survived>=5)/nrow(df2001_sd)
sr5yr2001sm_sd <- sum((df2001_sd_92$years.survived>=5 & df2001_sd_92$biz_size == 0), na.rm = TRUE)/sum(df2001_sd$biz_size == 0, na.rm = TRUE)
sr5yr2001non_sd <- sum((df2001_sd_92$years.survived>=5 & df2001_sd_92$biz_size == 1), na.rm = TRUE)/sum(df2001_sd$biz_size == 1, na.rm = TRUE)

sr5yr2002_sd <- sum(df2002_sd_92$years.survived>=5)/nrow(df2002_sd) 
sr5yr2002sm_sd <- sum((df2002_sd_92$years.survived>=5 & df2002_sd_92$biz_size == 0), na.rm = TRUE)/sum(df2002_sd$biz_size == 0, na.rm = TRUE)
sr5yr2002non_sd <- sum((df2002_sd_92$years.survived>=5 & df2002_sd_92$biz_size == 1), na.rm = TRUE)/sum(df2002_sd$biz_size == 1, na.rm = TRUE)

sr5yr2003_sd <- sum(df2003_sd_92$years.survived>=5)/nrow(df2003_sd)
sr5yr2003sm_sd <- sum((df2003_sd_92$years.survived>=5 & df2003_sd_92$biz_size == 0), na.rm = TRUE)/sum(df2003_sd$biz_size == 0, na.rm = TRUE)
sr5yr2003non_sd <- sum((df2003_sd_92$years.survived>=5 & df2003_sd_92$biz_size == 1), na.rm = TRUE)/sum(df2003_sd$biz_size == 1, na.rm = TRUE)

sr5yr2004_sd <- sum(df2004_sd_92$years.survived>=5)/nrow(df2004_sd) 
sr5yr2004sm_sd <- sum((df2004_sd_92$years.survived>=5 & df2004_sd_92$biz_size == 0), na.rm = TRUE)/sum(df2004_sd$biz_size == 0, na.rm = TRUE)
sr5yr2004non_sd <- sum((df2004_sd_92$years.survived>=5 & df2004_sd_92$biz_size == 1), na.rm = TRUE)/sum(df2004_sd$biz_size == 1, na.rm = TRUE)

sr5yr2005_sd <- sum(df2005_sd_92$years.survived>=5)/nrow(df2005_sd) 
sr5yr2005sm_sd <- sum((df2005_sd_92$years.survived>=5 & df2005_sd_92$biz_size == 0), na.rm = TRUE)/sum(df2005_sd$biz_size == 0, na.rm = TRUE)
sr5yr2005non_sd <- sum((df2005_sd_92$years.survived>=5 & df2005_sd_92$biz_size == 1), na.rm = TRUE)/sum(df2005_sd$biz_size == 1, na.rm = TRUE)

sr5yr2006_sd <- sum(df2006_sd_92$years.survived>=5)/nrow(df2006_sd) 
sr5yr2006sm_sd <- sum((df2006_sd_92$years.survived>=5 & df2006_sd_92$biz_size == 0), na.rm = TRUE)/sum(df2006_sd$biz_size == 0, na.rm = TRUE)
sr5yr2006non_sd <- sum((df2006_sd_92$years.survived>=5 & df2006_sd_92$biz_size == 1), na.rm = TRUE)/sum(df2006_sd$biz_size == 1, na.rm = TRUE)


##10-year
sr10yr2001_sd <- (sum(df2001_sd_92$years.survived>=10))/nrow(df2001_sd) 
sr10yr2001sm_sd <- sum((df2001_sd_92$years.survived>=10 & df2001_sd_92$biz_size == 0), na.rm = TRUE)/sum(df2001_sd$biz_size == 0, na.rm = TRUE)
sr10yr2001non_sd <- sum((df2001_sd_92$years.survived>=10 & df2001_sd_92$biz_size == 1), na.rm = TRUE)/sum(df2002_sd$biz_size == 1, na.rm = TRUE)


sr10yr2002_sd <- sum(df2002_sd_92$years.survived>=10)/nrow(df2002_sd) 
sr10yr2002sm_sd <- sum((df2002_sd_92$years.survived>=10 & df2002_sd_92$biz_size == 0), na.rm = TRUE)/sum(df2002_sd$biz_size == 0, na.rm = TRUE)
sr10yr2002non_sd <- sum((df2002_sd_92$years.survived>=10 & df2002_sd_92$biz_size == 1), na.rm = TRUE)/sum(df2002_sd$biz_size == 1, na.rm = TRUE)

sr10yr2003_sd <- sum(df2003_sd_92$years.survived>=10)/nrow(df2003_sd)
sr10yr2003sm_sd <- sum((df2003_sd_92$years.survived>=10 & df2003_sd_92$biz_size == 0), na.rm = TRUE)/sum(df2003_sd$biz_size == 0, na.rm = TRUE)
sr10yr2003non_sd <- sum((df2003_sd_92$years.survived>=10 & df2003_sd_92$biz_size == 1), na.rm = TRUE)/sum(df2003_sd$biz_size == 1, na.rm = TRUE)

sr10yr2004_sd <- sum(df2004_sd$years.survived>=10)/nrow(df2004_sd) 
sr10yr2004sm_sd <- sum((df2004_sd_92$years.survived>=10 & df2004_sd_92$biz_size == 0), na.rm = TRUE)/sum(df2004_sd$biz_size == 0, na.rm = TRUE)
sr10yr2004non_sd <- sum((df2004_sd_92$years.survived>=10 & df2004_sd_92$biz_size == 1), na.rm = TRUE)/sum(df2004_sd$biz_size == 1, na.rm = TRUE)

sr10yr2005_sd <- sum(df2005_sd_92$years.survived>=10)/nrow(df2005_sd) 
sr10yr2005sm_sd <- sum((df2005_sd_92$years.survived>=10 & df2005_sd_92$biz_size == 0), na.rm = TRUE)/sum(df2005_sd$biz_size == 0, na.rm = TRUE)
sr10yr2005non_sd <- sum((df2005_sd_92$years.survived>=10 & df2005_sd_92$biz_size == 1), na.rm = TRUE)/sum(df2005_sd$biz_size == 1, na.rm = TRUE)

sr10yr2006_sd <- sum(df2006_sd$years.survived>=10)/nrow(df2006_sd) 
sr10yr2006sm_sd <- sum((df2006_sd_92$years.survived>=10 & df2006_sd_92$biz_size == 0), na.rm = TRUE)/sum(df2006_sd$biz_size == 0, na.rm = TRUE)
sr10yr2006non_sd <- sum((df2006_sd_92$years.survived>=10 & df2006_sd_92$biz_size == 1), na.rm = TRUE)/sum(df2006_sd$biz_size == 1, na.rm = TRUE)

##bound all survival rates together and reformated dataframe

survivalrate_sd <- cbind(sr3yr2001_sd, sr3yr2001sm_sd, sr3yr2001non_sd, sr5yr2001_sd, sr5yr2001sm_sd, sr5yr2001non_sd, 
                         sr10yr2001_sd, sr10yr2001sm_sd, sr10yr2001non_sd,
                         sr3yr2002_sd, sr3yr2002sm_sd, sr3yr2002non_sd, sr5yr2002_sd, sr5yr2002sm_sd, sr5yr2002non_sd, 
                         sr10yr2002_sd, sr10yr2002sm_sd, sr10yr2002non_sd,
                         sr3yr2003_sd, sr3yr2003sm_sd, sr3yr2003non_sd, sr5yr2003_sd, sr5yr2003sm_sd, sr5yr2003non_sd, 
                         sr10yr2003_sd, sr10yr2003sm_sd, sr10yr2003non_sd,
                         sr3yr2004_sd, sr3yr2004sm_sd, sr3yr2004non_sd, sr5yr2004_sd, sr5yr2004sm_sd, sr5yr2004non_sd, 
                         sr10yr2004_sd, sr10yr2004sm_sd, sr10yr2004non_sd,
                         sr3yr2005_sd, sr3yr2005sm_sd, sr3yr2005non_sd, sr5yr2005_sd, sr5yr2005sm_sd, sr5yr2005non_sd, 
                         sr10yr2005_sd, sr10yr2005sm_sd, sr10yr2005non_sd,
                         sr3yr2006_sd, sr3yr2006sm_sd, sr3yr2006non_sd, sr5yr2006_sd, sr5yr2006sm_sd, sr5yr2006non_sd, 
                         sr10yr2006_sd, sr10yr2006sm_sd, sr10yr2006non_sd)
srnames_sd <- c("3yr_2001_all_sd", "3yr_2001_sm_sd", 
                "3yr_2001_non_sd", "5yr_2001_all_sd", "5yr_2001_sm_sd", "5yr_2001_non_sd", "10yr_2001_all_sd", 
                "10yr_2001_sm_sd", "10yr_2001_non_sd",
                "3yr_2002_all_sd", "3yr_2002_sm_sd", "3yr_2002_non_sd", "5yr_2002_all_sd", "5yr_2002_sm_sd", "5yr_2002_non_sd", 
                "10yr_2002_all_sd", "10yr_2002_sm_sd", "10yr_2002_non_sd",
                "3yr_2003_all_sd", "3yr_2003_sm_sd", "3yr_2003_non_sd", "5yr_2003_all_sd", "5yr_2003_sm_sd", "5yr_2003_non_sd", 
                "10yr_2003_all_sd", "10yr_2003_sm_sd", "10yr_2003_non_sd",
                "3yr_2004_all_sd", "3yr_2004_sm_sd", "3yr_2004_non_sd", "5yr_2004_all_sd", "5yr_2004_sm_sd", "5yr_2004_non_sd", 
                "10yr_2004_all_sd", "10yr_2004_sm_sd", "10yr_2004_non_sd",
                "3yr_2005_all_sd", "3yr_2005_sm_sd", "3yr_2005_non_sd", "5yr_2005_all_sd", "5yr_2005_sm_sd", "5yr_2005_non_sd", 
                "10yr2005_all_sd", "10yr_2005_sm_sd", "10yr_2005_non_sd",
                "3yr_2006_all_sd", "3yr_2006_sm_sd", "3yr_2006_non_sd", "5yr_2006_all_sd", "5yr_2006_sm_sd", "5yr_2006_non_sd", 
                "10yr_2006_all_sd", "10yr_2006_sm_sd", "10yr_2006_non_sd")
colnames(survivalrate_sd) <- srnames_sd

sr_sd <- as.data.frame(survivalrate_sd)

survival.rates_sd <- sr_sd %>% 
  gather("3yr_2001_all_sd":"10yr_2006_non_sd", key = "time_year_type", value = "survivalrate_sd") %>% 
  separate(time_year_type, into = c("time", "year", "type"), sep = "_") %>% 
  arrange(time, year) %>% 
  unite("time_type", time, type, sep = "_") 

survival.rates_sd$time_type[18] <- "10yr_all"
survival.rates_sd$year[18] <- 2005

survival.rates_sd_92 <- survival.rates_sd %>% 
  spread(key = "time_type", value = "survivalrate_sd")

survival.rates_sd_92[, c(1, 5, 7, 6, 8, 10, 9, 2, 4, 3)]

##other (14, 15, 55, 99)####

df2001_sd_other <- filter(df2001_sd, NAICS2 == 14 | NAICS2 == 15 | NAICS2 == 55 | NAICS2 == 99)
df2002_sd_other <- filter(df2002_sd, NAICS2 == 14 | NAICS2 == 15 | NAICS2 == 55 | NAICS2 == 99)
df2003_sd_other <- filter(df2003_sd, NAICS2 == 14 | NAICS2 == 15 | NAICS2 == 55 | NAICS2 == 99)
df2004_sd_other <- filter(df2004_sd, NAICS2 == 14 | NAICS2 == 15 | NAICS2 == 55 | NAICS2 == 99)
df2005_sd_other <- filter(df2005_sd, NAICS2 == 14 | NAICS2 == 15 | NAICS2 == 55 | NAICS2 == 99)
df2006_sd_other <- filter(df2006_sd, NAICS2 == 14 | NAICS2 == 15 | NAICS2 == 55 | NAICS2 == 99)

##3-year
sr3yr2001_sd <- sum(df2001_sd_other$years.survived>=3)/nrow(df2001_sd)
sr3yr2001sm_sd <- sum((df2001_sd_other$years.survived>=3 & df2001_sd_other$biz_size == 0), na.rm = TRUE)/sum(df2001_sd$biz_size == 0, na.rm = TRUE)
sr3yr2001non_sd <- sum((df2001_sd_other$years.survived>=3 & df2001_sd_other$biz_size == 1), na.rm = TRUE)/sum(df2001_sd$biz_size == 1, na.rm = TRUE)

sr3yr2002_sd <- sum(df2002_sd_other$years.survived>=3)/nrow(df2002_sd) 
sr3yr2002sm_sd <- sum((df2002_sd_other$years.survived>=3 & df2002_sd_other$biz_size == 0), na.rm = TRUE)/sum(df2002_sd$biz_size == 0, na.rm = TRUE)
sr3yr2002non_sd <- sum((df2002_sd_other$years.survived>=3 & df2002_sd_other$biz_size == 1), na.rm = TRUE)/sum(df2002_sd$biz_size == 1, na.rm = TRUE)

sr3yr2003_sd <- sum(df2003_sd_other$years.survived>=3)/nrow(df2003_sd)
sr3yr2003sm_sd <- sum((df2003_sd_other$years.survived>=3 & df2003_sd_other$biz_size == 0), na.rm = TRUE)/sum(df2003_sd$biz_size == 0, na.rm = TRUE)
sr3yr2003non_sd <- sum((df2003_sd_other$years.survived>=3 & df2003_sd_other$biz_size == 1), na.rm = TRUE)/sum(df2003_sd$biz_size == 1, na.rm = TRUE)

sr3yr2004_sd <- sum(df2004_sd_other$years.survived>=3)/nrow(df2004_sd) 
sr3yr2004sm_sd <- sum((df2004_sd_other$years.survived>=3 & df2004_sd_other$biz_size == 0), na.rm = TRUE)/sum(df2004_sd$biz_size == 0, na.rm = TRUE)
sr3yr2004non_sd <- sum((df2004_sd_other$years.survived>=3 & df2004_sd_other$biz_size == 1), na.rm = TRUE)/sum(df2004_sd$biz_size == 1, na.rm = TRUE)

sr3yr2005_sd <- sum(df2005_sd_other$years.survived>=3)/nrow(df2005_sd) 
sr3yr2005sm_sd <- sum((df2005_sd_other$years.survived>=3 & df2005_sd_other$biz_size == 0), na.rm = TRUE)/sum(df2005_sd$biz_size == 0, na.rm = TRUE)
sr3yr2005non_sd <- sum((df2005_sd_other$years.survived>=3 & df2005_sd_other$biz_size == 1), na.rm = TRUE)/sum(df2005_sd$biz_size == 1, na.rm = TRUE)

sr3yr2006_sd <- sum(df2006_sd_other$years.survived>=3)/nrow(df2006_sd) 
sr3yr2006sm_sd <- sum((df2006_sd_other$years.survived>=3 & df2006_sd_other$biz_size == 0), na.rm = TRUE)/sum(df2006_sd$biz_size == 0, na.rm = TRUE)
sr3yr2006non_sd <- sum((df2006_sd_other$years.survived>=3 & df2006_sd_other$biz_size == 1), na.rm = TRUE)/sum(df2006_sd$biz_size == 1, na.rm = TRUE)

##5-year
sr5yr2001_sd <- sum(df2001_sd_other$years.survived>=5)/nrow(df2001_sd)
sr5yr2001sm_sd <- sum((df2001_sd_other$years.survived>=5 & df2001_sd_other$biz_size == 0), na.rm = TRUE)/sum(df2001_sd$biz_size == 0, na.rm = TRUE)
sr5yr2001non_sd <- sum((df2001_sd_other$years.survived>=5 & df2001_sd_other$biz_size == 1), na.rm = TRUE)/sum(df2001_sd$biz_size == 1, na.rm = TRUE)

sr5yr2002_sd <- sum(df2002_sd_other$years.survived>=5)/nrow(df2002_sd) 
sr5yr2002sm_sd <- sum((df2002_sd_other$years.survived>=5 & df2002_sd_other$biz_size == 0), na.rm = TRUE)/sum(df2002_sd$biz_size == 0, na.rm = TRUE)
sr5yr2002non_sd <- sum((df2002_sd_other$years.survived>=5 & df2002_sd_other$biz_size == 1), na.rm = TRUE)/sum(df2002_sd$biz_size == 1, na.rm = TRUE)

sr5yr2003_sd <- sum(df2003_sd_other$years.survived>=5)/nrow(df2003_sd)
sr5yr2003sm_sd <- sum((df2003_sd_other$years.survived>=5 & df2003_sd_other$biz_size == 0), na.rm = TRUE)/sum(df2003_sd$biz_size == 0, na.rm = TRUE)
sr5yr2003non_sd <- sum((df2003_sd_other$years.survived>=5 & df2003_sd_other$biz_size == 1), na.rm = TRUE)/sum(df2003_sd$biz_size == 1, na.rm = TRUE)

sr5yr2004_sd <- sum(df2004_sd_other$years.survived>=5)/nrow(df2004_sd) 
sr5yr2004sm_sd <- sum((df2004_sd_other$years.survived>=5 & df2004_sd_other$biz_size == 0), na.rm = TRUE)/sum(df2004_sd$biz_size == 0, na.rm = TRUE)
sr5yr2004non_sd <- sum((df2004_sd_other$years.survived>=5 & df2004_sd_other$biz_size == 1), na.rm = TRUE)/sum(df2004_sd$biz_size == 1, na.rm = TRUE)

sr5yr2005_sd <- sum(df2005_sd_other$years.survived>=5)/nrow(df2005_sd) 
sr5yr2005sm_sd <- sum((df2005_sd_other$years.survived>=5 & df2005_sd_other$biz_size == 0), na.rm = TRUE)/sum(df2005_sd$biz_size == 0, na.rm = TRUE)
sr5yr2005non_sd <- sum((df2005_sd_other$years.survived>=5 & df2005_sd_other$biz_size == 1), na.rm = TRUE)/sum(df2005_sd$biz_size == 1, na.rm = TRUE)

sr5yr2006_sd <- sum(df2006_sd_other$years.survived>=5)/nrow(df2006_sd) 
sr5yr2006sm_sd <- sum((df2006_sd_other$years.survived>=5 & df2006_sd_other$biz_size == 0), na.rm = TRUE)/sum(df2006_sd$biz_size == 0, na.rm = TRUE)
sr5yr2006non_sd <- sum((df2006_sd_other$years.survived>=5 & df2006_sd_other$biz_size == 1), na.rm = TRUE)/sum(df2006_sd$biz_size == 1, na.rm = TRUE)


##10-year
sr10yr2001_sd <- (sum(df2001_sd_other$years.survived>=10))/nrow(df2001_sd) 
sr10yr2001sm_sd <- sum((df2001_sd_other$years.survived>=10 & df2001_sd_other$biz_size == 0), na.rm = TRUE)/sum(df2001_sd$biz_size == 0, na.rm = TRUE)
sr10yr2001non_sd <- sum((df2001_sd_other$years.survived>=10 & df2001_sd_other$biz_size == 1), na.rm = TRUE)/sum(df2002_sd$biz_size == 1, na.rm = TRUE)


sr10yr2002_sd <- sum(df2002_sd_other$years.survived>=10)/nrow(df2002_sd) 
sr10yr2002sm_sd <- sum((df2002_sd_other$years.survived>=10 & df2002_sd_other$biz_size == 0), na.rm = TRUE)/sum(df2002_sd$biz_size == 0, na.rm = TRUE)
sr10yr2002non_sd <- sum((df2002_sd_other$years.survived>=10 & df2002_sd_other$biz_size == 1), na.rm = TRUE)/sum(df2002_sd$biz_size == 1, na.rm = TRUE)

sr10yr2003_sd <- sum(df2003_sd_other$years.survived>=10)/nrow(df2003_sd)
sr10yr2003sm_sd <- sum((df2003_sd_other$years.survived>=10 & df2003_sd_other$biz_size == 0), na.rm = TRUE)/sum(df2003_sd$biz_size == 0, na.rm = TRUE)
sr10yr2003non_sd <- sum((df2003_sd_other$years.survived>=10 & df2003_sd_other$biz_size == 1), na.rm = TRUE)/sum(df2003_sd$biz_size == 1, na.rm = TRUE)

sr10yr2004_sd <- sum(df2004_sd_other$years.survived>=10)/nrow(df2004_sd) 
sr10yr2004sm_sd <- sum((df2004_sd_other$years.survived>=10 & df2004_sd_other$biz_size == 0), na.rm = TRUE)/sum(df2004_sd$biz_size == 0, na.rm = TRUE)
sr10yr2004non_sd <- sum((df2004_sd_other$years.survived>=10 & df2004_sd_other$biz_size == 1), na.rm = TRUE)/sum(df2004_sd$biz_size == 1, na.rm = TRUE)

sr10yr2005_sd <- sum(df2005_sd_other$years.survived>=10)/nrow(df2005_sd) 
sr10yr2005sm_sd <- sum((df2005_sd_other$years.survived>=10 & df2005_sd_other$biz_size == 0), na.rm = TRUE)/sum(df2005_sd$biz_size == 0, na.rm = TRUE)
sr10yr2005non_sd <- sum((df2005_sd_other$years.survived>=10 & df2005_sd_other$biz_size == 1), na.rm = TRUE)/sum(df2005_sd$biz_size == 1, na.rm = TRUE)

sr10yr2006_sd <- sum(df2006_sd_other$years.survived>=10)/nrow(df2006_sd) 
sr10yr2006sm_sd <- sum((df2006_sd_other$years.survived>=10 & df2006_sd_other$biz_size == 0), na.rm = TRUE)/sum(df2006_sd$biz_size == 0, na.rm = TRUE)
sr10yr2006non_sd <- sum((df2006_sd_other$years.survived>=10 & df2006_sd_other$biz_size == 1), na.rm = TRUE)/sum(df2006_sd$biz_size == 1, na.rm = TRUE)

##bound all survival rates together and reformated dataframe

survivalrate_sd <- cbind(sr3yr2001_sd, sr3yr2001sm_sd, sr3yr2001non_sd, sr5yr2001_sd, sr5yr2001sm_sd, sr5yr2001non_sd, 
                         sr10yr2001_sd, sr10yr2001sm_sd, sr10yr2001non_sd,
                         sr3yr2002_sd, sr3yr2002sm_sd, sr3yr2002non_sd, sr5yr2002_sd, sr5yr2002sm_sd, sr5yr2002non_sd, 
                         sr10yr2002_sd, sr10yr2002sm_sd, sr10yr2002non_sd,
                         sr3yr2003_sd, sr3yr2003sm_sd, sr3yr2003non_sd, sr5yr2003_sd, sr5yr2003sm_sd, sr5yr2003non_sd, 
                         sr10yr2003_sd, sr10yr2003sm_sd, sr10yr2003non_sd,
                         sr3yr2004_sd, sr3yr2004sm_sd, sr3yr2004non_sd, sr5yr2004_sd, sr5yr2004sm_sd, sr5yr2004non_sd, 
                         sr10yr2004_sd, sr10yr2004sm_sd, sr10yr2004non_sd,
                         sr3yr2005_sd, sr3yr2005sm_sd, sr3yr2005non_sd, sr5yr2005_sd, sr5yr2005sm_sd, sr5yr2005non_sd, 
                         sr10yr2005_sd, sr10yr2005sm_sd, sr10yr2005non_sd,
                         sr3yr2006_sd, sr3yr2006sm_sd, sr3yr2006non_sd, sr5yr2006_sd, sr5yr2006sm_sd, sr5yr2006non_sd, 
                         sr10yr2006_sd, sr10yr2006sm_sd, sr10yr2006non_sd)
srnames_sd <- c("3yr_2001_all_sd", "3yr_2001_sm_sd", 
                "3yr_2001_non_sd", "5yr_2001_all_sd", "5yr_2001_sm_sd", "5yr_2001_non_sd", "10yr_2001_all_sd", 
                "10yr_2001_sm_sd", "10yr_2001_non_sd",
                "3yr_2002_all_sd", "3yr_2002_sm_sd", "3yr_2002_non_sd", "5yr_2002_all_sd", "5yr_2002_sm_sd", "5yr_2002_non_sd", 
                "10yr_2002_all_sd", "10yr_2002_sm_sd", "10yr_2002_non_sd",
                "3yr_2003_all_sd", "3yr_2003_sm_sd", "3yr_2003_non_sd", "5yr_2003_all_sd", "5yr_2003_sm_sd", "5yr_2003_non_sd", 
                "10yr_2003_all_sd", "10yr_2003_sm_sd", "10yr_2003_non_sd",
                "3yr_2004_all_sd", "3yr_2004_sm_sd", "3yr_2004_non_sd", "5yr_2004_all_sd", "5yr_2004_sm_sd", "5yr_2004_non_sd", 
                "10yr_2004_all_sd", "10yr_2004_sm_sd", "10yr_2004_non_sd",
                "3yr_2005_all_sd", "3yr_2005_sm_sd", "3yr_2005_non_sd", "5yr_2005_all_sd", "5yr_2005_sm_sd", "5yr_2005_non_sd", 
                "10yr2005_all_sd", "10yr_2005_sm_sd", "10yr_2005_non_sd",
                "3yr_2006_all_sd", "3yr_2006_sm_sd", "3yr_2006_non_sd", "5yr_2006_all_sd", "5yr_2006_sm_sd", "5yr_2006_non_sd", 
                "10yr_2006_all_sd", "10yr_2006_sm_sd", "10yr_2006_non_sd")
colnames(survivalrate_sd) <- srnames_sd

sr_sd <- as.data.frame(survivalrate_sd)

survival.rates_sd <- sr_sd %>% 
  gather("3yr_2001_all_sd":"10yr_2006_non_sd", key = "time_year_type", value = "survivalrate_sd") %>% 
  separate(time_year_type, into = c("time", "year", "type"), sep = "_") %>% 
  arrange(time, year) %>% 
  unite("time_type", time, type, sep = "_") 

survival.rates_sd$time_type[18] <- "10yr_all"
survival.rates_sd$year[18] <- 2005

survival.rates_sd_other <- survival.rates_sd %>% 
  spread(key = "time_type", value = "survivalrate_sd")

survival.rates_sd_other[, c(1, 5, 7, 6, 8, 10, 9, 2, 4, 3)]




write.csv(survival.rates_sd_11, "NAICS 11 survival rates.csv")
write.csv(survival.rates_sd_21, "NAICS 21 survival rates.csv")
write.csv(survival.rates_sd_22, "NAICS 22 survival rates.csv")
write.csv(survival.rates_sd_23, "NAICS 23 survival rates.csv")
write.csv(survival.rates_sd_31, "NAICS 31 survival rates.csv")
write.csv(survival.rates_sd_42, "NAICS 42 survival rates.csv")
write.csv(survival.rates_sd_44, "NAICS 44 survival rates.csv")
write.csv(survival.rates_sd_48, "NAICS 48 survival rates.csv")
write.csv(survival.rates_sd_51, "NAICS 51 survival rates.csv")
write.csv(survival.rates_sd_52, "NAICS 52 survival rates.csv")
write.csv(survival.rates_sd_53, "NAICS 53 survival rates.csv")
write.csv(survival.rates_sd_54, "NAICS 54 survival rates.csv")
write.csv(survival.rates_sd_56, "NAICS 56 survival rates.csv")
write.csv(survival.rates_sd_61, "NAICS 61 survival rates.csv")
write.csv(survival.rates_sd_62, "NAICS 62 survival rates.csv")
write.csv(survival.rates_sd_71, "NAICS 71 survival rates.csv")
write.csv(survival.rates_sd_72, "NAICS 72 survival rates.csv")
write.csv(survival.rates_sd_81, "NAICS 81 survival rates.csv")
write.csv(survival.rates_sd_92, "NAICS 92 survival rates.csv")
write.csv(survival.rates_sd_other, "NAICS 14,15,55,99 survival rates.csv")
write.csv(NAICS_div, "NAICS count reg2001-2006.csv")
