library(tidyverse)
library(data.table)
library(lubridate)
library(plyr)

###determine in biz-size column if change (for each duns)

panel_data <- read_csv("Panel data reg 2001-2011 - no parent filter.csv")
full_FPDS <- read_csv("SAM Data merged with FPDS, exp2000-2019.csv")

### Small start firms

  

gradfirm<- panel_data %>% 
  filter(ten.year == 1) %>% ###all who survived 10 years
  filter(biz_size == 0) %>% ### all who started small
  left_join(full_FPDS, by = "duns") %>% ###get regdates
  select(duns, contractingofficerbusinesssizedetermination, biz_size, 
         NAICS2, signeddate, registrationDate, 
         expirationDate, survival.status) %>% 
  filter(contractingofficerbusinesssizedetermination == "O") %>% ###all who at some point became O
  group_by(duns) %>% 
  mutate(maxyear = max(signeddate), na) %>% 
  select(-signeddate)

gradfirmS<- panel_data %>% 
  filter(ten.year == 1) %>% ###all who survived 10 years
  filter(biz_size == 0) %>% ### all who started small
  left_join(full_FPDS, by = "duns") %>% ###get regdates
  select(duns, contractingofficerbusinesssizedetermination, biz_size, 
         NAICS2, signeddate, registrationDate, 
         expirationDate, survival.status) %>% 
  filter(contractingofficerbusinesssizedetermination == "S") %>% ###all who at some point became O
  group_by(duns) %>% 
  mutate(maxyear = max(signeddate)) %>% 
  select(-signeddate)


unique.grad <- gradfirm[!duplicated(gradfirm[,c('duns', 'maxyear')]),]

unique.S <- gradfirmS[!duplicated(gradfirmS[,c('duns', 'maxyear')]),]

test<- rbind(unique.grad, unique.S)
  
  
  


#gradfirm$signeddate <- year(as.Date(gradfirm$signeddate, format = "%Y/%m/%d"))
#gradfirm$registrationDate <- year(as.Date(gradfirm$registrationDate, format = "%Y/%m/%d"))

gradfirm1 <- gradfirm %>% 
  filter(contractingofficerbusinesssizedetermination == "O")%>% 
  filter(year(signeddate)<=2011) %>% 
  filter(year(registrationDate)>=2001 & year(registrationDate)<=2011) %>% 
  arrange(duns, signeddate)
  

maxdate <- gradfirm1[unlist(tapply(row.names(gradfirm1), gradfirm1$duns, tail, n = 1)), ] 

gradfirm2 <- maxdate %>% 
  filter(contractingofficerbusinesssizedetermination == "O") %>% 
  left_join(panel_data, by = c("duns")) %>% 
  select(-X1) %>% 
  left_join(full_FPDS, by = c("duns")) %>% 
  select(duns:AGENCY_NAME, registrationDate.x, expirationDate.x) %>% 
  select(-principalnaicscode.x)

unique.gradfirm <- gradfirm2[!duplicated(gradfirm2),]

  
write.csv(unique.gradfirm, "graduatedfirms.csv")

--------------------------------------------------------------
##for 2012-2016
 
  gradfirm <- panel_data %>% 
  filter(biz_size == 0) %>% 
  left_join(full_FPDS, by = "duns") %>% 
  select(duns, contractingofficerbusinesssizedetermination, biz_size.x, 
         principalnaicscode, NAICS2, registrationDate, signeddate, 
         expirationDate, survival.status)


#gradfirm$signeddate <- year(as.Date(gradfirm$signeddate, format = "%Y/%m/%d"))
#gradfirm$registrationDate <- year(as.Date(gradfirm$registrationDate, format = "%Y/%m/%d"))

gradfirm1 <- gradfirm %>% 
  filter(contractingofficerbusinesssizedetermination == "O")%>% 
  filter(year(registrationDate)<=2016 & year(registrationDate)>=2001) %>% 
  arrange(duns, signeddate)


maxdate <- gradfirm1[unlist(tapply(row.names(gradfirm1), gradfirm1$duns, tail, n = 1)), ] 

gradfirm2 <- maxdate %>% 
  filter(contractingofficerbusinesssizedetermination == "O") %>% 
  left_join(panel_data, by = c("duns")) %>% 
  select(-X1) %>% 
  left_join(full_FPDS, by = c("duns")) %>% 
  select(duns:AGENCY_NAME, registrationDate.x, expirationDate.x) %>% 
  select(-principalnaicscode.x)

unique.gradfirm <- gradfirm2[!duplicated(gradfirm2),]

write.csv(unique.gradfirm, "graduatedfirms0116.csv")  
  
  
###S consistant and Mixed (switching between S and O b/c change in NAICS) = not graduated
###O consistent = graduated

