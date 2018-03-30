library(tidyverse)
library(data.table)
library(lubridate)
library(plyr)

###determine in biz-size column if change (for each duns)

panel_data <- read_csv("K:/2018-01 NPS New Entrants/Data/Data/Cleaned Data/Panel Data reg2001-2016 - ver 2.csv")
full_FPDS <- read_csv("K:/2018-01 NPS New Entrants/Data/Data/Cleaned Data/SAM Data merged with FPDS, exp2000-2019.csv")

### Small start firms

total.survived<- panel_data %>% 
  filter(ten.year == "TRUE") %>% ###all who survived 10 years
  filter(biz_size == 0) %>% ### all who started small
  left_join(full_FPDS[c("duns", "contractingofficerbusinesssizedetermination","signeddate")], by = "duns") %>% ###get regdates
  select(duns, contractingofficerbusinesssizedetermination, biz_size, 
         NAICS2, registrationDate, signeddate, lastsigneddate, survival.status, 
         ten.year) %>% 
  filter(year(signeddate) <= year(registrationDate) + 10) %>%
  filter(contractingofficerbusinesssizedetermination == "S" | contractingofficerbusinesssizedetermination == "O") %>% 
  arrange(desc(signeddate))

unique.tot <- total.survived[!duplicated(total.survived[,c('duns')]),]

gradfirm <- unique.tot %>% 
  filter(contractingofficerbusinesssizedetermination == "O")
  
nrow(gradfirm)/nrow(unique.tot)

write.csv(gradfirm, "graduated firms.csv")


