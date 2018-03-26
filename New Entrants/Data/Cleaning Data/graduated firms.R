library(tidyverse)
library(data.table)
library(lubridate)
library(plyr)

###determine in biz-size column if change (for each duns)

panel_data <- read_csv("Panel Data reg 2001-2011.csv")
full_FPDS <- read_csv("SAM Data merged with FPDS, exp2000-2019.csv")

### Small start firms



gradfirm<- panel_data %>% 
  filter(biz_size == 0) %>% 
  left_join(full_FPDS, by = "duns") %>% 
  select(duns, contractingofficerbusinesssizedetermination, biz_size, principalnaicscode, NAICS2, registrationDate, signeddate, expirationDate) %>% 
  filter(!is.na(principalnaicscode)) %>% 
  filter(!is.na(signeddate))



#gradfirm$signeddate <- year(as.Date(gradfirm$signeddate, format = "%Y/%m/%d"))
#gradfirm$registrationDate <- year(as.Date(gradfirm$registrationDate, format = "%Y/%m/%d"))

gradfirm1 <- gradfirm %>%   
  filter(contractingofficerbusinesssizedetermination == "O") %>% 
  select(duns, contractingofficerbusinesssizedetermination) %>% 
  distinct() %>% 
  left_join(gradfirm, by = "duns") %>% 
  arrange(duns, signeddate, desc(contractingofficerbusinesssizedetermination.y)) %>% 
  filter(year(signeddate) <= 2011) %>% 
  select(duns, signeddate, contractingofficerbusinesssizedetermination.y, NAICS2, principalnaicscode)

maxdate <- gradfirm1[unlist(tapply(row.names(gradfirm1), gradfirm1$duns, tail, n = 1)), ] 

gradfirm2 <- maxdate %>% 
  filter(contractingofficerbusinesssizedetermination.y == "O")

write.csv(gradfirm2, "graduatedfirms.csv")


 
###S consistant and Mixed (switching between S and O b/c change in NAICS) = not graduated
###O consistent = graduated

