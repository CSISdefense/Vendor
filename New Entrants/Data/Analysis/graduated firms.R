library(tidyverse)
library(data.table)
library(lubridate)
library(plyr)

setwd("K:/2018-01 NPS New Entrants/Data/Data/Cleaned Data")
getwd()


###determine in biz-size column if change (for each duns)

panel_data <- read_csv("K:/2018-01 NPS New Entrants/Data/Data/Cleaned Data/Panel Data reg2001-2016 - ver 2.csv")
full_FPDS <- read_csv("K:/2018-01 NPS New Entrants/Data/Data/Cleaned Data/SAM Data merged with FPDS, exp2000-2019.csv")

### Small start firms

total <- panel_data %>% 
  filter(ten.year == "TRUE") %>% 
  group_by(biz_size) %>% 
  dplyr::summarize(count = n())


total.survived<- panel_data %>% 
  filter(ten.year == "TRUE") %>% ###all who survived 10 years
  filter(biz_size == 0) %>% ### all who started small
  left_join(full_FPDS[c("duns", "contractingofficerbusinesssizedetermination",
          "signeddate","obligatedamount")], by = "duns") %>% ###get regdates
  select(duns, obligatedamount, contractingofficerbusinesssizedetermination,
         signeddate, lastsigneddate,ten.year, registrationDate) %>% 
  filter(contractingofficerbusinesssizedetermination == "O" | contractingofficerbusinesssizedetermination == "S")

nonsmallfirm <- total.survived %>% 
  filter(contractingofficerbusinesssizedetermination == "O") %>% 
  group_by(duns) %>% 
  slice(which.min(signeddate)) %>% 
  left_join(total.survived, by = "duns") %>% ###signeddate.x as mindate, signeddate.y as all dates
  filter(signeddate.y >= signeddate.x) %>% 
  select(duns, signeddate.x, obligatedamount.y, contractingofficerbusinesssizedetermination.y, signeddate.y, lastsigneddate.y, ten.year.y, registrationDate.y)

#####find first instance contracting = O, filter out the earler years to that specific id,
### calculate total obligated amount beween O and S
  
maxobligated = nonsmallfirm %>%
   group_by(duns, contractingofficerbusinesssizedetermination.y) %>% 
   slice(which.max(obligatedamount.y)) %>% 
   arrange (duns, desc(obligatedamount.y))
  
unique.maxob <- maxobligated[!duplicated(maxobligated[,c('duns')]),] 

gradfirm <- unique.maxob %>% 
  filter(contractingofficerbusinesssizedetermination.y == "O")

unique.tot <- total.survived[!duplicated(total.survived[,c('duns')]),]
  
write.csv(gradfirm, "graduated firms.csv")

nrow(gradfirm)/nrow(unique.tot)

x2001 <- gradfirm %>% 
  filter(year(registrationDate.y) == 2001)
x2002 <- gradfirm %>% 
  filter(year(registrationDate.y) == 2002)
x2003 <- gradfirm %>% 
  filter(year(registrationDate.y) == 2003)
x2004 <- gradfirm %>% 
  filter(year(registrationDate.y) == 2004)
x2005 <- gradfirm %>% 
  filter(year(registrationDate.y) == 2005)
x2006 <- gradfirm %>% 
  filter(year(registrationDate.y) == 2006)

tot2001 <- panel_data %>% 
  filter(year(registrationDate) == 2001)
tot2002 <- panel_data %>% 
  filter(year(registrationDate) == 2002)
tot2003 <- panel_data %>% 
  filter(year(registrationDate) == 2003)
tot2004 <- panel_data %>% 
  filter(year(registrationDate) == 2004)
tot2005 <- panel_data %>% 
  filter(year(registrationDate) == 2005)
tot2006 <- panel_data %>% 
  filter(year(registrationDate) == 2006)


##10-year
sr10yr2001_grad <- nrow(x2001)/nrow(tot2001) 
                   
sr10yr2002_grad <- nrow(x2002)/nrow(tot2002) 
                   
sr10yr2003_grad <- nrow(x2003)/nrow(tot2003)
                   
sr10yr2004_grad <- nrow(x2004)/nrow(tot2004)

sr10yr2005_grad <- nrow(x2005)/nrow(tot2005)
                   
sr10yr2006_grad <- nrow(x2006)/nrow(tot2006)
                   
##bound all survival rates together and reformated dataframe
                    
survivalrate_grad <- cbind(sr10yr2001_grad, sr10yr2002_grad, sr10yr2003_grad,
                           sr10yr2004_grad, sr10yr2005_grad, sr10yr2006_grad)
srnames_grad <- c("10yr_2001_grad", "10yr_2002_grad", "10yr_2003_grad",
                  "10yr_2004_grad", "10yr_2005_grad", "10yr_2006_grad")
colnames(survivalrate_grad) <- srnames_grad
                    
sr_grad <- as.data.frame(survivalrate_grad)
                    
survival.rates_grad <- sr_grad %>% 
                      gather("10yr_2001_grad":"10yr_2006_grad", key = "time_year_type_delete", value = "survivalrate_grad") %>% 
                      separate(time_year_type_delete, into = c("time", "year", "type", "delete"), sep = "_") %>% 
                      arrange(time, year) %>% 
                      unite("time_type", time, type, sep = "_") 
                    
                    
                    
survival.rates_grad <- survival.rates_grad %>% 
      spread(key = "time_type", value = "survivalrate_grad") %>% 
      select(-delete)
                    
survival.rates_grad[, c(1, 5, 7, 6, 8, 10, 9, 2, 4, 3)]
                    
write.csv(survival.rates_grad, "graduation rates-nop.csv")
                    
                    
                    