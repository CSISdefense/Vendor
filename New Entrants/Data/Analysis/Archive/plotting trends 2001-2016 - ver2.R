
##for sam
setwd("K:/2018-01 NPS New Entrants/Data/Data/Cleaned Data")
getwd()

#install.packages("matrixStats")
library(matrixStats)
library(describer)
library(tidyverse)
#install.packages("lubridate")
#install.packages("openxlsx")
library(openxlsx)
#install.packages("httr")
library(httr)
#install.packages("jsonlite")
library(jsonlite)
library(plyr)
#install.packages("data.table")
library(data.table)
library(lubridate)
#install.packages("ggrepel")
library(ggrepel)

## x = year(registrationDate)  OR Regyear
##y = nrows()
##add lines for biz_size 0 or 1

##filtered 2001-2016####

full_FPDS <- read_csv("SAM Data merged with FPDS, exp2000-2019.csv")

final_joined = full_FPDS[!duplicated(full_FPDS),]

final_joined <- final_joined %>% 
  mutate(age_at_start = year(registrationDate) - year(businessStartDate)) %>% 
  dplyr::rename(country = `samAddress countryCode`)

#panel_data <- read_csv("Panel Data reg 2001-2011.csv")

x0116.dataset <- read_csv("K:/2018-01 NPS New Entrants/Data/Data/Cleaned Data/Panel Data reg2001-2016 - ver4.csv")

library(tidyverse)
library(lubridate)


##Plot####
###count filtered ###

###All fed agencies####

timeseries_data <- x0116.dataset %>% 
  left_join(full_FPDS[, c("duns", "registrationDate")], by = "duns") %>% 
  mutate(regyear = year(registrationDate.y))

test <- timeseries_data %>% 
  filter(regyear==2008)

timeseries_data = timeseries_data[!duplicated(timeseries_data["duns"]),]

timeseries_data.all <- timeseries_data %>% 
  group_by(regyear) %>% 
  dplyr::summarise(freq = n()) %>% 
  right_join(timeseries_data, by = "regyear")
  
timeseries_data.size <- timeseries_data.all %>% 
  group_by(regyear, biz_size, freq) %>% 
  dplyr::summarise(sizefreq = n()) %>%
  spread(biz_size, sizefreq) %>% 
  dplyr::rename("non_small" = "1", "small" = "0", "all" = "freq")
 # right_join(timeseries_data.all, by = c("regyear", "biz_size"))


ggplot(timeseries_data.size, aes(x = regyear)) +
  geom_line(aes(y = all, colour = "midnightblue")) +
  geom_line(aes(y = non_small, colour = "lightskyblue")) +
  geom_line(aes(y = small, colour = "blue")) +
  ylab("Number of new entrants") +
  xlab("Year") +
  scale_y_continuous(limits = c(0, 2300)) +
  scale_x_continuous("Registration Date", labels = as.character(timeseries_data.size$regyear), breaks = timeseries_data.size$regyear) +
  scale_color_manual(name = "New Entrants Types", values = c("midnightblue" = "midnightblue", "lightskyblue" = "lightskyblue", "blue" = "blue"), labels = c("all","non-small", "small")) +
  ggtitle("Number of New Entrants per Year (2001-2016) - All Federal Agencies")

##bar all####
year_div <- x0116.dataset %>% 
  mutate(regyear = year(registrationDate)) %>% 
  filter(biz_size == 1 | biz_size == 0) %>% 
  group_by(regyear) %>% 
  dplyr::summarise(n())  


bar.data <- x0116.dataset %>% 
  mutate(regyear = year(registrationDate)) %>% 
  filter(biz_size == 1 | biz_size == 0) %>% 
  group_by(regyear, biz_size) %>% 
  dplyr::summarise(n()) %>% 
  dplyr::rename("regpersize" = `n()`) %>% 
  left_join(year_div, by = "regyear") %>% 
  dplyr::rename("regperyear" = `n()`)

bar.data1 <- bar.data %>% 
  filter(regyear <=2006)

ggplot(bar.data1, aes(x = regyear, y = regpersize, fill = factor(biz_size), label = regperyear)) +
  geom_bar(stat = 'identity', position = 'stack') +
  ylab("Number of New Entrants") +
  xlab("Registration Year") +
  scale_x_continuous(breaks = c(2001:2006)) +
  ##scale_fill_manual(name = "New Entrants Types", values = c("deepskyblue", "royalblue1"), labels = c("small", "non-small")) +
  scale_fill_manual(name = "New Entrants Types", values = c("darkslategray1", "cadetblue4"), labels = c("small", "non-small")) +
  ggtitle("Number of New Entrants Per Year (2001-2006) - All Federal Agencies") +
  geom_text(data = bar.data1, aes(label = regpersize), size = 4, position = position_stack(vjust = .5), angle = 45)


bar.data2 <- bar.data %>% 
  filter(regyear > 2006 & regyear < 2013)

ggplot(bar.data2, aes(x = regyear, y = regpersize, fill = factor(biz_size), label = regperyear)) +
  geom_bar(stat = 'identity', position = 'stack') +
  ylab("Number of New Entrants") +
  xlab("Registration Year") +
  scale_x_continuous(breaks = c(2007:2012)) +
  ##scale_fill_manual(name = "New Entrants Types", values = c("deepskyblue", "royalblue1"), labels = c("small", "non-small")) +
  scale_fill_manual(name = "New Entrants Types", values = c("darkslategray1", "cadetblue4"), labels = c("small", "non-small")) +
  ggtitle("Number of New Entrants Per Year (2007-2012) - All Federal Agencies") +
  geom_text(data = bar.data2, aes(label = regpersize), size = 4, position = position_stack(vjust = .5), angle = 45)


bar.data3 <- bar.data %>% 
  filter(regyear >= 2013)

ggplot(bar.data3, aes(x = regyear, y = regpersize, fill = factor(biz_size), label = regperyear)) +
  geom_bar(stat = 'identity', position = 'stack') +
  ylab("Number of New Entrants") +
  xlab("Registration Year") +
  scale_x_continuous(breaks = c(2013:2016)) +
  ##scale_fill_manual(name = "New Entrants Types", values = c("deepskyblue", "royalblue1"), labels = c("small", "non-small")) +
  scale_fill_manual(name = "New Entrants Types", values = c("darkslategray1", "cadetblue4"), labels = c("small", "non-small")) +
  ggtitle("Number of New Entrants Per Year (2013-2016) - All Federal Agencies")+
  geom_text(data = bar.data3, aes(label = regpersize), size = 4, position = position_stack(vjust = .5), angle = 45)



ggplot(bar.data, aes(x = regyear, y = regpersize, fill = factor(biz_size), label = regperyear)) +
  geom_bar(stat = 'identity', position = 'stack') +
  ylab("Number of New Entrants") +
  xlab("Registration Year") +
  scale_x_continuous(breaks = c(2001:2016)) +
  ##scale_fill_manual(name = "New Entrants Types", values = c("deepskyblue", "royalblue1"), labels = c("small", "non-small")) +
  scale_fill_manual(name = "New Entrants Types", values = c("darkslategray1", "cadetblue4"), labels = c("small", "non-small")) +
  ggtitle("Number of New Entrants Per Year (2001-2016) - All Federal Agencies")+
  geom_text_repel(data = subset(bar.data, regyear >=2014), aes(label = regpersize), size = 4, box.padding = .1, 
                  angle = 45) +
  geom_text(data = subset(bar.data, regyear < 2014), aes(label = regpersize), size = 4, position = position_stack(vjust = .5), angle = 45)
  
  #geom_text(aes(label = regpersize), size = 4, position = position_stack(vjust = .5), angle = 45)
  #geom_label(aes(label = regpersize), size = 3, position = position_stack(vjust = .5))


  
  
  
  

##DoD####

###count DOD nop####

DODfildata0116 <- read_csv("K:/2018-01 NPS New Entrants/Data/Data/Cleaned Data/Panel Data reg2001-2016 DOD - ver4.csv")

timeseries_dataDOD <- DODfildata0116 %>% 
  left_join(full_FPDS[, c("duns", "registrationDate")], by = "duns") %>% 
  distinct() %>% 
  mutate(regyear = year(registrationDate.y))


timeseries_data.allDOD <- timeseries_dataDOD %>% 
  group_by(regyear) %>% 
  dplyr::summarise(freq = n()) %>% 
  right_join(timeseries_dataDOD, by = "regyear")

timeseries_data.sizeDOD <- timeseries_data.allDOD %>% 
  group_by(regyear, biz_size, freq) %>% 
  dplyr::summarise(sizefreq = n()) %>%
  spread(biz_size, sizefreq) %>% 
  dplyr::rename("non_small" = "1", "small" = "0", "all" = "freq")
# right_join(timeseries_data.all, by = c("regyear", "biz_size"))

ggplot(timeseries_data.sizeDOD, aes(x = regyear)) +
  geom_line(aes(y = all, colour = "midnightblue")) +
  geom_line(aes(y = non_small, colour = "skyblue")) +
  geom_line(aes(y = small, colour = "blue")) +
  ylab("Number of new entrants") +
  xlab("Year") +
  scale_y_continuous(limits = c(0, 2300)) +
  scale_x_continuous("Registration Date", labels = as.character(timeseries_data.size$regyear), breaks = timeseries_data.size$regyear) +
  scale_color_manual(name = "New Entrants Types", values = c("midnightblue" = "midnightblue", "skyblue" = "skyblue", "blue" = "blue"), labels = c("all","non-small", "small"))+
  ggtitle("Number of New Entrants per Year (2001-2016) - DOD")

########## bar DOD####

year_divDOD <- DODfildata0116 %>% 
  mutate(regyear = year(registrationDate)) %>% 
  filter(biz_size == 1 | biz_size == 0) %>% 
  group_by(regyear) %>% 
  dplyr::summarise(n())  


bar.dataDOD <- DODfildata0116 %>% 
  mutate(regyear = year(registrationDate)) %>% 
  filter(biz_size == 1 | biz_size == 0) %>% 
  group_by(regyear, biz_size) %>% 
  dplyr::summarise(n()) %>% 
  dplyr::rename("regpersize" = `n()`) %>% 
  left_join(year_divDOD, by = "regyear") %>% 
  dplyr::rename("regperyear" = `n()`)


ggplot(bar.dataDOD, aes(x = regyear, y = regpersize, fill = factor(biz_size), label = regperyear)) +
  geom_bar(stat = 'identity') +
  ylab("Number of New Entrants") +
  xlab("Registration Year") +
  scale_x_continuous(breaks = c(2001:2016)) +
  ##scale_fill_manual(name = "New Entrants Types", values = c("deepskyblue", "royalblue1"), labels = c("small", "non-small")) +
  scale_fill_manual(name = "New Entrants Types", values = c("darkslategray1", "cadetblue4"), labels = c("small", "non-small")) +
  ggtitle("Number of New Entrants Per Year (2001-2016) - DOD")+
  geom_text_repel(data = subset(bar.dataDOD, regyear >=2003), aes(label = regpersize), size = 4, box.padding = .1, 
                  angle = 45) +
  geom_text(data = subset(bar.dataDOD, regyear < 2003), aes(label = regpersize), size = 4, position = position_stack(vjust = .5), angle = 45)

###NAICS filtered####

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
  scale_y_continuous(limits = c(0, 3000)) +
  scale_color_manual(name = "New Entrants Types", values = c("black" = "black", "blue" = "blue", "red" = "red"), labels = c("all","non-small", "small")) +
  ggtitle("# of New Entrants per Year (2001-2016) - All Agencies")



##ggplot(timeseries_data.NAICS, aes(x = regyear, y = NAICSfreq, fill = NAICS2)) +
##  geom_bar(stat = "identity", width = .5, position = "dodge") +
##  ylab("NAICS") +
##  scale_x_continuous("Registration Date", labels = as.character(timeseries_data.size$regyear), breaks = timeseries_data.size$regyear) +
##  facet_wrap(~survival.status)
  
###stats filtered####

# t_test <- read_csv("t-test reg2000-2011.csv")
# t_test_nop <- read_csv("t-test reg2000-2011-no parent filter.csv")
# survival.rates <- read_csv("survival rates reg2000-2011.csv")
# survival.rates_nop <- read_csv("survival rates reg2000-2011-no parent filter.csv")
# 
# survival_rates_nop <- survival.rates_nop %>% 
#   gather("10yr_all":"5yr_sm", key = "years_bizsize", value = "survival rate") %>% 
#   separate(years_bizsize, into = c("year", "bizsize"), sep = "_") %>% 
#   select(-X1)
#  
#   
# ggplot(survival_rates_nop, aes(x = year, y = `survival rate`, fill = bizsize)) +
#   geom_bar(stat = "identity", width = .5, position = "dodge") +
#   ylab("Survival Rate")  +
#   xlab("Years Survived")


###unfiltered 2001-2016 ####

###count unfiltered by anything ####

# yearfildata0116 <- read_csv("Panel Data filtered only by year reg 2011-2016.csv")
# 
# timeseries_data <- yearfildata0116 %>% 
#   left_join(full_FPDS[, c("duns", "registrationDate")], by = "duns") %>% 
#   distinct()
# 
# 
# timeseries_data.all <- timeseries_data %>% 
#   group_by(regyear) %>% 
#   dplyr::summarise(freq = n()) %>% 
#   right_join(timeseries_data, by = "regyear")
# 
# timeseries_data.size <- timeseries_data.all %>% 
#   group_by(regyear, biz_size, freq) %>% 
#   dplyr::summarise(sizefreq = n()) %>%
#   spread(biz_size, sizefreq) %>% 
#   rename("non_small" = "1", "small" = "0", "all" = "freq")
# # right_join(timeseries_data.all, by = c("regyear", "biz_size"))
# 
# ggplot(timeseries_data.size, aes(x = regyear)) +
#   geom_line(aes(y = all, colour = "black")) +
#   geom_line(aes(y = non_small, colour = "blue")) +
#   geom_line(aes(y = small, colour = "red")) +
#   ylab("Number of new entrants") +
#   xlab("Year") +
#   scale_y_continuous(limits = c(0, 3000)) +
#   scale_x_continuous("Registration Date", labels = as.character(timeseries_data.size$regyear), breaks = timeseries_data.size$regyear) +
#   scale_color_manual(name = "New Entrants Types", values = c("black" = "black", "blue" = "blue", "red" = "red"), labels = c("all","non-small", "small"))+
#   ggtitle("# of New Entrants per Year (2001-2016) unfiltered")

###count nop ####

yearfildata0116 <- read_csv("Panel Data 2001-2016 -nop.csv")

timeseries_data <- yearfildata0116 %>% 
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

ggplot(timeseries_data.size, aes(x = regyear)) +
  geom_line(aes(y = all, colour = "black")) +
  geom_line(aes(y = non_small, colour = "blue")) +
  geom_line(aes(y = small, colour = "red")) +
  ylab("Number of new entrants") +
  xlab("Year") +
  scale_y_continuous(limits = c(0, 2300)) +
  scale_x_continuous("Registration Date", labels = as.character(timeseries_data.size$regyear), breaks = timeseries_data.size$regyear) +
  scale_color_manual(name = "New Entrants Types", values = c("black" = "black", "blue" = "blue", "red" = "red"), labels = c("all","non-small", "small"))+
  ggtitle("Number of New Entrants per Year (2001-2016) - All Federal Agencies")
###DOD filtered 2001-2016####

###count DOD filtered####

# DODfildata0116 <- read_csv("Panel Data 2001-2016 DOD.csv")
# 
# timeseries_data <- DODfildata0116 %>% 
#   left_join(full_FPDS[, c("duns", "registrationDate")], by = "duns") %>% 
#   distinct() %>% 
#   mutate(regyear = year(registrationDate))
# 
# 
# timeseries_data.all <- timeseries_data %>% 
#   group_by(regyear) %>% 
#   dplyr::summarise(freq = n()) %>% 
#   right_join(timeseries_data, by = "regyear")
# 
# timeseries_data.size <- timeseries_data.all %>% 
#   group_by(regyear, biz_size, freq) %>% 
#   dplyr::summarise(sizefreq = n()) %>%
#   spread(biz_size, sizefreq) %>% 
#   rename("non_small" = "1", "small" = "0", "all" = "freq")
# # right_join(timeseries_data.all, by = c("regyear", "biz_size"))
# 
# ggplot(timeseries_data.size, aes(x = regyear)) +
#   geom_line(aes(y = all, colour = "black")) +
#   geom_line(aes(y = non_small, colour = "blue")) +
#   geom_line(aes(y = small, colour = "red")) +
#   ylab("Number of new entrants") +
#   xlab("Year") +
#   scale_y_continuous(limits = c(0, 3000)) +
#   scale_x_continuous("Registration Date", labels = as.character(timeseries_data.size$regyear), breaks = timeseries_data.size$regyear) +
#   scale_color_manual(name = "New Entrants Types", values = c("black" = "black", "blue" = "blue", "red" = "red"), labels = c("all","non-small", "small"))+
#   ggtitle("# of New Entrants per Year (2001-2016) - DOD")


###count DOD nop####

DODfildata0116 <- read_csv("Panel Data reg2001-2016, SD2010-2025 DOD- nop, 10plus1 year view.csv")

timeseries_dataDOD <- DODfildata0116 %>% 
  left_join(full_FPDS[, c("duns", "registrationDate")], by = "duns") %>% 
  distinct() %>% 
  mutate(regyear = year(registrationDate.y))


timeseries_data.allDOD <- timeseries_dataDOD %>% 
  group_by(regyear) %>% 
  dplyr::summarise(freq = n()) %>% 
  right_join(timeseries_dataDOD, by = "regyear")

timeseries_data.sizeDOD <- timeseries_data.allDOD %>% 
  group_by(regyear, biz_size, freq) %>% 
  dplyr::summarise(sizefreq = n()) %>%
  spread(biz_size, sizefreq) %>% 
  rename("non_small" = "1", "small" = "0", "all" = "freq")
# right_join(timeseries_data.all, by = c("regyear", "biz_size"))

ggplot(timeseries_data.sizeDOD, aes(x = regyear)) +
  geom_line(aes(y = all, colour = "black")) +
  geom_line(aes(y = non_small, colour = "blue")) +
  geom_line(aes(y = small, colour = "red")) +
  ylab("Number of new entrants") +
  xlab("Year") +
  scale_y_continuous(limits = c(0, 2300)) +
  scale_x_continuous("Registration Date", labels = as.character(timeseries_data.size$regyear), breaks = timeseries_data.size$regyear) +
  scale_color_manual(name = "New Entrants Types", values = c("black" = "black", "blue" = "blue", "red" = "red"), labels = c("all","non-small", "small"))+
  ggtitle("Number of New Entrants per Year (2001-2016) - DOD")

###count grad nop####

gradfildata0116 <- read_csv("graduatedfirms0116.csv")

timeseries_datagrad <- gradfildata0116 %>% 
  distinct() %>% 
  mutate(regyear = year(registrationDate.x))


timeseries_data.allgrad <- timeseries_datagrad %>% 
  group_by(regyear) %>% 
  dplyr::summarise(freq = n()) %>% 
  right_join(timeseries_datagrad, by = "regyear")

timeseries_data.sizegrad <- timeseries_data.allgrad %>% 
  group_by(regyear, survival.status.x, freq) %>% 
  dplyr::summarise(survfreq = n()) %>%
  spread(survival.status.x, survfreq) %>% 
  rename("survived" = "1", "exited" = "0", "all" = "freq")
# right_join(timeseries_data.all, by = c("regyear", "biz_size"))

timeseries_data.sizegrad[is.na(timeseries_data.sizegrad)] <- 0

ggplot(timeseries_data.sizegrad, aes(x = regyear)) +
  geom_line(aes(y = all, colour = "black")) +
  geom_line(aes(y = survived, colour = "blue")) +
  geom_line(aes(y = exited, colour = "red")) +
  ylab("Number of Small New Entrants that Graduated to a Non-Small Firm") +
  xlab("Year") +
  scale_y_continuous(limits = c(0, 500)) +
  scale_x_continuous("Registration Date", labels = as.character(timeseries_data.size$regyear), breaks = timeseries_data.size$regyear) +
  scale_color_manual(name = "Survival Status", values = c("black" = "black", "blue" = "blue", "red" = "red"), labels = c("all","survived", "exited"))+
  ggtitle("Number of Small New Entrants that Graduated to a Non-Small Firm per Year (2001-2011) by Survival.Status")
