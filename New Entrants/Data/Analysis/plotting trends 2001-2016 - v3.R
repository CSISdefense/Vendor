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

x0116.dataset <- read_csv("K:/2018-01 NPS New Entrants/Data/Data/Cleaned Data/Panel Data reg2001-2016 - ver3.csv")

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



##DoD####

###count DOD nop####

DODfildata0116 <- read_csv("K:/2018-01 NPS New Entrants/Data/Data/Cleaned Data/Panel Data reg2001-2016 DOD - ver2.csv")

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
