
##for sam
setwd("K:/2018-01 NPS New Entrants/Data/Data/Cleaned Data")
getwd()

## x = year(registrationDate)  OR Regyear
##y = nrows()
##add lines for biz_size 0 or 1

##filtered 2001-2016####

full_FPDS <- read_csv("SAM Data merged with FPDS, exp2000-2019.csv")

final_joined = full_FPDS[!duplicated(full_FPDS),]

final_joined <- final_joined %>% 
  mutate(age_at_start = year(registrationDate) - year(businessStartDate)) %>% 
  rename(country = `samAddress countryCode`)

#panel_data <- read_csv("Panel Data reg 2001-2011.csv")

x0116.dataset <- read_csv("Panel Data reg2001-2016, SD2010-2025 - nop, 10plus1 year view.csv")

library(tidyverse)
library(lubridate)


###count filtered ####

timeseries_data <- x0116.dataset %>% 
  left_join(full_FPDS[, c("duns", "registrationDate")], by = "duns") %>% 
  mutate(regyear = year(registrationDate.y))

timeseries_data = timeseries_data[!duplicated(timeseries_data["duns"]),]

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
