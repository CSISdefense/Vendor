## x = year(registrationDate)  OR Regyear
##y = nrows()
##add lines for biz_size 0 or 1

full_FPDS <- read_csv("SAM Data merged with FPDS, exp2000-2019.csv")

  

final_joined = full_FPDS[!duplicated(full_FPDS),]

final_joined <- final_joined %>% 
  mutate(age_at_start = year(registrationDate) - year(businessStartDate)) %>% 
  mutate(months_in_SAM = ((year(expirationDate) - year(registrationDate)) * 12) + month(expirationDate) - month(registrationDate)) %>% 
  rename(country = `samAddress countryCode`)

panel_data <- read_csv("Panel Data reg 2001-2011.csv")

x0116.dataset <- read_csv("Panel Data 2001-2016.csv")

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
survival.rates <- read_csv("survival rates reg2000-2011.csv")
survival.rates_nop <- read_csv("survival rates reg2000-2011-no parent filter.csv")

survival_rates_nop <- survival.rates_nop %>% 
  gather("10yr_all":"5yr_sm", key = "years_bizsize", value = "survival rate") %>% 
  separate(years_bizsize, into = c("year", "bizsize"), sep = "_") %>% 
  select(-X1)
 
  
ggplot(survival_rates_nop, aes(x = year, y = `survival rate`, fill = bizsize)) +
  geom_bar(stat = "identity", width = .5, position = "dodge") +
  ylab("Survival Rate")  +
  xlab("Years Survived")



