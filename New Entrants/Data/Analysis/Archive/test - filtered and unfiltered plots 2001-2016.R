library(openxlsx)
library(httr)
library(jsonlite)
library(plyr)
library(data.table)
library(tidyverse)

##fully unfiltered

full_FPDS <- read_csv("SAM Data merged with FPDS, exp2000-2019.csv")

full_FPDS <- full_FPDS %>% 
  filter(contractingofficerbusinesssizedetermination == "O" |
           contractingofficerbusinesssizedetermination == "S" |
           contractingofficerbusinesssizedetermination == 1 |
           contractingofficerbusinesssizedetermination == 0) %>% 
  mutate(biz_size = ifelse(contractingofficerbusinesssizedetermination == "O" | 
                             contractingofficerbusinesssizedetermination == 1, 1, 0))





year.2001 = full_FPDS %>% 
  mutate(regyear = year(registrationDate)) %>%
  arrange(regyear) %>% 
  filter(regyear == 2001) 

x2001.unique = year.2001[!duplicated(year.2001[,c('duns')]),]

year.2002 = full_FPDS %>% 
  mutate(regyear = year(registrationDate)) %>%
  arrange(regyear) %>% 
  filter(regyear == 2002) 

x2002.unique = year.2002[!duplicated(year.2002[,c('duns')]),]

year.2003 = full_FPDS %>% 
  mutate(regyear = year(registrationDate)) %>%
  arrange(regyear) %>% 
  filter(regyear == 2003) 

x2003.unique = year.2003[!duplicated(year.2003[,c('duns')]),]

year.2004 = full_FPDS %>% 
  mutate(regyear = year(registrationDate)) %>%
  arrange(regyear) %>% 
  filter(regyear == 2004) 

x2004.unique = year.2004[!duplicated(year.2004[,c('duns')]),]

year.2005 = full_FPDS %>% 
  mutate(regyear = year(registrationDate)) %>%
  arrange(regyear) %>% 
  filter(regyear == 2005) 

x2005.unique = year.2005[!duplicated(year.2005[,c('duns')]),]

year.2006 = full_FPDS %>% 
  mutate(regyear = year(registrationDate)) %>%
  arrange(regyear) %>% 
  filter(regyear == 2006) 

x2006.unique = year.2006[!duplicated(year.2006[,c('duns')]),]

year.2007 = full_FPDS %>% 
  mutate(regyear = year(registrationDate)) %>%
  arrange(regyear) %>% 
  filter(regyear == 2007) 

x2007.unique = year.2007[!duplicated(year.2007[,c('duns')]),]

year.2008 = full_FPDS %>% 
  mutate(regyear = year(registrationDate)) %>%
  arrange(regyear) %>% 
  filter(regyear == 2008) 

x2008.unique = year.2008[!duplicated(year.2008[,c('duns')]),]

year.2009 = full_FPDS %>% 
  mutate(regyear = year(registrationDate)) %>%
  arrange(regyear) %>% 
  filter(regyear == 2009) 

x2009.unique = year.2009[!duplicated(year.2009[,c('duns')]),]

year.2010 = full_FPDS %>% 
  mutate(regyear = year(registrationDate)) %>%
  arrange(regyear) %>% 
  filter(regyear == 2010) 

x2010.unique = year.2010[!duplicated(year.2010[,c('duns')]),]

year.2011 = full_FPDS %>% 
  mutate(regyear = year(registrationDate)) %>%
  arrange(regyear) %>% 
  filter(regyear == 2011) 

x2011.unique = year.2011[!duplicated(year.2011[,c('duns')]),]

year.2012 = full_FPDS %>% 
  mutate(regyear = year(registrationDate)) %>%
  arrange(regyear) %>% 
  filter(regyear == 2012) 

x2012.unique = year.2012[!duplicated(year.2012[,c('duns')]),]

year.2013 = full_FPDS %>% 
  mutate(regyear = year(registrationDate)) %>%
  arrange(regyear) %>% 
  filter(regyear == 2013) 

x2013.unique = year.2013[!duplicated(year.2013[,c('duns')]),]

year.2014 = full_FPDS %>% 
  mutate(regyear = year(registrationDate)) %>%
  arrange(regyear) %>% 
  filter(regyear == 2014) 

x2014.unique = year.2014[!duplicated(year.2014[,c('duns')]),]

year.2015 = full_FPDS %>% 
  mutate(regyear = year(registrationDate)) %>%
  arrange(regyear) %>% 
  filter(regyear == 2015) 

x2015.unique = year.2015[!duplicated(year.2015[,c('duns')]),]

year.2016 = full_FPDS %>% 
  mutate(regyear = year(registrationDate)) %>%
  arrange(regyear) %>% 
  filter(regyear == 2016) 

x2016.unique = year.2016[!duplicated(year.2016[,c('duns')]),]


yearfildata0116 <- rbind(x2001.unique, x2002.unique, x2003.unique, x2004.unique, x2005.unique,
                         x2006.unique, x2007.unique, x2008.unique, x2009.unique, x2010.unique,
                         x2011.unique, x2012.unique, x2013.unique, x2014.unique, x2015.unique,
                         x2016.unique)

write.csv(yearfildata0116, "Panel Data filtered only by year reg 2011-2016.csv")

x0116.dataset <- read_csv("Panel Data 2001-2016.csv")


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

ggplot(timeseries_data.size, aes(x = regyear)) +
  geom_line(aes(y = all, colour = "black")) +
  geom_line(aes(y = non_small, colour = "blue")) +
  geom_line(aes(y = small, colour = "red")) +
  ylab("Number of new entrants") +
  xlab("Year") +
  scale_x_continuous("Registration Date", labels = as.character(timeseries_data.size$regyear), breaks = timeseries_data.size$regyear) +
  scale_color_manual(name = "New Entrants Types", values = c("black" = "black", "blue" = "blue", "red" = "red"), labels = c("all","non-small", "small"))+
  ggtitle("# of New Entrants per Year (2001-2016) filtered")


##ALL

timeseries_data <- yearfildata0116 %>% 
  left_join(full_FPDS[, c("duns", "registrationDate")], by = "duns") %>% 
  distinct()


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
  scale_x_continuous("Registration Date", labels = as.character(timeseries_data.size$regyear), breaks = timeseries_data.size$regyear) +
  scale_color_manual(name = "New Entrants Types", values = c("black" = "black", "blue" = "blue", "red" = "red"), labels = c("all","non-small", "small"))+
  ggtitle("# of New Entrants per Year (2001-2016) unfiltered")
