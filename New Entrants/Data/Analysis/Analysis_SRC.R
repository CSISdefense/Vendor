#******************************************************************
########################Set Up ################################
#******************************************************************

library(matrixStats)
library(describer)
library(tidyverse)
library(openxlsx)
library(httr)
library(jsonlite)
library(plyr)
library(data.table)
library(lubridate)
library(dplyr)
library(foreach)

setwd("K:/2018-01 NPS New Entrants/Data/Data/Cleaned Data")

load(file = "SAM_and_FPDS_uniqueDuns.Rda")


#******************************************************************
########################Count number of new entrants in each year! ################################
#******************************************************************

registrationyear_count <- table(SAM_and_FPDS_uniqueDuns$registrationYear)

registrationyear_count



load(file = "SAM_datapull_all.Rda")


load(file = "SAM_datapull_all.Rda")
registrationyear_count <- table(datapull_all$registrationYear)

registrationyear_count


##change top_small_biz to 1= small biz and 0 = non-small biz

str(SAM_and_FPDS_uniqueDuns$top_small_biz)

table(SAM_and_FPDS_uniqueDuns$top_small_biz)

SAM_and_FPDS_uniqueDuns <- SAM_and_FPDS_uniqueDuns[!(SAM_and_FPDS_uniqueDuns$top_small_biz==":"), ]

table(SAM_and_FPDS_uniqueDuns$top_small_biz)

SAM_and_FPDS_uniqueDuns$top_smallbiz_bin <- revalue(SAM_and_FPDS_uniqueDuns$top_small_biz, c("S"="1", "O"="0"))

str(SAM_and_FPDS_uniqueDuns$top_smallbiz_bin)

##bar graph

bar.data <- SAM_and_FPDS_uniqueDuns %>% 
  #mutate(regyear = year(registrationDate)) %>% 
  filter(top_smallbiz_bin == "1" | top_smallbiz_bin == "0") %>% 
  group_by(registrationYear, top_smallbiz_bin) %>% 
  dplyr::summarise(n()) %>% 
  dplyr::rename("regpersize" = `n()`) %>% 
  left_join(year_div, by = "registrationYear") %>% 
  dplyr::rename("regperyear" = `n()`)


ggplot(bar.data, aes(x = reigistrationYear, y = regpersize, fill = factor(top_smallbiz_bin), label = regperyear)) +
  geom_bar(stat = 'identity', position = 'stack') +
  ylab("Number of New Entrants") +
  xlab("Registration Year") +
  scale_x_continuous(breaks = c(2001:2016)) +
  ##scale_fill_manual(name = "New Entrants Types", values = c("deepskyblue", "royalblue1"), labels = c("small", "non-small")) +
  scale_fill_manual(name = "New Entrants Types", values = c("darkslategray1", "cadetblue4"), labels = c("small", "non-small")) +
  ggtitle("Number of New Entrants Per Year (2001-2016) - All Federal Agencies")+
  geom_text_repel(data = subset(bar.data, reigistrationYear >=2014), aes(label = regpersize), size = 4, box.padding = .1, 
                  angle = 45) +
  geom_text(data = subset(bar.data, reigistrationYear < 2014), aes(label = regpersize), size = 4, position = position_stack(vjust = .5), angle = 45)

