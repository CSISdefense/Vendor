#******************************************************************
########################SET UP################################
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
library(ggrepel)
library(ggthemes)
library(extrafont)
library(scales)
library(magrittr)
library(csis360)
library(scales)
library(gtable)
library(grid)
library(gridExtra)
library(ggplot2)


#******************************************************************************************************
#*******************#
#####NAVY DATA#####
#*******************#

##code mostly taken from Sam and Greg's code; reorganized
##edits and additions made by Maggie

##for clarity purposes, please run all chunks in the 'NAVY DATA' section

#**********#
####1. general data cleaning####
#*********#

####1.1 src####
setwd("K:/2018-01 NPS New Entrants/Data/Data/Cleaning data/FPDS")

load(file = "FPDS_datapull_all_v3.Rda")
length(unique(FPDS_cleaned_unique$Dunsnumber)) == nrow(FPDS_cleaned_unique)

##count number of new entrants entered each year; drop observations that aren't with the navy
FPDS_cleaned_unique <- FPDS_cleaned_unique[(FPDS_cleaned_unique$subcustomer=="Navy"), ]

##drop observations with Registration Year before 2001
FPDS_cleaned_unique <- FPDS_cleaned_unique[!(FPDS_cleaned_unique$registrationYear<2001), ]
FPDS_cleaned_unique <- FPDS_cleaned_unique[!(FPDS_cleaned_unique$registrationYear>2016), ]
length(unique(FPDS_cleaned_unique$Dunsnumber)) == nrow(FPDS_cleaned_unique)

####1.2 gs####
##load in data
setwd("K:/2018-01 NPS New Entrants/Data/Data/Raw Data/FPDS")
FPDS_data <- read.delim("Vendor.SP_DunsnumberNewEntrants_all.txt", fill = TRUE, header=TRUE,  na.strings = c("", "NULL"))

FPDS_data<-as.data.frame(FPDS_data)

FPDS_data<-csis360::remove_bom(FPDS_data)
FPDS_data<-subset(FPDS_data,fiscal_year>=2000)

FPDS_data<-csis360::deflate(FPDS_data,money_var="obligatedAmount",
                            fy_var="fiscal_year")
#Calculate first_year
FPDS_data<-FPDS_data %>%
  group_by(Dunsnumber) %>%
  dplyr::mutate(first_year=min(fiscal_year),
                entrant=ifelse(min(fiscal_year)==fiscal_year,TRUE,FALSE))

##Calculate annual spend and presence.
fed_duns_fyear<-FPDS_data %>%
  group_by(Dunsnumber,fiscal_year,entrant,first_year) %>%
  dplyr::summarize(obligatedAmount.Deflator.2017=sum(obligatedAmount.Deflator.2017,na.rm=TRUE),
                   present=max(obligatedAmount.Deflator.2017,na.rm=TRUE))
fed_duns_fyear$present<-ifelse(fed_duns_fyear$present>0,1,0)

##Check if dunsnumber is present in previous year
FPDS_duns_prev_fyear<-subset(fed_duns_fyear,select=c(fiscal_year,Dunsnumber,present))
FPDS_duns_prev_fyear$fiscal_year<-FPDS_duns_prev_fyear$fiscal_year+1
colnames(FPDS_duns_prev_fyear)[colnames(FPDS_duns_prev_fyear)=="present"]<-"prev_present"
fed_duns_fyear<-left_join(fed_duns_fyear,FPDS_duns_prev_fyear)

##Label NAs with 0, except when at start or end of series.
fed_duns_fyear$prev_present[is.na(fed_duns_fyear$prev_present) & fed_duns_fyear$fiscal_year!=2000]<-0
summary(fed_duns_fyear$prev_present)
rm(FPDS_duns_prev_fyear)

##Check if dunsnumber is present in next year
FPDS_duns_next_fyear<-subset(fed_duns_fyear,select=c(fiscal_year,Dunsnumber,present))
FPDS_duns_next_fyear$fiscal_year<-FPDS_duns_next_fyear$fiscal_year-1
colnames(FPDS_duns_next_fyear)[colnames(FPDS_duns_next_fyear)=="present"]<-"next_present"
fed_duns_fyear<-left_join(fed_duns_fyear,FPDS_duns_next_fyear)

##Label NAs with 0, except when at start or end of series.
fed_duns_fyear$next_present[is.na(fed_duns_fyear$next_present) & fed_duns_fyear$fiscal_year<2017]<-0
summary(fed_duns_fyear$next_present)
rm(FPDS_duns_next_fyear)

fed_duns_fyear$sample_year<-fed_duns_fyear$first_year
fed_duns_fyear$sample_year[fed_duns_fyear$sample_year<2001 | fed_duns_fyear$sample_year>2006]<-"Not in sample"
fed_duns_fyear$sample_year<-factor(fed_duns_fyear$sample_year,levels=c("Not in sample","2001","2002","2003","2004","2005","2006"))

fed_duns_fyear$status<-NA
fed_duns_fyear$status[fed_duns_fyear$present==0]<-"No net payments"
fed_duns_fyear$status[is.na(fed_duns_fyear$status) & fed_duns_fyear$prev_present==0 & fed_duns_fyear$next_present==0]<-"Blip"
fed_duns_fyear$status[is.na(fed_duns_fyear$status) & fed_duns_fyear$prev_present==0 & !is.na(fed_duns_fyear$next_present)]<-"Enter"
fed_duns_fyear$status[is.na(fed_duns_fyear$status) & fed_duns_fyear$next_present==0 & !is.na(fed_duns_fyear$prev_present)]<-"Exit"
fed_duns_fyear$status[is.na(fed_duns_fyear$status) & fed_duns_fyear$prev_present==1 & fed_duns_fyear$next_present==1]<-"Steady"
fed_duns_fyear$status<-factor(fed_duns_fyear$status)
summary(fed_duns_fyear$status)

fed_duns_fyear<-subset(fed_duns_fyear,fiscal_year>=2000)

####1.3 src - graph####
##1. make not in sample binary
table(fed_duns_fyear$sample_year)
fed_duns_fyear$sample_year_bin <- revalue(fed_duns_fyear$sample_year, c("Not in sample"="0", "2001"="1", "2002"="2", "2003"="3", "2004"="4", "2005"="5", "2006"="6"))
str(fed_duns_fyear$sample_year_bin)
table(fed_duns_fyear$sample_year_bin)
fed_duns_fyear$sample_year_bin <- as.numeric(as.character(fed_duns_fyear$sample_year_bin))
str(fed_duns_fyear$sample_year_bin)
table(fed_duns_fyear$sample_year_bin)

##2. drop those not in sample and for the years not included in observation period
fed_duns_fyear_samples <- fed_duns_fyear[!(fed_duns_fyear$sample_year_bin<1), ]
fed_duns_fyear_samples_op <- fed_duns_fyear_samples[!(fed_duns_fyear_samples$fiscal_year<2001), ]
fed_duns_fyear_samples_op <- fed_duns_fyear_samples_op[!(fed_duns_fyear_samples_op$fiscal_year>2016), ]


#**********#
####2. subset by year####
#*********#

##src code

##2001
data_2001 <- FPDS_cleaned_unique[!(FPDS_cleaned_unique$registrationYear!="2001"), ]

##create variable describing whether a firm survived 3 years
data_2001 <- data_2001 %>%
  dplyr::mutate(survive_3yr = ifelse(exitYear < 2003, "0", "1")) 

##create variable describing whether a firm survived 5 years
data_2001 <- data_2001 %>%
  dplyr::mutate(survive_5yr = ifelse(exitYear < 2005, "0", "1")) 

##create variable describing whether a firm survived 10 years
data_2001 <- data_2001 %>%
  dplyr::mutate(survive_10yr = ifelse(exitYear < 2010, "0", "1")) 

##create variable describing whether a firm survived in 2016
data_2001 <- data_2001 %>%
  dplyr::mutate(survive_2016 = ifelse(exitYear < 2016, "0", "1"))

##checking small buinesses
data_2001_smallbiz <- data_2001[!(data_2001$top_smallbiz_bin!="1"), ]
min(data_2001_smallbiz$exitYear)
data_2001_nonsmallbiz <- data_2001[!(data_2001$top_smallbiz_bin!="0"), ]
min(data_2001_nonsmallbiz$exitYear)
max(data_2001_nonsmallbiz$exitYear)

##make survival vars numeric
data_2001$survive_3yr<-as.numeric(as.character(data_2001$survive_3yr))
data_2001$survive_5yr<-as.numeric(as.character(data_2001$survive_5yr))
data_2001$survive_10yr<-as.numeric(as.character(data_2001$survive_10yr))
data_2001$survive_2016<-as.numeric(as.character(data_2001$survive_2016))


##2002
data_2002 <- FPDS_cleaned_unique[!(FPDS_cleaned_unique$registrationYear!="2002"), ]

##create variable describing whether a firm survived 3 years
data_2002 <- data_2002 %>%
  dplyr::mutate(survive_3yr = ifelse(exitYear < 2004, "0", "1")) 

##create variable describing whether a firm survived 5 years
data_2002 <- data_2002 %>%
  dplyr::mutate(survive_5yr = ifelse(exitYear < 2006, "0", "1")) 

##create variable describing whether a firm survived 10 years
data_2002 <- data_2002 %>%
  dplyr::mutate(survive_10yr = ifelse(exitYear < 2011, "0", "1")) 

##create variable describing whether a firm survived in 2016
data_2002 <- data_2002 %>%
  dplyr::mutate(survive_2016 = ifelse(exitYear < 2016, "0", "1"))

##make survival vars numeric
data_2002$survive_3yr<-as.numeric(as.character(data_2002$survive_3yr))
data_2002$survive_5yr<-as.numeric(as.character(data_2002$survive_5yr))
data_2002$survive_10yr<-as.numeric(as.character(data_2002$survive_10yr))
data_2002$survive_2016<-as.numeric(as.character(data_2002$survive_2016))


##2003
data_2003 <- FPDS_cleaned_unique[!(FPDS_cleaned_unique$registrationYear!="2003"), ]

##create variable describing whether a firm survived 3 years
data_2003 <- data_2003 %>%
  dplyr::mutate(survive_3yr = ifelse(exitYear < 2005, "0", "1")) 

##create variable describing whether a firm survived 5 years
data_2003 <- data_2003 %>%
  dplyr::mutate(survive_5yr = ifelse(exitYear < 2007, "0", "1")) 

##create variable describing whether a firm survived 10 years
data_2003 <- data_2003 %>%
  dplyr::mutate(survive_10yr = ifelse(exitYear < 2012, "0", "1"))

##create variable describing whether a firm survived in 2016
data_2003 <- data_2003 %>%
  dplyr::mutate(survive_2016 = ifelse(exitYear < 2016, "0", "1"))

##make survival vars numeric
data_2003$survive_3yr<-as.numeric(as.character(data_2003$survive_3yr))
data_2003$survive_5yr<-as.numeric(as.character(data_2003$survive_5yr))
data_2003$survive_10yr<-as.numeric(as.character(data_2003$survive_10yr))
data_2003$survive_2016<-as.numeric(as.character(data_2003$survive_2016))


##2004
data_2004 <- FPDS_cleaned_unique[!(FPDS_cleaned_unique$registrationYear!="2004"), ]

##create variable describing whether a firm survived 3 years
data_2004 <- data_2004 %>%
  dplyr::mutate(survive_3yr = ifelse(exitYear < 2006, "0", "1")) 

##create variable describing whether a firm survived 5 years
data_2004 <- data_2004 %>%
  dplyr::mutate(survive_5yr = ifelse(exitYear < 2009, "0", "1")) 

##create variable describing whether a firm survived 10 years
data_2004 <- data_2004 %>%
  dplyr::mutate(survive_10yr = ifelse(exitYear < 2013, "0", "1")) 

##create variable describing whether a firm survived in 2016
data_2004 <- data_2004 %>%
  dplyr::mutate(survive_2016 = ifelse(exitYear < 2016, "0", "1"))

##make survival vars numeric
data_2004$survive_3yr<-as.numeric(as.character(data_2004$survive_3yr))
data_2004$survive_5yr<-as.numeric(as.character(data_2004$survive_5yr))
data_2004$survive_10yr<-as.numeric(as.character(data_2004$survive_10yr))
data_2004$survive_2016<-as.numeric(as.character(data_2004$survive_2016))


##2005
data_2005 <- FPDS_cleaned_unique[!(FPDS_cleaned_unique$registrationYear!="2005"), ]

##create variable describing whether a firm survived 3 years
data_2005 <- data_2005 %>%
  dplyr::mutate(survive_3yr = ifelse(exitYear < 2007, "0", "1")) 

##create variable describing whether a firm survived 5 years
data_2005 <- data_2005 %>%
  dplyr::mutate(survive_5yr = ifelse(exitYear < 2010, "0", "1")) 

##create variable describing whether a firm survived 10 years
data_2005 <- data_2005 %>%
  dplyr::mutate(survive_10yr = ifelse(exitYear < 2014, "0", "1")) 

##create variable describing whether a firm survived in 2016
data_2005 <- data_2005 %>%
  dplyr::mutate(survive_2016 = ifelse(exitYear < 2016, "0", "1"))

##make survival vars numeric
data_2005$survive_3yr<-as.numeric(as.character(data_2005$survive_3yr))
data_2005$survive_5yr<-as.numeric(as.character(data_2005$survive_5yr))
data_2005$survive_10yr<-as.numeric(as.character(data_2005$survive_10yr))
data_2005$survive_2016<-as.numeric(as.character(data_2005$survive_2016))


##2006
data_2006 <- FPDS_cleaned_unique[!(FPDS_cleaned_unique$registrationYear!="2006"), ]

##create variable describing whether a firm survived 3 years
data_2006 <- data_2006 %>%
  dplyr::mutate(survive_3yr = ifelse(exitYear < 2008, "0", "1")) 

##create variable describing whether a firm survived 5 years
data_2006 <- data_2006 %>%
  dplyr::mutate(survive_5yr = ifelse(exitYear < 2011, "0", "1")) 

##create variable describing whether a firm survived 10 years
data_2006 <- data_2006 %>%
  dplyr::mutate(survive_10yr = ifelse(exitYear < 2015, "0", "1")) 

##create variable describing whether a firm survived in 2016
data_2006 <- data_2006 %>%
  dplyr::mutate(survive_2016 = ifelse(exitYear < 2016, "0", "1"))

##make survival vars numeric
data_2006$survive_3yr<-as.numeric(as.character(data_2006$survive_3yr))
data_2006$survive_5yr<-as.numeric(as.character(data_2006$survive_5yr))
data_2006$survive_10yr<-as.numeric(as.character(data_2006$survive_10yr))
data_2006$survive_2016<-as.numeric(as.character(data_2006$survive_2016))


#**********#
####3. navy specific####
#*********#

##mw code

##parse navy dunsnumber
navy_dunsnumber <- FPDS_cleaned_unique %>% 
  select(Dunsnumber) %>%
  distinct(Dunsnumber)

##filter for just navy
fed_duns_fyear_samples_op_navy <- fed_duns_fyear_samples_op %>% 
  semi_join(navy_dunsnumber)


#******************************************************************************************************
#*******************#
#####NAVY GRAPHS#####
#*******************#

##Only customer is DoD; do not differ All-Fed from DoD

#*******************#
####New Entrants Counts####
#*******************#

#**********#
####1. Number of newe Entrants per year 2001-2016####
#*********#

##creates a dataframe that counts how many new entrants enter in each year
count_total_newentrants <- FPDS_cleaned_unique %>% 
  filter(top_smallbiz_bin == 1 | top_smallbiz_bin == 0) %>% 
  group_by(registrationYear) %>% 
  dplyr::summarise(n())  

##creates a dataframe that counts how many small vendors and how many non-small vendors are in each year
#and then joins it with the counts of all new vendors in each year
FPDS_bargraphCount <- FPDS_cleaned_unique %>%
  filter(top_smallbiz_bin == 1 | top_smallbiz_bin == 0) %>%
  group_by(registrationYear, top_smallbiz_bin) %>%
  dplyr::summarise(n()) %>%
  dplyr::rename("regpersize"=`n()`) %>%
  left_join(count_total_newentrants, by = "registrationYear") %>%
  dplyr::rename("regperyear"=`n()`) 

##generate stacked bargraph
NE_count_navy <- ggplot(FPDS_bargraphCount, aes(x = registrationYear, y = regpersize, fill = factor(top_smallbiz_bin), label = regperyear)) +
  geom_bar(stat = 'identity', position = 'stack') +
  ylab("Number of New Entrants") +
  xlab("Registration Year") +
  scale_x_continuous(breaks = c(2001:2016)) +
  ##scale_fill_manual(name = "New Entrants Types", values = c("deepskyblue", "royalblue1"), labels = c("small", "non-small")) +
  #scale_fill_manual(name = "New Entrants Types", values = c("darkslategray1", "cadetblue4"), labels = c("non-small", "small")) +
  scale_fill_manual(name = "New Entrant Type", values = c("#66CCCC", "#336666"), labels = c("non-small", "small")) +
  ggtitle("Number of New Entrants Per Year (2001-2016) - Navy")+
  ##geom_text_repel(data = subset(FPDS_bargraphCount, registrationYear >=2014), aes(label = regpersize), size = 4, box.padding = .1, 
  ###    angle = 45) +
  ##geom_text(data = subset(FPDS_bargraphCount, registrationYear < 2014), aes(label = regpersize), size = 4, position = position_stack(vjust = .5), angle = 45)
  geom_text(data = subset(FPDS_bargraphCount, registrationYear <= 2016), aes(label = regpersize), size = 4, position = position_stack(vjust = .5), angle = 45) +
  scale_y_continuous(label=comma)

NE_count_navy

#**********#
#####2. Number of New Entrants in each sample and over time####
#*********#

##filter for just navy
fed_duns_fyear_samples_op_navy <- fed_duns_fyear_samples_op %>% 
  semi_join(navy_dunsnumber)

##faceted graph
NE_eachsample_overtime_navy <- ggplot(fed_duns_fyear_samples_op_navy, aes(x = fiscal_year, fill = factor(sample_year))) +
  geom_histogram(binwidth = .5) +
  ggtitle("Number of New Entrants in Each Sample Over Time - Navy") +
  xlab("Fiscal Year") +
  ylab("Count") +
  scale_fill_manual(name = "Sample", values = c("#33CCCC", "#000066","#3399CC", "#66FFFF", "#336666", "#999999"), labels = c("2001", "2002", "2003", "2004", "2005", "2006")) +
  scale_x_continuous(breaks = c(2001:2016)) +
  facet_wrap(~sample_year, scales="fixed", ncol=1) + 
  guides(fill=FALSE) +
  scale_y_continuous(label=comma)

NE_eachsample_overtime_navy

#**********#
#####3. Number of New Entrants vs. Incumbents in each year####
#*********#

##limit to 2001 to 2016
fed_duns_fyear_op <- fed_duns_fyear %>% 
  filter(fiscal_year %in% c(2001:2016))

##filter for just navy
fed_duns_fyear_op_navy <- fed_duns_fyear_op %>% 
  semi_join(navy_dunsnumber)

##stacked graph
NE_v_incumbent_count_navy <- ggplot(fed_duns_fyear_op, aes(x = fiscal_year, fill = factor(entrant))) +
  geom_histogram(position = "stack", binwidth = .5) +
  scale_x_continuous(breaks = c(2001:2016)) +
  xlab("Fiscal Year") +
  ylab("Number of Vendors") +
  scale_fill_manual(name = "Vendor Type", values = c("#66CCCC", "#336666"), labels = c("Incumbent", "New Entrant")) +
  ggtitle("Number of New Entrants vs. Number of Incumbent Firms Over Time - Navy") +
  scale_y_continuous(label=comma)

NE_v_incumbent_count_navy


#*******************#
#####Obligations#####
#*******************#

#**********#
#####1. Percent of Obligations for small and non-small new entrants all fed and DoD faceted####
#*********#

FPDS_cleaned_unique_graphs <- FPDS_cleaned_unique[!(FPDS_cleaned_unique$registrationYear<2001), ]
FPDS_cleaned_unique_graphs <- FPDS_cleaned_unique_graphs[!(FPDS_cleaned_unique_graphs$registrationYear>2016), ]

##creates a dataframe that counts the total number of obligations for each class of new entrants
#over the entire time period 
count_total_obligations <- FPDS_cleaned_unique_graphs %>% 
  filter(top_smallbiz_bin == 1 | top_smallbiz_bin == 0) %>% 
  group_by(registrationYear) %>% 
  dplyr::summarise(sum_obligations = sum(total_obligations)) 

##create a dataframe that calculates the number of obligations that go to small vendors in each
#year and then number of obligations that go to non-small vendors in each year and joins it 
#with the counts of total number of obligations in each year and then calculate the percent
#of obligations that go to each group in each year
FPDS_obligationscount <- FPDS_cleaned_unique_graphs %>%
  filter(top_smallbiz_bin == 1 | top_smallbiz_bin == 0) %>%
  group_by(registrationYear, top_smallbiz_bin) %>%
  dplyr::summarise(sum_obligations = sum(total_obligations)) %>%
  dplyr::rename("tot_obl_bysize"=`sum_obligations`) %>%
  left_join(count_total_obligations, by = "registrationYear") %>%
  dplyr::rename("tot_obl_byyear"=`sum_obligations`) %>%
  dplyr::mutate(percent_obl_dec = tot_obl_bysize / tot_obl_byyear) %>%
  dplyr::mutate(percent_obl = percent_obl_dec * 100) %>%
  dplyr::mutate(percent_obl = round(percent_obl, 0)) %>%
  dplyr::mutate(total_percent = 100)

##generate faceted graph
##change so 0=non-small and 1=small
levels(FPDS_obligationscount$top_smallbiz_bin) <- list("Non-Small"=c("0"), "Small"=c("1"))

obligations_small_v_nsmall_navy_facet <- ggplot(FPDS_obligationscount, aes(x = registrationYear, y = tot_obl_bysize, fill = factor(top_smallbiz_bin), label = percent_obl)) +
  geom_bar(stat = 'identity', position = 'stack') +
  ylab("Total Obligations") +
  xlab("Entry Year") +
  scale_x_continuous(breaks = c(2001:2016)) +
  scale_fill_manual(name = "New Entrants Types", values = c("#66CCCC", "#336666"), labels = c("non-small", "small")) +
  ggtitle("Obligations for Small and Non-Small New Entrants (2001-2016) - All Federal Agencies")+
  geom_text(data = subset(FPDS_obligationscount, registrationYear <= 2016), aes(label = scales::percent(percent_obl_dec)), size = 3, position = position_dodge(width = 1), vjust = -0.5) +
  facet_wrap(~top_smallbiz_bin, scales="fixed", ncol=1) + ##ncol=1 stack them above eachother (in 1 column)
  ylim(0, 2.7e+11) +
  scale_y_continuous(label=unit_format(unit = "m", scale=1e-6)) +
  guides(fill=FALSE)

obligations_small_v_nsmall_navy_facet

#**********#
####2. Obligations between incumbents and new entrants in each year####
#*********#

##clean data to remove negative values
fed_duns_fyear_op_navy_summarized<-fed_duns_fyear_op_navy %>% group_by(fiscal_year,entrant) %>%
  dplyr::summarize(obligatedAmount.Deflator.2017=sum(obligatedAmount.Deflator.2017,na.rm=TRUE))

##clean data to code true and false to incumbent (false) and new entrant (true)
fed_duns_fyear_op_navy_summarized$entrant<-factor(fed_duns_fyear_op_navy_summarized$entrant)
levels(fed_duns_fyear_op_navy_summarized$entrant) <- list("Incumbent Firm"=c("FALSE"), "New Entrant"=c("TRUE"))

##faceted graph
obl_NE_v_incumbents_navy <- ggplot(fed_duns_fyear_op_navy_summarized, aes(x = fiscal_year, y = obligatedAmount.Deflator.2017, fill = entrant)) +
  geom_bar(stat = 'identity', position = 'stack') + 
  ylab("Total Obligations") +
  xlab("Fiscal Year") +
  scale_x_continuous(breaks = c(2001:2016)) +
  scale_fill_manual(name = "Vendor Type", values = c("#66CCCC", "#336666"), labels = c("Incumbent Firms", "New Entrant")) +
  ggtitle("Obligations for New Entrants vs. Incumbents (2001-2016) - Navy") +
  facet_wrap(~entrant, scales="free", ncol=1) +
  scale_y_continuous(label=unit_format(unit = "m", scale=1e-6)) +##ncol=1 stack them above eachother (in 1 column) 
  guides(fill=FALSE)

obl_NE_v_incumbents_navy

#**********#
####3. Obligations to each sample over time####
#*********#

##generate graph: obligations to each sample over time
obl_eachsample_navy <- ggplot(fed_duns_fyear_samples_op_navy, aes(x = fiscal_year, y = obligatedAmount.Deflator.2017, fill = factor(sample_year))) +
  geom_bar(stat = 'identity', position = 'stack') +
  ylab("Total Obligations") +
  xlab("New Entrant Sample") +
  scale_x_continuous(breaks = c(2001:2016)) +
  scale_fill_manual(name = "Sample", values = c("#33CCCC", "#000066","#3399CC", "#66FFFF", "#336666", "#999999"), labels = c("2001", "2002", "2003", "2004", "2005", "2006")) +
  ggtitle("Obligations to Each Sample - Navy") +
  facet_wrap(~sample_year, scales="fixed", ncol=1) +
  guides(fill=FALSE) +
  scale_y_continuous(label=unit_format(unit = "m", scale = 1e-6))

obl_eachsample_navy

#**********#
####4. Obligations to non-graduated and graduated firms####
#*********#

##step 1create a variable that describes whether a firm graduated and survived 
#10 yrs 
FPDS_all_yrs1 <- rbind(data_2001, data_2002)
FPDS_all_yrs2 <- rbind(FPDS_all_yrs1, data_2003)
FPDS_all_yrs3 <- rbind(FPDS_all_yrs2, data_2004)
FPDS_all_yrs4 <- rbind(FPDS_all_yrs3, data_2005)
FPDS_all_yrs5 <- rbind(FPDS_all_yrs4, data_2006)

#step two, create a variable that describes whether a firm graduated and survived 10 yrs
FPDS_all_yrs <- FPDS_all_yrs5 %>% group_by(Dunsnumber) %>% 
  dplyr::mutate(graduated_10yr = ifelse(graduated==1 & survive_10yr==1, 1, 0))


##creates a dataframe that counts the total number of obligations in each year
count_total_obligations_grad <- FPDS_all_yrs %>% 
  filter(graduated_10yr == 1 | graduated_10yr == 0) %>% 
  group_by(registrationYear) %>% 
  dplyr::summarise(sum_obligations = sum(total_obligations)) 

##create a dataframe that calculates the number of obligations that go to graduated vendors in each
#year and then number of obligations that go to non-graduated vendors in each year and joins it 
#with the counts of total number of obligations in each year and then calculate the percent
#of obligations that go to each group in each year
FPDS_obligationscount_grad <- FPDS_all_yrs %>%
  filter(graduated_10yr == 1 | graduated_10yr == 0) %>%
  group_by(registrationYear, graduated_10yr) %>%
  dplyr::summarise(sum_obligations = sum(total_obligations)) %>%
  dplyr::rename("tot_obl_bygrad"=`sum_obligations`) %>%
  left_join(count_total_obligations_grad, by = "registrationYear") %>%
  dplyr::rename("tot_obl_byyear"=`sum_obligations`) %>%
  dplyr::mutate(percent_obl_dec = tot_obl_bygrad / tot_obl_byyear) %>%
  dplyr::mutate(percent_obl = percent_obl_dec * 100) %>%
  dplyr::mutate(percent_obl = round(percent_obl, 0)) %>%
  dplyr::mutate(total_percent = 100)

FPDS_obligationscount_grad$graduated_10yr<-factor(FPDS_obligationscount_grad$graduated_10yr)
levels(FPDS_obligationscount_grad$graduated_10yr) <- list("Non-Graduated"=c("0"), "Graduated"=c("1"))

#generate graph
graduated_v_nongrad_navy <- ggplot(FPDS_obligationscount_grad, aes(x = registrationYear, y = tot_obl_bygrad, fill = factor(graduated_10yr), label = percent_obl)) +
  geom_bar(stat = 'identity', position = 'stack') +
  ylab("Total Obligations") +
  xlab("Entry Year") +
  scale_x_continuous(breaks = c(2001:2006)) +
  scale_y_continuous(label=unit_format(unit = "m", scale=1e-6), breaks = c(0, 5e+10, 1e+11, 1.5e+11, 2e+11, 2.5e+11, 3.0e+11, 3.5e+11, 4e+11)) +
  scale_fill_manual(name = "New Entrant Type", values = c("#66CCCC", "#336666"), labels = c("Non-Graduated", "Graduated")) +
  ggtitle("Obligations for Graduated and Non-Graduated New Entrants - Navy")+
  geom_text(data = subset(FPDS_obligationscount_grad, registrationYear <= 2016), aes(label = scales::percent(percent_obl_dec)), size = 3, position = position_dodge(width = 1), vjust = -0.5) +
  facet_wrap(~graduated_10yr, ncol=1) + ##ncol=1 stack them above eachother (in 1 column)
  guides(fill=FALSE)

graduated_v_nongrad_navy


#******************************************************************************************************
#*******************#
#####NAVY STATS#####
#*******************#

#reorganized src code to match table format

#**********#
#####2001#####
#*********#

####Observations####
all_NE <- length(data_2001$top_smallbiz_bin)
all_NE
table(data_2001$top_smallbiz_bin)

####Survival rates - all####
##3-year##
table(data_2001$survive_3yr)

numerator_3yrALL_2001 <- length(which(data_2001$survive_3yr==1))
denominator_3yrALL_2001 <- length(data_2001$survive_3yr)

survival_3yrALL_2001 <- numerator_3yrALL_2001/denominator_3yrALL_2001
survival_3yrALL_2001

##5-year##
table(data_2001$survive_5yr)

numerator_5yrALL_2001 <- length(which(data_2001$survive_5yr==1))
denominator_5yrALL_2001 <- length(data_2001$survive_5yr)

survival_5yrALL_2001 <- numerator_5yrALL_2001/denominator_5yrALL_2001
survival_5yrALL_2001

##10-year##
table(data_2001$survive_10yr)

numerator_10yrALL_2001 <- length(which(data_2001$survive_10yr==1))
denominator_10yrALL_2001 <- length(data_2001$survive_10yr)

survival_10yrALL_2001 <- numerator_10yrALL_2001/denominator_10yrALL_2001
survival_10yrALL_2001

##2016##
table(data_2001$survive_2016)

numerator_2016_ALL_2001 <- length(which(data_2001$survive_2016==1))
denominator_2016_ALL_2001 <- length(data_2001$survive_2016)

survivalrate_2016_ALL_2001 <- numerator_2016_ALL_2001/denominator_2016_ALL_2001
survivalrate_2016_ALL_2001


####Survival rates - small####
##3-year##
numerator_3yrSM_2001 <- length(which(data_2001$survive_3yr==1 & data_2001$top_smallbiz_bin==1))
denominator_3yrSM_2001 <- length(which(data_2001$top_smallbiz_bin==1))

survival_3yrSM_2001 <- numerator_3yrSM_2001/denominator_3yrSM_2001
survival_3yrSM_2001

##5-year##
numerator_5yrSM_2001 <- length(which(data_2001$survive_5yr==1 & data_2001$top_smallbiz_bin==1))
denominator_5yrSM_2001 <- length(which(data_2001$top_smallbiz_bin==1))

survival_5yrSM_2001 <- numerator_5yrSM_2001/denominator_5yrSM_2001
survival_5yrSM_2001

##10-year##
numerator_10yrSM_2001 <- length(which(data_2001$survive_10yr==1 & data_2001$top_smallbiz_bin==1))
denominator_10yrSM_2001 <- length(which(data_2001$top_smallbiz_bin==1))

survival_10yrSM_2001 <- numerator_10yrSM_2001/denominator_10yrSM_2001
survival_10yrSM_2001

##2016##
numerator_2016_SM_2001 <- length(which(data_2001$survive_2016==1 & data_2001$top_smallbiz_bin==1))
denominator_2016_SM_2001 <- length(which(data_2001$top_smallbiz_bin==1))

survivalrate_2016_SM_2001 <- numerator_2016_SM_2001/denominator_2016_SM_2001
survivalrate_2016_SM_2001


####Survival rates - non-small####
##3-year##
numerator_3yrNSM_2001 <- length(which(data_2001$survive_3yr==1 & data_2001$top_smallbiz_bin==0))
denominator_3yrNSM_2001 <- length(which(data_2001$top_smallbiz_bin==0))

survival_3yrNSM_2001 <- numerator_3yrNSM_2001/denominator_3yrNSM_2001
survival_3yrNSM_2001

##5-year##
numerator_5yrNSM_2001 <- length(which(data_2001$survive_5yr==1 & data_2001$top_smallbiz_bin==0))
denominator_5yrNSM_2001 <- length(which(data_2001$top_smallbiz_bin==0))

survival_5yrNSM_2001 <- numerator_5yrNSM_2001/denominator_5yrNSM_2001
survival_5yrNSM_2001

##10-year##
numerator_10yrNSM_2001 <- length(which(data_2001$survive_10yr==1 & data_2001$top_smallbiz_bin==0))
denominator_10yrNSM_2001 <- length(which(data_2001$top_smallbiz_bin==0))

survival_10yrNSM_2001 <- numerator_10yrNSM_2001/denominator_10yrNSM_2001
survival_10yrNSM_2001

##2016##
numerator_2016_NS_2001 <- length(which(data_2001$survive_2016==1 & data_2001$top_smallbiz_bin==0))
denominator_2016_NS_2001 <- length(which(data_2001$top_smallbiz_bin==0))

survivalrate_2016_NS_2001 <- numerator_2016_NS_2001/denominator_2016_NS_2001
survivalrate_2016_NS_2001


####Graduation rates####
##for small firms that survived the ten years
numerator_gradALL_2001_10yr <- length(which(data_2001$graduated==1 & data_2001$survive_10yr==1))
denominator_gradALL_2001_10yr <- length(data_2001$graduated)

graduatedALL_2001_10yr <- numerator_gradALL_2001_10yr/denominator_gradALL_2001_10yr
graduatedALL_2001_10yr


####t-test####
#3-year#
table(data_2001$top_smallbiz_bin, data_2001$survive_3yr)
t.test(survive_3yr ~ top_smallbiz_bin, data = data_2001)

#5-year#
table(data_2001$top_smallbiz_bin, data_2001$survive_5yr)
t.test(survive_5yr ~ top_smallbiz_bin, data = data_2001)

#10-year#
table(data_2001$top_smallbiz_bin, data_2001$survive_10yr)
t.test(survive_10yr ~ top_smallbiz_bin, data = data_2001)

#2016#
table(data_2001$top_smallbiz_bin, data_2001$survive_2016)
t.test(survive_2016 ~ top_smallbiz_bin, data_2001)


#**********#
#####2002#####
#*********#

####Observations####
all_NE <- length(data_2002$top_smallbiz_bin)
all_NE
table(data_2002$top_smallbiz_bin)

####Survival rates - all####
##3-year##
table(data_2002$survive_3yr)

numerator_3yrALL_2002 <- length(which(data_2002$survive_3yr==1))
denominator_3yrALL_2002 <- length(data_2002$survive_3yr)

survival_3yrALL_2002 <- numerator_3yrALL_2002/denominator_3yrALL_2002
survival_3yrALL_2002

##5-year##
table(data_2002$survive_5yr)

numerator_5yrALL_2002 <- length(which(data_2002$survive_5yr==1))
denominator_5yrALL_2002 <- length(data_2002$survive_5yr)

survival_5yrALL_2002 <- numerator_5yrALL_2002/denominator_5yrALL_2002
survival_5yrALL_2002

##10-year##
table(data_2002$survive_10yr)

numerator_10yrALL_2002 <- length(which(data_2002$survive_10yr==1))
denominator_10yrALL_2002 <- length(data_2002$survive_10yr)

survival_10yrALL_2002 <- numerator_10yrALL_2002/denominator_10yrALL_2002
survival_10yrALL_2002

##2016##
table(data_2002$survive_2016)

numerator_2016_ALL_2002 <- length(which(data_2002$survive_2016==1))
denominator_2016_ALL_2002 <- length(data_2002$survive_2016)

survivalrate_2016_ALL_2002 <- numerator_2016_ALL_2002/denominator_2016_ALL_2002
survivalrate_2016_ALL_2002


####Survival rates - small####
##3-year##
numerator_3yrSM_2002 <- length(which(data_2002$survive_3yr==1 & data_2002$top_smallbiz_bin==1))
denominator_3yrSM_2002 <- length(which(data_2002$top_smallbiz_bin==1))

survival_3yrSM_2002 <- numerator_3yrSM_2002/denominator_3yrSM_2002
survival_3yrSM_2002

##5-year##
numerator_5yrSM_2002 <- length(which(data_2002$survive_5yr==1 & data_2002$top_smallbiz_bin==1))
denominator_5yrSM_2002 <- length(which(data_2002$top_smallbiz_bin==1))

survival_5yrSM_2002 <- numerator_5yrSM_2002/denominator_5yrSM_2002
survival_5yrSM_2002

##10-year##
numerator_10yrSM_2002 <- length(which(data_2002$survive_10yr==1 & data_2002$top_smallbiz_bin==1))
denominator_10yrSM_2002 <- length(which(data_2002$top_smallbiz_bin==1))

survival_10yrSM_2002 <- numerator_10yrSM_2002/denominator_10yrSM_2002
survival_10yrSM_2002

##2016##
numerator_2016_SM_2002 <- length(which(data_2002$survive_2016==1 & data_2002$top_smallbiz_bin==1))
denominator_2016_SM_2002 <- length(which(data_2002$top_smallbiz_bin==1))

survivalrate_2016_SM_2002 <- numerator_2016_SM_2002/denominator_2016_SM_2002
survivalrate_2016_SM_2002


####Survival rates - non-small####
##3-year##
numerator_3yrNSM_2002 <- length(which(data_2002$survive_3yr==1 & data_2002$top_smallbiz_bin==0))
denominator_3yrNSM_2002 <- length(which(data_2002$top_smallbiz_bin==0))

survival_3yrNSM_2002 <- numerator_3yrNSM_2002/denominator_3yrNSM_2002
survival_3yrNSM_2002

##5-year##
numerator_5yrNSM_2002 <- length(which(data_2002$survive_5yr==1 & data_2002$top_smallbiz_bin==0))
denominator_5yrNSM_2002 <- length(which(data_2002$top_smallbiz_bin==0))

survival_5yrNSM_2002 <- numerator_5yrNSM_2002/denominator_5yrNSM_2002
survival_5yrNSM_2002

##10-year##
numerator_10yrNSM_2002 <- length(which(data_2002$survive_10yr==1 & data_2002$top_smallbiz_bin==0))
denominator_10yrNSM_2002 <- length(which(data_2002$top_smallbiz_bin==0))

survival_10yrNSM_2002 <- numerator_10yrNSM_2002/denominator_10yrNSM_2002
survival_10yrNSM_2002

##2016##
numerator_2016_NS_2002 <- length(which(data_2002$survive_2016==1 & data_2002$top_smallbiz_bin==0))
denominator_2016_NS_2002 <- length(which(data_2002$top_smallbiz_bin==0))

survivalrate_2016_NS_2002 <- numerator_2016_NS_2002/denominator_2016_NS_2002
survivalrate_2016_NS_2002


####Graduation rates####
##for small firms that survived the ten years
numerator_gradALL_2002_10yr <- length(which(data_2002$graduated==1 & data_2002$survive_10yr==1))
denominator_gradALL_2002_10yr <- length(data_2002$graduated)

graduatedALL_2002_10yr <- numerator_gradALL_2002_10yr/denominator_gradALL_2002_10yr
graduatedALL_2002_10yr


####t-test####
#3-year#
table(data_2002$top_smallbiz_bin, data_2002$survive_3yr)
t.test(survive_3yr ~ top_smallbiz_bin, data = data_2002)

#5-year#
table(data_2002$top_smallbiz_bin, data_2002$survive_5yr)
t.test(survive_5yr ~ top_smallbiz_bin, data = data_2002)

#10-year#
table(data_2002$top_smallbiz_bin, data_2002$survive_10yr)
t.test(survive_10yr ~ top_smallbiz_bin, data = data_2002)

#2016#
table(data_2002$top_smallbiz_bin, data_2002$survive_2016)
t.test(survive_2016 ~ top_smallbiz_bin, data_2002)


#**********#
#####2003#####
#*********#

####Observations####
all_NE <- length(data_2003$top_smallbiz_bin)
all_NE
table(data_2003$top_smallbiz_bin)

####Survival rates - all####
##3-year##
table(data_2003$survive_3yr)

numerator_3yrALL_2003 <- length(which(data_2003$survive_3yr==1))
denominator_3yrALL_2003 <- length(data_2003$survive_3yr)

survival_3yrALL_2003 <- numerator_3yrALL_2003/denominator_3yrALL_2003
survival_3yrALL_2003

##5-year##
table(data_2003$survive_5yr)

numerator_5yrALL_2003 <- length(which(data_2003$survive_5yr==1))
denominator_5yrALL_2003 <- length(data_2003$survive_5yr)

survival_5yrALL_2003 <- numerator_5yrALL_2003/denominator_5yrALL_2003
survival_5yrALL_2003

##10-year##
table(data_2003$survive_10yr)

numerator_10yrALL_2003 <- length(which(data_2003$survive_10yr==1))
denominator_10yrALL_2003 <- length(data_2003$survive_10yr)

survival_10yrALL_2003 <- numerator_10yrALL_2003/denominator_10yrALL_2003
survival_10yrALL_2003

##2016##
table(data_2003$survive_2016)

numerator_2016_ALL_2003 <- length(which(data_2003$survive_2016==1))
denominator_2016_ALL_2003 <- length(data_2003$survive_2016)

survivalrate_2016_ALL_2003 <- numerator_2016_ALL_2003/denominator_2016_ALL_2003
survivalrate_2016_ALL_2003


####Survival rates - small####
##3-year##
numerator_3yrSM_2003 <- length(which(data_2003$survive_3yr==1 & data_2003$top_smallbiz_bin==1))
denominator_3yrSM_2003 <- length(which(data_2003$top_smallbiz_bin==1))

survival_3yrSM_2003 <- numerator_3yrSM_2003/denominator_3yrSM_2003
survival_3yrSM_2003

##5-year##
numerator_5yrSM_2003 <- length(which(data_2003$survive_5yr==1 & data_2003$top_smallbiz_bin==1))
denominator_5yrSM_2003 <- length(which(data_2003$top_smallbiz_bin==1))

survival_5yrSM_2003 <- numerator_5yrSM_2003/denominator_5yrSM_2003
survival_5yrSM_2003

##10-year##
numerator_10yrSM_2003 <- length(which(data_2003$survive_10yr==1 & data_2003$top_smallbiz_bin==1))
denominator_10yrSM_2003 <- length(which(data_2003$top_smallbiz_bin==1))

survival_10yrSM_2003 <- numerator_10yrSM_2003/denominator_10yrSM_2003
survival_10yrSM_2003

##2016##
numerator_2016_SM_2003 <- length(which(data_2003$survive_2016==1 & data_2003$top_smallbiz_bin==1))
denominator_2016_SM_2003 <- length(which(data_2003$top_smallbiz_bin==1))

survivalrate_2016_SM_2003 <- numerator_2016_SM_2003/denominator_2016_SM_2003
survivalrate_2016_SM_2003


####Survival rates - non-small####
##3-year##
numerator_3yrNSM_2003 <- length(which(data_2003$survive_3yr==1 & data_2003$top_smallbiz_bin==0))
denominator_3yrNSM_2003 <- length(which(data_2003$top_smallbiz_bin==0))

survival_3yrNSM_2003 <- numerator_3yrNSM_2003/denominator_3yrNSM_2003
survival_3yrNSM_2003

##5-year##
numerator_5yrNSM_2003 <- length(which(data_2003$survive_5yr==1 & data_2003$top_smallbiz_bin==0))
denominator_5yrNSM_2003 <- length(which(data_2003$top_smallbiz_bin==0))

survival_5yrNSM_2003 <- numerator_5yrNSM_2003/denominator_5yrNSM_2003
survival_5yrNSM_2003

##10-year##
numerator_10yrNSM_2003 <- length(which(data_2003$survive_10yr==1 & data_2003$top_smallbiz_bin==0))
denominator_10yrNSM_2003 <- length(which(data_2003$top_smallbiz_bin==0))

survival_10yrNSM_2003 <- numerator_10yrNSM_2003/denominator_10yrNSM_2003
survival_10yrNSM_2003

##2016##
numerator_2016_NS_2003 <- length(which(data_2003$survive_2016==1 & data_2003$top_smallbiz_bin==0))
denominator_2016_NS_2003 <- length(which(data_2003$top_smallbiz_bin==0))

survivalrate_2016_NS_2003 <- numerator_2016_NS_2003/denominator_2016_NS_2003
survivalrate_2016_NS_2003


####Graduation rates####
##for small firms that survived the ten years
numerator_gradALL_2003_10yr <- length(which(data_2003$graduated==1 & data_2003$survive_10yr==1))
denominator_gradALL_2003_10yr <- length(data_2003$graduated)

graduatedALL_2003_10yr <- numerator_gradALL_2003_10yr/denominator_gradALL_2003_10yr
graduatedALL_2003_10yr


####t-test####
#3-year#
table(data_2003$top_smallbiz_bin, data_2003$survive_3yr)
t.test(survive_3yr ~ top_smallbiz_bin, data = data_2003)

#5-year#
table(data_2003$top_smallbiz_bin, data_2003$survive_5yr)
t.test(survive_5yr ~ top_smallbiz_bin, data = data_2003)

#10-year#
table(data_2003$top_smallbiz_bin, data_2003$survive_10yr)
t.test(survive_10yr ~ top_smallbiz_bin, data = data_2003)

#2016#
table(data_2003$top_smallbiz_bin, data_2003$survive_2016)
t.test(survive_2016 ~ top_smallbiz_bin, data_2003)


#**********#
#####2004#####
#*********#

####Observations####
all_NE <- length(data_2004$top_smallbiz_bin)
all_NE
table(data_2004$top_smallbiz_bin)

####Survival rates - all####
##3-year##
table(data_2004$survive_3yr)

numerator_3yrALL_2004 <- length(which(data_2004$survive_3yr==1))
denominator_3yrALL_2004 <- length(data_2004$survive_3yr)

survival_3yrALL_2004 <- numerator_3yrALL_2004/denominator_3yrALL_2004
survival_3yrALL_2004

##5-year##
table(data_2004$survive_5yr)

numerator_5yrALL_2004 <- length(which(data_2004$survive_5yr==1))
denominator_5yrALL_2004 <- length(data_2004$survive_5yr)

survival_5yrALL_2004 <- numerator_5yrALL_2004/denominator_5yrALL_2004
survival_5yrALL_2004

##10-year##
table(data_2004$survive_10yr)

numerator_10yrALL_2004 <- length(which(data_2004$survive_10yr==1))
denominator_10yrALL_2004 <- length(data_2004$survive_10yr)

survival_10yrALL_2004 <- numerator_10yrALL_2004/denominator_10yrALL_2004
survival_10yrALL_2004

##2016##
table(data_2004$survive_2016)

numerator_2016_ALL_2004 <- length(which(data_2004$survive_2016==1))
denominator_2016_ALL_2004 <- length(data_2004$survive_2016)

survivalrate_2016_ALL_2004 <- numerator_2016_ALL_2004/denominator_2016_ALL_2004
survivalrate_2016_ALL_2004


####Survival rates - small####
##3-year##
numerator_3yrSM_2004 <- length(which(data_2004$survive_3yr==1 & data_2004$top_smallbiz_bin==1))
denominator_3yrSM_2004 <- length(which(data_2004$top_smallbiz_bin==1))

survival_3yrSM_2004 <- numerator_3yrSM_2004/denominator_3yrSM_2004
survival_3yrSM_2004

##5-year##
numerator_5yrSM_2004 <- length(which(data_2004$survive_5yr==1 & data_2004$top_smallbiz_bin==1))
denominator_5yrSM_2004 <- length(which(data_2004$top_smallbiz_bin==1))

survival_5yrSM_2004 <- numerator_5yrSM_2004/denominator_5yrSM_2004
survival_5yrSM_2004

##10-year##
numerator_10yrSM_2004 <- length(which(data_2004$survive_10yr==1 & data_2004$top_smallbiz_bin==1))
denominator_10yrSM_2004 <- length(which(data_2004$top_smallbiz_bin==1))

survival_10yrSM_2004 <- numerator_10yrSM_2004/denominator_10yrSM_2004
survival_10yrSM_2004

##2016##
numerator_2016_SM_2004 <- length(which(data_2004$survive_2016==1 & data_2004$top_smallbiz_bin==1))
denominator_2016_SM_2004 <- length(which(data_2004$top_smallbiz_bin==1))

survivalrate_2016_SM_2004 <- numerator_2016_SM_2004/denominator_2016_SM_2004
survivalrate_2016_SM_2004


####Survival rates - non-small####
##3-year##
numerator_3yrNSM_2004 <- length(which(data_2004$survive_3yr==1 & data_2004$top_smallbiz_bin==0))
denominator_3yrNSM_2004 <- length(which(data_2004$top_smallbiz_bin==0))

survival_3yrNSM_2004 <- numerator_3yrNSM_2004/denominator_3yrNSM_2004
survival_3yrNSM_2004

##5-year##
numerator_5yrNSM_2004 <- length(which(data_2004$survive_5yr==1 & data_2004$top_smallbiz_bin==0))
denominator_5yrNSM_2004 <- length(which(data_2004$top_smallbiz_bin==0))

survival_5yrNSM_2004 <- numerator_5yrNSM_2004/denominator_5yrNSM_2004
survival_5yrNSM_2004

##10-year##
numerator_10yrNSM_2004 <- length(which(data_2004$survive_10yr==1 & data_2004$top_smallbiz_bin==0))
denominator_10yrNSM_2004 <- length(which(data_2004$top_smallbiz_bin==0))

survival_10yrNSM_2004 <- numerator_10yrNSM_2004/denominator_10yrNSM_2004
survival_10yrNSM_2004

##2016##
numerator_2016_NS_2004 <- length(which(data_2004$survive_2016==1 & data_2004$top_smallbiz_bin==0))
denominator_2016_NS_2004 <- length(which(data_2004$top_smallbiz_bin==0))

survivalrate_2016_NS_2004 <- numerator_2016_NS_2004/denominator_2016_NS_2004
survivalrate_2016_NS_2004


####Graduation rates####
##for small firms that survived the ten years
numerator_gradALL_2004_10yr <- length(which(data_2004$graduated==1 & data_2004$survive_10yr==1))
denominator_gradALL_2004_10yr <- length(data_2004$graduated)

graduatedALL_2004_10yr <- numerator_gradALL_2004_10yr/denominator_gradALL_2004_10yr
graduatedALL_2004_10yr


####t-test####
#3-year#
table(data_2004$top_smallbiz_bin, data_2004$survive_3yr)
t.test(survive_3yr ~ top_smallbiz_bin, data = data_2004)

#5-year#
table(data_2004$top_smallbiz_bin, data_2004$survive_5yr)
t.test(survive_5yr ~ top_smallbiz_bin, data = data_2004)

#10-year#
table(data_2004$top_smallbiz_bin, data_2004$survive_10yr)
t.test(survive_10yr ~ top_smallbiz_bin, data = data_2004)

#2016#
table(data_2004$top_smallbiz_bin, data_2004$survive_2016)
t.test(survive_2016 ~ top_smallbiz_bin, data_2004)


#**********#
#####2005#####
#*********#

####Observations####
all_NE <- length(data_2005$top_smallbiz_bin)
all_NE
table(data_2005$top_smallbiz_bin)

####Survival rates - all####
##3-year##
table(data_2005$survive_3yr)

numerator_3yrALL_2005 <- length(which(data_2005$survive_3yr==1))
denominator_3yrALL_2005 <- length(data_2005$survive_3yr)

survival_3yrALL_2005 <- numerator_3yrALL_2005/denominator_3yrALL_2005
survival_3yrALL_2005

##5-year##
table(data_2005$survive_5yr)

numerator_5yrALL_2005 <- length(which(data_2005$survive_5yr==1))
denominator_5yrALL_2005 <- length(data_2005$survive_5yr)

survival_5yrALL_2005 <- numerator_5yrALL_2005/denominator_5yrALL_2005
survival_5yrALL_2005

##10-year##
table(data_2005$survive_10yr)

numerator_10yrALL_2005 <- length(which(data_2005$survive_10yr==1))
denominator_10yrALL_2005 <- length(data_2005$survive_10yr)

survival_10yrALL_2005 <- numerator_10yrALL_2005/denominator_10yrALL_2005
survival_10yrALL_2005

##2016##
table(data_2005$survive_2016)

numerator_2016_ALL_2005 <- length(which(data_2005$survive_2016==1))
denominator_2016_ALL_2005 <- length(data_2005$survive_2016)

survivalrate_2016_ALL_2005 <- numerator_2016_ALL_2005/denominator_2016_ALL_2005
survivalrate_2016_ALL_2005


####Survival rates - small####
##3-year##
numerator_3yrSM_2005 <- length(which(data_2005$survive_3yr==1 & data_2005$top_smallbiz_bin==1))
denominator_3yrSM_2005 <- length(which(data_2005$top_smallbiz_bin==1))

survival_3yrSM_2005 <- numerator_3yrSM_2005/denominator_3yrSM_2005
survival_3yrSM_2005

##5-year##
numerator_5yrSM_2005 <- length(which(data_2005$survive_5yr==1 & data_2005$top_smallbiz_bin==1))
denominator_5yrSM_2005 <- length(which(data_2005$top_smallbiz_bin==1))

survival_5yrSM_2005 <- numerator_5yrSM_2005/denominator_5yrSM_2005
survival_5yrSM_2005

##10-year##
numerator_10yrSM_2005 <- length(which(data_2005$survive_10yr==1 & data_2005$top_smallbiz_bin==1))
denominator_10yrSM_2005 <- length(which(data_2005$top_smallbiz_bin==1))

survival_10yrSM_2005 <- numerator_10yrSM_2005/denominator_10yrSM_2005
survival_10yrSM_2005

##2016##
numerator_2016_SM_2005 <- length(which(data_2005$survive_2016==1 & data_2005$top_smallbiz_bin==1))
denominator_2016_SM_2005 <- length(which(data_2005$top_smallbiz_bin==1))

survivalrate_2016_SM_2005 <- numerator_2016_SM_2005/denominator_2016_SM_2005
survivalrate_2016_SM_2005


####Survival rates - non-small####
##3-year##
numerator_3yrNSM_2005 <- length(which(data_2005$survive_3yr==1 & data_2005$top_smallbiz_bin==0))
denominator_3yrNSM_2005 <- length(which(data_2005$top_smallbiz_bin==0))

survival_3yrNSM_2005 <- numerator_3yrNSM_2005/denominator_3yrNSM_2005
survival_3yrNSM_2005

##5-year##
numerator_5yrNSM_2005 <- length(which(data_2005$survive_5yr==1 & data_2005$top_smallbiz_bin==0))
denominator_5yrNSM_2005 <- length(which(data_2005$top_smallbiz_bin==0))

survival_5yrNSM_2005 <- numerator_5yrNSM_2005/denominator_5yrNSM_2005
survival_5yrNSM_2005

##10-year##
numerator_10yrNSM_2005 <- length(which(data_2005$survive_10yr==1 & data_2005$top_smallbiz_bin==0))
denominator_10yrNSM_2005 <- length(which(data_2005$top_smallbiz_bin==0))

survival_10yrNSM_2005 <- numerator_10yrNSM_2005/denominator_10yrNSM_2005
survival_10yrNSM_2005

##2016##
numerator_2016_NS_2005 <- length(which(data_2005$survive_2016==1 & data_2005$top_smallbiz_bin==0))
denominator_2016_NS_2005 <- length(which(data_2005$top_smallbiz_bin==0))

survivalrate_2016_NS_2005 <- numerator_2016_NS_2005/denominator_2016_NS_2005
survivalrate_2016_NS_2005


####Graduation rates####
##for small firms that survived the ten years
numerator_gradALL_2005_10yr <- length(which(data_2005$graduated==1 & data_2005$survive_10yr==1))
denominator_gradALL_2005_10yr <- length(data_2005$graduated)

graduatedALL_2005_10yr <- numerator_gradALL_2005_10yr/denominator_gradALL_2005_10yr
graduatedALL_2005_10yr


####t-test####
#3-year#
table(data_2005$top_smallbiz_bin, data_2005$survive_3yr)
t.test(survive_3yr ~ top_smallbiz_bin, data = data_2005)

#5-year#
table(data_2005$top_smallbiz_bin, data_2005$survive_5yr)
t.test(survive_5yr ~ top_smallbiz_bin, data = data_2005)

#10-year#
table(data_2005$top_smallbiz_bin, data_2005$survive_10yr)
t.test(survive_10yr ~ top_smallbiz_bin, data = data_2005)

#2016#
table(data_2005$top_smallbiz_bin, data_2005$survive_2016)
t.test(survive_2016 ~ top_smallbiz_bin, data_2005)


#**********#
#####2006#####
#*********#

####Observations####
all_NE <- length(data_2006$top_smallbiz_bin)
all_NE
table(data_2006$top_smallbiz_bin)

####Survival rates - all####
##3-year##
table(data_2006$survive_3yr)

numerator_3yrALL_2006 <- length(which(data_2006$survive_3yr==1))
denominator_3yrALL_2006 <- length(data_2006$survive_3yr)

survival_3yrALL_2006 <- numerator_3yrALL_2006/denominator_3yrALL_2006
survival_3yrALL_2006

##5-year##
table(data_2006$survive_5yr)

numerator_5yrALL_2006 <- length(which(data_2006$survive_5yr==1))
denominator_5yrALL_2006 <- length(data_2006$survive_5yr)

survival_5yrALL_2006 <- numerator_5yrALL_2006/denominator_5yrALL_2006
survival_5yrALL_2006

##10-year##
table(data_2006$survive_10yr)

numerator_10yrALL_2006 <- length(which(data_2006$survive_10yr==1))
denominator_10yrALL_2006 <- length(data_2006$survive_10yr)

survival_10yrALL_2006 <- numerator_10yrALL_2006/denominator_10yrALL_2006
survival_10yrALL_2006

##2016##
table(data_2006$survive_2016)

numerator_2016_ALL_2006 <- length(which(data_2006$survive_2016==1))
denominator_2016_ALL_2006 <- length(data_2006$survive_2016)

survivalrate_2016_ALL_2006 <- numerator_2016_ALL_2006/denominator_2016_ALL_2006
survivalrate_2016_ALL_2006


####Survival rates - small####
##3-year##
numerator_3yrSM_2006 <- length(which(data_2006$survive_3yr==1 & data_2006$top_smallbiz_bin==1))
denominator_3yrSM_2006 <- length(which(data_2006$top_smallbiz_bin==1))

survival_3yrSM_2006 <- numerator_3yrSM_2006/denominator_3yrSM_2006
survival_3yrSM_2006

##5-year##
numerator_5yrSM_2006 <- length(which(data_2006$survive_5yr==1 & data_2006$top_smallbiz_bin==1))
denominator_5yrSM_2006 <- length(which(data_2006$top_smallbiz_bin==1))

survival_5yrSM_2006 <- numerator_5yrSM_2006/denominator_5yrSM_2006
survival_5yrSM_2006

##10-year##
numerator_10yrSM_2006 <- length(which(data_2006$survive_10yr==1 & data_2006$top_smallbiz_bin==1))
denominator_10yrSM_2006 <- length(which(data_2006$top_smallbiz_bin==1))

survival_10yrSM_2006 <- numerator_10yrSM_2006/denominator_10yrSM_2006
survival_10yrSM_2006

##2016##
numerator_2016_SM_2006 <- length(which(data_2006$survive_2016==1 & data_2006$top_smallbiz_bin==1))
denominator_2016_SM_2006 <- length(which(data_2006$top_smallbiz_bin==1))

survivalrate_2016_SM_2006 <- numerator_2016_SM_2006/denominator_2016_SM_2006
survivalrate_2016_SM_2006


####Survival rates - non-small####
##3-year##
numerator_3yrNSM_2006 <- length(which(data_2006$survive_3yr==1 & data_2006$top_smallbiz_bin==0))
denominator_3yrNSM_2006 <- length(which(data_2006$top_smallbiz_bin==0))

survival_3yrNSM_2006 <- numerator_3yrNSM_2006/denominator_3yrNSM_2006
survival_3yrNSM_2006

##5-year##
numerator_5yrNSM_2006 <- length(which(data_2006$survive_5yr==1 & data_2006$top_smallbiz_bin==0))
denominator_5yrNSM_2006 <- length(which(data_2006$top_smallbiz_bin==0))

survival_5yrNSM_2006 <- numerator_5yrNSM_2006/denominator_5yrNSM_2006
survival_5yrNSM_2006

##10-year##
numerator_10yrNSM_2006 <- length(which(data_2006$survive_10yr==1 & data_2006$top_smallbiz_bin==0))
denominator_10yrNSM_2006 <- length(which(data_2006$top_smallbiz_bin==0))

survival_10yrNSM_2006 <- numerator_10yrNSM_2006/denominator_10yrNSM_2006
survival_10yrNSM_2006

##2016##
numerator_2016_NS_2006 <- length(which(data_2006$survive_2016==1 & data_2006$top_smallbiz_bin==0))
denominator_2016_NS_2006 <- length(which(data_2006$top_smallbiz_bin==0))

survivalrate_2016_NS_2006 <- numerator_2016_NS_2006/denominator_2016_NS_2006
survivalrate_2016_NS_2006


####Graduation rates####
##for small firms that survived the ten years
numerator_gradALL_2006_10yr <- length(which(data_2006$graduated==1 & data_2006$survive_10yr==1))
denominator_gradALL_2006_10yr <- length(data_2006$graduated)

graduatedALL_2006_10yr <- numerator_gradALL_2006_10yr/denominator_gradALL_2006_10yr
graduatedALL_2006_10yr


####t-test####
#3-year#
table(data_2006$top_smallbiz_bin, data_2006$survive_3yr)
t.test(survive_3yr ~ top_smallbiz_bin, data = data_2006)

#5-year#
table(data_2006$top_smallbiz_bin, data_2006$survive_5yr)
t.test(survive_5yr ~ top_smallbiz_bin, data = data_2006)

#10-year#
table(data_2006$top_smallbiz_bin, data_2006$survive_10yr)
t.test(survive_10yr ~ top_smallbiz_bin, data = data_2006)

#2016#
table(data_2006$top_smallbiz_bin, data_2006$survive_2016)
t.test(survive_2016 ~ top_smallbiz_bin, data_2006)