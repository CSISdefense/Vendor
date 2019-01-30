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

#setwd("K:/2018-01 NPS New Entrants/Data/Data/Cleaned Data/FPDS")

##sam work computer
setwd("K:/2018-01 NPS New Entrants/Data/Data/Cleaning data/FPDS")

##sam laptop
#setwd("/Users/samanthacohen/Desktop/Diig backup/New Entrants/R Data")

load(file = "FPDS_datapull_all_v3_allfed.Rda")
length(unique(FPDS_cleaned_unique$Dunsnumber)) == nrow(FPDS_cleaned_unique)

#******************************************************************
####Count number of new entrants in each year! ####
#******************************************************************
table(FPDS_cleaned_unique$customer)

##fpds data
registrationyear_count <- table(SAM_and_FPDS_uniqueDuns$registrationYear)

registrationyear_count


#******************************#
####bar graph for FPDS Data####
#******************************#

##drop observations with Registration Year before 2001
FPDS_cleaned_unique <- FPDS_cleaned_unique[!(FPDS_cleaned_unique$registrationYear<2001), ]
FPDS_cleaned_unique <- FPDS_cleaned_unique[!(FPDS_cleaned_unique$registrationYear>2016), ]

length(unique(FPDS_cleaned_unique$Dunsnumber)) == nrow(FPDS_cleaned_unique)

##find out which are duplicates
n_occur <- data.frame(table(FPDS_cleaned_unique$Dunsnumber)) ##gives a data frame with a list of duns and the number of times they occurred

n_occur[n_occur$Freq > 1, ]

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


NE_count_allfed <- ggplot(FPDS_bargraphCount, aes(x = registrationYear, y = regpersize, fill = factor(top_smallbiz_bin), label = regperyear)) +
  geom_bar(stat = 'identity', position = 'stack') +
  ylab("Number of New Entrants") +
  xlab("Registration Year") +
  scale_x_continuous(breaks = c(2001:2016)) +
  ##scale_fill_manual(name = "New Entrants Types", values = c("deepskyblue", "royalblue1"), labels = c("small", "non-small")) +
  scale_fill_manual(name = "New Entrants Types", values = c("darkslategray1", "cadetblue4"), labels = c("non-small", "small")) +
  ggtitle("Number of New Entrants Per Year (2001-2016) - All Federal Agencies")+
  ##geom_text_repel(data = subset(FPDS_bargraphCount, registrationYear >=2014), aes(label = regpersize), size = 4, box.padding = .1, 
  ###    angle = 45) +
  ##geom_text(data = subset(FPDS_bargraphCount, registrationYear < 2014), aes(label = regpersize), size = 4, position = position_stack(vjust = .5), angle = 45)
  geom_text(data = subset(FPDS_bargraphCount, registrationYear <= 2016), aes(label = regpersize), size = 4, position = position_stack(vjust = .5), angle = 45)

#******************************
##chart for DoD
#******************************
FPDS_cleaned_unique_DOD <- FPDS_cleaned_unique[(FPDS_cleaned_unique$customer=="Defense"), ]

totyear_count <- FPDS_cleaned_unique_DOD %>% 
  filter(top_smallbiz_bin == 1 | top_smallbiz_bin == 0) %>% 
  group_by(registrationYear) %>% 
  dplyr::summarise(n())  


FPDS_bargraphCount <- FPDS_cleaned_unique_DOD %>%
  filter(top_smallbiz_bin == 1 | top_smallbiz_bin == 0) %>%
  group_by(registrationYear, top_smallbiz_bin) %>%
  dplyr::summarise(n()) %>%
  dplyr::rename("regpersize"=`n()`) %>%
  left_join(totyear_count, by = "registrationYear") %>%
  dplyr::rename("regperyear"=`n()`) 


NE_count_DoD <- ggplot(FPDS_bargraphCount, aes(x = registrationYear, y = regpersize, fill = factor(top_smallbiz_bin), label = regperyear)) +
  geom_bar(stat = 'identity', position = 'stack') +
  ylab("Number of New Entrants") +
  xlab("Registration Year") +
  scale_x_continuous(breaks = c(2001:2016)) +
  ##scale_fill_manual(name = "New Entrants Types", values = c("deepskyblue", "royalblue1"), labels = c("small", "non-small")) +
  scale_fill_manual(name = "New Entrants Types", values = c("darkslategray1", "cadetblue4"), labels = c("non-small", "small")) +
  ggtitle("Number of New Entrants Per Year (2001-2016) - DoD")+
  ##geom_text_repel(data = subset(FPDS_bargraphCount, registrationYear >=2014), aes(label = regpersize), size = 4, box.padding = .1, 
                  ##angle = 45) +
  ##geom_text(data = subset(FPDS_bargraphCount, registrationYear < 2014), aes(label = regpersize), size = 4, position = position_stack(vjust = .5), angle = 45)
  geom_text(data = subset(FPDS_bargraphCount, registrationYear <= 2016), aes(label = regpersize), size = 4, position = position_stack(vjust = .5), angle = 45)

##combine graphs

grid.arrange(NE_count_allfed, NE_count_DoD)


#******************************************************************************************************

#*****************************************************************#
####Calculate the survival and graduation rates using FPDS Data for all federal agencies####
#*****************************************************************#


#create necessary vars#

####2001 all fed agencies####
#************#
#survival vars
#************#

#subset the 2001 data
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

##make survival vars numeric##
str(data_2001)

data_2001$survive_3yr<-as.numeric(as.character(data_2001$survive_3yr))
data_2001$survive_5yr<-as.numeric(as.character(data_2001$survive_5yr))
data_2001$survive_10yr<-as.numeric(as.character(data_2001$survive_10yr))
data_2001$survive_2016<-as.numeric(as.character(data_2001$survive_2016))

str(data_2001)


##t-test between small and nonsmall survival rates##
#3-year#
table(data_2001$top_smallbiz_bin)
table(data_2001$survive_3yr)
table(data_2001$top_smallbiz_bin, data_2001$survive_3yr)

all_NE <- length(data_2001$top_smallbiz_bin)
all_NE
t.test(survive_3yr ~ top_smallbiz_bin, data = data_2001)

#5-year#
table(data_2001$top_smallbiz_bin, data_2001$survive_5yr)

t.test(survive_5yr ~ top_smallbiz_bin, data = data_2001)

#10-year#
table(data_2001$top_smallbiz_bin, data_2001$survive_10yr)

t.test(survive_10yr ~ top_smallbiz_bin, data = data_2001)

##2016
t.test(survive_2016 ~ top_smallbiz_bin, data_2001)


#*****************
#Survival#
#*****************
#************#
#*****ALL****#
#************#
##3-year##
table(data_2001$survive_3yr) #0=10168, 1=17266

numerator_3yrALL_2001 <- length(which(data_2001$survive_3yr==1))

denominator_3yrALL_2001 <- length(data_2001$survive_3yr)

survival_3yrALL_2001 <- numerator_3yrALL_2001/denominator_3yrALL_2001
survival_3yrALL_2001

##5-year##
table(data_2001$survive_5yr) #0=10168, 1=17266

numerator_5yrALL_2001 <- length(which(data_2001$survive_5yr==1))

denominator_5yrALL_2001 <- length(data_2001$survive_5yr)

survival_5yrALL_2001 <- numerator_5yrALL_2001/denominator_5yrALL_2001
survival_5yrALL_2001

##10-year##
table(data_2001$survive_10yr) #0=10168, 1=17266

numerator_10yrALL_2001 <- length(which(data_2001$survive_10yr==1))

denominator_10yrALL_2001 <- length(data_2001$survive_10yr)

survival_10yrALL_2001 <- numerator_10yrALL_2001/denominator_10yrALL_2001
survival_10yrALL_2001

#**********#
#Graduation#
#**********#

table(data_2001$graduated)
table(data_2001$top_smallbiz_bin)
table(data_2001$survive_10yr)

##for all firms, whether or not they survived after 10 years

numerator_gradALL_2001 <- length(which(data_2001$graduated==1))

denominator_gradALL_2001 <- length(data_2001$graduated)

graduatedALL_2001 <- numerator_gradALL_2001/denominator_gradALL_2001
graduatedALL_2001

##for only firms that survived the ten years
numerator_gradALL_2001_10yr <- length(which(data_2001$graduated==1 & data_2001$survive_10yr==1))

#denominator_gradALL_2001_10yr <- length(data_2001$graduated)
wrong <- length(data_2001$graduated)

denominator_gradALL_2001_10yr <- length(which(data_2001$top_smallbiz_bin==1))

denominator_gradALL_2001_10yr_v2 <- length(data_2001_smallbiz$graduated)


graduatedALL_2001_10yr <- numerator_gradALL_2001_10yr/denominator_gradALL_2001_10yr
graduatedALL_2001_10yr

graduated_ALL_2001_10yr_v2 <- numerator_gradALL_2001_10yr/denominator_gradALL_2001_10yr_v2
graduated_ALL_2001_10yr_v2

graduation_wrong <- numerator_gradALL_2001_10yr/wrong
graduation_wrong
#**************************
#**********#
#***SMALL**#
#**********#
##3-year##
table(data_2001$survive_3yr) #0=10168, 1=17266

numerator_3yrSM_2001 <- length(which(data_2001$survive_3yr==1 & data_2001$top_smallbiz_bin==1))
numerator_3yrSM_2001

denominator_3yrSM_2001 <- length(which(data_2001$top_smallbiz_bin==1))

survival_3yrSM_2001 <- numerator_3yrSM_2001/denominator_3yrSM_2001
survival_3yrSM_2001

##5-year##
table(data_2001$survive_5yr) #0=10168, 1=17266

numerator_5yrSM_2001 <- length(which(data_2001$survive_5yr==1 & data_2001$top_smallbiz_bin==1))

denominator_5yrSM_2001 <- length(which(data_2001$top_smallbiz_bin==1))

survival_5yrSM_2001 <- numerator_5yrSM_2001/denominator_5yrSM_2001
survival_5yrSM_2001

##10-year##
table(data_2001$survive_10yr) #0=10168, 1=17266

numerator_10yrSM_2001 <- length(which(data_2001$survive_10yr==1 & data_2001$top_smallbiz_bin==1))

denominator_10yrSM_2001 <- length(which(data_2001$top_smallbiz_bin==1))

survival_10yrSM_2001 <- numerator_10yrSM_2001/denominator_10yrSM_2001
survival_10yrSM_2001


#**********#
#***NON-SMALL**#
#**********#

##3-year##
table(data_2001$survive_3yr) #0=10168, 1=17266

numerator_3yrNSM_2001 <- length(which(data_2001$survive_3yr==1 & data_2001$top_smallbiz_bin==0))

denominator_3yrNSM_2001 <- length(which(data_2001$top_smallbiz_bin==0))

survival_3yrNSM_2001 <- numerator_3yrNSM_2001/denominator_3yrNSM_2001
survival_3yrNSM_2001

##5-year##
table(data_2001$survive_5yr) #0=10168, 1=17266

numerator_5yrNSM_2001 <- length(which(data_2001$survive_5yr==1 & data_2001$top_smallbiz_bin==0))

denominator_5yrNSM_2001 <- length(which(data_2001$top_smallbiz_bin==0))

survival_5yrNSM_2001 <- numerator_5yrNSM_2001/denominator_5yrNSM_2001
survival_5yrNSM_2001

##10-year##
table(data_2001$survive_10yr) #0=10168, 1=17266

numerator_10yrNSM_2001 <- length(which(data_2001$survive_10yr==1 & data_2001$top_smallbiz_bin==0))

denominator_10yrNSM_2001 <- length(which(data_2001$top_smallbiz_bin==0))

survival_10yrNSM_2001 <- numerator_10yrNSM_2001/denominator_10yrNSM_2001
survival_10yrNSM_2001
 
##t-test for the differences between small and non-small survivors
t.test(survive_10yr ~ top_smallbiz_bin, data = data_2001)

#*******************#
#******2016 check***#
#*******************#
##*********
#***2016 survival 
#**********
survive_10yr_count_2001 <- length(which(data_2001$survive_10yr==1))
survive_2016_count_2001 <- length(which(data_2001$survive_10yr==1))
##ALL##
numerator_2016_ALL_2001 <- length(which(data_2001$survive_2016==1))

denominator_2016_ALL_2001 <- length(data_2001$survive_2016)

survivalrate_2016_ALL_2001 <- numerator_2016_ALL_2001/denominator_2016_ALL_2001
survivalrate_2016_ALL_2001

##check
data_2001_survive2016 <- data_2001[!(data_2001$exitYear<"2016"), ] 

numerator_2016_ALL_2001_v2 <- length(data_2001_survive2016$top_smallbiz_bin)

denominator_2016_ALL_2001_v2 <- length(data_2001$top_smallbiz_bin)

survivalrate_2016_ALL_2001_v2 <- numerator_2016_ALL_2001_v2 / denominator_2016_ALL_2001_v2
survivalrate_2016_ALL_2001_v2


##SMALL##
numerator_2016_SM_2001 <- length(which(data_2001$survive_2016==1 & data_2001$top_smallbiz_bin==1))

denominator_2016_SM_2001 <- length(which(data_2001$top_smallbiz_bin==1))

survivalrate_2016_SM_2001 <- numerator_2016_SM_2001/denominator_2016_SM_2001
survivalrate_2016_SM_2001


##NONSMALL##
numerator_2016_NS_2001 <- length(which(data_2001$survive_2016==1 & data_2001$top_smallbiz_bin==0))

denominator_2016_NS_2001 <- length(which(data_2001$top_smallbiz_bin==0))

survivalrate_2016_NS_2001 <- numerator_2016_NS_2001/denominator_2016_NS_2001
survivalrate_2016_NS_2001


##**************##
####Pie Charts for 2001 check####
#****************#

##******##
##ALL NE##
##******##
pie_2001_ALL_df <- data.frame(
  group = c("3-yr", "5-yr", "10-yr", "2016"),
  value = c(survival_3yrALL_2001, survival_5yrALL_2001, survival_10yrALL_2001, survivalrate_2016_ALL_2001)
)

head(pie_2001_ALL_df)

pie_2001_ALL_df <- pie_2001_ALL_df %>%
  dplyr::mutate(perc_value = value * 100) %>%
  dplyr::mutate(perc_value = round(perc_value, 0))

head(pie_2001_ALL_df)
  
plot_2001_ALL <- ggplot(pie_2001_ALL_df, aes(x=group, y=perc_value, fill=group)) +
  scale_fill_manual(values=c("#99FFFF", "#0099CC", "#006699", "#999999" )) +
  geom_bar(stat = "identity", width=0.4) +
  scale_x_discrete(limits=c("3-yr", "5-yr", "10-yr", "2016")) +
  ylim(c(0, 70)) +
  geom_text(aes(label=percent(value)), size=3, vjust=-.25) +
  labs(title = "-All New Entrants All Fed", y = "Survival Rates (percentages)", x = "") + 
  guides(fill=FALSE)
  
  
##******##
##SMALL NE##
##******##
pie_2001_SM_df <- data.frame(
  group = c("3-yr", "5-yr", "10-yr", "2016", "Graduated"),
  value = c(survival_3yrSM_2001, survival_5yrSM_2001, survival_10yrSM_2001, survivalrate_2016_SM_2001, graduatedALL_2001_10yr)
)

head(pie_2001_SM_df)

pie_2001_SM_df <- pie_2001_SM_df %>%
  dplyr::mutate(perc_value = value * 100) %>%
  dplyr::mutate(perc_value = round(perc_value, 0))

head(pie_2001_SM_df)

plot_2001_SMALL <- ggplot(pie_2001_SM_df, aes(x=group, y=perc_value, fill=group)) +
  scale_fill_manual(values=c("#99FFFF", "#0099CC", "#006699", "#999999", "#000066" )) +
  geom_bar(stat = "identity", width=0.4) +
  scale_x_discrete(limits=c("3-yr", "5-yr", "10-yr", "2016", "Graduated")) +
  ylim(c(0, 70)) +
  geom_text(aes(label=percent(value)), vjust=-.25, size=3) +
  labs(title = "Survival Rates 2001 All Federal Agencies - Small Businesses", y = "Survival Rates (percentages)", x = "Year") + 
  guides(fill=FALSE)

##******##
##Non-Small NE##
##******##
pie_2001_NS_df <- data.frame(
  group = c("3-yr", "5-yr", "10-yr", "2016"),
  value = c(survival_3yrNSM_2001, survival_5yrNSM_2001, survival_10yrNSM_2001, survivalrate_2016_NS_2001)
)

head(pie_2001_NS_df)

pie_2001_NS_df <- pie_2001_NS_df %>%
  dplyr::mutate(perc_value = value * 100) %>%
  dplyr::mutate(perc_value = round(perc_value, 0))

head(pie_2001_NS_df)

plot_2001_NS <- ggplot(pie_2001_NS_df, aes(x=group, y=perc_value, fill=group)) +
  scale_fill_manual(values=c("#99FFFF", "#0099CC", "#006699", "#999999")) +
  geom_bar(stat = "identity", width=0.4) +
  scale_x_discrete(limits=c("3-yr", "5-yr", "10-yr", "2016")) +
  ylim(c(0, 70)) +
  geom_text(aes(label=percent(value)), vjust=-.25, size=3) +
  labs(title = "- Non-Small Businesses All Fed", y = "Survival Rates (percentages)", x = "Year") +
  guides(fill=FALSE)

##*****##
#combine plots#
#******##
#t <- textGrob("Survival Rates 2001 Sample", fontsize=42)
grid.arrange(plot_2001_SMALL, plot_2001_ALL, plot_2001_NS)


##*****************##
#**pie for graduation**#
#******************##
##make variables##
##*****************##
#**pie for graduation**#
#******************##
##make variables##
pie_2001_non_grad <- length(which(data_2001$survive_10yr==1 & data_2001$graduated==0))
pie_2001_grad <- length(which(data_2001$graduated==1 & data_2001$survive_10yr==1))
total_2001_10yrsurv <- length(which(data_2001$survive_10yr==1))

shouldbtot <- pie_2001_non_grad + pie_2001_grad

shouldbtot

pie_2001_graduation <- data.frame(
  group = c( "Graduated", "Non-Graduated"),
  value = c(pie_2001_grad, pie_2001_non_grad)
)


pie_2001_graduation <- pie_2001_graduation %>%
  dplyr::mutate(total_2001_10yrsurv = 9411) %>%
  dplyr::mutate(dec = value / total_2001_10yrsurv) %>%
  dplyr::mutate(perc = dec * 100) %>%
  dplyr::mutate(perc_value = round(perc, 0))


head(pie_2001_graduation)

pie_bar_2001 <- ggplot(pie_2001_graduation, aes(x="", y=value, fill=group)) +
  geom_bar(width = 1, stat = "identity") +
  geom_text(position = "stack", aes(label=percent(dec)), vjust=5, size=4)

pie_bar_2001

pie_bar_2001 + coord_polar(theta = "y", start = 0, direction = 1) #+
#geom_text(aes(y = dec, label = percent(dec)), vjust=0, size=5) 

#*************************************************************************************
  
#****************#
####2001 DOD only#####
#****************#
#subset the 2001 data
data_DOD_2001 <- FPDS_cleaned_unique[!(FPDS_cleaned_unique$registrationYear!="2001"), ]
data_DOD_2001 <- data_DOD_2001[!(data_DOD_2001$customer!="Defense"), ]

##create variable describing whether a firm survived 3 years

data_DOD_2001 <- data_DOD_2001 %>%
  dplyr::mutate(survive_3yr = ifelse(exitYear < 2003, "0", "1")) 

##create variable describing whether a firm survived 5 years
data_DOD_2001 <- data_DOD_2001 %>%
  dplyr::mutate(survive_5yr = ifelse(exitYear < 2005, "0", "1")) 


##create variable describing whether a firm survived 10 years
data_DOD_2001 <- data_DOD_2001 %>%
  dplyr::mutate(survive_10yr = ifelse(exitYear < 2010, "0", "1")) 

##create variable describing whether a firm survived in 2016
data_DOD_2001 <- data_DOD_2001 %>%
  dplyr::mutate(survive_2016 = ifelse(exitYear < 2016, "0", "1"))


str(data_DOD_2001)

data_DOD_2001$survive_3yr<-as.numeric(as.character(data_DOD_2001$survive_3yr))
data_DOD_2001$survive_5yr<-as.numeric(as.character(data_DOD_2001$survive_5yr))
data_DOD_2001$survive_10yr<-as.numeric(as.character(data_DOD_2001$survive_10yr))
data_DOD_2001$survive_2016<-as.numeric(as.character(data_DOD_2001$survive_2016))

str(data_DOD_2001)

##t-test between small and nonsmall survival rates##
#3-year#

table(data_DOD_2001$top_smallbiz_bin)
table(data_DOD_2001$survive_3yr)
table(data_DOD_2001$top_smallbiz_bin, data_DOD_2001$survive_3yr)

t.test(survive_3yr ~ top_smallbiz_bin, data = data_DOD_2001)

#5-year#
table(data_DOD_2001$top_smallbiz_bin, data_DOD_2001$survive_5yr)

t.test(survive_5yr ~ top_smallbiz_bin, data = data_DOD_2001)

#10-year#
table(data_DOD_2001$top_smallbiz_bin, data_DOD_2001$survive_10yr)

t.test(survive_10yr ~ top_smallbiz_bin, data = data_DOD_2001)


#*****************
#Survival#
#*****************
#************#
#*****ALL****#
#************#
##3-year##
table(data_DOD_2001$survive_3yr) #0=10168, 1=17266

numerator_3yrALL_2001_DOD <- length(which(data_DOD_2001$survive_3yr==1))

denominator_3yrALL_2001_DoD <- length(data_DOD_2001$survive_3yr)

survival_3yrALL_2001_DoD <- numerator_3yrALL_2001_DOD/denominator_3yrALL_2001_DoD
survival_3yrALL_2001_DoD

##5-year##
table(data_DOD_2001$survive_5yr) #0=10168, 1=17266

numerator_5yrALL_2001_DoD <- length(which(data_DOD_2001$survive_5yr==1))

denominator_5yrALL_2001_DoD <- length(data_DOD_2001$survive_5yr)

survival_5yrALL_2001_DoD <- numerator_5yrALL_2001_DoD/denominator_5yrALL_2001_DoD
survival_5yrALL_2001_DoD

##10-year##
table(data_DOD_2001$survive_10yr) #0=10168, 1=17266

numerator_10yrALL_2001_DoD <- length(which(data_DOD_2001$survive_10yr==1))

denominator_10yrALL_2001_DoD <- length(data_DOD_2001$survive_10yr)

survival_10yrALL_2001_DoD <- numerator_10yrALL_2001_DoD/denominator_10yrALL_2001_DoD
survival_10yrALL_2001_DoD

#**********#
#Graduation#
#**********#
##for all firms
table(data_DOD_2001$graduated)

numerator_gradALL_2001_DoD <- length(which(data_DOD_2001$graduated==1))

denominator_gradALL_2001_DoD <- length(data_DOD_2001$graduated)

graduatedALL_2001_DoD <- numerator_gradALL_2001_DoD/denominator_gradALL_2001_DoD
graduatedALL_2001_DoD

##for firms that only survived to the end of the study period
numerator_gradALL_2001_DoD_10yr <- length(which(data_DOD_2001$graduated==1 & data_DOD_2001$survive_10yr==1))

#denominator_gradALL_2001_DoD_10yr <- length(data_DOD_2001$graduated)

denominator_gradALL_2001_DoD_10yr <- length(which(data_DOD_2001$top_smallbiz_bin==1))

graduatedALL_2001_DoD_10yr <- numerator_gradALL_2001_DoD_10yr/denominator_gradALL_2001_DoD_10yr
graduatedALL_2001_DoD_10yr


#**************************
#**********#
#***SMALL**#
#**********#
##3-year##
table(data_DOD_2001$survive_3yr) #0=10168, 1=17266

numerator_3yrSM_2001_DoD <- length(which(data_DOD_2001$survive_3yr==1 & data_DOD_2001$top_smallbiz_bin==1))

denominator_3yrSM_2001_DoD <- length(which(data_DOD_2001$top_smallbiz_bin==1))

survival_3yrSM_2001_DoD <- numerator_3yrSM_2001_DoD/denominator_3yrSM_2001_DoD
survival_3yrSM_2001_DoD

##5-year##
table(data_DOD_2001$survive_5yr) #0=10168, 1=17266

numerator_5yrSM_2001_DoD <- length(which(data_DOD_2001$survive_5yr==1 & data_DOD_2001$top_smallbiz_bin==1))

denominator_5yrSM_2001_DoD <- length(which(data_DOD_2001$top_smallbiz_bin==1))

survival_5yrSM_2001_DoD <- numerator_5yrSM_2001_DoD/denominator_5yrSM_2001_DoD
survival_5yrSM_2001_DoD

##10-year##
table(data_DOD_2001$survive_10yr) #0=10168, 1=17266

numerator_10yrSM_2001_DoD <- length(which(data_DOD_2001$survive_10yr==1 & data_DOD_2001$top_smallbiz_bin==1))

denominator_10yrSM_2001_DoD <- length(which(data_DOD_2001$top_smallbiz_bin==1))

survival_10yrSM_2001_DoD <- numerator_10yrSM_2001_DoD/denominator_10yrSM_2001_DoD
survival_10yrSM_2001_DoD


#**********#
#***NON-SMALL**#
#**********#

##3-year##
table(data_DOD_2001$survive_3yr) #0=10168, 1=17266

numerator_3yrNSM_2001_DoD <- length(which(data_DOD_2001$survive_3yr==1 & data_DOD_2001$top_smallbiz_bin==0))

denominator_3yrNSM_2001_DoD <- length(which(data_DOD_2001$top_smallbiz_bin==0))

survival_3yrNSM_2001_DoD <- numerator_3yrNSM_2001_DoD/denominator_3yrNSM_2001_DoD
survival_3yrNSM_2001_DoD

##5-year##
table(data_DOD_2001$survive_5yr) #0=10168, 1=17266

numerator_5yrNSM_2001_DoD <- length(which(data_DOD_2001$survive_5yr==1 & data_DOD_2001$top_smallbiz_bin==0))

denominator_5yrNSM_2001_DoD <- length(which(data_DOD_2001$top_smallbiz_bin==0))

survival_5yrNSM_2001_DoD <- numerator_5yrNSM_2001_DoD/denominator_5yrNSM_2001_DoD
survival_5yrNSM_2001_DoD

##10-year##
table(data_DOD_2001$survive_10yr) #0=10168, 1=17266

numerator_10yrNSM_2001_DoD <- length(which(data_DOD_2001$survive_10yr==1 & data_DOD_2001$top_smallbiz_bin==0))

denominator_10yrNSM_2001_DoD <- length(which(data_DOD_2001$top_smallbiz_bin==0))

survival_10yrNSM_2001_DoD <- numerator_10yrNSM_2001_DoD/denominator_10yrNSM_2001_DoD
survival_10yrNSM_2001_DoD

#*******************#
#******2016 check***#
#*******************#
##*********
#***2016 survival 
#**********
survive_10yr_count_2001_DOD <- length(which(data_DOD_2001$survive_10yr==1))
##ALL##
numerator_2016_ALL_2001_DOD <- length(which(data_DOD_2001$survive_2016==1))

denominator_2016_ALL_2001_DOD <- length(data_DOD_2001$survive_2016)

survivalrate_2016_ALL_2001_DOD <- numerator_2016_ALL_2001_DOD/denominator_2016_ALL_2001_DOD
survivalrate_2016_ALL_2001_DOD


##SMALL##
numerator_2016_SM_2001_DOD <- length(which(data_DOD_2001$survive_2016==1 & data_DOD_2001$top_smallbiz_bin==1))

denominator_2016_SM_2001_DOD <- length(which(data_DOD_2001$top_smallbiz_bin==1))

survivalrate_2016_SM_2001_DOD <- numerator_2016_SM_2001_DOD/denominator_2016_SM_2001_DOD
survivalrate_2016_SM_2001_DOD


##NONSMALL##
numerator_2016_NS_2001_DOD <- length(which(data_DOD_2001$survive_2016==1 & data_DOD_2001$top_smallbiz_bin==0))

denominator_2016_NS_2001_DOD <- length(which(data_DOD_2001$top_smallbiz_bin==0))

survivalrate_2016_NS_2001_DOD <- numerator_2016_NS_2001_DOD/denominator_2016_NS_2001_DOD
survivalrate_2016_NS_2001_DOD


table(data_DOD_2001$top_smallbiz_bin, data_DOD_2001$survive_2016)


t.test(survive_2016 ~ top_smallbiz_bin, data = data_DOD_2001)


##**************##
####Pie Charts for DOD 2001 check####
#****************#

##******##
##ALL NE##
##******##
pie_2001_ALL_DoD_df <- data.frame(
  group = c("3-yr", "5-yr", "10-yr", "2016"),
  value = c(survival_3yrALL_2001_DoD, survival_5yrALL_2001_DoD, survival_10yrALL_2001_DoD, survivalrate_2016_ALL_2001_DOD)
)

head(pie_2001_ALL_DoD_df)

pie_2001_ALL_DoD_df <- pie_2001_ALL_DoD_df %>%
  dplyr::mutate(perc_value = value * 100) %>%
  dplyr::mutate(perc_value = round(perc_value, 0))

head(pie_2001_ALL_DoD_df)

plot_2001_DoD_ALL <- ggplot(pie_2001_ALL_DoD_df, aes(x=group, y=perc_value, fill=group)) +
  scale_fill_manual(values=c("#99FFFF", "#0099CC", "#006699", "#999999" )) +
  geom_bar(stat = "identity", width=0.4) +
  scale_x_discrete(limits=c("3-yr", "5-yr", "10-yr", "2016")) +
  ylim(c(0, 70)) +
  geom_text(aes(label=percent(value)), size=3, vjust=-.25) +
  labs(title = "- All New Entrants DoD", y = "Survival Rates (percentages)", x = "Year") + 
  guides(fill=FALSE)


##******##
##SMALL NE##
##******##
pie_2001_SM_DoD_df <- data.frame(
  group = c("3-yr", "5-yr", "10-yr", "2016", "Graduated"),
  value = c(survival_3yrSM_2001_DoD, survival_5yrSM_2001_DoD, survival_10yrSM_2001_DoD, survivalrate_2016_SM_2001_DOD, graduatedALL_2001_DoD_10yr)
)

head(pie_2001_SM_DoD_df)

pie_2001_SM_DoD_df <- pie_2001_SM_DoD_df %>%
  dplyr::mutate(perc_value = value * 100) %>%
  dplyr::mutate(perc_value = round(perc_value, 0))

head(pie_2001_SM_DoD_df)

plot_2001_DoD_SMALL <- ggplot(pie_2001_SM_DoD_df, aes(x=group, y=perc_value, fill=group)) +
  scale_fill_manual(values=c("#99FFFF", "#0099CC", "#006699", "#999999", "#000066")) +
  geom_bar(stat = "identity", width=0.4) +
  scale_x_discrete(limits=c("3-yr", "5-yr", "10-yr", "2016", "Graduated")) +
  ylim(c(0, 70)) +
  geom_text(aes(label=percent(value)), vjust=-.25, size=3) +
  labs(title = "Survival Rates 2001 DoD - Small Businesses", y = "Survival Rates (percentages)", x = "Year") 

##******##
##Non-Small NE##
##******##
pie_2001_NS_DoD_df <- data.frame(
  group = c("3-yr", "5-yr", "10-yr", "2016"),
  value = c(survival_3yrNSM_2001_DoD, survival_5yrNSM_2001_DoD, survival_10yrNSM_2001_DoD, survivalrate_2016_NS_2001_DOD)
)

head(pie_2001_NS_DoD_df)

pie_2001_NS_DoD_df <- pie_2001_NS_DoD_df %>%
  dplyr::mutate(perc_value = value * 100) %>%
  dplyr::mutate(perc_value = round(perc_value, 0))

head(pie_2001_NS_DoD_df)

plot_2001_DoD_NS <- ggplot(pie_2001_NS_DoD_df, aes(x=group, y=perc_value, fill=group)) +
  scale_fill_manual(values=c("#99FFFF", "#0099CC", "#006699", "#999999")) +
  geom_bar(stat = "identity", width=0.4) +
  scale_x_discrete(limits=c("3-yr", "5-yr", "10-yr", "2016")) +
  ylim(c(0, 70)) +
  geom_text(aes(label=percent(value)), vjust=-.25, size=3) +
  labs(title = "- Non-Small Buisinesses DoD", y = "Survival Rates (percentages)", x = "Year")+ 
  guides(fill=FALSE)

##*****##
#combine plots#
#******##
#t <- textGrob("Survival Rates 2001 Sample", fontsize=42)
grid.arrange(plot_2001_SMALL, plot_2001_DoD_SMALL, plot_2001_ALL, plot_2001_DoD_ALL, plot_2001_NS, plot_2001_DoD_NS)



#**********************************************************************#

#***********#
####2002 all fed agencies####
#***********#

#create necessary variables to do this#

#subset the 2001 data
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


str(data_2002)

data_2002$survive_3yr<-as.numeric(as.character(data_2002$survive_3yr))
data_2002$survive_5yr<-as.numeric(as.character(data_2002$survive_5yr))
data_2002$survive_10yr<-as.numeric(as.character(data_2002$survive_10yr))
data_2002$survive_2016<-as.numeric(as.character(data_2002$survive_2016))

str(data_2002)

##t test to test the differences between small and non small survival##
table(data_2002$top_smallbiz_bin)
table(data_2002$survive_3yr)
table(data_2002$top_smallbiz_bin, data_2002$survive_3yr)

t.test(survive_3yr ~ top_smallbiz_bin, data = data_2002)

#5-year#
table(data_2002$top_smallbiz_bin, data_2002$survive_5yr)

t.test(survive_5yr ~ top_smallbiz_bin, data = data_2002)

#10-year#
table(data_2002$top_smallbiz_bin, data_2002$survive_10yr)

t.test(survive_10yr ~ top_smallbiz_bin, data = data_2002)


#*************
#***ALL******#
#*************#
#*****************
##Survival Rates##
#*****************

##3-year##
table(data_2002$survive_3yr) #0=12449, 1=21743

numerator_3yrALL_2002 <- length(which(data_2002$survive_3yr==1))

denominator_3yrALL_2002 <- length(data_2002$survive_3yr)

survival_3yrALL_2002 <- numerator_3yrALL_2002/denominator_3yrALL_2002
survival_3yrALL_2002

##5-year##
table(data_2002$survive_5yr) #0=10168, 1=17266

numerator_5yrALL_2002 <- length(which(data_2002$survive_5yr==1))

denominator_5yrALL_2002 <- length(data_2002$survive_5yr)

survival_5yrALL_2002 <- numerator_5yrALL_2002/denominator_5yrALL_2002
survival_5yrALL_2002

##10-year##
table(data_2002$survive_10yr) #0=10168, 1=17266

numerator_10yrALL_2002 <- length(which(data_2002$survive_10yr==1))

denominator_10yrALL_2002 <- length(data_2002$survive_10yr)

survival_10yrALL_2002 <- numerator_10yrALL_2002/denominator_10yrALL_2002
survival_10yrALL_2002 
#****************#
#Graduation Rates#
#****************#
table(data_2002$graduated)

numerator_grad_2002 <- length(which(data_2002$graduated==1))

denominator_grad_2002 <- length(data_2002$graduated)

graduated_2002 <- numerator_grad_2002/denominator_grad_2002
graduated_2002

##graduation for only those who survived after 10 years
numerator_grad_2002_10yr <- length(which(data_2002$graduated==1 & data_2002$survive_10yr==1))

#denominator_grad_2002_10yr <- length(data_2002$graduated)

denominator_grad_2002_10yr <- length(which(data_2002$top_smallbiz_bin==1))

graduated_2002_10yr <- numerator_grad_2002_10yr/denominator_grad_2002_10yr
graduated_2002_10yr


#**********#
#***SMALL**#
#**********#
##3-year##
table(data_2002$survive_3yr) #0=12449, 1=21743

numerator_3yrSM_2002 <- length(which(data_2002$survive_3yr==1 & data_2002$top_smallbiz_bin==1))

denominator_3yrSM_2002 <- length(which(data_2002$top_smallbiz_bin==1))

survival_3yrSM_2002 <- numerator_3yrSM_2002/denominator_3yrSM_2002
survival_3yrSM_2002

##5-year##
table(data_2002$survive_5yr) #0=10168, 1=17266

numerator_5yrSM_2002 <- length(which(data_2002$survive_5yr==1 & data_2002$top_smallbiz_bin==1))

denominator_5yrSM_2002 <- length(which(data_2002$top_smallbiz_bin==1))

survival_5yrSM_2002 <- numerator_5yrSM_2002/denominator_5yrSM_2002
survival_5yrSM_2002 

##10-year##
table(data_2002$survive_10yr) #0=10168, 1=17266

numerator_10yrSM_2002 <- length(which(data_2002$survive_10yr==1 & data_2002$top_smallbiz_bin==1))

denominator_10yrSM_2002 <- length(which(data_2002$top_smallbiz_bin==1))

survival_10yrSM_2002 <- numerator_10yrSM_2002/denominator_10yrSM_2002
survival_10yrSM_2002

#**********#
#***NON-SMALL*#
#***********#
##3-year##
table(data_2002$survive_3yr) #0=12449, 1=21743

numerator_3yrNSM_2002 <- length(which(data_2002$survive_3yr==1 & data_2002$top_smallbiz_bin==0))

denominator_3yrNSM_2002 <- length(which(data_2002$top_smallbiz_bin==0))

survival_3yrNSM_2002 <- numerator_3yrNSM_2002/denominator_3yrNSM_2002
survival_3yrNSM_2002

##5-year##
table(data_2002$survive_5yr) #0=10168, 1=17266

numerator_5yrNSM_2002 <- length(which(data_2002$survive_5yr==1 & data_2002$top_smallbiz_bin==0))

denominator_5yrNSM_2002 <- length(which(data_2002$top_smallbiz_bin==0))

survival_5yrNSM_2002 <- numerator_5yrNSM_2002/denominator_5yrNSM_2002
survival_5yrNSM_2002 

##10-year##
table(data_2002$survive_10yr) #0=10168, 1=17266

numerator_10yrNSM_2002 <- length(which(data_2002$survive_10yr==1 & data_2002$top_smallbiz_bin==0))

denominator_10yrNSM_2002 <- length(which(data_2002$top_smallbiz_bin==0))

survival_10yrNSM_2002 <- numerator_10yrNSM_2002/denominator_10yrNSM_2002
survival_10yrNSM_2002

#*******************#
#******2016 check***#
#*******************#
##*********
#***2016 survival 
#**********
survive_10yr_count_2002 <- length(which(data_2002$survive_10yr==1))
##ALL##
numerator_2016_ALL_2002 <- length(which(data_2002$survive_2016==1))

denominator_2016_ALL_2002 <- length(data_2002$survive_2016)

survivalrate_2016_ALL_2002 <- numerator_2016_ALL_2002/denominator_2016_ALL_2002
survivalrate_2016_ALL_2002


##SMALL##
numerator_2016_SM_2002 <- length(which(data_2002$survive_2016==1 & data_2002$top_smallbiz_bin==1))

denominator_2016_SM_2002 <- length(which(data_2002$top_smallbiz_bin==1))

survivalrate_2016_SM_2002 <- numerator_2016_SM_2002/denominator_2016_SM_2002
survivalrate_2016_SM_2002


##NONSMALL##
numerator_2016_NS_2002 <- length(which(data_2002$survive_2016==1 & data_2002$top_smallbiz_bin==0))

denominator_2016_NS_2002 <- length(which(data_2002$top_smallbiz_bin==0))

survivalrate_2016_NS_2002<- numerator_2016_NS_2002/denominator_2016_NS_2002
survivalrate_2016_NS_2002


table(data_2002$top_smallbiz_bin, data_2002$survive_2016)


t.test(survive_2016 ~ top_smallbiz_bin, data = data_2002)


##**************##
####Pie Charts for 2002####
#****************#

##******##
##ALL NE##
##******##
pie_2002_ALL_df <- data.frame(
  group = c("3-yr", "5-yr", "10-yr", "2016"),
  value = c(survival_3yrALL_2002, survival_5yrALL_2002, survival_10yrALL_2002, survivalrate_2016_ALL_2002)
)

head(pie_2002_ALL_df)

pie_2002_ALL_df <- pie_2002_ALL_df %>%
  dplyr::mutate(perc_value = value * 100) %>%
  dplyr::mutate(perc_value = round(perc_value, 0))

head(pie_2002_ALL_df)

plot_2002_ALL <- ggplot(pie_2002_ALL_df, aes(x=group, y=perc_value, fill=group)) +
  scale_fill_manual(values=c("#99FFFF", "#0099CC", "#006699", "#999999" )) +
  geom_bar(stat = "identity", width=0.4) +
  scale_x_discrete(limits=c("3-yr", "5-yr", "10-yr", "2016")) +
  ylim(c(0, 70)) +
  geom_text(aes(label=percent(value)), size=3, vjust=-.25) +
  labs(title = "- All New Entrants All Fed", y = "Survival Rates (percentages)", x = "Year") + 
  guides(fill=FALSE)


##******##
##SMALL NE##
##******##
pie_2002_SM_df <- data.frame(
  group = c("3-yr", "5-yr", "10-yr", "2016", "Graduated"),
  value = c(survival_3yrSM_2002, survival_5yrSM_2002, survival_10yrSM_2002, survivalrate_2016_SM_2002, graduated_2002_10yr)
)

head(pie_2002_SM_df)

pie_2002_SM_df <- pie_2002_SM_df %>%
  dplyr::mutate(perc_value = value * 100) %>%
  dplyr::mutate(perc_value = round(perc_value, 0))

head(pie_2002_SM_df)

plot_2002_SMALL <- ggplot(pie_2002_SM_df, aes(x=group, y=perc_value, fill=group)) +
  scale_fill_manual(values=c("#99FFFF", "#0099CC", "#006699", "#999999", "#000066" )) +
  geom_bar(stat = "identity", width=0.4) +
  scale_x_discrete(limits=c("3-yr", "5-yr", "10-yr", "2016", "Graduated")) +
  ylim(c(0, 70)) +
  geom_text(aes(label=percent(value)), vjust=-.25, size=3) +
  labs(title = "Survival Rates 2002 All Federal Agencies- Small Businesses", y = "Survival Rates (percentages)", x = "Year") + 
  guides(fill=FALSE)

##******##
##Non-Small NE##
##******##
pie_2002_NS_df <- data.frame(
  group = c("3-yr", "5-yr", "10-yr", "2016"),
  value = c(survival_3yrNSM_2002, survival_5yrNSM_2002, survival_10yrNSM_2002, survivalrate_2016_NS_2002)
)

head(pie_2002_NS_df)

pie_2002_NS_df <- pie_2002_NS_df %>%
  dplyr::mutate(perc_value = value * 100) %>%
  dplyr::mutate(perc_value = round(perc_value, 0))

head(pie_2002_NS_df)

plot_2002_NS <- ggplot(pie_2002_NS_df, aes(x=group, y=perc_value, fill=group)) +
  scale_fill_manual(values=c("#99FFFF", "#0099CC", "#006699", "#999999")) +
  geom_bar(stat = "identity", width=0.4) +
  scale_x_discrete(limits=c("3-yr", "5-yr", "10-yr", "2016")) +
  ylim(c(0, 70)) +
  geom_text(aes(label=percent(value)), vjust=-.25, size=3) +
  labs(title = "- Non-Small Businesses All Fed", y = "Survival Rates (percents)", x = "Year") +
  guides(fill=FALSE)

##combine plots

grid.arrange(plot_2002_SMALL, plot_2002_ALL, plot_2002_NS)


#************#
#### 2002 DOD ONLY####
#****************#

#creae necessary variables to do this#

#subset the 2001 data
data_2002_DOD <- FPDS_cleaned_unique[!(FPDS_cleaned_unique$registrationYear!="2002"), ]
data_2002_DOD <- data_2002_DOD[!(data_2002_DOD$customer!="Defense"), ]

##create variable describing whether a firm survived 3 years

data_2002_DOD <- data_2002_DOD %>%
  dplyr::mutate(survive_3yr = ifelse(exitYear < 2004, "0", "1")) 

##create variable describing whether a firm survived 5 years
data_2002_DOD <- data_2002_DOD %>%
  dplyr::mutate(survive_5yr = ifelse(exitYear < 2006, "0", "1")) 


##create variable describing whether a firm survived 10 years
data_2002_DOD <- data_2002_DOD %>%
  dplyr::mutate(survive_10yr = ifelse(exitYear < 2011, "0", "1")) 

##create variable describing whether a firm survived in 2016
data_2002_DOD <- data_2002_DOD %>%
  dplyr::mutate(survive_2016 = ifelse(exitYear < 2016, "0", "1"))


str(data_2002_DOD)

data_2002_DOD$survive_3yr<-as.numeric(as.character(data_2002_DOD$survive_3yr))
data_2002_DOD$survive_5yr<-as.numeric(as.character(data_2002_DOD$survive_5yr))
data_2002_DOD$survive_10yr<-as.numeric(as.character(data_2002_DOD$survive_10yr))
data_2002_DOD$survive_2016<-as.numeric(as.character(data_2002_DOD$survive_2016))

str(data_2002_DOD)

##t test to test the differences between small and non small survival##
table(data_2002_DOD$top_smallbiz_bin)
table(data_2002_DOD$survive_3yr)
table(data_2002_DOD$top_smallbiz_bin, data_2002_DOD$survive_3yr)

t.test(survive_3yr ~ top_smallbiz_bin, data = data_2002_DOD)

#5-year#
table(data_2002_DOD$top_smallbiz_bin, data_2002_DOD$survive_5yr)

t.test(survive_5yr ~ top_smallbiz_bin, data = data_2002_DOD)

#10-year#
table(data_2002_DOD$top_smallbiz_bin, data_2002_DOD$survive_10yr)

t.test(survive_10yr ~ top_smallbiz_bin, data = data_2002_DOD)


#*************
#***ALL******#
#*************#
#*****************
##Survival Rates##
#*****************

##3-year##
table(data_2002_DOD$survive_3yr) #0=12449, 1=21743

numerator_3yrALL2002_DOD <- length(which(data_2002_DOD$survive_3yr==1))

denominator_3yrALL2002_DOD <- length(data_2002_DOD$survive_3yr)

survival_3yrALL2002_DOD <- numerator_3yrALL2002_DOD/denominator_3yrALL2002_DOD
survival_3yrALL2002_DOD

##5-year##
table(data_2002_DOD$survive_5yr) #0=10168, 1=17266

numerator_5yrALL2002_DOD <- length(which(data_2002_DOD$survive_5yr==1))

denominator_5yrALL2002_DOD <- length(data_2002_DOD$survive_5yr)

survival_5yrALL2002_DOD <- numerator_5yrALL2002_DOD/denominator_5yrALL2002_DOD
survival_5yrALL2002_DOD

##10-year##
table(data_2002_DOD$survive_10yr) #0=10168, 1=17266

numerator_10yrALL2002_DOD <- length(which(data_2002_DOD$survive_10yr==1))

denominator_10yrALL2002_DOD <- length(data_2002_DOD$survive_10yr)

survival_10yrALL2002_DOD <- numerator_10yrALL2002_DOD/denominator_10yrALL2002_DOD
survival_10yrALL2002_DOD 
#****************#
#Graduation Rates#
#****************#
table(data_2002_DOD$graduated)

numerator_grad2002_DOD <- length(which(data_2002_DOD$graduated==1))

denominator_grad2002_DOD <- length(data_2002_DOD$graduated)

graduated2002_DOD <- numerator_grad2002_DOD/denominator_grad2002_DOD
graduated2002_DOD

##for those that survived 10 years
numerator_grad2002_DOD_10yr <- length(which(data_2002_DOD$graduated==1 & data_2002_DOD$survive_10yr==0))

#denominator_grad2002_DOD_10yr <- length(data_2002_DOD$graduated)

denominator_grad2002_DOD_10yr <- length(which(data_2002_DOD$top_smallbiz_bin==1))

graduated_2002_DOD_10yr <- numerator_grad2002_DOD_10yr/denominator_grad2002_DOD_10yr
graduated_2002_DOD_10yr


#**********#
#***SMALL**#
#**********#
##3-year##
table(data_2002_DOD$survive_3yr) #0=12449, 1=21743

numerator_3yrSM2002_DOD <- length(which(data_2002_DOD$survive_3yr==1 & data_2002_DOD$top_smallbiz_bin==1))

denominator_3yrSM2002_DOD <- length(which(data_2002_DOD$top_smallbiz_bin==1))

survival_3yrSM2002_DOD <- numerator_3yrSM2002_DOD/denominator_3yrSM2002_DOD
survival_3yrSM2002_DOD

##5-year##
table(data_2002_DOD$survive_5yr) #0=10168, 1=17266

numerator_5yrSM2002_DOD <- length(which(data_2002_DOD$survive_5yr==1 & data_2002_DOD$top_smallbiz_bin==1))

denominator_5yrSM2002_DOD <- length(which(data_2002_DOD$top_smallbiz_bin==1))

survival_5yrSM2002_DOD <- numerator_5yrSM2002_DOD/denominator_5yrSM2002_DOD
survival_5yrSM2002_DOD 

##10-year##
table(data_2002_DOD$survive_10yr) #0=10168, 1=17266

numerator_10yrSM2002_DOD <- length(which(data_2002_DOD$survive_10yr==1 & data_2002_DOD$top_smallbiz_bin==1))

denominator_10yrSM2002_DOD <- length(which(data_2002_DOD$top_smallbiz_bin==1))

survival_10yrSM2002_DOD <- numerator_10yrSM2002_DOD/denominator_10yrSM2002_DOD
survival_10yrSM2002_DOD

#**********#
#***NON-SMALL*#
#***********#
##3-year##
table(data_2002_DOD$survive_3yr) #0=12449, 1=21743

numerator_3yrNSM2002_DOD <- length(which(data_2002_DOD$survive_3yr==1 & data_2002_DOD$top_smallbiz_bin==0))

denominator_3yrNSM2002_DOD <- length(which(data_2002_DOD$top_smallbiz_bin==0))

survival_3yrNSM2002_DOD <- numerator_3yrNSM2002_DOD/denominator_3yrNSM2002_DOD
survival_3yrNSM2002_DOD

##5-year##
table(data_2002_DOD$survive_5yr) #0=10168, 1=17266

numerator_5yrNSM2002_DOD <- length(which(data_2002_DOD$survive_5yr==1 & data_2002_DOD$top_smallbiz_bin==0))

denominator_5yrNSM2002_DOD <- length(which(data_2002_DOD$top_smallbiz_bin==0))

survival_5yrNSM2002_DOD <- numerator_5yrNSM2002_DOD/denominator_5yrNSM2002_DOD
survival_5yrNSM2002_DOD 

##10-year##
table(data_2002_DOD$survive_10yr) #0=10168, 1=17266

numerator_10yrNSM2002_DOD <- length(which(data_2002_DOD$survive_10yr==1 & data_2002_DOD$top_smallbiz_bin==0))

denominator_10yrNSM2002_DOD <- length(which(data_2002_DOD$top_smallbiz_bin==0))

survival_10yrNSM2002_DOD <- numerator_10yrNSM2002_DOD/denominator_10yrNSM2002_DOD
survival_10yrNSM2002_DOD

#*******************#
#******2016 check***#
#*******************#
##*********
#***2016 survival 
#**********
survive_10yr_count_2002_DOD <- length(which(data_2002_DOD$survive_10yr==1))
##ALL##
numerator_2016_ALL_2002_DOD <- length(which(data_2002_DOD$survive_2016==1))

denominator_2016_ALL_2002_DOD <- length(data_2002_DOD$survive_2016)

survivalrate_2016_ALL_2002_DOD <- numerator_2016_ALL_2002_DOD/denominator_2016_ALL_2002_DOD
survivalrate_2016_ALL_2002_DOD


##SMALL##
numerator_2016_SM_2002_DOD <- length(which(data_2002_DOD$survive_2016==1 & data_2002_DOD$top_smallbiz_bin==1))

denominator_2016_SM_2002_DOD <- length(which(data_2002_DOD$top_smallbiz_bin==1))

survivalrate_2016_SM_2002_DOD <- numerator_2016_SM_2002_DOD/denominator_2016_SM_2002_DOD
survivalrate_2016_SM_2002_DOD


##NONSMALL##
numerator_2016_NS_2002_DOD<- length(which(data_2002_DOD$survive_2016==1 & data_2002_DOD$top_smallbiz_bin==0))

denominator_2016_NS_2002_DOD <- length(which(data_2002_DOD$top_smallbiz_bin==0))

survivalrate_2016_NS_2002_DOD <- numerator_2016_NS_2002_DOD/denominator_2016_NS_2002_DOD
survivalrate_2016_NS_2002_DOD


table(data_2002_DOD$top_smallbiz_bin, data_2002_DOD$survive_2016)


t.test(survive_2016 ~ top_smallbiz_bin, data = data_2002_DOD)

##**************##
####Pie Charts for 2002 DoD ####
#****************#

##******##
##ALL NE##
##******##
pie_2002_ALL_DoD_df <- data.frame(
  group = c("3-yr", "5-yr", "10-yr", "2016"),
  value = c(survival_3yrALL2002_DOD, survival_5yrALL2002_DOD, survival_10yrALL2002_DOD, survivalrate_2016_ALL_2002_DOD)
)

head(pie_2002_ALL_DoD_df)

pie_2002_ALL_DoD_df <- pie_2002_ALL_DoD_df %>%
  dplyr::mutate(perc_value = value * 100) %>%
  dplyr::mutate(perc_value = round(perc_value, 0))

head(pie_2002_ALL_DoD_df)

plot_2002_DoD_ALL <- ggplot(pie_2002_ALL_DoD_df, aes(x=group, y=perc_value, fill=group)) +
  scale_fill_manual(values=c("#99FFFF", "#0099CC", "#006699", "#999999" )) +
  geom_bar(stat = "identity", width=0.4) +
  scale_x_discrete(limits=c("3-yr", "5-yr", "10-yr", "2016")) +
  ylim(c(0, 70)) +
  geom_text(aes(label=percent(value)), size=3, vjust=-.25) +
  labs(title = "- All New Entrants DoD", y = "Survival Rates (percentages)", x = "Year") + 
  guides(fill=FALSE)


##******##
##SMALL NE##
##******##
pie_2002_SM_DoD_df <- data.frame(
  group = c("3-yr", "5-yr", "10-yr", "2016", "Graduated"),
  value = c(survival_3yrALL2002_DOD, survival_5yrALL2002_DOD, survival_10yrALL2002_DOD, survivalrate_2016_ALL_2002_DOD, graduated_2002_DOD_10yr)
)

head(pie_2002_SM_DoD_df)

pie_2002_SM_DoD_df <- pie_2002_SM_DoD_df %>%
  dplyr::mutate(perc_value = value * 100) %>%
  dplyr::mutate(perc_value = round(perc_value, 0))

head(pie_2002_SM_DoD_df)

plot_2002_DoD_SMALL <- ggplot(pie_2002_SM_DoD_df, aes(x=group, y=perc_value, fill=group)) +
  scale_fill_manual(values=c("#99FFFF", "#0099CC", "#006699", "#999999", "#000066")) +
  geom_bar(stat = "identity", width=0.4) +
  scale_x_discrete(limits=c("3-yr", "5-yr", "10-yr", "2016", "Graduated")) +
  ylim(c(0, 70)) +
  geom_text(aes(label=percent(value)), vjust=-.25, size=3) +
  labs(title = "Survival Rates 2002 DoD - Small Businesses", y = "Survival Rates (percentages)", x = "Year")

##******##
##Non-Small NE##
##******##
pie_2002_NS_DoD_df <- data.frame(
  group = c("3-yr", "5-yr", "10-yr", "2016"),
  value = c(survival_3yrALL2002_DOD, survival_5yrALL2002_DOD, survival_10yrALL2002_DOD, survivalrate_2016_ALL_2002_DOD)
)

head(pie_2002_NS_DoD_df)

pie_2002_NS_DoD_df <- pie_2002_NS_DoD_df %>%
  dplyr::mutate(perc_value = value * 100) %>%
  dplyr::mutate(perc_value = round(perc_value, 0))

head(pie_2002_NS_DoD_df)

plot_2002_DoD_NS <- ggplot(pie_2002_NS_DoD_df, aes(x=group, y=perc_value, fill=group)) +
  scale_fill_manual(values=c("#99FFFF", "#0099CC", "#006699", "#999999")) +
  geom_bar(stat = "identity", width=0.4) +
  scale_x_discrete(limits=c("3-yr", "5-yr", "10-yr", "2016")) +
  ylim(c(0, 70)) +
  geom_text(aes(label=percent(value)), vjust=-.25, size=3) +
  labs(title = "- Non-Small Businesses DoD", y = "Survival Rates (percentages)", x = "Year") +
  guides(fill=FALSE)


#combine plots
grid.arrange(plot_2002_SMALL, plot_2002_DoD_SMALL, plot_2002_ALL, plot_2002_DoD_ALL, plot_2002_NS, plot_2002_DoD_NS)


#**********************************************************************#

#***********#
####2003 all federal agencies####
#***********#
#*********#
#ALL
#**********
##Create the variables to do this##

#subset the 2001 data
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


str(data_2003)

data_2003$survive_3yr<-as.numeric(as.character(data_2003$survive_3yr))
data_2003$survive_5yr<-as.numeric(as.character(data_2003$survive_5yr))
data_2003$survive_10yr<-as.numeric(as.character(data_2003$survive_10yr))
data_2003$survive_2016<-as.numeric(as.character(data_2003$survive_2016))

str(data_2003)

###
##t test to test the differences between small and non small survival##
table(data_2003$top_smallbiz_bin)
table(data_2003$survive_3yr)
table(data_2003$top_smallbiz_bin, data_2003$survive_3yr)

t.test(survive_3yr ~ top_smallbiz_bin, data = data_2003)

#5-year#
table(data_2003$top_smallbiz_bin, data_2003$survive_5yr)

t.test(survive_5yr ~ top_smallbiz_bin, data = data_2003)

#10-year#
table(data_2003$top_smallbiz_bin, data_2003$survive_10yr)

t.test(survive_10yr ~ top_smallbiz_bin, data = data_2003)



#***********#
#survival rates#
#**************#

##3-year##
table(data_2003$survive_3yr) #0=12449, 1=21743

numerator_3yrALL_2003 <- length(which(data_2003$survive_3yr==1))

denominator_3yrALL_2003 <- length(data_2003$survive_3yr)

survival_3yrALL_2003 <- numerator_3yrALL_2003/denominator_3yrALL_2003
survival_3yrALL_2003

##5-year##
table(data_2003$survive_5yr) #0=10168, 1=17266

numerator_5yrALL_2003 <- length(which(data_2003$survive_5yr==1))

denominator_5yrALL_2003 <- length(data_2003$survive_5yr)

survival_5yrALL_2003 <- numerator_5yrALL_2003/denominator_5yrALL_2003
survival_5yrALL_2003

##10-year##
table(data_2003$survive_10yr) #0=10168, 1=17266

numerator_10yrALL_2003 <- length(which(data_2003$survive_10yr==1))

denominator_10yrALL_2003 <- length(data_2003$survive_10yr)

survival_10yrALL_2003 <- numerator_10yrALL_2003/denominator_10yrALL_2003
survival_10yrALL_2003

#****************#
#Graduation Rates#
#****************#
table(data_2003$graduated)

numerator_grad_2003 <- length(which(data_2003$graduated==1))

denominator_grad_2003 <- length(data_2003$graduated)

graduated_2003 <- numerator_grad_2003/denominator_grad_2003
graduated_2003

##only for those that survived 10 years
numerator_grad_2003_10yr <- length(which(data_2003$graduated==1 & data_2003$survive_10yr==1))

#denominator_grad_2003_10yr <- length(data_2003$graduated)

denominator_grad_2003_10yr <- length(which(data_2003$top_smallbiz_bin==1))

graduated_2003_10yr <- numerator_grad_2003_10yr/denominator_grad_2003_10yr
graduated_2003_10yr


#*******#
#***SMALL***#
#********#
##3-year##
table(data_2003$survive_3yr) #0=12449, 1=21743

numerator_3yrSM_2003 <- length(which(data_2003$survive_3yr==1 & data_2003$top_smallbiz_bin==1))

denominator_3yrSM_2003 <- length(which(data_2003$top_smallbiz_bin==1))

survival_3yrSM_2003 <- numerator_3yrSM_2003/denominator_3yrSM_2003
survival_3yrSM_2003

##5-year##
table(data_2003$survive_5yr) #0=10168, 1=17266

numerator_5yrSM_2003 <- length(which(data_2003$survive_5yr==1 & data_2003$top_smallbiz_bin==1))

denominator_5yrSM_2003 <- length(which(data_2003$top_smallbiz_bin==1))

survival_5yrSM_2003 <- numerator_5yrSM_2003/denominator_5yrSM_2003
survival_5yrSM_2003

##10-year##
table(data_2003$survive_10yr) #0=10168, 1=17266

numerator_10yrSM_2003 <- length(which(data_2003$survive_10yr==1 & data_2003$top_smallbiz_bin==1))

denominator_10yrSM_2003 <- length(which(data_2003$top_smallbiz_bin==1))

survival_10yrSM_2003 <- numerator_10yrSM_2003/denominator_10yrSM_2003
survival_10yrSM_2003

#*****#
#NONSMALL*
#******#
##3-year##
table(data_2003$survive_3yr) #0=12449, 1=21743

numerator_3yrNSM_2003 <- length(which(data_2003$survive_3yr==1 & data_2003$top_smallbiz_bin==0))

denominator_3yrNSM_2003 <- length(which(data_2003$top_smallbiz_bin==0))

survival_3yrNSM_2003 <- numerator_3yrNSM_2003/denominator_3yrNSM_2003
survival_3yrNSM_2003

##5-year##
table(data_2003$survive_5yr) #0=10168, 1=17266

numerator_5yrNSM_2003 <- length(which(data_2003$survive_5yr==1 & data_2003$top_smallbiz_bin==0))

denominator_5yrNSM_2003 <- length(which(data_2003$top_smallbiz_bin==0))

survival_5yrNSM_2003 <- numerator_5yrNSM_2003/denominator_5yrNSM_2003
survival_5yrNSM_2003

##10-year##
table(data_2003$survive_10yr) #0=10168, 1=17266

numerator_10yrNSM_2003 <- length(which(data_2003$survive_10yr==1 & data_2003$top_smallbiz_bin==0))

denominator_10yrNSM_2003 <- length(which(data_2003$top_smallbiz_bin==0))

survival_10yrNSM_2003 <- numerator_10yrNSM_2003/denominator_10yrNSM_2003
survival_10yrNSM_2003


#*******************#
#******2016 check***#
#*******************#
##*********
#***2016 survival 
#**********
survive_10yr_count_2003 <- length(which(data_2003$survive_10yr==1))
##ALL##
numerator_2016_ALL_2003 <- length(which(data_2003$survive_2016==1))

denominator_2016_ALL_2003 <- length(data_2003$survive_2016)

survivalrate_2016_ALL_2003 <- numerator_2016_ALL_2003/denominator_2016_ALL_2003
survivalrate_2016_ALL_2003


##SMALL##
numerator_2016_SM_2003 <- length(which(data_2003$survive_2016==1 & data_2003$top_smallbiz_bin==1))

denominator_2016_SM_2003 <- length(which(data_2003$top_smallbiz_bin==1))

survivalrate_2016_SM_2003 <- numerator_2016_SM_2003/denominator_2016_SM_2003
survivalrate_2016_SM_2003


##NONSMALL##
numerator_2016_NS_2003 <- length(which(data_2003$survive_2016==1 & data_2003$top_smallbiz_bin==0))

denominator_2016_NS_2003<- length(which(data_2003$top_smallbiz_bin==0))

survivalrate_2016_NS_2003 <- numerator_2016_NS_2003/denominator_2016_NS_2003
survivalrate_2016_NS_2003


table(data_2003$top_smallbiz_bin, data_2003$survive_2016)


t.test(survive_2016 ~ top_smallbiz_bin, data = data_2003)

##**************##
####Pie Charts for 2003####
#****************#

##******##
##ALL NE##
##******##
pie_2003_ALL_df <- data.frame(
  group = c("3-yr", "5-yr", "10-yr", "2016"),
  value = c(survival_3yrALL_2003, survival_5yrALL_2003, survival_10yrALL_2003, survivalrate_2016_ALL_2003)
)

head(pie_2003_ALL_df)

pie_2003_ALL_df <- pie_2003_ALL_df %>%
  dplyr::mutate(perc_value = value * 100) %>%
  dplyr::mutate(perc_value = round(perc_value, 0))

head(pie_2003_ALL_df)

plot_2003_ALL <- ggplot(pie_2003_ALL_df, aes(x=group, y=perc_value, fill=group)) +
  scale_fill_manual(values=c("#99FFFF", "#0099CC", "#006699", "#999999" )) +
  geom_bar(stat = "identity", width=0.4) +
  scale_x_discrete(limits=c("3-yr", "5-yr", "10-yr", "2016")) +
  ylim(c(0, 70)) +
  geom_text(aes(label=percent(value)), size=3, vjust=-.25) +
  labs(title = "- All New Entrants All Fed", y = "Survival Rates (percentages)", x = "Year") + 
  guides(fill=FALSE)


##******##
##SMALL NE##
##******##
pie_2003_SM_df <- data.frame(
  group = c("3-yr", "5-yr", "10-yr", "2016", "Graduated"),
  value = c(survival_3yrSM_2003, survival_5yrSM_2003, survival_10yrSM_2003, survivalrate_2016_SM_2003, graduated_2003_10yr)
)

head(pie_2003_SM_df)

pie_2003_SM_df <- pie_2003_SM_df %>%
  dplyr::mutate(perc_value = value * 100) %>%
  dplyr::mutate(perc_value = round(perc_value, 0))

head(pie_2003_SM_df)

plot_2003_SMALL <- ggplot(pie_2003_SM_df, aes(x=group, y=perc_value, fill=group)) +
  scale_fill_manual(values=c("#99FFFF", "#0099CC", "#006699", "#999999", "#000066" )) +
  geom_bar(stat = "identity", width=0.4) +
  scale_x_discrete(limits=c("3-yr", "5-yr", "10-yr", "2016", "Graduated")) +
  ylim(c(0, 70)) +
  geom_text(aes(label=percent(value)), vjust=-.25, size=3) +
  labs(title = "Survival Rates 2003 All Federal Agencies - Small Businesses", y = "Survival Rates (percentages)", x = "Year") + 
  guides(fill=FALSE)

##******##
##Non-Small NE##
##******##
pie_2003_NS_df <- data.frame(
  group = c("3-yr", "5-yr", "10-yr", "2016"),
  value = c(survival_3yrNSM_2003, survival_5yrNSM_2003, survival_10yrNSM_2003, survivalrate_2016_NS_2003)
)

head(pie_2003_NS_df)

pie_2003_NS_df <- pie_2003_NS_df %>%
  dplyr::mutate(perc_value = value * 100) %>%
  dplyr::mutate(perc_value = round(perc_value, 0))

head(pie_2003_NS_df)

plot_2003_NS <- ggplot(pie_2003_NS_df, aes(x=group, y=perc_value, fill=group)) +
  scale_fill_manual(values=c("#99FFFF", "#0099CC", "#006699", "#999999")) +
  geom_bar(stat = "identity", width=0.4) +
  scale_x_discrete(limits=c("3-yr", "5-yr", "10-yr", "2016")) +
  ylim(c(0, 70)) +
  geom_text(aes(label=percent(value)), vjust=-.25, size=3) +
  labs(title = "- Non-Small Businesses All Fed", y = "Survival Rates (percents)", x = "Year") +
  guides(fill=FALSE)

##*****##
#combine plots#
#******##
#t <- textGrob("Survival Rates 2001 Sample", fontsize=42)
grid.arrange(plot_2003_SMALL, plot_2003_ALL, plot_2003_NS)


#*********#
#### 2003 DOD ONLY####
#*************#

#*********#
#ALL
#**********
##Create the variables to do this##

#subset the 2001 data
data_2003_DOD <- FPDS_cleaned_unique[!(FPDS_cleaned_unique$registrationYear!="2003"), ]
data_2003_DOD <- data_2003_DOD[!(data_2003_DOD$customer!="Defense"), ]


##create variable describing whether a firm survived 3 years

data_2003_DOD <- data_2003_DOD %>%
  dplyr::mutate(survive_3yr = ifelse(exitYear < 2005, "0", "1")) 

##create variable describing whether a firm survived 5 years
data_2003_DOD <- data_2003_DOD %>%
  dplyr::mutate(survive_5yr = ifelse(exitYear < 2007, "0", "1")) 


##create variable describing whether a firm survived 10 years
data_2003_DOD <- data_2003_DOD %>%
  dplyr::mutate(survive_10yr = ifelse(exitYear < 2012, "0", "1"))

##create variable describing whether a firm survived in 2016
data_2003_DOD <- data_2003_DOD %>%
  dplyr::mutate(survive_2016 = ifelse(exitYear < 2016, "0", "1"))


str(data_2003_DOD)

data_2003_DOD$survive_3yr<-as.numeric(as.character(data_2003_DOD$survive_3yr))
data_2003_DOD$survive_5yr<-as.numeric(as.character(data_2003_DOD$survive_5yr))
data_2003_DOD$survive_10yr<-as.numeric(as.character(data_2003_DOD$survive_10yr))
data_2003_DOD$survive_2016<-as.numeric(as.character(data_2003_DOD$survive_2016))

str(data_2003_DOD)

###
##t test to test the differences between small and non small survival##
table(data_2003_DOD$top_smallbiz_bin)
table(data_2003_DOD$survive_3yr)
table(data_2003_DOD$top_smallbiz_bin, data_2003_DOD$survive_3yr)

t.test(survive_3yr ~ top_smallbiz_bin, data = data_2003_DOD)

#5-year#
table(data_2003_DOD$top_smallbiz_bin, data_2003_DOD$survive_5yr)

t.test(survive_5yr ~ top_smallbiz_bin, data = data_2003_DOD)

#10-year#
table(data_2003_DOD$top_smallbiz_bin, data_2003_DOD$survive_10yr)

t.test(survive_10yr ~ top_smallbiz_bin, data = data_2003_DOD)

#***********#
#survival rates#
#**************#

##3-year##
table(data_2003_DOD$survive_3yr) #0=12449, 1=21743

numerator_3yrALL_2003_DOD <- length(which(data_2003_DOD$survive_3yr==1))

denominator_3yrALL_2003_DOD <- length(data_2003_DOD$survive_3yr)

survival_3yrALL_2003_DOD <- numerator_3yrALL_2003_DOD/denominator_3yrALL_2003_DOD
survival_3yrALL_2003_DOD

##5-year##
table(data_2003_DOD$survive_5yr) #0=10168, 1=17266

numerator_5yrALL_2003_DOD <- length(which(data_2003_DOD$survive_5yr==1))

denominator_5yrALL_2003_DOD <- length(data_2003_DOD$survive_5yr)

survival_5yrALL_2003_DOD <- numerator_5yrALL_2003_DOD/denominator_5yrALL_2003_DOD
survival_5yrALL_2003_DOD

##10-year##
table(data_2003_DOD$survive_10yr) #0=10168, 1=17266

numerator_10yrALL_2003_DOD <- length(which(data_2003_DOD$survive_10yr==1))

denominator_10yrALL_2003_DOD <- length(data_2003_DOD$survive_10yr)

survival_10yrALL_2003_DOD <- numerator_10yrALL_2003_DOD/denominator_10yrALL_2003_DOD
survival_10yrALL_2003_DOD

#****************#
#Graduation Rates#
#****************#
table(data_2003_DOD$graduated)

numerator_grad_2003_DOD <- length(which(data_2003_DOD$graduated==1))

denominator_grad_2003_DOD <- length(data_2003_DOD$graduated)

graduated_2003_DOD <- numerator_grad_2003_DOD/denominator_grad_2003_DOD
graduated_2003_DOD

##graduation for those who survived 10 years
table(data_2003$graduated)
table(data_2003$survive_10yr)
table(data_2003$top_smallbiz_bin)

table(data_2003_DOD$graduated)
table(data_2003_DOD$survive_10yr)
table(data_2003_DOD$top_smallbiz_bin)


numerator_grad_2003_DOD_10yr <- length(which(data_2003_DOD$graduated==1 & data_2003_DOD$survive_10yr==1))

#denominator_grad_2003_DOD_10yr <- length(data_2003_DOD$graduated)

denominator_grad_2003_DOD_10yr <- length(which(data_2003_DOD$top_smallbiz_bin==1))

graduated_2003_DOD_10yr <- numerator_grad_2003_DOD_10yr/denominator_grad_2003_DOD_10yr
graduated_2003_DOD_10yr


#*******#
#***SMALL***#
#********#
##3-year##
table(data_2003_DOD$survive_3yr) #0=12449, 1=21743

numerator_3yrSM_2003_DOD <- length(which(data_2003_DOD$survive_3yr==1 & data_2003_DOD$top_smallbiz_bin==1))

denominator_3yrSM_2003_DOD <- length(which(data_2003_DOD$top_smallbiz_bin==1))

survival_3yrSM_2003_DOD <- numerator_3yrSM_2003_DOD/denominator_3yrSM_2003_DOD
survival_3yrSM_2003_DOD

##5-year##
table(data_2003_DOD$survive_5yr) #0=10168, 1=17266

numerator_5yrSM_2003_DOD <- length(which(data_2003_DOD$survive_5yr==1 & data_2003_DOD$top_smallbiz_bin==1))

denominator_5yrSM_2003_DOD <- length(which(data_2003_DOD$top_smallbiz_bin==1))

survival_5yrSM_2003_DOD <- numerator_5yrSM_2003_DOD/denominator_5yrSM_2003_DOD
survival_5yrSM_2003_DOD

##10-year##
table(data_2003_DOD$survive_10yr) #0=10168, 1=17266

numerator_10yrSM_2003_DOD <- length(which(data_2003_DOD$survive_10yr==1 & data_2003_DOD$top_smallbiz_bin==1))

denominator_10yrSM_2003_DOD <- length(which(data_2003_DOD$top_smallbiz_bin==1))

survival_10yrSM_2003_DOD <- numerator_10yrSM_2003_DOD/denominator_10yrSM_2003_DOD
survival_10yrSM_2003_DOD

#*****#
#NONSMALL*
#******#
##3-year##
table(data_2003_DOD$survive_3yr) #0=12449, 1=21743

numerator_3yrNSM_2003_DOD <- length(which(data_2003_DOD$survive_3yr==1 & data_2003_DOD$top_smallbiz_bin==0))

denominator_3yrNSM_2003_DOD <- length(which(data_2003_DOD$top_smallbiz_bin==0))

survival_3yrNSM_2003_DOD <- numerator_3yrNSM_2003_DOD/denominator_3yrNSM_2003_DOD
survival_3yrNSM_2003_DOD

##5-year##
table(data_2003_DOD$survive_5yr) #0=10168, 1=17266

numerator_5yrNSM_2003_DOD <- length(which(data_2003_DOD$survive_5yr==1 & data_2003_DOD$top_smallbiz_bin==0))

denominator_5yrNSM_2003_DOD <- length(which(data_2003_DOD$top_smallbiz_bin==0))

survival_5yrNSM_2003_DOD <- numerator_5yrNSM_2003_DOD/denominator_5yrNSM_2003_DOD
survival_5yrNSM_2003_DOD

##10-year##
table(data_2003_DOD$survive_10yr) #0=10168, 1=17266

numerator_10yrNSM_2003_DOD <- length(which(data_2003_DOD$survive_10yr==1 & data_2003_DOD$top_smallbiz_bin==0))

denominator_10yrNSM_2003_DOD <- length(which(data_2003_DOD$top_smallbiz_bin==0))

survival_10yrNSM_2003_DOD <- numerator_10yrNSM_2003_DOD/denominator_10yrNSM_2003_DOD
survival_10yrNSM_2003_DOD

#*******************#
#******2016 check***#
#*******************#
##*********
#***2016 survival 
#**********
survive_10yr_count_2003_DOD <- length(which(data_2003_DOD$survive_10yr==1))
##ALL##
numerator_2016_ALL_2003_DOD <- length(which(data_2003_DOD$survive_2016==1))

denominator_2016_ALL_2003_DOD <- length(data_2003_DOD$survive_2016)

survivalrate_2016_ALL_2003_DOD <- numerator_2016_ALL_2003_DOD /denominator_2016_ALL_2003_DOD 
survivalrate_2016_ALL_2003_DOD 


##SMALL##
numerator_2016_SM_2003_DOD <- length(which(data_2003_DOD$survive_2016==1 & data_2003_DOD$top_smallbiz_bin==1))

denominator_2016_SM_2003_DOD <- length(which(data_2003_DOD$top_smallbiz_bin==1))

survivalrate_2016_SM_2003_DOD <- numerator_2016_SM_2003_DOD /denominator_2016_SM_2003_DOD 
survivalrate_2016_SM_2003_DOD 


##NONSMALL##
numerator_2016_NS_2003_DOD <- length(which(data_2003_DOD$survive_2016==1 & data_2003_DOD$top_smallbiz_bin==0))

denominator_2016_NS_2003_DOD <- length(which(data_2003_DOD$top_smallbiz_bin==0))

survivalrate_2016_NS_2003_DOD  <- numerator_2016_NS_2003_DOD /denominator_2016_NS_2003_DOD 
survivalrate_2016_NS_2003_DOD 


table(data_2003_DOD$top_smallbiz_bin, data_2003_DOD$survive_2016)


t.test(survive_2016 ~ top_smallbiz_bin, data = data_2003_DOD)


##**************##
####Pie Charts for 2003 DoD####
#****************#

##******##
##ALL NE##
##******##
pie_2003_ALL_DoD_df <- data.frame(
  group = c("3-yr", "5-yr", "10-yr", "2016"),
  value = c(survival_3yrALL_2003_DOD, survival_5yrALL_2003_DOD, survival_10yrALL_2003_DOD, survivalrate_2016_ALL_2003_DOD)
)

head(pie_2003_ALL_DoD_df)

pie_2003_ALL_DoD_df <- pie_2003_ALL_DoD_df %>%
  dplyr::mutate(perc_value = value * 100) %>%
  dplyr::mutate(perc_value = round(perc_value, 0))

head(pie_2003_ALL_DoD_df)

plot_2003_DoD_ALL <- ggplot(pie_2003_ALL_DoD_df, aes(x=group, y=perc_value, fill=group)) +
  scale_fill_manual(values=c("#99FFFF", "#0099CC", "#006699", "#999999" )) +
  geom_bar(stat = "identity", width=0.4) +
  scale_x_discrete(limits=c("3-yr", "5-yr", "10-yr", "2016")) +
  ylim(c(0, 70)) +
  geom_text(aes(label=percent(value)), size=3, vjust=-.25) +
  labs(title = "- All New Entrants DoD", y = "Survival Rates (percentages)", x = "Year") + 
  guides(fill=FALSE)


##******##
##SMALL NE##
##******##
pie_2003_SM_DoD_df <- data.frame(
  group = c("3-yr", "5-yr", "10-yr", "2016", "Graduated"),
  value = c(survival_3yrSM_2003_DOD, survival_5yrSM_2003_DOD, survival_10yrSM_2003_DOD, survivalrate_2016_SM_2003_DOD, graduated_2003_DOD_10yr)
)

head(pie_2003_SM_DoD_df)

pie_2003_SM_DoD_df <- pie_2003_SM_DoD_df %>%
  dplyr::mutate(perc_value = value * 100) %>%
  dplyr::mutate(perc_value = round(perc_value, 0))

head(pie_2003_SM_DoD_df)

plot_2003_DoD_SMALL <- ggplot(pie_2003_SM_DoD_df, aes(x=group, y=perc_value, fill=group)) +
  scale_fill_manual(values=c("#99FFFF", "#0099CC", "#006699", "#999999", "#000066")) +
  geom_bar(stat = "identity", width=0.4) +
  scale_x_discrete(limits=c("3-yr", "5-yr", "10-yr", "2016", "Graduated")) +
  ylim(c(0, 70)) +
  geom_text(aes(label=percent(value)), vjust=-.25, size=3) +
  labs(title = "Survival Rates 2003 DoD - Small Businesses", y = "Survival Rates (percentages)", x = "Year")

##******##
##Non-Small NE##
##******##
pie_2003_NS_DoD_df <- data.frame(
  group = c("3-yr", "5-yr", "10-yr", "2016"),
  value = c(survival_3yrNSM_2003_DOD, survival_5yrNSM_2003_DOD, survival_10yrNSM_2003_DOD, survivalrate_2016_NS_2003_DOD)
)

head(pie_2003_NS_DoD_df)

pie_2003_NS_DoD_df <- pie_2003_NS_DoD_df %>%
  dplyr::mutate(perc_value = value * 100) %>%
  dplyr::mutate(perc_value = round(perc_value, 0))

head(pie_2003_NS_DoD_df)

plot_2003_DoD_NS <- ggplot(pie_2003_NS_DoD_df, aes(x=group, y=perc_value, fill=group)) +
  scale_fill_manual(values=c("#99FFFF", "#0099CC", "#006699", "#999999")) +
  geom_bar(stat = "identity", width=0.4) +
  scale_x_discrete(limits=c("3-yr", "5-yr", "10-yr", "2016")) +
  ylim(c(0, 70)) +
  geom_text(aes(label=percent(value)), vjust=-.25, size=3) +
  labs(title = "- Non-Small Businesses DoD", y = "Survival Rates (percentages)", x = "Year") +
  guides(fill=FALSE)


##*****##
#combine plots#
#******##
#t <- textGrob("Survival Rates 2001 Sample", fontsize=42)
grid.arrange(plot_2003_SMALL, plot_2003_DoD_SMALL, plot_2003_ALL, plot_2003_DoD_ALL, plot_2003_NS, plot_2003_DoD_NS)


#**********************************************************************#


#**********#
####2004 all fed agencies####
#**********#
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


str(data_2004)

data_2004$survive_3yr<-as.numeric(as.character(data_2004$survive_3yr))
data_2004$survive_5yr<-as.numeric(as.character(data_2004$survive_5yr))
data_2004$survive_10yr<-as.numeric(as.character(data_2004$survive_10yr))
data_2004$survive_2016<-as.numeric(as.character(data_2004$survive_2016))

str(data_2004)

###
##t test to test the differences between small and non small survival##
table(data_2004$top_smallbiz_bin)
table(data_2004$survive_3yr)
table(data_2004$top_smallbiz_bin, data_2004$survive_3yr)

t.test(survive_3yr ~ top_smallbiz_bin, data = data_2004)

#5-year#
table(data_2004$top_smallbiz_bin, data_2004$survive_5yr)

t.test(survive_5yr ~ top_smallbiz_bin, data = data_2004)

#10-year#
table(data_2004$top_smallbiz_bin, data_2004$survive_10yr)

t.test(survive_10yr ~ top_smallbiz_bin, data = data_2004)



#********#
#**ALL**#
#********#

#***********#
#survival rates#
#**************#

##3-year##
table(data_2004$survive_3yr) #0=12449, 1=21743

numerator_3yrALL_2004 <- length(which(data_2004$survive_3yr==1))

denominator_3yrALL_2004 <- length(data_2004$survive_3yr)

survival_3yrALL_2004 <- numerator_3yrALL_2004/denominator_3yrALL_2004
survival_3yrALL_2004

##5-year##
table(data_2004$survive_5yr) #0=10168, 1=17266

numerator_5yrALL_2004 <- length(which(data_2004$survive_5yr==1))

denominator_5yrALL_2004 <- length(data_2004$survive_5yr)

survival_5yrALL_2004 <- numerator_5yrALL_2004/denominator_5yrALL_2004
survival_5yrALL_2004

##10-year##
table(data_2004$survive_10yr) #0=10168, 1=17266

numerator_10yrALL_2004 <- length(which(data_2004$survive_10yr==1))

denominator_10yrALL_2004 <- length(data_2004$survive_10yr)

survival_10yrALL_2004 <- numerator_10yrALL_2004/denominator_10yrALL_2004
survival_10yrALL_2004

#****************#
#Graduation Rates#
#****************#
table(data_2004$graduated)

numerator_grad_2004 <- length(which(data_2004$graduated==1))

denominator_grad_2004 <- length(data_2004$graduated)

graduated_2004 <- numerator_grad_2004/denominator_grad_2004
graduated_2004 

##graduation 10 year survivors only
numerator_grad_2004_10yr <- length(which(data_2004$graduated==1 & data_2004$survive_10yr==1))

#denominator_grad_2004_10yr <- length(data_2004$graduated)

denominator_grad_2004_10yr <- length(which(data_2004$top_smallbiz_bin==1))

graduated_2004_10yr <- numerator_grad_2004_10yr/denominator_grad_2004_10yr
graduated_2004_10yr 



#******#
#*SMALL*#
#******#
##3-year##
table(data_2004$survive_3yr) #0=12449, 1=21743

numerator_3yrSM_2004 <- length(which(data_2004$survive_3yr==1 & data_2004$top_smallbiz_bin==1))

denominator_3yrSM_2004 <- length(which(data_2004$top_smallbiz_bin==1))

survival_3yrSM_2004 <- numerator_3yrSM_2004/denominator_3yrSM_2004
survival_3yrSM_2004

##5-year##
table(data_2004$survive_5yr) #0=10168, 1=17266

numerator_5yrSM_2004 <- length(which(data_2004$survive_5yr==1 & data_2004$top_smallbiz_bin==1))

denominator_5yrSM_2004 <- length(which(data_2004$top_smallbiz_bin==1))

survival_5yrSM_2004 <- numerator_5yrSM_2004/denominator_5yrSM_2004
survival_5yrSM_2004

##10-year##
table(data_2004$survive_10yr) #0=10168, 1=17266

numerator_10yrSM_2004 <- length(which(data_2004$survive_10yr==1 & data_2004$top_smallbiz_bin==1))

denominator_10yrSM_2004 <- length(which(data_2004$top_smallbiz_bin==1))

survival_10yrSM_2004 <- numerator_10yrSM_2004/denominator_10yrSM_2004
survival_10yrSM_2004



#********#
#NON SMALL#
#********#
##3-year##
table(data_2004$survive_3yr) #0=12449, 1=21743

numerator_3yrNSM_2004 <- length(which(data_2004$survive_3yr==1 & data_2004$top_smallbiz_bin==0))

denominator_3yrNSM_2004 <- length(which(data_2004$top_smallbiz_bin==0))

survival_3yrNSM_2004 <- numerator_3yrNSM_2004/denominator_3yrNSM_2004
survival_3yrNSM_2004 

##5-year##
table(data_2004$survive_5yr) #0=10168, 1=17266

numerator_5yrNSM_2004 <- length(which(data_2004$survive_5yr==1 & data_2004$top_smallbiz_bin==0))

denominator_5yrNSM_2004 <- length(which(data_2004$top_smallbiz_bin==0))

survival_5yrNSM_2004 <- numerator_5yrNSM_2004/denominator_5yrNSM_2004
survival_5yrNSM_2004

##10-year##
table(data_2004$survive_10yr) #0=10168, 1=17266

numerator_10yrNSM_2004 <- length(which(data_2004$survive_10yr==1 & data_2004$top_smallbiz_bin==0))

denominator_10yrNSM_2004 <- length(which(data_2004$top_smallbiz_bin==0))

survival_10yrNSM_2004 <- numerator_10yrNSM_2004/denominator_10yrNSM_2004
survival_10yrNSM_2004 

#*******************#
#******2016 check***#
#*******************#
##*********
#***2016 survival 
#**********
survive_10yr_count_2004 <- length(which(data_2004$survive_10yr==1))
##ALL##
numerator_2016_ALL_2004 <- length(which(data_2004$survive_2016==1))

denominator_2016_ALL_2004 <- length(data_2004$survive_2016)

survivalrate_2016_ALL_2004 <- numerator_2016_ALL_2004 /denominator_2016_ALL_2004
survivalrate_2016_ALL_2004 


##SMALL##
numerator_2016_SM_2004 <- length(which(data_2004$survive_2016==1 & data_2004$top_smallbiz_bin==1))

denominator_2016_SM_2004 <- length(which(data_2004$top_smallbiz_bin==1))

survivalrate_2016_SM_2004 <- numerator_2016_SM_2004 /denominator_2016_SM_2004 
survivalrate_2016_SM_2004 


##NONSMALL##
numerator_2016_NS_2004 <- length(which(data_2004$survive_2016==1 & data_2004$top_smallbiz_bin==0))

denominator_2016_NS_2004 <- length(which(data_2004$top_smallbiz_bin==0))

survivalrate_2016_NS_2004  <- numerator_2016_NS_2004 /denominator_2016_NS_2004 
survivalrate_2016_NS_2004 


table(data_2004$top_smallbiz_bin, data_2004$survive_2016)


t.test(survive_2016 ~ top_smallbiz_bin, data = data_2004)

##**************##
####Pie Charts for 2004####
#****************#

##******##
##ALL NE##
##******##
pie_2004_ALL_df <- data.frame(
  group = c("3-yr", "5-yr", "10-yr", "2016"),
  value = c(survival_3yrALL_2004, survival_5yrALL_2004, survival_10yrALL_2004, survivalrate_2016_ALL_2004)
)

head(pie_2004_ALL_df)

pie_2004_ALL_df <- pie_2004_ALL_df %>%
  dplyr::mutate(perc_value = value * 100) %>%
  dplyr::mutate(perc_value = round(perc_value, 0))

head(pie_2004_ALL_df)

plot_2004_ALL <- ggplot(pie_2004_ALL_df, aes(x=group, y=perc_value, fill=group)) +
  scale_fill_manual(values=c("#99FFFF", "#0099CC", "#006699", "#999999" )) +
  geom_bar(stat = "identity", width=0.4) +
  scale_x_discrete(limits=c("3-yr", "5-yr", "10-yr", "2016")) +
  ylim(c(0, 70)) +
  geom_text(aes(label=percent(value)), size=3, vjust=-.25) +
  labs(title = "- All New Entrants All Fed", y = "Survival Rates (percentages)", x = "Year") + 
  guides(fill=FALSE)


##******##
##SMALL NE##
##******##
pie_2004_SM_df <- data.frame(
  group = c("3-yr", "5-yr", "10-yr", "2016", "Graduated"),
  value = c(survival_3yrSM_2004, survival_5yrSM_2004, survival_10yrSM_2004, survivalrate_2016_SM_2004, graduated_2004_10yr)
)

head(pie_2004_SM_df)

pie_2004_SM_df <- pie_2004_SM_df %>%
  dplyr::mutate(perc_value = value * 100) %>%
  dplyr::mutate(perc_value = round(perc_value, 0))

head(pie_2004_SM_df)

plot_2004_SMALL <- ggplot(pie_2004_SM_df, aes(x=group, y=perc_value, fill=group)) +
  scale_fill_manual(values=c("#99FFFF", "#0099CC", "#006699", "#999999", "#000066" )) +
  geom_bar(stat = "identity", width=0.4) +
  scale_x_discrete(limits=c("3-yr", "5-yr", "10-yr", "2016", "Graduated")) +
  ylim(c(0, 70)) +
  geom_text(aes(label=percent(value)), vjust=-.25, size=3) +
  labs(title = "Survival Rates 2004 All Federal Agencies - Small Businesses", y = "Survival Rates (percentages)", x = "Year") + 
  guides(fill=FALSE)

##******##
##Non-Small NE##
##******##
pie_2004_NS_df <- data.frame(
  group = c("3-yr", "5-yr", "10-yr", "2016"),
  value = c(survival_3yrNSM_2004, survival_5yrNSM_2004, survival_10yrNSM_2004, survivalrate_2016_NS_2004)
)

head(pie_2004_NS_df)

pie_2004_NS_df <- pie_2004_NS_df %>%
  dplyr::mutate(perc_value = value * 100) %>%
  dplyr::mutate(perc_value = round(perc_value, 0))

head(pie_2004_NS_df)

plot_2004_NS <- ggplot(pie_2004_NS_df, aes(x=group, y=perc_value, fill=group)) +
  scale_fill_manual(values=c("#99FFFF", "#0099CC", "#006699", "#999999")) +
  geom_bar(stat = "identity", width=0.4) +
  scale_x_discrete(limits=c("3-yr", "5-yr", "10-yr", "2016")) +
  ylim(c(0, 70)) +
  geom_text(aes(label=percent(value)), vjust=-.25, size=3) +
  labs(title = "- Non-Small Businesses All Fed", y = "Survival Rates (percentages)", x = "Year") +
  guides(fill=FALSE)

##*****##
#combine plots#
#******##
#t <- textGrob("Survival Rates 2001 Sample", fontsize=42)
grid.arrange(plot_2004_SMALL, plot_2004_ALL, plot_2004_NS)


#***************#
#### 2004 DOD ONLY####
#**************#
data_2004_DOD <- FPDS_cleaned_unique[!(FPDS_cleaned_unique$registrationYear!="2004"), ]
data_2004_DOD <- data_2004_DOD[!(data_2004_DOD$customer!="Defense"), ]

##create variable describing whether a firm survived 3 years

data_2004_DOD <- data_2004_DOD %>%
  dplyr::mutate(survive_3yr = ifelse(exitYear < 2006, "0", "1")) 

##create variable describing whether a firm survived 5 years
data_2004_DOD <- data_2004_DOD %>%
  dplyr::mutate(survive_5yr = ifelse(exitYear < 2009, "0", "1")) 


##create variable describing whether a firm survived 10 years
data_2004_DOD <- data_2004_DOD %>%
  dplyr::mutate(survive_10yr = ifelse(exitYear < 2013, "0", "1")) 

##create variable describing whether a firm survived in 2016
data_2004_DOD <- data_2004_DOD %>%
  dplyr::mutate(survive_2016 = ifelse(exitYear < 2016, "0", "1"))


str(data_2004)

data_2004_DOD$survive_3yr<-as.numeric(as.character(data_2004_DOD$survive_3yr))
data_2004_DOD$survive_5yr<-as.numeric(as.character(data_2004_DOD$survive_5yr))
data_2004_DOD$survive_10yr<-as.numeric(as.character(data_2004_DOD$survive_10yr))
data_2004_DOD$survive_2016<-as.numeric(as.character(data_2004_DOD$survive_2016))

str(data_2004_DOD)

###
##t test to test the differences between small and non small survival##
table(data_2004_DOD$top_smallbiz_bin)
table(data_2004_DOD$survive_3yr)
table(data_2004_DOD$top_smallbiz_bin, data_2004_DOD$survive_3yr)

t.test(survive_3yr ~ top_smallbiz_bin, data = data_2004_DOD)

#5-year#
table(data_2004_DOD$top_smallbiz_bin, data_2004_DOD$survive_5yr)

t.test(survive_5yr ~ top_smallbiz_bin, data = data_2004_DOD)

#10-year#
table(data_2004_DOD$top_smallbiz_bin, data_2004_DOD$survive_10yr)

t.test(survive_10yr ~ top_smallbiz_bin, data = data_2004_DOD)


#********#
#**ALL**#
#********#

#***********#
#survival rates#
#**************#

##3-year##
table(data_2004_DOD$survive_3yr) #0=12449, 1=21743

numerator_3yrALL_2004_DOD <- length(which(data_2004_DOD$survive_3yr==1))

denominator_3yrALL_2004_DOD <- length(data_2004_DOD$survive_3yr)

survival_3yrALL_2004_DOD <- numerator_3yrALL_2004_DOD/denominator_3yrALL_2004_DOD
survival_3yrALL_2004_DOD

##5-year##
table(data_2004_DOD$survive_5yr) #0=10168, 1=17266

numerator_5yrALL_2004_DOD <- length(which(data_2004_DOD$survive_5yr==1))

denominator_5yrALL_2004_DOD <- length(data_2004_DOD$survive_5yr)

survival_5yrALL_2004_DOD <- numerator_5yrALL_2004_DOD/denominator_5yrALL_2004_DOD
survival_5yrALL_2004_DOD

##10-year##
table(data_2004_DOD$survive_10yr) #0=10168, 1=17266

numerator_10yrALL_2004_DOD <- length(which(data_2004_DOD$survive_10yr==1))

denominator_10yrALL_2004_DOD <- length(data_2004_DOD$survive_10yr)

survival_10yrALL_2004_DOD <- numerator_10yrALL_2004_DOD/denominator_10yrALL_2004_DOD
survival_10yrALL_2004_DOD

#****************#
#Graduation Rates#
#****************#
table(data_2004_DOD$graduated)

numerator_grad_2004_DOD <- length(which(data_2004_DOD$graduated==1))

denominator_grad_2004_DOD <- length(data_2004_DOD$graduated)

graduated_2004_DOD <- numerator_grad_2004_DOD/denominator_grad_2004_DOD
graduated_2004_DOD 

##those who survived 10 years only
numerator_grad_2004_DOD_10yr <- length(which(data_2004_DOD$graduated==1 & data_2004_DOD$survive_10yr==1))

#denominator_grad_2004_DOD_10yr <- length(data_2004_DOD$graduated)

denominator_grad_2004_DOD_10yr <- length(which(data_2004_DOD$top_smallbiz_bin==1))

graduated_2004_DOD_10yr <- numerator_grad_2004_DOD_10yr/denominator_grad_2004_DOD_10yr
graduated_2004_DOD_10yr 


#******#
#*SMALL*#
#******#
##3-year##
table(data_2004_DOD$survive_3yr) #0=12449, 1=21743

numerator_3yrSM_2004_DOD <- length(which(data_2004_DOD$survive_3yr==1 & data_2004_DOD$top_smallbiz_bin==1))

denominator_3yrSM_2004_DOD <- length(which(data_2004_DOD$top_smallbiz_bin==1))

survival_3yrSM_2004_DOD <- numerator_3yrSM_2004_DOD/denominator_3yrSM_2004_DOD
survival_3yrSM_2004_DOD

##5-year##
table(data_2004_DOD$survive_5yr) #0=10168, 1=17266

numerator_5yrSM_2004_DOD <- length(which(data_2004_DOD$survive_5yr==1 & data_2004_DOD$top_smallbiz_bin==1))

denominator_5yrSM_2004_DOD <- length(which(data_2004_DOD$top_smallbiz_bin==1))

survival_5yrSM_2004_DOD <- numerator_5yrSM_2004_DOD/denominator_5yrSM_2004_DOD
survival_5yrSM_2004_DOD

##10-year##
table(data_2004_DOD$survive_10yr) #0=10168, 1=17266

numerator_10yrSM_2004_DOD <- length(which(data_2004_DOD$survive_10yr==1 & data_2004_DOD$top_smallbiz_bin==1))

denominator_10yrSM_2004_DOD <- length(which(data_2004_DOD$top_smallbiz_bin==1))

survival_10yrSM_2004_DOD <- numerator_10yrSM_2004_DOD/denominator_10yrSM_2004_DOD
survival_10yrSM_2004_DOD



#********#
#NON SMALL#
#********#
##3-year##
table(data_2004_DOD$survive_3yr) #0=12449, 1=21743

numerator_3yrNSM_2004_DOD <- length(which(data_2004_DOD$survive_3yr==1 & data_2004_DOD$top_smallbiz_bin==0))

denominator_3yrNSM_2004_DOD <- length(which(data_2004_DOD$top_smallbiz_bin==0))

survival_3yrNSM_2004_DOD <- numerator_3yrNSM_2004_DOD/denominator_3yrNSM_2004_DOD
survival_3yrNSM_2004_DOD 

##5-year##
table(data_2004_DOD$survive_5yr) #0=10168, 1=17266

numerator_5yrNSM_2004_DOD <- length(which(data_2004_DOD$survive_5yr==1 & data_2004_DOD$top_smallbiz_bin==0))

denominator_5yrNSM_2004_DOD <- length(which(data_2004_DOD$top_smallbiz_bin==0))

survival_5yrNSM_2004_DOD <- numerator_5yrNSM_2004_DOD/denominator_5yrNSM_2004_DOD
survival_5yrNSM_2004_DOD

##10-year##
table(data_2004_DOD$survive_10yr) #0=10168, 1=17266

numerator_10yrNSM_2004_DOD <- length(which(data_2004_DOD$survive_10yr==1 & data_2004_DOD$top_smallbiz_bin==0))

denominator_10yrNSM_2004_DOD <- length(which(data_2004_DOD$top_smallbiz_bin==0))

survival_10yrNSM_2004_DOD <- numerator_10yrNSM_2004_DOD/denominator_10yrNSM_2004_DOD
survival_10yrNSM_2004_DOD 

#*******************#
#******2016 check***#
#*******************#
##*********
#***2016 survival 
#**********
survive_10yr_count_2004_DOD <- length(which(data_2004_DOD$survive_10yr==1))
##ALL##
numerator_2016_ALL_2004_DOD <- length(which(data_2004_DOD$survive_2016==1))

denominator_2016_ALL_2004_DOD <- length(data_2004_DOD$survive_2016)

survivalrate_2016_ALL_2004_DOD <- numerator_2016_ALL_2004_DOD /denominator_2016_ALL_2004_DOD
survivalrate_2016_ALL_2004_DOD 


##SMALL##
numerator_2016_SM_2004_DOD <- length(which(data_2004_DOD$survive_2016==1 & data_2004_DOD$top_smallbiz_bin==1))

denominator_2016_SM_2004_DOD <- length(which(data_2004_DOD$top_smallbiz_bin==1))

survivalrate_2016_SM_2004_DOD <- numerator_2016_SM_2004_DOD /denominator_2016_SM_2004_DOD 
survivalrate_2016_SM_2004_DOD 


##NONSMALL##
numerator_2016_NS_2004_DOD <- length(which(data_2004_DOD$survive_2016==1 & data_2004_DOD$top_smallbiz_bin==0))

denominator_2016_NS_2004_DOD <- length(which(data_2004_DOD$top_smallbiz_bin==0))

survivalrate_2016_NS_2004_DOD  <- numerator_2016_NS_2004_DOD /denominator_2016_NS_2004_DOD 
survivalrate_2016_NS_2004_DOD 


table(data_2004_DOD$top_smallbiz_bin, data_2004_DOD$survive_2016)


t.test(survive_2016 ~ top_smallbiz_bin, data = data_2004_DOD)

##**************##
####Pie Charts for 2004 DoD ####
#****************#

##******##
##ALL NE##
##******##
pie_2004_ALL_DoD_df <- data.frame(
  group = c("3-yr", "5-yr", "10-yr", "2016"),
  value = c(survival_3yrALL_2004_DOD, survival_5yrALL_2004_DOD, survival_10yrALL_2004_DOD, survivalrate_2016_ALL_2004_DOD)
)

head(pie_2004_ALL_DoD_df)

pie_2004_ALL_DoD_df <- pie_2004_ALL_DoD_df %>%
  dplyr::mutate(perc_value = value * 100) %>%
  dplyr::mutate(perc_value = round(perc_value, 0))

head(pie_2004_ALL_DoD_df)

plot_2004_DoD_ALL <- ggplot(pie_2004_ALL_DoD_df, aes(x=group, y=perc_value, fill=group)) +
  scale_fill_manual(values=c("#99FFFF", "#0099CC", "#006699", "#999999" )) +
  geom_bar(stat = "identity", width=0.4) +
  scale_x_discrete(limits=c("3-yr", "5-yr", "10-yr", "2016")) +
  ylim(c(0, 70)) +
  geom_text(aes(label=percent(value)), size=3, vjust=-.25) +
  labs(title = "- All New Entrants DoD", y = "Survival Rates (percentages)", x = "Year") + 
  guides(fill=FALSE)


##******##
##SMALL NE##
##******##
pie_2004_SM_DoD_df <- data.frame(
  group = c("3-yr", "5-yr", "10-yr", "2016", "Graduated"),
  value = c(survival_3yrALL_2004_DOD, survival_5yrALL_2004_DOD, survival_10yrALL_2004_DOD, survivalrate_2016_ALL_2004_DOD, graduated_2004_DOD_10yr)
)

head(pie_2004_SM_DoD_df)

pie_2004_SM_DoD_df <- pie_2004_SM_DoD_df %>%
  dplyr::mutate(perc_value = value * 100) %>%
  dplyr::mutate(perc_value = round(perc_value, 0))

head(pie_2004_SM_DoD_df)

plot_2004_DoD_SMALL <- ggplot(pie_2004_SM_DoD_df, aes(x=group, y=perc_value, fill=group)) +
  scale_fill_manual(values=c("#99FFFF", "#0099CC", "#006699", "#999999", "#000066")) +
  geom_bar(stat = "identity", width=0.4) +
  scale_x_discrete(limits=c("3-yr", "5-yr", "10-yr", "2016", "Graduated")) +
  ylim(c(0, 70)) +
  geom_text(aes(label=percent(value)), vjust=-.25, size=3) +
  labs(title = "Survival Rates 2004 DoD - Small Businesses", y = "Survival Rates (percentages)", x = "Year")

##******##
##Non-Small NE##
##******##
pie_2004_NS_DoD_df <- data.frame(
  group = c("3-yr", "5-yr", "10-yr", "2016"),
  value = c(survival_3yrALL_2004_DOD, survival_5yrALL_2004_DOD, survival_10yrALL_2004_DOD, survivalrate_2016_ALL_2004_DOD)
)

head(pie_2004_NS_DoD_df)

pie_2004_NS_DoD_df <- pie_2004_NS_DoD_df %>%
  dplyr::mutate(perc_value = value * 100) %>%
  dplyr::mutate(perc_value = round(perc_value, 0))

head(pie_2004_NS_DoD_df)

plot_2004_DoD_NS <- ggplot(pie_2004_NS_DoD_df, aes(x=group, y=perc_value, fill=group)) +
  scale_fill_manual(values=c("#99FFFF", "#0099CC", "#006699", "#999999")) +
  geom_bar(stat = "identity", width=0.4) +
  scale_x_discrete(limits=c("3-yr", "5-yr", "10-yr", "2016")) +
  ylim(c(0, 70)) +
  geom_text(aes(label=percent(value)), vjust=-.25, size=3) +
  labs(title = "- Non-Small Businesses DoD", y = "Survival Rates (percentages)", x = "Year") +
  guides(fill=FALSE)


##*****##
#combine plots#
#******##
#t <- textGrob("Survival Rates 2001 Sample", fontsize=42)
grid.arrange(plot_2004_SMALL, plot_2004_DoD_SMALL, plot_2004_ALL, plot_2004_DoD_ALL, plot_2004_NS, plot_2004_DoD_NS)

#**********************************************************************#



#**********#
####2005 all fed agencies####
#***********#

##calculate the necessary vars##
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


str(data_2005)

data_2005$survive_3yr<-as.numeric(as.character(data_2005$survive_3yr))
data_2005$survive_5yr<-as.numeric(as.character(data_2005$survive_5yr))
data_2005$survive_10yr<-as.numeric(as.character(data_2005$survive_10yr))
data_2005$survive_2016<-as.numeric(as.character(data_2005$survive_2016))

str(data_2005)

##**********
##t test to test the differences between small and non small survival##
table(data_2005$top_smallbiz_bin)
table(data_2005$survive_3yr)
table(data_2005$top_smallbiz_bin, data_2005$survive_3yr)

t.test(survive_3yr ~ top_smallbiz_bin, data = data_2005)

#5-year#
table(data_2005$top_smallbiz_bin, data_2005$survive_5yr)

t.test(survive_5yr ~ top_smallbiz_bin, data = data_2005)

#10-year#
table(data_2005$top_smallbiz_bin, data_2005$survive_10yr)

t.test(survive_10yr ~ top_smallbiz_bin, data = data_2005)


#******#
#**ALL*#
#*****#
#*************#
#survival rates#
#***************#
##3-year##
table(data_2005$survive_3yr) #0=12449, 1=21743

numerator_3yrALL_2005 <- length(which(data_2005$survive_3yr==1))

denominator_3yrALL_2005 <- length(data_2005$survive_3yr)

survival_3yrALL_2005 <- numerator_3yrALL_2005/denominator_3yrALL_2005
survival_3yrALL_2005

##5-year##
table(data_2005$survive_5yr) #0=10168, 1=17266

numerator_5yrALL_2005 <- length(which(data_2005$survive_5yr==1))

denominator_5yrALL_2005 <- length(data_2005$survive_5yr)

survival_5yrALL_2005 <- numerator_5yrALL_2005/denominator_5yrALL_2005
survival_5yrALL_2005

##10-year##
table(data_2005$survive_10yr) #0=10168, 1=17266

numerator_10yrALL_2005 <- length(which(data_2005$survive_10yr==1))

denominator_10yrALL_2005 <- length(data_2005$survive_10yr)

survival_10yrALL_2005 <- numerator_10yrALL_2005/denominator_10yrALL_2005
survival_10yrALL_2005 
#****************#
#Graduation Rates#
#****************#
table(data_2005$graduated)

numerator_grad_2005 <- length(which(data_2005$graduated==1))

denominator_grad_2005 <- length(data_2005$graduated)

graduated_2005 <- numerator_grad_2005/denominator_grad_2005
graduated_2005

##for those that survived 10 years only
numerator_grad_2005_10yr <- length(which(data_2005$graduated==1 & data_2005$survive_10yr==1))

#denominator_grad_2005_10yr <- length(data_2005$graduated)

denominator_grad_2005_10yr <- length(which(data_2005$top_smallbiz_bin==1))

graduated_2005_10yr <- numerator_grad_2005_10yr/denominator_grad_2005_10yr
graduated_2005_10yr



#******#
#SMALL#
#******#
##3-year##
table(data_2005$survive_3yr) #0=12449, 1=21743

numerator_3yrSM_2005 <- length(which(data_2005$survive_3yr==1 & data_2005$top_smallbiz_bin==1))

denominator_3yrSM_2005 <- length(which(data_2005$top_smallbiz_bin==1))

survival_3yrSM_2005 <- numerator_3yrSM_2005/denominator_3yrSM_2005
survival_3yrSM_2005

##5-year##
table(data_2005$survive_5yr) #0=10168, 1=17266

numerator_5yrSM_2005 <- length(which(data_2005$survive_5yr==1 & data_2005$top_smallbiz_bin==1))

denominator_5yrSM_2005 <- length(which(data_2005$top_smallbiz_bin==1))

survival_5yrSM_2005 <- numerator_5yrSM_2005/denominator_5yrSM_2005
survival_5yrSM_2005 

##10-year##
table(data_2005$survive_10yr) #0=10168, 1=17266

numerator_10yrSM_2005 <- length(which(data_2005$survive_10yr==1 & data_2005$top_smallbiz_bin==1))

denominator_10yrSM_2005 <- length(which(data_2005$top_smallbiz_bin==1))

survival_10yrSM_2005 <- numerator_10yrSM_2005/denominator_10yrSM_2005
survival_10yrSM_2005

#*******#
#NON SMAL#
#*******#
##3-year##
table(data_2005$survive_3yr) #0=12449, 1=21743

numerator_3yrNSM_2005 <- length(which(data_2005$survive_3yr==1 & data_2005$top_smallbiz_bin==0))

denominator_3yrNSM_2005 <- length(which(data_2005$top_smallbiz_bin==0))

survival_3yrNSM_2005 <- numerator_3yrNSM_2005/denominator_3yrNSM_2005
survival_3yrNSM_2005

##5-year##
table(data_2005$survive_5yr) #0=10168, 1=17266

numerator_5yrNSM_2005 <- length(which(data_2005$survive_5yr==1 & data_2005$top_smallbiz_bin==0))

denominator_5yrNSM_2005 <- length(which(data_2005$top_smallbiz_bin==0))

survival_5yrNSM_2005 <- numerator_5yrNSM_2005/denominator_5yrNSM_2005
survival_5yrNSM_2005

##10-year##
table(data_2005$survive_10yr) #0=10168, 1=17266

numerator_10yrNSM_2005 <- length(which(data_2005$survive_10yr==1 & data_2005$top_smallbiz_bin==0))

denominator_10yrNSM_2005 <- length(which(data_2005$top_smallbiz_bin==0))

survival_10yrNSM_2005 <- numerator_10yrNSM_2005/denominator_10yrNSM_2005
survival_10yrNSM_2005

#*******************#
#******2016 check***#
#*******************#
##*********
#***2016 survival 
#**********
survive_10yr_count_2005 <- length(which(data_2005$survive_10yr==1))
##ALL##
numerator_2016_ALL_2005<- length(which(data_2005$survive_2016==1))

denominator_2016_ALL_2005 <- length(data_2005$survive_2016)

survivalrate_2016_ALL_2005 <- numerator_2016_ALL_2005 /denominator_2016_ALL_2005
survivalrate_2016_ALL_2005 


##SMALL##
numerator_2016_SM_2005 <- length(which(data_2005$survive_2016==1 & data_2005$top_smallbiz_bin==1))

denominator_2016_SM_2005 <- length(which(data_2005$top_smallbiz_bin==1))

survivalrate_2016_SM_2005 <- numerator_2016_SM_2005 /denominator_2016_SM_2005 
survivalrate_2016_SM_2005


##NONSMALL##
numerator_2016_NS_2005<- length(which(data_2005$survive_2016==1 & data_2005$top_smallbiz_bin==0))

denominator_2016_NS_2005 <- length(which(data_2005$top_smallbiz_bin==0))

survivalrate_2016_NS_2005  <- numerator_2016_NS_2005 /denominator_2016_NS_2005
survivalrate_2016_NS_2005 


table(data_2005$top_smallbiz_bin, data_2005$survive_2016)


t.test(survive_2016 ~ top_smallbiz_bin, data = data_2005)

##**************##
####Pie Charts for 2005####
#****************#

##******##
##ALL NE##
##******##
pie_2005_ALL_df <- data.frame(
  group = c("3-yr", "5-yr", "10-yr", "2016"),
  value = c(survival_3yrALL_2005, survival_5yrALL_2005, survival_10yrALL_2005, survivalrate_2016_ALL_2005)
)

head(pie_2005_ALL_df)

pie_2005_ALL_df <- pie_2005_ALL_df %>%
  dplyr::mutate(perc_value = value * 100) %>%
  dplyr::mutate(perc_value = round(perc_value, 0))

head(pie_2005_ALL_df)

plot_2005_ALL <- ggplot(pie_2005_ALL_df, aes(x=group, y=perc_value, fill=group)) +
  scale_fill_manual(values=c("#99FFFF", "#0099CC", "#006699", "#999999" )) +
  geom_bar(stat = "identity", width=0.4) +
  scale_x_discrete(limits=c("3-yr", "5-yr", "10-yr", "2016")) +
  ylim(c(0, 70)) +
  geom_text(aes(label=percent(value)), size=3, vjust=-.25) +
  labs(title = "- All New Entrants All Fed", y = "Survival Rates (percentages)", x = "Year") + 
  guides(fill=FALSE)


##******##
##SMALL NE##
##******##
pie_2005_SM_df <- data.frame(
  group = c("3-yr", "5-yr", "10-yr", "2016", "Graduated"),
  value = c(survival_3yrSM_2005, survival_5yrSM_2005, survival_10yrSM_2005, survivalrate_2016_SM_2005, graduated_2005_10yr)
)

head(pie_2005_SM_df)

pie_2005_SM_df <- pie_2005_SM_df %>%
  dplyr::mutate(perc_value = value * 100) %>%
  dplyr::mutate(perc_value = round(perc_value, 0))

head(pie_2005_SM_df)

plot_2005_SMALL <- ggplot(pie_2005_SM_df, aes(x=group, y=perc_value, fill=group)) +
  scale_fill_manual(values=c("#99FFFF", "#0099CC", "#006699", "#999999", "#000066" )) +
  geom_bar(stat = "identity", width=0.4) +
  scale_x_discrete(limits=c("3-yr", "5-yr", "10-yr", "2016", "Graduated")) +
  ylim(c(0, 70)) +
  geom_text(aes(label=percent(value)), vjust=-.25, size=3) +
  labs(title = "Survival Rates 2005 All Federal Agencies - Small Businesses", y = "Survival Rates (percentages)", x = "Year") + 
  guides(fill=FALSE)

##******##
##Non-Small NE##
##******##
pie_2005_NS_df <- data.frame(
  group = c("3-yr", "5-yr", "10-yr", "2016"),
  value = c(survival_3yrNSM_2005, survival_5yrNSM_2005, survival_10yrNSM_2005, survivalrate_2016_NS_2005)
)

head(pie_2005_NS_df)

pie_2005_NS_df <- pie_2005_NS_df %>%
  dplyr::mutate(perc_value = value * 100) %>%
  dplyr::mutate(perc_value = round(perc_value, 0))

head(pie_2005_NS_df)

plot_2005_NS <- ggplot(pie_2005_NS_df, aes(x=group, y=perc_value, fill=group)) +
  scale_fill_manual(values=c("#99FFFF", "#0099CC", "#006699", "#999999")) +
  geom_bar(stat = "identity", width=0.4) +
  scale_x_discrete(limits=c("3-yr", "5-yr", "10-yr", "2016")) +
  ylim(c(0, 70)) +
  geom_text(aes(label=percent(value)), vjust=-.25, size=3) +
  labs(title = "- Non-Small Businesses All Fed", y = "Survival Rates (percentages)", x = "Year") +
  guides(fill=FALSE)

##*****##
#combine plots#
#******##
#t <- textGrob("Survival Rates 2001 Sample", fontsize=42)
grid.arrange(plot_2005_SMALL, plot_2005_ALL, plot_2005_NS)


#************#
####2005 DOD ONLY####
#**************#
data_2005_DOD <- FPDS_cleaned_unique[!(FPDS_cleaned_unique$registrationYear!="2005"), ]
data_2005_DOD <- data_2005_DOD[!(data_2005_DOD$customer!="Defense"), ]


##create variable describing whether a firm survived 3 years

data_2005_DOD <- data_2005_DOD %>%
  dplyr::mutate(survive_3yr = ifelse(exitYear < 2007, "0", "1")) 

##create variable describing whether a firm survived 5 years
data_2005_DOD <- data_2005_DOD %>%
  dplyr::mutate(survive_5yr = ifelse(exitYear < 2010, "0", "1")) 


##create variable describing whether a firm survived 10 years
data_2005_DOD <- data_2005_DOD %>%
  dplyr::mutate(survive_10yr = ifelse(exitYear < 2014, "0", "1")) 

##create variable describing whether a firm survived in 2016
data_2005_DOD <- data_2005_DOD %>%
  dplyr::mutate(survive_2016 = ifelse(exitYear < 2016, "0", "1"))


str(data_2005_DOD)

data_2005_DOD$survive_3yr<-as.numeric(as.character(data_2005_DOD$survive_3yr))
data_2005_DOD$survive_5yr<-as.numeric(as.character(data_2005_DOD$survive_5yr))
data_2005_DOD$survive_10yr<-as.numeric(as.character(data_2005_DOD$survive_10yr))
data_2005_DOD$survive_2016<-as.numeric(as.character(data_2005_DOD$survive_2016))

str(data_2005_DOD)

##**********
##t test to test the differences between small and non small survival##
table(data_2005_DOD$top_smallbiz_bin)
table(data_2005_DOD$survive_3yr)
table(data_2005_DOD$top_smallbiz_bin, data_2005_DOD$survive_3yr)

t.test(survive_3yr ~ top_smallbiz_bin, data = data_2005_DOD)

#5-year#
table(data_2005_DOD$top_smallbiz_bin, data_2005_DOD$survive_5yr)

t.test(survive_5yr ~ top_smallbiz_bin, data = data_2005_DOD)

#10-year#
table(data_2005_DOD$top_smallbiz_bin, data_2005_DOD$survive_10yr)

t.test(survive_10yr ~ top_smallbiz_bin, data = data_2005_DOD)

#******#
#**ALL*#
#*****#
#*************#
#survival rates#
#***************#
##3-year##
table(data_2005_DOD$survive_3yr) #0=12449, 1=21743

numerator_3yrALL_2005_DOD <- length(which(data_2005_DOD$survive_3yr==1))

denominator_3yrALL_2005_DOD <- length(data_2005_DOD$survive_3yr)

survival_3yrALL_2005_DOD <- numerator_3yrALL_2005_DOD/denominator_3yrALL_2005_DOD
survival_3yrALL_2005_DOD

##5-year##
table(data_2005_DOD$survive_5yr) #0=10168, 1=17266

numerator_5yrALL_2005_DOD <- length(which(data_2005_DOD$survive_5yr==1))

denominator_5yrALL_2005_DOD <- length(data_2005_DOD$survive_5yr)

survival_5yrALL_2005_DOD <- numerator_5yrALL_2005_DOD/denominator_5yrALL_2005_DOD
survival_5yrALL_2005_DOD

##10-year##
table(data_2005_DOD$survive_10yr) #0=10168, 1=17266

numerator_10yrALL_2005_DOD <- length(which(data_2005_DOD$survive_10yr==1))

denominator_10yrALL_2005_DOD <- length(data_2005_DOD$survive_10yr)

survival_10yrALL_2005_DOD <- numerator_10yrALL_2005_DOD/denominator_10yrALL_2005_DOD
survival_10yrALL_2005_DOD 
#****************#
#Graduation Rates#
#****************#
table(data_2005_DOD$graduated)

numerator_grad_2005_DOD <- length(which(data_2005_DOD$graduated==1))

denominator_grad_2005_DOD <- length(data_2005_DOD$graduated)

graduated_2005_DOD <- numerator_grad_2005_DOD/denominator_grad_2005_DOD
graduated_2005_DOD

##survived 10 years only
numerator_grad_2005_DOD_10yr <- length(which(data_2005_DOD$graduated==1 & data_2005_DOD$survive_10yr==1))

#denominator_grad_2005_DOD_10yr <- length(data_2005_DOD$graduated)

denominator_grad_2005_DOD_10yr <- length(which(data_2005_DOD$top_smallbiz_bin==1))

graduated_2005_DOD_10yr <- numerator_grad_2005_DOD_10yr/denominator_grad_2005_DOD_10yr
graduated_2005_DOD_10yr


#******#
#SMALL#
#******#
##3-year##
table(data_2005_DOD$survive_3yr) #0=12449, 1=21743

numerator_3yrSM_2005_DOD <- length(which(data_2005_DOD$survive_3yr==1 & data_2005_DOD$top_smallbiz_bin==1))

denominator_3yrSM_2005_DOD <- length(which(data_2005_DOD$top_smallbiz_bin==1))

survival_3yrSM_2005_DOD <- numerator_3yrSM_2005_DOD/denominator_3yrSM_2005_DOD
survival_3yrSM_2005_DOD

##5-year##
table(data_2005_DOD$survive_5yr) #0=10168, 1=17266

numerator_5yrSM_2005_DOD <- length(which(data_2005_DOD$survive_5yr==1 & data_2005_DOD$top_smallbiz_bin==1))

denominator_5yrSM_2005_DOD <- length(which(data_2005_DOD$top_smallbiz_bin==1))

survival_5yrSM_2005_DOD <- numerator_5yrSM_2005_DOD/denominator_5yrSM_2005_DOD
survival_5yrSM_2005_DOD 

##10-year##
table(data_2005_DOD$survive_10yr) #0=10168, 1=17266

numerator_10yrSM_2005_DOD <- length(which(data_2005_DOD$survive_10yr==1 & data_2005_DOD$top_smallbiz_bin==1))

denominator_10yrSM_2005_DOD <- length(which(data_2005_DOD$top_smallbiz_bin==1))

survival_10yrSM_2005_DOD <- numerator_10yrSM_2005_DOD/denominator_10yrSM_2005_DOD
survival_10yrSM_2005_DOD

#*******#
#NON SMAL#
#*******#
##3-year##
table(data_2005_DOD$survive_3yr) #0=12449, 1=21743

numerator_3yrNSM_2005_DOD <- length(which(data_2005_DOD$survive_3yr==1 & data_2005_DOD$top_smallbiz_bin==0))

denominator_3yrNSM_2005_DOD <- length(which(data_2005_DOD$top_smallbiz_bin==0))

survival_3yrNSM_2005_DOD <- numerator_3yrNSM_2005_DOD/denominator_3yrNSM_2005_DOD
survival_3yrNSM_2005_DOD

##5-year##
table(data_2005_DOD$survive_5yr) #0=10168, 1=17266

numerator_5yrNSM_2005_DOD <- length(which(data_2005_DOD$survive_5yr==1 & data_2005_DOD$top_smallbiz_bin==0))

denominator_5yrNSM_2005_DOD <- length(which(data_2005_DOD$top_smallbiz_bin==0))

survival_5yrNSM_2005_DOD <- numerator_5yrNSM_2005_DOD/denominator_5yrNSM_2005_DOD
survival_5yrNSM_2005_DOD

##10-year##
table(data_2005_DOD$survive_10yr) #0=10168, 1=17266

numerator_10yrNSM_2005_DOD <- length(which(data_2005_DOD$survive_10yr==1 & data_2005_DOD$top_smallbiz_bin==0))

denominator_10yrNSM_2005_DOD <- length(which(data_2005_DOD$top_smallbiz_bin==0))

survival_10yrNSM_2005_DOD <- numerator_10yrNSM_2005_DOD/denominator_10yrNSM_2005_DOD
survival_10yrNSM_2005_DOD


#*******************#
#******2016 check***#
#*******************#
##*********
#***2016 survival 
#**********
survive_10yr_count_2005_DOD <- length(which(data_2005_DOD$survive_10yr==1))
##ALL##
numerator_2016_ALL_2005_DOD <- length(which(data_2005_DOD$survive_2016==1))

denominator_2016_ALL_2005_DOD <- length(data_2005_DOD$survive_2016)

survivalrate_2016_ALL_2005_DOD <- numerator_2016_ALL_2005_DOD /denominator_2016_ALL_2005_DOD
survivalrate_2016_ALL_2005_DOD 


##SMALL##
numerator_2016_SM_2005_DOD <- length(which(data_2005_DOD$survive_2016==1 & data_2005_DOD$top_smallbiz_bin==1))

denominator_2016_SM_2005_DOD <- length(which(data_2005_DOD$top_smallbiz_bin==1))

survivalrate_2016_SM_2005_DOD <- numerator_2016_SM_2005_DOD /denominator_2016_SM_2005_DOD 
survivalrate_2016_SM_2005_DOD


##NONSMALL##
numerator_2016_NS_2005_DOD <- length(which(data_2005_DOD$survive_2016==1 & data_2005_DOD$top_smallbiz_bin==0))

denominator_2016_NS_2005_DOD <- length(which(data_2005_DOD$top_smallbiz_bin==0))

survivalrate_2016_NS_2005_DOD  <- numerator_2016_NS_2005_DOD /denominator_2016_NS_2005_DOD
survivalrate_2016_NS_2005_DOD 


table(data_2005_DOD$top_smallbiz_bin, data_2005_DOD$survive_2016)


t.test(survive_2016 ~ top_smallbiz_bin, data = data_2005_DOD)


##**************##
####Pie Charts for 2005 DoD####
#****************#

##******##
##ALL NE##
##******##
pie_2005_ALL_DoD_df <- data.frame(
  group = c("3-yr", "5-yr", "10-yr", "2016"),
  value = c(survival_3yrALL_2005_DOD, survival_5yrALL_2005_DOD, survival_10yrALL_2005_DOD, survivalrate_2016_ALL_2005_DOD)
)

head(pie_2005_ALL_DoD_df)

pie_2005_ALL_DoD_df <- pie_2005_ALL_DoD_df %>%
  dplyr::mutate(perc_value = value * 100) %>%
  dplyr::mutate(perc_value = round(perc_value, 0))

head(pie_2005_ALL_DoD_df)

plot_2005_DoD_ALL <- ggplot(pie_2005_ALL_DoD_df, aes(x=group, y=perc_value, fill=group)) +
  scale_fill_manual(values=c("#99FFFF", "#0099CC", "#006699", "#999999" )) +
  geom_bar(stat = "identity", width=0.4) +
  scale_x_discrete(limits=c("3-yr", "5-yr", "10-yr", "2016")) +
  ylim(c(0, 70)) +
  geom_text(aes(label=percent(value)), size=3, vjust=-.25) +
  labs(title = "- All New Entrants DoD", y = "Survival Rates (percentages)", x = "Year") +
  guides(fill=FALSE)



##******##
##SMALL NE##
##******##
pie_2005_SM_DoD_df <- data.frame(
  group = c("3-yr", "5-yr", "10-yr", "2016", "Graduated"),
  value = c(survival_3yrALL_2005_DOD, survival_5yrALL_2005_DOD, survival_10yrALL_2005_DOD, survivalrate_2016_ALL_2005_DOD, graduated_2005_DOD_10yr)
)

head(pie_2005_SM_DoD_df)

pie_2005_SM_DoD_df <- pie_2005_SM_DoD_df %>%
  dplyr::mutate(perc_value = value * 100) %>%
  dplyr::mutate(perc_value = round(perc_value, 0))

head(pie_2005_SM_DoD_df)

plot_2005_DoD_SMALL <- ggplot(pie_2005_SM_DoD_df, aes(x=group, y=perc_value, fill=group)) +
  scale_fill_manual(values=c("#99FFFF", "#0099CC", "#006699", "#999999", "#000066")) +
  geom_bar(stat = "identity", width=0.4) +
  scale_x_discrete(limits=c("3-yr", "5-yr", "10-yr", "2016", "Graduated")) +
  ylim(c(0, 70)) +
  geom_text(aes(label=percent(value)), vjust=-.25, size=3) +
  labs(title = "Survival Rates 2005 DoD - Small Businesses", y = "Survival Rates (percentages)", x = "Year")

##******##
##Non-Small NE##
##******##
pie_2005_NS_DoD_df <- data.frame(
  group = c("3-yr", "5-yr", "10-yr", "2016"),
  value = c(survival_3yrNSM_2005_DOD, survival_5yrNSM_2005_DOD, survival_10yrNSM_2005_DOD, survivalrate_2016_NS_2005_DOD)
)

head(pie_2005_NS_DoD_df)

pie_2005_NS_DoD_df <- pie_2005_NS_DoD_df %>%
  dplyr::mutate(perc_value = value * 100) %>%
  dplyr::mutate(perc_value = round(perc_value, 0))

head(pie_2005_NS_DoD_df)

plot_2005_DoD_NS <- ggplot(pie_2005_NS_DoD_df, aes(x=group, y=perc_value, fill=group)) +
  scale_fill_manual(values=c("#99FFFF", "#0099CC", "#006699", "#999999")) +
  geom_bar(stat = "identity", width=0.4) +
  scale_x_discrete(limits=c("3-yr", "5-yr", "10-yr", "2016")) +
  ylim(c(0, 70)) +
  geom_text(aes(label=percent(value)), vjust=-.25, size=3) +
  labs(title = "- Non-Small Businesses DoD", y = "Survival Rates (percentages)", x = "Year") +
  guides(fill=FALSE)

##*****##
#combine plots#
#******##
#t <- textGrob("Survival Rates 2001 Sample", fontsize=42)
grid.arrange(plot_2005_SMALL, plot_2005_DoD_SMALL, plot_2005_ALL, plot_2005_DoD_ALL, plot_2005_NS, plot_2005_DoD_NS)


#**********************************************************************#

#***********#
####2006 all fed agencies####
#************#

#create necessary vars#

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


str(data_2006)

data_2006$survive_3yr<-as.numeric(as.character(data_2006$survive_3yr))
data_2006$survive_5yr<-as.numeric(as.character(data_2006$survive_5yr))
data_2006$survive_10yr<-as.numeric(as.character(data_2006$survive_10yr))
data_2006$survive_2016<-as.numeric(as.character(data_2006$survive_2016))

str(data_2006)

#**********
##t test to test the differences between small and non small survival##
table(data_2006$top_smallbiz_bin)
table(data_2006$survive_3yr)
table(data_2006$top_smallbiz_bin, data_2006$survive_3yr)

t.test(survive_3yr ~ top_smallbiz_bin, data = data_2006)

#5-year#
table(data_2006$top_smallbiz_bin, data_2006$survive_5yr)

t.test(survive_5yr ~ top_smallbiz_bin, data = data_2006)

#10-year#
table(data_2006$top_smallbiz_bin, data_2006$survive_10yr)

t.test(survive_10yr ~ top_smallbiz_bin, data = data_2006)


#*************#
#survival rates#
#**************#
#*********#
#**ALL****#
#*********
##3-year##
table(data_2006$survive_3yr) #0=12449, 1=21743

numerator_3yrALL_2006 <- length(which(data_2006$survive_3yr==1))

denominator_3yrALL_2006 <- length(data_2006$survive_3yr)

survival_3yrALL_2006 <- numerator_3yrALL_2006/denominator_3yrALL_2006
survival_3yrALL_2006

##5-year##
table(data_2006$survive_5yr) #0=10168, 1=17266

numerator_5yrALL_2006 <- length(which(data_2006$survive_5yr==1))

denominator_5yrALL_2006 <- length(data_2006$survive_5yr)

survival_5yrALL_2006 <- numerator_5yrALL_2006/denominator_5yrALL_2006
survival_5yrALL_2006

##10-year##
table(data_2006$survive_10yr) #0=10168, 1=17266

numerator_10yrALL_2006 <- length(which(data_2006$survive_10yr==1))

denominator_10yrALL_2006 <- length(data_2006$survive_10yr)

survival_10yrALL_2006 <- numerator_10yrALL_2006/denominator_10yrALL_2006
survival_10yrALL_2006
#****************#
#Graduation Rates#
#****************#
table(data_2006$graduated)

numerator_grad_2006 <- length(which(data_2006$graduated==1))

denominator_grad_2006 <- length(data_2006$graduated)

graduated_2006 <- numerator_grad_2006/denominator_grad_2006
graduated_2006

##for only those who survived 10 years
numerator_grad_2006_10yr <- length(which(data_2006$graduated==1 & data_2006$survive_10yr==1))

#denominator_grad_2006_10yr <- length(data_2006$graduated)

denominator_grad_2006_10yr <- length(which(data_2006$top_smallbiz_bin==1))

graduated_2006_10yr <- numerator_grad_2006_10yr/denominator_grad_2006_10yr
graduated_2006_10yr



#*****#
#***SMALL***
#*******#
##3-year##
table(data_2006$survive_3yr) #0=12449, 1=21743

numerator_3yrSM_2006 <- length(which(data_2006$survive_3yr==1 & data_2006$top_smallbiz_bin==1))

denominator_3yrSM_2006 <- length(which(data_2006$top_smallbiz_bin==1))

survival_3yrSM_2006 <- numerator_3yrSM_2006/denominator_3yrSM_2006
survival_3yrSM_2006 

##5-year##
table(data_2006$survive_5yr) #0=10168, 1=17266

numerator_5yrSM_2006 <- length(which(data_2006$survive_5yr==1 & data_2006$top_smallbiz_bin==1))

denominator_5yrSM_2006 <- length(which(data_2006$top_smallbiz_bin==1))

survival_5yrSM_2006 <- numerator_5yrSM_2006/denominator_5yrSM_2006
survival_5yrSM_2006 

##10-year##
table(data_2006$survive_10yr) #0=10168, 1=17266

numerator_10yrSM_2006 <- length(which(data_2006$survive_10yr==1 & data_2006$top_smallbiz_bin==1))

denominator_10yrSM_2006 <- length(which(data_2006$top_smallbiz_bin==1))

survival_10yrSM_2006 <- numerator_10yrSM_2006/denominator_10yrSM_2006
survival_10yrSM_2006


#********#
#*NON SMALL*#
#*********#
##3-year##
table(data_2006$survive_3yr) #0=12449, 1=21743

numerator_3yrNSM_2006 <- length(which(data_2006$survive_3yr==1 & data_2006$top_smallbiz_bin==0))

denominator_3yrNSM_2006 <- length(which(data_2006$top_smallbiz_bin==0))

survival_3yrNSM_2006 <- numerator_3yrNSM_2006/denominator_3yrNSM_2006
survival_3yrNSM_2006

##5-year##
table(data_2006$survive_5yr) #0=10168, 1=17266

numerator_5yrNSM_2006 <- length(which(data_2006$survive_5yr==1 & data_2006$top_smallbiz_bin==0))

denominator_5yrNSM_2006 <- length(which(data_2006$top_smallbiz_bin==0))

survival_5yrNSM_2006 <- numerator_5yrNSM_2006/denominator_5yrNSM_2006
survival_5yrNSM_2006 

##10-year##
table(data_2006$survive_10yr) #0=10168, 1=17266

numerator_10yrNSM_2006 <- length(which(data_2006$survive_10yr==1 & data_2006$top_smallbiz_bin==0))

denominator_10yrNSM_2006 <- length(which(data_2006$top_smallbiz_bin==0))

survival_10yrNSM_2006 <- numerator_10yrNSM_2006/denominator_10yrNSM_2006
survival_10yrNSM_2006 

#*******************#
#******2016 check***#
#*******************#
##*********
#***2016 survival 
#**********
survive_10yr_count_2006 <- length(which(data_2006$survive_10yr==1))
##ALL##
numerator_2016_ALL_2006 <- length(which(data_2006$survive_2016==1))

denominator_2016_ALL_2006 <- length(data_2006$survive_2016)

survivalrate_2016_ALL_2006 <- numerator_2016_ALL_2006 /denominator_2016_ALL_2006
survivalrate_2016_ALL_2006 


##SMALL##
numerator_2016_SM_2006 <- length(which(data_2006$survive_2016==1 & data_2006$top_smallbiz_bin==1))

denominator_2016_SM_2006 <- length(which(data_2006$top_smallbiz_bin==1))

survivalrate_2016_SM_2006 <- numerator_2016_SM_2006 /denominator_2016_SM_2006
survivalrate_2016_SM_2006


##NONSMALL##
numerator_2016_NS_2006<- length(which(data_2006$survive_2016==1 & data_2006$top_smallbiz_bin==0))

denominator_2016_NS_2006 <- length(which(data_2006$top_smallbiz_bin==0))

survivalrate_2016_NS_2006  <- numerator_2016_NS_2006 /denominator_2016_NS_2006
survivalrate_2016_NS_2006


table(data_2006$top_smallbiz_bin, data_2006$survive_2016)


t.test(survive_2016 ~ top_smallbiz_bin, data = data_2006)

##**************##
####Pie Charts for 2006####
#****************#

##******##
##ALL NE##
##******##
pie_2006_ALL_df <- data.frame(
  group = c("3-yr", "5-yr", "10-yr", "2016"),
  value = c(survival_3yrALL_2006, survival_5yrALL_2006, survival_10yrALL_2006, survivalrate_2016_ALL_2006)
)

head(pie_2006_ALL_df)

pie_2006_ALL_df <- pie_2006_ALL_df %>%
  dplyr::mutate(perc_value = value * 100) %>%
  dplyr::mutate(perc_value = round(perc_value, 0))

head(pie_2006_ALL_df)

plot_2006_ALL <- ggplot(pie_2006_ALL_df, aes(x=group, y=perc_value, fill=group)) +
  scale_fill_manual(values=c("#99FFFF", "#0099CC", "#006699", "#999999" )) +
  geom_bar(stat = "identity", width=0.4) +
  scale_x_discrete(limits=c("3-yr", "5-yr", "10-yr", "2016")) +
  ylim(c(0, 70)) +
  geom_text(aes(label=percent(value)), size=3, vjust=-.25) +
  labs(title = "- All New Entrants All Fed", y = "Survival Rates (percentages)", x = "Year") + 
  guides(fill=FALSE)


##******##
##SMALL NE##
##******##
pie_2006_SM_df <- data.frame(
  group = c("3-yr", "5-yr", "10-yr", "2016", "Graduated"),
  value = c(survival_3yrSM_2006, survival_5yrSM_2006, survival_10yrSM_2006, survivalrate_2016_SM_2006, graduated_2006_10yr)
)

head(pie_2006_SM_df)

pie_2006_SM_df <- pie_2006_SM_df %>%
  dplyr::mutate(perc_value = value * 100) %>%
  dplyr::mutate(perc_value = round(perc_value, 0))

head(pie_2006_SM_df)

plot_2006_SMALL <- ggplot(pie_2006_SM_df, aes(x=group, y=perc_value, fill=group)) +
  scale_fill_manual(values=c("#99FFFF", "#0099CC", "#006699", "#999999", "#000066" )) +
  geom_bar(stat = "identity", width=0.4) +
  scale_x_discrete(limits=c("3-yr", "5-yr", "10-yr", "2016", "Graduated")) +
  ylim(c(0, 70)) +
  geom_text(aes(label=percent(value)), vjust=-.25, size=3) +
  labs(title = "Survival Rates 2006 All Federal Agencies - Small Businesses", y = "Survival Rates (percentages)", x = "Year") + 
  guides(fill=FALSE)

##******##
##Non-Small NE##
##******##
pie_2006_NS_df <- data.frame(
  group = c("3-yr", "5-yr", "10-yr", "2016"),
  value = c(survival_3yrNSM_2006, survival_5yrNSM_2006, survival_10yrNSM_2006, survivalrate_2016_NS_2006)
)

head(pie_2006_NS_df)

pie_2006_NS_df <- pie_2006_NS_df %>%
  dplyr::mutate(perc_value = value * 100) %>%
  dplyr::mutate(perc_value = round(perc_value, 0))

head(pie_2006_NS_df)

plot_2006_NS <- ggplot(pie_2006_NS_df, aes(x=group, y=perc_value, fill=group)) +
  scale_fill_manual(values=c("#99FFFF", "#0099CC", "#006699", "#999999")) +
  geom_bar(stat = "identity", width=0.4) +
  scale_x_discrete(limits=c("3-yr", "5-yr", "10-yr", "2016")) +
  ylim(c(0, 70)) +
  geom_text(aes(label=percent(value)), vjust=-.25, size=3) +
  labs(title = "- Non-Small Businesses All Fed", y = "Survival Rates (percentages)", x = "Year") +
  guides(fill=FALSE)



##*****##
#combine plots#
#******##
#t <- textGrob("Survival Rates 2001 Sample", fontsize=42)
grid.arrange(plot_2006_SMALL, plot_2006_ALL, plot_2006_NS)


#***************#
####2006 DOD ONLY####
#***************#
#create necessary vars#

data_2006_DOD <- FPDS_cleaned_unique[!(FPDS_cleaned_unique$registrationYear!="2006"), ]
data_2006_DOD <- data_2006_DOD[!(data_2006_DOD$customer!="Defense"), ]


##create variable describing whether a firm survived 3 years

data_2006_DOD <- data_2006_DOD %>%
  dplyr::mutate(survive_3yr = ifelse(exitYear < 2008, "0", "1")) 

##create variable describing whether a firm survived 5 years
data_2006_DOD <- data_2006_DOD %>%
  dplyr::mutate(survive_5yr = ifelse(exitYear < 2011, "0", "1")) 


##create variable describing whether a firm survived 10 years
data_2006_DOD <- data_2006_DOD %>%
  dplyr::mutate(survive_10yr = ifelse(exitYear < 2015, "0", "1")) 

##create variable describing whether a firm survived in 2016
data_2006_DOD <- data_2006_DOD %>%
  dplyr::mutate(survive_2016 = ifelse(exitYear < 2016, "0", "1"))


str(data_2006_DOD)

data_2006_DOD$survive_3yr<-as.numeric(as.character(data_2006_DOD$survive_3yr))
data_2006_DOD$survive_5yr<-as.numeric(as.character(data_2006_DOD$survive_5yr))
data_2006_DOD$survive_10yr<-as.numeric(as.character(data_2006_DOD$survive_10yr))
data_2006_DOD$survive_2016<-as.numeric(as.character(data_2006_DOD$survive_2016))

str(data_2006_DOD)

#**********
##t test to test the differences between small and non small survival##
table(data_2006_DOD$top_smallbiz_bin)
table(data_2006_DOD$survive_3yr)
table(data_2006_DOD$top_smallbiz_bin, data_2006_DOD$survive_3yr)

t.test(survive_3yr ~ top_smallbiz_bin, data = data_2006_DOD)

#5-year#
table(data_2006_DOD$top_smallbiz_bin, data_2006_DOD$survive_5yr)

t.test(survive_5yr ~ top_smallbiz_bin, data = data_2006_DOD)

#10-year#
table(data_2006_DOD$top_smallbiz_bin, data_2006_DOD$survive_10yr)

t.test(survive_10yr ~ top_smallbiz_bin, data = data_2006_DOD)


#*************#
#survival rates#
#**************#
#*********#
#**ALL****#
#*********
##3-year##
table(data_2006_DOD$survive_3yr) #0=12449, 1=21743

numerator_3yrALL_2006_DOD <- length(which(data_2006_DOD$survive_3yr==1))

denominator_3yrALL_2006_DOD <- length(data_2006_DOD$survive_3yr)

survival_3yrALL_2006_DOD <- numerator_3yrALL_2006_DOD/denominator_3yrALL_2006_DOD
survival_3yrALL_2006_DOD

##5-year##
table(data_2006_DOD$survive_5yr) #0=10168, 1=17266

numerator_5yrALL_2006_DOD <- length(which(data_2006_DOD$survive_5yr==1))

denominator_5yrALL_2006_DOD <- length(data_2006_DOD$survive_5yr)

survival_5yrALL_2006_DOD <- numerator_5yrALL_2006_DOD/denominator_5yrALL_2006_DOD
survival_5yrALL_2006_DOD

##10-year##
table(data_2006_DOD$survive_10yr) #0=10168, 1=17266

numerator_10yrALL_2006_DOD <- length(which(data_2006_DOD$survive_10yr==1))

denominator_10yrALL_2006_DOD <- length(data_2006_DOD$survive_10yr)

survival_10yrALL_2006_DOD <- numerator_10yrALL_2006_DOD/denominator_10yrALL_2006_DOD
survival_10yrALL_2006_DOD
#****************#
#Graduation Rates#
#****************#
table(data_2006_DOD$graduated)

numerator_grad_2006_DOD <- length(which(data_2006_DOD$graduated==1))

denominator_grad_2006_DOD <- length(data_2006_DOD$graduated)

graduated_2006_DOD <- numerator_grad_2006_DOD/denominator_grad_2006_DOD
graduated_2006_DOD

##graduation for those who survived after 10 years only
numerator_grad_2006_DOD_10yr <- length(which(data_2006_DOD$graduated==1 & data_2006_DOD$survive_10yr==1))

#denominator_grad_2006_DOD_10yr <- length(data_2006_DOD$graduated)

denominator_grad_2006_DOD_10yr <- length(which(data_2006_DOD$top_smallbiz_bin==1))

graduated_2006_DOD_10yr <- numerator_grad_2006_DOD_10yr/denominator_grad_2006_DOD_10yr
graduated_2006_DOD_10yr


#*****#
#***SMALL***
#*******#
##3-year##
table(data_2006_DOD$survive_3yr) #0=12449, 1=21743

numerator_3yrSM_2006_DOD <- length(which(data_2006_DOD$survive_3yr==1 & data_2006_DOD$top_smallbiz_bin==1))

denominator_3yrSM_2006_DOD <- length(which(data_2006_DOD$top_smallbiz_bin==1))

survival_3yrSM_2006_DOD <- numerator_3yrSM_2006_DOD/denominator_3yrSM_2006_DOD
survival_3yrSM_2006_DOD 

##5-year##
table(data_2006_DOD$survive_5yr) #0=10168, 1=17266

numerator_5yrSM_2006_DOD <- length(which(data_2006_DOD$survive_5yr==1 & data_2006_DOD$top_smallbiz_bin==1))

denominator_5yrSM_2006_DOD <- length(which(data_2006_DOD$top_smallbiz_bin==1))

survival_5yrSM_2006_DOD <- numerator_5yrSM_2006_DOD/denominator_5yrSM_2006_DOD
survival_5yrSM_2006_DOD 

##10-year##
table(data_2006_DOD$survive_10yr) #0=10168, 1=17266

numerator_10yrSM_2006_DOD <- length(which(data_2006_DOD$survive_10yr==1 & data_2006_DOD$top_smallbiz_bin==1))

denominator_10yrSM_2006_DOD <- length(which(data_2006_DOD$top_smallbiz_bin==1))

survival_10yrSM_2006_DOD <- numerator_10yrSM_2006_DOD/denominator_10yrSM_2006_DOD
survival_10yrSM_2006_DOD


#********#
#*NON SMALL*#
#*********#
##3-year##
table(data_2006_DOD$survive_3yr) #0=12449, 1=21743

numerator_3yrNSM_2006_DOD <- length(which(data_2006_DOD$survive_3yr==1 & data_2006_DOD$top_smallbiz_bin==0))

denominator_3yrNSM_2006_DOD <- length(which(data_2006_DOD$top_smallbiz_bin==0))

survival_3yrNSM_2006_DOD <- numerator_3yrNSM_2006_DOD/denominator_3yrNSM_2006_DOD
survival_3yrNSM_2006_DOD

##5-year##
table(data_2006_DOD$survive_5yr) #0=10168, 1=17266

numerator_5yrNSM_2006_DOD <- length(which(data_2006_DOD$survive_5yr==1 & data_2006_DOD$top_smallbiz_bin==0))

denominator_5yrNSM_2006_DOD <- length(which(data_2006_DOD$top_smallbiz_bin==0))

survival_5yrNSM_2006_DOD <- numerator_5yrNSM_2006_DOD/denominator_5yrNSM_2006_DOD
survival_5yrNSM_2006_DOD 

##10-year##
table(data_2006_DOD$survive_10yr) #0=10168, 1=17266

numerator_10yrNSM_2006_DOD <- length(which(data_2006_DOD$survive_10yr==1 & data_2006_DOD$top_smallbiz_bin==0))

denominator_10yrNSM_2006_DOD <- length(which(data_2006_DOD$top_smallbiz_bin==0))

survival_10yrNSM_2006_DOD <- numerator_10yrNSM_2006_DOD/denominator_10yrNSM_2006_DOD
survival_10yrNSM_2006_DOD 

#*******************#
#******2016 check***#
#*******************#
##*********
#***2016 survival 
#**********
survive_10yr_count_2006_DOD <- length(which(data_2006_DOD$survive_10yr==1))
##ALL##
numerator_2016_ALL_2006_DOD <- length(which(data_2006_DOD$survive_2016==1))

denominator_2016_ALL_2006_DOD <- length(data_2006_DOD$survive_2016)

survivalrate_2016_ALL_2006_DOD <- numerator_2016_ALL_2006_DOD /denominator_2016_ALL_2006_DOD
survivalrate_2016_ALL_2006_DOD 


##SMALL##
numerator_2016_SM_2006_DOD <- length(which(data_2006_DOD$survive_2016==1 & data_2006_DOD$top_smallbiz_bin==1))

denominator_2016_SM_2006_DOD <- length(which(data_2006_DOD$top_smallbiz_bin==1))

survivalrate_2016_SM_2006_DOD <- numerator_2016_SM_2006_DOD /denominator_2016_SM_2006_DOD
survivalrate_2016_SM_2006_DOD


##NONSMALL##
numerator_2016_NS_2006_DOD <- length(which(data_2006_DOD$survive_2016==1 & data_2006_DOD$top_smallbiz_bin==0))

denominator_2016_NS_2006_DOD <- length(which(data_2006_DOD$top_smallbiz_bin==0))

survivalrate_2016_NS_2006_DOD  <- numerator_2016_NS_2006_DOD /denominator_2016_NS_2006_DOD
survivalrate_2016_NS_2006_DOD


table(data_2006_DOD$top_smallbiz_bin, data_2006_DOD$survive_2016)


t.test(survive_2016 ~ top_smallbiz_bin, data = data_2006_DOD)

##**************##
####Pie Charts for 2006 DOD####
#****************#

##******##
##ALL NE##
##******##
pie_2006_ALL_DoD_df <- data.frame(
  group = c("3-yr", "5-yr", "10-yr", "2016"),
  value = c(survival_3yrALL_2006_DOD, survival_5yrALL_2006_DOD, survival_10yrALL_2006_DOD, survivalrate_2016_ALL_2006_DOD)
)

head(pie_2006_ALL_DoD_df)

pie_2006_ALL_DoD_df <- pie_2006_ALL_DoD_df %>%
  dplyr::mutate(perc_value = value * 100) %>%
  dplyr::mutate(perc_value = round(perc_value, 0))

head(pie_2006_ALL_DoD_df)

plot_2006_DoD_ALL <- ggplot(pie_2006_ALL_DoD_df, aes(x=group, y=perc_value, fill=group)) +
  scale_fill_manual(values=c("#99FFFF", "#0099CC", "#006699", "#999999" )) +
  geom_bar(stat = "identity", width=0.4) +
  scale_x_discrete(limits=c("3-yr", "5-yr", "10-yr", "2016")) +
  ylim(c(0, 70)) +
  geom_text(aes(label=percent(value)), size=3, vjust=-.25) +
  labs(title = "- All New Entrants DoD", y = "Survival Rates (percentages)", x = "Year") + 
  guides(fill=FALSE)


##******##
##SMALL NE##
##******##
pie_2006_SM_DoD_df <- data.frame(
  group = c("3-yr", "5-yr", "10-yr", "2016", "Graduated"),
  value = c(survival_3yrALL_2006_DOD, survival_5yrALL_2006_DOD, survival_10yrALL_2006_DOD, survivalrate_2016_ALL_2006_DOD, graduated_2006_DOD_10yr)
)

head(pie_2006_SM_DoD_df)

pie_2006_SM_DoD_df <- pie_2006_SM_DoD_df %>%
  dplyr::mutate(perc_value = value * 100) %>%
  dplyr::mutate(perc_value = round(perc_value, 0))

head(pie_2006_SM_DoD_df)

plot_2006_DoD_SMALL <- ggplot(pie_2006_SM_DoD_df, aes(x=group, y=perc_value, fill=group)) +
  scale_fill_manual(values=c("#99FFFF", "#0099CC", "#006699", "#999999", "#000066")) +
  geom_bar(stat = "identity", width=0.4) +
  scale_x_discrete(limits=c("3-yr", "5-yr", "10-yr", "2016", "Graduated")) +
  ylim(c(0, 70)) +
  geom_text(aes(label=percent(value)), vjust=-.25, size=3) +
  labs(title = "Survival Rates 2006 DoD - Small Businesses", y = "Survival Rates (percentages)", x = "Year")

##******##
##Non-Small NE##
##******##
pie_2006_NS_DoD_df <- data.frame(
  group = c("3-yr", "5-yr", "10-yr", "2016"),
  value = c(survival_3yrNSM_2006_DOD, survival_5yrNSM_2006_DOD, survival_10yrNSM_2006_DOD, survivalrate_2016_NS_2006_DOD)
)

head(pie_2006_NS_DoD_df)

pie_2006_NS_DoD_df <- pie_2006_NS_DoD_df %>%
  dplyr::mutate(perc_value = value * 100) %>%
  dplyr::mutate(perc_value = round(perc_value, 0))

head(pie_2006_NS_DoD_df)

plot_2006_DoD_NS <- ggplot(pie_2006_NS_DoD_df, aes(x=group, y=perc_value, fill=group)) +
  scale_fill_manual(values=c("#99FFFF", "#0099CC", "#006699", "#999999")) +
  geom_bar(stat = "identity", width=0.4) +
  scale_x_discrete(limits=c("3-yr", "5-yr", "10-yr", "2016")) +
  ylim(c(0, 70)) +
  geom_text(aes(label=percent(value)), vjust=-.25, size=3) +
  labs(title = "- Non-Small Businesses DoD", y = "Survival Rates (percentages)", x = "Year") +
  guides(fill=FALSE)


##*****##
#combine plots#
#******##
#t <- textGrob("Survival Rates 2001 Sample", fontsize=42)
grid.arrange(plot_2006_SMALL, plot_2006_DoD_SMALL, plot_2006_ALL, plot_2006_DoD_ALL, plot_2006_NS, plot_2006_DoD_NS)


#************************************************************************************


####Display all graduation rates####
#2001
graduatedALL_2001_10yr
graduatedALL_2001_DoD_10yr

#2002
graduated_2002_10yr
graduated_2002_DOD_10yr

#2003
graduated_2003_10yr
graduated_2003_DOD_10yr

#2004
graduated_2004_10yr
graduated_2004_DOD_10yr

#2005
graduated_2005_10yr
graduated_2005_DOD_10yr


#2006
graduated_2006_10yr
graduated_2006_DOD_10yr

#********************************************************************

#******************#
####2016 checks####
#*******************#
data_2006_survive2016
data_2006_survive2016
data_2006_survive2016
data_2006_survive2016
data_2006_survive2016
data_2006_survive2016

survive_2016_v1 <- rbind(data_2001_survive2016, data_2002_survive2016)
survive_2016_v2 <- rbind(survive_2016_v1, data_2003_survive2016)
survive_2016_v3 <- rbind(survive_2016_v2, data_2004_survive2016)
survive_2016_v4 <- rbind(survive_2016_v3, data_2005_survive2016)
survive_2016_data <- rbind(survive_2016_v4, data_2006_survive2016)


#**************************************************************#
####calculating graduation rates for all samples using a loop####
#**************************************************************#
#function#
calc_gradrates <- function(x) {
  denominator <- length(which(x$top_smallbiz_bin==1))
  numerator_1 <- length(which(x$graduated==1))
  numerator_2 <- length(which(x$graduated==1 & x$survive_10yr==0)) 
  numerator_3 <- length(which(x$graduated==1 & x$survive_10yr==1)) 
  numerator_4 <- length(which(x$graduated==1 & x$survive_2016==0)) 
  numerator_5 <- length(which(x$graduated==1 & x$survive_2016==1))
  
  gradrate_1 <- numerator_1/denominator ##all small NE
  gradrate_2 <- numerator_2/denominator ##NE that did not survive 10 years
  gradrate_3 <- numerator_3/denominator ##NE that did survive 10 years
  gradrate_4 <- numerator_4/denominator ##NE that did not survive in 2016
  graduate_5 <- numerator_5/denominator ##NE that did survive in 2016
  
  allrates <- c(gradrate_1, gradrate_2, gradrate_3, gradrate_4, graduate_5)
  allrates
  
}

#loop#

datalist <- list(data_2001=data_2001, data_2001_DoD=data_DOD_2001, 
                 data_2002=data_2002, data_2002_DoD=data_2002_DOD, 
                 data_2003=data_2003, data_2003_DoD=data_2003_DOD, 
                 data_2004=data_2004, data_2004_DoD=data_2004_DOD, 
                 data_2005=data_2005, data_2005_DoD=data_2005_DOD, 
                 data_2006=data_2006, data_2006_DoD=data_2006_DOD)
gradrates <- lapply(datalist, calc_gradrates)

gradrates
#************************************************************************************#



#********************************************************************
####% of obligations that go to different subgroups of new entrants in each year####
#********************************************************************

##drop observations with Registration Year before 2000
FPDS_cleaned_unique_graphs <- FPDS_cleaned_unique[!(FPDS_cleaned_unique$registrationYear<2001), ]
FPDS_cleaned_unique_graphs <- FPDS_cleaned_unique_graphs[!(FPDS_cleaned_unique_graphs$registrationYear>2016), ]

 
#**********
####1. %of obligations that go to new entrants as opposed to incumbent firms in each year####
#**********

load(file = "FPDS_cleaned_unique_wtotalobligations.Rda")

FPDS_cleaned_unique_wtotalobl_graphs <- FPDS_cleaned_unique_wtotalobligations_allvend_NE[!(FPDS_cleaned_unique_wtotalobligations_allvend_NE$registrationYear<2001), ]
FPDS_cleaned_unique_wtotalobl_graphs <- FPDS_cleaned_unique_wtotalobl_graphs[!(FPDS_cleaned_unique_wtotalobl_graphs$registrationYear>2016), ]


##ALL Fed Agencies##

##subset data to get just sum_obligations_newentrants and sum_obligatoins_incumbents

names(FPDS_cleaned_unique_wtotalobl_graphs)


newentrants_graphs <- FPDS_cleaned_unique_wtotalobl_graphs %>% group_by(FYear) %>%
  dplyr::summarise(obligations_newentrants = min(sum_obligations_newentrants)) 

# newentrants_graphs <- newentrants_graphs %>% group_by(FYear) %>%
#   dplyr::mutate(new_entrant = 1)
  

incumbents_graphs <- FPDS_cleaned_unique_wtotalobl_graphs %>% group_by(FYear) %>%
  dplyr::summarise(obligations_incumbents = min(sum_obligations_incumbents)) 


newentrants_v_incumbent_graphs <- join(newentrants_graphs, incumbents_graphs, by = "FYear", type = "left", match = "all")

# incumbents_graphs <- incumbents_graphs %>% group_by(FYear) %>%
#   dplyr::mutate(new_entrant = 0)

##append two dataframes
# 
# newentrants_v_incumbent_graphs <- rbind(newentrants_graphs, incumbents_graphs)

##add variable that is all vendor obligations
names(FPDS_cleaned_unique_wtotalobl_graphs)
allvendors_graphs <- FPDS_cleaned_unique_wtotalobl_graphs %>% group_by(FYear) %>%
  dplyr::summarise(obligations_allvendors = min(total_obligations_allvendors))

newentrants_v_incumbent_graphs <- join(newentrants_v_incumbent_graphs, allvendors_graphs, by = "FYear", type = "left", match = "all")

##prepare data for graphs 

newentrants_v_incumbent_graphs <- newentrants_v_incumbent_graphs %>%
  group_by(FYear) %>%
  dplyr::mutate(perc_obligations_NE_decimal = obligations_newentrants / obligations_allvendors) %>%
  dplyr::mutate(perc_obligations_NE = perc_obligations_NE_decimal * 100) %>%
  dplyr::mutate(perc_obligations_NE = round(perc_obligations_NE, 0)) %>%
  dplyr::mutate(perc_obligations_inc_decimal = obligations_incumbents / obligations_allvendors) %>%
  dplyr::mutate(perc_obligations_inc = perc_obligations_inc_decimal * 100) %>%
  dplyr::mutate(perc_obligations_inc = round(perc_obligations_inc, 0)) %>%
  dplyr::mutate(total_percent = 100)

# ##subset new_v_incumbent_graphs to newentrants_graph_intermediate with FY, sumoblNE, percoblNE, totaloblallvend, and CREATE vendortype
names(newentrants_v_incumbent_graphs)
newentrants_graph_intermediate <- data.frame(newentrants_v_incumbent_graphs$FYear,
                                             newentrants_v_incumbent_graphs$obligations_newentrants,
                                             newentrants_v_incumbent_graphs$perc_obligations_NE,
                                             newentrants_v_incumbent_graphs$obligations_allvendors,
                                             newentrants_v_incumbent_graphs$perc_obligations_NE_decimal)
newentrants_graph_intermediate <- newentrants_graph_intermediate %>%
  dplyr::mutate(vendor_type=1)
#
names(newentrants_graph_intermediate)
names(newentrants_graph_intermediate)[names(newentrants_graph_intermediate) == "newentrants_v_incumbent_graphs.FYear"] <- "FYear"
names(newentrants_graph_intermediate)[names(newentrants_graph_intermediate) == "newentrants_v_incumbent_graphs.obligations_newentrants"] <- "sum_obligations"
names(newentrants_graph_intermediate)[names(newentrants_graph_intermediate) == "newentrants_v_incumbent_graphs.perc_obligations_NE"] <- "perc_obligations"
names(newentrants_graph_intermediate)[names(newentrants_graph_intermediate) == "newentrants_v_incumbent_graphs.obligations_allvendors"] <- "total_obligations_allvendors"
names(newentrants_graph_intermediate)[names(newentrants_graph_intermediate) == "newentrants_v_incumbent_graphs.perc_obligations_NE_decimal"] <- "perc_obligations_dec"
#
# ##subset new_v_incumbent_graphs to incumbent_graph_intermediate with FY, totaloblincumbent, percobligationsinc, totaloblallvend, and CREATE vendortype
names(newentrants_v_incumbent_graphs)
incumbent_graph_intermediate <- data.frame(newentrants_v_incumbent_graphs$FYear,
                                           newentrants_v_incumbent_graphs$obligations_incumbents,
                                           newentrants_v_incumbent_graphs$perc_obligations_inc,
                                           newentrants_v_incumbent_graphs$obligations_allvendors,
                                           newentrants_v_incumbent_graphs$perc_obligations_inc_decimal)

incumbent_graph_intermediate <- incumbent_graph_intermediate %>%
  dplyr::mutate(vendor_type=0)
#
names(incumbent_graph_intermediate)
names(incumbent_graph_intermediate)[names(incumbent_graph_intermediate) == "newentrants_v_incumbent_graphs.FYear"] <- "FYear"
names(incumbent_graph_intermediate)[names(incumbent_graph_intermediate) == "newentrants_v_incumbent_graphs.obligations_incumbents"] <- "sum_obligations"
names(incumbent_graph_intermediate)[names(incumbent_graph_intermediate) == "newentrants_v_incumbent_graphs.perc_obligations_inc"] <- "perc_obligations"
names(incumbent_graph_intermediate)[names(incumbent_graph_intermediate) == "newentrants_v_incumbent_graphs.obligations_allvendors"] <- "total_obligations_allvendors"
names(incumbent_graph_intermediate)[names(incumbent_graph_intermediate) == "newentrants_v_incumbent_graphs.perc_obligations_inc_decimal"] <- "perc_obligations_dec"
names(incumbent_graph_intermediate)

# ##append incumbent_graph_intermediate to newentrants_graph_intermediate
#
newentrants_v_incumbent_graphs <- rbind(newentrants_graph_intermediate, incumbent_graph_intermediate)

newentrants_v_incumbent_graphs <- newentrants_v_incumbent_graphs[!(newentrants_v_incumbent_graphs$FYear>2016), ]

##graph
ggplot(newentrants_v_incumbent_graphs, aes(x = FYear, y = total_obligations_allvendors, fill = factor(vendor_type), label = perc_obligations)) +
  geom_bar(stat = 'identity', position = 'stack') +
  ylab("Total Obligations") +
  xlab("Fiscal Year") +
  scale_x_continuous(breaks = c(2001:2016)) +
  ##scale_fill_manual(name = "New Entrants Types", values = c("deepskyblue", "royalblue1"), labels = c("small", "non-small")) +
  scale_fill_manual(name = "Vendor Type", values = c("darkslategray1", "cadetblue4"), labels = c("Incumbent Firms", "New Entrant")) +
  ggtitle("Percent of Obligations for New Entrnats vs. Incumbents (2001-2016) - All Federal Agencies")+
  ##geom_text_repel(data = subset(FPDS_bargraphCount, registrationYear >=2014), aes(label = regpersize), size = 4, box.padding = .1, 
  ###    angle = 45) +
  ##geom_text(data = subset(FPDS_bargraphCount, registrationYear < 2014), aes(label = regpersize), size = 4, position = position_stack(vjust = .5), angle = 45)
  geom_text_repel(data = subset(newentrants_v_incumbent_graphs, FYear <= 2016), aes(label = scales::percent(perc_obligations_dec)), size = 4, position = position_stack(vjust = .3), angle = 90)


#**********************************************************************************************



#*********
####2. %of obligations that go to small and non-small new entrants in each year####
#*********
##ALL Fed Agencies##

##creates a dataframe that counts the total number of obligations for each class of new entrants
#over the entire time period count_total_obligations <- FPDS_cleaned_unique_graphs %>% 
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


ggplot(FPDS_obligationscount, aes(x = registrationYear, y = tot_obl_bysize, fill = factor(top_smallbiz_bin), label = percent_obl)) +
  geom_bar(stat = 'identity', position = 'stack') +
  ylab("Total Obligations") +
  xlab("Entry Year") +
  scale_x_continuous(breaks = c(2001:2016)) +
  ##scale_fill_manual(name = "New Entrants Types", values = c("deepskyblue", "royalblue1"), labels = c("small", "non-small")) +
  scale_fill_manual(name = "New Entrants Types", values = c("darkslategray1", "cadetblue4"), labels = c("non-small", "small")) +
  ggtitle("Percent of Obligations for Small and Non-Small New Entrants (2001-2016) - All Federal Agencies")+
  ##geom_text_repel(data = subset(FPDS_bargraphCount, registrationYear >=2014), aes(label = regpersize), size = 4, box.padding = .1, 
  ###    angle = 45) +
  ##geom_text(data = subset(FPDS_bargraphCount, registrationYear < 2014), aes(label = regpersize), size = 4, position = position_stack(vjust = .5), angle = 45)
  geom_text_repel(data = subset(FPDS_obligationscount, registrationYear <= 2016), aes(label = scales::percent(percent_obl_dec)), size = 4, position = position_stack(vjust = .3), angle = 90)



##for DoD only##

FPDS_cleaned_unique_graphs_DOD <- FPDS_cleaned_unique_graphs[(FPDS_cleaned_unique_graphs$customer=="Defense"), ]

##creates a dataframe that counts the total number of obligations in each year
count_total_obligations_DOD <- FPDS_cleaned_unique_graphs_DOD %>% 
  filter(top_smallbiz_bin == 1 | top_smallbiz_bin == 0) %>% 
  group_by(registrationYear) %>% 
  dplyr::summarise(sum_obligations = sum(total_obligations)) 



##create a dataframe that calculates the number of obligations that go to small vendors in each
#year and then number of obligations that go to non-small vendors in each year and joins it 
#with the counts of total number of obligations in each year and then calculate the percent
#of obligations that go to each group in each year
FPDS_obligationscount_DOD <- FPDS_cleaned_unique_graphs_DOD %>%
  filter(top_smallbiz_bin == 1 | top_smallbiz_bin == 0) %>%
  group_by(registrationYear, top_smallbiz_bin) %>%
  dplyr::summarise(sum_obligations = sum(total_obligations)) %>%
  dplyr::rename("tot_obl_bysize"=`sum_obligations`) %>%
  left_join(count_total_obligations_DOD, by = "registrationYear") %>%
  dplyr::rename("tot_obl_byyear"=`sum_obligations`) %>%
  dplyr::mutate(percent_obl_dec = tot_obl_bysize / tot_obl_byyear) %>%
  dplyr::mutate(percent_obl = percent_obl_dec * 100) %>%
  dplyr::mutate(percent_obl = round(percent_obl, 0)) %>%
  dplyr::mutate(total_percent = 100)
 

ggplot(FPDS_obligationscount_DOD, aes(x = registrationYear, y = tot_obl_bysize, fill = factor(top_smallbiz_bin), label = percent_obl)) +
  geom_bar(stat = 'identity', position = 'stack') +
  ylab("Total Obligations") +
  xlab("Entry Year") +
  scale_x_continuous(breaks = c(2001:2016)) +
  ##scale_fill_manual(name = "New Entrants Types", values = c("deepskyblue", "royalblue1"), labels = c("small", "non-small")) +
  scale_fill_manual(name = "New Entrants Types", values = c("darkslategray1", "cadetblue4"), labels = c("non-small", "small")) +
  ggtitle("Percent of Obligations for Small and Non-Small New Entrants (2001-2016) - DoD")+
  ##geom_text_repel(data = subset(FPDS_bargraphCount, registrationYear >=2014), aes(label = regpersize), size = 4, box.padding = .1, 
  ###    angle = 45) +
  ##geom_text(data = subset(FPDS_bargraphCount, registrationYear < 2014), aes(label = regpersize), size = 4, position = position_stack(vjust = .5), angle = 45)
  geom_text_repel(data = subset(FPDS_obligationscount_DOD, registrationYear <= 2016), aes(label = scales::percent(percent_obl_dec)), size = 4, position = position_stack(vjust = .3), angle = 90)



#*********
####3. %of obl that go to small/lg new entrants vs. smll/lg incumbent firms in each year####
#*********



#********#
####4. %of obligations that go to different setaside programs####
#********#


##******#
####5. % of obligations that go to graduated firms####
#*******#

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


graduated_v_nongrad_allfed <- ggplot(FPDS_obligationscount_grad, aes(x = registrationYear, y = tot_obl_bygrad, fill = factor(graduated_10yr), label = percent_obl)) +
  geom_bar(stat = 'identity', position = 'stack') +
  ylab("Total Obligations") +
  xlab("Entry Year") +
  scale_x_continuous(breaks = c(2001:2016)) +
  ##scale_fill_manual(name = "New Entrants Types", values = c("deepskyblue", "royalblue1"), labels = c("small", "non-small")) +
  scale_fill_manual(name = "New Entrants Types", values = c("darkslategray1", "cadetblue4"), labels = c("Non-Graduated", "Graduated")) +
  ggtitle("Percent of Obligations for Graduated and Non-Graduated New Entrants")+
  ##geom_text_repel(data = subset(FPDS_bargraphCount, registrationYear >=2014), aes(label = regpersize), size = 4, box.padding = .1, 
  ###    angle = 45) +
  ##geom_text(data = subset(FPDS_bargraphCount, registrationYear < 2014), aes(label = regpersize), size = 4, position = position_stack(vjust = .5), angle = 45)
  #geom_text_repel(data = subset(FPDS_obligationscount_grad, registrationYear <= 2016), aes(label = scales::percent(percent_obl_dec)), size = 4, position = position_stack(vjust = .3), angle = 90) +
  geom_text(data = subset(FPDS_obligationscount_grad, registrationYear <= 2016), aes(label = scales::percent(percent_obl_dec)), size = 3, position = position_dodge(width = 1), vjust = -0.5) +
  facet_wrap(~graduated_10yr, scales="free", ncol=1) + ##ncol=1 stack them above eachother (in 1 column)
  ylim(0, 3e+11)

graduated_v_nongrad_allfed

##for DoD only##


FPDS_all_yrs1_DOD <- rbind(data_DOD_2001, data_2002_DOD)

FPDS_all_yrs2_DOD <- rbind(FPDS_all_yrs1_DOD, data_2003_DOD)

FPDS_all_yrs3_DOD <- rbind(FPDS_all_yrs2_DOD, data_2004_DOD)

FPDS_all_yrs4_DOD <- rbind(FPDS_all_yrs3_DOD, data_2005_DOD)

FPDS_all_yrs5_DOD <- rbind(FPDS_all_yrs4_DOD, data_2006_DOD)


#step two, create a variable that describes whether a firm graduated and survived 10 yrs
FPDS_all_yrs_DOD <- FPDS_all_yrs5_DOD %>% group_by(Dunsnumber) %>% 
  dplyr::mutate(graduated_10yr = ifelse(graduated==1 & survive_10yr==1, 1, 0))


##step 3, creates a dataframe that counts the total number of obligations in each year
count_total_obligations_grad_DOD <- FPDS_all_yrs_DOD %>% 
  filter(graduated_10yr == 1 | graduated_10yr == 0) %>% 
  group_by(registrationYear) %>% 
  dplyr::summarise(sum_obligations = sum(total_obligations)) 



##create a dataframe that calculates the number of obligations that go to graduated vendors in each
#year and then number of obligations that go to non-graduated vendors in each year and joins it 
#with the counts of total number of obligations in each year and then calculate the percent
#of obligations that go to each group in each year
FPDS_obligationscount_grad_DOD <- FPDS_all_yrs_DOD %>%
  filter(graduated_10yr == 1 | graduated_10yr == 0) %>%
  group_by(registrationYear, graduated_10yr) %>%
  dplyr::summarise(sum_obligations = sum(total_obligations)) %>%
  dplyr::rename("tot_obl_bygrad"=`sum_obligations`) %>%
  left_join(count_total_obligations_grad_DOD, by = "registrationYear") %>%
  dplyr::rename("tot_obl_byyear"=`sum_obligations`) %>%
  dplyr::mutate(percent_obl_dec = tot_obl_bygrad / tot_obl_byyear) %>%
  dplyr::mutate(percent_obl = percent_obl_dec * 100) %>%
  dplyr::mutate(percent_obl = round(percent_obl, 0)) %>%
  dplyr::mutate(total_percent = 100)


graduated_v_nongrad_DoD <- ggplot(FPDS_obligationscount_grad_DOD, aes(x = registrationYear, y = tot_obl_bygrad, fill = factor(graduated_10yr), label = percent_obl)) +
  geom_bar(stat = 'identity', position = 'stack') +
  ylab("Total Obligations") +
  xlab("Entry Year") +
  scale_x_continuous(breaks = c(2001:2016)) +
  ##scale_fill_manual(name = "New Entrants Types", values = c("deepskyblue", "royalblue1"), labels = c("small", "non-small")) +
  scale_fill_manual(name = "New Entrants Types", values = c("darkslategray1", "cadetblue4"), labels = c("Non-Graduated", "Graduated")) +
  ggtitle("Percent of Obligations for Graduated and Non-Graduated New Entrants DOD")+
  ##geom_text_repel(data = subset(FPDS_bargraphCount, registrationYear >=2014), aes(label = regpersize), size = 4, box.padding = .1, 
  ###    angle = 45) +
  ##geom_text(data = subset(FPDS_bargraphCount, registrationYear < 2014), aes(label = regpersize), size = 4, position = position_stack(vjust = .5), angle = 45)
  #geom_text_repel(data = subset(FPDS_obligationscount_grad_DOD, registrationYear <= 2016), aes(label = scales::percent(percent_obl_dec)), size = 4, position = position_stack(vjust = .3), angle = 90) +
  geom_text(data = subset(FPDS_obligationscount_grad_DOD, registrationYear <= 2016), aes(label = scales::percent(percent_obl_dec)), size = 3, position = position_dodge(width = 1), vjust = -0.5) +
  facet_wrap(~graduated_10yr, scales="free", ncol=1) + ##ncol=1 stack them above eachother (in 1 column)
  ylim(0, 2.1e+11)

graduated_v_nongrad_DoD

grid.arrange(graduated_v_nongrad_allfed, graduated_v_nongrad_DoD)

#*************************************************************************#
#*************************************#
####Graphs from data cleaned by Greg####
#*************************************#


####setup####
library(dplyr)
library(ggplot2)
library(csis360)


####data cleaning####
##SRC's cleaned data##
##sam work computer
setwd("K:/2018-01 NPS New Entrants/Data/Data/Cleaning data/FPDS")

##sam laptop
#setwd("/Users/samanthacohen/Desktop/Diig backup/New Entrants/R Data")

load(file = "FPDS_datapull_all_v3.Rda")
#******************************************

#******************************************
##GS's Cleaned data##
#load in data
load(file = file.path("../Cleaning Data/FPDS/FPDS_datapull_all_v3.Rda"))
load(file = file.path("../Cleaning Data/FPDS/FPDS_cleaned_unique_wtotalobligations.Rda"))

file.exists("Raw Data/FPDS/Vendor.SP_DunsnumberNewEntrants_all.txt")
dir.exists("Cleaning Data/")
dir.exists("Analysis")
file.exists("New Entrants.Rproj")
list.files("../")

####feddunsyear####
setwd("K:/2018-01 NPS New Entrants/Data/Data/Raw Data/FPDS")
FPDS_data <- read.delim("Vendor.SP_DunsnumberNewEntrants_all.txt", fill = TRUE, header=TRUE,  na.strings = c("", "NULL"))

FPDS_data<-as.data.frame(FPDS_data)

FPDS_data<-csis360::remove_bom(FPDS_data)
# FPDS_data<-subset(FPDS_data,fiscal_year>=2000)

FPDS_data<-csis360::deflate(FPDS_data,money_var="obligatedAmount",
                            fy_var="fiscal_year")
#Calculate first_year
FPDS_data<-FPDS_data %>%
  group_by(Dunsnumber) %>%
  dplyr::mutate(first_year=min(fiscal_year),
                entrant=ifelse(min(fiscal_year)==fiscal_year,TRUE,FALSE))

#Calculate annual spend and presence.
fed_duns_fyear<-FPDS_data %>%
  group_by(Dunsnumber,fiscal_year,entrant,first_year) %>%
  #dplyr::summarize(obligatedAmount.2017=sum(obligatedAmount.Deflator.2017,na.rm=TRUE),
  dplyr::summarize(obligatedAmount.Deflator.2016=sum(obligatedAmount.Deflator.2016,na.rm=TRUE),
                   present=max(obligatedAmount.Deflator.2016,na.rm=TRUE))
fed_duns_fyear$present<-ifelse(fed_duns_fyear$present>0,1,0)
summary(fed_duns_fyear$present)


##Dod
dod_duns_fyear<-subset(FPDS_data,customer=="Defense") %>%
  group_by(Dunsnumber,fiscal_year,entrant,first_year) %>%
  #dplyr::summarize(obligatedAmount.2017=sum(obligatedAmount.Deflator.2017,na.rm=TRUE),
  dplyr::summarize(obligatedAmount.Deflator.2016=sum(obligatedAmount.Deflator.2016,na.rm=TRUE),
                   present=max(obligatedAmount.Deflator.2016,na.rm=TRUE))
dod_duns_fyear$present<-ifelse(dod_duns_fyear$present>0,1,0)
summary(dod_duns_fyear$present)


#Check if dunsnumber is present in previous year
FPDS_duns_prev_fyear<-subset(fed_duns_fyear,select=c(fiscal_year,Dunsnumber,present))
FPDS_duns_prev_fyear$fiscal_year<-FPDS_duns_prev_fyear$fiscal_year+1
colnames(FPDS_duns_prev_fyear)[colnames(FPDS_duns_prev_fyear)=="present"]<-"prev_present"
fed_duns_fyear<-left_join(fed_duns_fyear,FPDS_duns_prev_fyear)
#Label NAs with 0, except when at start or end of series.
fed_duns_fyear$prev_present[is.na(fed_duns_fyear$prev_present) & fed_duns_fyear$fiscal_year!=2000]<-0
summary(fed_duns_fyear$prev_present)
rm(FPDS_duns_prev_fyear)

#dod
FPDS_duns_prev_fyear_dod<-subset(dod_duns_fyear,select=c(fiscal_year,Dunsnumber,present))
FPDS_duns_prev_fyear_dod$fiscal_year<-FPDS_duns_prev_fyear_dod$fiscal_year+1
colnames(FPDS_duns_prev_fyear_dod)[colnames(FPDS_duns_prev_fyear_dod)=="present"]<-"prev_present"
dod_duns_fyear<-left_join(dod_duns_fyear,FPDS_duns_prev_fyear_dod)
#Label NAs with 0, except when at start or end of series.
dod_duns_fyear$prev_present[is.na(dod_duns_fyear$prev_present) & dod_duns_fyear$fiscal_year!=2000]<-0
summary(dod_duns_fyear$prev_present)
rm(FPDS_duns_prev_fyear_dod)



#Check if dunsnumber is present in next year
FPDS_duns_next_fyear<-subset(fed_duns_fyear,select=c(fiscal_year,Dunsnumber,present))
FPDS_duns_next_fyear$fiscal_year<-FPDS_duns_next_fyear$fiscal_year-1
colnames(FPDS_duns_next_fyear)[colnames(FPDS_duns_next_fyear)=="present"]<-"next_present"
fed_duns_fyear<-left_join(fed_duns_fyear,FPDS_duns_next_fyear)
#Label NAs with 0, except when at start or end of series.
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


##DoD
FPDS_duns_next_fyear_dod<-subset(dod_duns_fyear,select=c(fiscal_year,Dunsnumber,present))
FPDS_duns_next_fyear_dod$fiscal_year<-FPDS_duns_next_fyear_dod$fiscal_year-1
colnames(FPDS_duns_next_fyear_dod)[colnames(FPDS_duns_next_fyear_dod)=="present"]<-"next_present"
dod_duns_fyear<-left_join(dod_duns_fyear,FPDS_duns_next_fyear_dod)
#Label NAs with 0, except when at start or end of series.
dod_duns_fyear$next_present[is.na(dod_duns_fyear$next_present) & dod_duns_fyear$fiscal_year<2017]<-0
summary(dod_duns_fyear$next_present)
rm(FPDS_duns_next_fyear)


dod_duns_fyear$sample_year<-dod_duns_fyear$first_year
dod_duns_fyear$sample_year[dod_duns_fyear$sample_year<2001 | dod_duns_fyear$sample_year>2006]<-"Not in sample"
dod_duns_fyear$sample_year<-factor(dod_duns_fyear$sample_year,levels=c("Not in sample","2001","2002","2003","2004","2005","2006"))



dod_duns_fyear$status<-NA
dod_duns_fyear$status[dod_duns_fyear$present==0]<-"No net payments"
dod_duns_fyear$status[is.na(dod_duns_fyear$status) & dod_duns_fyear$prev_present==0 & dod_duns_fyear$next_present==0]<-"Blip"
dod_duns_fyear$status[is.na(dod_duns_fyear$status) & dod_duns_fyear$prev_present==0 & !is.na(dod_duns_fyear$next_present)]<-"Enter"
dod_duns_fyear$status[is.na(dod_duns_fyear$status) & dod_duns_fyear$next_present==0 & !is.na(dod_duns_fyear$prev_present)]<-"Exit"
dod_duns_fyear$status[is.na(dod_duns_fyear$status) & dod_duns_fyear$prev_present==1 & dod_duns_fyear$next_present==1]<-"Steady"
dod_duns_fyear$status<-factor(dod_duns_fyear$status)
summary(dod_duns_fyear$status)

dod_duns_fyear<-subset(dod_duns_fyear,fiscal_year>=2000)

save(fed_duns_fyear, file = "fed_duns_fyear.rda")

save(file="../Cleaning Data/footing.rda",FPDS_data,fed_duns_fyear)
# load(file="../Cleaning Data/footing.rda")


#src for graphs
#all fed
table(fed_duns_fyear$sample_year)
fed_duns_fyear$sample_year_bin <- revalue(fed_duns_fyear$sample_year, c("Not in sample"="0", "2001"="1", "2002"="2", "2003"="3", "2004"="4", "2005"="5", "2006"="6"))
str(fed_duns_fyear$sample_year_bin)
table(fed_duns_fyear$sample_year_bin)
fed_duns_fyear$sample_year_bin <- as.numeric(as.character(fed_duns_fyear$sample_year_bin))
str(fed_duns_fyear$sample_year_bin)
table(fed_duns_fyear$sample_year_bin)

#2. drop those not in sample and for the years not included in observation period
fed_duns_fyear_samples <- fed_duns_fyear[!(fed_duns_fyear$sample_year_bin<1), ]
fed_duns_fyear_samples_op <- fed_duns_fyear_samples[!(fed_duns_fyear_samples$fiscal_year<2001), ]
fed_duns_fyear_samples_op <- fed_duns_fyear_samples_op[!(fed_duns_fyear_samples_op$fiscal_year>2016), ]

#DoD
table(dod_duns_fyear$sample_year)
dod_duns_fyear$sample_year_bin <- revalue(dod_duns_fyear$sample_year, c("Not in sample"="0", "2001"="1", "2002"="2", "2003"="3", "2004"="4", "2005"="5", "2006"="6"))
str(dod_duns_fyear$sample_year_bin)
table(dod_duns_fyear$sample_year_bin)
dod_duns_fyear$sample_year_bin <- as.numeric(as.character(dod_duns_fyear$sample_year_bin))
str(dod_duns_fyear$sample_year_bin)
table(dod_duns_fyear$sample_year_bin)

#2. drop those not in sample and for the years not included in observation period
dod_duns_fyear_samples <- dod_duns_fyear[!(dod_duns_fyear$sample_year_bin<1), ]
dod_duns_fyear_samples_op <- dod_duns_fyear_samples[!(dod_duns_fyear_samples$fiscal_year<2001), ]
dod_duns_fyear_samples_op <- dod_duns_fyear_samples_op[!(dod_duns_fyear_samples_op$fiscal_year>2016), ]

save(dod_duns_fyear_samples_op, file="dod_duns_fyear_samples_op.rda")


#******************************************************************

#*******************#
####New Entrants Counts####
#*******************#

#**********#
####1. Number of newe Entrants per year 2001-2016 all fed agencies####
#*********#

##drop observations with Registration Year before 2001
FPDS_cleaned_unique <- FPDS_cleaned_unique[!(FPDS_cleaned_unique$registrationYear<2001), ]
FPDS_cleaned_unique <- FPDS_cleaned_unique[!(FPDS_cleaned_unique$registrationYear>2016), ]

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


NE_count_allfed <- ggplot(FPDS_bargraphCount, aes(x = registrationYear, y = regpersize, fill = factor(top_smallbiz_bin), label = regperyear)) +
  geom_bar(stat = 'identity', position = 'stack') +
  ylab("Number of New Entrants") +
  xlab("Registration Year") +
  scale_x_continuous(breaks = c(2001:2016)) +
  scale_fill_manual(name = "New Entrant Type", values = c("#66CCCC", "#336666"), labels = c("non-small", "small")) +
  ggtitle("All Federal Agencies")+
  theme(text=element_text(size=16, family="Comic Sans MS")) +
  ##geom_text_repel(data = subset(FPDS_bargraphCount, registrationYear >=2014), aes(label = regpersize), size = 4, box.padding = .1, 
  ###    angle = 45) +
  ##geom_text(data = subset(FPDS_bargraphCount, registrationYear < 2014), aes(label = regpersize), size = 4, position = position_stack(vjust = .5), angle = 45)
  geom_text(data = subset(FPDS_bargraphCount, registrationYear <= 2016), aes(label = regpersize), size = 4, position = position_stack(vjust = .5), angle = 45) +
  scale_y_continuous(label=comma)

NE_count_allfed

#******************************
##chart for DoD
#******************************
FPDS_cleaned_unique_DOD <- FPDS_cleaned_unique[(FPDS_cleaned_unique$customer=="Defense"), ]

totyear_count <- FPDS_cleaned_unique_DOD %>% 
  filter(top_smallbiz_bin == 1 | top_smallbiz_bin == 0) %>% 
  group_by(registrationYear) %>% 
  dplyr::summarise(n())  


FPDS_bargraphCount <- FPDS_cleaned_unique_DOD %>%
  filter(top_smallbiz_bin == 1 | top_smallbiz_bin == 0) %>%
  group_by(registrationYear, top_smallbiz_bin) %>%
  dplyr::summarise(n()) %>%
  dplyr::rename("regpersize"=`n()`) %>%
  left_join(totyear_count, by = "registrationYear") %>%
  dplyr::rename("regperyear"=`n()`) 


NE_count_DoD <- ggplot(FPDS_bargraphCount, aes(x = registrationYear, y = regpersize, fill = factor(top_smallbiz_bin), label = regperyear)) +
  geom_bar(stat = 'identity', position = 'stack') +
  ylab("Number of New Entrants") +
  xlab("Registration Year") +
  scale_x_continuous(breaks = c(2001:2016)) +
  theme(text=element_text(size=16, family="Comic Sans MS")) +
  scale_fill_manual(name = "New Entrant Type", values = c("#66CCCC", "#336666"), labels = c("non-small", "small")) +
  ggtitle("Number of New Entrants Per Year (2001-2016) - DoD")+
  ##geom_text_repel(data = subset(FPDS_bargraphCount, registrationYear >=2014), aes(label = regpersize), size = 4, box.padding = .1, 
  ##angle = 45) +
  ##geom_text(data = subset(FPDS_bargraphCount, registrationYear < 2014), aes(label = regpersize), size = 4, position = position_stack(vjust = .5), angle = 45)
  geom_text(data = subset(FPDS_bargraphCount, registrationYear <= 2016), aes(label = regpersize), size = 4, position = position_stack(vjust = .5), angle = 45) +
  scale_y_continuous(label=comma)
NE_count_DoD

##combine graphs

grid.arrange(NE_count_allfed, NE_count_DoD)




#**********#
#####2. Number of New Entrants in each sample and over time####
#*********#

##****
#all fed agencies
#*****
##drop those not in sample 
#1. make not in sample binary
table(fed_duns_fyear$sample_year)
fed_duns_fyear$sample_year_bin <- revalue(fed_duns_fyear$sample_year, c("Not in sample"="0", "2001"="1", "2002"="2", "2003"="3", "2004"="4", "2005"="5", "2006"="6"))
str(fed_duns_fyear$sample_year_bin)
table(fed_duns_fyear$sample_year_bin)
fed_duns_fyear$sample_year_bin <- as.numeric(as.character(fed_duns_fyear$sample_year_bin))
str(fed_duns_fyear$sample_year_bin)
table(fed_duns_fyear$sample_year_bin)

#2. drop those not in sample and for the years not included in observation period
fed_duns_fyear_samples <- fed_duns_fyear[!(fed_duns_fyear$sample_year_bin<1), ]
fed_duns_fyear_samples_op <- fed_duns_fyear_samples[!(fed_duns_fyear_samples$fiscal_year<2001), ]
fed_duns_fyear_samples_op <- fed_duns_fyear_samples_op[!(fed_duns_fyear_samples_op$fiscal_year>2016), ]


###A. Stacked

ggplot(fed_duns_fyear_samples_op, aes(x = fiscal_year, fill = factor(sample_year))) +
  geom_histogram(position = "stack", binwidth = .5) +
  ggtitle("Number of New Entrants in Each Sample Over Time") +
  xlab("Fiscal Year") +
  ylab("Count") +
  scale_fill_manual(name = "Sample", values = c("#33CCCC", "#000066","#3399CC", "#66FFFF", "#336666", "#999999"), labels = c("2001", "2002", "2003", "2004", "2005", "2006")) +
  scale_x_continuous(breaks = c(2001:2016))




###B. Faceted (for faceted, x axis is fiscal year)

NE_eachsample_overtime_allfed <- ggplot(fed_duns_fyear_samples_op, aes(x = fiscal_year, fill = factor(sample_year))) +
  geom_histogram(binwidth = .5) +
  ggtitle("Number of New Entrants in Each Sample Over Time - All Fed Agencies") +
  xlab("Fiscal Year") +
  ylab("Count") +
  scale_fill_manual(name = "Sample", values = c("#33CCCC", "#000066","#3399CC", "#66FFFF", "#336666", "#999999"), labels = c("2001", "2002", "2003", "2004", "2005", "2006")) +
  scale_x_continuous(breaks = c(2001:2016)) +
  facet_wrap(~sample_year, scales="fixed", ncol=1) + 
  guides(fill=FALSE) +
  scale_y_continuous(label=comma)

##****
#DoD
#*****
table(dod_duns_fyear$sample_year)
dod_duns_fyear$sample_year_bin <- revalue(dod_duns_fyear$sample_year, c("Not in sample"="0", "2001"="1", "2002"="2", "2003"="3", "2004"="4", "2005"="5", "2006"="6"))
str(dod_duns_fyear$sample_year_bin)
table(dod_duns_fyear$sample_year_bin)
dod_duns_fyear$sample_year_bin <- as.numeric(as.character(dod_duns_fyear$sample_year_bin))
str(dod_duns_fyear$sample_year_bin)
table(dod_duns_fyear$sample_year_bin)

#2. drop those not in sample and for the years not included in observation period
dod_duns_fyear_samples <- dod_duns_fyear[!(dod_duns_fyear$sample_year_bin<1), ]
dod_duns_fyear_samples_op <- dod_duns_fyear_samples[!(dod_duns_fyear_samples$fiscal_year<2001), ]
dod_duns_fyear_samples_op <- dod_duns_fyear_samples_op[!(dod_duns_fyear_samples_op$fiscal_year>2016), ]



NE_eachsample_overtime_dod <- ggplot(dod_duns_fyear_samples_op, aes(x = fiscal_year, fill = factor(sample_year))) +
  geom_histogram(binwidth = .5) +
  ggtitle("Number of New Entrants in Each Sample Over Time - DoD") +
  xlab("Fiscal Year") +
  ylab("Count") +
  scale_fill_manual(name = "Sample", values = c("#33CCCC", "#000066","#3399CC", "#66FFFF", "#336666", "#999999"), labels = c("2001", "2002", "2003", "2004", "2005", "2006")) +
  scale_x_continuous(breaks = c(2001:2016)) +
  facet_wrap(~sample_year, scales="fixed", ncol=1) +
  scale_y_continuous(label=comma)

##combine all fed agencies with dod
grid.arrange(NE_eachsample_overtime_allfed, NE_eachsample_overtime_dod, ncol=2)


#**********#
#####3. Number of New Entrants vs. Incumbents in each year####
#*********#

#****
#all fed agencies
#*****
##stacked##

library(extrafont)

#drop years before 2001 and after 2016
load(file="fed_duns_fyear.rda")

fed_duns_fyear_op <- fed_duns_fyear[!(fed_duns_fyear$fiscal_year<2001), ]
fed_duns_fyear_op <- fed_duns_fyear_op[!(fed_duns_fyear_op$fiscal_year>2016), ]

NE_v_incumbent_count_allfed <- ggplot(fed_duns_fyear_op, aes(x = fiscal_year, fill = factor(entrant))) +
  geom_histogram(position = "stack", binwidth = .5) +
  scale_x_continuous(breaks = c(2001:2016)) +
  xlab("Fiscal Year") +
  ylab("Number of Vendors") +
  theme(text=element_text(size=16, family="Comic Sans MS")) +
  scale_fill_manual(name = "Vendor Type", values = c("#66CCCC", "#336666"), labels = c("Incumbent", "New Entrant")) +
  #ggtitle("Number of New Entrants vs. Number of Incumbent Firms Over Time - All Federal Agencies") +
  ggtitle("All Federal Agencies") +
  scale_y_continuous(label=comma)


#****
#DoD
#*****
#drop years before 2001 and after 2016
dod_duns_fyear_op <- dod_duns_fyear[!(dod_duns_fyear$fiscal_year<2001), ]
dod_duns_fyear_op <- dod_duns_fyear_op[!(dod_duns_fyear_op$fiscal_year>2016), ]

NE_v_incumbent_count_DoD <- ggplot(dod_duns_fyear_op, aes(x = fiscal_year, fill = factor(entrant))) +
  geom_histogram(position = "stack", binwidth = .5) +
  scale_x_continuous(breaks = c(2001:2016)) +
  xlab("Fiscal Year") +
  ylab("Number of Vendors") +
  theme(text=element_text(size=16, family="Comic Sans MS")) +
  scale_fill_manual(name = "Vendor Type", values = c("#66CCCC", "#336666"), labels = c("Incumbent", "New Entrant")) +
  ggtitle("DoD") 
#guides(fill=FALSE) +
#scale_y_continuous(label=comma)

##combine all fed and dod
grid.arrange(NE_v_incumbent_count_allfed, NE_v_incumbent_count_DoD, ncol=1)


#******************************************************************************************************
#*******************#
#####Obligations#####
#*******************#

#**********#
#####1. Percent of Obligations for small and non-small new entrants all fed and DoD faceted####
#*********#
FPDS_cleaned_unique_graphs <- FPDS_cleaned_unique[!(FPDS_cleaned_unique$registrationYear<2001), ]
FPDS_cleaned_unique_graphs <- FPDS_cleaned_unique_graphs[!(FPDS_cleaned_unique_graphs$registrationYear>2016), ]

##ALL Fed Agencies##

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


obligations_small_v_nsmall_allfed_facet <- ggplot(FPDS_obligationscount, aes(x = registrationYear, y = tot_obl_bysize, fill = factor(top_smallbiz_bin), label = percent_obl)) +
  geom_bar(stat = 'identity', position = 'stack') +
  ylab("Total Obligations") +
  xlab("Entry Year") +
  scale_x_continuous(breaks = c(2001:2016)) +
  ##scale_fill_manual(name = "New Entrants Types", values = c("deepskyblue", "royalblue1"), labels = c("small", "non-small")) +
  scale_fill_manual(name = "New Entrants Types", values = c("#66CCCC", "#336666"), labels = c("non-small", "small")) +
  ggtitle("All Federal Agencies")+
  theme(text=element_text(size=16, family="Comic Sans MS")) +
  geom_text(data = subset(FPDS_obligationscount, registrationYear <= 2016), aes(label = scales::percent(percent_obl_dec)), size = 3, position = position_dodge(width = 1), vjust = -0.5) +
  facet_wrap(~top_smallbiz_bin, scales="fixed", ncol=1) + ##ncol=1 stack them above eachother (in 1 column)
  theme(strip.background = element_blank(), strip.text = element_blank()) +
  theme(strip.background = element_blank(), strip.text = element_blank()) +
  ylim(0, 2.7e+11) +
  scale_y_continuous(label=unit_format(unit = "m", scale=1e-6))

obligations_small_v_nsmall_allfed_facet




##for DoD only##

FPDS_cleaned_unique_graphs_DOD <- FPDS_cleaned_unique_graphs[(FPDS_cleaned_unique_graphs$customer=="Defense"), ]

##creates a dataframe that counts the total number of obligations in each year
count_total_obligations_DOD <- FPDS_cleaned_unique_graphs_DOD %>% 
  filter(top_smallbiz_bin == 1 | top_smallbiz_bin == 0) %>% 
  group_by(registrationYear) %>% 
  dplyr::summarise(sum_obligations = sum(total_obligations)) 



##create a dataframe that calculates the number of obligations that go to small vendors in each
#year and then number of obligations that go to non-small vendors in each year and joins it 
#with the counts of total number of obligations in each year and then calculate the percent
#of obligations that go to each group in each year
FPDS_obligationscount_DOD <- FPDS_cleaned_unique_graphs_DOD %>%
  filter(top_smallbiz_bin == 1 | top_smallbiz_bin == 0) %>%
  group_by(registrationYear, top_smallbiz_bin) %>%
  dplyr::summarise(sum_obligations = sum(total_obligations)) %>%
  dplyr::rename("tot_obl_bysize"=`sum_obligations`) %>%
  left_join(count_total_obligations_DOD, by = "registrationYear") %>%
  dplyr::rename("tot_obl_byyear"=`sum_obligations`) %>%
  dplyr::mutate(percent_obl_dec = tot_obl_bysize / tot_obl_byyear) %>%
  dplyr::mutate(percent_obl = percent_obl_dec * 100) %>%
  dplyr::mutate(percent_obl = round(percent_obl, 0)) %>%
  dplyr::mutate(total_percent = 100)



obligations_small_v_nsmall_DoD_facet <- ggplot(FPDS_obligationscount_DOD, aes(x = registrationYear, y = tot_obl_bysize, fill = factor(top_smallbiz_bin), label = percent_obl)) +
  geom_bar(stat = 'identity', position = 'stack') +
  ylab("Total Obligations") +
  xlab("Entry Year") +
  scale_x_continuous(breaks = c(2001:2016)) +
  ##scale_fill_manual(name = "New Entrants Types", values = c("deepskyblue", "royalblue1"), labels = c("small", "non-small")) +
  scale_fill_manual(name = "New Entrants Types", values = c("#66CCCC", "#336666"), labels = c("non-small", "small")) +
  ggtitle("DoD")+
  theme(text=element_text(size=16, family="Comic Sans MS")) +
  geom_text(data = subset(FPDS_obligationscount_DOD, registrationYear <= 2016), aes(label = scales::percent(percent_obl_dec)), size = 3, position = position_dodge(width = 1), vjust = -0.5) +
  facet_wrap(~top_smallbiz_bin, scales="fixed", ncol=1) + ##ncol=1 stack them above eachother (in 1 column) 
  theme(strip.background = element_blank(), strip.text = element_blank()) +
  ylim(0, 2.55e+11) +
  scale_y_continuous(label=unit_format(unit = "m", scale=1e-6))


obligations_small_v_nsmall_DoD_facet


grid.arrange(obligations_small_v_nsmall_allfed_facet, obligations_small_v_nsmall_DoD_facet)



#**********#
#####2. Average obligation size for small and non-small new entrants in each year####
#*********#


#**********#
####3. Obligations between incumbents and new entrants in each year####
#*********#

load(file = "fed_duns_fyear_op.rda")

##************##
##all fed agencies##
##************##



###B. Faceted 
obl_NE_v_incumbents_allfed <- ggplot(fed_duns_fyear_op, aes(x = fiscal_year, y = obligatedAmount.Deflator.2016, fill = factor(entrant))) +
  geom_bar(stat = 'identity', position = 'stack') + 
  ylab("Total Obligations") +
  xlab("Fiscal Year") +
  scale_x_continuous(breaks = c(2001:2016)) +
  theme(text=element_text(size=16, family="Comic Sans MS")) +
  scale_fill_manual(name = "Vendor Type", values = c("#66CCCC", "#336666"), labels = c("Incumbent Firms", "New Entrant")) +
  ggtitle("All Federal Agencies") +
  facet_wrap(~entrant, scales="free", ncol=1) +
  theme(strip.background = element_blank(), strip.text = element_blank()) +
  scale_y_continuous(label=unit_format(unit = "m", scale=1e-6)) ##ncol=1 stack them above eachother (in 1 column) 

obl_NE_v_incumbents_allfed



##************##
##****DoD*****##
##************##

###A. Stacked


###B. Faceted
obl_NE_v_incumbents_dod <- ggplot(dod_duns_fyear_op, aes(x = fiscal_year, y = obligatedAmount.Deflator.2016, fill = factor(entrant))) +
  geom_bar(stat = 'identity', position = 'stack') + 
  ylab("Total Obligations") +
  xlab("Fiscal Year") +
  scale_x_continuous(breaks = c(2001:2016)) +
  theme(text=element_text(size=16, family="Comic Sans MS")) +
  scale_fill_manual(name = "Vendor Type", values = c("#66CCCC", "#336666"), labels = c("Incumbent Firms", "New Entrant")) +
  ggtitle("DoD") +
  facet_wrap(~entrant, scales="free", ncol=1) +
  theme(strip.background = element_blank(), strip.text = element_blank()) +
  scale_y_continuous(label=unit_format(unit = "m", scale=1e-6))##ncol=1 stack them above eachother (in 1 column)
obl_NE_v_incumbents_dod

grid.arrange(obl_NE_v_incumbents_allfed, obl_NE_v_incumbents_dod)

#**********#
####4. Average Obligations between incumbents and new entrants in each year####
#*********#


#**********#
####5. Obligations to each sample over time####
#*********#

###A. Stacked

##all fed agencies
ggplot(fed_duns_fyear_samples_op, aes(x = fiscal_year, y = obligatedAmount.Deflator.2016, fill = factor(sample_year))) +
  geom_bar(stat = 'identity', position = 'stack') +
  ylab("Total Obligations") +
  xlab("New Entrant Sample") +
  scale_x_continuous(breaks = c(2001:2016)) +
  scale_fill_manual(name = "Sample", values = c("#33CCCC", "#000066","#3399CC", "#66FFFF", "#336666", "#999999"), labels = c("2001", "2002", "2003", "2004", "2005", "2006")) +
  ggtitle("Obligations to Each Sample Over Time - All Federal Agencies") 
  


###B. Faceted
##all fed agencies
obl_eachsample_allfed <- ggplot(fed_duns_fyear_samples_op, aes(x = fiscal_year, y = obligatedAmount.Deflator.2016, fill = factor(sample_year))) +
  geom_bar(stat = 'identity', position = 'stack') +
  ylab("Total Obligations") +
  xlab("New Entrant Sample") +
  scale_x_continuous(breaks = c(2001:2016)) +
  scale_fill_manual(name = "Sample", values = c("#33CCCC", "#000066","#3399CC", "#66FFFF", "#336666", "#999999"), labels = c("2001", "2002", "2003", "2004", "2005", "2006")) +
  ggtitle("Obligations to Each Sample - All Federal Agencies") +
  facet_wrap(~sample_year, scales="fixed", ncol=1) +
  guides(fill=FALSE) +
  scale_y_continuous(label=unit_format(unit = "m", scale = 1e-6))

#*****
#dod
#****
obl_eachsample_dod <- ggplot(dod_duns_fyear_samples_op, aes(x = fiscal_year, y = obligatedAmount.Deflator.2016, fill = factor(sample_year))) +
  geom_bar(stat = 'identity', position = 'stack') +
  ylab("Total Obligations") +
  xlab("New Entrant Sample") +
  scale_x_continuous(breaks = c(2001:2016)) +
  scale_fill_manual(name = "Sample", values = c("#33CCCC", "#000066","#3399CC", "#66FFFF", "#336666", "#999999"), labels = c("2001", "2002", "2003", "2004", "2005", "2006")) +
  ggtitle("DoD") +
  facet_wrap(~sample_year, scales="fixed", ncol=1) +
  scale_y_continuous(label=unit_format(unit = "m", scale = 1e-6))
  #scale_y_continuous(label=dollar_format())

grid.arrange(obl_eachsample_allfed, obl_eachsample_dod, ncol=2)

#**********#
####6. Obligations to non-graduated and graduated firms####
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


graduated_v_nongrad_allfed <- ggplot(FPDS_obligationscount_grad, aes(x = registrationYear, y = tot_obl_bygrad, fill = factor(graduated_10yr), label = percent_obl)) +
  geom_bar(stat = 'identity', position = 'stack') +
  ylab("Total Obligations") +
  xlab("Entry Year") +
  scale_x_continuous(breaks = c(2001:2006)) +
  scale_y_continuous(label=unit_format(unit = "m", scale=1e-6), breaks = c(0, 5e+10, 1e+11, 1.5e+11, 2e+11, 2.5e+11, 3.0e+11, 3.5e+11, 4e+11)) +
  ##scale_fill_manual(name = "New Entrants Types", values = c("deepskyblue", "royalblue1"), labels = c("small", "non-small")) +
  scale_fill_manual(name = "New Entrant Type", values = c("#66CCCC", "#336666"), labels = c("Non-Graduated", "Graduated")) +
  ggtitle("Percent of Obligations for Graduated and Non-Graduated New Entrants")+
  ##geom_text_repel(data = subset(FPDS_bargraphCount, registrationYear >=2014), aes(label = regpersize), size = 4, box.padding = .1, 
  ###    angle = 45) +
  ##geom_text(data = subset(FPDS_bargraphCount, registrationYear < 2014), aes(label = regpersize), size = 4, position = position_stack(vjust = .5), angle = 45)
  #geom_text_repel(data = subset(FPDS_obligationscount_grad, registrationYear <= 2016), aes(label = scales::percent(percent_obl_dec)), size = 4, position = position_stack(vjust = .3), angle = 90) +
  geom_text(data = subset(FPDS_obligationscount_grad, registrationYear <= 2016), aes(label = scales::percent(percent_obl_dec)), size = 3, position = position_dodge(width = 1), vjust = -0.5) +
  facet_wrap(~graduated_10yr, ncol=1) ##ncol=1 stack them above eachother (in 1 column)
  #ylim(0, 3e+11) +
  

graduated_v_nongrad_allfed

##for DoD only##


FPDS_all_yrs1_DOD <- rbind(data_DOD_2001, data_2002_DOD)

FPDS_all_yrs2_DOD <- rbind(FPDS_all_yrs1_DOD, data_2003_DOD)

FPDS_all_yrs3_DOD <- rbind(FPDS_all_yrs2_DOD, data_2004_DOD)

FPDS_all_yrs4_DOD <- rbind(FPDS_all_yrs3_DOD, data_2005_DOD)

FPDS_all_yrs5_DOD <- rbind(FPDS_all_yrs4_DOD, data_2006_DOD)


#step two, create a variable that describes whether a firm graduated and survived 10 yrs
FPDS_all_yrs_DOD <- FPDS_all_yrs5_DOD %>% group_by(Dunsnumber) %>% 
  dplyr::mutate(graduated_10yr = ifelse(graduated==1 & survive_10yr==1, 1, 0))


##step 3, creates a dataframe that counts the total number of obligations in each year
count_total_obligations_grad_DOD <- FPDS_all_yrs_DOD %>% 
  filter(graduated_10yr == 1 | graduated_10yr == 0) %>% 
  group_by(registrationYear) %>% 
  dplyr::summarise(sum_obligations = sum(total_obligations)) 



##create a dataframe that calculates the number of obligations that go to graduated vendors in each
#year and then number of obligations that go to non-graduated vendors in each year and joins it 
#with the counts of total number of obligations in each year and then calculate the percent
#of obligations that go to each group in each year
FPDS_obligationscount_grad_DOD <- FPDS_all_yrs_DOD %>%
  filter(graduated_10yr == 1 | graduated_10yr == 0) %>%
  group_by(registrationYear, graduated_10yr) %>%
  dplyr::summarise(sum_obligations = sum(total_obligations)) %>%
  dplyr::rename("tot_obl_bygrad"=`sum_obligations`) %>%
  left_join(count_total_obligations_grad_DOD, by = "registrationYear") %>%
  dplyr::rename("tot_obl_byyear"=`sum_obligations`) %>%
  dplyr::mutate(percent_obl_dec = tot_obl_bygrad / tot_obl_byyear) %>%
  dplyr::mutate(percent_obl = percent_obl_dec * 100) %>%
  dplyr::mutate(percent_obl = round(percent_obl, 0)) %>%
  dplyr::mutate(total_percent = 100)


graduated_v_nongrad_DoD <- ggplot(FPDS_obligationscount_grad_DOD, aes(x = registrationYear, y = tot_obl_bygrad, fill = factor(graduated_10yr), label = percent_obl)) +
  geom_bar(stat = 'identity', position = 'stack') +
  ylab("Total Obligations") +
  xlab("Entry Year") +
  scale_x_continuous(breaks = c(2001:2006)) +
  scale_y_continuous(label=unit_format(unit = "m", scale=1e-6), breaks=c(0, 5e+10, 1e+11, 1.5e+11, 2e+11, 2.5e+11, 3.0e+11, 3.5e+11, 4e+11)) +
  ##scale_fill_manual(name = "New Entrants Types", values = c("deepskyblue", "royalblue1"), labels = c("small", "non-small")) +
  scale_fill_manual(name = "New Entrant Type", values = c("#66CCCC", "#336666"), labels = c("Non-Graduated", "Graduated")) +
  ggtitle("Percent of Obligations for Graduated and Non-Graduated New Entrants DOD")+
  ##geom_text_repel(data = subset(FPDS_bargraphCount, registrationYear >=2014), aes(label = regpersize), size = 4, box.padding = .1, 
  ###    angle = 45) +
  ##geom_text(data = subset(FPDS_bargraphCount, registrationYear < 2014), aes(label = regpersize), size = 4, position = position_stack(vjust = .5), angle = 45)
  #geom_text_repel(data = subset(FPDS_obligationscount_grad_DOD, registrationYear <= 2016), aes(label = scales::percent(percent_obl_dec)), size = 4, position = position_stack(vjust = .3), angle = 90) +
  geom_text(data = subset(FPDS_obligationscount_grad_DOD, registrationYear <= 2016), aes(label = scales::percent(percent_obl_dec)), size = 3, position = position_dodge(width = 1), vjust = -0.5) +
  facet_wrap(~graduated_10yr, ncol=1) #+ ##ncol=1 stack them above eachother (in 1 column)
  #ylim(0, 2.1e+11) +
  #scale_y_continuous(label=unit_format(unit = "m", scale=1e-6), breaks = c(0, 5e+10, 1e+11, 1.5e+11, 2.0e+11, 2.5e+11, 3.0e+11))

graduated_v_nongrad_DoD

grid.arrange(graduated_v_nongrad_allfed, graduated_v_nongrad_DoD)


####Rollout report survival and graduation presentation graphs####

##Survivval Rates##
#AVERAGES#

#step 1: create dataframe with averages across the six samples for the varialbes:
#a. survival_3yrALL
#b. survival_5yrALL
#c. survival_10yrALL
#d. survival_3yrSMALL
#e. survival_5yrSMALL
#f. survival_10yrSMALL
#g. survival_3yrNONSMALL
#h. survival_5yrNONSMALL
#i. survival_10yrNONSMALL
##ALL FED AGENCIES##
#ALL NE#
survival_averages_allfed_ALL <- data.frame(
  group = c("3-yr", "5-yr", "10-yr"),
  value = c(0.6070481, 0.460507967, 0.2850592)
)

survival_averages_allfed_ALL

survival_averages_allfed_ALL <- survival_averages_allfed_ALL %>%
  dplyr::mutate(perc_value = value * 100) %>%
  dplyr::mutate(perc_value = round(perc_value, 0))

survival_averages_allfed_ALL

display_survival_averages_allfed_ALL <- ggplot(survival_averages_allfed_ALL, aes(x=group, y=perc_value, fill=group)) +
  scale_fill_manual(values=c("#99FFFF", "#0099CC", "#006699" )) +
  geom_bar(stat = "identity", width=0.4) +
  scale_x_discrete(limits=c("3-yr", "5-yr", "10-yr")) +
  ylim(c(0, 70)) +
  theme(text=element_text(size=18, family="Comic Sans MS")) +
  geom_text(aes(label=percent(value)), size=6, vjust=-.25) +
  labs(title = "All Federal Agencies - All New Entrants", y = "", x = "") + 
  guides(fill=FALSE)

display_survival_averages_allfed_ALL

#SMALL NE#

survival_averages_allfed_SMALL <- data.frame(
  group = c("3-yr", "5-yr", "10-yr", "Graduated"),
  value = c(0.614615017, 0.46619465, 0.287530667, 0.11)
)

head(survival_averages_allfed_SMALL)

survival_averages_allfed_SMALL <- survival_averages_allfed_SMALL %>%
  dplyr::mutate(perc_value = value * 100) %>%
  dplyr::mutate(perc_value = round(perc_value, 0))

head(survival_averages_allfed_SMALL)

display_survival_averages_allfed_SMALL <- ggplot(survival_averages_allfed_SMALL, aes(x=group, y=perc_value, fill=group)) +
  scale_fill_manual(values=c("#99FFFF", "#0099CC", "#006699", "#000066")) +
  geom_bar(stat = "identity", width=0.4) +
  scale_x_discrete(limits=c("3-yr", "5-yr", "10-yr", "Graduated")) +
  ylim(c(0, 70)) +
  theme(text=element_text(size=18, family="Comic Sans MS")) +
  geom_text(aes(label=percent(value)), size=6, vjust=-.25) +
  labs(title = "All Federal Agencies - Small New Entrants", y = "Survival Rates (percentages)", x = "") + 
  guides(fill=FALSE)

display_survival_averages_allfed_SMALL

#NON-SMALL NE#,  

survival_averages_allfed_NONSMALL <- data.frame(
  group = c("3-yr", "5-yr", "10-yr"),
  value = c(0.5883759, 0.448124933, 0.280444467)
)


survival_averages_allfed_NONSMALL

survival_averages_allfed_NONSMALL <- survival_averages_allfed_NONSMALL %>%
  dplyr::mutate(perc_value = value * 100) %>%
  dplyr::mutate(perc_value = round(perc_value, 0))

survival_averages_allfed_NONSMALL
str(survival_averages_allfed_NONSMALL)

display_survival_averages_allfed_NONSMALL <- ggplot(survival_averages_allfed_NONSMALL, aes(x=group, y=perc_value, fill=group)) +
  scale_fill_manual(values=c("#99FFFF", "#0099CC", "#006699" )) +
  geom_bar(stat = "identity", width=0.4) +
  scale_x_discrete(limits=c("3-yr", "5-yr", "10-yr")) +
  ylim(c(0, 70)) +
  theme(text=element_text(size=18, family="Comic Sans MS")) +
  geom_text(aes(label=percent(value)), size=6, vjust=-.25) +
  labs(title = "All Federal Agencies - Non-small New Entrants", y = "", x = "") + 
  guides(fill=FALSE)

display_survival_averages_allfed_NONSMALL

grid.arrange(display_survival_averages_allfed_ALL, display_survival_averages_allfed_SMALL, display_survival_averages_allfed_NONSMALL)


###DOD###
survival_averages_DOD_ALL <- data.frame(
  group = c("3-yr", "5-yr", "10-yr"),
  value = c(0.629233333, 0.482083333, 0.298833333)
)

survival_averages_DOD_ALL

survival_averages_DOD_ALL <- survival_averages_DOD_ALL %>%
  dplyr::mutate(perc_value = value * 100) %>%
  dplyr::mutate(perc_value = round(perc_value, 0))

survival_averages_DOD_ALL

display_survival_averages_DOD_ALL <- ggplot(survival_averages_DOD_ALL, aes(x=group, y=perc_value, fill=group)) +
  scale_fill_manual(values=c("#99FFFF", "#0099CC", "#006699" )) +
  geom_bar(stat = "identity", width=0.4) +
  scale_x_discrete(limits=c("3-yr", "5-yr", "10-yr")) +
  ylim(c(0, 70)) +
  theme(text=element_text(size=18, family="Comic Sans MS")) +
  geom_text(aes(label=percent(value)), size=6, vjust=-.25) +
  labs(title = "DoD - All New Entrants", y = "",  x = "") + 
  guides(fill=FALSE)

display_survival_averages_DOD_ALL

#SMALL NE#

survival_averages_DOD_SMALL <- data.frame(
  group = c("3-yr", "5-yr", "10-yr", "Graduated"),
  value = c(0.628016667, 0.4834, 0.300333333, 0.13)
)

head(survival_averages_DOD_SMALL)

survival_averages_DOD_SMALL <- survival_averages_DOD_SMALL %>%
  dplyr::mutate(perc_value = value * 100) %>%
  dplyr::mutate(perc_value = round(perc_value, 0))

head(survival_averages_DOD_SMALL)

display_survival_averages_DOD_SMALL <- ggplot(survival_averages_DOD_SMALL, aes(x=group, y=perc_value, fill=group)) +
  scale_fill_manual(values=c("#99FFFF", "#0099CC", "#006699", "#000066")) +
  geom_bar(stat = "identity", width=0.4) +
  scale_x_discrete(limits=c("3-yr", "5-yr", "10-yr", "Graduated")) +
  ylim(c(0, 70)) +
  theme(text=element_text(size=18, family="Comic Sans MS")) +
  geom_text(aes(label=percent(value)), size=6, vjust=-.25) +
  labs(title = "DoD - Small New Entrants", y = "Survival Rates (percentages)", x = "") + 
  guides(fill=FALSE)

display_survival_averages_DOD_SMALL

#NON-SMALL NE#,  

survival_averages_DOD_NONSMALL <- data.frame(
  group = c("3-yr", "5-yr", "10-yr"),
  value = c(0.634066667, 0.480816667, 0.295366667)
)


survival_averages_DOD_NONSMALL

survival_averages_DOD_NONSMALL <- survival_averages_DOD_NONSMALL %>%
  dplyr::mutate(perc_value = value * 100) %>%
  dplyr::mutate(perc_value = round(perc_value, 0))

survival_averages_DOD_NONSMALL
str(survival_averages_DOD_NONSMALL)

display_survival_averages_DOD_NONSMALL <- ggplot(survival_averages_DOD_NONSMALL, aes(x=group, y=perc_value, fill=group)) +
  scale_fill_manual(values=c("#99FFFF", "#0099CC", "#006699" )) +
  geom_bar(stat = "identity", width=0.4) +
  scale_x_discrete(limits=c("3-yr", "5-yr", "10-yr")) +
  ylim(c(0, 70)) +
  theme(text=element_text(size=18, family="Comic Sans MS")) +
  geom_text(aes(label=percent(value)), size=6, vjust=-.25) +
  labs(title = "DoD - Non-small New Entrants", y = "", x = "") + 
  guides(fill=FALSE)

display_survival_averages_DOD_NONSMALL

grid.arrange(display_survival_averages_DOD_ALL, display_survival_averages_DOD_SMALL, display_survival_averages_DOD_NONSMALL)










