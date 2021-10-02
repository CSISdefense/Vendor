################################################################################
# Data Pre-Processing for Vendor Size Shiny Graphic
# UPDATED 2021/03/07
#
# This script does pre-processing to get a SQL query into usable form for shiny
# graphics
#
# Input:
#   CSV-format results from SQL query:
#     Vendor_SP_CompetitionVendorSizeHistoryBucketPlatformSubCustomer
#
# Output: CSV file (unaggregated_FPDS.Rda)
# with data in the minimal form needed by Shiny script
################################################################################

# install.packages("../csis360_0.0.0.9022.tar.gz")

library(tidyverse)
library(magrittr)
library(csis360)
library(Hmisc)
library(readr)

source("scripts\\NAICS.r")
# read in data


d<-read_delim(file.path("data","semi_clean","Summary_ProductOrServiceCodeAgency.txt"),delim="\t",na=c("NULL","NA"),
                     col_names = TRUE, guess_max = 10000000)
f<-read_delim(file.path("data","semi_clean","Federal.Summary_ProductOrServiceCodeAgency.txt"),delim="\t",na=c("NULL","NA"),
              col_names = TRUE, guess_max = 10000000)
f<-read_delim(file.path("data","semi_clean","Federal_Summary.ProductOrServiceCodePlatformAgency"),delim="\t",na=c("NULL","NA"),
              col_names = TRUE, guess_max = 10000000)

summary(factor(d$ProductOrServiceArea))
summary(factor(d$ContractingAgencyText))
summary(w)

#Limiting to just Missiles and Space / Launcher and Munitions or MDA
summary(factor(f$PlatformPortfolio) %in% c("Ordnance and Missiles", ))
wf <- f %>% filter(PlatformPortfolio %in% c("Ordnance and Missiles","Space Systems","Missile Defense")) %>%
  filter(fiscal_year>=1990) #Note, federal data starts at 2000
  # select(-ServicesCategory,-IsService,-ProductOrServiceArea,-productorservicecode)


w <- d %>% filter(ProductServiceOrRnDarea %in% c("Launchers & Munitions","Missiles & Space" ) | ContractingAgencyText=="MISSILE DEFENSE AGENCY (MDA)") %>%
  filter(fiscal_year>=1990) %>% #We don't trust the completeness of DD350 data before 1990
  select(-ServicesCategory,-IsService,-ProductOrServiceArea,-productorservicecode)


wf$MDAcat <- ifelse(w$ProductServiceOrRnDarea %in% c("Launchers & Munitions","Missiles & Space" ),
                   "Launcher, Munition, Missile, Space","Other MDA product or service")

w_wide <- w[order(w$fiscal_year),] %>%  pivot_wider(names_from="fiscal_year",values_from = "obligatedamount")

w_psc<- w %>% group_by(Simple,ProductServiceOrRnDarea,ProductOrServiceCode,ProductOrServiceCodeText,MDAcat,fiscal_year) %>% 
  summarise(obligatedamount=sum(obligatedamount))

w_psc_wide<-w_psc[order(w_psc$fiscal_year),] %>%  pivot_wider(names_from="fiscal_year",values_from = "obligatedamount")
w_psc_wide<-w_psc_wide[order(w_psc_wide$ProductOrServiceCode),]

write.csv(w_psc_wide, file=file.path("Output","mdp_prodserv_wide.csv"),row.names = FALSE)