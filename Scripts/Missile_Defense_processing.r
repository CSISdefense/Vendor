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
f<-read_delim(file.path("data","semi_clean","Federal_ProdservPlatform.txt"),delim="\t",na=c("NULL","NA"),
              col_names = TRUE, guess_max = 10000000)

summary(factor(d$ProductOrServiceArea))
summary(factor(d$ContractingAgencyText))
summary(w)

f<-standardize_variable_names(f)
colnames(f)[colnames(f)=="platformportfolio"]<-"PlatformPortfolio"

#Limiting to just Missiles and Space / Launcher and Munitions or MDA
# summary(factor(f$PlatformPortfolio) %in% c("Ordnance and Missiles", ))
wf <- f %>% filter(PlatformPortfolio %in% c("Ordnance and Missiles","Space Systems","Missile Defense") |
                     ProductServiceOrRnDarea %in% c("Launchers & Munitions","Missiles & Space" )) %>%
  filter(Fiscal.Year>=1990) #Note, federal data starts at 2000
  # select(-ServicesCategory,-IsService,-ProductOrServiceArea,-productorservicecode)


w <- d %>% filter(ProductServiceOrRnDarea %in% c("Launchers & Munitions","Missiles & Space" ) | ContractingAgencyText=="MISSILE DEFENSE AGENCY (MDA)") %>%
  filter(Fiscal.Year>=1990) %>% #We don't trust the completeness of DD350 data before 1990
  select(-ServicesCategory,-IsService,-ProductOrServiceArea,-productorservicecode)


wf$MDAcat <- ifelse(wf$ProductServiceOrRnDarea %in% c("Launchers & Munitions","Missiles & Space" ),
                   "Launcher, Munition, Missile, or Space Product","Other via Platform Portfolio")

w_wide <- w[order(w$Fiscal.Year),] %>%  pivot_wider(names_from="Fiscal.Year",values_from = "Action_Obligation")

w_psc<- w %>% group_by(Simple,ProductServiceOrRnDarea,ProductOrServiceCode,ProductOrServiceCodeText,MDAcat,Fiscal.Year) %>% 
  summarise(Action_Obligation=sum(Action_Obligation))

w_psc_wide<-w_psc[order(w_psc$Fiscal.Year),] %>%  pivot_wider(names_from="Fiscal.Year",values_from = "Action_Obligation")
w_psc_wide<-w_psc_wide[order(w_psc_wide$ProductOrServiceCode),]



wf_wide <- wf[order(wf$Fiscal.Year),] %>%  pivot_wider(names_from="Fiscal.Year",values_from = "Action_Obligation")

wf_psc<- wf %>% group_by(SimpleArea,ProductServiceOrRnDarea,ProductOrServiceCode,ProductOrServiceCodeText,MDAcat,Fiscal.Year) %>% 
  summarise(Action_Obligation=sum(Action_Obligation))

wf_psc_wide<-wf_psc[order(wf_psc$Fiscal.Year),] %>%  pivot_wider(names_from="Fiscal.Year",values_from = "Action_Obligation")
wf_psc_wide<-wf_psc_wide[order(wf_psc_wide$ProductOrServiceCode),]




wf_prog<- wf %>% group_by(ProjectAbbreviation,ProjectName,ProjectPlatform,Fiscal.Year) %>% 
  summarise(Action_Obligation=sum(Action_Obligation))
wf_prog_wide <- wf_prog[order(wf_prog$Fiscal.Year),] %>%  pivot_wider(names_from="Fiscal.Year",values_from = "Action_Obligation")
wf_prog_wide<-wf_prog_wide[order(wf_prog_wide$ProjectAbbreviation),]

write.csv(wf_psc_wide, file=file.path("Output","mdp_prodserv_wide.csv"),row.names = FALSE,na="")
write.csv(wf_psc, file=file.path("Output","mdp_prodserv.csv"),row.names = FALSE,na="")
write.csv(wf_prog_wide, file=file.path("Output","mdp_program_wide.csv"),row.names = FALSE,na="")
write.csv(wf_prog, file=file.path("Output","mdp_program.csv"),row.names = FALSE,na="")
write.csv(wf, file=file.path("Output","mdp_all_categorization.csv"),row.names = FALSE,na="")
write.csv(wf_wide, file=file.path("Output","mdp_all_categorization_wide.csv"),row.names = FALSE,na="")