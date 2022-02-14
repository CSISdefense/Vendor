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
library(readr)



source("scripts\\NAICS.r")
# read in data
# setwd("K:/Users/Greg/Repositories/Vendor")
c2<-read_delim(file.path("data","semi_clean","Defense_Summary.SP_C2detail.txt"),delim="\t",na=c("NULL","NA"),
                     col_names = TRUE, guess_max = 700000)

colnames(c2)<-trimws(colnames(c2))
c2entity<-c2 %>% group_by(EntityText,EntityID)%>% summarise(SumOfobligatedAmount=sum(text_to_number(SumOfobligatedAmount)),
                                                  SumOfnumberOfActions=sum(SumOfnumberOfActions))
write.csv(c2entity,file="c2entity.csv",row.names = FALSE)

c2_sum<-read_delim(file.path("data","semi_clean","c2_summary_veh.txt"),delim="\t",na=c("NULL","NA"),
                     col_names = TRUE, guess_max = 700000)


summary(factor(c2_gsa$CSISidvpiidid[c2_gsa$idvpiid==""]))

c2_gsa$idvpiid[is.na(c2_gsa$idvpiid)]<-""
c2_gsa$idv_or_piid[c2_gsa$idvpiid!=""]<-paste("IDV",c2_gsa$idvpiid[c2_gsa$idvpiid!=""],"_")
c2_gsa$idv_or_piid[c2_gsa$idvpiid==""]<-paste("AWD",c2_gsa$piid[c2_gsa$idvpiid==""],"_")

idv_spend<-c2_gsa %>% group_by(idv_or_piid,VehicleClassification) %>%
  summarise(obligatedAmount=sum(obligatedAmount))

idv_spend[order(desc(idv_spend$obligatedAmount)),]




c2_sum<-read_delim(file.path("data","semi_clean","c2_summary2.txt"),delim="\t",na=c("NULL","NA"),
                    col_names = TRUE, guess_max = 700000)



c2_sum<-apply_standard_lookups(c2_sum)
# c2_sum <- deflate(c2_sum,money_var="obligatedAmount",fy_var="Fiscal_Year",deflator_var="OMB20_GDP20")
# c2_sum$NAICS_ShortHand

c2_sum<-csis360::read_and_join(c2_sum,
                           "LOOKUP_Buckets.csv",
                           # by="ProductOrServiceArea",
                           by="ProductServiceOrRnDarea",
                           replace_na_var="ProductServiceOrRnDarea",
                           add_var=c("ServicesCategory.detail"),
                           path="https://raw.githubusercontent.com/CSISdefense/R-scripts-and-data/master/",
                           dir="Lookups/"
)

c2_sum %>% group_by(informationtechnologycommercialitemcategory) %>% filter(Fiscal_Year>="2000") %>%
  # group_by(ProductOrServiceCode,ProductOrServiceCodeText,principalnaicscode,NAICS_ShortHand,ContractingOfficeID,ContractingOfficeName) %>%
  summarise(Action_Obligation_OMB20_GDP20=sum(Action_Obligation_OMB20_GDP20))
# levels(factor(c2_sum$informationtechnologycommercialitemcategory))


ds_lc<-prepare_labels_and_colors(c2_sum)
ds_ck<-get_column_key(c2_sum)

save(c2_sum,file=file.path("data","semi_clean","c2.Rda"))




summary(factor(c2_gsa$contractingofficeagencyid))
c2<-c2_gsa %>% filter(contractingofficeagencyid=='97AK')
c2$idvpiid[is.na(c2$idvpiid)]<-""
c2$idv_or_piid[c2$idvpiid!=""]<-paste("IDV",c2$idvpiid[c2$idvpiid!=""],"_")
c2$idv_or_piid[c2$idvpiid==""]<-paste("AWD",c2$piid[c2$idvpiid==""],"_")
summary(factor(c2$ContractingOfficeName))
summary(factor(c2$ContractingOfficeID))
View(c2 %>%filter(is.na(ContractingOfficeName)))




(v<-build_plot(c2 %>% filter(Fiscal_Year>=2005),
           chart_geom = "Bar Chart",
           share=FALSE,
           x_var="Fiscal_Year",
           y_var="obligatedAmount",
           color_var = "VehicleClassification",
           facet_var = "ContractingOfficeID",
           format=TRUE
           )
)

c2 <- deflate(c2,money_var="obligatedAmount",fy_var="Fiscal_Year",deflator_var="OMB20_GDP20")



View(c2 %>% group_by(Fiscal_Year) %>% summarise(Action_Obligation_OMB20_GDP20=sum(Action_Obligation_OMB20_GDP20)))
View(c2_sum %>% group_by(Fiscal_Year) %>% summarise(Action_Obligation_OMB20_GDP20=sum(Action_Obligation_OMB20_GDP20)))

idv_spend_test<-c2 %>% filter(Fiscal_Year>=2005) %>% group_by(idv_or_piid,VehicleClassification,ContractingOfficeID,ContractingOfficeName,fundingrequestingagencyid,
                                                                ) %>%
  summarise(Action_Obligation_OMB20_GDP20=sum(Action_Obligation_OMB20_GDP20))


idv_spend<-c2 %>% filter(Fiscal_Year>=2005) %>% group_by(idv_or_piid,VehicleClassification,ContractingOfficeID,ContractingOfficeName) %>%
  summarise(Action_Obligation_OMB20_GDP20=sum(Action_Obligation_OMB20_GDP20),
            min_fyear=min(Fiscal_Year),
            max_fyear=max(Fiscal_Year))

idv_spend <-  idv_spend %>% group_by(ContractingOfficeID) %>%
  mutate(crank=order(order(Action_Obligation_OMB20_GDP20,decreasing = TRUE)),
         cshare=Action_Obligation_OMB20_GDP20/sum(Action_Obligation_OMB20_GDP20))

View(idv_spend %>% filter(crank<=5))

idv_spend %>% filter(crank<=5) %>% group_by(ContractingOfficeID) %>%
  dplyr::summarise(cshare=sum(cshare),
           Action_Obligation_OMB20_GDP20=sum(Action_Obligation_OMB20_GDP20))

idv_spend %>% filter(crank<=10) %>% group_by(ContractingOfficeID) %>%
  dplyr::summarise(cshare=sum(cshare),
                   Action_Obligation_OMB20_GDP20=sum(Action_Obligation_OMB20_GDP20))

idv_spend<-idv_spend[order(idv_spend$ContractingOfficeID,desc(idv_spend$Action_Obligation_OMB20_GDP20)),]
View(idv_spend %>% filter(crank<=5))
write.csv(idv_spend %>% filter(crank<=5), file="c2_top_office_contract.csv",row.names = FALSE)


idv_spend_2020<-c2 %>% filter(Fiscal_Year>=2010) %>% group_by(idv_or_piid,VehicleClassification,ContractingOfficeID) %>%
  summarise(Action_Obligation_OMB20_GDP20=sum(Action_Obligation_OMB20_GDP20)) %>%  group_by(ContractingOfficeID) %>%
  mutate(crank=order(order(Action_Obligation_OMB20_GDP20,decreasing = TRUE)),
         cshare=Action_Obligation_OMB20_GDP20/sum(Action_Obligation_OMB20_GDP20)) %>% filter(crank<=5) %>% group_by(ContractingOfficeID) %>%
  dplyr::summarise(cshare=sum(cshare),
                   Action_Obligation_OMB20_GDP20=sum(Action_Obligation_OMB20_GDP20))





