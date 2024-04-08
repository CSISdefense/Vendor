################################################################################
# Data processing for FPDS vendor count - March 2017
################################################################################

library(magrittr)
library(tidyverse)
library(forcats)
library(csis360)

  platform_sub <- read_delim(
    file.path("data","semi_clean","Defense_Vendor_sp_EntityCountHistoryPlatformSubCustomer.txt"),
    na=c("NA","NULL"),delim="\t")
  sub_only <- read_delim(
    file.path("data","semi_clean","Defense_Vendor_sp_EntityCountHistorySubCustomer.txt"),
    na=c("NA","NULL"),delim="\t")
  platform_only <- read_delim(
    file.path("data","semi_clean","Defense_Vendor_sp_EntityCountHistoryPlatformCustomer.txt"),
    na=c("NA","NULL"),delim="\t")
  platformUAS_only <- read_delim(
    file.path("data","semi_clean","Vendor_sp_EntityCountHistoryPlatformRemoteCustomer.txt"),
    na=c("NA","NULL"),delim="\t")
  top_level <- read_delim(
    file.path("data","semi_clean","Vendor_sp_EntityCountHistoryCustomer.txt"),
    na=c("NA","NULL"),delim="\t")

  
  
  # standardize_count<-function(c){
  #   c<-standardize_variable_names(c)
  #   c%<>%
  #     # select(-Customer) %>%
  #     mutate(
  #       NumberOfActions = text_to_number(NumberOfActions),
  #       Action_Obligation = text_to_number(Action_Obligation))%>%
  #     mutate(
  #       EntitySizeText.detail = EntitySizeText,
  #       EntitySizeText = fct_recode(
  #         EntitySizeText,
  #         Small = "Always Small Vendor",
  #         Small = "Sometimes Small Vendor",
  #         Medium = "Medium Vendor",
  #         "Large+" = "Big Five",
  #         "Large+" = "Large Vendor",
  #         "Large+" = "Large: Big 5 JV"))%>%
  #     filter(Fiscal_Year >= 2000)
  #   c
  # }

  key<-c("Fiscal_Year", "EntitySizeCode",
         "EntityCategory",
         "AnyEntityUSplaceOfPerformance",
         "AnyEntityForeignPlaceOfPerformance",
         "IsEntityAbove2016constantOneMillionThreshold",
         "IsEntityAbove1990constantReportingThreshold",
         "IsEntityAbove2016constantReportingThreshold"
         )
  platform_sub<-apply_standard_lookups(platform_sub)
  check_key(platform_sub,c("SubCustomer", "PlatformPortfolio",key))
  platform_only<-apply_standard_lookups(platform_only)
  # platform_only<-standardize_count(platform_only %>% filter(Customer=="Defense"))
  check_key(platform_only,c("PlatformPortfolio","IsEntityAbove2018constant10ThousandThreshold",key))
    # write.csv(all_duplicate(platform_only,c("PlatformPortfolio","IsEntityAbove2018constant10ThousandThreshold",key)),"dupe.csv")
  platformUAS_only<-apply_standard_lookups(platformUAS_only)
  check_key(platformUAS_only,c("PlatformPortfolioRemote","IsEntityAbove2018constant10ThousandThreshold",key))
  sub_only<-apply_standard_lookups(sub_only)
  check_key(sub_only,c("SubCustomer",key))
  
  top_level<-apply_standard_lookups(top_level)
  check_key(top_level,c(key,"Customer","IsEntityAbove2018constant10ThousandThreshold"))
  

prepare_vendor<-function(data)
  {
  data <- data %>%filter(Fiscal_Year >= 2000)
  #Add this to replace NAs function
  if("SubCustomer" %in% colnames(data)){
    if(!is.factor(data$SubCustomer))
      data$SubCustomer<-factor(data$SubCustomer)
    data<-replace_nas_with_unlabeled(data,"SubCustomer","Uncategorized")
  }
  if("PlatformPortfolio" %in% colnames(data)){
    if(!is.factor(data$PlatformPortfolio))
      data$PlatformPortfolio<-factor(data$PlatformPortfolio)
    data<-replace_nas_with_unlabeled(data,"PlatformPortfolio")
  }
  if("PlatformPortfolioRemote" %in% colnames(data)){
    if(!is.factor(data$PlatformPortfolioRemote))
      data$PlatformPortfolioRemote<-factor(data$PlatformPortfolioRemote)
    data<-replace_nas_with_unlabeled(data,"PlatformPortfolioRemote")
  }
  data
  if("EntitySizeText" %in% colnames(data)){
    if(!is.factor(data$EntitySizeText))
      data$EntitySizeText<-factor(data$EntitySizeText)
    data<-replace_nas_with_unlabeled(data,"EntitySizeText","Unlabeled Vendor")
  }
  if("EntitySizeText.detail" %in% colnames(data)){
    if(!is.factor(data$EntitySizeText.detail))
      data$EntitySizeText.detail<-factor(data$EntitySizeText.detail)
    data<-replace_nas_with_unlabeled(data,"EntitySizeText.detail","Unlabeled Vendor")
  }
  if("AnyEntityUSplaceOfPerformance" %in% colnames(data)){
    if(!is.factor(data$AnyEntityUSplaceOfPerformance))
      data$AnyEntityUSplaceOfPerformance<-factor(data$AnyEntityUSplaceOfPerformance)
    data<-replace_nas_with_unlabeled(data,"AnyEntityUSplaceOfPerformance")
  }
  if("AnyEntityForeignPlaceOfPerformance" %in% colnames(data)){
    if(!is.factor(data$AnyEntityForeignPlaceOfPerformance))
      data$AnyEntityForeignPlaceOfPerformance<-factor(data$AnyEntityForeignPlaceOfPerformance)
    data<-replace_nas_with_unlabeled(data,"AnyEntityForeignPlaceOfPerformance")
  }
  if("IsEntityAbove1990constantReportingThreshold" %in% colnames(data)){
    if(!is.factor(data$IsEntityAbove1990constantReportingThreshold))
      data$IsEntityAbove1990constantReportingThreshold<-factor(data$IsEntityAbove1990constantReportingThreshold)
    data<-replace_nas_with_unlabeled(data,"IsEntityAbove1990constantReportingThreshold")
  }
  if("IsEntityAbove2016constantReportingThreshold" %in% colnames(data)){
    if(!is.factor(data$IsEntityAbove2016constantReportingThreshold))
      data$IsEntityAbove2016constantReportingThreshold<-factor(data$IsEntityAbove2016constantReportingThreshold)
    data<-replace_nas_with_unlabeled(data,"IsEntityAbove2016constantReportingThreshold")
  }
  if("IsEntityAbove2016constantOneMillionThreshold" %in% colnames(data)){
    if(!is.factor(data$IsEntityAbove2016constantOneMillionThreshold))
      data$IsEntityAbove2016constantOneMillionThreshold<-factor(data$IsEntityAbove2016constantOneMillionThreshold)
    data<-replace_nas_with_unlabeled(data,"IsEntityAbove2016constantOneMillionThreshold")
  }
  
  if("EntityCategory" %in% colnames(data)){
    if(!is.factor(data$EntityCategory))
      data$EntityCategory<-factor(data$EntityCategory)
    data<-replace_nas_with_unlabeled(data,"EntityCategory")
  }
  
  
  data
}

platform_sub<-prepare_vendor(platform_sub)
top_level<-prepare_vendor(top_level)
sub_only<-prepare_vendor(sub_only)
platform_only<-prepare_vendor(platform_only)
platformUAS_only<-prepare_vendor(platformUAS_only)




labels_and_colors<-prepare_labels_and_colors(platform_sub)
column_key<-get_column_key(platform_sub)

write_csv(platform_only, file.path("analysis","FPDS_vendor_count","platform_only.csv"))
write_csv(sub_only, file.path("analysis","FPDS_vendor_count","sub_only.csv"))
write_csv(platform_sub, file.path("analysis","FPDS_vendor_count","platform_sub.csv"))
write_csv(platformUAS_only, file.path("analysis","FPDS_vendor_count","platformUAS_only.csv"))
write_csv(top_level, file.path("analysis","FPDS_vendor_count","top_level.csv"))

colnames(platformUAS_only)[colnames(platformUAS_only)=="EntitySizeSum"]<-"EntitySizeText.sum"
uav_lc<-prepare_labels_and_colors(platformUAS_only)
uav_ck<-get_column_key(platformUAS_only)

# write output to CleanedVendorSize.csv
save(platformUAS_only, uav_lc, uav_ck, 
     file=file.path("data","clean","uav_vendor_count.Rda"))  #Shiny Apps//FPDS vendor count//

  save(platform_only,sub_only,platform_sub,top_level,platformUAS_only, column_key, labels_and_colors, 
       file=file.path("analysis","FPDS_vendor_count","vendor_count.Rda"))  #Shiny Apps//FPDS vendor count//