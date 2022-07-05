################################################################################
# Data processing for FPDS vendor count - March 2017
################################################################################

library(magrittr)
library(tidyverse)
library(forcats)
library(csis360)

  platform_sub <- read_delim(
    "Defense_Vendor_sp_EntityCountHistoryPlatformSubCustomer.txt",
    na=c("NA","NULL"),delim="\t")
  sub_only <- read_delim(
    "Defense_Vendor_sp_EntityCountHistorySubCustomer.txt",
    na=c("NA","NULL"),delim="\t")
  platform_only <- read_delim(
    "Defense_Vendor_sp_EntityCountHistoryPlatformCustomer.txt",
    na=c("NA","NULL"),delim="\t")
  platformUAS_only <- read_delim(
    file.path("..","..","data","semi_clean","Vendor_sp_EntityCountHistoryPlatformRemoteCustomer.txt"),
    na=c("NA","NULL"),delim="\t")
  top_level <- read_delim(
    "Vendor_sp_EntityCountHistoryCustomer.txt",
    na=c("NA","NULL"),delim="\t")

  # remove unused variables
  platform_sub %<>%
    select(-Customer) %>%
    mutate(
      SumOfNumberOfActions = as.character(SumOfNumberOfActions),
      SumOfObligatedAmount = as.character(SumOfObligatedAmount)) %>%
    mutate(
      SumOfNumberOfActions = as.integer(
        ifelse(is.na(SumOfNumberOfActions), 0, SumOfNumberOfActions)),
      SumOfObligatedAmount = as.numeric(
        ifelse(is.na(SumOfObligatedAmount), 0, SumOfObligatedAmount))) %>%
    mutate(
      EntitySizeText.detail = EntitySizeText,
      EntitySizeText = fct_recode(
        EntitySizeText,
        Small = "Always Small Vendor",
        Small = "Sometimes Small Vendor",
        Medium = "Medium Vendor",
        "Large+" = "Big Five",
        "Large+" = "Large Vendor",
        "Large+" = "Large: Big 5 JV")) %>%
    group_by(
      fiscal_year, SubCustomer, PlatformPortfolio, EntitySizeText, EntitySizeText.detail,
      EntityCategory,
      AnyEntityUSplaceOfPerformance,
      AnyEntityForeignPlaceOfPerformance,
      IsEntityAbove2016constantOneMillionThreshold,
      IsEntityAbove1990constantReportingThreshold,
      IsEntityAbove2016constantReportingThreshold
      ) %>%
    summarize(
      EntityCount = sum(EntityCount), 
      AllContractorCount = sum(AllContractorCount),
      SumOfNumberOfActions = sum(SumOfNumberOfActions),
      SumOfObligatedAmount = sum(SumOfObligatedAmount)) %>%
    filter(fiscal_year >= 2000)
  
  
  
  platform_only %<>%
    select(-Customer) %>%
    rename(EntityCount = EntityCount) %>%
    mutate(
      SumOfNumberOfActions = as.character(SumOfNumberOfActions),
      SumOfObligatedAmount = as.character(SumOfObligatedAmount)) %>%
    mutate(
      SumOfNumberOfActions = as.integer(
        ifelse(is.na(SumOfNumberOfActions), 0, SumOfNumberOfActions)),
      SumOfObligatedAmount = as.numeric(
        ifelse(is.na(SumOfObligatedAmount), 0, SumOfObligatedAmount))) %>%
    mutate(
      EntitySizeText.detail = EntitySizeText,
      EntitySizeText = fct_recode(
        EntitySizeText,
        Small = "Always Small Vendor",
        Small = "Sometimes Small Vendor",
        Medium = "Medium Vendor",
        "Large+" = "Big Five",
        "Large+" = "Large Vendor",
        "Large+" = "Large: Big 5 JV")) %>%
    group_by(
      fiscal_year, PlatformPortfolio, EntitySizeText, EntitySizeText.detail,
      EntityCategory,
      AnyEntityUSplaceOfPerformance,
      AnyEntityForeignPlaceOfPerformance,
      IsEntityAbove2016constantOneMillionThreshold,
      IsEntityAbove1990constantReportingThreshold,
      IsEntityAbove2016constantReportingThreshold) %>%
    summarize(
      EntityCount = sum(EntityCount), 
      AllContractorCount = sum(AllContractorCount),
      SumOfNumberOfActions = sum(SumOfNumberOfActions),
      SumOfObligatedAmount = sum(SumOfObligatedAmount)) %>%
    filter(fiscal_year >= 2000)
                             
    
  names(sub_only)[1] <- "fiscal_year"
  
  sub_only %<>%
    select(-Customer) %>%
    mutate(
      SumOfNumberOfActions = as.character(SumOfNumberOfActions),
      SumOfObligatedAmount = as.character(SumOfObligatedAmount)) %>%
    mutate(
      SumOfNumberOfActions = as.integer(
        ifelse(is.na(SumOfNumberOfActions), 0, SumOfNumberOfActions)),
      SumOfObligatedAmount = as.numeric(
        ifelse(is.na(SumOfObligatedAmount), 0, SumOfObligatedAmount))) %>%
    mutate(
      EntitySizeText.detail = EntitySizeText,
      EntitySizeText = fct_recode(
        EntitySizeText,
        Small = "Always Small Vendor",
        Small = "Sometimes Small Vendor",
        Medium = "Medium Vendor",
        "Large+" = "Big Five",
        "Large+" = "Large Vendor",
        "Large+" = "Large: Big 5 JV")) %>%
    group_by(
      fiscal_year, SubCustomer, EntitySizeText, EntitySizeText.detail,
      EntityCategory,
      AnyEntityUSplaceOfPerformance,
      AnyEntityForeignPlaceOfPerformance,
      IsEntityAbove1990constantReportingThreshold,
      IsEntityAbove2016constantReportingThreshold) %>%
    summarize(
      EntityCount = sum(EntityCount), 
      AllContractorCount = sum(AllContractorCount),
      SumOfNumberOfActions = sum(SumOfNumberOfActions),
      SumOfObligatedAmount = sum(SumOfObligatedAmount)) %>%
    filter(fiscal_year >= 2000)
  
  names(top_level)[1] <- "fiscal_year"
  
  top_level %<>%
    filter(Customer == "Defense") %>%
    rename(EntityCount = EntityCount) %>%
    select(-Customer) %>%
    mutate(
      SumOfNumberOfActions = as.character(SumOfNumberOfActions), 
      SumOfObligatedAmount = as.character(SumOfObligatedAmount)) %>%
    mutate(
      SumOfNumberOfActions = as.integer(
        ifelse(is.na(SumOfNumberOfActions), 0, SumOfNumberOfActions)),
      SumOfObligatedAmount = as.numeric(
        ifelse(is.na(SumOfObligatedAmount), 0, SumOfObligatedAmount))) %>%
    mutate(
      EntitySizeText.detail = EntitySizeText,
      EntitySizeText = fct_recode(
        EntitySizeText,
        Small = "Always Small Vendor",
        Small = "Sometimes Small Vendor",
        Medium = "Medium Vendor",
        "Large+" = "Big Five",
        "Large+" = "Large Vendor",
        "Large+" =  "Large: Big 5 JV")) %>%
    group_by(
      fiscal_year, EntitySizeText, EntitySizeText.detail,
      EntityCategory,
      AnyEntityUSplaceOfPerformance,
      AnyEntityForeignPlaceOfPerformance,
      IsEntityAbove2016constantOneMillionThreshold,
      IsEntityAbove1990constantReportingThreshold,
      IsEntityAbove2016constantReportingThreshold) %>%
    summarize(
      EntityCount = sum(EntityCount), 
      AllContractorCount = sum(AllContractorCount),
      SumOfNumberOfActions = sum(SumOfNumberOfActions),
      SumOfObligatedAmount = sum(SumOfObligatedAmount)) %>%
    filter(fiscal_year >= 2000)
  
  
  
  deflate <- c(
    "2000" = 0.7057,
    "2001" = 0.7226,
    "2002" = 0.7343,
    "2003" = 0.7483,
    "2004" = 0.7668,
    "2005" = 0.7909,
    "2006" = 0.8166,
    "2007" = 0.8388,
    "2008" = 0.8562,
    "2009" = 0.8662,
    "2010" = 0.8738,
    "2011" = 0.8916,
    "2012" = 0.9078,
    "2013" = 0.9232,
    "2014" = 0.9401,
    "2015" = 0.9511,
    "2016" = 0.9625,
    "2017" = 0.9802,
    "2018" = 1.0000,
    "2019" = 1.0199,
    "2020" = 1.0404,
    "2021" = 1.0612,
    "2022" = 1.0824)
 
  
  sub_only$fiscal_year <- as.character(sub_only$fiscal_year)
  platform_only$fiscal_year <- as.character(platform_only$fiscal_year)
  top_level$fiscal_year <- as.character(top_level$fiscal_year)
  platform_sub$fiscal_year <- as.character(platform_sub$fiscal_year)
  
sub_only$SumOfObligatedAmount <- as.numeric(sub_only$SumOfObligatedAmount /
                           deflate[sub_only$fiscal_year])
platform_only$SumOfObligatedAmount <- round(platform_only$SumOfObligatedAmount /
                           deflate[platform_only$fiscal_year])
platform_sub$SumOfObligatedAmount <- round(platform_sub$SumOfObligatedAmount /
                           deflate[platform_sub$fiscal_year])
top_level$SumOfObligatedAmount <- round(top_level$SumOfObligatedAmount /
                           deflate[top_level$fiscal_year])

sub_only$fiscal_year <- as.numeric(sub_only$fiscal_year)
platform_only$fiscal_year <- as.numeric(platform_only$fiscal_year)
top_level$fiscal_year <- as.numeric(top_level$fiscal_year)
platform_sub$fiscal_year <- as.numeric(platform_sub$fiscal_year)
  

  # write_csv(platform_only, "platform_only.csv")
  # write_csv(sub_only, "sub_only.csv")
  # write_csv(platform_sub, "platform_sub.csv")
  # write_csv(top_level, "top_level.csv")

prepare_vendor<-function(data)
  {
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



labels_and_colors<-csis360::prepare_labels_and_colors(platform_sub)
column_key<-csis360::get_column_key(platform_sub)

  # write output to CleanedVendorSize.csv
  save(platform_only,sub_only,platform_sub,top_level, column_key, labels_and_colors, file="2016_vendor_count.Rda")  #Shiny Apps//FPDS vendor count//