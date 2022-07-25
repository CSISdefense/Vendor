###############################################################
# Get Top 10 rows of aggregated Amount and # of action
################################################################
library(dplyr)
library(tidyr)
library(csis360)
library(readr)

data <- read.csv("data/semi_clean/Vendor.SP_TopVendorHistoryBucketSimple.csv",header = TRUE,
                 na.strings=c("NA","NULL", " "),
                 fileEncoding="UTF-8-BOM")
data<-standardize_variable_names(data)

dataplat <- read_delim("data/semi_clean/Vendor.SP_TopVendorHistoryPlatformUAVisDefense.txt",#header = TRUE,
                 na=c("NA","NULL", " "),
                 delim="\t"
                 ) #fileEncoding="UTF-8-BOM"
dataplat<-remove_bom(dataplat)

dataplat<-dataplat %>% filter(fiscal_year!="An error occurred while executing batch. Error message is: One or more errors occurred.\r")
dataplat$fiscal_year[nrow(dataplat)]


dataplat<-dataplat[,colnames(dataplat)!="WarningFlag...11"]
colnames(dataplat)[colnames(dataplat)=="WarningFlag...7"]<-"WarningFlag"
colnames(dataplat)[colnames(dataplat)=="PlatformPortfolioRemote"]<-"PlatformPortfolioUAV"


dataplat<-standardize_variable_names(dataplat)
# 
# dataplat$IsDefense <- NA
# dataplat$IsDefense[dataplat$Customer=="Defense"]<-TRUE
# dataplat$IsDefense[dataplat$Customer!="Defense"]<-FALSE
# summary(dataplat$IsDefense)
# dataplat <- dataplat %>%
#   group_by(Fiscal_Year,ContractorDisplayName,
#            UnknownCompany,AllContractor,
#            jointventure,WarningFlag,PlatformPortfolioUAV,
#            IsDefense
#            ) %>%
#   summarize(Action_Obligation = sum(Action_Obligation),
#             NumberOfActions = sum(NumberOfActions))

dataplat<-dataplat%>% group_by(Fiscal_Year,IsDefense,PlatformPortfolioUAV,
                     ) %>%
  dplyr::mutate(pos=rank(-Action_Obligation))%>%
  arrange(Fiscal_Year,IsDefense,PlatformPortfolioUAV,pos)

save(file="data/semi_clean/TopVendorUAVHistoryPlatformCustomer.rda",dataplat)
colnames(dataplat)


View(dataplat %>% filter(PlatformPortfolioUAV=="Remotely Operated" & pos<=10 & IsDefense==TRUE & Fiscal_Year>=2000))
summary(factor(dataplat$PlatformPortfolioUAV))



# Asign the column class
data[9:10] = as.numeric(unlist(data[9:10]))
# Aggregation
mydata <- data %>%
  group_by(Fiscal_Year,Simple,ContractorDisplayName) %>%
  summarize(SumOfobligatedAmount = sum(SumOfobligatedAmount),
            SumOfnumberOfActions = sum(SumOfnumberOfActions))

# Split the data into 2 part
# Contratcor with out NA
mydata1 <- filter(mydata, !is.na(ContractorDisplayName))

# Unnamed Contractor
mydata2 <- filter(mydata, is.na(ContractorDisplayName))
mydata2$rank <- 11
mydata2$Top10 <- 0

# Rank the contractor with out NA value
sorted <- mydata1 %>% 
  arrange(Fiscal_Year,Simple,-SumOfobligatedAmount) %>%
  group_by(Fiscal_Year,Simple) %>%
  mutate(rank=row_number())
# Add a column to split the Top 1o with others
df <- sorted %>% mutate(Top10 = ifelse(rank %in% 1:10, 1, 0))
# Split data into 2 parts
# Top10
df1 <- filter(df, Top10 == 1)
# Not Top10
df2 <- filter(df, Top10 == 0)
# Combine notTOP10 data with the contractor NA data, and make aggregration
df2 <- bind_rows(mydata2,df2)
df2 <- df2 %>%
  group_by(Fiscal_Year,Simple) %>%
  summarize(SumOfobligatedAmount = sum(SumOfobligatedAmount),
            SumOfnumberOfActions = sum(SumOfnumberOfActions))
# dataframe manipulation for the binding in next step
df2$ContractorDisplayName <- "all other"
df2$rank <- 11
df2 <- df2[,c(1,2,5,3,4,6)]
df1 <- df1[-7]

# Combine two data: TOP 10 and all the rest
fulldata <- bind_rows(df1,df2)
# Reorder df
fulldata <- arrange(fulldata,Fiscal_Year,Simple,rank)

fulldata <- within(fulldata,rank[rank == 11]<- "other")

#fulldata <- fulldata %>%
#    replace_na(Simple = "Other")


write_csv(fulldata, "K:/R-Shiny/Interns/Zhian/Vendor/Top_10_v6.csv")
