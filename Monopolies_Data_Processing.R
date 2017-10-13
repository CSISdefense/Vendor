#=================================================================================================================#
# Data processing for Monopolies Data
#=================================================================================================================#
rm(list = ls())
library(tidyverse)
library(csis360)

  
# Path<-"C:\\Users\\gsand_000.ALPHONSE\\Documents\\Development\\R-scripts-and-data\\"
Path<-"K:\\2007-01 PROFESSIONAL SERVICES\\R scripts and data\\"
source(paste(Path,"lookups.r",sep=""))


file<-unz("Data\\Defense_Vendor_EntityIDhistoryNAICS.zip",
          filename="Defense_Vendor_EntityIDhistoryNAICS.txt")
# defense_naics_vendor <- read_tsv(file,
#                           col_names = TRUE,
#                           na = c("","NA","NULL"))

#Import Defense vendor list by NAICS.
defense_naics_vendor <- read.table(file,
                           header = TRUE,
                           na.strings = c("","NA","NULL"),
                           quote="\"",#Necessary because there are some 's in the names.
                           sep = "\t")

defense_naics_vendor<-apply_lookups(Path,defense_naics_vendor)

#Import Defense Vendor list.
file<-unz("Data\\Defense_Vendor_EntityIDhistory.zip",
          filename="Defense_Vendor_EntityIDhistory.txt")

defense_vendor <- read.table(file,
                                   header = TRUE,
                                   na.strings = c("","NA","NULL"),
                                   quote="\"",#Necessary because there are some 's in the names.
                                   sep = "\t")

defense_vendor<-apply_lookups(Path,defense_vendor)

save(defense_naics_vendor,defense_vendor,file="defense_naics_vendor.Rdata")




names<-unique(defense_vendor$ParentID)


names[toupper(names) %in% toupper(c(
"MDA",  #Check
"DigitalGlobe", #Check
"United Tech", #Check
"Rockwell Collins",  #Check
"Northrop Grumman",  #Check
"Orbital Sciences",
"Boeing", #Check
"Aurora Flight Sciences" #Check but redownload
))]



defense_vendor<-csis360::read_and_join(defense_vendor,
  "Lookup_ParentID.csv",
  path="https://raw.githubusercontent.com/CSISdefense/Lookup-Tables/master/",
  by="ParentID",
  directory="vendor//",
  new_var_checked=FALSE,
  add_var="Abbreviation"
  # NA.check.columns="Fair.Competed"
)

# Filter defense_naics_vendor to only include fical year ranging from 2008 to 2016

defense_vendor_2016 <- filter(defense_vendor,Fiscal.Year >= as.Date("2016-01-01"))

defense_vendor_2016<-ddply(defense_vendor_2016,
                           .(Fiscal.Year),
                           transform, 
                           pos = rank(-Action.Obligation,
                                               ties.method ="min"))

key_vendor_2016<-subset(defense_vendor_2016,pos<=10 |
         toupper(ParentID) %in% toupper(c("MDA",  #Check
       "DigitalGlobe", #Check
       "United Tech", #Check
       "Rockwell Collins",  #Check
       "Northrop Grumman",  #Check
       "Orbital Sciences",
       "Boeing", #Check
       "Aurora Flight Sciences" #Check but redownload
)))

updated_vendor<-key_vendor_2016
key_vendor_2016$Sample<-"Original"
updated_vendor$Sample<-"Merged"
updated_vendor<-rbind(key_vendor_2016,updated_vendor)
updated_vendor$VendorName<-updated_vendor$ParentID
updated_vendor$VendorName[toupper(updated_vendor$ParentID)==
                            toupper("DigitalGlobe")&
                            updated_vendor$Sample=="Merged"]<-"MDA"
updated_vendor$VendorName[toupper(updated_vendor$ParentID)==
                            toupper("Rockwell Collins")&
                            updated_vendor$Sample=="Merged"]<-"UNITED TECH"
updated_vendor$VendorName[toupper(updated_vendor$ParentID)==
                            toupper("Orbital Sciences")&
                            updated_vendor$Sample=="Merged"]<-"NORTHROP GRUMMAN"
updated_vendor$VendorName[toupper(updated_vendor$ParentID)==
                            toupper("Aurora Flight Sciences")&
                            updated_vendor$Sample=="Merged"]<-"BOEING"



# MDA buying Digital Globe, UTC buying Collins, Northrop buying Orbital, and Boeing buying Aurora

updated_vendor<-ddply(updated_vendor,
                           .(Fiscal.Year,VendorName,Sample),
                           transform, 
                      Merged.2016 = sum(Obligation.2016)
)

updated_vendor$ParentLabel<-as.character(updated_vendor$ParentID)
updated_vendor$ParentLabel[updated_vendor$Sample=="Merged"]<-""



library(ggrepel)


debug(csis360::read_and_join)
csis360::read_and_join(updated_vendor,
  "Lookup_ParentID.csv",
  path="https://raw.githubusercontent.com/CSISdefense/Lookup-Tables/master/",
  by="ParentID",
  directory="vendor//",
  new_var_checked=FALSE,
  add_var="Abbreviation"
  # NA.check.columns="Fair.Competed"
)


ggplot(subset(updated_vendor,pos<=10),
       aes(x=Sample,
           y=Merged.2016,
           color=ParentID,
           group=ParentID))+
  geom_line()+
  geom_label_repel(aes(label = Abbreviation),
    nudge_x = 1,
    na.rm = TRUE)+
  # geom_text(aes(label=ParentLabel,hjust=0))+
  theme(legend.position="none")

# Transform some columns of interest into factor or integer and deflating

# sequestration<-deflate(sequestration,
#   money_var = "PrimeOrSubObligatedAmount",
#   deflator_var="Deflator.2016"
# )



# Aggregating sequestration by Fiscal.Year, Platform Portfolio and IsSubContract

  #This suddenly stopped working after R / package updates. ??
sequestration_Facet<- sequestration %>% dplyr::group_by(Fiscal.Year,
                                                  PlatformPortfolio,
                                                #  IsSubContract,
                                                  SubCustomer.sum,
                                                SubCustomer.platform,
                                                Pricing.Mechanism.sum,
                                                Pricing.Mechanism.Fee,
                                                ProductServiceOrRnDarea,
                                                ProductServiceOrRnDarea.sum,
                                                  Faceting,
                                                Vendor.Size.sum,
                                                Shiny.VendorSize,
                                                IsFSRSreportable) %>%
    dplyr::summarise(PrimeOrSubTotalAmount.2016 = sum(PrimeOrSubObligatedAmount.2016)/1e+9)

# Rename 'IsSubContract' column
#sequestration_Facet$IsSubContract <- ifelse(sequestration_Facet$IsSubContract == 1,
#                                            "SubContract", 
#                                            "Prime Contract")
# colnames(sequestration_Facet)[3] <- "SubCustomer.sum"

full_data<- sequestration_Facet
labels_and_colors<-prepare_labels_and_colors(full_data)

column_key<-csis360::get_column_key(full_data)

save(labels_and_colors,column_key,full_data,file="Shiny Apps//SubContracts//subcontract_full_data.RData")






