#*************************************Required Libraries******************************************
require(dplyr)
require(grid)
require(reshape2)
require(stringr)
require(ggplot2)
library(Hmisc)
library(readr)
library(csis360)
#*************************************Options*****************************************************
options(error=recover)
options(warn=1)

#*************************************Lookup Files*****************************************************

source("scripts/DIIGstat.r")
source("https://raw.githubusercontent.com/CSISdefense/Crisis-Funding/master/Scripts/ContractCleanup.r")
source("https://raw.githubusercontent.com/CSISdefense/R-scripts-and-data/master/lookups.r")
source("https://raw.githubusercontent.com/CSISdefense/R-scripts-and-data/master/helper.r")


#For some reason I keep having to go to apply_lookups.r for  get_delim, should be covered by csis360 
if(!exists("def_palt")) def_palt<-NULL
# debug(input_sample_criteria)
def_palt<-input_sample_criteria(def_palt,  
                               dir="data\\semi_clean\\",
                               file="Contract.SP_ContractSampleCriteriaDetailsCustomer.txt",
                               last_date=as.Date("2019-12-31"),
                               drop_incomplete=FALSE)

def_palt<-filter(def_palt, StartFiscal_Year>=min(def_palt$StartFiscal_Year[!is.na(def_palt$MinOfSolicitation_Date)]))
fill_rate<-def_palt %>% group_by(StartFiscal_Year) %>% dplyr::summarize(
  p=sum(ifelse(!is.na(MinOfSolicitation_Date),1,0))/length(StartFiscal_Year)
  )
summary(def_palt$MinOfSolicitation_Date)
#Requirement comes in on 2018-06-19
def_palt<-def_palt %>% filter(MinOfSignedDate>=as.Date("2018-06-19"))
def_palt$PALT<-as.numeric(
  difftime(strptime(def_palt$MinOfSignedDate,"%Y-%m-%d")
           , strptime(def_palt$MinOfSolicitation_Date,"%Y-%m-%d")
           , unit="days"
  ))+1

sum(ifelse(def_palt$PALT<0,1,0),na.rm=TRUE)
def_palt$PALT[def_palt$PALT<0]<-NA
sum(ifelse(def_palt$PALT>10000,1,0),na.rm=TRUE)
summary(def_palt$MinOfSolicitation_Date[def_palt$PALT>10000])
old<-def_palt$MinOfSolicitation_Date[def_palt$PALT>10000 & !is.na(def_palt$PALT)]
def_palt$PALT[def_palt$MinOfSolicitation_Date<"1945-01-01"]<-NA
save(def_palt,file=file.path("data","semi_clean","def_palt.rda"))
build_plot(def_palt,
           )

summary(def_palt$PALT)

count<-ggplot(def_palt, aes(x=PALT))+geom_histogram()+scale_x_log10()
dollar<-ggplot(def_palt, aes(x=PALT,y=Action_Obligation))+geom_point()+scale_x_log10()+scale_y_log10()
# dollar<-ggplot(def_palt, aes(x=PALT,y=Action_Obligation))+geom_histogram()+scale_x_log10()


palt_hist<-build_plot(
  data=def_palt,
  chart_geom = "Histogram",
  share = FALSE,
  x_var="PALT",
  # y_var="hh_index", #Name of variable to plot on y-axis
  # color_var="PlatformPortfolio",       # name of coloration variable, as string
  facet_var="MinOfSolicitation_Date",        # name of facet variable, as string
  legend=FALSE, #Include a legend
  caption=FALSE, #Include a source caption
  # labels_and_colors=labels_and_colors,
  # column_key=NULL
)