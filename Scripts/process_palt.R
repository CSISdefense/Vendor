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
sum(ifelse(def_palt$PALT>3652,1,0),na.rm=TRUE)
summary(def_palt$MinOfSolicitation_Date[def_palt$PALT>3652])
old<-def_palt$MinOfSolicitation_Date[def_palt$PALT>10000 & !is.na(def_palt$PALT)]
def_palt$PALT[def_palt$MinOfSolicitation_Date<"1945-01-01"]<-NA
save(def_palt,file=file.path("data","semi_clean","def_palt.rda"))

load(file=file.path("data","semi_clean","def_palt.rda"))
build_plot(def_palt,
           )

summary(def_palt$PALT)

debug(input_initial_scope)
def_palt<-input_initial_scope(def_palt,
                             file="Defense_Contract.SP_ContractUnmodifiedScope.txt",
                             dir="data//semi_clean//",
                             col_types="idddTTTl")
#For some reason, the dates are being outputted with times, not sure why that changed SQL side
#But in addition to glitching our base column types, it's a waste of space


def_palt<-def_palt%>%filter(!is.na(IsModified))

summary(def_palt$UnmodifiedContractObligatedAmount)
summary(def_palt$UnmodifiedCeiling)
summary(def_palt$UnmodifiedBase)
summary(def_palt$IsModified)
save(def_palt,file=file.path("data","semi_clean","def_palt_scope.rda"))

#Competition vehicle

##Contract_SP_ContractUnmodifiedCompetitionvehicleCustomer.txt
def_palt<-read_and_join_experiment(data=def_palt
                                     ,"Defense_Contract.SP_ContractUnmodifiedCompetitionvehicleCustomer.txt"
                                     ,path=""
                                     ,dir="data/semi_clean/"
                                     ,by="CSIScontractID"
                                     ,new_var_checked=FALSE
                                     ,create_lookup_rdata=TRUE
                                     ,col_types="dddddddccc"
)


#Contract.SP_ContractCompetitionVehicleCustomer.txt
def_palt<-read_and_join_experiment(data=def_palt
                                     ,"Defense_Contract.SP_ContractCompetitionvehicleCustomer.txt"
                                     ,path=""
                                     ,dir="data/semi_clean/"
                                     ,by="CSIScontractID"
                                     ,new_var_checked=FALSE
                                     ,create_lookup_rdata=TRUE
                                     ,col_types="dddddddddccc"
) 


def_palt<-read_and_join_experiment(data=def_palt
                                       ,"Defense_Contract.SP_ContractDefenseSubCustomer.txt"
                                       ,path=""
                                       ,dir="data/semi_clean/"
                                       ,by="CSIScontractID"
                                       ,new_var_checked=FALSE
                                       ,create_lookup_rdata=TRUE
                                       # ,col_types="dddddddddccc"
) 





#Number of Offers

#FUTURE GREG: We'll need to implement the single-award IDV fix here.
def_palt$UnmodifiedNumberOfOffersReceived[def_palt$UnmodifiedNumberOfOffersReceived==0]<-NA
def_palt$NumberOfOffersReceived[def_palt$NumberOfOffersReceived==0]<-NA
def_palt$UnmodifiedNumberOfOffersReceived<-FactorToNumber(def_palt$UnmodifiedNumberOfOffersReceived)
def_palt$NumberOfOffersReceived<-FactorToNumber(def_palt$NumberOfOffersReceived)
def_palt$UnmodifiedNumberOfOffersReceived[is.null(def_palt$UnmodifiedNumberOfOffersReceived)]<-NA
def_palt$NumberOfOffersReceived[is.null(def_palt$NumberOfOffersReceived)]<-NA

def_palt$UnmodifiedNumberOfOffersReceived<-impute_unmodified(
  def_palt$UnmodifiedNumberOfOffersReceived,
  def_palt$NumberOfOffersReceived
)

def_palt$Offr <- cut2(def_palt$UnmodifiedNumberOfOffersReceived,cuts=c(1,2,3,5))



if (all(levels(def_palt$Offr)==c("  1","  2","[  3,  5)","[  5,999]"))){
  def_palt$Offr<-factor(def_palt$Offr, 
                          levels=c("  1","  2","[  3,  5)","[  5,999]"),
                          labels=c("1","2","3-4","5+"),
                          ordered=TRUE
  )
}



# 
# #Competition
# 
# 
# #IsSomeCompetition Impute missing values when labeled entries have a consistent value.
# 
# def_palt$UnmodifiedIsSomeCompetition<-impute_unmodified(
#   def_palt$UnmodifiedIsSomeCompetition,
#   def_palt$IsSomeCompetition
# )
# 
# 
# #Set is some competion = 1 when it is blank and there are multiple offers
# 
# def_palt$UnmodifiedIsSomeCompetition[is.na(def_palt$UnmodifiedIsSomeCompetition)&
#                                          !is.na(def_palt$UnmodifiedNumberOfOffersReceived)&
#                                          def_palt$UnmodifiedNumberOfOffersReceived>1
# ]<-1
# 
# #Set number of offers =1 when there is a NA and no competition
# def_palt$Offr[is.na(def_palt$Offr)&
#                   !is.na(def_palt$UnmodifiedIsSomeCompetition)&
#                   def_palt$UnmodifiedIsSomeCompetition==0
# ]<-"1"
# 
# def_palt$UnmodifiedNumberOfOffersReceived[is.na(def_palt$UnmodifiedNumberOfOffersReceived)&
#                                               !is.na(def_palt$UnmodifiedIsSomeCompetition)&
#                                               def_palt$UnmodifiedIsSomeCompetition==0
# ]<-1
# 
# 
# #Produce Comp factor
# def_palt$Comp<-NA
# def_palt$Comp[def_palt$UnmodifiedIsSomeCompetition==0]<-"No Comp."
# # def_palt$Comp[def_palt$UnmodifiedIsSomeCompetition=="Comp." &
# #                            def_palt$UnmodifiedIsFullAndOpen=="Not Full & Open"]<-"Comp."
# # def_palt$Comp[def_palt$UnmodifiedIsSomeCompetition=="Comp." &
# #                            def_palt$UnmodifiedIsFullAndOpen=="Comp."]<-"Comp."
# def_palt$Comp[def_palt$UnmodifiedIsSomeCompetition==1]<-"Comp."
# 
# def_palt$Comp<-factor(def_palt$Comp,
#                         levels=c("No Comp.","Comp.")
# )
# 
# 
# #Produce EffComp factor
# 
# #Competition and Offer  
# def_palt$EffComp<-as.character(def_palt$Comp)
# 
# def_palt$EffComp[def_palt$Comp=="Comp." & !is.na(def_palt$Comp)]<-
#   ifelse(def_palt$UnmodifiedNumberOfOffersReceived[
#     def_palt$Comp=="Comp." & !is.na(def_palt$Comp)]>1,"2+ Offers","1 Offer")
# 
# def_palt$EffComp<-factor(def_palt$EffComp,
#                            levels=c("No Comp.","1 Offer","2+ Offers")
# )
# 
# 
# #Impute missing urgency values
# def_palt$UnmodifiedIsUrgency<-impute_unmodified(
#   def_palt$UnmodifiedIsUrgency,
#   def_palt$IsUrgency
# )
# #If there's competition, than the urgency exception wasn't used
# def_palt$UnmodifiedIsUrgency[is.na(def_palt$UnmodifiedIsUrgency)&
#                                  !is.na(def_palt$UnmodifiedIsSomeCompetition)&
#                                  def_palt$UnmodifiedIsSomeCompetition==1]<-0
# 
# def_palt$Urg<-NA
# def_palt$Urg[def_palt$UnmodifiedIsUrgency==0]<-"Not Urgency"
# def_palt$Urg[def_palt$UnmodifiedIsUrgency==1]<-"Urgency Except."
# def_palt$Urg<-factor(def_palt$Urg,
#                        levels=c("Not Urgency","Urgency Except.")
# )

#Vehicle
#Award_Type_Code Impute missing values when labeled entries have a consistent value.
def_palt$unmodifiedaward_type_code<-impute_unmodified(
  def_palt$unmodifiedaward_type_code,
  def_palt$Award_Type_Code
)

#idv_type_code Impute missing values when labeled entries have a consistent value.
def_palt$unmodifiedidv_type_code<-impute_unmodified(
  def_palt$unmodifiedidv_type_code,
  def_palt$IDV_Type_Code
)


#Unmodifiedmultipleorsingleawardidc Impute missing values when labeled entries have a consistent value.
def_palt$Unmodifiedmultipleorsingleawardidc<-impute_unmodified(
  def_palt$Unmodifiedmultipleorsingleawardidc,
  def_palt$multipleorsingleawardidc
)

#Assign Is IDV
def_palt$IsIDV<-NA
#A = BPA Call, C = Delivery Order
def_palt$IsIDV[!is.na(def_palt$unmodifiedaward_type_code) &
                   def_palt$unmodifiedaward_type_code %in% c("A","C")]<-1
#B = Purchase Order, D = Definitive Contract
def_palt$IsIDV[!is.na(def_palt$unmodifiedaward_type_code) &
                   def_palt$unmodifiedaward_type_code %in% c("B","D")]<-0
def_palt$IsIDV[!is.na(def_palt$unmodifiedidv_type_code)&def_palt$unmodifiedidv_type_code!=""]<-1

def_palt$IsIDV[is.na(def_palt$unmodifiedaward_type_code)&def_palt$unmodifiedidv_type_code==""]<-0


if(is.numeric(def_palt$IsIDV)){
  def_palt$IsIDV<-factor(def_palt$IsIDV,levels=c(0,1),labels=c("Def/Pur","IDV"))
}


#Simple Vehicle, which consolidates IDV and award types
def_palt$Veh<-NA
def_palt$Veh[
  (!is.na(def_palt$unmodifiedaward_type_code) &
    def_palt$unmodifiedaward_type_code %in% c("B","D"))|
    (is.na(def_palt$unmodifiedaward_type_code) & 
       !is.na(def_palt$unmodifiedidv_type_code) &
      def_palt$unmodifiedidv_type_code=="")]<-"Def/Pur"


def_palt$Veh[
  !is.na(def_palt$unmodifiedidv_type_code) &
    def_palt$unmodifiedidv_type_code %in% c("B") &
    !is.na(def_palt$Unmodifiedmultipleorsingleawardidc)]<-
  paste(def_palt$Unmodifiedmultipleorsingleawardidc[
    !is.na(def_palt$unmodifiedidv_type_code) &
      def_palt$unmodifiedidv_type_code %in% c("B") &
      !is.na(def_palt$Unmodifiedmultipleorsingleawardidc)
  ],"IDC")

#IDV_type_Code D = BOA, E=BPA, Award_Type_code A=BPA Call
def_palt$Veh[
  !is.na(def_palt$unmodifiedidv_type_code) &
    def_palt$unmodifiedidv_type_code %in% c("D","E") |
    !is.na(def_palt$unmodifiedaward_type_code) &
    def_palt$unmodifiedaward_type_code %in% c("A") 
]<-
  "BPA/BOA"
#IDV_type_cde A=GWAC C=FSS
def_palt$Veh[
  !is.na(def_palt$unmodifiedidv_type_code)
  & def_palt$unmodifiedidv_type_code 
  %in% c('A','C')]<-"FSS/GWAC"

#Imputing that when Multiple Award/Single Award is known, but IDV_type_code is not
#that the vehicle type is an IDC
def_palt$Veh[
  is.na(def_palt$Veh)&
    is.na(def_palt$unmodifiedidv_type_code) &
    !is.na(def_palt$Unmodifiedmultipleorsingleawardidc)]<-
  paste(def_palt$Unmodifiedmultipleorsingleawardidc[
    is.na(def_palt$Veh)&
      is.na(def_palt$unmodifiedidv_type_code) &
      !is.na(def_palt$Unmodifiedmultipleorsingleawardidc)],"IDC")

def_palt$Veh<-factor(def_palt$Veh)
summary(def_palt$Veh)
#Imputing that when the simple vehicle is a kind of IDV, IsIDV should follow as well
def_palt$IsIDV[
  is.na(def_palt$IsIDV)&
    def_palt$Veh %in% c(
      'BPA/BOA','FSS/GWAC','MULTIPLE AWARD IDC','SINGLE AWARD IDC'
    )]<-"IDV"


def_palt$UnmodifiedCustomer<-impute_unmodified(
  def_palt$UnmodifiedCustomer,
  def_palt$Customer
)

def_palt$UnmodifiedSubCustomer<-impute_unmodified(
  def_palt$UnmodifiedSubCustomer,
  def_palt$SubCustomer
)

def_palt$UnmodifiedContractingOfficeAgencyID<-impute_unmodified(
  def_palt$UnmodifiedContractingOfficeAgencyID,
  def_palt$ContractingOfficeAgencyID
)

summary(factor(def_palt$UnmodifiedContractingOfficeAgencyID))

def_palt<-def_palt[,!colnames(def_palt) %in% 
                         c(
                           #     UnmodifiedIsFullAndOpen,
                           # "UnmodifiedIsOnlyOneSource",
                           "UnmodifiedIsFollowonToCompetedAction",
                           "Unmodifiedmultipleorsingleawardidc",
                           # "UnmodifiedIsUrgency",
                           "Unmodifiedaddmultipleorsingawardidc",
                           # "IsFullAndOpen",
                           # IsOnlyOneSource 
                           # "IsFollowonToCompetedAction"
                           "IsUrgency",
                           "multipleorsingleawardidc",
                           "AddMultipleOrSingleAwardIDC",
                           "Award_Type_Code",
                           "unmodifiedaward_type_code",
                           "IDV_Type_Code",
                           # "UnmodifiedNumberOfOffersReceived",
                           "unmodifiedidv_type_code",
                           "NumberOfOffersReceived",
                           "Customer",
                           "SubCustomer",
                           "ContractingOfficeAgencyID"
                         )]

save(def_palt,file=file.path("data","semi_clean","def_palt_veh.rda"))



load(file=file.path("data","semi_clean","def_palt_veh.rda"))