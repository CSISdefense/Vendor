library(csis360)

naics<-read.csv("C:/Users/GSanders/Documents/Repositories/Lookup-Tables/economic/Lookup_PrincipalNAICScode.csv",na.strings="NULL")
naics<-csis360::remove_bom(naics)
naics$principalNAICS2DigitCode<-as.character(naics$principalNAICS2DigitCode)
naics$principalNAICS2DigitCode[naics$principalNAICS2DigitCode %in% c('21','22','23')]<-'21-23'
naics$principalNAICS2DigitCode[naics$principalNAICS2DigitCode %in% c('44','45')]<-'44-45'
naics$principalNAICS2DigitCode[naics$principalNAICS2DigitCode %in% c('48','49')]<-'48-49'
naics$principalNAICS2DigitCode<-factor(naics$principalNAICS2DigitCode)

summary(naics$principalnaicscode)
summary(naics$principalNAICS2DigitCode)
summary(naics$principalNAICS3DigitCode)
summary(naics$principalNAICS4DigitCode)

naics2_adjust<-function(data){
  data$NAICS2<-as.character(data$NAICS2)
  data$NAICS2[data$NAICS2 %in% c('21','22','23')]<-'21-23'
  data$NAICS2[data$NAICS2 %in% c('44','45')]<-'44-45'
  data$NAICS2[data$NAICS2 %in% c('48','49')]<-'48-49'
  data$NAICS2<-factor(naics$NAICS2)

}
