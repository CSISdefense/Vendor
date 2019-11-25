---
title: "ContractTermination"
author: "Greg Sanders"
date: "Wednesday, February 8, 2017"
output:
  html_document:
    keep_md: yes
--- 

Annual Outcome 
============================================================================


#Setup

## Read in packages and functions

```r
source("https://raw.githubusercontent.com/CSISdefense/R-scripts-and-data/master/helper.r")
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following object is masked from 'package:plyr':
## 
##     here
```

```
## The following object is masked from 'package:base':
## 
##     date
```

```r
source("https://raw.githubusercontent.com/CSISdefense/R-scripts-and-data/master/lookups.r")
```

```
## Warning: replacing previous import 'Hmisc::summarize' by 'dplyr::summarize'
## when loading 'csis360'
```

```
## Warning: replacing previous import 'Hmisc::src' by 'dplyr::src' when
## loading 'csis360'
```

```
## Warning: replacing previous import 'dplyr::intersect' by
## 'lubridate::intersect' when loading 'csis360'
```

```
## Warning: replacing previous import 'dplyr::union' by 'lubridate::union'
## when loading 'csis360'
```

```
## Warning: replacing previous import 'dplyr::setdiff' by 'lubridate::setdiff'
## when loading 'csis360'
```

```r
source("../scripts/DIIGstat.r")
```

```
## Loading required package: MASS
```

```
## Loading required package: Matrix
```

```
## Loading required package: lme4
```

```
## 
## arm (Version 1.10-1, built: 2018-4-12)
```

```
## Working directory is F:/Users/Greg/Repositories/Vendor/analysis
```

```
## 
## Attaching package: 'arm'
```

```
## The following object is masked from 'package:scales':
## 
##     rescale
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following object is masked from 'package:MASS':
## 
##     select
```

```
## The following objects are masked from 'package:lubridate':
## 
##     intersect, setdiff, union
```

```
## The following objects are masked from 'package:plyr':
## 
##     arrange, count, desc, failwith, id, mutate, rename, summarise,
##     summarize
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```
## Loading required package: carData
```

```
## Registered S3 methods overwritten by 'car':
##   method                          from
##   influence.merMod                lme4
##   cooks.distance.influence.merMod lme4
##   dfbeta.influence.merMod         lme4
##   dfbetas.influence.merMod        lme4
```

```
## 
## Attaching package: 'car'
```

```
## The following object is masked _by_ '.GlobalEnv':
## 
##     Boxplot
```

```
## The following object is masked from 'package:dplyr':
## 
##     recode
```

```
## The following object is masked from 'package:arm':
## 
##     logit
```

```r
source("https://raw.githubusercontent.com/CSISdefense/Crisis-Funding/master/Scripts/ContractCleanup.r")
```

```
## Loading required package: lattice
```

```
## Loading required package: survival
```

```
## Loading required package: Formula
```

```
## 
## Attaching package: 'Hmisc'
```

```
## The following object is masked _by_ '.GlobalEnv':
## 
##     subplot
```

```
## The following object is masked from 'package:sjstats':
## 
##     deff
```

```
## The following objects are masked from 'package:dplyr':
## 
##     src, summarize
```

```
## The following objects are masked from 'package:plyr':
## 
##     is.discrete, summarize
```

```
## The following objects are masked from 'package:base':
## 
##     format.pval, units
```

```
## 
## Attaching package: 'readr'
```

```
## The following object is masked from 'package:scales':
## 
##     col_factor
```

```r
library(csis360)
library(ggplot2)
library(scales)
library(Hmisc)
library(dplyr)
# Coloration<-read.csv(
#     paste(Path,"Lookups\\","lookup_coloration.csv",sep=""),
#     header=TRUE, sep=",", na.strings="", dec=".", strip.white=TRUE, 
#     stringsAsFactors=FALSE
#     )
# 
# Coloration<-ddply(Coloration
#                   , c(.(R), .(G), .(B))
#                   , mutate
#                   , ColorRGB=as.character(
#                       if(min(is.na(c(R,G,B)))) {NA} 
#                       else {rgb(max(R),max(G),max(B),max=255)}
#                       )
#                   )

axis.text.size<-10
strip.text.size<-10
legend.text.size<-8
# table.text.size<-5.75
title.text.size<-12
geom.text.size<-12

main.text.size<-1
note.text.size<-1.40

all_labeled<-function(data){
  data<-data %>% dplyr::filter(
                                        !is.na(UnmodifiedDays) & 
               !is.na(UnmodifiedCeiling_OMB20_GDP18) &
               !is.na(p_CBre) & 
           !is.na(Term))
}

missing_caption<-function(data){
  data<-data %>% dplyr::filter(
                                        is.na(UnmodifiedDays) |
               is.na(UnmodifiedCeiling_OMB20_GDP18) |
               is.na(p_CBre) |
           is.na(Term))
  
  paste("Missing: Count:Data is missing for",prettyNum(nrow(data),big.mark=","),"contracts accounting for",
        round(sum(data$Action_Obligation_OMB20_GDP18)/1000000000,1),"B","in obligations.")
}

# FacetCount=paste("Count:",prettyNum(sum(Count),big.mark=",")),
#     FacetValue=paste(FacetCount,"\nObligated: $",round(sum(Action_Obligation_OMB20_GDP18)/1000000000,1),"B",sep="")

only_complete<-function(data){
  data<-all_labeled(data) %>%
    dplyr::filter((LastCurrentCompletionDate<=as.Date("2018-09-30") |
              IsClosed==1) &
           UnmodifiedCurrentCompletionDate<as.Date("2018-09-30"))
}



load(file="../data/clean/defense_contract_all_detail.RData")
# debug(transform_contract)
# undebug(transform_contract)

def_all<-def_all %>%
  dplyr::filter(StartFiscal_Year>=2007 & 
                  StartFiscal_Year<=2017 
  ) %>% dplyr::select(CSIScontractID,
                      IsTerminated,
                      IsClosed,
                      IsComplete,
                      pChangeOrderUnmodifiedBaseAndAll,
                      n_CBre,
                      Action_Obligation,
                      UnmodifiedDays,
                      qHighCeiling,
                      qLowCeiling,
                      UnmodifiedCeiling,
                      StartFiscal_Year,
                      LastCurrentCompletionDate,
                      UnmodifiedCurrentCompletionDate,
                      CBre,
                      SumOfisChangeOrder
                      )



#Unknown or uninitialised column: 'qCRais'.Unknown or uninitialised column: 'qNChg'.> 


if(any(duplicated(colnames(def_all)))) stop("Duplicate Contract Name")

def_all<-rename_dataset(def_all)
def_all<-transform_contract(def_all)
```

```
## Parsed with column specification:
## cols(
##   CSIScontractID = col_double(),
##   override_unmodified_ceiling = col_logical(),
##   override_unmodified_base = col_logical(),
##   override_change_order_growth = col_logical(),
##   override_exercised_growth = col_logical(),
##   CSIS_inspection = col_character()
## )
```

```
## 
##  Applying
##  OMB20_GDP18 
##  in 
##  Lookup_Deflators.csv 
##  from
##  https://raw.githubusercontent.com/CSISdefense/Lookup-Tables/master/
```

```
## Parsed with column specification:
## cols(
##   Fiscal_Year = col_double(),
##   GDPdeflator = col_double(),
##   GDPdeflatorName = col_character(),
##   GDPdeflator2015 = col_character(),
##   GDPdeflator2016 = col_character(),
##   GDPdeflator1990 = col_character(),
##   GDPdeflator2005 = col_character(),
##   GDPdeflator2011 = col_character(),
##   GDPdeflator2012 = col_character(),
##   GDPdeflator2013 = col_character(),
##   GDPdeflator2017 = col_character(),
##   Unknown2017 = col_character(),
##   OMB19_19 = col_character(),
##   GDPdeflator2014 = col_character(),
##   OMB20_GDP18 = col_double()
## )
```

```
## 
##  Applying
##  OMB20_GDP18 
##  in 
##  Lookup_Deflators.csv 
##  from
##  https://raw.githubusercontent.com/CSISdefense/Lookup-Tables/master/
```

```
## Parsed with column specification:
## cols(
##   Fiscal_Year = col_double(),
##   GDPdeflator = col_double(),
##   GDPdeflatorName = col_character(),
##   GDPdeflator2015 = col_character(),
##   GDPdeflator2016 = col_character(),
##   GDPdeflator1990 = col_character(),
##   GDPdeflator2005 = col_character(),
##   GDPdeflator2011 = col_character(),
##   GDPdeflator2012 = col_character(),
##   GDPdeflator2013 = col_character(),
##   GDPdeflator2017 = col_character(),
##   Unknown2017 = col_character(),
##   OMB19_19 = col_character(),
##   GDPdeflator2014 = col_character(),
##   OMB20_GDP18 = col_double()
## )
```

```
## Warning: Factor `qHighCeiling` contains implicit NA, consider using
## `forcats::fct_explicit_na`
```

```
## Warning: Factor `qHighCeiling` contains implicit NA, consider using
## `forcats::fct_explicit_na`

## Warning: Factor `qHighCeiling` contains implicit NA, consider using
## `forcats::fct_explicit_na`

## Warning: Factor `qHighCeiling` contains implicit NA, consider using
## `forcats::fct_explicit_na`
```

```r
def_all<-FormatContractModel(def_all)
```

```
## Warning: Unknown or uninitialised column: 'qCRais'.
```

```r
head(def_all)
```

```
## # A tibble: 6 x 44
## # Groups:   qHighCeiling [2]
##   CSIScontractID Term  IsClosed IsComplete pChangeOrderUnm~ n_CBre
##            <dbl> <fct> <fct>         <dbl>            <dbl>  <dbl>
## 1       20693487 Unte~ Unspeci~          1                0      0
## 2       25324904 Unte~ Unspeci~          1                0      0
## 3       26096275 Unte~ Unspeci~          1                0      0
## 4       62686048 Unte~ Unspeci~          1                0      0
## 5       64737592 Unte~ Unspeci~          1                0      0
## 6       69087825 Unte~ Unspeci~          1                0      0
## # ... with 38 more variables: Action_Obligation_Then_Year <dbl>,
## #   UnmodifiedDays <dbl>, Ceil <fct>, LowCeil <fct>,
## #   UnmodifiedCeiling_Then_Year <dbl>, StartFY <dbl>,
## #   LastCurrentCompletionDate <date>,
## #   UnmodifiedCurrentCompletionDate <date>, CBre <ord>,
## #   SumOfisChangeOrder <dbl>, qNChg <fct>, b_Term <dbl>, j_Term <dbl>,
## #   b_CBre <dbl>, j_CBre <dbl>, override_unmodified_ceiling <lgl>,
## #   override_unmodified_base <lgl>, override_change_order_growth <lgl>,
## #   override_exercised_growth <lgl>, CSIS_inspection <chr>,
## #   Action_Obligation_OMB20_GDP18 <dbl>,
## #   UnmodifiedCeiling_OMB20_GDP18 <dbl>, qHighCeiling <fct>,
## #   ceil.median.wt <dbl>, Ceil.Simple <fct>, Ceil.Big <fct>,
## #   Ceil.1m <fct>, capped_UnmodifiedDays <dbl>, cln_Days <dbl>,
## #   capped_cl_Days <dbl>, UnmodifiedYearsFloat <dbl>,
## #   UnmodifiedYearsCat <dbl>, qDuration <ord>, Dur.Simple <ord>,
## #   cln_Ceil <dbl>, ObligationWT_Then_Year <dbl>, CRais <fct>,
## #   ContractCount <dbl>
```

```r
# write.csv(subset(def_all,Term=="Terminated"),"Terminated.csv")

colnames(def_all)[colnames(def_all)=="pChangeOrderUnmodifiedBaseAndAll"]<-"p_CBre"


memory.limit(30000)
```

```
## [1] 30000
```


Contracts are classified using a mix of numerical and categorical variables. While the changes in numerical variables are easy to grasp and summarize, a contract may have one line item that is competed and another that is not. As is detailed in the exploration on R&D, we are only considering information available prior to contract start. The percentage of contract obligations that were competed is a valuable benchmark, but is highly influenced by factors that occured after contract start..


**A Histogram of the IsTerminated data** showing the distribution of whether or not a contract was terminated each year from 2007.  


```r
# TerminatedDurSummary<-ddply(subset(def_all,StartFY>=2007 & 
#                   !is.na(Ceil)&
#                   UnmodifiedCompletionDate<=as.Date("2015-09-30")&
#                       !is.na(Term)),
#                             .(Ceil,
#                               qDuration,
#                               StartFY,
#                               Term
#                             ),
#                             dplyr::summarise,
#                             Action_Obligation_OMB20_GDP18=sum(Action_Obligation_OMB20_GDP18),
#                             Count=length(CSIScontractID)
#                   )
# 
# 
# TerminatedDurSummary<-ddply(TerminatedDurSummary,.(Ceil,
#                                                   qDuration,
#                                              StartFY
#                                              ),transform,
#                       pContractCeilDurStart=Count/sum(Count),
#                       pObligationCeilDurStart=Action_Obligation_OMB20_GDP18/sum(Action_Obligation_OMB20_GDP18)
#       )
# 
# 
# ggplot(TerminatedDurSummary,
#        aes(x=StartFY,
#            y=Count,
#            color=Term))+geom_line()+    geom_point(aes(shape=metric))+facet_grid(Ceil ~ qDuration ) +scale_y_log10(labels=scales::comma)
# 
# 
# 
# 
# 
# 
# ggplot(
#   data = TerminatedEndSummary,
#   aes_string(x = "Term"),
#   ) + geom_bar() + 
#     facet_grid( Ceil ~ .,
#                 scales = "free_y",
#                 space = "free_y") + scale_y_continuous(expand = c(0,50)) 
# 
# 
# 
# 
# 
# ggplot(
#   data = subset(TerminatedEndSummary,Term=="Terminated"),
#   aes_string(x = "Ceil")
#   )+ geom_bar()+
#     scale_x_discrete("Original Ceiling (Current $ Value)")+scale_y_continuous("Number of Partially or Completely \nTerminated Contracts",labels = comma)+theme(axis.text.x=element_text(angle=90,size=12))
# 
# 
# 
# 
# 
# 
# TerminatedEndSummary$Graph[TerminatedEndSummary$Term=="Terminated"]<-TRUE
# 
# TerminatedEndSummary$Graph[TerminatedEndSummary$Term=="Unterminated"]<-FALSE
# 
# 
# head(TerminatedEndSummary)
# 
# ggplot(
#   data = subset(TerminatedEndSummary,Term=="Terminated"),
#   aes(x = Ceil,weight=Action_Obligation_OMB20_GDP18/1000000000)
#   )+ geom_bar()+
#     scale_x_discrete("Original Ceiling (Current $ Value)")+scale_y_continuous("Obligations to Partially or Completely\nTerminated Contracts (Current $ Billions)",labels = comma)+theme(axis.text.x=element_text(angle=90,size=12))
# 
# 
# ggplot(
#   data = subset(TerminatedEndSummary,Term=="Terminated"),
#   aes_string(x = "Ceil",weight="pContract")
# #   main="Percentage of Contracts going to Partially or Completely Terminated Contracts\nBy Initial Contract Ceiling"
#   )+ geom_bar()+ scale_y_continuous("Percent of Contracts Partially or Completely Terminated\nby Original Ceiling Category", labels=percent)+
#     scale_x_discrete("Original Ceiling (Current $ Value)")+theme(axis.text.x=element_text(angle=90,size=12))
# 
# 
# ggplot(
#   data = subset(TerminatedEndSummary,Term=="Terminated"),
#   aes_string(x = "Ceil",weight="pObligation"),
#   main="Percentage of Contract Obligations going to Partially or Completely Terminated Contracts\nBy Initial Contract Ceiling"
#   )+ geom_bar()+ scale_y_continuous("Percent of Obligations to Terminated Contracts \nin Original Ceiling Category", labels=percent)+
#     scale_x_discrete("Original Ceiling (Current $ Value)")+theme(axis.text.x=element_text(angle=90,size=12))
# 
# 
# # 
# # LatticePercentLineWrapper("VAR.name"
# #                                     ,"VAR.proper.name"
# #                                     ,"VAR.X.label"
# #                                     ,"VAR.Y.label"
# #                                     ,Coloration
# #                                     ,subset(TerminatedEndSummary,!is.na(Term))
# #                                     ,NULL
# #                                     ,"Ceil"
# #                                     ,"Count"
# #                                     ,"Term"
# #                                     ,NA
# #                                     ,NA
# #                                     )
# 
# # 
# # + 
# #     facet_grid( Ceil ~ .,
# #                 scales = "free_y",
# #                 space = "free_y") 
# # 
```



```r
# 
# ggplot(TerminatedEndSummary,
#        aes(x=StartFY,
#            y=Count,
#            color=Term))+geom_line()+    geom_point(aes(shape=metric))+facet_grid(Ceil ~ EndAfterPeriod ) +scale_y_log10()
```



```r
# 
# TerminatedUnmodifiedYearsCatStat<-rbind(ddply(subset(def_all,
#                                 !is.na(qDuration) & StartFY>=2007 & 
#                                     !is.na(UnmodifiedYearsCat) &
#                                     UnmodifiedCurrentCompletionDate<as.Date("2015-09-30")&
#                                     !is.na(Term)),
#                          .(UnmodifiedYearsCat,
#                            StartFY
#                          ),
#                          
#                          dplyr::summarise,
#                          Action_Obligation_OMB20_GDP18=sum(Action_Obligation_OMB20_GDP18),
#                          Count=length(CSIScontractID),
#                          mean = mean(TermNum),
#                          sd   = NA ,# sd(TermNum),
#                          se   = NA, #sd / sqrt(Count),
#                          metric="Unweighted"
#                           # ceil.mean = wtd.mean(TermNum,UnmodifiedContractBaseAndAllOptionsValue),
#                          # ceil.cat.mean = wtd.mean(TermNum,ceil.median.wt)
# ))
# 
# 
# 
# TerminatedUnmodifiedYearsCatStat<-rbind(TerminatedUnmodifiedYearsCatStat,
#                          ddply(subset(def_all,
#                                 !is.na(qDuration) & StartFY>=2007 & 
#                                     !is.na(Ceil) &
#                                     UnmodifiedCurrentCompletionDate<as.Date("2015-09-30")&
#                                     !is.na(Term)),
#                          .(UnmodifiedYearsCat,
#                            StartFY
#                          ),
#                          
#                          dplyr::summarise,
#                          Action_Obligation_OMB20_GDP18=sum(Action_Obligation_OMB20_GDP18),
#                          Count=length(CSIScontractID),
#                          mean = wtd.mean(TermNum,ObligationWT),
#                          sd   = NA ,# sd(TermNum),
#                          se   = NA, #sd / sqrt(Count),
#                          metric="Obligation Weighted"
#                           # ceil.mean = wtd.mean(TermNum,UnmodifiedContractBaseAndAllOptionsValue),
#                          # ceil.cat.mean = wtd.mean(TermNum,ceil.median.wt)
# 
# ))
# 
# TerminatedUnmodifiedYearsCatStat<-rbind(TerminatedUnmodifiedYearsCatStat,
#                          ddply(subset(def_all,
#                                 !is.na(qDuration) & StartFY>=2007 & 
#                                     !is.na(Ceil) &
#                                     UnmodifiedCurrentCompletionDate<as.Date("2015-09-30")&
#                                     !is.na(Term)),
#                          .(UnmodifiedYearsCat,
#                            StartFY
#                          ),
#                          
#                          dplyr::summarise,
#                          Action_Obligation_OMB20_GDP18=sum(Action_Obligation_OMB20_GDP18),
#                          Count=length(CSIScontractID),
#                          mean = wtd.mean(TermNum,UnmodifiedContractBaseAndAllOptionsValue),
#                          sd   = NA ,# sd(TermNum),
#                          se   = NA, #sd / sqrt(Count),
#                          metric="Ceiling Weighted"
#                          # obl.mean = ,
#                          # ceil.mean = wtd.mean(TermNum,UnmodifiedContractBaseAndAllOptionsValue),
#                          # ceil.cat.mean = wtd.mean(TermNum,ceil.median.wt)
# 
# ))
# 
# 
# TerminatedUnmodifiedYearsCatStat<-rbind(TerminatedUnmodifiedYearsCatStat,
#                          ddply(subset(def_all,
#                                 !is.na(qDuration) & StartFY>=2007 & 
#                                     !is.na(Ceil) &
#                                     UnmodifiedCurrentCompletionDate<as.Date("2015-09-30")&
#                                     !is.na(Term)),
#                          .(UnmodifiedYearsCat,
#                            StartFY
#                          ),
#                          
#                          dplyr::summarise,
#                          Action_Obligation_OMB20_GDP18=sum(Action_Obligation_OMB20_GDP18),
#                          Count=length(CSIScontractID),
#                          mean = wtd.mean(TermNum,ceil.median.wt),
#                          sd   = NA ,# sd(TermNum),
#                          se   = NA, #sd / sqrt(Count),
#                          metric="Ceiling Category Weighted"
# 
# ))
# 
# 
# # 
# # pd <- position_dodge(0.1) # move them .05 to the left and right
# # 
# # ggplot(tgc, aes(x=dose, y=len, colour=supp)) + 
# #     geom_errorbar(aes(ymin=len-se, ymax=len+se), width=.1, position=pd) 
# 
# ggplot(TerminatedUnmodifiedYearsCatStat,aes(x=StartFY,y=mean,color=metric))+
#     geom_line()+
#         geom_point(aes(shape=metric))+
#     facet_grid(.~ UnmodifiedYearsCat ) +
#     scale_x_continuous("Contract Starting Fiscal Year")+
#     scale_y_continuous(label=percent)+
#     geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1)+
#     theme(legend.position="bottom") #, position=pd
```


```r
# 
# TerminatedDurStat<-rbind(                         ddply(subset(def_all,
#                                 !is.na(qDuration) & StartFY>=2007 & 
#                                     StartFY<=2014 & 
#                                     !is.na(Ceil) &
#                                     UnmodifiedCurrentCompletionDate<as.Date("2015-09-30")&
#                                     !is.na(Term)),
#                          .(qDuration,
#                            StartFY
#                          ),
#                          
#                          dplyr::summarise,
#                          Action_Obligation_OMB20_GDP18=sum(Action_Obligation_OMB20_GDP18),
#                          Count=length(CSIScontractID),
#                          mean = mean(TermNum),
#                          sd   = sd(TermNum),
#                          se   = sd / sqrt(Count),
#                          metric="Unweighted"
#                           # ceil.mean = wtd.mean(TermNum,UnmodifiedContractBaseAndAllOptionsValue),
#                          # ceil.cat.mean = wtd.mean(TermNum,ceil.median.wt)
# ))
# 
# 
# TerminatedDurStat<-rbind(TerminatedDurStat,
#                          ddply(subset(def_all,
#                                 !is.na(qDuration) & StartFY>=2007 & 
#                                     StartFY<=2014 & 
#                                     !is.na(Ceil) &
#                                     UnmodifiedCurrentCompletionDate<as.Date("2015-09-30")&
#                                     !is.na(Term)),
#                          .(qDuration,
#                            StartFY
#                          ),
#                          
#                          dplyr::summarise,
#                          Action_Obligation_OMB20_GDP18=sum(Action_Obligation_OMB20_GDP18),
#                          Count=length(CSIScontractID),
#                          mean = wtd.mean(TermNum,ObligationWT),
#                          sd   = sqrt(wtd.var(TermNum,ObligationWT)) ,
#                          se   = sd / sqrt(Count),
#                          metric="Obligation Weighted"
#                           # ceil.mean = wtd.mean(TermNum,UnmodifiedContractBaseAndAllOptionsValue),
#                          # ceil.cat.mean = wtd.mean(TermNum,ceil.median.wt)
# ))
# 
# TerminatedDurStat<-rbind(TerminatedDurStat,
#                          ddply(subset(def_all,
#                                 !is.na(qDuration) & StartFY>=2007 & 
#                                     StartFY<=2014 & 
#                                     !is.na(Ceil) &
#                                     UnmodifiedCurrentCompletionDate<as.Date("2015-09-30")&
#                                     !is.na(Term)),
#                          .(qDuration,
#                            StartFY
#                          ),
#                          
#                          dplyr::summarise,
#                          Action_Obligation_OMB20_GDP18=sum(Action_Obligation_OMB20_GDP18),
#                          Count=length(CSIScontractID),
#                          mean = wtd.mean(TermNum,UnmodifiedContractBaseAndAllOptionsValue),
#                          sd   = sqrt(wtd.var(TermNum,UnmodifiedContractBaseAndAllOptionsValue)) ,
#                          se   = sd / sqrt(Count),
#                          metric="Ceiling Weighted"
#                          # obl.mean = ,
#                          # ceil.mean = wtd.mean(TermNum,UnmodifiedContractBaseAndAllOptionsValue),
#                          # ceil.cat.mean = wtd.mean(TermNum,ceil.median.wt)
# 
# ))
# 
# 
# TerminatedDurStat<-rbind(TerminatedDurStat,
#                          ddply(subset(def_all,
#                                 !is.na(qDuration) & StartFY>=2007 & 
#                                     StartFY<=2014 & 
#                                     !is.na(Ceil) &
#                                     UnmodifiedCurrentCompletionDate<as.Date("2015-09-30")&
#                                     !is.na(Term)),
#                          .(qDuration,
#                            StartFY
#                          ),
#                          
#                          dplyr::summarise,
#                          Action_Obligation_OMB20_GDP18=sum(Action_Obligation_OMB20_GDP18),
#                          Count=length(CSIScontractID),
#                          mean = wtd.mean(TermNum,ceil.median.wt),
#                          sd   = sqrt(wtd.var(TermNum,ceil.median.wt)) ,
#                          se   = sd / sqrt(Count),
#                          metric="Ceiling Category Weighted"
# 
# ))
# 
# 
# # 
# # pd <- position_dodge(0.1) # move them .05 to the left and right
# # 
# # ggplot(tgc, aes(x=dose, y=len, colour=supp)) + 
# #     geom_errorbar(aes(ymin=len-se, ymax=len+se), width=.1, position=pd) 
# 
# ggplot(subset(TerminatedDurStat,!metric %in% c("Ceiling Weighted")),
#        aes(x=StartFY,y=mean,color=metric))+
#     geom_line()+
#         geom_point(aes(shape=metric))+
#     facet_grid( qDuration  ~.) +
#     scale_x_continuous("Contract Starting Fiscal Year")+
#     scale_y_continuous(label=percent)+
#     geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1)+
#     theme(legend.position="bottom") #, position=pd
# 
# ggplot(subset(TerminatedDurStat,!metric %in% c("Ceiling Weighted","Ceiling Category Weighted")),
#        aes(x=StartFY,y=mean,color=metric))+
#     geom_line()+
#         geom_point(aes(shape=metric))+
#     facet_grid( qDuration  ~., space = "free_y", scales="free_y") +
#     scale_x_continuous("Contract Starting Fiscal Year")+
#     scale_y_continuous(label=percent)+
#     geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1)+
#     theme(legend.position="bottom") #, position=pd
# 
# 
```



```r
# TerminatedDur.SimpleStatCount<-ddply(subset(def_all,
#                                 !is.na(qDuration) & StartFY>=2007 & 
#                                     StartFY<=2014 & 
#                                     !is.na(Ceil) &
#                                     UnmodifiedCurrentCompletionDate<as.Date("2015-09-30")&
#                                     !is.na(Term)),
#                          .(Dur.Simple,
#                            StartFY,
#                            Term
#                          ),
#                          
#                          dplyr::summarise,
#                          Action_Obligation_OMB20_GDP18=sum(Action_Obligation_OMB20_GDP18),
#                          Count=length(CSIScontractID)
# )
# 
# ggplot(TerminatedDur.SimpleStatCount,
#        aes(x=StartFY,y=Count,color=Term))+
#     geom_line()+
#     geom_point(aes(shape=Term))+
#     facet_grid( Dur.Simple  ~.) +
#     scale_x_continuous("Contract Starting Fiscal Year")+
#     scale_y_log10("Number of Contracts",label=comma)
#     geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1)+
#     # theme(legend.position="bottom") #, position=pd
# 
# ggplot(TerminatedDur.SimpleStatCount,
#        aes(x=StartFY,y=Count,color=Term))+
#     geom_line()+
#         geom_point(aes(shape=Term))+
#     facet_grid( Dur.Simple  ~., ) +#
#     scale_x_continuous("Contract Starting Fiscal Year")+
#         
#     scale_y_log10("Number of Contracts (Variable Scale)",label=comma)
#     # geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1)+
#     theme(legend.position="bottom") #, position=pd
#     
```


```r
# ddply(TerminatedDurStat,
#       .(qDuration),
#       dplyr::summarise,
#       Count=sum(Count),
#       Action_Obligation_OMB20_GDP18=sum(Action_Obligation_OMB20_GDP18))
# 
# 
# 
# TerminatedDur.SimpleStat<-rbind(                         ddply(subset(def_all,
#                                 !is.na(qDuration) & StartFY>=2007 & 
#                                     StartFY<=2014 & 
#                                     !is.na(Ceil) &
#                                     UnmodifiedCurrentCompletionDate<as.Date("2015-09-30")&
#                                     !is.na(Term)),
#                          .(Dur.Simple,
#                            StartFY
#                          ),
#                          
#                          dplyr::summarise,
#                          Action_Obligation_OMB20_GDP18=sum(Action_Obligation_OMB20_GDP18),
#                          Count=length(CSIScontractID),
#                          mean = mean(TermNum),
#                          sd   = sd(TermNum),
#                          se   = sd / sqrt(Count),
#                          metric="Unweighted"
#                           # ceil.mean = wtd.mean(TermNum,UnmodifiedContractBaseAndAllOptionsValue),
#                          # ceil.cat.mean = wtd.mean(TermNum,ceil.median.wt)
# ))
# 
# 
# TerminatedDur.SimpleStat<-rbind(TerminatedDur.SimpleStat,
#                          ddply(subset(def_all,
#                                 !is.na(qDuration) & StartFY>=2007 & 
#                                     StartFY<=2014 & 
#                                     !is.na(Ceil) &
#                                     UnmodifiedCurrentCompletionDate<as.Date("2015-09-30")&
#                                     !is.na(Term)),
#                          .(Dur.Simple,
#                            StartFY
#                          ),
#                          
#                          dplyr::summarise,
#                          Action_Obligation_OMB20_GDP18=sum(Action_Obligation_OMB20_GDP18),
#                          Count=length(CSIScontractID),
#                          mean = wtd.mean(TermNum,ObligationWT),
#                          sd   = sqrt(wtd.var(TermNum,ObligationWT)) ,
#                          se   = sd / sqrt(Count),
#                          metric="Obligation Weighted"
#                           # ceil.mean = wtd.mean(TermNum,UnmodifiedContractBaseAndAllOptionsValue),
#                          # ceil.cat.mean = wtd.mean(TermNum,ceil.median.wt)
# ))
# 
# TerminatedDur.SimpleStat<-rbind(TerminatedDur.SimpleStat,
#                          ddply(subset(def_all,
#                                 !is.na(qDuration) & StartFY>=2007 & 
#                                     StartFY<=2014 & 
#                                     !is.na(Ceil) &
#                                     UnmodifiedCurrentCompletionDate<as.Date("2015-09-30")&
#                                     !is.na(Term)),
#                          .(Dur.Simple,
#                            StartFY
#                          ),
#                          
#                          dplyr::summarise,
#                          Action_Obligation_OMB20_GDP18=sum(Action_Obligation_OMB20_GDP18),
#                          Count=length(CSIScontractID),
#                          mean = wtd.mean(TermNum,UnmodifiedContractBaseAndAllOptionsValue),
#                          sd   = sqrt(wtd.var(TermNum,UnmodifiedContractBaseAndAllOptionsValue)) ,
#                          se   = sd / sqrt(Count),
#                          metric="Ceiling Weighted"
#                          # obl.mean = ,
#                          # ceil.mean = wtd.mean(TermNum,UnmodifiedContractBaseAndAllOptionsValue),
#                          # ceil.cat.mean = wtd.mean(TermNum,ceil.median.wt)
# 
# ))
# 
# 
# TerminatedDur.SimpleStat<-rbind(TerminatedDur.SimpleStat,
#                          ddply(subset(def_all,
#                                 !is.na(qDuration) & StartFY>=2007 & 
#                                     StartFY<=2014 & 
#                                     !is.na(Ceil) &
#                                     UnmodifiedCurrentCompletionDate<as.Date("2015-09-30")&
#                                     !is.na(Term)),
#                          .(Dur.Simple,
#                            StartFY
#                          ),
#                          
#                          dplyr::summarise,
#                          Action_Obligation_OMB20_GDP18=sum(Action_Obligation_OMB20_GDP18),
#                          Count=length(CSIScontractID),
#                          mean = wtd.mean(TermNum,ceil.median.wt),
#                          sd   = sqrt(wtd.var(TermNum,ceil.median.wt)) ,
#                          se   = sd / sqrt(Count),
#                          metric="Ceiling Category Weighted"
# 
# ))
# 
# 
# # 
# # pd <- position_dodge(0.1) # move them .05 to the left and right
# # 
# # ggplot(tgc, aes(x=dose, y=len, colour=supp)) + 
# #     geom_errorbar(aes(ymin=len-se, ymax=len+se), width=.1, position=pd) 
# 
# ggplot(subset(TerminatedDur.SimpleStat,!metric %in% c("Ceiling Weighted","Ceiling Category Weighted")),
#        aes(x=StartFY,y=mean,color=metric))+
#     geom_line()+
#     geom_point(aes(shape=metric))+
#     facet_grid( Dur.Simple  ~.) +
#     scale_x_continuous("Contract Starting Fiscal Year")+
#     scale_y_continuous(label=percent)+
#     geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1)+
#     theme(legend.position="bottom") #, position=pd
# 
# ggplot(subset(TerminatedDur.SimpleStat,!metric %in% c("Ceiling Weighted","Ceiling Category Weighted")),
#        aes(x=StartFY,y=mean,color=metric))+
#     geom_line()+
#         geom_point(aes(shape=metric))+
#     facet_grid( Dur.Simple  ~., space = "free_y", scales="free_y") +
#     scale_x_continuous("Contract Starting Fiscal Year")+
#     scale_y_continuous("Percent Terminated",label=percent)+
#     geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1)+
#     theme(legend.position="bottom")+ #, position=pd
```




```r
# TerminatedSDurSCeilStatCount<-ddply(subset(def_all,
#                                 !is.na(qDuration) & StartFY>=2007 & 
#                                     StartFY<=2014 & 
#                                     !is.na(Ceil) &
#                                     UnmodifiedCurrentCompletionDate<as.Date("2015-09-30")&
#                                     !is.na(Term)),
#                          .(Dur.Simple,
#                            Ceil,
#                            StartFY,
#                            Term
#                          ),
#                          
#                          dplyr::summarise,
#                          Action_Obligation_OMB20_GDP18=sum(Action_Obligation_OMB20_GDP18),
#                          Count=length(CSIScontractID)
# )
# 
# 
# 
# ggplot(TerminatedSDurSCeilStatCount,
#        aes(x=StartFY,y=Count,color=Term))+
#     geom_line()+
#     geom_point(aes(shape=Term))+
#     facet_grid( Dur.Simple  ~ Ceil) +
#     scale_x_continuous("Contract Starting Fiscal Year")+
#     scale_y_log10("Number of Contracts",label=comma)
#     # theme(legend.position="bottom") #, position=pd
# 
# ggplot(TerminatedSDurSCeilStatCount,
#        aes(x=StartFY,y=Count,color=Term))+
#     geom_line()+
#         geom_point(aes(shape=Term))+
#     facet_grid( Dur.Simple  ~ Ceil ) +#
#     scale_x_continuous("Contract Starting Fiscal Year")+
#         
#     scale_y_log10("Number of Contracts (Variable Scale)",label=comma)
#     # geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1)+
#     theme(legend.position="bottom") #, position=pd
#     
```

# Terminations


Contract terminations and the number of change orders can be calculated for the entire sample.  Contract termination is determined using the *Reason for Modification* field in FPDS.  A contract is considered to be terminated if it has at least one modification with the following values:

* "Terminate for Default (complete or partial)"
* "Terminate for Convenience (complete or partial)"
* "Terminate for Cause"
* "Legal Contract Cancellation"

These four catetegories and the "Close Out" category are used to mark a contract as closed.  Many contracts in FPDS and in the sample are never marked closed.  




## Termination Timeline



```r
# def_all<-as.data.frame(def_all)
# 
summary(def_all$qDuration)
```

```
##  [0 months,~2 months) [~2 months,~7 months)   [~7 months-~1 year] 
##                     0               3004049                877325 
##    (~1 year,~2 years]           (~2 years+]                  NA's 
##                633898                133886              11420731
```

```r
summary(def_all$Dur.Simple)
```

```
##           <~1 year (~1 year,~2 years]        (~2 years+] 
##            3881374             633898             133886 
##               NA's 
##           11420731
```

```r
summary((def_all %>% dplyr::filter(is.na(Dur.Simple)))$UnmodifiedDays)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##    1.00    2.00    6.00   13.08   22.00   60.96  206214
```

```r
# def_all$Dur.Simple<-def_all$qDuration
# 
# 
#     def_all$Dur.Simple<-as.character(def_all$qDuration)
#     def_all$Dur.Simple[def_all$Dur.Simple %in% c(
#       "[0 months,~2 months)",
#       "[~2 months,~7 months)",
#       "[~7 months-~1 year]")]<-"<~1 year"
#     def_all$qDuration.Simple<-factor(def_all$Dur.Simple,
#                                       levels=c("<~1 year",
#                                                "(~1 year,~2 years]",
#                                                "(~2 years+]"),
#                                       ordered=TRUE
#     )
# 
# def_all$Dur.Simple<-factor(factor(def_all$Dur.Simple))
# summary(TerminatedSDurSCeilStatCount$Ceil.Simple)
# summary(def_all$Ceil.Simple)
# summary(def_all$Ceil)
# 
# View(TerminatedSDurSCeilStatCount)
# write.csv(TerminatedSDurSCeilStatCount,"TerminatedSDurSCeilStatCount.csv")

# summary(def_all$qHighCeiling)
# if (all(levels(def_all$qHighCeiling)==c("[0.00e+00,1.50e+04)",
#                                              "[1.50e+04,1.00e+05)",
#                                              "[1.00e+05,1.00e+06)",
#                                              "[1.00e+06,1.00e+07)",
#                                              "[1.00e+07,7.50e+07)",
#                                              "[7.50e+07,3.36e+12]"))){
#       def_all$qHighCeiling<-factor(def_all$qHighCeiling,
# 
#                                     levels=c("[0.00e+00,1.50e+04)",
#                                              "[1.50e+04,1.00e+05)",
#                                              "[1.00e+05,1.00e+06)",
#                                              "[1.00e+06,1.00e+07)",
#                                              "[1.00e+07,7.50e+07)",
#                                              "[7.50e+07,3.36e+12]"),
#                                     labels=c("[0,15k)",
#                                              "[15k,100k)",
#                                              "[100k,1m)",
#                                              "[1m,10m)",
#                                              "[10m,75m)",
#                                              "[75m+]"),
#                                     ordered=TRUE
#       )
#     }
# 
# 
# 
#  if (identical(levels(def_all$qHighCeiling),c("[0,15k)",
#                                                   "[15k,100k)",
#                                                   "[100k,1m)",
#                                                   "[1m,10m)",
#                                                   "[10m,75m)",
#                                                   "[75m+]"
#     ))){
#       def_all$Ceil.Simple<-def_all$qHighCeiling
#       levels(def_all$Ceil.Simple)<- list("0k - <100k"=c("[15k,100k)",
#                                                          "[0,15k)"),
#                                           "100k - <10m"=c("[1m,10m)",
#                                                           "[100k,1m)"),
#                                           "10m+"=c("[75m+]",
#                                                    "[10m,75m)"))
# 
#       def_all$Ceil.Big<-def_all$Ceil
#       levels(def_all$Ceil.Big)<- list("0k - <100k"=c("[15k,100k)",
#                                                       "[0,15k)"),
#                                        "100k - <10m"=c("[1m,10m)",
#                                                        "[100k,1m)"),
#                                        "10m - <75m"=c("[10m,75m)"),
#                                        "75m+"=c("[75m+]"))
# }
# summary(def_all$Ceil.Simple)

TerminatedSDurSCeilStatCount<-
  only_complete(def_all) %>%
  group_by(Dur.Simple,
      Ceil.Simple,
      StartFY,
      Term
    ) %>%
    dplyr::summarise(
    Action_Obligation_OMB20_GDP18=sum(Action_Obligation_OMB20_GDP18),
    Count=length(CSIScontractID),
    metric="Contracts within Period"
)
```

```
## Warning: Factor `Dur.Simple` contains implicit NA, consider using
## `forcats::fct_explicit_na`
```

```r
TerminatedSDurSCeilStatCount<-rbind(TerminatedSDurSCeilStatCount,
                                    all_labeled(def_all) %>%                
                                    group_by(Dur.Simple,
                                      Ceil.Simple,
                                      StartFY,
                                      Term
                                    ) %>%
                                    dplyr::summarise(
                                    Action_Obligation_OMB20_GDP18=sum(Action_Obligation_OMB20_GDP18),
                                    Count=length(CSIScontractID),
                                    metric="Early Results for All Contracts"
))
```

```
## Warning: Factor `Dur.Simple` contains implicit NA, consider using
## `forcats::fct_explicit_na`

## Warning: Factor `Dur.Simple` contains implicit NA, consider using
## `forcats::fct_explicit_na`
```

```r
TerminatedSDurSCeilStatCount$metric<-factor(TerminatedSDurSCeilStatCount$metric,
                                            levels=c("Contracts within Period",
                                                   "Early Results for All Contracts"),
                                            ordered=TRUE)

TerminatedSDurSCeilStatCount$Term<-factor(TerminatedSDurSCeilStatCount$Term,
                                          levels=c("Unterminated",
                                                   "Terminated"),
                                          labels=c("Unterminated",
                                                   "Complete or Partial Termination"),
                                            ordered=TRUE)


TerminatedSDurSCeilLabels<-
    subset(TerminatedSDurSCeilStatCount,metric=="Contracts within Period") %>%
    group_by(Dur.Simple,Ceil.Simple) %>%
    dplyr::summarise(
    FacetCount=paste("Count:",prettyNum(sum(Count),big.mark=",")),
    FacetValue=paste(FacetCount,"\nObligated: $",round(sum(Action_Obligation_OMB20_GDP18)/1000000000,1),"B",sep="")
    )
```

```
## Warning: Factor `Dur.Simple` contains implicit NA, consider using
## `forcats::fct_explicit_na`

## Warning: Factor `Dur.Simple` contains implicit NA, consider using
## `forcats::fct_explicit_na`
```

```r
Ypos<-max(TerminatedSDurSCeilStatCount$Count)

(
Term_Plot<-ggplot(TerminatedSDurSCeilStatCount,
       aes(x=StartFY,y=Count,color=Term))+
    geom_line(aes(linetype=metric))+
    geom_point(aes(shape=Term))+
    geom_text(data=TerminatedSDurSCeilLabels,
              aes(x=2007,y=Ypos,label=FacetValue),
              # parse=TRUE,
              hjust=0,
              vjust=1,
              color="black")+
    facet_grid( Dur.Simple  ~ Ceil.Simple ) +#
    scale_x_continuous("Contract Starting Fiscal Year")+
    scale_color_manual("Status", values=c("blue","red"))+
    scale_linetype_discrete("Early Results")+
    scale_shape_discrete("Status")+
    scale_y_log10("Number of Contracts (Logorithmic Scale)",label=scales::comma)+
    # geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1)+
    theme(legend.position="bottom")+ #, position=pd
    labs(caption=paste("Source: FPDS and CSIS Analysis",missing_caption(def_all)))
)
```

![](Start_Year_Outcomes_files/figure-html/TermSDurSCeilCount-1.png)<!-- -->

```r
  # View(def_all %>% filter(is.na(Ceil.Simple)))

summary(def_all$StartFY        )
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    2007    2011    2015    2013    2016    2017
```

```r
ggplot(def_all,aes(x=na_non_positive_log(UnmodifiedDays)))+geom_histogram()
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

```
## Warning: Removed 206214 rows containing non-finite values (stat_bin).
```

![](Start_Year_Outcomes_files/figure-html/TermSDurSCeilCount-2.png)<!-- -->

```r
ggplot(subset(def_all,UnmodifiedDays<1),aes(x=UnmodifiedDays))+geom_histogram()
```

```
## Warning: Factor `qHighCeiling` contains implicit NA, consider using
## `forcats::fct_explicit_na`
```

![](Start_Year_Outcomes_files/figure-html/TermSDurSCeilCount-3.png)<!-- -->




```r
# summary(def_all$qDuration)
# 
# TerminatedDur.SimpleIntlStat<-rbind(                         ddply(subset(def_all,
#                                 !is.na(qDuration) & StartFY>=2007 & 
#                                     StartFY<=2014 & (LastCurrentCompletionDate<=strptime("2015-09-30","%Y-%m-%d") | IsClosed==1) &
#                                     !is.na(Ceil) &
#                                     !is.na(Intl) &
#                                     UnmodifiedCurrentCompletionDate<as.Date("2015-09-30")&
#                                     !is.na(Term)),
#                          .(Dur.Simple,
#                            StartFY,
#                            Intl
#                          ),
#                          
#                          dplyr::summarise,
#                          Action_Obligation_OMB20_GDP18=sum(Action_Obligation_OMB20_GDP18),
#                          Count=length(CSIScontractID),
#                          mean = mean(TermNum),
#                          sd   = sd(TermNum),
#                          se   = sd / sqrt(Count),
#                          metric="Unweighted"
#                           # ceil.mean = wtd.mean(TermNum,UnmodifiedContractBaseAndAllOptionsValue),
#                          # ceil.cat.mean = wtd.mean(TermNum,ceil.median.wt)
# ))
# 
# 
# 
# TerminatedDur.SimpleIntlStat<-rbind(TerminatedDur.SimpleIntlStat,
#                          ddply(subset(def_all,
#                                 !is.na(qDuration) & StartFY>=2007 & 
#                                     StartFY<=2014 & (LastCurrentCompletionDate<=strptime("2015-09-30","%Y-%m-%d") | IsClosed==1) &
#                                     !is.na(Ceil) &
#                                     !is.na(Intl) &
#                                     UnmodifiedCurrentCompletionDate<as.Date("2015-09-30")&
#                                     !is.na(Term)),
#                          .(Dur.Simple,
#                            StartFY,
#                            Intl
#                          ),
#                          
#                          dplyr::summarise,
#                          Action_Obligation_OMB20_GDP18=sum(Action_Obligation_OMB20_GDP18),
#                          Count=length(CSIScontractID),
#                          mean = wtd.mean(TermNum,ObligationWT),
#                          sd   = sqrt(wtd.var(TermNum,ObligationWT)) ,
#                          se   = sd / sqrt(Count),
#                          metric="Obligation Weighted"
#                           # ceil.mean = wtd.mean(TermNum,UnmodifiedContractBaseAndAllOptionsValue),
#                          # ceil.cat.mean = wtd.mean(TermNum,ceil.median.wt)
# ))
# 
# TerminatedDur.SimpleIntlStat<-rbind(TerminatedDur.SimpleIntlStat,
#                          ddply(subset(def_all,
#                                 !is.na(qDuration) & StartFY>=2007 & 
#                                     StartFY<=2014 & (LastCurrentCompletionDate<=strptime("2015-09-30","%Y-%m-%d") | IsClosed==1) &
#                                     !is.na(Ceil) &
#                                     !is.na(Intl) &
#                                     UnmodifiedCurrentCompletionDate<as.Date("2015-09-30")&
#                                     !is.na(Term)),
#                          .(Dur.Simple,
#                            StartFY,
#                            Intl
#                          ),
#                          
#                          dplyr::summarise,
#                          Action_Obligation_OMB20_GDP18=sum(Action_Obligation_OMB20_GDP18),
#                          Count=length(CSIScontractID),
#                          mean = wtd.mean(TermNum,UnmodifiedContractBaseAndAllOptionsValue),
#                          sd   = sqrt(wtd.var(TermNum,UnmodifiedContractBaseAndAllOptionsValue)) ,
#                          se   = sd / sqrt(Count),
#                          metric="Ceiling Weighted"
#                          # obl.mean = ,
#                          # ceil.mean = wtd.mean(TermNum,UnmodifiedContractBaseAndAllOptionsValue),
#                          # ceil.cat.mean = wtd.mean(TermNum,ceil.median.wt)
# 
# ))
# 
# 
# TerminatedDur.SimpleIntlStat<-rbind(TerminatedDur.SimpleIntlStat,
#                          ddply(subset(def_all,
#                                 !is.na(qDuration) & StartFY>=2007 & 
#                                     StartFY<=2014 & (LastCurrentCompletionDate<=strptime("2015-09-30","%Y-%m-%d") | IsClosed==1) &
#                                     !is.na(Ceil) &
#                                     !is.na(Intl) &
#                                     UnmodifiedCurrentCompletionDate<as.Date("2015-09-30")&
#                                     !is.na(Term)),
#                          .(Dur.Simple,
#                            StartFY,
#                            Intl
#                          ),
#                          
#                          dplyr::summarise,
#                          Action_Obligation_OMB20_GDP18=sum(Action_Obligation_OMB20_GDP18),
#                          Count=length(CSIScontractID),
#                          mean = wtd.mean(TermNum,ceil.median.wt),
#                          sd   = sqrt(wtd.var(TermNum,ceil.median.wt)) ,
#                          se   = sd / sqrt(Count),
#                          metric="Ceiling Category Weighted"
# 
# ))
# 
# 
# # 
# # pd <- position_dodge(0.1) # move them .05 to the left and right
# # 
# # ggplot(tgc, aes(x=dose, y=len, colour=supp)) + 
# #     geom_errorbar(aes(ymin=len-se, ymax=len+se), width=.1, position=pd) 
# 
# ddply(TerminatedDur.SimpleIntlStat,
#       .(Dur.Simple,
#         Intl,
#         metric),
#       dplyr::summarise,
#       Count=sum(Count),
#       Action_Obligation_OMB20_GDP18=sum(Action_Obligation_OMB20_GDP18))
# 
# TermLabels<-ddply(
#     subset(TerminatedDur.SimpleIntlStat,
#            !metric %in% c("Ceiling Weighted",
#                           "Ceiling Category Weighted")),
#     .(Dur.Simple,Intl,metric),
#     dplyr::summarise,
#     FacetCount=paste("Count:",prettyNum(sum(Count),big.mark=",")),
#     FacetValue=paste(FacetCount,"\nObligated: $",round(sum(Action_Obligation_OMB20_GDP18)/1000000000,1),"B",sep=""),
#     FacetY=max(mean+se))
# 
# TermLabels<-ddply(TermLabels,
#       .(Dur.Simple),
#       dplyr::mutate,
#       FacetY=max(FacetY))
# 
# ggplot(subset(TerminatedDur.SimpleIntlStat,!metric %in% c("Ceiling Weighted","Ceiling Category Weighted")),
#        aes(x=StartFY,y=mean,color=metric))+
#     geom_line()+
#         geom_point(aes(shape=metric))+
#     geom_text(data=TermLabels,
#               aes(x=2007,y=FacetY,label=FacetValue),
#               # parse=TRUE,
#               hjust=0,
#               vjust=1,
#               color="black")+
#     
#     facet_grid( Dur.Simple  ~ Intl, space = "free_y", scales="free_y") +
#     scale_x_continuous("Contract Starting Fiscal Year")+
#     scale_y_continuous("Percent Terminated",label=percent)+
#     geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1)+
#     theme(legend.position="bottom") #, position=pd
# 
# 
# 
# ggplot(subset(TerminatedDur.SimpleIntlStat,!metric %in% c("Ceiling Weighted","Ceiling Category Weighted")),
#        aes(x=StartFY,y=mean,color=metric))+
#     geom_line()+
#         geom_point(aes(shape=metric))+
#     geom_text(data=TermLabels,
#               aes(x=2007,y=FacetY,label=FacetValue),
#               # parse=TRUE,
#               hjust=0,
#               vjust=1,
#               color="black")+
#     facet_grid( Dur.Simple  ~ Intl, space = "free_y", scales="free_y") +
#     scale_x_continuous("Contract Starting Fiscal Year")+
#     scale_y_continuous("Percent Terminated",label=percent)+
#     geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1)+
#     theme(legend.position="bottom") #, position=pd
# 
```



```r
# 
# TerminatedDurCeilStat<-ddply(subset(def_all,
#                                 !is.na(qDuration) & StartFY>=2007 & 
#                                     !is.na(Ceil) &
#                                     UnmodifiedCurrentCompletionDate<as.Date("2015-09-30")&
#                                     !is.na(Term)),
#                          .(Ceil,
#                            qDuration,
#                            StartFY
#                          ),
#                          
#                          dplyr::summarise,
#                          Action_Obligation_OMB20_GDP18=sum(Action_Obligation_OMB20_GDP18),
#                          Count=length(CSIScontractID),
#                          N    = length(TermNum),
#                          mean = mean(TermNum),
#                          sd   = sd(TermNum),
#                          se   = sd / sqrt(N),
#                          obl.mean = wtd.mean(TermNum,ObligationWT,na.rm=TRUE),
#                          ceil.mean = wtd.mean(TermNum,UnmodifiedContractBaseAndAllOptionsValue)
# )
# # 
# # pd <- position_dodge(0.1) # move them .05 to the left and right
# # 
# # ggplot(tgc, aes(x=dose, y=len, colour=supp)) + 
# #     geom_errorbar(aes(ymin=len-se, ymax=len+se), width=.1, position=pd) 
# 
# ggplot(TerminatedDurCeilStat,aes(x=StartFY))+
#     geom_line(aes(y=mean))+
#     # geom_line(aes(y=ceil.mean))+
#     geom_line(aes(y=obl.mean))+
#     geom_point(aes(y=mean))+
#     facet_grid(Ceil ~ qDuration ) +
#     scale_x_continuous("Contract Starting Fiscal Year")+
#     scale_y_continuous("Percent Terminated",label=percent)+
#     geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1) #, position=pd
# 
# 
# ggplot(TerminatedDurCeilStat,
#        aes(x=StartFY,
#            y=obl.mean))+geom_line()+    geom_point()+facet_grid(Ceil ~ qDuration ) +
#     scale_x_continuous("Contract Starting Fiscal Year")+
#     scale_y_continuous("Percent Terminated",label=percent)
#          # geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1) #, position=pd
# 
# 
# ggplot(TerminatedDurCeilStat,
#        aes(x=StartFY,
#            y=ceil.mean))+geom_line()+    geom_point()+facet_grid(Ceil ~ qDuration ) +
#     scale_x_continuous("Contract Starting Fiscal Year")+
#     scale_y_continuous("Percent Terminated",label=percent)
#          # geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1) #, position=pd
# 
# 
# ```
# 
# 
# 
# 
# 
# ```{r FxCBcategories, fig.width=3,fig.height=9, dpi=600}
# 
# 
# 
# 
# TerminatedFxCb<-rbind(                         ddply(subset(def_all,
#                                 !is.na(FxCb) & StartFY>=2007 & 
#                                     StartFY<=2014 & (LastCurrentCompletionDate<=strptime("2015-09-30","%Y-%m-%d") | IsClosed==1) &
#                                     !is.na(Ceil) &
#                                     UnmodifiedCurrentCompletionDate<as.Date("2015-09-30")&
#                                     !is.na(Term)),
#                          .(FxCb,
#                            StartFY
#                          ),
#                          
#                          dplyr::summarise,
#                          Action_Obligation_OMB20_GDP18=sum(Action_Obligation_OMB20_GDP18),
#                          Count=length(CSIScontractID),
#                          mean = mean(TermNum),
#                          sd   = sd(TermNum),
#                          se   = sd / sqrt(Count),
#                          metric="Unweighted"
#                           # ceil.mean = wtd.mean(TermNum,UnmodifiedContractBaseAndAllOptionsValue),
#                          # ceil.cat.mean = wtd.mean(TermNum,ceil.median.wt)
# ))
# 
# 
# TerminatedFxCb<-rbind(TerminatedFxCb,
#                          ddply(subset(def_all,
#                                 !is.na(FxCb) & StartFY>=2007 & 
#                                     StartFY<=2014 & (LastCurrentCompletionDate<=strptime("2015-09-30","%Y-%m-%d") | IsClosed==1) &
#                                     !is.na(Ceil) &
#                                     UnmodifiedCurrentCompletionDate<as.Date("2015-09-30")&
#                                     !is.na(Term)),
#                          .(FxCb,
#                            StartFY
#                          ),
#                          
#                          dplyr::summarise,
#                          Action_Obligation_OMB20_GDP18=sum(Action_Obligation_OMB20_GDP18),
#                          Count=length(CSIScontractID),
#                          mean = wtd.mean(TermNum,ObligationWT),
#                          sd   = sqrt(wtd.var(TermNum,ObligationWT)) ,
#                          se   = sd / sqrt(Count),
#                          metric="Obligation Weighted"
#                           # ceil.mean = wtd.mean(TermNum,UnmodifiedContractBaseAndAllOptionsValue),
#                          # ceil.cat.mean = wtd.mean(TermNum,ceil.median.wt)
# ))
# 
# TerminatedFxCb<-rbind(TerminatedFxCb,
#                          ddply(subset(def_all,
#                                 !is.na(FxCb) & StartFY>=2007 & 
#                                     StartFY<=2014 & (LastCurrentCompletionDate<=strptime("2015-09-30","%Y-%m-%d") | IsClosed==1) &
#                                     !is.na(Ceil) &
#                                     UnmodifiedCurrentCompletionDate<as.Date("2015-09-30")&
#                                     !is.na(Term)),
#                          .(FxCb,
#                            StartFY
#                          ),
#                          
#                          dplyr::summarise,
#                          Action_Obligation_OMB20_GDP18=sum(Action_Obligation_OMB20_GDP18),
#                          Count=length(CSIScontractID),
#                          mean = wtd.mean(TermNum,UnmodifiedContractBaseAndAllOptionsValue),
#                          sd   = sqrt(wtd.var(TermNum,UnmodifiedContractBaseAndAllOptionsValue)) ,
#                          se   = sd / sqrt(Count),
#                          metric="Ceiling Weighted"
#                          # obl.mean = ,
#                          # ceil.mean = wtd.mean(TermNum,UnmodifiedContractBaseAndAllOptionsValue),
#                          # ceil.cat.mean = wtd.mean(TermNum,ceil.median.wt)
# 
# ))
# 
# 
# TerminatedFxCb<-rbind(TerminatedFxCb,
#                          ddply(subset(def_all,
#                                 !is.na(FxCb) & StartFY>=2007 & 
#                                     StartFY<=2014 & (LastCurrentCompletionDate<=strptime("2015-09-30","%Y-%m-%d") | IsClosed==1) &
#                                     !is.na(Ceil) &
#                                     UnmodifiedCurrentCompletionDate<as.Date("2015-09-30")&
#                                     !is.na(Term)),
#                          .(FxCb,
#                            StartFY
#                          ),
#                          
#                          dplyr::summarise,
#                          Action_Obligation_OMB20_GDP18=sum(Action_Obligation_OMB20_GDP18),
#                          Count=length(CSIScontractID),
#                          mean = wtd.mean(TermNum,ceil.median.wt),
#                          sd   = sqrt(wtd.var(TermNum,ceil.median.wt)) ,
#                          se   = sd / sqrt(Count),
#                          metric="Ceiling Category Weighted"
# 
# ))
# 
# 
# # 
# # pd <- position_dodge(0.1) # move them .05 to the left and right
# # 
# # ggplot(tgc, aes(x=dose, y=len, colour=supp)) + 
# #     geom_errorbar(aes(ymin=len-se, ymax=len+se), width=.1, position=pd) 
# 
# ggplot(subset(TerminatedFxCb,!metric %in% c("Ceiling Weighted","Ceiling Category Weighted")),
#        aes(x=StartFY,y=mean,color=metric))+
#     geom_line()+
#     geom_point(aes(shape=metric))+
#     facet_grid( FxCb  ~.) +
#     scale_x_continuous("Contract Starting Fiscal Year")+
#     scale_y_continuous(label=percent)+
#     geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1)+
#     theme(legend.position="bottom") #, position=pd
# 
# ggplot(TerminatedFxCb,#subset(TerminatedFxCb,!metric %in% c("Ceiling Weighted","Ceiling Category Weighted")),
#        aes(x=StartFY,y=mean,color=metric))+
#     geom_line()+
#         geom_point(aes(shape=metric))+
#     facet_grid( FxCb  ~., space = "free_y", scales="free_y") +
#     scale_x_continuous("Contract Starting Fiscal Year")+
#     scale_y_continuous("Percent Terminated",label=percent)+
#     geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1)+
#     theme(legend.position="bottom") #, position=pd
```
Contracts are classified using a mix of numerical and categorical variables. While the changes in numerical variables are easy to grasp and summarize, a contract may have one line item that is competed and another that is not. As is detailed in the exploration on R&D, we are only considering information available prior to contract start. The percentage of contract obligations that were competed is a valuable benchmark, but is highly influenced by factors that occured after contract start..

# Ceiling Breaches

In the same manner as contract terminations, change orders are reported in the *reason for modification* field.  There are two values that this study counts as change orders: "Change Order" and "Definitize Change Order."  For the remainder of this report, contracts with at least one change order are called **Changed Contracts**.  

There are also multiple modifications captured in FPDS that this current study will not investigate as change orders.  These include:

* Additional World (new agreement, FAR part 6 applies)
* Supplemental Agreement for work within scope
* Exercise an Option
* Definitize Letter Contract

In addition, there are a number of other modifications that may be undertaken based on changes on the government or vendor side that are not included in this analysis. 



**A histogram of the data** showing the distribution of the number of change orders each year from 2007.


```r
  NChgCeil<-ddply(def_all,
               .(SumOfisChangeOrder,
                 StartFY,
                 Ceil),
               plyr::summarise,
               ContractCount=length(CSIScontractID),
               Action_Obligation_OMB20_GDP18=sum(Action_Obligation_OMB20_GDP18))

NChgCeil<-ddply(NChgCeil, 
                .(Ceil), 
                transform, 
                pContractByCeil=ContractCount/sum(ContractCount),
                pObligationByCeil=Action_Obligation_OMB20_GDP18/sum(Action_Obligation_OMB20_GDP18))

NChgCeil$pTotalObligation<-NChgCeil$Action_Obligation_OMB20_GDP18/sum(NChgCeil$Action_Obligation_OMB20_GDP18,na.rm=TRUE)
NChgCeil$pTotalContract<-NChgCeil$ContractCount/sum(NChgCeil$ContractCount,na.rm=TRUE)
```


```r
# 
# ggplot(
#   data = subset(NChgCeil,SumOfisChangeOrder>0),
#   aes_string(x = "SumOfisChangeOrder")
#   ) + geom_bar(binwidth=1) + 
#     facet_grid( Ceil ~ .,
#                 scales = "free_y",
#                 space = "free_y") + scale_y_continuous(expand = c(0,50)) +scale_x_continuous(limits=c(0,10))
# 
# 
# 
# ggplot(
#   data = subset(NChgCeil,SumOfisChangeOrder>0),
#   aes_string(x = "Ceil",weight="ContractCount"),
#   main="Number of Contracts with Change Orders\nBy Initial Contract Ceiling")+ 
#   geom_bar()+
#     scale_x_discrete("Initial Cost Ceiling (Current $ Value)")+scale_y_continuous("Number of Contracts with Change Orders")+theme(axis.text.x=element_text(angle=90))
# 
# 
# ggplot(
#   data = subset(NChgCeil,SumOfisChangeOrder>0),
#   aes_string(x = "Ceil",weight="pContractByCeil"),
#   main="Percentage of Contracts going to Contracts with Change Orders\nBy Initial Contract Ceiling")+ geom_bar()+ scale_y_continuous("Percent of Contracts with Change Orders", labels=percent)+
#     scale_x_discrete("Initial Cost Ceiling (Current $ Value)")+theme(axis.text.x=element_text(angle=90))
# 
# 
# ggplot(
#   data =subset(NChgCeil,SumOfisChangeOrder>0),
#   aes_string(x = "Ceil",weight="pObligationByCeil"),
#   main="Percentage of Contract Obligations going to Contracts with Change Orders\nBy Initial Contract Ceiling"
#   )+ geom_bar()+ scale_y_continuous("Percent of Obligations in Cost Ceiling Category", labels=percent)+
#     scale_x_discrete("Initial Cost Ceiling (Current $ Value)")+theme(axis.text.x=element_text(angle=90))
# 
# 
# ggplot(
#   data = subset(NChgCeil,SumOfisChangeOrder>0),
#   aes_string(x = "Ceil",weight="Action_Obligation_OMB20_GDP18")
#   )+ geom_bar()+
#     scale_x_discrete("Initial Cost Ceiling (Current $ Value)")+scale_y_continuous("Total Obligated Value of Contracts with Change Orders")+theme(axis.text.x=element_text(angle=90))
# 
# 
# 
# sum(subset(NChgCeil,SumOfisChangeOrder>0)$pTotalObligation)
# sum(subset(NChgCeil,SumOfisChangeOrder>0)$pTotalContract)
```


This study uses changes in the *Base and All Options Value Amount* as a way of tracking the potential cost of change orders.

* The *Base and All Options Value Amount* refers to the ceiling of contract costs if all available options were exercised. 
* The *Base and Exercised Value Amount* is not used because contracts are often specified such that the bulk of the eventually executed contract in dollar terms are treated as options.  In these cases, the all-inclusive value provides a better baseline for tracking growth.  
* The *Action Obligation* refers to the actual amount transferred to vendors.  This study team does not use this value because spending for change orders are not necessarily front-loaded.  For example, a change to a contract in May of 2010 could easily result in payments from May 2010 through August 2013.

The % Growth in Base and All Options Value Amount form Change Orders is calculated as follows: 

*Base and All Options Value Amount* increases for all Change Order Modifications/
*Base and All Options Value Amount* from the original unmodified contract transaction


**A histogram of the data** showing the distribution of the initial amount of the specific change order 



```r
# 
# pChgCeil<-ddply(def_all,
#              .(pChange3Sig,
#                StartFY,
#                Ceil),
#              plyr::summarise,
#              ContractCount=length(CSIScontractID),
#              Action_Obligation_OMB20_GDP18=sum(Action_Obligation_OMB20_GDP18))
# 
# pChgCeil<-ddply(pChgCeil, 
#                 .(Ceil), 
#                 transform, 
#                 pContractByCeil=ContractCount/sum(ContractCount),
#                 pObligationByCeil=Action_Obligation_OMB20_GDP18/sum(Action_Obligation_OMB20_GDP18))
# 
# pChgCeil<-ddply(pChgCeil, 
#                 .(StartFY), 
#                 transform, 
#                 pContractByFYear=ContractCount/sum(ContractCount),
#                 pObligationByFYear=Action_Obligation_OMB20_GDP18/sum(Action_Obligation_OMB20_GDP18))
# 
# pChgCeil$pChange3Sig[pChgCeil$pChange3Sig==-Inf]<-NA
# pChgCeil$pChange3Sig[pChgCeil$pChange3Sig==Inf]<-NA
# 
# pChgCeilAverage<-ddply(pChgCeil,
#                 .(Ceil),
#                 plyr::summarise,
#                 mean = wtd.mean(pChange3Sig,ContractCount),
#                 sd   = sqrt(wtd.var(pChange3Sig,ContractCount))
#                 # se   = sd / sqrt(ContractCount)
#                 )
# 
# 
# 
# 
# pChgCeil$pTotalObligation<-pChgCeil$Action_Obligation_OMB20_GDP18/sum(NChgCeil$Action_Obligation_OMB20_GDP18,na.rm=TRUE)
# pChgCeil$pTotalContract<-pChgCeil$ContractCount/sum(NChgCeil$ContractCount,na.rm=TRUE)
# 
# pChgCeil$p_CBre <- cut2(
#     pChgCeil$pChange3Sig,c(
#                                               -0.001,
#                                               0.001,
#                                               0.15)
#     )
# 
# 
```


```r
# 
# ggplot(
#   data = pChgCeil,
#   aes_string(x = "pChange3Sig",
#              weights = "ContractCount")
#   ) + geom_histogram(binwidth=0.01) +
#     facet_grid( Ceil ~ .,
#                 scales = "free_y",
#                 space = "free_y") +
#     scale_y_log10("Number of Contracts")+
#     scale_x_continuous("Percentage of Cost-Ceiling-Raising Change Orders b
#                        y\nInitial Cost Ceiling (Current $ Value)",
#                        limits=c(-1.25,1.25), labels=percent)+
#     theme(axis.text.x=element_text(angle=90,size=1))+
#   geom_vline(data=pChgCeilAverage,aes(xintercept=mean),color="red")
# 
# 
# 
# 
# # ggplot(
# #   data = subset(pChgCeil,is.numeric(pChange3Sig)&is.finite(pChange3Sig)),
# #   aes_string(y = "pChange3Sig")
# #   ) + geom_boxplot() 
# 
# ggplot(
#   data = subset(pChgCeil,is.finite(pChange3Sig)&
#                   !is.na(pChange3Sig)&StartFY>2007&StartFY<=2014&pChange3Sig!=0),
#   aes(y = pChange3Sig,x=factor(StartFY),
#              weight = ContractCount)
#   ) + geom_violin() + 
#     facet_grid( Ceil ~ .) +
#     # scale_y_log10("Number of Contracts",limits=c(-1.25,1.25))+
#      scale_y_continuous(
#        "Cost-Ceiling-Raising Change Orders Percent (Current $ Value)",
#                        limits=c(-0.05,0.05), labels=percent)
#     # theme(axis.text.x=element_text(angle=90,size=1))
# 
# 
# 
# ggplot(
#   data = subset(pChgCeil,is.finite(pChange3Sig)&
#                   !is.na(pChange3Sig)&StartFY>2007&StartFY<=2014),
#   aes(y = pChange3Sig,x=factor(StartFY),
#              weight = ContractCount)
#   ) + geom_boxplot(outlier.shape = NA,notch=TRUE) + 
#     facet_grid( Ceil ~ .) +
#     # scale_y_log10("Number of Contracts",limits=c(-1.25,1.25))+
#      scale_y_continuous(
#        "Cost-Ceiling-Raising Change Orders Percent (Current $ Value)",
#                        limits=c(-0.05,0.05), labels=percent)
#     # theme(axis.text.x=element_text(angle=90,size=1))
# 
# 
# # Percent of Contracts breakdown by StartYear
# ggplot(
#   data = subset(pChgCeil,
#                 StartFY>=2007 & 
#                   StartFY<=2015 &
#                   pChange3Sig!=0),
#   aes_string(x = "pChange3Sig",
#              weight="pContractByFYear")
#   ) + geom_histogram(binwidth=0.01) +
#   scale_x_continuous("Percentage of Cost-Ceiling-Raising Change Orders b
#                        y\nInitial Cost Ceiling (Current $ Value)",
#                        limits=c(-1.25,1.25), labels=percent)+
#   scale_y_continuous()+
#   facet_wrap("StartFY")
# 
# 
# # Percent of Contracts breakdown by Ceiling
# ggplot(
#   data = subset(pChgCeil,pChange3Sig!=0),
#   aes_string(x = "pChange3Sig",weight="pContractByCeil",fill="p_CBre")#
#   )+ geom_histogram(binwidth=0.05)+
# #     scale_x_continuous("Percentage of Cost-Ceiling-Raising Change Orders by\nInitial Cost Ceiling (Current $ Value)")
#     scale_y_continuous("Percent of Contracts", labels=percent)+
#         facet_grid( . ~ Ceil )+scale_x_continuous("Extent of Ceiling Breach in 5% Increments",limits=c(-0.5,1), labels=percent)+theme(axis.text.x=element_text(angle=90),legend.position="bottom")+scale_fill_discrete(name="Extent of Ceiling Breach")
# 
# 
# 
# tapply(pChgCeil$pChange3Sig, pChgCeil$Ceil, summary)
# 
# 
# 
# 
# #Percent of obligations breakdown
# ggplot(
#   data = subset(pChgCeil,pChange3Sig!=0),
#   aes_string(x = "pChange3Sig",weight="pTotalObligation",fill="p_CBre")#
#   )+ geom_bar(binwidth=0.01)+
# #     scale_x_continuous("Percentage of Obligations  by\nInitial Cost Ceiling (Current $ Value)")
#     scale_y_continuous("Percent of Completed Contracts\n(Weighted by Current $ Obligations)", labels=percent)+
#        # facet_grid( . ~ Term )+
#     scale_x_continuous("Extent of Ceiling Breach \n(Percent Change in Current $ Value in 1% Increments)",labels=percent,limits=c(-0.5,1))+
#     coord_cartesian(xlim=c(-0.5,1))+ theme(axis.text.x=element_text(angle=90),legend.position="bottom")+
#     scale_fill_discrete(name="Extent of Ceiling Breach")
# 
# 
# tapply(pChgCeil$p_CBre, pChgCeil$Ceil, summary)
# 
# 
# sum(subset(pChgCeil,pChange3Sig>0)$pTotalObligation)
# 
```




```r
# BreachSummary<-ddply(def_all,
#                      .(Ceil,
#                        pChange3Sig,
#                        SumOfisChangeOrder,
#                        p_CBre,
#                        Term),
#                      summarise,
#                      pContractByCeil=sum(pContractByCeil),
#                      pObligationByCeil=sum(pObligationByCeil),
#                      pTotalObligation=sum(pTotalObligation))
# 
# 
# 
# ddply(pChgCeil,.(Term,p_CBre),
#                      summarise,
#                      pTotalObligation=sum(pTotalObligation))
```

## Any Ceiling Breach


```r
BreachedSDurSCeilStatCount<-
  only_complete(def_all) %>%
  group_by(Dur.Simple,
      Ceil.Big,
      StartFY,
      CBre
    ) %>%
    dplyr::summarise(
    Action_Obligation_OMB20_GDP18=sum(Action_Obligation_OMB20_GDP18),
    Count=length(CSIScontractID),
    metric="Contracts within Period"
)
```

```
## Warning: Factor `Dur.Simple` contains implicit NA, consider using
## `forcats::fct_explicit_na`
```

```r
BreachedSDurSCeilStatCount<-rbind(BreachedSDurSCeilStatCount,
                                    all_labeled(def_all) %>%                
                                    group_by(Dur.Simple,
                                      Ceil.Big,
                                      StartFY,
                                      CBre
                                    ) %>%
                                    dplyr::summarise(
                                    Action_Obligation_OMB20_GDP18=sum(Action_Obligation_OMB20_GDP18),
                                    Count=length(CSIScontractID),
                                    metric="Early Results for All Contracts"
))
```

```
## Warning: Factor `Dur.Simple` contains implicit NA, consider using
## `forcats::fct_explicit_na`

## Warning: Factor `Dur.Simple` contains implicit NA, consider using
## `forcats::fct_explicit_na`
```

```r
BreachedSDurSCeilStatCount$metric<-factor(BreachedSDurSCeilStatCount$metric,
                                            levels=c("Contracts within Period",
                                                   "Early Results for All Contracts"),
                                            ordered=TRUE)

BreachedSDurSCeilLabels<-
    subset(BreachedSDurSCeilStatCount,metric=="Contracts within Period") %>%
    group_by(Dur.Simple,Ceil.Big) %>%
    dplyr::summarise(
    FacetCount=paste("Count:",prettyNum(sum(Count),big.mark=",")),
    FacetValue=paste(FacetCount,"\nObligated: $",round(sum(Action_Obligation_OMB20_GDP18)/1000000000,1),"B",sep="")
    )
```

```
## Warning: Factor `Dur.Simple` contains implicit NA, consider using
## `forcats::fct_explicit_na`

## Warning: Factor `Dur.Simple` contains implicit NA, consider using
## `forcats::fct_explicit_na`
```

```r
Ypos<-max(BreachedSDurSCeilStatCount$Count)


ggplot(BreachedSDurSCeilStatCount,
       aes(x=StartFY,y=Count,color=CBre))+
    geom_line(aes(linetype=metric))+
    geom_point(aes(shape=CBre))+
    geom_text(data=BreachedSDurSCeilLabels,
              aes(x=2007,y=Ypos,label=FacetValue),
              # parse=TRUE,
              hjust=0,
              vjust=1,
              color="black")+
    facet_grid( Dur.Simple  ~ Ceil.Big ) +#
    scale_x_continuous("Contract Starting Fiscal Year")+
    scale_color_manual("Status", values=c("blue","red"))+
    scale_linetype_discrete("Early Results")+
    scale_shape_discrete("Status")+
    scale_y_log10("Number of Contracts (Logorithmic Scale)",label=scales::comma)+
    # geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1)+
    theme(legend.position="bottom") #, position=pd
```

![](Start_Year_Outcomes_files/figure-html/CBreSDurSCeilCount-1.png)<!-- -->

```r
summary(def_all$StartFY
        )
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    2007    2011    2015    2013    2016    2017
```

```r
ggplot(def_all,aes(x=log(UnmodifiedDays)))+geom_histogram()
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

```
## Warning: Removed 206214 rows containing non-finite values (stat_bin).
```

![](Start_Year_Outcomes_files/figure-html/CBreSDurSCeilCount-2.png)<!-- -->

```r
ggplot(subset(def_all,UnmodifiedDays<1),aes(x=UnmodifiedDays))+geom_histogram()
```

```
## Warning: Factor `qHighCeiling` contains implicit NA, consider using
## `forcats::fct_explicit_na`
```

![](Start_Year_Outcomes_files/figure-html/CBreSDurSCeilCount-3.png)<!-- -->

```r
View(subset(def_all,Ceil.Big=="0k - <100k" & Dur.Simple=="(~2 years+]"))
```

## Ceiling Breach Quantile

```r
df.QCrai<-only_complete(def_all)%>%
      group_by(StartFY,
               Ceil,
               qDuration)%>%
      dplyr::summarise(
      X50 = quantile(p_CBre, probs = 0.5,na.rm=TRUE),
      X75 = quantile(p_CBre, probs = 0.75,na.rm=TRUE), 
      X80 = quantile(p_CBre, probs = 0.80,na.rm=TRUE), 
      X90 = quantile(p_CBre, probs = 0.90,na.rm=TRUE), 
      X95 = quantile(p_CBre, probs = 0.95,na.rm=TRUE),
      X99 = quantile(p_CBre, probs = 0.99,na.rm=TRUE),
      ContractCount=length(CSIScontractID),
             Action_Obligation_OMB20_GDP18=sum(Action_Obligation_OMB20_GDP18),
      metric="Contracts within Period")
```

```
## Warning: Factor `qDuration` contains implicit NA, consider using
## `forcats::fct_explicit_na`
```

```r
df.QCrai<-melt(df.QCrai,variable.name="Quantile",value.name="pCRai",measure.vars=c(
  "X50",
  "X75",
  "X80",
  "X90",
  "X95",
  "X99")
)

ggplot(df.QCrai,
       aes(x=StartFY,y=pCRai,color=Quantile))+
  geom_line()+
  scale_y_continuous(labels=percent)+
  facet_grid(Ceil~qDuration)+labs(title="All Six Quantiles")
```

![](Start_Year_Outcomes_files/figure-html/Quantile -1.png)<!-- -->

```r
ggplot(subset(df.QCrai,
                !Quantile %in% c("X99")),
       aes(x=StartFY,y=pCRai,color=Quantile))+
  geom_line()+
  facet_grid(Ceil~qDuration#,
             # scales="free_y",
             # space="free_y"
             )+
  scale_y_continuous(labels=percent)+
  labs(title="Five Quantiles (no 99%)")
```

![](Start_Year_Outcomes_files/figure-html/Quantile -2.png)<!-- -->

```r
#Test to see which percentiles register at all.
df.ecdf<-def_all %>%
      group_by(Ceil,
               qDuration)%>%
      dplyr::summarise(
      r001 = ecdf(p_CBre)(0.001),
      r01 = ecdf(p_CBre)(0.01),
      r05 = ecdf(p_CBre)(0.01)
)
```

```
## Warning: Factor `qDuration` contains implicit NA, consider using
## `forcats::fct_explicit_na`
```

```r
# df.ecdf<-subset(df.ecdf,StartFY>=2007&StartFY<=2014)

# test<-tapply(def_all, p_CBre, ecdf)
```
## Simple.Dur / Ceiling.Big


```r
df.QCrai.SDur<-only_complete(def_all) %>%
      group_by(StartFY,
               Ceil.Big,
               Dur.Simple) %>%
      dplyr::summarise(
      X50 = quantile(p_CBre, probs = 0.5,na.rm=TRUE),
      X75 = quantile(p_CBre, probs = 0.75,na.rm=TRUE), 
      X80 = quantile(p_CBre, probs = 0.80,na.rm=TRUE), 
      X90 = quantile(p_CBre, probs = 0.90,na.rm=TRUE), 
      X95 = quantile(p_CBre, probs = 0.95,na.rm=TRUE),
      X99 = quantile(p_CBre, probs = 0.99,na.rm=TRUE),
      ContractCount=length(CSIScontractID),
             Action_Obligation_OMB20_GDP18=sum(Action_Obligation_OMB20_GDP18),
      metric="Contracts within Period")
```

```
## Warning: Factor `Dur.Simple` contains implicit NA, consider using
## `forcats::fct_explicit_na`
```

```r
df.QCrai.SDur<-rbind(df.QCrai.SDur,
                all_labeled(def_all)%>%
      group_by(StartFY,
               Ceil.Big,
               Dur.Simple)%>%
      dplyr::summarise( 
      X50 = quantile(p_CBre, probs = 0.5,na.rm=TRUE),
      X75 = quantile(p_CBre, probs = 0.75,na.rm=TRUE), 
      X80 = quantile(p_CBre, probs = 0.80,na.rm=TRUE), 
      X90 = quantile(p_CBre, probs = 0.90,na.rm=TRUE), 
      X95 = quantile(p_CBre, probs = 0.95,na.rm=TRUE),
      X99 = quantile(p_CBre, probs = 0.99,na.rm=TRUE),
      ContractCount=length(CSIScontractID),
             Action_Obligation_OMB20_GDP18=sum(Action_Obligation_OMB20_GDP18),
      metric="Early Results for All Contracts")
)
```

```
## Warning: Factor `Dur.Simple` contains implicit NA, consider using
## `forcats::fct_explicit_na`
```

```r
df.QCrai.SDur<-melt(df.QCrai.SDur,
                      variable.name="Quantile",value.name="pCRai",measure.vars=c(
  "X50",
  "X75",
  "X80",
  "X90",
  "X95",
  "X99")
)

df.QCrai.SDur$Quantile<-factor(df.QCrai.SDur$Quantile,
  levels=c("X50",
  "X75",
  "X80",
  "X90",
  "X95",
  "X99"),
  labels=c("50th Percentile",
  "75th Percentile",
  "80th Percentile",
  "90th Percentile",
  "95th Percentile",
  "99th Percentile")
)

CRaiSDurCeilLabels<-ddply(
  subset(df.QCrai.SDur,Quantile=="50th Percentile" &
           metric=="Contracts within Period"),
    .(Dur.Simple,Ceil.Big),
    plyr::summarise,
    FacetCount=paste("Count:",prettyNum(sum(ContractCount),big.mark=",")),
    FacetValue=paste(FacetCount,"\nObligated: $",round(sum(Action_Obligation_OMB20_GDP18)/1000000000,1),"B",sep="")
    )

Ypos<-max(subset(df.QCrai.SDur,
                   !Quantile %in% c("99th Percentile")
                 )$pCRai,na.rm=TRUE)


CRaiOutput<-ggplot(subset(df.QCrai.SDur,
                !Quantile %in% c("99th Percentile",
                                 "75th Percentile")),
       aes(x=StartFY,y=pCRai,color=Quantile))+
  geom_line(aes(linetype=metric))+
  geom_point(aes(shape=Quantile))+
  geom_text(data=CRaiSDurCeilLabels,
              aes(x=2007,y=Ypos,label=FacetValue),
              # parse=TRUE,
              hjust=0,
              vjust=1,
              color="black")+
  facet_grid(Dur.Simple~Ceil.Big)+
               scale_y_continuous("Cost-Ceiling-Raising Change Orders Percent (Current $ Value)",
                                  labels=percent)+
  scale_x_continuous("Contract Starting Fiscal Year")+
  scale_linetype_discrete("Early Results")+
  theme(legend.position="bottom") #, position=pd

CRaiOutput
```

![](Start_Year_Outcomes_files/figure-html/QuantileSimpleDur-1.png)<!-- -->

```r
ggsave("CRaiOutput.png",
       CRaiOutput,
       width=8,
       height=7,
       dpi=600)

ggplot(subset(df.QCrai.SDur,
                # !Quantile %in% c("99th Percentile")
                !Ceil.Big %in% c("15k - <100k","0 - <15k")
              ),
       aes(x=StartFY,
           y=pCRai,
           color=Quantile))+
  geom_line(aes(linetype=metric))+
  facet_grid(Ceil.Big~Dur.Simple,
             scales="free_y",
             space="free_y")+
  scale_y_continuous(labels=percent)
```

![](Start_Year_Outcomes_files/figure-html/QuantileSimpleDur-2.png)<!-- -->

```r
#Test to see which percentiles register at all.
df.ecdf<-ddply(def_all,
      .(Ceil.Big,
               Dur.Simple),
      summarise, 
      r001 = ecdf(p_CBre)(0.001),
      r01 = ecdf(p_CBre)(0.01),
      r05 = ecdf(p_CBre)(0.01)
)

# df.ecdf<-subset(df.ecdf,StartFY>=2007&StartFY<=2014)


CRaiSDurCeilFYearSummary<-ddply(
  subset(df.QCrai.SDur,Quantile=="50th Percentile" &
           metric=="Contracts within Period"),
    .(Dur.Simple,Ceil.Big,StartFY),
    plyr::summarise,
    FacetCount=paste("Count:",prettyNum(sum(ContractCount),big.mark=",")),
    FacetValue=paste(FacetCount,"\nObligated: $",round(sum(Action_Obligation_OMB20_GDP18)/1000000000,1),"B",sep="")
    )

DurBoundary<-subset(def_all,Ceil=="75m+"&
         qDuration=="(~2 years+]"&
         StartFY==2013&
         UnmodifiedCurrentCompletionDate<as.Date("2015-09-30")
         )
```

```
## Warning: Factor `qHighCeiling` contains implicit NA, consider using
## `forcats::fct_explicit_na`
```
## Dur.Simple / Ceiling.Simple

```r
df.QCrai.SDur<-only_complete(def_all) %>%
      group_by(StartFY,
               Ceil.Simple,
               Dur.Simple) %>%
      dplyr::summarise(
      X50 = quantile(p_CBre, probs = 0.5,na.rm=TRUE),
      X75 = quantile(p_CBre, probs = 0.75,na.rm=TRUE), 
      X80 = quantile(p_CBre, probs = 0.80,na.rm=TRUE), 
      X90 = quantile(p_CBre, probs = 0.90,na.rm=TRUE), 
      X95 = quantile(p_CBre, probs = 0.95,na.rm=TRUE),
      X99 = quantile(p_CBre, probs = 0.99,na.rm=TRUE),
      ContractCount=length(CSIScontractID),
             Action_Obligation_OMB20_GDP18=sum(Action_Obligation_OMB20_GDP18),
      metric="Contracts within Period")
```

```
## Warning: Factor `Dur.Simple` contains implicit NA, consider using
## `forcats::fct_explicit_na`
```

```r
df.QCrai.SDur<-rbind(df.QCrai.SDur,
                all_labeled(def_all)%>%
      group_by(StartFY,
               Ceil.Simple,
               Dur.Simple)%>%
      dplyr::summarise( 
      X50 = quantile(p_CBre, probs = 0.5,na.rm=TRUE),
      X75 = quantile(p_CBre, probs = 0.75,na.rm=TRUE), 
      X80 = quantile(p_CBre, probs = 0.80,na.rm=TRUE), 
      X90 = quantile(p_CBre, probs = 0.90,na.rm=TRUE), 
      X95 = quantile(p_CBre, probs = 0.95,na.rm=TRUE),
      X99 = quantile(p_CBre, probs = 0.99,na.rm=TRUE),
      ContractCount=length(CSIScontractID),
             Action_Obligation_OMB20_GDP18=sum(Action_Obligation_OMB20_GDP18),
      metric="Early Results for All Contracts")
)
```

```
## Warning: Factor `Dur.Simple` contains implicit NA, consider using
## `forcats::fct_explicit_na`
```

```r
write.csv(df.QCrai.SDur,"df.QCrai.SDur.csv")

df.QCrai.SDur<-melt(df.QCrai.SDur,
                      variable.name="Quantile",value.name="pCRai",measure.vars=c(
  "X50",
  "X75",
  "X80",
  "X90",
  "X95",
  "X99")
)


df.QCrai.SDur$Quantile<-factor(df.QCrai.SDur$Quantile,
  levels=c("X50",
  "X75",
  "X80",
  "X90",
  "X95",
  "X99"),
  labels=c("50th Percentile",
  "75th Percentile",
  "80th Percentile",
  "90th Percentile",
  "95th Percentile",
  "99th Percentile")
)

CRaiSDurCeilLabels<-ddply(
  subset(df.QCrai.SDur,Quantile=="50th Percentile" &
           metric=="Contracts within Period"),
    .(Dur.Simple,Ceil.Simple),
    plyr::summarise,
    FacetCount=paste("Count:",prettyNum(sum(ContractCount),big.mark=",")),
    FacetValue=paste(FacetCount,"\nObligated: $",round(sum(Action_Obligation_OMB20_GDP18)/1000000000,1),"B",sep="")
    )

Ypos<-max(subset(df.QCrai.SDur,
                   !Quantile %in% c("99th Percentile")
                 )$pCRai,na.rm=TRUE)


CRaiOutput<-ggplot(subset(df.QCrai.SDur,
                !Quantile %in% c("99th Percentile",
                                 "75th Percentile")),
       aes(x=StartFY,y=pCRai,color=Quantile))+
  geom_line(aes(linetype=metric))+
  geom_point(aes(shape=Quantile))+
  geom_text(data=CRaiSDurCeilLabels,
              aes(x=2007,y=Ypos,label=FacetValue),
              # parse=TRUE,
              hjust=0,
              vjust=1,
              color="black")+
  facet_grid(Dur.Simple~Ceil.Simple)+
               scale_y_continuous("Cost-Ceiling-Raising Change Orders Percent (Current $ Value)",
                                  labels=percent)+
  scale_x_continuous("Contract Starting Fiscal Year")+
  scale_linetype_discrete("Early Results")+
  theme(legend.position="bottom") #, position=pd

CRaiOutput
```

![](Start_Year_Outcomes_files/figure-html/QuantileSimpleDurSimpleCeil-1.png)<!-- -->

```r
ggsave("CRaiOutput.png",
       CRaiOutput,
       width=8,
       height=7,
       dpi=600)

ggplot(subset(df.QCrai.SDur,
                # !Quantile %in% c("99th Percentile")
                !Ceil.Simple %in% c("15k - <100k","0 - <15k")
              ),
       aes(x=StartFY,
           y=pCRai,
           color=Quantile))+
  geom_line(aes(linetype=metric))+
  facet_grid(Ceil.Simple~Dur.Simple,
             scales="free_y",
             space="free_y")+
  scale_y_continuous(labels=percent)
```

![](Start_Year_Outcomes_files/figure-html/QuantileSimpleDurSimpleCeil-2.png)<!-- -->

```r
#Test to see which percentiles register at all.
df.ecdf<-ddply(def_all,
      .(Ceil.Simple,
               Dur.Simple),
      summarise, 
      r001 = ecdf(p_CBre)(0.001),
      r01 = ecdf(p_CBre)(0.01),
      r05 = ecdf(p_CBre)(0.01)
)

# df.ecdf<-subset(df.ecdf,StartFY>=2007&StartFY<=2014)


CRaiSDurCeilFYearSummary<-ddply(
  subset(df.QCrai.SDur,Quantile=="50th Percentile" &
           metric=="Contracts within Period"),
    .(Dur.Simple,Ceil.Simple,StartFY),
    plyr::summarise,
    FacetCount=paste("Count:",prettyNum(sum(ContractCount),big.mark=",")),
    FacetValue=paste(FacetCount,"\nObligated: $",round(sum(Action_Obligation_OMB20_GDP18)/1000000000,1),"B",sep="")
    )

DurBoundary<-subset(def_all,Ceil=="75m+"&
         qDuration=="(~2 years+]"&
         StartFY==2013&
         UnmodifiedCurrentCompletionDate<as.Date("2015-09-30")
         )
```

```
## Warning: Factor `qHighCeiling` contains implicit NA, consider using
## `forcats::fct_explicit_na`
```

```r
levels(def_all$Ceil.Simple)
```

```
## [1] "0k - <100k"  "100k - <10m" "10m+"
```

```r
def_all$StartFY
```

```
##     [1] 2012 2013 2013 2015 2016 2017 2017 2009 2009 2016 2015 2017 2013
##    [14] 2017 2013 2017 2010 2015 2015 2017 2016 2016 2017 2014 2017 2015
##    [27] 2016 2016 2016 2017 2016 2016 2015 2017 2009 2016 2016 2017 2015
##    [40] 2013 2017 2011 2013 2016 2013 2017 2017 2008 2010 2015 2015 2017
##    [53] 2017 2007 2013 2016 2015 2010 2015 2008 2010 2013 2017 2013 2016
##    [66] 2010 2015 2016 2017 2007 2011 2017 2009 2015 2017 2017 2009 2012
##    [79] 2007 2015 2013 2016 2017 2007 2011 2014 2015 2017 2010 2017 2016
##    [92] 2012 2017 2011 2014 2015 2016 2017 2012 2016 2017 2011 2015 2015
##   [105] 2011 2015 2007 2007 2015 2008 2009 2015 2016 2017 2017 2007 2017
##   [118] 2009 2011 2016 2013 2015 2009 2009 2015 2017 2007 2012 2015 2008
##   [131] 2009 2017 2009 2014 2017 2007 2011 2010 2007 2011 2011 2008 2011
##   [144] 2017 2017 2015 2016 2016 2015 2009 2016 2016 2015 2017 2012 2009
##   [157] 2011 2009 2013 2016 2012 2014 2016 2017 2009 2016 2007 2008 2014
##   [170] 2015 2015 2016 2011 2011 2012 2013 2016 2016 2013 2016 2017 2008
##   [183] 2017 2012 2017 2007 2010 2012 2014 2014 2016 2013 2015 2017 2017
##   [196] 2014 2015 2016 2012 2008 2012 2014 2017 2011 2009 2012 2014 2016
##   [209] 2008 2011 2017 2017 2007 2009 2017 2008 2016 2017 2016 2008 2017
##   [222] 2017 2017 2015 2012 2012 2014 2015 2017 2011 2015 2016 2015 2016
##   [235] 2017 2010 2016 2016 2017 2016 2017 2008 2014 2015 2014 2016 2015
##   [248] 2015 2010 2015 2015 2017 2009 2013 2013 2014 2009 2008 2015 2010
##   [261] 2012 2017 2016 2012 2016 2017 2009 2011 2010 2016 2010 2017 2012
##   [274] 2009 2010 2013 2013 2015 2008 2011 2014 2015 2008 2017 2008 2013
##   [287] 2013 2012 2014 2014 2015 2017 2012 2010 2016 2007 2012 2016 2016
##   [300] 2016 2017 2008 2016 2015 2017 2017 2011 2013 2014 2016 2011 2013
##   [313] 2015 2016 2007 2010 2013 2012 2011 2015 2016 2017 2008 2017 2017
##   [326] 2016 2017 2012 2012 2009 2010 2016 2009 2013 2015 2017 2008 2010
##   [339] 2008 2008 2010 2013 2015 2007 2015 2015 2016 2009 2008 2015 2012
##   [352] 2017 2008 2011 2009 2012 2016 2010 2014 2016 2017 2009 2011 2010
##   [365] 2007 2015 2017 2017 2017 2012 2013 2017 2011 2011 2011 2017 2015
##   [378] 2017 2017 2007 2008 2011 2014 2016 2015 2010 2012 2007 2016 2017
##   [391] 2017 2014 2016 2017 2011 2013 2017 2015 2010 2016 2017 2013 2015
##   [404] 2009 2016 2017 2010 2007 2016 2016 2016 2015 2017 2017 2010 2009
##   [417] 2016 2016 2012 2015 2007 2012 2017 2008 2014 2017 2008 2016 2015
##   [430] 2017 2009 2016 2017 2009 2014 2012 2008 2013 2014 2015 2015 2016
##   [443] 2016 2011 2014 2015 2016 2016 2008 2010 2012 2011 2017 2013 2015
##   [456] 2011 2013 2015 2012 2015 2016 2017 2016 2016 2017 2017 2015 2016
##   [469] 2009 2008 2007 2012 2011 2014 2016 2016 2016 2017 2009 2016 2012
##   [482] 2015 2016 2009 2014 2015 2015 2015 2016 2016 2012 2011 2011 2015
##   [495] 2011 2008 2016 2016 2012 2012 2012 2015 2016 2017 2007 2016 2017
##   [508] 2017 2008 2010 2011 2011 2016 2016 2017 2012 2014 2017 2017 2009
##   [521] 2007 2011 2017 2007 2016 2017 2007 2016 2017 2017 2012 2007 2017
##   [534] 2007 2011 2017 2012 2016 2016 2011 2015 2016 2017 2017 2007 2015
##   [547] 2016 2016 2017 2015 2016 2009 2011 2017 2007 2014 2017 2007 2010
##   [560] 2014 2015 2016 2017 2008 2013 2014 2010 2011 2017 2008 2012 2008
##   [573] 2015 2009 2010 2015 2015 2017 2017 2015 2012 2016 2016 2017 2007
##   [586] 2010 2015 2016 2015 2016 2016 2016 2016 2017 2012 2015 2016 2016
##   [599] 2010 2015 2017 2011 2008 2009 2016 2017 2017 2008 2009 2016 2016
##   [612] 2015 2016 2016 2013 2016 2016 2016 2016 2007 2016 2011 2017 2017
##   [625] 2010 2010 2010 2013 2014 2015 2016 2015 2007 2016 2017 2017 2017
##   [638] 2017 2016 2011 2016 2009 2010 2016 2017 2016 2007 2015 2017 2014
##   [651] 2016 2016 2017 2017 2011 2011 2016 2017 2015 2014 2016 2015 2012
##   [664] 2013 2016 2017 2017 2008 2015 2009 2017 2012 2011 2015 2016 2010
##   [677] 2013 2016 2017 2009 2016 2016 2017 2016 2017 2015 2012 2007 2012
##   [690] 2013 2016 2016 2017 2010 2016 2008 2010 2015 2012 2009 2015 2017
##   [703] 2017 2016 2017 2011 2014 2017 2017 2017 2008 2010 2017 2010 2010
##   [716] 2016 2015 2017 2017 2017 2012 2016 2010 2017 2017 2017 2013 2016
##   [729] 2015 2016 2011 2013 2015 2015 2017 2010 2014 2014 2012 2010 2013
##   [742] 2015 2016 2016 2015 2017 2008 2017 2015 2007 2008 2012 2013 2016
##   [755] 2014 2016 2017 2015 2017 2017 2016 2017 2013 2011 2016 2017 2016
##   [768] 2016 2015 2008 2012 2009 2009 2017 2007 2014 2011 2011 2009 2015
##   [781] 2015 2009 2008 2015 2016 2016 2017 2009 2012 2015 2017 2014 2011
##   [794] 2011 2017 2007 2012 2015 2017 2009 2016 2016 2017 2009 2008 2007
##   [807] 2007 2011 2016 2016 2014 2010 2010 2015 2017 2008 2016 2016 2008
##   [820] 2013 2009 2014 2015 2015 2015 2017 2014 2015 2014 2011 2008 2016
##   [833] 2016 2010 2009 2016 2016 2016 2013 2007 2010 2010 2015 2015 2017
##   [846] 2012 2012 2014 2015 2008 2013 2008 2008 2013 2017 2017 2016 2009
##   [859] 2007 2016 2017 2008 2017 2010 2009 2013 2014 2015 2015 2016 2017
##   [872] 2010 2015 2015 2014 2017 2017 2008 2007 2008 2012 2014 2014 2014
##   [885] 2016 2015 2007 2010 2015 2016 2012 2009 2014 2015 2014 2017 2017
##   [898] 2016 2017 2009 2011 2008 2014 2015 2017 2014 2015 2015 2009 2015
##   [911] 2016 2017 2012 2017 2015 2017 2009 2009 2009 2014 2016 2017 2015
##   [924] 2007 2017 2016 2009 2010 2016 2017 2013 2014 2011 2017 2013 2017
##   [937] 2015 2016 2016 2017 2008 2010 2014 2015 2010 2016 2008 2015 2008
##   [950] 2008 2012 2014 2015 2016 2015 2017 2011 2015 2016 2016 2016 2017
##   [963] 2017 2017 2017 2014 2017 2017 2017 2008 2007 2013 2016 2009 2016
##   [976] 2017 2017 2011 2016 2017 2017 2011 2014 2016 2017 2014 2015 2007
##   [989] 2007 2011 2013 2015 2016 2008 2017 2010 2017 2009 2016 2017 2011
##  [1002] 2011 2015 2016 2015 2016 2008 2009 2011 2015 2016 2016 2017 2012
##  [1015] 2012 2008 2015 2016 2016 2015 2012 2015 2016 2017 2009 2007 2012
##  [1028] 2015 2017 2017 2017 2016 2016 2009 2015 2017 2017 2016 2016 2017
##  [1041] 2017 2011 2013 2014 2015 2014 2016 2016 2012 2010 2017 2013 2014
##  [1054] 2015 2015 2015 2017 2010 2015 2017 2017 2017 2007 2011 2008 2014
##  [1067] 2017 2010 2013 2013 2017 2010 2011 2015 2017 2013 2015 2013 2016
##  [1080] 2010 2010 2009 2014 2015 2008 2009 2008 2011 2013 2009 2016 2017
##  [1093] 2016 2017 2011 2016 2017 2008 2016 2009 2015 2017 2010 2015 2016
##  [1106] 2017 2017 2017 2008 2014 2012 2016 2013 2015 2012 2011 2010 2015
##  [1119] 2016 2016 2011 2017 2007 2014 2016 2007 2008 2012 2017 2017 2009
##  [1132] 2015 2017 2017 2010 2016 2015 2009 2017 2009 2017 2011 2014 2015
##  [1145] 2007 2017 2009 2011 2016 2013 2015 2015 2010 2009 2007 2007 2008
##  [1158] 2009 2017 2010 2014 2016 2015 2016 2015 2010 2012 2015 2016 2010
##  [1171] 2015 2016 2007 2013 2007 2009 2013 2017 2012 2011 2015 2015 2017
##  [1184] 2014 2012 2014 2017 2017 2008 2017 2015 2009 2015 2017 2017 2016
##  [1197] 2017 2014 2016 2010 2010 2014 2016 2010 2009 2009 2017 2017 2008
##  [1210] 2008 2009 2011 2015 2014 2016 2016 2008 2015 2007 2017 2010 2012
##  [1223] 2016 2017 2017 2017 2008 2008 2016 2015 2017 2017 2010 2016 2009
##  [1236] 2016 2011 2017 2017 2007 2013 2012 2007 2015 2016 2008 2014 2014
##  [1249] 2017 2017 2017 2007 2010 2015 2016 2017 2017 2017 2007 2014 2014
##  [1262] 2017 2016 2015 2015 2009 2015 2016 2016 2008 2017 2012 2013 2015
##  [1275] 2016 2017 2017 2007 2016 2017 2016 2017 2009 2014 2017 2012 2008
##  [1288] 2010 2014 2016 2011 2014 2017 2010 2017 2012 2016 2011 2008 2009
##  [1301] 2016 2007 2013 2016 2015 2012 2017 2012 2016 2017 2009 2008 2011
##  [1314] 2013 2017 2009 2008 2015 2011 2016 2017 2017 2011 2008 2016 2016
##  [1327] 2016 2013 2007 2009 2015 2017 2008 2017 2008 2016 2016 2016 2017
##  [1340] 2017 2017 2017 2017 2013 2015 2016 2015 2015 2007 2013 2015 2009
##  [1353] 2014 2015 2016 2012 2016 2017 2008 2014 2015 2015 2011 2008 2009
##  [1366] 2009 2016 2017 2015 2015 2013 2016 2014 2015 2017 2017 2014 2016
##  [1379] 2010 2014 2016 2016 2012 2017 2016 2017 2009 2014 2007 2012 2016
##  [1392] 2015 2010 2015 2016 2015 2010 2014 2010 2013 2013 2015 2014 2015
##  [1405] 2011 2017 2013 2015 2016 2017 2009 2007 2007 2017 2015 2011 2015
##  [1418] 2015 2015 2016 2008 2010 2013 2015 2016 2016 2017 2017 2014 2015
##  [1431] 2015 2015 2014 2014 2014 2015 2016 2015 2008 2017 2013 2015 2011
##  [1444] 2015 2016 2013 2016 2017 2011 2007 2009 2015 2017 2010 2013 2015
##  [1457] 2009 2016 2017 2017 2015 2015 2012 2012 2016 2017 2008 2016 2016
##  [1470] 2010 2010 2016 2017 2011 2017 2009 2012 2012 2012 2015 2011 2016
##  [1483] 2010 2012 2017 2010 2015 2017 2015 2008 2015 2017 2017 2010 2015
##  [1496] 2011 2012 2017 2009 2017 2015 2016 2008 2017 2017 2008 2011 2009
##  [1509] 2017 2008 2012 2007 2011 2016 2016 2016 2015 2008 2010 2016 2011
##  [1522] 2012 2007 2008 2014 2016 2016 2015 2015 2007 2008 2008 2010 2015
##  [1535] 2013 2014 2015 2016 2008 2007 2013 2016 2017 2017 2017 2009 2017
##  [1548] 2012 2007 2013 2015 2017 2016 2011 2010 2013 2015 2007 2016 2012
##  [1561] 2007 2016 2016 2016 2007 2017 2017 2014 2017 2017 2013 2015 2015
##  [1574] 2017 2017 2011 2012 2015 2017 2013 2009 2014 2017 2017 2008 2015
##  [1587] 2017 2008 2008 2016 2013 2013 2009 2011 2011 2013 2015 2015 2016
##  [1600] 2017 2011 2014 2015 2016 2017 2015 2015 2015 2016 2017 2017 2016
##  [1613] 2007 2016 2015 2011 2015 2007 2017 2007 2011 2016 2017 2015 2015
##  [1626] 2008 2015 2017 2015 2016 2017 2013 2014 2017 2007 2015 2016 2016
##  [1639] 2015 2017 2008 2011 2008 2015 2015 2017 2015 2017 2017 2007 2016
##  [1652] 2015 2007 2008 2015 2017 2014 2015 2015 2016 2009 2009 2009 2016
##  [1665] 2016 2015 2016 2015 2009 2015 2012 2015 2011 2007 2011 2016 2015
##  [1678] 2017 2017 2012 2013 2015 2017 2012 2016 2009 2012 2008 2015 2007
##  [1691] 2010 2013 2009 2008 2015 2016 2016 2017 2015 2016 2016 2017 2007
##  [1704] 2007 2014 2015 2017 2015 2016 2015 2011 2017 2014 2007 2012 2010
##  [1717] 2007 2016 2016 2016 2015 2016 2016 2016 2008 2011 2016 2017 2017
##  [1730] 2016 2016 2015 2017 2016 2017 2008 2017 2017 2008 2009 2011 2015
##  [1743] 2016 2016 2007 2012 2016 2016 2009 2015 2017 2017 2017 2017 2010
##  [1756] 2009 2016 2017 2007 2013 2017 2009 2008 2014 2017 2012 2010 2008
##  [1769] 2016 2017 2017 2015 2015 2010 2015 2015 2015 2013 2014 2015 2012
##  [1782] 2009 2015 2017 2017 2016 2015 2017 2015 2016 2008 2010 2013 2015
##  [1795] 2008 2010 2017 2011 2015 2016 2011 2016 2017 2016 2017 2017 2008
##  [1808] 2007 2015 2017 2011 2016 2016 2015 2011 2015 2015 2014 2008 2015
##  [1821] 2015 2017 2016 2017 2010 2009 2008 2010 2017 2007 2007 2014 2016
##  [1834] 2016 2007 2011 2013 2017 2007 2014 2016 2017 2017 2017 2013 2015
##  [1847] 2015 2016 2016 2016 2016 2017 2007 2015 2017 2016 2007 2012 2007
##  [1860] 2017 2013 2012 2010 2012 2011 2017 2016 2010 2012 2017 2015 2016
##  [1873] 2015 2017 2010 2012 2011 2017 2015 2017 2009 2011 2008 2017 2017
##  [1886] 2016 2017 2007 2007 2014 2017 2008 2016 2017 2016 2016 2017 2014
##  [1899] 2012 2012 2017 2010 2013 2014 2015 2017 2013 2016 2015 2017 2010
##  [1912] 2011 2010 2009 2011 2016 2017 2012 2015 2017 2017 2017 2008 2015
##  [1925] 2013 2015 2017 2010 2008 2014 2016 2017 2007 2015 2015 2015 2016
##  [1938] 2016 2016 2017 2008 2016 2008 2017 2010 2011 2010 2010 2009 2015
##  [1951] 2017 2010 2017 2017 2015 2007 2016 2010 2010 2016 2012 2012 2014
##  [1964] 2016 2017 2007 2008 2011 2016 2017 2015 2008 2013 2016 2016 2013
##  [1977] 2017 2017 2015 2009 2016 2017 2017 2007 2007 2015 2016 2012 2008
##  [1990] 2016 2015 2008 2010 2012 2015 2016 2015 2015 2015 2016 2011 2014
##  [2003] 2008 2007 2015 2016 2012 2011 2017 2016 2017 2010 2015 2016 2017
##  [2016] 2009 2008 2008 2015 2017 2016 2017 2017 2017 2017 2017 2009 2007
##  [2029] 2009 2014 2016 2017 2008 2008 2015 2016 2012 2017 2010 2013 2015
##  [2042] 2017 2015 2016 2012 2011 2007 2015 2015 2015 2015 2017 2011 2016
##  [2055] 2016 2017 2017 2007 2009 2010 2014 2007 2016 2016 2017 2007 2010
##  [2068] 2014 2016 2007 2007 2010 2015 2015 2017 2007 2015 2007 2017 2008
##  [2081] 2013 2017 2017 2016 2013 2015 2017 2012 2011 2016 2017 2016 2017
##  [2094] 2009 2015 2015 2015 2011 2015 2015 2012 2013 2016 2010 2015 2010
##  [2107] 2007 2015 2015 2008 2010 2017 2015 2017 2016 2017 2011 2013 2015
##  [2120] 2017 2007 2009 2013 2016 2015 2012 2015 2016 2015 2016 2010 2016
##  [2133] 2017 2011 2016 2008 2015 2015 2016 2016 2016 2007 2007 2012 2008
##  [2146] 2015 2009 2012 2013 2015 2016 2016 2009 2012 2015 2016 2017 2007
##  [2159] 2016 2013 2010 2010 2010 2013 2016 2015 2010 2008 2016 2011 2016
##  [2172] 2007 2016 2017 2011 2008 2010 2009 2017 2017 2015 2017 2017 2015
##  [2185] 2008 2016 2017 2007 2016 2016 2013 2015 2016 2013 2015 2015 2016
##  [2198] 2012 2009 2009 2016 2017 2017 2017 2009 2011 2008 2011 2017 2015
##  [2211] 2016 2016 2012 2008 2016 2009 2013 2016 2016 2016 2017 2017 2016
##  [2224] 2016 2016 2008 2008 2017 2009 2012 2015 2015 2009 2015 2015 2017
##  [2237] 2013 2015 2010 2017 2009 2008 2010 2008 2016 2007 2015 2016 2016
##  [2250] 2011 2016 2017 2017 2017 2010 2010 2016 2008 2017 2016 2016 2016
##  [2263] 2010 2012 2015 2016 2010 2007 2015 2017 2015 2015 2016 2016 2017
##  [2276] 2014 2015 2017 2016 2015 2008 2007 2017 2017 2017 2017 2009 2007
##  [2289] 2011 2016 2007 2012 2016 2015 2017 2015 2016 2015 2012 2017 2017
##  [2302] 2008 2016 2008 2008 2007 2014 2015 2015 2007 2016 2012 2014 2016
##  [2315] 2017 2010 2011 2017 2017 2011 2016 2017 2017 2010 2016 2016 2016
##  [2328] 2016 2007 2012 2016 2017 2011 2009 2010 2017 2017 2014 2015 2013
##  [2341] 2014 2015 2009 2008 2007 2017 2015 2016 2015 2011 2014 2015 2016
##  [2354] 2016 2015 2010 2009 2012 2016 2017 2015 2017 2015 2012 2009 2008
##  [2367] 2013 2016 2016 2013 2015 2015 2011 2011 2007 2011 2008 2016 2011
##  [2380] 2015 2016 2016 2016 2015 2016 2017 2015 2012 2015 2016 2015 2014
##  [2393] 2017 2007 2016 2016 2017 2010 2007 2016 2016 2017 2009 2017 2017
##  [2406] 2014 2016 2015 2008 2012 2016 2017 2016 2017 2009 2016 2017 2017
##  [2419] 2010 2014 2015 2016 2016 2015 2010 2016 2016 2016 2017 2017 2007
##  [2432] 2015 2015 2013 2014 2016 2010 2015 2009 2014 2016 2013 2017 2017
##  [2445] 2011 2017 2008 2014 2016 2010 2007 2008 2008 2015 2015 2015 2014
##  [2458] 2015 2011 2013 2015 2017 2015 2015 2009 2012 2015 2012 2012 2015
##  [2471] 2009 2017 2009 2017 2017 2007 2009 2010 2014 2017 2017 2013 2016
##  [2484] 2008 2016 2013 2015 2009 2015 2016 2007 2013 2015 2009 2016 2012
##  [2497] 2015 2016 2016 2007 2011 2015 2015 2015 2016 2017 2017 2010 2008
##  [2510] 2017 2011 2011 2007 2009 2012 2007 2016 2017 2016 2009 2017 2016
##  [2523] 2017 2014 2017 2015 2007 2010 2015 2015 2009 2011 2012 2015 2012
##  [2536] 2014 2017 2013 2015 2016 2017 2017 2015 2016 2017 2017 2007 2007
##  [2549] 2015 2016 2008 2013 2013 2014 2014 2016 2011 2017 2017 2007 2008
##  [2562] 2009 2015 2012 2012 2016 2008 2010 2016 2017 2017 2009 2013 2013
##  [2575] 2015 2010 2015 2008 2008 2016 2008 2010 2016 2017 2017 2013 2016
##  [2588] 2015 2016 2017 2009 2016 2017 2009 2013 2016 2015 2008 2011 2009
##  [2601] 2017 2017 2007 2015 2016 2017 2015 2015 2015 2017 2015 2017 2008
##  [2614] 2008 2013 2017 2009 2008 2014 2016 2017 2017 2008 2009 2015 2017
##  [2627] 2007 2009 2007 2009 2007 2016 2017 2012 2017 2015 2014 2016 2008
##  [2640] 2017 2017 2017 2016 2010 2007 2015 2017 2017 2017 2017 2009 2009
##  [2653] 2015 2015 2016 2013 2013 2015 2017 2010 2014 2015 2017 2017 2012
##  [2666] 2015 2011 2010 2010 2011 2017 2008 2017 2017 2009 2013 2014 2016
##  [2679] 2016 2017 2007 2008 2015 2014 2014 2014 2015 2017 2016 2016 2017
##  [2692] 2008 2017 2008 2011 2011 2008 2008 2016 2013 2015 2016 2012 2007
##  [2705] 2007 2009 2017 2017 2009 2017 2015 2017 2016 2010 2016 2017 2017
##  [2718] 2007 2017 2007 2011 2016 2017 2012 2009 2007 2008 2016 2016 2008
##  [2731] 2017 2010 2009 2015 2017 2016 2016 2017 2015 2011 2016 2016 2016
##  [2744] 2007 2008 2017 2012 2012 2009 2017 2012 2015 2016 2016 2017 2010
##  [2757] 2013 2016 2011 2009 2009 2016 2016 2015 2017 2016 2017 2013 2015
##  [2770] 2015 2016 2007 2008 2010 2016 2007 2015 2016 2017 2008 2015 2015
##  [2783] 2015 2017 2010 2017 2017 2011 2011 2008 2012 2013 2015 2017 2017
##  [2796] 2012 2010 2015 2016 2017 2007 2012 2015 2017 2010 2016 2009 2013
##  [2809] 2013 2015 2015 2009 2008 2014 2017 2008 2015 2017 2007 2008 2016
##  [2822] 2015 2017 2017 2011 2007 2014 2009 2016 2017 2013 2016 2016 2013
##  [2835] 2016 2017 2009 2013 2013 2015 2015 2016 2017 2008 2013 2016 2016
##  [2848] 2015 2013 2017 2017 2010 2009 2016 2017 2017 2017 2007 2008 2016
##  [2861] 2017 2013 2009 2017 2017 2017 2017 2010 2012 2014 2012 2008 2016
##  [2874] 2015 2014 2017 2011 2012 2016 2013 2016 2017 2017 2015 2008 2010
##  [2887] 2007 2017 2017 2016 2016 2012 2014 2008 2017 2008 2010 2015 2016
##  [2900] 2010 2007 2008 2016 2016 2011 2008 2013 2017 2017 2009 2009 2007
##  [2913] 2015 2009 2013 2014 2017 2017 2015 2013 2007 2008 2014 2008 2015
##  [2926] 2007 2017 2008 2008 2017 2016 2016 2016 2017 2012 2009 2014 2015
##  [2939] 2016 2017 2009 2014 2017 2012 2016 2012 2016 2009 2009 2015 2011
##  [2952] 2015 2016 2016 2013 2015 2016 2016 2015 2017 2008 2014 2015 2016
##  [2965] 2016 2016 2012 2017 2008 2017 2007 2012 2017 2017 2008 2008 2015
##  [2978] 2016 2010 2010 2007 2017 2012 2015 2010 2015 2017 2009 2016 2016
##  [2991] 2007 2008 2008 2015 2016 2017 2014 2016 2017 2015 2009 2011 2017
##  [3004] 2017 2007 2016 2015 2017 2010 2010 2015 2015 2017 2010 2011 2015
##  [3017] 2016 2016 2017 2011 2017 2011 2014 2016 2008 2016 2017 2015 2015
##  [3030] 2014 2015 2016 2010 2011 2017 2017 2017 2011 2012 2013 2014 2015
##  [3043] 2010 2010 2015 2016 2014 2015 2007 2016 2007 2014 2010 2016 2017
##  [3056] 2010 2014 2015 2012 2016 2016 2008 2015 2016 2009 2008 2011 2013
##  [3069] 2011 2017 2011 2014 2017 2008 2008 2014 2015 2016 2012 2013 2013
##  [3082] 2016 2017 2008 2009 2015 2015 2015 2016 2016 2016 2011 2008 2015
##  [3095] 2017 2013 2017 2014 2015 2016 2016 2017 2012 2011 2012 2010 2015
##  [3108] 2009 2013 2016 2016 2015 2011 2017 2012 2016 2017 2008 2011 2014
##  [3121] 2015 2016 2015 2009 2017 2015 2011 2014 2016 2014 2015 2017 2011
##  [3134] 2008 2008 2011 2016 2016 2016 2015 2010 2012 2012 2016 2015 2012
##  [3147] 2015 2016 2015 2017 2007 2012 2015 2017 2010 2013 2015 2010 2013
##  [3160] 2015 2015 2016 2017 2015 2015 2015 2016 2017 2009 2010 2015 2015
##  [3173] 2017 2007 2010 2015 2010 2014 2016 2016 2015 2017 2017 2011 2009
##  [3186] 2012 2017 2013 2017 2007 2016 2016 2017 2015 2008 2012 2014 2017
##  [3199] 2010 2015 2011 2017 2011 2017 2017 2015 2015 2017 2007 2014 2015
##  [3212] 2016 2017 2009 2010 2014 2016 2007 2017 2014 2017 2015 2016 2007
##  [3225] 2009 2016 2011 2011 2007 2010 2015 2007 2007 2017 2017 2007 2007
##  [3238] 2017 2017 2008 2016 2016 2011 2013 2011 2008 2012 2007 2016 2017
##  [3251] 2015 2017 2015 2016 2007 2015 2017 2017 2017 2008 2013 2015 2016
##  [3264] 2017 2012 2012 2013 2017 2017 2015 2017 2017 2011 2008 2014 2009
##  [3277] 2017 2015 2010 2016 2017 2009 2014 2016 2015 2007 2009 2017 2017
##  [3290] 2017 2017 2009 2016 2016 2011 2012 2008 2015 2015 2017 2012 2013
##  [3303] 2017 2017 2017 2011 2014 2016 2011 2008 2014 2016 2017 2015 2017
##  [3316] 2010 2007 2015 2015 2016 2017 2017 2012 2010 2010 2010 2016 2016
##  [3329] 2017 2016 2016 2017 2015 2012 2013 2016 2016 2015 2010 2013 2017
##  [3342] 2011 2014 2017 2007 2017 2015 2015 2015 2016 2017 2011 2009 2014
##  [3355] 2009 2008 2013 2016 2015 2010 2012 2016 2016 2017 2017 2015 2010
##  [3368] 2011 2015 2016 2017 2015 2015 2016 2017 2017 2015 2016 2016 2016
##  [3381] 2017 2007 2010 2015 2013 2011 2016 2017 2017 2012 2007 2017 2008
##  [3394] 2016 2017 2008 2011 2015 2012 2007 2015 2017 2007 2008 2017 2007
##  [3407] 2014 2016 2007 2016 2016 2017 2015 2009 2011 2007 2017 2017 2011
##  [3420] 2016 2008 2017 2017 2017 2015 2015 2017 2013 2010 2009 2013 2015
##  [3433] 2016 2009 2015 2015 2010 2012 2007 2009 2014 2015 2012 2013 2008
##  [3446] 2015 2016 2010 2017 2007 2016 2017 2012 2008 2014 2016 2015 2007
##  [3459] 2016 2016 2011 2015 2016 2016 2015 2008 2010 2009 2008 2008 2011
##  [3472] 2010 2017 2017 2007 2013 2016 2015 2017 2013 2015 2015 2017 2009
##  [3485] 2007 2009 2015 2015 2016 2008 2013 2017 2011 2009 2011 2016 2016
##  [3498] 2014 2016 2016 2017 2008 2016 2017 2015 2015 2017 2017 2007 2017
##  [3511] 2007 2008 2017 2009 2013 2014 2007 2015 2015 2010 2012 2016 2017
##  [3524] 2007 2009 2017 2011 2014 2015 2016 2010 2015 2015 2014 2017 2016
##  [3537] 2017 2012 2008 2017 2009 2012 2017 2017 2011 2010 2017 2014 2015
##  [3550] 2009 2015 2017 2011 2015 2017 2009 2016 2017 2010 2015 2017 2008
##  [3563] 2008 2011 2016 2011 2014 2016 2016 2016 2015 2017 2017 2010 2015
##  [3576] 2015 2017 2011 2015 2017 2017 2012 2017 2010 2010 2016 2016 2017
##  [3589] 2016 2012 2014 2015 2009 2016 2015 2017 2014 2015 2007 2007 2016
##  [3602] 2007 2007 2015 2016 2017 2008 2012 2016 2017 2012 2014 2015 2017
##  [3615] 2011 2009 2009 2015 2013 2015 2016 2017 2017 2009 2017 2017 2017
##  [3628] 2011 2008 2007 2012 2016 2015 2014 2015 2017 2017 2010 2010 2015
##  [3641] 2016 2008 2017 2011 2015 2016 2016 2017 2016 2017 2017 2013 2015
##  [3654] 2016 2009 2016 2017 2016 2016 2016 2017 2017 2011 2016 2015 2009
##  [3667] 2009 2011 2010 2013 2016 2016 2015 2017 2012 2007 2015 2016 2017
##  [3680] 2012 2016 2009 2007 2014 2016 2017 2012 2011 2016 2017 2009 2010
##  [3693] 2014 2016 2017 2012 2015 2016 2017 2007 2015 2011 2012 2016 2012
##  [3706] 2014 2015 2011 2011 2014 2016 2017 2008 2012 2014 2016 2016 2017
##  [3719] 2017 2007 2011 2016 2011 2012 2011 2009 2014 2015 2017 2012 2012
##  [3732] 2007 2014 2016 2016 2016 2012 2009 2013 2017 2016 2010 2012 2010
##  [3745] 2009 2007 2015 2012 2012 2015 2017 2008 2007 2016 2015 2016 2016
##  [3758] 2015 2017 2016 2011 2008 2011 2010 2014 2017 2011 2015 2015 2016
##  [3771] 2016 2016 2013 2015 2007 2008 2013 2017 2012 2008 2015 2016 2015
##  [3784] 2009 2016 2017 2016 2015 2008 2016 2015 2014 2015 2008 2014 2015
##  [3797] 2017 2014 2007 2015 2016 2015 2016 2012 2008 2012 2013 2015 2016
##  [3810] 2015 2007 2007 2015 2015 2017 2017 2017 2017 2017 2009 2015 2017
##  [3823] 2017 2017 2015 2015 2016 2017 2016 2015 2015 2012 2015 2015 2015
##  [3836] 2017 2017 2008 2008 2013 2016 2016 2017 2017 2017 2017 2016 2011
##  [3849] 2016 2016 2010 2011 2013 2016 2017 2017 2017 2011 2016 2008 2008
##  [3862] 2015 2017 2016 2012 2015 2016 2016 2013 2017 2012 2011 2017 2008
##  [3875] 2014 2014 2015 2014 2015 2014 2016 2017 2015 2009 2015 2015 2017
##  [3888] 2012 2012 2014 2015 2016 2016 2016 2007 2011 2014 2015 2016 2017
##  [3901] 2014 2015 2015 2017 2009 2012 2007 2017 2017 2015 2017 2017 2010
##  [3914] 2015 2017 2016 2014 2017 2016 2010 2014 2017 2013 2016 2015 2016
##  [3927] 2017 2015 2007 2015 2017 2016 2007 2015 2016 2017 2017 2010 2015
##  [3940] 2016 2016 2013 2015 2012 2015 2015 2017 2011 2009 2016 2012 2016
##  [3953] 2010 2016 2010 2010 2010 2007 2015 2016 2017 2007 2013 2007 2007
##  [3966] 2010 2016 2016 2017 2017 2012 2017 2017 2017 2013 2015 2016 2012
##  [3979] 2014 2016 2016 2015 2010 2015 2015 2017 2015 2017 2007 2012 2016
##  [3992] 2014 2010 2011 2015 2012 2016 2016 2016 2007 2017 2013 2017 2016
##  [4005] 2017 2007 2013 2015 2016 2016 2014 2014 2015 2017 2011 2017 2017
##  [4018] 2009 2015 2016 2017 2009 2009 2013 2014 2016 2015 2010 2009 2015
##  [4031] 2017 2016 2008 2017 2011 2011 2016 2015 2011 2016 2016 2017 2015
##  [4044] 2008 2015 2008 2017 2017 2015 2015 2008 2011 2015 2016 2016 2017
##  [4057] 2009 2008 2008 2014 2015 2017 2017 2012 2008 2011 2017 2017 2017
##  [4070] 2015 2016 2016 2016 2017 2007 2015 2016 2010 2015 2016 2017 2016
##  [4083] 2016 2016 2017 2017 2007 2007 2016 2017 2016 2016 2008 2013 2011
##  [4096] 2008 2014 2013 2015 2017 2010 2012 2011 2016 2016 2017 2008 2017
##  [4109] 2017 2017 2017 2010 2008 2015 2012 2015 2015 2007 2009 2013 2017
##  [4122] 2012 2015 2017 2017 2008 2012 2017 2011 2010 2015 2010 2016 2010
##  [4135] 2015 2017 2015 2015 2015 2007 2015 2015 2015 2008 2015 2016 2015
##  [4148] 2016 2015 2010 2015 2015 2010 2008 2008 2016 2016 2017 2009 2016
##  [4161] 2017 2009 2009 2009 2015 2015 2015 2011 2009 2008 2015 2016 2012
##  [4174] 2016 2014 2016 2016 2011 2017 2013 2014 2015 2017 2009 2014 2016
##  [4187] 2017 2010 2013 2016 2016 2015 2017 2017 2013 2015 2017 2008 2013
##  [4200] 2014 2008 2017 2010 2016 2012 2014 2017 2007 2015 2017 2017 2017
##  [4213] 2016 2016 2015 2017 2012 2015 2017 2016 2017 2017 2010 2008 2015
##  [4226] 2017 2017 2007 2016 2011 2013 2017 2014 2016 2013 2017 2012 2015
##  [4239] 2016 2016 2008 2015 2016 2015 2016 2008 2009 2015 2016 2013 2015
##  [4252] 2017 2015 2016 2012 2009 2007 2011 2010 2016 2016 2012 2017 2009
##  [4265] 2012 2017 2015 2016 2016 2017 2012 2010 2015 2017 2015 2016 2015
##  [4278] 2017 2007 2007 2013 2012 2013 2016 2008 2008 2014 2017 2016 2011
##  [4291] 2015 2015 2008 2012 2016 2016 2011 2008 2013 2015 2011 2013 2013
##  [4304] 2015 2017 2017 2007 2007 2010 2016 2017 2009 2013 2016 2015 2015
##  [4317] 2017 2011 2017 2016 2017 2009 2007 2009 2016 2011 2016 2017 2009
##  [4330] 2007 2017 2007 2016 2011 2011 2013 2016 2013 2008 2015 2007 2017
##  [4343] 2016 2011 2015 2015 2011 2017 2016 2008 2008 2012 2016 2009 2008
##  [4356] 2012 2015 2016 2016 2017 2009 2017 2008 2015 2011 2007 2015 2017
##  [4369] 2011 2016 2015 2017 2017 2009 2011 2017 2012 2015 2016 2013 2015
##  [4382] 2016 2007 2014 2010 2009 2015 2015 2016 2008 2015 2017 2014 2016
##  [4395] 2015 2017 2016 2013 2015 2010 2010 2014 2015 2012 2010 2012 2013
##  [4408] 2011 2016 2013 2016 2017 2007 2011 2009 2015 2014 2015 2016 2017
##  [4421] 2007 2012 2015 2015 2015 2015 2007 2015 2017 2011 2015 2007 2015
##  [4434] 2015 2012 2015 2016 2015 2017 2017 2015 2014 2016 2015 2015 2016
##  [4447] 2015 2012 2017 2012 2008 2017 2009 2007 2007 2015 2017 2009 2013
##  [4460] 2016 2016 2016 2016 2013 2017 2017 2009 2013 2011 2017 2017 2009
##  [4473] 2008 2011 2017 2011 2014 2016 2016 2010 2011 2013 2013 2015 2016
##  [4486] 2017 2008 2017 2017 2007 2017 2007 2008 2009 2017 2015 2016 2010
##  [4499] 2015 2007 2008 2013 2016 2016 2008 2015 2017 2017 2007 2015 2013
##  [4512] 2012 2017 2017 2017 2012 2015 2017 2017 2015 2015 2017 2007 2015
##  [4525] 2015 2015 2017 2015 2015 2015 2012 2015 2007 2016 2017 2016 2016
##  [4538] 2009 2012 2015 2016 2012 2007 2016 2017 2011 2017 2017 2010 2011
##  [4551] 2010 2008 2011 2016 2016 2011 2011 2013 2015 2016 2016 2017 2017
##  [4564] 2015 2015 2015 2016 2017 2017 2015 2016 2009 2010 2008 2016 2017
##  [4577] 2017 2015 2016 2010 2013 2015 2016 2017 2009 2011 2013 2013 2014
##  [4590] 2012 2015 2010 2016 2014 2016 2009 2015 2015 2015 2016 2016 2007
##  [4603] 2009 2014 2016 2010 2015 2017 2014 2010 2017 2008 2013 2015 2010
##  [4616] 2015 2016 2017 2016 2009 2008 2015 2007 2016 2012 2014 2017 2017
##  [4629] 2017 2017 2011 2011 2015 2016 2017 2017 2011 2015 2007 2011 2010
##  [4642] 2008 2013 2014 2015 2016 2016 2017 2017 2017 2016 2010 2014 2007
##  [4655] 2014 2008 2010 2016 2009 2015 2010 2016 2007 2012 2007 2012 2007
##  [4668] 2017 2012 2015 2016 2017 2008 2009 2013 2015 2015 2007 2012 2010
##  [4681] 2014 2014 2007 2015 2008 2011 2017 2016 2016 2008 2007 2015 2016
##  [4694] 2008 2012 2015 2016 2011 2015 2017 2010 2012 2015 2017 2012 2012
##  [4707] 2015 2013 2012 2015 2014 2015 2016 2008 2017 2007 2015 2007 2017
##  [4720] 2015 2017 2007 2013 2017 2017 2015 2016 2011 2010 2012 2015 2016
##  [4733] 2015 2008 2015 2009 2013 2015 2016 2017 2011 2016 2017 2009 2010
##  [4746] 2007 2016 2017 2012 2014 2015 2016 2013 2015 2015 2015 2016 2016
##  [4759] 2017 2016 2017 2009 2017 2007 2007 2011 2007 2015 2016 2012 2009
##  [4772] 2011 2014 2016 2013 2015 2016 2010 2017 2017 2015 2014 2015 2008
##  [4785] 2008 2013 2014 2007 2007 2013 2017 2009 2017 2016 2014 2016 2016
##  [4798] 2015 2017 2011 2010 2017 2017 2008 2015 2017 2009 2016 2016 2017
##  [4811] 2016 2017 2007 2009 2017 2011 2008 2013 2016 2017 2007 2016 2011
##  [4824] 2015 2011 2017 2016 2017 2009 2015 2017 2017 2015 2007 2013 2011
##  [4837] 2015 2016 2016 2016 2017 2010 2013 2015 2017 2009 2007 2009 2017
##  [4850] 2008 2011 2011 2015 2010 2010 2017 2017 2017 2016 2017 2007 2017
##  [4863] 2012 2017 2012 2007 2017 2017 2017 2016 2009 2014 2017 2017 2009
##  [4876] 2016 2007 2008 2016 2017 2017 2017 2009 2016 2016 2017 2017 2010
##  [4889] 2015 2012 2009 2007 2007 2008 2015 2017 2009 2011 2016 2017 2007
##  [4902] 2017 2015 2016 2012 2015 2015 2015 2013 2014 2015 2016 2015 2008
##  [4915] 2008 2014 2016 2015 2016 2007 2015 2007 2011 2011 2008 2013 2014
##  [4928] 2016 2008 2013 2015 2008 2008 2013 2017 2007 2015 2012 2013 2015
##  [4941] 2016 2017 2009 2016 2017 2015 2017 2017 2008 2009 2015 2015 2010
##  [4954] 2009 2016 2012 2009 2015 2017 2017 2012 2017 2016 2008 2016 2017
##  [4967] 2017 2009 2009 2016 2009 2008 2017 2011 2015 2009 2014 2008 2011
##  [4980] 2009 2015 2016 2017 2017 2009 2013 2016 2016 2015 2017 2017 2009
##  [4993] 2013 2015 2016 2012 2017 2016 2008 2016 2017 2009 2016 2014 2015
##  [5006] 2013 2017 2017 2015 2007 2015 2007 2014 2015 2017 2007 2011 2010
##  [5019] 2007 2014 2015 2016 2016 2015 2015 2015 2015 2015 2016 2016 2016
##  [5032] 2017 2017 2012 2010 2017 2017 2017 2008 2013 2008 2015 2016 2017
##  [5045] 2010 2017 2013 2012 2012 2017 2017 2017 2016 2012 2015 2016 2017
##  [5058] 2015 2016 2015 2009 2008 2016 2017 2015 2017 2017 2017 2007 2015
##  [5071] 2007 2011 2011 2015 2015 2008 2012 2008 2015 2016 2012 2015 2015
##  [5084] 2015 2017 2012 2013 2015 2008 2016 2008 2012 2014 2015 2010 2009
##  [5097] 2013 2011 2016 2011 2016 2016 2017 2017 2016 2016 2013 2016 2016
##  [5110] 2007 2011 2017 2015 2015 2016 2017 2016 2007 2016 2007 2017 2016
##  [5123] 2012 2015 2017 2008 2013 2015 2011 2007 2015 2016 2015 2015 2015
##  [5136] 2017 2017 2015 2007 2009 2015 2017 2009 2014 2008 2017 2017 2013
##  [5149] 2014 2015 2009 2012 2015 2012 2013 2010 2009 2014 2015 2007 2009
##  [5162] 2017 2009 2012 2010 2012 2013 2015 2009 2016 2017 2017 2012 2008
##  [5175] 2016 2016 2017 2017 2010 2016 2015 2009 2009 2016 2015 2015 2015
##  [5188] 2017 2010 2012 2015 2010 2009 2013 2016 2017 2017 2012 2010 2010
##  [5201] 2011 2015 2016 2017 2010 2007 2013 2017 2008 2012 2015 2012 2007
##  [5214] 2008 2016 2017 2008 2008 2016 2010 2016 2016 2017 2017 2009 2016
##  [5227] 2017 2010 2015 2013 2007 2015 2016 2012 2009 2015 2015 2017 2014
##  [5240] 2015 2011 2009 2016 2008 2015 2015 2016 2016 2017 2010 2011 2016
##  [5253] 2012 2013 2017 2016 2017 2011 2016 2017 2016 2017 2010 2011 2013
##  [5266] 2017 2015 2015 2009 2012 2014 2010 2010 2013 2015 2017 2015 2016
##  [5279] 2017 2015 2016 2017 2015 2014 2016 2017 2017 2017 2017 2015 2011
##  [5292] 2016 2016 2016 2017 2012 2017 2009 2009 2014 2012 2016 2008 2008
##  [5305] 2016 2014 2015 2011 2013 2017 2015 2016 2015 2015 2017 2017 2017
##  [5318] 2011 2010 2015 2011 2015 2016 2017 2015 2013 2016 2013 2017 2009
##  [5331] 2013 2007 2016 2017 2015 2017 2010 2012 2012 2016 2017 2010 2017
##  [5344] 2007 2013 2015 2016 2010 2015 2015 2016 2010 2007 2017 2016 2017
##  [5357] 2017 2017 2012 2015 2007 2016 2015 2017 2008 2007 2017 2012 2017
##  [5370] 2011 2016 2016 2009 2016 2015 2016 2016 2016 2011 2016 2017 2017
##  [5383] 2015 2016 2015 2013 2015 2015 2017 2011 2007 2016 2016 2016 2015
##  [5396] 2015 2017 2016 2016 2017 2017 2015 2009 2008 2015 2015 2016 2016
##  [5409] 2016 2016 2009 2016 2017 2017 2009 2014 2015 2015 2016 2015 2016
##  [5422] 2016 2015 2007 2016 2016 2015 2010 2007 2012 2007 2007 2009 2016
##  [5435] 2007 2013 2016 2017 2015 2012 2016 2016 2010 2009 2016 2012 2011
##  [5448] 2008 2007 2010 2017 2017 2009 2009 2016 2007 2012 2012 2007 2011
##  [5461] 2013 2017 2007 2017 2017 2016 2009 2015 2017 2017 2008 2013 2016
##  [5474] 2015 2017 2015 2017 2011 2015 2016 2016 2016 2012 2017 2008 2014
##  [5487] 2017 2017 2012 2017 2011 2017 2015 2016 2010 2013 2017 2015 2015
##  [5500] 2015 2011 2011 2009 2016 2016 2009 2014 2016 2017 2017 2015 2017
##  [5513] 2017 2015 2016 2013 2016 2016 2015 2017 2009 2017 2010 2015 2011
##  [5526] 2008 2017 2017 2010 2007 2015 2016 2017 2017 2017 2017 2009 2009
##  [5539] 2012 2015 2016 2017 2017 2007 2008 2013 2015 2011 2015 2007 2017
##  [5552] 2017 2015 2009 2010 2017 2013 2009 2015 2015 2017 2017 2017 2008
##  [5565] 2009 2012 2012 2014 2017 2014 2012 2010 2015 2012 2008 2010 2016
##  [5578] 2009 2015 2016 2017 2016 2007 2016 2008 2016 2008 2012 2017 2012
##  [5591] 2010 2016 2017 2007 2017 2009 2016 2016 2017 2015 2016 2016 2016
##  [5604] 2007 2009 2011 2015 2007 2008 2009 2012 2016 2017 2008 2016 2016
##  [5617] 2017 2013 2017 2016 2015 2016 2007 2014 2016 2016 2017 2010 2013
##  [5630] 2011 2015 2017 2012 2010 2016 2015 2016 2011 2007 2015 2015 2016
##  [5643] 2016 2017 2016 2017 2015 2014 2016 2010 2011 2009 2017 2017 2010
##  [5656] 2016 2014 2016 2012 2007 2009 2010 2013 2015 2016 2015 2015 2016
##  [5669] 2015 2015 2015 2016 2016 2016 2017 2017 2014 2015 2007 2014 2015
##  [5682] 2016 2017 2010 2010 2015 2017 2008 2014 2015 2007 2011 2013 2016
##  [5695] 2017 2016 2016 2017 2010 2016 2017 2015 2013 2014 2015 2016 2015
##  [5708] 2015 2017 2014 2015 2016 2016 2015 2017 2012 2015 2016 2017 2017
##  [5721] 2012 2015 2017 2012 2015 2012 2009 2009 2015 2015 2016 2008 2015
##  [5734] 2017 2017 2015 2007 2015 2016 2016 2016 2017 2017 2009 2017 2015
##  [5747] 2011 2016 2016 2017 2009 2012 2012 2016 2010 2013 2014 2014 2015
##  [5760] 2017 2015 2010 2010 2014 2008 2008 2015 2016 2010 2012 2007 2010
##  [5773] 2016 2016 2017 2010 2013 2015 2007 2016 2017 2017 2015 2014 2015
##  [5786] 2010 2007 2016 2016 2013 2016 2017 2009 2016 2007 2015 2015 2011
##  [5799] 2012 2009 2017 2009 2011 2008 2008 2016 2015 2017 2017 2012 2017
##  [5812] 2010 2016 2016 2017 2017 2010 2010 2014 2016 2017 2011 2013 2016
##  [5825] 2007 2008 2017 2012 2016 2014 2016 2016 2008 2010 2015 2017 2015
##  [5838] 2007 2011 2015 2015 2016 2017 2007 2010 2012 2013 2016 2017 2014
##  [5851] 2012 2017 2015 2012 2009 2011 2013 2016 2012 2016 2016 2009 2009
##  [5864] 2017 2016 2017 2014 2015 2015 2012 2016 2016 2008 2016 2015 2016
##  [5877] 2008 2016 2016 2016 2015 2009 2007 2014 2014 2016 2015 2015 2008
##  [5890] 2009 2014 2012 2010 2016 2017 2017 2014 2015 2011 2016 2016 2017
##  [5903] 2010 2015 2016 2015 2012 2016 2017 2009 2008 2007 2007 2007 2016
##  [5916] 2017 2015 2010 2014 2015 2015 2007 2008 2007 2015 2017 2009 2017
##  [5929] 2017 2017 2009 2015 2016 2014 2017 2016 2016 2017 2008 2008 2015
##  [5942] 2016 2015 2017 2012 2007 2016 2016 2017 2007 2016 2016 2017 2008
##  [5955] 2012 2007 2016 2017 2015 2016 2014 2015 2015 2016 2015 2017 2008
##  [5968] 2016 2016 2007 2010 2013 2017 2016 2008 2009 2011 2012 2014 2017
##  [5981] 2011 2016 2011 2016 2017 2017 2017 2008 2008 2007 2016 2008 2017
##  [5994] 2015 2016 2010 2013 2015 2016 2017 2017 2015 2011 2014 2016 2013
##  [6007] 2015 2016 2015 2016 2016 2017 2017 2008 2010 2009 2017 2009 2015
##  [6020] 2016 2016 2011 2013 2016 2015 2015 2008 2010 2015 2015 2008 2017
##  [6033] 2011 2010 2013 2015 2016 2017 2011 2007 2016 2013 2016 2017 2017
##  [6046] 2009 2010 2017 2015 2007 2012 2015 2016 2015 2011 2007 2013 2015
##  [6059] 2016 2017 2010 2015 2011 2011 2010 2015 2014 2015 2010 2016 2017
##  [6072] 2010 2017 2014 2017 2008 2013 2015 2012 2016 2008 2015 2016 2008
##  [6085] 2009 2007 2007 2017 2017 2013 2012 2017 2011 2016 2017 2016 2009
##  [6098] 2016 2017 2007 2010 2016 2016 2017 2017 2010 2016 2009 2007 2015
##  [6111] 2016 2015 2016 2017 2008 2015 2016 2016 2017 2017 2016 2011 2011
##  [6124] 2016 2016 2016 2016 2014 2011 2015 2013 2017 2015 2016 2017 2012
##  [6137] 2017 2016 2015 2010 2008 2016 2011 2014 2015 2015 2016 2015 2015
##  [6150] 2010 2010 2007 2016 2016 2017 2017 2017 2014 2016 2017 2013 2015
##  [6163] 2010 2012 2015 2016 2017 2009 2016 2017 2015 2016 2017 2015 2017
##  [6176] 2008 2016 2016 2015 2017 2016 2013 2017 2012 2013 2016 2010 2008
##  [6189] 2007 2008 2016 2012 2011 2010 2015 2015 2014 2015 2008 2010 2016
##  [6202] 2016 2017 2017 2008 2008 2016 2016 2017 2017 2010 2012 2013 2016
##  [6215] 2017 2016 2016 2016 2014 2016 2013 2017 2016 2013 2017 2016 2015
##  [6228] 2017 2015 2009 2016 2015 2009 2013 2014 2016 2017 2012 2011 2013
##  [6241] 2014 2016 2009 2010 2013 2016 2016 2008 2010 2015 2015 2011 2015
##  [6254] 2017 2016 2016 2016 2013 2014 2014 2016 2012 2015 2015 2016 2016
##  [6267] 2017 2017 2016 2010 2014 2017 2017 2017 2008 2010 2014 2015 2017
##  [6280] 2017 2012 2013 2007 2014 2015 2015 2016 2008 2014 2016 2008 2008
##  [6293] 2008 2010 2011 2014 2015 2009 2017 2015 2017 2017 2016 2010 2012
##  [6306] 2012 2011 2015 2016 2012 2014 2015 2016 2011 2015 2016 2016 2017
##  [6319] 2008 2008 2011 2016 2007 2015 2015 2011 2013 2016 2016 2011 2010
##  [6332] 2017 2009 2009 2011 2016 2016 2017 2016 2008 2016 2011 2010 2016
##  [6345] 2007 2009 2014 2016 2017 2015 2007 2011 2015 2016 2016 2017 2007
##  [6358] 2007 2012 2013 2016 2016 2017 2015 2016 2016 2012 2011 2008 2017
##  [6371] 2017 2010 2015 2015 2015 2011 2015 2010 2015 2007 2015 2011 2012
##  [6384] 2015 2015 2016 2015 2017 2011 2017 2015 2016 2015 2016 2016 2016
##  [6397] 2017 2009 2014 2016 2017 2009 2016 2015 2009 2015 2016 2015 2011
##  [6410] 2015 2017 2015 2017 2015 2012 2015 2016 2016 2015 2009 2013 2013
##  [6423] 2016 2015 2017 2015 2017 2017 2017 2009 2008 2009 2016 2008 2016
##  [6436] 2017 2012 2007 2016 2007 2011 2011 2007 2017 2010 2016 2017 2008
##  [6449] 2010 2011 2017 2017 2007 2015 2013 2015 2017 2009 2015 2015 2015
##  [6462] 2012 2012 2009 2015 2016 2016 2008 2016 2017 2014 2014 2015 2016
##  [6475] 2017 2010 2009 2016 2013 2015 2015 2017 2017 2008 2007 2016 2010
##  [6488] 2010 2016 2015 2015 2016 2007 2013 2017 2009 2011 2016 2016 2016
##  [6501] 2010 2013 2014 2014 2007 2008 2015 2016 2016 2010 2013 2016 2016
##  [6514] 2010 2009 2016 2017 2008 2011 2015 2016 2017 2017 2015 2017 2010
##  [6527] 2010 2016 2009 2016 2015 2017 2011 2012 2007 2017 2016 2015 2007
##  [6540] 2007 2016 2007 2015 2016 2016 2016 2017 2017 2010 2016 2011 2008
##  [6553] 2016 2015 2015 2016 2015 2015 2017 2016 2015 2016 2017 2009 2008
##  [6566] 2013 2008 2017 2017 2008 2016 2014 2017 2010 2015 2007 2007 2016
##  [6579] 2015 2009 2013 2017 2007 2012 2012 2009 2012 2015 2016 2008 2010
##  [6592] 2016 2015 2017 2016 2015 2012 2013 2013 2016 2010 2014 2015 2017
##  [6605] 2011 2014 2016 2007 2015 2015 2016 2015 2008 2016 2017 2016 2011
##  [6618] 2008 2015 2016 2011 2010 2017 2012 2015 2017 2015 2008 2010 2015
##  [6631] 2016 2007 2009 2008 2016 2016 2015 2010 2015 2009 2014 2017 2016
##  [6644] 2017 2012 2007 2014 2016 2017 2016 2010 2015 2017 2015 2017 2017
##  [6657] 2015 2011 2017 2009 2009 2014 2016 2015 2017 2013 2015 2016 2017
##  [6670] 2008 2013 2016 2016 2017 2010 2016 2008 2014 2010 2016 2017 2015
##  [6683] 2016 2016 2015 2015 2016 2017 2017 2009 2016 2017 2017 2017 2007
##  [6696] 2007 2009 2017 2008 2009 2017 2016 2017 2014 2017 2014 2015 2015
##  [6709] 2017 2017 2013 2016 2015 2009 2014 2015 2016 2017 2011 2010 2017
##  [6722] 2008 2010 2014 2016 2016 2017 2014 2016 2017 2017 2014 2008 2010
##  [6735] 2017 2017 2012 2016 2011 2013 2015 2017 2016 2016 2016 2017 2008
##  [6748] 2013 2015 2017 2012 2011 2016 2011 2015 2016 2009 2012 2015 2015
##  [6761] 2016 2016 2016 2010 2008 2012 2014 2012 2010 2015 2017 2007 2016
##  [6774] 2017 2011 2017 2007 2015 2009 2011 2008 2016 2016 2008 2010 2013
##  [6787] 2016 2016 2012 2010 2009 2017 2011 2010 2007 2017 2011 2016 2017
##  [6800] 2015 2017 2012 2009 2015 2015 2017 2017 2016 2015 2016 2016 2011
##  [6813] 2015 2016 2017 2016 2012 2017 2017 2010 2008 2010 2013 2008 2009
##  [6826] 2007 2017 2014 2016 2010 2015 2011 2014 2010 2017 2012 2009 2016
##  [6839] 2016 2017 2017 2009 2017 2016 2011 2017 2012 2010 2017 2015 2015
##  [6852] 2008 2014 2007 2007 2012 2011 2009 2011 2011 2010 2007 2015 2015
##  [6865] 2010 2010 2017 2010 2009 2010 2009 2011 2012 2017 2010 2012 2013
##  [6878] 2016 2017 2009 2016 2017 2010 2010 2015 2017 2011 2015 2007 2014
##  [6891] 2016 2008 2015 2017 2013 2016 2016 2017 2015 2009 2007 2008 2017
##  [6904] 2007 2015 2014 2016 2016 2016 2016 2013 2015 2015 2017 2017 2011
##  [6917] 2015 2015 2016 2009 2012 2016 2009 2015 2017 2016 2016 2016 2011
##  [6930] 2011 2008 2013 2017 2008 2015 2016 2016 2013 2007 2013 2015 2015
##  [6943] 2016 2011 2010 2008 2013 2016 2016 2017 2016 2015 2007 2008 2012
##  [6956] 2012 2013 2012 2009 2014 2015 2016 2015 2017 2007 2015 2015 2016
##  [6969] 2017 2009 2011 2015 2015 2011 2009 2015 2008 2015 2017 2011 2017
##  [6982] 2014 2014 2016 2017 2009 2016 2017 2007 2011 2015 2017 2017 2017
##  [6995] 2007 2017 2017 2015 2012 2016 2016 2016 2008 2015 2008 2016 2016
##  [7008] 2017 2008 2016 2015 2009 2010 2010 2016 2008 2016 2016 2009 2007
##  [7021] 2017 2015 2013 2015 2016 2017 2008 2011 2012 2016 2016 2008 2011
##  [7034] 2015 2012 2016 2016 2010 2012 2013 2016 2016 2015 2016 2011 2007
##  [7047] 2017 2017 2016 2017 2010 2011 2011 2008 2016 2015 2008 2014 2015
##  [7060] 2009 2010 2014 2015 2014 2012 2013 2015 2015 2015 2016 2015 2012
##  [7073] 2016 2015 2016 2007 2014 2015 2017 2007 2015 2017 2015 2015 2016
##  [7086] 2017 2017 2015 2013 2017 2016 2010 2007 2008 2016 2011 2015 2016
##  [7099] 2015 2008 2014 2012 2010 2014 2016 2012 2015 2015 2017 2012 2010
##  [7112] 2016 2010 2008 2013 2012 2011 2011 2011 2010 2015 2015 2016 2015
##  [7125] 2014 2017 2014 2007 2013 2016 2015 2015 2017 2017 2012 2013 2014
##  [7138] 2015 2015 2009 2015 2016 2016 2016 2015 2017 2009 2016 2017 2017
##  [7151] 2017 2016 2017 2009 2014 2008 2011 2010 2015 2009 2015 2016 2016
##  [7164] 2016 2011 2011 2013 2014 2017 2010 2008 2016 2017 2010 2009 2014
##  [7177] 2017 2015 2016 2017 2017 2010 2015 2015 2017 2016 2009 2014 2017
##  [7190] 2017 2017 2012 2015 2007 2012 2012 2016 2017 2010 2015 2015 2017
##  [7203] 2013 2014 2016 2017 2016 2016 2014 2015 2016 2011 2017 2008 2016
##  [7216] 2007 2015 2007 2015 2015 2011 2015 2007 2017 2008 2016 2017 2010
##  [7229] 2017 2017 2013 2014 2016 2009 2016 2016 2016 2008 2014 2015 2010
##  [7242] 2017 2017 2015 2012 2013 2016 2010 2008 2008 2016 2015 2008 2008
##  [7255] 2016 2017 2009 2009 2015 2007 2008 2011 2007 2012 2016 2017 2015
##  [7268] 2015 2013 2012 2010 2013 2010 2015 2016 2016 2017 2017 2007 2015
##  [7281] 2017 2009 2009 2013 2015 2016 2015 2011 2009 2007 2012 2017 2014
##  [7294] 2017 2017 2017 2017 2016 2010 2015 2009 2009 2015 2015 2016 2017
##  [7307] 2017 2017 2017 2017 2015 2008 2009 2013 2016 2017 2017 2012 2007
##  [7320] 2010 2015 2016 2017 2014 2015 2015 2016 2015 2016 2016 2010 2010
##  [7333] 2009 2010 2014 2016 2017 2016 2017 2008 2013 2015 2014 2017 2009
##  [7346] 2017 2015 2012 2007 2016 2017 2008 2015 2016 2015 2017 2016 2007
##  [7359] 2012 2016 2016 2007 2012 2015 2015 2016 2012 2012 2013 2013 2014
##  [7372] 2015 2017 2015 2009 2013 2007 2015 2010 2017 2012 2015 2017 2017
##  [7385] 2015 2016 2016 2007 2015 2016 2017 2016 2016 2007 2011 2014 2007
##  [7398] 2013 2017 2016 2016 2017 2008 2015 2017 2017 2013 2013 2017 2009
##  [7411] 2009 2017 2011 2016 2011 2015 2007 2012 2015 2007 2008 2015 2017
##  [7424] 2015 2016 2011 2010 2010 2015 2016 2011 2007 2015 2007 2016 2014
##  [7437] 2011 2013 2007 2013 2015 2017 2012 2016 2016 2015 2015 2016 2014
##  [7450] 2016 2017 2008 2011 2007 2009 2010 2013 2016 2017 2017 2017 2016
##  [7463] 2015 2008 2007 2012 2017 2015 2015 2007 2010 2015 2017 2012 2016
##  [7476] 2007 2007 2007 2010 2010 2014 2007 2015 2017 2007 2016 2017 2012
##  [7489] 2010 2010 2016 2008 2008 2013 2015 2012 2016 2017 2009 2015 2010
##  [7502] 2017 2017 2012 2013 2013 2015 2017 2009 2012 2014 2017 2007 2010
##  [7515] 2009 2016 2007 2015 2010 2010 2010 2016 2017 2007 2016 2016 2015
##  [7528] 2016 2015 2017 2010 2016 2009 2014 2016 2017 2010 2008 2015 2016
##  [7541] 2017 2016 2017 2015 2012 2015 2015 2016 2007 2015 2016 2014 2015
##  [7554] 2009 2014 2015 2015 2016 2008 2012 2016 2013 2017 2017 2017 2011
##  [7567] 2007 2016 2009 2015 2016 2015 2017 2007 2015 2016 2016 2017 2017
##  [7580] 2017 2007 2015 2012 2010 2009 2010 2007 2015 2016 2015 2016 2016
##  [7593] 2008 2015 2015 2014 2008 2011 2014 2017 2008 2016 2008 2016 2016
##  [7606] 2009 2009 2016 2016 2016 2017 2009 2010 2015 2009 2016 2017 2014
##  [7619] 2007 2015 2016 2009 2015 2016 2017 2013 2015 2015 2016 2008 2010
##  [7632] 2016 2012 2016 2016 2013 2016 2017 2009 2016 2016 2016 2016 2015
##  [7645] 2017 2017 2017 2009 2016 2016 2017 2011 2010 2015 2012 2016 2010
##  [7658] 2016 2017 2016 2008 2015 2016 2016 2017 2009 2007 2017 2017 2007
##  [7671] 2008 2013 2007 2016 2015 2016 2017 2016 2007 2013 2016 2013 2015
##  [7684] 2008 2007 2009 2008 2007 2013 2014 2015 2011 2013 2013 2017 2012
##  [7697] 2013 2017 2008 2016 2016 2017 2017 2007 2010 2008 2015 2010 2009
##  [7710] 2015 2017 2008 2015 2016 2015 2012 2014 2015 2017 2007 2015 2015
##  [7723] 2016 2012 2015 2011 2013 2011 2016 2017 2015 2017 2011 2011 2013
##  [7736] 2015 2007 2017 2017 2012 2015 2017 2010 2015 2013 2017 2017 2010
##  [7749] 2015 2008 2016 2012 2015 2014 2015 2017 2010 2013 2016 2015 2015
##  [7762] 2013 2009 2009 2015 2017 2009 2017 2017 2009 2016 2016 2017 2007
##  [7775] 2009 2011 2015 2014 2017 2012 2010 2009 2014 2014 2016 2014 2017
##  [7788] 2014 2017 2011 2008 2013 2016 2016 2015 2015 2012 2016 2015 2017
##  [7801] 2010 2008 2016 2015 2017 2016 2017 2010 2007 2015 2015 2009 2009
##  [7814] 2017 2017 2013 2011 2013 2014 2016 2009 2014 2013 2016 2015 2007
##  [7827] 2015 2016 2008 2009 2012 2014 2009 2010 2017 2014 2017 2017 2017
##  [7840] 2016 2016 2008 2012 2008 2013 2014 2016 2014 2017 2017 2016 2017
##  [7853] 2013 2015 2016 2016 2007 2011 2017 2012 2009 2016 2007 2017 2009
##  [7866] 2008 2016 2016 2016 2014 2015 2016 2016 2017 2012 2011 2011 2008
##  [7879] 2013 2015 2017 2017 2011 2013 2017 2017 2007 2009 2016 2016 2008
##  [7892] 2009 2011 2008 2015 2017 2009 2017 2016 2017 2017 2008 2016 2017
##  [7905] 2009 2015 2016 2015 2017 2017 2009 2009 2015 2017 2016 2016 2017
##  [7918] 2017 2016 2016 2016 2016 2016 2015 2009 2014 2016 2017 2012 2016
##  [7931] 2017 2009 2008 2011 2017 2017 2010 2016 2017 2015 2017 2012 2016
##  [7944] 2010 2009 2016 2013 2016 2017 2011 2010 2012 2013 2014 2015 2017
##  [7957] 2016 2015 2011 2008 2009 2017 2010 2009 2016 2016 2012 2017 2010
##  [7970] 2013 2008 2012 2014 2017 2016 2012 2015 2016 2010 2015 2016 2017
##  [7983] 2010 2013 2016 2017 2016 2015 2008 2017 2007 2017 2013 2015 2017
##  [7996] 2014 2016 2007 2016 2007 2007 2013 2014 2010 2010 2017 2008 2015
##  [8009] 2016 2010 2015 2008 2016 2017 2016 2017 2011 2008 2014 2017 2012
##  [8022] 2015 2016 2016 2017 2011 2017 2010 2016 2017 2013 2014 2017 2009
##  [8035] 2015 2015 2017 2017 2014 2017 2009 2016 2007 2012 2008 2008 2014
##  [8048] 2016 2015 2015 2014 2017 2015 2016 2016 2016 2017 2007 2014 2011
##  [8061] 2016 2010 2010 2016 2016 2010 2012 2007 2015 2017 2017 2007 2017
##  [8074] 2012 2014 2017 2011 2013 2015 2008 2012 2010 2012 2010 2014 2015
##  [8087] 2012 2015 2009 2009 2007 2014 2017 2011 2017 2009 2008 2015 2016
##  [8100] 2017 2015 2011 2016 2012 2007 2016 2015 2016 2015 2017 2011 2008
##  [8113] 2017 2017 2016 2017 2012 2017 2007 2012 2010 2017 2016 2017 2015
##  [8126] 2016 2017 2008 2017 2015 2009 2015 2016 2015 2017 2010 2008 2011
##  [8139] 2016 2017 2017 2015 2015 2015 2016 2012 2010 2016 2011 2015 2017
##  [8152] 2011 2015 2016 2007 2012 2017 2017 2011 2016 2012 2009 2013 2013
##  [8165] 2008 2009 2015 2010 2011 2011 2017 2010 2007 2016 2010 2011 2014
##  [8178] 2016 2015 2017 2011 2009 2016 2015 2010 2008 2016 2017 2010 2010
##  [8191] 2009 2010 2015 2007 2012 2016 2012 2015 2016 2017 2010 2011 2014
##  [8204] 2011 2012 2016 2015 2016 2007 2013 2016 2016 2013 2012 2008 2007
##  [8217] 2016 2012 2007 2016 2017 2008 2017 2017 2007 2011 2016 2016 2007
##  [8230] 2015 2015 2011 2009 2016 2016 2017 2007 2012 2015 2007 2011 2009
##  [8243] 2014 2016 2011 2015 2017 2011 2008 2007 2009 2014 2017 2017 2017
##  [8256] 2016 2009 2015 2011 2013 2013 2008 2007 2014 2007 2015 2008 2009
##  [8269] 2013 2008 2010 2016 2010 2008 2016 2017 2010 2015 2016 2016 2017
##  [8282] 2017 2017 2009 2016 2016 2017 2010 2015 2016 2017 2017 2010 2011
##  [8295] 2015 2010 2015 2012 2009 2016 2016 2010 2010 2011 2016 2011 2015
##  [8308] 2016 2009 2013 2017 2017 2010 2016 2015 2017 2011 2013 2012 2009
##  [8321] 2015 2016 2012 2011 2017 2015 2016 2014 2015 2015 2017 2010 2015
##  [8334] 2014 2016 2011 2014 2016 2017 2013 2015 2016 2017 2017 2012 2008
##  [8347] 2016 2017 2008 2010 2013 2016 2017 2013 2008 2015 2016 2009 2015
##  [8360] 2016 2017 2017 2007 2017 2009 2017 2007 2013 2016 2011 2008 2007
##  [8373] 2017 2008 2014 2015 2016 2017 2007 2009 2016 2016 2013 2013 2017
##  [8386] 2017 2017 2016 2016 2017 2015 2017 2015 2016 2012 2015 2017 2011
##  [8399] 2010 2016 2016 2017 2012 2016 2015 2017 2010 2008 2015 2016 2017
##  [8412] 2007 2011 2016 2016 2016 2014 2016 2010 2008 2016 2017 2012 2007
##  [8425] 2009 2007 2017 2007 2011 2007 2014 2012 2012 2008 2015 2017 2017
##  [8438] 2011 2012 2011 2008 2015 2013 2016 2016 2016 2011 2010 2011 2016
##  [8451] 2007 2016 2017 2012 2007 2009 2014 2015 2010 2009 2015 2015 2009
##  [8464] 2016 2016 2015 2013 2016 2016 2017 2012 2016 2017 2008 2017 2009
##  [8477] 2015 2016 2016 2016 2012 2011 2016 2016 2016 2017 2015 2012 2013
##  [8490] 2016 2015 2008 2011 2014 2009 2009 2015 2017 2007 2009 2010 2010
##  [8503] 2016 2015 2015 2017 2017 2017 2010 2013 2016 2017 2016 2017 2015
##  [8516] 2017 2008 2008 2017 2017 2016 2017 2011 2014 2015 2015 2016 2017
##  [8529] 2010 2016 2016 2008 2015 2016 2015 2017 2016 2017 2010 2010 2007
##  [8542] 2011 2011 2007 2016 2017 2011 2015 2015 2011 2010 2009 2015 2017
##  [8555] 2007 2016 2017 2015 2016 2016 2017 2014 2015 2015 2009 2013 2015
##  [8568] 2017 2011 2017 2012 2016 2007 2011 2013 2014 2015 2015 2016 2017
##  [8581] 2015 2017 2010 2016 2016 2016 2011 2014 2011 2015 2016 2017 2013
##  [8594] 2016 2016 2017 2017 2011 2008 2017 2012 2016 2011 2008 2016 2008
##  [8607] 2007 2008 2016 2016 2017 2014 2016 2012 2015 2015 2015 2015 2017
##  [8620] 2011 2010 2009 2013 2015 2017 2008 2015 2009 2016 2009 2011 2016
##  [8633] 2012 2017 2009 2008 2016 2016 2017 2014 2007 2012 2017 2007 2014
##  [8646] 2007 2013 2016 2017 2012 2015 2017 2011 2011 2014 2016 2011 2013
##  [8659] 2017 2017 2011 2011 2014 2014 2010 2012 2017 2012 2012 2015 2015
##  [8672] 2015 2015 2008 2017 2017 2016 2012 2015 2010 2013 2013 2014 2015
##  [8685] 2016 2016 2007 2017 2014 2016 2016 2012 2015 2011 2015 2007 2009
##  [8698] 2012 2014 2016 2015 2015 2016 2010 2009 2007 2017 2017 2012 2013
##  [8711] 2016 2016 2017 2015 2016 2017 2017 2013 2007 2015 2016 2016 2012
##  [8724] 2015 2016 2017 2017 2010 2014 2008 2010 2007 2012 2008 2015 2011
##  [8737] 2007 2014 2016 2015 2015 2007 2008 2007 2017 2007 2013 2015 2016
##  [8750] 2010 2015 2009 2014 2016 2011 2015 2017 2008 2017 2010 2011 2016
##  [8763] 2017 2011 2009 2007 2014 2017 2008 2017 2017 2017 2007 2014 2015
##  [8776] 2015 2015 2008 2011 2016 2017 2009 2009 2010 2015 2016 2017 2017
##  [8789] 2016 2015 2011 2007 2007 2016 2014 2008 2012 2015 2016 2015 2014
##  [8802] 2017 2017 2011 2016 2015 2010 2016 2017 2009 2010 2009 2009 2009
##  [8815] 2017 2007 2011 2009 2017 2007 2007 2008 2017 2014 2015 2017 2016
##  [8828] 2009 2010 2015 2016 2011 2009 2015 2016 2012 2012 2007 2010 2008
##  [8841] 2017 2008 2016 2011 2017 2009 2014 2017 2017 2017 2007 2016 2007
##  [8854] 2012 2015 2016 2017 2012 2014 2011 2017 2007 2015 2016 2011 2009
##  [8867] 2008 2016 2016 2016 2017 2017 2017 2009 2017 2016 2015 2007 2011
##  [8880] 2016 2016 2010 2017 2012 2013 2017 2007 2017 2010 2015 2015 2008
##  [8893] 2013 2017 2012 2011 2007 2017 2015 2017 2010 2014 2016 2017 2011
##  [8906] 2009 2015 2008 2014 2016 2010 2017 2017 2014 2010 2016 2015 2011
##  [8919] 2015 2010 2007 2012 2007 2007 2009 2013 2016 2007 2013 2013 2016
##  [8932] 2007 2010 2017 2016 2016 2016 2016 2016 2017 2015 2017 2016 2009
##  [8945] 2017 2016 2017 2017 2013 2009 2012 2012 2009 2014 2016 2017 2017
##  [8958] 2008 2016 2016 2007 2010 2014 2015 2017 2017 2016 2015 2014 2016
##  [8971] 2007 2011 2016 2017 2017 2017 2012 2017 2017 2011 2007 2007 2016
##  [8984] 2015 2009 2015 2016 2011 2015 2017 2012 2016 2017 2010 2010 2007
##  [8997] 2015 2009 2011 2007 2016 2017 2017 2011 2011 2007 2017 2016 2013
##  [9010] 2007 2017 2015 2015 2015 2017 2007 2012 2011 2015 2009 2015 2015
##  [9023] 2017 2016 2015 2017 2017 2017 2017 2015 2015 2016 2008 2009 2008
##  [9036] 2013 2015 2012 2015 2015 2016 2008 2013 2015 2017 2015 2017 2017
##  [9049] 2011 2010 2012 2013 2013 2015 2015 2017 2017 2015 2017 2010 2007
##  [9062] 2016 2016 2015 2016 2017 2009 2014 2016 2015 2017 2015 2015 2016
##  [9075] 2011 2007 2015 2016 2016 2017 2017 2017 2013 2008 2015 2016 2008
##  [9088] 2017 2010 2015 2017 2010 2013 2016 2015 2012 2007 2016 2017 2007
##  [9101] 2015 2012 2014 2015 2016 2010 2010 2009 2009 2012 2012 2014 2012
##  [9114] 2010 2015 2016 2017 2017 2010 2007 2016 2007 2013 2015 2017 2007
##  [9127] 2015 2017 2009 2015 2016 2008 2009 2009 2014 2015 2007 2012 2009
##  [9140] 2017 2013 2015 2008 2017 2017 2007 2017 2011 2007 2011 2016 2017
##  [9153] 2012 2008 2010 2015 2012 2016 2016 2016 2017 2017 2016 2017 2017
##  [9166] 2010 2015 2016 2011 2017 2013 2016 2014 2011 2016 2015 2017 2009
##  [9179] 2009 2013 2015 2016 2017 2017 2007 2008 2009 2011 2011 2013 2017
##  [9192] 2010 2013 2015 2015 2007 2010 2007 2016 2011 2016 2017 2015 2016
##  [9205] 2016 2017 2008 2012 2017 2017 2009 2015 2010 2010 2017 2014 2015
##  [9218] 2015 2016 2017 2017 2009 2017 2017 2012 2016 2017 2016 2016 2017
##  [9231] 2009 2011 2016 2017 2017 2017 2012 2010 2016 2016 2016 2015 2012
##  [9244] 2009 2016 2015 2015 2017 2014 2017 2011 2016 2017 2015 2017 2017
##  [9257] 2017 2014 2016 2016 2010 2014 2016 2016 2016 2017 2017 2017 2015
##  [9270] 2014 2015 2016 2008 2016 2017 2014 2015 2016 2015 2015 2009 2017
##  [9283] 2014 2015 2014 2016 2011 2016 2015 2011 2008 2016 2017 2015 2015
##  [9296] 2015 2015 2007 2013 2008 2014 2007 2015 2007 2007 2011 2011 2016
##  [9309] 2016 2017 2015 2010 2017 2017 2011 2017 2011 2015 2016 2017 2017
##  [9322] 2017 2008 2016 2016 2016 2015 2016 2009 2009 2017 2017 2017 2015
##  [9335] 2015 2010 2017 2016 2016 2010 2007 2007 2016 2017 2017 2007 2012
##  [9348] 2007 2016 2017 2016 2012 2012 2013 2011 2015 2015 2017 2015 2015
##  [9361] 2017 2011 2013 2013 2014 2007 2009 2016 2016 2015 2017 2017 2007
##  [9374] 2011 2016 2009 2015 2017 2017 2007 2008 2017 2011 2016 2016 2011
##  [9387] 2015 2016 2016 2016 2017 2012 2010 2008 2013 2014 2017 2017 2009
##  [9400] 2009 2015 2016 2017 2013 2013 2013 2014 2016 2017 2014 2016 2017
##  [9413] 2011 2011 2014 2015 2015 2016 2017 2007 2008 2008 2011 2015 2017
##  [9426] 2012 2017 2008 2015 2017 2015 2017 2007 2008 2010 2016 2016 2017
##  [9439] 2010 2011 2015 2009 2009 2013 2014 2016 2009 2012 2017 2015 2012
##  [9452] 2008 2014 2015 2016 2017 2008 2010 2015 2017 2014 2015 2016 2017
##  [9465] 2013 2017 2007 2013 2016 2011 2016 2015 2015 2009 2014 2016 2016
##  [9478] 2017 2017 2017 2010 2009 2014 2015 2015 2011 2016 2011 2009 2014
##  [9491] 2016 2011 2016 2010 2011 2014 2016 2011 2013 2017 2017 2017 2008
##  [9504] 2013 2016 2015 2007 2016 2009 2007 2014 2016 2014 2016 2016 2012
##  [9517] 2014 2016 2015 2017 2011 2011 2016 2016 2017 2009 2007 2015 2017
##  [9530] 2017 2008 2007 2007 2017 2017 2009 2013 2015 2015 2008 2014 2017
##  [9543] 2017 2017 2017 2008 2014 2016 2017 2017 2014 2014 2007 2010 2012
##  [9556] 2012 2013 2008 2015 2016 2016 2017 2015 2017 2010 2016 2015 2015
##  [9569] 2010 2010 2016 2011 2014 2016 2016 2008 2015 2017 2007 2008 2013
##  [9582] 2015 2008 2007 2010 2015 2016 2016 2015 2008 2008 2016 2017 2017
##  [9595] 2008 2014 2016 2016 2017 2009 2014 2016 2016 2016 2011 2007 2008
##  [9608] 2015 2016 2017 2009 2015 2017 2016 2016 2010 2008 2016 2017 2008
##  [9621] 2012 2015 2016 2017 2008 2016 2017 2007 2012 2016 2017 2009 2012
##  [9634] 2009 2016 2017 2009 2016 2017 2008 2015 2017 2015 2017 2012 2015
##  [9647] 2017 2007 2010 2008 2015 2016 2010 2013 2017 2011 2011 2016 2017
##  [9660] 2017 2017 2017 2015 2016 2011 2016 2009 2013 2015 2009 2015 2015
##  [9673] 2007 2016 2016 2015 2015 2009 2012 2014 2017 2011 2009 2017 2011
##  [9686] 2007 2011 2013 2015 2016 2016 2008 2015 2014 2011 2015 2015 2013
##  [9699] 2017 2017 2011 2013 2013 2016 2011 2017 2008 2016 2008 2009 2014
##  [9712] 2015 2007 2010 2017 2010 2015 2017 2015 2016 2015 2016 2017 2015
##  [9725] 2013 2015 2017 2014 2015 2016 2016 2015 2017 2017 2007 2010 2010
##  [9738] 2016 2017 2017 2015 2017 2007 2007 2011 2015 2016 2007 2009 2015
##  [9751] 2015 2011 2008 2015 2015 2016 2009 2010 2014 2015 2008 2011 2009
##  [9764] 2011 2009 2013 2016 2017 2011 2007 2014 2017 2017 2011 2009 2009
##  [9777] 2014 2016 2016 2017 2010 2008 2013 2017 2010 2015 2016 2017 2017
##  [9790] 2017 2014 2016 2017 2011 2008 2015 2009 2008 2014 2016 2010 2012
##  [9803] 2008 2016 2010 2014 2016 2007 2008 2017 2017 2017 2011 2012 2015
##  [9816] 2015 2016 2015 2007 2014 2016 2016 2016 2016 2015 2015 2016 2015
##  [9829] 2017 2017 2007 2008 2007 2009 2013 2015 2010 2015 2016 2016 2017
##  [9842] 2015 2007 2013 2012 2012 2015 2017 2013 2013 2015 2015 2010 2011
##  [9855] 2010 2009 2012 2015 2016 2017 2012 2015 2011 2007 2015 2012 2016
##  [9868] 2010 2013 2016 2017 2009 2015 2017 2011 2014 2015 2015 2016 2017
##  [9881] 2012 2010 2016 2015 2008 2016 2011 2009 2014 2011 2012 2012 2008
##  [9894] 2009 2016 2012 2014 2012 2017 2011 2016 2008 2008 2014 2009 2015
##  [9907] 2017 2009 2017 2009 2016 2016 2007 2016 2015 2017 2017 2007 2007
##  [9920] 2008 2007 2013 2016 2010 2009 2017 2016 2016 2017 2011 2012 2014
##  [9933] 2014 2017 2016 2008 2013 2011 2012 2012 2015 2008 2016 2008 2015
##  [9946] 2007 2009 2013 2013 2015 2015 2016 2016 2015 2014 2007 2011 2011
##  [9959] 2010 2010 2015 2015 2016 2016 2010 2014 2008 2010 2011 2013 2014
##  [9972] 2015 2017 2014 2015 2016 2017 2007 2017 2007 2013 2013 2016 2008
##  [9985] 2015 2014 2016 2017 2010 2013 2016 2010 2017 2017 2007 2017 2015
##  [9998] 2015 2017 2009 2015 2017 2012 2017 2010 2012 2007 2008 2013 2009
## [10011] 2007 2015 2011 2011 2008 2016 2016 2017 2010 2014 2016 2016 2017
## [10024] 2017 2014 2015 2016 2017 2017 2015 2017 2015 2015 2017 2015 2017
## [10037] 2017 2010 2008 2015 2015 2016 2015 2007 2009 2009 2015 2008 2013
## [10050] 2016 2016 2012 2013 2017 2012 2015 2015 2015 2017 2015 2008 2016
## [10063] 2009 2015 2009 2015 2016 2016 2016 2009 2008 2010 2016 2016 2011
## [10076] 2015 2016 2012 2008 2010 2009 2016 2015 2015 2010 2013 2017 2016
## [10089] 2011 2013 2017 2007 2014 2016 2011 2010 2008 2015 2009 2013 2017
## [10102] 2017 2007 2007 2010 2014 2015 2016 2016 2016 2017 2009 2015 2017
## [10115] 2015 2017 2015 2017 2012 2015 2012 2008 2007 2012 2014 2014 2016
## [10128] 2016 2008 2010 2010 2017 2010 2011 2017 2017 2013 2016 2016 2016
## [10141] 2017 2010 2016 2016 2007 2010 2015 2015 2010 2017 2009 2007 2015
## [10154] 2015 2017 2011 2012 2014 2015 2017 2017 2016 2010 2015 2015 2016
## [10167] 2011 2010 2015 2016 2012 2011 2015 2015 2014 2010 2016 2015 2016
## [10180] 2008 2014 2015 2016 2012 2016 2016 2012 2009 2011 2013 2015 2016
## [10193] 2008 2015 2017 2008 2012 2015 2017 2009 2007 2007 2015 2016 2016
## [10206] 2015 2016 2017 2009 2016 2015 2011 2007 2009 2009 2010 2007 2007
## [10219] 2013 2015 2012 2017 2008 2010 2016 2011 2007 2015 2017 2008 2008
## [10232] 2008 2011 2016 2007 2016 2016 2012 2010 2014 2016 2016 2016 2014
## [10245] 2015 2008 2008 2016 2009 2016 2009 2013 2017 2011 2012 2015 2016
## [10258] 2017 2014 2014 2017 2008 2008 2011 2015 2016 2015 2017 2011 2010
## [10271] 2017 2015 2016 2017 2017 2008 2015 2016 2011 2014 2015 2017 2009
## [10284] 2007 2009 2017 2008 2016 2016 2016 2017 2009 2009 2014 2017 2010
## [10297] 2015 2016 2017 2007 2016 2015 2017 2014 2014 2016 2017 2010 2013
## [10310] 2014 2017 2010 2009 2010 2011 2017 2015 2017 2015 2017 2007 2011
## [10323] 2017 2012 2017 2007 2016 2017 2017 2014 2009 2017 2009 2012 2015
## [10336] 2016 2016 2017 2013 2015 2011 2013 2016 2016 2016 2016 2008 2017
## [10349] 2015 2016 2015 2017 2015 2016 2017 2013 2011 2015 2017 2016 2009
## [10362] 2008 2009 2016 2017 2016 2011 2008 2007 2015 2017 2008 2007 2011
## [10375] 2008 2016 2017 2017 2017 2014 2015 2016 2017 2017 2017 2017 2012
## [10388] 2016 2015 2014 2015 2016 2016 2017 2013 2013 2007 2010 2015 2017
## [10401] 2014 2015 2015 2007 2017 2016 2015 2015 2016 2017 2012 2008 2013
## [10414] 2016 2013 2016 2009 2007 2015 2016 2016 2013 2016 2017 2015 2016
## [10427] 2008 2016 2016 2015 2017 2017 2007 2017 2012 2015 2017 2013 2014
## [10440] 2015 2016 2010 2008 2009 2012 2014 2017 2013 2016 2011 2015 2015
## [10453] 2014 2016 2017 2017 2008 2007 2015 2007 2014 2016 2013 2017 2009
## [10466] 2012 2017 2015 2016 2017 2015 2014 2016 2016 2010 2014 2016 2008
## [10479] 2009 2016 2017 2011 2011 2015 2012 2007 2011 2017 2014 2017 2010
## [10492] 2007 2015 2007 2014 2015 2015 2015 2016 2017 2009 2008 2009 2012
## [10505] 2010 2016 2015 2007 2008 2017 2008 2008 2009 2015 2015 2016 2015
## [10518] 2009 2015 2017 2016 2015 2017 2012 2008 2015 2017 2007 2015 2015
## [10531] 2016 2016 2013 2016 2008 2012 2007 2016 2015 2016 2016 2008 2012
## [10544] 2007 2012 2012 2007 2015 2017 2008 2015 2015 2016 2017 2012 2015
## [10557] 2012 2012 2016 2009 2015 2017 2015 2008 2014 2015 2017 2010 2009
## [10570] 2016 2017 2016 2016 2017 2016 2016 2017 2010 2016 2017 2016 2015
## [10583] 2016 2017 2017 2007 2017 2014 2016 2016 2017 2015 2014 2017 2012
## [10596] 2007 2011 2011 2015 2017 2016 2017 2007 2008 2012 2010 2014 2015
## [10609] 2017 2012 2016 2016 2012 2011 2016 2008 2014 2016 2007 2012 2011
## [10622] 2016 2016 2011 2007 2017 2009 2009 2012 2014 2013 2009 2015 2015
## [10635] 2016 2017 2017 2007 2016 2014 2015 2016 2017 2008 2012 2014 2016
## [10648] 2011 2008 2016 2017 2017 2014 2010 2009 2014 2016 2017 2009 2012
## [10661] 2015 2015 2008 2014 2012 2017 2007 2009 2014 2016 2009 2015 2015
## [10674] 2017 2017 2008 2016 2017 2017 2017 2011 2014 2015 2011 2014 2016
## [10687] 2008 2016 2017 2013 2017 2015 2016 2012 2007 2017 2007 2017 2010
## [10700] 2013 2008 2016 2015 2017 2017 2017 2007 2011 2015 2015 2015 2017
## [10713] 2007 2015 2016 2017 2011 2014 2015 2016 2011 2016 2016 2016 2008
## [10726] 2015 2016 2012 2014 2017 2017 2015 2016 2015 2016 2016 2016 2017
## [10739] 2007 2016 2015 2015 2008 2015 2015 2011 2016 2016 2010 2016 2017
## [10752] 2016 2010 2015 2016 2015 2009 2015 2010 2014 2011 2010 2015 2015
## [10765] 2016 2017 2007 2016 2011 2015 2016 2016 2013 2015 2008 2008 2007
## [10778] 2017 2015 2013 2015 2016 2016 2015 2013 2016 2016 2015 2011 2009
## [10791] 2017 2009 2015 2015 2015 2015 2016 2017 2015 2016 2017 2007 2007
## [10804] 2012 2015 2015 2017 2009 2015 2017 2011 2013 2015 2007 2017 2010
## [10817] 2008 2010 2009 2008 2009 2016 2015 2008 2007 2010 2009 2012 2009
## [10830] 2012 2013 2017 2008 2015 2016 2016 2007 2014 2008 2016 2017 2011
## [10843] 2009 2008 2010 2016 2015 2015 2017 2009 2008 2016 2014 2014 2017
## [10856] 2010 2017 2008 2015 2009 2014 2016 2016 2014 2015 2013 2015 2016
## [10869] 2016 2015 2007 2009 2016 2017 2016 2012 2010 2015 2016 2012 2008
## [10882] 2012 2014 2014 2015 2016 2015 2015 2017 2017 2017 2017 2017 2017
## [10895] 2013 2016 2009 2016 2017 2012 2013 2013 2017 2017 2013 2013 2014
## [10908] 2015 2016 2011 2010 2014 2014 2015 2016 2017 2015 2017 2011 2014
## [10921] 2017 2010 2007 2016 2016 2017 2008 2015 2017 2017 2011 2009 2016
## [10934] 2015 2016 2017 2014 2017 2014 2015 2016 2017 2017 2015 2010 2016
## [10947] 2016 2015 2017 2007 2010 2016 2012 2007 2014 2009 2016 2007 2010
## [10960] 2016 2009 2011 2016 2015 2016 2017 2010 2007 2011 2016 2016 2015
## [10973] 2015 2015 2008 2013 2015 2016 2015 2017 2014 2017 2017 2010 2016
## [10986] 2017 2013 2015 2016 2017 2017 2008 2016 2012 2016 2017 2017 2011
## [10999] 2015 2016 2017 2015 2016 2010 2011 2009 2017 2016 2017 2008 2015
## [11012] 2015 2015 2016 2017 2009 2008 2013 2015 2016 2016 2016 2017 2017
## [11025] 2014 2014 2015 2015 2017 2012 2015 2015 2009 2009 2016 2015 2011
## [11038] 2012 2010 2014 2016 2017 2013 2013 2015 2016 2016 2017 2017 2014
## [11051] 2015 2016 2016 2014 2016 2015 2008 2013 2014 2017 2009 2009 2015
## [11064] 2016 2015 2013 2010 2012 2012 2015 2012 2016 2016 2011 2013 2016
## [11077] 2015 2010 2017 2007 2015 2014 2015 2016 2016 2013 2013 2015 2012
## [11090] 2015 2011 2013 2015 2012 2011 2007 2016 2017 2017 2012 2015 2009
## [11103] 2015 2016 2017 2008 2012 2017 2012 2013 2014 2017 2012 2015 2016
## [11116] 2017 2012 2015 2012 2017 2012 2011 2014 2016 2016 2016 2016 2017
## [11129] 2011 2013 2016 2008 2015 2015 2015 2012 2008 2015 2015 2017 2016
## [11142] 2016 2016 2015 2015 2015 2010 2015 2015 2008 2016 2017 2015 2016
## [11155] 2016 2015 2015 2016 2017 2015 2009 2014 2009 2016 2013 2015 2016
## [11168] 2017 2009 2013 2015 2009 2008 2007 2016 2015 2011 2016 2016 2008
## [11181] 2013 2016 2014 2017 2009 2010 2016 2017 2015 2016 2017 2008 2009
## [11194] 2013 2017 2017 2011 2010 2008 2016 2007 2008 2015 2016 2009 2017
## [11207] 2016 2016 2015 2015 2013 2017 2009 2016 2017 2008 2015 2016 2016
## [11220] 2016 2008 2015 2017 2008 2015 2016 2015 2017 2010 2011 2016 2016
## [11233] 2015 2011 2010 2013 2009 2013 2010 2015 2008 2017 2010 2009 2011
## [11246] 2008 2011 2015 2016 2017 2009 2009 2017 2008 2009 2015 2017 2012
## [11259] 2007 2015 2007 2010 2013 2015 2016 2016 2017 2015 2017 2015 2017
## [11272] 2010 2016 2008 2015 2017 2017 2017 2017 2010 2016 2015 2017 2008
## [11285] 2012 2014 2017 2012 2012 2017 2017 2011 2008 2015 2015 2015 2012
## [11298] 2011 2007 2015 2015 2015 2016 2017 2012 2014 2014 2015 2014 2015
## [11311] 2017 2013 2007 2009 2015 2015 2017 2013 2015 2014 2017 2015 2008
## [11324] 2013 2009 2017 2008 2016 2016 2010 2010 2007 2016 2015 2011 2015
## [11337] 2016 2011 2007 2011 2010 2016 2017 2009 2014 2017 2009 2012 2016
## [11350] 2015 2009 2015 2017 2017 2017 2015 2010 2016 2009 2015 2015 2016
## [11363] 2017 2014 2016 2012 2008 2009 2015 2008 2015 2007 2015 2016 2011
## [11376] 2014 2016 2017 2011 2016 2016 2015 2012 2012 2015 2007 2009 2016
## [11389] 2016 2009 2013 2014 2017 2012 2017 2007 2016 2007 2013 2013 2017
## [11402] 2017 2016 2009 2008 2016 2017 2017 2009 2015 2016 2016 2011 2015
## [11415] 2017 2009 2007 2015 2013 2017 2007 2016 2015 2009 2015 2016 2010
## [11428] 2007 2008 2010 2015 2010 2012 2011 2015 2017 2017 2010 2009 2012
## [11441] 2015 2009 2009 2015 2015 2016 2017 2014 2015 2016 2016 2016 2008
## [11454] 2017 2008 2017 2017 2017 2011 2011 2013 2013 2015 2016 2012 2013
## [11467] 2017 2009 2014 2007 2016 2012 2008 2013 2016 2011 2013 2015 2009
## [11480] 2015 2017 2014 2012 2009 2008 2007 2013 2010 2007 2013 2014 2015
## [11493] 2016 2017 2016 2017 2015 2016 2016 2017 2015 2016 2015 2008 2008
## [11506] 2015 2010 2008 2015 2017 2012 2007 2010 2015 2017 2012 2016 2017
## [11519] 2017 2009 2016 2011 2012 2014 2017 2011 2012 2017 2016 2009 2016
## [11532] 2015 2017 2007 2015 2016 2014 2016 2017 2007 2014 2015 2015 2017
## [11545] 2017 2010 2011 2007 2016 2011 2012 2013 2015 2017 2015 2011 2012
## [11558] 2017 2017 2017 2013 2015 2015 2011 2017 2007 2015 2016 2011 2007
## [11571] 2009 2009 2013 2011 2013 2015 2017 2015 2007 2015 2016 2017 2012
## [11584] 2012 2007 2015 2017 2007 2007 2016 2016 2007 2016 2016 2017 2017
## [11597] 2017 2017 2017 2017 2007 2011 2012 2014 2016 2016 2016 2010 2017
## [11610] 2009 2016 2009 2008 2010 2009 2008 2013 2015 2012 2010 2015 2015
## [11623] 2015 2015 2017 2007 2012 2010 2007 2012 2010 2017 2009 2007 2010
## [11636] 2014 2017 2015 2016 2016 2016 2017 2013 2016 2017 2007 2015 2015
## [11649] 2016 2016 2009 2015 2016 2012 2007 2013 2014 2015 2017 2014 2015
## [11662] 2016 2016 2007 2015 2008 2015 2017 2016 2016 2017 2009 2014 2015
## [11675] 2017 2014 2014 2015 2011 2014 2016 2017 2010 2012 2015 2007 2015
## [11688] 2015 2017 2011 2011 2009 2016 2014 2014 2016 2016 2017 2011 2010
## [11701] 2015 2016 2009 2008 2017 2017 2009 2016 2017 2011 2010 2015 2017
## [11714] 2017 2017 2015 2015 2016 2017 2017 2009 2013 2017 2007 2010 2012
## [11727] 2007 2011 2017 2009 2015 2007 2012 2015 2015 2016 2015 2017 2008
## [11740] 2007 2009 2009 2008 2017 2017 2017 2017 2009 2017 2010 2009 2017
## [11753] 2007 2007 2017 2017 2016 2017 2016 2011 2008 2014 2015 2016 2012
## [11766] 2016 2016 2016 2017 2011 2012 2013 2017 2017 2011 2013 2014 2016
## [11779] 2008 2010 2015 2011 2011 2013 2015 2017 2017 2017 2007 2015 2013
## [11792] 2015 2016 2017 2017 2015 2016 2015 2016 2009 2017 2016 2017 2016
## [11805] 2015 2009 2007 2016 2017 2017 2014 2007 2010 2013 2016 2010 2016
## [11818] 2017 2007 2011 2016 2010 2015 2007 2014 2014 2014 2017 2012 2011
## [11831] 2014 2011 2016 2017 2016 2007 2013 2015 2017 2017 2015 2017 2017
## [11844] 2010 2016 2011 2007 2015 2008 2011 2016 2017 2015 2009 2017 2015
## [11857] 2007 2017 2011 2017 2009 2015 2017 2017 2017 2009 2015 2016 2017
## [11870] 2009 2009 2017 2007 2012 2008 2012 2013 2010 2016 2016 2010 2009
## [11883] 2011 2010 2016 2010 2014 2014 2010 2011 2016 2016 2015 2015 2015
## [11896] 2016 2016 2017 2015 2016 2017 2013 2007 2012 2014 2016 2007 2017
## [11909] 2015 2015 2017 2010 2013 2017 2010 2017 2012 2013 2016 2017 2017
## [11922] 2017 2015 2015 2010 2015 2012 2010 2015 2016 2008 2008 2010 2015
## [11935] 2008 2015 2015 2017 2017 2015 2015 2015 2017 2010 2009 2015 2015
## [11948] 2017 2016 2017 2007 2016 2016 2008 2009 2013 2015 2017 2008 2016
## [11961] 2011 2016 2014 2014 2016 2009 2016 2016 2009 2010 2016 2017 2015
## [11974] 2017 2017 2008 2009 2013 2013 2015 2010 2010 2017 2008 2015 2015
## [11987] 2015 2016 2010 2016 2008 2007 2014 2010 2009 2017 2016 2017 2015
## [12000] 2014 2016 2013 2015 2017 2010 2008 2008 2011 2015 2008 2010 2008
## [12013] 2014 2017 2008 2015 2017 2015 2017 2012 2017 2017 2012 2015 2016
## [12026] 2016 2016 2015 2013 2015 2010 2015 2016 2017 2017 2007 2017 2017
## [12039] 2007 2013 2015 2007 2015 2010 2012 2016 2015 2017 2009 2010 2013
## [12052] 2014 2010 2007 2007 2016 2016 2011 2015 2016 2017 2017 2012 2012
## [12065] 2014 2015 2015 2017 2008 2009 2013 2012 2017 2017 2010 2016 2016
## [12078] 2012 2016 2016 2017 2015 2015 2016 2011 2015 2016 2009 2012 2014
## [12091] 2014 2016 2015 2017 2012 2009 2013 2015 2015 2016 2017 2009 2015
## [12104] 2016 2010 2008 2015 2016 2014 2010 2014 2015 2007 2010 2015 2016
## [12117] 2014 2014 2015 2016 2015 2009 2013 2014 2010 2012 2017 2017 2010
## [12130] 2007 2011 2012 2014 2014 2007 2010 2010 2014 2015 2008 2016 2017
## [12143] 2007 2012 2015 2011 2007 2012 2007 2017 2013 2013 2015 2016 2016
## [12156] 2015 2012 2008 2008 2015 2009 2012 2014 2016 2016 2010 2011 2010
## [12169] 2015 2015 2008 2015 2017 2017 2017 2011 2011 2016 2017 2015 2017
## [12182] 2011 2015 2016 2015 2016 2016 2012 2015 2010 2008 2007 2015 2017
## [12195] 2011 2008 2007 2014 2015 2015 2016 2017 2008 2015 2012 2012 2007
## [12208] 2014 2015 2015 2015 2016 2017 2015 2017 2014 2015 2007 2017 2017
## [12221] 2017 2015 2017 2007 2015 2015 2016 2015 2017 2011 2012 2008 2016
## [12234] 2016 2016 2014 2015 2015 2015 2017 2010 2011 2008 2016 2012 2012
## [12247] 2017 2016 2015 2008 2015 2016 2017 2017 2016 2017 2013 2017 2013
## [12260] 2011 2017 2011 2015 2016 2012 2012 2012 2010 2017 2011 2016 2017
## [12273] 2015 2016 2017 2017 2011 2016 2010 2008 2008 2015 2016 2016 2015
## [12286] 2010 2012 2014 2009 2015 2017 2016 2017 2017 2011 2017 2013 2015
## [12299] 2015 2008 2015 2016 2014 2015 2016 2016 2016 2012 2017 2016 2016
## [12312] 2016 2017 2014 2015 2017 2017 2015 2016 2016 2017 2017 2011 2016
## [12325] 2016 2015 2007 2013 2015 2008 2008 2008 2015 2016 2012 2007 2015
## [12338] 2007 2015 2017 2017 2010 2015 2016 2017 2017 2007 2016 2017 2014
## [12351] 2017 2008 2009 2017 2017 2012 2015 2017 2017 2007 2013 2007 2010
## [12364] 2015 2017 2012 2009 2013 2015 2015 2010 2009 2015 2016 2012 2015
## [12377] 2015 2017 2008 2017 2012 2010 2015 2010 2007 2014 2015 2016 2017
## [12390] 2017 2015 2017 2016 2008 2008 2009 2009 2015 2007 2009 2016 2016
## [12403] 2012 2011 2017 2015 2010 2012 2012 2015 2007 2011 2015 2017 2007
## [12416] 2007 2016 2015 2016 2017 2014 2015 2013 2015 2016 2012 2007 2015
## [12429] 2017 2012 2010 2016 2016 2017 2012 2010 2016 2012 2016 2008 2015
## [12442] 2015 2017 2016 2017 2017 2011 2010 2017 2017 2013 2014 2016 2015
## [12455] 2015 2015 2017 2008 2017 2012 2010 2015 2008 2009 2011 2017 2017
## [12468] 2015 2016 2017 2016 2016 2015 2017 2015 2010 2007 2007 2007 2016
## [12481] 2015 2017 2017 2016 2017 2011 2010 2017 2013 2010 2014 2014 2016
## [12494] 2017 2008 2012 2016 2016 2012 2009 2017 2017 2017 2017 2014 2014
## [12507] 2007 2016 2015 2017 2017 2014 2014 2016 2011 2010 2017 2017 2007
## [12520] 2014 2015 2017 2010 2011 2016 2017 2017 2013 2010 2016 2014 2016
## [12533] 2015 2015 2017 2015 2017 2012 2016 2016 2008 2016 2014 2016 2012
## [12546] 2016 2011 2016 2017 2017 2008 2012 2015 2016 2017 2010 2008 2015
## [12559] 2017 2017 2013 2017 2007 2007 2016 2017 2007 2015 2017 2013 2009
## [12572] 2013 2015 2017 2017 2012 2015 2015 2016 2010 2012 2010 2007 2009
## [12585] 2016 2011 2008 2013 2016 2017 2007 2017 2010 2016 2011 2017 2015
## [12598] 2015 2012 2014 2016 2016 2017 2017 2017 2015 2017 2013 2016 2013
## [12611] 2014 2016 2008 2017 2017 2008 2014 2017 2017 2009 2015 2016 2016
## [12624] 2012 2007 2016 2017 2015 2017 2009 2017 2017 2016 2015 2017 2007
## [12637] 2010 2010 2011 2014 2015 2012 2007 2016 2017 2017 2015 2017 2017
## [12650] 2011 2017 2012 2017 2017 2016 2016 2016 2010 2010 2016 2016 2016
## [12663] 2015 2017 2015 2017 2012 2009 2007 2016 2012 2016 2016 2016 2017
## [12676] 2012 2013 2015 2016 2017 2010 2016 2017 2016 2015 2016 2008 2016
## [12689] 2016 2017 2017 2013 2016 2011 2007 2015 2015 2007 2010 2009 2014
## [12702] 2017 2007 2009 2010 2008 2016 2010 2013 2017 2011 2008 2014 2016
## [12715] 2017 2008 2017 2016 2016 2009 2009 2016 2017 2008 2012 2013 2017
## [12728] 2009 2012 2015 2015 2017 2010 2015 2010 2015 2008 2011 2016 2013
## [12741] 2016 2016 2008 2007 2011 2012 2016 2015 2015 2015 2016 2016 2017
## [12754] 2017 2012 2017 2007 2010 2017 2009 2017 2016 2016 2010 2016 2013
## [12767] 2016 2017 2013 2016 2009 2008 2015 2015 2010 2014 2016 2017 2017
## [12780] 2016 2015 2014 2015 2015 2008 2007 2008 2008 2014 2011 2008 2014
## [12793] 2016 2015 2008 2015 2012 2009 2011 2014 2015 2009 2007 2013 2015
## [12806] 2014 2016 2012 2009 2009 2016 2016 2017 2007 2013 2016 2016 2016
## [12819] 2017 2017 2011 2010 2007 2008 2010 2009 2015 2017 2013 2017 2011
## [12832] 2015 2017 2015 2016 2015 2017 2017 2016 2017 2017 2011 2016 2010
## [12845] 2016 2016 2017 2007 2015 2011 2007 2016 2017 2011 2008 2016 2016
## [12858] 2013 2015 2013 2007 2008 2009 2007 2014 2016 2017 2017 2007 2015
## [12871] 2015 2017 2016 2009 2015 2017 2012 2007 2015 2017 2015 2017 2008
## [12884] 2015 2015 2016 2010 2011 2017 2010 2015 2017 2011 2016 2015 2008
## [12897] 2009 2007 2010 2008 2013 2017 2017 2010 2015 2010 2015 2015 2017
## [12910] 2012 2017 2007 2008 2015 2011 2007 2015 2010 2015 2015 2016 2017
## [12923] 2007 2012 2016 2015 2015 2012 2013 2015 2017 2013 2015 2016 2016
## [12936] 2017 2015 2017 2017 2012 2016 2016 2015 2017 2017 2012 2016 2009
## [12949] 2016 2017 2017 2015 2014 2017 2015 2017 2011 2009 2015 2017 2012
## [12962] 2009 2012 2014 2016 2016 2017 2010 2009 2015 2011 2007 2012 2013
## [12975] 2016 2016 2017 2012 2007 2008 2014 2014 2011 2007 2010 2013 2014
## [12988] 2016 2015 2017 2008 2015 2010 2010 2012 2008 2016 2017 2008 2015
## [13001] 2016 2015 2017 2007 2015 2015 2017 2013 2016 2017 2013 2017 2017
## [13014] 2011 2009 2007 2016 2011 2014 2015 2017 2010 2013 2014 2015 2016
## [13027] 2016 2016 2008 2017 2008 2010 2017 2009 2016 2016 2011 2016 2010
## [13040] 2017 2011 2007 2015 2007 2015 2013 2017 2008 2010 2012 2011 2010
## [13053] 2009 2007 2016 2009 2007 2016 2007 2009 2009 2016 2017 2016 2014
## [13066] 2016 2016 2017 2010 2013 2015 2009 2010 2016 2017 2008 2016 2017
## [13079] 2017 2011 2015 2016 2017 2016 2017 2017 2016 2017 2009 2007 2011
## [13092] 2011 2008 2016 2017 2011 2016 2015 2017 2016 2017 2017 2017 2015
## [13105] 2017 2009 2008 2008 2008 2015 2015 2016 2012 2015 2013 2012 2016
## [13118] 2007 2011 2012 2014 2015 2017 2015 2016 2017 2007 2015 2016 2017
## [13131] 2010 2015 2011 2016 2016 2011 2014 2016 2017 2008 2008 2008 2009
## [13144] 2010 2008 2012 2016 2016 2016 2010 2012 2017 2017 2009 2008 2010
## [13157] 2015 2016 2015 2016 2016 2017 2013 2011 2016 2017 2009 2007 2016
## [13170] 2013 2007 2017 2010 2017 2016 2017 2009 2008 2017 2012 2013 2014
## [13183] 2015 2016 2017 2012 2010 2015 2016 2016 2016 2017 2009 2007 2015
## [13196] 2015 2015 2016 2015 2015 2011 2012 2015 2007 2016 2008 2013 2016
## [13209] 2016 2009 2009 2015 2008 2015 2016 2016 2007 2016 2017 2015 2017
## [13222] 2012 2016 2010 2015 2012 2009 2015 2017 2009 2015 2016 2013 2014
## [13235] 2017 2007 2013 2009 2012 2017 2016 2016 2009 2014 2014 2015 2010
## [13248] 2016 2014 2011 2017 2015 2015 2016 2013 2015 2017 2015 2016 2017
## [13261] 2017 2008 2007 2016 2016 2012 2013 2015 2017 2017 2013 2016 2015
## [13274] 2017 2017 2016 2017 2017 2010 2010 2016 2017 2009 2017 2014 2015
## [13287] 2013 2017 2017 2017 2009 2007 2009 2016 2016 2015 2015 2015 2016
## [13300] 2017 2017 2012 2007 2013 2016 2009 2011 2017 2010 2007 2014 2015
## [13313] 2017 2017 2017 2011 2014 2014 2016 2007 2008 2009 2015 2017 2008
## [13326] 2016 2017 2017 2008 2009 2011 2015 2016 2016 2017 2015 2016 2017
## [13339] 2008 2011 2010 2013 2014 2016 2015 2017 2014 2015 2017 2017 2010
## [13352] 2015 2015 2017 2010 2007 2007 2012 2017 2011 2015 2011 2016 2016
## [13365] 2016 2017 2011 2016 2015 2012 2008 2014 2016 2016 2017 2017 2010
## [13378] 2007 2009 2015 2017 2009 2015 2015 2015 2016 2007 2014 2016 2017
## [13391] 2009 2015 2017 2009 2011 2014 2017 2009 2014 2017 2017 2012 2017
## [13404] 2017 2017 2007 2010 2017 2008 2013 2015 2016 2016 2008 2015 2015
## [13417] 2012 2017 2015 2016 2017 2017 2010 2012 2016 2017 2008 2016 2009
## [13430] 2015 2017 2017 2007 2015 2017 2017 2008 2014 2015 2015 2017 2009
## [13443] 2007 2014 2015 2010 2009 2017 2015 2015 2016 2017 2012 2016 2016
## [13456] 2007 2013 2017 2012 2010 2007 2010 2010 2017 2012 2014 2016 2017
## [13469] 2008 2015 2015 2016 2010 2008 2015 2017 2015 2017 2016 2011 2014
## [13482] 2017 2010 2007 2016 2014 2007 2008 2013 2015 2009 2007 2009 2010
## [13495] 2014 2016 2017 2010 2008 2013 2015 2015 2011 2016 2017 2015 2009
## [13508] 2011 2013 2016 2017 2010 2015 2016 2017 2016 2017 2017 2017 2017
## [13521] 2011 2017 2008 2009 2015 2016 2012 2008 2017 2010 2009 2013 2015
## [13534] 2017 2017 2017 2007 2016 2016 2016 2011 2008 2010 2012 2016 2016
## [13547] 2008 2016 2017 2017 2017 2008 2008 2015 2014 2017 2007 2015 2016
## [13560] 2012 2015 2017 2008 2017 2010 2008 2010 2009 2011 2009 2008 2016
## [13573] 2017 2017 2017 2011 2007 2010 2014 2015 2017 2017 2009 2008 2014
## [13586] 2016 2012 2009 2011 2010 2014 2016 2016 2017 2015 2013 2016 2015
## [13599] 2016 2017 2011 2014 2015 2011 2014 2007 2007 2008 2011 2015 2017
## [13612] 2017 2009 2011 2012 2016 2008 2015 2016 2007 2014 2015 2017 2013
## [13625] 2013 2016 2011 2012 2016 2016 2017 2013 2015 2017 2008 2012 2015
## [13638] 2007 2016 2017 2014 2011 2015 2016 2017 2007 2010 2017 2017 2009
## [13651] 2011 2016 2016 2017 2007 2009 2016 2011 2008 2017 2015 2015 2017
## [13664] 2015 2012 2009 2013 2015 2015 2017 2008 2014 2015 2016 2017 2015
## [13677] 2015 2009 2016 2016 2016 2016 2016 2012 2014 2017 2009 2013 2017
## [13690] 2017 2017 2017 2014 2014 2011 2010 2012 2016 2007 2015 2011 2009
## [13703] 2013 2017 2007 2012 2015 2010 2016 2009 2017 2015 2016 2016 2010
## [13716] 2016 2016 2015 2017 2013 2016 2017 2017 2010 2015 2015 2012 2013
## [13729] 2014 2015 2007 2011 2012 2014 2014 2016 2017 2007 2009 2009 2014
## [13742] 2017 2016 2016 2010 2007 2009 2012 2016 2017 2015 2017 2015 2015
## [13755] 2008 2010 2015 2015 2009 2010 2017 2016 2017 2010 2016 2016 2017
## [13768] 2017 2017 2017 2015 2010 2013 2015 2017 2015 2016 2015 2015 2017
## [13781] 2009 2007 2015 2016 2012 2013 2015 2017 2014 2017 2010 2016 2010
## [13794] 2007 2017 2008 2012 2017 2007 2009 2015 2015 2017 2011 2015 2016
## [13807] 2016 2016 2017 2015 2010 2015 2017 2011 2011 2009 2014 2015 2017
## [13820] 2013 2016 2013 2008 2007 2010 2013 2017 2016 2016 2007 2015 2014
## [13833] 2015 2017 2017 2010 2016 2016 2010 2009 2016 2015 2015 2013 2015
## [13846] 2010 2017 2007 2015 2016 2008 2017 2007 2017 2017 2009 2016 2010
## [13859] 2010 2017 2012 2009 2016 2008 2009 2011 2015 2016 2011 2013 2013
## [13872] 2016 2015 2017 2016 2015 2008 2015 2017 2015 2016 2015 2015 2013
## [13885] 2015 2015 2017 2015 2015 2012 2015 2016 2017 2009 2008 2015 2016
## [13898] 2017 2017 2008 2007 2016 2009 2011 2012 2009 2016 2012 2008 2017
## [13911] 2015 2012 2012 2017 2017 2008 2012 2010 2016 2016 2007 2015 2015
## [13924] 2016 2013 2010 2017 2008 2013 2014 2017 2007 2013 2017 2017 2007
## [13937] 2013 2017 2009 2015 2016 2017 2016 2008 2015 2016 2007 2007 2016
## [13950] 2016 2015 2017 2010 2011 2015 2017 2009 2010 2016 2017 2010 2015
## [13963] 2016 2017 2007 2017 2007 2007 2015 2015 2011 2013 2015 2017 2017
## [13976] 2012 2016 2011 2008 2009 2007 2013 2016 2007 2012 2010 2016 2016
## [13989] 2017 2015 2008 2010 2009 2017 2017 2008 2015 2017 2012 2007 2009
## [14002] 2011 2015 2016 2017 2016 2015 2015 2011 2012 2015 2015 2015 2010
## [14015] 2014 2015 2016 2017 2013 2016 2016 2009 2016 2017 2012 2008 2013
## [14028] 2016 2016 2015 2017 2017 2014 2016 2017 2017 2010 2015 2014 2016
## [14041] 2017 2017 2009 2015 2013 2015 2010 2008 2015 2008 2017 2017 2015
## [14054] 2016 2015 2015 2016 2017 2017 2007 2015 2015 2012 2017 2017 2008
## [14067] 2017 2014 2015 2017 2017 2014 2016 2017 2012 2015 2009 2011 2016
## [14080] 2016 2016 2016 2016 2016 2011 2015 2017 2017 2007 2015 2016 2016
## [14093] 2009 2009 2015 2015 2016 2008 2015 2016 2016 2017 2011 2012 2013
## [14106] 2017 2009 2007 2008 2016 2007 2016 2017 2011 2015 2007 2013 2015
## [14119] 2016 2016 2016 2015 2007 2011 2016 2016 2009 2007 2011 2016 2017
## [14132] 2017 2013 2014 2015 2016 2017 2016 2016 2016 2009 2015 2007 2016
## [14145] 2017 2015 2016 2013 2015 2017 2012 2012 2017 2015 2009 2010 2008
## [14158] 2009 2017 2007 2011 2017 2011 2016 2017 2007 2016 2013 2015 2017
## [14171] 2007 2011 2015 2017 2009 2011 2017 2012 2016 2017 2007 2008 2009
## [14184] 2009 2014 2014 2015 2016 2015 2009 2010 2012 2016 2017 2009 2010
## [14197] 2007 2008 2016 2016 2015 2015 2015 2017 2012 2009 2016 2016 2013
## [14210] 2015 2016 2017 2017 2017 2007 2016 2011 2007 2013 2011 2007 2016
## [14223] 2017 2015 2017 2011 2013 2016 2013 2016 2015 2016 2012 2010 2010
## [14236] 2016 2017 2012 2015 2015 2008 2011 2007 2008 2016 2017 2010 2012
## [14249] 2016 2008 2013 2008 2012 2008 2014 2015 2016 2016 2017 2017 2009
## [14262] 2016 2016 2014 2016 2008 2017 2015 2011 2017 2007 2015 2017 2016
## [14275] 2017 2011 2008 2012 2012 2016 2011 2012 2016 2017 2011 2013 2011
## [14288] 2013 2014 2007 2016 2017 2015 2009 2016 2017 2017 2012 2010 2008
## [14301] 2008 2014 2016 2011 2010 2011 2016 2015 2017 2015 2015 2015 2008
## [14314] 2016 2015 2014 2016 2015 2017 2007 2010 2014 2016 2016 2016 2012
## [14327] 2013 2015 2016 2009 2017 2009 2009 2016 2017 2017 2017 2016 2015
## [14340] 2010 2011 2013 2015 2009 2012 2010 2013 2016 2015 2017 2007 2014
## [14353] 2016 2016 2017 2012 2016 2009 2011 2016 2017 2008 2012 2010 2007
## [14366] 2008 2012 2015 2015 2017 2017 2007 2011 2012 2012 2014 2007 2016
## [14379] 2008 2010 2017 2015 2016 2016 2016 2017 2013 2016 2008 2016 2014
## [14392] 2014 2016 2017 2009 2016 2009 2011 2017 2009 2008 2014 2009 2015
## [14405] 2017 2017 2017 2008 2009 2007 2015 2010 2010 2012 2015 2017 2013
## [14418] 2012 2010 2007 2016 2017 2014 2009 2013 2016 2017 2017 2013 2016
## [14431] 2017 2008 2014 2016 2017 2012 2017 2016 2007 2016 2012 2016 2017
## [14444] 2008 2012 2015 2016 2016 2011 2012 2012 2009 2010 2016 2017 2017
## [14457] 2008 2013 2015 2016 2010 2007 2008 2010 2016 2015 2017 2008 2015
## [14470] 2017 2016 2015 2017 2010 2017 2012 2009 2009 2014 2016 2017 2013
## [14483] 2016 2017 2017 2017 2017 2016 2016 2013 2017 2009 2012 2008 2017
## [14496] 2008 2010 2010 2016 2007 2009 2015 2016 2016 2008 2010 2015 2012
## [14509] 2009 2013 2017 2011 2011 2014 2016 2009 2009 2013 2015 2012 2012
## [14522] 2009 2016 2016 2007 2007 2012 2015 2016 2017 2017 2015 2016 2016
## [14535] 2017 2017 2015 2015 2017 2017 2011 2015 2015 2017 2015 2013 2015
## [14548] 2016 2016 2017 2007 2016 2009 2007 2014 2015 2012 2017 2017 2015
## [14561] 2015 2007 2015 2016 2017 2011 2010 2010 2016 2010 2011 2015 2017
## [14574] 2012 2017 2007 2015 2016 2015 2017 2011 2015 2017 2007 2017 2015
## [14587] 2016 2016 2017 2009 2014 2014 2015 2009 2015 2017 2010 2014 2010
## [14600] 2010 2014 2016 2008 2007 2009 2011 2015 2015 2015 2008 2017 2007
## [14613] 2010 2016 2012 2009 2007 2015 2008 2008 2017 2012 2007 2008 2015
## [14626] 2015 2016 2010 2017 2009 2013 2016 2016 2017 2008 2008 2007 2015
## [14639] 2017 2017 2010 2012 2013 2013 2016 2012 2007 2008 2011 2009 2011
## [14652] 2016 2017 2007 2015 2016 2007 2016 2016 2016 2013 2008 2017 2008
## [14665] 2016 2009 2008 2011 2012 2008 2013 2015 2017 2011 2007 2011 2016
## [14678] 2017 2012 2014 2016 2016 2016 2017 2017 2011 2014 2016 2016 2012
## [14691] 2013 2017 2012 2010 2015 2010 2012 2016 2016 2017 2007 2015 2017
## [14704] 2013 2015 2017 2017 2011 2010 2010 2010 2010 2017 2009 2009 2015
## [14717] 2016 2008 2015 2015 2017 2017 2016 2015 2015 2016 2016 2016 2017
## [14730] 2016 2017 2010 2016 2016 2016 2017 2007 2016 2015 2009 2015 2015
## [14743] 2016 2016 2007 2017 2010 2014 2017 2017 2007 2013 2017 2007 2014
## [14756] 2014 2017 2011 2017 2017 2008 2011 2014 2015 2015 2016 2017 2017
## [14769] 2012 2016 2012 2010 2016 2010 2011 2015 2015 2016 2010 2008 2014
## [14782] 2016 2010 2011 2015 2007 2008 2015 2015 2017 2011 2014 2017 2017
## [14795] 2016 2011 2011 2015 2013 2016 2017 2012 2010 2014 2016 2009 2013
## [14808] 2015 2016 2017 2011 2008 2008 2016 2016 2015 2013 2007 2011 2017
## [14821] 2017 2015 2009 2010 2015 2016 2010 2014 2016 2016 2007 2013 2017
## [14834] 2016 2008 2010 2016 2015 2016 2010 2015 2008 2017 2010 2013 2015
## [14847] 2009 2008 2011 2009 2016 2016 2010 2010 2014 2016 2009 2016 2012
## [14860] 2011 2007 2014 2007 2013 2016 2015 2016 2012 2013 2016 2011 2016
## [14873] 2009 2017 2012 2015 2009 2008 2015 2016 2013 2012 2015 2016 2017
## [14886] 2017 2008 2014 2016 2011 2011 2015 2017 2017 2011 2008 2015 2009
## [14899] 2010 2009 2017 2009 2008 2015 2015 2015 2016 2016 2015 2015 2013
## [14912] 2015 2016 2007 2008 2008 2015 2017 2015 2016 2015 2009 2015 2010
## [14925] 2007 2015 2007 2012 2007 2011 2016 2017 2011 2008 2009 2014 2016
## [14938] 2015 2012 2007 2007 2015 2017 2007 2007 2010 2013 2015 2010 2010
## [14951] 2009 2015 2017 2017 2010 2008 2013 2016 2007 2017 2009 2011 2009
## [14964] 2017 2017 2009 2008 2011 2014 2015 2016 2016 2016 2016 2007 2010
## [14977] 2010 2017 2015 2017 2013 2017 2012 2015 2010 2016 2017 2012 2008
## [14990] 2015 2016 2017 2009 2017 2013 2015 2008 2016 2017 2017 2017 2017
## [15003] 2013 2016 2016 2008 2012 2014 2015 2015 2015 2016 2016 2016 2015
## [15016] 2008 2014 2015 2017 2014 2016 2011 2015 2015 2016 2011 2016 2017
## [15029] 2009 2015 2009 2013 2017 2017 2008 2017 2017 2010 2015 2015 2017
## [15042] 2015 2015 2007 2015 2015 2017 2017 2009 2014 2017 2017 2011 2015
## [15055] 2017 2010 2010 2016 2009 2008 2016 2016 2017 2014 2016 2015 2016
## [15068] 2017 2013 2016 2016 2017 2011 2013 2016 2017 2007 2011 2014 2014
## [15081] 2016 2017 2016 2016 2016 2017 2008 2015 2015 2016 2015 2016 2008
## [15094] 2010 2008 2015 2008 2011 2012 2012 2014 2016 2007 2016 2017 2011
## [15107] 2013 2016 2011 2008 2016 2017 2010 2013 2015 2016 2011 2015 2013
## [15120] 2016 2016 2016 2017 2016 2017 2016 2009 2007 2016 2016 2017 2013
## [15133] 2015 2017 2009 2015 2008 2015 2016 2016 2017 2007 2013 2015 2015
## [15146] 2016 2017 2013 2016 2016 2010 2015 2016 2015 2017 2009 2016 2016
## [15159] 2017 2017 2015 2015 2015 2012 2008 2015 2007 2013 2013 2016 2016
## [15172] 2016 2009 2013 2016 2017 2010 2017 2017 2017 2012 2015 2016 2016
## [15185] 2015 2016 2011 2011 2013 2016 2017 2007 2017 2017 2012 2010 2016
## [15198] 2016 2011 2015 2015 2017 2010 2016 2017 2015 2008 2014 2016 2017
## [15211] 2017 2017 2013 2008 2015 2017 2017 2017 2012 2017 2014 2016 2017
## [15224] 2017 2015 2012 2015 2012 2010 2016 2011 2017 2017 2016 2009 2014
## [15237] 2015 2014 2016 2015 2013 2017 2009 2015 2009 2016 2012 2008 2014
## [15250] 2008 2007 2015 2015 2016 2008 2012 2015 2016 2017 2008 2012 2014
## [15263] 2015 2009 2015 2013 2016 2016 2016 2009 2010 2015 2017 2008 2017
## [15276] 2017 2009 2016 2014 2016 2017 2015 2011 2011 2009 2008 2014 2017
## [15289] 2011 2013 2017 2007 2017 2016 2016 2014 2015 2008 2010 2014 2014
## [15302] 2016 2017 2017 2017 2011 2011 2016 2010 2008 2013 2011 2015 2009
## [15315] 2010 2009 2011 2015 2016 2010 2010 2016 2011 2015 2016 2015 2017
## [15328] 2015 2017 2008 2012 2015 2010 2016 2008 2012 2013 2015 2017 2012
## [15341] 2010 2017 2017 2017 2010 2013 2015 2009 2008 2008 2015 2016 2016
## [15354] 2017 2016 2017 2012 2012 2011 2016 2017 2017 2016 2007 2015 2015
## [15367] 2016 2016 2012 2009 2017 2017 2011 2012 2013 2016 2015 2017 2007
## [15380] 2013 2012 2017 2017 2016 2016 2016 2017 2010 2013 2016 2017 2009
## [15393] 2010 2008 2016 2016 2015 2016 2015 2008 2008 2017 2015 2016 2016
## [15406] 2011 2007 2015 2013 2016 2017 2010 2017 2015 2016 2017 2010 2012
## [15419] 2007 2017 2011 2014 2016 2016 2016 2010 2009 2008 2007 2016 2010
## [15432] 2007 2016 2015 2017 2009 2010 2008 2014 2015 2007 2011 2008 2010
## [15445] 2015 2010 2016 2017 2009 2010 2014 2016 2017 2012 2010 2010 2015
## [15458] 2016 2017 2017 2010 2010 2015 2016 2015 2017 2017 2015 2015 2016
## [15471] 2016 2016 2017 2016 2017 2011 2015 2016 2012 2016 2016 2015 2010
## [15484] 2014 2015 2016 2016 2017 2017 2012 2011 2016 2017 2007 2007 2010
## [15497] 2010 2011 2016 2016 2015 2008 2007 2015 2015 2015 2016 2010 2015
## [15510] 2017 2014 2017 2017 2017 2012 2014 2010 2009 2014 2015 2015 2011
## [15523] 2015 2016 2017 2008 2017 2007 2014 2014 2016 2016 2015 2007 2013
## [15536] 2012 2015 2016 2010 2008 2013 2016 2010 2008 2015 2016 2017 2013
## [15549] 2014 2015 2016 2017 2007 2011 2007 2010 2015 2017 2017 2011 2010
## [15562] 2014 2012 2016 2007 2013 2014 2009 2011 2015 2017 2017 2008 2016
## [15575] 2017 2012 2017 2017 2012 2014 2014 2015 2014 2016 2011 2016 2017
## [15588] 2017 2017 2011 2013 2013 2014 2016 2017 2017 2008 2011 2015 2017
## [15601] 2009 2017 2008 2013 2007 2011 2016 2017 2017 2008 2016 2017 2012
## [15614] 2015 2016 2007 2007 2016 2007 2016 2017 2015 2017 2017 2007 2015
## [15627] 2009 2012 2013 2015 2007 2016 2017 2017 2017 2017 2008 2012 2015
## [15640] 2017 2017 2016 2017 2016 2008 2015 2015 2016 2015 2015 2014 2016
## [15653] 2015 2017 2009 2017 2017 2012 2016 2017 2008 2014 2015 2010 2013
## [15666] 2009 2011 2014 2017 2016 2007 2011 2017 2011 2011 2009 2016 2015
## [15679] 2013 2015 2015 2015 2016 2017 2013 2017 2015 2007 2016 2016 2007
## [15692] 2013 2009 2015 2016 2017 2011 2007 2009 2007 2015 2016 2017 2017
## [15705] 2017 2011 2017 2010 2015 2015 2016 2015 2016 2011 2016 2008 2017
## [15718] 2008 2015 2016 2017 2007 2007 2016 2015 2016 2015 2015 2017 2010
## [15731] 2017 2013 2013 2015 2017 2016 2016 2010 2010 2014 2011 2012 2014
## [15744] 2014 2009 2015 2015 2016 2017 2015 2008 2017 2017 2007 2014 2017
## [15757] 2007 2012 2009 2010 2009 2014 2010 2007 2011 2016 2015 2017 2013
## [15770] 2017 2009 2015 2017 2017 2011 2013 2016 2016 2011 2015 2017 2013
## [15783] 2011 2015 2010 2016 2017 2011 2010 2015 2016 2010 2010 2015 2017
## [15796] 2012 2012 2009 2014 2011 2011 2014 2016 2016 2016 2016 2016 2010
## [15809] 2016 2008 2013 2016 2016 2017 2012 2015 2016 2009 2015 2017 2017
## [15822] 2017 2017 2016 2017 2015 2015 2016 2017 2015 2010 2015 2015 2016
## [15835] 2017 2008 2009 2016 2016 2015 2017 2015 2016 2008 2008 2017 2013
## [15848] 2016 2015 2017 2010 2011 2013 2015 2012 2013 2015 2016 2017 2009
## [15861] 2016 2017 2017 2015 2016 2011 2015 2017 2016 2016 2011 2009 2007
## [15874] 2008 2016 2009 2013 2016 2007 2014 2016 2010 2011 2011 2007 2016
## [15887] 2016 2017 2009 2007 2010 2016 2007 2009 2015 2011 2015 2009 2015
## [15900] 2012 2013 2015 2017 2017 2007 2009 2015 2017 2015 2015 2015 2015
## [15913] 2010 2012 2017 2009 2009 2016 2010 2016 2016 2017 2015 2015 2017
## [15926] 2017 2016 2016 2010 2017 2012 2017 2017 2012 2009 2016 2010 2009
## [15939] 2016 2015 2016 2016 2008 2017 2009 2012 2012 2007 2009 2016 2017
## [15952] 2011 2007 2015 2014 2015 2009 2017 2015 2016 2017 2008 2015 2011
## [15965] 2010 2015 2015 2015 2008 2016 2015 2015 2015 2015 2015 2016 2012
## [15978] 2016 2017 2007 2015 2007 2007 2016 2013 2017 2015 2014 2015 2017
## [15991] 2009 2011 2015 2016 2013 2017 2017 2008 2010 2015 2007 2014 2017
## [16004] 2012 2016 2014 2015 2017 2007 2007 2015 2017 2007 2010 2010 2015
## [16017] 2009 2012 2017 2016 2017 2009 2014 2014 2009 2008 2008 2015 2010
## [16030] 2015 2017 2008 2015 2009 2017 2014 2017 2017 2013 2015 2016 2017
## [16043] 2010 2016 2014 2017 2015 2016 2016 2014 2014 2016 2017 2008 2014
## [16056] 2014 2015 2009 2015 2017 2009 2014 2017 2017 2017 2010 2010 2007
## [16069] 2008 2015 2016 2016 2016 2016 2015 2015 2016 2017 2009 2013 2015
## [16082] 2017 2009 2010 2010 2009 2010 2015 2016 2009 2011 2010 2016 2012
## [16095] 2008 2015 2017 2007 2007 2013 2017 2016 2007 2007 2015 2010 2013
## [16108] 2015 2017 2015 2017 2017 2015 2016 2017 2016 2008 2010 2016 2016
## [16121] 2008 2013 2014 2016 2017 2014 2016 2007 2009 2013 2015 2016 2014
## [16134] 2016 2017 2016 2012 2009 2007 2012 2012 2010 2014 2015 2016 2015
## [16147] 2015 2015 2015 2009 2009 2008 2010 2013 2012 2015 2015 2016 2017
## [16160] 2017 2012 2008 2009 2010 2015 2017 2017 2017 2015 2015 2016 2007
## [16173] 2012 2009 2016 2017 2007 2016 2016 2010 2013 2015 2015 2017 2016
## [16186] 2017 2017 2016 2017 2008 2015 2016 2011 2013 2013 2015 2015 2013
## [16199] 2011 2015 2007 2008 2017 2012 2011 2016 2017 2017 2010 2013 2012
## [16212] 2007 2015 2017 2017 2009 2007 2017 2016 2017 2017 2016 2016 2016
## [16225] 2008 2017 2010 2015 2007 2011 2015 2017 2017 2008 2008 2017 2016
## [16238] 2012 2017 2017 2017 2016 2017 2010 2009 2016 2016 2010 2009 2017
## [16251] 2008 2016 2007 2016 2015 2010 2015 2016 2017 2017 2009 2016 2016
## [16264] 2015 2016 2011 2012 2010 2016 2016 2008 2016 2012 2015 2015 2016
## [16277] 2012 2017 2008 2016 2017 2011 2013 2017 2015 2011 2016 2017 2017
## [16290] 2007 2009 2010 2012 2012 2011 2015 2008 2015 2015 2015 2016 2007
## [16303] 2015 2008 2012 2016 2017 2009 2015 2015 2010 2017 2009 2015 2015
## [16316] 2016 2017 2008 2009 2007 2008 2008 2016 2017 2010 2016 2016 2008
## [16329] 2016 2016 2017 2008 2008 2013 2016 2015 2016 2015 2009 2009 2016
## [16342] 2011 2017 2017 2016 2016 2009 2016 2017 2011 2015 2016 2017 2010
## [16355] 2010 2009 2014 2015 2016 2009 2017 2012 2012 2015 2016 2017 2010
## [16368] 2015 2017 2009 2012 2017 2009 2015 2015 2015 2015 2016 2008 2015
## [16381] 2012 2013 2012 2009 2015 2015 2017 2015 2017 2017 2017 2007 2013
## [16394] 2016 2017 2016 2011 2015 2010 2015 2016 2017 2015 2015 2017 2015
## [16407] 2011 2015 2010 2014 2016 2017 2007 2017 2017 2017 2017 2010 2015
## [16420] 2017 2009 2009 2007 2015 2008 2011 2016 2016 2017 2017 2014 2017
## [16433] 2016 2016 2017 2017 2012 2009 2008 2017 2008 2014 2014 2016 2016
## [16446] 2016 2016 2009 2013 2013 2015 2013 2016 2017 2012 2008 2017 2017
## [16459] 2007 2015 2015 2017 2017 2009 2010 2014 2007 2016 2012 2015 2017
## [16472] 2011 2015 2016 2016 2008 2015 2015 2017 2015 2017 2008 2008 2009
## [16485] 2010 2017 2017 2016 2009 2008 2015 2010 2013 2017 2013 2014 2016
## [16498] 2011 2017 2008 2016 2017 2016 2007 2015 2017 2016 2011 2008 2016
## [16511] 2017 2009 2013 2015 2015 2015 2016 2015 2016 2012 2009 2016 2017
## [16524] 2017 2008 2012 2016 2017 2007 2008 2010 2013 2017 2017 2011 2009
## [16537] 2009 2016 2008 2007 2007 2007 2016 2008 2011 2014 2016 2007 2012
## [16550] 2017 2017 2015 2015 2015 2017 2008 2012 2010 2015 2016 2017 2009
## [16563] 2008 2015 2016 2016 2017 2011 2016 2007 2009 2011 2014 2012 2015
## [16576] 2015 2017 2017 2015 2012 2013 2016 2016 2015 2014 2015 2016 2016
## [16589] 2009 2010 2017 2017 2016 2017 2008 2017 2017 2011 2014 2017 2016
## [16602] 2017 2017 2012 2017 2015 2016 2016 2009 2007 2017 2008 2013 2012
## [16615] 2017 2017 2008 2015 2015 2016 2016 2017 2012 2007 2014 2009 2012
## [16628] 2015 2016 2010 2013 2015 2016 2017 2008 2012 2008 2011 2014 2015
## [16641] 2016 2015 2015 2015 2007 2012 2009 2017 2012 2007 2017 2013 2015
## [16654] 2010 2016 2016 2009 2015 2016 2016 2017 2008 2008 2015 2015 2007
## [16667] 2012 2016 2016 2014 2017 2011 2008 2016 2015 2009 2012 2008 2011
## [16680] 2016 2014 2017 2015 2015 2017 2016 2016 2017 2014 2007 2013 2017
## [16693] 2011 2012 2015 2011 2014 2017 2017 2007 2017 2015 2017 2011 2014
## [16706] 2016 2017 2015 2017 2017 2016 2017 2017 2010 2012 2016 2017 2010
## [16719] 2015 2016 2016 2017 2008 2009 2013 2016 2016 2017 2015 2010 2007
## [16732] 2015 2017 2015 2015 2015 2015 2011 2015 2007 2010 2016 2015 2014
## [16745] 2016 2016 2016 2017 2013 2012 2016 2017 2014 2014 2014 2017 2008
## [16758] 2014 2015 2015 2008 2009 2011 2017 2017 2017 2009 2016 2007 2012
## [16771] 2007 2017 2009 2011 2007 2007 2016 2014 2017 2015 2016 2010 2009
## [16784] 2009 2010 2017 2011 2015 2016 2016 2015 2015 2016 2016 2017 2015
## [16797] 2015 2012 2009 2010 2007 2014 2015 2016 2017 2008 2015 2017 2016
## [16810] 2016 2016 2008 2015 2016 2007 2012 2015 2013 2016 2012 2008 2014
## [16823] 2015 2015 2016 2017 2017 2011 2015 2017 2015 2016 2007 2008 2008
## [16836] 2015 2015 2010 2016 2016 2010 2012 2015 2011 2014 2015 2017 2015
## [16849] 2014 2010 2016 2016 2017 2007 2011 2008 2013 2016 2017 2015 2015
## [16862] 2015 2015 2014 2017 2017 2016 2017 2011 2013 2015 2015 2016 2016
## [16875] 2015 2009 2010 2015 2015 2017 2015 2016 2017 2016 2016 2007 2010
## [16888] 2016 2008 2009 2008 2013 2015 2015 2012 2010 2016 2015 2011 2016
## [16901] 2017 2011 2007 2016 2016 2016 2017 2011 2009 2008 2015 2015 2017
## [16914] 2010 2015 2017 2009 2011 2007 2009 2009 2007 2017 2011 2007 2009
## [16927] 2008 2016 2016 2016 2008 2014 2017 2017 2017 2017 2011 2016 2013
## [16940] 2014 2014 2014 2016 2017 2012 2007 2016 2017 2016 2013 2016 2016
## [16953] 2017 2013 2016 2013 2015 2016 2015 2017 2015 2015 2017 2007 2008
## [16966] 2010 2015 2011 2014 2014 2007 2014 2016 2007 2010 2007 2016 2007
## [16979] 2016 2010 2017 2015 2011 2009 2015 2016 2007 2013 2015 2016 2016
## [16992] 2010 2007 2009 2007 2016 2010 2010 2010 2007 2015 2010 2009 2010
## [17005] 2016 2016 2007 2008 2007 2015 2010 2012 2008 2013 2016 2017 2015
## [17018] 2015 2012 2014 2016 2009 2008 2017 2017 2016 2016 2017 2009 2016
## [17031] 2016 2012 2007 2011 2015 2009 2009 2016 2015 2016 2014 2016 2016
## [17044] 2017 2017 2009 2010 2013 2015 2016 2016 2017 2008 2008 2013 2015
## [17057] 2014 2013 2015 2016 2011 2015 2014 2011 2012 2009 2011 2011 2014
## [17070] 2014 2017 2015 2016 2017 2017 2011 2017 2017 2017 2007 2017 2017
## [17083] 2009 2012 2016 2016 2017 2016 2016 2017 2009 2011 2011 2012 2016
## [17096] 2013 2016 2017 2015 2016 2017 2015 2007 2015 2017 2015 2015 2016
## [17109] 2008 2007 2009 2015 2015 2016 2017 2007 2016 2017 2017 2008 2009
## [17122] 2016 2017 2017 2017 2008 2012 2016 2016 2016 2008 2010 2013 2016
## [17135] 2012 2015 2017 2016 2017 2008 2017 2008 2009 2007 2013 2011 2013
## [17148] 2014 2015 2016 2015 2008 2016 2016 2016 2016 2017 2015 2017 2008
## [17161] 2013 2015 2015 2016 2017 2016 2017 2017 2017 2014 2011 2017 2017
## [17174] 2011 2009 2012 2017 2011 2017 2008 2015 2017 2009 2011 2017 2009
## [17187] 2008 2014 2016 2016 2011 2017 2017 2011 2012 2007 2017 2017 2009
## [17200] 2010 2015 2017 2017 2017 2008 2015 2015 2016 2009 2009 2011 2010
## [17213] 2014 2016 2017 2010 2012 2017 2016 2017 2017 2009 2015 2015 2016
## [17226] 2017 2010 2007 2014 2008 2015 2016 2017 2008 2008 2008 2012 2012
## [17239] 2017 2017 2008 2007 2012 2015 2016 2017 2007 2008 2014 2015 2017
## [17252] 2016 2012 2008 2007 2013 2015 2015 2015 2016 2017 2017 2008 2017
## [17265] 2017 2015 2010 2015 2014 2010 2016 2016 2015 2012 2013 2014 2010
## [17278] 2016 2007 2014 2015 2017 2017 2017 2015 2009 2017 2017 2015 2011
## [17291] 2015 2016 2017 2017 2017 2015 2015 2016 2007 2015 2015 2016 2017
## [17304] 2013 2016 2012 2016 2015 2009 2014 2015 2011 2015 2012 2007 2015
## [17317] 2010 2010 2011 2013 2015 2012 2010 2009 2017 2017 2017 2017 2012
## [17330] 2011 2008 2016 2016 2010 2014 2016 2015 2015 2010 2008 2016 2017
## [17343] 2015 2012 2009 2013 2017 2009 2011 2015 2011 2017 2015 2016 2012
## [17356] 2016 2008 2017 2011 2017 2017 2011 2015 2015 2007 2017 2007 2013
## [17369] 2016 2016 2016 2010 2014 2015 2015 2015 2014 2016 2016 2017 2008
## [17382] 2017 2017 2017 2017 2015 2016 2007 2016 2011 2013 2017 2007 2011
## [17395] 2015 2016 2016 2015 2017 2016 2017 2011 2008 2016 2017 2017 2012
## [17408] 2017 2017 2016 2017 2017 2013 2017 2017 2017 2013 2017 2013 2016
## [17421] 2017 2016 2017 2015 2015 2014 2017 2016 2017 2017 2008 2015 2015
## [17434] 2007 2009 2014 2010 2010 2009 2013 2013 2015 2017 2017 2010 2017
## [17447] 2017 2011 2012 2010 2015 2017 2012 2017 2017 2014 2016 2016 2009
## [17460] 2012 2010 2014 2016 2012 2016 2011 2011 2017 2009 2014 2015 2016
## [17473] 2015 2015 2017 2017 2011 2015 2015 2017 2011 2014 2016 2017 2016
## [17486] 2010 2017 2007 2011 2010 2015 2017 2017 2015 2015 2017 2011 2015
## [17499] 2016 2015 2017 2009 2015 2010 2017 2008 2017 2015 2012 2011 2013
## [17512] 2016 2016 2017 2008 2015 2015 2015 2017 2017 2011 2007 2010 2010
## [17525] 2011 2014 2016 2017 2017 2016 2017 2017 2015 2008 2017 2017 2011
## [17538] 2017 2008 2016 2016 2017 2017 2015 2017 2017 2013 2015 2016 2016
## [17551] 2017 2017 2008 2017 2011 2008 2012 2015 2017 2010 2009 2016 2011
## [17564] 2012 2007 2008 2015 2015 2017 2007 2015 2015 2017 2017 2012 2010
## [17577] 2010 2013 2013 2015 2016 2011 2014 2016 2016 2008 2009 2011 2015
## [17590] 2007 2015 2016 2017 2017 2012 2016 2017 2015 2016 2016 2015 2016
## [17603] 2017 2017 2007 2010 2010 2013 2007 2007 2008 2014 2012 2014 2015
## [17616] 2015 2012 2016 2014 2015 2013 2016 2010 2017 2009 2012 2016 2011
## [17629] 2015 2009 2011 2008 2008 2015 2017 2007 2010 2015 2015 2015 2013
## [17642] 2014 2016 2016 2015 2016 2016 2015 2009 2015 2017 2017 2017 2007
## [17655] 2013 2015 2007 2013 2016 2016 2011 2008 2015 2016 2011 2014 2017
## [17668] 2010 2017 2015 2016 2017 2011 2011 2010 2007 2007 2008 2016 2016
## [17681] 2017 2015 2015 2015 2015 2016 2011 2017 2008 2007 2015 2016 2016
## [17694] 2014 2017 2008 2011 2013 2010 2010 2016 2017 2013 2016 2016 2012
## [17707] 2008 2014 2013 2009 2014 2010 2012 2007 2011 2017 2009 2016 2016
## [17720] 2011 2011 2016 2015 2010 2010 2012 2014 2017 2009 2017 2008 2011
## [17733] 2014 2016 2015 2015 2008 2016 2010 2009 2015 2017 2013 2016 2007
## [17746] 2016 2017 2011 2009 2008 2015 2016 2017 2016 2011 2007 2017 2009
## [17759] 2011 2017 2017 2009 2009 2010 2015 2012 2015 2017 2017 2011 2008
## [17772] 2016 2016 2016 2010 2009 2007 2011 2016 2016 2011 2007 2011 2016
## [17785] 2011 2009 2013 2015 2017 2017 2008 2007 2011 2015 2015 2016 2015
## [17798] 2017 2017 2010 2011 2008 2010 2016 2016 2015 2011 2017 2017 2009
## [17811] 2011 2013 2015 2008 2016 2015 2008 2017 2015 2016 2016 2017 2017
## [17824] 2017 2009 2008 2017 2015 2017 2016 2016 2017 2015 2017 2007 2015
## [17837] 2009 2007 2015 2016 2016 2008 2017 2007 2008 2009 2014 2008 2009
## [17850] 2007 2016 2015 2017 2010 2015 2015 2015 2017 2015 2016 2008 2015
## [17863] 2016 2016 2017 2015 2007 2015 2010 2009 2012 2015 2016 2016 2017
## [17876] 2017 2013 2017 2009 2007 2015 2013 2014 2008 2010 2016 2015 2014
## [17889] 2015 2017 2017 2012 2009 2014 2017 2010 2008 2016 2016 2008 2007
## [17902] 2016 2017 2007 2013 2015 2015 2017 2017 2017 2008 2015 2007 2014
## [17915] 2015 2015 2015 2015 2015 2017 2015 2015 2017 2012 2008 2017 2010
## [17928] 2009 2007 2015 2017 2017 2017 2015 2013 2015 2016 2008 2015 2014
## [17941] 2016 2017 2011 2014 2015 2016 2015 2010 2010 2015 2016 2016 2017
## [17954] 2015 2011 2009 2015 2017 2015 2014 2016 2016 2007 2014 2016 2016
## [17967] 2011 2016 2017 2010 2007 2009 2016 2015 2014 2016 2016 2009 2016
## [17980] 2016 2017 2007 2017 2012 2015 2016 2016 2012 2013 2009 2011 2008
## [17993] 2008 2015 2016 2010 2007 2016 2017 2010 2010 2016 2015 2013 2016
## [18006] 2016 2017 2014 2016 2009 2015 2017 2017 2011 2010 2015 2016 2017
## [18019] 2015 2009 2017 2015 2015 2017 2007 2008 2007 2008 2013 2016 2017
## [18032] 2011 2014 2008 2013 2015 2017 2007 2008 2008 2013 2013 2014 2017
## [18045] 2007 2007 2015 2007 2011 2007 2015 2016 2008 2007 2015 2015 2016
## [18058] 2013 2016 2009 2007 2007 2014 2011 2015 2015 2014 2015 2017 2016
## [18071] 2016 2016 2017 2015 2012 2015 2017 2007 2007 2015 2016 2010 2012
## [18084] 2016 2016 2015 2016 2017 2015 2016 2017 2016 2017 2017 2009 2015
## [18097] 2017 2013 2016 2015 2015 2007 2007 2014 2016 2008 2015 2010 2007
## [18110] 2010 2015 2016 2015 2016 2015 2016 2017 2009 2014 2016 2017 2012
## [18123] 2016 2017 2008 2016 2009 2016 2016 2013 2015 2017 2017 2011 2010
## [18136] 2012 2017 2009 2014 2016 2013 2013 2016 2017 2009 2007 2007 2013
## [18149] 2016 2017 2015 2012 2011 2008 2017 2008 2013 2007 2015 2016 2014
## [18162] 2017 2010 2015 2017 2007 2008 2014 2015 2017 2017 2015 2017 2017
## [18175] 2016 2007 2008 2007 2017 2008 2013 2014 2007 2008 2015 2015 2015
## [18188] 2015 2008 2015 2016 2017 2015 2008 2008 2015 2015 2015 2007 2016
## [18201] 2016 2007 2007 2009 2007 2010 2014 2015 2015 2009 2016 2015 2013
## [18214] 2013 2015 2015 2016 2016 2016 2009 2017 2014 2017 2015 2015 2017
## [18227] 2017 2007 2008 2014 2015 2012 2012 2013 2013 2015 2016 2015 2016
## [18240] 2017 2011 2011 2008 2016 2016 2017 2017 2009 2013 2016 2016 2016
## [18253] 2015 2007 2014 2015 2017 2017 2017 2017 2010 2015 2015 2016 2008
## [18266] 2011 2015 2015 2012 2014 2017 2008 2015 2010 2011 2016 2016 2017
## [18279] 2017 2012 2008 2014 2014 2016 2008 2016 2007 2008 2016 2011 2015
## [18292] 2007 2011 2011 2013 2009 2015 2013 2015 2015 2008 2015 2016 2017
## [18305] 2017 2015 2017 2011 2009 2015 2015 2009 2014 2017 2017 2012 2008
## [18318] 2015 2017 2014 2014 2010 2010 2009 2007 2008 2013 2007 2017 2016
## [18331] 2008 2012 2007 2014 2017 2015 2016 2015 2011 2007 2016 2016 2017
## [18344] 2009 2013 2016 2007 2011 2013 2009 2016 2015 2012 2015 2017 2011
## [18357] 2014 2017 2015 2017 2017 2009 2010 2011 2015 2016 2011 2012 2017
## [18370] 2007 2016 2012 2012 2011 2013 2017 2016 2017 2015 2007 2013 2010
## [18383] 2016 2017 2017 2008 2016 2007 2017 2013 2014 2011 2012 2014 2017
## [18396] 2009 2016 2017 2011 2017 2013 2017 2017 2008 2015 2017 2008 2014
## [18409] 2015 2016 2017 2017 2017 2010 2015 2015 2016 2016 2016 2017 2016
## [18422] 2017 2007 2007 2008 2008 2013 2013 2015 2015 2017 2017 2014 2012
## [18435] 2012 2011 2015 2016 2008 2014 2016 2017 2008 2009 2014 2016 2013
## [18448] 2012 2014 2010 2011 2017 2010 2008 2014 2016 2017 2016 2017 2017
## [18461] 2011 2016 2009 2013 2011 2012 2007 2016 2016 2017 2016 2008 2010
## [18474] 2012 2016 2015 2010 2008 2012 2015 2008 2008 2009 2017 2017 2015
## [18487] 2016 2015 2008 2016 2007 2010 2015 2017 2009 2008 2016 2017 2008
## [18500] 2015 2017 2010 2009 2016 2016 2017 2017 2011 2011 2013 2015 2015
## [18513] 2017 2010 2007 2014 2017 2017 2010 2009 2013 2015 2016 2017 2015
## [18526] 2010 2007 2016 2012 2016 2017 2014 2015 2011 2016 2017 2010 2009
## [18539] 2015 2017 2010 2017 2014 2015 2007 2008 2008 2017 2016 2016 2015
## [18552] 2014 2015 2009 2017 2009 2008 2016 2017 2008 2016 2016 2017 2015
## [18565] 2017 2015 2017 2010 2016 2017 2010 2016 2016 2015 2017 2007 2016
## [18578] 2007 2009 2017 2012 2007 2009 2013 2014 2016 2015 2007 2007 2017
## [18591] 2008 2017 2017 2017 2011 2007 2016 2017 2016 2012 2014 2017 2017
## [18604] 2007 2015 2015 2017 2016 2016 2016 2016 2015 2017 2017 2010 2016
## [18617] 2017 2008 2010 2012 2016 2008 2009 2007 2007 2015 2015 2007 2014
## [18630] 2016 2017 2015 2015 2016 2017 2015 2016 2016 2012 2015 2009 2016
## [18643] 2015 2015 2010 2012 2015 2015 2015 2017 2010 2016 2015 2008 2013
## [18656] 2016 2008 2015 2015 2016 2008 2007 2014 2016 2015 2017 2017 2008
## [18669] 2017 2015 2015 2015 2016 2010 2011 2007 2015 2016 2016 2017 2017
## [18682] 2011 2012 2008 2008 2016 2012 2016 2007 2009 2008 2016 2017 2016
## [18695] 2016 2017 2017 2015 2016 2016 2012 2015 2016 2016 2017 2007 2007
## [18708] 2013 2012 2011 2010 2012 2008 2012 2008 2016 2017 2007 2015 2011
## [18721] 2016 2016 2016 2017 2017 2007 2017 2008 2007 2009 2008 2013 2015
## [18734] 2017 2007 2008 2013 2015 2017 2008 2012 2016 2009 2008 2012 2015
## [18747] 2017 2012 2017 2009 2010 2011 2016 2016 2009 2009 2007 2008 2007
## [18760] 2016 2016 2017 2010 2007 2015 2017 2017 2015 2016 2008 2016 2017
## [18773] 2011 2010 2013 2017 2012 2016 2017 2016 2015 2015 2015 2015 2008
## [18786] 2009 2015 2009 2015 2008 2014 2016 2015 2016 2016 2015 2015 2009
## [18799] 2012 2013 2015 2016 2016 2017 2017 2011 2017 2017 2015 2008 2007
## [18812] 2016 2016 2017 2007 2012 2015 2015 2017 2010 2008 2010 2017 2013
## [18825] 2016 2012 2014 2015 2015 2010 2008 2015 2012 2009 2011 2016 2012
## [18838] 2012 2008 2011 2016 2017 2017 2017 2011 2012 2011 2015 2014 2015
## [18851] 2016 2017 2010 2013 2015 2017 2016 2017 2016 2016 2008 2015 2017
## [18864] 2017 2011 2016 2013 2016 2010 2011 2013 2016 2016 2017 2010 2010
## [18877] 2007 2016 2011 2015 2014 2016 2010 2008 2015 2017 2017 2017 2016
## [18890] 2016 2010 2014 2015 2009 2007 2009 2015 2016 2010 2014 2017 2017
## [18903] 2009 2011 2016 2017 2010 2008 2012 2015 2017 2015 2015 2011 2014
## [18916] 2015 2017 2015 2016 2017 2017 2017 2012 2011 2017 2015 2017 2015
## [18929] 2011 2017 2015 2016 2016 2008 2017 2011 2017 2017 2011 2015 2016
## [18942] 2016 2016 2015 2015 2010 2017 2009 2013 2016 2008 2010 2015 2010
## [18955] 2008 2017 2009 2011 2011 2007 2016 2016 2015 2015 2017 2015 2017
## [18968] 2016 2008 2008 2010 2013 2016 2016 2017 2017 2016 2009 2017 2014
## [18981] 2014 2016 2015 2016 2015 2007 2007 2013 2015 2015 2015 2010 2013
## [18994] 2014 2017 2016 2016 2009 2015 2009 2016 2009 2007 2015 2015 2016
## [19007] 2017 2017 2017 2017 2015 2017 2008 2015 2015 2017 2017 2009 2011
## [19020] 2015 2016 2016 2017 2007 2015 2016 2016 2016 2008 2015 2015 2010
## [19033] 2015 2015 2015 2008 2012 2008 2013 2017 2007 2012 2015 2017 2017
## [19046] 2007 2015 2016 2016 2015 2017 2017 2013 2015 2016 2016 2016 2011
## [19059] 2016 2017 2007 2016 2010 2016 2016 2012 2008 2007 2015 2016 2016
## [19072] 2016 2017 2007 2008 2013 2017 2016 2017 2010 2016 2015 2016 2015
## [19085] 2011 2010 2016 2007 2008 2016 2016 2016 2009 2017 2017 2017 2014
## [19098] 2016 2016 2017 2014 2015 2012 2013 2011 2013 2016 2008 2014 2016
## [19111] 2017 2011 2011 2007 2015 2017 2010 2016 2017 2008 2008 2016 2011
## [19124] 2014 2009 2016 2016 2008 2015 2015 2017 2010 2010 2007 2016 2008
## [19137] 2017 2010 2016 2016 2008 2017 2017 2016 2008 2010 2007 2013 2014
## [19150] 2017 2008 2015 2016 2016 2016 2011 2014 2015 2015 2017 2008 2015
## [19163] 2015 2015 2010 2012 2009 2010 2016 2017 2007 2017 2017 2009 2017
## [19176] 2017 2015 2015 2015 2007 2016 2017 2012 2017 2011 2016 2017 2017
## [19189] 2012 2017 2017 2015 2015 2015 2007 2015 2015 2015 2017 2014 2014
## [19202] 2013 2013 2016 2017 2016 2017 2011 2008 2015 2016 2014 2016 2015
## [19215] 2014 2011 2016 2016 2016 2012 2011 2008 2010 2015 2015 2017 2008
## [19228] 2012 2007 2007 2017 2010 2010 2017 2015 2017 2017 2008 2015 2016
## [19241] 2016 2011 2015 2016 2017 2012 2013 2008 2013 2013 2015 2016 2009
## [19254] 2007 2015 2015 2017 2017 2009 2014 2010 2011 2008 2017 2015 2008
## [19267] 2015 2015 2017 2015 2010 2016 2017 2007 2013 2016 2015 2015 2017
## [19280] 2011 2014 2014 2016 2009 2010 2017 2013 2012 2015 2016 2016 2017
## [19293] 2017 2008 2013 2017 2012 2011 2012 2016 2016 2017 2015 2010 2015
## [19306] 2016 2015 2015 2017 2017 2011 2008 2014 2014 2016 2015 2015 2015
## [19319] 2017 2017 2015 2017 2017 2014 2015 2015 2015 2010 2011 2015 2016
## [19332] 2014 2015 2015 2016 2011 2015 2016 2016 2017 2010 2007 2016 2017
## [19345] 2007 2014 2015 2016 2008 2007 2013 2016 2009 2014 2015 2012 2016
## [19358] 2017 2016 2016 2015 2007 2009 2015 2013 2017 2017 2011 2011 2016
## [19371] 2015 2017 2011 2015 2010 2016 2017 2008 2017 2016 2016 2016 2017
## [19384] 2014 2016 2016 2016 2009 2008 2017 2007 2015 2016 2017 2016 2007
## [19397] 2013 2016 2016 2017 2008 2016 2017 2008 2008 2010 2016 2016 2013
## [19410] 2017 2011 2015 2011 2015 2009 2007 2010 2016 2016 2016 2017 2017
## [19423] 2012 2013 2016 2017 2008 2008 2016 2007 2007 2017 2008 2014 2016
## [19436] 2015 2016 2017 2012 2007 2010 2016 2016 2017 2016 2017 2015 2011
## [19449] 2012 2013 2017 2009 2016 2016 2017 2010 2009 2016 2016 2015 2015
## [19462] 2016 2013 2009 2015 2011 2015 2016 2016 2015 2016 2016 2011 2013
## [19475] 2015 2009 2016 2016 2015 2008 2007 2007 2011 2015 2015 2016 2017
## [19488] 2016 2017 2017 2011 2016 2011 2012 2016 2016 2010 2007 2007 2008
## [19501] 2016 2016 2009 2011 2016 2016 2017 2015 2008 2012 2017 2016 2010
## [19514] 2015 2009 2015 2007 2009 2015 2016 2011 2007 2010 2013 2014 2015
## [19527] 2011 2016 2016 2017 2010 2016 2015 2015 2007 2014 2014 2017 2008
## [19540] 2016 2007 2008 2016 2016 2017 2011 2011 2012 2013 2015 2008 2008
## [19553] 2016 2011 2012 2009 2010 2014 2015 2009 2015 2015 2017 2015 2010
## [19566] 2015 2016 2016 2015 2017 2016 2017 2008 2016 2016 2015 2014 2016
## [19579] 2016 2017 2012 2009 2013 2015 2017 2010 2015 2017 2011 2015 2016
## [19592] 2007 2012 2016 2011 2008 2014 2011 2015 2015 2010 2015 2016 2015
## [19605] 2011 2008 2009 2012 2015 2016 2011 2015 2009 2017 2017 2007 2013
## [19618] 2017 2014 2015 2015 2010 2008 2016 2016 2010 2007 2016 2015 2016
## [19631] 2009 2007 2016 2017 2007 2015 2010 2015 2017 2010 2017 2017 2010
## [19644] 2016 2014 2014 2017 2013 2016 2017 2014 2016 2017 2017 2017 2009
## [19657] 2016 2015 2007 2015 2017 2015 2010 2007 2016 2009 2012 2016 2017
## [19670] 2007 2016 2009 2012 2015 2017 2009 2015 2015 2008 2015 2008 2012
## [19683] 2016 2017 2017 2017 2017 2011 2008 2013 2017 2017 2017 2014 2014
## [19696] 2016 2017 2016 2015 2017 2015 2016 2017 2014 2017 2017 2015 2015
## [19709] 2017 2008 2007 2012 2016 2012 2014 2017 2007 2016 2015 2017 2010
## [19722] 2009 2016 2015 2011 2010 2014 2015 2016 2016 2015 2016 2010 2016
## [19735] 2017 2016 2016 2017 2016 2016 2017 2008 2017 2015 2017 2012 2007
## [19748] 2016 2012 2016 2012 2014 2014 2015 2012 2014 2014 2015 2016 2017
## [19761] 2016 2017 2011 2008 2010 2016 2013 2013 2015 2010 2012 2016 2016
## [19774] 2017 2011 2017 2012 2012 2008 2008 2017 2012 2008 2011 2016 2009
## [19787] 2007 2011 2017 2014 2016 2016 2014 2017 2012 2012 2012 2009 2015
## [19800] 2015 2016 2017 2017 2008 2008 2013 2017 2017 2017 2011 2014 2016
## [19813] 2016 2014 2016 2017 2017 2012 2014 2010 2015 2017 2007 2016 2016
## [19826] 2017 2008 2012 2012 2015 2016 2016 2016 2016 2008 2008 2011 2008
## [19839] 2007 2007 2015 2007 2012 2016 2010 2015 2014 2015 2016 2017 2009
## [19852] 2007 2012 2014 2017 2012 2007 2014 2014 2016 2016 2017 2015 2015
## [19865] 2007 2008 2016 2017 2016 2012 2007 2017 2017 2014 2015 2015 2015
## [19878] 2015 2015 2008 2013 2016 2017 2007 2015 2015 2016 2017 2011 2015
## [19891] 2016 2010 2014 2017 2014 2015 2016 2008 2011 2015 2017 2008 2017
## [19904] 2012 2010 2009 2010 2014 2016 2017 2010 2016 2016 2014 2017 2009
## [19917] 2016 2016 2015 2016 2016 2014 2016 2008 2008 2015 2016 2007 2012
## [19930] 2016 2009 2016 2015 2017 2007 2010 2017 2009 2016 2017 2012 2016
## [19943] 2017 2011 2016 2017 2010 2015 2015 2016 2008 2008 2015 2008 2010
## [19956] 2015 2014 2011 2007 2009 2013 2016 2014 2015 2016 2016 2017 2008
## [19969] 2015 2015 2016 2017 2014 2016 2011 2012 2010 2009 2017 2008 2010
## [19982] 2014 2016 2015 2015 2011 2007 2007 2016 2015 2008 2013 2008 2008
## [19995] 2008 2012 2014 2016 2007 2011 2013 2015 2016 2015 2016 2016 2017
## [20008] 2009 2014 2015 2015 2016 2016 2017 2016 2007 2016 2008 2015 2015
## [20021] 2015 2015 2016 2015 2017 2008 2016 2016 2008 2013 2017 2017 2007
## [20034] 2015 2016 2017 2009 2017 2007 2012 2017 2011 2009 2014 2015 2016
## [20047] 2016 2017 2008 2015 2016 2017 2007 2017 2014 2011 2009 2008 2011
## [20060] 2015 2015 2016 2016 2017 2007 2015 2015 2017 2010 2015 2017 2017
## [20073] 2017 2012 2007 2012 2016 2015 2010 2015 2009 2008 2012 2007 2012
## [20086] 2015 2010 2013 2014 2015 2012 2015 2015 2016 2016 2016 2017 2015
## [20099] 2016 2016 2017 2015 2015 2016 2016 2017 2013 2016 2009 2017 2017
## [20112] 2017 2007 2011 2013 2009 2016 2016 2015 2017 2007 2012 2016 2017
## [20125] 2016 2007 2016 2016 2016 2012 2011 2014 2015 2017 2011 2010 2016
## [20138] 2007 2013 2015 2016 2017 2012 2014 2015 2016 2007 2007 2012 2016
## [20151] 2017 2017 2016 2009 2010 2009 2015 2016 2016 2017 2017 2016 2017
## [20164] 2014 2016 2008 2010 2008 2010 2016 2017 2010 2016 2017 2017 2010
## [20177] 2012 2011 2016 2008 2011 2012 2016 2012 2008 2016 2017 2008 2016
## [20190] 2012 2014 2010 2015 2008 2016 2017 2015 2016 2014 2016 2015 2016
## [20203] 2017 2015 2009 2012 2015 2016 2010 2014 2016 2017 2009 2015 2016
## [20216] 2015 2010 2015 2015 2016 2012 2009 2011 2015 2017 2007 2009 2011
## [20229] 2016 2010 2016 2008 2014 2017 2015 2017 2008 2015 2015 2016 2011
## [20242] 2017 2017 2008 2009 2008 2014 2017 2011 2014 2014 2016 2007 2015
## [20255] 2015 2017 2007 2017 2013 2013 2011 2009 2008 2010 2016 2010 2010
## [20268] 2012 2010 2015 2015 2016 2017 2017 2012 2013 2015 2015 2016 2016
## [20281] 2013 2015 2017 2009 2007 2009 2010 2009 2013 2014 2016 2016 2016
## [20294] 2017 2009 2009 2014 2015 2013 2015 2007 2014 2016 2016 2008 2011
## [20307] 2017 2008 2008 2007 2016 2017 2017 2007 2007 2013 2016 2017 2017
## [20320] 2009 2016 2015 2017 2017 2008 2009 2015 2009 2017 2009 2017 2015
## [20333] 2012 2010 2008 2008 2011 2016 2016 2015 2017 2017 2016 2017 2015
## [20346] 2015 2016 2015 2008 2012 2012 2010 2010 2012 2008 2015 2015 2015
## [20359] 2017 2010 2009 2017 2017 2017 2016 2008 2013 2017 2012 2015 2016
## [20372] 2015 2007 2016 2016 2016 2016 2008 2008 2013 2015 2008 2015 2015
## [20385] 2017 2016 2010 2015 2015 2012 2016 2015 2016 2017 2016 2017 2015
## [20398] 2015 2007 2013 2017 2016 2017 2017 2010 2013 2014 2015 2017 2017
## [20411] 2007 2015 2015 2015 2011 2009 2014 2013 2017 2010 2015 2015 2009
## [20424] 2009 2016 2016 2016 2016 2017 2016 2016 2015 2017 2015 2016 2015
## [20437] 2011 2017 2017 2010 2013 2017 2007 2015 2016 2012 2016 2016 2017
## [20450] 2008 2016 2017 2017 2011 2016 2016 2015 2017 2007 2009 2016 2010
## [20463] 2010 2015 2016 2016 2017 2017 2016 2017 2014 2016 2009 2008 2015
## [20476] 2016 2017 2012 2011 2015 2017 2015 2009 2010 2017 2009 2011 2015
## [20489] 2015 2016 2017 2012 2008 2012 2015 2010 2016 2017 2017 2008 2008
## [20502] 2016 2011 2013 2015 2016 2016 2010 2016 2010 2017 2017 2010 2011
## [20515] 2017 2017 2010 2016 2015 2012 2014 2015 2017 2010 2013 2015 2016
## [20528] 2017 2010 2017 2010 2014 2015 2015 2010 2007 2010 2015 2009 2008
## [20541] 2016 2016 2007 2007 2011 2013 2007 2013 2017 2014 2014 2017 2010
## [20554] 2015 2010 2015 2017 2017 2017 2009 2009 2007 2014 2015 2009 2013
## [20567] 2015 2015 2010 2008 2017 2012 2015 2016 2011 2013 2016 2016 2017
## [20580] 2009 2011 2015 2016 2014 2012 2011 2012 2015 2016 2008 2012 2007
## [20593] 2012 2016 2017 2008 2012 2017 2010 2015 2017 2016 2017 2015 2012
## [20606] 2011 2017 2012 2012 2013 2015 2015 2016 2016 2013 2014 2016 2015
## [20619] 2015 2016 2011 2016 2016 2017 2008 2010 2016 2017 2017 2013 2016
## [20632] 2016 2010 2015 2016 2016 2011 2010 2015 2015 2015 2016 2017 2010
## [20645] 2016 2015 2017 2012 2010 2015 2015 2016 2017 2007 2012 2008 2014
## [20658] 2017 2017 2011 2009 2011 2013 2017 2010 2016 2011 2011 2011 2017
## [20671] 2011 2016 2012 2017 2009 2016 2015 2016 2017 2007 2014 2015 2016
## [20684] 2017 2007 2013 2013 2014 2016 2016 2017 2008 2008 2010 2008 2008
## [20697] 2015 2009 2013 2015 2015 2016 2016 2009 2011 2013 2014 2017 2011
## [20710] 2016 2015 2017 2017 2013 2012 2011 2007 2012 2013 2016 2017 2017
## [20723] 2013 2013 2012 2016 2017 2007 2015 2017 2007 2014 2016 2009 2011
## [20736] 2012 2014 2016 2017 2007 2017 2015 2010 2013 2016 2017 2016 2015
## [20749] 2015 2009 2009 2012 2015 2016 2017 2017 2017 2009 2009 2010 2016
## [20762] 2015 2010 2015 2015 2012 2012 2014 2015 2016 2010 2009 2017 2010
## [20775] 2016 2015 2017 2007 2011 2011 2013 2008 2012 2015 2017 2017 2009
## [20788] 2015 2017 2013 2015 2016 2017 2016 2008 2013 2015 2017 2015 2015
## [20801] 2009 2008 2017 2011 2008 2015 2012 2016 2013 2015 2015 2008 2008
## [20814] 2011 2015 2017 2007 2015 2017 2017 2015 2017 2016 2012 2015 2017
## [20827] 2014 2015 2008 2013 2015 2011 2009 2010 2014 2015 2015 2017 2008
## [20840] 2011 2016 2016 2014 2011 2016 2017 2009 2016 2017 2016 2016 2009
## [20853] 2015 2017 2017 2008 2008 2011 2015 2016 2016 2009 2015 2012 2013
## [20866] 2016 2010 2016 2017 2017 2012 2015 2017 2015 2017 2012 2007 2007
## [20879] 2012 2015 2016 2017 2009 2017 2016 2016 2008 2014 2017 2012 2014
## [20892] 2014 2017 2007 2016 2012 2015 2015 2015 2015 2010 2014 2010 2016
## [20905] 2016 2008 2008 2015 2017 2013 2013 2017 2017 2009 2009 2014 2009
## [20918] 2016 2017 2015 2017 2017 2011 2008 2016 2017 2008 2009 2007 2015
## [20931] 2016 2010 2017 2010 2016 2016 2017 2015 2010 2010 2009 2013 2015
## [20944] 2012 2008 2009 2015 2015 2009 2009 2016 2015 2017 2017 2011 2016
## [20957] 2008 2016 2015 2007 2009 2011 2012 2011 2015 2015 2016 2017 2016
## [20970] 2007 2014 2009 2012 2011 2011 2017 2010 2010 2013 2015 2016 2011
## [20983] 2009 2016 2015 2017 2011 2007 2015 2015 2017 2017 2012 2012 2016
## [20996] 2017 2015 2016 2008 2016 2016 2013 2015 2017 2011 2015 2017 2011
## [21009] 2007 2009 2010 2017 2008 2011 2009 2013 2016 2014 2008 2015 2015
## [21022] 2017 2013 2016 2017 2017 2010 2013 2016 2014 2016 2015 2008 2014
## [21035] 2012 2012 2010 2014 2015 2016 2017 2008 2008 2012 2008 2017 2009
## [21048] 2015 2016 2007 2015 2015 2008 2012 2015 2016 2009 2007 2014 2010
## [21061] 2012 2014 2015 2017 2010 2011 2008 2010 2007 2014 2016 2015 2017
## [21074] 2010 2008 2007 2010 2017 2017 2011 2008 2017 2016 2015 2017 2012
## [21087] 2017 2012 2016 2016 2016 2012 2016 2008 2012 2016 2017 2007 2009
## [21100] 2008 2016 2013 2014 2014 2015 2017 2017 2017 2011 2008 2016 2017
## [21113] 2016 2017 2010 2010 2015 2010 2015 2017 2017 2017 2017 2014 2014
## [21126] 2010 2013 2015 2012 2017 2017 2011 2011 2017 2012 2016 2016 2007
## [21139] 2011 2007 2014 2014 2007 2013 2013 2016 2009 2011 2010 2013 2015
## [21152] 2015 2017 2016 2013 2012 2008 2014 2014 2016 2011 2012 2007 2007
## [21165] 2014 2016 2017 2008 2015 2007 2010 2016 2017 2012 2008 2014 2014
## [21178] 2017 2011 2007 2010 2010 2007 2015 2017 2016 2015 2017 2017 2017
## [21191] 2013 2015 2016 2017 2012 2015 2017 2008 2009 2011 2016 2017 2014
## [21204] 2016 2015 2007 2015 2016 2017 2010 2016 2011 2011 2013 2016 2013
## [21217] 2015 2007 2010 2009 2017 2015 2016 2017 2012 2014 2017 2017 2017
## [21230] 2011 2010 2013 2017 2017 2012 2017 2015 2017 2008 2016 2015 2016
## [21243] 2016 2017 2017 2009 2015 2017 2015 2015 2017 2017 2015 2017 2013
## [21256] 2015 2007 2013 2015 2016 2017 2014 2017 2007 2015 2017 2015 2010
## [21269] 2007 2016 2017 2010 2007 2009 2016 2016 2017 2016 2010 2008 2007
## [21282] 2013 2017 2014 2016 2015 2009 2015 2016 2015 2017 2009 2013 2013
## [21295] 2014 2016 2008 2009 2013 2016 2016 2017 2010 2012 2017 2013 2017
## [21308] 2008 2016 2016 2017 2016 2012 2014 2017 2013 2009 2014 2015 2015
## [21321] 2016 2013 2010 2007 2010 2010 2015 2016 2017 2017 2015 2007 2009
## [21334] 2011 2013 2015 2016 2017 2008 2015 2016 2010 2015 2016 2017 2013
## [21347] 2007 2010 2016 2016 2017 2017 2017 2017 2008 2008 2012 2015 2016
## [21360] 2017 2015 2008 2012 2008 2015 2015 2017 2008 2016 2016 2016 2007
## [21373] 2008 2015 2009 2009 2007 2013 2017 2010 2008 2010 2017 2015 2016
## [21386] 2014 2017 2017 2015 2017 2014 2015 2016 2015 2014 2016 2010 2016
## [21399] 2013 2015 2017 2010 2007 2007 2013 2015 2016 2008 2017 2015 2015
## [21412] 2016 2017 2010 2015 2013 2014 2009 2009 2017 2015 2011 2008 2010
## [21425] 2015 2015 2010 2008 2016 2015 2017 2012 2014 2016 2017 2017 2011
## [21438] 2016 2017 2017 2010 2009 2010 2009 2010 2015 2016 2013 2010 2013
## [21451] 2016 2017 2009 2011 2015 2015 2013 2015 2017 2009 2011 2014 2014
## [21464] 2008 2014 2015 2016 2017 2017 2011 2008 2016 2017 2015 2015 2010
## [21477] 2011 2010 2015 2008 2013 2015 2013 2016 2017 2011 2015 2009 2008
## [21490] 2016 2008 2011 2012 2012 2015 2015 2007 2007 2012 2008 2016 2008
## [21503] 2013 2017 2013 2014 2015 2015 2016 2016 2017 2015 2016 2015 2015
## [21516] 2016 2016 2016 2017 2012 2016 2017 2008 2015 2016 2017 2011 2015
## [21529] 2015 2017 2015 2014 2013 2016 2017 2007 2011 2016 2013 2017 2016
## [21542] 2017 2008 2012 2015 2016 2013 2015 2015 2017 2012 2015 2015 2014
## [21555] 2015 2016 2017 2011 2017 2008 2012 2016 2008 2014 2015 2010 2013
## [21568] 2017 2009 2011 2014 2015 2016 2017 2015 2017 2017 2012 2013 2015
## [21581] 2017 2011 2015 2011 2008 2017 2012 2015 2015 2007 2013 2013 2015
## [21594] 2016 2017 2007 2008 2009 2015 2016 2016 2016 2017 2015 2011 2007
## [21607] 2007 2007 2015 2017 2017 2014 2015 2016 2017 2017 2015 2016 2016
## [21620] 2016 2015 2015 2017 2011 2017 2008 2009 2015 2017 2011 2007 2016
## [21633] 2016 2009 2016 2013 2016 2017 2016 2017 2015 2017 2017 2017 2010
## [21646] 2010 2011 2016 2017 2012 2007 2007 2015 2015 2016 2017 2007 2010
## [21659] 2017 2016 2017 2009 2013 2008 2011 2015 2012 2017 2010 2011 2011
## [21672] 2013 2016 2008 2016 2011 2017 2017 2017 2013 2015 2016 2011 2012
## [21685] 2009 2016 2016 2009 2017 2016 2016 2010 2015 2008 2008 2014 2015
## [21698] 2007 2015 2015 2017 2016 2008 2015 2016 2017 2016 2015 2012 2007
## [21711] 2016 2017 2008 2015 2016 2016 2009 2014 2008 2009 2009 2012 2015
## [21724] 2016 2015 2013 2015 2016 2007 2013 2015 2017 2009 2015 2015 2016
## [21737] 2007 2014 2010 2016 2008 2008 2013 2016 2017 2015 2016 2014 2016
## [21750] 2016 2015 2017 2015 2012 2016 2017 2016 2016 2011 2016 2016 2016
## [21763] 2016 2008 2016 2009 2014 2016 2016 2017 2013 2010 2017 2009 2014
## [21776] 2016 2016 2017 2016 2008 2016 2017 2017 2011 2017 2017 2011 2015
## [21789] 2008 2015 2017 2017 2016 2016 2017 2017 2007 2008 2011 2016 2016
## [21802] 2008 2009 2013 2016 2017 2017 2016 2017 2017 2008 2016 2017 2015
## [21815] 2007 2013 2015 2016 2008 2016 2017 2017 2016 2017 2010 2011 2011
## [21828] 2013 2017 2013 2016 2017 2017 2009 2014 2016 2015 2015 2015 2017
## [21841] 2017 2010 2017 2008 2013 2012 2016 2012 2016 2011 2012 2017 2010
## [21854] 2008 2011 2015 2015 2012 2015 2017 2017 2009 2013 2015 2012 2017
## [21867] 2017 2017 2017 2017 2017 2017 2011 2016 2017 2017 2016 2016 2014
## [21880] 2016 2010 2016 2015 2017 2017 2007 2012 2013 2010 2015 2016 2017
## [21893] 2014 2015 2016 2007 2007 2008 2012 2015 2011 2016 2012 2009 2011
## [21906] 2014 2015 2017 2012 2016 2016 2013 2015 2013 2016 2007 2017 2017
## [21919] 2009 2016 2017 2016 2017 2007 2007 2010 2017 2009 2016 2017 2012
## [21932] 2014 2015 2015 2007 2015 2016 2016 2015 2011 2016 2017 2008 2011
## [21945] 2016 2017 2016 2016 2017 2013 2015 2016 2016 2016 2010 2015 2010
## [21958] 2017 2015 2016 2017 2016 2017 2017 2009 2016 2016 2008 2011 2015
## [21971] 2008 2009 2007 2009 2017 2017 2010 2013 2017 2017 2007 2007 2011
## [21984] 2014 2015 2016 2007 2013 2013 2016 2008 2015 2016 2009 2012 2014
## [21997] 2011 2008 2013 2007 2009 2007 2015 2016 2017 2017 2008 2010 2016
## [22010] 2016 2010 2008 2014 2016 2016 2017 2012 2008 2008 2016 2017 2017
## [22023] 2008 2010 2007 2013 2015 2017 2016 2017 2015 2016 2017 2015 2015
## [22036] 2011 2014 2016 2007 2016 2016 2017 2017 2017 2016 2017 2007 2012
## [22049] 2011 2015 2017 2009 2014 2015 2017 2017 2014 2014 2016 2017 2009
## [22062] 2015 2017 2016 2011 2011 2016 2017 2015 2011 2007 2016 2015 2017
## [22075] 2017 2011 2008 2014 2016 2008 2013 2015 2016 2017 2011 2008 2015
## [22088] 2016 2015 2007 2010 2017 2011 2010 2007 2013 2014 2009 2008 2015
## [22101] 2015 2008 2013 2017 2009 2009 2011 2010 2017 2017 2017 2017 2014
## [22114] 2014 2016 2017 2011 2014 2014 2016 2017 2016 2015 2008 2007 2017
## [22127] 2017 2017 2011 2016 2015 2010 2007 2015 2017 2010 2016 2011 2016
## [22140] 2017 2007 2007 2011 2012 2016 2017 2016 2011 2008 2015 2015 2015
## [22153] 2016 2016 2017 2017 2012 2015 2015 2016 2017 2017 2009 2015 2017
## [22166] 2010 2016 2017 2010 2016 2017 2009 2009 2007 2015 2011 2015 2016
## [22179] 2017 2015 2008 2008 2017 2013 2013 2016 2016 2008 2007 2015 2016
## [22192] 2011 2007 2015 2015 2008 2016 2014 2016 2008 2010 2017 2014 2016
## [22205] 2017 2015 2011 2013 2013 2010 2008 2014 2008 2017 2017 2014 2016
## [22218] 2011 2016 2017 2017 2007 2010 2015 2016 2007 2009 2017 2012 2007
## [22231] 2008 2010 2007 2010 2015 2017 2011 2007 2016 2008 2015 2015 2012
## [22244] 2011 2012 2008 2015 2016 2017 2011 2017 2015 2017 2013 2013 2016
## [22257] 2012 2012 2015 2015 2015 2016 2013 2010 2007 2017 2017 2009 2009
## [22270] 2013 2010 2013 2013 2017 2016 2008 2017 2017 2014 2017 2015 2017
## [22283] 2013 2010 2008 2017 2017 2014 2015 2015 2011 2010 2015 2013 2014
## [22296] 2016 2014 2016 2007 2012 2011 2007 2008 2016 2016 2015 2015 2017
## [22309] 2015 2016 2013 2014 2014 2015 2016 2007 2007 2011 2016 2017 2012
## [22322] 2016 2016 2008 2011 2016 2015 2015 2017 2011 2012 2017 2017 2017
## [22335] 2012 2009 2008 2016 2017 2016 2016 2017 2009 2014 2016 2016 2013
## [22348] 2014 2009 2017 2017 2015 2011 2015 2008 2012 2008 2015 2017 2009
## [22361] 2007 2013 2014 2016 2015 2016 2016 2016 2009 2015 2008 2010 2009
## [22374] 2011 2008 2016 2017 2013 2008 2017 2013 2015 2014 2017 2011 2011
## [22387] 2015 2017 2016 2007 2015 2008 2017 2017 2017 2011 2009 2016 2016
## [22400] 2017 2015 2014 2016 2017 2017 2012 2007 2015 2017 2015 2010 2016
## [22413] 2010 2015 2016 2015 2016 2017 2017 2011 2015 2017 2017 2011 2008
## [22426] 2009 2013 2014 2015 2015 2015 2017 2017 2008 2010 2014 2015 2016
## [22439] 2009 2011 2015 2015 2016 2012 2015 2016 2017 2012 2008 2014 2015
## [22452] 2016 2016 2016 2017 2015 2016 2011 2009 2009 2016 2017 2017 2010
## [22465] 2014 2017 2011 2010 2015 2012 2017 2009 2014 2017 2017 2007 2012
## [22478] 2010 2011 2010 2014 2015 2017 2015 2012 2016 2016 2017 2007 2011
## [22491] 2008 2013 2015 2016 2015 2008 2016 2016 2016 2015 2014 2013 2015
## [22504] 2015 2009 2015 2016 2016 2017 2017 2016 2015 2015 2017 2016 2016
## [22517] 2017 2009 2015 2015 2015 2012 2010 2014 2007 2007 2015 2017 2015
## [22530] 2016 2012 2008 2014 2015 2016 2010 2017 2016 2017 2010 2009 2016
## [22543] 2015 2016 2007 2016 2017 2017 2010 2010 2007 2015 2016 2007 2016
## [22556] 2017 2015 2015 2016 2011 2014 2015 2016 2017 2007 2014 2017 2016
## [22569] 2014 2014 2017 2009 2014 2016 2016 2017 2017 2009 2011 2007 2014
## [22582] 2015 2016 2015 2017 2010 2015 2015 2009 2015 2016 2011 2012 2016
## [22595] 2016 2015 2017 2017 2017 2010 2017 2012 2015 2007 2015 2016 2016
## [22608] 2013 2008 2008 2007 2010 2015 2009 2010 2013 2014 2015 2015 2017
## [22621] 2017 2017 2011 2016 2011 2012 2015 2016 2012 2014 2017 2010 2011
## [22634] 2008 2011 2015 2015 2016 2017 2017 2009 2009 2017 2017 2009 2014
## [22647] 2016 2016 2017 2009 2012 2016 2007 2013 2016 2015 2012 2016 2011
## [22660] 2011 2017 2007 2014 2016 2017 2009 2010 2014 2011 2010 2016 2008
## [22673] 2013 2010 2015 2016 2017 2007 2015 2016 2008 2009 2016 2016 2009
## [22686] 2009 2013 2015 2015 2016 2017 2007 2007 2008 2017 2011 2017 2015
## [22699] 2008 2017 2009 2007 2010 2008 2015 2007 2017 2012 2012 2009 2017
## [22712] 2009 2015 2017 2017 2011 2012 2015 2016 2016 2017 2008 2012 2008
## [22725] 2016 2017 2007 2014 2013 2007 2012 2011 2013 2014 2015 2016 2011
## [22738] 2013 2016 2012 2013 2017 2017 2015 2016 2008 2010 2008 2015 2015
## [22751] 2016 2016 2015 2016 2008 2007 2009 2016 2016 2015 2017 2008 2016
## [22764] 2017 2016 2016 2016 2009 2007 2015 2017 2007 2016 2007 2007 2007
## [22777] 2008 2015 2015 2016 2010 2015 2007 2011 2011 2009 2007 2007 2012
## [22790] 2013 2016 2017 2015 2016 2017 2013 2017 2015 2009 2007 2016 2016
## [22803] 2007 2008 2010 2014 2008 2014 2009 2013 2013 2014 2015 2016 2017
## [22816] 2017 2007 2017 2017 2010 2008 2015 2015 2015 2017 2015 2007 2016
## [22829] 2014 2017 2017 2012 2011 2015 2010 2015 2015 2017 2017 2015 2011
## [22842] 2011 2015 2013 2014 2015 2016 2017 2009 2015 2011 2011 2007 2015
## [22855] 2012 2016 2016 2010 2007 2017 2012 2009 2012 2015 2009 2015 2016
## [22868] 2017 2014 2011 2011 2009 2012 2015 2017 2015 2016 2012 2011 2009
## [22881] 2010 2013 2015 2015 2017 2017 2007 2016 2017 2009 2008 2015 2010
## [22894] 2015 2017 2014 2016 2017 2013 2016 2017 2009 2016 2017 2017 2017
## [22907] 2008 2014 2015 2011 2008 2016 2017 2017 2009 2016 2017 2017 2015
## [22920] 2017 2015 2016 2008 2016 2017 2007 2010 2017 2007 2008 2014 2015
## [22933] 2013 2016 2009 2009 2010 2011 2009 2008 2007 2015 2016 2014 2017
## [22946] 2009 2008 2011 2015 2016 2017 2007 2008 2016 2016 2007 2016 2016
## [22959] 2007 2009 2016 2009 2010 2008 2017 2013 2016 2015 2011 2008 2016
## [22972] 2016 2009 2017 2014 2015 2015 2015 2010 2017 2011 2017 2008 2016
## [22985] 2010 2011 2009 2015 2017 2007 2015 2015 2016 2017 2017 2015 2017
## [22998] 2012 2013 2016 2010 2015 2009 2012 2010 2015 2017 2008 2015 2015
## [23011] 2007 2012 2015 2015 2017 2011 2014 2016 2017 2014 2017 2009 2013
## [23024] 2015 2009 2016 2017 2017 2012 2010 2009 2007 2015 2007 2011 2013
## [23037] 2008 2015 2016 2016 2016 2011 2015 2016 2017 2011 2014 2007 2012
## [23050] 2015 2016 2011 2015 2016 2009 2013 2015 2016 2017 2011 2012 2017
## [23063] 2010 2011 2015 2008 2015 2009 2012 2012 2014 2015 2010 2015 2010
## [23076] 2011 2016 2015 2017 2015 2016 2016 2016 2009 2017 2008 2013 2010
## [23089] 2009 2011 2015 2017 2013 2012 2013 2015 2015 2015 2009 2013 2016
## [23102] 2015 2017 2017 2016 2017 2015 2008 2010 2012 2010 2017 2017 2015
## [23115] 2017 2017 2007 2016 2016 2017 2013 2016 2017 2013 2017 2012 2015
## [23128] 2010 2016 2015 2015 2016 2015 2007 2009 2014 2014 2017 2009 2011
## [23141] 2010 2017 2008 2015 2015 2015 2017 2017 2016 2008 2015 2016 2017
## [23154] 2011 2011 2015 2016 2008 2016 2011 2016 2016 2017 2010 2007 2017
## [23167] 2011 2009 2010 2017 2007 2015 2011 2013 2015 2016 2017 2017 2011
## [23180] 2015 2007 2007 2007 2012 2017 2007 2016 2016 2017 2008 2016 2009
## [23193] 2009 2008 2014 2016 2008 2014 2015 2008 2014 2016 2016 2017 2017
## [23206] 2008 2016 2007 2015 2012 2016 2007 2013 2016 2016 2017 2017 2017
## [23219] 2017 2010 2015 2017 2013 2016 2008 2011 2007 2016 2014 2015 2016
## [23232] 2016 2016 2017 2008 2010 2015 2015 2015 2015 2017 2008 2007 2014
## [23245] 2016 2017 2017 2013 2016 2017 2015 2017 2017 2013 2014 2015 2017
## [23258] 2008 2015 2009 2008 2015 2013 2017 2008 2016 2017 2017 2008 2007
## [23271] 2015 2014 2015 2017 2016 2016 2015 2016 2015 2017 2008 2011 2015
## [23284] 2015 2008 2014 2015 2011 2008 2017 2008 2014 2016 2015 2017 2012
## [23297] 2014 2017 2008 2008 2016 2017 2017 2015 2012 2008 2017 2009 2008
## [23310] 2015 2016 2017 2015 2016 2017 2010 2010 2017 2013 2015 2016 2009
## [23323] 2013 2015 2015 2016 2012 2017 2015 2012 2014 2015 2017 2009 2007
## [23336] 2011 2016 2008 2007 2012 2015 2017 2008 2009 2014 2015 2015 2008
## [23349] 2016 2017 2007 2011 2010 2014 2009 2015 2009 2013 2016 2015 2012
## [23362] 2015 2008 2010 2017 2017 2009 2015 2016 2017 2017 2016 2016 2016
## [23375] 2017 2017 2015 2016 2009 2016 2017 2015 2017 2015 2015 2016 2016
## [23388] 2008 2013 2017 2011 2013 2015 2016 2012 2015 2010 2016 2017 2008
## [23401] 2008 2015 2015 2015 2012 2008 2007 2014 2017 2011 2008 2016 2010
## [23414] 2007 2015 2017 2010 2016 2008 2015 2017 2009 2008 2016 2013 2013
## [23427] 2015 2014 2015 2016 2017 2017 2009 2009 2016 2017 2011 2009 2016
## [23440] 2008 2014 2015 2011 2010 2016 2017 2007 2013 2017 2009 2015 2016
## [23453] 2015 2017 2012 2015 2009 2007 2015 2009 2008 2016 2009 2009 2014
## [23466] 2015 2015 2015 2010 2017 2014 2007 2007 2014 2015 2015 2016 2016
## [23479] 2017 2017 2014 2017 2015 2009 2016 2016 2017 2015 2017 2017 2009
## [23492] 2009 2009 2015 2015 2007 2010 2017 2014 2017 2010 2007 2017 2009
## [23505] 2016 2010 2010 2014 2014 2012 2017 2010 2016 2016 2011 2014 2016
## [23518] 2016 2010 2015 2017 2007 2016 2017 2013 2011 2015 2008 2012 2016
## [23531] 2017 2012 2007 2016 2017 2011 2015 2015 2013 2016 2017 2014 2016
## [23544] 2015 2016 2016 2016 2008 2008 2014 2015 2017 2007 2016 2007 2014
## [23557] 2015 2016 2016 2016 2017 2017 2015 2017 2009 2016 2009 2015 2015
## [23570] 2015 2015 2017 2009 2013 2014 2017 2008 2015 2017 2008 2016 2017
## [23583] 2015 2014 2009 2015 2017 2007 2016 2016 2017 2007 2014 2007 2011
## [23596] 2014 2016 2014 2017 2017 2017 2007 2007 2015 2012 2010 2014 2015
## [23609] 2015 2017 2016 2016 2017 2017 2008 2011 2017 2015 2015 2007 2012
## [23622] 2016 2017 2007 2015 2012 2008 2015 2015 2015 2016 2014 2014 2015
## [23635] 2016 2011 2017 2017 2007 2013 2016 2008 2015 2016 2017 2007 2014
## [23648] 2015 2008 2016 2015 2016 2009 2013 2016 2007 2015 2016 2012 2015
## [23661] 2016 2017 2011 2016 2011 2013 2016 2015 2011 2015 2015 2008 2009
## [23674] 2007 2013 2015 2016 2012 2009 2017 2013 2015 2015 2016 2017 2015
## [23687] 2010 2012 2010 2013 2015 2015 2016 2016 2012 2015 2017 2016 2017
## [23700] 2008 2016 2017 2010 2015 2015 2016 2015 2017 2017 2013 2017 2016
## [23713] 2017 2012 2015 2015 2010 2015 2017 2017 2007 2015 2015 2016 2016
## [23726] 2009 2009 2012 2015 2016 2017 2011 2012 2011 2013 2015 2017 2016
## [23739] 2015 2017 2016 2012 2014 2016 2017 2011 2010 2009 2015 2015 2012
## [23752] 2016 2009 2007 2013 2015 2016 2016 2011 2011 2016 2016 2014 2015
## [23765] 2015 2016 2009 2012 2010 2015 2009 2010 2015 2016 2007 2016 2008
## [23778] 2007 2015 2017 2017 2007 2009 2013 2014 2009 2016 2017 2010 2009
## [23791] 2013 2015 2015 2007 2011 2015 2015 2016 2014 2015 2017 2010 2016
## [23804] 2016 2017 2010 2015 2016 2016 2017 2007 2015 2016 2017 2011 2013
## [23817] 2016 2017 2013 2014 2015 2016 2016 2017 2009 2014 2015 2017 2015
## [23830] 2017 2009 2009 2012 2007 2013 2015 2014 2016 2017 2017 2009 2015
## [23843] 2016 2015 2013 2016 2016 2015 2015 2017 2017 2017 2008 2012 2008
## [23856] 2007 2017 2016 2015 2012 2007 2011 2012 2016 2015 2017 2009 2017
## [23869] 2015 2015 2016 2011 2016 2017 2009 2017 2017 2012 2012 2017 2007
## [23882] 2010 2015 2010 2016 2017 2012 2016 2013 2016 2017 2016 2011 2016
## [23895] 2015 2008 2009 2017 2014 2016 2010 2013 2016 2016 2010 2015 2015
## [23908] 2016 2017 2008 2016 2016 2007 2016 2017 2016 2014 2017 2010 2010
## [23921] 2015 2016 2015 2009 2017 2007 2016 2015 2012 2010 2016 2017 2011
## [23934] 2014 2017 2013 2010 2016 2017 2010 2010 2015 2013 2016 2017 2017
## [23947] 2007 2013 2016 2017 2011 2016 2017 2009 2008 2009 2016 2017 2017
## [23960] 2010 2011 2007 2016 2017 2008 2008 2007 2013 2016 2008 2010 2017
## [23973] 2010 2012 2016 2010 2016 2011 2016 2016 2012 2007 2017 2017 2010
## [23986] 2009 2011 2017 2016 2008 2008 2014 2017 2017 2010 2010 2015 2013
## [23999] 2009 2011 2015 2016 2017 2009 2016 2016 2017 2017 2007 2007 2010
## [24012] 2013 2013 2015 2013 2013 2015 2012 2015 2016 2009 2014 2015 2015
## [24025] 2016 2015 2017 2015 2016 2011 2014 2010 2009 2012 2017 2012 2017
## [24038] 2008 2015 2017 2017 2015 2015 2016 2017 2012 2007 2011 2016 2017
## [24051] 2007 2013 2009 2008 2008 2016 2016 2009 2014 2016 2008 2007 2016
## [24064] 2016 2017 2011 2016 2016 2011 2015 2016 2016 2015 2015 2016 2012
## [24077] 2008 2017 2015 2010 2010 2013 2016 2007 2007 2013 2016 2010 2015
## [24090] 2016 2009 2015 2015 2007 2012 2015 2010 2012 2009 2008 2009 2017
## [24103] 2007 2013 2012 2013 2017 2017 2015 2007 2008 2016 2012 2015 2016
## [24116] 2017 2008 2016 2011 2016 2016 2017 2009 2016 2016 2016 2011 2015
## [24129] 2017 2011 2008 2008 2016 2007 2015 2016 2009 2015 2016 2016 2011
## [24142] 2016 2017 2016 2016 2016 2009 2008 2008 2012 2015 2017 2007 2013
## [24155] 2016 2017 2012 2016 2015 2016 2017 2015 2016 2016 2017 2011 2013
## [24168] 2008 2008 2010 2013 2016 2012 2007 2009 2010 2015 2016 2016 2016
## [24181] 2014 2010 2017 2015 2009 2013 2016 2015 2017 2009 2017 2017 2012
## [24194] 2016 2011 2008 2017 2008 2010 2015 2014 2011 2014 2017 2017 2012
## [24207] 2014 2017 2008 2016 2017 2008 2011 2016 2016 2015 2007 2016 2015
## [24220] 2012 2015 2016 2016 2016 2010 2016 2016 2012 2011 2017 2017 2015
## [24233] 2009 2007 2016 2016 2017 2009 2010 2015 2017 2017 2017 2009 2013
## [24246] 2015 2015 2016 2011 2013 2016 2016 2015 2016 2015 2009 2016 2017
## [24259] 2011 2009 2008 2008 2015 2017 2014 2015 2015 2015 2016 2016 2017
## [24272] 2007 2015 2017 2012 2017 2007 2007 2007 2009 2010 2011 2013 2015
## [24285] 2015 2016 2017 2016 2015 2007 2008 2016 2012 2012 2017 2017 2016
## [24298] 2011 2011 2007 2017 2013 2008 2016 2017 2017 2009 2007 2007 2016
## [24311] 2016 2014 2016 2016 2017 2008 2017 2017 2016 2016 2016 2017 2010
## [24324] 2015 2016 2017 2007 2007 2017 2017 2011 2013 2017 2008 2015 2016
## [24337] 2017 2012 2009 2016 2016 2015 2013 2009 2016 2017 2017 2017 2011
## [24350] 2017 2015 2014 2016 2016 2017 2017 2011 2014 2016 2009 2010 2007
## [24363] 2016 2012 2010 2007 2012 2015 2010 2009 2015 2012 2014 2016 2017
## [24376] 2016 2008 2016 2016 2017 2009 2014 2016 2016 2016 2007 2014 2010
## [24389] 2013 2014 2016 2008 2016 2017 2012 2011 2016 2017 2010 2012 2007
## [24402] 2017 2016 2017 2008 2011 2014 2016 2017 2017 2017 2007 2015 2015
## [24415] 2015 2012 2011 2007 2016 2016 2013 2015 2016 2017 2016 2011 2009
## [24428] 2013 2017 2010 2014 2016 2009 2010 2016 2017 2008 2017 2008 2015
## [24441] 2017 2010 2008 2007 2016 2016 2016 2017 2010 2016 2016 2016 2016
## [24454] 2008 2013 2007 2017 2009 2009 2013 2015 2015 2016 2015 2009 2007
## [24467] 2016 2015 2011 2007 2007 2012 2016 2011 2011 2009 2015 2007 2014
## [24480] 2015 2015 2017 2007 2010 2015 2009 2016 2016 2017 2011 2011 2012
## [24493] 2010 2008 2015 2015 2015 2017 2009 2015 2016 2017 2016 2016 2017
## [24506] 2010 2014 2016 2013 2017 2012 2010 2016 2016 2009 2010 2014 2014
## [24519] 2015 2015 2015 2017 2016 2016 2013 2015 2015 2017 2008 2016 2017
## [24532] 2007 2011 2016 2015 2012 2016 2017 2017 2017 2012 2017 2010 2009
## [24545] 2008 2010 2013 2009 2016 2007 2015 2014 2017 2017 2011 2012 2013
## [24558] 2016 2016 2016 2015 2017 2017 2015 2016 2017 2012 2010 2017 2016
## [24571] 2017 2017 2013 2017 2015 2016 2017 2017 2009 2010 2013 2016 2016
## [24584] 2013 2016 2017 2011 2013 2015 2016 2017 2015 2016 2016 2013 2016
## [24597] 2016 2011 2016 2008 2012 2009 2009 2016 2008 2014 2017 2013 2013
## [24610] 2015 2015 2016 2015 2015 2009 2009 2009 2014 2016 2017 2008 2016
## [24623] 2009 2017 2017 2010 2016 2016 2007 2013 2014 2016 2016 2012 2016
## [24636] 2017 2017 2017 2015 2008 2013 2015 2015 2016 2007 2012 2009 2007
## [24649] 2016 2017 2015 2017 2008 2014 2012 2017 2015 2017 2008 2013 2016
## [24662] 2017 2010 2008 2008 2012 2011 2008 2015 2013 2017 2017 2015 2008
## [24675] 2008 2015 2016 2017 2007 2012 2015 2016 2016 2016 2007 2017 2017
## [24688] 2015 2009 2011 2008 2015 2015 2009 2009 2017 2010 2010 2008 2013
## [24701] 2014 2016 2017 2015 2016 2016 2016 2017 2009 2010 2012 2013 2016
## [24714] 2017 2011 2014 2017 2012 2009 2017 2011 2017 2017 2016 2010 2015
## [24727] 2016 2017 2017 2017 2016 2015 2010 2011 2012 2013 2015 2017 2009
## [24740] 2017 2012 2007 2017 2010 2016 2016 2017 2012 2007 2009 2012 2016
## [24753] 2011 2015 2017 2017 2015 2016 2009 2012 2015 2008 2012 2015 2016
## [24766] 2017 2007 2010 2015 2015 2015 2017 2015 2016 2016 2015 2009 2011
## [24779] 2013 2016 2011 2007 2007 2016 2011 2016 2015 2012 2014 2008 2007
## [24792] 2017 2016 2016 2015 2007 2016 2015 2016 2012 2014 2015 2016 2011
## [24805] 2016 2015 2014 2012 2016 2017 2017 2017 2014 2016 2017 2009 2013
## [24818] 2016 2009 2015 2015 2015 2017 2009 2015 2011 2009 2010 2013 2015
## [24831] 2015 2007 2007 2010 2015 2016 2007 2016 2013 2016 2016 2007 2015
## [24844] 2015 2016 2017 2011 2010 2011 2008 2012 2011 2014 2017 2010 2014
## [24857] 2015 2016 2012 2014 2017 2013 2014 2015 2007 2014 2017 2017 2017
## [24870] 2008 2007 2007 2013 2016 2017 2007 2017 2017 2008 2007 2009 2016
## [24883] 2015 2009 2011 2015 2016 2016 2017 2017 2017 2010 2008 2016 2016
## [24896] 2009 2012 2011 2017 2009 2008 2015 2008 2016 2016 2016 2017 2015
## [24909] 2017 2017 2015 2008 2015 2008 2010 2015 2016 2016 2013 2015 2015
## [24922] 2017 2012 2008 2015 2015 2016 2016 2016 2017 2010 2012 2016 2016
## [24935] 2016 2017 2016 2012 2011 2014 2016 2017 2011 2016 2015 2008 2011
## [24948] 2015 2017 2015 2015 2008 2017 2015 2015 2016 2009 2016 2008 2007
## [24961] 2009 2012 2014 2016 2017 2008 2014 2011 2010 2015 2016 2017 2008
## [24974] 2016 2015 2010 2013 2016 2014 2016 2008 2011 2015 2016 2016 2014
## [24987] 2016 2016 2017 2008 2016 2009 2016 2016 2015 2016 2008 2008 2015
## [25000] 2012 2016 2015 2017 2011 2016 2015 2011 2008 2012 2009 2016 2017
## [25013] 2007 2007 2014 2016 2013 2016 2017 2016 2016 2017 2007 2014 2015
## [25026] 2016 2016 2016 2017 2015 2017 2014 2015 2015 2016 2015 2017 2015
## [25039] 2016 2016 2012 2008 2013 2016 2017 2008 2007 2012 2016 2017 2008
## [25052] 2013 2017 2017 2008 2017 2008 2014 2014 2016 2012 2017 2009 2007
## [25065] 2011 2011 2016 2013 2013 2016 2017 2017 2007 2008 2016 2016 2012
## [25078] 2016 2015 2012 2015 2015 2007 2008 2015 2017 2008 2013 2016 2015
## [25091] 2017 2010 2015 2015 2017 2007 2009 2007 2016 2016 2017 2015 2017
## [25104] 2007 2009 2014 2016 2011 2010 2016 2017 2007 2013 2013 2016 2016
## [25117] 2011 2015 2017 2015 2009 2014 2015 2016 2009 2010 2013 2015 2016
## [25130] 2017 2017 2017 2009 2014 2011 2009 2015 2016 2017 2017 2013 2015
## [25143] 2016 2016 2012 2015 2015 2009 2009 2014 2015 2017 2009 2009 2013
## [25156] 2015 2016 2011 2013 2016 2010 2015 2016 2017 2015 2017 2017 2016
## [25169] 2016 2013 2015 2015 2017 2008 2016 2013 2015 2012 2011 2008 2016
## [25182] 2014 2016 2016 2012 2017 2010 2014 2010 2016 2010 2013 2007 2015
## [25195] 2015 2016 2015 2014 2016 2009 2011 2009 2007 2015 2009 2014 2016
## [25208] 2017 2008 2015 2017 2009 2015 2015 2016 2009 2015 2016 2017 2017
## [25221] 2017 2015 2015 2016 2017 2017 2014 2014 2015 2017 2012 2007 2016
## [25234] 2017 2009 2016 2009 2012 2007 2007 2008 2016 2015 2015 2013 2016
## [25247] 2017 2009 2017 2009 2015 2017 2009 2012 2014 2017 2011 2014 2016
## [25260] 2015 2007 2013 2015 2015 2014 2016 2016 2015 2010 2013 2015 2016
## [25273] 2007 2009 2015 2016 2016 2017 2013 2015 2016 2017 2007 2010 2017
## [25286] 2017 2009 2007 2015 2016 2009 2014 2016 2008 2008 2017 2007 2010
## [25299] 2013 2013 2016 2017 2017 2012 2010 2009 2008 2017 2017 2007 2017
## [25312] 2012 2007 2012 2016 2009 2014 2016 2016 2016 2010 2008 2007 2009
## [25325] 2010 2014 2017 2012 2015 2015 2017 2017 2017 2014 2017 2011 2014
## [25338] 2009 2016 2015 2016 2017 2015 2008 2010 2010 2015 2017 2011 2008
## [25351] 2014 2016 2008 2015 2016 2017 2010 2014 2015 2016 2016 2016 2017
## [25364] 2008 2014 2015 2015 2016 2015 2016 2012 2010 2014 2016 2017 2011
## [25377] 2008 2007 2008 2012 2013 2012 2007 2011 2015 2015 2016 2017 2014
## [25390] 2015 2017 2011 2016 2017 2017 2011 2012 2014 2017 2015 2016 2015
## [25403] 2015 2017 2014 2016 2016 2015 2011 2007 2015 2015 2007 2017 2017
## [25416] 2016 2011 2009 2016 2016 2017 2017 2012 2016 2010 2016 2012 2011
## [25429] 2009 2015 2016 2017 2017 2017 2007 2011 2007 2012 2008 2010 2015
## [25442] 2017 2014 2016 2016 2008 2015 2012 2014 2016 2015 2008 2007 2017
## [25455] 2017 2014 2007 2013 2015 2009 2015 2017 2015 2011 2010 2015 2013
## [25468] 2011 2016 2010 2015 2016 2017 2012 2014 2015 2017 2017 2016 2016
## [25481] 2015 2013 2015 2015 2017 2017 2016 2017 2017 2016 2010 2012 2016
## [25494] 2017 2016 2007 2010 2009 2009 2017 2017 2009 2016 2017 2011 2016
## [25507] 2016 2008 2015 2016 2015 2015 2008 2009 2012 2016 2017 2010 2009
## [25520] 2017 2017 2012 2011 2015 2008 2009 2017 2010 2010 2011 2015 2016
## [25533] 2014 2017 2015 2016 2015 2016 2017 2009 2009 2017 2008 2007 2013
## [25546] 2017 2010 2012 2011 2012 2016 2009 2015 2016 2017 2008 2012 2016
## [25559] 2016 2017 2008 2016 2017 2007 2007 2013 2015 2017 2007 2016 2017
## [25572] 2012 2015 2017 2017 2008 2017 2017 2015 2016 2016 2017 2017 2013
## [25585] 2016 2016 2017 2017 2016 2011 2013 2016 2012 2010 2017 2017 2014
## [25598] 2016 2015 2010 2009 2013 2012 2015 2009 2015 2016 2017 2016 2017
## [25611] 2010 2010 2015 2016 2016 2016 2011 2013 2016 2016 2009 2009 2016
## [25624] 2007 2016 2016 2016 2012 2013 2015 2016 2007 2008 2014 2017 2012
## [25637] 2010 2014 2015 2010 2012 2015 2017 2009 2007 2016 2016 2017 2015
## [25650] 2016 2009 2008 2010 2007 2016 2017 2016 2016 2017 2010 2009 2017
## [25663] 2015 2016 2016 2017 2010 2016 2017 2011 2017 2015 2008 2017 2007
## [25676] 2016 2015 2017 2010 2017 2017 2014 2017 2015 2017 2009 2007 2014
## [25689] 2016 2016 2017 2017 2007 2008 2017 2017 2012 2010 2016 2016 2009
## [25702] 2012 2013 2017 2013 2011 2012 2014 2016 2017 2017 2015 2007 2011
## [25715] 2008 2012 2013 2015 2016 2007 2016 2017 2016 2015 2017 2011 2016
## [25728] 2016 2017 2015 2017 2012 2016 2017 2016 2012 2011 2007 2013 2015
## [25741] 2014 2007 2015 2017 2017 2011 2010 2009 2013 2014 2014 2016 2012
## [25754] 2012 2011 2008 2007 2016 2015 2007 2011 2015 2015 2011 2014 2016
## [25767] 2017 2016 2015 2009 2017 2017 2011 2015 2016 2017 2007 2010 2013
## [25780] 2014 2014 2007 2016 2017 2015 2011 2015 2016 2014 2007 2017 2008
## [25793] 2007 2011 2015 2015 2008 2017 2009 2016 2008 2014 2016 2008 2016
## [25806] 2008 2016 2017 2009 2007 2014 2016 2017 2007 2016 2017 2008 2017
## [25819] 2011 2009 2010 2010 2017 2017 2014 2016 2017 2015 2016 2017 2010
## [25832] 2010 2015 2017 2011 2008 2011 2016 2017 2017 2009 2008 2013 2008
## [25845] 2013 2015 2017 2017 2007 2016 2017 2017 2011 2016 2015 2016 2010
## [25858] 2016 2012 2013 2017 2016 2017 2017 2017 2009 2014 2015 2015 2016
## [25871] 2007 2016 2017 2015 2017 2017 2013 2015 2015 2017 2009 2013 2016
## [25884] 2017 2016 2016 2016 2013 2016 2016 2017 2008 2010 2017 2007 2013
## [25897] 2016 2015 2015 2011 2014 2015 2015 2017 2017 2017 2016 2016 2015
## [25910] 2008 2016 2015 2017 2012 2007 2014 2012 2012 2015 2008 2016 2016
## [25923] 2017 2011 2010 2013 2017 2017 2012 2007 2016 2015 2017 2013 2015
## [25936] 2010 2008 2015 2016 2017 2017 2016 2008 2014 2008 2014 2017 2017
## [25949] 2013 2015 2016 2016 2016 2014 2012 2008 2015 2017 2008 2014 2016
## [25962] 2009 2011 2015 2008 2016 2007 2014 2015 2007 2010 2011 2012 2015
## [25975] 2016 2016 2016 2011 2010 2015 2015 2015 2007 2009 2013 2014 2010
## [25988] 2009 2015 2015 2016 2016 2017 2017 2017 2011 2013 2016 2014 2008
## [26001] 2015 2016 2012 2016 2015 2015 2015 2009 2015 2016 2016 2017 2009
## [26014] 2007 2012 2011 2017 2017 2010 2008 2017 2017 2014 2015 2017 2011
## [26027] 2013 2016 2015 2015 2017 2010 2010 2016 2016 2016 2016 2015 2015
## [26040] 2016 2016 2016 2016 2016 2016 2016 2016 2012 2016 2017 2017 2016
## [26053] 2017 2007 2007 2015 2014 2010 2009 2015 2016 2014 2011 2011 2011
## [26066] 2011 2016 2011 2016 2010 2015 2015 2017 2008 2007 2010 2010 2016
## [26079] 2008 2015 2016 2015 2007 2010 2011 2015 2011 2017 2017 2007 2008
## [26092] 2015 2016 2007 2008 2014 2015 2017 2016 2017 2017 2016 2010 2012
## [26105] 2015 2016 2017 2010 2015 2016 2012 2015 2011 2017 2008 2015 2016
## [26118] 2017 2008 2012 2016 2017 2016 2017 2010 2016 2017 2017 2016 2009
## [26131] 2010 2010 2011 2015 2015 2014 2015 2015 2011 2012 2014 2017 2012
## [26144] 2016 2009 2013 2014 2015 2011 2009 2012 2012 2015 2016 2008 2012
## [26157] 2017 2013 2008 2012 2009 2014 2015 2016 2015 2012 2009 2015 2015
## [26170] 2017 2007 2015 2015 2017 2013 2015 2016 2016 2009 2009 2016 2008
## [26183] 2007 2007 2011 2015 2017 2016 2017 2017 2011 2016 2010 2008 2014
## [26196] 2017 2017 2017 2012 2015 2016 2010 2012 2017 2007 2014 2015 2015
## [26209] 2017 2017 2012 2007 2009 2016 2017 2016 2016 2016 2017 2015 2014
## [26222] 2015 2015 2007 2016 2015 2007 2015 2007 2009 2007 2009 2015 2010
## [26235] 2009 2009 2011 2015 2016 2010 2007 2008 2012 2015 2009 2012 2010
## [26248] 2016 2017 2008 2015 2009 2017 2010 2013 2014 2017 2007 2013 2014
## [26261] 2016 2016 2016 2017 2013 2016 2015 2007 2013 2016 2010 2010 2014
## [26274] 2007 2017 2012 2016 2016 2017 2010 2016 2017 2017 2013 2014 2014
## [26287] 2016 2017 2012 2012 2011 2014 2016 2017 2015 2017 2017 2007 2011
## [26300] 2017 2010 2008 2015 2015 2015 2016 2017 2013 2016 2009 2015 2009
## [26313] 2016 2016 2017 2017 2016 2017 2017 2017 2017 2011 2013 2014 2016
## [26326] 2016 2012 2008 2017 2017 2017 2007 2016 2017 2015 2016 2010 2017
## [26339] 2007 2008 2013 2014 2011 2017 2011 2016 2016 2017 2007 2008 2015
## [26352] 2017 2015 2015 2016 2016 2015 2017 2008 2011 2012 2015 2016 2013
## [26365] 2016 2014 2015 2015 2017 2017 2009 2014 2014 2017 2009 2010 2009
## [26378] 2015 2015 2011 2012 2008 2015 2015 2014 2014 2015 2016 2017 2017
## [26391] 2016 2016 2011 2012 2010 2017 2012 2016 2016 2016 2015 2017 2011
## [26404] 2010 2010 2012 2015 2007 2017 2009 2017 2017 2011 2010 2016 2008
## [26417] 2016 2016 2014 2017 2009 2010 2016 2017 2017 2009 2008 2017 2010
## [26430] 2015 2012 2011 2009 2014 2009 2008 2009 2016 2016 2011 2016 2017
## [26443] 2008 2015 2015 2015 2008 2010 2013 2016 2015 2008 2016 2016 2017
## [26456] 2017 2008 2017 2009 2017 2017 2017 2017 2011 2015 2016 2012 2016
## [26469] 2013 2013 2015 2017 2015 2016 2017 2017 2017 2007 2011 2015 2015
## [26482] 2015 2015 2016 2016 2007 2015 2015 2017 2015 2009 2013 2015 2017
## [26495] 2017 2011 2017 2011 2015 2016 2017 2009 2007 2015 2017 2017 2012
## [26508] 2017 2011 2008 2010 2011 2015 2016 2010 2016 2014 2007 2007 2011
## [26521] 2017 2017 2008 2007 2015 2016 2008 2011 2011 2009 2017 2013 2016
## [26534] 2015 2008 2010 2014 2015 2016 2015 2017 2011 2010 2010 2011 2017
## [26547] 2016 2016 2016 2017 2017 2007 2017 2007 2016 2016 2011 2015 2015
## [26560] 2015 2017 2009 2008 2016 2017 2014 2016 2010 2008 2015 2009 2015
## [26573] 2016 2016 2016 2017 2008 2015 2016 2017 2012 2007 2008 2012 2013
## [26586] 2017 2016 2009 2012 2010 2016 2015 2017 2017 2007 2015 2010 2017
## [26599] 2007 2015 2016 2015 2016 2017 2007 2017 2007 2008 2008 2017 2015
## [26612] 2015 2015 2015 2010 2011 2015 2016 2017 2009 2013 2016 2017 2012
## [26625] 2011 2014 2012 2011 2008 2013 2016 2016 2012 2011 2007 2014 2014
## [26638] 2016 2016 2017 2017 2008 2012 2014 2014 2016 2015 2012 2016 2017
## [26651] 2017 2007 2016 2016 2011 2015 2015 2010 2016 2013 2014 2010 2015
## [26664] 2017 2013 2009 2008 2009 2013 2017 2017 2017 2009 2007 2016 2016
## [26677] 2016 2010 2010 2015 2016 2017 2012 2007 2013 2014 2015 2016 2016
## [26690] 2015 2016 2010 2009 2017 2007 2007 2015 2017 2015 2015 2015 2009
## [26703] 2016 2012 2011 2017 2012 2009 2014 2015 2010 2016 2012 2010 2009
## [26716] 2014 2015 2011 2015 2015 2016 2011 2014 2015 2016 2017 2017 2012
## [26729] 2017 2011 2015 2012 2014 2015 2016 2007 2015 2016 2017 2008 2008
## [26742] 2015 2009 2016 2014 2014 2015 2010 2012 2010 2017 2016 2017 2013
## [26755] 2016 2017 2017 2009 2013 2016 2010 2014 2015 2007 2017 2017 2010
## [26768] 2015 2016 2014 2015 2017 2012 2008 2016 2016 2011 2011 2011 2015
## [26781] 2015 2012 2016 2017 2017 2012 2013 2009 2008 2013 2016 2017 2017
## [26794] 2010 2010 2012 2017 2017 2017 2015 2016 2014 2009 2015 2015 2013
## [26807] 2016 2017 2012 2007 2012 2017 2009 2016 2016 2009 2016 2017 2009
## [26820] 2013 2016 2017 2010 2007 2011 2016 2016 2016 2017 2012 2008 2016
## [26833] 2011 2016 2017 2017 2007 2015 2017 2013 2016 2017 2010 2016 2009
## [26846] 2017 2017 2008 2016 2016 2008 2009 2017 2010 2007 2016 2011 2017
## [26859] 2017 2013 2014 2010 2012 2016 2015 2015 2017 2012 2009 2017 2008
## [26872] 2016 2016 2016 2015 2007 2012 2010 2010 2016 2017 2015 2016 2017
## [26885] 2017 2008 2007 2017 2017 2017 2016 2013 2017 2009 2016 2015 2017
## [26898] 2009 2016 2016 2017 2015 2009 2008 2013 2016 2007 2017 2017 2007
## [26911] 2011 2015 2016 2014 2015 2017 2010 2011 2014 2016 2015 2017 2015
## [26924] 2015 2015 2016 2017 2012 2013 2015 2011 2010 2014 2016 2015 2016
## [26937] 2016 2017 2017 2008 2009 2017 2015 2008 2011 2016 2017 2009 2013
## [26950] 2016 2011 2008 2016 2016 2016 2010 2016 2015 2009 2015 2017 2017
## [26963] 2011 2017 2007 2007 2009 2015 2017 2014 2015 2017 2015 2016 2009
## [26976] 2015 2014 2010 2011 2014 2011 2010 2015 2015 2017 2017 2009 2015
## [26989] 2017 2009 2007 2007 2012 2014 2016 2017 2009 2016 2015 2017 2009
## [27002] 2014 2015 2010 2015 2015 2015 2016 2012 2008 2015 2012 2014 2015
## [27015] 2012 2017 2015 2007 2015 2010 2016 2017 2013 2014 2007 2012 2010
## [27028] 2016 2008 2011 2014 2017 2017 2017 2017 2008 2015 2016 2012 2008
## [27041] 2007 2014 2016 2015 2011 2015 2016 2010 2016 2015 2016 2016 2017
## [27054] 2017 2014 2017 2010 2017 2013 2016 2007 2009 2016 2016 2016 2009
## [27067] 2011 2012 2008 2016 2016 2011 2007 2012 2016 2017 2017 2009 2013
## [27080] 2013 2016 2017 2009 2017 2013 2013 2015 2008 2009 2014 2016 2017
## [27093] 2017 2008 2010 2012 2010 2016 2016 2017 2007 2008 2014 2007 2017
## [27106] 2016 2011 2011 2017 2012 2015 2016 2017 2013 2017 2012 2016 2008
## [27119] 2012 2010 2015 2008 2013 2011 2007 2013 2017 2015 2017 2010 2016
## [27132] 2016 2014 2016 2012 2015 2017 2007 2014 2015 2011 2013 2015 2016
## [27145] 2012 2010 2010 2014 2015 2015 2016 2011 2011 2014 2015 2015 2016
## [27158] 2007 2015 2017 2017 2017 2011 2016 2015 2017 2008 2010 2008 2013
## [27171] 2016 2008 2016 2017 2010 2007 2009 2016 2012 2007 2015 2015 2016
## [27184] 2016 2016 2017 2017 2007 2007 2014 2016 2016 2017 2015 2017 2010
## [27197] 2016 2016 2015 2017 2017 2017 2017 2017 2013 2009 2007 2012 2015
## [27210] 2010 2010 2014 2016 2017 2011 2017 2008 2011 2007 2007 2017 2007
## [27223] 2016 2011 2012 2010 2008 2010 2015 2016 2016 2010 2015 2016 2017
## [27236] 2008 2008 2012 2010 2017 2017 2013 2009 2011 2015 2012 2016 2010
## [27249] 2016 2017 2012 2007 2016 2012 2012 2008 2015 2017 2011 2016 2015
## [27262] 2016 2012 2015 2015 2009 2012 2014 2015 2017 2017 2011 2015 2015
## [27275] 2016 2008 2010 2010 2016 2017 2016 2012 2016 2017 2008 2015 2017
## [27288] 2007 2013 2017 2010 2017 2016 2016 2015 2017 2016 2009 2010 2011
## [27301] 2011 2013 2015 2016 2015 2011 2011 2014 2015 2013 2014 2016 2011
## [27314] 2010 2016 2017 2009 2007 2009 2009 2017 2017 2017 2008 2007 2011
## [27327] 2008 2014 2017 2008 2016 2007 2014 2015 2007 2007 2013 2015 2015
## [27340] 2016 2017 2017 2017 2008 2010 2012 2007 2017 2015 2016 2017 2015
## [27353] 2017 2012 2015 2016 2016 2016 2016 2017 2009 2009 2009 2008 2011
## [27366] 2015 2010 2009 2013 2016 2016 2009 2012 2011 2016 2007 2007 2013
## [27379] 2015 2015 2015 2013 2008 2009 2010 2017 2017 2016 2013 2017 2017
## [27392] 2015 2016 2009 2012 2016 2015 2015 2017 2008 2015 2016 2016 2016
## [27405] 2012 2010 2008 2015 2007 2015 2016 2016 2007 2016 2017 2016 2016
## [27418] 2015 2017 2012 2011 2015 2016 2010 2011 2010 2009 2008 2017 2015
## [27431] 2015 2016 2016 2015 2017 2011 2007 2007 2014 2015 2016 2017 2009
## [27444] 2014 2017 2015 2010 2011 2011 2012 2015 2016 2011 2011 2016 2017
## [27457] 2010 2015 2008 2015 2009 2017 2008 2016 2016 2007 2008 2008 2017
## [27470] 2015 2008 2013 2017 2017 2012 2015 2016 2017 2011 2009 2017 2017
## [27483] 2013 2014 2014 2017 2017 2012 2007 2008 2015 2007 2008 2013 2017
## [27496] 2008 2016 2017 2013 2016 2015 2015 2015 2016 2015 2007 2015 2012
## [27509] 2014 2010 2016 2015 2012 2010 2013 2015 2016 2015 2017 2012 2015
## [27522] 2016 2017 2008 2011 2010 2014 2015 2017 2016 2017 2011 2016 2016
## [27535] 2016 2016 2017 2009 2007 2013 2016 2007 2016 2017 2012 2016 2014
## [27548] 2015 2016 2017 2009 2009 2014 2016 2017 2009 2008 2013 2016 2016
## [27561] 2011 2010 2010 2016 2015 2017 2010 2008 2015 2012 2017 2013 2015
## [27574] 2011 2017 2017 2008 2016 2008 2016 2017 2008 2012 2013 2017 2017
## [27587] 2017 2016 2010 2017 2007 2009 2013 2015 2017 2015 2016 2009 2008
## [27600] 2011 2009 2007 2009 2015 2017 2017 2012 2010 2009 2008 2007 2016
## [27613] 2016 2015 2016 2016 2016 2009 2011 2016 2014 2016 2017 2017 2008
## [27626] 2015 2017 2017 2008 2010 2015 2011 2007 2013 2016 2008 2016 2015
## [27639] 2008 2016 2016 2017 2015 2014 2015 2013 2015 2017 2010 2010 2015
## [27652] 2016 2008 2008 2015 2016 2015 2007 2016 2017 2013 2011 2008 2010
## [27665] 2016 2016 2015 2016 2017 2015 2015 2011 2008 2017 2016 2017 2007
## [27678] 2010 2015 2013 2015 2015 2009 2014 2015 2012 2015 2008 2009 2008
## [27691] 2011 2016 2016 2015 2015 2011 2017 2010 2016 2016 2008 2007 2008
## [27704] 2012 2016 2017 2017 2011 2016 2015 2011 2016 2016 2017 2015 2015
## [27717] 2008 2009 2008 2012 2015 2016 2017 2017 2008 2014 2017 2017 2015
## [27730] 2015 2016 2007 2008 2016 2014 2008 2014 2009 2013 2015 2015 2017
## [27743] 2017 2017 2017 2015 2016 2016 2007 2007 2016 2016 2017 2009 2016
## [27756] 2015 2009 2013 2016 2011 2013 2013 2016 2017 2009 2014 2016 2007
## [27769] 2014 2009 2013 2017 2012 2017 2017 2012 2012 2015 2017 2017 2011
## [27782] 2007 2010 2016 2009 2008 2017 2016 2010 2009 2013 2015 2015 2017
## [27795] 2013 2007 2013 2016 2017 2015 2015 2017 2017 2008 2011 2017 2007
## [27808] 2012 2013 2015 2015 2016 2017 2017 2010 2009 2008 2010 2016 2016
## [27821] 2017 2017 2011 2015 2017 2016 2012 2014 2008 2015 2016 2015 2011
## [27834] 2013 2014 2017 2008 2017 2013 2015 2016 2017 2014 2008 2009 2009
## [27847] 2012 2016 2017 2012 2009 2014 2014 2015 2007 2016 2014 2016 2008
## [27860] 2015 2017 2011 2016 2014 2015 2009 2008 2017 2014 2016 2015 2012
## [27873] 2015 2011 2010 2015 2015 2017 2009 2010 2007 2013 2009 2016 2014
## [27886] 2016 2015 2009 2011 2011 2017 2017 2016 2007 2016 2017 2011 2008
## [27899] 2015 2017 2012 2013 2015 2016 2010 2016 2016 2017 2013 2007 2015
## [27912] 2007 2008 2013 2017 2009 2016 2014 2015 2007 2016 2010 2017 2017
## [27925] 2007 2013 2015 2017 2007 2010 2016 2016 2017 2012 2007 2017 2015
## [27938] 2008 2017 2009 2015 2010 2011 2009 2010 2008 2014 2016 2016 2007
## [27951] 2010 2015 2014 2010 2011 2015 2017 2009 2016 2007 2014 2016 2015
## [27964] 2017 2013 2010 2015 2011 2007 2016 2012 2017 2017 2016 2016 2017
## [27977] 2010 2016 2016 2010 2015 2011 2009 2017 2011 2015 2008 2013 2017
## [27990] 2008 2015 2015 2008 2007 2011 2016 2015 2016 2011 2016 2016 2017
## [28003] 2016 2015 2017 2013 2017 2017 2017 2010 2017 2009 2008 2017 2017
## [28016] 2017 2017 2008 2017 2015 2017 2007 2015 2017 2017 2008 2013 2012
## [28029] 2015 2017 2013 2011 2009 2014 2015 2017 2013 2016 2009 2007 2008
## [28042] 2010 2017 2014 2009 2016 2011 2015 2015 2016 2008 2014 2011 2007
## [28055] 2014 2016 2017 2007 2008 2012 2012 2017 2010 2017 2015 2017 2017
## [28068] 2012 2010 2010 2017 2010 2011 2010 2007 2011 2015 2016 2016 2016
## [28081] 2013 2015 2012 2012 2009 2015 2016 2008 2015 2008 2013 2016 2016
## [28094] 2016 2015 2015 2017 2009 2010 2015 2016 2007 2016 2011 2015 2015
## [28107] 2015 2009 2011 2015 2017 2011 2010 2013 2014 2015 2016 2013 2015
## [28120] 2016 2015 2010 2011 2015 2017 2017 2007 2009 2010 2016 2017 2017
## [28133] 2017 2012 2017 2015 2012 2009 2015 2016 2007 2015 2012 2010 2010
## [28146] 2015 2015 2017 2008 2016 2016 2015 2017 2017 2009 2007 2017 2016
## [28159] 2016 2015 2016 2017 2009 2007 2015 2015 2015 2015 2008 2014 2017
## [28172] 2016 2016 2017 2008 2012 2013 2016 2017 2011 2007 2014 2016 2011
## [28185] 2008 2017 2011 2015 2016 2014 2017 2007 2011 2010 2015 2015 2017
## [28198] 2017 2017 2016 2017 2008 2010 2013 2013 2015 2016 2010 2011 2015
## [28211] 2017 2008 2012 2007 2009 2014 2014 2009 2009 2016 2017 2011 2016
## [28224] 2009 2012 2017 2016 2015 2011 2013 2016 2017 2017 2011 2016 2016
## [28237] 2011 2010 2017 2017 2017 2015 2011 2014 2016 2014 2015 2016 2007
## [28250] 2013 2014 2008 2009 2017 2010 2015 2016 2016 2015 2016 2015 2017
## [28263] 2007 2016 2016 2014 2016 2017 2017 2009 2016 2016 2016 2017 2007
## [28276] 2016 2015 2012 2015 2017 2009 2008 2009 2015 2016 2017 2011 2017
## [28289] 2007 2013 2016 2015 2015 2015 2016 2017 2012 2010 2015 2017 2011
## [28302] 2011 2013 2015 2015 2016 2009 2008 2009 2010 2016 2016 2017 2017
## [28315] 2013 2015 2015 2016 2017 2017 2015 2017 2007 2016 2012 2010 2013
## [28328] 2010 2009 2008 2010 2015 2016 2015 2015 2017 2017 2017 2008 2017
## [28341] 2008 2011 2014 2016 2008 2009 2015 2017 2016 2008 2013 2013 2016
## [28354] 2016 2015 2016 2017 2017 2017 2016 2008 2010 2015 2014 2016 2016
## [28367] 2016 2012 2007 2014 2015 2009 2016 2017 2017 2009 2016 2011 2008
## [28380] 2011 2009 2016 2016 2015 2016 2015 2011 2015 2015 2017 2015 2017
## [28393] 2016 2008 2014 2015 2015 2016 2016 2016 2009 2007 2013 2016 2015
## [28406] 2017 2012 2010 2016 2016 2015 2017 2009 2016 2017 2014 2009 2016
## [28419] 2015 2016 2017 2014 2015 2016 2016 2009 2016 2016 2010 2015 2017
## [28432] 2015 2016 2016 2016 2015 2016 2007 2011 2016 2010 2016 2017 2017
## [28445] 2013 2017 2017 2016 2016 2015 2017 2017 2010 2013 2014 2015 2008
## [28458] 2014 2015 2016 2015 2017 2009 2017 2016 2007 2013 2017 2007 2012
## [28471] 2015 2008 2010 2007 2009 2007 2016 2016 2016 2009 2009 2015 2007
## [28484] 2017 2013 2015 2016 2015 2017 2015 2007 2015 2016 2017 2012 2012
## [28497] 2016 2016 2009 2016 2016 2016 2008 2017 2017 2013 2016 2015 2009
## [28510] 2007 2008 2011 2007 2015 2015 2015 2017 2015 2016 2008 2008 2016
## [28523] 2015 2016 2009 2009 2016 2007 2008 2017 2008 2017 2008 2016 2017
## [28536] 2010 2008 2016 2017 2008 2011 2015 2016 2011 2015 2016 2010 2007
## [28549] 2011 2015 2016 2007 2007 2015 2017 2017 2017 2017 2016 2016 2017
## [28562] 2017 2017 2016 2007 2016 2010 2011 2009 2016 2009 2016 2016 2007
## [28575] 2013 2016 2008 2015 2010 2015 2017 2014 2016 2016 2009 2015 2015
## [28588] 2015 2016 2009 2007 2017 2015 2010 2008 2008 2013 2016 2011 2015
## [28601] 2008 2017 2016 2017 2015 2015 2016 2016 2016 2015 2015 2017 2010
## [28614] 2016 2017 2017 2010 2016 2015 2017 2007 2013 2013 2016 2017 2016
## [28627] 2010 2015 2016 2017 2017 2007 2010 2017 2010 2008 2008 2014 2016
## [28640] 2008 2013 2013 2008 2013 2016 2007 2011 2015 2013 2015 2017 2017
## [28653] 2011 2015 2016 2017 2008 2015 2011 2008 2017 2008 2007 2011 2014
## [28666] 2017 2014 2016 2016 2017 2017 2015 2017 2017 2011 2015 2007 2013
## [28679] 2008 2013 2014 2016 2008 2017 2015 2017 2016 2017 2012 2008 2010
## [28692] 2013 2012 2007 2013 2015 2015 2015 2016 2007 2009 2015 2016 2014
## [28705] 2017 2012 2012 2014 2017 2007 2015 2015 2012 2015 2016 2015 2016
## [28718] 2017 2014 2014 2015 2010 2015 2008 2015 2017 2009 2009 2008 2007
## [28731] 2007 2015 2015 2015 2007 2007 2011 2011 2015 2007 2007 2012 2008
## [28744] 2010 2016 2011 2009 2014 2017 2008 2010 2016 2007 2008 2013 2017
## [28757] 2016 2016 2017 2016 2016 2007 2013 2016 2017 2008 2011 2017 2010
## [28770] 2007 2008 2014 2015 2016 2017 2013 2015 2017 2011 2010 2016 2017
## [28783] 2007 2015 2012 2009 2011 2012 2007 2015 2016 2016 2016 2017 2012
## [28796] 2013 2017 2011 2016 2016 2016 2017 2015 2016 2010 2010 2007 2017
## [28809] 2017 2013 2016 2017 2015 2017 2009 2015 2008 2011 2010 2011 2017
## [28822] 2013 2012 2015 2017 2011 2014 2011 2008 2009 2013 2015 2012 2017
## [28835] 2007 2016 2016 2007 2009 2016 2016 2016 2017 2013 2016 2017 2017
## [28848] 2012 2017 2012 2014 2011 2017 2017 2015 2010 2013 2013 2016 2016
## [28861] 2017 2011 2016 2017 2017 2017 2011 2008 2013 2016 2012 2008 2012
## [28874] 2012 2016 2017 2017 2016 2010 2007 2015 2016 2014 2017 2017 2010
## [28887] 2015 2007 2015 2017 2015 2017 2015 2017 2015 2010 2009 2017 2017
## [28900] 2016 2012 2011 2009 2016 2016 2017 2015 2011 2012 2012 2013 2015
## [28913] 2017 2009 2014 2015 2010 2007 2014 2015 2017 2009 2014 2011 2009
## [28926] 2012 2017 2007 2015 2011 2012 2007 2015 2010 2007 2008 2016 2017
## [28939] 2008 2008 2017 2008 2012 2016 2017 2008 2009 2013 2015 2012 2013
## [28952] 2016 2008 2012 2017 2011 2015 2013 2015 2016 2017 2016 2007 2009
## [28965] 2009 2015 2017 2010 2011 2017 2009 2009 2010 2014 2007 2017 2017
## [28978] 2017 2007 2015 2008 2009 2015 2015 2017 2016 2015 2008 2017 2017
## [28991] 2011 2017 2012 2008 2012 2017 2007 2007 2017 2012 2014 2008 2009
## [29004] 2016 2007 2014 2016 2016 2015 2012 2014 2016 2007 2013 2010 2014
## [29017] 2015 2016 2010 2016 2015 2017 2017 2015 2017 2010 2009 2017 2016
## [29030] 2014 2015 2014 2015 2016 2016 2017 2017 2012 2010 2014 2015 2009
## [29043] 2012 2008 2012 2016 2017 2012 2012 2015 2017 2013 2015 2015 2017
## [29056] 2011 2014 2012 2016 2017 2010 2007 2013 2015 2012 2017 2007 2012
## [29069] 2009 2010 2008 2017 2016 2007 2015 2017 2010 2016 2017 2009 2012
## [29082] 2009 2009 2009 2016 2016 2016 2013 2013 2010 2015 2007 2011 2011
## [29095] 2017 2010 2008 2013 2016 2008 2013 2015 2016 2017 2017 2012 2007
## [29108] 2015 2017 2017 2010 2017 2017 2011 2015 2016 2010 2013 2015 2016
## [29121] 2013 2016 2016 2016 2016 2017 2012 2007 2013 2009 2009 2011 2016
## [29134] 2011 2016 2017 2017 2013 2008 2014 2017 2011 2010 2016 2016 2017
## [29147] 2017 2016 2012 2016 2017 2017 2007 2008 2016 2016 2017 2012 2012
## [29160] 2015 2015 2016 2012 2011 2009 2016 2017 2014 2016 2015 2009 2007
## [29173] 2015 2016 2015 2010 2016 2017 2010 2012 2015 2016 2008 2013 2016
## [29186] 2016 2010 2007 2012 2015 2016 2016 2017 2011 2015 2016 2015 2017
## [29199] 2015 2015 2015 2015 2017 2015 2016 2013 2013 2014 2016 2017 2017
## [29212] 2011 2015 2007 2016 2016 2016 2017 2010 2010 2016 2017 2017 2015
## [29225] 2017 2017 2015 2015 2017 2008 2011 2010 2012 2016 2010 2009 2013
## [29238] 2015 2017 2009 2015 2015 2017 2007 2013 2014 2017 2011 2016 2015
## [29251] 2012 2015 2009 2014 2017 2008 2012 2011 2016 2008 2016 2009 2016
## [29264] 2016 2017 2011 2013 2015 2007 2015 2017 2016 2015 2017 2013 2008
## [29277] 2011 2013 2016 2007 2015 2017 2008 2015 2016 2015 2016 2011 2008
## [29290] 2014 2011 2009 2017 2008 2017 2017 2007 2012 2011 2015 2015 2016
## [29303] 2011 2012 2017 2015 2007 2011 2013 2015 2016 2016 2013 2017 2008
## [29316] 2008 2010 2014 2016 2010 2007 2016 2011 2016 2008 2008 2008 2017
## [29329] 2017 2015 2008 2016 2009 2010 2008 2015 2016 2017 2017 2011 2014
## [29342] 2015 2008 2009 2016 2017 2014 2015 2015 2016 2012 2016 2017 2008
## [29355] 2016 2011 2015 2015 2015 2016 2016 2016 2017 2010 2016 2017 2009
## [29368] 2015 2016 2015 2008 2015 2016 2008 2011 2008 2009 2016 2017 2007
## [29381] 2016 2013 2015 2017 2017 2017 2007 2015 2017 2017 2007 2014 2015
## [29394] 2016 2017 2007 2017 2017 2008 2011 2016 2007 2011 2011 2015 2016
## [29407] 2017 2012 2015 2013 2008 2016 2016 2017 2014 2009 2007 2011 2011
## [29420] 2010 2016 2016 2017 2012 2013 2014 2017 2016 2016 2010 2015 2017
## [29433] 2009 2016 2016 2007 2013 2016 2016 2017 2011 2010 2009 2013 2011
## [29446] 2016 2008 2013 2010 2015 2017 2017 2009 2008 2007 2016 2015 2010
## [29459] 2014 2015 2015 2017 2015 2008 2016 2009 2015 2017 2015 2015 2016
## [29472] 2017 2015 2017 2015 2017 2017 2012 2008 2016 2017 2017 2015 2014
## [29485] 2015 2017 2017 2009 2007 2011 2011 2011 2011 2008 2015 2017 2009
## [29498] 2014 2015 2009 2009 2014 2015 2017 2017 2015 2015 2017 2011 2014
## [29511] 2015 2008 2009 2010 2016 2013 2015 2017 2010 2014 2016 2017 2012
## [29524] 2015 2010 2014 2016 2007 2012 2015 2016 2017 2012 2012 2012 2015
## [29537] 2017 2008 2008 2009 2008 2008 2012 2016 2015 2017 2013 2016 2009
## [29550] 2011 2015 2017 2017 2013 2017 2010 2007 2011 2014 2016 2016 2012
## [29563] 2008 2011 2015 2016 2017 2013 2017 2007 2009 2009 2007 2014 2008
## [29576] 2011 2012 2015 2015 2017 2012 2014 2015 2011 2007 2008 2008 2014
## [29589] 2016 2015 2017 2013 2017 2009 2016 2016 2015 2008 2007 2010 2016
## [29602] 2017 2012 2016 2017 2008 2008 2007 2010 2016 2017 2008 2016 2016
## [29615] 2012 2017 2007 2012 2016 2016 2008 2012 2016 2017 2008 2015 2016
## [29628] 2016 2017 2014 2008 2012 2007 2015 2015 2010 2012 2015 2015 2017
## [29641] 2007 2013 2011 2010 2015 2015 2017 2017 2011 2014 2015 2011 2011
## [29654] 2010 2011 2013 2008 2014 2015 2017 2009 2014 2015 2016 2008 2008
## [29667] 2016 2017 2012 2016 2007 2017 2017 2015 2017 2008 2015 2016 2017
## [29680] 2017 2008 2007 2017 2017 2011 2014 2008 2008 2015 2012 2009 2015
## [29693] 2015 2010 2017 2011 2013 2016 2016 2017 2016 2017 2011 2015 2017
## [29706] 2008 2008 2011 2015 2015 2016 2016 2010 2009 2017 2007 2012 2007
## [29719] 2009 2016 2017 2017 2017 2007 2015 2015 2016 2016 2007 2015 2016
## [29732] 2017 2017 2017 2011 2014 2017 2015 2017 2017 2010 2008 2016 2016
## [29745] 2011 2014 2016 2017 2013 2017 2009 2010 2014 2017 2017 2017 2008
## [29758] 2016 2008 2016 2017 2017 2017 2017 2016 2017 2017 2009 2010 2011
## [29771] 2009 2011 2015 2016 2015 2011 2011 2008 2013 2017 2007 2016 2017
## [29784] 2008 2013 2015 2016 2016 2017 2017 2011 2017 2017 2016 2016 2016
## [29797] 2017 2012 2015 2015 2008 2009 2013 2015 2017 2016 2015 2010 2010
## [29810] 2013 2014 2015 2017 2007 2017 2015 2011 2011 2009 2015 2016 2017
## [29823] 2007 2014 2016 2017 2015 2015 2017 2017 2010 2017 2017 2017 2015
## [29836] 2016 2009 2010 2013 2016 2017 2007 2011 2010 2017 2013 2017 2017
## [29849] 2016 2016 2017 2016 2016 2010 2010 2015 2016 2010 2015 2016 2007
## [29862] 2015 2008 2009 2016 2017 2016 2008 2016 2015 2017 2010 2015 2016
## [29875] 2017 2016 2010 2017 2012 2015 2015 2017 2008 2017 2009 2008 2014
## [29888] 2014 2012 2007 2016 2017 2010 2010 2009 2016 2016 2015 2012 2014
## [29901] 2016 2016 2017 2011 2017 2017 2011 2015 2017 2011 2008 2015 2017
## [29914] 2015 2016 2017 2017 2010 2011 2014 2014 2015 2016 2014 2016 2007
## [29927] 2010 2016 2017 2017 2017 2017 2012 2017 2017 2014 2016 2016 2016
## [29940] 2017 2012 2008 2013 2014 2009 2010 2015 2015 2009 2015 2016 2016
## [29953] 2015 2016 2011 2012 2017 2008 2016 2017 2009 2012 2009 2010 2015
## [29966] 2017 2011 2007 2012 2008 2013 2015 2015 2017 2015 2009 2013 2017
## [29979] 2013 2014 2015 2011 2015 2016 2016 2017 2017 2010 2015 2017 2013
## [29992] 2011 2013 2014 2017 2014 2016 2014 2014 2015 2011 2008 2015 2017
## [30005] 2010 2014 2015 2017 2007 2008 2008 2010 2008 2009 2016 2017 2008
## [30018] 2010 2016 2010 2017 2008 2007 2015 2016 2017 2014 2009 2007 2013
## [30031] 2015 2007 2008 2013 2014 2014 2017 2011 2011 2013 2014 2016 2015
## [30044] 2008 2017 2013 2014 2016 2012 2009 2015 2015 2016 2015 2015 2017
## [30057] 2015 2017 2007 2011 2008 2014 2016 2015 2009 2016 2009 2009 2011
## [30070] 2008 2015 2009 2010 2016 2017 2011 2016 2010 2015 2015 2012 2014
## [30083] 2016 2015 2008 2016 2016 2016 2017 2012 2010 2010 2016 2012 2017
## [30096] 2015 2015 2017 2015 2016 2017 2014 2009 2015 2017 2014 2009 2013
## [30109] 2015 2016 2016 2009 2015 2016 2017 2009 2015 2016 2015 2015 2008
## [30122] 2010 2015 2016 2015 2016 2016 2007 2015 2016 2012 2011 2015 2015
## [30135] 2016 2009 2016 2017 2008 2013 2008 2011 2007 2010 2011 2017 2017
## [30148] 2011 2013 2014 2015 2015 2016 2017 2017 2009 2009 2011 2015 2010
## [30161] 2015 2015 2009 2013 2017 2016 2017 2016 2015 2008 2011 2016 2014
## [30174] 2016 2008 2016 2017 2015 2011 2016 2016 2015 2017 2016 2010 2014
## [30187] 2016 2007 2015 2016 2016 2010 2012 2007 2017 2017 2017 2013 2015
## [30200] 2016 2007 2015 2016 2010 2014 2015 2015 2008 2011 2016 2016 2009
## [30213] 2014 2014 2008 2013 2016 2010 2017 2009 2009 2014 2015 2017 2016
## [30226] 2014 2016 2007 2017 2015 2009 2017 2017 2013 2017 2009 2012 2008
## [30239] 2015 2007 2008 2016 2017 2011 2015 2017 2017 2012 2012 2011 2017
## [30252] 2014 2017 2017 2009 2016 2017 2017 2012 2014 2017 2017 2015 2017
## [30265] 2007 2013 2016 2016 2012 2014 2016 2016 2015 2009 2011 2016 2017
## [30278] 2017 2016 2017 2007 2013 2016 2017 2011 2014 2015 2013 2014 2015
## [30291] 2015 2011 2008 2017 2010 2016 2017 2008 2008 2016 2017 2012 2015
## [30304] 2015 2015 2017 2015 2017 2015 2017 2010 2016 2017 2009 2012 2012
## [30317] 2008 2012 2015 2008 2016 2017 2009 2012 2016 2010 2010 2013 2015
## [30330] 2015 2007 2013 2014 2016 2015 2016 2017 2012 2010 2009 2008 2015
## [30343] 2015 2017 2008 2008 2013 2015 2017 2010 2016 2016 2010 2016 2007
## [30356] 2009 2016 2016 2016 2015 2009 2014 2015 2011 2008 2009 2013 2008
## [30369] 2016 2011 2016 2015 2016 2016 2017 2017 2009 2015 2014 2015 2017
## [30382] 2011 2016 2017 2010 2014 2008 2012 2008 2015 2011 2015 2016 2017
## [30395] 2017 2012 2011 2017 2008 2016 2016 2012 2010 2013 2016 2017 2010
## [30408] 2008 2014 2016 2017 2015 2017 2017 2011 2011 2010 2009 2013 2008
## [30421] 2007 2007 2010 2009 2016 2007 2008 2017 2008 2013 2017 2015 2017
## [30434] 2011 2008 2015 2016 2016 2008 2011 2008 2017 2010 2016 2012 2017
## [30447] 2009 2015 2017 2010 2011 2011 2016 2016 2008 2010 2017 2008 2014
## [30460] 2008 2007 2017 2017 2011 2011 2016 2016 2015 2017 2013 2016 2017
## [30473] 2014 2014 2015 2014 2015 2015 2015 2017 2016 2016 2017 2017 2013
## [30486] 2014 2015 2007 2013 2015 2016 2016 2017 2008 2011 2014 2017 2007
## [30499] 2013 2017 2008 2008 2007 2015 2016 2015 2016 2017 2016 2017 2016
## [30512] 2017 2008 2016 2013 2015 2016 2017 2011 2016 2017 2015 2015 2017
## [30525] 2013 2015 2016 2013 2017 2015 2010 2007 2016 2015 2015 2016 2016
## [30538] 2017 2009 2017 2015 2017 2011 2011 2013 2016 2016 2017 2016 2017
## [30551] 2017 2017 2017 2009 2013 2008 2008 2010 2009 2014 2015 2014 2016
## [30564] 2016 2007 2015 2015 2016 2017 2012 2007 2016 2016 2009 2013 2015
## [30577] 2015 2016 2016 2011 2017 2012 2013 2013 2016 2007 2010 2015 2016
## [30590] 2017 2016 2014 2015 2017 2010 2009 2015 2008 2010 2008 2011 2012
## [30603] 2013 2014 2017 2015 2015 2016 2008 2011 2014 2015 2016 2015 2016
## [30616] 2015 2008 2010 2008 2008 2008 2016 2011 2014 2016 2010 2017 2010
## [30629] 2007 2015 2016 2008 2010 2016 2016 2013 2017 2011 2010 2007 2013
## [30642] 2016 2007 2017 2015 2013 2015 2007 2012 2013 2015 2017 2010 2014
## [30655] 2017 2009 2015 2014 2015 2016 2016 2017 2014 2016 2011 2017 2017
## [30668] 2010 2010 2017 2009 2014 2015 2011 2011 2017 2017 2017 2010 2009
## [30681] 2010 2015 2016 2017 2007 2008 2007 2016 2009 2008 2017 2014 2015
## [30694] 2016 2017 2014 2017 2017 2017 2015 2015 2016 2017 2007 2017 2017
## [30707] 2015 2016 2015 2016 2010 2007 2009 2007 2009 2012 2010 2011 2007
## [30720] 2016 2013 2014 2016 2017 2008 2017 2007 2010 2007 2008 2015 2016
## [30733] 2016 2013 2016 2015 2016 2016 2017 2017 2012 2015 2011 2014 2016
## [30746] 2012 2017 2007 2014 2017 2007 2016 2008 2014 2015 2015 2017 2017
## [30759] 2012 2011 2016 2007 2014 2016 2017 2015 2014 2016 2007 2007 2008
## [30772] 2015 2015 2011 2010 2009 2015 2014 2016 2017 2008 2016 2009 2011
## [30785] 2013 2016 2013 2015 2017 2016 2007 2012 2016 2017 2010 2016 2007
## [30798] 2008 2012 2014 2016 2017 2013 2014 2015 2017 2010 2015 2015 2010
## [30811] 2008 2009 2011 2014 2015 2016 2016 2009 2015 2014 2008 2013 2015
## [30824] 2016 2009 2008 2014 2016 2016 2017 2009 2015 2016 2016 2010 2009
## [30837] 2017 2010 2015 2015 2017 2017 2016 2014 2010 2016 2017 2010 2015
## [30850] 2008 2010 2013 2016 2009 2016 2007 2013 2016 2017 2013 2007 2017
## [30863] 2016 2017 2015 2016 2017 2008 2015 2016 2007 2012 2014 2011 2014
## [30876] 2016 2009 2016 2016 2016 2016 2015 2007 2008 2010 2011 2012 2009
## [30889] 2017 2009 2014 2014 2016 2008 2009 2010 2017 2015 2009 2017 2010
## [30902] 2017 2009 2017 2010 2008 2014 2016 2009 2009 2009 2009 2016 2016
## [30915] 2010 2012 2008 2016 2012 2015 2016 2017 2015 2016 2010 2009 2015
## [30928] 2015 2014 2014 2016 2008 2008 2015 2016 2009 2007 2007 2015 2016
## [30941] 2017 2010 2012 2016 2017 2016 2016 2010 2016 2007 2007 2015 2017
## [30954] 2017 2015 2010 2012 2013 2015 2014 2017 2016 2012 2017 2010 2016
## [30967] 2016 2017 2016 2017 2010 2007 2016 2017 2007 2011 2008 2009 2013
## [30980] 2014 2016 2007 2008 2011 2013 2015 2017 2015 2007 2013 2010 2008
## [30993] 2011 2015 2016 2017 2011 2010 2017 2017 2007 2010 2015 2015 2017
## [31006] 2007 2016 2009 2008 2011 2017 2008 2007 2010 2007 2009 2009 2013
## [31019] 2015 2017 2011 2010 2016 2016 2017 2012 2014 2007 2015 2017 2010
## [31032] 2009 2015 2016 2012 2011 2016 2016 2017 2011 2008 2015 2017 2016
## [31045] 2017 2009 2009 2011 2009 2017 2010 2009 2015 2014 2015 2015 2017
## [31058] 2017 2010 2013 2016 2017 2008 2010 2009 2011 2011 2015 2017 2017
## [31071] 2009 2010 2015 2016 2016 2013 2016 2016 2016 2010 2012 2017 2017
## [31084] 2008 2012 2015 2016 2017 2009 2008 2015 2014 2016 2012 2015 2007
## [31097] 2017 2009 2016 2017 2011 2010 2013 2015 2017 2007 2009 2016 2015
## [31110] 2008 2017 2013 2017 2009 2011 2015 2015 2010 2017 2010 2011 2016
## [31123] 2009 2009 2015 2016 2016 2007 2011 2016 2007 2007 2011 2016 2011
## [31136] 2015 2014 2015 2013 2015 2016 2015 2016 2013 2014 2014 2015 2015
## [31149] 2017 2008 2016 2016 2011 2011 2012 2016 2015 2017 2015 2010 2008
## [31162] 2008 2008 2014 2016 2015 2008 2017 2016 2015 2017 2012 2010 2008
## [31175] 2010 2010 2017 2017 2012 2009 2008 2014 2015 2015 2015 2015 2016
## [31188] 2008 2016 2016 2017 2017 2016 2012 2007 2011 2015 2016 2016 2007
## [31201] 2008 2015 2015 2009 2011 2008 2008 2013 2016 2015 2009 2016 2017
## [31214] 2010 2017 2011 2016 2016 2009 2011 2016 2016 2017 2017 2011 2017
## [31227] 2008 2011 2013 2015 2015 2016 2016 2016 2015 2009 2008 2016 2008
## [31240] 2008 2008 2016 2011 2012 2008 2011 2011 2017 2017 2015 2011 2012
## [31253] 2014 2015 2016 2016 2007 2016 2015 2017 2017 2011 2012 2014 2017
## [31266] 2009 2008 2012 2013 2015 2014 2015 2015 2017 2016 2017 2011 2007
## [31279] 2008 2009 2011 2016 2017 2012 2017 2015 2016 2017 2016 2016 2017
## [31292] 2009 2017 2017 2009 2016 2015 2007 2008 2014 2017 2012 2010 2015
## [31305] 2016 2016 2016 2015 2015 2016 2015 2016 2017 2012 2015 2017 2016
## [31318] 2016 2017 2008 2008 2010 2013 2015 2017 2013 2016 2017 2012 2015
## [31331] 2017 2017 2009 2012 2015 2017 2012 2014 2015 2007 2013 2015 2017
## [31344] 2017 2017 2017 2007 2013 2016 2016 2013 2013 2013 2014 2014 2015
## [31357] 2009 2011 2009 2008 2016 2008 2007 2015 2015 2016 2009 2009 2015
## [31370] 2016 2016 2016 2009 2009 2009 2009 2007 2014 2015 2017 2012 2012
## [31383] 2015 2017 2011 2016 2015 2015 2017 2011 2017 2010 2010 2007 2015
## [31396] 2016 2017 2009 2014 2015 2017 2017 2009 2007 2011 2015 2008 2013
## [31409] 2012 2013 2017 2008 2008 2017 2017 2009 2015 2015 2016 2015 2014
## [31422] 2015 2016 2016 2010 2015 2017 2008 2016 2017 2017 2017 2011 2013
## [31435] 2016 2007 2014 2016 2016 2007 2012 2016 2007 2010 2015 2016 2017
## [31448] 2012 2016 2017 2017 2009 2008 2015 2016 2016 2015 2008 2010 2015
## [31461] 2016 2017 2007 2016 2017 2016 2011 2016 2017 2007 2013 2015 2016
## [31474] 2016 2016 2017 2014 2016 2010 2015 2015 2016 2017 2009 2017 2017
## [31487] 2012 2015 2016 2007 2013 2016 2016 2017 2008 2010 2015 2017 2015
## [31500] 2015 2010 2016 2008 2017 2011 2011 2011 2016 2010 2015 2014 2011
## [31513] 2015 2017 2011 2015 2012 2015 2016 2010 2011 2007 2012 2014 2007
## [31526] 2015 2016 2011 2011 2008 2017 2016 2015 2011 2017 2017 2010 2008
## [31539] 2008 2016 2015 2009 2015 2017 2012 2012 2008 2008 2012 2013 2014
## [31552] 2013 2015 2017 2017 2017 2012 2016 2016 2008 2014 2007 2007 2007
## [31565] 2007 2013 2014 2016 2016 2017 2008 2007 2007 2014 2016 2016 2017
## [31578] 2015 2015 2010 2009 2014 2015 2009 2016 2016 2012 2010 2014 2016
## [31591] 2008 2015 2016 2009 2017 2009 2010 2015 2009 2013 2015 2017 2007
## [31604] 2013 2015 2007 2009 2007 2014 2017 2008 2017 2017 2016 2017 2017
## [31617] 2017 2017 2011 2016 2016 2017 2016 2016 2009 2009 2015 2007 2009
## [31630] 2007 2013 2014 2016 2015 2009 2017 2016 2015 2009 2016 2017 2010
## [31643] 2009 2012 2010 2015 2017 2007 2017 2011 2010 2011 2016 2009 2015
## [31656] 2015 2015 2017 2008 2015 2015 2015 2015 2015 2017 2012 2016 2015
## [31669] 2010 2007 2009 2007 2015 2007 2016 2016 2017 2007 2016 2016 2016
## [31682] 2008 2007 2011 2009 2011 2014 2017 2017 2008 2013 2016 2011 2016
## [31695] 2016 2015 2010 2016 2011 2015 2016 2015 2011 2011 2013 2015 2017
## [31708] 2017 2017 2014 2016 2012 2016 2017 2008 2010 2007 2011 2010 2013
## [31721] 2016 2017 2017 2008 2011 2012 2010 2008 2017 2014 2015 2015 2016
## [31734] 2010 2015 2017 2009 2017 2010 2016 2017 2008 2014 2009 2007 2010
## [31747] 2015 2015 2015 2017 2014 2016 2017 2011 2008 2011 2013 2015 2017
## [31760] 2011 2013 2014 2015 2016 2017 2017 2017 2015 2017 2015 2014 2015
## [31773] 2016 2017 2011 2008 2008 2007 2007 2008 2015 2008 2012 2011 2007
## [31786] 2016 2008 2014 2015 2017 2008 2017 2017 2013 2009 2015 2014 2015
## [31799] 2015 2017 2015 2015 2017 2008 2012 2008 2013 2017 2017 2017 2017
## [31812] 2011 2011 2017 2017 2008 2007 2011 2007 2008 2007 2015 2015 2011
## [31825] 2009 2010 2007 2016 2016 2015 2007 2008 2015 2015 2016 2016 2017
## [31838] 2009 2016 2015 2007 2010 2008 2013 2014 2015 2016 2016 2009 2016
## [31851] 2016 2016 2017 2015 2016 2014 2017 2015 2016 2010 2012 2009 2016
## [31864] 2007 2014 2016 2016 2017 2008 2014 2016 2017 2017 2010 2012 2017
## [31877] 2017 2012 2011 2009 2017 2017 2011 2009 2016 2012 2015 2017 2017
## [31890] 2010 2015 2017 2008 2008 2016 2012 2009 2013 2008 2007 2015 2012
## [31903] 2009 2007 2007 2013 2016 2010 2015 2016 2015 2017 2015 2011 2016
## [31916] 2015 2010 2014 2017 2016 2017 2014 2016 2017 2013 2015 2008 2016
## [31929] 2015 2016 2010 2016 2016 2007 2016 2017 2008 2016 2010 2010 2015
## [31942] 2015 2010 2014 2007 2016 2017 2010 2009 2012 2010 2011 2017 2010
## [31955] 2015 2015 2016 2017 2017 2008 2007 2016 2007 2012 2012 2010 2015
## [31968] 2016 2017 2017 2008 2008 2010 2014 2017 2009 2017 2009 2013 2016
## [31981] 2017 2007 2015 2008 2015 2016 2009 2008 2013 2017 2013 2009 2007
## [31994] 2011 2011 2016 2017 2017 2015 2016 2015 2017 2010 2011 2010 2010
## [32007] 2016 2016 2017 2009 2017 2017 2009 2011 2016 2015 2017 2012 2011
## [32020] 2016 2017 2017 2017 2017 2013 2013 2017 2017 2011 2008 2010 2013
## [32033] 2013 2015 2008 2011 2015 2016 2009 2008 2016 2015 2009 2017 2014
## [32046] 2015 2015 2017 2008 2014 2014 2017 2010 2013 2016 2017 2007 2013
## [32059] 2014 2012 2015 2015 2016 2011 2014 2016 2015 2016 2017 2010 2009
## [32072] 2011 2007 2013 2016 2016 2015 2009 2009 2009 2007 2014 2016 2017
## [32085] 2011 2011 2008 2015 2010 2011 2007 2008 2015 2017 2017 2011 2015
## [32098] 2016 2017 2009 2015 2016 2016 2016 2012 2016 2007 2013 2015 2017
## [32111] 2016 2015 2011 2016 2010 2014 2016 2017 2009 2015 2015 2015 2016
## [32124] 2017 2016 2016 2015 2009 2016 2016 2011 2008 2016 2012 2007 2014
## [32137] 2016 2017 2017 2008 2015 2012 2016 2016 2016 2008 2011 2014 2009
## [32150] 2017 2010 2015 2016 2016 2017 2011 2011 2010 2007 2016 2017 2016
## [32163] 2017 2011 2017 2007 2010 2012 2013 2010 2014 2015 2015 2016 2009
## [32176] 2011 2009 2016 2017 2017 2012 2015 2014 2016 2014 2014 2015 2015
## [32189] 2016 2017 2010 2015 2010 2012 2011 2015 2015 2016 2012 2007 2011
## [32202] 2014 2015 2012 2009 2015 2009 2014 2016 2007 2011 2015 2016 2017
## [32215] 2017 2017 2012 2015 2017 2017 2015 2015 2015 2016 2016 2007 2016
## [32228] 2017 2016 2010 2015 2008 2008 2013 2015 2017 2008 2009 2014 2015
## [32241] 2011 2015 2011 2009 2011 2014 2016 2016 2010 2014 2013 2017 2014
## [32254] 2016 2016 2017 2017 2014 2017 2017 2007 2014 2016 2009 2015 2017
## [32267] 2017 2011 2016 2015 2010 2015 2007 2010 2012 2011 2016 2014 2015
## [32280] 2016 2014 2016 2016 2015 2015 2016 2015 2015 2015 2017 2017 2007
## [32293] 2012 2017 2013 2016 2017 2012 2015 2017 2017 2010 2016 2016 2015
## [32306] 2017 2017 2015 2015 2017 2016 2009 2007 2009 2017 2007 2014 2015
## [32319] 2015 2017 2016 2010 2015 2015 2015 2017 2011 2007 2015 2017 2010
## [32332] 2016 2017 2008 2017 2007 2017 2011 2007 2017 2010 2008 2015 2009
## [32345] 2016 2016 2012 2009 2007 2017 2015 2017 2010 2008 2015 2016 2017
## [32358] 2011 2015 2017 2007 2009 2009 2011 2014 2015 2016 2012 2011 2014
## [32371] 2016 2017 2017 2009 2015 2017 2017 2008 2008 2009 2015 2017 2016
## [32384] 2015 2015 2016 2009 2008 2010 2013 2017 2011 2016 2016 2012 2015
## [32397] 2015 2016 2015 2012 2016 2014 2010 2011 2017 2017 2011 2007 2017
## [32410] 2017 2017 2008 2011 2017 2009 2012 2013 2015 2007 2011 2017 2017
## [32423] 2017 2009 2011 2013 2014 2016 2014 2014 2015 2016 2017 2013 2015
## [32436] 2011 2015 2016 2010 2014 2017 2013 2016 2015 2015 2010 2013 2014
## [32449] 2016 2017 2017 2016 2016 2016 2010 2012 2015 2016 2016 2017 2007
## [32462] 2008 2014 2015 2017 2007 2008 2016 2016 2007 2007 2008 2012 2015
## [32475] 2009 2015 2015 2017 2015 2008 2013 2015 2010 2012 2011 2015 2016
## [32488] 2017 2009 2016 2016 2017 2015 2016 2007 2015 2016 2008 2017 2010
## [32501] 2013 2014 2016 2016 2007 2009 2007 2008 2014 2016 2015 2017 2016
## [32514] 2017 2017 2011 2010 2008 2015 2012 2011 2008 2016 2015 2009 2015
## [32527] 2017 2015 2015 2016 2016 2011 2017 2007 2010 2010 2014 2017 2017
## [32540] 2009 2015 2008 2008 2016 2016 2017 2010 2007 2011 2008 2015 2015
## [32553] 2017 2007 2007 2016 2015 2016 2016 2016 2017 2007 2015 2015 2017
## [32566] 2016 2010 2014 2016 2016 2016 2009 2016 2017 2012 2016 2016 2017
## [32579] 2011 2016 2015 2008 2013 2014 2017 2016 2017 2011 2017 2014 2015
## [32592] 2008 2014 2017 2015 2016 2016 2013 2015 2015 2017 2009 2012 2007
## [32605] 2017 2017 2017 2017 2015 2015 2013 2015 2016 2017 2008 2013 2013
## [32618] 2017 2009 2011 2016 2016 2010 2017 2010 2011 2016 2017 2015 2017
## [32631] 2015 2017 2012 2012 2014 2015 2017 2017 2011 2017 2011 2011 2009
## [32644] 2015 2015 2016 2014 2016 2016 2017 2015 2017 2009 2013 2014 2016
## [32657] 2015 2015 2015 2014 2014 2016 2011 2015 2010 2007 2015 2011 2017
## [32670] 2015 2016 2017 2017 2017 2015 2016 2016 2017 2016 2017 2011 2015
## [32683] 2016 2016 2011 2015 2015 2013 2014 2017 2007 2009 2007 2016 2016
## [32696] 2016 2016 2016 2010 2007 2015 2016 2008 2011 2016 2016 2016 2017
## [32709] 2009 2007 2008 2011 2015 2017 2008 2014 2015 2015 2008 2007 2008
## [32722] 2012 2010 2011 2016 2009 2016 2011 2012 2015 2013 2017 2008 2011
## [32735] 2016 2007 2011 2016 2016 2015 2010 2014 2009 2007 2017 2012 2011
## [32748] 2017 2017 2007 2009 2017 2017 2017 2010 2009 2008 2007 2017 2011
## [32761] 2015 2016 2016 2017 2013 2012 2011 2017 2007 2017 2015 2016 2010
## [32774] 2015 2012 2011 2013 2010 2009 2008 2015 2008 2011 2014 2015 2009
## [32787] 2009 2010 2017 2015 2015 2016 2015 2015 2013 2015 2013 2015 2015
## [32800] 2011 2007 2011 2015 2012 2016 2015 2016 2017 2017 2016 2017 2017
## [32813] 2008 2013 2007 2014 2007 2017 2011 2017 2017 2012 2015 2008 2009
## [32826] 2013 2017 2017 2012 2016 2009 2013 2016 2009 2008 2009 2015 2016
## [32839] 2008 2007 2016 2008 2013 2016 2012 2017 2015 2016 2008 2009 2014
## [32852] 2015 2016 2012 2014 2016 2016 2017 2009 2015 2016 2015 2017 2013
## [32865] 2017 2017 2008 2012 2016 2007 2011 2007 2016 2015 2011 2012 2013
## [32878] 2009 2016 2008 2009 2016 2015 2017 2015 2017 2017 2017 2009 2017
## [32891] 2011 2016 2011 2015 2017 2017 2012 2015 2016 2015 2017 2017 2010
## [32904] 2007 2015 2015 2015 2015 2007 2015 2016 2015 2015 2016 2015 2015
## [32917] 2017 2014 2016 2016 2009 2014 2017 2007 2008 2017 2017 2016 2007
## [32930] 2008 2016 2017 2012 2009 2007 2016 2017 2015 2016 2016 2012 2015
## [32943] 2016 2012 2011 2007 2007 2009 2015 2016 2017 2017 2017 2012 2013
## [32956] 2007 2013 2014 2007 2016 2017 2012 2016 2016 2017 2010 2016 2017
## [32969] 2013 2016 2008 2016 2009 2016 2017 2017 2009 2013 2015 2016 2017
## [32982] 2008 2013 2014 2015 2008 2007 2013 2016 2015 2007 2015 2017 2017
## [32995] 2009 2015 2015 2016 2015 2007 2009 2015 2016 2011 2012 2010 2015
## [33008] 2017 2012 2015 2015 2015 2015 2016 2016 2014 2011 2013 2015 2010
## [33021] 2013 2016 2017 2007 2015 2016 2015 2016 2011 2014 2016 2015 2017
## [33034] 2008 2007 2010 2016 2016 2011 2015 2017 2015 2011 2017 2008 2011
## [33047] 2015 2016 2016 2017 2016 2016 2017 2012 2013 2012 2015 2009 2012
## [33060] 2015 2016 2011 2014 2017 2010 2014 2016 2015 2015 2015 2015 2016
## [33073] 2016 2016 2015 2009 2010 2007 2015 2017 2009 2008 2012 2014 2017
## [33086] 2010 2008 2009 2010 2013 2016 2016 2017 2008 2014 2017 2016 2015
## [33099] 2009 2013 2016 2009 2007 2010 2015 2015 2017 2012 2014 2015 2017
## [33112] 2011 2009 2011 2017 2015 2016 2016 2008 2007 2015 2007 2017 2010
## [33125] 2008 2016 2017 2017 2017 2015 2017 2012 2016 2012 2016 2007 2012
## [33138] 2008 2017 2007 2015 2015 2017 2017 2010 2007 2012 2010 2015 2016
## [33151] 2017 2013 2017 2017 2017 2010 2016 2008 2008 2013 2016 2017 2007
## [33164] 2012 2010 2016 2017 2017 2012 2007 2009 2007 2015 2016 2009 2015
## [33177] 2012 2014 2015 2015 2015 2017 2011 2014 2016 2017 2017 2017 2012
## [33190] 2008 2016 2013 2013 2017 2011 2011 2010 2013 2016 2016 2015 2015
## [33203] 2017 2010 2012 2015 2015 2017 2015 2017 2011 2013 2011 2015 2016
## [33216] 2017 2017 2017 2017 2016 2017 2007 2010 2009 2009 2016 2012 2008
## [33229] 2012 2016 2016 2016 2017 2017 2010 2013 2015 2016 2013 2008 2007
## [33242] 2010 2016 2016 2015 2015 2017 2011 2011 2015 2016 2008 2016 2017
## [33255] 2011 2010 2007 2015 2016 2011 2010 2015 2017 2015 2016 2017 2015
## [33268] 2008 2009 2015 2007 2009 2017 2008 2010 2016 2016 2011 2009 2015
## [33281] 2016 2008 2009 2014 2011 2017 2011 2015 2015 2017 2017 2011 2015
## [33294] 2015 2008 2016 2012 2009 2015 2015 2007 2015 2017 2017 2014 2015
## [33307] 2016 2016 2015 2016 2017 2016 2011 2013 2017 2013 2017 2017 2009
## [33320] 2016 2012 2015 2015 2015 2016 2009 2010 2009 2013 2013 2014 2015
## [33333] 2016 2016 2017 2015 2017 2012 2010 2015 2017 2008 2016 2017 2013
## [33346] 2015 2017 2012 2014 2015 2015 2016 2016 2008 2012 2010 2013 2016
## [33359] 2017 2007 2016 2012 2015 2017 2014 2015 2017 2017 2017 2011 2013
## [33372] 2016 2008 2008 2011 2008 2016 2009 2016 2016 2017 2012 2016 2017
## [33385] 2007 2015 2015 2010 2015 2016 2016 2016 2017 2007 2014 2015 2014
## [33398] 2017 2017 2017 2015 2017 2012 2015 2016 2010 2015 2016 2010 2015
## [33411] 2016 2017 2017 2008 2016 2016 2015 2012 2010 2015 2016 2010 2013
## [33424] 2016 2010 2008 2016 2017 2017 2010 2015 2012 2012 2015 2017 2012
## [33437] 2008 2015 2015 2017 2008 2011 2009 2007 2007 2011 2008 2014 2014
## [33450] 2016 2012 2011 2007 2009 2007 2010 2009 2007 2016 2017 2008 2015
## [33463] 2007 2017 2008 2015 2015 2016 2016 2016 2011 2015 2017 2012 2009
## [33476] 2016 2016 2012 2012 2015 2016 2015 2010 2012 2016 2017 2012 2014
## [33489] 2017 2015 2016 2015 2017 2015 2017 2007 2014 2017 2010 2015 2012
## [33502] 2011 2017 2017 2009 2008 2014 2016 2017 2012 2007 2017 2017 2008
## [33515] 2014 2012 2014 2017 2016 2014 2017 2008 2008 2009 2007 2007 2009
## [33528] 2014 2016 2017 2017 2008 2011 2012 2014 2008 2012 2012 2007 2016
## [33541] 2013 2016 2016 2013 2015 2015 2017 2013 2015 2015 2017 2016 2017
## [33554] 2007 2015 2008 2016 2017 2017 2013 2015 2017 2008 2011 2011 2014
## [33567] 2017 2007 2010 2016 2017 2015 2015 2016 2009 2016 2016 2007 2011
## [33580] 2015 2015 2015 2016 2015 2008 2007 2011 2007 2007 2012 2015 2015
## [33593] 2015 2017 2007 2010 2010 2012 2009 2012 2014 2017 2012 2011 2008
## [33606] 2007 2015 2016 2015 2016 2015 2015 2017 2016 2009 2017 2008 2007
## [33619] 2014 2016 2016 2016 2016 2016 2016 2009 2016 2010 2015 2017 2017
## [33632] 2016 2011 2017 2015 2016 2017 2012 2007 2011 2014 2016 2015 2013
## [33645] 2011 2015 2015 2016 2017 2008 2009 2015 2016 2008 2017 2016 2017
## [33658] 2011 2009 2009 2014 2016 2016 2016 2017 2008 2015 2016 2016 2014
## [33671] 2010 2014 2016 2011 2014 2011 2009 2015 2007 2009 2009 2015 2017
## [33684] 2015 2011 2007 2014 2017 2015 2017 2010 2011 2010 2009 2014 2015
## [33697] 2016 2009 2007 2009 2014 2015 2016 2017 2011 2013 2016 2017 2011
## [33710] 2013 2015 2016 2016 2017 2015 2015 2015 2015 2016 2008 2010 2009
## [33723] 2013 2017 2016 2015 2015 2009 2015 2017 2015 2009 2014 2014 2015
## [33736] 2016 2017 2011 2010 2016 2007 2016 2016 2016 2017 2008 2010 2013
## [33749] 2008 2016 2016 2016 2017 2010 2010 2014 2016 2016 2016 2013 2015
## [33762] 2015 2017 2017 2009 2016 2016 2013 2017 2008 2014 2014 2017 2017
## [33775] 2013 2015 2017 2010 2008 2009 2010 2017 2012 2009 2015 2017 2007
## [33788] 2016 2016 2013 2015 2016 2010 2015 2015 2016 2016 2017 2010 2008
## [33801] 2009 2014 2007 2009 2016 2015 2016 2011 2008 2016 2016 2016 2008
## [33814] 2017 2007 2015 2017 2007 2015 2015 2013 2017 2009 2013 2014 2015
## [33827] 2016 2009 2010 2014 2017 2012 2015 2016 2009 2011 2017 2016 2017
## [33840] 2017 2009 2014 2015 2016 2017 2017 2017 2011 2014 2016 2015 2017
## [33853] 2013 2016 2009 2015 2011 2013 2016 2008 2016 2015 2015 2016 2017
## [33866] 2012 2010 2015 2007 2011 2017 2011 2009 2007 2014 2015 2017 2013
## [33879] 2017 2014 2017 2017 2017 2016 2010 2008 2016 2016 2017 2012 2011
## [33892] 2012 2014 2015 2011 2013 2015 2017 2009 2010 2016 2017 2017 2015
## [33905] 2017 2012 2009 2015 2008 2011 2012 2013 2017 2012 2015 2016 2015
## [33918] 2017 2011 2014 2015 2016 2017 2008 2016 2012 2013 2013 2015 2017
## [33931] 2007 2008 2009 2015 2016 2016 2016 2016 2017 2010 2009 2016 2015
## [33944] 2016 2015 2016 2017 2015 2017 2012 2017 2009 2016 2017 2012 2015
## [33957] 2016 2008 2016 2017 2017 2008 2015 2017 2010 2014 2016 2010 2009
## [33970] 2007 2017 2011 2015 2017 2010 2015 2014 2011 2010 2017 2017 2017
## [33983] 2014 2015 2017 2015 2016 2015 2007 2011 2016 2016 2009 2011 2015
## [33996] 2017 2007 2015 2015 2015 2008 2012 2008 2017 2007 2017 2017 2012
## [34009] 2010 2015 2015 2017 2017 2010 2015 2010 2017 2009 2015 2013 2015
## [34022] 2008 2015 2015 2008 2016 2017 2011 2010 2013 2017 2017 2017 2015
## [34035] 2016 2011 2015 2016 2015 2016 2017 2017 2007 2015 2016 2016 2017
## [34048] 2015 2016 2014 2011 2016 2015 2017 2016 2017 2017 2008 2016 2017
## [34061] 2011 2016 2017 2009 2015 2016 2014 2015 2007 2013 2008 2017 2008
## [34074] 2013 2017 2015 2016 2015 2008 2008 2010 2016 2011 2016 2011 2011
## [34087] 2017 2007 2010 2017 2011 2007 2007 2016 2007 2015 2017 2009 2010
## [34100] 2008 2010 2016 2017 2010 2012 2016 2017 2012 2015 2016 2017 2008
## [34113] 2016 2015 2016 2016 2010 2008 2014 2011 2011 2015 2015 2017 2017
## [34126] 2014 2017 2012 2012 2008 2013 2016 2012 2010 2015 2016 2017 2017
## [34139] 2008 2007 2014 2016 2015 2008 2016 2017 2010 2016 2007 2013 2015
## [34152] 2017 2009 2009 2013 2015 2016 2012 2016 2016 2011 2011 2015 2015
## [34165] 2007 2012 2015 2016 2017 2017 2012 2011 2015 2017 2017 2016 2015
## [34178] 2017 2010 2009 2016 2010 2015 2007 2014 2015 2015 2016 2017 2010
## [34191] 2013 2015 2017 2010 2013 2016 2015 2017 2017 2017 2009 2013 2013
## [34204] 2017 2011 2011 2009 2010 2017 2017 2015 2011 2016 2017 2007 2015
## [34217] 2015 2013 2014 2016 2007 2016 2008 2013 2015 2009 2015 2012 2015
## [34230] 2011 2014 2015 2017 2012 2017 2007 2010 2016 2009 2011 2008 2010
## [34243] 2015 2015 2016 2008 2007 2008 2016 2017 2008 2008 2015 2007 2009
## [34256] 2008 2013 2011 2016 2010 2007 2017 2009 2013 2017 2017 2007 2008
## [34269] 2015 2017 2008 2010 2012 2015 2011 2015 2016 2015 2017 2015 2015
## [34282] 2016 2016 2008 2010 2015 2015 2012 2013 2016 2007 2016 2015 2007
## [34295] 2016 2017 2007 2008 2011 2010 2007 2012 2015 2016 2016 2010 2010
## [34308] 2015 2017 2017 2009 2015 2015 2017 2009 2014 2016 2017 2010 2017
## [34321] 2012 2011 2016 2017 2011 2009 2007 2013 2016 2017 2007 2010 2009
## [34334] 2012 2008 2014 2014 2017 2017 2008 2007 2016 2007 2016 2007 2012
## [34347] 2016 2017 2017 2012 2014 2015 2015 2016 2017 2007 2007 2011 2007
## [34360] 2009 2012 2011 2009 2009 2014 2015 2016 2012 2015 2016 2017 2009
## [34373] 2008 2014 2017 2009 2011 2012 2013 2017 2017 2016 2016 2009 2015
## [34386] 2015 2007 2013 2016 2016 2017 2015 2016 2016 2015 2010 2016 2016
## [34399] 2017 2016 2012 2015 2017 2016 2017 2015 2015 2016 2008 2014 2015
## [34412] 2016 2017 2017 2010 2011 2015 2017 2009 2016 2016 2017 2015 2017
## [34425] 2017 2014 2016 2016 2007 2010 2016 2015 2016 2011 2008 2015 2017
## [34438] 2008 2007 2015 2010 2008 2013 2012 2008 2010 2010 2016 2017 2010
## [34451] 2016 2010 2007 2016 2017 2016 2016 2010 2015 2010 2011 2009 2014
## [34464] 2015 2015 2017 2017 2011 2014 2016 2015 2017 2008 2008 2010 2016
## [34477] 2015 2016 2011 2008 2016 2015 2016 2015 2011 2015 2017 2017 2015
## [34490] 2008 2012 2016 2016 2017 2011 2017 2017 2010 2017 2017 2010 2017
## [34503] 2007 2007 2013 2014 2008 2017 2016 2009 2012 2014 2016 2017 2013
## [34516] 2015 2008 2017 2017 2017 2007 2011 2013 2016 2016 2012 2008 2015
## [34529] 2016 2016 2017 2009 2011 2011 2010 2015 2016 2017 2007 2015 2015
## [34542] 2017 2009 2008 2010 2014 2016 2009 2010 2011 2017 2015 2011 2007
## [34555] 2008 2013 2014 2016 2016 2016 2011 2011 2009 2015 2007 2016 2016
## [34568] 2016 2011 2009 2009 2013 2013 2015 2015 2016 2017 2017 2009 2012
## [34581] 2015 2017 2017 2008 2013 2016 2014 2016 2015 2007 2008 2008 2014
## [34594] 2017 2010 2016 2007 2016 2016 2009 2016 2017 2013 2014 2016 2016
## [34607] 2017 2014 2016 2016 2017 2015 2010 2017 2010 2015 2011 2008 2014
## [34620] 2017 2010 2015 2016 2017 2015 2015 2016 2008 2017 2015 2008 2015
## [34633] 2012 2009 2010 2009 2007 2011 2016 2017 2010 2008 2013 2016 2017
## [34646] 2015 2014 2015 2017 2007 2014 2017 2017 2010 2017 2009 2008 2015
## [34659] 2016 2015 2016 2016 2008 2015 2016 2017 2007 2011 2010 2011 2011
## [34672] 2014 2017 2017 2010 2011 2009 2009 2014 2007 2017 2008 2015 2015
## [34685] 2016 2011 2016 2013 2016 2014 2007 2016 2017 2017 2015 2017 2012
## [34698] 2016 2016 2016 2011 2011 2007 2009 2016 2010 2011 2016 2017 2015
## [34711] 2015 2016 2012 2016 2011 2013 2015 2011 2013 2016 2014 2017 2011
## [34724] 2013 2015 2016 2016 2017 2015 2017 2009 2008 2007 2008 2009 2009
## [34737] 2016 2015 2015 2014 2017 2007 2012 2009 2016 2016 2017 2017 2014
## [34750] 2014 2017 2014 2016 2008 2009 2007 2009 2012 2015 2015 2008 2010
## [34763] 2010 2014 2015 2015 2008 2015 2007 2011 2016 2017 2012 2016 2007
## [34776] 2016 2017 2010 2016 2017 2013 2010 2007 2008 2016 2015 2017 2015
## [34789] 2017 2017 2015 2008 2017 2016 2011 2007 2011 2012 2013 2017 2017
## [34802] 2015 2009 2010 2017 2017 2012 2016 2017 2008 2015 2017 2010 2008
## [34815] 2009 2015 2016 2017 2008 2010 2007 2015 2017 2011 2016 2017 2017
## [34828] 2007 2014 2015 2015 2017 2013 2015 2015 2016 2017 2017 2012 2014
## [34841] 2012 2015 2015 2016 2017 2012 2014 2015 2017 2012 2015 2015 2015
## [34854] 2017 2009 2014 2014 2017 2016 2017 2009 2015 2016 2016 2017 2014
## [34867] 2011 2011 2008 2013 2017 2010 2013 2017 2007 2011 2016 2009 2015
## [34880] 2017 2017 2017 2012 2015 2016 2009 2012 2015 2017 2010 2016 2016
## [34893] 2009 2015 2016 2015 2017 2013 2017 2016 2017 2015 2016 2009 2015
## [34906] 2016 2010 2009 2011 2017 2007 2017 2009 2016 2011 2015 2015 2015
## [34919] 2017 2015 2011 2013 2016 2012 2016 2015 2016 2007 2017 2014 2017
## [34932] 2011 2012 2011 2011 2009 2014 2014 2015 2015 2016 2016 2014 2007
## [34945] 2010 2009 2008 2016 2014 2015 2016 2010 2016 2012 2015 2011 2007
## [34958] 2017 2010 2010 2017 2017 2007 2009 2015 2016 2011 2015 2008 2007
## [34971] 2016 2007 2009 2012 2013 2015 2014 2016 2010 2010 2008 2008 2015
## [34984] 2016 2009 2011 2015 2008 2016 2017 2011 2016 2012 2011 2014 2017
## [34997] 2007 2007 2007 2011 2017 2008 2007 2015 2017 2017 2008 2009 2016
## [35010] 2017 2015 2017 2011 2007 2009 2008 2016 2017 2009 2014 2014 2016
## [35023] 2012 2016 2017 2009 2008 2014 2015 2015 2016 2017 2016 2017 2017
## [35036] 2009 2015 2016 2016 2017 2017 2012 2017 2017 2009 2007 2011 2014
## [35049] 2015 2015 2016 2016 2015 2016 2017 2017 2014 2017 2009 2013 2016
## [35062] 2016 2017 2007 2016 2011 2011 2016 2017 2008 2008 2010 2017 2011
## [35075] 2011 2013 2015 2016 2015 2017 2010 2015 2015 2017 2017 2007 2015
## [35088] 2017 2008 2015 2016 2016 2015 2007 2015 2016 2012 2014 2016 2013
## [35101] 2016 2017 2017 2015 2017 2011 2011 2010 2015 2012 2008 2016 2015
## [35114] 2009 2015 2013 2017 2012 2009 2012 2012 2017 2014 2016 2007 2012
## [35127] 2010 2016 2017 2008 2014 2015 2017 2012 2016 2017 2015 2017 2015
## [35140] 2015 2016 2007 2013 2011 2009 2009 2009 2015 2016 2017 2014 2015
## [35153] 2009 2013 2016 2007 2010 2013 2014 2015 2017 2014 2016 2009 2008
## [35166] 2015 2016 2016 2016 2017 2010 2013 2012 2016 2017 2017 2008 2010
## [35179] 2015 2017 2011 2009 2014 2014 2016 2011 2015 2016 2016 2017 2009
## [35192] 2009 2009 2009 2009 2007 2014 2016 2017 2010 2015 2011 2007 2017
## [35205] 2015 2012 2009 2007 2011 2016 2013 2016 2007 2016 2017 2011 2015
## [35218] 2016 2017 2017 2007 2016 2016 2017 2010 2010 2015 2017 2012 2011
## [35231] 2015 2016 2017 2012 2014 2016 2016 2015 2015 2015 2008 2017 2007
## [35244] 2014 2017 2017 2008 2007 2011 2017 2017 2015 2014 2012 2013 2015
## [35257] 2007 2012 2016 2008 2014 2016 2011 2014 2017 2008 2014 2009 2011
## [35270] 2008 2010 2013 2016 2015 2015 2013 2015 2017 2012 2011 2007 2011
## [35283] 2015 2011 2008 2011 2007 2017 2016 2014 2017 2011 2014 2017 2011
## [35296] 2011 2014 2016 2008 2013 2014 2016 2017 2011 2009 2008 2012 2008
## [35309] 2011 2016 2017 2016 2012 2008 2015 2016 2014 2016 2009 2016 2015
## [35322] 2017 2010 2008 2016 2017 2015 2017 2015 2017 2014 2017 2017 2007
## [35335] 2017 2016 2017 2007 2015 2016 2015 2016 2015 2016 2017 2013 2015
## [35348] 2008 2015 2010 2010 2008 2008 2008 2017 2012 2013 2017 2008 2011
## [35361] 2017 2016 2016 2016 2017 2017 2007 2017 2016 2017 2009 2012 2007
## [35374] 2009 2017 2014 2015 2017 2010 2009 2016 2017 2013 2015 2016 2017
## [35387] 2010 2008 2015 2017 2008 2009 2015 2014 2015 2015 2008 2007 2015
## [35400] 2015 2015 2016 2017 2007 2014 2015 2015 2017 2017 2011 2014 2015
## [35413] 2017 2008 2017 2013 2016 2017 2007 2008 2015 2016 2008 2015 2017
## [35426] 2011 2008 2016 2012 2012 2015 2016 2016 2007 2011 2017 2008 2015
## [35439] 2017 2008 2015 2017 2017 2012 2015 2015 2015 2016 2017 2014 2015
## [35452] 2017 2017 2015 2015 2016 2012 2009 2015 2015 2009 2007 2017 2017
## [35465] 2016 2016 2017 2007 2015 2008 2012 2017 2009 2014 2015 2017 2007
## [35478] 2011 2017 2009 2012 2014 2012 2013 2014 2017 2008 2011 2012 2008
## [35491] 2010 2007 2016 2015 2017 2010 2010 2009 2011 2011 2013 2015 2017
## [35504] 2007 2016 2015 2017 2010 2016 2015 2017 2011 2015 2015 2017 2008
## [35517] 2015 2017 2016 2012 2016 2017 2016 2015 2016 2016 2016 2014 2016
## [35530] 2009 2009 2015 2015 2017 2013 2017 2017 2014 2016 2016 2017 2010
## [35543] 2008 2017 2013 2015 2015 2016 2016 2014 2016 2013 2015 2014 2015
## [35556] 2016 2014 2016 2013 2011 2008 2007 2014 2015 2015 2015 2015 2017
## [35569] 2010 2016 2015 2010 2015 2015 2009 2010 2009 2008 2007 2012 2013
## [35582] 2014 2015 2016 2012 2015 2007 2015 2016 2017 2017 2011 2015 2011
## [35595] 2015 2017 2016 2017 2008 2014 2017 2017 2011 2014 2017 2016 2017
## [35608] 2016 2016 2015 2016 2007 2015 2011 2007 2011 2014 2017 2010 2015
## [35621] 2017 2017 2015 2009 2016 2016 2007 2016 2015 2015 2012 2009 2007
## [35634] 2009 2016 2017 2016 2009 2012 2008 2015 2017 2010 2013 2016 2017
## [35647] 2017 2010 2009 2007 2015 2016 2016 2016 2013 2017 2012 2010 2009
## [35660] 2007 2008 2016 2016 2016 2011 2012 2009 2015 2015 2008 2013 2017
## [35673] 2017 2008 2013 2015 2017 2007 2013 2008 2009 2015 2015 2016 2017
## [35686] 2015 2016 2007 2007 2010 2015 2011 2009 2009 2017 2014 2016 2007
## [35699] 2007 2015 2015 2015 2015 2016 2017 2007 2016 2015 2008 2015 2015
## [35712] 2016 2015 2008 2017 2015 2017 2017 2011 2015 2014 2015 2017 2010
## [35725] 2016 2015 2015 2017 2016 2015 2010 2016 2015 2014 2016 2016 2017
## [35738] 2017 2017 2017 2017 2017 2017 2017 2011 2016 2015 2012 2011 2015
## [35751] 2015 2016 2011 2016 2007 2012 2009 2008 2016 2017 2014 2016 2015
## [35764] 2008 2007 2009 2014 2016 2016 2011 2008 2016 2017 2013 2015 2016
## [35777] 2011 2017 2011 2008 2015 2015 2008 2016 2010 2012 2017 2015 2016
## [35790] 2012 2017 2015 2011 2015 2017 2007 2014 2007 2017 2012 2015 2016
## [35803] 2017 2011 2010 2008 2015 2012 2011 2017 2016 2016 2012 2015 2015
## [35816] 2017 2015 2017 2008 2012 2015 2017 2017 2016 2015 2007 2013 2016
## [35829] 2016 2011 2009 2016 2009 2016 2015 2017 2008 2010 2009 2015 2015
## [35842] 2013 2015 2008 2015 2015 2015 2012 2016 2017 2009 2015 2012 2008
## [35855] 2008 2015 2017 2017 2015 2015 2011 2009 2013 2014 2015 2015 2016
## [35868] 2016 2008 2014 2016 2017 2009 2007 2017 2008 2011 2015 2015 2015
## [35881] 2017 2015 2010 2017 2009 2010 2011 2013 2013 2017 2007 2017 2007
## [35894] 2017 2011 2016 2017 2015 2009 2016 2016 2014 2012 2015 2016 2011
## [35907] 2014 2009 2016 2008 2009 2010 2014 2014 2015 2009 2015 2017 2012
## [35920] 2017 2008 2008 2016 2016 2008 2012 2013 2017 2007 2008 2015 2008
## [35933] 2016 2016 2016 2015 2011 2016 2009 2012 2015 2017 2016 2008 2013
## [35946] 2016 2017 2008 2007 2011 2017 2008 2009 2015 2016 2010 2015 2016
## [35959] 2015 2012 2015 2007 2010 2015 2017 2014 2015 2017 2010 2013 2016
## [35972] 2008 2009 2017 2008 2011 2011 2016 2017 2008 2015 2014 2017 2011
## [35985] 2008 2011 2014 2015 2016 2016 2016 2012 2016 2008 2016 2017 2015
## [35998] 2016 2017 2015 2017 2011 2013 2014 2016 2007 2016 2017 2015 2017
## [36011] 2011 2010 2013 2009 2016 2016 2017 2007 2016 2016 2011 2008 2011
## [36024] 2016 2007 2010 2015 2017 2017 2009 2007 2015 2016 2017 2017 2009
## [36037] 2017 2008 2017 2008 2016 2017 2017 2009 2008 2015 2016 2016 2016
## [36050] 2016 2017 2017 2014 2015 2015 2012 2010 2007 2015 2016 2016 2017
## [36063] 2017 2017 2017 2017 2008 2007 2012 2007 2015 2016 2017 2014 2015
## [36076] 2016 2008 2014 2017 2010 2010 2014 2014 2016 2015 2017 2017 2014
## [36089] 2015 2016 2017 2015 2016 2017 2017 2015 2007 2015 2015 2015 2016
## [36102] 2016 2014 2015 2008 2008 2013 2013 2017 2008 2008 2011 2015 2015
## [36115] 2016 2016 2017 2011 2015 2017 2016 2017 2012 2016 2016 2017 2007
## [36128] 2012 2007 2015 2016 2014 2017 2014 2015 2016 2012 2015 2015 2016
## [36141] 2016 2017 2012 2013 2017 2007 2016 2009 2015 2017 2015 2016 2016
## [36154] 2012 2016 2008 2008 2007 2016 2017 2017 2008 2011 2017 2009 2010
## [36167] 2016 2015 2014 2016 2010 2015 2017 2012 2017 2017 2014 2016 2016
## [36180] 2017 2010 2007 2008 2007 2016 2016 2017 2013 2017 2008 2009 2016
## [36193] 2017 2015 2008 2016 2016 2017 2008 2013 2015 2010 2017 2014 2015
## [36206] 2015 2007 2010 2016 2015 2016 2011 2011 2015 2010 2014 2016 2017
## [36219] 2014 2015 2010 2015 2015 2007 2013 2014 2015 2016 2017 2010 2011
## [36232] 2016 2009 2015 2015 2015 2017 2007 2014 2016 2010 2017 2015 2011
## [36245] 2012 2014 2016 2016 2017 2014 2016 2016 2017 2016 2017 2017 2007
## [36258] 2011 2013 2015 2017 2009 2012 2012 2010 2008 2007 2015 2015 2011
## [36271] 2011 2010 2017 2017 2007 2009 2017 2017 2014 2008 2008 2014 2014
## [36284] 2015 2016 2008 2015 2017 2015 2013 2016 2007 2017 2017 2011 2008
## [36297] 2015 2017 2010 2007 2015 2008 2012 2016 2016 2012 2007 2013 2016
## [36310] 2017 2017 2017 2016 2017 2016 2016 2017 2007 2015 2008 2015 2011
## [36323] 2008 2017 2017 2015 2016 2016 2007 2010 2013 2017 2009 2017 2015
## [36336] 2016 2008 2016 2017 2007 2007 2010 2014 2017 2017 2007 2016 2017
## [36349] 2012 2011 2014 2015 2009 2012 2012 2015 2015 2016 2016 2010 2015
## [36362] 2017 2012 2017 2017 2012 2015 2017 2010 2007 2017 2015 2015 2015
## [36375] 2008 2016 2017 2011 2012 2008 2012 2013 2015 2015 2011 2012 2010
## [36388] 2015 2016 2012 2015 2016 2015 2010 2011 2010 2015 2015 2017 2012
## [36401] 2009 2017 2007 2014 2016 2016 2017 2016 2016 2017 2012 2008 2010
## [36414] 2017 2016 2017 2017 2010 2013 2015 2016 2016 2017 2017 2015 2017
## [36427] 2008 2014 2015 2016 2015 2015 2007 2016 2016 2009 2014 2015 2015
## [36440] 2012 2016 2011 2016 2017 2008 2012 2015 2011 2010 2014 2016 2007
## [36453] 2016 2012 2009 2008 2014 2015 2015 2016 2017 2015 2017 2010 2015
## [36466] 2017 2010 2012 2011 2008 2015 2016 2017 2012 2011 2010 2016 2017
## [36479] 2009 2010 2017 2015 2017 2017 2017 2009 2017 2017 2008 2008 2016
## [36492] 2016 2015 2007 2014 2016 2015 2016 2014 2016 2015 2016 2014 2015
## [36505] 2017 2015 2009 2015 2015 2016 2017 2010 2013 2015 2016 2009 2016
## [36518] 2016 2017 2009 2016 2017 2012 2011 2010 2017 2012 2012 2011 2009
## [36531] 2010 2014 2016 2010 2014 2017 2010 2016 2016 2016 2017 2011 2015
## [36544] 2015 2016 2016 2015 2008 2007 2009 2017 2012 2013 2016 2016 2017
## [36557] 2015 2012 2015 2016 2011 2013 2016 2013 2015 2017 2008 2007 2013
## [36570] 2014 2009 2007 2017 2012 2014 2015 2017 2008 2015 2017 2009 2011
## [36583] 2007 2014 2015 2015 2017 2017 2017 2014 2016 2015 2017 2015 2014
## [36596] 2009 2013 2009 2014 2016 2014 2016 2009 2009 2013 2016 2017 2016
## [36609] 2017 2009 2008 2013 2016 2017 2016 2016 2007 2015 2010 2016 2016
## [36622] 2015 2016 2010 2016 2017 2017 2015 2012 2016 2016 2007 2016 2007
## [36635] 2012 2016 2017 2010 2014 2016 2017 2013 2008 2017 2007 2014 2014
## [36648] 2017 2016 2017 2017 2010 2013 2017 2010 2011 2014 2015 2015 2016
## [36661] 2017 2012 2013 2014 2009 2015 2016 2017 2017 2007 2015 2010 2016
## [36674] 2015 2016 2010 2016 2011 2010 2014 2015 2015 2017 2008 2012 2015
## [36687] 2015 2014 2016 2017 2010 2013 2016 2015 2010 2016 2007 2012 2011
## [36700] 2010 2012 2014 2015 2015 2012 2012 2009 2009 2011 2016 2008 2010
## [36713] 2012 2010 2015 2016 2017 2007 2014 2007 2008 2016 2016 2017 2012
## [36726] 2017 2012 2009 2007 2017 2009 2011 2016 2016 2015 2016 2008 2010
## [36739] 2008 2013 2016 2016 2016 2015 2007 2013 2013 2016 2016 2017 2017
## [36752] 2010 2010 2013 2017 2014 2015 2017 2013 2016 2017 2017 2017 2014
## [36765] 2014 2015 2016 2016 2015 2016 2010 2015 2010 2016 2017 2010 2013
## [36778] 2016 2016 2009 2015 2010 2008 2014 2015 2015 2017 2011 2017 2009
## [36791] 2011 2011 2015 2015 2016 2015 2015 2017 2015 2012 2015 2016 2008
## [36804] 2008 2015 2016 2016 2012 2014 2015 2016 2015 2015 2016 2013 2016
## [36817] 2017 2012 2007 2008 2015 2017 2012 2017 2015 2014 2016 2016 2017
## [36830] 2015 2017 2009 2008 2016 2008 2007 2010 2015 2017 2015 2014 2016
## [36843] 2016 2016 2017 2007 2011 2015 2017 2017 2011 2017 2017 2016 2012
## [36856] 2012 2008 2016 2016 2016 2014 2012 2011 2012 2017 2015 2009 2016
## [36869] 2015 2009 2013 2017 2015 2009 2010 2016 2017 2010 2017 2007 2016
## [36882] 2016 2015 2015 2015 2016 2016 2017 2014 2016 2011 2015 2007 2013
## [36895] 2014 2015 2015 2015 2016 2009 2013 2015 2017 2017 2012 2015 2017
## [36908] 2008 2008 2009 2017 2017 2011 2012 2016 2007 2016 2013 2013 2009
## [36921] 2008 2016 2007 2008 2008 2016 2016 2017 2008 2010 2012 2016 2007
## [36934] 2007 2015 2015 2016 2017 2010 2009 2015 2016 2017 2016 2017 2008
## [36947] 2015 2012 2013 2016 2016 2013 2017 2012 2012 2016 2016 2011 2010
## [36960] 2009 2016 2017 2014 2015 2007 2013 2017 2017 2011 2008 2017 2016
## [36973] 2016 2007 2017 2011 2011 2017 2009 2008 2015 2009 2016 2017 2017
## [36986] 2008 2008 2007 2014 2016 2007 2007 2014 2015 2016 2016 2008 2015
## [36999] 2016 2016 2016 2016 2017 2017 2012 2011 2013 2015 2017 2016 2015
## [37012] 2015 2015 2017 2007 2016 2015 2011 2017 2017 2008 2012 2015 2017
## [37025] 2017 2009 2017 2010 2010 2016 2007 2010 2016 2007 2010 2009 2007
## [37038] 2010 2015 2017 2015 2017 2010 2015 2015 2012 2009 2015 2016 2017
## [37051] 2010 2009 2013 2008 2016 2017 2011 2015 2016 2010 2010 2017 2014
## [37064] 2017 2011 2015 2017 2010 2009 2009 2013 2014 2015 2015 2016 2010
## [37077] 2007 2010 2014 2015 2009 2015 2011 2011 2017 2008 2014 2014 2016
## [37090] 2009 2008 2015 2017 2007 2015 2016 2016 2016 2017 2008 2016 2014
## [37103] 2016 2015 2007 2016 2016 2015 2009 2012 2013 2017 2009 2009 2016
## [37116] 2016 2015 2016 2015 2015 2017 2015 2013 2009 2015 2016 2017 2013
## [37129] 2011 2013 2015 2010 2012 2015 2016 2016 2015 2016 2010 2013 2013
## [37142] 2015 2017 2008 2016 2009 2017 2011 2012 2012 2008 2017 2016 2012
## [37155] 2016 2016 2011 2007 2015 2013 2013 2017 2015 2015 2008 2015 2016
## [37168] 2015 2012 2015 2010 2015 2012 2010 2008 2014 2015 2016 2016 2016
## [37181] 2015 2016 2016 2015 2017 2016 2010 2017 2011 2014 2011 2011 2013
## [37194] 2015 2017 2013 2016 2016 2010 2016 2011 2011 2013 2014 2015 2007
## [37207] 2014 2016 2017 2011 2011 2010 2015 2017 2008 2011 2009 2016 2017
## [37220] 2011 2017 2017 2008 2014 2014 2015 2017 2017 2008 2015 2015 2008
## [37233] 2011 2011 2015 2017 2008 2014 2012 2015 2016 2009 2010 2009 2009
## [37246] 2013 2013 2016 2007 2014 2015 2007 2015 2015 2014 2015 2017 2017
## [37259] 2008 2013 2017 2010 2015 2008 2007 2015 2016 2013 2016 2011 2007
## [37272] 2009 2010 2017 2008 2009 2010 2013 2015 2015 2007 2009 2012 2007
## [37285] 2011 2007 2016 2016 2009 2013 2010 2016 2017 2011 2016 2008 2010
## [37298] 2015 2016 2013 2013 2014 2015 2016 2011 2013 2010 2012 2016 2017
## [37311] 2015 2009 2011 2010 2016 2010 2010 2016 2011 2013 2017 2017 2008
## [37324] 2016 2007 2007 2015 2011 2011 2007 2010 2012 2008 2007 2012 2013
## [37337] 2007 2012 2007 2011 2011 2012 2013 2007 2009 2011 2012 2016 2015
## [37350] 2008 2008 2016 2014 2011 2016 2017 2014 2012 2016 2007 2017 2009
## [37363] 2012 2015 2016 2017 2008 2012 2011 2007 2017 2017 2011 2016 2017
## [37376] 2012 2007 2017 2015 2011 2014 2016 2015 2015 2010 2014 2007 2011
## [37389] 2009 2017 2016 2008 2016 2017 2014 2011 2013 2017 2016 2016 2017
## [37402] 2016 2007 2010 2009 2015 2015 2016 2016 2015 2016 2016 2017 2012
## [37415] 2009 2011 2012 2013 2013 2013 2008 2015 2017 2017 2007 2013 2008
## [37428] 2009 2016 2017 2014 2015 2008 2008 2010 2013 2015 2017 2015 2016
## [37441] 2015 2009 2009 2014 2014 2015 2017 2017 2012 2010 2017 2011 2015
## [37454] 2015 2008 2015 2016 2017 2008 2015 2009 2014 2016 2015 2015 2012
## [37467] 2007 2017 2010 2014 2016 2016 2017 2007 2012 2016 2007 2015 2017
## [37480] 2011 2016 2011 2016 2016 2007 2015 2015 2013 2015 2010 2017 2010
## [37493] 2009 2008 2014 2015 2016 2009 2009 2015 2016 2017 2007 2017 2011
## [37506] 2007 2011 2015 2017 2010 2012 2015 2016 2016 2015 2015 2017 2017
## [37519] 2009 2013 2015 2016 2016 2016 2007 2007 2015 2017 2017 2017 2017
## [37532] 2015 2009 2013 2017 2009 2007 2010 2009 2009 2016 2015 2017 2009
## [37545] 2012 2008 2015 2015 2017 2010 2007 2015 2011 2011 2015 2015 2008
## [37558] 2010 2016 2008 2017 2017 2013 2016 2016 2017 2017 2014 2016 2017
## [37571] 2008 2017 2017 2017 2017 2015 2017 2015 2009 2016 2017 2009 2012
## [37584] 2015 2016 2016 2017 2012 2007 2013 2015 2016 2010 2009 2017 2016
## [37597] 2017 2008 2010 2009 2016 2017 2017 2017 2007 2010 2015 2007 2017
## [37610] 2011 2008 2012 2016 2015 2017 2011 2007 2014 2016 2009 2016 2016
## [37623] 2010 2009 2014 2015 2017 2015 2015 2017 2007 2016 2016 2007 2011
## [37636] 2007 2013 2007 2017 2017 2012 2016 2011 2007 2014 2017 2009 2015
## [37649] 2016 2015 2008 2012 2009 2010 2008 2017 2011 2012 2012 2015 2008
## [37662] 2007 2016 2016 2010 2015 2007 2016 2017 2012 2010 2008 2015 2016
## [37675] 2017 2010 2012 2008 2017 2015 2010 2007 2015 2012 2016 2012 2015
## [37688] 2016 2016 2017 2009 2015 2015 2015 2015 2017 2017 2009 2015 2016
## [37701] 2016 2017 2015 2015 2015 2016 2016 2012 2014 2016 2015 2016 2014
## [37714] 2011 2014 2017 2011 2015 2017 2013 2015 2012 2012 2011 2015 2017
## [37727] 2009 2008 2011 2007 2015 2011 2013 2015 2016 2014 2016 2017 2010
## [37740] 2011 2016 2017 2017 2017 2017 2017 2017 2009 2011 2008 2010 2015
## [37753] 2015 2016 2017 2016 2009 2011 2012 2017 2014 2015 2014 2015 2017
## [37766] 2011 2013 2015 2011 2010 2013 2010 2010 2009 2015 2009 2007 2007
## [37779] 2016 2016 2009 2016 2017 2017 2017 2010 2015 2017 2007 2014 2015
## [37792] 2017 2016 2017 2009 2015 2016 2017 2007 2010 2016 2016 2009 2016
## [37805] 2016 2017 2008 2015 2016 2014 2016 2017 2015 2009 2010 2010 2016
## [37818] 2017 2007 2010 2017 2007 2015 2017 2009 2015 2015 2007 2014 2016
## [37831] 2016 2017 2007 2010 2016 2016 2016 2016 2017 2016 2009 2014 2016
## [37844] 2017 2017 2014 2015 2016 2016 2017 2013 2014 2017 2009 2013 2014
## [37857] 2011 2011 2011 2016 2017 2015 2017 2012 2015 2007 2014 2017 2016
## [37870] 2016 2017 2008 2017 2017 2017 2017 2011 2007 2007 2013 2015 2017
## [37883] 2016 2017 2007 2011 2009 2009 2013 2016 2017 2009 2016 2016 2017
## [37896] 2017 2009 2016 2016 2016 2015 2016 2017 2010 2016 2016 2016 2016
## [37909] 2010 2015 2016 2007 2011 2016 2017 2011 2011 2015 2015 2016 2008
## [37922] 2016 2017 2010 2014 2013 2015 2015 2015 2016 2017 2017 2008 2017
## [37935] 2009 2008 2016 2007 2015 2012 2015 2015 2016 2017 2011 2011 2016
## [37948] 2017 2010 2013 2015 2015 2016 2008 2015 2008 2015 2016 2016 2015
## [37961] 2017 2011 2009 2008 2016 2016 2015 2015 2012 2013 2017 2012 2011
## [37974] 2015 2016 2010 2007 2009 2007 2016 2017 2007 2013 2015 2017 2010
## [37987] 2007 2013 2017 2011 2017 2016 2010 2017 2015 2008 2016 2017 2010
## [38000] 2017 2008 2015 2010 2013 2016 2016 2015 2017 2009 2010 2015 2017
## [38013] 2017 2015 2008 2013 2013 2013 2017 2007 2011 2014 2016 2011 2008
## [38026] 2016 2017 2011 2008 2007 2013 2015 2017 2008 2008 2013 2011 2013
## [38039] 2016 2017 2017 2008 2016 2015 2010 2010 2015 2017 2009 2016 2015
## [38052] 2010 2011 2013 2016 2016 2017 2011 2015 2012 2011 2016 2015 2017
## [38065] 2017 2012 2014 2017 2017 2010 2008 2015 2016 2016 2017 2017 2011
## [38078] 2009 2011 2014 2014 2015 2015 2016 2016 2009 2012 2014 2014 2017
## [38091] 2017 2017 2007 2016 2008 2011 2011 2011 2015 2012 2017 2015 2015
## [38104] 2016 2017 2012 2007 2013 2014 2015 2007 2012 2009 2017 2017 2014
## [38117] 2015 2015 2012 2009 2017 2015 2017 2017 2011 2010 2015 2012 2008
## [38130] 2012 2014 2015 2015 2017 2013 2016 2014 2016 2015 2007 2013 2015
## [38143] 2017 2015 2015 2017 2009 2012 2013 2015 2017 2014 2008 2017 2015
## [38156] 2008 2010 2016 2016 2017 2007 2008 2009 2014 2015 2016 2017 2013
## [38169] 2016 2007 2013 2016 2015 2015 2016 2016 2017 2010 2009 2010 2016
## [38182] 2015 2016 2012 2008 2008 2009 2012 2016 2016 2015 2016 2015 2015
## [38195] 2016 2016 2014 2010 2013 2014 2016 2015 2015 2017 2008 2016 2011
## [38208] 2014 2014 2017 2008 2016 2016 2016 2015 2017 2014 2015 2009 2009
## [38221] 2013 2016 2016 2012 2013 2015 2017 2017 2012 2010 2014 2017 2017
## [38234] 2017 2007 2009 2013 2015 2010 2016 2015 2017 2011 2011 2012 2014
## [38247] 2016 2016 2016 2017 2013 2017 2009 2017 2011 2016 2010 2014 2016
## [38260] 2017 2015 2016 2010 2012 2011 2016 2016 2016 2015 2017 2015 2016
## [38273] 2016 2013 2015 2016 2016 2015 2016 2016 2015 2017 2017 2017 2017
## [38286] 2007 2013 2008 2013 2015 2016 2015 2017 2008 2016 2017 2017 2015
## [38299] 2017 2017 2008 2010 2013 2015 2008 2009 2011 2010 2011 2015 2015
## [38312] 2017 2008 2009 2014 2017 2008 2007 2016 2017 2017 2009 2015 2016
## [38325] 2012 2010 2017 2017 2011 2008 2014 2015 2017 2007 2015 2011 2013
## [38338] 2015 2017 2012 2014 2011 2013 2017 2012 2010 2009 2011 2011 2013
## [38351] 2017 2009 2015 2016 2017 2008 2013 2017 2017 2009 2011 2016 2017
## [38364] 2007 2008 2015 2017 2017 2017 2016 2012 2015 2016 2008 2007 2016
## [38377] 2015 2015 2017 2016 2017 2009 2015 2017 2008 2016 2016 2007 2008
## [38390] 2017 2011 2014 2016 2017 2017 2014 2011 2016 2007 2016 2010 2009
## [38403] 2008 2013 2012 2014 2015 2010 2010 2010 2017 2015 2017 2011 2013
## [38416] 2013 2011 2012 2009 2010 2014 2017 2011 2017 2017 2007 2016 2016
## [38429] 2013 2016 2015 2017 2016 2017 2010 2008 2013 2015 2017 2007 2012
## [38442] 2007 2009 2015 2017 2007 2007 2008 2011 2013 2016 2010 2008 2013
## [38455] 2015 2011 2007 2015 2016 2012 2012 2016 2017 2017 2016 2012 2016
## [38468] 2017 2017 2015 2016 2017 2017 2007 2007 2013 2017 2009 2014 2016
## [38481] 2016 2017 2015 2016 2017 2016 2017 2012 2017 2016 2007 2010 2015
## [38494] 2009 2010 2009 2017 2013 2011 2011 2017 2010 2010 2014 2014 2015
## [38507] 2015 2016 2007 2012 2014 2017 2012 2014 2016 2017 2017 2010 2008
## [38520] 2017 2012 2016 2017 2015 2011 2017 2017 2016 2017 2015 2017 2014
## [38533] 2017 2012 2010 2014 2015 2015 2012 2013 2015 2016 2017 2016 2015
## [38546] 2014 2017 2016 2017 2016 2017 2017 2016 2008 2016 2016 2008 2008
## [38559] 2010 2017 2017 2011 2014 2016 2011 2015 2009 2012 2016 2017 2011
## [38572] 2013 2017 2017 2014 2015 2017 2008 2017 2012 2016 2007 2008 2017
## [38585] 2011 2008 2011 2009 2015 2016 2017 2007 2015 2017 2017 2009 2009
## [38598] 2015 2017 2014 2015 2017 2007 2010 2014 2017 2007 2016 2016 2017
## [38611] 2007 2015 2016 2010 2009 2008 2015 2015 2011 2013 2015 2015 2007
## [38624] 2008 2014 2016 2015 2012 2007 2013 2015 2016 2010 2009 2013 2010
## [38637] 2014 2017 2014 2016 2015 2017 2008 2015 2017 2009 2008 2011 2015
## [38650] 2015 2016 2017 2017 2007 2008 2016 2017 2017 2017 2014 2015 2016
## [38663] 2007 2016 2012 2015 2010 2012 2012 2007 2016 2015 2017 2017 2012
## [38676] 2017 2017 2008 2010 2013 2016 2011 2011 2010 2014 2015 2016 2016
## [38689] 2017 2011 2015 2017 2017 2016 2016 2016 2017 2012 2017 2017 2009
## [38702] 2010 2007 2016 2016 2010 2009 2015 2017 2013 2015 2017 2017 2009
## [38715] 2010 2008 2016 2017 2017 2010 2007 2007 2015 2017 2008 2016 2017
## [38728] 2016 2016 2017 2008 2016 2017 2011 2011 2014 2016 2011 2008 2017
## [38741] 2009 2015 2017 2008 2014 2014 2015 2016 2014 2015 2011 2010 2017
## [38754] 2015 2017 2010 2009 2016 2016 2017 2014 2017 2017 2009 2015 2015
## [38767] 2015 2011 2013 2016 2016 2016 2009 2016 2010 2008 2016 2011 2009
## [38780] 2014 2017 2007 2008 2016 2017 2007 2009 2017 2017 2008 2014 2016
## [38793] 2013 2014 2015 2011 2015 2015 2017 2017 2010 2017 2007 2011 2013
## [38806] 2016 2017 2009 2013 2008 2013 2017 2010 2012 2008 2014 2017 2007
## [38819] 2010 2017 2017 2009 2015 2007 2016 2007 2007 2017 2012 2011 2013
## [38832] 2009 2016 2017 2016 2015 2016 2007 2013 2015 2017 2017 2017 2011
## [38845] 2010 2017 2015 2016 2015 2012 2016 2016 2017 2017 2016 2013 2016
## [38858] 2016 2008 2008 2013 2012 2015 2016 2017 2011 2011 2008 2012 2010
## [38871] 2016 2012 2017 2017 2009 2017 2009 2009 2016 2016 2012 2016 2007
## [38884] 2015 2016 2017 2011 2007 2014 2016 2015 2013 2015 2016 2017 2017
## [38897] 2012 2017 2017 2017 2008 2008 2008 2012 2008 2015 2016 2014 2017
## [38910] 2017 2016 2017 2015 2016 2017 2015 2017 2017 2008 2013 2016 2010
## [38923] 2013 2007 2016 2008 2015 2015 2011 2013 2014 2017 2012 2016 2016
## [38936] 2017 2009 2009 2015 2016 2012 2015 2011 2012 2007 2016 2017 2011
## [38949] 2016 2017 2008 2015 2008 2015 2009 2009 2015 2016 2012 2009 2015
## [38962] 2015 2015 2010 2014 2016 2015 2016 2017 2014 2012 2016 2015 2016
## [38975] 2016 2015 2014 2014 2015 2015 2017 2008 2016 2017 2016 2017 2017
## [38988] 2017 2007 2010 2015 2017 2013 2015 2016 2016 2017 2015 2015 2017
## [39001] 2015 2008 2007 2014 2010 2011 2007 2015 2015 2016 2017 2017 2009
## [39014] 2010 2011 2015 2015 2015 2016 2016 2017 2010 2014 2015 2015 2017
## [39027] 2017 2016 2008 2007 2013 2015 2017 2010 2013 2016 2016 2017 2008
## [39040] 2016 2008 2016 2017 2017 2017 2009 2015 2016 2009 2008 2008 2016
## [39053] 2014 2014 2008 2015 2016 2016 2011 2007 2016 2011 2015 2015 2015
## [39066] 2015 2016 2015 2011 2013 2011 2012 2016 2016 2017 2016 2016 2015
## [39079] 2016 2016 2017 2009 2009 2016 2016 2015 2017 2010 2010 2017 2017
## [39092] 2011 2016 2017 2008 2009 2017 2008 2009 2016 2013 2015 2010 2017
## [39105] 2010 2014 2015 2017 2012 2008 2016 2011 2016 2014 2015 2010 2007
## [39118] 2011 2016 2013 2016 2017 2015 2008 2012 2008 2015 2015 2016 2017
## [39131] 2014 2008 2017 2008 2010 2013 2015 2017 2016 2016 2015 2016 2017
## [39144] 2016 2011 2016 2011 2012 2017 2016 2007 2011 2015 2016 2016 2017
## [39157] 2017 2009 2014 2010 2015 2015 2016 2017 2015 2017 2017 2017 2009
## [39170] 2015 2017 2009 2016 2016 2017 2017 2012 2008 2013 2012 2015 2015
## [39183] 2016 2008 2013 2017 2017 2017 2010 2016 2011 2009 2013 2016 2007
## [39196] 2011 2016 2017 2017 2008 2011 2010 2008 2016 2015 2011 2016 2016
## [39209] 2016 2012 2015 2015 2009 2014 2015 2015 2016 2009 2007 2015 2015
## [39222] 2009 2007 2014 2016 2014 2015 2017 2015 2014 2017 2017 2012 2013
## [39235] 2017 2017 2017 2008 2009 2013 2011 2009 2013 2016 2017 2008 2011
## [39248] 2012 2014 2016 2016 2017 2017 2010 2012 2017 2016 2016 2008 2017
## [39261] 2008 2016 2012 2007 2017 2011 2008 2017 2017 2017 2015 2016 2017
## [39274] 2015 2017 2016 2017 2015 2016 2017 2008 2009 2007 2007 2012 2011
## [39287] 2009 2016 2016 2015 2017 2015 2012 2011 2013 2012 2015 2008 2011
## [39300] 2013 2016 2015 2016 2017 2010 2011 2008 2016 2017 2014 2008 2017
## [39313] 2012 2016 2013 2014 2015 2012 2008 2016 2007 2016 2016 2008 2008
## [39326] 2008 2016 2017 2015 2017 2017 2010 2012 2016 2017 2011 2017 2009
## [39339] 2013 2013 2015 2017 2011 2017 2016 2007 2008 2008 2013 2013 2016
## [39352] 2015 2017 2017 2017 2013 2016 2017 2012 2015 2017 2007 2015 2015
## [39365] 2016 2017 2017 2017 2016 2016 2009 2015 2015 2016 2017 2017 2014
## [39378] 2010 2008 2015 2017 2017 2017 2015 2017 2010 2009 2017 2013 2014
## [39391] 2016 2016 2016 2017 2015 2017 2015 2010 2015 2014 2015 2015 2008
## [39404] 2014 2017 2016 2016 2017 2017 2017 2016 2015 2010 2016 2017 2007
## [39417] 2011 2014 2016 2015 2012 2011 2014 2015 2017 2011 2016 2007 2012
## [39430] 2014 2015 2016 2015 2016 2017 2010 2017 2017 2015 2017 2015 2015
## [39443] 2012 2012 2015 2015 2016 2010 2016 2017 2012 2008 2013 2007 2011
## [39456] 2015 2016 2017 2007 2016 2015 2016 2017 2015 2011 2013 2016 2012
## [39469] 2015 2015 2011 2015 2017 2008 2013 2017 2008 2017 2017 2014 2017
## [39482] 2017 2009 2015 2016 2015 2017 2009 2009 2017 2017 2017 2012 2009
## [39495] 2012 2016 2017 2015 2007 2013 2012 2013 2009 2015 2015 2015 2011
## [39508] 2017 2016 2017 2017 2011 2017 2016 2008 2009 2015 2015 2011 2012
## [39521] 2016 2017 2009 2013 2015 2016 2015 2009 2017 2008 2011 2017 2010
## [39534] 2015 2009 2014 2016 2011 2009 2008 2013 2014 2016 2016 2015 2017
## [39547] 2017 2015 2017 2008 2015 2016 2017 2009 2016 2008 2014 2017 2009
## [39560] 2017 2013 2014 2015 2017 2017 2012 2015 2016 2017 2017 2017 2015
## [39573] 2011 2007 2016 2016 2016 2007 2015 2015 2008 2015 2016 2011 2017
## [39586] 2014 2016 2015 2010 2013 2015 2017 2015 2015 2011 2015 2015 2008
## [39599] 2014 2017 2007 2010 2015 2016 2015 2015 2016 2015 2014 2015 2016
## [39612] 2015 2016 2015 2015 2011 2007 2015 2016 2007 2014 2015 2017 2008
## [39625] 2010 2017 2012 2013 2016 2017 2016 2017 2012 2015 2017 2007 2016
## [39638] 2017 2017 2017 2015 2017 2013 2014 2009 2016 2011 2009 2015 2015
## [39651] 2016 2016 2016 2017 2011 2016 2013 2017 2016 2015 2007 2012 2016
## [39664] 2009 2014 2017 2015 2008 2010 2009 2009 2007 2007 2015 2013 2015
## [39677] 2017 2012 2007 2014 2015 2016 2010 2007 2016 2015 2016 2014 2014
## [39690] 2016 2010 2013 2014 2016 2009 2015 2017 2017 2013 2017 2007 2014
## [39703] 2017 2007 2009 2011 2015 2015 2017 2017 2011 2015 2017 2017 2011
## [39716] 2008 2015 2015 2016 2017 2016 2007 2016 2017 2010 2015 2017 2007
## [39729] 2008 2007 2016 2015 2009 2012 2017 2011 2009 2014 2014 2015 2017
## [39742] 2017 2013 2009 2014 2017 2017 2010 2008 2017 2011 2015 2017 2014
## [39755] 2015 2017 2017 2008 2016 2013 2009 2013 2013 2008 2011 2015 2012
## [39768] 2011 2011 2007 2009 2015 2008 2010 2013 2008 2012 2008 2015 2009
## [39781] 2016 2016 2011 2007 2015 2015 2017 2016 2012 2016 2016 2008 2010
## [39794] 2013 2016 2015 2016 2015 2008 2007 2016 2017 2015 2012 2011 2010
## [39807] 2016 2015 2017 2015 2007 2010 2008 2012 2015 2017 2015 2016 2015
## [39820] 2014 2015 2008 2016 2016 2016 2011 2013 2014 2016 2016 2016 2017
## [39833] 2010 2008 2016 2016 2015 2016 2009 2008 2015 2015 2012 2013 2015
## [39846] 2013 2015 2012 2016 2009 2014 2011 2012 2014 2016 2016 2007 2014
## [39859] 2016 2013 2010 2015 2017 2009 2015 2009 2013 2014 2017 2016 2017
## [39872] 2010 2014 2016 2015 2015 2016 2017 2011 2008 2015 2017 2016 2008
## [39885] 2011 2015 2017 2016 2010 2014 2015 2016 2016 2010 2012 2007 2015
## [39898] 2016 2016 2017 2015 2016 2017 2015 2016 2017 2016 2014 2016 2015
## [39911] 2017 2014 2016 2016 2017 2016 2017 2016 2017 2011 2009 2012 2015
## [39924] 2016 2011 2016 2016 2017 2017 2017 2011 2007 2014 2017 2010 2009
## [39937] 2008 2015 2011 2010 2011 2016 2016 2016 2017 2009 2017 2014 2016
## [39950] 2016 2016 2015 2017 2016 2008 2015 2007 2007 2015 2015 2007 2012
## [39963] 2016 2010 2008 2014 2017 2008 2012 2013 2013 2016 2013 2016 2017
## [39976] 2008 2010 2009 2014 2011 2016 2016 2016 2016 2017 2007 2010 2017
## [39989] 2017 2008 2016 2007 2011 2016 2016 2010 2008 2012 2008 2015 2017
## [40002] 2016 2017 2015 2010 2012 2016 2008 2013 2010 2016 2015 2017 2009
## [40015] 2017 2009 2015 2017 2017 2011 2012 2008 2008 2008 2016 2016 2017
## [40028] 2009 2016 2017 2008 2007 2012 2015 2017 2008 2008 2007 2013 2015
## [40041] 2016 2017 2010 2008 2012 2015 2016 2015 2016 2012 2016 2016 2016
## [40054] 2017 2016 2017 2015 2016 2017 2010 2008 2012 2016 2017 2008 2016
## [40067] 2017 2017 2011 2013 2016 2013 2017 2008 2015 2017 2008 2017 2016
## [40080] 2013 2014 2017 2015 2015 2017 2017 2013 2016 2016 2017 2012 2014
## [40093] 2009 2008 2012 2012 2016 2016 2015 2010 2015 2016 2007 2014 2016
## [40106] 2017 2007 2014 2015 2011 2015 2016 2016 2013 2017 2007 2008 2017
## [40119] 2016 2017 2017 2017 2010 2008 2011 2015 2016 2009 2015 2016 2016
## [40132] 2009 2017 2009 2011 2015 2010 2016 2016 2011 2015 2016 2012 2009
## [40145] 2009 2014 2015 2017 2008 2008 2015 2008 2008 2007 2015 2007 2007
## [40158] 2015 2007 2013 2014 2014 2016 2016 2017 2015 2010 2012 2015 2015
## [40171] 2008 2015 2017 2017 2017 2017 2016 2007 2008 2014 2015 2015 2015
## [40184] 2016 2012 2010 2015 2017 2015 2017 2011 2014 2014 2015 2016 2016
## [40197] 2007 2010 2011 2015 2017 2017 2017 2012 2015 2012 2010 2010 2010
## [40210] 2015 2016 2014 2008 2015 2011 2013 2014 2008 2007 2016 2017 2010
## [40223] 2017 2012 2016 2016 2017 2017 2015 2017 2011 2017 2008 2008 2016
## [40236] 2016 2017 2017 2007 2015 2011 2011 2015 2016 2016 2016 2016 2015
## [40249] 2007 2013 2015 2007 2010 2010 2010 2007 2016 2016 2012 2016 2017
## [40262] 2008 2009 2008 2012 2015 2017 2015 2015 2010 2017 2007 2016 2016
## [40275] 2017 2007 2007 2011 2015 2017 2009 2015 2016 2011 2016 2012 2017
## [40288] 2009 2015 2017 2017 2013 2017 2010 2017 2011 2012 2007 2012 2008
## [40301] 2016 2009 2016 2013 2017 2015 2007 2011 2014 2016 2013 2014 2017
## [40314] 2011 2008 2015 2016 2010 2016 2010 2015 2017 2017 2012 2010 2015
## [40327] 2015 2016 2017 2007 2014 2015 2007 2007 2015 2016 2017 2008 2017
## [40340] 2011 2016 2011 2011 2015 2015 2016 2017 2010 2016 2017 2015 2017
## [40353] 2017 2007 2015 2017 2008 2013 2014 2017 2014 2016 2016 2007 2007
## [40366] 2015 2017 2012 2011 2013 2008 2016 2017 2010 2008 2015 2017 2017
## [40379] 2011 2015 2017 2017 2010 2016 2016 2009 2015 2017 2008 2014 2011
## [40392] 2010 2017 2017 2007 2016 2011 2013 2010 2008 2016 2012 2016 2017
## [40405] 2017 2017 2017 2013 2016 2016 2015 2015 2008 2017 2012 2010 2013
## [40418] 2010 2014 2015 2016 2016 2015 2015 2016 2015 2009 2015 2011 2007
## [40431] 2014 2017 2013 2007 2015 2017 2012 2017 2015 2015 2017 2008 2017
## [40444] 2008 2016 2017 2016 2016 2016 2015 2016 2017 2017 2011 2017 2014
## [40457] 2016 2015 2015 2017 2012 2015 2008 2015 2017 2009 2016 2017 2017
## [40470] 2010 2011 2007 2012 2011 2015 2017 2017 2013 2015 2017 2013 2012
## [40483] 2015 2015 2011 2010 2015 2017 2016 2015 2014 2008 2017 2012 2010
## [40496] 2012 2015 2016 2016 2016 2008 2014 2015 2017 2016 2007 2017 2015
## [40509] 2015 2015 2012 2010 2014 2015 2016 2016 2017 2010 2009 2015 2009
## [40522] 2013 2015 2015 2015 2014 2015 2007 2013 2016 2015 2007 2010 2014
## [40535] 2014 2017 2012 2013 2016 2007 2008 2016 2017 2016 2017 2011 2009
## [40548] 2013 2012 2013 2017 2014 2017 2017 2015 2016 2015 2017 2009 2016
## [40561] 2016 2007 2017 2017 2007 2016 2016 2016 2016 2017 2012 2011 2010
## [40574] 2014 2016 2007 2015 2015 2017 2011 2014 2016 2016 2016 2017 2015
## [40587] 2017 2010 2016 2016 2016 2017 2017 2017 2008 2014 2015 2015 2017
## [40600] 2009 2007 2015 2016 2017 2012 2015 2015 2008 2015 2017 2012 2016
## [40613] 2015 2007 2012 2010 2013 2015 2015 2015 2016 2017 2011 2010 2013
## [40626] 2007 2016 2017 2016 2008 2014 2015 2017 2007 2016 2007 2012 2011
## [40639] 2011 2010 2013 2015 2016 2017 2015 2017 2009 2015 2017 2017 2011
## [40652] 2007 2011 2016 2017 2017 2010 2015 2010 2007 2008 2016 2017 2008
## [40665] 2015 2015 2009 2007 2008 2017 2017 2008 2009 2017 2017 2009 2007
## [40678] 2009 2016 2017 2017 2011 2016 2015 2008 2016 2013 2015 2017 2017
## [40691] 2012 2016 2015 2016 2017 2015 2007 2013 2010 2015 2015 2016 2016
## [40704] 2017 2010 2016 2007 2009 2014 2017 2017 2010 2014 2013 2016 2017
## [40717] 2017 2015 2017 2007 2017 2008 2007 2016 2008 2015 2015 2008 2015
## [40730] 2016 2015 2007 2009 2009 2010 2017 2008 2015 2015 2016 2015 2017
## [40743] 2011 2017 2014 2015 2008 2010 2008 2016 2017 2015 2014 2015 2015
## [40756] 2012 2015 2016 2017 2011 2008 2017 2007 2014 2015 2017 2017 2015
## [40769] 2017 2016 2016 2009 2011 2017 2016 2016 2017 2011 2007 2013 2010
## [40782] 2016 2016 2016 2017 2015 2015 2017 2012 2007 2010 2007 2017 2016
## [40795] 2017 2010 2011 2017 2015 2013 2011 2015 2011 2009 2017 2008 2012
## [40808] 2017 2007 2007 2012 2012 2015 2016 2016 2017 2017 2010 2015 2016
## [40821] 2007 2007 2007 2016 2007 2015 2016 2016 2016 2008 2015 2015 2013
## [40834] 2017 2010 2010 2017 2009 2010 2016 2017 2017 2009 2015 2017 2011
## [40847] 2017 2012 2010 2014 2015 2015 2017 2016 2016 2013 2017 2007 2008
## [40860] 2015 2015 2015 2011 2016 2017 2008 2008 2016 2016 2017 2017 2010
## [40873] 2011 2013 2016 2010 2016 2015 2015 2017 2015 2015 2017 2008 2007
## [40886] 2009 2010 2012 2014 2016 2010 2013 2014 2015 2015 2016 2012 2017
## [40899] 2017 2016 2016 2011 2010 2008 2015 2016 2017 2010 2012 2009 2014
## [40912] 2013 2015 2015 2016 2017 2017 2012 2015 2014 2016 2017 2012 2016
## [40925] 2007 2007 2014 2011 2013 2017 2016 2008 2016 2007 2013 2011 2016
## [40938] 2015 2016 2017 2015 2017 2017 2014 2007 2013 2017 2016 2017 2017
## [40951] 2015 2016 2012 2013 2010 2009 2015 2009 2010 2016 2016 2015 2009
## [40964] 2009 2015 2015 2017 2017 2010 2015 2015 2011 2016 2012 2011 2007
## [40977] 2015 2010 2014 2015 2017 2010 2016 2016 2015 2017 2008 2011 2011
## [40990] 2014 2015 2011 2014 2015 2016 2015 2016 2017 2015 2015 2017 2017
## [41003] 2009 2015 2013 2010 2017 2014 2015 2016 2016 2016 2016 2011 2008
## [41016] 2009 2014 2010 2009 2015 2016 2008 2007 2010 2013 2010 2014 2012
## [41029] 2017 2015 2016 2016 2016 2016 2016 2016 2012 2015 2017 2017 2008
## [41042] 2016 2016 2016 2013 2014 2016 2010 2009 2015 2014 2016 2016 2015
## [41055] 2017 2017 2012 2016 2015 2010 2011 2015 2015 2015 2016 2016 2017
## [41068] 2017 2017 2015 2017 2017 2017 2014 2017 2017 2009 2007 2015 2015
## [41081] 2017 2015 2011 2007 2015 2016 2017 2007 2017 2017 2007 2012 2015
## [41094] 2009 2014 2014 2015 2016 2017 2016 2017 2017 2009 2016 2017 2010
## [41107] 2010 2017 2017 2016 2017 2016 2011 2007 2015 2017 2017 2017 2017
## [41120] 2017 2010 2009 2008 2012 2007 2007 2008 2016 2011 2015 2016 2016
## [41133] 2017 2010 2008 2011 2015 2017 2010 2009 2016 2017 2011 2012 2014
## [41146] 2016 2016 2016 2012 2011 2015 2010 2017 2007 2012 2013 2015 2017
## [41159] 2017 2012 2017 2014 2015 2015 2015 2017 2008 2016 2016 2017 2017
## [41172] 2017 2011 2014 2015 2017 2017 2007 2016 2014 2014 2015 2016 2010
## [41185] 2016 2017 2012 2011 2013 2012 2015 2016 2016 2010 2015 2016 2017
## [41198] 2011 2015 2016 2017 2009 2015 2017 2017 2017 2017 2009 2013 2015
## [41211] 2016 2013 2015 2016 2016 2009 2015 2016 2017 2017 2011 2017 2014
## [41224] 2016 2010 2008 2014 2008 2016 2010 2015 2016 2007 2009 2008 2011
## [41237] 2016 2017 2007 2013 2015 2016 2010 2010 2011 2010 2017 2008 2016
## [41250] 2017 2007 2017 2017 2017 2010 2017 2015 2015 2017 2013 2014 2015
## [41263] 2017 2014 2015 2015 2016 2017 2017 2015 2015 2016 2011 2009 2016
## [41276] 2017 2009 2017 2010 2011 2015 2015 2017 2015 2015 2011 2012 2016
## [41289] 2008 2014 2015 2016 2016 2016 2012 2007 2017 2009 2017 2016 2009
## [41302] 2011 2016 2015 2016 2016 2012 2015 2016 2016 2015 2016 2017 2011
## [41315] 2007 2017 2011 2014 2015 2008 2009 2016 2017 2014 2015 2016 2016
## [41328] 2008 2015 2016 2012 2009 2017 2011 2011 2008 2008 2016 2015 2016
## [41341] 2016 2013 2016 2012 2013 2016 2017 2017 2012 2013 2016 2017 2017
## [41354] 2013 2014 2016 2012 2015 2011 2017 2017 2012 2007 2007 2017 2017
## [41367] 2010 2014 2008 2014 2016 2017 2015 2011 2015 2016 2011 2010 2015
## [41380] 2016 2017 2011 2012 2009 2009 2009 2016 2016 2008 2015 2016 2016
## [41393] 2016 2015 2017 2012 2016 2012 2013 2013 2016 2009 2009 2017 2008
## [41406] 2010 2016 2016 2017 2009 2007 2016 2008 2013 2016 2017 2009 2012
## [41419] 2014 2015 2015 2014 2016 2017 2015 2008 2010 2007 2013 2016 2012
## [41432] 2010 2016 2015 2015 2017 2011 2016 2010 2008 2015 2016 2017 2007
## [41445] 2017 2013 2016 2017 2008 2016 2016 2016 2017 2012 2008 2015 2009
## [41458] 2008 2010 2017 2017 2008 2015 2016 2017 2016 2016 2007 2015 2015
## [41471] 2016 2017 2017 2015 2017 2017 2011 2017 2011 2016 2015 2009 2013
## [41484] 2017 2009 2015 2010 2010 2013 2015 2017 2017 2010 2008 2016 2017
## [41497] 2007 2015 2015 2016 2016 2015 2016 2016 2015 2017 2017 2017 2017
## [41510] 2008 2012 2015 2016 2016 2016 2015 2015 2008 2009 2014 2017 2015
## [41523] 2010 2015 2015 2017 2012 2013 2015 2015 2016 2016 2015 2015 2016
## [41536] 2016 2008 2015 2007 2016 2009 2008 2008 2008 2017 2017 2017 2014
## [41549] 2015 2017 2017 2007 2017 2015 2007 2015 2016 2014 2015 2015 2012
## [41562] 2007 2016 2013 2016 2017 2009 2008 2013 2017 2012 2013 2014 2015
## [41575] 2015 2013 2017 2017 2011 2016 2017 2007 2015 2015 2017 2010 2014
## [41588] 2015 2013 2016 2012 2009 2008 2015 2017 2008 2015 2016 2007 2008
## [41601] 2017 2017 2010 2017 2008 2014 2015 2015 2015 2016 2008 2015 2015
## [41614] 2011 2016 2016 2010 2015 2017 2017 2007 2009 2015 2016 2017 2017
## [41627] 2012 2007 2011 2012 2012 2012 2015 2016 2014 2009 2015 2007 2015
## [41640] 2008 2012 2017 2017 2017 2017 2011 2007 2014 2016 2016 2017 2017
## [41653] 2011 2017 2008 2009 2008 2016 2016 2016 2017 2011 2016 2016 2015
## [41666] 2016 2010 2015 2017 2017 2015 2010 2013 2013 2008 2009 2015 2015
## [41679] 2016 2009 2013 2015 2017 2008 2017 2015 2009 2010 2014 2017 2015
## [41692] 2017 2014 2017 2012 2016 2017 2017 2009 2013 2012 2015 2016 2017
## [41705] 2009 2013 2017 2008 2015 2016 2016 2008 2014 2015 2017 2009 2017
## [41718] 2009 2007 2016 2017 2010 2008 2016 2017 2017 2010 2016 2016 2011
## [41731] 2017 2017 2012 2008 2015 2016 2015 2017 2007 2015 2008 2013 2016
## [41744] 2015 2008 2015 2008 2017 2010 2015 2015 2016 2015 2008 2014 2016
## [41757] 2016 2008 2010 2011 2015 2016 2017 2016 2015 2017 2015 2016 2010
## [41770] 2010 2016 2015 2012 2011 2017 2012 2014 2017 2017 2017 2015 2017
## [41783] 2011 2016 2017 2009 2007 2012 2009 2014 2016 2015 2010 2017 2010
## [41796] 2016 2010 2011 2017 2017 2008 2015 2015 2010 2008 2016 2017 2012
## [41809] 2007 2017 2017 2011 2008 2011 2017 2017 2013 2013 2017 2014 2016
## [41822] 2017 2011 2008 2011 2010 2016 2016 2016 2014 2016 2017 2017 2017
## [41835] 2015 2016 2017 2016 2017 2010 2013 2014 2016 2016 2016 2007 2009
## [41848] 2010 2015 2016 2010 2015 2008 2015 2016 2016 2016 2007 2012 2007
## [41861] 2011 2008 2014 2015 2016 2016 2017 2009 2016 2010 2015 2016 2017
## [41874] 2007 2016 2017 2007 2008 2015 2015 2007 2013 2016 2015 2017 2009
## [41887] 2014 2008 2016 2017 2009 2007 2015 2015 2016 2017 2017 2015 2017
## [41900] 2012 2009 2017 2008 2009 2014 2009 2010 2012 2010 2007 2008 2014
## [41913] 2015 2015 2012 2012 2007 2017 2007 2014 2015 2016 2016 2017 2016
## [41926] 2017 2014 2016 2008 2009 2016 2008 2015 2016 2017 2010 2011 2014
## [41939] 2017 2012 2013 2015 2017 2016 2016 2017 2015 2008 2012 2015 2015
## [41952] 2017 2010 2013 2017 2011 2015 2017 2011 2016 2010 2008 2016 2015
## [41965] 2011 2015 2017 2017 2010 2008 2015 2016 2017 2017 2017 2008 2015
## [41978] 2015 2011 2011 2007 2015 2016 2016 2016 2017 2007 2015 2012 2013
## [41991] 2013 2014 2009 2008 2009 2016 2017 2017 2017 2012 2010 2009 2009
## [42004] 2008 2016 2017 2015 2009 2007 2014 2016 2016 2017 2009 2007 2015
## [42017] 2007 2017 2011 2017 2015 2017 2015 2015 2017 2015 2014 2016 2016
## [42030] 2017 2017 2013 2017 2013 2017 2008 2015 2017 2015 2009 2008 2015
## [42043] 2016 2016 2016 2016 2011 2016 2010 2016 2016 2015 2016 2017 2016
## [42056] 2017 2017 2008 2011 2012 2014 2011 2011 2013 2016 2015 2011 2007
## [42069] 2017 2008 2009 2012 2017 2008 2010 2015 2017 2008 2007 2013 2017
## [42082] 2013 2016 2015 2012 2017 2007 2015 2009 2011 2013 2017 2008 2011
## [42095] 2017 2010 2010 2011 2013 2017 2009 2017 2010 2011 2011 2010 2015
## [42108] 2017 2017 2013 2016 2015 2015 2007 2013 2015 2017 2011 2011 2010
## [42121] 2016 2017 2013 2007 2016 2016 2017 2017 2010 2009 2015 2017 2010
## [42134] 2014 2017 2008 2016 2017 2016 2014 2011 2009 2011 2007 2013 2007
## [42147] 2015 2016 2016 2017 2009 2015 2008 2015 2011 2013 2011 2016 2007
## [42160] 2010 2016 2008 2008 2011 2014 2016 2011 2008 2009 2016 2013 2017
## [42173] 2015 2017 2007 2009 2008 2016 2016 2011 2012 2015 2012 2007 2015
## [42186] 2010 2017 2016 2016 2017 2010 2017 2007 2016 2017 2012 2016 2007
## [42199] 2015 2016 2016 2016 2016 2017 2017 2012 2013 2016 2008 2008 2017
## [42212] 2017 2015 2009 2012 2010 2010 2014 2016 2010 2013 2016 2017 2008
## [42225] 2016 2015 2015 2017 2012 2011 2017 2009 2011 2015 2016 2017 2011
## [42238] 2007 2013 2017 2007 2014 2015 2013 2015 2016 2007 2007 2017 2017
## [42251] 2009 2017 2007 2011 2016 2010 2016 2008 2015 2010 2008 2015 2017
## [42264] 2012 2008 2010 2007 2007 2007 2013 2017 2014 2016 2007 2013 2016
## [42277] 2010 2016 2017 2017 2014 2017 2017 2015 2012 2007 2009 2016 2017
## [42290] 2011 2011 2014 2016 2016 2008 2011 2011 2016 2016 2015 2015 2013
## [42303] 2015 2017 2008 2009 2015 2015 2007 2017 2016 2017 2017 2017 2015
## [42316] 2007 2013 2014 2015 2012 2015 2017 2010 2017 2011 2014 2016 2017
## [42329] 2009 2010 2011 2016 2017 2009 2010 2008 2015 2016 2009 2009 2014
## [42342] 2011 2016 2012 2007 2008 2016 2011 2015 2017 2010 2012 2007 2009
## [42355] 2016 2017 2007 2013 2015 2016 2015 2007 2013 2009 2013 2016 2009
## [42368] 2011 2017 2008 2015 2016 2009 2010 2015 2015 2016 2012 2010 2016
## [42381] 2008 2016 2012 2010 2010 2015 2012 2015 2015 2015 2015 2016 2016
## [42394] 2017 2016 2008 2012 2007 2007 2009 2015 2017 2012 2015 2016 2017
## [42407] 2017 2007 2016 2017 2007 2012 2015 2007 2015 2016 2013 2015 2014
## [42420] 2009 2015 2016 2009 2017 2007 2015 2015 2017 2017 2017 2011 2016
## [42433] 2016 2015 2017 2017 2012 2015 2009 2015 2015 2009 2017 2007 2016
## [42446] 2016 2017 2010 2016 2017 2010 2013 2015 2013 2015 2009 2017 2009
## [42459] 2013 2015 2008 2008 2016 2007 2015 2017 2008 2017 2008 2015 2016
## [42472] 2009 2014 2013 2015 2008 2014 2016 2016 2014 2016 2016 2016 2015
## [42485] 2016 2012 2016 2016 2011 2011 2017 2015 2017 2011 2014 2016 2016
## [42498] 2013 2009 2008 2016 2017 2017 2015 2009 2010 2015 2016 2017 2012
## [42511] 2015 2017 2008 2010 2016 2007 2016 2010 2016 2015 2015 2011 2008
## [42524] 2012 2014 2015 2017 2010 2016 2012 2014 2017 2017 2010 2015 2016
## [42537] 2016 2017 2011 2017 2012 2012 2016 2015 2017 2017 2015 2011 2017
## [42550] 2015 2017 2011 2010 2016 2017 2017 2017 2010 2017 2012 2015 2014
## [42563] 2015 2011 2008 2013 2014 2015 2017 2016 2015 2015 2016 2009 2008
## [42576] 2008 2009 2011 2016 2016 2016 2016 2017 2012 2010 2016 2017 2016
## [42589] 2015 2009 2011 2015 2017 2007 2014 2008 2015 2017 2011 2013 2015
## [42602] 2017 2013 2014 2017 2013 2016 2016 2015 2016 2017 2017 2009 2013
## [42615] 2015 2017 2017 2014 2017 2017 2013 2016 2017 2007 2010 2017 2015
## [42628] 2015 2016 2012 2010 2017 2016 2007 2007 2015 2016 2015 2016 2014
## [42641] 2014 2016 2010 2010 2016 2007 2017 2007 2009 2017 2017 2007 2013
## [42654] 2010 2015 2017 2015 2016 2016 2016 2013 2016 2008 2009 2016 2012
## [42667] 2016 2015 2013 2008 2015 2016 2016 2011 2014 2015 2009 2007 2013
## [42680] 2010 2012 2015 2016 2011 2017 2017 2011 2014 2015 2016 2016 2007
## [42693] 2014 2017 2007 2013 2014 2012 2008 2014 2017 2011 2016 2017 2015
## [42706] 2017 2007 2009 2015 2017 2014 2015 2017 2012 2014 2015 2016 2010
## [42719] 2013 2016 2016 2016 2007 2014 2013 2015 2014 2016 2015 2017 2017
## [42732] 2016 2016 2017 2015 2017 2017 2007 2009 2009 2011 2015 2016 2017
## [42745] 2010 2015 2017 2010 2016 2017 2007 2016 2008 2007 2011 2016 2015
## [42758] 2009 2013 2014 2015 2015 2017 2007 2016 2008 2012 2008 2014 2016
## [42771] 2016 2007 2008 2013 2015 2016 2017 2007 2013 2016 2017 2017 2015
## [42784] 2015 2017 2017 2012 2007 2007 2016 2017 2017 2015 2012 2017 2010
## [42797] 2015 2015 2008 2009 2010 2016 2015 2016 2015 2016 2013 2016 2017
## [42810] 2012 2017 2015 2017 2017 2016 2017 2008 2009 2013 2014 2015 2015
## [42823] 2017 2007 2011 2017 2017 2015 2015 2016 2012 2016 2017 2012 2015
## [42836] 2015 2009 2015 2010 2013 2014 2016 2017 2014 2015 2017 2016 2015
## [42849] 2016 2017 2017 2008 2016 2016 2017 2010 2015 2016 2010 2012 2015
## [42862] 2016 2016 2009 2010 2015 2017 2017 2007 2010 2010 2013 2016 2015
## [42875] 2015 2009 2016 2016 2016 2015 2015 2017 2017 2017 2017 2008 2015
## [42888] 2008 2009 2009 2010 2014 2016 2007 2016 2014 2008 2015 2017 2015
## [42901] 2017 2008 2017 2009 2012 2015 2016 2015 2017 2010 2007 2012 2017
## [42914] 2015 2016 2010 2014 2017 2017 2010 2008 2015 2016 2017 2017 2012
## [42927] 2014 2008 2008 2017 2016 2016 2008 2012 2011 2016 2016 2016 2011
## [42940] 2011 2016 2016 2015 2016 2016 2017 2012 2009 2011 2008 2017 2017
## [42953] 2015 2016 2017 2017 2009 2007 2008 2007 2017 2008 2016 2016 2013
## [42966] 2016 2007 2016 2016 2016 2010 2016 2015 2017 2017 2012 2013 2007
## [42979] 2015 2009 2010 2013 2011 2009 2014 2017 2008 2010 2013 2016 2016
## [42992] 2017 2013 2014 2014 2016 2017 2015 2017 2007 2015 2007 2015 2008
## [43005] 2011 2013 2016 2008 2015 2014 2008 2013 2008 2015 2017 2015 2017
## [43018] 2011 2010 2015 2015 2011 2014 2015 2010 2013 2015 2015 2015 2014
## [43031] 2017 2009 2008 2015 2016 2017 2009 2015 2017 2007 2013 2012 2017
## [43044] 2016 2007 2015 2015 2011 2009 2007 2012 2017 2010 2007 2008 2014
## [43057] 2017 2007 2010 2013 2017 2013 2016 2017 2010 2008 2013 2016 2016
## [43070] 2011 2012 2007 2012 2016 2016 2017 2015 2007 2015 2011 2015 2008
## [43083] 2017 2017 2017 2017 2014 2016 2016 2017 2012 2015 2011 2011 2015
## [43096] 2015 2010 2012 2012 2010 2011 2013 2016 2016 2017 2015 2015 2011
## [43109] 2012 2016 2016 2017 2014 2016 2008 2016 2017 2010 2011 2016 2015
## [43122] 2017 2007 2009 2017 2014 2015 2016 2008 2015 2016 2017 2017 2017
## [43135] 2017 2014 2016 2016 2016 2007 2015 2016 2011 2015 2015 2016 2015
## [43148] 2011 2014 2016 2007 2013 2015 2017 2017 2017 2015 2012 2007 2016
## [43161] 2016 2016 2013 2017 2008 2010 2014 2010 2016 2014 2016 2016 2016
## [43174] 2017 2017 2010 2011 2016 2016 2017 2010 2010 2013 2017 2017 2015
## [43187] 2013 2016 2015 2015 2010 2016 2009 2009 2015 2016 2017 2007 2015
## [43200] 2015 2016 2017 2008 2012 2016 2016 2009 2010 2016 2016 2012 2010
## [43213] 2007 2007 2015 2016 2015 2017 2012 2016 2016 2016 2016 2016 2017
## [43226] 2017 2013 2015 2015 2011 2011 2007 2013 2013 2016 2017 2008 2007
## [43239] 2008 2015 2008 2012 2007 2011 2016 2016 2008 2012 2009 2015 2016
## [43252] 2011 2017 2017 2012 2012 2017 2015 2008 2016 2017 2016 2017 2009
## [43265] 2016 2017 2010 2007 2009 2013 2017 2010 2015 2012 2016 2017 2010
## [43278] 2009 2015 2014 2015 2009 2008 2016 2009 2013 2015 2008 2009 2014
## [43291] 2008 2015 2017 2017 2011 2016 2011 2007 2017 2016 2015 2017 2009
## [43304] 2016 2009 2011 2007 2017 2017 2011 2011 2012 2015 2017 2008 2011
## [43317] 2011 2015 2007 2007 2014 2016 2016 2012 2014 2017 2017 2009 2016
## [43330] 2007 2017 2016 2016 2009 2008 2009 2008 2015 2011 2012 2016 2015
## [43343] 2016 2016 2017 2007 2007 2017 2010 2016 2011 2016 2017 2014 2009
## [43356] 2009 2017 2008 2011 2011 2015 2017 2017 2014 2017 2017 2010 2009
## [43369] 2009 2007 2008 2015 2016 2009 2012 2016 2007 2007 2016 2017 2011
## [43382] 2016 2009 2012 2013 2013 2014 2009 2015 2016 2017 2009 2014 2015
## [43395] 2017 2013 2007 2010 2009 2016 2015 2015 2015 2017 2015 2017 2009
## [43408] 2014 2016 2017 2008 2007 2014 2016 2017 2011 2015 2016 2015 2015
## [43421] 2015 2008 2014 2015 2017 2017 2007 2008 2010 2013 2016 2015 2009
## [43434] 2014 2016 2016 2017 2016 2015 2015 2014 2017 2012 2007 2016 2010
## [43447] 2015 2009 2015 2012 2009 2017 2016 2010 2007 2012 2011 2013 2015
## [43460] 2015 2008 2017 2010 2017 2011 2016 2017 2017 2017 2007 2017 2010
## [43473] 2007 2015 2015 2007 2016 2016 2015 2016 2016 2017 2009 2010 2010
## [43486] 2010 2010 2012 2013 2014 2009 2017 2008 2010 2015 2016 2016 2016
## [43499] 2017 2017 2010 2012 2009 2015 2017 2017 2015 2015 2011 2012 2009
## [43512] 2010 2009 2014 2015 2017 2011 2013 2013 2016 2008 2015 2016 2010
## [43525] 2017 2012 2008 2010 2013 2015 2008 2010 2014 2012 2007 2015 2016
## [43538] 2007 2016 2017 2012 2014 2011 2008 2012 2016 2011 2012 2010 2014
## [43551] 2016 2016 2011 2016 2016 2017 2008 2008 2013 2014 2016 2017 2017
## [43564] 2010 2016 2016 2008 2014 2016 2016 2009 2011 2013 2015 2012 2012
## [43577] 2013 2016 2015 2016 2009 2016 2007 2015 2017 2008 2016 2015 2016
## [43590] 2007 2012 2009 2013 2016 2013 2016 2011 2012 2016 2009 2011 2008
## [43603] 2011 2016 2007 2011 2008 2013 2015 2015 2008 2013 2015 2017 2007
## [43616] 2015 2016 2017 2015 2008 2010 2010 2016 2010 2009 2013 2007 2012
## [43629] 2015 2015 2016 2010 2016 2017 2013 2017 2011 2012 2008 2013 2014
## [43642] 2016 2017 2010 2015 2010 2009 2017 2015 2009 2009 2017 2015 2017
## [43655] 2009 2008 2008 2015 2016 2016 2015 2008 2015 2008 2014 2017 2012
## [43668] 2012 2017 2009 2017 2009 2012 2015 2015 2016 2007 2015 2016 2007
## [43681] 2015 2017 2013 2009 2017 2015 2017 2013 2015 2012 2013 2016 2014
## [43694] 2016 2010 2017 2017 2008 2011 2007 2013 2015 2015 2012 2011 2008
## [43707] 2016 2016 2016 2016 2016 2016 2011 2015 2016 2015 2016 2017 2013
## [43720] 2012 2007 2015 2016 2008 2007 2016 2007 2013 2015 2017 2017 2017
## [43733] 2008 2010 2010 2014 2016 2007 2008 2007 2016 2010 2015 2015 2016
## [43746] 2017 2017 2012 2016 2007 2015 2009 2016 2012 2013 2016 2016 2007
## [43759] 2017 2010 2009 2011 2007 2017 2007 2011 2015 2017 2007 2007 2009
## [43772] 2014 2017 2012 2014 2010 2011 2016 2007 2015 2012 2017 2008 2011
## [43785] 2014 2017 2014 2017 2008 2016 2015 2017 2007 2015 2016 2017 2017
## [43798] 2015 2016 2017 2017 2017 2016 2017 2009 2015 2014 2017 2009 2011
## [43811] 2008 2013 2013 2015 2016 2015 2014 2016 2015 2017 2017 2008 2008
## [43824] 2014 2016 2016 2011 2012 2007 2013 2014 2015 2017 2014 2007 2009
## [43837] 2015 2016 2016 2017 2015 2015 2017 2009 2011 2009 2015 2009 2008
## [43850] 2008 2016 2009 2016 2016 2010 2016 2017 2015 2010 2017 2017 2011
## [43863] 2016 2009 2012 2015 2011 2009 2013 2016 2012 2012 2014 2016 2017
## [43876] 2015 2012 2009 2016 2012 2010 2008 2015 2017 2012 2008 2007 2014
## [43889] 2016 2016 2017 2017 2012 2010 2015 2008 2016 2016 2010 2014 2016
## [43902] 2017 2017 2011 2007 2007 2010 2016 2017 2011 2009 2012 2014 2017
## [43915] 2016 2017 2016 2015 2012 2017 2010 2013 2015 2016 2010 2017 2017
## [43928] 2010 2015 2008 2011 2017 2016 2017 2012 2007 2016 2017 2017 2017
## [43941] 2009 2014 2009 2015 2013 2016 2016 2007 2013 2017 2013 2015 2016
## [43954] 2016 2017 2017 2007 2009 2016 2016 2015 2017 2016 2015 2008 2016
## [43967] 2017 2009 2015 2017 2013 2016 2017 2011 2017 2015 2016 2015 2011
## [43980] 2010 2009 2017 2017 2015 2016 2016 2011 2014 2016 2017 2015 2016
## [43993] 2010 2015 2017 2010 2014 2017 2017 2009 2016 2015 2016 2013 2016
## [44006] 2016 2007 2008 2007 2008 2010 2017 2011 2017 2017 2008 2011 2016
## [44019] 2017 2017 2017 2008 2008 2007 2008 2011 2014 2016 2009 2011 2012
## [44032] 2013 2012 2016 2017 2015 2012 2007 2015 2017 2012 2013 2015 2017
## [44045] 2015 2016 2017 2017 2008 2016 2015 2017 2013 2015 2016 2017 2009
## [44058] 2008 2007 2014 2014 2015 2017 2011 2013 2014 2015 2009 2009 2012
## [44071] 2014 2016 2015 2016 2015 2009 2015 2009 2009 2013 2016 2017 2009
## [44084] 2014 2015 2011 2014 2015 2008 2012 2015 2017 2012 2017 2017 2014
## [44097] 2016 2011 2015 2015 2007 2010 2011 2015 2007 2016 2017 2009 2016
## [44110] 2012 2014 2017 2012 2008 2008 2013 2016 2016 2017 2017 2008 2012
## [44123] 2013 2016 2017 2015 2011 2007 2017 2009 2008 2010 2015 2017 2015
## [44136] 2007 2017 2008 2017 2017 2015 2016 2016 2012 2010 2009 2017 2012
## [44149] 2007 2017 2016 2015 2017 2017 2015 2016 2017 2017 2017 2008 2008
## [44162] 2016 2017 2017 2017 2017 2009 2016 2016 2015 2012 2011 2017 2017
## [44175] 2012 2011 2017 2007 2016 2011 2007 2017 2016 2016 2015 2008 2016
## [44188] 2016 2017 2011 2013 2016 2017 2009 2008 2015 2016 2016 2015 2016
## [44201] 2016 2008 2017 2014 2015 2016 2017 2007 2015 2016 2017 2013 2014
## [44214] 2015 2017 2009 2017 2017 2008 2016 2010 2016 2016 2017 2011 2017
## [44227] 2012 2011 2016 2011 2010 2015 2016 2015 2017 2016 2012 2017 2008
## [44240] 2014 2014 2016 2016 2008 2010 2010 2017 2017 2007 2009 2017 2011
## [44253] 2012 2008 2016 2016 2016 2013 2014 2016 2016 2012 2015 2016 2017
## [44266] 2009 2009 2017 2008 2011 2014 2011 2007 2013 2011 2017 2017 2008
## [44279] 2013 2015 2016 2010 2010 2013 2016 2017 2017 2012 2009 2008 2015
## [44292] 2015 2013 2015 2008 2013 2016 2017 2017 2009 2007 2016 2008 2015
## [44305] 2016 2011 2012 2013 2015 2016 2017 2017 2010 2008 2011 2017 2015
## [44318] 2016 2017 2010 2007 2017 2015 2016 2017 2014 2015 2016 2011 2014
## [44331] 2016 2011 2009 2015 2016 2010 2015 2007 2015 2016 2017 2017 2017
## [44344] 2017 2010 2008 2016 2016 2009 2015 2016 2017 2016 2016 2007 2017
## [44357] 2017 2008 2007 2013 2015 2016 2013 2011 2007 2014 2016 2016 2016
## [44370] 2017 2008 2011 2015 2015 2016 2017 2015 2017 2016 2015 2017 2012
## [44383] 2010 2010 2016 2016 2008 2016 2017 2015 2017 2015 2015 2016 2016
## [44396] 2017 2012 2009 2013 2013 2010 2007 2016 2015 2015 2012 2013 2015
## [44409] 2017 2009 2012 2009 2016 2016 2017 2017 2016 2016 2016 2016 2015
## [44422] 2013 2014 2010 2010 2016 2017 2016 2017 2017 2012 2013 2008 2008
## [44435] 2013 2014 2016 2017 2017 2007 2016 2015 2008 2007 2014 2015 2017
## [44448] 2013 2013 2015 2015 2008 2016 2016 2017 2009 2015 2016 2016 2017
## [44461] 2017 2015 2017 2008 2016 2015 2017 2007 2009 2014 2014 2015 2016
## [44474] 2011 2009 2009 2009 2010 2014 2012 2007 2009 2017 2013 2015 2016
## [44487] 2012 2015 2015 2016 2015 2016 2017 2011 2014 2011 2008 2011 2008
## [44500] 2016 2016 2016 2016 2015 2012 2007 2017 2017 2010 2007 2015 2017
## [44513] 2007 2012 2016 2016 2015 2009 2011 2016 2017 2010 2017 2007 2014
## [44526] 2015 2017 2008 2011 2016 2016 2009 2008 2008 2010 2009 2017 2017
## [44539] 2009 2016 2016 2008 2016 2014 2015 2014 2011 2014 2012 2012 2013
## [44552] 2014 2017 2012 2011 2017 2016 2016 2014 2016 2007 2017 2017 2011
## [44565] 2013 2015 2016 2007 2011 2009 2009 2010 2008 2016 2017 2016 2016
## [44578] 2015 2016 2007 2010 2007 2015 2016 2011 2016 2017 2009 2008 2015
## [44591] 2016 2008 2016 2017 2009 2015 2016 2016 2016 2012 2015 2010 2015
## [44604] 2015 2015 2016 2017 2017 2017 2015 2008 2008 2010 2016 2016 2007
## [44617] 2008 2008 2013 2017 2008 2013 2016 2017 2016 2016 2017 2015 2017
## [44630] 2009 2016 2017 2017 2015 2017 2008 2015 2015 2010 2016 2017 2013
## [44643] 2015 2017 2011 2011 2014 2016 2016 2017 2011 2007 2016 2016 2017
## [44656] 2012 2015 2016 2017 2013 2013 2016 2017 2017 2011 2017 2017 2009
## [44669] 2015 2007 2016 2017 2009 2015 2016 2016 2010 2012 2011 2011 2012
## [44682] 2015 2015 2016 2016 2016 2009 2010 2013 2016 2016 2016 2007 2014
## [44695] 2016 2008 2015 2017 2009 2011 2008 2011 2009 2015 2015 2007 2012
## [44708] 2016 2009 2015 2015 2016 2017 2013 2015 2008 2016 2017 2009 2010
## [44721] 2014 2015 2014 2016 2017 2007 2015 2017 2011 2016 2007 2013 2017
## [44734] 2007 2008 2017 2012 2016 2017 2016 2015 2017 2015 2011 2015 2015
## [44747] 2009 2015 2008 2014 2017 2017 2017 2013 2017 2017 2014 2015 2015
## [44760] 2008 2011 2010 2016 2017 2017 2011 2015 2016 2010 2014 2015 2016
## [44773] 2015 2017 2015 2017 2014 2014 2015 2017 2007 2007 2015 2016 2017
## [44786] 2008 2015 2015 2017 2010 2015 2017 2013 2016 2017 2017 2007 2008
## [44799] 2015 2016 2017 2013 2016 2017 2007 2012 2012 2015 2016 2017 2017
## [44812] 2017 2017 2009 2017 2016 2016 2007 2012 2007 2016 2008 2015 2014
## [44825] 2017 2017 2014 2017 2008 2007 2013 2015 2009 2015 2017 2013 2017
## [44838] 2015 2012 2014 2016 2016 2007 2007 2013 2017 2015 2016 2017 2013
## [44851] 2014 2015 2015 2015 2015 2008 2008 2009 2007 2013 2017 2017 2012
## [44864] 2015 2016 2010 2016 2017 2015 2017 2014 2012 2017 2017 2010 2011
## [44877] 2013 2016 2017 2017 2010 2010 2016 2017 2017 2017 2011 2007 2013
## [44890] 2008 2012 2008 2009 2010 2010 2016 2017 2011 2009 2013 2014 2015
## [44903] 2015 2016 2011 2015 2017 2017 2012 2010 2016 2008 2014 2015 2016
## [44916] 2016 2017 2011 2013 2016 2017 2015 2016 2017 2008 2016 2017 2017
## [44929] 2015 2017 2011 2011 2016 2016 2009 2009 2016 2015 2008 2016 2014
## [44942] 2016 2016 2007 2015 2016 2015 2017 2009 2016 2017 2015 2016 2017
## [44955] 2007 2015 2015 2017 2012 2008 2013 2012 2010 2013 2015 2012 2015
## [44968] 2010 2015 2016 2015 2007 2012 2009 2011 2008 2013 2015 2008 2014
## [44981] 2015 2007 2007 2015 2017 2014 2017 2012 2017 2010 2008 2017 2017
## [44994] 2007 2011 2015 2017 2016 2008 2013 2016 2016 2007 2013 2015 2016
## [45007] 2016 2017 2014 2010 2009 2011 2015 2009 2013 2015 2017 2017 2009
## [45020] 2007 2016 2007 2015 2017 2010 2009 2015 2014 2008 2013 2015 2017
## [45033] 2015 2016 2013 2015 2008 2014 2016 2008 2015 2012 2014 2016 2011
## [45046] 2015 2013 2015 2015 2008 2013 2015 2017 2017 2010 2009 2012 2015
## [45059] 2015 2013 2017 2015 2016 2009 2015 2016 2008 2008 2017 2007 2015
## [45072] 2014 2007 2012 2008 2013 2015 2008 2015 2015 2016 2016 2009 2008
## [45085] 2009 2016 2007 2009 2010 2014 2016 2010 2016 2017 2007 2011 2007
## [45098] 2016 2016 2017 2009 2009 2016 2017 2016 2017 2010 2016 2017 2015
## [45111] 2010 2016 2017 2011 2015 2016 2012 2016 2011 2016 2016 2016 2015
## [45124] 2012 2007 2014 2017 2009 2016 2017 2011 2011 2017 2010 2009 2016
## [45137] 2016 2016 2017 2010 2013 2017 2007 2015 2011 2015 2016 2010 2009
## [45150] 2016 2011 2008 2007 2017 2016 2015 2016 2016 2012 2014 2017 2012
## [45163] 2010 2015 2008 2013 2017 2011 2009 2012 2009 2008 2017 2016 2017
## [45176] 2009 2008 2009 2008 2010 2015 2016 2016 2017 2007 2017 2007 2011
## [45189] 2014 2015 2007 2015 2017 2017 2014 2015 2015 2016 2011 2007 2016
## [45202] 2016 2007 2015 2016 2016 2017 2009 2009 2017 2012 2014 2015 2012
## [45215] 2014 2017 2017 2017 2012 2008 2013 2014 2008 2013 2010 2014 2016
## [45228] 2016 2017 2017 2017 2009 2012 2007 2016 2017 2007 2016 2016 2009
## [45241] 2016 2008 2016 2014 2015 2017 2016 2015 2017 2017 2010 2015 2008
## [45254] 2007 2017 2007 2010 2014 2015 2009 2012 2016 2017 2009 2013 2014
## [45267] 2012 2013 2015 2016 2009 2015 2016 2017 2011 2007 2009 2015 2016
## [45280] 2015 2017 2011 2007 2010 2015 2016 2011 2015 2016 2015 2017 2007
## [45293] 2012 2017 2016 2012 2007 2007 2016 2017 2015 2016 2007 2016 2017
## [45306] 2010 2017 2016 2010 2009 2007 2015 2015 2010 2015 2016 2011 2017
## [45319] 2017 2017 2011 2009 2014 2017 2011 2010 2014 2015 2016 2011 2013
## [45332] 2014 2015 2017 2017 2007 2014 2016 2015 2010 2010 2015 2016 2009
## [45345] 2007 2013 2014 2015 2017 2010 2016 2016 2017 2011 2015 2015 2016
## [45358] 2017 2017 2016 2015 2009 2015 2017 2017 2017 2011 2013 2014 2017
## [45371] 2017 2011 2015 2017 2011 2014 2015 2016 2016 2017 2016 2015 2010
## [45384] 2009 2016 2015 2017 2016 2017 2009 2016 2016 2015 2017 2010 2012
## [45397] 2013 2016 2008 2008 2017 2015 2016 2016 2016 2017 2008 2015 2008
## [45410] 2016 2008 2016 2012 2015 2016 2016 2015 2011 2016 2016 2017 2007
## [45423] 2014 2016 2017 2017 2009 2011 2017 2015 2017 2017 2007 2012 2011
## [45436] 2010 2009 2016 2016 2017 2007 2015 2014 2011 2013 2015 2016 2016
## [45449] 2011 2012 2010 2017 2017 2015 2017 2017 2007 2012 2007 2016 2017
## [45462] 2016 2012 2017 2010 2009 2013 2015 2015 2017 2014 2017 2011 2011
## [45475] 2014 2016 2015 2009 2007 2014 2016 2016 2008 2011 2016 2016 2017
## [45488] 2014 2014 2016 2017 2017 2017 2017 2016 2017 2008 2009 2011 2016
## [45501] 2009 2015 2015 2017 2011 2009 2015 2015 2008 2009 2015 2015 2015
## [45514] 2015 2016 2016 2009 2007 2017 2017 2008 2011 2011 2010 2016 2016
## [45527] 2007 2011 2015 2011 2015 2015 2017 2014 2016 2017 2008 2011 2015
## [45540] 2009 2012 2011 2009 2009 2017 2016 2017 2010 2013 2016 2017 2008
## [45553] 2013 2017 2017 2009 2007 2007 2016 2016 2017 2012 2017 2017 2017
## [45566] 2011 2017 2017 2009 2013 2016 2009 2015 2016 2010 2007 2009 2011
## [45579] 2007 2016 2015 2017 2017 2017 2010 2015 2016 2015 2016 2016 2008
## [45592] 2010 2012 2008 2015 2016 2012 2011 2014 2014 2017 2016 2017 2007
## [45605] 2009 2010 2016 2009 2011 2016 2017 2013 2017 2011 2007 2010 2017
## [45618] 2017 2007 2009 2015 2015 2014 2017 2014 2016 2016 2017 2013 2011
## [45631] 2014 2016 2015 2017 2016 2017 2009 2012 2007 2007 2017 2007 2008
## [45644] 2014 2007 2012 2009 2007 2010 2013 2016 2016 2017 2010 2014 2016
## [45657] 2017 2016 2017 2007 2015 2014 2017 2014 2016 2007 2017 2017 2011
## [45670] 2015 2016 2017 2014 2016 2017 2017 2016 2017 2012 2017 2017 2014
## [45683] 2012 2008 2013 2015 2015 2010 2009 2012 2007 2010 2014 2016 2017
## [45696] 2007 2016 2017 2017 2016 2017 2011 2015 2017 2007 2007 2013 2015
## [45709] 2017 2016 2009 2014 2011 2016 2017 2007 2007 2014 2016 2017 2014
## [45722] 2015 2017 2010 2007 2007 2008 2014 2011 2010 2011 2015 2016 2011
## [45735] 2013 2014 2014 2017 2012 2015 2015 2017 2015 2015 2015 2016 2013
## [45748] 2016 2008 2015 2016 2016 2011 2013 2014 2017 2015 2010 2007 2015
## [45761] 2008 2014 2016 2008 2008 2010 2015 2015 2017 2017 2015 2016 2015
## [45774] 2017 2010 2012 2013 2015 2016 2016 2013 2016 2017 2008 2011 2009
## [45787] 2011 2016 2016 2017 2007 2015 2017 2015 2016 2008 2013 2015 2016
## [45800] 2016 2017 2017 2016 2017 2015 2010 2011 2016 2016 2015 2010 2015
## [45813] 2017 2010 2008 2014 2011 2008 2017 2016 2010 2017 2012 2017 2017
## [45826] 2017 2017 2007 2009 2017 2017 2016 2017 2009 2016 2010 2008 2013
## [45839] 2015 2011 2009 2014 2015 2016 2017 2015 2017 2012 2014 2011 2017
## [45852] 2017 2017 2016 2016 2013 2015 2017 2013 2015 2011 2011 2012 2015
## [45865] 2016 2017 2014 2007 2017 2017 2011 2007 2010 2008 2014 2016 2016
## [45878] 2011 2016 2017 2009 2013 2015 2017 2015 2015 2016 2015 2007 2015
## [45891] 2016 2008 2011 2009 2009 2011 2016 2017 2015 2015 2016 2010 2009
## [45904] 2017 2010 2014 2017 2009 2009 2011 2012 2012 2016 2016 2011 2009
## [45917] 2009 2011 2014 2016 2014 2015 2015 2017 2017 2015 2015 2017 2017
## [45930] 2017 2017 2008 2016 2017 2017 2010 2017 2017 2011 2012 2015 2016
## [45943] 2017 2017 2017 2012 2017 2011 2016 2017 2009 2014 2015 2016 2017
## [45956] 2017 2009 2015 2017 2014 2012 2010 2014 2017 2014 2017 2014 2015
## [45969] 2009 2011 2012 2016 2017 2008 2016 2017 2012 2007 2015 2015 2016
## [45982] 2015 2011 2017 2009 2009 2011 2016 2016 2008 2007 2014 2015 2017
## [45995] 2017 2010 2013 2015 2017 2009 2015 2016 2012 2016 2016 2009 2016
## [46008] 2016 2009 2007 2014 2016 2016 2008 2008 2016 2007 2011 2016 2012
## [46021] 2014 2015 2011 2016 2017 2017 2017 2009 2016 2014 2010 2011 2014
## [46034] 2007 2016 2008 2016 2011 2009 2017 2007 2016 2016 2017 2013 2015
## [46047] 2017 2008 2014 2016 2010 2014 2015 2017 2014 2015 2015 2015 2015
## [46060] 2008 2015 2016 2016 2016 2017 2007 2015 2017 2009 2016 2017 2009
## [46073] 2011 2009 2016 2015 2015 2007 2012 2014 2016 2015 2011 2015 2016
## [46086] 2017 2008 2010 2016 2017 2010 2017 2009 2008 2015 2017 2012 2013
## [46099] 2017 2008 2015 2017 2011 2016 2011 2017 2011 2012 2013 2013 2014
## [46112] 2012 2007 2015 2008 2010 2014 2012 2015 2016 2017 2016 2007 2017
## [46125] 2009 2014 2015 2017 2013 2014 2016 2015 2010 2015 2017 2011 2015
## [46138] 2015 2016 2008 2008 2010 2016 2012 2011 2015 2010 2007 2009 2015
## [46151] 2017 2008 2010 2014 2016 2017 2016 2016 2010 2008 2012 2013 2014
## [46164] 2016 2015 2008 2017 2010 2007 2010 2011 2007 2011 2012 2010 2014
## [46177] 2015 2016 2016 2009 2015 2010 2017 2012 2015 2016 2015 2017 2013
## [46190] 2017 2012 2009 2016 2016 2015 2017 2017 2012 2008 2014 2016 2008
## [46203] 2008 2016 2017 2008 2016 2009 2016 2010 2016 2016 2015 2014 2015
## [46216] 2010 2016 2008 2010 2008 2017 2010 2015 2016 2017 2017 2010 2015
## [46229] 2009 2017 2016 2016 2017 2011 2017 2017 2011 2016 2017 2010 2017
## [46242] 2017 2007 2016 2017 2017 2011 2016 2017 2007 2015 2012 2009 2017
## [46255] 2013 2016 2016 2015 2012 2007 2010 2015 2016 2015 2017 2011 2011
## [46268] 2016 2008 2012 2016 2009 2015 2015 2016 2017 2017 2012 2010 2016
## [46281] 2008 2012 2014 2017 2007 2007 2008 2015 2008 2010 2017 2011 2016
## [46294] 2015 2016 2016 2017 2015 2017 2016 2011 2007 2009 2016 2016 2012
## [46307] 2009 2016 2016 2009 2011 2016 2016 2017 2009 2015 2015 2015 2017
## [46320] 2017 2015 2017 2016 2009 2017 2008 2011 2011 2015 2015 2010 2014
## [46333] 2009 2012 2014 2015 2017 2013 2017 2011 2015 2016 2014 2017 2008
## [46346] 2015 2016 2016 2010 2010 2017 2011 2016 2017 2016 2016 2016 2011
## [46359] 2013 2013 2012 2010 2016 2017 2014 2016 2017 2016 2016 2015 2016
## [46372] 2015 2016 2017 2017 2017 2017 2015 2009 2009 2016 2016 2007 2015
## [46385] 2012 2017 2015 2015 2010 2017 2017 2017 2010 2011 2013 2016 2016
## [46398] 2010 2016 2017 2007 2007 2015 2012 2017 2007 2011 2016 2017 2015
## [46411] 2014 2016 2017 2017 2015 2013 2013 2013 2016 2017 2009 2017 2017
## [46424] 2017 2008 2009 2013 2015 2016 2017 2013 2015 2017 2008 2016 2009
## [46437] 2016 2017 2015 2015 2015 2016 2017 2015 2016 2017 2010 2010 2012
## [46450] 2015 2016 2017 2016 2011 2011 2014 2016 2016 2017 2010 2009 2015
## [46463] 2016 2017 2017 2015 2015 2017 2008 2011 2015 2017 2010 2007 2017
## [46476] 2008 2011 2010 2008 2015 2017 2013 2013 2010 2016 2015 2016 2017
## [46489] 2015 2015 2017 2015 2012 2013 2016 2009 2013 2016 2014 2010 2012
## [46502] 2009 2011 2015 2015 2015 2017 2011 2010 2013 2015 2012 2017 2017
## [46515] 2009 2011 2015 2015 2008 2009 2008 2016 2017 2017 2017 2014 2017
## [46528] 2013 2016 2009 2017 2015 2009 2010 2010 2015 2015 2016 2016 2015
## [46541] 2015 2015 2015 2015 2007 2015 2009 2010 2010 2008 2014 2016 2011
## [46554] 2009 2013 2014 2017 2012 2007 2016 2016 2016 2016 2014 2016 2016
## [46567] 2015 2014 2017 2015 2016 2017 2012 2015 2010 2009 2010 2014 2015
## [46580] 2010 2016 2016 2017 2007 2012 2008 2015 2012 2014 2017 2007 2008
## [46593] 2015 2016 2012 2013 2015 2009 2015 2007 2014 2014 2013 2015 2016
## [46606] 2012 2016 2014 2016 2017 2012 2011 2014 2014 2015 2015 2013 2015
## [46619] 2017 2008 2016 2017 2007 2017 2008 2008 2009 2015 2016 2016 2017
## [46632] 2015 2008 2016 2012 2008 2013 2015 2017 2011 2016 2016 2016 2015
## [46645] 2008 2015 2015 2017 2014 2015 2015 2015 2017 2011 2017 2017 2017
## [46658] 2013 2015 2017 2016 2017 2007 2015 2011 2015 2015 2015 2017 2013
## [46671] 2011 2011 2016 2010 2010 2015 2013 2015 2011 2011 2015 2016 2017
## [46684] 2017 2011 2015 2007 2012 2008 2013 2016 2016 2017 2017 2009 2016
## [46697] 2008 2008 2017 2015 2014 2016 2016 2009 2010 2007 2014 2015 2015
## [46710] 2015 2016 2007 2016 2016 2016 2015 2015 2010 2016 2009 2008 2011
## [46723] 2013 2009 2012 2015 2010 2008 2015 2012 2016 2017 2017 2011 2007
## [46736] 2007 2012 2017 2009 2010 2008 2017 2011 2014 2016 2017 2017 2016
## [46749] 2007 2015 2015 2016 2012 2007 2016 2008 2016 2016 2017 2014 2017
## [46762] 2014 2017 2014 2015 2016 2016 2007 2008 2008 2015 2012 2009 2014
## [46775] 2007 2015 2011 2012 2009 2016 2016 2015 2009 2013 2017 2009 2007
## [46788] 2015 2013 2016 2010 2007 2015 2015 2016 2017 2009 2011 2007 2007
## [46801] 2015 2015 2017 2014 2014 2016 2017 2017 2010 2013 2016 2008 2016
## [46814] 2017 2017 2017 2015 2008 2007 2016 2015 2015 2015 2015 2016 2016
## [46827] 2015 2017 2017 2017 2009 2015 2016 2011 2013 2016 2016 2009 2012
## [46840] 2012 2017 2010 2007 2009 2011 2015 2017 2015 2017 2011 2007 2009
## [46853] 2016 2017 2009 2016 2017 2017 2017 2015 2016 2014 2016 2017 2015
## [46866] 2008 2016 2016 2017 2017 2008 2014 2015 2008 2008 2016 2015 2015
## [46879] 2017 2008 2014 2014 2015 2008 2016 2016 2017 2017 2014 2015 2016
## [46892] 2016 2010 2017 2017 2010 2012 2014 2015 2017 2016 2008 2011 2013
## [46905] 2015 2017 2013 2015 2008 2017 2011 2013 2014 2016 2012 2016 2015
## [46918] 2009 2008 2017 2013 2008 2015 2015 2017 2016 2016 2016 2012 2009
## [46931] 2011 2015 2016 2016 2008 2012 2014 2015 2016 2016 2016 2007 2017
## [46944] 2008 2017 2015 2012 2013 2015 2016 2017 2009 2017 2013 2017 2012
## [46957] 2015 2016 2017 2017 2007 2012 2009 2013 2008 2015 2007 2007 2014
## [46970] 2015 2017 2011 2011 2008 2016 2017 2016 2011 2010 2016 2017 2014
## [46983] 2015 2017 2009 2014 2017 2017 2017 2009 2012 2017 2012 2009 2015
## [46996] 2016 2017 2015 2007 2012 2012 2017 2015 2007 2015 2017 2017 2015
## [47009] 2016 2015 2012 2009 2016 2017 2012 2016 2015 2014 2007 2014 2015
## [47022] 2008 2015 2017 2015 2017 2017 2017 2012 2015 2016 2016 2017 2012
## [47035] 2007 2015 2017 2008 2015 2007 2014 2017 2016 2017 2008 2008 2015
## [47048] 2016 2017 2015 2016 2016 2015 2008 2008 2014 2012 2009 2017 2008
## [47061] 2010 2016 2016 2016 2016 2016 2017 2017 2014 2017 2017 2016 2016
## [47074] 2012 2016 2015 2016 2017 2015 2015 2017 2008 2015 2017 2017 2010
## [47087] 2013 2008 2013 2008 2016 2009 2008 2015 2016 2010 2014 2015 2011
## [47100] 2016 2017 2016 2016 2015 2017 2015 2016 2017 2016 2016 2015 2017
## [47113] 2008 2011 2016 2016 2017 2017 2008 2007 2010 2013 2015 2015 2016
## [47126] 2009 2013 2016 2015 2015 2017 2017 2009 2016 2017 2015 2015 2016
## [47139] 2011 2015 2009 2009 2008 2016 2016 2015 2016 2017 2017 2017 2012
## [47152] 2010 2014 2016 2017 2008 2017 2007 2008 2013 2008 2013 2016 2017
## [47165] 2017 2007 2017 2010 2015 2015 2017 2016 2009 2014 2016 2008 2012
## [47178] 2017 2012 2007 2017 2007 2016 2012 2016 2017 2008 2008 2011 2010
## [47191] 2014 2007 2009 2015 2017 2016 2016 2017 2008 2017 2012 2015 2016
## [47204] 2017 2009 2015 2014 2017 2017 2017 2010 2008 2016 2012 2015 2015
## [47217] 2016 2017 2012 2012 2013 2017 2017 2010 2014 2014 2010 2016 2016
## [47230] 2010 2012 2008 2015 2016 2016 2012 2010 2010 2007 2007 2011 2014
## [47243] 2014 2016 2017 2012 2016 2017 2007 2017 2009 2017 2017 2009 2017
## [47256] 2010 2012 2016 2007 2015 2013 2015 2016 2017 2010 2016 2017 2009
## [47269] 2009 2015 2017 2016 2014 2015 2015 2016 2015 2015 2016 2014 2016
## [47282] 2014 2015 2009 2013 2016 2008 2015 2016 2015 2015 2010 2007 2015
## [47295] 2016 2009 2016 2017 2008 2009 2009 2012 2014 2015 2015 2017 2017
## [47308] 2011 2016 2012 2016 2010 2008 2015 2016 2016 2017 2010 2009 2014
## [47321] 2014 2014 2016 2017 2008 2016 2017 2009 2012 2015 2009 2007 2016
## [47334] 2016 2008 2014 2015 2017 2017 2011 2013 2016 2017 2017 2008 2017
## [47347] 2017 2012 2014 2015 2016 2017 2007 2015 2015 2016 2008 2010 2010
## [47360] 2017 2016 2015 2015 2016 2007 2014 2015 2017 2012 2015 2016 2013
## [47373] 2016 2017 2015 2011 2009 2011 2012 2016 2013 2015 2016 2017 2015
## [47386] 2016 2016 2008 2011 2015 2011 2015 2010 2015 2017 2014 2015 2015
## [47399] 2016 2007 2014 2014 2016 2009 2008 2015 2013 2015 2015 2017 2008
## [47412] 2017 2008 2007 2010 2015 2015 2012 2009 2007 2009 2015 2017 2015
## [47425] 2010 2011 2015 2008 2012 2009 2012 2016 2016 2015 2007 2012 2015
## [47438] 2016 2017 2009 2013 2015 2016 2016 2008 2007 2016 2015 2017 2010
## [47451] 2008 2008 2016 2017 2007 2010 2011 2010 2013 2017 2015 2016 2015
## [47464] 2009 2014 2012 2008 2016 2013 2015 2017 2017 2008 2011 2017 2012
## [47477] 2012 2017 2015 2016 2008 2016 2017 2017 2017 2015 2007 2014 2016
## [47490] 2016 2009 2010 2007 2016 2017 2017 2009 2009 2007 2017 2011 2016
## [47503] 2016 2016 2016 2016 2017 2011 2013 2016 2007 2016 2016 2013 2017
## [47516] 2008 2013 2016 2016 2016 2017 2013 2010 2009 2012 2015 2010 2015
## [47529] 2012 2017 2016 2017 2017 2008 2015 2014 2015 2016 2017 2016 2012
## [47542] 2010 2015 2015 2016 2017 2016 2016 2008 2007 2015 2015 2016 2016
## [47555] 2007 2016 2017 2012 2009 2017 2016 2017 2017 2013 2016 2017 2012
## [47568] 2016 2016 2017 2010 2017 2007 2010 2015 2015 2016 2017 2017 2016
## [47581] 2017 2007 2008 2013 2016 2007 2008 2014 2010 2007 2010 2016 2010
## [47594] 2010 2007 2017 2017 2009 2015 2015 2010 2013 2016 2010 2011 2013
## [47607] 2014 2017 2008 2010 2015 2017 2017 2017 2007 2007 2017 2016 2017
## [47620] 2010 2015 2007 2010 2013 2015 2016 2017 2017 2017 2009 2015 2017
## [47633] 2008 2014 2017 2017 2011 2016 2015 2008 2015 2007 2016 2016 2015
## [47646] 2016 2017 2015 2007 2017 2011 2010 2008 2014 2017 2008 2016 2017
## [47659] 2008 2007 2015 2008 2012 2009 2015 2008 2007 2012 2015 2016 2015
## [47672] 2008 2013 2016 2016 2008 2007 2016 2011 2008 2010 2017 2017 2012
## [47685] 2010 2017 2016 2015 2010 2009 2017 2007 2008 2011 2014 2017 2008
## [47698] 2016 2016 2010 2013 2015 2015 2017 2015 2011 2016 2017 2017 2017
## [47711] 2015 2017 2017 2014 2015 2016 2016 2015 2007 2016 2016 2017 2017
## [47724] 2017 2008 2008 2015 2007 2008 2013 2016 2017 2008 2016 2016 2011
## [47737] 2014 2016 2011 2014 2016 2017 2015 2010 2008 2008 2014 2007 2009
## [47750] 2009 2011 2016 2015 2017 2015 2007 2015 2015 2008 2010 2015 2012
## [47763] 2013 2016 2011 2008 2015 2008 2016 2007 2016 2017 2017 2016 2012
## [47776] 2017 2016 2015 2016 2017 2015 2015 2017 2012 2007 2015 2016 2017
## [47789] 2017 2017 2015 2011 2015 2015 2009 2013 2015 2016 2017 2012 2016
## [47802] 2011 2010 2007 2016 2012 2017 2010 2016 2016 2016 2015 2016 2017
## [47815] 2010 2008 2008 2012 2016 2015 2010 2008 2015 2017 2008 2011 2014
## [47828] 2017 2017 2007 2016 2015 2016 2010 2015 2017 2015 2010 2015 2010
## [47841] 2014 2014 2015 2016 2015 2017 2015 2017 2015 2017 2007 2009 2010
## [47854] 2015 2015 2016 2017 2007 2012 2015 2017 2014 2016 2008 2013 2016
## [47867] 2009 2014 2016 2016 2015 2015 2008 2011 2015 2011 2015 2009 2016
## [47880] 2010 2016 2015 2012 2017 2016 2017 2015 2017 2012 2017 2016 2015
## [47893] 2017 2017 2017 2008 2007 2009 2008 2015 2017 2017 2007 2016 2017
## [47906] 2014 2011 2008 2014 2016 2017 2007 2010 2009 2016 2007 2016 2017
## [47919] 2008 2013 2015 2016 2016 2010 2010 2015 2015 2017 2012 2016 2017
## [47932] 2008 2008 2007 2012 2016 2010 2013 2014 2008 2011 2015 2017 2009
## [47945] 2012 2013 2014 2013 2015 2015 2015 2016 2015 2017 2010 2013 2016
## [47958] 2012 2007 2015 2016 2011 2012 2014 2015 2016 2016 2012 2008 2011
## [47971] 2013 2013 2013 2015 2016 2007 2013 2014 2017 2015 2013 2016 2016
## [47984] 2016 2015 2008 2015 2016 2016 2011 2013 2014 2017 2009 2009 2016
## [47997] 2010 2016 2017 2017 2017 2010 2009 2017 2008 2012 2014 2017 2008
## [48010] 2017 2017 2008 2015 2016 2011 2016 2017 2008 2013 2016 2016 2016
## [48023] 2017 2008 2016 2017 2010 2014 2008 2016 2017 2010 2012 2016 2016
## [48036] 2008 2014 2014 2010 2015 2017 2010 2013 2016 2013 2016 2017 2017
## [48049] 2014 2016 2012 2016 2016 2016 2017 2008 2011 2008 2008 2012 2017
## [48062] 2007 2011 2016 2016 2009 2007 2016 2017 2011 2010 2016 2012 2013
## [48075] 2014 2016 2010 2016 2016 2017 2017 2014 2016 2016 2016 2017 2014
## [48088] 2016 2012 2015 2016 2012 2016 2015 2017 2008 2010 2015 2016 2016
## [48101] 2007 2013 2017 2017 2017 2017 2007 2011 2017 2015 2016 2017 2009
## [48114] 2015 2012 2013 2016 2016 2017 2017 2015 2016 2008 2016 2017 2009
## [48127] 2015 2017 2016 2008 2012 2010 2017 2013 2017 2016 2017 2008 2014
## [48140] 2013 2015 2007 2013 2014 2015 2015 2009 2007 2011 2016 2016 2010
## [48153] 2014 2014 2016 2015 2016 2011 2014 2016 2016 2013 2015 2015 2015
## [48166] 2016 2017 2012 2015 2016 2012 2015 2017 2014 2016 2017 2015 2016
## [48179] 2015 2017 2012 2012 2015 2011 2011 2011 2009 2008 2010 2017 2012
## [48192] 2010 2008 2016 2007 2011 2007 2010 2015 2016 2017 2017 2008 2011
## [48205] 2010 2008 2017 2015 2016 2017 2011 2015 2009 2011 2014 2016 2017
## [48218] 2017 2012 2008 2016 2015 2016 2015 2013 2016 2011 2015 2014 2016
## [48231] 2017 2017 2015 2010 2016 2017 2017 2012 2017 2012 2007 2017 2013
## [48244] 2015 2015 2014 2016 2015 2010 2016 2017 2008 2008 2007 2015 2017
## [48257] 2007 2010 2011 2016 2012 2009 2015 2016 2010 2017 2008 2016 2016
## [48270] 2017 2017 2014 2015 2016 2011 2011 2016 2016 2016 2015 2016 2015
## [48283] 2016 2017 2015 2017 2014 2013 2015 2016 2016 2015 2012 2014 2014
## [48296] 2016 2015 2010 2017 2015 2013 2016 2017 2009 2015 2007 2010 2015
## [48309] 2016 2016 2017 2011 2017 2015 2015 2009 2013 2016 2009 2012 2011
## [48322] 2017 2017 2009 2015 2017 2017 2009 2012 2016 2012 2016 2010 2015
## [48335] 2016 2008 2012 2008 2012 2015 2016 2016 2016 2017 2017 2012 2017
## [48348] 2017 2008 2013 2014 2014 2014 2010 2007 2014 2016 2017 2017 2007
## [48361] 2015 2013 2013 2012 2017 2017 2017 2009 2008 2017 2007 2011 2014
## [48374] 2016 2016 2008 2016 2016 2017 2015 2015 2017 2017 2012 2010 2016
## [48387] 2017 2008 2015 2016 2016 2017 2012 2013 2013 2015 2014 2015 2009
## [48400] 2009 2008 2008 2015 2017 2011 2011 2016 2017 2007 2013 2015 2007
## [48413] 2015 2017 2011 2007 2013 2016 2007 2015 2016 2015 2017 2017 2007
## [48426] 2009 2016 2016 2007 2016 2011 2011 2016 2017 2017 2009 2015 2016
## [48439] 2007 2007 2014 2017 2007 2016 2016 2015 2017 2010 2016 2015 2011
## [48452] 2011 2008 2011 2008 2013 2016 2016 2013 2015 2010 2012 2008 2015
## [48465] 2016 2017 2009 2016 2015 2017 2008 2016 2010 2012 2015 2010 2014
## [48478] 2010 2013 2014 2017 2017 2017 2007 2013 2013 2013 2014 2015 2016
## [48491] 2011 2014 2017 2013 2015 2016 2016 2017 2009 2016 2015 2007 2015
## [48504] 2012 2008 2016 2016 2017 2015 2010 2015 2015 2016 2009 2008 2017
## [48517] 2017 2008 2012 2017 2007 2011 2011 2017 2017 2008 2008 2011 2013
## [48530] 2017 2010 2010 2015 2015 2017 2007 2015 2017 2017 2016 2013 2008
## [48543] 2009 2015 2016 2016 2017 2017 2015 2017 2010 2011 2008 2010 2015
## [48556] 2015 2012 2009 2013 2013 2011 2015 2013 2015 2017 2007 2009 2016
## [48569] 2015 2017 2008 2007 2016 2016 2016 2007 2008 2015 2016 2013 2013
## [48582] 2012 2014 2010 2009 2012 2007 2017 2013 2015 2015 2017 2017 2017
## [48595] 2016 2017 2008 2015 2017 2008 2010 2013 2014 2015 2017 2017 2011
## [48608] 2007 2015 2017 2015 2011 2008 2015 2016 2012 2012 2017 2011 2016
## [48621] 2017 2017 2017 2014 2015 2016 2012 2015 2017 2015 2017 2017 2011
## [48634] 2015 2016 2016 2016 2011 2016 2016 2017 2013 2015 2016 2014 2016
## [48647] 2014 2015 2017 2008 2011 2009 2008 2016 2017 2014 2007 2010 2010
## [48660] 2017 2010 2012 2015 2016 2016 2017 2010 2014 2016 2016 2017 2008
## [48673] 2013 2017 2017 2017 2012 2010 2009 2014 2015 2012 2016 2015 2017
## [48686] 2008 2015 2017 2012 2016 2009 2009 2017 2009 2009 2010 2017 2017
## [48699] 2016 2015 2007 2007 2007 2017 2008 2017 2015 2017 2015 2010 2016
## [48712] 2016 2015 2016 2016 2016 2008 2015 2015 2017 2014 2016 2012 2015
## [48725] 2017 2010 2012 2016 2007 2007 2017 2008 2014 2017 2014 2016 2007
## [48738] 2010 2016 2016 2017 2012 2013 2017 2017 2017 2007 2009 2015 2016
## [48751] 2017 2017 2011 2015 2017 2017 2009 2011 2008 2009 2015 2007 2011
## [48764] 2009 2008 2007 2017 2014 2015 2016 2011 2017 2011 2016 2008 2013
## [48777] 2014 2015 2016 2013 2016 2008 2015 2016 2012 2017 2010 2015 2016
## [48790] 2015 2012 2015 2016 2017 2015 2015 2007 2009 2016 2008 2014 2016
## [48803] 2016 2016 2016 2016 2016 2012 2008 2014 2016 2008 2010 2015 2015
## [48816] 2008 2011 2015 2016 2011 2014 2014 2010 2009 2009 2017 2017 2013
## [48829] 2014 2015 2016 2016 2016 2008 2014 2015 2007 2015 2017 2010 2014
## [48842] 2015 2016 2008 2007 2007 2013 2015 2015 2017 2015 2016 2007 2007
## [48855] 2016 2011 2009 2015 2013 2014 2015 2008 2008 2013 2016 2016 2017
## [48868] 2010 2014 2015 2016 2012 2008 2015 2015 2007 2012 2010 2014 2015
## [48881] 2016 2017 2017 2009 2008 2008 2015 2017 2009 2012 2016 2017 2015
## [48894] 2007 2016 2008 2017 2012 2015 2016 2015 2016 2016 2014 2015 2015
## [48907] 2012 2016 2009 2010 2007 2015 2015 2017 2007 2007 2013 2015 2015
## [48920] 2014 2016 2012 2015 2009 2007 2017 2017 2009 2011 2013 2016 2017
## [48933] 2010 2013 2016 2013 2017 2007 2009 2015 2017 2007 2009 2015 2017
## [48946] 2015 2015 2009 2007 2016 2008 2015 2017 2016 2015 2016 2017 2008
## [48959] 2016 2007 2008 2014 2016 2016 2012 2011 2010 2008 2016 2007 2012
## [48972] 2016 2016 2016 2017 2017 2016 2009 2012 2007 2016 2015 2017 2017
## [48985] 2016 2010 2007 2014 2009 2009 2015 2016 2015 2016 2017 2008 2015
## [48998] 2016 2016 2017 2009 2012 2007 2009 2015 2017 2014 2016 2010 2017
## [49011] 2012 2008 2017 2010 2009 2015 2016 2016 2017 2010 2007 2015 2015
## [49024] 2017 2016 2015 2008 2011 2015 2015 2017 2015 2007 2015 2015 2015
## [49037] 2017 2010 2012 2012 2013 2017 2008 2012 2015 2017 2014 2017 2017
## [49050] 2010 2007 2010 2016 2007 2017 2007 2014 2015 2007 2016 2016 2016
## [49063] 2014 2010 2016 2017 2017 2012 2015 2016 2016 2017 2017 2007 2017
## [49076] 2017 2017 2012 2015 2013 2015 2015 2017 2017 2010 2016 2016 2009
## [49089] 2011 2014 2016 2008 2007 2017 2012 2011 2008 2014 2017 2008 2011
## [49102] 2009 2016 2009 2016 2017 2017 2011 2017 2017 2015 2016 2015 2011
## [49115] 2011 2014 2015 2008 2007 2016 2014 2015 2017 2010 2017 2012 2016
## [49128] 2010 2014 2015 2007 2009 2015 2012 2016 2017 2007 2008 2013 2015
## [49141] 2010 2014 2017 2015 2017 2008 2009 2010 2016 2015 2017 2009 2010
## [49154] 2013 2015 2016 2008 2011 2016 2016 2017 2009 2016 2015 2014 2015
## [49167] 2016 2017 2015 2012 2010 2015 2016 2012 2010 2016 2010 2013 2017
## [49180] 2016 2017 2014 2013 2015 2015 2015 2015 2016 2017 2012 2015 2017
## [49193] 2017 2015 2015 2015 2016 2009 2015 2015 2016 2017 2017 2016 2016
## [49206] 2017 2017 2007 2010 2007 2007 2014 2015 2016 2016 2015 2016 2007
## [49219] 2014 2015 2011 2016 2017 2010 2010 2015 2017 2007 2016 2016 2009
## [49232] 2015 2016 2016 2015 2008 2016 2012 2009 2015 2016 2016 2016 2013
## [49245] 2016 2010 2015 2016 2016 2015 2016 2016 2015 2016 2016 2010 2014
## [49258] 2011 2008 2012 2010 2008 2009 2015 2016 2009 2007 2010 2008 2008
## [49271] 2012 2013 2014 2017 2007 2014 2011 2007 2015 2015 2017 2011 2008
## [49284] 2015 2008 2014 2017 2017 2012 2007 2016 2017 2017 2013 2013 2015
## [49297] 2015 2016 2010 2009 2015 2015 2014 2015 2017 2009 2017 2014 2016
## [49310] 2016 2011 2008 2015 2009 2016 2017 2009 2016 2015 2015 2016 2012
## [49323] 2016 2015 2015 2011 2007 2008 2016 2016 2017 2012 2017 2007 2015
## [49336] 2017 2010 2008 2015 2015 2011 2017 2017 2017 2017 2011 2010 2013
## [49349] 2015 2016 2007 2017 2007 2016 2016 2015 2009 2012 2007 2016 2011
## [49362] 2015 2010 2015 2015 2009 2016 2010 2008 2011 2013 2007 2008 2009
## [49375] 2014 2015 2017 2017 2011 2016 2016 2017 2017 2017 2016 2011 2012
## [49388] 2015 2016 2017 2007 2009 2016 2009 2012 2007 2013 2015 2017 2010
## [49401] 2012 2007 2012 2014 2007 2012 2017 2008 2008 2015 2015 2016 2013
## [49414] 2016 2015 2015 2016 2013 2015 2010 2010 2017 2011 2009 2015 2016
## [49427] 2011 2013 2016 2016 2015 2015 2009 2013 2015 2016 2008 2017 2015
## [49440] 2014 2016 2017 2017 2012 2015 2015 2017 2017 2011 2015 2016 2016
## [49453] 2015 2008 2013 2015 2016 2017 2016 2012 2010 2009 2016 2016 2017
## [49466] 2017 2017 2017 2015 2009 2015 2017 2015 2017 2007 2015 2016 2015
## [49479] 2016 2017 2008 2016 2011 2008 2011 2015 2017 2007 2015 2017 2009
## [49492] 2008 2017 2012 2007 2015 2017 2010 2016 2008 2015 2007 2012 2011
## [49505] 2008 2009 2015 2016 2017 2010 2012 2009 2015 2010 2009 2010 2017
## [49518] 2007 2013 2014 2015 2016 2015 2016 2017 2017 2010 2010 2013 2015
## [49531] 2008 2016 2017 2008 2015 2007 2009 2008 2009 2009 2014 2017 2010
## [49544] 2008 2015 2007 2015 2017 2009 2011 2015 2015 2016 2016 2017 2015
## [49557] 2017 2017 2010 2015 2012 2012 2011 2016 2016 2016 2007 2016 2007
## [49570] 2016 2015 2015 2016 2008 2015 2016 2017 2009 2014 2015 2016 2017
## [49583] 2017 2016 2017 2010 2015 2017 2017 2015 2017 2016 2014 2014 2015
## [49596] 2010 2013 2012 2012 2016 2009 2007 2008 2016 2008 2015 2016 2015
## [49609] 2016 2015 2008 2017 2012 2012 2017 2008 2010 2016 2015 2008 2011
## [49622] 2013 2016 2017 2015 2016 2016 2015 2015 2014 2011 2010 2011 2014
## [49635] 2017 2015 2008 2012 2013 2015 2016 2016 2017 2007 2016 2017 2017
## [49648] 2008 2016 2011 2017 2011 2013 2015 2011 2016 2009 2014 2016 2016
## [49661] 2017 2011 2015 2017 2007 2017 2012 2017 2015 2009 2007 2013 2013
## [49674] 2015 2017 2017 2009 2016 2017 2017 2015 2016 2016 2017 2015 2007
## [49687] 2007 2016 2017 2017 2008 2008 2013 2015 2015 2016 2016 2016 2016
## [49700] 2017 2010 2008 2017 2010 2013 2013 2014 2016 2017 2016 2016 2011
## [49713] 2012 2016 2017 2012 2016 2010 2008 2012 2016 2014 2016 2017 2017
## [49726] 2008 2009 2014 2016 2016 2016 2017 2011 2014 2015 2016 2015 2017
## [49739] 2009 2013 2017 2017 2011 2016 2010 2017 2012 2013 2015 2012 2016
## [49752] 2017 2010 2010 2007 2008 2008 2008 2009 2015 2017 2007 2007 2007
## [49765] 2007 2016 2016 2017 2010 2014 2014 2015 2016 2015 2017 2009 2016
## [49778] 2017 2011 2012 2014 2015 2017 2015 2008 2007 2017 2015 2016 2016
## [49791] 2011 2009 2011 2016 2015 2016 2017 2017 2017 2015 2012 2017 2007
## [49804] 2015 2016 2017 2017 2007 2016 2016 2017 2008 2017 2015 2017 2008
## [49817] 2012 2008 2017 2011 2007 2007 2015 2016 2016 2017 2009 2008 2008
## [49830] 2016 2010 2014 2017 2012 2011 2017 2013 2015 2015 2008 2014 2015
## [49843] 2011 2015 2016 2017 2008 2011 2016 2008 2009 2016 2017 2015 2011
## [49856] 2010 2014 2017 2016 2015 2015 2016 2012 2017 2013 2013 2014 2015
## [49869] 2015 2010 2016 2011 2013 2010 2012 2008 2007 2013 2016 2016 2015
## [49882] 2017 2016 2017 2010 2015 2017 2015 2008 2008 2007 2017 2017 2017
## [49895] 2013 2017 2011 2011 2013 2015 2015 2012 2012 2017 2015 2011 2015
## [49908] 2013 2016 2017 2017 2017 2016 2008 2010 2013 2017 2007 2016 2008
## [49921] 2007 2014 2011 2010 2007 2017 2015 2008 2007 2008 2011 2008 2009
## [49934] 2014 2015 2015 2016 2016 2017 2008 2016 2016 2015 2009 2009 2017
## [49947] 2017 2015 2015 2015 2016 2016 2017 2017 2007 2012 2016 2016 2016
## [49960] 2010 2009 2016 2017 2010 2013 2016 2010 2008 2013 2007 2016 2016
## [49973] 2015 2017 2016 2010 2014 2009 2010 2017 2009 2013 2017 2009 2015
## [49986] 2009 2015 2016 2016 2017 2007 2013 2012 2014 2016 2016 2016 2017
## [49999] 2015 2009 2011 2017 2017 2013 2016 2016 2008 2009 2012 2014 2015
## [50012] 2010 2010 2012 2009 2016 2017 2011 2013 2014 2017 2017 2017 2017
## [50025] 2012 2011 2016 2017 2015 2008 2011 2013 2015 2015 2016 2013 2007
## [50038] 2009 2017 2017 2008 2016 2012 2011 2011 2013 2009 2014 2014 2014
## [50051] 2016 2015 2016 2007 2015 2014 2016 2017 2017 2011 2008 2015 2017
## [50064] 2009 2008 2017 2007 2013 2016 2009 2012 2017 2016 2015 2010 2017
## [50077] 2012 2017 2007 2016 2016 2017 2011 2017 2016 2010 2009 2015 2007
## [50090] 2015 2017 2008 2016 2012 2009 2016 2017 2014 2015 2017 2012 2010
## [50103] 2014 2017 2015 2007 2017 2012 2015 2011 2013 2013 2016 2015 2012
## [50116] 2016 2017 2015 2016 2016 2015 2017 2012 2013 2015 2016 2008 2016
## [50129] 2016 2017 2015 2017 2017 2008 2007 2013 2008 2011 2007 2017 2016
## [50142] 2015 2016 2016 2017 2010 2014 2014 2015 2017 2008 2015 2012 2009
## [50155] 2016 2012 2008 2016 2015 2007 2013 2014 2017 2011 2015 2017 2010
## [50168] 2008 2014 2015 2015 2016 2016 2016 2017 2010 2015 2015 2016 2017
## [50181] 2008 2016 2007 2017 2017 2010 2013 2015 2012 2009 2015 2014 2011
## [50194] 2017 2012 2008 2008 2014 2009 2008 2015 2012 2009 2009 2016 2016
## [50207] 2017 2010 2016 2011 2016 2015 2017 2017 2008 2012 2008 2016 2010
## [50220] 2007 2007 2017 2008 2010 2016 2015 2017 2016 2007 2008 2008 2014
## [50233] 2016 2007 2016 2016 2017 2013 2015 2016 2015 2013 2016 2009 2007
## [50246] 2015 2015 2015 2017 2017 2016 2016 2015 2015 2015 2017 2016 2015
## [50259] 2010 2009 2011 2011 2007 2008 2014 2014 2014 2013 2016 2015 2010
## [50272] 2007 2015 2017 2012 2013 2013 2015 2017 2012 2010 2017 2013 2016
## [50285] 2015 2015 2016 2017 2017 2008 2016 2016 2015 2013 2015 2017 2015
## [50298] 2017 2007 2016 2016 2017 2017 2009 2014 2016 2017 2013 2016 2017
## [50311] 2016 2015 2016 2013 2009 2016 2017 2009 2015 2010 2016 2017 2017
## [50324] 2010 2017 2009 2010 2013 2016 2010 2008 2013 2017 2009 2012 2015
## [50337] 2017 2017 2007 2008 2015 2017 2013 2007 2015 2008 2010 2015 2015
## [50350] 2017 2017 2012 2008 2016 2007 2008 2007 2013 2015 2016 2012 2014
## [50363] 2017 2010 2007 2015 2015 2017 2012 2009 2015 2016 2008 2009 2009
## [50376] 2016 2017 2017 2011 2010 2008 2010 2008 2016 2011 2010 2015 2015
## [50389] 2013 2013 2016 2012 2015 2017 2010 2008 2009 2011 2017 2013 2011
## [50402] 2016 2009 2015 2008 2012 2016 2017 2010 2008 2007 2015 2015 2016
## [50415] 2017 2009 2015 2013 2013 2015 2016 2015 2017 2015 2016 2015 2009
## [50428] 2016 2017 2010 2008 2016 2016 2012 2017 2012 2007 2014 2015 2017
## [50441] 2007 2011 2007 2008 2017 2010 2009 2016 2015 2015 2007 2017 2012
## [50454] 2014 2015 2013 2017 2012 2010 2013 2016 2017 2017 2009 2014 2016
## [50467] 2015 2012 2017 2015 2015 2009 2011 2015 2015 2015 2015 2016 2017
## [50480] 2017 2008 2014 2017 2016 2015 2016 2017 2017 2008 2015 2017 2014
## [50493] 2017 2008 2017 2009 2015 2015 2017 2017 2008 2016 2015 2015 2010
## [50506] 2016 2015 2016 2016 2017 2009 2012 2011 2015 2008 2014 2014 2013
## [50519] 2015 2011 2007 2013 2015 2017 2017 2012 2011 2015 2008 2008 2017
## [50532] 2016 2017 2010 2015 2009 2011 2009 2010 2011 2012 2017 2015 2010
## [50545] 2007 2016 2014 2015 2015 2015 2017 2017 2016 2010 2017 2015 2007
## [50558] 2010 2007 2007 2017 2014 2007 2007 2007 2016 2008 2017 2015 2009
## [50571] 2008 2016 2015 2016 2016 2017 2017 2008 2015 2010 2016 2017 2015
## [50584] 2017 2016 2010 2010 2011 2016 2017 2009 2007 2007 2015 2017 2015
## [50597] 2017 2011 2007 2010 2016 2008 2017 2016 2016 2017 2012 2009 2015
## [50610] 2016 2012 2009 2015 2017 2009 2009 2015 2017 2011 2007 2009 2010
## [50623] 2014 2015 2015 2016 2016 2017 2015 2017 2017 2015 2012 2016 2017
## [50636] 2007 2009 2017 2015 2016 2017 2015 2016 2015 2016 2012 2015 2017
## [50649] 2017 2017 2007 2014 2014 2016 2017 2015 2017 2017 2008 2016 2017
## [50662] 2017 2011 2015 2015 2016 2016 2012 2007 2011 2014 2016 2017 2009
## [50675] 2017 2007 2016 2017 2008 2017 2008 2010 2009 2014 2016 2016 2017
## [50688] 2010 2012 2015 2015 2016 2015 2015 2016 2017 2009 2007 2010 2009
## [50701] 2014 2011 2013 2017 2007 2017 2011 2008 2011 2016 2015 2008 2011
## [50714] 2015 2016 2011 2011 2012 2017 2017 2009 2014 2016 2016 2016 2010
## [50727] 2013 2007 2013 2015 2015 2017 2011 2012 2016 2017 2017 2017 2013
## [50740] 2017 2010 2016 2016 2016 2016 2015 2017 2016 2015 2017 2010 2014
## [50753] 2016 2013 2016 2012 2016 2009 2016 2016 2017 2014 2015 2016 2015
## [50766] 2017 2009 2012 2007 2012 2014 2015 2015 2015 2017 2014 2017 2007
## [50779] 2016 2011 2009 2012 2015 2015 2008 2015 2015 2015 2015 2007 2017
## [50792] 2015 2007 2013 2016 2015 2015 2010 2016 2011 2016 2017 2009 2017
## [50805] 2008 2017 2011 2008 2015 2016 2009 2011 2009 2011 2017 2009 2014
## [50818] 2017 2016 2012 2015 2016 2010 2010 2014 2015 2016 2012 2015 2007
## [50831] 2015 2009 2017 2012 2015 2007 2011 2013 2014 2008 2016 2015 2017
## [50844] 2017 2017 2016 2010 2010 2013 2017 2017 2015 2015 2015 2010 2017
## [50857] 2008 2013 2016 2008 2011 2015 2016 2017 2015 2008 2012 2015 2017
## [50870] 2015 2017 2015 2015 2016 2017 2010 2015 2017 2012 2013 2015 2016
## [50883] 2011 2012 2008 2016 2016 2009 2012 2014 2007 2016 2017 2014 2008
## [50896] 2015 2016 2017 2011 2009 2016 2016 2009 2010 2016 2011 2012 2013
## [50909] 2017 2015 2016 2016 2016 2010 2016 2017 2016 2017 2007 2016 2008
## [50922] 2015 2017 2017 2012 2009 2014 2014 2016 2016 2008 2013 2015 2009
## [50935] 2013 2014 2016 2017 2007 2015 2015 2010 2013 2017 2017 2007 2014
## [50948] 2016 2015 2011 2016 2015 2017 2017 2009 2011 2015 2008 2016 2016
## [50961] 2017 2017 2010 2014 2012 2013 2011 2016 2010 2008 2013 2016 2008
## [50974] 2017 2010 2017 2017 2015 2017 2017 2015 2015 2016 2017 2017 2017
## [50987] 2008 2016 2017 2009 2016 2012 2009 2016 2016 2009 2014 2014 2015
## [51000] 2015 2017 2010 2013 2011 2008 2016 2015 2016 2016 2017 2015 2010
## [51013] 2014 2014 2014 2015 2017 2010 2011 2015 2017 2017 2017 2008 2010
## [51026] 2009 2017 2011 2016 2011 2012 2009 2016 2017 2010 2016 2016 2010
## [51039] 2013 2015 2016 2017 2016 2017 2017 2012 2016 2016 2007 2011 2017
## [51052] 2017 2007 2010 2008 2008 2017 2007 2016 2015 2007 2008 2010 2013
## [51065] 2013 2013 2007 2014 2014 2011 2011 2014 2015 2017 2015 2016 2017
## [51078] 2015 2017 2014 2016 2010 2015 2016 2017 2012 2008 2016 2017 2016
## [51091] 2013 2014 2016 2009 2016 2017 2011 2013 2014 2017 2017 2011 2015
## [51104] 2007 2014 2007 2017 2012 2010 2016 2014 2015 2016 2016 2016 2011
## [51117] 2012 2011 2016 2016 2017 2008 2012 2016 2010 2010 2014 2017 2017
## [51130] 2016 2007 2012 2014 2017 2007 2008 2013 2015 2017 2007 2015 2017
## [51143] 2015 2007 2009 2017 2011 2014 2015 2012 2017 2017 2017 2010 2007
## [51156] 2010 2008 2016 2008 2016 2013 2015 2012 2017 2008 2008 2016 2015
## [51169] 2010 2016 2017 2013 2015 2015 2016 2015 2017 2010 2015 2015 2010
## [51182] 2010 2014 2017 2013 2013 2008 2014 2015 2016 2009 2015 2008 2010
## [51195] 2017 2017 2016 2015 2016 2016 2016 2007 2013 2017 2007 2017 2011
## [51208] 2014 2017 2017 2009 2016 2017 2017 2011 2011 2010 2009 2015 2012
## [51221] 2007 2010 2013 2008 2007 2011 2013 2013 2015 2015 2017 2007 2009
## [51234] 2008 2014 2007 2017 2012 2016 2015 2017 2011 2014 2008 2009 2009
## [51247] 2010 2007 2015 2016 2017 2017 2017 2012 2011 2012 2017 2008 2013
## [51260] 2015 2015 2016 2016 2013 2014 2016 2007 2012 2011 2016 2007 2017
## [51273] 2017 2015 2016 2017 2017 2008 2007 2015 2015 2016 2017 2015 2017
## [51286] 2015 2009 2015 2015 2016 2017 2008 2015 2015 2015 2011 2013 2012
## [51299] 2015 2015 2015 2012 2014 2016 2017 2009 2008 2017 2017 2010 2013
## [51312] 2016 2010 2012 2016 2010 2014 2013 2015 2011 2007 2017 2010 2016
## [51325] 2008 2014 2016 2017 2010 2015 2016 2017 2010 2011 2013 2008 2007
## [51338] 2012 2015 2017 2016 2009 2011 2015 2016 2016 2016 2017 2012 2016
## [51351] 2017 2008 2014 2014 2016 2017 2007 2007 2007 2007 2010 2017 2011
## [51364] 2015 2017 2014 2017 2017 2010 2008 2015 2017 2007 2017 2017 2011
## [51377] 2010 2016 2016 2007 2010 2010 2009 2013 2016 2015 2015 2016 2016
## [51390] 2017 2008 2013 2011 2015 2009 2009 2017 2010 2011 2014 2008 2012
## [51403] 2017 2017 2017 2015 2016 2011 2012 2016 2016 2008 2012 2011 2007
## [51416] 2014 2016 2009 2009 2016 2016 2008 2007 2017 2015 2015 2017 2017
## [51429] 2013 2012 2015 2007 2012 2014 2015 2008 2010 2016 2008 2015 2016
## [51442] 2017 2013 2017 2016 2014 2016 2016 2016 2017 2011 2015 2016 2012
## [51455] 2016 2007 2015 2015 2008 2015 2017 2017 2007 2016 2016 2016 2016
## [51468] 2016 2016 2016 2017 2007 2016 2013 2013 2015 2017 2017 2012 2013
## [51481] 2015 2015 2017 2012 2016 2016 2017 2015 2016 2009 2014 2015 2015
## [51494] 2016 2017 2010 2015 2017 2015 2015 2016 2017 2016 2012 2011 2013
## [51507] 2016 2017 2016 2017 2015 2016 2009 2010 2017 2008 2016 2017 2010
## [51520] 2015 2016 2017 2012 2016 2016 2015 2016 2017 2015 2016 2009 2013
## [51533] 2014 2016 2017 2009 2007 2015 2012 2014 2008 2015 2012 2015 2016
## [51546] 2010 2010 2016 2007 2007 2009 2017 2008 2008 2015 2012 2013 2015
## [51559] 2015 2016 2017 2012 2008 2008 2016 2017 2008 2009 2010 2016 2016
## [51572] 2016 2017 2011 2016 2015 2013 2015 2016 2008 2010 2015 2016 2010
## [51585] 2014 2016 2016 2012 2009 2011 2007 2013 2015 2015 2016 2007 2012
## [51598] 2013 2013 2015 2017 2015 2017 2015 2007 2014 2015 2016 2016 2015
## [51611] 2016 2015 2009 2014 2015 2009 2014 2017 2011 2007 2007 2011 2014
## [51624] 2015 2011 2008 2008 2014 2008 2009 2008 2017 2015 2014 2016 2016
## [51637] 2008 2007 2016 2010 2013 2015 2016 2009 2009 2008 2012 2010 2007
## [51650] 2012 2016 2009 2017 2011 2007 2013 2013 2014 2016 2015 2009 2015
## [51663] 2011 2017 2011 2013 2007 2015 2016 2014 2015 2016 2016 2017 2007
## [51676] 2013 2015 2016 2007 2007 2015 2016 2016 2017 2009 2009 2015 2016
## [51689] 2011 2015 2014 2017 2017 2017 2015 2017 2015 2015 2015 2017 2012
## [51702] 2011 2017 2017 2012 2016 2016 2017 2008 2011 2013 2014 2016 2007
## [51715] 2007 2008 2017 2015 2009 2009 2013 2016 2007 2013 2014 2016 2009
## [51728] 2016 2016 2017 2015 2011 2012 2007 2007 2015 2017 2007 2015 2015
## [51741] 2016 2017 2015 2007 2014 2015 2017 2010 2010 2008 2011 2016 2013
## [51754] 2013 2010 2012 2017 2015 2016 2017 2016 2014 2007 2007 2008 2015
## [51767] 2008 2008 2016 2017 2011 2011 2008 2010 2011 2012 2009 2008 2015
## [51780] 2015 2015 2008 2009 2010 2014 2007 2007 2010 2013 2015 2017 2015
## [51793] 2017 2015 2016 2017 2016 2010 2015 2016 2016 2017 2017 2011 2015
## [51806] 2017 2017 2007 2013 2013 2008 2010 2013 2014 2016 2009 2013 2015
## [51819] 2016 2017 2014 2017 2011 2015 2017 2010 2017 2011 2013 2016 2011
## [51832] 2010 2010 2011 2015 2011 2010 2010 2011 2008 2010 2012 2008 2016
## [51845] 2017 2017 2009 2011 2016 2015 2016 2017 2009 2009 2013 2016 2016
## [51858] 2014 2016 2015 2017 2011 2017 2008 2015 2008 2017 2011 2015 2017
## [51871] 2014 2015 2016 2017 2015 2016 2012 2008 2013 2014 2015 2007 2009
## [51884] 2017 2017 2015 2011 2014 2007 2009 2014 2017 2010 2009 2017 2016
## [51897] 2010 2008 2007 2015 2009 2016 2010 2008 2017 2011 2013 2015 2016
## [51910] 2012 2014 2011 2007 2011 2009 2011 2014 2016 2017 2015 2011 2015
## [51923] 2015 2011 2013 2014 2015 2015 2010 2011 2010 2008 2016 2016 2016
## [51936] 2011 2016 2017 2008 2015 2015 2016 2017 2010 2015 2008 2010 2013
## [51949] 2015 2015 2011 2012 2012 2017 2017 2014 2016 2016 2017 2016 2016
## [51962] 2011 2016 2017 2012 2016 2016 2017 2012 2016 2013 2015 2015 2009
## [51975] 2010 2017 2007 2009 2011 2017 2011 2017 2012 2013 2015 2015 2016
## [51988] 2016 2017 2007 2008 2009 2011 2009 2012 2011 2013 2015 2014 2017
## [52001] 2008 2012 2007 2007 2010 2013 2013 2008 2008 2013 2016 2017 2009
## [52014] 2016 2017 2010 2007 2016 2011 2014 2017 2017 2013 2017 2016 2008
## [52027] 2010 2017 2007 2011 2011 2014 2015 2010 2007 2014 2011 2007 2015
## [52040] 2016 2016 2010 2015 2016 2017 2007 2007 2015 2017 2016 2014 2014
## [52053] 2017 2017 2010 2007 2011 2009 2008 2014 2016 2016 2008 2015 2008
## [52066] 2016 2015 2016 2017 2011 2017 2015 2016 2017 2012 2008 2015 2016
## [52079] 2015 2015 2016 2007 2013 2015 2015 2009 2013 2016 2017 2014 2015
## [52092] 2009 2008 2015 2013 2015 2016 2017 2015 2014 2017 2012 2014 2014
## [52105] 2017 2013 2016 2017 2017 2014 2014 2016 2017 2017 2009 2016 2017
## [52118] 2011 2014 2017 2015 2010 2012 2016 2015 2015 2011 2016 2016 2008
## [52131] 2012 2016 2017 2017 2017 2011 2014 2017 2011 2017 2015 2017 2016
## [52144] 2008 2015 2015 2017 2010 2014 2016 2015 2015 2016 2017 2017 2012
## [52157] 2011 2016 2016 2016 2017 2017 2016 2011 2013 2017 2007 2012 2017
## [52170] 2010 2015 2016 2007 2014 2017 2007 2007 2017 2016 2016 2008 2010
## [52183] 2014 2016 2016 2016 2016 2010 2012 2016 2015 2008 2007 2008 2010
## [52196] 2010 2011 2007 2011 2013 2017 2015 2016 2007 2016 2016 2016 2016
## [52209] 2007 2007 2015 2017 2017 2011 2016 2008 2008 2013 2014 2008 2017
## [52222] 2010 2008 2016 2016 2016 2017 2015 2015 2017 2009 2008 2016 2016
## [52235] 2007 2015 2015 2015 2010 2011 2011 2014 2009 2016 2016 2007 2015
## [52248] 2017 2017 2016 2017 2017 2017 2011 2014 2015 2016 2017 2007 2008
## [52261] 2015 2007 2007 2017 2017 2009 2009 2015 2014 2016 2015 2016 2015
## [52274] 2012 2017 2015 2017 2015 2013 2014 2017 2011 2010 2017 2007 2009
## [52287] 2013 2014 2016 2017 2008 2017 2017 2010 2016 2013 2013 2014 2015
## [52300] 2015 2014 2011 2010 2015 2017 2017 2010 2015 2017 2017 2007 2016
## [52313] 2016 2007 2017 2017 2013 2016 2017 2017 2011 2014 2012 2012 2015
## [52326] 2012 2015 2014 2016 2017 2015 2015 2011 2009 2015 2016 2009 2017
## [52339] 2008 2012 2014 2016 2017 2012 2011 2011 2010 2014 2015 2016 2011
## [52352] 2013 2017 2017 2007 2007 2011 2017 2011 2012 2016 2015 2017 2014
## [52365] 2008 2017 2010 2011 2012 2016 2017 2009 2014 2015 2017 2013 2016
## [52378] 2009 2009 2009 2013 2017 2016 2014 2017 2013 2016 2008 2007 2008
## [52391] 2017 2009 2015 2017 2007 2008 2013 2015 2012 2017 2010 2010 2017
## [52404] 2017 2011 2015 2014 2016 2010 2010 2013 2015 2017 2010 2009 2010
## [52417] 2008 2015 2015 2009 2017 2013 2015 2016 2010 2007 2011 2008 2014
## [52430] 2015 2016 2016 2010 2013 2015 2015 2016 2017 2017 2009 2016 2016
## [52443] 2017 2017 2008 2009 2007 2015 2015 2016 2011 2010 2011 2015 2007
## [52456] 2008 2017 2016 2017 2017 2009 2015 2017 2008 2013 2016 2017 2015
## [52469] 2016 2016 2015 2016 2016 2016 2017 2008 2017 2017 2010 2017 2011
## [52482] 2012 2010 2015 2007 2017 2011 2015 2016 2016 2009 2016 2011 2013
## [52495] 2016 2015 2011 2009 2011 2011 2015 2016 2016 2015 2008 2016 2014
## [52508] 2016 2017 2010 2016 2015 2013 2014 2017 2015 2016 2016 2016 2011
## [52521] 2014 2015 2016 2012 2015 2008 2009 2015 2017 2012 2012 2017 2010
## [52534] 2011 2015 2015 2017 2007 2015 2016 2007 2016 2017 2012 2016 2017
## [52547] 2016 2016 2017 2015 2017 2008 2017 2007 2015 2017 2017 2016 2016
## [52560] 2017 2009 2012 2008 2014 2015 2008 2015 2017 2016 2011 2008 2015
## [52573] 2016 2010 2008 2015 2015 2015 2017 2017 2008 2015 2015 2009 2015
## [52586] 2015 2016 2017 2015 2016 2017 2012 2016 2017 2012 2017 2016 2010
## [52599] 2008 2008 2013 2015 2015 2016 2016 2017 2017 2014 2016 2015 2016
## [52612] 2016 2016 2016 2011 2014 2011 2014 2015 2016 2017 2017 2017 2011
## [52625] 2016 2014 2016 2010 2011 2009 2016 2016 2007 2007 2015 2009 2012
## [52638] 2015 2010 2010 2010 2007 2015 2016 2008 2014 2015 2012 2010 2016
## [52651] 2015 2016 2016 2015 2017 2011 2011 2015 2017 2017 2010 2015 2015
## [52664] 2016 2017 2008 2015 2016 2007 2014 2013 2012 2016 2007 2017 2015
## [52677] 2017 2017 2016 2016 2017 2009 2009 2010 2017 2017 2008 2016 2017
## [52690] 2011 2009 2013 2014 2011 2017 2015 2010 2015 2016 2016 2016 2017
## [52703] 2016 2016 2010 2016 2017 2008 2015 2016 2016 2007 2011 2015 2015
## [52716] 2009 2014 2015 2008 2008 2016 2016 2017 2013 2009 2008 2010 2012
## [52729] 2010 2010 2015 2016 2009 2011 2016 2016 2016 2017 2007 2014 2015
## [52742] 2017 2017 2017 2017 2017 2016 2014 2016 2009 2016 2009 2013 2015
## [52755] 2015 2014 2017 2012 2011 2012 2016 2011 2015 2015 2017 2009 2010
## [52768] 2016 2017 2007 2016 2015 2010 2017 2015 2017 2008 2008 2012 2015
## [52781] 2015 2010 2012 2011 2016 2016 2017 2017 2017 2007 2017 2017 2012
## [52794] 2011 2015 2016 2016 2017 2007 2011 2014 2015 2016 2007 2007 2009
## [52807] 2017 2011 2009 2013 2015 2016 2013 2009 2008 2017 2017 2017 2014
## [52820] 2016 2016 2017 2017 2009 2010 2015 2017 2015 2017 2015 2016 2017
## [52833] 2017 2015 2016 2012 2012 2014 2016 2015 2017 2010 2007 2008 2017
## [52846] 2017 2010 2009 2010 2017 2017 2015 2015 2015 2016 2016 2017 2007
## [52859] 2007 2015 2017 2011 2015 2015 2017 2017 2017 2017 2016 2016 2010
## [52872] 2014 2015 2017 2017 2017 2017 2015 2016 2015 2013 2016 2016 2016
## [52885] 2017 2015 2012 2008 2013 2016 2016 2016 2016 2007 2013 2016 2012
## [52898] 2015 2007 2017 2017 2017 2010 2015 2016 2016 2015 2016 2013 2017
## [52911] 2013 2015 2012 2010 2017 2007 2009 2012 2017 2011 2007 2007 2011
## [52924] 2011 2008 2015 2015 2016 2015 2017 2015 2017 2017 2007 2016 2008
## [52937] 2013 2015 2013 2015 2017 2007 2016 2017 2017 2017 2015 2016 2008
## [52950] 2015 2010 2015 2017 2015 2016 2016 2012 2008 2007 2008 2013 2015
## [52963] 2009 2009 2008 2015 2017 2017 2013 2015 2016 2015 2007 2015 2015
## [52976] 2009 2016 2011 2014 2009 2014 2016 2016 2010 2013 2015 2007 2015
## [52989] 2009 2016 2016 2017 2007 2013 2012 2008 2015 2016 2017 2007 2007
## [53002] 2009 2015 2017 2009 2016 2011 2015 2016 2016 2010 2008 2010 2017
## [53015] 2009 2011 2015 2007 2011 2008 2011 2013 2016 2009 2009 2016 2015
## [53028] 2016 2012 2007 2012 2011 2013 2012 2007 2017 2010 2014 2016 2016
## [53041] 2016 2016 2015 2017 2012 2015 2016 2016 2009 2008 2016 2008 2010
## [53054] 2014 2017 2012 2008 2016 2017 2008 2010 2016 2016 2016 2011 2015
## [53067] 2016 2010 2007 2015 2015 2017 2010 2015 2016 2017 2012 2007 2010
## [53080] 2015 2009 2015 2011 2008 2015 2014 2012 2016 2016 2008 2012 2008
## [53093] 2011 2009 2009 2017 2007 2009 2015 2016 2016 2016 2012 2007 2013
## [53106] 2017 2008 2010 2007 2013 2011 2009 2017 2011 2012 2017 2008 2016
## [53119] 2016 2010 2014 2015 2016 2008 2017 2016 2017 2009 2009 2016 2013
## [53132] 2017 2009 2008 2015 2011 2007 2011 2013 2015 2016 2009 2016 2007
## [53145] 2009 2015 2017 2007 2016 2009 2009 2007 2016 2010 2015 2011 2016
## [53158] 2017 2017 2011 2008 2008 2016 2013 2007 2015 2017 2013 2014 2016
## [53171] 2017 2009 2007 2013 2015 2017 2012 2015 2016 2010 2011 2016 2015
## [53184] 2017 2016 2016 2017 2007 2015 2015 2016 2015 2008 2017 2015 2016
## [53197] 2016 2009 2009 2015 2014 2016 2017 2017 2016 2017 2017 2017 2011
## [53210] 2008 2008 2016 2013 2017 2017 2015 2016 2016 2017 2015 2017 2010
## [53223] 2013 2015 2017 2017 2017 2015 2007 2008 2015 2007 2011 2010 2013
## [53236] 2015 2015 2013 2013 2015 2017 2015 2015 2013 2016 2011 2016 2016
## [53249] 2011 2015 2016 2016 2009 2007 2016 2012 2012 2009 2013 2017 2015
## [53262] 2016 2013 2016 2017 2017 2009 2017 2014 2011 2013 2012 2017 2016
## [53275] 2017 2017 2017 2017 2017 2015 2015 2016 2017 2015 2016 2016 2017
## [53288] 2015 2016 2016 2017 2015 2015 2016 2010 2011 2009 2017 2012 2014
## [53301] 2016 2008 2015 2016 2011 2015 2016 2012 2015 2017 2008 2009 2013
## [53314] 2016 2017 2017 2013 2014 2009 2015 2015 2015 2017 2016 2007 2015
## [53327] 2014 2015 2015 2008 2012 2013 2015 2017 2015 2013 2014 2007 2017
## [53340] 2010 2010 2015 2009 2009 2015 2007 2014 2017 2009 2015 2017 2009
## [53353] 2011 2010 2016 2017 2016 2017 2008 2016 2015 2016 2017 2010 2009
## [53366] 2015 2007 2016 2016 2012 2009 2007 2010 2017 2014 2015 2015 2008
## [53379] 2008 2011 2007 2011 2017 2016 2016 2013 2011 2017 2010 2012 2015
## [53392] 2010 2016 2017 2011 2008 2008 2015 2016 2017 2017 2009 2015 2016
## [53405] 2017 2010 2015 2015 2015 2010 2007 2013 2015 2007 2013 2014 2015
## [53418] 2017 2017 2017 2007 2011 2009 2008 2011 2008 2007 2009 2007 2015
## [53431] 2016 2009 2016 2017 2012 2010 2013 2016 2009 2007 2017 2014 2016
## [53444] 2009 2008 2010 2014 2015 2017 2007 2014 2017 2008 2015 2017 2013
## [53457] 2017 2008 2008 2016 2017 2016 2014 2016 2009 2016 2017 2017 2010
## [53470] 2007 2015 2017 2017 2016 2016 2017 2016 2017 2017 2009 2016 2016
## [53483] 2017 2017 2015 2008 2016 2017 2008 2016 2010 2015 2015 2008 2008
## [53496] 2017 2008 2011 2016 2017 2016 2016 2014 2015 2016 2015 2008 2017
## [53509] 2017 2008 2014 2016 2015 2012 2016 2007 2016 2017 2014 2016 2016
## [53522] 2012 2016 2015 2016 2012 2011 2017 2010 2017 2012 2011 2015 2016
## [53535] 2007 2008 2015 2017 2013 2016 2016 2017 2016 2010 2013 2016 2014
## [53548] 2016 2017 2012 2007 2015 2009 2012 2017 2016 2015 2016 2010 2015
## [53561] 2016 2015 2016 2017 2017 2014 2014 2015 2008 2013 2016 2007 2008
## [53574] 2014 2017 2016 2017 2009 2015 2015 2016 2017 2017 2012 2015 2015
## [53587] 2016 2017 2017 2007 2014 2014 2017 2015 2008 2013 2017 2010 2013
## [53600] 2015 2015 2007 2016 2017 2009 2016 2017 2017 2017 2012 2016 2010
## [53613] 2010 2016 2015 2017 2012 2013 2016 2016 2016 2016 2017 2016 2016
## [53626] 2017 2007 2017 2012 2014 2014 2016 2008 2017 2010 2008 2009 2014
## [53639] 2017 2008 2012 2010 2012 2016 2008 2008 2008 2011 2017 2017 2008
## [53652] 2009 2016 2017 2015 2016 2008 2007 2009 2016 2017 2011 2013 2007
## [53665] 2007 2012 2009 2017 2015 2015 2012 2017 2014 2017 2012 2011 2009
## [53678] 2017 2012 2010 2009 2012 2007 2009 2013 2016 2007 2015 2017 2007
## [53691] 2015 2017 2015 2017 2014 2016 2007 2015 2012 2017 2011 2016 2010
## [53704] 2009 2016 2015 2007 2007 2010 2010 2017 2017 2007 2008 2017 2015
## [53717] 2016 2015 2016 2008 2015 2016 2016 2016 2017 2017 2015 2017 2009
## [53730] 2009 2015 2017 2014 2011 2015 2016 2016 2016 2015 2013 2016 2008
## [53743] 2015 2017 2008 2011 2007 2015 2015 2016 2016 2016 2017 2013 2016
## [53756] 2017 2017 2007 2010 2014 2015 2015 2012 2008 2016 2017 2010 2011
## [53769] 2015 2015 2015 2017 2008 2016 2015 2015 2017 2012 2016 2016 2008
## [53782] 2015 2016 2016 2009 2014 2017 2015 2016 2017 2010 2013 2015 2017
## [53795] 2011 2007 2014 2017 2012 2008 2010 2014 2014 2007 2012 2015 2015
## [53808] 2016 2017 2009 2009 2014 2010 2011 2010 2015 2007 2008 2016 2007
## [53821] 2010 2007 2016 2015 2010 2015 2012 2008 2016 2017 2009 2008 2009
## [53834] 2015 2017 2017 2009 2011 2009 2012 2014 2016 2017 2015 2015 2017
## [53847] 2011 2015 2015 2016 2017 2009 2015 2016 2016 2016 2010 2015 2017
## [53860] 2017 2017 2013 2013 2014 2015 2007 2012 2012 2015 2017 2007 2014
## [53873] 2016 2017 2013 2016 2015 2011 2015 2017 2016 2015 2015 2016 2015
## [53886] 2007 2012 2009 2013 2016 2014 2016 2017 2017 2016 2016 2016 2011
## [53899] 2017 2009 2015 2016 2017 2011 2012 2007 2015 2015 2016 2008 2012
## [53912] 2016 2016 2011 2017 2016 2010 2011 2015 2016 2016 2017 2017 2017
## [53925] 2013 2017 2010 2008 2013 2017 2008 2015 2015 2012 2014 2015 2016
## [53938] 2016 2013 2013 2015 2017 2015 2016 2010 2017 2017 2009 2007 2012
## [53951] 2016 2017 2016 2017 2017 2011 2010 2008 2011 2007 2016 2015 2015
## [53964] 2017 2012 2010 2013 2015 2017 2010 2015 2016 2017 2013 2016 2017
## [53977] 2017 2017 2012 2015 2017 2017 2017 2011 2012 2017 2015 2016 2016
## [53990] 2016 2016 2012 2015 2009 2013 2014 2015 2015 2015 2011 2016 2011
## [54003] 2011 2016 2015 2009 2009 2015 2016 2017 2010 2013 2012 2015 2017
## [54016] 2009 2015 2016 2016 2007 2007 2016 2017 2016 2010 2015 2017 2013
## [54029] 2011 2014 2016 2009 2013 2016 2017 2013 2015 2009 2016 2016 2017
## [54042] 2017 2010 2010 2017 2015 2016 2016 2016 2008 2007 2012 2015 2015
## [54055] 2008 2015 2015 2017 2013 2013 2016 2016 2017 2017 2012 2015 2016
## [54068] 2007 2016 2017 2017 2014 2015 2013 2015 2013 2013 2016 2009 2010
## [54081] 2014 2012 2017 2017 2017 2015 2015 2015 2008 2009 2007 2014 2017
## [54094] 2017 2017 2011 2014 2015 2015 2016 2016 2016 2008 2017 2017 2009
## [54107] 2007 2014 2013 2014 2014 2016 2015 2015 2013 2016 2017 2015 2015
## [54120] 2010 2014 2017 2016 2017 2017 2017 2015 2015 2015 2016 2017 2017
## [54133] 2015 2011 2017 2011 2016 2007 2008 2009 2017 2014 2015 2016 2013
## [54146] 2008 2017 2017 2017 2008 2016 2017 2017 2007 2009 2013 2016 2016
## [54159] 2017 2011 2009 2016 2017 2012 2016 2009 2015 2015 2015 2016 2017
## [54172] 2017 2011 2012 2011 2016 2015 2008 2016 2016 2017 2017 2015 2010
## [54185] 2016 2009 2016 2017 2017 2010 2013 2016 2017 2017 2012 2011 2011
## [54198] 2009 2015 2009 2017 2009 2008 2009 2014 2016 2016 2016 2017 2013
## [54211] 2015 2014 2017 2017 2016 2015 2011 2011 2010 2015 2007 2017 2017
## [54224] 2015 2015 2011 2016 2016 2017 2015 2010 2008 2013 2015 2016 2012
## [54237] 2009 2016 2012 2017 2009 2007 2016 2017 2016 2017 2010 2014 2012
## [54250] 2009 2015 2017 2008 2011 2008 2012 2007 2015 2016 2009 2016 2010
## [54263] 2011 2015 2016 2007 2015 2016 2017 2012 2017 2017 2017 2015 2015
## [54276] 2017 2009 2016 2015 2012 2013 2015 2016 2017 2011 2007 2012 2016
## [54289] 2016 2015 2013 2017 2017 2014 2014 2016 2015 2010 2010 2015 2017
## [54302] 2010 2012 2009 2015 2016 2017 2016 2012 2008 2014 2015 2015 2010
## [54315] 2008 2010 2016 2015 2016 2015 2017 2015 2011 2008 2007 2016 2017
## [54328] 2010 2016 2017 2017 2017 2015 2010 2017 2008 2014 2016 2015 2017
## [54341] 2017 2016 2017 2016 2010 2011 2009 2008 2009 2015 2015 2012 2014
## [54354] 2016 2017 2010 2011 2015 2015 2015 2017 2008 2013 2015 2016 2015
## [54367] 2010 2011 2010 2014 2009 2007 2011 2011 2012 2015 2017 2017 2007
## [54380] 2017 2017 2017 2008 2008 2009 2011 2012 2016 2008 2014 2016 2016
## [54393] 2015 2012 2009 2015 2009 2008 2016 2017 2016 2010 2015 2016 2017
## [54406] 2007 2015 2016 2016 2014 2015 2017 2017 2011 2015 2016 2009 2016
## [54419] 2009 2016 2008 2010 2014 2012 2008 2012 2017 2017 2012 2015 2017
## [54432] 2011 2017 2017 2009 2015 2015 2015 2017 2009 2009 2010 2015 2015
## [54445] 2015 2016 2016 2017 2007 2010 2013 2016 2017 2009 2016 2011 2016
## [54458] 2015 2016 2017 2010 2011 2010 2015 2017 2013 2014 2017 2009 2007
## [54471] 2015 2016 2016 2015 2017 2010 2016 2015 2015 2017 2008 2013 2014
## [54484] 2015 2016 2009 2007 2016 2017 2017 2010 2007 2014 2007 2011 2016
## [54497] 2016 2017 2007 2015 2016 2011 2017 2013 2015 2011 2012 2016 2010
## [54510] 2015 2016 2008 2015 2017 2007 2017 2015 2007 2015 2016 2017 2017
## [54523] 2008 2010 2016 2016 2017 2015 2016 2017 2011 2008 2013 2017 2017
## [54536] 2009 2014 2016 2017 2010 2012 2013 2010 2010 2009 2009 2016 2016
## [54549] 2017 2007 2008 2008 2012 2014 2014 2015 2015 2017 2009 2013 2015
## [54562] 2015 2016 2017 2010 2015 2009 2016 2008 2015 2017 2011 2012 2014
## [54575] 2007 2016 2012 2009 2014 2016 2016 2017 2008 2009 2010 2007 2007
## [54588] 2017 2011 2010 2016 2011 2015 2016 2009 2013 2009 2016 2017 2015
## [54601] 2016 2015 2013 2007 2008 2008 2017 2008 2015 2016 2017 2016 2015
## [54614] 2008 2011 2007 2014 2015 2016 2008 2013 2017 2017 2017 2008 2007
## [54627] 2017 2011 2011 2013 2009 2009 2017 2007 2010 2014 2015 2009 2016
## [54640] 2016 2016 2015 2014 2015 2008 2008 2009 2007 2009 2013 2016 2015
## [54653] 2017 2007 2012 2008 2017 2008 2016 2017 2011 2015 2016 2009 2014
## [54666] 2015 2016 2013 2017 2008 2013 2017 2017 2014 2014 2015 2016 2012
## [54679] 2015 2010 2013 2015 2017 2017 2012 2010 2015 2016 2009 2015 2009
## [54692] 2015 2015 2015 2017 2013 2015 2017 2014 2016 2017 2010 2011 2012
## [54705] 2017 2017 2017 2016 2015 2008 2015 2016 2016 2016 2015 2017 2008
## [54718] 2009 2015 2015 2016 2016 2017 2009 2015 2007 2008 2017 2015 2013
## [54731] 2017 2008 2015 2016 2017 2010 2007 2015 2016 2013 2017 2011 2008
## [54744] 2008 2011 2007 2013 2015 2016 2017 2017 2007 2009 2016 2007 2015
## [54757] 2007 2016 2015 2012 2015 2016 2010 2015 2012 2013 2017 2015 2015
## [54770] 2008 2015 2016 2016 2016 2017 2017 2007 2017 2016 2010 2008 2017
## [54783] 2015 2016 2017 2013 2017 2007 2017 2008 2017 2011 2014 2015 2016
## [54796] 2007 2016 2016 2010 2017 2016 2008 2015 2017 2007 2010 2011 2013
## [54809] 2015 2016 2015 2009 2007 2016 2010 2009 2013 2016 2010 2016 2011
## [54822] 2014 2015 2012 2014 2016 2017 2013 2017 2008 2012 2011 2010 2013
## [54835] 2016 2014 2017 2008 2014 2016 2016 2016 2009 2017 2011 2011 2015
## [54848] 2015 2016 2010 2016 2016 2017 2009 2008 2008 2012 2012 2009 2013
## [54861] 2017 2011 2011 2014 2007 2010 2016 2009 2012 2016 2014 2016 2016
## [54874] 2016 2017 2009 2015 2011 2010 2015 2016 2016 2017 2017 2017 2012
## [54887] 2011 2014 2015 2017 2017 2017 2010 2013 2013 2014 2017 2007 2016
## [54900] 2015 2016 2017 2015 2017 2008 2012 2010 2016 2015 2015 2016 2017
## [54913] 2017 2012 2015 2016 2012 2009 2010 2016 2017 2012 2016 2015 2016
## [54926] 2017 2017 2013 2016 2015 2008 2015 2016 2017 2008 2008 2011 2016
## [54939] 2008 2013 2015 2009 2007 2011 2013 2016 2017 2012 2011 2016 2016
## [54952] 2015 2015 2017 2017 2017 2016 2017 2010 2009 2008 2009 2015 2009
## [54965] 2007 2013 2014 2017 2016 2015 2017 2009 2012 2017 2012 2008 2013
## [54978] 2015 2016 2008 2010 2013 2009 2016 2016 2012 2010 2009 2013 2013
## [54991] 2009 2007 2016 2011 2017 2008 2013 2014 2015 2017 2017 2007 2009
## [55004] 2012 2016 2014 2016 2016 2014 2011 2010 2015 2015 2016 2017 2009
## [55017] 2013 2016 2017 2007 2008 2014 2007 2012 2007 2017 2016 2012 2016
## [55030] 2017 2008 2017 2014 2017 2014 2016 2016 2015 2013 2015 2016 2016
## [55043] 2012 2013 2015 2016 2010 2007 2009 2014 2016 2015 2010 2016 2017
## [55056] 2012 2009 2015 2016 2016 2016 2015 2017 2012 2012 2015 2017 2012
## [55069] 2015 2017 2011 2016 2017 2017 2015 2017 2013 2016 2017 2009 2009
## [55082] 2013 2014 2016 2016 2010 2011 2017 2011 2009 2015 2017 2016 2017
## [55095] 2008 2008 2016 2016 2008 2014 2016 2010 2013 2015 2015 2016 2016
## [55108] 2014 2015 2008 2008 2012 2017 2007 2017 2012 2011 2017 2011 2014
## [55121] 2017 2017 2016 2017 2012 2009 2012 2014 2017 2008 2017 2011 2010
## [55134] 2017 2010 2016 2017 2007 2010 2017 2007 2015 2016 2017 2011 2016
## [55147] 2017 2016 2016 2015 2017 2008 2015 2017 2008 2013 2012 2016 2017
## [55160] 2015 2017 2017 2010 2016 2017 2017 2009 2014 2016 2016 2016 2009
## [55173] 2011 2011 2013 2014 2015 2015 2017 2007 2013 2015 2017 2011 2010
## [55186] 2013 2016 2016 2017 2011 2015 2015 2017 2017 2007 2016 2017 2017
## [55199] 2008 2011 2017 2014 2017 2016 2017 2008 2016 2017 2013 2017 2011
## [55212] 2017 2017 2007 2013 2015 2007 2015 2007 2015 2015 2015 2016 2007
## [55225] 2016 2009 2007 2017 2009 2013 2015 2015 2012 2016 2017 2015 2017
## [55238] 2012 2010 2016 2009 2016 2016 2007 2010 2016 2016 2010 2016 2017
## [55251] 2017 2015 2015 2015 2017 2009 2017 2013 2015 2011 2015 2014 2015
## [55264] 2012 2017 2016 2015 2017 2008 2015 2016 2016 2017 2011 2015 2010
## [55277] 2007 2017 2008 2015 2015 2014 2009 2007 2014 2016 2017 2014 2016
## [55290] 2017 2016 2017 2007 2013 2014 2015 2015 2010 2016 2009 2010 2011
## [55303] 2009 2011 2015 2016 2017 2009 2007 2016 2010 2013 2015 2017 2010
## [55316] 2016 2017 2017 2017 2014 2017 2007 2015 2017 2017 2017 2009 2011
## [55329] 2016 2015 2011 2009 2015 2015 2016 2017 2010 2016 2011 2009 2016
## [55342] 2008 2013 2014 2017 2012 2015 2015 2015 2017 2008 2010 2015 2017
## [55355] 2012 2012 2011 2013 2016 2017 2015 2011 2016 2017 2012 2011 2011
## [55368] 2017 2017 2009 2008 2015 2016 2009 2010 2007 2016 2016 2015 2010
## [55381] 2017 2017 2008 2012 2017 2016 2015 2017 2011 2012 2017 2010 2016
## [55394] 2016 2015 2015 2016 2008 2012 2009 2016 2016 2016 2007 2016 2012
## [55407] 2011 2017 2009 2008 2017 2017 2016 2015 2015 2016 2017 2010 2013
## [55420] 2016 2016 2016 2007 2012 2017 2015 2017 2012 2017 2016 2017 2017
## [55433] 2017 2008 2011 2015 2017 2017 2017 2013 2015 2017 2017 2007 2015
## [55446] 2016 2016 2016 2017 2017 2009 2013 2017 2008 2016 2010 2010 2016
## [55459] 2011 2015 2016 2017 2010 2007 2016 2014 2015 2016 2015 2007 2014
## [55472] 2016 2017 2008 2016 2015 2017 2011 2016 2017 2017 2017 2016 2012
## [55485] 2010 2013 2017 2008 2014 2016 2016 2009 2013 2015 2009 2011 2012
## [55498] 2016 2017 2007 2015 2007 2013 2008 2011 2016 2016 2017 2015 2016
## [55511] 2017 2015 2016 2007 2015 2009 2014 2017 2015 2017 2010 2016 2015
## [55524] 2016 2007 2008 2009 2009 2014 2015 2015 2015 2017 2007 2009 2016
## [55537] 2016 2016 2017 2017 2008 2015 2015 2015 2009 2016 2016 2007 2008
## [55550] 2016 2015 2014 2008 2013 2017 2017 2008 2011 2016 2015 2017 2015
## [55563] 2016 2016 2008 2014 2010 2013 2016 2017 2011 2009 2015 2016 2016
## [55576] 2009 2015 2016 2011 2008 2007 2016 2015 2016 2017 2011 2016 2012
## [55589] 2017 2012 2010 2016 2017 2017 2017 2010 2008 2013 2017 2016 2017
## [55602] 2015 2017 2007 2012 2007 2016 2015 2010 2015 2016 2015 2016 2016
## [55615] 2011 2008 2017 2011 2017 2017 2017 2014 2016 2017 2009 2017 2015
## [55628] 2016 2011 2011 2012 2014 2015 2015 2017 2009 2007 2009 2016 2008
## [55641] 2012 2010 2017 2015 2010 2017 2008 2016 2009 2015 2016 2017 2007
## [55654] 2008 2016 2009 2016 2016 2016 2017 2011 2011 2009 2016 2017 2012
## [55667] 2008 2012 2013 2012 2010 2014 2014 2009 2011 2017 2015 2016 2017
## [55680] 2008 2009 2011 2012 2014 2016 2017 2012 2007 2015 2009 2014 2016
## [55693] 2015 2015 2007 2012 2017 2009 2016 2008 2015 2015 2016 2015 2007
## [55706] 2017 2017 2016 2017 2009 2011 2015 2017 2012 2015 2015 2016 2016
## [55719] 2017 2014 2015 2013 2015 2017 2008 2015 2016 2015 2009 2014 2017
## [55732] 2008 2008 2007 2017 2017 2015 2009 2008 2017 2017 2015 2017 2007
## [55745] 2009 2008 2012 2016 2008 2010 2009 2014 2017 2017 2017 2010 2007
## [55758] 2015 2016 2007 2010 2007 2015 2016 2013 2015 2013 2009 2013 2017
## [55771] 2017 2009 2010 2017 2017 2013 2010 2016 2015 2016 2016 2016 2008
## [55784] 2015 2009 2016 2017 2008 2010 2010 2016 2011 2016 2008 2013 2017
## [55797] 2015 2017 2008 2007 2014 2016 2016 2010 2009 2014 2015 2015 2016
## [55810] 2016 2017 2014 2017 2016 2008 2011 2010 2017 2016 2016 2011 2010
## [55823] 2012 2014 2015 2015 2010 2013 2010 2014 2017 2017 2017 2017 2015
## [55836] 2017 2010 2016 2007 2013 2012 2007 2008 2017 2016 2016 2015 2017
## [55849] 2017 2012 2007 2017 2008 2013 2015 2011 2008 2017 2011 2017 2016
## [55862] 2007 2015 2015 2016 2007 2015 2012 2010 2008 2016 2015 2015 2016
## [55875] 2011 2014 2015 2016 2007 2013 2015 2017 2017 2015 2016 2008 2015
## [55888] 2015 2013 2010 2016 2016 2017 2008 2017 2016 2017 2012 2014 2016
## [55901] 2010 2009 2017 2015 2015 2016 2014 2016 2015 2007 2015 2016 2015
## [55914] 2009 2007 2015 2017 2008 2013 2015 2017 2008 2016 2016 2011 2009
## [55927] 2015 2017 2017 2013 2015 2015 2017 2017 2011 2009 2016 2009 2009
## [55940] 2012 2012 2014 2014 2014 2016 2017 2009 2016 2016 2017 2009 2010
## [55953] 2016 2016 2016 2009 2015 2016 2009 2014 2017 2010 2010 2008 2016
## [55966] 2015 2016 2017 2011 2014 2016 2015 2016 2016 2015 2008 2011 2008
## [55979] 2015 2017 2015 2017 2015 2017 2017 2010 2016 2011 2012 2008 2010
## [55992] 2015 2015 2017 2015 2017 2009 2009 2017 2013 2012 2015 2016 2008
## [56005] 2011 2013 2015 2015 2016 2017 2017 2010 2009 2015 2017 2012 2016
## [56018] 2017 2016 2008 2016 2009 2008 2010 2017 2015 2016 2016 2008 2010
## [56031] 2016 2017 2009 2016 2016 2007 2015 2011 2012 2009 2016 2016 2007
## [56044] 2015 2017 2010 2011 2012 2009 2017 2017 2017 2007 2013 2011 2011
## [56057] 2013 2016 2016 2017 2007 2015 2016 2016 2016 2016 2008 2013 2015
## [56070] 2015 2015 2009 2015 2016 2017 2013 2016 2010 2009 2015 2015 2008
## [56083] 2009 2008 2013 2016 2016 2008 2017 2017 2012 2011 2007 2009 2014
## [56096] 2012 2014 2017 2017 2011 2008 2009 2015 2007 2007 2017 2015 2007
## [56109] 2017 2015 2012 2010 2015 2015 2016 2017 2016 2015 2012 2010 2016
## [56122] 2008 2007 2015 2012 2008 2010 2016 2016 2017 2012 2017 2012 2016
## [56135] 2016 2016 2008 2016 2016 2017 2011 2012 2008 2014 2015 2016 2017
## [56148] 2012 2008 2015 2017 2015 2017 2010 2008 2016 2016 2009 2008 2010
## [56161] 2011 2012 2017 2012 2016 2015 2011 2010 2014 2011 2009 2015 2016
## [56174] 2017 2009 2014 2012 2013 2016 2012 2016 2011 2008 2017 2010 2016
## [56187] 2015 2017 2015 2017 2014 2015 2015 2008 2008 2017 2015 2016 2016
## [56200] 2011 2009 2009 2012 2012 2015 2015 2012 2010 2017 2017 2017 2009
## [56213] 2011 2012 2007 2008 2010 2015 2015 2015 2009 2015 2008 2011 2007
## [56226] 2016 2017 2017 2017 2011 2010 2011 2017 2008 2014 2015 2012 2009
## [56239] 2011 2016 2010 2016 2017 2016 2016 2012 2016 2016 2017 2017 2011
## [56252] 2015 2017 2017 2012 2015 2017 2016 2016 2017 2009 2010 2016 2016
## [56265] 2017 2015 2008 2015 2010 2013 2015 2009 2017 2014 2016 2015 2010
## [56278] 2016 2010 2013 2009 2010 2010 2013 2016 2008 2009 2015 2010 2016
## [56291] 2016 2017 2009 2015 2016 2017 2014 2015 2015 2015 2011 2011 2017
## [56304] 2008 2016 2011 2015 2015 2017 2016 2016 2017 2017 2014 2017 2016
## [56317] 2009 2016 2016 2017 2013 2017 2008 2010 2014 2012 2014 2008 2008
## [56330] 2009 2016 2016 2011 2010 2015 2016 2012 2011 2007 2013 2016 2017
## [56343] 2014 2016 2017 2012 2008 2012 2016 2016 2011 2014 2015 2017 2010
## [56356] 2016 2016 2017 2017 2015 2016 2009 2017 2015 2010 2008 2015 2009
## [56369] 2015 2016 2008 2015 2017 2015 2016 2017 2013 2007 2016 2009 2016
## [56382] 2016 2017 2007 2015 2016 2016 2017 2015 2016 2008 2013 2014 2015
## [56395] 2007 2008 2015 2011 2008 2008 2016 2015 2016 2007 2012 2008 2007
## [56408] 2016 2016 2016 2016 2015 2017 2014 2008 2014 2014 2016 2016 2017
## [56421] 2007 2017 2017 2013 2016 2017 2008 2016 2016 2017 2017 2017 2011
## [56434] 2008 2015 2016 2017 2008 2008 2010 2014 2013 2011 2011 2014 2011
## [56447] 2009 2007 2014 2014 2016 2016 2017 2017 2013 2016 2015 2015 2017
## [56460] 2017 2016 2016 2007 2013 2015 2016 2017 2012 2016 2016 2016 2011
## [56473] 2015 2016 2017 2015 2008 2017 2015 2015 2016 2016 2015 2016 2016
## [56486] 2017 2014 2016 2015 2016 2017 2017 2012 2015 2016 2010 2007 2015
## [56499] 2016 2008 2012 2016 2016 2007 2015 2015 2016 2007 2012 2016 2017
## [56512] 2016 2017 2017 2011 2008 2013 2014 2017 2017 2015 2017 2016 2016
## [56525] 2016 2009 2015 2016 2016 2017 2012 2010 2015 2015 2008 2010 2012
## [56538] 2010 2013 2016 2016 2017 2011 2012 2014 2016 2007 2009 2008 2013
## [56551] 2016 2015 2008 2007 2008 2013 2014 2015 2017 2011 2015 2007 2007
## [56564] 2013 2017 2017 2007 2011 2015 2015 2017 2008 2016 2017 2017 2007
## [56577] 2008 2016 2017 2008 2007 2010 2011 2013 2009 2016 2015 2017 2017
## [56590] 2012 2009 2017 2017 2011 2016 2017 2017 2015 2012 2007 2013 2010
## [56603] 2013 2015 2016 2015 2009 2013 2017 2008 2007 2007 2017 2016 2015
## [56616] 2016 2017 2008 2013 2015 2015 2017 2017 2017 2009 2011 2009 2015
## [56629] 2007 2008 2016 2017 2008 2016 2016 2015 2016 2016 2017 2008 2013
## [56642] 2017 2016 2017 2015 2016 2013 2016 2017 2011 2015 2016 2017 2007
## [56655] 2015 2017 2016 2015 2011 2007 2015 2016 2017 2017 2010 2010 2017
## [56668] 2015 2008 2007 2014 2015 2016 2017 2017 2014 2015 2017 2008 2015
## [56681] 2016 2017 2016 2017 2008 2015 2007 2012 2013 2015 2017 2017 2013
## [56694] 2014 2015 2011 2015 2015 2015 2017 2015 2011 2011 2016 2016 2007
## [56707] 2015 2009 2014 2010 2008 2010 2015 2008 2016 2010 2015 2016 2016
## [56720] 2012 2012 2012 2016 2011 2015 2016 2008 2016 2017 2017 2012 2012
## [56733] 2011 2014 2015 2017 2015 2012 2011 2010 2015 2016 2017 2017 2009
## [56746] 2017 2017 2012 2015 2017 2017 2017 2013 2015 2011 2010 2016 2012
## [56759] 2015 2017 2011 2009 2009 2016 2017 2012 2017 2010 2007 2009 2016
## [56772] 2015 2012 2017 2016 2015 2017 2015 2017 2012 2013 2014 2017 2017
## [56785] 2013 2016 2017 2017 2016 2016 2017 2009 2017 2010 2015 2016 2017
## [56798] 2012 2016 2010 2011 2010 2016 2011 2008 2014 2017 2012 2015 2015
## [56811] 2008 2012 2012 2013 2012 2012 2010 2012 2015 2016 2016 2016 2017
## [56824] 2017 2016 2015 2008 2010 2013 2017 2016 2017 2017 2012 2013 2015
## [56837] 2017 2010 2008 2008 2017 2011 2011 2017 2009 2013 2008 2008 2014
## [56850] 2015 2015 2016 2016 2007 2008 2009 2016 2017 2015 2012 2015 2010
## [56863] 2009 2015 2017 2017 2016 2017 2017 2010 2014 2015 2007 2013 2016
## [56876] 2016 2010 2015 2016 2015 2017 2012 2010 2010 2016 2016 2013 2015
## [56889] 2016 2015 2015 2014 2016 2017 2015 2017 2010 2016 2015 2017 2007
## [56902] 2015 2010 2016 2015 2016 2012 2009 2017 2010 2008 2007 2009 2013
## [56915] 2015 2017 2008 2012 2011 2009 2014 2016 2008 2007 2010 2015 2017
## [56928] 2015 2014 2016 2010 2013 2016 2013 2015 2010 2009 2012 2007 2009
## [56941] 2015 2016 2016 2016 2015 2014 2016 2017 2009 2015 2017 2016 2015
## [56954] 2015 2017 2017 2009 2012 2010 2009 2014 2016 2015 2012 2016 2016
## [56967] 2017 2015 2015 2017 2013 2017 2017 2015 2010 2009 2015 2016 2017
## [56980] 2012 2010 2015 2012 2015 2016 2016 2015 2007 2017 2009 2014 2016
## [56993] 2017 2017 2008 2015 2017 2010 2015 2016 2017 2016 2017 2015 2016
## [57006] 2016 2017 2008 2008 2016 2017 2017 2008 2010 2011 2016 2015 2015
## [57019] 2011 2012 2016 2016 2017 2017 2012 2007 2010 2016 2016 2017 2017
## [57032] 2017 2012 2015 2007 2011 2015 2016 2017 2012 2013 2015 2017 2011
## [57045] 2007 2014 2015 2008 2009 2010 2015 2007 2017 2011 2010 2011 2015
## [57058] 2017 2017 2013 2017 2017 2010 2012 2007 2017 2017 2017 2016 2017
## [57071] 2007 2015 2016 2010 2011 2011 2013 2014 2016 2017 2011 2007 2011
## [57084] 2013 2016 2017 2014 2015 2017 2011 2009 2012 2017 2017 2017 2017
## [57097] 2007 2013 2016 2017 2015 2016 2015 2017 2007 2008 2015 2016 2017
## [57110] 2010 2013 2016 2017 2007 2015 2014 2016 2017 2015 2017 2017 2009
## [57123] 2014 2016 2012 2008 2016 2016 2016 2007 2017 2012 2011 2015 2016
## [57136] 2016 2017 2016 2016 2017 2017 2017 2017 2012 2010 2011 2013 2015
## [57149] 2016 2011 2009 2012 2016 2015 2017 2012 2017 2012 2008 2015 2009
## [57162] 2013 2008 2008 2016 2017 2008 2007 2010 2010 2014 2009 2011 2012
## [57175] 2009 2015 2015 2016 2017 2011 2016 2016 2016 2017 2016 2017 2017
## [57188] 2011 2017 2008 2011 2013 2017 2017 2010 2017 2008 2016 2012 2017
## [57201] 2017 2012 2013 2017 2016 2017 2013 2016 2017 2015 2015 2015 2015
## [57214] 2016 2015 2017 2008 2017 2015 2017 2015 2016 2015 2017 2011 2013
## [57227] 2010 2015 2010 2015 2016 2015 2008 2010 2011 2015 2016 2011 2010
## [57240] 2015 2016 2017 2014 2016 2015 2017 2012 2007 2009 2011 2016 2010
## [57253] 2014 2015 2008 2008 2016 2012 2015 2016 2016 2016 2011 2016 2007
## [57266] 2009 2008 2016 2016 2012 2015 2016 2017 2008 2016 2007 2009 2013
## [57279] 2012 2015 2016 2017 2011 2015 2011 2015 2015 2016 2016 2010 2014
## [57292] 2016 2016 2012 2008 2012 2015 2017 2008 2009 2009 2017 2017 2008
## [57305] 2007 2013 2013 2016 2015 2015 2015 2007 2015 2017 2016 2017 2015
## [57318] 2012 2016 2015 2015 2017 2008 2011 2011 2012 2016 2016 2014 2016
## [57331] 2013 2015 2012 2015 2017 2016 2016 2014 2012 2008 2017 2007 2013
## [57344] 2007 2010 2008 2011 2016 2017 2017 2012 2009 2009 2015 2017 2017
## [57357] 2010 2016 2013 2015 2016 2015 2007 2016 2016 2016 2008 2009 2017
## [57370] 2013 2015 2014 2016 2016 2016 2011 2007 2014 2014 2011 2007 2007
## [57383] 2015 2011 2012 2016 2016 2012 2009 2010 2015 2011 2014 2017 2017
## [57396] 2008 2015 2015 2015 2010 2009 2016 2017 2010 2016 2011 2016 2008
## [57409] 2016 2009 2016 2017 2015 2016 2010 2016 2016 2011 2013 2017 2008
## [57422] 2012 2015 2007 2014 2017 2011 2007 2017 2009 2016 2015 2016 2016
## [57435] 2007 2007 2016 2017 2017 2010 2012 2013 2016 2014 2015 2016 2017
## [57448] 2010 2009 2013 2016 2007 2008 2014 2016 2016 2007 2013 2017 2009
## [57461] 2010 2017 2010 2017 2008 2012 2013 2015 2016 2016 2011 2015 2016
## [57474] 2015 2017 2011 2015 2016 2015 2016 2015 2015 2010 2009 2015 2016
## [57487] 2017 2017 2011 2017 2010 2015 2016 2009 2015 2016 2017 2007 2015
## [57500] 2016 2011 2014 2017 2007 2016 2016 2017 2015 2017 2016 2015 2015
## [57513] 2017 2015 2016 2007 2015 2016 2016 2016 2009 2009 2009 2017 2009
## [57526] 2009 2012 2015 2015 2010 2016 2010 2016 2007 2007 2017 2017 2007
## [57539] 2011 2012 2014 2015 2015 2015 2017 2008 2015 2017 2015 2013 2016
## [57552] 2017 2012 2015 2016 2016 2017 2013 2015 2012 2009 2009 2012 2014
## [57565] 2016 2016 2015 2017 2012 2010 2015 2015 2017 2012 2015 2014 2016
## [57578] 2016 2017 2014 2015 2015 2017 2010 2007 2017 2007 2013 2015 2011
## [57591] 2016 2011 2015 2016 2017 2015 2010 2015 2007 2016 2012 2015 2016
## [57604] 2015 2016 2016 2017 2012 2011 2016 2016 2010 2007 2017 2007 2009
## [57617] 2014 2012 2011 2015 2016 2013 2014 2011 2011 2017 2008 2016 2016
## [57630] 2014 2017 2017 2008 2013 2015 2017 2008 2017 2008 2017 2017 2007
## [57643] 2013 2015 2016 2017 2015 2016 2017 2010 2015 2013 2014 2017 2014
## [57656] 2008 2014 2016 2016 2017 2009 2011 2015 2017 2008 2015 2015 2009
## [57669] 2008 2016 2017 2017 2010 2017 2017 2010 2011 2007 2016 2017 2008
## [57682] 2008 2014 2017 2017 2007 2010 2013 2014 2015 2016 2015 2016 2017
## [57695] 2015 2010 2013 2016 2015 2017 2009 2016 2017 2013 2016 2016 2010
## [57708] 2010 2010 2012 2017 2016 2012 2015 2017 2015 2017 2009 2016 2007
## [57721] 2010 2011 2016 2017 2009 2010 2015 2017 2011 2012 2009 2008 2007
## [57734] 2015 2015 2017 2015 2017 2015 2008 2016 2015 2013 2014 2010 2017
## [57747] 2008 2014 2007 2016 2016 2012 2010 2011 2007 2017 2017 2015 2016
## [57760] 2007 2015 2017 2008 2010 2013 2015 2017 2015 2010 2010 2012 2007
## [57773] 2016 2012 2007 2015 2016 2010 2017 2016 2017 2007 2015 2016 2012
## [57786] 2017 2016 2017 2008 2016 2017 2016 2008 2009 2013 2012 2012 2011
## [57799] 2011 2016 2015 2008 2017 2017 2008 2009 2010 2015 2017 2011 2008
## [57812] 2016 2016 2017 2017 2016 2016 2016 2009 2008 2013 2016 2017 2017
## [57825] 2007 2016 2011 2010 2016 2017 2017 2017 2009 2016 2011 2007 2013
## [57838] 2015 2012 2008 2007 2013 2015 2015 2015 2008 2012 2014 2015 2009
## [57851] 2016 2016 2007 2014 2013 2010 2009 2016 2015 2010 2008 2008 2015
## [57864] 2016 2015 2011 2016 2017 2010 2015 2015 2015 2017 2011 2017 2017
## [57877] 2017 2016 2017 2010 2007 2008 2011 2007 2008 2016 2014 2014 2013
## [57890] 2010 2013 2007 2009 2016 2014 2017 2017 2017 2015 2017 2015 2016
## [57903] 2008 2015 2016 2016 2016 2017 2017 2015 2007 2013 2017 2017 2012
## [57916] 2012 2010 2015 2015 2016 2012 2015 2007 2008 2016 2016 2012 2014
## [57929] 2017 2014 2015 2010 2017 2017 2017 2017 2011 2012 2017 2014 2015
## [57942] 2012 2010 2016 2017 2017 2014 2016 2017 2017 2016 2015 2015 2010
## [57955] 2012 2016 2010 2008 2017 2015 2015 2007 2009 2016 2015 2017 2014
## [57968] 2016 2016 2012 2008 2012 2009 2016 2015 2007 2010 2013 2016 2016
## [57981] 2015 2016 2008 2007 2007 2008 2014 2016 2016 2017 2009 2011 2016
## [57994] 2016 2015 2017 2009 2011 2017 2016 2017 2012 2013 2015 2016 2017
## [58007] 2008 2010 2015 2016 2007 2016 2015 2015 2017 2017 2010 2015 2016
## [58020] 2015 2017 2017 2010 2017 2009 2015 2007 2011 2016 2016 2016 2015
## [58033] 2015 2015 2007 2009 2016 2015 2011 2012 2017 2017 2009 2011 2014
## [58046] 2016 2011 2012 2017 2015 2017 2010 2015 2009 2015 2017 2008 2017
## [58059] 2013 2016 2017 2013 2011 2012 2007 2017 2012 2012 2015 2008 2012
## [58072] 2017 2015 2015 2017 2007 2007 2016 2007 2010 2014 2016 2010 2008
## [58085] 2016 2016 2009 2014 2017 2009 2007 2007 2017 2010 2015 2015 2015
## [58098] 2015 2016 2012 2012 2016 2016 2008 2011 2016 2017 2010 2015 2013
## [58111] 2014 2015 2016 2016 2010 2007 2010 2015 2017 2009 2008 2007 2016
## [58124] 2017 2012 2015 2016 2010 2015 2015 2017 2016 2017 2012 2014 2009
## [58137] 2013 2012 2016 2011 2016 2017 2007 2015 2010 2009 2016 2015 2017
## [58150] 2011 2007 2008 2015 2015 2008 2008 2016 2014 2015 2015 2007 2008
## [58163] 2009 2016 2013 2015 2009 2016 2007 2015 2016 2015 2010 2008 2015
## [58176] 2017 2017 2012 2016 2017 2007 2008 2014 2017 2016 2007 2015 2016
## [58189] 2017 2017 2015 2016 2016 2017 2008 2015 2016 2016 2016 2016 2007
## [58202] 2010 2015 2009 2014 2016 2016 2016 2012 2008 2011 2010 2015 2017
## [58215] 2007 2016 2017 2017 2010 2010 2011 2008 2012 2016 2011 2016 2013
## [58228] 2017 2007 2008 2010 2008 2016 2017 2014 2007 2015 2017 2007 2010
## [58241] 2015 2017 2015 2016 2016 2013 2017 2012 2011 2009 2015 2016 2007
## [58254] 2015 2017 2012 2009 2011 2011 2015 2017 2015 2016 2013 2014 2017
## [58267] 2017 2011 2015 2017 2016 2016 2012 2015 2016 2017 2015 2009 2007
## [58280] 2015 2017 2014 2016 2010 2017 2010 2012 2010 2011 2014 2015 2016
## [58293] 2008 2009 2014 2015 2011 2013 2015 2016 2017 2013 2016 2010 2010
## [58306] 2009 2016 2017 2017 2009 2007 2013 2014 2015 2016 2008 2015 2007
## [58319] 2011 2017 2014 2016 2016 2016 2017 2015 2012 2017 2007 2015 2016
## [58332] 2017 2016 2016 2008 2015 2012 2016 2016 2016 2017 2009 2017 2010
## [58345] 2013 2016 2016 2017 2017 2011 2007 2013 2016 2011 2014 2016 2017
## [58358] 2011 2016 2016 2007 2007 2012 2008 2015 2017 2015 2017 2008 2016
## [58371] 2016 2008 2015 2015 2016 2007 2013 2016 2016 2017 2009 2014 2015
## [58384] 2016 2015 2007 2016 2016 2017 2010 2013 2014 2015 2017 2013 2015
## [58397] 2016 2016 2013 2010 2014 2015 2016 2015 2015 2012 2010 2015 2017
## [58410] 2008 2015 2011 2017 2017 2016 2017 2016 2007 2014 2016 2014 2017
## [58423] 2008 2011 2012 2015 2017 2010 2015 2013 2016 2017 2011 2015 2007
## [58436] 2015 2016 2007 2011 2013 2017 2007 2008 2016 2009 2012 2014 2011
## [58449] 2009 2008 2017 2017 2007 2015 2010 2012 2017 2013 2017 2011 2013
## [58462] 2016 2011 2007 2012 2015 2016 2016 2007 2012 2013 2014 2017 2017
## [58475] 2017 2015 2015 2015 2017 2009 2010 2014 2014 2015 2016 2015 2014
## [58488] 2008 2015 2016 2016 2017 2011 2016 2017 2014 2009 2014 2007 2016
## [58501] 2017 2007 2007 2013 2016 2009 2017 2009 2017 2017 2007 2013 2016
## [58514] 2009 2016 2017 2017 2017 2007 2009 2017 2007 2007 2014 2008 2015
## [58527] 2015 2009 2008 2017 2017 2009 2015 2016 2017 2009 2011 2013 2016
## [58540] 2013 2016 2015 2016 2016 2008 2017 2012 2013 2015 2016 2016 2016
## [58553] 2017 2008 2011 2008 2015 2017 2014 2015 2016 2016 2016 2015 2016
## [58566] 2017 2014 2012 2015 2016 2010 2013 2014 2015 2016 2008 2015 2017
## [58579] 2010 2007 2009 2014 2017 2012 2016 2015 2015 2008 2010 2015 2015
## [58592] 2008 2016 2011 2016 2014 2015 2008 2011 2013 2015 2016 2016 2016
## [58605] 2017 2015 2009 2015 2015 2014 2011 2016 2017 2015 2012 2011 2016
## [58618] 2017 2017 2017 2016 2009 2010 2016 2015 2014 2015 2007 2009 2015
## [58631] 2017 2011 2013 2012 2014 2017 2015 2016 2016 2009 2010 2016 2017
## [58644] 2017 2013 2015 2009 2017 2012 2014 2010 2017 2017 2016 2008 2008
## [58657] 2015 2016 2015 2017 2012 2015 2016 2015 2016 2015 2017 2012 2008
## [58670] 2016 2016 2016 2017 2015 2017 2015 2015 2017 2017 2008 2017 2017
## [58683] 2010 2007 2008 2017 2015 2012 2012 2015 2017 2007 2008 2012 2015
## [58696] 2008 2014 2017 2017 2010 2014 2016 2015 2017 2009 2011 2011 2016
## [58709] 2016 2012 2015 2015 2017 2017 2017 2014 2017 2009 2015 2015 2015
## [58722] 2015 2007 2017 2017 2016 2007 2016 2016 2008 2015 2011 2016 2017
## [58735] 2017 2009 2007 2009 2015 2011 2015 2009 2016 2010 2008 2015 2008
## [58748] 2017 2008 2011 2011 2009 2015 2017 2015 2008 2015 2015 2011 2009
## [58761] 2017 2008 2015 2015 2010 2012 2007 2016 2017 2016 2017 2011 2012
## [58774] 2015 2009 2007 2011 2017 2012 2016 2015 2013 2017 2012 2015 2017
## [58787] 2007 2012 2014 2017 2010 2014 2016 2017 2008 2011 2016 2017 2013
## [58800] 2016 2011 2014 2015 2017 2008 2016 2017 2008 2015 2016 2016 2016
## [58813] 2008 2015 2017 2017 2017 2010 2008 2017 2007 2017 2007 2013 2014
## [58826] 2017 2017 2007 2009 2016 2012 2012 2015 2007 2011 2015 2016 2015
## [58839] 2017 2017 2012 2015 2016 2011 2009 2017 2015 2013 2014 2016 2017
## [58852] 2011 2007 2015 2011 2016 2008 2014 2015 2016 2008 2008 2014 2007
## [58865] 2009 2016 2010 2011 2014 2015 2017 2017 2013 2017 2017 2015 2016
## [58878] 2017 2011 2015 2016 2017 2017 2017 2016 2017 2011 2009 2016 2011
## [58891] 2017 2013 2014 2017 2008 2011 2017 2008 2014 2015 2017 2016 2016
## [58904] 2017 2017 2017 2007 2015 2016 2016 2008 2010 2015 2015 2017 2007
## [58917] 2013 2015 2016 2016 2016 2016 2015 2007 2012 2014 2016 2016 2012
## [58930] 2007 2017 2010 2017 2017 2011 2013 2015 2010 2009 2012 2016 2007
## [58943] 2015 2016 2014 2013 2014 2015 2008 2016 2015 2008 2008 2017 2007
## [58956] 2015 2010 2016 2008 2010 2014 2017 2017 2008 2015 2016 2017 2015
## [58969] 2017 2010 2016 2017 2015 2009 2015 2017 2017 2016 2015 2015 2010
## [58982] 2010 2008 2007 2008 2010 2007 2016 2017 2014 2015 2013 2015 2015
## [58995] 2015 2015 2012 2009 2007 2011 2011 2015 2016 2016 2016 2017 2016
## [59008] 2016 2008 2013 2016 2011 2016 2017 2012 2017 2007 2017 2016 2007
## [59021] 2008 2015 2017 2016 2014 2017 2017 2010 2015 2008 2009 2015 2016
## [59034] 2012 2009 2008 2017 2007 2011 2015 2016 2017 2011 2008 2016 2017
## [59047] 2015 2016 2016 2017 2009 2012 2015 2014 2016 2011 2016 2012 2009
## [59060] 2014 2015 2010 2007 2013 2015 2015 2015 2017 2017 2017 2017 2017
## [59073] 2007 2012 2012 2008 2016 2012 2007 2013 2013 2017 2012 2016 2017
## [59086] 2017 2016 2017 2017 2009 2016 2016 2013 2016 2011 2007 2013 2009
## [59099] 2008 2017 2017 2017 2015 2015 2009 2011 2009 2008 2015 2015 2010
## [59112] 2013 2010 2016 2015 2012 2016 2014 2015 2015 2007 2015 2015 2015
## [59125] 2013 2017 2017 2017 2016 2017 2016 2017 2016 2012 2007 2017 2015
## [59138] 2017 2010 2015 2016 2007 2010 2015 2011 2016 2015 2010 2014 2013
## [59151] 2016 2012 2016 2009 2011 2012 2015 2015 2017 2015 2016 2015 2015
## [59164] 2017 2017 2015 2007 2016 2011 2016 2017 2011 2011 2016 2015 2011
## [59177] 2017 2016 2016 2015 2008 2014 2013 2017 2010 2010 2016 2010 2012
## [59190] 2009 2015 2014 2015 2016 2016 2017 2009 2016 2013 2015 2007 2015
## [59203] 2016 2016 2016 2017 2015 2015 2017 2009 2015 2017 2015 2011 2011
## [59216] 2010 2016 2010 2013 2008 2015 2016 2017 2008 2007 2017 2011 2015
## [59229] 2016 2016 2007 2010 2016 2016 2017 2012 2016 2007 2008 2015 2007
## [59242] 2015 2016 2015 2009 2017 2009 2015 2017 2017 2012 2010 2011 2015
## [59255] 2016 2017 2017 2009 2017 2008 2015 2016 2016 2015 2017 2017 2017
## [59268] 2017 2012 2014 2015 2015 2015 2009 2010 2007 2016 2007 2015 2015
## [59281] 2007 2008 2015 2016 2016 2017 2017 2007 2007 2016 2016 2017 2009
## [59294] 2008 2016 2008 2007 2013 2014 2017 2008 2009 2010 2008 2009 2008
## [59307] 2007 2007 2009 2016 2017 2011 2017 2017 2010 2011 2016 2015 2015
## [59320] 2008 2012 2008 2017 2015 2017 2015 2017 2012 2017 2015 2017 2009
## [59333] 2010 2011 2016 2017 2017 2017 2012 2010 2017 2017 2017 2017 2012
## [59346] 2011 2012 2011 2012 2013 2008 2013 2009 2009 2007 2007 2016 2016
## [59359] 2017 2017 2017 2017 2011 2007 2017 2009 2016 2016 2015 2008 2017
## [59372] 2017 2015 2016 2017 2017 2010 2008 2008 2016 2015 2017 2015 2011
## [59385] 2015 2015 2008 2016 2012 2009 2014 2016 2016 2012 2008 2007 2009
## [59398] 2014 2016 2016 2011 2007 2015 2015 2017 2015 2016 2015 2016 2017
## [59411] 2016 2017 2009 2015 2016 2016 2009 2010 2016 2007 2017 2007 2015
## [59424] 2016 2009 2007 2010 2013 2015 2015 2011 2011 2009 2015 2016 2016
## [59437] 2010 2010 2015 2016 2010 2015 2016 2017 2008 2016 2015 2017 2017
## [59450] 2017 2012 2013 2008 2015 2016 2016 2015 2017 2007 2016 2016 2016
## [59463] 2015 2015 2009 2010 2009 2009 2012 2010 2011 2013 2015 2016 2017
## [59476] 2010 2014 2010 2017 2007 2015 2017 2014 2015 2011 2015 2010 2016
## [59489] 2016 2012 2013 2015 2016 2015 2012 2013 2015 2015 2013 2014 2015
## [59502] 2011 2012 2016 2012 2011 2013 2011 2015 2012 2016 2015 2015 2015
## [59515] 2017 2008 2008 2015 2016 2015 2014 2009 2015 2015 2017 2009 2012
## [59528] 2008 2013 2015 2016 2010 2007 2008 2016 2016 2016 2015 2017 2015
## [59541] 2017 2011 2014 2016 2010 2013 2013 2015 2015 2015 2008 2013 2014
## [59554] 2016 2011 2016 2015 2010 2007 2010 2011 2012 2013 2013 2015 2010
## [59567] 2014 2016 2008 2011 2016 2017 2013 2015 2015 2008 2016 2017 2017
## [59580] 2016 2007 2016 2016 2016 2015 2016 2017 2007 2010 2007 2010 2016
## [59593] 2016 2016 2010 2010 2014 2016 2007 2013 2015 2017 2017 2007 2011
## [59606] 2017 2016 2017 2017 2010 2015 2017 2009 2008 2013 2017 2015 2014
## [59619] 2017 2015 2010 2015 2010 2013 2017 2010 2010 2009 2009 2016 2016
## [59632] 2007 2010 2011 2012 2017 2017 2016 2017 2016 2017 2016 2007 2016
## [59645] 2017 2017 2011 2010 2015 2016 2015 2016 2012 2008 2013 2013 2013
## [59658] 2017 2008 2016 2016 2015 2016 2016 2015 2016 2015 2015 2016 2007
## [59671] 2008 2015 2012 2008 2014 2016 2016 2008 2010 2013 2013 2013 2015
## [59684] 2017 2017 2017 2015 2017 2008 2016 2015 2016 2016 2014 2016 2008
## [59697] 2016 2009 2007 2012 2017 2012 2016 2016 2017 2017 2017 2011 2015
## [59710] 2015 2012 2009 2016 2017 2016 2017 2010 2015 2007 2009 2016 2015
## [59723] 2014 2015 2016 2015 2017 2008 2016 2017 2009 2015 2015 2015 2017
## [59736] 2011 2015 2016 2015 2016 2016 2010 2016 2007 2016 2017 2017 2017
## [59749] 2015 2017 2015 2017 2012 2015 2016 2016 2017 2011 2009 2017 2011
## [59762] 2015 2016 2008 2010 2016 2010 2017 2017 2017 2016 2015 2011 2012
## [59775] 2015 2009 2016 2016 2017 2010 2008 2015 2017 2011 2016 2012 2008
## [59788] 2015 2016 2010 2012 2010 2013 2015 2015 2015 2017 2012 2010 2011
## [59801] 2014 2017 2016 2017 2009 2010 2010 2016 2017 2015 2007 2012 2009
## [59814] 2014 2016 2010 2010 2013 2015 2008 2011 2010 2016 2007 2016 2013
## [59827] 2015 2010 2009 2013 2015 2017 2008 2013 2015 2015 2009 2015 2016
## [59840] 2008 2016 2015 2014 2013 2013 2015 2017 2012 2010 2010 2015 2017
## [59853] 2008 2015 2017 2017 2016 2015 2017 2011 2013 2016 2012 2011 2013
## [59866] 2016 2017 2015 2010 2011 2012 2016 2015 2015 2016 2013 2014 2015
## [59879] 2007 2010 2011 2017 2017 2008 2013 2016 2017 2017 2017 2008 2007
## [59892] 2012 2011 2017 2017 2012 2007 2012 2014 2015 2007 2008 2010 2015
## [59905] 2016 2017 2008 2017 2013 2007 2008 2010 2015 2015 2017 2015 2016
## [59918] 2017 2017 2015 2016 2017 2016 2017 2016 2009 2017 2017 2017 2016
## [59931] 2016 2017 2009 2014 2016 2011 2008 2007 2014 2016 2017 2007 2016
## [59944] 2015 2011 2015 2016 2017 2007 2011 2015 2012 2016 2016 2017 2017
## [59957] 2012 2013 2017 2010 2017 2017 2017 2012 2013 2017 2011 2008 2011
## [59970] 2017 2013 2017 2012 2009 2015 2016 2017 2011 2017 2009 2017 2013
## [59983] 2016 2015 2016 2007 2010 2008 2012 2017 2011 2015 2010 2013 2017
## [59996] 2015 2017 2009 2012 2008 2015 2015 2010 2016 2016 2015 2017 2016
## [60009] 2010 2011 2011 2013 2013 2015 2016 2016 2017 2015 2016 2016 2009
## [60022] 2016 2015 2007 2010 2011 2015 2017 2017 2016 2015 2015 2016 2016
## [60035] 2008 2016 2016 2015 2017 2017 2010 2016 2017 2008 2015 2011 2017
## [60048] 2007 2011 2007 2011 2011 2009 2013 2016 2016 2010 2008 2015 2017
## [60061] 2017 2016 2017 2008 2013 2016 2007 2017 2016 2010 2017 2010 2013
## [60074] 2016 2010 2010 2011 2015 2009 2017 2007 2009 2015 2016 2016 2014
## [60087] 2015 2017 2015 2016 2016 2016 2017 2017 2017 2010 2012 2014 2015
## [60100] 2017 2016 2017 2007 2015 2007 2015 2012 2007 2013 2015 2016 2017
## [60113] 2015 2016 2008 2015 2015 2015 2016 2017 2008 2009 2009 2015 2015
## [60126] 2008 2013 2017 2009 2012 2015 2017 2008 2007 2007 2015 2016 2016
## [60139] 2016 2015 2017 2017 2008 2012 2015 2012 2007 2015 2007 2015 2011
## [60152] 2014 2015 2016 2012 2014 2017 2015 2017 2011 2010 2016 2017 2009
## [60165] 2014 2008 2013 2014 2016 2017 2008 2016 2016 2015 2016 2017 2017
## [60178] 2008 2008 2011 2016 2017 2011 2017 2017 2016 2015 2015 2017 2017
## [60191] 2008 2015 2015 2015 2015 2007 2017 2015 2016 2017 2017 2013 2015
## [60204] 2017 2010 2015 2016 2017 2007 2008 2010 2013 2015 2010 2010 2012
## [60217] 2014 2007 2014 2017 2008 2007 2007 2009 2013 2016 2016 2015 2016
## [60230] 2008 2017 2017 2017 2008 2016 2013 2014 2015 2015 2013 2014 2016
## [60243] 2015 2011 2014 2015 2015 2017 2013 2016 2016 2017 2013 2017 2015
## [60256] 2009 2009 2016 2017 2009 2015 2013 2014 2015 2017 2009 2015 2013
## [60269] 2015 2016 2012 2015 2016 2016 2014 2016 2015 2009 2011 2011 2010
## [60282] 2016 2016 2016 2016 2017 2008 2007 2009 2016 2007 2015 2015 2016
## [60295] 2017 2011 2009 2014 2016 2017 2010 2007 2015 2016 2017 2016 2016
## [60308] 2017 2012 2009 2017 2008 2014 2017 2011 2013 2011 2008 2009 2015
## [60321] 2017 2013 2010 2007 2010 2013 2010 2011 2012 2015 2016 2008 2015
## [60334] 2016 2015 2017 2010 2015 2015 2013 2015 2016 2017 2014 2015 2016
## [60347] 2017 2017 2015 2009 2012 2010 2012 2015 2015 2015 2016 2008 2007
## [60360] 2013 2014 2015 2015 2016 2017 2017 2009 2007 2008 2014 2017 2015
## [60373] 2009 2015 2017 2007 2008 2017 2010 2012 2016 2017 2016 2015 2016
## [60386] 2016 2015 2017 2010 2013 2011 2011 2015 2015 2016 2017 2008 2007
## [60399] 2014 2015 2016 2016 2017 2014 2016 2016 2010 2015 2013 2017 2015
## [60412] 2008 2016 2014 2015 2017 2008 2012 2015 2015 2012 2011 2011 2013
## [60425] 2015 2017 2015 2017 2007 2009 2016 2008 2012 2014 2017 2009 2008
## [60438] 2013 2016 2016 2012 2011 2016 2017 2008 2010 2010 2015 2017 2015
## [60451] 2010 2016 2011 2017 2017 2013 2008 2008 2011 2008 2013 2015 2016
## [60464] 2016 2015 2016 2008 2012 2015 2011 2012 2013 2014 2008 2014 2016
## [60477] 2017 2015 2012 2007 2012 2017 2017 2008 2008 2017 2012 2009 2017
## [60490] 2017 2008 2009 2012 2011 2011 2008 2009 2016 2016 2010 2012 2016
## [60503] 2008 2010 2015 2017 2016 2015 2012 2011 2016 2008 2009 2011 2007
## [60516] 2016 2016 2016 2016 2015 2016 2009 2016 2010 2009 2011 2010 2014
## [60529] 2016 2016 2016 2009 2015 2017 2015 2016 2016 2014 2015 2016 2015
## [60542] 2015 2017 2013 2016 2016 2017 2012 2011 2017 2011 2013 2015 2016
## [60555] 2017 2007 2008 2012 2010 2016 2015 2017 2009 2009 2017 2017 2016
## [60568] 2015 2009 2007 2012 2017 2011 2016 2011 2014 2016 2007 2008 2015
## [60581] 2009 2009 2016 2017 2017 2007 2015 2015 2015 2014 2015 2015 2015
## [60594] 2017 2008 2015 2017 2017 2017 2007 2007 2012 2016 2016 2010 2015
## [60607] 2015 2016 2012 2017 2009 2015 2017 2012 2011 2014 2015 2016 2015
## [60620] 2008 2016 2017 2017 2016 2016 2017 2010 2013 2015 2012 2015 2011
## [60633] 2011 2007 2010 2015 2017 2017 2010 2015 2012 2012 2012 2010 2009
## [60646] 2012 2015 2016 2008 2011 2009 2015 2016 2017 2008 2010 2010 2010
## [60659] 2014 2015 2015 2010 2008 2017 2007 2013 2013 2017 2016 2015 2017
## [60672] 2013 2016 2017 2008 2009 2015 2016 2007 2015 2016 2007 2016 2017
## [60685] 2008 2014 2016 2015 2016 2016 2016 2017 2015 2015 2015 2015 2016
## [60698] 2010 2007 2009 2010 2015 2016 2009 2013 2017 2012 2016 2009 2008
## [60711] 2013 2017 2017 2011 2011 2010 2011 2017 2007 2015 2015 2015 2017
## [60724] 2010 2015 2016 2016 2017 2017 2016 2015 2016 2007 2016 2010 2016
## [60737] 2016 2008 2007 2015 2017 2009 2010 2016 2016 2016 2015 2017 2009
## [60750] 2013 2015 2017 2013 2015 2016 2017 2014 2014 2008 2009 2017 2008
## [60763] 2017 2009 2017 2016 2017 2016 2016 2015 2012 2010 2016 2015 2009
## [60776] 2015 2016 2016 2013 2016 2016 2017 2017 2012 2012 2015 2016 2017
## [60789] 2016 2010 2014 2015 2017 2017 2014 2012 2008 2013 2008 2009 2014
## [60802] 2015 2015 2017 2011 2007 2011 2015 2017 2015 2016 2015 2017 2008
## [60815] 2007 2011 2015 2015 2007 2009 2016 2016 2017 2017 2011 2017 2017
## [60828] 2009 2016 2016 2009 2007 2009 2011 2010 2017 2016 2016 2015 2015
## [60841] 2017 2010 2014 2016 2010 2011 2015 2017 2011 2008 2014 2017 2009
## [60854] 2013 2015 2017 2010 2015 2016 2017 2017 2017 2017 2016 2009 2014
## [60867] 2016 2017 2015 2017 2012 2014 2016 2009 2008 2015 2016 2015 2017
## [60880] 2017 2012 2009 2016 2016 2017 2010 2015 2015 2015 2016 2016 2015
## [60893] 2010 2015 2015 2015 2017 2016 2017 2017 2016 2011 2015 2011 2013
## [60906] 2017 2015 2016 2017 2017 2016 2013 2015 2016 2009 2009 2007 2009
## [60919] 2013 2015 2016 2017 2015 2016 2016 2011 2016 2017 2009 2016 2017
## [60932] 2008 2014 2015 2015 2013 2008 2016 2016 2016 2016 2017 2008 2015
## [60945] 2015 2017 2009 2008 2014 2016 2017 2007 2017 2017 2017 2011 2010
## [60958] 2013 2016 2007 2013 2015 2017 2008 2007 2014 2017 2015 2015 2012
## [60971] 2009 2014 2016 2007 2015 2008 2017 2013 2013 2014 2015 2017 2007
## [60984] 2015 2011 2008 2012 2016 2017 2011 2010 2015 2011 2010 2013 2017
## [60997] 2010 2009 2008 2009 2014 2017 2015 2017 2016 2009 2015 2015 2007
## [61010] 2012 2009 2015 2016 2015 2008 2008 2007 2013 2016 2016 2016 2017
## [61023] 2007 2017 2017 2017 2010 2009 2011 2013 2014 2009 2016 2015 2012
## [61036] 2008 2016 2009 2014 2016 2017 2007 2017 2008 2011 2016 2016 2010
## [61049] 2016 2017 2012 2014 2016 2016 2017 2011 2007 2014 2007 2017 2008
## [61062] 2008 2014 2014 2011 2010 2016 2015 2016 2017 2007 2009 2009 2015
## [61075] 2017 2017 2009 2011 2016 2017 2017 2017 2016 2015 2013 2015 2016
## [61088] 2016 2011 2013 2016 2009 2015 2017 2017 2013 2011 2010 2011 2016
## [61101] 2008 2016 2009 2015 2015 2012 2008 2009 2016 2016 2014 2014 2015
## [61114] 2016 2017 2017 2016 2016 2017 2016 2015 2016 2017 2017 2017 2010
## [61127] 2017 2015 2011 2016 2017 2012 2015 2016 2016 2016 2017 2017 2013
## [61140] 2014 2015 2017 2008 2015 2017 2016 2017 2017 2011 2016 2009 2015
## [61153] 2016 2009 2015 2017 2008 2010 2007 2012 2007 2009 2016 2017 2016
## [61166] 2015 2016 2017 2007 2017 2009 2017 2016 2010 2010 2009 2013 2016
## [61179] 2009 2017 2007 2008 2017 2017 2008 2013 2015 2011 2010 2009 2016
## [61192] 2012 2017 2012 2015 2015 2009 2007 2017 2010 2015 2015 2016 2016
## [61205] 2016 2017 2012 2010 2014 2012 2009 2015 2016 2017 2009 2015 2011
## [61218] 2014 2015 2016 2016 2016 2008 2012 2011 2011 2010 2011 2009 2015
## [61231] 2013 2016 2007 2016 2016 2017 2007 2017 2014 2015 2008 2009 2011
## [61244] 2008 2017 2007 2007 2015 2009 2010 2011 2008 2015 2017 2017 2009
## [61257] 2008 2014 2016 2015 2008 2016 2011 2016 2013 2015 2016 2017 2017
## [61270] 2015 2017 2009 2007 2009 2017 2011 2014 2007 2011 2016 2015 2013
## [61283] 2011 2015 2016 2017 2017 2017 2016 2011 2013 2015 2016 2016 2012
## [61296] 2013 2017 2015 2015 2017 2011 2016 2008 2015 2015 2017 2015 2015
## [61309] 2015 2017 2017 2012 2016 2015 2015 2017 2010 2015 2017 2017 2016
## [61322] 2017 2016 2015 2010 2010 2016 2016 2016 2015 2016 2007 2017 2017
## [61335] 2016 2011 2016 2017 2012 2013 2017 2013 2015 2017 2015 2012 2009
## [61348] 2016 2008 2017 2014 2014 2017 2017 2011 2011 2016 2017 2017 2007
## [61361] 2015 2009 2015 2015 2008 2008 2008 2011 2012 2017 2016 2012 2016
## [61374] 2017 2017 2010 2016 2017 2016 2017 2008 2008 2011 2013 2015 2015
## [61387] 2016 2011 2015 2011 2011 2013 2015 2017 2016 2012 2016 2017 2010
## [61400] 2011 2015 2016 2010 2008 2011 2014 2013 2016 2017 2017 2017 2007
## [61413] 2016 2017 2014 2017 2012 2012 2017 2017 2012 2016 2015 2010 2016
## [61426] 2011 2016 2015 2009 2012 2016 2016 2010 2012 2015 2015 2015 2015
## [61439] 2012 2012 2009 2017 2016 2016 2008 2014 2011 2014 2016 2015 2015
## [61452] 2017 2010 2007 2007 2010 2016 2017 2011 2016 2017 2007 2013 2016
## [61465] 2008 2014 2017 2010 2017 2015 2009 2014 2015 2016 2016 2017 2017
## [61478] 2007 2010 2014 2015 2015 2015 2017 2016 2017 2011 2015 2016 2008
## [61491] 2013 2016 2014 2015 2016 2017 2017 2015 2010 2013 2015 2016 2008
## [61504] 2008 2015 2016 2017 2017 2012 2010 2013 2015 2017 2013 2017 2017
## [61517] 2010 2009 2008 2013 2013 2012 2013 2009 2008 2015 2015 2015 2016
## [61530] 2015 2009 2011 2007 2009 2013 2013 2015 2016 2009 2013 2015 2015
## [61543] 2016 2016 2017 2008 2015 2017 2012 2014 2017 2017 2017 2012 2013
## [61556] 2016 2007 2010 2011 2015 2016 2016 2017 2017 2016 2015 2017 2015
## [61569] 2016 2015 2015 2017 2012 2016 2010 2014 2015 2016 2016 2007 2013
## [61582] 2010 2016 2016 2008 2009 2014 2007 2016 2017 2012 2011 2010 2012
## [61595] 2013 2017 2013 2015 2010 2012 2015 2016 2015 2017 2009 2017 2012
## [61608] 2013 2013 2016 2015 2014 2014 2016 2016 2008 2013 2011 2007 2010
## [61621] 2016 2015 2011 2015 2017 2017 2016 2013 2016 2009 2007 2016 2015
## [61634] 2016 2017 2011 2010 2015 2017 2015 2015 2015 2016 2015 2010 2008
## [61647] 2015 2010 2017 2017 2015 2016 2008 2017 2007 2015 2017 2012 2008
## [61660] 2007 2014 2011 2015 2016 2008 2015 2011 2016 2007 2011 2017 2009
## [61673] 2008 2015 2017 2016 2016 2017 2017 2017 2015 2015 2017 2016 2016
## [61686] 2015 2017 2012 2015 2014 2017 2009 2014 2015 2016 2010 2010 2015
## [61699] 2017 2017 2010 2009 2010 2009 2016 2017 2008 2008 2016 2015 2009
## [61712] 2007 2007 2008 2016 2017 2017 2017 2009 2007 2014 2014 2015 2017
## [61725] 2013 2017 2008 2013 2015 2017 2017 2012 2017 2012 2013 2008 2016
## [61738] 2016 2017 2009 2007 2015 2011 2015 2017 2016 2012 2010 2010 2017
## [61751] 2015 2016 2007 2008 2015 2016 2015 2012 2017 2015 2017 2017 2010
## [61764] 2009 2016 2017 2011 2016 2017 2014 2017 2017 2010 2013 2015 2011
## [61777] 2015 2017 2015 2016 2011 2016 2016 2016 2012 2007 2012 2010 2016
## [61790] 2017 2011 2008 2012 2009 2013 2016 2017 2010 2015 2017 2015 2016
## [61803] 2014 2015 2016 2017 2017 2008 2013 2015 2016 2015 2011 2014 2014
## [61816] 2007 2016 2017 2017 2010 2017 2015 2016 2017 2017 2016 2017 2017
## [61829] 2007 2009 2016 2017 2008 2013 2016 2017 2007 2011 2015 2016 2017
## [61842] 2017 2014 2016 2016 2016 2016 2007 2010 2017 2015 2017 2017 2015
## [61855] 2016 2013 2015 2017 2011 2010 2013 2016 2016 2015 2017 2008 2013
## [61868] 2013 2015 2011 2017 2016 2015 2015 2008 2009 2015 2017 2011 2012
## [61881] 2016 2017 2017 2011 2007 2011 2014 2016 2015 2017 2015 2017 2009
## [61894] 2014 2014 2008 2016 2010 2008 2007 2011 2017 2013 2011 2010 2017
## [61907] 2010 2016 2011 2011 2010 2016 2015 2014 2017 2017 2017 2017 2015
## [61920] 2016 2017 2007 2013 2009 2016 2016 2016 2016 2009 2013 2008 2010
## [61933] 2016 2013 2015 2015 2007 2009 2012 2013 2014 2015 2016 2017 2017
## [61946] 2007 2008 2008 2015 2016 2017 2012 2014 2016 2017 2017 2012 2011
## [61959] 2015 2017 2017 2011 2012 2011 2013 2013 2016 2008 2009 2013 2017
## [61972] 2015 2007 2014 2015 2015 2015 2016 2007 2013 2015 2016 2007 2007
## [61985] 2016 2016 2016 2011 2015 2016 2013 2016 2008 2014 2010 2009 2017
## [61998] 2008 2012 2014 2017 2008 2011 2016 2016 2017 2008 2010 2015 2016
## [62011] 2009 2016 2017 2012 2007 2015 2015 2009 2014 2011 2015 2008 2007
## [62024] 2016 2007 2007 2009 2016 2016 2017 2017 2008 2014 2016 2007 2015
## [62037] 2016 2016 2011 2007 2008 2017 2010 2015 2015 2017 2017 2008 2012
## [62050] 2009 2010 2013 2015 2012 2010 2015 2016 2016 2009 2017 2015 2012
## [62063] 2010 2015 2017 2015 2008 2008 2015 2017 2013 2014 2017 2010 2010
## [62076] 2011 2012 2015 2016 2015 2016 2008 2013 2016 2016 2015 2007 2008
## [62089] 2008 2016 2016 2016 2016 2009 2015 2016 2017 2012 2008 2016 2011
## [62102] 2012 2012 2017 2014 2015 2016 2009 2016 2015 2017 2016 2017 2010
## [62115] 2017 2013 2015 2016 2015 2016 2015 2017 2015 2016 2016 2016 2017
## [62128] 2017 2008 2007 2008 2016 2017 2007 2007 2015 2015 2016 2016 2016
## [62141] 2017 2010 2009 2015 2012 2012 2007 2015 2017 2007 2014 2014 2016
## [62154] 2012 2016 2015 2015 2015 2016 2016 2017 2015 2016 2016 2015 2016
## [62167] 2017 2009 2011 2010 2015 2017 2010 2008 2015 2016 2008 2009 2015
## [62180] 2012 2016 2015 2008 2007 2009 2016 2017 2008 2007 2011 2013 2015
## [62193] 2012 2016 2017 2011 2015 2017 2016 2016 2017 2015 2017 2016 2007
## [62206] 2014 2017 2016 2016 2016 2017 2017 2017 2013 2013 2017 2017 2017
## [62219] 2011 2015 2016 2017 2014 2015 2010 2009 2012 2007 2008 2014 2016
## [62232] 2015 2008 2007 2013 2013 2017 2012 2012 2016 2016 2014 2016 2007
## [62245] 2017 2010 2016 2014 2015 2017 2016 2016 2015 2016 2017 2017 2016
## [62258] 2009 2016 2016 2017 2015 2007 2009 2014 2016 2016 2016 2008 2007
## [62271] 2017 2007 2016 2016 2007 2014 2015 2007 2014 2016 2017 2009 2012
## [62284] 2017 2017 2012 2009 2009 2015 2015 2015 2017 2017 2011 2010 2015
## [62297] 2011 2011 2015 2014 2015 2012 2012 2011 2016 2016 2015 2017 2016
## [62310] 2017 2016 2017 2010 2015 2016 2016 2017 2011 2007 2014 2013 2007
## [62323] 2014 2016 2009 2008 2010 2007 2015 2017 2017 2017 2007 2012 2016
## [62336] 2016 2009 2015 2016 2017 2017 2009 2009 2016 2008 2011 2010 2015
## [62349] 2017 2016 2012 2016 2009 2010 2013 2017 2017 2008 2007 2015 2017
## [62362] 2017 2013 2015 2015 2017 2015 2014 2015 2009 2015 2007 2016 2017
## [62375] 2011 2011 2013 2015 2015 2017 2010 2008 2013 2013 2016 2012 2016
## [62388] 2015 2014 2015 2017 2017 2017 2012 2015 2017 2017 2015 2015 2011
## [62401] 2016 2007 2015 2009 2008 2017 2017 2009 2008 2007 2014 2016 2017
## [62414] 2012 2007 2015 2017 2017 2017 2012 2011 2017 2015 2017 2017 2011
## [62427] 2012 2010 2010 2016 2013 2017 2015 2011 2015 2017 2009 2010 2016
## [62440] 2016 2008 2016 2017 2017 2015 2017 2014 2017 2008 2010 2015 2015
## [62453] 2016 2016 2016 2016 2017 2010 2011 2011 2017 2011 2010 2017 2013
## [62466] 2016 2012 2014 2016 2016 2008 2009 2013 2009 2010 2016 2009 2015
## [62479] 2016 2016 2016 2017 2011 2008 2011 2016 2015 2017 2017 2008 2011
## [62492] 2014 2015 2015 2016 2016 2007 2010 2016 2012 2007 2016 2017 2011
## [62505] 2010 2013 2008 2016 2017 2017 2008 2010 2010 2014 2016 2007 2016
## [62518] 2017 2015 2015 2016 2017 2016 2016 2016 2016 2017 2011 2007 2011
## [62531] 2012 2012 2016 2016 2010 2016 2017 2017 2008 2013 2015 2015 2007
## [62544] 2010 2016 2017 2007 2015 2008 2008 2016 2016 2009 2012 2015 2016
## [62557] 2016 2016 2017 2017 2011 2015 2015 2017 2017 2015 2011 2016 2017
## [62570] 2008 2008 2015 2016 2009 2016 2007 2011 2008 2017 2017 2007 2016
## [62583] 2015 2007 2015 2011 2011 2015 2016 2009 2015 2016 2016 2008 2015
## [62596] 2016 2016 2016 2016 2017 2008 2007 2009 2011 2013 2016 2017 2012
## [62609] 2015 2017 2015 2007 2008 2014 2015 2015 2016 2017 2007 2015 2011
## [62622] 2007 2010 2014 2016 2016 2011 2007 2009 2015 2011 2010 2009 2013
## [62635] 2014 2017 2009 2010 2013 2017 2014 2016 2017 2014 2016 2017 2007
## [62648] 2011 2008 2016 2017 2014 2009 2017 2017 2009 2016 2016 2016 2012
## [62661] 2009 2008 2010 2015 2016 2013 2016 2016 2011 2017 2010 2008 2015
## [62674] 2015 2017 2015 2015 2017 2010 2016 2012 2014 2010 2015 2016 2017
## [62687] 2007 2013 2015 2007 2017 2016 2012 2016 2015 2017 2011 2008 2016
## [62700] 2016 2016 2016 2011 2008 2017 2017 2010 2016 2015 2016 2017 2009
## [62713] 2010 2007 2010 2016 2012 2015 2011 2015 2007 2012 2015 2016 2016
## [62726] 2010 2015 2008 2012 2016 2007 2009 2017 2007 2015 2016 2009 2008
## [62739] 2016 2017 2015 2016 2017 2017 2017 2010 2009 2009 2013 2010 2015
## [62752] 2016 2017 2017 2017 2017 2009 2011 2013 2015 2017 2016 2016 2017
## [62765] 2017 2017 2008 2017 2007 2007 2014 2009 2011 2009 2017 2009 2017
## [62778] 2008 2015 2016 2007 2017 2009 2011 2015 2015 2017 2013 2014 2016
## [62791] 2015 2016 2017 2013 2015 2016 2008 2012 2017 2015 2016 2008 2016
## [62804] 2016 2017 2014 2011 2015 2017 2007 2016 2017 2015 2010 2016 2015
## [62817] 2017 2015 2015 2015 2017 2017 2015 2015 2016 2014 2015 2011 2015
## [62830] 2016 2017 2017 2010 2016 2017 2017 2017 2009 2009 2016 2017 2016
## [62843] 2009 2012 2013 2015 2017 2017 2013 2015 2012 2011 2015 2016 2016
## [62856] 2017 2009 2015 2017 2017 2008 2007 2014 2010 2016 2017 2017 2010
## [62869] 2016 2017 2014 2015 2015 2016 2013 2012 2009 2013 2013 2016 2017
## [62882] 2017 2011 2010 2017 2017 2008 2009 2008 2015 2016 2016 2017 2012
## [62895] 2013 2015 2015 2008 2017 2017 2013 2016 2017 2016 2013 2008 2016
## [62908] 2008 2016 2016 2008 2010 2015 2016 2017 2007 2011 2009 2010 2013
## [62921] 2015 2010 2007 2016 2008 2011 2012 2015 2016 2016 2017 2010 2015
## [62934] 2017 2017 2015 2017 2008 2008 2009 2015 2017 2016 2014 2015 2016
## [62947] 2007 2015 2017 2011 2009 2011 2016 2017 2013 2012 2009 2008 2015
## [62960] 2015 2007 2014 2009 2013 2017 2012 2011 2008 2014 2016 2016 2017
## [62973] 2017 2013 2015 2016 2016 2017 2009 2011 2007 2013 2014 2016 2014
## [62986] 2015 2017 2017 2017 2016 2010 2015 2016 2017 2011 2012 2016 2017
## [62999] 2009 2014 2016 2009 2014 2015 2016 2017 2017 2015 2017 2008 2015
## [63012] 2017 2010 2016 2015 2010 2017 2008 2014 2015 2015 2017 2017 2015
## [63025] 2010 2013 2010 2013 2014 2012 2008 2015 2015 2009 2009 2015 2016
## [63038] 2017 2009 2016 2011 2015 2016 2008 2017 2017 2007 2016 2009 2017
## [63051] 2015 2015 2016 2017 2017 2012 2007 2008 2017 2007 2012 2013 2015
## [63064] 2011 2015 2016 2015 2016 2015 2008 2017 2007 2012 2011 2015 2016
## [63077] 2012 2017 2014 2017 2010 2013 2017 2011 2013 2012 2017 2013 2016
## [63090] 2012 2016 2011 2016 2017 2013 2017 2017 2013 2016 2016 2009 2008
## [63103] 2014 2015 2016 2016 2013 2013 2014 2015 2016 2016 2012 2013 2016
## [63116] 2016 2008 2008 2008 2009 2014 2015 2016 2017 2015 2017 2009 2015
## [63129] 2017 2007 2016 2017 2015 2011 2015 2015 2010 2013 2017 2008 2012
## [63142] 2014 2015 2013 2008 2016 2016 2013 2015 2009 2017 2016 2017 2013
## [63155] 2017 2012 2017 2017 2017 2013 2017 2010 2017 2017 2010 2013 2015
## [63168] 2009 2016 2017 2008 2014 2017 2015 2016 2016 2011 2014 2016 2016
## [63181] 2016 2015 2016 2017 2009 2010 2013 2017 2017 2015 2015 2015 2010
## [63194] 2011 2010 2009 2016 2016 2017 2011 2014 2016 2016 2017 2008 2011
## [63207] 2009 2015 2017 2015 2016 2017 2010 2016 2008 2015 2017 2010 2017
## [63220] 2007 2007 2016 2017 2014 2017 2011 2010 2009 2009 2013 2017 2012
## [63233] 2010 2013 2015 2016 2017 2017 2010 2012 2007 2014 2016 2009 2007
## [63246] 2010 2010 2016 2017 2012 2016 2016 2008 2015 2010 2008 2017 2017
## [63259] 2008 2014 2015 2017 2012 2011 2015 2010 2008 2015 2016 2015 2008
## [63272] 2014 2010 2017 2012 2008 2015 2016 2016 2014 2016 2017 2017 2011
## [63285] 2011 2014 2015 2008 2010 2011 2016 2015 2016 2016 2016 2015 2016
## [63298] 2013 2013 2014 2016 2014 2011 2014 2007 2015 2016 2016 2011 2016
## [63311] 2015 2011 2015 2008 2016 2008 2008 2007 2007 2012 2009 2009 2016
## [63324] 2016 2017 2017 2007 2014 2016 2017 2017 2007 2010 2016 2016 2017
## [63337] 2009 2016 2017 2015 2013 2014 2016 2015 2015 2008 2012 2009 2012
## [63350] 2013 2014 2015 2016 2016 2011 2015 2009 2010 2016 2016 2010 2010
## [63363] 2010 2016 2017 2016 2016 2016 2010 2009 2013 2017 2017 2015 2017
## [63376] 2016 2011 2015 2017 2017 2011 2014 2016 2017 2017 2009 2012 2017
## [63389] 2017 2016 2016 2017 2014 2014 2009 2011 2015 2017 2015 2017 2015
## [63402] 2007 2015 2012 2011 2010 2008 2010 2015 2016 2016 2017 2008 2010
## [63415] 2013 2015 2017 2015 2012 2015 2017 2016 2016 2017 2011 2017 2010
## [63428] 2017 2017 2008 2015 2017 2010 2016 2017 2015 2010 2009 2008 2012
## [63441] 2013 2016 2015 2016 2009 2009 2014 2016 2008 2016 2017 2009 2007
## [63454] 2015 2015 2016 2008 2016 2011 2010 2014 2015 2014 2016 2010 2016
## [63467] 2012 2009 2015 2017 2013 2015 2015 2015 2017 2009 2014 2017 2010
## [63480] 2016 2007 2016 2016 2013 2017 2013 2016 2011 2015 2016 2015 2017
## [63493] 2008 2008 2013 2011 2013 2015 2016 2017 2017 2011 2016 2015 2013
## [63506] 2017 2007 2015 2017 2012 2007 2013 2017 2008 2016 2017 2016 2016
## [63519] 2016 2011 2013 2009 2017 2012 2016 2014 2017 2017 2008 2015 2016
## [63532] 2015 2008 2016 2017 2010 2008 2014 2017 2012 2007 2016 2015 2008
## [63545] 2016 2017 2016 2017 2013 2007 2010 2014 2014 2016 2017 2017 2017
## [63558] 2010 2017 2011 2011 2017 2016 2008 2008 2008 2010 2014 2016 2008
## [63571] 2017 2016 2012 2007 2015 2007 2008 2008 2010 2009 2016 2016 2007
## [63584] 2011 2014 2016 2015 2016 2017 2007 2009 2008 2016 2009 2016 2015
## [63597] 2007 2017 2010 2013 2011 2012 2011 2015 2017 2008 2009 2016 2016
## [63610] 2016 2008 2007 2017 2017 2014 2015 2011 2016 2015 2016 2015 2016
## [63623] 2017 2013 2013 2007 2010 2011 2017 2017 2008 2015 2016 2016 2016
## [63636] 2011 2010 2011 2012 2010 2013 2014 2015 2015 2010 2012 2014 2016
## [63649] 2017 2017 2017 2008 2008 2010 2009 2007 2007 2007 2009 2016 2015
## [63662] 2017 2008 2014 2014 2017 2015 2010 2017 2017 2010 2012 2014 2014
## [63675] 2013 2014 2015 2017 2010 2015 2007 2017 2010 2011 2016 2015 2010
## [63688] 2011 2017 2008 2007 2016 2016 2009 2015 2016 2017 2013 2016 2011
## [63701] 2010 2009 2010 2017 2008 2009 2016 2016 2009 2015 2015 2016 2015
## [63714] 2010 2008 2016 2016 2017 2010 2013 2017 2007 2013 2013 2017 2017
## [63727] 2016 2016 2016 2017 2017 2007 2008 2013 2014 2015 2017 2017 2017
## [63740] 2010 2010 2009 2015 2016 2015 2017 2015 2017 2015 2016 2010 2011
## [63753] 2016 2012 2011 2015 2017 2017 2007 2011 2009 2010 2015 2015 2017
## [63766] 2008 2016 2012 2016 2016 2015 2008 2016 2013 2016 2016 2014 2009
## [63779] 2009 2013 2013 2014 2008 2016 2017 2011 2008 2010 2017 2010 2016
## [63792] 2017 2007 2008 2010 2009 2008 2015 2011 2014 2015 2011 2011 2017
## [63805] 2012 2007 2017 2016 2017 2014 2015 2017 2007 2010 2012 2009 2015
## [63818] 2015 2016 2016 2009 2015 2013 2017 2008 2015 2016 2015 2015 2016
## [63831] 2017 2016 2011 2016 2008 2010 2015 2017 2010 2016 2015 2017 2014
## [63844] 2014 2017 2017 2017 2009 2014 2017 2017 2012 2010 2014 2017 2017
## [63857] 2010 2015 2015 2016 2016 2016 2016 2016 2017 2010 2016 2016 2009
## [63870] 2011 2009 2013 2013 2014 2015 2012 2007 2007 2015 2015 2008 2011
## [63883] 2012 2015 2017 2009 2008 2016 2014 2009 2015 2015 2016 2008 2012
## [63896] 2010 2015 2017 2016 2017 2015 2016 2017 2015 2009 2011 2015 2017
## [63909] 2010 2016 2017 2017 2008 2012 2015 2009 2011 2014 2017 2017 2014
## [63922] 2017 2011 2009 2009 2016 2017 2008 2017 2017 2010 2016 2012 2009
## [63935] 2016 2007 2016 2016 2016 2015 2016 2017 2007 2007 2017 2013 2014
## [63948] 2011 2014 2007 2011 2017 2008 2017 2015 2017 2015 2015 2008 2015
## [63961] 2017 2010 2012 2011 2016 2008 2012 2017 2014 2015 2017 2016 2008
## [63974] 2017 2017 2008 2013 2015 2017 2007 2010 2008 2017 2007 2008 2011
## [63987] 2015 2017 2012 2011 2015 2010 2012 2008 2009 2015 2017 2014 2015
## [64000] 2015 2011 2016 2009 2011 2014 2014 2015 2016 2017 2016 2016 2014
## [64013] 2017 2012 2007 2014 2017 2015 2017 2017 2017 2011 2015 2007 2009
## [64026] 2010 2014 2016 2016 2017 2013 2008 2015 2016 2017 2008 2017 2016
## [64039] 2014 2016 2010 2011 2016 2009 2017 2012 2012 2009 2014 2017 2017
## [64052] 2015 2017 2010 2008 2015 2016 2015 2017 2017 2007 2010 2008 2015
## [64065] 2016 2016 2016 2017 2009 2009 2009 2015 2015 2009 2015 2017 2014
## [64078] 2015 2016 2017 2008 2015 2016 2016 2013 2013 2016 2017 2017 2010
## [64091] 2015 2014 2017 2012 2014 2010 2016 2017 2015 2014 2014 2015 2007
## [64104] 2016 2011 2013 2014 2015 2009 2016 2016 2017 2007 2017 2008 2008
## [64117] 2009 2016 2007 2008 2009 2014 2017 2011 2011 2016 2017 2016 2007
## [64130] 2011 2014 2015 2016 2017 2017 2016 2016 2017 2007 2009 2016 2015
## [64143] 2008 2014 2016 2009 2016 2009 2011 2010 2015 2010 2008 2015 2015
## [64156] 2016 2017 2012 2007 2015 2008 2012 2015 2015 2016 2016 2015 2012
## [64169] 2009 2015 2016 2015 2016 2015 2017 2016 2015 2013 2016 2007 2007
## [64182] 2016 2016 2008 2014 2016 2008 2010 2013 2016 2017 2011 2015 2008
## [64195] 2017 2010 2012 2015 2016 2010 2013 2013 2016 2016 2016 2017 2011
## [64208] 2017 2015 2007 2015 2016 2017 2013 2017 2009 2016 2016 2015 2015
## [64221] 2017 2016 2011 2010 2016 2015 2016 2008 2014 2017 2017 2010 2009
## [64234] 2015 2015 2016 2016 2010 2014 2009 2016 2017 2009 2016 2011 2012
## [64247] 2008 2013 2013 2007 2009 2008 2015 2016 2012 2014 2017 2008 2015
## [64260] 2010 2015 2016 2011 2011 2016 2015 2010 2017 2017 2017 2009 2010
## [64273] 2014 2015 2017 2014 2017 2017 2013 2017 2007 2009 2007 2014 2014
## [64286] 2014 2015 2015 2012 2010 2014 2015 2017 2008 2017 2007 2007 2014
## [64299] 2016 2016 2017 2016 2017 2017 2017 2014 2015 2016 2008 2007 2009
## [64312] 2015 2016 2017 2009 2015 2014 2017 2008 2013 2013 2016 2016 2015
## [64325] 2017 2017 2017 2007 2015 2014 2008 2012 2016 2017 2010 2013 2016
## [64338] 2016 2011 2017 2015 2010 2017 2008 2013 2015 2008 2008 2013 2010
## [64351] 2011 2016 2017 2017 2007 2016 2010 2010 2012 2008 2017 2011 2008
## [64364] 2015 2016 2017 2008 2016 2014 2016 2017 2008 2016 2016 2017 2008
## [64377] 2012 2009 2008 2015 2016 2017 2015 2017 2009 2007 2016 2013 2013
## [64390] 2016 2015 2016 2010 2013 2007 2015 2015 2017 2017 2016 2017 2008
## [64403] 2015 2016 2017 2009 2013 2015 2016 2016 2017 2012 2015 2010 2009
## [64416] 2015 2014 2015 2016 2016 2014 2015 2015 2015 2013 2016 2014 2015
## [64429] 2016 2016 2013 2017 2017 2009 2012 2015 2016 2017 2016 2009 2016
## [64442] 2017 2009 2017 2017 2016 2011 2010 2013 2016 2011 2016 2016 2017
## [64455] 2007 2011 2009 2016 2015 2013 2017 2007 2017 2008 2008 2017 2015
## [64468] 2015 2017 2007 2011 2017 2012 2015 2016 2017 2011 2010 2008 2014
## [64481] 2017 2015 2010 2012 2009 2013 2016 2015 2015 2017 2016 2007 2013
## [64494] 2017 2011 2017 2007 2017 2014 2014 2017 2017 2016 2016 2016 2008
## [64507] 2014 2015 2017 2017 2007 2014 2016 2016 2017 2007 2012 2017 2011
## [64520] 2008 2014 2015 2012 2007 2016 2017 2017 2008 2016 2007 2017 2015
## [64533] 2016 2017 2009 2012 2016 2008 2017 2012 2008 2015 2016 2017 2010
## [64546] 2009 2010 2008 2007 2013 2015 2017 2017 2009 2011 2007 2015 2016
## [64559] 2013 2015 2017 2007 2011 2016 2010 2016 2017 2017 2007 2008 2007
## [64572] 2016 2016 2015 2017 2007 2015 2016 2015 2008 2015 2015 2017 2015
## [64585] 2009 2015 2016 2017 2014 2016 2010 2011 2016 2017 2007 2017 2017
## [64598] 2011 2012 2009 2016 2017 2010 2009 2011 2013 2015 2015 2011 2016
## [64611] 2016 2008 2009 2014 2016 2008 2011 2015 2011 2009 2008 2016 2017
## [64624] 2017 2017 2007 2014 2010 2014 2016 2017 2016 2015 2016 2017 2010
## [64637] 2015 2009 2010 2010 2015 2014 2015 2015 2010 2009 2008 2008 2016
## [64650] 2015 2010 2014 2015 2017 2017 2017 2017 2016 2016 2010 2007 2016
## [64663] 2012 2017 2009 2008 2008 2013 2017 2016 2009 2015 2017 2012 2017
## [64676] 2017 2015 2015 2016 2014 2015 2016 2017 2015 2017 2009 2014 2014
## [64689] 2015 2007 2012 2016 2017 2012 2016 2013 2014 2016 2009 2013 2016
## [64702] 2017 2015 2016 2012 2007 2016 2015 2010 2015 2016 2010 2010 2013
## [64715] 2017 2017 2009 2013 2017 2009 2011 2008 2014 2015 2009 2008 2017
## [64728] 2017 2008 2016 2007 2013 2015 2015 2014 2016 2017 2017 2007 2016
## [64741] 2015 2015 2016 2011 2008 2015 2015 2016 2017 2015 2010 2014 2009
## [64754] 2017 2017 2008 2016 2012 2010 2012 2011 2007 2016 2015 2017 2015
## [64767] 2016 2008 2016 2016 2008 2016 2017 2014 2014 2016 2007 2007 2016
## [64780] 2016 2017 2010 2007 2017 2017 2010 2013 2015 2016 2010 2007 2015
## [64793] 2016 2015 2017 2017 2012 2012 2016 2017 2013 2017 2011 2017 2010
## [64806] 2014 2011 2016 2017 2016 2015 2015 2017 2009 2016 2017 2012 2009
## [64819] 2015 2015 2015 2016 2013 2016 2011 2007 2015 2016 2017 2017 2007
## [64832] 2015 2017 2016 2012 2010 2017 2012 2017 2017 2013 2016 2013 2016
## [64845] 2017 2012 2014 2016 2013 2008 2013 2017 2017 2007 2016 2017 2015
## [64858] 2016 2010 2017 2009 2012 2009 2016 2016 2017 2009 2010 2017 2010
## [64871] 2016 2016 2010 2009 2009 2017 2012 2016 2010 2015 2016 2017 2012
## [64884] 2014 2014 2009 2014 2017 2009 2015 2017 2016 2017 2017 2015 2016
## [64897] 2017 2008 2015 2014 2017 2017 2015 2016 2016 2016 2016 2015 2007
## [64910] 2017 2017 2015 2017 2017 2015 2016 2016 2015 2015 2009 2016 2011
## [64923] 2009 2007 2014 2016 2015 2016 2015 2016 2008 2017 2017 2016 2016
## [64936] 2009 2008 2017 2009 2014 2016 2011 2007 2008 2014 2016 2017 2015
## [64949] 2016 2008 2009 2014 2009 2011 2015 2016 2017 2015 2013 2014 2015
## [64962] 2016 2017 2017 2012 2010 2015 2017 2008 2008 2011 2009 2015 2015
## [64975] 2011 2013 2013 2015 2017 2017 2008 2011 2016 2015 2007 2008 2010
## [64988] 2014 2008 2012 2014 2014 2015 2017 2010 2016 2017 2011 2016 2010
## [65001] 2011 2011 2015 2014 2015 2016 2016 2016 2009 2011 2016 2011 2017
## [65014] 2010 2010 2007 2016 2015 2007 2012 2016 2016 2016 2015 2008 2013
## [65027] 2017 2017 2015 2007 2015 2011 2012 2008 2017 2008 2011 2015 2009
## [65040] 2016 2012 2017 2017 2009 2017 2017 2010 2010 2013 2017 2015 2013
## [65053] 2016 2016 2015 2015 2016 2016 2017 2015 2013 2016 2016 2016 2017
## [65066] 2015 2015 2010 2015 2011 2010 2017 2017 2011 2011 2015 2016 2007
## [65079] 2013 2016 2015 2012 2015 2007 2016 2011 2012 2007 2016 2016 2015
## [65092] 2010 2008 2016 2009 2012 2013 2013 2013 2017 2017 2016 2016 2016
## [65105] 2016 2017 2012 2008 2017 2010 2015 2016 2016 2015 2016 2015 2010
## [65118] 2016 2017 2013 2010 2015 2017 2017 2012 2017 2012 2009 2015 2015
## [65131] 2016 2017 2017 2016 2016 2017 2012 2008 2015 2017 2010 2011 2009
## [65144] 2013 2017 2016 2015 2017 2012 2016 2016 2016 2012 2012 2015 2013
## [65157] 2011 2016 2009 2017 2017 2010 2017 2009 2017 2015 2007 2009 2009
## [65170] 2007 2015 2016 2007 2016 2017 2014 2017 2010 2014 2014 2011 2015
## [65183] 2016 2017 2017 2012 2011 2016 2016 2016 2008 2015 2016 2015 2013
## [65196] 2015 2007 2014 2016 2017 2008 2016 2015 2016 2016 2015 2015 2016
## [65209] 2017 2010 2014 2015 2015 2015 2012 2017 2017 2015 2016 2017 2014
## [65222] 2017 2010 2014 2011 2014 2017 2015 2017 2012 2012 2015 2017 2014
## [65235] 2015 2016 2016 2009 2011 2016 2017 2017 2015 2016 2009 2015 2017
## [65248] 2017 2011 2017 2011 2017 2016 2011 2007 2009 2017 2016 2016 2017
## [65261] 2008 2015 2015 2015 2016 2015 2007 2010 2008 2009 2013 2010 2013
## [65274] 2015 2016 2015 2016 2011 2012 2009 2013 2015 2015 2010 2011 2012
## [65287] 2009 2016 2017 2011 2012 2008 2016 2012 2011 2012 2009 2016 2011
## [65300] 2012 2014 2016 2015 2008 2017 2017 2014 2017 2007 2013 2017 2015
## [65313] 2017 2015 2010 2011 2015 2015 2015 2016 2017 2015 2012 2011 2012
## [65326] 2016 2017 2009 2017 2017 2016 2016 2017 2017 2012 2017 2017 2011
## [65339] 2014 2015 2015 2010 2015 2010 2015 2008 2011 2008 2016 2017 2013
## [65352] 2016 2011 2013 2016 2012 2010 2009 2012 2015 2009 2011 2015 2008
## [65365] 2010 2009 2011 2015 2017 2017 2015 2016 2010 2017 2009 2017 2017
## [65378] 2015 2009 2011 2007 2016 2011 2013 2017 2007 2014 2017 2017 2012
## [65391] 2014 2014 2012 2015 2016 2016 2015 2008 2007 2007 2007 2010 2015
## [65404] 2017 2007 2014 2015 2017 2015 2016 2016 2015 2010 2011 2016 2016
## [65417] 2015 2015 2008 2011 2013 2014 2016 2015 2017 2007 2009 2013 2015
## [65430] 2017 2011 2013 2015 2017 2008 2009 2017 2007 2011 2007 2015 2017
## [65443] 2009 2015 2015 2016 2008 2016 2015 2009 2016 2016 2017 2014 2015
## [65456] 2016 2016 2016 2017 2014 2015 2008 2009 2010 2017 2014 2012 2016
## [65469] 2015 2016 2008 2014 2017 2017 2008 2013 2016 2008 2012 2012 2012
## [65482] 2007 2016 2011 2016 2009 2012 2016 2015 2017 2009 2017 2016 2016
## [65495] 2017 2008 2008 2016 2008 2017 2016 2012 2013 2015 2009 2016 2017
## [65508] 2017 2007 2015 2015 2009 2009 2010 2008 2010 2009 2011 2009 2010
## [65521] 2013 2015 2015 2015 2011 2015 2017 2011 2008 2013 2015 2015 2016
## [65534] 2015 2013 2016 2007 2008 2014 2007 2008 2017 2009 2015 2009 2011
## [65547] 2015 2015 2010 2014 2016 2011 2008 2017 2015 2013 2017 2011 2009
## [65560] 2015 2016 2008 2016 2017 2016 2015 2017 2009 2016 2013 2017 2009
## [65573] 2014 2017 2012 2007 2013 2015 2016 2017 2015 2016 2015 2010 2011
## [65586] 2013 2017 2008 2009 2014 2007 2016 2016 2007 2013 2015 2015 2015
## [65599] 2015 2017 2012 2014 2015 2017 2015 2015 2017 2007 2014 2017 2013
## [65612] 2016 2015 2017 2009 2012 2014 2015 2016 2016 2015 2007 2015 2011
## [65625] 2014 2012 2016 2008 2014 2015 2016 2017 2012 2007 2009 2015 2008
## [65638] 2016 2015 2011 2016 2010 2016 2012 2012 2015 2016 2014 2016 2010
## [65651] 2015 2016 2011 2014 2015 2017 2016 2011 2016 2009 2015 2016 2017
## [65664] 2017 2010 2016 2017 2011 2015 2017 2017 2011 2007 2016 2012 2011
## [65677] 2013 2015 2017 2014 2016 2008 2015 2017 2007 2017 2009 2008 2017
## [65690] 2009 2007 2014 2008 2013 2008 2009 2008 2010 2011 2012 2015 2016
## [65703] 2016 2010 2015 2016 2016 2010 2011 2012 2015 2015 2017 2010 2009
## [65716] 2009 2015 2016 2008 2015 2016 2017 2007 2012 2012 2017 2012 2016
## [65729] 2011 2017 2007 2008 2015 2010 2013 2016 2013 2016 2015 2013 2017
## [65742] 2010 2011 2014 2017 2009 2016 2014 2016 2017 2017 2012 2016 2016
## [65755] 2017 2015 2017 2007 2012 2007 2015 2017 2012 2007 2007 2012 2017
## [65768] 2011 2012 2016 2015 2015 2016 2016 2017 2012 2015 2016 2016 2017
## [65781] 2017 2011 2014 2015 2016 2013 2017 2015 2013 2015 2015 2016 2017
## [65794] 2017 2014 2015 2017 2017 2008 2017 2012 2015 2016 2017 2017 2017
## [65807] 2011 2011 2014 2015 2017 2015 2012 2012 2016 2016 2016 2016 2009
## [65820] 2013 2011 2017 2015 2011 2012 2017 2011 2011 2016 2016 2016 2008
## [65833] 2013 2014 2014 2016 2009 2015 2015 2011 2016 2014 2014 2014 2016
## [65846] 2007 2016 2016 2016 2017 2017 2012 2016 2017 2017 2014 2015 2012
## [65859] 2013 2016 2010 2017 2016 2017 2009 2017 2011 2008 2017 2016 2017
## [65872] 2017 2016 2015 2015 2017 2014 2015 2016 2017 2016 2008 2013 2015
## [65885] 2016 2009 2013 2015 2017 2012 2016 2011 2010 2012 2012 2011 2014
## [65898] 2017 2017 2008 2011 2009 2010 2011 2012 2009 2014 2017 2010 2009
## [65911] 2010 2016 2016 2015 2016 2015 2016 2017 2013 2008 2017 2015 2009
## [65924] 2017 2017 2007 2015 2015 2017 2017 2013 2012 2017 2017 2017 2007
## [65937] 2016 2016 2015 2017 2010 2015 2007 2015 2017 2009 2007 2017 2013
## [65950] 2012 2009 2016 2017 2015 2017 2007 2010 2016 2017 2013 2016 2015
## [65963] 2015 2016 2012 2014 2015 2016 2015 2017 2015 2009 2009 2016 2008
## [65976] 2013 2013 2015 2009 2017 2007 2007 2016 2017 2007 2016 2008 2012
## [65989] 2015 2015 2017 2007 2007 2008 2015 2016 2017 2010 2009 2013 2013
## [66002] 2014 2015 2017 2012 2015 2016 2011 2007 2015 2017 2011 2008 2010
## [66015] 2015 2016 2011 2008 2016 2017 2017 2012 2013 2007 2007 2016 2010
## [66028] 2016 2009 2008 2008 2014 2013 2017 2017 2007 2012 2015 2012 2010
## [66041] 2014 2016 2011 2010 2016 2009 2009 2011 2016 2017 2007 2010 2017
## [66054] 2015 2015 2016 2016 2017 2009 2012 2007 2012 2009 2014 2017 2017
## [66067] 2013 2016 2015 2017 2012 2015 2017 2014 2017 2011 2015 2016 2013
## [66080] 2014 2010 2007 2015 2016 2010 2009 2016 2017 2017 2012 2011 2010
## [66093] 2017 2013 2015 2015 2016 2009 2016 2016 2012 2008 2012 2013 2015
## [66106] 2015 2017 2007 2017 2011 2015 2016 2016 2016 2017 2017 2012 2014
## [66119] 2013 2015 2017 2007 2014 2015 2015 2016 2017 2010 2012 2016 2016
## [66132] 2011 2014 2017 2016 2008 2010 2012 2016 2017 2009 2016 2015 2016
## [66145] 2017 2011 2010 2010 2016 2017 2012 2008 2016 2016 2017 2017 2008
## [66158] 2009 2013 2016 2015 2012 2011 2009 2014 2016 2011 2015 2017 2017
## [66171] 2007 2016 2016 2016 2017 2017 2017 2008 2007 2007 2011 2015 2015
## [66184] 2010 2017 2013 2015 2017 2013 2015 2007 2015 2016 2016 2015 2017
## [66197] 2015 2015 2015 2008 2017 2007 2016 2017 2015 2016 2015 2016 2015
## [66210] 2016 2008 2016 2010 2007 2016 2016 2015 2017 2011 2010 2017 2016
## [66223] 2015 2015 2017 2017 2016 2017 2011 2014 2017 2015 2017 2008 2014
## [66236] 2016 2017 2009 2007 2008 2015 2017 2010 2010 2015 2017 2017 2007
## [66249] 2014 2017 2015 2009 2014 2016 2016 2017 2015 2013 2012 2010 2016
## [66262] 2017 2011 2009 2009 2009 2015 2015 2007 2016 2017 2015 2016 2007
## [66275] 2008 2007 2016 2010 2008 2007 2007 2007 2009 2015 2017 2014 2014
## [66288] 2015 2017 2016 2016 2015 2015 2011 2011 2015 2008 2014 2017 2017
## [66301] 2015 2015 2016 2012 2012 2017 2009 2014 2014 2017 2008 2013 2015
## [66314] 2016 2015 2016 2017 2017 2017 2011 2015 2008 2017 2008 2016 2012
## [66327] 2013 2011 2009 2016 2012 2016 2008 2012 2010 2016 2017 2009 2016
## [66340] 2016 2016 2016 2017 2017 2010 2009 2016 2015 2015 2016 2017 2008
## [66353] 2017 2017 2011 2009 2013 2013 2016 2008 2015 2014 2016 2017 2008
## [66366] 2014 2015 2015 2016 2010 2010 2008 2015 2017 2012 2007 2012 2015
## [66379] 2015 2016 2011 2010 2016 2016 2016 2017 2011 2017 2010 2014 2016
## [66392] 2017 2010 2016 2016 2016 2009 2011 2011 2007 2008 2011 2008 2012
## [66405] 2015 2009 2017 2015 2016 2017 2011 2015 2017 2011 2016 2017 2017
## [66418] 2016 2017 2015 2007 2007 2008 2015 2017 2010 2012 2012 2015 2015
## [66431] 2017 2015 2017 2017 2017 2009 2017 2017 2008 2010 2014 2016 2012
## [66444] 2009 2014 2017 2007 2016 2015 2016 2017 2008 2016 2016 2007 2007
## [66457] 2016 2015 2015 2015 2016 2014 2013 2014 2015 2015 2007 2016 2016
## [66470] 2016 2016 2016 2017 2017 2008 2011 2007 2015 2017 2016 2012 2015
## [66483] 2017 2016 2010 2007 2012 2017 2012 2016 2015 2007 2015 2016 2015
## [66496] 2016 2012 2015 2007 2010 2013 2007 2017 2017 2017 2017 2015 2015
## [66509] 2007 2012 2009 2017 2010 2014 2015 2016 2017 2011 2016 2016 2007
## [66522] 2009 2013 2016 2011 2016 2009 2015 2016 2017 2017 2012 2008 2008
## [66535] 2013 2013 2013 2016 2016 2016 2011 2011 2007 2007 2012 2016 2017
## [66548] 2015 2017 2010 2010 2013 2015 2015 2009 2017 2016 2015 2014 2014
## [66561] 2015 2015 2017 2017 2008 2009 2016 2010 2009 2013 2015 2008 2010
## [66574] 2012 2015 2017 2012 2015 2016 2008 2008 2015 2017 2017 2011 2011
## [66587] 2016 2017 2017 2011 2013 2016 2014 2015 2016 2016 2017 2007 2015
## [66600] 2015 2008 2015 2015 2015 2016 2011 2009 2015 2016 2015 2015 2015
## [66613] 2017 2009 2015 2015 2016 2016 2017 2007 2015 2015 2017 2017 2017
## [66626] 2015 2014 2016 2017 2009 2016 2015 2010 2007 2015 2008 2007 2015
## [66639] 2012 2015 2016 2016 2017 2010 2007 2015 2011 2008 2007 2016 2016
## [66652] 2017 2007 2016 2015 2015 2015 2013 2016 2017 2012 2015 2016 2010
## [66665] 2016 2016 2014 2016 2011 2010 2015 2007 2007 2017 2016 2011 2016
## [66678] 2017 2015 2016 2009 2015 2009 2012 2012 2016 2017 2012 2012 2007
## [66691] 2016 2016 2017 2017 2011 2012 2015 2012 2017 2012 2012 2011 2015
## [66704] 2016 2009 2012 2017 2007 2009 2011 2015 2017 2016 2016 2010 2017
## [66717] 2017 2009 2012 2007 2016 2017 2011 2011 2013 2016 2016 2017 2015
## [66730] 2011 2009 2013 2015 2016 2017 2012 2014 2012 2007 2011 2015 2016
## [66743] 2016 2008 2011 2011 2016 2017 2008 2016 2015 2009 2015 2015 2015
## [66756] 2017 2017 2015 2013 2015 2015 2016 2007 2009 2013 2014 2015 2015
## [66769] 2016 2015 2016 2016 2015 2008 2008 2015 2013 2015 2016 2016 2016
## [66782] 2011 2016 2017 2016 2016 2009 2008 2016 2017 2016 2017 2017 2009
## [66795] 2014 2017 2008 2017 2015 2015 2017 2017 2011 2013 2016 2017 2015
## [66808] 2017 2017 2011 2015 2017 2017 2015 2017 2011 2017 2008 2008 2010
## [66821] 2012 2016 2015 2017 2008 2016 2015 2015 2011 2017 2017 2017 2017
## [66834] 2010 2010 2017 2015 2016 2016 2016 2012 2016 2011 2007 2007 2016
## [66847] 2016 2010 2012 2016 2011 2015 2017 2017 2007 2014 2015 2016 2016
## [66860] 2009 2015 2015 2015 2016 2014 2016 2009 2007 2017 2017 2017 2011
## [66873] 2017 2017 2009 2014 2015 2016 2014 2017 2015 2008 2011 2007 2016
## [66886] 2015 2011 2015 2011 2015 2011 2016 2017 2009 2011 2015 2015 2015
## [66899] 2017 2010 2008 2015 2016 2010 2010 2012 2014 2016 2007 2012 2013
## [66912] 2015 2017 2011 2016 2017 2015 2012 2014 2017 2009 2013 2016 2007
## [66925] 2017 2011 2015 2017 2010 2015 2015 2017 2017 2015 2015 2016 2017
## [66938] 2011 2014 2017 2015 2016 2016 2016 2008 2011 2016 2016 2015 2017
## [66951] 2011 2014 2015 2015 2016 2014 2015 2016 2016 2017 2009 2012 2009
## [66964] 2015 2015 2016 2016 2017 2014 2016 2008 2016 2017 2009 2016 2015
## [66977] 2011 2009 2009 2010 2012 2013 2016 2009 2016 2012 2015 2016 2017
## [66990] 2015 2016 2016 2015 2017 2017 2017 2007 2014 2015 2016 2015 2016
## [67003] 2016 2016 2017 2017 2013 2014 2016 2017 2007 2015 2017 2007 2010
## [67016] 2015 2011 2016 2017 2016 2011 2016 2017 2016 2008 2017 2011 2008
## [67029] 2007 2015 2009 2008 2015 2016 2012 2008 2017 2014 2016 2016 2016
## [67042] 2014 2016 2017 2008 2016 2016 2017 2008 2016 2017 2011 2015 2015
## [67055] 2007 2008 2009 2017 2009 2007 2016 2016 2017 2017 2017 2017 2015
## [67068] 2015 2017 2009 2016 2017 2012 2008 2015 2016 2017 2011 2009 2017
## [67081] 2010 2016 2012 2016 2016 2017 2012 2016 2016 2009 2014 2016 2012
## [67094] 2015 2008 2011 2011 2014 2015 2015 2015 2017 2016 2009 2015 2017
## [67107] 2017 2010 2007 2017 2010 2012 2007 2010 2016 2016 2017 2017 2017
## [67120] 2009 2013 2014 2007 2014 2016 2010 2008 2015 2015 2016 2016 2017
## [67133] 2017 2008 2015 2015 2016 2016 2012 2014 2007 2012 2015 2012 2012
## [67146] 2010 2007 2016 2015 2017 2008 2011 2014 2016 2007 2012 2016 2016
## [67159] 2015 2012 2008 2016 2016 2009 2016 2011 2015 2017 2017 2008 2016
## [67172] 2016 2007 2016 2016 2016 2015 2011 2014 2016 2017 2011 2013 2010
## [67185] 2015 2016 2009 2014 2015 2016 2016 2011 2007 2015 2016 2017 2017
## [67198] 2015 2016 2011 2007 2013 2009 2011 2015 2017 2017 2010 2012 2016
## [67211] 2016 2015 2017 2009 2008 2012 2015 2010 2015 2016 2015 2016 2008
## [67224] 2013 2013 2015 2011 2011 2017 2007 2008 2014 2015 2008 2012 2009
## [67237] 2014 2014 2016 2016 2017 2007 2009 2009 2014 2015 2017 2015 2015
## [67250] 2009 2015 2015 2015 2009 2010 2009 2009 2008 2013 2015 2017 2013
## [67263] 2017 2017 2017 2010 2010 2014 2016 2009 2012 2009 2017 2017 2009
## [67276] 2009 2011 2011 2015 2016 2015 2010 2015 2015 2017 2013 2016 2015
## [67289] 2008 2009 2007 2009 2014 2015 2009 2012 2015 2015 2017 2007 2010
## [67302] 2015 2016 2015 2016 2011 2008 2011 2013 2010 2013 2016 2017 2015
## [67315] 2007 2015 2007 2016 2016 2015 2008 2010 2013 2014 2016 2016 2010
## [67328] 2009 2008 2016 2016 2008 2017 2007 2016 2015 2009 2016 2015 2016
## [67341] 2017 2017 2012 2014 2016 2012 2016 2017 2016 2015 2017 2008 2009
## [67354] 2012 2015 2008 2010 2017 2011 2011 2011 2014 2015 2016 2016 2017
## [67367] 2007 2013 2016 2017 2008 2009 2015 2016 2015 2016 2017 2011 2014
## [67380] 2011 2014 2009 2009 2013 2015 2016 2007 2015 2016 2017 2007 2008
## [67393] 2011 2016 2009 2014 2008 2016 2010 2009 2013 2017 2013 2015 2011
## [67406] 2014 2017 2011 2015 2016 2007 2016 2017 2010 2016 2017 2008 2009
## [67419] 2012 2009 2016 2016 2017 2010 2014 2010 2014 2015 2008 2012 2012
## [67432] 2010 2017 2009 2013 2012 2007 2017 2008 2011 2007 2015 2017 2017
## [67445] 2017 2011 2010 2013 2015 2010 2014 2016 2017 2016 2007 2017 2017
## [67458] 2014 2015 2017 2017 2014 2016 2016 2014 2017 2016 2017 2017 2013
## [67471] 2011 2015 2016 2007 2009 2010 2015 2016 2017 2017 2008 2017 2016
## [67484] 2017 2011 2007 2012 2013 2014 2016 2017 2016 2015 2012 2008 2013
## [67497] 2015 2016 2009 2009 2015 2017 2011 2013 2015 2016 2015 2016 2016
## [67510] 2016 2015 2007 2014 2015 2017 2008 2014 2009 2011 2016 2017 2017
## [67523] 2016 2015 2017 2008 2009 2015 2016 2017 2015 2013 2014 2017 2012
## [67536] 2017 2012 2010 2009 2008 2009 2013 2014 2015 2008 2014 2014 2011
## [67549] 2015 2017 2016 2015 2017 2008 2011 2008 2015 2016 2007 2011 2015
## [67562] 2010 2009 2013 2015 2008 2008 2016 2016 2009 2015 2016 2016 2009
## [67575] 2010 2013 2016 2017 2007 2013 2017 2008 2008 2012 2014 2015 2015
## [67588] 2015 2016 2017 2017 2017 2014 2015 2010 2013 2015 2015 2017 2009
## [67601] 2008 2015 2016 2017 2015 2017 2009 2015 2015 2015 2016 2017 2009
## [67614] 2008 2014 2014 2015 2017 2017 2013 2015 2016 2015 2015 2017 2009
## [67627] 2007 2010 2014 2012 2016 2010 2017 2014 2016 2017 2015 2009 2010
## [67640] 2012 2010 2012 2014 2015 2017 2012 2010 2017 2011 2016 2017 2014
## [67653] 2015 2012 2012 2016 2017 2007 2010 2012 2015 2015 2017 2015 2010
## [67666] 2015 2015 2016 2017 2007 2009 2017 2017 2007 2011 2016 2008 2017
## [67679] 2009 2014 2015 2016 2017 2007 2015 2017 2009 2016 2017 2017 2010
## [67692] 2008 2008 2015 2015 2017 2011 2015 2016 2015 2017 2017 2016 2016
## [67705] 2008 2010 2013 2016 2017 2009 2014 2016 2007 2015 2015 2016 2012
## [67718] 2015 2016 2015 2017 2008 2017 2010 2008 2016 2017 2016 2007 2015
## [67731] 2017 2010 2008 2007 2016 2017 2017 2012 2007 2016 2015 2008 2016
## [67744] 2014 2017 2011 2013 2016 2017 2017 2011 2015 2017 2007 2010 2013
## [67757] 2016 2017 2017 2008 2013 2015 2016 2009 2010 2011 2015 2016 2009
## [67770] 2014 2014 2016 2017 2012 2009 2008 2007 2007 2016 2008 2013 2016
## [67783] 2016 2015 2015 2016 2015 2015 2015 2015 2014 2015 2016 2016 2017
## [67796] 2009 2013 2016 2017 2015 2016 2012 2017 2017 2010 2014 2015 2013
## [67809] 2015 2016 2015 2012 2012 2015 2016 2016 2011 2008 2011 2011 2017
## [67822] 2015 2007 2008 2013 2007 2013 2014 2007 2015 2016 2017 2015 2016
## [67835] 2016 2017 2017 2009 2017 2015 2016 2016 2008 2011 2016 2017 2011
## [67848] 2015 2009 2008 2008 2017 2017 2008 2009 2013 2014 2015 2008 2011
## [67861] 2013 2014 2017 2013 2014 2016 2016 2017 2007 2009 2010 2007 2013
## [67874] 2015 2015 2016 2017 2008 2010 2016 2016 2015 2009 2008 2013 2015
## [67887] 2017 2007 2013 2017 2008 2009 2015 2016 2009 2015 2016 2012 2017
## [67900] 2009 2015 2016 2009 2011 2014 2015 2015 2015 2016 2015 2012 2007
## [67913] 2014 2016 2017 2009 2013 2013 2010 2007 2014 2015 2017 2012 2015
## [67926] 2017 2007 2010 2015 2015 2016 2017 2007 2016 2009 2010 2016 2016
## [67939] 2008 2013 2016 2009 2015 2017 2016 2014 2015 2007 2009 2017 2012
## [67952] 2012 2009 2014 2016 2016 2009 2015 2016 2017 2012 2017 2017 2016
## [67965] 2009 2009 2016 2016 2016 2009 2012 2011 2017 2017 2017 2011 2016
## [67978] 2016 2015 2015 2011 2009 2017 2017 2008 2015 2015 2016 2015 2016
## [67991] 2009 2011 2017 2017 2012 2016 2016 2015 2011 2009 2011 2015 2015
## [68004] 2016 2016 2010 2014 2017 2016 2010 2014 2009 2009 2015 2017 2008
## [68017] 2017 2015 2016 2017 2014 2016 2011 2016 2012 2007 2016 2015 2015
## [68030] 2016 2016 2015 2008 2012 2014 2017 2008 2015 2017 2015 2017 2010
## [68043] 2016 2016 2017 2015 2008 2014 2017 2017 2016 2011 2017 2013 2013
## [68056] 2015 2016 2007 2012 2013 2017 2014 2017 2007 2013 2011 2009 2013
## [68069] 2015 2016 2011 2015 2015 2007 2011 2016 2009 2014 2017 2010 2009
## [68082] 2015 2014 2015 2016 2007 2016 2017 2008 2007 2015 2016 2017 2017
## [68095] 2016 2012 2007 2013 2015 2011 2016 2015 2009 2015 2009 2016 2016
## [68108] 2011 2012 2009 2013 2016 2016 2010 2015 2012 2017 2017 2010 2013
## [68121] 2011 2016 2017 2009 2012 2012 2015 2016 2015 2012 2015 2016 2017
## [68134] 2017 2009 2012 2017 2009 2015 2015 2013 2017 2017 2017 2009 2016
## [68147] 2017 2007 2007 2015 2015 2010 2013 2010 2017 2009 2012 2009 2016
## [68160] 2015 2012 2014 2016 2011 2010 2010 2015 2016 2008 2015 2017 2009
## [68173] 2012 2008 2010 2011 2017 2016 2011 2017 2009 2015 2007 2010 2015
## [68186] 2012 2007 2016 2016 2017 2009 2014 2016 2017 2016 2016 2017 2010
## [68199] 2009 2014 2017 2017 2017 2007 2010 2017 2010 2013 2015 2017 2017
## [68212] 2011 2015 2016 2009 2017 2017 2015 2015 2012 2014 2015 2017 2016
## [68225] 2017 2010 2008 2013 2016 2015 2017 2017 2017 2011 2014 2015 2013
## [68238] 2011 2008 2015 2012 2010 2015 2016 2017 2017 2014 2017 2017 2008
## [68251] 2015 2016 2016 2017 2010 2016 2017 2012 2015 2012 2013 2016 2007
## [68264] 2015 2015 2012 2017 2012 2008 2012 2015 2011 2010 2017 2017 2009
## [68277] 2008 2016 2010 2010 2012 2013 2016 2014 2016 2017 2017 2015 2013
## [68290] 2016 2014 2015 2016 2007 2013 2016 2009 2015 2014 2015 2016 2010
## [68303] 2015 2016 2007 2011 2010 2013 2016 2016 2017 2011 2015 2014 2015
## [68316] 2011 2015 2009 2009 2017 2007 2016 2016 2017 2015 2017 2017 2017
## [68329] 2008 2010 2016 2015 2008 2013 2015 2015 2017 2008 2016 2015 2007
## [68342] 2010 2017 2017 2008 2009 2016 2015 2011 2013 2016 2016 2016 2017
## [68355] 2016 2017 2016 2017 2017 2017 2008 2013 2015 2015 2015 2007 2015
## [68368] 2016 2009 2016 2010 2011 2007 2012 2014 2016 2016 2017 2007 2014
## [68381] 2017 2017 2008 2007 2015 2016 2013 2013 2014 2016 2017 2009 2016
## [68394] 2007 2011 2015 2012 2015 2015 2016 2016 2014 2015 2017 2015 2015
## [68407] 2016 2008 2009 2015 2016 2016 2016 2016 2017 2012 2012 2016 2016
## [68420] 2015 2016 2016 2010 2014 2014 2016 2016 2017 2017 2017 2012 2017
## [68433] 2008 2010 2009 2017 2017 2009 2012 2007 2017 2017 2010 2008 2015
## [68446] 2016 2016 2017 2016 2013 2015 2016 2012 2010 2017 2010 2017 2013
## [68459] 2007 2012 2015 2016 2016 2016 2015 2017 2017 2009 2014 2016 2008
## [68472] 2015 2015 2015 2010 2009 2017 2017 2017 2011 2014 2017 2010 2017
## [68485] 2017 2009 2008 2015 2015 2017 2014 2015 2014 2015 2011 2007 2014
## [68498] 2015 2016 2009 2007 2009 2007 2009 2015 2015 2015 2017 2015 2016
## [68511] 2017 2008 2016 2015 2016 2017 2017 2015 2010 2007 2014 2015 2016
## [68524] 2013 2016 2017 2011 2013 2014 2017 2017 2008 2014 2015 2013 2015
## [68537] 2016 2017 2009 2007 2016 2016 2016 2009 2014 2017 2011 2007 2015
## [68550] 2016 2016 2014 2017 2017 2007 2009 2014 2016 2015 2017 2017 2009
## [68563] 2017 2011 2010 2015 2017 2007 2011 2007 2016 2017 2014 2015 2017
## [68576] 2017 2015 2016 2017 2007 2015 2015 2015 2017 2010 2010 2012 2016
## [68589] 2016 2017 2011 2015 2016 2013 2013 2014 2014 2014 2016 2016 2014
## [68602] 2017 2014 2016 2016 2015 2008 2011 2014 2015 2015 2012 2009 2016
## [68615] 2009 2011 2016 2017 2008 2014 2010 2008 2010 2014 2017 2016 2017
## [68628] 2017 2009 2009 2016 2007 2017 2017 2015 2017 2017 2014 2015 2015
## [68641] 2015 2011 2015 2016 2016 2016 2016 2017 2008 2017 2008 2013 2016
## [68654] 2010 2012 2016 2017 2016 2009 2008 2015 2016 2017 2012 2013 2016
## [68667] 2016 2017 2008 2008 2014 2014 2015 2015 2016 2011 2009 2013 2014
## [68680] 2015 2008 2007 2009 2016 2016 2017 2007 2008 2015 2016 2010 2015
## [68693] 2012 2009 2012 2015 2007 2012 2011 2017 2017 2012 2015 2017 2007
## [68706] 2007 2017 2015 2017 2008 2014 2015 2017 2011 2007 2014 2017 2017
## [68719] 2015 2016 2009 2007 2007 2015 2013 2015 2015 2015 2016 2016 2017
## [68732] 2011 2016 2009 2016 2013 2017 2017 2012 2009 2008 2013 2017 2007
## [68745] 2013 2016 2014 2007 2015 2015 2010 2009 2014 2016 2017 2016 2009
## [68758] 2014 2016 2017 2011 2016 2013 2012 2015 2011 2007 2014 2017 2007
## [68771] 2015 2016 2017 2017 2011 2016 2016 2009 2008 2007 2008 2014 2015
## [68784] 2015 2013 2016 2008 2015 2011 2015 2017 2007 2015 2008 2010 2015
## [68797] 2016 2017 2017 2014 2017 2015 2016 2016 2010 2009 2013 2015 2008
## [68810] 2017 2007 2008 2015 2017 2016 2013 2011 2012 2013 2013 2016 2009
## [68823] 2014 2016 2011 2009 2007 2015 2013 2014 2016 2009 2010 2016 2016
## [68836] 2017 2014 2016 2017 2017 2013 2015 2016 2007 2015 2017 2017 2017
## [68849] 2015 2017 2009 2015 2010 2015 2013 2016 2008 2015 2015 2016 2016
## [68862] 2017 2017 2016 2016 2008 2011 2017 2008 2014 2016 2016 2015 2015
## [68875] 2017 2012 2015 2016 2016 2012 2016 2015 2008 2013 2015 2017 2008
## [68888] 2010 2017 2017 2017 2007 2009 2009 2017 2007 2015 2015 2016 2016
## [68901] 2016 2016 2011 2016 2009 2013 2015 2015 2017 2015 2008 2007 2015
## [68914] 2015 2015 2015 2017 2007 2007 2010 2015 2015 2015 2015 2012 2008
## [68927] 2008 2015 2015 2016 2009 2011 2015 2016 2016 2016 2013 2013 2015
## [68940] 2015 2008 2009 2017 2017 2008 2007 2016 2011 2013 2013 2016 2017
## [68953] 2010 2016 2007 2009 2015 2016 2011 2014 2017 2015 2008 2015 2010
## [68966] 2016 2016 2016 2017 2012 2017 2012 2016 2014 2015 2008 2017 2015
## [68979] 2015 2007 2017 2008 2013 2009 2011 2014 2011 2012 2013 2015 2017
## [68992] 2009 2014 2016 2016 2011 2016 2010 2008 2015 2009 2013 2010 2015
## [69005] 2016 2010 2015 2017 2016 2016 2017 2009 2008 2009 2014 2015 2010
## [69018] 2015 2007 2014 2015 2016 2013 2016 2015 2010 2010 2016 2016 2015
## [69031] 2017 2014 2015 2017 2016 2009 2011 2010 2009 2015 2014 2017 2010
## [69044] 2009 2015 2008 2011 2011 2009 2017 2008 2009 2015 2016 2017 2012
## [69057] 2014 2015 2015 2017 2007 2008 2010 2016 2012 2007 2015 2015 2009
## [69070] 2016 2017 2009 2017 2010 2016 2016 2009 2013 2012 2012 2016 2016
## [69083] 2015 2017 2008 2010 2015 2017 2016 2012 2012 2015 2016 2015 2017
## [69096] 2012 2015 2015 2017 2015 2015 2011 2017 2016 2009 2008 2016 2010
## [69109] 2010 2009 2009 2017 2017 2012 2008 2007 2016 2017 2015 2008 2015
## [69122] 2013 2013 2016 2015 2015 2016 2017 2011 2016 2017 2017 2017 2008
## [69135] 2008 2017 2012 2010 2015 2016 2015 2016 2010 2017 2016 2017 2007
## [69148] 2007 2015 2016 2016 2015 2007 2012 2016 2016 2012 2012 2015 2016
## [69161] 2016 2015 2017 2010 2015 2017 2017 2017 2014 2015 2015 2007 2015
## [69174] 2015 2016 2015 2015 2015 2016 2015 2010 2012 2016 2014 2010 2015
## [69187] 2009 2012 2015 2017 2011 2007 2007 2015 2016 2007 2015 2012 2009
## [69200] 2008 2013 2008 2013 2016 2016 2016 2015 2015 2017 2011 2015 2015
## [69213] 2009 2008 2007 2009 2009 2015 2017 2017 2009 2007 2015 2015 2015
## [69226] 2009 2013 2015 2011 2008 2009 2016 2012 2015 2017 2017 2008 2010
## [69239] 2008 2011 2016 2017 2017 2012 2007 2017 2013 2016 2017 2017 2017
## [69252] 2015 2011 2016 2015 2016 2017 2017 2012 2017 2007 2015 2016 2017
## [69265] 2013 2013 2016 2009 2012 2010 2015 2014 2014 2011 2011 2014 2016
## [69278] 2016 2015 2017 2009 2013 2007 2007 2009 2008 2009 2016 2016 2015
## [69291] 2009 2008 2017 2017 2016 2017 2013 2010 2010 2017 2017 2008 2017
## [69304] 2009 2014 2015 2016 2017 2015 2016 2016 2017 2012 2012 2017 2015
## [69317] 2015 2015 2012 2011 2015 2011 2016 2017 2007 2017 2017 2015 2010
## [69330] 2012 2014 2016 2007 2017 2017 2008 2015 2015 2016 2017 2011 2016
## [69343] 2015 2015 2011 2012 2012 2015 2017 2017 2009 2014 2016 2015 2015
## [69356] 2011 2017 2008 2016 2016 2009 2011 2017 2016 2016 2016 2017 2017
## [69369] 2007 2016 2016 2016 2016 2016 2015 2017 2017 2012 2016 2014 2017
## [69382] 2011 2015 2017 2007 2016 2016 2013 2016 2016 2017 2008 2017 2016
## [69395] 2009 2016 2010 2016 2017 2017 2012 2007 2017 2015 2011 2015 2017
## [69408] 2009 2007 2010 2007 2009 2016 2017 2008 2013 2016 2011 2007 2007
## [69421] 2015 2009 2013 2015 2017 2017 2008 2008 2008 2017 2012 2016 2016
## [69434] 2010 2012 2010 2017 2017 2007 2010 2012 2013 2017 2010 2016 2016
## [69447] 2017 2015 2016 2017 2017 2008 2010 2012 2015 2011 2007 2010 2011
## [69460] 2007 2010 2011 2011 2007 2010 2007 2016 2007 2013 2016 2017 2017
## [69473] 2009 2011 2016 2016 2017 2007 2010 2015 2015 2017 2017 2015 2016
## [69486] 2017 2007 2015 2017 2016 2017 2017 2012 2010 2014 2016 2017 2011
## [69499] 2014 2014 2015 2007 2015 2016 2008 2015 2010 2016 2017 2017 2017
## [69512] 2016 2013 2013 2017 2008 2010 2007 2007 2011 2012 2009 2016 2016
## [69525] 2016 2016 2015 2015 2008 2010 2015 2013 2015 2016 2015 2011 2016
## [69538] 2009 2007 2010 2016 2007 2010 2015 2015 2009 2017 2011 2016 2015
## [69551] 2016 2017 2017 2009 2009 2010 2013 2015 2011 2010 2010 2009 2016
## [69564] 2017 2015 2015 2016 2015 2015 2012 2015 2017 2017 2015 2016 2017
## [69577] 2015 2016 2012 2010 2009 2008 2014 2016 2017 2016 2010 2010 2015
## [69590] 2015 2015 2016 2017 2011 2009 2014 2015 2016 2009 2015 2017 2008
## [69603] 2013 2013 2015 2011 2016 2017 2017 2014 2016 2016 2008 2007 2017
## [69616] 2017 2008 2015 2017 2017 2009 2008 2017 2011 2014 2017 2017 2010
## [69629] 2014 2012 2016 2014 2016 2015 2016 2016 2016 2016 2011 2011 2015
## [69642] 2017 2017 2017 2016 2015 2008 2016 2015 2008 2014 2017 2010 2010
## [69655] 2014 2010 2009 2016 2017 2015 2009 2016 2010 2013 2015 2017 2008
## [69668] 2017 2017 2012 2015 2017 2010 2011 2013 2015 2008 2015 2008 2015
## [69681] 2008 2016 2013 2015 2015 2008 2009 2015 2016 2007 2016 2017 2015
## [69694] 2017 2014 2015 2008 2007 2011 2013 2007 2011 2007 2012 2016 2017
## [69707] 2017 2008 2007 2008 2016 2016 2010 2016 2008 2012 2010 2015 2017
## [69720] 2007 2007 2008 2014 2017 2008 2012 2015 2008 2012 2014 2016 2017
## [69733] 2007 2007 2011 2012 2014 2015 2012 2017 2015 2016 2015 2009 2011
## [69746] 2008 2011 2015 2016 2016 2014 2016 2015 2008 2013 2016 2017 2010
## [69759] 2016 2017 2017 2008 2011 2017 2008 2010 2007 2015 2016 2015 2016
## [69772] 2016 2017 2017 2013 2015 2016 2015 2008 2008 2013 2016 2015 2008
## [69785] 2015 2016 2008 2016 2017 2010 2015 2015 2016 2017 2011 2012 2008
## [69798] 2010 2007 2010 2016 2016 2010 2008 2017 2010 2009 2017 2017 2010
## [69811] 2016 2012 2011 2009 2016 2016 2017 2009 2015 2017 2008 2008 2016
## [69824] 2014 2016 2017 2017 2012 2008 2017 2016 2008 2011 2016 2016 2017
## [69837] 2015 2015 2015 2017 2012 2017 2017 2008 2008 2012 2016 2017 2010
## [69850] 2012 2017 2016 2015 2009 2015 2009 2012 2015 2008 2017 2017 2009
## [69863] 2012 2014 2015 2015 2016 2016 2017 2017 2015 2009 2008 2017 2012
## [69876] 2010 2007 2015 2011 2008 2015 2015 2014 2015 2017 2011 2015 2016
## [69889] 2017 2015 2017 2009 2011 2016 2009 2007 2016 2009 2010 2016 2017
## [69902] 2007 2008 2016 2017 2013 2010 2008 2015 2016 2017 2007 2007 2017
## [69915] 2011 2008 2011 2014 2017 2017 2017 2017 2010 2010 2014 2017 2007
## [69928] 2013 2017 2011 2009 2017 2017 2011 2008 2017 2008 2014 2015 2015
## [69941] 2012 2017 2012 2016 2009 2017 2017 2007 2010 2013 2015 2016 2017
## [69954] 2016 2008 2014 2016 2010 2012 2008 2009 2016 2012 2015 2014 2015
## [69967] 2016 2017 2015 2011 2007 2008 2008 2014 2016 2010 2017 2013 2014
## [69980] 2017 2011 2015 2015 2016 2011 2013 2016 2011 2012 2010 2010 2017
## [69993] 2015 2013 2015 2015 2015 2010 2015 2016 2017 2011 2017 2007 2007
## [70006] 2017 2011 2017 2017 2010 2013 2015 2017 2017 2007 2015 2016 2016
## [70019] 2015 2016 2009 2013 2016 2017 2009 2016 2007 2012 2008 2014 2016
## [70032] 2017 2017 2015 2016 2017 2012 2013 2015 2016 2016 2014 2017 2017
## [70045] 2007 2016 2016 2015 2017 2007 2009 2013 2016 2017 2016 2016 2009
## [70058] 2010 2009 2015 2016 2012 2017 2013 2013 2017 2012 2014 2015 2015
## [70071] 2016 2012 2015 2016 2017 2015 2015 2017 2011 2015 2015 2007 2009
## [70084] 2013 2016 2016 2017 2013 2014 2015 2015 2007 2011 2012 2015 2015
## [70097] 2017 2015 2017 2011 2010 2012 2015 2015 2016 2010 2016 2017 2011
## [70110] 2016 2016 2017 2016 2017 2016 2009 2011 2016 2014 2009 2007 2009
## [70123] 2016 2016 2007 2012 2016 2010 2016 2016 2007 2015 2015 2016 2017
## [70136] 2014 2016 2017 2015 2013 2011 2013 2015 2016 2016 2008 2008 2017
## [70149] 2011 2012 2017 2012 2007 2009 2014 2015 2012 2013 2013 2016 2011
## [70162] 2009 2015 2015 2017 2007 2008 2009 2014 2008 2015 2008 2009 2016
## [70175] 2016 2017 2013 2015 2016 2009 2009 2009 2016 2012 2015 2017 2017
## [70188] 2010 2013 2014 2015 2017 2015 2016 2016 2007 2015 2017 2008 2015
## [70201] 2015 2008 2009 2010 2013 2016 2007 2014 2008 2016 2015 2011 2011
## [70214] 2016 2017 2008 2011 2015 2008 2008 2015 2016 2011 2016 2017 2012
## [70227] 2008 2015 2016 2015 2017 2010 2014 2015 2015 2009 2016 2016 2017
## [70240] 2010 2011 2016 2017 2017 2007 2014 2014 2015 2016 2016 2010 2010
## [70253] 2009 2015 2008 2010 2015 2011 2007 2017 2017 2007 2011 2012 2015
## [70266] 2007 2015 2016 2016 2017 2011 2012 2015 2015 2015 2017 2007 2016
## [70279] 2015 2015 2016 2016 2016 2014 2010 2010 2008 2017 2014 2016 2011
## [70292] 2012 2009 2017 2017 2011 2015 2017 2012 2015 2015 2014 2015 2016
## [70305] 2007 2016 2011 2007 2007 2016 2017 2015 2011 2008 2015 2016 2017
## [70318] 2016 2016 2017 2007 2016 2009 2014 2015 2014 2010 2009 2016 2007
## [70331] 2016 2016 2015 2012 2008 2017 2009 2011 2010 2014 2017 2007 2016
## [70344] 2016 2013 2017 2017 2015 2016 2017 2014 2016 2016 2011 2008 2013
## [70357] 2014 2009 2013 2013 2016 2016 2017 2015 2010 2011 2017 2009 2015
## [70370] 2017 2017 2010 2017 2017 2010 2017 2017 2017 2016 2017 2015 2017
## [70383] 2016 2015 2016 2010 2016 2017 2012 2016 2016 2009 2015 2017 2011
## [70396] 2009 2016 2015 2016 2017 2017 2011 2015 2008 2007 2015 2017 2011
## [70409] 2016 2016 2016 2017 2017 2008 2016 2016 2017 2007 2012 2008 2015
## [70422] 2017 2008 2017 2013 2012 2015 2007 2015 2015 2012 2012 2011 2015
## [70435] 2015 2016 2009 2015 2016 2016 2007 2007 2016 2007 2014 2015 2017
## [70448] 2011 2013 2016 2015 2016 2016 2016 2017 2016 2016 2008 2015 2015
## [70461] 2016 2017 2011 2007 2016 2011 2015 2016 2015 2012 2008 2016 2016
## [70474] 2015 2017 2015 2007 2007 2013 2016 2015 2016 2012 2016 2011 2011
## [70487] 2015 2015 2008 2013 2017 2017 2011 2013 2015 2011 2016 2015 2011
## [70500] 2008 2015 2017 2011 2012 2014 2016 2015 2016 2015 2012 2009 2013
## [70513] 2015 2008 2016 2012 2017 2017 2011 2015 2016 2015 2016 2016 2015
## [70526] 2008 2011 2007 2016 2017 2017 2014 2015 2017 2008 2017 2015 2016
## [70539] 2013 2016 2015 2011 2015 2017 2008 2011 2009 2017 2011 2008 2015
## [70552] 2017 2009 2014 2017 2009 2013 2008 2016 2017 2017 2017 2015 2008
## [70565] 2013 2011 2015 2014 2015 2015 2017 2015 2010 2017 2017 2012 2009
## [70578] 2017 2016 2008 2015 2017 2009 2012 2012 2013 2016 2012 2016 2015
## [70591] 2016 2013 2017 2017 2015 2017 2016 2011 2015 2015 2015 2007 2013
## [70604] 2015 2016 2016 2010 2011 2012 2017 2010 2013 2017 2013 2015 2016
## [70617] 2017 2017 2010 2007 2015 2016 2015 2009 2016 2016 2017 2016 2016
## [70630] 2016 2014 2016 2017 2017 2016 2016 2017 2012 2015 2016 2012 2008
## [70643] 2011 2016 2014 2013 2015 2015 2016 2015 2011 2012 2016 2016 2015
## [70656] 2017 2012 2015 2016 2008 2015 2015 2015 2016 2017 2015 2013 2016
## [70669] 2017 2008 2007 2013 2016 2016 2017 2009 2016 2016 2017 2015 2017
## [70682] 2012 2016 2015 2017 2014 2009 2012 2016 2017 2017 2009 2013 2015
## [70695] 2015 2015 2015 2017 2017 2015 2016 2012 2008 2017 2016 2016 2017
## [70708] 2017 2011 2012 2017 2015 2015 2017 2012 2013 2012 2014 2016 2008
## [70721] 2007 2016 2014 2015 2017 2007 2016 2017 2014 2017 2017 2017 2016
## [70734] 2017 2010 2012 2011 2016 2016 2017 2009 2011 2017 2017 2016 2016
## [70747] 2015 2017 2014 2015 2017 2016 2016 2012 2010 2008 2015 2017 2017
## [70760] 2015 2011 2015 2015 2017 2009 2012 2010 2015 2017 2009 2011 2011
## [70773] 2017 2008 2016 2013 2016 2015 2011 2008 2010 2017 2016 2016 2017
## [70786] 2011 2015 2009 2017 2015 2016 2008 2016 2011 2010 2015 2008 2017
## [70799] 2011 2008 2017 2011 2013 2015 2008 2016 2009 2010 2009 2008 2011
## [70812] 2010 2017 2009 2016 2016 2017 2011 2010 2016 2016 2017 2017 2017
## [70825] 2015 2009 2007 2007 2015 2007 2015 2017 2017 2011 2008 2007 2016
## [70838] 2015 2010 2010 2011 2017 2015 2009 2009 2015 2015 2015 2015 2017
## [70851] 2015 2016 2017 2011 2016 2015 2013 2016 2017 2009 2017 2016 2007
## [70864] 2008 2008 2011 2008 2011 2010 2016 2016 2017 2008 2009 2013 2016
## [70877] 2012 2015 2015 2008 2015 2017 2010 2011 2010 2016 2017 2007 2009
## [70890] 2012 2017 2012 2014 2015 2016 2016 2017 2009 2011 2009 2016 2009
## [70903] 2012 2013 2017 2017 2008 2010 2009 2016 2016 2016 2016 2016 2016
## [70916] 2017 2017 2014 2017 2008 2010 2016 2017 2016 2008 2008 2011 2012
## [70929] 2015 2016 2012 2016 2017 2017 2007 2007 2009 2009 2015 2016 2009
## [70942] 2017 2016 2016 2013 2016 2016 2016 2017 2016 2016 2015 2007 2016
## [70955] 2016 2017 2008 2012 2008 2007 2013 2016 2011 2011 2017 2008 2015
## [70968] 2017 2009 2010 2015 2017 2010 2017 2016 2017 2017 2007 2009 2016
## [70981] 2011 2007 2016 2012 2012 2017 2017 2015 2016 2008 2009 2013 2014
## [70994] 2015 2016 2015 2017 2008 2012 2010 2015 2017 2009 2015 2017 2015
## [71007] 2011 2015 2010 2010 2011 2008 2017 2017 2017 2017 2011 2008 2015
## [71020] 2017 2012 2009 2009 2013 2007 2012 2016 2017 2017 2009 2016 2011
## [71033] 2008 2012 2017 2011 2017 2008 2014 2017 2010 2008 2015 2016 2012
## [71046] 2015 2016 2016 2007 2016 2016 2017 2009 2015 2007 2007 2017 2017
## [71059] 2014 2017 2017 2015 2010 2016 2014 2014 2016 2008 2011 2015 2016
## [71072] 2017 2017 2012 2016 2016 2016 2009 2011 2012 2008 2016 2017 2008
## [71085] 2007 2016 2015 2017 2011 2010 2009 2014 2016 2016 2008 2009 2015
## [71098] 2008 2014 2015 2016 2017 2017 2017 2008 2015 2016 2017 2007 2014
## [71111] 2016 2017 2017 2016 2016 2007 2009 2007 2008 2007 2015 2016 2008
## [71124] 2016 2007 2016 2015 2016 2016 2015 2013 2015 2016 2016 2011 2014
## [71137] 2015 2011 2012 2008 2017 2016 2015 2017 2008 2015 2016 2015 2009
## [71150] 2013 2015 2012 2017 2017 2016 2016 2017 2011 2016 2016 2008 2016
## [71163] 2016 2009 2012 2017 2013 2007 2014 2015 2016 2015 2008 2014 2009
## [71176] 2010 2014 2007 2014 2017 2008 2017 2012 2016 2015 2017 2011 2016
## [71189] 2017 2015 2014 2015 2015 2017 2010 2008 2007 2015 2009 2015 2017
## [71202] 2017 2009 2007 2016 2010 2015 2017 2017 2014 2015 2014 2017 2012
## [71215] 2008 2017 2017 2013 2009 2011 2014 2017 2017 2017 2011 2015 2017
## [71228] 2017 2008 2011 2010 2010 2011 2009 2008 2015 2017 2017 2014 2016
## [71241] 2008 2015 2008 2013 2010 2008 2016 2017 2013 2010 2015 2016 2012
## [71254] 2009 2016 2017 2010 2010 2016 2012 2007 2017 2010 2015 2008 2014
## [71267] 2017 2015 2008 2017 2010 2016 2008 2010 2007 2016 2016 2017 2007
## [71280] 2012 2014 2015 2015 2008 2017 2017 2007 2007 2017 2017 2016 2010
## [71293] 2016 2017 2014 2010 2017 2016 2016 2012 2015 2017 2017 2016 2007
## [71306] 2012 2015 2016 2008 2007 2011 2008 2012 2008 2009 2012 2009 2009
## [71319] 2009 2015 2016 2016 2016 2007 2016 2015 2015 2007 2013 2017 2017
## [71332] 2008 2009 2007 2015 2014 2016 2013 2015 2016 2016 2007 2012 2009
## [71345] 2015 2007 2009 2008 2014 2015 2017 2017 2009 2016 2010 2016 2008
## [71358] 2007 2016 2016 2007 2014 2017 2017 2016 2017 2007 2014 2016 2016
## [71371] 2017 2016 2017 2016 2008 2017 2011 2015 2007 2009 2015 2010 2017
## [71384] 2015 2017 2010 2009 2013 2011 2017 2016 2016 2016 2009 2010 2016
## [71397] 2017 2017 2017 2010 2010 2016 2017 2017 2008 2016 2015 2007 2012
## [71410] 2010 2017 2017 2011 2007 2017 2014 2015 2017 2015 2015 2016 2015
## [71423] 2017 2015 2016 2016 2014 2016 2016 2007 2009 2017 2009 2010 2007
## [71436] 2015 2007 2016 2017 2016 2009 2015 2016 2015 2011 2014 2016 2016
## [71449] 2016 2015 2007 2017 2017 2012 2017 2010 2015 2016 2016 2016 2015
## [71462] 2008 2017 2012 2007 2013 2015 2015 2017 2008 2017 2007 2016 2017
## [71475] 2015 2015 2015 2017 2008 2008 2016 2015 2015 2016 2016 2016 2017
## [71488] 2014 2015 2016 2015 2007 2013 2014 2015 2015 2016 2016 2011 2014
## [71501] 2016 2009 2014 2016 2007 2016 2008 2014 2016 2007 2007 2012 2016
## [71514] 2009 2016 2015 2010 2016 2015 2017 2017 2017 2016 2016 2016 2010
## [71527] 2010 2008 2017 2014 2014 2016 2016 2017 2016 2017 2015 2016 2017
## [71540] 2017 2013 2013 2008 2014 2016 2016 2017 2016 2017 2017 2017 2008
## [71553] 2015 2011 2011 2016 2016 2015 2014 2015 2015 2009 2015 2017 2011
## [71566] 2008 2017 2015 2008 2016 2015 2017 2017 2016 2017 2014 2014 2015
## [71579] 2016 2017 2017 2009 2015 2012 2017 2008 2014 2017 2012 2012 2015
## [71592] 2016 2015 2008 2015 2014 2016 2017 2010 2016 2013 2015 2015 2016
## [71605] 2016 2017 2015 2016 2017 2011 2016 2017 2017 2008 2011 2014 2016
## [71618] 2011 2008 2009 2009 2013 2016 2016 2016 2017 2011 2016 2016 2017
## [71631] 2010 2008 2011 2010 2008 2011 2012 2012 2014 2010 2013 2008 2008
## [71644] 2012 2013 2016 2016 2016 2017 2010 2012 2007 2007 2014 2017 2017
## [71657] 2007 2011 2011 2009 2015 2016 2009 2017 2007 2013 2016 2017 2010
## [71670] 2013 2016 2017 2016 2017 2007 2009 2017 2017 2012 2015 2017 2012
## [71683] 2015 2015 2008 2012 2016 2016 2010 2014 2016 2007 2016 2017 2015
## [71696] 2016 2015 2008 2008 2007 2015 2017 2017 2010 2009 2007 2007 2016
## [71709] 2010 2015 2017 2011 2009 2008 2010 2009 2014 2017 2010 2009 2015
## [71722] 2010 2015 2010 2008 2012 2016 2007 2008 2013 2009 2016 2016 2010
## [71735] 2016 2017 2017 2008 2012 2010 2017 2008 2011 2009 2012 2009 2016
## [71748] 2012 2016 2016 2012 2016 2008 2016 2015 2010 2013 2015 2011 2015
## [71761] 2015 2013 2015 2017 2016 2017 2017 2015 2016 2016 2015 2015 2009
## [71774] 2015 2016 2017 2016 2015 2017 2008 2011 2011 2015 2017 2010 2015
## [71787] 2016 2015 2017 2009 2008 2017 2012 2011 2014 2017 2015 2016 2015
## [71800] 2008 2011 2008 2012 2008 2017 2016 2017 2010 2016 2012 2009 2008
## [71813] 2013 2015 2015 2017 2009 2015 2016 2016 2016 2017 2012 2013 2016
## [71826] 2017 2009 2012 2012 2016 2017 2009 2009 2007 2009 2009 2009 2008
## [71839] 2017 2017 2015 2011 2016 2012 2011 2014 2015 2016 2016 2017 2013
## [71852] 2016 2016 2016 2010 2014 2015 2016 2016 2007 2011 2008 2009 2016
## [71865] 2007 2011 2011 2014 2016 2017 2017 2017 2010 2013 2015 2015 2017
## [71878] 2011 2017 2011 2007 2016 2015 2008 2010 2007 2012 2017 2015 2015
## [71891] 2017 2015 2015 2017 2015 2015 2017 2007 2008 2016 2008 2010 2009
## [71904] 2015 2010 2015 2017 2011 2016 2016 2017 2015 2015 2016 2016 2017
## [71917] 2007 2010 2016 2017 2017 2011 2011 2015 2016 2017 2007 2012 2008
## [71930] 2016 2017 2017 2014 2017 2017 2007 2012 2010 2013 2016 2012 2016
## [71943] 2017 2007 2011 2010 2013 2014 2014 2015 2015 2008 2017 2009 2015
## [71956] 2016 2017 2017 2017 2008 2013 2016 2016 2015 2016 2010 2015 2016
## [71969] 2008 2008 2014 2015 2011 2009 2017 2016 2016 2007 2012 2013 2015
## [71982] 2017 2012 2014 2017 2017 2015 2017 2008 2012 2009 2016 2008 2011
## [71995] 2015 2015 2016 2010 2017 2009 2012 2013 2016 2015 2015 2015 2016
## [72008] 2015 2017 2017 2014 2016 2017 2011 2017 2017 2011 2015 2017 2013
## [72021] 2015 2016 2015 2017 2009 2008 2011 2015 2016 2015 2017 2012 2015
## [72034] 2016 2016 2008 2016 2017 2015 2016 2016 2016 2007 2013 2008 2011
## [72047] 2016 2017 2017 2008 2015 2017 2012 2010 2008 2008 2015 2007 2008
## [72060] 2011 2015 2016 2012 2009 2014 2015 2015 2015 2012 2017 2008 2012
## [72073] 2012 2007 2015 2017 2007 2007 2016 2013 2013 2015 2017 2016 2013
## [72086] 2017 2008 2015 2010 2009 2014 2016 2016 2010 2012 2012 2013 2016
## [72099] 2015 2017 2017 2015 2015 2015 2016 2013 2014 2012 2008 2016 2016
## [72112] 2015 2011 2015 2015 2007 2013 2016 2012 2016 2017 2008 2013 2014
## [72125] 2016 2011 2007 2013 2015 2016 2017 2017 2016 2017 2017 2009 2015
## [72138] 2012 2015 2013 2016 2017 2015 2010 2017 2017 2008 2014 2015 2016
## [72151] 2007 2013 2015 2017 2017 2017 2017 2009 2013 2015 2016 2017 2015
## [72164] 2014 2011 2010 2015 2017 2010 2008 2016 2015 2017 2017 2009 2008
## [72177] 2015 2017 2016 2008 2008 2017 2017 2009 2008 2015 2017 2013 2009
## [72190] 2009 2013 2016 2017 2013 2016 2016 2016 2008 2010 2016 2010 2017
## [72203] 2012 2007 2016 2007 2015 2008 2011 2010 2010 2009 2014 2017 2008
## [72216] 2009 2012 2016 2016 2016 2009 2015 2008 2008 2015 2016 2008 2015
## [72229] 2017 2017 2017 2012 2008 2008 2017 2017 2014 2010 2013 2016 2016
## [72242] 2017 2015 2015 2011 2010 2009 2015 2015 2015 2017 2017 2008 2008
## [72255] 2008 2015 2017 2009 2015 2012 2015 2016 2016 2017 2015 2007 2008
## [72268] 2008 2015 2017 2016 2017 2015 2010 2009 2013 2016 2016 2017 2014
## [72281] 2016 2011 2012 2008 2016 2016 2010 2007 2014 2016 2011 2009 2016
## [72294] 2017 2015 2017 2007 2008 2015 2016 2015 2015 2016 2016 2017 2011
## [72307] 2011 2013 2017 2007 2010 2011 2016 2016 2016 2016 2016 2016 2016
## [72320] 2017 2008 2008 2016 2015 2017 2012 2008 2008 2013 2015 2014 2008
## [72333] 2014 2017 2013 2014 2015 2008 2010 2015 2017 2017 2008 2011 2013
## [72346] 2016 2016 2016 2017 2009 2007 2016 2013 2017 2017 2012 2008 2009
## [72359] 2017 2017 2011 2007 2016 2015 2017 2017 2008 2010 2017 2015 2017
## [72372] 2015 2010 2010 2009 2017 2017 2015 2016 2011 2016 2016 2011 2015
## [72385] 2007 2016 2016 2008 2009 2016 2008 2016 2011 2016 2011 2010 2016
## [72398] 2012 2011 2007 2016 2007 2011 2009 2009 2015 2015 2016 2016 2015
## [72411] 2010 2013 2016 2012 2009 2016 2015 2017 2007 2016 2011 2014 2017
## [72424] 2013 2016 2016 2017 2016 2017 2015 2016 2011 2008 2017 2010 2014
## [72437] 2015 2016 2017 2017 2016 2012 2010 2014 2016 2017 2015 2017 2012
## [72450] 2015 2008 2017 2017 2008 2016 2016 2016 2008 2015 2017 2015 2009
## [72463] 2009 2014 2009 2011 2015 2008 2016 2017 2007 2013 2015 2016 2017
## [72476] 2017 2016 2013 2017 2007 2010 2010 2012 2015 2009 2014 2015 2017
## [72489] 2017 2017 2016 2015 2017 2017 2017 2017 2011 2016 2015 2011 2017
## [72502] 2009 2009 2015 2016 2016 2016 2017 2017 2008 2011 2015 2015 2016
## [72515] 2016 2014 2016 2015 2014 2015 2017 2017 2014 2017 2017 2014 2015
## [72528] 2015 2016 2016 2016 2011 2016 2016 2007 2010 2016 2016 2008 2007
## [72541] 2013 2015 2016 2017 2017 2010 2013 2017 2009 2008 2013 2013 2016
## [72554] 2011 2008 2009 2015 2011 2015 2008 2015 2015 2016 2009 2016 2008
## [72567] 2009 2016 2017 2011 2015 2017 2017 2017 2015 2008 2014 2016 2012
## [72580] 2017 2013 2016 2015 2014 2015 2017 2017 2012 2008 2017 2009 2007
## [72593] 2015 2011 2011 2013 2016 2010 2009 2016 2007 2008 2017 2016 2017
## [72606] 2017 2016 2007 2015 2011 2016 2007 2012 2015 2016 2015 2012 2014
## [72619] 2015 2015 2017 2017 2009 2016 2016 2016 2007 2010 2014 2016 2017
## [72632] 2008 2008 2016 2010 2017 2007 2016 2010 2009 2016 2016 2015 2016
## [72645] 2010 2017 2010 2008 2010 2016 2008 2015 2016 2013 2017 2007 2016
## [72658] 2015 2009 2015 2016 2015 2016 2010 2015 2017 2012 2015 2007 2012
## [72671] 2009 2017 2012 2016 2017 2016 2011 2009 2015 2008 2014 2012 2013
## [72684] 2016 2017 2009 2012 2012 2010 2012 2011 2015 2008 2015 2016 2016
## [72697] 2010 2016 2017 2017 2008 2016 2016 2007 2009 2011 2009 2015 2016
## [72710] 2014 2007 2011 2015 2008 2008 2007 2007 2014 2016 2012 2016 2016
## [72723] 2007 2015 2016 2016 2016 2017 2009 2017 2015 2009 2014 2014 2015
## [72736] 2007 2009 2015 2016 2015 2009 2016 2017 2010 2011 2017 2015 2015
## [72749] 2011 2016 2017 2015 2015 2017 2010 2011 2013 2017 2007 2011 2015
## [72762] 2015 2012 2009 2009 2016 2017 2009 2014 2012 2012 2015 2015 2008
## [72775] 2017 2008 2009 2015 2016 2007 2014 2016 2017 2011 2013 2017 2012
## [72788] 2011 2013 2015 2015 2008 2010 2016 2017 2007 2014 2015 2016 2017
## [72801] 2016 2008 2016 2007 2015 2016 2017 2009 2008 2015 2010 2017 2011
## [72814] 2016 2017 2009 2015 2015 2015 2013 2016 2017 2010 2015 2017 2017
## [72827] 2017 2009 2010 2015 2015 2015 2008 2016 2017 2017 2012 2017 2017
## [72840] 2017 2010 2015 2017 2012 2016 2016 2009 2015 2016 2016 2007 2015
## [72853] 2016 2016 2016 2012 2017 2015 2016 2008 2016 2017 2017 2011 2008
## [72866] 2016 2015 2015 2016 2012 2014 2015 2017 2011 2012 2009 2016 2013
## [72879] 2012 2015 2015 2016 2008 2013 2015 2015 2007 2013 2017 2016 2016
## [72892] 2012 2012 2015 2016 2017 2010 2012 2007 2017 2009 2009 2014 2015
## [72905] 2017 2015 2011 2012 2015 2016 2016 2008 2011 2008 2014 2015 2016
## [72918] 2007 2016 2015 2010 2013 2016 2016 2017 2016 2014 2015 2016 2016
## [72931] 2017 2013 2014 2015 2016 2017 2007 2014 2007 2017 2015 2011 2012
## [72944] 2013 2014 2015 2015 2017 2015 2015 2013 2009 2011 2015 2016 2015
## [72957] 2008 2016 2009 2007 2016 2017 2010 2009 2015 2017 2012 2015 2016
## [72970] 2015 2017 2008 2015 2013 2017 2015 2015 2016 2014 2015 2017 2007
## [72983] 2013 2016 2015 2017 2017 2008 2013 2016 2011 2011 2007 2010 2010
## [72996] 2015 2015 2009 2008 2016 2017 2011 2012 2016 2017 2008 2009 2017
## [73009] 2015 2010 2016 2017 2017 2011 2008 2015 2015 2008 2016 2017 2013
## [73022] 2015 2016 2016 2016 2017 2016 2009 2011 2009 2009 2016 2013 2015
## [73035] 2016 2008 2015 2009 2015 2016 2016 2009 2010 2016 2007 2016 2015
## [73048] 2008 2012 2015 2017 2016 2017 2011 2009 2015 2017 2009 2007 2015
## [73061] 2007 2011 2011 2009 2016 2014 2015 2016 2016 2017 2017 2010 2008
## [73074] 2017 2011 2017 2010 2016 2016 2013 2014 2015 2011 2016 2016 2010
## [73087] 2008 2011 2011 2014 2010 2015 2015 2017 2009 2015 2017 2008 2014
## [73100] 2010 2016 2015 2007 2013 2016 2008 2015 2012 2007 2011 2016 2012
## [73113] 2015 2009 2009 2007 2015 2015 2008 2011 2015 2015 2008 2010 2013
## [73126] 2014 2015 2016 2009 2016 2015 2017 2015 2008 2011 2016 2017 2010
## [73139] 2017 2015 2017 2015 2016 2012 2008 2011 2011 2007 2010 2016 2017
## [73152] 2017 2007 2007 2013 2016 2011 2015 2016 2013 2017 2017 2017 2008
## [73165] 2008 2015 2016 2016 2007 2007 2016 2017 2012 2015 2015 2011 2012
## [73178] 2011 2017 2017 2009 2016 2009 2015 2008 2015 2016 2010 2011 2012
## [73191] 2007 2017 2008 2017 2008 2012 2015 2015 2007 2017 2016 2015 2010
## [73204] 2010 2017 2011 2009 2016 2017 2017 2017 2010 2015 2017 2017 2008
## [73217] 2017 2015 2016 2017 2011 2009 2015 2011 2007 2013 2017 2012 2015
## [73230] 2016 2007 2007 2008 2011 2010 2009 2015 2015 2016 2017 2010 2014
## [73243] 2016 2016 2016 2017 2017 2008 2009 2016 2017 2012 2017 2008 2011
## [73256] 2015 2016 2016 2015 2015 2017 2010 2008 2008 2015 2016 2016 2017
## [73269] 2017 2007 2015 2016 2007 2007 2012 2016 2017 2007 2013 2016 2016
## [73282] 2015 2015 2016 2016 2016 2009 2014 2015 2008 2016 2012 2013 2014
## [73295] 2010 2015 2016 2017 2012 2012 2013 2016 2016 2017 2012 2010 2009
## [73308] 2015 2015 2015 2009 2015 2015 2017 2017 2007 2017 2017 2011 2015
## [73321] 2015 2016 2007 2009 2015 2009 2009 2012 2007 2015 2015 2016 2017
## [73334] 2017 2010 2007 2011 2015 2015 2016 2007 2016 2015 2012 2009 2016
## [73347] 2009 2011 2013 2016 2016 2015 2017 2010 2008 2017 2015 2008 2016
## [73360] 2016 2017 2014 2017 2015 2016 2016 2011 2017 2015 2011 2017 2013
## [73373] 2008 2007 2015 2016 2008 2016 2015 2012 2016 2016 2017 2011 2015
## [73386] 2008 2014 2015 2015 2015 2017 2012 2007 2015 2017 2008 2010 2016
## [73399] 2017 2017 2013 2014 2015 2016 2017 2017 2015 2015 2016 2007 2015
## [73412] 2009 2014 2015 2012 2010 2014 2017 2008 2013 2016 2017 2017 2012
## [73425] 2009 2008 2012 2016 2016 2017 2015 2016 2016 2007 2015 2017 2015
## [73438] 2016 2017 2017 2009 2007 2016 2015 2012 2016 2017 2013 2015 2017
## [73451] 2009 2013 2016 2016 2017 2010 2013 2015 2017 2017 2012 2016 2011
## [73464] 2010 2012 2017 2014 2015 2017 2007 2016 2017 2010 2013 2017 2012
## [73477] 2010 2010 2009 2017 2015 2017 2016 2015 2015 2017 2016 2014 2016
## [73490] 2011 2014 2015 2015 2017 2015 2009 2017 2015 2017 2008 2015 2014
## [73503] 2016 2016 2017 2012 2013 2013 2014 2007 2016 2017 2017 2015 2017
## [73516] 2017 2017 2016 2013 2015 2016 2017 2016 2008 2016 2015 2008 2016
## [73529] 2010 2017 2009 2011 2016 2010 2012 2009 2017 2009 2011 2013 2015
## [73542] 2016 2011 2011 2016 2017 2015 2007 2014 2017 2008 2010 2009 2015
## [73555] 2016 2013 2016 2017 2016 2016 2014 2015 2017 2010 2015 2016 2012
## [73568] 2016 2017 2017 2015 2017 2014 2017 2011 2016 2016 2017 2017 2011
## [73581] 2016 2013 2015 2015 2016 2016 2010 2013 2016 2016 2012 2012 2016
## [73594] 2016 2016 2007 2011 2016 2016 2017 2011 2016 2015 2015 2013 2015
## [73607] 2012 2015 2015 2010 2015 2015 2016 2017 2013 2017 2007 2008 2013
## [73620] 2016 2017 2014 2016 2016 2014 2016 2016 2015 2017 2011 2007 2009
## [73633] 2009 2010 2013 2015 2013 2016 2013 2013 2015 2017 2010 2016 2015
## [73646] 2011 2012 2012 2007 2015 2017 2011 2010 2008 2007 2015 2016 2015
## [73659] 2017 2017 2011 2011 2017 2017 2013 2013 2016 2016 2014 2016 2015
## [73672] 2007 2017 2007 2013 2015 2016 2017 2016 2015 2010 2016 2015 2016
## [73685] 2017 2007 2017 2011 2008 2007 2016 2013 2016 2016 2016 2015 2017
## [73698] 2016 2015 2017 2008 2011 2014 2015 2010 2011 2016 2016 2017 2017
## [73711] 2017 2013 2014 2017 2014 2008 2015 2016 2017 2011 2015 2016 2017
## [73724] 2017 2008 2012 2016 2016 2011 2010 2011 2014 2015 2017 2011 2007
## [73737] 2009 2017 2008 2012 2016 2016 2015 2017 2017 2017 2011 2009 2016
## [73750] 2016 2017 2017 2010 2016 2017 2016 2017 2009 2014 2015 2015 2007
## [73763] 2015 2012 2014 2016 2013 2013 2015 2017 2009 2007 2013 2015 2016
## [73776] 2017 2017 2015 2013 2014 2011 2016 2017 2015 2016 2009 2011 2015
## [73789] 2016 2011 2013 2012 2017 2011 2012 2016 2015 2007 2008 2012 2015
## [73802] 2015 2007 2015 2014 2017 2007 2015 2015 2015 2015 2017 2012 2016
## [73815] 2017 2011 2016 2013 2017 2011 2009 2016 2017 2010 2007 2017 2017
## [73828] 2017 2007 2013 2014 2015 2017 2017 2011 2009 2009 2017 2008 2016
## [73841] 2009 2015 2016 2011 2010 2017 2017 2017 2015 2010 2009 2012 2009
## [73854] 2016 2017 2017 2007 2009 2016 2017 2017 2017 2011 2012 2016 2017
## [73867] 2012 2016 2016 2015 2017 2007 2015 2016 2016 2017 2017 2013 2007
## [73880] 2013 2013 2017 2017 2008 2013 2017 2011 2008 2010 2013 2016 2016
## [73893] 2017 2011 2016 2017 2010 2008 2009 2009 2011 2013 2016 2016 2008
## [73906] 2014 2017 2007 2016 2017 2007 2013 2016 2011 2015 2015 2008 2015
## [73919] 2016 2017 2016 2017 2015 2015 2010 2012 2016 2016 2007 2013 2014
## [73932] 2012 2016 2015 2016 2016 2017 2010 2017 2009 2014 2016 2011 2015
## [73945] 2017 2011 2012 2015 2014 2009 2007 2016 2009 2016 2015 2017 2017
## [73958] 2015 2016 2017 2015 2012 2015 2010 2017 2015 2008 2007 2015 2013
## [73971] 2017 2009 2007 2017 2015 2009 2010 2015 2013 2016 2016 2009 2015
## [73984] 2012 2014 2016 2007 2016 2016 2010 2017 2007 2013 2015 2015 2016
## [73997] 2008 2016 2017 2012 2017 2011 2008 2008 2016 2017 2015 2016 2017
## [74010] 2016 2012 2015 2007 2009 2015 2017 2017 2013 2014 2016 2016 2013
## [74023] 2016 2017 2008 2016 2015 2015 2013 2016 2017 2008 2015 2009 2008
## [74036] 2015 2016 2016 2007 2008 2008 2014 2014 2015 2017 2015 2009 2017
## [74049] 2017 2017 2016 2016 2017 2016 2017 2017 2010 2015 2017 2012 2017
## [74062] 2015 2013 2016 2017 2010 2012 2007 2014 2014 2016 2016 2009 2013
## [74075] 2011 2017 2010 2011 2007 2015 2012 2009 2016 2016 2017 2017 2013
## [74088] 2016 2012 2013 2017 2017 2016 2017 2009 2010 2016 2016 2017 2017
## [74101] 2017 2008 2015 2013 2015 2010 2017 2008 2013 2017 2017 2013 2016
## [74114] 2015 2017 2017 2008 2012 2010 2017 2017 2013 2016 2016 2017 2013
## [74127] 2011 2013 2007 2017 2014 2007 2016 2017 2017 2009 2012 2013 2016
## [74140] 2017 2017 2015 2016 2016 2008 2017 2017 2015 2016 2015 2016 2017
## [74153] 2008 2015 2016 2012 2010 2013 2015 2011 2013 2014 2016 2017 2013
## [74166] 2016 2015 2011 2007 2017 2012 2007 2016 2017 2009 2011 2015 2017
## [74179] 2009 2010 2016 2011 2007 2016 2014 2016 2017 2017 2017 2013 2015
## [74192] 2016 2016 2015 2017 2012 2015 2016 2017 2013 2007 2008 2016 2010
## [74205] 2016 2016 2007 2015 2016 2015 2017 2017 2007 2011 2013 2015 2015
## [74218] 2016 2014 2017 2016 2016 2017 2017 2007 2008 2016 2017 2017 2014
## [74231] 2016 2016 2016 2011 2014 2016 2017 2015 2016 2011 2016 2007 2017
## [74244] 2009 2016 2016 2016 2012 2015 2013 2015 2010 2014 2013 2016 2017
## [74257] 2017 2008 2015 2012 2017 2009 2017 2016 2017 2015 2015 2015 2015
## [74270] 2015 2016 2017 2011 2017 2016 2010 2013 2016 2016 2016 2017 2007
## [74283] 2008 2007 2013 2017 2012 2007 2015 2017 2016 2016 2017 2010 2014
## [74296] 2016 2017 2017 2016 2015 2010 2016 2008 2010 2008 2007 2015 2016
## [74309] 2012 2016 2016 2015 2016 2016 2017 2008 2016 2017 2012 2012 2009
## [74322] 2014 2008 2010 2014 2009 2014 2017 2007 2016 2016 2015 2016 2009
## [74335] 2016 2011 2011 2016 2009 2016 2015 2016 2008 2015 2016 2016 2015
## [74348] 2009 2007 2017 2015 2016 2017 2010 2012 2016 2017 2007 2014 2016
## [74361] 2016 2011 2015 2015 2016 2015 2017 2009 2016 2017 2012 2007 2017
## [74374] 2017 2007 2011 2015 2011 2017 2017 2012 2013 2015 2015 2017 2011
## [74387] 2008 2011 2010 2017 2010 2017 2016 2017 2007 2014 2017 2017 2010
## [74400] 2007 2012 2017 2016 2016 2012 2014 2015 2015 2016 2016 2017 2016
## [74413] 2016 2016 2009 2016 2017 2013 2015 2017 2007 2017 2010 2008 2012
## [74426] 2015 2016 2014 2016 2015 2011 2012 2016 2010 2016 2016 2011 2012
## [74439] 2017 2017 2011 2014 2016 2016 2016 2017 2013 2013 2014 2016 2017
## [74452] 2010 2010 2016 2016 2009 2007 2015 2017 2017 2017 2008 2015 2017
## [74465] 2015 2016 2016 2017 2015 2016 2015 2017 2017 2017 2007 2008 2015
## [74478] 2017 2011 2013 2016 2017 2015 2016 2012 2009 2013 2016 2008 2016
## [74491] 2015 2017 2011 2015 2017 2017 2010 2015 2016 2016 2017 2017 2009
## [74504] 2016 2016 2017 2011 2017 2017 2012 2015 2009 2015 2011 2017 2015
## [74517] 2011 2016 2013 2008 2014 2015 2015 2009 2007 2008 2010 2017 2017
## [74530] 2015 2016 2010 2016 2017 2012 2015 2015 2015 2015 2015 2017 2015
## [74543] 2015 2016 2016 2017 2010 2016 2016 2012 2007 2015 2016 2012 2016
## [74556] 2007 2009 2013 2014 2016 2016 2017 2012 2015 2017 2008 2015 2011
## [74569] 2015 2017 2009 2013 2015 2015 2011 2014 2015 2015 2015 2017 2010
## [74582] 2007 2010 2011 2011 2016 2016 2017 2012 2017 2008 2015 2015 2015
## [74595] 2015 2011 2007 2010 2017 2007 2012 2011 2016 2013 2013 2015 2015
## [74608] 2017 2009 2016 2016 2017 2009 2013 2015 2015 2015 2017 2015 2016
## [74621] 2016 2014 2015 2015 2015 2017 2017 2008 2016 2017 2016 2016 2010
## [74634] 2016 2016 2016 2011 2014 2015 2017 2016 2017 2017 2017 2010 2015
## [74647] 2016 2017 2008 2017 2016 2016 2016 2017 2011 2014 2015 2015 2017
## [74660] 2016 2016 2017 2007 2015 2014 2014 2016 2016 2010 2009 2013 2016
## [74673] 2008 2011 2016 2013 2014 2014 2016 2016 2017 2016 2015 2015 2012
## [74686] 2007 2015 2011 2015 2010 2012 2008 2015 2015 2017 2017 2007 2011
## [74699] 2013 2016 2013 2015 2016 2009 2012 2008 2017 2017 2012 2016 2007
## [74712] 2010 2012 2016 2017 2017 2010 2017 2007 2017 2008 2009 2012 2010
## [74725] 2012 2007 2013 2014 2017 2010 2011 2016 2009 2014 2016 2016 2017
## [74738] 2015 2014 2012 2013 2014 2014 2016 2010 2007 2017 2011 2010 2016
## [74751] 2011 2007 2014 2015 2011 2015 2015 2012 2009 2008 2017 2017 2007
## [74764] 2014 2011 2016 2012 2013 2008 2014 2015 2013 2014 2016 2016 2008
## [74777] 2017 2016 2017 2007 2016 2017 2008 2011 2008 2015 2016 2017 2008
## [74790] 2015 2016 2016 2017 2012 2012 2012 2012 2014 2015 2017 2009 2015
## [74803] 2017 2017 2017 2011 2008 2017 2016 2017 2017 2011 2012 2013 2015
## [74816] 2016 2010 2016 2017 2016 2017 2017 2010 2009 2015 2016 2010 2017
## [74829] 2011 2013 2015 2011 2011 2015 2015 2015 2015 2017 2015 2016 2015
## [74842] 2007 2016 2016 2013 2015 2017 2014 2016 2010 2012 2015 2013 2015
## [74855] 2013 2013 2015 2017 2012 2010 2009 2016 2015 2017 2007 2008 2016
## [74868] 2016 2007 2015 2011 2013 2016 2017 2011 2015 2015 2009 2016 2017
## [74881] 2013 2016 2017 2009 2012 2011 2015 2014 2016 2017 2017 2014 2010
## [74894] 2015 2016 2016 2017 2008 2014 2009 2010 2011 2009 2016 2016 2017
## [74907] 2008 2008 2011 2012 2017 2011 2008 2008 2017 2009 2011 2013 2014
## [74920] 2015 2015 2015 2007 2016 2015 2015 2017 2017 2017 2016 2017 2017
## [74933] 2015 2014 2007 2015 2010 2016 2016 2016 2017 2015 2015 2008 2016
## [74946] 2011 2008 2012 2010 2017 2007 2015 2013 2016 2017 2017 2009 2016
## [74959] 2017 2011 2014 2017 2016 2016 2017 2007 2015 2017 2010 2009 2014
## [74972] 2017 2015 2008 2016 2017 2013 2015 2015 2017 2009 2008 2016 2016
## [74985] 2017 2009 2016 2017 2013 2016 2015 2008 2015 2016 2011 2015 2017
## [74998] 2017 2011 2012 2016 2016 2009 2008 2017 2015 2017 2008 2016 2016
## [75011] 2015 2017 2011 2010 2010 2007 2015 2016 2017 2009 2015 2015 2016
## [75024] 2009 2016 2016 2017 2013 2017 2015 2016 2012 2012 2015 2016 2015
## [75037] 2016 2017 2013 2015 2016 2016 2010 2015 2017 2014 2016 2017 2008
## [75050] 2010 2016 2011 2008 2011 2014 2008 2016 2016 2014 2016 2015 2016
## [75063] 2007 2007 2010 2011 2016 2017 2017 2017 2017 2007 2009 2012 2015
## [75076] 2016 2017 2010 2013 2015 2015 2016 2017 2015 2017 2010 2008 2012
## [75089] 2008 2014 2009 2015 2015 2009 2014 2017 2017 2017 2017 2017 2015
## [75102] 2015 2017 2007 2017 2017 2017 2009 2007 2008 2014 2009 2011 2014
## [75115] 2015 2011 2008 2017 2017 2009 2010 2015 2015 2016 2011 2015 2009
## [75128] 2010 2015 2015 2012 2013 2017 2007 2007 2007 2007 2010 2014 2007
## [75141] 2007 2014 2015 2015 2015 2017 2015 2016 2016 2007 2010 2009 2015
## [75154] 2016 2017 2009 2009 2016 2011 2008 2013 2008 2016 2017 2014 2015
## [75167] 2010 2015 2015 2015 2016 2017 2012 2016 2007 2008 2007 2013 2016
## [75180] 2017 2017 2017 2010 2007 2011 2015 2017 2017 2015 2017 2009 2010
## [75193] 2010 2015 2016 2010 2011 2015 2016 2016 2016 2017 2012 2014 2014
## [75206] 2015 2016 2012 2016 2010 2015 2009 2016 2016 2015 2017 2012 2013
## [75219] 2012 2015 2016 2015 2017 2010 2015 2009 2011 2016 2008 2010 2016
## [75232] 2012 2008 2017 2011 2009 2016 2015 2008 2013 2016 2016 2016 2009
## [75245] 2013 2016 2017 2011 2013 2015 2016 2015 2009 2013 2015 2017 2013
## [75258] 2015 2017 2011 2016 2015 2015 2015 2017 2010 2015 2015 2007 2015
## [75271] 2016 2016 2016 2008 2007 2016 2009 2011 2007 2008 2015 2016 2010
## [75284] 2009 2017 2009 2017 2017 2017 2011 2008 2011 2014 2015 2012 2017
## [75297] 2017 2009 2013 2016 2010 2014 2017 2016 2010 2009 2012 2016 2016
## [75310] 2017 2008 2012 2016 2012 2013 2015 2016 2015 2017 2017 2017 2008
## [75323] 2016 2009 2015 2016 2009 2010 2014 2017 2011 2017 2016 2016 2017
## [75336] 2017 2011 2014 2015 2016 2017 2015 2011 2013 2010 2011 2016 2008
## [75349] 2016 2015 2016 2016 2017 2017 2009 2007 2016 2017 2009 2015 2017
## [75362] 2012 2009 2015 2008 2008 2014 2007 2008 2014 2017 2008 2011 2010
## [75375] 2016 2016 2012 2015 2017 2015 2016 2011 2014 2016 2014 2016 2016
## [75388] 2011 2011 2011 2015 2016 2013 2016 2017 2017 2008 2012 2016 2016
## [75401] 2009 2012 2014 2015 2015 2007 2007 2009 2014 2017 2008 2017 2015
## [75414] 2015 2011 2011 2012 2016 2011 2015 2017 2012 2013 2016 2007 2017
## [75427] 2013 2012 2015 2017 2017 2016 2007 2010 2014 2015 2015 2010 2007
## [75440] 2009 2009 2013 2015 2017 2009 2016 2014 2008 2016 2016 2016 2016
## [75453] 2010 2013 2007 2015 2017 2011 2016 2017 2007 2013 2015 2017 2014
## [75466] 2012 2011 2012 2015 2015 2015 2007 2010 2015 2016 2011 2014 2016
## [75479] 2015 2016 2007 2013 2013 2016 2008 2009 2010 2009 2016 2015 2008
## [75492] 2011 2015 2015 2012 2009 2016 2013 2015 2009 2008 2014 2015 2016
## [75505] 2016 2016 2016 2017 2015 2016 2014 2017 2008 2008 2012 2015 2017
## [75518] 2010 2010 2016 2007 2010 2015 2016 2010 2008 2010 2017 2011 2017
## [75531] 2007 2016 2017 2017 2014 2009 2014 2015 2016 2014 2014 2008 2010
## [75544] 2017 2016 2016 2015 2015 2010 2015 2017 2017 2013 2010 2011 2016
## [75557] 2017 2015 2017 2012 2013 2017 2015 2017 2015 2016 2017 2014 2016
## [75570] 2014 2014 2015 2016 2016 2015 2017 2017 2017 2009 2016 2017 2009
## [75583] 2015 2016 2009 2016 2017 2017 2016 2007 2017 2015 2008 2015 2017
## [75596] 2011 2008 2016 2016 2017 2015 2015 2013 2015 2015 2016 2011 2011
## [75609] 2013 2017 2011 2014 2016 2011 2015 2017 2011 2015 2016 2009 2011
## [75622] 2012 2013 2016 2017 2015 2017 2017 2016 2007 2007 2014 2014 2007
## [75635] 2013 2017 2010 2015 2017 2013 2015 2016 2016 2017 2007 2007 2015
## [75648] 2017 2016 2009 2008 2014 2015 2015 2008 2016 2017 2010 2009 2016
## [75661] 2017 2016 2007 2009 2015 2016 2016 2016 2015 2016 2012 2008 2017
## [75674] 2011 2011 2016 2015 2015 2015 2008 2012 2009 2010 2016 2015 2011
## [75687] 2007 2010 2013 2015 2015 2016 2007 2016 2017 2010 2009 2011 2015
## [75700] 2009 2011 2017 2008 2017 2012 2013 2015 2015 2016 2017 2008 2016
## [75713] 2017 2017 2012 2014 2014 2016 2016 2017 2017 2011 2010 2013 2016
## [75726] 2016 2011 2009 2015 2016 2009 2011 2016 2017 2017 2011 2009 2008
## [75739] 2010 2016 2015 2009 2012 2014 2017 2013 2015 2017 2010 2009 2016
## [75752] 2007 2013 2016 2016 2008 2015 2016 2016 2017 2011 2010 2016 2017
## [75765] 2012 2008 2011 2009 2013 2015 2015 2017 2013 2014 2015 2011 2015
## [75778] 2008 2015 2016 2016 2017 2009 2017 2008 2007 2008 2014 2015 2015
## [75791] 2011 2009 2016 2015 2007 2011 2014 2014 2014 2015 2016 2016 2015
## [75804] 2016 2016 2017 2017 2017 2017 2009 2017 2010 2015 2016 2016 2010
## [75817] 2014 2015 2011 2015 2017 2014 2015 2008 2014 2015 2015 2017 2010
## [75830] 2015 2016 2016 2017 2017 2015 2017 2009 2014 2015 2007 2017 2011
## [75843] 2010 2008 2008 2015 2016 2012 2014 2014 2016 2007 2011 2008 2015
## [75856] 2016 2017 2017 2016 2015 2016 2011 2008 2015 2007 2010 2011 2012
## [75869] 2015 2017 2013 2010 2011 2011 2015 2012 2009 2014 2017 2016 2015
## [75882] 2011 2009 2014 2015 2016 2015 2016 2016 2017 2011 2011 2008 2008
## [75895] 2017 2017 2017 2013 2013 2014 2015 2016 2015 2017 2015 2017 2017
## [75908] 2012 2009 2016 2010 2016 2015 2011 2013 2016 2017 2017 2017 2008
## [75921] 2008 2012 2013 2017 2007 2013 2015 2017 2014 2015 2017 2011 2016
## [75934] 2017 2007 2012 2016 2016 2010 2015 2007 2017 2012 2010 2014 2016
## [75947] 2009 2016 2015 2011 2007 2015 2017 2015 2016 2017 2017 2016 2017
## [75960] 2009 2014 2016 2017 2012 2015 2017 2016 2017 2010 2008 2011 2009
## [75973] 2014 2015 2017 2017 2008 2007 2010 2010 2015 2007 2010 2015 2016
## [75986] 2017 2015 2017 2014 2016 2010 2014 2016 2015 2011 2016 2017 2015
## [75999] 2008 2008 2013 2017 2013 2015 2016 2017 2017 2010 2016 2017 2017
## [76012] 2015 2017 2010 2017 2016 2017 2014 2016 2017 2009 2013 2015 2015
## [76025] 2017 2017 2009 2011 2016 2012 2007 2017 2012 2013 2016 2008 2008
## [76038] 2015 2008 2017 2010 2016 2009 2016 2010 2013 2016 2017 2009 2016
## [76051] 2017 2015 2008 2010 2007 2011 2016 2016 2017 2017 2017 2007 2015
## [76064] 2008 2009 2009 2012 2012 2014 2016 2015 2012 2008 2014 2016 2012
## [76077] 2015 2009 2013 2016 2017 2007 2008 2009 2008 2016 2016 2016 2016
## [76090] 2015 2017 2015 2016 2016 2008 2010 2016 2009 2013 2017 2013 2017
## [76103] 2010 2016 2008 2015 2014 2016 2015 2013 2015 2009 2013 2016 2008
## [76116] 2013 2015 2015 2013 2016 2016 2010 2008 2007 2017 2016 2017 2017
## [76129] 2017 2007 2013 2015 2017 2017 2010 2017 2012 2008 2011 2015 2017
## [76142] 2015 2015 2011 2007 2010 2010 2015 2016 2009 2015 2016 2015 2017
## [76155] 2017 2008 2015 2016 2015 2015 2016 2017 2017 2017 2007 2010 2015
## [76168] 2016 2017 2014 2015 2016 2016 2009 2010 2014 2014 2016 2016 2009
## [76181] 2013 2016 2017 2017 2009 2011 2013 2014 2014 2015 2007 2015 2015
## [76194] 2017 2016 2008 2015 2017 2016 2007 2009 2014 2017 2007 2011 2007
## [76207] 2017 2016 2009 2011 2013 2015 2015 2017 2017 2014 2012 2009 2007
## [76220] 2012 2017 2009 2007 2016 2011 2017 2009 2012 2009 2016 2017 2017
## [76233] 2010 2011 2011 2008 2010 2017 2017 2010 2008 2012 2016 2015 2015
## [76246] 2016 2016 2017 2015 2010 2008 2010 2007 2017 2008 2015 2016 2008
## [76259] 2012 2016 2017 2015 2007 2015 2015 2015 2016 2007 2016 2017 2009
## [76272] 2007 2010 2007 2015 2016 2017 2007 2010 2016 2017 2009 2015 2017
## [76285] 2008 2007 2014 2011 2014 2017 2011 2016 2011 2008 2009 2011 2011
## [76298] 2010 2015 2016 2016 2012 2013 2014 2016 2009 2009 2009 2013 2016
## [76311] 2017 2013 2017 2017 2009 2009 2015 2013 2017 2017 2010 2012 2016
## [76324] 2015 2016 2017 2008 2017 2009 2008 2008 2016 2007 2013 2016 2016
## [76337] 2016 2013 2011 2008 2013 2015 2015 2016 2017 2009 2016 2016 2010
## [76350] 2014 2009 2015 2011 2009 2014 2017 2017 2008 2014 2015 2008 2008
## [76363] 2010 2016 2009 2016 2013 2016 2017 2017 2009 2015 2016 2008 2010
## [76376] 2010 2015 2016 2017 2011 2016 2017 2017 2011 2010 2010 2011 2016
## [76389] 2016 2017 2013 2015 2015 2016 2016 2015 2015 2017 2010 2011 2007
## [76402] 2015 2015 2012 2013 2014 2015 2016 2015 2007 2013 2016 2017 2014
## [76415] 2015 2017 2013 2016 2016 2017 2017 2012 2016 2016 2012 2007 2015
## [76428] 2017 2007 2007 2013 2016 2012 2015 2015 2017 2010 2009 2011 2016
## [76441] 2017 2008 2016 2015 2016 2016 2016 2011 2010 2009 2013 2016 2014
## [76454] 2015 2014 2016 2017 2016 2016 2017 2015 2016 2016 2017 2009 2014
## [76467] 2015 2016 2013 2016 2015 2007 2015 2016 2016 2017 2009 2012 2012
## [76480] 2013 2014 2014 2008 2015 2016 2009 2016 2015 2012 2007 2016 2010
## [76493] 2016 2017 2009 2016 2015 2010 2015 2017 2015 2017 2017 2017 2015
## [76506] 2016 2016 2017 2015 2015 2011 2012 2015 2007 2014 2012 2014 2015
## [76519] 2015 2016 2016 2015 2017 2011 2014 2016 2016 2017 2015 2015 2016
## [76532] 2015 2016 2016 2017 2011 2011 2011 2015 2009 2009 2013 2016 2016
## [76545] 2017 2013 2015 2007 2009 2015 2016 2017 2008 2015 2016 2014 2015
## [76558] 2012 2009 2015 2015 2008 2007 2014 2017 2017 2013 2014 2016 2012
## [76571] 2008 2017 2010 2011 2008 2016 2015 2011 2016 2017 2010 2010 2010
## [76584] 2011 2015 2017 2017 2015 2017 2009 2013 2016 2017 2015 2008 2016
## [76597] 2017 2017 2017 2016 2010 2010 2015 2016 2017 2011 2010 2017 2009
## [76610] 2014 2016 2017 2008 2016 2009 2011 2014 2014 2016 2014 2016 2016
## [76623] 2010 2008 2014 2017 2017 2017 2008 2010 2010 2015 2017 2013 2016
## [76636] 2015 2016 2016 2016 2016 2017 2016 2017 2014 2015 2017 2008 2007
## [76649] 2015 2017 2008 2008 2010 2012 2007 2015 2014 2015 2016 2009 2017
## [76662] 2016 2017 2017 2016 2017 2017 2008 2010 2016 2017 2014 2015 2017
## [76675] 2017 2017 2015 2016 2016 2017 2009 2015 2015 2015 2016 2016 2017
## [76688] 2009 2017 2016 2009 2017 2015 2012 2016 2012 2007 2010 2016 2015
## [76701] 2008 2010 2015 2017 2010 2017 2010 2016 2016 2016 2007 2015 2015
## [76714] 2017 2014 2008 2007 2016 2017 2015 2016 2016 2016 2017 2015 2017
## [76727] 2010 2017 2009 2015 2014 2016 2015 2012 2010 2008 2015 2015 2017
## [76740] 2017 2014 2013 2014 2007 2008 2016 2016 2017 2007 2011 2008 2016
## [76753] 2017 2015 2017 2015 2017 2017 2017 2015 2017 2013 2016 2017 2014
## [76766] 2014 2010 2012 2016 2016 2010 2015 2016 2017 2015 2015 2017 2017
## [76779] 2015 2017 2008 2012 2008 2015 2010 2015 2015 2017 2017 2014 2016
## [76792] 2008 2010 2012 2016 2016 2015 2008 2016 2017 2008 2007 2017 2015
## [76805] 2017 2016 2017 2016 2015 2017 2008 2008 2015 2016 2016 2017 2017
## [76818] 2017 2015 2017 2011 2013 2014 2015 2007 2007 2016 2017 2016 2017
## [76831] 2017 2013 2014 2007 2015 2015 2016 2012 2013 2017 2017 2011 2013
## [76844] 2017 2012 2014 2015 2017 2017 2017 2017 2007 2014 2016 2015 2013
## [76857] 2012 2014 2015 2007 2013 2014 2016 2017 2011 2016 2010 2008 2010
## [76870] 2012 2013 2017 2017 2010 2009 2016 2013 2017 2017 2010 2014 2013
## [76883] 2007 2013 2015 2015 2008 2015 2017 2016 2016 2015 2017 2017 2016
## [76896] 2016 2017 2016 2008 2011 2015 2015 2016 2009 2016 2017 2013 2016
## [76909] 2010 2013 2016 2016 2015 2010 2015 2017 2013 2016 2016 2016 2009
## [76922] 2017 2015 2017 2007 2008 2015 2015 2016 2015 2017 2017 2017 2007
## [76935] 2008 2009 2017 2010 2017 2016 2012 2008 2014 2016 2017 2012 2010
## [76948] 2009 2010 2015 2013 2016 2016 2009 2011 2016 2016 2008 2015 2016
## [76961] 2017 2015 2017 2010 2017 2016 2017 2013 2017 2008 2017 2015 2016
## [76974] 2008 2015 2009 2008 2015 2016 2016 2013 2015 2017 2017 2017 2010
## [76987] 2016 2011 2015 2017 2015 2016 2012 2008 2014 2015 2011 2016 2010
## [77000] 2009 2011 2012 2013 2017 2017 2016 2015 2012 2008 2008 2015 2015
## [77013] 2016 2015 2007 2013 2016 2012 2010 2009 2017 2014 2010 2015 2015
## [77026] 2017 2014 2014 2007 2007 2016 2017 2016 2010 2010 2007 2011 2011
## [77039] 2012 2015 2017 2008 2010 2011 2016 2011 2014 2014 2014 2015 2010
## [77052] 2017 2011 2009 2009 2009 2016 2012 2013 2010 2016 2015 2017 2015
## [77065] 2007 2016 2016 2007 2015 2016 2017 2017 2017 2008 2015 2015 2017
## [77078] 2009 2007 2012 2014 2012 2010 2015 2017 2010 2017 2009 2016 2016
## [77091] 2016 2016 2017 2016 2017 2017 2010 2009 2008 2013 2014 2016 2017
## [77104] 2016 2009 2008 2015 2017 2017 2017 2016 2017 2007 2007 2011 2015
## [77117] 2012 2015 2015 2017 2017 2010 2015 2014 2016 2013 2011 2009 2013
## [77130] 2013 2016 2017 2017 2015 2015 2009 2013 2016 2015 2008 2015 2017
## [77143] 2017 2016 2017 2015 2011 2008 2007 2015 2015 2016 2015 2017 2010
## [77156] 2012 2012 2017 2016 2008 2016 2017 2012 2015 2017 2014 2014 2014
## [77169] 2016 2015 2015 2016 2016 2017 2011 2012 2016 2013 2015 2016 2007
## [77182] 2008 2008 2007 2012 2011 2015 2014 2016 2010 2012 2015 2017 2015
## [77195] 2009 2011 2008 2008 2008 2009 2012 2008 2015 2014 2017 2017 2007
## [77208] 2016 2016 2009 2008 2016 2009 2012 2011 2009 2008 2015 2015 2014
## [77221] 2016 2015 2016 2015 2009 2009 2010 2016 2016 2017 2007 2009 2008
## [77234] 2017 2008 2013 2007 2010 2010 2013 2016 2008 2013 2015 2016 2017
## [77247] 2017 2008 2016 2016 2017 2012 2016 2009 2007 2011 2017 2017 2016
## [77260] 2015 2017 2016 2017 2017 2015 2010 2016 2011 2012 2012 2011 2017
## [77273] 2015 2013 2013 2017 2014 2015 2007 2010 2017 2016 2017 2015 2016
## [77286] 2016 2015 2014 2015 2014 2016 2017 2012 2013 2015 2016 2010 2015
## [77299] 2017 2014 2010 2016 2007 2015 2017 2015 2017 2015 2008 2016 2017
## [77312] 2017 2009 2013 2007 2016 2011 2007 2015 2016 2016 2017 2016 2015
## [77325] 2016 2016 2017 2013 2016 2017 2017 2017 2016 2016 2015 2016 2015
## [77338] 2016 2016 2009 2015 2017 2010 2017 2013 2014 2011 2013 2016 2008
## [77351] 2016 2017 2015 2011 2013 2017 2011 2016 2015 2011 2015 2015 2010
## [77364] 2013 2016 2017 2016 2017 2017 2010 2007 2008 2012 2015 2011 2010
## [77377] 2012 2014 2016 2015 2011 2008 2014 2016 2016 2016 2015 2008 2016
## [77390] 2012 2015 2017 2017 2009 2012 2012 2014 2016 2015 2017 2017 2012
## [77403] 2008 2015 2016 2016 2014 2016 2012 2014 2017 2011 2011 2010 2011
## [77416] 2015 2017 2007 2015 2015 2016 2016 2017 2011 2012 2010 2016 2016
## [77429] 2008 2016 2016 2016 2017 2017 2017 2011 2015 2016 2016 2017 2017
## [77442] 2008 2015 2013 2015 2012 2014 2017 2015 2012 2012 2016 2017 2010
## [77455] 2013 2014 2013 2014 2016 2017 2015 2016 2011 2010 2015 2016 2016
## [77468] 2011 2016 2017 2012 2013 2017 2007 2007 2014 2010 2012 2015 2007
## [77481] 2016 2011 2015 2017 2009 2014 2016 2017 2007 2011 2016 2017 2008
## [77494] 2011 2008 2011 2011 2013 2015 2016 2008 2007 2010 2014 2015 2017
## [77507] 2008 2017 2016 2017 2011 2017 2008 2013 2015 2016 2017 2008 2017
## [77520] 2007 2009 2008 2008 2015 2017 2008 2015 2007 2009 2007 2015 2017
## [77533] 2010 2008 2012 2013 2015 2016 2011 2015 2015 2017 2017 2015 2015
## [77546] 2007 2008 2016 2016 2017 2007 2008 2015 2016 2009 2016 2015 2015
## [77559] 2015 2016 2008 2011 2015 2017 2017 2015 2016 2012 2015 2017 2008
## [77572] 2009 2016 2010 2016 2016 2017 2012 2016 2017 2008 2011 2016 2007
## [77585] 2017 2009 2017 2017 2011 2016 2017 2007 2013 2016 2008 2010 2016
## [77598] 2016 2017 2017 2017 2016 2017 2010 2010 2007 2017 2008 2014 2015
## [77611] 2016 2009 2017 2017 2008 2008 2009 2009 2013 2014 2016 2016 2009
## [77624] 2016 2017 2012 2007 2007 2016 2016 2017 2016 2017 2011 2016 2017
## [77637] 2017 2009 2014 2016 2011 2013 2014 2011 2015 2016 2016 2017 2015
## [77650] 2013 2009 2011 2016 2010 2009 2007 2013 2015 2009 2016 2016 2009
## [77663] 2007 2017 2017 2015 2016 2017 2008 2009 2015 2017 2015 2017 2010
## [77676] 2008 2016 2010 2011 2015 2016 2010 2013 2009 2016 2016 2017 2015
## [77689] 2015 2017 2017 2016 2017 2007 2010 2016 2010 2015 2016 2017 2016
## [77702] 2010 2011 2010 2015 2016 2015 2017 2012 2016 2017 2012 2012 2014
## [77715] 2016 2008 2009 2016 2014 2015 2016 2016 2016 2017 2017 2017 2011
## [77728] 2008 2016 2017 2017 2013 2015 2017 2017 2015 2017 2012 2008 2011
## [77741] 2015 2016 2017 2016 2012 2010 2016 2010 2017 2010 2012 2010 2017
## [77754] 2017 2015 2011 2007 2015 2007 2010 2016 2016 2015 2016 2017 2011
## [77767] 2014 2007 2008 2016 2013 2015 2015 2015 2016 2016 2017 2011 2013
## [77780] 2016 2016 2008 2007 2016 2016 2011 2017 2010 2009 2016 2016 2017
## [77793] 2017 2008 2012 2017 2017 2017 2012 2017 2017 2008 2015 2013 2008
## [77806] 2013 2014 2015 2016 2012 2008 2016 2016 2007 2013 2010 2010 2016
## [77819] 2017 2008 2015 2016 2007 2011 2017 2007 2009 2012 2009 2011 2016
## [77832] 2013 2017 2016 2007 2010 2007 2015 2016 2016 2009 2016 2009 2010
## [77845] 2008 2009 2010 2007 2013 2017 2013 2015 2011 2013 2013 2015 2016
## [77858] 2015 2015 2016 2017 2015 2017 2008 2009 2015 2017 2017 2012 2013
## [77871] 2014 2016 2017 2015 2016 2017 2017 2014 2016 2016 2015 2008 2009
## [77884] 2009 2013 2016 2016 2016 2017 2015 2017 2010 2009 2015 2016 2017
## [77897] 2015 2010 2010 2013 2017 2011 2007 2017 2009 2013 2014 2016 2014
## [77910] 2016 2016 2010 2009 2017 2013 2013 2012 2010 2016 2010 2009 2015
## [77923] 2014 2015 2016 2015 2017 2011 2007 2016 2012 2015 2015 2016 2010
## [77936] 2010 2013 2015 2017 2010 2010 2015 2016 2017 2008 2007 2016 2011
## [77949] 2016 2009 2013 2016 2017 2017 2014 2009 2016 2007 2014 2015 2016
## [77962] 2016 2017 2009 2007 2010 2010 2014 2014 2008 2012 2016 2016 2017
## [77975] 2009 2015 2017 2017 2007 2012 2009 2015 2011 2017 2007 2014 2008
## [77988] 2013 2015 2017 2007 2007 2011 2014 2016 2017 2015 2017 2011 2007
## [78001] 2013 2008 2008 2015 2014 2010 2013 2010 2015 2009 2007 2017 2009
## [78014] 2009 2014 2009 2013 2016 2015 2017 2016 2016 2017 2015 2015 2015
## [78027] 2017 2008 2015 2016 2016 2017 2007 2010 2017 2010 2011 2013 2016
## [78040] 2011 2012 2010 2013 2017 2017 2008 2013 2017 2017 2012 2017 2017
## [78053] 2017 2017 2009 2017 2008 2016 2016 2008 2007 2015 2016 2007 2016
## [78066] 2017 2017 2017 2015 2011 2008 2015 2016 2017 2016 2017 2016 2017
## [78079] 2017 2007 2015 2015 2017 2008 2015 2015 2016 2013 2017 2010 2007
## [78092] 2009 2009 2015 2008 2013 2016 2016 2007 2011 2015 2016 2016 2015
## [78105] 2017 2009 2010 2014 2017 2014 2015 2016 2009 2016 2017 2013 2016
## [78118] 2017 2017 2017 2011 2011 2011 2017 2007 2007 2016 2017 2007 2007
## [78131] 2016 2017 2013 2016 2012 2013 2015 2017 2008 2017 2017 2014 2016
## [78144] 2015 2016 2012 2014 2015 2016 2016 2016 2007 2016 2017 2017 2017
## [78157] 2015 2016 2017 2009 2010 2015 2012 2007 2017 2010 2012 2016 2017
## [78170] 2010 2011 2013 2015 2009 2016 2016 2016 2017 2017 2017 2012 2009
## [78183] 2015 2012 2013 2014 2015 2010 2011 2013 2017 2016 2017 2015 2016
## [78196] 2016 2016 2007 2013 2016 2017 2017 2010 2008 2016 2015 2017 2012
## [78209] 2014 2015 2017 2010 2016 2010 2017 2011 2014 2017 2013 2017 2010
## [78222] 2016 2015 2016 2017 2011 2010 2009 2015 2012 2012 2010 2015 2009
## [78235] 2014 2016 2012 2013 2015 2017 2008 2015 2010 2016 2015 2016 2017
## [78248] 2017 2017 2011 2014 2015 2015 2015 2017 2008 2012 2014 2015 2016
## [78261] 2016 2013 2007 2012 2012 2014 2014 2016 2017 2007 2007 2009 2013
## [78274] 2015 2016 2016 2016 2014 2015 2015 2007 2013 2007 2010 2015 2015
## [78287] 2017 2007 2015 2016 2017 2012 2007 2015 2017 2015 2017 2010 2016
## [78300] 2017 2009 2017 2013 2014 2016 2016 2017 2011 2017 2012 2016 2007
## [78313] 2015 2016 2017 2016 2017 2016 2008 2013 2016 2016 2017 2009 2015
## [78326] 2017 2008 2015 2016 2014 2014 2015 2016 2007 2015 2015 2017 2017
## [78339] 2017 2016 2009 2008 2014 2016 2016 2017 2017 2008 2014 2015 2015
## [78352] 2016 2009 2016 2016 2017 2007 2008 2012 2008 2014 2017 2017 2011
## [78365] 2008 2009 2015 2016 2015 2015 2017 2017 2013 2017 2014 2015 2016
## [78378] 2016 2012 2007 2010 2015 2015 2015 2016 2017 2009 2012 2013 2007
## [78391] 2016 2017 2016 2008 2015 2017 2009 2017 2009 2007 2008 2014 2015
## [78404] 2017 2009 2016 2017 2008 2012 2014 2015 2016 2012 2010 2016 2009
## [78417] 2014 2014 2016 2010 2015 2015 2007 2013 2017 2015 2011 2012 2010
## [78430] 2015 2017 2015 2013 2015 2015 2015 2017 2012 2015 2017 2008 2013
## [78443] 2014 2009 2008 2016 2008 2014 2015 2017 2017 2017 2017 2017 2012
## [78456] 2013 2016 2009 2011 2015 2017 2017 2007 2007 2009 2016 2017 2016
## [78469] 2017 2012 2016 2012 2017 2009 2014 2016 2017 2012 2013 2015 2010
## [78482] 2009 2015 2015 2015 2015 2015 2011 2012 2013 2015 2010 2016 2014
## [78495] 2015 2008 2015 2009 2011 2014 2016 2017 2009 2008 2016 2007 2013
## [78508] 2016 2017 2017 2008 2011 2015 2017 2012 2014 2015 2017 2016 2017
## [78521] 2017 2010 2008 2017 2015 2007 2012 2008 2007 2014 2016 2010 2008
## [78534] 2015 2017 2007 2016 2012 2016 2016 2015 2012 2016 2017 2015 2015
## [78547] 2016 2015 2017 2014 2015 2016 2017 2017 2014 2008 2009 2013 2014
## [78560] 2010 2016 2008 2017 2016 2016 2015 2016 2013 2016 2011 2007 2016
## [78573] 2017 2017 2016 2016 2013 2014 2009 2008 2009 2009 2013 2016 2016
## [78586] 2016 2017 2017 2007 2017 2015 2015 2008 2014 2017 2013 2007 2015
## [78599] 2014 2008 2015 2011 2013 2009 2015 2016 2008 2015 2017 2010 2017
## [78612] 2010 2017 2015 2008 2017 2010 2011 2015 2016 2008 2009 2014 2016
## [78625] 2009 2016 2017 2014 2014 2015 2016 2016 2015 2016 2011 2016 2017
## [78638] 2009 2015 2016 2017 2016 2016 2012 2008 2017 2017 2016 2017 2016
## [78651] 2015 2017 2017 2012 2016 2015 2016 2007 2009 2017 2011 2015 2016
## [78664] 2017 2009 2016 2017 2009 2014 2017 2017 2008 2011 2014 2017 2009
## [78677] 2017 2011 2016 2016 2017 2009 2016 2013 2009 2016 2015 2012 2011
## [78690] 2015 2016 2015 2017 2015 2015 2016 2010 2015 2015 2017 2007 2008
## [78703] 2012 2015 2009 2011 2014 2015 2009 2013 2017 2016 2011 2008 2016
## [78716] 2016 2017 2015 2017 2015 2016 2015 2012 2012 2007 2010 2017 2007
## [78729] 2012 2014 2008 2014 2016 2009 2015 2007 2008 2007 2015 2009 2010
## [78742] 2017 2017 2009 2015 2014 2016 2016 2009 2008 2017 2017 2017 2017
## [78755] 2017 2008 2015 2016 2017 2007 2007 2008 2015 2017 2011 2015 2016
## [78768] 2017 2012 2009 2014 2015 2015 2017 2017 2017 2007 2007 2010 2015
## [78781] 2015 2017 2011 2015 2017 2017 2016 2016 2015 2015 2008 2008 2011
## [78794] 2007 2010 2007 2013 2015 2016 2015 2016 2017 2012 2008 2011 2010
## [78807] 2015 2015 2008 2012 2012 2016 2015 2016 2017 2012 2013 2013 2013
## [78820] 2014 2015 2016 2017 2016 2017 2014 2014 2016 2010 2010 2016 2015
## [78833] 2017 2011 2008 2015 2016 2015 2017 2017 2017 2012 2012 2012 2015
## [78846] 2010 2017 2011 2007 2015 2010 2009 2010 2009 2014 2017 2017 2017
## [78859] 2011 2011 2007 2015 2017 2012 2013 2016 2010 2007 2013 2011 2016
## [78872] 2016 2008 2015 2016 2016 2017 2012 2015 2016 2016 2015 2015 2011
## [78885] 2009 2015 2015 2015 2016 2015 2010 2015 2017 2008 2016 2017 2013
## [78898] 2015 2011 2015 2017 2008 2017 2011 2015 2016 2011 2013 2014 2015
## [78911] 2015 2017 2013 2016 2017 2010 2012 2013 2017 2008 2016 2016 2014
## [78924] 2017 2010 2017 2016 2007 2013 2009 2013 2015 2016 2016 2016 2017
## [78937] 2009 2012 2017 2017 2008 2016 2015 2016 2016 2017 2011 2014 2016
## [78950] 2017 2013 2017 2016 2008 2016 2017 2011 2016 2015 2015 2008 2014
## [78963] 2015 2017 2017 2007 2012 2009 2013 2017 2015 2016 2017 2015 2016
## [78976] 2016 2008 2014 2016 2015 2014 2015 2007 2014 2015 2017 2013 2016
## [78989] 2015 2016 2017 2016 2017 2017 2013 2015 2015 2016 2017 2007 2007
## [79002] 2016 2015 2016 2017 2009 2007 2015 2016 2017 2007 2015 2016 2011
## [79015] 2015 2016 2017 2011 2008 2017 2008 2015 2015 2017 2017 2017 2010
## [79028] 2016 2016 2016 2012 2014 2015 2011 2015 2015 2012 2016 2017 2017
## [79041] 2008 2016 2017 2017 2017 2008 2013 2015 2017 2015 2017 2008 2016
## [79054] 2015 2015 2015 2015 2016 2010 2015 2013 2016 2015 2009 2013 2014
## [79067] 2013 2015 2011 2008 2017 2007 2009 2013 2015 2015 2016 2008 2017
## [79080] 2017 2015 2015 2016 2010 2016 2010 2015 2011 2015 2017 2016 2016
## [79093] 2017 2010 2007 2012 2008 2017 2017 2011 2015 2017 2015 2015 2016
## [79106] 2010 2008 2012 2016 2014 2015 2016 2015 2017 2012 2016 2013 2016
## [79119] 2017 2012 2017 2016 2009 2009 2015 2016 2016 2007 2015 2017 2009
## [79132] 2012 2016 2017 2007 2015 2008 2015 2017 2017 2016 2014 2016 2008
## [79145] 2017 2011 2016 2017 2017 2017 2017 2015 2017 2009 2015 2015 2016
## [79158] 2015 2015 2016 2008 2013 2017 2017 2009 2010 2015 2016 2016 2016
## [79171] 2016 2013 2016 2008 2016 2017 2007 2007 2011 2017 2007 2013 2014
## [79184] 2009 2013 2015 2015 2015 2015 2017 2015 2017 2017 2015 2016 2010
## [79197] 2014 2007 2009 2007 2009 2012 2009 2013 2015 2017 2011 2008 2015
## [79210] 2017 2017 2012 2016 2017 2016 2017 2008 2011 2011 2013 2017 2017
## [79223] 2011 2010 2016 2016 2016 2009 2017 2009 2010 2015 2016 2009 2009
## [79236] 2009 2014 2014 2017 2016 2015 2014 2015 2015 2008 2010 2011 2012
## [79249] 2017 2012 2015 2017 2014 2016 2014 2015 2017 2017 2017 2010 2014
## [79262] 2016 2016 2015 2016 2011 2010 2016 2017 2008 2011 2016 2016 2015
## [79275] 2010 2016 2017 2008 2012 2013 2016 2016 2015 2017 2017 2010 2015
## [79288] 2016 2017 2015 2015 2017 2017 2007 2014 2015 2011 2011 2010 2015
## [79301] 2016 2008 2011 2017 2008 2011 2013 2015 2015 2015 2008 2009 2012
## [79314] 2009 2017 2017 2015 2017 2017 2015 2016 2007 2015 2017 2017 2009
## [79327] 2016 2010 2015 2016 2009 2014 2017 2011 2007 2016 2009 2011 2016
## [79340] 2016 2008 2008 2013 2015 2015 2017 2017 2012 2013 2013 2015 2017
## [79353] 2017 2016 2017 2017 2017 2012 2007 2013 2015 2016 2009 2014 2015
## [79366] 2016 2017 2012 2009 2009 2015 2007 2012 2015 2015 2016 2015 2017
## [79379] 2010 2008 2016 2016 2016 2016 2015 2009 2011 2014 2016 2015 2017
## [79392] 2017 2012 2014 2014 2014 2017 2007 2016 2012 2017 2010 2013 2016
## [79405] 2016 2017 2008 2007 2009 2014 2015 2017 2011 2015 2010 2014 2017
## [79418] 2008 2007 2015 2015 2017 2009 2012 2016 2008 2016 2010 2015 2015
## [79431] 2010 2009 2016 2016 2015 2015 2009 2016 2016 2016 2008 2017 2015
## [79444] 2016 2015 2010 2015 2012 2016 2015 2009 2016 2017 2008 2015 2017
## [79457] 2013 2017 2014 2015 2017 2017 2010 2017 2010 2010 2011 2007 2015
## [79470] 2017 2015 2010 2015 2010 2016 2017 2015 2015 2007 2016 2016 2016
## [79483] 2008 2010 2014 2014 2015 2016 2007 2012 2008 2010 2013 2015 2017
## [79496] 2009 2016 2017 2013 2016 2016 2010 2014 2016 2009 2009 2010 2009
## [79509] 2009 2015 2016 2017 2016 2016 2016 2007 2016 2017 2011 2016 2009
## [79522] 2008 2012 2015 2007 2017 2013 2017 2017 2011 2010 2010 2015 2009
## [79535] 2008 2012 2008 2013 2013 2015 2017 2014 2016 2016 2017 2016 2015
## [79548] 2017 2016 2015 2017 2015 2014 2016 2016 2016 2010 2017 2012 2015
## [79561] 2016 2010 2007 2016 2017 2007 2014 2016 2017 2008 2009 2009 2016
## [79574] 2016 2010 2009 2007 2017 2016 2017 2012 2013 2015 2016 2017 2014
## [79587] 2016 2017 2009 2016 2008 2012 2013 2015 2016 2016 2017 2017 2016
## [79600] 2016 2017 2012 2011 2016 2008 2007 2017 2016 2008 2017 2017 2013
## [79613] 2016 2011 2009 2012 2015 2015 2017 2017 2007 2011 2016 2017 2016
## [79626] 2017 2015 2014 2016 2016 2016 2017 2016 2016 2011 2014 2016 2010
## [79639] 2011 2007 2015 2010 2014 2015 2016 2017 2008 2012 2013 2016 2017
## [79652] 2013 2015 2016 2017 2017 2009 2009 2007 2012 2015 2015 2016 2008
## [79665] 2009 2014 2016 2016 2017 2017 2012 2012 2012 2010 2007 2016 2015
## [79678] 2016 2017 2007 2015 2016 2015 2011 2016 2007 2008 2016 2015 2016
## [79691] 2017 2016 2017 2008 2015 2017 2013 2012 2012 2015 2016 2009 2015
## [79704] 2017 2015 2008 2016 2017 2016 2017 2007 2013 2008 2007 2010 2015
## [79717] 2016 2015 2008 2009 2014 2014 2016 2011 2010 2010 2015 2016 2009
## [79730] 2007 2016 2017 2017 2010 2017 2010 2007 2014 2016 2008 2017 2011
## [79743] 2013 2014 2017 2008 2012 2017 2008 2015 2015 2016 2007 2014 2016
## [79756] 2007 2012 2015 2016 2015 2015 2012 2012 2017 2017 2009 2015 2016
## [79769] 2015 2014 2015 2014 2010 2016 2017 2015 2016 2017 2014 2017 2015
## [79782] 2016 2015 2010 2016 2008 2013 2007 2011 2017 2011 2016 2017 2017
## [79795] 2016 2017 2011 2016 2007 2015 2016 2016 2017 2015 2016 2017 2017
## [79808] 2008 2016 2017 2013 2016 2010 2015 2016 2017 2017 2015 2017 2009
## [79821] 2008 2008 2015 2016 2017 2016 2010 2016 2008 2014 2017 2011 2010
## [79834] 2012 2007 2015 2015 2017 2009 2017 2011 2016 2017 2009 2015 2016
## [79847] 2013 2015 2016 2015 2007 2009 2015 2017 2015 2009 2015 2015 2017
## [79860] 2017 2009 2014 2015 2010 2013 2016 2008 2007 2010 2017 2015 2015
## [79873] 2009 2008 2015 2011 2017 2010 2016 2017 2011 2007 2017 2017 2007
## [79886] 2013 2016 2017 2016 2012 2015 2016 2017 2007 2014 2017 2017 2007
## [79899] 2017 2012 2011 2017 2015 2010 2012 2017 2010 2011 2014 2015 2016
## [79912] 2007 2007 2013 2010 2015 2017 2009 2016 2016 2017 2015 2017 2008
## [79925] 2007 2015 2016 2016 2015 2007 2014 2015 2012 2016 2016 2016 2017
## [79938] 2017 2009 2007 2012 2011 2014 2015 2015 2017 2009 2012 2007 2015
## [79951] 2017 2012 2016 2013 2016 2017 2017 2012 2017 2014 2015 2015 2016
## [79964] 2016 2017 2017 2014 2014 2016 2017 2011 2015 2016 2016 2016 2015
## [79977] 2016 2017 2017 2009 2008 2013 2009 2014 2016 2017 2012 2013 2011
## [79990] 2010 2010 2016 2017 2014 2010 2007 2015 2016 2016 2014 2008 2010
## [80003] 2016 2009 2016 2017 2007 2015 2016 2016 2015 2014 2015 2009 2013
## [80016] 2015 2016 2016 2007 2011 2015 2013 2014 2015 2011 2013 2013 2015
## [80029] 2007 2011 2007 2016 2016 2011 2011 2015 2007 2008 2016 2017 2016
## [80042] 2016 2008 2013 2013 2015 2015 2010 2014 2013 2007 2014 2017 2017
## [80055] 2015 2016 2015 2017 2012 2012 2012 2011 2015 2017 2009 2016 2016
## [80068] 2017 2017 2017 2007 2009 2011 2009 2016 2016 2015 2017 2009 2011
## [80081] 2013 2016 2012 2013 2015 2017 2010 2009 2013 2012 2015 2015 2017
## [80094] 2011 2016 2012 2016 2017 2017 2009 2017 2017 2014 2015 2016 2016
## [80107] 2017 2017 2015 2017 2016 2017 2010 2015 2016 2008 2007 2009 2015
## [80120] 2015 2016 2009 2016 2008 2013 2016 2016 2015 2014 2015 2016 2016
## [80133] 2015 2015 2009 2016 2016 2016 2014 2015 2007 2008 2014 2014 2016
## [80146] 2016 2010 2015 2015 2015 2014 2015 2017 2010 2011 2016 2016 2007
## [80159] 2007 2016 2016 2017 2012 2017 2010 2010 2013 2015 2008 2015 2017
## [80172] 2007 2010 2017 2017 2012 2016 2015 2016 2017 2009 2010 2017 2017
## [80185] 2015 2016 2015 2015 2016 2016 2017 2008 2012 2011 2015 2009 2008
## [80198] 2008 2009 2007 2007 2015 2007 2007 2017 2017 2014 2015 2016 2016
## [80211] 2016 2008 2011 2015 2016 2017 2009 2008 2013 2015 2012 2014 2017
## [80224] 2009 2016 2016 2015 2009 2013 2016 2007 2012 2010 2016 2016 2016
## [80237] 2017 2011 2011 2007 2008 2014 2015 2015 2016 2013 2016 2010 2008
## [80250] 2016 2017 2012 2015 2015 2016 2012 2015 2016 2016 2015 2017 2011
## [80263] 2007 2011 2017 2015 2015 2016 2015 2016 2012 2014 2015 2008 2016
## [80276] 2017 2017 2011 2017 2009 2016 2017 2012 2008 2016 2017 2008 2016
## [80289] 2017 2015 2016 2008 2011 2016 2017 2008 2013 2016 2016 2011 2016
## [80302] 2015 2016 2016 2007 2015 2015 2017 2013 2015 2016 2016 2017 2010
## [80315] 2007 2010 2017 2011 2016 2008 2008 2015 2015 2016 2017 2015 2014
## [80328] 2016 2015 2012 2015 2011 2008 2010 2015 2016 2007 2016 2017 2010
## [80341] 2016 2016 2009 2014 2017 2017 2007 2014 2017 2011 2008 2010 2013
## [80354] 2011 2008 2013 2007 2009 2010 2011 2014 2017 2007 2016 2017 2012
## [80367] 2013 2014 2016 2017 2015 2007 2011 2009 2016 2016 2017 2014 2009
## [80380] 2011 2016 2016 2011 2008 2015 2016 2009 2016 2017 2015 2008 2012
## [80393] 2007 2016 2016 2007 2011 2007 2017 2017 2009 2008 2012 2007 2016
## [80406] 2010 2008 2016 2015 2010 2011 2009 2015 2017 2015 2017 2007 2017
## [80419] 2007 2015 2017 2009 2013 2016 2015 2015 2016 2008 2014 2015 2015
## [80432] 2017 2014 2016 2015 2015 2017 2012 2015 2016 2013 2016 2008 2009
## [80445] 2011 2016 2017 2017 2016 2017 2017 2015 2009 2010 2010 2008 2015
## [80458] 2017 2015 2017 2008 2014 2017 2007 2015 2009 2015 2009 2013 2010
## [80471] 2013 2014 2016 2017 2014 2014 2014 2016 2011 2007 2017 2017 2010
## [80484] 2016 2015 2008 2008 2016 2017 2009 2008 2017 2010 2011 2017 2014
## [80497] 2016 2017 2010 2008 2015 2008 2015 2017 2017 2008 2009 2017 2007
## [80510] 2010 2013 2007 2016 2017 2010 2009 2013 2015 2012 2007 2014 2016
## [80523] 2017 2011 2007 2015 2015 2008 2015 2011 2007 2015 2010 2011 2016
## [80536] 2011 2010 2014 2016 2017 2017 2014 2017 2009 2016 2015 2015 2016
## [80549] 2010 2017 2016 2017 2013 2015 2009 2017 2013 2016 2012 2016 2016
## [80562] 2007 2008 2010 2016 2012 2009 2017 2015 2011 2016 2014 2009 2016
## [80575] 2016 2017 2016 2009 2011 2013 2016 2017 2015 2016 2008 2015 2016
## [80588] 2016 2015 2012 2017 2017 2017 2008 2016 2013 2017 2010 2016 2007
## [80601] 2015 2009 2007 2007 2012 2011 2015 2009 2013 2016 2016 2015 2017
## [80614] 2009 2009 2016 2016 2015 2017 2014 2016 2015 2009 2017 2017 2009
## [80627] 2010 2015 2015 2016 2016 2016 2017 2017 2017 2009 2015 2016 2017
## [80640] 2016 2012 2016 2008 2009 2007 2015 2016 2015 2017 2017 2014 2015
## [80653] 2016 2017 2017 2012 2011 2010 2015 2007 2016 2017 2010 2009 2016
## [80666] 2015 2017 2016 2009 2016 2007 2017 2009 2008 2008 2016 2016 2017
## [80679] 2012 2013 2014 2017 2008 2011 2012 2017 2008 2015 2017 2012 2010
## [80692] 2015 2017 2017 2009 2015 2017 2013 2017 2017 2016 2010 2012 2016
## [80705] 2008 2010 2011 2011 2014 2016 2016 2017 2017 2015 2015 2017 2008
## [80718] 2016 2016 2016 2014 2016 2017 2009 2017 2017 2010 2017 2007 2009
## [80731] 2017 2017 2013 2015 2017 2017 2012 2008 2013 2014 2015 2015 2011
## [80744] 2015 2009 2013 2013 2015 2016 2013 2011 2016 2016 2017 2015 2017
## [80757] 2015 2016 2014 2016 2008 2014 2017 2015 2012 2007 2013 2011 2010
## [80770] 2010 2012 2016 2017 2017 2012 2009 2008 2017 2008 2009 2007 2009
## [80783] 2014 2015 2012 2016 2017 2017 2017 2011 2010 2013 2016 2007 2009
## [80796] 2016 2007 2014 2010 2015 2011 2017 2017 2009 2012 2015 2017 2008
## [80809] 2016 2017 2011 2011 2017 2017 2011 2008 2010 2010 2016 2016 2011
## [80822] 2015 2016 2011 2007 2012 2016 2017 2017 2007 2015 2016 2015 2015
## [80835] 2015 2017 2017 2007 2017 2011 2007 2015 2016 2017 2014 2015 2009
## [80848] 2014 2016 2016 2017 2009 2016 2017 2008 2007 2015 2009 2007 2015
## [80861] 2016 2017 2012 2016 2012 2016 2012 2007 2013 2013 2008 2008 2013
## [80874] 2009 2007 2013 2015 2015 2016 2016 2015 2008 2015 2010 2016 2015
## [80887] 2015 2017 2007 2014 2016 2017 2013 2016 2017 2017 2010 2010 2011
## [80900] 2014 2015 2011 2013 2015 2015 2017 2016 2016 2017 2007 2008 2007
## [80913] 2015 2009 2016 2017 2017 2012 2010 2008 2013 2014 2012 2011 2015
## [80926] 2010 2013 2016 2017 2016 2017 2017 2017 2016 2017 2016 2017 2015
## [80939] 2010 2015 2017 2009 2016 2007 2016 2016 2017 2017 2007 2014 2016
## [80952] 2011 2015 2015 2007 2014 2007 2008 2013 2011 2015 2016 2008 2015
## [80965] 2017 2008 2008 2011 2016 2016 2012 2011 2016 2008 2008 2013 2016
## [80978] 2017 2014 2015 2016 2016 2017 2009 2007 2015 2017 2012 2016 2012
## [80991] 2007 2013 2016 2010 2015 2017 2012 2012 2012 2012 2012 2011 2016
## [81004] 2016 2015 2015 2007 2015 2016 2017 2017 2012 2017 2015 2017 2008
## [81017] 2015 2017 2011 2015 2016 2016 2017 2014 2011 2010 2016 2016 2017
## [81030] 2017 2015 2017 2007 2012 2007 2008 2015 2015 2017 2007 2010 2007
## [81043] 2012 2009 2015 2012 2010 2012 2016 2017 2017 2007 2017 2013 2017
## [81056] 2007 2012 2017 2011 2016 2016 2016 2014 2015 2016 2017 2017 2012
## [81069] 2013 2016 2015 2016 2016 2017 2011 2013 2015 2016 2017 2008 2009
## [81082] 2010 2013 2016 2016 2017 2011 2017 2015 2009 2011 2010 2010 2015
## [81095] 2016 2017 2016 2017 2017 2016 2015 2017 2011 2017 2017 2014 2015
## [81108] 2016 2010 2009 2012 2015 2012 2008 2015 2015 2015 2015 2011 2016
## [81121] 2008 2016 2017 2017 2008 2015 2007 2007 2011 2010 2015 2016 2016
## [81134] 2014 2016 2016 2016 2008 2013 2016 2017 2008 2016 2017 2012 2010
## [81147] 2016 2016 2008 2011 2016 2017 2011 2015 2017 2017 2017 2009 2016
## [81160] 2009 2016 2010 2016 2017 2008 2016 2017 2017 2017 2017 2016 2017
## [81173] 2007 2012 2016 2016 2016 2008 2016 2014 2016 2010 2016 2016 2016
## [81186] 2016 2016 2017 2017 2007 2008 2016 2017 2012 2007 2013 2016 2015
## [81199] 2009 2009 2009 2009 2017 2015 2016 2007 2007 2013 2016 2015 2016
## [81212] 2017 2009 2012 2015 2017 2008 2010 2016 2016 2017 2008 2008 2014
## [81225] 2016 2011 2011 2017 2016 2011 2016 2015 2017 2012 2016 2015 2012
## [81238] 2015 2016 2017 2017 2016 2016 2007 2011 2013 2013 2017 2017 2017
## [81251] 2012 2007 2016 2017 2015 2012 2016 2017 2017 2014 2016 2017 2011
## [81264] 2015 2017 2014 2015 2012 2010 2008 2015 2015 2016 2012 2017 2014
## [81277] 2016 2011 2008 2007 2013 2016 2017 2009 2015 2017 2016 2008 2007
## [81290] 2008 2014 2010 2016 2009 2016 2015 2017 2009 2014 2016 2017 2011
## [81303] 2009 2016 2017 2017 2015 2008 2014 2016 2016 2014 2016 2017 2015
## [81316] 2015 2016 2016 2007 2015 2016 2016 2009 2017 2011 2016 2010 2016
## [81329] 2015 2016 2010 2009 2017 2017 2016 2009 2016 2015 2008 2017 2012
## [81342] 2009 2015 2016 2007 2015 2015 2015 2007 2013 2016 2016 2017 2015
## [81355] 2010 2016 2008 2008 2016 2017 2017 2012 2017 2008 2015 2016 2012
## [81368] 2014 2016 2011 2007 2015 2010 2015 2016 2011 2017 2016 2016 2007
## [81381] 2014 2016 2008 2015 2011 2016 2016 2017 2017 2015 2017 2012 2016
## [81394] 2015 2017 2008 2013 2016 2017 2017 2012 2013 2009 2017 2017 2015
## [81407] 2017 2010 2016 2017 2010 2010 2007 2016 2017 2009 2015 2016 2007
## [81420] 2009 2015 2016 2012 2016 2017 2016 2015 2016 2008 2008 2014 2015
## [81433] 2015 2016 2017 2015 2008 2009 2011 2011 2012 2015 2015 2017 2009
## [81446] 2010 2014 2016 2017 2014 2016 2017 2010 2008 2017 2011 2016 2013
## [81459] 2012 2007 2017 2017 2014 2015 2017 2017 2011 2011 2014 2015 2017
## [81472] 2012 2013 2008 2011 2008 2016 2017 2007 2012 2016 2017 2017 2017
## [81485] 2017 2008 2015 2008 2015 2014 2016 2011 2013 2017 2017 2008 2017
## [81498] 2008 2016 2017 2013 2013 2016 2017 2017 2017 2017 2017 2008 2015
## [81511] 2016 2017 2014 2008 2013 2015 2017 2015 2016 2016 2015 2017 2015
## [81524] 2015 2015 2016 2008 2015 2016 2012 2008 2007 2015 2017 2015 2016
## [81537] 2016 2017 2007 2016 2016 2017 2017 2009 2009 2014 2015 2015 2009
## [81550] 2008 2010 2009 2015 2016 2015 2017 2014 2016 2016 2015 2016 2017
## [81563] 2012 2016 2013 2008 2017 2010 2016 2015 2008 2011 2015 2016 2010
## [81576] 2014 2015 2016 2016 2012 2012 2016 2017 2014 2012 2015 2015 2017
## [81589] 2015 2016 2017 2007 2007 2007 2011 2007 2016 2011 2008 2016 2016
## [81602] 2016 2011 2015 2008 2017 2017 2016 2010 2017 2015 2008 2008 2015
## [81615] 2017 2008 2008 2016 2017 2011 2013 2016 2012 2011 2008 2015 2017
## [81628] 2008 2015 2014 2015 2009 2015 2016 2017 2015 2012 2015 2011 2016
## [81641] 2015 2009 2010 2016 2017 2008 2015 2015 2015 2016 2015 2016 2009
## [81654] 2016 2015 2017 2012 2007 2014 2010 2011 2010 2015 2016 2009 2007
## [81667] 2011 2015 2017 2017 2008 2010 2016 2016 2009 2014 2015 2016 2017
## [81680] 2012 2015 2009 2013 2009 2015 2009 2011 2014 2016 2016 2017 2009
## [81693] 2009 2008 2016 2016 2011 2010 2016 2008 2015 2010 2016 2015 2009
## [81706] 2009 2008 2015 2017 2017 2013 2015 2010 2012 2017 2011 2016 2015
## [81719] 2017 2012 2007 2012 2008 2014 2017 2007 2011 2010 2015 2016 2017
## [81732] 2007 2015 2016 2015 2008 2010 2010 2016 2008 2007 2010 2014 2013
## [81745] 2007 2015 2017 2015 2012 2013 2016 2012 2014 2015 2010 2015 2017
## [81758] 2017 2015 2008 2009 2016 2012 2017 2014 2010 2011 2014 2016 2012
## [81771] 2007 2016 2009 2015 2012 2017 2010 2012 2008 2008 2007 2014 2016
## [81784] 2015 2012 2015 2016 2017 2014 2016 2017 2017 2012 2007 2011 2013
## [81797] 2010 2008 2014 2016 2015 2016 2017 2012 2016 2016 2017 2014 2009
## [81810] 2012 2007 2008 2017 2009 2016 2009 2015 2015 2011 2011 2015 2016
## [81823] 2016 2010 2007 2013 2015 2009 2016 2016 2016 2017 2007 2012 2008
## [81836] 2015 2015 2017 2011 2014 2015 2015 2017 2008 2008 2008 2016 2011
## [81849] 2014 2015 2008 2013 2014 2016 2016 2017 2015 2015 2016 2017 2017
## [81862] 2011 2016 2017 2014 2015 2010 2007 2013 2014 2016 2017 2013 2015
## [81875] 2016 2017 2017 2009 2007 2010 2016 2017 2011 2017 2016 2009 2016
## [81888] 2010 2015 2011 2010 2015 2013 2017 2013 2017 2017 2011 2015 2010
## [81901] 2009 2015 2016 2012 2015 2011 2012 2017 2016 2015 2007 2011 2015
## [81914] 2016 2016 2017 2017 2012 2012 2015 2016 2015 2015 2016 2017 2013
## [81927] 2013 2015 2015 2016 2015 2017 2008 2009 2017 2013 2016 2017 2012
## [81940] 2010 2017 2016 2016 2017 2015 2015 2015 2012 2012 2015 2015 2017
## [81953] 2017 2010 2017 2015 2017 2013 2015 2017 2017 2016 2011 2011 2011
## [81966] 2010 2013 2013 2015 2015 2016 2017 2011 2017 2012 2012 2008 2015
## [81979] 2017 2007 2008 2015 2014 2012 2008 2012 2007 2016 2011 2014 2014
## [81992] 2010 2015 2016 2009 2011 2014 2016 2010 2010 2016 2016 2017 2013
## [82005] 2015 2015 2015 2007 2012 2015 2016 2017 2012 2013 2017 2010 2011
## [82018] 2011 2008 2012 2013 2015 2017 2013 2013 2009 2011 2015 2015 2016
## [82031] 2011 2010 2014 2008 2017 2011 2007 2010 2010 2016 2017 2015 2017
## [82044] 2015 2016 2017 2010 2012 2015 2015 2008 2016 2016 2011 2011 2008
## [82057] 2015 2015 2009 2017 2017 2011 2012 2016 2017 2016 2007 2012 2008
## [82070] 2016 2016 2014 2015 2016 2017 2017 2014 2008 2017 2017 2010 2009
## [82083] 2015 2017 2017 2015 2016 2016 2016 2008 2014 2014 2015 2008 2016
## [82096] 2016 2017 2017 2016 2015 2017 2017 2017 2016 2007 2012 2012 2007
## [82109] 2011 2007 2014 2015 2007 2012 2015 2015 2017 2007 2010 2014 2011
## [82122] 2010 2017 2011 2015 2013 2015 2015 2011 2008 2012 2015 2015 2016
## [82135] 2017 2011 2015 2013 2008 2015 2015 2015 2013 2016 2016 2015 2007
## [82148] 2010 2013 2015 2016 2017 2017 2010 2010 2009 2015 2015 2016 2013
## [82161] 2015 2017 2009 2011 2008 2015 2017 2010 2013 2015 2017 2015 2012
## [82174] 2009 2010 2017 2007 2014 2016 2016 2012 2015 2016 2017 2017 2012
## [82187] 2013 2016 2017 2017 2015 2016 2016 2008 2010 2007 2017 2017 2011
## [82200] 2010 2017 2007 2017 2017 2009 2015 2011 2013 2017 2007 2010 2011
## [82213] 2013 2014 2017 2016 2015 2011 2016 2011 2016 2013 2016 2017 2011
## [82226] 2017 2010 2016 2012 2016 2008 2013 2008 2011 2015 2017 2017 2017
## [82239] 2013 2016 2007 2016 2009 2011 2007 2015 2008 2017 2008 2016 2016
## [82252] 2016 2007 2015 2016 2016 2017 2009 2010 2016 2016 2013 2015 2016
## [82265] 2011 2010 2011 2010 2008 2016 2016 2016 2016 2015 2015 2011 2015
## [82278] 2015 2011 2010 2015 2016 2013 2015 2010 2014 2016 2007 2008 2013
## [82291] 2015 2011 2016 2008 2009 2015 2015 2016 2017 2008 2015 2015 2017
## [82304] 2013 2015 2012 2012 2008 2013 2014 2015 2016 2017 2015 2017 2016
## [82317] 2015 2008 2007 2012 2013 2016 2012 2010 2017 2017 2010 2014 2017
## [82330] 2014 2016 2015 2009 2007 2016 2011 2015 2017 2016 2016 2009 2016
## [82343] 2009 2015 2017 2015 2011 2008 2015 2017 2011 2010 2010 2011 2015
## [82356] 2017 2008 2016 2017 2017 2009 2013 2017 2013 2015 2017 2015 2017
## [82369] 2012 2015 2017 2009 2007 2012 2014 2016 2016 2011 2016 2016 2007
## [82382] 2012 2014 2008 2017 2008 2016 2009 2012 2013 2017 2007 2012 2010
## [82395] 2016 2016 2017 2015 2017 2017 2011 2010 2015 2016 2017 2017 2017
## [82408] 2016 2016 2017 2010 2014 2012 2014 2016 2017 2009 2010 2015 2007
## [82421] 2007 2011 2008 2016 2016 2007 2010 2007 2010 2015 2008 2009 2016
## [82434] 2016 2008 2011 2014 2016 2017 2009 2012 2007 2016 2015 2016 2015
## [82447] 2016 2017 2014 2015 2017 2015 2017 2007 2007 2013 2008 2008 2016
## [82460] 2014 2016 2010 2009 2013 2017 2017 2015 2011 2017 2007 2015 2015
## [82473] 2015 2017 2017 2012 2016 2008 2015 2017 2017 2010 2012 2009 2015
## [82486] 2015 2017 2015 2017 2007 2007 2016 2007 2014 2015 2015 2015 2009
## [82499] 2016 2017 2015 2008 2011 2010 2015 2015 2013 2015 2015 2007 2015
## [82512] 2016 2011 2012 2012 2008 2009 2009 2015 2017 2015 2015 2016 2016
## [82525] 2017 2012 2009 2012 2017 2010 2010 2012 2009 2016 2016 2015 2017
## [82538] 2012 2015 2015 2016 2017 2017 2017 2012 2012 2013 2007 2011 2015
## [82551] 2017 2011 2013 2015 2012 2017 2016 2011 2016 2012 2010 2015 2017
## [82564] 2010 2008 2012 2015 2017 2011 2007 2017 2017 2008 2008 2015 2016
## [82577] 2010 2015 2007 2015 2017 2015 2007 2012 2015 2016 2016 2016 2010
## [82590] 2013 2015 2017 2007 2009 2015 2016 2009 2008 2015 2015 2016 2010
## [82603] 2014 2016 2016 2008 2012 2008 2016 2017 2007 2016 2017 2007 2015
## [82616] 2016 2016 2017 2017 2012 2012 2009 2012 2012 2017 2017 2015 2015
## [82629] 2017 2012 2014 2017 2015 2008 2016 2016 2016 2017 2009 2015 2012
## [82642] 2015 2016 2017 2016 2017 2010 2010 2011 2013 2015 2017 2008 2017
## [82655] 2017 2014 2016 2011 2016 2015 2016 2017 2015 2010 2015 2017 2011
## [82668] 2009 2015 2011 2015 2016 2015 2015 2008 2008 2009 2014 2017 2017
## [82681] 2007 2015 2016 2017 2010 2012 2007 2017 2009 2008 2012 2017 2010
## [82694] 2015 2015 2016 2016 2017 2008 2015 2017 2014 2017 2014 2015 2010
## [82707] 2017 2017 2009 2009 2016 2008 2010 2010 2015 2015 2008 2012 2016
## [82720] 2016 2015 2017 2011 2012 2009 2011 2007 2017 2015 2010 2017 2014
## [82733] 2017 2016 2011 2009 2012 2015 2016 2010 2014 2016 2012 2012 2015
## [82746] 2015 2016 2016 2017 2009 2010 2012 2015 2017 2009 2007 2012 2009
## [82759] 2017 2015 2017 2017 2012 2011 2008 2009 2009 2015 2017 2014 2007
## [82772] 2016 2015 2016 2015 2008 2014 2016 2012 2009 2015 2007 2016 2017
## [82785] 2017 2008 2009 2009 2015 2015 2017 2015 2008 2014 2015 2017 2016
## [82798] 2015 2007 2011 2016 2016 2007 2011 2014 2016 2007 2009 2015 2016
## [82811] 2012 2015 2008 2011 2012 2011 2008 2017 2017 2015 2015 2016 2016
## [82824] 2008 2007 2008 2015 2014 2015 2011 2015 2016 2011 2016 2017 2011
## [82837] 2012 2016 2017 2012 2014 2015 2014 2016 2016 2015 2013 2010 2015
## [82850] 2011 2016 2016 2016 2017 2017 2017 2015 2016 2017 2012 2009 2017
## [82863] 2009 2012 2008 2012 2016 2016 2009 2014 2015 2017 2008 2015 2016
## [82876] 2017 2010 2016 2017 2017 2017 2016 2009 2015 2015 2016 2017 2008
## [82889] 2017 2007 2009 2013 2016 2017 2017 2009 2009 2014 2017 2015 2013
## [82902] 2017 2012 2009 2015 2014 2008 2007 2015 2017 2010 2015 2016 2014
## [82915] 2015 2007 2015 2016 2007 2011 2015 2017 2008 2016 2016 2016 2015
## [82928] 2007 2015 2016 2017 2014 2015 2016 2015 2017 2015 2015 2017 2016
## [82941] 2017 2009 2009 2015 2017 2007 2010 2013 2017 2009 2009 2011 2013
## [82954] 2016 2015 2015 2007 2011 2011 2016 2007 2009 2009 2008 2013 2016
## [82967] 2010 2007 2014 2016 2017 2017 2013 2016 2016 2009 2016 2015 2016
## [82980] 2008 2016 2016 2010 2012 2016 2017 2015 2017 2011 2007 2015 2017
## [82993] 2017 2010 2015 2010 2008 2007 2007 2009 2012 2014 2015 2016 2009
## [83006] 2013 2016 2016 2016 2016 2011 2007 2015 2017 2016 2015 2012 2015
## [83019] 2017 2017 2007 2014 2010 2016 2017 2009 2013 2017 2009 2016 2013
## [83032] 2014 2016 2015 2017 2016 2017 2013 2007 2009 2013 2010 2016 2007
## [83045] 2016 2012 2017 2008 2016 2017 2015 2009 2015 2016 2017 2009 2009
## [83058] 2016 2015 2017 2014 2015 2017 2015 2010 2009 2016 2015 2017 2013
## [83071] 2016 2017 2011 2014 2016 2011 2008 2008 2013 2016 2016 2016 2016
## [83084] 2017 2015 2016 2011 2015 2008 2010 2015 2015 2016 2017 2009 2017
## [83097] 2007 2014 2017 2015 2017 2008 2015 2008 2016 2008 2015 2016 2016
## [83110] 2016 2009 2008 2008 2007 2016 2016 2012 2007 2015 2017 2011 2010
## [83123] 2017 2011 2007 2014 2016 2017 2015 2016 2012 2014 2015 2015 2011
## [83136] 2007 2011 2007 2016 2015 2011 2011 2014 2016 2017 2015 2017 2011
## [83149] 2016 2008 2012 2015 2015 2017 2015 2016 2010 2015 2016 2017 2008
## [83162] 2009 2015 2017 2013 2017 2017 2011 2008 2012 2013 2016 2016 2017
## [83175] 2010 2015 2015 2016 2007 2016 2015 2016 2016 2016 2017 2010 2009
## [83188] 2012 2009 2011 2017 2016 2010 2015 2016 2016 2010 2016 2015 2016
## [83201] 2016 2015 2017 2017 2010 2011 2010 2007 2013 2015 2016 2009 2016
## [83214] 2014 2016 2014 2016 2016 2016 2007 2016 2017 2014 2013 2016 2016
## [83227] 2017 2011 2009 2012 2010 2016 2016 2015 2016 2010 2012 2017 2015
## [83240] 2017 2007 2017 2015 2017 2012 2016 2017 2012 2016 2008 2008 2011
## [83253] 2013 2016 2015 2016 2010 2015 2010 2008 2015 2016 2017 2011 2015
## [83266] 2015 2008 2011 2011 2013 2015 2015 2017 2017 2009 2014 2015 2016
## [83279] 2016 2015 2016 2017 2008 2017 2016 2008 2016 2017 2011 2017 2008
## [83292] 2008 2008 2010 2013 2016 2015 2016 2017 2015 2017 2007 2013 2015
## [83305] 2017 2016 2016 2009 2009 2015 2008 2007 2008 2016 2010 2008 2013
## [83318] 2016 2011 2011 2011 2009 2008 2011 2015 2015 2016 2016 2017 2011
## [83331] 2008 2015 2017 2012 2009 2008 2007 2011 2011 2016 2016 2016 2008
## [83344] 2008 2007 2015 2009 2010 2017 2017 2010 2011 2009 2010 2015 2016
## [83357] 2016 2016 2015 2009 2009 2015 2016 2016 2017 2009 2008 2010 2017
## [83370] 2017 2012 2011 2012 2017 2013 2015 2010 2017 2016 2017 2016 2016
## [83383] 2017 2014 2017 2012 2014 2016 2017 2015 2015 2016 2016 2009 2010
## [83396] 2010 2012 2008 2007 2016 2016 2016 2016 2017 2016 2012 2013 2015
## [83409] 2015 2017 2017 2011 2016 2014 2016 2017 2009 2017 2009 2009 2016
## [83422] 2016 2013 2017 2017 2015 2007 2011 2015 2016 2011 2016 2017 2017
## [83435] 2010 2015 2014 2015 2017 2016 2015 2016 2012 2008 2015 2017 2013
## [83448] 2015 2017 2007 2017 2008 2010 2016 2008 2011 2015 2016 2016 2010
## [83461] 2016 2016 2010 2008 2016 2013 2007 2015 2017 2007 2008 2009 2015
## [83474] 2017 2007 2017 2013 2017 2017 2012 2016 2016 2015 2016 2008 2010
## [83487] 2012 2013 2008 2013 2007 2017 2007 2009 2010 2015 2017 2009 2008
## [83500] 2011 2008 2014 2015 2008 2014 2015 2017 2017 2014 2011 2017 2011
## [83513] 2008 2015 2011 2016 2015 2016 2017 2011 2016 2015 2017 2016 2017
## [83526] 2007 2015 2016 2017 2017 2011 2009 2010 2015 2010 2015 2015 2013
## [83539] 2014 2015 2007 2015 2010 2016 2008 2015 2016 2017 2012 2011 2011
## [83552] 2016 2011 2007 2016 2013 2016 2017 2007 2009 2016 2008 2015 2015
## [83565] 2016 2012 2008 2007 2017 2007 2014 2015 2017 2012 2007 2014 2015
## [83578] 2017 2010 2017 2012 2010 2017 2014 2013 2017 2008 2010 2016 2016
## [83591] 2017 2015 2016 2010 2009 2009 2016 2012 2009 2016 2013 2016 2017
## [83604] 2011 2010 2015 2016 2014 2009 2013 2016 2009 2016 2013 2016 2009
## [83617] 2015 2010 2016 2015 2016 2017 2017 2013 2015 2015 2015 2017 2009
## [83630] 2014 2010 2015 2010 2012 2016 2016 2015 2016 2008 2016 2017 2011
## [83643] 2014 2015 2007 2016 2009 2007 2015 2008 2008 2016 2017 2017 2009
## [83656] 2013 2012 2015 2014 2015 2007 2013 2015 2017 2007 2017 2009 2009
## [83669] 2007 2008 2016 2015 2016 2015 2008 2016 2015 2010 2007 2016 2017
## [83682] 2013 2014 2007 2014 2015 2017 2017 2012 2015 2015 2015 2015 2011
## [83695] 2016 2015 2015 2017 2011 2007 2015 2011 2009 2013 2007 2015 2007
## [83708] 2010 2016 2016 2017 2011 2015 2007 2016 2011 2016 2016 2017 2015
## [83721] 2014 2017 2017 2016 2017 2017 2009 2013 2017 2011 2010 2007 2014
## [83734] 2016 2016 2008 2009 2017 2017 2012 2008 2017 2016 2016 2017 2008
## [83747] 2012 2015 2008 2015 2014 2016 2017 2016 2015 2017 2010 2016 2012
## [83760] 2011 2016 2008 2012 2009 2008 2015 2015 2010 2008 2016 2016 2016
## [83773] 2017 2010 2016 2007 2009 2012 2009 2013 2016 2017 2012 2013 2013
## [83786] 2015 2016 2017 2016 2009 2015 2009 2013 2015 2011 2016 2017 2008
## [83799] 2010 2016 2017 2012 2009 2008 2008 2017 2009 2016 2017 2008 2016
## [83812] 2010 2007 2015 2013 2017 2010 2008 2007 2012 2017 2017 2017 2016
## [83825] 2016 2016 2016 2011 2015 2016 2016 2017 2016 2017 2017 2017 2016
## [83838] 2016 2016 2016 2017 2016 2014 2015 2017 2017 2017 2012 2011 2016
## [83851] 2017 2012 2015 2010 2014 2016 2012 2017 2017 2013 2017 2017 2017
## [83864] 2016 2017 2015 2009 2007 2017 2017 2017 2008 2014 2014 2017 2017
## [83877] 2010 2007 2008 2014 2008 2016 2010 2015 2016 2008 2012 2011 2013
## [83890] 2016 2009 2008 2016 2015 2008 2016 2017 2007 2012 2009 2008 2013
## [83903] 2015 2016 2016 2017 2016 2012 2017 2013 2008 2007 2014 2014 2015
## [83916] 2008 2009 2015 2009 2016 2017 2007 2010 2016 2017 2017 2016 2016
## [83929] 2015 2016 2016 2007 2010 2016 2016 2010 2009 2015 2015 2007 2012
## [83942] 2013 2009 2015 2017 2007 2015 2009 2015 2017 2008 2013 2015 2007
## [83955] 2012 2009 2009 2011 2017 2011 2016 2017 2009 2007 2015 2016 2016
## [83968] 2017 2007 2016 2017 2011 2014 2016 2008 2017 2011 2016 2016 2016
## [83981] 2017 2010 2013 2016 2017 2015 2007 2014 2014 2012 2015 2016 2017
## [83994] 2010 2011 2016 2012 2010 2014 2015 2015 2011 2014 2015 2009 2007
## [84007] 2015 2012 2009 2009 2014 2016 2017 2016 2011 2012 2017 2015 2017
## [84020] 2017 2008 2010 2015 2016 2009 2015 2016 2013 2009 2010 2012 2007
## [84033] 2016 2010 2008 2015 2016 2016 2010 2015 2015 2011 2009 2014 2017
## [84046] 2015 2008 2012 2010 2013 2007 2017 2008 2012 2012 2015 2016 2016
## [84059] 2015 2007 2015 2011 2009 2016 2012 2017 2017 2010 2017 2010 2015
## [84072] 2013 2017 2012 2010 2007 2014 2015 2015 2017 2015 2017 2010 2010
## [84085] 2013 2015 2016 2007 2010 2007 2017 2011 2009 2015 2016 2016 2016
## [84098] 2017 2017 2007 2009 2007 2008 2016 2017 2016 2008 2015 2013 2016
## [84111] 2008 2009 2016 2016 2008 2012 2015 2015 2015 2015 2017 2017 2007
## [84124] 2008 2014 2017 2010 2011 2016 2016 2011 2015 2015 2017 2017 2010
## [84137] 2016 2016 2014 2016 2017 2008 2015 2015 2016 2016 2017 2007 2011
## [84150] 2012 2016 2015 2009 2010 2015 2016 2016 2017 2012 2017 2010 2017
## [84163] 2017 2013 2015 2015 2016 2016 2007 2016 2009 2012 2009 2015 2015
## [84176] 2017 2015 2012 2012 2008 2015 2017 2011 2015 2010 2013 2017 2014
## [84189] 2013 2016 2017 2011 2012 2016 2010 2007 2007 2008 2010 2016 2016
## [84202] 2007 2013 2015 2007 2017 2010 2016 2015 2010 2015 2015 2008 2016
## [84215] 2015 2016 2010 2016 2016 2017 2012 2015 2017 2016 2013 2016 2017
## [84228] 2010 2016 2016 2008 2015 2009 2015 2016 2017 2016 2017 2013 2010
## [84241] 2014 2015 2007 2013 2017 2017 2017 2009 2007 2015 2012 2016 2016
## [84254] 2015 2015 2015 2010 2015 2017 2017 2016 2015 2008 2010 2017 2017
## [84267] 2017 2011 2009 2008 2009 2011 2017 2017 2009 2013 2016 2016 2017
## [84280] 2015 2017 2015 2016 2017 2008 2012 2017 2012 2014 2010 2016 2016
## [84293] 2016 2017 2008 2016 2016 2008 2012 2017 2017 2009 2017 2011 2015
## [84306] 2017 2017 2007 2016 2014 2016 2017 2015 2015 2016 2014 2016 2017
## [84319] 2013 2017 2008 2009 2008 2013 2016 2007 2007 2012 2017 2017 2015
## [84332] 2017 2010 2016 2016 2016 2007 2012 2016 2017 2016 2017 2008 2016
## [84345] 2015 2015 2017 2017 2007 2016 2015 2009 2015 2007 2017 2011 2009
## [84358] 2009 2007 2015 2016 2008 2015 2017 2011 2015 2011 2017 2017 2010
## [84371] 2008 2007 2010 2015 2011 2015 2017 2012 2016 2017 2014 2015 2017
## [84384] 2009 2015 2010 2017 2008 2010 2010 2017 2007 2015 2017 2012 2014
## [84397] 2017 2017 2017 2011 2017 2015 2008 2011 2007 2008 2008 2009 2017
## [84410] 2010 2009 2016 2008 2007 2017 2017 2017 2017 2011 2010 2017 2013
## [84423] 2013 2009 2008 2014 2015 2016 2017 2016 2008 2014 2007 2016 2011
## [84436] 2015 2015 2016 2009 2016 2015 2015 2016 2017 2011 2016 2010 2013
## [84449] 2014 2015 2016 2016 2011 2009 2015 2016 2016 2017 2011 2011 2012
## [84462] 2014 2015 2015 2016 2015 2017 2009 2016 2011 2010 2008 2010 2008
## [84475] 2016 2016 2016 2015 2016 2016 2012 2010 2013 2016 2016 2012 2010
## [84488] 2010 2014 2015 2016 2016 2014 2016 2017 2017 2017 2016 2017 2012
## [84501] 2008 2012 2016 2009 2016 2012 2017 2007 2008 2014 2008 2017 2009
## [84514] 2008 2017 2017 2009 2008 2016 2016 2017 2008 2013 2013 2015 2016
## [84527] 2016 2009 2009 2017 2010 2008 2008 2015 2010 2009 2013 2016 2016
## [84540] 2015 2014 2017 2010 2015 2016 2017 2017 2013 2017 2015 2017 2010
## [84553] 2008 2013 2016 2016 2012 2014 2016 2016 2015 2008 2016 2017 2008
## [84566] 2016 2016 2016 2015 2009 2008 2009 2013 2011 2017 2012 2007 2011
## [84579] 2014 2012 2015 2017 2017 2016 2016 2010 2015 2007 2011 2013 2015
## [84592] 2015 2017 2007 2008 2011 2012 2012 2013 2015 2017 2010 2013 2015
## [84605] 2015 2015 2015 2016 2015 2017 2017 2009 2012 2014 2015 2016 2017
## [84618] 2011 2016 2016 2009 2015 2007 2013 2015 2016 2016 2011 2011 2015
## [84631] 2016 2017 2013 2008 2016 2015 2015 2015 2007 2016 2008 2007 2014
## [84644] 2016 2015 2016 2009 2012 2016 2017 2010 2015 2017 2015 2011 2016
## [84657] 2016 2017 2009 2010 2015 2017 2017 2017 2010 2017 2016 2009 2008
## [84670] 2014 2017 2009 2009 2017 2010 2012 2010 2012 2016 2008 2017 2007
## [84683] 2009 2010 2016 2016 2017 2012 2015 2015 2015 2017 2011 2016 2017
## [84696] 2011 2017 2017 2010 2015 2015 2017 2014 2012 2013 2013 2010 2012
## [84709] 2011 2008 2010 2014 2015 2015 2009 2015 2017 2012 2008 2014 2014
## [84722] 2016 2016 2017 2015 2016 2017 2012 2008 2012 2015 2017 2010 2012
## [84735] 2015 2017 2015 2011 2014 2007 2013 2015 2016 2016 2015 2017 2013
## [84748] 2015 2009 2016 2010 2015 2016 2007 2010 2011 2016 2016 2012 2011
## [84761] 2010 2010 2015 2012 2016 2017 2017 2007 2016 2009 2007 2013 2017
## [84774] 2013 2016 2016 2016 2007 2016 2017 2017 2015 2017 2009 2015 2008
## [84787] 2017 2017 2015 2011 2009 2015 2009 2017 2009 2009 2016 2016 2015
## [84800] 2010 2007 2014 2009 2014 2014 2015 2016 2016 2012 2013 2017 2016
## [84813] 2017 2012 2015 2010 2008 2017 2015 2017 2007 2017 2009 2008 2008
## [84826] 2011 2016 2009 2009 2013 2014 2009 2017 2017 2007 2014 2014 2016
## [84839] 2015 2016 2017 2011 2011 2017 2015 2015 2016 2016 2016 2009 2015
## [84852] 2017 2007 2015 2016 2010 2017 2016 2016 2017 2016 2017 2008 2013
## [84865] 2014 2016 2015 2016 2012 2013 2015 2015 2017 2015 2016 2016 2017
## [84878] 2009 2016 2017 2016 2017 2014 2016 2017 2017 2015 2015 2017 2011
## [84891] 2015 2016 2017 2017 2009 2009 2010 2008 2017 2017 2017 2011 2007
## [84904] 2010 2015 2017 2017 2008 2014 2015 2010 2015 2015 2010 2016 2016
## [84917] 2017 2016 2012 2011 2015 2016 2015 2015 2017 2017 2008 2015 2011
## [84930] 2008 2016 2017 2008 2017 2016 2016 2016 2015 2017 2017 2015 2016
## [84943] 2017 2014 2017 2017 2017 2010 2009 2010 2014 2009 2007 2017 2017
## [84956] 2015 2016 2017 2015 2015 2017 2017 2017 2010 2007 2013 2017 2017
## [84969] 2011 2007 2007 2013 2013 2015 2016 2015 2017 2014 2016 2015 2015
## [84982] 2016 2016 2016 2008 2017 2017 2010 2010 2015 2014 2014 2017 2014
## [84995] 2017 2016 2016 2015 2017 2017 2012 2013 2016 2017 2010 2012 2016
## [85008] 2009 2007 2008 2011 2015 2008 2012 2015 2017 2009 2015 2015 2016
## [85021] 2016 2017 2013 2015 2010 2015 2017 2012 2008 2016 2015 2017 2012
## [85034] 2017 2007 2007 2015 2016 2010 2013 2014 2017 2017 2007 2015 2015
## [85047] 2016 2008 2011 2013 2017 2017 2014 2017 2017 2017 2008 2012 2013
## [85060] 2010 2012 2016 2017 2010 2014 2016 2016 2008 2015 2008 2011 2012
## [85073] 2013 2016 2016 2017 2008 2011 2011 2015 2007 2015 2017 2008 2007
## [85086] 2016 2009 2009 2009 2014 2015 2016 2016 2011 2008 2017 2009 2016
## [85099] 2011 2013 2007 2016 2015 2012 2016 2007 2011 2016 2015 2017 2007
## [85112] 2013 2016 2016 2017 2007 2015 2010 2012 2015 2015 2009 2011 2016
## [85125] 2017 2010 2011 2013 2016 2016 2007 2015 2014 2008 2009 2015 2017
## [85138] 2015 2016 2017 2011 2016 2012 2014 2016 2015 2008 2015 2009 2009
## [85151] 2015 2015 2016 2017 2016 2016 2017 2011 2009 2008 2014 2017 2007
## [85164] 2015 2017 2012 2009 2017 2017 2010 2015 2016 2017 2013 2014 2017
## [85177] 2015 2015 2016 2016 2017 2007 2012 2012 2015 2017 2017 2016 2016
## [85190] 2017 2012 2012 2013 2017 2017 2016 2015 2016 2017 2012 2013 2015
## [85203] 2015 2009 2011 2016 2016 2009 2013 2015 2009 2015 2017 2017 2011
## [85216] 2016 2015 2012 2008 2013 2015 2016 2017 2007 2012 2016 2012 2015
## [85229] 2015 2016 2010 2013 2015 2010 2008 2014 2016 2012 2009 2015 2016
## [85242] 2017 2014 2016 2016 2011 2013 2009 2009 2009 2008 2015 2016 2016
## [85255] 2017 2007 2010 2015 2010 2008 2016 2016 2017 2017 2008 2009 2015
## [85268] 2008 2010 2015 2016 2017 2017 2008 2014 2015 2016 2016 2017 2008
## [85281] 2016 2008 2016 2009 2012 2016 2016 2015 2015 2015 2015 2016 2017
## [85294] 2010 2013 2015 2017 2010 2009 2015 2016 2016 2017 2015 2016 2012
## [85307] 2015 2016 2016 2015 2015 2016 2011 2012 2011 2016 2016 2010 2008
## [85320] 2014 2015 2016 2016 2010 2010 2009 2015 2016 2007 2011 2011 2015
## [85333] 2017 2012 2016 2011 2016 2016 2017 2017 2016 2017 2015 2017 2014
## [85346] 2007 2017 2007 2011 2016 2017 2015 2011 2015 2016 2013 2015 2015
## [85359] 2007 2013 2017 2016 2007 2008 2008 2008 2017 2017 2012 2015 2016
## [85372] 2016 2016 2017 2007 2016 2017 2012 2017 2015 2016 2017 2013 2014
## [85385] 2015 2017 2016 2016 2015 2016 2012 2009 2010 2008 2016 2008 2016
## [85398] 2016 2017 2011 2016 2015 2009 2017 2008 2013 2017 2008 2007 2014
## [85411] 2016 2013 2015 2015 2015 2016 2017 2017 2017 2011 2008 2013 2015
## [85424] 2017 2015 2007 2015 2016 2016 2010 2015 2016 2016 2012 2009 2017
## [85437] 2011 2013 2015 2016 2011 2011 2016 2009 2016 2015 2016 2016 2017
## [85450] 2008 2015 2016 2017 2011 2012 2015 2015 2009 2014 2014 2017 2010
## [85463] 2007 2016 2017 2017 2011 2014 2017 2009 2011 2012 2014 2016 2014
## [85476] 2016 2007 2015 2016 2017 2017 2015 2015 2016 2016 2017 2017 2017
## [85489] 2010 2011 2007 2014 2012 2009 2008 2016 2012 2015 2016 2016 2016
## [85502] 2011 2009 2016 2014 2009 2014 2009 2009 2013 2009 2015 2015 2017
## [85515] 2009 2016 2009 2013 2014 2016 2016 2016 2009 2015 2016 2017 2015
## [85528] 2017 2009 2014 2016 2017 2017 2017 2010 2015 2017 2009 2015 2017
## [85541] 2010 2017 2011 2012 2009 2010 2014 2012 2011 2007 2016 2016 2017
## [85554] 2017 2008 2010 2010 2014 2016 2012 2017 2015 2008 2009 2015 2017
## [85567] 2015 2015 2017 2014 2014 2012 2013 2016 2016 2012 2016 2015 2010
## [85580] 2015 2016 2016 2010 2010 2017 2015 2017 2017 2015 2009 2016 2017
## [85593] 2010 2015 2017 2016 2012 2016 2016 2017 2017 2017 2017 2012 2009
## [85606] 2007 2013 2017 2014 2016 2015 2017 2017 2011 2011 2016 2016 2017
## [85619] 2016 2011 2017 2009 2011 2008 2015 2007 2011 2007 2017 2007 2015
## [85632] 2015 2016 2017 2015 2017 2010 2009 2016 2016 2014 2014 2011 2015
## [85645] 2015 2015 2016 2016 2007 2015 2016 2017 2017 2009 2015 2015 2017
## [85658] 2010 2015 2015 2008 2009 2017 2017 2015 2015 2016 2011 2016 2015
## [85671] 2017 2017 2007 2017 2007 2013 2016 2017 2008 2014 2014 2016 2008
## [85684] 2015 2010 2011 2016 2012 2013 2016 2010 2016 2016 2016 2017 2017
## [85697] 2017 2016 2017 2013 2015 2017 2012 2012 2008 2015 2015 2016 2016
## [85710] 2009 2009 2015 2016 2016 2016 2017 2007 2013 2013 2015 2016 2016
## [85723] 2017 2017 2014 2014 2016 2017 2008 2016 2016 2016 2017 2017 2015
## [85736] 2017 2014 2015 2014 2015 2012 2013 2013 2014 2015 2011 2016 2017
## [85749] 2012 2009 2016 2012 2010 2016 2017 2010 2008 2016 2016 2007 2011
## [85762] 2017 2010 2013 2016 2017 2015 2017 2015 2009 2011 2015 2017 2008
## [85775] 2007 2015 2017 2017 2007 2008 2016 2016 2016 2009 2009 2008 2014
## [85788] 2017 2012 2011 2014 2017 2010 2015 2017 2008 2009 2007 2013 2015
## [85801] 2016 2016 2016 2016 2017 2012 2015 2016 2017 2009 2016 2014 2017
## [85814] 2017 2012 2014 2013 2015 2016 2015 2012 2014 2009 2011 2007 2010
## [85827] 2015 2012 2015 2015 2011 2008 2013 2007 2014 2016 2016 2011 2017
## [85840] 2014 2014 2015 2016 2015 2015 2011 2009 2012 2017 2017 2017 2017
## [85853] 2012 2016 2016 2007 2010 2013 2015 2015 2009 2009 2008 2008 2016
## [85866] 2016 2016 2016 2017 2015 2015 2016 2016 2015 2017 2008 2007 2011
## [85879] 2009 2015 2017 2009 2012 2015 2017 2017 2017 2016 2016 2007 2010
## [85892] 2016 2010 2016 2015 2017 2007 2009 2013 2016 2011 2016 2011 2011
## [85905] 2008 2015 2015 2015 2015 2016 2017 2012 2010 2009 2009 2017 2009
## [85918] 2014 2016 2016 2017 2014 2014 2015 2015 2015 2009 2008 2014 2016
## [85931] 2017 2017 2008 2012 2009 2017 2015 2009 2016 2012 2009 2014 2015
## [85944] 2017 2009 2016 2008 2015 2009 2008 2015 2015 2011 2016 2017 2011
## [85957] 2017 2009 2013 2016 2016 2015 2015 2015 2015 2008 2013 2017 2007
## [85970] 2016 2017 2017 2008 2009 2016 2010 2016 2017 2008 2008 2017 2017
## [85983] 2017 2012 2008 2015 2017 2016 2010 2016 2016 2015 2014 2016 2011
## [85996] 2007 2008 2013 2014 2017 2007 2013 2015 2016 2017 2017 2017 2013
## [86009] 2015 2017 2010 2013 2014 2017 2012 2016 2012 2009 2011 2009 2017
## [86022] 2010 2011 2014 2016 2011 2009 2014 2016 2015 2016 2017 2015 2017
## [86035] 2009 2009 2015 2011 2013 2016 2008 2010 2014 2017 2015 2015 2016
## [86048] 2016 2009 2013 2015 2014 2011 2012 2008 2017 2017 2015 2015 2011
## [86061] 2017 2007 2014 2015 2008 2014 2015 2016 2011 2016 2017 2017 2014
## [86074] 2017 2017 2015 2011 2008 2009 2007 2009 2016 2016 2010 2010 2008
## [86087] 2012 2015 2016 2011 2016 2008 2009 2013 2017 2016 2017 2008 2016
## [86100] 2015 2017 2016 2010 2009 2014 2015 2016 2008 2008 2015 2015 2015
## [86113] 2016 2017 2011 2014 2016 2015 2014 2014 2015 2016 2008 2007 2010
## [86126] 2015 2013 2016 2015 2008 2007 2016 2017 2010 2010 2017 2016 2017
## [86139] 2010 2016 2011 2015 2009 2014 2015 2016 2008 2015 2017 2015 2017
## [86152] 2011 2008 2016 2008 2017 2017 2017 2008 2013 2011 2015 2010 2012
## [86165] 2014 2015 2008 2015 2015 2017 2017 2017 2017 2015 2015 2007 2008
## [86178] 2015 2016 2017 2010 2014 2016 2017 2009 2008 2016 2012 2012 2015
## [86191] 2017 2008 2015 2017 2013 2017 2017 2008 2009 2008 2016 2017 2008
## [86204] 2012 2008 2016 2007 2015 2010 2008 2014 2015 2009 2008 2010 2017
## [86217] 2017 2012 2015 2016 2011 2016 2017 2015 2011 2017 2017 2017 2009
## [86230] 2014 2017 2016 2009 2010 2012 2015 2017 2017 2016 2009 2007 2016
## [86243] 2017 2017 2015 2017 2010 2017 2017 2008 2009 2015 2007 2013 2017
## [86256] 2012 2016 2017 2009 2007 2016 2017 2016 2008 2007 2017 2017 2012
## [86269] 2013 2016 2008 2008 2015 2011 2016 2016 2008 2011 2009 2007 2017
## [86282] 2015 2017 2011 2011 2010 2017 2008 2007 2013 2015 2016 2007 2010
## [86295] 2008 2015 2015 2015 2015 2016 2007 2008 2008 2015 2010 2012 2015
## [86308] 2017 2017 2012 2015 2017 2012 2008 2013 2008 2015 2017 2009 2012
## [86321] 2017 2008 2010 2014 2017 2017 2014 2015 2015 2015 2016 2013 2014
## [86334] 2016 2008 2015 2016 2016 2011 2007 2015 2016 2015 2017 2016 2014
## [86347] 2016 2015 2007 2007 2016 2009 2016 2015 2012 2009 2016 2017 2014
## [86360] 2015 2015 2011 2015 2015 2017 2017 2016 2009 2017 2008 2015 2016
## [86373] 2017 2015 2010 2016 2011 2016 2017 2014 2011 2013 2016 2017 2017
## [86386] 2007 2012 2017 2017 2008 2014 2015 2013 2015 2017 2012 2016 2008
## [86399] 2013 2014 2014 2015 2016 2009 2012 2010 2016 2008 2016 2009 2016
## [86412] 2017 2009 2015 2016 2012 2015 2016 2011 2009 2016 2016 2012 2016
## [86425] 2017 2010 2013 2009 2008 2016 2016 2017 2012 2012 2017 2017 2017
## [86438] 2008 2014 2016 2017 2012 2016 2009 2015 2015 2015 2008 2015 2017
## [86451] 2015 2017 2012 2015 2010 2016 2007 2012 2013 2017 2007 2010 2010
## [86464] 2015 2016 2016 2016 2016 2011 2015 2015 2017 2009 2015 2017 2011
## [86477] 2011 2017 2011 2017 2017 2011 2014 2015 2017 2016 2016 2017 2007
## [86490] 2013 2016 2015 2015 2017 2015 2009 2014 2015 2016 2007 2007 2017
## [86503] 2011 2016 2011 2015 2016 2009 2014 2007 2017 2010 2017 2010 2013
## [86516] 2014 2014 2015 2015 2015 2015 2016 2017 2015 2011 2017 2016 2016
## [86529] 2015 2017 2015 2015 2016 2017 2009 2007 2014 2016 2016 2016 2011
## [86542] 2012 2013 2016 2016 2011 2015 2016 2016 2016 2016 2009 2007 2013
## [86555] 2016 2015 2007 2012 2007 2008 2017 2009 2008 2016 2017 2012 2009
## [86568] 2015 2015 2017 2008 2008 2013 2010 2016 2016 2010 2016 2013 2008
## [86581] 2008 2011 2016 2017 2008 2009 2011 2011 2012 2007 2013 2015 2015
## [86594] 2015 2012 2014 2015 2017 2015 2016 2015 2010 2015 2017 2009 2012
## [86607] 2010 2010 2010 2016 2016 2015 2016 2015 2017 2008 2016 2007 2015
## [86620] 2013 2017 2016 2016 2015 2008 2015 2016 2015 2014 2017 2009 2017
## [86633] 2009 2013 2013 2016 2008 2014 2011 2017 2007 2015 2016 2016 2016
## [86646] 2015 2017 2012 2016 2017 2016 2016 2012 2011 2007 2017 2011 2015
## [86659] 2017 2016 2016 2010 2015 2016 2012 2009 2011 2007 2008 2016 2017
## [86672] 2015 2008 2015 2008 2008 2013 2014 2014 2017 2010 2011 2016 2016
## [86685] 2017 2008 2015 2016 2015 2016 2013 2016 2009 2017 2009 2012 2015
## [86698] 2011 2008 2014 2008 2013 2016 2017 2015 2007 2015 2009 2015 2010
## [86711] 2017 2011 2016 2016 2017 2007 2016 2010 2012 2015 2017 2016 2010
## [86724] 2012 2016 2017 2012 2011 2013 2016 2016 2016 2012 2011 2014 2008
## [86737] 2007 2015 2017 2016 2010 2016 2015 2017 2017 2017 2011 2016 2016
## [86750] 2017 2017 2010 2007 2016 2017 2017 2009 2015 2017 2012 2016 2007
## [86763] 2015 2016 2017 2017 2009 2010 2016 2017 2011 2009 2016 2013 2017
## [86776] 2015 2015 2017 2008 2010 2015 2015 2015 2017 2011 2011 2010 2017
## [86789] 2017 2010 2010 2017 2013 2008 2011 2016 2016 2011 2007 2007 2009
## [86802] 2016 2011 2015 2016 2016 2017 2017 2015 2016 2016 2015 2017 2007
## [86815] 2008 2007 2007 2008 2016 2010 2009 2013 2015 2016 2016 2011 2017
## [86828] 2012 2015 2015 2008 2009 2008 2015 2015 2015 2016 2012 2016 2016
## [86841] 2012 2012 2013 2011 2014 2015 2011 2009 2011 2010 2017 2013 2015
## [86854] 2015 2016 2017 2014 2016 2008 2016 2017 2009 2008 2016 2008 2017
## [86867] 2013 2015 2016 2016 2016 2009 2015 2008 2015 2015 2015 2015 2007
## [86880] 2008 2015 2008 2008 2007 2012 2011 2017 2016 2007 2010 2014 2017
## [86893] 2010 2014 2007 2016 2007 2016 2009 2008 2014 2015 2017 2013 2011
## [86906] 2010 2015 2008 2015 2016 2017 2011 2012 2017 2010 2012 2012 2009
## [86919] 2007 2013 2016 2011 2015 2014 2015 2017 2007 2013 2014 2016 2008
## [86932] 2013 2008 2014 2016 2016 2009 2008 2017 2017 2012 2010 2013 2016
## [86945] 2016 2017 2017 2007 2015 2017 2012 2016 2007 2015 2016 2011 2009
## [86958] 2010 2012 2015 2009 2011 2007 2015 2010 2009 2016 2016 2007 2008
## [86971] 2009 2012 2009 2015 2010 2007 2017 2011 2008 2014 2015 2017 2009
## [86984] 2012 2007 2010 2008 2016 2016 2017 2017 2015 2015 2017 2011 2011
## [86997] 2008 2013 2015 2015 2017 2015 2013 2013 2016 2017 2011 2015 2016
## [87010] 2017 2013 2011 2013 2016 2016 2017 2009 2016 2016 2016 2016 2016
## [87023] 2009 2013 2013 2014 2007 2010 2014 2016 2016 2016 2017 2017 2007
## [87036] 2010 2015 2016 2015 2017 2015 2016 2016 2017 2015 2015 2015 2017
## [87049] 2011 2015 2017 2015 2017 2010 2010 2016 2017 2009 2010 2015 2016
## [87062] 2017 2016 2017 2011 2015 2017 2011 2013 2015 2016 2015 2016 2010
## [87075] 2011 2009 2015 2014 2015 2011 2016 2013 2015 2017 2009 2012 2017
## [87088] 2007 2017 2017 2010 2012 2011 2016 2007 2016 2015 2014 2016 2016
## [87101] 2015 2015 2015 2017 2010 2013 2014 2010 2007 2017 2010 2012 2015
## [87114] 2016 2012 2013 2013 2017 2008 2007 2013 2014 2015 2010 2011 2016
## [87127] 2015 2009 2007 2014 2016 2017 2016 2015 2017 2012 2010 2010 2013
## [87140] 2015 2009 2015 2017 2016 2016 2017 2007 2007 2016 2010 2015 2015
## [87153] 2016 2017 2011 2013 2016 2015 2007 2013 2016 2013 2017 2015 2014
## [87166] 2017 2012 2012 2016 2007 2014 2016 2017 2015 2017 2011 2015 2015
## [87179] 2017 2015 2015 2017 2012 2007 2014 2016 2009 2008 2015 2016 2015
## [87192] 2007 2007 2008 2016 2008 2015 2015 2016 2017 2010 2008 2015 2016
## [87205] 2007 2011 2007 2015 2015 2012 2008 2011 2015 2015 2016 2017 2007
## [87218] 2013 2014 2015 2010 2011 2016 2015 2013 2014 2015 2015 2015 2016
## [87231] 2010 2009 2016 2017 2016 2017 2016 2016 2015 2016 2017 2009 2013
## [87244] 2016 2016 2016 2017 2015 2007 2007 2007 2016 2010 2009 2016 2012
## [87257] 2010 2017 2010 2007 2015 2015 2008 2009 2016 2017 2012 2017 2009
## [87270] 2016 2012 2016 2016 2017 2011 2014 2011 2013 2015 2017 2007 2016
## [87283] 2017 2017 2017 2013 2015 2008 2016 2015 2016 2007 2010 2007 2012
## [87296] 2016 2017 2010 2012 2015 2015 2009 2016 2007 2008 2014 2017 2017
## [87309] 2009 2016 2016 2008 2013 2015 2015 2016 2017 2008 2013 2014 2015
## [87322] 2009 2007 2012 2013 2010 2012 2008 2016 2010 2010 2007 2010 2016
## [87335] 2016 2017 2017 2007 2015 2014 2017 2016 2017 2017 2008 2012 2009
## [87348] 2017 2017 2008 2009 2013 2011 2015 2016 2016 2017 2017 2016 2008
## [87361] 2013 2017 2008 2012 2015 2017 2007 2008 2014 2017 2017 2014 2010
## [87374] 2009 2012 2007 2016 2017 2015 2017 2008 2009 2014 2015 2017 2015
## [87387] 2015 2011 2008 2013 2012 2012 2010 2013 2015 2017 2012 2013 2015
## [87400] 2017 2010 2007 2008 2016 2016 2017 2009 2009 2013 2017 2009 2017
## [87413] 2012 2017 2017 2011 2009 2016 2015 2013 2016 2014 2015 2015 2017
## [87426] 2017 2007 2012 2015 2016 2017 2008 2016 2015 2016 2016 2010 2010
## [87439] 2013 2016 2010 2007 2016 2017 2017 2011 2016 2015 2017 2012 2016
## [87452] 2016 2012 2012 2007 2009 2009 2010 2015 2012 2013 2016 2017 2010
## [87465] 2015 2017 2016 2012 2015 2016 2007 2011 2014 2016 2009 2017 2009
## [87478] 2013 2017 2015 2016 2016 2017 2009 2013 2015 2015 2008 2013 2015
## [87491] 2009 2009 2008 2014 2015 2016 2016 2016 2017 2007 2017 2015 2010
## [87504] 2016 2009 2015 2017 2010 2017 2012 2007 2008 2012 2016 2017 2007
## [87517] 2013 2010 2015 2010 2010 2007 2012 2007 2012 2015 2016 2008 2011
## [87530] 2017 2017 2016 2015 2016 2016 2017 2012 2013 2014 2015 2008 2010
## [87543] 2011 2013 2013 2016 2016 2011 2014 2015 2010 2017 2008 2015 2017
## [87556] 2010 2010 2008 2014 2015 2015 2015 2016 2012 2010 2013 2014 2016
## [87569] 2011 2009 2017 2012 2016 2017 2010 2014 2017 2017 2012 2013 2017
## [87582] 2015 2016 2017 2010 2007 2015 2008 2008 2015 2014 2016 2016 2011
## [87595] 2010 2009 2011 2013 2014 2017 2013 2015 2015 2009 2016 2007 2008
## [87608] 2010 2014 2017 2007 2016 2014 2013 2011 2014 2015 2017 2011 2015
## [87621] 2015 2017 2015 2015 2009 2017 2012 2010 2009 2015 2015 2009 2014
## [87634] 2015 2009 2014 2017 2007 2013 2017 2010 2015 2015 2016 2016 2017
## [87647] 2012 2007 2015 2008 2016 2017 2010 2007 2007 2017 2017 2012 2010
## [87660] 2014 2016 2017 2012 2014 2015 2017 2007 2011 2016 2017 2009 2015
## [87673] 2016 2016 2016 2007 2015 2016 2008 2010 2009 2008 2011 2009 2015
## [87686] 2016 2017 2010 2008 2015 2015 2010 2007 2016 2008 2015 2012 2014
## [87699] 2015 2010 2008 2015 2013 2015 2017 2017 2017 2013 2014 2014 2015
## [87712] 2015 2016 2011 2015 2011 2015 2010 2016 2017 2012 2009 2016 2008
## [87725] 2016 2017 2008 2009 2015 2009 2016 2014 2015 2017 2015 2017 2012
## [87738] 2014 2010 2016 2010 2016 2016 2010 2015 2017 2015 2014 2009 2011
## [87751] 2016 2017 2017 2013 2014 2017 2007 2008 2009 2016 2012 2008 2012
## [87764] 2017 2015 2016 2012 2008 2015 2017 2007 2009 2016 2008 2014 2013
## [87777] 2014 2016 2016 2015 2017 2007 2009 2015 2016 2017 2007 2016 2017
## [87790] 2009 2010 2015 2016 2011 2009 2008 2015 2011 2016 2016 2015 2011
## [87803] 2012 2016 2012 2013 2016 2016 2007 2013 2015 2016 2017 2016 2007
## [87816] 2014 2015 2017 2016 2012 2010 2016 2013 2013 2015 2016 2012 2015
## [87829] 2010 2012 2013 2015 2017 2017 2007 2017 2009 2016 2012 2015 2015
## [87842] 2015 2009 2015 2017 2010 2013 2013 2016 2017 2017 2007 2016 2017
## [87855] 2012 2009 2011 2015 2016 2016 2016 2016 2016 2016 2017 2017 2009
## [87868] 2013 2012 2011 2016 2017 2008 2011 2017 2008 2010 2016 2016 2017
## [87881] 2011 2015 2017 2017 2015 2017 2017 2012 2010 2016 2009 2016 2008
## [87894] 2016 2010 2010 2013 2015 2015 2017 2017 2017 2010 2016 2016 2015
## [87907] 2009 2010 2015 2016 2013 2014 2016 2016 2017 2014 2016 2007 2011
## [87920] 2013 2013 2015 2008 2008 2013 2014 2017 2011 2016 2009 2016 2017
## [87933] 2010 2010 2010 2007 2010 2013 2017 2011 2012 2015 2011 2012 2010
## [87946] 2013 2010 2016 2015 2016 2008 2015 2017 2015 2010 2008 2017 2012
## [87959] 2008 2011 2008 2007 2011 2015 2016 2016 2017 2016 2013 2014 2016
## [87972] 2016 2016 2009 2016 2013 2017 2013 2015 2015 2017 2008 2015 2017
## [87985] 2014 2015 2011 2011 2016 2015 2007 2015 2016 2008 2011 2011 2015
## [87998] 2011 2014 2017 2011 2015 2017 2017 2017 2013 2016 2012 2013 2017
## [88011] 2017 2007 2008 2016 2015 2009 2016 2015 2016 2009 2010 2016 2017
## [88024] 2008 2015 2016 2008 2013 2016 2015 2007 2012 2016 2016 2010 2013
## [88037] 2016 2016 2012 2017 2013 2016 2010 2014 2015 2009 2013 2013 2016
## [88050] 2016 2016 2017 2017 2009 2014 2014 2016 2015 2016 2012 2011 2011
## [88063] 2016 2016 2008 2008 2009 2013 2014 2016 2016 2016 2017 2010 2010
## [88076] 2016 2008 2009 2016 2014 2009 2010 2013 2015 2016 2008 2013 2013
## [88089] 2017 2008 2008 2016 2012 2008 2016 2014 2015 2017 2014 2015 2017
## [88102] 2015 2015 2017 2011 2014 2017 2008 2015 2016 2016 2008 2016 2016
## [88115] 2016 2008 2014 2011 2017 2017 2008 2016 2015 2016 2017 2008 2012
## [88128] 2014 2015 2016 2012 2012 2016 2008 2013 2015 2017 2017 2010 2016
## [88141] 2012 2010 2008 2015 2011 2015 2017 2011 2014 2014 2009 2014 2015
## [88154] 2015 2017 2010 2008 2015 2017 2007 2016 2009 2016 2007 2014 2016
## [88167] 2015 2015 2012 2007 2016 2009 2009 2014 2015 2008 2017 2011 2016
## [88180] 2014 2015 2016 2017 2016 2017 2016 2010 2016 2012 2009 2009 2014
## [88193] 2015 2017 2015 2015 2017 2016 2009 2010 2011 2009 2015 2010 2011
## [88206] 2008 2014 2015 2017 2017 2010 2008 2012 2016 2017 2007 2011 2015
## [88219] 2008 2015 2009 2015 2008 2017 2012 2015 2015 2007 2016 2009 2014
## [88232] 2015 2015 2008 2007 2016 2014 2008 2015 2010 2015 2010 2012 2008
## [88245] 2010 2008 2010 2014 2015 2015 2013 2017 2007 2014 2017 2009 2017
## [88258] 2016 2010 2012 2016 2017 2011 2010 2017 2007 2016 2016 2009 2016
## [88271] 2016 2016 2016 2017 2015 2016 2015 2009 2012 2017 2009 2008 2007
## [88284] 2015 2017 2016 2008 2013 2016 2016 2009 2016 2017 2009 2016 2017
## [88297] 2007 2009 2017 2017 2009 2009 2014 2012 2016 2016 2016 2012 2008
## [88310] 2014 2015 2017 2008 2011 2010 2017 2007 2010 2010 2015 2015 2010
## [88323] 2016 2016 2008 2017 2012 2015 2010 2015 2015 2017 2012 2015 2016
## [88336] 2015 2016 2012 2016 2008 2012 2012 2010 2015 2016 2017 2008 2016
## [88349] 2009 2008 2017 2017 2017 2015 2017 2013 2011 2009 2016 2011 2012
## [88362] 2012 2014 2008 2013 2015 2016 2010 2016 2010 2014 2016 2016 2017
## [88375] 2009 2012 2016 2012 2013 2014 2017 2017 2007 2015 2010 2017 2014
## [88388] 2009 2007 2016 2015 2008 2016 2017 2009 2015 2015 2016 2015 2011
## [88401] 2015 2014 2017 2017 2007 2010 2015 2016 2016 2010 2017 2015 2012
## [88414] 2008 2014 2016 2013 2015 2016 2016 2015 2016 2017 2009 2008 2016
## [88427] 2017 2012 2011 2017 2009 2007 2012 2015 2016 2015 2017 2010 2015
## [88440] 2015 2017 2016 2015 2016 2009 2016 2010 2012 2015 2007 2007 2016
## [88453] 2012 2007 2015 2008 2012 2016 2016 2007 2016 2017 2007 2015 2015
## [88466] 2016 2017 2009 2013 2017 2008 2012 2015 2015 2015 2017 2017 2007
## [88479] 2009 2017 2013 2016 2017 2017 2017 2015 2016 2017 2009 2008 2012
## [88492] 2007 2016 2017 2009 2008 2010 2015 2014 2015 2016 2011 2007 2008
## [88505] 2015 2017 2009 2015 2017 2007 2013 2015 2017 2016 2017 2016 2007
## [88518] 2010 2011 2009 2008 2015 2017 2012 2011 2007 2008 2015 2014 2016
## [88531] 2017 2012 2015 2016 2011 2016 2017 2008 2015 2016 2016 2013 2016
## [88544] 2017 2009 2014 2008 2008 2017 2017 2017 2014 2015 2016 2017 2017
## [88557] 2010 2016 2015 2007 2015 2011 2017 2016 2008 2010 2011 2009 2015
## [88570] 2016 2017 2015 2012 2011 2011 2015 2016 2017 2012 2013 2014 2014
## [88583] 2015 2015 2011 2014 2009 2017 2012 2010 2015 2015 2016 2010 2007
## [88596] 2013 2015 2016 2015 2008 2016 2015 2010 2014 2015 2017 2008 2015
## [88609] 2017 2017 2017 2009 2012 2016 2015 2017 2011 2011 2008 2010 2015
## [88622] 2015 2008 2013 2017 2012 2016 2010 2016 2015 2009 2008 2010 2017
## [88635] 2008 2011 2008 2010 2008 2010 2007 2013 2015 2015 2017 2007 2015
## [88648] 2017 2011 2010 2007 2013 2016 2017 2010 2011 2008 2008 2007 2007
## [88661] 2016 2012 2011 2015 2012 2012 2010 2016 2017 2009 2016 2016 2016
## [88674] 2017 2015 2014 2011 2016 2016 2015 2007 2007 2012 2015 2017 2014
## [88687] 2008 2010 2007 2013 2015 2016 2016 2017 2017 2017 2009 2010 2015
## [88700] 2016 2017 2017 2007 2009 2013 2016 2016 2011 2015 2017 2017 2009
## [88713] 2016 2015 2016 2009 2015 2015 2017 2017 2017 2013 2015 2015 2008
## [88726] 2007 2015 2017 2014 2008 2007 2010 2017 2012 2012 2015 2017 2011
## [88739] 2016 2017 2008 2016 2016 2009 2008 2008 2014 2017 2017 2011 2007
## [88752] 2010 2007 2017 2017 2016 2016 2009 2007 2010 2015 2017 2017 2011
## [88765] 2013 2016 2008 2016 2016 2016 2015 2014 2015 2016 2016 2017 2017
## [88778] 2008 2011 2017 2017 2011 2013 2015 2015 2016 2012 2013 2016 2015
## [88791] 2008 2015 2016 2013 2013 2017 2016 2012 2014 2017 2016 2007 2013
## [88804] 2013 2014 2016 2016 2011 2016 2016 2017 2017 2007 2016 2017 2016
## [88817] 2017 2013 2017 2017 2012 2016 2009 2015 2016 2008 2017 2015 2008
## [88830] 2009 2012 2008 2016 2012 2017 2012 2016 2017 2012 2016 2015 2007
## [88843] 2011 2009 2014 2016 2016 2017 2012 2013 2010 2007 2015 2011 2009
## [88856] 2013 2016 2015 2015 2013 2015 2015 2008 2012 2016 2010 2008 2013
## [88869] 2015 2017 2011 2015 2017 2008 2010 2016 2015 2017 2010 2007 2009
## [88882] 2016 2009 2015 2011 2015 2016 2008 2015 2017 2007 2015 2008 2016
## [88895] 2016 2012 2012 2016 2016 2008 2012 2014 2016 2010 2015 2015 2015
## [88908] 2013 2016 2015 2012 2012 2015 2017 2008 2013 2015 2016 2017 2010
## [88921] 2013 2016 2017 2007 2013 2014 2016 2016 2017 2008 2017 2013 2010
## [88934] 2011 2015 2017 2007 2016 2017 2016 2012 2015 2016 2016 2016 2007
## [88947] 2016 2015 2016 2017 2008 2015 2016 2011 2010 2010 2013 2015 2013
## [88960] 2014 2015 2010 2012 2014 2016 2015 2016 2009 2016 2015 2014 2016
## [88973] 2016 2017 2008 2016 2016 2009 2015 2015 2015 2016 2007 2017 2016
## [88986] 2013 2013 2016 2010 2017 2017 2012 2016 2017 2013 2015 2016 2010
## [88999] 2015 2017 2009 2015 2015 2015 2015 2016 2017 2012 2013 2015 2015
## [89012] 2015 2010 2014 2017 2009 2012 2016 2016 2008 2011 2016 2016 2016
## [89025] 2017 2015 2015 2016 2017 2013 2016 2017 2017 2012 2007 2010 2011
## [89038] 2009 2016 2017 2013 2017 2011 2016 2010 2009 2013 2017 2017 2009
## [89051] 2015 2010 2011 2008 2007 2016 2015 2008 2017 2017 2017 2017 2010
## [89064] 2017 2015 2017 2010 2016 2017 2015 2015 2017 2017 2010 2012 2013
## [89077] 2008 2009 2007 2017 2017 2016 2016 2013 2015 2015 2011 2014 2015
## [89090] 2012 2009 2017 2009 2015 2009 2013 2015 2009 2011 2016 2016 2007
## [89103] 2010 2016 2009 2014 2016 2014 2017 2017 2010 2017 2009 2011 2015
## [89116] 2016 2016 2007 2013 2013 2016 2008 2017 2007 2007 2015 2015 2017
## [89129] 2009 2016 2016 2016 2011 2016 2016 2017 2015 2016 2008 2016 2017
## [89142] 2016 2016 2017 2007 2008 2016 2016 2011 2015 2010 2015 2012 2013
## [89155] 2017 2017 2012 2015 2015 2013 2016 2016 2007 2008 2010 2017 2010
## [89168] 2011 2007 2008 2017 2017 2012 2017 2016 2016 2007 2015 2007 2007
## [89181] 2008 2008 2013 2008 2016 2017 2009 2016 2010 2008 2008 2009 2010
## [89194] 2015 2017 2017 2011 2007 2016 2016 2017 2007 2015 2017 2015 2017
## [89207] 2015 2016 2017 2017 2017 2016 2016 2016 2014 2016 2017 2008 2014
## [89220] 2015 2017 2017 2008 2015 2016 2016 2015 2016 2016 2017 2008 2017
## [89233] 2007 2010 2015 2017 2011 2010 2017 2007 2010 2014 2014 2011 2016
## [89246] 2017 2012 2016 2016 2016 2009 2012 2014 2017 2009 2017 2017 2017
## [89259] 2008 2016 2016 2008 2008 2008 2012 2014 2017 2010 2015 2017 2014
## [89272] 2015 2007 2009 2010 2011 2014 2017 2016 2014 2015 2007 2008 2016
## [89285] 2017 2017 2016 2009 2012 2015 2016 2017 2011 2012 2016 2016 2017
## [89298] 2010 2012 2015 2008 2011 2008 2009 2010 2010 2009 2017 2017 2010
## [89311] 2016 2016 2017 2007 2010 2011 2013 2016 2017 2015 2017 2017 2017
## [89324] 2015 2012 2017 2009 2016 2007 2015 2015 2016 2016 2007 2015 2015
## [89337] 2017 2015 2009 2014 2013 2013 2016 2016 2017 2015 2013 2014 2015
## [89350] 2015 2016 2009 2017 2017 2012 2017 2015 2017 2008 2010 2009 2016
## [89363] 2009 2008 2012 2015 2017 2014 2015 2016 2009 2009 2017 2012 2014
## [89376] 2017 2011 2015 2015 2008 2016 2015 2017 2015 2017 2007 2015 2015
## [89389] 2017 2012 2009 2007 2012 2008 2016 2009 2016 2012 2017 2017 2010
## [89402] 2017 2017 2017 2008 2009 2014 2015 2016 2017 2014 2016 2017 2011
## [89415] 2017 2013 2015 2017 2017 2017 2015 2015 2011 2017 2007 2008 2014
## [89428] 2015 2009 2015 2017 2016 2010 2009 2015 2015 2016 2017 2009 2016
## [89441] 2012 2016 2011 2010 2015 2015 2017 2016 2014 2015 2016 2011 2010
## [89454] 2008 2014 2012 2010 2008 2015 2009 2007 2008 2014 2015 2016 2016
## [89467] 2015 2007 2008 2014 2011 2009 2017 2015 2017 2007 2010 2015 2016
## [89480] 2015 2016 2007 2017 2014 2016 2016 2017 2017 2017 2009 2007 2010
## [89493] 2014 2017 2015 2016 2017 2017 2015 2015 2016 2015 2017 2014 2015
## [89506] 2015 2017 2008 2016 2015 2016 2016 2012 2015 2017 2016 2008 2016
## [89519] 2017 2011 2008 2012 2007 2012 2008 2014 2014 2017 2012 2010 2008
## [89532] 2016 2017 2010 2009 2017 2017 2016 2017 2007 2012 2017 2017 2007
## [89545] 2007 2015 2017 2007 2012 2017 2012 2014 2015 2015 2017 2016 2016
## [89558] 2017 2009 2015 2012 2017 2007 2013 2009 2008 2016 2017 2008 2012
## [89571] 2015 2016 2014 2017 2017 2017 2016 2017 2007 2015 2016 2017 2017
## [89584] 2012 2015 2015 2017 2015 2016 2017 2015 2015 2017 2016 2017 2015
## [89597] 2007 2015 2015 2015 2017 2016 2010 2008 2015 2017 2011 2008 2010
## [89610] 2016 2016 2013 2011 2017 2010 2015 2016 2009 2011 2010 2015 2008
## [89623] 2015 2010 2009 2010 2013 2017 2015 2007 2008 2014 2015 2010 2016
## [89636] 2009 2014 2016 2014 2016 2016 2017 2012 2011 2008 2016 2011 2008
## [89649] 2016 2016 2017 2012 2017 2008 2015 2017 2009 2015 2015 2016 2011
## [89662] 2015 2011 2015 2016 2015 2010 2010 2010 2016 2017 2016 2017 2008
## [89675] 2016 2017 2012 2009 2007 2010 2015 2016 2016 2017 2012 2010 2016
## [89688] 2010 2007 2012 2011 2012 2015 2017 2017 2008 2008 2016 2017 2017
## [89701] 2016 2008 2017 2012 2011 2016 2017 2013 2017 2017 2015 2016 2016
## [89714] 2015 2017 2009 2017 2011 2015 2010 2016 2009 2013 2016 2017 2011
## [89727] 2014 2015 2016 2016 2016 2012 2014 2015 2015 2017 2017 2010 2016
## [89740] 2017 2016 2011 2009 2017 2011 2016 2009 2017 2015 2015 2016 2010
## [89753] 2013 2008 2012 2008 2013 2016 2016 2009 2009 2008 2007 2014 2017
## [89766] 2007 2013 2017 2017 2009 2015 2017 2015 2016 2016 2017 2010 2017
## [89779] 2008 2013 2007 2016 2017 2014 2008 2013 2014 2016 2016 2015 2016
## [89792] 2016 2017 2012 2017 2010 2014 2017 2017 2010 2009 2015 2015 2009
## [89805] 2015 2015 2016 2017 2011 2007 2014 2012 2009 2015 2016 2016 2014
## [89818] 2017 2008 2016 2017 2015 2016 2017 2017 2012 2008 2007 2007 2007
## [89831] 2014 2014 2015 2017 2017 2013 2016 2017 2007 2015 2017 2017 2011
## [89844] 2016 2017 2017 2009 2013 2010 2010 2007 2017 2012 2015 2016 2012
## [89857] 2014 2015 2008 2015 2015 2015 2016 2017 2016 2017 2015 2009 2010
## [89870] 2015 2016 2017 2015 2015 2017 2015 2008 2016 2016 2016 2015 2017
## [89883] 2016 2017 2016 2008 2012 2010 2017 2013 2015 2015 2009 2008 2014
## [89896] 2017 2009 2015 2016 2009 2013 2016 2017 2017 2012 2015 2017 2013
## [89909] 2011 2012 2014 2015 2015 2016 2009 2015 2016 2009 2015 2015 2017
## [89922] 2017 2014 2016 2017 2016 2009 2007 2007 2015 2015 2017 2008 2010
## [89935] 2015 2011 2016 2017 2007 2008 2011 2016 2016 2010 2015 2016 2007
## [89948] 2015 2015 2017 2011 2015 2016 2016 2013 2017 2016 2008 2010 2009
## [89961] 2013 2015 2014 2012 2015 2008 2007 2016 2016 2016 2011 2011 2010
## [89974] 2015 2017 2016 2017 2010 2009 2010 2010 2007 2009 2009 2015 2017
## [89987] 2007 2014 2016 2015 2016 2017 2017 2015 2017 2011 2008 2007 2016
## [90000] 2012 2012 2012 2009 2016 2017 2011 2008 2010 2016 2013 2017 2011
## [90013] 2013 2014 2017 2016 2014 2008 2016 2017 2017 2015 2009 2013 2014
## [90026] 2016 2016 2016 2007 2009 2009 2014 2010 2012 2014 2015 2016 2011
## [90039] 2009 2016 2016 2017 2010 2009 2016 2016 2017 2012 2009 2012 2016
## [90052] 2016 2017 2017 2011 2012 2016 2016 2016 2009 2012 2017 2008 2016
## [90065] 2017 2011 2013 2007 2015 2014 2016 2007 2012 2017 2008 2016 2016
## [90078] 2016 2016 2015 2017 2011 2017 2017 2009 2015 2016 2017 2011 2016
## [90091] 2016 2016 2017 2013 2013 2013 2010 2008 2013 2016 2017 2015 2009
## [90104] 2009 2016 2016 2017 2010 2015 2017 2015 2008 2015 2010 2013 2015
## [90117] 2014 2016 2017 2008 2016 2017 2012 2016 2017 2012 2014 2012 2017
## [90130] 2017 2007 2012 2016 2016 2012 2015 2008 2016 2008 2015 2016 2009
## [90143] 2009 2016 2012 2015 2015 2009 2012 2014 2009 2015 2007 2013 2015
## [90156] 2017 2008 2016 2017 2017 2017 2013 2016 2011 2013 2016 2011 2017
## [90169] 2015 2014 2015 2015 2009 2008 2012 2015 2015 2016 2009 2010 2014
## [90182] 2017 2017 2011 2013 2016 2016 2015 2016 2009 2016 2017 2011 2015
## [90195] 2015 2016 2017 2017 2012 2016 2017 2013 2015 2016 2016 2017 2008
## [90208] 2011 2007 2016 2017 2012 2008 2011 2008 2014 2016 2016 2011 2015
## [90221] 2016 2016 2017 2017 2016 2017 2017 2008 2009 2015 2017 2009 2017
## [90234] 2013 2015 2017 2017 2016 2014 2014 2012 2008 2015 2017 2017 2009
## [90247] 2016 2016 2007 2013 2012 2015 2016 2017 2013 2015 2017 2013 2014
## [90260] 2015 2017 2008 2013 2016 2016 2011 2015 2016 2010 2008 2011 2016
## [90273] 2016 2012 2011 2010 2013 2016 2010 2008 2013 2015 2017 2007 2016
## [90286] 2017 2009 2011 2013 2016 2007 2011 2008 2012 2013 2016 2016 2017
## [90299] 2011 2008 2012 2009 2016 2015 2015 2017 2012 2008 2015 2015 2015
## [90312] 2007 2013 2009 2007 2012 2011 2016 2008 2010 2015 2016 2011 2012
## [90325] 2011 2009 2017 2016 2011 2015 2010 2016 2016 2017 2007 2016 2012
## [90338] 2011 2011 2008 2008 2016 2016 2015 2017 2011 2014 2016 2016 2017
## [90351] 2009 2008 2015 2015 2009 2012 2015 2016 2007 2016 2017 2017 2010
## [90364] 2017 2007 2016 2017 2011 2013 2016 2015 2016 2016 2012 2016 2013
## [90377] 2009 2016 2015 2016 2016 2017 2017 2009 2016 2011 2016 2015 2011
## [90390] 2007 2013 2017 2009 2014 2011 2008 2013 2015 2007 2015 2017 2010
## [90403] 2015 2016 2017 2017 2011 2007 2007 2009 2016 2016 2017 2011 2011
## [90416] 2014 2016 2017 2007 2016 2016 2007 2017 2017 2013 2017 2009 2015
## [90429] 2017 2017 2012 2008 2008 2011 2008 2016 2009 2009 2013 2013 2013
## [90442] 2010 2017 2007 2016 2017 2011 2011 2010 2013 2016 2016 2009 2008
## [90455] 2015 2017 2011 2011 2014 2015 2016 2016 2017 2008 2017 2016 2015
## [90468] 2017 2008 2015 2016 2017 2016 2015 2011 2007 2017 2014 2017 2009
## [90481] 2016 2016 2016 2016 2017 2009 2009 2017 2017 2007 2016 2017 2017
## [90494] 2008 2017 2007 2015 2016 2015 2017 2014 2017 2017 2017 2016 2016
## [90507] 2007 2016 2016 2016 2009 2016 2015 2012 2017 2012 2008 2014 2015
## [90520] 2016 2017 2007 2009 2012 2010 2017 2015 2017 2017 2017 2011 2009
## [90533] 2015 2011 2012 2007 2015 2015 2007 2010 2011 2015 2017 2009 2015
## [90546] 2015 2011 2012 2009 2009 2008 2015 2016 2016 2017 2016 2017 2008
## [90559] 2015 2012 2010 2007 2015 2017 2007 2015 2016 2015 2011 2015 2009
## [90572] 2008 2012 2015 2010 2015 2017 2010 2010 2009 2012 2015 2017 2007
## [90585] 2010 2016 2017 2014 2016 2008 2012 2014 2016 2010 2008 2011 2013
## [90598] 2010 2007 2011 2014 2014 2015 2014 2008 2008 2017 2014 2015 2016
## [90611] 2015 2017 2007 2011 2015 2016 2016 2016 2017 2011 2012 2013 2014
## [90624] 2010 2015 2015 2012 2015 2015 2015 2008 2014 2017 2017 2017 2008
## [90637] 2016 2017 2012 2017 2017 2013 2011 2009 2017 2017 2007 2012 2016
## [90650] 2016 2007 2015 2008 2016 2016 2011 2012 2011 2015 2007 2011 2015
## [90663] 2017 2015 2016 2017 2007 2014 2016 2015 2017 2009 2011 2011 2014
## [90676] 2016 2016 2017 2015 2009 2010 2012 2016 2007 2009 2015 2015 2017
## [90689] 2016 2017 2010 2014 2015 2017 2008 2014 2016 2015 2013 2015 2016
## [90702] 2009 2007 2016 2015 2014 2014 2016 2015 2016 2016 2017 2016 2013
## [90715] 2015 2016 2013 2015 2016 2016 2016 2009 2012 2016 2015 2012 2008
## [90728] 2015 2017 2017 2017 2007 2014 2017 2015 2015 2015 2017 2014 2014
## [90741] 2010 2010 2008 2014 2010 2009 2016 2017 2017 2016 2017 2009 2014
## [90754] 2016 2007 2009 2010 2016 2015 2017 2011 2008 2008 2017 2015 2015
## [90767] 2016 2011 2016 2007 2016 2016 2017 2015 2015 2017 2013 2008 2007
## [90780] 2017 2009 2015 2017 2017 2017 2012 2017 2016 2017 2015 2016 2010
## [90793] 2011 2007 2014 2016 2016 2016 2013 2015 2016 2013 2009 2012 2016
## [90806] 2013 2015 2017 2015 2016 2016 2016 2017 2007 2014 2014 2015 2016
## [90819] 2015 2017 2010 2016 2016 2008 2009 2015 2013 2016 2010 2015 2012
## [90832] 2013 2013 2015 2016 2017 2010 2017 2010 2015 2014 2011 2008 2016
## [90845] 2017 2008 2011 2015 2017 2009 2015 2017 2015 2015 2010 2016 2015
## [90858] 2016 2010 2012 2013 2015 2011 2011 2011 2015 2015 2017 2017 2016
## [90871] 2016 2015 2016 2017 2009 2014 2015 2016 2017 2017 2014 2016 2016
## [90884] 2017 2015 2009 2009 2012 2017 2017 2009 2010 2013 2007 2010 2016
## [90897] 2017 2012 2012 2017 2009 2013 2016 2010 2014 2015 2017 2015 2015
## [90910] 2016 2008 2011 2007 2007 2011 2017 2017 2010 2014 2017 2011 2008
## [90923] 2016 2017 2011 2012 2015 2015 2017 2008 2016 2017 2010 2016 2017
## [90936] 2008 2016 2010 2014 2016 2015 2016 2009 2017 2009 2016 2017 2011
## [90949] 2016 2015 2016 2015 2016 2007 2007 2016 2017 2008 2010 2013 2015
## [90962] 2007 2017 2011 2010 2017 2012 2010 2009 2012 2015 2016 2016 2017
## [90975] 2017 2010 2016 2010 2007 2013 2014 2017 2009 2011 2014 2017 2010
## [90988] 2007 2011 2013 2015 2010 2013 2016 2017 2007 2017 2017 2009 2008
## [91001] 2016 2017 2015 2012 2017 2012 2013 2015 2016 2015 2017 2009 2012
## [91014] 2015 2017 2008 2016 2012 2016 2015 2016 2015 2009 2010 2016 2016
## [91027] 2016 2016 2007 2015 2016 2015 2011 2007 2010 2015 2015 2016 2017
## [91040] 2014 2015 2008 2008 2013 2014 2007 2012 2017 2007 2010 2010 2016
## [91053] 2016 2007 2012 2009 2017 2017 2009 2011 2016 2011 2010 2009 2015
## [91066] 2016 2013 2016 2017 2017 2015 2009 2011 2016 2007 2012 2015 2017
## [91079] 2017 2016 2017 2015 2016 2009 2011 2014 2015 2016 2017 2017 2007
## [91092] 2007 2016 2017 2013 2015 2017 2011 2015 2017 2007 2007 2009 2014
## [91105] 2015 2017 2011 2013 2015 2015 2017 2007 2012 2013 2016 2015 2014
## [91118] 2017 2017 2012 2015 2009 2009 2016 2017 2012 2007 2012 2012 2010
## [91131] 2008 2016 2009 2010 2015 2016 2011 2015 2014 2015 2017 2017 2013
## [91144] 2016 2011 2014 2016 2016 2017 2011 2013 2008 2011 2010 2011 2015
## [91157] 2017 2009 2008 2014 2017 2017 2012 2016 2017 2011 2011 2011 2007
## [91170] 2011 2017 2016 2016 2010 2017 2014 2015 2017 2017 2009 2017 2017
## [91183] 2014 2015 2015 2007 2011 2016 2017 2011 2008 2016 2017 2017 2010
## [91196] 2015 2016 2017 2010 2013 2017 2009 2014 2016 2013 2016 2014 2016
## [91209] 2016 2009 2009 2008 2012 2008 2015 2016 2012 2016 2015 2009 2016
## [91222] 2014 2014 2017 2008 2012 2012 2016 2017 2011 2016 2011 2008 2016
## [91235] 2009 2014 2017 2012 2009 2012 2016 2011 2007 2013 2009 2011 2016
## [91248] 2016 2017 2009 2017 2017 2017 2007 2008 2010 2011 2015 2017 2015
## [91261] 2017 2017 2008 2017 2014 2016 2012 2014 2017 2015 2016 2017 2015
## [91274] 2007 2010 2012 2009 2016 2016 2016 2016 2017 2012 2015 2017 2017
## [91287] 2009 2016 2011 2015 2017 2017 2010 2015 2015 2017 2017 2017 2010
## [91300] 2013 2013 2009 2011 2017 2017 2017 2007 2016 2017 2013 2013 2015
## [91313] 2017 2017 2015 2016 2009 2011 2013 2013 2010 2012 2010 2010 2011
## [91326] 2014 2016 2011 2016 2015 2015 2017 2015 2015 2017 2013 2015 2017
## [91339] 2008 2016 2017 2017 2016 2017 2015 2017 2009 2010 2009 2017 2007
## [91352] 2015 2010 2009 2013 2017 2017 2017 2014 2017 2017 2007 2011 2016
## [91365] 2016 2012 2015 2016 2013 2016 2017 2011 2012 2017 2015 2008 2008
## [91378] 2012 2011 2015 2015 2016 2017 2016 2016 2007 2012 2012 2010 2015
## [91391] 2016 2016 2017 2017 2017 2015 2007 2017 2008 2015 2015 2008 2008
## [91404] 2013 2016 2016 2010 2008 2007 2007 2017 2013 2012 2011 2016 2016
## [91417] 2016 2017 2015 2016 2016 2017 2008 2007 2007 2012 2012 2014 2009
## [91430] 2015 2015 2015 2016 2008 2015 2016 2017 2017 2009 2012 2016 2014
## [91443] 2014 2015 2016 2007 2010 2008 2016 2011 2017 2007 2016 2015 2012
## [91456] 2016 2017 2011 2013 2015 2011 2009 2007 2008 2014 2015 2009 2012
## [91469] 2016 2009 2009 2014 2011 2013 2011 2017 2011 2008 2008 2015 2015
## [91482] 2015 2016 2009 2017 2015 2007 2008 2012 2013 2015 2015 2017 2017
## [91495] 2017 2016 2017 2017 2017 2010 2013 2015 2016 2012 2017 2017 2008
## [91508] 2008 2017 2008 2014 2009 2015 2013 2017 2017 2009 2012 2010 2008
## [91521] 2016 2016 2007 2009 2012 2012 2017 2016 2015 2017 2011 2016 2016
## [91534] 2015 2016 2008 2011 2007 2008 2016 2016 2017 2017 2012 2014 2016
## [91547] 2016 2017 2012 2015 2012 2015 2016 2017 2008 2010 2010 2016 2009
## [91560] 2015 2015 2015 2015 2007 2017 2007 2008 2007 2016 2012 2016 2007
## [91573] 2017 2009 2015 2015 2017 2017 2017 2007 2017 2010 2009 2017 2010
## [91586] 2014 2014 2016 2017 2011 2011 2016 2017 2017 2013 2011 2016 2012
## [91599] 2017 2007 2008 2015 2015 2012 2016 2017 2015 2009 2007 2015 2007
## [91612] 2016 2017 2011 2012 2012 2016 2017 2008 2010 2016 2008 2014 2015
## [91625] 2016 2017 2016 2015 2008 2008 2008 2015 2017 2011 2009 2011 2011
## [91638] 2013 2016 2016 2011 2009 2017 2013 2015 2016 2016 2017 2015 2016
## [91651] 2017 2009 2007 2016 2017 2009 2010 2012 2015 2017 2007 2015 2008
## [91664] 2009 2009 2017 2017 2007 2016 2015 2012 2014 2016 2017 2010 2016
## [91677] 2010 2007 2015 2017 2017 2013 2016 2017 2012 2017 2008 2015 2014
## [91690] 2015 2015 2015 2015 2015 2017 2009 2012 2016 2016 2016 2012 2016
## [91703] 2016 2017 2008 2009 2014 2017 2012 2016 2016 2017 2015 2017 2016
## [91716] 2015 2017 2009 2009 2017 2017 2015 2013 2014 2016 2016 2010 2015
## [91729] 2017 2017 2011 2017 2015 2009 2016 2017 2015 2017 2007 2015 2016
## [91742] 2014 2014 2017 2016 2015 2015 2008 2007 2008 2009 2016 2016 2016
## [91755] 2015 2007 2015 2010 2012 2007 2017 2016 2017 2017 2013 2017 2017
## [91768] 2011 2007 2008 2007 2016 2017 2012 2012 2010 2016 2016 2016 2016
## [91781] 2010 2015 2015 2017 2010 2015 2016 2016 2015 2016 2011 2016 2016
## [91794] 2013 2016 2008 2011 2016 2017 2014 2016 2016 2017 2007 2017 2015
## [91807] 2016 2007 2016 2016 2016 2015 2008 2009 2015 2016 2016 2016 2011
## [91820] 2014 2016 2017 2008 2013 2014 2017 2012 2009 2015 2015 2017 2011
## [91833] 2009 2016 2013 2016 2016 2014 2015 2016 2017 2008 2016 2017 2017
## [91846] 2008 2014 2014 2011 2015 2008 2017 2015 2007 2007 2013 2014 2016
## [91859] 2016 2007 2013 2015 2016 2012 2013 2015 2017 2015 2014 2016 2017
## [91872] 2008 2017 2015 2017 2010 2008 2014 2016 2014 2017 2012 2007 2009
## [91885] 2010 2015 2016 2012 2007 2009 2008 2012 2016 2017 2012 2007 2014
## [91898] 2014 2016 2016 2016 2016 2016 2017 2015 2016 2008 2016 2014 2015
## [91911] 2012 2016 2017 2017 2011 2016 2017 2010 2015 2017 2012 2012 2014
## [91924] 2014 2007 2009 2015 2013 2016 2016 2016 2015 2009 2014 2017 2008
## [91937] 2016 2017 2008 2017 2011 2016 2017 2008 2017 2007 2013 2015 2016
## [91950] 2013 2014 2015 2017 2007 2007 2011 2016 2016 2016 2017 2009 2015
## [91963] 2016 2011 2014 2015 2016 2010 2015 2016 2016 2007 2010 2016 2016
## [91976] 2017 2007 2015 2016 2017 2008 2015 2017 2016 2017 2013 2016 2016
## [91989] 2015 2007 2015 2016 2015 2012 2015 2012 2007 2008 2009 2012 2013
## [92002] 2017 2015 2013 2007 2009 2016 2008 2013 2015 2015 2017 2008 2011
## [92015] 2013 2014 2015 2017 2016 2007 2015 2015 2016 2017 2017 2011 2013
## [92028] 2016 2017 2008 2012 2007 2007 2016 2009 2012 2013 2015 2017 2008
## [92041] 2012 2016 2007 2016 2017 2011 2015 2015 2016 2015 2017 2016 2017
## [92054] 2015 2015 2016 2007 2016 2017 2017 2017 2011 2010 2010 2015 2017
## [92067] 2017 2015 2013 2010 2011 2013 2015 2016 2010 2008 2016 2008 2016
## [92080] 2011 2013 2010 2014 2017 2013 2015 2017 2010 2014 2016 2015 2017
## [92093] 2014 2017 2012 2016 2016 2008 2007 2014 2016 2017 2009 2010 2011
## [92106] 2017 2013 2014 2015 2016 2007 2014 2012 2007 2015 2010 2015 2017
## [92119] 2016 2015 2016 2016 2016 2015 2016 2011 2008 2014 2014 2010 2010
## [92132] 2013 2015 2008 2017 2015 2016 2010 2011 2014 2011 2013 2008 2010
## [92145] 2015 2016 2017 2012 2010 2017 2017 2017 2012 2007 2015 2016 2014
## [92158] 2016 2010 2008 2011 2015 2009 2015 2015 2011 2017 2016 2017 2009
## [92171] 2011 2016 2007 2010 2016 2016 2017 2011 2009 2012 2016 2014 2017
## [92184] 2007 2012 2017 2007 2016 2017 2009 2010 2011 2017 2017 2017 2017
## [92197] 2014 2017 2012 2011 2017 2015 2016 2017 2009 2013 2016 2017 2012
## [92210] 2015 2017 2009 2014 2017 2015 2017 2015 2017 2017 2007 2007 2017
## [92223] 2010 2015 2008 2011 2007 2012 2015 2010 2013 2015 2015 2014 2015
## [92236] 2015 2017 2014 2016 2016 2016 2017 2015 2010 2015 2015 2016 2015
## [92249] 2016 2008 2008 2015 2016 2016 2016 2017 2007 2013 2007 2012 2016
## [92262] 2011 2009 2016 2017 2017 2014 2017 2008 2015 2013 2012 2014 2016
## [92275] 2017 2009 2007 2012 2015 2015 2014 2007 2015 2016 2016 2017 2017
## [92288] 2015 2013 2016 2017 2011 2008 2015 2016 2017 2017 2011 2013 2007
## [92301] 2017 2012 2008 2015 2016 2017 2009 2009 2009 2017 2017 2015 2017
## [92314] 2009 2008 2010 2015 2016 2007 2017 2017 2008 2016 2016 2017 2017
## [92327] 2008 2012 2015 2010 2014 2015 2017 2013 2015 2014 2011 2011 2013
## [92340] 2015 2017 2007 2016 2016 2008 2010 2008 2014 2009 2017 2010 2013
## [92353] 2017 2016 2010 2008 2010 2014 2015 2009 2015 2017 2017 2010 2013
## [92366] 2015 2016 2017 2011 2017 2017 2012 2016 2016 2011 2014 2016 2015
## [92379] 2015 2014 2016 2016 2016 2016 2008 2011 2013 2015 2016 2007 2016
## [92392] 2015 2008 2015 2015 2016 2008 2007 2016 2015 2015 2016 2016 2017
## [92405] 2011 2011 2013 2007 2008 2007 2014 2007 2013 2008 2015 2016 2016
## [92418] 2012 2016 2017 2015 2015 2016 2017 2015 2017 2007 2015 2015 2015
## [92431] 2010 2010 2008 2016 2015 2017 2016 2015 2017 2015 2016 2017 2017
## [92444] 2008 2011 2016 2017 2012 2007 2016 2010 2015 2015 2015 2015 2013
## [92457] 2016 2017 2016 2013 2016 2016 2017 2012 2016 2016 2017 2011 2016
## [92470] 2017 2008 2016 2017 2017 2010 2014 2016 2017 2014 2016 2011 2012
## [92483] 2007 2008 2008 2011 2011 2015 2016 2016 2017 2017 2017 2015 2011
## [92496] 2008 2016 2013 2008 2010 2014 2016 2015 2017 2013 2013 2016 2017
## [92509] 2017 2011 2012 2013 2016 2007 2011 2015 2016 2017 2008 2008 2017
## [92522] 2016 2017 2017 2017 2010 2017 2010 2013 2017 2013 2017 2017 2008
## [92535] 2012 2007 2009 2010 2014 2017 2011 2007 2012 2008 2012 2013 2016
## [92548] 2012 2013 2016 2012 2008 2016 2015 2017 2014 2016 2015 2016 2017
## [92561] 2015 2011 2010 2013 2015 2010 2007 2015 2014 2016 2017 2017 2017
## [92574] 2015 2017 2016 2016 2017 2015 2016 2016 2017 2009 2015 2017 2008
## [92587] 2017 2013 2016 2017 2007 2016 2016 2016 2012 2010 2015 2016 2015
## [92600] 2007 2014 2016 2016 2009 2016 2016 2015 2010 2012 2015 2010 2014
## [92613] 2009 2013 2014 2017 2010 2014 2015 2017 2017 2010 2016 2017 2007
## [92626] 2008 2016 2016 2016 2017 2012 2015 2014 2013 2015 2017 2016 2015
## [92639] 2017 2009 2010 2016 2017 2011 2015 2015 2016 2017 2015 2016 2017
## [92652] 2015 2016 2017 2015 2010 2010 2010 2015 2017 2017 2009 2012 2016
## [92665] 2017 2008 2015 2015 2012 2015 2009 2011 2011 2010 2016 2016 2012
## [92678] 2009 2017 2009 2011 2012 2007 2017 2011 2017 2013 2012 2017 2011
## [92691] 2008 2009 2016 2016 2016 2011 2015 2009 2015 2015 2017 2017 2007
## [92704] 2012 2007 2015 2008 2014 2015 2012 2013 2015 2017 2010 2015 2011
## [92717] 2015 2012 2013 2016 2008 2016 2016 2008 2014 2015 2016 2017 2017
## [92730] 2017 2011 2015 2016 2017 2015 2015 2016 2007 2011 2015 2016 2016
## [92743] 2017 2017 2010 2012 2011 2015 2013 2008 2008 2015 2013 2008 2011
## [92756] 2014 2016 2017 2017 2017 2017 2015 2016 2010 2016 2017 2008 2009
## [92769] 2017 2016 2008 2012 2015 2017 2017 2011 2014 2017 2016 2017 2017
## [92782] 2013 2015 2017 2016 2015 2008 2016 2016 2015 2009 2017 2017 2011
## [92795] 2014 2016 2017 2017 2007 2009 2010 2017 2007 2014 2015 2015 2015
## [92808] 2017 2011 2009 2012 2016 2011 2012 2017 2010 2009 2008 2015 2016
## [92821] 2015 2007 2013 2014 2016 2015 2007 2013 2013 2014 2011 2016 2008
## [92834] 2010 2014 2015 2017 2012 2017 2008 2013 2015 2016 2016 2016 2015
## [92847] 2009 2015 2017 2017 2007 2007 2013 2016 2016 2016 2017 2015 2017
## [92860] 2017 2008 2016 2017 2007 2011 2007 2016 2017 2017 2017 2011 2008
## [92873] 2016 2014 2016 2017 2007 2015 2007 2016 2017 2009 2015 2016 2015
## [92886] 2009 2012 2014 2008 2012 2009 2017 2012 2015 2017 2009 2016 2017
## [92899] 2011 2007 2016 2016 2016 2015 2017 2013 2017 2014 2017 2011 2008
## [92912] 2015 2007 2007 2012 2010 2011 2011 2009 2007 2013 2017 2015 2017
## [92925] 2015 2017 2015 2017 2017 2013 2012 2008 2009 2017 2013 2015 2016
## [92938] 2015 2016 2017 2009 2017 2007 2007 2015 2008 2011 2014 2015 2016
## [92951] 2017 2008 2016 2015 2017 2009 2010 2011 2015 2017 2017 2007 2016
## [92964] 2016 2016 2017 2011 2012 2007 2012 2015 2017 2017 2009 2011 2011
## [92977] 2010 2008 2012 2008 2015 2015 2017 2011 2016 2008 2007 2009 2008
## [92990] 2015 2017 2013 2016 2009 2008 2017 2007 2015 2017 2011 2012 2015
## [93003] 2016 2008 2015 2015 2012 2015 2017 2009 2015 2015 2016 2016 2017
## [93016] 2009 2010 2016 2016 2017 2009 2013 2015 2017 2008 2012 2015 2011
## [93029] 2015 2017 2012 2015 2008 2016 2017 2013 2013 2015 2015 2012 2009
## [93042] 2011 2013 2014 2014 2017 2015 2016 2017 2017 2007 2013 2016 2017
## [93055] 2012 2015 2016 2015 2015 2017 2017 2008 2012 2007 2016 2016 2007
## [93068] 2011 2014 2016 2016 2016 2009 2015 2016 2007 2011 2010 2015 2016
## [93081] 2016 2009 2010 2015 2015 2017 2017 2017 2010 2013 2015 2017 2007
## [93094] 2016 2017 2017 2011 2015 2017 2017 2017 2017 2016 2016 2016 2007
## [93107] 2007 2017 2011 2008 2014 2012 2015 2016 2016 2016 2016 2007 2017
## [93120] 2016 2017 2015 2016 2017 2011 2011 2011 2016 2017 2009 2017 2017
## [93133] 2015 2009 2016 2016 2011 2015 2012 2017 2017 2016 2016 2012 2016
## [93146] 2017 2007 2016 2016 2017 2011 2012 2017 2017 2012 2016 2010 2017
## [93159] 2008 2017 2017 2012 2009 2012 2009 2011 2015 2015 2009 2009 2016
## [93172] 2016 2008 2012 2017 2017 2009 2012 2010 2009 2015 2016 2017 2012
## [93185] 2009 2010 2016 2015 2016 2016 2016 2015 2008 2009 2015 2008 2009
## [93198] 2015 2008 2016 2016 2017 2008 2008 2015 2016 2017 2017 2009 2009
## [93211] 2010 2011 2015 2015 2015 2015 2017 2012 2014 2016 2016 2007 2010
## [93224] 2008 2016 2007 2016 2017 2017 2015 2017 2015 2009 2013 2016 2017
## [93237] 2008 2008 2014 2014 2016 2015 2015 2015 2016 2008 2016 2016 2008
## [93250] 2011 2015 2017 2007 2017 2008 2016 2017 2009 2012 2017 2008 2015
## [93263] 2016 2015 2011 2010 2010 2017 2015 2016 2009 2016 2017 2008 2015
## [93276] 2016 2017 2008 2015 2016 2017 2017 2017 2009 2016 2015 2017 2008
## [93289] 2008 2007 2014 2017 2008 2015 2015 2016 2017 2012 2012 2011 2016
## [93302] 2017 2008 2009 2014 2015 2015 2008 2016 2016 2016 2017 2017 2015
## [93315] 2017 2013 2015 2015 2016 2016 2017 2015 2007 2010 2013 2014 2015
## [93328] 2017 2010 2010 2010 2014 2016 2008 2016 2016 2016 2010 2007 2014
## [93341] 2017 2017 2016 2017 2017 2014 2016 2016 2016 2017 2013 2015 2009
## [93354] 2008 2009 2012 2017 2016 2017 2015 2016 2017 2008 2007 2015 2016
## [93367] 2009 2011 2015 2017 2009 2016 2016 2017 2017 2007 2008 2007 2015
## [93380] 2017 2016 2016 2017 2014 2017 2011 2010 2009 2016 2016 2017 2017
## [93393] 2017 2008 2016 2011 2010 2017 2007 2010 2016 2016 2008 2017 2017
## [93406] 2011 2015 2017 2017 2007 2015 2016 2017 2009 2016 2012 2015 2016
## [93419] 2017 2012 2015 2017 2015 2016 2008 2015 2011 2010 2015 2014 2016
## [93432] 2007 2011 2016 2017 2016 2017 2013 2015 2017 2017 2008 2011 2013
## [93445] 2015 2016 2017 2015 2016 2007 2007 2015 2016 2016 2017 2013 2015
## [93458] 2016 2017 2008 2014 2016 2017 2017 2016 2009 2007 2013 2017 2012
## [93471] 2015 2016 2016 2015 2016 2017 2015 2015 2013 2014 2015 2016 2009
## [93484] 2017 2014 2016 2014 2016 2017 2007 2007 2016 2017 2017 2017 2007
## [93497] 2016 2017 2017 2017 2010 2009 2011 2012 2013 2009 2008 2015 2016
## [93510] 2016 2016 2017 2017 2007 2016 2007 2010 2015 2016 2016 2015 2007
## [93523] 2008 2008 2015 2016 2016 2017 2008 2012 2009 2013 2008 2010 2016
## [93536] 2016 2010 2015 2008 2013 2016 2015 2016 2017 2015 2007 2015 2016
## [93549] 2017 2012 2014 2015 2017 2017 2017 2017 2015 2011 2015 2017 2017
## [93562] 2007 2007 2011 2010 2015 2012 2014 2017 2012 2007 2012 2016 2017
## [93575] 2008 2010 2017 2009 2007 2009 2016 2017 2016 2014 2012 2015 2016
## [93588] 2015 2013 2016 2016 2017 2014 2016 2016 2015 2008 2011 2017 2016
## [93601] 2017 2017 2011 2016 2017 2008 2013 2015 2016 2017 2007 2007 2010
## [93614] 2009 2008 2010 2013 2017 2012 2008 2017 2010 2008 2017 2008 2016
## [93627] 2016 2016 2017 2017 2015 2016 2016 2015 2012 2008 2015 2015 2017
## [93640] 2017 2009 2011 2013 2016 2008 2015 2016 2015 2007 2011 2014 2008
## [93653] 2017 2017 2017 2012 2009 2014 2015 2015 2015 2010 2017 2011 2016
## [93666] 2016 2016 2015 2007 2007 2016 2007 2017 2009 2014 2014 2016 2017
## [93679] 2008 2011 2008 2013 2015 2008 2011 2011 2017 2016 2012 2013 2015
## [93692] 2016 2008 2017 2009 2009 2015 2011 2015 2017 2010 2010 2015 2015
## [93705] 2010 2011 2016 2017 2014 2010 2015 2016 2007 2015 2016 2012 2008
## [93718] 2009 2013 2015 2017 2010 2013 2013 2016 2015 2015 2017 2015 2016
## [93731] 2017 2017 2016 2016 2016 2009 2013 2010 2011 2014 2016 2016 2008
## [93744] 2009 2009 2016 2016 2016 2017 2017 2016 2016 2009 2014 2015 2015
## [93757] 2017 2007 2015 2017 2017 2007 2008 2015 2016 2015 2016 2017 2017
## [93770] 2016 2017 2014 2015 2015 2017 2007 2016 2016 2011 2008 2016 2015
## [93783] 2017 2011 2016 2014 2017 2017 2007 2015 2016 2016 2017 2017 2017
## [93796] 2015 2016 2017 2010 2016 2017 2007 2012 2013 2015 2009 2016 2016
## [93809] 2016 2016 2016 2017 2017 2016 2016 2017 2009 2010 2017 2016 2015
## [93822] 2013 2014 2015 2015 2011 2016 2017 2008 2010 2015 2016 2017 2015
## [93835] 2015 2016 2012 2008 2012 2013 2015 2012 2014 2016 2008 2016 2017
## [93848] 2012 2015 2017 2017 2011 2016 2010 2016 2011 2010 2014 2015 2017
## [93861] 2009 2013 2014 2016 2008 2011 2017 2017 2009 2016 2016 2008 2016
## [93874] 2016 2017 2014 2007 2015 2017 2015 2017 2008 2015 2017 2009 2008
## [93887] 2014 2010 2010 2012 2011 2015 2015 2016 2016 2008 2016 2015 2013
## [93900] 2015 2015 2015 2017 2016 2008 2010 2017 2017 2014 2009 2013 2016
## [93913] 2016 2016 2017 2017 2015 2017 2014 2015 2016 2017 2016 2015 2017
## [93926] 2008 2009 2017 2009 2016 2011 2009 2007 2012 2016 2016 2016 2017
## [93939] 2009 2016 2016 2017 2015 2017 2011 2016 2016 2014 2017 2010 2009
## [93952] 2016 2011 2012 2014 2017 2017 2015 2009 2007 2014 2015 2017 2016
## [93965] 2017 2016 2017 2017 2007 2009 2015 2015 2009 2015 2017 2012 2016
## [93978] 2016 2015 2017 2017 2010 2014 2016 2010 2007 2013 2016 2007 2012
## [93991] 2007 2016 2015 2017 2016 2009 2014 2010 2013 2016 2017 2012 2012
## [94004] 2008 2009 2012 2012 2009 2007 2014 2014 2016 2010 2010 2017 2007
## [94017] 2009 2013 2016 2016 2017 2009 2015 2016 2011 2014 2010 2015 2016
## [94030] 2016 2007 2009 2017 2014 2016 2016 2015 2009 2007 2013 2017 2012
## [94043] 2011 2011 2016 2015 2017 2017 2017 2017 2008 2013 2017 2008 2008
## [94056] 2015 2017 2009 2015 2015 2017 2017 2009 2016 2014 2015 2007 2009
## [94069] 2015 2016 2017 2016 2012 2016 2010 2015 2015 2015 2011 2015 2015
## [94082] 2008 2015 2011 2007 2015 2016 2017 2017 2008 2009 2008 2012 2009
## [94095] 2016 2015 2016 2016 2017 2012 2010 2016 2015 2009 2016 2012 2017
## [94108] 2017 2017 2008 2007 2016 2009 2017 2017 2007 2014 2015 2012 2015
## [94121] 2015 2016 2017 2015 2016 2008 2015 2016 2010 2017 2007 2007 2013
## [94134] 2014 2016 2007 2011 2015 2016 2017 2017 2007 2013 2016 2011 2014
## [94147] 2016 2009 2008 2016 2015 2016 2017 2015 2009 2010 2011 2014 2016
## [94160] 2013 2014 2016 2016 2017 2017 2015 2010 2008 2013 2016 2007 2007
## [94173] 2016 2016 2017 2010 2017 2009 2009 2007 2014 2016 2016 2015 2015
## [94186] 2017 2008 2016 2014 2007 2015 2015 2017 2012 2009 2016 2015 2016
## [94199] 2017 2016 2015 2017 2017 2016 2007 2016 2016 2014 2017 2009 2015
## [94212] 2016 2015 2017 2014 2015 2016 2016 2007 2007 2008 2009 2013 2014
## [94225] 2008 2016 2015 2007 2013 2016 2014 2013 2016 2016 2015 2015 2016
## [94238] 2015 2017 2015 2017 2015 2010 2015 2013 2017 2016 2014 2015 2015
## [94251] 2015 2016 2017 2016 2017 2008 2013 2016 2010 2008 2015 2010 2008
## [94264] 2015 2008 2009 2017 2017 2007 2015 2007 2015 2012 2015 2007 2007
## [94277] 2013 2015 2011 2016 2007 2012 2016 2017 2017 2016 2015 2016 2017
## [94290] 2010 2017 2015 2014 2016 2010 2009 2015 2017 2017 2010 2016 2013
## [94303] 2014 2017 2011 2009 2007 2016 2016 2015 2013 2015 2015 2009 2012
## [94316] 2010 2016 2010 2017 2013 2015 2015 2016 2017 2013 2015 2007 2008
## [94329] 2007 2009 2013 2007 2017 2017 2011 2017 2017 2015 2017 2017 2012
## [94342] 2016 2015 2011 2015 2015 2017 2013 2017 2017 2008 2015 2015 2016
## [94355] 2009 2015 2016 2016 2016 2017 2010 2007 2015 2007 2012 2016 2016
## [94368] 2017 2011 2015 2016 2017 2015 2009 2016 2016 2008 2015 2016 2014
## [94381] 2015 2017 2012 2015 2015 2015 2010 2013 2017 2015 2016 2017 2009
## [94394] 2010 2010 2017 2017 2017 2011 2017 2009 2009 2015 2017 2016 2015
## [94407] 2016 2015 2015 2015 2017 2010 2012 2015 2015 2017 2008 2015 2017
## [94420] 2015 2016 2011 2010 2014 2016 2017 2016 2016 2017 2010 2015 2015
## [94433] 2012 2013 2016 2017 2016 2017 2007 2007 2016 2016 2017 2016 2017
## [94446] 2013 2009 2017 2012 2016 2007 2016 2017 2015 2017 2009 2009 2015
## [94459] 2017 2017 2007 2016 2016 2010 2016 2017 2011 2011 2013 2015 2016
## [94472] 2017 2011 2007 2015 2009 2013 2015 2017 2007 2010 2015 2016 2016
## [94485] 2008 2016 2017 2017 2007 2014 2016 2011 2014 2007 2016 2016 2017
## [94498] 2017 2009 2011 2007 2016 2011 2011 2016 2007 2007 2007 2015 2015
## [94511] 2017 2012 2015 2010 2013 2013 2015 2017 2011 2011 2011 2015 2016
## [94524] 2010 2016 2015 2017 2015 2017 2014 2016 2017 2007 2008 2009 2010
## [94537] 2017 2015 2017 2013 2013 2016 2017 2011 2008 2015 2016 2011 2016
## [94550] 2016 2010 2010 2015 2009 2014 2015 2015 2016 2016 2011 2016 2017
## [94563] 2017 2007 2011 2015 2015 2017 2017 2017 2017 2008 2016 2012 2014
## [94576] 2009 2015 2016 2011 2013 2014 2017 2017 2012 2017 2011 2013 2017
## [94589] 2016 2009 2009 2015 2017 2009 2007 2015 2017 2017 2007 2016 2017
## [94602] 2017 2012 2016 2015 2016 2011 2017 2008 2013 2017 2008 2013 2014
## [94615] 2009 2016 2015 2016 2016 2012 2015 2017 2014 2015 2017 2017 2009
## [94628] 2016 2016 2017 2017 2009 2014 2016 2015 2017 2015 2016 2015 2015
## [94641] 2015 2016 2008 2012 2014 2016 2015 2009 2011 2015 2011 2013 2015
## [94654] 2016 2017 2009 2008 2017 2015 2015 2016 2016 2017 2016 2012 2011
## [94667] 2013 2015 2016 2015 2017 2010 2009 2014 2015 2009 2009 2011 2017
## [94680] 2016 2016 2017 2016 2015 2011 2011 2011 2011 2015 2011 2016 2017
## [94693] 2010 2011 2008 2016 2008 2009 2012 2010 2016 2016 2015 2007 2011
## [94706] 2015 2015 2017 2009 2016 2016 2017 2007 2008 2016 2017 2015 2016
## [94719] 2016 2008 2015 2016 2010 2013 2015 2016 2017 2013 2010 2008 2010
## [94732] 2011 2010 2014 2015 2015 2017 2007 2016 2009 2013 2012 2016 2017
## [94745] 2009 2015 2017 2017 2012 2010 2012 2016 2015 2017 2016 2017 2009
## [94758] 2009 2012 2015 2015 2016 2017 2017 2009 2014 2016 2017 2012 2015
## [94771] 2011 2013 2015 2014 2017 2016 2017 2017 2017 2008 2017 2016 2017
## [94784] 2012 2007 2017 2016 2010 2016 2011 2017 2017 2009 2008 2007 2012
## [94797] 2013 2015 2015 2017 2017 2017 2007 2009 2017 2011 2014 2015 2015
## [94810] 2016 2015 2010 2007 2017 2012 2011 2009 2017 2011 2010 2016 2016
## [94823] 2015 2015 2016 2008 2014 2017 2017 2009 2007 2015 2016 2016 2009
## [94836] 2015 2016 2013 2016 2016 2007 2009 2011 2013 2015 2014 2015 2017
## [94849] 2007 2012 2011 2010 2008 2013 2008 2013 2017 2009 2013 2015 2016
## [94862] 2017 2010 2016 2009 2012 2008 2014 2017 2008 2008 2015 2008 2017
## [94875] 2017 2016 2016 2017 2008 2012 2013 2011 2014 2016 2007 2010 2015
## [94888] 2013 2017 2012 2017 2013 2016 2016 2017 2010 2009 2013 2016 2017
## [94901] 2017 2012 2017 2017 2014 2015 2016 2010 2007 2015 2010 2013 2015
## [94914] 2016 2015 2008 2011 2007 2008 2017 2010 2009 2015 2016 2017 2011
## [94927] 2016 2017 2008 2013 2013 2016 2014 2017 2016 2016 2017 2016 2008
## [94940] 2017 2009 2010 2010 2017 2017 2017 2016 2008 2017 2017 2009 2008
## [94953] 2011 2008 2014 2014 2016 2016 2017 2017 2017 2007 2015 2016 2007
## [94966] 2012 2016 2009 2008 2012 2016 2015 2013 2013 2016 2017 2008 2008
## [94979] 2017 2011 2016 2017 2009 2012 2015 2016 2017 2017 2011 2012 2009
## [94992] 2015 2016 2016 2016 2017 2015 2014 2016 2009 2015 2012 2016 2017
## [95005] 2007 2010 2016 2016 2017 2010 2015 2014 2017 2015 2017 2015 2010
## [95018] 2015 2016 2007 2008 2010 2016 2017 2007 2013 2017 2008 2013 2015
## [95031] 2014 2016 2016 2017 2011 2015 2016 2017 2009 2012 2016 2009 2016
## [95044] 2017 2011 2008 2012 2016 2012 2010 2016 2016 2012 2015 2016 2017
## [95057] 2009 2015 2017 2013 2014 2016 2017 2008 2014 2010 2012 2012 2015
## [95070] 2017 2010 2007 2015 2013 2014 2016 2016 2017 2010 2010 2015 2016
## [95083] 2016 2010 2017 2007 2012 2014 2015 2015 2015 2016 2016 2015 2016
## [95096] 2016 2016 2012 2012 2013 2015 2016 2008 2016 2015 2017 2007 2013
## [95109] 2015 2009 2017 2008 2016 2015 2017 2015 2011 2011 2012 2014 2017
## [95122] 2011 2016 2016 2015 2016 2015 2016 2007 2010 2009 2014 2007 2010
## [95135] 2015 2010 2016 2015 2015 2016 2016 2016 2017 2015 2010 2009 2016
## [95148] 2016 2016 2017 2017 2014 2015 2010 2009 2016 2008 2010 2012 2015
## [95161] 2015 2015 2017 2017 2008 2008 2017 2013 2012 2017 2012 2017 2011
## [95174] 2015 2016 2016 2007 2015 2015 2016 2017 2017 2017 2015 2017 2017
## [95187] 2007 2007 2016 2015 2016 2008 2015 2016 2015 2015 2009 2017 2017
## [95200] 2017 2017 2012 2010 2008 2011 2015 2010 2016 2012 2016 2010 2009
## [95213] 2014 2015 2015 2016 2012 2013 2017 2016 2015 2017 2017 2014 2016
## [95226] 2017 2017 2015 2015 2017 2007 2008 2016 2016 2016 2007 2012 2014
## [95239] 2014 2015 2016 2015 2016 2017 2013 2015 2016 2017 2017 2012 2016
## [95252] 2008 2014 2015 2015 2015 2017 2016 2017 2017 2011 2014 2017 2013
## [95265] 2015 2014 2012 2012 2015 2015 2017 2009 2014 2016 2017 2011 2009
## [95278] 2010 2015 2015 2017 2012 2015 2014 2015 2015 2015 2016 2017 2010
## [95291] 2016 2008 2010 2014 2014 2012 2011 2009 2017 2012 2017 2012 2012
## [95304] 2009 2014 2016 2017 2007 2013 2016 2012 2016 2007 2007 2015 2016
## [95317] 2007 2008 2011 2016 2017 2017 2017 2015 2012 2007 2016 2017 2017
## [95330] 2010 2007 2007 2010 2013 2011 2009 2017 2009 2008 2015 2012 2016
## [95343] 2017 2017 2007 2007 2016 2017 2013 2016 2015 2015 2016 2016 2008
## [95356] 2016 2014 2015 2017 2015 2015 2016 2011 2016 2016 2008 2009 2007
## [95369] 2016 2017 2017 2015 2015 2015 2017 2015 2017 2017 2015 2016 2016
## [95382] 2012 2015 2014 2016 2011 2013 2016 2016 2017 2007 2007 2007 2017
## [95395] 2007 2014 2014 2016 2015 2010 2016 2010 2016 2017 2013 2016 2010
## [95408] 2011 2015 2016 2017 2016 2015 2017 2017 2011 2015 2016 2015 2011
## [95421] 2010 2008 2015 2017 2017 2015 2015 2015 2015 2017 2017 2013 2017
## [95434] 2017 2009 2007 2015 2015 2016 2017 2009 2010 2017 2016 2016 2007
## [95447] 2015 2016 2016 2015 2010 2015 2017 2010 2011 2016 2017 2017 2011
## [95460] 2013 2017 2008 2008 2016 2017 2015 2016 2007 2017 2014 2014 2017
## [95473] 2012 2007 2016 2016 2016 2016 2016 2016 2016 2016 2017 2015 2014
## [95486] 2012 2016 2007 2014 2010 2010 2012 2016 2012 2015 2016 2012 2007
## [95499] 2013 2015 2017 2015 2012 2009 2013 2017 2008 2010 2016 2015 2017
## [95512] 2017 2009 2014 2007 2009 2013 2015 2015 2014 2015 2016 2016 2016
## [95525] 2011 2016 2017 2017 2017 2011 2014 2015 2009 2011 2015 2015 2017
## [95538] 2016 2008 2016 2016 2016 2013 2017 2017 2012 2015 2016 2015 2016
## [95551] 2012 2012 2010 2013 2017 2008 2012 2015 2015 2015 2011 2008 2017
## [95564] 2016 2016 2011 2017 2011 2016 2015 2015 2015 2016 2017 2016 2017
## [95577] 2017 2017 2015 2016 2008 2008 2007 2017 2016 2016 2007 2009 2011
## [95590] 2015 2015 2008 2015 2016 2009 2015 2017 2012 2015 2017 2017 2009
## [95603] 2012 2015 2016 2017 2007 2009 2007 2013 2016 2009 2007 2008 2015
## [95616] 2011 2010 2014 2009 2011 2014 2010 2016 2010 2007 2014 2015 2015
## [95629] 2008 2013 2013 2016 2016 2016 2015 2009 2017 2008 2017 2010 2016
## [95642] 2011 2013 2016 2007 2008 2015 2016 2016 2017 2014 2015 2015 2012
## [95655] 2010 2014 2015 2008 2016 2016 2016 2009 2015 2017 2011 2008 2012
## [95668] 2015 2016 2010 2015 2016 2007 2007 2010 2010 2007 2011 2014 2017
## [95681] 2012 2013 2015 2015 2017 2009 2013 2015 2010 2009 2016 2016 2016
## [95694] 2016 2015 2011 2010 2016 2016 2016 2011 2012 2013 2015 2017 2015
## [95707] 2015 2010 2014 2016 2017 2017 2016 2013 2014 2016 2017 2008 2013
## [95720] 2016 2017 2011 2016 2017 2012 2015 2016 2017 2010 2016 2016 2016
## [95733] 2016 2012 2010 2013 2016 2015 2011 2016 2009 2016 2017 2009 2014
## [95746] 2014 2016 2017 2009 2013 2015 2014 2016 2015 2010 2013 2016 2013
## [95759] 2012 2012 2016 2016 2015 2017 2017 2007 2013 2015 2015 2008 2016
## [95772] 2015 2014 2016 2017 2017 2017 2010 2009 2007 2016 2016 2017 2017
## [95785] 2017 2017 2009 2007 2013 2016 2017 2011 2010 2008 2014 2011 2008
## [95798] 2009 2016 2013 2009 2009 2015 2016 2016 2007 2011 2013 2016 2016
## [95811] 2016 2013 2015 2016 2010 2009 2013 2015 2015 2012 2012 2014 2013
## [95824] 2010 2016 2016 2007 2008 2015 2017 2015 2011 2014 2016 2015 2014
## [95837] 2008 2017 2009 2016 2016 2014 2016 2016 2008 2012 2016 2016 2009
## [95850] 2015 2017 2016 2017 2011 2015 2016 2016 2013 2015 2011 2016 2012
## [95863] 2012 2017 2017 2012 2008 2015 2016 2017 2017 2012 2016 2013 2014
## [95876] 2017 2017 2009 2015 2016 2015 2014 2013 2015 2017 2017 2012 2017
## [95889] 2014 2015 2016 2017 2017 2008 2007 2017 2015 2010 2009 2016 2017
## [95902] 2009 2007 2008 2015 2016 2009 2012 2015 2016 2016 2007 2010 2016
## [95915] 2017 2010 2015 2015 2017 2015 2015 2015 2015 2016 2016 2017 2009
## [95928] 2008 2010 2013 2015 2015 2007 2016 2014 2014 2017 2016 2015 2017
## [95941] 2011 2016 2015 2015 2017 2015 2016 2017 2011 2015 2008 2011 2017
## [95954] 2017 2017 2011 2011 2016 2017 2011 2010 2015 2010 2010 2017 2007
## [95967] 2015 2014 2015 2016 2012 2015 2016 2017 2016 2017 2010 2010 2008
## [95980] 2008 2016 2011 2012 2016 2015 2007 2015 2013 2016 2016 2015 2011
## [95993] 2013 2008 2007 2011 2014 2015 2016 2016 2015 2012 2016 2011 2015
## [96006] 2016 2016 2017 2013 2016 2017 2017 2007 2017 2011 2015 2015 2017
## [96019] 2016 2016 2017 2011 2014 2015 2015 2011 2012 2014 2011 2015 2012
## [96032] 2012 2010 2014 2016 2016 2016 2015 2017 2008 2017 2017 2009 2012
## [96045] 2016 2017 2012 2015 2016 2017 2009 2009 2009 2017 2016 2017 2014
## [96058] 2017 2017 2011 2016 2016 2011 2016 2015 2017 2008 2017 2017 2012
## [96071] 2013 2016 2015 2009 2007 2017 2008 2007 2010 2014 2015 2010 2013
## [96084] 2016 2008 2016 2017 2010 2016 2015 2016 2015 2012 2017 2012 2008
## [96097] 2015 2016 2017 2017 2008 2009 2012 2015 2017 2015 2017 2008 2012
## [96110] 2016 2017 2017 2007 2008 2015 2017 2008 2014 2015 2017 2015 2017
## [96123] 2015 2016 2017 2015 2012 2016 2016 2011 2008 2013 2014 2014 2014
## [96136] 2016 2013 2007 2011 2015 2010 2014 2016 2016 2015 2012 2008 2015
## [96149] 2009 2011 2015 2016 2007 2017 2013 2014 2014 2016 2008 2016 2016
## [96162] 2015 2013 2017 2016 2017 2008 2008 2013 2015 2015 2015 2010 2016
## [96175] 2015 2015 2016 2017 2017 2011 2009 2012 2013 2017 2009 2011 2007
## [96188] 2016 2008 2009 2010 2017 2017 2016 2007 2010 2014 2015 2017 2015
## [96201] 2016 2012 2015 2016 2015 2008 2015 2016 2009 2007 2012 2015 2007
## [96214] 2013 2016 2017 2017 2007 2007 2015 2014 2015 2016 2011 2017 2017
## [96227] 2008 2010 2010 2015 2010 2007 2014 2016 2016 2007 2013 2014 2015
## [96240] 2016 2017 2007 2015 2013 2014 2012 2015 2016 2016 2017 2017 2015
## [96253] 2015 2017 2009 2016 2010 2007 2016 2010 2016 2007 2017 2017 2009
## [96266] 2010 2016 2007 2014 2015 2009 2007 2014 2015 2016 2008 2017 2009
## [96279] 2012 2009 2014 2010 2011 2012 2015 2016 2011 2009 2013 2016 2016
## [96292] 2015 2010 2016 2016 2016 2016 2017 2010 2017 2016 2012 2016 2017
## [96305] 2016 2015 2016 2011 2013 2015 2017 2013 2014 2015 2015 2016 2008
## [96318] 2013 2015 2013 2015 2016 2016 2015 2009 2017 2008 2012 2017 2010
## [96331] 2017 2016 2017 2014 2017 2008 2009 2010 2015 2016 2007 2008 2016
## [96344] 2017 2015 2016 2017 2011 2009 2008 2012 2008 2010 2008 2014 2015
## [96357] 2017 2014 2017 2007 2009 2008 2015 2016 2007 2011 2016 2015 2011
## [96370] 2010 2007 2014 2015 2015 2017 2017 2017 2017 2012 2014 2015 2017
## [96383] 2014 2009 2008 2017 2014 2015 2016 2016 2015 2017 2015 2017 2017
## [96396] 2017 2015 2017 2017 2017 2014 2017 2012 2011 2014 2015 2015 2017
## [96409] 2013 2015 2015 2011 2012 2015 2017 2011 2009 2015 2016 2011 2013
## [96422] 2017 2008 2013 2016 2015 2017 2017 2007 2010 2014 2011 2012 2015
## [96435] 2016 2015 2010 2009 2017 2009 2009 2007 2014 2017 2009 2017 2016
## [96448] 2016 2016 2014 2017 2017 2017 2011 2014 2015 2008 2007 2008 2017
## [96461] 2009 2007 2012 2017 2009 2012 2016 2016 2017 2015 2015 2016 2016
## [96474] 2017 2017 2007 2012 2016 2017 2015 2016 2016 2011 2016 2016 2016
## [96487] 2017 2017 2007 2016 2007 2014 2016 2016 2017 2017 2017 2010 2014
## [96500] 2015 2016 2007 2013 2017 2010 2009 2012 2009 2011 2013 2013 2015
## [96513] 2010 2012 2015 2014 2010 2016 2011 2009 2016 2015 2017 2016 2007
## [96526] 2007 2017 2017 2017 2008 2009 2007 2015 2016 2008 2008 2017 2016
## [96539] 2016 2017 2017 2007 2009 2016 2010 2016 2017 2010 2014 2015 2014
## [96552] 2014 2017 2009 2007 2013 2015 2016 2017 2017 2012 2012 2010 2013
## [96565] 2015 2016 2017 2013 2017 2015 2016 2016 2016 2014 2009 2015 2007
## [96578] 2014 2007 2011 2013 2017 2009 2013 2017 2011 2015 2016 2017 2007
## [96591] 2007 2016 2007 2009 2015 2015 2017 2011 2011 2010 2010 2015 2017
## [96604] 2007 2017 2016 2007 2008 2011 2016 2011 2015 2015 2017 2010 2015
## [96617] 2017 2017 2010 2010 2016 2015 2017 2014 2016 2015 2010 2012 2015
## [96630] 2015 2017 2007 2014 2016 2008 2007 2007 2007 2015 2016 2016 2017
## [96643] 2010 2007 2016 2016 2016 2011 2017 2009 2010 2012 2011 2015 2008
## [96656] 2009 2013 2015 2010 2016 2014 2015 2015 2008 2010 2016 2009 2017
## [96669] 2017 2017 2017 2008 2017 2016 2017 2012 2008 2010 2015 2016 2017
## [96682] 2008 2016 2008 2011 2017 2014 2014 2017 2016 2015 2016 2015 2016
## [96695] 2015 2015 2015 2015 2012 2015 2015 2017 2017 2009 2009 2015 2016
## [96708] 2011 2009 2007 2013 2016 2011 2009 2012 2017 2011 2014 2016 2008
## [96721] 2014 2016 2013 2015 2016 2009 2013 2016 2017 2014 2015 2012 2017
## [96734] 2017 2011 2010 2007 2013 2014 2016 2009 2008 2011 2017 2011 2010
## [96747] 2015 2007 2015 2011 2007 2014 2017 2015 2009 2010 2014 2015 2015
## [96760] 2007 2014 2017 2010 2011 2017 2015 2012 2012 2012 2016 2016 2015
## [96773] 2007 2009 2011 2009 2016 2017 2010 2012 2017 2008 2007 2013 2017
## [96786] 2016 2013 2016 2017 2017 2015 2007 2015 2017 2007 2015 2016 2016
## [96799] 2017 2015 2016 2017 2016 2010 2008 2014 2012 2010 2015 2017 2008
## [96812] 2016 2016 2009 2014 2017 2008 2011 2007 2013 2014 2016 2015 2017
## [96825] 2010 2012 2010 2014 2016 2015 2016 2010 2007 2016 2015 2016 2015
## [96838] 2017 2009 2007 2017 2016 2016 2016 2010 2009 2016 2017 2015 2017
## [96851] 2008 2016 2012 2007 2017 2017 2013 2016 2017 2015 2015 2016 2013
## [96864] 2016 2017 2008 2007 2010 2016 2016 2015 2007 2008 2015 2010 2012
## [96877] 2014 2017 2014 2016 2017 2017 2009 2016 2012 2017 2012 2014 2015
## [96890] 2010 2017 2012 2016 2009 2007 2016 2017 2010 2013 2015 2008 2016
## [96903] 2009 2011 2008 2014 2015 2011 2012 2017 2009 2017 2008 2016 2017
## [96916] 2008 2016 2007 2007 2016 2010 2009 2016 2016 2015 2015 2016 2017
## [96929] 2011 2008 2017 2010 2016 2010 2009 2014 2010 2015 2015 2007 2016
## [96942] 2016 2017 2012 2015 2017 2010 2015 2012 2013 2015 2017 2008 2013
## [96955] 2015 2017 2015 2016 2016 2017 2016 2017 2015 2017 2014 2016 2015
## [96968] 2013 2016 2017 2017 2011 2017 2013 2015 2010 2015 2015 2015 2016
## [96981] 2016 2017 2016 2016 2016 2016 2016 2017 2013 2015 2015 2017 2009
## [96994] 2014 2016 2013 2016 2016 2016 2017 2017 2011 2015 2017 2015 2017
## [97007] 2008 2015 2016 2008 2016 2010 2013 2016 2017 2008 2011 2017 2011
## [97020] 2010 2015 2011 2015 2016 2015 2015 2015 2015 2016 2017 2015 2007
## [97033] 2010 2015 2015 2015 2009 2015 2015 2015 2016 2010 2011 2008 2016
## [97046] 2016 2016 2007 2017 2010 2017 2010 2013 2016 2016 2017 2017 2007
## [97059] 2013 2015 2015 2017 2016 2012 2013 2015 2015 2016 2011 2016 2016
## [97072] 2007 2016 2011 2016 2017 2017 2017 2009 2016 2015 2015 2015 2012
## [97085] 2012 2017 2011 2012 2015 2008 2010 2015 2016 2014 2015 2008 2009
## [97098] 2008 2015 2016 2017 2017 2016 2017 2012 2011 2008 2013 2016 2015
## [97111] 2016 2007 2008 2011 2016 2017 2015 2009 2016 2016 2011 2016 2015
## [97124] 2017 2008 2016 2016 2016 2012 2014 2015 2012 2015 2017 2017 2015
## [97137] 2011 2015 2008 2011 2016 2017 2007 2014 2015 2017 2008 2011 2016
## [97150] 2016 2010 2014 2017 2009 2015 2008 2014 2013 2017 2016 2014 2015
## [97163] 2016 2010 2012 2012 2016 2012 2013 2016 2007 2015 2016 2016 2017
## [97176] 2011 2008 2016 2013 2015 2015 2009 2016 2008 2016 2015 2015 2016
## [97189] 2016 2015 2017 2007 2008 2007 2016 2015 2017 2009 2017 2012 2013
## [97202] 2016 2017 2015 2010 2017 2011 2017 2007 2008 2008 2014 2016 2016
## [97215] 2008 2007 2013 2013 2016 2010 2014 2015 2017 2012 2012 2012 2014
## [97228] 2016 2017 2017 2015 2014 2016 2007 2017 2008 2008 2015 2017 2012
## [97241] 2012 2015 2015 2014 2017 2016 2017 2015 2012 2012 2011 2016 2017
## [97254] 2008 2010 2015 2016 2011 2016 2015 2016 2017 2008 2009 2013 2014
## [97267] 2015 2016 2016 2009 2015 2013 2016 2016 2017 2008 2010 2014 2015
## [97280] 2016 2016 2012 2015 2007 2008 2015 2016 2015 2016 2017 2015 2012
## [97293] 2009 2017 2017 2015 2016 2017 2017 2008 2016 2017 2015 2016 2011
## [97306] 2010 2009 2014 2016 2007 2015 2013 2014 2008 2008 2008 2013 2015
## [97319] 2017 2015 2008 2015 2016 2016 2017 2007 2016 2007 2017 2012 2011
## [97332] 2010 2016 2017 2017 2012 2015 2015 2010 2007 2007 2012 2013 2015
## [97345] 2008 2012 2017 2011 2011 2013 2010 2014 2015 2016 2015 2017 2008
## [97358] 2015 2017 2009 2015 2014 2017 2013 2010 2016 2012 2017 2017 2010
## [97371] 2015 2011 2015 2017 2010 2015 2017 2009 2016 2015 2014 2015 2017
## [97384] 2009 2013 2016 2017 2007 2016 2010 2007 2014 2016 2017 2009 2016
## [97397] 2016 2017 2008 2008 2016 2017 2007 2012 2015 2016 2015 2017 2017
## [97410] 2009 2015 2016 2009 2008 2010 2016 2016 2016 2017 2009 2014 2017
## [97423] 2008 2009 2013 2014 2015 2015 2016 2017 2017 2010 2009 2007 2009
## [97436] 2016 2010 2015 2017 2017 2007 2016 2017 2014 2012 2013 2015 2016
## [97449] 2015 2016 2017 2013 2014 2015 2016 2016 2016 2015 2013 2014 2007
## [97462] 2009 2015 2017 2007 2017 2008 2007 2015 2015 2015 2013 2016 2007
## [97475] 2015 2009 2008 2015 2014 2015 2017 2012 2016 2012 2015 2016 2016
## [97488] 2007 2016 2012 2007 2008 2016 2010 2016 2016 2016 2017 2012 2015
## [97501] 2016 2007 2015 2008 2012 2008 2011 2015 2008 2010 2011 2016 2009
## [97514] 2016 2017 2017 2016 2016 2016 2017 2011 2017 2013 2015 2016 2016
## [97527] 2017 2010 2010 2009 2007 2008 2017 2013 2016 2017 2008 2017 2008
## [97540] 2007 2007 2014 2011 2012 2014 2015 2016 2017 2009 2015 2011 2011
## [97553] 2016 2013 2015 2017 2017 2012 2016 2014 2016 2016 2017 2017 2017
## [97566] 2012 2012 2011 2008 2011 2017 2017 2017 2009 2015 2010 2010 2013
## [97579] 2009 2008 2011 2015 2015 2016 2008 2016 2008 2015 2016 2011 2016
## [97592] 2016 2011 2016 2017 2012 2012 2010 2009 2016 2016 2017 2010 2016
## [97605] 2017 2017 2010 2016 2015 2012 2017 2009 2014 2015 2017 2016 2015
## [97618] 2017 2015 2010 2017 2011 2014 2015 2015 2016 2014 2007 2014 2016
## [97631] 2016 2014 2017 2008 2007 2015 2016 2016 2008 2009 2007 2014 2016
## [97644] 2015 2017 2007 2012 2017 2016 2015 2016 2015 2017 2017 2012 2007
## [97657] 2011 2010 2011 2015 2016 2007 2014 2016 2016 2017 2007 2017 2017
## [97670] 2016 2008 2015 2008 2013 2013 2012 2014 2015 2007 2016 2016 2017
## [97683] 2015 2017 2012 2008 2017 2016 2017 2014 2015 2016 2012 2010 2015
## [97696] 2014 2013 2014 2010 2015 2017 2010 2013 2015 2017 2017 2010 2012
## [97709] 2008 2014 2010 2008 2016 2017 2016 2017 2012 2016 2009 2016 2016
## [97722] 2012 2015 2016 2017 2010 2016 2017 2008 2013 2017 2015 2012 2008
## [97735] 2016 2017 2012 2015 2009 2013 2014 2016 2017 2016 2017 2007 2017
## [97748] 2007 2011 2009 2016 2007 2015 2015 2007 2015 2015 2007 2011 2015
## [97761] 2016 2016 2015 2016 2008 2014 2014 2016 2016 2015 2008 2009 2008
## [97774] 2016 2015 2017 2017 2007 2014 2015 2016 2016 2007 2007 2016 2017
## [97787] 2017 2011 2015 2016 2017 2007 2016 2016 2017 2012 2010 2011 2011
## [97800] 2016 2015 2017 2014 2016 2016 2008 2007 2011 2010 2017 2017 2015
## [97813] 2017 2017 2011 2012 2014 2016 2011 2012 2016 2017 2017 2007 2007
## [97826] 2008 2013 2016 2017 2007 2015 2017 2017 2009 2009 2016 2014 2016
## [97839] 2016 2016 2010 2012 2015 2015 2014 2010 2014 2015 2017 2010 2009
## [97852] 2011 2015 2008 2015 2014 2015 2009 2016 2015 2007 2013 2016 2016
## [97865] 2017 2016 2016 2016 2009 2017 2017 2017 2012 2010 2009 2013 2014
## [97878] 2017 2017 2009 2012 2013 2009 2014 2016 2017 2007 2016 2017 2015
## [97891] 2010 2015 2014 2014 2016 2016 2016 2010 2010 2007 2011 2015 2012
## [97904] 2016 2016 2017 2009 2012 2014 2015 2017 2007 2016 2007 2009 2010
## [97917] 2016 2016 2016 2016 2017 2013 2016 2016 2014 2008 2013 2015 2017
## [97930] 2017 2016 2016 2014 2016 2016 2011 2014 2016 2007 2016 2015 2017
## [97943] 2007 2015 2017 2015 2008 2015 2017 2017 2008 2016 2016 2010 2009
## [97956] 2016 2016 2017 2010 2016 2012 2016 2016 2017 2016 2008 2012 2015
## [97969] 2017 2008 2008 2009 2016 2017 2015 2017 2013 2014 2016 2016 2016
## [97982] 2016 2015 2017 2011 2007 2012 2013 2015 2016 2013 2015 2016 2016
## [97995] 2015 2008 2009 2015 2017 2017 2016 2017 2017 2017 2015 2016 2017
## [98008] 2017 2007 2009 2010 2007 2010 2013 2014 2016 2017 2007 2012 2012
## [98021] 2015 2016 2017 2017 2014 2015 2016 2017 2012 2015 2016 2015 2014
## [98034] 2011 2014 2011 2008 2014 2015 2017 2007 2015 2017 2017 2011 2010
## [98047] 2016 2008 2015 2016 2016 2017 2008 2015 2015 2016 2016 2017 2011
## [98060] 2011 2008 2016 2016 2015 2015 2016 2017 2017 2009 2016 2016 2017
## [98073] 2007 2008 2008 2008 2016 2017 2017 2009 2007 2011 2016 2008 2008
## [98086] 2016 2017 2017 2009 2017 2016 2016 2016 2016 2017 2014 2017 2008
## [98099] 2017 2017 2010 2016 2017 2008 2015 2016 2017 2008 2015 2015 2016
## [98112] 2016 2015 2016 2008 2014 2016 2017 2011 2007 2009 2015 2017 2009
## [98125] 2014 2015 2007 2016 2016 2017 2017 2014 2015 2016 2016 2016 2015
## [98138] 2017 2015 2017 2013 2014 2015 2016 2017 2008 2013 2016 2016 2010
## [98151] 2012 2015 2015 2016 2016 2013 2014 2015 2008 2007 2015 2015 2016
## [98164] 2017 2017 2007 2015 2009 2017 2007 2015 2008 2010 2007 2016 2011
## [98177] 2008 2014 2014 2016 2017 2017 2017 2008 2009 2009 2016 2011 2009
## [98190] 2015 2017 2017 2010 2013 2016 2011 2009 2014 2017 2013 2016 2015
## [98203] 2016 2017 2015 2016 2015 2008 2013 2015 2016 2010 2007 2015 2016
## [98216] 2010 2010 2013 2008 2007 2014 2017 2011 2010 2009 2011 2009 2008
## [98229] 2016 2013 2008 2010 2008 2007 2016 2016 2016 2016 2017 2011 2010
## [98242] 2008 2015 2016 2017 2016 2016 2007 2014 2014 2015 2009 2013 2015
## [98255] 2016 2016 2012 2011 2017 2017 2011 2007 2015 2008 2016 2017 2017
## [98268] 2015 2014 2007 2014 2016 2016 2016 2015 2008 2015 2017 2016 2017
## [98281] 2007 2008 2016 2011 2017 2016 2017 2008 2007 2014 2007 2008 2016
## [98294] 2016 2008 2013 2016 2015 2017 2017 2010 2016 2007 2007 2011 2013
## [98307] 2016 2017 2010 2016 2017 2012 2013 2007 2016 2017 2017 2015 2012
## [98320] 2013 2011 2009 2012 2015 2009 2012 2017 2007 2015 2010 2016 2010
## [98333] 2007 2015 2017 2008 2010 2009 2014 2012 2016 2017 2014 2016 2016
## [98346] 2007 2013 2016 2016 2010 2017 2010 2010 2015 2015 2017 2009 2015
## [98359] 2010 2014 2015 2017 2017 2007 2015 2010 2015 2016 2009 2016 2017
## [98372] 2016 2017 2017 2014 2017 2008 2016 2017 2008 2007 2013 2016 2017
## [98385] 2014 2016 2007 2013 2017 2017 2009 2016 2015 2015 2016 2016 2015
## [98398] 2017 2008 2009 2012 2008 2015 2014 2017 2015 2016 2015 2017 2014
## [98411] 2015 2015 2017 2010 2017 2012 2007 2013 2015 2016 2010 2016 2011
## [98424] 2008 2015 2017 2017 2008 2009 2008 2016 2017 2017 2012 2016 2015
## [98437] 2010 2010 2015 2017 2013 2008 2015 2015 2010 2014 2017 2017 2014
## [98450] 2017 2017 2013 2014 2017 2017 2016 2016 2017 2009 2015 2017 2017
## [98463] 2010 2017 2007 2007 2016 2016 2017 2016 2015 2008 2009 2015 2015
## [98476] 2016 2016 2017 2017 2008 2010 2017 2011 2011 2010 2016 2007 2016
## [98489] 2015 2009 2012 2011 2012 2016 2012 2016 2016 2016 2017 2012 2015
## [98502] 2016 2011 2012 2016 2017 2011 2010 2010 2009 2011 2010 2013 2014
## [98515] 2015 2015 2012 2016 2011 2009 2008 2016 2016 2017 2007 2007 2012
## [98528] 2016 2010 2012 2008 2013 2016 2015 2013 2015 2015 2017 2015 2014
## [98541] 2015 2016 2007 2012 2014 2008 2016 2016 2017 2017 2008 2009 2015
## [98554] 2015 2016 2007 2010 2012 2013 2016 2017 2014 2017 2015 2008 2015
## [98567] 2010 2009 2011 2016 2016 2017 2017 2007 2017 2010 2009 2015 2017
## [98580] 2007 2014 2015 2014 2007 2015 2011 2016 2017 2010 2011 2017 2009
## [98593] 2014 2015 2017 2017 2007 2015 2017 2009 2011 2014 2015 2017 2016
## [98606] 2017 2017 2009 2015 2010 2010 2016 2017 2017 2010 2015 2016 2011
## [98619] 2014 2014 2015 2017 2008 2011 2010 2010 2016 2017 2017 2008 2015
## [98632] 2015 2010 2017 2016 2009 2013 2016 2016 2017 2010 2016 2016 2009
## [98645] 2010 2014 2015 2008 2015 2017 2008 2015 2007 2008 2013 2015 2014
## [98658] 2016 2007 2013 2014 2015 2016 2016 2012 2016 2010 2013 2011 2012
## [98671] 2016 2017 2017 2017 2010 2015 2017 2017 2016 2011 2016 2011 2015
## [98684] 2016 2008 2017 2013 2015 2016 2017 2017 2010 2014 2016 2007 2010
## [98697] 2014 2017 2009 2014 2017 2007 2016 2016 2016 2017 2017 2015 2017
## [98710] 2014 2015 2016 2015 2015 2011 2009 2008 2010 2016 2016 2012 2015
## [98723] 2015 2015 2016 2008 2007 2011 2013 2015 2016 2015 2015 2016 2016
## [98736] 2007 2016 2016 2017 2010 2013 2015 2015 2011 2009 2014 2016 2017
## [98749] 2012 2012 2016 2016 2016 2016 2017 2008 2007 2017 2011 2010 2007
## [98762] 2012 2017 2015 2017 2008 2008 2011 2012 2017 2012 2009 2015 2017
## [98775] 2008 2015 2016 2017 2017 2015 2015 2014 2007 2015 2016 2015 2017
## [98788] 2012 2015 2016 2017 2017 2011 2011 2017 2017 2014 2016 2016 2016
## [98801] 2015 2014 2016 2017 2007 2014 2015 2016 2016 2017 2014 2016 2016
## [98814] 2011 2017 2017 2012 2010 2017 2011 2013 2014 2016 2017 2017 2007
## [98827] 2009 2015 2017 2017 2017 2017 2016 2010 2009 2009 2017 2007 2012
## [98840] 2015 2009 2009 2014 2016 2015 2017 2010 2009 2014 2015 2014 2016
## [98853] 2016 2016 2010 2007 2013 2014 2017 2008 2015 2015 2015 2015 2008
## [98866] 2017 2017 2008 2015 2011 2008 2008 2016 2016 2007 2010 2017 2015
## [98879] 2015 2011 2010 2016 2016 2017 2011 2016 2016 2017 2015 2016 2017
## [98892] 2016 2017 2016 2016 2011 2011 2014 2015 2017 2017 2007 2015 2016
## [98905] 2016 2017 2017 2008 2013 2016 2016 2017 2015 2015 2016 2013 2016
## [98918] 2017 2014 2016 2015 2015 2017 2010 2010 2017 2015 2007 2014 2015
## [98931] 2013 2014 2016 2016 2016 2017 2008 2011 2010 2010 2017 2017 2014
## [98944] 2010 2013 2016 2017 2017 2011 2007 2016 2017 2017 2014 2012 2012
## [98957] 2016 2017 2017 2017 2015 2017 2010 2012 2016 2017 2008 2010 2010
## [98970] 2015 2017 2013 2014 2016 2007 2016 2011 2013 2016 2014 2008 2010
## [98983] 2010 2013 2017 2008 2015 2016 2017 2017 2013 2015 2017 2017 2015
## [98996] 2012 2016 2017 2008 2013 2015 2013 2015 2009 2007 2015 2016 2014
## [99009] 2016 2016 2016 2007 2013 2009 2017 2009 2009 2017 2010 2007 2010
## [99022] 2015 2015 2017 2008 2007 2011 2015 2016 2016 2009 2008 2015 2017
## [99035] 2017 2017 2017 2015 2017 2010 2011 2015 2013 2016 2016 2016 2008
## [99048] 2016 2007 2009 2016 2015 2015 2008 2014 2015 2017 2016 2009 2008
## [99061] 2016 2016 2015 2007 2013 2012 2007 2011 2009 2015 2017 2016 2015
## [99074] 2016 2017 2012 2014 2016 2011 2014 2016 2017 2014 2014 2017 2011
## [99087] 2015 2007 2013 2014 2016 2016 2015 2017 2016 2016 2009 2016 2008
## [99100] 2012 2016 2011 2017 2015 2012 2009 2012 2013 2014 2016 2008 2010
## [99113] 2009 2013 2015 2016 2007 2011 2010 2010 2015 2015 2015 2009 2012
## [99126] 2011 2011 2009 2015 2009 2009 2013 2015 2017 2010 2016 2016 2016
## [99139] 2010 2016 2009 2015 2015 2015 2017 2016 2015 2013 2014 2015 2015
## [99152] 2016 2017 2017 2015 2016 2016 2016 2017 2015 2017 2015 2017 2008
## [99165] 2016 2017 2013 2014 2016 2016 2017 2012 2014 2016 2013 2015 2015
## [99178] 2016 2011 2010 2012 2013 2009 2016 2010 2015 2017 2016 2017 2010
## [99191] 2012 2016 2008 2015 2015 2016 2015 2016 2017 2009 2010 2014 2015
## [99204] 2012 2012 2012 2008 2015 2010 2015 2017 2017 2015 2017 2014 2015
## [99217] 2014 2015 2012 2016 2007 2010 2008 2016 2017 2014 2017 2009 2007
## [99230] 2016 2017 2015 2016 2016 2007 2008 2017 2017 2015 2015 2011 2013
## [99243] 2013 2015 2016 2010 2008 2016 2015 2016 2007 2015 2007 2008 2016
## [99256] 2017 2017 2012 2009 2015 2012 2015 2012 2016 2016 2011 2014 2016
## [99269] 2016 2017 2012 2015 2015 2016 2015 2016 2017 2012 2016 2015 2016
## [99282] 2007 2008 2015 2017 2010 2007 2007 2016 2016 2015 2017 2008 2014
## [99295] 2015 2008 2016 2016 2015 2016 2017 2016 2017 2007 2015 2017 2009
## [99308] 2010 2015 2012 2010 2014 2013 2017 2015 2016 2007 2016 2017 2011
## [99321] 2014 2015 2015 2014 2015 2016 2015 2016 2017 2009 2007 2016 2016
## [99334] 2011 2009 2017 2011 2014 2016 2017 2011 2014 2013 2015 2016 2016
## [99347] 2007 2009 2017 2007 2008 2009 2011 2013 2012 2014 2016 2010 2014
## [99360] 2017 2015 2012 2015 2016 2015 2011 2007 2013 2015 2017 2017 2017
## [99373] 2012 2010 2010 2015 2015 2011 2011 2017 2012 2009 2013 2015 2012
## [99386] 2008 2008 2011 2011 2011 2015 2009 2015 2015 2017 2011 2016 2015
## [99399] 2014 2016 2017 2008 2014 2011 2009 2010 2016 2017 2010 2017 2017
## [99412] 2017 2016 2017 2009 2017 2016 2012 2017 2014 2015 2017 2008 2013
## [99425] 2013 2012 2015 2011 2007 2017 2017 2007 2007 2013 2013 2015 2015
## [99438] 2010 2008 2017 2013 2014 2017 2011 2007 2010 2015 2007 2013 2016
## [99451] 2011 2015 2016 2008 2009 2015 2010 2015 2012 2008 2015 2016 2011
## [99464] 2013 2015 2017 2017 2011 2014 2016 2009 2012 2016 2013 2016 2017
## [99477] 2010 2016 2016 2017 2012 2009 2009 2013 2016 2016 2016 2016 2016
## [99490] 2017 2016 2007 2015 2016 2008 2007 2016 2017 2008 2012 2016 2016
## [99503] 2015 2016 2008 2008 2012 2016 2016 2017 2011 2014 2016 2016 2014
## [99516] 2007 2012 2016 2017 2011 2016 2010 2011 2007 2011 2007 2016 2014
## [99529] 2011 2015 2011 2009 2011 2009 2013 2015 2011 2008 2009 2012 2017
## [99542] 2017 2008 2016 2016 2017 2017 2009 2009 2017 2007 2012 2017 2012
## [99555] 2017 2017 2017 2017 2017 2015 2016 2014 2016 2017 2015 2017 2010
## [99568] 2007 2016 2016 2008 2016 2017 2014 2015 2016 2009 2014 2010 2013
## [99581] 2013 2007 2017 2009 2015 2017 2016 2010 2014 2016 2017 2012 2009
## [99594] 2015 2017 2017 2011 2007 2010 2009 2007 2016 2017 2016 2014 2015
## [99607] 2015 2007 2007 2009 2014 2015 2008 2012 2012 2009 2008 2016 2017
## [99620] 2009 2009 2013 2016 2012 2008 2011 2010 2016 2017 2010 2007 2012
## [99633] 2016 2016 2010 2014 2017 2012 2011 2013 2013 2014 2016 2017 2015
## [99646] 2009 2008 2017 2010 2015 2015 2017 2017 2017 2008 2016 2015 2017
## [99659] 2015 2017 2017 2016 2016 2017 2010 2014 2008 2009 2015 2017 2015
## [99672] 2010 2009 2016 2016 2016 2016 2015 2008 2013 2017 2007 2008 2015
## [99685] 2016 2014 2017 2007 2015 2009 2012 2017 2013 2016 2016 2016 2009
## [99698] 2009 2013 2012 2008 2014 2014 2016 2016 2015 2015 2009 2008 2009
## [99711] 2014 2016 2017 2017 2017 2011 2017 2009 2008 2009 2012 2016 2009
## [99724] 2015 2016 2016 2017 2016 2016 2017 2016 2017 2008 2016 2010 2014
## [99737] 2015 2013 2017 2010 2010 2008 2016 2007 2008 2016 2014 2012 2016
## [99750] 2015 2015 2016 2016 2016 2017 2015 2017 2016 2016 2009 2008 2012
## [99763] 2016 2017 2017 2009 2010 2017 2017 2011 2011 2015 2015 2015 2015
## [99776] 2015 2016 2016 2017 2009 2014 2015 2016 2013 2017 2012 2012 2007
## [99789] 2016 2017 2013 2013 2016 2011 2015 2016 2016 2017 2016 2015 2016
## [99802] 2017 2017 2016 2011 2013 2015 2016 2016 2016 2009 2007 2017 2012
## [99815] 2014 2012 2014 2017 2012 2009 2011 2008 2016 2017 2009 2015 2016
## [99828] 2016 2010 2010 2015 2017 2016 2009 2012 2008 2010 2014 2017 2015
## [99841] 2010 2011 2015 2008 2017 2015 2017 2010 2008 2008 2011 2009 2017
## [99854] 2016 2016 2013 2013 2016 2009 2011 2008 2016 2016 2008 2013 2007
## [99867] 2015 2007 2013 2016 2016 2010 2017 2013 2015 2016 2015 2017 2007
## [99880] 2017 2016 2016 2008 2017 2016 2007 2011 2016 2016 2010 2016 2008
## [99893] 2007 2017 2016 2017 2015 2016 2010 2014 2014 2016 2017 2012 2008
## [99906] 2012 2016 2017 2012 2008 2017 2015 2016 2017 2009 2015 2017 2007
## [99919] 2016 2013 2015 2015 2014 2017 2015 2016 2016 2008 2015 2015 2010
## [99932] 2016 2010 2015 2014 2016 2016 2015 2012 2015 2017 2008 2013 2015
## [99945] 2016 2017 2015 2016 2017 2011 2011 2010 2015 2016 2011 2016 2016
## [99958] 2016 2017 2008 2011 2010 2009 2010 2017 2011 2009 2015 2016 2008
## [99971] 2014 2016 2016 2017 2017 2009 2009 2007 2012 2016 2017 2012 2016
## [99984] 2016 2016 2007 2016 2010 2010 2014 2017 2007 2016 2017 2008 2015
## [99997] 2016 2007 2016
##  [ reached getOption("max.print") -- omitted 15969890 entries ]
```

```r
summary(def_all$qCRais[def_all$Dur.Simple=="(~2 years+]" & def_all$StartFY==2015 & def_all$Ceil.Simple=="0k - <100k"])
```

```
## Warning: Unknown or uninitialised column: 'qCRais'.
```

```
## Length  Class   Mode 
##      0   NULL   NULL
```

```r
# def_all[def_all$Dur.Simple=="(~2 years+]" & def_all$StartFY==2015 & def_all$Ceil.Simple=="0k - <100k"]
write.csv(def_all[def_all$Dur.Simple=="(~2 years+]" & def_all$StartFY==2015 & def_all$Ceil.Simple=="0k - <100k",],"cbre_weird.csv")
```

```
## Warning: Factor `qHighCeiling` contains implicit NA, consider using
## `forcats::fct_explicit_na`
```
## Dur.Simple / Ceiling.1m

```r
df.QCrai.SDur<-only_complete(def_all) %>%
      group_by(StartFY,
               Ceil.1m,
               Dur.Simple) %>%
      dplyr::summarise(
      X50 = quantile(p_CBre, probs = 0.5,na.rm=TRUE),
      X75 = quantile(p_CBre, probs = 0.75,na.rm=TRUE), 
      X80 = quantile(p_CBre, probs = 0.80,na.rm=TRUE), 
      X90 = quantile(p_CBre, probs = 0.90,na.rm=TRUE), 
      X95 = quantile(p_CBre, probs = 0.95,na.rm=TRUE),
      X99 = quantile(p_CBre, probs = 0.99,na.rm=TRUE),
      ContractCount=length(CSIScontractID),
             Action_Obligation_OMB20_GDP18=sum(Action_Obligation_OMB20_GDP18),
      metric="Contracts within Period")
```

```
## Warning: Factor `Dur.Simple` contains implicit NA, consider using
## `forcats::fct_explicit_na`
```

```r
df.QCrai.SDur<-rbind(df.QCrai.SDur,
                all_labeled(def_all)%>%
      group_by(StartFY,
               Ceil.1m,
               Dur.Simple)%>%
      dplyr::summarise( 
      X50 = quantile(p_CBre, probs = 0.5,na.rm=TRUE),
      X75 = quantile(p_CBre, probs = 0.75,na.rm=TRUE), 
      X80 = quantile(p_CBre, probs = 0.80,na.rm=TRUE), 
      X90 = quantile(p_CBre, probs = 0.90,na.rm=TRUE), 
      X95 = quantile(p_CBre, probs = 0.95,na.rm=TRUE),
      X99 = quantile(p_CBre, probs = 0.99,na.rm=TRUE),
      ContractCount=length(CSIScontractID),
             Action_Obligation_OMB20_GDP18=sum(Action_Obligation_OMB20_GDP18),
      metric="Early Results for All Contracts")
)
```

```
## Warning: Factor `Dur.Simple` contains implicit NA, consider using
## `forcats::fct_explicit_na`
```

```r
df.QCrai.SDur<-melt(df.QCrai.SDur,
                      variable.name="Quantile",value.name="pCRai",measure.vars=c(
  "X50",
  "X75",
  "X80",
  "X90",
  "X95",
  "X99")
)

df.QCrai.SDur$Quantile<-factor(df.QCrai.SDur$Quantile,
  levels=c("X50",
  "X75",
  "X80",
  "X90",
  "X95",
  "X99"),
  labels=c("50th Percentile",
  "75th Percentile",
  "80th Percentile",
  "90th Percentile",
  "95th Percentile",
  "99th Percentile")
)

CRaiSDurCeilLabels<-ddply(
  subset(df.QCrai.SDur,Quantile=="50th Percentile" &
           metric=="Contracts within Period"),
    .(Dur.Simple,Ceil.1m),
    plyr::summarise,
    FacetCount=paste("Count:",prettyNum(sum(ContractCount),big.mark=",")),
    FacetValue=paste(FacetCount,"\nObligated: $",round(sum(Action_Obligation_OMB20_GDP18)/1000000000,1),"B",sep="")
    )

Ypos<-max(subset(df.QCrai.SDur,
                   !Quantile %in% c("99th Percentile")
                 )$pCRai,na.rm=TRUE)


CRaiOutput<-ggplot(subset(df.QCrai.SDur,
                !Quantile %in% c("99th Percentile",
                                 "75th Percentile")),
       aes(x=StartFY,y=pCRai,color=Quantile))+
  geom_line(aes(linetype=metric))+
  geom_point(aes(shape=Quantile))+
  geom_text(data=CRaiSDurCeilLabels,
              aes(x=2007,y=Ypos,label=FacetValue),
              # parse=TRUE,
              hjust=0,
              vjust=1,
              color="black")+
  facet_grid(Dur.Simple~Ceil.1m)+
               scale_y_continuous("Cost-Ceiling-Raising Change Orders Percent (Current $ Value)",
                                  labels=percent)+
  scale_x_continuous("Contract Starting Fiscal Year")+
  scale_linetype_discrete("Early Results")+
  theme(legend.position="bottom") #, position=pd

CRaiOutput
```

![](Start_Year_Outcomes_files/figure-html/QuantileSimpleDurCeil.1m-1.png)<!-- -->

```r
ggsave("CRaiOutput.png",
       CRaiOutput,
       width=8,
       height=7,
       dpi=600)

ggplot(subset(df.QCrai.SDur,
                # !Quantile %in% c("99th Percentile")
                !Ceil.1m %in% c("15k - <100k","0 - <15k")
              ),
       aes(x=StartFY,
           y=pCRai,
           color=Quantile))+
  geom_line(aes(linetype=metric))+
  facet_grid(Ceil.1m~Dur.Simple,
             scales="free_y",
             space="free_y")+
  scale_y_continuous(labels=percent)
```

![](Start_Year_Outcomes_files/figure-html/QuantileSimpleDurCeil.1m-2.png)<!-- -->

```r
#Test to see which percentiles register at all.
df.ecdf<-ddply(def_all,
      .(Ceil.1m,
               Dur.Simple),
      summarise, 
      r001 = ecdf(p_CBre)(0.001),
      r01 = ecdf(p_CBre)(0.01),
      r05 = ecdf(p_CBre)(0.01)
)

# df.ecdf<-subset(df.ecdf,StartFY>=2007&StartFY<=2014)


CRaiSDurCeilFYearSummary<-ddply(
  subset(df.QCrai.SDur,Quantile=="50th Percentile" &
           metric=="Contracts within Period"),
    .(Dur.Simple,Ceil.1m,StartFY),
    plyr::summarise,
    FacetCount=paste("Count:",prettyNum(sum(ContractCount),big.mark=",")),
    FacetValue=paste(FacetCount,"\nObligated: $",round(sum(Action_Obligation_OMB20_GDP18)/1000000000,1),"B",sep="")
    )

DurBoundary<-subset(def_all,Ceil=="75m+"&
         qDuration=="(~2 years+]"&
         StartFY==2013&
         UnmodifiedCurrentCompletionDate<as.Date("2015-09-30")
         )
```

```
## Warning: Factor `qHighCeiling` contains implicit NA, consider using
## `forcats::fct_explicit_na`
```

```r
# View(subset(def_all,Ceil.Big=="75m+" & Dur.Simple=="(~2 years+]" & StartFY==2014))

# write.csv(subset(def_all,Ceil.Big=="75m+" & Dur.Simple=="(~2 years+]" & StartFY==2014),"Long2014.csv")
```
