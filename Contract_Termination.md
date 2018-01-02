# ContractTermination
Greg Sanders  
Wednesday, February 8, 2017  

Modeling Likelihood of Contract Termination
============================================================================

#Setup

```r
library(csis360)
library(ggplot2)
library(scales)
library(Hmisc)
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
## The following objects are masked from 'package:base':
## 
##     format.pval, round.POSIXt, trunc.POSIXt, units
```

```r
library(plyr)
```

```
## 
## Attaching package: 'plyr'
```

```
## The following objects are masked from 'package:Hmisc':
## 
##     is.discrete, summarize
```

```r
library(arm)
```

```
## Warning: package 'arm' was built under R version 3.4.3
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
## Warning: package 'lme4' was built under R version 3.4.3
```

```
## 
## arm (Version 1.9-3, built: 2016-11-21)
```

```
## Working directory is C:/Users/gsand_000.ALPHONSE/Documents/Development/Vendor
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

```r
library(R2WinBUGS)
```

```
## Warning: package 'R2WinBUGS' was built under R version 3.4.3
```

```
## Loading required package: coda
```

```
## Warning: package 'coda' was built under R version 3.4.3
```

```
## 
## Attaching package: 'coda'
```

```
## The following object is masked from 'package:arm':
## 
##     traceplot
```

```
## Loading required package: boot
```

```
## 
## Attaching package: 'boot'
```

```
## The following object is masked from 'package:arm':
## 
##     logit
```

```
## The following object is masked from 'package:survival':
## 
##     aml
```

```
## The following object is masked from 'package:lattice':
## 
##     melanoma
```

```r
library(lme4)

axis.text.size<-10
strip.text.size<-10
legend.text.size<-8
# table.text.size<-5.75
title.text.size<-12
geom.text.size<-12

main.text.size<-1
note.text.size<-1.40
```

Contracts are classified using a mix of numerical and categorical variables. While the changes in numerical variables are easy to grasp and summarize, a contract may have one line item that is competed and another that is not. As is detailed in the exploration on R&D, we are only considering information available prior to contract start. The percentage of contract obligations that were competed is a valuable benchmark, but is highly influenced by factors that occured after contract start..

## Contract Terminations

Contract terminations and the number of change orders can be calculated for the entire sample.  Contract termination is determined using the *Reason for Modification* field in FPDS.  A contract is considered to be terminated if it has at least one modification with the following values:

* "Terminate for Default (complete or partial)"
* "Terminate for Convenience (complete or partial)"
* "Terminate for Cause"
* "Legal Contract Cancellation"

These four catetegories and the "Close Out" category are used to mark a contract as closed.  Many contracts in FPDS and in the sample are never marked closed.  


##Prepare Data
First we load the data. The dataset used is a U.S. Defense Contracting dataset derived from   FPDS.


```r
  load(file="Data/defense_contract_CSIScontractID_detail.Rdata")
  

head(DefenseModelAndDetail)
```

```
##   CSIScontractID                 FxCb                      Fee
## 1        5261947          Fixed-Price            FFP or No Fee
## 2       63603967          Fixed-Price Combination or Other Fee
## 3       22544223 Combination or Other Combination or Other Fee
## 4        9334467          Fixed-Price            FFP or No Fee
## 5       61736309          Fixed-Price            FFP or No Fee
## 6       22071327          Fixed-Price            FFP or No Fee
##             Who                        What      Intl      PSR
## 1     Other DoD Facilities and Construction Just U.S. Products
## 2 Uncategorized                       Other Unlabeled Products
## 3     Other DoD         Aircraft and Drones Just U.S. Products
## 4          Army Facilities and Construction Just U.S. Services
## 5     Other DoD                       Other Just U.S. Products
## 6     Other DoD         Aircraft and Drones Just U.S. Products
##               LowCeil                Ceil             Dur SingleOffer Offr
## 1 [0.00e+00,1.50e+04) [0.00e+00,1.50e+04) [-43558,    61)       Multi  3-4
## 2 [0.00e+00,1.50e+04) [0.00e+00,1.50e+04) [-43558,    61)       Multi  3-4
## 3 [0.00e+00,1.50e+04) [0.00e+00,1.50e+04) [   214,   366)      Single    1
## 4 [1.50e+04,1.00e+05) [1.50e+04,1.00e+05) [    61,   214)       Multi  3-4
## 5 [0.00e+00,1.50e+04) [0.00e+00,1.50e+04) [-43558,    61)       Multi   5+
## 6 [0.00e+00,1.50e+04) [0.00e+00,1.50e+04) [   366,   732)      Single    1
##       UCA            CRai NChg          Veh
## 1 Not UCA [-0.001, 0.001)    0      Def/Pur
## 2    <NA> [-0.001, 0.001)    0         <NA>
## 3 Not UCA [-0.001, 0.001)    0 SINGLE AWARD
## 4 Not UCA [-0.001, 0.001)    0 SINGLE AWARD
## 5    <NA> [-0.001, 0.001)    0 SINGLE AWARD
## 6 Not UCA [-0.001, 0.001)    0 SINGLE AWARD
##   UnmodifiedNumberOfOffersReceived         Term
## 1                                4 Unterminated
## 2                                3 Unterminated
## 3                                1 Unterminated
## 4                                3 Unterminated
## 5                                5 Unterminated
## 6                                1 Unterminated
##   UnmodifiedContractBaseAndAllOptionsValue SumOfisChangeOrder
## 1                                  6500.00                  0
## 2                                  3469.78                  0
## 3                                  7687.00                  0
## 4                                 22000.00                  0
## 5                                   778.72                  0
## 6                                  4406.00                  0
##   pChangeOrderUnmodifiedBaseAndAll UnmodifiedDays MinOfEffectiveDate
## 1                                0       31.00000         2011-09-29
## 2                                0        3.00000         2015-12-07
## 3                                0      271.00000         2008-10-23
## 4                                0       63.04167         2010-09-19
## 5                                0        1.00000         2014-12-18
## 6                                0      468.00000         2009-05-06
##   Action.Obligation
## 1           6500.00
## 2           3469.78
## 3           7687.00
## 4          22000.00
## 5            778.72
## 6           4406.00
```

###Data Transformations
After the data is loaded, we perform a series of transformations, creating derived variables that fit our purposes.

* lCeil is the natural log of the initial contract cost ceiling (UnmodifiedContractBaseAndAllOptionsValue).
* lDays is the natural log of the initial maximum contract duration in days (UnmodifiedDays)
* nTerm transforms the factor that reports whether a contract was terminated (Term) to a numerical variable, 0 for no terminations, 1 for any partial or complete terminations.
* nFixed transforms the factor that reports whether a contract was fixed price or cost-based (FxCB) to a numberical value. 0 for cost-based, 0.5 or "combination or other", 1 for any fixed price (excluding fixed-price level of effort which is classified as cost-based).
* nIncent transforms the factor that reports the fee type (Fee) to a numerical value. 1 for incentive fee or cost sharing, 0.5 or "combination or other", 0 for all remaining types
*  nComp transforms the factor that reports the fee type (Comp) to a numerical value. 1 for Competed, 0 for not competed.
*  nIntl transforms the factor that reports the location (Intl) to a numerical value. 1 for Any International obligations, 0 for All in U.S.
*  nIntl transforms the factor that reports the use of undefinitized contract awards (UCA) to a numerical value. 1 for UCA, 0 for otherwise.
*  lOffer is the natural log of the number of offers received. Uncompeted contracts are classified as having received one offer.


In addition, lCeil and lDays are rescaled to have an average value of 0 and a standard deviation of one. This standardizes interpretation and prevents a "very large eigen value" error later when lCeil:lDay is introduced.


```r
#lCeil
DefenseModelAndDetail$lCeil<-log(DefenseModelAndDetail$UnmodifiedContractBaseAndAllOptionsValue)
DefenseModelAndDetail$lCeil[is.infinite(DefenseModelAndDetail$lCeil)]<-NA

#lDays
DefenseModelAndDetail$lDays<-log(DefenseModelAndDetail$UnmodifiedDays)
```

```
## Warning in log(DefenseModelAndDetail$UnmodifiedDays): NaNs produced
```

```r
DefenseModelAndDetail$lDays[is.infinite(DefenseModelAndDetail$lDays)]<-NA

#nTerm
DefenseModelAndDetail$nTerm<-as.integer(as.character(factor(DefenseModelAndDetail$Term,
                                  levels=c("Terminated","Unterminated"),
                                  labels=c(1,0))))


#nFixed

DefenseModelAndDetail$nFixed<-DefenseModelAndDetail$FxCb
levels(DefenseModelAndDetail$nFixed)<- list("1"=c("Fixed-Price"), 
       "0.5"=c("Combination or Other"),
       "0"=c("Cost-Based"))
DefenseModelAndDetail$nFixed<-as.integer(as.character(DefenseModelAndDetail$nFixed))

 #nIncent
DefenseModelAndDetail$nIncent<-DefenseModelAndDetail$Fee
levels(DefenseModelAndDetail$nIncent) <-
  list("1"=c("Incentive"), 
       "0.5"=c("Combination or Other"),
       "0"=c("Award Fee", "FFP or No Fee", "Fixed Fee"))
DefenseModelAndDetail$nIncent<-as.integer(as.character(DefenseModelAndDetail$nIncent))


#nComp
DefenseModelAndDetail$nComp<-DefenseModelAndDetail$SingleOffer #Fix in Rdata, and add back comp
levels(DefenseModelAndDetail$nComp)
```

```
## [1] "Single" "Multi"
```

```r
#nIntl
DefenseModelAndDetail$nIntl<-DefenseModelAndDetail$Intl
DefenseModelAndDetail$nIntl[DefenseModelAndDetail$nIntl=="Unlabeled"]<-NA
levels(DefenseModelAndDetail$nIntl) <-
  list("0"=c("Just U.S."), 
       "1"=c("Any International"))
DefenseModelAndDetail$nIntl<-as.integer(as.character(DefenseModelAndDetail$nIntl))



#nUCA
DefenseModelAndDetail$nUCA<-DefenseModelAndDetail$UCA
levels(DefenseModelAndDetail$nUCA) <-
  list("0"=c("Not UCA"), 
       "1"=c("UCA"))
DefenseModelAndDetail$nUCA<-as.integer(as.character(DefenseModelAndDetail$nUCA))
DefenseModelAndDetail$nUCA[is.na(DefenseModelAndDetail$nUCA)]<-0



#lOffer
DefenseModelAndDetail$lOffer<-log(DefenseModelAndDetail$UnmodifiedNumberOfOffersReceived)
DefenseModelAndDetail$lOffer[is.infinite(DefenseModelAndDetail$lDays)]<-NA

# DefenseModelAndDetail$DecisionTree<-as.character(DefenseModelAndDetail$MaxOfDecisionTree)
# DefenseModelAndDetail$DecisionTree[
#   DefenseModelAndDetail$DecisionTree=="Excluded"|
#     is.na(DefenseModelAndDetail$DecisionTree)]<-"All Other"
# DefenseModelAndDetail$DecisionTree<-factor(DefenseModelAndDetail$DecisionTree,levels=c("OCO","Disaster","ARRA","All Other"))
# 
# if(!"Is.Defense" %in% colnames(DefenseModelAndDetail)){
#   DefenseModelAndDetail$Is.Defense<-as.character(DefenseModelAndDetail$Who)
#   DefenseModelAndDetail$Is.Defense[DefenseModelAndDetail$Is.Defense %in%
#                                      c("Air Force","Army",
#                                        "Navy","Other DoD","Uncategorized"  )
#                                    ]<-"Defense"
#   DefenseModelAndDetail$Is.Defense<-factor(DefenseModelAndDetail$Is.Defense)
# }
```

Next, we eliminate missing data entries and then draw a sample. The final computation uses all of the data, but as a computation shortcut, only a subset of the data is needed to allow for processing of models in minutes rather than hours.


```r
smp<-DefenseModelAndDetail[!is.na(DefenseModelAndDetail$nTerm),]

smp[,names(smp) %in% c("lCeil","lDays")]<-scale(smp[,names(smp) %in% c("lCeil","lDays")])


smp<-smp[sample(nrow(smp),250000),]
# detach(2)
R2WinBUGS::attach.all(smp)
```

#Estimate Model

##Model 1: Cost Ceiling

The model starts by examing the influence of large ceilings on contracts, the core idea here, as observed in the fixed priced paper, is that larger contracts are at greater risk of termination. This can be explained by both the inherent risk in the contract and high transaction costs of contract termination which discourage making the effort for smaller contracts.


```r
M1 <- glmer (nTerm ~ lCeil +
               (1 | PSR) + (1 | What) + (1 | Veh) + (1 | Who), family=binomial(link="logit"))
display(M1)
```

```
## glmer(formula = nTerm ~ lCeil + (1 | PSR) + (1 | What) + (1 | 
##     Veh) + (1 | Who), family = binomial(link = "logit"))
##             coef.est coef.se
## (Intercept) -4.95     0.57  
## lCeil        0.42     0.03  
## 
## Error terms:
##  Groups   Name        Std.Dev.
##  What     (Intercept) 0.49    
##  Who      (Intercept) 0.20    
##  Veh      (Intercept) 0.47    
##  PSR      (Intercept) 0.80    
##  Residual             1.00    
## ---
## number of obs: 186835, groups: What, 9; Who, 5; Veh, 4; PSR, 3
## AIC = 16807.5, DIC = 16648
## deviance = 16721.8
```

##Model 2: Maximum Duration


```r
M2 <- glmer (nTerm ~ lCeil + 
               lDays  +
               (1 | PSR)  + (1 | What) + (1 | Veh) + (1 | Who), family=binomial(link="logit"))
display(M2)
```

```
## glmer(formula = nTerm ~ lCeil + lDays + (1 | PSR) + (1 | What) + 
##     (1 | Veh) + (1 | Who), family = binomial(link = "logit"))
##             coef.est coef.se
## (Intercept) -5.59     0.61  
## lCeil        0.19     0.04  
## lDays        0.73     0.04  
## 
## Error terms:
##  Groups   Name        Std.Dev.
##  What     (Intercept) 0.41    
##  Who      (Intercept) 0.22    
##  Veh      (Intercept) 0.38    
##  PSR      (Intercept) 0.94    
##  Residual             1.00    
## ---
## number of obs: 185239, groups: What, 9; Who, 5; Veh, 4; PSR, 3
## AIC = 16201.1, DIC = 16045
## deviance = 16116.1
```
The initial model results are as expected, but the relationship between initial contract ceiling and terminations is weak compared to what was observed in data exploration. This may be because of the interaction between the two terms. Both initial ceiling and duration are seperately associated with a higher risk of termination, but what is the influence of contracts that are expected to be both long lasting and large?

##Model 3: Interation of Ceiling and Duration

```r
M3 <- glmer (nTerm ~ lCeil + 
               lDays + lCeil:lDays +
               (1 | PSR)  + (1 | What) + (1 | Veh) + (1 | Who), family=binomial(link="logit"))
display(M3)
```

```
## glmer(formula = nTerm ~ lCeil + lDays + lCeil:lDays + (1 | PSR) + 
##     (1 | What) + (1 | Veh) + (1 | Who), family = binomial(link = "logit"))
##             coef.est coef.se
## (Intercept) -5.60     0.61  
## lCeil        0.25     0.06  
## lDays        0.77     0.05  
## lCeil:lDays -0.06     0.04  
## 
## Error terms:
##  Groups   Name        Std.Dev.
##  What     (Intercept) 0.40    
##  Who      (Intercept) 0.21    
##  Veh      (Intercept) 0.37    
##  PSR      (Intercept) 0.92    
##  Residual             1.00    
## ---
## number of obs: 185239, groups: What, 9; Who, 5; Veh, 4; PSR, 3
## AIC = 16201, DIC = 16043.8
## deviance = 16114.4
```

The combination of ceiling and duration improves the model only very slightly and is not significant in its own right. We will be leaving it out going forward.

##Model 4: Fixed-Price Contracts

The next step adds a measure for whether the contract was cost-based or fixed-price. Prior CSIS research has found that fixed-price contracts do face a higher risk of termination across the board.


```r
M4 <- glmer (nTerm ~ lCeil + 
               lDays + nFixed +
               (1 | PSR)  + (1 | What) + (1 | Veh) + (1 | Who), family=binomial(link="logit"))
display(M4)
```

```
## glmer(formula = nTerm ~ lCeil + lDays + nFixed + (1 | PSR) + 
##     (1 | What) + (1 | Veh) + (1 | Who), family = binomial(link = "logit"))
##             coef.est coef.se
## (Intercept) -6.69     0.52  
## lCeil        0.23     0.04  
## lDays        0.74     0.04  
## nFixed       1.31     0.20  
## 
## Error terms:
##  Groups   Name        Std.Dev.
##  What     (Intercept) 0.41    
##  Who      (Intercept) 0.21    
##  Veh      (Intercept) 0.34    
##  PSR      (Intercept) 0.70    
##  Residual             1.00    
## ---
## number of obs: 185238, groups: What, 9; Who, 5; Veh, 4; PSR, 3
## AIC = 16144.4, DIC = 15991.7
## deviance = 16060.0
```

And fixed price contracts do indeed have a appreciably higher termination rate. This does not improve the overall AIC of the model, but it captures another important dynamic and has a coefficient that greatly exceeds its standard error. 

##Model 5: Fixed-Price and Maximum Duration
Past CSIS research has found that fixed-price contracts do appear to be at higher risk if they have a longer maximum duration. Fixed-price contracting does require upfront estimation of likely costs, and thus a longer duration means more opportunity for changed circumstance.


```r
M11 <- glmer (nTerm ~ lCeil + 
               lDays + nFixed + nIntl + nUCA + lOffer + lDays:nFixed +
               (1 | PSR)  + (1 | What) + (1 | Veh) + (1 | Who), family=binomial(link="logit"))
display(M11)
```

```
## glmer(formula = nTerm ~ lCeil + lDays + nFixed + nIntl + nUCA + 
##     lOffer + lDays:nFixed + (1 | PSR) + (1 | What) + (1 | Veh) + 
##     (1 | Who), family = binomial(link = "logit"))
##              coef.est coef.se
## (Intercept)  -6.94     0.85  
## lCeil         0.19     0.04  
## lDays         0.72     0.38  
## nFixed        1.23     0.59  
## nIntl        -0.12     0.10  
## nUCA          1.41     0.16  
## lOffer        0.27     0.03  
## lDays:nFixed  0.03     0.38  
## 
## Error terms:
##  Groups   Name        Std.Dev.
##  What     (Intercept) 0.40    
##  Who      (Intercept) 0.25    
##  Veh      (Intercept) 0.36    
##  PSR      (Intercept) 0.90    
##  Residual             1.00    
## ---
## number of obs: 165661, groups: What, 9; Who, 4; Veh, 4; PSR, 3
## AIC = 15225.1, DIC = 15063.6
## deviance = 15132.4
```
No strong relationship is suggested by the coefficient and the direction is opposite the one expected. This suggests that both maximum duration and fixed price contracting are termination increasing factors, but that they operate independently.


##Model 6: Incentive Fees


```r
M5 <- glmer (nTerm ~ lCeil + 
               lDays + nFixed + nIncent +
               (1 | PSR)  + (1 | What) + (1 | Veh) + (1 | Who), family=binomial(link="logit"))
display(M5)
```

```
## glmer(formula = nTerm ~ lCeil + lDays + nFixed + nIncent + (1 | 
##     PSR) + (1 | What) + (1 | Veh) + (1 | Who), family = binomial(link = "logit"))
##             coef.est coef.se
## (Intercept) -6.66     0.65  
## lCeil        0.21     0.04  
## lDays        0.73     0.04  
## nFixed       1.17     0.26  
## nIncent      0.54     0.39  
## 
## Error terms:
##  Groups   Name        Std.Dev.
##  What     (Intercept) 0.42    
##  Who      (Intercept) 0.19    
##  Veh      (Intercept) 0.36    
##  PSR      (Intercept) 0.93    
##  Residual             1.00    
## ---
## number of obs: 165449, groups: What, 9; Who, 5; Veh, 4; PSR, 3
## AIC = 15509.1, DIC = 15353.4
## deviance = 15422.2
```
Fee type appears to have no effect on terminations. In the performance of the defense acquisition system report, the benefits found were reduced cost overruns. That could help avoid terminations, but it's an indirect connection at best. Nonetheless, because they are closely connected, it's worth testing the interaction with fixed price.


##Model 7: Fixed Price and Incentive Fee


```r
M6 <- glmer (nTerm ~ lCeil + 
               lDays + nFixed + nIncent + nFixed:nIncent +
               (1 | PSR)  + (1 | What) + (1 | Veh) + (1 | Who), family=binomial(link="logit"))
display(M6)
```

```
## glmer(formula = nTerm ~ lCeil + lDays + nFixed + nIncent + nFixed:nIncent + 
##     (1 | PSR) + (1 | What) + (1 | Veh) + (1 | Who), family = binomial(link = "logit"))
##                coef.est coef.se
## (Intercept)     -6.59     0.65 
## lCeil            0.21     0.04 
## lDays            0.73     0.04 
## nFixed           1.10     0.25 
## nIncent        -10.61     4.46 
## nFixed:nIncent  11.35     4.45 
## 
## Error terms:
##  Groups   Name        Std.Dev.
##  What     (Intercept) 0.42    
##  Who      (Intercept) 0.20    
##  Veh      (Intercept) 0.36    
##  PSR      (Intercept) 0.94    
##  Residual             1.00    
## ---
## number of obs: 165449, groups: What, 9; Who, 5; Veh, 4; PSR, 3
## AIC = 15508.2, DIC = 15350.3
## deviance = 15419.3
```

Adding the intersection of Fixed and Incent does not improve the model and results in a failure to converge. Leaving it out.

##Model 8: International work


```r
M7 <- glmer (nTerm ~ lCeil + 
               lDays + nFixed + nIntl +
               (1 | PSR)  + (1 | What) + (1 | Veh) + (1 | Who), family=binomial(link="logit"))
display(M7)
```

```
## glmer(formula = nTerm ~ lCeil + lDays + nFixed + nIntl + (1 | 
##     PSR) + (1 | What) + (1 | Veh) + (1 | Who), family = binomial(link = "logit"))
##             coef.est coef.se
## (Intercept) -6.59     0.53  
## lCeil        0.21     0.04  
## lDays        0.73     0.04  
## nFixed       1.30     0.20  
## nIntl       -0.13     0.10  
## 
## Error terms:
##  Groups   Name        Std.Dev.
##  What     (Intercept) 0.40    
##  Who      (Intercept) 0.22    
##  Veh      (Intercept) 0.34    
##  PSR      (Intercept) 0.70    
##  Residual             1.00    
## ---
## number of obs: 167699, groups: What, 9; Who, 4; Veh, 4; PSR, 3
## AIC = 15749.7, DIC = 15598.9
## deviance = 15665.3
```

The lower termination rate of international contrats is somewhat surprising, given their potentially greater risks. The coefficient exceeds the standard deviation, though missing data reduced the number of observations, reducing both the AIC and DIC. An alternative would be to treat all missing international labels as domestic. 

##Model 9: Undefinitized Contract Awards
Undefinitized Contract Awards allow for quick action in situations where there is not time or information to establish all of a contracts properties at the time of signing. They have been found by the GAO and the Performance of the Defense Acquisition studies to contain notable risks, primarily relating to cost overruns. Will these risks also carry into terminations?

Note that in this analysis, if UCA status is unlabeled, the study team assumes the contract to not be a UCA.


```r
M8 <- glmer (nTerm ~ lCeil + 
               lDays + nFixed + nIntl + nUCA +
               (1 | PSR)  + (1 | What) + (1 | Veh) + (1 | Who), family=binomial(link="logit"))
display(M8)
```

```
## glmer(formula = nTerm ~ lCeil + lDays + nFixed + nIntl + nUCA + 
##     (1 | PSR) + (1 | What) + (1 | Veh) + (1 | Who), family = binomial(link = "logit"))
##             coef.est coef.se
## (Intercept) -6.60     0.52  
## lCeil        0.20     0.04  
## lDays        0.71     0.04  
## nFixed       1.29     0.20  
## nIntl       -0.11     0.10  
## nUCA         1.27     0.16  
## 
## Error terms:
##  Groups   Name        Std.Dev.
##  What     (Intercept) 0.37    
##  Who      (Intercept) 0.22    
##  Veh      (Intercept) 0.35    
##  PSR      (Intercept) 0.70    
##  Residual             1.00    
## ---
## number of obs: 167699, groups: What, 9; Who, 4; Veh, 4; PSR, 3
## AIC = 15702.6, DIC = 15551.4
## deviance = 15617.0
```

Use of UCA significantly increases risks of termination, well in excess of the error term. Interestingly, while this addition further reduceds the AIC and DIC of the model, it also increases the coefficient for fixed price contracts.

##Model 10: Competition

##Model 11: Number of offers


```r
M10 <- glmer (nTerm ~ lCeil + 
               lDays + nFixed + nIntl + nUCA + lOffer +
               (1 | PSR)  + (1 | What) + (1 | Veh) + (1 | Who), family=binomial(link="logit"))
display(M10)
```

```
## glmer(formula = nTerm ~ lCeil + lDays + nFixed + nIntl + nUCA + 
##     lOffer + (1 | PSR) + (1 | What) + (1 | Veh) + (1 | Who), 
##     family = binomial(link = "logit"))
##             coef.est coef.se
## (Intercept) -6.98     0.63  
## lCeil        0.19     0.04  
## lDays        0.74     0.04  
## nFixed       1.27     0.20  
## nIntl       -0.12     0.10  
## nUCA         1.41     0.16  
## lOffer       0.27     0.03  
## 
## Error terms:
##  Groups   Name        Std.Dev.
##  What     (Intercept) 0.40    
##  Who      (Intercept) 0.25    
##  Veh      (Intercept) 0.36    
##  PSR      (Intercept) 0.90    
##  Residual             1.00    
## ---
## number of obs: 165661, groups: What, 9; Who, 4; Veh, 4; PSR, 3
## AIC = 15223.1, DIC = 15063.6
## deviance = 15132.4
```

The finding that contracts that receive more offers have a higher risk of termination is surprising. Bid protests, which can result in a partial or complete terminations, may be one driver. Another may be that when multiple offers are available, there are more alternatives for the government to turn to rather than being stuck in a contract.
