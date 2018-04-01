---
title: "Monopoly Results Summary"
date: "Wednesday, February 8, 2017"
output: 
  html_document: 
    keep_md: yes
    toc: yes

---

#Setup

```
## 
## Attaching package: 'dplyr'
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
## Loading required package: MASS
```

```
## 
## Attaching package: 'MASS'
```

```
## The following object is masked from 'package:dplyr':
## 
##     select
```

```
## Loading required package: Matrix
```

```
## Loading required package: lme4
```

```
## 
## arm (Version 1.9-3, built: 2016-11-21)
```

```
## Working directory is H:/Users/Greg/Documents/Repositories/Vendor
```

```
## Loading required package: coda
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


###Data Transformations and Summary




```r
  load(file="Data/defense_contract_CSIScontractID_detail.Rdata")
  

head(DefenseModelAndDetail)
```

```
##   CSIScontractID                 FxCb                      Fee Comp
## 1        5261947          Fixed-Price            FFP or No Fee    1
## 2       63603967          Fixed-Price Combination or Other Fee    1
## 3       22544223 Combination or Other Combination or Other Fee    0
## 4        9334467          Fixed-Price            FFP or No Fee    1
## 5       61736309          Fixed-Price            FFP or No Fee    1
## 6       22071327          Fixed-Price            FFP or No Fee    0
##             Who                        What      Intl      PSR
## 1     Other DoD Facilities and Construction Just U.S. Products
## 2 Uncategorized                       Other Unlabeled Products
## 3     Other DoD         Aircraft and Drones Just U.S. Products
## 4          Army Facilities and Construction Just U.S. Services
## 5     Other DoD                       Other Just U.S. Products
## 6     Other DoD         Aircraft and Drones Just U.S. Products
##               LowCeil                Ceil             Dur SingleOffer
## 1 [0.00e+00,1.50e+04) [0.00e+00,1.50e+04) [-43558,    61)       Multi
## 2 [0.00e+00,1.50e+04) [0.00e+00,1.50e+04) [-43558,    61)       Multi
## 3 [0.00e+00,1.50e+04) [0.00e+00,1.50e+04) [   214,   366)      Single
## 4 [1.50e+04,1.00e+05) [1.50e+04,1.00e+05) [    61,   214)       Multi
## 5 [0.00e+00,1.50e+04) [0.00e+00,1.50e+04) [-43558,    61)       Multi
## 6 [0.00e+00,1.50e+04) [0.00e+00,1.50e+04) [   366,   732)      Single
##        Offr     UCA            CRai NChg          Veh
## 1 [  3,  5) Not UCA [-0.001, 0.001)    0      Def/Pur
## 2 [  3,  5)    <NA> [-0.001, 0.001)    0         <NA>
## 3         1 Not UCA [-0.001, 0.001)    0 SINGLE AWARD
## 4 [  3,  5) Not UCA [-0.001, 0.001)    0 SINGLE AWARD
## 5 [  5,999]    <NA> [-0.001, 0.001)    0 SINGLE AWARD
## 6         1 Not UCA [-0.001, 0.001)    0 SINGLE AWARD
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
##   Action.Obligation StartFY Agency topContractingOfficeID ProdServ  NAICS
## 1           6500.00    2011   97AS                 SPMYM2     3405 333991
## 2           3469.78    2016   97AS                 SPE300     8940 311812
## 3           7687.00    2009   97AS                 SPM740     2590 333911
## 4          22000.00    2010   2100                 W9126G     S299 561730
## 5            778.72    2015   97AS                 SPM200     6505 424210
## 6           4406.00    2009   97AS                 SPM400     1560 336411
```

```r
# undebug(transform_contract)
# debug(transform_contract)
# debug(transform_contract)
DefenseModelAndDetail<-transform_contract(DefenseModelAndDetail)
```

```
## Warning in log(contract$UnmodifiedDays): NaNs produced
```


DefenseModelAndDetails$


NA_stats(DefenseModelAndDetail,"b_Crai")

* b_Crai is a binary variable that has a value of 1 for any contracts that have experienced a partial or complete termination, 0 otherwise. ('r summary(DefenseModelAndDetail$b_Crai)
NA_stats(DefenseModelAndDetail,"b_Crai"))
* b_Comp is a binary variable based on competition. It has a value of 0 for uncompeted contracts and a value of 1 for competition, regardless of number of offers. 'r  summary(DefenseModelAndDetail$b_Comp);
NA_stats(DefenseModelAndDetail,"b_Comp")'
* c_hh_index_lag1 is a numeric variable based on competition and number of offers. It has a value of 0 for uncompeted contracts and a value of 1 for single-offer competition, and a value of 2 for multi-offer competition. 'r summary(DefenseModelAndDetail$c_hh_index_lag1);
NA_stats(DefenseModelAndDetail,"c_hh_index_lag1")'
* n_Offr is a numeric variable based on competition and with more granularity on number offers. It has a value of 0 for uncompeted contracts and a value of 1 for single-offer competition, and a value of 2 for 2-offers, 3 for 3-4 offers, and 4 for 5+ offers. 'r summary(DefenseModelAndDetail$n_Offr);
NA_stats(DefenseModelAndDetail,"n_Offr")'
* hh_index_lag1 is the natural log of the number of offers received. Uncompeted contracts are classified as having received one offer. 'r summary(DefenseModelAndDetail$hh_index_lag1);
NA_stats(DefenseModelAndDetail,"hh_index_lag1")'
* cl_Ceil is the natural log of the initial contract cost ceiling, in then-year dollars. 'r centered_log_description(DefenseModelAndDetail$l_Ceil,"dollars");
NA_stats(DefenseModelAndDetail,"l_Ceil")'
* cl_Days is the natural log of the initial maximum duration of the contract, in days. The variable is centered, by subtracting its mean ('r centered_log_description(DefenseModelAndDetail$l_Days,"days");
NA_stats(DefenseModelAndDetail,"l_Days")' 
* SIDV, MIDV, and OIDV are dummy variables based on the contract vehicle. They correspond to Single-Award IDVs, Multi-Award IDVs, and Other IDVs, respectively, having a value of 1 when the task order has that vehicle type, and having a 0 other. The remaining types, definitive contracts and purchase orders, are intentionally left out. ('r summary(DefenseModelAndDetail$SIDV);
summary(DefenseModelAndDetail$MIDV);
summary(DefenseModelAndDetail$OIDV);
NA_stats(DefenseModelAndDetail,"Veh")'
* n_Fixed is a numeric variable based on contract pricing. It has a value of 0 for cost-based, 0.5 or "combination or other", 1 for any fixed price (excluding fixed-price level of effort which is classified as cost-based). ('r summary(DefenseModelAndDetail$n_Fixed);
NA_stats(DefenseModelAndDetail,"n_Fixed")')
* n_Incent is a numeric variable based on fee type. Ig has a value. 1 for incentive fee or cost sharing, 0.5 or "combination or other", 0 for all remaining types. ('r summary(DefenseModelAndDetail$n_Incent);
NA_stats(DefenseModelAndDetail,"n_Incent")')
* b_UCA is a binary variable with a value of 1 for contracts/task orders that begin as letter contracts or undefinitized contract awards (UCA) and a value of 0 otherwise. 'r
summary(DefenseModelAndDetail$b_UCA);
NA_stats(DefenseModelAndDetail,"b_UCA")'
* b_Intl is a binary variable with a value of 1 for contracts/task orders with any transactions performed internationally and a value of 0 otherwise. 'r
summary(DefenseModelAndDetail$b_Intl);
NA_stats(DefenseModelAndDetail,"b_Intl")'
* PSR is a factor reporting whether a contract is primarily for products/services/R&D. 'r
summary(DefenseModelAndDetail$PSR);
NA_stats(DefenseModelAndDetail,"PSR")'
* ProdServ is a factor reporting the top Product or Service Code of each contract. 'r
summary(DefenseModelAndDetail$ProdServ);
NA_stats(DefenseModelAndDetail,"ProdServ")'
* NAICS is a factor reporting the top North American Industrial Classification Code of each contract. 'r
summary(DefenseModelAndDetail$NAICS);
NA_stats(DefenseModelAndDetail,"NAICS")'
* Agency is a factor reporting the top Contracting Agency of each contract. 'r
summary(DefenseModelAndDetail$Agency);
NA_stats(DefenseModelAndDetail,"Agency")'
* Office is a factor reporting the top Contracting office of each contract. 'r
summary(DefenseModelAndDetail$Office);
NA_stats(DefenseModelAndDetail,"Office")'

