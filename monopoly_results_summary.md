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
#   load(file="Data/defense_contract_CSIScontractID_detail.Rdata")
# head(DefenseModelAndDetail)
# DefenseModelAndDetail<-transform_contract(DefenseModelAndDetail)
# save(file="Data/transformed_def.Rdata", DefenseModelAndDetail)
 load(file="Data/transformed_def.Rdata")
```


DefenseModelAndDetails$


NA_stats(DefenseModelAndDetail,"b_Crai")

* b_Crai is a binary variable that has a value of 1 for any contracts that have experienced a partial or complete termination, 0 otherwise. (Data is missing for 0.0116 of records and 0.171 of obligated dollars.)
* b_Comp is a binary variable based on competition. It has a value of 0 for uncompeted contracts and a value of 1 for competition, regardless of number of offers. Data is missing for 0.00105 of records and 0.00122 of obligated dollars.
* c_hh_index_lag1 is a numeric variable based on competition and number of offers. It has a value of 0 for uncompeted contracts and a value of 1 for single-offer competition, and a value of 2 for multi-offer competition. Data is missing for 0.0363 of records and 0.0501 of obligated dollars.
* n_Offr is a numeric variable based on competition and with more granularity on number offers. It has a value of 0 for uncompeted contracts and a value of 1 for single-offer competition, and a value of 2 for 2-offers, 3 for 3-4 offers, and 4 for 5+ offers. Data is missing for 0.00653 of records and 0.00362 of obligated dollars.
* cl_Ceil is the natural log of the initial contract cost ceiling, in then-year dollars. Data is missing for 0.0018 of records and 0.00347 of obligated dollars.
* cl_Days is the natural log of the initial maximum duration of the contract, in days. The variable is centered, by subtracting its mean (Data is missing for 0.00748 of records and 2.09e-09 of obligated dollars. 
* SIDV, MIDV, and OIDV are dummy variables based on the contract vehicle. They correspond to Single-Award IDVs, Multi-Award IDVs, and Other IDVs, respectively, having a value of 1 when the task order has that vehicle type, and having a 0 other. The remaining types, definitive contracts and purchase orders, are intentionally left out. (Data is missing for 0.251 of records and 0.0626 of obligated dollars.
* n_Fixed is a numeric variable based on contract pricing. It has a value of 0 for cost-based, 0.5 or "combination or other", 1 for any fixed price (excluding fixed-price level of effort which is classified as cost-based). (Data is missing for 0.00105 of records and 1.01e-05 of obligated dollars.)
* n_Incent is a numeric variable based on fee type. Ig has a value. 1 for incentive fee or cost sharing, 0.5 or "combination or other", 0 for all remaining types. (Data is missing for 0.00102 of records and 0 of obligated dollars.)
* b_UCA is a binary variable with a value of 1 for contracts/task orders that begin as letter contracts or undefinitized contract awards (UCA) and a value of 0 otherwise. `r
summary(DefenseModelAndDetail$b_UCA);
NA_stats(DefenseModelAndDetail,"b_UCA")`
* b_Intl is a binary variable with a value of 1 for contracts/task orders with any transactions performed internationally and a value of 0 otherwise. Data is missing for 0.228 of records and 0.0187 of obligated dollars.
* PSR is a factor reporting whether a contract is primarily for products/services/R&D. 
Data is missing for 0 of records and 0 of obligated dollars.
* ProdServ is a factor reporting the top Product or Service Code of each contract. 
Data is missing for 5.68e-05 of records and 0 of obligated dollars.
* NAICS is a factor reporting the top North American Industrial Classification Code of each contract. 
Data is missing for 0.000633 of records and 0.00235 of obligated dollars.
* Agency is a factor reporting the top Contracting Agency of each contract. 
Data is missing for 0.000153 of records and 2.17e-06 of obligated dollars.
* Office is a factor reporting the top Contracting office of each contract. 
Data is missing for 0.000319 of records and 3.54e-06 of obligated dollars.

