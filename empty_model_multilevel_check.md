---
title: "empty_model_multilevel_check"
author: "Greg Sanders"
date: "August 15, 2018"
output: 
  html_document: 
    fig_caption: yes
    keep_md: yes
    toc: yes
---


# Setup

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
## Working directory is C:/Users/gsand/Repositories/Vendor
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

```
## Loading required package: lattice
```

```
## 
## Attaching package: 'lattice'
```

```
## The following object is masked from 'package:boot':
## 
##     melanoma
```

```
## Loading required package: survival
```

```
## 
## Attaching package: 'survival'
```

```
## The following object is masked from 'package:boot':
## 
##     aml
```

```
## Loading required package: Formula
```

```
## 
## Attaching package: 'Hmisc'
```

```
## The following objects are masked from 'package:dplyr':
## 
##     src, summarize
```

```
## The following objects are masked from 'package:base':
## 
##     format.pval, units
```

```
## Warning: package 'sjstats' was built under R version 3.4.4
```

```
## Warning in checkMatrixPackageVersion(): Package version inconsistency detected.
## TMB was built with Matrix version 1.2.14
## Current Matrix version is 1.2.12
## Please re-install 'TMB' from source using install.packages('TMB', type = 'source') or ask CRAN for a binary version of 'TMB' matching CRAN's 'Matrix' package
```

```
## 
## Attaching package: 'sjstats'
```

```
## The following object is masked from 'package:Hmisc':
## 
##     deff
```

```
## Warning: package 'car' was built under R version 3.4.4
```

```
## Loading required package: carData
```

```
## Warning: package 'carData' was built under R version 3.4.4
```

```
## 
## Attaching package: 'car'
```

```
## The following object is masked from 'package:boot':
## 
##     logit
```

```
## The following object is masked from 'package:arm':
## 
##     logit
```

```
## The following object is masked from 'package:dplyr':
## 
##     recode
```

## Load in data

```r
 load(file="data//def_sample.Rdata")
# save(smp,smp1m,file="data//def_sample.Rdata")



# complete<-!is.na(def$b_Term)&
#   !is.na(def$b_CBre)&
#   !is.na(def$n_Comp)&
#   !is.na(def$l_Ceil)&
#   !is.na(def$l_Days)&
#   !is.na(def$Veh) &
#   !is.na(def$n_Fixed)&
#   !is.na(def$b_Intl)&
#   !is.na(def$b_UCA)&
#   !is.na(def$NAICS)&
#   !is.na(def$NAICS2)&
#   !is.na(def$cl_def6_HHI_lag1)&
#   !is.na(def$US6_avg_sal_lag1)
# # 
# smp<-def[complete,]
# #8818392  to 8818392
# 13057769 to 8335209
# nrow(def[!complete,])/nrow(def)
# sum(def[!complete,]$Action.Obligation,na.rm=TRUE)/sum(def$Action.Obligation,na.rm=TRUE)
# 
# smp1m<-smp[sample(nrow(smp),1000000),]
# smp<-smp[sample(nrow(smp),250000),]
# save(file="data//def_sample.Rdata",smp,smp1m)

# levels(smp$Intl)<- list("Just U.S."=c("Just U.S."), 
#                                 "Any Intl."=c("Any International"))
# levels(smp1m$Intl)<- list("Just U.S."=c("Just U.S."), 
#                                 "Any Intl."=c("Any International"))
```

#ICC Testing
## NAICS
### NAICS level 6

```r
load(file="Output//naics_empty_models.rdata")

if(!exists("naics06_term"))
naics06_term <- glmer(data=smp,
                 b_Term ~ (1 | NAICS)
                  , family=binomial(link="logit"))
get_icc(display=TRUE,naics06_term)
```

```
## glmer(formula = b_Term ~ (1 | NAICS), data = smp, family = binomial(link = "logit"))
## coef.est  coef.se 
##    -4.63     0.06 
## 
## Error terms:
##  Groups   Name        Std.Dev.
##  NAICS    (Intercept) 0.99    
##  Residual             1.00    
## ---
## number of obs: 250000, groups: NAICS, 886
## AIC = 25716, DIC = 24061.3
## deviance = 24886.7
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_Term ~ (1 | NAICS)
## 
##   ICC (NAICS): 0.227905
```

```r
if(!exists("naics06_cbre"))
naics06_cbre <- glmer(data=smp,
                 b_CBre ~ (1 | NAICS)
                  , family=binomial(link="logit"))
get_icc(display=TRUE,naics06_cbre)
```

```
## glmer(formula = b_CBre ~ (1 | NAICS), data = smp, family = binomial(link = "logit"))
## coef.est  coef.se 
##    -5.02     0.09 
## 
## Error terms:
##  Groups   Name        Std.Dev.
##  NAICS    (Intercept) 1.56    
##  Residual             1.00    
## ---
## number of obs: 250000, groups: NAICS, 886
## AIC = 20686.6, DIC = 18452.5
## deviance = 19567.5
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_CBre ~ (1 | NAICS)
## 
##   ICC (NAICS): 0.426581
```

```r
if(!exists("naics06_comp"))
naics06_comp <- glmer(data=smp,
                 b_Comp ~ (1 | NAICS)
                  , family=binomial(link="logit"))
get_icc(display=TRUE,naics06_comp)
```

```
## glmer(formula = b_Comp ~ (1 | NAICS), data = smp, family = binomial(link = "logit"))
## coef.est  coef.se 
##     1.31     0.04 
## 
## Error terms:
##  Groups   Name        Std.Dev.
##  NAICS    (Intercept) 1.09    
##  Residual             1.00    
## ---
## number of obs: 250000, groups: NAICS, 1041
## AIC = 190707, DIC = 185181.5
## deviance = 187942.1
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_Comp ~ (1 | NAICS)
## 
##   ICC (NAICS): 0.263687
```

### NAICS level 5

```r
if(!exists("naics05_term"))
  naics05_term <- glmer(data=smp,
                 b_Term ~ (1 | NAICS5)
                  , family=binomial(link="logit"))
get_icc(display=TRUE,naics05_term)
```

```
## glmer(formula = b_Term ~ (1 | NAICS5), data = smp, family = binomial(link = "logit"))
## coef.est  coef.se 
##    -4.64     0.00 
## 
## Error terms:
##  Groups   Name        Std.Dev.
##  NAICS5   (Intercept) 1.06    
##  Residual             1.00    
## ---
## number of obs: 250000, groups: NAICS5, 637
## AIC = 28173.3, DIC = 26692.3
## deviance = 27430.8
```

```
## [[1]]
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_Term ~ (1 | NAICS5)
## 
##   ICC (NAICS5): 0.256112
## 
## [[2]]
## [[2]][[1]]
## [1] "Model failed to converge with max|grad| = 0.256599 (tol = 0.001, component 1)"
## 
## [[2]][[2]]
## [1] "Model is nearly unidentifiable: very large eigenvalue\n - Rescale variables?"
```

```r
if(!exists("naics05_cbre"))
naics05_cbre <- glmer(data=smp,
                 b_CBre ~ (1 | NAICS5)
                  , family=binomial(link="logit"))
get_icc(display=TRUE,naics05_cbre)
```

```
## glmer(formula = b_CBre ~ (1 | NAICS5), data = smp, family = binomial(link = "logit"))
## coef.est  coef.se 
##    -4.67     0.09 
## 
## Error terms:
##  Groups   Name        Std.Dev.
##  NAICS5   (Intercept) 1.49    
##  Residual             1.00    
## ---
## number of obs: 250000, groups: NAICS5, 637
## AIC = 26521, DIC = 24524.6
## deviance = 25520.8
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_CBre ~ (1 | NAICS5)
## 
##   ICC (NAICS5): 0.403479
```

```r
if(!exists("naics05_comp"))
naics05_comp <- glmer(data=smp,
                 b_Comp ~ (1 | NAICS5)
                  , family=binomial(link="logit"))
get_icc(display=TRUE,naics05_comp)
```

```
## glmer(formula = b_Comp ~ (1 | NAICS5), data = smp, family = binomial(link = "logit"))
## coef.est  coef.se 
##     1.30     0.05 
## 
## Error terms:
##  Groups   Name        Std.Dev.
##  NAICS5   (Intercept) 1.11    
##  Residual             1.00    
## ---
## number of obs: 250000, groups: NAICS5, 637
## AIC = 194808, DIC = 190969.4
## deviance = 192886.7
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_Comp ~ (1 | NAICS5)
## 
##   ICC (NAICS5): 0.271440
```

### NAICS level 4

```r
if(!exists("naics04_term"))
  naics04_term <- glmer(data=smp,
                        b_Term ~ (1 | NAICS4)
                        , family=binomial(link="logit"))
get_icc(display=TRUE,naics04_term)
```

```
## glmer(formula = b_Term ~ (1 | NAICS4), data = smp, family = binomial(link = "logit"))
## coef.est  coef.se 
##    -4.55     0.08 
## 
## Error terms:
##  Groups   Name        Std.Dev.
##  NAICS4   (Intercept) 1.01    
##  Residual             1.00    
## ---
## number of obs: 250000, groups: NAICS4, 310
## AIC = 28273.3, DIC = 27223.7
## deviance = 27746.5
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_Term ~ (1 | NAICS4)
## 
##   ICC (NAICS4): 0.238173
```

```r
if(!exists("naics04_cbre"))
  naics04_cbre <- glmer(data=smp,
                        b_CBre ~ (1 | NAICS4)
                        , family=binomial(link="logit"))
get_icc(display=TRUE,naics04_cbre)
```

```
## glmer(formula = b_CBre ~ (1 | NAICS4), data = smp, family = binomial(link = "logit"))
## coef.est  coef.se 
##    -4.50     0.10 
## 
## Error terms:
##  Groups   Name        Std.Dev.
##  NAICS4   (Intercept) 1.37    
##  Residual             1.00    
## ---
## number of obs: 250000, groups: NAICS4, 310
## AIC = 26628.9, DIC = 25303.2
## deviance = 25964.0
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_CBre ~ (1 | NAICS4)
## 
##   ICC (NAICS4): 0.363670
```

```r
if(!exists("naics04_comp"))
  naics04_comp <- glmer(data=smp,
                        b_Comp ~ (1 | NAICS4)
                        , family=binomial(link="logit"))
get_icc(display=TRUE,naics04_comp)
```

```
## glmer(formula = b_Comp ~ (1 | NAICS4), data = smp, family = binomial(link = "logit"))
## coef.est  coef.se 
##     1.47     0.07 
## 
## Error terms:
##  Groups   Name        Std.Dev.
##  NAICS4   (Intercept) 1.07    
##  Residual             1.00    
## ---
## number of obs: 250000, groups: NAICS4, 258
## AIC = 180460, DIC = 178419.4
## deviance = 179437.8
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_Comp ~ (1 | NAICS4)
## 
##   ICC (NAICS4): 0.258077
```

### NAICS level 3

```r
if(!exists("naics03_term"))
  naics03_term <- glmer(data=smp,
                        b_Term ~ (1 | NAICS3)
                        , family=binomial(link="logit"))
get_icc(display=TRUE,naics03_term)
```

```
## glmer(formula = b_Term ~ (1 | NAICS3), data = smp, family = binomial(link = "logit"))
## coef.est  coef.se 
##    -4.59     0.12 
## 
## Error terms:
##  Groups   Name        Std.Dev.
##  NAICS3   (Intercept) 0.94    
##  Residual             1.00    
## ---
## number of obs: 250000, groups: NAICS3, 101
## AIC = 28633.8, DIC = 28182.6
## deviance = 28406.2
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_Term ~ (1 | NAICS3)
## 
##   ICC (NAICS3): 0.210757
```

```r
if(!exists("naics03_cbre"))
  naics03_cbre <- glmer(data=smp,
                        b_CBre ~ (1 | NAICS3)
                        , family=binomial(link="logit"))
get_icc(display=TRUE,naics03_cbre)
```

```
## glmer(formula = b_CBre ~ (1 | NAICS3), data = smp, family = binomial(link = "logit"))
## coef.est  coef.se 
##    -4.37     0.15 
## 
## Error terms:
##  Groups   Name        Std.Dev.
##  NAICS3   (Intercept) 1.29    
##  Residual             1.00    
## ---
## number of obs: 250000, groups: NAICS3, 101
## AIC = 27602.5, DIC = 27019
## deviance = 27308.7
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_CBre ~ (1 | NAICS3)
## 
##   ICC (NAICS3): 0.335355
```

```r
if(!exists("naics03_comp"))
  naics03_comp <- glmer(data=smp,
                        b_Comp ~ (1 | NAICS3)
                        , family=binomial(link="logit"))
get_icc(display=TRUE,naics03_comp)
```

```
## glmer(formula = b_Comp ~ (1 | NAICS3), data = smp, family = binomial(link = "logit"))
## coef.est  coef.se 
##     1.17     0.09 
## 
## Error terms:
##  Groups   Name        Std.Dev.
##  NAICS3   (Intercept) 0.98    
##  Residual             1.00    
## ---
## number of obs: 250000, groups: NAICS3, 101
## AIC = 211886, DIC = 210960.8
## deviance = 211421.6
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_Comp ~ (1 | NAICS3)
## 
##   ICC (NAICS3): 0.226141
```
### NAICS level 2

```r
if(!exists("naics02_term"))
  naics02_term <- glmer(data=smp,
                        b_Term ~ (1 | NAICS2)
                        , family=binomial(link="logit"))
get_icc(display=TRUE,naics02_term)
```

```
## glmer(formula = b_Term ~ (1 | NAICS2), data = smp, family = binomial(link = "logit"))
## coef.est  coef.se 
##    -4.49     0.21 
## 
## Error terms:
##  Groups   Name        Std.Dev.
##  NAICS2   (Intercept) 0.84    
##  Residual             1.00    
## ---
## number of obs: 250000, groups: NAICS2, 23
## AIC = 28961.9, DIC = 28778.3
## deviance = 28868.1
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_Term ~ (1 | NAICS2)
## 
##   ICC (NAICS2): 0.176498
```

```r
if(!exists("naics02_cbre"))
  naics02_cbre <- glmer(data=smp,
                        b_CBre ~ (1 | NAICS2)
                        , family=binomial(link="logit"))
get_icc(display=TRUE,naics02_cbre)
```

```
## glmer(formula = b_CBre ~ (1 | NAICS2), data = smp, family = binomial(link = "logit"))
## coef.est  coef.se 
##    -4.10     0.19 
## 
## Error terms:
##  Groups   Name        Std.Dev.
##  NAICS2   (Intercept) 1.17    
##  Residual             1.00    
## ---
## number of obs: 250000, groups: NAICS2, 23
## AIC = 28102.9, DIC = 27872.7
## deviance = 27985.8
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_CBre ~ (1 | NAICS2)
## 
##   ICC (NAICS2): 0.294267
```

```r
if(!exists("naics02_comp"))
  naics02_comp<- glmer(data=smp,
                       b_Comp ~ (1 | NAICS2)
                       , family=binomial(link="logit"))

get_icc(display=TRUE,naics02_comp)
```

```
## glmer(formula = b_Comp ~ (1 | NAICS2), data = smp, family = binomial(link = "logit"))
## coef.est  coef.se 
##     1.23     0.10 
## 
## Error terms:
##  Groups   Name        Std.Dev.
##  NAICS2   (Intercept) 0.66    
##  Residual             1.00    
## ---
## number of obs: 250000, groups: NAICS2, 23
## AIC = 218988, DIC = 218703.9
## deviance = 218843.7
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_Comp ~ (1 | NAICS2)
## 
##   ICC (NAICS2): 0.115593
```


### NAICS 2/3/6

```r
smp$NAICS<-factor(smp$NAICS)
smp$NAICS3<-factor(smp$NAICS3)
smp$NAICS<-factor(smp$NAICS)
smp$NAICS2<-factor(smp$NAICS2)
# naics23456_term <- glmer(data=smp,
#                  b_Term ~ (1 | NAICS2/NAICS3/NAICS4/NAICS5/NAICS)
#                   , family=binomial(link="logit"))
#Fails even when everything is a factor Error: couldn't evaluate grouping factor NAICS:(NAICS5:(NAICS4:(NAICS3:NAICS2))) within model frame: try adding grouping factor to data frame explicitly if possible
# naics2346_term <- glmer(data=smp,
#                  b_Term ~ (1 | NAICS2/NAICS3/NAICS4/NAICS)
#                   , family=binomial(link="logit"))
#Fails even when everything is a factor Error: couldn't evaluate grouping factor NAICS:(NAICS4:(NAICS3:NAICS2)) within model frame: try adding grouping factor to data frame explicitly if possible

if(!exists("naics236_term"))
  naics236_term <- glmer(data=smp,
                         b_Term ~ (1 | NAICS2/NAICS3/NAICS)
                         , family=binomial(link="logit"))
get_icc(display=TRUE,naics236_term)
```

```
## glmer(formula = b_Term ~ (1 | NAICS2/NAICS3/NAICS), data = smp, 
##     family = binomial(link = "logit"))
## coef.est  coef.se 
##    -4.64     0.12 
## 
## Error terms:
##  Groups                Name        Std.Dev.
##  NAICS:(NAICS3:NAICS2) (Intercept) 0.81    
##  NAICS3:NAICS2         (Intercept) 0.44    
##  NAICS2                (Intercept) 0.37    
##  Residual                          1.00    
## ---
## number of obs: 250000, groups: NAICS:(NAICS3:NAICS2), 1041; NAICS3:NAICS2, 101; NAICS2, 23
## AIC = 27975.6, DIC = 26223.8
## deviance = 27095.7
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_Term ~ (1 | NAICS2/NAICS3/NAICS)
## 
##   ICC (NAICS:(NAICS3:NAICS2)): 0.153859
##           ICC (NAICS3:NAICS2): 0.045573
##                  ICC (NAICS2): 0.032283
```

```r
if(!exists("naics236_cbre"))
  naics236_cbre <- glmer(data=smp,
                         b_CBre ~ (1 | NAICS2/NAICS3/NAICS)
                         , family=binomial(link="logit"))
get_icc(display=TRUE,naics236_cbre)
```

```
## glmer(formula = b_CBre ~ (1 | NAICS2/NAICS3/NAICS), data = smp, 
##     family = binomial(link = "logit"))
## coef.est  coef.se 
##    -4.38     0.17 
## 
## Error terms:
##  Groups                Name        Std.Dev.
##  NAICS:(NAICS3:NAICS2) (Intercept) 1.07    
##  NAICS3:NAICS2         (Intercept) 0.08    
##  NAICS2                (Intercept) 0.78    
##  Residual                          1.00    
## ---
## number of obs: 250000, groups: NAICS:(NAICS3:NAICS2), 1041; NAICS3:NAICS2, 101; NAICS2, 23
## AIC = 26308.1, DIC = 24246.5
## deviance = 25273.3
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_CBre ~ (1 | NAICS2/NAICS3/NAICS)
## 
##   ICC (NAICS:(NAICS3:NAICS2)): 0.225842
##           ICC (NAICS3:NAICS2): 0.001202
##                  ICC (NAICS2): 0.120045
```

```r
if(!exists("naics236_comp"))
  naics236_comp <- glmer(data=smp,
                         b_Comp ~ (1 | NAICS2/NAICS3/NAICS)
                         , family=binomial(link="logit"))
get_icc(display=TRUE,naics236_comp)
```

```
## glmer(formula = b_Comp ~ (1 | NAICS2/NAICS3/NAICS), data = smp, 
##     family = binomial(link = "logit"))
## coef.est  coef.se 
##     1.21     0.12 
## 
## Error terms:
##  Groups                Name        Std.Dev.
##  NAICS:(NAICS3:NAICS2) (Intercept) 0.97    
##  NAICS3:NAICS2         (Intercept) 0.34    
##  NAICS2                (Intercept) 0.53    
##  Residual                          1.00    
## ---
## number of obs: 250000, groups: NAICS:(NAICS3:NAICS2), 1041; NAICS3:NAICS2, 101; NAICS2, 23
## AIC = 190627, DIC = 185304.3
## deviance = 187961.8
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_Comp ~ (1 | NAICS2/NAICS3/NAICS)
## 
##   ICC (NAICS:(NAICS3:NAICS2)): 0.201675
##           ICC (NAICS3:NAICS2): 0.025181
##                  ICC (NAICS2): 0.061186
```
### NAICS 2/6

```r
if(!exists("naics26_term"))
  naics26_term <- glmer(data=smp,
                        b_Term ~ (1 | NAICS2/NAICS)
                        , family=binomial(link="logit"))
get_icc(display=TRUE,naics26_term)
```

```
## glmer(formula = b_Term ~ (1 | NAICS2/NAICS), data = smp, family = binomial(link = "logit"))
## coef.est  coef.se 
##    -4.57     0.15 
## 
## Error terms:
##  Groups       Name        Std.Dev.
##  NAICS:NAICS2 (Intercept) 0.84    
##  NAICS2       (Intercept) 0.50    
##  Residual                 1.00    
## ---
## number of obs: 250000, groups: NAICS:NAICS2, 886; NAICS2, 17
## AIC = 25689.4, DIC = 24185.2
## deviance = 24934.3
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_Term ~ (1 | NAICS2/NAICS)
## 
##   ICC (NAICS:NAICS2): 0.165667
##         ICC (NAICS2): 0.059614
```

```r
if(!exists("naics26_cbre"))
  naics26_cbre <- glmer(data=smp,
                        b_CBre ~ (1 | NAICS2/NAICS)
                        , family=binomial(link="logit"))
get_icc(display=TRUE,naics26_cbre)
```

```
## glmer(formula = b_CBre ~ (1 | NAICS2/NAICS), data = smp, family = binomial(link = "logit"))
## coef.est  coef.se 
##    -4.20     0.21 
## 
## Error terms:
##  Groups       Name        Std.Dev.
##  NAICS:NAICS2 (Intercept) 1.07    
##  NAICS2       (Intercept) 0.79    
##  Residual                 1.00    
## ---
## number of obs: 250000, groups: NAICS:NAICS2, 886; NAICS2, 17
## AIC = 20526.9, DIC = 18805.2
## deviance = 19663.1
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_CBre ~ (1 | NAICS2/NAICS)
## 
##   ICC (NAICS:NAICS2): 0.227190
##         ICC (NAICS2): 0.123482
```

```r
if(!exists("naics26_comp"))
  naics26_comp <- glmer(data=smp,
                        b_Comp ~ (1 | NAICS2/NAICS)
                        , family=binomial(link="logit"))

get_icc(display=TRUE,naics26_comp)
```

```
## glmer(formula = b_Comp ~ (1 | NAICS2/NAICS), data = smp, family = binomial(link = "logit"))
## coef.est  coef.se 
##     1.20     0.11 
## 
## Error terms:
##  Groups       Name        Std.Dev.
##  NAICS:NAICS2 (Intercept) 1.00    
##  NAICS2       (Intercept) 0.54    
##  Residual                 1.00    
## ---
## number of obs: 250000, groups: NAICS:NAICS2, 1041; NAICS2, 23
## AIC = 190638, DIC = 185292.7
## deviance = 187962.3
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_Comp ~ (1 | NAICS2/NAICS)
## 
##   ICC (NAICS:NAICS2): 0.218123
##         ICC (NAICS2): 0.062651
```
### NAICS 3/6

```r
if(!exists("naics36_term"))
  naics36_term <- glmer(data=smp,
                        b_Term ~ (1 | NAICS3/NAICS)
                        , family=binomial(link="logit"))
get_icc(display=TRUE,naics36_term)
```

```
## glmer(formula = b_Term ~ (1 | NAICS3/NAICS), data = smp, family = binomial(link = "logit"))
## coef.est  coef.se 
##    -4.66     0.10 
## 
## Error terms:
##  Groups       Name        Std.Dev.
##  NAICS:NAICS3 (Intercept) 0.77    
##  NAICS3       (Intercept) 0.58    
##  Residual                 1.00    
## ---
## number of obs: 250000, groups: NAICS:NAICS3, 886; NAICS3, 79
## AIC = 25674.9, DIC = 24206.8
## deviance = 24937.8
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_Term ~ (1 | NAICS3/NAICS)
## 
##   ICC (NAICS:NAICS3): 0.141022
##         ICC (NAICS3): 0.080624
```

```r
if(!exists("naics36_cbre"))
  naics36_cbre <- glmer(data=smp,
                        b_CBre ~ (1 | NAICS3/NAICS)
                        , family=binomial(link="logit"))
get_icc(display=TRUE,naics36_cbre)
```

```
## glmer(formula = b_CBre ~ (1 | NAICS3/NAICS), data = smp, family = binomial(link = "logit"))
## coef.est  coef.se 
##    -4.74     0.15 
## 
## Error terms:
##  Groups       Name        Std.Dev.
##  NAICS:NAICS3 (Intercept) 1.02    
##  NAICS3       (Intercept) 1.01    
##  Residual                 1.00    
## ---
## number of obs: 250000, groups: NAICS:NAICS3, 886; NAICS3, 79
## AIC = 20557.7, DIC = 18776.1
## deviance = 19663.9
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_CBre ~ (1 | NAICS3/NAICS)
## 
##   ICC (NAICS:NAICS3): 0.193197
##         ICC (NAICS3): 0.190459
```

```r
if(!exists("naics36_comp"))
  naics36_comp <- glmer(data=smp,
                        b_Comp ~ (1 | NAICS3/NAICS)
                        , family=binomial(link="logit"))
get_icc(display=TRUE,naics36_cbre)
```

```
## glmer(formula = b_CBre ~ (1 | NAICS3/NAICS), data = smp, family = binomial(link = "logit"))
## coef.est  coef.se 
##    -4.74     0.15 
## 
## Error terms:
##  Groups       Name        Std.Dev.
##  NAICS:NAICS3 (Intercept) 1.02    
##  NAICS3       (Intercept) 1.01    
##  Residual                 1.00    
## ---
## number of obs: 250000, groups: NAICS:NAICS3, 886; NAICS3, 79
## AIC = 20557.7, DIC = 18776.1
## deviance = 19663.9
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_CBre ~ (1 | NAICS3/NAICS)
## 
##   ICC (NAICS:NAICS3): 0.193197
##         ICC (NAICS3): 0.190459
```


### NAICS 4/6

```r
if(!exists("naics46_term"))
  naics46_term <- glmer(data=smp,
                        b_Term ~ (1 | NAICS4/NAICS)
                        , family=binomial(link="logit"))
get_icc(display=TRUE,naics46_term)
```

```
## glmer(formula = b_Term ~ (1 | NAICS4/NAICS), data = smp, family = binomial(link = "logit"))
## coef.est  coef.se 
##    -4.56     0.07 
## 
## Error terms:
##  Groups       Name        Std.Dev.
##  NAICS:NAICS4 (Intercept) 0.75    
##  NAICS4       (Intercept) 0.71    
##  Residual                 1.00    
## ---
## number of obs: 250000, groups: NAICS:NAICS4, 1041; NAICS4, 310
## AIC = 28002.6, DIC = 26156.5
## deviance = 27076.5
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_Term ~ (1 | NAICS4/NAICS)
## 
##   ICC (NAICS:NAICS4): 0.128259
##         ICC (NAICS4): 0.116694
```

```r
if(!exists("naics46_cbre"))
  naics46_cbre <- glmer(data=smp,
                        b_CBre ~ (1 | NAICS4/NAICS)
                        , family=binomial(link="logit"))
get_icc(display=TRUE,naics46_cbre)
```

```
## glmer(formula = b_CBre ~ (1 | NAICS4/NAICS), data = smp, family = binomial(link = "logit"))
## coef.est  coef.se 
##    -4.63     0.10 
## 
## Error terms:
##  Groups       Name        Std.Dev.
##  NAICS:NAICS4 (Intercept) 0.90    
##  NAICS4       (Intercept) 1.11    
##  Residual                 1.00    
## ---
## number of obs: 250000, groups: NAICS:NAICS4, 1041; NAICS4, 310
## AIC = 26360.9, DIC = 24146.7
## deviance = 25250.8
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_CBre ~ (1 | NAICS4/NAICS)
## 
##   ICC (NAICS:NAICS4): 0.151950
##         ICC (NAICS4): 0.229554
```

```r
if(!exists("naics46_comp"))
  naics46_comp <- glmer(data=smp,
                        b_Comp ~ (1 | NAICS4/NAICS)
                        , family=binomial(link="logit"))
get_icc(display=TRUE,naics46_comp)
```

```
## glmer(formula = b_Comp ~ (1 | NAICS4/NAICS), data = smp, family = binomial(link = "logit"))
## coef.est  coef.se 
##     1.29     0.05 
## 
## Error terms:
##  Groups       Name        Std.Dev.
##  NAICS:NAICS4 (Intercept) 0.91    
##  NAICS4       (Intercept) 0.63    
##  Residual                 1.00    
## ---
## number of obs: 250000, groups: NAICS:NAICS4, 1041; NAICS4, 310
## AIC = 190659, DIC = 185268.8
## deviance = 187960.7
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_Comp ~ (1 | NAICS4/NAICS)
## 
##   ICC (NAICS:NAICS4): 0.184022
##         ICC (NAICS4): 0.087934
```

### Compare NAICS


```r
save(naics02_cbre,naics02_term,naics02_comp,
     naics03_cbre,naics03_term,naics03_comp,
     naics04_cbre,naics04_term,naics04_comp,
     naics05_cbre,naics05_term,naics05_comp,
     naics06_cbre,naics06_term,naics06_comp,
     naics26_cbre,naics26_term,naics26_comp,
     naics36_cbre,naics36_term,naics36_comp,
     naics46_cbre,naics46_term,naics46_comp,
     naics236_cbre,naics236_term,naics236_comp,
     file="Output//naics_empty_models.rdata")


get_icc(naics02_cbre)
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_CBre ~ (1 | NAICS2)
## 
##   ICC (NAICS2): 0.294267
```

```r
get_icc(naics03_cbre)
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_CBre ~ (1 | NAICS3)
## 
##   ICC (NAICS3): 0.335355
```

```r
get_icc(naics04_cbre)
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_CBre ~ (1 | NAICS4)
## 
##   ICC (NAICS4): 0.363670
```

```r
get_icc(naics05_cbre)
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_CBre ~ (1 | NAICS5)
## 
##   ICC (NAICS5): 0.403479
```

```r
get_icc(naics06_cbre)
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_CBre ~ (1 | NAICS)
## 
##   ICC (NAICS): 0.426581
```

```r
get_icc(naics26_cbre)
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_CBre ~ (1 | NAICS2/NAICS)
## 
##   ICC (NAICS:NAICS2): 0.227190
##         ICC (NAICS2): 0.123482
```

```r
get_icc(naics36_cbre)
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_CBre ~ (1 | NAICS3/NAICS)
## 
##   ICC (NAICS:NAICS3): 0.193197
##         ICC (NAICS3): 0.190459
```

```r
get_icc(naics46_cbre)
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_CBre ~ (1 | NAICS4/NAICS)
## 
##   ICC (NAICS:NAICS4): 0.151950
##         ICC (NAICS4): 0.229554
```

```r
get_icc(naics236_cbre)
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_CBre ~ (1 | NAICS2/NAICS3/NAICS)
## 
##   ICC (NAICS:(NAICS3:NAICS2)): 0.225842
##           ICC (NAICS3:NAICS2): 0.001202
##                  ICC (NAICS2): 0.120045
```

```r
get_icc(naics02_term)
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_Term ~ (1 | NAICS2)
## 
##   ICC (NAICS2): 0.176498
```

```r
get_icc(naics03_term)
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_Term ~ (1 | NAICS3)
## 
##   ICC (NAICS3): 0.210757
```

```r
get_icc(naics04_term)
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_Term ~ (1 | NAICS4)
## 
##   ICC (NAICS4): 0.238173
```

```r
get_icc(naics05_term)
```

```
## [[1]]
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_Term ~ (1 | NAICS5)
## 
##   ICC (NAICS5): 0.256112
## 
## [[2]]
## [[2]][[1]]
## [1] "Model failed to converge with max|grad| = 0.256599 (tol = 0.001, component 1)"
## 
## [[2]][[2]]
## [1] "Model is nearly unidentifiable: very large eigenvalue\n - Rescale variables?"
```

```r
get_icc(naics06_term)
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_Term ~ (1 | NAICS)
## 
##   ICC (NAICS): 0.227905
```

```r
get_icc(naics26_term)
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_Term ~ (1 | NAICS2/NAICS)
## 
##   ICC (NAICS:NAICS2): 0.165667
##         ICC (NAICS2): 0.059614
```

```r
get_icc(naics36_term)
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_Term ~ (1 | NAICS3/NAICS)
## 
##   ICC (NAICS:NAICS3): 0.141022
##         ICC (NAICS3): 0.080624
```

```r
get_icc(naics46_term)
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_Term ~ (1 | NAICS4/NAICS)
## 
##   ICC (NAICS:NAICS4): 0.128259
##         ICC (NAICS4): 0.116694
```

```r
get_icc(naics236_term)
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_Term ~ (1 | NAICS2/NAICS3/NAICS)
## 
##   ICC (NAICS:(NAICS3:NAICS2)): 0.153859
##           ICC (NAICS3:NAICS2): 0.045573
##                  ICC (NAICS2): 0.032283
```

```r
get_icc(naics02_comp)
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_Comp ~ (1 | NAICS2)
## 
##   ICC (NAICS2): 0.115593
```

```r
get_icc(naics03_comp)
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_Comp ~ (1 | NAICS3)
## 
##   ICC (NAICS3): 0.226141
```

```r
get_icc(naics04_comp)
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_Comp ~ (1 | NAICS4)
## 
##   ICC (NAICS4): 0.258077
```

```r
get_icc(naics05_comp)
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_Comp ~ (1 | NAICS5)
## 
##   ICC (NAICS5): 0.271440
```

```r
get_icc(naics06_comp)
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_Comp ~ (1 | NAICS)
## 
##   ICC (NAICS): 0.263687
```

```r
get_icc(naics26_comp)
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_Comp ~ (1 | NAICS2/NAICS)
## 
##   ICC (NAICS:NAICS2): 0.218123
##         ICC (NAICS2): 0.062651
```

```r
get_icc(naics36_comp)
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_Comp ~ (1 | NAICS3/NAICS)
## 
##   ICC (NAICS:NAICS3): 0.202860
##         ICC (NAICS3): 0.085484
```

```r
get_icc(naics46_comp)
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_Comp ~ (1 | NAICS4/NAICS)
## 
##   ICC (NAICS:NAICS4): 0.184022
##         ICC (NAICS4): 0.087934
```

```r
get_icc(naics236_comp)
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_Comp ~ (1 | NAICS2/NAICS3/NAICS)
## 
##   ICC (NAICS:(NAICS3:NAICS2)): 0.201675
##           ICC (NAICS3:NAICS2): 0.025181
##                  ICC (NAICS2): 0.061186
```
The ideal levels varies some depending on the output variable. On the whole NAICS 3 and NAICS 6 combined seem to explain comparatively high levels of variance when summed and are reasonably balanced between the two levels.

Fiscal year generally has more explanatory power than calendar year.

## Start Year

### Calendar Year

```r
load(file="Output//when_empty_models.rdata")

if(!exists("StartCY_term"))
  StartCY_term <- glmer(data=smp,
                        b_Term ~ (1 | StartCY)
                        , family=binomial(link="logit"))
get_icc(display=TRUE,StartCY_term)
```

```
## glmer(formula = b_Term ~ (1 | StartCY), data = smp, family = binomial(link = "logit"))
## coef.est  coef.se 
##    -4.59     0.07 
## 
## Error terms:
##  Groups   Name        Std.Dev.
##  StartCY  (Intercept) 0.19    
##  Residual             1.00    
## ---
## number of obs: 250000, groups: StartCY, 9
## AIC = 28068.5, DIC = 28005.8
## deviance = 28035.1
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_Term ~ (1 | StartCY)
## 
##   ICC (StartCY): 0.010637
```

```r
if(!exists("StartCY_cbre"))
  StartCY_cbre <- glmer(data=smp,
                        b_CBre ~ (1 | StartCY)
                        , family=binomial(link="logit"))
get_icc(display=TRUE,StartCY_cbre)
```

```
## glmer(formula = b_CBre ~ (1 | StartCY), data = smp, family = binomial(link = "logit"))
## coef.est  coef.se 
##    -4.82     0.24 
## 
## Error terms:
##  Groups   Name        Std.Dev.
##  StartCY  (Intercept) 0.88    
##  Residual             1.00    
## ---
## number of obs: 250000, groups: StartCY, 9
## AIC = 26120.5, DIC = 26006.7
## deviance = 26061.6
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_CBre ~ (1 | StartCY)
## 
##   ICC (StartCY): 0.189125
```

```r
if(!exists("StartCY_term"))
  StartCY_comp <- glmer(data=smp,
                        b_Comp ~ (1 | StartCY)
                        , family=binomial(link="logit"))
get_icc(display=TRUE,StartCY_term)
```

```
## glmer(formula = b_Term ~ (1 | StartCY), data = smp, family = binomial(link = "logit"))
## coef.est  coef.se 
##    -4.59     0.07 
## 
## Error terms:
##  Groups   Name        Std.Dev.
##  StartCY  (Intercept) 0.19    
##  Residual             1.00    
## ---
## number of obs: 250000, groups: StartCY, 9
## AIC = 28068.5, DIC = 28005.8
## deviance = 28035.1
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_Term ~ (1 | StartCY)
## 
##   ICC (StartCY): 0.010637
```

The ICC is highest for ceiling breaches and is backloaded, with 2015 and 2016 having lower than usual odds. This is likely an artifact of those years having fewer completed longer contracts. That said, this should be largely compensated for with the inclusion of initial duration. If this proves challenges to manage, it may be worth considering dropping 2016. 

### Month Unnested

```r
smp$SignedMonth<-lubridate::month(smp$MinOfSignedDate)

if(!exists("SignedMonth_term"))
  SignedMonth_term <- glmer(data=smp,
                            b_Term ~ (1 | StartCY:SignedMonth)
                            , family=binomial(link="logit"))
get_icc(display=TRUE,SignedMonth_term)
```

```
## glmer(formula = b_Term ~ (1 | StartCY:SignedMonth), data = smp, 
##     family = binomial(link = "logit"))
## coef.est  coef.se 
##    -4.60     0.04 
## 
## Error terms:
##  Groups              Name        Std.Dev.
##  StartCY:SignedMonth (Intercept) 0.31    
##  Residual                        1.00    
## ---
## number of obs: 250000, groups: StartCY:SignedMonth, 108
## AIC = 28017.8, DIC = 27625.4
## deviance = 27819.6
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_Term ~ (1 | StartCY:SignedMonth)
## 
##   ICC (StartCY:SignedMonth): 0.029251
```

```r
if(!exists("SignedMonth_cbre"))
  SignedMonth_cbre <- glmer(data=smp,
                            b_CBre ~ (1 | StartCY:SignedMonth)
                            , family=binomial(link="logit"))
get_icc(display=TRUE,SignedMonth_cbre)
```

```
## glmer(formula = b_CBre ~ (1 | StartCY:SignedMonth), data = smp, 
##     family = binomial(link = "logit"))
## coef.est  coef.se 
##    -4.80     0.08 
## 
## Error terms:
##  Groups              Name        Std.Dev.
##  StartCY:SignedMonth (Intercept) 0.80    
##  Residual                        1.00    
## ---
## number of obs: 250000, groups: StartCY:SignedMonth, 108
## AIC = 26153.2, DIC = 25428.2
## deviance = 25788.7
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_CBre ~ (1 | StartCY:SignedMonth)
## 
##   ICC (StartCY:SignedMonth): 0.162726
```

```r
if(!exists("SignedMonth_comp"))
  SignedMonth_comp <- glmer(data=smp,
                            b_Comp ~ (1 | StartCY:SignedMonth)
                            , family=binomial(link="logit"))
get_icc(display=TRUE,SignedMonth_comp)
```

```
## glmer(formula = b_Comp ~ (1 | StartCY:SignedMonth), data = smp, 
##     family = binomial(link = "logit"))
## coef.est  coef.se 
##     1.61     0.03 
## 
## Error terms:
##  Groups              Name        Std.Dev.
##  StartCY:SignedMonth (Intercept) 0.35    
##  Residual                        1.00    
## ---
## number of obs: 250000, groups: StartCY:SignedMonth, 108
## AIC = 218840, DIC = 217865.4
## deviance = 218350.8
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_Comp ~ (1 | StartCY:SignedMonth)
## 
##   ICC (StartCY:SignedMonth): 0.035931
```

### Month Nested

```r
if(!exists("NestedMonth_term"))
NestedMonth_term <- glmer(data=smp,
                 b_Term ~ (1 | StartCY/SignedMonth)
                  , family=binomial(link="logit"))
get_icc(display=TRUE,NestedMonth_term)
```

```
## glmer(formula = b_Term ~ (1 | StartCY/SignedMonth), data = smp, 
##     family = binomial(link = "logit"))
## coef.est  coef.se 
##    -4.60     0.07 
## 
## Error terms:
##  Groups              Name        Std.Dev.
##  SignedMonth:StartCY (Intercept) 0.26    
##  StartCY             (Intercept) 0.18    
##  Residual                        1.00    
## ---
## number of obs: 250000, groups: SignedMonth:StartCY, 108; StartCY, 9
## AIC = 28005.4, DIC = 27652.6
## deviance = 27826.0
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_Term ~ (1 | StartCY/SignedMonth)
## 
##   ICC (SignedMonth:StartCY): 0.019330
##               ICC (StartCY): 0.009989
```

```r
if(!exists("NestedMonth_cbre"))
NestedMonth_cbre <- glmer(data=smp,
                 b_CBre ~ (1 | StartCY/SignedMonth)
                  , family=binomial(link="logit"))
get_icc(display=TRUE,NestedMonth_cbre)
```

```
## glmer(formula = b_CBre ~ (1 | StartCY/SignedMonth), data = smp, 
##     family = binomial(link = "logit"))
## coef.est  coef.se 
##    -4.87     0.28 
## 
## Error terms:
##  Groups              Name        Std.Dev.
##  SignedMonth:StartCY (Intercept) 0.29    
##  StartCY             (Intercept) 0.86    
##  Residual                        1.00    
## ---
## number of obs: 250000, groups: SignedMonth:StartCY, 108; StartCY, 9
## AIC = 26021.3, DIC = 25605.1
## deviance = 25810.2
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_CBre ~ (1 | StartCY/SignedMonth)
## 
##   ICC (SignedMonth:StartCY): 0.020718
##               ICC (StartCY): 0.179313
```

```r
if(!exists("NestedMonth_comp"))
NestedMonth_comp <- glmer(data=smp,
                 b_Comp ~ (1 | StartCY/SignedMonth)
                  , family=binomial(link="logit"))
get_icc(display=TRUE,NestedMonth_comp)
```

```
## glmer(formula = b_Comp ~ (1 | StartCY/SignedMonth), data = smp, 
##     family = binomial(link = "logit"))
## coef.est  coef.se 
##     1.62     0.10 
## 
## Error terms:
##  Groups              Name        Std.Dev.
##  SignedMonth:StartCY (Intercept) 0.13    
##  StartCY             (Intercept) 0.34    
##  Residual                        1.00    
## ---
## number of obs: 250000, groups: SignedMonth:StartCY, 108; StartCY, 9
## AIC = 218694, DIC = 218040.2
## deviance = 218364.0
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_Comp ~ (1 | StartCY/SignedMonth)
## 
##   ICC (SignedMonth:StartCY): 0.005281
##               ICC (StartCY): 0.033118
```

### NAICS and Calendar Year

```r
if(!exists("NAICS_StartCY_term"))
  NAICS_StartCY_term <- glmer(data=smp,
                              b_Term ~  (1 | NAICS3/NAICS) + (1 | StartCY)
                              , family=binomial(link="logit"))
get_icc(display=TRUE,NAICS_StartCY_term)
```

```
## glmer(formula = b_Term ~ (1 | NAICS3/NAICS) + (1 | StartCY), 
##     data = smp, family = binomial(link = "logit"))
## coef.est  coef.se 
##    -4.66     0.12 
## 
## Error terms:
##  Groups       Name        Std.Dev.
##  NAICS:NAICS3 (Intercept) 0.78    
##  NAICS3       (Intercept) 0.61    
##  StartCY      (Intercept) 0.21    
##  Residual                 1.00    
## ---
## number of obs: 250000, groups: NAICS:NAICS3, 886; NAICS3, 79; StartCY, 9
## AIC = 25604.2, DIC = 24063.9
## deviance = 24830.1
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_Term ~ (1 | NAICS3/NAICS) + (1 | StartCY)
## 
##   ICC (NAICS:NAICS3): 0.140341
##         ICC (NAICS3): 0.086116
##        ICC (StartCY): 0.010416
```

```r
if(!exists("NAICS_StartCY_cbre"))
  NAICS_StartCY_cbre <- glmer(data=smp,
                              b_CBre ~  (1 | NAICS3/NAICS) + (1 | StartCY)
                              , family=binomial(link="logit"))
get_icc(display=TRUE,NAICS_StartCY_cbre)
```

```
## glmer(formula = b_CBre ~ (1 | NAICS3/NAICS) + (1 | StartCY), 
##     data = smp, family = binomial(link = "logit"))
## coef.est  coef.se 
##    -4.83     0.18 
## 
## Error terms:
##  Groups       Name        Std.Dev.
##  NAICS:NAICS3 (Intercept) 0.97    
##  NAICS3       (Intercept) 0.98    
##  StartCY      (Intercept) 0.35    
##  Residual                 1.00    
## ---
## number of obs: 250000, groups: NAICS:NAICS3, 886; NAICS3, 79; StartCY, 9
## AIC = 20470.3, DIC = 18678.4
## deviance = 19570.4
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_CBre ~ (1 | NAICS3/NAICS) + (1 | StartCY)
## 
##   ICC (NAICS:NAICS3): 0.176667
##         ICC (NAICS3): 0.180294
##        ICC (StartCY): 0.023467
```

```r
if(!exists("NAICS_StartCY_comp"))
  NAICS_StartCY_comp <- glmer(data=smp,
                              b_Comp ~  (1 | NAICS3/NAICS) + (1 | StartCY)
                              , family=binomial(link="logit"))
get_icc(display=TRUE,NAICS_StartCY_comp)
```

```
## glmer(formula = b_Comp ~ (1 | NAICS3/NAICS) + (1 | StartCY), 
##     data = smp, family = binomial(link = "logit"))
## coef.est  coef.se 
##     1.54     0.14 
## 
## Error terms:
##  Groups       Name        Std.Dev.
##  NAICS:NAICS3 (Intercept) 1.00    
##  NAICS3       (Intercept) 0.53    
##  StartCY      (Intercept) 0.37    
##  Residual                 1.00    
## ---
## number of obs: 250000, groups: NAICS:NAICS3, 886; NAICS3, 79; StartCY, 9
## AIC = 167689, DIC = 162911.1
## deviance = 165295.8
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_Comp ~ (1 | NAICS3/NAICS) + (1 | StartCY)
## 
##   ICC (NAICS:NAICS3): 0.213552
##         ICC (NAICS3): 0.058957
##        ICC (StartCY): 0.028867
```

### Compare Signed Dates

```r
save(StartCY_cbre,StartCY_term,StartCY_comp,
     SignedMonth_cbre,SignedMonth_term,SignedMonth_comp,
     NestedMonth_cbre,NestedMonth_term,NestedMonth_comp,
     NAICS_StartCY_cbre,NAICS_StartCY_term,NAICS_StartCY_comp,
     file="Output//when_empty_models.rdata")

get_icc(naics36_cbre)
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_CBre ~ (1 | NAICS3/NAICS)
## 
##   ICC (NAICS:NAICS3): 0.193197
##         ICC (NAICS3): 0.190459
```

```r
get_icc(StartCY_cbre)
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_CBre ~ (1 | StartCY)
## 
##   ICC (StartCY): 0.189125
```

```r
get_icc(SignedMonth_cbre)
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_CBre ~ (1 | StartCY:SignedMonth)
## 
##   ICC (StartCY:SignedMonth): 0.162726
```

```r
get_icc(NestedMonth_cbre)
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_CBre ~ (1 | StartCY/SignedMonth)
## 
##   ICC (SignedMonth:StartCY): 0.020718
##               ICC (StartCY): 0.179313
```

```r
get_icc(NAICS_StartCY_cbre)
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_CBre ~ (1 | NAICS3/NAICS) + (1 | StartCY)
## 
##   ICC (NAICS:NAICS3): 0.176667
##         ICC (NAICS3): 0.180294
##        ICC (StartCY): 0.023467
```

```r
get_icc(naics36_term)
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_Term ~ (1 | NAICS3/NAICS)
## 
##   ICC (NAICS:NAICS3): 0.141022
##         ICC (NAICS3): 0.080624
```

```r
get_icc(StartCY_term)
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_Term ~ (1 | StartCY)
## 
##   ICC (StartCY): 0.010637
```

```r
get_icc(SignedMonth_term)
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_Term ~ (1 | StartCY:SignedMonth)
## 
##   ICC (StartCY:SignedMonth): 0.029251
```

```r
get_icc(NestedMonth_term)
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_Term ~ (1 | StartCY/SignedMonth)
## 
##   ICC (SignedMonth:StartCY): 0.019330
##               ICC (StartCY): 0.009989
```

```r
get_icc(NAICS_StartCY_term)
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_Term ~ (1 | NAICS3/NAICS) + (1 | StartCY)
## 
##   ICC (NAICS:NAICS3): 0.140341
##         ICC (NAICS3): 0.086116
##        ICC (StartCY): 0.010416
```

```r
get_icc(naics36_comp)
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_Comp ~ (1 | NAICS3/NAICS)
## 
##   ICC (NAICS:NAICS3): 0.202860
##         ICC (NAICS3): 0.085484
```

```r
get_icc(StartCY_comp)
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_Comp ~ (1 | StartCY)
## 
##   ICC (StartCY): 0.034316
```

```r
coef(StartCY_comp)
```

```
## $StartCY
##      (Intercept)
## 2008    1.402589
## 2009    1.523072
## 2010    1.413140
## 2011    1.247248
## 2012    1.274083
## 2013    1.542333
## 2014    1.778452
## 2015    2.098333
## 2016    2.276914
## 
## attr(,"class")
## [1] "coef.mer"
```

```r
get_icc(SignedMonth_comp)
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_Comp ~ (1 | StartCY:SignedMonth)
## 
##   ICC (StartCY:SignedMonth): 0.035931
```

```r
get_icc(NestedMonth_comp)
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_Comp ~ (1 | StartCY/SignedMonth)
## 
##   ICC (SignedMonth:StartCY): 0.005281
##               ICC (StartCY): 0.033118
```

```r
get_icc(NAICS_StartCY_comp)
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_Comp ~ (1 | NAICS3/NAICS) + (1 | StartCY)
## 
##   ICC (NAICS:NAICS3): 0.213552
##         ICC (NAICS3): 0.058957
##        ICC (StartCY): 0.028867
```
Signed month has ICC values typically below 0.03 and it appears it can be safely left out as start calendar year captures most of the variance. Interestingly, while start year had been most prominent for ceiling breaches no longer stands out once NAICS are incoporated in the model.
##Customer
### Office

```r
load(file="Output//who_empty_models.rdata")

if(!exists("Office_term"))
  Office_term <- glmer(data=smp,
                       b_Term ~ (1 | Agency:Office)
                       , family=binomial(link="logit"))
get_icc(display=TRUE,Office_term)
```

```
## glmer(formula = b_Term ~ (1 | Agency:Office), data = smp, family = binomial(link = "logit"))
## coef.est  coef.se 
##    -4.81     0.06 
## 
## Error terms:
##  Groups        Name        Std.Dev.
##  Agency:Office (Intercept) 1.12    
##  Residual                  1.00    
## ---
## number of obs: 249999, groups: Agency:Office, 1296
## AIC = 24878.8, DIC = 22383.7
## deviance = 23629.2
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_Term ~ (1 | Agency:Office)
## 
##   ICC (Agency:Office): 0.276888
```

```r
if(!exists("Office_cbre"))
  Office_cbre <- glmer(data=smp,
                       b_CBre ~ (1 | Agency:Office)
                       , family=binomial(link="logit"))
get_icc(display=TRUE,Office_cbre)
```

```
## glmer(formula = b_CBre ~ (1 | Agency:Office), data = smp, family = binomial(link = "logit"))
## coef.est  coef.se 
##    -5.46     0.11 
## 
## Error terms:
##  Groups        Name        Std.Dev.
##  Agency:Office (Intercept) 2.15    
##  Residual                  1.00    
## ---
## number of obs: 249999, groups: Agency:Office, 1296
## AIC = 20239.1, DIC = 16464.2
## deviance = 18349.7
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_CBre ~ (1 | Agency:Office)
## 
##   ICC (Agency:Office): 0.584471
```

```r
if(!exists("Office_comp"))
  Office_comp <- glmer(data=smp,
                       b_Comp ~ (1 | Agency:Office)
                       , family=binomial(link="logit"))
get_icc(display=TRUE,Office_comp)
```

```
## glmer(formula = b_Comp ~ (1 | Agency:Office), data = smp, family = binomial(link = "logit"))
## coef.est  coef.se 
##     1.66     0.05 
## 
## Error terms:
##  Groups        Name        Std.Dev.
##  Agency:Office (Intercept) 1.75    
##  Residual                  1.00    
## ---
## number of obs: 249999, groups: Agency:Office, 1296
## AIC = 155770, DIC = 147398.1
## deviance = 151582.1
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_Comp ~ (1 | Agency:Office)
## 
##   ICC (Agency:Office): 0.482698
```

### Agency

```r
if(!exists("Agency_term"))
  Agency_term <- glmer(data=smp,
                       b_Term ~ (1 | Agency)
                       , family=binomial(link="logit"))
get_icc(display=TRUE,Agency_term)
```

```
## glmer(formula = b_Term ~ (1 | Agency), data = smp, family = binomial(link = "logit"))
## coef.est  coef.se 
##    -4.65     0.19 
## 
## Error terms:
##  Groups   Name        Std.Dev.
##  Agency   (Intercept) 0.92    
##  Residual             1.00    
## ---
## number of obs: 250000, groups: Agency, 24
## AIC = 27616.3, DIC = 27512.1
## deviance = 27562.2
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_Term ~ (1 | Agency)
## 
##   ICC (Agency): 0.203568
```

```r
if(!exists("Agency_cbre"))
  Agency_cbre <- glmer(data=smp,
                       b_CBre ~ (1 | Agency)
                       , family=binomial(link="logit"))
get_icc(display=TRUE,Agency_cbre)
```

```
## glmer(formula = b_CBre ~ (1 | Agency), data = smp, family = binomial(link = "logit"))
## coef.est  coef.se 
##    -3.82     0.21 
## 
## Error terms:
##  Groups   Name        Std.Dev.
##  Agency   (Intercept) 0.91    
##  Residual             1.00    
## ---
## number of obs: 250000, groups: Agency, 24
## AIC = 23502.6, DIC = 23373.3
## deviance = 23435.9
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_CBre ~ (1 | Agency)
## 
##   ICC (Agency): 0.200320
```

```r
if(!exists("Agency_comp"))
  Agency_comp <- glmer(data=smp,
                       b_Comp ~ (1 | Agency)
                       , family=binomial(link="logit"))
get_icc(display=TRUE,Agency_comp)
```

```
## glmer(formula = b_Comp ~ (1 | Agency), data = smp, family = binomial(link = "logit"))
## coef.est  coef.se 
##     1.14     0.11 
## 
## Error terms:
##  Groups   Name        Std.Dev.
##  Agency   (Intercept) 1.01    
##  Residual             1.00    
## ---
## number of obs: 250000, groups: Agency, 24
## AIC = 207451, DIC = 207230.2
## deviance = 207338.6
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_Comp ~ (1 | Agency)
## 
##   ICC (Agency): 0.237468
```

### Nested Office

```r
if(!exists("NestedOffice_term"))
  NestedOffice_term <- glmer(data=smp,
                             b_Term ~ (1 | Agency/Office)
                             , family=binomial(link="logit"))
get_icc(display=TRUE,NestedOffice_term)
```

```
## glmer(formula = b_Term ~ (1 | Agency/Office), data = smp, family = binomial(link = "logit"))
## coef.est  coef.se 
##    -4.81     0.06 
## 
## Error terms:
##  Groups        Name        Std.Dev.
##  Office:Agency (Intercept) 1.12    
##  Agency        (Intercept) 0.00    
##  Residual                  1.00    
## ---
## number of obs: 249999, groups: Office:Agency, 1296; Agency, 24
## AIC = 24880.8, DIC = 22383.7
## deviance = 23629.3
```

```
## [[1]]
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_Term ~ (1 | Agency/Office)
## 
##   ICC (Office:Agency): 0.276869
##          ICC (Agency): 0.000000
## 
## [[2]]
## [1] "Model failed to converge with max|grad| = 0.0254071 (tol = 0.001, component 1)"
```

```r
if(!exists("NestedOffice_cbre"))
  NestedOffice_cbre <- glmer(data=smp,
                             b_CBre ~ (1 | Agency/Office)
                             , family=binomial(link="logit"))
get_icc(display=TRUE,NestedOffice_cbre)
```

```
## glmer(formula = b_CBre ~ (1 | Agency/Office), data = smp, family = binomial(link = "logit"))
## coef.est  coef.se 
##    -4.45     0.29 
## 
## Error terms:
##  Groups        Name        Std.Dev.
##  Office:Agency (Intercept) 1.37    
##  Agency        (Intercept) 1.08    
##  Residual                  1.00    
## ---
## number of obs: 249999, groups: Office:Agency, 1296; Agency, 24
## AIC = 19894.2, DIC = 17091.2
## deviance = 18489.7
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_CBre ~ (1 | Agency/Office)
## 
##   ICC (Office:Agency): 0.295761
##          ICC (Agency): 0.184318
```

```r
if(!exists("NestedOffice_comp"))
  NestedOffice_comp <- glmer(data=smp,
                             b_Comp ~ (1 | Agency/Office)
                             , family=binomial(link="logit"))
get_icc(display=TRUE,NestedOffice_comp)
```

```
## glmer(formula = b_Comp ~ (1 | Agency/Office), data = smp, family = binomial(link = "logit"))
## coef.est  coef.se 
##     1.17     0.18 
## 
## Error terms:
##  Groups        Name        Std.Dev.
##  Office:Agency (Intercept) 1.55    
##  Agency        (Intercept) 0.97    
##  Residual                  1.00    
## ---
## number of obs: 249999, groups: Office:Agency, 1296; Agency, 24
## AIC = 155557, DIC = 147651.2
## deviance = 151600.8
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_Comp ~ (1 | Agency/Office)
## 
##   ICC (Office:Agency): 0.362118
##          ICC (Agency): 0.142641
```

### NAICS36 Office

```r
if(!exists("naics36_Office_term"))
  naics36_Office_term <- glmer(data=smp,
                             b_Term ~  (1 | NAICS3/NAICS) + (1 | Agency:Office)
                             , family=binomial(link="logit"))
get_icc(display=TRUE,naics36_Office_term)
```

```
## glmer(formula = b_Term ~ (1 | NAICS3/NAICS) + (1 | Agency:Office), 
##     data = smp, family = binomial(link = "logit"))
## coef.est  coef.se 
##    -4.84     0.08 
## 
## Error terms:
##  Groups        Name        Std.Dev.
##  Agency:Office (Intercept) 0.97    
##  NAICS:NAICS3  (Intercept) 0.52    
##  NAICS3        (Intercept) 0.32    
##  Residual                  1.00    
## ---
## number of obs: 249999, groups: Agency:Office, 1296; NAICS:NAICS3, 886; NAICS3, 79
## AIC = 24637.4, DIC = 21538.9
## deviance = 23084.1
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_Term ~ (1 | NAICS3/NAICS) + (1 | Agency:Office)
## 
##   ICC (Agency:Office): 0.203260
##    ICC (NAICS:NAICS3): 0.059131
##          ICC (NAICS3): 0.022486
```

```r
if(!exists("naics36_Office_cbre"))
  naics36_Office_cbre <- glmer(data=smp,
                             b_CBre ~  (1 | NAICS3/NAICS) + (1 | Agency:Office)
                             , family=binomial(link="logit"))
get_icc(display=TRUE,naics36_Office_cbre)
```

```
## glmer(formula = b_Comp ~ (1 | NAICS3/NAICS) + (1 | Agency:Office), 
##     data = smp, family = binomial(link = "logit"))
## coef.est  coef.se 
##     1.82     0.09 
## 
## Error terms:
##  Groups        Name        Std.Dev.
##  Agency:Office (Intercept) 1.73    
##  NAICS:NAICS3  (Intercept) 0.86    
##  NAICS3        (Intercept) 0.45    
##  Residual                  1.00    
## ---
## number of obs: 249999, groups: Agency:Office, 1296; NAICS:NAICS3, 886; NAICS3, 79
## AIC = 136716, DIC = 124616.2
## deviance = 130662.2
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_Comp ~ (1 | NAICS3/NAICS) + (1 | Agency:Office)
## 
##   ICC (Agency:Office): 0.414357
##    ICC (NAICS:NAICS3): 0.102832
##          ICC (NAICS3): 0.027815
```

```r
if(!exists("naics36_Office_comp"))
  naics36_Office_comp <- glmer(data=smp,
                             b_Comp ~  (1 | NAICS3/NAICS) + (1 | Agency:Office)
                             , family=binomial(link="logit"))
get_icc(display=TRUE,naics36_Office_comp)
```

```
## glmer(formula = b_Comp ~ (1 | NAICS3/NAICS) + (1 | Agency:Office), 
##     data = smp, family = binomial(link = "logit"))
## coef.est  coef.se 
##     1.82     0.09 
## 
## Error terms:
##  Groups        Name        Std.Dev.
##  Agency:Office (Intercept) 1.73    
##  NAICS:NAICS3  (Intercept) 0.86    
##  NAICS3        (Intercept) 0.45    
##  Residual                  1.00    
## ---
## number of obs: 249999, groups: Agency:Office, 1296; NAICS:NAICS3, 886; NAICS3, 79
## AIC = 136716, DIC = 124616.2
## deviance = 130662.2
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_Comp ~ (1 | NAICS3/NAICS) + (1 | Agency:Office)
## 
##   ICC (Agency:Office): 0.414357
##    ICC (NAICS:NAICS3): 0.102832
##          ICC (NAICS3): 0.027815
```
Office turns out to explain much of the variation previously captured by NAICS36, double checking next out NAICS6 alone holds up.

### NAICS06 Office

```r
if(!exists("naics06_Office_term"))
  naics06_Office_term <- glmer(data=smp,
                             b_Term ~  (1 | NAICS) + (1 | Agency:Office)
                             , family=binomial(link="logit"))
get_icc(display=TRUE,naics06_Office_term)
```

```
## glmer(formula = b_Term ~ (1 | NAICS) + (1 | Agency:Office), data = smp, 
##     family = binomial(link = "logit"))
## coef.est  coef.se 
##    -4.84     0.07 
## 
## Error terms:
##  Groups        Name        Std.Dev.
##  Agency:Office (Intercept) 0.99    
##  NAICS         (Intercept) 0.61    
##  Residual                  1.00    
## ---
## number of obs: 249999, groups: Agency:Office, 1296; NAICS, 886
## AIC = 24650.1, DIC = 21460.1
## deviance = 23052.1
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_Term ~ (1 | NAICS) + (1 | Agency:Office)
## 
##   ICC (Agency:Office): 0.210146
##           ICC (NAICS): 0.079708
```

```r
if(!exists("naics06_Office_cbre"))
  naics06_Office_cbre <- glmer(data=smp,
                             b_CBre ~  (1 | NAICS) + (1 | Agency:Office)
                             , family=binomial(link="logit"))
get_icc(display=TRUE,naics06_Office_cbre)
```

```
## glmer(formula = b_CBre ~ (1 | NAICS) + (1 | Agency:Office), data = smp, 
##     family = binomial(link = "logit"))
## coef.est  coef.se 
##    -5.44     0.10 
## 
## Error terms:
##  Groups        Name        Std.Dev.
##  Agency:Office (Intercept) 1.58    
##  NAICS         (Intercept) 0.93    
##  Residual                  1.00    
## ---
## number of obs: 249999, groups: Agency:Office, 1296; NAICS, 886
## AIC = 19335.7, DIC = 14887.9
## deviance = 17108.8
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_CBre ~ (1 | NAICS) + (1 | Agency:Office)
## 
##   ICC (Agency:Office): 0.374932
##           ICC (NAICS): 0.130449
```

```r
if(!exists("naics06_Office_comp"))
  naics06_Office_comp <- glmer(data=smp,
                             b_Comp ~  (1 | NAICS) + (1 | Agency:Office)
                             , family=binomial(link="logit"))
get_icc(display=TRUE,naics06_Office_comp)
```

```
## glmer(formula = b_Comp ~ (1 | NAICS) + (1 | Agency:Office), data = smp, 
##     family = binomial(link = "logit"))
## coef.est  coef.se 
##     1.86     0.07 
## 
## Error terms:
##  Groups        Name        Std.Dev.
##  Agency:Office (Intercept) 1.73    
##  NAICS         (Intercept) 0.94    
##  Residual                  1.00    
## ---
## number of obs: 249999, groups: Agency:Office, 1296; NAICS, 886
## AIC = 136752, DIC = 124573.4
## deviance = 130659.6
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_Comp ~ (1 | NAICS) + (1 | Agency:Office)
## 
##   ICC (Agency:Office): 0.418561
##           ICC (NAICS): 0.122903
```
NAICS06 does come in a bit higher in summed VIF than NAICS36, but only a little.

### NAICS 4/6 Office

```r
if(!exists("naics46_Office_term"))
naics46_Office_term <- glmer(data=smp,
                             b_Term ~  (1 | NAICS4/NAICS) + (1 | Agency:Office)
                             , family=binomial(link="logit"))
get_icc(display=TRUE,naics46_Office_term)
```

```
## glmer(formula = b_Term ~ (1 | NAICS4/NAICS) + (1 | Agency:Office), 
##     data = smp, family = binomial(link = "logit"))
## coef.est  coef.se 
##    -4.84     0.07 
## 
## Error terms:
##  Groups        Name        Std.Dev.
##  Agency:Office (Intercept) 0.98    
##  NAICS:NAICS4  (Intercept) 0.55    
##  NAICS4        (Intercept) 0.26    
##  Residual                  1.00    
## ---
## number of obs: 249999, groups: Agency:Office, 1296; NAICS:NAICS4, 886; NAICS4, 258
## AIC = 24648.8, DIC = 21472.9
## deviance = 23056.8
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_Term ~ (1 | NAICS4/NAICS) + (1 | Agency:Office)
## 
##   ICC (Agency:Office): 0.208233
##    ICC (NAICS:NAICS4): 0.065879
##          ICC (NAICS4): 0.015029
```

```r
if(!exists("naics46_Office_cbre"))
  naics46_Office_cbre <- glmer(data=smp,
                             b_CBre ~  (1 | NAICS4/NAICS) + (1 | Agency:Office)
                             , family=binomial(link="logit"))
get_icc(display=TRUE,naics46_Office_cbre)
```

```
## glmer(formula = b_CBre ~ (1 | NAICS4/NAICS) + (1 | Agency:Office), 
##     data = smp, family = binomial(link = "logit"))
## coef.est  coef.se 
##    -5.33     0.11 
## 
## Error terms:
##  Groups        Name        Std.Dev.
##  Agency:Office (Intercept) 1.53    
##  NAICS:NAICS4  (Intercept) 0.65    
##  NAICS4        (Intercept) 0.65    
##  Residual                  1.00    
## ---
## number of obs: 249999, groups: Agency:Office, 1296; NAICS:NAICS4, 886; NAICS4, 258
## AIC = 19313.8, DIC = 15040.9
## deviance = 17173.3
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_CBre ~ (1 | NAICS4/NAICS) + (1 | Agency:Office)
## 
##   ICC (Agency:Office): 0.362726
##    ICC (NAICS:NAICS4): 0.064222
##          ICC (NAICS4): 0.065897
```

```r
if(!exists("naics46_Office_comp"))
  naics46_Office_comp <- glmer(data=smp,
                             b_Comp ~  (1 | NAICS4/NAICS) + (1 | Agency:Office)
                             , family=binomial(link="logit"))
get_icc(display=TRUE,naics46_Office_comp)
```

```
## glmer(formula = b_Comp ~ (1 | NAICS4/NAICS) + (1 | Agency:Office), 
##     data = smp, family = binomial(link = "logit"))
## coef.est  coef.se 
##     1.86     0.07 
## 
## Error terms:
##  Groups        Name        Std.Dev.
##  Agency:Office (Intercept) 1.73    
##  NAICS:NAICS4  (Intercept) 0.81    
##  NAICS4        (Intercept) 0.51    
##  Residual                  1.00    
## ---
## number of obs: 249999, groups: Agency:Office, 1296; NAICS:NAICS4, 886; NAICS4, 258
## AIC = 136719, DIC = 124611.7
## deviance = 130661.4
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_Comp ~ (1 | NAICS4/NAICS) + (1 | Agency:Office)
## 
##   ICC (Agency:Office): 0.415800
##    ICC (NAICS:NAICS4): 0.092036
##          ICC (NAICS4): 0.035598
```


### NAICS When Office

```r
# if(!exists("naics36_When_Office_term"))
#   naics36_When_Office_term <- glmer(data=smp,
#                              b_Term ~  (1 | NAICS3/NAICS) + (1 | StartCY) + (1 | Agency:Office)
#                              , family=binomial(link="logit"))
# get_icc(display=TRUE,naics36_When_Office_term)
# 
# if(!exists("naics36_When_Office_cbre"))
#   naics36_When_Office_cbre <- glmer(data=smp,
#                              b_CBre ~  (1 | NAICS3/NAICS)+ (1 | StartCY) + (1 | Agency;Office)
#                              , family=binomial(link="logit"))
# get_icc(display=TRUE,naics36_When_Office_cbre)
# # odel failed to converge with max|grad| = 0.343334 (tol = 0.001, component 1)Model is nearly unidentifiable: very large eigenvalue
# #  - Rescale variables?
# if(!exists("naics36_When_Office_comp"))
#   naics36_When_Office_comp <- glmer(data=smp,
#                              b_Comp ~  (1 | NAICS3/NAICS)+ (1 | StartCY) + (1 | Agency:Office)
#                              , family=binomial(link="logit"))
# get_icc(display=TRUE,naics36_When_Office_comp)
```


### Compare Who

```r
save(Office_cbre,Office_term,Office_comp,
     Agency_cbre,Agency_term,Agency_comp,
     NestedOffice_cbre,NestedOffice_term,NestedOffice_comp,
     naics36_Office_cbre,naics36_Office_term,naics36_Office_comp,
     naics06_Office_cbre,naics06_Office_term,naics06_Office_comp,
     naics46_Office_cbre,naics46_Office_term,naics46_Office_comp,
     # naics36_When_Office_cbre,naics36_When_Office_term,naics36_When_Office_comp,
     file="Output//who_empty_models.rdata")


get_icc(naics36_term)
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_Term ~ (1 | NAICS3/NAICS)
## 
##   ICC (NAICS:NAICS3): 0.141022
##         ICC (NAICS3): 0.080624
```

```r
get_icc(NAICS_StartCY_term)
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_Term ~ (1 | NAICS3/NAICS) + (1 | StartCY)
## 
##   ICC (NAICS:NAICS3): 0.140341
##         ICC (NAICS3): 0.086116
##        ICC (StartCY): 0.010416
```

```r
get_icc(Office_term)
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_Term ~ (1 | Agency:Office)
## 
##   ICC (Agency:Office): 0.276888
```

```r
get_icc(Agency_term)
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_Term ~ (1 | Agency)
## 
##   ICC (Agency): 0.203568
```

```r
get_icc(NestedOffice_term)
```

```
## [[1]]
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_Term ~ (1 | Agency/Office)
## 
##   ICC (Office:Agency): 0.276869
##          ICC (Agency): 0.000000
## 
## [[2]]
## [1] "Model failed to converge with max|grad| = 0.0254071 (tol = 0.001, component 1)"
```

```r
get_icc(naics36_Office_term)
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_Term ~ (1 | NAICS3/NAICS) + (1 | Agency:Office)
## 
##   ICC (Agency:Office): 0.203260
##    ICC (NAICS:NAICS3): 0.059131
##          ICC (NAICS3): 0.022486
```

```r
get_icc(naics06_Office_term)
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_Term ~ (1 | NAICS) + (1 | Agency:Office)
## 
##   ICC (Agency:Office): 0.210146
##           ICC (NAICS): 0.079708
```

```r
get_icc(naics46_Office_term)
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_Term ~ (1 | NAICS4/NAICS) + (1 | Agency:Office)
## 
##   ICC (Agency:Office): 0.208233
##    ICC (NAICS:NAICS4): 0.065879
##          ICC (NAICS4): 0.015029
```

```r
# get_icc(naics36_When_Office_term)


get_icc(naics36_cbre)
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_CBre ~ (1 | NAICS3/NAICS)
## 
##   ICC (NAICS:NAICS3): 0.193197
##         ICC (NAICS3): 0.190459
```

```r
get_icc(NAICS_StartCY_cbre)
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_CBre ~ (1 | NAICS3/NAICS) + (1 | StartCY)
## 
##   ICC (NAICS:NAICS3): 0.176667
##         ICC (NAICS3): 0.180294
##        ICC (StartCY): 0.023467
```

```r
get_icc(Office_cbre)
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_CBre ~ (1 | Agency:Office)
## 
##   ICC (Agency:Office): 0.584471
```

```r
get_icc(Agency_cbre)
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_CBre ~ (1 | Agency)
## 
##   ICC (Agency): 0.200320
```

```r
get_icc(NestedOffice_cbre)
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_CBre ~ (1 | Agency/Office)
## 
##   ICC (Office:Agency): 0.295761
##          ICC (Agency): 0.184318
```

```r
get_icc(naics36_Office_cbre)
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_Comp ~ (1 | NAICS3/NAICS) + (1 | Agency:Office)
## 
##   ICC (Agency:Office): 0.414357
##    ICC (NAICS:NAICS3): 0.102832
##          ICC (NAICS3): 0.027815
```

```r
get_icc(naics06_Office_cbre)
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_CBre ~ (1 | NAICS) + (1 | Agency:Office)
## 
##   ICC (Agency:Office): 0.374932
##           ICC (NAICS): 0.130449
```

```r
get_icc(naics46_Office_cbre)
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_CBre ~ (1 | NAICS4/NAICS) + (1 | Agency:Office)
## 
##   ICC (Agency:Office): 0.362726
##    ICC (NAICS:NAICS4): 0.064222
##          ICC (NAICS4): 0.065897
```

```r
# get_icc(naics36_When_Office_cbre)

get_icc(naics36_comp)
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_Comp ~ (1 | NAICS3/NAICS)
## 
##   ICC (NAICS:NAICS3): 0.202860
##         ICC (NAICS3): 0.085484
```

```r
get_icc(NAICS_StartCY_comp)
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_Comp ~ (1 | NAICS3/NAICS) + (1 | StartCY)
## 
##   ICC (NAICS:NAICS3): 0.213552
##         ICC (NAICS3): 0.058957
##        ICC (StartCY): 0.028867
```

```r
get_icc(Office_comp)
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_Comp ~ (1 | Agency:Office)
## 
##   ICC (Agency:Office): 0.482698
```

```r
get_icc(Agency_comp)
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_Comp ~ (1 | Agency)
## 
##   ICC (Agency): 0.237468
```

```r
get_icc(NestedOffice_comp)
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_Comp ~ (1 | Agency/Office)
## 
##   ICC (Office:Agency): 0.362118
##          ICC (Agency): 0.142641
```

```r
get_icc(naics36_Office_comp)
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_Comp ~ (1 | NAICS3/NAICS) + (1 | Agency:Office)
## 
##   ICC (Agency:Office): 0.414357
##    ICC (NAICS:NAICS3): 0.102832
##          ICC (NAICS3): 0.027815
```

```r
get_icc(naics06_Office_comp)
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_Comp ~ (1 | NAICS) + (1 | Agency:Office)
## 
##   ICC (Agency:Office): 0.418561
##           ICC (NAICS): 0.122903
```

```r
get_icc(naics46_Office_comp)
```

```
## 
## Generalized linear mixed model
##  Family: binomial (logit)
## Formula: b_Comp ~ (1 | NAICS4/NAICS) + (1 | Agency:Office)
## 
##   ICC (Agency:Office): 0.415800
##    ICC (NAICS:NAICS4): 0.092036
##          ICC (NAICS4): 0.035598
```

```r
# get_icc(naics36_When_Office_comp)
```
There is some contradiction in which model best serves this category. For Comp, the nested office has the largest summed ICC, but for term this results in an failure to converge and a 0 ICC for agency. Agency:Office alone is respectable for all three so that is used in the combination attempt.
