---
title: "Ceiling_Breach_Examination"
author: "Greg Sanders"
date: "June 24, 2019"
output: 
  html_document: 
    fig_caption: yes
    keep_md: yes
    number_sections: yes
    toc: yes
---


#Setup

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
## arm (Version 1.10-1, built: 2018-4-12)
```

```
## Working directory is C:/Users/gsand/Repositories/Vendor/scripts
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
## 
## Please cite as:
```

```
##  Hlavac, Marek (2018). stargazer: Well-Formatted Regression and Summary Statistics Tables.
```

```
##  R package version 5.2.2. https://CRAN.R-project.org/package=stargazer
```

```
## Version:  1.36.23
## Date:     2017-03-03
## Author:   Philip Leifeld (University of Glasgow)
## 
## Please cite the JSS article in your publications -- see citation("texreg").
```

```
## 
## Attaching package: 'texreg'
```

```
## The following object is masked from 'package:arm':
## 
##     coefplot
```

```
## Loading required package: carData
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

```
## 
## Attaching package: 'scales'
```

```
## The following object is masked from 'package:arm':
## 
##     rescale
```


```r
# load(file="../Data/Clean/transformed_def.Rdata")
load(file="..\\data\\semi_clean\\CBre_pre_clean.rdata")
# W912UM <- def %>% filter(Office=="W912UM")
# W912UMtrans<-read.delim(file="..\\data\\semi_clean\\W912UM_trans.csv", sep=",")
# W912UMtrans<-remove_bom(W912UMtrans)
# cbre_preclean<-def %>% filter(b_CBre==1)
# cbre_preclean$qGrowth<-Hmisc::cut2(cbre_preclean$p_CBre-1,c(1,10))
# summary(cbre_preclean$qGrowth)
# save(W912UM,W912UMtrans,cbre_preclean,file="..\\data\\semi_clean\\CBre_pre_clean.rdata")
```



# Before Cleaning

```r
nrow(cbre_preclean %>% filter((p_CBre-1)>1))
```

```
## [1] 11954
```

```r
nrow(cbre_preclean %>% filter((p_CBre-1)>10))
```

```
## [1] 1462
```

```r
nrow(cbre_preclean %>% filter((p_CBre-1)>100))
```

```
## [1] 419
```

```r
nrow(cbre_preclean %>% filter((p_CBre-1)>100 & UnmodifiedContractBaseAndAllOptionsValue_Then_Year<=0))
```

```
## [1] 232
```

```r
summary(cbre_preclean$Ceil[(cbre_preclean$p_CBre-1)>10 & cbre_preclean$UnmodifiedContractBaseAndAllOptionsValue_Then_Year>0])
```

```
##    [0,15k) [15k,100k)  [100k,1m)   [1m,10m)  [10m,75m)     [75m+] 
##        859        213        120         34          4          0
```

```r
summary(cbre_preclean$Ceil[(cbre_preclean$p_CBre-1)>100 & cbre_preclean$UnmodifiedContractBaseAndAllOptionsValue_Then_Year>0])
```

```
##    [0,15k) [15k,100k)  [100k,1m)   [1m,10m)  [10m,75m)     [75m+] 
##        138         32         15          2          0          0
```

```r
cbre_preclean$Why_Outlier<-NA
cbre_preclean$Why_Outlier[cbre_preclean$UnmodifiedContractBaseAndAllOptionsValue_Then_Year<=0]<-"No Unmodified Ceiling"
cbre_preclean$Why_Outlier[is.na(cbre_preclean$Why_Outlier)&
                   cbre_preclean$Action_Obligation_Then_Year*2>=cbre_preclean$UnmodifiedContractBaseAndAllOptionsValue_Then_Year+
                   cbre_preclean$n_CBre]<-
  "Obligations at least half Orig+CRai"
cbre_preclean$Why_Outlier[is.na(cbre_preclean$Why_Outlier)&
                   cbre_preclean$Office=="W912UM"]<-
  "Korean Office W912UM"
cbre_preclean$Why_Outlier[is.na(cbre_preclean$Why_Outlier)&
                   cbre_preclean$n_CBre>=2.5e8]<-
  ">=$250M, Insepect"
cbre_preclean$Why_Outlier[is.na(cbre_preclean$Why_Outlier)&
                   cbre_preclean$p_CBre-1>10]<-
  "Other Unexplained 10x Ceiling Breach"
cbre_preclean$Why_Outlier<-factor(cbre_preclean$Why_Outlier,
                         levels=c(
                           "No Unmodified Ceiling",
                           "Obligations at least half Orig+CRai",
                           "Later Deobligated",
                           "Korean Office W912UM",
                           ">=$250M, Insepect",
                           "Other Unexplained 10x Ceiling Breach"
                         ))
summary(cbre_preclean$Why_Outlier[(cbre_preclean$p_CBre-1)>10])
```

```
##                No Unmodified Ceiling  Obligations at least half Orig+CRai 
##                                  232                                 1146 
##                    Later Deobligated                 Korean Office W912UM 
##                                    0                                   11 
##                    >=$250M, Insepect Other Unexplained 10x Ceiling Breach 
##                                    5                                   68
```

```r
summary(cbre_preclean$Why_Outlier)
```

```
##                No Unmodified Ceiling  Obligations at least half Orig+CRai 
##                                  232                                91626 
##                    Later Deobligated                 Korean Office W912UM 
##                                    0                                  198 
##                    >=$250M, Insepect Other Unexplained 10x Ceiling Breach 
##                                    8                                   68 
##                                 NA's 
##                                 2358
```

```r
p_outlier_summary<-cbre_preclean %>% filter(p_CBre-1>10) %>% group_by(Why_Outlier) %>%
  dplyr::summarise(nContract=length(n_CBre),
    SumOfChangeOrderCeilingGrowth=sum(n_CBre),
                   MaxOfChangeOrderCeilingGrowth=max(n_CBre),
                   SumOfAction_Obligation_Then_Year=sum(Action_Obligation_Then_Year))


n_outlier_summary<-cbre_preclean %>% filter(n_CBre>2.5e8) %>% group_by(Why_Outlier) %>%
  dplyr::summarise(nContract=length(n_CBre),
    SumOfChangeOrderCeilingGrowth=sum(n_CBre),
                   MaxOfChangeOrderCeilingGrowth=max(n_CBre),
                   SumOfAction_Obligation_Then_Year=sum(Action_Obligation_Then_Year))


nrow(cbre_preclean %>% filter((n_CBre)>=1e3))
```

```
## [1] 76252
```

```r
nrow(cbre_preclean %>% filter((n_CBre)>=1e6))
```

```
## [1] 4199
```

```r
nrow(cbre_preclean %>% filter((n_CBre)>=1e7))
```

```
## [1] 682
```

```r
nrow(cbre_preclean %>% filter((n_CBre)>=1e8))
```

```
## [1] 111
```

```r
nrow(cbre_preclean %>% filter((n_CBre)>=2.5e8))
```

```
## [1] 44
```

```r
nrow(cbre_preclean %>% filter((n_CBre)>=1e9))
```

```
## [1] 12
```

```r
nrow(cbre_preclean %>% filter((n_CBre)>=1e10))
```

```
## [1] 3
```

```r
nrow(cbre_preclean %>% filter((n_CBre)>=2e10))
```

```
## [1] 2
```

```r
summary(cbre_preclean$Ceil[cbre_preclean$n_CBre>=1e6])
```

```
##    [0,15k) [15k,100k)  [100k,1m)   [1m,10m)  [10m,75m)     [75m+] 
##         59         97        488       1777       1366        412
```

```r
summary(cbre_preclean$Ceil[cbre_preclean$n_CBre>=1e9])
```

```
##    [0,15k) [15k,100k)  [100k,1m)   [1m,10m)  [10m,75m)     [75m+] 
##          0          1          1          2          0          8
```

```r
write.csv(file="..\\Data\\semi_clean\\p_CBre_outliers.csv",cbre_preclean %>% filter((p_CBre-1)>10),row.names = FALSE)
write.csv(file="..\\Data\\semi_clean\\n_CBre_outliers.csv",cbre_preclean %>% filter(n_CBre>=2.5e8),row.names = FALSE)
```
Examining cases of large ceiling growth, 1462 contracts experienced greater than 10 fold growth. An increase of that side strains credulity, even in high risk defense contracting. While by no means impossible, the more likely explaination is a misrecorded initial ceiling.

The study team broke down the outliers into 6 categories:


Why_Outlier                             nContract   SumOfChangeOrderCeilingGrowth   MaxOfChangeOrderCeilingGrowth   SumOfAction_Obligation_Then_Year
-------------------------------------  ----------  ------------------------------  ------------------------------  ---------------------------------
No Unmodified Ceiling                         232                        81591718                        20862815                          383117339
Obligations at least half Orig+CRai          1146                      4418306936                       769789464                         8799240742
Korean Office W912UM                           11                      7681224515                      5364187370                           16969010
>=$250M, Insepect                               5                    476290148280                    344739578535                           29052394
Other Unexplained 10x Ceiling Breach           68                       484851277                        95979870                           86701052


* No Unmodified Ceiling: Contracts with an initial ceiling <=0. These are eliminated from the sample as missing data.
* Obligations at least half Orig+CRai: For this category, total obligations of the contract were at least half the value of the initial ceiling plus ceiling growth under change orders. These contrats have had spending that massively exceeded their original ceiling, so the growth in absolute terrms seems plausible. This category accounts for the overwhelming majority of outlier spending but only a tiny fraction of change order growth.
* Later Deobligated: The change order growth metrics only counts increases. These may simply have been mistaken increases, as when including deobligation the growth no longer exceeded 10x the original ceiling. The number, obligations, and change order growth of these contracts are comparatively small, and thus should not distort the overall data.
* Korean Office W912UM refers to a contracting office that sometimes records base and all options values in Korean Won, approximate exchange rate 1,000 Won : 1 USD. 
* There are nrow(cbre_preclean %>% dplyr::filter(Why_Outlier ==">=$250M, Insepect" & (p_CBre-1)>10)) contracts with ceiling growth of over $250 million that account for hundreds of billions in change order growth. These merit manual inspection.
* Finally a few score contrats have unexplained growth, but remain below the $10M threshold. The quantity and magnitude of these contrats is not sufficient to risk the overall model.

This examination left the study team less confident in percentage growth as a metric, especially in extreme cases, while increasing the study team's confidence in measures of growth in absoute term. In the worst case, simply removing all of the unexplained over  10 million contracts from the sample would reduce the number of contracts by a tiny amount and reduce the spending accounted for by  2.9052394\times 10^{7}.

Shifting the focus to all contracts with growth of at least 250 million, there are far fewer contracts that account for far more money.


Why_Outlier                            nContract   SumOfChangeOrderCeilingGrowth   MaxOfChangeOrderCeilingGrowth   SumOfAction_Obligation_Then_Year
------------------------------------  ----------  ------------------------------  ------------------------------  ---------------------------------
Obligations at least half Orig+CRai           11                      5525269281                       992698908                        16999937992
Korean Office W912UM                          24                     27401542851                      5364187370                          197697808
>=$250M, Insepect                              8                    479197384535                    344739578535                         1611779996



Inspecting W912UM, either to remove or fix its oversized growth, is an imperative as it accounts for the majority of these contracts or task orders. Even so, there are still 8 That merit special inspection for given that there growth far outpaces their spending.


## Ceiling Growth

```r
(
ggplot(cbre_preclean, aes(x=UnmodifiedContractBaseAndAllOptionsValue_Then_Year,y=p_CBre-1)) +#,color=qGrowth
  geom_point(alpha=0.25,shape=".")+
  # theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_x_log10()+scale_y_log10()+
  #+
  geom_vline(xintercept = c(1,10,100))+#+geom_vline(xintercept = 0.1)+
# facet_wrap(~Ceil,scales="free_y")+#+, space="free_y"
  labs(title="Distribution of Ceiling Breaches",
       y="Percent of Growth in  Ceiling",
       x="Unmodified Contract Ceiling")#,
       # fill="Termination Completion"
)
```

```
## Warning: Transformation introduced infinite values in continuous x-axis
```

![](Ceiling_Breach_Examination_files/figure-html/CeilingGrowthGraphs-1.png)<!-- -->

```r
(
ggplot(cbre_preclean, aes(x=UnmodifiedContractBaseAndAllOptionsValue_Then_Year,y=n_CBre)) +#,color=qGrowth
  geom_point(alpha=0.25,shape=".")+
  # theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_x_log10()+scale_y_log10()+
  #+
  geom_vline(xintercept = c(1,10,100))+#+geom_vline(xintercept = 0.1)+
# facet_wrap(~Ceil,scales="free_y")+#+, space="free_y"
  labs(title="Distribution of Ceiling Breaches",
       y="Percent of Growth in  Ceiling",
       x="Unmodified Contract Ceiling")#,
       # fill="Termination Completion"
)
```

```
## Warning: Transformation introduced infinite values in continuous x-axis
```

![](Ceiling_Breach_Examination_files/figure-html/CeilingGrowthGraphs-2.png)<!-- -->

```r
(
ggplot(cbre_preclean, aes(x=UnmodifiedContractBaseAndAllOptionsValue_Then_Year+ChangeOrderBaseAndAllOptionsValue,y=Action_Obligation_Then_Year)) +#,color=qGrowth
  geom_point(alpha=0.25,shape=".")+
  # theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_x_log10()+scale_y_log10()#+
  #+
#   geom_vline(xintercept = c(1,10,100))+#+geom_vline(xintercept = 0.1)+
# # facet_wrap(~Ceil,scales="free_y")+#+, space="free_y"
#   labs(title="Distribution of Ceiling Breaches",
#        y="Percent of Growth in  Ceiling",
#        x="Unmodified Contract Ceiling")#,
#        # fill="Termination Completion"
)
```

```
## Warning in self$trans$transform(x): NaNs produced
```

```
## Warning: Transformation introduced infinite values in continuous y-axis
```

```
## Warning: Removed 24 rows containing missing values (geom_point).
```

![](Ceiling_Breach_Examination_files/figure-html/CeilingGrowthGraphs-3.png)<!-- -->

```r
summary(cbre_preclean$ChangeOrderBaseAndAllOptionsValue)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## 0.000e+00 1.578e+03 9.159e+03 5.824e+06 5.252e+04 3.447e+11
```

```r
(
ggplot(cbre_preclean, aes(x=n_CBre,y=ChangeOrderBaseAndAllOptionsValue)) +#,color=qGrowth
  geom_point(alpha=0.25,shape=".")+
  # theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_x_log10()+scale_y_log10()#+
  #+
#   geom_vline(xintercept = c(1,10,100))+#+geom_vline(xintercept = 0.1)+
# # facet_wrap(~Ceil,scales="free_y")+#+, space="free_y"
#   labs(title="Distribution of Ceiling Breaches",
#        y="Percent of Growth in  Ceiling",
#        x="Unmodified Contract Ceiling")#,
#        # fill="Termination Completion"
)
```

![](Ceiling_Breach_Examination_files/figure-html/CeilingGrowthGraphs-4.png)<!-- -->

```r
(
ggplot(cbre_preclean, aes(x=p_CBre-1,fill=qGrowth)) +
  geom_histogram(bins=100)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_x_log10()+
  #+
  geom_vline(xintercept = c(1,10,100))+#+geom_vline(xintercept = 0.1)+
facet_wrap(~Ceil,scales="free_y")+#+, space="free_y"
  labs(title="Distribution of Ceiling Breaches",
       y="Contract Count",
       x="Percent of Growth in  Ceiling")#,
       # fill="Termination Completion"
)
```

```
## Warning: Removed 232 rows containing non-finite values (stat_bin).
```

![](Ceiling_Breach_Examination_files/figure-html/CeilingGrowthGraphs-5.png)<!-- -->

```r
(
ggplot(cbre_preclean, aes(x=n_CBre,fill=qGrowth)) +
  geom_histogram(bins=100)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_x_log10()+
  #+
  geom_vline(xintercept = 1)#+geom_vline(xintercept = 0.1)+
#facet_grid(NoPreTermObl~.,scales="free_y", space="free_y")+
  # labs(title="Distribution of Contracts with Obligations After Last termination",
  #      y="Contract Count",
  #      x="Percent of Obligations After Day of Termination",
  #      fill="Termination Completion"
)
```

![](Ceiling_Breach_Examination_files/figure-html/CeilingGrowthGraphs-6.png)<!-- -->

```r
(
ggplot(cbre_preclean, aes(x=n_CBre,fill=qGrowth)) +
  geom_histogram(bins=100)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_x_log10()+
  #+
  geom_vline(xintercept = 1)+
facet_wrap(~Ceil,scales="free_y")#+, space="free_y"
#+geom_vline(xintercept = 0.1)+
#facet_grid(NoPreTermObl~.,scales="free_y", space="free_y")+
  # labs(title="Distribution of Contracts with Obligations After Last termination",
  #      y="Contract Count",
  #      x="Percent of Obligations After Day of Termination",
  #      fill="Termination Completion"
)
```

![](Ceiling_Breach_Examination_files/figure-html/CeilingGrowthGraphs-7.png)<!-- -->

## W912IM

### Contract Initial Examination

```r
sum(W912UM$Action_Obligation_Then_Year[])
```

```
## [1] 1441699342
```

```r
(
ggplot(W912UM, aes(x=UnmodifiedContractBaseAndAllOptionsValue_Then_Year+ChangeOrderBaseAndAllOptionsValue,y=Action_Obligation_Then_Year)) +#,color=qGrowth
  geom_point(alpha=0.25,shape=".")+
  # theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_x_log10()+scale_y_log10()+
  #+
#   geom_vline(xintercept = c(1,10,100))+#+geom_vline(xintercept = 0.1)+
facet_wrap(~StartFY,scales="free_y")#+, space="free_y"
#   labs(title="Distribution of Ceiling Breaches",
#        y="Percent of Growth in  Ceiling",
#        x="Unmodified Contract Ceiling")#,
#        # fill="Termination Completion"
)
```

```
## Warning in self$trans$transform(x): NaNs produced
```

```
## Warning: Transformation introduced infinite values in continuous x-axis
```

```
## Warning: Transformation introduced infinite values in continuous y-axis
```

```
## Warning: Removed 4 rows containing missing values (geom_point).
```

![](Ceiling_Breach_Examination_files/figure-html/W912UM-1.png)<!-- -->

```r
summary(W912UM$UnmodifiedContractBaseAndAllOptionsValue_Then_Year)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## 0.000e+00 6.052e+04 4.989e+05 4.717e+08 1.341e+08 8.548e+10
```

```r
W912UM$unmodWon<-NA
W912UM$unmodWon[W912UM$UnmodifiedContractBaseAndAllOptionsValue_Then_Year>=W912UM$Action_Obligation_Then_Year*400&
                  W912UM$Action_Obligation_Then_Year>0]<-'WON Unm'
W912UM$unmodWon[is.na(W912UM$unmodWon) &
                    W912UM$UnmodifiedContractBaseAndAllOptionsValue_Then_Year>=W912UM$Action_Obligation_Then_Year*20 &
                  W912UM$UnmodifiedContractBaseAndAllOptionsValue_Then_Year>10000
                  ]<-'? Unm'
W912UM$unmodWon[is.na(W912UM$unmodWon) &
                    (W912UM$UnmodifiedContractBaseAndAllOptionsValue_Then_Year<W912UM$Action_Obligation_Then_Year*20|
                  W912UM$UnmodifiedContractBaseAndAllOptionsValue_Then_Year<10000)]<-'$ Unm'
summary(factor(W912UM$unmodWon))
```

```
##   $ Unm   ? Unm WON Unm 
##    1083      48     805
```

```r
W912UM$changeWon<-NA
W912UM$changeWon[abs(W912UM$ChangeOrderBaseAndAllOptionsValue)==0]<-'0 Chg'
W912UM$changeWon[abs(W912UM$ChangeOrderBaseAndAllOptionsValue)>=W912UM$Action_Obligation_Then_Year*100&
                  W912UM$ChangeOrderBaseAndAllOptionsValue>10000]<-'WON Chg'
W912UM$changeWon[is.na(W912UM$changeWon) &
                    abs(W912UM$ChangeOrderBaseAndAllOptionsValue)>=W912UM$Action_Obligation_Then_Year*10&
                  W912UM$ChangeOrderBaseAndAllOptionsValue>10000]<-'? Chg'
W912UM$changeWon[is.na(W912UM$changeWon) &
                  (abs(W912UM$ChangeOrderBaseAndAllOptionsValue)<W912UM$Action_Obligation_Then_Year*10|
                   W912UM$ChangeOrderBaseAndAllOptionsValue<=10000)]<-'$ Chg'
summary(factor(W912UM$changeWon))
```

```
##   $ Chg   ? Chg   0 Chg WON Chg 
##     177      95    1587      77
```

```r
W912UM$sumWon<-NA
W912UM$sumWon[W912UM$Action_Obligation_Then_Year==0]<-'0 Obl'
W912UM$sumWon[W912UM$UnmodifiedContractBaseAndAllOptionsValue_Then_Year+
                W912UM$ChangeOrderBaseAndAllOptionsValue>=W912UM$Action_Obligation_Then_Year*400&
                 W912UM$UnmodifiedContractBaseAndAllOptionsValue_Then_Year+
                W912UM$ChangeOrderBaseAndAllOptionsValue>10000]<-'WON Sum'
W912UM$sumWon[is.na(W912UM$sumWon) &
                    W912UM$UnmodifiedContractBaseAndAllOptionsValue_Then_Year+
                W912UM$ChangeOrderBaseAndAllOptionsValue>=W912UM$Action_Obligation_Then_Year*20&
                 W912UM$UnmodifiedContractBaseAndAllOptionsValue_Then_Year+
                W912UM$ChangeOrderBaseAndAllOptionsValue>10000]<-'? Sum'
W912UM$sumWon[is.na(W912UM$sumWon) &
                    (W912UM$UnmodifiedContractBaseAndAllOptionsValue_Then_Year+
                W912UM$ChangeOrderBaseAndAllOptionsValue<W912UM$Action_Obligation_Then_Year*20|
                 W912UM$UnmodifiedContractBaseAndAllOptionsValue_Then_Year+
                W912UM$ChangeOrderBaseAndAllOptionsValue>10000)]<-'$ Sum'
summary(factor(W912UM$sumWon))
```

```
##   $ Sum   ? Sum   0 Obl WON Sum 
##    1062      22      11     841
```

```r
(
ggplot(W912UM, aes(x=UnmodifiedContractBaseAndAllOptionsValue_Then_Year+ChangeOrderBaseAndAllOptionsValue,y=Action_Obligation_Then_Year,color=sumWon)) +#,color=qGrowth
  geom_point(alpha=0.25)+
  # theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_x_log10()+scale_y_log10()+
  #+
#   geom_vline(xintercept = c(1,10,100))+#+geom_vline(xintercept = 0.1)+
facet_grid(unmodWon~changeWon)#+, space="free_y"
#   labs(title="Distribution of Ceiling Breaches",
#        y="Percent of Growth in  Ceiling",
#        x="Unmodified Contract Ceiling")#,
#        # fill="Termination Completion"
)
```

```
## Warning in self$trans$transform(x): NaNs produced
```

```
## Warning: Transformation introduced infinite values in continuous x-axis
```

```
## Warning: Transformation introduced infinite values in continuous y-axis
```

```
## Warning: Removed 4 rows containing missing values (geom_point).
```

![](Ceiling_Breach_Examination_files/figure-html/W912UM-2.png)<!-- -->

```r
(
ggplot(W912UM, aes(x=UnmodifiedContractBaseAndAllOptionsValue_Then_Year+ChangeOrderBaseAndAllOptionsValue,y=Action_Obligation_Then_Year,color=sumWon)) +#,color=qGrowth
  geom_point(alpha=0.25)+
  # theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_x_log10()+scale_y_log10()+
  #+
#   geom_vline(xintercept = c(1,10,100))+#+geom_vline(xintercept = 0.1)+
facet_wrap(~Veh)#+, space="free_y"
#   labs(title="Distribution of Ceiling Breaches",
#        y="Percent of Growth in  Ceiling",
#        x="Unmodified Contract Ceiling")#,
#        # fill="Termination Completion"
)
```

```
## Warning in self$trans$transform(x): NaNs produced
```

```
## Warning: Transformation introduced infinite values in continuous x-axis
```

```
## Warning: Transformation introduced infinite values in continuous y-axis
```

```
## Warning: Removed 4 rows containing missing values (geom_point).
```

![](Ceiling_Breach_Examination_files/figure-html/W912UM-3.png)<!-- -->

```r
(
ggplot(W912UM, aes(x=UnmodifiedContractBaseAndAllOptionsValue_Then_Year+ChangeOrderBaseAndAllOptionsValue,y=Action_Obligation_Then_Year,color=sumWon)) +#,color=qGrowth
  geom_point(alpha=0.25)+
  # theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_x_log10()+scale_y_log10()+
  #+
#   geom_vline(xintercept = c(1,10,100))+#+geom_vline(xintercept = 0.1)+
facet_wrap(~Intl)#+, space="free_y"
#   labs(title="Distribution of Ceiling Breaches",
#        y="Percent of Growth in  Ceiling",
#        x="Unmodified Contract Ceiling")#,
#        # fill="Termination Completion"
)
```

```
## Warning in self$trans$transform(x): NaNs produced
```

```
## Warning: Transformation introduced infinite values in continuous x-axis
```

```
## Warning: Transformation introduced infinite values in continuous y-axis
```

```
## Warning: Removed 4 rows containing missing values (geom_point).
```

![](Ceiling_Breach_Examination_files/figure-html/W912UM-4.png)<!-- -->

```r
summary(W912UM$Intl)
```

```
## Just U.S. Any Intl. 
##       111      1825
```

```r
W912UM$unmodWon<-factor(W912UM$unmodWon)
summary(W912UM$Veh)
```

```
##  Def/Pur    S-IDC    M-IDC FSS/GWAC  BPA/BOA 
##      354     1290      271       10       11
```

```r
summary(W912UM$unmodWon)
```

```
##   $ Unm   ? Unm WON Unm 
##    1083      48     805
```

```r
summary(factor(W912UM$changeWon))
```

```
##   $ Chg   ? Chg   0 Chg WON Chg 
##     177      95    1587      77
```

```r
statsummary_discrete(c("unmodWon"), W912UM %>% filter(Intl=="Any Intl."&
                                                        !Veh %in% c("FSS/GWAC","BPA/BOA")),
                     value_col="Action_Obligation_Then_Year")
```

```
##   unmodWon %of records % of $s
## 1    $ Unm      52.80%  35.56%
## 2    ? Unm       2.66%   0.40%
## 3  WON Unm      44.55%  64.03%
```

All of the questionable contracts take place internationally and none use BPA/BOA or FSS/GWACs. That makes sense and raises confidence, but given that the clearly USD contract categories are less common, this doesn't help in resolving the ambiguous cases. That said, Single Award IDCs appear to have most of the ambigious cases, which suggests that this might be resolvable by looking at parent IDVs in those cases. 
### Transaction
#### Unmodified Transactions

```r
# W912UMtrans<-read.delim(file="..\\data\\semi_clean\\W912UM_complete.txt", sep="\t")



W912UMtrans<-inner_join(W912UMtrans,W912UM %>% group_by() %>%
                          dplyr::select(CSIScontractID,
                                        unmodWon,sumWon,changeWon,
                                        ChangeOrderBaseAndAllOptionsValue,
                                        UnmodifiedContractBaseAndAllOptionsValue_Then_Year,
                                        Action_Obligation_Then_Year,
                                        n_CBre),
                        by="CSIScontractID")

W912UMtrans$unmodWonT<-NA
W912UMtrans$unmodWonT[W912UMtrans$baseandalloptionsvalue>=W912UMtrans$obligatedamount*400&
                        W912UMtrans$baseandalloptionsvalue>10000&
                        W912UMtrans$baseandalloptionsvalue>=W912UMtrans$Action_Obligation_Then_Year*400&
                       (W912UMtrans$obligatedamount>0 | W912UMtrans$Action_Obligation_Then_Year>0)& 
                       W912UMtrans$modnumber=='0']<-'WON Unm'
W912UMtrans$unmodWonT[is.na(W912UMtrans$unmodWonT) &
                    W912UMtrans$baseandalloptionsvalue>=W912UMtrans$obligatedamount*20&
                      W912UMtrans$baseandalloptionsvalue>10000&
                        W912UMtrans$baseandalloptionsvalue>=W912UMtrans$Action_Obligation_Then_Year*20&
                      W912UMtrans$modnumber=='0']<-'? Unm'
W912UMtrans$unmodWonT[is.na(W912UMtrans$unmodWonT) &
                    (W912UMtrans$baseandalloptionsvalue<W912UMtrans$obligatedamount*20|
                        W912UMtrans$baseandalloptionsvalue<W912UMtrans$Action_Obligation_Then_Year*20|
                       W912UMtrans$baseandalloptionsvalue<=10000) &
                      W912UMtrans$modnumber=='0']<-'$ Unm'
W912UMtrans$unmodWonT[W912UMtrans$modnumber!='0']<-"Not Unmodified Transaction"




summary(factor(W912UMtrans$unmodWonT))
```

```
##                      $ Unm                      ? Unm 
##                       1099                         25 
## Not Unmodified Transaction                    WON Unm 
##                       3465                        812
```

```r
if(any(is.na(W912UMtrans$unmodWonT))) stop("Unclassified unmodWonT")
# View(W912UMtrans[is.na(),])
# write.csv(file="..\\Data\\semi_clean\\NA_unmodWonT.csv",W912UMtrans[is.na(W912UMtrans$unmodWonT),],row.names = FALSE)

#Examining disagreements
# View(W912UMtrans %>% filter(unmodWonT=='WON Unm'& unmodWon!='WON Unm') )
# View(W912UMtrans %>% filter(unmodWonT!='WON Unm'& unmodWon=='WON Unm') )
#Examining ?s
# View(W912UMtrans %>% filter(unmodWonT=='? Unm'& obligatedamount>0) )
# View(W912UMtrans %>% filter(unmodWonT=='? Unm'& obligatedamount==0) )
  

statsummary_discrete(c("unmodWon"),W912UMtrans %>% filter(modnumber=='0'),
                     value_col="Action_Obligation_Then_Year")
```

```
##   unmodWon %of records % of $s
## 1    $ Unm      55.94%  36.22%
## 2    ? Unm       2.48%   0.40%
## 3  WON Unm      41.58%  63.39%
```

```r
grouped_barplot(c("unmodWon"),W912UMtrans %>% filter(modnumber=='0'),
                     value_col="Action_Obligation_Then_Year")
```

![](Ceiling_Breach_Examination_files/figure-html/W912UM_transction_unmodified-1.png)<!-- -->

```r
UnmodDisagree<-W912UMtrans %>% filter(unmodWonT=='WON Unm'& unmodWon!='WON Unm') 
# View(W912UM %>% filter(CSIScontractID %in% UnmodDisagree$CSIScontractID))
CSIScontractID_ceil_to_na<-W912UMtrans$CSIScontractID[W912UMtrans$unmodWonT == 'WON Unm'&
                                                        !is.na(W912UMtrans$unmodWonT)]


#Spreading  the labeled values to modified entries, which helps in the next step.
W912UMtrans$unmodWonT[W912UMtrans$unmodWonT=="Not Unmodified Transaction" &
  W912UMtrans$CSIScontractID %in% CSIScontractID_ceil_to_na]<-'WON Unm'
# 
W912UMtrans$unmodWonT[W912UMtrans$unmodWonT=="Not Unmodified Transaction" &
  W912UMtrans$CSIScontractID %in% W912UMtrans$CSIScontractID[W912UMtrans$unmodWonT == '? Unm'&
                                                        !is.na(W912UMtrans$unmodWonT)]]<- '? Unm'
W912UMtrans$unmodWonT[W912UMtrans$unmodWonT=="Not Unmodified Transaction" &
  W912UMtrans$CSIScontractID %in% W912UMtrans$CSIScontractID[W912UMtrans$unmodWonT == '$ Unm'&
                                                        !is.na(W912UMtrans$unmodWonT)]]<- '$ Unm'

summary(factor(W912UMtrans$unmodWonT))
```

```
##   $ Unm   ? Unm WON Unm 
##    2325      71    3005
```


Contracts ceilings are marked null if:
* Contracting office W912UM 
* Initial transaction has a ceiling 400 times obligations.
* Initial ceiling 400 times total obligations.
* Initial or total obligations are positive.
Then it the ceiling is set to NA and it will not be in the sample.


#### Change Order Transactions

```r
W912UMtrans$chgWonT<-NA
W912UMtrans$chgWonT[W912UMtrans$modnumber=='0']<-"Unmodified Transaction"
W912UMtrans$chgWonT[is.na(W912UMtrans$chgWonT)&
                      (W912UMtrans$baseandalloptionsvalue==0 | 
                       W912UMtrans$n_CBre==0)
                    &W912UMtrans$modnumber!='0']<-'0 Chg Growth'
W912UMtrans$chgWonT[is.na(W912UMtrans$chgWonT)&
                      abs(W912UMtrans$baseandalloptionsvalue)>=abs(W912UMtrans$obligatedamount*400)&
                        (abs(W912UMtrans$ChangeOrderBaseAndAllOptionsValue)+
                           W912UMtrans$UnmodifiedContractBaseAndAllOptionsValue_Then_Year)>=
                       W912UMtrans$Action_Obligation_Then_Year*10&
                      abs(W912UMtrans$ChangeOrderBaseAndAllOptionsValue)>10000
                        # (W912UMtrans$baseandalloptionsvalue>=
                        #    W912UMtrans$UnmodifiedContractBaseAndAllOptionsValue_Then_Year*100 |
                       #   (!is.na(W912UMtrans$unmodWonT) & W912UMtrans$unmodWonT %in% c('WON Unm','? Unm')))&
                       # # (W912UMtrans$obligatedamount>0 | W912UMtrans$Action_Obligation_Then_Year>0)& 
                      # abs(W912UMtrans$baseandalloptionsvalue)>0 & W912UMtrans$ChangeOrderBaseAndAllOptionsValue>0
                    ]<-'WON Chg'
W912UMtrans$chgWonT[is.na(W912UMtrans$chgWonT) &
                        abs(W912UMtrans$baseandalloptionsvalue)>=abs(W912UMtrans$obligatedamount*20)&
                        (abs(W912UMtrans$ChangeOrderBaseAndAllOptionsValue)+
                           W912UMtrans$UnmodifiedContractBaseAndAllOptionsValue_Then_Year)>=
                       W912UMtrans$Action_Obligation_Then_Year*5&
                      abs(W912UMtrans$ChangeOrderBaseAndAllOptionsValue)
                        # (W912UMtrans$baseandalloptionsvalue>=
                        #    W912UMtrans$UnmodifiedContractBaseAndAllOptionsValue_Then_Year*10 |
                        #  (!is.na(W912UMtrans$unmodWonT) & W912UMtrans$unmodWonT %in% c('WON Unm','? Unm')))&
                      # abs(W912UMtrans$baseandalloptionsvalue)>0 & W912UMtrans$ChangeOrderBaseAndAllOptionsValue>0
                      ]<-'? Chg'
W912UMtrans$chgWonT[is.na(W912UMtrans$chgWonT) &
                        (abs(W912UMtrans$baseandalloptionsvalue)<abs(W912UMtrans$obligatedamount*20)|
                        (abs(W912UMtrans$ChangeOrderBaseAndAllOptionsValue)+
                           W912UMtrans$UnmodifiedContractBaseAndAllOptionsValue_Then_Year)<
                       W912UMtrans$Action_Obligation_Then_Year*5 |
                         abs(W912UMtrans$ChangeOrderBaseAndAllOptionsValue)<=10000)
                        # (W912UMtrans$baseandalloptionsvalue<
                        #    W912UMtrans$UnmodifiedContractBaseAndAllOptionsValue_Then_Year*10 |
                        #  (!is.na(W912UMtrans$unmodWonT) & W912UMtrans$unmodWonT %in% c('WON Unm','? Unm')))&
                      # abs(W912UMtrans$baseandalloptionsvalue)>0 & W912UMtrans$ChangeOrderBaseAndAllOptionsValue>0
                      ]<-'$ Chg'

summary(factor(W912UMtrans$chgWonT))
```

```
##                  $ Chg                  ? Chg           0 Chg Growth 
##                    265                      2                   2142 
## Unmodified Transaction                WON Chg 
##                   1936                   1056
```

```r
if(any(is.na(W912UMtrans$chgWonT))) stop("Unclassified chgWonT")
# View(W912UMtrans[is.na(is.na(W912UMtrans$chgWonT)),])
write.csv(file="..\\Data\\semi_clean\\chgWonT.csv",W912UMtrans %>% filter(chgWonT=="WON Chg"),row.names = FALSE)
```


#### Examining International Related Vars

```r
levels(W912UMtrans$vendorcountrycode)<-list(
  # ""="",
  "ABW: ARUBA"="ABW: ARUBA",
  "JPN: JAPAN"="JPN: JAPAN",
  "KOR: KOREA, REPUBLIC OF"=c("KOR: KOREA, REPUBLIC OF","KOR","SOUTH KOREA"),
  "USA: UNITED STATES OF AMERICA"=c("USA: UNITED STATES OF AMERICA","UNITED STATES","USA")
)

summary(W912UMtrans$chgWonT)
```

```
##    Length     Class      Mode 
##      5401 character character
```

```r
ggplot(W912UMtrans %>% filter(modnumber=='0') ,aes(x=unmodWonT))+geom_bar()+facet_wrap(~vendorcountrycode)
```

![](Ceiling_Breach_Examination_files/figure-html/tran_related_vars-1.png)<!-- -->

```r
ggplot(W912UMtrans %>% filter(modnumber!='0'),aes(x=chgWonT))+geom_bar()+facet_wrap(~vendorcountrycode)
```

![](Ceiling_Breach_Examination_files/figure-html/tran_related_vars-2.png)<!-- -->

```r
ggplot(W912UMtrans %>% filter(modnumber!='0'),aes(x=chgWonT))+geom_bar()+facet_wrap(~unmodWonT)
```

![](Ceiling_Breach_Examination_files/figure-html/tran_related_vars-3.png)<!-- -->

```r
ggplot(W912UMtrans,aes(x=sumWon))+geom_bar()+facet_wrap(~vendorcountrycode)
```

![](Ceiling_Breach_Examination_files/figure-html/tran_related_vars-4.png)<!-- -->

```r
W912UMtrans$placeofperformancecountrycode
```

```
##    [1] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
##   [18] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
##   [35] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
##   [52] KOR KOR KOR KOR USA KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
##   [69] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
##   [86] KOR KOR KOR KOR KOR KOR USA KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
##  [103] KOR KOR KOR USA KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR USA
##  [120] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
##  [137] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
##  [154] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
##  [171] KOR KOR USA KOR KOR USA KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
##  [188] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
##  [205] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
##  [222] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR USA KOR
##  [239] KOR KOR KOR KOR USA KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
##  [256] USA KOR KOR KOR KOR KOR KOR KOR LSO KOR KOR KOR KOR KOR KOR KOR KOR
##  [273] KOR KOR KOR KOR KOR KOR KOR LSO KOR KOR KOR KOR KOR KOR KOR KOR KOR
##  [290] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
##  [307] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
##  [324] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
##  [341] USA KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
##  [358] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
##  [375] KOR KOR KOR USA KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
##  [392] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
##  [409] KOR KOR USA USA KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
##  [426] KOR KOR KOR KOR KOR USA KOR KOR KOR KOR KOR KOR KOR KOR USA KOR KOR
##  [443] KOR KOR KOR KOR KOR USA KOR KOR KOR KOR USA KOR USA KOR KOR KOR KOR
##  [460] KOR USA KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
##  [477] KOR KOR KOR KOR KOR USA KOR KOR KOR KOR KOR USA KOR KOR KOR USA KOR
##  [494] KOR KOR KOR KOR KOR KOR KOR KOR KOR USA USA KOR KOR KOR KOR KOR KOR
##  [511] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
##  [528] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
##  [545] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
##  [562] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
##  [579] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
##  [596] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
##  [613] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
##  [630] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
##  [647] KOR KOR KOR KOR KOR LSO KOR KOR KOR KOR KOR KOR KOR USA KOR KOR KOR
##  [664] KOR KOR USA KOR KOR KOR KOR KOR KOR KOR KOR KOR USA KOR KOR KOR KOR
##  [681] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR USA KOR KOR KOR KOR KOR
##  [698] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
##  [715] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
##  [732] KOR MNG KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
##  [749] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
##  [766] KOR USA KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
##  [783] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
##  [800] KOR KOR KOR KOR USA KOR KOR KOR USA KOR KOR KOR KOR KOR KOR KOR KOR
##  [817] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
##  [834] KOR KOR KOR KOR KOR KOR USA KOR KOR KOR KOR USA KOR KOR KOR KOR KOR
##  [851] KOR KOR KOR KOR KOR USA KOR KOR KOR LSO KOR KOR KOR KOR KOR KOR KOR
##  [868] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
##  [885] KOR KOR KOR KOR KOR USA KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
##  [902] USA KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
##  [919] KOR KOR USA KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR USA KOR KOR KOR
##  [936] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
##  [953] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR USA KOR
##  [970] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
##  [987] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [1004] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR LSO KOR KOR KOR
## [1021] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [1038] KOR KOR KOR KOR KOR KOR USA KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [1055] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [1072] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR USA KOR USA KOR KOR KOR KOR
## [1089] KOR KOR USA KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [1106] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR USA
## [1123] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR USA KOR KOR KOR KOR KOR KOR
## [1140] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [1157] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [1174] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [1191] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [1208] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [1225] KOR KOR KOR KOR KOR KOR KOR KOR USA KOR KOR KOR KOR KOR KOR KOR KOR
## [1242] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [1259] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [1276] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR USA KOR KOR KOR KOR KOR
## [1293] KOR KOR KOR KOR KOR KOR KOR KOR USA KOR KOR KOR KOR KOR KOR KOR KOR
## [1310] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [1327] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR USA KOR KOR KOR KOR KOR
## [1344] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [1361] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [1378] KOR KOR USA KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [1395] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [1412] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR USA KOR KOR KOR KOR
## [1429] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [1446] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [1463] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR USA KOR KOR KOR KOR KOR KOR
## [1480] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [1497] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [1514] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR USA KOR KOR KOR
## [1531] KOR KOR KOR KOR KOR KOR KOR KOR USA KOR KOR KOR KOR KOR KOR KOR KOR
## [1548] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [1565] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [1582] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [1599] KOR KOR KOR KOR KOR KOR USA KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [1616] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR USA
## [1633] KOR KOR KOR KOR USA KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [1650] KOR KOR KOR KOR KOR KOR KOR KOR KOR USA KOR KOR KOR KOR KOR KOR KOR
## [1667] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [1684] KOR USA KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [1701] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [1718] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [1735] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [1752] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [1769] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [1786] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [1803] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR USA KOR KOR KOR KOR KOR KOR
## [1820] KOR KOR KOR USA KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [1837] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [1854] KOR KOR USA KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [1871] KOR KOR KOR KOR KOR USA KOR KOR KOR KOR KOR KOR USA KOR USA KOR KOR
## [1888] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR USA USA KOR KOR KOR
## [1905] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [1922] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [1939] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [1956] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [1973] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [1990] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [2007] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [2024] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [2041] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [2058] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [2075] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [2092] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [2109] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [2126] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [2143] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [2160] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [2177] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [2194] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [2211] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [2228] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [2245] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR USA
## [2262] USA USA USA USA KOR USA USA USA USA USA USA KOR KOR KOR KOR KOR KOR
## [2279] USA USA KOR USA USA KOR KOR KOR USA KOR KOR KOR KOR KOR KOR KOR KOR
## [2296] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [2313] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [2330] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [2347] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [2364] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [2381] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [2398] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [2415] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [2432] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [2449] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [2466] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [2483] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [2500] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [2517] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [2534] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [2551] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [2568] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [2585] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [2602] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [2619] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [2636] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [2653] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [2670] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [2687] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [2704] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [2721] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR USA KOR KOR
## [2738] KOR KOR KOR KOR KOR KOR KOR USA KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [2755] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [2772] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [2789] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR USA KOR
## [2806] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [2823] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR USA KOR
## [2840] KOR KOR KOR KOR KOR KOR KOR KOR KOR USA KOR KOR KOR KOR KOR KOR KOR
## [2857] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [2874] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR USA KOR KOR KOR KOR KOR
## [2891] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [2908] KOR KOR KOR USA KOR KOR KOR USA KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [2925] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR MNG KOR KOR KOR KOR KOR KOR
## [2942] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [2959] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [2976] KOR KOR KOR KOR KOR USA KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [2993] KOR KOR KOR KOR USA KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [3010] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [3027] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [3044] KOR KOR KOR KOR KOR KOR KOR KOR USA KOR KOR USA KOR KOR KOR KOR KOR
## [3061] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [3078] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [3095] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [3112] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [3129] KOR KOR KOR KOR KOR KOR KOR USA KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [3146] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [3163] USA KOR KOR KOR KOR USA USA KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [3180] KOR USA KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [3197] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [3214] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [3231] KOR KOR KOR KOR KOR KOR USA KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [3248] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR USA KOR KOR KOR KOR KOR KOR
## [3265] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [3282] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [3299] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [3316] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [3333] KOR KOR KOR KOR KOR KOR KOR USA KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [3350] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [3367] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [3384] KOR KOR KOR KOR KOR KOR KOR KOR USA KOR KOR KOR USA KOR USA KOR KOR
## [3401] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [3418] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [3435] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [3452] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [3469] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR USA KOR KOR KOR KOR
## [3486] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [3503] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [3520] KOR KOR KOR KOR KOR KOR KOR KOR USA KOR KOR KOR KOR KOR KOR KOR KOR
## [3537] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [3554] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [3571] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [3588] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR USA KOR KOR KOR KOR KOR KOR
## [3605] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [3622] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [3639] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR USA KOR KOR KOR KOR KOR KOR
## [3656] USA KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [3673] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [3690] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [3707] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [3724] KOR KOR KOR KOR KOR KOR KOR USA KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [3741] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [3758] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [3775] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR USA KOR KOR KOR KOR KOR
## [3792] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [3809] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR USA
## [3826] KOR KOR KOR KOR KOR KOR KOR KOR USA KOR KOR KOR KOR KOR KOR KOR KOR
## [3843] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [3860] KOR KOR KOR KOR KOR KOR KOR KOR USA KOR KOR KOR KOR KOR KOR KOR KOR
## [3877] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [3894] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [3911] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [3928] KOR KOR KOR USA KOR KOR USA KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [3945] USA KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [3962] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [3979] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [3996] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [4013] USA KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR USA KOR KOR KOR KOR KOR
## [4030] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR USA KOR
## [4047] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [4064] KOR KOR KOR KOR KOR KOR KOR KOR USA KOR KOR KOR KOR KOR KOR KOR KOR
## [4081] KOR KOR KOR KOR KOR KOR USA KOR KOR KOR KOR KOR KOR KOR KOR KOR USA
## [4098] KOR USA KOR KOR KOR USA KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [4115] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [4132] KOR USA KOR KOR KOR USA KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [4149] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [4166] KOR KOR KOR KOR KOR KOR USA KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [4183] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [4200] KOR KOR KOR KOR KOR KOR KOR KOR KOR USA KOR USA KOR KOR KOR KOR KOR
## [4217] KOR KOR KOR KOR USA KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [4234] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [4251] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [4268] KOR USA KOR KOR USA KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [4285] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [4302] KOR KOR KOR KOR USA KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [4319] KOR USA KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR USA
## [4336] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [4353] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [4370] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR USA
## [4387] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [4404] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR USA KOR KOR KOR
## [4421] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [4438] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [4455] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR USA KOR KOR KOR
## [4472] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [4489] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [4506] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR USA
## [4523] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [4540] KOR KOR KOR KOR KOR KOR KOR KOR USA KOR KOR KOR KOR KOR KOR KOR KOR
## [4557] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [4574] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR USA KOR KOR KOR KOR KOR
## [4591] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR USA
## [4608] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [4625] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [4642] KOR KOR KOR KOR KOR USA KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [4659] KOR KOR USA KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [4676] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [4693] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [4710] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [4727] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [4744] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [4761] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [4778] KOR KOR USA KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [4795] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR USA KOR KOR KOR KOR
## [4812] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [4829] USA KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [4846] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [4863] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [4880] KOR KOR USA KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [4897] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [4914] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [4931] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [4948] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [4965] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR     KOR KOR KOR KOR
## [4982] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [4999] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR USA KOR KOR KOR KOR KOR KOR
## [5016] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR USA KOR KOR KOR
## [5033] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [5050] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR USA KOR KOR KOR KOR KOR KOR
## [5067] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [5084] KOR KOR USA USA KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [5101] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [5118] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR USA KOR KOR KOR KOR
## [5135] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [5152] KOR KOR KOR KOR KOR KOR KOR USA KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [5169] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [5186] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR USA KOR KOR KOR KOR KOR
## [5203] KOR USA KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [5220] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [5237] USA KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [5254] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [5271] KOR KOR KOR KOR KOR KOR KOR USA KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [5288] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [5305] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [5322] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [5339] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [5356] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [5373] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## [5390] KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR KOR
## Levels:  KOR LSO MKD MNG USA VNM
```

```r
ggplot(W912UMtrans %>% filter(modnumber=='0'),aes(x=unmodWon))+
  geom_bar()+facet_wrap(~placeofmanufacture)
```

![](Ceiling_Breach_Examination_files/figure-html/tran_related_vars-5.png)<!-- -->

```r
ggplot(W912UMtrans %>% filter(modnumber=='0'),aes(x=unmodWon))+
  geom_bar()+facet_wrap(~countryoforigin)
```

![](Ceiling_Breach_Examination_files/figure-html/tran_related_vars-6.png)<!-- -->

```r
ggplot(W912UMtrans %>% filter(modnumber=='0'),aes(x=unmodWon))+
  geom_bar()+facet_wrap(~placeofperformancecountrycode)
```

![](Ceiling_Breach_Examination_files/figure-html/tran_related_vars-7.png)<!-- -->

```r
# View(W912UMtrans %>% filter(unmodWon %in% c("? Unm","Won Unm") & vendorcountrycode=="USA: UNITED STATES OF AMERICA" & modnumber=='0'))

# Miscategorized<-W912UMtrans %>% filter(unmodWon %in% c("? Unm","WON Unm") & (
#   vendorcountrycode=="USA: UNITED STATES OF AMERICA" |
#     countryoforigin=="USA" |
#     placeofperformancecountrycode=="USA")
#   & modnumber=='0')

summary(factor(W912UMtrans$chgWonT))
```

```
##                  $ Chg                  ? Chg           0 Chg Growth 
##                    265                      2                   2142 
## Unmodified Transaction                WON Chg 
##                   1936                   1056
```

```r
Miscategorized<-W912UMtrans %>% filter((unmodWon %in% c("? Unm","WON Unm")|
                                          unmodWonT %in% c("? Unm","WON Unm")|
                                          changeWon %in% c("? Chg","WON Chg")|
                                          chgWonT %in% c("? Chg","WON Chg")
                                        )& (
  
    placeofperformancecountrycode=="USA")
  & modnumber=='0')

if(nrow(Miscategorized)>0) stop("False positive contract in Won performed in US")
# View(W912UM %>% filter(CSIScontractID %in% Miscategorized$CSIScontractID))
```

# Cleaning



```r
load(file="../Data/Clean/transformed_def.Rdata")

colnames(def)[colnames(def)=="l_CBre"]<-"lp_CBre"
colnames(def)[colnames(def)=="n_CBre"]<-"p_CBre"
colnames(def)[colnames(def)=="ChangeOrderCeilingGrowth"]<-"n_CBre"

summary(def$n_CBre)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## 0.000e+00 0.000e+00 0.000e+00 6.080e+04 0.000e+00 3.447e+11
```

```r
summary(def$p_CBre)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##       1       1       1     Inf       1     Inf
```

```r
summary(factor(W912UMtrans$chgWonT))
```

```
##                  $ Chg                  ? Chg           0 Chg Growth 
##                    265                      2                   2142 
## Unmodified Transaction                WON Chg 
##                   1936                   1056
```

```r
summary(factor(W912UMtrans$changeWon))
```

```
##   $ Chg   ? Chg   0 Chg WON Chg 
##    1006     724    3208     463
```

```r
summary(factor(W912UM$changeWon))
```

```
##   $ Chg   ? Chg   0 Chg WON Chg 
##     177      95    1587      77
```

```r
def$UnmodifiedContractBaseAndAllOptionsValue_Then_Year[
  def$CSIScontractID %in% c(W912UM$CSIScontractID[W912UM$unmodWon=="WON Unm"],
                                     W912UMtrans$CSIScontractID[W912UMtrans$unmodWonT=="WON Unm"])]<-NA
def$UnmodifiedContractBaseAndAllOptionsValue_Then_Year[def$UnmodifiedContractBaseAndAllOptionsValue_Then_Year==0]<-NA

def$Ceil[
  def$CSIScontractID %in% c(W912UM$CSIScontractID[W912UM$unmodWon=="WON Unm"],
                                     W912UMtrans$CSIScontractID[W912UMtrans$unmodWonT=="WON Unm"])]<-NA
def$Ceil[def$UnmodifiedContractBaseAndAllOptionsValue_Then_Year==0]<-NA

def$cl_Ceil[
  def$CSIScontractID %in% c(W912UM$CSIScontractID[W912UM$unmodWon=="WON Unm"],
                                     W912UMtrans$CSIScontractID[W912UMtrans$unmodWonT=="WON Unm"])]<-NA
def$cl_Ceil[def$UnmodifiedContractBaseAndAllOptionsValue_Then_Year==0]<-NA


summary(def$UnmodifiedContractBaseAndAllOptionsValue_Then_Year)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
## 0.000e+00 1.706e+03 6.680e+03 4.447e+06 2.389e+04 7.255e+10      8410
```

```r
def$ChangeOrderBaseAndAllOptionsValue[def$CSIScontractID %in% c(W912UM$CSIScontractID[W912UM$changeWon=="WON Chg"],
                                                       W912UMtrans$CSIScontractID[W912UMtrans$chgWonT=="WON Chg"]
                                                         )]<-NA
def$n_CBre[def$CSIScontractID %in% c(W912UM$CSIScontractID[W912UM$changeWon=="WON Chg"],
                                                       W912UMtrans$CSIScontractID[W912UMtrans$chgWonT=="WON Chg"]
                                                         )]<-NA
def$p_CBre[def$CSIScontractID %in% c(W912UM$CSIScontractID[W912UM$changeWon=="WON Chg"],
                                                       W912UMtrans$CSIScontractID[W912UMtrans$chgWonT=="WON Chg"]
                                                         )]<-NA
def$l_Cbre[def$CSIScontractID %in% c(W912UM$CSIScontractID[W912UM$changeWon=="WON Chg"],
                                                       W912UMtrans$CSIScontractID[W912UMtrans$chgWonT=="WON Chg"]
                                                         )]<-NA
```

```
## Warning: Unknown or uninitialised column: 'l_Cbre'.
```

```r
summary(def$ChangeOrderBaseAndAllOptionsValue)
```

```
##       Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
## -3.002e+09  0.000e+00  0.000e+00  5.481e+04  0.000e+00  3.447e+11 
##       NA's 
##        209
```

```r
summary(def$Ceil)
```

```
##    [0,15k) [15k,100k)  [100k,1m)   [1m,10m)  [10m,75m)     [75m+] 
##    6139861    2127538     713044     134183      18195       2448 
##       NA's 
##        817
```

```r
summary(def$cl_Ceil)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##  -2.641  -0.235   0.038   0.000   0.292   3.274    8410
```

```r
#The original variables for b_Term and b_CBre are Term and CBre
grouped_barplot("CBre", def,value_col="Action_Obligation_Then_Year")
```

![](Ceiling_Breach_Examination_files/figure-html/clean_setup-1.png)<!-- -->

```r
statsummary_discrete(c("CBre"), def,value_col="Action_Obligation_Then_Year")
```

```
##             CBre %of records % of $s
## 1           None      98.97%  82.52%
## 2 Ceiling Breach       1.03%  17.48%
```

```r
cbre<-def %>% filter(b_CBre==1)
cbre$qGrowth<-Hmisc::cut2(cbre$p_CBre-1,c(1,10))
summary(cbre$qGrowth)
```

```
## [4.69e-10,1.00e+00) [1.00e+00,1.00e+01) [1.00e+01,     Inf] 
##               81024               11819                1451 
##                NA's 
##                 196
```

## Examining Outliers


```r
cbre$Why_Outlier<-NA
cbre$Why_Outlier[is.na(cbre$UnmodifiedContractBaseAndAllOptionsValue_Then_Year)]<-"No Unmodified Ceiling"
cbre$Why_Outlier[is.na(cbre$Why_Outlier)&
                   cbre$Action_Obligation_Then_Year*2>=cbre$UnmodifiedContractBaseAndAllOptionsValue_Then_Year+
                   cbre$n_CBre]<-
  "Obligations at least half Orig+CRai"
cbre$Why_Outlier[is.na(cbre$Why_Outlier)&
                   cbre$Office=="W912UM"]<-
  "Korean Office W912UM"
cbre$Why_Outlier[is.na(cbre$Why_Outlier)&
                   cbre$n_CBre>=2.5e8]<-
  ">=$250M, Insepect"
cbre$Why_Outlier[is.na(cbre$Why_Outlier)&
                   cbre$p_CBre-1>10]<-
  "Other Unexplained 10x Ceiling Breach"
cbre$Why_Outlier<-factor(cbre$Why_Outlier,
                         levels=c(
                           "No Unmodified Ceiling",
                           "Obligations at least half Orig+CRai",
                           "Later Deobligated",
                           "Korean Office W912UM",
                           ">=$250M, Insepect",
                           "Other Unexplained 10x Ceiling Breach"
                         ))

#Percent Growth
summary(Hmisc::cut2(cbre_preclean$p_CBre-1,c(1,
                                          10,
                                          100
                                          )))
```

```
## [4.69e-10,1.00e+00) [1.00e+00,1.00e+01) [1.00e+01,1.00e+02) 
##               81198               11830                1043 
## [1.00e+02,     Inf] 
##                 419
```

```r
summary(Hmisc::cut2(cbre$p_CBre-1,c(1,
                                          10,
                                          100
                                          )))
```

```
## [4.69e-10,1.00e+00) [1.00e+00,1.00e+01) [1.00e+01,1.00e+02) 
##               81024               11819                1040 
## [1.00e+02,     Inf]                NA's 
##                 411                 196
```

```r
summary(cbre$Ceil[(cbre$p_CBre-1)>10])
```

```
##    [0,15k) [15k,100k)  [100k,1m)   [1m,10m)  [10m,75m)     [75m+] 
##       1088        213        116         30          4          0 
##       NA's 
##        196
```

```r
summary(cbre$Ceil[(cbre$p_CBre-1)>100])
```

```
##    [0,15k) [15k,100k)  [100k,1m)   [1m,10m)  [10m,75m)     [75m+] 
##        367         32         11          1          0          0 
##       NA's 
##        196
```

```r
p_outlier_summary<-cbre %>% filter(p_CBre-1>10) %>% group_by(Why_Outlier) %>%
  dplyr::summarise(nContract=length(n_CBre),
    SumOfChangeOrderCeilingGrowth=sum(n_CBre),
                   MaxOfChangeOrderCeilingGrowth=max(n_CBre),
                   SumOfAction_Obligation_Then_Year=sum(Action_Obligation_Then_Year))

#Absolute Growth
summary(Hmisc::cut2(cbre_preclean$n_CBre,c(1e3,
                                          1e6,
                                          1e7,
                                          1e8,
                                          2.5e8,
                                          1e9,
                                          1e10,
                                          2e10
                                          ))
)
```

```
## [1.00e-02,1.00e+03) [1.00e+03,1.00e+06) [1.00e+06,1.00e+07) 
##               18238               72053                3517 
## [1.00e+07,1.00e+08) [1.00e+08,2.50e+08) [2.50e+08,1.00e+09) 
##                 571                  67                  32 
## [1.00e+09,1.00e+10) [1.00e+10,2.00e+10) [2.00e+10,3.45e+11] 
##                   9                   1                   2
```

```r
summary(Hmisc::cut2(cbre$n_CBre,c(1e3,
                                          1e6,
                                          1e7,
                                          1e8,
                                          2.5e8,
                                          1e9,
                                          1e10,
                                          2e10
                                          ))
)
```

```
## [1.00e-02,1.00e+03) [1.00e+03,1.00e+06) [1.00e+06,1.00e+07) 
##               18238               72051                3486 
## [1.00e+07,1.00e+08) [1.00e+08,2.50e+08) [2.50e+08,1.00e+09) 
##                 473                  27                  15 
## [1.00e+09,1.00e+10) [1.00e+10,2.00e+10) [2.00e+10,3.45e+11] 
##                   1                   1                   2 
##                NA's 
##                 196
```

```r
summary(cbre$Ceil[cbre$n_CBre>=1e6])
```

```
##    [0,15k) [15k,100k)  [100k,1m)   [1m,10m)  [10m,75m)     [75m+] 
##         56         97        484       1766       1353        248 
##       NA's 
##        197
```

```r
summary(cbre$Ceil[cbre$n_CBre>=1e9])
```

```
##    [0,15k) [15k,100k)  [100k,1m)   [1m,10m)  [10m,75m)     [75m+] 
##          0          1          1          1          0          1 
##       NA's 
##        196
```

```r
n_outlier_summary<-cbre %>% filter(n_CBre>2.5e8) %>% group_by(Why_Outlier) %>%
  dplyr::summarise(nContract=length(n_CBre),
    SumOfChangeOrderCeilingGrowth=sum(n_CBre),
                   MaxOfChangeOrderCeilingGrowth=max(n_CBre),
                   SumOfAction_Obligation_Then_Year=sum(Action_Obligation_Then_Year))


# write.csv(file="..\\Data\\semi_clean\\p_CBre_post_clean_outliers.csv",cbre %>% filter((p_CBre-1)>10),row.names = FALSE)
write.csv(file="..\\Data\\semi_clean\\n_CBre_post_clean_outliers.csv",cbre %>% filter(n_CBre>=2.5e8),row.names = FALSE)
```

The cleaning has cut in half the outliers with growth >=100,000,000, although the influence has been much smaller on the percentage growth outliers, reinforcing the choice to switch to absolute growth. The study team chose to set a threshold of Shifting the focus to all contracts with growth of at least 250 million, there are far fewer contracts that account for far more money.


Why_Outlier                            nContract   SumOfChangeOrderCeilingGrowth   MaxOfChangeOrderCeilingGrowth   SumOfAction_Obligation_Then_Year
------------------------------------  ----------  ------------------------------  ------------------------------  ---------------------------------
Obligations at least half Orig+CRai           11                      5525269281                       992698908                        16999937992
>=$250M, Insepect                              8                    479197384535                    344739578535                         1611779996



After the cleaning, 2 categories remain relevant.



Why_Outlier                             nContract   SumOfChangeOrderCeilingGrowth   MaxOfChangeOrderCeilingGrowth   SumOfAction_Obligation_Then_Year
-------------------------------------  ----------  ------------------------------  ------------------------------  ---------------------------------
No Unmodified Ceiling                         232                        81591718                        20862815                          383117339
Obligations at least half Orig+CRai          1146                      4418306936                       769789464                         8799240742
>=$250M, Insepect                               5                    476290148280                    344739578535                           29052394
Other Unexplained 10x Ceiling Breach           68                       484851277                        95979870                           86701052


* Obligations at least half Orig+CRai: For this category, total obligations of the contract were at least half the value of the initial ceiling plus ceiling growth under change orders. As before, this category accounts for the overwhelming majority of outlier spending but only a tiny fraction of change order growth.
* There are 8 contracts with ceiling growth of over $250 million that account for hundreds of billions in change order growth. These merit manual inspection.

## Graphs after Cleaning


```r
(
ggplot(cbre, aes(x=UnmodifiedContractBaseAndAllOptionsValue_Then_Year,y=p_CBre-1)) +#,color=qGrowth
  geom_point(alpha=0.25,shape=".")+
  # theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_x_log10()+scale_y_log10()+
  #+
  geom_vline(xintercept = c(1,10,100))+#+geom_vline(xintercept = 0.1)+
# facet_wrap(~Ceil,scales="free_y")+#+, space="free_y"
  labs(title="Distribution of Ceiling Breaches",
       y="Percent of Growth in  Ceiling",
       x="Unmodified Contract Ceiling")#,
       # fill="Termination Completion"
)
```

```
## Warning: Removed 429 rows containing missing values (geom_point).
```

![](Ceiling_Breach_Examination_files/figure-html/CeilingGrowthAfterCleaning-1.png)<!-- -->

```r
(
ggplot(cbre, aes(x=UnmodifiedContractBaseAndAllOptionsValue_Then_Year,y=n_CBre)) +#,color=qGrowth
  geom_point(alpha=0.25,shape=".")+
  # theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_x_log10()+scale_y_log10()+
  #+
  geom_vline(xintercept = c(1,10,100))+#+geom_vline(xintercept = 0.1)+
# facet_wrap(~Ceil,scales="free_y")+#+, space="free_y"
  labs(title="Distribution of Ceiling Breaches",
       y="Percent of Growth in  Ceiling",
       x="Unmodified Contract Ceiling")#,
       # fill="Termination Completion"
)
```

```
## Warning: Removed 429 rows containing missing values (geom_point).
```

![](Ceiling_Breach_Examination_files/figure-html/CeilingGrowthAfterCleaning-2.png)<!-- -->

```r
(
ggplot(cbre, aes(x=UnmodifiedContractBaseAndAllOptionsValue_Then_Year+ChangeOrderBaseAndAllOptionsValue,y=Action_Obligation_Then_Year)) +#,color=qGrowth
  geom_point(alpha=0.25,shape=".")+
  # theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_x_log10()+scale_y_log10()#+
  #+
#   geom_vline(xintercept = c(1,10,100))+#+geom_vline(xintercept = 0.1)+
# # facet_wrap(~Ceil,scales="free_y")+#+, space="free_y"
#   labs(title="Distribution of Ceiling Breaches",
#        y="Percent of Growth in  Ceiling",
#        x="Unmodified Contract Ceiling")#,
#        # fill="Termination Completion"
)
```

```
## Warning in self$trans$transform(x): NaNs produced
```

```
## Warning: Transformation introduced infinite values in continuous y-axis
```

```
## Warning: Removed 452 rows containing missing values (geom_point).
```

![](Ceiling_Breach_Examination_files/figure-html/CeilingGrowthAfterCleaning-3.png)<!-- -->

```r
summary(cbre$ChangeOrderBaseAndAllOptionsValue)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
## 0.000e+00 1.570e+03 9.089e+03 5.448e+06 5.194e+04 3.447e+11       196
```

```r
(
ggplot(cbre, aes(x=n_CBre,y=ChangeOrderBaseAndAllOptionsValue)) +#,color=qGrowth
  geom_point(alpha=0.25,shape=".")+
  # theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_x_log10()+scale_y_log10()#+
  #+
#   geom_vline(xintercept = c(1,10,100))+#+geom_vline(xintercept = 0.1)+
# # facet_wrap(~Ceil,scales="free_y")+#+, space="free_y"
#   labs(title="Distribution of Ceiling Breaches",
#        y="Percent of Growth in  Ceiling",
#        x="Unmodified Contract Ceiling")#,
#        # fill="Termination Completion"
)
```

```
## Warning: Removed 196 rows containing missing values (geom_point).
```

![](Ceiling_Breach_Examination_files/figure-html/CeilingGrowthAfterCleaning-4.png)<!-- -->

```r
(
ggplot(cbre, aes(x=p_CBre-1,fill=qGrowth)) +
  geom_histogram(bins=100)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_x_log10()+
  #+
  geom_vline(xintercept = c(1,10,100))+#+geom_vline(xintercept = 0.1)+
facet_wrap(~Ceil,scales="free_y")+#+, space="free_y"
  labs(title="Distribution of Ceiling Breaches",
       y="Contract Count",
       x="Percent of Growth in  Ceiling")#,
       # fill="Termination Completion"
)
```

```
## Warning: Removed 428 rows containing non-finite values (stat_bin).
```

![](Ceiling_Breach_Examination_files/figure-html/CeilingGrowthAfterCleaning-5.png)<!-- -->

```r
(
ggplot(cbre, aes(x=n_CBre,fill=qGrowth)) +
  geom_histogram(bins=100)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_x_log10()+
  #+
  geom_vline(xintercept = 1)#+geom_vline(xintercept = 0.1)+
#facet_grid(NoPreTermObl~.,scales="free_y", space="free_y")+
  # labs(title="Distribution of Contracts with Obligations After Last termination",
  #      y="Contract Count",
  #      x="Percent of Obligations After Day of Termination",
  #      fill="Termination Completion"
)
```

```
## Warning: Removed 196 rows containing non-finite values (stat_bin).
```

![](Ceiling_Breach_Examination_files/figure-html/CeilingGrowthAfterCleaning-6.png)<!-- -->

```r
(
ggplot(cbre, aes(x=n_CBre,fill=qGrowth)) +
  geom_histogram(bins=100)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_x_log10()+
  #+
  geom_vline(xintercept = 1)+
facet_wrap(~Ceil,scales="free_y")#+, space="free_y"
#+geom_vline(xintercept = 0.1)+
#facet_grid(NoPreTermObl~.,scales="free_y", space="free_y")+
  # labs(title="Distribution of Contracts with Obligations After Last termination",
  #      y="Contract Count",
  #      x="Percent of Obligations After Day of Termination",
  #      fill="Termination Completion"
)
```

```
## Warning: Removed 196 rows containing non-finite values (stat_bin).
```

![](Ceiling_Breach_Examination_files/figure-html/CeilingGrowthAfterCleaning-7.png)<!-- -->
