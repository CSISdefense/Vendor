# Contract Termination
Greg Sanders  
Wednesday, February 8, 2017  

Modeling Likelihood of Contract Termination
============================================================================

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
## Warning: package 'arm' was built under R version 3.4.3
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


###Data Transformations
After the data is loaded, we perform a series of transformations, creating derived variables that fit our purposes.

* l_Ceil is the natural log of the initial contract cost ceiling (UnmodifiedContractBaseAndAllOptionsValue).
* l_Days is the natural log of the initial maximum contract duration in days (UnmodifiedDays)
* Changing the unlabeled option for International to NA as it should not be seperately computed.
* b_Term transforms the factor that reports whether a contract was terminated (Term) to a numerical variable, 0 for no terminations, 1 for any partial or complete terminations.
* n_Fixed transforms the factor that reports whether a contract was fixed price or cost-based (FxCB) to a numberical value. 0 for cost-based, 0.5 or "combination or other", 1 for any fixed price (excluding fixed-price level of effort which is classified as cost-based).
* nIncent transforms the factor that reports the fee type (Fee) to a numerical value. 1 for incentive fee or cost sharing, 0.5 or "combination or other", 0 for all remaining types
*  nComp transforms the factor that reports the fee type (cb_Comp) to a numerical value. 1 for Competed, 0 for not competed.
*  n_UCA transforms the factor that reports the use of undefinitized contract awards (UCA) to a numerical value. 1 for UCA, 0 for otherwise.
*  lOffer is the natural log of the number of offers received. Uncompeted contracts are classified as having received one offer.


In addition, l_Ceil and l_Days are rescaled to have an average value of 0 and a standard deviation of one. This standardizes interpretation and prevents a "very large eigen value" error later when l_Ceil:lDay is introduced.


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
##   Action.Obligation StartFY
## 1           6500.00    2011
## 2           3469.78    2016
## 3           7687.00    2009
## 4          22000.00    2010
## 5            778.72    2015
## 6           4406.00    2009
```



Next, we eliminate missing data entries and then draw a sample. The final computation uses all of the data, but as a computation shortcut, only a subset of the data is needed to allow for processing of models in minutes rather than hours.

###Computational Sample Creation

```r
DefenseModelAndDetail<-transform_contract(DefenseModelAndDetail)
```

```
## Warning in log(contract$UnmodifiedDays): NaNs produced
```

```r
Term_smp<-DefenseModelAndDetail[!is.na(DefenseModelAndDetail$b_Term)&
                                  !is.na(DefenseModelAndDetail$Comp),]


Term_smp<-Term_smp[sample(nrow(Term_smp),250000),]


Term_smp$cl_Ceil<-scale(Term_smp$l_Ceil)
Term_smp$cl_Days<-scale(Term_smp$l_Days)
Term_smp$cb_Comp<-scale(Term_smp$b_Comp)
Term_smp$cn_Comp<-scale(Term_smp$n_Comp)
Term_smp$cn_Offr<-scale(Term_smp$n_Offr)
Term_smp$cl_Offer<-scale(Term_smp$l_Offer)


head(Term_smp)
```

```
##          CSIScontractID        FxCb                      Fee Comp
## 217902         63748753 Fixed-Price Combination or Other Fee    1
## 8684335        60711081 Fixed-Price Combination or Other Fee    0
## 11100305       63994008 Fixed-Price Combination or Other Fee    1
## 6168115        24702086 Fixed-Price            FFP or No Fee    0
## 2588907        65595020 Fixed-Price            FFP or No Fee    0
## 1902140        65340974 Fixed-Price Combination or Other Fee    1
##                    Who                           What      Intl      PSR
## 217902   Uncategorized                          Other Unlabeled Products
## 8684335      Other DoD    Facilities and Construction Just U.S. Products
## 11100305 Uncategorized                          Other Unlabeled Products
## 6168115           Army    Facilities and Construction Just U.S. Services
## 2588907  Uncategorized Electronics and Communications Unlabeled Services
## 1902140  Uncategorized                          Other Unlabeled Products
##                      LowCeil                Ceil             Dur
## 217902   [0.00e+00,1.50e+04) [0.00e+00,1.50e+04) [-43558,    61)
## 8684335  [0.00e+00,1.50e+04) [0.00e+00,1.50e+04) [-43558,    61)
## 11100305 [0.00e+00,1.50e+04) [0.00e+00,1.50e+04) [-43558,    61)
## 6168115  [0.00e+00,1.50e+04) [0.00e+00,1.50e+04) [-43558,    61)
## 2588907  [0.00e+00,1.50e+04) [0.00e+00,1.50e+04) [   366,   732)
## 1902140  [0.00e+00,1.50e+04) [0.00e+00,1.50e+04) [-43558,    61)
##          SingleOffer      Offr     UCA            CRai NChg          Veh
## 217902        Single         1    <NA> [-0.001, 0.001)    0 SINGLE AWARD
## 8684335       Single         1    <NA> [-0.001, 0.001)    0 SINGLE AWARD
## 11100305       Multi [  5,999]    <NA> [-0.001, 0.001)    0         <NA>
## 6168115       Single         1 Not UCA [-0.001, 0.001)    0 SINGLE AWARD
## 2588907       Single         1    <NA> [-0.001, 0.001)    0         <NA>
## 1902140        Multi [  5,999]    <NA> [-0.001, 0.001)    0         <NA>
##          UnmodifiedNumberOfOffersReceived         Term
## 217902                                  1 Unterminated
## 8684335                                 1 Unterminated
## 11100305                              140 Unterminated
## 6168115                                 1 Unterminated
## 2588907                                 1 Unterminated
## 1902140                                54 Unterminated
##          UnmodifiedContractBaseAndAllOptionsValue SumOfisChangeOrder
## 217902                                     403.13                  0
## 8684335                                   4180.24                  0
## 11100305                                 10464.41                  0
## 6168115                                   2667.04                  0
## 2588907                                   8675.00                  0
## 1902140                                   2163.00                  0
##          pChangeOrderUnmodifiedBaseAndAll UnmodifiedDays
## 217902                                  0              1
## 8684335                                 0              2
## 11100305                                0              1
## 6168115                                 0             23
## 2588907                                 0            366
## 1902140                                 0              6
##          MinOfEffectiveDate Action.Obligation StartFY
## 217902           2016-03-04            403.13    2016
## 8684335          2015-08-04           4180.24    2015
## 11100305         2016-06-11          10464.41    2016
## 6168115          2011-04-06           2667.04    2011
## 2588907          2015-10-01           8675.00    2016
## 1902140          2016-09-14           2163.00    2016
##                                         PSR_What b_Crai     j_Crai b_Term
## 217902                            Products.Other      0 0.04334889      0
## 8684335     Products.Facilities and Construction      0 0.01766423      0
## 11100305                          Products.Other      0 0.04941184      0
## 6168115     Services.Facilities and Construction      0 0.02885874      0
## 2588907  Services.Electronics and Communications      0 0.03581444      0
## 1902140                           Products.Other      0 0.00090889      0
##               j_Term n_Crai l_Crai   l_Ceil    l_Days n_Fixed n_Incent
## 217902   0.003297944      0     NA 5.999259 0.0000000       1       NA
## 8684335  0.015224915      0     NA 8.338124 0.6931472       1       NA
## 11100305 0.028313225      0     NA 9.255735 0.0000000       1       NA
## 6168115  0.019454491      0     NA 7.888725 3.1354942       1        0
## 2588907  0.017997757      0     NA 9.068201 5.9026333       1        0
## 1902140  0.047650020      0     NA 7.679251 1.7917595       1       NA
##          b_Comp n_Comp n_Offr n_Intl n_UCA  l_Offer IDV SIDV MIDV OIDV
## 217902        1      1      1     NA    NA 0.000000   1    1    0    0
## 8684335       0      0      0      0    NA 0.000000   1    1    0    0
## 11100305      1      2      4     NA    NA 4.941642  NA   NA   NA   NA
## 6168115       0      0      0      0     0 0.000000   1    1    0    0
## 2588907       0      0      0     NA    NA 0.000000  NA   NA   NA   NA
## 1902140       1      2      4     NA    NA 3.988984  NA   NA   NA   NA
##              cl_Ceil    cl_Days   cb_Comp    cn_Comp    cn_Offr  cl_Offer
## 217902   -0.96712434 -1.5057100  0.428753 -0.7323899 -0.9438623 -1.035351
## 8684335  -0.05719406 -1.1399426 -2.332336 -2.0687567 -1.6493608 -1.035351
## 11100305  0.29980064 -1.5057100  0.428753  0.6039768  1.1726332  3.721709
## 6168115  -0.23203192  0.1488614 -2.332336 -2.0687567 -1.6493608 -1.035351
## 2588907   0.22684068  1.6090552 -2.332336 -2.0687567 -1.6493608 -1.035351
## 1902140  -0.31352697 -0.5602150  0.428753  0.6039768  1.1726332  2.804635
```

#Estimate Model

##Study Variable: Competition
Competition has the potential to reduce terminations by giving the government more options in vendors and encouraging better behavior from the contractor that was chosen. 
###Model 01A: No Competition / Competition

```r
#Frequency Plot
freq_discrete_term_plot(Term_smp,"Comp")
```

```
## Warning: Ignoring unknown parameters: binwidth, bins, pad
```

![](Contract_Termination_files/figure-html/Model01A-1.png)<!-- -->

```r
#Percent Terminated Plot
discrete_percent_term_plot(Term_smp,"b_Comp")
```

![](Contract_Termination_files/figure-html/Model01A-2.png)<!-- -->

```r
#Model
Term_01A <- glm (data=Term_smp,
                 b_Term ~ cb_Comp, family=binomial(link="logit"))
display(Term_01A)
```

```
## glm(formula = b_Term ~ cb_Comp, family = binomial(link = "logit"), 
##     data = Term_smp)
##             coef.est coef.se
## (Intercept) -4.81     0.02  
## cb_Comp     -0.11     0.02  
## ---
##   n = 250000, k = 2
##   residual deviance = 23636.4, null deviance = 23662.8 (difference = 26.4)
```

```r
#Plot the new variable and coefficients
#Add the fitted regression line
discrete_fitted_term_model(Term_smp,"cb_Comp") + 
  stat_function(fun = fit_curve, args=list(a=coef(Term_01A)[1],                                                        b=coef(Term_01A)[2]),color="blue")
```

![](Contract_Termination_files/figure-html/Model01A-3.png)<!-- -->

```r
#Save this for a future GLM
# Term_data_01A<-data.frame(fitted=fitted(Term_01A),
#                        residuals=residuals(Term_01A),
#                        nTerm=Term_01A@frame$nTerm,
#                        cb_Comp=Term_01A@frame$cb_Comp
#                        )
```
Competition alone has a very slight, but significant, negative correlation with terminations. The next step is to check alternate formulations. There's not yet enough in this model for any meaningful residual models. The next step is to look at a more detailed model, where single offer competition is broken out as a middle option between competition and no competion.

###Model 01B: No Competition / Single Offer / Competition

```r
#Frequency Plot
freq_discrete_term_plot(Term_smp,"n_Comp")
```

```
## Warning: Ignoring unknown parameters: binwidth, bins, pad
```

```
## Warning: Removed 1351 rows containing non-finite values (stat_count).
```

![](Contract_Termination_files/figure-html/Model01B-1.png)<!-- -->

```r
#Percent Terminated Plot
discrete_percent_term_plot(Term_smp,"n_Comp")
```

```
## Warning: Removed 1 rows containing missing values (geom_point).
```

![](Contract_Termination_files/figure-html/Model01B-2.png)<!-- -->

```r
#Model
Term_01B <- glm (data=Term_smp,
                 b_Term ~ cn_Comp, family=binomial(link="logit"))
display(Term_01B)
```

```
## glm(formula = b_Term ~ cn_Comp, family = binomial(link = "logit"), 
##     data = Term_smp)
##             coef.est coef.se
## (Intercept) -4.82     0.02  
## cn_Comp     -0.13     0.02  
## ---
##   n = 248649, k = 2
##   residual deviance = 23331.9, null deviance = 23371.7 (difference = 39.8)
```

```r
#Plot the Fitted
#Add the fitted regression line
discrete_fitted_term_model(Term_smp,"n_Comp") +
  stat_function(fun = fit_curve, args=list(a=coef(Term_01B)[1],
                                           b=coef(Term_01B)[2]),
                color="blue")
```

```
## Warning: Removed 1351 rows containing missing values (geom_point).
```

![](Contract_Termination_files/figure-html/Model01B-3.png)<!-- -->


The correltion between the three categories of competition has a smaller coefficent, but is still signficant. In the next step, the multiple award competition is broken down in greater detail.

###Model 01C: No Competition / Number of Offer Categories 

```r
#Frequency Plot
freq_discrete_term_plot(Term_smp,"n_Offr")
```

```
## Warning: Ignoring unknown parameters: binwidth, bins, pad
```

```
## Warning: Removed 1351 rows containing non-finite values (stat_count).
```

![](Contract_Termination_files/figure-html/Model01C-1.png)<!-- -->

```r
#Percent Terminated Plot
discrete_percent_term_plot(Term_smp,"n_Offr")
```

```
## Warning: Removed 1 rows containing missing values (geom_point).
```

![](Contract_Termination_files/figure-html/Model01C-2.png)<!-- -->

```r
#Model
Term_01C <- glm (data=Term_smp,
                 b_Term ~ cn_Offr, family=binomial(link="logit"))
display(Term_01C)
```

```
## glm(formula = b_Term ~ cn_Offr, family = binomial(link = "logit"), 
##     data = Term_smp)
##             coef.est coef.se
## (Intercept) -4.81     0.02  
## cn_Offr     -0.04     0.02  
## ---
##   n = 248649, k = 2
##   residual deviance = 23368.5, null deviance = 23371.7 (difference = 3.2)
```

```r
#Plot the Fitted
#Add the fitted regression line
discrete_fitted_term_model(Term_smp,"n_Comp") +
  stat_function(fun = fit_curve, args=list(a=coef(Term_01C)[1],
                                           b=coef(Term_01C)[2]),
                color="blue")
```

```
## Warning: Removed 1351 rows containing missing values (geom_point).
```

![](Contract_Termination_files/figure-html/Model01C-3.png)<!-- -->

As measured by residual deviance, this variable has slightly less predictive power than random noise. That can likely be attributed to the fact that 5+ offers category has a higher termination rate than 2 and 3-4. In theory, this could be because of non-liner relationship, for example higher number of offers competition may be more vulnerable to aggressive bidding beavior. However, this explaination would likely be better captured by other hidden variables such as competition methods rather than having a direct relationship with an increased number of offers. As a result, we are not testing offers as a categorical variable with dummies. 

Instead, the last variant to be checked is the number of offers directly. This is handled logarithmicly because the differences between number of offers at the bottom end of the scale mtters much more than at the high end of the scale. In this method, contracts that were not open to competition are grouped with contracts that received only a single offer.


###Model 01D: Number of Offer Categories 

```r
#Frequency Plot
freq_continuous_term_plot(Term_smp,"l_Offer")
```

```
## Warning: Removed 2038 rows containing non-finite values (stat_bin).
```

![](Contract_Termination_files/figure-html/Model01D-1.png)<!-- -->

```r
#Percent Terminated Plot 
binned_percent_term_plot(Term_smp,"l_Offer")
```

![](Contract_Termination_files/figure-html/Model01D-2.png)<!-- -->

```r
#Percent Terminated Plot
binned_percent_term_plot(Term_smp,"l_Offer","FxCb")
```

![](Contract_Termination_files/figure-html/Model01D-3.png)<!-- -->

```r
#Percent Terminated Plot
binned_percent_term_plot(Term_smp,"l_Offer","PSR")
```

![](Contract_Termination_files/figure-html/Model01D-4.png)<!-- -->

```r
#Percent Terminated Plot
binned_percent_term_plot(Term_smp,"l_Offer","Veh")
```

![](Contract_Termination_files/figure-html/Model01D-5.png)<!-- -->

```r
#Model
Term_01D <- glm (data=Term_smp,
                 b_Term ~ cl_Offer, family=binomial(link="logit"))
display(Term_01D)
```

```
## glm(formula = b_Term ~ cl_Offer, family = binomial(link = "logit"), 
##     data = Term_smp)
##             coef.est coef.se
## (Intercept) -4.81     0.02  
## cl_Offer    -0.02     0.02  
## ---
##   n = 247962, k = 2
##   residual deviance = 23311.9, null deviance = 23312.4 (difference = 0.5)
```

```r
#Variant Model
Term_smp$clsqr_Offer<-Term_smp$cl_Offer^2
Term_01E <- glm (data=Term_smp,
                 b_Term ~ cl_Offer +clsqr_Offer, family=binomial(link="logit"))
display(Term_01E)
```

```
## glm(formula = b_Term ~ cl_Offer + clsqr_Offer, family = binomial(link = "logit"), 
##     data = Term_smp)
##             coef.est coef.se
## (Intercept) -4.79     0.03  
## cl_Offer     0.01     0.03  
## clsqr_Offer -0.02     0.01  
## ---
##   n = 247962, k = 3
##   residual deviance = 23310.4, null deviance = 23312.4 (difference = 2.1)
```

```r
#Plot the data and fitted curve
Term_01D_plot<-ggplot(data=Term_smp,
       aes(y=j_Term,x=cl_Offer))+geom_jitter(alpha=0.01,height=0)+scale_y_sqrt() +
  labs(title="Fitted Linear Model", caption="Source: FPDS, CSIS Analysis")

#Plot the Fitted
#Add the fitted regression line
discrete_fitted_term_model(Term_smp,"cl_Offer") +
  stat_function(fun = fit_curve, args=list(a=coef(Term_01D)[1],
                                           b=coef(Term_01D)[2]),
                             color="blue")
```

```
## Warning: Removed 2038 rows containing missing values (geom_point).
```

![](Contract_Termination_files/figure-html/Model01D-6.png)<!-- -->

```r
#Plot the fitted data versus actual outcomes
Term_01D_data<-data.frame(fitted=fitted(Term_01D),
                       residuals=residuals(Term_01D),
                       b_Term=Term_01D$model$b_Term,
                       cl_Offer=Term_01D$model$cl_Offer
                       )


#Plot the fitted values vs actual results
Term_01D_data$bin_fitted<-bin_df(Term_01D_data,rank_col="fitted")
ggplot(data=subset(Term_01D_data,!is.na(fitted) & !is.na(b_Term) ) %>% 
  group_by(bin_fitted) %>% 
  summarise (mean_Term = mean(b_Term),
             mean_fitted =mean(fitted)),
       aes(y=mean_Term,x=mean_fitted))+geom_point()+
  labs(title="Binned Fitted Linear Model",
          caption="Source: FPDS, CSIS Analysis")
```

![](Contract_Termination_files/figure-html/Model01D-7.png)<!-- -->

```r
#Plot the Residualts
Term_01D_res<-binned.resids (Term_01D_data$fitted,
                              Term_01D_data$fitted-Term_01D_data$b_Term, nclass=3)$binned

ggplot(data=Term_01D_res,
       aes(x=xbar,y-ybar))+
  geom_point(aes(y=ybar))+ #Residuals
  geom_line(aes(y=se2),col="grey")+
  geom_line(aes(y=-se2),col="grey")+
  labs(title="Binned residual plot",
       x="Estimated  Pr (Termination)",
       y="Average residual")
```

![](Contract_Termination_files/figure-html/Model01D-8.png)<!-- -->

Closer examination further undercuts the idea that more offers is associated with a lower termination risk. As measured by deviance, the number offers adds less to the model than random noise. A quick experiment also found that use of number of offer squared only made the model slightly better than noise.

A variety of binned percent terminated graphs found that in the middle range of the number of offers, there is often an increase in the percent terminated. The study team had expected that this phenomenon may prove isolated to a certain type of contracting mechanism or product/service, but that did not prove to be the case.

The difference between single-offer and effective competition is worth exploring, even if it lowers the significance of the starting model. Happily, this decision is supported by the fact that this three category variable offers the greatest reduction in residual deviance. However, lacking a specific theoretical argument for the importance of a greater number of offers, and given the results, the more detailed competition breakdowns will not be used. This may be an issue worth exploring more in a future work.

The greatest distinctiveness can be found in types of indefinite delivery vehicles. Definitive contracts and purchase orders show a higher termination rate as the number of offers increase. However, for single and multiple award IDVs, there's a decline in terminations rates correlated with low levels of competition, that often fades and even reverses as the number of offers increases. However, the results do not seem consistent with am exponential relationship.

##Scope Variables
###Model 02A: Cost Ceiling

The model starts by examing the influence of large ceilings on contracts, the core idea here, as observed in the fixed priced paper, is that larger contracts are at greater risk of termination. This can be explained by both the inherent risk in the contract and high transaction costs of contract termination which discourage making the effort for smaller contracts.


```r
#Frequency Plot for unlogged ceiling
freq_continuous_term_plot(Term_smp,"UnmodifiedContractBaseAndAllOptionsValue",
               bins=1000)
```

![](Contract_Termination_files/figure-html/Model02A-1.png)<!-- -->

```r
freq_continuous_term_plot(subset(Term_smp,UnmodifiedContractBaseAndAllOptionsValue<100000000),
               "UnmodifiedContractBaseAndAllOptionsValue",
               bins=1000)
```

![](Contract_Termination_files/figure-html/Model02A-2.png)<!-- -->

```r
#Frequency Plot for logged ceiling
freq_continuous_term_plot(Term_smp,"l_Ceil")
```

```
## Warning: Removed 396 rows containing non-finite values (stat_bin).
```

![](Contract_Termination_files/figure-html/Model02A-3.png)<!-- -->

```r
#Percent Terminated Plot
binned_percent_term_plot(Term_smp,"l_Ceil")
```

![](Contract_Termination_files/figure-html/Model02A-4.png)<!-- -->

```r
#Model
Term_02A <- glm (data=Term_smp,
                 b_Term ~n_Comp + cl_Ceil, family=binomial(link="logit"))
display(Term_02A)
```

```
## glm(formula = b_Term ~ n_Comp + cl_Ceil, family = binomial(link = "logit"), 
##     data = Term_smp)
##             coef.est coef.se
## (Intercept) -4.80     0.05  
## n_Comp      -0.09     0.03  
## cl_Ceil      0.51     0.02  
## ---
##   n = 248258, k = 3
##   residual deviance = 22791.7, null deviance = 23346.1 (difference = 554.4)
```

```r
#Plot the fitted model plot
Term_02A_curve<-function(x, Comp){invlogit(cbind(1,Comp,x) %*%  coef(Term_02A))}

#Competition curves
fitted_term_model(Term_smp,"cl_Ceil") + stat_function(fun = Term_02A_curve, 
                             args=list(Comp=0),color="blue")+
  stat_function(fun = Term_02A_curve, 
                             args=list(Comp=2),color="blue")
```

```
## Warning: Removed 396 rows containing missing values (geom_point).
```

![](Contract_Termination_files/figure-html/Model02A-5.png)<!-- -->

```r
#Plot the fitted values vs actual results
binned_fitted_versus_residuals(Term_02A)
```

![](Contract_Termination_files/figure-html/Model02A-6.png)<!-- -->

```r
#Plot residuals versus fitted
residuals_term_plot(Term_02A)+
  labs(x="Estimated  Pr (Termination)")
```

![](Contract_Termination_files/figure-html/Model02A-7.png)<!-- -->

```r
residuals_term_plot(Term_02A,"cl_Ceil")+
  labs(x="Centered Log(Ceiling)")
```

![](Contract_Termination_files/figure-html/Model02A-8.png)<!-- -->

Contract ceiling has a significant relationship, though the residuals show a possible non-linear patterns. When the initial contract ceiling is particularly low or high, the fitted model underestimates the risk of terminations. This suggests that addding the log(Ceil)^2 might better capture the underlying data.



###Model 02B: Cost Ceiling Squared


```r
Term_smp$clsqr_Ceil<-Term_smp$cl_Ceil^2
Term_smp$lsqr_Ceil<-Term_smp$l_Ceil^2


#Frequency Plot
freq_continuous_term_plot(Term_smp,"lsqr_Ceil")
```

```
## Warning: Removed 396 rows containing non-finite values (stat_bin).
```

![](Contract_Termination_files/figure-html/Model02B-1.png)<!-- -->

```r
#Percent Terminated Plot
binned_percent_term_plot(Term_smp,"lsqr_Ceil")
```

![](Contract_Termination_files/figure-html/Model02B-2.png)<!-- -->

```r
#Model
Term_02B <- glm (data=Term_smp,
                 b_Term ~n_Comp + cl_Ceil + clsqr_Ceil, family=binomial(link="logit"))
display(Term_02B)
```

```
## glm(formula = b_Term ~ n_Comp + cl_Ceil + clsqr_Ceil, family = binomial(link = "logit"), 
##     data = Term_smp)
##             coef.est coef.se
## (Intercept) -4.79     0.05  
## n_Comp      -0.09     0.03  
## cl_Ceil      0.58     0.03  
## clsqr_Ceil  -0.04     0.01  
## ---
##   n = 248258, k = 4
##   residual deviance = 22778.5, null deviance = 23346.1 (difference = 567.6)
```

```r
#Plot the fitted model plot
Term_02B_curve<-function(x, Comp){invlogit(cbind(1,Comp,x, x^2) %*%  coef(Term_02B))}

#Competition curves
fitted_term_model(Term_smp,"cl_Ceil") + stat_function(fun = Term_02B_curve, 
                             args=list(Comp=0),color="blue")+
  stat_function(fun = Term_02B_curve, 
                             args=list(Comp=2),color="blue")
```

```
## Warning: Removed 396 rows containing missing values (geom_point).
```

![](Contract_Termination_files/figure-html/Model02B-3.png)<!-- -->

```r
#Plot the fitted values vs actual results
binned_fitted_versus_residuals(Term_02B)
```

![](Contract_Termination_files/figure-html/Model02B-4.png)<!-- -->

```r
#Plot residuals versus fitted
residuals_term_plot(Term_02B)+
  labs(x="Estimated  Pr (Termination)")
```

![](Contract_Termination_files/figure-html/Model02B-5.png)<!-- -->

```r
residuals_term_plot(Term_02B,"cl_Ceil")+
  labs(x="Centered Log(Ceiling)")
```

![](Contract_Termination_files/figure-html/Model02B-6.png)<!-- -->

Contracts with very small or very large ceilings appear have a negative coefficient for calculating termination. On the upper end, this might represent contracts that are "too large to fail"" or simply that above a certain size, risk stops rising. Adding the log(Ceiling) squared greatly reduces the apparent pattern in the residuals, although more than 5 percent of the binned residuals still fall outside the 95% confidence interval.

###Model 02C: Cost Ceiling and Competition




```r
#Frequency Plot
freq_continuous_term_plot(Term_smp,"l_Ceil","n_Comp")
```

```
## Warning: Removed 396 rows containing non-finite values (stat_bin).
```

![](Contract_Termination_files/figure-html/Model02C-1.png)<!-- -->

```r
#Plot the percent terminated across cost ceiling and competition
binned_percent_term_plot(Term_smp,"l_Ceil","n_Comp")
```

![](Contract_Termination_files/figure-html/Model02C-2.png)<!-- -->

```r
#Create the model
Term_02C <- glm (data=Term_smp,
                 b_Term ~n_Comp + cl_Ceil + clsqr_Ceil + n_Comp:cl_Ceil, family=binomial(link="logit"))
display(Term_02C)
```

```
## glm(formula = b_Term ~ n_Comp + cl_Ceil + clsqr_Ceil + n_Comp:cl_Ceil, 
##     family = binomial(link = "logit"), data = Term_smp)
##                coef.est coef.se
## (Intercept)    -4.62     0.05  
## n_Comp         -0.19     0.03  
## cl_Ceil         0.32     0.05  
## clsqr_Ceil     -0.05     0.01  
## n_Comp:cl_Ceil  0.18     0.03  
## ---
##   n = 248258, k = 5
##   residual deviance = 22743.2, null deviance = 23346.1 (difference = 602.9)
```

```r
Term_02C_curve<-function(x, Comp){invlogit(cbind(1,Comp,x,x^2,Comp*x) %*%  coef(Term_02C))}

#Competition Curves
fitted_term_model(Term_smp,"cl_Ceil") + stat_function(fun = Term_02C_curve, 
                             args=list(Comp=0),color="blue")+
  stat_function(fun = Term_02C_curve, 
                             args=list(Comp=2),color="blue")
```

```
## Warning: Removed 396 rows containing missing values (geom_point).
```

![](Contract_Termination_files/figure-html/Model02C-3.png)<!-- -->

```r
#Plot the fitted values vs actual results
binned_fitted_versus_residuals(Term_02C)
```

![](Contract_Termination_files/figure-html/Model02C-4.png)<!-- -->

```r
#Plot residuals versus fitted
residuals_term_plot(Term_02C)+
  labs(x="Estimated  Pr (Termination)")
```

![](Contract_Termination_files/figure-html/Model02C-5.png)<!-- -->

```r
residuals_term_plot(Term_02C,"cl_Ceil")+
  labs(x="Centered Log(Ceiling)")
```

![](Contract_Termination_files/figure-html/Model02C-6.png)<!-- -->
The interaction went in the expected direction, but is surprisingly powerful. Competition heightens the importance of contract ceiling, and larger contract ceilings mitigate the importance of competition. The other powerful part of this interaction was the dramatic increase in the coefficient for competition and the comparative reduction in the correlation of ceiling.

###Model 02D: Maximum Duration


```r
#Frequency Plot for max duration
freq_continuous_term_plot(Term_smp,"UnmodifiedDays",
               bins=1000)
```

```
## Warning: Removed 8 rows containing non-finite values (stat_bin).
```

![](Contract_Termination_files/figure-html/Model02D-1.png)<!-- -->

```r
#Frequency Plot for logged max duration
freq_continuous_term_plot(Term_smp,"l_Days")
```

```
## Warning: Removed 1598 rows containing non-finite values (stat_bin).
```

![](Contract_Termination_files/figure-html/Model02D-2.png)<!-- -->

```r
#Percent Terminated Plot
binned_percent_term_plot(Term_smp,"l_Days")
```

![](Contract_Termination_files/figure-html/Model02D-3.png)<!-- -->

```r
#Model
Term_02D <- glm (data=Term_smp,
                 b_Term ~n_Comp + 
                   cl_Ceil + clsqr_Ceil + cl_Days + clsqr_Ceil +
                   n_Comp:cl_Ceil , family=binomial(link="logit"))
display(Term_02D)
```

```
## glm(formula = b_Term ~ n_Comp + cl_Ceil + clsqr_Ceil + cl_Days + 
##     clsqr_Ceil + n_Comp:cl_Ceil, family = binomial(link = "logit"), 
##     data = Term_smp)
##                coef.est coef.se
## (Intercept)    -5.35     0.06  
## n_Comp          0.03     0.03  
## cl_Ceil        -0.08     0.06  
## clsqr_Ceil     -0.05     0.02  
## cl_Days         1.04     0.03  
## n_Comp:cl_Ceil  0.14     0.03  
## ---
##   n = 246889, k = 6
##   residual deviance = 21138.7, null deviance = 22986.9 (difference = 1848.2)
```

```r
Term_02D_curve<-function(x, Comp,Ceil){invlogit(cbind(1,Comp,Ceil,Ceil^2,x,Comp*Ceil) %*%  coef(Term_02D))}

#Competition Curves
fitted_term_model(Term_smp,"cl_Days") + stat_function(fun = Term_02D_curve, 
                             args=list(Comp=0,Ceil=0),color="blue")+
  stat_function(fun = Term_02D_curve, 
                             args=list(Comp=2,Ceil=0),color="blue")
```

```
## Warning: Removed 1598 rows containing missing values (geom_point).
```

![](Contract_Termination_files/figure-html/Model02D-4.png)<!-- -->

```r
#Ceiling Curves
fitted_term_model(Term_smp,"cl_Days") +  stat_function(fun = Term_02D_curve, 
                             args=list(Comp=0,Ceil=0),color="blue")+
  stat_function(fun = Term_02D_curve, 
                             args=list(Comp=0,Ceil=1),color="blue")
```

```
## Warning: Removed 1598 rows containing missing values (geom_point).
```

![](Contract_Termination_files/figure-html/Model02D-5.png)<!-- -->

```r
#Plot the fitted values vs actual results
binned_fitted_versus_residuals(Term_02D)
```

![](Contract_Termination_files/figure-html/Model02D-6.png)<!-- -->

```r
#Plot residuals versus fitted
residuals_term_plot(Term_02D)+
  labs(x="Estimated  Pr (Termination)")
```

![](Contract_Termination_files/figure-html/Model02D-7.png)<!-- -->

```r
# debug(binned.resids)
# residuals_term_plot(Term_02D,"cl_Days")+
  # labs(x="Centered Log(Ceiling)")
```
The initial model results are surprising as ceiling change direction, reducing and correlates with a lower risk of termiation now, although the interaction between competition and ceiling remains roughly the same. The graph of the fitted values shows that this model The next step will be to look at the interaction of ceiling and duration. The difference in deviance for residual difference has more than trippled, an indication of the correlative power of duration. However, the residuals do show a clear pattern that indicates another exponential variable may be necessary. However, the interaction between ceiling and duration is a more important analytical question and will be the next variable added.

###Model 02E: Interation of Ceiling and Duration

```r
#First a quick scatter plot for terminations by duration and ceiling
ggplot(data=subset(Term_smp,!is.na(l_Ceil)),
       aes(x=l_Days,y=l_Ceil))+geom_point(alpha=0.1)+facet_grid(Term~.)+
  labs(title="Frequency by Termination",
          caption="Source: FPDS, CSIS Analysis")
```

```
## Warning: Removed 1369 rows containing missing values (geom_point).
```

![](Contract_Termination_files/figure-html/Mode02E-1.png)<!-- -->

```r
#Create the model
Term_02E <- glm (data=Term_smp,
                 b_Term ~n_Comp + 
                   cl_Ceil + clsqr_Ceil + cl_Days+
                   n_Comp:cl_Ceil  + cl_Ceil:cl_Days, family=binomial(link="logit"))
display(Term_02E)
```

```
## glm(formula = b_Term ~ n_Comp + cl_Ceil + clsqr_Ceil + cl_Days + 
##     n_Comp:cl_Ceil + cl_Ceil:cl_Days, family = binomial(link = "logit"), 
##     data = Term_smp)
##                 coef.est coef.se
## (Intercept)     -5.44     0.07  
## n_Comp           0.05     0.03  
## cl_Ceil          0.14     0.06  
## clsqr_Ceil       0.02     0.02  
## cl_Days          1.13     0.03  
## n_Comp:cl_Ceil   0.10     0.03  
## cl_Ceil:cl_Days -0.26     0.04  
## ---
##   n = 246889, k = 7
##   residual deviance = 21089.0, null deviance = 22986.9 (difference = 1897.9)
```

```r
Term_02E_curve<-function(x, Comp,Ceil){invlogit(
  cbind(1,Comp,Ceil,Ceil^2,x,
        Comp*Ceil,x*Ceil) %*%  coef(Term_02E))}



#Competition Curves
fitted_term_model(Term_smp,"cl_Days") + stat_function(fun = Term_02E_curve, 
                             args=list(Comp=0,Ceil=0),color="blue")+
  stat_function(fun = Term_02E_curve, 
                             args=list(Comp=2,Ceil=0),color="blue")
```

```
## Warning: Removed 1598 rows containing missing values (geom_point).
```

![](Contract_Termination_files/figure-html/Mode02E-2.png)<!-- -->

```r
#Ceiling Curves
fitted_term_model(Term_smp,"cl_Days") +  stat_function(fun = Term_02E_curve, 
                             args=list(Comp=0,Ceil=0),color="blue")+
  stat_function(fun = Term_02E_curve, 
                             args=list(Comp=0,Ceil=1),color="blue")
```

```
## Warning: Removed 1598 rows containing missing values (geom_point).
```

![](Contract_Termination_files/figure-html/Mode02E-3.png)<!-- -->

```r
#Plot the fitted values vs actual results
binned_fitted_versus_residuals(Term_02E)
```

![](Contract_Termination_files/figure-html/Mode02E-4.png)<!-- -->

```r
#Plot residuals versus fitted
residuals_term_plot(Term_02E)+
  labs(x="Estimated  Pr (Termination)")
```

![](Contract_Termination_files/figure-html/Mode02E-5.png)<!-- -->

```r
# debug(binned.resids)
# residuals_term_plot(Term_02E,"cl_Days")+
  # labs(x="Centered Log(Ceiling)")
```


The interaction of ceiling and duration is correlated with a lower rate of termination, which is to say that when either factor is high, the other has less effect. Thus long contracts with fewer dollars or high ceiling contract with shorter durations are at higher risk than more balanced contracts. This interaction also restores the positive coefficient for ceiling and switches ceiling squared to positive. The residuals still show a pattern tht could be explained by an exponential. Examining that possibility will be the next step.

###Model 02F: Duration Squared


```r
Term_smp$clsqr_Days<-Term_smp$cl_Days^2
Term_smp$lsqr_Days<-Term_smp$l_Days^2


#Frequency Plot
freq_continuous_term_plot(Term_smp,"lsqr_Days")
```

```
## Warning: Removed 1598 rows containing non-finite values (stat_bin).
```

![](Contract_Termination_files/figure-html/Model02F-1.png)<!-- -->

```r
#Percent Terminated Plot
binned_percent_term_plot(Term_smp,"lsqr_Days")
```

![](Contract_Termination_files/figure-html/Model02F-2.png)<!-- -->

```r
#Create the model
Term_02F <- glm (data=Term_smp,
                 b_Term ~n_Comp + 
                   cl_Ceil + clsqr_Ceil + cl_Days+ clsqr_Days +
                   n_Comp:cl_Ceil  + cl_Ceil:cl_Days, family=binomial(link="logit"))
display(Term_02F)
```

```
## glm(formula = b_Term ~ n_Comp + cl_Ceil + clsqr_Ceil + cl_Days + 
##     clsqr_Days + n_Comp:cl_Ceil + cl_Ceil:cl_Days, family = binomial(link = "logit"), 
##     data = Term_smp)
##                 coef.est coef.se
## (Intercept)     -5.38     0.07  
## n_Comp           0.05     0.03  
## cl_Ceil          0.11     0.07  
## clsqr_Ceil       0.02     0.02  
## cl_Days          1.24     0.05  
## clsqr_Days      -0.11     0.03  
## n_Comp:cl_Ceil   0.11     0.03  
## cl_Ceil:cl_Days -0.22     0.04  
## ---
##   n = 246889, k = 8
##   residual deviance = 21074.1, null deviance = 22986.9 (difference = 1912.9)
```

```r
Term_02F_curve<-function(x, Comp,Ceil){invlogit(
  cbind(1,Comp,Ceil,Ceil^2,x,x^2,
        Comp*Ceil,x*Ceil) %*%  coef(Term_02F))}

#Competition curves
fitted_term_model(Term_smp,"cl_Ceil") + stat_function(fun = Term_02F_curve, 
                             args=list(Comp=0),color="blue")+
  stat_function(fun = Term_02F_curve, 
                             args=list(Comp=2),color="blue")
```

```
## Warning: Computation failed in `stat_function()`:
## argument "Ceil" is missing, with no default
```

```
## Warning: Computation failed in `stat_function()`:
## argument "Ceil" is missing, with no default
```

```
## Warning: Removed 396 rows containing missing values (geom_point).
```

![](Contract_Termination_files/figure-html/Model02F-3.png)<!-- -->

```r
#Plot the fitted values vs actual results
binned_fitted_versus_residuals(Term_02F)
```

![](Contract_Termination_files/figure-html/Model02F-4.png)<!-- -->

```r
#Plot residuals versus fitted
residuals_term_plot(Term_02F)+
  labs(x="Estimated  Pr (Termination)")
```

![](Contract_Termination_files/figure-html/Model02F-5.png)<!-- -->

```r
# residuals_term_plot(Term_02F,"cl_days")+
#   labs(x="Centered Log(Days)")


residuals_term_plot(Term_02F,"cl_Ceil")+
  labs(x="Centered Log(Ceiling)")
```

![](Contract_Termination_files/figure-html/Model02F-6.png)<!-- -->
Adding log(initial duration)^2 does seem to address the troubling pattern present in the residuals, namely the parabolic curve and the large number of data bins falling outside of the 95% confidence interval. Both of the ceiling coefficients remain positive but they are no longer coefficient. The next step is to see whether log(initial cost ceiling)^2 is still useful in this model now that the square of duration is included.

###Model 02G: Testing the removal of Ceiling^2 


```r
#Create the model
Term_02G <- glm (data=Term_smp,
                 b_Term ~n_Comp + 
                   cl_Ceil + cl_Days+ clsqr_Days +
                   n_Comp:cl_Ceil  + cl_Ceil:cl_Days, family=binomial(link="logit"))
display(Term_02G)
```

```
## glm(formula = b_Term ~ n_Comp + cl_Ceil + cl_Days + clsqr_Days + 
##     n_Comp:cl_Ceil + cl_Ceil:cl_Days, family = binomial(link = "logit"), 
##     data = Term_smp)
##                 coef.est coef.se
## (Intercept)     -5.36     0.07  
## n_Comp           0.04     0.03  
## cl_Ceil          0.10     0.07  
## cl_Days          1.23     0.05  
## clsqr_Days      -0.11     0.03  
## n_Comp:cl_Ceil   0.11     0.03  
## cl_Ceil:cl_Days -0.20     0.03  
## ---
##   n = 246889, k = 7
##   residual deviance = 21075.8, null deviance = 22986.9 (difference = 1911.1)
```

```r
Term_02G_curve<-function(x, Comp,Ceil){invlogit(
  cbind(1,Comp,Ceil,x,x^2,
        Comp*Ceil,x*Ceil) %*%  coef(Term_02E))}

#Competition curves
fitted_term_model(Term_smp,"cl_Ceil") + stat_function(fun = Term_02G_curve, 
                             args=list(Comp=0),color="blue")+
  stat_function(fun = Term_02G_curve, 
                             args=list(Comp=2),color="blue")
```

```
## Warning: Computation failed in `stat_function()`:
## argument "Ceil" is missing, with no default

## Warning: Computation failed in `stat_function()`:
## argument "Ceil" is missing, with no default
```

```
## Warning: Removed 396 rows containing missing values (geom_point).
```

![](Contract_Termination_files/figure-html/Model02G-1.png)<!-- -->

```r
#Plot the fitted values vs actual results
binned_fitted_versus_residuals(Term_02G)
```

![](Contract_Termination_files/figure-html/Model02G-2.png)<!-- -->

```r
#Plot residuals versus fitted
residuals_term_plot(Term_02G)+
  labs(x="Estimated  Pr (Termination)")
```

![](Contract_Termination_files/figure-html/Model02G-3.png)<!-- -->

```r
residuals_term_plot(Term_02G,"cl_Ceil")+
  labs(x="Centered Log(Ceiling)")
```

![](Contract_Termination_files/figure-html/Model02G-4.png)<!-- -->

While the effect on the residual deviance is fairly minor, it does still appear that the ceiling^2 does reduce the incidence of non-linear patterns in the residuals. So for now we will be leaving it in.

###Model 02H: Interaction of Duration and Competition

```r
#Frequency Plot
ggplot(data=subset(Term_smp,!is.na(n_Comp)),
       aes(x=l_Days))+geom_histogram(bins=20)+facet_grid(Term~n_Comp,scales="free_y")+
  labs(title="Frequency by Termination",
          caption="Source: FPDS, CSIS Analysis")
```

```
## Warning: Removed 1598 rows containing non-finite values (stat_bin).
```

![](Contract_Termination_files/figure-html/Mode02H-1.png)<!-- -->

```r
#Plot the percent terminated across duration and competition
Term_smp$bin_Days_Comp<-bin_df(Term_smp,"l_Days")
ggplot(data=subset(Term_smp,!is.na(n_Comp)&!is.na(bin_Days_Comp)) %>% 
  group_by(bin_Days_Comp,n_Comp) %>% 
  summarise (mean_Term = mean(b_Term),
             mean_l_Days =mean(l_Days)),
       aes(y=mean_Term,x=mean_l_Days))+geom_point()+facet_wrap(~n_Comp)+facet_wrap(~n_Comp)+
  labs(title="Average Termination Rate",
          caption="Source: FPDS, CSIS Analysis")
```

```
## Warning: Removed 3 rows containing missing values (geom_point).
```

![](Contract_Termination_files/figure-html/Mode02H-2.png)<!-- -->

```r
#Create the model
Term_02H <- glm (data=Term_smp,
                 b_Term ~n_Comp + 
                   cl_Ceil + clsqr_Ceil + cl_Days+ clsqr_Days +
                   n_Comp:cl_Ceil  + cl_Ceil:cl_Days  + n_Comp:cl_Days, family=binomial(link="logit"))
display(Term_02H)
```

```
## glm(formula = b_Term ~ n_Comp + cl_Ceil + clsqr_Ceil + cl_Days + 
##     clsqr_Days + n_Comp:cl_Ceil + cl_Ceil:cl_Days + n_Comp:cl_Days, 
##     family = binomial(link = "logit"), data = Term_smp)
##                 coef.est coef.se
## (Intercept)     -5.09     0.08  
## n_Comp          -0.15     0.05  
## cl_Ceil          0.23     0.07  
## clsqr_Ceil       0.03     0.02  
## cl_Days          0.87     0.08  
## clsqr_Days      -0.08     0.03  
## n_Comp:cl_Ceil   0.04     0.03  
## cl_Ceil:cl_Days -0.25     0.04  
## n_Comp:cl_Days   0.23     0.04  
## ---
##   n = 246889, k = 9
##   residual deviance = 21044.3, null deviance = 22986.9 (difference = 1942.7)
```

```r
Term_02H_curve<-function(x, Comp,Ceil){invlogit(
  cbind(1,Comp,Ceil,Ceil^2,x,x^2,
        Comp*Ceil,x*Ceil,x*Comp) %*%  coef(Term_02H))}

#Competition curves
fitted_term_model(Term_smp,"cl_Days") + stat_function(fun = Term_02H_curve, 
                             args=list(Comp=0,Ceil=0),color="blue")+
  stat_function(fun = Term_02H_curve, 
                             args=list(Comp=2,Ceil=0),color="blue")
```

```
## Warning: Removed 1598 rows containing missing values (geom_point).
```

![](Contract_Termination_files/figure-html/Mode02H-3.png)<!-- -->

```r
#Plot the fitted values vs actual results
binned_fitted_versus_residuals(Term_02G)
```

![](Contract_Termination_files/figure-html/Mode02H-4.png)<!-- -->

```r
#Plot residuals versus fitted
residuals_term_plot(Term_02G)+
  labs(x="Estimated  Pr (Termination)")
```

![](Contract_Termination_files/figure-html/Mode02H-5.png)<!-- -->

```r
# residuals_term_plot(Term_02F,"cl_days")+
#   labs(x="Centered Log(Days)")
```
The interaction of competition and duration is significant in itself and also has interesting consequences for other variables, notably boosting the cooeficient for ceiling and reducing the coefficient for duration. Competed contracts have a greater coefficient for termination. For short duration contracts, competition has a negative and significant coefficient but for long duration the relationship reverses itself. This would be consistent with the benefits of competition fading as the competition slipped further into the past. Unfortunately the residuals still show questionable patterns that merit continued observation.

##Contract Vehicle
Our dataset includes both stand alone contract awards and task orders that are under larger indefinite delivery vehilces (IDVs). 

###Model 03A: Single-Award, Multi-Award, and Other Indefinite Delivery Vehicles



```r
#Frequency Plot
freq_discrete_term_plot(Term_smp,"Veh")
```

```
## Warning: Ignoring unknown parameters: binwidth, bins, pad
```

![](Contract_Termination_files/figure-html/Model03A-1.png)<!-- -->

```r
#Percent Terminated Plot 
discrete_percent_term_plot(Term_smp,"Veh")
```

![](Contract_Termination_files/figure-html/Model03A-2.png)<!-- -->

```r
#Create the model
Term_03A <- glm (data=Term_smp,
                 b_Term ~n_Comp + 
                   cl_Ceil + clsqr_Ceil + cl_Days+ clsqr_Days +
                   SIDV + MIDV + OIDV +
                   n_Comp:cl_Ceil  + cl_Ceil:cl_Days  + n_Comp:cl_Days, family=binomial(link="logit"))
display(Term_03A)
```

```
## glm(formula = b_Term ~ n_Comp + cl_Ceil + clsqr_Ceil + cl_Days + 
##     clsqr_Days + SIDV + MIDV + OIDV + n_Comp:cl_Ceil + cl_Ceil:cl_Days + 
##     n_Comp:cl_Days, family = binomial(link = "logit"), data = Term_smp)
##                 coef.est coef.se
## (Intercept)     -4.73     0.09  
## n_Comp          -0.14     0.05  
## cl_Ceil          0.11     0.08  
## clsqr_Ceil       0.02     0.02  
## cl_Days          0.66     0.08  
## clsqr_Days       0.01     0.03  
## SIDV            -0.92     0.07  
## MIDV            -0.31     0.11  
## OIDV            -0.14     0.08  
## n_Comp:cl_Ceil   0.10     0.04  
## cl_Ceil:cl_Days -0.12     0.05  
## n_Comp:cl_Days   0.14     0.05  
## ---
##   n = 184080, k = 12
##   residual deviance = 16307.3, null deviance = 17819.6 (difference = 1512.3)
```

```r
#Plot the model and Vehicle for SIDV
Term_03A_SIDV_curve<-function(x, Comp,Ceil,Days,MIDV,OIDV){invlogit(
  cbind(1,Comp,Ceil,Ceil^2,Days,Days^2,x,MIDV,OIDV,
        Comp*Ceil,Days*Ceil,Days*Comp) %*%  coef(Term_03A))}

#Competition curves
discrete_fitted_term_model(Term_smp,"SIDV") +
  stat_function(fun = Term_03A_SIDV_curve, 
                             args=list(Comp=0,Ceil=0,Days=0,
                                       MIDV=0,OIDV=0),color="blue")+
  stat_function(fun = Term_03A_SIDV_curve, 
                             args=list(Comp=2,Ceil=0,Days=0,
                                       MIDV=0,OIDV=0),color="blue")
```

```
## Warning: Removed 62833 rows containing missing values (geom_point).
```

![](Contract_Termination_files/figure-html/Model03A-3.png)<!-- -->

```r
Term_03A_MIDV_curve<-function(x, Comp,Ceil,Days,SIDV,OIDV){invlogit(
  cbind(1,Comp,Ceil,Ceil^2,Days,Days^2,SIDV,x,OIDV,
        Comp*Ceil,Days*Ceil,Days*Comp) %*%  coef(Term_03A))}

#Plot the model and Vehicle for MIDV
#Competition curves
discrete_fitted_term_model(Term_smp,"MIDV") +
  stat_function(fun = Term_03A_MIDV_curve, 
                             args=list(Comp=0,Ceil=0,Days=0,
                                       SIDV=0,OIDV=0),color="blue")+
  stat_function(fun = Term_03A_MIDV_curve, 
                             args=list(Comp=2,Ceil=0,Days=0,
                                       SIDV=0,OIDV=0),color="blue")
```

```
## Warning: Removed 62833 rows containing missing values (geom_point).
```

![](Contract_Termination_files/figure-html/Model03A-4.png)<!-- -->

```r
Term_03A_OIDV_curve<-function(x, Comp,Ceil,Days,SIDV,MIDV){invlogit(
  cbind(1,Comp,Ceil,Ceil^2,Days,Days^2,SIDV,MIDV,x,
        Comp*Ceil,Days*Ceil,Days*Comp) %*%  coef(Term_03A))}

#Plot the model and Vehicle for OIDV
#Competition curves
discrete_fitted_term_model(Term_smp,"OIDV") +
  stat_function(fun = Term_03A_OIDV_curve, 
                             args=list(Comp=0,Ceil=0,Days=0,
                                       SIDV=0,MIDV=0),color="blue")+
  stat_function(fun = Term_03A_OIDV_curve, 
                             args=list(Comp=2,Ceil=0,Days=0,
                                       SIDV=0,MIDV=0),color="blue")
```

```
## Warning: Removed 62833 rows containing missing values (geom_point).
```

![](Contract_Termination_files/figure-html/Model03A-5.png)<!-- -->

```r
#Plot the fitted values vs actual results
binned_fitted_versus_residuals(Term_03A)
```

![](Contract_Termination_files/figure-html/Model03A-6.png)<!-- -->

```r
#Plot residuals versus fitted
residuals_term_plot(Term_03A)+
  labs(x="Estimated  Pr (Termination)")
```

![](Contract_Termination_files/figure-html/Model03A-7.png)<!-- -->

```r
# residuals_term_plot(Term_02F,"cl_days")+
#   labs(x="Centered Log(Days)")
```

The first check lumps looks at three categories of IDVs, each facing a notably lower a risk of termination. The largest negative coefficient is for single award IDCs, multiple award IDVs second, and other IDVs third. The deviation residual notably creeps up after this addition, though this is likely attributable to a missing data problem rather than the new information not being useful. Unfortunately, while the for both ceiling^2 and duration^2 were notably reduced in magnitude, the residuals show significant patterns, underestimating the risk of termination in the fitted range from around 0.005 to 0.01 and overestimating by 0.02. There's no exponential term that makes sense to add here, which leaves the hope that interactions may address this phenomenon.

This may reflect that the Ceiling and Days entries reflect only the task order and not the larger contract. The next step is to check the intersections with ceiling.


###Model 03B: Vehicle and Contract Duration

```r
#Frequency Plot
ggplot(data=subset(Term_smp,!is.na(Veh)),
       aes(x=l_Days))+geom_histogram(bins=20)+facet_grid(Term~Veh,scales="free_y")+
  labs(title="Frequency by Termination",
          caption="Source: FPDS, CSIS Analysis")
```

```
## Warning: Removed 1593 rows containing non-finite values (stat_bin).
```

![](Contract_Termination_files/figure-html/Model03B-1.png)<!-- -->

```r
#Plot the percent terminated across duration and competition
Term_smp$bin_Days_Veh<-bin_df(Term_smp,"l_Days","Veh")
ggplot(data=subset(Term_smp,!is.na(n_Comp)&!is.na(bin_Days_Comp)) %>% 
  group_by(bin_Days_Veh,Veh) %>% 
  summarise (mean_Term = mean(b_Term),
             mean_l_Days =mean(cl_Days)),
       aes(y=mean_Term,x=mean_l_Days))+geom_point()+facet_wrap(~Veh)+
  labs(title="Average Termination Rate",
          caption="Source: FPDS, CSIS Analysis")
```

```
## Warning: Removed 4 rows containing missing values (geom_point).
```

![](Contract_Termination_files/figure-html/Model03B-2.png)<!-- -->

```r
#Create the model
Term_03B <- glm (data=Term_smp,
                 b_Term ~n_Comp + 
                   cl_Ceil + clsqr_Ceil + cl_Days+ clsqr_Days +
                   SIDV + MIDV + OIDV +
                   n_Comp:cl_Ceil  + cl_Ceil:cl_Days  + n_Comp:cl_Days +
                 SIDV:cl_Days+MIDV:cl_Days+OIDV:cl_Days, family=binomial(link="logit"))
display(Term_03B)
```

```
## glm(formula = b_Term ~ n_Comp + cl_Ceil + clsqr_Ceil + cl_Days + 
##     clsqr_Days + SIDV + MIDV + OIDV + n_Comp:cl_Ceil + cl_Ceil:cl_Days + 
##     n_Comp:cl_Days + SIDV:cl_Days + MIDV:cl_Days + OIDV:cl_Days, 
##     family = binomial(link = "logit"), data = Term_smp)
##                 coef.est coef.se
## (Intercept)     -4.66     0.09  
## n_Comp          -0.03     0.06  
## cl_Ceil          0.07     0.08  
## clsqr_Ceil       0.03     0.02  
## cl_Days          0.52     0.09  
## clsqr_Days       0.04     0.03  
## SIDV            -1.41     0.10  
## MIDV            -0.79     0.22  
## OIDV            -0.03     0.12  
## n_Comp:cl_Ceil   0.10     0.04  
## cl_Ceil:cl_Days -0.12     0.05  
## n_Comp:cl_Days   0.06     0.05  
## cl_Days:SIDV     0.52     0.08  
## cl_Days:MIDV     0.44     0.16  
## cl_Days:OIDV    -0.15     0.11  
## ---
##   n = 184080, k = 15
##   residual deviance = 16251.7, null deviance = 17819.6 (difference = 1567.9)
```

```r
# Plot the model and Vehicle for SIDV
Term_03B_SIDV_curve<-function(x, Comp,Ceil,Days,MIDV,OIDV){invlogit(
  cbind(1,Comp,Ceil,Ceil^2,Days,Days^2,x,MIDV,OIDV,
        Comp*Ceil,Days*Ceil,Days*Comp,
        Days*x,Days*MIDV,Days*OIDV) %*%  coef(Term_03B))}

#Duration curves
discrete_fitted_term_model(Term_smp,"SIDV") +
  stat_function(fun = Term_03B_SIDV_curve,
                             args=list(Comp=0,Ceil=0,Days=-1,
                                       MIDV=0,OIDV=0),color="blue")+
  stat_function(fun = Term_03B_SIDV_curve,
                             args=list(Comp=0,Ceil=0,Days=1,
                                       MIDV=0,OIDV=0),color="blue")
```

```
## Warning: Removed 62833 rows containing missing values (geom_point).
```

![](Contract_Termination_files/figure-html/Model03B-3.png)<!-- -->

```r
Term_03B_MIDV_curve<-function(x, Comp,Ceil,Days,SIDV,OIDV){invlogit(
  cbind(1,Comp,Ceil,Ceil^2,Days,Days^2,SIDV,x,OIDV,
        Comp*Ceil,Days*Ceil,Days*Comp,
        Days*SIDV,Days*x,Days*OIDV) %*%  coef(Term_03B))}

#Plot the model and Vehicle for MIDV
#Duration Curves
discrete_fitted_term_model(Term_smp,"MIDV") +
  stat_function(fun = Term_03B_MIDV_curve,
                             args=list(Comp=0,Ceil=0,Days=-1,
                                       SIDV=0,OIDV=0),color="blue")+
  stat_function(fun = Term_03B_MIDV_curve,
                             args=list(Comp=0,Ceil=0,Days=1,
                                       SIDV=0,OIDV=0),color="blue")
```

```
## Warning: Removed 62833 rows containing missing values (geom_point).
```

![](Contract_Termination_files/figure-html/Model03B-4.png)<!-- -->

```r
Term_03B_OIDV_curve<-function(x, Comp,Ceil,Days,SIDV,MIDV){invlogit(
  cbind(1,Comp,Ceil,Ceil^2,Days,Days^2,SIDV,MIDV,x,
        Comp*Ceil,Days*Ceil,Days*Comp,
        Days*SIDV,Days*MIDV,Days*x) %*%  coef(Term_03B))}

#Plot the model and Vehicle for OIDV
#Duration curves
discrete_fitted_term_model(Term_smp,"OIDV") +
  stat_function(fun = Term_03B_OIDV_curve,
                             args=list(Comp=0,Ceil=0,Days=-1,
                                       SIDV=0,MIDV=0),color="blue")+
  stat_function(fun = Term_03B_OIDV_curve,
                             args=list(Comp=0,Ceil=0,Days=1,
                                       SIDV=0,MIDV=0),color="blue")
```

```
## Warning: Removed 62833 rows containing missing values (geom_point).
```

![](Contract_Termination_files/figure-html/Model03B-5.png)<!-- -->

```r
#Plot the fitted values vs actual results
binned_fitted_versus_residuals(Term_03B)
```

![](Contract_Termination_files/figure-html/Model03B-6.png)<!-- -->

```r
#Plot residuals versus fittedO
residuals_term_plot(Term_03B)+
  labs(x="Estimated  Pr (Termination)")
```

![](Contract_Termination_files/figure-html/Model03B-7.png)<!-- -->

```r
# residuals_term_plot(Term_02F,"cl_days")+
#   labs(x="Centered Log(Days)")
```

The vast majority of very short duration contracts are IDVs. The distribution in each of the categories is typically not normal, Other IDVs in particularly being prone to two peaks. Interestingly enough, definitive contracts and purchase orders have the most consistent relationship with terminations, and single-award IDVs the most noisy. For the MIDVs and OIDVs, below l_days=2, or about the average point, there's almost no risk of termination, but that risk steadily rises above that level. 

Interactions:
* Single-Award IDVs have a large negative coefficient, but as the duration grows longer this affect is diminished and the importance of duration grows. 
* Multiple-Award IDVs now have the largest negative coefficient, but to an even greater degree than with Single-Award IDVs, as the duration grows longer this affect is diminished and the importance of duration grows. 
* Other IDVs now have a no longer signicant risk of termination, but the relationship with duration is the reverse of the other two, OIDVs dampen the rise in termination risk for longer contracts.



###Model 03C: Vehicle and Competition

```r
#Frequency Plot
ggplot(data=subset(Term_smp,!is.na(Veh)),
       aes(x=n_Comp))+geom_histogram(bins=20)+facet_grid(Term~Veh,scales="free_y")+
  labs(title="Frequency by Termination",
          caption="Source: FPDS, CSIS Analysis")
```

```
## Warning: Removed 1332 rows containing non-finite values (stat_bin).
```

![](Contract_Termination_files/figure-html/Model03C-1.png)<!-- -->

```r
#Plot the percent terminated across duration and competition
Term_smp$bin_n_Comp_Veh<-bin_df(Term_smp,"n_Comp","Veh")
ggplot(data=subset(Term_smp,!is.na(n_Comp)&!is.na(bin_n_Comp_Veh)) %>% 
  group_by(bin_n_Comp_Veh,Veh) %>% 
  summarise (mean_Term = mean(b_Term),
             mean_n_Comp =mean(n_Comp)),
       aes(y=mean_Term,x=mean_n_Comp))+geom_point()+facet_wrap(~Veh)+
  labs(title="Average Termination Rate",
          caption="Source: FPDS, CSIS Analysis")
```

![](Contract_Termination_files/figure-html/Model03C-2.png)<!-- -->

```r
#Create the model
Term_03C <- glm (data=Term_smp,
                 b_Term ~n_Comp + 
                   cl_Ceil + clsqr_Ceil + cl_Days+ clsqr_Days +
                   SIDV + MIDV + OIDV +
                   n_Comp:cl_Ceil  + cl_Ceil:cl_Days  + n_Comp:cl_Days +
                 SIDV:cl_Days+MIDV:cl_Days+OIDV:cl_Days +
                   SIDV:n_Comp+MIDV:n_Comp+OIDV:n_Comp , family=binomial(link="logit"))
display(Term_03C)
```

```
## glm(formula = b_Term ~ n_Comp + cl_Ceil + clsqr_Ceil + cl_Days + 
##     clsqr_Days + SIDV + MIDV + OIDV + n_Comp:cl_Ceil + cl_Ceil:cl_Days + 
##     n_Comp:cl_Days + SIDV:cl_Days + MIDV:cl_Days + OIDV:cl_Days + 
##     SIDV:n_Comp + MIDV:n_Comp + OIDV:n_Comp, family = binomial(link = "logit"), 
##     data = Term_smp)
##                 coef.est coef.se
## (Intercept)     -4.73     0.10  
## n_Comp           0.03     0.06  
## cl_Ceil          0.04     0.08  
## clsqr_Ceil       0.02     0.02  
## cl_Days          0.46     0.09  
## clsqr_Days       0.05     0.03  
## SIDV            -0.91     0.15  
## MIDV            -0.23     0.31  
## OIDV            -0.20     0.20  
## n_Comp:cl_Ceil   0.13     0.04  
## cl_Ceil:cl_Days -0.11     0.05  
## n_Comp:cl_Days   0.09     0.05  
## cl_Days:SIDV     0.46     0.08  
## cl_Days:MIDV     0.40     0.16  
## cl_Days:OIDV    -0.11     0.11  
## n_Comp:SIDV     -0.34     0.07  
## n_Comp:MIDV     -0.35     0.14  
## n_Comp:OIDV      0.11     0.11  
## ---
##   n = 184080, k = 18
##   residual deviance = 16224.0, null deviance = 17819.6 (difference = 1595.6)
```

```r
# Plot the model and Vehicle for SIDV
Term_03C_SIDV_curve<-function(x, Comp,Ceil,Days,MIDV,OIDV){invlogit(
  cbind(1,Comp,Ceil,Ceil^2,Days,Days^2,x,MIDV,OIDV,
        Comp*Ceil,Days*Ceil,Days*Comp,
        Days*x,Days*MIDV,Days*OIDV,
        Comp*x,Comp*MIDV,Comp*OIDV) %*%  coef(Term_03C))}

#Competition curves
discrete_fitted_term_model(Term_smp,"SIDV") +
  stat_function(fun = Term_03C_SIDV_curve,
                             args=list(Comp=0,Ceil=0,Days=0,
                                       MIDV=0,OIDV=0),color="blue")+
  stat_function(fun = Term_03C_SIDV_curve,
                             args=list(Comp=2,Ceil=0,Days=0,
                                       MIDV=0,OIDV=0),color="blue")
```

```
## Warning: Removed 62833 rows containing missing values (geom_point).
```

![](Contract_Termination_files/figure-html/Model03C-3.png)<!-- -->

```r
Term_03C_MIDV_curve<-function(x, Comp,Ceil,Days,SIDV,OIDV){invlogit(
  cbind(1,Comp,Ceil,Ceil^2,Days,Days^2,SIDV,x,OIDV,
        Comp*Ceil,Days*Ceil,Days*Comp,
        Days*SIDV,Days*x,Days*OIDV,
        Comp*SIDV,Comp*x,Comp*OIDV) %*%  coef(Term_03C))}

#Plot the model and Vehicle for MIDV
#Competition Curves
discrete_fitted_term_model(Term_smp,"MIDV") +
  stat_function(fun = Term_03C_MIDV_curve,
                             args=list(Comp=0,Ceil=0,Days=0,
                                       SIDV=0,OIDV=0),color="blue")+
  stat_function(fun = Term_03C_MIDV_curve,
                             args=list(Comp=2,Ceil=0,Days=0,
                                       SIDV=0,OIDV=0),color="blue")
```

```
## Warning: Removed 62833 rows containing missing values (geom_point).
```

![](Contract_Termination_files/figure-html/Model03C-4.png)<!-- -->

```r
Term_03C_OIDV_curve<-function(x, Comp,Ceil,Days,SIDV,MIDV){invlogit(
  cbind(1,Comp,Ceil,Ceil^2,Days,Days^2,SIDV,MIDV,x,
        Comp*Ceil,Days*Ceil,Days*Comp,
        Days*SIDV,Days*MIDV,Days*x,
        Comp*SIDV,Comp*MIDV,Comp*x) %*%  coef(Term_03C))}

#Plot the model and Vehicle for OIDV
#Competition curves
discrete_fitted_term_model(Term_smp,"OIDV") +
  stat_function(fun = Term_03C_OIDV_curve,
                             args=list(Comp=0,Ceil=0,Days=0,
                                       SIDV=0,MIDV=0),color="blue")+
  stat_function(fun = Term_03C_OIDV_curve,
                             args=list(Comp=2,Ceil=0,Days=0,
                                       SIDV=0,MIDV=0),color="blue")
```

```
## Warning: Removed 62833 rows containing missing values (geom_point).
```

![](Contract_Termination_files/figure-html/Model03C-5.png)<!-- -->

```r
#Plot the fitted values vs actual results
binned_fitted_versus_residuals(Term_03C)
```

![](Contract_Termination_files/figure-html/Model03C-6.png)<!-- -->

```r
#Plot residuals versus fittedO
residuals_term_plot(Term_03C)+
  labs(x="Estimated  Pr (Termination)")
```

![](Contract_Termination_files/figure-html/Model03C-7.png)<!-- -->

```r
# residuals_term_plot(Term_02F,"cl_days")+
#   labs(x="Centered Log(Days)")
```
The average plots did show varying relationships and slopes between different types of vehicles. However, 
the coeficient for competition itself remains negative, though not significant, despite the baseline vehicle state, definitive/purchase, having higher termination rates among competed contracts. Given the other interactions, this suggests that this phenomenon is already captured by other inputs and interactions in the dataset. 

Interactions
* For single-award IDVs, competition magnifies the lower termination coeffient. This is somewhat surprising, as competition reflects the competed status of the IDV, not the individual task order. Nonetheless, that rate appears to very much matter, and suggests that uncompeted IDVs lose some of their termination reduction correlation.
* For multiple-award IDVs, competition also dampens the termination rate. This also magnificies the import of duration for multiple-award IDVs, which now can complete overcome the base termination coefficient of the vehicle.
* Competition dampens the termination reduction coefficient for other IDVs. It is not significant, I am tempted to leave it out, though it may be appropriate to always leave in the triples.

###Model 03D: Any IDV and Contract ceiling

```r
# Term_03D <- glmer (data=Term_smp,
#                  b_Term ~ l_Ceil + l_Days + l_Ceil:l_Days + 
#                    IDV + IDV:l_Days+
#                (1 | Who) + (1 | PSR_What) + (1 | StartFY), family=binomial(link="logit"))
# display(Term_03D)
```

The results a dramatic, longer IDVs do face higher risks of termination. That said, the termination risk reduction for all IDVs has grown even further as a result, so even longer task orders may not face greater termination risks than comparable contract awards. With this addition, the intersection of contract ceiling and maximum duration is no longer significant, the next step will be testing its removal from the equation.


The lower rate of task order termination did not prove to be relevant to contract ceiling. The next step is to see whether there is any relationship to maximum duration. Under this theory, the fact that IDVs make it simpler to do multiple short task orders could mask any risk when task order duratioN grows longer.

###Model 03E: Trimming the intersection of duration and ceiling

```r
# Term_03E <- glmer (data=Term_smp,
#                  b_Term ~ l_Ceil + l_Days + 
#                    IDV + IDV:l_Days+
#                (1 | Who) + (1 | PSR_What) + (1 | StartFY), family=binomial(link="logit"))
# display(Term_03E)
```

The removal has little effect on the AIC or DIC but does simplify the model.

Subsequent steps will explore the difference between multiple-award IDVs, where the government can choose from a pre-qualified pool of vendors, and single-award IDVs, where the government only has one option. 


##Type of Contract
###Model 13A: Fixed-Price Contracts

The next step adds a measure for whether the contract was cost-based or fixed-price. Prior CSIS research has found that fixed-price contracts do face a higher risk of termination across the board.


```r
# Term_13A <- glmer (data=Term_smp,
#                  b_Term ~ l_Ceil + l_Days + 
#                    SIDV + MIDV + OIDV + SIDV:l_Days + 
#                    n_Fixed +
#                (1 | Who) + (1 | PSR_What) + (1 | StartFY), family=binomial(link="logit"))
# display(Term_13A)
```

And fixed price contracts do indeed have a appreciably higher average termination rate. This does not improve the overall AIC of the model, but it captures another important dynamic and has a coefficient that greatly exceeds its standard error. 

###Model 13B: Fixed-Price and Maximum Duration
Past CSIS research has found that fixed-price contracts do appear to be at higher risk if they have a longer maximum duration. Fixed-price contracting does require upfront estimation of likely costs, and thus a longer duration means more opportunity for changed circumstance.


```r
# Term_13B <- glmer (data=Term_smp,
#                   b_Term ~ l_Ceil + l_Days + 
#                    SIDV + MIDV + OIDV + SIDV:l_Days + 
#                    n_Fixed + l_Days:n_Fixed +
#                (1 | Who) + (1 | PSR_What) + (1 | StartFY), family=binomial(link="logit"))
# display(Term_13B)
```
The model failed to converge and the coefficient, while large, did not exceed the standard error. That said, it is worth noting that the sign went in the opposite direction from what was expected, with longer fixed-price contracts not being associated with disproprtionately higher terminations rates. Thus, the earlier finding is not replicated. 

###Model 13C: Incentive Fees

In the performance of the defense acquisition system report, the benefits found were reduced cost overruns. That could help avoid terminations, but it's an indirect connection at best. 


```r
# Term_13C <- glmer (data=Term_smp,
#                   b_Term ~ l_Ceil + l_Days + 
#                    SIDV + MIDV + OIDV + SIDV:l_Days + 
#                    n_Fixed + nIncent +
#                (1 | Who) + (1 | PSR_What) + (1 | StartFY), family=binomial(link="logit"))
# display(Term_13C)
```

###Model 13D: Undefinitized Contract Awards
Undefinitized Contract Awards allow for quick action in situations where there is not time or information to establish all of a contracts properties at the time of signing. They have been found by the GAO and the Performance of the Defense Acquisition studies to contain notable risks, primarily relating to cost overruns. Will these risks also carry into terminations?


```r
#Run through stopped here
# Term3D <- glmer (data=Term_smp,
#                   b_Term ~ l_Ceil + l_Days + 
#                    SIDV + MIDV + OIDV + SIDV:l_Days + 
#                    n_Fixed  + n_UCA +
#                (1 | Who) + (1 | PSR_What) + (1 | StartFY), family=binomial(link="logit"))
# display(Term3D)
```

Use of UCA significantly increases risks of termination, well in excess of the error term. Interestingly, while this addition further reduceds the AIC and DIC of the model, it also increases the coefficient for fixed price contracts.

##Place of Performance
###Model 14A: Any International


```r
# Term4A <- glmer (data=Term_smp,
#                   b_Term ~ l_Ceil + l_Days + 
#                    SIDV + MIDV + OIDV + SIDV:l_Days + 
#                    n_Fixed  + n_UCA +
#                    nIntl +
#                (1 | Who) + (1 | PSR_What) + (1 | StartFY), family=binomial(link="logit"))
# display(Term4A)
```

International performance of contract proved to not be an effective predictor of contract termination. In addition, the model failed to converge. This variable will not be added.

##Competition Variables
###Model 15A: Competition



###Model 15C: Competition and Fixed Price 


```r
# Term_02C <- glmer (data=Term_smp,b_Term ~ l_Ceil + l_Days + 
#                    SIDV + MIDV + OIDV + SIDV:l_Days + 
#                    n_Fixed  + n_UCA +
#                    cb_Comp + cb_Comp:n_Fixed +
#                (1 | Who) + (1 | PSR_What) + (1 | StartFY), family=binomial(link="logit"))
# display(Term_02C)
```
THese results are not sufficiently signficiant to justify inclusion, but the interaction between fixed price and competition is potentially interesting and suggests that the demographics of competed contrats may be one factor driving their increase in termination risk. Specifically, one possibility may be that competitive fixed-price contracts are at greater risk of aggressive bids that increase contract risk. 

###Model 15D: Fixed Price and Number of Offers for Competed Contracts


```r
# Term_02D <- glmer (data=subset(Term_smp,cb_Comp==1),
#               b_Term ~ l_Ceil + l_Days + 
#                 SIDV + MIDV + OIDV + SIDV:l_Days + 
#                    n_Fixed  + n_UCA +  lOffer + lOffer:n_Fixed +
#                (1 | Who) + (1 | PSR_What) + (1 | StartFY), family=binomial(link="logit"))
# display(Term_02D)
```

Focusing again on only those contracts that were competed, there does not appear to be a relationship between fixed price contracting and the number of offers. This undercuts the idea that the competition dynamics can be explained as simply relating to aggressive bidding on fixed price contracts.

###Model 15E: Competition and Contract Ceiling

```r
# Term_15E <- glmer (data=Term_smp,
#                  b_Term ~ l_Ceil + l_Days +
#                    SIDV + MIDV + OIDV + SIDV:l_Days +
#                 n_Fixed  + n_UCA +  
#                   cb_Comp + cb_Comp:l_Ceil +
#                (1 | Who) + (1 | PSR_What) + (1 | StartFY), family=binomial(link="logit"))
# display(Term_15E)
```
The coefficient for the interaction of Ceiling and Competition exceeds the standard error, but only minimally. The model also fails to converge, so we'll leave out this interaction.

###Model 15F: Single-Award IDVs and Competition 
The next step is to check interactions with the contract vehicles. Each has a slightly different approach to competition. For example, the competition for a Single-Award IDV happens at the start of the IDV, after that, contracts awarded through that vehicle go to that single winning vendor.


```r
# Term_15F <- glmer (data=Term_smp,
#                 b_Term ~ l_Ceil + l_Days +
#                 SIDV + MIDV + OIDV + SIDV:l_Days +
#                 n_Fixed  + n_UCA +  
#                 cb_Comp + cb_Comp:SIDV + 
#                (1 | Who) + (1 | PSR_What) + (1 | StartFY), family=binomial(link="logit"))
# display(Term_15F)
# 
```
That result proved stronger than expected and does seem to show that that competed IDVs do have a lower average risk of terminations. However, the model failed to converge.

Next, we'll check the same relationship, but with number of offers instead.

###Model 15G: Single-Award IDVs and Number of Offers 


```r
# Term_15G <- glmer (data=Term_smp,
#                  b_Term ~ l_Ceil + l_Days +
#                 SIDV + MIDV + OIDV + SIDV:l_Days +
#                 n_Fixed  + n_UCA +
#                 lOffer + lOffer:SIDV + 
#                (1 | Who) + (1 | PSR_What) + (1 | StartFY), family=binomial(link="logit"))
# display(Term_15G)
```
The model likewise failed to converge with number of offers and the number of offers proved a less powerful interaction variable than extent of competition.


###Model 15H: Multiple-Award IDVs and Competition


```r
# Term_15H <- glmer (data=Term_smp,
#                  b_Term ~ l_Ceil + l_Days +
#                 SIDV + MIDV + OIDV + SIDV:l_Days +
#                 n_Fixed  + n_UCA +
#                 cb_Comp + cb_Comp:MIDV + 
#                (1 | Who) + (1 | PSR_What) + (1 | StartFY), family=binomial(link="logit"))
# display(Term_15H)
# nrow(Term_smp[!is.na(Term_smp$l_Ceil) &
#                 !is.na(Term_smp$lDays) &
#                 !is.na(Term_smp$IDV) &
#               !is.na(Term_smp$nUCA) & 
#                 !is.na(Term_smp$Fixed)
#                 !is.na(Term_smp$cb_Comp),])
```
The combination of competition and multiple-award IDVs is powerful. Multiple-award IDVs entirely explains the competition 

###Model 15I: Multiple-Award IDVs and Number of Offers

```r
# Term_15I <- glmer (data=Term_smp,
#                  b_Term ~ l_Ceil + l_Days +
#                 SIDV + MIDV + OIDV + SIDV:l_Days +
#                 n_Fixed  + n_UCA +
#                 lOffer + MIDV:lOffer + 
#                (1 | Who) + (1 | PSR_What) + (1 | StartFY), family=binomial(link="logit"))
# display(Term_15I)
```
###Model 15J: Other IDVs and Competition

Unlike the existance of competition, having more offers does not have the same dramatic relationship with multi-award IDVs that it does with competition. This does raise the question as to whether the long tail of high numbers of offers might be undercutting the relationship. However, for now, we will stick with competition. 


```r
# Term_15J <- glmer (data=Term_smp,
#                  b_Term ~ l_Ceil + l_Days +
#                 SIDV + MIDV + OIDV + SIDV:l_Days +
#                 n_Fixed  + n_UCA +
#                 cb_Comp + cb_Comp:MIDV + cb_Comp:OIDV +
#                (1 | Who) + (1 | PSR_What) + (1 | StartFY), family=binomial(link="logit"))
# display(Term_15J)
```
Competition and Other IDVs only slightly exceeds the standard error.


##Selection: Model 05H

While the direction of competition is still puzzling, the interaction with Multiple-Award clarifies it somewhat. The study team will use model 05A going forward.


```r
# TermModel<-Term_15H
# display(Term_15H)
```
