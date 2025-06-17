---
title: "Defense Acquisition Trends"
output:
  html_document:
    keep_md: yes
    toc: yes
date: "Wednesday, October 6, 2021"
---

# Setup
First we load the data. The dataset used is a U.S. DOD Contracting dataset derived from FPDS.


```
## Warning: replacing previous import 'Hmisc::src' by 'dplyr::src' when loading
## 'csis360'
```

```
## Warning: replacing previous import 'Hmisc::summarize' by 'dplyr::summarize'
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
## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
## ✔ forcats   1.0.0     ✔ stringr   1.5.0
## ✔ lubridate 1.9.3     ✔ tibble    3.2.1
## ✔ purrr     1.0.2     ✔ tidyr     1.3.0
## ✔ readr     2.1.4     
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
## 
## Attaching package: 'scales'
## 
## 
## The following object is masked from 'package:purrr':
## 
##     discard
## 
## 
## The following object is masked from 'package:readr':
## 
##     col_factor
```

# Topline



## Topline Competition

```r
pbl_short %>% group_by(Fiscal_Year) %>% summarise(Action_Obligation_OMB25_GDP23=sum(Action_Obligation_OMB25_GDP23))
```

```
## # A tibble: 27 × 2
##    Fiscal_Year Action_Obligation_OMB25_GDP23
##          <int>                         <dbl>
##  1        1999                     29459651.
##  2        2000                    445523749.
##  3        2001                    848348774.
##  4        2002                    970329856.
##  5        2003                   1746966098.
##  6        2004                   2424386473.
##  7        2005                   2076017042.
##  8        2006                   3175912693.
##  9        2007                   4444299722.
## 10        2008                   5247582346.
## # ℹ 17 more rows
```

```r
pbl_short %>% filter(Fiscal_Year==2021) %>% group_by(Competition.sum) %>% summarise(Action_Obligation_OMB25_GDP23=sum(Action_Obligation_OMB25_GDP23))
```

```
## # A tibble: 5 × 2
##   Competition.sum Action_Obligation_OMB25_GDP23
##   <fct>                                   <dbl>
## 1 1 Offer                             33884379.
## 2 2 Offers                           383250774.
## 3 3+ Offers                           26146807.
## 4 No Comp.                          6522165661.
## 5 Unlabeled                                  0
```

```r
(
ToplineCompMulti<-build_plot(
  data=pbl_short %>% filter(Fiscal_Year>=1991&Fiscal_Year<=2024),
  chart_geom = "Bar Chart",
  share = FALSE,
  labels_and_colors=pbl_lc,
  # NA, #VAR.ncol
  x_var="dFYear",##alpha_var="YTD", #x_var
  y_var="Action_Obligation_OMB25_GDP23", #VAR.y.variable
  color_var="Competition.multisum", #color_var
  # facet_var="Competition.sum", #facet_var
  column_key=pbl_ck,
  format=TRUE,
  ytextposition=FALSE
)+
  # theme(strip.text.y = element_text(angle=0))+
 # theme(axis.text.x = element_text(angle=90))+
 #    scale_y_continuous("Percent of Obligations", labels = percent_format(accuracy=1))+
 #      facet_grid(.~Competition.sum ,scales="free_x", space="free_x"
 #             )+labs(title=NULL,
  # 
  # y="Percent of Obligations",
  # color="Competition")+
  theme(legend.position = "bottom")+
  labs(x="Fiscal Year",
       y="Obligations (Constant 2023 $s)")+
    date_x_year_breaks(1992,2022, by=2)#,partial_year=2024,partial_label="\nOct–Nov")
)
```

```
## Warning: `summarise_()` was deprecated in dplyr 0.7.0.
## ℹ Please use `summarise()` instead.
## ℹ The deprecated feature was likely used in the csis360 package.
##   Please report the issue to the authors.
## This warning is displayed once every 8 hours.
## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
## generated.
```

```
## Warning: `group_by_()` was deprecated in dplyr 0.7.0.
## ℹ Please use `group_by()` instead.
## ℹ See vignette('programming') for more help
## ℹ The deprecated feature was likely used in the csis360 package.
##   Please report the issue to the authors.
## This warning is displayed once every 8 hours.
## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
## generated.
```

```
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
```

```
## Warning in grid.Call(C_stringMetric, as.graphicsAnnot(x$label)): font family
## not found in Windows font database
```

```
## Warning in grid.Call(C_stringMetric, as.graphicsAnnot(x$label)): font family
## not found in Windows font database
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

```
## Warning in grid.Call(C_stringMetric, as.graphicsAnnot(x$label)): font family
## not found in Windows font database
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

```
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, :
## font family not found in Windows font database
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

![](pbl_trends_files/figure-html/ToplineComp-1.png)<!-- -->

```r
(
ToplineComp<-build_plot(
  data=pbl_short %>% filter(Fiscal_Year>=1997&Fiscal_Year<=2024),
  chart_geom = "Bar Chart",
  share = FALSE,
  labels_and_colors=pbl_lc,
  # NA, #VAR.ncol
  x_var="dFYear",##alpha_var="YTD", #x_var
  y_var="Action_Obligation_OMB25_GDP23", #VAR.y.variable
  color_var="Competition.sum", #color_var
  # facet_var="Competition.sum", #facet_var
  column_key=pbl_ck,
  format=TRUE,
  ytextposition=FALSE
)+
  # theme(strip.text.y = element_text(angle=0))+
 # theme(axis.text.x = element_text(angle=90))+
 #    scale_y_continuous("Percent of Obligations", labels = percent_format(accuracy=1))+
 #      facet_grid(.~Competition.sum ,scales="free_x", space="free_x"
 #             )+labs(title=NULL,
  # 
  # y="Percent of Obligations",
  # color="Competition")+
  theme(legend.position = "bottom")+
  labs(x="Fiscal Year",
       y="Obligations (Constant 2023 $s)")+
    date_x_year_breaks(1998,2022,2)#)#,partial_year=2024,partial_label="\nOct–Nov")
)
```

```
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

```
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, :
## font family not found in Windows font database
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

![](pbl_trends_files/figure-html/ToplineComp-2.png)<!-- -->

```r
(
ToplineCompLine<-build_plot(
  data=pbl_short %>% filter(Fiscal_Year>=1997&Fiscal_Year<=2024),
  chart_geom = "Line Chart",
  share = TRUE,
  labels_and_colors=pbl_lc,
  # NA, #VAR.ncol
  x_var="dFYear",##alpha_var="YTD", #x_var
  y_var="Action_Obligation_OMB25_GDP23", #VAR.y.variable
  color_var="Competition.sum", #color_var
  # facet_var="Competition.sum", #facet_var
  column_key=pbl_ck,
  format=TRUE,
  ytextposition=FALSE
)+
  # theme(strip.text.y = element_text(angle=0))+
 # theme(axis.text.x = element_text(angle=90))+
 #    scale_y_continuous("Percent of Obligations", labels = percent_format(accuracy=1))+
 #      facet_grid(.~Competition.sum ,scales="free_x", space="free_x"
 #             )+labs(title=NULL,
  
  # y="Percent of Obligations",
  # color="Competition")+
  theme(legend.position = "bottom")+
  labs(x="Fiscal Year",
       y="Obligations (Constant 2023 $s)")+
    date_x_year_breaks(1998,2022, by=2)
)
```

```
## Warning: `mutate_()` was deprecated in dplyr 0.7.0.
## ℹ Please use `mutate()` instead.
## ℹ See vignette('programming') for more help
## ℹ The deprecated feature was likely used in the csis360 package.
##   Please report the issue to the authors.
## This warning is displayed once every 8 hours.
## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
## generated.
```

```
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

```
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, :
## font family not found in Windows font database
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

![](pbl_trends_files/figure-html/ToplineComp-3.png)<!-- -->

```r
pbl_short %>% filter(Competition.sum =="Unlabeled" & Fiscal_Year>=2019) %>% group_by(CompetitionClassification)%>%
  summarise(Action_Obligation_OMB25_GDP23=sum(Action_Obligation_OMB25_GDP23))
```

```
## # A tibble: 3 × 2
##   CompetitionClassification                  Action_Obligation_OMB25_GDP23
##   <fct>                                                              <dbl>
## 1 Unlabeled: Blank Extent Competed                                      0 
## 2 Unlabeled: Competition; Unlabeled Offers                       57508150.
## 3 Unlabeled: No competition; multiple offers                        -8267.
```

```r
log_plot(plot=ToplineComp, df=pbl_short %>% filter(Fiscal_Year<=2024),
                     filename="acq_fig6_5_competition",xlsx="PBL_Contracts.xlsx",
                     sheet="6-5 Comp",path="../output/PBL/", height=3.5,
                     startRow=1,startCol=12,format=TRUE, excel_formulas = TRUE, excel_share = TRUE,
         include_YTD=FALSE#,var_list=c("Vehicle.AwardTask","Vehicle.sum7"),z
         )
```



### Missing Competition

```r
(
UnlabeledComp<-build_plot(
  data=pbl_short %>% filter(Competition.sum =="Unlabeled"),
  chart_geom = "Bar Chart",
  share = FALSE,
  # labels_and_colors=pbl_lc,
  # NA, #VAR.ncol
  x_var="Fiscal_Year", #x_var
  y_var="Action_Obligation_OMB25_GDP23", #VAR.y.variable
  color_var="CompetitionClassification", #color_var
  facet_var="Vehicle.sum7", #facet_var
  # column_key=pbl_ck,
  format=TRUE,
  ytextposition=FALSE
)+
  # theme(strip.text.y = element_text(angle=0))+
 # theme(axis.text.x = element_text(angle=90))+
 #    scale_y_continuous("Percent of Obligations", labels = percent_format(accuracy=1))+
 #      facet_grid(.~Competition.sum ,scales="free_x", space="free_x"
 #             )+labs(title=NULL,
  # x="Fiscal Year",
  # y="Percent of Obligations",
  # color="Competition")+
  theme(legend.position = "right")+
  labs(y="Obligations (Constant 2023 $s)")
)
```

```
## Warning in add_preassigned_scales(mainplot, labels_and_colors, var =
## color_var): CompetitionClassification not found in labels_and_colors
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

```
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, :
## font family not found in Windows font database
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

![](pbl_trends_files/figure-html/UnlabeledComp-1.png)<!-- -->

```r
pbl_short %>% filter(Competition.sum =="Unlabeled" & Fiscal_Year>=2019) %>% group_by(CompetitionClassification)%>%
  summarise(Action_Obligation_OMB25_GDP23=sum(Action_Obligation_OMB25_GDP23))
```

```
## # A tibble: 3 × 2
##   CompetitionClassification                  Action_Obligation_OMB25_GDP23
##   <fct>                                                              <dbl>
## 1 Unlabeled: Blank Extent Competed                                      0 
## 2 Unlabeled: Competition; Unlabeled Offers                       57508150.
## 3 Unlabeled: No competition; multiple offers                        -8267.
```

```r
ggsave600dpi(path="..//Output//PBL//",filename="unlabeled_comp.png", UnlabeledComp,  
             width=12, height= 6, units="in",size=12, lineheight=1.2
             )



write.csv(file="..//Output//PBL//unlabeled_competition.csv",row.names = FALSE, na = "",
          # ToplineComp$data%>% mutate(dFYear=lubridate::year(dFYear))%>%
            pivot_wider(UnlabeledComp$data,id_cols=c(CompetitionClassification,Vehicle.sum7),
                        names_from=Fiscal_Year,values_from=Action_Obligation_OMB25_GDP23)
          %>% arrange(CompetitionClassification,Vehicle.sum7))
```







## Topline Vehicle

```r
pbl_short %>% filter(Vehicle=="Unlabeled Indefinite Delivery Contract (IDC)" ) %>% group_by(ClassifyNumberOfOffers,No.Competition.sum)%>%
  summarise(n=length(Vehicle))
```

```
## `summarise()` has grouped output by 'ClassifyNumberOfOffers'. You can override
## using the `.groups` argument.
```

```
## # A tibble: 0 × 3
## # Groups:   ClassifyNumberOfOffers [0]
## # ℹ 3 variables: ClassifyNumberOfOffers <fct>, No.Competition.sum <fct>,
## #   n <int>
```

```r
(
ToplineVehicle7<-build_plot(
  data=pbl_short %>% filter(!is.na(Vehicle.AwardTask)),
  chart_geom = "Bar Chart",
  share = FALSE,
  labels_and_colors=pbl_lc,
  # NA, #VAR.ncol
  x_var="dFYear",##alpha_var="YTD", #x_var
  y_var="Action_Obligation_OMB25_GDP23", #VAR.y.variable
  color_var="Vehicle.sum7", #color_var
  facet_var="Vehicle.AwardTask", #facet_var
  column_key=pbl_ck,
  format=TRUE,
  ytextposition=FALSE
)+date_x_year_breaks(2000,2022,2)+#,partial_year=2024,partial_label="\nQ1")   
  # theme(strip.text.y = element_text(angle=0))+
 # theme(axis.text.x = element_text(angle=90))+
 #    scale_y_continuous("Percent of Obligations", labels = percent_format(accuracy=1))+
 #      facet_grid(.~Competition.sum ,scales="free_x", space="free_x"
 #             )+labs(title=NULL,
  # x="Fiscal Year",
  # y="Percent of Obligations",
  # color="Competition")+
  theme(legend.position = "right")+
  labs(y="Obligations (Constant 2023 $s)",
       caption="Note: Unlabeled not shown. Source: FPDS and CSIS analysis.")
)
```

```
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

```
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, :
## font family not found in Windows font database
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

![](pbl_trends_files/figure-html/ToplineVehicle-1.png)<!-- -->

```r
(
ToplineVehicle<-build_plot(
  data=pbl_short %>% filter(!is.na(Vehicle.AwardTask)),
  chart_geom = "Bar Chart",
  share = FALSE,
  labels_and_colors=pbl_lc,
  # NA, #VAR.ncol
  x_var="dFYear",##alpha_var="YTD", #x_var
  y_var="Action_Obligation_OMB25_GDP23", #VAR.y.variable
  color_var="Vehicle.sum", #color_var
  facet_var="Vehicle.AwardTask", #facet_var
  column_key=pbl_ck,
  format=TRUE,
  ytextposition=FALSE
)+date_x_year_breaks(2000,2022,2)+#,partial_year=2024,partial_label="\nQ1")   
  # theme(strip.text.y = element_text(angle=0))+
 # theme(axis.text.x = element_text(angle=90))+
 #    scale_y_continuous("Percent of Obligations", labels = percent_format(accuracy=1))+
 #      facet_grid(.~Competition.sum ,scales="free_x", space="free_x"
 #             )+labs(title=NULL,
  # x="Fiscal Year",
  # y="Percent of Obligations",
  # color="Competition")+
  theme(legend.position = "right")+
  labs(y="Obligations (Constant 2023 $s)",
       caption="Note: Unlabeled not shown. Source: FPDS and CSIS analysis.")
)
```

```
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

```
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, :
## font family not found in Windows font database
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

![](pbl_trends_files/figure-html/ToplineVehicle-2.png)<!-- -->

```r
log_plot(plot=ToplineVehicle7, df=pbl_short %>% filter(Fiscal_Year<=2024),
                     filename="acq_fig4_X_vehicle",xlsx="PBL_Contracts.xlsx",
                     sheet="4-X Veh",path="../output/PBL/", height=4,
                     startRow=1,startCol=14,format=TRUE,
         var_list=c("Vehicle.AwardTask","Vehicle.sum7"))
```


### Vehicle Plat Breakout

```r
platform_levels<-levels(factor(pbl_short$PlatformPortfolio))
for(p in platform_levels){
  file_p<-gsub("__*","_",gsub("&","and",gsub("[ |,]","_",p)))
  (Breakout<-build_plot(
    data=pbl_short %>% filter(Fiscal_Year>=2000&Fiscal_Year<=2024&PlatformPortfolio==p),
  chart_geom = "Bar Chart",
  share = FALSE,
  labels_and_colors=pbl_lc,
  # NA, #VAR.ncol
  x_var="dFYear",##alpha_var="YTD", #x_var
  y_var="Action_Obligation_OMB25_GDP23", #VAR.y.variable
  color_var="Vehicle.sum7", #color_var
  facet_var="Vehicle.AwardTask", #facet_var
  column_key=pbl_ck,
  format=TRUE,
  ytextposition=FALSE
)+date_x_year_breaks(2000,2022,2)+#,partial_year=2024,partial_label="\nQ1")   
  theme(legend.position = "right")+
    labs(y="Obligations (Constant 2023 $s)",title=p))

  if(!dir.exists(file.path("../output/PBL/Platform",file_p)))
    dir.create(file.path("../output/PBL/Platform",file_p))
  undebug(log_plot)
  log_plot(plot=Breakout, df=pbl_short  %>% filter(Fiscal_Year<=2024&PlatformPortfolio==p),
           filename=paste(file_p,"vehicle",sep="_"),xlsx=paste("DoD",file_p,"Contracts.xlsx",sep="_"),
           sheet="Veh",path=file.path("../output/PBL/Platform/",file_p), height=3.5,
           startRow=1,startCol=14,format=TRUE,var_list=c("Vehicle.AwardTask","Vehicle.sum7"),
           output_doc_png=TRUE,excel_y_var=TRUE
           )
  
}
```

```
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
```

```
## Warning in undebug(log_plot): argument is not being debugged
```

```
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
```

```
## Warning in undebug(log_plot): argument is not being debugged
```

```
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
```

```
## Warning in undebug(log_plot): argument is not being debugged
```

```
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
```

```
## Warning in undebug(log_plot): argument is not being debugged
```

```
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
```

```
## Warning in undebug(log_plot): argument is not being debugged
```

```
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
```

```
## Warning in undebug(log_plot): argument is not being debugged
```

```
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
```

```
## Warning in undebug(log_plot): argument is not being debugged
```

```
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
```

```
## Warning in undebug(log_plot): argument is not being debugged
```

```
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
```

```
## Warning in undebug(log_plot): argument is not being debugged
```

```
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
```

```
## Warning in undebug(log_plot): argument is not being debugged
```

```
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
```

```
## Warning in undebug(log_plot): argument is not being debugged
```

### Vehicle Cust Breakout

```r
subcustomer_levels<-levels(factor(pbl_short$SubCustomer.sum))
for(c in subcustomer_levels){
  file_c<-gsub("__*","_",gsub("&","and",gsub("[ |,]","_",c)))
  (Breakout<-build_plot(
    data=pbl_short %>% filter(Fiscal_Year>=2000&SubCustomer.sum==c& Fiscal_Year<=2024),
  chart_geom = "Bar Chart",
  share = FALSE,
  labels_and_colors=pbl_lc,
  # NA, #VAR.ncol
  x_var="dFYear",##alpha_var="YTD", #x_var
  y_var="Action_Obligation_OMB25_GDP23", #VAR.y.variable
  color_var="Vehicle.sum7", #color_var
  facet_var="Vehicle.AwardTask", #facet_var
  column_key=pbl_ck,
  format=TRUE,
  ytextposition=FALSE
)+date_x_year_breaks(2000,2022,2)+#,partial_year=2024,partial_label="\nQ1")   
  theme(legend.position = "right")+
    labs(y="Obligations (Constant 2023 $s)",title=c))
  
  if(!dir.exists(file.path("../output/PBL/Customer",file_c)))
    dir.create(file.path("../output/PBL/Customer",file_c))
  
  log_plot(plot=Breakout, df=pbl_short  %>% filter(Fiscal_Year<=2024&SubCustomer.sum==c),
           filename=paste(file_c,"vehicle",sep="_"),xlsx=paste(file_c,"Contracts.xlsx",sep="_"),
           sheet="Veh",path=file.path("../output/PBL/Customer/",file_c), height=3.5,
           startRow=1,startCol=14,format=TRUE,var_list=c("Vehicle.AwardTask","Vehicle.sum7"),
           output_doc_png=TRUE,excel_y_var=TRUE
           )
  
}
```

```
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
```

### Vehicle Cost 

```r
(
CAS<-build_plot(
  data=pbl_short %>% filter(Fiscal_Year>=2000 & Fiscal_Year<=2024) ,
  chart_geom = "Bar Chart",
  share = FALSE,
  labels_and_colors=pbl_lc,
  # NA, #VAR.ncol
  x_var="dFYear",##alpha_var="YTD", #x_var
  y_var="Action_Obligation_OMB25_GDP23", #VAR.y.variable
  color_var="costaccountingstandardsclause", #color_var
  facet_var="Vehicle.sum7", #facet_var
  column_key=pbl_ck,
  format=TRUE,
  ytextposition=FALSE
)+
  # theme(strip.text.y = element_text(angle=0))+
 # theme(axis.text.x = element_text(angle=90))+
 #    scale_y_continuous("Percent of Obligations", labels = percent_format(accuracy=1))+
 #      facet_grid(.~Competition.sum ,scales="free_x", space="free_x"
 #             )+labs(title=NULL,
  # x="Fiscal Year",
  # y="Percent of Obligations",
  # color="Competition")+
  theme(legend.position = "right")+
  labs(y="Obligations (Constant 2022 $s)")
)
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

```
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, :
## font family not found in Windows font database
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

![](pbl_trends_files/figure-html/VehCost-1.png)<!-- -->
### Vehicle Current Duration 

```r
(
VehCurDur<-build_plot(
  data=pbl_short %>% filter(Fiscal_Year>=2000 & Fiscal_Year<=2024) ,
  chart_geom = "Bar Chart",
  share = FALSE,
  labels_and_colors=pbl_lc,
  # NA, #VAR.ncol
  x_var="dFYear",##alpha_var="YTD", #x_var
  y_var="Action_Obligation_OMB25_GDP23", #VAR.y.variable
  color_var="CurrentDurationIsYear", #color_var
  facet_var="Vehicle.sum7", #facet_var
  column_key=pbl_ck,
  format=TRUE,
  ytextposition=FALSE
)+
  # theme(strip.text.y = element_text(angle=0))+
 # theme(axis.text.x = element_text(angle=90))+
 #    scale_y_continuous("Percent of Obligations", labels = percent_format(accuracy=1))+
 #      facet_grid(.~Competition.sum ,scales="free_x", space="free_x"
 #             )+labs(title=NULL,
  # x="Fiscal Year",
  # y="Percent of Obligations",
  # color="Competition")+
  theme(legend.position = "right")+
  labs(y="Obligations (Constant 2022 $s)")
)
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

```
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, :
## font family not found in Windows font database
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

![](pbl_trends_files/figure-html/VehCurDur-1.png)<!-- -->

```r
(
VehCurDur<-build_plot(
  data=pbl_short %>% filter(Fiscal_Year>=2000 & Fiscal_Year<=2024) ,
  chart_geom = "Bar Chart",
  share = FALSE,
  labels_and_colors=pbl_lc,
  # NA, #VAR.ncol
  x_var="dFYear",##alpha_var="YTD", #x_var
  y_var="Action_Obligation_OMB25_GDP23", #VAR.y.variable
  color_var="CurrentDurationCategory", #color_var
  facet_var="Vehicle.sum7", #facet_var
  column_key=pbl_ck,
  format=TRUE,
  ytextposition=FALSE
)+
  # theme(strip.text.y = element_text(angle=0))+
 # theme(axis.text.x = element_text(angle=90))+
 #    scale_y_continuous("Percent of Obligations", labels = percent_format(accuracy=1))+
 #      facet_grid(.~Competition.sum ,scales="free_x", space="free_x"
 #             )+labs(title=NULL,
  # x="Fiscal Year",
  # y="Percent of Obligations",
  # color="Competition")+
  theme(legend.position = "right")+
  labs(y="Obligations (Constant 2022 $s)")
)
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

```
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, :
## font family not found in Windows font database
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

![](pbl_trends_files/figure-html/VehCurDur-2.png)<!-- -->

```r
log_plot(plot=VehCurDur, df=pbl_short  %>% filter(Fiscal_Year<=2024),
                     filename="Veh_CurDur",xlsx="PBL_Contracts.xlsx",
                     sheet="Veh-CurDur",path="../output/PBL/", height=3.5,
                     format=TRUE,output_slide_png=TRUE #,var_list=c("SimpleArea","Competition.multisum")
         )
```

### Vehicle Ultimate Duration 

```r
(
VehUltDur<-build_plot(
  data=pbl_short %>% filter(Fiscal_Year>=2000 & Fiscal_Year<=2024) ,
  chart_geom = "Bar Chart",
  share = FALSE,
  labels_and_colors=pbl_lc,
  # NA, #VAR.ncol
  x_var="dFYear",##alpha_var="YTD", #x_var
  y_var="Action_Obligation_OMB25_GDP23", #VAR.y.variable
  color_var="UnmodifiedUltimateDurationIsYear", #color_var
  facet_var="Vehicle.sum7", #facet_var
  column_key=pbl_ck,
  format=TRUE,
  ytextposition=FALSE
)+
  # theme(strip.text.y = element_text(angle=0))+
 # theme(axis.text.x = element_text(angle=90))+
 #    scale_y_continuous("Percent of Obligations", labels = percent_format(accuracy=1))+
 #      facet_grid(.~Competition.sum ,scales="free_x", space="free_x"
 #             )+labs(title=NULL,
  # x="Fiscal Year",
  # y="Percent of Obligations",
  # color="Competition")+
  theme(legend.position = "right")+
  labs(y="Obligations (Constant 2022 $s)")
)
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

```
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, :
## font family not found in Windows font database
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

![](pbl_trends_files/figure-html/VehUltDur-1.png)<!-- -->

```r
(
VehUltDur<-build_plot(
  data=pbl_short %>% filter(Fiscal_Year>=2000 & Fiscal_Year<=2024) ,
  chart_geom = "Bar Chart",
  share = FALSE,
  labels_and_colors=pbl_lc,
  # NA, #VAR.ncol
  x_var="dFYear",##alpha_var="YTD", #x_var
  y_var="Action_Obligation_OMB25_GDP23", #VAR.y.variable
  color_var="UnmodifiedUltimateDurationCategory", #color_var
  facet_var="Vehicle.sum7", #facet_var
  column_key=pbl_ck,
  format=TRUE,
  ytextposition=FALSE
)+
  # theme(strip.text.y = element_text(angle=0))+
 # theme(axis.text.x = element_text(angle=90))+
 #    scale_y_continuous("Percent of Obligations", labels = percent_format(accuracy=1))+
 #      facet_grid(.~Competition.sum ,scales="free_x", space="free_x"
 #             )+labs(title=NULL,
  # x="Fiscal Year",
  # y="Percent of Obligations",
  # color="Competition")+
  theme(legend.position = "right")+
  labs(y="Obligations (Constant 2022 $s)")
)
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

```
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, :
## font family not found in Windows font database
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

![](pbl_trends_files/figure-html/VehUltDur-2.png)<!-- -->

```r
log_plot(plot=VehUltDur, df=pbl_short  %>% filter(Fiscal_Year<=2024),
                     filename="Veh_UltDur",xlsx="PBL_Contracts.xlsx",
                     sheet="Veh-UltDur",path="../output/PBL/", height=3.5,
                     format=TRUE,output_slide_png=TRUE #,var_list=c("SimpleArea","Competition.multisum")
         )
```

## Topline Pricing

```r
# pbl_short$PricingUCA.sum<-pbl_short$PricingUCA.sum
# levels(pbl_short$PricingUCA.sum)<-list("Firm Fixed-Price"="FFP",
#                                            "Less Common"="Less Common",
#                                            "Incentive"="Incentive",
#                                            "Other Cost-Based"="Other CB",
#                                            "Undefinitized\nContract Award"="UCA",
#                                            "Unclear"="Unclear"   )
(
ToplinePricing<-build_plot(
  data=pbl_short %>% filter(Fiscal_Year>=2000 & Fiscal_Year<=2024),
  chart_geom = "Bar Chart",
  share = FALSE,
  labels_and_colors=pbl_lc,
  # NA, #VAR.ncol
  x_var="dFYear", #x_var
  y_var="Action_Obligation_OMB25_GDP23", #VAR.y.variable
  color_var="PricingUCA", #color_var
  facet_var="PricingUCA.sum", #facet_var
  column_key=pbl_ck,
  format=TRUE,
  ytextposition=FALSE,
  #alpha_var="YTD",
)+date_x_year_breaks(2000,2023,7)+#,partial_year=2024,partial_label="\nQ1")
  # theme(strip.text.y = element_text(angle=0))+
 # theme(axis.text.x = element_text(angle=90))+
 #    scale_y_continuous("Percent of Obligations", labels = percent_format(accuracy=1))+
 #      facet_grid(.~Competition.sum ,scales="free_x", space="free_x"
 #             )+labs(title=NULL,
  # x="Fiscal Year",
  # y="Percent of Obligations",
  # color="Competition")+
  theme(legend.position = "bottom")+facet_wrap(~PricingUCA.sum,nrow=1)+
  labs(y="Obligations\n(Constant 2023 $s)",
       title="DOD Contract Obligations by Pricing Mechanism, FY 2000–FY 2023",
       caption="Note: CB=Cost-Based, FFP=Firm Fixed Price, FP=Fixed-Price, FPLOE=Fixed-Price Level-of-Effort,\nLH=Labor Hours,T&M=Time & Materials, UCA=Undefinitized Contract Award\nSource: FPDS and CSIS analysis.")
)
```

```
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

```
## Warning in grid.Call(C_stringMetric, as.graphicsAnnot(x$label)): font family
## not found in Windows font database
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

```
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, :
## font family not found in Windows font database

## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, :
## font family not found in Windows font database

## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, :
## font family not found in Windows font database
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

![](pbl_trends_files/figure-html/ToplinePricing-1.png)<!-- -->

```r
ggsave600dpi(path="..//Output//PBL//",filename="topline_pricing.png", ToplinePricing,  
             width=12, height= 6, units="in",size=12, lineheight=1.2
             )


log_plot(plot=ToplinePricing+theme(plot.margin = margin(t=0,r=0.25,b=0.1,l=0.1,"inches")), df=pbl_short  %>% filter(Fiscal_Year<=2024),
                     filename="aila_fig54_pricing",xlsx="PBL_Contracts.xlsx",
                     sheet="4-1 Price",path="../output/PBL/", height=3.5,
         output_slide_png = TRUE,
                     format=TRUE,var_list=c("PricingUCA.sum","PricingUCA"),
         excel_formula=TRUE, excel_y_var = TRUE
         )
```


### Pricing UCA History

```r
(
ToplinePricing<-build_plot(
  data=pbl_short %>% filter(Fiscal_Year>=2000 & Fiscal_Year<2025),
  chart_geom = "Bar Chart",
  share = FALSE,
  labels_and_colors=pbl_lc,
  # NA, #VAR.ncol
  x_var="dFYear", #x_var
  y_var="Action_Obligation_OMB25_GDP23", #VAR.y.variable
  color_var="PricingUCA", #color_var
  facet_var="PricingUCA.sum", #facet_var
  column_key=pbl_ck,
  format=TRUE,
  ytextposition=FALSE,
  # #alpha_var="YTD",
)+date_x_year_breaks(2000,2024,6)+#,partial_year=2024,partial_label="\nQ1")
  # theme(strip.text.y = element_text(angle=0))+
 # theme(axis.text.x = element_text(angle=90))+
 #    scale_y_continuous("Percent of Obligations", labels = percent_format(accuracy=1))+
 #      facet_grid(.~Competition.sum ,scales="free_x", space="free_x"
 #             )+labs(title=NULL,
  # x="Fiscal Year",
  # y="Percent of Obligations",
  # color="Competition")+
  theme(legend.position = "bottom")+facet_wrap(~PricingUCA.sum,nrow=1)+
  labs(y="Obligations\n(Constant 2023 $s)",
       title="DOD Contract Obligations by Pricing Mechanism, FY 2000–FY 2024",
       caption="Note: CB=Cost-Based, FFP=Firm Fixed Price, FP=Fixed-Price, FPLOE=Fixed-Price Level-of-Effort,\nLH=Labor Hours,T&M=Time & Materials, UCA=Undefinitized Contract Award\nSource: FPDS and CSIS analysis.")
)
```

```
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

```
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, :
## font family not found in Windows font database

## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, :
## font family not found in Windows font database

## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, :
## font family not found in Windows font database
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

![](pbl_trends_files/figure-html/ToplinePricingUCA-1.png)<!-- -->

```r
  ggsave600dpi(path="..//Output//PBL//",filename="Topline_PricingUCA.png", ToplinePricing,  
               width=6.5, height= 3, units="in",size=12, lineheight=1.2
               )
  
  
  ggsave600dpi(path="..//Output//PBL//",filename="Topline_PricingUCA.svg", ToplinePricing,  
               width=12, height= 5.5, units="in",size=, lineheight=1.2
               )



(
PricingHistory<-build_plot(
  data=pbl_short %>% filter(Fiscal_Year < 2025 & Fiscal_Year>=2000),
  chart_geom = "Line Chart",
  share = TRUE,
  labels_and_colors=pbl_lc,
  # NA, #VAR.ncol
  x_var="dFYear",##alpha_var="YTD", #x_var
  y_var="Action_Obligation_OMB25_GDP23", #VAR.y.variable
  color_var="PricingUCA.sum", #color_var
  # facet_var="PricingUCA.sum", #facet_var
  column_key=pbl_ck,
  format=TRUE,
  ytextposition=FALSE
)+#+date_x_year_breaks(2000,2022,2)+#,partial_year=2024,partial_label="\nQ1")   
  # theme(strip.text.y = element_text(angle=0))+
 # theme(axis.text.x = element_text(angle=90))+
 #    scale_y_continuous("Percent of Obligations", labels = percent_format(accuracy=1))+
 #      facet_grid(.~Competition.sum ,scales="free_x", space="free_x"
 #             )+labs(title=NULL,
  # x="Fiscal Year",
  # y="Percent of Obligations",
  # color="Competition")+
  theme(legend.position = "right",
        plot.caption = element_text(hjust = 0,face="plain"))+
    date_x_year_breaks(2000,2024,6)+#,partial_year=2024,partial_label="\nQ1")
    # xlim(c(as.Date("1979-01-01"), as.Date("2023-12-30")))+
    # coord_cartesian(xlim = c(as.Date("1980-01-01"), as.Date("2022-01-01"))
  labs(y="Share of Obligations",caption="Source: Federal Procurement Data System (FPDS) and CSIS analysis.")
)
```

```
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

```
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, :
## font family not found in Windows font database
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

![](pbl_trends_files/figure-html/ToplinePricingUCA-2.png)<!-- -->

```r
log_plot(plot=PricingHistory, df=pbl_short  %>% filter(Fiscal_Year<2025&Fiscal_Year>=2000),
                     filename="acq_fig8_2_pricing_uca",xlsx="PBL_Contracts.xlsx",
                     sheet="8-2 PriceUCA",path="../output/PBL/", height=3.5,
                     format=TRUE,#,var_list=c("PricingInflation",
         excel_y_var =TRUE,
         excel_share =TRUE
         )
```


### Pricing History

```r
(
PricingHistory<-build_plot(
  data=pbl_short %>% filter(Fiscal_Year < 2025),
  chart_geom = "Line Chart",
  share = TRUE,
  labels_and_colors=pbl_lc,
  # NA, #VAR.ncol
  x_var="dFYear",##alpha_var="YTD", #x_var
  y_var="Action_Obligation_OMB25_GDP23", #VAR.y.variable
  color_var="PricingInflation", #color_var
  # facet_var="PricingUCA.sum", #facet_var
  column_key=pbl_ck,
  format=TRUE,
  ytextposition=FALSE
)+#+date_x_year_breaks(2000,2022,2)+#,partial_year=2024,partial_label="\nQ1")   
  # theme(strip.text.y = element_text(angle=0))+
 # theme(axis.text.x = element_text(angle=90))+
 #    scale_y_continuous("Percent of Obligations", labels = percent_format(accuracy=1))+
 #      facet_grid(.~Competition.sum ,scales="free_x", space="free_x"
 #             )+labs(title=NULL,
  # x="Fiscal Year",
  # y="Percent of Obligations",
  # color="Competition")+
  theme(legend.position = "right",
        plot.caption = element_text(hjust = 0,face="plain"))+
    date_x_year_breaks(1980,2022,5)+#,partial_year=2024,partial_label="\nQ1")
    # xlim(c(as.Date("1979-01-01"), as.Date("2023-12-30")))+
    # coord_cartesian(xlim = c(as.Date("1980-01-01"), as.Date("2022-01-01"))
  labs(y="Share of Obligations",caption="Source: Federal Procurement Data System (FPDS) and CSIS analysis.")
)
```

```
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

```
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, :
## font family not found in Windows font database
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

![](pbl_trends_files/figure-html/ToplinePricingHistory-1.png)<!-- -->

```r
log_plot(plot=PricingHistory, df=pbl_short  %>% filter(Fiscal_Year<=2024),
                     filename="acq_fig8_2_pricing_history",xlsx="PBL_Contracts.xlsx",
                     sheet="8-2 PriceHist",path="../output/PBL/", height=3.5,
                     format=TRUE#,var_list=c("PricingInflation")
         )
```



### Pricing Plat Breakout

```r
platform_levels<-levels(factor(pbl_short$PlatformPortfolio))

for(p in platform_levels){
  file_p<-gsub("__*","_",gsub("&","and",gsub("[ |,]","_",p)))
  (Breakout<-build_plot(
    data=pbl_short %>% filter(Fiscal_Year>=2000&Fiscal_Year<=2024&PlatformPortfolio==p),
    chart_geom = "Bar Chart",
    share = FALSE,
    labels_and_colors=pbl_lc,
    # NA, #VAR.ncol
    x_var="dFYear", #x_var
    y_var="Action_Obligation_OMB25_GDP23", #VAR.y.variable
    color_var="PricingUCA", #color_var
    facet_var="PricingUCA.sum", #facet_var
    column_key=pbl_ck,
    format=TRUE,
    ytextposition=FALSE,
    #alpha_var="YTD",
  )+date_x_year_breaks(2000,2023,6)+#,partial_year=2024,partial_label="\nQ1")
    theme(legend.position = "right")+
    labs(y="Obligations (Constant 2023 $s)",title=p))

  if(!dir.exists(file.path("../output/PBL/Platform",file_p)))
    dir.create(file.path("../output/PBL/Platform",file_p))
  undebug(log_plot)
  log_plot(plot=Breakout, df=pbl_short  %>% filter(Fiscal_Year<=2024&PlatformPortfolio==p),
           filename=paste(file_p,"pricing",sep="_"),xlsx=paste("DoD",file_p,"Contracts.xlsx",sep="_"),
           sheet="Price",path=file.path("../output/PBL/Platform/",file_p), height=3.5,
           startRow=1,startCol=14,format=TRUE,var_list=c("PricingUCA.sum","PricingUCA"),
           output_doc_png=TRUE,excel_y_var=TRUE
           )
  
}
```

```
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
```

```
## Warning in undebug(log_plot): argument is not being debugged
```

```
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
```

```
## Warning in undebug(log_plot): argument is not being debugged
```

```
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
```

```
## Warning in undebug(log_plot): argument is not being debugged
```

```
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
```

```
## Warning in undebug(log_plot): argument is not being debugged
```

```
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
```

```
## Warning in undebug(log_plot): argument is not being debugged
```

```
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
```

```
## Warning in undebug(log_plot): argument is not being debugged
```

```
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
```

```
## Warning in undebug(log_plot): argument is not being debugged
```

```
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
```

```
## Warning in undebug(log_plot): argument is not being debugged
```

```
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
```

```
## Warning in undebug(log_plot): argument is not being debugged
```

```
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
```

```
## Warning in undebug(log_plot): argument is not being debugged
```

```
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
```

```
## Warning in undebug(log_plot): argument is not being debugged
```

### Pricing Cust Breakout

```r
subcustomer_levels<-levels(factor(pbl_short$SubCustomer.sum))
for(c in subcustomer_levels){
  file_c<-gsub("__*","_",gsub("&","and",gsub("[ |,]","_",c)))
  (Breakout<-build_plot(
    data=pbl_short %>% filter(Fiscal_Year>=2000&SubCustomer.sum==c& Fiscal_Year<=2024),
    chart_geom = "Bar Chart",
    share = FALSE,
    labels_and_colors=pbl_lc,
    # NA, #VAR.ncol
    x_var="dFYear", #x_var
    y_var="Action_Obligation_OMB25_GDP23", #VAR.y.variable
    color_var="PricingUCA", #color_var
    facet_var="PricingUCA.sum", #facet_var
    column_key=pbl_ck,
    format=TRUE,
    ytextposition=FALSE,
    #alpha_var="YTD",
  )+date_x_year_breaks(2000,2023,6)+#,partial_year=2024,partial_label="\nQ1")
    theme(legend.position = "right")+
    labs(y="Obligations (Constant 2023 $s)",title=c))
  
  if(!dir.exists(file.path("../output/PBL/Customer",file_c)))
    dir.create(file.path("../output/PBL/Customer",file_c))
  
  log_plot(plot=Breakout, df=pbl_short  %>% filter(Fiscal_Year<=2024&SubCustomer.sum==c),
           filename=paste(file_c,"pricing",sep="_"),xlsx=paste(file_c,"Contracts.xlsx",sep="_"),
           sheet="Price",path=file.path("../output/PBL/Customer/",file_c), height=3.5,
           startRow=1,startCol=14,format=TRUE,var_list=c("PricingUCA.sum","PricingUCA"),
           output_doc_png=TRUE,excel_y_var=TRUE
           )
  
}
```

```
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
```

## Topline Commercial

```r
pbl_short$AnyCommercialText<-factor(pbl_short$AnyCommercial)
levels(pbl_short$AnyCommercialText)<-list(
  "Not Classified\nas Commercial"="N",
  "Non-development\nor Commercial Similar"= "NonDev",
  "Any Commercial\nClassification"="Y"
  
)

(
Commercial<-build_plot(
  data=pbl_short %>% filter(Fiscal_Year>=2000 & Fiscal_Year<=2024),
  chart_geom = "Bar Chart",
  share = FALSE,
  labels_and_colors=pbl_lc,
  # NA, #VAR.ncol
  x_var="dFYear",##alpha_var="YTD", #x_var
  y_var="Action_Obligation_OMB25_GDP23", #VAR.y.variable
  color_var="AnyCommercialText", #color_var
  # facet_var="SubCustomer.platform", #facet_var
  column_key=pbl_ck,
  format=TRUE,
  ytextposition=FALSE
)+
  # theme(strip.text.y = element_text(angle=0))+
 # theme(axis.text.x = element_text(angle=90))+
 #    scale_y_continuous("Percent of Obligations", labels = percent_format(accuracy=1))+
 #      facet_grid(.~Competition.sum ,scales="free_x", space="free_x"
 #             )+labs(title=NULL,
  # x="Fiscal Year",
  # y="Percent of Obligations",
  # color="Competition")+
  theme(legend.position = "right")+
  labs(y="Obligations (Constant 2023 $s)")+
  date_x_year_breaks(2000,2022,2)#)#,partial_year=2024,partial_label="\nOct–Nov")
)
```

```
## Warning in format_data_for_plot(data = data, share = share, fy_var = x_var, :
## color_var missing from labels_and_colors
```

```
## Warning in add_preassigned_scales(mainplot, labels_and_colors, var =
## color_var): AnyCommercialText not found in labels_and_colors
```

```
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

```
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, :
## font family not found in Windows font database
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

![](pbl_trends_files/figure-html/Commercial-1.png)<!-- -->

```r
log_plot(plot=Commercial+labs(y="Obligations\n(Constant 2023 $s)"), df=pbl_short %>% filter(Fiscal_Year<=2024),
                     filename="acq_fig5_4_commercial",xlsx="PBL_Contracts.xlsx",
                     sheet="5-4 Comm",path="../output/PBL/", height=2.5,
                     startRow=1,startCol=12,format=TRUE,#var_list=c("SimpleArea","Competition.multisum")
         )
```

## Topline Duration

```r
pbl_short$CurrentDurationIsYear<-factor(pbl_short$CurrentDurationCategory)
levels(pbl_short$CurrentDurationIsYear)<-list(
   "<=1 year" =c("<=2 Months", ">2-7 Months"  ,">7-12 Months"),
   ">1 year"=c(">1-2 Years",   ">2-4 Years",  ">4 years")
)
pbl_short$UnmodifiedUltimateDurationIsYear<-factor(pbl_short$UnmodifiedUltimateDurationCategory)
levels(pbl_short$UnmodifiedUltimateDurationIsYear)<-list(
   "<=1 year" =c("<=2 Months", ">2-7 Months"  ,">7-12 Months"),
   ">1 year"=c(">1-2 Years",   ">2-4 Years",  ">4 years")
)

(
CurDuration<-build_plot(
  data=pbl_short %>% filter(Fiscal_Year>=2000 & Fiscal_Year<=2024),
  chart_geom = "Bar Chart",
  share = TRUE,
  labels_and_colors=pbl_lc,
  # NA, #VAR.ncol
  x_var="dFYear",##alpha_var="YTD", #x_var
  y_var="Action_Obligation_OMB25_GDP23", #VAR.y.variable
  color_var="CurrentDurationCategory", #color_var
  # facet_var="SubCustomer.platform", #facet_var
  column_key=pbl_ck,
  format=TRUE,
  ytextposition=FALSE
)+
  # theme(strip.text.y = element_text(angle=0))+
 # theme(axis.text.x = element_text(angle=90))+
 #    scale_y_continuous("Percent of Obligations", labels = percent_format(accuracy=1))+
 #      facet_grid(.~Competition.sum ,scales="free_x", space="free_x"
 #             )+labs(title=NULL,
  # x="Fiscal Year",
  # y="Percent of Obligations",
  # color="Competition")+
  theme(legend.position = "right")+
  labs(y="Share of Obligations")+
     date_x_year_breaks(2007,2023,2)#)#,partial_year=2024,partial_label="\nOct–Nov")

)
```

```
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

```
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, :
## font family not found in Windows font database
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

![](pbl_trends_files/figure-html/Duration-1.png)<!-- -->

```r
(
UnmDuration<-build_plot(
  data=pbl_short %>% filter(Fiscal_Year>=2007 & Fiscal_Year<=2024),
  chart_geom = "Bar Chart",
  share = TRUE,
  labels_and_colors=pbl_lc,
  # NA, #VAR.ncol
  x_var="dFYear",##alpha_var="YTD", #x_var
  y_var="Action_Obligation_OMB25_GDP23", #VAR.y.variable
  color_var="UnmodifiedUltimateDurationCategory", #color_var
  # facet_var="SubCustomer.platform", #facet_var
  column_key=pbl_ck,
  format=TRUE,
  ytextposition=FALSE
)+
  # theme(strip.text.y = element_text(angle=0))+
 # theme(axis.text.x = element_text(angle=90))+
 #    scale_y_continuous("Percent of Obligations", labels = percent_format(accuracy=1))+
 #      facet_grid(.~Competition.sum ,scales="free_x", space="free_x"
 #             )+labs(title=NULL,
  # x="Fiscal Year",
  # y="Percent of Obligations",
  # color="Competition")+
  theme(legend.position = "right")+
  labs(y="Share of Obligations",
       title="Market Share of DOD Contracts by Initial Award\nor Task Order Ultimate Duration, FY 2007-FY 2023")+
      date_x_year_breaks(2007,2023,2)#)#,partial_year=2024,partial_label="\nOct–Nov")

)
```

```
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

```
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, :
## font family not found in Windows font database
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

![](pbl_trends_files/figure-html/Duration-2.png)<!-- -->

```r
log_plot(plot=UnmDuration, df=pbl_short  %>% filter(Fiscal_Year<=2024),
                     filename="acq_fig8_3_UmdDuration",xlsx="PBL_Contracts.xlsx",
                     sheet="8-3 Dur",path="../output/PBL/", height=3.5,
                     startRow=1,startCol=12,format=TRUE,
         output_slide_png = TRUE, suppress_doc_svg_text = "title"
         #,var_list=c("PricingInflation"),
         )


log_plot(plot=UnmDuration, df=pbl_short  %>% filter(Fiscal_Year<=2024),
                     filename="nps_fig06_UmdDuration",xlsx="DoD_2024_NPS.xlsx",
                     sheet="9 Dur",path="../output/PBL/",
         height=2.75,include_YTD=FALSE,suppress_doc_svg_text = "title",
                     startRow=1,format=TRUE,excel_y_var=TRUE,excel_formulas = TRUE,
         output_slide_png=TRUE
         )
```

## Topline Multi-Year
### Multi-Year setup

```r
    pbl_short<-pbl_short%>% mutate(multiyearcontracttext=factor(multiyearcontract,levels=c("0","1"),
                              labels=c("Not Multi-Year","Multi-Year Contract")))

pbl_short$multiyearcontracttext[is.na(pbl_short$multiyearcontract)&
                             pbl_short$Vehicle %in% c("BOA","BPA","FSS","GWAC","PO","DO") ]<-"Not Multi-Year"
  # "MULTIPLE AWARD IDC"  "SINGLE AWARD IDC"   "Unlabeled IDC"        "DCA"    "DO"   


# pbl_short %>% group_by(Vehicle) %>%
#   dplyr::summarise(multiyearcontracttext=sum(
#     ifelse(multiyearcontracttext=="Multi-Year Contract",Action_Obligation_OMB25_GDP23,0),na.rm=TRUE))



pbl_short<-pbl_short %>% group_by(Fiscal_Year,Vehicle.sum7)%>%
  mutate(pPlatFYear=Action_Obligation_OMB25_GDP23/sum(Action_Obligation_OMB25_GDP23,na.rm=TRUE))
```
### Topline Multi-year



```r
pbl_short<-pbl_short%>% mutate(multiyearcontracttext=factor(multiyearcontract,levels=c("0","1"),
                              labels=c("Not Multi-Year","Multi-Year Contract")))


(
MultiYear<-build_plot(
  data=pbl_short %>% filter(Fiscal_Year>=2000 & Fiscal_Year<=2024),
  chart_geom = "Bar Chart",
  share = FALSE,
  labels_and_colors=pbl_lc,
  # NA, #VAR.ncol
  x_var="dFYear",##alpha_var="YTD", #x_var
  y_var="Action_Obligation_OMB25_GDP23", #VAR.y.variable
  color_var="multiyearcontract", #color_var
  # facet_var="SubCustomer.platform", #facet_var
  column_key=pbl_ck,
  format=TRUE,
  ytextposition=FALSE
)+
  theme(legend.position = "right")+
  labs(y="Obligations (Constant 2023 $s)")+
  date_x_year_breaks(2000,2022,2)#)#,partial_year=2024,partial_label="\nOct–Nov")
)
```

```
## Warning in format_data_for_plot(data = data, share = share, fy_var = x_var, :
## color_var missing from labels_and_colors
```

```
## Warning in add_preassigned_scales(mainplot, labels_and_colors, var =
## color_var): multiyearcontract not found in labels_and_colors
```

```
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

```
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, :
## font family not found in Windows font database
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

![](pbl_trends_files/figure-html/Multi-Year-1.png)<!-- -->

```r
ggsave600dpi(path="..//Output//PBL//",filename="topline_Multiyear.png", MultiYear, 
             width=11, height= 5.5, units="in",size=14,lineheight = 1.2
             )

ggsave600dpi(path="..//Output//PBL//",filename="topline_Multiyear.svg", MultiYear+
  labs(y="Obligations\n(Constant 2023 $s)"),#+facet_wrap(~SubCustomer.platform,nrow=1), 
             width=6.5, height= 2.25, units="in",size=11,lineheight = 1.2
             )


# pbl_short %>% filter(SubCustomer.sum=="Army", PlatformPortfolio=="Other Products", Fiscal_Year==2021) %>%
#   group_by(AnyMultiyearText,Competition.multisum) %>% summarise(
#     COVID19obligated=sum(COVID19obligated,na.rm=TRUE),
#     Action_Obligation_OMB25_GDP23=sum(Action_Obligation_OMB25_GDP23),
#     Action_Obligation_Then_Year=sum(Action_Obligation_Then_Year),
#     
#   )
```

## Topline GFE

```r
(
GFE<-build_plot(
  data=pbl_short %>% filter(Fiscal_Year>=2000 & Fiscal_Year<=2024),
  chart_geom = "Bar Chart",
  share = FALSE,
  labels_and_colors=pbl_lc,
  # NA, #VAR.ncol
  x_var="dFYear",##alpha_var="YTD", #x_var
  y_var="Action_Obligation_OMB25_GDP23", #VAR.y.variable
  color_var="gfe_gfp_value", #color_var
  # facet_var="SubCustomer.platform", #facet_var
  column_key=pbl_ck,
  format=TRUE,
  ytextposition=FALSE
)+
  # theme(strip.text.y = element_text(angle=0))+
 # theme(axis.text.x = element_text(angle=90))+
 #    scale_y_continuous("Percent of Obligations", labels = percent_format(accuracy=1))+
 #      facet_grid(.~Competition.sum ,scales="free_x", space="free_x"
 #             )+labs(title=NULL,
  # x="Fiscal Year",
  # y="Percent of Obligations",
  # color="Competition")+
  theme(legend.position = "right")+
  labs(y="Obligations (Constant 2023 $s)")+
  date_x_year_breaks(2000,2022,2)#)#,partial_year=2024,partial_label="\nOct–Nov")
)
```

```
## Warning in format_data_for_plot(data = data, share = share, fy_var = x_var, :
## color_var missing from labels_and_colors
```

```
## Warning in add_preassigned_scales(mainplot, labels_and_colors, var =
## color_var): gfe_gfp_value not found in labels_and_colors
```

```
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

```
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, :
## font family not found in Windows font database
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

![](pbl_trends_files/figure-html/GFE-1.png)<!-- -->

```r
log_plot(plot=GFE+labs(y="Obligations\n(Constant 2023 $s)"), df=pbl_short %>% filter(Fiscal_Year<=2024),
                     filename="acq_fig5_4_GFE",xlsx="PBL_Contracts.xlsx",
                     sheet="5-4 GFE",path="../output/PBL/", height=2.5,
                     startRow=1,startCol=12,format=TRUE,#var_list=c("SimpleArea","Competition.multisum")
         )
```

## Topline Cost Accounting

```r
(
CAS<-build_plot(
  data=pbl_short %>% filter(Fiscal_Year>=2001 & Fiscal_Year<=2024) ,
  chart_geom = "Bar Chart",
  share = FALSE,
  labels_and_colors=pbl_lc,
  # NA, #VAR.ncol
  x_var="dFYear",##alpha_var="YTD", #x_var
  y_var="Action_Obligation_OMB25_GDP23", #VAR.y.variable
  color_var="costaccountingstandardsclause", #color_var
  facet_var="SubCustomer.platform", #facet_var
  column_key=pbl_ck,
  format=TRUE,
  ytextposition=FALSE
)+
  # theme(strip.text.y = element_text(angle=0))+
 # theme(axis.text.x = element_text(angle=90))+
 #    scale_y_continuous("Percent of Obligations", labels = percent_format(accuracy=1))+
 #      facet_grid(.~Competition.sum ,scales="free_x", space="free_x"
 #             )+labs(title=NULL,
  # x="Fiscal Year",
  # y="Percent of Obligations",
  # color="Competition")+
  theme(legend.position = "right")+
  labs(y="Obligations (Constant 2023 $s)")
)
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

```
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, :
## font family not found in Windows font database
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

![](pbl_trends_files/figure-html/ToplineCAS-1.png)<!-- -->

```r
(
CAS<-build_plot(
  data=pbl_short %>% filter(Fiscal_Year>=2007 & Fiscal_Year<=2024) ,
  chart_geom = "Bar Chart",
  share = FALSE,
  labels_and_colors=pbl_lc,
  # NA, #VAR.ncol
  x_var="dFYear",##alpha_var="YTD", #x_var
  y_var="Action_Obligation_OMB25_GDP23", #VAR.y.variable
  color_var="costaccountingstandardsclause", #color_var
  facet_var="PlatformPortfolio", #facet_var
  column_key=pbl_ck,
  format=TRUE,
  ytextposition=FALSE
)+
  # theme(strip.text.y = element_text(angle=0))+
 # theme(axis.text.x = element_text(angle=90))+
 #    scale_y_continuous("Percent of Obligations", labels = percent_format(accuracy=1))+
 #      facet_grid(.~Competition.sum ,scales="free_x", space="free_x"
 #             )+labs(title=NULL,
  # x="Fiscal Year",
  # y="Percent of Obligations",
  # color="Competition")+
  theme(legend.position = "right")+
  labs(y="Obligations (Constant 2023 $s)")
)
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

```
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, :
## font family not found in Windows font database
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

![](pbl_trends_files/figure-html/ToplineCAS-2.png)<!-- -->

```r
(
CAS<-build_plot(
  data=pbl_short %>% filter(Fiscal_Year>=2007 & Fiscal_Year<=2024) ,
  chart_geom = "Bar Chart",
  share = FALSE,
  labels_and_colors=pbl_lc,
  # NA, #VAR.ncol
  x_var="dFYear",##alpha_var="YTD", #x_var
  y_var="Action_Obligation_OMB25_GDP23", #VAR.y.variable
  color_var="Competition.sum", #color_var
  facet_var="costaccountingstandardsclause", #facet_var
  column_key=pbl_ck,
  format=TRUE,
  ytextposition=FALSE
)+
  # theme(strip.text.y = element_text(angle=0))+
 # theme(axis.text.x = element_text(angle=90))+
 #    scale_y_continuous("Percent of Obligations", labels = percent_format(accuracy=1))+
 #      facet_grid(.~Competition.sum ,scales="free_x", space="free_x"
 #             )+labs(title=NULL,
  # x="Fiscal Year",
  # y="Percent of Obligations",
  # color="Competition")+
  theme(legend.position = "right")+
  labs(y="Obligations (Constant 2023 $s)")
)
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

```
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, :
## font family not found in Windows font database
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

![](pbl_trends_files/figure-html/ToplineCAS-3.png)<!-- -->

```r
(
CAS<-build_plot(
  data=pbl_short %>% filter(Fiscal_Year>=2007 & Fiscal_Year<=2024) ,
  chart_geom = "Bar Chart",
  share = FALSE,
  labels_and_colors=pbl_lc,
  # NA, #VAR.ncol
  x_var="dFYear",##alpha_var="YTD", #x_var
  y_var="Action_Obligation_OMB25_GDP23", #VAR.y.variable
  color_var="AnyCommercial", #color_var
  facet_var="costaccountingstandardsclause", #facet_var
  column_key=pbl_ck,
  format=TRUE,
  ytextposition=FALSE
)+
  # theme(strip.text.y = element_text(angle=0))+
 # theme(axis.text.x = element_text(angle=90))+
 #    scale_y_continuous("Percent of Obligations", labels = percent_format(accuracy=1))+
 #      facet_grid(.~Competition.sum ,scales="free_x", space="free_x"
 #             )+labs(title=NULL,
  # x="Fiscal Year",
  # y="Percent of Obligations",
  # color="Competition")+
  theme(legend.position = "right")+
  labs(y="Obligations (Constant 2023 $s)")
)
```

```
## Warning in format_data_for_plot(data = data, share = share, fy_var = x_var, :
## color_var missing from labels_and_colors
```

```
## Warning in add_preassigned_scales(mainplot, labels_and_colors, var =
## color_var): AnyCommercial not found in labels_and_colors
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

```
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, :
## font family not found in Windows font database
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

![](pbl_trends_files/figure-html/ToplineCAS-4.png)<!-- -->


#Product/Service/R&D

## Topline Product/Service/R&D

```r
(
ToplinePSR<-build_plot(
  data=pbl_short %>% filter(Fiscal_Year<=2024 & !is.na(SimpleArea) & SimpleArea!="Unlabeled"),
  chart_geom = "Bar Chart",
  share = FALSE,
  labels_and_colors=pbl_lc,
  # NA, #VAR.ncol
  x_var="dFYear", #x_var
  # #alpha_var="YTD",
  y_var="Action_Obligation_OMB25_GDP23", #VAR.y.variable
  color_var="SimpleArea", #color_var
  # facet_var="Competition.sum", #facet_var
  column_key=pbl_ck,
  format=TRUE,
  ytextposition=FALSE
)+
  theme(legend.position = "right")+
  labs(y="Obligations (Constant 2023 $s)",
       caption="Note: Unlabeled product, service, and R&D areas not shown.\nSource: FPDS and CSIS analysis.")+
  date_x_year_breaks(1990,2020,5)#,partial_year=2024,partial_label="\nQ1",)
)
```

```
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

```
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, :
## font family not found in Windows font database
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

![](pbl_trends_files/figure-html/ToplinePSR-1.png)<!-- -->

```r
log_plot(plot=ToplinePSR+facet_wrap(~SimpleArea), df=pbl_short %>% filter(Fiscal_Year<=2024),
                     filename="acq_fig3_1_area",xlsx="PBL_Contracts.xlsx",
                     sheet="3-1 Area",path="../output/PBL/", height=3.5,
                     startRow=1,startCol=12,format=TRUE,#var_list=c("SimpleArea","Competition.multisum"),
         excel_y_var = TRUE,excel_formulas = TRUE
         )
```


## PSR Plat Breakout

```r
platform_levels<-levels(factor(pbl_short$PlatformPortfolio))
for(p in platform_levels){
  file_p<-gsub("__*","_",gsub("&","and",gsub("[ |,]","_",p)))
  (Breakout<-build_plot(
    data=pbl_short %>% filter(Fiscal_Year>=2000&PlatformPortfolio==p& Fiscal_Year<=2024),
  chart_geom = "Bar Chart",
  share = FALSE,
  labels_and_colors=pbl_lc,
  # NA, #VAR.ncol
  x_var="dFYear", #x_var
  #alpha_var="YTD",
  y_var="Action_Obligation_OMB25_GDP23", #VAR.y.variable
  color_var="SimpleArea", #color_var
  # facet_var="Competition.sum", #facet_var
  column_key=pbl_ck,
  format=TRUE,
  ytextposition=FALSE
)+
  theme(legend.position = "right")+
  labs(y="Obligations (Constant 2023 $s)",title=p)+
  date_x_year_breaks(1990,2020,5)#,partial_year=2024,partial_label="\nQ1")
)

  if(!dir.exists(file.path("../output/PBL/Platform",file_p)))
    dir.create(file.path("../output/PBL/Platform",file_p))
  
  log_plot(plot=Breakout, df=pbl_short  %>% filter(Fiscal_Year<=2024&PlatformPortfolio==p),
           filename=paste(file_p,"PSR",sep="_"),xlsx=paste("DoD",file_p,"Contracts.xlsx",sep="_"),
           sheet="PSR",path=file.path("../output/PBL/Platform/",file_p), height=3.5,
           startRow=1,startCol=13,format=TRUE,#,var_list=c("PricingUCA.sum","PricingUCA"),
           output_doc_png=TRUE,excel_y_var=TRUE
           )
  
}
```

```
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
```

## PSR Cust Breakout

```r
subcustomer_levels<-levels(factor(pbl_short$SubCustomer.sum))
for(c in subcustomer_levels){
  file_c<-gsub("__*","_",gsub("&","and",gsub("[ |,]","_",c)))
  (Breakout<-build_plot(
    data=pbl_short %>% filter(Fiscal_Year>=2000&SubCustomer.sum==c& Fiscal_Year<=2024),
  chart_geom = "Bar Chart",
  share = FALSE,
  labels_and_colors=pbl_lc,
  # NA, #VAR.ncol
  x_var="dFYear", #x_var
  #alpha_var="YTD",
  y_var="Action_Obligation_OMB25_GDP23", #VAR.y.variable
  color_var="SimpleArea", #color_var
  # facet_var="Competition.sum", #facet_var
  column_key=pbl_ck,
  format=TRUE,
  ytextposition=FALSE
)+
  theme(legend.position = "right")+
  labs(y="Obligations (Constant 2023 $s)",title=c)+
  date_x_year_breaks(1990,2020,5)#,partial_year=2024,partial_label="\nQ1")
)

  if(!dir.exists(file.path("../output/PBL/Customer",file_c)))
    dir.create(file.path("../output/PBL/Customer",file_c))
  
  log_plot(plot=Breakout, df=pbl_short  %>% filter(Fiscal_Year<=2024&SubCustomer.sum==c),
           filename=paste(file_c,"PSR",sep="_"),xlsx=paste(file_c,"Contracts.xlsx",sep="_"),
           sheet="PSR",path=file.path("../output/PBL/Customer/",file_c), height=3.5,
           startRow=1,startCol=13,format=TRUE,#,var_list=c("PricingUCA.sum","PricingUCA"),
           output_doc_png=TRUE,excel_y_var=TRUE
           )
  
}
```

```
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
```

## Services

```r
summary(factor(pbl_short$ServicesCategory.sum))
```

```
## Warning: Unknown or uninitialised column: `ServicesCategory.sum`.
```

```
## integer(0)
```

```r
(
Services<-build_plot(
  data=pbl_short %>% filter(SimpleArea=="Services" & Fiscal_Year<=2024),
  chart_geom = "Bar Chart",
  share = FALSE,
  labels_and_colors=pbl_lc,
  # NA, #VAR.ncol
  x_var="dFYear", #x_var
  #alpha_var="YTD", #x_var
  y_var="Action_Obligation_OMB25_GDP23", #VAR.y.variable
  color_var="ProductServiceOrRnDarea", #color_var
  # facet_var="Competition.sum", #facet_var
  column_key=pbl_ck,
  format=TRUE,
  ytextposition=FALSE
)+
  date_x_year_breaks(1990,2020,5)+#,partial_year=2024,partial_label="\nQ1")
  theme(legend.position = "right")+
  labs(y="Obligations (Constant 2023 $s)")
)
```

```
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

```
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, :
## font family not found in Windows font database
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

![](pbl_trends_files/figure-html/Services-1.png)<!-- -->

```r
log_plot(plot=Services, df=pbl_short  %>% filter(Fiscal_Year<=2024 &SimpleArea=="Services (Non-R&D)"),
                     filename="acq_fig3_3_services",xlsx="PBL_Contracts.xlsx",
                     sheet="3-3 Serv",path="../output/PBL/", height=3.5,
                     startRow=1,startCol=12,format=TRUE,#,var_list=c("SimpleArea","Competition.multisum")
         )
```

## PSR Pricing

```r
(
PSRpricing_dollar<-build_plot(
  data=pbl_short %>% filter(Fiscal_Year>=2000 & Fiscal_Year <= 2024 & !is.na(SimpleArea) & SimpleArea!="Unlabeled"),
  chart_geom = "Bar Chart",
  labels_and_colors=pbl_lc,
  # NA, #VAR.ncol
  x_var="dFYear",##alpha_var="YTD", #x_var
  y_var="Action_Obligation_OMB25_GDP23", #VAR.y.variable
  color_var="PricingUCA.sum", #color_var
  facet_var="SimpleArea", #facet_var
  column_key=pbl_ck,
  format=TRUE,
  ytextposition=FALSE,
  share=FALSE
)+theme(legend.position = "right")+
  labs(y="Obligations (Constant 2023 $s)",caption="Source: FPDS and CSIS analysis.",
       title="DOD Contract Obligations by Pricing Mechanism, FY 2000–FY 2023")
)
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

```
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, :
## font family not found in Windows font database
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

![](pbl_trends_files/figure-html/PSR_pricing-1.png)<!-- -->

```r
ggsave600dpi(path="..//Output//PBL//",filename="psr_pricing_dollar.png", PSRpricing_dollar, 
             width=6.5, height= 5, units="in",size=11, lineheight=1.2
             )


(
SimpleVendor_share<-build_plot(
  data=pbl_short %>% filter(Fiscal_Year>=2000 &Fiscal_Year<=2024 & !is.na(SimpleArea) & SimpleArea!="Unlabeled"),
  chart_geom = "Bar Chart",
  labels_and_colors=pbl_lc,
  # NA, #VAR.ncol
  x_var="dFYear",##alpha_var="YTD", #x_var
  y_var="Action_Obligation_OMB25_GDP23", #VAR.y.variable
  color_var="PricingUCA.sum", #color_var
  facet_var="SimpleArea", #facet_var
  column_key=pbl_ck,
  format=TRUE,
  ytextposition=FALSE,
  share=TRUE
)+
  # theme(strip.text.y = element_text(angle=0))+
 # theme(axis.text.x = element_text(angle=90))+
 #    scale_y_continuous("Percent of Obligations", labels = percent_format(accuracy=1))+
 #      facet_grid(.~Competition.sum ,scales="free_x", space="free_x"
 #             )+labs(title=NULL,
  # x="Fiscal Year",
  # y="Percent of Obligations",
  # color="Competition")+
  theme(legend.position = "right")+
  labs(y="Obligations (Constant 2023 $s)",caption="Source: FPDS and CSIS analysis.")
)
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

```
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, :
## font family not found in Windows font database
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

![](pbl_trends_files/figure-html/PSR_pricing-2.png)<!-- -->

```r
log_plot(plot=Services, df=pbl_short  %>% filter(Fiscal_Year<=2024 &SimpleArea=="Services (Non-R&D)"),
                     filename="aila30_pricing",xlsx="PBL_Contracts.xlsx",
                     sheet="PSR-price",path="../output/PBL/", height=3.5,
                     format=TRUE,output_slide_png=TRUE #,var_list=c("SimpleArea","Competition.multisum")
         )
```

## PSR Vendor size 

```r
(
SimpleVendor_dollar<-build_plot(
  data=pbl_short %>% filter(Fiscal_Year<=2021 & !is.na(SimpleArea) & SimpleArea!="Unlabeled"),
  chart_geom = "Bar Chart",
  labels_and_colors=pbl_lc,
  # NA, #VAR.ncol
  x_var="dFYear",##alpha_var="YTD", #x_var
  y_var="Action_Obligation_OMB25_GDP23", #VAR.y.variable
  color_var="Shiny.VendorSize", #color_var
  facet_var="SimpleArea", #facet_var
  column_key=pbl_ck,
  format=TRUE,
  ytextposition=FALSE,
  share=FALSE
)+
  # theme(strip.text.y = element_text(angle=0))+
 # theme(axis.text.x = element_text(angle=90))+
 #    scale_y_continuous("Percent of Obligations", labels = percent_format(accuracy=1))+
 #      facet_grid(.~Competition.sum ,scales="free_x", space="free_x"
 #             )+labs(title=NULL,
  # x="Fiscal Year",
  # y="Percent of Obligations",
  # color="Competition")+
  theme(legend.position = "right")+
  labs(y="Obligations (Constant 2023 $s)",caption="Source: FPDS and CSIS analysis.\nNote: The merger of Raytheon and United Technologies took place in April of 2020\n and thus did not make the cutofff for inclusion in FY2020.\nMcDonnell Douglas is grouped with the Big-5.")
)
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

```
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, :
## font family not found in Windows font database
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

![](pbl_trends_files/figure-html/PSR_vendorsize-1.png)<!-- -->

```r
ggsave600dpi(path="..//Output//PBL//",filename="psr_vendorsize_dollar.png", SimpleVendor_dollar, 
             width=6.5, height= 5, units="in",size=11, lineheight=1.2
             )


(
SimpleVendor_share<-build_plot(
  data=pbl_short %>% filter(Fiscal_Year<=2021 & !is.na(SimpleArea) & SimpleArea!="Unlabeled"),
  chart_geom = "Bar Chart",
  labels_and_colors=pbl_lc,
  # NA, #VAR.ncol
  x_var="dFYear",##alpha_var="YTD", #x_var
  y_var="Action_Obligation_OMB25_GDP23", #VAR.y.variable
  color_var="Shiny.VendorSize", #color_var
  facet_var="SimpleArea", #facet_var
  column_key=pbl_ck,
  format=TRUE,
  ytextposition=FALSE,
  share=TRUE
)+
  # theme(strip.text.y = element_text(angle=0))+
 # theme(axis.text.x = element_text(angle=90))+
 #    scale_y_continuous("Percent of Obligations", labels = percent_format(accuracy=1))+
 #      facet_grid(.~Competition.sum ,scales="free_x", space="free_x"
 #             )+labs(title=NULL,
  # x="Fiscal Year",
  # y="Percent of Obligations",
  # color="Competition")+
  theme(legend.position = "right")+
  labs(y="Obligations (Constant 2023 $s)",caption="Source: FPDS and CSIS analysis.")
)
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

```
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, :
## font family not found in Windows font database
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

![](pbl_trends_files/figure-html/PSR_vendorsize-2.png)<!-- -->

```r
ggsave600dpi(path="..//Output//PBL//",filename="psr_vendorsize_share.png", SimpleVendor_share, 
             width=6.5, height= 5, units="in",size=11, lineheight=1.2
             )
```


## PSR Competition

```r
summary(pbl_short$SimpleArea)
```

```
## Products      R&D Services 
##   112563     1513    19215
```

```r
(
PSRComp_bar<-build_plot(
  data=pbl_short %>% filter(Fiscal_Year>=1991 & Fiscal_Year<=2024 & SimpleArea!="Unlabeled"),
  chart_geom = "Bar Chart",
  share = FALSE,
  labels_and_colors=pbl_lc,
  # NA, #VAR.ncol
  x_var="dFYear",##alpha_var="YTD", #x_var
  y_var="Action_Obligation_OMB25_GDP23", #VAR.y.variable
  color_var="Competition.multisum", #color_var
  facet_var="SimpleArea", #facet_var
  column_key=pbl_ck,
  format=TRUE,
  ytextposition=FALSE
)+
  theme(legend.position = "bottom")+
  labs(x="Fiscal Year",
       y="Obligations (Constant 2023 $s)",
       title="DOD Contract Obligations by Extent of Competition, FY 1991–FY 2023")+
    date_x_year_breaks(1993,2023,by=6)#,partial_year=2024,partial_label="\nQ1")
)
```

```
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

```
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, :
## font family not found in Windows font database
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

![](pbl_trends_files/figure-html/PSRcomp_plat-1.png)<!-- -->

```r
(
PSRComp_line<-build_plot(
  data=pbl_short %>% filter(Fiscal_Year>=1991 & Fiscal_Year<=2024 & SimpleArea!="Unlabeled"),
  chart_geom = "Line Chart",
  share = TRUE,
  labels_and_colors=pbl_lc,
  # NA, #VAR.ncol
  x_var="dFYear",##alpha_var="YTD", #x_var
  y_var="Action_Obligation_OMB25_GDP23", #VAR.y.variable
  color_var="Competition.multisum", #color_var
  facet_var="SimpleArea", #facet_var
  column_key=pbl_ck,
  format=TRUE,
  ytextposition=FALSE
)+
  theme(legend.position = "bottom")+
  labs(x="Fiscal Year",y="Share of Obligations")+
    date_x_year_breaks(1993,2023,by=6)#,partial_year=2024,partial_label="\nQ1")
)
```

```
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

```
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, :
## font family not found in Windows font database
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

![](pbl_trends_files/figure-html/PSRcomp_plat-2.png)<!-- -->

```r
ggsave600dpi(path="..//Output//PBL//",filename="PSRComp_line.png", PSRComp_line, 
             width=6.5, height= 5, units="in",size=12, lineheight=1.2
             )


ggsave("..//Output//PBL//PSRComp_both.svg", 
             gridExtra::grid.arrange(
               PSRComp_bar+labs(caption = NULL,x=NULL,title=NULL)+theme(legend.position = "none"),
               PSRComp_line), 
             width=6.5, height= 5, units="in"
)
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

```
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, :
## font family not found in Windows font database
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

```
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, :
## font family not found in Windows font database
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

![](pbl_trends_files/figure-html/PSRcomp_plat-3.png)<!-- -->

```r
ggsave("..//Output//PBL//PSRComp_both_keynote.png", 
             gridExtra::grid.arrange(
               PSRComp_bar+labs(
                  y="Obligations\n(Const. 2023 $s)",
               caption = NULL,x=NULL)+theme(legend.position = "none",text=element_text(size=20)),
               PSRComp_line+labs(y="Share of\nObl.")+theme(text=element_text(size=20),
               plot.caption = element_text(size=round(20 * 5/6,0))
               )),
             width=13, height= 5.5, units="in" #, size=20, lineheight = 0.75
             )
```

```
## Warning in grid.Call(C_stringMetric, as.graphicsAnnot(x$label)): font family
## not found in Windows font database

## Warning in grid.Call(C_stringMetric, as.graphicsAnnot(x$label)): font family
## not found in Windows font database
```

```
## Warning in grid.Call(C_stringMetric, as.graphicsAnnot(x$label)): font family
## not found in Windows font database

## Warning in grid.Call(C_stringMetric, as.graphicsAnnot(x$label)): font family
## not found in Windows font database
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

```
## Warning in grid.Call(C_stringMetric, as.graphicsAnnot(x$label)): font family
## not found in Windows font database
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

```
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, :
## font family not found in Windows font database

## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, :
## font family not found in Windows font database

## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, :
## font family not found in Windows font database
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

```
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, :
## font family not found in Windows font database

## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, :
## font family not found in Windows font database

## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, :
## font family not found in Windows font database
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

![](pbl_trends_files/figure-html/PSRcomp_plat-4.png)<!-- -->

```r
ggsave("..//Output//PBL//acq_fig6_6_PSR_competition.svg", 
             gridExtra::grid.arrange(
               PSRComp_bar+labs(
                 title=NULL,
                  y="Obligations\n(Constant 2023 $s)",
               caption = NULL,x=NULL)+theme(legend.position = "none",text=element_text(size=12),
                                            plot.margin = margin(t=0,b=0,l=0.1,r=0.5,unit="inches")),
               PSRComp_line+labs(y="Share of\nObligations",caption=NULL)+theme(text=element_text(size=12),
               plot.caption = element_text(size=round(20 * 5/6,0)),
                                           plot.margin = margin(t=0,b=0,l=0.1,r=0.5,unit="inches"))),
             width=6.5, height= 4.5, units="in" #, size=20, lineheight = 0.75
             )
```

```
## Warning in grid.Call(C_stringMetric, as.graphicsAnnot(x$label)): font family
## not found in Windows font database

## Warning in grid.Call(C_stringMetric, as.graphicsAnnot(x$label)): font family
## not found in Windows font database
```

```
## Warning in grid.Call(C_stringMetric, as.graphicsAnnot(x$label)): font family
## not found in Windows font database
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

```
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, :
## font family not found in Windows font database

## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, :
## font family not found in Windows font database

## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, :
## font family not found in Windows font database
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

```
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, :
## font family not found in Windows font database

## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, :
## font family not found in Windows font database

## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, :
## font family not found in Windows font database
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

![](pbl_trends_files/figure-html/PSRcomp_plat-5.png)<!-- -->

```r
log_plot(plot=PSRComp_bar, df=pbl_short  %>% filter(Fiscal_Year<=2024),
                     filename="acq_fig6_6_PSR_competition",xlsx="PBL_Contracts.xlsx",
                     sheet="6-6 PSRcomp",path="../output/PBL/", height=3.5,
                     startRow=1,startCol=13,format=TRUE,var_list=c("SimpleArea","Competition.multisum"),
         output_doc_svg=FALSE,excel_y_var = TRUE, excel_formulas = TRUE, excel_share = TRUE,
         include_YTD=FALSE
         )
```
## PSR Multi-Year

```r
(
PSRMultiYear<-build_plot(
  data=pbl_short %>% filter(Fiscal_Year>=2000 ),
  chart_geom = "Bar Chart",
  share = FALSE,
  labels_and_colors=pbl_lc,
  # NA, #VAR.ncol
  x_var="dFYear",##alpha_var="YTD", #x_var
  y_var="Action_Obligation_OMB25_GDP23", #VAR.y.variable
  color_var="multiyearcontracttext", #color_var
  facet_var="SimpleArea",
  column_key=pbl_ck,
  format=TRUE,
  ytextposition=FALSE
)+date_x_year_breaks(2000,2022,2)+#,partial_year=2024,partial_label="\nQ1")   
  # theme(strip.text.y = element_text(angle=0))+
 # theme(axis.text.x = element_text(angle=90))+
 #    scale_y_continuous("Percent of Obligations", labels = percent_format(accuracy=1))+
 #      facet_grid(.~Competition.sum ,scales="free_x", space="free_x"
 #             )+labs(title=NULL,
  # x="Fiscal Year",
  # y="Percent of Obligations",
  # color="Competition")+
  theme(legend.position = "right")+
  labs(y="Obligations (Constant 2023 $s)")
)
```

```
## Warning in format_data_for_plot(data = data, share = share, fy_var = x_var, :
## color_var missing from labels_and_colors
```

```
## Warning in add_preassigned_scales(mainplot, labels_and_colors, var =
## color_var): multiyearcontracttext not found in labels_and_colors
```

```
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

```
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, :
## font family not found in Windows font database
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

![](pbl_trends_files/figure-html/PSRmultiyear-1.png)<!-- -->

```r
pbl_short<-pbl_short %>% group_by(Fiscal_Year,SimpleArea)%>%
  mutate(pPlatFYear=Action_Obligation_OMB25_GDP23/sum(Action_Obligation_OMB25_GDP23,na.rm=TRUE))
(
PSRMultiYearShare<-build_plot(
  data=pbl_short %>% filter(Fiscal_Year>=2000 & SimpleArea!="Unlabeled"&
                             (is.na(multiyearcontract)|multiyearcontract==1)),
  chart_geom = "Line Chart",
  share = FALSE,
  labels_and_colors=pbl_lc,
  # NA, #VAR.ncol
  x_var="dFYear",##alpha_var="YTD", #x_var
  y_var="pPlatFYear", #VAR.y.variable
  color_var="multiyearcontracttext", #color_var
  facet_var="SimpleArea",
  column_key=pbl_ck,
  format=TRUE,
  ytextposition=FALSE
)+date_x_year_breaks(2000,2022,2)+#,partial_year=2024,partial_label="\nQ1")   
  # theme(strip.text.y = element_text(angle=0))+
 # theme(axis.text.x = element_text(angle=90))+
 #    scale_y_continuous("Percent of Obligations", labels = percent_format(accuracy=1))+
 #      facet_grid(.~Competition.sum ,scales="free_x", space="free_x"
 #             )+labs(title=NULL,
  # x="Fiscal Year",
  # y="Percent of Obligations",
  # color="Competition")+
  theme(legend.position = "bottom")+
  labs(y="Share of Obligations")+
    scale_y_continuous(labels = percent)
)
```

```
## Warning in format_data_for_plot(data = data, share = share, fy_var = x_var, :
## color_var missing from labels_and_colors
```

```
## Warning in add_preassigned_scales(mainplot, labels_and_colors, var =
## color_var): multiyearcontracttext not found in labels_and_colors
```

```
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
## Scale for y is already present.
## Adding another scale for y, which will replace the existing scale.
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

```
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, :
## font family not found in Windows font database
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

![](pbl_trends_files/figure-html/PSRmultiyear-2.png)<!-- -->

```r
ggsave600dpi(path="..//Output//PBL//",filename="PSR_MultiYear.png", PSRMultiYear,  
             width=12, height= 6, units="in",size=12, lineheight=1.2
             )

ggsave600dpi(path="..//Output//PBL//",filename="PSR_MultiYearShare.png", PSRMultiYearShare,  
             width=6.5, height= 4, units="in",size=12, lineheight=1.2
             )

write.csv(file="..//Output//PBL//PSRMultiYearShare.csv",row.names = FALSE, na = "",
  pivot_wider(PSRMultiYearShare$data %>% 
                        arrange(dFYear) %>% mutate(Fiscal_Year=lubridate::year(dFYear)),
                      id_cols=c(SimpleArea,multiyearcontracttext),
                      names_from=Fiscal_Year,values_from=pPlatFYear)%>%
  arrange(SimpleArea,multiyearcontracttext))
```


# SubCustomer
## Topline SubCustomer 

```r
(
SubCustomer<-build_plot(
  data=pbl_short %>% filter(Fiscal_Year<=2024),
  chart_geom = "Bar Chart",
  share = FALSE,
  labels_and_colors=pbl_lc,
  # NA, #VAR.ncol
  x_var="dFYear",##alpha_var="YTD", #x_var
  y_var="Action_Obligation_OMB25_GDP23", #VAR.y.variable
  color_var="SubCustomer.JPO", #color_var
  facet_var="SubCustomer.sum", #facet_var
  column_key=pbl_ck,
  format=TRUE,
  ytextposition=FALSE
)+
  # theme(strip.text.y = element_text(angle=0))+
 # theme(axis.text.x = element_text(angle=90))+
 #    scale_y_continuous("Percent of Obligations", labels = percent_format(accuracy=1))+
 #      facet_grid(.~Competition.sum ,scales="free_x", space="free_x"
 #             )+labs(title=NULL,
  # x="Fiscal Year",
  # y="Percent of Obligations",
  # color="Competition")+
  theme(legend.position = "right")+
  labs(y="Obligations (Constant 2023 $s)",
       title="DOD Contract Obligations by DOD Component, FY 2000-FY 2024") +date_x_year_breaks(1990,2020,5)#,partial_year=2024,partial_label="\nQ1")
)
```

```
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

```
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, :
## font family not found in Windows font database
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

![](pbl_trends_files/figure-html/ToplineSubCustomer-1.png)<!-- -->

```r
log_plot(plot=SubCustomer+facet_wrap(~SubCustomer.sum,nrow=1)+
           date_x_year_breaks(1990,2020,8),#,partial_year=2024,partial_label="\nQ1"),
         df=pbl_short %>% filter(Fiscal_Year<=2024),
                     filename="acq_fig2_3_subcustomer",xlsx="PBL_Contracts.xlsx",
                     sheet="2-3 Cust",path="../output/PBL/", height=3.5,
                     startRow=1,startCol=13,format=TRUE,var_list=c("SubCustomer.sum","SubCustomer.JPO")
         )
```

```
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
```

```r
log_plot(plot=SubCustomer+facet_wrap(~SubCustomer.sum,nrow=1)+
           date_x_year_breaks(1990,2022,8), df=pbl_short %>% filter(Fiscal_Year<=2024),
                     filename="nps_fig04_component",xlsx="DOD_2024_NPS.xlsx",
                     sheet="4 Cust",path="../output/PBL/",
         height=3,include_YTD=FALSE,suppress_doc_svg_text="title",
                     startRow=1,format=TRUE,excel_y_var=TRUE,excel_formulas = TRUE,
         output_slide_png=TRUE
         )
```

```
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
```

## SuCustomer Plat Breakout

```r
platform_levels<-levels(factor(pbl_short$PlatformPortfolio))
for(p in platform_levels){
  file_p<-gsub("__*","_",gsub("&","and",gsub("[ |,]","_",p)))
  (Breakout<-build_plot(
    data=pbl_short %>% filter(Fiscal_Year>=2000&PlatformPortfolio==p& Fiscal_Year<=2024),
chart_geom = "Bar Chart",
  share = FALSE,
  labels_and_colors=pbl_lc,
  # NA, #VAR.ncol
  x_var="dFYear",##alpha_var="YTD", #x_var
  y_var="Action_Obligation_OMB25_GDP23", #VAR.y.variable
  color_var="SubCustomer.JPO", #color_var
  facet_var="SubCustomer.sum", #facet_var
  column_key=pbl_ck,
  format=TRUE,
  ytextposition=FALSE
)+
  theme(legend.position = "right")+
  labs(y="Obligations (Constant 2023 $s)") +date_x_year_breaks(1990,2020,5)#,partial_year=2024,partial_label="\nQ1")
)
  if(!dir.exists(file.path("../output/PBL/Platform",file_p)))
    dir.create(file.path("../output/PBL/Platform",file_p))
  
  log_plot(plot=Breakout, df=pbl_short  %>% filter(Fiscal_Year<=2024&PlatformPortfolio==p),
           filename=paste(file_p,"subcustomer",sep="_"),xlsx=paste("DoD",file_p,"Contracts.xlsx",sep="_"),
           sheet="Cust",path=file.path("../output/PBL/Platform/",file_p), height=3.5,
           startRow=1,startCol=13,format=TRUE,#,var_list=c("PricingUCA.sum","PricingUCA"),
           output_doc_png=TRUE,excel_y_var=TRUE
           )
  
}
```

```
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
```


## SubCustomer Product/Service/R&D

```r
(
SubCustomerPSR<-build_plot(
  data=pbl_short,
  chart_geom = "Bar Chart",
  share = FALSE,
  labels_and_colors=pbl_lc,
  # NA, #VAR.ncol
  x_var="Fiscal_Year", #x_var
  y_var="Action_Obligation_OMB25_GDP23", #VAR.y.variable
  color_var="SimpleArea", #color_var
  facet_var="SubCustomer.sum", #facet_var
  column_key=pbl_ck,
  format=TRUE,
  ytextposition=FALSE
)+
  # theme(strip.text.y = element_text(angle=0))+
 # theme(axis.text.x = element_text(angle=90))+
 #    scale_y_continuous("Percent of Obligations", labels = percent_format(accuracy=1))+
 #      facet_grid(.~Competition.sum ,scales="free_x", space="free_x"
 #             )+labs(title=NULL,
  # x="Fiscal Year",
  # y="Percent of Obligations",
  # color="Competition")+
  theme(legend.position = "bottom")+
  labs(y="Obligations (Constant 2023 $s)")
)
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

```
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, :
## font family not found in Windows font database
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

![](pbl_trends_files/figure-html/SubCustomerPSR-1.png)<!-- -->

```r
# ggsave600dpi(path="..//Output//PBL//",filename="subcustomer_PSR.png", SubCustomerPSR, 
#              width=6.5, height= 4, units="in",size=11
#              )
ggsave600dpi(path="..//Output//PBL//",filename="subcustomer_PSR.png", SubCustomerPSR, 
             width=12, height= 6, units="in",size=12,lineheight = 1.2
             )
```



# Platform
## Topline  Platform

```r
(
Platform<-build_plot(
  data=pbl_short %>% filter(Fiscal_Year>=1990 & Fiscal_Year<=2024),
  chart_geom = "Line Chart",
  share = FALSE,
  labels_and_colors=pbl_lc,
  # NA, #VAR.ncol
  x_var="dFYear",##alpha_var="YTD", #x_var
  y_var="Action_Obligation_OMB25_GDP23", #VAR.y.variable
  color_var="PlatformPortfolio", #color_var
  facet_var="PlatformPortfolio", #facet_var
  column_key=pbl_ck,
  format=TRUE,
  ytextposition=FALSE
)+date_x_year_breaks(1990,2020,8)+#,partial_year=2024,partial_label="\nQ1")   
  # theme(strip.text.y = element_text(angle=0))+
 # theme(axis.text.x = element_text(angle=90))+
 #    scale_y_continuous("Percent of Obligations", labels = percent_format(accuracy=1))+
 #      facet_grid(.~Competition.sum ,scales="free_x", space="free_x"
 #             )+labs(title=NULL,
  # x="Fiscal Year",
  # y="Percent of Obligations",
  # color="Competition")+
  theme(legend.position = "none")+
  labs(y="Obligations (Constant 2023 $s)",
       title="DOD Contract Obligations by Platform Portfolio, FY 2000-FY 2024")
)
```

```
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

```
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, :
## font family not found in Windows font database
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

![](pbl_trends_files/figure-html/Platform-1.png)<!-- -->

```r
(
Platform_Bar<-build_plot(
  data=pbl_short %>% filter(Fiscal_Year>=1990 & Fiscal_Year<=2024),
  chart_geom = "Bar Chart",
  share = FALSE,
  labels_and_colors=pbl_lc,
  # NA, #VAR.ncol
  x_var="dFYear",##alpha_var="YTD", #x_var
  y_var="Action_Obligation_OMB25_GDP23", #VAR.y.variable
  color_var="PlatformPortfolio", #color_var
  facet_var="PlatformPortfolio", #facet_var
  column_key=pbl_ck,
  format=TRUE,
  ytextposition=FALSE
)+date_x_year_breaks(1990,2020,8)+#,partial_year=2024,partial_label="\nQ1")   
  theme(legend.position = "none")+
  labs(y="Obligations (Constant 2023 $s)",
       title="DOD Contract Obligations by Platform Portfolio, FY 2000-FY 2024")
)
```

```
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

```
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, :
## font family not found in Windows font database
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database

## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

![](pbl_trends_files/figure-html/Platform-2.png)<!-- -->

```r
log_plot(plot=Platform_Bar+theme(plot.margin = margin(t=0,r=0.25,b=0.1,l=0.1,"inches")), df=pbl_short   %>% filter(Fiscal_Year<=2024),
                     filename="acq_fig3_2_platform",xlsx="PBL_Contracts.xlsx",
                     sheet="3-2 Plat",path="../output/PBL/", height=3.5,
                     startRow=1,startCol=12,format=TRUE#var_list=c("Source","fiscal_quarter"),
         )


log_plot(plot=Platform_Bar+theme(plot.margin = margin(t=0,r=0.25,b=0.1,l=0.1,"inches")), df=pbl_short   %>% filter(Fiscal_Year<=2024),
                     filename="nps_fig03_platform",xlsx="DoD_2024_NPS.xlsx",
                     sheet="3 Plat",path="../output/PBL/",
         height=3.5,include_YTD=FALSE,suppress_doc_svg_text="title",output_slide_png=TRUE,
                     startRow=1,format=TRUE,excel_y_var=TRUE,excel_formulas = TRUE,
         output_doc_svg=TRUE
         )
```

