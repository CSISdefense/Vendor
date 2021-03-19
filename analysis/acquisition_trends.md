---
title: "Defense Acquisition Trends"
output:
  html_document:
    keep_md: yes
    toc: yes
date: "Thursday, March 4, 2031"
---

# Setup
First we load the data. The dataset used is a U.S. Defense Contracting dataset derived from   FPDS.


```
## Warning: replacing previous import 'Hmisc::summarize' by 'dplyr::summarize' when
## loading 'csis360'
```

```
## Warning: replacing previous import 'Hmisc::src' by 'dplyr::src' when loading
## 'csis360'
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
## -- Attaching packages --------------------------------------- tidyverse 1.3.0 --
```

```
## v tibble  3.0.4     v purrr   0.3.4
## v tidyr   1.1.2     v stringr 1.4.0
## v readr   1.4.0     v forcats 0.5.0
```

```
## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

```
## 
## Attaching package: 'scales'
```

```
## The following object is masked from 'package:purrr':
## 
##     discard
```

```
## The following object is masked from 'package:readr':
## 
##     col_factor
```

# Topline

## Topline Product/Service/R&D

```r
(
ToplinePSR<-build_plot(
  data=full_data,
  chart_geom = "Bar Chart",
  share = FALSE,
  labels_and_colors=labels_and_colors,
  # NA, #VAR.ncol
  x_var="fiscal_year", #x_var
  y_var="Action_Obligation_OMB19_19", #VAR.y.variable
  color_var="ProductServiceOrRnDarea.sum", #color_var
  # facet_var="Competition.sum", #facet_var
  column_key=column_key,
  format=TRUE,
  ytextposition=FALSE
)+
  # theme(strip.text.y = element_text(angle=0))+
 # theme(axis.text.x = element_text(angle=90))+
 #    scale_y_continuous("Percent of Obligations", labels = percent_format(accuracy=1))+
 #      facet_grid(.~Competition.sum ,scales="free_x", space="free_x"
 #             )+labs(title=NULL,
  # xlab="Fiscal Year",
  # ylab="Percent of Obligations",
  # color="Competition")+
  theme(legend.position = "right")+
  labs(y="Obligations (Constant 2019 $s)")
)
```

```
## Warning: `summarise_()` is deprecated as of dplyr 0.7.0.
## Please use `summarise()` instead.
## This warning is displayed once every 8 hours.
## Call `lifecycle::last_warnings()` to see where this warning was generated.
```

```
## Warning: `group_by_()` is deprecated as of dplyr 0.7.0.
## Please use `group_by()` instead.
## See vignette('programming') for more help
## This warning is displayed once every 8 hours.
## Call `lifecycle::last_warnings()` to see where this warning was generated.
```

```
## Warning in grid.Call(C_stringMetric, as.graphicsAnnot(x$label)): font family not
## found in Windows font database

## Warning in grid.Call(C_stringMetric, as.graphicsAnnot(x$label)): font family not
## found in Windows font database
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

```
## Warning in grid.Call(C_stringMetric, as.graphicsAnnot(x$label)): font family not
## found in Windows font database
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

![](acquisition_trends_files/figure-html/ToplinePSR-1.png)<!-- -->

```r
ggsave600dpi("..//Output//topline_psr.png", ToplinePSR, 
             width=6.5, height= 4, units="in",size=11
             )
```

```
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
## family not found in Windows font database
```

```
## Warning in grid.Call(C_stringMetric, as.graphicsAnnot(x$label)): font family not
## found in Windows font database
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


# SubCustomer
## SubCustomer Product/Service/R&D

```r
(
SubCustomerPSR<-build_plot(
  data=full_data,
  chart_geom = "Bar Chart",
  share = FALSE,
  labels_and_colors=labels_and_colors,
  # NA, #VAR.ncol
  x_var="fiscal_year", #x_var
  y_var="Action_Obligation_OMB19_19", #VAR.y.variable
  color_var="ProductServiceOrRnDarea.sum", #color_var
  facet_var="SubCustomer.sum", #facet_var
  column_key=column_key,
  format=TRUE,
  ytextposition=FALSE
)+
  # theme(strip.text.y = element_text(angle=0))+
 # theme(axis.text.x = element_text(angle=90))+
 #    scale_y_continuous("Percent of Obligations", labels = percent_format(accuracy=1))+
 #      facet_grid(.~Competition.sum ,scales="free_x", space="free_x"
 #             )+labs(title=NULL,
  # xlab="Fiscal Year",
  # ylab="Percent of Obligations",
  # color="Competition")+
  theme(legend.position = "bottom")+
  labs(y="Obligations (Constant 2019 $s)")
)
```

```
## Warning in if (is.na(breakout)) breakout <- NULL: the condition has length > 1
## and only the first element will be used
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

![](acquisition_trends_files/figure-html/SubCustomerPSR-1.png)<!-- -->

```r
ggsave600dpi("..//Output//subcustomer_PSR.png", SubCustomerPSR, 
             width=6.5, height= 4, units="in",size=11
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



# Platform
## Platform

```r
(
Platform<-build_plot(
  data=full_data,
  chart_geom = "Line Chart",
  share = FALSE,
  labels_and_colors=labels_and_colors,
  # NA, #VAR.ncol
  x_var="dFYear", #x_var
  y_var="Action_Obligation_OMB19_19", #VAR.y.variable
  color_var="PlatformPortfolio", #color_var
  facet_var="PlatformPortfolio", #facet_var
  column_key=column_key,
  format=TRUE,
  ytextposition=FALSE
)+scale_x_date("Fiscal Year",labels = date_format("'%y"))+
  # theme(strip.text.y = element_text(angle=0))+
 # theme(axis.text.x = element_text(angle=90))+
 #    scale_y_continuous("Percent of Obligations", labels = percent_format(accuracy=1))+
 #      facet_grid(.~Competition.sum ,scales="free_x", space="free_x"
 #             )+labs(title=NULL,
  # xlab="Fiscal Year",
  # ylab="Percent of Obligations",
  # color="Competition")+
  theme(legend.position = "none")+
  labs(y="Obligations (Constant 2019 $s)")
)
```

```
## Warning in if (is.na(breakout)) breakout <- NULL: the condition has length > 1
## and only the first element will be used
```

```
## Scale for 'x' is already present. Adding another scale for 'x', which will
## replace the existing scale.
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
```

![](acquisition_trends_files/figure-html/Platform-1.png)<!-- -->

```r
ggsave600dpi("..//Output//platform.png", Platform, 
             width=6.5, height= 4, units="in",size=11,lineheight = 0.75
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
```



## Platform SubCustomer

```r
(
PlatformSubCustomer<-build_plot(
  data=full_data,
  chart_geom = "Bar Chart",
  share = FALSE,
  labels_and_colors=labels_and_colors,
  # NA, #VAR.ncol
  x_var="dFYear", #x_var
  y_var="Action_Obligation_OMB19_19", #VAR.y.variable
  color_var="SubCustomer.platform", #color_var
  facet_var="PlatformPortfolio", #facet_var
  column_key=column_key,
  format=TRUE,
  ytextposition=FALSE
)+scale_x_date("Fiscal Year",labels = date_format("'%y"))+
  # theme(strip.text.y = element_text(angle=0))+
 # theme(axis.text.x = element_text(angle=90))+
 #    scale_y_continuous("Percent of Obligations", labels = percent_format(accuracy=1))+
 #      facet_grid(.~Competition.sum ,scales="free_x", space="free_x"
 #             )+labs(title=NULL,
  # xlab="Fiscal Year",
  # ylab="Percent of Obligations",
  # color="Competition")+
  theme(legend.position = "bottom")+
  labs(y="Obligations (Constant 2019 $s)")
)
```

```
## Warning in if (is.na(breakout)) breakout <- NULL: the condition has length > 1
## and only the first element will be used
```

```
## Scale for 'x' is already present. Adding another scale for 'x', which will
## replace the existing scale.
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

![](acquisition_trends_files/figure-html/PlatformSubCustomer-1.png)<!-- -->

```r
ggsave600dpi("..//Output//platform_subcustomer.png", PlatformSubCustomer, 
             width=6.5, height= 4, units="in",size=11,lineheight = 0.75
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
