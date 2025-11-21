---
title: "UEI Graphing"
author: "Greg Sanders"
date: "November 21, 2025"
output: 
  html_document: 
    toc: yes
    keep_md: yes
---
# Setup



# Defense Recipient_UEI

### Count by largest contract


```r
  (
  all_10k_small_bar<-build_plot(
    data=def_rpuh %>% dplyr::filter(IsEntityAbove2018constantMTAthreshold==1
                                          & Fiscal_Year>=2007& Fiscal_Year<=2025), ##where is FPDS summary?
    chart_geom = "Bar Chart",
    share = FALSE,
    x_var="dFYear",alpha_var="YTD",
    y_var="count", #Name of variable to plot on y-axis
    color_var="LargestContract2018dollars",       # name of coloration variable, as string
    facet_var="AlwaysIsSmallLabel",        # name of facet variable, as string
    legend=TRUE, #Include a legend
    caption=TRUE, #Include a source caption
        labels_and_colors=rpuh_lc,
    column_key=rpuh_ck,
    format=TRUE,
    first_color_on_bottom = FALSE
  )+labs(y="Count of Vendors with\nContracts Above $10 K",x="Fiscal Year",
         title="DOD Contractor Count by Small Business Status\n and Size of Largest Federal Contract, FY 2007-FY 2025 YTD",
         caption="Note: Threshold values are adjusted for inflation (in 2018 $s).\nSource: FPDS Unique Entities (UEI) and CSIS analysis. YTD through 90 days prior to 8/8/2025.")+
    date_x_year_breaks(2007,2022,3,partial_year=2025,partial_label="\nYTD")+date_x_year_breaks(2007,2022,3,partial_year=2025,partial_label="\nYTD")
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
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
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

![](industrial_base_ruh_pruh_files/figure-html/def_rpuh_uei-1.png)<!-- -->

```r
log_plot(plot=all_10k_small_bar+theme(legend.position="right"), df=def_rpuh %>% dplyr::filter(IsEntityAbove2018constantMTAthreshold==1
                                          & Fiscal_Year>=2007& Fiscal_Year<=2025),#%>% filter(Fiscal_YQ<=2023.2)
                     filename="nps_fig09_def_vendor_count",xlsx="DoD_2025_NPS.xlsx",
                     sheet="9 DIB count",path="../output/AcqTrends/NPS2025/",
         # second_path=online_path,
         height=2.5,
         csv_then_year=FALSE  ,excel_then_year=FALSE,excel_y_var=TRUE,
         excel_formulas = TRUE,
                     startRow=1,format=TRUE,#,var_list=c("SimpleArea","Competition.multisum"),
         output_doc_png = TRUE,output_slide_png = TRUE,suppress_doc_svg_text = "title",
         include_YTD = FALSE,
         num_format = "0.00,\"K\""
         )

(
  all_10k_small_line<-build_plot(
    data=def_rpuh %>% dplyr::filter(IsEntityAbove2018constantMTAthreshold==1
                                          & Fiscal_Year>=2007& Fiscal_Year<=2025), ##where is FPDS summary?
    chart_geom = "Line Chart",
    share = FALSE,
    x_var="dFYear",alpha_var="YTD",
    y_var="count", #Name of variable to plot on y-axis
    color_var="LargestContract2018dollars",       # name of coloration variable, as string
    facet_var="AlwaysIsSmallLabel",        # name of facet variable, as string
    legend=TRUE, #Include a legend
    caption=TRUE, #Include a source caption
    labels_and_colors=rpuh_lc,
    column_key=rpuh_ck,
    format=TRUE,
    first_color_on_bottom = FALSE
  )+labs(y="Count of Vendors with\nContracts Above $10 K, FY 2007-FY 2025 YTD",x="Fiscal Year",
         title="DOD Contractor Count by Small Business Status\n and Size of Largest Federal Contract",
         caption="Note: Threshold values are adjusted for inflation (in 2018 $s).\nSource: FPDS Unique Entities (UEI) and CSIS analysis. YTD through 90 days prior to 8/8/2025.")+date_x_year_breaks(2007,2022,3,partial_year=2025,partial_label="\nYTD")
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

![](industrial_base_ruh_pruh_files/figure-html/def_rpuh_uei-2.png)<!-- -->

```r
ggsave600dpi(all_10k_small_line+labs(title=NULL),file="nps_fig09a_def_vendor_count_line.svg",size=12,lineheight = 1.2,
             height=3,width=6.5, path="../output/AcqTrends/NPS2025/", 
         # second_path=online_path
         )

def_rpuh %>% dplyr::filter( Fiscal_Year>=2007 & Fiscal_Year<=2025)  %>% group_by(LargestContract2018dollars) %>% summarise(DefenseObligated_OMB25_GDP23=sum(DefenseObligated_OMB25_GDP23))
```

```
## # A tibble: 5 × 2
##   LargestContract2018dollars DefenseObligated_OMB25_GDP23
##   <chr>                                             <dbl>
## 1 [$10 K - $250 K)                                4.65e10
## 2 [$2.0 M - $7.5M)                                1.86e11
## 3 [$250k K - $2.0 M)                              1.12e11
## 4 [$7.5 M+]                                       7.87e12
## 5 [0 K - $10 K)                                   1.08e 9
```

```r
  (
  all_10k_small_dollar<-build_plot(
    data=def_rpuh %>% dplyr::filter( Fiscal_Year>=2007 & Fiscal_Year<=2025), ##where is FPDS summary?
    chart_geom = "Bar Chart",
    share = FALSE,
    x_var="dFYear",alpha_var="YTD",
    y_var="DefenseObligated_OMB25_GDP23", #Name of variable to plot on y-axis
    color_var="LargestContract2018dollars",       # name of coloration variable, as string
    facet_var="AlwaysIsSmallLabel",        # name of facet variable, as string
    legend=TRUE, #Include a legend
    caption=TRUE, #Include a source caption
    labels_and_colors=rpuh_lc,
    column_key=rpuh_ck,
    format=TRUE,
    first_color_on_bottom = FALSE
  )+labs(y="Obligations (2023 $s)",x="Fiscal Year",
         title="DOD Contract Obligations by Small Business Status\nand Size of Largest Federal Contract, FY 2007-FY 2025 YTD",
         caption="Note: Threshold values are adjusted for inflation (in 2018 $s).\nSource: FPDS Unique Entities (UEI) and CSIS analysis. YTD through 90 days prior to 8/8/2025.")+date_x_year_breaks(2007,2022,3,partial_year=2025,partial_label="\nYTD")+
      theme(legend.position = "right")#guides(fill=guide_legend(nrow=2))
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

![](industrial_base_ruh_pruh_files/figure-html/def_rpuh_uei-3.png)<!-- -->

```r
log_plot(plot=all_10k_small_dollar, df=def_rpuh %>% dplyr::filter(IsEntityAbove2018constantMTAthreshold==1
                                                                  & Fiscal_Year>=2007& Fiscal_Year<=2025),#%>% filter(Fiscal_YQ<=2023.2)
         # second_path=online_path,
         filename="nps_fig10_def_vendor_obl",xlsx="DoD_2025_NPS.xlsx",
         sheet="10 DIB dollars",path="../output/AcqTrends/NPS2025/", height=2.5,
         excel_formulas=TRUE,
         startRow=1,format=TRUE,#,var_list=c("SimpleArea","Competition.multisum"),
         output_slide_png = TRUE,suppress_doc_svg_text = "title"
)
```







## Overall HHI

```r
  (
  def_rpuh_hhi<-build_plot(
    data=def_rpuh %>% dplyr::filter( Fiscal_Year>=2007& Fiscal_Year<=2025), ##where is FPDS summary?
    chart_geom = "Line Chart",
    share = FALSE,
    x_var="Fiscal_Year",
    y_var="hhi", #Name of variable to plot on y-axis
    # color_var="IsEntityAbove2018constantSimplifedAcquisition250kThreshold",       # name of coloration variable, as string
    # facet_var="AlwaysIsSmallLabel",        # name of facet variable, as string
    legend=TRUE, #Include a legend
    caption=TRUE, #Include a source caption
    # labels_and_colors=eid_lc,
    column_key=NULL,
    format=TRUE,
    first_color_on_bottom=FALSE
  )+labs(y="Herfindahl-\nHirschman\nIndex (HHI)",x="Fiscal Year")
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
```

```
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, :
## font family not found in Windows font database

## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, :
## font family not found in Windows font database

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

![](industrial_base_ruh_pruh_files/figure-html/def_rpuh_hhi-1.png)<!-- -->

```r
  log_plot(plot=def_rpuh_hhi+labs(title=NULL),
           df=def_rpuh %>% dplyr::filter(
                                            Fiscal_Year>=2007& Fiscal_Year<=2025),#%>% filter(Fiscal_YQ<=2023.2)
           # second_path=online_path,
                       filename="nps_figXX_hhi",xlsx="DoD_2025_NPS.xlsx",
                       sheet="XX hhi",path="../output/AcqTrends/NPS2025/",
           height=2,
           excel_formulas=FALSE,excel_then_year = FALSE,csv_then_year=FALSE,
             suppress_doc_svg_text="title",output_slide_png=TRUE,
                       startRow=1,format=TRUE,#,var_list=c("SimpleArea","Competition.multisum"),
           output_doc_png = TRUE
           )
```
