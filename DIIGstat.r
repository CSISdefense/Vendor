#DIIGstat.r
library(arm)
library(dplyr)
#This will likely be folded into CSIS360
#But for now, using it to create and refine functions for regression analysis.

transform_contract<-function(
  contract
){

  # contract$pNewWorkUnmodifiedBaseAndAll<-as.numeric(as.character(contract$pNewWorkUnmodifiedBaseAndAll))
  #Newwork and change
  # contract$pNewWork3Sig<-round(
    # contract$pNewWorkUnmodifiedBaseAndAll,3)
  


  #Customer
  if(!"Is.Defense" %in% colnames(contract)){
    contract$Is.Defense<-as.character(contract$Who)
    contract$Is.Defense[contract$Is.Defense %in%
                          c("Air Force","Army",
                            "Navy","Other DoD","Uncategorized"  )
                        ]<-"Defense"
    contract$Is.Defense<-factor(contract$Is.Defense)
  }
  
  
  #PSR_What
contract$PSR_What<-factor(paste(as.character(contract$PSR),
                                             as.character(contract$What),sep="."))
contract$PSR_What[contract$PSR_What=="Unlabeled"]<-NA

#b_CBre
contract$b_CBre<-ifelse(contract$CBre=="Ceiling Breach",1,NA)
contract$b_CBre[contract$CBre=="None"]<-0


#Create a jittered version of Crai for display purposes
#Unlike geom_jitter, this caps values at 0 and 1
contract$j_Crai<-jitter.binary(contract$b_CBre)


#b_Term
contract$b_Term<-ifelse(contract$Term=="Terminated",1,NA)
contract$b_Term[contract$Term=="Unterminated"]<-0

#Create a jittered version of Term for display purposes
#Unlike geom_jitter, this caps values at 0 and 1
contract$j_Term<-jitter.binary(contract$b_Term)

#n_Crai
# contract$pChangeOrderUnmodifiedBaseAndAll<-as.numeric(as.character(contract$pChangeOrderUnmodifiedBaseAndAll))
# contract$pChange3Sig<-round(
#   contract$pChangeOrderUnmodifiedBaseAndAll,3)

#Should include this in the original data frame but for now can drive it.
contract$n_Crai<-contract$ChangeOrderBaseAndAllOptionsValue

#l_Crai
contract$l_Crai<-NA
contract$l_Crai[contract$b_CBre==1 & !is.na(contract$b_CBre)]<-
  log(contract$n_Crai[contract$b_CBre==1 & !is.na(contract$b_CBre)])

#l_Ceil
contract$l_Ceil<-log(contract$UnmodifiedContractBaseAndAllOptionsValue)
contract$l_Ceil[is.infinite(contract$l_Ceil)]<-NA

contract<-contract %>% group_by(Ceil) %>%
  mutate(ceil.median.wt = median(UnmodifiedContractBaseAndAllOptionsValue))

contract$Ceil.Simple<-as.character(contract$Ceil)

contract$Ceil.Simple[contract$Ceil.Simple %in% c(
  "75m+",
  "10m - <75m")]<-"10m+"
contract$Ceil.Simple[contract$Ceil.Simple %in% c(
  "1m - <10m",
  "100k - <1m")]<-"100k - <10m"
contract$Ceil.Simple[contract$Ceil.Simple %in% c(
  "15k - <100k",
  "0 - <15k")]<-"0k - <100k"
contract$Ceil.Simple<-factor(contract$Ceil.Simple,
                                           levels=c("0k - <100k",
                                                    "100k - <10m",
                                                    "10m+"),
                                           ordered=TRUE
)


contract$Ceil.Big<-as.character(contract$Ceil)

contract$Ceil.Big[contract$Ceil.Big %in% c(
  "100k - <1m",
  "15k - <100k",
  "0 - <15k")]<-"0k - <1m"

contract$Ceil.Big<-factor(contract$Ceil.Big,
                                        levels=c("0k - <1m",
                                                 "1m - <10m",
                                                 "10m - <75m",
                                                 "75m+"),
                                        ordered=TRUE
)



#l_Days

contract$l_Days<-log(contract$UnmodifiedDays)
contract$l_Days[is.infinite(contract$l_Days)]<-NA


contract$UnmodifiedYearsFloat<-contract$UnmodifiedDays/365.25
contract$UnmodifiedYearsCat<-floor(contract$UnmodifiedYearsFloat)
contract$Dur[contract$UnmodifiedYearsCat<0]<-NA

contract$Dur.Simple<-as.character(contract$Dur)
contract$Dur.Simple[contract$Dur.Simple %in% c(
  "[0 months,~2 months)",
  "[~2 months,~7 months)",
  "[~7 months-~1 year]")]<-"<~1 year"
contract$Dur.Simple<-factor(contract$Dur.Simple,
                                         levels=c("<~1 year",
                                                  "(~1 year,~2 years]",
                                                  "(~2 years+]"),
                                         ordered=TRUE
)


#b_ODoD
contract$b_ODoD<-contract$Who
levels(contract$b_ODoD)<- list("1"=c("Other DoD"), 
                                "0"=c("Air Force","Army","Navy"))
contract$b_ODoD[contract$b_ODoD=="Uncategorized"]<-NA
contract$b_ODoD<-as.integer(as.character(contract$b_ODoD))


#n_Fixed

contract$n_Fixed<-contract$FxCb
levels(contract$n_Fixed)<- list("1"=c("Fixed-Price"), 
                                             "0.5"=c("Combination or Other"),
                                             "0"=c("Cost-Based"))
contract$n_Fixed<-as.integer(as.character(contract$n_Fixed))

#n_Incent
contract$n_Incent<-contract$Fee
levels(contract$n_Incent) <-
  list("1"=c("Incentive"), 
       "0.5"=c("Combination or Other Fee"),
       "0"=c("Award Fee", "FFP or No Fee", "Fixed Fee"))
contract$n_Incent<-as.integer(as.character(contract$n_Incent))

#n_NoFee
contract$n_NoFee<-contract$Fee
levels(contract$n_NoFee) <-
  list("1"=c("FFP or No Fee"), 
       "0.5"=c("Combination or Other Fee"),
       "0"=c("Award Fee", "Incentive", "Fixed Fee"))
contract$n_NoFee<-as.integer(as.character(contract$n_NoFee))


#n_Comp
if (all(levels(factor(contract$Comp))==c("0","1"))){
  contract$Comp<-factor(contract$Comp)
  levels(contract$Comp) <-
  list("No Competition"="0",
       "Competition"="1")

#Right now comp is not actually a factor, so don't need to process it
contract$b_Comp<-contract$Comp #Fix in Rdata, and add back comp
levels(contract$b_Comp) <-
  list("0"="No Competition",
       "1"="Competition")
contract$b_Comp<-as.integer(as.character(contract$b_Comp))


contract$n_Comp<-contract$b_Comp
contract$n_Comp[contract$b_Comp==1 & !is.na(contract$b_Comp)]<-
  ifelse(contract$SingleOffer[contract$b_Comp==1 & !is.na(contract$b_Comp)]=="Multi",2,1)
contract$EffComp<-factor(contract$n_Comp)
levels(contract$EffComp) <-
  list("No Competition"="0",
       "1 Offer"="1",
       "2+ Offer"="2")

contract$n_Offr<-contract$Offr
levels(contract$n_Offr) <-
  list("1"=c("  1"), 
       "2"=c("  2"),
       "3"=c("[  3,  5)"),
       "4"=c("[  5,999]"))
contract$n_Offr<-as.integer(as.character(contract$n_Offr))
contract$n_Offr[contract$b_Comp==0 & !is.na(contract$b_Comp)]<-0
levels(contract$Offr) <-
  list("1"="  1", 
       "2"="  2",
       "3-4"="[  3,  5)",
       "5+"="[  5,999]")
contract$CompOffr<-factor(contract$n_Offr)
levels(contract$CompOffr) <-
  list("No Competition"="0",
       "1 offer"="1", 
       "2 offers"="2",
       "3-4 offers"="3",
       "5+ offers"="4")
  
  
  #l_Offr
  contract$l_Offr<-log(contract$UnmodifiedNumberOfOffersReceived)
  contract$l_Offr[is.infinite(contract$l_Days)]<-NA
  
  contract$cb_Comp<-scale(contract$b_Comp)
  contract$cn_Comp<-scale(contract$n_Comp)
  contract$cn_Offr<-scale(contract$n_Offr)
  contract$cl_Offr<-scale(contract$l_Offr)
  
}

#b_Intl
contract$b_Intl<-contract$Intl
contract$b_Intl[contract$b_Intl=="Unlabeled"]<-NA
levels(contract$b_Intl) <-
  list("0"=c("Just U.S."), 
       "1"=c("Any International"))
contract$b_Intl<-as.integer(as.character(contract$b_Intl))



#b_UCA
contract$b_UCA<-contract$UCA
levels(contract$b_UCA) <-
  list("0"=c("Not UCA"), 
       "1"=c("UCA"))
contract$b_UCA<-as.integer(as.character(contract$b_UCA))





# contract$DecisionTree<-as.character(contract$MaxOfDecisionTree)
# contract$DecisionTree[
#   contract$DecisionTree=="Excluded"|
#     is.na(contract$DecisionTree)]<-"All Other"
# contract$DecisionTree<-factor(contract$DecisionTree,levels=c("OCO","Disaster","ARRA","All Other"))
# 
# if(!"Is.Defense" %in% colnames(contract)){
#   contract$Is.Defense<-as.character(contract$Who)
#   contract$Is.Defense[contract$Is.Defense %in%
#                                      c("Air Force","Army",
#                                        "Navy","Other DoD","Uncategorized"  )
#                                    ]<-"Defense"
#   contract$Is.Defense<-factor(contract$Is.Defense)
# }



#IDV
contract$IDV<-contract$Veh
levels(contract$IDV) <-
  list("1"=c("MULTIPLE AWARD", "SINGLE AWARD","MULTIPLE AWARD"), 
       "0"=c("Def/Pur"))
contract$IDV<-as.integer(as.character(contract$IDV))

#SIDV
contract$SIDV<-contract$Veh
levels(contract$SIDV) <-
  list("1"=c("SINGLE AWARD"), 
       "0"=c("Def/Pur","MULTIPLE AWARD","Other IDV"))
contract$SIDV<-as.integer(as.character(contract$SIDV))

#MIDV
contract$MIDV<-contract$Veh
levels(contract$MIDV) <-
  list("1"=c("MULTIPLE AWARD"), 
       "0"=c("Def/Pur","SINGLE AWARD", "Other IDV"))
contract$MIDV<-as.integer(as.character(contract$MIDV))

#OIDV
contract$OIDV<-contract$Veh
levels(contract$OIDV) <-
  list("1"=c("Other IDV"), 
       "0"=c("Def/Pur","SINGLE AWARD", "MULTIPLE AWARD"))
contract$OIDV<-as.integer(as.character(contract$OIDV))

#NAICS
load("annual_naics_summary.Rdata")
contract$NAICS<-as.integer(as.character(contract$NAICS))
contract<-left_join(contract,NAICS_join, by=c("StartFY"="StartFY",
                                                           "NAICS"))



contract$c_hh_index_lag1<-scale(contract$hh_index_lag1)
contract$cl_Ceil<-scale(contract$l_Ceil)
contract$cl_Days<-scale(contract$l_Days)
contract$clsqr_Ceil<-contract$cl_Ceil^2
contract$lsqr_Ceil<-contract$l_Ceil^2

contract$clsqr_Days<-contract$cl_Days^2
contract$lsqr_Days<-contract$l_Days^2


contract
}


#From Gelman and Hill
jitter.binary<-function(a, jitt=0.05){
  ifelse(a==0,runif(length(a),0,jitt),runif(length(a),1-jitt,1))
}

fit_curve<-function(x, a, b){invlogit(b *  x +a)}


bin_df<-function(data,rank_col,group_col=NULL,n=20,ties.method="random"){
  #https://stats.stackexchange.com/questions/34008/how-does-ties-method-argument-of-rs-rank-function-work
  if(!is.null(group_col)){
    # Convert character vector to list of symbols
    dots <- lapply(group_col, as.symbol)
    
    # Group by
    data %>%
      group_by_(.dots=dots) 
  }
  #Calculate rank, this allows cut_number to work even when some answers have to be broken up into multiple bins
  bin<-rank(as.data.frame(data[,which(colnames(data)==rank_col)]),ties.method=ties.method)
  cut_number(bin,n)
}

# bin_plot<-function(data,x_col,y_col,group_col=NULL,n=20,ties.method="random")
# 
# 
# data$bin_plot<-bin_df(data,rank_col=x_col,group_col=group_col)
# data<-data[,!is.na(data[,colnames(data)==x_col]) &
#              !is.na(data[,colnames(data)==y_col])
#            !is.na(data[,colnames(data)==group_col])
#            ]
# dots <- lapply(c(x_col,y_col,group_col), as.symbol)
# 
# # Group by
# data %>%
#   group_by_(.dots=dots) 
# 
# 
# 
# 
# 
# Term_01D_line_FxCb<-ggplot(data=subset(Term_smp,!is.na(l_Offr) & !is.na(FxCb)) %>% 
#                              group_by(bin_Offer_FxCb,FxCb) %>% 
#                              summarise (mean_Term = mean(b_Term),
#                                         mean_l_Offr =mean(l_Offr)),
#                            aes(y=mean_Term,x=mean_l_Offr))+geom_point()+facet_wrap(~FxCb)
# 
# }

#From Gelman and Hill
#http://www.stat.columbia.edu/~gelman/arm/software/
binned.resids <- function (x, y, nclass=sqrt(length(x))){
  breaks.index <- floor(length(x)*(1:(nclass-1))/nclass)
  breaks <- c (-Inf, sort(x)[breaks.index], Inf)
  output <- data.frame(xbar=double(),
                       ybar=double(),
                       n=integer(), 
                       x.lo=double(),
                       x.hi=double(),
                       se2=double())
  xbreaks <- NULL
  x.binned <- as.numeric (cut (x, breaks))
  for (i in 1:nclass){
    items <- (1:length(x))[x.binned==i]
    x.range <- range(x[items])
    xbar <- mean(x[items])
    ybar <- mean(y[items])
    n <- length(items)
    sdev <- sd(y[items])
    output <- rbind (output, data.frame(xbar=xbar,
                                           ybar=ybar,
                                           n=n, 
                                           x.lo=x.range[1],
                                           x.hi=x.range[2],
                                           se2=2*sdev/sqrt(n)))
  }
  # colnames (output) <- c ("xbar", "ybar", "n", "x.lo", "x.hi", "se2")
  return (list (binned=output, xbreaks=xbreaks))
}

binned_fitted_versus_residuals<-function(model){
  
  #Save this for a future GLM
  # Term_data_01A<-data.frame(fitted=fitted(Term_01A),
  #                        residuals=residuals(Term_01A),
  #                        nTerm=Term_01A@frame$nTerm,
  #                        cb_Comp=Term_01A@frame$cb_Comp
  #                        )
  
  if(class(model)=="glmerMod")
  {
    data <-data.frame(
      fitted=fitted(model),
      residuals=residuals(model),
      b_Term=model@frame$b_Term
    )
    
  }
  else
  {
  data <-data.frame(
    fitted=fitted(model),
    residuals=residuals(model),
    b_Term=model$model$b_Term
  )
  }

  data$bin_fitted<-bin_df(data,rank_col="fitted")
  
  data<-subset(data,!is.na(fitted) & !is.na(residuals) )
  
  ggplot(data= data %>% 
           group_by(bin_fitted) %>% 
           summarise (mean_Term = mean(b_Term),
                      mean_fitted =mean(fitted)),
         aes(y=mean_Term,x=mean_fitted))+geom_point() +
    labs(title="Binned Fitted Linear Model",           caption="Source: FPDS, CSIS Analysis")
}

residuals_term_plot<-function(model,x_col="fitted",bins=40){
  #Plot the fitted values vs actual results
  
  
  if(class(model)=="glmerMod")
  {
    data <-data.frame(
      fitted=fitted(model),
      residuals=residuals(model),
      b_Term=model@frame$b_Term
    )
    
  }
  else
  {
    data <-data.frame(
      fitted=fitted(model),
      residuals=residuals(model),
      b_Term=model$model$b_Term
    )
  }
  
  
  if (x_col!="fitted"){
    data$x_col<-
      test<-model$model[,x_col]
    colnames(data)[colnames(data)=="x_col"]<-x_col
  }
  
  
  data<-binned.resids (data[,x_col],
                       data$fitted-data$b_Term, nclass=bins)$binned

   ggplot(data=data,
         aes(x=xbar,y-ybar))+
    geom_point(aes(y=ybar))+ #Residuals
    geom_line(aes(y=se2),col="grey")+
    geom_line(aes(y=-se2),col="grey")+
    labs(title="Binned residual plot",
         y="Average residual")

  
}

freq_discrete_term_plot<-function(data,x_col,group_col=NA){
  if(is.na(group_col)){
    plot<-ggplot(data=data,
           aes_string(x=x_col))+
    facet_wrap(~Term,ncol=1,scales="free_y")
  }
  else{
    plot<-ggplot(data=data,
           aes_string(x=x_col))+
      facet_grid(as.formula(paste("Term~",group_col)),scales="free_y")
  }
  plot+geom_histogram(stat="count") +
    scale_y_continuous(labels = scales::comma) +
    labs(title="Frequency by Termination",
            caption="Source: FPDS, CSIS Analysis")
  
}


freq_discrete_crai_plot<-function(data,x_col,group_col=NA){
  if(is.na(group_col)){
    plot<-ggplot(data=data,
                 aes_string(x=x_col))+
      facet_wrap(~b_CBre,ncol=1,scales="free_y")
  }
  else{
    plot<-ggplot(data=data,
                 aes_string(x=x_col))+
      facet_grid(as.formula(paste("b_CBre~",group_col)),scales="free_y")
  }
  plot+geom_histogram(stat="count") +
    scale_y_continuous(labels = scales::comma) +
    labs(title="Frequency by Ceiling Breaches",
         caption="Source: FPDS, CSIS Analysis")
  
}

freq_continuous_term_plot<-function(data,x_col,group_col=NA,bins=20){
  if(is.na(group_col)){
    plot<-ggplot(data=data,
         aes_string(x=x_col))+
      facet_wrap(~Term,ncol=1,scales="free_y")
  }
  else{
    plot<-ggplot(data=data,
           aes_string(x=x_col))+geom_histogram(bins=bins) +
      facet_grid(as.formula(paste("Term~",group_col)),scales="free_y")
      
  }
  plot+labs(title="Frequency by Termination",
            caption="Source: FPDS, CSIS Analysis")+
    scale_y_continuous(labels = scales::comma) + 
    geom_histogram(bins=bins) 
}


binned_percent_term_plot<-function(data,x_col,group_col=NA){
  data<-data[!is.na(data[,x_col]),]
  if(is.na(group_col)){
    data$bin_x<-bin_df(data,x_col)
    plot<-ggplot(data=data %>%
           group_by(bin_x) %>%
           summarise_ (   mean_Term = "mean(b_Term)"   
                          , mean_x =  paste( "mean(" ,  x_col  ,")"  ))     ,
         aes(y=mean_Term,x=mean_x))
  }
  else{
    data<-data[!is.na(data[,group_col]),]
    data$bin_x<-bin_df(data,rank_col=x_col,group_col=group_col)
    plot<-ggplot(data=data %>%
                   group_by_("bin_x",group_col) %>%
                   summarise_ (   mean_Term = "mean(b_Term)"   
                                  , mean_x =  paste( "mean(" ,  x_col  ,")"  ))     ,
                 aes(y=mean_Term,x=mean_x))+
      facet_wrap(as.formula(paste("~",group_col)))
  }
  plot+geom_point()+
    labs(title="Percent Terminated",
         caption="Source: FPDS, CSIS Analysis")
}



binned_percent_crai_plot<-function(data,x_col,group_col=NA){
  data<-data[!is.na(data[,x_col]),]
  if(is.na(group_col)){
    data$bin_x<-bin_df(data,x_col)
    plot<-ggplot(data=data %>%
                   group_by(bin_x) %>%
                   summarise_ (   mean_Crai = "mean(b_CBre)"   
                                  , mean_x =  paste( "mean(" ,  x_col  ,")"  ))     ,
                 aes(y=mean_Crai,x=mean_x))
  }
  else{
    data<-data[!is.na(data[,group_col]),]
    data$bin_x<-bin_df(data,rank_col=x_col,group_col=group_col)
    plot<-ggplot(data=data %>%
                   group_by_("bin_x",group_col) %>%
                   summarise_ (   mean_Crai = "mean(b_CBre)"   
                                  , mean_x =  paste( "mean(" ,  x_col  ,")"  ))     ,
                 aes(y=mean_Crai,x=mean_x))+
      facet_wrap(as.formula(paste("~",group_col)))
  }
  plot+geom_point()+
    labs(title="Percent Ceiling Breaches",
         caption="Source: FPDS, CSIS Analysis")
}

discrete_percent_term_plot<-function(data,x_col,group_col=NA){
  data<-data[!is.na(data[,x_col]),]
    if(is.na(group_col)){
    plot<-ggplot(data=data %>%
             group_by_(x_col) %>%
             summarise (   mean_Term = mean(b_Term)),
           aes_string(y="mean_Term",x=x_col))
      
  }
  else{
    data<-data[!is.na(data[,group_col]),]
    plot<-ggplot(data=data %>%
                   group_by_(x_col,group_col) %>%
                   summarise (   mean_Term = mean(b_Term)),
                 aes_string(y="mean_Term",x=x_col))+
      facet_wrap(as.formula(paste("~",group_col)))
    
  }
  plot+    geom_point()+
    labs(title="Percent Terminated",
         caption="Source: FPDS, CSIS Analysis")
  
}


discrete_percent_crai_plot<-function(data,x_col,group_col=NA){
  data<-data[!is.na(data[,x_col]) & !is.na(data[,"b_CBre"]),]
  if(is.na(group_col)){
    plot<-ggplot(data=data %>%
                   group_by_(x_col) %>%
                   summarise (   mean_Crai = mean(b_CBre)),
                 aes_string(y="mean_Crai",x=x_col))
    
  }
  else{
    data<-data[!is.na(data[,group_col]),]
    plot<-ggplot(data=data %>%
                   group_by_(x_col,group_col) %>%
                   summarise (   mean_Crai = mean(b_CBre)),
                 aes_string(y="mean_Crai",x=x_col))+
      facet_wrap(as.formula(paste("~",group_col)))
    
  }
  plot+    geom_point()+
    labs(title="Percent Ceiling Breaches",
         caption="Source: FPDS, CSIS Analysis")
  
}

fitted_term_model<-function(data,x_col){
  ggplot(data=data,
         aes_string(y="j_Term",x=x_col))+geom_point(alpha=0.01)+scale_y_sqrt() +
    labs(title="Fitted Linear Model", caption="Source: FPDS, CSIS Analysis")
}

discrete_fitted_term_model<-function(data,x_col){
  ggplot(data=data,
         aes_string(y="j_Term",x=x_col))+geom_jitter(alpha=0.01,height=0)+scale_y_sqrt() +
    labs(title="Fitted Linear Model", caption="Source: FPDS, CSIS Analysis")
}


centered_log_description<-function(x,units=NA){
  xbar<-mean(x,na.rm=TRUE)
    xsd<-sd(x,na.rm=TRUE)
  paste("The variable is centered, by subtracting its mean (",
        format(xbar,digits=3,big.mark=","),
        ") and dividing by its standard deviation (",
        format(xsd,digits=3,big.mark=","),
        "). Values of -1, 0, 1, and 2 correspond to ",
        format(exp(xbar-xsd),digits=2,big.mark=","), ", ",
        format(exp(xbar),digits=2,big.mark=","),", ",
        format(exp(xbar+xsd),digits=2,big.mark=","),", and ",
        format(exp(xbar+2*xsd),digits=2,big.mark=","),
        ifelse(is.na(units),"",paste("",units)),
        " respectively.",sep="")
}


NA_stats<-function(data,col){
  paste("Data is missing for ",
        format(sum(is.na(data[,col]))/nrow(data),digits=3),
        " of records and ",
        format(sum(data$Action.Obligation[is.na(data[,col])],na.rm=TRUE)/
                 sum(data$Action.Obligation,na.rm=TRUE),digits=3),
        " of obligated dollars."
              ,sep="")

}
