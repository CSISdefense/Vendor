#DIIGstat.r
library(arm)
library(dplyr)
library(ggplot2)
#This will likely be folded into CSIS360
#But for now, using it to create and refine functions for regression analysis.


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

binned_fitted_versus_term_residuals<-function(model){
  
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
      b_Term=model@frame$b_Term,
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

binned_fitted_versus_cbre_residuals<-function(model){
  
  #Save this for a future GLM
  # CBre_data_01A<-data.frame(fitted=fitted(CBre_01A),
  #                        residuals=residuals(CBre_01A),
  #                        nCBre=CBre_01A@frame$nCBre,
  #                        cb_Comp=CBre_01A@frame$cb_Comp
  #                        )
  
  if(class(model)=="glmerMod")
  {
    data <-data.frame(
      fitted=fitted(model),
      residuals=residuals(model),
      b_CBre=model@frame$b_CBre,
    )
    
  }
  else
  {
    data <-data.frame(
      fitted=fitted(model),
      residuals=residuals(model),
      b_CBre=model$model$b_CBre
    )
  }
  
  data$bin_fitted<-bin_df(data,rank_col="fitted")
  
  data<-subset(data,!is.na(fitted) & !is.na(residuals) )
  
  ggplot(data= data %>% 
           group_by(bin_fitted) %>% 
           summarise (mean_CBre = mean(b_CBre),
                      mean_fitted =mean(fitted)),
         aes(y=mean_CBre,x=mean_fitted))+geom_point() +
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

residuals_cbre_plot<-function(model,x_col="fitted",bins=40){
  #Plot the fitted values vs actual results
  
  
  if(class(model)=="glmerMod")
  {
    data <-data.frame(
      fitted=fitted(model),
      residuals=residuals(model),
      b_CBre=model@frame$b_CBre
    )
    
  }
  else
  {
    data <-data.frame(
      fitted=fitted(model),
      residuals=residuals(model),
      b_CBre=model$model$b_CBre
    )
  }
  
  
  if (x_col!="fitted"){
    data$x_col<-
      test<-model$model[,x_col]
    colnames(data)[colnames(data)=="x_col"]<-x_col
  }
  
  
  data<-binned.resids (data[,x_col],
                       data$fitted-data$b_CBre, nclass=bins)$binned
  
  ggplot(data=data,
         aes(x=xbar,y-ybar))+
    geom_point(aes(y=ybar))+ #Residuals
    geom_line(aes(y=se2),col="grey")+
    geom_line(aes(y=-se2),col="grey")+
    labs(title="Binned residual plot",
         y="Average residual")
  
  
}


freq_discrete_term_plot<-function(data,x_col,
                                  group_col=NA,
                                  na_remove=FALSE){
  
  if(na_remove==TRUE){
    data<-data[!is.na(data[,group_col]),]
    data<-data[!is.na(data[,x_col]),]
  }
  
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


freq_discrete_cbre_plot<-function(data,x_col,
                                  group_col=NA,
                                  na_remove=FALSE){
  
  if(na_remove==TRUE){
    data<-data[!is.na(data[,group_col]),]
    data<-data[!is.na(data[,x_col]),]
  }
  
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


freq_continuous_cbre_plot<-function(data,x_col,group_col=NA,bins=20){
  if(is.na(group_col)){
    plot<-ggplot(data=data,
                 aes_string(x=x_col))+
      facet_wrap(~CBre,ncol=1,scales="free_y")
  }
  else{
    plot<-ggplot(data=data,
                 aes_string(x=x_col))+geom_histogram(bins=bins) +
      facet_grid(as.formula(paste("CBre~",group_col)),scales="free_y")
    
  }
  plot+labs(title="Frequency by Ceiling Breach",
            caption="Source: FPDS, CSIS Analysis")+
    scale_y_continuous(labels = scales::comma) + 
    geom_histogram(bins=bins) 
}


binned_percent_term_plot<-function(data,x_col,group_col=NA,bins=20){
  data<-data[!is.na(data[,x_col]),]
  if(is.na(group_col)){
    data$bin_x<-bin_df(data,x_col,n=bins)
    plot<-ggplot(data=data %>%
           group_by(bin_x) %>%
           summarise_ (   mean_Term = "mean(b_Term)"   
                          , mean_x =  paste( "mean(" ,  x_col  ,")"  ))     ,
         aes(y=mean_Term,x=mean_x))
  }
  else{
    data<-data[!is.na(data[,group_col]),]
    data$bin_x<-bin_df(data,rank_col=x_col,group_col=group_col,n=bins)
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



binned_percent_cbre_plot<-function(data,x_col,group_col=NA,bins=20){
  data<-data[!is.na(data[,x_col]),]
  if(is.na(group_col)){
    data$bin_x<-bin_df(data,x_col,n=bins)
    plot<-ggplot(data=data %>%
                   group_by(bin_x) %>%
                   summarise_ (   mean_CBre = "mean(b_CBre)"   
                                  , mean_x =  paste( "mean(" ,  x_col  ,")"  ))     ,
                 aes(y=mean_CBre,x=mean_x))
  }
  else{
    data<-data[!is.na(data[,group_col]),]
    data$bin_x<-bin_df(data,rank_col=x_col,group_col=group_col,n=bins)
    plot<-ggplot(data=data %>%
                   group_by_("bin_x",group_col) %>%
                   summarise_ (   mean_CBre = "mean(b_CBre)"   
                                  , mean_x =  paste( "mean(" ,  x_col  ,")"  ))     ,
                 aes(y=mean_CBre,x=mean_x))+
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


discrete_percent_cbre_plot<-function(data,x_col,group_col=NA){
  data<-data[!is.na(data[,x_col]) & !is.na(data[,"b_CBre"]),]
  if(is.na(group_col)){
    plot<-ggplot(data=data %>%
                   group_by_(x_col) %>%
                   summarise (   mean_CBre = mean(b_CBre)),
                 aes_string(y="mean_CBre",x=x_col))
    
  }
  else{
    data<-data[!is.na(data[,group_col]),]
    plot<-ggplot(data=data %>%
                   group_by_(x_col,group_col) %>%
                   summarise (   mean_CBre = mean(b_CBre)),
                 aes_string(y="mean_CBre",x=x_col))+
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

fitted_cbre_model<-function(data,x_col){
  ggplot(data=data,
         aes_string(y="j_CBre",x=x_col))+geom_point(alpha=0.01)+scale_y_sqrt() +
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



centered_description<-function(x,units=NA){
  xbar<-mean(x,na.rm=TRUE)
  xsd<-sd(x,na.rm=TRUE)
  paste("The variable is centered, by subtracting its mean (",
        format(xbar,digits=3,big.mark=","),
        ") and dividing by its standard deviation (",
        format(xsd,digits=3,big.mark=","),
        "). Values of -1, 0, 1, and 2 correspond to ",
        format(xbar-xsd,digits=2,big.mark=","), ", ",
        format(xbar,digits=2,big.mark=","),", ",
        format(xbar+xsd,digits=2,big.mark=","),", and ",
        format(xbar+2*xsd,digits=2,big.mark=","),
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
