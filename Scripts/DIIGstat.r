#DIIGstat.r
library(arm)
library(dplyr)
library(ggplot2)
library(sjstats)
library(car)
library(scales)
library(grid)
#This will likely be folded into CSIS360
#But for now, using it to create and refine functions for regression analysis.


fit_curve<-function(x, a, b){invlogit(b *  x +a)}


bin_df<-function(data,rank_col,group_col=NULL,bins=20,ties.method="random"){
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
  cut_number(bin,bins)
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





freq_discrete_term_plot<-function(data,x_col,
                                  group_col=NA,
                                  na_remove=FALSE,
                                  caption=TRUE,rotate_text=FALSE){
  
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
  if(caption==TRUE){
    plot<-plot+labs(caption="Source: FPDS, CSIS Analysis")
  }
  
  if(rotate_text==TRUE){
    plot<-plot+theme(axis.text.x=element_text(angle=90,hjust=1))
  }
  
  plot+labs(title="Frequency by Termination")+
    geom_histogram(stat="count") +
    scale_y_continuous(labels = scales::comma)
}




freq_discrete_cbre_plot<-function(data,x_col,
                                  group_col=NA,
                                  na_remove=FALSE,
                                  caption=TRUE,
                                  rotate_text=FALSE){
  
  if(na_remove==TRUE){
    data<-data[!is.na(data[,group_col]),]
    data<-data[!is.na(data[,x_col]),]
  }
  
  if(is.na(group_col)){
    plot<-ggplot(data=data,
                 aes_string(x=x_col))
    # +
    # facet_wrap(~b_CBre,ncol=1,scales="free_y")
  }
  else{
    plot<-ggplot(data=data,
                 aes_string(x=x_col))
    # +
    # facet_grid(as.formula(paste("b_CBre~",group_col)),scales="free_y")
  }
  if(caption==TRUE){
    plot<-plot+labs(caption="Source: FPDS, CSIS Analysis")
  }
  if(rotate_text==TRUE){
    plot<-plot+theme(axis.text.x=element_text(angle=90,hjust=1))
  }
  
  plot+labs(title="Frequency by Ceiling Breach")+
    geom_histogram(stat="count") +
    scale_y_continuous(labels = scales::comma) 
  
}


freq_discrete_plot<-function(data,x_col,
                             group_col=NA,
                             na_remove=FALSE,
                             caption=TRUE,rotate_text=FALSE){
  
  if(na_remove==TRUE){
    data<-data[!is.na(data[,group_col]),]
    data<-data[!is.na(data[,x_col]),]
  }
  
  if(is.na(group_col)){
    plot<-ggplot(data=data,
                 aes_string(x=x_col))
  }
  else{
    plot<-ggplot(data=data,
                 aes_string(x=x_col))+
      facet_wrap(as.formula(paste("~",group_col)),scales="free_y")
  }
  if(caption==TRUE){
    plot<-plot+labs(caption="Source: FPDS, CSIS Analysis")
  }
  if(rotate_text==TRUE){
    plot<-plot+theme(axis.text.x=element_text(angle=90,hjust=1))
  }
  plot+labs(title="Frequency")+
    geom_histogram(stat="count") +
    scale_y_continuous(labels = scales::comma)
  
}


summary_continuous_plot<-function(data,x_col,group_col=NA,bins=20,metric="perform", log=FALSE){
  if(metric!="none")
    gridExtra::grid.arrange(freq_continuous_plot(data,x_col,group_col,bins=bins,caption=FALSE,log),
                          binned_percent_plot(data,x_col=x_col,group_col=group_col,bins=bins,caption=TRUE,metric=metric,log=log))
  else freq_continuous_plot(data,x_col,group_col,bins=bins,caption=FALSE)
  
}


summary_double_continuous<-function(data,x_col,y_col,bins=20,metric="perform"){
  data<-data[!is.na(data[,y_col]),]
  data<-data[!is.na(data[,x_col]),]
  data<-as.data.frame(data)
  data$interaction<-data[,x_col]*data[,y_col]
  
  
  #First a quick scatter plot for terminations by duration and ceiling
  gridExtra::grid.arrange(
    ggplot(data=data,
           aes_string(x=x_col,y=y_col))+geom_point(alpha=0.1)+
      labs(title="Distribution",
           caption="Source: FPDS, CSIS Analysis"),
    freq_continuous_plot(data,"interaction",bins=bins,caption=FALSE))
  
  
  
  #First a quick scatter plot for terminations by duration and ceiling
  if(metric=="perform"){
    gridExtra::grid.arrange(ggplot(data=data,
                                   aes_string(x=x_col,y=y_col))+geom_point(alpha=0.1)+facet_grid(CBre~.)+
                              labs(title="Distribution by Breach",
                                   caption="Source: FPDS, CSIS Analysis"),
                            
                            
                            #First a quick scatter plot for terminations by duration and ceiling
                            ggplot(data=data,
                                   aes_string(x=x_col,y=y_col))+geom_point(alpha=0.1)+facet_grid(Term~.)+
                              labs(title="Distribution by Termination"),
                            
                            ncol=2
                            
    )
  } else if (metric=="opt"){
    gridExtra::grid.arrange(ggplot(data=data,
                                   aes_string(x=x_col,y=y_col))+geom_point(alpha=0.1)+facet_grid(b_SomeOpt~.)+
                              labs(title="Distribution by Some Options",
                                   caption="Source: FPDS, CSIS Analysis"),
                            
                            
                            #First a quick scatter plot for terminations by duration and ceiling
                            ggplot(data=data %>% filter(b_SomeOpt==1),
                                   aes_string(x=x_col,y=y_col))+geom_point(alpha=0.1)+facet_grid(b_AllOpt~.)+
                              labs(title="Distribution by All Options"),
                            
                            ncol=2
    )
  #If none, nothing else to add.
    } else if (metric != "none") stop(paste("Unknown metric:",metric))
  
  
  binned_double_percent_plot(data,x_col,y_col,bins,metric=metric)
  # min_i<-min(data[,"interaction"])
  # max_i<-max(data[,"interaction"])
  # 
  # gridExtra::grid.arrange(binned_percent_plot(data,x_col,caption=FALSE)+xlim(min_i,max_i),
  #                         binned_percent_plot(data,y_col,caption=FALSE)+xlim(min_i,max_i),
  #                         binned_percent_plot(data,"interaction",caption=TRUE)+xlim(min_i,max_i))
  
}

summary_discrete_plot<-function(data,x_col,group_col=NA,rotate_text=FALSE,metric="perform"){
  if(is.na(group_col)){
    if (metric=="opt")
      output<-list(table(unlist(data[,x_col])),
                   table(unlist(data[,x_col]),data$b_SomeOpt),
                   table(unlist(data[,x_col]),data$b_AllOpt))
    else
      output<-list(table(unlist(data[,x_col])),
                   table(unlist(data[,x_col]),data$CBre),
                   table(unlist(data[,x_col]),data$Term))
    
    
  }
  else{
    if(is.numeric(data[,x_col]) & length(unique(data[,x_col]))>=10) stop(paste(x_col," is numeric with over ten distinct values."))
    if (metric=="opt")
      output<-list(table(unlist(data[,x_col]),unlist(data[,group_col])),
                   table(unlist(data[,x_col]),unlist(data[,group_col]),data$b_SomeOpt),
                   table(unlist(data[,x_col]),unlist(data[,group_col]),data$b_AllOpt))  
    else
    output<-list(table(unlist(data[,x_col]),unlist(data[,group_col])),
                 table(unlist(data[,x_col]),unlist(data[,group_col]),data$CBre),
                 table(unlist(data[,x_col]),unlist(data[,group_col]),data$Term))
    
  }
  
  gridExtra::grid.arrange(freq_discrete_plot(data,x_col,group_col,caption=FALSE,rotate_text=rotate_text),
                          discrete_percent_plot(data,x_col,group_col,caption=TRUE,rotate_text=rotate_text,metric=metric))
  
  output
}


freq_continuous_term_plot<-function(data,x_col,group_col=NA,bins=20,
                                    caption=TRUE){
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
  
  if(caption==TRUE){
    plot<-plot+labs(caption="Source: FPDS, CSIS Analysis")
  }
  plot+labs(title="Frequency by Termination")+
    scale_y_continuous(labels = scales::comma) + 
    geom_histogram(bins=bins) 
}

freq_continuous_plot<-function(data,x_col,group_col=NA,bins=20,
                               caption=TRUE, log=FALSE, denominator=NULL, plus1=FALSE,cap1=FALSE){
  
  if(!x_col %in% colnames(data)) stop(paste(x_col,"is not found in data."))
  
  if(!is.null(denominator)){
    if(!denominator %in% colnames(data)) stop(paste(denominator,"is not found in data."))
    data$numerator<-data[,x_col]
    data[,x_col]<-data[,x_col]/data[,denominator]
    if(cap1==TRUE)
      data[data[,x_col]>1 & !is.na(data[,x_col]),x_col]<-1
  }
  
  if(plus1==TRUE){
    if(log==FALSE) warning("The variable is not being logged. Are you sure you want to add one?")
    data[,x_col]<-data[,x_col]+1
    if(!is.null(denominator)){
      data$numerator<-data$numerator+1
    }
  }
    
  if(is.na(group_col)){
    plot<-ggplot(data=data,
                 aes_string(x=x_col))
  }
  else{
    plot<-ggplot(data=data,
                 aes_string(x=x_col))+geom_histogram(bins=bins)+
      facet_wrap(as.formula(paste("~",group_col)),scales="free_y")
    
  }
  
  if(caption==TRUE){
    plot<-plot+labs(caption="Source: FPDS, CSIS Analysis")
  }
  
  if(!is.null(denominator)){
    if(is.na(group_col)){
      numer_plot<-ggplot(data=data,
                   aes_string(x="numerator"))
    }
    else{
      numer_plot<-ggplot(data=data,
                   aes_string(x="numerator"))+geom_histogram(bins=bins)+
        facet_wrap(as.formula(paste("~",group_col)),scales="free_y")
      
    }
    
    if(caption==TRUE){
      numer_plot<-numer_plot+labs(caption="Source: FPDS, CSIS Analysis")
    }
    
    
    
  }
  
  plot<-plot+labs(title="Frequency")+
    scale_y_continuous(labels = scales::comma) + 
    geom_histogram(bins=bins) 
  
  
  if(is.null(denominator)){
    if(log==TRUE){
      gridExtra::grid.arrange(plot,plot+scale_x_log10(),
                              ncol=2)#labels = scales::comma
    } else{ plot
    }
    
  } else {
    numer_plot<-numer_plot+
      labs(title="Numerator Frequency")+
      scale_y_continuous(labels = scales::comma) + 
      geom_histogram(bins=bins)
    plot<-plot+labs(title="Post-Division Frequency")
    if(log==TRUE){
    gridExtra::grid.arrange(
                 numer_plot,numer_plot+scale_x_log10(),
               plot ,plot+scale_x_log10() ,
               ncol=2
                 )
    
    }
    else{
      gridExtra::grid.arrange(
        numer_plot,
        plot 
      )
      
    }
  }
}


freq_continuous_cbre_plot<-function(data,x_col,group_col=NA,bins=20,
                                    caption=TRUE){
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
  
  if(caption==TRUE){
    plot<-plot+labs(caption="Source: FPDS, CSIS Analysis")
  }
  plot+labs(title="Frequency by Ceiling Breach")+
    scale_y_continuous(labels = scales::comma) + 
    geom_histogram(bins=bins) 
}


binned_percent_plot<-function(data,x_col,group_col=NA,bins=20,caption=TRUE,metric="perform",log=FALSE){
  data<-data[!is.na(data[,x_col]),]
  if(is.na(group_col)){
    data$bin_x<-bin_df(data,x_col,bins=bins)
    data<-data %>% group_by(bin_x)
    
    
    if(metric=="perform"){
      cbre<-data %>% summarise_ (   mean_y = "mean(b_CBre)"   
                                    , mean_x =  paste( "mean(" ,  x_col  ,")"  ))  
      term<-data %>% summarise_ (   mean_y = "mean(b_Term)"   
                                    , mean_x =  paste( "mean(" ,  x_col  ,")"  ))  
      term$output<-"Terminations"
      cbre$output<-"Ceiling Breaches"
      data<-rbind(cbre,term)
      data$output<-factor(data$output,c("Ceiling Breaches","Terminations"))  
    }
    else if(metric=="opt"){
      some<-data %>% summarise_ (   mean_y = "mean(b_SomeOpt)"   
                                    , mean_x =  paste( "mean(" ,  x_col  ,")"  ))  
      all<-data %>% filter(b_SomeOpt==1) %>% summarise_ (   mean_y = "mean(b_AllOpt)"   
                                    , mean_x =  paste( "mean(" ,  x_col  ,")"  ))  
      some$output<-"P(Some Options)"
      all$output<-"P(All Options|Some Options)"
      data<-rbind(some,all)
      data$output<-factor(data$output,c("P(Some Options)","P(All Options|Some Options)"))  
       
      
    }
    else if (metric=="comp"){
      comp<-data %>% summarise_ (   mean_y = "mean(b_Comp)"   
                                    , mean_x =  paste( "mean(" ,  x_col  ,")"  ))  
      offer<-data %>% summarise_ (   mean_y = "mean(l_Offr)"   
                                     , mean_x =  paste( "mean(" ,  x_col  ,")"  ))  
      comp$output<-"Competed"
      offer$output<-"Offers (logged)"
      data<-rbind(comp,offer)
      data$output<-factor(data$output,c("Competed","Offers (logged)"))  
    }
    else if (metric=="cbre"){
      b_CBre<-data %>% summarise_ (   mean_y = "mean(b_CBre)"
                                    , mean_x =  paste( "mean(" ,  x_col  ,")"  ))
      ln_CBre<-data %>% summarise_ (   mean_y = get_n_CBre_mean(data) 
                                     , mean_x =  paste( "mean(" ,  x_col  ,")"  ))
      scatter_CBre<-data[data$b_CBre==1,colnames(data) %in% c("ln_CBre","ln_CBre_OMB20_GDP18",x_col)]
      colnames(scatter_CBre)[colnames(scatter_CBre) %in% c("ln_CBre","ln_CBre_OMB20_GDP18")]<-"mean_y"
      colnames(scatter_CBre)[colnames(scatter_CBre)==x_col]<-"mean_x"
      scatter_CBre$bin_x<-0
      b_CBre$output<-"Breach Occured"
      ln_CBre$output<-"Average Size (logged)"
      scatter_CBre$output<-"Breach Scatterplot (logged)"
      #For the def_breach dataset, there are no unbreached contracts, making this graph unhelpful.
      if(any(b_CBre$mean_y<1)) 
        data<-rbind(b_CBre,b_CBre,b_CBre,b_CBre,
                    ln_CBre,ln_CBre,ln_CBre,ln_CBre,
                    scatter_CBre)
      else
        data<-rbind(ln_CBre,ln_CBre,ln_CBre,ln_CBre,
                    scatter_CBre)
      data$output<-factor(data$output,c("Breach Occured","Average Size (logged)","Breach Scatterplot (logged)"))  
    }
    
    plot<-ggplot(data=data,
                 aes(y=mean_y,x=mean_x))+facet_grid(output~.,scales="free_y")
  }
  else{
    data<-data[!is.na(data[,group_col]),]
    data$bin_x<-bin_df(data,rank_col=x_col,group_col=group_col,bins=bins)
    data<-data %>%
      group_by_("bin_x",group_col)
    
    
    if(metric=="perform"){
      cbre<-data %>% summarise_ (   mean_y = "mean(b_CBre)"   
                                    , mean_x =  paste( "mean(" ,  x_col  ,")"  ))  
      term<-data %>% summarise_ (   mean_y = "mean(b_Term)"   
                                    , mean_x =  paste( "mean(" ,  x_col  ,")"  ))  
      term$output<-"Term."
      cbre$output<-"C. Bre."
      data<-rbind(cbre,term)
      data$output<-factor(data$output,c("C. Bre.","Term."))  
    }
    else if (metric=="comp"){
      comp<-data %>% summarise_ (   mean_y = "mean(b_Comp)"   
                                    , mean_x =  paste( "mean(" ,  x_col  ,")"  ))  
      offer<-data %>% summarise_ (   mean_y = "mean(l_Offr)"   
                                     , mean_x =  paste( "mean(" ,  x_col  ,")"  ))  
      comp$output<-"Comp."
      offer$output<-"Offers (logged)"
      data<-rbind(comp,offer)
      data$output<-factor(data$output,c("Comp.","Offers (logged)"))  
    }
    else if (metric=="cbre"){
      
      b_CBre<-data %>% summarise_ (   mean_y = "mean(b_CBre)"
                                      , mean_x =  paste( "mean(" ,  x_col  ,")"  ))
      ln_CBre<-data %>% summarise_ (   mean_y = get_n_CBre_mean(data) 
                                       , mean_x =  paste( "mean(" ,  x_col  ,")"  ))
      scatter_CBre<-data[data$b_CBre==1,colnames(data) %in% c("ln_CBre","ln_CBre_OMB20_GDP18",x_col,group_col)]
      colnames(scatter_CBre)[colnames(scatter_CBre) %in% c("ln_CBre","ln_CBre_OMB20_GDP18")]<-"mean_y"
      colnames(scatter_CBre)[colnames(scatter_CBre)==x_col]<-"mean_x"
      b_CBre<-as.data.frame(b_CBre)
      ln_CBre<-as.data.frame(ln_CBre)
      scatter_CBre<-as.data.frame(scatter_CBre)
      scatter_CBre$bin_x<-0
      b_CBre$output<-"Breach Occured"
      ln_CBre$output<-"Average Size (logged)"
      scatter_CBre$output<-"Breach Scatterplot (logged)"
      if(any(b_CBre$mean_y<1)) 
        data<-rbind(b_CBre,b_CBre,b_CBre,b_CBre,
                    ln_CBre,ln_CBre,ln_CBre,ln_CBre,
                    scatter_CBre)
      else
        data<-rbind(ln_CBre,ln_CBre,ln_CBre,ln_CBre,
                    scatter_CBre)
      data$output<-factor(data$output,c("Breach Occured","Average Size (logged)","Breach Scatterplot (logged)"))  
    }
    
    plot<-ggplot(data=data,
                 aes(y=mean_y,x=mean_x))+
      facet_grid(as.formula(paste("output~",group_col)),scales="free_y")
  }
  if(caption==TRUE){
    plot<-plot+labs(caption="Source: FPDS, CSIS Analysis")
  }
  if(log==TRUE) plot<-plot+scale_x_log10()#labels = scales::comma
  if(metric=="cbre") plot<-plot+geom_point(alpha=0.25)
  else plot<-plot+geom_point()
  plot
}


bin_group<-function(data,bin_col,bins=20,metric="perform"){
  
  data$bin<-bin_df(data,rank_col=bin_col,bins=bins)
  data<-data %>%
    group_by(bin)
  if(metric=="perform"){
    term<-data %>% summarise_ (   mean_y = "mean(b_Term)"   
                                  , mean_x =  paste( "mean(" ,  bin_col  ,")"  ))  
    cbre<-data %>% summarise_ (   mean_y = "mean(b_CBre)"   
                                  , mean_x =  paste( "mean(" ,  bin_col  ,")"  ))  
    term$output<-"Term."
    cbre$output<-"C. Bre."
    data<-rbind(term,cbre)
  } else if (metric=="opt"){
    SomeOpt<-data %>% summarise_ (   mean_y = "mean(b_SomeOpt)"   
                                  , mean_x =  paste( "mean(" ,  bin_col  ,")"  ))  
    AllOpt<-data %>% summarise_ (   mean_y = "mean(b_AllOpt)"   
                                  , mean_x =  paste( "mean(" ,  bin_col  ,")"  ))  
    SomeOpt$output<-"Some Opt."
    AllOpt$output<-"All Opt."
    data<-rbind(SomeOpt,AllOpt)
  }
  data$bin_col<-bin_col
  data
}

binned_double_percent_plot<-function(data,x_col,y_col,bins=20,caption=TRUE,metric="perform"){
  data<-data[!is.na(data[,x_col]),]
  data<-data[!is.na(data[,y_col]),]
  data<-as.data.frame(data)
  data$interaction<-data[,x_col]*data[,y_col]
  data<-rbind(bin_group(data,x_col,bins,metric=metric),
              bin_group(data,y_col,bins,metric=metric),
              bin_group(data,"interaction",bins,metric=metric))
  
  data$bin_col<-factor(data$bin_col,c(x_col,y_col,"interaction"))
  plot<-ggplot(data=data,
               aes(y=mean_y,x=mean_x))+
    facet_grid(output~bin_col,scales="free")
  
  if(caption==TRUE){
    plot<-plot+labs(caption="Source: FPDS, CSIS Analysis")
  }
  plot+geom_point()
}


binned_percent_term_plot<-function(data,x_col,group_col=NA,bins=20,caption=TRUE){
  data<-data[!is.na(data[,x_col]),]
  if(is.na(group_col)){
    data$bin_x<-bin_df(data,x_col,bins=bins)
    plot<-ggplot(data=data %>%
                   group_by(bin_x) %>%
                   summarise_ (   mean_Term = "mean(b_Term)"   
                                  , mean_x =  paste( "mean(" ,  x_col  ,")"  ))     ,
                 aes(y=mean_Term,x=mean_x))
  }
  else{
    data<-data[!is.na(data[,group_col]),]
    data$bin_x<-bin_df(data,rank_col=x_col,group_col=group_col,bins=bins)
    plot<-ggplot(data=data %>%
                   group_by_("bin_x",group_col) %>%
                   summarise_ (   mean_Term = "mean(b_Term)"   
                                  , mean_x =  paste( "mean(" ,  x_col  ,")"  ))     ,
                 aes(y=mean_Term,x=mean_x))+
      facet_wrap(as.formula(paste("~",group_col)))
  }
  if(caption==TRUE){
    plot<-plot+labs(caption="Source: FPDS, CSIS Analysis")
  }
  plot+geom_point()+
    labs(title="Percent Terminated")
}



binned_percent_cbre_plot<-function(data,x_col,group_col=NA,bins=20,caption=TRUE){
  data<-data[!is.na(data[,x_col]),]
  if(is.na(group_col)){
    data$bin_x<-bin_df(data,x_col,bins=bins)
    plot<-ggplot(data=data %>%
                   group_by(bin_x) %>%
                   summarise_ (   mean_CBre = "mean(b_CBre)"   
                                  , mean_x =  paste( "mean(" ,  x_col  ,")"  ))     ,
                 aes(y=mean_CBre,x=mean_x))
  }
  else{
    data<-data[!is.na(data[,group_col]),]
    data$bin_x<-bin_df(data,rank_col=x_col,group_col=group_col,bins=bins)
    plot<-ggplot(data=data %>%
                   group_by_("bin_x",group_col) %>%
                   summarise_ (   mean_CBre = "mean(b_CBre)"   
                                  , mean_x =  paste( "mean(" ,  x_col  ,")"  ))     ,
                 aes(y=mean_CBre,x=mean_x))+
      facet_wrap(as.formula(paste("~",group_col)))
  }
  if(caption==TRUE){
    plot<-plot+labs(caption="Source: FPDS, CSIS Analysis")
  }
  plot+geom_point()+
    labs(title="Percent Ceiling Breaches")
}

discrete_percent_term_plot<-function(data,x_col,group_col=NA,caption=TRUE){
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
  if(caption==TRUE){
    plot<-plot+labs(caption="Source: FPDS, CSIS Analysis")
  }
  plot+geom_point()+
    labs(title="Percent Terminated")
  
}

discrete_percent_cbre_plot<-function(data,x_col,group_col=NA,caption=TRUE){
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
  
  if(caption==TRUE){
    plot<-plot+labs(caption="Source: FPDS, CSIS Analysis")
  }
  plot+geom_point()+
    labs(title="Percent Ceiling Breaches")
  
}

get_n_CBre_mean<-function(data){
  cbre_var<-"ln_CBre"
  if(!cbre_var %in% colnames(data)){
    if("ln_CBre_OMB20_GDP18" %in% colnames(data)) cbre_var<-"ln_CBre_OMB20_GDP18"
    else stop("ln_CBre is missing")
  }
  return(paste("mean(",cbre_var,",na.rm=TRUE)"))
}

discrete_percent_plot<-function(data,x_col,group_col=NA,bins=20,caption=TRUE,rotate_text=FALSE,metric="perform"){
  data<-data[!is.na(data[,x_col]),]
  if(is.na(group_col)){
    data<-data %>% group_by_(x_col)
    
    
    
    
    if(metric=="perform"){
      cbre<-data %>% summarise_ (   mean_y = "mean(b_CBre)"   )
      term<-data %>% summarise_ (   mean_y = "mean(b_Term)"   )
      term$output<-"Terminations"
      cbre$output<-"Ceiling Breaches"
      data<-rbind(cbre,term)
      data$output<-factor(data$output,c("Ceiling Breaches","Terminations"))  
    }
    
    else if (metric=="opt"){
      some<-data %>% summarise_ (   mean_y = "mean(b_SomeOpt)"   )
      all<-data %>% filter(b_SomeOpt==1) %>% summarise_ (   mean_y = "mean(b_AllOpt)"   )
      some$output<-"P(Some Options)"
      all$output<-"P(All Options|Some Options)"
      data<-rbind(some,all)
      data$output<-factor(data$output,c("P(Some Options)","P(All Options|Some Options)"))  
    }
    else if (metric=="comp"){
      comp<-data %>% summarise_ (   mean_y = "mean(b_Comp)"   )  
      offer<-data %>% summarise_ (   mean_y = "mean(l_Offr)"   )  
      comp$output<-"Competed"
      offer$output<-"Offers (logged)"
      data<-rbind(comp,offer)
      data$output<-factor(data$output,c("Competed","Offers (logged)"))  
    }
    else if (metric=="cbre"){
      b_CBre<-data %>% summarise_ (   mean_y = "mean(b_CBre)"   )  
      ln_CBre<-data %>% summarise_ (   mean_y =  get_n_CBre_mean(data)  )  
      b_CBre$output<-"Breach Occured"
      ln_CBre$output<-"Breach Size (logged)"
      #For the def_breach dataset, there are no unbreached contracts, making this graph unhelpful.
      if(any(b_CBre$mean_y<1)) 
        data<-rbind(b_CBre,ln_CBre)
      else
        data<-ln_CBre
      data$output<-factor(data$output,c("Breach Occured","Breach Size (logged)"))  
    }
    
    plot<-ggplot(data=data,
                 aes_string(y="mean_y",x=x_col))
    if (metric=="cbre") plot<-plot+facet_grid(output~.,scales="free_y")
    else plot<-plot+facet_wrap(~output)
  }
  else{
    data<-data[!is.na(data[,group_col]),]
    data<-data %>%
      group_by_(x_col,group_col)
    
    if(metric=="perform"){
      cbre<-data %>% summarise_ (   mean_y = "mean(b_CBre)"   )  
      term<-data %>% summarise_ (   mean_y = "mean(b_Term)"   )  
      term$output<-"Term."
      cbre$output<-"C. Bre."
      data<-rbind(cbre,term)
      data$output<-factor(data$output,c("C. Bre.","Term."))  
    }
    else if (metric=="comp"){
      comp<-data %>% summarise_ (   mean_y = "mean(b_Comp)"   )  
      offer<-data %>% summarise_ (   mean_y = "mean(l_Offr)"  )  
      comp$output<-"Comp."
      offer$output<-"Offers (logged)"
      data<-rbind(comp,offer)
      data$output<-factor(data$output,c("Comp.","Offers (logged)"))  
    }
    else if (metric=="cbre"){
      b_CBre<-data %>% summarise_ (   mean_y = "mean(b_CBre)"   )  
      ln_CBre<-data %>% summarise_ (   mean_y = get_n_CBre_mean(data)    )  
      b_CBre$output<-"Breach Occured"
      ln_CBre$output<-"Breach Size (logged)"
      #For the def_breach dataset, there are no unbreached contracts, making this graph unhelpful.
      if(any(b_CBre$mean_y<1)) 
        data<-rbind(b_CBre,ln_CBre)
      else
        data<-ln_CBre
      data$output<-factor(data$output,c("Breach Occured","Breach Size (logged)"))  
    }
    
    
    plot<-ggplot(data=data,
                 aes_string(y="mean_y",x=x_col))+
      facet_grid(as.formula(paste("output~",group_col)),scales="free_y")+
      theme(axis.text.x=element_text(angle=90,hjust=1))
  }
  if(caption==TRUE){
    plot<-plot+labs(caption="Source: FPDS, CSIS Analysis")
  }
  if(rotate_text==TRUE){
    plot<-plot+theme(axis.text.x=element_text(angle=90,hjust=1))
  }
  
  plot+geom_point()
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


test<-c(1,2,3,4,5)
sd(test)
mean(test)
arm::rescale(test)

na_non_positive_log<-function(x){
  x[x<=0]<-NA
  log(x)
}

centered_log_description<-function(x,units=NA){
  x<-na_non_positive_log(x)
  xbar<-mean(x,na.rm=TRUE)
  xsd<-sd(x,na.rm=TRUE)
  paste("The variable is rescaled, by subtracting its mean (",
        format(xbar,digits=3,big.mark=","),
        ") and dividing by its standard deviation doubled (",
        format(2*xsd,digits=3,big.mark=","),
        "). Values of -1, -0.5, 0, 0.5, and 1 correspond to ",
        format(exp(xbar-2*xsd),digits=2,big.mark=","), ", ",
        format(exp(xbar-xsd),digits=2,big.mark=","), ", ",
        format(exp(xbar),digits=2,big.mark=","),", ",
        format(exp(xbar+xsd),digits=2,big.mark=","),", and ",
        format(exp(xbar+2*xsd),digits=2,big.mark=","),
        ifelse(is.na(units),"",paste("",units)),
        " respectively.",sep="")
}
# Old eversion
# centered_log_description<-function(x,units=NA){
#   xbar<-mean(x,na.rm=TRUE)
#   xsd<-sd(x,na.rm=TRUE)
#   paste("The variable is centered, by subtracting its mean (",
#         format(xbar,digits=3,big.mark=","),
#         ") and dividing by its standard deviation (",
#         format(xsd,digits=3,big.mark=","),
#         "). Values of -1, 0, 1, and 2 correspond to ",
#         format(exp(xbar-xsd),digits=2,big.mark=","), ", ",
#         format(exp(xbar),digits=2,big.mark=","),", ",
#         format(exp(xbar+xsd),digits=2,big.mark=","),", and ",
#         format(exp(xbar+2*xsd),digits=2,big.mark=","),
#         ifelse(is.na(units),"",paste("",units)),
#         " respectively.",sep="")
# }


centered_description<-function(x,units=NA){
  xbar<-mean(x,na.rm=TRUE)
  xsd<-sd(x,na.rm=TRUE)
  paste("The variable is rescaled, by subtracting its mean (",
        format(xbar,digits=3,big.mark=","),
        ") and dividing by its standard deviation doubled(",
        format(2*xsd,digits=3,big.mark=","),
        "). Values of -1, -0.5, 0, 0.5, and 1 correspond to ",
        format(xbar-2*xsd,digits=2,big.mark=","), ", ",
        format(xbar-xsd,digits=2,big.mark=","), ", ",
        format(xbar,digits=2,big.mark=","),", ",
        format(xbar+xsd,digits=2,big.mark=","),", and ",
        format(xbar+2*xsd,digits=2,big.mark=","),
        ifelse(is.na(units),"",paste("",units)),
        " respectively.",sep="")
}


NA_stats<-function(data,col,exclude_before_2008=TRUE,value_col=NULL){
  value_col<-get_value_col(data,value_col)
  if(exclude_before_2008==TRUE) before2008<-data$StartCY<2008
  else before2008<-FALSE
  paste("Data is missing for ",
        format(sum(is.na(data[!before2008,col]))/length(data[!before2008,col]),digits=3),
        " of records and ",
        format(sum(data[[value_col]][is.na(data[!before2008,col])],na.rm=TRUE)/
                 sum(data[[value_col]][!before2008],na.rm=TRUE),digits=3),
        " of obligated dollars."
        ,sep="")
  
}





deviance_stats<-function(model,model_name){
  # if(class(model)[1] %in% c("glmerMod","lmerMod"))
  # {
  #   getME(model,"devcom")$dev
  #   output<-data.frame(model=model_name,
  #                      deviance=model$deviance,
  #                      null.deviance=model$null.deviance,
  #                      difference=model$null.deviance-model$deviance)
  # 
  # }
  # else
  # {
  output<-data.frame(model=model_name,
                     deviance=model$deviance,
                     null.deviance=model$null.deviance,
                     difference=model$null.deviance-model$deviance)
  # }
  output
  
}

model_colnames<-function(model){
  if(class(model)[1] %in% c("glmerMod","lmerMod"))
  { 
    output<-colnames(model@frame)
    
  }
  else
  {
    output<-colnames(model$model)
  }
  output
}




resid_plot<-function(model,sample=NA){
  #Source https://rpubs.com/therimalaya/43190
  # Raju Rimal
  results<-data.frame(
    fitted=fitted(model),
    resid=residuals(model)
  )
  #For reasons of speed, I give the option to only partially show the results. 250k or 1m takes a while to plot.
  if(!is.na(sample) & sample<nrow(results)){
    results<-results[sample(nrow(results),sample),]
    
  }
  
  p1<-ggplot(results, aes(x=fitted, y=resid))+geom_point()
  p1<-p1+stat_smooth(method="loess")+geom_hline(yintercept=0, col="red", linetype="dashed")
  p1<-p1+xlab("Fitted values")+ylab("Residuals")
  p1<-p1+ggtitle("Residual vs Fitted Plot")+theme_bw()
  p1
}

binned_fitted_versus_cbre_residuals<-function(model,bins=20){
  warning("binned_fitted_versus_cbre_residuals is deprecated. Use binned_fitted_residuals instead.")
  binned_fitted_residuals(model,"b_CBre",bins)
}

binned_fitted_versus_term_residuals<-function(model,bins=20){
  warning("binned_fitted_versus_term_residuals is deprecated. Use binned_fitted_residuals instead.")
  binned_fitted_residuals(model,"b_Term",bins)
}



binned_fitted_residuals<-function(model,
                                  versus_col,
                                  bins=20,
                                  caption="Source: FPDS, CSIS Analysis"){
  
  #Save this for a future GLM
  # Term_data_01A<-data.frame(fitted=fitted(Term_01A),
  #                        residuals=residuals(Term_01A),
  #                        nTerm=Term_01A@frame$nTerm,
  #                        cb_Comp=Term_01A@frame$cb_Comp
  #                        )
  
  if(class(model)[1] %in% c("glmerMod","lmerMod"))
  {
    data <-data.frame(
      fitted=fitted(model),
      residuals=residuals(model),
      versus=model@frame[[versus_col]]
    )
    
  }
  else
  {
    data <-data.frame(
      fitted=fitted(model),
      residuals=residuals(model),
      versus=model$model[[versus_col]]
    )
  }
  
  data$bin_fitted<-bin_df(data,rank_col="fitted",bins=bins)
  
  data<-subset(data,!is.na(fitted) & !is.na(residuals) )
  
  ggplot(data= data %>% 
           group_by(bin_fitted) %>% 
           dplyr::summarise (mean_versus = mean(versus),
                             mean_fitted =mean(fitted)),
         aes(y=mean_versus,x=mean_fitted))+geom_point() +
    labs(title="Binned Fitted Linear Model", caption=caption)
}



binned_fitted_versus_SomeOpt_residuals<-function(model,bins=20){
  warning("binned_fitted_versus_SomeOpt_residuals is deprecated. Use binned_fitted_residuals instead.")
  binned_fitted_residuals(model,"b_SomeOpt",bins)
}



binned_fitted_versus_AllOpt_residuals<-function(model,bins=20){
  warning("binned_fitted_versus_AllOpt_residuals is deprecated. Use binned_fitted_residuals instead.")
  binned_fitted_residuals(model,"b_AllOpt",bins)
}


binned_fitted_versus_lp_CBre_residuals<-function(model,bins=20){
  warning("binned_fitted_versus_lp_CBre_residuals is deprecated. Use binned_fitted_residuals instead.")
  binned_fitted_residuals(model,"lp_CBre",bins)
}


binned_fitted_versus_ln_CBre_residuals<-function(model,bins=20){
  warning("binned_fitted_versus_ln_CBre_residuals is deprecated. Use binned_fitted_residuals instead.")
  binned_fitted_residuals(model,"ln_CBre",bins)
}

binned_fitted_versus_residuals<-function(model,bins=20){
  if(class(model)[1] %in% c("glmerMod","lmerMod"))
  {
    if(!is.null(model@frame$b_CBre)){
      graph<-binned_fitted_residuals(model,"b_CBre",bins)
    } else if(!is.null(model@frame$lp_CBre)){
      graph<-binned_fitted_residuals(model,"lp_CBre",bins)
    } else if(!is.null(model@frame$ln_CBre)){
      graph<-binned_fitted_residuals(model,"ln_CBre",bins)
    } else if(!is.null(model@frame$b_Term)){
      graph<-binned_fitted_residuals(model,"b_Term",bins)
    } else if(!is.null(model@frame$b_SomeOpt)){
      graph<-binned_fitted_residuals(model,"b_SomeOpt",bins)
    } else if(!is.null(model@frame$b_AllOpt)){
      graph<-binned_fitted_residuals(model,"b_AllOpt",bins)
    } else if(any(c("l_Offr",
                    "lp_OptGrowth",
                    "ln_OptGrowth",
                    "log(FYDP2_Actual + 1)",
                    "log(FYDP2_ActCml + 1)"
    ) %in% colnames(model@frame))){
      graph<-resid_plot(model,sample=25000)
    }
    else{stop("Outcome variable not recognized.")}
  }
  else
  {
    if(!is.null(model$model$b_CBre)){
      graph<-binned_fitted_residuals(model,"b_CBre",bins)
    } else if(!is.null(model$model$lp_CBre)){
      graph<-binned_fitted_residuals(model,"lp_CBre",bins)
    } else if(!is.null(model$model$ln_CBre)){
      graph<-binned_fitted_residuals(model,"ln_CBre",bins)
    } else if(!is.null(model$model$b_Term)){
      graph<-binned_fitted_residuals(model,"b_Term",bins)
    } else if(!is.null(model$model$b_SomeOpt)){
      graph<-binned_fitted_residuals(model,"b_SomeOpt",bins)
    } else if(!is.null(model$model$b_AllOpt)){
      graph<-binned_fitted_residuals(model,"b_AllOpt",bins)
    } else if(any(c("l_Offr",
                    "lp_OptGrowth",
                    "ln_OptGrowth",
                    "log(FYDP2_Actual + 1)",
                    "log(FYDP2_ActCml + 1)"
                    ) %in% colnames(model$model))){
      graph<-resid_plot(model,sample=25000)  
    }
    else{stop("Outcome variable not recognized.")}
  }
  graph
}




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

residuals_binned<-function(model,col="fitted",bins=40){
  if(class(model)[1] %in% c("glmerMod","lmerMod"))
  {
    data <-data.frame(
      fitted=fitted(model),
      residuals=residuals(model)
    )
    if (col!="fitted"){
      data$x_col<-model@frame[,col]
      colnames(data)[colnames(data)=="x_col"]<-col
    }
    
  }
  else
  {
    data <-data.frame(
      fitted=fitted(model),
      residuals=residuals(model)
    )
    
    if (col!="fitted"){
      data$x_col<-model$model[,col]
      colnames(data)[colnames(data)=="x_col"]<-col
    }
  }
  
  #Safety measure for missing output variables.
  graph<-NULL
  if(class(model)[1] %in% c("glmerMod","lmerMod"))
  {
    if(!is.null(model@frame$b_CBre)){
      data$outcome<-model@frame$b_CBre
      data<-binned.resids (data[,col],
                           data$fitted-data$outcome, nclass=bins)$binned
    } else if(!is.null(model@frame$b_Term)){
      data$outcome<-model@frame$b_Term
      data<-binned.resids (data[,col],
                           data$fitted-data$outcome, nclass=bins)$binned
    } else if(!is.null(model@frame$b_SomeOpt)){
      data$outcome<-model@frame$b_SomeOpt
      data<-binned.resids (data[,col],
                           data$fitted-data$outcome, nclass=bins)$binned
    } else if(!is.null(model@frame$b_AllOpt)){
      data$outcome<-model@frame$b_AllOpt
      data<-binned.resids (data[,col],
                           data$fitted-data$outcome, nclass=bins)$binned
      
    } else if(!is.null(model@frame$lp_CBre)){
      data$outcome<-model@frame$lp_CBre
      data<-binned.resids (data[,col],
                           data$residuals, nclass=bins)$binned
    } else if(!is.null(model@frame$ln_CBre)){
      data$outcome<-model@frame$ln_CBre
      data<-binned.resids (data[,col],
                           data$residuals, nclass=bins)$binned
    } else if(!is.null(model@frame$lp_OptGrowth)){
      data$outcome<-model@frame$lp_OptGrowth
      data<-binned.resids (data[,col],
                           data$residuals, nclass=bins)$binned
    } else if(!is.null(model@frame$ln_OptGrowth)){
      data$outcome<-model@frame$ln_OptGrowth
      data<-binned.resids (data[,col],
                           data$residuals, nclass=bins)$binned
    } else if(!is.null(model@frame$l_Offr)){
      data$outcome<-model@frame$l_Offr
      data<-binned.resids(data[,col],
                          data$residuals, nclass=bins)$binned
    } 
    else{stop("Outcome variable not recognized.")}
  }
  else
  {
    data$outcome<-model$y
    if(is.null(model$model$b_CBre) & is.null(model$model$b_Term))
      data<-binned.resids (data[,col],
                           data$residuals, nclass=bins)$binned
    else
      data<-binned.resids (data[,col],
                           data$fitted-model$y, nclass=bins)$binned
    # if(!is.null(model$model$b_CBre)){
    #   data$outcome<-model$model$b_CBre
    #   data<-binned.resids (data[,col],
    #                        data$residuals, nclass=bins)$binned
    # } else if(!is.null(model$model$b_Term)){
    #   data$outcome<-model$model$b_Term
    #   data<-binned.resids (data[,col],
    #                        data$residuals, nclass=bins)$binned
    # } else if(!is.null(model$model$lp_CBre)){
    #   data$outcome<-model$model$lp_CBre
    #   data<-binned.resids (data[,col],
    #                        data$residuals, nclass=bins)$binned
    # } else if(!is.null(model$model$ln_CBre)){
    #   data$outcome<-model$model$ln_CBre
    #   data<-binned.resids (data[,col],
    #                        data$residuals, nclass=bins)$binned
    # } else if(!is.null(model$model$lp_OptGrowth)){
    #   data$outcome<-model$model$lp_OptGrowth
    #   data<-binned.resids (data[,col],
    #                        data$residuals, nclass=bins)$binned
    # } else if(!is.null(model$model$ln_OptGrowth)){
    #   data$outcome<-model$model$ln_OptGrowth
    #   data<-binned.resids (data[,col],
    #                        data$residuals, nclass=bins)$binned
    # } else if(!is.null(model$model$l_Offr)){
    #   # graph<-resid_plot(model,sample=25000)
    #   data$outcome<-model$model$l_Offr
    #   data<-binned.resids (data[,col],
    #                        data$residuals, nclass=bins)$binned
    # } 
    # else{stop("Outcome variable not recognized.")}
  }
  
  
  
  br<-ggplot(data=data,
             aes(x=xbar,y-ybar))+
    geom_point(aes(y=ybar))+ #Residuals
    geom_line(aes(y=se2),col="grey")+
    geom_line(aes(y=-se2),col="grey")+
    labs(title="Binned residual plot",
         y="Average Residual")
  
  if (col=="fitted"){
    br<-br+labs(x="Estimated Pr(Termination)")
    warning("Always uses Xlb Estimated Pr(Termination), should update.")
  }
  br
}










summary_regression_compare<-function(model_old,model_new){
  
  arm::residual.plot(fitted(model_old),
                     resid(model_old),
                     sigma(model_old)
  )
  arm::residual.plot(fitted(model_new),
                     resid(model_new),
                     sigma(model_new)
  )
  
}

residual_compare<-function(model1_old,model1_new,model2_old,model2_new,col,x_axis_name,bins=20){
  if(col %in% model_colnames(model1_old) & col %in% model_colnames(model2_old)){
    gridExtra::grid.arrange(residuals_binned(model1_old,col,bins=bins)+
                              labs(x=x_axis_name),
                            residuals_binned(model1_new,col,bins=bins)+
                              labs(x=x_axis_name),
                            residuals_binned(model2_old,col,bins=bins)+
                              labs(x=x_axis_name),
                            residuals_binned(model2_new,col,bins=bins)+
                              labs(x=x_axis_name),
                            ncol=2)
  }
  else{#If the variable is just in the new model
    gridExtra::grid.arrange(residuals_binned(model1_new,col,bins=bins)+
                              labs(x=x_axis_name),
                            residuals_binned(model2_new,col,bins=bins)+
                              labs(x=x_axis_name),
                            ncol=1)
    
  }
}


summary_residual_compare<-function(model1_old,model1_new=NULL,
                                   model2_old=NULL,model2_new=NULL,
                                   skip_vif=TRUE,bins=5){
  if(skip_vif==FALSE) warning("Deprecating VIF. Just use glmer_examine on the models")
  #Plot the fitted values vs actual results
  if(!is.na(bins)){
    #Plot residuals versus fitted
    if("cl_US6_avg_sal_lag1Const" %in% model_colnames(model1_old) &"cl_US6_avg_sal_lag1Const" %in% model_colnames(model1_new)) bins<-bins+5
    if("cl_CFTE" %in% model_colnames(model1_old) &"cl_CFTE" %in% model_colnames(model1_new)) bins<-bins+5
    if("c_pPBSC" %in% model_colnames(model1_old) &"c_pPBSC" %in% model_colnames(model1_new)) bins<-bins+5
    if("c_pOffPSC" %in% model_colnames(model1_old) &"c_pOffPSC" %in% model_colnames(model1_new)) bins<-bins+5
    if("cl_pairCA" %in% model_colnames(model1_old) &"cl_pairCA" %in% model_colnames(model1_new)) bins<-bins+5
    if("c_OffCri" %in% model_colnames(model1_old) &"c_OffCri" %in% model_colnames(model1_new)) bins<-bins+5
    if(("cl_Ceil" %in% model_colnames(model1_old) & "cl_Ceil" %in% model_colnames(model1_new))|
       ("cl_Ceil_Then_Year" %in% model_colnames(model1_old) & "cl_Ceil_Then_Year" %in% model_colnames(model1_new))) bins<-bins+10
    
    if("cl_Days" %in% model_colnames(model1_old) & "cl_Days" %in% model_colnames(model1_new)) bins<-bins+5
    
  }   
  if(!is.null(model2_new)){
    #All four passed
     
    
    gridExtra::grid.arrange(binned_fitted_versus_residuals(model1_old,bins=bins),
                            binned_fitted_versus_residuals(model1_new,bins=bins),
                            binned_fitted_versus_residuals(model2_old,bins=bins),
                            binned_fitted_versus_residuals(model2_new,bins=bins),
                            ncol=2)
    #This only works once you have some continuous variables or set a small bin count
    
    if(!"b_Term" %in% model_colnames(model1_old) & !"b_CBre" %in% model_colnames(model1_old) &
       !"b_SomeOpt" %in% model_colnames(model1_old) & !"b_AllOpt" %in% model_colnames(model1_old))
    gridExtra::grid.arrange(resid_plot(model1_old,sample=25000),
                            resid_plot(model1_new,sample=25000),
                            resid_plot(model2_old,sample=25000),
                            resid_plot(model2_new,sample=25000),
                            ncol=2)
    else{
      gridExtra::grid.arrange(residuals_binned(model1_old,bins=bins),
                              residuals_binned(model1_new,bins=bins),
                              residuals_binned(model2_old,bins=bins),
                              residuals_binned(model2_new,bins=bins),
                              ncol=2)
    }
    
    
    # if("c_OffCri" %in% model_colnames(model1_new) & "c_OffCri" %in% model_colnames(model2_new)){
    # residual_compare(model1_old,model1_new,model2_old,model2_new,"c_OffCri","Office Crisis %",10)
    # }
    
    if("cl_Ceil" %in% model_colnames(model1_new)){
      residual_compare(model1_old,model1_new,model2_old,model2_new,"cl_Ceil","Centered Log(Ceiling)",20)
    }
    
    if("cl_Days" %in% model_colnames(model1_new)){
      residual_compare(model1_old,model1_new,model2_old,model2_new,"cl_Days","Centered Log(Days)",10)
    }
    output<-NULL
    if(class(model1_new)%in% c("glmerMod","lme4","lmerMod") & class(model2_new)%in% c("glmerMod","lme4","lmerMod")) 
    { 
      # If the fit is singular or near-singular, there might be a higher chance of a false positive (we’re not necessarily screening out gradient and Hessian checking on singular directions properly); a higher chance that the model has actually misconverged (because the optimization problem is difficult on the boundary); and a reasonable argument that the random effects model should be simplified.
      # https://rstudio-pubs-static.s3.amazonaws.com/33653_57fc7b8e5d484c909b615d8633c01d51.html
      # The definition of singularity is that some of the constrained parameters of the random effects theta parameters are on the boundary (equal to zero, or very very close to zero, say <10−6):
      m1t<-getME(model1_new,"theta")
      m1l<-getME(model1_new,"lower")
      m2t<-getME(model2_new,"theta")
      m2l<-getME(model2_new,"lower")
      # min(m2t[ll==0])
      
      # min(m2t[ll==0])
      output<-list(m1t[m1l==0],
                   m2t[m2l==0],
                   model1_new@optinfo$conv$lme4$messages,
                   model2_new@optinfo$conv$lme4$messages
      )
    } 
    else if ((!class(model1_new)%in% c("glmerMod","lme4","lmerMod") & !class(model2_new)%in% c("glmerMod","lme4","lmerMod")) &
             (!class(model1_old)%in% c("glmerMod","lme4","lmerMod") & !class(model2_old)%in% c("glmerMod","lme4","lmerMod"))
             ){
      output<-list(rbind(deviance_stats(model1_old,"model1_old"),
                         deviance_stats(model1_new,"model1_new"),
                         deviance_stats(model2_old,"model2_old"),
                         deviance_stats(model2_new,"model2_new"))
      )
    }
    
  } else if(!is.null(model1_new)){
    
  
    gridExtra::grid.arrange(binned_fitted_versus_residuals(model1_old,bins=bins),
                            binned_fitted_versus_residuals(model1_new,bins=bins),
                            ncol=2)
    
    if(!"b_Term" %in% model_colnames(model1_old) & !"b_CBre" %in% model_colnames(model1_old) &
       !"b_SomeOpt" %in% model_colnames(model1_old) & !"b_AllOpt" %in% model_colnames(model1_old)){
      
      gridExtra::grid.arrange(resid_plot(model1_old,sample=25000),
                              resid_plot(model1_new,sample=25000),
                              ncol=2)
    }
    else{
      gridExtra::grid.arrange(residuals_binned(model1_old,bins=bins),
                              residuals_binned(model1_new,bins=bins),
                              ncol=2)
    }
    
    # if("c_OffCri" %in% model_colnames(model1_new) & "c_OffCri" %in% model_colnames(model2_new)){
    # residual_compare(model1_old,model1_new,model2_old,model2_new,"c_OffCri","Office Crisis %",10)
    # }
    
    # if("cl_Ceil" %in% model_colnames(model1_new)){
    #   residual_compare(model1_old,model1_new,model2_old,model2_new,"cl_Ceil","Centered Log(Ceiling)",20)
    # }
    # 
    # if("cl_Days" %in% model_colnames(model1_new)){
    #   residual_compare(model1_old,model1_new,model2_old,model2_new,"cl_Days","Centered Log(Days)",10)
    # }
    output<-NULL
    if(class(model1_new)%in% c("glmerMod","lme4","lmerMod")) 
    { 
      # If the fit is singular or near-singular, there might be a higher chance of a false positive (we’re not necessarily screening out gradient and Hessian checking on singular directions properly); a higher chance that the model has actually misconverged (because the optimization problem is difficult on the boundary); and a reasonable argument that the random effects model should be simplified.
      # https://rstudio-pubs-static.s3.amazonaws.com/33653_57fc7b8e5d484c909b615d8633c01d51.html
      # The definition of singularity is that some of the constrained parameters of the random effects theta parameters are on the boundary (equal to zero, or very very close to zero, say <10−6):
      m1t<-getME(model1_new,"theta")
      m1l<-getME(model1_new,"lower")
      # min(m2t[ll==0])
      
      # min(m2t[ll==0])
      output<-list(m1t[m1l==0],
                   model1_new@optinfo$conv$lme4$messages
      )
    } 
    else if (!class(model1_new)%in% c("glmerMod","lme4","lmerMod") & !class(model1_old)%in% c("glmerMod","lme4","lmerMod")){#First 2 passed
      output<-list(rbind(deviance_stats(model1_old,"model1_old"),
                         deviance_stats(model1_new,"model1_new"))
      )
    }
  } else{ #Only a single model
    
    
    #This only works once you have some continuous variables 
    
    if(!is.na(bins)){
      #Plot residuals versus fitted
      if("cl_US6_avg_sal_lag1Const" %in% model_colnames(model1_old)) bins<-bins+5
      if("cl_CFTE" %in% model_colnames(model1_old)) bins<-bins+5
      if("c_pPBSC" %in% model_colnames(model1_old)) bins<-bins+5
      if("c_pOffPSC" %in% model_colnames(model1_old)) bins<-bins+5
      if("cl_pairCA" %in% model_colnames(model1_old)) bins<-bins+5
      if("c_OffCri" %in% model_colnames(model1_old)) bins<-bins+5
      if(("cl_Ceil" %in% model_colnames(model1_old))|
         ("cl_Ceil_Then_Year" %in% model_colnames(model1_old))) bins<-bins+10
      
      if("cl_Days" %in% model_colnames(model1_old)) bins<-bins+5
      
    }   
    
    
    if(!"b_Term" %in% model_colnames(model1_old) & !"b_CBre" %in% model_colnames(model1_old) &
       !"b_SomeOpt" %in% model_colnames(model1_old) & !"b_AllOpt" %in% model_colnames(model1_old))
      
      gridExtra::grid.arrange(
        binned_fitted_versus_residuals(model1_old,bins=bins),
        residuals_binned(model1_old,bins=bins),
        resid_plot(model1_old,sample=25000)
      )
    else gridExtra::grid.arrange(
      binned_fitted_versus_residuals(model1_old,bins=bins),
      residuals_binned(model1_old,bins=bins)
    )
    # }
    # else{
    #   gridExtra::grid.arrange(
    #     binned_fitted_versus_residuals(model1_old,bins=bins),
    #     residuals_binned(model1_old,bins=bins),
    #     resid_plot(model1_old,sample=25000)
    #   ) 
  # }
  
  # if("c_OffCri" %in% model_colnames(model1_old) & "c_OffCri" %in% model_colnames(model2_new)){
  # residual_compare(model1_old,model1_old,model2_old,model2_new,"c_OffCri","Office Crisis %",10)
  # }
  
  # if("cl_Ceil" %in% model_colnames(model1_old)){
  #   residual_compare(model1_old,model1_old,model2_old,model2_new,"cl_Ceil","Centered Log(Ceiling)",20)
  # }
  # 
  # if("cl_Days" %in% model_colnames(model1_old)){
  #   residual_compare(model1_old,model1_old,model2_old,model2_new,"cl_Days","Centered Log(Days)",10)
  # }
  output<-NULL
  if(class(model1_old)%in% c("glmerMod","lme4","lmerMod")) 
  { 
    # If the fit is singular or near-singular, there might be a higher chance of a false positive (we’re not necessarily screening out gradient and Hessian checking on singular directions properly); a higher chance that the model has actually misconverged (because the optimization problem is difficult on the boundary); and a reasonable argument that the random effects model should be simplified.
    # https://rstudio-pubs-static.s3.amazonaws.com/33653_57fc7b8e5d484c909b615d8633c01d51.html
    # The definition of singularity is that some of the constrained parameters of the random effects theta parameters are on the boundary (equal to zero, or very very close to zero, say <10−6):
    m1t<-getME(model1_old,"theta")
    m1l<-getME(model1_old,"lower")
    # min(m2t[ll==0])
    
    # min(m2t[ll==0])
    output<-list(m1t[m1l==0],
                 model1_old@optinfo$conv$lme4$messages
    )
  } 
  else if (!class(model1_old)%in% c("glmerMod","lme4","lmerMod") & !class(model1_old)%in% c("glmerMod","lme4","lmerMod")){
    output<-list(deviance_stats(model1_old,"model1_old")
    )
  }
}

output

}


glmer_examine<-function(model,display=FALSE){
  if(display==TRUE) display(model)
  output<-car::vif(model)
  if(class(model)[1] %in% c("glmerMod","lmerMod")) 
  { 
    
    # If the fit is singular or near-singular, there might be a higher chance of a false positive (we’re not necessarily screening out gradient and Hessian checking on singular directions properly); a higher chance that the model has actually misconverged (because the optimization problem is difficult on the boundary); and a reasonable argument that the random effects model should be simplified.
    # https://rstudio-pubs-static.s3.amazonaws.com/33653_57fc7b8e5d484c909b615d8633c01d51.html
    # The definition of singularity is that some of the constrained parameters of the random effects theta parameters are on the boundary (equal to zero, or very very close to zero, say <10−6):
    t<-getME(model,"theta")
    l<-getME(model,"lower")
    
    # min(m2t[ll==0])
    
    # min(m2t[ll==0])
    if(!is.null(model@optinfo$conv$lme4$messages)){
      output<-list(car::vif(model),
                   icc(model),
                   model@optinfo$conv$lme4$messages,
                   t[l==0])
    }
    else{
      output<-list(car::vif(model),
                   icc(model),
                   t[l==0]
      )
    }
  } 
  output
}

get_icc<-function(model,display=FALSE){
  ############################################################################################################################
  #Keep Calm and Learn Multilevel Logistic Modeling: A Simplified Three-Step Procedure for Beginners Using SPSS, Stata, and R#
  ############################################################################################################################
  # icc <- model@theta^2/ (model@theta^2 + (3.14159^2/3))
  # icc
  if(display==TRUE) display(model)
  if(!is.null(model@optinfo$conv$lme4$messages)){
    output<-list(performance::icc(model),
                 model@optinfo$conv$lme4$messages)
  }
  else{
    output<-performance::icc(model)
  }
  output
}



get_pars<-function(model){
  if (isLMM(model)) {
    pars <- getME(model,"theta")
  } else {
    ## GLMM: requires both random and fixed parameters
    pars <- getME(model, c("theta","fixef"))
  }
  pars
}


# #Extract statistic information of the 15 discrete variables and generate dataframe    
# name_categorical <- c("CompOffr","Veh","PricingFee","UCA","Intl","Term",
#                      "Dur","Ceil","CBre","PSR","Urg","FxCb","Fee","CRai",
#                      "NoComp")   #list of all categorial and binary variables
# 

# memory.limit(56000)

#Input(contract: the dataframe; value_col: the name of the value measuring variable)
#Output: an updated value_col, if the value_col is null
get_value_col<-function(contract,
                        value_col=NULL){
  if(is.null(value_col)){
    if("Action_Obligation_OMB20_GDP18" %in% colnames(contract)){
      value_col<-"Action_Obligation_OMB20_GDP18"
    }
    else if("Action_Obligation.OMB20_GDP18" %in% colnames(contract)){
      value_col<-"Action_Obligation.OMB20_GDP18"
    }
    else if("Action_Obligation" %in% colnames(contract)){
      value_col<-"Action_Obligation"
    }
    else if("Action.Obligation" %in% colnames(contract)){
      value_col<-"Action.Obligation"
    }
    else if("Action_Obligation.Then.Year" %in% colnames(contract)){
      value_col<-"Action_Obligation.Then.Year"
    }
    else if("Action_Obligation.Then.Year" %in% colnames(contract)){
      value_col<-"Action_Obligation.Then.Year"
    }
    else if("Action_Obligation_Then_Year" %in% colnames(contract)){
      value_col<-"Action_Obligation_Then_Year"
    }
    else stop("No standard value column in dataset, pass the desired column to oolumn_name.")
  }
  else if(!value_col %in% colnames(contract) ){
    stop(paste(value_col,"not present in contract."))
  }
  value_col
}

statsummary_discrete <- function(x, 
                                 contract,accuracy=0.01,
                                 value_col=NULL,
                                 top_rank=NULL){      #input(x: name of the discrete variable, contract：name of the dataframe)
  if(is.data.frame(x)) stop("Pass the data frame as the second parameter")
  value_col<-get_value_col(contract,value_col)
  
  if(!x %in% colnames(contract)) stop(paste(x,"is not a column in contract."))
  if(!is.factor(contract[[x]])) contract[[x]]<-factor(contract[[x]])
  
  
  # unique_value_list <- levels(contract[[x]])
  # categories <- c(unique_value_list)
  # Percent_Actions <- c()
  # Percent_Records <- c()

  Percent_Actions <- contract %>% group_by(!!sym(x)) %>%
    dplyr::summarise(
      Records=length(!!sym(x)),
      Value=sum(!!sym(value_col),na.rm=TRUE)
    ) 
  
  if(!is.null(top_rank)){
    # Percent_Actions[[x]]<-as.character(Percent_Actions[[x]])
    Percent_Actions <- Percent_Actions %>% 
      mutate(Top=ifelse(rank(-Value)>top_rank & 
                          rank(-Records)>top_rank 
                          ,"All Other",as.character(!!sym(x))))
    Percent_Actions$Top[is.na(Percent_Actions[[x]])]<-NA
    Percent_Actions[[x]]<-Percent_Actions$Top
    Percent_Actions<-Percent_Actions %>% group_by(!!sym(x)) %>%
      dplyr::summarise(
        Records=sum(Records,na.rm=TRUE),
        Value=sum(Value)
      )
  }
  Percent_Actions <- Percent_Actions %>% group_by() %>%
    dplyr::mutate(
      Records=Records/sum(Records,na.rm=TRUE),
      Value=Value/sum(Value,na.rm=TRUE)
    )
  
  if(!is.null(accuracy)){
    Percent_Actions<-Percent_Actions %>% mutate(
      Records=percent(Records,accuracy=accuracy),
      Value=percent(Value,accuracy=accuracy)
    )
    
  }
  # for (i in 1:length(unique_value_list)){
  #   Percent_Records <- c(Percent_Records, percent(round(sum(contract[[x]] == unique_value_list[i],na.rm = TRUE)/nrow(contract),5),accuracy = accuracy))
  #   Percent_Actions <- c(Percent_Actions, percent(round(sum(contract[contract[[x]] == unique_value_list[i],value_col],na.rm = TRUE)/sum(contract[,value_col],na.rm = TRUE),5),accuracy = accuracy))    
  # }
  # if(sum(is.na(contract[[x]]))>1){#If any NA}
  #   categories <- c(unique_value_list,"NA")
  #   Percent_Records <- c(Percent_Records, percent(round(sum(is.na(contract[[x]]))/nrow(contract),5),accuracy = .01))
  #   Percent_Actions <- c(Percent_Actions, percent(round(sum(contract[[value_col]][is.na(contract[[x]])],na.rm = TRUE)/sum(contract[,value_col],na.rm = TRUE),5),accuracy = accuracy))
  # }
  name_categorical <- c(x,"% of Records","% of $s")
  # categories <- as.data.frame(cbind(categories,Percent_Records,Percent_Actions))
  colnames(Percent_Actions) <- name_categorical
  return(Percent_Actions)
}

#Extract statistic information of the 26 continuous variables and generate dataframe      
name_Continuous <- c("Action_Obligation","UnmodifiedContractBaseAndAllOptionsValue",
                     "ChangeOrderBaseAndAllOptionsValue","UnmodifiedDays",
                     "UnmodifiedNumberOfOffersReceived",
                     "capped_def6_ratio_lag1","capped_def5_ratio_lag1","capped_def4_ratio_lag1","capped_def3_ratio_lag1","capped_def2_ratio_lag1",
                     "def6_HHI_lag1","def5_HHI_lag1","def4_HHI_lag1","def3_HHI_lag1","def2_HHI_lag1",
                     "def6_obl_lag1","def5_obl_lag1","def4_obl_lag1","def3_obl_lag1","def2_obl_lag1",
                     "US6_avg_sal_lag1","US5_avg_sal_lag1","US4_avg_sal_lag1","US3_avg_sal_lag1","US2_avg_sal_lag1"
)



verify_transform<-function(x,
                           original_col,
                           transformed_col,
                           log=TRUE,
                           rescale=TRUE,
                           cap_value=NULL,
                           plus1=FALSE,
                           just_check_na=FALSE){
  
  cap<-function(column,cap){
    column[column>cap]<-cap
    column
  }
  x<-as.data.frame(x)
  if(!original_col %in% colnames(x))
    stop(paste("original_col",original_col,"is missing"))
  
  if(!transformed_col %in% colnames(x))
    stop(paste("transformed_col,",transformed_col,"is missing"))
  
  
  if(plus1)  
    x[,original_col]<-x[,original_col]+1
  
  if(!is.null(cap_value))
    x[,original_col]<-cap(x[,original_col],cap_value)
  
  if(log)
    x[,original_col]<-na_non_positive_log(x[,original_col])
  
  if(rescale)
    x[,original_col]<-arm::rescale(x[,original_col])
  
  
  match<-all(is.na(x[,original_col])==is.na(x[,transformed_col]))
  
  if(!match) stop(paste("NA mismatch for",original_col,"and",transformed_col))
  
  if(just_check_na) 
    x[,transformed_col]<-arm::rescale(x[,transformed_col])
  
  match<-all(x[!is.na(x[,original_col]),original_col]==x[!is.na(x[,transformed_col]),transformed_col])
  if(!match)
    match<-all(abs(x[!is.na(x[,original_col]),original_col]-
                     x[!is.na(x[,transformed_col]),transformed_col]) <1e-10
    )
  
  if(!match) stop(paste("Values mismatch for",original_col,"and",transformed_col))
  #}
  return(match)
}

contract_transform_verify<-function(contract,just_check_na=FALSE,dollars_suffix="OMB20_GDP18",unlogged_ratio=FALSE){
  #Outcome
  if("n_Cbre" %in% colnames(contract)){
    if(dollars_suffix=="Then_Year")
      verify_transform(contract,paste("n_CBre",dollars_suffix,sep="_"),
                       paste("ln_CBre",dollars_suffix,sep="_"),rescale=FALSE)
    else
      verify_transform(contract,paste("n_CBre",dollars_suffix,sep="_"),
                       "ln_CBre",rescale=FALSE)
  }
  else warning("No n_Cbre in dataset")
  #Scope
  if(dollars_suffix=="Then_Year" & paste("cln_Base",dollars_suffix,sep="_") %in% colnames(contract))
    verify_transform(contract,paste("UnmodifiedBase",dollars_suffix,sep="_"),
                   paste("cln_Base",dollars_suffix,sep="_"),just_check_na=just_check_na)
  else if ("cln_Base" %in% colnames(contract))
    verify_transform(contract,paste("UnmodifiedBase",dollars_suffix,sep="_"),
                     "cln_Base",just_check_na=just_check_na)
  
  if ("clr_Ceil2Base" %in% colnames(contract))
    verify_transform(contract,"Ceil2Base","clr_Ceil2Base",just_check_na=just_check_na)
  
  if("cln_Ceil" %in% colnames(contract)) 
    verify_transform(contract,paste("UnmodifiedCeiling",dollars_suffix,sep="_"),
                     "cln_Ceil",just_check_na=just_check_na)
  if("cl_Ceil" %in% colnames(contract)) #Legacy Monopolies Name
    verify_transform(contract,paste("UnmodifiedCeiling",dollars_suffix,sep="_"),
                     "cl_Ceil",just_check_na=just_check_na)
    
  
  if("cln_Days" %in% colnames(contract)) 
    verify_transform(contract,"UnmodifiedDays","cln_Days",cap_value=3650,just_check_na=just_check_na) 
  
  
  if("cl_Days" %in% colnames(contract)) #Legacy Monopolies
    verify_transform(contract,"UnmodifiedDays","cl_Days",just_check_na=just_check_na) 
  
  #NAICS
  
  #cln_Def3HHI
  if("cln_Def3HHI" %in% colnames(contract))
    verify_transform(contract,"def3_HHI_lag1","cln_Def3HHI",just_check_na=just_check_na)
  if("cl_def3_HHI_lag1" %in% colnames(contract)) #Legacy Monopolies Name
    verify_transform(contract,"def3_HHI_lag1","cl_def3_HHI_lag1",just_check_na=just_check_na)
  
  
  #cln_Def6HHI
  if("cln_Def6HHI" %in% colnames(contract))
    verify_transform(contract,"def6_HHI_lag1","cln_Def6HHI",just_check_na=just_check_na)
  if("cl_def6_HHI_lag1" %in% colnames(contract))
    verify_transform(contract,"def6_HHI_lag1","cl_def6_HHI_lag1",just_check_na=just_check_na)
  
  
  
  #clr_Def3toUS
  if("clr_Def3toUS" %in% colnames(contract))
    verify_transform(contract,"def3_ratio_lag1","clr_Def3toUS",cap_value=1,just_check_na=just_check_na)
  if("cl_def3_ratio_lag1" %in% colnames(contract)) #Legacy Monopolies Name
    verify_transform(contract,"def3_ratio_lag1","cl_def3_ratio_lag1",cap_value=1,just_check_na=just_check_na)
  if("c_def3_ratio_lag1" %in% colnames(contract)) #Legacy Monopolies Name / Accidental Unlogged
    verify_transform(contract,"def3_ratio_lag1","c_def3_ratio_lag1",log=FALSE,just_check_na=just_check_na)
  
  #clr_Def6toUS
  if("clr_Def6toUS" %in% colnames(contract))
    verify_transform(contract,"def6_ratio_lag1","clr_Def6toUS",cap_value=1,just_check_na=just_check_na)
  if("cl_def6_ratio_lag1" %in% colnames(contract)) #Legacy Monopolies Name
    verify_transform(contract,"def6_ratio_lag1","cl_def6_ratio_lag1",cap_value=1,just_check_na=just_check_na)
  if("c_def6_ratio_lag1" %in% colnames(contract)) #Legacy Monopolies Name / Accidental Unlogged
    verify_transform(contract,"def6_ratio_lag1","c_def6_ratio_lag1",log=FALSE,just_check_na=just_check_na)
  
  #cln_US6sal
  if("cln_US6sal" %in% colnames(contract))
    verify_transform(contract,"US6_avg_sal_lag1Const","cln_US6sal",just_check_na=just_check_na)
  if("cl_US6_avg_sal_lag1" %in% colnames(contract))
    verify_transform(contract,"US6_avg_sal_lag1","cl_US6_avg_sal_lag1",just_check_na=just_check_na)
  
  #cln_Def6Obl
  if("cln_Def6Obl" %in% colnames(contract))
    verify_transform(contract,"def6_obl_lag1Const","cln_Def6Obl",just_check_na=just_check_na)
  if("cl_def6_obl_lag1" %in% colnames(contract))
    verify_transform(contract,"def6_obl_lag1","cl_def6_obl_lag1",just_check_na=just_check_na)
  
  #Office
  if("cln_OffFocus" %in% colnames(contract))
    verify_transform(contract,"office_naics_hhi_k","cln_OffFocus",just_check_na=just_check_na)
  
  if("cp_PairObl7" %in% colnames(contract))
    verify_transform(contract,"pMarket","cp_PairObl7",log=FALSE,just_check_na=just_check_na)
}


statsummary_continuous <- function(x, 
                                   contract,
                                   log=TRUE,
                                   digits=2,
                                   value_col=NULL,
                                   plus1=FALSE)
  {       #input(x: namelist of all continuous variables contract: name of the data frame)
    if(is.data.frame(x)) stop("Pass the data frame as the second parameter")
    #
    if(plus1==TRUE){
      if(log==FALSE) warning("The variable is not being logged. Are you sure you want to add one?")
      contract[,x]<-contract[,x]+1
    }
  
  if(!x %in% colnames(contract)) stop(paste(x,"is not a column in contract."))
  value_col<-get_value_col(contract,value_col)
  continuous_Info <- data.frame(matrix(ncol = 9,nrow = 0))
  colnames(continuous_Info) <- c("Variable_Name","Min","Max","Median","Logarithmic Mean",
                                 "1 unit below","1 unit above","% of records NA", 
                                 "% of Obligations to NA records")
  contract<-as.data.frame(contract)
  if(log==FALSE)
    colnames(continuous_Info)[colnames(continuous_Info)=="Logarithmic Mean"]<-"Arithmatic Mean"  
  for (i in x){
    if(log==TRUE) contract[[i]][contract[[i]]<=0]<-NA
      
    maxval <- round(max(contract[[i]],na.rm = TRUE), digits)
    medianval <- round(median(contract[[i]],na.rm = TRUE), digits)
    minval <- round(min(contract[[i]],na.rm = TRUE), digits)
    Percent_NA <- round(sum(is.na(contract[[i]]))/nrow(contract),5)
    Percent_Ob <- round(sum(contract[[value_col]][is.na(contract[[i]])],na.rm = TRUE)/sum(contract[[value_col]],na.rm = TRUE),5)
    
    if(log==TRUE){
      transformed_i <- log(contract[[i]])
      meanlog <- round(exp(mean(transformed_i,na.rm = TRUE)), digits)
      sdlog <- sd(transformed_i,na.rm = TRUE)
      unitabovelog <- round(exp(mean(transformed_i,na.rm = TRUE)+2*sdlog),digits)
      unitbelowlog <- round(exp(mean(transformed_i,na.rm = TRUE)-2*sdlog),digits)
      newrow <- c(i, minval, maxval, medianval, meanlog, unitbelowlog, unitabovelog,
                  Percent_NA, Percent_Ob)
    }
    else {
      meanval <- round(mean(contract[[i]],na.rm = TRUE), digits)
      sdval <- sd(contract[[i]],na.rm = TRUE)
      unitaboveval <- round(mean(contract[[i]],na.rm = TRUE)+2*sdval,digits)
      unitbelowval <- round(mean(contract[[i]],na.rm = TRUE)-2*sdval,digits)
      newrow <- c(i, minval, maxval, medianval, meanval, unitbelowval, unitaboveval,
                  Percent_NA, Percent_Ob)
    }
    continuous_Info[nrow(continuous_Info)+1,] <- newrow
  }
  # formating
  continuous_Info[,-1] <- lapply(continuous_Info[,-1], function(x) as.numeric(x))
  continuous_Info$aboveMax[continuous_Info$Max < continuous_Info$`1 unit above`] <- "*"
  continuous_Info$belowMin[continuous_Info$Min > continuous_Info$`1 unit below`] <- "*"
  # editing percentage values
  continuous_Info[,8:9] <- lapply(continuous_Info[,8:9], function(x) percent(x, accuracy = .01))
  continuous_Info[,2:7] <- lapply(continuous_Info[,2:7], function(x) comma_format(accuracy = 10^-digits)(x))#big.mark = ',',
  continuous_Info$`% of Obligations to NA records`[continuous_Info$`% of Obligations to NA records`=="NA%"] <- NA
  
  continuous_Info$`1 unit below` <- paste(continuous_Info$`1 unit below`,continuous_Info$belowMin,sep="")
  continuous_Info$`1 unit below` <- gsub("NA","",continuous_Info$`1 unit below`)
  continuous_Info$`1 unit above` <- paste(continuous_Info$`1 unit above`,continuous_Info$aboveMax,sep="")
  continuous_Info$`1 unit above` <- gsub("NA","",continuous_Info$`1 unit above`)
  continuous_Info[,c("belowMin","aboveMax")] <- NULL
  return(continuous_Info)
}















#function for making grouped bar plot for each categorical variable, data set used in function "Data/transformed_def.Rdata"
name_categorical <- c("CompOffr","Veh","PricingFee","UCA","Intl","Term",
                      "Dur","Ceil","CBre","PSR","Urg","FxCb","Fee","CRai",
                      "NoComp")

#Input(x: name of the categorical variable needs plot; contract: name of the dataframe)
#Output: grouped bar plot for the selected variable;
grouped_barplot <- function(x, contract,
                            value_col=NULL,
                            top_rank=NULL) {
  if(!x %in% colnames(contract)) stop(paste(x,"is not a column in contract."))
  value_col<-get_value_col(contract,value_col)
  
  #perparing data for ploting
  name_Info <- as.data.frame(statsummary_discrete(x, 
                                    contract,
                                    value_col=value_col,
                                    accuracy=NULL,#Skips formatting that adds %
                                    top_rank=top_rank)
  )
  
    
  name_Info_noNAN <-levels(droplevels(
    factor(name_Info[!is.na(name_Info[, 1]),1])))
  
  # name_Info[, -1] <- lapply(name_Info[, -1], function(x) as.numeric(gsub("%","",x)))
  name_Info <- reshape2::melt(name_Info, id = x)
  levels(name_Info$variable)[levels(name_Info$variable)=="% of $s"] = "% of Obligations"
  name_Info$variable <- factor(name_Info$variable, rev(levels(name_Info$variable)))
  if(any(is.na(name_Info[, 1])))
    limits<-rev(c(name_Info_noNAN,NA))
  else
    limits<-rev(name_Info_noNAN)
  basicplot <- ggplot(data = name_Info, 
                      aes(x = name_Info[, 1], 
                          y = name_Info[, 3], 
                          fill = factor(variable))) +
    geom_bar(stat = "identity", position = "dodge", width = 0.8) + 
    xlab("Category") + 
    ylab("") + scale_y_continuous(labels = scales::percent)+
    coord_flip() + 
    guides(fill = guide_legend(reverse = TRUE)) +   #reverse the order of legend
    theme_grey() + 
    scale_fill_grey() + 
    theme(legend.title = element_blank(), 
          legend.position = "bottom", 
          legend.margin = margin(t=-0.8, r=0, b=0.5, l=0, unit="cm"),
          legend.text = element_text(margin = margin(r = 0.5, unit = "cm")),    #increase the distances between two legends
          plot.margin = margin(t=0.3, r=0.5, b=0, l=0.5, unit = "cm")) + 
    ggtitle(paste("Statistic summary of categorical variable: ", x)) +
    scale_x_discrete(limits = limits)    #keep the order of category the same
  return(basicplot)
}



#function for generating frequency information table for categorical variables                            
freq_table <- function(x, contract){
  Frequency <- as.data.frame(table(contract[[x]]))
  Frequency[["Percent_Freq"]] <- round(Frequency[["Freq"]]/sum(Frequency[["Freq"]]),4)*100
  colnames(Frequency) <- c(x, "Count_Freq", "Percent_Freq")
  Percent_Obli <- c()
  for (i in Frequency[[x]]) {
    Percent_Obligation <- round(sum(contract[["Action_Obligation"]][contract[[x]] == i], na.rm = TRUE)/sum(contract[["Action_Obligation"]], na.rm = TRUE),5)
    Percent_Obli <- c(Percent_Obli, Percent_Obligation)
  }
  Frequency[["Percent_Obli"]] <- Percent_Obli*100
  return(Frequency)
}

#generate barplot according to frequency information table for categorical variables                                 
part_grouped_barplot <- function(name, 
                                 frequency_Info,
                                 description_var="Description"){
  if(description_var!="Description") frequency_Info$Description<-frequency_Info[,description_var]
  part_barplot <- ggplot(data = frequency_Info, 
                         aes(x = Description, 
                             y = value, 
                             fill=factor(variable))) + 
    geom_bar(stat = "identity", 
             position= "dodge", 
             width = 0.8) + 
    xlab("") + 
    ylab("") + 
    coord_flip() + 
    theme_grey() +
    scale_fill_grey(labels = c("% of Records", "% of obligations"),
                    guide = guide_legend(reverse = TRUE)) +
    theme(legend.title = element_blank(),
          legend.position = "bottom",
          legend.margin = margin(t=-0.8, r=0, b=0.5, l=0, unit = "cm"),
          legend.text = element_text(margin = margin(r=0.5, unit = "cm")),
          plot.margin = margin(t=0.3, r=0.5, b=0, l=0.5, unit = "cm")) 
  return(part_barplot)
}





get_pars<-function(model){
  if (isLMM(model)) {
  } else {
    pars <- getME(model,"theta")
    ## GLMM: requires both random and fixed parameters
    pars <- getME(model, c("theta","fixef"))
  }
  pars
}

get_study_variables_odds_ratio<-function(or.df,study="monopoly"){
  if(study=="monopoly"){
    study_list<-c("cl_def3_HHI_lag1"    ,
                  "cl_def6_HHI_lag1" ,
                  "CompOffr1 offer" ,
                  "CompOffr2 offers",
                  "CompOffr3-4 offers",
                  "CompOffr5+ offers"
                  # "CompOffr1 offer:b_UCA",
                  # "CompOffr2 offers:b_UCA"   ,         
                  # "CompOffr3-4 offers:b_UCA",
                  # "CompOffr5+ offers:b_UCA",
                  # "cl_def6_HHI_lag1:b_UCA",
                  # "cl_def6_HHI_lag1:cl_def6_obl_lag1"
    )
    study_coef_list<-list("Log(Subsector HHI)"=c("cl_def3_HHI_lag1"),
                          "Log(Det. Ind. HHI)"=c("cl_def6_HHI_lag1"),
                          "Comp. w/ 1 Offer"=c("CompOffr1 offer"),
                          "2 offers"=c("CompOffr2 offers"),
                          "3-4 offers"=c("CompOffr3-4 offers"),
                          "Comp. w/ 5+ Offers"=c("CompOffr5+ offers")
    )
  } else if (study=="services"){
    study_list<-get_coef_list(limit="services")
    study_list<-c(names(study_list),study_list)
    study_list<-study_list[-which(study_list=="(Intercept)")]
    study_coef_list<-NA
  } else if (study=="crisis"){
    study_list<-get_coef_list(limit="crisis")
    study_list<-c(names(study_list),study_list)
    study_list<-study_list[-which(study_list=="(Intercept)")]
    study_coef_list<-NA
    
  } else if (study=="FMS"){
    stop("Doesn't work yet")
  } else stop(paste("Unknown study: ",study, "available options: 'monopoly','services'"))
  or.df<-or.df[or.df$variable %in% study_list,]

    or.df$variable<-factor(or.df$variable)
  column_order<-c("output" ,  "input"  ,  "variable",   "OR"  ,"2.5 %"  ,  "97.5 %" )
  column_order<-column_order[column_order %in% colnames(or.df)]
  or.df<-or.df[,column_order]
  or.df$OR<-round(or.df$OR,digits=2)
  or.df[["2.5 %"]]<-round(or.df[["2.5 %"]],digits=2)
  or.df[["97.5 %"]]<-round(or.df[["97.5 %"]],digits=2)
  colnames(or.df)[colnames(or.df)=="output"]<-"Output"
  colnames(or.df)[colnames(or.df)=="input"]<-"Model"
  colnames(or.df)[colnames(or.df)=="OR"]<-"Odds Ratio"
  colnames(or.df)[colnames(or.df)=="2.5 %"]<-"Lower Bound"
  colnames(or.df)[colnames(or.df)=="97.5 %"]<-"Upper Bound"
  colnames(or.df)[colnames(or.df)=="variable"]<-"Variable"

  if(!is.na(study_coef_list))  
    levels(or.df$Variable)<- study_coef_list
  or.df
}

odds_ratio<-function(FM,name,input=NA,output=NA,walds=FALSE,rename_list=NA){
  OR <- exp(fixef(FM))
  if(walds==TRUE){
    CI <- exp(confint(FM,parm="beta_",method="Wald"))
  }
  else{
    CI <- exp(confint(FM,parm="beta_")) # it can be slow (~ hours). As alternative, the much faster but less precise Wald's method can be used: CI <- exp(confint(FM,parm="beta_",method="Wald"))
  }
  OR<-as.data.frame(cbind(OR,CI))
  OR$variable<-factor(rownames(OR))
  if(!is.na(rename_list)){ 
    rename_list<-rename_list[names(rename_list)[names(rename_list) %in% OR$variable]]
    if(any(!OR$variable %in% names(rename_list))) stop(paste("Missing values in renamelist",
                                                      OR$variable[!OR$variable %in% names(rename_list)]))
    OR$variable<-factor(OR$variable,
      levels=names(rename_list),
      labels=rename_list
    )
    OR<-OR[order(OR$variable),]
  }
  OR<-OR[,c("variable","OR",	"2.5 %",	"97.5 %")]
  write.csv(OR,file=paste("..//output//",name,"_odds_ratio",ifelse(walds==TRUE,"_wald",""),".csv",sep=""),row.names=FALSE)
  if(!is.na(output)) OR$output<-output
  if(!is.na(input)) OR$input<-input
  
  OR
}


log_analysis<-function(model){
  exp(cbind(coef(model),confint(model)))-1
}




# Helper function for string wrapping. 
# Default 20 character target width.
swr <- function(string, nwrap=20) {
  # https://stackoverflow.com/questions/37174316/how-to-fit-long-text-into-ggplot2-facet-titles
  
  paste(strwrap(string, width=nwrap), collapse="\n")
}
swr <- Vectorize(swr)




allFit_save <- function(m,meth.tab=NULL ,
                        data=NULL,
                        verbose=TRUE,
                        maxfun=1e5,
                        filename="output//allFit.rdata")
{
  source(system.file("utils", "allFit.R", package="lme4"))
  if (is.null(meth.tab))
    meth.tab <- meth.tab.0
  stopifnot(length(dm <- dim(meth.tab)) == 2, dm[1] >= 1, dm[2] >= 2,
            is.character(optimizer <- meth.tab[,"optimizer"]),
            is.character(method    <- meth.tab[,"method"]))
  fit.names <- gsub("\\.$","",paste(optimizer, method, sep="."))
  if(file.exists(filename))
    load(filename)
  else
    res <- setNames(as.list(fit.names), fit.names)
  for (i in seq_along(fit.names)) {
    if(typeof(res[[i]])=="character"){
      if (verbose) cat(fit.names[i],": ")
      ctrl <- list(optimizer=optimizer[i])
      ctrl$optCtrl <- switch(optimizer[i],
                             optimx    = list(method   = method[i]),
                             nloptWrap = list(algorithm= method[i]),
                             verbose=TRUE,
                             list(maxfun=maxfun))
      ctrl <- do.call(if(isGLMM(m)) glmerControl else lmerControl, ctrl)
      tt <- system.time(rr <- tryCatch(update(m, control = ctrl),
                                       error = function(e) e))
      attr(rr, "optCtrl") <- ctrl$optCtrl # contains crucial info here
      attr(rr, "time") <- tt  # store timing info
      res[[i]] <- rr
      save(m,res,file=filename)
      if (verbose) cat("[OK]\n")
    }
  }
  
  structure(res, class = "allfit", fit = m, sessionInfo =  sessionInfo(),
            data = data # is dropped if NULL
  )
}




# Helper function for string wrapping. 
# Default 20 character target width.
swr <- function(string, nwrap=20) {
  paste(strwrap(string, width=nwrap), collapse="\n")
}
swr <- Vectorize(swr)


transition_variable_names_common<-function(contract){
  for(i in 1:ncol(contract)){
    if(is.character(contract[,i])){
      print(colnames(contract)[i])
      contract[,i]<-factor(contract[,i])
    } 
  }
  
  #Dropping Redundant
  contract<-contract[,!colnames(contract) %in% c( "UnmodifiedNumberOfOffersSummary",
                                                  "SizeOfUnmodifiedContractBaseAndAll" 
  )]
  
      
  
  contract<-contract[,!colnames(contract) %in% c("n_Comp","cb_Comp",
                                                 "cn_Offr","cl_Offr",
                                                 "nq_Offr","NAICS5",
                                                 "NAICS4","SteadyScopeOptionGrowthAlone" ,
                                                 "SteadyScopeOptionGrowthMixed",
                                                 "ChangeOrderCeilingGrowth",
                                                 "SteadyScopeOptionRescision",
                                                 "AdminOptionModification",
                                                 "ChangeOrderOptionModification",
                                                 "EndingOptionModification",
                                                 "OtherOptionModification", "override_unmodified_ceiling",
                                                 "override_unmodified_base",
                                                 "override_change_order_growth",
                                                 "override_exercised_growth",
                                                 "j_Term",
                                                 "j_CBre",
                                                 "ln_CBre_Then_Year",
                                                 "n_Fixed",
                                                 "n_Incent",
                                                 "n_Nofee",
                                                 "UnmodifiedYearsFloat",
                                                 "b_ODoD",
                                                 "ODoD"
  )]
  
  contract<-contract[,!colnames(contract) %in% c( "l_US6_avg_sal_lag1Const",
                                                  "l_def6_obl_lag1Const" ,
                                                  "l_def3_obl_lag1Const",
                                                  # "capped_def6_ratio_lag1"  ,
                                                  # "capped_def3_ratio_lag1"  ,
                                                  "lp_OptGrowth",
                                                  "capped_cl_Days",
                                                  "lp_CBre" 
  )]
  
  
  if("cln_days" %in% colnames(contract))
    contract<-contract[,!colnames(contract) %in% c("capped_cl_Days")]
  
  if("Agency" %in% colnames(contract))
    contract<-contract[,!colnames(contract) %in% c("AgencyID")]
  
  
  if("ProductServiceOrRnDarea" %in% colnames(contract))
    contract<-contract[,!colnames(contract) %in% c("ProductsCategory",#"ProductOrServiceArea",
                                                   "ServicesCategory", "ProductOrServiceCode1L", "ProductOrServiceCode2L", "ProductOrServiceCode2R", 
                                                   "ProductOrServiceCode1L1R", #"Simple", "HostNation3Category",
                                                   "Unseperated", "IsService", 
                                                   "IsCatchAllCode", "DoDportfolioGroup", "DoDportfolio", "DoDportfolioCategory", 
                                                   "DoDportfolioSubCategory", "PlatformPortfolio", "isRnD1to5", "PBLscore", 
                                                   "IsPossibleReclassification", "IsPossibleSoftwareEngineering", "RnD_BudgetActivity", 
                                                   "ProductServiceOrRnDarea", "CanadaSector", "VAPortfolio", "ServArea", "IsRnDdefenseSystem", 
                                                   "Level1_Code", "Level1_Category", "Level2_Code", "Level2_Category" #"CrisisProductOrServiceArea",
    )]
  
  
  if("Office" %in% colnames(contract))  
    contract<-contract[,!colnames(contract) %in% c(#"DepartmentID", "AgencyID", "ContractingOfficeName", 
      "StartDate", "EndDate", "AddressLine1", "AddressLine2", 
      "AddressLine3", "AddressCity", "AddressState", "ZipCode", 
      "CountryCode", "Depot", "FISC", "TFBSOrelated",
      "CSIScreatedDate", "CSISmodifieddDate", "OCOcrisisScore"
    )
    ]
  
  if("Veh" %in% colnames(contract))
    contract<-contract[,!colnames(contract) %in% c("SIDV","MIDV","FSSGWAC","BPABOA")]
  
  #Text search, note we need the if any because if no results, then it gets rid of all columns
  if(any(grep("def[2,4-5]",colnames(contract))))
    contract<-contract[,-grep("def[2,4-5]",colnames(contract))]
  if(any(grep("US[2,4-5]",colnames(contract))))
    contract<-contract[,-grep("US[2,4-5]",colnames(contract))]
  
  
  if(any(duplicated(colnames(contract)))) stop("Duplicate Contract Name")
  contract
}

transition_variable_names_service<-function(contract){
  
  # if(!"ln_CBre" %in% colnames(contract))
  contract$ln_CBre<-contract$ln_CBre_OMB20_GDP18
  
  if(!"cln_US6sal" %in% colnames(contract))
    contract$cln_US6sal<-contract$cl_US6_avg_sal_lag1Const
  if(!"cln_PSCrate" %in% colnames(contract))
    contract$cln_PSCrate<-contract$cl_CFTE
  if(!"cp_OffPerf7" %in% colnames(contract))
    contract$cp_OffPerf7<-contract$c_pPBSC
  if(!"cp_OffPSC7" %in% colnames(contract))
    contract$cp_OffPSC7<-contract$c_pOffPSC
  if(!"cn_PairHist7" %in% colnames(contract))
    contract$cn_PairHist7<-contract$c_pairHist
  if(!"cln_PairCA" %in% colnames(contract))
    contract$cln_PairCA<-contract$cl_pairCA
  
  if(!"cln_OffObl7" %in% colnames(contract))
    contract$cln_OffObl7<-contract$cl_OffVol
  if(!"cp_PairObl7" %in% colnames(contract))
    contract$cp_PairObl7<-contract$c_pMarket
  if(!"cln_OffFocus" %in% colnames(contract))
    contract$cln_OffFocus<-contract$cl_office_naics_hhi_k
  
  if(!"cln_Def3HHI" %in% colnames(contract))
    contract$cln_Def3HHI<-contract$cl_def3_HHI_lag1
  if(!"cln_Def6HHI" %in% colnames(contract))
    contract$cln_Def6HHI<-contract$cl_def6_HHI_lag1
  if(!"clr_Def3toUS" %in% colnames(contract))
    contract$clr_Def3toUS<-contract$cl_def3_ratio_lag1
  if(!"clr_Def6toUS" %in% colnames(contract))
    contract$clr_Def6toUS<-contract$cl_def6_ratio_lag1
  if(!"cln_Def6Obl" %in% colnames(contract))
    contract$cln_Def6Obl<-contract$cl_def6_obl_lag1
  
  if(!"Comp" %in% colnames(contract))
    contract$Comp<-contract$Comp1or5
  # contract$Pricing<-contract$PricingUCA
  
  if(!"cln_Base" %in% colnames(contract))
    contract$cln_Base<-contract$cl_Base
  if(!"cln_Days" %in% colnames(contract))
    colnames(contract)[colnames(contract)=="capped_cl_Days"]<-"cln_Days"
  if(!"cln_Days" %in% colnames(contract))
    contract$cln_Days<-contract$cl_Days
  if(!"clr_Ceil2Base" %in% colnames(contract))
    contract$clr_Ceil2Base<-contract$cl_Base2Ceil
  if(!"NAICS6" %in% colnames(contract))
    contract$NAICS6<-contract$NAICS
  if(!"ServArea" %in% colnames(contract))
    contract$ServArea<-contract$CrisisProductOrServiceArea
  if(!"Place" %in% colnames(contract))
    contract$Place<-contract$PlaceCountryISO3
  contract
}

transition_variable_names_FMS<-function(contract){
  contract<-transition_variable_names_common(contract)
  
  #Services variables not using
  contract<-contract[,!colnames(contract) %in% c( "office_PBSCobligated_1year",
                                                  "office_numberofactions_1year",
                                                  "office_obligatedamount_7year"  ,
                                                  "pPBSC" ,
                                                  "cl_OffCA"
  )]
  
  #Crisis vriables not using
  contract<-contract[,!colnames(contract) %in% c( "OffCri",
                                                  "c_OffCri" 
  )]
  
                                           
  
  #Using new naming scheme
  
      colnames(contract)[colnames(contract)=="cl_US6_avg_sal_lag1Const"]<-"cln_US6sal"
    colnames(contract)[colnames(contract)=="cln_PSCrate"]<-"cl_CFTE"
    colnames(contract)[colnames(contract) %in% c("cn_pairHist7","c_pairHist","cn_pairHist")]<-"cn_PairHist7"
    colnames(contract)[colnames(contract)=="c_pOffPSC"]<-"cp_OffPSC7"
    colnames(contract)[colnames(contract)=="cl_pairCA"]<-"cln_PairCA"
    colnames(contract)[colnames(contract)=="office_entity_obligatedamount_7yearConst"]<-"cln_PairOBL"
    
    colnames(contract)[colnames(contract)=="cl_OffVol"]<-"cln_OffObl7"
    colnames(contract)[colnames(contract)=="cl_OffVol"]<-"cln_OffObl7"
    colnames(contract)[colnames(contract)=="c_pMarket"]<-"cp_PairObl7"
    colnames(contract)[colnames(contract)=="cl_office_naics_hhi_k"]<-"cln_OffFocus"
  
    
  
    colnames(contract)[colnames(contract)=="cl_def3_HHI_lag1"]<-"cln_Def3HHI"
    colnames(contract)[colnames(contract)=="cl_def6_HHI_lag1"]<-"cln_Def6HHI"
    colnames(contract)[colnames(contract)=="cl_def3_ratio_lag1"]<-"clr_Def3toUS"
    colnames(contract)[colnames(contract)=="cl_def6_ratio_lag1"]<-"clr_Def6toUS"
    colnames(contract)[colnames(contract)=="cl_def3_obl_lag1Const"]<-"cln_Def3Obl"
    colnames(contract)[colnames(contract)=="cl_def6_obl_lag1Const"]<-"cln_Def6Obl"
    
    
    
    colnames(contract)[colnames(contract)=="cl_Base"]<-"cln_Base"
    colnames(contract)[colnames(contract)=="cl_Ceil"]<-"cln_Ceil"
    colnames(contract)[colnames(contract)=="cl_Base2Ceil"]<-"clr_Ceil2Base"
    colnames(contract)[colnames(contract) %in% c("cl_Days","cl_Days_Capped")]<-"cln_Days"
    
    
    
    colnames(contract)[colnames(contract)=="NAICS"]<-"NAICS6"
    colnames(contract)[colnames(contract)=="CrisisProductOrServiceArea"]<-"ServArea"
    colnames(contract)[colnames(contract)=="PlaceCountryISO3"]<-"Place"
    
    
    
    #Picking versions of variabls for this model
    if("ln_CBre_OMB20_GDP18" %in% colnames(contract)){
      contract<-contract[,  !colnames(contract) %in% c("ln_Cbre")]
      colnames(contract)[colnames(contract)=="ln_CBre_OMB20_GDP18"]<-"ln_CBre"
    }
    
    
    if("Comp1or5" %in% colnames(contract)){
      contract<-contract[, !colnames(contract) %in% c("Comp") ]
      colnames(contract)[colnames(contract)=="Comp1or5"]<-"Comp"
    }
    
  
    if("PricingUCA" %in% colnames(contract)){
      contract<-contract[,!colnames(contract) %in% c("Pricing")]
      colnames(contract)[colnames(contract)=="PricingUCA"]<-"Pricing"
    }
    
    if(any(duplicated(colnames(contract)))) stop("Duplicate Contract Name")
  
  contract
}


get_coef_list<-function(limit=NULL){
  if(is.null(limit)) #All Variables
    coef_list<-list("(Intercept)"="(Intercept)",
                    "FMSAlways"="Initial FMS",
                    "FMSPost-Start"="Post-Start FMS",
                    "FMSFMS post-start"="Post-Start FMS",
                    "cl_US6_avg_sal_lag1Const"="Log(Det. Ind. Salary)",
                    "cln_US6sal"="Log(Det. Ind. Salary)",
                    "cl_CFTE"="Log(Serv. Code Invoice Rate)",
                    "cln_PSCrate"="Log(Serv. Code Invoice Rate)",
                    "c_pPBSC"="Office PBSA Prop.",
                    "cp_OffPerf7"="Office PBSA Prop.",
                    "c_pOffPSC"="Office Serv. Code Exp. %",
                    "pOffPSC"="ERROR, UNCENTERED Office Serv. Code Exp. %",
                    "cp_OffPSC7"="Office Serv. Code Exp. %",
                    "c_pairHist"="Paired Years",
                    "cn_PairHist7"="Paired Years",
                    "cl_pairCA"="Log(Paired Actions)",
                    "cln_PairCA"="Log(Paired Actions)",
                    "cln_PairObl7"="Log(Paired Obl.)",
                    
                    #Contract Controls
                    #Scope
                    "cl_Ceil"="Log(Init. Ceiling)",
                    "cln_Ceil"="Log(Init. Ceiling)",
                    "cl_Base"="Log(Init. Base)",
                    "cln_Base"="Log(Init. Base)",
                    "cl_Base2Ceil"="Log(Init. Ceiling:Base)",
                    "clr_Ceil2Base"="Log(Init. Ceiling:Base)",
                    "capped_cl_Days"="Log(Planned Dur.)",
                    "cl_Days"="Log(Planned Dur.)",
                    "cln_Days"="Log(Planned Dur.)",
                    
                    #Competition
                    "Comp1or51 offer"="Comp. w/ 1 Offer",
                    "Comp1or52-4 offers"="Comp. w/ 2-4 Offers",
                    "Comp1or55+ offers"="Comp. w/ 5+ Offers",
                    
                    "CompOffr1 offer"="Comp. w/ 1 Offer",
                    "CompOffr2 offers"="2 offers",
                    "CompOffr3-4 offers"="3-4 offers",
                    "CompOffr5+ offers"="Comp. w/ 5+ Offers",
                    
                    "Comp1 offer"="Comp. w/ 1 Offer",
                    "Comp2-4 offers"="Comp. w/ 2-4 Offers",
                    "Comp5+ offers"="Comp. w/ 5+ Offers",
                    
                    "Comp1 Offer"="Comp. w/ 1 Offer",
                    "Comp2-4 Offers"="Comp. w/ 2-4 Offers",
                    "Comp5+ Offers"="Comp. w/ 5+ Offers",
                    
                    "NoCompOffr2Urgency"="No Comp., Urgency",
                    "NoCompOffr2Other No"="No Comp., Other",
                    "NoCompOffr21 offer"="Comp. w/ 1 Offer",
                    "NoCompOffr25+ offers"="Comp. w/ 5+ Offers",
                    
                    "c_OffCri"="Cont. Office Crisis %",
                    "OffPlaceMixed"="Cont. Office 1-50% Intl",
                    "OffPlaceIntl"="Cont. Office 50%+ Intl.",
                    
                    
                    #Vehicle
                    "VehS-IDC"="S-IDC",
                    "VehM-IDC"="M-IDC",
                    "VehFSS/GWAC"="FSS/GWAC",
                    "VehBPA/BOA"="BPA/BOA",
                    #Pricing and UCA
                    "PricingUCAFFP"="FFP",
                    "PricingUCAOther FP"="Other Fixed-Price",
                    "PricingUCAIncentive"="Incentive Fee",
                    "PricingUCACombination or Other"="Comb./Other",
                    "PricingUCAOther CB"="Other Cost-Based",
                    "PricingUCAT&M/LH/FPLOE"="T&M/LH/FP:LoE",
                    "PricingUCAUCA"="UCA",
                    
                    "PricingFeeOther FP"="Other Fixed-Price",
                    "PricingFeeIncentive"="Incentive Fee",
                    "PricingFeeCombination or Other"="Comb./Other",
                    "PricingFeeOther CB"="Other Cost-Based",
                    "PricingFeeT&M/LH/FPLOE"="T&M/LH/FP:LoE",
                    "b_UCA"="UCA",
                    "PricingFFP"="FFP",
                    "PricingOther FP"="Other Fixed-Price",
                    "PricingIncentive"="Incentive Fee",
                    "PricingCombination or Other"="Comb./Other",
                    "PricingOther CB"="Other Cost-Based",
                    "PricingT&M/LH/FPLOE"="T&M/LH/FP:LoE",
                    "PricingUCA"="UCA",
                    
                    #Crisis and international
                    "CrisisARRA"="Crisis=Recovery Act",
                    "CrisisDis"="Crisis=Disaster",
                    "CrisisOCO"="Crisis=OCO",
                    "b_Intl"="Performed Abroad",
                    
                    #NAICS
                    "cl_def3_HHI_lag1"="Log(Subsector HHI)",
                    "cln_Def3HHI_lag1"="Log(Subsector HHI)",
                    "cln_Def3HHI" = "Log(Subsector HHI)",
                    
                    "cl_def3_ratio_lag1"="Log(Subsector DoD:U.S.)",
                    "clr_Def3toUS_lag1"="Log(Subsector DoD:U.S.)",
                    "clr_Def3toUS" = "Log(Subsector DoD:U.S.)",
                    
                    "cl_def6_HHI_lag1"="Log(Det. Ind. HHI)",
                    "cln_Def6HHI" = "Log(Det. Ind. HHI)",
                    "cln_Def6HHI_lag1"="Log(Det. Ind. HHI)",
                    
                    "cl_def6_ratio_lag1"="Log(Det. Ind. DoD:U.S.)",
                    "clr_Def6toUS_lag1"="Log(Det. Ind. DoD:U.S.)",
                    "clr_Def6toUS" = "Log(Det. Ind. DoD:U.S.)",
                    
                    "cl_def6_obl_lag1"="Log(Det. Ind. DoD Obl.)",
                    "cln_Def6Obl"="Log(Det. Ind. DoD Obl.)",
                    
                    
                    #Office
                    "cl_OffVol"="Log(Office Obl.)",
                    "cln_OffObl"="Log(Office Obl.)",
                    "cln_OffObl7"="Log(Office Obl.)",
                    
                    "cln_OffFocus"="Log(Office Focus)",
                    "cl_office_naics_hhi_k"="Log(Office Focus)",
                    
                    "cp_PairObl7"="Paired Share %",
                    "c_pMarket"="Paired Share %",
                    
                    #Budget
                    "dActual_Lead3"="&Delta;Actual('19 vs. '16)",
                    "dFYDP2_PB_Base"="&Delta;FYDP2('19 vs. '18 PB Base)",
                    "dFYDP2_Actual"="&Delta;FYDP2('19 vs. '16 Actual)",
                    "dPB_Base_Actual"="&Delta;PB Base('18 vs. '16 Actual)",
                    "PB_OCO"="PB OCO ('18)",
                    
                    "log(FYDP2 + 1)"="log(FYDP2+1)",
                    "log(FYDP2_Base + 1)"="log(FYDP2+1)",
                    
                    "MilDepArmy"="Army",
                    "MilDepAir Force"="Air Force",
                    "MilDepOther DoD"="Other DoD",
                    "log(Actual + 1)"="log(Actual+1)",
                    "log(Actual_Total + 1)"="log(Actual_Total+1)",
                    "log(PB_Base + 1)"="log(PB Base+1)",
                    "log(PB_OCO + 1)"="log(PB OCO+1)",
                    
                    #interations
                    
                    #Consolidation
                    "cl_def6_HHI_lag1:capped_cl_Days"="Log(Det. Ind. HHI):Log(Planned Dur.)",
                    "cl_def6_HHI_lag1:cl_def6_obl_lag1"="Log(Det. Ind. HHI):Log(Det. Ind. DoD Obl.)",
                    "cl_def3_HHI_lag1:cl_def3_ratio_lag1"="Log(Subsector HHI):Log(Subsector DoD:U.S.)",
                    "cl_def6_HHI_lag1:b_UCA"="Log(Det. Ind. HHI):UCA",
                    "cl_Ceil:b_UCA"="Log(Init. Ceiling):UCA",
                    
                    
                    
                    #Competition
                    "CompOffr1 offer:b_UCA"="1 offer:UCA",
                    "CompOffr2 offers:b_UCA"="2 offers:UCA",
                    "CompOffr3-4 offers:b_UCA"="3-4 offers:UCA",
                    "CompOffr5+ offers:b_UCA"="5+ offers:UCA",
                    
                    "NoCompOffr2Urgency:cl_Ceil"="No Comp., Urgency:Log(Init. Ceiling)",
                    "NoCompOffr2Other No:cl_Ceil"="No Comp., Other:Log(Init. Ceiling)",
                    "NoCompOffr21 offer:cl_Ceil"="Comp. w/ 1 Offer:Log(Init. Ceiling)",
                    "NoCompOffr25+ offers:cl_Ceil"="Comp. w/ 5+ Offers:Log(Init. Ceiling)",
                    
                    #Service Complexity
                    "cl_US6_avg_sal_lag1:PricingFeeOther FP"="Log(Det. Ind. Salary):Other Fixed-Price",
                    "cl_US6_avg_sal_lag1:PricingFeeIncentive"="Log(Det. Ind. Salary):Incentive Fee",
                    "cl_US6_avg_sal_lag1:PricingFeeCombination or Other"="Log(Det. Ind. Salary):Comb./Other",
                    "cl_US6_avg_sal_lag1:PricingFeeOther CB"="Log(Det. Ind. Salary):Other Cost-Based",
                    "cl_US6_avg_sal_lag1:PricingFeeT&M/LH/FPLOE"="Log(Det. Ind. Salary):T&M/LH/FP:LoE",
                    "cln_US6sal_lag1:PricingOther FP"="Log(Det. Ind. Salary):Other Fixed-Price",
                    "cln_US6sal_lag1:PricingIncentive"="Log(Det. Ind. Salary):Incentive Fee",
                    "cln_US6sal_lag1:PricingCombination or Other"="Log(Det. Ind. Salary):Comb./Other",
                    "cln_US6sal_lag1:PricingOther CB"="Log(Det. Ind. Salary):Other Cost-Based",
                    "cln_US6sal_lag1:PricingT&M/LH/FPLOE"="Log(Det. Ind. Salary):T&M/LH/FP:LoE",

                    
                    "cln_PSCrate:PricingOther FP"="Log(Serv. Code Invoice Rate):Other Fixed-Price",
                    "cln_PSCrate:PricingIncentive"="Log(Serv. Code Invoice Rate):Incentive Fee",
                    "cln_PSCrate:PricingCombination or Other"="Log(Serv. Code Invoice Rate):Comb./Other",                    
                    "cln_PSCrate:PricingOther CB"="Log(Serv. Code Invoice Rate):Other Cost-Based",
                    "cln_PSCrate:PricingT&M/LH/FPLOE"="Log(Serv. Code Invoice Rate):T&M/LH/FP:LoE",
                    "cln_PSCrate:PricingUCA"="Log(Serv. Code Invoice Rate):UCA",
                    
                    "cln_PSCrate:PricingUCAOther FP"="Log(Serv. Code Invoice Rate):Other Fixed-Price",
                    "cln_PSCrate:PricingUCAIncentive"="Log(Serv. Code Invoice Rate):Incentive Fee",
                    "cln_PSCrate:PricingUCACombination or Other"="Log(Serv. Code Invoice Rate):Comb./Other",                    
                    "cln_PSCrate:PricingUCAOther CB"="Log(Serv. Code Invoice Rate):Other Cost-Based",
                    "cln_PSCrate:PricingUCAT&M/LH/FPLOE"="Log(Serv. Code Invoice Rate):T&M/LH/FP:LoE",
                    "cln_PSCrate:PricingUCAUCA"="Log(Serv. Code Invoice Rate):UCA",
                    
                    
                    
                    #Office Capability
                    "c_pPBSC:cl_pairCA"="Office PBSA Prop.:Log(Paired Actions)",
                    "cp_OffPerf7:cln_PairCA"="Office PBSA Prop.:Log(Paired Actions)",
                    "cp_OffPerf7:cln_Days"="Office PBSA Prop.:Log(Planned Dur.)",
                    "c_pPBSC:cl_Days"="Office PBSA Prop.:Log(Planned Dur.)",
                    "c_pPBSC:c_pMarket"="Office PBSA Prop.:Paired Share %",
                    "cp_OffPerf7:cp_PairObl7"="Office PBSA Prop.:Paired Share %",
                    
                    "cln_OffObl7:pOffPSC" = "ERROR UNCENTERED Office Serv. Code Exp. %:Log(Office Obl.)",
                    "cln_OffObl7:cp_OffPSC7" = "Office Serv. Code Exp. %:Log(Office Obl.)",
                    "cp_OffPSC7:cln_OffObl7" = "Office Serv. Code Exp. %:Log(Office Obl.)",
                    "cln_OffFocus:pOffPSC" = "ERROR UNCENTERED Office Serv. Code Exp. %:Log(Office Focus)",
                    "cln_OffFocus:cp_OffPSC7" = "Office Serv. Code Exp. %:Log(Office Focus)",
                    "cp_OffPSC7:cln_OffFocus"="Office Serv. Code Exp. %:Log(Office Focus)",
                    
                    
                    
                    
                    
                    #Paired Relationship
                    "c_pairHist:PricingUCAOther FP"="Paired Years:Other Fixed-Price",
                    "c_pairHist:PricingUCAIncentive"="Paired Years:Incentive Fee",
                    "c_pairHist:PricingUCACombination or Other"="Paired Years:Comb./Other",
                    "c_pairHist:PricingUCAOther CB"="Paired Years:Other Cost-Based",
                    "c_pairHist:PricingUCAT&M/LH/FPLOE"="Paired Years:T&M/LH/FP:LoE",
                    "c_pairHist:PricingUCAUCA"="Paired Years:UCA",
                    "cn_PairHist7:PricingUCAOther FP"="Paired Years:Other Fixed-Price",
                    "cn_PairHist7:PricingUCAIncentive"="Paired Years:Incentive Fee",
                    "cn_PairHist7:PricingUCACombination or Other"="Paired Years:Comb./Other",
                    "cn_PairHist7:PricingUCAOther CB"="Paired Years:Other Cost-Based",
                    "cn_PairHist7:PricingUCAT&M/LH/FPLOE"="Paired Years:T&M/LH/FP:LoE",
                    "cn_PairHist7:PricingUCAUCA"="Paired Years:UCA",
                    "cn_PairHist7:PricingOther FP"="Paired Years:Other Fixed-Price",
                    "cn_PairHist7:PricingIncentive"="Paired Years:Incentive Fee",
                    "cn_PairHist7:PricingCombination or Other"="Paired Years:Comb./Other",
                    "cn_PairHist7:PricingOther CB"="Paired Years:Other Cost-Based",
                    "cn_PairHist7:PricingT&M/LH/FPLOE"="Paired Years:T&M/LH/FP:LoE",
                    "cn_PairHist7:PricingUCA"="Paired Years:UCA",
                    
                    #Non-Study Variale interactions
                    "VehS-IDC:b_Intl"="S-IDC:Performed Abroad",
                    "VehM-IDC:b_Intl"="M-IDC:Performed Abroad",
                    "VehFSS/GWAC:b_Intl"="FSS/GWAC:Performed Abroad",
                    "VehBPA/BOA:b_Intl"="BPA/BOA:Performed Abroad",
                    
                    "cl_Base:cl_Base2Ceil"="Log(Init. Base):Log(Init. Ceiling:Base)",
                    "cln_Base:clr_Ceil2Base"="Log(Init. Base):Log(Init. Ceiling:Base)",
                    "cl_Ceil:cl_Base2Ceil"="Log(Init. Ceiling):Log(Init. Ceiling:Base)",
                    "cln_Ceil:clr_Ceil2Base"="Log(Init. Ceiling):Log(Init. Ceiling:Base)",
                    "cp_PairObl7:cln_OffObl7"="Paired Share %:Log(Office Obligations+1)",
                    "cln_OffObl7:cln_OffFocus" = "Log(Office Obligations+1):Log(Office Focus)",
                    
                    
                    "FMSAlways:cln_Base"="Initial FMS:Log(Init. Base)",  
                    "FMSPost-Start:cln_Base"="Post-Start FMS:Log(Init. Base)",
                    "FMSFMS post-start:cln_Base"="Post-Start FMS:Log(Init. Base)"
                    
    ) else if(limit=="FMS_all"){
    coef_list<-list("(Intercept)"="(Intercept)",
                    "FMSAlways"="Initial FMS",
                    "FMSAlways FMS"="Initial FMS",
                    "FMSPost-Start"="Post-Start FMS",
                    "FMSFMS post-start"="Post-Start FMS",
                    
                    
                    #Contract Controls
                    #Scope
                    "cl_Ceil"="Log(Init. Ceiling)",
                    "cln_Ceil"="Log(Init. Ceiling)",
                    "cl_Base"="Log(Init. Base)",
                    "cln_Base"="Log(Init. Base)",
                    "cl_Base2Ceil"="Log(Init. Ceiling:Base)",
                    "clr_Ceil2Base"="Log(Init. Ceiling:Base)",
                    "capped_cl_Days"="Log(Planned Dur.)",
                    "cl_Days"="Log(Planned Dur.)",
                    "cln_Days"="Log(Planned Dur.)",
                    
                    #Competition
                    "Comp1or51 offer"="Comp. w/ 1 Offer",
                    "Comp1or52-4 offers"="Comp. w/ 2-4 Offers",
                    "Comp1or55+ offers"="Comp. w/ 5+ Offers",
                    
                    "CompOffr1 offer"="Comp. w/ 1 Offer",
                    "CompOffr2 offers"="2 offers",
                    "CompOffr3-4 offers"="3-4 offers",
                    "CompOffr5+ offers"="Comp. w/ 5+ Offers",
                    
                    "Comp1 offer"="Comp. w/ 1 Offer",
                    "Comp2-4 offers"="Comp. w/ 2-4 Offers",
                    "Comp5+ offers"="Comp. w/ 5+ Offers",
                    
                    
                    
                    
                    
                    #Vehicle
                    "VehS-IDC"="S-IDC",
                    "VehM-IDC"="M-IDC",
                    "VehFSS/GWAC"="FSS/GWAC",
                    "VehBPA/BOA"="BPA/BOA",
                    #Pricing and UCA
                    "PricingUCAFFP"="FFP",
                    "PricingUCAOther FP"="Other Fixed-Price",
                    "PricingUCAIncentive"="Incentive Fee",
                    "PricingUCACombination or Other"="Comb./Other",
                    "PricingUCAOther CB"="Other Cost-Based",
                    "PricingUCAT&M/LH/FPLOE"="T&M/LH/FP:LoE",
                    "PricingUCAUCA"="UCA",
                    
                    "PricingFeeOther FP"="Other Fixed-Price",
                    "PricingFeeIncentive"="Incentive Fee",
                    "PricingFeeCombination or Other"="Comb./Other",
                    "PricingFeeOther CB"="Other Cost-Based",
                    "PricingFeeT&M/LH/FPLOE"="T&M/LH/FP:LoE",
                    "b_UCA"="UCA",
                    "PricingFFP"="FFP",
                    "PricingOther FP"="Other Fixed-Price",
                    "PricingIncentive"="Incentive Fee",
                    "PricingCombination or Other"="Comb./Other",
                    "PricingOther CB"="Other Cost-Based",
                    "PricingT&M/LH/FPLOE"="T&M/LH/FP:LoE",
                    "PricingUCA"="UCA",
                    
                    #Crisis and international
                    "CrisisARRA"="Crisis=Recovery Act",
                    "CrisisDis"="Crisis=Disaster",
                    "CrisisOCO"="Crisis=OCO",
                    "b_Intl"="Performed Abroad",
                    
                    "cl_CFTE"="Log(Serv. Code Invoice Rate)",
                    "cln_PSCrate"="Log(Serv. Code Invoice Rate)",
                    
                    
                    #NAICS
                    "cl_def3_HHI_lag1"="Log(Subsector HHI)",
                    "cln_Def3HHI_lag1"="Log(Subsector HHI)",
                    "cln_Def3HHI" = "Log(Subsector HHI)",
                    
                    "cl_def3_ratio_lag1"="Log(Subsector DoD:U.S.)",
                    "clr_Def3toUS_lag1"="Log(Subsector DoD:U.S.)",
                    "clr_Def3toUS" = "Log(Subsector DoD:U.S.)",
                    
                    "cl_def6_HHI_lag1"="Log(Det. Ind. HHI)",
                    "cln_Def6HHI" = "Log(Det. Ind. HHI)",
                    "cln_Def6HHI_lag1"="Log(Det. Ind. HHI)",
                    
                    "cl_def6_ratio_lag1"="Log(Det. Ind. DoD:U.S.)",
                    "clr_Def6toUS_lag1"="Log(Det. Ind. DoD:U.S.)",
                    "clr_Def6toUS" = "Log(Det. Ind. DoD:U.S.)",
                    
                    "cl_def6_obl_lag1"="Log(Det. Ind. DoD Obl.)",
                    "cln_Def6Obl"="Log(Det. Ind. DoD Obl.)",
                    "cl_US6_avg_sal_lag1Const"="Log(Det. Ind. Salary)",
                    "cln_US6sal"="Log(Det. Ind. Salary)",
                    
                    
                    
                    #Office
                    "c_pPBSC"="Office PBSA Prop.",
                    "cp_OffPerf7"="Office PBSA Prop.",
                    "c_pOffPSC"="Office Serv. Code Exp. %",
                    "pOffPSC"="ERROR, UNCENTERED Office Serv. Code Exp. %",
                    "cp_OffPSC7"="Office Serv. Code Exp. %",
                    
                    
                    "cl_OffVol"="Log(Office Obl.)",
                    "cln_OffObl"="Log(Office Obl.)",
                    "cln_OffObl7"="Log(Office Obl.)",
                    
                    "cln_OffFocus"="Log(Office Focus)",
                    "cl_office_naics_hhi_k"="Log(Office Focus)",
                    
                    "c_pairHist"="Paired Years",
                    "cn_PairHist7"="Paired Years",
                    "cl_pairCA"="Log(Paired Actions)",
                    "cln_PairCA"="Log(Paired Actions)",
                    "cln_PairObl7"="Log(Paired Obl.)",
                    
                    "cp_PairObl7"="Paired Share %",
                    "c_pMarket"="Paired Share %",
                    
                    
                    
                    #interations
                    
                    #Consolidation
                    "cl_def6_HHI_lag1:capped_cl_Days"="Log(Det. Ind. HHI):Log(Planned Dur.)",
                    "cl_def6_HHI_lag1:cl_def6_obl_lag1"="Log(Det. Ind. HHI):Log(Det. Ind. DoD Obl.)",
                    "cl_def3_HHI_lag1:cl_def3_ratio_lag1"="Log(Subsector HHI):Log(Subsector DoD:U.S.)",
                    "cl_def6_HHI_lag1:b_UCA"="Log(Det. Ind. HHI):UCA",
                    "cl_Ceil:b_UCA"="Log(Init. Ceiling):UCA",
                    
                    #Competition
                    "CompOffr1 offer:b_UCA"="1 offer:UCA",
                    "CompOffr2 offers:b_UCA"="2 offers:UCA",
                    "CompOffr3-4 offers:b_UCA"="3-4 offers:UCA",
                    "CompOffr5+ offers:b_UCA"="5+ offers:UCA",
                    
                    
                    
                    #Service Complexity
                    "cl_US6_avg_sal_lag1:PricingFeeOther FP"="Log(Det. Ind. Salary):Other Fixed-Price",
                    "cl_US6_avg_sal_lag1:PricingFeeIncentive"="Log(Det. Ind. Salary):Incentive Fee",
                    "cl_US6_avg_sal_lag1:PricingFeeCombination or Other"="Log(Det. Ind. Salary):Comb./Other",
                    "cl_US6_avg_sal_lag1:PricingFeeOther CB"="Log(Det. Ind. Salary):Other Cost-Based",
                    "cl_US6_avg_sal_lag1:PricingFeeT&M/LH/FPLOE"="Log(Det. Ind. Salary):T&M/LH/FP:LoE",
                    "cln_US6sal_lag1:PricingOther FP"="Log(Det. Ind. Salary):Other Fixed-Price",
                    "cln_US6sal_lag1:PricingIncentive"="Log(Det. Ind. Salary):Incentive Fee",
                    "cln_US6sal_lag1:PricingCombination or Other"="Log(Det. Ind. Salary):Comb./Other",
                    "cln_US6sal_lag1:PricingOther CB"="Log(Det. Ind. Salary):Other Cost-Based",
                    "cln_US6sal_lag1:PricingT&M/LH/FPLOE"="Log(Det. Ind. Salary):T&M/LH/FP:LoE",
                    
                    
                    "PricingOther FP:cln_US6sal"="Log(Det. Ind. Salary):Other Fixed-Price",
                    "PricingIncentive:cln_US6sal"="Log(Det. Ind. Salary):Incentive Fee",
                    "PricingCombination or Other:cln_US6sal"="Log(Det. Ind. Salary):Comb./Other",
                    "PricingOther CB:cln_US6sal"="Log(Det. Ind. Salary):Other Cost-Based",
                    "PricingT&M/LH/FPLOE:cln_US6sal"="Log(Det. Ind. Salary):T&M/LH/FP:LoE",
                    "PricingUCA:cln_US6sal"="Log(Det. Ind. Salary):UCA",
                    
                    "cln_PSCrate:PricingOther FP"="Log(Serv. Code Invoice Rate):Other Fixed-Price",
                    "cln_PSCrate:PricingIncentive"="Log(Serv. Code Invoice Rate):Incentive Fee",
                    "cln_PSCrate:PricingCombination or Other"="Log(Serv. Code Invoice Rate):Comb./Other",
                    "cln_PSCrate:PricingOther CB"="Log(Serv. Code Invoice Rate):Other Cost-Based",
                    "cln_PSCrate:PricingT&M/LH/FPLOE"="Log(Serv. Code Invoice Rate):T&M/LH/FP:LoE",
                    "cln_PSCrate:PricingUCA"="Log(Serv. Code Invoice Rate):UCA",
                    
                    "cln_PSCrate:PricingUCAOther FP"="Log(Serv. Code Invoice Rate):Other Fixed-Price",
                    "cln_PSCrate:PricingUCAIncentive"="Log(Serv. Code Invoice Rate):Incentive Fee",
                    "cln_PSCrate:PricingUCACombination or Other"="Log(Serv. Code Invoice Rate):Comb./Other",                    
                    "cln_PSCrate:PricingUCAOther CB"="Log(Serv. Code Invoice Rate):Other Cost-Based",
                    "cln_PSCrate:PricingUCAT&M/LH/FPLOE"="Log(Serv. Code Invoice Rate):T&M/LH/FP:LoE",
                    "cln_PSCrate:PricingUCAUCA"="Log(Serv. Code Invoice Rate):UCA",
                    
                    #Office Capability
                    
                    "cp_OffPerf7:cln_PairCA"="Office PBSA Prop.:Log(Paired Actions)",
                    "c_pPBSC:cl_pairCA"="Office PBSA Prop.:Log(Paired Actions)",
                    "cp_OffPerf7:cln_Days"="Office PBSA Prop.:Log(Planned Dur.)",
                    "c_pPBSC:cl_Days"="Office PBSA Prop.:Log(Planned Dur.)",
                    "cp_OffPerf7:cp_PairObl7"="Office PBSA Prop.:Paired Share %",
                    "c_pPBSC:c_pMarket"="Office PBSA Prop.:Paired Share %",
                    
                    "cln_OffObl7:pOffPSC" = "ERROR UNCENTERED Office Serv. Code Exp. %:Log(Office Obl.)",
                    "cln_OffObl7:cp_OffPSC7" = "Office Serv. Code Exp. %:Log(Office Obl.)",
                    "cp_OffPSC7:cln_OffObl7" = "Office Serv. Code Exp. %:Log(Office Obl.)",
                    "cln_OffFocus:pOffPSC" = "ERROR UNCENTERED Office Serv. Code Exp. %:Log(Office Focus)",
                    "cln_OffFocus:cp_OffPSC7" = "Office Serv. Code Exp. %:Log(Office Focus)",
                    "cp_OffPSC7:cln_OffFocus"="Office Serv. Code Exp. %:Log(Office Focus)",
                    
                    
                    
                    #Paired Relationship
                    "c_pairHist:PricingUCAOther FP"="Paired Years:Other Fixed-Price",
                    "c_pairHist:PricingUCAIncentive"="Paired Years:Incentive Fee",
                    "c_pairHist:PricingUCACombination or Other"="Paired Years:Comb./Other",
                    "c_pairHist:PricingUCAOther CB"="Paired Years:Other Cost-Based",
                    "c_pairHist:PricingUCAT&M/LH/FPLOE"="Paired Years:T&M/LH/FP:LoE",
                    "c_pairHist:PricingUCAUCA"="Paired Years:UCA",
                    "cn_PairHist7:PricingUCAOther FP"="Paired Years:Other Fixed-Price",
                    "cn_PairHist7:PricingUCAIncentive"="Paired Years:Incentive Fee",
                    "cn_PairHist7:PricingUCACombination or Other"="Paired Years:Comb./Other",
                    "cn_PairHist7:PricingUCAOther CB"="Paired Years:Other Cost-Based",
                    "cn_PairHist7:PricingUCAT&M/LH/FPLOE"="Paired Years:T&M/LH/FP:LoE",
                    "cn_PairHist7:PricingUCAUCA"="Paired Years:UCA",
                    "cn_PairHist7:PricingOther FP"="Paired Years:Other Fixed-Price",
                    "cn_PairHist7:PricingIncentive"="Paired Years:Incentive Fee",
                    "cn_PairHist7:PricingCombination or Other"="Paired Years:Comb./Other",
                    "cn_PairHist7:PricingOther CB"="Paired Years:Other Cost-Based",
                    "cn_PairHist7:PricingT&M/LH/FPLOE"="Paired Years:T&M/LH/FP:LoE",
                    "cn_PairHist7:PricingUCA"="Paired Years:UCA",
                    
                    
                    #Non-Study Variale interactions
                    "VehS-IDC:b_Intl"="S-IDC:Performed Abroad",
                    "VehM-IDC:b_Intl"="M-IDC:Performed Abroad",
                    "VehFSS/GWAC:b_Intl"="FSS/GWAC:Performed Abroad",
                    "VehBPA/BOA:b_Intl"="BPA/BOA:Performed Abroad",
                    
                    "cl_Base:cl_Base2Ceil"="Log(Init. Base):Log(Init. Ceiling:Base)",
                    "cln_Base:clr_Ceil2Base"="Log(Init. Base):Log(Init. Ceiling:Base)",
                    "cl_Ceil:cl_Base2Ceil"="Log(Init. Ceiling):Log(Init. Ceiling:Base)",
                    "cln_Ceil:clr_Ceil2Base"="Log(Init. Ceiling):Log(Init. Ceiling:Base)",
                    "cp_PairObl7:cln_OffObl7"="Paired Share %:Log(Office Obligations+1)",
                    "cln_OffObl7:cln_OffFocus" = "Log(Office Obligations+1):Log(Office Focus)",
                    
                    
                    "FMSAlways:cln_Base"="Initial FMS:Log(Init. Base)",  
                    "FMSPost-Start:cln_Base"="Post-Start FMS:Log(Init. Base)",
                    "FMSFMS post-start:cln_Base"="Post-Start FMS:Log(Init. Base)"
    )
  } else if (limit=="services"){
    coef_list<-list("(Intercept)"="(Intercept)",
                    "cl_US6_avg_sal_lag1Const"="Log(Det. Ind. Salary)",
                    "cln_US6sal"="Log(Det. Ind. Salary)",
                    "cl_CFTE"="Log(Serv. Code Invoice Rate)",
                    "cln_PSCrate"="Log(Serv. Code Invoice Rate)",
                    "c_pPBSC"="Office PBSA Prop.",
                    "cp_OffPerf7"="Office PBSA Prop.",
                    "c_pOffPSC"="Office Serv. Code Exp. %",
                    "pOffPSC"="ERROR, UNCENTERED Office Serv. Code Exp. %",
                    "cp_OffPSC7"="Office Serv. Code Exp. %",
                    "c_pairHist"="Paired Years",
                    "cn_PairHist7"="Paired Years",
                    "cl_pairCA"="Log(Paired Actions)",
                    "cln_PairCA"="Log(Paired Actions)"
    )
  } else if (limit=="crisis"){
    coef_list<-list("(Intercept)"="(Intercept)",
                    "CrisisARRA"="Crisis=Recovery Act",
                    "CrisisDis"="Crisis=Disaster",
                    "CrisisOCO"="Crisis=OCO",
                    "NoCompOffr2Urgency"="No Comp., Urgency",
                    "NoCompOffr2Other No"="No Comp., Other",
                    "NoCompOffr21 offer"="Comp. w/ 1 Offer",
                    "NoCompOffr25+ offers"="Comp. w/ 5+ Offers",
                    "b_UCA"="UCA",
                    "c_OffCri"="Cont. Office Crisis %",
                    "OffPlaceMixed"="Cont. Office 1-50% Intl",
                    "OffPlaceIntl"="Cont. Office 50%+ Intl."
                    
    )
  }
  else stop(paste("Do not know how to process limit:",limit))
 
  if(any(duplicated(names(coef_list)))) stop("Duplicate names in coef_list")
  return(coef_list) 
}