#DIIGstat.r
library(arm)

#This will likely be folded into CSIS360
#But for now, using it to create and refine functions for regression analysis.

transform_contract<-function(
  contract
){
#PSR_What
contract$PSR_What<-factor(paste(as.character(contract$PSR),
                                             as.character(contract$What),sep="."))
contract$PSR_What[contract$PSR_What=="Unlabeled"]<-NA

#b_Crai
contract$b_Crai<-ifelse(contract$pChangeOrderUnmodifiedBaseAndAll>0,1,NA)
contract$b_Crai[contract$pChangeOrderUnmodifiedBaseAndAll<=0]<-0

#Create a jittered version of Crai for display purposes
#Unlike geom_jitter, this caps values at 0 and 1
contract$j_Crai<-jitter.binary(contract$b_Crai)


#b_Term
contract$b_Term<-ifelse(contract$Term=="Terminated",1,NA)
contract$b_Term[contract$Term=="Unterminated"]<-0

#Create a jittered version of Term for display purposes
#Unlike geom_jitter, this caps values at 0 and 1
contract$j_Term<-jitter.binary(contract$b_Term)

#n_Crai
#Should include this in the original data frame but for now can drive it.
contract$n_Crai<-contract$pChangeOrderUnmodifiedBaseAndAll*
  contract$UnmodifiedContractBaseAndAllOptionsValue

#l_Crai
contract$l_Crai<-NA
contract$l_Crai[contract$b_Crai==1 & !is.na(contract$b_Crai)]<-
  log(contract$n_Crai[contract$b_Crai==1 & !is.na(contract$b_Crai)])

#l_Ceil
contract$l_Ceil<-log(contract$UnmodifiedContractBaseAndAllOptionsValue)
contract$l_Ceil[is.infinite(contract$l_Ceil)]<-NA

#l_Days
contract$l_Days<-log(contract$UnmodifiedDays)
contract$l_Days[is.infinite(contract$l_Days)]<-NA



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
       "0.5"=c("Combination or Other"),
       "0"=c("Award Fee", "FFP or No Fee", "Fixed Fee"))
contract$n_Incent<-as.integer(as.character(contract$n_Incent))


#n_Comp
#Right now comp is not actually a factor, so don't need to process it
contract$b_Comp<-contract$Comp #Fix in Rdata, and add back comp
contract$n_Comp<-contract$b_Comp
contract$n_Comp[contract$b_Comp==1 & !is.na(contract$b_Comp)]<-
  ifelse(contract$SingleOffer[contract$b_Comp==1 & !is.na(contract$b_Comp)]=="Multi",2,1)

contract$n_Offr<-contract$Offr
levels(contract$n_Offr) <-
  list("1"=c("  1"), 
       "2"=c("  2"),
       "3"=c("[  3,  5)"),
       "4"=c("[  5,999]"))
contract$n_Offr<-as.integer(as.character(contract$n_Offr))
contract$n_Offr[contract$b_Comp==0 & !is.na(contract$b_Comp)]<-0




summary(DefenseModelAndDetail$Offr)

#n_Intl
contract$n_Intl<-contract$Intl
contract$n_Intl[contract$n_Intl=="Unlabeled"]<-NA
levels(contract$n_Intl) <-
  list("0"=c("Just U.S."), 
       "1"=c("Any International"))
contract$n_Intl<-as.integer(as.character(contract$n_Intl))



#n_UCA
contract$n_UCA<-contract$UCA
levels(contract$n_UCA) <-
  list("0"=c("Not UCA"), 
       "1"=c("UCA"))
contract$n_UCA<-as.integer(as.character(contract$n_UCA))




#l_Offer
contract$l_Offer<-log(contract$UnmodifiedNumberOfOffersReceived)
contract$l_Offer[is.infinite(contract$l_Days)]<-NA

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
  bin<-rank(data[,colnames(data)==rank_col],ties.method=ties.method)
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
# Term_01D_line_FxCb<-ggplot(data=subset(Term_smp,!is.na(l_Offer) & !is.na(FxCb)) %>% 
#                              group_by(bin_Offer_FxCb,FxCb) %>% 
#                              summarise (mean_Term = mean(b_Term),
#                                         mean_l_Offer =mean(l_Offer)),
#                            aes(y=mean_Term,x=mean_l_Offer))+geom_point()+facet_wrap(~FxCb)
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


Term_02A_res<-binned.resids (Term_02A_data$cl_Ceil,
                             Term_02A_data$fitted-Term_02A_data$b_Term, nclass=40)$binned

ggplot(data=Term_02A_res,
       aes(x=xbar,y-ybar))+
  geom_point(aes(y=ybar))+ #Residuals
  geom_line(aes(y=se2),col="grey")+
  geom_line(aes(y=-se2),col="grey")+
  labs(title="Binned residual plot",
       x="Centered Log(Ceiling)",
       y="Average residual")
