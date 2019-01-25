

#************************************************************************************#
#function#
calc_gradrates <- function(x) {
  denominator <- length(which(x$top_smallbiz_bin==1))
  numerator_1 <- length(which(x$graduated==1))
  numerator_2 <- length(which(x$graduated==1 & x$survive_10yr==0)) 
  numerator_3 <- length(which(x$graduated==1 & x$survive_10yr==1)) 
  numerator_4 <- length(which(x$graduated==1 & x$survive_10yr==1 & x$survive_2016==0)) 
  numerator_5 <- length(which(x$graduated==1 & x$survive_2016==1))
  
  gradrate_1 <- numerator_1/denominator ##all small NE
  gradrate_2 <- numerator_2/denominator ##NE that did not survive 10 years
  gradrate_3 <- numerator_3/denominator ##NE that did survive 10 years
  gradrate_4 <- numerator_4/denominator ##NE that did not survive in 2016
  graduate_5 <- numerator_5/denominator ##NE that did survive in 2016
  
  allrates <- c(gradrate_1, gradrate_2, gradrate_3, gradrate_4, graduate_5)
  allrates
  
}

#loop#

datalist <- list(data_2001=data_2001, data_2001_DoD=data_DOD_2001, 
                 data_2002=data_2002, data_2002_DoD=data_2002_DOD, 
                 data_2003=data_2003, data_2003_DoD=data_2003_DOD, 
                 data_2004=data_2004, data_2004_DoD=data_2004_DOD, 
                 data_2005=data_2005, data_2005_DoD=data_2005_DOD, 
                 data_2006=data_2006, data_2006_DoD=data_2006_DOD)
gradrates <- lapply(datalist, calc_gradrates)

gradrates