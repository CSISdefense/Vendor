## no signdate limitation

panel_data <- read_csv("K:/2018-01 NPS New Entrants/Data/Data/Cleaned Data/Panel data reg 2001-2011 - no parent filter.csv")
full_fpds <- read_csv("K:/2018-01 NPS New Entrants/Data/Data/Cleaned Data/SAM Data merged with FPDS, exp2000-2019.csv")
all_panel <- left_join(panel_data, full_fpds[c("duns","signeddate","registrationDate")], by = "duns")

###restart or continue through 10 year
#2001####
x2001.3.11 <- all_panel %>% 
  filter(year(registrationDate)==2001) %>% 
  filter(three.year == "TRUE") %>%   
  filter(year(signeddate) > 2011) %>% 
  arrange(signeddate)

x2001.3.11 <- x2001.3.11[!duplicated(x2001.3.11["duns"]),]

x2001.5.11 <- all_panel %>% 
  filter(year(registrationDate)==2001) %>% 
  filter(five.year == "TRUE") %>%   
  filter(year(signeddate) > 2011) %>% 
  arrange(signeddate)

x2001.5.11 <- x2001.5.11[!duplicated(x2001.5.11["duns"]),]

x2001.10.11 <- all_panel %>% 
  filter(year(registrationDate)==2001) %>% 
  filter(ten.year == "TRUE") %>%   
  filter(year(signeddate) > 2011) %>% 
  arrange(signeddate)

x2001.10.11 <- x2001.10.11[!duplicated(x2001.10.11["duns"]),]


x3 <- nrow(x2001.3.11)
x5 <- nrow(x2001.5.11)
x10 <- nrow(x2001.10.11)

df.01 <- as.data.frame(c(x3,x5,x10))
colnames(df.01) = "2001, beyond SD2011"
rownames(df.01) <- c("3","5","10")

#2002####
x2002.3.12 <- all_panel %>% 
  filter(year(registrationDate)==2002) %>% 
  filter(three.year == "TRUE") %>%   
  filter(year(signeddate) > 2012) %>% 
  arrange(signeddate)

x2002.3.12 <- x2002.3.12[!duplicated(x2002.3.12["duns"]),]

x2002.5.12 <- all_panel %>% 
  filter(year(registrationDate)==2002) %>% 
  filter(five.year == "TRUE") %>%   
  filter(year(signeddate) > 2012) %>% 
  arrange(signeddate)

x2002.5.12 <- x2002.5.12[!duplicated(x2002.5.12["duns"]),]

x2002.10.12 <- all_panel %>% 
  filter(year(registrationDate)==2002) %>% 
  filter(ten.year == "TRUE") %>%   
  filter(year(signeddate) > 2012) %>% 
  arrange(signeddate)

x2002.10.12 <- x2002.10.12[!duplicated(x2002.10.12["duns"]),]


x3 <- nrow(x2002.3.12)
x5 <- nrow(x2002.5.12)
x10 <- nrow(x2002.10.12)

df.02 <- as.data.frame(c(x3,x5,x10))
colnames(df.02) = "2002, beyond SD2012"
rownames(df.02) <- c("3","5","10")

#2003####
x2003.3.13 <- all_panel %>% 
  filter(year(registrationDate)==2003) %>% 
  filter(three.year == "TRUE") %>%   
  filter(year(signeddate) > 2013) %>% 
  arrange(signeddate)

x2003.3.13 <- x2003.3.13[!duplicated(x2003.3.13["duns"]),]

x2003.5.13 <- all_panel %>% 
  filter(year(registrationDate)==2003) %>% 
  filter(five.year == "TRUE") %>%   
  filter(year(signeddate) > 2013) %>% 
  arrange(signeddate)

x2003.5.13 <- x2003.5.13[!duplicated(x2003.5.13["duns"]),]

x2003.10.13 <- all_panel %>% 
  filter(year(registrationDate)==2003) %>% 
  filter(ten.year == "TRUE") %>%   
  filter(year(signeddate) > 2013) %>% 
  arrange(signeddate)

x2003.10.13 <- x2003.10.13[!duplicated(x2003.10.13["duns"]),]


x3 <- nrow(x2003.3.13)
x5 <- nrow(x2003.5.13)
x10 <- nrow(x2003.10.13)

df.03 <- as.data.frame(c(x3,x5,x10))
colnames(df.03) = "2003, beyond SD2013"
rownames(df.03) <- c("3","5","10")

#2004####
x2004.3.14 <- all_panel %>% 
  filter(year(registrationDate)==2004) %>% 
  filter(three.year == "TRUE") %>%   
  filter(year(signeddate) > 2014) %>% 
  arrange(signeddate)

x2004.3.14 <- x2004.3.14[!duplicated(x2004.3.14["duns"]),]

x2004.5.14 <- all_panel %>% 
  filter(year(registrationDate)==2004) %>% 
  filter(five.year == "TRUE") %>%   
  filter(year(signeddate) > 2014) %>% 
  arrange(signeddate)

x2004.5.14 <- x2004.5.14[!duplicated(x2004.5.14["duns"]),]

x2004.10.14 <- all_panel %>% 
  filter(year(registrationDate)==2004) %>% 
  filter(ten.year == "TRUE") %>%   
  filter(year(signeddate) > 2014) %>% 
  arrange(signeddate)

x2004.10.14 <- x2004.10.14[!duplicated(x2004.10.14["duns"]),]


x3 <- nrow(x2004.3.14)
x5 <- nrow(x2004.5.14)
x10 <- nrow(x2004.10.14)

df.04 <- as.data.frame(c(x3,x5,x10))
colnames(df.04) = "2004, beyond SD2014"
rownames(df.04) <- c("3","5","10")

#2005####
x2005.3.15 <- all_panel %>% 
  filter(year(registrationDate)==2005) %>% 
  filter(three.year == "TRUE") %>%   
  filter(year(signeddate) > 2015) %>% 
  arrange(signeddate)

x2005.3.15 <- x2005.3.15[!duplicated(x2005.3.15["duns"]),]

x2005.5.15 <- all_panel %>% 
  filter(year(registrationDate)==2005) %>% 
  filter(five.year == "TRUE") %>%   
  filter(year(signeddate) > 2015) %>% 
  arrange(signeddate)

x2005.5.15 <- x2005.5.15[!duplicated(x2005.5.15["duns"]),]

x2005.10.15 <- all_panel %>% 
  filter(year(registrationDate)==2005) %>% 
  filter(ten.year == "TRUE") %>%   
  filter(year(signeddate) > 2015) %>% 
  arrange(signeddate)

x2005.10.15 <- x2005.10.15[!duplicated(x2005.10.15["duns"]),]


x3 <- nrow(x2005.3.15)
x5 <- nrow(x2005.5.15)
x10 <- nrow(x2005.10.15)

df.05 <- as.data.frame(c(x3,x5,x10))
colnames(df.05) = "2005, beyond SD2015"
rownames(df.05) <- c("3","5","10")

#2006####
x2006.3.16 <- all_panel %>% 
  filter(year(registrationDate)==2006) %>% 
  filter(three.year == "TRUE") %>%   
  filter(year(signeddate) > 2016) %>% 
  arrange(signeddate)

x2006.3.16 <- x2006.3.16[!duplicated(x2006.3.16["duns"]),]

x2006.5.16 <- all_panel %>% 
  filter(year(registrationDate)==2006) %>% 
  filter(five.year == "TRUE") %>%   
  filter(year(signeddate) > 2016) %>% 
  arrange(signeddate)

x2006.5.16 <- x2006.5.16[!duplicated(x2006.5.16["duns"]),]

x2006.10.16 <- all_panel %>% 
  filter(year(registrationDate)==2006) %>% 
  filter(ten.year == "TRUE") %>%   
  filter(year(signeddate) > 2016) %>% 
  arrange(signeddate)

x2006.10.16 <- x2006.10.16[!duplicated(x2006.10.16["duns"]),]


x3 <- nrow(x2006.3.16)
x5 <- nrow(x2006.5.16)
x10 <- nrow(x2006.10.16)

df.06 <- as.data.frame(c(x3,x5,x10))
colnames(df.06) = "2006, beyond SD2016"
rownames(df.06) <- c("3","5","10")

df <- cbind(df.01, df.02, df.03, df.04, df.05, df.06)
write.csv(df, "count after 10 SD.csv")
