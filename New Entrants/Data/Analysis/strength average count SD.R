#strength of argument//avg number of contracts 
##in 10 year span

panel_dataSD <- read_csv("Panel Data reg 2001-2011 - nop%2C 10plus1 year view.csv")
full_fpds <- read_csv("SAM Data merged with FPDS, exp2000-2019.csv")
all_panelSD <- left_join(panel_dataSD, full_fpds[c("duns","signeddate","registrationDate")], by = "duns")



###getting signeddates in t year span = all transactions in t year span####

#2001 ####

#3 year firm####

x2001.3 = all_panelSD %>% 
  filter(year(registrationDate.x) == 2001) %>% 
  filter(year(signeddate) <= 2004)

#5 year firms####

x2001.5 = all_panelSD %>% 
  filter(year(registrationDate.x) == 2001) %>% 
  filter(year(signeddate) <= 2006)

#10 year firms####

x2001.10 = all_panelSD %>% 
  filter(year(registrationDate.x) == 2001) %>% 
  filter(year(signeddate) <= 2010)

##find how many unique duns numbers and how many total transactions####
#2001 total duns 3
x2001.3.duns <- panel_dataSD %>%
  filter(year(registrationDate) == 2001) %>% 
  filter(three.year == "TRUE")

x2001.3.duns <- nrow(x2001.3.duns)
#2001 total dun 5
x2001.5.duns <- panel_dataSD %>% 
  filter(year(registrationDate) == 2001) %>% 
  filter(five.year == "TRUE")

x2001.5.duns <- nrow(x2001.5.duns)
#2001 total sun 10
x2001.10.duns <- panel_dataSD %>% 
  filter(year(registrationDate) == 2001) %>% 
  filter(ten.year == "TRUE")

x2001.10.duns <- nrow(x2001.10.duns)
#2001 total trans 3
x2001.3.trans <- x2001.3 %>% 
  filter(three.year == "TRUE")

x2001.3.trans <- nrow(x2001.3.trans)
#2001 total trans 5
x2001.5.trans <- x2001.5 %>% 
  filter(five.year == "TRUE")

x2001.5.trans <- nrow(x2001.5.trans)
#2001 total trans 10
x2001.10.trans <- x2001.10 %>% 
  filter(ten.year == "TRUE")

x2001.10.trans <- nrow(x2001.10.trans)


x2001.3.avg <- (x2001.3.duns*3)/x2001.3.trans

x2001.5.avg <-(x2001.5.duns*5)/x2001.5.trans

x2001.10.avg <- (x2001.10.duns*10)/x2001.10.trans

x2001.3.trans/(x2001.3.duns*3)
rbind(x2001.3.avg, x2001.5.avg, x2001.10.avg)
### bind each created dataset together

all.dataset.parentno <- rbind(dataset.2001, dataset.2002, dataset.2003, dataset.2004, dataset.2005, dataset.2006, dataset.2007, dataset.2008, dataset.2009, dataset.2010, dataset.2011)

# ###filtered parent
# 
# write.csv(dataset.2001, "dataset2001.csv")
# write.csv(dataset.2002, "dataset2002.csv")
# write.csv(dataset.2003, "dataset2003.csv")
# write.csv(dataset.2004, "dataset2004.csv")
# write.csv(dataset.2005, "dataset2005.csv")
# write.csv(dataset.2006, "dataset2006.csv")

#write.csv(all.dataset, "Panel Data reg 2001-2011.csv")

write.csv(all.dataset.parentno, "Panel Data reg 2001-2011 - nop, 10plus1 year view.csv")
getwd()
