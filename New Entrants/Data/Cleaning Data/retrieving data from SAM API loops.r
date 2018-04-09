library(jsonlite)
library(httr)
library(openxlsx)
library(plyr)
library(jsonlite)
library(data.table)
library(tidyverse)
installr::install.rtools()
Sys.setenv("R_ZIPCMD" = "C:/Rtools/bin/zip.exe")

##Marielle working Directory
setwd("K:/2018-01 NPS New Entrants/Data/Data")
getwd()

##Sam working Directory
setwd("K:.....")
getwd()


######## searchDataAPI pull

##### the Search pull provides the DUNs numbers within a range of dates (as well as 16 other fields)

expDate = seq(as.Date("2010-01-01"), as.Date("2011-12-31"), by="days") ##change date

base_url.search = "https://api.data.gov/sam/v2/registrations"
myapikey = "Xkyoz6sJxZgqNyChQoeMydse5BNFmWP53IqBZA93"
myapikey2 = "pEFRIdk6OwgiRzawvh34wU1MR7ERmw9Ro2PUuDVe"

url = paste(base_url.search,"?","fields=_all_","&","qterms=expirationDate:[",expDate,"]","&api_key=",myapikey, sep = "")
df.search = data.frame(matrix(NA, nrow = 0, ncol = 0))

for(i in 1:length(expDate)){
  if(x<=0){                          
    url = paste(base_url.search,"?","fields=_all_","&","qterms=expirationDate:[",expDate,"]","&api_key=",myapikey2, sep = "")
    y=y-1
    if(y == 0)
      break
  }
  while(TRUE){
    get.data <- try(fromJSON(url[i], flatten = TRUE))
    x=x-1
    if(!is(get.data, 'try-error')) break
  }
  message("retrieving query", i)
  df = as.data.frame(get.data$results)
  message("dataframe ", class(df), i)
  df.search=rbind.fill(df.search, df)
  message("appended frame", i)
  
  Sys.sleep(2)
}

df.unique = df.search[!duplicated(df.search),]        ##change name per year and below

write.xlsx(df.unique, "Search_unique_2010_11.xlsx", asTable = TRUE, col.names=TRUE)

######## getDataAPI pull

##### The Get pull takes the duns numbers from the search pulll and runs them through the API to get the full columns offered

getwd()
setwd("C:/Users/MRoth/Documents")
duns_nums = read.xlsx("Search_unique_2014_15.xlsx", cols = c(5,7))
duns4_nums = print(paste0(duns_nums[,1],duns_nums[,2], sep = ""))

base_url.get = "https://api.data.gov/sam/v4/registrations"


get.df = data.frame(matrix(NA, nrow = 0, ncol = 0))


url = paste(base_url.get,"/",duns4_nums,"?","return_values=full", "&api_key=",myapikey, sep = "")

f = as.integer((GET("https://api.data.gov/sam/v4/registrations/8784448350000?return_values=full&api_key=Xkyoz6sJxZgqNyChQoeMydse5BNFmWP53IqBZA93"))[["headers"]][["x-ratelimit-remaining"]])

g = as.integer((GET("https://api.data.gov/sam/v4/registrations/8784448350000?return_values=full&api_key=pEFRIdk6OwgiRzawvh34wU1MR7ERmw9Ro2PUuDVe"))[["headers"]][["x-ratelimit-remaining"]])
error = list()


for(i in 1:length(duns4_nums)){
  if(f <= 0){                          
    url = paste(base_url.get,"/",duns4_nums,"?","return_values=full", "&api_key=",myapikey2, sep = "")
    g = g-1
    if(g == 0)
          break
      }
  if(i %% 100 ==0){
    cat(paste0("completed", i))
  }
  get.data = try(fromJSON(url[i], flatten = TRUE))
  f=f-1
  if(class(get.data) != "try-error"){ 
    dt = t(unlist(get.data$sam_data$registration))
    df = as.data.frame(dt, col.names=names(dt), row.names = NULL)
    get.df=rbind.fill(get.df, df)
  } else if(class(get.data) == "try-error"){
    message("error", i)
    error = list(error, i)
  }
  Sys.sleep(2)
}

###to redo any errors

redo_nums = unlist(error)
redo_duns = duns4_nums[redo_nums]
redo_duns          ##### These are the duns numbers that produced an error
error2 = list()
url = paste(base_url.get,"/",redo_duns,"?","return_values=full", "&api_key=Xdud5Tkd55MqXFmpIGsxL6Z1W8GJkZxZ6f9ycc6W", sep = "")
####apply each seperately and add to data frame
for(i in 1:length(redo_nums)){
  get.data = try(fromJSON(url[i], flatten = TRUE))
  if(class(get.data) != "try-error"){ 
    message("retrieving query", i)
    dt = t(unlist(get.data$sam_data$registration))
    message(class(dt), i)
    df = as.data.frame(dt, col.names=names(dt), row.names = NULL)
    message(class(df), i)
    get.df=rbind.fill(get.df, df)
    message("appended frame", i)
  } else if(class(get.data) == "try-error"){
    message("error", i)
    error2 = list(error2, i)
  }
  Sys.sleep(2)
}
errornums = unlist(error2)
finalerrors = duns4_nums[errornums]  ###Final list of errors

#### The title changes with the above year setting


get.unique = get.df[!duplicated(get.df[,c('duns','dunsPlus4')]),]
write.csv(get.unique, "Get_2012_13.csv", asTable = TRUE, col.names=TRUE)



#####Loading the xlsx files sometimes caused problems in excel - if so, use write.csv
#####For importing into SQL Server, us write.table to produce txt
