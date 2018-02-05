library(jsonlite)
library(httr)
library(openxlsx)
library(plyr)

###Duns Number getDataAPI, Brute Force, 3 Variables
options(warn=1)
duns4_nums = c("0607047800000",
                 "0609024130000",
                 "7917925830000",
                 "0814668490000",
                 "0061730820000",
                 "0013679600000",
                 "0091225320000")
  
  
    ##"8847455300000",  --> not found
    ##"1963378640000",  -->forbidden
    ##"0013680830000",  --> not found

                         

base_url = "https://api.data.gov/sam/v4/registrations"
myapikey = "Xkyoz6sJxZgqNyChQoeMydse5BNFmWP53IqBZA93"


url = paste(base_url,"/",duns4_nums,"?","return_values=full", "&api_key=",myapikey, sep = "")

df_all = data.frame(matrix(NA, nrow = 0, ncol = 0))

for(i in 1:length(duns4_nums)){
xurl=url
get.data = fromJSON(URLencode(url[[i]]), flatten = TRUE)
dt = t(unlist(get.data$sam_data$registration))
df = as.data.frame(dt, col.names=names(dt), row.names = NULL)
df_all=rbind.fill(df_all, df)

#Sys.sleep(1)
}
