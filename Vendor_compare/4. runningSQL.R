#################################################################
# Running SQL server in R
#################################################################

# Reference: https://www.mssqltips.com/sqlservertip/4880/sql-server-data-access-using-r--part-1/

library("RODBC") #package commonly used for SQL Server connections
library("dplyr")
library(readr)

# build a new connection
dbhandle <- odbcDriverConnect('driver={SQL Server};server=VMDatabase;database=DIIG;trusted_connection=true')

data <- sqlQuery(dbhandle, 'select * from [Contractor].[DunsnumberToParentContractorHistory]')

# save to csv file
write_csv(data, "DunsnumberToParentContractorHistory_20171024.csv")

