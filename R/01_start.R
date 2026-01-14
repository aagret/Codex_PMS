
##########
## Init ##
##########

# set directories
workingDir <- "/home/Alexandre/PMS"
setwd(workingDir)

# set directory for raw source
targetDirectory <- paste0(workingDir,"/rawData/newData")

# Load necessary packages
source("./R/loadLibraries.R")

library(data.table)
library(plyr)
library(openxlsx)
library(jsonlite)
library(zoo)
library(RPostgres)

# load functions
source("./R/loadFunctions.R")


###################
## get positions ##
###################

# check if exisitng bank positions updates
if (length(list.files(targetDirectory)) == 0) stop()

# extract positions from banks files
bankNames <- list("Quintet", "Edr", "Swissquote", "Cfm")
#                  "Lodh")#, "Cite", "Oddo", "Pictet")
#bankNames <- list("Quintet", "Edr", "Swissquote", "Lodh")
#positions <- getBankPositions(bankNames, targetDirectory)
positions <- lapply(bankNames, 
                    function(x) getBankPositions(x, targetDirectory))

# remove null if no file for a bank
positions <- Filter(Negate(is.null), positions)

# select fields and format data
fields    <- getFieldNames(workingDir)
positions <- setDT(ldply(positions, formatPositions, fields))

# fill missing fxRates
positions <- fillFxRates(positions)

# remove useless datas
#positions <- cleanPositions(positions)

# compute daily cross fx rates
fxCrossRates <- computeCrossRates(positions)

# keep only main datas
positions <- positions[, .(PositionDate, BankName, ClientNumber, BaseCurrency,
                          Isin, PositionName, AssetClass, AssetType, PositionCurrency,
                          Position, AssetPrice, CostPrice, Evaluation)]
### avoid doubles
#positions <- positions[, `:=` (Position= sum(Position), Evaluation=sum(Evaluation))]

# remove duplicates
positions <- unique(positions)


##################
## save dataset ##
##################

fwrite(positions, "./tidyData/newPositions.csv")
fwrite(positions, "./tidyData/fxCrossRates.csv")


###########################
## update to postgres DB ##
###########################

# connect to postgres
con <- dbConnect(RPostgres::Postgres(),
                 dbname = 'PMS', 
                 host = '192.168.14.140', # i.e. 'ec2-54-83-201-96.compute-1.amazonaws.com'
                 port = 5432, # or any other port specified by your DBA
                 user = 'admin',
                 password = 'alexNEW.2539')

#update tables
upsertTable(con, "fxcrossrates", fxCrossRates, 
            keyCols = c("PositionDate", "PositionCurrency", "QuoteCurrency"))

upsertTable(con, "positions", positions, 
            keyCols = c("PositionDate", "ClientNumber", "BaseCurrency", "PositionCurrency", "PositionName"))

# Disconnect from the database
dbDisconnect(con)

# delete all new files in directory
unlink("./rawData/newData/*")


## USE only to delete/recreate database

# names(fxCrossRates) <- tolower(names(fxCrossRates))
#colnames(fxCrossRates) <- tolower(colnames(fxCrossRates))
#dbWriteTable(conn = con, name = "fxcrossrates", value = fxCrossRates, overwrite=TRUE)

#colnames(positions) <- tolower(colnames(positions))
#dbWriteTable(conn = con, name = "positions", value = positions, overwrite=TRUE)

