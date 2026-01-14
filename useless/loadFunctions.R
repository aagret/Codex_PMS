
############################
# Load necessary functions #
############################

# functions to extract datas from bank files
source("./R/source/getAllPositions.R")
    source("./R/source/getBankPositions.R")
        source("./R/source/getQuintetPositions.R")
            source("./R/source/formatQuintetFile.R")
        source("./R/source/getSwissQuotePositions.R")
            source("./R/source/processJson.R")
        source("./R/source/getEdrPositions.R")
        source("./R/source/getLodhPositions.R")  


# read nessary fields names from xls configuration file
source("./R/source/getFieldNames.R")

# format datas
source("./R/source/formatPositions.R")
    
source("./R/source/convertToNumeric.R")

source("./R/source/cleanPositions.R")
source("./R/source/fillFxRates.R")
source("./R/source/computeCrossRates.R")
source("./R/source/upsertSql.R")
