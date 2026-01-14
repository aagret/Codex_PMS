
###############################
### format SwissQuote Datas ###
###############################


getSwissquotePositions <- function(targetDirectory) {
    
    # select file name
    fileName  <- "swissquote"
    
    ## list all new files
    fileList <- list.files(targetDirectory, pattern= fileName, full.names = TRUE)
    if (length(fileList) == 0) return(invisible(NULL))
    
    # Read all JSON files and merge
    positions  <- rbindlist(lapply(fileList, processJson, "positions"),  fill = TRUE)
    
    # remove 0 positions
    positions <- positions[qty !=0, ]
    
#    setnames(positions, "currency", "PositionCurrency")
    setkey(positions, PositionDate, securityId)
    
    securities <- rbindlist(lapply(fileList, processJson, "securities"), fill = TRUE)
    securities <- securities[!is.na(securityId)]
    securities[,PositionDate:= NULL]
    securities <- unique(securities[,-"ClientNumber"])
    setkey(securities, securityId)
    
    # merge positions and securities infos
    #setnames(securities, "currency", "PositionCurrency")
    
    tokeep <- positions[ is.na(securityId), -"securityGroup"]
    tofill <- positions[!is.na(securityId), -"currency"]
    
    tofill <- merge(securities,tofill)

    positions <- rbind(tofill, tokeep, fill=TRUE)
    
    #complete missing datas
    positions[type == "CASH", isin:=  paste0("CC-", currency)]
    positions[is.na(securityId), securityId:= isin]
    # complete positions with securties details
    
    
    # split complete/incomplete
    #toComplete <- positions[!is.na(securityId)][, -"PositionCurrency"]
    #toKeep     <- positions[is.na(securityId)]

 
    # add client currency to cash postions
    clientCur <- positions[!is.na(refCurrency), .(fill_cur = unique(refCurrency)[1L]), by= ClientNumber]
    positions[is.na(refCurrency), refCurrency := clientCur[.SD, on= .(ClientNumber), fill_cur]]
    
    
    
    # merge both 
 #   positions  <- securities[toComplete, on= c("PositionDate", "securityId")]
 #   positions  <- setDT(rbind.fill(toKeep,positions), key = "PositionDate")
 #   positions  <- positions[qty != 0]
    #remove duplicate columns
 #   positions[, c("i.securityGroup"):= NULL]
    
    # add type & class
    positions[type == "SECURITY", AssetClass:= "Securities"]
    positions[type == "CASH",     AssetClass:= "Cash"]
    
    # chhnce cash price
    positions[AssetClass == "Cash", evaluationPrice:= 1]
    positions[AssetClass == "Cash", name:= paste0("Cash-",currency)]
    #positions[AssetClass == "Cash", isin:= name]
    positions[AssetClass == "Cash", currentCost:= qty * evaluationPrice]
    
    # extract costs
    positions[, CostPrice:= get(grep(refCurrency, names(.SD), value=TRUE)), by= refCurrency]
    positions[, PositionDate:= as.Date(PositionDate, format= "%Y-%m-%d")]
    positions[AssetClass == "Cash", CostPrice:= 1]
    
    # complete missing datas
    positions[refCurrency == currency, currentCostInRefCurrency:= currentCost]

    positions[, FxCC:= currentCostInRefCurrency / currentCost]
    
    positions[refCurrency == "CHF", FxChf:= FxCC]
    positions[, FxEur:= ifelse(currency == "EUR", 1, FxChf / mean(positions[currency== "EUR", FxChf]))]
    
    positions[, EvaluationChf:= currentCost * FxChf]
    positions[, EvaluationEur:= currentCost * FxEur]
    positions[, BankName:= "Swissquote"]
    
    return(positions)
    
}


