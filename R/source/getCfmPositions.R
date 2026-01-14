
# get all positions
###################

getCfmPositions <- function(targetDirectory) {
    
    ## select file name
    fileName  <- "poti"
    
    ## list all new files
    fileList <- list.files(targetDirectory, pattern= fileName, full.names = TRUE)
    if (length(fileList) == 0) return(invisible(NULL))
    
    ## extract positions
    securities <- rbindlist(lapply(fileList, fread))
    
    
    colnames(securities) <- c("ClientNumber", "x1", "x2", "Isin" ,"PositionName",
                             "Weight", "AssetType", "PositionCurrency", "Position",
                             "AssetPrice", "Evaluation", "x3", "EvaluationCC",
                             "PriceDate", "Cost", "CostPriceCC", "AccruedInterest",
                             "x4", "PositionDate", "x5", "x6", "x7")
    
    # mNge double isin for subscriptions
    securities[duplicated(Isin) & AssetPrice == 100, PositionName:= paste(PositionName, "-NEW") ]
    
    
    setkey(securities, PositionDate, ClientNumber, Isin)
    
    
    ## select file name
    fileName  <- "poes"
    
    ## list all new files
    fileList <- list.files(targetDirectory, pattern= fileName, full.names = TRUE)
    if (length(fileList) == 0) return(invisible(NULL))
    
    ## extract positions
    cash <- rbindlist(lapply(fileList, fread))
    
    
    colnames(cash) <- c("ClientNumber", "x8", "AssetType", "PositionCurrency",
                             "Weight", "Evaluation", "EvaluationCC", "x9",
                             "Iban", "x10", "xCurrency", "PositionDate", "x7", "x7'")
    
    setkey(cash, PositionDate, ClientNumber)    
    
    positions <- rbind(cash,securities, fill=TRUE)
    
    # get base currencies
    fileName <- "fort"
    
    ## list all new files
    fileList <- list.files(targetDirectory, pattern= fileName, full.names = TRUE)
    if (length(fileList) == 0) return(invisible(NULL))
    
    ## extract positions
    accounts <- rbindlist(lapply(fileList, fread))[,1:3]
    colnames(accounts) <- c("ClientNumber", "ClientName", "BaseCurrency")
    accounts <- unique(accounts)
    
    setkey(accounts, ClientNumber)
    
    positions <- accounts[positions, on="ClientNumber"]

    ## complete datas and reformat

    positions[, ClientNumber:= as.character(ClientNumber)]
    positions[grepl("COMPTE", AssetType), `:=` 
              (PriceDate= PositionDate, Position= Evaluation, AssetPrice= 1,
                  Cost= 1, Isin= paste0("CC-", PositionCurrency),
                  PositionName= paste0("CC-", PositionCurrency))]
    
    positions <- positions[Position != 0]
    
#    positions <- positions[Position != 0, ] 
    positions[Isin=="LU1901202280", PositionName:= paste0(PositionName, " Equity Fund")]
    positions[Isin=="LU1901202363", PositionName:= paste0(PositionName, " Credit Fund")]
    positions[Isin=="LU1693257310" & AssetPrice==100, `:=` (PositionName= paste0(PositionName, "-sub"),
                                                            Isin= paste0(Isin, "-sub"))]
    positions[Isin=="XD1521244883", PositionName:= paste0(PositionName, "-11/25")]
    positions[Isin=="LU0410168768", PositionName:= "MSIF - Global Convertible"]
    positions[Isin=="LU0712123511", PositionName:= "MSIF - Global Fixed Income Opportunities"]
    
    positions[, FxCC:= ifelse(Position == 0, 1, Evaluation/EvaluationCC)]  

    
    positions[, AssetClass:= ifelse(grepl("COMPTE", AssetType), "Cash", "Securities")]

    
    positions[, PositionDate:= as.Date(sprintf("%08d", positions$PositionDate),
                                       format="%d%m%Y")]
    
    positions[, PriceDate:= as.Date(sprintf("%08d", positions$PriceDate),
                                    format="%d%m%Y")]
    
    positions[!is.na(AccruedInterest), Evaluation:= Evaluation + AccruedInterest]
    positions[!is.na(AccruedInterest), EvaluationCC:= Evaluation / FxCC]
    
    positions[, FxEur:= NA]
    positions[, FxChf:= NA]
    positions[, EvaluationEur:= NA]
    positions[, EvaluationChf:= NA]
    positions[, BankName:="Cfm"]
    
    
       
}
