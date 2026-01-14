
############################
### format Quintet Datas ###
############################


getQuintetPositions <- function(targetDirectory) {
    
    
    ## get Securities positions metric
    ##################################
    
    fileName <- "SolCptTit"
    dropLine <- 1
    
    ## list all new files
    fileList <- list.files(targetDirectory, pattern= fileName, full.names = TRUE)
    if (length(fileList) == 0) return(invisible(NULL))
    
    ## extract Securities positions
    positionsTitres <- rbindlist(lapply(fileList, formatQuintetFile, dropLine))
    
    # remove closed positions
    positionsTitres <- positionsTitres[as.numeric(Balance) != 0, ]
    
    # complete missing datas
    positionsTitres[, AssetClass:= "Securities"]

    positionsTitres[, AssetPrice:= as.numeric(CashEvaluation) / as.numeric(Balance)]
    # change rose grove price
    positionsTitres[ISINCode == "XD0035937404", 
                    `:=` (CashEvaluation= as.numeric(CashEvaluation) / 1000,
                         Countervalue= as.numeric(Countervalue) / 1000)]
    
    setnames(positionsTitres, "Countervalue", "CounterValue")
    positionsTitres[, FxEur:= as.numeric(CounterValue) / as.numeric(CashEvaluation)]
   
    positionsTitres[CashEvaluationCurrency == "", CashEvaluationCurrency:= CurrencyCode]

    
    ## get Cash positions metric
    ############################
    
    fileName   <- "SolCptCsh"
    dropLine <- c(1,3)
    
    ## list all new files
    fileList <- list.files(targetDirectory, pattern= fileName, full.names = TRUE)
    if (length(fileList) == 0) return(invisible(NULL))
    
    ## extract Cash positions
    positionsCash <- rbindlist(lapply(fileList, formatQuintetFile, dropLine))
    
    # removed 0 balance
    positionsCash <- positionsCash[as.numeric(AccountingBalance) != 0, ]

    # modify duplicates column names
    colnames(positionsCash)[c(2,3,6)] <- 
        c("Balance", "CounterValue", "Balance at Value Date Exchange Value")

    # rename columns
    setnames(positionsCash, "AccountType", "TypeofFinancialInstrument")
    setnames(positionsCash, "Accountcurrency", "CashEvaluationCurrency")
    
    # complete missing datas
    positionsCash[, AssetClass:= "Cash"]
    positionsCash[, CashEvaluation:= Balance]
    positionsCash[, AssetPrice:= 1]
    positionsCash[, WeightedAverageCost:= 1]
    positionsCash[, FxEur:= as.numeric(ExchangeValuesRate)]

    positionsCash[, ISINCode:= paste(TypeofFinancialInstrument, CashEvaluationCurrency, sep="-")]
    positionsCash[, DescriptionofSecurity:= ISINCode]
    
    
    # merge cash & securities positions
    ###################################
    positions <- rbind.fill(positionsCash, positionsTitres)
    setDT(positions)
    
    
    # format dates
    positions[, ValueDate:= as.Date(ValueDate, format= "%Y-%m-%d")]

    # complete missing columns
    positions[, BaseCurrency:=  NA]
    positions[, EvaluationChf:= NA]
    positions[, EvaluationCC:=  NA]
    positions[, FxChf:= NA]
    positions[, FxCC:= NA]
    
    # complete missing datas
    positions[, BankName:= "Quintet"]
    positions[, ClientNumber:= ClientID]
    positions[, AssetPrice:= as.numeric(CashEvaluation) / as.numeric(Balance)]
    positions[is.na(CounterValue), CounterValue:=  as.numeric(CashEvaluation) * FxEur]
    
    
    #add baseCurrency if missing
    clientProfiles <- setDT(read.xlsx(paste0(workingDir,"/Clients/profiles.xlsx"),
                                      , colNames= TRUE))
    
    setkey(positions, ClientNumber)
    setkey(clientProfiles, ClientNumber)
    
    positions <- clientProfiles[positions]
    positions[, BankName:= i.BankName]
    positions[is.na(BaseCurrency), BaseCurrency:=i.BaseCurrency]
    
    # remove double i.datas
    positions$i.BankName <- NULL
    positions$i.BaseCurrency <- NULL
    positions$i.ClientName   <- NULL
    
    # replace 75 & IV
  #  positions[, ISINCode:= sub("75|IV", "CC", ISINCode)]   
  #  positions[, DescriptionofSecurity:= sub("75|IV", "CC", DescriptionofSecurity)]
    
    # convert to numeric
    fields <- c("WeightedAverageCost", "CashEvaluation", "CounterValue", "Balance")
    positions[, (fields):= lapply(.SD, as.numeric), .SDcols= fields]

    # round to 4 digits
    positions[, AssetPrice:= round(AssetPrice, 4)]

    return(positions)
    
}
