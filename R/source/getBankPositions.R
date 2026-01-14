###################################
### get a single bank positions ###
###################################


getBankPositions <- function(bankName, targetDirectory) {
    
    targetDirectory <- as.list(targetDirectory)
    selectFunction  <- paste0("get", bankName, "Positions")
    
    positions <- do.call(selectFunction, targetDirectory)
    #positions[, BankName:= bankName]
    
    return(positions)
    
}
