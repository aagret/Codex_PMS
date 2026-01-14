########################################
### select and convert common Fields ###
########################################


# function to drop useless datas and change headers names
formatPositions <- function(position, fields) {
    
    bankName   <- unique(position$BankName)
    
    keepFields <- c(as.character(unlist(fields[Keep == "Yes", ..bankName])), "BankName")
    
    position <- position[, ..keepFields]
    
    newNames <- c(fields[Keep == "Yes", Artha], "BankName")
    
    setnames(position, keepFields, newNames)
    
    position <- convertToNumeric(position)
    
    position[is.na(Evaluation), Evaluation:= Position]
    position[AssetPrice == 0, AssetPrice:= Evaluation / Position]
    
    return(position)
    
}
