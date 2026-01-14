
###############################
### get all banks positions ###
###############################


getAllPositions <- function(bankNames, targetDirectory) {
    
    positions <- lapply(bankNames, 
                        function(x) getBankPositions(x, targetDirectory))
}
