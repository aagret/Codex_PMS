
#######################
### format LODH Datas ##
#######################

###################
## main function ##
###################


getLodhPositions <- function(targetDirectory) {
    
    
    # get all positions
    ###################
    
    ## select file name
    fileName  <- "LODH"
    #regex <- "(?=.*pattern1)(?=.*pattern2)"
    
    dropLine  <- 8
    
    ## list all new files
    fileList <- list.files(targetDirectory, pattern= fileName, full.names = TRUE)
    if (length(fileList) == 0) return(invisible(NULL))
    
    ## extract positions
    positions <- rbindlist(lapply(fileList, function(x) {
        positions <- fread(x,  skip = dropLine)
        dt <- regmatches(x, regexpr("\\d{8}", x))
        dt <- as.Date(dt, format="%Y%m%d")
        positions[, ValueDate:= dt]
        return(positions)
        }
        ))
    
#    colnames(positions) <- c("Client", "Isin", "SecName", "Curncy", "Weight")
    
    positions <- positions[!is.na('N° client'), ]
    
    # assimilate Fidu & CC to cash
    positions[grepl("Fid|COMPT|COMPTE|REVENU", Libellé), ':=' ('Code ISIN'= paste0("cash", Devise), 
                                                               Libellé= paste0("cash ", Devise))]
    
    # format fx fwd
    fxFwd <- positions[grep("Achat|Vente", Libellé), ]
    
    # remove fxFwd from database
    positions <- positions[!positions$Libellé %in% fxFwd$Libellé ]
    
    # find maturity dates in SecName
    dt <- regexpr("Taux", fxFwd$Libellé) - 11
    dt <- substr(fxFwd$Libellé, dt, dt + 9)
    dt <- as.Date(dt, "%d.%m.%Y")
    dt <- format(dt, "%d.%m.%Y")
    
    isin <- substr(fxFwd$Libellé, 7, 13)
    isin <- paste("fxFwd", isin, dt, sep = "-")
    
    newName  <- paste("fwd", fxFwd$Devise, dt, sep=" ")
    
    fxFwd$Libellé <- newName
    fxFwd$'Code ISIN'    <- isin
    
    # re merge both
    positions <- rbind(positions, fxFwd)
    
    # add bank name
    positions$BankName <- "LODH"
    #positions$SecName <- NULL
    
    # Encoding(positions$Type) <- Encoding(positions$SecName) <- "unknown"
    
    
    return(positions)
    
}
