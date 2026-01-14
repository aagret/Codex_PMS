
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
    fileName  <- "CITE"
    #regex <- "(?=.*pattern1)(?=.*pattern2)"
    
    dropLine  <- 3
    
    ## list all new files
    fileList <- list.files(targetDirectory, pattern= fileName, full.names = TRUE)
    if (length(fileList) == 0) return(invisible(NULL))
    
    ## extract positions
    positions <- rbindlist(lapply(fileList, function(x) {
        db <- fread(x,  skip = dropLine)
        dt <- regmatches(x, regexpr("\\d{8}", x))
        dt <- as.Date(dt, format="%Y%m%d")
        db[, ValueDate:= dt]
        return(db)
        }
        ))
    
    # 
    # colnames(db) <- c("SecName", "Isin", "Curncy", "Weight", "Client")
    # 
    # db <- db[SecName != "Description"]
    # db[, Weight:=as.numeric(Weight) * 100, ]
    # 
    # # assimilate Fidu & CC to cash
    # db[grepl("Fid|COMPT|COMPTE|ACCOUNT|REVENU", SecName), ':=' (Isin= paste0("cash", Curncy), SecName= paste0("cash ",Curncy))]
    # 
    # # format fx fwd
    # fxFwd <- db[grep("Achat|Vente", SecName), ]
    # 
    # # remove fxFwd from database
    # db <- db[!db$SecName %in% fxFwd$SecName ]
    # 
    # # find maturity dates in SecName
    # dt <- regexpr("Taux", fxFwd$SecName) - 11
    # dt <- substr(fxFwd$SecName, dt, dt + 9)
    # dt <- as.Date(dt, "%d.%m.%Y")
    # dt <- format(dt, "%d.%m.%Y")
    # 
    # isin <- substr(fxFwd$SecName, 7, 13)
    # isin <- paste("fxFwd", isin, dt, sep = "-")
    # 
    # newName  <- paste("fwd", fxFwd$Curncy, dt, sep=" ")
    # 
    # fxFwd$SecName <- newName
    # fxFwd$Isin    <- isin
    # 
    # # re merge both
    # db <- rbind(db, fxFwd)
    # 
    # # add bank name
    # db$Bank <- "CITE"
    # db$SecName <- NULL
    # 
    return(positions)
    
}
