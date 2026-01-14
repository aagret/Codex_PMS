
## function to extract line by line datas 
formatQuintetFile <- function(fileList= fileList, dropLine = dropLine) {
    # clean first column
    db <- readLines(fileList)
    db <- gsub("\t", "", db)
    db <- gsub('^\\"|\\"$',"", db)
    
    db <- gsub("\\.", "", db) # remove thousand separator
    db <- gsub(",", ".", db) # change comma separator in numbers
    
    db <- strsplit(db[-dropLine], ";")
    db <- do.call("rbind.data.frame", c(db, stringsAsFactors=FALSE))
    colnames(db) <- gsub(" ", "", as.character(unlist(db[1, ])))
    db  <- db[-1, ]
}
