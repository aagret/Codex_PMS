
readQuintetFiles <- function(directory, pattern, dropLines = 1) {
    # List all matching files in the target directory
    fileList <- list.files(directory, pattern = pattern, full.names = TRUE)
    
    # Apply formatQuintetFile to each file with dropLines
    data <- rbindlist(lapply(fileList, formatQuintetFile, dropLines), fill = TRUE)
    
    return(data)
}
