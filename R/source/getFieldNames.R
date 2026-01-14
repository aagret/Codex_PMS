# function to retrieve all fieldnames for every bank from xls file

getFieldNames <- function(workingDir) {
    
    file <- paste0(workingDir,"/Config/databaseFields.xlsx")
    
    fields <-read.xlsx(file, colNames= TRUE)
    
    setDT(fields)
    
}
