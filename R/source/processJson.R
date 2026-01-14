
#####################################
### Process Swissquote Json datas ###
#####################################


###################
## Sub-Functions ##
###################

source("./R/source/extractDate.R")



###################
## main function ##
###################

processJson <- function(file, column) {
    
    json_data <- fromJSON(file)

    dt <- as.data.table(json_data[[column]]) # Extract positions (or securities if needed)
    dt$ClientNumber <- sub(".*swissquote_([0-9]+)_.*", "\\1", file)
    dt$PositionDate <- extractDate(file)
    
    return(dt)
    
}



