

# define function to extract dates
extractDates <- function(myString) {
    
    # extract dates in format dd.mm.yy or dd.mm.yyyy
    dates <- regmatches(myString, 
                        gregexpr("(\\d{1,2}[.]\\d{1,2}[.]\\d{2,4})", 
                                 myString))[[1]]
    
    # format dates as yyyy-mm-dd
    dates <- as.Date(dates, format = c("%d.%m.%y", "%d.%m.%Y"))
    dates <- format(max(dates), "%d.%m.%Y")
    dates <- max(dates)
    
    # return vector of dates
    return(dates)
    
}
