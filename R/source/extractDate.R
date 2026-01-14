
# function to extract date from filename

extractDate <- function(file) {

    pattern <- "\\d{4}-\\d{1,2}-\\d{1,2}"

    match   <- regexpr(pattern, file)
    
    date    <- regmatches(file, match)

}   
