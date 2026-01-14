
## function to load or install all required packages

loadRequiredPackages <- function() {
    
    pkgs <- c("jsonlite", "data.table", "plyr", "openxlsx", "zoo", "RPostgres", "DBI")
    
    for (pkg in pkgs) {
        
        if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
        
        library(pkg, character.only = TRUE)
    }

}
