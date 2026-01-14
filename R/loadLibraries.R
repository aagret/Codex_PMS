
###########################
# Load necessary packages #
###########################
    
loadLibraries <- function () {
    
    # data.table: Extension of 'data.frame'
    if(!requireNamespace("data.table", quietly= TRUE)) 
    { install.packages("data.table") }

    # Tools for Splitting, Applying and Combining Data
    if(!requireNamespace("plyr", quietly= TRUE)) 
    { install.packages("plyr") }

    # Read, Write and Edit xlsx Files
    if(!requireNamespace("openxlsx", quietly= TRUE)) 
    { install.packages("openxlsx") }

    # JSON Parser and Generator    
    if(!requireNamespace("jsonlite", quietly= TRUE)) 
    { install.packages("jsonlite") }

    # S3 Infrastructure for Regular and Irregular Time Series 
    if(!requireNamespace("zoo", quietly= TRUE)) 
    { install.packages("zoo") }

    # C++ Interface to PostgreSQL
    if(!requireNamespace("RPostgres", quietly= TRUE)) 
    { install.packages("RPostgres") }
    
    library(data.table)
    library(plyr)
    library(openxlsx)
    library(jsonlite)
    library(zoo)
    library(RPostgres)
    
}
