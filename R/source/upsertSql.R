
#################
## upsert Data ##
#################


library(DBI)
library(data.table)

upsertTable <- function(con, table, dt, keyCols = c("PositionDate", "PositionCurrency")) {
    
    stopifnot(all(keyCols %in% names(dt)))
    
    # Ensure dt is data.table
    if (!is.data.table(dt)) dt <- as.data.table(dt)
    
    # use lower case columns names
    names(dt) <- tolower(names(dt))
    keyCols   <-tolower(keyCols)
    
    # Get existing table fields from database
    existingCols <- dbListFields(con, table)
    newCols <- setdiff(names(dt), existingCols)
    
    # Add missing columns
    for (col in newCols) {
        # Choose SQL type based on R column type
        val <- dt[[col]]
        if (is.integer(val))      sqlType <- "INTEGER"
        else if (is.numeric(val)) sqlType <- "NUMERIC"
        else if (inherits(val, "Date")) sqlType <- "DATE"
        else if (inherits(val, "POSIXct")) sqlType <- "TIMESTAMP"
        else                         sqlType <- "VARCHAR"
        
        alterSql <- sprintf("ALTER TABLE %s ADD COLUMN %s %s;", table, dbQuoteIdentifier(con, col), sqlType)
        dbExecute(con, alterSql)
    }
    
    # Write data to a temporary table
    tmpTable <- paste0("tmp_", as.integer(Sys.time()))
    dbWriteTable(con, tmpTable, dt, overwrite = TRUE)
    
    # Prepare column lists
    allCols <- names(dt)
    allColsSql <- paste(sapply(allCols, dbQuoteIdentifier, conn = con), collapse = ", ")
    updateCols <- setdiff(allCols, keyCols)
    updateSql <- paste(
        sprintf("%s = EXCLUDED.%s", 
                sapply(updateCols, dbQuoteIdentifier, conn = con), 
                updateCols
        ),
        collapse = ",\n    "
    )
    keyColsSql <- paste(sapply(keyCols, dbQuoteIdentifier, conn = con), collapse = ", ")
    
    # Compose and execute UPSERT SQL
    upsertSql <- sprintf(
        
        "INSERT INTO %s (%s)
        SELECT %s FROM %s
        ON CONFLICT (%s)
        DO UPDATE SET %s;",
        
        dbQuoteIdentifier(con, table),
        allColsSql,
        allColsSql,
        dbQuoteIdentifier(con, tmpTable),
        keyColsSql,
        updateSql
    )
    
    dbExecute(con, upsertSql)
    
    # Drop temporary table
    dbExecute(con, sprintf("DROP TABLE %s", dbQuoteIdentifier(con, tmpTable)))
    
}


