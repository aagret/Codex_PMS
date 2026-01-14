
## upload new datas to postgres
uploadPsql <- function () {
    
    con <- dbConnect(RPostgres::Postgres(),
                     dbname = 'PMS', 
                     host = '192.168.14.140', # i.e. 'ec2-54-83-201-96.compute-1.amazonaws.com'
                     port = 5432, # or any other port specified by your DBA
                     user = 'admin',
                     password = 'alexNEW.2539')
    
    
    upsertTable(con, "fxcrossrates", fxCrossRates, keyCols = c("PositionDate", "PositionCurrency", "QuoteCurrency"))
    
    upsertTable(con, "positions", positions, keyCols = c("PositionDate", "ClientNumber", "BaseCurrency", "PositionCurrency", "PositionName"))
    
    # Disconnect from the database
    dbDisconnect(con)
    
}
