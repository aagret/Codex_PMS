
# format securites database

securities <- unique(positions[, .(Isin, PositionName, PositionCurrency)])

securities[grepl("75-", Isin),
           ':=' (Isin= gsub("75-", "fwdCash-", Isin),
                 PositionName= gsub("75-", "fwdCash-", PositionName))]

securities[grepl("IV-", Isin),
           ':=' (Isin= gsub("IV-", "accrued-", Isin),
                 PositionName= gsub("IV-", "accrued-", PositionName))]

securities[grepl("FWD-", Isin),
           ':=' (Isin= gsub("FWD-", "fwdCash-", Isin),
                 PositionName= gsub("FWD", "fwdCash-", PositionName))]

securities[grepl("Cash-", Isin),
           ':=' (Isin= gsub("Cash-", "CC-", Isin),
                 PositionName= gsub("Cash", "CC-", PositionName))]


securities[grepl("Dépôts/Emprunts à terme-", Isin),
           ':=' (Isin= gsub("Dépôts/Emprunts à terme-", "deposit-", Isin),
                 PositionName= gsub("Dépôts/Emprunts à terme-", "deposit-", PositionName))]

# rename fx swaps
securities[grepl("FX Swap", PositionName), Isin:= paste0("fwdFx-", PositionCurrency)]

# rename cash
securities[grepl("Client à Vue", PositionName), 
           ':=' (Isin= paste0("CC-", PositionCurrency),
                 PositionName= paste0("CC-", PositionCurrency))]

# rename cash
securities[grepl("guarantee", PositionName), 
           ':=' (Isin= paste0("guar-", PositionCurrency),
                 PositionName= paste0("guar-", PositionCurrency))]

# remove duplicates
securities <- unique(securities)
isin <- as.list(unique(securities$Isin))

write.csv(isin, "./Securities/listIsin.csv")

## after Bloomberg data completion of securities file
securitiesDb <- read.xlsx("./Securities/securities.xlsx", sheet=2)

# test
securities[!Isin %in% securitiesDb$Isin, ]

## upload new2 datas to postgres
con <- dbConnect(RPostgres::Postgres(),
                 dbname = 'PMS', 
                 host = '192.168.14.140', # i.e. 'ec2-54-83-201-96.compute-1.amazonaws.com'
                 port = 5432, # or any other port specified by your DBA
                 user = 'admin',
                 password = 'alexNEW.2539')


upsertTable(con, "securities", securitiesDb, keyCols = c("Isin","Ticker","Ccy"))

# Disconnect from the database
dbDisconnect(con)

