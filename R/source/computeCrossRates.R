
# ## v II
# # Function to compute cross-rate matrix for each date
# computeCrossRates <- function(dt) {
#     
#     dt <- as.data.table(dt)  # ensure input is a data.table
#     dt <-dt[,.(PositionDate, PositionCurrency, FxEur=mean(FxEur, na.rm=TRUE)), by= .(PositionDate, PositionCurrency)]
#     
#     
#     # Self-join on PositionDate to pair each currency with each other on the same date
#     # 'i' prefix refers to the base currency side, 'x' prefix to the quote currency side
#     cross_long <- dt[dt, on=.(PositionDate), allow.cartesian=TRUE, 
#                          .(PositionDate = i.PositionDate,
#                            PositionCurrency = i.PositionCurrency,    # base currency (row)
#                            QuoteCurrency = x.PositionCurrency,       # quote currency (col)
#                            crossRate = i.FxEur / x.FxEur)]
#     # Pivot to wide format: one row per date/base, columns for each quote currency
#     cross_wide <- dcast(cross_long, PositionDate + PositionCurrency ~ QuoteCurrency, 
#                         value.var = "crossRate")
#     setorder(cross_wide, PositionDate, PositionCurrency)  # sort for readability
#     
#     
#     cols <- setdiff(names(cross_wide), c("PositionDate", "PositionCurrency"))
#     fxCrossRates<- cross_wide[, (cols) := lapply(.SD, function(x) na.locf(x, na.rm=FALSE)), .SDcols = cols]
#     
#     fxCrossRates <- melt(fxCrossRates, id.vars=c("PositionDate","PositionCurrency"), variable.name = "Fx", value.name= "rate")
#     setkey(fxCrossRates, PositionDate, PositionCurrency)
#     return(fxCrossRates)
# }


computeCrossRates <- function(dt) {
    
    dt <- as.data.table(dt)  # ensure input is a data.table
    
    # Compute average FxEur per PositionDate and PositionCurrency
    dt_avg <- dt[, .(FxEur = mean(FxEur, na.rm = TRUE)), by = .(PositionDate, PositionCurrency)]
    
    # Self-join to get all currency pairs per date (Cartesian join on PositionDate)
    cross_rates <- dt_avg[dt_avg, on = .(PositionDate), allow.cartesian = TRUE,
                          .(PositionDate,
                            PositionCurrency = i.PositionCurrency,  # base
                            QuoteCurrency = x.PositionCurrency,     # quote
                            rate = i.FxEur / x.FxEur)]
    
    # Optional: sort the result
    setorder(cross_rates, PositionDate, PositionCurrency, QuoteCurrency)
    
    # Apply LOCF per base-quote pair across dates (if needed)
    cross_rates[, rate := na.locf(rate, na.rm = FALSE), 
                by = .(PositionCurrency, QuoteCurrency)]
    
    setkey(cross_rates, PositionDate, PositionCurrency)
    
    return(cross_rates)
}

