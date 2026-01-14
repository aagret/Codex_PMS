
# Function to compute cross-matrix for one date group
extractFxCross <- function(positions) {
    
    db <- positions[, .(PositionCurrency, FxEur)]
    db <- db[, FxEur:= mean(FxEur, na.rm=TRUE), by= PositionCurrency]
    db <- unique(db)
    
    vals <- db$FxEur
    names(vals) <- db$PositionCurrency
    
    fxCross <- outer(vals, vals, `/`)
    fxCross <- as.data.table(fxCross)
    setnames(fxCross, db$PositionCurrency) ##
    
    fxCross[, PositionCurrency := db$PositionCurrency]
    # fxCross[, value := db$value]
    fxCross[, PositionDate := db$PositionDate[1]]
    
    setcolorder(fxCross, c("PositionDate", "PositionCurrency", positions$PositionCurrency))
    
    return(fxCross)
    
}


fxCross <- positions[, extractFxCross(.SD), by = PositionDate]


dt <- data.table(
    date = as.Date(c("2025-01-01", "2025-01-01", "2025-01-01",
                     "2025-01-02", "2025-01-02", "2025-01-02")),
    names = c("A", "B", "C", "A", "B", "C"),
    value = c(10, 20, 40, 5, 10, 20)
)


# Function to compute cross-matrix for one date group
make_ratio_matrix <- function(sub_dt) {
    
    vals <- sub_dt$value
    names(vals) <- sub_dt$names
    
    ratio_mat <- outer(vals, vals, `/`)
    ratio_dt <- as.data.table(ratio_mat)
    #setnames(ratio_dt, sub_dt$names)
    
    ratio_dt[, names := sub_dt$names]
   # ratio_dt[, value := sub_dt$value]
    ratio_dt[, date := sub_dt$date[1]]
    
    setcolorder(ratio_dt, c("date", "names", sub_dt$names))
    
    return(ratio_dt)
}

# Apply function by date group
result <- dt[, make_ratio_matrix(.SD), by = date]
sub_dt <- dt[date==unique(date)[1]]

# function to calc average Fx cross rates to use

fxCross <- function(positions) {
    
    Fx <- positions[, mean(FxEur, na.rm=TRUE), by= PositionCurrency]
    
    matrix <- outer(Fx$V1, Fx$V1, "/")
    cross  <- as.data.table(matrix)
    setnames(cross, Fx$PositionCurrency)
    cross[, names:= Fx$PositionCurrency]
    setcolorder(cross, c("names", Fx$PositionCurrrency))
    
}

