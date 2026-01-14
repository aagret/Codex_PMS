
# function to convert character fields to numeric

convertToNumeric <- function(positions) {
    
    fields <- c("CostPrice", "Evaluation", "EvaluationEur", "FxEur", "Position")
    
    positions[, (fields):= lapply(.SD, as.numeric), .SDcols= fields]
    
    return(positions)
    
}
