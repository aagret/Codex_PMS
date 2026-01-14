
# Function to compute and replace average fx rates
fillFxRates <- function(positions) {

    # --- EUR base currency logic ---
    positions[BaseCurrency == "EUR" & is.na(EvaluationEur), EvaluationEur := EvaluationCC]   # Fill missing EvaluationEur
    positions[BaseCurrency == "EUR" & is.na(EvaluationCC), EvaluationCC := EvaluationEur]    # Fill missing EvaluationCC
    positions[BaseCurrency == "EUR" & is.na(FxEur) & Evaluation != 0, FxEur := EvaluationEur / Evaluation] # Compute FxEur
    positions[BaseCurrency == "EUR" & (is.na(FxEur) | FxEur == 0), FxEur := FxCC]            # Fill missing/zero FxEur
    positions[BaseCurrency == PositionCurrency & BaseCurrency == "EUR", FxEur := 1]          # Set FxEur = 1 when currencies match
    positions[, FxEur := mean(FxEur[FxEur != 0], na.rm=TRUE), by = .(PositionDate, PositionCurrency)] # replace FxEur with average FxEur
    
    # --- CHF base currency logic ---
    positions[BaseCurrency == "CHF" & is.na(EvaluationChf), EvaluationChf := EvaluationCC]   # Fill missing EvaluationChf
    positions[BaseCurrency == "CHF" & is.na(EvaluationCC), EvaluationCC := EvaluationChf]    # Fill missing EvaluationCC
    positions[BaseCurrency == "CHF" & is.na(FxChf) & Evaluation != 0, FxChf := EvaluationChf / Evaluation] # Compute FxChf
    positions[BaseCurrency == "CHF" & (is.na(FxChf) | FxChf == 0), FxEur := FxCC]            # Fill missing/zero FxChf
    positions[BaseCurrency == PositionCurrency & BaseCurrency == "CHF", FxChf := 1]          # Set FxChf = 1 when currencies match
    positions[, FxChf := mean(FxChf[FxChf != 0], na.rm=TRUE), by = .(PositionDate, PositionCurrency)] # replace FxChf with average FxEur
    
    setkey(positions, PositionDate, PositionCurrency)
    
    return(positions)
    
}
