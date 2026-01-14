
cleanPositions <- function(positions){
# 
#     # classification by Asset Type
#     positions[grepl("CC|75|Current Account|CASH", AssetType),
#               ':=' (AssetType= "Cash", AssetClass= "Cash")]
# 
#     positions[grepl("FWD", AssetType),
#               ':=' (AssetType= "FxFwd", AssetClass= "Cash")]
# 
#     positions[grepl("Dépôts", AssetType),
#               ':=' (AssetType= "St-Deposit", AssetClass= "Cash")]
# 
#     positions[grepl("IV", AssetType),
#               ':=' (AssetType= "Interest", AssetClass= "Cash")]
# 
#     positions[grepl("Contract", AssetType) & grepl("Swap|FX", PositionName),
#               ':=' (AssetType= "FxFwd", AssetClass= "Cash")]
# 
#     positions[grepl("Contract", AssetType) & grepl("card guarantee", PositionName),
#               ':=' (AssetType= "Guarantee", AssetClass= "Cash")]
# 
#     positions[is.na(AssetType) | grepl("SECURITY|Security", AssetType),
#               ':=' (AssetType= "Securities", AssetClass= "Securities")]
    
    positions[is.na(Evaluation), Evaluation:= Position]
    positions[AssetPrice == 0, AssetPrice:= Evaluation / Position]
    
    return(positions)
    
}
