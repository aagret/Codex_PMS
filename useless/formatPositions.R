
formatPostions <- function(positions){
    
    # classification by Asset Type
    positions[grepl("CC|75|Current Account|CASH", AssetType), 
              ':=' (AssetType= "Cash", AssetClass= "Cash")]
    
    positions[grepl("FWD", AssetType), 
              ':=' (AssetType= "FxFwd", AssetClass= "Cash")]
    
    positions[grepl("Dépôts", AssetType), 
              ':=' (AssetType= "St-Deposit", AssetClass= "Cash")]
    
    positions[grepl("IV", AssetType), 
              ':=' (AssetType= "Interest", AssetClass= "Cash")]
    
    positions[grepl("Contract", AssetType) & grepl("Swap|FX", PositionName), 
              ':=' (AssetType= "FxFwd", AssetClass= "Cash")]
    
    positions[grepl("Contract", AssetType) & grepl("card guarantee", PositionName), 
              ':=' (AssetType= "Guarantee", AssetClass= "Cash")]
    
    positions[is.na(AssetType) | grepl("SECURITY|Security", AssetType), 
              ':=' (AssetType= "Securities", AssetClass= "Securities")]
    
    # change Position Name
    positions[AssetClass == "Cash", PositionName:= AssetType]
    
    return(postions)
    
}
