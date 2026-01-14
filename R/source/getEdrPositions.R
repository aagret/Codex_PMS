
#######################
### format Edr Datas ##
#######################

###################
## main function ##
###################


getEdrPositions <- function(targetDirectory) {
  
  
  # get all positions
  ###################
  
  ## select file name
  fileName  <- "EDR.*positions|positions.*EDR"
  #regex <- "(?=.*pattern1)(?=.*pattern2)"
  
  dropLine  <- 0
  
  ## list all new files
  fileList <- list.files(targetDirectory, pattern= fileName, full.names = TRUE)
  if (length(fileList) == 0) return(invisible(NULL))
  
  ## extract positions
  positions <- rbindlist(lapply(fileList, fread, encoding = "Latin-1"))
  
  # remove zero positions
  positions <- positions[Quantity != 0, ]
  
  # format date
  positions[, `Reference date`:= as.Date(`Reference date`, format="%d.%m.%Y")]
  
  # complete missing datas
  positions[`Position type` == "Security", AssetClass:= "Securities"]
  positions[`Position type` != "Security", AssetClass:= "Cash"]

  
  positions[, Evaluation:=    `Accrued Interests in position currency` + 
              `Valuation in position currency`]
  
  positions[, EvaluationCC:=  `Accrued Interests in BP currency` + 
              `Valuation in BP currency`]
  
  positions[, EvaluationChf:= `Accrued Interests in BU currency` + 
              `Valuation in BU currency`]
  
  # create isin if missing
  positions[ISIN == ''& grepl("Guar|guar", `Asset Name`), ISIN:= paste0("guar-", `Position ISO Currency`)]
  positions[ISIN == ''& grepl("Fx Swap|FX Swap", `Asset Name`), ISIN:= paste0("fwdFx-", `Position ISO Currency`)]
  positions[ISIN == ''& grepl("Client à Vue", `Asset Name`), ISIN:= paste0("CC-", `Position ISO Currency`)]
  positions[ISIN == ''& grepl("Fiduciary", `Asset Name`), ISIN:= paste0("call-", `Position ISO Currency`)]
  
  # complete missing columns
  positions[, EvaluationEur:= NA]
  positions[, FxEur:= NA]
  positions[, BankName:= "Edr"]
  
  

  
  return(positions)
  
  }
