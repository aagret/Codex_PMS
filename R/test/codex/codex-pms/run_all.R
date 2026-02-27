#!/usr/bin/env Rscript

run_with_log <- function(script_path, name) {
  cat(sprintf("\n[%s] START %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), name))
  t0 <- Sys.time()
  source(script_path, echo = FALSE)
  dt <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)
  cat(sprintf("[%s] DONE  %s (%.1fs)\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), name, dt))
}

this_file <- tryCatch(normalizePath(sys.frame(1)$ofile), error = function(e) NA_character_)
this_dir <- if (is.na(this_file)) getwd() else dirname(this_file)

scripts <- list(
  cfm = file.path(this_dir, "run_cfm.R"),
  edr = file.path(this_dir, "run_edr.R"),
  quintet = file.path(this_dir, "run_quintet.R")
)

for (nm in names(scripts)) {
  if (!file.exists(scripts[[nm]])) stop("Missing script: ", scripts[[nm]])
}

tryCatch({
  run_with_log(scripts$cfm, "CFM")
  run_with_log(scripts$edr, "EDR")
  run_with_log(scripts$quintet, "QUINTET")
  cat(sprintf("\n[%s] ALL DONE\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
}, error = function(e) {
  cat(sprintf("\n[%s] FAILED: %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), conditionMessage(e)))
  if (interactive()) {
    stop(e)
  } else {
    quit(save = "no", status = 1, runLast = FALSE)
  }
})
