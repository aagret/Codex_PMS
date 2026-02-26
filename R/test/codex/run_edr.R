library(DBI)
library(RPostgres)

load_dotenv <- function(path) {
    if (!file.exists(path)) return(invisible(FALSE))
    lines <- readLines(path, warn = FALSE)
    lines <- trimws(lines)
    lines <- lines[nzchar(lines) & !grepl("^#", lines)]
    for (line in lines) {
        parts <- strsplit(line, "=", fixed = TRUE)[[1]]
        if (length(parts) < 2) next
        key <- trimws(parts[[1]])
        val <- trimws(paste(parts[-1], collapse = "="))
        if (nzchar(key)) do.call(Sys.setenv, setNames(list(val), key))
    }
    invisible(TRUE)
}

load_dotenv("/home/Alexandre/PMS/R/test/codex/.env")

source("/home/Alexandre/PMS/R/test/codex/cfg_edr.R")
source("/home/Alexandre/PMS/R/test/codex/types_edr.R")
source("/home/Alexandre/PMS/R/test/codex/load_edr.R")
source("/home/Alexandre/PMS/R/test/codex/db_schema.R")
source("/home/Alexandre/PMS/R/test/codex/db_upsert.R")

base_dir <- Sys.getenv("PMS_BASE_DIR", "/home/Alexandre/PMS")
provider_schema <- tolower(Sys.getenv("PMS_SCHEMA", "edr"))

if (exists("type_overrides") && length(type_overrides) > 0) {
    for (nm in intersect(names(cfg), names(type_overrides))) {
        cfg[[nm]]$db$types <- type_overrides[[nm]]
    }
}

edr_datasets <- build_edr_datasets(cfg, base_dir = base_dir)

pg_dbname <- Sys.getenv("PMS_DBNAME", "PMS_test")
pg_host   <- Sys.getenv("PMS_HOST", "192.168.14.140")
pg_port   <- as.integer(Sys.getenv("PMS_PORT", "5432"))
pg_user   <- Sys.getenv("PMS_USER", "admin")
pg_pass   <- Sys.getenv("PMS_PASSWORD", "")

if (pg_pass == "") {
    stop("PMS_PASSWORD is empty. Set it in your environment.")
}

con <- DBI::dbConnect(
    RPostgres::Postgres(),
    dbname   = pg_dbname,
    host     = pg_host,
    port     = pg_port,
    user     = pg_user,
    password = pg_pass
)

ensure_tables_from_datasets(con, edr_datasets, cfg, provider_schema)
upsert_all(con, edr_datasets, cfg, schema = provider_schema)

DBI::dbDisconnect(con)
