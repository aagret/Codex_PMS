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

this_file <- tryCatch(normalizePath(sys.frame(1)$ofile), error = function(e) NA_character_)
this_dir <- if (is.na(this_file)) getwd() else dirname(this_file)
root_dir <- normalizePath(file.path(this_dir, ".."), mustWork = FALSE)

load_dotenv(file.path(root_dir, ".env"))

source(file.path(this_dir, "cfg_edr.R"))
source(file.path(this_dir, "types_edr.R"))
source(file.path(this_dir, "load_edr.R"))
source(file.path(this_dir, "db_schema.R"))
source(file.path(this_dir, "db_upsert.R"))

base_dir <- Sys.getenv("PMS_BASE_DIR", "/home/Alexandre/PMS")
provider_schema <- tolower(Sys.getenv("PMS_SCHEMA", "edr"))

if (exists("type_overrides") && length(type_overrides) > 0) {
    for (nm in intersect(names(cfg), names(type_overrides))) {
        cfg[[nm]]$db$types <- type_overrides[[nm]]
    }
}

edr_datasets <- build_edr_datasets(cfg, base_dir = base_dir)

con <- DBI::dbConnect(
    RPostgres::Postgres(),
    dbname   = Sys.getenv("PMS_DBNAME", "PMS_test"),
    host     = Sys.getenv("PMS_HOST", "192.168.14.140"),
    port     = as.integer(Sys.getenv("PMS_PORT", "5432")),
    user     = Sys.getenv("PMS_USER", "admin"),
    password = Sys.getenv("PMS_PASSWORD", "")
)

ensure_tables_from_datasets(con, edr_datasets, cfg, provider_schema)
upsert_all(con, edr_datasets, cfg, schema = provider_schema)

DBI::dbDisconnect(con)
