library(DBI)
library(RPostgres)
library(data.table)

pg_type_for_vector <- function(x) {
    if (inherits(x, "Date")) return("date")
    if (inherits(x, "POSIXct") || inherits(x, "POSIXt")) return("timestamptz")
    if (is.integer(x)) return("bigint")
    if (is.numeric(x)) return("double precision")
    if (is.logical(x)) return("boolean")
    "text"
}

quote_ident <- function(con, x) as.character(DBI::dbQuoteIdentifier(con, x))

schema_exists <- function(con, schema) {
    sql <- "SELECT 1 FROM information_schema.schemata WHERE schema_name = $1 LIMIT 1;"
    nrow(DBI::dbGetQuery(con, sql, params = list(schema))) > 0
}

table_exists <- function(con, schema, table) {
    sql <- "
    SELECT 1
    FROM information_schema.tables
    WHERE table_schema = $1 AND table_name = $2
    LIMIT 1;"
    nrow(DBI::dbGetQuery(con, sql, params = list(schema, table))) > 0
}

get_existing_columns <- function(con, schema, table) {
    sql <- "
    SELECT column_name
    FROM information_schema.columns
    WHERE table_schema = $1 AND table_name = $2;"
    DBI::dbGetQuery(con, sql, params = list(schema, table))$column_name
}

constraint_exists <- function(con, schema, table, constraint_name) {
    sql <- "
    SELECT 1
    FROM information_schema.table_constraints
    WHERE table_schema = $1 AND table_name = $2 AND constraint_name = $3
    LIMIT 1;"
    nrow(DBI::dbGetQuery(con, sql, params = list(schema, table, constraint_name))) > 0
}

ensure_table_from_dt <- function(con, DT, schema, table, keys,
                                 pk_col = "id",
                                 unique_name = NULL,
                                 type_overrides = NULL) {

    stopifnot(data.table::is.data.table(DT))
    if (is.null(unique_name)) unique_name <- paste0("uq_", table)

    if (!schema_exists(con, schema)) {
        DBI::dbExecute(con, paste0("CREATE SCHEMA ", quote_ident(con, schema), ";"))
    }

    dt_cols <- names(DT)
    dt_types <- if (length(dt_cols) > 0) vapply(DT, pg_type_for_vector, character(1)) else character(0)
    if (!is.null(type_overrides) && length(type_overrides) > 0) {
        common <- intersect(names(type_overrides), dt_cols)
        if (length(common) > 0) dt_types[common] <- type_overrides[common]
    }

    q_full_table <- paste0(quote_ident(con, schema), ".", quote_ident(con, table))

    if (!table_exists(con, schema, table)) {
        col_defs <- character(0)

        col_defs <- c(col_defs,
                      paste0(quote_ident(con, pk_col),
                             " BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY"))

        if (length(dt_cols) > 0) {
            col_defs <- c(col_defs, paste0(quote_ident(con, dt_cols), " ", dt_types))
        }

        create_sql <- paste0(
            "CREATE TABLE ", q_full_table, " (\n  ",
            paste(col_defs, collapse = ",\n  "),
            "\n);"
        )
        DBI::dbExecute(con, create_sql)

    } else {
        existing <- get_existing_columns(con, schema, table)

        if (!(pk_col %in% existing)) {
            DBI::dbExecute(con, paste0(
                "ALTER TABLE ", q_full_table,
                " ADD COLUMN ", quote_ident(con, pk_col),
                " BIGINT GENERATED ALWAYS AS IDENTITY;"
            ))
            DBI::dbExecute(con, paste0(
                "ALTER TABLE ", q_full_table,
                " ADD PRIMARY KEY (", quote_ident(con, pk_col), ");"
            ))
            existing <- c(existing, pk_col)
        }

        missing_cols <- setdiff(dt_cols, existing)
        if (length(missing_cols) > 0) {
            for (col in missing_cols) {
                DBI::dbExecute(con, paste0(
                    "ALTER TABLE ", q_full_table,
                    " ADD COLUMN ", quote_ident(con, col), " ", dt_types[[col]], ";"
                ))
            }
        }
    }

    if (length(keys) == 0L) {
        warning("No UNIQUE keys provided for ", schema, ".", table, " (skipping constraint).")
        return(invisible(TRUE))
    }

    existing_now <- get_existing_columns(con, schema, table)
    missing_keys <- setdiff(keys, existing_now)
    if (length(missing_keys) > 0) {
        warning("Skipping UNIQUE constraint on ", schema, ".", table,
                " because key column(s) missing: ", paste(missing_keys, collapse = ", "))
        return(invisible(TRUE))
    }

    if (!constraint_exists(con, schema, table, unique_name)) {
        uq_sql <- paste0(
            "ALTER TABLE ", q_full_table,
            " ADD CONSTRAINT ", quote_ident(con, unique_name),
            " UNIQUE (", paste(quote_ident(con, keys), collapse = ", "), ");"
        )
        DBI::dbExecute(con, uq_sql)
    }

    invisible(TRUE)
}

ensure_tables_from_datasets <- function(con, datasets, cfg, provider_schema,
                                        pk_col = "id",
                                        translit_keys = TRUE) {

    stopifnot(is.list(datasets), is.list(cfg))

    for (nm in names(datasets)) {
        DT <- datasets[[nm]]
        if (is.null(DT)) next

        table <- tolower(cfg[[nm]]$db$table)
        keys  <- cfg[[nm]]$db$keys
        type_overrides <- cfg[[nm]]$db$types

        if (translit_keys) {
            keys <- iconv(keys, from = "UTF-8", to = "ASCII//TRANSLIT")
        }

        ensure_table_from_dt(
            con    = con,
            DT     = DT,
            schema = provider_schema,
            table  = table,
            keys   = keys,
            pk_col = pk_col,
            unique_name = paste0("uq_", table),
            type_overrides = type_overrides
        )
    }

    invisible(TRUE)
}
