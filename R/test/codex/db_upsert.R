library(DBI)
library(RPostgres)
library(data.table)

upsert_table_pg <- function(conn, DT, table, key_cols, schema = "public", temp_prefix = "stg_") {
    stopifnot(data.table::is.data.table(DT))
    if (nrow(DT) == 0L) return(invisible(TRUE))

    if ("id" %in% names(DT)) {
        DT[, id := NULL]
    }

    if (!all(key_cols %in% names(DT))) {
        stop("Some key_cols are missing from DT: ", paste(setdiff(key_cols, names(DT)), collapse = ", "))
    }

    table_id <- DBI::Id(schema = schema, table = table)
    stg_name <- paste0(temp_prefix, table, "_", as.integer(runif(1, 1, 1e9)))

    cols <- names(DT)
    non_keys <- setdiff(cols, key_cols)

    q_table <- DBI::dbQuoteIdentifier(conn, table_id)
    q_stg   <- DBI::dbQuoteIdentifier(conn, stg_name)

    q_cols     <- DBI::dbQuoteIdentifier(conn, cols)
    q_key_cols <- DBI::dbQuoteIdentifier(conn, key_cols)

    if (length(non_keys) > 0L) {
        q_non_keys <- DBI::dbQuoteIdentifier(conn, non_keys)
        set_clause <- paste0(q_non_keys, " = EXCLUDED.", q_non_keys, collapse = ", ")
    } else {
        set_clause <- NULL
    }

    insert_sql <- paste0(
        "INSERT INTO ", q_table, " (", paste(q_cols, collapse = ", "), ")\n",
        "SELECT ", paste(q_cols, collapse = ", "), " FROM ", q_stg, "\n",
        "ON CONFLICT (", paste(q_key_cols, collapse = ", "), ")\n",
        if (!is.null(set_clause)) paste0("DO UPDATE SET ", set_clause) else "DO NOTHING",
        ";"
    )

    DBI::dbWithTransaction(conn, {
        DBI::dbExecute(conn, paste0(
            "CREATE TEMP TABLE ", q_stg, " ON COMMIT DROP AS ",
            "SELECT ", paste(q_cols, collapse = ", "),
            " FROM ", q_table, " WHERE FALSE;"
        ))

        DBI::dbWriteTable(conn, stg_name, as.data.frame(DT), append = TRUE, row.names = FALSE)

        DBI::dbExecute(conn, insert_sql)

        DBI::dbExecute(conn, paste0("DROP TABLE IF EXISTS ", q_stg, ";"))
    })

    invisible(TRUE)
}

upsert_all <- function(conn, datasets, cfg, schema) {

    for (nm in names(datasets)) {
        DT <- datasets[[nm]]
        if (is.null(DT) || nrow(DT) == 0L) {
            cat("Skipping empty:", nm, "\n")
            next
        }
        cat("Upserting:", nm, "->", cfg[[nm]]$db$table, "\n")
        upsert_table_pg(
            conn     = conn,
            DT       = DT,
            schema   = schema,
            table    = cfg[[nm]]$db$table,
            key_cols = cfg[[nm]]$db$keys
        )
    }
    invisible(TRUE)
}
