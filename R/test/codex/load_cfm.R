library(data.table)

# Data loading helpers for CFM files

drop_trailing_na_cols <- function(dt) {
    stopifnot(data.table::is.data.table(dt))
    if (nrow(dt) == 0L || ncol(dt) == 0L) return(invisible(dt))

    is_all_na <- colSums(!is.na(dt)) == 0
    r <- rle(is_all_na)

    if (length(r$values) > 0L && tail(r$values, 1L)) {
        n_drop <- tail(r$lengths, 1L)
        dt[, tail(names(dt), n_drop) := NULL]
    }

    invisible(dt)
}

extract_date_from_filename <- function(file) {
    d <- substr(basename(file), 1, 8)
    as.Date(d, format = "%Y%m%d")
}

convert_dates <- function(dt, cols) {
    stopifnot(data.table::is.data.table(dt))

    cols <- intersect(cols, names(dt))
    if (length(cols) == 0L) return(invisible(dt))

    if (nrow(dt) == 0L) {
        dt[, (cols) := lapply(.SD, function(.) as.Date(character())), .SDcols = cols]
        return(invisible(dt))
    }

    dt[, (cols) := lapply(.SD, function(x) {
        x <- trimws(as.character(x))
        x[x %in% c("", "0")] <- NA_character_

        valid <- grepl("^[0-9]{7,8}$", x)
        idx7 <- valid & nchar(x) == 7L
        if (any(idx7)) x[idx7] <- sprintf("%08d", as.integer(x[idx7]))

        as.Date(x, format = "%d%m%Y")
    }), .SDcols = cols]

    invisible(dt)
}

load_cfm_file <- function(target_dir, pattern, cols_raw,
                          date_cols = character(),
                          add_filename_date = FALSE,
                          filename_date_col = "as_of_date",
                          sep = ";") {

    files <- list.files(target_dir, pattern = pattern, full.names = TRUE)

    cols_expected <- cols_raw
    if (isTRUE(add_filename_date) && !(filename_date_col %in% cols_expected)) {
        cols_expected <- c(cols_expected, filename_date_col)
    }

    if (length(files) == 0L) {
        DT <- setDT(setNames(vector("list", length(cols_expected)), cols_expected))
    } else {
        DT <- rbindlist(lapply(files, function(f) {
            tmp <- fread(f, sep = sep, fill = TRUE)
            if (isTRUE(add_filename_date)) {
                tmp[, (filename_date_col) := extract_date_from_filename(f)]
            }

            tmp
        }), fill = TRUE)

        if (nrow(DT) == 0L) {
            DT <- setDT(setNames(vector("list", length(cols_expected)), cols_expected))
        }
    }

    expected_n <- length(cols_expected)
    actual_n   <- ncol(DT)

    if (actual_n > expected_n) {
        DT <- DT[, seq_len(expected_n), with = FALSE]
    } else if (actual_n < expected_n) {
        for (i in (actual_n + 1L):expected_n) DT[, (i) := NA]
    }

    setnames(DT, cols_expected)

    char_cols <- names(DT)[vapply(DT, is.character, logical(1))]
    if (length(char_cols) > 0L) {
        DT[, (char_cols) := lapply(.SD, iconv, from = "latin1", to = "UTF-8", sub = ""), .SDcols = char_cols]
    }

    normalize_numeric_cols <- function(dt) {
        char_cols <- names(dt)[vapply(dt, is.character, logical(1))]
        if (length(char_cols) == 0L) return(invisible(dt))

        re_comma <- "^[-+]?[0-9]{1,3}(\\.[0-9]{3})*,[0-9]+$|^[-+]?[0-9]+,[0-9]+$"
        re_plain <- "^[-+]?[0-9]+$"
        re_dot <- "^[-+]?[0-9]+\\.[0-9]+$"

        for (col in char_cols) {
            x <- trimws(as.character(dt[[col]]))
            x[x == ""] <- NA_character_
            nz <- x[!is.na(x)]
            if (length(nz) == 0L) next
            if (!any(grepl(",", nz, fixed = TRUE))) next
            if (any(grepl("[A-Za-z]", nz))) next

            is_comma <- grepl(re_comma, nz)
            is_plain <- grepl(re_plain, nz)
            is_dot <- grepl(re_dot, nz)

            if (any(is_dot)) next
            if (!all(is_comma | is_plain)) next

            y <- gsub("\\.", "", x)
            y <- gsub(",", ".", y, fixed = TRUE)
            dt[[col]] <- suppressWarnings(as.numeric(y))
        }

        invisible(dt)
    }

    normalize_numeric_cols(DT)

    drop_trailing_na_cols(DT)

    if (length(date_cols) > 0L) convert_dates(DT, date_cols)

    DT
}

build_cfm_datasets <- function(cfg, base_dir) {
    stopifnot(is.list(cfg))

    target_dir <- file.path(base_dir, "rawData", "cfm")

    lapply(cfg, function(x) {
        load_cfm_file(
            target_dir         = target_dir,
            pattern            = x$file$pattern,
            cols_raw           = x$file$cols_raw,
            date_cols          = x$file$date_cols,
            add_filename_date  = isTRUE(x$file$add_filename_date),
            filename_date_col  = if (!is.null(x$file$filename_date_col)) x$file$filename_date_col else "as_of_date"
        )
    })
}
