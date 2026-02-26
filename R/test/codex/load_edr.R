library(data.table)

# EDR loader helpers

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

extract_date_from_filename_regex <- function(file, regex, fmt) {
    m <- regexec(regex, basename(file))
    parts <- regmatches(basename(file), m)
    if (length(parts) == 0L || length(parts[[1]]) < 2L) return(as.Date(NA))
    as.Date(parts[[1]][2L], format = fmt)
}

convert_dates <- function(dt, cols, date_formats = c("%d%m%Y", "%d.%m.%Y", "%Y%m%d", "%Y-%m-%d"),
                          allow_excel_serial = TRUE, excel_origin = "1899-12-30") {
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

        out <- rep(as.Date(NA), length(x))
        for (fmt in date_formats) {
            parsed <- as.Date(x, format = fmt)
            mask <- is.na(out) & !is.na(parsed)
            if (any(mask)) out[mask] <- parsed[mask]
        }
        if (isTRUE(allow_excel_serial)) {
            num_like <- grepl("^[0-9]{5,6}$", x)
            if (any(num_like)) {
                serial <- suppressWarnings(as.numeric(x))
                parsed_serial <- as.Date(serial, origin = excel_origin)
                mask <- is.na(out) & num_like & !is.na(parsed_serial)
                if (any(mask)) out[mask] <- parsed_serial[mask]
            }
        }
        out
    }), .SDcols = cols]

    invisible(dt)
}

load_edr_file <- function(target_dir, pattern, cols_raw,
                          date_cols = character(),
                          add_filename_date = FALSE,
                          filename_date_col = "as_of_date",
                          filename_date_fmt = "%Y%m%d",
                          filename_date_regex = NULL,
                          sep = ";",
                          date_formats = NULL) {

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
                if (!is.null(filename_date_regex)) {
                    tmp[, (filename_date_col) := extract_date_from_filename_regex(f, filename_date_regex, filename_date_fmt)]
                } else {
                    tmp[, (filename_date_col) := as.Date(NA)]
                }
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

    drop_trailing_na_cols(DT)

    if (length(date_cols) > 0L) {
        if (is.null(date_formats)) {
            convert_dates(DT, date_cols)
        } else {
            convert_dates(DT, date_cols, date_formats = date_formats)
        }
    }

    DT
}

build_edr_datasets <- function(cfg, base_dir) {
    stopifnot(is.list(cfg))

    target_dir <- file.path(base_dir, "rawData", "edr")

    lapply(cfg, function(x) {
        load_edr_file(
            target_dir         = target_dir,
            pattern            = x$file$pattern,
            cols_raw           = x$file$cols_raw,
            date_cols          = x$file$date_cols,
            add_filename_date  = isTRUE(x$file$add_filename_date),
            filename_date_col  = if (!is.null(x$file$filename_date_col)) x$file$filename_date_col else "as_of_date",
            filename_date_fmt  = if (!is.null(x$file$filename_date_fmt)) x$file$filename_date_fmt else "%Y%m%d",
            filename_date_regex = x$file$filename_date_regex,
            sep                = if (!is.null(x$file$sep)) x$file$sep else ";",
            date_formats       = x$file$date_formats
        )
    })
}
