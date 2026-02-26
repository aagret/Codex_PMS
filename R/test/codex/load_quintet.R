library(data.table)

convert_dates <- function(dt, cols, date_formats = c("%d.%m.%Y"), allow_excel_serial = TRUE, excel_origin = "1899-12-30") {
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

extract_date_from_filename_regex <- function(file, regex, fmt) {
    m <- regexec(regex, basename(file))
    parts <- regmatches(basename(file), m)
    if (length(parts) == 0L || length(parts[[1]]) < 2L) return(as.Date(NA))
    as.Date(parts[[1]][2L], format = fmt)
}

load_quintet_file <- function(target_dir, pattern, cols_raw,
                              cols_raw_src = NULL,
                              date_cols = character(),
                              add_filename_date = FALSE,
                              filename_date_col = "as_of_date",
                              filename_date_fmt = "%Y-%m-%d",
                              filename_date_regex = NULL,
                              sep = ";",
                              date_formats = c("%d.%m.%Y")) {

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

    if (!is.null(cols_raw_src)) {
        setnames(DT, cols_raw_src)
        setnames(DT, cols_raw_src, cols_raw)
    } else {
        setnames(DT, cols_expected)
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

    if (identical(date_cols, "AUTO_DATE_COLS")) {
        date_cols <- names(DT)[grepl("date", names(DT), ignore.case = TRUE)]
    }

    if (length(date_cols) > 0L) convert_dates(DT, date_cols, date_formats = date_formats)

    DT
}

build_quintet_datasets <- function(cfg, base_dir) {
    stopifnot(is.list(cfg))

    target_dir <- file.path(base_dir, "rawData", "quintet")

    lapply(cfg, function(x) {
        load_quintet_file(
            target_dir          = target_dir,
            pattern             = x$file$pattern,
            cols_raw            = x$file$cols_raw,
            cols_raw_src        = x$file$cols_raw_src,
            date_cols           = x$file$date_cols,
            add_filename_date   = isTRUE(x$file$add_filename_date),
            filename_date_col   = if (!is.null(x$file$filename_date_col)) x$file$filename_date_col else "as_of_date",
            filename_date_fmt   = if (!is.null(x$file$filename_date_fmt)) x$file$filename_date_fmt else "%Y-%m-%d",
            filename_date_regex = x$file$filename_date_regex,
            sep                 = if (!is.null(x$file$sep)) x$file$sep else ";",
            date_formats        = if (!is.null(x$file$date_formats)) x$file$date_formats else c("%d.%m.%Y")
        )
    })
}
