library(data.table)

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

extract_text_from_filename_regex <- function(file, regex) {
    m <- regexec(regex, basename(file))
    parts <- regmatches(basename(file), m)
    if (length(parts) == 0L || length(parts[[1]]) < 2L) return(NA_character_)
    parts[[1]][2L]
}

convert_dates <- function(dt, cols, date_formats = c(
    "%Y-%m-%d",
    "%d.%m.%Y",
    "%d/%m/%Y",
    "%Y%m%d",
    "%Y-%m-%d %H:%M:%S",
    "%d.%m.%Y %H:%M:%S",
    "%d/%m/%Y %H:%M:%S",
    "%Y-%m-%dT%H:%M:%S"
),
                          allow_excel_serial = TRUE, excel_origin = "1899-12-30") {
    stopifnot(data.table::is.data.table(dt))

    cols <- intersect(cols, names(dt))
    if (length(cols) == 0L) return(invisible(dt))

    if (nrow(dt) == 0L) {
        dt[, (cols) := lapply(.SD, function(.) as.Date(character())), .SDcols = cols]
        return(invisible(dt))
    }

    dt[, (cols) := lapply(.SD, function(x) {
        x <- as.character(x)
        x <- gsub("^\"|\"$", "", x)
        x <- trimws(x)
        x <- sub("^([0-9]{4}-[0-9]{2}-[0-9]{2})[ T].*$", "\\1", x)
        x <- sub("^([0-9]{2}[./][0-9]{2}[./][0-9]{4})[ T].*$", "\\1", x)
        x[x %in% c("", "0", "\t")] <- NA_character_

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

dedupe_clean_names <- function(x) {
    out <- character(length(x))
    counts <- integer(0)
    names(counts) <- character(0)

    for (i in seq_along(x)) {
        base <- x[[i]]
        n <- if (base %in% names(counts)) counts[[base]] + 1L else 1L
        counts[[base]] <- n
        out[[i]] <- if (n == 1L) base else paste0(base, "_", n)
    }

    out
}

clean_colnames <- function(x) {
    x <- iconv(x, from = "", to = "UTF-8", sub = "")
    x <- trimws(x)
    x <- tolower(x)
    x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
    x <- gsub("['`´\"]+", "", x)
    x <- gsub("[()\\[\\]\\{\\}]+", " ", x)
    x <- gsub("[^a-z0-9]+", "_", x)
    x <- gsub("_+", "_", x)
    x <- gsub("^_|_$", "", x)
    x[x == "" | is.na(x)] <- "col"

    # If both "balance" and "date" are present, keep balance semantics and
    # drop the date token to avoid ambiguous balance/date naming collisions.
    x <- vapply(x, function(nm) {
        if (grepl("balance", nm) && grepl("date", nm)) {
            parts <- unlist(strsplit(nm, "_", fixed = TRUE))
            parts <- parts[nzchar(parts) & parts != "date"]
            parts <- c("balance", parts[parts != "balance"])
            nm2 <- paste(parts, collapse = "_")
            nm2 <- gsub("_+", "_", nm2)
            nm2 <- gsub("^_|_$", "", nm2)
            if (!nzchar(nm2)) nm2 <- "balance"
            return(nm2)
        }
        nm
    }, character(1))

    dedupe_clean_names(x)
}

parse_quintet_header <- function(file, sep = ";", merge_header_lines_2_3 = FALSE) {
    n_read <- if (isTRUE(merge_header_lines_2_3)) 3L else 2L
    lines <- readLines(file, n = n_read, warn = FALSE, encoding = "UTF-8")
    if (length(lines) < 2L) return(character(0))

    split_line <- function(x) {
        x <- gsub("^\\s*\"|\"\\s*$", "", x)
        y <- strsplit(x, sep, fixed = TRUE)[[1]]
        y <- gsub("^\\s*\"|\"\\s*$", "", y)
        trimws(y)
    }

    cols2 <- split_line(lines[[2L]])

    if (isTRUE(merge_header_lines_2_3) && length(lines) >= 3L) {
        cols3 <- split_line(lines[[3L]])
        max_n <- max(length(cols2), length(cols3))
        if (length(cols2) < max_n) cols2 <- c(cols2, rep("", max_n - length(cols2)))
        if (length(cols3) < max_n) cols3 <- c(cols3, rep("", max_n - length(cols3)))

        cols <- mapply(function(a, b) {
            if (nzchar(a) && nzchar(b)) {
                if (tolower(a) == tolower(b)) a else paste(a, b, sep = "_")
            } else if (nzchar(a)) {
                a
            } else {
                b
            }
        }, cols2, cols3, USE.NAMES = FALSE)
    } else {
        cols <- cols2
    }

    while (length(cols) > 0L && cols[[length(cols)]] == "") cols <- cols[-length(cols)]

    clean_colnames(cols)
}

parse_quintet_body_lines <- function(lines, sep = ";") {
    if (length(lines) == 0L) return(data.table())

    rows <- lapply(lines, function(line) {
        line <- gsub("^\\s*\"|\"\\s*$", "", line)
        vals <- strsplit(line, sep, fixed = TRUE)[[1]]
        vals <- gsub("^\\s*\"|\"\\s*$", "", vals)
        vals
    })

    max_n <- max(vapply(rows, length, integer(1)))
    rows <- lapply(rows, function(v) {
        if (length(v) < max_n) c(v, rep(NA_character_, max_n - length(v))) else v
    })

    data.table::as.data.table(do.call(rbind, rows))
}

normalize_numeric_cols <- function(dt) {
    char_cols <- names(dt)[vapply(dt, is.character, logical(1))]
    if (length(char_cols) == 0L) return(invisible(dt))

    parse_euro_numeric_strict <- function(x) {
        x <- trimws(as.character(x))
        x <- gsub("^\"|\"$", "", x)
        x <- gsub("\u2212", "-", x, fixed = TRUE)
        x <- gsub("\u00A0|\u202F", "", x, perl = TRUE)
        x <- gsub("\\s+", "", x, perl = TRUE)
        x <- gsub("'", "", x, fixed = TRUE)
        x[x == ""] <- NA_character_
        x <- gsub("[^0-9,\\.\\-\\+]", "", x, perl = TRUE)

        has_both <- grepl(",", x, fixed = TRUE) & grepl("\\.", x)
        if (any(has_both, na.rm = TRUE)) {
            x[has_both] <- gsub("\\.", "", x[has_both], perl = TRUE)
        }

        x <- gsub(",", ".", x, fixed = TRUE)
        suppressWarnings(as.numeric(x))
    }

    parse_euro_numeric <- function(x) {
        x <- trimws(as.character(x))
        x <- gsub("\u00A0|\u202F", "", x, perl = TRUE)
        x <- gsub("\\s+", "", x, perl = TRUE)
        x <- gsub("'", "", x, fixed = TRUE)
        x[x == ""] <- NA_character_

        has_alpha <- grepl("[A-Za-z]", x)
        x[has_alpha] <- NA_character_

        # European style: 1.234.567,89 -> 1234567.89
        y <- gsub("\\.", "", x, perl = TRUE)
        y <- gsub(",", ".", y, fixed = TRUE)

        num <- suppressWarnings(as.numeric(y))
        list(num = num, cleaned = x)
    }

    for (col in char_cols) {
        parsed <- parse_euro_numeric(dt[[col]])
        x <- parsed$cleaned
        num <- parsed$num
        nz <- x[!is.na(x)]
        if (length(nz) == 0L) next

        is_numeric_name <- grepl(
            "(^|_)(quantity|market_price|price|amount|balance|cost|rate|principal|interest|countervalue|evaluation|accrued|nominal|gross|net|income)(_|$)",
            col
        ) &&
            !grepl("(^|_)(id|code|reference|iban|account|currency|date|type|status|name|description)(_|$)", col)
        is_force_numeric_name <- grepl(
            "(^|_)(nominal|price|strike|quantity|amount|value|size|balance|income|accrued|countervalue|cost)(_|$)",
            col
        ) && !grepl("(^|_)date(_|$)", col)

        if (isTRUE(is_force_numeric_name)) {
            num_force <- parse_euro_numeric_strict(dt[[col]])
            if (any(!is.na(num_force))) {
                dt[[col]] <- num_force
                next
            }
        }

        # Convert when most non-empty values are parseable and there is
        # clear numeric formatting evidence.
        parse_rate <- mean(!is.na(num[!is.na(x)]))
        has_numeric_signals <- any(grepl("[0-9]", nz)) &&
            any(grepl("[,\\.]", nz) | grepl("^[+-]?[0-9]+$", nz))

        if (isTRUE(has_numeric_signals) && (isTRUE(is_force_numeric_name) || isTRUE(is_numeric_name) || parse_rate >= 0.8)) {
            dt[[col]] <- num
        }
    }

    invisible(dt)
}

force_named_numeric_cols <- function(dt, name_pattern = "(^|_)(nominal|price|strike|quantity|amount|value|size|balance|income|accrued|countervalue|cost)(_|$)") {
    stopifnot(data.table::is.data.table(dt))
    cols <- names(dt)[grepl(name_pattern, names(dt)) & !grepl("(^|_)date(_|$)", names(dt))]
    if (length(cols) == 0L) return(invisible(dt))

    parse_euro_numeric_strict <- function(x) {
        x <- trimws(as.character(x))
        x <- gsub("^\"|\"$", "", x)
        x <- gsub("\u2212", "-", x, fixed = TRUE)
        x <- gsub("\u00A0|\u202F", "", x, perl = TRUE)
        x <- gsub("\\s+", "", x, perl = TRUE)
        x <- gsub("'", "", x, fixed = TRUE)
        x[x == ""] <- NA_character_
        x <- gsub("[^0-9,\\.\\-\\+]", "", x, perl = TRUE)
        both <- grepl(",", x, fixed = TRUE) & grepl("\\.", x)
        if (any(both, na.rm = TRUE)) x[both] <- gsub("\\.", "", x[both], perl = TRUE)
        x <- gsub(",", ".", x, fixed = TRUE)
        suppressWarnings(as.numeric(x))
    }

    dt[, (cols) := lapply(.SD, parse_euro_numeric_strict), .SDcols = cols]

    invisible(dt)
}

force_named_date_cols <- function(dt, date_pattern = "date") {
    stopifnot(data.table::is.data.table(dt))
    cols <- names(dt)[grepl(date_pattern, names(dt), ignore.case = TRUE)]
    if (length(cols) == 0L) return(invisible(dt))

    parse_date_strict <- function(x) {
        x <- trimws(as.character(x))
        x <- gsub("^\"|\"$", "", x)
        x <- gsub("\u00A0|\u202F", "", x, perl = TRUE)
        x[x %in% c("", "0", "\t")] <- NA_character_

        out <- rep(as.Date(NA), length(x))

        # Keep date token if datetime-like text is present.
        y <- x
        y <- sub("^.*?([0-9]{4}-[0-9]{2}-[0-9]{2}).*$", "\\1", y)
        y <- sub("^.*?([0-9]{2}[./][0-9]{2}[./][0-9]{4}).*$", "\\1", y)

        fmts <- c("%Y-%m-%d", "%d.%m.%Y", "%d/%m/%Y", "%Y%m%d")
        for (fmt in fmts) {
            parsed <- as.Date(y, format = fmt)
            mask <- is.na(out) & !is.na(parsed)
            if (any(mask)) out[mask] <- parsed[mask]
        }

        # Excel serial fallback
        serial_like <- grepl("^[0-9]{5,6}$", x)
        if (any(serial_like, na.rm = TRUE)) {
            s <- suppressWarnings(as.numeric(x))
            d <- as.Date(s, origin = "1899-12-30")
            mask <- is.na(out) & serial_like & !is.na(d)
            if (any(mask)) out[mask] <- d[mask]
        }

        out
    }

    dt[, (cols) := lapply(.SD, parse_date_strict), .SDcols = cols]
    invisible(dt)
}

load_quintet_file <- function(target_dir, pattern,
                              date_cols = character(),
                              merge_header_lines_2_3 = FALSE,
                              add_filename_date = FALSE,
                              filename_date_col = "as_of_date",
                              filename_date_fmt = "%Y-%m-%d",
                              filename_date_regex = NULL,
                              add_filename_ts = FALSE,
                              filename_ts_col = "as_of_file_ts",
                              filename_ts_regex = "JRN_([0-9]{4}-[0-9]{2}-[0-9]{2}_[0-9]{2}_[0-9]{2}_[0-9]{2})_",
                              add_source_row_id = FALSE,
                              source_file_col = "source_file",
                              source_row_col = "source_row_num",
                              sep = ";",
                              date_formats = c("%Y-%m-%d", "%d.%m.%Y", "%d/%m/%Y", "%Y%m%d")) {

    files <- list.files(target_dir, pattern = pattern, full.names = TRUE)

    if (length(files) == 0L) {
        DT <- data.table()
        if (isTRUE(add_filename_date)) DT[, (filename_date_col) := as.Date(character())]
        if (isTRUE(add_filename_ts)) DT[, (filename_ts_col) := character()]
        if (isTRUE(add_source_row_id)) {
            DT[, (source_file_col) := character()]
            DT[, (source_row_col) := integer()]
        }
    } else {
        DT <- rbindlist(lapply(files, function(f) {
            header_names <- parse_quintet_header(
                f,
                sep = sep,
                merge_header_lines_2_3 = isTRUE(merge_header_lines_2_3)
            )
            file_lines <- readLines(f, warn = FALSE, encoding = "UTF-8")
            n_header_lines <- if (isTRUE(merge_header_lines_2_3)) 3L else 2L

            if (length(file_lines) <= n_header_lines) {
                tmp <- data.table()
                if (length(header_names) > 0L) {
                    for (nm in header_names) tmp[, (nm) := character()]
                    tmp <- tmp[0]
                }
            } else {
                tmp <- tryCatch(
                    fread(
                        f,
                        sep = sep,
                        skip = n_header_lines,
                        header = FALSE,
                        fill = TRUE,
                        encoding = "UTF-8"
                    ),
                    error = function(e) {
                        if (!grepl("invalid quotes", conditionMessage(e), ignore.case = TRUE)) {
                            stop(e)
                        }

                        tmp2 <- fread(
                            f,
                            sep = sep,
                            skip = n_header_lines,
                            header = FALSE,
                            fill = TRUE,
                            encoding = "UTF-8",
                            quote = ""
                        )

                        chr_cols <- names(tmp2)[vapply(tmp2, is.character, logical(1))]
                        if (length(chr_cols) > 0L) {
                            tmp2[, (chr_cols) := lapply(.SD, function(z) {
                                z <- trimws(z)
                                z <- gsub("^\"|\"$", "", z)
                                z
                            }), .SDcols = chr_cols]
                        }

                        tmp2
                    }
                )
            }

            # Some malformed files are read as a single column despite sep=';'.
            # In that case force a manual split of body lines.
            if (ncol(tmp) == 1L && length(header_names) > 1L) {
                body_lines <- file_lines[-seq_len(n_header_lines)]
                if (length(body_lines) > 0L) {
                    tmp <- parse_quintet_body_lines(body_lines, sep = sep)
                }
            }

            if (length(header_names) > 0L) {
                expected_n <- length(header_names)
                actual_n <- ncol(tmp)

                if (actual_n > expected_n) {
                    tmp <- tmp[, seq_len(expected_n), with = FALSE]
                } else if (actual_n < expected_n) {
                    for (i in (actual_n + 1L):expected_n) {
                        tmp[, (paste0("v", i)) := NA]
                    }
                }

                setnames(tmp, header_names)
            }

            if (isTRUE(add_filename_date)) {
                if (!is.null(filename_date_regex)) {
                    tmp[, (filename_date_col) := extract_date_from_filename_regex(f, filename_date_regex, filename_date_fmt)]
                } else {
                    tmp[, (filename_date_col) := as.Date(NA)]
                }
            }
            if (isTRUE(add_filename_ts)) {
                if (!is.null(filename_ts_regex)) {
                    tmp[, (filename_ts_col) := extract_text_from_filename_regex(f, filename_ts_regex)]
                } else {
                    tmp[, (filename_ts_col) := NA_character_]
                }
            }
            if (isTRUE(add_source_row_id)) {
                tmp[, (source_file_col) := basename(f)]
                tmp[, (source_row_col) := .I]
            }

            tmp
        }), fill = TRUE, use.names = TRUE)
    }

    char_cols <- names(DT)[vapply(DT, is.character, logical(1))]
    if (length(char_cols) > 0L) {
        DT[, (char_cols) := lapply(.SD, function(x) {
            x <- iconv(x, from = "", to = "UTF-8", sub = "")
            x <- gsub("^\"|\"$", "", x)
            x <- trimws(x)
            x[x %in% c("", "\t")] <- NA_character_
            x
        }), .SDcols = char_cols]
    }

    normalize_numeric_cols(DT)
    force_named_numeric_cols(DT)
    drop_trailing_na_cols(DT)

    if (identical(date_cols, "AUTO_DATE_COLS")) {
        date_cols <- names(DT)[grepl("date", names(DT), ignore.case = TRUE)]
    }
    if (length(date_cols) > 0L) convert_dates(DT, date_cols, date_formats = date_formats)
    force_named_date_cols(DT)

    DT
}

build_quintet_datasets <- function(cfg, base_dir) {
    stopifnot(is.list(cfg))

    target_dir <- file.path(base_dir, "rawData", "quintet")

    lapply(cfg, function(x) {
        load_quintet_file(
            target_dir          = target_dir,
            pattern             = x$file$pattern,
            date_cols           = x$file$date_cols,
            merge_header_lines_2_3 = isTRUE(x$file$merge_header_lines_2_3),
            add_filename_date   = isTRUE(x$file$add_filename_date),
            filename_date_col   = if (!is.null(x$file$filename_date_col)) x$file$filename_date_col else "as_of_date",
            filename_date_fmt   = if (!is.null(x$file$filename_date_fmt)) x$file$filename_date_fmt else "%Y-%m-%d",
            filename_date_regex = x$file$filename_date_regex,
            add_filename_ts     = isTRUE(x$file$add_filename_ts),
            filename_ts_col     = if (!is.null(x$file$filename_ts_col)) x$file$filename_ts_col else "as_of_file_ts",
            filename_ts_regex   = if (!is.null(x$file$filename_ts_regex)) x$file$filename_ts_regex else "JRN_([0-9]{4}-[0-9]{2}-[0-9]{2}_[0-9]{2}_[0-9]{2}_[0-9]{2})_",
            add_source_row_id   = isTRUE(x$file$add_source_row_id),
            source_file_col     = if (!is.null(x$file$source_file_col)) x$file$source_file_col else "source_file",
            source_row_col      = if (!is.null(x$file$source_row_col)) x$file$source_row_col else "source_row_num",
            sep                 = if (!is.null(x$file$sep)) x$file$sep else ";",
            date_formats        = if (!is.null(x$file$date_formats)) x$file$date_formats else c("%Y-%m-%d", "%d.%m.%Y", "%d/%m/%Y", "%Y%m%d")
        )
    })
}
