library(data.table)

merge_lists <- function(base, override) {
    if (is.null(base)) return(override)
    if (is.null(override)) return(base)
    out <- base
    for (nm in names(override)) {
        if (is.list(out[[nm]]) && is.list(override[[nm]])) {
            out[[nm]] <- merge_lists(out[[nm]], override[[nm]])
        } else {
            out[[nm]] <- override[[nm]]
        }
    }
    out
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

extract_date_from_filename_prefix <- function(file, n = 8L, fmt = "%Y%m%d") {
    x <- substr(basename(file), 1L, n)
    as.Date(x, format = fmt)
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
    x <- gsub("['`\"´]+", "", x)
    x <- gsub("[()\\[\\]\\{\\}]+", " ", x)
    x <- gsub("[^a-z0-9]+", "_", x)
    x <- gsub("_+", "_", x)
    x <- gsub("^_|_$", "", x)
    x[x == "" | is.na(x)] <- "col"

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

parse_header_line <- function(line, sep = ";") {
    line <- gsub("^\\s*\"|\"\\s*$", "", line)
    out <- strsplit(line, sep, fixed = TRUE)[[1]]
    out <- gsub("^\\s*\"|\"\\s*$", "", out)
    trimws(out)
}

parse_header_names <- function(file, header_mode = "none", sep = ";") {
    if (identical(header_mode, "none")) return(character(0))

    n_read <- switch(header_mode,
                     line2 = 2L,
                     merge_2_3 = 3L,
                     stop("Unsupported header_mode: ", header_mode))

    lines <- readLines(file, n = n_read, warn = FALSE, encoding = "UTF-8")
    if (length(lines) < 2L) return(character(0))

    c2 <- parse_header_line(lines[[2L]], sep = sep)

    if (identical(header_mode, "merge_2_3") && length(lines) >= 3L) {
        c3 <- parse_header_line(lines[[3L]], sep = sep)
        max_n <- max(length(c2), length(c3))
        if (length(c2) < max_n) c2 <- c(c2, rep("", max_n - length(c2)))
        if (length(c3) < max_n) c3 <- c(c3, rep("", max_n - length(c3)))

        cols <- mapply(function(a, b) {
            if (nzchar(a) && nzchar(b)) {
                if (tolower(a) == tolower(b)) a else paste(a, b, sep = "_")
            } else if (nzchar(a)) {
                a
            } else {
                b
            }
        }, c2, c3, USE.NAMES = FALSE)
    } else {
        cols <- c2
    }

    while (length(cols) > 0L && cols[[length(cols)]] == "") cols <- cols[-length(cols)]
    clean_colnames(cols)
}

parse_body_lines <- function(lines, sep = ";") {
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

convert_dates <- function(dt, cols,
                          date_formats = c("%Y-%m-%d", "%d.%m.%Y", "%d/%m/%Y", "%Y%m%d",
                                           "%Y-%m-%d %H:%M:%S", "%d.%m.%Y %H:%M:%S",
                                           "%d/%m/%Y %H:%M:%S", "%Y-%m-%dT%H:%M:%S"),
                          allow_excel_serial = TRUE,
                          excel_origin = "1899-12-30",
                          pad_7_digit_dates = FALSE) {
    cols <- intersect(cols, names(dt))
    if (length(cols) == 0L) return(invisible(dt))

    dt[, (cols) := lapply(.SD, function(x) {
        x <- as.character(x)
        x <- gsub("^\"|\"$", "", x)
        x <- trimws(x)
        x <- sub("^([0-9]{4}-[0-9]{2}-[0-9]{2})[ T].*$", "\\1", x)
        x <- sub("^([0-9]{2}[./][0-9]{2}[./][0-9]{4})[ T].*$", "\\1", x)
        x[x %in% c("", "0", "\\t")] <- NA_character_
        if (isTRUE(pad_7_digit_dates)) {
            idx7 <- grepl("^[0-9]{7}$", x)
            if (any(idx7, na.rm = TRUE)) x[idx7] <- sprintf("%08d", as.integer(x[idx7]))
        }

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

force_named_date_cols <- function(dt, date_pattern = "date") {
    cols <- names(dt)[grepl(date_pattern, names(dt), ignore.case = TRUE)]
    if (length(cols) == 0L) return(invisible(dt))
    convert_dates(dt, cols)
}

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

force_named_numeric_cols <- function(dt,
                                    name_pattern = "(^|_)(nominal|price|strike|quantity|amount|value|size|balance|income|accrued|countervalue|cost)(_|$)",
                                    exclude_pattern = "(^|_)date(_|$)") {
    cols <- names(dt)[grepl(name_pattern, names(dt)) & !grepl(exclude_pattern, names(dt))]
    if (length(cols) == 0L) return(invisible(dt))
    dt[, (cols) := lapply(.SD, parse_euro_numeric_strict), .SDcols = cols]
    invisible(dt)
}

normalize_numeric_cols <- function(dt,
                                   detect_pattern = "(^|_)(quantity|market_price|price|amount|balance|cost|rate|principal|interest|countervalue|evaluation|accrued|nominal|gross|net|income)(_|$)",
                                   exclude_pattern = "(^|_)(id|code|reference|iban|account|currency|date|type|status|name|description)(_|$)") {
    char_cols <- names(dt)[vapply(dt, is.character, logical(1))]
    if (length(char_cols) == 0L) return(invisible(dt))

    for (col in char_cols) {
        x <- trimws(as.character(dt[[col]]))
        x[x == ""] <- NA_character_
        nz <- x[!is.na(x)]
        if (length(nz) == 0L) next

        num <- parse_euro_numeric_strict(x)
        parse_rate <- mean(!is.na(num[!is.na(x)]))
        is_numeric_name <- grepl(detect_pattern, col) && !grepl(exclude_pattern, col)
        has_numeric_signal <- any(grepl("[0-9]", nz)) && any(grepl("[,\\.]", nz) | grepl("^[+-]?[0-9]+$", nz))

        if (isTRUE(has_numeric_signal) && (isTRUE(is_numeric_name) || parse_rate >= 0.8)) {
            dt[[col]] <- num
        }
    }

    invisible(dt)
}

drop_trailing_na_cols <- function(dt) {
    if (nrow(dt) == 0L || ncol(dt) == 0L) return(invisible(dt))
    is_all_na <- colSums(!is.na(dt)) == 0
    r <- rle(is_all_na)
    if (length(r$values) > 0L && tail(r$values, 1L)) {
        n_drop <- tail(r$lengths, 1L)
        dt[, tail(names(dt), n_drop) := NULL]
    }
    invisible(dt)
}

load_provider_file <- function(target_dir, file_cfg) {
    files <- list.files(target_dir, pattern = file_cfg$pattern, full.names = TRUE)
    header_mode <- if (!is.null(file_cfg$header_mode)) file_cfg$header_mode else "none"
    sep <- if (!is.null(file_cfg$sep)) file_cfg$sep else ";"

    base_skip_n <- switch(header_mode,
                     none = 0L,
                     line2 = 2L,
                     merge_2_3 = 3L,
                     stop("Unsupported header_mode: ", header_mode))
    extra_skip_n <- if (!is.null(file_cfg$skip_lines)) as.integer(file_cfg$skip_lines) else 0L
    if (is.na(extra_skip_n) || extra_skip_n < 0L) extra_skip_n <- 0L
    skip_n <- base_skip_n + extra_skip_n

    if (length(files) == 0L) {
        cols_expected <- if (!is.null(file_cfg$cols_raw)) file_cfg$cols_raw else character(0)
        DT <- data.table()
        if (length(cols_expected) > 0L) for (nm in cols_expected) DT[, (nm) := character()]
        if (isTRUE(file_cfg$add_filename_date)) DT[, (ifelse(is.null(file_cfg$filename_date_col), "as_of_date", file_cfg$filename_date_col)) := as.Date(character())]
        if (isTRUE(file_cfg$add_filename_ts)) DT[, (ifelse(is.null(file_cfg$filename_ts_col), "as_of_file_ts", file_cfg$filename_ts_col)) := character()]
        if (isTRUE(file_cfg$add_source_row_id)) {
            DT[, (ifelse(is.null(file_cfg$source_file_col), "source_file", file_cfg$source_file_col)) := character()]
            DT[, (ifelse(is.null(file_cfg$source_row_col), "source_row_num", file_cfg$source_row_col)) := integer()]
        }
    } else {
        DT <- rbindlist(lapply(files, function(f) {
            file_lines <- readLines(f, warn = FALSE, encoding = "UTF-8")

            header_names <- switch(header_mode,
                                   none = if (!is.null(file_cfg$cols_raw)) file_cfg$cols_raw else character(0),
                                   line2 = parse_header_names(f, "line2", sep = sep),
                                   merge_2_3 = parse_header_names(f, "merge_2_3", sep = sep))

            if (length(file_lines) <= skip_n) {
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
                        skip = skip_n,
                        header = FALSE,
                        fill = TRUE,
                        encoding = if (!is.null(file_cfg$fread_encoding)) file_cfg$fread_encoding else "UTF-8",
                        quote = if (!is.null(file_cfg$quote)) file_cfg$quote else "\""
                    ),
                    error = function(e) {
                        if (!grepl("invalid quotes", conditionMessage(e), ignore.case = TRUE)) stop(e)
                        fread(
                            f,
                            sep = sep,
                            skip = skip_n,
                            header = FALSE,
                            fill = TRUE,
                            encoding = if (!is.null(file_cfg$fread_encoding)) file_cfg$fread_encoding else "UTF-8",
                            quote = ""
                        )
                    }
                )
            }

            if (ncol(tmp) == 1L && length(header_names) > 1L) {
                body_lines <- file_lines[-seq_len(skip_n)]
                if (length(body_lines) > 0L) tmp <- parse_body_lines(body_lines, sep = sep)
            }

            if (length(header_names) > 0L) {
                expected_n <- length(header_names)
                actual_n <- ncol(tmp)
                if (actual_n > expected_n) {
                    tmp <- tmp[, seq_len(expected_n), with = FALSE]
                } else if (actual_n < expected_n) {
                    for (i in (actual_n + 1L):expected_n) tmp[, (paste0("v", i)) := NA]
                }
                setnames(tmp, header_names)
            }

            if (isTRUE(file_cfg$add_filename_date)) {
                date_col <- if (!is.null(file_cfg$filename_date_col)) file_cfg$filename_date_col else "as_of_date"
                if (!is.null(file_cfg$filename_date_regex)) {
                    fmt <- if (!is.null(file_cfg$filename_date_fmt)) file_cfg$filename_date_fmt else "%Y-%m-%d"
                    tmp[, (date_col) := extract_date_from_filename_regex(f, file_cfg$filename_date_regex, fmt)]
                } else if (!is.null(file_cfg$filename_date_prefix_n)) {
                    fmt <- if (!is.null(file_cfg$filename_date_fmt)) file_cfg$filename_date_fmt else "%Y%m%d"
                    tmp[, (date_col) := extract_date_from_filename_prefix(f, n = file_cfg$filename_date_prefix_n, fmt = fmt)]
                } else {
                    tmp[, (date_col) := as.Date(NA)]
                }
            }

            if (isTRUE(file_cfg$add_filename_ts)) {
                ts_col <- if (!is.null(file_cfg$filename_ts_col)) file_cfg$filename_ts_col else "as_of_file_ts"
                if (!is.null(file_cfg$filename_ts_regex)) {
                    tmp[, (ts_col) := extract_text_from_filename_regex(f, file_cfg$filename_ts_regex)]
                } else {
                    tmp[, (ts_col) := NA_character_]
                }
            }

            if (isTRUE(file_cfg$add_source_row_id)) {
                sf_col <- if (!is.null(file_cfg$source_file_col)) file_cfg$source_file_col else "source_file"
                sr_col <- if (!is.null(file_cfg$source_row_col)) file_cfg$source_row_col else "source_row_num"
                tmp[, (sf_col) := basename(f)]
                tmp[, (sr_col) := .I]
            }

            tmp
        }), fill = TRUE, use.names = TRUE)
    }

    char_cols <- names(DT)[vapply(DT, is.character, logical(1))]
    if (length(char_cols) > 0L) {
        from_enc <- if (!is.null(file_cfg$file_encoding)) file_cfg$file_encoding else ""
        DT[, (char_cols) := lapply(.SD, function(x) {
            x <- iconv(x, from = from_enc, to = "UTF-8", sub = "")
            x <- gsub("^\"|\"$", "", x)
            x <- trimws(x)
            x[x %in% c("", "\\t")] <- NA_character_
            x
        }), .SDcols = char_cols]
    }

    if (!isFALSE(file_cfg$apply_auto_numeric)) {
        normalize_numeric_cols(DT)
    }
    if (!isFALSE(file_cfg$apply_force_numeric)) {
        force_named_numeric_cols(DT)
    }
    drop_trailing_na_cols(DT)

    date_cols <- file_cfg$date_cols
    if (identical(date_cols, "AUTO_DATE_COLS")) {
        date_cols <- names(DT)[grepl("date", names(DT), ignore.case = TRUE)]
    }
    if (length(date_cols) > 0L) {
        fmts <- if (!is.null(file_cfg$date_formats)) file_cfg$date_formats else NULL
        if (is.null(fmts)) {
            convert_dates(DT, date_cols, pad_7_digit_dates = isTRUE(file_cfg$pad_7_digit_dates))
        } else {
            convert_dates(DT, date_cols, date_formats = fmts, pad_7_digit_dates = isTRUE(file_cfg$pad_7_digit_dates))
        }
    }
    force_named_date_cols(DT)

    DT
}

build_provider_datasets <- function(cfg, base_dir, provider_dirname, default_file_cfg = list()) {
    stopifnot(is.list(cfg))
    target_dir <- file.path(base_dir, "rawData", provider_dirname)

    lapply(cfg, function(x) {
        file_cfg <- merge_lists(default_file_cfg, x$file)
        load_provider_file(target_dir, file_cfg)
    })
}
