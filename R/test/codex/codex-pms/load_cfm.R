this_file <- tryCatch(normalizePath(sys.frame(1)$ofile), error = function(e) NA_character_)
this_dir <- if (is.na(this_file)) getwd() else dirname(this_file)
source(file.path(this_dir, "load_common.R"))

build_cfm_datasets <- function(cfg, base_dir) {
    default_file_cfg <- list(
        header_mode = "none",
        sep = ";",
        file_encoding = "latin1",
        fread_encoding = "UTF-8",
        apply_auto_numeric = FALSE,
        apply_force_numeric = FALSE,
        add_filename_date = FALSE,
        filename_date_col = "as_of_date",
        filename_date_prefix_n = 8L,
        filename_date_fmt = "%Y%m%d",
        date_formats = c("%d%m%Y"),
        pad_7_digit_dates = TRUE
    )

    build_provider_datasets(
        cfg = cfg,
        base_dir = base_dir,
        provider_dirname = "cfm",
        default_file_cfg = default_file_cfg
    )
}
