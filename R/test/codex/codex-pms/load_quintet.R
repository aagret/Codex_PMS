this_file <- tryCatch(normalizePath(sys.frame(1)$ofile), error = function(e) NA_character_)
this_dir <- if (is.na(this_file)) getwd() else dirname(this_file)
source(file.path(this_dir, "load_common.R"))

build_quintet_datasets <- function(cfg, base_dir) {
    default_file_cfg <- list(
        header_mode = "line2",
        sep = ";",
        file_encoding = "",
        fread_encoding = "UTF-8",
        add_filename_date = TRUE,
        filename_date_col = "as_of_date",
        filename_date_fmt = "%Y-%m-%d",
        filename_date_regex = "JRN_([0-9]{4}-[0-9]{2}-[0-9]{2})_"
    )

    build_provider_datasets(
        cfg = cfg,
        base_dir = base_dir,
        provider_dirname = "quintet",
        default_file_cfg = default_file_cfg
    )
}
