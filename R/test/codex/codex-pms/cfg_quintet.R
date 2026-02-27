cfg <- list(
    cshnxc = list(
        file = list(
            pattern = "^G000359_.*_Global_CshNxc_JRN_[0-9]{4}-[0-9]{2}-[0-9]{2}_.*_CSV\\.csv$",
            date_cols = "AUTO_DATE_COLS",
            add_filename_date = TRUE,
            filename_date_col = "as_of_date",
            filename_date_fmt = "%Y-%m-%d",
            filename_date_regex = "JRN_([0-9]{4}-[0-9]{2}-[0-9]{2})_",
            sep = ";"
        ),
        db = list(
            table = "cashtransactions",
            keys = c("client_id", "transaction_reference", "operation_date", "operation_currency", "iban", "quantity", "as_of_date")
        )
    ),

    titnxc = list(
        file = list(
            pattern = "^G000359_.*_Global_TitNxc_JRN_[0-9]{4}-[0-9]{2}-[0-9]{2}_.*_CSV\\.csv$",
            date_cols = "AUTO_DATE_COLS",
            add_filename_date = TRUE,
            filename_date_col = "as_of_date",
            filename_date_fmt = "%Y-%m-%d",
            filename_date_regex = "JRN_([0-9]{4}-[0-9]{2}-[0-9]{2})_",
            sep = ";"
        ),
        db = list(
            table = "securitytransactions",
            keys = c(
                "client_id",
                "transaction_reference",
                "type",
                "creation_date",
                "value_date",
                "isin_code",
                "kbl_account_number",
                "quantity",
                "as_of_date"
            )
        )
    ),

    cshstmt = list(
        file = list(
            pattern = "^G000359_.*_Global_CshStmt_JRN_[0-9]{4}-[0-9]{2}-[0-9]{2}_.*_CSV\\.csv$",
            date_cols = "AUTO_DATE_COLS",
            add_filename_date = TRUE,
            filename_date_col = "as_of_date",
            filename_date_fmt = "%Y-%m-%d",
            filename_date_regex = "JRN_([0-9]{4}-[0-9]{2}-[0-9]{2})_",
            sep = ";"
        ),
        db = list(
            table = "cashmovements",
            keys = c(
                "client_id",
                "transaction_reference",
                "value_date",
                "accounting_date",
                "transaction_currency",
                "movement_amount",
                "iban",
                "kbl_cash_account_number",
                "as_of_date"
            )
        )
    ),

    sctiesstmt = list(
        file = list(
            pattern = "^G000359_.*_Global_SctiesStmt_JRN_[0-9]{4}-[0-9]{2}-[0-9]{2}_.*_CSV\\.csv$",
            date_cols = "AUTO_DATE_COLS",
            add_filename_date = TRUE,
            filename_date_col = "as_of_date",
            filename_date_fmt = "%Y-%m-%d",
            filename_date_regex = "JRN_([0-9]{4}-[0-9]{2}-[0-9]{2})_",
            sep = ";"
        ),
        db = list(
            table = "securitymovements",
            keys = c(
                "client_id",
                "transaction_reference",
                "statement_number",
                "transaction_name",
                "balance_quantity",
                "transaction_quantity",
                "kbl_account_number",
                "value_date",
                "isin_code",
                "as_of_date"
            )
        )
    ),

    solcptcsh = list(
        file = list(
            pattern = "^G000359_.*_Global_SolCptCsh_JRN_[0-9]{4}-[0-9]{2}-[0-9]{2}_.*_CSV\\.csv$",
            merge_header_lines_2_3 = TRUE,
            date_cols = "AUTO_DATE_COLS",
            add_filename_date = TRUE,
            filename_date_col = "as_of_date",
            filename_date_fmt = "%Y-%m-%d",
            filename_date_regex = "JRN_([0-9]{4}-[0-9]{2}-[0-9]{2})_",
            add_filename_ts = TRUE,
            filename_ts_col = "as_of_file_ts",
            filename_ts_regex = "JRN_([0-9]{4}-[0-9]{2}-[0-9]{2}_[0-9]{2}_[0-9]{2}_[0-9]{2})_",
            add_source_row_id = TRUE,
            source_file_col = "source_file",
            source_row_col = "source_row_num",
            sep = ";"
        ),
        db = list(
            table = "cashpositions",
            keys = c("client_id", "kbl_cash_account_number", "iban", "value_date", "account_currency", "as_of_date", "as_of_file_ts", "source_file", "source_row_num")
        )
    ),

    solcpttit = list(
        file = list(
            pattern = "^G000359_.*_Global_SolCptTit_JRN_[0-9]{4}-[0-9]{2}-[0-9]{2}_.*_CSV\\.csv$",
            date_cols = "AUTO_DATE_COLS",
            add_filename_date = TRUE,
            filename_date_col = "as_of_date",
            filename_date_fmt = "%Y-%m-%d",
            filename_date_regex = "JRN_([0-9]{4}-[0-9]{2}-[0-9]{2})_",
            add_filename_ts = TRUE,
            filename_ts_col = "as_of_file_ts",
            filename_ts_regex = "JRN_([0-9]{4}-[0-9]{2}-[0-9]{2}_[0-9]{2}_[0-9]{2}_[0-9]{2})_",
            add_source_row_id = TRUE,
            source_file_col = "source_file",
            source_row_col = "source_row_num",
            sep = ";"
        ),
        db = list(
            table = "securitypostions",
            keys = c(
                "client_id",
                "kbl_account_number",
                "isin_code",
                "value_date",
                "as_of_date",
                "as_of_file_ts",
                "source_file",
                "source_row_num"
            )
        )
    )
)
