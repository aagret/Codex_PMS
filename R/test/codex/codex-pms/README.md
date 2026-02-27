# codex-pms

Refactored ingestion layout with shared loader logic and provider-specific thin wrappers.

## Structure

- `load_common.R`: shared CSV ingestion engine (header handling, encoding cleanup, numeric/date parsing, filename metadata, provenance columns).
- `load_cfm.R`, `load_edr.R`, `load_quintet.R`: provider wrappers with defaults only.
- `cfg_*.R`: provider datasets/tables/keys definitions.
- `db_schema.R`, `db_upsert.R`: schema + upsert logic.
- `run_*.R`: runnable provider entrypoints.

## Run

```r
source("/home/Alexandre/PMS/R/test/codex/codex-pms/run_quintet.R")
```

or:

```r
source("/home/Alexandre/PMS/R/test/codex/codex-pms/run_edr.R")
source("/home/Alexandre/PMS/R/test/codex/codex-pms/run_cfm.R")
```

## Notes

- Provider differences should go in `cfg_*.R` (file patterns, keys, header mode, metadata fields).
- Keep core transformations in `load_common.R` to avoid code repetition.
