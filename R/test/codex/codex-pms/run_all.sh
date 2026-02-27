#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LOG_DIR="${SCRIPT_DIR}/logs"
mkdir -p "${LOG_DIR}"

TS="$(date +"%Y%m%d_%H%M%S")"
LOG_FILE="${LOG_DIR}/run_all_${TS}.log"

echo "[$(date +"%F %T")] START run_all" | tee -a "${LOG_FILE}"

Rscript "${SCRIPT_DIR}/run_all.R" 2>&1 | tee -a "${LOG_FILE}"

RC=${PIPESTATUS[0]}
if [[ ${RC} -ne 0 ]]; then
  echo "[$(date +"%F %T")] FAILED rc=${RC}" | tee -a "${LOG_FILE}"
  exit ${RC}
fi

echo "[$(date +"%F %T")] DONE run_all" | tee -a "${LOG_FILE}"
