#!/usr/bin/env bash
# CI-style wrapper to run the pure-Lisp bring-up and capture logs

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LOG_DIR="${LOG_DIR:-ci-logs}"
LOG_FILE="${LOG_FILE:-${LOG_DIR}/run-habu-lisp.log}"

mkdir -p "${LOG_DIR}"

echo "[CI] running run-habu-lisp.sh; log -> ${LOG_FILE}"
(cd "${SCRIPT_DIR}" && ./run-habu-lisp.sh) | tee "${LOG_FILE}"
