#!/usr/bin/env bash
# CI-style wrapper to run the pure-Lisp bring-up and capture logs

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LOG_DIR="${LOG_DIR:-ci-logs}"
LOG_FILE="${LOG_FILE:-${LOG_DIR}/run-habu-lisp.log}"

mkdir -p "${LOG_DIR}"

echo "[CI] running run-habu-lisp.sh; log -> ${LOG_FILE}"
(cd "${SCRIPT_DIR}" && ./run-habu-lisp.sh) | tee "${LOG_FILE}"

if ! grep -E "compile-to-arm64 42 produced [1-9][0-9]* bytes" "${LOG_FILE}" >/dev/null; then
  echo "[CI] smoke output missing byte count" >&2
  exit 1
fi

if ! grep -q "HEXDUMP" "${LOG_FILE}"; then
  echo "[CI] smoke output missing hexdump" >&2
  exit 1
fi

echo "[CI] smoke check passed"
