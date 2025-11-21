#!/usr/bin/env bash
# Pure-Lisp runner for Habu compiler/REPL (no C backend)
# Loads run-habu.lisp which pulls in habu-arm64-codegen.lisp

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

echo "[Habu] Starting pure-Lisp driver (run-habu.lisp)..."
sbcl --noinform --non-interactive --load run-habu.lisp
