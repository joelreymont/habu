#!/bin/sh
# gate runner: gforth exits nonzero on test failures (#ERRORS) AND on aborts.
set -e
G=${GFORTH:-$HOME/.local/bin/gforth}
cd "$(dirname "$0")/.."
./tools/parity-lint.py || { echo "FAIL: parity-lint"; exit 1; }
$G test/all.fs -e bye > /tmp/habu-gate.log 2>&1 || { tail -5 /tmp/habu-gate.log; echo "FAIL: all.fs"; exit 1; }
echo "PASS: full suite ($(grep -ac 'INCORRECT' /tmp/habu-gate.log || true) incorrect)"
