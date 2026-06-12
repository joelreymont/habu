#!/bin/sh
# gate runner: gforth exits nonzero on test failures (#ERRORS) AND on aborts.
# BOTH suites run: all.fs (gforth-hosted) and selfhost-all.fs (t-sh-* gates).
set -e
G=${GFORTH:-$HOME/.local/bin/gforth}
cd "$(dirname "$0")/.."
./tools/parity-lint.py || { echo "FAIL: parity-lint"; exit 1; }
$G test/all.fs -e bye > /tmp/habu-gate.log 2>&1 || { tail -5 /tmp/habu-gate.log; echo "FAIL: all.fs"; exit 1; }
$G test/selfhost-all.fs -e bye > /tmp/habu-shgate.log 2>&1 || { tail -5 /tmp/habu-shgate.log; echo "FAIL: selfhost-all.fs"; exit 1; }
[ -x bin/hbi ] && { ./tools/test-hb.sh || exit 1; }
echo "PASS: full suite (all.fs + selfhost-all.fs + hb-suite)"
