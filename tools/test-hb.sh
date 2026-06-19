#!/bin/sh
# test-hb.sh — run the behavior suite ON THE ENGINE (bin/hb), no gforth.
# The native gate keeps lints/fixpoint/build coverage; this proves the same
# behaviors on the installed self-hosted binary.
cd "$(dirname "$0")/.."
[ -x bin/hb ] || { echo "no bin/hb — install a trusted seed with tools/seed.sh /path/to/hb"; exit 1; }
out=$(bin/hb < test/engine-suite.f) || { echo "$out"; echo "FAIL: engine suite (engine died)"; exit 1; }
case "$out" in
  *ok) echo "PASS: engine suite on bin/hb" ;;
  *) echo "$out"; echo "FAIL: engine suite"; exit 1 ;;
esac
