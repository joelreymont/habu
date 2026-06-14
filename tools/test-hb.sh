#!/bin/sh
# test-hb.sh — run the behavior suite ON THE ENGINE (bin/hbi), no gforth.
# The native gate keeps lints/fixpoint/build coverage; this proves the same
# behaviors on the installed self-hosted binary.
cd "$(dirname "$0")/.."
[ -x bin/hbi ] || { echo "no bin/hbi — run tools/build.sh"; exit 1; }
out=$(bin/hbi < test/hb-suite.f) || { echo "$out"; echo "FAIL: hb-suite (engine died)"; exit 1; }
case "$out" in
  *ok) echo "PASS: hb-suite on bin/hbi" ;;
  *) echo "$out"; echo "FAIL: hb-suite"; exit 1 ;;
esac
