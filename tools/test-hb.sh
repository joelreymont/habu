#!/bin/sh
# test-hb.sh — run the behavior suite ON THE ENGINE (bin/hb), no gforth.
# The native gate keeps lints/fixpoint/build coverage; this proves the same
# behaviors on the installed self-hosted binary.
cd "$(dirname "$0")/.."
[ -x bin/hb ] || { echo "no bin/hb — run tools/build.sh"; exit 1; }
out=$(bin/hb < test/hb-suite.f) || { echo "$out"; echo "FAIL: hb-suite (engine died)"; exit 1; }
case "$out" in
  *ok) echo "PASS: hb-suite on bin/hb" ;;
  *) echo "$out"; echo "FAIL: hb-suite"; exit 1 ;;
esac
