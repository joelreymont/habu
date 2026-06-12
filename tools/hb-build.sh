#!/bin/sh
# hb-build.sh — compile a Forth program into a standalone signed macOS binary.
# usage: tools/hb-build.sh prog.f -o prog
# The output needs neither habu nor gforth to run: it is the bare engine with
# the program baked as its source (runs at startup, then exits).
set -e
cd "$(dirname "$0")/.."
T=${HB_TMP:-/tmp}
SRC=$1
[ "$2" = "-o" ] && [ -n "$3" ] || { echo "usage: hb-build.sh prog.f -o out"; exit 64; }
OUT=$3
[ -f "$SRC" ] || { echo "hb-build: no such source: $SRC"; exit 66; }
[ -x bin/hb ] || { echo "hb-build: bin/hb missing (run tools/build.sh first)"; exit 69; }

# maker = checker-hooked toolchain + build.f, compiled by bin/hb (build.sh's
# hbi-mk pattern; bin/hb's stage2 driver reads $T/stage2-src)
for f in $(./tools/srclist.sh build); do
  [ "$f" = "src/core/sha256.f" ] && printf ': HOOK CHECK ; '"'"' HOOK set-check\n'
  cat "$f"; printf '\n'
done > $T/stage2-src
rm -f $T/stage2-got
bin/hb
mv $T/stage2-got $T/hb-build-mk && chmod +x $T/hb-build-mk

cp "$SRC" $T/hb-build-src
rm -f $T/hb-build-got
$T/hb-build-mk
mv $T/hb-build-got "$OUT" && chmod +x "$OUT"
echo "hb-build OK: $OUT"
