#!/bin/sh
# hb-aot.sh — AOT-compile a Forth program into a standalone signed macOS binary.
# Unlike hb-build.sh (which bakes the program as source for an embedded
# interpreter), this compiles the program in the MAKER and serializes only the
# native-reachable closure of MAIN — no interpreter, no engine. The program must
# define `: MAIN ;`. usage: tools/hb-aot.sh prog.f -o out
set -e
cd "$(dirname "$0")/.."
T=${HB_TMP:-/tmp}
SRC=$1
[ "$2" = "-o" ] && [ -n "$3" ] || { echo "usage: hb-aot.sh prog.f -o out"; exit 64; }
OUT=$3
[ -f "$SRC" ] || { echo "hb-aot: no such source: $SRC"; exit 66; }
[ -x bin/hb ] || { echo "hb-aot: bin/hb missing (run tools/build.sh first)"; exit 69; }

# maker = checker-hooked toolchain + aot.f, compiled by bin/hb (the program is
# type-checked as it compiles in the maker).
for f in $(./tools/srclist.sh aot); do
  [ "$f" = "src/core/sha256.f" ] && printf ': HOOK CHECK ; '"'"' HOOK set-check\n'
  cat "$f"; printf '\n'
done > $T/stage2-src
rm -f $T/stage2-got
bin/hb
mv $T/stage2-got $T/hb-aot-mk && chmod +x $T/hb-aot-mk

cp "$SRC" $T/hb-aot-src
rm -f $T/hb-aot-got
$T/hb-aot-mk
mv $T/hb-aot-got "$OUT" && chmod +x "$OUT"
echo "hb-aot OK: $OUT ($(stat -f%z "$OUT") B)"
