#!/bin/sh
# hb-build.sh — compile a Forth program into a standalone signed macOS binary.
#   tools/hb-build.sh prog.f -o out          AOT (default): compile MAIN to native,
#                                            strip the engine. Program must define `: MAIN ;`.
#   tools/hb-build.sh --repl prog.f -o out   bundle the full engine + the program's
#                                            definitions and drop into the REPL on a tty.
# In --repl mode the textual tree-shaker keeps every word NAMED in the source;
# add `EXPORT word1 word2 …` lines to keep extra words callable at the REPL.
# The output needs neither habu nor gforth to run.
set -e
cd "$(dirname "$0")/.."
T=${HB_TMP:-/tmp}
REPL=0
[ "$1" = "--repl" ] && { REPL=1; shift; }
SRC=$1
[ "$2" = "-o" ] && [ -n "$3" ] || { echo "usage: hb-build.sh [--repl] prog.f -o out"; exit 64; }
OUT=$3
[ -f "$SRC" ] || { echo "hb-build: no such source: $SRC"; exit 66; }
[ -x bin/hb ] || { echo "hb-build: bin/hb missing (run tools/build.sh first)"; exit 69; }

if [ "$REPL" = 1 ]; then DRIVER=build; ISRC=$T/hb-build-src; GOT=hb-build-got; MK=hb-build-mk
else                     DRIVER=aot;   ISRC=$T/hb-aot-src;   GOT=hb-aot-got;   MK=hb-aot-mk; fi

# maker = checker-hooked toolchain + the chosen driver, compiled by bin/hb.
# In default AOT mode the driver compiles the program in-process under CHECK!.
# In --repl mode the source is bundled and recompiled by the emitted engine at
# startup, so the bundle keeps the interpreter/REPL rather than enforcing checks
# at build time.
for f in $(./tools/srclist.sh $DRIVER); do
  [ "$f" = "src/core/sha256.f" ] && printf ': HOOK CHECK ; '"'"' HOOK set-check\n'
  cat "$f"; printf '\n'
done > $T/stage2-src
rm -f $T/stage2-got
bin/hb
mv $T/stage2-got $T/$MK && chmod +x $T/$MK

# the program. EXPORT lines are commented out (the names stay in the source text
# so the tree-shaker keeps them, but they don't execute). --repl appends repl.f so
# the bundle installs the interactive REPL on a tty.
sed 's/^[[:space:]]*EXPORT /\\ EXPORT /' "$SRC" > $ISRC
if [ "$REPL" = 1 ]; then printf '\n' >> $ISRC; cat src/habu/repl.f >> $ISRC; fi
rm -f $T/$GOT
$T/$MK
mv $T/$GOT "$OUT" && chmod +x "$OUT"
echo "hb-build OK: $OUT ($(stat -f%z "$OUT") B, $([ "$REPL" = 1 ] && echo 'engine+REPL bundle' || echo 'AOT — engine stripped'))"
