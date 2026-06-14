#!/bin/sh
# hb-build.sh — compile a Forth program into a standalone signed macOS binary.
#   tools/hb-build.sh prog.f -o out          AOT (default): compile MAIN to native,
#                                            strip the engine. Program must define `: MAIN ;`.
#   tools/hb-build.sh --repl prog.f -o out   verify, then bundle the full engine +
#                                            the program's definitions and REPL.
# In --repl mode the textual tree-shaker keeps every word NAMED in the source;
# add `EXPORT word1 word2 …` lines to keep extra words callable at the REPL.
# The output needs neither habu nor gforth to run.
set -e
cd "$(dirname "$0")/.."
CLEAN_T=0
if [ -n "${HB_TMP:-}" ]; then
  T=$HB_TMP
else
  T=$(mktemp -d "${TMPDIR:-/tmp}/hb-build.XXXXXX")
  CLEAN_T=1
fi
mkdir -p "$T"
export HB_TMP=$T
cleanup() {
  [ "$CLEAN_T" = 0 ] || rm -rf "$T"
}
trap cleanup EXIT HUP INT TERM
REPL=0
[ "$1" = "--repl" ] && { REPL=1; shift; }
SRC=$1
[ "$2" = "-o" ] && [ -n "$3" ] || { echo "usage: hb-build.sh [--repl] prog.f -o out"; exit 64; }
OUT=$3
[ -f "$SRC" ] || { echo "hb-build: no such source: $SRC"; exit 66; }
[ -x bin/hb ] || { echo "hb-build: bin/hb missing (run tools/build.sh first)"; exit 69; }

if [ "$REPL" = 1 ]; then DRIVER=build; ISRC=$T/hb-build-src; GOT=hb-build-got; MK=hb-build-mk
else                     DRIVER=aot;   ISRC=$T/hb-aot-src;   GOT=hb-aot-got;   MK=hb-aot-mk; fi
STAGE2_SRC=$T/stage2-src
STAGE2_GOT=$T/stage2-got
MKPATH=$T/$MK
GOTPATH=$T/$GOT

# maker = checker-hooked toolchain + the chosen driver, compiled by bin/hb.
# In default AOT mode the driver compiles the program in-process under CHECK!.
# In --repl mode the driver pre-verifies the user source with CHECK!, then
# bundles that source plus trusted REPL support for startup/runtime execution.
for f in $(./tools/srclist.sh $DRIVER); do
  [ "$f" = "src/core/sha256.f" ] && printf ': HOOK CHECK ; '"'"' HOOK set-check\n'
  cat "$f"; printf '\n'
done > "$STAGE2_SRC"
rm -f "$STAGE2_GOT"
bin/hb
[ -f "$STAGE2_GOT" ] || { echo "hb-build: bootstrap maker did not produce stage2-got"; exit 74; }
mv "$STAGE2_GOT" "$MKPATH"
chmod +x "$MKPATH"

# the program. EXPORT lines are commented out (the names stay in the source text
# so the tree-shaker keeps them, but they don't execute). --repl keeps a
# user-only copy for build-time verification, then appends repl.f so the bundle
# installs the interactive REPL on a tty.
sed 's/^[[:space:]]*EXPORT /\\ EXPORT /' "$SRC" > "$ISRC"
if [ "$REPL" = 1 ]; then
  cp "$ISRC" "$T/hb-build-check-src"
  printf '\n' >> "$ISRC"
  cat src/habu/repl.f >> "$ISRC"
fi
rm -f "$GOTPATH"
"$MKPATH"
[ -f "$GOTPATH" ] || { echo "hb-build: maker did not produce $GOT"; exit 74; }
mv "$GOTPATH" "$OUT"
chmod +x "$OUT"
echo "hb-build OK: $OUT ($(stat -f%z "$OUT") B, $([ "$REPL" = 1 ] && echo 'engine+REPL bundle' || echo 'AOT — engine stripped'))"
