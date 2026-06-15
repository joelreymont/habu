#!/bin/sh
# bootstrap.sh — build bin/hb from nothing but gforth. Used once (or after deep
# changes); daily rebuilds use tools/build.sh (no gforth). The result is the
# SELF-COMPILED binary: gforth builds stage0, stage0 compiles the source, and we
# keep stage0's output (verified: it reproduces itself byte-for-byte).
set -e
G=${GFORTH:-$HOME/.local/bin/gforth}
cd "$(dirname "$0")/.."
CLEAN_T=0
if [ -n "${HB_TMP:-}" ]; then
  T=$HB_TMP
else
  T=$(mktemp -d "${TMPDIR:-/tmp}/hb-bootstrap.XXXXXX")
  CLEAN_T=1
fi
mkdir -p "$T"
export HB_TMP=$T
cleanup() {
  [ "$CLEAN_T" = 0 ] || rm -rf "$T"
}
trap cleanup EXIT HUP INT TERM
mkdir -p bin
SRC=$(./tools/srclist.sh)
# the compiler source, with the checker hooked (habu type-checks itself)
{ for f in $SRC; do
    [ "$f" = "src/core/sha256.f" ] && printf ': HOOK CHECK ; '"'"' HOOK set-check\n'
    cat "$f"; printf '\n'
  done } > "$T/hb-src"
cat > "$T/hb-boot.fs" <<FS
require $(pwd)/test/nf.fs
s" $T/hb-src" slurp-file s" $T/hb-stage0" FORTH-EXE
FS
"$G" "$T/hb-boot.fs" -e bye > "$T/hb-boot.log" 2>&1 || { tail -3 "$T/hb-boot.log"; exit 1; }
cp "$T/hb-src" "$T/stage2-src"
rm -f "$T/stage2-got"
"$T/hb-stage0"                                # stage0 compiles the source -> stage1
[ -f "$T/stage2-got" ] || { echo "bootstrap: stage0 did not produce stage2-got"; exit 1; }
mv "$T/stage2-got" bin/hb
chmod +x bin/hb
cp "$T/hb-src" "$T/stage2-src"
rm -f "$T/stage2-got"
bin/hb                                          # stage1 -> stage2
[ -f "$T/stage2-got" ] || { echo "bootstrap: stage1 did not produce stage2-got"; exit 1; }
cmp bin/hb "$T/stage2-got" || { echo "FIXPOINT BROKEN"; exit 1; }
codesign -v bin/hb 2>/dev/null
echo "bootstrap OK: stage compiler bootstrapped"
./tools/build.sh                                # installs the checked bin/hb
