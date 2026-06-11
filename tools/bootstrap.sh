#!/bin/sh
# bootstrap.sh — build bin/hb from nothing but gforth. Used once (or after deep
# changes); daily rebuilds use tools/build.sh (no gforth). The result is the
# SELF-COMPILED binary: gforth builds stage0, stage0 compiles the source, and we
# keep stage0's output (verified: it reproduces itself byte-for-byte).
set -e
G=${GFORTH:-$HOME/.local/bin/gforth}
cd "$(dirname "$0")/.."
mkdir -p bin
SRC=$(./tools/srclist.sh)
# the compiler source, with the checker hooked (habu type-checks itself)
{ for f in $SRC; do
    [ "$f" = "src/core/sha256.f" ] && printf ': HOOK CHECK ; '"'"' HOOK set-check\n'
    cat "$f"; printf '\n'
  done } > /tmp/hb-src
cat > /tmp/hb-boot.fs <<FS
require $(pwd)/test/nf.fs
s" /tmp/hb-src" slurp-file s" /tmp/hb-stage0" FORTH-EXE
FS
$G /tmp/hb-boot.fs -e bye > /tmp/hb-boot.log 2>&1 || { tail -3 /tmp/hb-boot.log; exit 1; }
cp /tmp/hb-src /tmp/stage2-src
rm -f /tmp/stage2-got
/tmp/hb-stage0                                  # stage0 compiles the source -> stage1
mv /tmp/stage2-got bin/hb && chmod +x bin/hb
cp /tmp/hb-src /tmp/stage2-src && rm -f /tmp/stage2-got
bin/hb                                          # stage1 -> stage2
cmp bin/hb /tmp/stage2-got || { echo "FIXPOINT BROKEN"; exit 1; }
codesign -v bin/hb 2>/dev/null && echo "bootstrap OK: bin/hb (self-compiled, signed, fixpoint verified)"
./tools/build.sh                                # bin/hbi (stdin engine) for probe.sh
