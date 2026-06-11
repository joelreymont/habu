#!/bin/sh
# probe.sh '<habu program>' [expected-output] — run a habu program on the
# WORKING-TREE engine (gforth-built stdin exe, cached on source checksum), so
# engine changes are probeable before any install. With expected-output: exits 1
# on mismatch. Without: prints rc + output.
# PROBE_FILES: source files piped before the program.
# PROBE_INSTALLED=1: run on bin/hbi (last installed fixpoint) instead.
cd "$(dirname "$0")/.."
G=${GFORTH:-$HOME/.local/bin/gforth}
if [ "$PROBE_INSTALLED" = 1 ]; then
  BIN=bin/hbi
  [ -x "$BIN" ] || { echo "no bin/hbi — run tools/build.sh"; exit 1; }
else
  BIN=/tmp/hb-probe-repl
  SUM=$(cat bootstrap/cg/*.fs | shasum | cut -d' ' -f1)
  if [ ! -x "$BIN" ] || [ "$(cat /tmp/hb-probe-repl.sum 2>/dev/null)" != "$SUM" ]; then
    cat > /tmp/hb-probe.fs <<FS
require $(pwd)/test/nf.fs
s" $BIN" FORTH-REPL-EXE
FS
    rm -f "$BIN"
    $G /tmp/hb-probe.fs -e bye > /tmp/hb-probe.log 2>&1 || true
    [ -x "$BIN" ] || { echo "BUILD FAILED:"; grep -aE 'error' /tmp/hb-probe.log | head -3; exit 1; }
    echo "$SUM" > /tmp/hb-probe-repl.sum
  fi
fi
rc=0
out=$({ for f in $PROBE_FILES; do cat "$f"; printf '\n'; done
        printf '%s\n' "$1"; } | timeout 10 "$BIN" 2>/tmp/hb-probe.err) || rc=$?
if [ $# -ge 2 ]; then
  [ "$out" = "$2" ] && { echo "OK [$out]"; exit 0; }
  echo "MISMATCH rc=$rc got=[$out] want=[$2]"; head -2 /tmp/hb-probe.err; exit 1
fi
echo "rc=$rc out=[$out]"; head -2 /tmp/hb-probe.err
