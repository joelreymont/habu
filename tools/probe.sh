#!/bin/sh
# probe.sh '<habu program>' [expected-output] — run a habu program on bin/hbi
# (the stdin-program engine). With expected-output: exits 1 on mismatch.
# Without: prints rc + output. PROBE_FILES: source files piped before the program.
cd "$(dirname "$0")/.."
[ -x bin/hbi ] || { echo "no bin/hbi — run tools/build.sh"; exit 1; }
rc=0
out=$({ for f in $PROBE_FILES; do cat "$f"; printf '\n'; done
        printf '%s\n' "$1"; } | timeout 10 bin/hbi 2>/tmp/hb-probe.err) || rc=$?
if [ $# -ge 2 ]; then
  [ "$out" = "$2" ] && { echo "OK [$out]"; exit 0; }
  echo "MISMATCH rc=$rc got=[$out] want=[$2]"; head -2 /tmp/hb-probe.err; exit 1
fi
echo "rc=$rc out=[$out]"; head -2 /tmp/hb-probe.err
