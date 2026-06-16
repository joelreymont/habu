#!/bin/sh
# probe.sh '<habu program>' [expected-output] — run a habu program on the
# installed native engine. With expected-output: exits 1 on mismatch.
# Without: prints rc + output. PROBE_FILES are piped before the program.
set -e
cd "$(dirname "$0")/.."
[ -x bin/hb ] || { echo "no bin/hb — install a trusted seed with tools/seed.sh /path/to/hb"; exit 1; }
T=$(mktemp -d "${TMPDIR:-/tmp}/hb-probe.XXXXXX")
cleanup() {
  if command -v trash >/dev/null 2>&1; then
    trash "$T"
  else
    rm -r "$T"
  fi
}
trap cleanup EXIT HUP INT TERM
rc=0
out=$({ for f in $PROBE_FILES; do cat "$f"; printf '\n'; done
        printf '%s\n' "$1"; } | timeout 10 bin/hb 2>"$T/probe.err") || rc=$?
if [ $# -ge 2 ]; then
  [ "$out" = "$2" ] && { echo "OK [$out]"; exit 0; }
  echo "MISMATCH rc=$rc got=[$out] want=[$2]"; head -2 "$T/probe.err"; exit 1
fi
echo "rc=$rc out=[$out]"; head -2 "$T/probe.err"
