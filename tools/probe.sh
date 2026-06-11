#!/bin/sh
# probe.sh '<habu program>' [expected-output] — build a one-off habu binary and
# run it. With expected-output: exits 1 on mismatch. Without: prints rc + output.
set -e
G=${GFORTH:-$HOME/.local/bin/gforth}
cd "$(dirname "$0")/.."
cat > /tmp/hb-probe.fs <<FS
require $(pwd)/test/sh-driver.fs
0 CL ! s" $1" +B CBUF CL @ s" /tmp/hb-probe-bin" FORTH-EXE
FS
rm -f /tmp/hb-probe-bin
$G /tmp/hb-probe.fs -e bye > /tmp/hb-probe.log 2>&1 || true
[ -x /tmp/hb-probe-bin ] || { echo "BUILD FAILED:"; grep -aE 'error' /tmp/hb-probe.log | head -3; exit 1; }
out=$(timeout 10 /tmp/hb-probe-bin 2>/tmp/hb-probe.err); rc=$?
if [ $# -ge 2 ]; then
  [ "$out" = "$2" ] && { echo "OK [$out]"; exit 0; }
  echo "MISMATCH rc=$rc got=[$out] want=[$2]"; head -2 /tmp/hb-probe.err; exit 1
fi
echo "rc=$rc out=[$out]"; head -2 /tmp/hb-probe.err
