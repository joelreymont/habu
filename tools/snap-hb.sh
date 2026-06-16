#!/bin/sh
# snap-hb.sh — refresh bin/hb from the currently installed checked engine.
# The installed hb runs the snapshot script from stdin, serializes its live
# toolchain state, and this wrapper signs/replaces bin/hb. Source changes should
# normally go through tools/build.sh, which rebuilds from source before this step.
set -e
cd "$(dirname "$0")/.."
T=${HB_TMP:-/tmp}
[ -x bin/hb ] || { echo "no bin/hb — install a trusted seed with tools/seed.sh /path/to/hb"; exit 1; }
rm -f "$T/hb-snap0" "$T/hb-new"
bin/hb < src/habu/snap.f
[ -f "$T/hb-snap0" ] || { echo "FAIL: snapshot not written"; exit 1; }
mv "$T/hb-snap0" "$T/hb-new"
codesign -s - --force "$T/hb-new" 2>/dev/null
chmod +x "$T/hb-new"
mv "$T/hb-new" bin/hb
find bin -maxdepth 1 -type f ! -name hb -delete
echo "snap OK: bin/hb ($(stat -f%z bin/hb) bytes)"
