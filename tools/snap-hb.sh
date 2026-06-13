#!/bin/sh
# snap-hb.sh — produce $T/hb-warm: a SNAPSHOT binary that boots with the
# whole toolchain already compiled (AOT — zero recompile at startup).
# Mechanics: feed bin/hbi the toolchain + the snap driver; the running engine
# serializes its own warm state; codesign the result.
set -e
cd "$(dirname "$0")/.."
T=${HB_TMP:-/tmp}
[ -x bin/hbi ] || { echo "no bin/hbi — run tools/build.sh"; exit 1; }
{ for f in $(./tools/srclist.sh snap); do
    [ "$f" = "src/core/sha256.f" ] && printf ': HOOK CHECK ; '"'"' HOOK set-check\n'
    cat "$f"; printf '\n'
  done } > $T/hb-snap-src
rm -f $T/hb-warm0 $T/hb-warm
bin/hbi < $T/hb-snap-src
[ -f $T/hb-warm0 ] || { echo "FAIL: snapshot not written"; exit 1; }
mv $T/hb-warm0 $T/hb-warm
codesign -s - --force $T/hb-warm 2>/dev/null
chmod +x $T/hb-warm
# bin/habu = the checked native REPL: full toolchain warm, verify hook on, ~3ms
# boot (gitignored, regenerable). bin/hbi stays the minimal stdin engine.
cp $T/hb-warm bin/habu && codesign -s - --force bin/habu 2>/dev/null && chmod +x bin/habu
echo "snap OK: $T/hb-warm + bin/habu ($(stat -f%z $T/hb-warm) bytes)"
