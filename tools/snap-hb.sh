#!/bin/sh
# snap-hb.sh — produce /tmp/hb-warm: a SNAPSHOT binary that boots with the
# whole toolchain already compiled (AOT — zero recompile at startup).
# Mechanics: feed bin/hbi the toolchain + the snap driver; the running engine
# serializes its own warm state; codesign the result.
set -e
cd "$(dirname "$0")/.."
[ -x bin/hbi ] || { echo "no bin/hbi — run tools/build.sh"; exit 1; }
{ for f in $(./tools/srclist.sh snap); do
    [ "$f" = "src/core/sha256.f" ] && printf ': HOOK CHECK ; '"'"' HOOK set-check\n'
    cat "$f"; printf '\n'
  done } > /tmp/hb-snap-src
rm -f /tmp/hb-warm0 /tmp/hb-warm
bin/hbi < /tmp/hb-snap-src
[ -f /tmp/hb-warm0 ] || { echo "FAIL: snapshot not written"; exit 1; }
mv /tmp/hb-warm0 /tmp/hb-warm
codesign -s - --force /tmp/hb-warm 2>/dev/null
chmod +x /tmp/hb-warm
echo "snap OK: /tmp/hb-warm ($(stat -f%z /tmp/hb-warm) bytes)"
