#!/bin/sh
# build.sh — rebuild bin/hb USING bin/hb (no gforth). The daily loop.
set -e
cd "$(dirname "$0")/.."
[ -x bin/hb ] || { echo "no bin/hb — run tools/bootstrap.sh once"; exit 1; }
SRC=$(./tools/srclist.sh)
{ for f in $SRC; do
    [ "$f" = "src/habu/rt.f" ] && printf ': HOOK CHECK ; '"'"' HOOK set-check\n'
    cat "$f"; printf '\n'
  done } > /tmp/stage2-src
rm -f /tmp/stage2-got
bin/hb
cmp bin/hb /tmp/stage2-got >/dev/null 2>&1 && { echo "build OK: bin/hb unchanged (fixpoint)"; exit 0; }
rm -f /tmp/stage2-got2; cp /tmp/stage2-got /tmp/hb-new; chmod +x /tmp/hb-new
rm -f /tmp/stage2-got; /tmp/hb-new               # verify the NEW binary self-reproduces
cmp /tmp/hb-new /tmp/stage2-got || { echo "FIXPOINT BROKEN for new build"; exit 1; }
mv /tmp/hb-new bin/hb
echo "build OK: bin/hb updated (new fixpoint verified)"
