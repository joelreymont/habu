#!/bin/sh
# build.sh — rebuild bin/hb USING bin/hb (no gforth). The daily loop.
# bin/hb = engine + embedded builder source. A rebuild runs the OLD embedded
# builder, so gen1 = old engine + new text; only gen2 carries the new engine.
# Any change to the emitted engine therefore needs an extra generation before
# the output reproduces itself — iterate until gen_n == gen_{n+1}, bounded.
set -e
cd "$(dirname "$0")/.."
[ -x bin/hb ] || { echo "no bin/hb — run tools/bootstrap.sh once"; exit 1; }
SRC=$(./tools/srclist.sh)
{ for f in $SRC; do
    [ "$f" = "src/habu/rt.f" ] && printf ': HOOK CHECK ; '"'"' HOOK set-check\n'
    cat "$f"; printf '\n'
  done } > /tmp/stage2-src
cp bin/hb /tmp/hb-gen; chmod +x /tmp/hb-gen
for g in 1 2 3 4; do
  rm -f /tmp/stage2-got
  /tmp/hb-gen
  if cmp -s /tmp/hb-gen /tmp/stage2-got; then
    codesign -v /tmp/hb-gen
    if cmp -s bin/hb /tmp/hb-gen; then
      echo "build OK: bin/hb unchanged (fixpoint)"
    else
      mv /tmp/hb-gen bin/hb
      echo "build OK: bin/hb updated (fixpoint after $((g-1)) rebuild(s))"
    fi
    exit 0
  fi
  mv /tmp/stage2-got /tmp/hb-gen; chmod +x /tmp/hb-gen
done
echo "FIXPOINT BROKEN: no convergence after 4 generations"; exit 1
