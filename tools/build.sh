#!/bin/sh
# build.sh — rebuild bin/hb and bin/hbi USING bin/hb (no gforth). The daily loop.
# bin/hb = engine + embedded builder source. A rebuild runs the OLD embedded
# builder, so gen1 = old engine + new text; only gen2 carries the new engine.
# Any change to the emitted engine therefore needs an extra generation before
# the output reproduces itself — iterate until gen_n == gen_{n+1}, bounded.
set -e
cd "$(dirname "$0")/.."
[ -x bin/hb ] || { echo "no bin/hb — run tools/bootstrap.sh once"; exit 1; }

mksrc() {  # $1 = driver (stage2|hbi); writes /tmp/stage2-src (checker hooked)
  for f in $(./tools/srclist.sh "$1"); do
    [ "$f" = "src/core/sha256.f" ] && printf ': HOOK CHECK ; '"'"' HOOK set-check\n'
    cat "$f"; printf '\n'
  done > /tmp/stage2-src
}

mksrc stage2
cp bin/hb /tmp/hb-gen; chmod +x /tmp/hb-gen
ok=0
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
    ok=1; break
  fi
  mv /tmp/stage2-got /tmp/hb-gen; chmod +x /tmp/hb-gen
done
[ "$ok" = 1 ] || { echo "FIXPOINT BROKEN: no convergence after 4 generations"; exit 1; }

# bin/hbi — the stdin-program engine (tools/probe.sh): bin/hb compiles
# toolchain+hbi.f into a maker binary; running the maker emits hbi.
mksrc hbi
rm -f /tmp/stage2-got
bin/hb
mv /tmp/stage2-got /tmp/hbi-mk; chmod +x /tmp/hbi-mk
rm -f /tmp/hbi-got
/tmp/hbi-mk
mv /tmp/hbi-got bin/hbi; chmod +x bin/hbi
codesign -v bin/hbi
echo "build OK: bin/hbi (stdin engine)"
