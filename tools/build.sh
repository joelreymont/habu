#!/bin/sh
# build.sh — rebuild bin/hb and bin/hbi USING bin/hb (no gforth). The daily loop.
# bin/hb = engine + embedded builder source. A rebuild runs the OLD embedded
# builder, so gen1 = old engine + new text; only gen2 carries the new engine.
# Any change to the emitted engine therefore needs an extra generation before
# the output reproduces itself — iterate until gen_n == gen_{n+1}, bounded.
set -e
cd "$(dirname "$0")/.."
T=${HB_TMP:-/tmp}
[ -x bin/hb ] || { echo "no bin/hb — run tools/bootstrap.sh once"; exit 1; }

mksrc() {  # $1 = driver (stage2|hbi); writes $T/stage2-src (checker hooked)
  for f in $(./tools/srclist.sh "$1"); do
    [ "$f" = "src/core/sha256.f" ] && printf ': HOOK CHECK ; '"'"' HOOK set-check\n'
    cat "$f"; printf '\n'
  done > $T/stage2-src
}

mksrc stage2
cp bin/hb $T/hb-gen; chmod +x $T/hb-gen
ok=0
for g in 1 2 3 4; do
  rm -f $T/stage2-got
  $T/hb-gen
  if cmp -s $T/hb-gen $T/stage2-got; then
    codesign -v $T/hb-gen
    if cmp -s bin/hb $T/hb-gen; then
      echo "build OK: bin/hb unchanged (fixpoint)"
    else
      mv $T/hb-gen bin/hb
      echo "build OK: bin/hb updated (fixpoint after $((g-1)) rebuild(s))"
    fi
    ok=1; break
  fi
  mv $T/stage2-got $T/hb-gen; chmod +x $T/hb-gen
done
[ "$ok" = 1 ] || { echo "FIXPOINT BROKEN: no convergence after 4 generations"; exit 1; }

# bin/hbi — the stdin-program engine (tools/probe.sh): bin/hb compiles
# toolchain+hbi.f into a maker binary; running the maker emits hbi.
mksrc hbi
rm -f $T/stage2-got
bin/hb
mv $T/stage2-got $T/hbi-mk; chmod +x $T/hbi-mk
rm -f $T/hbi-got
$T/hbi-mk
mv $T/hbi-got bin/hbi; chmod +x bin/hbi
codesign -v bin/hbi
echo "build OK: bin/hbi (stdin engine)"
