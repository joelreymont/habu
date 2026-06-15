#!/bin/sh
# build.sh — rebuild the single installed binary, bin/hb, USING bin/hb.
#
# bin/hb is the checked native engine users run. Build-only compiler engines are
# temporary files under $HB_TMP: first prove the stage2 compiler reaches a
# byte-for-byte fixpoint, then use that compiler to build a temporary stdin
# engine, feed it the snapshot script, and install the resulting checked engine
# as bin/hb. No other public artifacts are produced.
set -e
cd "$(dirname "$0")/.."
T=${HB_TMP:-/tmp}
[ -x bin/hb ] || { echo "no bin/hb — run tools/bootstrap.sh once"; exit 1; }

mkstage() {  # $1 = driver; writes $T/stage2-src (checker hooked)
  for f in $(./tools/srclist.sh "$1"); do
    [ "$f" = "src/core/sha256.f" ] && printf ': HOOK CHECK ; '"'"' HOOK set-check\n'
    cat "$f"; printf '\n'
  done > $T/stage2-src
}

mktool() {  # $1 = driver, $2 = output file
  for f in $(./tools/srclist.sh "$1"); do
    [ "$f" = "src/core/sha256.f" ] && printf ': HOOK CHECK ; '"'"' HOOK set-check\n'
    cat "$f"; printf '\n'
  done > "$2"
}

mkstage stage2
rm -f $T/stage2-got
bin/hb < src/habu/stage2.f
[ -f $T/stage2-got ] || { echo "build: stage script did not produce stage2-got"; exit 1; }
mv $T/stage2-got $T/hb-stage
chmod +x $T/hb-stage
ok=0
for g in 1 2 3 4; do
  rm -f $T/stage2-got
  $T/hb-stage
  if cmp -s $T/hb-stage $T/stage2-got; then
    codesign -v $T/hb-stage
    echo "build OK: stage compiler fixpoint"
    ok=1; break
  fi
  mv $T/stage2-got $T/hb-stage; chmod +x $T/hb-stage
done
[ "$ok" = 1 ] || { echo "FIXPOINT BROKEN: no convergence after 4 generations"; exit 1; }

# Temporary stdin engine: needed only to compile and snapshot the full toolchain
# from source without installing a second binary.
mkstage stdin
rm -f $T/stage2-got $T/hb-stdin-got
$T/hb-stage
[ -f $T/stage2-got ] || { echo "build: stdin maker not produced"; exit 1; }
mv $T/stage2-got $T/hb-stdin-mk
chmod +x $T/hb-stdin-mk
$T/hb-stdin-mk
[ -f $T/hb-stdin-got ] || { echo "build: stdin engine not produced"; exit 1; }
mv $T/hb-stdin-got $T/hb-stdin
chmod +x $T/hb-stdin
codesign -v $T/hb-stdin

# Installed hb: full checked toolchain plus tty REPL / pipeline stdin behavior.
mktool snap "$T/hb-snap-src"
rm -f $T/hb-snap0 $T/hb-new
$T/hb-stdin < "$T/hb-snap-src"
[ -f $T/hb-snap0 ] || { echo "build: checked hb image not produced"; exit 1; }
mv $T/hb-snap0 $T/hb-new
codesign -s - --force $T/hb-new 2>/dev/null
chmod +x $T/hb-new
mv $T/hb-new bin/hb
find bin -maxdepth 1 -type f ! -name hb -delete
echo "build OK: bin/hb (checked engine, tty REPL + stdin)"
