#!/bin/sh
# run.sh — the DEFAULT gate: habu-native, no gforth anywhere on the path.
#   lints -> self-rebuild fixpoint -> hb-suite -> AOT snapshot -> tty REPL ->
#   hb-build standalone.
# The gforth differential (boot-vs-port goldens + the gforth-hosted checker
# suite) lives in tools/oracle.sh — run it before pushing emitter changes.
# `test/run.sh full` runs both.
set -e
cd "$(dirname "$0")/.."
T=${HB_TMP:-/tmp}
./tools/parity-lint.py || { echo "FAIL: parity-lint"; exit 1; }
./tools/shadow-lint.py || { echo "FAIL: shadow-lint"; exit 1; }
./tools/clobber-lint.py || { echo "FAIL: clobber-lint"; exit 1; }
[ -x bin/hb ] || { echo "no bin/hb — run tools/bootstrap.sh once"; exit 1; }
./tools/build.sh > $T/hb-build.log 2>&1 || { tail -5 $T/hb-build.log; echo "FAIL: build (fixpoint)"; exit 1; }
echo "PASS: self-rebuild fixpoint"
./tools/test-hb.sh || exit 1
./tools/snap-hb.sh >/dev/null || { echo "FAIL: snap-hb"; exit 1; }
out=$(echo 's" w" s" n -- n" trust 7 . : Q 5 dup * . ; Q' | $T/hb-warm)
[ "$out" = "7
25" ] || { echo "FAIL: warm snapshot (got: $out)"; exit 1; }
out=$(echo 's" HOME" getenv nip 0 > .' | $T/hb-warm)
[ "$out" = "-1" ] || { echo "FAIL: getenv (got: $out)"; exit 1; }
# the warm snapshot is checked-Forth: a typed def whose body violates its sig
# is rejected (unpublished -> calling it exits 70); a correct one runs.
out=$(printf ': SQOK ( i64 -- i64 ) dup * ;\n7 SQOK .\n' | $T/hb-warm 2>/dev/null)
[ "$out" = "49" ] || { echo "FAIL: snapshot good typed def (got: $out)"; exit 1; }
printf ': SQBAD ( i64 -- i64 ) dup ;\nSQBAD\n' | $T/hb-warm >/dev/null 2>&1 && { echo "FAIL: snapshot did NOT reject bad sig"; exit 1; }
# named rows + quot sub-sigs VERIFY (Gap3): CHECK! body-vs-declared-sig.
# V1 row-poly certifies, V2 combinator-param certifies, V3 bad row count rejects.
out=$(printf 's" V1 ( R -- R i64 ) 5" CHECK! .\ns" V2 ( i64 [ i64 -- i64 ] -- i64 ) execute" CHECK! .\ns" V3 ( R -- R i64 ) 5 5" CHECK! .\n' | $T/hb-warm 2>/dev/null)
[ "$out" = "-1
-1
0" ] || { echo "FAIL: snapshot rows/quot sig verify (got: $out)"; exit 1; }
# and the row sig runs end to end
out=$(printf ': PSH ( R -- R i64 ) 5 ;\nPSH .\n' | $T/hb-warm 2>/dev/null)
[ "$out" = "5" ] || { echo "FAIL: snapshot named-row sig run (got: $out)"; exit 1; }
echo "PASS: AOT snapshot (warm toolchain boot) + getenv + sig-check (rows+quots)"
HT=$(mktemp -d)
HB_TMP=$HT ./tools/snap-hb.sh >/dev/null && [ -x "$HT/hb-warm" ] || { echo "FAIL: HB_TMP isolation"; exit 1; }
rm -rf "$HT"
echo "PASS: HB_TMP isolation"
python3 test/repl-pty.py || { echo "FAIL: tty REPL"; exit 1; }
printf ': T 6 7 * . ;\nT\n' > $T/hb-bt.f
./tools/hb-build.sh $T/hb-bt.f -o $T/hb-bt >/dev/null || { echo "FAIL: hb-build"; exit 1; }
[ "$($T/hb-bt)" = "42" ] || { echo "FAIL: hb-build output (got: $($T/hb-bt))"; exit 1; }
[ "$(stat -f%z $T/hb-bt)" -lt 20000 ] || { echo "FAIL: hb-build size ($(stat -f%z $T/hb-bt) >= 20000 — tree shake regressed)"; exit 1; }
echo "PASS: hb-build standalone (shaken, $(stat -f%z $T/hb-bt) B)"
if [ "$1" = "full" ]; then
  ./tools/oracle.sh || exit 1
fi
echo "PASS: native gate (fixpoint + hb-suite + snapshot + repl + hb-build)${1:+ + oracle}"
