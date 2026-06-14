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
./tools/repl-lint.py || { echo "FAIL: repl-lint"; exit 1; }
./tools/trust-lint.py || { echo "FAIL: trust-lint"; exit 1; }
./tools/stale-status-lint.py || { echo "FAIL: stale-status-lint"; exit 1; }
[ -x bin/hb ] || { echo "no bin/hb — run tools/bootstrap.sh once"; exit 1; }
./tools/build.sh > $T/hb-build.log 2>&1 || { tail -5 $T/hb-build.log; echo "FAIL: build (fixpoint)"; exit 1; }
echo "PASS: self-rebuild fixpoint"
./tools/test-hb.sh || exit 1
# divide/modulo by zero must fail loudly (ARM64 SDIV yields 0 silently); the
# engine traps, so the run exits nonzero instead of printing a bogus result.
printf '1 0 / .\n'   | bin/hbi >/dev/null 2>&1 && { echo "FAIL: 1 0 / did not trap"; exit 1; }
printf '1 0 mod .\n' | bin/hbi >/dev/null 2>&1 && { echo "FAIL: 1 0 mod did not trap"; exit 1; }
printf '7 2 / . 7 2 mod . cr\n' | bin/hbi 2>/dev/null | tr -d '\n ' | grep -q '^31$' || { echo "FAIL: nonzero div/mod regressed"; exit 1; }
echo "PASS: div/mod by zero traps (no silent 0)"
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
printf ': SQBAD ( i64 -- i64 ) dup ;\n7 SQBAD .\n' | $T/hb-warm >/dev/null 2>&1 && { echo "FAIL: snapshot did NOT reject bad sig"; exit 1; }
# named rows + quot sub-sigs VERIFY (Gap3): CHECK! body-vs-declared-sig.
# V1 row-poly certifies, V2 combinator-param certifies, V3 bad row count rejects.
out=$(printf 's" V1 ( R -- R i64 ) 5" CHECK! .\ns" V2 ( i64 [ i64 -- i64 ] -- i64 ) execute" CHECK! .\ns" V3 ( R -- R i64 ) 5 5" CHECK! .\n' | $T/hb-warm 2>/dev/null)
[ "$out" = "-1
-1
0" ] || { echo "FAIL: snapshot rows/quot sig verify (got: $out)"; exit 1; }
# and the row sig runs end to end
out=$(printf ': PSH ( R -- R i64 ) 5 ;\nPSH .\n' | $T/hb-warm 2>/dev/null)
[ "$out" = "5" ] || { echo "FAIL: snapshot named-row sig run (got: $out)"; exit 1; }
[ -x bin/habu ] || { echo "FAIL: bin/habu (checked REPL) not produced"; exit 1; }
printf ': JBAD ( i64 -- i64 ) dup ;\n' | ./tools/check.sh --json-errors >/dev/null 2>$T/habu-json.err || { echo "FAIL: tools/check.sh --json-errors"; exit 1; }
grep -q '"verdict":"rejected"' $T/habu-json.err || { echo "FAIL: --json-errors missing verdict"; exit 1; }
grep -q '"declared_effect":"i64 -- i64 ' $T/habu-json.err || { echo "FAIL: --json-errors missing declared effect"; exit 1; }
grep -q '"inferred_effect":"i64 -- i64 i64 ' $T/habu-json.err || { echo "FAIL: --json-errors missing inferred effect"; exit 1; }
grep -q '"token_index":1' $T/habu-json.err || { echo "FAIL: --json-errors missing token index"; exit 1; }
echo "PASS: AOT snapshot (warm toolchain boot) + getenv + sig-check (rows+quots) + bin/habu"
HT=$(mktemp -d)
HB_TMP=$HT ./tools/snap-hb.sh >/dev/null && [ -x "$HT/hb-warm" ] || { echo "FAIL: HB_TMP isolation"; exit 1; }
out=$(printf '$340000000 $1B0 + @ 0= .\n: SQOK ( i64 -- i64 ) dup * ;\n7 SQOK .\n' | "$HT/hb-warm" 2>/dev/null)
[ "$out" = "0
49" ] || { echo "FAIL: HB_TMP snapshot restore/check hook (got: $out)"; exit 1; }
rm -rf "$HT"
echo "PASS: HB_TMP isolation"
python3 test/repl-pty.py || { echo "FAIL: tty REPL"; exit 1; }
# hb-build DEFAULT = AOT: compile MAIN to native, engine stripped (no interpreter).
printf ': FIB ( n -- n ) DUP 2 < IF EXIT THEN DUP 1 - RECURSE SWAP 2 - RECURSE + ;\n: MAIN 10 FIB . CR ;\n' > $T/hb-at.f
./tools/hb-build.sh $T/hb-at.f -o $T/hb-at >/dev/null || { echo "FAIL: hb-build (AOT)"; exit 1; }
[ "$($T/hb-at)" = "55" ] || { echo "FAIL: hb-build AOT output (got: $($T/hb-at))"; exit 1; }
ATX=$(size -m $T/hb-at 2>/dev/null | awk '/__text/{print $3}')
[ "${ATX:-99999}" -lt 2000 ] || { echo "FAIL: hb-build AOT did not strip the engine (__text=$ATX, expected <2000)"; exit 1; }
echo "PASS: hb-build AOT (engine stripped, __text $ATX B vs ~11800 embed)"
# AOT closure stress: a 260-word reachable chain (above the old 256-cell tables,
# which silently overflowed and crashed the linker). Must build and compute 260.
{ printf ': W259 ( -- n ) 1 ;\n'
  i=258; while [ $i -ge 0 ]; do printf ': W%s ( -- n ) W%s 1 + ;\n' "$i" "$((i+1))"; i=$((i-1)); done
  printf ': MAIN W0 . CR ;\n'; } > $T/hb-cl.f
./tools/hb-build.sh $T/hb-cl.f -o $T/hb-cl >/dev/null || { echo "FAIL: hb-build AOT closure stress (260 words)"; exit 1; }
[ "$($T/hb-cl)" = "260" ] || { echo "FAIL: hb-build AOT closure stress output (got: $($T/hb-cl))"; exit 1; }
echo "PASS: hb-build AOT closure stress (260 reachable words)"
# AOT S" string literal: the body is embedded in MAIN's blob and its address is
# pushed PC-relative, so it survives the blob copy + ASLR (an absolute push would
# point back into the builder's JIT region and print nothing).
printf ': MAIN s" hi" type CR ;\n' > $T/hb-str.f
./tools/hb-build.sh $T/hb-str.f -o $T/hb-str >/dev/null || { echo "FAIL: hb-build AOT S\" build"; exit 1; }
[ "$($T/hb-str)" = "hi" ] || { echo "FAIL: hb-build AOT S\" output (got: $($T/hb-str))"; exit 1; }
echo "PASS: hb-build AOT S\" string literal (PC-relative, relocation-safe)"
# hb-build default AOT must verify declared signatures and treat rejection as fatal.
printf ': BAD ( i64 -- i64 ) 0= ;\n: MAIN 0 BAD . CR ;\n' > $T/hb-badsig.f
if ./tools/hb-build.sh $T/hb-badsig.f -o $T/hb-badsig >/dev/null 2>$T/hb-badsig.err; then
  echo "FAIL: hb-build accepted bool-as-i64 false cert"; exit 1
fi
grep -q "expected: i64" $T/hb-badsig.err || { echo "FAIL: bool-as-i64 diagnostic lost expected type"; exit 1; }
grep -q "actual: bool" $T/hb-badsig.err || { echo "FAIL: bool-as-i64 diagnostic lost actual type"; exit 1; }
printf ': M ( i64 ) drop ;\n: MAIN 5 M 7 . CR ;\n' > $T/hb-malsig.f
./tools/hb-build.sh $T/hb-malsig.f -o $T/hb-malsig >/dev/null 2>&1 && { echo "FAIL: hb-build accepted malformed sig"; exit 1; }
printf ': B ( -- i64 ) 1.5 1 + ;\n: MAIN B . CR ;\n' > $T/hb-typebad.f
./tools/hb-build.sh $T/hb-typebad.f -o $T/hb-typebad >/dev/null 2>&1 && { echo "FAIL: hb-build ignored checker rejection"; exit 1; }
echo "PASS: hb-build rejects bad checked programs"
# hb-build --repl = build-time checked user source + engine/REPL bundle.
printf ': SQ ( i64 -- i64 ) DUP * ;\nEXPORT SQ\n9 SQ . CR\n' > $T/hb-rt.f
./tools/hb-build.sh --repl $T/hb-rt.f -o $T/hb-rt >/dev/null || { echo "FAIL: hb-build --repl"; exit 1; }
[ "$($T/hb-rt)" = "81" ] || { echo "FAIL: hb-build --repl output (got: $($T/hb-rt))"; exit 1; }
printf ': RBAD ( i64 -- i64 ) 0= ;\nEXPORT RBAD\n' > $T/hb-rt-bad.f
if ./tools/hb-build.sh --repl $T/hb-rt-bad.f -o $T/hb-rt-bad >/dev/null 2>$T/hb-rt-bad.err; then
  echo "FAIL: hb-build --repl accepted bool-as-i64 false cert"; exit 1
fi
grep -q "expected: i64" $T/hb-rt-bad.err || { echo "FAIL: hb-build --repl diagnostic lost expected type"; exit 1; }
grep -q "actual: bool" $T/hb-rt-bad.err || { echo "FAIL: hb-build --repl diagnostic lost actual type"; exit 1; }
echo "PASS: hb-build --repl verifies user defs ($(stat -f%z $T/hb-rt) B, engine + library)"
if [ "$1" = "full" ]; then
  ./tools/oracle.sh || exit 1
fi
echo "PASS: native gate (fixpoint + hb-suite + snapshot + repl + hb-build)${1:+ + oracle}"
