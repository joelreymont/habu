#!/bin/sh
# run.sh — the DEFAULT gate: habu-native, no gforth anywhere on the path.
#   lints -> self-rebuild fixpoint -> engine suite -> checked hb -> tty REPL ->
#   hb-build standalone.
set -e
cd "$(dirname "$0")/.."
if [ "${1:-}" = "full" ]; then
  echo "FAIL: test/run.sh full retired; the native gate is test/run.sh"
  exit 64
fi
if [ "$#" -gt 0 ]; then
  echo "usage: test/run.sh"
  exit 64
fi
CLEAN_T=0
if [ -n "${HB_TMP:-}" ]; then
  T=$HB_TMP
else
  T=$(mktemp -d "${TMPDIR:-/tmp}/hb-gate.XXXXXX")
  CLEAN_T=1
fi
mkdir -p "$T"
export HB_TMP=$T
CHECK="bin/hb --load lib/errors.f lib/string.f lib/fs.f lib/fs-mutate.f lib/process.f lib/process-argv.f lib/source.f tools/argv.f tools/check.f --"
cleanup() {
  [ "$CLEAN_T" = 0 ] || rm -rf "$T"
}
trap cleanup EXIT HUP INT TERM
bin/hb --load lib/errors.f lib/string.f lib/fs.f lib/fs-mutate.f lib/process.f lib/process-argv.f lib/process-env.f lib/test-runner.f test/gate-stdlib.f || { echo "FAIL: native lint/stdlib gate phase"; exit 1; }
bin/hb --load lib/errors.f lib/string.f lib/fs.f lib/fs-mutate.f lib/process.f lib/process-argv.f lib/process-env.f lib/test-runner.f test/gate-common.f lib/build.f tools/build-fixpoint.f test/gate-engine.f || { echo "FAIL: native engine gate phase"; exit 1; }
bin/hb --load lib/errors.f lib/string.f lib/fs.f lib/fs-mutate.f lib/process.f lib/process-argv.f lib/process-env.f lib/test-runner.f test/gate-common.f test/gate-dictionary.f || { echo "FAIL: native dictionary/checker gate phase"; exit 1; }
[ -x bin/hb ] || { echo "FAIL: bin/hb not produced"; exit 1; }
bin/hb --load lib/errors.f lib/string.f lib/fs.f lib/fs-mutate.f lib/process.f lib/process-argv.f lib/process-env.f lib/test-runner.f test/gate-common.f test/gate-diagnostics.f || { echo "FAIL: native checker diagnostics gate phase"; exit 1; }
bin/hb --load lib/errors.f lib/string.f lib/fs.f lib/fs-mutate.f lib/process.f lib/process-argv.f lib/process-env.f lib/test-runner.f test/gate-common.f lib/codesign.f test/gate-debug.f || { echo "FAIL: native prop/snapshot/debug gate phase"; exit 1; }
bin/hb --load lib/errors.f lib/string.f lib/fs.f lib/fs-mutate.f lib/process.f lib/process-argv.f lib/process-env.f lib/test-runner.f test/gate-common.f test/gate-build-common.f test/gate-aot-positive.f || { echo "FAIL: native hb-build AOT positive gate phase"; exit 1; }
# build verification requires certification; strict signature mode catches
# missing signatures as source-lint errors before the maker runs.
printf ': NOSIG 42 . CR ;\n' > $T/hb-nosig.f
./tools/hb-build.sh --strict-signatures $T/hb-nosig.f -o $T/hb-nosig >$T/hb-nosig.err 2>&1 && { echo "FAIL: hb-build --strict-signatures accepted nosig"; exit 1; }
grep -q 'E-MISSING-SIGNATURE' $T/hb-nosig.err || { echo "FAIL: hb-build --strict-signatures missing diagnostic"; exit 1; }
printf ': MAIN ( -- ) 42 . CR ;\n' > $T/hb-strict-ok.f
./tools/hb-build.sh --strict-signatures $T/hb-strict-ok.f -o $T/hb-strict-ok >/dev/null || { echo "FAIL: hb-build --strict-signatures good build"; exit 1; }
[ "$($T/hb-strict-ok)" = "42" ] || { echo "FAIL: hb-build --strict-signatures output (got: $($T/hb-strict-ok))"; exit 1; }
# CHECK! verdict 1 means uncheckable, not certified; build hooks must reject it.
printf ': U ( -- ) [: leave ;] drop ;\n: MAIN ( -- ) U ;\n' > $T/hb-uncheckable.f
./tools/hb-build.sh $T/hb-uncheckable.f -o $T/hb-uncheckable >/dev/null 2>$T/hb-uncheckable.err && { echo "FAIL: hb-build accepted uncheckable CHECK! verdict"; exit 1; }
grep -q 'check did not certify' $T/hb-uncheckable.err || { echo "FAIL: hb-build uncheckable diagnostic missing"; exit 1; }
# stripped AOT has no persistent data region. Reject data-space primitives
# statically before/while linking instead of emitting a runtime-crashing binary.
printf ': MAIN ( -- ) here . CR ;\n' > $T/hb-aot-unsafe.f
./tools/hb-build.sh --json-errors $T/hb-aot-unsafe.f -o $T/hb-aot-unsafe >/dev/null 2>$T/hb-aot-unsafe.err && { echo "FAIL: hb-build AOT accepted here"; exit 1; }
grep -q '"code":"E-AOT-UNSUPPORTED"' $T/hb-aot-unsafe.err || { echo "FAIL: hb-build AOT unsafe missing JSON code"; exit 1; }
grep -q '"schema_version":1' $T/hb-aot-unsafe.err || { echo "FAIL: hb-build AOT unsafe missing schema version"; exit 1; }
grep -q '"token":"here"' $T/hb-aot-unsafe.err || { echo "FAIL: hb-build AOT unsafe missing token"; exit 1; }
grep -q '"word":"MAIN"' $T/hb-aot-unsafe.err || { echo "FAIL: hb-build AOT unsafe missing word"; exit 1; }
grep -q '"reason":"stripped AOT has no persistent data region"' $T/hb-aot-unsafe.err || { echo "FAIL: hb-build AOT unsafe missing reason"; exit 1; }
grep -q '"byte_end":' $T/hb-aot-unsafe.err || { echo "FAIL: hb-build AOT unsafe missing byte_end"; exit 1; }
printf ': LONG-AOT-UNSAFE-CALLER-WORD ( -- ) here drop ;\n: MAIN ( -- ) LONG-AOT-UNSAFE-CALLER-WORD ;\n' > $T/hb-aot-long-unsafe.f
./tools/hb-build.sh --json-errors $T/hb-aot-long-unsafe.f -o $T/hb-aot-long-unsafe >/dev/null 2>$T/hb-aot-long-unsafe.err && { echo "FAIL: hb-build AOT accepted long unsafe"; exit 1; }
grep -q '"word":"LONG-AOT-UNSAFE-CALLER-WORD"' $T/hb-aot-long-unsafe.err || { echo "FAIL: hb-build AOT unsafe lost long caller"; exit 1; }
{ printf '8 CLO-LIMIT!\n'
  printf ': W8 ( n -- n ) dup 0< if negate then ;\n'
  i=7; while [ $i -ge 0 ]; do printf ': W%s ( n -- n ) W%s dup 0< if negate then ;\n' "$i" "$((i+1))"; i=$((i-1)); done
  printf ': MAIN ( -- ) 1 W0 drop ;\n'; } > $T/hb-clo-limit.f
./tools/hb-build.sh --json-errors $T/hb-clo-limit.f -o $T/hb-clo-limit >/dev/null 2>$T/hb-clo-limit.err && { echo "FAIL: hb-build accepted closure over MAX-CLO"; exit 1; }
grep -q '"code":"E-AOT-CLOSURE-LIMIT"' $T/hb-clo-limit.err || { echo "FAIL: hb-build closure limit missing JSON code"; exit 1; }
grep -q '"schema_version":1' $T/hb-clo-limit.err || { echo "FAIL: hb-build closure limit missing schema version"; exit 1; }
grep -q '"reachable_count":8' $T/hb-clo-limit.err || { echo "FAIL: hb-build closure limit missing reachable_count"; exit 1; }
grep -q '"max_closure":8' $T/hb-clo-limit.err || { echo "FAIL: hb-build closure limit missing max_closure"; exit 1; }
grep -q '"root_word":"MAIN"' $T/hb-clo-limit.err || { echo "FAIL: hb-build closure limit missing root_word"; exit 1; }
bin/hb --load tools/json.f tools/gate-json-assert.f -- json-one-schema "$T/hb-clo-limit.err"
echo "PASS: hb-build strict signatures + uncheckable/AOT-unsafe rejection"
# hb-build default AOT must verify declared signatures and treat rejection as fatal.
printf ': BAD ( i64 -- i64 ) 0= ;\n: MAIN ( -- ) 0 BAD . CR ;\n' > $T/hb-badsig.f
if ./tools/hb-build.sh $T/hb-badsig.f -o $T/hb-badsig >/dev/null 2>$T/hb-badsig.err; then
  echo "FAIL: hb-build accepted bool-as-i64 false cert"; exit 1
fi
grep -q "expected: i64" $T/hb-badsig.err || { echo "FAIL: bool-as-i64 diagnostic lost expected type"; exit 1; }
grep -q "actual: bool" $T/hb-badsig.err || { echo "FAIL: bool-as-i64 diagnostic lost actual type"; exit 1; }
printf ': M ( i64 ) drop ;\n: MAIN ( -- ) 5 M 7 . CR ;\n' > $T/hb-malsig.f
./tools/hb-build.sh $T/hb-malsig.f -o $T/hb-malsig >/dev/null 2>&1 && { echo "FAIL: hb-build accepted malformed sig"; exit 1; }
printf ': B ( -- i64 ) 1.5 1 + ;\n: MAIN ( -- ) B . CR ;\n' > $T/hb-typebad.f
./tools/hb-build.sh $T/hb-typebad.f -o $T/hb-typebad >/dev/null 2>&1 && { echo "FAIL: hb-build ignored checker rejection"; exit 1; }
echo "PASS: hb-build rejects bad checked programs"
# hb-build --repl = build-time checked user source + engine/REPL bundle.
printf ': SQ ( i64 -- i64 ) DUP * ;\nEXPORT SQ\n9 SQ . CR\n' > $T/hb-rt.f
./tools/hb-build.sh --repl $T/hb-rt.f -o $T/hb-rt >/dev/null || { echo "FAIL: hb-build --repl"; exit 1; }
[ "$($T/hb-rt)" = "81" ] || { echo "FAIL: hb-build --repl output (got: $($T/hb-rt))"; exit 1; }
bin/hb tools/imgdump.f $T/hb-rt > $T/hb-rt-dict || { echo "FAIL: imgdump generated engine"; exit 1; }
grep -q '^+ ' $T/hb-rt-dict || { echo "FAIL: imgdump missing seed dict"; exit 1; }
printf ': RBAD ( i64 -- i64 ) 0= ;\nEXPORT RBAD\n' > $T/hb-rt-bad.f
if ./tools/hb-build.sh --repl $T/hb-rt-bad.f -o $T/hb-rt-bad >/dev/null 2>$T/hb-rt-bad.err; then
  echo "FAIL: hb-build --repl accepted bool-as-i64 false cert"; exit 1
fi
grep -q "expected: i64" $T/hb-rt-bad.err || { echo "FAIL: hb-build --repl diagnostic lost expected type"; exit 1; }
grep -q "actual: bool" $T/hb-rt-bad.err || { echo "FAIL: hb-build --repl diagnostic lost actual type"; exit 1; }
echo "PASS: hb-build --repl verifies user defs ($(stat -f%z $T/hb-rt) B, engine + library)"
echo "PASS: native gate (fixpoint + engine suite + checked hb + repl + hb-build)"
