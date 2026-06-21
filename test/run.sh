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
# property-based soundness smoke, SELF-HOSTED in habu: generate typed defs,
# check them, and RUN the certified ones IN-PROCESS (via `evaluate`); a false-cert
# (real out-arity != declared) calls `die` -> nonzero exit. Default seed/count
# are reproducible; argv can override longer sweeps. No host scripting, no gforth, no
# spawning. See PROP-TESTING.md.
bin/hb < test/prop-test.f > $T/prop.out 2>/dev/null || { echo "FAIL: prop-test (self-hosted) found a FALSE-CERT"; exit 1; }
grep -q "self-test OK" $T/prop.out || { echo "FAIL: prop-test self-test/run did not complete"; exit 1; }
echo "PASS: prop-test soundness smoke (self-hosted in habu, in-process via evaluate)"
HT=$(mktemp -d)
HB_TMP=$HT ./tools/snap-hb.sh >/dev/null || { echo "FAIL: HB_TMP isolation"; exit 1; }
out=$(printf '$340000000 $1B0 + @ 0= .\n: SQOK ( i64 -- i64 ) dup * ;\n7 SQOK .\n' | bin/hb 2>/dev/null)
[ "$out" = "0
49" ] || { echo "FAIL: HB_TMP hb refresh/check hook (got: $out)"; exit 1; }
{ printf ': LONG-SNAPSHOT-DICTIONARY-WORD ( i64 -- i64 ) 3 + ;\n'
  cat src/habu/snap.f; } > $T/hb-snap-long.f
HB_TMP=$HT bin/hb < $T/hb-snap-long.f >/dev/null || { echo "FAIL: long-name snapshot write"; exit 1; }
codesign -s - --force "$HT/hb-snap0" 2>/dev/null
chmod +x "$HT/hb-snap0"
out=$(printf '39 LONG-SNAPSHOT-DICTIONARY-WORD .\n' | "$HT/hb-snap0" 2>/dev/null)
[ "$out" = "42" ] || { echo "FAIL: long-name snapshot restore (got: $out)"; exit 1; }
rm -rf "$HT"
echo "PASS: HB_TMP isolation"
bin/hb --load lib/errors.f lib/process.f test/proc-pty.f || { echo "FAIL: process/pty"; exit 1; }
out=$(printf ': LONG-PROFILER-BUSY-WORD ( -- ) 80000000 begin 1- dup dup * drop dup 0= until drop ;\n: GO ( -- ) 100000 prof-on LONG-PROFILER-BUSY-WORD prof-report ;\nGO\n' | bin/hb 2>/dev/null | head -1)
case "$out" in
  "LONG-PROFILER-BUSY-WORD "*) ;;
  *) echo "FAIL: profiler long-name output (got: $out)"; exit 1 ;;
esac
echo "PASS: profiler long dictionary names"
out=$(bin/hb --load src/arch/arm64/disasm.f tools/jitdump.f -- ': JITDUMP-SMOKE ( -- i64 ) 7 ;' JITDUMP-SMOKE 2>/dev/null)
case "$out" in
  *ret*) ;;
  *) echo "FAIL: jitdump direct CLI output (got: $out)"; exit 1 ;;
esac
echo "PASS: jitdump direct CLI"
# hb-build DEFAULT = AOT: compile MAIN to native, engine stripped (no interpreter).
GATE_JSON=$T/gate-json-assert.f
cat tools/json.f tools/gate-json-assert.f > "$GATE_JSON"
printf ': FIB ( n -- n ) DUP 2 < IF EXIT THEN DUP 1 - RECURSE SWAP 2 - RECURSE + ;\n: MAIN ( -- ) 10 FIB . CR ;\n' > $T/hb-at.f
./tools/hb-build.sh $T/hb-at.f -o $T/hb-at >/dev/null || { echo "FAIL: hb-build (AOT)"; exit 1; }
[ "$($T/hb-at)" = "55" ] || { echo "FAIL: hb-build AOT output (got: $($T/hb-at))"; exit 1; }
ATX=$(size -m $T/hb-at 2>/dev/null | awk '/__text/{print $3}')
[ "${ATX:-99999}" -lt 2000 ] || { echo "FAIL: hb-build AOT did not strip the engine (__text=$ATX, expected <2000)"; exit 1; }
bin/hb tools/aot-call-report.f $T/hb-at < /dev/null > $T/hb-at-call-report.json
bin/hb "$GATE_JSON" aot-stripped "$T/hb-at-call-report.json"
echo "PASS: hb-build AOT (engine stripped, __text $ATX B vs ~11800 embed)"
# AOT compacts reachable call stencils inside blobs that also contain branches
# and embedded S" bodies. The linker rewrites B/CBZ/ADR through an old->new byte
# map, so there must be no leftover NOP,NOP,NOP,BL padding.
printf ': BIG ( i64 -- i64 ) 1+ 1+ 1+ 1+ 1+ 1+ 1+ 1+ 1+ 1+ 1+ 1+ 1+ 1+ 1+ 1+ 1+ 1+ 1+ 1+ ;\n: WRAP ( i64 -- i64 ) DUP 0< IF NEGATE ELSE BIG THEN 1+ ;\n: MAIN ( -- ) 1 WRAP . s" ok" type CR ;\n' > $T/hb-compact.f
./tools/hb-build.sh $T/hb-compact.f -o $T/hb-compact >/dev/null || { echo "FAIL: hb-build AOT compact calls"; exit 1; }
[ "$($T/hb-compact)" = "22
ok" ] || { echo "FAIL: hb-build AOT compact call output (got: $($T/hb-compact))"; exit 1; }
bin/hb tools/aot-call-report.f $T/hb-compact < /dev/null > $T/hb-compact-call-report.json
bin/hb "$GATE_JSON" aot-compact "$T/hb-compact-call-report.json"
echo "PASS: hb-build AOT compact call layout"
# AOT closure stress: a 260-word reachable chain (above the old 256-cell tables,
# which silently overflowed and crashed the linker). Must build and compute 260.
{ printf ': W259 ( -- n ) 1 ;\n'
  i=258; while [ $i -ge 0 ]; do printf ': W%s ( -- n ) W%s 1 + ;\n' "$i" "$((i+1))"; i=$((i-1)); done
  printf ': MAIN ( -- ) W0 . CR ;\n'; } > $T/hb-cl.f
./tools/hb-build.sh $T/hb-cl.f -o $T/hb-cl >/dev/null || { echo "FAIL: hb-build AOT closure stress (260 words)"; exit 1; }
[ "$($T/hb-cl)" = "260" ] || { echo "FAIL: hb-build AOT closure stress output (got: $($T/hb-cl))"; exit 1; }
echo "PASS: hb-build AOT closure stress (260 reachable words)"
printf ': LONG-AOT-CALLED-WORD-NAME ( -- n ) 34 ;\n: MAIN ( -- ) LONG-AOT-CALLED-WORD-NAME . CR ;\n' > $T/hb-aot-long.f
./tools/hb-build.sh $T/hb-aot-long.f -o $T/hb-aot-long >/dev/null || { echo "FAIL: hb-build AOT long names"; exit 1; }
[ "$($T/hb-aot-long)" = "34" ] || { echo "FAIL: hb-build AOT long-name output (got: $($T/hb-aot-long))"; exit 1; }
echo "PASS: hb-build AOT long dictionary names"
# AOT S" string literal: the body is embedded in MAIN's blob and its address is
# pushed PC-relative, so it survives the blob copy + ASLR (an absolute push would
# point back into the builder's JIT region and print nothing).
printf ': MAIN ( -- ) s" hi" type CR ;\n' > $T/hb-str.f
./tools/hb-build.sh $T/hb-str.f -o $T/hb-str >/dev/null || { echo "FAIL: hb-build AOT S\" build"; exit 1; }
[ "$($T/hb-str)" = "hi" ] || { echo "FAIL: hb-build AOT S\" output (got: $($T/hb-str))"; exit 1; }
echo "PASS: hb-build AOT S\" string literal (PC-relative, relocation-safe)"
printf ': MAIN ( -- ) ." hi" CR c" ok" count type CR ;\n' > $T/hb-parse.f
./tools/hb-build.sh $T/hb-parse.f -o $T/hb-parse >/dev/null || { echo "FAIL: hb-build AOT parsing words"; exit 1; }
[ "$($T/hb-parse)" = "hi
ok" ] || { echo "FAIL: hb-build AOT parsing-word output (got: $($T/hb-parse))"; exit 1; }
echo "PASS: hb-build AOT .\"/C\" parsing words"
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
bin/hb "$GATE_JSON" json-one-schema "$T/hb-clo-limit.err"
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
