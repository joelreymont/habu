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
cleanup() {
  [ "$CLEAN_T" = 0 ] || rm -rf "$T"
}
trap cleanup EXIT HUP INT TERM
cat tools/lint/lib.f tools/lint/shadow-lint.f | bin/hb || { echo "FAIL: shadow-lint"; exit 1; }
cat tools/lint/lib.f tools/lint/clobber-lint.f | bin/hb || { echo "FAIL: clobber-lint"; exit 1; }
cat tools/lint/lib.f tools/repl-lint.f | bin/hb || { echo "FAIL: repl-lint"; exit 1; }
cat tools/date.f tools/lint/lib.f tools/fs.f tools/trust-lint.f | bin/hb || { echo "FAIL: trust-lint"; exit 1; }
./tools/trust-lint-test.sh || { echo "FAIL: trust-lint fixtures"; exit 1; }
cat tools/date.f tools/lint/lib.f tools/fs.f tools/stale-status-lint.f | bin/hb || { echo "FAIL: stale-status-lint"; exit 1; }
cat tools/lint/lib.f tools/fs.f tools/host-lint.f | bin/hb || { echo "FAIL: host-lint"; exit 1; }
cat tools/lint/lib.f tools/parallel-agent-lint.f | bin/hb || { echo "FAIL: parallel-agent-lint"; exit 1; }
cat tools/lint/lib.f tools/filemap-lint.f | bin/hb || { echo "FAIL: filemap-lint"; exit 1; }
./tools/checked-boundary-lint-test.sh || { echo "FAIL: checked-boundary-lint"; exit 1; }
./tools/string-test.sh || { echo "FAIL: string helpers"; exit 1; }
./tools/array-test.sh || { echo "FAIL: array helpers"; exit 1; }
cat lib/errors.f lib/test.f lib/array.f lib/table.f lib/table-test.f | bin/hb || { echo "FAIL: table stdlib"; exit 1; }
./lib/regex-test.sh || { echo "FAIL: regex stdlib"; exit 1; }
./lib/map-test.sh || { echo "FAIL: map stdlib"; exit 1; }
./lib/fs-test.sh || { echo "FAIL: fs stdlib"; exit 1; }
./lib/process-test.sh || { echo "FAIL: process stdlib"; exit 1; }
./lib/argv-test.sh || { echo "FAIL: argv stdlib"; exit 1; }
./lib/test-test.sh || { echo "FAIL: test stdlib"; exit 1; }
./lib/property-test.sh || { echo "FAIL: property stdlib"; exit 1; }
./lib/build-test.sh || { echo "FAIL: build stdlib"; exit 1; }
./tools/date-test.sh || { echo "FAIL: date helpers"; exit 1; }
./tools/bundle-lib-test.sh || { echo "FAIL: stdlib bundle wrapper"; exit 1; }
./tools/build-fixpoint-test.sh || { echo "FAIL: build fixpoint driver"; exit 1; }
./tools/repair-schema-doc-test.sh || { echo "FAIL: repair diagnostic schema doc"; exit 1; }
./tools/repair-packet-test.sh || { echo "FAIL: repair packet tool"; exit 1; }
[ -x bin/hb ] || { echo "no bin/hb — install a trusted seed with tools/seed.sh /path/to/hb"; exit 1; }
./tools/build.sh > $T/hb-build.log 2>&1 || { tail -5 $T/hb-build.log; echo "FAIL: build (fixpoint)"; exit 1; }
echo "PASS: self-rebuild fixpoint"
./lib/fs-mutate-test.sh || { echo "FAIL: fs mutation stdlib"; exit 1; }
./lib/process-argv-test.sh || { echo "FAIL: process argv stdlib"; exit 1; }
./tools/check-repair-hints-test.sh || { echo "FAIL: repair diagnostic hints"; exit 1; }
./tools/hb-baseline-contracts-test.sh || { echo "FAIL: hb baseline contracts"; exit 1; }
./tools/test-hb.sh || exit 1
# divide/modulo by zero must fail loudly (ARM64 SDIV yields 0 silently); the
# engine traps, so the run exits nonzero instead of printing a bogus result.
printf '1 0 / .\n'   | bin/hb >/dev/null 2>&1 && { echo "FAIL: 1 0 / did not trap"; exit 1; }
printf '1 0 mod .\n' | bin/hb >/dev/null 2>&1 && { echo "FAIL: 1 0 mod did not trap"; exit 1; }
printf '7 2 / . 7 2 mod . cr\n' | bin/hb 2>/dev/null | tr -d '\n ' | grep -q '^31$' || { echo "FAIL: nonzero div/mod regressed"; exit 1; }
echo "PASS: div/mod by zero traps (no silent 0)"
out=$(echo 's" w" s" n -- n" trust 7 . : Q 5 dup * . ; Q' | bin/hb)
[ "$out" = "7
25" ] || { echo "FAIL: checked hb trust/run smoke (got: $out)"; exit 1; }
out=$(echo 's" HOME" getenv nip 0 > .' | bin/hb)
[ "$out" = "-1" ] || { echo "FAIL: getenv (got: $out)"; exit 1; }
cat > $T/hb-script-argv.f <<'EOF'
SCRIPT-ARGC .
0 SCRIPT-ARGV$ type cr
1 SCRIPT-ARGV$ type cr
EOF
out=$(bin/hb $T/hb-script-argv.f alpha beta)
[ "$out" = "2
alpha
beta" ] || { echo "FAIL: hb script argv mode (got: $out)"; exit 1; }
out=$(printf 'ARGC .\n1 ARGV$ type cr\n2 ARGV$ type cr\n' | bin/hb alpha beta)
[ "$out" = "3
alpha
beta" ] || { echo "FAIL: hb pipeline argv mode (got: $out)"; exit 1; }
set +e
bin/hb $T/no-such-hb-script.f >/dev/null 2>&1
rc=$?
set -e
[ "$rc" -eq 74 ] || { echo "FAIL: hb missing script rc $rc (want 74)"; exit 1; }
# hb is checked-Forth: a typed def whose body violates its sig
# is rejected (unpublished -> calling it exits 70); a correct one runs.
out=$(printf ': SQOK ( i64 -- i64 ) dup * ;\n7 SQOK .\n' | bin/hb 2>/dev/null)
[ "$out" = "49" ] || { echo "FAIL: hb good typed def (got: $out)"; exit 1; }
printf ': SQBAD ( i64 -- i64 ) dup ;\n7 SQBAD .\n' | bin/hb >/dev/null 2>&1 && { echo "FAIL: hb did NOT reject bad sig"; exit 1; }
# depth ( -- n ): certifies (else QDEPTH unpublished -> empty out) and reads the
# real data-stack cell count (0 when empty) — sentinel-free arity measurement.
out=$(printf ': QDEPTH ( -- n ) depth ;\nQDEPTH .\n' | bin/hb 2>/dev/null)
[ "$out" = "0" ] || { echo "FAIL: hb depth prim certify+run (got: $out)"; exit 1; }
out=$(printf 'TRUSTED: TLEAK ( n -- n ) dup ;\ns" TUSE ( n -- n ) TLEAK" CHECK! .\ns" TBAD ( n -- n n ) TLEAK" CHECK! .\n5 TLEAK . .\n' | bin/hb 2>/dev/null)
[ "$out" = "-1
0
5
5" ] || { echo "FAIL: hb TRUSTED: effect recording (got: $out)"; exit 1; }
out=$(printf ': LONG-DICTIONARY-NAME-ADDONE ( i64 -- i64 ) 1 + ;\n41 LONG-DICTIONARY-NAME-ADDONE .\n123 constant LONG-DICTIONARY-CONSTANT\nLONG-DICTIONARY-CONSTANT .\nvariable LONG-DICTIONARY-VARIABLE\n77 LONG-DICTIONARY-VARIABLE !\nLONG-DICTIONARY-VARIABLE @ .\ns" LONG-DICTIONARY-NAME-ADDONE" get-current search-wl 0= .\ns" long-dictionary-name-addone" get-current search-wl 0= .\n: LONG-REDEFINE-NAME ( -- i64 ) 1 ;\n: LONG-REDEFINE-NAME ( -- i64 ) 2 ;\nLONG-REDEFINE-NAME .\nTRUSTED: LONG-DICTIONARY-TRUSTED ( n -- n ) dup ;\ns" USE ( n -- n ) LONG-DICTIONARY-TRUSTED" CHECK! .\ns" BAD ( n -- n n ) LONG-DICTIONARY-TRUSTED" CHECK! .\n9 LONG-DICTIONARY-TRUSTED . .\n' | bin/hb 2>/dev/null)
[ "$out" = "42
123
77
0
0
2
-1
0
9
9" ] || { echo "FAIL: hb long dictionary names (got: $out)"; exit 1; }
out=$(printf 'wordlist constant LONG-WL\nLONG-WL set-current\n: LONG-WORDLIST-ONLY-NAME ( -- i64 ) 8 ;\n0 set-current\ns" LONG-WORDLIST-ONLY-NAME" 0 search-wl 0= .\ns" LONG-WORDLIST-ONLY-NAME" LONG-WL search-wl 0= .\n' | bin/hb 2>/dev/null)
[ "$out" = "-1
0" ] || { echo "FAIL: hb long dictionary wordlist isolation (got: $out)"; exit 1; }
long_name=$(printf '%600s' '' | tr ' ' A)
out=$(printf ': %s ( -- n ) 1 ;\n%s .\ns" %s" get-current search-wl 0= .\n' "$long_name" "$long_name" "$long_name" | bin/hb 2>/dev/null)
[ "$out" = "1
0" ] || { echo "FAIL: hb dictionary name over 255 bytes (got: $out)"; exit 1; }
out=$(printf 'TRUSTED: ARR ( n -- ) CREATES ( n -- ptr a ) create cells allot does> swap 0 ?do cell+ loop ;\n4 ARR A4\ns" USE ( n -- ptr a ) A4" CHECK! .\n7 2 A4 !\n2 A4 @ .\n' | bin/hb 2>/dev/null)
[ "$out" = "-1
7" ] || { echo "FAIL: hb trusted CREATE...DOES> effect recording (got: $out)"; exit 1; }
set +e
printf 'TRUSTED: BADARR ( n -- ) CREATES ( n -- ptr a ) create cells allot does> drop ;\n' | bin/hb >$T/habu-bad-does.out 2>$T/habu-bad-does.err
rc=$?
set -e
[ "$rc" -eq 70 ] || { echo "FAIL: hb bad trusted DOES> rc $rc (want 70)"; exit 1; }
grep -q 'does>' $T/habu-bad-does.err || { echo "FAIL: hb bad trusted DOES> missing diagnostic"; exit 1; }
set +e
printf 'TRUSTED: BADDEF ( n -- ) create cells allot does> drop ;\n' | bin/hb >$T/habu-missing-creates.out 2>$T/habu-missing-creates.err
rc=$?
set -e
[ "$rc" -eq 70 ] || { echo "FAIL: hb trusted DOES> without CREATES rc $rc (want 70)"; exit 1; }
grep -q 'does>' $T/habu-missing-creates.err || { echo "FAIL: hb trusted DOES> without CREATES missing diagnostic"; exit 1; }
# named rows + quot sub-sigs VERIFY (Gap3): CHECK! body-vs-declared-sig.
# V1 row-poly certifies, V2 combinator-param certifies, V3 bad row count rejects.
out=$(printf 's" V1 ( R -- R i64 ) 5" CHECK! .\ns" V2 ( i64 [ i64 -- i64 ] -- i64 ) execute" CHECK! .\ns" V3 ( R -- R i64 ) 5 5" CHECK! .\n' | bin/hb 2>/dev/null)
[ "$out" = "-1
-1
0" ] || { echo "FAIL: hb rows/quot sig verify (got: $out)"; exit 1; }
out=$(printf 's" P1 ( i64 i64 i64 i64 -- i64 i64 i64 i64 i64 i64 ) 2over" CHECK! .\ns" P2 ( i64 i64 -- i64 i64 ) 2>r 2r>" CHECK! .\ns" P3 ( i64 -- i64 ) abs" CHECK! .\ns" P4 ( i64 i64 -- i64 i64 ) /mod" CHECK! .\ns" P5 ( ptr u8 -- ptr u8 i64 ) count" CHECK! .\ns" P6 ( i64 i64 -- i64 i64 i64 ) depth" CHECK! .\ns" P7 ( -- n ) 0 4096 3 $1002 -1 0 mmap" CHECK! .\n' | bin/hb 2>/dev/null)
[ "$out" = "-1
-1
-1
-1
-1
-1
-1" ] || { echo "FAIL: hb primitive checklist signatures (got: $out)"; exit 1; }
out=$(printf 's" RBAD1 ( i64 i64 -- ) 2>r" CHECK! .\ns" RBAD2 ( -- i64 i64 ) 2r>" CHECK! .\ns" RPEEK ( i64 i64 -- i64 i64 i64 i64 ) 2>r 2r@ 2r>" CHECK! .\ns" QD ( i64 -- i64 i64 ) ?dup" CHECK! .\n' | bin/hb 2>/dev/null)
[ "$out" = "0
0
-1
1" ] || { echo "FAIL: hb return-stack/?dup primitive verdicts (got: $out)"; exit 1; }
out=$(printf 's" CDIP ( i64 i64 -- i64 i64 ) [: 1+ ;] DIP" CHECK! .\ns" CKEEP ( i64 -- i64 i64 ) [: 1+ ;] KEEP" CHECK! .\ns" CBI ( i64 -- i64 i64 ) [: 1+ ;] [: 2 * ;] BI" CHECK! .\ns" CTRI ( i64 -- i64 i64 i64 ) [: 1+ ;] [: 2 * ;] [: 3 + ;] TRI" CHECK! .\ns" CTIMES ( i64 -- i64 ) 5 [: 1+ ;] TIMES" CHECK! .\ns" CEACH ( i64 ptr i64 i64 -- i64 ) [: + ;] EACH" CHECK! .\ns" CMAP ( ptr i64 i64 -- ) [: 1+ ;] MAP" CHECK! .\ns" CFOLD ( ptr i64 i64 i64 -- i64 ) [: + ;] FOLD" CHECK! .\n' | bin/hb 2>/dev/null)
[ "$out" = "-1
-1
-1
-1
-1
-1
-1
-1" ] || { echo "FAIL: hb combinator/iterator verdicts (got: $out)"; exit 1; }
out=$(printf '." hi" cr\nc" ok" count type cr\n: DQ ( -- ) ." bye" ;\nDQ cr\n: CQ ( -- ptr u8 n ) c" yo" count ;\nCQ type cr\n' | bin/hb 2>/dev/null)
[ "$out" = "hi
ok
bye
yo" ] || { echo "FAIL: hb parsing-word runtime surface (got: $out)"; exit 1; }
printf ': DQ ( -- ) ." ok" ;\n: CQ ( -- ptr u8 n ) c" ok" count ;\n' | ./tools/check.sh || { echo "FAIL: check.sh parsing-word certification"; exit 1; }
set +e
printf '$400000 allot\n' | bin/hb >/dev/null 2>&1
rc=$?
set -e
[ "$rc" -eq 76 ] || { echo "FAIL: data-space overflow rc $rc (want 76)"; exit 1; }
# and the row sig runs end to end
out=$(printf ': PSH ( R -- R i64 ) 5 ;\nPSH .\n' | bin/hb 2>/dev/null)
[ "$out" = "5" ] || { echo "FAIL: hb named-row sig run (got: $out)"; exit 1; }
[ -x bin/hb ] || { echo "FAIL: bin/hb not produced"; exit 1; }
GATE_JSON=$T/gate-json-assert.f
cat tools/json.f tools/gate-json-assert.f > "$GATE_JSON"
printf ': JBAD ( i64 -- i64 ) dup ;\n' | ./tools/check.sh --json-errors >/dev/null 2>$T/habu-json.err && { echo "FAIL: tools/check.sh --json-errors accepted bad def"; exit 1; }
bin/hb "$GATE_JSON" json-lines-schema "$T/habu-json.err"
bin/hb "$GATE_JSON" diag-repair-class "$T/habu-json.err" remove_producer
grep -q '"verdict":"rejected"' $T/habu-json.err || { echo "FAIL: --json-errors missing verdict"; exit 1; }
grep -q '"declared_effect":"i64 -- i64 ' $T/habu-json.err || { echo "FAIL: --json-errors missing declared effect"; exit 1; }
grep -q '"inferred_effect":"i64 -- i64 i64 ' $T/habu-json.err || { echo "FAIL: --json-errors missing inferred effect"; exit 1; }
grep -q '"token_index":1' $T/habu-json.err || { echo "FAIL: --json-errors missing token index"; exit 1; }
grep -q '"file":"<stdin>"' $T/habu-json.err || { echo "FAIL: --json-errors missing file"; exit 1; }
grep -q '"line":1' $T/habu-json.err || { echo "FAIL: --json-errors missing line"; exit 1; }
grep -q '"column":' $T/habu-json.err || { echo "FAIL: --json-errors missing column"; exit 1; }
grep -q '"byte_start":' $T/habu-json.err || { echo "FAIL: --json-errors missing byte_start"; exit 1; }
grep -q '"byte_end":' $T/habu-json.err || { echo "FAIL: --json-errors missing byte_end"; exit 1; }
grep -q '"definition_source":' $T/habu-json.err || { echo "FAIL: --json-errors missing definition source"; exit 1; }
printf ': JMISS ( i64 -- i64 ) drop ;\n' | ./tools/check.sh --json-errors >/dev/null 2>$T/habu-json-miss.err && { echo "FAIL: tools/check.sh --json-errors accepted missing producer"; exit 1; }
bin/hb "$GATE_JSON" diag-repair-class "$T/habu-json-miss.err" add_producer
printf ': JTYPE ( i64 -- i64 ) 0= ;\n' | ./tools/check.sh --json-errors >/dev/null 2>$T/habu-json-type.err && { echo "FAIL: tools/check.sh --json-errors accepted type mismatch"; exit 1; }
bin/hb "$GATE_JSON" diag-repair-class "$T/habu-json-type.err" fix_type
printf ': JRET ( i64 -- ) >r ;\n' | ./tools/check.sh --json-errors >/dev/null 2>$T/habu-json-ret.err && { echo "FAIL: tools/check.sh --json-errors accepted return-stack imbalance"; exit 1; }
bin/hb "$GATE_JSON" diag-repair-class "$T/habu-json-ret.err" fix_return_stack
cat > $T/habu-json-file.f <<'EOF'
\ prelude

: JBAD ( i64 -- i64 ) dup ;
EOF
./tools/check.sh --json-errors $T/habu-json-file.f >/dev/null 2>$T/habu-json-file.err && { echo "FAIL: tools/check.sh --json-errors accepted file bad def"; exit 1; }
bin/hb "$GATE_JSON" diag-file-origin "$T/habu-json-file.err" "$T/habu-json-file.f"
printf ': NOSIG dup ;\n' | ./tools/check.sh --strict-signatures >$T/habu-strict.err 2>&1 && { echo "FAIL: tools/check.sh --strict-signatures accepted nosig"; exit 1; }
grep -q 'E-MISSING-SIGNATURE' $T/habu-strict.err || { echo "FAIL: strict-signatures missing text diagnostic"; exit 1; }
printf ': NOSIG dup ;\n' | ./tools/check.sh --strict-signatures --json-errors >$T/habu-strict-json.out 2>&1 && { echo "FAIL: tools/check.sh --strict-signatures --json-errors accepted nosig"; exit 1; }
grep -q '"code":"E-MISSING-SIGNATURE"' $T/habu-strict-json.out || { echo "FAIL: strict-signatures missing JSON diagnostic"; exit 1; }
printf ': X ( infer ) dup ;\n' | ./tools/check.sh --strict-signatures --json-errors >$T/habu-strict-infer.out 2>&1 && { echo "FAIL: tools/check.sh --strict-signatures accepted infer opt-out"; exit 1; }
grep -q '"code":"E-UNVERIFIED-SIGNATURE"' $T/habu-strict-infer.out || { echo "FAIL: strict-signatures missing opt-out diagnostic"; exit 1; }
out=$(printf 's" EV ( -- n ) evaluate" CHECK! .\ns" PO ( -- ) postpone dup" CHECK! .\ns" CO ( -- ) compile," CHECK! .\ns" IM ( -- ) immediate" CHECK! .\ns" LB ( -- ) [" CHECK! .\ns" RB ( -- ) ]" CHECK! .\n' | bin/hb 2>/dev/null)
[ "$out" = "0
0
0
0
0
0" ] || { echo "FAIL: unsafe compiler words did not hard-reject (got: $out)"; exit 1; }
printf ': EV ( -- n ) evaluate ;\n' | ./tools/check.sh --json-errors >/dev/null 2>$T/habu-unsafe.err && { echo "FAIL: tools/check.sh accepted unsafe evaluate"; exit 1; }
grep -q '"code":"E-UNSAFE"' $T/habu-unsafe.err || { echo "FAIL: unsafe checker missing E-UNSAFE"; exit 1; }
grep -q '"token":"evaluate"' $T/habu-unsafe.err || { echo "FAIL: unsafe checker missing token"; exit 1; }
bin/hb "$GATE_JSON" diag-repair-class "$T/habu-unsafe.err" trusted_boundary_required
printf ': EV ( -- n ) evaluate ;\nEV .\n' | bin/hb >/dev/null 2>&1 && { echo "FAIL: hb published unsafe evaluate definition"; exit 1; }
cat > $T/habu-all-errors.f <<'EOF'
: OK ( i64 -- i64 ) dup * ;
: SEMI ( -- i64 ) [char] ; ;
: BAD1 ( i64 -- i64 ) dup ;
: BAD2 ( i64 -- ) >r ;
EOF
./tools/check.sh --json-errors --all-errors $T/habu-all-errors.f >/dev/null 2>$T/habu-all-errors.err && { echo "FAIL: tools/check.sh --all-errors accepted bad defs"; exit 1; }
bin/hb "$GATE_JSON" all-errors "$T/habu-all-errors.err"
cat tools/json.f tools/diag-to-sarif.f > $T/diag-to-sarif.f
bin/hb $T/diag-to-sarif.f $T/habu-all-errors.err < /dev/null > $T/habu-all-errors.sarif
bin/hb "$GATE_JSON" sarif "$T/habu-all-errors.sarif"
cat tools/lint/lib.f tools/public-signatures.f > $T/public-signatures.f
bin/hb $T/public-signatures.f examples/llm/good.f < /dev/null > $T/public-signatures.json
bin/hb "$GATE_JSON" public-signatures "$T/public-signatures.json"
TRUST_LINT_TODAY=2026-10-01 sh -c 'cat tools/date.f tools/lint/lib.f tools/fs.f tools/trust-lint.f | bin/hb' >$T/trust-stale.out 2>&1 && { echo "FAIL: trust-lint accepted stale audit dates"; exit 1; }
grep -q 'STALE-AUDIT' $T/trust-stale.out || { echo "FAIL: trust-lint stale audit diagnostic missing"; exit 1; }
echo "PASS: checked bin/hb + getenv + sig-check (rows+quots)"
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
cat lib/errors.f lib/process.f test/proc-pty.f | bin/hb || { echo "FAIL: process/pty"; exit 1; }
out=$(printf ': LONG-PROFILER-BUSY-WORD ( -- ) 80000000 begin 1- dup dup * drop dup 0= until drop ;\n: GO ( -- ) 100000 prof-on LONG-PROFILER-BUSY-WORD prof-report ;\nGO\n' | bin/hb 2>/dev/null | head -1)
case "$out" in
  "LONG-PROFILER-BUSY-WORD "*) ;;
  *) echo "FAIL: profiler long-name output (got: $out)"; exit 1 ;;
esac
echo "PASS: profiler long dictionary names"
# hb-build DEFAULT = AOT: compile MAIN to native, engine stripped (no interpreter).
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
./tools/imgdump.sh $T/hb-rt > $T/hb-rt-dict || { echo "FAIL: imgdump generated engine"; exit 1; }
grep -q '^+ ' $T/hb-rt-dict || { echo "FAIL: imgdump missing seed dict"; exit 1; }
printf ': RBAD ( i64 -- i64 ) 0= ;\nEXPORT RBAD\n' > $T/hb-rt-bad.f
if ./tools/hb-build.sh --repl $T/hb-rt-bad.f -o $T/hb-rt-bad >/dev/null 2>$T/hb-rt-bad.err; then
  echo "FAIL: hb-build --repl accepted bool-as-i64 false cert"; exit 1
fi
grep -q "expected: i64" $T/hb-rt-bad.err || { echo "FAIL: hb-build --repl diagnostic lost expected type"; exit 1; }
grep -q "actual: bool" $T/hb-rt-bad.err || { echo "FAIL: hb-build --repl diagnostic lost actual type"; exit 1; }
echo "PASS: hb-build --repl verifies user defs ($(stat -f%z $T/hb-rt) B, engine + library)"
echo "PASS: native gate (fixpoint + engine suite + checked hb + repl + hb-build)"
