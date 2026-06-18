#!/bin/sh
# run.sh — validate the benchmark reference answer key, functional tests, and
# reference metric data using the native Habu engine only.
set -e
cd "$(dirname "$0")/../.."
T=$(mktemp -d "${TMPDIR:-/tmp}/habu-llm.XXXXXX")
cleanup() {
  if command -v trash >/dev/null 2>&1; then
    trash "$T"
  else
    rm -r "$T"
  fi
}
trap cleanup EXIT HUP INT TERM
check_tsv_shape() {
  awk -F '\t' 'NF != 12 { print "FAIL: tasks.tsv line " NR " has " NF " field(s)"; bad=1 } END { exit bad ? 1 : 0 }' bench/llm/tasks.tsv
}
require_task() {
  id=$1
  name=$2
  category=$3
  harness=$4
  conv=$5
  tags=$6
  awk -F '\t' -v id="$id" -v name="$name" -v category="$category" \
    -v harness="$harness" -v conv="$conv" -v tags="$tags" '
    BEGIN { split(tags, want, ",") }
    NR > 1 && $1 == id && $2 == name && $4 == category &&
      $6 == harness && $7 == conv {
        ok = 1
        for (i in want) {
          if (!index("," $10 ",", "," want[i] ",")) ok = 0
        }
        if (ok) found = 1
      }
    END { exit found ? 0 : 1 }
  ' bench/llm/tasks.tsv || {
    echo "FAIL: missing V2 task row $id $name ($category/$harness/$conv/$tags)"
    exit 1
  }
}
check_v2_manifest() {
  require_task 56 CALL-TWICE quotation forth stack v2
  require_task 57 R-KEEP2 return-stack forth stack v2
  require_task 58 ROW-DUP row-polymorphism forth stack v2
  require_task 59 UNTIL5 control-loop forth stack v2
  require_task 60 MEM-SWAPCELL memory forth stack v2
  require_task 61 TRI checked-combinator forth stack v2
  require_task 62 DATE-PARSE-OK? date stdlib stack parse-ymd
  require_task 63 DATE-FORMAT-OK? date stdlib stack format-ymd
  require_task 64 EPOCH-UTC-OK? date stdlib stack format-epoch-utc
  require_task 65 MONO-ELAPSED? time stdlib stack mono-ns
  require_task 66 INVALID-DATE? date stdlib stack invalid-date
  require_task 67 AOT-MAIN-ARITH aot-safe aot build-run aot-positive
  require_task 68 AOT-MAIN-STRING aot-safe aot build-run aot-positive
  require_task 69 AOT-UNSAFE-HERE aot-unsupported aot-negative reject aot-negative
  require_task 70 AOT-UNSAFE-ALLOT aot-unsupported aot-negative reject aot-negative
  require_task 71 DIAG-REMOVE-PRODUCER diagnostic-repair forth stack v2,remove_producer
  require_task 72 DIAG-ADD-PRODUCER diagnostic-repair forth stack v2,add_producer
  require_task 73 DIAG-FIX-TYPE diagnostic-repair forth stack v2,fix_type
  require_task 74 DIAG-FIX-RSTACK diagnostic-repair forth stack v2,fix_return_stack
  require_task 75 DIAG-TRUSTED-BOUNDARY diagnostic-repair forth stack v2,trusted_boundary_required
  require_task 76 DIAG-SIGNATURE-SYNTAX diagnostic-repair forth stack v2,fix_signature_syntax
  require_task 77 DIAG-REWRITE-UNCHECKABLE diagnostic-repair forth stack v2,rewrite_uncheckable
  require_task 78 FIND-FIRST-NEG arrays array as v2,find-index
  require_task 79 ABS-EACH arrays array aa v2,map
  require_task 80 ADD-INDEX arrays array aa v2,indexed-map
  require_task 81 PREFIX-PROD arrays array aa v2,scan
  require_task 82 REVERSE-INNER arrays array aa v2,reverse-range
  require_task 83 STR-TRIM-OK? strings stdlib stack v2,trim
  require_task 84 STR-SPLIT-OK? strings stdlib stack v2,split
  require_task 85 STR-BUILDER-OK? strings stdlib stack v2,builder
  require_task 86 STR-PARSE-I64-OK? strings stdlib stack v2,parse-i64
  require_task 87 STR-PREFIX-SUFFIX-OK? strings stdlib stack v2,prefix-suffix
  require_task 88 STR-SEARCH-OK? strings stdlib stack v2,search
  require_task 89 MAP-COUNT-OK? maps stdlib stack v2,count
  require_task 90 MAP-MISS-OK? maps stdlib stack v2,miss
  require_task 91 MAP-UPDATE-OK? maps stdlib stack v2,update
  require_task 92 MAP-COLLISION-OK? maps stdlib stack v2,collision
  require_task 93 MAP-EACH-OK? maps stdlib stack v2,iteration
  require_task 94 MAP-GROUP-OK? maps stdlib stack v2,grouping
  require_task 95 RX-MATCH-OK? regex stdlib stack v2,match
  require_task 96 RX-FIND-OK? regex stdlib stack v2,find
  require_task 97 RX-COUNT-OK? regex stdlib stack v2,count
  require_task 98 RX-BAD-PATTERN regex stdlib-negative reject v2,negative-syntax
  require_task 99 RX-CAPACITY regex stdlib-negative reject v2,negative-capacity
}
assert_repair_class() {
  name=$1
  class=$2
  source=$3
  printf '%s\n' "$source" >"$T/$name.f"
  ./tools/check.sh --json-errors "$T/$name.f" >/dev/null 2>"$T/$name.err" && {
    echo "FAIL: diagnostic fixture accepted $name"
    exit 1
  }
  bin/hb "$T/gate-json-assert.f" diag-repair-class "$T/$name.err" "$class"
}
check_diagnostic_v2_fixtures() {
  cat tools/json.f tools/gate-json-assert.f >"$T/gate-json-assert.f"
  assert_repair_class diag-remove-producer remove_producer ': DIAG-REMOVE-PRODUCER ( i64 -- i64 ) dup ;'
  assert_repair_class diag-add-producer add_producer ': DIAG-ADD-PRODUCER ( i64 -- i64 ) drop ;'
  assert_repair_class diag-fix-type fix_type ': DIAG-FIX-TYPE ( i64 -- i64 ) 0= ;'
  assert_repair_class diag-fix-rstack fix_return_stack ': DIAG-FIX-RSTACK ( i64 -- ) >r ;'
  assert_repair_class diag-trusted-boundary trusted_boundary_required ': DIAG-TRUSTED-BOUNDARY ( -- i64 ) evaluate ;'
  assert_repair_class diag-signature-syntax fix_signature_syntax ': DIAG-SIGNATURE-SYNTAX ( i64 ) 1 + ;'
  assert_repair_class diag-rewrite-uncheckable rewrite_uncheckable ': DIAG-REWRITE-UNCHECKABLE ( i64 -- i64 ) leave ;'
}
check_aot_v2_fixtures() {
  printf '%s\n' ': MAIN ( -- ) 6 7 * . cr ;' >"$T/aot-ok.f"
  ./tools/hb-build.sh "$T/aot-ok.f" -o "$T/aot-ok" >/dev/null
  [ "$("$T/aot-ok")" = "42" ] || { echo "FAIL: V2 AOT positive fixture"; exit 1; }
  printf '%s\n' ': MAIN ( -- ) s" hi" nip [char] 0 + . cr ;' >"$T/aot-string.f"
  ./tools/hb-build.sh "$T/aot-string.f" -o "$T/aot-string" >/dev/null
  [ "$("$T/aot-string")" = "50" ] || { echo "FAIL: V2 AOT string fixture"; exit 1; }
  printf '%s\n' ': MAIN ( -- ) here drop ;' >"$T/aot-bad-here.f"
  ./tools/hb-build.sh --json-errors "$T/aot-bad-here.f" -o "$T/aot-bad-here" >/dev/null 2>"$T/aot-bad-here.err" && {
    echo "FAIL: V2 AOT accepted here"
    exit 1
  }
  grep -q '"code":"E-AOT-UNSUPPORTED"' "$T/aot-bad-here.err" || { echo "FAIL: V2 AOT here code"; exit 1; }
  grep -q '"token":"here"' "$T/aot-bad-here.err" || { echo "FAIL: V2 AOT here token"; exit 1; }
  printf '%s\n' ': MAIN ( -- ) 8 allot ;' >"$T/aot-bad-allot.f"
  ./tools/hb-build.sh --json-errors "$T/aot-bad-allot.f" -o "$T/aot-bad-allot" >/dev/null 2>"$T/aot-bad-allot.err" && {
    echo "FAIL: V2 AOT accepted allot"
    exit 1
  }
  grep -q '"code":"E-AOT-UNSUPPORTED"' "$T/aot-bad-allot.err" || { echo "FAIL: V2 AOT allot code"; exit 1; }
  grep -q '"token":"allot"' "$T/aot-bad-allot.err" || { echo "FAIL: V2 AOT allot token"; exit 1; }
}
assert_regex_throw_file() {
  name=$1
  code=$2
  source=$3
  out=$(bin/hb "$source" 2>"$T/$name.err") || {
    cat "$T/$name.err"
    echo "FAIL: regex fixture did not catch $name"
    exit 1
  }
  [ "$out" = "$code" ] || {
    echo "FAIL: regex fixture $name got $out want $code"
    exit 1
  }
}
check_regex_v2_fixtures() {
  cat lib/errors.f lib/string.f lib/regex.f >"$T/rx-bad-pattern.f"
  cat >>"$T/rx-bad-pattern.f" <<'EOF'
64 constant RX-BENCH-CAP
create RX-BENCH RX-BENCH-CAP allot
variable RX-BENCH-LEN
: RX-BENCH-BAD ( -- )
   s" *a" RX-BENCH RX-BENCH-CAP RX-COMPILE RX-BENCH-LEN !
   s" aaa" RX-BENCH RX-BENCH-LEN @ RX-MATCH? drop ;
' RX-BENCH-BAD catch . cr
EOF
  assert_regex_throw_file rx-bad-pattern "-2300" "$T/rx-bad-pattern.f"

  cat lib/errors.f lib/string.f lib/regex.f >"$T/rx-bad-anchor.f"
  cat >>"$T/rx-bad-anchor.f" <<'EOF'
64 constant RX-BENCH-CAP
create RX-BENCH RX-BENCH-CAP allot
variable RX-BENCH-LEN
: RX-BENCH-BAD ( -- )
   s" ^*" RX-BENCH RX-BENCH-CAP RX-COMPILE RX-BENCH-LEN !
   s" aaa" RX-BENCH RX-BENCH-LEN @ RX-MATCH? drop ;
' RX-BENCH-BAD catch . cr
EOF
  assert_regex_throw_file rx-bad-anchor "-2300" "$T/rx-bad-anchor.f"

  cat lib/errors.f lib/string.f lib/regex.f >"$T/rx-capacity.f"
  cat >>"$T/rx-capacity.f" <<'EOF'
create RX-BENCH 2 allot
: RX-BENCH-BAD ( -- )
   s" abc" RX-BENCH 2 RX-COMPILE drop ;
' RX-BENCH-BAD catch . cr
EOF
  assert_regex_throw_file rx-capacity "-2301" "$T/rx-capacity.f"
}
check_tsv_shape
check_v2_manifest
check_diagnostic_v2_fixtures
N=$(awk -F '\t' 'NR>1 && $6 == "forth" {n++} END{print n+0}' bench/llm/tasks.tsv)
DEFN=$(grep -c '^: ' bench/llm/solutions.f)
[ "$DEFN" = "$N" ] || { echo "FAIL: task/solution count mismatch ($N task(s), $DEFN definition(s))"; exit 1; }
[ -x bin/hb ] || ./tools/build.sh >/dev/null
./tools/check.sh bench/llm/solutions.f >"$T/check.out" 2>"$T/check.err" || {
  cat "$T/check.err"
  echo "FAIL: answer key is not all-certified"
  exit 1
}
echo "hb LLM bench: $N/$N reference solutions certified, 0 rejected"
TEST_OUT=$(cat bench/llm/solutions.f bench/llm/tests.f | bin/hb 2>"$T/tests.err")
[ "$TEST_OUT" = "ok" ] || { echo "FAIL: reference functional tests (got: $TEST_OUT)"; exit 1; }
REF=$T/ref-solutions.f
cat lib/errors.f lib/string.f lib/regex.f lib/map.f lib/date.f lib/time.f bench/llm/ref-solutions.f >"$REF"
./tools/check.sh "$REF" >"$T/ref-check.out" 2>"$T/ref-check.err" || {
  cat "$T/ref-check.err"
  echo "FAIL: V2 reference solutions are not all-certified"
  exit 1
}
REF_OUT=$(bin/hb < "$REF" 2>"$T/ref.err")
[ "$REF_OUT" = "REF-OK" ] || { echo "FAIL: V2 reference tests (got: $REF_OUT)"; exit 1; }
check_aot_v2_fixtures
check_regex_v2_fixtures
VALIDATOR=$T/validate-results.f
cat tools/date.f tools/lint/lib.f tools/json.f tools/argv.f bench/llm/validate-results.f >"$VALIDATOR"
bin/hb "$VALIDATOR"
bench/llm/attempt-runner-test.sh
echo "PASS: answer key valid ($N/$N certified, $N/$N tests passed, metrics valid)"
