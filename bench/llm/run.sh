#!/bin/sh
# run.sh — validate the benchmark reference answer key, functional tests, and
# reference metric data using the native Habu engine only.
set -e
cd "$(dirname "$0")/../.."
T=$(mktemp -d "${TMPDIR:-/tmp}/habu-llm.XXXXXX")
cleanup() {
  if command -v trash >/dev/null 2>&1; then
    trash "$T" 2>/dev/null && return
  fi
  rm -rf "$T"
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
  require_task 122 DIAG-TRUST-BOUNDARY diagnostic-repair forth stack v2,trusted_boundary_required,trust
  require_task 123 DIAG-SET-CHECK-BOUNDARY diagnostic-repair forth stack v2,trusted_boundary_required,set-check
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
  require_task 100 FS-PATH-KINDS-OK? files stdlib stack v2,path-kind
  require_task 101 FS-BASENAME-OK? files stdlib stack v2,basename
  require_task 102 FS-JOIN-OK? files stdlib stack v2,join-path
  require_task 103 FS-READ-ALL-OK? files stdlib-file run v2,read-all
  require_task 104 FS-WRITE-ALL-OK? files stdlib-file run v2,write-all
  require_task 105 FS-APPEND-OK? files stdlib-file run v2,append
  require_task 106 FS-READ-CAPACITY files stdlib-negative reject v2,negative-capacity
  require_task 107 PROC-RUN-RC-OK? process stdlib-process run v2,run-rc
  require_task 108 PROC-CAPTURE-OUTERR-OK? process stdlib-process run v2,capture-streams
  require_task 109 PROC-CAPTURE-NONZERO-OK? process stdlib-process run v2,nonzero-rc
  require_task 110 PROC-CAPTURE-TIMEOUT process stdlib-negative reject v2,timeout
  require_task 111 PROC-CAPTURE-TRUNCATED process stdlib-negative reject v2,negative-truncation
  require_task 112 PROP-DEFAULTS-OK? property stdlib-property run v2,defaults
  require_task 113 PROP-RND-SEQ-OK? property stdlib-property run v2,deterministic-rnd
  require_task 114 PROP-GEN-SCRIPT-OK? property stdlib-property run v2,generator
  require_task 115 PROP-SHRINK-OK? property stdlib-property run v2,shrink
  require_task 116 PROP-BAD-SEED property stdlib-negative reject v2,negative-seed
  require_task 117 BUILD-CHECK-SOURCE-OK? build stdlib-build run v2,check-source
  require_task 118 BUILD-ARTIFACT-OK? build stdlib-build run v2,artifact
  require_task 119 BUILD-STEP-STATUS build stdlib-negative reject v2,step-status
  require_task 120 BUILD-RUN-ARTIFACT-OK? build stdlib-build run v2,run-artifact
  require_task 121 BUILD-MISSING-ARTIFACT build stdlib-negative reject v2,missing-artifact
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
  bin/hb "$T/gate-json-assert.f" json-one-schema "$T/$name.err"
  bin/hb "$T/gate-json-assert.f" diag-repair-class "$T/$name.err" "$class"
}
check_diagnostic_v2_fixtures() {
  cat tools/json.f tools/gate-json-assert.f >"$T/gate-json-assert.f"
  assert_repair_class diag-remove-producer remove_producer ': DIAG-REMOVE-PRODUCER ( i64 -- i64 ) dup ;'
  assert_repair_class diag-add-producer add_producer ': DIAG-ADD-PRODUCER ( i64 -- i64 ) drop ;'
  assert_repair_class diag-fix-type fix_type ': DIAG-FIX-TYPE ( i64 -- i64 ) 0= ;'
  assert_repair_class diag-fix-rstack fix_return_stack ': DIAG-FIX-RSTACK ( i64 -- ) >r ;'
  assert_repair_class diag-trusted-boundary trusted_boundary_required ': DIAG-TRUSTED-BOUNDARY ( -- i64 ) evaluate ;'
  assert_repair_class diag-trust-boundary trusted_boundary_required ': DIAG-TRUST-BOUNDARY ( -- i64 ) s" HIDDEN" s" -- i64" TRUST 42 ;'
  assert_repair_class diag-set-check-boundary trusted_boundary_required ': DIAG-SET-CHECK-BOUNDARY ( -- i64 ) 0 set-check 42 ;'
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
assert_file_fixture() {
  name=$1
  want=$2
  source=$3
  shift 3
  out=$(bin/hb "$source" "$@" 2>"$T/$name.err") || {
    cat "$T/$name.err"
    echo "FAIL: file fixture failed $name"
    exit 1
  }
  [ "$out" = "$want" ] || {
    echo "FAIL: file fixture $name got $out want $want"
    exit 1
  }
}
check_file_v2_fixtures() {
  printf '%s' 'alpha' >"$T/fs-read.txt"
  cat lib/errors.f lib/string.f lib/fs.f >"$T/fs-read-all.f"
  cat >>"$T/fs-read-all.f" <<'EOF'
16 constant FS-BENCH-CAP
create FS-BENCH-BUF FS-BENCH-CAP allot
: FS-READ-ALL-OK? ( -- bool )
   0 SCRIPT-ARGV$ FS-BENCH-BUF FS-BENCH-CAP READ-ALL
   FS-BENCH-BUF swap s" alpha" STR= ;
FS-READ-ALL-OK? . cr
EOF
  assert_file_fixture fs-read-all "-1" "$T/fs-read-all.f" "$T/fs-read.txt"

  cat lib/errors.f lib/string.f lib/fs.f >"$T/fs-write-all.f"
  cat >>"$T/fs-write-all.f" <<'EOF'
16 constant FS-BENCH-CAP
create FS-BENCH-BUF FS-BENCH-CAP allot
: FS-WRITE-ALL-OK? ( -- bool )
   0 SCRIPT-ARGV$ s" xy" WRITE-ALL
   0 SCRIPT-ARGV$ FS-BENCH-BUF FS-BENCH-CAP READ-ALL
   FS-BENCH-BUF swap s" xy" STR= ;
FS-WRITE-ALL-OK? . cr
EOF
  assert_file_fixture fs-write-all "-1" "$T/fs-write-all.f" "$T/fs-write.txt"

  cat lib/errors.f lib/string.f lib/fs.f >"$T/fs-append.f"
  cat >>"$T/fs-append.f" <<'EOF'
16 constant FS-BENCH-CAP
create FS-BENCH-BUF FS-BENCH-CAP allot
: FS-APPEND-OK? ( -- bool )
   0 SCRIPT-ARGV$ s" xy" WRITE-ALL
   0 SCRIPT-ARGV$ s" z" APPEND-FILE
   0 SCRIPT-ARGV$ FS-BENCH-BUF FS-BENCH-CAP READ-ALL
   FS-BENCH-BUF swap s" xyz" STR= ;
FS-APPEND-OK? . cr
EOF
  assert_file_fixture fs-append "-1" "$T/fs-append.f" "$T/fs-append.txt"

  printf '%s' 'abcd' >"$T/fs-big.txt"
  cat lib/errors.f lib/string.f lib/fs.f >"$T/fs-read-capacity.f"
  cat >>"$T/fs-read-capacity.f" <<'EOF'
3 constant FS-BENCH-CAP
create FS-BENCH-BUF FS-BENCH-CAP allot
: FS-BENCH-BAD ( -- )
   0 SCRIPT-ARGV$ FS-BENCH-BUF FS-BENCH-CAP READ-ALL drop ;
' FS-BENCH-BAD catch . cr
EOF
  assert_file_fixture fs-read-capacity "-2106" "$T/fs-read-capacity.f" "$T/fs-big.txt"
}
assert_process_fixture() {
  name=$1
  want=$2
  source=$3
  shift 3
  out=$(bin/hb "$source" "$@" 2>"$T/$name.err") || {
    cat "$T/$name.err"
    echo "FAIL: process fixture failed $name"
    exit 1
  }
  [ "$out" = "$want" ] || {
    echo "FAIL: process fixture $name got $out want $want"
    exit 1
  }
}
check_process_v2_fixtures() {
  cat >"$T/proc-rc-ok" <<'EOF'
#!/bin/sh
exit 0
EOF
  cat >"$T/proc-capture-out-err" <<'EOF'
#!/bin/sh
printf 'out'
printf 'err' >&2
exit 0
EOF
  cat >"$T/proc-capture-nonzero" <<'EOF'
#!/bin/sh
exit 7
EOF
  cat >"$T/proc-capture-sleep" <<'EOF'
#!/bin/sh
sleep 2
EOF
  cat >"$T/proc-capture-long" <<'EOF'
#!/bin/sh
printf 'abcdef'
EOF
  chmod +x "$T/proc-rc-ok" "$T/proc-capture-out-err" \
    "$T/proc-capture-nonzero" "$T/proc-capture-sleep" \
    "$T/proc-capture-long"

  cat lib/errors.f lib/string.f lib/process.f >"$T/proc-run-rc-ok.f"
  cat >>"$T/proc-run-rc-ok.f" <<'EOF'
: PROC-RUN-RC-OK? ( -- bool )
   0 SCRIPT-ARGV$ RUN-RC 0= ;
PROC-RUN-RC-OK? . cr
EOF
  assert_process_fixture proc-run-rc-ok "-1" "$T/proc-run-rc-ok.f" "$T/proc-rc-ok"

  cat lib/errors.f lib/string.f lib/process.f >"$T/proc-capture-out-err.f"
  cat >>"$T/proc-capture-out-err.f" <<'EOF'
32 constant PROC-BENCH-CAP
create PROC-BENCH-OUT PROC-BENCH-CAP allot
create PROC-BENCH-ERR PROC-BENCH-CAP allot
: PROC-CAPTURE-OUTERR-OK? ( -- bool )
   0 SCRIPT-ARGV$ PROC-BENCH-OUT PROC-BENCH-CAP PROC-BENCH-ERR PROC-BENCH-CAP 1000 RUN-CAPTURE
   {: outu erru rc :}
   rc 0 <> if STR-FALSE exit then
   outu 3 <> if STR-FALSE exit then
   erru 3 <> if STR-FALSE exit then
   PROC-BENCH-OUT outu s" out" STR= 0= if STR-FALSE exit then
   PROC-BENCH-ERR erru s" err" STR= ;
PROC-CAPTURE-OUTERR-OK? . cr
EOF
  assert_process_fixture proc-capture-out-err "-1" "$T/proc-capture-out-err.f" "$T/proc-capture-out-err"

  cat lib/errors.f lib/string.f lib/process.f >"$T/proc-capture-nonzero.f"
  cat >>"$T/proc-capture-nonzero.f" <<'EOF'
32 constant PROC-BENCH-CAP
create PROC-BENCH-OUT PROC-BENCH-CAP allot
create PROC-BENCH-ERR PROC-BENCH-CAP allot
: PROC-CAPTURE-NONZERO-OK? ( -- bool )
   0 SCRIPT-ARGV$ PROC-BENCH-OUT PROC-BENCH-CAP PROC-BENCH-ERR PROC-BENCH-CAP 1000 RUN-CAPTURE
   {: outu erru rc :}
   rc 7 = outu 0= and erru 0= and ;
PROC-CAPTURE-NONZERO-OK? . cr
EOF
  assert_process_fixture proc-capture-nonzero "-1" "$T/proc-capture-nonzero.f" "$T/proc-capture-nonzero"

  cat lib/errors.f lib/string.f lib/process.f >"$T/proc-capture-timeout.f"
  cat >>"$T/proc-capture-timeout.f" <<'EOF'
32 constant PROC-BENCH-CAP
create PROC-BENCH-OUT PROC-BENCH-CAP allot
create PROC-BENCH-ERR PROC-BENCH-CAP allot
: PROC-BENCH-BAD ( -- )
   0 SCRIPT-ARGV$ PROC-BENCH-OUT PROC-BENCH-CAP PROC-BENCH-ERR PROC-BENCH-CAP 100 RUN-CAPTURE 2drop drop ;
' PROC-BENCH-BAD catch . cr
EOF
  assert_process_fixture proc-capture-timeout "-2502" "$T/proc-capture-timeout.f" "$T/proc-capture-sleep"

  cat lib/errors.f lib/string.f lib/process.f >"$T/proc-capture-truncated.f"
  cat >>"$T/proc-capture-truncated.f" <<'EOF'
3 constant PROC-BENCH-CAP
create PROC-BENCH-OUT PROC-BENCH-CAP allot
create PROC-BENCH-ERR PROC-BENCH-CAP allot
: PROC-BENCH-BAD ( -- )
   0 SCRIPT-ARGV$ PROC-BENCH-OUT PROC-BENCH-CAP PROC-BENCH-ERR PROC-BENCH-CAP 1000 RUN-CAPTURE 2drop drop ;
' PROC-BENCH-BAD catch . cr
EOF
  assert_process_fixture proc-capture-truncated "-2504" "$T/proc-capture-truncated.f" "$T/proc-capture-long"
}
assert_property_fixture() {
  name=$1
  want=$2
  source=$3
  out=$(bin/hb "$source" 2>"$T/$name.err") || {
    cat "$T/$name.err"
    echo "FAIL: property fixture failed $name"
    exit 1
  }
  [ "$out" = "$want" ] || {
    echo "FAIL: property fixture $name got $out want $want"
    exit 1
  }
}
check_property_v2_fixtures() {
  cat lib/errors.f lib/string.f lib/property.f >"$T/prop-defaults.f"
  cat >>"$T/prop-defaults.f" <<'EOF'
: PROP-DEFAULTS-OK? ( -- bool )
   PROP-DEFAULTS 250 = swap 1 = and ;
PROP-DEFAULTS-OK? . cr
EOF
  assert_property_fixture prop-defaults "-1" "$T/prop-defaults.f"

  cat lib/errors.f lib/string.f lib/property.f >"$T/prop-rnd-seq.f"
  cat >>"$T/prop-rnd-seq.f" <<'EOF'
: PROP-RND-SEQ-OK? ( -- bool )
   1 5 PROP-RUN-RESET
   PROP-RND 1103527590 <> if STR-FALSE exit then
   PROP-SEED@ 1103527590 <> if STR-FALSE exit then
   10 PROP-RND% 5 <> if STR-FALSE exit then
   PROP-COUNT@ 5 = ;
PROP-RND-SEQ-OK? . cr
EOF
  assert_property_fixture prop-rnd-seq "-1" "$T/prop-rnd-seq.f"

  cat lib/errors.f lib/string.f lib/property.f >"$T/prop-gen-script.f"
  cat >>"$T/prop-gen-script.f" <<'EOF'
: PROP-GEN-SCRIPT-OK? ( -- bool )
   0 PROP-GEN-START
   s" 7 " 0 1 PROP-GEN-STEP
   s" drop " 1 -1 PROP-GEN-STEP
   PROP-GEN-DEPTH@ 0 <> if STR-FALSE exit then
   PROP-BUF$ s" 7 drop " STR= ;
PROP-GEN-SCRIPT-OK? . cr
EOF
  assert_property_fixture prop-gen-script "-1" "$T/prop-gen-script.f"

  cat lib/errors.f lib/string.f lib/property.f >"$T/prop-shrink.f"
  cat >>"$T/prop-shrink.f" <<'EOF'
: PROP-BENCH-KEEP? ( -- bool )
   PROP-BUF$ nip 4 >= ;
: PROP-SHRINK-OK? ( -- bool )
   PROP-BUF-RESET
   s" dup drop 1+ " PROP-BUF+
   [: PROP-BENCH-KEEP? ;] PROP-SHRINK
   PROP-BUF$ s" dup " STR= ;
PROP-SHRINK-OK? . cr
EOF
  assert_property_fixture prop-shrink "-1" "$T/prop-shrink.f"

  cat lib/errors.f lib/string.f lib/property.f >"$T/prop-bad-seed.f"
  cat >>"$T/prop-bad-seed.f" <<'EOF'
: PROP-BENCH-BAD ( -- )
   -1 1 PROP-RUN-RESET ;
' PROP-BENCH-BAD catch . cr
EOF
  assert_property_fixture prop-bad-seed "-2700" "$T/prop-bad-seed.f"
}
assert_build_fixture() {
  name=$1
  want=$2
  source=$3
  shift 3
  out=$(bin/hb "$source" "$@" 2>"$T/$name.err") || {
    cat "$T/$name.err"
    echo "FAIL: build fixture failed $name"
    exit 1
  }
  [ "$out" = "$want" ] || {
    echo "FAIL: build fixture $name got $out want $want"
    exit 1
  }
}
check_build_v2_fixtures() {
  printf '%s\n' ': MAIN ( -- i64 ) 42 ;' >"$T/build-source-ok.f"
  printf '%s\n' ': BAD ( i64 -- i64 ) 0= ;' >"$T/build-source-bad.f"
  cat >"$T/build-make-artifact" <<EOF
#!/bin/sh
printf 'artifact' >"$T/build-artifact.out"
EOF
  cat >"$T/build-no-artifact" <<'EOF'
#!/bin/sh
exit 0
EOF
  chmod +x "$T/build-make-artifact" "$T/build-no-artifact"

  cat lib/errors.f lib/string.f lib/fs.f lib/process.f lib/build.f >"$T/build-check-source.f"
  cat >>"$T/build-check-source.f" <<'EOF'
: BUILD-CHECK-SOURCE-OK? ( -- bool )
   0 SCRIPT-ARGV$ BUILD-CHECK
   BUILD-TRUE ;
BUILD-CHECK-SOURCE-OK? . cr
EOF
  assert_build_fixture build-check-source "-1" "$T/build-check-source.f" "$T/build-source-ok.f"

  cat lib/errors.f lib/string.f lib/fs.f lib/process.f lib/build.f >"$T/build-artifact.f"
  cat >>"$T/build-artifact.f" <<'EOF'
: BUILD-ARTIFACT-OK? ( -- bool )
   0 SCRIPT-ARGV$ s" out.bin" BUILD-ARTIFACT
   0 SCRIPT-ARGV$ s" out.bin" BUILD-PATH-BUF JOIN-PATH
   BUILD-PATH-BUF swap STR= ;
BUILD-ARTIFACT-OK? . cr
EOF
  assert_build_fixture build-artifact "-1" "$T/build-artifact.f" "$T"

  cat lib/errors.f lib/string.f lib/fs.f lib/process.f lib/build.f >"$T/build-step-status.f"
  cat >>"$T/build-step-status.f" <<'EOF'
: BUILD-BENCH-BAD-STEP ( -- n )
   7 ;
: BUILD-BENCH-BAD ( -- )
   s" bad-step" [: BUILD-BENCH-BAD-STEP ;] BUILD-STEP ;
' BUILD-BENCH-BAD catch . cr
EOF
  assert_build_fixture build-step-status "-2802" "$T/build-step-status.f"

  cat lib/errors.f lib/string.f lib/fs.f lib/process.f lib/build.f >"$T/build-run-artifact.f"
  cat >>"$T/build-run-artifact.f" <<'EOF'
: BUILD-RUN-ARTIFACT-OK? ( -- bool )
   0 SCRIPT-ARGV$ 1 SCRIPT-ARGV$ BUILD-RUN 0= ;
BUILD-RUN-ARTIFACT-OK? . cr
EOF
  assert_build_fixture build-run-artifact "-1" "$T/build-run-artifact.f" "$T/build-make-artifact" "$T/build-artifact.out"

  cat lib/errors.f lib/string.f lib/fs.f lib/process.f lib/build.f >"$T/build-missing-artifact.f"
  cat >>"$T/build-missing-artifact.f" <<'EOF'
: BUILD-BENCH-BAD ( -- )
   0 SCRIPT-ARGV$ 1 SCRIPT-ARGV$ BUILD-RUN drop ;
' BUILD-BENCH-BAD catch . cr
EOF
  assert_build_fixture build-missing-artifact "-2803" "$T/build-missing-artifact.f" "$T/build-no-artifact" "$T/build-missing.out"
}
check_tsv_shape
check_v2_manifest
check_diagnostic_v2_fixtures
N=$(awk -F '\t' 'NR>1 && $6 == "forth" {n++} END{print n+0}' bench/llm/tasks.tsv)
DEFN=$(grep -c '^: ' bench/llm/solutions.f)
[ "$DEFN" = "$N" ] || { echo "FAIL: task/solution count mismatch ($N task(s), $DEFN definition(s))"; exit 1; }
[ -x bin/hb ] || ./tools/build.sh >/dev/null
cat lib/errors.f lib/test.f bench/llm/json-row.f bench/llm/json-row-test.f | bin/hb || {
  echo "FAIL: llm json row emitter"
  exit 1
}
./tools/check.sh bench/llm/solutions.f >"$T/check.out" 2>"$T/check.err" || {
  cat "$T/check.err"
  echo "FAIL: answer key is not all-certified"
  exit 1
}
echo "hb LLM bench: $N/$N reference solutions certified, 0 rejected"
TEST_OUT=$(cat bench/llm/solutions.f bench/llm/tests.f | bin/hb 2>"$T/tests.err")
[ "$TEST_OUT" = "ok" ] || { echo "FAIL: reference functional tests (got: $TEST_OUT)"; exit 1; }
REF=$T/ref-solutions.f
cat lib/errors.f lib/string.f lib/regex.f lib/map.f lib/date.f lib/time.f lib/fs.f bench/llm/ref-solutions.f >"$REF"
./tools/check.sh "$REF" >"$T/ref-check.out" 2>"$T/ref-check.err" || {
  cat "$T/ref-check.err"
  echo "FAIL: V2 reference solutions are not all-certified"
  exit 1
}
REF_OUT=$(bin/hb < "$REF" 2>"$T/ref.err")
[ "$REF_OUT" = "REF-OK" ] || { echo "FAIL: V2 reference tests (got: $REF_OUT)"; exit 1; }
check_aot_v2_fixtures
check_regex_v2_fixtures
check_file_v2_fixtures
check_process_v2_fixtures
check_property_v2_fixtures
check_build_v2_fixtures
VALIDATOR=$T/validate-results.f
cat tools/date.f tools/lint/lib.f tools/json.f tools/argv.f bench/llm/validate-results.f >"$VALIDATOR"
bin/hb "$VALIDATOR"
bench/llm/attempt-runner-test.sh
echo "PASS: answer key valid ($N/$N certified, $N/$N tests passed, metrics valid)"
