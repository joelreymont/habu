#!/bin/sh
set -eu

cd "$(dirname "$0")/../.."
T=$(mktemp -d "${TMPDIR:-/tmp}/habu-attempt-runner-test.XXXXXX")
cleanup() { rm -rf "$T"; }
trap cleanup EXIT HUP INT TERM

C=$T/candidates
mkdir -p "$C"
awk -v dir="$C" '
  BEGIN { FS = "\t" }
  FNR == NR {
    if (FNR > 1 && $6 == "forth") {
      task_id[$2] = $1
    }
    next
  }
  /^: / {
    split($0, parts, /[ \t]+/)
    print > (dir "/" task_id[parts[2]] ".f")
  }
' bench/llm/tasks.tsv bench/llm/solutions.f

mkdir "$C/1"
cat > "$C/1/1.f" <<'EOF'
: SQUARE ( i64 -- i64 ) dup ;
EOF
cp "$C/1.f" "$C/1/2.f"
rm "$C/1.f"

mkdir "$C/2"
cat > "$C/2/1.f" <<'EOF'
: CUBE ( i64 -- i64 ) drop ;
EOF
cp "$C/2.f" "$C/2/2.f"
rm "$C/2.f"

mkdir "$C/3"
cat > "$C/3/1.f" <<'EOF'
: ABSV ( i64 -- i64 ) 0= ;
EOF
cp "$C/3.f" "$C/3/2.f"
rm "$C/3.f"

mkdir "$C/4"
cat > "$C/4/1.f" <<'EOF'
: NEG? ( i64 -- ) >r ;
EOF
cp "$C/4.f" "$C/4/2.f"
rm "$C/4.f"

mkdir "$C/5"
cat > "$C/5/1.f" <<'EOF'
: CLAMP0 ( i64 -- i64 ) evaluate ;
EOF
cp "$C/5.f" "$C/5/2.f"
rm "$C/5.f"

mkdir "$C/6"
cat > "$C/6/1.f" <<'EOF'
: SUM3 ( i64 i64 i64 ) + + ;
EOF
cp "$C/6.f" "$C/6/2.f"
rm "$C/6.f"

mkdir "$C/7"
cat > "$C/7/1.f" <<'EOF'
: AVG2 ( i64 i64 -- i64 ) leave ;
EOF
cp "$C/7.f" "$C/7/2.f"
rm "$C/7.f"

mkdir "$C/8"
cat > "$C/8/1.f" <<'EOF'
: MAX2 ( i64 -- i64 ) drop ;
: EXTRA ( i64 -- i64 ) dup ;
EOF
cp "$C/8.f" "$C/8/2.f"
rm "$C/8.f"

cat > "$C/9.f" <<'EOF'
: SWAP2 ( n n -- n n ) swap ;
EOF

OUT=$T/attempt.jsonl
SUMMARY=$T/summary.txt
bench/llm/run-attempts.sh "$C" "$OUT" attempt-fixture-2026-06-16 fixture-model >"$T/stdout" 2>"$SUMMARY"

expected_count=$(awk -F '\t' 'NR>1 && $6 == "forth" {n++} END{print n+0}' bench/llm/tasks.tsv)
[ "$(wc -l < "$OUT" | tr -d ' ')" = "$expected_count" ] || {
  echo "FAIL: attempt runner row count"
  cat "$SUMMARY"
  exit 1
}

grep -q "run=attempt-fixture-2026-06-16 model=fixture-model rows=$expected_count certified=$((expected_count - 8)) first_tests=$((expected_count - 8)) tests=$expected_count repairs=8 checker_iterations=$((expected_count + 8)) diagnostics=9 tokens=0" "$SUMMARY" || {
  echo "FAIL: attempt runner summary totals"
  cat "$SUMMARY"
  exit 1
}

grep -q 'buckets checker_rejected=8 first_tests_failed=8 tests_failed=0 trust_used=0 signature_weakened=1' "$SUMMARY" || {
  echo "FAIL: attempt runner summary buckets"
  cat "$SUMMARY"
  exit 1
}

grep -q 'category arithmetic rows=6 certified=3 tests=6' "$SUMMARY" || {
  echo "FAIL: attempt runner category accounting"
  cat "$SUMMARY"
  exit 1
}
grep -q 'repair_class remove_producer rows=2 repair_success=2 repair_iterations=2 diagnostics=2 token_delta=0' "$SUMMARY" || {
  echo "FAIL: attempt runner remove_producer class accounting"
  cat "$SUMMARY"
  exit 1
}
grep -q 'repair_class add_producer rows=2 repair_success=2 repair_iterations=2 diagnostics=2 token_delta=0' "$SUMMARY" || {
  echo "FAIL: attempt runner add_producer class accounting"
  cat "$SUMMARY"
  exit 1
}
grep -q 'repair_class fix_type rows=1 repair_success=1 repair_iterations=1 diagnostics=1 token_delta=0' "$SUMMARY" || {
  echo "FAIL: attempt runner fix_type class accounting"
  cat "$SUMMARY"
  exit 1
}
grep -q 'repair_class fix_return_stack rows=1 repair_success=1 repair_iterations=1 diagnostics=1 token_delta=0' "$SUMMARY" || {
  echo "FAIL: attempt runner fix_return_stack class accounting"
  cat "$SUMMARY"
  exit 1
}
grep -q 'repair_class trusted_boundary_required rows=1 repair_success=1 repair_iterations=1 diagnostics=1 token_delta=0' "$SUMMARY" || {
  echo "FAIL: attempt runner trusted_boundary_required class accounting"
  cat "$SUMMARY"
  exit 1
}
grep -q 'repair_class fix_signature_syntax rows=1 repair_success=1 repair_iterations=1 diagnostics=1 token_delta=0' "$SUMMARY" || {
  echo "FAIL: attempt runner fix_signature_syntax class accounting"
  cat "$SUMMARY"
  exit 1
}
grep -q 'repair_class rewrite_uncheckable rows=1 repair_success=1 repair_iterations=1 diagnostics=1 token_delta=0' "$SUMMARY" || {
  echo "FAIL: attempt runner rewrite_uncheckable class accounting"
  cat "$SUMMARY"
  exit 1
}

grep -q '"task_id":1,.*"first_pass_checker":"rejected".*"tests_passed":true.*"repair_iterations":1.*"diagnostic_count":1' "$OUT" || {
  echo "FAIL: task 1 repair row"
  cat "$OUT"
  exit 1
}

grep -q '"task_id":8,.*"diagnostic_count":2.*"repair_class_stats":\[{"repair_class":"remove_producer","diagnostic_count":1,"repair_success":true,"repair_iterations":1,"token_delta":0},{"repair_class":"add_producer","diagnostic_count":1,"repair_success":true,"repair_iterations":1,"token_delta":0}\]' "$OUT" || {
  echo "FAIL: task 8 multi-diagnostic repair class row"
  cat "$OUT"
  exit 1
}

grep -q '"task_id":9,.*"first_pass_checker":"certified".*"signature_weakened":true' "$OUT" || {
  echo "FAIL: task 9 signature row"
  cat "$OUT"
  exit 1
}

echo "PASS: attempt runner fixtures"
