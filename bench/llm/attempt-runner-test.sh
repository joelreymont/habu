#!/bin/sh
set -eu

cd "$(dirname "$0")/../.."
T=$(mktemp -d "${TMPDIR:-/tmp}/habu-attempt-runner-test.XXXXXX")
cleanup() { rm -rf "$T"; }
trap cleanup EXIT HUP INT TERM

C=$T/candidates
mkdir -p "$C"
awk -v dir="$C" '/^: /{ n++; print > (dir "/" n ".f") }' bench/llm/solutions.f

mkdir "$C/1"
cat > "$C/1/1.f" <<'EOF'
: SQUARE ( i64 -- i64 ) dup ;
EOF
cp "$C/1.f" "$C/1/2.f"
rm "$C/1.f"

cat > "$C/2.f" <<'EOF'
: CUBE ( n -- n ) dup dup * * ;
EOF

OUT=$T/attempt.jsonl
SUMMARY=$T/summary.txt
bench/llm/run-attempts.sh "$C" "$OUT" attempt-fixture-2026-06-16 fixture-model >"$T/stdout" 2>"$SUMMARY"

expected_count=$(awk 'NR>1{n++} END{print n+0}' bench/llm/tasks.tsv)
[ "$(wc -l < "$OUT" | tr -d ' ')" = "$expected_count" ] || {
  echo "FAIL: attempt runner row count"
  cat "$SUMMARY"
  exit 1
}

grep -q "run=attempt-fixture-2026-06-16 model=fixture-model rows=$expected_count certified=$((expected_count - 1)) first_tests=$((expected_count - 1)) tests=$expected_count repairs=1 checker_iterations=$((expected_count + 1)) diagnostics=1 tokens=0" "$SUMMARY" || {
  echo "FAIL: attempt runner summary totals"
  cat "$SUMMARY"
  exit 1
}

grep -q 'buckets checker_rejected=1 first_tests_failed=1 tests_failed=0 trust_used=0 signature_weakened=1' "$SUMMARY" || {
  echo "FAIL: attempt runner summary buckets"
  cat "$SUMMARY"
  exit 1
}

grep -q 'category arithmetic rows=6 certified=5 tests=6' "$SUMMARY" || {
  echo "FAIL: attempt runner category accounting"
  cat "$SUMMARY"
  exit 1
}

grep -q '"task_id":1,.*"first_pass_checker":"rejected".*"tests_passed":true.*"repair_iterations":1.*"diagnostic_count":1' "$OUT" || {
  echo "FAIL: task 1 repair row"
  cat "$OUT"
  exit 1
}

grep -q '"task_id":2,.*"first_pass_checker":"certified".*"signature_weakened":true' "$OUT" || {
  echo "FAIL: task 2 signature row"
  cat "$OUT"
  exit 1
}

echo "PASS: attempt runner fixtures"
