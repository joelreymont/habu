#!/bin/sh
set -eu

cd "$(dirname "$0")/../.."
ROOT=$(pwd)
T=$(mktemp -d "${TMPDIR:-/tmp}/hb-llm-results.XXXXXX")
cleanup() {
  if command -v trash >/dev/null 2>&1; then
    trash "$T"
  else
    rm -r "$T"
  fi
}
trap cleanup EXIT HUP INT TERM

BUNDLE=$T/validate-results.f
cat "$ROOT/tools/lint/lib.f" "$ROOT/tools/json.f" "$ROOT/tools/argv.f" "$ROOT/bench/llm/validate-results.f" > "$BUNDLE"

mkdir -p "$T/bench/llm/results"
cp "$ROOT/bench/llm/tasks.tsv" "$T/bench/llm/tasks.tsv"
cp "$ROOT/bench/llm/results/reference.jsonl" "$T/bench/llm/results/reference.jsonl"

out=$(cd "$T" && "$ROOT/bin/hb" "$BUNDLE")
expected_count=$(awk 'NR>1{n++} END{print n+0}' "$ROOT/bench/llm/tasks.tsv")
expected="llm-results: $expected_count reference metric row(s), 0 finding(s)"
[ "$out" = "$expected" ] || {
  echo "FAIL: validate-results good fixture: $out"
  exit 1
}

awk '
{
  gsub(/"run_id":"[^"]+"/, "\"run_id\":\"attempt-fixture\"");
  gsub(/"model":"reference"/, "\"model\":\"toy-model\"");
  if ($0 ~ /"task_id":1,/) {
    gsub(/"first_pass_checker":"certified"/, "\"first_pass_checker\":\"rejected\"");
    gsub(/"first_pass_tests":true/, "\"first_pass_tests\":false");
    gsub(/"tests_passed":true/, "\"tests_passed\":false");
    gsub(/"repair_iterations":0/, "\"repair_iterations\":2");
    gsub(/"checker_iterations":1/, "\"checker_iterations\":3");
    gsub(/"diagnostic_count":0/, "\"diagnostic_count\":4");
    gsub(/"tokens_used":0/, "\"tokens_used\":100");
    gsub(/"wall_ms":0/, "\"wall_ms\":250");
  }
  if ($0 ~ /"task_id":2,/) {
    gsub(/"trust_uses":0/, "\"trust_uses\":1");
    gsub(/"signature_weakened":false/, "\"signature_weakened\":true");
  }
  print
}
' "$ROOT/bench/llm/results/reference.jsonl" > "$T/bench/llm/results/attempt.jsonl"

out=$(cd "$T" && "$ROOT/bin/hb" "$BUNDLE" bench/llm/results/attempt.jsonl)
printf '%s\n' "$out" | grep -q 'run=attempt-fixture model=toy-model rows=33 certified=32 first_tests=32 tests=32 repairs=2 checker_iterations=35 diagnostics=4 tokens=100 wall_ms=250' || {
  echo "FAIL: validate-results summary totals"
  printf '%s\n' "$out"
  exit 1
}
printf '%s\n' "$out" | grep -q 'buckets checker_rejected=1 first_tests_failed=1 tests_failed=1 trust_used=1 signature_weakened=1' || {
  echo "FAIL: validate-results summary buckets"
  printf '%s\n' "$out"
  exit 1
}
printf '%s\n' "$out" | grep -q 'category arithmetic rows=6 certified=5 tests=5' || {
  echo "FAIL: validate-results summary category"
  printf '%s\n' "$out"
  exit 1
}

out=$(cd "$T" && "$ROOT/bin/hb" "$BUNDLE" --json bench/llm/results/attempt.jsonl)
printf '%s\n' "$out" | grep -q '"rows":33' || {
  echo "FAIL: validate-results json rows"
  printf '%s\n' "$out"
  exit 1
}
printf '%s\n' "$out" | grep -q '"checker_rejected":1' || {
  echo "FAIL: validate-results json buckets"
  printf '%s\n' "$out"
  exit 1
}
printf '%s\n' "$out" | grep -q '"category":"arithmetic","rows":6,"certified":5,"tests_passed":5' || {
  echo "FAIL: validate-results json category"
  printf '%s\n' "$out"
  exit 1
}

cp "$ROOT/bench/llm/results/reference.jsonl" "$T/bench/llm/results/reference.jsonl"
head -n 1 "$ROOT/bench/llm/results/reference.jsonl" >> "$T/bench/llm/results/reference.jsonl"
set +e
out=$(cd "$T" && "$ROOT/bin/hb" "$BUNDLE" 2>&1)
rc=$?
set -e
[ "$rc" -ne 0 ] || { echo "FAIL: validate-results accepted duplicate"; exit 1; }
printf '%s\n' "$out" | grep -q 'duplicate task_id' || {
  echo "FAIL: validate-results duplicate diagnostic"
  printf '%s\n' "$out"
  exit 1
}

echo "PASS: validate-results fixtures"
