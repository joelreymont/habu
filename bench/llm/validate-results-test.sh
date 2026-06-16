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
cat "$ROOT/tools/lint/lib.f" "$ROOT/tools/json.f" "$ROOT/bench/llm/validate-results.f" > "$BUNDLE"

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
